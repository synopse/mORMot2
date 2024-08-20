/// TFTP Server-Side Process 
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.tftp.server;

{
  *****************************************************************************

    TFTP Server Processing with RFC 1350/2347/2348/2349/7440 Support
    - TFTP Connection Thread and State Machine
    - TTftpServerThread Server Class

    Current limitation: only RRQ requests are supported yet.

  *****************************************************************************

}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.data,
  mormot.core.threads,
  mormot.core.log,
  mormot.core.rtti,
  mormot.core.buffers,
  mormot.core.json,
  mormot.net.sock,
  mormot.net.server, // for TUdpServerThread
  mormot.net.tftp.client;




{ ******************** TFTP Connection Thread and State Machine }

type
  /// tune what is TTftpServerThread accepting
  // - ttoRrq will let the TFTP server process RRQ/Get requests
  // - ttoWrq will let the TFTP server process WRQ/Put requests
  // - ttoNoBlksize will disable RFC 2348 "blksize" option on TFTP server
  // - ttoNoTimeout will disable RFC 2349 "timeout" option on TFTP server
  // - ttoNoTsize will disable RFC 2349 "tsize" option on TFTP server
  // - ttoNoWindowsize will disable RFC 7440 "windowsize" option on TFTP server
  // - ttoAllowSubFolders will allow RRW/WRQ to access nested files in
  // TTftpServerThread.FileFolder sub-directories
  // - ttoLowLevelLog will log each incoming/outgoing TFTP/UDP frames
  // - ttoDropPriviledges on POSIX would impersonate the process as 'nobody'
  // - ttoChangeRoot on POSIX would make the FileFolder the root folder
  // - ttoCaseInsensitiveFileName on POSIX would make file names case-insensitive
  // as they are on Windows (using an in-memory cache, refreshed every minute)
  TTftpThreadOption = (
    ttoRrq,
    ttoWrq,
    ttoNoBlksize,
    ttoNoTimeout,
    ttoNoTsize,
    ttoNoWindowsize,
    ttoAllowSubFolders,
    ttoLowLevelLog,
    ttoDropPriviledges,
    ttoChangeRoot,
    ttoCaseInsensitiveFileName);

  TTftpThreadOptions = set of TTftpThreadOption;

  TTftpServerThread = class;

  /// low-level TFTP process of a single connection
  TTftpConnectionThread = class(TLoggedThread)
  protected
    fContext: TTftpContext;
    fOwner: TTftpServerThread;
    fFrameMaxSize: integer;
    fFileSize: integer;
    fLastSent: pointer;
    fLastSentLen: integer;
    procedure DoExecute; override;
    procedure NotifyShutdown;
  public
    /// initialize this connection
    constructor Create(const Source: TTftpContext; Owner: TTftpServerThread); reintroduce;
    /// finalize this connection
    destructor Destroy; override;
  end;


{ ******************** TTftpServerThread Server Class }

  /// event signature of the TTftpServerThread.OnConnect optional callback
  // - you can change e.g. Context.FileName/FileNameFull/FileStream
  // - should return teNoError to continue the process
  TOnTftpConnect = function(Sender: TTftpServerThread;
    var Context: TTftpContext): TTftpError of object;

  /// server thread handling several TFTP connections
  // - this main thread binds the supplied UDP address:port, then process any
  // incoming requests from UDP packets and create TTftpConnectionThread instances
  TTftpServerThread = class(TUdpServerThread)
  protected
    fConnection: TSynObjectListLocked;
    fFileFolder: TFileName;
    fMaxConnections: integer;
    fMaxRetry: integer;
    fConnectionTotal: integer;
    fOptions: TTftpThreadOptions;
    fAsNobody: boolean;
    fRangeLow, fRangeHigh: word;
    fFileCache: TSynDictionary; // thread-safe <16MB files content cache
    {$ifdef OSPOSIX}
    fPosixFileNames: TPosixFileCaseInsensitive; // ttoCaseInsensitiveFileName
    {$endif OSPOSIX}
    fOnConnect: TOnTftpConnect;
    function GetConnectionCount: integer;
    function GetContextOptions: TTftpContextOptions;
    // default implementation will read/write from FileFolder
    procedure SetFileFolder(const Value: TFileName);
    function GetFileName(const RequestedFileName: RawUtf8): TFileName; virtual;
    function SetRrqStream(var Context: TTftpContext): TTftpError; virtual;
    function SetWrqStream(var Context: TTftpContext): TTftpError; virtual;
    // main processing methods for all incoming frames
    procedure OnFrameReceived(len: integer; var remote: TNetAddr); override;
    procedure OnIdle(tix64: Int64); override;
    procedure OnShutdown; override; // = Destroy
    procedure NotifyShutdown;
  public
    /// initialize and bind the server instance, in non-suspended state
    // - will cache small file content for 15 minutes by default, but you could
    // set CacheTimeoutSecs=0 to disable any file caching
    constructor Create(const SourceFolder: TFileName;
      Options: TTftpThreadOptions; LogClass: TSynLogClass;
      const BindAddress, BindPort, ProcessName: RawUtf8;
      CacheTimeoutSecs: integer = 15 * 60); reintroduce;
    /// finalize the server instance
    destructor Destroy; override;
    /// notify the server thread(s) to be terminated, and wait for pending
    // threads to actually abort their background process
    procedure TerminateAndWaitFinished(TimeOutMs: integer = 5000); override;
    /// this method is called when a request is received, just before creating
    // the processing thread
    property OnConnect: TOnTftpConnect
      read fOnConnect write fOnConnect;
  published
    /// how many requests are currently used
    property ConnectionCount: integer
      read GetConnectionCount;
    /// how many concurrent requests are allowed at most - default is 100
    property MaxConnections: integer
      read fMaxConnections write fMaxConnections;
    /// how many retries should be done after timeout
    // - default is 1 (i.e. it will send up to 2 times the DAT packet)
    property MaxRetry: integer
      read fMaxRetry write fMaxRetry;
    /// how many connections have been processed since the server start
    property ConnectionTotal: integer
      read fConnectionTotal;
    /// the local folder where the files are read or written
    property FileFolder: TFileName
      read fFileFolder write SetFileFolder;
    /// optional lowest UDP port to be used for the responses
    // - default 0 will let the OS select an ephemeral port
    property RangeLow: word
      read fRangeLow write fRangeLow;
    /// optional highest UDP port to be used for the responses
    // - default 0 will let the OS select an ephemeral port
    property RangeHigh: word
      read fRangeHigh write fRangeHigh;
  end;





implementation


{ ******************** TFTP Connection Thread and State Machine }

{ TTftpConnectionThread }

var
  TTftpConnectionThreadCounter: integer; // to name the working threads

constructor TTftpConnectionThread.Create(
  const Source: TTftpContext; Owner: TTftpServerThread);
begin
  fContext := Source;
  fOwner := Owner;
  inc(fOwner.fConnectionTotal);
  fFrameMaxSize := Source.BlockSize + 16; // e.g. 512 + 16
  if Source.FrameLen > fFrameMaxSize then
    raise EUdpServer.CreateFmt('%s.Create: %d>%d',
      [ClassNameShort(self)^, Source.FrameLen, fFrameMaxSize]);
  fFileSize := fContext.FileStream.Size;
  GetMem(fContext.Frame, fFrameMaxSize);
  GetMem(fLastSent, fFrameMaxSize);
  MoveFast(Source.Frame^, fLastSent^, Source.FrameLen);
  fLastSentLen := Source.FrameLen;
  FreeOnTerminate := true;
  inherited Create({suspended=}false, fOwner.LogClass, FormatUtf8('%#% % %',
    [TFTP_OPCODE[Source.OpCode], InterlockedIncrement(TTftpConnectionThreadCounter),
     fContext.FileName, KB(fFileSize)]));
end;

destructor TTftpConnectionThread.Destroy;
begin
  Terminate;
  fContext.Shutdown;
  if fOwner <> nil then
    fOwner.fConnection.Remove(self);
  inherited Destroy;
  Freemem(fLastSent);
  FreeMem(fContext.Frame);
end;

procedure TTftpConnectionThread.DoExecute;
var
  len: integer;
  res: TTftpError;
  nr: TNetResult;
  tix: Int64;
  fn: RawUtf8;
begin
  tix := mormot.core.os.GetTickCount64;
  fLog.Log(sllDebug, 'DoExecute % % % as %',
    [fContext.Remote.IPShort({withport=}true), TFTP_OPCODE[fContext.OpCode],
     fContext.FileName, fContext.FileNameFull], self);
  StringToUtf8(ExtractFileName(Utf8ToString(fContext.FileName)), fn);
  fContext.RetryCount := fOwner.MaxRetry;
  fContext.Sock.SetReceiveTimeout(1000); // check fTerminated every second
  repeat
    // try to receive a frame on this UDP/IP link
    PInteger(fContext.Frame)^ := 0;
    len := fContext.Sock.RecvFrom(fContext.Frame, fFrameMaxSize, fContext.Remote);
    if len > 0 then
      PByteArray(fContext.Frame)^[len] := 0; // 0 ended for StrLen() safety
    if Terminated then
      break;
    if ttoLowLevelLog in fOwner.fOptions then
      fLog.Log(LOG_TRACEWARNING[len <= 0], '% recv % %/%',
        [fn, ToText(fContext.Frame^, len),
         CardinalToHexShort(fContext.CurrentSize), CardinalToHexShort(fFileSize)]);
    if Terminated or
       (len = 0) then // -1=error, 0=shutdown
      break;
    if len < 0 then
    begin
      // network error (may be timeout)
      nr := NetLastError;
      if nr <> nrRetry then
      begin
        fLog.Log(sllError, 'DoExecute % recvfrom failed: %',
          [fn, ToText(nr)^], self);
        break;
      end;
      if mormot.core.os.GetTickCount64 < fContext.TimeoutTix then
        // wait for incoming UDP packet within the timeout period
        continue;
      // retry after timeout
      if fContext.RetryCount = 0 then
      begin
        fLog.Log(sllError, 'DoExecute % retried %: abort',
          [fn, Plural('time', fOwner.MaxRetry)], self);
        break;
      end;
      dec(fContext.RetryCount);
      // will send again the previous ACK/DAT frame
      fLog.Log(sllWarning, 'DoExecute % timeout: resend %/%',
        [fn, CardinalToHexShort(fContext.CurrentSize),
         CardinalToHexShort(fFileSize)], self);
      MoveFast(fLastSent^, fContext.Frame^, fLastSentLen); // restore frame
      fContext.FrameLen := fLastSentLen;
    end
    else
    begin
      // parse incoming DAT/ACK and generate the answer
      res := fContext.ParseData(len);
      if Terminated then
        break;
      if res <> teNoError then
      begin
        if res <> teFinished then
          // fatal error - e.g. teDiskFull
          fContext.SendErrorAndShutdown(res, fLog, self, 'DoExecute');
        break;
      end;
      MoveFast(fContext.Frame^, fLastSent^, fContext.FrameLen); // backup
      fLastSentLen := fContext.FrameLen;
      fContext.RetryCount := fOwner.MaxRetry; // reset RetryCount on next block
    end;
    // send next ACK or DAT block(s)
    if ttoLowLevelLog in fOwner.fOptions then
      fLog.Log(sllTrace, '% send % %/%',
        [fn, ToText(fContext.Frame^, fContext.FrameLen),
         CardinalToHexShort(fContext.CurrentSize), CardinalToHexShort(fFileSize)]);
    nr := fContext.SendFrame;
    if nr <> nrOk then
    begin
      fLog.Log(sllDebug, 'DoExecute %: % abort sending %',
        [fn, ToText(nr)^, ToText(fContext.Frame^)], self);
      break;
    end;
  until Terminated;
  // Destroy will call fContext.Shutdown and remove the connection
  tix := mormot.core.os.GetTickCount64 - tix;
  if tix <> 0 then
    fLog.Log(sllDebug, 'DoExecute: % finished at %/s - connections=%/%',
      [fContext.FileName, KB((fFileSize * 1000) div tix),
       fOwner.ConnectionCount, fOwner.ConnectionTotal], self);
end;

procedure TTftpConnectionThread.NotifyShutdown;
begin
  fOwner := nil;
  Terminate;
end;


{ ******************** TTftpServerThread Server Class }

{ TTftpServerThread }

constructor TTftpServerThread.Create(const SourceFolder: TFileName;
  Options: TTftpThreadOptions; LogClass: TSynLogClass;
  const BindAddress, BindPort, ProcessName: RawUtf8; CacheTimeoutSecs: integer);
{$ifdef OSPOSIX}
var
  ok: boolean;
{$endif OSPOSIX}
begin
  fConnection := TSynObjectListLocked.Create({ownobject=}false);
  SetFileFolder(SourceFolder);
  fMaxConnections := 100; // = 100 threads, good enough for regular TFTP server
  fMaxRetry := 2;
  fOptions := Options;
  inherited Create(LogClass, BindAddress, BindPort, ProcessName, 5000); // bind
  {$ifdef OSPOSIX}
  if ttoDropPriviledges in fOptions then
  begin
    ok := DropPriviledges;
    if not ok then
      exclude(fOptions, ttoDropPriviledges);
    LogClass.Add.Log(LOG_INFOWARNING[not ok],
      'Create: DropPriviledges(nobody)=%', [ok], self);
  end;
  if ttoChangeRoot in fOptions then
  begin
    ok := ChangeRoot(StringToUtf8(ExcludeTrailingPathDelimiter(fFileFolder)));
    if ok then
      fFileFolder := '/'
    else
      exclude(fOptions, ttoDropPriviledges);
    LogClass.Add.Log(LOG_INFOWARNING[not ok],
      'Create: ChangeRoot(%)=%', [SourceFolder, ok], self);
  end;
  if ttoCaseInsensitiveFileName in fOptions then
    fPosixFileNames := TPosixFileCaseInsensitive.Create(
      SourceFolder, ttoAllowSubFolders in fOptions);
  {$endif OSPOSIX}
  if CacheTimeoutSecs > 0 then
    fFileCache := TSynDictionary.Create(
      TypeInfo(TFileNameDynArray), TypeInfo(TRawByteStringDynArray),
      PathCaseInsensitive, CacheTimeoutSecs);
end;

destructor TTftpServerThread.Destroy;
begin
  inherited Destroy;
  fFileCache.Free;
end;

procedure TTftpServerThread.OnShutdown;
begin
  // called by Executed on Terminated
  if fConnection = nil then
    exit;
  NotifyShutdown;
  FreeAndNil(fConnection);
  {$ifdef OSPOSIX}
  FreeAndNil(fPosixFileNames);
  {$endif OSPOSIX}
end;

procedure TTftpServerThread.NotifyShutdown;
var
  i: integer;
  t: ^TTftpConnectionThread;
begin
  if (self = nil) or
     (fConnection = nil) then
    exit;
  t := pointer(fConnection.List);
  for i := 1 to fConnection.Count do
  begin
    t^.NotifyShutdown; // also set fOwner=nil to avoid fConnection.Delete()
    inc(t);
  end;
end;

procedure TTftpServerThread.TerminateAndWaitFinished(TimeOutMs: integer);
var
  i: integer;
  endtix: Int64;
  t: ^TTftpConnectionThread;
begin
  endtix := mormot.core.os.GetTickCount64 + TimeOutMs;
  // first notify all sub threads to terminate
  NotifyShutdown;
  // shutdown and wait for main accept() thread
  inherited TerminateAndWaitFinished(TimeOutMs);
  // wait for sub threads finalization
  if fConnection = nil then
    exit;
  t := pointer(fConnection.List);
  for i := 1 to fConnection.Count do
  begin
    t^.TerminateAndWaitFinished(endtix - mormot.core.os.GetTickCount64);
    inc(t);
  end;
end;

function TTftpServerThread.GetConnectionCount: integer;
begin
  if (self = nil) or
     (fConnection = nil) then
    result := 0
  else
    result := fConnection.Count;
end;

procedure TTftpServerThread.SetFileFolder(const Value: TFileName);
begin
  if fFileFolder = Value then
    exit;
  fFileFolder := IncludeTrailingPathDelimiter(Value);
  {$ifdef OSPOSIX}
  fPosixFileNames.Folder := fFileFolder;
  {$endif OSPOSIX}
end;

function TTftpServerThread.GetFileName(const RequestedFileName: RawUtf8): TFileName;
var
  fn: TFileName;
  {$ifdef OSPOSIX}
  readms: integer;
  {$endif OSPOSIX}
begin
  result := '';
  fn := NormalizeFileName(Utf8ToString(RequestedFileName));
  if fn = '' then
    exit;
  while (fn[1] = '/') or
        (fn[1] = '\') do
    delete(fn, 1, 1); // trim any leading root (we start from fFileFolder anyway)
  if SafeFileName(fn) and
     ((ttoAllowSubFolders in fOptions) or
      (Pos(PathDelim, result) = 0)) then
  begin
    {$ifdef OSPOSIX}
    if Assigned(fPosixFileNames) then
    begin
      fn := fPosixFileNames.Find(fn, @readms);
      if (readms <> 0) and
         (ttoLowLevelLog in fOptions) then
        // e.g. 4392 filenames from /home/ab/dev/lib/ in 7.20ms
        fLog.Log(sllDebug, 'GetFileName: cached % filenames from % in %',
          [fPosixFileNames.Count, fFileFolder, MicroSecToString(readms)], self);
      if fn = '' then
        exit; // file does not exist
    end;
    {$endif OSPOSIX}
    result := fFileFolder + fn;
  end;
end;

const
  RRQ_FILE_MAX  = 1 shl 30;   // don't send files bigger than 1 GB
  RRQ_CACHE_MAX = 16 shl 20;  // cache files < 16MB in memory
  RRQ_MEM_CHUNK = 128 shl 10; // huge file is read buffered in 128KB chunks

function TTftpServerThread.SetRrqStream(var Context: TTftpContext): TTftpError;
var
  fsize: Int64;
  cached: RawByteString;
begin
  if ttoRrq in fOptions then
  begin
    if Context.FileStream = nil then
    begin
      result := teFileNotFound;
      if Context.FileNameFull = '' then // if not set by OnConnect() callback
        Context.FileNameFull := GetFileName(Context.FileName);
      if Context.FileNameFull = '' then
        exit;
      fsize := FileSize(Context.FileNameFull);
      if (fsize = 0) or
         (fsize >= RRQ_FILE_MAX) then
        exit;
      if Assigned(fFileCache) and
         (fsize < RRQ_CACHE_MAX) then
        if (not fFileCache.FindAndCopy(Context.FileNameFull, cached)) or
           (fsize <> length(cached)) then
        begin
          // not yet available in cache, or changed on disk
          cached := StringFromFile(Context.FileNameFull);
          fFileCache.AddOrUpdate(Context.FileNameFull, cached);
        end;
      if cached <> '' then
        Context.FileStream := TRawByteStringStream.Create(cached)
      else
        Context.FileStream := TBufferedStreamReader.Create(
                                Context.FileNameFull, RRQ_MEM_CHUNK);
    end;
    result := teNoError;
  end
  else
    result := teIllegalOperation;
end;

function TTftpServerThread.SetWrqStream(var Context: TTftpContext): TTftpError;
begin
  if ttoWrq in fOptions then
  begin
    if Context.FileStream = nil then
    begin
      result := teFileAlreadyExists;
      if Context.FileNameFull = '' then // if not set by OnConnect() callback
        Context.FileNameFull := GetFileName(Context.FileName);
      if (Context.FileNameFull = '') or
         FileExists(Context.FileNameFull) then
        exit;
      Context.FileStream := TFileStreamEx.Create(Context.FileNameFull, fmCreate);
    end;
    result := teNoError;
  end
  else
    result := teIllegalOperation;
end;

function TTftpServerThread.GetContextOptions: TTftpContextOptions;
begin
  result := [];
  if not (ttoNoBlksize in fOptions) then
    include(result, tcoBlksize);
  if not (ttoNoTimeout in fOptions) then
    include(result, tcoTimeout);
  if not (ttoNoTsize in fOptions) then
    include(result, tcoTsize);
  if not (ttoNoWindowsize in fOptions) then
    include(result, tcoWindowsize);
end;

procedure TTftpServerThread.OnFrameReceived(len: integer; var remote: TNetAddr);
var
  op: TTftpOpcode;
  c: TTftpContext;
  res: TTftpError;
  range, retry: integer;
  nr: TNetResult;
  local: TNetAddr;
begin
  // is called from TTftpServerThread.DoExecute context (so fLog is set)
  // with a RRQ/WRQ incoming UDP frame on port 69
  if len < 4 then
    exit;
  // validate incoming frame
  fFrame^[len] := 0; // ensure always 0 ended for StrLen() safety
  fLog.Log(sllDebug, 'OnFrameReceived: % %',
    [remote.IPShort, ToText(PTftpFrame(fFrame)^, len)], self);
  op := ToOpCode(PTftpFrame(fFrame)^);
  if not (op in [toRrq, toWrq]) then
    exit; // just ignore to avoid DoS on fuzzing
  if fConnection.Count >= fMaxConnections then
  begin
    // this request will be ignored with no ERR sent -> client will retry later
    fLog.Log(sllWarning, 'OnFrameReceived: Too Many Connections = %',
      [fConnection.Count], self);
    exit;
  end;
  FillCharFast(c, SizeOf(c), 0);
  // create new c.Sock
  c.Sock := remote.NewSocket(nlUdp);
  if c.Sock = nil then
  begin
    fLog.Log(sllError, 'OnFrameReceived: NewSocket failed as %',
      [NetLastErrorMsg], self);
    exit;
  end;
  // fix to a random port in supplied range if OS epĥemeral port is not enough
  if (fRangeLow or fRangeHigh) <> 0 then
  begin
    range := integer(fRangeHigh) - integer(fRangeLow);
    if (range >= 100) and
       (fRangeLow > 1024) then
    begin
      local := fSockAddr; // server address
      for retry := 1 to 50 do // avoid endless loop
      begin
        local.SetPort(fRangeLow + Random32(range));
        nr := local.SocketBind(c.Sock);
        if nr in [nrOk, nrInvalidParameter] then
          break;
      end;
      if nr <> nrOk then
        fLog.Log(sllWarning, 'OnFrameReceived: SocketBind(%..%) failed as %',
          [fRangeLow, fRangeHigh, NetLastErrorMsg], self);
    end
    else
      fLog.Log(sllWarning, ': invalid specified RangeLow..RangeHigh %..% ports',
        [fRangeLow, fRangeHigh], self);
  end;
  // main request parsing method (if TStream exists)
  c.Remote := remote;
  c.Frame := pointer(fFrame);
  res := c.ParseRequestFileName(len, GetContextOptions);
  // allow any kind of customization (e.g. c.FileNameFull)
  if Assigned(fOnConnect) and
     (res = teNoError) then
    res := fOnConnect(self, c);
  if res = teNoError then
  begin
    // create the associated TStream to read to or write from
    if op = toRrq then
      res := SetRrqStream(c)
    else
      res := SetWrqStream(c);
    if res = teNoError then
      // compute the toAck/toOck response
      res := c.ParseRequestOptions;
  end;
  // send back error frame and abort if needed
  if res <> teNoError then
  begin
    c.SendErrorAndShutdown(res, fLog, self, 'OnFrameReceived');
    exit;
  end;
  // send initial DAT/OACK response when request was validated
  if ttoLowLevelLog in fOptions then
    fLog.Log(sllTrace, 'OnFrameReceived send %',
      [ToText(c.Frame^, c.FrameLen)], self);
  nr := c.SendFrame;
  if nr = nrOk then
    // actual RRQ/WRQ transmission will take place on a dedicated thread
    fConnection.Add(TTftpConnectionThread.Create(c, self))
  else
  begin
    c.Shutdown;
    fLog.Log(sllDebug, 'OnFrameReceived: [%] sending %',
      [ToText(nr)^, ToText(c.Frame^)], self);
  end;
end;

procedure TTftpServerThread.OnIdle(tix64: Int64);
begin
  fFileCache.DeleteDeprecated(tix64);
  {$ifdef OSPOSIX}
  if fPosixFileNames <> nil then
    // refresh the fPosixFileNames cache from disk every minute
    fPosixFileNames.OnIdle(tix64);
  {$endif OSPOSIX}
end;



end.

