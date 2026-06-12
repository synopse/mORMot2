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
  mormot.core.datetime,
  mormot.core.data,
  mormot.core.threads,
  mormot.core.log,
  mormot.core.rtti,
  mormot.core.buffers,
  mormot.core.json,
  mormot.net.sock,
  mormot.net.http,
  mormot.net.client, // for IHttpClient
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
  // - ttoHttpVerboseLog will log RedirectUri() HTTP proxy request for debugging
  // - ttoDropPriviledges on POSIX would impersonate the process as 'nobody' -
  // but note that it is incompatible with the AutoRebind := true feature with
  // any port < 1024 which requires root priviledged for socket binding
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
    ttoHttpVerboseLog,
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
  public
    /// initialize this connection
    constructor Create(const Source: TTftpContext; Owner: TTftpServerThread); reintroduce;
    /// finalize this connection
    destructor Destroy; override;
  end;


{ ******************** TTftpServerThread Server Class }

  /// registered information for one RedirectUri()
  TTftpServerRedirect = class(TObjectOSLightLock)
  public
    /// IdemPChar() lookup to locate this URI, ending with a '/' character
    up: RawUtf8;
    /// the associated remote URI, ending with a '/' character
    uri: RawUtf8;
    /// the shared main HTTP connection
    client: THttpClientSocket;
    /// maximum size of a resource to cache in memory
    memcachesize: integer;
    /// finalize this instance
    destructor Destroy; override;
  end;

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
    fRedirect: array of TTftpServerRedirect;
    fFileCache: TSynDictionary; // thread-safe <16MB files content cache
    {$ifdef OSPOSIX}
    fPosixFileNames: TPosixFileCaseInsensitive; // ttoCaseInsensitiveFileName
    {$endif OSPOSIX}
    fOnConnect: TOnTftpConnect;
    function GetConnectionCount: integer;
    function GetContextOptions: TTftpContextOptions;
    // default implementation will read/write from FileFolder
    procedure SetFileFolder(const Value: TFileName);
    function ParseUri(var Context: TTftpContext): TTftpError; virtual;
    function SetLocal(var Uri: RawUtf8; var Context: TTftpContext): TTftpError; virtual;
    function SetRemote(const Uri: RawUtf8; var Remote: TTftpServerRedirect;
      var Context: TTftpContext): TTftpError; virtual;
    procedure BackgroundGet(Sender: TObject);
    function SetRrqStream(var Context: TTftpContext): TTftpError; virtual;
    function SetWrqStream(var Context: TTftpContext): TTftpError; virtual;
    // main processing methods for all incoming frames
    procedure OnFrameReceived(len: integer; var remote: TNetAddr); override;
    procedure OnIdle(tix64: Int64); override; // called every second
    procedure OnShutdown; override; // from Destroy
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
    /// register one sub-URI folder pointing to a remote HTTP location
    // - return the internal >= 0 index in fRangeLow[], -1 on failure
    function RedirectUri(const UriPrefix, Remote: RawUtf8;
      MemCacheBytes: integer = 2 shl 20; Tls: PNetTlsContext = nil;
      ExtOptions: PHttpRequestExtendedOptions = nil;
      Schemes: TUriSchemes = [usHttp, usHttps]): PtrInt;
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

const
  RRQ_FILE_MAX  = 1 shl 30;   // don't send files bigger than 1 GB
  RRQ_CACHE_MAX = 2 shl 20;   // cache files < 2MB in memory
  RRQ_MEM_CHUNK = 128 shl 10; // huge files are read buffered in 128KB chunks



implementation


{ ******************** TFTP Connection Thread and State Machine }

{ TTftpConnectionThread }

var
  TTftpConnectionThreadCounter: integer; // to name the working threads

constructor TTftpConnectionThread.Create(
  const Source: TTftpContext; Owner: TTftpServerThread);
var
  name: RawUtf8;
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
  FormatUtf8('%#% % %', [TFTP_OPCODE[Source.OpCode],
    InterlockedIncrement(TTftpConnectionThreadCounter), fContext.FileName,
    KB(fFileSize)], name);
  inherited Create({suspended=}false, nil, nil, fOwner.LogClass, name);
end;

destructor TTftpConnectionThread.Destroy;
begin
  Terminate;
  fContext.Shutdown;
  if Assigned(fOwner.fConnection) then // may be nil from fOwner.Destroy
    fOwner.fConnection.Remove(self); // ownobject=false: just decrease Count
  inherited Destroy;
  FreeMem(fLastSent);
  FreeMem(fContext.Frame);
end;

procedure TTftpConnectionThread.DoExecute;
var
  len: integer;
  te: TTftpError;
  nr: TNetResult;
  ev: TNetEvents;
  tix: Int64;
  fn: RawUtf8;
begin
  tix := mormot.core.os.GetTickCount64;
  fLog.Log(sllDebug, 'DoExecute % % % as %',
    [fContext.Remote.IPShort({withport=}true), TFTP_OPCODE[fContext.OpCode],
     fContext.FileName, fContext.FileNameFull], self);
  fn := ExtractNameU(fContext.FileName); // exclude path in logs
  repeat
    // use poll/select and wait up to one second
    ev := fContext.Sock.WaitFor(1000, [neRead, neError]);
    if Terminated or
       (neError in ev) then // socket error (maybe ICMP error on Windows)
    begin
      fLogClass.Add.Log(sllWarning, 'DoExecute: abort after WaitFor', self);
      break;
    end;
    len := 0;
    if neRead in ev then
    begin
      // receive the pending data
      PInteger(fContext.Frame)^ := 0;
      len := fContext.Sock.RecvFrom(fContext.Frame, fFrameMaxSize, fContext.Remote);
      if Terminated then
        break;
      if len >= 0 then
      begin
        nr := nrOK;
        PByteArray(fContext.Frame)^[len] := 0; // #0 ended for StrLen() safety
      end
      else
        nr := NetLastError;
      if ttoLowLevelLog in fOwner.fOptions then
        fLog.Log(sllTrace, '% recv=% % %/%',
          [fn, _NR[nr], ToText(fContext.Frame^, len),
           CardinalToHexShort(fContext.CurrentSize), CardinalToHexShort(fFileSize)]);
      if (len <= 0) and
         (nr <> nrRetry) then // on Windows, ICMP error = WSAECONNRESET=nrClosed
      begin
        fLogClass.Add.Log(sllWarning, 'DoExecute: abort % after len=% RecvFrom=%',
          [fn, len, _NR[nr]], self);
        break; // len<0 = raw socket error, len=0 = ICMP error on POSIX
      end;
    end;
    if Terminated then
      break;
    if len <= 0 then
    begin
      // handle WaitFor() timeout
      if GetTickSec <= fContext.TimeoutTicks then // set by fContext.SendFrame
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
      // parse incoming len>0 DAT/ACK and generate the answer
      te := fContext.ParseData(len);
      if Terminated then
        break;
      if te <> teNoError then
      begin
        if te <> teFinished then
          // fatal error - e.g. teDiskFull
          fContext.SendErrorAndShutdown(te, fLog, self, 'DoExecute'); // and log
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
        [fn, _NR[nr], ToText(fContext.Frame^)], self);
      break;
    end;
  until Terminated;
  // thread/socket was aborted or we reached teFinished
  tix := mormot.core.os.GetTickCount64 - tix;
  if tix <> 0 then
    fLog.Log(sllDebug, 'DoExecute: % finished at %/s - shutdown=%',
      [fn, KB((fFileSize * 1000) div tix), BOOL_STR[Terminated]], self);
  // note: Destroy will call fContext.Shutdown and remove the connection
end;


{ ******************** TTftpServerThread Server Class }

{ TTftpServerRedirect }

destructor TTftpServerRedirect.Destroy;
begin
  FreeAndNil(client);
  inherited Destroy;
end;


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
  // bind and launch the thread to start serving content
  fAutoRebind := true; // make it resilient on raw socket errors
  inherited Create(LogClass, BindAddress, BindPort, ProcessName, 5000);
  // setup the execution parameters
  {$ifdef OSPOSIX}
  if ttoDropPriviledges in fOptions then
  begin
    ok := DropPriviledges;
    if not ok then
      exclude(fOptions, ttoDropPriviledges)
    else if fAutoRebind and
            (GetInteger(pointer(BindPort)) < 1024) then
      fAutoRebind := false; // only root can bind low ports
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
    fFileCache := TSynDictionary.Create({uri=}TypeInfo(TRawUtf8DynArray),
        {content=}TypeInfo(TRawByteStringDynArray),
      PathCaseInsensitive, CacheTimeoutSecs);
end;

destructor TTftpServerThread.Destroy;
begin
  inherited Destroy;
  fFileCache.Free;
  FreeAndNil(fConnection); // paranoid (usually done in OnShutdown)
  {$ifdef OSPOSIX}
  FreeAndNil(fPosixFileNames);
  {$endif OSPOSIX}
  ObjArrayClear(fRedirect, true);
end;

procedure TTftpServerThread.OnShutdown;
begin
  // called by Executed on Terminated or AutoRebind
  if fConnection = nil then
    exit;
  NotifyShutdown;
  fConnection.Clear;
end;

procedure TTftpServerThread.NotifyShutdown;
var
  i: integer;
  t: ^TTftpConnectionThread;
begin
  if (self = nil) or
     (fConnection = nil) then
    exit;
  fConnection.Safe.WriteLock;
  try
    t := pointer(fConnection.List);
    for i := 1 to fConnection.Count do
    try
      t^.Terminate;
      inc(t);
    except
      // ignore any exception here
    end;
  finally
    fConnection.Safe.WriteUnLock;
  end;
end;

procedure TTftpServerThread.TerminateAndWaitFinished(TimeOutMs: integer);
var
  endtix: Int64;
begin
  endtix := mormot.core.os.GetTickCount64 + TimeOutMs;
  // first notify all sub threads to terminate
  NotifyShutdown;
  // shutdown and wait for main accept() thread
  inherited TerminateAndWaitFinished(TimeOutMs);
  // wait for sub threads finalization
  if ConnectionCount <> 0 then
    repeat
      SleepHiRes(10);
    until (ConnectionCount = 0) or
          (mormot.core.os.GetTickCount64 > endtix);
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
  if Assigned(fPosixFileNames) then
    fPosixFileNames.Folder := fFileFolder;
  {$endif OSPOSIX}
end;

function TTftpServerThread.RedirectUri(const UriPrefix, Remote: RawUtf8;
  MemCacheBytes: integer; Tls: PNetTlsContext;
  ExtOptions: PHttpRequestExtendedOptions; Schemes: TUriSchemes): PtrInt;
var
  one: TTftpServerRedirect;
  opt: THttpRequestExtendedOptions;
  client: THttpClientSocket;
  up: RawUtf8;
  onlog: TSynLogProc;
  u: TUri;
begin
  result := -1;
  if not (ttoRrq in fOptions) then
  begin
    fLog.Log(sllWarning, 'RedirectUri with no RRQ support', self);
    exit;
  end;
  // validate and prepare the UriPrefix parameter
  up := UpperCase(StringReplaceChars(TrimU(UriPrefix), '\', '/'));
  if (up = '') or
     not IsAnsiCompatible(pointer(up)) then
  begin
    fLog.Log(sllWarning, 'RedirectUri uri=% failed', [UriPrefix], self);
    exit;
  end;
  AppendIfNone(up, '/');
  // validate the Remote address parameter
  if not u.From(Remote) or
     not (u.UriScheme in Schemes) then
  begin
    fLog.Log(sllWarning, 'RedirectUri remote=% failed', [Remote], self);
    exit;
  end;
  AppendIfNone(u.Address, '/');
  // ensure the remote URI is valid by connecting to the server
  if ExtOptions = nil then
  begin
    opt.Init; // all to 0, including RedirectMax=0
    opt.RecreateConnectionAfterSecs := 30; // 30 secs idle -> reopen
    if Tls <> nil then
      opt.Tls := Tls^;
  end
  else
    opt := ExtOptions^;
  onlog := nil;
  if (ttoHttpVerboseLog in fOptions) and
     Assigned(fLogClass) then
    onlog := fLogClass.DoLog;
  try
    client := THttpClientSocket.OpenOptions(u, opt, onlog);
  except
    on E: Exception do
    begin
      fLog.Log(sllWarning, 'RedirectUri OpenUri=% failed as %',
               [Remote, E], self);
      exit;
    end;
  end;
  // append to the internal list
  one := TTftpServerRedirect.Create;
  one.up := up;
  one.uri := u.URI;
  one.client := client;
  one.memcachesize := MemCacheBytes;
  result := ObjArrayAdd(fRedirect, one);
  fLog.Log(sllDebug, 'RedirectUri=% uri=% remote=%', [result, up, one.uri], self);
end;

type
  // additional context parameters to background GET into TPipeStream.Write
  TFtpHttpClientSocket = class(THttpClientSocket)
  public
    Url: RawUtf8;
    Thread: TLoggedWorkThread;
    Stream: TStream;
  end;

function TTftpServerThread.SetRemote(const Uri: RawUtf8;
  var Remote: TTftpServerRedirect; var Context: TTftpContext): TTftpError;
var
  url: RawUtf8;
  cached: RawByteString;
  size: Int64;
  status: integer;
  c: TFtpHttpClientSocket;
begin
  // check and compute the full remote URL
  result := teFileNotFound;
  if (Uri = '') or
     not IsAnsiCompatible(pointer(Uri)) then
    exit;
  Join([Remote.uri, Uri], url);
  Remote.Lock; // protect main Remote.client connection (paranoid)
  try
    // always perform a HEAD to the remote server and retrieve the resource size
    status := Remote.client.Head(url, 10000);
    size := Remote.client.ContentLength;
    fLog.Log(sllTrace, 'SetRemote: % HEAD=% size=%', [Uri, status, size], self);
    if (status <> HTTP_SUCCESS) or
       (size <= 0) or
       (size >= RRQ_FILE_MAX) then
      exit;
    // now we can return this resource
    if size > Remote.memcachesize then
    begin
      // big files (>2MB by default) require their own HTTP connection and thread
      c := nil;
      try
        c := TFtpHttpClientSocket.OpenFrom(Remote.client); // new socket connect
        c.Url := url;
        c.Thread := TLoggedWorkThread.Create(              // background thread
          fLogClass, url, c, BackgroundGet, {suspended=}true);
      except
        on E: Exception do
        begin
          fLog.Log(sllWarning, 'RedirectUri OpenFrom=% failed as %',
                   [url, E], self);
          c.Free;
          exit;
        end;
      end;
      c.Stream := TBackgroundPipeStream.Create(c.Thread);
      c.Thread.Start;
      fLog.Log(sllTrace, 'SetRemote: started background GET', self);
      Context.FileStream := c.Stream;
    end
    else
    begin
      // smallest files: first check from in-memory cache
      if (not fFileCache.FindAndCopy(url, cached)) or
         (size <> length(cached)) then
      begin
        // if unknown, download and cache from the main HTTP connection
        status := Remote.client.Get(url, 10000);
        cached := Remote.client.Content;
        fLog.Log(sllTrace, 'SetRemote: GET=% size=%', [status, length(cached)], self);
        if (status <> HTTP_SUCCESS) or
           (length(cached) <> size) then
          exit; // invalid download
        fFileCache.AddOrUpdate(url, cached);
      end
      else
        fLog.Log(sllTrace, 'SetRemote: size=% from cache', [length(cached)], self);
      Context.FileStream := TRawByteStringStream.Create(cached);
    end;
    // success
    Utf8ToFileName(url, Context.FileNameFull); // for logging and debug
    result := teNoError;
  finally
    Remote.Unlock;
  end;
end;

procedure TTftpServerThread.BackgroundGet(Sender: TObject);
var
  c: TFtpHttpClientSocket absolute Sender;
  status: integer;
begin
  try
    status := c.Request(c.Url, 'GET', 30000, '', '', '', {asretry=}false,
      {instream=}nil, {outstream=}c.Stream);
    fLog.Log(sllDebug, 'BackgroundGet=% size=%', [status, c.ContentLength], self);
  finally
    c.Free;
  end;
end;

function TTftpServerThread.SetLocal(var Uri: RawUtf8;
  var Context: TTftpContext): TTftpError;
var
  fn: TFileName;
  cached: RawByteString;
  size: Int64;
  {$ifdef OSPOSIX}
  readms: integer;
  {$endif OSPOSIX}
begin
  result := teFileNotFound;
  Utf8ToFileName(Uri, fn);
  // check the actual file on disk and create a proper Context.FileStream
  {$ifdef OSPOSIX}
  if Assigned(fPosixFileNames) then
  begin
    fn := fPosixFileNames.Find(fn, @readms, @Uri); // convert to actual casing
    if readms <> 0 then
      // e.g. 4392 filenames from /home/ab/dev/lib/ in 7.20ms
      fLog.Log(sllDebug, 'SetLocal: cached % filenames from % in %',
        [fPosixFileNames.Count, fFileFolder, MicroSecToString(readms)], self);
  end;
  {$endif OSPOSIX}
  case Context.OpCode of
    toRrq:
      begin
        if fn = '' then
          exit; // file does not exist after fPosixFileNames lookup
        fn := fFileFolder + fn;
        size := FileSize(fn);
        if (size = 0) or
           (size >= RRQ_FILE_MAX) then
          exit;
        // handle optional file cache in memory
        if Assigned(fFileCache) and
           (size < RRQ_CACHE_MAX) then // < 2MB by default in memory cache
        begin
          if fFileCache.FindAndCopy(Uri, cached) and
             (size = length(cached)) then
            fLog.Log(sllTrace, 'SetLocal: % size=% from cache',
              [Uri, length(cached)], self)
          else
          begin
            // not yet available in cache, or changed on disk
            cached := StringFromFile(fn);
            fFileCache.AddOrUpdate(Uri, cached);
            fLog.Log(sllTrace, 'SetLocal: % size=% to cache',
              [Uri, length(cached)], self);
          end;
          Context.FileStream := TRawByteStringStream.Create(cached);
        end
        else
          // return big files directly from disk, with 128KB buffering
          Context.FileStream := TBufferedStreamReader.Create(fn, RRQ_MEM_CHUNK);
      end;
    toWrq:
      begin
        result := teFileAlreadyExists;
        fn := fFileFolder + fn;
        if FileExists(fn) then
          exit;
        Context.FileStream := TFileStreamEx.Create(fn, fmCreate);
      end;
  else
    begin
      result := teIllegalOperation;
      exit;
    end;
  end;
  // success
  Context.FileNameFull := fn; // for logging and debug
  result := teNoError;
end;

function TTftpServerThread.ParseUri(var Context: TTftpContext): TTftpError;
var
  i: PtrInt;
  r: ^TTftpServerRedirect;
  u: RawUtf8;
begin
  result := teFileNotFound;
  // validate the input resource
  u := Context.FileName;
  NormalizeFileNameU(u);
  TrimFirstChar(u, PathDelim); // trim any leading path delimiter
  if (u = '') or
     not SafeFileNameU(u) then
    exit;
  // try any RedirectUri() registration
  if Context.OpCode = toRrq then
  begin
    r := pointer(fRedirect);
    if r <> nil then
      for i := 1 to length(fRedirect) do
      begin
        if IdemPChar(pointer(u), pointer(r^.up)) then
        begin
          delete(u, 1, length(r^.up)); // trim /local/uri/
          result := SetRemote(u, r^, Context);
          exit;
        end;
        inc(r);
      end;
  end;
  // download/upload this file from the main FileFolder
  if (ttoAllowSubFolders in fOptions) or
     (PosExChar(PathDelim, u) = 0) then
    result := SetLocal(u, Context);
end;

function TTftpServerThread.SetRrqStream(var Context: TTftpContext): TTftpError;
begin
  if ttoRrq in fOptions then
    if Context.FileStream <> nil then
      // OnConnect() callback have set something
      result := teNoError
    else
      // parse Context.FileName and call SetLocal/SetRemote to set FileStream
      result := ParseUri(Context)
  else
    // ensure this server is not configured to allow RRQ
    result := teIllegalOperation;
end;

function TTftpServerThread.SetWrqStream(var Context: TTftpContext): TTftpError;
begin
  if ttoWrq in fOptions then
    if Context.FileStream <> nil then
      // OnConnect() callback have set something
      result := teNoError
    else
      // parse Context.FileName and call SetLocal to set FileStream
      result := ParseUri(Context)
  else
    // ensure this server is not configured to allow WRQ
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
  if (fConnection = nil) or
     not (op in [toRrq, toWrq]) then
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
  // allow any kind of customization (e.g. c.FileNameFull/FileStream)
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
      [_NR[nr], ToText(c.Frame^)], self);
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

