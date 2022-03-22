/// TFTP Server-Side Process 
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.tftp.server;

{
  *****************************************************************************

    TFTP Server Processing with RFC 1350/2347/2348/2349/7440 Support
    - Abstract UDP Server
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
  mormot.net.sock,
  mormot.net.tftp.client;



{ ******************** Abstract UDP Server }

type
  /// work memory buffer of the maximum size of UDP frame (64KB)
  TUdpFrame = array[word] of byte;

  /// pointer to a memory buffer of the maximum size of UDP frame
  PUdpFrame = ^TUdpFrame;

  /// abstract UDP server thread
  TUdpServerThread = class(TLoggedThread)
  protected
    fSock: TNetSocket;
    fSockAddr: TNetAddr;
    fExecuteMessage: RawUtf8;
    fFrame: PUdpFrame;
    /// will loop for any pending UDP frame, and execute FrameReceived method
    procedure DoExecute; override;
    // this is the main processing method for all incoming frames
    procedure OnFrameReceived(len: integer; var remote: TNetAddr); virtual; abstract;
    procedure OnShutdown; virtual; abstract;
    procedure OnIdle; virtual;
  public
    /// initialize and bind the server instance, in non-suspended state
    constructor Create(LogClass: TSynLogClass;
      const BindAddress, BindPort, ProcessName: RawUtf8); reintroduce;
    /// finalize the processing thread
    destructor Destroy; override;
  end;


{ ******************** TFTP Connection Thread and State Machine }

type
  /// tune what is TTftpThread accepting
  TTftpThreadOption = (
    ttoRrq,
    ttoWrq,
    ttoLowLevelLog);

  TTftpThreadOptions = set of TTftpThreadOption;

  TTftpServerThread = class;

  /// low-level TFTP process of a single connection
  TTftpConnectionThread = class(TLoggedThread)
  protected
    fContext: TTftpContext;
    fOwner: TTftpServerThread;
    fFrameMaxSize: integer;
    procedure DoExecute; override;
    procedure NotifyShutdown;
  public
    /// initialize this connection
    constructor Create(const Source: TTftpContext; Owner: TTftpServerThread); reintroduce;
    /// finalize this connection
    destructor Destroy; override;
  end;


{ ******************** TTftpServerThread Server Class }

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
    function GetConnectionCount: integer;
    // default implementation will read/write from FileFolder
    procedure SetFileFolder(const Value: TFileName);
    function GetFileName(const FileName: RawUtf8): TFileName; virtual;
    function SetRrqStream(var Context: TTftpContext): TTftpError; virtual;
    function SetWrqStream(var Context: TTftpContext): TTftpError; virtual;
    // main processing methods for all incoming frames
    procedure OnFrameReceived(len: integer; var remote: TNetAddr); override;
    procedure OnShutdown; override; // = Destroy
    procedure NotifyShutdown;
  public
    /// initialize and bind the server instance, in non-suspended state
    constructor Create(const SourceFolder: TFileName;
      Options: TTftpThreadOptions; LogClass: TSynLogClass;
      const BindAddress, BindPort, ProcessName: RawUtf8); reintroduce;
    /// notify the server thread to be terminated, and wait for finish
    procedure TerminateAndWaitFinished(TimeOutMs: integer = 5000); override;
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
  end;





implementation


{ ******************** Abstract UDP Server }

{ TUdpServerThread }

procedure TUdpServerThread.OnIdle;
begin
  // do nothing by default
end;

constructor TUdpServerThread.Create(LogClass: TSynLogClass;
  const BindAddress, BindPort, ProcessName: RawUtf8);
var
  ident: RawUtf8;
  res: TNetResult;
begin
  GetMem(fFrame, SizeOf(fFrame^));
  ident := ProcessName;
  if ident = '' then
    FormatUtf8('udp%srv', [BindPort], ident);
  res := NewSocket(BindAddress, BindPort, nlUdp, {bind=}true,
    5000, 5000, 5000, 10, fSock, @fSockAddr);
  if res <> nrOk then
    // on binding error, raise exception before the thread is actually created
    raise ENetSock.Create('%s.Create binding error on %s:%s',
      [ClassNameShort(self)^, BindAddress, BindPort], res);
  inherited Create({suspended=}false, LogClass, ident);
end;

destructor TUdpServerThread.Destroy;
begin
  TerminateAndWaitFinished;
  inherited Destroy;
  if fSock <> nil then
    fSock.ShutdownAndClose({rdwr=}true);
  FreeMem(fFrame);
end;

procedure TUdpServerThread.DoExecute;
var
  len: integer;
  tix, lasttix: cardinal;
  remote: TNetAddr;
  res: TNetResult;
begin
  fProcessing := true;
  lasttix := 0;
  // main server process loop
  try
    if fSock = nil then // paranoid check
      raise ENetSock.CreateFmt('%.Execute: Bind failed', [ClassNameShort(self)^]);
    while not Terminated do
    begin
      if fSock.WaitFor(1000, [neRead]) <> [] then
      begin
        if Terminated then
          break;
        res := fSock.RecvPending(len);
        if (res = nrOk) and
           (len >= 4) then
        begin
          PInteger(fFrame)^ := 0;
          len := fSock.RecvFrom(fFrame, SizeOf(fFrame^), remote);
          if len >= 0 then // -1=error, 0=shutdown
            OnFrameReceived(len, remote);
        end;
      end;
      tix := mormot.core.os.GetTickCount64 shr 9;
      if tix <> lasttix then
      begin
        lasttix := tix;
        OnIdle; // called every 512 ms at most
      end;
    end;
    OnShutdown; // should close all connections
  except
    on E: Exception do
      // any exception would break and release the thread
      FormatUtf8('% [%]', [E, E.Message], fExecuteMessage);
  end;
  fProcessing := false;
end;


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
  GetMem(fContext.Frame, fFrameMaxSize);
  inherited Create({suspended=}false, fOwner.LogClass, FormatUtf8('%%',
    [TFTP_OPCODE[Source.OpCode], InterlockedIncrement(TTftpConnectionThreadCounter)]));
end;

destructor TTftpConnectionThread.Destroy;
begin
  Terminate;
  fContext.Shutdown;
  if fOwner <> nil then
    fOwner.fConnection.Remove(self);
  inherited Destroy;
  FreeMem(fContext.Frame);
end;

procedure TTftpConnectionThread.DoExecute;
var
  len: integer;
  res: TTftpError;
  nr: TNetResult;
begin
  fLog.Log(sllDebug, 'DoExecute % % %',
    [fContext.Remote.IPShort({withport=}true), TFTP_OPCODE[fContext.OpCode],
     fContext.FileName], self);
  fContext.RetryCount := fOwner.MaxRetry;
  fContext.Sock.SetReceiveTimeout(1000); // check fTerminated every second
  repeat
    // try to receive a frame on this UDP/IP link
    len := fContext.Sock.RecvFrom(fContext.Frame, fFrameMaxSize, fContext.Remote);
    fLog.Log(sllTrace, 'DoExecute %', [ToText(fContext.Frame^)], self);
    if Terminated or
       (len = 0) then // -1=error, 0=shutdown
      break;
    if len < 0 then
    begin
      // network error (may be timeout)
      nr := NetLastError;
      if nr <> nrRetry then
      begin
        fLog.Log(sllTrace, 'DoExecute recvfrom=%', [ToText(nr)^], self);
        break;
      end;
      if mormot.core.os.GetTickCount64 >= fContext.TimeoutTix then
        // wait for incoming UDP packet within the timeout period
        continue;
      // retry after timeout
      if fContext.RetryCount = 0 then
        break;
      dec(fContext.RetryCount);
      // will send again the previous ACK/DAT frame
    end
    else
    begin
      // parse incoming DAT/ACK
      res := fContext.ParseData(len);
      if Terminated then
        break;
      if res <> teNoError then
      begin
        if res <> teFinished then
          // fatal error - e.g. teDiskFull
          fContext.SendErrorAndShutdown(res, fLog, 'DoExecute');
        break;
      end;
    end;
    // send next ACK or DAT block
    nr := fContext.SendFrame;
    if nr <> nrOk then
    begin
      fLog.Log(sllDebug, 'DoExecute: % abort sending %',
        [ToText(nr)^, ToText(fContext.Frame^)], self);
      break;
    end;
    fContext.RetryCount := fOwner.MaxRetry;
  until Terminated;
  // Destroy will call fContext.Shutdown
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
  const BindAddress, BindPort, ProcessName: RawUtf8);
begin
  fConnection := TSynObjectListLocked.Create({ownobject=}false);
  SetFileFolder(SourceFolder);
  fMaxConnections := 100; // = 100 threads, good enough for regular TFTP server
  fMaxRetry := 2;
  fOptions := Options;
  inherited Create(LogClass, BindAddress, BindPort, ProcessName);
end;

// note: no need to override Destroy since OnShutdown is eventually called
//       and will release fConnection list

procedure TTftpServerThread.OnShutdown;
begin
  // called by Executed on Terminated
  if fConnection = nil then
    exit;
  NotifyShutdown;
  FreeAndNil(fConnection);
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
    t^.NotifyShutdown;
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
  result := fConnection.Count;
end;

procedure TTftpServerThread.SetFileFolder(const Value: TFileName);
begin
  if fFileFolder <> Value then
    fFileFolder := IncludeTrailingPathDelimiter(Value);
end;

function TTftpServerThread.GetFileName(const FileName: RawUtf8): TFileName;
begin
  result := NormalizeFileName(Utf8ToString(FileName));
  if SafeFileName(result) then
    result := fFileFolder + result
  else
    result := '';
end;

const
  RRQ_MEM_CHUNK = 128 shl 10; // buffered read in 128KB chunks

function TTftpServerThread.SetRrqStream(var Context: TTftpContext): TTftpError;
var
  fn: TFileName;
begin
  if ttoRrq in fOptions then
  begin
    result := teFileNotFound;
    fn := GetFileName(Context.FileName);
    if (fn = '') or
       not FileExists(fn) then
      exit;
    Context.FileStream := TBufferedStreamReader.Create(fn, RRQ_MEM_CHUNK);
    if Context.FileStream.Size < maxInt then
      result := teNoError
    else
      FreeAndNil(Context.FileStream);
  end
  else
    result := teIllegalOperation;
end;

function TTftpServerThread.SetWrqStream(var Context: TTftpContext): TTftpError;
var
  fn: TFileName;
begin
  if ttoWrq in fOptions then
  begin
    result := teFileAlreadyExists;
    fn := GetFileName(Context.FileName);
    if (fn = '') or
       FileExists(fn) then
      exit;
    Context.FileStream := TFileStream.Create(fn, fmCreate);
    result := teNoError;
  end
  else
    result := teIllegalOperation;
end;

procedure TTftpServerThread.OnFrameReceived(len: integer; var remote: TNetAddr);
var
  op: TTftpOpcode;
  c: TTftpContext;
  res: TTftpError;
  nr: TNetResult;
begin
  // is called from TTftpServerThread.DoExecute context (so fLog is set)
  // with a RRQ/WRQ incoming UDP frame on port 69
  if len < 4 then
    exit;
  // validate incoming frame
  fLog.Log(sllTrace, 'OnFrameReceived: % %',
    [remote.IPShort, ToText(PTftpFrame(fFrame)^, len)], self);
  op := ToOpCode(PTftpFrame(fFrame)^);
  if not (op in [toRrq, toWrq]) then
    exit; // just ignore to avoid DoS on fuzzing
  if fConnection.Count >= fMaxConnections then
  begin
    // this request will be ignored with no ERR sent -> client will retry later
    fLog.Log(sllDebug, 'OnFrameReceived: Too Many Connections = %',
      [fConnection.Count], self);
    exit;
  end;
  FillCharFast(c, SizeOf(c), 0);
  // create new c.Sock on ephemeral port
  c.Sock := remote.NewSocket(nlUdp);
  if c.Sock = nil then
  begin
    fLog.Log(sllWarning, 'OnFrameReceived: NewSocket failed as %',
      [NetLastErrorMsg], self);
    exit;
  end;
  // main request parsing method (if TStream exists)
  c.Remote := remote;
  c.Frame := pointer(fFrame);
  res := c.ParseRequestFileName(len);
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
  // send back error frame if needed
  if res <> teNoError then
  begin
    c.SendErrorAndShutdown(res, fLog, 'OnFrameReceived');
    exit;
  end;
  // send initial DAT/OACK response when request was validated
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



end.

