/// multi-threaded regression tests for RESTful ORM
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit test.orm.threads;

interface

{$I ..\src\mormot.defines.inc}

uses
  sysutils,
  contnrs,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.core.crypto,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.log,
  mormot.core.perf,
  mormot.core.test,
  mormot.core.interfaces,
  mormot.core.secure,
  mormot.core.jwt,
  mormot.core.threads,
  mormot.net.client,
  mormot.net.server,
  mormot.net.relay,
  mormot.net.ws.core,
  mormot.net.ws.client,
  mormot.net.ws.server,
  mormot.db.core,
  mormot.db.nosql.bson,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.orm.storage,
  mormot.orm.sqlite3,
  mormot.orm.client,
  mormot.orm.server,
  mormot.soa.core,
  mormot.soa.server,
  mormot.rest.core,
  mormot.rest.client,
  mormot.rest.server,
  mormot.rest.memserver,
  mormot.rest.sqlite3,
  mormot.rest.http.client,
  mormot.rest.http.server,
  mormot.db.raw.sqlite3,
  mormot.db.raw.sqlite3.static,
  test.core.data,
  test.core.base,
  test.orm.core,
  test.orm.sqlite3;

// may be implemented in the future
{.$define HAS_NAMEDPIPES}
{.$define HAS_MESSAGES}

type
  /// a test case for multi-threading abilities of the framework
  // - will test all direct or remote access protocols with a growing number
  // of concurrent clients (1,2,5,10,30,50 concurent threads), to ensure
  // stability, scalibility and safety of the framework
  TTestMultiThreadProcess = class(TSynTestCase)
  protected
    fModel: TOrmModel;
    fDatabase: TRestServerDB;
    fTestClass: TRestClass;
    fThreads: TSynObjectList;
    fRunningThreadCount: integer;
    fHttpServer: TRestHttpServer;
    fMinThreads: integer;
    fMaxThreads: integer;
    fOperationCount: integer;
    fClientPerThread: integer;
    fClientOnlyServerIP, fClientOnlyPort: RawUtf8;
    fTimer: TPrecisionTimer;
    procedure DatabaseClose;
    procedure Test(aClass: TRestClass;
      aHttp: TRestHttpServerUse = HTTP_DEFAULT_MODE;
      aWriteMode: TRestServerAcquireMode = amLocked;
      const aPort: RawUtf8 = HTTP_DEFAULTPORT);
    function CreateClient: TRest;
  public
    /// create the test case instance
    constructor Create(Owner: TSynTests; const Ident: string = ''); override;
    /// release used instances (e.g. server) and memory
    procedure CleanUp; override;
    /// if not '', forces the test not to initiate any server and connnect to
    // the specified server IP address
    property ClientOnlyServerIP: RawUtf8
      read fClientOnlyServerIP write fClientOnlyServerIP;
    /// the minimum number of threads used for this test
    // - is 1 by default
    property MinThreads: integer
      read fMinThreads write fMinThreads;
    /// the maximum number of threads used for this test
    // - is 50 by default
    property MaxThreads: integer
      read fMaxThreads write fMaxThreads;
    /// how many Add() + Retrieve() operations are performed during each test
    // - is 200 by default, i.e. 200 Add() plus 200 Retrieve() globally
    property OperationCount: integer
      read fOperationCount write fOperationCount;
    /// how many TRest instance is initialized per thread
    // - is 1 by default
    property ClientPerThread: Integer
      read fClientPerThread write fClientPerThread;
  published
    /// initialize fDatabase and create MaxThreads threads for clients
    procedure CreateThreadPool;
    /// direct test of its RESTful methods
    procedure _TRestServerDB;
    /// test via TRestClientDB instances
    procedure _TRestClientDB;
    {$ifdef HAS_NAMEDPIPES}
    /// test via TRestClientURINamedPipe instances
    procedure _TRestClientURINamedPipe;
    {$endif HAS_NAMEDPIPES}
    {$ifdef HAS_MESSAGES}
    /// test via TRestClientURIMessage instances
    procedure _TRestClientURIMessage;
    {$endif HAS_MESSAGES}
    /// test via TRestHttpClientSocket instances over OS's socket API server
    // - note: Delphi IDE debugger may have trouble following the thread pool
    procedure TCPSockets;
    {$ifdef USEWININET}
    {$ifndef ONLYUSEHTTPSOCKET}
    /// test via TRestHttpClientWinHTTP instances over http.sys (HTTP API) server
    procedure WindowsAPI;
    {$endif ONLYUSEHTTPSOCKET}
    {$endif USEWININET}
    {$ifdef OSPOSIX}
    /// test via TRestHttpClientSocket instances over Unix Socket API server
    procedure UnixDomainSockets;
    {$endif OSPOSIX}
    //// test via TRestHttpClientWebsockets instances
    procedure Websockets;
    {$ifdef USELIBCURL}
    /// test via TRestHttpClientCurl using libcurl library
    procedure _libcurl;
    {$endif USELIBCURL}
    /// test via TRestClientDB instances with AcquireWriteMode=amLocked
    procedure Locked;
    /// test via TRestClientDB instances with AcquireWriteMode=amUnlocked
    procedure Unlocked;
    /// test via TRestClientDB instances with AcquireWriteMode=amMainThread
    procedure MainThread;
    /// test via TRestClientDB instances with AcquireWriteMode=amBackgroundThread
    procedure BackgroundThread;
  end;



implementation


{ TTestMultiThreadProcessThread }
type
  TTestMultiThreadProcessThread = class(TSynThread)
  protected
    fTest: TTestMultiThreadProcess;
    fID: integer;
    fEvent: TEvent;
    fIterationCount: integer;
    fProcessFinished: boolean;
    fIDs: TIntegerDynArray;
    procedure Execute; override;
    procedure LaunchProcess;
  public
    constructor Create(
      aTest: TTestMultiThreadProcess; aID: integer); reintroduce;
    destructor Destroy; override;
  end;


constructor TTestMultiThreadProcessThread.Create(
  aTest: TTestMultiThreadProcess; aID: integer);
begin
  FreeOnTerminate := false;
  fEvent := TEvent.Create(nil, false, false, '');
  fTest := aTest;
  fID := aID;
  SetLength(fIDs, fTest.fOperationCount);
  inherited Create(False);
end;

destructor TTestMultiThreadProcessThread.Destroy;
begin
  fProcessFinished := true;
  fEvent.SetEvent; // notify terminate
  Sleep(1); // is expected for proper process
  inherited Destroy;
  FreeAndNil(fEvent);
end;

procedure TTestMultiThreadProcessThread.Execute;
var
  Rest: array of TRest;
  Rec: TOrmPeople;
  i, n, r: PtrInt;
begin
  SetCurrentThreadName('% #%', [self, fID]);
  Rec := TOrmPeople.Create;
  try
    Rec.LastName := 'Thread ' + CardinalToHex(PtrUInt(GetCurrentThreadId));
    while not Terminated do
      case fEvent.WaitFor(INFINITE) of
        wrSignaled:
          if Terminated or
             fProcessFinished then // from Destroy
            break
          else
          try
            try
              SetLength(Rest, fTest.ClientPerThread);
              for i := 0 to high(Rest) do
                Rest[i] := fTest.CreateClient;
              if not fTest.CheckFailed(Rest <> nil) then
              begin
                n := 0;
                r := 0;
                for i := 0 to fIterationCount - 1 do
                begin
                  Rec.FirstName := FormatUTF8('%/%', [i, fIterationCount - 1]);
                  Rec.YearOfBirth := 1000 + i;
                  Rec.YearOfDeath := 1040 + i;
                  fIDs[i] := Rest[r].Orm.Add(Rec, true);
                  if r = high(Rest) then
                    r := 0
                  else
                    inc(r);
                  if fTest.CheckFailed(fIDs[i] <> 0, 'Rest.Add') then
                    break;
                  inc(n);
                end;
                for i := 0 to n - 1 do
                  if fTest.CheckFailed(Rest[r].Orm.Retrieve(fIDs[i], Rec)) then
                    break
                  else
                  begin
                    fTest.Check(Rec.YearOfBirth = 1000 + i);
                    fTest.Check(Rec.YearOfDeath = 1040 + i);
                    //if (Rec.YearOfBirth<>1000+i) or (Rec.YearOfDeath<>1040+i) then writeln(i,'  ',ObjectToJSON(Rec));
                    if r = high(Rest) then
                      r := 0
                    else
                      inc(r);
                  end;
              end;
            finally
              for i := 0 to high(Rest) do
                if Rest[i] <> fTest.fDatabase then
                  FreeAndNil(Rest[i]);
              fProcessFinished := true;
            end;
          except
            on E: Exception do
              fTest.Check(False, E.Message);
          end;
      end;
  finally
    Rec.Free;
  end;
  fProcessFinished := true;
end;

procedure TTestMultiThreadProcessThread.LaunchProcess;
begin
  fProcessFinished := false;
  fIterationCount := fTest.fOperationCount div fTest.fRunningThreadCount;
  fEvent.SetEvent;
  Sleep(0); // is expected for proper process
end;


{ TTestMultiThreadProcess }

procedure TTestMultiThreadProcess.CleanUp;
begin
  DatabaseClose;
  FreeAndNil(fModel);
  FreeAndNil(fThreads);
end;

constructor TTestMultiThreadProcess.Create(
  Owner: TSynTests; const Ident: string);
begin
  inherited;
  fMinThreads := 1;
  fMaxThreads := 50;
  fOperationCount := 200;
  fClientPerThread := 1;
end;

function TTestMultiThreadProcess.CreateClient: TRest;
var
  ClientIP, ClientPort: RawUtf8;
begin
  ClientIP := fClientOnlyServerIP;
  if ClientIP = '' then
    ClientIP := '127.0.0.1';
  if IdemPChar(pointer(fClientOnlyPort), 'UNIX:') then
    ClientIP := fClientOnlyPort
  else
    ClientPort := fClientOnlyPort;
  if fTestClass = TRestServerDB then
    result := fDatabase
  else
  {$ifdef HAS_NAMEDPIPES}
  if fTestClass = TRestClientURINamedPipe then
    result := TRestClientURINamedPipe.Create(fModel, 'test')
  else
  {$endif HAS_NAMEDPIPES}
  if fTestClass = TRestClientDB then
    result := TRestClientDB.Create(fDatabase)
  else
  {$ifdef HAS_MESSAGES}
  if fTestClass = TRestClientURIMessage then
  begin
    result := TRestClientURIMessage.Create(fModel, 'test',
      'Client' + IntToStr(GetCurrentThreadId), 1000);
    TRestClientURIMessage(result).DoNotProcessMessages := true;
  end
  else
  {$endif HAS_MESSAGES}
  if fTestClass.InheritsFrom(TRestHttpClientGeneric) then
  begin
    //writeln('New Client: ',ClientIP,':',ClientPort);
    result := TRestHttpClientGenericClass(fTestClass).Create(
      ClientIP, ClientPort, fModel);
    TRestHttpClientGeneric(result).ServerTimestampSynchronize;
    TRestHttpClientGeneric(result).CallBackGetResult('timestamp', []);
    if fTestClass = TRestHttpClientWebsockets then
      with (result as TRestHttpClientWebsockets) do
      begin
        WebSockets.Settings.SetFullLog;
        WebSocketsUpgrade('wskey');
      end;
  end
  else
    raise ESynException.CreateUTF8('Invalid fTestClass=%', [fTestClass]);
end;

procedure TTestMultiThreadProcess.CreateThreadPool;
var
  i: integer;
begin
  fModel := TOrmModel.Create([TOrmPeople]);
  fThreads := TSynObjectList.Create;
  for i := 1 to fMaxThreads do
    fThreads.Add(TTestMultiThreadProcessThread.Create(self, i));
  Check(fThreads.Count = fMaxThreads);
end;

procedure TTestMultiThreadProcess.DatabaseClose;
begin
  if fDatabase = nil then
    exit;
  fHttpServer.Shutdown;
  FreeAndNil(fHttpServer);
  FreeAndNil(fDatabase);
  fTestClass := nil;
end;

const
  TTESTMULTITHREADPROCESS_DBFILENAME = 'testMT.db3';

procedure TTestMultiThreadProcess.Test(aClass: TRestClass;
  aHttp: TRestHttpServerUse; aWriteMode: TRestServerAcquireMode;
  const aPort: RawUtf8);
var
  n: integer;
  i, j: integer;
  allFinished: boolean;
  Thread: TTestMultiThreadProcessThread;
  {$ifdef HAS_MESSAGES}
  aMsg: TMsg;
  {$endif HAS_MESSAGES}
begin
  if CheckFailed(fTestClass = nil) then
    exit;
  fTestClass := aClass;
  fClientOnlyPort := aPort;
  // 1. Prepare a new blank SQLite3 database in high speed mode
  if fClientOnlyServerIP = '' then
  begin
    DeleteFile(WorkDir + TTESTMULTITHREADPROCESS_DBFILENAME);
    if CheckFailed(
         not FileExists(WorkDir + TTESTMULTITHREADPROCESS_DBFILENAME)) or
       CheckFailed(aClass <> nil) then
      exit;
    fDatabase := TRestServerDB.Create(
      fModel, WorkDir + TTESTMULTITHREADPROCESS_DBFILENAME);
    fDatabase.AcquireWriteMode := aWriteMode;
    fDatabase.DB.Synchronous := smOff;
    fDatabase.DB.LockingMode := lmExclusive;
    fDatabase.NoAJAXJSON := true;
    fDatabase.Server.CreateMissingTables;
    {$ifdef HAS_NAMEDPIPES}
    if fTestClass = TRestClientURINamedPipe then
      fDatabase.ExportServerNamedPipe('test')
    else
    {$endif HAS_NAMEDPIPES}
    {$ifdef HAS_MESSAGES}
    if fTestClass = TRestClientURIMessage then
      fDatabase.ExportServerMessage('test')
    else
    {$endif HAS_MESSAGES}
    if fTestClass.InheritsFrom(TRestHttpClientGeneric) then
    begin
      fHttpServer := TRestHttpServer.Create(aPort, [fDataBase], '+', aHttp);
      if aHttp = useBidirSocket then
        fHttpServer.WebSocketsEnable(fDatabase, 'wskey').Settings.SetFullLog;
    end;
  end;
  // 2. Perform the tests
  fRunningThreadCount := fMinThreads;
  repeat
    // 2.1. Reset the DB content between loops
    if (fRunningThreadCount > 1) and
       (fDatabase <> nil) then
      fDatabase.DB.Execute('delete from people');
    // 2.2. Launch the background client threads
    fTimer.Start;
    for n := 0 to fRunningThreadCount - 1 do
    begin
      TTestMultiThreadProcessThread(fThreads[n]).LaunchProcess;
      sleep(10); // ensure thread process is actually started
    end;
    // 2.3. Wait for the background client threads process to be finished
    repeat
      {$ifdef HAS_MESSAGES}
      if (fTestClass = TRestClientURIMessage) or
         (fClientOnlyServerIP <> '') then
        while PeekMessage(aMsg, 0, 0, 0, PM_REMOVE) do
        begin
          TranslateMessage(aMsg);
          DispatchMessage(aMsg);
        end;
      {$endif HAS_MESSAGES}
      if (fDatabase <> nil) and
         (fDatabase.AcquireWriteMode = amMainThread) then
        CheckSynchronize{$ifndef DELPHI6OROLDER}(1){$endif};
      SleepHiRes(0);
      allFinished := true;
      for n := 0 to fRunningThreadCount - 1 do
        if not TTestMultiThreadProcessThread(fThreads.List[n]).fProcessFinished then
        begin
          allFinished := false;
          break;
        end;
    until allFinished;
    fTimer.Stop;
    fRunConsole := Format('%s%d=%d/s  ',
      [fRunConsole, fRunningThreadCount, fTimer.PerSec(fOperationCount * 2)]);
    // 2.4. Check INSERTed IDs consistency
    for n := 0 to fRunningThreadCount - 1 do
      with TTestMultiThreadProcessThread(fThreads.List[n]) do
        for i := 0 to fRunningThreadCount - 1 do
          if i <> n then
          begin
            Thread := fThreads.List[i];
            for j := 0 to high(fIDs) do
              if fIDs[j] > 0 then
                if IntegerScanExists(pointer(Thread.fIDs),
                     Thread.fIterationCount, fIDs[j]) then
                  Check(false, format('Duplicate ID %d for thread %d and %d',
                    [fIDs[j], i, n]));
          end;
    // 2.5. Execution sequence is with 1,2,5,10,30,50 concurent threads
    if fRunningThreadCount = 1 then
      fRunningThreadCount := 2
    else if fRunningThreadCount = 2 then
      fRunningThreadCount := 5
    else if fRunningThreadCount = 5 then
      {$ifdef HAS_NAMEDPIPES}
      if fTestClass = TRestClientURINamedPipe then
        break
      else
      {$endif HAS_NAMEDPIPES}
      {$ifdef CPUARM3264}
      if fTestClass = TRestHttpClientWebsockets then
        break
      else
      {$endif CPUARM3264}
        fRunningThreadCount := 10
    else
    {$ifdef HAS_MESSAGES}
    if fTestClass = TRestClientURIMessage then
      break
    else
    {$endif HAS_MESSAGES}
      fRunningThreadCount := fRunningThreadCount + 20;
  until fRunningThreadCount > fMaxThreads;
  // 3. Cleanup for this protocol (but reuse the same threadpool)
  DatabaseClose;
  Check(fDatabase = nil);
end;

procedure TTestMultiThreadProcess.Locked;
begin
  // 1=7310/s  2=8689/s  5=7693/s  10=3893/s  30=1295/s  50=777/s
  // (numbers are taken from a Xeon Phi 2 @ 1.5GHz with 288 cores)
  Test(TRestClientDB, HTTP_DEFAULT_MODE, amLocked);
end;

procedure TTestMultiThreadProcess.Unlocked;
begin
  // 1=7342/s  2=9400/s  5=7693/s  10=3894/s  30=1295/s  50=777/s
  Test(TRestClientDB, HTTP_DEFAULT_MODE, amUnlocked);
end;

procedure TTestMultiThreadProcess.BackgroundThread;
begin
  // 1=6173/s  2=7299/s  5=7244/s  10=3912/s  30=1301/s  50=777/s
  Test(TRestClientDB, HTTP_DEFAULT_MODE, amBackgroundThread);
end;

procedure TTestMultiThreadProcess.MainThread;
begin
  // 1=5000/s  2=5911/s  5=4260/s  10=2663/s  30=1126/s  50=707/s
  Test(TRestClientDB, HTTP_DEFAULT_MODE, amMainThread);
end;

{$ifdef USEWININET}
{$ifndef ONLYUSEHTTPSOCKET}
procedure TTestMultiThreadProcess.WindowsAPI;
begin
  Test(TRestHttpClientWinHTTP, useHttpApi);
end;
{$endif ONLYUSEHTTPSOCKET}
{$endif USEWININET}

procedure TTestMultiThreadProcess.TCPSockets;
begin
  // 1=2470/s  2=3866/s  5=3608/s  10=3556/s  30=1303/s  50=780/s
  Test(TRestHttpClientSocket, useHttpSocket);
end;

{$ifdef OSPOSIX}
procedure TTestMultiThreadProcess.UnixDomainSockets;
begin
  Test(TRestHttpClientSocket, useHttpSocket, amLocked,
    'unix:' + RawUtf8(ChangeFileExt(Executable.ProgramFileName, '.sock')));
end;
{$endif OSPOSIX}

procedure TTestMultiThreadProcess.Websockets;
begin
  // 1=2433/s  2=3389/s  5=3208/s  10=3354/s  30=1303/s  50=778/s
  Test(TRestHttpClientWebsockets, useBidirSocket);
end;

{$ifdef USELIBCURL}
procedure TTestMultiThreadProcess._libcurl;
begin
  // 1=48/s  2=95/s  5=234/s  10=433/s  30=729/s  50=594/s
  Test(TRestHttpClientCurl, useHttpSocket);
end;
{$endif USELIBCURL}

procedure TTestMultiThreadProcess._TRestClientDB;
begin
  // 1=7347/s  2=8100/s  5=7654/s  10=3898/s  30=1295/s  50=777/s
  Test(TRestClientDB);
end;

{$ifdef HAS_MESSAGES}
procedure TTestMultiThreadProcess._TRestClientURIMessage;
begin
  Test(TRestClientURIMessage);
end;
{$endif HAS_MESSAGES}

{$ifdef HAS_NAMEDPIPES}

procedure TTestMultiThreadProcess._TRestClientURINamedPipe;
begin
  Test(TRestClientURINamedPipe);
end;
{$endif HAS_NAMEDPIPES}

procedure TTestMultiThreadProcess._TRestServerDB;
begin
  // 1=9332/s  2=9300/s  5=7826/s  10=3891/s  30=1295/s  50=777/s
  Test(TRestServerDB);
end;


end.

