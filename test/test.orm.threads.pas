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
  mormot.crypt.core,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.log,
  mormot.core.perf,
  mormot.core.test,
  mormot.core.interfaces,
  mormot.crypt.secure,
  mormot.crypt.jwt,
  mormot.core.threads,
  mormot.net.client,
  mormot.net.server,
  mormot.net.relay,
  mormot.net.http,
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

{.$define FORCE_HTTP10}
{.$define FORCE_TCPONLY}

const
  MIN_THREADS = 1;
  MAX_THREADS = 50; // 1, 2, 5, 10, 30, 50
  MAX_CLIENTS = 50;

// unlikely to be implemented in the future (can't work from Services)
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
    fPendingThreadCount: integer;
    fPendingThreadFinished: TSynEvent;
    fHttpServer: TRestHttpServer;
    fMinThreads: integer;
    fMaxThreads: integer;
    fOperationCount: integer;
    fIterationTotalCount, fClientsTotalCount: integer;
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
    /// release used instances (e.g. server) and memory after all methods exec
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
{$ifdef FORCE_TCPONLY}
  public
{$endif FORCE_TCPONLY}
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
{$ifdef FORCE_TCPONLY}
  published
{$endif FORCE_TCPONLY}
    /// test via TRestHttpClientSocket instances over OS's socket API server
    // - note: Delphi IDE debugger may have trouble following the thread pool
    procedure TCPSockets;
{$ifdef FORCE_TCPONLY}
  public
{$endif FORCE_TCPONLY}
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

{
  Some Numbers taken on a Core i5 CPU with 2 Cores / 4 Threads:
  - Create thread pool: 1 assertion passed  3.49ms
  - TRestServerDB: 84,016 assertions passed  744.20ms
     1=74184/s  2=57643/s  5=59855/s  10=57237/s  30=57584/s  50=57811/s
  - TRestClientDB: 84,022 assertions passed  754.43ms
     1=69211/s  2=61700/s  5=60689/s  10=58181/s  30=55311/s  50=52662/s
  - TCP sockets: 83,970 assertions passed  2.04s
     1=17978/s  2=21060/s  5=22636/s  10=26235/s  30=28397/s  50=27965/s
  - Unix domain sockets: 83,998 assertions passed  2.05s
     1=15017/s  2=24857/s  5=29631/s  10=33961/s  30=35360/s  50=33696/s
  - Websockets: 83,927 assertions passed  6.83s
     1=8043/s  2=16269/s  5=18262/s  10=16404/s  30=6006/s  50=2060/s
  - Locked: 84,018 assertions passed  812.22ms
     1=40923/s  2=69135/s  5=59971/s  10=59818/s  30=55788/s  50=51902/s
  - Unlocked: 84,020 assertions passed  786.65ms
     1=68147/s  2=60392/s  5=55510/s  10=53308/s  30=53418/s  50=51896/s
  - Main thread: 83,996 assertions passed  785.41ms
     1=48828/s  2=60954/s  5=57680/s  10=60557/s  30=58928/s  50=55293/s
  - Background thread: 84,022 assertions passed  989.21ms
     1=50817/s  2=47593/s  5=46471/s  10=45656/s  30=36338/s  50=43257/s
   MaxThreads=50 MaxClients=500 TotalOps=188820 TotalClients=8820
}

{ TTestMultiThreadProcessThread }

type
  TTestMultiThreadProcessThread = class(TSynThread)
  protected
    fTest: TTestMultiThreadProcess;
    fID: integer;
    fEvent: TSynEvent;
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
  fEvent := TSynEvent.Create;
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
  infoUri, peopleUri: RawUtf8;
  http: THttpClientSocket;
  id: TID;
  log: ISynLog;
begin
  SetCurrentThreadName('% #%', [self, fID]);
  Rec := TOrmPeople.Create;
  try
    Rec.LastName := 'Thread ' + CardinalToHex(PtrUInt(GetCurrentThreadId));
    while not Terminated do
    begin
      fEvent.WaitForEver; // triggered from LaunchProcess
      if Terminated or
         fProcessFinished then // from Destroy
        break;
      infoUri := '';
      if fTest.fHttpServer <> nil then
        if fTest.fHttpServer.HttpServer.Router = nil then
        begin
          infoUri := '/root/timestamp/info';
          peopleUri := '/root/people/';
        end
        else
        begin
          // Route.Get('/info', '/root/timestamp/info');
          // Route.Get('/people/<id>', '/root/people/<id>');
          infoUri := '/info';
          peopleUri := '/people/';
        end;
      try
        try
          SetLength(Rest, fTest.ClientPerThread);
          for i := 0 to high(Rest) do
            Rest[i] := fTest.CreateClient;
          log := TSynLog.Enter('Execute %=% iterations=%',
            [Rest[0].ClassType, length(Rest), fIterationCount], self);
          if not fTest.CheckFailed(Rest <> nil) then
          begin
            n := 0;
            r := 0;
            log.Log(sllTrace, 'Execute Add', self);
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
            log.Log(sllTrace, 'Execute http.Get', self);
            if (infoUri <> '') and
               not IdemPChar(pointer(fTest.fClientOnlyPort), 'UNIX:') and
               (fTest.fHttpServer.Use in [useHttpSocket, useHttpAsync]) then
            begin
              http := OpenHttp('127.0.0.1', fTest.fClientOnlyPort);
              if not fTest.CheckFailed(http <> nil, 'openhttp') then
                try
                  fTest.CheckEqual(
                    http.Get(infoUri, 1000), HTTP_SUCCESS, infoUri);
                  fTest.CheckUtf8(http.Content <> '', infoUri);
                  for i := 0 to (n div 5) - 1 do
                  begin
                    fTest.CheckEqual(
                      http.Get(peopleUri + Int64ToUtf8(fIDs[i]), 1000),
                      HTTP_SUCCESS, peopleUri);
                    fTest.CheckUtf8(JsonGetID(pointer(Http.Content), id), peopleUri);
                    fTest.CheckEqual(id, fIDs[i]);
                  end;
                finally
                  http.Free;
                end;
            end;
            log.Log(sllTrace, 'Execute Retrieve', self);
            for i := 0 to n - 1 do
              if fTest.CheckFailed(Rest[r].Orm.Retrieve(fIDs[i], Rec), 'get') then
                break
              else
              begin
                fTest.Check(Rec.YearOfBirth = 1000 + i, 'yob');
                fTest.Check(Rec.YearOfDeath = (1040 + i) and $ffff, 'yod');
                //if (Rec.YearOfBirth<>1000+i) or (Rec.YearOfDeath<>1040+i) then writeln(i,'  ',ObjectToJSON(Rec));
                if r = high(Rest) then
                  r := 0
                else
                  inc(r);
              end;
            log.Log(sllTrace, 'Execute wait', self);
          end;
        finally
          log.Log(sllTrace, 'Execute finally', self);
          for i := 0 to high(Rest) do
            if Rest[i] <> fTest.fDatabase then
              FreeAndNil(Rest[i]);
          fProcessFinished := true;
          if InterlockedDecrement(fTest.fPendingThreadCount) = 0 then
            fTest.fPendingThreadFinished.SetEvent; // notify all finished
          log := nil;
        end;
      except
        on E: Exception do
          fTest.Check(False, E.Message);
      end;
    end;
  finally
    Rec.Free;
    fProcessFinished := true;
  end;
end;

procedure TTestMultiThreadProcessThread.LaunchProcess;
begin
  fProcessFinished := false;
  fIterationCount := fTest.fOperationCount div fTest.fRunningThreadCount;
  inc(fTest.fIterationTotalCount, fIterationCount);
  inc(fTest.fClientsTotalCount, fTest.fClientPerThread);
  fEvent.SetEvent; // launch work in Execute loop
end;


{ TTestMultiThreadProcess }

procedure TTestMultiThreadProcess.CleanUp;
begin
  DatabaseClose;
  if fThreads <> nil then
    AddConsole('MaxThreads=% MaxClients=% TotalOps=% TotalClients=%'
      {$ifdef FORCE_HTTP10} + ' HTTP1.0' {$endif},
      [fMaxThreads, fClientPerThread * fMaxThreads,
       fIterationTotalCount, fClientsTotalCount]);
  FreeAndNil(fModel);
  FreeAndNil(fThreads);
  FreeAndNil(fPendingThreadFinished);
end;

constructor TTestMultiThreadProcess.Create(
  Owner: TSynTests; const Ident: string);
begin
  inherited;
  fMinThreads := MIN_THREADS;
  fMaxThreads := MAX_THREADS;      // 1, 2, 5, 10, 30, 50
  fOperationCount := MAX_CLIENTS
    {$ifndef FORCE_HTTP10} * 7 {$endif}; // divided among threads
  fClientPerThread := MAX_CLIENTS div MAX_THREADS;
  // note: IterationCount := OperationCount div RunningThreadCount
  //       and make some round robin around fClientPerThread
  fPendingThreadFinished := TSynEvent.Create;
end;

const
  WS_FULLLOG = false;
  WS_KEY = 'wbsecpwd';
  WS_JSON = false;
  WS_BIN = [pboNoLocalHostEncrypt];

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
      ClientIP, ClientPort{%H-}, fModel);
    {$ifdef FORCE_HTTP10}
    TRestHttpClientGeneric(result).KeepAliveMS := 0; // force HTTP/1.0
    {$endif FORCE_HTTP10}
    TRestHttpClientGeneric(result).ServerTimestampSynchronize;
    if fTestClass = TRestHttpClientWebsockets then
      with (result as TRestHttpClientWebsockets) do
      begin
        if WS_FULLLOG then
          {%H-}WebSockets.Settings.SetFullLog;
        WebSocketsUpgrade(WS_KEY, WS_JSON, WS_BIN);
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
  longstandingclient: TRest;
  {$ifdef HAS_MESSAGES}
  aMsg: TMsg;
  {$endif HAS_MESSAGES}
begin
  if CheckFailed(fTestClass = nil) then
    exit;
  fTestClass := aClass;
  fClientOnlyPort := aPort;
  longstandingclient := nil;
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
      WebSocketLog := TSynLog;
      fHttpServer := TRestHttpServer.Create(aPort, [fDataBase], '+', aHttp,
        {threads=}8, secNone, '', '', HTTPSERVER_DEFAULT_OPTIONS + [rsoLogVerbose] );
      if aHttp in HTTP_BIDIR then
        fHttpServer.WebSocketsEnable(fDatabase, WS_KEY, WS_JSON, WS_BIN)^.SetFullLog;
      if aHttp in HTTP_SOCKET_MODES then // http.sys may have not /* registered
      begin
        fHttpServer.Route.Get('/info', '/root/timestamp/info');
        fHttpServer.Route.Get('/people/<id>', '/root/people/<id>');
      end;
      //writeln('server running on ',fDatabase.Model.Root,':',fHttpserver.Port); readln;
    end;
  end;
  // 2. Perform the tests
  if fTestClass.InheritsFrom(TRestHttpClientGeneric) then
    longstandingclient := CreateClient;
  fRunningThreadCount := fMinThreads;
  repeat
    // 2.1. Reset the DB content between loops
    if (fRunningThreadCount > 1) and
       (fDatabase <> nil) then
      fDatabase.DB.Execute('delete from people');
    if longstandingclient <> nil then
      Check(not longstandingclient.Orm.MemberExists(TOrmPeople,
        TTestMultiThreadProcessThread(fThreads.List[0]).fIDs[0]), 'client 1');
    // 2.2. Launch the background client threads
    fPendingThreadFinished.ResetEvent;
    fPendingThreadCount := fRunningThreadCount;
    //Write(fRunningThreadCount, ' ');
    fTimer.Start;
    for n := 0 to fRunningThreadCount - 1 do
      TTestMultiThreadProcessThread(fThreads[n]).LaunchProcess;
    //write('.');
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
        CheckSynchronize{$ifndef DELPHI6OROLDER}(1){$endif}
      else
        fPendingThreadFinished.WaitForEver;
      allFinished := true;
      for n := 0 to fRunningThreadCount - 1 do
        if not TTestMultiThreadProcessThread(fThreads.List[n]).fProcessFinished then
        begin
          allFinished := false;
          break;
        end;
    until allFinished;
    fTimer.Stop;
    //WriteLn(' ',fTimer.PerSec(fOperationCount * 2));
    fRunConsole := FormatString('%%=%/s  ',
      [fRunConsole, fRunningThreadCount, fTimer.PerSec(fOperationCount * 2)]);
    if longstandingclient <> nil then
      Check(longstandingclient.Orm.MemberExists(TOrmPeople,
        TTestMultiThreadProcessThread(fThreads.List[0]).fIDs[0]), 'client 2');
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
      {$ifdef CPUARM32}
      if fTestClass = TRestHttpClientWebsockets then
        break
      else
      {$endif CPUARM32}
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
  if longstandingclient <> nil then
  begin
    // validate server shutdown with connected client
    with TSynLog.Enter(longstandingclient, 'Free') do
      longstandingclient.Free;
  end;
end;

procedure TTestMultiThreadProcess.Locked;
begin
  Test(TRestClientDB, HTTP_DEFAULT_MODE, amLocked);
end;

procedure TTestMultiThreadProcess.Unlocked;
begin
  Test(TRestClientDB, HTTP_DEFAULT_MODE, amUnlocked);
end;

procedure TTestMultiThreadProcess.BackgroundThread;
begin
  Test(TRestClientDB, HTTP_DEFAULT_MODE, amBackgroundThread);
end;

procedure TTestMultiThreadProcess.MainThread;
begin
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
  Test(TRestHttpClientSocket, useHttpAsync);
end;

{$ifdef OSPOSIX}
procedure TTestMultiThreadProcess.UnixDomainSockets;
begin
  Test(TRestHttpClientSocket, useHttpAsync, amLocked,
    'unix:' + RawUtf8(ChangeFileExt(Executable.ProgramFileName, '.sock')));
end;
{$endif OSPOSIX}

procedure TTestMultiThreadProcess.Websockets;
begin
  // use a specific port, especially on Windows where http.sys may locked it
  Test(TRestHttpClientWebsockets, useBidirAsync, amLocked, '8888');
end;

{$ifdef USELIBCURL}
procedure TTestMultiThreadProcess._libcurl;
begin
  Test(TRestHttpClientCurl, useHttpAsync);
end;
{$endif USELIBCURL}

procedure TTestMultiThreadProcess._TRestClientDB;
begin
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
  Test(TRestServerDB);
end;


end.

