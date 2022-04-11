program httpServerRaw;

{

  usr@pc:~ sudo apt install wrk
  usr@pc:~ wrk -c 10000 -t 8  http://localhost:8888/echo

  Running 10s test @ http://localhost:8888/echo
    8 threads and 10000 connections
    Thread Stats   Avg      Stdev     Max   +/- Stdev
      Latency     4.77ms    4.21ms 417.16ms   87.83%
      Req/Sec    20.99k    10.12k   64.65k    77.22%
    1644051 requests in 10.09s, 246.95MB read
    Socket errors: connect 8987, read 0, write 0, timeout 0
  Requests/sec: 162907.07
  Transfer/sec:     24.47MB

}

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$apptype console}
  {$R mormot.win.default.manifest.res}
{$endif OSWINDOWS}

uses
  {$I mormot.uses.inc} // may include mormot.core.fpcx64mm.pas
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.log,
  mormot.core.text,
  mormot.net.http,
  mormot.net.server,
  mormot.net.async;

  
type
  TSimpleHttpAsyncServer = class
  private
    fHttpServer: THttpServerSocketGeneric;
  protected
    // this is where the process would take place
    function DoOnRequest(Ctxt: THttpServerRequestAbstract): cardinal;
  public
    constructor Create;
    destructor Destroy; override;
  end;


{ TSimpleHttpAsyncServer }

function TSimpleHttpAsyncServer.DoOnRequest(Ctxt: THttpServerRequestAbstract): cardinal;
begin
  if fHttpServer is THttpAsyncServer then
    with THttpAsyncServer(fHttpServer) do
    if Assigned(Async.Log) then
      Async.Log.Add.Log(sllInfo, 'DoOnRequest %', [Ctxt], self);
  if Ctxt.Method = 'GET' then
    Ctxt.OutContent := FormatUtf8('got % request #% from connection #%',
      [Ctxt.Url, CardinalToHexShort(Ctxt.RequestID), CardinalToHexShort(Ctxt.ConnectionID)])
    // we use CardinalToHexShort() to keep the same length (as request by ab)
  else
    Ctxt.OutContent := Ctxt.InContent;  // POST = echo
  Ctxt.OutContentType := TEXT_CONTENT_TYPE;
  result := HTTP_SUCCESS;
end;

constructor TSimpleHttpAsyncServer.Create;
begin
  inherited Create;
  fHttpServer := THttpAsyncServer.Create(
    '8888', nil, nil, '', 4, 30000,
    [hsoNoXPoweredHeader
    //, hsoLogVerbose
    ]);
  //writeln('DropPriviledges=', DropPriviledges('abouchez'));
  fHttpServer.HttpQueueLength := 100000; // needed e.g. from wrk/ab benchmarks
  fHttpServer.OnRequest := DoOnRequest;
  fHttpServer.WaitStarted; // raise exception e.g. on binding issue
end;

destructor TSimpleHttpAsyncServer.Destroy;
begin
  fHttpServer.Free;
  inherited Destroy;
end;

var
  simpleServer: TSimpleHttpAsyncServer;

begin
  with TSynLog.Family do
    begin
      Level :=
        //LOG_VERBOSE +
        LOG_FILTER[lfErrors];
      //EchoToConsole := Level;
      PerThreadLog := ptIdentifiedInOneFile;
      HighResolutionTimestamp := true;
    end;

  simpleServer := TSimpleHttpAsyncServer.Create();
  try
    {$I-}
    writeln;
    TextColor(ccLightGreen);
    writeln(simpleServer.fHttpServer.ClassName, ' running on localhost:8888'#10);
    TextColor(ccWhite);
    writeln('try curl http://localhost:8888/echo'#10);
    TextColor(ccLightGray);
    writeln('Press [Enter] to quit'#10);
    TextColor(ccCyan);
    //sleep(10000);
    ConsoleWaitForEnterKey;
    writeln(ObjectToJson(simpleServer.fHttpServer, [woHumanReadable]));
    TextColor(ccLightGray);
    {$ifdef FPC_X64MM}
    WriteHeapStatus(' ', 16, 8, {compileflags=}true);
    {$endif FPC_X64MM}
  finally
    simpleServer.Free;
  end;

end.

