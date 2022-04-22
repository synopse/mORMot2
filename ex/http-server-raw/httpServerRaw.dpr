program httpServerRaw;

{
  usr@pc:~ sudo apt install wrk
  usr@pc:~ wrk -c 1000 -t 4  http://localhost:8888/echo
  Running 10s test @ http://localhost:8888/echo
    4 threads and 1000 connections
    Thread Stats   Avg      Stdev     Max   +/- Stdev
      Latency     5.46ms    2.01ms  22.48ms   72.35%
      Req/Sec    40.34k     4.09k   65.62k    74.24%
    1600804 requests in 10.06s, 277.85MB read
  Requests/sec: 159093.08
  Transfer/sec:     27.61MB

  usr@pc:~ sudo apt install apache2-utils
  usr@pc:~ ab -t 20 -n 100000 -c 1000 -k http://127.0.0.1:8888/echo
  This is ApacheBench, Version 2.3 <$Revision: 1879490 $>

  Server Software:        mORMot2
  Server Hostname:        127.0.0.1
  Server Port:            8888

  Document Path:          /echo
  Document Length:        53 bytes

  Concurrency Level:      1000
  Time taken for tests:   1.057 seconds
  Complete requests:      100000
  Failed requests:        0
  Keep-Alive requests:    100000
  Total transferred:      18200000 bytes
  HTML transferred:       5300000 bytes
  Requests per second:    94575.35 [#/sec] (mean)
  Time per request:       10.574 [ms] (mean)
  Time per request:       0.011 [ms] (mean, across all concurrent requests)
  Transfer rate:          16809.29 [Kbytes/sec] received

  Connection Times (ms)
                min  mean[+/-sd] median   max
  Connect:        0    0   1.9      0      41
  Processing:     1    8  10.4      7     165
  Waiting:        0    8  10.4      7     165
  Total:          1    8  10.6      7     165

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
      [Ctxt.Url, CardinalToHexShort(Ctxt.RequestID),
       CardinalToHexShort(Ctxt.ConnectionID)])
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

