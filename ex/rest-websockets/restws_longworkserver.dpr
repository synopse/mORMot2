/// simple SOA server using a callback for long process ending notification
// - this program is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
program restws_longworkserver;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$apptype console}
  {$R mormot.win.default.manifest.res}
{$endif OSWINDOWS}

uses
  {$I mormot.uses.inc}
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.rtti,
  mormot.core.log,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.memserver,
  mormot.rest.http.server,
  mormot.soa.core,
  mormot.soa.server,
  mormot.net.ws.core,
  restws.longworkshared in 'restws.longworkshared.pas';

type
  /// ILongWorkServer service implementation
  TLongWorkService = class(TInjectableObjectRest, ILongWorkService)
  protected
    fTotalWorkCount: Integer;
  public
    // ILongWorkServer methods
    procedure StartWork(const workName: string; const onFinish: ILongWorkCallback);
    function TotalWorkCount: Integer;
  end;

  // how the background work is done in a temporary thread
  /// - as initialized and run by TLongWorkService.StartWork
  TLongWorkServiceThread = class(TRestThread)
  protected
    fCallback: ILongWorkCallback;
    fWorkName: string;
    // this is the main execution: just a sleep() with some random timing
    procedure InternalExecute; override;
  public
    // when Execute is finished, will trigger callback WorkFinished/WorkFailed
    constructor Create(owner: TRest; const workName: string; const callback: ILongWorkCallback);
  end;


{ TLongWorkService }

procedure TLongWorkService.StartWork(
  const workName: string; const onFinish: ILongWorkCallback);
begin
  InterlockedIncrement(fTotalWorkCount);
  TLongWorkServiceThread.Create(fServer, workName, onFinish);
end;

function TLongWorkService.TotalWorkCount: Integer;
begin
  result := fTotalWorkCount;
end;


{ TLongWorkServiceThread }

constructor TLongWorkServiceThread.Create(owner: TRest;
  const workName: string; const callback: ILongWorkCallback);
begin
  inherited Create(owner, false, false);
  fCallback := callback;
  fWorkName := workName;
  FreeOnTerminate := true;
end;

procedure TLongWorkServiceThread.InternalExecute;
var
  tix: Int64;
begin
  TSynLog.Add.Log(sllInfo, '%.Execute(%) started', [self, fWorkName]);
  tix := GetTickCount64;
  Sleep(5000 + Random(1000)); // some hard work
  if Random(100) > 20 then
    fCallback.WorkFinished(fWorkName, GetTickCount64 - tix)
  else
    fCallback.WorkFailed(fWorkName, 'expected random failure');
  TSynLog.Add.Log(sllInfo, '%.Execute(%) notified', [self, fWorkName]);
end;


// this is the main block of this sample console server

procedure Run;
var
  HttpServer: TRestHttpServer;
  Server: TRestServerFullMemory;
begin
  // start a REST server with a table-less in-memory ORM store
  Server := TRestServerFullMemory.CreateWithOwnModel([]);
  try
    // setup of the ORM tables (not mandatory here)
    Server.CreateMissingTables;

    // register our ILongWorkService implementation as shared TLongWorkService
    Server.ServiceDefine(TLongWorkService, [ILongWorkService], sicShared).
      ByPassAuthentication := true;

    // publish this REST server over HTTP
    HttpServer := TRestHttpServer.Create(
      '8888', [Server], '+', WEBSOCKETS_DEFAULT_MODE);
    try
      // allow the HTTP server to upgrade to WebSockets with binary encryption
      HttpServer.WebSocketsEnable(Server, PROJECT31_TRANSMISSION_KEY).
        SetFullLog; // full verbose logs for this demo

      TextColor(ccLightGreen);
      writeln('WebSockets Long Work Server running on localhost:8888'#13#10);
      TextColor(ccWhite);
      writeln('Please compile and run one or several restws_longworkclient'#13#10);
      TextColor(ccLightGray);
      writeln('Press [Enter] to quit'#13#10);
      TextColor(ccCyan);
      readln;

    finally
      HttpServer.Free;
    end;
  finally
    Server.Free;
  end;
end;



begin
  with TSynLog.Family do
    begin // enable logging to file and to console
      Level := LOG_VERBOSE;
      EchoToConsole := LOG_VERBOSE;
      PerThreadLog := ptIdentifiedInOnFile;
    end;
  WebSocketLog := TSynLog; // verbose log of all WebSockets activity

  try
    Run;
  except
    on E: Exception do
      ConsoleShowFatalException(E);
  end;

  {$ifdef FPC_X64MM}
  WriteHeapStatus(' ', 16, 8, {compileflags=}true);
  {$endif FPC_X64MM}
end.

