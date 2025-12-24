unit service.impl;

{$I mormot.defines.inc}

interface

uses
  {$ifdef OSWINDOWS}
  Windows,
  {$endif}
  SysUtils,
  Classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.rtti,
  mormot.orm.core,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.memserver,
  mormot.rest.http.server,
  mormot.soa.core,
  api.interfaces,
  api.impl;

type
  /// Windows Service implementation with mORMot2 REST server
  // - uses TServiceSingle from mormot.core.os for Windows Service functionality
  TRestServiceImpl = class(TServiceSingle)
  private
    fModel: TOrmModel;
    fServer: TRestServerFullMemory;
    fHttpServer: TRestHttpServer;
    fServiceApi: TServiceApi;
  protected
    procedure DoStart(Sender: TService);
    procedure DoStop(Sender: TService);
    procedure DoExecute(Sender: TService);
  public
    constructor Create(const aServiceName, aDisplayName: RawUtf8); override;
    destructor Destroy; override;
    // Public methods for console mode
    procedure StartRestServer;
    procedure StopRestServer;
  end;

implementation

{ TRestServiceImpl }

constructor TRestServiceImpl.Create(const aServiceName, aDisplayName: RawUtf8);
begin
  inherited Create(aServiceName, aDisplayName);

  // Set service event handlers
  OnStart := DoStart;
  OnStop := DoStop;
  OnExecute := DoExecute;

  TSynLog.Add.Log(sllInfo, 'Service % created', [aDisplayName]);
end;

destructor TRestServiceImpl.Destroy;
begin
  StopRestServer;
  inherited Destroy;
end;

procedure TRestServiceImpl.DoStart(Sender: TService);
begin
  TSynLog.Add.Log(sllInfo, 'Service OnStart event triggered');
  StartRestServer;
end;

procedure TRestServiceImpl.DoStop(Sender: TService);
begin
  TSynLog.Add.Log(sllInfo, 'Service OnStop event triggered');
  StopRestServer;
end;

procedure TRestServiceImpl.DoExecute(Sender: TService);
begin
  TSynLog.Add.Log(sllInfo, 'Service Execute event triggered');

  // The HTTP server runs in its own threads, so we don't need a loop here
  // The service will keep running until DoStop is called

  // In a real service, you might want to implement a monitoring loop here
  // For this sample, we just return and let the service framework handle the lifecycle

  TSynLog.Add.Log(sllInfo, 'Service Execute event completed');
end;

procedure TRestServiceImpl.StartRestServer;
const
  PORT = '8080';
begin
  if Assigned(fHttpServer) then
  begin
    TSynLog.Add.Log(sllWarning, 'REST server already running');
    Exit;
  end;

  TSynLog.Add.Log(sllInfo, 'Starting REST server on port %', [PORT]);

  try
    // Create model
    fModel := TOrmModel.Create([]);

    // Create server
    fServer := TRestServerFullMemory.Create(fModel, false);
    fServer.CreateMissingTables;

    // Register service with custom instance
    fServiceApi := TServiceApi.Create(StrToIntDef(PORT, 8080));
    fServer.ServiceRegister(fServiceApi, [TypeInfo(IServiceApi)]);

    // Create HTTP server
    fHttpServer := TRestHttpServer.Create(PORT, [fServer], '+', useBidirSocket);
    fHttpServer.AccessControlAllowOrigin := '*';

    TSynLog.Add.Log(sllInfo, 'REST server started successfully on port %', [PORT]);
  except
    on E: Exception do
    begin
      TSynLog.Add.Log(sllError, 'Failed to start REST server: %', [E.Message]);
      // Cleanup on error
      FreeAndNil(fHttpServer);
      FreeAndNil(fServer);
      FreeAndNil(fModel);
      FreeAndNil(fServiceApi);
      raise;
    end;
  end;
end;

procedure TRestServiceImpl.StopRestServer;
begin
  if not Assigned(fHttpServer) then
  begin
    TSynLog.Add.Log(sllDebug, 'REST server not running');
    Exit;
  end;

  TSynLog.Add.Log(sllInfo, 'Stopping REST server');

  try
    FreeAndNil(fHttpServer);
    FreeAndNil(fServer);
    FreeAndNil(fModel);
    FreeAndNil(fServiceApi);
    TSynLog.Add.Log(sllInfo, 'REST server stopped successfully');
  except
    on E: Exception do
      TSynLog.Add.Log(sllError, 'Error stopping REST server: %', [E.Message]);
  end;
end;

end.
