unit server.container;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  Classes,
  Generics.Collections,
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
  api.interfaces;

type
  /// Server configuration for container
  TServerConfig = record
    Name: RawUtf8;
    Port: RawUtf8;
    CalculatorClass: TClass;
  end;

  /// Individual server instance
  TServerInstance = class
  private
    fConfig: TServerConfig;
    fModel: TOrmModel;
    fServer: TRestServerFullMemory;
    fHttpServer: TRestHttpServer;
    fCalculator: ICalculatorApi;
  public
    constructor Create(const aConfig: TServerConfig);
    destructor Destroy; override;
    property Config: TServerConfig read fConfig;
  end;

  /// Container for multiple REST servers
  TServerContainer = class
  private
    fServers: TObjectList<TServerInstance>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddServer(const aName, aPort: RawUtf8; aCalculatorClass: TClass);
    procedure StartAll;
    procedure StopAll;
    function Count: Integer;
  end;

implementation

uses
  api.impl;

{ TServerInstance }

constructor TServerInstance.Create(const aConfig: TServerConfig);
var
  calculator: TCalculatorApi;
begin
  inherited Create;
  fConfig := aConfig;

  TSynLog.Add.Log(sllInfo, 'Creating server: % on port %', [aConfig.Name, aConfig.Port]);

  // Create model
  fModel := TOrmModel.Create([]);

  // Create server
  fServer := TRestServerFullMemory.Create(fModel, false);
  fServer.CreateMissingTables;

  // Create and register calculator instance
  if aConfig.CalculatorClass.InheritsFrom(TCalculatorApi) then
  begin
    calculator := TCalculatorApiClass(aConfig.CalculatorClass).Create(aConfig.Name);
    fCalculator := calculator; // Store as interface to keep reference
    // ServiceDefine with instance implies sicShared mode
    fServer.ServiceDefine(calculator, [ICalculatorApi]);
  end
  else
    raise Exception.CreateFmt('Invalid calculator class for %', [aConfig.Name]);

  // Create HTTP server
  fHttpServer := TRestHttpServer.Create(aConfig.Port, [fServer], '+', useBidirSocket);
  fHttpServer.AccessControlAllowOrigin := '*';

  TSynLog.Add.Log(sllInfo, 'Server % started on port %', [aConfig.Name, aConfig.Port]);
end;

destructor TServerInstance.Destroy;
begin
  TSynLog.Add.Log(sllInfo, 'Stopping server: %', [fConfig.Name]);
  FreeAndNil(fHttpServer);
  FreeAndNil(fServer);
  FreeAndNil(fModel);
  inherited;
end;

{ TServerContainer }

constructor TServerContainer.Create;
begin
  inherited;
  fServers := TObjectList<TServerInstance>.Create(True);
end;

destructor TServerContainer.Destroy;
begin
  StopAll;
  FreeAndNil(fServers);
  inherited;
end;

procedure TServerContainer.AddServer(const aName, aPort: RawUtf8; aCalculatorClass: TClass);
var
  config: TServerConfig;
begin
  config.Name := aName;
  config.Port := aPort;
  config.CalculatorClass := aCalculatorClass;
  fServers.Add(TServerInstance.Create(config));
end;

procedure TServerContainer.StartAll;
begin
  TSynLog.Add.Log(sllInfo, 'All servers started (% total)', [fServers.Count]);
end;

procedure TServerContainer.StopAll;
begin
  TSynLog.Add.Log(sllInfo, 'Stopping all servers...');
  fServers.Clear;
end;

function TServerContainer.Count: Integer;
begin
  Result := fServers.Count;
end;

end.
