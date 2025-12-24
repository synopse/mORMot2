unit server;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  Classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.rtti,
  mormot.core.unicode,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.soa.core,
  mormot.soa.server,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.http.server,
  mormot.rest.memserver,
  api.interfaces,
  api.impl;

type
  /// Service Registry Manager implementation
  /// Uses ACCESS CONTROL to dynamically enable/disable services at runtime
  /// This is the only way in mORMot2 to "turn off" a feature without restart
  TServiceRegistry = class(TInjectableObject, IServiceRegistry)
  private
    fServer: TRestServer;
    fEnabledServices: TStringList;  // Services that are currently "loaded"
    fLock: TRTLCriticalSection;
  public
    constructor Create(aServer: TRestServer);
    destructor Destroy; override;

    /// Check if a service is enabled (called from OnMethodExecute)
    function IsServiceEnabled(const ServiceName: RawUtf8): boolean;

    // IServiceRegistry implementation
    function ListServices: RawUtf8;
    function LoadService(const ServiceName: RawUtf8): boolean;
    function UnloadService(const ServiceName: RawUtf8): boolean;
    function GetServiceStatus(const ServiceName: RawUtf8): RawUtf8;
  end;

  /// Main server for Controllers Register demo
  /// Demonstrates dynamic service enable/disable via access control
  TControllersRegisterServer = class
  private
    fModel: TOrmModel;
    fRestServer: TRestServer;
    fHttpServer: TRestHttpServer;
    fServiceRegistry: TServiceRegistry;
    /// Callback for service method execution - implements access control
    function OnMethodExecute(Ctxt: TRestServerUriContext;
      const Method: TInterfaceMethod): boolean;
  public
    constructor Create(const aPort: RawUtf8);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  end;

implementation

{ TServiceRegistry }

constructor TServiceRegistry.Create(aServer: TRestServer);
begin
  inherited Create;
  fServer := aServer;
  fEnabledServices := TStringList.Create;
  fEnabledServices.Sorted := True;
  fEnabledServices.Duplicates := dupIgnore;
  fEnabledServices.CaseSensitive := False;
  InitializeCriticalSection(fLock);
end;

destructor TServiceRegistry.Destroy;
begin
  DeleteCriticalSection(fLock);
  fEnabledServices.Free;
  inherited;
end;

function TServiceRegistry.IsServiceEnabled(const ServiceName: RawUtf8): boolean;
begin
  EnterCriticalSection(fLock);
  try
    Result := fEnabledServices.IndexOf(Utf8ToString(ServiceName)) >= 0;
  finally
    LeaveCriticalSection(fLock);
  end;
end;

function TServiceRegistry.ListServices: RawUtf8;
var
  i: integer;
  list: RawUtf8;
begin
  EnterCriticalSection(fLock);
  try
    list := '[';
    for i := 0 to fEnabledServices.Count - 1 do
    begin
      if i > 0 then
        list := list + ',';
      list := list + '"' + StringToUtf8(fEnabledServices[i]) + '"';
    end;
    list := list + ']';
    Result := list;
  finally
    LeaveCriticalSection(fLock);
  end;
end;

function TServiceRegistry.LoadService(const ServiceName: RawUtf8): boolean;
begin
  Result := False;

  // Only allow known services
  if not ((ServiceName = 'Service1') or (ServiceName = 'Service2')) then
    exit;

  EnterCriticalSection(fLock);
  try
    if fEnabledServices.IndexOf(Utf8ToString(ServiceName)) < 0 then
    begin
      fEnabledServices.Add(Utf8ToString(ServiceName));
      TSynLog.Add.Log(sllInfo, 'Service ENABLED: %', [ServiceName]);
    end;
    Result := True;
  finally
    LeaveCriticalSection(fLock);
  end;
end;

function TServiceRegistry.UnloadService(const ServiceName: RawUtf8): boolean;
var
  idx: integer;
begin
  EnterCriticalSection(fLock);
  try
    idx := fEnabledServices.IndexOf(Utf8ToString(ServiceName));
    Result := idx >= 0;
    if Result then
    begin
      fEnabledServices.Delete(idx);
      TSynLog.Add.Log(sllInfo, 'Service DISABLED: %', [ServiceName]);
    end;
  finally
    LeaveCriticalSection(fLock);
  end;
end;

function TServiceRegistry.GetServiceStatus(const ServiceName: RawUtf8): RawUtf8;
begin
  if IsServiceEnabled(ServiceName) then
    Result := 'enabled'
  else
    Result := 'disabled';
end;

{ TControllersRegisterServer }

function TControllersRegisterServer.OnMethodExecute(
  Ctxt: TRestServerUriContext;
  const Method: TInterfaceMethod): boolean;
var
  serviceName: RawUtf8;
begin
  // Extract service name from the interface type
  // Method.InterfaceDotMethodName format is "IServiceX.MethodName"
  serviceName := Method.InterfaceDotMethodName;
  if Pos('.', serviceName) > 0 then
    serviceName := Copy(serviceName, 1, Pos('.', serviceName) - 1);

  // Remove the 'I' prefix to get the service name
  if (Length(serviceName) > 1) and (serviceName[1] = 'I') then
    Delete(serviceName, 1, 1);

  // ServiceRegistry is always accessible
  if serviceName = 'ServiceRegistry' then
  begin
    Result := True;
    exit;
  end;

  // Check if this service is enabled
  if fServiceRegistry.IsServiceEnabled(serviceName) then
  begin
    Result := True; // Allow execution
    TSynLog.Add.Log(sllDebug, 'Service call allowed: %', [Method.InterfaceDotMethodName]);
  end
  else
  begin
    Result := False; // Block execution
    Ctxt.Error('Service "%s" is currently disabled. Use ServiceRegistry/LoadService to enable it.',
      [serviceName], HTTP_FORBIDDEN);
    TSynLog.Add.Log(sllWarning, 'Service call BLOCKED (disabled): %', [Method.InterfaceDotMethodName]);
  end;
end;

constructor TControllersRegisterServer.Create(const aPort: RawUtf8);
var
  factory: TServiceFactoryServer;
begin
  inherited Create;

  // Create model (no ORM tables needed for interface-based services)
  fModel := TOrmModel.Create([]);

  // Create REST server (no database needed for this demo)
  fRestServer := TRestServerFullMemory.Create(fModel, False);
  fRestServer.CreateMissingTables;

  // Create service registry (must be created before registering services)
  fServiceRegistry := TServiceRegistry.Create(fRestServer);

  // Register ALL services at startup (required by mORMot2 RTTI)
  // Access control will determine which are actually accessible
  fRestServer.ServiceDefine(TService1, [IService1], sicShared);
  fRestServer.ServiceDefine(TService2, [IService2], sicShared);

  // Register the registry manager itself
  fRestServer.ServiceRegister(fServiceRegistry, [TypeInfo(IServiceRegistry)]);

  // Wire access control to ALL service factories
  factory := fRestServer.Services.Info(TypeInfo(IService1)) as TServiceFactoryServer;
  if Assigned(factory) then
    factory.OnMethodExecute := OnMethodExecute;

  factory := fRestServer.Services.Info(TypeInfo(IService2)) as TServiceFactoryServer;
  if Assigned(factory) then
    factory.OnMethodExecute := OnMethodExecute;

  // Pre-enable Service1 only (simulating DMVC's AddControllersInEngine)
  // Service2 starts disabled - must be enabled via LoadService
  fServiceRegistry.LoadService('Service1');

  // Create HTTP server
  fHttpServer := TRestHttpServer.Create(
    Utf8ToString(aPort),
    [fRestServer],
    '+',  // domain name
    useHttpSocket
  );

  fHttpServer.AccessControlAllowOrigin := '*'; // CORS for all origins

  TSynLog.Add.Log(sllInfo, 'Server created - Service1=enabled, Service2=disabled');
  TSynLog.Add.Log(sllInfo, 'Use ServiceRegistry/LoadService("Service2") to enable Service2');
end;

destructor TControllersRegisterServer.Destroy;
begin
  Stop;
  fHttpServer.Free;
  fRestServer.Free;
  fModel.Free;
  inherited;
end;

procedure TControllersRegisterServer.Start;
begin
  TSynLog.Add.Log(sllInfo, 'Server started');
end;

procedure TControllersRegisterServer.Stop;
begin
  TSynLog.Add.Log(sllInfo, 'Server stopped');
end;

end.
