unit server;

{$I mormot.defines.inc}

interface

uses
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.log,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.soa.core,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.http.server,
  mormot.rest.memserver,
  api.interfaces,
  api.impl,
  authentication;

type
  /// Basic Authentication Server
  // Port of DMVC WebModule1 and RunServer with authentication middleware
  TBasicAuthServer = class
  protected
    fRestServer: TRestServerFullMemory;
    fHttpServer: TRestHttpServer;
    fBasicAuthApi: TBasicAuthApi;

    /// Authentication filter - intercepts all requests
    // Port of TMVCBasicAuthenticationMiddleware
    function OnBeforeBody(Ctxt: TRestServerUriContext): Boolean;
  public
    constructor Create(const aPort: RawUtf8 = '8080'); reintroduce;
    destructor Destroy; override;

    /// Start the HTTP server
    procedure Start;
    /// Stop the HTTP server
    procedure Stop;
  end;

implementation

{ TBasicAuthServer }

constructor TBasicAuthServer.Create(const aPort: RawUtf8);
begin
  // Create in-memory REST server (no database needed)
  fRestServer := TRestServerFullMemory.Create(
    TOrmModel.Create([]), 'root', {authentication=}false);

  // Create and register the API implementation
  fBasicAuthApi := TBasicAuthApi.Create;
  fRestServer.ServiceDefine(fBasicAuthApi, [IBasicAuthApi]);

  // Register authentication filter
  fRestServer.OnBeforeUri := OnBeforeBody;

  // Create HTTP server
  fHttpServer := TRestHttpServer.Create(aPort, [fRestServer],
    '+', HTTP_DEFAULT_MODE, {threaded=}32);
  fHttpServer.AccessControlAllowOrigin := '*'; // CORS

  TSynLog.Add.Log(sllInfo, 'Basic Auth Server created on port %', [aPort]);
end;

destructor TBasicAuthServer.Destroy;
begin
  Stop;
  fHttpServer.Free;
  fBasicAuthApi.Free;
  fRestServer.Free;
  inherited Destroy;
end;

function TBasicAuthServer.OnBeforeBody(Ctxt: TRestServerUriContext): Boolean;
var
  authHeader, userName, password: RawUtf8;
  roles: TRawUtf8DynArray;
  methodName: RawUtf8;
begin
  Result := True; // Allow by default

  // Extract method name from URI
  // Format: /root/BasicAuthApi.MethodName or /BasicAuthApi/MethodName
  methodName := Ctxt.UriMethodPath;
  if methodName = '' then
  begin
    // Fallback to full URI
    methodName := Ctxt.Call^.Url;
  end;

  // Check if this endpoint requires authentication
  if not TBasicAuthHandler.RequiresAuthentication(methodName) then
  begin
    TSynLog.Add.Log(sllDebug, 'Public endpoint, no auth required: %', [methodName]);
    Exit; // Public endpoint
  end;

  // Get Authorization header
  authHeader := Ctxt.InHeader['Authorization'];
  if authHeader = '' then
  begin
    TSynLog.Add.Log(sllDebug, 'No Authorization header for protected endpoint: %',
      [methodName]);
    Ctxt.Error('Unauthorized', HTTP_UNAUTHORIZED);
    Result := False;
    Exit;
  end;

  // Parse Basic Auth credentials
  if not TBasicAuthHandler.ParseBasicAuth(authHeader, userName, password) then
  begin
    TSynLog.Add.Log(sllDebug, 'Failed to parse Authorization header');
    Ctxt.Error('Unauthorized', HTTP_UNAUTHORIZED);
    Result := False;
    Exit;
  end;

  // Authenticate user
  if not TBasicAuthHandler.Authenticate(userName, password, roles) then
  begin
    TSynLog.Add.Log(sllDebug, 'Authentication failed for user: %', [userName]);
    Ctxt.Error('Unauthorized', HTTP_UNAUTHORIZED);
    Result := False;
    Exit;
  end;

  // Check authorization (role-based)
  if not TBasicAuthHandler.Authorize(roles, methodName) then
  begin
    TSynLog.Add.Log(sllDebug, 'Authorization failed for user % on method %',
      [userName, methodName]);
    Ctxt.Error('Forbidden: insufficient privileges', HTTP_FORBIDDEN);
    Result := False;
    Exit;
  end;

  // Authentication and authorization successful
  // Set context in API instance
  fBasicAuthApi.SetAuthContext(userName, roles);

  TSynLog.Add.Log(sllDebug, 'User % authenticated with roles [%] for method %',
    [userName, RawUtf8ArrayToCsv(roles), methodName]);
end;

procedure TBasicAuthServer.Start;
begin
  TSynLog.Add.Log(sllInfo, 'Starting Basic Auth Server...');
end;

procedure TBasicAuthServer.Stop;
begin
  TSynLog.Add.Log(sllInfo, 'Stopping Basic Auth Server...');
end;

end.
