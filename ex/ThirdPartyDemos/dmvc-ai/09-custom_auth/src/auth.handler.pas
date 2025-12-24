unit auth.handler;

{$I mormot.defines.inc}

interface

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.log,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.rtti,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.memserver,
  mormot.soa.core,
  mormot.soa.server,
  api.interfaces;

type
  /// Custom authentication handler for role-based access control
  // This handler implements custom user validation and role checking
  // without using the built-in TAuthUser ORM authentication
  TCustomAuthenticationHandler = class(TRestServerAuthenticationDefault)
  protected
    /// Validate user credentials and populate session with roles
    // Returns True if authentication succeeds
    function CheckPassword(Ctxt: TRestServerUriContext; User: TAuthUser;
      const aClientNonce, aPassWord: RawUtf8): boolean; override;
  public
    /// Check if the current user has permission to execute a method
    // This is called for each service method call
    function RetrieveSession(Ctxt: TRestServerUriContext): TAuthSession; override;
  end;

  /// Custom authentication server with role-based authorization
  TCustomAuthServer = class(TRestServerFullMemory)
  protected
    /// Custom authorization check based on interface and method name
    // Called by TServiceFactoryServer.OnMethodExecute for each service call
    function OnMethodAuth(Ctxt: TRestServerUriContext;
      const Method: TInterfaceMethod): boolean;
  public
    constructor Create(aModel: TOrmModel; const aRoot: RawUtf8 = 'root'); reintroduce;
    /// Wire service authorization after services are registered
    procedure SetupServiceAuthorization;
  end;

implementation

{ TCustomAuthenticationHandler }

function TCustomAuthenticationHandler.CheckPassword(Ctxt: TRestServerUriContext;
  User: TAuthUser; const aClientNonce, aPassWord: RawUtf8): boolean;
var
  roles: TRawUtf8DynArray;
  username: RawUtf8;
begin
  // This is where we validate the user and set up their roles
  // In a real application, you would query a database or LDAP here

  Result := False;
  username := User.LogonName;

  // Static user definitions (for demonstration)
  // In production, this would query a users database/service

  if (username = 'admin') and (aPassWord = 'adminpass') then
  begin
    Result := True;
    AddRawUtf8(roles, 'admin');
    TSynLog.Add.Log(sllDebug, 'User % authenticated with admin role', [username]);
  end
  else if (username = 'user1') and (aPassWord = 'user1pass') then
  begin
    Result := True;
    AddRawUtf8(roles, 'role1');
    TSynLog.Add.Log(sllDebug, 'User % authenticated with role1', [username]);
  end
  else if (username = 'user2') and (aPassWord = 'user2pass') then
  begin
    Result := True;
    AddRawUtf8(roles, 'role2');
    TSynLog.Add.Log(sllDebug, 'User % authenticated with role2', [username]);
  end;

  if Result then
  begin
    // Store roles in the user's session data
    // Store roles as CSV in the Data field for later retrieval
    User.Data := RawUtf8ArrayToCsv(roles);

    TSynLog.Add.Log(sllDebug, 'User % session created with roles: %',
      [username, User.Data]);
  end
  else
    TSynLog.Add.Log(sllDebug, 'Authentication failed for user %', [username]);
end;

function TCustomAuthenticationHandler.RetrieveSession(
  Ctxt: TRestServerUriContext): TAuthSession;
begin
  // Use default session retrieval mechanism
  Result := inherited RetrieveSession(Ctxt);

  if Result <> nil then
    TSynLog.Add.Log(sllDebug, 'Session retrieved for user %, ID=%',
      [Result.User.LogonName, Result.ID]);
end;

{ TCustomAuthServer }

constructor TCustomAuthServer.Create(aModel: TOrmModel; const aRoot: RawUtf8);
begin
  inherited Create(aModel, aRoot, {authentication=}true);

  // Register our custom authentication handler
  AuthenticationRegister(TCustomAuthenticationHandler);

  // Set the custom authorization callback
  OnAuthenticationFailed := nil; // Use default behavior

  TSynLog.Add.Log(sllDebug, 'Custom authentication server created');
end;

procedure TCustomAuthServer.SetupServiceAuthorization;
var
  publicFactory, privateFactory: TServiceFactoryServer;
begin
  // Get the service factories and wire up the OnMethodExecute callback
  publicFactory := Services.Info(TypeInfo(IPublicApi)) as TServiceFactoryServer;
  privateFactory := Services.Info(TypeInfo(IPrivateApi)) as TServiceFactoryServer;

  if publicFactory <> nil then
  begin
    publicFactory.OnMethodExecute := OnMethodAuth;
    TSynLog.Add.Log(sllDebug, 'IPublicApi authorization callback wired');
  end;

  if privateFactory <> nil then
  begin
    privateFactory.OnMethodExecute := OnMethodAuth;
    // Bypass authentication for the PublicAction method
    ServiceMethodByPassAuthentication('IPrivateApi.PublicAction');
    TSynLog.Add.Log(sllDebug, 'IPrivateApi authorization callback wired');
  end;

  // Bypass authentication for all IPublicApi methods
  ServiceMethodByPassAuthentication('IPublicApi');

  TSynLog.Add.Log(sllDebug, 'Service authorization setup complete');
end;

function TCustomAuthServer.OnMethodAuth(Ctxt: TRestServerUriContext;
  const Method: TInterfaceMethod): boolean;
var
  interfaceName, methodName: RawUtf8;
  roles: TRawUtf8DynArray;
  user: TAuthUser;
  hasRole: boolean;
  i: PtrInt;
begin
  // Get interface and method names from the Method parameter
  interfaceName := Method.InterfaceDotMethodName;
  Split(interfaceName, '.', interfaceName, methodName);

  TSynLog.Add.Log(sllDebug, 'Authorization check: Interface=%, Method=%, User=%',
    [interfaceName, methodName, Ctxt.SessionUserName]);

  // Public API - no authentication required (also handled by ByPassAuthentication)
  if interfaceName = 'IPublicApi' then
  begin
    Result := True;
    TSynLog.Add.Log(sllDebug, 'Public API - access granted');
    Exit;
  end;

  // Private API - authentication required
  if interfaceName = 'IPrivateApi' then
  begin
    // PublicAction exception - no auth required (also handled by ByPassAuthentication)
    if methodName = 'PublicAction' then
    begin
      Result := True;
      TSynLog.Add.Log(sllDebug, 'PublicAction - access granted without auth');
      Exit;
    end;

    // For all other methods, authentication is required
    if Ctxt.SessionUserName = '' then
    begin
      Result := False;
      Ctxt.Error('Authentication required', HTTP_UNAUTHORIZED);
      TSynLog.Add.Log(sllWarning, 'Authentication required for %', [methodName]);
      Exit;
    end;

    // Get user's session to check roles
    user := SessionGetUser(Ctxt.Session);
    if user = nil then
    begin
      Result := False;
      Ctxt.Error('No valid session', HTTP_UNAUTHORIZED);
      TSynLog.Add.Log(sllWarning, 'No valid session for user %', [Ctxt.SessionUserName]);
      Exit;
    end;

    // Parse roles from session data (comma-separated)
    CsvToRawUtf8DynArray(pointer(user.Data), roles);

    TSynLog.Add.Log(sllDebug, 'User % has roles: %',
      [Ctxt.SessionUserName, user.Data]);

    // Check authorization based on method name
    hasRole := False;

    // Admin can access everything
    for i := 0 to High(roles) do
      if roles[i] = 'admin' then
      begin
        hasRole := True;
        break;
      end;

    // Method-specific role checks
    if not hasRole then
    begin
      if methodName = 'OnlyRole1' then
      begin
        for i := 0 to High(roles) do
          if roles[i] = 'role1' then
          begin
            hasRole := True;
            break;
          end;
      end
      else if methodName = 'OnlyRole2' then
      begin
        for i := 0 to High(roles) do
          if roles[i] = 'role2' then
          begin
            hasRole := True;
            break;
          end;
      end
      else if methodName = 'Index' then
      begin
        // Index requires admin role
        hasRole := False;
      end;
    end;

    Result := hasRole;

    if Result then
      TSynLog.Add.Log(sllDebug, 'Authorization granted for %', [methodName])
    else
    begin
      Ctxt.Error('Insufficient privileges', HTTP_FORBIDDEN);
      TSynLog.Add.Log(sllWarning, 'Authorization denied for % - insufficient privileges',
        [methodName]);
    end;

    Exit;
  end;

  // Unknown interface - deny by default
  Result := False;
  Ctxt.Error('Unknown interface', HTTP_FORBIDDEN);
  TSynLog.Add.Log(sllWarning, 'Unknown interface % - access denied', [interfaceName]);
end;

end.
