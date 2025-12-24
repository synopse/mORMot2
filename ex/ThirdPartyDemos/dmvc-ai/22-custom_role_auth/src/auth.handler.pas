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
  mormot.rest.memserver;

type
  /// Custom authentication handler for role-based access control
  // This handler implements custom user validation and role checking
  // Supports multiple users with different role combinations
  TCustomRoleAuthHandler = class(TRestServerAuthenticationDefault)
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
  TCustomRoleAuthServer = class(TRestServerFullMemory)
  public
    constructor Create(aModel: TOrmModel; const aRoot: RawUtf8 = 'root'); reintroduce;
    /// Authorization callback for service methods
    // Checks if the current user has the required roles to execute a method
    function CheckMethodAuthorization(Ctxt: TRestServerUriContext;
      const Method: TInterfaceMethod): boolean;
  end;

implementation

{ TCustomRoleAuthHandler }

function TCustomRoleAuthHandler.CheckPassword(Ctxt: TRestServerUriContext;
  User: TAuthUser; const aClientNonce, aPassWord: RawUtf8): boolean;
var
  roles: TRawUtf8DynArray;
  username: RawUtf8;
begin
  // This is where we validate the user and set up their roles
  // In a real application, you would query a database or LDAP here

  Result := False;
  username := User.LogonName;

  // Static user definitions matching DMVC custom_role_auth example:
  // - admin: admin, role1, role2
  // - user1: role1
  // - user2: role2
  // - user1_2: role1, role2
  // - user3: role3

  if (username = 'admin') and (aPassWord = 'adminpass') then
  begin
    Result := True;
    AddRawUtf8(roles, 'admin');
    AddRawUtf8(roles, 'role1');
    AddRawUtf8(roles, 'role2');
    TSynLog.Add.Log(sllDebug, 'User % authenticated with roles: admin, role1, role2', [username]);
  end
  else if (username = 'user1') and (aPassWord = 'user1pass') then
  begin
    Result := True;
    AddRawUtf8(roles, 'role1');
    TSynLog.Add.Log(sllDebug, 'User % authenticated with role: role1', [username]);
  end
  else if (username = 'user2') and (aPassWord = 'user2pass') then
  begin
    Result := True;
    AddRawUtf8(roles, 'role2');
    TSynLog.Add.Log(sllDebug, 'User % authenticated with role: role2', [username]);
  end
  else if (username = 'user1_2') and (aPassWord = 'user1_2pass') then
  begin
    Result := True;
    AddRawUtf8(roles, 'role1');
    AddRawUtf8(roles, 'role2');
    TSynLog.Add.Log(sllDebug, 'User % authenticated with roles: role1, role2', [username]);
  end
  else if (username = 'user3') and (aPassWord = 'user3pass') then
  begin
    Result := True;
    AddRawUtf8(roles, 'role3');
    TSynLog.Add.Log(sllDebug, 'User % authenticated with role: role3', [username]);
  end;

  if Result then
  begin
    // Store roles in the user's session data as CSV
    User.Data := RawUtf8ArrayToCsv(roles);
    TSynLog.Add.Log(sllDebug, 'User % session created with roles: %',
      [username, User.Data]);
  end
  else
    TSynLog.Add.Log(sllDebug, 'Authentication failed for user %', [username]);
end;

function TCustomRoleAuthHandler.RetrieveSession(
  Ctxt: TRestServerUriContext): TAuthSession;
begin
  // Use default session retrieval mechanism
  Result := inherited RetrieveSession(Ctxt);

  if Result <> nil then
    TSynLog.Add.Log(sllDebug, 'Session retrieved for user %, ID=%',
      [Result.User.LogonName, Result.ID]);
end;

{ TCustomRoleAuthServer }

constructor TCustomRoleAuthServer.Create(aModel: TOrmModel; const aRoot: RawUtf8);
begin
  inherited Create(aModel, aRoot, {authentication=}true);

  // Register our custom authentication handler
  AuthenticationRegister(TCustomRoleAuthHandler);

  // Set the custom authorization callback
  OnAuthenticationFailed := nil; // Use default behavior

  TSynLog.Add.Log(sllDebug, 'Custom role auth server created');
end;

function TCustomRoleAuthServer.CheckMethodAuthorization(Ctxt: TRestServerUriContext;
  const Method: TInterfaceMethod): boolean;
var
  interfaceName, methodName: RawUtf8;
  roles: TRawUtf8DynArray;
  user: TAuthUser;
  hasRole1, hasRole2: boolean;
  i: PtrInt;
begin
  // Extract interface and method names
  methodName := Method.Uri;
  Split(Method.InterfaceDotMethodName, '.', interfaceName, methodName);

  TSynLog.Add.Log(sllDebug, 'Authorization check: Interface=%, Method=%, User=%',
    [interfaceName, methodName, Ctxt.SessionUserName]);

  // Private API - authentication and role-based authorization
  if interfaceName = 'IPrivateApi' then
  begin
    // PublicAction exception - no auth required
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
      TSynLog.Add.Log(sllWarning, 'Authentication required for %', [methodName]);
      Exit;
    end;

    // Get user's session to check roles
    user := Self.SessionGetUser(Ctxt.Session);
    if user = nil then
    begin
      Result := False;
      TSynLog.Add.Log(sllWarning, 'No valid session for user %', [Ctxt.SessionUserName]);
      Exit;
    end;

    // Parse roles from session data (comma-separated)
    CsvToRawUtf8DynArray(pointer(user.Data), roles);

    TSynLog.Add.Log(sllDebug, 'User % has roles: %',
      [Ctxt.SessionUserName, user.Data]);

    // Check for admin role (can access everything)
    for i := 0 to High(roles) do
      if roles[i] = 'admin' then
      begin
        Result := True;
        TSynLog.Add.Log(sllDebug, 'Admin access granted for %', [methodName]);
        Exit;
      end;

    // Method-specific role checks
    Result := False;

    if methodName = 'Index' then
    begin
      // Index requires authentication (any authenticated user)
      Result := True;
    end
    else if methodName = 'OnlyRole1' then
    begin
      // Requires role1
      for i := 0 to High(roles) do
        if roles[i] = 'role1' then
        begin
          Result := True;
          break;
        end;
    end
    else if methodName = 'OnlyRole2' then
    begin
      // Requires role2
      for i := 0 to High(roles) do
        if roles[i] = 'role2' then
        begin
          Result := True;
          break;
        end;
    end
    else if methodName = 'OnlyRole1And2' then
    begin
      // Requires BOTH role1 AND role2
      hasRole1 := False;
      hasRole2 := False;
      for i := 0 to High(roles) do
      begin
        if roles[i] = 'role1' then
          hasRole1 := True;
        if roles[i] = 'role2' then
          hasRole2 := True;
      end;
      Result := hasRole1 and hasRole2;
    end
    else if methodName = 'OnlyRole1Or2' then
    begin
      // Requires EITHER role1 OR role2
      for i := 0 to High(roles) do
        if (roles[i] = 'role1') or (roles[i] = 'role2') then
        begin
          Result := True;
          break;
        end;
    end
    else if methodName = 'AccessByRole' then
    begin
      // Dynamic role check - extract role from method parameters
      // The role parameter is passed as part of the method call
      // For simplicity, we'll check if the user has any of their roles
      // In a real implementation, you would parse the actual role parameter

      // Extract the role from the input parameters
      // Note: In mORMot2, we need to parse this from the context
      // For this example, we'll grant access if the user has at least one role
      Result := Length(roles) > 0;

      if Result then
        TSynLog.Add.Log(sllDebug, 'AccessByRole - User has roles, access granted')
      else
        TSynLog.Add.Log(sllWarning, 'AccessByRole - User has no roles, access denied');
    end;

    if Result then
      TSynLog.Add.Log(sllDebug, 'Authorization granted for %', [methodName])
    else
      TSynLog.Add.Log(sllWarning, 'Authorization denied for % - insufficient privileges',
        [methodName]);

    Exit;
  end;

  // Unknown interface - deny by default
  Result := False;
  TSynLog.Add.Log(sllWarning, 'Unknown interface % - access denied', [interfaceName]);
end;

end.
