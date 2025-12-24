unit api.impl;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.json,
  mormot.core.variants,
  mormot.core.log,
  mormot.core.interfaces,
  mormot.rest.server,
  mormot.rest.core,
  mormot.orm.base,
  mormot.soa.server,
  api.interfaces;

type
  /// Base class for services with role checking
  TRoleBasedService = class(TInjectableObjectRest)
  protected
    function GetCurrentUsername: RawUtf8;
    function GetUserRolesFromJwt: TRawUtf8DynArray;
    function HasRole(const aRole: RawUtf8): Boolean;
    function HasAnyRole(const aRoles: array of RawUtf8): Boolean;
    function HasAllRoles(const aRoles: array of RawUtf8): Boolean;
    function CheckRole(const aRole: RawUtf8): Boolean;
  end;

  /// Public service implementation
  TPublicService = class(TInjectableObjectRest, IPublicService)
  public
    function GetPublicMessage: RawUtf8;
  end;

  /// User service implementation - authenticated users only
  TUserService = class(TRoleBasedService, IUserService)
  public
    function GetProfile: RawUtf8;
    function GetMyData: RawUtf8;
  end;

  /// Manager service implementation - requires 'manager' role
  TManagerService = class(TRoleBasedService, IManagerService)
  public
    function GetTeamInfo: RawUtf8;
    function ApproveRequest(const aRequestId: RawUtf8): RawUtf8;
  end;

  /// Admin service implementation - requires 'admin' role
  TAdminService = class(TRoleBasedService, IAdminService)
  public
    function GetSystemInfo: RawUtf8;
    function ManageUsers: RawUtf8;
    function GetAllSessions: RawUtf8;
  end;

  /// Multi-role service implementation
  TMultiRoleService = class(TRoleBasedService, IMultiRoleService)
  public
    function GetFinancialReport: RawUtf8;
    function GetAuditLog: RawUtf8;
  end;

implementation

{ TRoleBasedService }

function TRoleBasedService.GetCurrentUsername: RawUtf8;
var
  ctx: PServiceRunningContext;
begin
  Result := '';
  ctx := ServiceRunningContext;
  if ctx <> nil then
    Result := ctx^.Request.AuthenticationBearerToken;
end;

function TRoleBasedService.GetUserRolesFromJwt: TRawUtf8DynArray;
begin
  // Note: In a real implementation, you would parse the JWT token here
  // and extract the roles from the custom claims.
  // For this demo, we're simplifying by just checking username patterns
  SetLength(Result, 0);
  // This is a simplified demo - in production, parse JWT claims
end;

function TRoleBasedService.HasRole(const aRole: RawUtf8): Boolean;
var
  username: RawUtf8;
begin
  // Simplified demo: determine roles based on username
  // In production, parse JWT and check roles claim
  Result := False;
  username := GetCurrentUsername;
  if username = '' then
    exit;

  // Role mappings from auth.pas Login logic
  if IdemPropNameU(aRole, 'role1') then
    Result := (username = 'user1') or (username = 'user3')
  else if IdemPropNameU(aRole, 'role2') then
    Result := (username = 'user2') or (username = 'user3')
  else if IdemPropNameU(aRole, 'manager') then
    Result := (username = 'manager_user') or (username = 'finance_user')
  else if IdemPropNameU(aRole, 'admin') then
    Result := (username = 'admin_user')
  else if IdemPropNameU(aRole, 'auditor') then
    Result := (username = 'audit_user')
  else if IdemPropNameU(aRole, 'reports') then
    Result := (username = 'finance_user');
end;

function TRoleBasedService.HasAnyRole(const aRoles: array of RawUtf8): Boolean;
var
  i: PtrInt;
begin
  Result := False;
  for i := 0 to High(aRoles) do
    if HasRole(aRoles[i]) then
    begin
      Result := True;
      exit;
    end;
end;

function TRoleBasedService.HasAllRoles(const aRoles: array of RawUtf8): Boolean;
var
  i: PtrInt;
begin
  Result := True;
  for i := 0 to High(aRoles) do
    if not HasRole(aRoles[i]) then
    begin
      Result := False;
      exit;
    end;
end;

function TRoleBasedService.CheckRole(const aRole: RawUtf8): Boolean;
begin
  Result := HasRole(aRole);
  if not Result then
  begin
    TSynLog.Add.Log(sllWarning, 'Access denied: role % required', [aRole]);
    CurrentServiceContext.Request.Error(FormatUtf8('Access denied: role % required', [aRole]), HTTP_FORBIDDEN);
  end;
end;

{ TPublicService }

function TPublicService.GetPublicMessage: RawUtf8;
begin
  TSynLog.Add.Log(sllInfo, 'GetPublicMessage called');
  Result := 'This is a public section - no authentication required';
end;

{ TUserService }

function TUserService.GetProfile: RawUtf8;
var
  username: RawUtf8;
  doc: TDocVariantData;
begin
  username := GetCurrentUsername;
  if username = '' then
  begin
    CurrentServiceContext.Request.Error('Not authenticated', HTTP_UNAUTHORIZED);
    exit;
  end;

  TSynLog.Add.Log(sllInfo, 'GetProfile called for user: %', [username]);

  doc.InitObject([
    'user', username,
    'displayName', username,
    'message', 'User profile information'
  ]);

  Result := doc.ToJson;
end;

function TUserService.GetMyData: RawUtf8;
var
  username: RawUtf8;
  doc: TDocVariantData;
begin
  username := GetCurrentUsername;
  if username = '' then
  begin
    CurrentServiceContext.Request.Error('Not authenticated', HTTP_UNAUTHORIZED);
    exit;
  end;

  doc.InitObject([
    'message', 'Your personal data',
    'username', username
  ]);

  Result := doc.ToJson;
end;

{ TManagerService }

function TManagerService.GetTeamInfo: RawUtf8;
var
  username: RawUtf8;
  doc: TDocVariantData;
  team: TDocVariantData;
begin
  if not CheckRole('manager') then
    exit;

  username := GetCurrentUsername;
  TSynLog.Add.Log(sllInfo, 'GetTeamInfo called by manager: %', [username]);

  team.InitArray(['Alice', 'Bob', 'Charlie'], JSON_FAST);

  doc.InitObject([
    'message', 'Team information (manager access)',
    'manager', username,
    'teamMembers', variant(team),
    'teamSize', 3
  ]);

  Result := doc.ToJson;
end;

function TManagerService.ApproveRequest(const aRequestId: RawUtf8): RawUtf8;
var
  username: RawUtf8;
  doc: TDocVariantData;
begin
  if not CheckRole('manager') then
    exit;

  username := GetCurrentUsername;
  TSynLog.Add.Log(sllInfo, 'ApproveRequest % by manager: %', [aRequestId, username]);

  doc.InitObject([
    'message', 'Request approved',
    'requestId', aRequestId,
    'approvedBy', username,
    'status', 'approved'
  ]);

  Result := doc.ToJson;
end;

{ TAdminService }

function TAdminService.GetSystemInfo: RawUtf8;
var
  username: RawUtf8;
  doc: TDocVariantData;
begin
  if not CheckRole('admin') then
    exit;

  username := GetCurrentUsername;
  TSynLog.Add.Log(sllInfo, 'GetSystemInfo called by admin: %', [username]);

  doc.InitObject([
    'message', 'System information (admin access)',
    'version', '1.0.0',
    'uptime', '24h 15m',
    'activeUsers', 42,
    'accessedBy', username
  ]);

  Result := doc.ToJson;
end;

function TAdminService.ManageUsers: RawUtf8;
var
  username: RawUtf8;
  doc: TDocVariantData;
  users: TDocVariantData;
begin
  if not CheckRole('admin') then
    exit;

  username := GetCurrentUsername;
  users.InitArray(['user1', 'user2', 'user3'], JSON_FAST);

  doc.InitObject([
    'message', 'User management (admin access)',
    'totalUsers', 3,
    'users', variant(users),
    'managedBy', username
  ]);

  Result := doc.ToJson;
end;

function TAdminService.GetAllSessions: RawUtf8;
var
  username: RawUtf8;
  doc: TDocVariantData;
begin
  if not CheckRole('admin') then
    exit;

  username := GetCurrentUsername;
  doc.InitObject([
    'message', 'Active sessions (admin access)',
    'activeSessions', 5,
    'queriedBy', username
  ]);

  Result := doc.ToJson;
end;

{ TMultiRoleService }

function TMultiRoleService.GetFinancialReport: RawUtf8;
var
  username: RawUtf8;
  doc: TDocVariantData;
begin
  // Requires BOTH manager AND reports roles
  if not HasAllRoles(['manager', 'reports']) then
  begin
    TSynLog.Add.Log(sllWarning, 'Access denied: requires both manager and reports roles');
    CurrentServiceContext.Request.Error('Access denied: requires both manager and reports roles', HTTP_FORBIDDEN);
    exit;
  end;

  username := GetCurrentUsername;
  TSynLog.Add.Log(sllInfo, 'GetFinancialReport accessed by: %', [username]);

  doc.InitObject([
    'message', 'Financial report (requires manager + reports roles)',
    'revenue', 1000000,
    'expenses', 750000,
    'profit', 250000,
    'accessedBy', username
  ]);

  Result := doc.ToJson;
end;

function TMultiRoleService.GetAuditLog: RawUtf8;
var
  username: RawUtf8;
  doc: TDocVariantData;
  entries: TDocVariantData;
begin
  // Requires EITHER admin OR auditor role
  if not HasAnyRole(['admin', 'auditor']) then
  begin
    TSynLog.Add.Log(sllWarning, 'Access denied: requires admin or auditor role');
    CurrentServiceContext.Request.Error('Access denied: requires admin or auditor role', HTTP_FORBIDDEN);
    exit;
  end;

  username := GetCurrentUsername;
  TSynLog.Add.Log(sllInfo, 'GetAuditLog accessed by: %', [username]);

  entries.InitArray([
    'User login: user1',
    'Data accessed: records.db',
    'Report generated: financial_2024.pdf'
  ], JSON_FAST);

  doc.InitObject([
    'message', 'Audit log (requires admin OR auditor role)',
    'totalEntries', 3,
    'entries', variant(entries),
    'accessedBy', username
  ]);

  Result := doc.ToJson;
end;

end.
