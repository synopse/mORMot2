unit authentication;

{$I mormot.defines.inc}

interface

uses
  sysutils,
  mormot.core.base,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.buffers,
  mormot.rest.core,
  mormot.rest.server;

type
  /// Basic Authentication Handler
  // Port of DMVC TAuthenticationSample
  TBasicAuthHandler = class
  public
    /// Check if endpoint requires authentication
    // Port of OnRequest
    class function RequiresAuthentication(const aMethod: RawUtf8): Boolean;

    /// Validate credentials and return user roles
    // Port of OnAuthentication
    class function Authenticate(const aUserName, aPassword: RawUtf8;
      out aRoles: TRawUtf8DynArray): Boolean;

    /// Check if user has required role for endpoint
    // Port of OnAuthorization
    class function Authorize(const aRoles: TRawUtf8DynArray;
      const aMethod: RawUtf8): Boolean;

    /// Parse Basic Auth header and extract credentials
    class function ParseBasicAuth(const aAuthHeader: RawUtf8;
      out aUserName, aPassword: RawUtf8): Boolean;
  end;

implementation

{ TBasicAuthHandler }

class function TBasicAuthHandler.RequiresAuthentication(
  const aMethod: RawUtf8): Boolean;
begin
  // Public endpoints don't require auth
  Result := not (
    (aMethod = 'PublicSection') or
    (aMethod = 'Index')
  );
end;

class function TBasicAuthHandler.Authenticate(const aUserName,
  aPassword: RawUtf8; out aRoles: TRawUtf8DynArray): Boolean;
begin
  // Simple authentication: username equals password (demo only!)
  Result := aUserName = aPassword;

  if Result then
  begin
    // Assign roles based on username
    if aUserName = 'user1' then
      aRoles := ['role1']
    else if aUserName = 'user2' then
      aRoles := ['role2']
    else if aUserName = 'user3' then
      aRoles := ['role1', 'role2']; // user3 has all roles
  end
  else
    SetLength(aRoles, 0);
end;

class function TBasicAuthHandler.Authorize(const aRoles: TRawUtf8DynArray;
  const aMethod: RawUtf8): Boolean;
begin
  Result := False;

  // Check role-based authorization
  if aMethod = 'OnlyRole1' then
    Result := FindRawUtf8(aRoles, 'role1') >= 0
  else if aMethod = 'OnlyRole1Json' then
    Result := FindRawUtf8(aRoles, 'role1') >= 0
  else if aMethod = 'OnlyRole2' then
    Result := FindRawUtf8(aRoles, 'role2') >= 0;
end;

class function TBasicAuthHandler.ParseBasicAuth(const aAuthHeader: RawUtf8;
  out aUserName, aPassword: RawUtf8): Boolean;
var
  encoded, decoded: RawUtf8;
  colonPos: Integer;
begin
  Result := False;

  // Check for "Basic " prefix
  if not IdemPChar(Pointer(aAuthHeader), 'BASIC ') then
    Exit;

  // Extract base64 encoded part
  encoded := Copy(aAuthHeader, 7, MaxInt); // Skip "Basic "

  // Decode base64
  decoded := Base64ToBin(encoded);
  if decoded = '' then
    Exit;

  // Split username:password
  colonPos := PosEx(':', decoded);
  if colonPos = 0 then
    Exit;

  aUserName := Copy(decoded, 1, colonPos - 1);
  aPassword := Copy(decoded, colonPos + 1, MaxInt);
  Result := True;
end;

end.
