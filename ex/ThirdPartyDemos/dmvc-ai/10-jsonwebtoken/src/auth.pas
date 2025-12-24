unit auth;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.text,
  mormot.core.os,
  mormot.core.log,
  mormot.core.rtti,
  mormot.core.unicode,
  mormot.core.variants,
  mormot.core.json,
  mormot.crypt.jwt,
  mormot.orm.core,
  mormot.rest.core,
  mormot.rest.server;

type
  /// JWT claims extracted from the token - stored per-request
  TJwtClaims = record
    /// The authenticated username (from JWT subject)
    UserName: RawUtf8;
    /// User roles from JWT claims
    Roles: TDocVariantData;
    /// Custom claims from JWT
    CustomClaims: TDocVariantData;
    /// Whether claims are valid/populated
    Valid: boolean;
    /// Clear all claims
    procedure Clear;
  end;

  /// Custom authentication using JWT
  TJwtAuthenticationServer = class(TRestServerAuthentication)
  private
    fJwt: TJwtAbstract;
    fSecret: RawUtf8;
  protected
    procedure AuthFailed(Ctxt: TRestServerUriContext;
      const aReason: RawUtf8);
  public
    /// Initialize JWT authentication with HS256
    constructor Create(aServer: TRestServer; const aSecret: RawUtf8); reintroduce;
    destructor Destroy; override;
    /// Handle login endpoint and issue JWT token
    procedure Login(Ctxt: TRestServerUriContext);
    /// Verify JWT token from Authorization header
    function RetrieveSession(Ctxt: TRestServerUriContext): TAuthSession; override;
    /// Not used for JWT authentication (uses RetrieveSession instead)
    function Auth(Ctxt: TRestServerUriContext;
      const aUserName: RawUtf8): boolean; override;
    /// JWT instance
    property Jwt: TJwtAbstract read fJwt;
  end;

/// Get the current request's JWT claims (thread-safe)
// Call this from your service implementation to access JWT identity
function CurrentJwtClaims: TJwtClaims;

implementation

threadvar
  /// Thread-local storage for current request's JWT claims
  CurrentThreadJwtClaims: TJwtClaims;

function CurrentJwtClaims: TJwtClaims;
begin
  Result := CurrentThreadJwtClaims;
end;

{ TJwtClaims }

procedure TJwtClaims.Clear;
begin
  UserName := '';
  Roles.Clear;
  CustomClaims.Clear;
  Valid := false;
end;

{ TJwtAuthenticationServer }

constructor TJwtAuthenticationServer.Create(aServer: TRestServer;
  const aSecret: RawUtf8);
begin
  inherited Create(aServer);
  fSecret := aSecret;
  // Create JWT instance with HS256 algorithm
  // Parameters: secret, PBKDF2 rounds, required claims, optional claims, expiration seconds
  fJwt := TJwtHS256.Create(fSecret, 0,
    [jrcIssuer, jrcSubject, jrcExpirationTime, jrcIssuedAt],
    [], // No optional claims
    3600); // 1 hour expiration
  TSynLog.Add.Log(sllInfo, 'JWT authentication initialized with HS256');
end;

destructor TJwtAuthenticationServer.Destroy;
begin
  fJwt.Free;
  inherited;
end;

procedure TJwtAuthenticationServer.AuthFailed(
  Ctxt: TRestServerUriContext; const aReason: RawUtf8);
begin
  // Clear thread-local claims on auth failure
  CurrentThreadJwtClaims.Clear;
  TSynLog.Add.Log(sllWarning, 'Authentication failed: %', [aReason]);
  Ctxt.Error('Unauthorized: ' + aReason, HTTP_UNAUTHORIZED);
end;

procedure TJwtAuthenticationServer.Login(Ctxt: TRestServerUriContext);
var
  username, password: RawUtf8;
  doc: TDocVariantData;
  token: RawUtf8;
  roles: TDocVariantData;
  user: TAuthUser;
begin
  // Parse login request
  if not doc.InitJson(Ctxt.Call^.InBody, JSON_FAST) then
  begin
    Ctxt.Error('Invalid JSON body', HTTP_BADREQUEST);
    exit;
  end;

  username := doc.U['username'];
  password := doc.U['password'];

  TSynLog.Add.Log(sllDebug, 'Login attempt for user: %', [username]);

  // Simple demo authentication: username = password
  if (username = '') or (username <> password) then
  begin
    Ctxt.Error('Invalid credentials', HTTP_UNAUTHORIZED);
    exit;
  end;

  // Determine roles based on username (demo logic - matches DMVC example)
  roles.InitArray([], JSON_FAST);
  if (username = 'user1') or (username = 'user3') then
    roles.AddItem('role1');
  if (username = 'user2') or (username = 'user3') then
    roles.AddItem('role2');

  // Create user for session
  user := TAuthUser.Create;
  try
    user.LogonName := username;
    user.PasswordHashHexa := ''; // Not used for JWT
    user.DisplayName := username;
    // Store roles as CSV in Data field for later retrieval
    user.Data := roles.ToCsv;

    TSynLog.Add.Log(sllInfo, 'User % authenticated with roles %', [username, user.Data]);

    // Generate JWT token with custom claims (matches DMVC example)
    token := fJwt.Compute(
      [
        'customkey1', 'customvalue1',
        'customkey2', 'customvalue2',
        'roles', variant(roles)
      ],
      'mORMot2 JWT Demo',  // issuer
      username);           // subject

    // Return token
    Ctxt.Returns(
      JsonEncode([
        'token', token,
        'user', username,
        'roles', variant(roles)
      ]),
      HTTP_SUCCESS,
      JSON_CONTENT_TYPE_HEADER
    );
  finally
    user.Free;
  end;
end;

function TJwtAuthenticationServer.RetrieveSession(
  Ctxt: TRestServerUriContext): TAuthSession;
var
  authHeader: RawUtf8;
  token: RawUtf8;
  content: TJwtContent;
  p: PUtf8Char;
  rolesDoc: PDocVariantData;
begin
  Result := nil;

  // Clear previous claims at start of each request
  CurrentThreadJwtClaims.Clear;

  // Extract Bearer token from Authorization header
  authHeader := Ctxt.InHeader['Authorization'];
  if authHeader = '' then
    exit;

  // Parse "Bearer <token>"
  p := pointer(authHeader);
  if not IdemPChar(p, 'BEARER ') then
    exit;

  inc(p, 7); // Skip "Bearer "
  token := p;

  // Verify JWT
  fJwt.Verify(token, content);
  if content.result <> jwtValid then
  begin
    AuthFailed(Ctxt, 'Invalid or expired JWT token');
    exit;
  end;

  // Store JWT claims in thread-local storage for service access
  CurrentThreadJwtClaims.UserName := content.reg[jrcSubject];
  CurrentThreadJwtClaims.Valid := true;

  // Extract roles from JWT payload
  if content.data.GetAsArray('roles', rolesDoc) then
    CurrentThreadJwtClaims.Roles.InitCopy(variant(rolesDoc^), JSON_FAST)
  else
    CurrentThreadJwtClaims.Roles.InitArray([], JSON_FAST);

  // Extract custom claims
  CurrentThreadJwtClaims.CustomClaims.InitObject([
    'customkey1', content.data.U['customkey1'],
    'customkey2', content.data.U['customkey2']
  ], JSON_FAST);

  TSynLog.Add.Log(sllDebug, 'JWT verified for user: % with roles: %',
    [CurrentThreadJwtClaims.UserName, CurrentThreadJwtClaims.Roles.ToCsv]);

  // Note: For JWT, we don't create a traditional session
  // The JWT token itself is the session - validated on each request
  // The claims are stored in thread-local storage for service access
  Result := nil;
end;

function TJwtAuthenticationServer.Auth(Ctxt: TRestServerUriContext;
  const aUserName: RawUtf8): boolean;
begin
  // JWT authentication doesn't use the traditional Auth flow
  // Authentication happens via RetrieveSession which validates the JWT token
  // This method is not used but must be implemented (abstract override)
  Result := false;
end;

end.
