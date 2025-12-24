unit auth;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.text,
  mormot.core.os,
  mormot.core.log,
  mormot.core.unicode,
  mormot.core.variants,
  mormot.core.json,
  mormot.crypt.jwt,
  mormot.rest.core,
  mormot.rest.server;

type
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
    /// JWT instance
    property Jwt: TJwtAbstract read fJwt;
  end;

implementation

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

  // Determine roles based on username (demo logic)
  roles.InitArray([], JSON_FAST);

  // Legacy test users
  if (username = 'user1') or (username = 'user3') then
    roles.AddItem('role1');
  if (username = 'user2') or (username = 'user3') then
    roles.AddItem('role2');

  // Role-specific users
  if username = 'manager_user' then
    roles.AddItem('manager');
  if username = 'admin_user' then
    roles.AddItem('admin');
  if username = 'audit_user' then
    roles.AddItem('auditor');

  // Multi-role user
  if username = 'finance_user' then
  begin
    roles.AddItem('manager');
    roles.AddItem('reports');
  end;

  // Create user for session
  user := TAuthUser.Create;
  try
    user.LogonName := username;
    user.PasswordHashHexa := ''; // Not used for JWT
    user.DisplayName := username;
    // Store roles as CSV in Data field for later retrieval
    user.Data := roles.ToCsv;

    // Note: JWT auth doesn't use mORMot's session system in this demo
    // The token itself carries all auth info
    TSynLog.Add.Log(sllInfo, 'User % authenticated with roles %', [username, user.Data]);

    // Generate JWT token with custom claims
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
begin
  Result := nil;

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

  TSynLog.Add.Log(sllDebug, 'JWT verified for subject: %', [content.reg[jrcSubject]]);

  // Note: For JWT with role-based auth, we don't create a traditional session
  // The JWT token itself is the session - it's validated on each request
  // The roles are embedded in the JWT claims and can be extracted as needed
  // by the application logic from content.data

  TSynLog.Add.Log(sllDebug, 'JWT token validated for subject: % with roles',
    [content.reg[jrcSubject]]);

  // Return nil - the authentication is handled by the JWT token itself
  // The Ctxt will proceed with the request if we return successfully from Verify
  Result := nil;
end;

end.
