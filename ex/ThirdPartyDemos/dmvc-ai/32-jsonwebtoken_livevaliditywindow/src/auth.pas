unit auth;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  DateUtils,
  mormot.core.base,
  mormot.core.text,
  mormot.core.os,
  mormot.core.log,
  mormot.core.rtti,
  mormot.core.datetime,
  mormot.core.unicode,
  mormot.core.variants,
  mormot.core.json,
  mormot.crypt.jwt,
  mormot.rest.core,
  mormot.rest.server;

type
  /// Custom authentication using JWT with live validity window checking
  /// This demonstrates real-time token expiration validation
  TJwtLiveValidityAuth = class(TRestServerAuthentication)
  private
    fJwt: TJwtAbstract;
    fSecret: RawUtf8;
    fValidityWindowSeconds: Integer;
  protected
    procedure AuthenticationFailed(Ctxt: TRestServerUriContext;
      const aReason: RawUtf8);
  public
    /// Traditional Auth endpoint - not used for JWT (use Login instead)
    function Auth(Ctxt: TRestServerUriContext;
      const aUserName: RawUtf8): boolean; override;
    /// Initialize JWT authentication with live validity window
    /// @param aValidityWindowSeconds Token validity in seconds (default 60)
    constructor Create(aServer: TRestServer; const aSecret: RawUtf8;
      aValidityWindowSeconds: Integer = 60); reintroduce;
    destructor Destroy; override;
    /// Handle login endpoint and issue JWT token with short validity
    procedure Login(Ctxt: TRestServerUriContext);
    /// Verify JWT token with LIVE expiration checking
    function RetrieveSession(Ctxt: TRestServerUriContext): TAuthSession; override;
    /// JWT instance
    property Jwt: TJwtAbstract read fJwt;
    /// Token validity window in seconds
    property ValidityWindowSeconds: Integer read fValidityWindowSeconds;
  end;

implementation

{ TJwtLiveValidityAuth }

constructor TJwtLiveValidityAuth.Create(aServer: TRestServer;
  const aSecret: RawUtf8; aValidityWindowSeconds: Integer);
begin
  inherited Create(aServer);
  fSecret := aSecret;
  fValidityWindowSeconds := aValidityWindowSeconds;

  // Create JWT instance with HS256 and SHORT expiration window
  fJwt := TJwtHS256.Create(fSecret, 0,
    [jrcIssuer, jrcSubject, jrcExpirationTime, jrcIssuedAt, jrcNotBefore],
    [], // No optional claims
    fValidityWindowSeconds); // Short expiration for demonstration

  TSynLog.Add.Log(sllInfo, 'JWT live validity auth initialized: window=%s seconds',
    [fValidityWindowSeconds]);
end;

destructor TJwtLiveValidityAuth.Destroy;
begin
  fJwt.Free;
  inherited;
end;

procedure TJwtLiveValidityAuth.AuthenticationFailed(
  Ctxt: TRestServerUriContext; const aReason: RawUtf8);
begin
  TSynLog.Add.Log(sllWarning, 'Authentication failed: %', [aReason]);
  Ctxt.AuthenticationFailed(afInvalidSignature, '');
end;

function TJwtLiveValidityAuth.Auth(Ctxt: TRestServerUriContext;
  const aUserName: RawUtf8): boolean;
begin
  // JWT auth doesn't use traditional Auth endpoint - use Login() instead
  Result := false; // Let next authentication scheme try
end;

procedure TJwtLiveValidityAuth.Login(Ctxt: TRestServerUriContext);
var
  username, password: RawUtf8;
  doc: TDocVariantData;
  token: RawUtf8;
  roles: TDocVariantData;
  user: TAuthUser;
  group: TAuthGroup;
  now: TDateTime;
  expiresAt: TDateTime;
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
  if (username = 'user1') or (username = 'user3') then
    roles.AddItem('role1');
  if (username = 'user2') or (username = 'user3') then
    roles.AddItem('role2');

  // Create user and group in mORMot's auth system
  user := TAuthUser.Create;
  try
    user.LogonName := username;
    user.PasswordHashHexa := ''; // Not used for JWT
    user.DisplayName := username;

    // Add primary group (first role)
    // Note: mORMot2 uses single GroupRights, not a collection
    if roles.Count > 0 then
    begin
      group := TAuthGroup.Create;
      group.Ident := VariantToUtf8(roles.Value[0]);
      group.SessionTimeout := fValidityWindowSeconds;
      user.GroupRights := group;
    end;

    // Create session using inherited method from TRestServerAuthentication
    SessionCreate(Ctxt, user);

    // Calculate expiration time
    now := NowUtc;
    expiresAt := IncSecond(now, fValidityWindowSeconds);

    TSynLog.Add.Log(sllInfo, 'User % logged in, session=%, expires in %s at %',
      [username, Ctxt.Session, fValidityWindowSeconds, DateTimeToStr(expiresAt)]);

    // Generate JWT token with custom claims and LIVE expiration
    token := fJwt.Compute(
      [
        'customkey1', 'customvalue1',
        'customkey2', 'customvalue2',
        'roles', variant(roles),
        'valid_for_seconds', fValidityWindowSeconds
      ],
      'mORMot2 JWT Live Validity Demo',  // issuer
      username);                          // subject

    // Return token with expiration info
    Ctxt.Returns(
      JsonEncode([
        'token', token,
        'user', username,
        'roles', variant(roles),
        'expires_in_seconds', fValidityWindowSeconds,
        'issued_at', DateTimeToStr(now),
        'expires_at', DateTimeToStr(expiresAt),
        'message', FormatUtf8('Token is valid for % seconds. Use it quickly!',
          [fValidityWindowSeconds])
      ]),
      HTTP_SUCCESS,
      JSON_CONTENT_TYPE_HEADER
    );
  finally
    user.Free;
  end;
end;

function TJwtLiveValidityAuth.RetrieveSession(
  Ctxt: TRestServerUriContext): TAuthSession;
var
  authHeader: RawUtf8;
  token: RawUtf8;
  content: TJwtContent;
  user: TAuthUser;
  group: TAuthGroup;
  roles: PDocVariantData;
  p: PUtf8Char;
  verifyResult: TJwtResult;
  now: TDateTime;
  expiresAt: TDateTime;
  remainingSeconds: Integer;
begin
  Result := nil;

  // Extract Bearer token from Authorization header
  authHeader := Ctxt.InHeader['Authorization'];
  if authHeader = '' then
  begin
    TSynLog.Add.Log(sllDebug, 'No Authorization header found');
    exit;
  end;

  // Parse "Bearer <token>"
  p := pointer(authHeader);
  if not IdemPChar(p, 'BEARER ') then
  begin
    TSynLog.Add.Log(sllDebug, 'Authorization header is not Bearer token');
    exit;
  end;

  inc(p, 7); // Skip "Bearer "
  token := p;

  // LIVE VALIDITY CHECK: Verify JWT with real-time expiration validation
  now := NowUtc;
  fJwt.Verify(token, content);
  verifyResult := content.result;

  if verifyResult <> jwtValid then
  begin
    // Log detailed reason for failure
    case verifyResult of
      jwtNoToken:
        AuthenticationFailed(Ctxt, 'No JWT token provided');
      jwtWrongFormat:
        AuthenticationFailed(Ctxt, 'JWT token has wrong format');
      jwtInvalidSignature:
        AuthenticationFailed(Ctxt, 'JWT signature is invalid');
      jwtInvalidAlgorithm:
        AuthenticationFailed(Ctxt, 'JWT algorithm mismatch');
      jwtExpired:
        begin
          // Calculate how long ago it expired
          if content.reg[jrcExpirationTime] <> '' then
          begin
            expiresAt := Iso8601ToDateTime(content.reg[jrcExpirationTime]);
            remainingSeconds := SecondsBetween(now, expiresAt);
            AuthenticationFailed(Ctxt,
              FormatUtf8('JWT token EXPIRED % seconds ago (validity window was %s)',
                [remainingSeconds, fValidityWindowSeconds]));
          end
          else
            AuthenticationFailed(Ctxt, 'JWT token expired (no expiration claim found)');
        end;
      jwtNotBeforeFailed:
        AuthenticationFailed(Ctxt, 'JWT token used before valid time (nbf claim)');
    else
      AuthenticationFailed(Ctxt, FormatUtf8('JWT verification failed: %',
        [ToText(verifyResult)^]));
    end;
    exit;
  end;

  // Calculate remaining validity
  if content.reg[jrcExpirationTime] <> '' then
  begin
    expiresAt := Iso8601ToDateTime(content.reg[jrcExpirationTime]);
    remainingSeconds := SecondsBetween(now, expiresAt);
    if now < expiresAt then
      TSynLog.Add.Log(sllDebug, 'JWT VALID for subject: % (expires in %s)',
        [content.reg[jrcSubject], remainingSeconds])
    else
      TSynLog.Add.Log(sllWarning, 'JWT EXPIRED for subject: % (expired %s ago)',
        [content.reg[jrcSubject], remainingSeconds]);
  end
  else
    TSynLog.Add.Log(sllDebug, 'JWT verified for subject: %', [content.reg[jrcSubject]]);

  // Create user from JWT content
  user := TAuthUser.Create;
  try
    user.LogonName := content.reg[jrcSubject];
    user.DisplayName := user.LogonName;

    // Extract roles from custom claims
    // Note: GetAsArray returns PDocVariantData (pointer)
    if content.data.GetAsArray('roles', roles) then
    begin
      if roles^.Count > 0 then
      begin
        group := TAuthGroup.Create;
        group.Ident := VariantToUtf8(roles^.Value[0]);
        group.SessionTimeout := fValidityWindowSeconds;
        user.GroupRights := group;
      end;
    end;

    // Create session using inherited method from TRestServerAuthentication
    SessionCreate(Ctxt, user);
    Result := Ctxt.AuthSession;
    if Result <> nil then
      TSynLog.Add.Log(sllDebug, 'Session created: % for user: % (token still valid)',
        [Result.ID, user.LogonName]);
  finally
    user.Free;
  end;
end;

end.
