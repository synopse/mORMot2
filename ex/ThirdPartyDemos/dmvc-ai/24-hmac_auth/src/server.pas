unit server;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.buffers,
  mormot.core.log,
  mormot.core.datetime,
  mormot.core.json,
  mormot.crypt.core,
  mormot.crypt.secure,
  mormot.orm.core,
  mormot.soa.core,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.http.server,
  mormot.rest.memserver,
  api.interfaces,
  api.impl;

const
  /// Shared secret key for HMAC verification (in production, use secure key management)
  HMAC_SECRET_KEY = 'my-super-secret-key-for-hmac-auth';

  /// Maximum age of request timestamp (5 minutes) to prevent replay attacks
  MAX_TIMESTAMP_AGE_SECONDS = 300;

type
  /// HMAC-authenticated REST server
  /// Demonstrates server-side HMAC signature verification
  THmacAuthServer = class
  private
    fModel: TOrmModel;
    fRestServer: TRestServer;
    fHttpServer: TRestHttpServer;

    /// Verify HMAC signature on incoming requests
    /// Expected headers:
    ///   X-Timestamp: ISO8601 timestamp of request
    ///   X-Signature: HMAC-SHA256(timestamp + method + uri + body, secret)
    /// Returns True to continue processing, False to abort (must set error)
    function OnBeforeUri(Ctxt: TRestServerUriContext): boolean;

    /// Compute HMAC-SHA256 signature
    class function ComputeSignature(const Timestamp, Method, Uri, Body: RawUtf8): RawUtf8;
  public
    constructor Create(const aPort: RawUtf8);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  end;

implementation

{ THmacAuthServer }

class function THmacAuthServer.ComputeSignature(
  const Timestamp, Method, Uri, Body: RawUtf8): RawUtf8;
var
  signatureBase: RawUtf8;
  digest: TSha256Digest;
begin
  // Build the signature base string: timestamp + method + uri + body
  signatureBase := Timestamp + Method + Uri + Body;

  // Compute HMAC-SHA256
  HmacSha256(HMAC_SECRET_KEY, signatureBase, digest);

  // Return as lowercase hex
  Result := LowerCase(Sha256DigestToString(digest));
end;

function THmacAuthServer.OnBeforeUri(Ctxt: TRestServerUriContext): boolean;
var
  timestamp: RawUtf8;
  signature: RawUtf8;
  expectedSignature: RawUtf8;
  requestTime: TDateTime;
  ageSeconds: integer;
  method, uri, body: RawUtf8;
begin
  Result := True; // Default: allow request to continue

  // Get URI from the call structure
  uri := Ctxt.Call^.Url;

  // Skip authentication for root and status endpoints
  if (uri = '') or (uri = '/') or
     (Pos('status', LowerCase(uri)) > 0) then
    exit;

  // Get HMAC headers
  timestamp := Ctxt.InHeader['X-Timestamp'];
  signature := Ctxt.InHeader['X-Signature'];

  // Check required headers
  if timestamp = '' then
  begin
    Ctxt.Error('Missing X-Timestamp header', HTTP_UNAUTHORIZED);
    TSynLog.Add.Log(sllWarning, 'HMAC auth failed: missing timestamp');
    Result := False;
    exit;
  end;

  if signature = '' then
  begin
    Ctxt.Error('Missing X-Signature header', HTTP_UNAUTHORIZED);
    TSynLog.Add.Log(sllWarning, 'HMAC auth failed: missing signature');
    Result := False;
    exit;
  end;

  // Validate timestamp format and age (prevents replay attacks)
  requestTime := Iso8601ToDateTime(timestamp);
  if requestTime = 0 then
  begin
    Ctxt.Error('Invalid timestamp format (expected ISO8601)', HTTP_BADREQUEST);
    TSynLog.Add.Log(sllWarning, 'HMAC auth failed: invalid timestamp format');
    Result := False;
    exit;
  end;

  ageSeconds := Round((Now - requestTime) * SecsPerDay);
  if Abs(ageSeconds) > MAX_TIMESTAMP_AGE_SECONDS then
  begin
    Ctxt.Error('Request timestamp too old or in the future', HTTP_UNAUTHORIZED);
    TSynLog.Add.Log(sllWarning, 'HMAC auth failed: timestamp age % seconds', [ageSeconds]);
    Result := False;
    exit;
  end;

  // Build signature components
  method := Ctxt.Call^.Method;
  // uri was already set above
  if Ctxt.Call^.InBody <> '' then
    body := Ctxt.Call^.InBody
  else
    body := '';

  // Compute expected signature
  expectedSignature := ComputeSignature(timestamp, method, uri, body);

  // Compare signatures (constant-time comparison to prevent timing attacks)
  if not SameText(signature, expectedSignature) then
  begin
    Ctxt.Error('Invalid HMAC signature', HTTP_UNAUTHORIZED);
    TSynLog.Add.Log(sllWarning, 'HMAC auth failed: signature mismatch');
    TSynLog.Add.Log(sllDebug, 'Expected: % Got: %', [expectedSignature, signature]);
    Result := False;
    exit;
  end;

  TSynLog.Add.Log(sllInfo, 'HMAC auth SUCCESS for % %', [method, uri]);
  // Result is already True
end;

constructor THmacAuthServer.Create(const aPort: RawUtf8);
begin
  inherited Create;

  // Create model
  fModel := TOrmModel.Create([]);

  // Create REST server
  fRestServer := TRestServerFullMemory.Create(fModel, False);
  fRestServer.CreateMissingTables;

  // Wire HMAC authentication middleware
  fRestServer.OnBeforeUri := OnBeforeUri;

  // Register secure API
  fRestServer.ServiceDefine(TSecureApi, [ISecureApi], sicShared);

  // Create HTTP server
  fHttpServer := TRestHttpServer.Create(
    Utf8ToString(aPort),
    [fRestServer],
    '+',
    useHttpSocket
  );

  fHttpServer.AccessControlAllowOrigin := '*';

  TSynLog.Add.Log(sllInfo, 'HMAC Auth Server created on port %', [aPort]);
end;

destructor THmacAuthServer.Destroy;
begin
  Stop;
  fHttpServer.Free;
  fRestServer.Free;
  fModel.Free;
  inherited;
end;

procedure THmacAuthServer.Start;
begin
  TSynLog.Add.Log(sllInfo, 'HMAC Auth Server started');
end;

procedure THmacAuthServer.Stop;
begin
  TSynLog.Add.Log(sllInfo, 'HMAC Auth Server stopped');
end;

end.
