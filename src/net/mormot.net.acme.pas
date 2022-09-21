/// Network ACME / Let's Encrypt Support
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.acme;

{
  *****************************************************************************

   Automatic Certificate Management Environment (ACME v2) Client
    - Low-Level Cryptographic Wrappers
    - JWS HTTP-client implementation
    - ACME client implementation

  *****************************************************************************

}

interface

{$I ..\mormot.defines.inc}

{$ifdef USE_OPENSSL}

// compile as a void unit if USE_OPENSSL is not defined

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.data,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.buffers,
  mormot.core.variants,
  mormot.core.rtti,
  mormot.core.json,
  mormot.crypt.core,
  mormot.crypt.secure,
  mormot.lib.openssl11,
  mormot.net.client;


{ **************** Low-Level Cryptographic Wrappers }

/// convert a DER signature into its raw base-64-uri encoded value
// - as expected by JSON Web Signature (JWS)
// - RSA is just directly encoded
// - ECC are ASN1-decoded into their raw xy coordinates concatenation
function DerToJwsSign(algo: TCryptAsymAlgo; const sign_der: RawByteString): RawUtf8;



{ **************** JWS HTTP-client implementation }

type
  /// exception associated with TJwsHttpClient
  EJwsHttp = class(ESynException);

  /// JSON Web Signature (JWS) HTTP-client
  // - send content secured with digital signature
  TJwsHttpClient = class(TSimpleHttpClient)
  protected
    fCert: ICryptCert;
    fNonce: RawUtf8;
    fKid: RawUtf8;
    fJwkThumbprint: RawUtf8;
    function GetNonceAndBody: RawJson;
  public
    /// initialize this JWS-oriented HTTP client
    constructor Create(const aCert: ICryptCert);
    /// perform a GET request, not signed/authenticated
    function Get(const aUrl: RawUtf8): RawJson;
    /// perform a POST request, with a signed JWS body as plain JSON
    function Post(const aUrl: RawUtf8; const aJson: RawJson): RawJson; overload;
    /// perform a POST request, with a signed JWS body as key/value pairs
    function Post(const aUrl: RawUtf8; const aNameValues: array of const): RawJson; overload;
    /// JWK Thumbprint as SHA-256
    property JwkThumbprint: RawUtf8
      read fJwkThumbprint;
  end;

const
  /// the JWS ECC curve names according to our known asymmetric algorithms
  // - see https://www.iana.org/assignments/jose/jose.xhtml#web-key-elliptic-curve
  CAA_CRV: array[TCryptAsymAlgo] of RawUtf8 = (
    'P-256',     // caaES256
    'P-384',     // caaES384
    'P-521',     // caaES512, note that P-521 is not a typo ;)
    'secp256k1', // caaES256K
    '',          // caaRS256
    '',          // caaRS384
    '',          // caaRS512
    '',          // caaPS256
    '',          // caaPS384
    '',          // caaPS512
    'Ed25519');  // caaEdDSA


{ **************** ACME client implementation }

type
  /// exception associated with TAcmeClient
  EAcmeClient = class(ESynException);

  /// the status of a given ACME challenge
  TAcmeStatus = (
    asInvalid,
    asPending,
    asValid
  );

  /// the details of one ACME challenge
  TAcmeChallenge = record
    SubjectType: RawUtf8;
    Subject: RawUtf8;
    Status: TAcmeStatus;
    Url: RawUtf8;
    Token: RawUtf8;
    Key: RawUtf8;
  end;

  /// the details of all ACME challenges
  TAcmeChallengeDynArray = array of TAcmeChallenge;

  /// callback to actually implement one HTTP-01 ACME challenge
  // - is called once before the challenge process with non void Key
  // - is called once after the challenge process with Key = '', so that
  // you can remove the challenge content (e.g. files) from your web server
  // - should publish the Key content into the expected HTTP server URI, i.e.
  // http://<Domain>/.well-known/acme-challenge/<Token>
  TOnAcmeChallenge = procedure(Sender: TObject;
    const Domain, Key, Token: RawUtf8) of object;

  /// ACME client processing class
  // - implements the ACME V2 client (specified in RFC8555) to download
  // free domain validated certificates, mainly from Let's Encrypt
  // - see https://letsencrypt.org/how-it-works for a high-level description
  TAcmeClient = class
  protected
    fDirectoryUrl: RawUtf8;
    fContact: RawUtf8;
    fDomain: RawUtf8;
    fSubjects: RawUtf8;
    fHttpClient: TJwsHttpClient;
    fChallenges: TAcmeChallengeDynArray;
    fOnChallenges: TOnAcmeChallenge;
    fChallengeWwwFolder: TFileName;
    // URI filled by ReadDirectory
    fNewNonce: RawUtf8;
    fNewAccount: RawUtf8;
    fNewOrder: RawUtf8;
    // URI filled by CreateOrder
    fFinalize: RawUtf8;
    procedure ReadDirectory;
    procedure CreateAccount;
    function CreateOrder: TAcmeStatus;
    procedure RequestAuth(const aJson: RawJson);
    procedure OnChallengeWwwFolder(Sender: TObject;
      const Domain, Key, Token: RawUtf8);
  public
    /// create an ACME client instance
    // - aDirectoryUrl is the main URL of a directory object, typically
    // ACME_LETSENCRYPT_URL (for production) or ACME_LETSENCRYPT_DEBUG_URL
    // (for testing)
    // - aCert is the certificate used to identify the client account
    constructor Create(const aCert: ICryptCert;
      const aDirectoryUrl, aContact, aDomain, aSubjects: RawUtf8);
    /// finalize the instance
    destructor Destroy; override;
    /// register account and applying for Certificate Issuance
    // - caller should have set the OnChallenges, Contact and Subjects
    // properties with the expected event or values
    procedure StartDomainRegistration;
    /// check if challenge for a domain is completed
    function CheckChallengesStatus: TAcmeStatus;
    /// finalize the order and download the signed certificate
    // - aCert contain the final PEM certificate as signed by the ACME server
    // - aPrivateKey contains the PEM private key generated locally, and
    // optionally encrypted via aPrivateKeyPassword
    function CompleteDomainRegistration(out aCert, aPrivateKey: RawUtf8;
      const aPrivateKeyPassword: SpiUtf8): TAcmeStatus;
    /// will run StartDomainRegistration and wait until it is completed
    // - low-level wrapper using any TOnAcmeChallenges callback
    function RegisterAndWait(const OnChallenge: TOnAcmeChallenge;
      const OutSignedCert, OutPrivateKey: TFileName;
      const aPrivateKeyPassword: SpiUtf8; WaitForSec: integer): TAcmeStatus; overload;
    /// will run StartDomainRegistration and wait until it is completed
    // - ChallengeWwwFolder is a local folder where to store the temporary
    // challenges, to be served by an external web server, e.g. nginx - the
    // method will append ACME_CHALLENGE_PATH to the supplied local folder name
    function RegisterAndWait(
      const ChallengeWwwFolder, OutSignedCert, OutPrivateKey: TFileName;
      const aPrivateKeyPassword: SpiUtf8; WaitForSec: integer): TAcmeStatus; overload;
    /// associated contact as mailto: link, e.g. 'mailto:admin@synopse.info'
    property Contact: RawUtf8
      read fContact write fContact;
    /// associated subjects as CSV text, e.g. 'synopse.info,www.synopse.info'
    property Subjects: RawUtf8
      read fSubjects write fSubjects;
    /// low-level direct access to the associated challenges
    // - may be used instead of OnChallenges callback
    property Challenges: TAcmeChallengeDynArray
      read fChallenges;
    /// callback to let the top-level application publish the challenges
    property OnChallenges: TOnAcmeChallenge
      read fOnChallenges write fOnChallenges;
  end;



function ToText(status: TAcmeStatus): PShortString; overload;

const
  ACME_LETSENCRYPT_URL =
    'https://acme-v02.api.letsencrypt.org/directory';
  ACME_LETSENCRYPT_DEBUG_URL =
    'https://acme-staging-v02.api.letsencrypt.org/directory';

  ACME_CHALLENGE_PATH =
    '/.well-known/acme-challenge/';
  ACME_CHALLENGE_PATH_UPPER =
    '/.WELL-KNOWN/ACME-CHALLENGE/';


implementation


{ **************** Low-Level Cryptographic Wrappers }

function DerToEccSign(algo: TCryptAsymAlgo; const sign_der: RawByteString): RawUtf8;
const
  DER_SEQUENCE = #$30;
  CAA_ECCBYTES: array[TCryptAsymAlgo] of Integer = (
    32, // caaES256
    48, // caaES384
    66, // caaES512
    32, // caaES256K
    0,  // caaRS256
    0,  // caaRS384
    0,  // caaRS512
    0,  // caaPS256
    0,  // caaPS384
    0,  // caaPS512
    32); // caaEdDSA
var
  derlen: cardinal;
  der: PByteArray;
  eccbytes, len: integer;
  buf: array [0..131] of AnsiChar;
begin
  if algo = caaEdDSA then
  begin
    result := BinToBase64uri(pointer(sign_der), length(sign_der));
    exit;
  end;
  result := '';
  derlen := length(sign_der);
  der := pointer(sign_der);
  if (derlen < 50) or
     (der[0] <> ord(DER_SEQUENCE)) or
     (der[1] > derlen - 2) then
    exit;
  eccbytes := CAA_ECCBYTES[algo];
  if der[1] and $80 <> 0 then
  begin
    // 2-byte length
    assert((der[1] and $7f) = 1);
    len := der[2];
    if DerParse(DerParse(@der[3], @buf[0], eccbytes),
        @buf[eccbytes], eccbytes) <> PAnsiChar(@der[len + 3]) then
      exit;
  end
  else
  begin
    len := der[1];
    if DerParse(DerParse(@der[2], @buf[0], eccbytes),
        @buf[eccbytes], eccbytes) <> PAnsiChar(@der[len + 2]) then
      exit;
  end;
  result := BinToBase64uri(@buf[0], eccbytes * 2);
end;

function DerToJwsSign(algo: TCryptAsymAlgo; const sign_der: RawByteString): RawUtf8;
begin
  if algo in CAA_ECC then
    result := DerToEccSign(algo, sign_der)
  else
    result := BinToBase64uri(pointer(sign_der), length(sign_der));
end;

{ **************** JWS HTTP-client implementation }

{ TJwsHttpClient }

constructor TJwsHttpClient.Create(const aCert: ICryptCert);
begin
  inherited Create({aOnlyUseClientSocket=}false);
  fCert := aCert;
end;

function TJwsHttpClient.GetNonceAndBody: RawJson;
var
  err: RawUtf8;
begin
  // the server includes a Replay-Nonce header field in every response
  fNonce := FindIniNameValue(pointer(fHeaders), 'REPLAY-NONCE: ');
  if (fStatus <> HTTP_SUCCESS) and
     (fStatus <> HTTP_CREATED) and
     (fStatus <> HTTP_NOCONTENT) then
  begin
    err := JsonDecode(pointer(fBody), 'detail', nil, false);
    if err = '' then
      StatusCodeToReason(fStatus, err);
    raise EJwsHttp.CreateUtf8(
      'Error % [%] while querying %', [fStatus, err, fUri]);
  end;
  result := fBody;
end;

function TJwsHttpClient.Get(const aUrl: RawUtf8): RawJson;
begin
  Request(aUrl, 'GET');
  result := GetNonceAndBody;
end;

function TJwsHttpClient.Post(const aUrl: RawUtf8; const aJson: RawJson): RawJson;
var
  x, y: RawByteString;
  jwk, header: RawUtf8;
  thumb: TSha256Digest;
  header_enc, json_enc, body_enc: RawUtf8;
  sign: RawUtf8;
  data: RawUtf8;
begin
  if fKid <> '' then
  begin
    // we have the key identifier provided by the server
    header := FormatUtf8('{"alg":?,"kid":?,"nonce":?,"url":?}', [],
      [CAA_JWT[fCert.AsymAlgo], fKid, fNonce, aUrl], {json=}true)
  end
  else
  begin
    if fCert.PrivateKeyHandle = nil then
      raise EJwsHttp.Create('No private key');
    // No key identifier, need to provide JSON Web Key
    if fCert.AsymAlgo in CAA_ECC then
    begin
      PEVP_PKEY(fCert.PrivateKeyHandle).EccGetPubKeyUncompressed(x, y);
      jwk := FormatUtf8('{"crv":?,"kty":"EC","x":?,"y":?}',
        [], [CAA_CRV[fCert.AsymAlgo], BinToBase64uri(x), BinToBase64uri(y)], true);
    end
    else
    begin
      PEVP_PKEY(fCert.PrivateKeyHandle).RsaGetPubKey(x, y);
      jwk := FormatUtf8('{"e":?,"kty":"RSA","n":?}',
        [], [BinToBase64uri(x), BinToBase64uri(y)], true);
    end;
    // the thumbprint of a JWK is computed with no whitespace or line breaks
    // before or after any syntaxic elements and with the required members
    // ordered lexicographically, using SHA-256 hashing
    thumb := Sha256Digest(jwk);
    fJwkThumbprint := BinToBase64uri(@thumb, sizeof(thumb));
    header := FormatUtf8('{"alg":?,"jwk":%,"nonce":?,"url":?}',
      [jwk], [CAA_JWT[fCert.AsymAlgo], fNonce, aUrl], true);
  end;
  header_enc := BinToBase64uri(header);
  json_enc := BinToBase64uri(aJson);
  body_enc := header_enc + '.' + json_enc;
  sign := DerToJwsSign(fCert.AsymAlgo, fCert.Sign(body_enc));
  data := FormatUtf8('{"protected":?,"payload":?,"signature":?}',
    [], [header_enc, json_enc, sign], true);
  Request(aUrl, 'POST', '', data, 'application/jose+json');
  result := GetNonceAndBody;
  if fKid = '' then
    fKid := FindIniNameValue(pointer(fHeaders), 'LOCATION: ');
end;

function TJwsHttpClient.Post(const aUrl: RawUtf8;
  const aNameValues: array of const): RawJson;
begin
  result := Post(aUrl, JsonEncode(aNameValues));
end;


{ **************** ACME client implementation }

function AcmeTextToStatus(p: PUtf8Char): TAcmeStatus;
begin
  if IdemPChar(p, 'VALID') then
    result := asValid
  else if IdemPChar(p, 'READY') then
    result := asValid
  else if IdemPChar(p, 'PENDING') then
    result := asPending
  else
    result := asInvalid;
end;

function ToText(status: TAcmeStatus): PShortString;
begin
  result := GetEnumName(TypeInfo(TAcmeStatus), ord(status));
end;

// return identifers TDocVariant array from Subjects, as
// [ {"type":"dns", "value":"synopse.info"} ]
function GetIdentifiersArr(const aSubjects: RawUtf8): variant;
var
  ids: TRawUtf8DynArray;
  i, j: PtrInt;
  typ, val: RawUtf8;
begin
  CsvToRawUtf8DynArray(pointer(aSubjects), ids, ',', {trim=}true);
  VarClear(result);
  TDocVariantData(result).InitFast(length(ids), dvArray);
  for i := 0 to length(ids) - 1 do
  begin
    val := ids[i];
    j := PosExChar(':', val);
    if j > 0 then
    begin
      typ := copy(val, 1, j - 1);
      delete(val, 1, j);
    end
    else
      typ := 'dns';
    TDocVariantData(result).AddItem(_ObjFast([
      'type',  typ,
      'value', val]));
  end;
end;


{ TAcmeClient }

constructor TAcmeClient.Create(const aCert: ICryptCert;
  const aDirectoryUrl, aContact, aDomain, aSubjects: RawUtf8);
begin
  fDirectoryUrl := aDirectoryUrl;
  fContact := aContact;
  fDomain := aDomain;
  fSubjects := aSubjects;
  fHttpClient := TJwsHttpClient.Create(aCert);
end;

destructor TAcmeClient.Destroy;
begin
  FreeAndNil(fHttpClient);
end;

procedure TAcmeClient.ReadDirectory;
var
  resp: RawJson;
  v: array [0..2] of TValuePUtf8Char;
begin
  // In order to help clients configure themselves with the right URLs for
  // each ACME operation, ACME servers provide a directory object
  resp := fHttpClient.Get(fDirectoryUrl);
  JsonDecode(pointer(resp), [
    'newNonce',
    'newAccount',
    'newOrder'], @v, true);
  v[0].ToUtf8(fNewNonce);
  v[1].ToUtf8(fNewAccount);
  v[2].ToUtf8(fNewOrder);
  if (fNewNonce = '') or
     (fNewAccount = '') or
     (fNewOrder = '') then
    raise EAcmeClient.CreateUtf8('Invalid directory %', [fDirectoryUrl]);
end;

procedure TAcmeClient.CreateAccount;
var
  resp: RawJson;
  status: RawUtf8;
begin
  // A client creates a new account with the server by sending a POST
  // request to the server's newAccount URL
  resp := fHttpClient.Post(fNewAccount,
    ['termsOfServiceAgreed', true,
     'contact',              _ArrFast([fContact])]);
  status := JsonDecode(pointer(resp), 'status', nil, true);
  if AcmeTextToStatus(pointer(status)) <> asValid then
    raise EAcmeClient.CreateUtf8('% returned status % (expected "valid")',
      [fNewAccount, status]);
end;

function TAcmeClient.CreateOrder: TAcmeStatus;
var
  i, j: PtrInt;
  ch: ^TAcmeChallenge;
  r1, r2: RawJson;
  v1, v2: array [0..2] of TValuePUtf8Char;
  auth: TRawUtf8DynArray;
  p: PUtf8Char;
  chs: TPUtf8CharDynArray;
begin
  fChallenges := nil;
  // The client begins the certificate issuance process by sending a POST
  // request to the server's newOrder resource
  r1 := fHttpClient.Post(fNewOrder,
    ['identifiers', GetIdentifiersArr(fSubjects)]);
  JsonDecode(pointer(r1), [
    'status',
    'finalize',
    'authorizations'], @v1, true);
  result := AcmeTextToStatus(v1[0].Text);
  if result = asInvalid then
    raise EAcmeClient.CreateUtf8('% returned "%" (expected "pending" or "ready")',
      [fNewOrder, v1[0].Text]);
  v1[1].ToUtf8(fFinalize);
  // When a client receives an order from the server in reply to a
  // newOrder request, it downloads the authorization resources by sending
  // requests to the indicated URLs
  DynArrayLoadJson(auth, v1[2].Text, TypeInfo(TRawUtf8DynArray));
  SetLength(fChallenges, length(auth));
  ch := pointer(fChallenges);
  for i := 0 to length(auth) - 1 do
  begin
    r2 := fHttpClient.Post(auth[i], '');
    JsonDecode(pointer(r2), [
      'status',
      'identifier',
      'challenges'], @v1, true);
    ch^.Status := AcmeTextToStatus(v1[0].Text);
    JsonDecode(v1[1].Text, [
      'type',
      'value'], @v2, false);
    v2[0].ToUtf8(ch^.SubjectType);
    v2[1].ToUtf8(ch^.Subject);
    if ch^.Status = asPending then
    begin
      p := v1[2].Text;
      if NextNotSpaceCharIs(p, '[') then
        JsonArrayDecode(p, chs)
      else
        chs := nil;
      for j := 0 to length(chs) - 1 do
      begin
        JsonDecode(chs[j], [
          'type',
          'url',
          'token'], @v2, false);
        // support only HTTP validation by now
        if v2[0].Idem('HTTP-01') then
        begin
          v2[1].ToUtf8(ch^.Url);
          v2[2].ToUtf8(ch^.Token);
          // A key authorization is a string that
          // concatenates the token for the challenge with a key fingerprint
          // (using the SHA-256 digest), separated by a "." character
          ch^.Key := ch^.Token + '.' + fHttpClient.JwkThumbprint;
          break;
        end;
      end;
    end;
    inc(ch);
  end;
end;

procedure TAcmeClient.RequestAuth(const aJson: RawJson);
var
  i: PtrInt;
  resp: RawJson;
  status: RawUtf8;
begin
  // The client indicates to the server that it is ready for the challenge
  // validation by sending an empty body aJson = '{}'.
  // If aJson = '' then client requests validation state
  for i := 0 to length(fChallenges) - 1 do
  begin
    if fChallenges[i].Status = asPending then
    begin
      resp := fHttpClient.Post(fChallenges[i].Url, aJson);
      status := JsonDecode(pointer(resp), 'status', nil, false);
      fChallenges[i].Status := AcmeTextToStatus(pointer(status));
    end;
  end;
end;

procedure TAcmeClient.OnChallengeWwwFolder(Sender: TObject;
  const Domain, Key, Token: RawUtf8);
var
  fn: TFileName;
begin
  if fChallengeWwwFolder = '' then
    exit;
  fn := FormatString('%%', [fChallengeWwwFolder, Token]);
  if Key <> '' then
    FileFromString(Key, fn) // initial call
  else
    DeleteFile(fn);         // final call
end;

procedure TAcmeClient.StartDomainRegistration;
var
  i: PtrInt;
begin
  // In order to help clients configure themselves with the right URLs for
  // each ACME operation, ACME servers provide a directory object
  ReadDirectory;
  // Before sending a POST request to the server, an ACME client needs to
  // have a fresh anti-replay nonce to put in the "nonce" header of the JWS
  fHttpClient.Get(fNewNonce);
  // Create an account on an ACME server or retrieve existing
  CreateAccount;
  // Applying for Certificate Issuance
  if CreateOrder = asPending then
  begin
    // Notify top-level application
    if Assigned(fOnChallenges) then
      for i := 0 to length(fChallenges) - 1 do
        if fChallenges[i].Key <> '' then
          fOnChallenges(Self, fDomain, fChallenges[i].Key, fChallenges[i].Token);
    // Queue challenge testing by sending {} to initiate the server process
    RequestAuth('{}');
  end;
end;

function TAcmeClient.CheckChallengesStatus: TAcmeStatus;
var
  i, valid: PtrInt;
begin
  // Before sending a POST request to the server, an ACME client needs to
  // have a fresh anti-replay nonce to put in the "nonce" header of the JWS
  fHttpClient.Get(fNewNonce);
  // Check if challenge for a domain is completed
  RequestAuth(''); // {} to initiate, '' to check status
  // Compute result:
  // One invalid -> invalid
  // All valid -> valid
  // else pending
  result := asPending;
  valid := 0;
  for i := 0 to length(fChallenges) - 1 do
  begin
    if fChallenges[i].Status = asInvalid then
      result := asInvalid;
    if fChallenges[i].Status = asValid then
      inc(valid);
  end;
  if (result = asPending) and
     (valid = length(fChallenges)) then
    result := asValid;
end;

function TAcmeClient.CompleteDomainRegistration(out aCert, aPrivateKey: RawUtf8;
  const aPrivateKeyPassword: SpiUtf8): TAcmeStatus;
var
  csr: RawByteString;
  pk, resp: RawUtf8;
  i: PtrInt;
  v: array [0..1] of TValuePUtf8Char;
begin
  try
    // Compute the PKCS#10 Certificate Signing Request
    csr := fHttpClient.fCert.CreateSelfSignedCsr(
      fSubjects, aPrivateKeyPassword, pk);
    // Before sending a POST request to the server, an ACME client needs to
    // have a fresh anti-replay nonce to put in the "nonce" header of the JWS
    fHttpClient.Get(fNewNonce);
    // Once the validation process is complete and the server is satisfied
    // that the client has met its requirements, the client finalizes the
    // order by submitting a Certificate Signing Request (CSR)
    resp := fHttpClient.Post(fFinalize, [
      'csr', BinToBase64uri(csr)]);
    JsonDecode(pointer(resp), [
      'status',
      'certificate'], @v, true);
    result := AcmeTextToStatus(v[0].Text);
    if result = asValid then
    begin
      // The server has issued the certificate and provisioned its
      // URL to the "certificate" field of the order.
      // Download the certificate
      aCert := fHttpClient.Post(v[1].ToUtf8, '');
      if not IsPem(aCert) then
        result := asInvalid;
    end;
    if result = asValid then
      // only keep the generated private key on success
      aPrivateKey := pk
  finally
    FillZero(pk);
    FillZero(resp);
    if Assigned(fOnChallenges) then // call with key = '' to notify final state
      for i := 0 to length(fChallenges) - 1 do
        if fChallenges[i].Key <> '' then
          fOnChallenges(nil, fDomain, {key=}'', fChallenges[i].Token);
    fChallenges := nil;
  end;
end;

function TAcmeClient.RegisterAndWait(const OnChallenge: TOnAcmeChallenge;
  const OutSignedCert, OutPrivateKey: TFileName;
  const aPrivateKeyPassword: SpiUtf8; WaitForSec: integer): TAcmeStatus;
var
  endtix: Int64;
  cert, pk: RawUtf8;
begin
  fOnChallenges := OnChallenge;
  StartDomainRegistration;
  endtix := GetTickCount64 + WaitForSec * 1000;
  repeat
    sleep(1000);
    result := CheckChallengesStatus;
    if result <> asPending then
      break;
  until GetTickCount64 > endtix;
  if result <> asValid then
    exit;
  result := CompleteDomainRegistration(cert, pk, aPrivateKeyPassword);
  if result = asValid then
    try
      FileFromString(cert, OutSignedCert);
      FileFromString(pk, OutPrivateKey);
    finally
      FillZero(cert);
      FillZero(pk);
    end;
end;

function TAcmeClient.RegisterAndWait(const ChallengeWwwFolder, OutSignedCert,
  OutPrivateKey: TFileName; const aPrivateKeyPassword: SpiUtf8;
  WaitForSec: integer): TAcmeStatus;
begin
  if fChallengeWwwFolder <> '' then
    raise EAcmeClient.CreateUtf8(
      '%.RegisterAndWait: already called as %', [self, fChallengeWwwFolder]);
  if not DirectoryExists(ChallengeWwwFolder) then
    raise EAcmeClient.CreateUtf8(
      '%.RegisterAndWait: unknown %', [self, ChallengeWwwFolder]);
  fChallengeWwwFolder := EnsureDirectoryExists(
    FormatString('%.well-known%acme-challenge',
    [IncludeTrailingPathDelimiter(ChallengeWwwFolder), PathDelim]), true);
  try
    result := RegisterAndWait(OnChallengeWwwFolder,
      OutSignedCert, OutPrivateKey, aPrivateKeyPassword, WaitForSec);
  finally
    fChallengeWwwFolder := '';
  end;
end;


{$else}

implementation

{$endif USE_OPENSSL}

end.
