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
    - Let's Encrypt TLS / HTTPS Encryption Certificates Support

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
  mormot.core.log,
  mormot.crypt.core,
  mormot.crypt.secure,
  mormot.lib.openssl11,
  mormot.net.sock,
  mormot.net.client, // for TJwsHttpClient
  mormot.net.server; // for HTTP-01 challenge server


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
    /// perform a HEAD request, not signed/authenticated
    function Head(const aUrl: RawUtf8): RawJson;
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
  CAA_CRV: array[TCryptAsymAlgo] of PUtf8Char = (
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

  /// the details of one ACME HTTP-01 challenge
  TAcmeChallenge = record
    /// the current status of this challenge
    Status: TAcmeStatus;
    /// the "identifier.type" field, typically 'dns'
    SubjectType: RawUtf8;
    /// the "identifier.value" field, typically 'synopse.info'
    SubjectValue: RawUtf8;
    /// the "url" field of the challenge
    // - used to trigger and check the challenge state
    // - e.g. 'https://<LETSENCRYPT>/acme/chall/prV_B7yEyA4' in RFC 8555
    Url: RawUtf8;
    /// the "token" field of the challenge
    // - e.g. 'DGyRejmCefe7v4NfDGDKfA' in RFC 8555
    Token: RawUtf8;
    /// the key authorization, i.e. the challenge content to be returned
    // - i.e. the concatenation of the Token, '.', and the JWK base-64 hash
    // - from URI as http://<Domain>/.well-known/acme-challenge/<Token>
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
    fSubjects: TRawUtf8DynArray;
    fHttpClient: TJwsHttpClient;
    fChallenges: TAcmeChallengeDynArray;
    fOnChallenges: TOnAcmeChallenge;
    fChallengeWwwFolder: TFileName;
    fSafe: TLightLock;
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
    // - aCert is a local private certificate used to identify the client
    // account for the JWK requests - so is not involved in the TLS chain
    constructor Create(const aCert: ICryptCert;
      const aDirectoryUrl, aContact: RawUtf8; const aSubjects: TRawUtf8DynArray);
    /// finalize the instance
    destructor Destroy; override;
    /// search for a given Challenge token, and return the associated key
    function GetChallenge(aUri: PUtf8Char; aUriLen: PtrInt;
      var Content: RawUtf8): boolean;
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
    // - won't execute function CompleteDomainRegistration() if OutSignedCert
    // or OutPrivateKey are not set
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
    /// make this instance thread-safe
    property Safe: TLightLock
      read fSafe;
    /// associated contact as mailto: link, e.g. 'mailto:admin@synopse.info'
    property Contact: RawUtf8
      read fContact write fContact;
    /// associated subjects as array, typically domain names to authenticate
    // - e.g. ['synopse.info','www.synopse.info']
    property Subjects: TRawUtf8DynArray
      read fSubjects write fSubjects;
    /// low-level direct access to the associated challenges
    // - may be used instead of OnChallenges callback
    property Challenges: TAcmeChallengeDynArray
      read fChallenges;
    /// callback to let the top-level application publish the challenges
    property OnChallenges: TOnAcmeChallenge
      read fOnChallenges write fOnChallenges;
  end;

  TAcmeClientObjArray = array of TAcmeClient;



function ToText(status: TAcmeStatus): PShortString; overload;


{ **************** Let's Encrypt TLS / HTTPS Encryption Certificates Support }

const
  ACME_LETSENCRYPT_URL =
    'https://acme-v02.api.letsencrypt.org/directory';
  ACME_LETSENCRYPT_DEBUG_URL =
    'https://acme-staging-v02.api.letsencrypt.org/directory';

  ACME_CHALLENGE_PATH =
    '/.well-known/acme-challenge/';
  ACME_CHALLENGE_PATH_UPPER =
    '/.WELL-KNOWN/ACME-CHALLENGE/';
  ACME_CHALLENGE_PATH_LEN = length(ACME_CHALLENGE_PATH);

type
  EAcmeLetsEncrypt = class(ESynException);

  TAcmeLetsEncrypt = class;

  /// used to generate or renew one Let's Encrypt domain certificate
  TAcmeLetsEncryptClient = class(TAcmeClient)
  protected
    fDomainJson, fReferenceCert, fSignedCert, fPrivKey: TFileName;
    fOwner: TAcmeLetsEncrypt;
    fCtx: PSSL_CTX;
    fRenewing: boolean;
    procedure ClearCtx;
    // internal method called by TAcmeLetsEncrypt.OnNetTlsAcceptServerName
    function GetServerContext: PSSL_CTX;
  public
    /// initialize certificate management with Let's Encrypt of one domain
    // - aDomainFile is the ##.json of this domain to be read from disk
    // with its associated ##.acme.pem, ##.crt.me, and ##.key.pem files
    constructor Create(aOwner: TAcmeLetsEncrypt; const aDomainFile: TFileName); reintroduce;
    /// finalize the information of this domain certificate
    destructor Destroy; override;
  end;
  TAcmeLetsEncryptClientObjArray = array of TAcmeLetsEncryptClient;

  /// handle one or several Let's Encrypt domains certificates
  // - with automated generation and renewal
  // - information is located in a single aKeyStoreFolder directory, as
  // associated ##.json, ##.acme.pem, ##.crt.me, and ##.key.pem files
  TAcmeLetsEncrypt = class(TSynPersistentRWLightLock)
  protected
    fSafe: TLightLock;
    fClient: TAcmeLetsEncryptClientObjArray;
    fKeyStoreFolder: TFileName;
    fPrivateKeyPassword: SpiUtf8;
    fDirectoryUrl, fAlgo: RawUtf8;
    fRenewBeforeEndDays: integer;
    fRenewWaitForSeconds: integer;
    fOnChallenge: TOnAcmeChallenge;
    function GetClient(const ServerName: RawUtf8): TAcmeLetsEncryptClient;
    function GetClientLocked(const ServerName: RawUtf8): TAcmeLetsEncryptClient;
  public
    /// initialize certificates management with Let's Encrypt
    // - if aDirectoryUrl is not '', will use the "staging" environment - you
    // should specify ACME_LETSENCRYPT_URL on production
    // - if aAlgo is '', will use 'x509-es256' as default
    // - a global aPrivateKeyPassword could be set to protect ##.key.pem files
    constructor Create(const aKeyStoreFolder: TFileName;
      const aDirectoryUrl, aAlgo: RawUtf8;
      const aPrivateKeyPassword: SpiUtf8); reintroduce;
    /// read the certificates from the local storage folder
    procedure LoadFromKeyStoreFolder;
    /// validate the stored certificates
    // - load each one, check their expiration date against RenewBeforeEndDays,
    // and generate or renew them in order
    procedure CheckCertificates;
    /// finalize the domain
    destructor Destroy; override;
    /// TOnNetTlsAcceptServerName event, set to OnNetTlsAcceptServerName
    // global variable of mormot.net.sock
    function OnNetTlsAcceptServerName(Context: PNetTlsContext; TLS: pointer;
      const ServerName: RawUtf8): pointer;
    /// TOnNetTlsAcceptChallenge event, set to OnNetTlsAcceptChallenge
    // global variable of mormot.net.sock
    // - Let's Encrypt typical uri is '/.well-known/acme-challenge/<TOKEN>'
    function OnNetTlsAcceptChallenge(const domain, uri: RawUtf8;
      var content: RawUtf8): boolean;
    /// raw access to the internal Client list
    property Client: TAcmeLetsEncryptClientObjArray
      read fClient;
    /// where the certificates and related information are persisted
    property KeyStoreFolder: TFileName
      read fKeyStoreFolder;
    /// how many days before expiration CheckCertificates() should renew a
    // certificate
    // - default is 30 days
    // - set to <= 0 to disable the whole CheckCertificates() process
    property RenewBeforeEndDays: integer
      read fRenewBeforeEndDays write fRenewBeforeEndDays;
    /// how many seconds CheckCertificates() should wait for getting a certificate
    // - default is 30 seconds
    property RenewWaitForSeconds: integer
      read fRenewWaitForSeconds write fRenewWaitForSeconds;
    /// a callback which may be needed during CheckCertificates() process
    // - not needed if an internal HTTP server is processed
    property OnChallenge: TOnAcmeChallenge
      read fOnChallenge write fOnChallenge;
    /// make the internal Client list thread-safe
    property Safe: TLightLock
      read fSafe;
  end;


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

function TJwsHttpClient.Head(const aUrl: RawUtf8): RawJson;
begin
  Request(aUrl, 'HEAD');
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
function GetIdentifiersArr(const aSubjects: TRawUtf8DynArray): variant;
var
  i, j: PtrInt;
  typ, val: RawUtf8;
begin
  VarClear(result);
  TDocVariantData(result).InitFast(length(aSubjects), dvArray);
  for i := 0 to length(aSubjects) - 1 do
  begin
    val := aSubjects[i];
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
  const aDirectoryUrl, aContact: RawUtf8; const aSubjects: TRawUtf8DynArray);
begin
  inherited Create;
  fDirectoryUrl := aDirectoryUrl;
  fContact := aContact;
//  CsvToRawUtf8DynArray(pointer(aSubjects), fSubjects, ',', {trim=}true);
  if aSubjects = nil then
    raise EAcmeClient.Create('Create with aSubjects=nil');
  fSubjects := aSubjects;
  fHttpClient := TJwsHttpClient.Create(aCert);
end;

destructor TAcmeClient.Destroy;
begin
  FreeAndNil(fHttpClient);
  inherited Destroy;
end;

function TAcmeClient.GetChallenge(aUri: PUtf8Char; aUriLen: PtrInt;
  var Content: RawUtf8): boolean;
var
  i: PtrInt;
  c: ^TAcmeChallenge;
begin
  c := pointer(fChallenges);
  if aUriLen > 0 then
    for i := 1 to length(fChallenges) do
    begin
      if (aUriLen = length(c^.Token)) and
        mormot.core.base.CompareMem(pointer(c^.Token), aUri, aUriLen) then
      begin
        result := true;
        Content := c^.Key;
        exit;
      end;
      inc(c);
    end;
  result := false;
end;

procedure TAcmeClient.ReadDirectory;
var
  resp: RawJson;
  v: array [0..2] of TValuePUtf8Char;
begin
  // In order to help clients configure themselves with the right URLs for
  // each ACME operation, ACME servers provide a directory object
  resp := fHttpClient.Head(fDirectoryUrl);
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
    v2[1].ToUtf8(ch^.SubjectValue);
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
          break; // we found and stored the "http-01" challenge
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
  fHttpClient.Head(fNewNonce);
  // Create an account on an ACME server or retrieve existing
  CreateAccount;
  // Applying for Certificate Issuance
  if CreateOrder = asPending then
  begin
    // Notify top-level application
    if Assigned(fOnChallenges) then
      for i := 0 to length(fChallenges) - 1 do
        if fChallenges[i].Key <> '' then
          fOnChallenges(Self, fSubjects[0], fChallenges[i].Key, fChallenges[i].Token);
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
  fHttpClient.Head(fNewNonce);
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
    // Generate a new PKCS#10 Certificate Signing Request
    csr := fHttpClient.fCert.CertAlgo.CreateSelfSignedCsr(
      fSubjects, aPrivateKeyPassword, pk);
    // Before sending a POST request to the server, an ACME client needs to
    // have a fresh anti-replay nonce to put in the "nonce" header of the JWS
    fHttpClient.Head(fNewNonce);
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
          fOnChallenges(nil, fSubjects[0], {key=}'', fChallenges[i].Token);
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
  if (result <> asValid) or
     (OutSignedCert = '') or
     (OutPrivateKey = '') then
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


{ **************** Let's Encrypt TLS / HTTPS Encryption Certificates Support }

{ TAcmeLetsEncryptClient }

constructor TAcmeLetsEncryptClient.Create(
  aOwner: TAcmeLetsEncrypt; const aDomainFile: TFileName);
var
  cc: ICryptCert;
  dom: TDocVariantData;
  c: RawUtf8;
  s: TRawUtf8DynArray;
begin
  fOwner := aOwner;
  fDomainJson    := aDomainFile + '.json';
  fReferenceCert := aDomainFile + '.acme.pem';
  fSignedCert    := aDomainFile + '.crt.pem';
  fPrivKey       := aDomainFile + '.key.pem';
  dom.InitJsonFromFile(fDomainJson, []);
  c := dom.U['contact'];
  dom.A['subjects'].ToRawUtf8DynArray(s);
  cc := Cert(fOwner.fAlgo);
  if cc = nil then
    raise EAcmeLetsEncrypt.CreateUtf8(
      '%.Create: unsupported %', [self, fOwner.fAlgo]);
  if not FileExists(fSignedCert) or
     not FileExists(fPrivKey) or
     not cc.LoadFromFile(fReferenceCert, cccCertWithPrivateKey) then
  begin
    DeleteFile(fReferenceCert);
    DeleteFile(fSignedCert);
    DeleteFile(fPrivKey);
    // create a new certificate to sign the JWK queries (not used for TLS)
    cc.Generate([cuDigitalSignature], s[0], nil, 3650);
    cc.SaveToFile(fReferenceCert, cccCertWithPrivateKey); // no password needed
  end;
  inherited Create(cc, fOwner.fDirectoryUrl, c, s);
end;

destructor TAcmeLetsEncryptClient.Destroy;
begin
  ClearCtx;
  inherited Destroy;
end;

procedure TAcmeLetsEncryptClient.ClearCtx;
begin
  if fCtx <> nil then
    fCtx.Free;
  fCtx := nil;
end;

function TAcmeLetsEncryptClient.GetServerContext: PSSL_CTX;
begin
  // client made fSafe.Lock
  result := nil;
  if fRenewing then
    exit; // not the right time for sure
  result := fCtx;
  if result <> nil then
    exit; // most of time, quick return from cache
  // will be assigned by SSL_set_SSL_CTX() which requires only a certificate
  result := SSL_CTX_new(TLS_server_method);
  // cut-down version of TOpenSslNetTls.SetupCtx
  if result.SetCertificateFiles(fSignedCert, fPrivKey, fOwner.fPrivateKeyPassword) then
    fCtx := result
  else
  begin
    result.Free;
    result := nil; // silently fallback to main certificate
  end;
end;



{ TAcmeLetsEncrypt }

constructor TAcmeLetsEncrypt.Create(const aKeyStoreFolder: TFileName;
  const aDirectoryUrl, aAlgo: RawUtf8; const aPrivateKeyPassword: SpiUtf8);
begin
  inherited Create;
  if aAlgo = '' then
    fAlgo := 'x509-es256'
  else
    fAlgo := aAlgo;
  if aDirectoryUrl = '' then
    fDirectoryUrl := ACME_LETSENCRYPT_DEBUG_URL
  else
    fDirectoryUrl := aDirectoryUrl;
  fKeyStoreFolder := EnsureDirectoryExists(aKeyStoreFolder, {raiseonfail=}true);
  fPrivateKeyPassword := aPrivateKeyPassword;
  fRenewWaitForSeconds := 30;
  fRenewBeforeEndDays := 30;
end;

destructor TAcmeLetsEncrypt.Destroy;
begin
  FillZero(fPrivateKeyPassword);
  ObjArrayClear(fClient);
  inherited Destroy;
end;

procedure TAcmeLetsEncrypt.LoadFromKeyStoreFolder;
var
  f: TSearchRec;
  fn: TFileName;
begin
  mormot.net.sock.OnNetTlsAcceptServerName := nil;
  fSafe.Lock;
  try
    ObjArrayClear(fClient);
    if FindFirst(fKeyStoreFolder + '*.json', faAnyFile, f) = 0 then
    begin
      repeat
         if SearchRecValidFile(f) then
           try
             fn := fKeyStoreFolder + GetFileNameWithoutExt(f.Name);
             ObjArrayAdd(fClient, TAcmeLetsEncryptClient.Create(self, fn));
           except
             RenameFile(fn, fn + '.invalid'); // don't try it again
           end;
      until FindNext(f) <> 0;
      FindClose(f);
    end;
  finally
    fSafe.UnLock;
  end;
  if fClient <> nil then
    mormot.net.sock.OnNetTlsAcceptServerName := OnNetTlsAcceptServerName;
end;

procedure TAcmeLetsEncrypt.CheckCertificates;
var
  i: PtrInt;
  c: TAcmeLetsEncryptClient;
  cc: ICryptCert;
  needed: TRawUtf8DynArray; // no long standing fSafe.Lock
  expired: TDateTime;
  res: TAcmeStatus;
begin
  if (self = nil) or
     (fRenewBeforeEndDays <= 0) then
    exit;
  // quickly retrieve all certificates which need generation/renewal
  cc := Cert(fAlgo);
  if cc = nil then
    exit;
  expired := NowUtc - fRenewBeforeEndDays;
  fSafe.Lock;
  try
    for i := 0 to length(fClient) - 1 do
    begin
      c := fClient[i];
      if GetClient(c.Subjects[0]) = c then // avoid duplicated names confusion
        if not cc.LoadFromFile(c.fSignedCert) or
           (cc.GetNotAfter < expired) then
          AddRawUtf8(needed, c.Subjects[0]);
    end;
  finally
    fSafe.UnLock;
  end;
  // renew the needed certificates
  for i := 0 to length(needed) - 1 do
  begin
    c := GetClientLocked(needed[i]); // lookup by subject
    if c = nil then
      continue; // paranoid
    if c.fRenewing then
    begin
      c.Safe.UnLock;
      continue; // unlikely race condition
    end;
    res := asPending;
    c.fRenewing := true;
    c.Safe.UnLock; // allow e.g. OnNetTlsAcceptChallenge() lookup
    try
      res := c.RegisterAndWait(nil,
        c.fSignedCert, c.fPrivKey, fPrivateKeyPassword, fRenewWaitForSeconds);
      if res = asValid then
        c.ClearCtx;
    except
      res := asInvalid;
    end;
    c.fRenewing := false;
    if res <> asValid then
      exit;
    // validate and pre-load this new certificate
    c.Safe.Lock; // as expected by c.GetServerContext
    try
      if c.GetServerContext = nil then
        res := asInvalid;
    except
      res := asInvalid;
    end;
    c.Safe.UnLock; // no need to restart the server :)
  end;
end;

function TAcmeLetsEncrypt.GetClient(
  const ServerName: RawUtf8): TAcmeLetsEncryptClient;
var
  i: PtrInt;
begin
  for i := 0 to length(fClient) - 1 do
  begin
    result := fClient[i];
    if FindPropName(pointer(result.Subjects), ServerName,
         length(result.Subjects)) >= 0 then
      exit;
  end;
  result := nil; // not found
end;

function TAcmeLetsEncrypt.GetClientLocked(
  const ServerName: RawUtf8): TAcmeLetsEncryptClient;
begin
  fSafe.Lock;
  try
    result := GetClient(ServerName);
    if result <> nil then
      result.Safe.Lock;
  finally
    fSafe.UnLock;
  end;
end;

function TAcmeLetsEncrypt.OnNetTlsAcceptServerName(Context: PNetTlsContext;
  TLS: pointer; const ServerName: RawUtf8): pointer;
var
  client: TAcmeLetsEncryptClient;
begin
  client := GetClientLocked(ServerName);
  if client <> nil then
    try
      result := client.GetServerContext; // PSSL_CTX from cache
    finally
      client.Safe.UnLock;
    end
  else
    result := nil;
end;

const
  _ACME_CHALLENGE_PATH: PUtf8Char = ACME_CHALLENGE_PATH;

function TAcmeLetsEncrypt.OnNetTlsAcceptChallenge(const domain, uri: RawUtf8;
  var content: RawUtf8): boolean;
var
  client: TAcmeLetsEncryptClient;
  P: PUtf8Char;
  len: PtrInt;
begin
  result := false;
  P := pointer(uri);
  len := length(uri) - ACME_CHALLENGE_PATH_LEN;
  if (fClient = nil) or
     (len <= 0) or
     not CompareMem(P, @_ACME_CHALLENGE_PATH, ACME_CHALLENGE_PATH_LEN) then
    exit;
  client := GetClientLocked(domain);
  if client <> nil then
    try
      result := client.GetChallenge(P + ACME_CHALLENGE_PATH_LEN, len, content);
    finally
      client.Safe.UnLock;
    end;
end;


{$else}

implementation

{$endif USE_OPENSSL}

end.
