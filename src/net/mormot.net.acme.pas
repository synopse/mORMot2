/// Network ACME / Let's Encrypt - ZeroSSL Support
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.acme;

{
  *****************************************************************************

   Automatic Certificate Management Environment (ACME v2) Client
    - JWS HTTP-client implementation
    - ACME client implementation
    - Let's Encrypt - ZeroSSL TLS / HTTPS Encryption Certificates Support
    - HTTP-01 Let's Encrypt Challenges HTTP Server on port 80
    - HTTP/HTTPS Fully Featured Multi-Host Web Server

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
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.datetime,
  mormot.core.variants,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.log,
  mormot.core.threads,
  mormot.core.search,
  mormot.crypt.core,
  mormot.crypt.jwt,
  mormot.crypt.secure,
  mormot.lib.openssl11, // for per-domain-name PSSL_CTX certificates
  mormot.net.sock,
  mormot.net.http,
  mormot.net.client, // for TJwsHttpClient
  mormot.net.server; // for HTTP-01 challenge server


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
    fLog: TSynLogClass;
    function GetNonceAndBody: RawJson;
  public
    /// initialize this JWS-oriented HTTP client
    constructor Create(aLog: TSynLogClass; const aCert: ICryptCert);
    /// perform a HEAD request, not signed/authenticated
    function Head(const aUrl: RawUtf8): RawJson;
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
  // free domain validated certificates, mainly from Let's Encrypt or ZeroSSL
  // - see https://letsencrypt.org/how-it-works for a high-level description
  TAcmeClient = class(TObjectOSLightLock)
  protected
    fDirectoryUrl: RawUtf8;
    fContact: RawUtf8;
    fSubjects: RawUtf8;
    fSubject: TRawUtf8DynArray;
    fEabAlgo: TSignAlgo;
    fEabKid: RawUtf8;
    fEabMacKey: RawUtf8;
    fHttpClient: TJwsHttpClient;
    fChallenges: TAcmeChallengeDynArray;
    fOnChallenges: TOnAcmeChallenge;
    fChallengeWwwFolder: TFileName;
    fLog: TSynLogClass;
    // URI filled by ReadDirectory
    fNewNonce: RawUtf8;
    fNewAccount: RawUtf8;
    fNewOrder: RawUtf8;
    // URI filled by CreateOrder
    fOrder: RawUtf8;
    fFinalize: RawUtf8;
    procedure ReadDirectory;
    procedure ComputeEab(out result: RawUtf8);
    procedure CreateAccount;
    function CreateOrder: TAcmeStatus;
    function RequestAuth(const aJson: RawJson): integer;
    procedure OnChallengeWwwFolder(Sender: TObject;
      const Domain, Key, Token: RawUtf8);
  public
    /// create an ACME client instance
    // - aDirectoryUrl is the main URL of a directory object, typically
    // ACME_LETSENCRYPT_URL / ACME_ZEROSSL_URL (for production) or
    // ACME_LETSENCRYPT_DEBUG_URL / ACME_ZEROSSL_DEBUG_URL (for testing)
    // - aCert is a local private certificate used to identify the client
    // account for the JWK requests - so is not involved in the TLS chain
    constructor Create(aLog: TSynLogClass; const aCert: ICryptCert;
      const aDirectoryUrl, aContact, aSubjects: RawUtf8); reintroduce;
    /// finalize the instance
    destructor Destroy; override;
    /// check if a Server Name text is part of associated Subjects
    function MatchAny(const aServerName: RawUtf8): boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// search for a given Challenge token, and return the associated key
    function GetChallenge(aUri: PUtf8Char; aUriLen: PtrInt;
      var Content: RawUtf8): boolean;
    /// register account and applying for Certificate Issuance
    // - caller should have set the OnChallenges, Contact and Subjects
    // properties with the expected event or values
    procedure StartDomainRegistration;
    /// check if challenge for a domain is completed
    function CheckChallengesStatus: TAcmeStatus;
    /// finalize the order by submitting a Certificate Signing Request (CSR)
    // - RegisterAndWait will eventually wait and call CompleteDomainRegistration()
    // to actually download the signed certificate
    // - aPrivateKey contains the PEM private key generated locally,
    // optionally encrypted via aPrivateKeyPassword, corresponding to the CSR
    function FinalizeDomainRegistration(out aPrivateKey: RawUtf8;
      const aPrivateKeyPassword: SpiUtf8): TAcmeStatus;
    /// download the signed certificate
    // - aCert contain the final PEM certificate as signed by the ACME server
    function CompleteDomainRegistration(out aCert: RawUtf8): TAcmeStatus;
    /// call OnChallenges to clear temp files, if needed
    procedure ClearDomainRegistration;
    /// will run StartDomainRegistration and wait until it is completed
    // - low-level wrapper using any TOnAcmeChallenge callback
    // - won't execute function CompleteDomainRegistration() if OutSignedCert
    // or OutPrivateKey are not set
    function RegisterAndWait(const OnChallenge: TOnAcmeChallenge;
      const OutSignedCert, OutPrivateKey: TFileName;
      const aPrivateKeyPassword: SpiUtf8; WaitForSec: cardinal;
      Terminated: PBoolean): TAcmeStatus;
    /// will run StartDomainRegistration and wait until it is completed
    // - ChallengeWwwFolder is a local folder where to store the temporary
    // challenges, to be served by an external web server, e.g. nginx - the
    // method will append ACME_CHALLENGE_PATH to the supplied local folder name
    function RegisterAndWaitFolder(
      const ChallengeWwwFolder, OutSignedCert, OutPrivateKey: TFileName;
      const aPrivateKeyPassword: SpiUtf8; WaitForSec: integer): TAcmeStatus;
    /// associated contact as mailto: link, e.g. 'mailto:admin@synopse.info'
    property Contact: RawUtf8
      read fContact write fContact;
    /// associated subjects as CSV, typically domain names to authenticate
    // - e.g. 'synopse.info,www.synopse.info'
    // - match Subject[] array field
    property Subjects: RawUtf8
      read fSubjects write fSubjects;
    /// associated subjects as array, typically domain names to authenticate
    // - e.g. ['synopse.info', 'www.synopse.info']
    // - match Subjects CSV field
    property Subject: TRawUtf8DynArray
      read fSubject write fSubject;
    /// EAB MAC algoritm
    // - "External Account Binding" can be used to associate an ACME account
    // with an existing account in non-ACME system
    // - typically saSha256
    property EabAlgo: TSignAlgo
      read fEabAlgo write fEabAlgo;
    /// EAB key identifier
    // - provided by external system
    property EabKid: RawUtf8
      read fEabKid write fEabKid;
    /// EAB MAC key
    // - provided by external system
    property EabMacKey: RawUtf8
      read fEabMacKey write fEabMacKey;
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


{ ********** Let's Encrypt - ZeroSSL TLS / HTTPS Encryption Certificates Support }

const
  ACME_LETSENCRYPT_URL =
    'https://acme-v02.api.letsencrypt.org/directory';
  ACME_LETSENCRYPT_DEBUG_URL =
    'https://acme-staging-v02.api.letsencrypt.org/directory';

  ACME_ZEROSSL_URL =
    'https://acme.zerossl.com/v2/DV90';
  ACME_ZEROSSL_DEBUG_URL =
    'https://acme.zerossl.com/v2/DV90/staging';

  ACME_CHALLENGE_PATH =
    '/.well-known/acme-challenge/';
  ACME_CHALLENGE_PATH_LEN = length(ACME_CHALLENGE_PATH);

type
  EAcmeLetsEncrypt = class(ESynException);

  TAcmeLetsEncrypt = class;

  /// used to generate or renew one Let's Encrypt - ZeroSSL domain certificate
  TAcmeLetsEncryptClient = class(TAcmeClient)
  protected
    fDomainJson, fReferenceCert, fSignedCert, fPrivKey: TFileName;
    fOwner: TAcmeLetsEncrypt;
    fSslCtx: PSSL_CTX; // shared for all server connections
    fRedirectHttps: RawUtf8;
    fRenewing: boolean;
    fServerContextTix32: cardinal;
    fSignedCertTime, fPrivKeyTime: TUnixTime;
    // internal method called by TAcmeLetsEncrypt.OnNetTlsAcceptServerName
    function GetServerContext: PSSL_CTX;
    function NewServerContext(const Cert, Key: TFileName): PSSL_CTX;
    procedure ReplaceContext(New: PSSL_CTX);
  public
    /// initialize certificate management with Let's Encrypt of one domain
    // - aDomainFile is the ##.json of this domain to be read from disk
    // with its associated ##.acme.pem, ##.crt.me, and ##.key.pem files
    // - typically aDomainFile is
    // $ {"contact":"mailto:admin@synopse.info","subjects":["synopse.info","www.synopse.info"]}
    // optionally with a "eab" member e.g. for ZeroSSL:
    // $ { .. , "eab":{"algo":"Sha256","kid":"xxx","mac_key":"zzzzz'}}
    constructor Create(aOwner: TAcmeLetsEncrypt; const aDomainFile: TFileName); reintroduce;
    /// finalize the information of this domain certificate
    destructor Destroy; override;
  end;
  TAcmeLetsEncryptClientObjArray = array of TAcmeLetsEncryptClient;

  /// handle one or several Let's Encrypt - ZeroSSL domains certificates
  // - with automated generation and renewal
  // - information is located in a single aKeyStoreFolder directory, as
  // associated ##.json, ##.acme.pem, ##.crt.me, and ##.key.pem files
  TAcmeLetsEncrypt = class(TSynPersistent)
  protected
    fSafe: TRWLightLock;
    fClient: TAcmeLetsEncryptClientObjArray;
    fKeyStoreFolder: TFileName;
    fPrivateKeyPassword: SpiUtf8;
    fDirectoryUrl, fAlgo: RawUtf8;
    fLog: TSynLogClass;
    fRenewBeforeEndDays: integer;
    fRenewWaitForSeconds: integer;
    fClientsRenewing: boolean;
    fShutdown: boolean;
    fOnChallenge: TOnAcmeChallenge;
    function GetClient(const ServerName: RawUtf8): TAcmeLetsEncryptClient;
    function GetClientLocked(const ServerName: RawUtf8): TAcmeLetsEncryptClient;
    procedure WaitUntilNotRenewing(const Context: ShortString);
    procedure SetCallbackForLoadFromKeyStoreFolder(Enabled: boolean); virtual; abstract;
  public
    /// initialize certificates management with Let's Encrypt - ZeroSSL
    // - if aDirectoryUrl is not '', will use the "staging" environment - you
    // should specify ACME_LETSENCRYPT_URL / ACME_ZEROSSL_URL on production
    // - if aAlgo is '', will use 'x509-es256' as default
    // - a global aPrivateKeyPassword could be set to protect ##.key.pem files
    constructor Create(aLog: TSynLogClass; const aKeyStoreFolder: TFileName;
      const aDirectoryUrl, aAlgo: RawUtf8;
      const aPrivateKeyPassword: SpiUtf8); reintroduce;
    /// finalize the certificates management
    destructor Destroy; override;
    /// read the certificates from the local storage folder
    procedure LoadFromKeyStoreFolder;
    /// validate the stored certificates in a background TLoggedWorkThread
    procedure CheckCertificatesBackground;
    /// run by CheckCertificatesBackground to validate the stored certificates
    // - load each one, check their expiration date against RenewBeforeEndDays,
    // and generate or renew them in order
    // - follow RenewWaitForSeconds timeout for each certificate
    // - this blocking process could take some time (several seconds per domain)
    procedure CheckCertificates(Sender: TObject);
    /// low-level TOnNetTlsAcceptServerName event for the HTTPS server SNI
    // - as supplied to THttpServerGeneric.SetTlsServerNameCallback()
    function OnNetTlsAcceptServerName(Context: PNetTlsContext; TLS: pointer;
      ServerName: PUtf8Char): pointer;
    /// compute the challenge to be returned by the HTTP server on port 80
    // - ACME typical uri is '/.well-known/acme-challenge/<TOKEN>'
    function HttpServerChallenge(const domain, uri: RawUtf8;
      var content: RawUtf8): boolean;
    /// raw access to the internal Client list
    property Client: TAcmeLetsEncryptClientObjArray
      read fClient;
    /// a callback which may be needed during CheckCertificates() process
    // - not needed if an internal HTTP server is processed
    property OnChallenge: TOnAcmeChallenge
      read fOnChallenge write fOnChallenge;
  published
    /// the algorithm used for the certificates
    property KeyAlgo: RawUtf8
      read fAlgo;
    /// where the certificates and related information are persisted
    property KeyStoreFolder: TFileName
      read fKeyStoreFolder;
    /// the URI root folder used for ACME authentication
    // - typically ACME_LETSENCRYPT_URL / ACME_ZEROSSL_URL (for production) or
    // ACME_LETSENCRYPT_DEBUG_URL / ACME_ZEROSSL_DEBUG_URL (for testing)
    property DirectoryUrl: RawUtf8
      read fDirectoryUrl;
    /// how many days before expiration CheckCertificates() should renew a
    // certificate
    // - default is 30 days, as stated by https://letsencrypt.org/docs/faq
    // - set to <= 0 to disable the whole CheckCertificates() process
    property RenewBeforeEndDays: integer
      read fRenewBeforeEndDays write fRenewBeforeEndDays;
    /// how many seconds CheckCertificates() should wait for getting a certificate
    // - default is 30 seconds, which seems fair enough
    property RenewWaitForSeconds: integer
      read fRenewWaitForSeconds write fRenewWaitForSeconds;
  end;


{ *********** HTTP-01 Let's Encrypt - ZeroSSL Challenges HTTP Server on port 80 }

type
  /// handle one or several Let's Encrypt - ZeroSSL domains certificates with an
  // associated HTTP server running on port 80
  // - will support HTTP-01 challenge answers when renewing is active
  // - will redirect any plain HTTP port 80 request to HTTPS port 443
  // - at startup, then twice a day, will try to renew the certificates in the
  // background, following RenewBeforeEndDays property policy
  // - is typically associated to a main THttpAsyncServer for the HTTPS requests
  TAcmeLetsEncryptServer = class(TAcmeLetsEncrypt)
  protected
    fHttpServer: THttpServer; // a single threaded HTTP server is enough
    fHttpsServer: THttpServerGeneric;
    fNextCheckTix: Int64;
    fRedirectHttpsCount: integer; // how many active custom Redirect()
    // (un)assign the TNetTlsContext.OnAcceptServerName callback to this instance
    procedure SetCallbackForLoadFromKeyStoreFolder(Enabled: boolean); override;
    // main entry point for all HTTP requests on port 80
    function OnHeaderParsed(Request: THttpServerSocket): THttpServerSocketGetRequestResult;
    // will check twice a day if any certificate is to be renewed
    procedure OnAcceptIdle(Sender: TObject; Tix64: Int64);
  public
    /// initialize certificates management and HTTP server with Let's Encrypt
    // - if aDirectoryUrl is not '', will use the ACME_LETSENCRYPT_DEBUG_URL
    // "staging" environment - you should specify ACME_LETSENCRYPT_URL /
    // ACME_ZEROSSL_URL on production
    // - if aAlgo is '', will use 'x509-es256' as default
    // - a global aPrivateKeyPassword could be set to protect ##.key.pem files
    // - you can specify the associated main HTTPS server into aHttpsServer so
    // that our plain HTTP server will follow its configuration (e.g. logging)
    // - by default, the port 80 HTTP server will consume a single thread, but
    // you can set e.g. aHttpServerThreadCount = 2 on a production server
    // - aPort can be set to something else than 80, e.g. behind a reverse proxy
    // - will raise an exception if port 80 is not available for binding (e.g.
    // if the user is not root on Linux/POSIX)
    constructor Create(aLog: TSynLogClass; const aKeyStoreFolder: TFileName;
      const aDirectoryUrl, aAlgo: RawUtf8; const aPrivateKeyPassword: SpiUtf8;
      aHttpsServer: THttpServerGeneric = nil;
      aHttpServerThreadCount: integer = -1; const aPort: RawUtf8 = ''); reintroduce;
    /// finalize the certificates management and the associated HTTP server
    destructor Destroy; override;
    /// customize the https URI to redirect from any request on port 80
    // - Redirection should include the full URI, e.g. 'https://blog.synopse.info'
    function Redirect(const Domain, Redirection: RawUtf8): boolean;
  published
    /// the associated HTTPS server as supplied to Create()
    // - could be also set later one, if really needed
    property HttpsServer: THttpServerGeneric
      read fHttpsServer write fHttpsServer;
    /// the limited HTTP server launched by this class, running on port 80
    // - a single-threaded OnHeaderParsed() process scales well enough
    property HttpServer: THttpServer
      read fHttpServer;
  end;


{ **************** HTTP/HTTPS Fully Featured Multi-Host Web Server }


implementation



{ **************** JWS HTTP-client implementation }

{ TJwsHttpClient }

constructor TJwsHttpClient.Create(aLog: TSynLogClass; const aCert: ICryptCert);
begin
  inherited Create({aOnlyUseClientSocket=}false);
  fLog := aLog;
  fCert := aCert;
end;

function TJwsHttpClient.GetNonceAndBody: RawJson;
var
  err: RawUtf8;
begin
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, '% = % %',
       [fUri, fStatus, KBNoSpace(length(fBody))], self);
  // the server includes a Replay-Nonce header field in every response
  FindNameValue(fHeaders, 'REPLAY-NONCE: ', fNonce);
  // validate the response
  if not (fStatus in [HTTP_SUCCESS, HTTP_CREATED, HTTP_NOCONTENT]) then
  begin
    err := JsonDecode(pointer(fBody), 'detail', nil, {handlejsonobjarr=} false);
    if err = '' then
      err := StatusCodeToText(fStatus)^;
    EJwsHttp.RaiseUtf8('Error % [%] while querying %', [fStatus, err, fUri]);
  end;
  result := fBody;
end;

function TJwsHttpClient.Head(const aUrl: RawUtf8): RawJson;
begin
  Request(aUrl, 'HEAD');
  result := GetNonceAndBody;
end;

function TJwsHttpClient.Get(const aUrl: RawUtf8): RawJson;
begin
  Request(aUrl, 'GET');
  result := GetNonceAndBody;
end;

function TJwsHttpClient.Post(const aUrl: RawUtf8; const aJson: RawJson): RawJson;
var
  jwk, header: RawUtf8;
  thumb: TSha256Digest;
  header_enc, json_enc, body_enc: RawUtf8;
  sign: RawUtf8;
  data: RawUtf8;
begin
  if fKid <> '' then
  begin
    // we have the key identifier provided by the server
    header := FormatJson('{"alg":?,"kid":?,"nonce":?,"url":?}', [],
      [CAA_JWT[fCert.AsymAlgo], fKid, fNonce, aUrl])
  end
  else
  begin
    // no key identifier, need to provide JSON Web Key
    if not fCert.HasPrivateSecret then
      EJwsHttp.RaiseUtf8('%.Post: No private key', [self]);
    // compute JWK JSON object - e.g. '{"e":..,"kty":"RSA","n":..}' for RSA
    jwk := fCert.JwkCompute;
    // the thumbprint of a JWK is computed with no whitespace or line breaks
    // before or after any syntaxic elements and with the required members
    // ordered lexicographically, using SHA-256 hashing
    thumb := Sha256Digest(jwk);
    fJwkThumbprint := BinToBase64uri(thumb);
    header := FormatJson('{"alg":?,"jwk":%,"nonce":?,"url":?}',
      [jwk], [CAA_JWT[fCert.AsymAlgo], fNonce, aUrl]);
  end;
  header_enc := BinToBase64uri(header);
  json_enc := BinToBase64uri(aJson);
  body_enc := header_enc + '.' + json_enc;
  sign := GetSignatureSecurityRaw(fCert.AsymAlgo, fCert.Sign(body_enc));
  data := FormatJson('{"protected":?,"payload":?,"signature":?}',
    [], [header_enc, json_enc, sign]);
  Request(aUrl, 'POST', '', data, 'application/jose+json');
  result := GetNonceAndBody;
  if fKid = '' then
    FindNameValue(fHeaders, 'LOCATION: ', fKid);
end;

function TJwsHttpClient.Post(const aUrl: RawUtf8;
  const aNameValues: array of const): RawJson;
begin
  result := Post(aUrl, JsonEncode(aNameValues));
end;


{ **************** ACME client implementation }

function AcmeTextToStatus(p: PUtf8Char): TAcmeStatus;
begin
  if IdemPChar(p, 'VALID') or   // request has been successfully validated
     IdemPChar(p, 'READY') then // request is ready for the next step
    result := asValid
  else if IdemPChar(p, 'PENDING') or      // request is still being processed
          IdemPChar(p, 'PROCESSING') then
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

constructor TAcmeClient.Create(aLog: TSynLogClass; const aCert: ICryptCert;
  const aDirectoryUrl, aContact, aSubjects: RawUtf8);
var
  i: PtrInt;
begin
  fLog := aLog;
  inherited Create;
  fDirectoryUrl := aDirectoryUrl;
  fContact := aContact;
  if aSubjects = '' then
    EAcmeClient.RaiseUtf8('%.Create with aSubjects=nil', [self]);
  fSubjects := aSubjects;
  fSubject := CsvToRawUtf8DynArray(fSubjects);
  for i := 0 to high(fSubject) do
    if fSubject[i] = '' then // not allowed by FindPropName()
      EAcmeClient.RaiseUtf8('%.Create with a void entry in aSubjects=%',
        [self, aSubjects]);
  fHttpClient := TJwsHttpClient.Create(fLog, aCert);
end;

destructor TAcmeClient.Destroy;
begin
  FreeAndNil(fHttpClient);
  inherited Destroy;
end;

function TAcmeClient.MatchAny(const aServerName: RawUtf8): boolean;
begin
  // very fast case insensitive O(n) search
  result := FindPropName(pointer(fSubject), aServerName, length(fSubject)) >= 0;
end;

function TAcmeClient.GetChallenge(aUri: PUtf8Char; aUriLen: PtrInt;
  var Content: RawUtf8): boolean;
var
  i: integer;
  c: ^TAcmeChallenge;
begin
  c := pointer(fChallenges);
  if aUriLen > 0 then
    for i := 1 to length(fChallenges) do
      if CompareBuf(c^.Token, aUri, aUriLen) = 0 then
      begin
        if Assigned(fLog) then
          fLog.Add.Log(sllTrace, 'GetChallenge %', [c^.SubjectValue], self);
        result := true;
        Content := c^.Key;
        exit;
      end
      else
        inc(c);
  result := false;
end;

procedure TAcmeClient.ReadDirectory;
var
  resp: RawJson;
  v: array [0..2] of TValuePUtf8Char;
begin
  // In order to help clients configure themselves with the right URLs for
  // each ACME operation, ACME servers provide a directory object
  resp := fHttpClient.Get(fDirectoryUrl);
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'ReadDirectory %', [resp], self);
  JsonDecode(pointer(resp), [
    'newNonce',
    'newAccount',
    'newOrder'], @v, {handlejsonobjarr=} true);
  v[0].ToUtf8(fNewNonce);
  v[1].ToUtf8(fNewAccount);
  v[2].ToUtf8(fNewOrder);
  if (fNewNonce = '') or
     (fNewAccount = '') or
     (fNewOrder = '') then
    EAcmeClient.RaiseUtf8('Invalid directory %', [fDirectoryUrl]);
end;

procedure TAcmeClient.ComputeEab(out result: RawUtf8);
var
  protected: RawUtf8;
  payload: RawUtf8;
  signature: RawUtf8;
  signer: TSynSigner;
  hash: THash512Rec;
begin
  protected := BinToBase64uri(FormatJson('{"alg":?,"kid":?,"url":?',
    [], [JWT_TEXT[fEabAlgo], fEabKid, fNewAccount]));
  payload := BinToBase64uri(fHttpClient.fCert.JwkCompute);
  signer.Init(fEabAlgo, Base64uriToBin(fEabMacKey));
  signer.Update(protected);
  signer.Update('.');
  signer.Update(payload);
  signer.Final(@hash);
  signature := BinToBase64uri(@hash, signer.SignatureSize);
  result := FormatJson(
    ',"externalAccountBinding":{"protected":?,"payload":?,"signature":?}',
    [], [protected, payload, signature]);
end;

procedure TAcmeClient.CreateAccount;
var
  eab: RawUtf8;
  req: RawJson;
  resp: RawJson;
  status: RawUtf8;
begin
  // Optional External Account Binding member
  if (fEabKid <> '') and
     (fEabMacKey <> '') then
    ComputeEab(eab);
  // A client creates a new account with the server by sending a POST
  // request to the server's newAccount URL
  req := FormatJson('{"termsOfServiceAgreed":true,"contact":[?]%}',
      [eab], [fContact]);
  resp := fHttpClient.Post(fNewAccount, req);
  status := JsonDecode(pointer(resp), 'status', nil, {handlejsonobjarr=} true);
  if AcmeTextToStatus(pointer(status)) <> asValid then
    EAcmeClient.RaiseUtf8('% returned status % (expected "valid")',
      [fNewAccount, status]);
end;

function TAcmeClient.CreateOrder: TAcmeStatus;
var
  i, j, n: PtrInt;
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
  r1 := fHttpClient.Post(fNewOrder, ['identifiers', GetIdentifiersArr(fSubject)]);
  if not fHttpClient.Header('Location', fOrder) then
    EAcmeClient.RaiseU('Location not found for new order');
  JsonDecode(pointer(r1), [
    'status',
    'finalize',
    'authorizations'], @v1, {handleJsonObjectsOrArray=}true);
  result := AcmeTextToStatus(v1[0].Text);
  if result = asInvalid then
    EAcmeClient.RaiseUtf8('% returned "%" (expected "pending" or "ready")',
      [fNewOrder, v1[0].Text]);
  v1[1].ToUtf8(fFinalize);
  // When a client receives an order from the server in reply to a
  // newOrder request, it downloads the authorization resources by sending
  // requests to the indicated URLs
  n := 0;
  DynArrayLoadJson(auth, v1[2].Text, TypeInfo(TRawUtf8DynArray));
  SetLength(fChallenges, length(auth));
  ch := pointer(fChallenges);
  for i := 0 to length(auth) - 1 do
  begin
    r2 := fHttpClient.Post(auth[i], '');
    JsonDecode(pointer(r2), [
      'status',
      'identifier',
      'challenges'], @v1, {handlejsonobjarr=} true);
    ch^.Status := AcmeTextToStatus(v1[0].Text);
    JsonDecode(v1[1].Text, [
      'type',
      'value'], @v2, {handlejsonobjarr=} false);
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
          'token'], @v2, {handlejsonobjarr=} false);
        // support only HTTP validation by now
        if v2[0].Idem('HTTP-01') then
        begin
          v2[1].ToUtf8(ch^.Url);
          v2[2].ToUtf8(ch^.Token);
          // A key authorization is a string that
          // concatenates the token for the challenge with a key fingerprint
          // (using the SHA-256 digest), separated by a "." character
          ch^.Key := ch^.Token + '.' + fHttpClient.JwkThumbprint;
          inc(n);
          break; // we found and stored the "http-01" challenge
        end;
      end;
    end;
    inc(ch);
  end;
  fLog.Add.Log(sllDebug, 'CreateOrder chal=%/%', [n, length(fChallenges)], self);
end;

function TAcmeClient.RequestAuth(const aJson: RawJson): integer;
var
  i: integer;
  resp: RawJson;
  status: RawUtf8;
  c: ^TAcmeChallenge;
begin
  result := 0; // return how many pending
  // The client indicates to the server that it is ready for the challenge
  // validation by sending an empty body aJson = '{}'.
  // If aJson = '' then client requests validation state
  c := pointer(fChallenges);
  for i := 1 to length(fChallenges) do
  begin
    if c^.Status = asPending then
    begin
      resp := fHttpClient.Post(c^.Url, aJson);
      status := JsonDecode(pointer(resp), 'status', nil, {handleobjarr=} false);
      c^.Status := AcmeTextToStatus(pointer(status));
      inc(result, ord(c^.Status = asPending));
    end;
    inc(c);
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
  i, n: PtrInt;
begin
  fLog.Add.Log(sllTrace, 'StartDomainRegistration %', [fSubjects], self);
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
        with fChallenges[i] do
          if Key <> '' then
            fOnChallenges(self, fSubjects, Key, Token);
    // Queue challenge testing by sending {} to initiate the server process
    n := RequestAuth('{}');
    fLog.Add.Log(sllTrace, 'StartDomainRegistration pending=%', [n], self);
  end;
end;

function TAcmeClient.CheckChallengesStatus: TAcmeStatus;
var
  i, pending, valid: integer;
  c: ^TAcmeChallenge;
begin
  // Before sending a POST request to the server, an ACME client needs to
  // have a fresh anti-replay nonce to put in the "nonce" header of the JWS
  fHttpClient.Head(fNewNonce);
  // Check if challenge for a domain is completed
  pending := RequestAuth(''); // {} to initiate, '' to check status
  // Compute result:
  // One invalid -> invalid
  // All valid -> valid
  // else pending
  result := asPending;
  valid := 0;
  c := pointer(fChallenges);
  for i := 1 to length(fChallenges) do
  begin
    case c^.Status of
      asInvalid:
        result := asInvalid;
      asValid:
        inc(valid);
    end;
    inc(c);
  end;
  if (result = asPending) and
     (valid = length(fChallenges)) then
    result := asValid;
  fLog.Add.Log(sllDebug, 'CheckChallengesStatus=% pending=% valid=% count=%',
    [ToText(result)^, pending, valid, length(fChallenges)], self);
end;

function TAcmeClient.FinalizeDomainRegistration(out aPrivateKey: RawUtf8;
  const aPrivateKeyPassword: SpiUtf8): TAcmeStatus;
var
  csr: RawByteString;
  pk, resp: RawUtf8;
  status: RawUtf8;
begin
  try
    // Generate a new private key and its PKCS#10 Certificate Signing Request
    csr := PemToDer(fHttpClient.fCert.CertAlgo.CreateSelfSignedCsr(
      fSubjects, aPrivateKeyPassword, pk));
    // Before sending a POST request to the server, an ACME client needs to
    // have a fresh anti-replay nonce to put in the "nonce" header of the JWS
    fHttpClient.Head(fNewNonce);
    // Once the validation process is complete and the server is satisfied
    // that the client has met its requirements, the client finalizes the
    // order by submitting the Certificate Signing Request (CSR)
    resp := fHttpClient.Post(fFinalize, [
      'csr', BinToBase64uri(csr)]);
    status := JsonDecode(pointer(resp), 'status', nil, {handleobjarr=} false);
    result := AcmeTextToStatus(pointer(status));
    if result in [asValid, asPending] then
      // only keep the generated private key on success
      aPrivateKey := pk
  finally
    FillZero(pk);
  end;
end;

function TAcmeClient.CompleteDomainRegistration(out aCert: RawUtf8): TAcmeStatus;
var
  resp: RawUtf8;
  v: array [0..1] of TValuePUtf8Char;
begin
  try
    // Before sending a POST request to the server, an ACME client needs to
    // have a fresh anti-replay nonce to put in the "nonce" header of the JWS
    fHttpClient.Head(fNewNonce);
    // If a request to finalize an order is successful, the server will
    // return a 200 (OK) with an updated order object. The status of the
    // order will indicate what action the client should take
    resp := fHttpClient.Post(fOrder, '');
    JsonDecode(pointer(resp), [
      'status',
      'certificate'], @v, {handlejsonobjarr=} true);
    result := AcmeTextToStatus(v[0].Text);
    if result = asValid then
    begin
      // The server has issued the certificate and provisioned its
      // URL to the "certificate" field of the order.
      // Download the certificate
      aCert := fHttpClient.Post(v[1].ToUtf8, '');
      if IsPem(aCert) then
        fLog.Add.Log(sllDebug, 'CompleteDomainRegistration cert=%',
          [aCert], self)
      else
        result := asInvalid;
    end;
  finally
    FillZero(resp);
  end;
end;

procedure TAcmeClient.ClearDomainRegistration;
var
  i: PtrInt;
begin
  if Assigned(fOnChallenges) then
    // call with key = '' to notify final state
    for i := 0 to length(fChallenges) - 1 do
      with fChallenges[i] do
        if Key <> '' then
          fOnChallenges(nil, fSubjects, {key=}'', Token);
  fChallenges := nil;
  fOrder := '';
  fFinalize := '';
end;

function TAcmeClient.RegisterAndWait(const OnChallenge: TOnAcmeChallenge;
  const OutSignedCert, OutPrivateKey: TFileName;
  const aPrivateKeyPassword: SpiUtf8; WaitForSec: cardinal;
  Terminated: PBoolean): TAcmeStatus;
var
  endtix: cardinal;
  cert, pk: RawUtf8;
  log: ISynLog;
begin
  fLog.EnterLocal(log, self, 'RegisterAndWait');
  fOnChallenges := OnChallenge;
  try
    StartDomainRegistration;
    // First loop: wait for domain validation
    endtix := GetTickSec + WaitForSec; // e.g. RenewWaitForSeconds = 30 secs
    repeat
      result := asInvalid;
      if Terminated = nil then
        SleepHiRes(500)
      else if SleepHiRes(500, Terminated^) then
        exit;
      result := CheckChallengesStatus; // checking twice per second seems fair
      if result <> asPending then
        break;
    until GetTickSec > endtix;
    if (result <> asValid) or
       (OutSignedCert = '') or
       (OutPrivateKey = '') then
      exit;
    // Send CSR
    result := FinalizeDomainRegistration(pk, aPrivateKeyPassword);
    if Assigned(log) then
      log.Log(sllDebug, 'FinalizeDomainRegistration=%', [ToText(result)^], self);
    if result = asInvalid then
      exit;
    // Second loop: wait for certificate
    repeat
      result := asInvalid;
      if Terminated = nil then
        SleepHiRes(500)
      else if SleepHiRes(500, Terminated^) then
        exit;
      result := CompleteDomainRegistration(cert); // retry twice per second
      if result <> asPending then
        break;
    until GetTickSec > endtix;
    if Assigned(log) then
      log.Log(sllDebug, 'CompleteDomainRegistration=%', [ToText(result)^], self);
    if result = asValid then
      try
        FileFromString(cert, OutSignedCert);
        FileFromString(pk,   OutPrivateKey);
      finally
        FillZero(cert);
        FillZero(pk);
      end;
  finally
    ClearDomainRegistration;
  end;
end;

function TAcmeClient.RegisterAndWaitFolder(
  const ChallengeWwwFolder, OutSignedCert, OutPrivateKey: TFileName;
  const aPrivateKeyPassword: SpiUtf8; WaitForSec: integer): TAcmeStatus;
begin
  if fChallengeWwwFolder <> '' then
    EAcmeClient.RaiseUtf8(
      '%.RegisterAndWaitFolder: already called as %', [self, fChallengeWwwFolder]);
  if not DirectoryExists(ChallengeWwwFolder) then
    EAcmeClient.RaiseUtf8(
      '%.RegisterAndWaitFolder: unknown %', [self, ChallengeWwwFolder]);
  fChallengeWwwFolder := EnsureDirectoryExists(
    [ChallengeWwwFolder, '.well-known', 'acme-challenge'], EAcmeClient);
  try
    result := RegisterAndWait(OnChallengeWwwFolder,
      OutSignedCert, OutPrivateKey, aPrivateKeyPassword, WaitForSec, nil);
  finally
    fChallengeWwwFolder := '';
  end;
end;


{ ********** Let's Encrypt - ZeroSSL TLS / HTTPS Encryption Certificates Support }

{ TAcmeLetsEncryptClient }

constructor TAcmeLetsEncryptClient.Create(
  aOwner: TAcmeLetsEncrypt; const aDomainFile: TFileName);
var
  local: ICryptCert;
  dom: TDocVariantData;
  json, cont: RawUtf8;
  sub: TRawUtf8DynArray;
  eab: PDocVariantData;
  algo: RawUtf8;
begin
  fOwner := aOwner;
  fDomainJson    := aDomainFile + '.json';
  fReferenceCert := aDomainFile + '.acme.pem';
  fSignedCert    := aDomainFile + '.crt.pem';
  fPrivKey       := aDomainFile + '.key.pem';
  // load the main domain.json global parameters
  json := StringFromFile(fDomainJson);
  fOwner.fLog.Add.Log(sllTrace, 'Create(%) %', [aDomainFile, json], self);
  dom.InitJsonInPlace(pointer(json), []);
  dom.A['subjects'].ToRawUtf8DynArray(sub);
  if sub = nil then
    EAcmeLetsEncrypt.RaiseUtf8(
      '%.Create: void "subjects":[] in %', [self, fDomainJson]);
  cont := dom.U['contact'];
  if cont = '' then
    EAcmeLetsEncrypt.RaiseUtf8(
      '%.Create: missing "contact":"mailto:xx@yy.zz" in %',
      [self, fDomainJson]);
  if dom.GetAsDocVariant('eab', eab) then
  begin
    algo := eab^.U['algo'];
    if not TextToSignAlgo(algo, fEabAlgo) then
      EAcmeLetsEncrypt.RaiseUtf8(
       '%.Create: unsupported "eab.algo":"%"', [self, algo]);
    fEabKid    := eab^.U['kid'];
    fEabMacKey := eab^.U['mac_key'];
  end;
  // load acme.pem local key (used to sign the JWK queries, not for TLS itself)
  local := Cert(fOwner.fAlgo);
  if local = nil then
    EAcmeLetsEncrypt.RaiseUtf8('%.Create: unsupported %', [self, fOwner.fAlgo]);
  if not FileExists(fSignedCert) or
     not FileExists(fPrivKey) or
     not local.LoadFromFile(fReferenceCert, cccCertWithPrivateKey) then
  begin
    fOwner.fLog.Add.Log(sllDebug, 'Create(%): invalid % -> recreate all',
      [aDomainFile, fReferenceCert], self);
    DeleteFile(fReferenceCert);
    DeleteFile(fSignedCert);
    DeleteFile(fPrivKey);
    local.Generate([cuDigitalSignature], sub[0], nil, 3650);
    local.SaveToFile(fReferenceCert, cccCertWithPrivateKey); // no password needed
  end;
  // initialize the process
  inherited Create(fOwner.fLog, local, fOwner.fDirectoryUrl,
    cont, RawUtf8ArrayToCsv(sub));
end;

destructor TAcmeLetsEncryptClient.Destroy;
begin
  ReplaceContext({new=}nil);
  inherited Destroy;
end;

function TAcmeLetsEncryptClient.NewServerContext(const Cert, Key: TFileName): PSSL_CTX;
var
  c, k: RawUtF8;
begin
  StringToUtf8(Cert, c);
  StringToUtf8(Key, k);
  result := SSL_CTX_new_server(c);
  try
    result.SetCertificateFiles(c, k, fOwner.fPrivateKeyPassword);
  except
    result.Free; // release this invalid context on any EOpenSslNetTls
    result := nil;
  end;
  fLog.Add.Log(sllTrace, 'NewServerContext=% for % and %',
    [BOOL_STR[result <> nil], c, k], self);
end;

function TAcmeLetsEncryptClient.GetServerContext: PSSL_CTX;
var
  sc, pk: TUnixTime;
  tix32: cardinal;
begin
  // client made fSafe.Lock and will eventually make fSafe.UnLock
  sc := 0;
  pk := 0;
  tix32 := GetTickSec;
  // result <> nil will be attached to a TLS connection via SSL_set_SSL_CTX()
  result := fSslCtx;
  // immediate return from cache on a very active server or during renewal
  if fRenewing or
     (fServerContextTix32 = tix32) then
    exit;
  fServerContextTix32 := tix32; // accessing files once per second is enough
  if result <> nil then // we already have a PSSL_CTX: check it is still valid
  begin
    sc := FileAgeToUnixTimeUtc(fSignedCert); // ####.crt.pem
    pk := FileAgeToUnixTimeUtc(fPrivKey);    // ####.key.pem
    // unmodified key files will also return the cached PSSL_CTX (most often)
    if (fSignedCertTime = sc) and
       (fPrivKeyTime = pk) then
      exit;
    // missing key files would still return the cached PSSL_CTX
    if (sc <= 0) or
       (pk <= 0) then
    begin
      fLog.Add.Log(sllTrace, 'GetServerContext(%): unexpected %=% %=%',
        [fSubjects, fSignedCert, sc, fPrivKey, pk], self);
      exit;
    end;
  end;
  // if we reached here, we have deprecated (or no) PSSL_CTX -> create one
  result := NewServerContext(fSignedCert, fPrivKey);
  if result <> nil then
  begin
    fSslCtx := result; // owned and cached - previous instance is leaked
    if sc = 0 then
      sc := FileAgeToUnixTimeUtc(fSignedCert); // ####.crt.pem
    if pk = 0 then
      pk := FileAgeToUnixTimeUtc(fPrivKey);   //  ####.key.pem
    fSignedCertTime := sc;
    fPrivKeyTime := pk;
  end
  else
    result := fSslCtx; // silently fallback to previous (maybe nil) PSSL_CTX
end;

procedure TAcmeLetsEncryptClient.ReplaceContext(New: PSSL_CTX);
var
  old: PSSL_CTX;
begin
  fSafe.Lock;
  try
    old := fSslCtx;
    fSslCtx := New;
  finally
    fSafe.UnLock;
  end;
  if old <> nil then
    old.Free;
end;


{ TAcmeLetsEncrypt }

constructor TAcmeLetsEncrypt.Create(aLog: TSynLogClass;
  const aKeyStoreFolder: TFileName; const aDirectoryUrl, aAlgo: RawUtf8;
  const aPrivateKeyPassword: SpiUtf8);
begin
  inherited Create;
  fLog := aLog;
  if aAlgo = '' then
    // Let’s Encrypt accepts RSA keys that are 2048, 3072, or 4096 bits in length
    // and P-256 or P-384 ECDSA keys - we favor the later for their shortness
    fAlgo := 'x509-es256'
  else
    fAlgo := aAlgo;
  if aDirectoryUrl = '' then
    fDirectoryUrl := ACME_LETSENCRYPT_DEBUG_URL
  else
    fDirectoryUrl := aDirectoryUrl;
  fKeyStoreFolder := EnsureDirectoryExists(aKeyStoreFolder, EAcmeLetsEncrypt);
  fPrivateKeyPassword := aPrivateKeyPassword;
  fRenewWaitForSeconds := 30;
  fRenewBeforeEndDays := 30;
end;

procedure TAcmeLetsEncrypt.WaitUntilNotRenewing(const Context: ShortString);
var
  endtix: cardinal;
begin
  endtix := GetTickSec + 30; // never wait forever
  with fLog.Enter('%: WaitUntilNotRenewing', [Context], self) do
    repeat
      SleepHiRes(50); // wait for background task to abort
    until (GetTickSec > endtix) or
          not fClientsRenewing;
end;

destructor TAcmeLetsEncrypt.Destroy;
begin
  fShutdown := true; // set flag to abort any background task
  if fClientsRenewing then
    WaitUntilNotRenewing('Destroy');
  FillZero(fPrivateKeyPassword);
  ObjArrayClear(fClient);
  inherited Destroy;
end;

procedure TAcmeLetsEncrypt.LoadFromKeyStoreFolder;
var
  f: TSearchRec;
  fn: TFileName;
  log: ISynLog;
begin
  fLog.EnterLocal(log, self, 'LoadFromKeyStoreFolder');
  if fShutdown then
    exit;
  if fClientsRenewing then
    WaitUntilNotRenewing('LoadFromKeyStoreFolder'); // paranoid
  if fShutdown then
    exit;
  SetCallbackForLoadFromKeyStoreFolder({enabled=}false);
  fSafe.WriteLock;
  try
    ObjArrayClear(fClient);
    if FindFirst(fKeyStoreFolder + '*.json', faAnyFile, f) = 0 then
    begin
      repeat
         if SearchRecValidFile(f, {includehidden=}true) then
           try
             fn := fKeyStoreFolder + GetFileNameWithoutExt(f.Name);
             ObjArrayAdd(fClient, TAcmeLetsEncryptClient.Create(self, fn));
             if Assigned(log) then
               log.Log(sllDebug, 'LoadFromKeyStoreFolder: added %', [fn], self);
           except
             on E: Exception do
             begin
               RenameFile(fn, fn + '.invalid'); // don't try it again
               if Assigned(log) then
                 log.Log(sllWarning, 'LoadFromKeyStoreFolder: renamed as ' +
                   '%.invalid after %', [fn, PClass(E)^], self);
             end;
           end;
      until FindNext(f) <> 0;
      FindClose(f);
    end;
  finally
    fSafe.WriteUnLock;
  end;
  if fClient <> nil then
    SetCallbackForLoadFromKeyStoreFolder({enabled=}true);
  if Assigned(log) then
    log.Log(sllDebug, 'LoadFromKeyStoreFolder: added %',
      [Plural('domain', length(fClient))], self);
end;

procedure TAcmeLetsEncrypt.CheckCertificates(Sender: TObject);
var
  i: PtrInt;
  c: TAcmeLetsEncryptClient;
  cc: ICryptCert;
  ctx: PSSL_CTX;
  sub: RawUtf8;
  needed: TRawUtf8DynArray; // no long standing fSafe.Lock
  expired: TDateTime;
  res: TAcmeStatus;
  tmpCert, tmpKey: TFileName;
  log: ISynLog;
begin
  // this method is run from a transient TLoggedWorkThread
  fLog.EnterLocal(log, self, 'CheckCertificates');
  if (self = nil) or
     (fClient = nil) or
     fShutdown or
     (fRenewBeforeEndDays <= 0) or
     fClientsRenewing then
    exit;
  // quickly retrieve all certificates which need generation/renewal
  cc := Cert(fAlgo);
  if cc = nil then
    exit;
  expired := NowUtc + fRenewBeforeEndDays;
  fSafe.ReadLock;
  try
    for i := 0 to length(fClient) - 1 do
    begin
      c := fClient[i];
      if c.Subject = nil then
        continue; // paranoid
      sub := c.Subject[0];
      if GetClient(sub) = c then // avoid duplicated names confusion
        if not cc.LoadFromFile(c.fSignedCert) or // needs generation
           (cc.GetNotAfter < expired) then       // needs renewal
          AddRawUtf8(needed, sub);
    end;
  finally
    fSafe.ReadUnLock;
  end;
  if Assigned(log) then
    log.Log(sllDebug, 'CheckCertificates: renew %',
      [Plural('certificate', length(needed))], self);
  if needed = nil then
    exit;
  // protect the fClient[] list and the c instance during the slow process below
  fSafe.WriteLock;
  try
    if fClientsRenewing then
      exit; // (unlikely) race condition
    fClientsRenewing := true;
  finally
    fSafe.WriteUnLock;
  end;
  // renew all needed certifiates
  try
    for i := 0 to length(needed) - 1 do
    begin
      if fShutdown then
        exit;
      c := GetClientLocked(needed[i]); // lookup by subject
      if c = nil then
        continue; // paranoid
      if c.fRenewing then
      begin
        c.Safe.UnLock;
        continue; // (unlikely) race condition
      end;
      c.fRenewing := true;
      c.Safe.UnLock; // allow e.g. HttpServerChallenge() background lookup
      try
        tmpCert := c.fSignedCert + '.tmp'; // apply challenge on transient files
        tmpKey  := c.fPrivKey    + '.tmp';
        res := c.RegisterAndWait(nil, tmpCert, tmpKey, fPrivateKeyPassword,
                 fRenewWaitForSeconds, @fShutdown);
        if (res = asValid) and
           not fShutdown then
        begin
          // validate these new certificate and private key
          res := asInvalid;
          ctx := c.NewServerContext(tmpCert, tmpKey);
          if ctx <> nil then
            // we can safely replace the main files
            DeleteFile(c.fSignedCert);
            DeleteFile(c.fPrivKey);
            if RenameFile(tmpCert, c.fSignedCert) and
               RenameFile(tmpKey, c.fPrivKey) then
            begin
              // GetServerContext should now use this new context
              c.ReplaceContext(ctx);
              res := asValid;
            end
            else
            begin
              if Assigned(log) then
                log.Log(sllLastError,
                  'CheckCertificates(%): impossible to replace % and %',
                  [needed[i], c.fSignedCert, c.fPrivKey], self);
              ctx.Free;
            end;
        end;
      except
        res := asInvalid;
      end;
      c.fRenewing := false;
      if Assigned(log) then
        log.Log(sllTrace, 'CheckCertificates: % = %',
          [needed[i], ToText(res)^], self);
    end;
  finally
    fClientsRenewing := false;
  end;
end;

procedure TAcmeLetsEncrypt.CheckCertificatesBackground;
begin
  TLoggedWorkThread.Create(fLog, 'CheckCertificates', self, CheckCertificates);
end;

function TAcmeLetsEncrypt.GetClient(
  const ServerName: RawUtf8): TAcmeLetsEncryptClient;
var
  i: integer;
  p: ^TAcmeLetsEncryptClient;
begin
  p := pointer(fClient);
  for i := 1 to length(fClient) do
  begin
    result := p^;
    if result.MatchAny(ServerName) then
      exit;
    inc(p);
  end;
  result := nil; // not found
end;

function TAcmeLetsEncrypt.GetClientLocked(
  const ServerName: RawUtf8): TAcmeLetsEncryptClient;
begin
  fSafe.ReadLock;
  try
    result := GetClient(ServerName); // case-insensitive search
    if result <> nil then
      result.Safe.Lock; // caller should eventually call result.Safe.UnLock
  finally
    fSafe.ReadUnLock;
  end;
end;

function TAcmeLetsEncrypt.OnNetTlsAcceptServerName(Context: PNetTlsContext;
  TLS: pointer; ServerName: PUtf8Char): pointer;
var
  client: TAcmeLetsEncryptClient;
  name: RawUtf8;
begin
  result := nil;
  if (fClient = nil) or
     (ServerName = nil) then
    exit;
  FastSetString(name, ServerName, StrLen(ServerName));
  client := GetClientLocked(name); // case-insensitive search
  if client <> nil then
    try
      result := client.GetServerContext; // returns the cached PSSL_CTX
    finally
      client.Safe.UnLock;
    end
end;

const
  _ACME_CHALLENGE_PATH: PUtf8Char = ACME_CHALLENGE_PATH;

function TAcmeLetsEncrypt.HttpServerChallenge(const domain, uri: RawUtf8;
  var content: RawUtf8): boolean;
var
  client: TAcmeLetsEncryptClient;
  P: PUtf8Char;
  len: PtrInt;
begin
  result := false;
  if fShutdown or
     (fClient = nil) or
     not fClientsRenewing then
    exit; // no challenge currently happening
  // quickly parse the /.well-known/acme-challenge/<token> case-insensitive URI
  P := pointer(uri);
  len := length(uri) - ACME_CHALLENGE_PATH_LEN;
  if (len <= 0) or
     not CompareMem(P, _ACME_CHALLENGE_PATH, ACME_CHALLENGE_PATH_LEN) then
    exit;
  // recognize the <token> and return its matching TAcmeChallenge.Key
  client := GetClientLocked(domain);
  if client <> nil then
    try
      result := client.GetChallenge(P + ACME_CHALLENGE_PATH_LEN, len, content);
    finally
      client.Safe.UnLock;
    end;
end;


{ *********** HTTP-01 Let's Encrypt - ZeroSSL Challenges HTTP Server on port 80 }

{ TAcmeLetsEncryptServer }

constructor TAcmeLetsEncryptServer.Create(aLog: TSynLogClass;
  const aKeyStoreFolder: TFileName; const aDirectoryUrl, aAlgo: RawUtf8;
  const aPrivateKeyPassword: SpiUtf8; aHttpsServer: THttpServerGeneric;
  aHttpServerThreadCount: integer; const aPort: RawUtf8);
var
  opt: THttpServerOptions;
  i: PtrInt;
  p, hp: RawUtf8;
  log: ISynLog;
begin
  // prepare the needed information for our HTTP server (on port 80 by default)
  p := aPort;
  if p = '' then
    p := '80';
  opt := [hsoBan40xIP, hsoNoXPoweredHeader];
  if aHttpsServer <> nil then
  begin
    // retrieve some information from the main HTTPS server
    fHttpsServer := aHttpsServer;
    // bind to the same interface/IP
    if fHttpsServer.InheritsFrom(THttpServerSocketGeneric) then
    begin
      hp := THttpServerSocketGeneric(fHttpsServer).SockPort;
      i := PosExChar(':', hp);
      if (i <> 0) and
         (PosExChar(':', p) = 0) then
        p := copy(hp, 1, i) + p; // e.g. 'IP:443' into 'IP:80'
    end;
    // enable logging also into an "access80.log" file
    if hsoEnableLogging in fHttpsServer.Options then
      include(opt, hsoEnableLogging);
  end;
  // start a basic HTTP server on port 80
  aLog.EnterLocal(log, 'Create: start THttpServer on %', [p], self);
  fHttpServer := THttpServer.Create(p, nil, nil, 'Acme Server',
    aHttpServerThreadCount, 30000, opt, aLog);
  // retrieve some parameters from the main HTTPS server
  if fHttpsServer <> nil then
  begin
    fHttpServer.ServerName := fHttpsServer.ServerName;
    if hsoEnableLogging in opt then
    begin
      fHttpServer.Logger.CopyParams(fHttpsServer.Logger);
      fHttpServer.Logger.Settings.DestMainFile := 'access80.log';
    end;
  end;
  // setup the ACME configuration
  inherited Create(aLog, aKeyStoreFolder, aDirectoryUrl, aAlgo,
    aPrivateKeyPassword);
  // handle requests on port 80 as HTTP/1.0 redirection or ACME challenges
  fHttpServer.OnHeaderParsed := OnHeaderParsed;
  // ban an IP for 4 seconds on any DoS attack
  fHttpServer.HeaderRetrieveAbortDelay := 200; // grTimeOut after 200ms headers
  // automated certificate renewal
  fHttpServer.OnAcceptIdle := OnAcceptIdle; // try now, then every half a day
  // log the current state
  if Assigned(log) then
    log.Log(sllTrace, self);
end;

destructor TAcmeLetsEncryptServer.Destroy;
begin
  fShutdown := true; // abort any background task ASAP
  FreeAndNil(fHttpServer);
  inherited Destroy;
end;

function TAcmeLetsEncryptServer.Redirect(
  const Domain, Redirection: RawUtf8): boolean;
var
  client: TAcmeLetsEncryptClient;
begin
  result := false;
  client := GetClientLocked(Domain);
  if client <> nil then
    try
      if (Redirection <> '') <> (client.fRedirectHttps <> '') then
        if Redirection = '' then
          LockedDec32(@fRedirectHttpsCount)
        else
          LockedInc32(@fRedirectHttpsCount);
      client.fRedirectHttps := Redirection;
      result := true;
    finally
      client.Safe.UnLock;
    end;
end;

function TAcmeLetsEncryptServer.OnHeaderParsed(
  Request: THttpServerSocket): THttpServerSocketGetRequestResult;
var
  client: TAcmeLetsEncryptClient;
  body, redirect: RawUtf8;
begin
  // (very) quick process of HTTP requests on port 80 into HTTP/1.0 responses
  // - a single thread is able to serve and scale a lot of incoming connections
  if fShutdown then
    Request.SockSend('HTTP/1.0 503 Service Unavailable')
  else if (Request.Http.CommandUri <> '') and
          (PCardinal(Request.Http.CommandUri)^ =
             ord('/') + ord('.') shl 8 + ord('w') shl 16 + ord('e') shl 24) then
    // handle Let's Encrypt - ZeroSSL challenges on /.well-known/* URI
    if fClientsRenewing and
       HttpServerChallenge(Request.Http.Host, Request.Http.CommandUri, body) then
      // return the HTTP-01 challenge content
      Request.SockSend('HTTP/1.0 200 OK'#13#10 +
                       BINARY_CONTENT_TYPE_HEADER)
    else
      // no redirection for inactive /.well-known/acme-challenge/<Token> URIs
      Request.SockSend('HTTP/1.0 404 Not Found')
  else
  begin
    // redirect GET or POST on port 80 to port 443 using 301 or 308 response
    if HttpMethodWithNoBody(Request.Http.CommandMethod) then
      Request.SockSend('HTTP/1.0 301 Moved Permanently')
    else
      Request.SockSend('HTTP/1.0 308 Permanent Redirect');
    if fRedirectHttpsCount <> 0 then // same active custom Redirect()
    begin
      client := GetClientLocked(Request.Http.Host);
      if client <> nil then
      begin
        redirect := client.fRedirectHttps; // <> '' if customized
        client.Safe.UnLock;
      end;
    end;
    if redirect <> '' then
      // redirect to the customized URI for this host
      Request.SockSendLine([
        'Location: ', redirect])
    else
      // redirect to the same URI but on HTTPS host
      Request.SockSendLine([
        'Location: https://', Request.Http.Host, Request.Http.CommandUri]);
    // 301 and 308 responses expect no body
  end;
  // finalize the headers and send the response body
  Request.SockSend([
    'Server: ', fHttpServer.ServerName, #13#10 +
    'Content-Length: ', length(body), #13#10 +
    'Connection: Close'#13#10]);
  if (body <> '') and
     HttpMethodWithNoBody(Request.Http.CommandMethod) then
    body := ''; // if the ACME server tries a HEAD (unlikely)
  Request.SockSendFlush(body);
  // no regular OnRequest() event: we have sent the response
  result := grIntercepted;
  // grIntercepted won't trigger any IP ban, just close the connection
  // HeaderRetrieveAbortDelay=200 will trigger grTimeOut to ban the IP
end;

procedure TAcmeLetsEncryptServer.OnAcceptIdle(Sender: TObject; Tix64: Int64);
begin
  if fClientsRenewing or
     fShutdown or
     (fClient = nil) or
     (fRenewBeforeEndDays <= 0) or
     (fHttpsServer = nil) or
     (Tix64 < fNextCheckTix) then
    exit;
  fNextCheckTix := Tix64 + (MilliSecsPerDay shr 1); // retry every half a day
  CheckCertificatesBackground; // launch a dedicated background thread
end;

procedure TAcmeLetsEncryptServer.SetCallbackForLoadFromKeyStoreFolder(Enabled: boolean);
begin
  if fHttpsServer <> nil then
    if Enabled then
      fHttpsServer.SetTlsServerNameCallback(OnNetTlsAcceptServerName)
    else
      fHttpsServer.SetTlsServerNameCallback(nil);
end;


{ **************** HTTP/HTTPS Fully Featured Multi-Host Web Server }



initialization
  EnableOnNetTlsAcceptServerName := true; // this global variable should be set

{$else}

implementation

{$endif USE_OPENSSL}


end.
