/// Framework Core JSON Web Tokens (JWT) Support
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.jwt;

{
  *****************************************************************************

   JSON Web Tokens (JWT) Implementation - see RFC 7797
    - Abstract JWT Parsing and Computation
    - JWT Implementation of HS* and S3* Symmetric Algorithms
    - JWT Implementation of ES256 Asymmetric Algorithm

   Uses optimized mormot.core.crypto.pas and mormot.core.ecc for its process.

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  classes,
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.data,
  mormot.core.datetime,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.crypto,
  mormot.core.secure,
  mormot.core.ecc256r1,
  mormot.core.ecc;


{ **************** Abstract JWT Parsing and Computation }

type
  /// JWT Registered Claims, as defined in RFC 7519
  // - known registered claims have a specific name and behavior, and will be
  // handled automatically by TJWTAbstract
  // - corresponding field names are iss,sub,aud,exp,nbf,iat,jti - as defined
  // in JWT_CLAIMS_TEXT constant
  // - jrcIssuer identifies the server which originated the token, e.g.
  // "iss":"https://example.auth0.com/" when the token comes from Auth0 servers
  // - jrcSubject is the application-specific extent which is protected by this
  // JWT, e.g. an User or Resource ID, e.g. "sub":"auth0|57fe9f1bad961aa242870e"
  // - jrcAudience claims that the token is valid only for one or several
  // resource servers (may be a JSON string or a JSON array of strings), e.g.
  // "aud":["https://myshineyfileserver.sometld"] - TJWTAbstract will check
  // that the supplied "aud" field does match an expected list of identifiers
  // - jrcExpirationTime contains the Unix timestamp in seconds after which
  // the token must not be granted access, e.g. "exp":1477474667
  // - jrcNotBefore contains the Unix timestamp in seconds before which the
  // token must not be granted access, e.g. "nbf":147745438
  // - jrcIssuedAt contains the Unix timestamp in seconds when the token was
  // generated, e.g. "iat":1477438667
  // - jrcJwtID provides a unique identifier for the JWT, to prevent any replay;
  // TJWTAbstract.Compute will set an obfuscated TSynUniqueIdentifierGenerator
  // hexadecimal value
  TJWTClaim = (
    jrcIssuer,
    jrcSubject,
    jrcAudience,
    jrcExpirationTime,
    jrcNotBefore,
    jrcIssuedAt,
    jrcJwtID);

  /// set of JWT Registered Claims, as in TJWTAbstract.Claims
  TJWTClaims = set of TJWTClaim;

  /// Exception raised when running JSON Web Tokens
  EJWTException = class(ESynException);

  /// TJWTContent.result codes after TJWTAbstract.Verify method call
  TJWTResult = (
    jwtValid,
    jwtNoToken,
    jwtWrongFormat,
    jwtInvalidAlgorithm,
    jwtInvalidPayload,
    jwtUnexpectedClaim,
    jwtMissingClaim,
    jwtUnknownAudience,
    jwtExpired,
    jwtNotBeforeFailed,
    jwtInvalidIssuedAt,
    jwtInvalidID,
    jwtInvalidSignature);

  //// set of TJWTContent.result codes
  TJWTResults = set of TJWTResult;

  /// JWT decoded content, as processed by TJWTAbstract
  // - optionally cached in memory
  TJWTContent = record
    /// store latest Verify() result
    result: TJWTResult;
    /// set of known/registered claims, as stored in the JWT payload
    claims: TJWTClaims;
    /// match TJWTAbstract.Audience[] indexes for reg[jrcAudience]
    audience: set of 0..15;
    /// known/registered claims UTF-8 values, as stored in the JWT payload
    // - e.g. reg[jrcSubject]='1234567890' and reg[jrcIssuer]='' for
    // $ {"sub": "1234567890","name": "John Doe","admin": true}
    reg: array[TJWTClaim] of RawUTF8;
    /// custom/unregistered claim values, as stored in the JWT payload
    // - registered claims will be available from reg[], not in this field
    // - e.g. data.U['name']='John Doe' and data.B['admin']=true for
    // $ {"sub": "1234567890","name": "John Doe","admin": true}
    // but data.U['sub'] if not defined, and reg[jrcSubject]='1234567890'
    data: TDocVariantData;
  end;
  /// pointer to a JWT decoded content, as processed by TJWTAbstract

  PJWTContent = ^TJWTContent;
  /// used to store a list of JWT decoded content
  // - as used e.g. by TJWTAbstract cache

  TJWTContentDynArray = array of TJWTContent;

  /// available options for TJWTAbstract process
  TJWTOption = (
    joHeaderParse,
    joAllowUnexpectedClaims,
    joAllowUnexpectedAudience,
    joNoJwtIDGenerate,
    joNoJwtIDCheck,
    joDoubleInData);

  /// store options for TJWTAbstract process
  TJWTOptions = set of TJWTOption;

  /// abstract parent class for implementing JSON Web Tokens
  // - to represent claims securely between two parties, as defined in industry
  // standard @http://tools.ietf.org/html/rfc7519
  // - you should never use this abstract class directly, but e.g. TJWTHS256,
  // TJWTHS384, TJWTHS512 or TJWTES256 (as defined in SynEcc.pas) inherited classes
  // - for security reasons, one inherited class is implementing a single
  // algorithm, as is very likely to be the case on production: you pickup one
  // "alg", then you stick to it; if your server needs more than one algorithm
  // for compatibility reasons, use a separate key and URI - this design will
  // reduce attack surface, and fully avoid weaknesses as described in
  // @https://auth0.com/blog/critical-vulnerabilities-in-json-web-token-libraries
  // and @http://tools.ietf.org/html/rfc7518#section-8.5
  TJWTAbstract = class(TSynPersistent)
  protected
    fAlgorithm: RawUTF8;
    fHeader: RawUTF8;
    fHeaderB64: RawUTF8;
    fClaims: TJWTClaims;
    fOptions: TJWTOptions;
    fAudience: TRawUTF8DynArray;
    fExpirationSeconds: integer;
    fIDGen: TSynUniqueIdentifierGenerator;
    fCacheTimeoutSeconds: integer;
    fCacheResults: TJWTResults;
    fCache: TSynDictionary;
    procedure SetCacheTimeoutSeconds(value: integer); virtual;
    function PayloadToJSON(const DataNameValue: array of const;
      const Issuer, Subject, Audience: RawUTF8; NotBefore: TDateTime;
      ExpirationMinutes: cardinal): RawUTF8; virtual;
    procedure Parse(const Token: RawUTF8; var JWT: TJWTContent;
      out headpayload: RawUTF8; out signature: RawByteString;
      excluded: TJWTClaims); virtual;
    function CheckAgainstActualTimestamp(var JWT: TJWTContent): boolean;
    // abstract methods which should be overriden by inherited classes
    function ComputeSignature(const headpayload: RawUTF8): RawUTF8;
     virtual; abstract;
    procedure CheckSignature(const headpayload: RawUTF8;
      const signature: RawByteString; var JWT: TJWTContent); virtual; abstract;
  public
    /// initialize the JWT processing instance
    // - the supplied set of claims are expected to be defined in the JWT payload
    // - aAudience are the allowed values for the jrcAudience claim
    // - aExpirationMinutes is the deprecation time for the jrcExpirationTime claim
    // - aIDIdentifier and aIDObfuscationKey are passed to a
    // TSynUniqueIdentifierGenerator instance used for jrcJwtID claim
    constructor Create(const aAlgorithm: RawUTF8; aClaims: TJWTClaims;
      const aAudience: array of RawUTF8; aExpirationMinutes: integer;
      aIDIdentifier: TSynUniqueIdentifierProcess; aIDObfuscationKey: RawUTF8); reintroduce;
    /// finalize the instance
    destructor Destroy; override;
    /// compute a new JWT for a given payload
    // - here the data payload is supplied as Name,Value pairs - by convention,
    // some registered Names (see TJWTClaim) should not be used here, and private
    // claims names are expected to be short (typically 3 chars), or an URI
    // - depending on the instance Claims, you should also specify associated
    // Issuer, Subject, Audience and NotBefore values; expected 'exp', 'nbf',
    // 'iat', 'jti' claims will also be generated and included, if needed
    // - you can override the aExpirationMinutes value as defined in Create()
    // - Audience is usually a single text, serialized as a JSON string, but
    // if the value supplied starts with '[', it is expected to be an array
    // of text values, already serialized as a JSON array of strings
    // - this method is thread-safe
    function Compute(const DataNameValue: array of const;
      const Issuer: RawUTF8 = ''; const Subject: RawUTF8 = '';
      const Audience: RawUTF8 = ''; NotBefore: TDateTime = 0;
      ExpirationMinutes: integer = 0; Signature: PRawUTF8 = nil): RawUTF8;
    /// compute a HTTP Authorization header containing a JWT for a given payload
    // - just a wrapper around Compute(), returned the HTTP header value:
    // $ Authorization: <HttpAuthorizationHeader>
    // following the expected pattern:
    // $ Authorization: Bearer <Token>
    // - this method is thread-safe
    function ComputeAuthorizationHeader(const DataNameValue: array of const;
      const Issuer: RawUTF8 = ''; const Subject: RawUTF8 = '';
      const Audience: RawUTF8 = ''; NotBefore: TDateTime = 0;
      ExpirationMinutes: integer = 0): RawUTF8;
    /// check a JWT value, and its signature
    // - will validate all expected Claims (minus ExcludedClaims optional
    // parameter), and the associated signature
    // - verification state is returned in JWT.result (jwtValid for a valid JWT),
    // together with all parsed payload information
    // - supplied JWT is transmitted e.g. in HTTP header:
    // $ Authorization: Bearer <Token>
    // - this method is thread-safe
    procedure Verify(const Token: RawUTF8; out JWT: TJWTContent;
      ExcludedClaims: TJWTClaims = []); overload;
    /// check a JWT value, and its signature
    // - will validate all expected Claims, and the associated signature
    // - verification state is returned as function result
    // - supplied JWT is transmitted e.g. in HTTP header:
    // $ Authorization: Bearer <Token>
    // - this method is thread-safe
    function Verify(const Token: RawUTF8): TJWTResult; overload;
    /// check a HTTP Authorization header value as JWT, and its signature
    // - will validate all expected Claims, and the associated signature
    // - verification state is returned in JWT.result (jwtValid for a valid JWT),
    // together with all parsed payload information
    // - expect supplied HttpAuthorizationHeader as transmitted in HTTP header:
    // $ Authorization: <HttpAuthorizationHeader>
    // - this method is thread-safe
    function VerifyAuthorizationHeader(const HttpAuthorizationHeader: RawUTF8;
      out JWT: TJWTContent): boolean; overload;
    /// in-place decoding and quick check of the JWT paylod
    // - it won't check the signature, but the header's algorithm against the
    // class name (use TJWTAbstract class to allow any algorithm)
    // - it will decode the JWT payload and check for its expiration, and some
    // mandatory fied values - you can optionally retrieve the Expiration time,
    // the ending Signature, and/or the Payload decoded as TDocVariant
    // - NotBeforeDelta allows to define some time frame for the "nbf" field
    // - may be used on client side to quickly validate a JWT received from
    // server, without knowing the exact algorithm or secret keys
    class function VerifyPayload(const Token, ExpectedSubject, ExpectedIssuer,
      ExpectedAudience: RawUTF8; Expiration: PUnixTime = nil;
      Signature: PRawUTF8 = nil; Payload: PVariant = nil;
      IgnoreTime: boolean = false; NotBeforeDelta: TUnixTime = 15): TJWTResult;
  published
    /// the name of the algorithm used by this instance (e.g. 'HS256')
    property Algorithm: RawUTF8
      read fAlgorithm;
    /// allow to tune the Verify and Compute method process
    property Options: TJWTOptions
      read fOptions write fOptions;
    /// the JWT Registered Claims, as implemented by this instance
    // - Verify() method will ensure all claims are defined in the payload,
    // then fill TJWTContent.reg[] with all corresponding values
    property Claims: TJWTClaims
      read fClaims;
    /// the period, in seconds, for the "exp" claim
    property ExpirationSeconds: integer
      read fExpirationSeconds;
    /// the audience string values associated with this instance
    // - will be checked by Verify() method, and set in TJWTContent.audience
    property Audience: TRawUTF8DynArray
      read fAudience;
    /// delay of optional in-memory cache of Verify() TJWTContent
    // - equals 0 by default, i.e. cache is disabled
    // - may be useful if the signature process is very resource consumming
    // (e.g. for TJWTES256 or even HMAC-SHA-256) - see also CacheResults
    // - each time this property is assigned, internal cache content is flushed
    property CacheTimeoutSeconds: integer
      read fCacheTimeoutSeconds write SetCacheTimeoutSeconds;
    /// which TJWTContent.result should be stored in in-memory cache
    // - default is [jwtValid] but you may also include jwtInvalidSignature
    // if signature checking uses a lot of resources
    // - only used if CacheTimeoutSeconds>0
    property CacheResults: TJWTResults
      read fCacheResults write fCacheResults;
  end;

  /// class-reference type (metaclass) of a JWT algorithm process
  TJWTAbstractClass = class of TJWTAbstract;

  /// implements JSON Web Tokens using 'none' algorithm
  // - as defined in @http://tools.ietf.org/html/rfc7518 paragraph 3.6
  // - you should never use this weak algorithm in production, unless your
  // communication is already secured by other means, and use JWT as cookies
  TJWTNone = class(TJWTAbstract)
  protected
    function ComputeSignature(const headpayload: RawUTF8): RawUTF8; override;
    procedure CheckSignature(const headpayload: RawUTF8;
      const signature: RawByteString; var JWT: TJWTContent); override;
  public
    /// initialize the JWT processing using the 'none' algorithm
    // - the supplied set of claims are expected to be defined in the JWT payload
    // - aAudience are the allowed values for the jrcAudience claim
    // - aExpirationMinutes is the deprecation time for the jrcExpirationTime claim
    // - aIDIdentifier and aIDObfuscationKey are passed to a
    // TSynUniqueIdentifierGenerator instance used for jrcJwtID claim
    constructor Create(aClaims: TJWTClaims; const aAudience: array of RawUTF8;
      aExpirationMinutes: integer = 0; aIDIdentifier: TSynUniqueIdentifierProcess = 0;
      aIDObfuscationKey: RawUTF8 = ''); reintroduce;
  end;


const
  /// the text field names of the registerd claims, as defined by RFC 7519
  // - see TJWTClaim enumeration and TJWTClaims set
  // - RFC standard expects those to be case-sensitive
  JWT_CLAIMS_TEXT: array[TJWTClaim] of RawUTF8 = (
    'iss', 'sub', 'aud', 'exp', 'nbf', 'iat', 'jti');

function ToText(res: TJWTResult): PShortString; overload;
function ToCaption(res: TJWTResult): string; overload;
function ToText(claim: TJWTClaim): PShortString; overload;
function ToText(claims: TJWTClaims): ShortString; overload;


{ **************** JWT Implementation of HS and S3 Algorithms }

type
  /// abstract parent of JSON Web Tokens using HMAC-SHA2 or SHA-3 algorithms
  // - SHA-3 is not yet officially defined in @http://tools.ietf.org/html/rfc7518
  // but could be used as a safer (and sometimes faster) alternative to HMAC-SHA2
  // - digital signature will be processed by an internal TSynSigner instance
  // - never use this abstract class, but any inherited class, or
  // JWT_CLASS[].Create to instantiate a JWT process from a given algorithm
  TJWTSynSignerAbstract = class(TJWTAbstract)
  protected
    fSignPrepared: TSynSigner;
    function GetAlgo: TSignAlgo; virtual; abstract;
    function ComputeSignature(const headpayload: RawUTF8): RawUTF8; override;
    procedure CheckSignature(const headpayload: RawUTF8;
      const signature: RawByteString; var JWT: TJWTContent); override;
  public
    /// initialize the JWT processing using SHA3 algorithm
    // - the supplied set of claims are expected to be defined in the JWT payload
    // - the supplied secret text will be used to compute the digital signature,
    // directly if aSecretPBKDF2Rounds=0, or via PBKDF2 iterative key derivation
    // if some number of rounds were specified
    // - aAudience are the allowed values for the jrcAudience claim
    // - aExpirationMinutes is the deprecation time for the jrcExpirationTime claim
    // - aIDIdentifier and aIDObfuscationKey are passed to a
    // TSynUniqueIdentifierGenerator instance used for jrcJwtID claim
    // - optionally return the PBKDF2 derivated key for aSecretPBKDF2Rounds>0
    constructor Create(const aSecret: RawUTF8; aSecretPBKDF2Rounds: integer;
      aClaims: TJWTClaims; const aAudience: array of RawUTF8;
      aExpirationMinutes: integer = 0; aIDIdentifier: TSynUniqueIdentifierProcess = 0;
      aIDObfuscationKey: RawUTF8 = ''; aPBKDF2Secret: PHash512Rec = nil); reintroduce;
    /// finalize the instance
    destructor Destroy; override;
    /// the digital signature size, in byte
    property SignatureSize: integer
      read fSignPrepared.SignatureSize;
    /// the TSynSigner raw algorithm used for digital signature
    property SignatureAlgo: TSignAlgo
      read fSignPrepared.Algo;
    /// low-level read access to the internal signature structure
    property SignPrepared: TSynSigner
      read fSignPrepared;
  end;

  /// meta-class for TJWTSynSignerAbstract creations
  TJWTSynSignerAbstractClass = class of TJWTSynSignerAbstract;


type
  /// implements JSON Web Tokens using 'HS256' (HMAC SHA-256) algorithm
  // - as defined in @http://tools.ietf.org/html/rfc7518 paragraph 3.2
  // - our HMAC SHA-256 implementation used is thread safe, and very fast
  // (x86: 3us, x64: 2.5us) so cache is not needed
  // - resulting signature size will be of 256 bits
  TJWTHS256 = class(TJWTSynSignerAbstract)
  protected
    function GetAlgo: TSignAlgo; override;
  end;

  /// implements JSON Web Tokens using 'HS384' (HMAC SHA-384) algorithm
  // - as defined in @http://tools.ietf.org/html/rfc7518 paragraph 3.2
  // - our HMAC SHA-384 implementation used is thread safe, and very fast
  // even on x86 (if the CPU supports SSE3 opcodes)
  // - resulting signature size will be of 384 bits
  TJWTHS384 = class(TJWTSynSignerAbstract)
  protected
    function GetAlgo: TSignAlgo; override;
  end;

  /// implements JSON Web Tokens using 'HS512' (HMAC SHA-512) algorithm
  // - as defined in @http://tools.ietf.org/html/rfc7518 paragraph 3.2
  // - our HMAC SHA-512 implementation used is thread safe, and very fast
  // even on x86 (if the CPU supports SSE3 opcodes)
  // - resulting signature size will be of 512 bits
  TJWTHS512 = class(TJWTSynSignerAbstract)
  protected
    function GetAlgo: TSignAlgo; override;
  end;

  /// experimental JSON Web Tokens using SHA3-224 algorithm
  // - SHA-3 is not yet officially defined in @http://tools.ietf.org/html/rfc7518
  // but could be used as a safer (and sometimes faster) alternative to HMAC-SHA2
  // - resulting signature size will be of 224 bits
  TJWTS3224 = class(TJWTSynSignerAbstract)
  protected
    function GetAlgo: TSignAlgo; override;
  end;

  /// experimental JSON Web Tokens using SHA3-256 algorithm
  // - SHA-3 is not yet officially defined in @http://tools.ietf.org/html/rfc7518
  // but could be used as a safer (and sometimes faster) alternative to HMAC-SHA2
  // - resulting signature size will be of 256 bits
  TJWTS3256 = class(TJWTSynSignerAbstract)
  protected
    function GetAlgo: TSignAlgo; override;
  end;

  /// experimental JSON Web Tokens using SHA3-384 algorithm
  // - SHA-3 is not yet officially defined in @http://tools.ietf.org/html/rfc7518
  // but could be used as a safer (and sometimes faster) alternative to HMAC-SHA2
  // - resulting signature size will be of 384 bits
  TJWTS3384 = class(TJWTSynSignerAbstract)
  protected
    function GetAlgo: TSignAlgo; override;
  end;

  /// experimental JSON Web Tokens using SHA3-512 algorithm
  // - SHA-3 is not yet officially defined in @http://tools.ietf.org/html/rfc7518
  // but could be used as a safer (and sometimes faster) alternative to HMAC-SHA2
  // - resulting signature size will be of 512 bits
  TJWTS3512 = class(TJWTSynSignerAbstract)
  protected
    function GetAlgo: TSignAlgo; override;
  end;

  /// experimental JSON Web Tokens using SHA3-SHAKE128 algorithm
  // - SHA-3 is not yet officially defined in @http://tools.ietf.org/html/rfc7518
  // but could be used as a safer (and sometimes faster) alternative to HMAC-SHA2
  // - resulting signature size will be of 256 bits
  TJWTS3S128 = class(TJWTSynSignerAbstract)
  protected
    function GetAlgo: TSignAlgo; override;
  end;

  /// experimental JSON Web Tokens using SHA3-SHAKE256 algorithm
  // - SHA-3 is not yet officially defined in @http://tools.ietf.org/html/rfc7518
  // but could be used as a safer (and sometimes faster) alternative to HMAC-SHA2
  // - resulting signature size will be of 512 bits
  TJWTS3S256 = class(TJWTSynSignerAbstract)
  protected
    function GetAlgo: TSignAlgo; override;
  end;


const
  /// how TJWTSynSignerAbstract algorithms are identified in the JWT
  // - SHA-1 will fallback to HS256 (since there will never be SHA-1 support)
  // - SHA-3 is not yet officially defined in @http://tools.ietf.org/html/rfc7518
  JWT_TEXT: array[TSignAlgo] of RawUTF8 = (
    'HS256', 'HS256', 'HS384', 'HS512',
    'S3224', 'S3256', 'S3384', 'S3512', 'S3S128', 'S3S256');

  /// able to instantiate any of the TJWTSynSignerAbstract instance expected
  // - SHA-1 will fallback to TJWTHS256 (since SHA-1 will never be supported)
  // - SHA-3 is not yet officially defined in @http://tools.ietf.org/html/rfc7518
  // - typical use is the following:
  // ! result := JWT_CLASS[algo].Create(master, round, claims, [], expirationMinutes);
  JWT_CLASS: array[TSignAlgo] of TJWTSynSignerAbstractClass = (
    TJWTHS256, TJWTHS256, TJWTHS384, TJWTHS512,
    TJWTS3224, TJWTS3256, TJWTS3384, TJWTS3512, TJWTS3S128, TJWTS3S256);


{ **************  JWT Implementation of ES256 Algorithm }

type
  /// implements JSON Web Tokens using 'ES256' algorithm
  // - i.e. ECDSA using the P-256 curve and the SHA-256 hash algorithm
  // - as defined in http://tools.ietf.org/html/rfc7518 paragraph 3.4
  // - since ECDSA signature and verification is CPU consumming (under x86, it
  // takes 2.5 ms, but only 0.3 ms on x64) you may enable CacheTimeoutSeconds
  TJWTES256 = class(TJWTAbstract)
  protected
    fCertificate: TECCCertificate;
    fOwnCertificate: boolean;
    function ComputeSignature(const headpayload: RawUTF8): RawUTF8; override;
    procedure CheckSignature(const headpayload: RawUTF8; const signature: RawByteString;
      var JWT: TJWTContent); override;
  public
    /// initialize the JWT processing instance using ECDSA P-256 algorithm
    // - the supplied set of claims are expected to be defined in the JWT payload
    // - the supplied ECC certificate should be a TECCCertificate storing the
    // public key needed for Verify(), or a TECCCertificateSecret storing also
    // the private key required by Compute()
    // - aCertificate is owned by this instance if property OwnCertificate is true
    // - aAudience are the allowed values for the jrcAudience claim
    // - aExpirationMinutes is the deprecation time for the jrcExpirationTime claim
    // - aIDIdentifier and aIDObfuscationKey are passed to a
    // TSynUniqueIdentifierGenerator instance used for jrcJwtID claim
    constructor Create(aCertificate: TECCCertificate; aClaims: TJWTClaims;
      const aAudience: array of RawUTF8; aExpirationMinutes: integer = 0;
      aIDIdentifier: TSynUniqueIdentifierProcess = 0;
      aIDObfuscationKey: RawUTF8 = ''); reintroduce;
    /// finalize the instance
    destructor Destroy; override;
    /// access to the associated TECCCertificate instance
    // - which may be a TECCCertificateSecret for Compute() private key
    property Certificate: TECCCertificate
      read fCertificate;
    /// if the associated TECCCertificate is to be owned by this instance
    property OwnCertificate: boolean
      read fOwnCertificate write fOwnCertificate;
  end;



implementation


{ **************** Abstract JWT Parsing and Computation }

{ TJWTAbstract }

constructor TJWTAbstract.Create(const aAlgorithm: RawUTF8; aClaims: TJWTClaims;
  const aAudience: array of RawUTF8; aExpirationMinutes: integer;
  aIDIdentifier: TSynUniqueIdentifierProcess; aIDObfuscationKey: RawUTF8);
begin
  if aAlgorithm = '' then
    raise EJWTException.CreateUTF8('%.Create(algo?)', [self]);
  inherited Create;
  if high(aAudience) >= 0 then
  begin
    fAudience := TRawUTF8DynArrayFrom(aAudience);
    include(aClaims, jrcAudience);
  end;
  if aExpirationMinutes > 0 then
  begin
    include(aClaims, jrcExpirationTime);
    fExpirationSeconds := aExpirationMinutes * 60;
  end
  else
    exclude(aClaims, jrcExpirationTime);
  fAlgorithm := aAlgorithm;
  fClaims := aClaims;
  if jrcJwtID in aClaims then
    fIDGen := TSynUniqueIdentifierGenerator.Create(aIDIdentifier, aIDObfuscationKey);
  if fHeader = '' then
    FormatUTF8('{"alg":"%","typ":"JWT"}', [aAlgorithm], fHeader);
  fHeaderB64 := BinToBase64URI(fHeader) + '.';
  fCacheResults := [jwtValid];
end;

destructor TJWTAbstract.Destroy;
begin
  fIDGen.Free;
  fCache.Free;
  inherited;
end;

const
  JWT_MAXSIZE = 4096; // coherent with HTTP headers limitations

function TJWTAbstract.Compute(const DataNameValue: array of const;
  const Issuer, Subject, Audience: RawUTF8; NotBefore: TDateTime;
  ExpirationMinutes: integer; Signature: PRawUTF8): RawUTF8;
var
  payload, headpayload, signat: RawUTF8;
begin
  result := '';
  if self = nil then
    exit;
  payload := PayloadToJSON(DataNameValue, Issuer, Subject, Audience,
    NotBefore, ExpirationMinutes);
  headpayload := fHeaderB64 + BinToBase64URI(payload);
  signat := ComputeSignature(headpayload);
  result := headpayload + '.' + signat;
  if length(result) > JWT_MAXSIZE then
    raise EJWTException.CreateUTF8('%.Compute oversize: len=%',
      [self, length(result)]);
  if Signature <> nil then
    Signature^ := signat;
end;

function TJWTAbstract.ComputeAuthorizationHeader(
  const DataNameValue: array of const; const Issuer, Subject, Audience: RawUTF8;
  NotBefore: TDateTime; ExpirationMinutes: integer): RawUTF8;
begin
  if self = nil then
    result := ''
  else
    result := 'Bearer ' + Compute(DataNameValue, Issuer, Subject, Audience,
      NotBefore, ExpirationMinutes);
end;

function TJWTAbstract.PayloadToJSON(const DataNameValue: array of const;
  const Issuer, Subject, Audience: RawUTF8; NotBefore: TDateTime;
  ExpirationMinutes: cardinal): RawUTF8;

  procedure RaiseMissing(c: TJWTClaim);
  begin
    raise EJWTException.CreateUTF8('%.PayloadJSON: missing %', [self, ToText(c)^]);
  end;

var
  payload: TDocVariantData;
begin
  result := '';
  payload.InitObject(DataNameValue, JSON_OPTIONS_FAST);
  if jrcIssuer in fClaims then
    if Issuer = '' then
      RaiseMissing(jrcIssuer)
    else
      payload.AddValueFromText(JWT_CLAIMS_TEXT[jrcIssuer], Issuer, true);
  if jrcSubject in fClaims then
    if Subject = '' then
      RaiseMissing(jrcSubject)
    else
      payload.AddValueFromText(JWT_CLAIMS_TEXT[jrcSubject], Subject, true);
  if jrcAudience in fClaims then
    if Audience = '' then
      RaiseMissing(jrcAudience)
    else if Audience[1] = '[' then
      payload.AddOrUpdateValue(JWT_CLAIMS_TEXT[jrcAudience], _JsonFast(Audience))
    else
      payload.AddValueFromText(JWT_CLAIMS_TEXT[jrcAudience], Audience, true);
  if jrcNotBefore in fClaims then
    if NotBefore <= 0 then
      payload.AddOrUpdateValue(JWT_CLAIMS_TEXT[jrcNotBefore], UnixTimeUTC)
    else
      payload.AddOrUpdateValue(JWT_CLAIMS_TEXT[jrcNotBefore], DateTimeToUnixTime(NotBefore));
  if jrcIssuedAt in fClaims then
    payload.AddOrUpdateValue(JWT_CLAIMS_TEXT[jrcIssuedAt], UnixTimeUTC);
  if jrcExpirationTime in fClaims then
  begin
    if ExpirationMinutes = 0 then
      ExpirationMinutes := fExpirationSeconds
    else
      ExpirationMinutes := ExpirationMinutes * 60;
    payload.AddOrUpdateValue(JWT_CLAIMS_TEXT[jrcExpirationTime],
      UnixTimeUTC + ExpirationMinutes);
  end;
  if jrcJwtID in fClaims then
    if joNoJwtIDGenerate in fOptions then
    begin
      if payload.GetValueIndex(JWT_CLAIMS_TEXT[jrcJwtID]) < 0 then
        exit; // not generated, but should be supplied
    end
    else
      payload.AddValueFromText(JWT_CLAIMS_TEXT[jrcJwtID],
        fIDGen.ToObfuscated(fIDGen.ComputeNew));
  result := payload.ToJSON;
end;

procedure TJWTAbstract.SetCacheTimeoutSeconds(value: integer);
begin
  fCacheTimeoutSeconds := value;
  FreeAndNil(fCache);
  if (value > 0) and
     (fCacheResults <> []) then
    fCache := TSynDictionary.Create(
      TypeInfo(TRawUTF8DynArray), TypeInfo(TJWTContentDynArray), false, value);
end;

procedure TJWTAbstract.Verify(const Token: RawUTF8; out JWT: TJWTContent;
  ExcludedClaims: TJWTClaims);
var
  headpayload: RawUTF8;
  signature: RawByteString;
  fromcache: boolean;
begin
  JWT.result := jwtNoToken;
  if (self = nil) or
     (fCache = nil) then
    fromcache := false
  else
  begin
    fromcache := fCache.FindAndCopy(Token, JWT);
    fCache.DeleteDeprecated;
  end;
  if not fromcache then
    Parse(Token, JWT, headpayload, signature, ExcludedClaims);
  if JWT.result in [jwtValid, jwtNotBeforeFailed] then
    if CheckAgainstActualTimestamp(JWT) and
       not fromcache then
      // depending on the algorithm used
      CheckSignature(headpayload{%H-}, signature{%H-}, JWT);
  if not fromcache and
     (self <> nil) and
     (fCache <> nil) and
     (JWT.result in fCacheResults) then
    fCache.Add(Token, JWT);
end;

function TJWTAbstract.Verify(const Token: RawUTF8): TJWTResult;
var
  jwt: TJWTContent;
begin
  Verify(Token, jwt);
  result := jwt.result;
end;

function TJWTAbstract.CheckAgainstActualTimestamp(var JWT: TJWTContent): boolean;
var
  nowunix, unix: cardinal;
begin
  if [jrcExpirationTime, jrcNotBefore, jrcIssuedAt] * JWT.claims <> [] then
  begin
    result := false;
    nowunix := UnixTimeUTC; // validate against actual timestamp
    if jrcExpirationTime in JWT.claims then
      if not ToCardinal(JWT.reg[jrcExpirationTime], unix) or
         (nowunix > {%H-}unix) then
      begin
        JWT.result := jwtExpired;
        exit;
      end;
    if jrcNotBefore in JWT.claims then
      if not ToCardinal(JWT.reg[jrcNotBefore], unix) or
         (nowunix < unix) then
      begin
        JWT.result := jwtNotBeforeFailed;
        exit;
      end;
    if jrcIssuedAt in JWT.claims then
      if not ToCardinal(JWT.reg[jrcIssuedAt], unix) or
         // +60 to allow 1 minute time lap between nodes
         (unix > nowunix + 60) then
      begin
        JWT.result := jwtInvalidIssuedAt;
        exit;
      end;
  end;
  result := true;
  JWT.result := jwtValid;
end;

procedure TJWTAbstract.Parse(const Token: RawUTF8; var JWT: TJWTContent;
  out headpayload: RawUTF8; out signature: RawByteString; excluded: TJWTClaims);
var
  payloadend, j, toklen, c, cap, headerlen, len, a: integer;
  P: PUTF8Char;
  N, V: PUTF8Char;
  wasString: boolean;
  EndOfObject: AnsiChar;
  claim: TJWTClaim;
  requiredclaims: TJWTClaims;
  id: TSynUniqueIdentifierBits;
  value: variant;
  payload: RawUTF8;
  head: array[0..1] of TValuePUTF8Char;
  aud: TDocVariantData;
  tok: PAnsiChar absolute Token;
begin
  // 0. initialize parsing
  Finalize(JWT.reg);
  JWT.data.InitFast(0, dvObject); // custom claims
  byte(JWT.claims) := 0;
  word(JWT.audience) := 0;
  toklen := length(Token);
  if (toklen = 0) or
     (self = nil) then
  begin
    JWT.result := jwtNoToken;
    exit;
  end;
  // 1. validate the header (including algorithm "alg" verification)
  JWT.result := jwtInvalidAlgorithm;
  if joHeaderParse in fOptions then
  begin // slower parsing
    headerlen := PosExChar('.', Token);
    if (headerlen = 0) or
       (headerlen > 512) then
      exit;
    Base64URIToBin(tok, headerlen - 1, signature);
    JSONDecode(pointer(signature), ['alg', 'typ'], @head);
    if not head[0].Idem(fAlgorithm) or
       ((head[1].value <> nil) and
        not head[1].Idem('JWT')) then
      exit;
  end
  else
  begin // fast direct compare of fHeaderB64 (including "alg")
    headerlen := length(fHeaderB64);
    if (toklen <= headerlen) or
       not CompareMem(pointer(fHeaderB64), tok, headerlen) then
      exit;
  end;
  // 2. extract the payload
  JWT.result := jwtWrongFormat;
  if toklen > JWT_MAXSIZE then
    exit;
  payloadend := PosEx('.', Token, headerlen + 1);
  if (payloadend = 0) or
     (payloadend - headerlen > 2700) then
    exit;
  Base64URIToBin(tok + payloadend, toklen - payloadend, signature);
  if (signature = '') and
     (payloadend <> toklen) then
    exit;
  JWT.result := jwtInvalidPayload;
  Base64URIToBin(tok + headerlen, payloadend - headerlen - 1, RawByteString(payload));
  if payload = '' then
    exit;
  // 3. decode the payload into JWT.reg[]/JWT.claims (known) and JWT.data (custom)
  P := GotoNextNotSpace(pointer(payload));
  if P^ <> '{' then
    exit;
  P := GotoNextNotSpace(P + 1);
  cap := JSONObjectPropCount(P);
  if cap < 0 then
    exit;
  requiredclaims := fClaims - excluded;
  if cap > 0 then
    repeat
      N := GetJSONPropName(P);
      if N = nil then
        exit;
      V := GetJSONFieldOrObjectOrArray(P, @wasString, @EndOfObject, true);
      if V = nil then
        exit;
      len := StrLen(N);
      if len = 3 then
      begin
        c := PInteger(N)^;
        for claim := low(claim) to high(claim) do
          if PInteger(JWT_CLAIMS_TEXT[claim])^ = c then
          begin
            if V^ = #0 then
              exit;
            include(JWT.claims, claim);
            if not (claim in fClaims) and
               not (joAllowUnexpectedClaims in fOptions) then
            begin
              JWT.result := jwtUnexpectedClaim;
              exit;
            end;
            FastSetString(JWT.reg[claim], V, StrLen(V));
            if claim in requiredclaims then
              case claim of
                jrcJwtID:
                  if not (joNoJwtIDCheck in fOptions) then
                    if not fIDGen.FromObfuscated(JWT.reg[jrcJwtID], id.Value) or
                       (id.CreateTimeUnix < UNIXTIME_MINIMAL) then
                    begin
                      JWT.result := jwtInvalidID;
                      exit;
                    end;
                jrcAudience:
                  if JWT.reg[jrcAudience][1] = '[' then
                  begin
                    aud.InitJSON(JWT.reg[jrcAudience], JSON_OPTIONS_FAST);
                    if aud.Count = 0 then
                      exit;
                    for j := 0 to aud.Count - 1 do
                    begin
                      a := FindRawUTF8(fAudience, VariantToUTF8(aud.Values[j]));
                      if a < 0 then
                      begin
                        JWT.result := jwtUnknownAudience;
                        if not (joAllowUnexpectedAudience in fOptions) then
                          exit;
                      end
                      else
                        include(JWT.audience, a);
                    end;
                    aud.Clear;
                  end
                  else
                  begin
                    a := FindRawUTF8(fAudience, JWT.reg[jrcAudience]);
                    if a < 0 then
                    begin
                      JWT.result := jwtUnknownAudience;
                      if not (joAllowUnexpectedAudience in fOptions) then
                        exit;
                    end
                    else
                      include(JWT.audience, a);
                  end;
              end;
            len := 0; // don't add to JWT.data
            dec(cap);
            break;
          end;
        if len = 0 then
          continue;
      end;
      GetVariantFromJSON(V, wasString, value, @JSON_OPTIONS[true],
        joDoubleInData in fOptions);
      if JWT.data.Count = 0 then
        JWT.data.Capacity := cap;
      JWT.data.AddValue(N, len, value)
    until EndOfObject = '}';
  if JWT.data.Count > 0 then
    JWT.data.Capacity := JWT.data.Count;
  if requiredclaims - JWT.claims <> [] then
    JWT.result := jwtMissingClaim
  else
  begin
    FastSetString(headpayload, tok, payloadend - 1);
    JWT.result := jwtValid;
  end;
end;

function TJWTAbstract.VerifyAuthorizationHeader(
  const HttpAuthorizationHeader: RawUTF8; out JWT: TJWTContent): boolean;
begin
  if (cardinal(length(HttpAuthorizationHeader) - 10) > 4096) or
     not IdemPChar(pointer(HttpAuthorizationHeader), 'BEARER ') then
    JWT.result := jwtWrongFormat
  else
    Verify(copy(HttpAuthorizationHeader, 8, maxInt), JWT);
  result := JWT.result = jwtValid;
end;

class function TJWTAbstract.VerifyPayload(const Token,
  ExpectedSubject, ExpectedIssuer, ExpectedAudience: RawUTF8;
  Expiration: PUnixTime; Signature: PRawUTF8; Payload: PVariant;
  IgnoreTime: boolean; NotBeforeDelta: TUnixTime): TJWTResult;
var
  P, B: PUTF8Char;
  V: array[0..4] of TValuePUTF8Char;
  now, time: PtrUInt;
  text: RawUTF8;
begin
  result := jwtInvalidAlgorithm;
  B := pointer(Token);
  P := PosChar(B, '.');
  if P = nil then
    exit;
  if self <> TJWTAbstract then
  begin
    text := Base64URIToBin(PAnsiChar(B), P - B);
    if not IdemPropNameU(copy(ToText(self), 5, 10), JSONDecode(text, 'alg')) then
      exit;
  end;
  B := P + 1;
  P := PosChar(B, '.');
  result := jwtInvalidSignature;
  if P = nil then
    exit;
  result := jwtInvalidPayload;
  text := Base64URIToBin(PAnsiChar(B), P - B);
  if text = '' then
    exit;
  if Payload <> nil then
    _Json(text, Payload^, JSON_OPTIONS_FAST);
  JSONDecode(pointer(text), ['iss', 'aud', 'exp', 'nbf', 'sub'], @V, true);
  result := jwtUnexpectedClaim;
  if ((ExpectedSubject <> '') and
      not V[4].Idem(ExpectedSubject)) or
     ((ExpectedIssuer <> '') and
      not V[0].Idem(ExpectedIssuer)) then
    exit;
  result := jwtUnknownAudience;
  if (ExpectedAudience <> '') and
     not V[1].Idem(ExpectedAudience) then
    exit;
  if Expiration <> nil then
    Expiration^ := 0;
  if (V[2].value <> nil) or
     (V[3].value <> nil) then
  begin
    now := UnixTimeUTC;
    if V[2].value <> nil then
    begin
      time := V[2].ToCardinal;
      result := jwtExpired;
      if not IgnoreTime and
         (now > time) then
        exit;
      if Expiration <> nil then
        Expiration^ := time;
    end;
    if not IgnoreTime and
       (V[3].value <> nil) then
    begin
      time := V[3].ToCardinal;
      result := jwtNotBeforeFailed;
      if (time = 0) or
         (now + PtrUInt(NotBeforeDelta) < time) then
        exit;
    end;
  end;
  inc(P);
  if Signature <> nil then
    FastSetString(Signature^, P, StrLen(P));
  result := jwtValid;
end;


{ TJWTNone }

constructor TJWTNone.Create(aClaims: TJWTClaims;
  const aAudience: array of RawUTF8; aExpirationMinutes: integer;
  aIDIdentifier: TSynUniqueIdentifierProcess; aIDObfuscationKey: RawUTF8);
begin
  fHeader := '{"alg":"none"}'; // "typ":"JWT" is optional, so we save a few bytes
  inherited Create('none', aClaims, aAudience, aExpirationMinutes,
    aIDIdentifier, aIDObfuscationKey);
end;

procedure TJWTNone.CheckSignature(const headpayload: RawUTF8;
  const signature: RawByteString; var JWT: TJWTContent);
begin
  if signature = '' then // JWA defined empty string for "none" JWS
    JWT.result := jwtValid
  else
    JWT.result := jwtInvalidSignature;
end;

function TJWTNone.ComputeSignature(const headpayload: RawUTF8): RawUTF8;
begin
  result := '';
end;


var
  _TJWTResult: array[TJWTResult] of PShortString;
  _TJWTClaim: array[TJWTClaim] of PShortString;

function ToText(res: TJWTResult): PShortString;
begin
  result := _TJWTResult[res];
end;

function ToCaption(res: TJWTResult): string;
begin
  GetCaptionFromTrimmed(_TJWTResult[res], result);
end;

function ToText(claim: TJWTClaim): PShortString;
begin
  result := _TJWTClaim[claim];
end;

function ToText(claims: TJWTClaims): ShortString;
begin
  GetSetNameShort(TypeInfo(TJWTClaims), claims, result);
end;



{ **************** JWT Implementation of HS* and S3* Algorithms }

{ TJWTSynSignerAbstract }

constructor TJWTSynSignerAbstract.Create(const aSecret: RawUTF8;
  aSecretPBKDF2Rounds: integer; aClaims: TJWTClaims;
  const aAudience: array of RawUTF8; aExpirationMinutes: integer;
  aIDIdentifier: TSynUniqueIdentifierProcess; aIDObfuscationKey: RawUTF8;
  aPBKDF2Secret: PHash512Rec);
var
  algo: TSignAlgo;
begin
  algo := GetAlgo;
  inherited Create(JWT_TEXT[algo], aClaims, aAudience, aExpirationMinutes,
    aIDIdentifier, aIDObfuscationKey);
  if (aSecret <> '') and
     (aSecretPBKDF2Rounds > 0) then
    fSignPrepared.Init(algo, aSecret, fHeaderB64, aSecretPBKDF2Rounds, aPBKDF2Secret)
  else
    fSignPrepared.Init(algo, aSecret);
end;

procedure TJWTSynSignerAbstract.CheckSignature(const headpayload: RawUTF8;
  const signature: RawByteString; var JWT: TJWTContent);
var
  signer: TSynSigner;
  temp: THash512Rec;
begin
  JWT.result := jwtInvalidSignature;
  if length(signature) <> SignatureSize then
    exit;
  signer := fSignPrepared; // thread-safe re-use of prepared TSynSigner
  signer.Update(pointer(headpayload), length(headpayload));
  signer.Final(temp);
{  writeln('payload=',headpayload);
   writeln('sign=',bintohex(@temp,SignatureSize));
   writeln('expected=',bintohex(pointer(signature),SignatureSize)); }
  if CompareMem(@temp, pointer(signature), SignatureSize) then
    JWT.result := jwtValid;
end;

function TJWTSynSignerAbstract.ComputeSignature(
  const headpayload: RawUTF8): RawUTF8;
var
  signer: TSynSigner;
  temp: THash512Rec;
begin
  signer := fSignPrepared;
  signer.Update(pointer(headpayload), length(headpayload));
  signer.Final(temp);
  result := BinToBase64URI(@temp, SignatureSize);
end;

destructor TJWTSynSignerAbstract.Destroy;
begin
  FillCharFast(fSignPrepared, SizeOf(fSignPrepared), 0);
  inherited Destroy;
end;


{ TJWTHS256 }

function TJWTHS256.GetAlgo: TSignAlgo;
begin
  result := saSha256;
end;

{ TJWTHS384 }

function TJWTHS384.GetAlgo: TSignAlgo;
begin
  result := saSha384;
end;

{ TJWTHS512 }

function TJWTHS512.GetAlgo: TSignAlgo;
begin
  result := saSha512;
end;

{ TJWTS3224 }

function TJWTS3224.GetAlgo: TSignAlgo;
begin
  result := saSha3224;
end;

{ TJWTS3256 }

function TJWTS3256.GetAlgo: TSignAlgo;
begin
  result := saSha3256;
end;

{ TJWTS3384 }

function TJWTS3384.GetAlgo: TSignAlgo;
begin
  result := saSha3384;
end;

{ TJWTS3512 }

function TJWTS3512.GetAlgo: TSignAlgo;
begin
  result := saSha3512;
end;

{ TJWTS3S128 }

function TJWTS3S128.GetAlgo: TSignAlgo;
begin
  result := saSha3S128;
end;

{ TJWTS3S256 }

function TJWTS3S256.GetAlgo: TSignAlgo;
begin
  result := saSha3S256;
end;



{ **************  JWT Implementation of ES256 Algorithm }

{ TJWTES256 }

constructor TJWTES256.Create(aCertificate: TECCCertificate; aClaims: TJWTClaims;
  const aAudience: array of RawUTF8; aExpirationMinutes: integer;
  aIDIdentifier: TSynUniqueIdentifierProcess; aIDObfuscationKey: RawUTF8);
begin
  if not aCertificate.CheckCRC then
    raise EJWTException.CreateUTF8('%.Create(aCertificate?)', [self]);
  inherited Create('ES256', aClaims, aAudience, aExpirationMinutes,
    aIDIdentifier, aIDObfuscationKey);
  fCertificate := aCertificate;
end;

destructor TJWTES256.Destroy;
begin
  if fOwnCertificate then
    fCertificate.Free;
  inherited;
end;

procedure TJWTES256.CheckSignature(const headpayload: RawUTF8;
  const signature: RawByteString; var JWT: TJWTContent);
var
  sha: TSHA256;
  hash: TSHA256Digest;
begin
  JWT.result := jwtInvalidSignature;
  if length(signature) <> sizeof(TECCSignature) then
    exit;
  sha.Full(pointer(headpayload), length(headpayload), hash);
  if ecdsa_verify(fCertificate.Content.Signed.PublicKey, hash, PECCSignature(signature)^) then
    JWT.result := jwtValid;
end;

function TJWTES256.ComputeSignature(const headpayload: RawUTF8): RawUTF8;
var
  sha: TSHA256;
  hash: TSHA256Digest;
  sign: TECCSignature;
begin
  if not fCertificate.InheritsFrom(TECCCertificateSecret) or
     not TECCCertificateSecret(fCertificate).HasSecret then
    raise EECCException.CreateUTF8('%.ComputeSignature expects % (%) to hold ' +
      'a private key', [self, fCertificate, fCertificate.Serial]);
  sha.Full(pointer(headpayload), length(headpayload), hash);
  if not ecdsa_sign(TECCCertificateSecret(fCertificate).PrivateKey, hash, sign) then
    raise EECCException.CreateUTF8('%.ComputeSignature: ecdsa_sign?', [self]);
  result := BinToBase64URI(@sign, sizeof(sign));
end;


procedure InitializeUnit;
begin
  GetEnumNames(TypeInfo(TJWTResult), @_TJWTResult);
  GetEnumNames(TypeInfo(TJWTClaim), @_TJWTClaim);
end;

procedure FinalizeUnit;
begin
end;


initialization
  InitializeUnit;

finalization
  FinalizeUnit;
end.
