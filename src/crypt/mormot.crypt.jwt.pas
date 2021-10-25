/// Framework Core JSON Web Tokens (JWT) Support
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.crypt.jwt;

{
  *****************************************************************************

   JSON Web Tokens (JWT) Implementation - see RFC 7797
    - Abstract JWT Parsing and Computation
    - JWT Implementation of HS* and S3* Symmetric Algorithms
    - JWT Implementation of ES256 Asymmetric Algorithm

   Uses optimized mormot.crypt.core.pas and mormot.crypt.ecc for its process.
   See mormot.crypt.openssl.pas to support all other JWT algorithms.

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
  mormot.crypt.core,
  mormot.crypt.secure,
  mormot.crypt.ecc256r1,
  mormot.crypt.ecc;


{ **************** Abstract JWT Parsing and Computation }

type
  /// JWT Registered Claims, as defined in RFC 7519
  // - known registered claims have a specific name and behavior, and will be
  // handled automatically by TJwtAbstract
  // - corresponding field names are iss,sub,aud,exp,nbf,iat,jti - as defined
  // in JWT_CLAIMS_TEXT constant
  // - jrcIssuer identifies the server which originated the token, e.g.
  // "iss":"https://example.auth0.com/" when the token comes from Auth0 servers
  // - jrcSubject is the application-specific extent which is protected by this
  // JWT, e.g. an User or Resource ID, e.g. "sub":"auth0|57fe9f1bad961aa242870e"
  // - jrcAudience claims that the token is valid only for one or several
  // resource servers (may be a JSON string or a JSON array of strings), e.g.
  // "aud":["https://myshineyfileserver.sometld"] - TJwtAbstract will check
  // that the supplied "aud" field does match an expected list of identifiers
  // - jrcExpirationTime contains the Unix timestamp in seconds after which
  // the token must not be granted access, e.g. "exp":1477474667
  // - jrcNotBefore contains the Unix timestamp in seconds before which the
  // token must not be granted access, e.g. "nbf":147745438
  // - jrcIssuedAt contains the Unix timestamp in seconds when the token was
  // generated, e.g. "iat":1477438667
  // - jrcJwtID provides a unique identifier for the JWT, to prevent any replay;
  // TJwtAbstract.Compute will set an obfuscated TSynUniqueIdentifierGenerator
  // hexadecimal value stored as "jti" payload field
  // - jrcData is set when TJwtContent.data has been filled
  TJwtClaim = (
    jrcIssuer,
    jrcSubject,
    jrcAudience,
    jrcExpirationTime,
    jrcNotBefore,
    jrcIssuedAt,
    jrcJwtID,
    jrcData);

  /// set of JWT Registered Claims, as in TJwtAbstract.Claims
  TJwtClaims = set of TJwtClaim;

  /// Exception raised when running JSON Web Tokens
  EJwtException = class(ESynException);

  /// TJwtContent.result codes after TJwtAbstract.Verify method call
  TJwtResult = (
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

  //// set of TJwtContent.result codes
  TJwtResults = set of TJwtResult;

  /// JWT decoded content, as processed by TJwtAbstract
  // - optionally cached in memory
  TJwtContent = record
    /// store latest Verify() result
    result: TJwtResult;
    /// set of known/registered claims, as stored in the JWT payload
    claims: TJwtClaims;
    /// match TJwtAbstract.Audience[] indexes for reg[jrcAudience]
    audience: set of 0..15;
    /// known/registered claims UTF-8 values, as stored in the JWT payload
    // - e.g. reg[jrcSubject]='1234567890' and reg[jrcIssuer]='' for
    // $ {"sub": "1234567890","name": "John Doe","admin": true}
    // - jrcData claim is stored in the data TDocVariant field
    reg: array[low(TJwtClaim)..jrcJwtID] of RawUtf8;
    /// custom/unregistered claim values, as stored in the JWT payload
    // - registered claims will be available from reg[], not in this field
    // - e.g. data.U['name']='John Doe' and data.B['admin']=true for
    // $ {"sub": "1234567890","name": "John Doe","admin": true}
    // but data.U['sub'] if not defined, and reg[jrcSubject]='1234567890'
    data: TDocVariantData;
    /// match the jrcJwtID "jti" claim desobfuscated value
    id: TSynUniqueIdentifierBits;
  end;

  /// pointer to a JWT decoded content, as processed by TJwtAbstract
  PJwtContent = ^TJwtContent;

  /// used to store a list of JWT decoded content
  // - as used e.g. by TJwtAbstract cache
  TJwtContentDynArray = array of TJwtContent;

  /// available options for TJwtAbstract process
  TJwtOption = (
    joHeaderParse,
    joAllowUnexpectedClaims,
    joAllowUnexpectedAudience,
    joNoJwtIDGenerate,
    joNoJwtIDCheck,
    joDoubleInData);

  /// store options for TJwtAbstract process
  TJwtOptions = set of TJwtOption;

  /// abstract parent class for implementing JSON Web Tokens
  // - to represent claims securely between two parties, as defined in industry
  // standard @http://tools.ietf.org/html/rfc7519
  // - you should never use this abstract class directly, but e.g. TJwtHS256,
  // TJwtHS384, TJwtHS512 or TJwtES256 inherited classes
  // - for security reasons, one inherited class is implementing a single
  // algorithm, as is very likely to be the case on production: you pickup one
  // "alg", then you stick to it; if your server needs more than one algorithm
  // for compatibility reasons, use a separate key and URI - this design will
  // reduce attack surface, and fully avoid weaknesses as described in
  // @https://auth0.com/blog/critical-vulnerabilities-in-json-web-token-libraries
  // and @http://tools.ietf.org/html/rfc7518#section-8.5
  TJwtAbstract = class(TSynPersistent)
  protected
    fAlgorithm: RawUtf8;
    fHeader: RawUtf8;
    fHeaderB64: RawUtf8;
    fClaims: TJwtClaims;
    fOptions: TJwtOptions;
    fAudience: TRawUtf8DynArray;
    fExpirationSeconds: integer;
    fIDGen: TSynUniqueIdentifierGenerator;
    fCacheTimeoutSeconds: integer;
    fCacheResults: TJwtResults;
    fCache: TSynDictionary; // TRawUtf8DynArray/TJwtContentDynArray
    procedure SetCacheTimeoutSeconds(value: integer); virtual;
    function PayloadToJson(const DataNameValue: array of const;
      const Issuer, Subject, Audience: RawUtf8; NotBefore: TDateTime;
      ExpirationMinutes: cardinal): RawUtf8; virtual;
    procedure Parse(const Token: RawUtf8; var JWT: TJwtContent;
      out headpayload: RawUtf8; out signature: RawByteString;
      excluded: TJwtClaims); virtual;
    function CheckAgainstActualTimestamp(var JWT: TJwtContent): boolean;
    // abstract methods which should be overriden by inherited classes
    function ComputeSignature(const headpayload: RawUtf8): RawUtf8;
     virtual; abstract;
    procedure CheckSignature(const headpayload: RawUtf8;
      const signature: RawByteString; var JWT: TJwtContent); virtual; abstract;
  public
    /// initialize the JWT processing instance
    // - the supplied set of claims are expected to be defined in the JWT payload
    // - aAudience are the allowed values for the jrcAudience claim
    // - aExpirationMinutes is the deprecation time for the jrcExpirationTime claim
    // - aIDIdentifier and aIDObfuscationKey/aIDObfuscationKeyNewKdf are passed
    // to a TSynUniqueIdentifierGenerator instance used for jrcJwtID claim
    constructor Create(const aAlgorithm: RawUtf8; aClaims: TJwtClaims;
      const aAudience: array of RawUtf8; aExpirationMinutes: integer;
      aIDIdentifier: TSynUniqueIdentifierProcess; aIDObfuscationKey: RawUtf8;
      aIDObfuscationKeyNewKdf: integer = 0); reintroduce;
    /// finalize the instance
    destructor Destroy; override;
    /// compute a new JWT for a given payload
    // - here the data payload is supplied as Name,Value pairs - by convention,
    // some registered Names (see TJwtClaim) should not be used here, and private
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
      const Issuer: RawUtf8 = ''; const Subject: RawUtf8 = '';
      const Audience: RawUtf8 = ''; NotBefore: TDateTime = 0;
      ExpirationMinutes: integer = 0; Signature: PRawUtf8 = nil): RawUtf8;
    /// compute a HTTP Authorization header containing a JWT for a given payload
    // - just a wrapper around Compute(), returned the HTTP header value:
    // $ Authorization: <HttpAuthorizationHeader>
    // following the expected pattern:
    // $ Authorization: Bearer <Token>
    // - this method is thread-safe
    function ComputeAuthorizationHeader(const DataNameValue: array of const;
      const Issuer: RawUtf8 = ''; const Subject: RawUtf8 = '';
      const Audience: RawUtf8 = ''; NotBefore: TDateTime = 0;
      ExpirationMinutes: integer = 0): RawUtf8;
    /// check a JWT value, and its signature
    // - will validate all expected Claims (minus ExcludedClaims optional
    // parameter, jrcData being for JWT.data), and the associated signature
    // - verification state is returned in JWT.result (jwtValid for a valid JWT),
    // together with all parsed payload information
    // - supplied JWT is transmitted e.g. in HTTP header:
    // $ Authorization: Bearer <Token>
    // - this method is thread-safe
    procedure Verify(const Token: RawUtf8; out JWT: TJwtContent;
      ExcludedClaims: TJwtClaims = []); overload;
    /// check a JWT value, and its signature
    // - will validate all expected Claims, and the associated signature
    // - verification state is returned as function result
    // - supplied JWT is transmitted e.g. in HTTP header:
    // $ Authorization: Bearer <Token>
    // - this method is thread-safe
    function Verify(const Token: RawUtf8): TJwtResult; overload;
    /// check a HTTP Authorization header value as JWT, and its signature
    // - will validate all expected Claims, and the associated signature
    // - verification state is returned in JWT.result (jwtValid for a valid JWT),
    // together with all parsed payload information
    // - expect supplied HttpAuthorizationHeader as transmitted in HTTP header:
    // $ Authorization: <HttpAuthorizationHeader>
    // - this method is thread-safe
    function VerifyAuthorizationHeader(const HttpAuthorizationHeader: RawUtf8;
      out JWT: TJwtContent): boolean; overload;
    /// in-place decoding and quick check of the JWT header and paylod
    // - it won't check the signature, only the header and payload
    // - the header's algorithm is checked against ExpectedAlgo (if not '')
    // - it will parse the JWT payload and check for its expiration, and some
    // mandatory fied values - you can optionally retrieve the Expiration time,
    // the ending Signature, and/or the Payload decoded as TDocVariant
    // - NotBeforeDelta allows to define some time frame for the "nbf" field
    // - may be used on client side to quickly validate a JWT received from
    // server, without knowing the exact algorithm or secret keys
    class function VerifyPayload(const Token,
      ExpectedAlgo, ExpectedSubject, ExpectedIssuer, ExpectedAudience: RawUtf8;
      Expiration: PUnixTime = nil; Signature: PRawUtf8 = nil;
      Payload: PVariant = nil;
      IgnoreTime: boolean = false; NotBeforeDelta: TUnixTime = 15): TJwtResult;
  published
    /// the name of the algorithm used by this instance (e.g. 'HS256')
    property Algorithm: RawUtf8
      read fAlgorithm;
    /// allow to tune the Verify and Compute method process
    property Options: TJwtOptions
      read fOptions write fOptions;
    /// the JWT Registered Claims, as implemented by this instance
    // - Verify() method will ensure all claims are defined in the payload,
    // then fill TJwtContent.reg[] with all corresponding values
    property Claims: TJwtClaims
      read fClaims;
    /// the period, in seconds, for the "exp" claim
    property ExpirationSeconds: integer
      read fExpirationSeconds;
    /// the audience string values associated with this instance
    // - will be checked by Verify() method, and set in TJwtContent.audience
    property Audience: TRawUtf8DynArray
      read fAudience;
    /// delay of optional in-memory cache of Verify() TJwtContent
    // - equals 0 by default, i.e. cache is disabled
    // - may be useful if the signature process is very resource consumming
    // (e.g. for TJwtES256 or even HMAC-SHA-256) - see also CacheResults
    // - each time this property is assigned, internal cache content is flushed
    property CacheTimeoutSeconds: integer
      read fCacheTimeoutSeconds write SetCacheTimeoutSeconds;
    /// which TJwtContent.result should be stored in in-memory cache
    // - default is [jwtValid] but you may also include jwtInvalidSignature
    // if signature checking uses a lot of resources
    // - only used if CacheTimeoutSeconds>0
    property CacheResults: TJwtResults
      read fCacheResults write fCacheResults;
    /// access to the low-level generator associated with jrcJwtID "jti" claim
    property IDGen: TSynUniqueIdentifierGenerator
      read fIDGen;
  end;

  /// class-reference type (metaclass) of a JWT algorithm process
  TJwtAbstractClass = class of TJwtAbstract;

  /// implements JSON Web Tokens using 'none' algorithm
  // - as defined in @http://tools.ietf.org/html/rfc7518 paragraph 3.6
  // - you should never use this weak algorithm in production, unless your
  // communication is already secured by other means, and use JWT as cookies
  TJwtNone = class(TJwtAbstract)
  protected
    function ComputeSignature(const headpayload: RawUtf8): RawUtf8; override;
    procedure CheckSignature(const headpayload: RawUtf8;
      const signature: RawByteString; var JWT: TJwtContent); override;
  public
    /// initialize the JWT processing using the 'none' algorithm
    // - the supplied set of claims are expected to be defined in the JWT payload
    // - aAudience are the allowed values for the jrcAudience claim
    // - aExpirationMinutes is the deprecation time for the jrcExpirationTime claim
    // - aIDIdentifier and aIDObfuscationKey/aIDObfuscationKeyNewKdf are passed
    // to a TSynUniqueIdentifierGenerator instance used for jrcJwtID claim
    constructor Create(aClaims: TJwtClaims; const aAudience: array of RawUtf8;
      aExpirationMinutes: integer = 0;
      aIDIdentifier: TSynUniqueIdentifierProcess = 0;
      aIDObfuscationKey: RawUtf8 = '';
      aIDObfuscationKeyNewKdf: integer = 0); reintroduce;
  end;


const
  /// the text field names of the registerd claims, as defined by RFC 7519
  // - see TJwtClaim enumeration and TJwtClaims set
  // - RFC standard expects those to be case-sensitive
  JWT_CLAIMS_TEXT: array[TJwtClaim] of RawUtf8 = (
    'iss',    // jrcIssuer
    'sub',    // jrcSubject
    'aud',    // jrcAudience
    'exp',    // jrcExpirationTime
    'nbf',    // jrcNotBefore
    'iat',    // jrcIssuedAt
    'jti',    // jrcJwtID
    'data');  // jrcData


function ToText(res: TJwtResult): PShortString; overload;
function ToCaption(res: TJwtResult): string; overload;
function ToText(claim: TJwtClaim): PShortString; overload;
function ToText(claims: TJwtClaims): ShortString; overload;

/// try to recognize a JWT from a supplied text, which may be an URI
// - will ignore any trailing spaces, then extract any ending Base64-URI encoded
// text which matches the JWT 'algo.payload.sign' layout
// - returns '' if no JWT-like pattern was found
// - it won't validate the exact JWT format, nor any signature, only guess if
// there is a chance the supplied text contains a JWT, and extract it
function ParseTrailingJwt(const aText: RawUtf8; noDotCheck: boolean = false): RawUtf8;



{ **************** JWT Implementation of HS and S3 Algorithms }

type
  /// abstract parent of JSON Web Tokens using HMAC-SHA2 or SHA-3 algorithms
  // - SHA-3 is not yet officially defined in @http://tools.ietf.org/html/rfc7518
  // but could be used as a safer (and sometimes faster) alternative to HMAC-SHA2
  // - digital signature will be processed by an internal TSynSigner instance
  // - never use this abstract class, but any inherited class, or
  // JWT_CLASS[].Create to instantiate a JWT process from a given algorithm
  TJwtSynSignerAbstract = class(TJwtAbstract)
  protected
    fSignPrepared: TSynSigner;
    function GetAlgo: TSignAlgo; virtual; abstract;
    function ComputeSignature(const headpayload: RawUtf8): RawUtf8; override;
    procedure CheckSignature(const headpayload: RawUtf8;
      const signature: RawByteString; var JWT: TJwtContent); override;
  public
    /// initialize the JWT processing using SHA3 algorithm
    // - the supplied set of claims are expected to be defined in the JWT payload
    // - the supplied secret text will be used to compute the digital signature,
    // directly if aSecretPbkdf2Round=0, or via PBKDF2 iterative key derivation
    // if some number of rounds were specified
    // - aAudience are the allowed values for the jrcAudience claim
    // - aExpirationMinutes is the deprecation time for the jrcExpirationTime claim
    // - aIDIdentifier and aIDObfuscationKey/aIDObfuscationKeyNewKdf are passed
    // to a TSynUniqueIdentifierGenerator instance used for jrcJwtID claim
    // - optionally return the PBKDF2 derivated key for aSecretPbkdf2Round>0
    constructor Create(const aSecret: RawUtf8; aSecretPbkdf2Round: integer;
      aClaims: TJwtClaims; const aAudience: array of RawUtf8;
      aExpirationMinutes: integer = 0;
      aIDIdentifier: TSynUniqueIdentifierProcess = 0;
      aIDObfuscationKey: RawUtf8 = '';
      aIDObfuscationKeyNewKdf: integer = 0;
      aPBKDF2Secret: PHash512Rec = nil); reintroduce;
    /// finalize the instance
    destructor Destroy; override;
    /// low-level read access to the internal signature structure
    property SignPrepared: TSynSigner
      read fSignPrepared;
    {$ifndef ISDELPHI2009} // avoid Delphi 2009 F2084 Internal Error: DT5830
    /// the digital signature size, in byte
    property SignatureSize: integer
      read fSignPrepared.SignatureSize;
    /// the TSynSigner raw algorithm used for digital signature
    property SignatureAlgo: TSignAlgo
      read fSignPrepared.Algo;
    {$endif ISDELPHI2009}
  end;

  /// meta-class for TJwtSynSignerAbstract creations
  TJwtSynSignerAbstractClass = class of TJwtSynSignerAbstract;


type
  /// implements JSON Web Tokens using 'HS256' (HMAC SHA-256) algorithm
  // - as defined in @http://tools.ietf.org/html/rfc7518 paragraph 3.2
  // - our HMAC SHA-256 implementation used is thread safe, and very fast
  // (x86: 3us, x64: 2.5us) so cache is not needed
  // - resulting signature size will be of 256 bits
  TJwtHS256 = class(TJwtSynSignerAbstract)
  protected
    function GetAlgo: TSignAlgo; override;
  end;

  /// implements JSON Web Tokens using 'HS384' (HMAC SHA-384) algorithm
  // - as defined in @http://tools.ietf.org/html/rfc7518 paragraph 3.2
  // - our HMAC SHA-384 implementation used is thread safe, and very fast
  // even on x86 (if the CPU supports SSE3 opcodes)
  // - resulting signature size will be of 384 bits
  TJwtHS384 = class(TJwtSynSignerAbstract)
  protected
    function GetAlgo: TSignAlgo; override;
  end;

  /// implements JSON Web Tokens using 'HS512' (HMAC SHA-512) algorithm
  // - as defined in @http://tools.ietf.org/html/rfc7518 paragraph 3.2
  // - our HMAC SHA-512 implementation used is thread safe, and very fast
  // even on x86 (if the CPU supports SSE3 opcodes)
  // - resulting signature size will be of 512 bits
  TJwtHS512 = class(TJwtSynSignerAbstract)
  protected
    function GetAlgo: TSignAlgo; override;
  end;

  /// experimental JSON Web Tokens using SHA3-224 algorithm
  // - SHA-3 is not yet officially defined in @http://tools.ietf.org/html/rfc7518
  // but could be used as a safer (and sometimes faster) alternative to HMAC-SHA2
  // - resulting signature size will be of 224 bits
  TJwtS3224 = class(TJwtSynSignerAbstract)
  protected
    function GetAlgo: TSignAlgo; override;
  end;

  /// experimental JSON Web Tokens using SHA3-256 algorithm
  // - SHA-3 is not yet officially defined in @http://tools.ietf.org/html/rfc7518
  // but could be used as a safer (and sometimes faster) alternative to HMAC-SHA2
  // - resulting signature size will be of 256 bits
  TJwtS3256 = class(TJwtSynSignerAbstract)
  protected
    function GetAlgo: TSignAlgo; override;
  end;

  /// experimental JSON Web Tokens using SHA3-384 algorithm
  // - SHA-3 is not yet officially defined in @http://tools.ietf.org/html/rfc7518
  // but could be used as a safer (and sometimes faster) alternative to HMAC-SHA2
  // - resulting signature size will be of 384 bits
  TJwtS3384 = class(TJwtSynSignerAbstract)
  protected
    function GetAlgo: TSignAlgo; override;
  end;

  /// experimental JSON Web Tokens using SHA3-512 algorithm
  // - SHA-3 is not yet officially defined in @http://tools.ietf.org/html/rfc7518
  // but could be used as a safer (and sometimes faster) alternative to HMAC-SHA2
  // - resulting signature size will be of 512 bits
  TJwtS3512 = class(TJwtSynSignerAbstract)
  protected
    function GetAlgo: TSignAlgo; override;
  end;

  /// experimental JSON Web Tokens using SHA3-SHAKE128 algorithm
  // - SHA-3 is not yet officially defined in @http://tools.ietf.org/html/rfc7518
  // but could be used as a safer (and sometimes faster) alternative to HMAC-SHA2
  // - resulting signature size will be of 256 bits
  TJwtS3S128 = class(TJwtSynSignerAbstract)
  protected
    function GetAlgo: TSignAlgo; override;
  end;

  /// experimental JSON Web Tokens using SHA3-SHAKE256 algorithm
  // - SHA-3 is not yet officially defined in @http://tools.ietf.org/html/rfc7518
  // but could be used as a safer (and sometimes faster) alternative to HMAC-SHA2
  // - resulting signature size will be of 512 bits
  TJwtS3S256 = class(TJwtSynSignerAbstract)
  protected
    function GetAlgo: TSignAlgo; override;
  end;


const
  /// how TJwtSynSignerAbstract algorithms are identified in the JWT
  // - SHA-1 will fallback to HS256 (since there will never be SHA-1 support)
  // - SHA-3 is not yet officially defined in @http://tools.ietf.org/html/rfc7518
  JWT_TEXT: array[TSignAlgo] of RawUtf8 = (
    'HS256',
    'HS256',
    'HS384',
    'HS512',
    'S3224',
    'S3256',
    'S3384',
    'S3512',
    'S3S128',
    'S3S256');

  /// able to instantiate any of the TJwtSynSignerAbstract instance expected
  // - SHA-1 will fallback to TJwtHS256 (since SHA-1 will never be supported)
  // - SHA-3 is not yet officially defined in @http://tools.ietf.org/html/rfc7518
  // - typical use is the following:
  // ! result := JWT_CLASS[algo].Create(master, round, claims, [], expirationMinutes);
  JWT_CLASS: array[TSignAlgo] of TJwtSynSignerAbstractClass = (
    TJwtHS256,
    TJwtHS256,
    TJwtHS384,
    TJwtHS512,
    TJwtS3224,
    TJwtS3256,
    TJwtS3384,
    TJwtS3512,
    TJwtS3S128,
    TJwtS3S256);


{ **************  JWT Implementation of ES256 Algorithm }

type
  /// implements JSON Web Tokens using 'ES256' algorithm
  // - i.e. ECDSA using the P-256 curve and the SHA-256 hash algorithm
  // - as defined in http://tools.ietf.org/html/rfc7518 paragraph 3.4
  // - since ECDSA signature and verification is CPU consumming (especially
  // under x86) you may enable CacheTimeoutSeconds
  // - will use the OpenSSL library if available - about 5 times faster than
  // our pascal/asm code - here are some numbers on x86_64:
  //  TJwtES256 pascal:   100 ES256 in 33.57ms i.e. 2.9K/s, aver. 335us
  //  TJwtES256 OpenSSL:  100 ES256 in 6.90ms i.e. 14.1K/s, aver. 69us
  // - pre-compute the public key so is even faster than TJwtES256Osl:
  //    TJwtES256Osl:     100 ES256 in 9.56ms i.e. 10.2K/s, aver. 95us
  TJwtES256 = class(TJwtAbstract)
  protected
    fCertificate: TEccCertificate;
    fVerify: TEcc256r1VerifyAbstract; // faster pre-computed public key
    fOwnCertificate: boolean;
    function ComputeSignature(const headpayload: RawUtf8): RawUtf8; override;
    procedure CheckSignature(const headpayload: RawUtf8; const signature: RawByteString;
      var JWT: TJwtContent); override;
  public
    /// initialize the JWT processing instance using ECDSA P-256 algorithm
    // - the supplied set of claims are expected to be defined in the JWT payload
    // - the supplied ECC certificate should be a TEccCertificate storing the
    // public key needed for Verify(), or a TEccCertificateSecret storing also
    // the private key required by Compute()
    // - aCertificate is owned by this instance if property OwnCertificate is true
    // - aAudience are the allowed values for the jrcAudience claim
    // - aExpirationMinutes is the deprecation time for the jrcExpirationTime claim
    // - aIDIdentifier and aIDObfuscationKey/aIDObfuscationKeyNewKdf are passed
    // to a TSynUniqueIdentifierGenerator instance used for jrcJwtID claim
    constructor Create(aCertificate: TEccCertificate; aClaims: TJwtClaims;
      const aAudience: array of RawUtf8; aExpirationMinutes: integer = 0;
      aIDIdentifier: TSynUniqueIdentifierProcess = 0;
      aIDObfuscationKey: RawUtf8 = '';
      aIDObfuscationKeyNewKdf: integer = 0); reintroduce;
    /// finalize the instance
    destructor Destroy; override;
    /// access to the associated TEccCertificate instance
    // - which may be a TEccCertificateSecret for Compute() private key
    property Certificate: TEccCertificate
      read fCertificate;
    /// if the associated TEccCertificate is to be owned by this instance
    property OwnCertificate: boolean
      read fOwnCertificate write fOwnCertificate;
  end;



implementation


{ **************** Abstract JWT Parsing and Computation }

var
  _TJwtResult: array[TJwtResult] of PShortString;
  _TJwtClaim: array[TJwtClaim] of PShortString;

function ToText(res: TJwtResult): PShortString;
begin
  result := _TJwtResult[res];
end;

function ToCaption(res: TJwtResult): string;
begin
  GetCaptionFromTrimmed(_TJwtResult[res], result);
end;

function ToText(claim: TJwtClaim): PShortString;
begin
  result := _TJwtClaim[claim];
end;

function ToText(claims: TJwtClaims): ShortString;
begin
  GetSetNameShort(TypeInfo(TJwtClaims), claims, result);
end;

function ParseTrailingJwt(const aText: RawUtf8; noDotCheck: boolean): RawUtf8;
var
  txtlen, beg, dotcount: PtrInt;
  tc: PTextCharSet;
begin
  result := ''; // no JWT found
  txtlen := length(aText);
  while (txtlen > 10) and
        (aText[txtlen] <= ' ') do // trim right
    dec(txtlen);
  beg := txtlen + 1;
  dotcount := 0;
  tc := @TEXT_CHARS;
  while (beg > 1) and
        (tcURIUnreserved in tc[aText[beg - 1]]) do // search backward end of JWT
  begin
    dec(beg);
    if aText[beg] = '.' then
      inc(dotcount);
  end;
  dec(txtlen, beg - 1);
  if not noDotCheck then
    if (dotcount <> 2) or
       (txtlen <= 10) then
      exit;
  result := copy(aText, beg, txtlen); // trim base64 encoded part
end;


{ TJwtAbstract }

constructor TJwtAbstract.Create(const aAlgorithm: RawUtf8; aClaims: TJwtClaims;
  const aAudience: array of RawUtf8; aExpirationMinutes: integer;
  aIDIdentifier: TSynUniqueIdentifierProcess; aIDObfuscationKey: RawUtf8;
  aIDObfuscationKeyNewKdf: integer);
begin
  if aAlgorithm = '' then
    raise EJwtException.CreateUtf8('%.Create(algo?)', [self]);
  inherited Create;
  if high(aAudience) >= 0 then
  begin
    fAudience := TRawUtf8DynArrayFrom(aAudience);
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
    fIDGen := TSynUniqueIdentifierGenerator.Create(
      aIDIdentifier, aIDObfuscationKey, aIDObfuscationKeyNewKdf);
  if fHeader = '' then
    FormatUtf8('{"alg":"%","typ":"JWT"}', [aAlgorithm], fHeader);
  fHeaderB64 := BinToBase64Uri(fHeader) + '.';
  fCacheResults := [jwtValid];
end;

destructor TJwtAbstract.Destroy;
begin
  fIDGen.Free;
  fCache.Free;
  inherited;
end;

const
  JWT_MAXSIZE = 4096; // coherent with HTTP headers limitations

function TJwtAbstract.Compute(const DataNameValue: array of const;
  const Issuer, Subject, Audience: RawUtf8; NotBefore: TDateTime;
  ExpirationMinutes: integer; Signature: PRawUtf8): RawUtf8;
var
  payload, headpayload, signat: RawUtf8;
begin
  result := '';
  if self = nil then
    exit;
  payload := PayloadToJson(DataNameValue, Issuer, Subject, Audience,
    NotBefore, ExpirationMinutes);
  headpayload := fHeaderB64 + BinToBase64Uri(payload);
  signat := ComputeSignature(headpayload);
  result := headpayload + '.' + signat;
  if length(result) > JWT_MAXSIZE then
    raise EJwtException.CreateUtf8('%.Compute oversize: len=%',
      [self, length(result)]);
  if Signature <> nil then
    Signature^ := signat;
end;

function TJwtAbstract.ComputeAuthorizationHeader(
  const DataNameValue: array of const; const Issuer, Subject, Audience: RawUtf8;
  NotBefore: TDateTime; ExpirationMinutes: integer): RawUtf8;
begin
  if self = nil then
    result := ''
  else
    result := 'Bearer ' + Compute(DataNameValue, Issuer, Subject, Audience,
      NotBefore, ExpirationMinutes);
end;

function TJwtAbstract.PayloadToJson(const DataNameValue: array of const;
  const Issuer, Subject, Audience: RawUtf8; NotBefore: TDateTime;
  ExpirationMinutes: cardinal): RawUtf8;

  procedure RaiseMissing(c: TJwtClaim);
  begin
    raise EJwtException.CreateUtf8('%.PayloadToJson: missing % (''%'')',
      [self, _TJwtClaim[c]^, JWT_CLAIMS_TEXT[c]]);
  end;

var
  payload: TDocVariantData;
begin
  result := '';
  payload.InitObject(DataNameValue, JSON_FAST);
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
      payload.AddOrUpdateValue(JWT_CLAIMS_TEXT[jrcNotBefore], UnixTimeUtc)
    else
      payload.AddOrUpdateValue(JWT_CLAIMS_TEXT[jrcNotBefore], DateTimeToUnixTime(NotBefore));
  if jrcIssuedAt in fClaims then
    payload.AddOrUpdateValue(JWT_CLAIMS_TEXT[jrcIssuedAt], UnixTimeUtc);
  if jrcExpirationTime in fClaims then
  begin
    if ExpirationMinutes = 0 then
      ExpirationMinutes := fExpirationSeconds
    else
      ExpirationMinutes := ExpirationMinutes * 60;
    payload.AddOrUpdateValue(JWT_CLAIMS_TEXT[jrcExpirationTime],
      UnixTimeUtc + ExpirationMinutes);
  end;
  if jrcJwtID in fClaims then
    if joNoJwtIDGenerate in fOptions then
    begin
      if payload.GetValueIndex(JWT_CLAIMS_TEXT[jrcJwtID]) < 0 then
        exit; // not generated, but should be supplied
    end
    else
      payload.AddValue(JWT_CLAIMS_TEXT[jrcJwtID],
        RawUtf8ToVariant(fIDGen.ToObfuscated(fIDGen.ComputeNew)));
  result := payload.ToJson;
end;

procedure TJwtAbstract.SetCacheTimeoutSeconds(value: integer);
begin
  fCacheTimeoutSeconds := value;
  FreeAndNil(fCache);
  if (value > 0) and
     (fCacheResults <> []) then
    fCache := TSynDictionary.Create(
      TypeInfo(TRawUtf8DynArray), TypeInfo(TJwtContentDynArray), false, value);
end;

procedure TJwtAbstract.Verify(const Token: RawUtf8; out JWT: TJwtContent;
  ExcludedClaims: TJwtClaims);
var
  headpayload: RawUtf8;
  signature: RawByteString;
  fromcache: boolean;
begin
  JWT.result := jwtNoToken;
  if Token = '' then
    exit;
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

function TJwtAbstract.Verify(const Token: RawUtf8): TJwtResult;
var
  jwt: TJwtContent;
begin
  Verify(Token, jwt, [jrcData]); // we won't use jwt.data for sure
  result := jwt.result;
end;

function TJwtAbstract.CheckAgainstActualTimestamp(var JWT: TJwtContent): boolean;
var
  nowunix, unix: cardinal;
begin
  if [jrcExpirationTime, jrcNotBefore, jrcIssuedAt] * JWT.claims <> [] then
  begin
    result := false;
    nowunix := UnixTimeUtc; // validate against actual timestamp
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

procedure TJwtAbstract.Parse(const Token: RawUtf8; var JWT: TJwtContent;
  out headpayload: RawUtf8; out signature: RawByteString; excluded: TJwtClaims);
var
  payloadend, j, toklen, c, cap, headerlen, Nlen, VLen, a: integer;
  P: PUtf8Char;
  N, V: PUtf8Char;
  wasString: boolean;
  EndOfObject: AnsiChar;
  claim: TJwtClaim;
  requiredclaims: TJwtClaims;
  value: variant;
  head: array[0..1] of TValuePUtf8Char;
  aud: TDocVariantData;
  tok: PAnsiChar absolute Token;
  temp: TSynTempBuffer;
begin
  // 0. initialize parsing
  JWT.result := jwtNoToken;
  byte(JWT.claims) := 0;
  word(JWT.audience) := 0;
  Finalize(JWT.reg);
  JWT.data.InitFast; // custom claims
  JWT.id.Value := 0;
  toklen := length(Token);
  if (toklen = 0) or
     (self = nil) then
    exit;
  // 1. validate the header (including algorithm "alg" verification)
  JWT.result := jwtInvalidAlgorithm;
  if joHeaderParse in fOptions then
  begin
    // (slightly) slower parsing
    headerlen := PosExChar('.', Token);
    if (headerlen = 0) or
       (headerlen > 512) then
      exit;
    if not Base64UriToBin(tok, headerlen - 1, temp) or
       (JsonDecode(temp.buf, ['alg', // 0
                              'typ'  // 1
                             ], @head) = nil) or
       not {%H-}head[0].Idem(fAlgorithm) or
       ((head[1].Value <> nil) and
        not head[1].Idem('JWT')) then
      headerlen := 0;
    temp.Done;
    if headerlen = 0 then
      exit;
  end
  else
  begin
    // fast direct compare of fHeaderB64 (including "alg")
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
  Base64UriToBin(tok + payloadend, toklen - payloadend, signature);
  if (signature = '') and
     (payloadend <> toklen) then
    exit;
  JWT.result := jwtInvalidPayload;
  try
    if not Base64UriToBin(tok + headerlen, payloadend - headerlen - 1, temp) then
      exit;
    // 3. decode payload into JWT.reg[]/JWT.claims (known) and JWT.data (custom)
    P := GotoNextNotSpace(temp.buf);
    if P^ <> '{' then
      exit;
    P := GotoNextNotSpace(P + 1);
    if P^ = '}' then
      cap := 0
    else
    begin
      cap := JsonObjectPropCount(P); // fast pre-parsing
      if cap = 0 then
        exit; // invalid input
    end;
    requiredclaims := fClaims - excluded;
    if cap > 0 then
      repeat
        N := GetJsonPropName(P, @Nlen);
        if N = nil then
          exit;
        V := GetJsonFieldOrObjectOrArray(
          P, @wasString, @EndOfObject, true, true, @Vlen);
        if V = nil then
          exit;
        if Nlen = 3 then
        begin
          c := PInteger(N)^;
          for claim := low(JWT.reg) to high(JWT.reg) do
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
              FastSetString(JWT.reg[claim], V, VLen);
              if claim in requiredclaims then
                case claim of
                  jrcJwtID:
                    if not (joNoJwtIDCheck in fOptions) then
                      if not fIDGen.FromObfuscated(JWT.reg[jrcJwtID], JWT.id.Value) or
                         (JWT.id.CreateTimeUnix < UNIXTIME_MINIMAL) then
                      begin
                        JWT.result := jwtInvalidID;
                        exit;
                      end;
                  jrcAudience:
                    if JWT.reg[jrcAudience][1] = '[' then
                    begin
                      aud.InitJson(JWT.reg[jrcAudience], JSON_FAST);
                      if aud.Count = 0 then
                        exit;
                      for j := 0 to aud.Count - 1 do
                      begin
                        a := FindRawUtf8(fAudience, VariantToUtf8(aud.Values[j]));
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
                      a := FindRawUtf8(fAudience, JWT.reg[jrcAudience]);
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
              Nlen := 0; // don't add to JWT.data
              dec(cap);
              break;
            end;
          if Nlen = 0 then
            continue;
        end;
        if jrcData in excluded then
          continue; // caller didn't want to fill JWT.data
        include(JWT.claims, jrcData);
        GetVariantFromJsonField(V, wasString, value, @JSON_[mFast],
          joDoubleInData in fOptions, VLen);
        if JWT.data.Count = 0 then
          JWT.data.Capacity := cap;
        JWT.data.AddValue(N, Nlen, value)
      until EndOfObject = '}';
    if requiredclaims - JWT.claims <> [] then
      JWT.result := jwtMissingClaim
    else
    begin
      FastSetString(headpayload, tok, payloadend - 1);
      JWT.result := jwtValid;
    end;
  finally
    temp.Done;
  end;
end;

function TJwtAbstract.VerifyAuthorizationHeader(
  const HttpAuthorizationHeader: RawUtf8; out JWT: TJwtContent): boolean;
begin
  if (cardinal(length(HttpAuthorizationHeader) - 10) > JWT_MAXSIZE) or
     not IdemPChar(pointer(HttpAuthorizationHeader), 'BEARER ') then
    JWT.result := jwtWrongFormat
  else
    Verify(copy(HttpAuthorizationHeader, 8, maxInt), JWT);
  result := JWT.result = jwtValid;
end;

class function TJwtAbstract.VerifyPayload(const Token,
  ExpectedAlgo, ExpectedSubject, ExpectedIssuer, ExpectedAudience: RawUtf8;
  Expiration: PUnixTime; Signature: PRawUtf8; Payload: PVariant;
  IgnoreTime: boolean; NotBeforeDelta: TUnixTime): TJwtResult;
var
  P, B: PUtf8Char;
  V: array[0..4] of TValuePUtf8Char;
  now, time: PtrUInt;
  temp, temp2: TSynTempBuffer;
begin
  result := jwtInvalidAlgorithm;
  P := PosChar(pointer(Token), '.');
  if P = nil then
    exit;
  if self <> TJwtAbstract then
  begin
    B := pointer(Token);
    if not Base64UriToBin(PAnsiChar(B), P - B, temp) or
       (JsonDecode(temp.buf, ['alg'], @V, false) = nil) or
       ((ExpectedAlgo <> '') and
        not IdemPropNameU(ExpectedAlgo, {%H-}V[0].Value, {%H-}V[0].ValueLen)) then
      B := nil;
    temp.Done;
    if B = nil then
      exit;
  end;
  B := P + 1;
  P := PosChar(B, '.');
  result := jwtInvalidSignature;
  if P = nil then
    exit;
  result := jwtInvalidPayload;
  if not Base64UriToBin(PAnsiChar(B), P - B, temp) then
  begin
    temp.Done;
    exit;
  end;
  if Payload <> nil then
  begin
    VarClear(PayLoad^);
    temp2.Init(temp.buf, temp.len); // its own copy for in-place parsing
    PDocVariantData(PayLoad)^.InitJsonInPlace(temp2.buf, JSON_FAST);
    temp2.Done;
  end;
  repeat // avoid try..finally for temp.Done
    if JsonDecode(temp.buf, ['iss', // 0
                             'aud', // 1
                             'exp', // 2
                             'nbf', // 3
                             'sub'  // 4
                            ], @V, true) = nil then
      break;
    result := jwtUnexpectedClaim;
    if ((ExpectedSubject <> '') and
        not V[4].Idem(ExpectedSubject)) or
       ((ExpectedIssuer <> '') and
        not V[0].Idem(ExpectedIssuer)) then
      break;
    result := jwtUnknownAudience;
    if (ExpectedAudience <> '') and
       not V[1].Idem(ExpectedAudience) then
      break;
    if Expiration <> nil then
      Expiration^ := 0;
    if (V[2].value <> nil) or
       (V[3].value <> nil) then
    begin
      now := UnixTimeUtc;
      if V[2].value <> nil then
      begin
        time := V[2].ToCardinal;
        result := jwtExpired;
        if not IgnoreTime and
           (now > time) then
          break;
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
          break;
      end;
    end;
    inc(P);
    if Signature <> nil then
      FastSetString(Signature^, P, StrLen(P));
    result := jwtValid;
    break;
  until false;
  temp.Done;
end;


{ TJwtNone }

constructor TJwtNone.Create(aClaims: TJwtClaims;
  const aAudience: array of RawUtf8; aExpirationMinutes: integer;
  aIDIdentifier: TSynUniqueIdentifierProcess; aIDObfuscationKey: RawUtf8;
  aIDObfuscationKeyNewKdf: integer);
begin
  fHeader := '{"alg":"none"}'; // "typ":"JWT" is optional, so we save a few bytes
  inherited Create('none', aClaims, aAudience, aExpirationMinutes,
    aIDIdentifier, aIDObfuscationKey, aIDObfuscationKeyNewKdf);
end;

procedure TJwtNone.CheckSignature(const headpayload: RawUtf8;
  const signature: RawByteString; var JWT: TJwtContent);
begin
  if signature = '' then // JWA defined empty string for "none" JWS
    JWT.result := jwtValid
  else
    JWT.result := jwtInvalidSignature;
end;

function TJwtNone.ComputeSignature(const headpayload: RawUtf8): RawUtf8;
begin
  result := '';
end;



{ **************** JWT Implementation of HS* and S3* Algorithms }

{ TJwtSynSignerAbstract }

constructor TJwtSynSignerAbstract.Create(const aSecret: RawUtf8;
  aSecretPbkdf2Round: integer; aClaims: TJwtClaims;
  const aAudience: array of RawUtf8; aExpirationMinutes: integer;
  aIDIdentifier: TSynUniqueIdentifierProcess; aIDObfuscationKey: RawUtf8;
  aIDObfuscationKeyNewKdf: integer; aPBKDF2Secret: PHash512Rec);
var
  algo: TSignAlgo;
begin
  algo := GetAlgo;
  inherited Create(JWT_TEXT[algo], aClaims, aAudience, aExpirationMinutes,
    aIDIdentifier, aIDObfuscationKey, aIDObfuscationKeyNewKdf);
  if (aSecret <> '') and
     (aSecretPbkdf2Round > 0) then
    fSignPrepared.Init(algo, aSecret, fHeaderB64, aSecretPbkdf2Round, aPBKDF2Secret)
  else
    fSignPrepared.Init(algo, aSecret);
end;

procedure TJwtSynSignerAbstract.CheckSignature(const headpayload: RawUtf8;
  const signature: RawByteString; var JWT: TJwtContent);
var
  signer: TSynSigner;
  temp: THash512Rec;
begin
  JWT.result := jwtInvalidSignature;
  if length(signature) <> fSignPrepared.SignatureSize then
    exit;
  signer := fSignPrepared; // thread-safe re-use of prepared TSynSigner
  signer.Update(pointer(headpayload), length(headpayload));
  signer.Final(temp);
{  writeln('payload=',headpayload);
   writeln('sign=',bintohex(@temp,SignatureSize));
   writeln('expected=',bintohex(pointer(signature),SignatureSize)); }
  if CompareMem(@temp, pointer(signature), fSignPrepared.SignatureSize) then
    JWT.result := jwtValid;
end;

function TJwtSynSignerAbstract.ComputeSignature(
  const headpayload: RawUtf8): RawUtf8;
var
  signer: TSynSigner;
  temp: THash512Rec;
begin
  signer := fSignPrepared;
  signer.Update(pointer(headpayload), length(headpayload));
  signer.Final(temp);
  result := BinToBase64Uri(@temp, fSignPrepared.SignatureSize);
end;

destructor TJwtSynSignerAbstract.Destroy;
begin
  fSignPrepared.Done;
  inherited Destroy;
end;


{ TJwtHS256 }

function TJwtHS256.GetAlgo: TSignAlgo;
begin
  result := saSha256;
end;

{ TJwtHS384 }

function TJwtHS384.GetAlgo: TSignAlgo;
begin
  result := saSha384;
end;

{ TJwtHS512 }

function TJwtHS512.GetAlgo: TSignAlgo;
begin
  result := saSha512;
end;

{ TJwtS3224 }

function TJwtS3224.GetAlgo: TSignAlgo;
begin
  result := saSha3224;
end;

{ TJwtS3256 }

function TJwtS3256.GetAlgo: TSignAlgo;
begin
  result := saSha3256;
end;

{ TJwtS3384 }

function TJwtS3384.GetAlgo: TSignAlgo;
begin
  result := saSha3384;
end;

{ TJwtS3512 }

function TJwtS3512.GetAlgo: TSignAlgo;
begin
  result := saSha3512;
end;

{ TJwtS3S128 }

function TJwtS3S128.GetAlgo: TSignAlgo;
begin
  result := saSha3S128;
end;

{ TJwtS3S256 }

function TJwtS3S256.GetAlgo: TSignAlgo;
begin
  result := saSha3S256;
end;



{ **************  JWT Implementation of ES256 Algorithm }

{ TJwtES256 }

constructor TJwtES256.Create(aCertificate: TEccCertificate; aClaims: TJwtClaims;
  const aAudience: array of RawUtf8; aExpirationMinutes: integer;
  aIDIdentifier: TSynUniqueIdentifierProcess; aIDObfuscationKey: RawUtf8;
  aIDObfuscationKeyNewKdf: integer);
begin
  if not aCertificate.CheckCRC then
    raise EJwtException.CreateUtf8('%.Create(aCertificate?)', [self]);
  inherited Create('ES256', aClaims, aAudience, aExpirationMinutes,
    aIDIdentifier, aIDObfuscationKey, aIDObfuscationKeyNewKdf);
  fCertificate := aCertificate;
  fVerify := TEcc256r1Verify.Create(fCertificate.Content.Signed.PublicKey);
end;

destructor TJwtES256.Destroy;
begin
  if fOwnCertificate then
    fCertificate.Free;
  fVerify.Free;
  inherited;
end;

procedure TJwtES256.CheckSignature(const headpayload: RawUtf8;
  const signature: RawByteString; var JWT: TJwtContent);
var
  sha: TSha256;
  hash: TSha256Digest;
begin
  JWT.result := jwtInvalidSignature;
  if length(signature) <> SizeOf(TEccSignature) then
    exit;
  sha.Full(pointer(headpayload), length(headpayload), hash);
  if fVerify.Verify(hash, PEccSignature(signature)^) then
    JWT.result := jwtValid;
end;

function TJwtES256.ComputeSignature(const headpayload: RawUtf8): RawUtf8;
var
  sha: TSha256;
  hash: TSha256Digest;
  sign: TEccSignature;
begin
  if not fCertificate.InheritsFrom(TEccCertificateSecret) or
     not TEccCertificateSecret(fCertificate).HasSecret then
    raise EECCException.CreateUtf8('%.ComputeSignature expects % (%) to hold ' +
      'a private key', [self, fCertificate, fCertificate.Serial]);
  sha.Full(pointer(headpayload), length(headpayload), hash);
  if not Ecc256r1Sign(TEccCertificateSecret(fCertificate).PrivateKey, hash, sign) then
    raise EECCException.CreateUtf8('%.ComputeSignature: ecdsa_sign?', [self]);
  result := BinToBase64Uri(@sign, SizeOf(sign));
end;


procedure InitializeUnit;
begin
  GetEnumNames(TypeInfo(TJwtResult), @_TJwtResult);
  GetEnumNames(TypeInfo(TJwtClaim), @_TJwtClaim);
end;


initialization
  InitializeUnit;
  
end.
