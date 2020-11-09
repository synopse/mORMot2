/// Framework Core Authentication and Security Features
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.secure;

{
  *****************************************************************************

   Authentication and Security types shared by all framework units.
    - TSyn***Password and TSynConnectionDefinition Classes
    - Reusable Authentication Classes
    - High-Level TSynSigner/TSynHasher Multi-Algorithm Wrappers
    - 64-bit TSynUniqueIdentifier and its efficient Generator
    - IProtocol Safe Communication with Unilateral or Mutual Authentication

   Uses optimized mormot.core.crypto.pas for its process.

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.buffers,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.crypto;


{ ***************** TSyn***Password and TSynConnectionDefinition Classes }

type
  /// abstract TSynPersistent class allowing safe storage of a password
  // - the associated Password, e.g. for storage or transmission encryption
  // will be persisted encrypted with a private key (which can be customized)
  // - if default simple symmetric encryption is not enough, it will also
  // read passwords strongly obfuscated for a given user using mormot.core.crypto
  // CryptDataForCurrentUser()
  // - a published property should be defined as such in inherited class:
  // ! property PasswordPropertyName: RawUTF8 read fPassword write fPassword;
  // - use the PassWordPlain property to access to its uncyphered value
  TSynPersistentWithPassword = class(TSynPersistent)
  protected
    fPassWord: SPIUTF8;
    fKey: cardinal;
    function GetKey: cardinal;
      {$ifdef HASINLINE}inline;{$endif}
    function GetPassWordPlain: SPIUTF8;
    function GetPassWordPlainInternal(AppSecret: RawUTF8): SPIUTF8;
    procedure SetPassWordPlain(const Value: SPIUTF8);
  public
    /// finalize the instance
    destructor Destroy; override;
    /// this class method could be used to compute the encrypted password,
    // ready to be stored as JSON, according to a given private key
    class function ComputePassword(const PlainPassword: SPIUTF8;
      CustomKey: cardinal = 0): SPIUTF8; overload;
    /// this class method could be used to compute the encrypted password from
    // a binary digest, ready to be stored as JSON, according to a given private key
    // - just a wrapper around ComputePassword(BinToBase64URI())
    class function ComputePassword(PlainPassword: pointer; PlainPasswordLen: integer;
      CustomKey: cardinal = 0): SPIUTF8; overload;
    /// this class method could be used to decrypt a password, stored as JSON,
    // according to a given private key
    // - may trigger a ESynException if the password was stored using hardened
    // CryptDataForCurrentUser, and the current user doesn't match the
    // expected user stored in the field
    class function ComputePlainPassword(const CypheredPassword: SPIUTF8;
      CustomKey: cardinal = 0; const AppSecret: RawUTF8 = ''): SPIUTF8;
    /// the private key used to cypher the password storage on serialization
    // - application can override the default 0 value at runtime
    property Key: cardinal read GetKey write fKey;
    /// access to the associated unencrypted Password value
    // - may trigger a ESynException if the password was stored using hardened
    // CryptDataForCurrentUser, and the current user doesn't match the
    // expected user stored in the field
    property PasswordPlain: SPIUTF8 read GetPassWordPlain write SetPassWordPlain;
  end;

type
  /// could be used to store a credential pair, as user name and password
  // - password will be stored with TSynPersistentWithPassword encryption
  TSynUserPassword = class(TSynPersistentWithPassword)
  protected
    fUserName: RawUTF8;
  published
    /// the associated user name
    property UserName: RawUTF8 read fUserName write fUserName;
    /// the associated encrypted password
    // - use the PasswordPlain public property to access to the uncrypted password
    property Password: SPIUTF8 read fPassword write fPassword;
  end;

  /// handle safe storage of any connection properties
  // - would be used by mormot.db to serialize TSQLDBConnectionProperties, or
  // by mormot.rest.core.pas to serialize TRest instances
  // - the password will be stored as Base64, after a simple encryption as
  // defined by TSynPersistentWithPassword
  // - typical content could be:
  // $ {
  // $	"Kind": "TSQLDBSQLite3ConnectionProperties",
  // $	"ServerName": "server",
  // $	"DatabaseName": "",
  // $	"User": "",
  // $	"Password": "PtvlPA=="
  // $ }
  // - the "Kind" value will be used to let the corresponding TRest or
  // TSQLDBConnectionProperties NewInstance*() class methods create the
  // actual instance, from its class name
  TSynConnectionDefinition = class(TSynPersistentWithPassword)
  protected
    fKind: string;
    fServerName: RawUTF8;
    fDatabaseName: RawUTF8;
    fUser: RawUTF8;
  public
    /// unserialize the database definition from JSON
    // - as previously serialized with the SaveToJSON method
    // - you can specify a custom Key used for password encryption, if the
    // default value is not safe enough for you
    constructor CreateFromJSON(const JSON: RawUTF8; Key: cardinal = 0); virtual;
    /// serialize the database definition as JSON
    function SaveToJSON: RawUTF8; virtual;
  published
    /// the class name implementing the connection or TRest instance
    // - will be used to instantiate the expected class type
    property Kind: string read fKind write fKind;
    /// the associated server name (or file, for SQLite3) to be connected to
    property ServerName: RawUTF8 read fServerName write fServerName;
    /// the associated database name (if any), or additional options
    property DatabaseName: RawUTF8 read fDatabaseName write fDatabaseName;
    /// the associated User Identifier (if any)
    property User: RawUTF8 read fUser write fUser;
    /// the associated Password, e.g. for storage or transmission encryption
    // - will be persisted encrypted with a private key
    // - use the PassWordPlain property to access to its uncyphered value
    property Password: SPIUTF8 read fPassword write fPassword;
  end;


/// naive symmetric encryption scheme using a 32-bit key
// - used e.g. by TSynPersistentWithPassword and mormot.db.proxy for password or
// content obfuscation
// - fast, but not cryptographically secure, since uses crc32ctab[] content as fixed
// xor table: consider using mormot.core.crypto proven AES-based algorithms instead
procedure SymmetricEncrypt(key: cardinal; var data: RawByteString);



{ ***************** Reusable Authentication Classes }

type
  /// class-reference type (metaclass) of an authentication class
  TSynAuthenticationClass = class of TSynAuthenticationAbstract;

  /// abstract authentication class, implementing safe token/challenge security
  // and a list of active sessions
  // - do not use this class, but plain TSynAuthentication
  TSynAuthenticationAbstract = class
  protected
    fSessions: TIntegerDynArray;
    fSessionsCount: integer;
    fSessionGenerator: integer;
    fTokenSeed: Int64;
    fSafe: TSynLocker;
    function ComputeCredential(previous: boolean;
      const UserName, PassWord: RawUTF8): cardinal; virtual;
    function GetPassword(const UserName: RawUTF8;
      out Password: RawUTF8): boolean; virtual; abstract;
    function GetUsersCount: integer; virtual; abstract;
    // check the given Hash challenge, against stored credentials
    function CheckCredentials(const UserName: RaWUTF8; Hash: cardinal): boolean; virtual;
  public
    /// initialize the authentication scheme
    constructor Create;
    /// finalize the authentation
    destructor Destroy; override;
    /// register one credential for a given user
    // - this abstract method will raise an exception: inherited classes should
    // implement them as expected
    procedure AuthenticateUser(const aName, aPassword: RawUTF8); virtual;
    /// unregister one credential for a given user
    // - this abstract method will raise an exception: inherited classes should
    // implement them as expected
    procedure DisauthenticateUser(const aName: RawUTF8); virtual;
    /// create a new session
    // - should return 0 on authentication error, or an integer session ID
    // - this method will check the User name and password, and create a new session
    function CreateSession(const User: RawUTF8; Hash: cardinal): integer; virtual;
    /// check if the session exists in the internal list
    function SessionExists(aID: integer): boolean;
    /// delete a session
    procedure RemoveSession(aID: integer);
    /// returns the current identification token
    // - to be sent to the client for its authentication challenge
    function CurrentToken: Int64;
    /// the number of current opened sessions
    property SessionsCount: integer read fSessionsCount;
    /// the number of registered users
    property UsersCount: integer read GetUsersCount;
    /// to be used to compute a Hash on the client sude, for a given Token
    // - the token should have been retrieved from the server, and the client
    // should compute and return this hash value, to perform the authentication
    // challenge and create the session
    // - internal algorithm is not cryptographic secure, but fast and safe
    class function ComputeHash(Token: Int64;
      const UserName, PassWord: RawUTF8): cardinal; virtual;
  end;

  /// simple authentication class, implementing safe token/challenge security
  // - maintain a list of user / name credential pairs, and a list of sessions
  // - is not meant to handle authorization, just plain user access validation
  // - used e.g. by TSQLDBConnection.RemoteProcessMessage (on server side) and
  // TSQLDBProxyConnectionPropertiesAbstract (on client side) in mormot.db.proxy
  TSynAuthentication = class(TSynAuthenticationAbstract)
  protected
    fCredentials: TSynNameValue; // store user/password pairs
    function GetPassword(const UserName: RawUTF8;
      out Password: RawUTF8): boolean; override;
    function GetUsersCount: integer; override;
  public
    /// initialize the authentication scheme
    // - you can optionally register one user credential
    constructor Create(const aUserName: RawUTF8 = '';
      const aPassword: RawUTF8 = ''); reintroduce;
    /// register one credential for a given user
    procedure AuthenticateUser(const aName, aPassword: RawUTF8); override;
    /// unregister one credential for a given user
    procedure DisauthenticateUser(const aName: RawUTF8); override;
  end;


type
  /// optimized thread-safe storage of a list of IP v4 adresses
  // - can be used e.g. as white-list or black-list of clients
  // - will maintain internally a sorted list of 32-bit integers for fast lookup
  // - with optional binary persistence
  TIPBan = class(TSynPersistentStore)
  protected
    fIP4: TIntegerDynArray;
    fCount: integer;
    procedure LoadFromReader; override;
    procedure SaveToWriter(aWriter: TBufferWriter); override;
  public
    /// register one IP to the list
    function Add(const aIP: RawUTF8): boolean;
    /// unregister one IP to the list
    function Delete(const aIP: RawUTF8): boolean;
    /// returns true if the IP is in the list
    function Exists(const aIP: RawUTF8): boolean;
    /// creates a TDynArray wrapper around the stored list of values
    // - could be used e.g. for binary persistence
    // - warning: caller should make Safe.Unlock when finished
    function DynArrayLocked: TDynArray;
    /// low-level access to the internal IPv4 list
    // - 32-bit unsigned values are sorted, for fast O(log(n)) binary search
    property IP4: TIntegerDynArray read fIP4;
  published
    /// how many IPs are currently banned
    property Count: integer read fCount;
  end;


{ **************** 64-bit TSynUniqueIdentifier and its Efficient Generator }

type
  /// 64-bit integer unique identifier, as computed by TSynUniqueIdentifierGenerator
  // - they are increasing over time (so are much easier to store/shard/balance
  // than UUID/GUID), and contain generation time and a 16-bit process ID
  // - mapped by TSynUniqueIdentifierBits memory structure
  // - may be used on client side for something similar to a MongoDB ObjectID,
  // but compatible with TOrm.ID: TID properties
  TSynUniqueIdentifier = type Int64;

  /// 16-bit unique process identifier, used to compute TSynUniqueIdentifier
  // - each TSynUniqueIdentifierGenerator instance is expected to have
  // its own unique process identifier, stored as a 16 bit integer 1..65535 value
  TSynUniqueIdentifierProcess = type word;

  {$A-}
  /// map 64-bit integer unique identifier internal memory structure
  // - as stored in TSynUniqueIdentifier = Int64 values, and computed by
  // TSynUniqueIdentifierGenerator
  // - bits 0..14 map a 15-bit increasing counter (collision-free)
  // - bits 15..30 map a 16-bit process identifier
  // - bits 31..63 map a 33-bit UTC time, encoded as seconds since Unix epoch
  TSynUniqueIdentifierBits = object
  public
    /// the actual 64-bit storage value
    // - in practice, only first 63 bits are used
    Value: TSynUniqueIdentifier;
    /// 15-bit counter (0..32767), starting with a random value
    function Counter: word;
      {$ifdef HASINLINE}inline;{$endif}
    /// 16-bit unique process identifier
    // - as specified to TSynUniqueIdentifierGenerator constructor
    function ProcessID: TSynUniqueIdentifierProcess;
      {$ifdef HASINLINE}inline;{$endif}
    /// low-endian 4-byte value representing the seconds since the Unix epoch
    // - time is expressed in Coordinated Universal Time (UTC), not local time
    // - it uses in fact a 33-bit resolution, so is "Year 2038" bug-free
    function CreateTimeUnix: TUnixTime;
      {$ifdef HASINLINE}inline;{$endif}
    /// fill this unique identifier structure from its TSynUniqueIdentifier value
    // - is just a wrapper around PInt64(@self)^
    procedure From(const AID: TSynUniqueIdentifier);
      {$ifdef HASINLINE}inline;{$endif}
    /// convert this identifier as an explicit TDocVariant JSON object
    // - returns e.g.
    // ! {"Created":"2016-04-19T15:27:58","Identifier":1,"Counter":1,
    // ! "Value":3137644716930138113,"Hex":"2B8B273F00008001"}
    function AsVariant: variant;
      {$ifdef HASINLINE}inline;{$endif}
    /// convert this identifier to an explicit TDocVariant JSON object
    // - returns e.g.
    // ! {"Created":"2016-04-19T15:27:58","Identifier":1,"Counter":1,
    // ! "Value":3137644716930138113,"Hex":"2B8B273F00008001"}
    procedure ToVariant(out result: variant);
    /// extract the UTC generation timestamp from the identifier as TDateTime
    // - time is expressed in Coordinated Universal Time (UTC), not local time
    function CreateDateTime: TDateTime;
      {$ifdef HASINLINE}inline;{$endif}
    /// extract the UTC generation timestamp from the identifier
    // - time is expressed in Coordinated Universal Time (UTC), not local time
    function CreateTimeLog: TTimeLog;
    /// compare two Identifiers
    function Equal(const Another: TSynUniqueIdentifierBits): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// convert the identifier into a 16 chars hexadecimal string
    function ToHexa: RawUTF8;
      {$ifdef HASINLINE}inline;{$endif}
    /// fill this unique identifier back from a 16 chars hexadecimal string
    // - returns TRUE if the supplied hexadecimal is on the expected format
    // - returns FALSE if the supplied text is invalid
    function FromHexa(const hexa: RawUTF8): boolean;
    /// fill this unique identifier with a fake value corresponding to a given
    // timestamp
    // - may be used e.g. to limit database queries on a particular time range
    // - bits 0..30 would be 0, i.e. would set Counter = 0 and ProcessID = 0
    procedure FromDateTime(const aDateTime: TDateTime);
    /// fill this unique identifier with a fake value corresponding to a given
    // timestamp
    // - may be used e.g. to limit database queries on a particular time range
    // - bits 0..30 would be 0, i.e. would set Counter = 0 and ProcessID = 0
    procedure FromUnixTime(const aUnixTime: TUnixTime);
  end;
  {$A+}

  /// points to a 64-bit integer identifier, as computed by TSynUniqueIdentifierGenerator
  // - may be used to access the identifier internals, from its stored
  // Int64 or TSynUniqueIdentifier value

  PSynUniqueIdentifierBits = ^TSynUniqueIdentifierBits;

  /// a 24 chars cyphered hexadecimal string, mapping a TSynUniqueIdentifier
  // - has handled by TSynUniqueIdentifierGenerator.ToObfuscated/FromObfuscated
  TSynUniqueIdentifierObfuscated = type RawUTF8;

  /// thread-safe 64-bit integer unique identifier computation
  // - may be used on client side for something similar to a MongoDB ObjectID,
  // but compatible with TOrm.ID: TID properties, since it will contain
  // a 63-bit unsigned integer, following our ORM expectations
  // - each identifier would contain a 16-bit process identifier, which is
  // supplied by the application, and should be unique for this process at a
  // given time
  // - identifiers may be obfuscated as hexadecimal text, using both encryption
  // and digital signature
  TSynUniqueIdentifierGenerator = class(TSynPersistent)
  protected
    fUnixCreateTime: cardinal;
    fLatestCounterOverflowUnixCreateTime: cardinal;
    fIdentifier: TSynUniqueIdentifierProcess;
    fIdentifierShifted: cardinal;
    fLastCounter: cardinal;
    fCrypto: array[0..7] of cardinal; // only fCrypto[6..7] are used in practice
    fCryptoCRC: cardinal;
    fSafe: TSynLocker;
    function GetComputedCount: Int64;
  public
    /// initialize the generator for the given 16-bit process identifier
    // - you can supply an obfuscation key, which should be shared for the
    // whole system, so that you may use FromObfuscated/ToObfuscated methods
    constructor Create(aIdentifier: TSynUniqueIdentifierProcess;
      const aSharedObfuscationKey: RawUTF8 = ''); reintroduce;
    /// finalize the generator structure
    destructor Destroy; override;
    /// return a new unique ID
    // - this method is very optimized, and would use very little CPU
    procedure ComputeNew(out result: TSynUniqueIdentifierBits); overload;
    /// return a new unique ID, type-casted to an Int64
    function ComputeNew: Int64; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// return an unique ID matching this generator pattern, at a given timestamp
    // - may be used e.g. to limit database queries on a particular time range
    procedure ComputeFromDateTime(const aDateTime: TDateTime;
      out result: TSynUniqueIdentifierBits);
    /// return an unique ID matching this generator pattern, at a given timestamp
    // - may be used e.g. to limit database queries on a particular time range
    procedure ComputeFromUnixTime(const aUnixTime: TUnixTime;
      out result: TSynUniqueIdentifierBits);
    /// map a TSynUniqueIdentifier as 24 chars cyphered hexadecimal text
    // - cyphering includes simple key-based encryption and a CRC-32 digital signature
    function ToObfuscated(
      const aIdentifier: TSynUniqueIdentifier): TSynUniqueIdentifierObfuscated;
    /// retrieve a TSynUniqueIdentifier from 24 chars cyphered hexadecimal text
    // - any file extension (e.g. '.jpeg') would be first deleted from the
    // supplied obfuscated text
    // - returns true if the supplied obfuscated text has the expected layout
    // and a valid digital signature
    // - returns false if the supplied obfuscated text is invalid
    function FromObfuscated(const aObfuscated: TSynUniqueIdentifierObfuscated;
      out aIdentifier: TSynUniqueIdentifier): boolean;
    /// some 32-bit value, derivated from aSharedObfuscationKey as supplied
    // to the class constructor
    // - FromObfuscated and ToObfuscated methods will validate their hexadecimal
    // content with this value to secure the associated CRC
    // - may be used e.g. as system-depending salt
    property CryptoCRC: cardinal read fCryptoCRC;
    /// direct access to the associated mutex
    property Safe: TSynLocker read fSafe;
  published
    /// the process identifier, associated with this generator
    property Identifier: TSynUniqueIdentifierProcess read fIdentifier;
    /// how many times ComputeNew method has been called
    property ComputedCount: Int64 read GetComputedCount;
  end;

  /// hold a dynamic array of TSynUniqueIdentifierGenerator instances
  TSynUniqueIdentifierGenerators = array of TSynUniqueIdentifierGenerator;



{ **************** High-Level TSynSigner/TSynHasher Multi-Algorithm Wrappers }

{ implemented in this unit and not in mormot.core.crypto, since TSynSignerParams
  expects JSON support, which requires mormot.core.json }

type
  /// the HMAC/SHA-3 algorithms known by TSynSigner
  TSignAlgo = (
    saSha1,
    saSha256,
    saSha384,
    saSha512,
    saSha3224,
    saSha3256,
    saSha3384,
    saSha3512,
    saSha3S128,
    saSha3S256);

  /// JSON-serialization ready object as used by TSynSigner.PBKDF2 overloaded methods
  // - default value for unspecified parameters will be SHAKE_128 with
  // rounds=1000 and a fixed salt
  TSynSignerParams = packed record
    algo: TSignAlgo;
    secret, salt: RawUTF8;
    rounds: integer;
  end;

  /// a generic wrapper object to handle digital HMAC-SHA-2/SHA-3 signatures
  // - used e.g. to implement TJWTSynSignerAbstract
  TSynSigner = object
  private
    ctxt: packed array[1..SHA3ContextSize] of byte; // enough space for all
  public
    /// the size, in bytes, of the digital signature of this algorithm
    // - potential values are 20, 28, 32, 48 and 64
    SignatureSize: integer;
    /// the algorithm used for digitial signature
    Algo: TSignAlgo;
    /// initialize the digital HMAC/SHA-3 signing context with some secret text
    procedure Init(aAlgo: TSignAlgo; const aSecret: RawUTF8); overload;
    /// initialize the digital HMAC/SHA-3 signing context with some secret binary
    procedure Init(aAlgo: TSignAlgo;
      aSecret: pointer; aSecretLen: integer); overload;
    /// initialize the digital HMAC/SHA-3 signing context with PBKDF2 safe
    // iterative key derivation of a secret salted text
    procedure Init(aAlgo: TSignAlgo; const aSecret, aSalt: RawUTF8;
      aSecretPBKDF2Rounds: integer; aPBKDF2Secret: PHash512Rec = nil); overload;
    /// process some message content supplied as memory buffer
    procedure Update(aBuffer: pointer; aLen: integer); overload;
    /// process some message content supplied as string
    procedure Update(const aBuffer: RawByteString); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns the computed digital signature as lowercase hexadecimal text
    function Final: RawUTF8; overload;
    /// returns the raw computed digital signature
    // - SignatureSize bytes will be written: use Signature.Lo/h0/b3/b accessors
    procedure Final(out aSignature: THash512Rec;
      aNoInit: boolean = false); overload;
    /// one-step digital signature of a buffer as lowercase hexadecimal string
    function Full(aAlgo: TSignAlgo; const aSecret: RawUTF8;
      aBuffer: Pointer; aLen: integer): RawUTF8; overload;
    /// one-step digital signature of a buffer with PBKDF2 derivation
    function Full(aAlgo: TSignAlgo; const aSecret, aSalt: RawUTF8;
      aSecretPBKDF2Rounds: integer; aBuffer: Pointer; aLen: integer): RawUTF8; overload;
    /// convenient wrapper to perform PBKDF2 safe iterative key derivation
    procedure PBKDF2(aAlgo: TSignAlgo; const aSecret, aSalt: RawUTF8;
      aSecretPBKDF2Rounds: integer; out aDerivatedKey: THash512Rec); overload;
    /// convenient wrapper to perform PBKDF2 safe iterative key derivation
    procedure PBKDF2(const aParams: TSynSignerParams;
      out aDerivatedKey: THash512Rec); overload;
    /// convenient wrapper to perform PBKDF2 safe iterative key derivation
    // - accept as input a TSynSignerParams serialized as JSON object
    procedure PBKDF2(aParamsJSON: PUTF8Char; aParamsJSONLen: integer;
      out aDerivatedKey: THash512Rec;
      const aDefaultSalt: RawUTF8 = 'I6sWioAidNnhXO9BK';
      aDefaultAlgo: TSignAlgo = saSha3S128); overload;
    /// convenient wrapper to perform PBKDF2 safe iterative key derivation
    // - accept as input a TSynSignerParams serialized as JSON object
    procedure PBKDF2(const aParamsJSON: RawUTF8;
      out aDerivatedKey: THash512Rec;
      const aDefaultSalt: RawUTF8 = 'I6sWioAidNnhXO9BK';
      aDefaultAlgo: TSignAlgo = saSha3S128); overload;
    /// prepare a TAES object with the key derivated via a PBKDF2() call
    // - aDerivatedKey is defined as "var", since it will be zeroed after use
    procedure AssignTo(var aDerivatedKey: THash512Rec;
      out aAES: TAES; aEncrypt: boolean);
    /// fill the intenral context with zeros, for security
    procedure Done;
  end;

  /// reference to a TSynSigner wrapper object
  PSynSigner = ^TSynSigner;


  /// hash algorithms available for HashFile/HashFull functions
  // and TSynHasher object
  THashAlgo = (
    hfMD5,
    hfSHA1,
    hfSHA256,
    hfSHA384,
    hfSHA512,
    hfSHA3_256,
    hfSHA3_512);

  /// set of algorithms available for HashFile/HashFull functions and TSynHasher object
  THashAlgos = set of THashAlgo;

  /// convenient multi-algorithm hashing wrapper
  // - as used e.g. by HashFile/HashFull functions
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance
  TSynHasher = object
  private
    fAlgo: THashAlgo;
    ctxt: array[1..SHA3ContextSize] of byte; // enough space for all algorithms
  public
    /// initialize the internal hashing structure for a specific algorithm
    // - returns false on unknown/unsupported algorithm
    function Init(aAlgo: THashAlgo): boolean;
    /// hash the supplied memory buffer
    procedure Update(aBuffer: Pointer; aLen: integer); overload;
    /// hash the supplied string content
    procedure Update(const aBuffer: RawByteString); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns the resulting hash as lowercase hexadecimal string
    function Final: RawUTF8;
    /// one-step hash computation of a buffer as lowercase hexadecimal string
    function Full(aAlgo: THashAlgo; aBuffer: Pointer; aLen: integer): RawUTF8;
    /// the hash algorithm used by this instance
    property Algo: THashAlgo read fAlgo;
  end;

function ToText(algo: TSignAlgo): PShortString; overload;
function ToText(algo: THashAlgo): PShortString; overload;

/// compute the hexadecimal hash of any (big) file
// - using a temporary buffer of 1MB for the sequential reading
function HashFile(const aFileName: TFileName; aAlgo: THashAlgo): RawUTF8; overload;

/// compute the hexadecimal hashe(s) of one file, as external .md5/.sha256/.. files
// - reading the file once in memory, then apply all algorithms on it and
// generate the text hash files in the very same folder
procedure HashFile(const aFileName: TFileName; aAlgos: THashAlgos); overload;

/// one-step hash computation of a buffer as lowercase hexadecimal string
function HashFull(aAlgo: THashAlgo; aBuffer: Pointer; aLen: integer): RawUTF8;



{ ****** IProtocol Safe Communication with Unilateral or Mutual Authentication }

type
  /// possible return codes by IProtocol classes
  TProtocolResult = (
    sprSuccess,
    sprBadRequest,
    sprUnsupported,
    sprUnexpectedAlgorithm,
    sprInvalidCertificate,
    sprInvalidSignature,
    sprInvalidEphemeralKey,
    sprInvalidPublicKey,
    sprInvalidPrivateKey,
    sprInvalidMAC);

  /// perform safe communication after unilateral or mutual authentication
  // - see e.g. TProtocolNone or SynEcc's TECDHEProtocolClient and
  // TECDHEProtocolServer implementation classes
  IProtocol = interface
    ['{91E3CA39-3AE2-44F4-9B8C-673AC37C1D1D}']
    /// initialize the communication by exchanging some client/server information
    // - expects the handshaking messages to be supplied as UTF-8 text, may be as
    // base64-encoded binary - see e.g. TWebSocketProtocolBinary.ProcessHandshake
    // - should return sprUnsupported if the implemented protocol does not
    // expect any handshaking mechanism
    // - returns sprSuccess and set something into OutData, depending on the
    // current step of the handshake
    // - returns an error code otherwise
    function ProcessHandshake(const MsgIn: RawUTF8;
      out MsgOut: RawUTF8): TProtocolResult;
    /// encrypt a message on one side, ready to be transmitted to the other side
    // - this method should be thread-safe in the implementation class
    procedure Encrypt(const aPlain: RawByteString;
      out aEncrypted: RawByteString);
    /// decrypt a message on one side, as transmitted from the other side
    // - should return sprSuccess if the
    // - should return sprInvalidMAC in case of wrong aEncrypted input (e.g.
    // packet corruption, MiM or Replay attacks attempts)
    // - this method should be thread-safe in the implementation class
    function Decrypt(const aEncrypted: RawByteString;
      out aPlain: RawByteString): TProtocolResult;
    /// will create another instance of this communication protocol
    function Clone: IProtocol;
  end;

  /// stores a list of IProtocol instances
  IProtocolDynArray = array of IProtocol;

  /// implements a fake no-encryption protocol
  // - may be used for debugging purposes, or when encryption is not needed
  TProtocolNone = class(TInterfacedObject, IProtocol)
  public
    /// initialize the communication by exchanging some client/server information
    // - this method will return sprUnsupported
    function ProcessHandshake(const MsgIn: RawUTF8;
      out MsgOut: RawUTF8): TProtocolResult;
    /// encrypt a message on one side, ready to be transmitted to the other side
    // - this method will return the plain text with no actual encryption
    procedure Encrypt(const aPlain: RawByteString;
      out aEncrypted: RawByteString);
    /// decrypt a message on one side, as transmitted from the other side
    // - this method will return the encrypted text with no actual decryption
    function Decrypt(const aEncrypted: RawByteString;
      out aPlain: RawByteString): TProtocolResult;
    /// will create another instance of this communication protocol
    function Clone: IProtocol;
  end;

  /// implements a secure protocol using AES encryption
  // - as used e.g. by 'synopsebinary' WebSockets protocol
  // - this class will maintain two TAESAbstract instances, one for encryption
  // and another one for decryption, with PKCS7 padding and no MAC validation
  TProtocolAES = class(TInterfacedObject, IProtocol)
  protected
    fSafe: TRTLCriticalSection; // no need of TSynLocker padding
    fAES: array[boolean] of TAESAbstract; // [false]=decrypt [true]=encrypt
  public
    /// initialize this encryption protocol with the given AES settings
    // - warning: aKey is an untyped constant, i.e. expects a raw set of memory
    // bytes: do NOT use assign it with a string or a TBytes instance: you would
    // use the pointer to the data as key
    constructor Create(aClass: TAESAbstractClass; const aKey; aKeySize: cardinal;
      aIVReplayAttackCheck: TAESIVReplayAttackCheck = repCheckedIfAvailable);
        reintroduce; virtual;
    /// will create another instance of this communication protocol
    constructor CreateFrom(aAnother: TProtocolAES); reintroduce; virtual;
    /// finalize the encryption
    destructor Destroy; override;
    /// initialize the communication by exchanging some client/server information
    // - this method will return sprUnsupported
    function ProcessHandshake(const MsgIn: RawUTF8;
      out MsgOut: RawUTF8): TProtocolResult;
    /// encrypt a message on one side, ready to be transmitted to the other side
    // - this method uses AES encryption and PKCS7 padding
    procedure Encrypt(const aPlain: RawByteString;
      out aEncrypted: RawByteString);
    /// decrypt a message on one side, as transmitted from the other side
    // - this method uses AES decryption and PKCS7 padding
    function Decrypt(const aEncrypted: RawByteString;
      out aPlain: RawByteString): TProtocolResult;
    /// will create another instance of this communication protocol
    function Clone: IProtocol;
  end;

  /// class-reference type (metaclass) of an AES secure protocol
  TProtocolAESClass = class of TProtocolAES;

function ToText(res: TProtocolResult): PShortString; overload;



implementation



{ **************** High-Level TSynSigner/TSynHasher Multi-Algorithm Wrappers }

{ TSynHasher }

function TSynHasher.Init(aAlgo: THashAlgo): boolean;
begin
  fAlgo := aAlgo;
  result := true;
  case aAlgo of
    hfMD5:
      PMD5(@ctxt)^.Init;
    hfSHA1:
      PSHA1(@ctxt)^.Init;
    hfSHA256:
      PSHA256(@ctxt)^.Init;
    hfSHA384:
      PSHA384(@ctxt)^.Init;
    hfSHA512:
      PSHA512(@ctxt)^.Init;
    hfSHA3_256:
      PSHA3(@ctxt)^.Init(SHA3_256);
    hfSHA3_512:
      PSHA3(@ctxt)^.Init(SHA3_512);
  else
    result := false;
  end;
end;

procedure TSynHasher.Update(aBuffer: Pointer; aLen: integer);
begin
  case fAlgo of
    hfMD5:
      PMD5(@ctxt)^.Update(aBuffer^, aLen);
    hfSHA1:
      PSHA1(@ctxt)^.Update(aBuffer, aLen);
    hfSHA256:
      PSHA256(@ctxt)^.Update(aBuffer, aLen);
    hfSHA384:
      PSHA384(@ctxt)^.Update(aBuffer, aLen);
    hfSHA512:
      PSHA512(@ctxt)^.Update(aBuffer, aLen);
    hfSHA3_256:
      PSHA3(@ctxt)^.Update(aBuffer, aLen);
    hfSHA3_512:
      PSHA3(@ctxt)^.Update(aBuffer, aLen);
  end;
end;

procedure TSynHasher.Update(const aBuffer: RawByteString);
begin
  Update(pointer(aBuffer), length(aBuffer));
end;

function TSynHasher.Final: RawUTF8;
begin
  case fAlgo of
    hfMD5:
      result := MD5DigestToString(PMD5(@ctxt)^.Final);
    hfSHA1:
      result := SHA1DigestToString(PSHA1(@ctxt)^.Final);
    hfSHA256:
      result := SHA256DigestToString(PSHA256(@ctxt)^.Final);
    hfSHA384:
      result := SHA384DigestToString(PSHA384(@ctxt)^.Final);
    hfSHA512:
      result := SHA512DigestToString(PSHA512(@ctxt)^.Final);
    hfSHA3_256:
      result := SHA256DigestToString(PSHA3(@ctxt)^.Final256);
    hfSHA3_512:
      result := SHA512DigestToString(PSHA3(@ctxt)^.Final512);
  end;
end;

function TSynHasher.Full(aAlgo: THashAlgo; aBuffer: Pointer; aLen: integer): RawUTF8;
begin
  Init(aAlgo);
  Update(aBuffer, aLen);
  result := Final;
end;

function HashFull(aAlgo: THashAlgo; aBuffer: Pointer; aLen: integer): RawUTF8;
var
  hasher: TSynHasher;
begin
  result := hasher.Full(aAlgo, aBuffer, aLen);
end;

function HashFile(const aFileName: TFileName; aAlgo: THashAlgo): RawUTF8;
var
  hasher: TSynHasher;
  temp: RawByteString;
  F: THandle;
  size: Int64;
  read: cardinal;
begin
  result := '';
  if (aFileName = '') or
     not hasher.Init(aAlgo) then
    exit;
  F := FileOpenSequentialRead(aFileName);
  if PtrInt(F) >= 0 then
  try
    size := FileSize(F);
    SetLength(temp, 1 shl 20); // 1MB temporary buffer for reading
    while size > 0 do
    begin
      read := FileRead(F, pointer(temp)^, 1 shl 20);
      if read <= 0 then
        exit;
      hasher.Update(pointer(temp), read);
      dec(size, read);
    end;
    result := hasher.Final;
  finally
    FileClose(F);
  end;
end;

procedure HashFile(const aFileName: TFileName; aAlgos: THashAlgos);
var
  data, hash: RawUTF8;
  efn, fn: string;
  a: THashAlgo;
begin
  if aAlgos = [] then
    exit;
  efn := ExtractFileName(aFileName);
  data := StringFromFile(aFileName);
  if data <> '' then
    for a := low(a) to high(a) do
      if a in aAlgos then
      begin
        FormatUTF8('% *%',
          [HashFull(a, pointer(data), length(data)), efn], hash);
        FormatString('%.%',
          [efn, LowerCase(TrimLeftLowerCaseShort(ToText(a)))], fn);
        FileFromString(hash, fn);
      end;
end;


{ TSynSigner }

procedure TSynSigner.Init(aAlgo: TSignAlgo; aSecret: pointer; aSecretLen: integer);
const
  SIGN_SIZE: array[TSignAlgo] of byte = (
    20, 32, 48, 64, 28, 32, 48, 64, 32, 64);
  SHA3_ALGO: array[saSha3224..saSha3S256] of TSHA3Algo = (
    SHA3_224, SHA3_256, SHA3_384, SHA3_512, SHAKE_128, SHAKE_256);
begin
  Algo := aAlgo;
  SignatureSize := SIGN_SIZE[Algo];
  case Algo of
    saSha1:
      PHMAC_SHA1(@ctxt)^.Init(aSecret, aSecretLen);
    saSha256:
      PHMAC_SHA256(@ctxt)^.Init(aSecret, aSecretLen);
    saSha384:
      PHMAC_SHA384(@ctxt)^.Init(aSecret, aSecretLen);
    saSha512:
      PHMAC_SHA512(@ctxt)^.Init(aSecret, aSecretLen);
    saSha3224..saSha3S256:
      begin
        PSHA3(@ctxt)^.Init(SHA3_ALGO[Algo]);
        PSHA3(@ctxt)^.Update(aSecret, aSecretLen);
      end; // note: the HMAC pattern is included in SHA-3
  end;
end;

procedure TSynSigner.Init(aAlgo: TSignAlgo; const aSecret: RawUTF8);
begin
  Init(aAlgo, pointer(aSecret), length(aSecret));
end;

procedure TSynSigner.Init(aAlgo: TSignAlgo; const aSecret, aSalt: RawUTF8;
  aSecretPBKDF2Rounds: integer; aPBKDF2Secret: PHash512Rec);
var
  temp: THash512Rec;
begin
  if aSecretPBKDF2Rounds > 1 then
  begin
    PBKDF2(aAlgo, aSecret, aSalt, aSecretPBKDF2Rounds, temp);
    Init(aAlgo, @temp, SignatureSize);
    if aPBKDF2Secret <> nil then
      aPBKDF2Secret^ := temp;
    FillZero(temp.b);
  end
  else
    Init(aAlgo, aSecret);
end;

procedure TSynSigner.Update(const aBuffer: RawByteString);
begin
  Update(pointer(aBuffer), length(aBuffer));
end;

procedure TSynSigner.Update(aBuffer: pointer; aLen: integer);
begin
  case Algo of
    saSha1:
      PHMAC_SHA1(@ctxt)^.Update(aBuffer, aLen);
    saSha256:
      PHMAC_SHA256(@ctxt)^.Update(aBuffer, aLen);
    saSha384:
      PHMAC_SHA384(@ctxt)^.Update(aBuffer, aLen);
    saSha512:
      PHMAC_SHA512(@ctxt)^.Update(aBuffer, aLen);
    saSha3224..saSha3S256:
      PSHA3(@ctxt)^.Update(aBuffer, aLen);
  end;
end;

procedure TSynSigner.Final(out aSignature: THash512Rec; aNoInit: boolean);
begin
  case Algo of
    saSha1:
      PHMAC_SHA1(@ctxt)^.Done(aSignature.b160, aNoInit);
    saSha256:
      PHMAC_SHA256(@ctxt)^.Done(aSignature.Lo, aNoInit);
    saSha384:
      PHMAC_SHA384(@ctxt)^.Done(aSignature.b384, aNoInit);
    saSha512:
      PHMAC_SHA512(@ctxt)^.Done(aSignature.b, aNoInit);
    saSha3224..saSha3S256:
      PSHA3(@ctxt)^.Final(@aSignature, SignatureSize shl 3, aNoInit);
  end;
end;

function TSynSigner.Final: RawUTF8;
var
  sig: THash512Rec;
begin
  Final(sig);
  result := BinToHexLower(@sig, SignatureSize);
end;

function TSynSigner.Full(aAlgo: TSignAlgo; const aSecret: RawUTF8;
  aBuffer: Pointer; aLen: integer): RawUTF8;
begin
  Init(aAlgo, aSecret);
  Update(aBuffer, aLen);
  result := Final;
end;

function TSynSigner.Full(aAlgo: TSignAlgo; const aSecret, aSalt: RawUTF8;
  aSecretPBKDF2Rounds: integer; aBuffer: Pointer; aLen: integer): RawUTF8;
begin
  Init(aAlgo, aSecret, aSalt, aSecretPBKDF2Rounds);
  Update(aBuffer, aLen);
  result := Final;
end;

procedure TSynSigner.PBKDF2(aAlgo: TSignAlgo; const aSecret, aSalt: RawUTF8;
  aSecretPBKDF2Rounds: integer; out aDerivatedKey: THash512Rec);
var
  iter: TSynSigner;
  temp: THash512Rec;
  i: integer;
begin
  Init(aAlgo, aSecret);
  iter := self;
  iter.Update(aSalt);
  if Algo < saSha3224 then
    iter.Update(#0#0#0#1); // padding and XoF mode already part of SHA-3 process
  iter.Final(aDerivatedKey, true);
  if aSecretPBKDF2Rounds < 2 then
    exit;
  temp := aDerivatedKey;
  for i := 2 to aSecretPBKDF2Rounds do
  begin
    iter := self;
    iter.Update(@temp, SignatureSize);
    iter.Final(temp, true);
    XorMemory(@aDerivatedKey, @temp, SignatureSize);
  end;
  FillZero(temp.b);
  FillCharFast(iter.ctxt, SizeOf(iter.ctxt), 0);
  FillCharFast(ctxt, SizeOf(ctxt), 0);
end;

procedure TSynSigner.PBKDF2(const aParams: TSynSignerParams;
  out aDerivatedKey: THash512Rec);
begin
  PBKDF2(aParams.algo, aParams.secret, aParams.salt, aParams.rounds, aDerivatedKey);
end;

procedure TSynSigner.PBKDF2(aParamsJSON: PUTF8Char; aParamsJSONLen: integer;
  out aDerivatedKey: THash512Rec; const aDefaultSalt: RawUTF8; aDefaultAlgo: TSignAlgo);
var
  tmp: TSynTempBuffer;
  k: TSynSignerParams;

  procedure SetDefault;
  begin
    k.algo := aDefaultAlgo;
    k.secret := '';
    k.salt := aDefaultSalt;
    k.rounds := 1000;
  end;

begin
  SetDefault;
  if (aParamsJSON = nil) or
     (aParamsJSONLen <= 0) then
    k.secret := aDefaultSalt
  else if aParamsJSON[1] <> '{' then
    FastSetString(k.secret, aParamsJSON, aParamsJSONLen)
  else
  begin
    tmp.Init(aParamsJSON, aParamsJSONLen);
    try
      if (RecordLoadJSON(k, tmp.buf, TypeInfo(TSynSignerParams)) = nil) or
         (k.secret = '') or
         (k.salt = '') then
      begin
        SetDefault;
        FastSetString(k.secret, aParamsJSON, aParamsJSONLen);
      end;
    finally
      FillCharFast(tmp.buf^, tmp.len, 0);
      tmp.Done;
    end;
  end;
  PBKDF2(k.algo, k.secret, k.salt, k.rounds, aDerivatedKey);
  FillZero(k.secret);
end;

procedure TSynSigner.PBKDF2(const aParamsJSON: RawUTF8;
  out aDerivatedKey: THash512Rec; const aDefaultSalt: RawUTF8; aDefaultAlgo: TSignAlgo);
begin
  PBKDF2(pointer(aParamsJSON), length(aParamsJSON),
    aDerivatedKey, aDefaultSalt, aDefaultAlgo);
end;

procedure TSynSigner.AssignTo(var aDerivatedKey: THash512Rec;
  out aAES: TAES; aEncrypt: boolean);
var
  ks: integer;
begin
  case algo of
    saSha3S128:
      ks := 128; // truncate to Keccak sponge precision
    saSha3S256:
      ks := 256;
  else
    case SignatureSize of
      20:
        begin
          ks := 128;
          aDerivatedKey.i0 := aDerivatedKey.i0 xor aDerivatedKey.i4;
        end;
      28:
        ks := 192;
      32:
        ks := 256;
      48:
        begin
          ks := 256;
          aDerivatedKey.d0 := aDerivatedKey.d0 xor aDerivatedKey.d4;
          aDerivatedKey.d1 := aDerivatedKey.d1 xor aDerivatedKey.d5;
        end;
      64:
        begin
          ks := 256;
          aDerivatedKey.d0 := aDerivatedKey.d0 xor aDerivatedKey.d4;
          aDerivatedKey.d1 := aDerivatedKey.d1 xor aDerivatedKey.d5;
          aDerivatedKey.d2 := aDerivatedKey.d0 xor aDerivatedKey.d6;
          aDerivatedKey.d3 := aDerivatedKey.d1 xor aDerivatedKey.d7;
        end;
    else
      exit;
    end;
  end;
  aAES.DoInit(aDerivatedKey, ks, aEncrypt);
  FillZero(aDerivatedKey.b);
end;

procedure TSynSigner.Done;
begin
  FillCharFast(self, SizeOf(self), 0);
end;

function ToText(algo: TSignAlgo): PShortString;
begin
  result := GetEnumName(TypeInfo(TSignAlgo), ord(algo));
end;

function ToText(algo: THashAlgo): PShortString;
begin
  result := GetEnumName(TypeInfo(THashAlgo), ord(algo));
end;


{ ***************** TSyn***Password and TSynConnectionDefinition Classes }

procedure SymmetricEncrypt(key: cardinal; var data: RawByteString);
var
  i, len: integer;
  d: PCardinal;
  tab: PCrc32tab;
begin
  if data = '' then
    exit; // nothing to cypher
  {$ifdef FPC}
  UniqueString(data); // @data[1] won't call UniqueString() under FPC :(
  {$endif}
  d := @data[1];
  len := length(data);
  key := key xor cardinal(len);
  tab := @crc32ctab;
  for i := 0 to (len shr 2) - 1 do
  begin
    key := key xor tab[0, (cardinal(i) xor key) and 1023];
    d^ := d^ xor key;
    inc(d);
  end;
  for i := 0 to (len and 3) - 1 do
    PByteArray(d)^[i] := PByteArray(d)^[i] xor key xor tab[0, 17 shl i];
end;


{ TSynPersistentWithPassword }

destructor TSynPersistentWithPassword.Destroy;
begin
  FillZero(fPassword);
  inherited Destroy;
end;

class function TSynPersistentWithPassword.ComputePassword(
  const PlainPassword: SPIUTF8; CustomKey: cardinal): SPIUTF8;
var
  instance: TSynPersistentWithPassword;
begin
  instance := TSynPersistentWithPassword.Create;
  try
    instance.Key := CustomKey;
    instance.SetPassWordPlain(PlainPassword);
    result := instance.fPassWord;
  finally
    instance.Free;
  end;
end;

class function TSynPersistentWithPassword.ComputePassword(PlainPassword: pointer;
  PlainPasswordLen: integer; CustomKey: cardinal): SPIUTF8;
begin
  result := ComputePassword(BinToBase64uri(PlainPassword, PlainPasswordLen));
end;

class function TSynPersistentWithPassword.ComputePlainPassword(
  const CypheredPassword: SPIUTF8; CustomKey: cardinal;
  const AppSecret: RawUTF8): SPIUTF8;
var
  instance: TSynPersistentWithPassword;
begin
  instance := TSynPersistentWithPassword.Create;
  try
    instance.Key := CustomKey;
    instance.fPassWord := CypheredPassword;
    result := instance.GetPassWordPlainInternal(AppSecret);
  finally
    instance.Free;
  end;
end;

function TSynPersistentWithPassword.GetKey: cardinal;
begin
  if self = nil then
    result := 0
  else
    result := fKey xor $A5abba5A;
end;

function TSynPersistentWithPassword.GetPassWordPlain: SPIUTF8;
begin
  result := GetPassWordPlainInternal('');
end;

function TSynPersistentWithPassword.GetPassWordPlainInternal(
  AppSecret: RawUTF8): SPIUTF8;
var
  value, pass: RawByteString;
  usr: RawUTF8;
  i, j: integer;
begin
  result := '';
  if (self = nil) or
     (fPassWord = '') then
    exit;
  if AppSecret = '' then
    ClassToText(ClassType, AppSecret);
  usr := ExeVersion.User + ':';
  i := PosEx(usr, fPassword);
  if (i = 1) or
     ((i > 0) and
      (fPassword[i - 1] = ',')) then
  begin
    // handle '....,username:passwordbase64,....' or 'unsername:passwordbase64'
    inc(i, length(usr));
    j := PosEx(',', fPassword, i);
    if j = 0 then
      j := length(fPassword) + 1;
    Base64ToBin(@fPassword[i], j - i, pass);
    if pass <> '' then
      result := CryptDataForCurrentUser(pass, AppSecret, false);
  end
  else
  begin
    i := PosExChar(':', fPassword);
    if i > 0 then
      raise ESynException.CreateUTF8('%.GetPassWordPlain unable to retrieve the ' +
        'stored value: current user is [%], but password in % was encoded for [%]',
        [self, ExeVersion.User, AppSecret, copy(fPassword, 1, i - 1)]);
  end;
  if result = '' then
  begin
    value := Base64ToBin(fPassWord);
    SymmetricEncrypt(GetKey, value);
    result := value;
  end;
end;

procedure TSynPersistentWithPassword.SetPassWordPlain(const Value: SPIUTF8);
var
  tmp: RawByteString;
begin
  if self = nil then
    exit;
  if value = '' then
  begin
    fPassWord := '';
    exit;
  end;
  SetString(tmp, PAnsiChar(pointer(value)), Length(value)); // private copy
  SymmetricEncrypt(GetKey, tmp);
  fPassWord := BinToBase64(tmp);
end;


{ TSynConnectionDefinition }

constructor TSynConnectionDefinition.CreateFromJSON(const JSON: RawUTF8; Key: cardinal);
var
  privateCopy: RawUTF8;
  values: array[0..4] of TValuePUTF8Char;
begin
  fKey := Key;
  privateCopy := JSON;
  JSONDecode(privateCopy,
    ['Kind', 'ServerName', 'DatabaseName', 'User', 'Password'], @values);
  fKind := values[0].ToString;
  values[1].ToUTF8(fServerName);
  values[2].ToUTF8(fDatabaseName);
  values[3].ToUTF8(fUser);
  fPassWord := values[4].ToUTF8;
end;

function TSynConnectionDefinition.SaveToJSON: RawUTF8;
begin
  result := JSONEncode([
    'Kind', fKind,
    'ServerName', fServerName,
    'DatabaseName', fDatabaseName,
    'User', fUser,
    'Password', fPassword]);
end;


{ ***************** Reusable Authentication Classes }

{ TSynAuthenticationAbstract }

constructor TSynAuthenticationAbstract.Create;
begin
  fSafe.Init;
  fTokenSeed := Random32;
  fSessionGenerator := abs(fTokenSeed * PPtrInt(self)^);
  fTokenSeed := abs(fTokenSeed * Random32);
end;

destructor TSynAuthenticationAbstract.Destroy;
begin
  fSafe.Done;
  inherited;
end;

class function TSynAuthenticationAbstract.ComputeHash(Token: Int64;
  const UserName, PassWord: RawUTF8): cardinal;
begin // rough authentication - xxHash32 is less reversible than crc32c
  result := xxHash32( xxHash32( xxHash32(
    Token, @Token, SizeOf(Token)),
    pointer(UserName), length(UserName)),
    pointer(PassWord), length(PassWord));
end;

function TSynAuthenticationAbstract.ComputeCredential(previous: boolean;
  const UserName, PassWord: RawUTF8): cardinal;
var
  tok: Int64;
begin
  tok := GetTickCount64 div 10000;
  if previous then
    dec(tok);
  result := ComputeHash(tok xor fTokenSeed, UserName, PassWord);
end;

function TSynAuthenticationAbstract.CurrentToken: Int64;
begin
  result := (GetTickCount64 div 10000) xor fTokenSeed;
end;

procedure TSynAuthenticationAbstract.AuthenticateUser(const aName, aPassword: RawUTF8);
begin
  raise ESynException.CreateUTF8('%.AuthenticateUser() is not implemented', [self]);
end;

procedure TSynAuthenticationAbstract.DisauthenticateUser(const aName: RawUTF8);
begin
  raise ESynException.CreateUTF8('%.DisauthenticateUser() is not implemented', [self]);
end;

function TSynAuthenticationAbstract.CheckCredentials(const UserName: RaWUTF8;
  Hash: cardinal): boolean;
var
  password: RawUTF8;
begin
  result := GetPassword(UserName, password) and
    ((ComputeCredential({previous=}false, UserName, password{%H-}) = Hash) or
     (ComputeCredential({previous=}true,  UserName, password) = Hash));
end;

function TSynAuthenticationAbstract.CreateSession(const User: RawUTF8;
  Hash: cardinal): integer;
begin
  result := 0;
  fSafe.Lock;
  try
    if not CheckCredentials(User, Hash) then
      exit;
    repeat
      result := fSessionGenerator;
      inc(fSessionGenerator);
    until result <> 0;
    AddSortedInteger(fSessions, fSessionsCount, result);
  finally
    fSafe.UnLock;
  end;
end;

function TSynAuthenticationAbstract.SessionExists(aID: integer): boolean;
begin
  fSafe.Lock;
  try
    result := FastFindIntegerSorted(
      pointer(fSessions), fSessionsCount - 1, aID) >= 0;
  finally
    fSafe.UnLock;
  end;
end;

procedure TSynAuthenticationAbstract.RemoveSession(aID: integer);
var
  i: integer;
begin
  fSafe.Lock;
  try
    i := FastFindIntegerSorted(pointer(fSessions), fSessionsCount - 1, aID);
    if i >= 0 then
      DeleteInteger(fSessions, fSessionsCount, i);
  finally
    fSafe.UnLock;
  end;
end;


{ TSynAuthentication }

constructor TSynAuthentication.Create(const aUserName, aPassword: RawUTF8);
begin
  inherited Create;
  fCredentials.Init(true);
  if aUserName <> '' then
    AuthenticateUser(aUserName, aPassword);
end;

function TSynAuthentication.GetPassword(const UserName: RawUTF8;
  out Password: RawUTF8): boolean;
var
  i: integer;
begin // caller did protect this method via fSafe.Lock
  i := fCredentials.Find(UserName);
  if i < 0 then
  begin
    result := false;
    exit;
  end;
  Password := fCredentials.List[i].Value;
  result := true;
end;

function TSynAuthentication.GetUsersCount: integer;
begin
  fSafe.Lock;
  try
    result := fCredentials.Count;
  finally
    fSafe.UnLock;
  end;
end;

procedure TSynAuthentication.AuthenticateUser(const aName, aPassword: RawUTF8);
begin
  fSafe.Lock;
  try
    fCredentials.Add(aName, aPassword);
  finally
    fSafe.UnLock;
  end;
end;

procedure TSynAuthentication.DisauthenticateUser(const aName: RawUTF8);
begin
  fSafe.Lock;
  try
    fCredentials.Delete(aName);
  finally
    fSafe.UnLock;
  end;
end;


{ TIPBan }

procedure TIPBan.LoadFromReader;
begin
  inherited;
  fReader.ReadVarUInt32Array(fIP4);
  fCount := length(fIP4);
end;

procedure TIPBan.SaveToWriter(aWriter: TBufferWriter);
begin
  // wkSorted not efficient: too big diffs between IPs
  aWriter.WriteVarUInt32Array(fIP4, fCount, wkUInt32);
end;

function TIPBan.Add(const aIP: RawUTF8): boolean;
var
  ip4: cardinal;
begin
  result := false;
  if (self = nil) or
     not IPToCardinal(aIP, ip4) then
    exit;
  fSafe.Lock;
  try
    AddSortedInteger(fIP4, fCount, ip4);
    result := true;
  finally
    fSafe.UnLock;
  end;
end;

function TIPBan.Delete(const aIP: RawUTF8): boolean;
var
  ip4: cardinal;
  i: integer;
begin
  result := false;
  if (self = nil) or
     not IPToCardinal(aIP, ip4) then
    exit;
  fSafe.Lock;
  try
    i := FastFindIntegerSorted(pointer(fIP4), fCount - 1, ip4);
    if i < 0 then
      exit;
    DeleteInteger(fIP4, fCount, i);
    result := true;
  finally
    fSafe.UnLock;
  end;
end;

function TIPBan.Exists(const aIP: RawUTF8): boolean;
var
  ip4: cardinal;
begin
  result := false;
  if (self = nil) or
     (fCount = 0) or
     not IPToCardinal(aIP, ip4) then
    exit;
  fSafe.Lock;
  try
    if FastFindIntegerSorted(pointer(fIP4), fCount - 1, ip4) >= 0 then
      result := true;
  finally
    fSafe.UnLock;
  end;
end;

function TIPBan.DynArrayLocked: TDynArray;
begin
  fSafe.Lock;
  result.InitSpecific(TypeInfo(TCardinalDynArray), fIP4, ptCardinal, @fCount);
end;



{ **************** 64-bit TSynUniqueIdentifier and its Efficient Generator }

{ TSynUniqueIdentifierBits }

function TSynUniqueIdentifierBits.Counter: word;
begin
  result := PWord(@Value)^ and $7fff;
end;

function TSynUniqueIdentifierBits.ProcessID: TSynUniqueIdentifierProcess;
begin
  result := (PCardinal(@Value)^ shr 15) and $ffff;
end;

function TSynUniqueIdentifierBits.CreateTimeUnix: TUnixTime;
begin
  result := Value shr 31;
end;

function TSynUniqueIdentifierBits.AsVariant: variant;
begin
  ToVariant(result);
end;

procedure TSynUniqueIdentifierBits.ToVariant(out result: variant);
begin
  TDocVariantData(result).InitObject([
    'Created', DateTimeToIso8601Text(CreateDateTime),
    'Identifier', ProcessID,
    'Counter', Counter,
    'Value', Value,
    'Hex', Int64ToHex(Value)], JSON_OPTIONS_FAST);
end;

function TSynUniqueIdentifierBits.Equal(
  const Another: TSynUniqueIdentifierBits): boolean;
begin
  result := Value = Another.Value;
end;

procedure TSynUniqueIdentifierBits.From(const AID: TSynUniqueIdentifier);
begin
  Value := AID;
end;

function TSynUniqueIdentifierBits.CreateTimeLog: TTimeLog;
begin
  PTimeLogBits(@result)^.From(UnixTimeToDateTime(Value shr 31));
end;

function TSynUniqueIdentifierBits.CreateDateTime: TDateTime;
begin
  result := UnixTimeToDateTime(Value shr 31);
end;

function TSynUniqueIdentifierBits.ToHexa: RawUTF8;
begin
  Int64ToHex(Value, result);
end;

function TSynUniqueIdentifierBits.FromHexa(const hexa: RawUTF8): boolean;
begin
  result := (Length(hexa) = 16) and HexDisplayToBin(pointer(hexa), @Value, SizeOf(Value));
end;

procedure TSynUniqueIdentifierBits.FromDateTime(const aDateTime: TDateTime);
begin
  Value := DateTimeToUnixTime(aDateTime) shl 31;
end;

procedure TSynUniqueIdentifierBits.FromUnixTime(const aUnixTime: TUnixTime);
begin
  Value := aUnixTime shl 31;
end;


{ TSynUniqueIdentifierGenerator }

const
  // fSafe.Padding[] slots
  SYNUNIQUEGEN_COMPUTECOUNT = 0;

procedure TSynUniqueIdentifierGenerator.ComputeNew(
  out result: TSynUniqueIdentifierBits);
var
  currentTime: cardinal;
begin
  currentTime := UnixTimeUTC; // under Windows faster than GetTickCount64
  fSafe.Lock;
  try
    if currentTime > fUnixCreateTime then
    begin
      fUnixCreateTime := currentTime;
      fLastCounter := 0; // reset
    end;
    if fLastCounter = $7fff then
    begin // collision (unlikely) -> cheat on timestamp
      inc(fUnixCreateTime);
      fLastCounter := 0;
    end
    else
      inc(fLastCounter);
    result.Value := Int64(fLastCounter or fIdentifierShifted) or
                    (Int64(fUnixCreateTime) shl 31);
    inc(fSafe.Padding[SYNUNIQUEGEN_COMPUTECOUNT].VInt64);
  finally
    fSafe.UnLock;
  end;
end;

function TSynUniqueIdentifierGenerator.ComputeNew: Int64;
begin
  ComputeNew(PSynUniqueIdentifierBits(@result)^);
end;

function TSynUniqueIdentifierGenerator.GetComputedCount: Int64;
begin
  result := fSafe.LockedInt64[SYNUNIQUEGEN_COMPUTECOUNT];
end;

procedure TSynUniqueIdentifierGenerator.ComputeFromDateTime(
  const aDateTime: TDateTime; out result: TSynUniqueIdentifierBits);
begin
  // assume fLastCounter=0
  ComputeFromUnixTime(DateTimeToUnixTime(aDateTime), result);
end;

procedure TSynUniqueIdentifierGenerator.ComputeFromUnixTime(const aUnixTime: TUnixTime;
  out result: TSynUniqueIdentifierBits);
begin // assume fLastCounter=0
  result.Value := aUnixTime shl 31;
  if self <> nil then
    result.Value := result.Value or fIdentifierShifted;
end;

constructor TSynUniqueIdentifierGenerator.Create(
  aIdentifier: TSynUniqueIdentifierProcess; const aSharedObfuscationKey: RawUTF8);
var
  i, len: integer;
  crc: cardinal;
begin
  fIdentifier := aIdentifier;
  fIdentifierShifted := aIdentifier shl 15;
  fSafe.Init;
  fSafe.Padding[SYNUNIQUEGEN_COMPUTECOUNT].VType := varInt64; // reset to 0
  // compute obfuscation key using hash diffusion of the supplied text
  len := length(aSharedObfuscationKey);
  crc := crc32ctab[0, len and 1023];
  for i := 0 to high(fCrypto) + 1 do
  begin
    crc := crc32ctab[0, crc and 1023] xor
           crc32ctab[3, i] xor
           kr32(crc, pointer(aSharedObfuscationKey), len) xor
           crc32c(crc, pointer(aSharedObfuscationKey), len) xor
           fnv32(crc, pointer(aSharedObfuscationKey), len);
    // do not modify those hashes above or you will break obfuscation pattern!
    if i <= high(fCrypto) then
      fCrypto[i] := crc
    else
      fCryptoCRC := crc;
  end;
  // due to the weakness of the hash algorithms used, this approach is a bit
  // naive and would be broken easily with brute force - but point here is to
  // hide/obfuscate public values at end-user level (e.g. when publishing URIs),
  // not implement strong security, so it sounds good enough for our purpose
end;

destructor TSynUniqueIdentifierGenerator.Destroy;
begin
  fSafe.Done;
  FillCharFast(fCrypto, SizeOf(fCrypto), 0);
  fCryptoCRC := 0;
  inherited Destroy;
end;

type // compute a 24 hexadecimal chars (96 bits) obfuscated pseudo file name
  TSynUniqueIdentifierObfuscatedBits = packed record
    crc: cardinal;
    id: TSynUniqueIdentifierBits;
  end;

function TSynUniqueIdentifierGenerator.ToObfuscated(
  const aIdentifier: TSynUniqueIdentifier): TSynUniqueIdentifierObfuscated;
var
  bits: TSynUniqueIdentifierObfuscatedBits;
  key: cardinal;
begin
  result := '';
  if aIdentifier = 0 then
    exit;
  bits.id.Value := aIdentifier;
  if self = nil then
    key := 0
  else
    key := crc32ctab[0, bits.id.ProcessID and 1023] xor fCryptoCRC;
  bits.crc := crc32c(bits.id.ProcessID, @bits.id, SizeOf(bits.id)) xor key;
  if self <> nil then
    bits.id.Value := bits.id.Value xor PInt64(@fCrypto[high(fCrypto) - 1])^;
  result := BinToHex(@bits, SizeOf(bits));
end;

function TSynUniqueIdentifierGenerator.FromObfuscated(
  const aObfuscated: TSynUniqueIdentifierObfuscated;
  out aIdentifier: TSynUniqueIdentifier): boolean;
var
  bits: TSynUniqueIdentifierObfuscatedBits;
  len: integer;
  key: cardinal;
begin
  result := false;
  len := PosExChar('.', aObfuscated);
  if len = 0 then
    len := Length(aObfuscated)
  else
    dec(len); // trim right '.jpg'
  if (len <> SizeOf(bits) * 2) or
     not mormot.core.text.HexToBin(pointer(aObfuscated), @bits, SizeOf(bits)) then
    exit;
  if self = nil then
    key := 0
  else
  begin
    bits.id.Value := bits.id.Value xor PInt64(@fCrypto[high(fCrypto) - 1])^;
    key := crc32ctab[0, bits.id.ProcessID and 1023] xor fCryptoCRC;
  end;
  if crc32c(bits.id.ProcessID, @bits.id, SizeOf(bits.id)) xor key = bits.crc then
  begin
    aIdentifier := bits.id.Value;
    result := true;
  end;
end;



{ ****** IProtocol Safe Communication with Unilateral or Mutual Authentication }

function ToText(res: TProtocolResult): PShortString;
begin
  result := GetEnumName(TypeInfo(TProtocolResult), ord(res));
end;


{ TProtocolNone }

function TProtocolNone.ProcessHandshake(const MsgIn: RawUTF8;
  out MsgOut: RawUTF8): TProtocolResult;
begin
  result := sprUnsupported;
end;

function TProtocolNone.Decrypt(const aEncrypted: RawByteString;
  out aPlain: RawByteString): TProtocolResult;
begin
  aPlain := aEncrypted;
  result := sprSuccess;
end;

procedure TProtocolNone.Encrypt(const aPlain: RawByteString;
  out aEncrypted: RawByteString);
begin
  aEncrypted := aPlain;
end;

function TProtocolNone.Clone: IProtocol;
begin
  result := TProtocolNone.Create;
end;


{ TProtocolAES }

constructor TProtocolAES.Create(aClass: TAESAbstractClass;
  const aKey; aKeySize: cardinal; aIVReplayAttackCheck: TAESIVReplayAttackCheck);
begin
  inherited Create;
  InitializeCriticalSection(fSafe);
  fAES[false] := aClass.Create(aKey, aKeySize);
  fAES[false].IVReplayAttackCheck := aIVReplayAttackCheck;
  fAES[true] := fAES[false].Clone;
end;

constructor TProtocolAES.CreateFrom(aAnother: TProtocolAES);
begin
  inherited Create;
  InitializeCriticalSection(fSafe);
  fAES[false] := aAnother.fAES[false].Clone;
  fAES[true] := fAES[false].Clone;
end;

destructor TProtocolAES.Destroy;
begin
  fAES[false].Free;
  fAES[true].Free;
  DeleteCriticalSection(fSafe);
  inherited Destroy;
end;

function TProtocolAES.ProcessHandshake(const MsgIn: RawUTF8;
  out MsgOut: RawUTF8): TProtocolResult;
begin
  result := sprUnsupported;
end;

function TProtocolAES.Decrypt(const aEncrypted: RawByteString;
  out aPlain: RawByteString): TProtocolResult;
begin
  EnterCriticalSection(fSafe);;
  try
    try
      aPlain := fAES[false].DecryptPKCS7(aEncrypted, {iv=}true, {raise=}false);
      if aPlain = '' then
        result := sprBadRequest
      else
        result := sprSuccess;
    except
      result := sprInvalidMAC;
    end;
  finally
    LeaveCriticalSection(fSafe);
  end;
end;

procedure TProtocolAES.Encrypt(const aPlain: RawByteString;
  out aEncrypted: RawByteString);
begin
  EnterCriticalSection(fSafe);;
  try
    aEncrypted := fAES[true].EncryptPKCS7(aPlain, {iv=}true);
  finally
    LeaveCriticalSection(fSafe);
  end;
end;

function TProtocolAES.Clone: IProtocol;
begin
  result := TProtocolAESClass(ClassType).CreateFrom(self);
end;



procedure InitializeUnit;
begin
  Rtti.RegisterType(TypeInfo(TSignAlgo));
  Rtti.RegisterFromText(TypeInfo(TSynSignerParams),
    'algo:TSignAlgo secret,salt:RawUTF8 rounds:integer');
end;

procedure FinalizeUnit;
begin
end;


initialization
  InitializeUnit;
finalization
  FinalizeUnit;
end.
