/// Framework Core Authentication and Security Features
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.crypt.secure;

{
  *****************************************************************************

   Authentication and Security types shared by all framework units
    - TSyn***Password and TSynConnectionDefinition Classes
    - Reusable Authentication Classes
    - High-Level TSynSigner/TSynHasher Multi-Algorithm Wrappers
    - 64-bit TSynUniqueIdentifier and its efficient Generator
    - IProtocol Safe Communication with Unilateral or Mutual Authentication
    - TBinaryCookieGenerator Simple Cookie Generator

   Uses optimized mormot.crypt.core.pas for its actual process.

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
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
  mormot.crypt.core;


{ ***************** TSyn***Password and TSynConnectionDefinition Classes }

type
  /// abstract TSynPersistent class allowing safe storage of a password
  // - the associated Password, e.g. for storage or transmission encryption
  // will be persisted encrypted with a private key (which can be customized)
  // - if default simple symmetric encryption is not enough, it will also
  // read passwords strongly obfuscated for a given user using
  // mormot.crypt.core.pas' CryptDataForCurrentUser()
  // - a published property should be defined as such in inherited class:
  // ! property PasswordPropertyName: RawUtf8 read fPassword write fPassword;
  // - use the PassWordPlain property to access to its uncyphered value
  TSynPersistentWithPassword = class(TSynPersistent)
  protected
    fPassWord: SpiUtf8;
    fKey: cardinal;
    function GetKey: cardinal;
      {$ifdef HASINLINE}inline;{$endif}
    function GetPassWordPlain: SpiUtf8;
    function GetPassWordPlainInternal(AppSecret: RawUtf8): SpiUtf8;
    procedure SetPassWordPlain(const Value: SpiUtf8);
  public
    /// finalize the instance
    destructor Destroy; override;
    /// this class method could be used to compute the encrypted password,
    // ready to be stored as JSON, according to a given private key
    class function ComputePassword(const PlainPassword: SpiUtf8;
      CustomKey: cardinal = 0): SpiUtf8; overload;
    /// this class method could be used to compute the encrypted password from
    // a binary digest, ready to be stored as JSON, according to a given private key
    // - just a wrapper around ComputePassword(BinToBase64Uri())
    class function ComputePassword(PlainPassword: pointer; PlainPasswordLen: integer;
      CustomKey: cardinal = 0): SpiUtf8; overload;
    /// this class method could be used to decrypt a password, stored as JSON,
    // according to a given private key
    // - may trigger a ESynException if the password was stored using hardened
    // CryptDataForCurrentUser, and the current user doesn't match the
    // expected user stored in the field
    class function ComputePlainPassword(const CypheredPassword: SpiUtf8;
      CustomKey: cardinal = 0; const AppSecret: RawUtf8 = ''): SpiUtf8;
    /// the private key used to cypher the password storage on serialization
    // - application can override the default 0 value at runtime
    property Key: cardinal
      read GetKey write fKey;
    /// access to the associated unencrypted Password value
    // - may trigger a ESynException if the password was stored using hardened
    // CryptDataForCurrentUser, and the current user doesn't match the
    // expected user stored in the field
    property PasswordPlain: SpiUtf8
      read GetPassWordPlain write SetPassWordPlain;
  end;

type
  /// could be used to store a credential pair, as user name and password
  // - password will be stored with TSynPersistentWithPassword encryption
  TSynUserPassword = class(TSynPersistentWithPassword)
  protected
    fUserName: RawUtf8;
  published
    /// the associated user name
    property UserName: RawUtf8
      read fUserName write fUserName;
    /// the associated encrypted password
    // - use the PasswordPlain public property to access to the uncrypted password
    property Password: SpiUtf8
      read fPassword write fPassword;
  end;

  /// handle safe storage of any connection properties
  // - would be used by mormot.db to serialize TSqlDBConnectionProperties, or
  // by mormot.rest.core.pas to serialize TRest instances
  // - the password will be stored as Base64, after a simple encryption as
  // defined by TSynPersistentWithPassword
  // - typical content could be:
  // $ {
  // $	"Kind": "TSqlDBSQLite3ConnectionProperties",
  // $	"ServerName": "server",
  // $	"DatabaseName": "",
  // $	"User": "",
  // $	"Password": "PtvlPA=="
  // $ }
  // - the "Kind" value will be used to let the corresponding TRest or
  // TSqlDBConnectionProperties NewInstance*() class methods create the
  // actual instance, from its class name
  TSynConnectionDefinition = class(TSynPersistentWithPassword)
  protected
    fKind: string;
    fServerName: RawUtf8;
    fDatabaseName: RawUtf8;
    fUser: RawUtf8;
  public
    /// unserialize the database definition from JSON
    // - as previously serialized with the SaveToJson method
    // - you can specify a custom Key used for password encryption, if the
    // default value is not safe enough for you
    constructor CreateFromJson(const Json: RawUtf8; Key: cardinal = 0); virtual;
    /// serialize the database definition as JSON
    function SaveToJson: RawUtf8; virtual;
  published
    /// the class name implementing the connection or TRest instance
    // - will be used to instantiate the expected class type
    property Kind: string
      read fKind write fKind;
    /// the associated server name (or file, for SQLite3) to be connected to
    property ServerName: RawUtf8
      read fServerName write fServerName;
    /// the associated database name (if any), or additional options
    property DatabaseName: RawUtf8
      read fDatabaseName write fDatabaseName;
    /// the associated User Identifier (if any)
    property User: RawUtf8
      read fUser write fUser;
    /// the associated Password, e.g. for storage or transmission encryption
    // - will be persisted encrypted with a private key
    // - use the PassWordPlain property to access to its uncyphered value
    property Password: SpiUtf8
      read fPassword write fPassword;
  end;


/// simple symmetric obfuscation scheme using a 32-bit key
// - used e.g. by TSynPersistentWithPassword and mormot.db.proxy to obfuscate
// password or content - so it is not a real encryption
// - fast, but not cryptographically secure, since naively xor data bytes with
// crc32ctab[]: consider using mormot.crypt.core proven algorithms instead
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
      const UserName, PassWord: RawUtf8): cardinal; virtual;
    function GetPassword(const UserName: RawUtf8;
      out Password: RawUtf8): boolean; virtual; abstract;
    function GetUsersCount: integer; virtual; abstract;
    // check the given Hash challenge, against stored credentials
    function CheckCredentials(const UserName: RawUtf8; Hash: cardinal): boolean; virtual;
  public
    /// initialize the authentication scheme
    constructor Create;
    /// finalize the authentation
    destructor Destroy; override;
    /// register one credential for a given user
    // - this abstract method will raise an exception: inherited classes should
    // implement them as expected
    procedure AuthenticateUser(const aName, aPassword: RawUtf8); virtual;
    /// unregister one credential for a given user
    // - this abstract method will raise an exception: inherited classes should
    // implement them as expected
    procedure DisauthenticateUser(const aName: RawUtf8); virtual;
    /// create a new session
    // - should return 0 on authentication error, or an integer session ID
    // - this method will check the User name and password, and create a new session
    function CreateSession(const User: RawUtf8; Hash: cardinal): integer; virtual;
    /// check if the session exists in the internal list
    function SessionExists(aID: integer): boolean;
    /// delete a session
    procedure RemoveSession(aID: integer);
    /// returns the current identification token
    // - to be sent to the client for its authentication challenge
    function CurrentToken: Int64;
    /// the number of current opened sessions
    property SessionsCount: integer
      read fSessionsCount;
    /// the number of registered users
    property UsersCount: integer
      read GetUsersCount;
    /// to be used to compute a Hash on the client sude, for a given Token
    // - the token should have been retrieved from the server, and the client
    // should compute and return this hash value, to perform the authentication
    // challenge and create the session
    // - internal algorithm is not cryptographic secure, but fast and safe
    class function ComputeHash(Token: Int64;
      const UserName, PassWord: RawUtf8): cardinal; virtual;
  end;

  /// simple authentication class, implementing safe token/challenge security
  // - maintain a list of user / name credential pairs, and a list of sessions
  // - is not meant to handle authorization, just plain user access validation
  // - used e.g. by TSqlDBConnection.RemoteProcessMessage (on server side) and
  // TSqlDBProxyConnectionPropertiesAbstract (on client side) in mormot.db.proxy
  TSynAuthentication = class(TSynAuthenticationAbstract)
  protected
    fCredentials: TSynNameValue; // store user/password pairs
    function GetPassword(const UserName: RawUtf8;
      out Password: RawUtf8): boolean; override;
    function GetUsersCount: integer; override;
  public
    /// initialize the authentication scheme
    // - you can optionally register one user credential
    constructor Create(const aUserName: RawUtf8 = '';
      const aPassword: RawUtf8 = ''); reintroduce;
    /// register one credential for a given user
    procedure AuthenticateUser(const aName, aPassword: RawUtf8); override;
    /// unregister one credential for a given user
    procedure DisauthenticateUser(const aName: RawUtf8); override;
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
    function Add(const aIP: RawUtf8): boolean;
    /// unregister one IP to the list
    function Delete(const aIP: RawUtf8): boolean;
    /// returns true if the IP is in the list
    function Exists(const aIP: RawUtf8): boolean;
    /// creates a TDynArray wrapper around the stored list of values
    // - could be used e.g. for binary persistence
    // - warning: caller should make Safe.Unlock when finished
    function DynArrayLocked: TDynArray;
    /// low-level access to the internal IPv4 list
    // - 32-bit unsigned values are sorted, for fast O(log(n)) binary search
    property IP4: TIntegerDynArray
      read fIP4;
  published
    /// how many IPs are currently banned
    property Count: integer
      read fCount;
  end;


{ **************** 64-bit TSynUniqueIdentifier and its Efficient Generator }

type
  /// 64-bit integer unique identifier, as computed by TSynUniqueIdentifierGenerator
  // - they are increasing over time (so are much easier to store/shard/balance
  // than UUID/GUID), and contain generation time and a 16-bit process ID
  // - mapped by TSynUniqueIdentifierBits memory structure
  // - may be used on client side for something similar to a MongoDB ObjectID,
  // but compatible with TOrm.ID: TID properties
  TSynUniqueIdentifier = type TID;

  /// 16-bit unique process identifier, used to compute TSynUniqueIdentifier
  // - each TSynUniqueIdentifierGenerator instance is expected to have
  // its own unique process identifier, stored as a 16-bit integer 0..65535 value
  TSynUniqueIdentifierProcess = type word;

  {$A-}
  /// map 64-bit integer unique identifier internal memory structure
  // - as stored in TSynUniqueIdentifier = TID = Int64 values, and computed by
  // TSynUniqueIdentifierGenerator
  // - bits 0..14 map a 15-bit increasing counter (collision-free)
  // - bits 15..30 map a 16-bit process identifier
  // - bits 31..63 map a 33-bit UTC time, encoded as seconds since Unix epoch
  TSynUniqueIdentifierBits = object
  public
    /// the actual 64-bit storage value
    // - in practice, only first 63 bits are used
    Value: TSynUniqueIdentifier;
    /// extract the 15-bit counter (0..32767), starting with a random value
    function Counter: word;
      {$ifdef HASINLINE}inline;{$endif}
    /// extract the 16-bit unique process identifier
    // - as specified to TSynUniqueIdentifierGenerator constructor
    function ProcessID: TSynUniqueIdentifierProcess;
      {$ifdef HASINLINE}inline;{$endif}
    /// extract the UTC generation timestamp as seconds since the Unix epoch
    // - time is expressed in Coordinated Universal Time (UTC), not local time
    // - it uses in fact a 33-bit resolution, so is "Year 2038" bug-free
    function CreateTimeUnix: TUnixTime;
      {$ifdef HASINLINE}inline;{$endif}
    /// extract the UTC generation timestamp as TDateTime
    // - time is expressed in Coordinated Universal Time (UTC), not local time
    function CreateDateTime: TDateTime;
      {$ifdef HASINLINE}inline;{$endif}
    /// extract the UTC generation timestamp as our TTimeLog
    // - time is expressed in Coordinated Universal Time (UTC), not local time
    function CreateTimeLog: TTimeLog;
    /// fill this unique identifier structure from its TSynUniqueIdentifier value
    // - is just a wrapper around PInt64(@self)^
    procedure From(const AID: TSynUniqueIdentifier);
      {$ifdef HASINLINE}inline;{$endif}
    /// fill this unique identifier back from a 16 chars hexadecimal string
    // - returns TRUE if the supplied hexadecimal is on the expected format
    // - returns FALSE if the supplied text is invalid
    function FromHexa(const hexa: RawUtf8): boolean;
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
    /// compare two Identifiers
    function Equal(const Another: TSynUniqueIdentifierBits): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// convert the identifier into a 16 chars hexadecimal string
    function ToHexa: RawUtf8;
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
    procedure ToVariant(out Result: variant);
  end;
  {$A+}

  /// points to a 64-bit integer identifier, as computed by TSynUniqueIdentifierGenerator
  // - may be used to access the identifier internals, from its stored
  // Int64 or TSynUniqueIdentifier value

  PSynUniqueIdentifierBits = ^TSynUniqueIdentifierBits;

  /// a 24 chars cyphered hexadecimal string, mapping a TSynUniqueIdentifier
  // - has handled by TSynUniqueIdentifierGenerator.ToObfuscated/FromObfuscated
  TSynUniqueIdentifierObfuscated = type RawUtf8;

  /// thread-safe 64-bit integer unique identifier computation
  // - may be used on client side for something similar to a MongoDB ObjectID,
  // but compatible with TOrm.ID: TID properties, since it will contain
  // a 63-bit unsigned integer, following our ORM expectations
  // - each identifier would contain a 16-bit process identifier, which is
  // supplied by the application, and should be unique for this process at a
  // given time
  // - identifiers may be obfuscated as hexadecimal text, using both encryption
  // and digital signature
  // - all its methods are thread-safe, even during obfuscation processing
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
    fCryptoAesE, fCryptoAesD: TAes; // Initialized if aSharedObfuscationKeyNewKdf
    function GetComputedCount: Int64;
    function GetCollisions: Int64;
  public
    /// initialize the generator for the given 16-bit process identifier
    // - you can supply an obfuscation key, which should be shared for the
    // whole system, so that you may use FromObfuscated/ToObfuscated methods
    // - if aSharedObfuscationKeyNewKdf is > 0, indicates the rounds count for
    // a safer AES/SHA3 algorithm used for the obfuscation cryptography - keep
    // it as default 0 for mORMot 1.18 backward compatibility
    constructor Create(aIdentifier: TSynUniqueIdentifierProcess;
      const aSharedObfuscationKey: RawUtf8 = '';
      aSharedObfuscationKeyNewKdf: integer = 0); reintroduce;
    /// finalize the generator structure
    destructor Destroy; override;
    /// return a new unique ID
    // - this method is very optimized, and would use very little CPU
    procedure ComputeNew(out result: TSynUniqueIdentifierBits); overload;
    /// return a new unique ID, type-casted to an Int64
    function ComputeNew: Int64; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// return an ID matching this generator pattern, at a given timestamp
    // - may be used e.g. to limit database queries on a particular time range
    // - the ID is not guaranted to be unique, but match the supplied TDateTime
    procedure ComputeFromDateTime(const aDateTime: TDateTime;
      out result: TSynUniqueIdentifierBits);
    /// return an ID matching this generator pattern, at a given timestamp
    // - may be used e.g. to limit database queries on a particular time range
    // - the ID is not guaranted to be unique, but match the supplied TUnixTime
    procedure ComputeFromUnixTime(const aUnixTime: TUnixTime;
      out result: TSynUniqueIdentifierBits);
    /// map a TSynUniqueIdentifier as 24/32 chars cyphered hexadecimal text
    // - cyphering includes simple key-based encryption and a CRC-32 digital signature
    // - returned text size is 24 for the legacy format, and 32 chars if
    // aSharedObfuscationKeyNewKdf was set to true
    function ToObfuscated(
      const aIdentifier: TSynUniqueIdentifier): TSynUniqueIdentifierObfuscated;
    /// retrieve a TSynUniqueIdentifier from 24/32 chars cyphered hexadecimal text
    // - any file extension (e.g. '.jpeg') would be first deleted from the
    // supplied obfuscated text
    // - returns true if the supplied obfuscated text has the expected layout
    // and a valid digital signature
    // - returns false if the supplied obfuscated text is invalid
    // - note that this method will work for any TSynUniqueIdentifierProcess
    // of the same aSharedObfuscationKey - not only the Identifier of this node
    function FromObfuscated(const aObfuscated: TSynUniqueIdentifierObfuscated;
      out aIdentifier: TSynUniqueIdentifier): boolean;
    /// paranoid loop until LastUnixCreateTime
    // - may be called at server shutdown, if you expect a lot of collisions,
    // and want to ensure the "fake" timestamp match the time at server restart
    procedure WaitForSafeCreateTime(TimeOutSeconds: integer = 30);
    /// some 32-bit value, derivated from aSharedObfuscationKey as supplied
    // to the class constructor
    // - FromObfuscated and ToObfuscated methods will validate their hexadecimal
    // content with this value to secure the associated CRC
    // - may be used e.g. as system-depending salt
    property CryptoCRC: cardinal
      read fCryptoCRC;
    /// direct access to the associated mutex
    property Safe: TSynLocker
      read fSafe;
  published
    /// the process identifier, associated with this generator
    property Identifier: TSynUniqueIdentifierProcess
      read fIdentifier;
    /// how many times ComputeNew method has been called
    property ComputedCount: Int64
      read GetComputedCount;
    /// how many times ComputeNew method did have a collision and a fake
    // increased timestamp has been involved
    property Collisions: Int64
      read GetCollisions;
    /// low-level access to the last generated timestamp
    // - you may need to persist this value if a lot of Collisions happened, and
    // the timestamp was faked - you may also call WaitForSafeCreateTime
    property LastUnixCreateTime: cardinal
      read fUnixCreateTime write fUnixCreateTime;
  end;

  /// hold a dynamic array of TSynUniqueIdentifierGenerator instances
  TSynUniqueIdentifierGenerators = array of TSynUniqueIdentifierGenerator;



{ **************** High-Level TSynSigner/TSynHasher Multi-Algorithm Wrappers }

{ implemented in this unit and not in mormot.crypt.core, since TSynSignerParams
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

  /// JSON-serialization ready object as used by TSynSigner.Pbkdf2() overloaded methods
  // - default value for unspecified parameters will be SHAKE_128 with
  // rounds=1000 and a fixed salt
  TSynSignerParams = packed record
    algo: TSignAlgo;
    secret, salt: RawUtf8;
    rounds: integer;
  end;

  /// a generic wrapper object to handle digital HMAC-SHA-2/SHA-3 signatures
  // - used e.g. to implement TJwtSynSignerAbstract
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
    procedure Init(aAlgo: TSignAlgo; const aSecret: RawUtf8); overload;
    /// initialize the digital HMAC/SHA-3 signing context with some secret binary
    procedure Init(aAlgo: TSignAlgo;
      aSecret: pointer; aSecretLen: integer); overload;
    /// initialize the digital HMAC/SHA-3 signing context with PBKDF2 safe
    // iterative key derivation of a secret salted text
    procedure Init(aAlgo: TSignAlgo; const aSecret, aSalt: RawUtf8;
      aSecretPbkdf2Round: integer; aPbkdf2Secret: PHash512Rec = nil); overload;
    /// process some message content supplied as memory buffer
    procedure Update(aBuffer: pointer; aLen: integer); overload;
    /// process some message content supplied as string
    procedure Update(const aBuffer: RawByteString); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns the computed digital signature as lowercase hexadecimal text
    function Final: RawUtf8; overload;
    /// returns the raw computed digital signature
    // - SignatureSize bytes will be written: use Signature.Lo/h0/b3/b accessors
    procedure Final(out aSignature: THash512Rec;
      aNoInit: boolean = false); overload;
    /// one-step digital signature of a buffer as lowercase hexadecimal string
    function Full(aAlgo: TSignAlgo; const aSecret: RawUtf8;
      aBuffer: Pointer; aLen: integer): RawUtf8; overload;
    /// one-step digital signature of a buffer with PBKDF2 derivation
    function Full(aAlgo: TSignAlgo; const aSecret, aSalt: RawUtf8;
      aSecretPbkdf2Round: integer; aBuffer: Pointer; aLen: integer): RawUtf8; overload;
    /// convenient wrapper to perform PBKDF2 safe iterative key derivation
    procedure Pbkdf2(aAlgo: TSignAlgo; const aSecret, aSalt: RawUtf8;
      aSecretPbkdf2Round: integer; out aDerivatedKey: THash512Rec); overload;
    /// convenient wrapper to perform PBKDF2 safe iterative key derivation
    procedure Pbkdf2(const aParams: TSynSignerParams;
      out aDerivatedKey: THash512Rec); overload;
    /// convenient wrapper to perform PBKDF2 safe iterative key derivation
    // - accept as input a TSynSignerParams serialized as JSON object
    procedure Pbkdf2(aParamsJson: PUtf8Char; aParamsJsonLen: integer;
      out aDerivatedKey: THash512Rec;
      const aDefaultSalt: RawUtf8 = 'I6sWioAidNnhXO9BK';
      aDefaultAlgo: TSignAlgo = saSha3S128); overload;
    /// convenient wrapper to perform PBKDF2 safe iterative key derivation
    // - accept as input a TSynSignerParams serialized as JSON object
    procedure Pbkdf2(const aParamsJson: RawUtf8;
      out aDerivatedKey: THash512Rec;
      const aDefaultSalt: RawUtf8 = 'I6sWioAidNnhXO9BK';
      aDefaultAlgo: TSignAlgo = saSha3S128); overload;
    /// prepare a TAes object with the key derivated via a Pbkdf2() call
    // - aDerivatedKey is defined as "var", since it will be zeroed after use
    procedure AssignTo(var aDerivatedKey: THash512Rec;
      out aAES: TAes; aEncrypt: boolean);
    /// fill the internal context with zeros, for security
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
    function Final: RawUtf8; overload;
    /// returns the resulting hash as a binary buffer
    function Final(out aDigest: THash512Rec): integer; overload;
    /// one-step hash computation of a buffer as lowercase hexadecimal string
    function Full(aAlgo: THashAlgo; aBuffer: Pointer; aLen: integer): RawUtf8; overload;
    /// one-step hash computation of a buffer as a binary buffer
    function Full(aAlgo: THashAlgo; aBuffer: Pointer; aLen: integer;
      out aDigest: THash512Rec): integer; overload;
    /// returns the number of bytes of the hash of the current Algo
    function HashSize: integer;
    /// the hash algorithm used by this instance
    property Algo: THashAlgo
      read fAlgo;
  end;

  /// TStreamRedirect with TSynHasher cryptographic hashing
  // - do not use this abstract class but inherited with overloaded GetAlgo
  TStreamRedirectSynHasher = class(TStreamRedirect)
  protected
    fHash: TSynHasher;
    class function GetAlgo: THashAlgo; virtual; abstract;
    procedure DoHash(data: pointer; len: integer); override;
  public
    constructor Create(aDestination: TStream; aRead: boolean = false); override;
    function GetHash: RawUtf8; override;
    class function GetHashFileExt: RawUtf8; override;
  end;

  /// TStreamRedirect with MD5 cryptographic hashing
  TStreamRedirectMd5 = class(TStreamRedirectSynHasher)
  protected
    class function GetAlgo: THashAlgo; override;
  end;

  /// TStreamRedirect with SHA-1 cryptographic hashing
  TStreamRedirectSha1 = class(TStreamRedirectSynHasher)
  protected
    class function GetAlgo: THashAlgo; override;
  end;

  /// TStreamRedirect with SHA-256 cryptographic hashing
  TStreamRedirectSha256 = class(TStreamRedirectSynHasher)
  protected
    class function GetAlgo: THashAlgo; override;
  end;

  /// TStreamRedirect with SHA-384 cryptographic hashing
  TStreamRedirectSha384 = class(TStreamRedirectSynHasher)
  protected
    class function GetAlgo: THashAlgo; override;
  end;

  /// TStreamRedirect with SHA-512 cryptographic hashing
  TStreamRedirectSha512 = class(TStreamRedirectSynHasher)
  protected
    class function GetAlgo: THashAlgo; override;
  end;

  /// TStreamRedirect with SHA-3-256 cryptographic hashing
  TStreamRedirectSha3_256 = class(TStreamRedirectSynHasher)
  protected
    class function GetAlgo: THashAlgo; override;
  end;

  /// TStreamRedirect with SHA-3-512 cryptographic hashing
  TStreamRedirectSha3_512 = class(TStreamRedirectSynHasher)
  protected
    class function GetAlgo: THashAlgo; override;
  end;




function ToText(algo: TSignAlgo): PShortString; overload;
function ToText(algo: THashAlgo): PShortString; overload;


/// compute the hexadecimal hash of any (big) file
// - using a temporary buffer of 1MB for the sequential reading
function HashFile(const aFileName: TFileName; aAlgo: THashAlgo): RawUtf8; overload;

/// compute the hexadecimal hashe(s) of one file, as external .md5/.sha256/.. files
// - reading the file once in memory, then apply all algorithms on it and
// generate the text hash files in the very same folder
procedure HashFile(const aFileName: TFileName; aAlgos: THashAlgos); overload;

/// one-step hash computation of a buffer as lowercase hexadecimal string
function HashFull(aAlgo: THashAlgo; aBuffer: Pointer; aLen: integer): RawUtf8;

/// compute the MD5 checksum of a given file
// - this function maps the THashFile signature as defined in mormot.core.buffers
function HashFileMd5(const FileName: TFileName): RawUtf8;

/// compute the SHA-1 checksum of a given file
// - this function maps the THashFile signature as defined in mormot.core.buffers
function HashFileSha1(const FileName: TFileName): RawUtf8;

/// compute the SHA-256 checksum of a given file
// - this function maps the THashFile signature as defined in mormot.core.buffers
function HashFileSha256(const FileName: TFileName): RawUtf8;

/// compute the SHA-384 checksum of a given file
// - this function maps the THashFile signature as defined in mormot.core.buffers
function HashFileSha384(const FileName: TFileName): RawUtf8;

/// compute the SHA-512 checksum of a given file
// - this function maps the THashFile signature as defined in mormot.core.buffers
function HashFileSha512(const FileName: TFileName): RawUtf8;

/// compute the SHA-3-256 checksum of a given file
// - this function maps the THashFile signature as defined in mormot.core.buffers
function HashFileSha3_256(const FileName: TFileName): RawUtf8;

/// compute the SHA-3-512 checksum of a given file
// - this function maps the THashFile signature as defined in mormot.core.buffers
function HashFileSha3_512(const FileName: TFileName): RawUtf8;



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
  // - see e.g. TProtocolNone or mormot.crypt.ecc's TEcdheProtocolClient and
  // TEcdheProtocolServer implementation classes
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
    function ProcessHandshake(const MsgIn: RawUtf8;
      out MsgOut: RawUtf8): TProtocolResult;
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
    function ProcessHandshake(const MsgIn: RawUtf8;
      out MsgOut: RawUtf8): TProtocolResult;
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

  /// implements a secure protocol using symetric AES encryption
  // - as used e.g. by 'synopsebinary' WebSockets protocol
  // - a secret password is shared between client and server
  // - this class will maintain two TAesAbstract instances, one for encryption
  // and another for decryption, with PKCS7 padding and no MAC/AEAD validation
  TProtocolAes = class(TInterfacedObject, IProtocol)
  protected
    fSafe: TRTLCriticalSection; // no need of TSynLocker padding
    fAes: array[boolean] of TAesAbstract; // [false]=decrypt [true]=encrypt
  public
    /// initialize this encryption protocol with the given AES settings
    // - warning: aKey is an untyped constant, i.e. expects a raw set of memory
    // bytes: do NOT use assign it with a string or a TBytes instance
    // - if aClass is nil, TAesFast[mCtr] will be used as default
    // - AEAD Cfc,mOfc,mCtc,mGcm modes will be rejected since unsupported
    constructor Create(aClass: TAesAbstractClass; const aKey; aKeySize: cardinal);
      reintroduce; virtual;
    /// will create another instance of this communication protocol
    constructor CreateFrom(aAnother: TProtocolAes); reintroduce; virtual;
    /// finalize the encryption
    destructor Destroy; override;
    /// initialize the communication by exchanging some client/server information
    // - this method will return sprUnsupported
    function ProcessHandshake(const MsgIn: RawUtf8;
      out MsgOut: RawUtf8): TProtocolResult;
    /// encrypt a message on one side, ready to be transmitted to the other side
    // - wrapper around fAes[true].EncryptPkcs7(), so does not support AEAD yet
    procedure Encrypt(const aPlain: RawByteString;
      out aEncrypted: RawByteString);
    /// decrypt a message on one side, as transmitted from the other side
    // - wrapper around fAes[false].DecryptPkcs7(), so does not support AEAD yet
    function Decrypt(const aEncrypted: RawByteString;
      out aPlain: RawByteString): TProtocolResult;
    /// will create another instance of this communication protocol
    function Clone: IProtocol;
  end;

  /// class-reference type (metaclass) of an AES secure protocol
  TProtocolAesClass = class of TProtocolAes;

function ToText(res: TProtocolResult): PShortString; overload;



{ ******* TBinaryCookieGenerator Simple Cookie Generator }

type
  /// a 31-bit increasing sequence used for TBinaryCookieGenerator sessions
  TBinaryCookieGeneratorSessionID = type integer;

  {$A-}
  /// efficient thread-safe cookie generation
  // - you can see it as a JWT-Of-The-Poor: faster to parse and validate
  // its content, and with very efficiently binary-based serialization
  // - stores a session ID, cookie name, encryption and HMAC secret keys
  // - can optionally store any associated record as efficient binary
  // - it is NOT cryptographic secure, because cookies are not, but it is
  // strong enough to avoid naive attacks, and uses less space than a JWT
  {$ifdef USERECORDWITHMETHODS}
  TBinaryCookieGenerator = record
  {$else}
  TBinaryCookieGenerator = object
  {$endif USERECORDWITHMETHODS}
    /// the cookie name, used for storage in the client side HTTP headers
    // - is not part of the Generate/Validate content
    CookieName: RawUtf8;
    /// an increasing counter, to implement unique session ID
    SessionSequence: TBinaryCookieGeneratorSessionID;
    /// secret information, used for HMAC digital signature of cookie content
    Secret: THmacCrc32c;
    /// random IV used as CTR on Crypt[] secret key
    CryptNonce: cardinal;
    /// used when Generate() has TimeOutMinutes=0
    DefaultTimeOutMinutes: cardinal;
    /// secret information, used for encryption of the cookie content
    Crypt: array[byte] of byte;
    /// initialize ephemeral temporary cookie generation
    procedure Init(const Name: RawUtf8 = 'mORMot';
      DefaultSessionTimeOutMinutes: cardinal = 0);
    /// low-level wrapper to cipher/uncipher a cookie binary content
    procedure Cipher(P: PAnsiChar; bytes: integer);
    /// will initialize a new Base64Uri-encoded session cookie
    // - with an optional record data
    // - will return the 32-bit internal session ID and
    // - you can supply a time period, after which the session will expire -
    // default is 1 hour, and could go up to
    function Generate(out Cookie: RawUtf8; TimeOutMinutes: cardinal = 0;
      PRecordData: pointer = nil; PRecordTypeInfo: PRttiInfo = nil
      ): TBinaryCookieGeneratorSessionID;
    ///  decode a base64uri cookie and optionally fill an associated record
    // - return the associated session/sequence number, 0 on error
    function Validate(const cookie: RawUtf8;
      PRecordData: pointer = nil; PRecordTypeInfo: PRttiInfo = nil;
      PExpires: PCardinal = nil; PIssued: PCardinal = nil): TBinaryCookieGeneratorSessionID;
    /// allow the very same cookie to be recognized after server restart
    function Save: RawUtf8;
    /// unserialize the cookie generation context as serialized by Save
    function Load(const Saved: RawUtf8): boolean;
  end;
  {$A+}

  PBinaryCookieGenerator = ^TBinaryCookieGenerator;



implementation


{ **************** High-Level TSynSigner/TSynHasher Multi-Algorithm Wrappers }

{ TSynHasher }

function TSynHasher.Init(aAlgo: THashAlgo): boolean;
begin
  fAlgo := aAlgo;
  result := true;
  case aAlgo of
    hfMD5:
      PMd5(@ctxt)^.Init;
    hfSHA1:
      PSha1(@ctxt)^.Init;
    hfSHA256:
      PSha256(@ctxt)^.Init;
    hfSHA384:
      PSha384(@ctxt)^.Init;
    hfSHA512:
      PSha512(@ctxt)^.Init;
    hfSHA3_256:
      PSha3(@ctxt)^.Init(SHA3_256);
    hfSHA3_512:
      PSha3(@ctxt)^.Init(SHA3_512);
  else
    result := false;
  end;
end;

procedure TSynHasher.Update(aBuffer: Pointer; aLen: integer);
begin
  case fAlgo of
    hfMD5:
      PMd5(@ctxt)^.Update(aBuffer^, aLen);
    hfSHA1:
      PSha1(@ctxt)^.Update(aBuffer, aLen);
    hfSHA256:
      PSha256(@ctxt)^.Update(aBuffer, aLen);
    hfSHA384:
      PSha384(@ctxt)^.Update(aBuffer, aLen);
    hfSHA512:
      PSha512(@ctxt)^.Update(aBuffer, aLen);
    hfSHA3_256:
      PSha3(@ctxt)^.Update(aBuffer, aLen);
    hfSHA3_512:
      PSha3(@ctxt)^.Update(aBuffer, aLen);
  end;
end;

procedure TSynHasher.Update(const aBuffer: RawByteString);
begin
  Update(pointer(aBuffer), length(aBuffer));
end;

function TSynHasher.Final: RawUtf8;
var
  dig: THash512Rec;
begin
  BinToHexLower(@dig, Final(dig), result);
end;

const
  HASH_SIZE: array[THashAlgo] of integer = (
    SizeOf(TMd5Digest), SizeOf(TSHA1Digest), SizeOf(TSHA256Digest),
    SizeOf(TSHA384Digest), SizeOf(TSHA512Digest), SizeOf(THash256),
    SizeOf(THash512));

function TSynHasher.HashSize: integer;
begin
  result := HASH_SIZE[fAlgo];
end;

function TSynHasher.Final(out aDigest: THash512Rec): integer;
begin
  case fAlgo of
    hfMD5:
      PMd5(@ctxt)^.Final(aDigest.h0);
    hfSHA1:
      PSha1(@ctxt)^.Final(aDigest.b160);
    hfSHA256:
      PSha256(@ctxt)^.Final(aDigest.Lo);
    hfSHA384:
      PSha384(@ctxt)^.Final(aDigest.b384);
    hfSHA512:
      PSha512(@ctxt)^.Final(aDigest.b);
    hfSHA3_256:
      PSha3(@ctxt)^.Final(aDigest.Lo);
    hfSHA3_512:
      PSha3(@ctxt)^.Final(aDigest.b);
  end;
  result := HASH_SIZE[fAlgo];
end;

function TSynHasher.Full(aAlgo: THashAlgo; aBuffer: Pointer; aLen: integer): RawUtf8;
begin
  Init(aAlgo);
  Update(aBuffer, aLen);
  result := Final;
end;

function TSynHasher.Full(aAlgo: THashAlgo; aBuffer: Pointer; aLen: integer;
  out aDigest: THash512Rec): integer;
begin
  Init(aAlgo);
  Update(aBuffer, aLen);
  result := Final(aDigest);
end;


{ TStreamRedirectSynHasher }

constructor TStreamRedirectSynHasher.Create(aDestination: TStream; aRead: boolean);
begin
  inherited Create(aDestination, aRead);
  fHash.Init(GetAlgo);
end;

procedure TStreamRedirectSynHasher.DoHash(data: pointer; len: integer);
begin
  fHash.Update(data, len);
end;

function TStreamRedirectSynHasher.GetHash: RawUtf8;
begin
  result := fHash.Final;
end;

const
  HASH_EXT: array[THashAlgo] of RawUtf8 = (
    '.md5', '.sha1', '.sha256', '.sha384', '.sha512', '.sha3-256', '.sha3-512');

class function TStreamRedirectSynHasher.GetHashFileExt: RawUtf8;
begin
  result := HASH_EXT[GetAlgo];
end;


{ TStreamRedirectSha3_512 }

class function TStreamRedirectSha3_512.GetAlgo: THashAlgo;
begin
  result := hfSHA3_512;
end;

{ TStreamRedirectSha3_256 }

class function TStreamRedirectSha3_256.GetAlgo: THashAlgo;
begin
  result := hfSHA3_256;
end;

{ TStreamRedirectSha512 }

class function TStreamRedirectSha512.GetAlgo: THashAlgo;
begin
  result := hfSHA512;
end;

{ TStreamRedirectSha384 }

class function TStreamRedirectSha384.GetAlgo: THashAlgo;
begin
  result := hfSHA384;
end;

{ TStreamRedirectSha256 }

class function TStreamRedirectSha256.GetAlgo: THashAlgo;
begin
  result := hfSHA256;
end;

{ TStreamRedirectSha1 }

class function TStreamRedirectSha1.GetAlgo: THashAlgo;
begin
  result := hfSHA1;
end;

{ TStreamRedirectMd5 }

class function TStreamRedirectMd5.GetAlgo: THashAlgo;
begin
  result := hfMD5;
end;



function HashFull(aAlgo: THashAlgo; aBuffer: Pointer; aLen: integer): RawUtf8;
var
  hasher: TSynHasher;
begin
  result := hasher.Full(aAlgo, aBuffer, aLen);
end;

function HashFile(const aFileName: TFileName; aAlgo: THashAlgo): RawUtf8;
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
  if ValidHandle(F) then
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
  data, hash: RawUtf8;
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
        FormatUtf8('% *%',
          [HashFull(a, pointer(data), length(data)), efn], hash);
        FormatString('%.%',
          [efn, LowerCase(TrimLeftLowerCaseShort(ToText(a)))], fn);
        FileFromString(hash, fn);
      end;
end;

function HashFileMd5(const FileName: TFileName): RawUtf8;
begin
  result := HashFile(FileName, hfMD5);
end;

function HashFileSha1(const FileName: TFileName): RawUtf8;
begin
  result := HashFile(FileName, hfSHA1);
end;

function HashFileSha256(const FileName: TFileName): RawUtf8;
begin
  result := HashFile(FileName, hfSHA256);
end;

function HashFileSha384(const FileName: TFileName): RawUtf8;
begin
  result := HashFile(FileName, hfSHA384);
end;

function HashFileSha512(const FileName: TFileName): RawUtf8;
begin
  result := HashFile(FileName, hfSHA512);
end;

function HashFileSha3_256(const FileName: TFileName): RawUtf8;
begin
  result := HashFile(FileName, hfSHA3_256);
end;

function HashFileSha3_512(const FileName: TFileName): RawUtf8;
begin
  result := HashFile(FileName, hfSHA3_512);
end;



{ TSynSigner }

procedure TSynSigner.Init(aAlgo: TSignAlgo; aSecret: pointer; aSecretLen: integer);
const
  SIGN_SIZE: array[TSignAlgo] of byte = (
    20, 32, 48, 64, 28, 32, 48, 64, 32, 64);
  SHA3_ALGO: array[saSha3224..saSha3S256] of TSha3Algo = (
    SHA3_224, SHA3_256, SHA3_384, SHA3_512, SHAKE_128, SHAKE_256);
begin
  Algo := aAlgo;
  SignatureSize := SIGN_SIZE[Algo];
  case Algo of
    saSha1:
      PHmacSha1(@ctxt)^.Init(aSecret, aSecretLen);
    saSha256:
      PHmacSha256(@ctxt)^.Init(aSecret, aSecretLen);
    saSha384:
      PHmacSha384(@ctxt)^.Init(aSecret, aSecretLen);
    saSha512:
      PHmacSha512(@ctxt)^.Init(aSecret, aSecretLen);
    saSha3224..saSha3S256:
      begin
        PSha3(@ctxt)^.Init(SHA3_ALGO[Algo]);
        PSha3(@ctxt)^.Update(aSecret, aSecretLen);
      end; // note: the HMAC pattern is included in SHA-3
  end;
end;

procedure TSynSigner.Init(aAlgo: TSignAlgo; const aSecret: RawUtf8);
begin
  Init(aAlgo, pointer(aSecret), length(aSecret));
end;

procedure TSynSigner.Init(aAlgo: TSignAlgo; const aSecret, aSalt: RawUtf8;
  aSecretPbkdf2Round: integer; aPbkdf2Secret: PHash512Rec);
var
  temp: THash512Rec;
begin
  if aSecretPbkdf2Round > 1 then
  begin
    Pbkdf2(aAlgo, aSecret, aSalt, aSecretPbkdf2Round, temp);
    Init(aAlgo, @temp, SignatureSize);
    if aPbkdf2Secret <> nil then
      aPbkdf2Secret^ := temp;
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
      PHmacSha1(@ctxt)^.Update(aBuffer, aLen);
    saSha256:
      PHmacSha256(@ctxt)^.Update(aBuffer, aLen);
    saSha384:
      PHmacSha384(@ctxt)^.Update(aBuffer, aLen);
    saSha512:
      PHmacSha512(@ctxt)^.Update(aBuffer, aLen);
    saSha3224..saSha3S256:
      PSha3(@ctxt)^.Update(aBuffer, aLen);
  end;
end;

procedure TSynSigner.Final(out aSignature: THash512Rec; aNoInit: boolean);
begin
  case Algo of
    saSha1:
      PHmacSha1(@ctxt)^.Done(aSignature.b160, aNoInit);
    saSha256:
      PHmacSha256(@ctxt)^.Done(aSignature.Lo, aNoInit);
    saSha384:
      PHmacSha384(@ctxt)^.Done(aSignature.b384, aNoInit);
    saSha512:
      PHmacSha512(@ctxt)^.Done(aSignature.b, aNoInit);
    saSha3224..saSha3S256:
      PSha3(@ctxt)^.Final(@aSignature, SignatureSize shl 3, aNoInit);
  end;
end;

function TSynSigner.Final: RawUtf8;
var
  sig: THash512Rec;
begin
  Final(sig);
  result := BinToHexLower(@sig, SignatureSize);
end;

function TSynSigner.Full(aAlgo: TSignAlgo; const aSecret: RawUtf8;
  aBuffer: Pointer; aLen: integer): RawUtf8;
begin
  Init(aAlgo, aSecret);
  Update(aBuffer, aLen);
  result := Final;
end;

function TSynSigner.Full(aAlgo: TSignAlgo; const aSecret, aSalt: RawUtf8;
  aSecretPbkdf2Round: integer; aBuffer: Pointer; aLen: integer): RawUtf8;
begin
  Init(aAlgo, aSecret, aSalt, aSecretPbkdf2Round);
  Update(aBuffer, aLen);
  result := Final;
end;

procedure TSynSigner.Pbkdf2(aAlgo: TSignAlgo; const aSecret, aSalt: RawUtf8;
  aSecretPbkdf2Round: integer; out aDerivatedKey: THash512Rec);
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
  if aSecretPbkdf2Round < 2 then
    exit;
  temp := aDerivatedKey;
  for i := 2 to aSecretPbkdf2Round do
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

procedure TSynSigner.Pbkdf2(const aParams: TSynSignerParams;
  out aDerivatedKey: THash512Rec);
begin
  Pbkdf2(aParams.algo, aParams.secret, aParams.salt, aParams.rounds, aDerivatedKey);
end;

procedure TSynSigner.Pbkdf2(aParamsJson: PUtf8Char; aParamsJsonLen: integer;
  out aDerivatedKey: THash512Rec; const aDefaultSalt: RawUtf8; aDefaultAlgo: TSignAlgo);
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
  if (aParamsJson = nil) or
     (aParamsJsonLen <= 0) then
    k.secret := aDefaultSalt
  else if aParamsJson[1] <> '{' then
    FastSetString(k.secret, aParamsJson, aParamsJsonLen)
  else
  begin
    tmp.Init(aParamsJson, aParamsJsonLen);
    try
      if (RecordLoadJson(k, tmp.buf, TypeInfo(TSynSignerParams)) = nil) or
         (k.secret = '') or
         (k.salt = '') then
      begin
        SetDefault;
        FastSetString(k.secret, aParamsJson, aParamsJsonLen);
      end;
    finally
      FillCharFast(tmp.buf^, tmp.len, 0);
      tmp.Done;
    end;
  end;
  Pbkdf2(k.algo, k.secret, k.salt, k.rounds, aDerivatedKey);
  FillZero(k.secret);
end;

procedure TSynSigner.Pbkdf2(const aParamsJson: RawUtf8;
  out aDerivatedKey: THash512Rec; const aDefaultSalt: RawUtf8; aDefaultAlgo: TSignAlgo);
begin
  Pbkdf2(pointer(aParamsJson), length(aParamsJson),
    aDerivatedKey, aDefaultSalt, aDefaultAlgo);
end;

procedure TSynSigner.AssignTo(var aDerivatedKey: THash512Rec;
  out aAES: TAes; aEncrypt: boolean);
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
  {$endif FPC}
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
  const PlainPassword: SpiUtf8; CustomKey: cardinal): SpiUtf8;
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
  PlainPasswordLen: integer; CustomKey: cardinal): SpiUtf8;
begin
  result := ComputePassword(BinToBase64uri(PlainPassword, PlainPasswordLen));
end;

class function TSynPersistentWithPassword.ComputePlainPassword(
  const CypheredPassword: SpiUtf8; CustomKey: cardinal;
  const AppSecret: RawUtf8): SpiUtf8;
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

function TSynPersistentWithPassword.GetPassWordPlain: SpiUtf8;
begin
  result := GetPassWordPlainInternal('');
end;

function TSynPersistentWithPassword.GetPassWordPlainInternal(
  AppSecret: RawUtf8): SpiUtf8;
var
  value, pass: RawByteString;
  usr: RawUtf8;
  i, j: integer;
begin
  result := '';
  if (self = nil) or
     (fPassWord = '') then
    exit;
  if AppSecret = '' then
    ClassToText(ClassType, AppSecret);
  usr := Executable.User + ':';
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
      raise ESynException.CreateUtf8('%.GetPassWordPlain unable to retrieve the ' +
        'stored value: current user is [%], but password in % was encoded for [%]',
        [self, Executable.User, AppSecret, copy(fPassword, 1, i - 1)]);
  end;
  if result = '' then
  begin
    value := Base64ToBin(fPassWord);
    SymmetricEncrypt(GetKey, value);
    result := value;
  end;
end;

procedure TSynPersistentWithPassword.SetPassWordPlain(const Value: SpiUtf8);
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

constructor TSynConnectionDefinition.CreateFromJson(const Json: RawUtf8; Key: cardinal);
var
  privateCopy: RawUtf8;
  values: array[0..4] of TValuePUtf8Char;
begin
  fKey := Key;
  privateCopy := Json;
  JsonDecode(privateCopy,
    ['Kind', 'ServerName', 'DatabaseName', 'User', 'Password'], @values);
  fKind := values[0].ToString;
  values[1].ToUtf8(fServerName);
  values[2].ToUtf8(fDatabaseName);
  values[3].ToUtf8(fUser);
  fPassWord := values[4].ToUtf8;
end;

function TSynConnectionDefinition.SaveToJson: RawUtf8;
begin
  result := JsonEncode([
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
  const UserName, PassWord: RawUtf8): cardinal;
begin
  // rough authentication - xxHash32 is less reversible than crc32c
  result := xxHash32( xxHash32( xxHash32(
    Token, @Token, SizeOf(Token)),
    pointer(UserName), length(UserName)),
    pointer(PassWord), length(PassWord));
end;

function TSynAuthenticationAbstract.ComputeCredential(previous: boolean;
  const UserName, PassWord: RawUtf8): cardinal;
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

procedure TSynAuthenticationAbstract.AuthenticateUser(const aName, aPassword: RawUtf8);
begin
  raise ESynException.CreateUtf8('%.AuthenticateUser() is not implemented', [self]);
end;

procedure TSynAuthenticationAbstract.DisauthenticateUser(const aName: RawUtf8);
begin
  raise ESynException.CreateUtf8('%.DisauthenticateUser() is not implemented', [self]);
end;

function TSynAuthenticationAbstract.CheckCredentials(const UserName: RawUtf8;
  Hash: cardinal): boolean;
var
  password: RawUtf8;
begin
  result := GetPassword(UserName, password) and
    ((ComputeCredential({previous=}false, UserName, password{%H-}) = Hash) or
     (ComputeCredential({previous=}true,  UserName, password) = Hash));
end;

function TSynAuthenticationAbstract.CreateSession(const User: RawUtf8;
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

constructor TSynAuthentication.Create(const aUserName, aPassword: RawUtf8);
begin
  inherited Create;
  fCredentials.Init(true);
  if aUserName <> '' then
    AuthenticateUser(aUserName, aPassword);
end;

function TSynAuthentication.GetPassword(const UserName: RawUtf8;
  out Password: RawUtf8): boolean;
var
  i: integer;
begin
  // caller did protect this method via fSafe.Lock
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

procedure TSynAuthentication.AuthenticateUser(const aName, aPassword: RawUtf8);
begin
  fSafe.Lock;
  try
    fCredentials.Add(aName, aPassword);
  finally
    fSafe.UnLock;
  end;
end;

procedure TSynAuthentication.DisauthenticateUser(const aName: RawUtf8);
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

function TIPBan.Add(const aIP: RawUtf8): boolean;
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

function TIPBan.Delete(const aIP: RawUtf8): boolean;
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

function TIPBan.Exists(const aIP: RawUtf8): boolean;
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

procedure TSynUniqueIdentifierBits.ToVariant(out Result: variant);
begin
  TDocVariantData(Result).InitObject([
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

function TSynUniqueIdentifierBits.ToHexa: RawUtf8;
begin
  Int64ToHex(Value, result);
end;

function TSynUniqueIdentifierBits.FromHexa(const hexa: RawUtf8): boolean;
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
  SYNUNIQUEGEN_COLLISIONCOUNT = 0;

procedure TSynUniqueIdentifierGenerator.ComputeNew(
  out result: TSynUniqueIdentifierBits);
var
  currentTime: cardinal;
begin
  currentTime := UnixTimeUtc; // under Windows faster than GetTickCount64
  fSafe.Lock;
  try
    if currentTime > fUnixCreateTime then
    begin
      fUnixCreateTime := currentTime;
      fLastCounter := 0; // reset
    end;
    if fLastCounter = $7fff then
    begin
      // collision (unlikely) -> cheat on timestamp
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

function TSynUniqueIdentifierGenerator.GetCollisions: Int64;
begin
  result := fSafe.LockedInt64[SYNUNIQUEGEN_COLLISIONCOUNT];
end;

procedure TSynUniqueIdentifierGenerator.ComputeFromDateTime(
  const aDateTime: TDateTime; out result: TSynUniqueIdentifierBits);
begin
  // assume fLastCounter=0
  ComputeFromUnixTime(DateTimeToUnixTime(aDateTime), result);
end;

procedure TSynUniqueIdentifierGenerator.ComputeFromUnixTime(const aUnixTime: TUnixTime;
  out result: TSynUniqueIdentifierBits);
begin
  // assume fLastCounter=0
  result.Value := aUnixTime shl 31;
  if self <> nil then
    result.Value := result.Value or fIdentifierShifted;
end;

constructor TSynUniqueIdentifierGenerator.Create(
  aIdentifier: TSynUniqueIdentifierProcess; const aSharedObfuscationKey: RawUtf8;
  aSharedObfuscationKeyNewKdf: integer);
var
  i, len: integer;
  crc: cardinal;
  key: THash256Rec;
begin
  fIdentifier := aIdentifier;
  fIdentifierShifted := aIdentifier shl 15;
  fSafe.Init;
  fSafe.LockedInt64[SYNUNIQUEGEN_COMPUTECOUNT] := 0;
  fSafe.LockedInt64[SYNUNIQUEGEN_COLLISIONCOUNT] := 0;
  // compute obfuscation key using hash diffusion of the supplied text
  len := length(aSharedObfuscationKey);
  if aSharedObfuscationKeyNewKdf > 0 then
  begin
    // efficient and safe obfuscation based on proven algoriths (AES + SHA3)
    Pbkdf2Sha3(SHA3_256, aSharedObfuscationKey,
       ToText(ClassType), aSharedObfuscationKeyNewKdf, @key);
    fCryptoAesE.EncryptInit(key, 128);
    fCryptoAesD.DecryptInitFrom(fCryptoAesE, key, 128);
    fCryptoCRC := key.c[7];
    // fCrypto[] is not used if fCryptoAes*.Initialized are set
  end
  else
  begin
    // due to the weakness of the hash algorithms used, this approach is a bit
    // naive and would be broken easily with brute force - but point here is to
    // hide/obfuscate public values at end-user level (e.g. when publishing URIs),
    // not implement strong security, so it sounds good enough for our purpose
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
  end;
end;

destructor TSynUniqueIdentifierGenerator.Destroy;
begin
  fSafe.Done;
  fCryptoAesE.Done;
  fCryptoAesD.Done;
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
  block: THash128Rec;
  bits: TSynUniqueIdentifierObfuscatedBits absolute block;
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
    if fCryptoAesE.Initialized then
    begin
      block.c3 := fCryptoCRC; // used as IV during AES permutation
      fCryptoAesE.Encrypt(block.b);
      result := BinToHexLower(@block, SizeOf(block)); // 32 hexa chars
      exit;
    end
    else
      bits.id.Value := bits.id.Value xor PInt64(@fCrypto[high(fCrypto) - 1])^;
  result := BinToHexLower(@bits, SizeOf(bits)); // 24 hexa chars
end;

function TSynUniqueIdentifierGenerator.FromObfuscated(
  const aObfuscated: TSynUniqueIdentifierObfuscated;
  out aIdentifier: TSynUniqueIdentifier): boolean;
var
  block: THash128Rec;
  bits: TSynUniqueIdentifierObfuscatedBits absolute block;
  len: integer;
  key: cardinal;
begin
  result := false;
  len := PosExChar('.', aObfuscated);
  if len = 0 then
    len := Length(aObfuscated)
  else
    dec(len); // trim right '.jpg'
  if (self <> nil) and
     fCryptoAesD.Initialized then
  begin
    if (len <> SizeOf(block) * 2) or // 32 hexa chars
       not mormot.core.text.HexToBin(pointer(aObfuscated), @block, SizeOf(block)) then
      exit;
    fCryptoAesD.Decrypt(block.b, block.b);
    if block.c3 <> fCryptoCRC then
      exit;
    key := crc32ctab[0, bits.id.ProcessID and 1023] xor fCryptoCRC;
  end
  else
  begin
    if (len <> SizeOf(bits) * 2) or // 24 hexa chars
       not mormot.core.text.HexToBin(pointer(aObfuscated), @bits, SizeOf(bits)) then
      exit;
    if self = nil then
      key := 0
    else
    begin
      bits.id.Value := bits.id.Value xor PInt64(@fCrypto[high(fCrypto) - 1])^;
      key := crc32ctab[0, bits.id.ProcessID and 1023] xor fCryptoCRC;
    end;
  end;
  if crc32c(bits.id.ProcessID, @bits.id, SizeOf(bits.id)) xor key = bits.crc then
  begin
    aIdentifier := bits.id.Value;
    result := true;
  end;
end;

procedure TSynUniqueIdentifierGenerator.WaitForSafeCreateTime(
  TimeOutSeconds: integer);
var
  tix: Int64;
begin
  tix := GetTickCount64 + TimeOutSeconds * 1000;
  repeat
    if UnixTimeUtc >= fUnixCreateTime then
      break;
    SleepHiRes(100);
  until GetTickCount64 > tix;
end;


{ ****** IProtocol Safe Communication with Unilateral or Mutual Authentication }

function ToText(res: TProtocolResult): PShortString;
begin
  result := GetEnumName(TypeInfo(TProtocolResult), ord(res));
end;


{ TProtocolNone }

function TProtocolNone.ProcessHandshake(const MsgIn: RawUtf8;
  out MsgOut: RawUtf8): TProtocolResult;
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


{ TProtocolAes }

constructor TProtocolAes.Create(aClass: TAesAbstractClass;
  const aKey; aKeySize: cardinal);
begin
  inherited Create;
  InitializeCriticalSection(fSafe);
  if aClass = nil then
    aClass := TAesFast[mCtr]; // fastest on x86_64 or OpenSSL - server friendly
  fAes[false] := aClass.Create(aKey, aKeySize);
  if fAes[false].AlgoMode in [mCfc, mOfc, mCtc, mGcm] then
    raise ESynCrypto.CreateUtf8('%.Create: incompatible % AEAD mode',
      [self, fAes[false].AlgoName]);
  fAes[true] := fAes[false].CloneEncryptDecrypt;
end;

constructor TProtocolAes.CreateFrom(aAnother: TProtocolAes);
begin
  inherited Create;
  InitializeCriticalSection(fSafe);
  fAes[false] := aAnother.fAes[false].Clone;
  fAes[true] := fAes[false].CloneEncryptDecrypt;
end;

destructor TProtocolAes.Destroy;
begin
  fAes[false].Free;
  if fAes[true] <> fAes[false] then
    fAes[true].Free; // fAes[false].CloneEncryptDecrypt may return self
  DeleteCriticalSection(fSafe);
  inherited Destroy;
end;

function TProtocolAes.ProcessHandshake(const MsgIn: RawUtf8;
  out MsgOut: RawUtf8): TProtocolResult;
begin
  result := sprUnsupported;
end;

function TProtocolAes.Decrypt(const aEncrypted: RawByteString;
  out aPlain: RawByteString): TProtocolResult;
begin
  EnterCriticalSection(fSafe);
  try
    try
      aPlain := fAes[false].DecryptPkcs7(aEncrypted, {iv=}true, {raise=}false);
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

procedure TProtocolAes.Encrypt(const aPlain: RawByteString;
  out aEncrypted: RawByteString);
begin
  EnterCriticalSection(fSafe);
  try
    aEncrypted := fAes[true].EncryptPkcs7(aPlain, {iv=}true);
  finally
    LeaveCriticalSection(fSafe);
  end;
end;

function TProtocolAes.Clone: IProtocol;
begin
  result := TProtocolAesClass(ClassType).CreateFrom(self);
end;



{ ******* TBinaryCookieGenerator Simple Cookie Generator }

procedure XorMemoryCtr(data: PCardinal; key256bytes: PCardinalArray;
  size: PtrUInt; ctr: cardinal);
begin
  while size >= sizeof(cardinal) do
  begin
    dec(size, sizeof(cardinal));
    data^ := data^ xor key256bytes[ctr and $3f] xor ctr;
    inc(data);
    ctr := ((ctr xor (ctr shr 15)) * 2246822519); // prime-number ctr diffusion
    ctr := ((ctr xor (ctr shr 13)) * 3266489917);
    ctr := ctr xor (ctr shr 16);
  end;
  while size <> 0 do
  begin
    dec(size);
    PByteArray(data)[size] := PByteArray(data)[size] xor ctr;
    ctr := ctr shr 8; // 1..3 pending iterations
  end;
end;


{ TBinaryCookieGenerator }

procedure TBinaryCookieGenerator.Init(const Name: RawUtf8;
  DefaultSessionTimeOutMinutes: cardinal);
var
  rnd: THash512;
begin
  DefaultTimeOutMinutes := DefaultSessionTimeOutMinutes;
  CookieName := Name;
  SessionSequence := Random32 and $7ffffff;
  // temporary secret for encryption
  CryptNonce := Random32;
  TAesPrng.Main.FillRandom(@Crypt, sizeof(Crypt));
  // temporary secret for HMAC-CRC32C
  TAesPrng.Main.FillRandom(@rnd, sizeof(rnd));
  Secret.Init(@rnd, sizeof(rnd));
end;

procedure TBinaryCookieGenerator.Cipher(P: PAnsiChar; bytes: integer);
begin
  XorMemoryCtr(@P[4], @Crypt, bytes - 4, {ctr=}xxHash32(CryptNonce, P, 4));
end;

type
  // map the binary layout of our base-64 serialized cookies
  TCookieContent = packed record
    head: packed record
      cryptnonce: cardinal; // ctr=hash32(cryptnonce) to cipher following bytes
      hmac: cardinal;       // = signature
      session: integer;     // = jti claim
      issued: cardinal;     // = iat claim (from UnixTimeUtc)
      expires: cardinal;    // = exp claim
    end;
    data: array[0..2047] of byte; // optional record binary serialization
  end;

function TBinaryCookieGenerator.Generate(out Cookie: RawUtf8;
  TimeOutMinutes: cardinal; PRecordData: pointer;
  PRecordTypeInfo: PRttiInfo): TBinaryCookieGeneratorSessionID;
var
  cc: TCookieContent; // local working buffer
  tmp: TSynTempBuffer;
begin
  tmp.Init(0);
  try
    result := InterlockedIncrement(integer(SessionSequence));
    if (PRecordData <> nil) and
       (PRecordTypeInfo <> nil) then
    begin
      BinarySave(PRecordData, tmp, PRecordTypeInfo, rkRecordTypes);
      if tmp.len > sizeof(cc.data) then
        // all cookies storage should be < 4K
        raise ESynException.Create('TBinaryCookieGenerator: Too Big Too Fat');
    end;
    cc.head.cryptnonce := Random32;
    cc.head.session := result;
    cc.head.issued := UnixTimeUtc;
    if TimeOutMinutes = 0 then
      TimeOutMinutes := DefaultTimeOutMinutes;
    if TimeOutMinutes = 0 then
      // 1 month expiration is a reasonable high value for "never expires"
      TimeOutMinutes := 31 * 24 * 60;
    cc.head.expires := cc.head.issued + TimeOutMinutes * 60;
    if tmp.len > 0 then
      MoveFast(tmp.buf^, cc.data, tmp.len);
    inc(tmp.len, sizeof(cc.head));
    cc.head.hmac := Secret.Compute(@cc.head.session, tmp.len - 8);
    Cipher(@cc, tmp.len);
    Cookie := BinToBase64Uri(@cc, tmp.len);
  finally
    tmp.Done;
  end;
end;

function TBinaryCookieGenerator.Validate(const cookie: RawUtf8;
  PRecordData: pointer; PRecordTypeInfo: PRttiInfo;
  PExpires, PIssued: PCardinal): TBinaryCookieGeneratorSessionID;
var
  clen, len: integer;
  now: cardinal;
  ccend: PAnsiChar;
  cc: TCookieContent;
begin
  result := 0; // parsing error
  if cookie = '' then
    exit;
  clen := length(cookie);
  len := Base64uriToBinLength(clen);
  if (len >= sizeof(cc.head)) and
     (len <= sizeof(cc)) and
     Base64uriDecode(pointer(cookie), @cc, clen) then
  begin
    Cipher(@cc, len);
    if (cardinal(cc.head.session) <= cardinal(SessionSequence)) then
    begin
      if PExpires <> nil then
        PExpires^ := cc.head.expires;
      if PIssued <> nil then
        PIssued^ := cc.head.issued;
      now := UnixTimeUtc;
      if (cc.head.issued <= now) and
         (cc.head.expires >= now) and
         (Secret.Compute(@cc.head.session, len - 8) = cc.head.hmac) then
        if (PRecordData = nil) or
           (PRecordTypeInfo = nil) then
          result := cc.head.session
        else if len > sizeof(cc.head) then
        begin
          ccend := PAnsiChar(@cc) + len;
          if BinaryLoad(PRecordData, @cc.data, PRecordTypeInfo,
              nil, ccend, rkRecordTypes) = ccend then
            result := cc.head.session;
        end;
    end;
  end;
end;

function TBinaryCookieGenerator.Save: RawUtf8;
begin
  result := BinarySaveBase64(
    @self, TypeInfo(TBinaryCookieGenerator), {uri=}true, rkRecordTypes);
end;

function TBinaryCookieGenerator.Load(const Saved: RawUtf8): boolean;
begin
  result := RecordLoadBase64(pointer(Saved), length(Saved),
    self, TypeInfo(TBinaryCookieGenerator), {uri=}true);
end;





procedure InitializeUnit;
begin
  Rtti.RegisterType(TypeInfo(TSignAlgo));
  Rtti.RegisterFromText(TypeInfo(TSynSignerParams),
    'algo:TSignAlgo secret,salt:RawUtf8 rounds:integer');
end;

procedure FinalizeUnit;
begin
end;


initialization
  InitializeUnit;
finalization
  FinalizeUnit;
  
end.
