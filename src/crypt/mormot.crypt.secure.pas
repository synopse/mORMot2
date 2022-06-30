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
    - Rnd/Hash/Sign/Cipher/Asym/Cert/Store High-Level Algorithms Factories
    - Minimal PEM/DER Encoding/Decoding

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
    // - may trigger a ECrypt if the password was stored using hardened
    // CryptDataForCurrentUser, and the current user doesn't match the
    // expected user stored in the field
    class function ComputePlainPassword(const CypheredPassword: SpiUtf8;
      CustomKey: cardinal = 0; const AppSecret: RawUtf8 = ''): SpiUtf8;
    /// the private key used to cypher the password storage on serialization
    // - application can override the default 0 value at runtime
    property Key: cardinal
      read GetKey write fKey;
    /// access to the associated unencrypted Password value
    // - may trigger a ECrypt if the password was stored using hardened
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
    // - warning: caller should make Safe.Unlock(aLock) when finished
    function DynArrayLocked(aLock: TRWLockContext = cWrite): TDynArray;
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
    procedure From(const aID: TSynUniqueIdentifier);
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
    fSafe: TLightLock;
    fUnixCreateTime: cardinal;
    fLatestCounterOverflowUnixCreateTime: cardinal;
    fIdentifier: TSynUniqueIdentifierProcess;
    fIdentifierShifted: cardinal;
    fLastCounter: cardinal;
    fComputedCount: Int64;
    fCollisions: cardinal;
    fCryptoCRC: cardinal;
    fCrypto: array[0..7] of cardinal; // only fCrypto[6..7] are used in practice
    fCryptoAesE, fCryptoAesD: TAes; // Initialized if aSharedObfuscationKeyNewKdf
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
    property Safe: TLightLock
      read fSafe;
  published
    /// the process identifier, associated with this generator
    property Identifier: TSynUniqueIdentifierProcess
      read fIdentifier;
    /// how many times ComputeNew method has been called
    property ComputedCount: Int64
      read fComputedCount;
    /// how many times ComputeNew method did have a collision and a fake
    // increased timestamp has been involved
    property Collisions: cardinal
      read fCollisions;
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

const
  SIGNER_DEFAULT_SALT = 'I6sWioAidNnhXO9BK';

type
  /// the HMAC/SHA-3 algorithms known by TSynSigner
  // - HMAC/SHA-1 is considered unsafe, HMAC/SHA-2 are well proven, and
  // HMAC/SHA-3 is newer but strong, so a good candidate for safety
  // - saSha3S128 is used by default, i.e. SHA-3 in SHAKE_128 mode
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
  // - a typical (extended) JSON to supply to TSynSigner.Pbkdf2() may be
  // ${algo:"saSha512",secret:"StrongPassword",salt:"FixedSalt",rounds:10000}
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
    procedure Init(aAlgo: TSignAlgo; aSecret: pointer; aSecretLen: integer); overload;
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
    // - accept as input a TSynSignerParams serialized as JSON object e.g.
    // ${algo:"saSha512",secret:"StrongPassword",salt:"FixedSalt",rounds:10000}
    procedure Pbkdf2(aParamsJson: PUtf8Char; aParamsJsonLen: integer;
      out aDerivatedKey: THash512Rec;
      const aDefaultSalt: RawUtf8 = SIGNER_DEFAULT_SALT;
      aDefaultAlgo: TSignAlgo = saSha3S128); overload;
    /// convenient wrapper to perform PBKDF2 safe iterative key derivation
    // - accept as input a TSynSignerParams serialized as JSON object e.g.
    // ${algo:"saSha512",secret:"StrongPassword",salt:"FixedSalt",rounds:10000}
    procedure Pbkdf2(const aParamsJson: RawUtf8;
      out aDerivatedKey: THash512Rec;
      const aDefaultSalt: RawUtf8 = SIGNER_DEFAULT_SALT;
      aDefaultAlgo: TSignAlgo = saSha3S128); overload;
    /// prepare a TAes object with the key derivated via a Pbkdf2() call
    // - aDerivatedKey is defined as "var", since it will be zeroed after use
    procedure AssignTo(var aDerivatedKey: THash512Rec;
      out aAes: TAes; aEncrypt: boolean);
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
    /// set the resulting hash into a binary buffer, and the size as result
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


  /// the known 32-bit crc algorithms as returned by CryptCrc32()
  // - ccaCrc32 and ccaAdler32 require mormot.lib.z.pas to be included
  // - AesNiHash() is not part of it, because it is not cross-platform, and
  // randomly seeded at process startup
  TCrc32Algo = (
    caCrc32c,
    caCrc32,
    caAdler32,
    caxxHash32,
    caFnv32);

/// returns the 32-bit crc function for a given algorithm
// - may return nil, e.g. for caCrc32/caAdler32 when mormot.lib.z is not loaded
function CryptCrc32(algo: TCrc32Algo): THasher;

function ToText(algo: TSignAlgo): PShortString; overload;
function ToText(algo: THashAlgo): PShortString; overload;
function ToText(algo: TCrc32Algo): PShortString; overload;

/// compute the hexadecimal hash of any (big) file
// - using a temporary buffer of 1MB for the sequential reading
function HashFile(const aFileName: TFileName; aAlgo: THashAlgo): RawUtf8; overload;

/// compute one or several hexadecimal hash(es) of any (big) file
// - using a temporary buffer of 1MB for the sequential reading
function HashFileRaw(const aFileName: TFileName; aAlgos: THashAlgos): TRawUtf8DynArray;

/// compute the hexadecimal hashe(s) of one file, as external .md5/.sha256/.. files
// - generate the text hash files in the very same folder
procedure HashFile(const aFileName: TFileName; aAlgos: THashAlgos); overload;

/// one-step hash computation of a buffer as lowercase hexadecimal string
function HashFull(aAlgo: THashAlgo; aBuffer: Pointer; aLen: integer): RawUtf8; overload;

/// one-step hash computation of a buffer as lowercase hexadecimal string
function HashFull(aAlgo: THashAlgo; const aBuffer: RawByteString): RawUtf8; overload;

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

  /// implements a secure protocol using symmetric AES encryption
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
  // - stores a session ID, cookie name, and encryption and signature keys
  // - can optionally store any associated record as efficient binary
  // - it is NOT cryptographic secure, because cookies are not, but it is
  // strong enough to avoid most attacks, and uses less space than a JWT
  {$ifdef USERECORDWITHMETHODS}
  TBinaryCookieGenerator = record
  {$else}
  TBinaryCookieGenerator = object
  {$endif USERECORDWITHMETHODS}
    /// the cookie name, used for storage in the client side HTTP headers
    // - is not part of the Generate/Validate content, but could be used
    // when the cookie is actually stored in HTTP headers
    CookieName: RawUtf8;
    /// an increasing 31-bit counter, to implement unique session ID
    SessionSequence: TBinaryCookieGeneratorSessionID;
    /// the random initial value of the SessionSequence counter
    SessionSequenceStart: TBinaryCookieGeneratorSessionID;
    /// secret information, used for digital signature of the cookie content
    Secret: cardinal;
    /// random IV used as CTR on Crypt[] secret key
    CryptNonce: cardinal;
    /// used when Generate() has TimeOutMinutes=0
    // - if equals 0, one month delay is used as "never expire"
    DefaultTimeOutMinutes: word;
    /// 32-bit checksum algorithm used for digital signature
    CrcAlgo: TCrc32Algo;
    /// padding byte for backward compatibility
    Padding: byte;
    /// private random secret, used for encryption of the cookie content
    Crypt: array[byte] of byte;
    /// initialize ephemeral temporary cookie generation
    procedure Init(const Name: RawUtf8 = 'mORMot';
      DefaultSessionTimeOutMinutes: cardinal = 0;
      SignAlgo: TCrc32Algo = caCrc32c);
    /// will initialize a new Base64Uri-encoded session cookie
    // - with an optional record data
    // - will return the 32-bit internal session ID and a Base64Uri cookie
    // - you can supply a time period, after which the session will expire -
    // default 0 will use DefaultTimeOutMinutes as supplied to Init()
    function Generate(out Cookie: RawUtf8; TimeOutMinutes: cardinal = 0;
      PRecordData: pointer = nil;
      PRecordTypeInfo: PRttiInfo = nil): TBinaryCookieGeneratorSessionID;
    ///  decode a base64uri cookie and optionally fill an associated record
    // - return the associated session/sequence number, 0 on error
    function Validate(const Cookie: RawUtf8;
      PRecordData: pointer = nil; PRecordTypeInfo: PRttiInfo = nil;
      PExpires: PUnixTime = nil;
      PIssued: PUnixTime = nil): TBinaryCookieGeneratorSessionID;
    /// allow the very same cookie to be recognized after server restart
    function Save: RawUtf8;
    /// unserialize the cookie generation context as serialized by Save
    function Load(const Saved: RawUtf8): boolean;
  end;
  {$A+}

  PBinaryCookieGenerator = ^TBinaryCookieGenerator;


{ ************* Rnd/Hash/Sign/Cipher/Asym/Cert/Store High-Level Algorithms Factories }

type
  ECrypt = class(ESynException);

  TCryptAlgo = class;
  TCryptAlgos = array of TCryptAlgo;

  /// abstract class implemented e.g. by TCryptRandom/TCryptHasher/TCryptAsym
  // - we define a class and not a meta-class since it allows to resolve and
  // store some engine-specific context ahead of time, for faster process
  // - inherited classes would dedicated New() factory methods; this parent
  // features the internal registration feature of the known algorithms
  TCryptAlgo = class(TObject)
  protected
    fName: RawUtf8;
    // case-insensitive quick lookup of the algorithms into a TCryptAlgo instance
    class function InternalFind(const name: RawUtf8; var Last: TCryptAlgo): pointer;
    class function InternalResolve(const name: RawUtf8; CSV: PUtf8Char): integer;
  public
    /// inherited classes should properly initialize this kind of process
    constructor Create(const name: RawUtf8); virtual;
    /// register this class to override one or several identifiers implementation
    // - returns the last instance created for name[]
    class function Implements(const name: array of RawUtf8): pointer; overload;
    /// register this class to override one or several identifiers implementation
    class procedure Implements(csv: PUtf8Char; const suffix: RawUtf8 = ''); overload;
    /// return all the TCryptAlog instances matching this class type
    // - could be used e.g. as TCryptRandom.Instances
    class function Instances: TCryptAlgos;
    /// return all the TCryptAlog instances matching this class type
    // - could be used e.g. as TCryptRandom.Names
    class function Names: TRawUtf8DynArray;
    /// process-wide case-insensitive identifier for quick lookup of the algorithms
    // - typical values may follow OpenSSL naming, e.g. 'MD5', 'AES-128-GCM' or
    // 'prime256v1'
    property AlgoName: RawUtf8
      read fName;
  end;

  /// interface as implemented e.g. by TCryptHash
  ICryptHash = interface
    /// iterative process of a memory buffer
    function Update(buf: pointer; buflen: PtrInt): ICryptHash; overload;
    /// iterative process of a memory buffer
    function Update(const buf: RawByteString): ICryptHash; overload;
    /// iterative process of a file content
    procedure UpdateFile(const filename: TFileName);
    /// compute the digest, and return it in a memory buffer
    function Final(digest: pointer; digestlen: PtrInt): PtrInt; overload;
    /// compute the digest, and return it as UTF-8 hexadecimal text
    function Final: RawUtf8; overload;
  end;

  /// abstract class implemented e.g. by TCryptHash/TCryptCipher/TCryptKey
  TCryptInstance = class(TInterfacedObject)
  protected
    fCryptAlgo: TCryptAlgo;
  public
    /// initialize the instance
    constructor Create(algo: TCryptAlgo); overload; virtual;
    /// resolve the name via TCryptAlgo.InternalFind() and initialize the instance
    constructor Create(const name: RawUtf8); overload;
    /// access to the associated algorithm
    property CryptAlgo: TCryptAlgo
      read fCryptAlgo;
  end;

  /// randomness generator parent class, as resolved by Rnd()
  TCryptRandom = class(TCryptAlgo)
  public
    /// retrieve some random bytes into a buffer
    procedure Get(dst: pointer; dstlen: PtrInt); overload; virtual; abstract;
    /// retrieve some random bytes into a RawByteString
    function Get(len: PtrInt): RawByteString; overload; virtual;
    /// retrieve some random bytes into a TBytes
    function GetBytes(len: PtrInt): TBytes;
    /// retrieve a random 32-bit value
    function Get32: cardinal; overload; virtual;
    /// retrieve a random 32-bit value
    function Get32(max: cardinal): cardinal; overload;
    /// retrieve a random floating point value in the [0..1) range
    function GetDouble: double;
  end;

  /// hashing/signing parent class, as returned by Hash/Sign() factories
  TCryptHash = class(TCryptInstance, ICryptHash)
  protected
    function InternalFinal(out dig: THash512Rec): PtrInt; virtual; abstract;
  public
    // ICryptHash methods
    function Update(buf: pointer; buflen: PtrInt): ICryptHash; overload; virtual; abstract;
    function Update(const buf: RawByteString): ICryptHash; overload;
    procedure UpdateFile(const filename: TFileName);
    function Final(digest: pointer; digestlen: PtrInt): PtrInt; overload;
    function Final: RawUtf8; overload;
  end;

  /// hashing parent class, as resolved by Hasher()
  // - this class is to ensure a content as not been tempered: use Signer()
  // to compute a digital signature from a given secret
  TCryptHasher = class(TCryptAlgo)
  public
    /// one-step process of a whole memory buffer, into an hexadecimal digest
    function Full(buf: pointer; buflen: PtrInt): RawUtf8; overload;
    /// one-step process of a whole memory buffer, into an hexadecimal digest
    function Full(const buf: RawByteString): RawUtf8; overload;
    /// one-step process of a whole memory buffer, into an binary digest
    // - returns the number of bytes stored in digest
    function Full(buf: pointer; buflen: PtrInt; out digest: THash512Rec): PtrInt; overload;
    /// one-step process of a whole file content, into an hexadecimal digest
    function FullFile(const filename: TFileName): RawUtf8;
    /// main factory to create a new hasher instance with this algorithm
    function New: ICryptHash; virtual; abstract;
  end;

  /// signing parent class, as resolved by Signer()
  // - in respect to TCryptHasher, requires a secret key to be supplied
  // for safe HMAC content signature
  TCryptSigner = class(TCryptAlgo)
  public
    /// one-step process of a whole memory buffer, into an hexadecimal digest
    function Full(key, buf: pointer; keylen, buflen: PtrInt): RawUtf8; overload;
    /// one-step process of a whole memory buffer, into an hexadecimal digest
    function Full(const key, buf: RawByteString): RawUtf8; overload;
    /// one-step process of a whole file content, into an hexadecimal digest
    function FullFile(key: pointer; keylen: PtrInt; const filename: TFileName): RawUtf8;
    /// strong PBKDF2 derivation of a secret (and salt) using this algorithm
    // - returns the number of bytes computed in returned key memory
    function Pbkdf2(const secret, salt: RawUtf8; rounds: integer;
      out key: THash512Rec): integer; overload; virtual; abstract;
    /// main factory to create a new signer instance with this algorithm
    function New(key: pointer; keylen: PtrInt): ICryptHash; virtual; abstract;
    /// main factory to create a new signer instance from PBKDF2 derivation
    function NewPbkdf2(const secret, salt: RawUtf8; rounds: integer): ICryptHash;
  end;

  /// interface as implemented e.g. by TCryptCipher
  ICryptCipher = interface
    /// quickly generate a cipher with the same algorithm, direction and key
    function Clone: ICryptCipher;
    /// general encryption/decryption method using RawByteString buffers
    // - an IV is generated at the start of this if none was specified at New()
    // - will do proper PKCS7 padding on the src input buffer
    // - return TRUE on success, FALSE if input padding or AEAD MAC is incorrect
    function Process(const src: RawByteString; out dst: RawByteString;
      const aeadinfo: RawByteString = ''): boolean; overload;
    /// general encryption/decryption method using TBytes buffers
    // - use use TByteDynArray for aeadinfo because TBytes raise a Delphi XE
    // compiler bug - it should be assignment compatible with any TBytes value
    function Process(const src: TBytes; out dst: TBytes;
      const aeadinfo: TByteDynArray = nil): boolean; overload;
    /// low-level encryption/decryption on memory buffers
    // - srclen/dsstlen should match the size block of the algorithm,
    // e.g. 16 bytes for AES or 1 byte for SHAKE (i.e. SHA-3 in XOF cipher mode)
    // - on AES-GCM algorithm, if dst=nil then AAD is set from src/srclen
    procedure RawProcess(src, dst: pointer; srclen, dstlen: PtrInt);
    /// low-level GMAC computation for AES-GCM
    // - after Encrypt, fill gmac with the tag value of the data and return true
    // - after Decrypt, return true only if the tag value of the data match gmac
    // - always return false if not AES-GCM is used as algorithm
    function RawFinal(var gmac: TAesBlock): boolean;
  end;

  /// symmetric encryption class, as resolved by CipherAlgo()
  TCryptCipherAlgo = class(TCryptAlgo)
  public
    /// check if this algorithm is of AEAD kind, i.e. can cipher and authenticate
    // - note that currently our OpenSSL AES-GCM wrapper has troubles with
    // AEAD associated authentication so returns false: it will compute and check
    // the GMAC of the content as expected, but only our internal 'AES-###-GCM-INT'
    // actually supports aeadinfo <> '' in ICryptCipher.Process
    function IsAead: boolean; virtual; abstract;
    /// main factory to create a new instance with this algorithm
    // - the supplied key should match the size expected by the algorithm
    function New(key: pointer; encrypt: boolean; iv: pointer = nil): ICryptCipher;
      overload; virtual; abstract;
    /// main factory to create a new instance from PBKDF2 key derivation
    function New(const hash, secret, salt: RawUtf8; rounds: integer;
      encrypt: boolean): ICryptCipher; overload;
    /// main factory to create a new encryption instance with this algorithm
    function Encrypt(key: pointer): ICryptCipher; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// main factory to create a new decryption instance with this algorithm
    function Decrypt(key: pointer): ICryptCipher; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// main factory to create a new encryption instance from PBKDF2 key derivation
    function Encrypt(const sign, secret, salt: RawUtf8; rounds: integer): ICryptCipher; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// main factory to create a new decryption instance from PBKDF2 key derivation
    function Decrypt(const sign, secret, salt: RawUtf8; rounds: integer): ICryptCipher; overload;
      {$ifdef HASINLINE} inline; {$endif}
  end;

  /// symmetric encryption parent class, as returned by Cipher() factory
  TCryptCipher = class(TCryptInstance, ICryptCipher)
  public
    // ICryptCipher methods
    function Clone: ICryptCipher; virtual; abstract;
    function Process(const src: RawByteString; out dst: RawByteString;
      const aeadinfo: RawByteString): boolean; overload; virtual; abstract;
    function Process(const src: TBytes; out dst: TBytes;
      const aeadinfo: TByteDynArray): boolean; overload; virtual; abstract;
    procedure RawProcess(src, dst: pointer; srclen, dstlen: PtrInt); virtual; abstract;
    function RawFinal(var gmac: TAesBlock): boolean; virtual; abstract;
  end;

  /// asymmetric public-key cryptography parent class, as returned by Asym()
  TCryptAsym = class(TCryptAlgo)
  public
    /// generate a public/private pair of keys in the PEM text format
    procedure GeneratePem(out pub, priv: RawUtf8; const privpwd: RawUtf8); virtual;
    /// generate a public/private pair of keys in the DER binary format
    procedure GenerateDer(out pub, priv: RawByteString; const privpwd: RawUtf8); virtual;
    /// digital signature of some message using a private key
    // - the message is first hashed with the supplied TCryptHasher
    // - the signature is returned in binary DER format
    function Sign(hasher: TCryptHasher; msg: pointer; msglen: PtrInt;
      const priv: RawByteString; out sig: RawByteString;
      const privpwd: RawUtf8 = ''): boolean; overload; virtual; abstract;
    /// digital signature of some message using a private key
    // - the message is first hashed with the default hasher of this
    // algorithm, or the specific hashername
    function Sign(const msg, priv: RawByteString; out sig: RawByteString;
      const hashername: RawUtf8 = ''; const privpwd: RawUtf8 = ''): boolean; overload;
    /// digital signature of some message using a private key
    // - the message is first hashed with the default hasher of this
    // algorithm, or the specific hashername
    function Sign(const msg, priv: TBytes; out sig: TBytes;
      const hashername: RawUtf8 = ''; const privpwd: RawUtf8 = ''): boolean; overload;
    /// digital signature verification of some message using a public key
    // - the message is first hashed with the supplied TCryptHasher
    function Verify(hasher: TCryptHasher; msg: pointer; msglen: PtrInt;
      const pub, sig: RawByteString): boolean; overload; virtual; abstract;
    /// digital signature verification of some message using a public key
    // - the message is first hashed with the default hasher of this
    // algorithm, or the specific hashername
    function Verify(const msg, pub, sig: RawByteString;
      const hashername: RawUtf8 = ''): boolean; overload;
    /// digital signature verification of some message using a public key
    // - the message is first hashed with the default hasher of this
    // algorithm, or the specific hashername
    function Verify(const msg, pub, sig: TBytes;
      const hashername: RawUtf8 = ''): boolean; overload;
    /// compute a shared secret from local private key and a public key
    // - used for encryption with no key transmission
    // - returns '' if this algorithm doesn't support this feature (e.g. RSA)
    function SharedSecret(const pub, priv: RawByteString): RawByteString; virtual;
  end;

  /// exception class raised by our High-Level Certificates Process
  ECryptCert = class(ESynException);

  /// the known asymmetric algorithms, e.g. as published by OpenSSL
  // - as implemented e.g. by TJwtAbstractOsl inherited classes, or
  // TCryptAsymOsl/TCryptCertAlgoOpenSsl implementing TCryptAsym/ICryptCert,
  // accessible via CryptAsymOpenSsl[] and CryptCertAlgoOpenSsl[] factories
  TCryptAsymAlgo = (
    caaES256,
    caaES384,
    caaES512,
    caaES256K,
    caaRS256,
    caaRS384,
    caaRS512,
    caaPS256,
    caaPS384,
    caaPS512,
    caaEdDSA);

  /// the known Key Usages for a given Certificate
  // - is an exact match of TX509Usage enumerate in mormot.lib.openssl11.pas
  // - stored as a 16-bit memory block, with CERTIFICATE_USAGE_ALL = 65535
  TCryptCertUsage = (
    cuCA,
    cuEncipherOnly,
    cuCrlSign,
    cuKeyCertSign,
    cuKeyAgreement,
    cuDataEncipherment,
    cuKeyEncipherment,
    cuNonRepudiation,
    cuDigitalSignature,
    cuDecipherOnly,
    cuTlsServer,
    cuTlsClient,
    cuEmail,
    cuCodeSign,
    cuOcspSign,
    cuTimestamp);

  /// set of Key Usages for a given Certificate - stored as a 16-bit word
  TCryptCertUsages = set of TCryptCertUsage;

  /// the RFC5280-compatible reasons why a Certificate could be revoked
  // - used for each item in a Certificate Revocation List (CRL)
  // - crrNotRevoked (item 7) is not used in the RFC, and used internally here
  TCryptCertRevocationReason = (
    crrUnspecified,
    crrCompromised,
    crrAuthorityCompromised,
    crrUnAffiliated,
    crrSuperseded,
    crrReplaced,
    crrTempHold,
    crrNotRevoked,
    crrRemoved,
    crrWithdrawn,
    crrServerCompromised);

  /// the Digital Signature results for a given Certificate
  // - is an exact match of TEccValidity enumerate in mormot.crypt.ecc256r1.pas
  TCryptCertValidity = (
    cvUnknown,
    cvValidSigned,
    cvValidSelfSigned,
    cvNotSupported,
    cvBadParameter,
    cvCorrupted,
    cvInvalidDate,
    cvUnknownAuthority,
    cvDeprecatedAuthority,
    cvInvalidSignature,
    cvRevoked,
    cvWrongUsage);

  /// convenient wrapper of X509 Certificate subject name fields
  // - not always implemented - mainly our 'syn-es256' certificate won't
  TCryptCertFields = record
    Country,
    State,
    Locality,
    Organization,
    OrgUnit,
    CommonName: RawUtf8;
  end;
  PCryptCertFields = ^TCryptCertFields;

  /// ICryptCert.Save possible output formats
  // - 'syn-es256' from mormot.crypt.ecc certificate will use its own proprietary
  // format, i.e. SaveToBinary/SaveToSecureBinary for ccfBinary, or non-standard
  // '-----BEGIN/END SYNECC CERTIFICATE-----' headers for ccfPem
  // - 'x509-rs256'..'x509-es256' from mormot.crypt.openssl will use the standard
  // x509 format, as DER (or PKCS12 if PrivatePassword is set) for ccfBinary,
  // or PEM for ccfPEM (concatenating the private key if PrivatePassword is set)
  // - ccfHexa, ccfBase64 and ccfBase64Uri will use the ccfBinary output, then
  // encode it as Hexadecimal or Base-64 (URI)
  TCryptCertFormat = (
    ccfBinary,
    ccfPem,
    ccfHexa,
    ccfBase64,
    ccfBase64Uri);

  TCryptCert = class;

  /// abstract interface to a Certificate, as returned by Cert() factory
  // - may be X509 or not, OpenSSL implemented or not
  // - note: features and serialization are not fully compatible between engines,
  // but those high-level methods work as expected within each TCryptCertAlgo
  ICryptCert = interface
    /// create a new Certificate instance with its genuine private key
    // - Subjects is given as a CSV text, e.g. 'synopse.info,www.synopse.info'
    // - if Authority is nil, will generate a self-signed certificate, otherwise
    // will use this Authority private key to sign the certificate
    // - ValidDays and ExpireDays are relative to the current time - ValidDays
    // is -1 by default to avoid most clock synch issues
    procedure Generate(Usages: TCryptCertUsages; const Subjects: RawUtf8 = '';
      const Authority: ICryptCert = nil; ExpireDays: integer = 365;
      ValidDays: integer = -1; Fields: PCryptCertFields = nil);
    /// the Certificate Genuine Serial Number
    // - e.g. '04:f9:25:39:39:f8:ce:79:1a:a4:0e:b3:fa:72:e3:bc:9e:d6'
    function GetSerial: RawUtf8;
    /// the Low-Level Certificate Main Subject
    // - e.g. 'synopse.info' from OpenSSL X509 CN= subject field
    function GetSubject: RawUtf8;
    /// an array of all Subject names covered by this Certificate
    // - e.g. ['synopse.info', 'www.synopse.info']
    function GetSubjects: TRawUtf8DynArray;
    /// the High-Level Certificate Issuer
    // - e.g. '/C=US/O=Let''s Encrypt/CN=R3' or some Baudot-encoded text
    function GetIssuerName: RawUtf8;
    /// the Issuer Key Identifier of this Certificate
    // - e.g. '14:2E:B3:17:B7:58:56:CB:AE:50:09:40:E6:1F:AF:9D:8B:14:C2:C6'
    function GetIssuerSerial: RawUtf8;
    /// the minimum Validity timestamp of this Certificate
    function GetNotBefore: TDateTime;
    /// the maximum Validity timestamp of this Certificate
    function GetNotAfter: TDateTime;
    /// the Key Usages of this Certificate
    function GetUsage: TCryptCertUsages;
    /// verbose Certificate information, returned as huge text/JSON blob
    function GetPeerInfo: RawUtf8;
    /// compute the hexadecimal fingerprint of this Certificate
    // - is usually the hash of its binary (e.g. DER) serialization
    function GetDigest(Algo: THashAlgo = hfSHA256): RawUtf8;
    /// load a Certificate from a Save() content
    // - PrivatePassword is needed if the input contains a private key
    // - will only recognize and support the ccfBinary and ccfPem formats
    function Load(const Saved: RawByteString;
      const PrivatePassword: RawUtf8 = ''): boolean;
    /// serialize the Certificate as reusable content
    // - after Generate, will contain the public and private key, so
    // PrivatePassword is needed to secure its content - if PrivatePassword is
    // left to '' then only the generated public key will be serialized
    // - will use binary by default, but you can export to another formats,
    // depending on the underlying TCryptCertAlgo
    function Save(const PrivatePassword: RawUtf8 = '';
      Format: TCryptCertFormat = ccfBinary): RawByteString;
    /// compute a digital signature of some digital content
    // - memory buffer will be hashed then signed using the private secret key
    // of this certificate instance
    // - you could later on verify this text signature according to the public
    // key of this certificate, using ICryptCert.Verify() or ICertStore.Verify()
    // - returns '' on failure, e.g. if this Certificate has no private key
    // - returns the binary signature of the Data buffer
    function Sign(Data: pointer; Len: integer): RawByteString; overload;
    /// compute a digital signature of some digital content
    // - will use the private key of this certificate
    // - just a wrapper around the overloaded Sign() function
    function Sign(const Data: RawByteString): RawByteString; overload;
    /// verify a digital signature of some digital content
    // - will use the public key of this certificate
    // - see ICertStore.Verify() for a complete CA chain validation
    function Verify(Sign, Data: pointer;
      SignLen, DataLen: integer): TCryptCertValidity; overload;
    /// verify a digital signature of some digital content
    // - just a wrapper around the overloaded Verify() function
    function Verify(
      const Signature, Data: RawByteString): TCryptCertValidity; overload;
    /// returns true if the Certificate contains a private key secret
    function HasPrivateSecret: boolean;
    /// retrieve the private key as raw binary, or '' if none
    function GetPrivateKey: RawByteString;
    /// compare two Certificates, which should share the same algorithm
    // - will compare the internal properties and the public key, not the
    // private key: you could e.g. use it to verify that a ICryptCert with
    // HasPrivateSecret=false matches another with HasPrivateSecret=true
    function IsEqual(const another: ICryptCert): boolean;
    /// access to the low-level implementation class
    function Instance: TCryptCert;
    /// access to the low-level implementation handle
    // - e.g. PX509 for OpenSsl, or TEccCertificate for mormot.crypt.ecc
    function Handle: pointer;
  end;

  /// abstract parent class to implement ICryptCert, as returned by Cert() factory
  TCryptCert = class(TCryptInstance, ICryptCert)
  protected
    procedure RaiseVoid(Instance: pointer; const Msg: shortstring);
    procedure RaiseError(const Msg: shortstring); overload;
    procedure RaiseError(const Fmt: RawUtf8; const Args: array of const); overload;
  public
    // ICryptCert methods
    procedure Generate(Usages: TCryptCertUsages; const Subjects: RawUtf8;
      const Authority: ICryptCert; ExpireDays, ValidDays: integer;
      Fields: PCryptCertFields); virtual; abstract;
    function Load(const Saved: RawByteString;
      const PrivatePassword: RawUtf8): boolean; virtual; abstract;
    function GetSerial: RawUtf8; virtual; abstract;
    function GetSubject: RawUtf8; virtual; abstract;
    function GetSubjects: TRawUtf8DynArray; virtual; abstract;
    function GetIssuerName: RawUtf8; virtual; abstract;
    function GetIssuerSerial: RawUtf8; virtual; abstract;
    function GetNotBefore: TDateTime; virtual; abstract;
    function GetNotAfter: TDateTime; virtual; abstract;
    function GetUsage: TCryptCertUsages; virtual; abstract;
    function GetPeerInfo: RawUtf8; virtual; abstract;
    function GetDigest(Algo: THashAlgo): RawUtf8; virtual;
    function Save(const PrivatePassword: RawUtf8;
      Format: TCryptCertFormat): RawByteString; virtual;
    function HasPrivateSecret: boolean; virtual; abstract;
    function GetPrivateKey: RawByteString; virtual; abstract;
    function IsEqual(const another: ICryptCert): boolean; virtual;
    function Sign(Data: pointer; Len: integer): RawByteString;
      overload; virtual; abstract;
    function Sign(const Data: RawByteString): RawByteString; overload; virtual;
    function Verify(Sign, Data: pointer; SignLen, DataLen: integer): TCryptCertValidity;
      overload; virtual; abstract;
    function Verify(const Signature, Data: RawByteString): TCryptCertValidity;
      overload; virtual;
    function Instance: TCryptCert;
    function Handle: pointer; virtual; abstract;
  end;

  /// meta-class of the abstract parent to implement ICryptCert interface
  TCryptCertClass = class of TCryptCert;

  /// abstract parent class for ICryptCert factories
  TCryptCertAlgo = class(TCryptAlgo)
  public
    /// main factory to create a new Certificate instance with this algorithm
    function New: ICryptCert; virtual; abstract;
    /// factory to generate a new Certificate instance
    // - just a wrapper around New and ICryptCert.Generate()
    function Generate(Usages: TCryptCertUsages; const Subjects: RawUtf8 = '';
      const Authority: ICryptCert = nil; ExpireDays: integer = 365;
      ValidDays: integer = -1; Fields: PCryptCertFields = nil): ICryptCert;
  end;

  /// abstract interface to a Certificates Store, as returned by Store() factory
  // - may be X509 or not, OpenSSL implemented or not
  ICryptStore = interface
    /// load a Certificates Store from a ToBinary content
    function FromBinary(const Binary: RawByteString): boolean;
    /// serialize the Certificates Store as raw binary
    function ToBinary: RawByteString;
    /// search for a certificate from its (hexadecimal) identifier
    function GetBySerial(const Serial: RawUtf8): ICryptCert;
    /// quickly check if a given certificate ID is part of the CRL
    // - returns crrNotRevoked if the serial is not known as part of the CRL
    // - returns the reason why this certificate has been revoked otherwise
    function IsRevoked(const Serial: RawUtf8): TCryptCertRevocationReason; overload;
    /// quickly check if a given certificate is part of the CRL
    // - returns crrNotRevoked is the serial is not known as part of the CRL
    // - returns the reason why this certificate has been revoked otherwise
    function IsRevoked(const cert: ICryptCert): TCryptCertRevocationReason; overload;
    /// register a certificate in the internal certificate chain
    // - returns false e.g. if the certificate was not valid, or its
    // serial was already part of the internal list
    // - self-signed certificate could be included - but add them with caution
    // - the Certificate should have one of cuCA, cuDigitalSignature usages
    function Add(const cert: ICryptCert): boolean;
    /// load a register a certificate or certificate chain from a memory buffer
    // - returns the serials of added certificate(s)
    function AddFromBuffer(const Content: RawByteString): TRawUtf8DynArray;
    /// load a register a certificate file or file chain
    // - returns the serials of added certificate(s)
    // - the Certificate should have one of cuCA, cuDigitalSignature usages
    function AddFromFile(const FileName: TFileName): TRawUtf8DynArray;
    /// search and register all certificate files from a given folder
    // - returns the serials of added certificate(s)
    // - the Certificate(s) should have one of cuCA, cuDigitalSignature usages
    function AddFromFolder(const Folder: TFileName;
      const Mask: TFileName = FILES_ALL; Recursive: boolean = false): TRawUtf8DynArray;
    /// add a new Serial number to the internal Certificate Revocation List
    // - on some engines (our internal ECC, but not OpenSSL), Reason=crrNotRevoked
    // could be used to unregister a certificate revocation
    function Revoke(const Cert: ICryptCert; RevocationDate: TDateTime;
      Reason: TCryptCertRevocationReason): boolean;
    /// check if the certificate is valid, against known certificates chain
    // - will check internal properties of the certificate (e.g. validity dates),
    // and validate the stored digital signature according to the public key of
    // the associated signing authority, as found within the store
    function IsValid(const cert: ICryptCert): TCryptCertValidity;
    /// verify the digital signature of a given memory buffer
    // - this signature should have come from a previous ICryptCert.Sign() call
    // - will check internal properties of the certificate (e.g. validity dates),
    // and validate the stored signature according to the public key of
    // the associated signing authority (which should be in this Store)
    function Verify(const Signature: RawByteString;
      Data: pointer; Len: integer): TCryptCertValidity;
    /// how many certificates are currently stored
    function Count: integer;
    /// how many CRLs are currently stored
    function CrlCount: integer;
    /// call e.g. CertAlgo.New to prepare a new ICryptCert to add to this store
    function CertAlgo: TCryptCertAlgo;
  end;

  /// abstract parent class to implement ICryptCert, as returned by Cert() factory
  TCryptStore = class(TCryptInstance, ICryptStore)
  public
    // ICryptStore methods
    function FromBinary(const Binary: RawByteString): boolean; virtual; abstract;
    function ToBinary: RawByteString; virtual; abstract;
    function GetBySerial(const Serial: RawUtf8): ICryptCert; virtual; abstract;
    function IsRevoked(const Serial: RawUtf8): TCryptCertRevocationReason;
      overload; virtual; abstract;
    function IsRevoked(const cert: ICryptCert): TCryptCertRevocationReason; overload; virtual;
    function Add(const cert: ICryptCert): boolean; virtual; abstract;
    function AddFromBuffer(const Content: RawByteString): TRawUtf8DynArray; virtual; abstract;
    function AddFromFile(const FileName: TFileName): TRawUtf8DynArray; virtual;
    function AddFromFolder(const Folder, Mask: TFileName;
       Recursive: boolean): TRawUtf8DynArray; virtual;
    function Revoke(const Cert: ICryptCert; RevocationDate: TDateTime;
      Reason: TCryptCertRevocationReason): boolean; virtual; abstract;
    function IsValid(const cert: ICryptCert): TCryptCertValidity; virtual; abstract;
    function Verify(const Signature: RawByteString;
      Data: pointer; Len: integer): TCryptCertValidity; virtual; abstract;
    function Count: integer; virtual; abstract;
    function CrlCount: integer; virtual; abstract;
    function CertAlgo: TCryptCertAlgo; virtual; abstract;
  end;

  /// meta-class of the abstract parent to implement ICryptStore interface
  TCryptStoreClass = class of TCryptStore;

  /// abstract parent class for ICryptStore factories
  TCryptStoreAlgo = class(TCryptAlgo)
  public
    /// main factory to create a new Store instance with this engine
    function New: ICryptStore; virtual; abstract;
    /// main factory to create a new Store instance from saved Binary
    function NewFrom(const Binary: RawByteString): ICryptStore; virtual;
  end;

const
  /// such a Certificate could be used for anything
  CERTIFICATE_USAGE_ALL = [low(TCryptCertUsage) .. high(TCryptCertUsage)];

function ToText(r: TCryptCertRevocationReason): PShortString; overload;
function ToText(u: TCryptCertUsage): PShortString; overload;
function ToText(u: TCryptCertUsages): ShortString; overload;
function ToText(v: TCryptCertValidity): PShortString; overload;

/// main resolver of the randomness generators
// - the shared TCryptRandom of this algorithm is returned: caller should NOT free it
// - e.g. Rnd.GetBytes(100) to get 100 random bytes from 'rnd-default' engine
// - call Rnd('rnd-entropy').Get() to gather OS entropy, optionally as
// 'rnd-entropysys', 'rnd-entropysysblocking', 'rnd-entropyuser'
// - alternative generators are 'rnd-aes' (same as 'rnd-default'), 'rnd-lecuyer'
// (fast on small content) and 'rnd-system'/'rnd-systemblocking' (mapping
// FillSystemRandom)
function Rnd(const name: RawUtf8 = 'rnd-default'): TCryptRandom;

/// main resolver of the registered hashers
// - hashers ensure a content as not been tempered: use Signer()
// to compute a digital signature from a given secret
// - the shared TCryptHasher of this algorithm is returned: caller should NOT free it
// - if not nil, you could call New or Full/FullFile methods
// - this unit supports 'MD5','SHA1','SHA256','SHA384','SHA512','SHA3_256','SHA3_512'
// and 32-bit non-cryptographic 'CRC32','CRC32C','XXHASH32','ADLER32','FNV32'
function Hasher(const name: RawUtf8): TCryptHasher;

/// main factory of the hashers instances as returned by Hasher()
// - if not nil, caller should call Update then Final
function Hash(const name: RawUtf8): ICryptHash;

/// main resolver of the registered signers
// - in respect to Hasher(), will require a secret for safe digital signature
// - the shared TCryptSigner of this algorithm is returned: caller should NOT free it
// - if not nil, you could call New or Full/FullFile methods
// - this unit supports 'HMAC-SHA1','HMAC-SHA256','HMAC-SHA384','HMAC-SHA512',
// and 'SHA3-224','SHA3-256','SHA3-384','SHA3-512','SHA3-S128','SHA3-S256'
function Signer(const name: RawUtf8): TCryptSigner;

/// main factory of the signer instances as returned by Signer()
// - in respect to Hash(), expects a key/salt to be supplied for the HMAC signing
// - if not nil, caller should call Update then Final
function Sign(key: pointer; keylen: PtrInt; const name: RawUtf8): ICryptHash; overload;

/// main factory of the signer instances, with PBKDF2 secret derivation
// - if not nil, caller should call Update then Final
function Sign(const name, secret, salt: RawUtf8; rounds: integer): ICryptHash; overload;

/// main factory of a signer instance from strong PBKDF2 derivation
// - paramsjson contains a TSynSignerParams serialized as JSON object
// - returns the number of bytes computed in returned key memory
function Sign(const paramsjson: RawUtf8;
  const defaultsalt: RawUtf8 = SIGNER_DEFAULT_SALT): ICryptHash; overload;

/// main resolver for symmetric encryption/decryption algorithms
// - names are e.g. 'aes-128-cfb' or 'aes-256-gcm' standard combinations
// - recognize some non-standard algorithms with trailing 'c64', 'cfc', 'ofc' and
// 'ctc' mode names e.g. as 'aes-256-cfc' - with AED 256-bit support (but c64)
// - will return the fastest engine, which may be OpenSSL, unless 'aes-nnn-xxx-int'
// is used to force using our mormot.crypt.core.pas own implementation
// - the shared TCryptCipherAlgo of this algorithm is returned: caller should
// NOT free it
function CipherAlgo(const name: RawUtf8): TCryptCipherAlgo;

/// main factory for symmetric encryption/decryption process
// - supplied key is expected to match the algorithm size
function Cipher(const name: RawUtf8; key: pointer; encrypt: boolean): ICryptCipher; overload;

/// main factory for symmetric encryption/decryption with PBKDF2 key derivation
function Cipher(const name, hash, secret, salt: RawUtf8; rounds: integer;
  encrypt: boolean): ICryptCipher; overload;

/// main factory for symmetric encryption process
function Encrypt(const name: RawUtf8; key: pointer): ICryptCipher; overload;
  {$ifdef HASINLINE} inline; {$endif}

/// main factory for symmetric decryption process
function Decrypt(const name: RawUtf8; key: pointer): ICryptCipher; overload;
  {$ifdef HASINLINE} inline; {$endif}

/// main factory for symmetric encryption process with PBKDF2 key derivation
function Encrypt(const name, hash, secret, salt: RawUtf8; rounds: integer): ICryptCipher; overload;
  {$ifdef HASINLINE} inline; {$endif}

/// main factory for symmetric decryption process with PBKDF2 key derivation
function Decrypt(const name, hash, secret, salt: RawUtf8; rounds: integer): ICryptCipher; overload;
  {$ifdef HASINLINE} inline; {$endif}

/// main resolver for asymmetric public key algorithms
// - mormot.crypt.ecc.pas defines 'secp256r1','NISTP-256' and 'prime256v1'
// which are synonymous of our secp256r1 ECDSA and ECDHE pascal code
// - mormot.crypt.openssl.pas will define 'es256' .. 'EdDSA' including
// 'rs256' for the well known 2048-bit RSA + SHA256 digital signature
// - the shared TCryptAsym of this algorithm is returned: caller should NOT free it
function Asym(const name: RawUtf8): TCryptAsym;

/// main resolver for Certificates algorithms
// - mormot.crypt.ecc.pas defines 'syn-es256' for our TEccCertificate proprietary
// format (safe and efficient), with 'syn-es256-v1' for the V1 revision with
// limited Usage and Subjects support
// - mormot.crypt.openssl.pas will define 'x509-es256' .. 'x509-eddsa' including
// 'x509-rs256' for the well established 2048-bit RSA + SHA256 certificates
// - the shared TCryptCertAlgo of this algorithm is returned: caller should
// NOT free it
function CertAlgo(const name: RawUtf8): TCryptCertAlgo;

/// main factory of the Certificates instances as returned by CertAlgo()
function Cert(const name: RawUtf8): ICryptCert;

/// main resolver for Certificates Store engines
// - mormot.crypt.ecc.pas defines 'syn-store' or 'syn-store-nocache' for our
// TEccCertificateChain proprietary format (safe and efficient)
// - mormot.crypt.openssl.pas will define 'x509-store'
// - the shared TCryptStoreAlgo of this algorithm is returned: caller should
// NOT free it
function StoreAlgo(const name: RawUtf8): TCryptStoreAlgo;

/// main factory of Certificates Store engines as returned by StoreAlgo()
function Store(const name: RawUtf8): ICryptStore;

var
  /// direct access to the mormot.crypt.ecc.pas 'syn-store' algorithm
  // - may be nil if this unit was not included
  CryptStoreAlgoSyn: TCryptStoreAlgo;

  /// direct access to the mormot.crypt.ecc.pas 'syn-store-nocache' algorithm
  // - may be nil if this unit was not included
  CryptStoreAlgoSynNoCache: TCryptStoreAlgo;

  /// direct access to the mormot.crypt.openssl.pas 'x509-store' algorithm
  // - may be nil if this unit was not included or if OpenSSL is not available
  // - is currently nil because TCryptStoreOpenSsl is not stable yet
  CryptStoreAlgoOpenSsl: TCryptStoreAlgo;

  /// direct access to the mormot.crypt.openssl.pas TCryptAsym factories
  // - may be nil if this unit was not included or if OpenSSL is not available
  CryptAsymOpenSsl: array[TCryptAsymAlgo] of TCryptAsym;

  /// direct access to the mormot.crypt.openssl.pas  ICryptCert factories
  // - may be nil if this unit was not included or if OpenSSL is not available
  // - to return a ICryptCert instance using OpenSSL RSA 2048 key, use e.g.
  // $ CryptCertAlgoOpenSsl[caaRS256].New
  CryptCertAlgoOpenSsl: array[TCryptAsymAlgo] of TCryptCertAlgo;



{ ************************** Minimal PEM/DER Encoding/Decoding }

type
  /// a certificate (typically X509) encoded as PEM / text
  TCertPem = type RawUtf8;

  /// a certificate (typically X509) encoded as binary
  TCertDer = type RawByteString;

  /// the DerToPem() supported contents of a PEM text instance
  // - pemSynopseSignature, pemSynopseCertificate and
  // pemSynopseCertificateAndPrivateKey follow our proprietary
  // mormot.crypt.ecc format, so are not compatible with other libraries
  TPemKind = (
    pemUnspecified,
    pemCertificate,
    pemCrl,
    pemPrivateKey,
    pemPublicKey,
    pemRsaPrivateKey,
    pemRsaPublicKey,
    pemEcPrivateKey,
    pemEncryptedPrivateKey,
    pemCertificateRequest,
    pemDhParameters,
    pemEcParameters,
    pemSsh2EncryptedPrivateKey,
    pemSsh2PublicKey,
    pemSynopseSignature,
    pemSynopseCertificate,
    pemSynopsePrivateKeyAndCertificate);
  PPemKind = ^TPemKind;

const
  /// the supported trailer markers of a PEM text instance
  // - only the first 10 chars after -----BEGIN will be used for recognition
  PEM_BEGIN: array[TPemKind] of RawUtf8 = (
    '-----BEGIN PRIVACY-ENHANCED MESSAGE-----'#13#10,
    '-----BEGIN CERTIFICATE-----'#13#10,
    '-----BEGIN X509 CRL-----'#13#10,
    '-----BEGIN PRIVATE KEY-----'#13#10,
    '-----BEGIN PUBLIC KEY-----'#13#10,
    '-----BEGIN RSA PRIVATE KEY-----'#13#10,
    '-----BEGIN RSA PUBLIC KEY-----'#13#10,
    '-----BEGIN EC PRIVATE KEY-----'#13#10,
    '-----BEGIN ENCRYPTED PRIVATE KEY-----'#13#10,
    '-----BEGIN CERTIFICATE REQUEST-----'#13#10,
    '-----BEGIN DH PARAMETERS-----'#13#10,
    '-----BEGIN EC PARAMETERS-----'#13#10,
    '-----BEGIN SSH2 ENCRYPTED PRIVATE KEY-----'#13#10,
    '-----BEGIN SSH2 PUBLIC KEY-----'#13#10,
    '-----BEGIN SYNECC SIGNATURE-----'#13#10,
    '-----BEGIN SYNECC CERTIFICATE-----'#13#10,
    '-----BEGIN SYNECC PRIVATE KEY AND CERTIFICATE-----'#13#10);

  /// the supported ending markers of a PEM text instance
  PEM_END: array[TPemKind] of RawUtf8 = (
    '-----END PRIVACY-ENHANCED MESSAGE-----'#13#10,
    '-----END CERTIFICATE-----'#13#10,
    '-----END X509 CRL-----'#13#10,
    '-----END PRIVATE KEY-----'#13#10,
    '-----END PUBLIC KEY-----'#13#10,
    '-----END RSA PRIVATE KEY-----'#13#10,
    '-----END RSA PUBLIC KEY-----'#13#10,
    '-----END EC PRIVATE KEY-----'#13#10,
    '-----END ENCRYPTED PRIVATE KEY-----'#13#10,
    '-----END CERTIFICATE REQUEST-----'#13#10,
    '-----END DH PARAMETERS-----'#13#10,
    '-----END EC PARAMETERS-----'#13#10,
    '-----END SSH2 ENCRYPTED PRIVATE KEY-----'#13#10,
    '-----END SSH2 PUBLIC KEY-----'#13#10,
    '-----END SYNECC SIGNATURE-----'#13#10,
    '-----END SYNECC CERTIFICATE-----'#13#10,
    '-----END SYNECC PRIVATE KEY AND CERTIFICATE-----'#13#10);

/// convert a binary DER content into a single-instance PEM text
function DerToPem(der: pointer; len: PtrInt; kind: TPemKind): TCertPem; overload;

/// convert a binary DER content into a single-instance PEM text
function DerToPem(const der: TCertDer; kind: TPemKind): TCertPem; overload;

/// convert a single-instance PEM text file into a binary DER
// - if the supplied buffer doesn't start with '-----BEGIN .... -----'
// trailer, will expect the input to be plain DER binary and return it
function PemToDer(const pem: TCertPem; kind: PPemKind = nil): TCertDer;

/// parse a multi-PEM text input and return the next PEM content
// - search and identify any PEM_BEGIN/PEM_END markers
// - ready to be decoded via PemToDer()
// - optionally returning the recognized TPemKind (maybe pemUnspecified)
function NextPem(var P: PUtf8Char; Kind: PPemKind = nil): TCertPem;

/// quickly check the begin/end of a single-instance PEM text
// - do not validate the internal Base64 encoding, just the trailer/ending lines
// - expects at least a single-instance PEM, with ----BEGIN and -----END markers
function IsPem(const pem: RawUtf8): boolean;

/// quickcly check if a PEM text is likely to be encrypted
// - search for a PEM format, with ENCRYPTED keyword
// - won't decode the Base64 encoded binary, so may return some false negative
function IsPemEncrypted(const pem: TCertPem): boolean;

/// low-level binary-to-DER encoder
function DerAppend(P: PAnsiChar; buf: PByteArray; buflen: PtrUInt): PAnsiChar;

/// low-level DER sequence to binary decoding
// - only support a single DER_INTEGER sequence format as generated by DerAppend()
function DerParse(P: PAnsiChar; buf: PByteArray; buflen: PtrInt): PAnsiChar;


{ ************** High-Level (X509) Certificates Process }


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
  FillZero(dig.b);
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
    '.md5',
    '.sha1',
    '.sha256',
    '.sha384',
    '.sha512',
    '.sha3-256',
    '.sha3-512');

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


function CryptCrc32(algo: TCrc32Algo): THasher;
begin
  case algo of
    caCrc32c:
      result := crc32c;
    caCrc32:
      result := crc32;   // maybe from mormot.lib.z
    caAdler32:
      result := adler32; // maybe from mormot.lib.z
    caxxHash32:
      result := @xxHash32;
    caFnv32:
      result := @fnv32;
  else
    result := nil;
  end;
end;


function HashFull(aAlgo: THashAlgo; aBuffer: Pointer; aLen: integer): RawUtf8;
var
  hasher: TSynHasher;
begin
  result := hasher.Full(aAlgo, aBuffer, aLen);
end;

function HashFull(aAlgo: THashAlgo; const aBuffer: RawByteString): RawUtf8;
var
  hasher: TSynHasher;
begin
  result := hasher.Full(aAlgo, pointer(aBuffer), length(aBuffer));
end;

function HashFileRaw(const aFileName: TFileName; aAlgos: THashAlgos): TRawUtf8DynArray;
var
  hasher: array of TSynHasher;
  temp: RawByteString;
  F: THandle;
  size, tempsize: Int64;
  n, read: integer;
  a: THashAlgo;
  h: PtrInt;
begin
  result := nil;
  if aFileName = '' then
    exit;
  n := 0;
  for a := low(a) to high(a) do
    inc(n, ord(a in aAlgos));
  if n = 0 then
    exit;
  F := FileOpenSequentialRead(aFileName);
  if ValidHandle(F) then
  try
    SetLength(hasher, n);
    h := 0;
    for a := low(a) to high(a) do
      if a in aAlgos then
        if hasher[h].Init(a) then
          inc(h)
        else
          exit;
    size := FileSize(F);
    tempsize := 1 shl 20; // 1MB temporary buffer for reading
    if tempsize > size then
      tempsize := size;
    SetLength(temp, tempsize);
    dec(n);
    while size > 0 do
    begin
      read := FileRead(F, pointer(temp)^, tempsize);
      if read <= 0 then
        exit;
      for h := 0 to n do
        hasher[h].Update(pointer(temp), read);
      dec(size, read);
    end;
    SetLength(result, n + 1);
    for h := 0 to n do
      result[h] := hasher[h].Final;
  finally
    FileClose(F);
  end;
end;

function HashFile(const aFileName: TFileName; aAlgo: THashAlgo): RawUtf8;
var
  h: TRawUtf8DynArray;
begin
  h := HashFileRaw(aFileName, [aAlgo]);
  if h = nil then
    result := ''
  else
    result := h[0];
end;

const
  ALGO_EXT: array[THashAlgo] of string[8] = (
    'md5',
    'sha1',
    'sha256',
    'sha384',
    'sha512',
    'sha3_256',
    'sha3_512');

procedure HashFile(const aFileName: TFileName; aAlgos: THashAlgos);
var
  h: TRawUtf8DynArray;
  efn, fn: TFileName;
  a: THashAlgo;
  n: PtrInt;
begin
  h := HashFileRaw(aFileName, aAlgos);
  if h = nil then
    exit;
  efn := ExtractFileName(aFileName);
  n := 0;
  for a := low(a) to high(a) do
    if a in aAlgos then
    begin
      fn := FormatString('%.%', [efn, ALGO_EXT[a]]);
      FileFromString(FormatUtf8('% *%', [h[n], efn]), fn);
      inc(n);
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
      end; // note: the HMAC pattern is included in SHA-3 sponge design
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
  out aDerivatedKey: THash512Rec; const aDefaultSalt: RawUtf8;
  aDefaultAlgo: TSignAlgo);
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
         (ord(k.algo) > ord(high(k.algo))) or
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
  out aDerivatedKey: THash512Rec; const aDefaultSalt: RawUtf8;
  aDefaultAlgo: TSignAlgo);
begin
  Pbkdf2(pointer(aParamsJson), length(aParamsJson),
    aDerivatedKey, aDefaultSalt, aDefaultAlgo);
end;

procedure TSynSigner.AssignTo(var aDerivatedKey: THash512Rec;
  out aAes: TAes; aEncrypt: boolean);
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
  aAes.DoInit(aDerivatedKey, ks, aEncrypt);
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

function ToText(algo: TCrc32Algo): PShortString;
begin
  result := GetEnumName(TypeInfo(TCrc32Algo), ord(algo));
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
    // handle '....,username:passwordbase64,....' or 'username:passwordbase64'
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
      raise ECrypt.CreateUtf8('%.GetPassWordPlain unable to retrieve the ' +
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
  FastSetRawByteString(tmp, pointer(value), Length(value)); // private copy
  SymmetricEncrypt(GetKey, tmp);
  fPassWord := BinToBase64(tmp);
end;


{ TSynConnectionDefinition }

constructor TSynConnectionDefinition.CreateFromJson(
  const Json: RawUtf8; Key: cardinal);
var
  privateCopy: RawUtf8;
  values: array[0..4] of TValuePUtf8Char;
begin
  inherited Create; // may have been overriden
  fKey := Key;
  privateCopy := Json;
  JsonDecode(privateCopy,
    ['Kind',          // 0
     'ServerName',    // 1
     'DatabaseName',  // 2
     'User',          // 3
     'Password'],     // 4
    @values);
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
  raise ECrypt.CreateUtf8('%.AuthenticateUser() is not implemented', [self]);
end;

procedure TSynAuthenticationAbstract.DisauthenticateUser(const aName: RawUtf8);
begin
  raise ECrypt.CreateUtf8('%.DisauthenticateUser() is not implemented', [self]);
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
  fSafe.WriteLock;
  try
    AddSortedInteger(fIP4, fCount, ip4);
    result := true;
  finally
    fSafe.WriteUnLock;
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
  fSafe.ReadWriteLock;
  try
    i := FastFindIntegerSorted(pointer(fIP4), fCount - 1, ip4);
    if i < 0 then
      exit;
    fSafe.WriteLock;
    DeleteInteger(fIP4, fCount, i);
    fSafe.WriteUnLock;
    result := true;
  finally
    fSafe.ReadWriteUnLock;
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
  fSafe.ReadOnlyLock;
  try
    if FastFindIntegerSorted(pointer(fIP4), fCount - 1, ip4) >= 0 then
      result := true;
  finally
    fSafe.ReadOnlyUnLock;
  end;
end;

function TIPBan.DynArrayLocked(aLock: TRWLockContext): TDynArray;
begin
  fSafe.Lock(aLock);
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
    'Created',    DateTimeToIso8601Text(CreateDateTime),
    'Identifier', ProcessID,
    'Counter',    Counter,
    'Value',      Value,
    'Hex',        Int64ToHex(Value)], JSON_FAST);
end;

function TSynUniqueIdentifierBits.Equal(
  const Another: TSynUniqueIdentifierBits): boolean;
begin
  result := Value = Another.Value;
end;

procedure TSynUniqueIdentifierBits.From(const aID: TSynUniqueIdentifier);
begin
  Value := aID;
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

procedure TSynUniqueIdentifierGenerator.ComputeNew(
  out result: TSynUniqueIdentifierBits);
var
  currentTime: cardinal;
begin
  currentTime := UnixTimeUtc; // under Windows faster than GetTickCount64
  fSafe.Lock;
  {$ifdef HASFASTTRYFINALLY}
  try
  {$else}
  begin
  {$endif HASFASTTRYFINALLY}
    if currentTime > fUnixCreateTime then // time may have been tweaked: compare
    begin
      fUnixCreateTime := currentTime;
      fLastCounter := 0; // reset
    end;
    if fLastCounter = $7fff then
    begin
      // collide if more than 32768 per second (unlikely) -> tweak the timestamp
      inc(fUnixCreateTime);
      inc(fCollisions);
      fLastCounter := 0;
    end
    else
      inc(fLastCounter);
    result.Value := Int64(fLastCounter or fIdentifierShifted) or
                    (Int64(fUnixCreateTime) shl 31);
    inc(fComputedCount);
  {$ifdef HASFASTTRYFINALLY}
  finally
  {$endif HASFASTTRYFINALLY}
    fSafe.UnLock;
  end;
end;

function TSynUniqueIdentifierGenerator.ComputeNew: Int64;
begin
  ComputeNew(PSynUniqueIdentifierBits(@result)^);
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
  inherited Create; // may have been overriden
  fIdentifier := aIdentifier;
  fIdentifierShifted := aIdentifier shl 15;
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
  block: THash128Rec; // 128-bit
  bits: TSynUniqueIdentifierObfuscatedBits absolute block; // 64+32 = 96-bit
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
      block.c3 := fCryptoCRC; // last 32-bit used as IV during AES permutation
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

procedure XorMemoryCtr(data: PCardinal; size: PtrUInt; ctr: cardinal;
  key256bytes: PCardinalArray);
begin
  while size >= SizeOf(cardinal) do
  begin
    dec(size, SizeOf(cardinal));
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
  DefaultSessionTimeOutMinutes: cardinal; SignAlgo: TCrc32Algo);
begin
  CookieName := Name;
  // initial random session ID, small enough to remain 31-bit > 0
  SessionSequence := Random32 and $07ffffff;
  SessionSequenceStart := SessionSequence;
  // temporary secret for checksum
  Secret := Random32;
  // temporary secret for encryption
  CryptNonce := Random32;
  // expiration
  DefaultTimeOutMinutes := DefaultSessionTimeOutMinutes;
  // default algorithm is 0, i.e. crc32c()
  if not Assigned(CryptCrc32(SignAlgo)) then
    raise ECrypt.CreateUtf8(
      'Unsupported TBinaryCookieGenerator.Init(%)', [ToText(SignAlgo)^]);
  CrcAlgo := SignAlgo;
  Padding := 0;
  // cryptographic randomness
  MainAesPrng.FillRandom(@Crypt, SizeOf(Crypt));
end;

type
  // map the binary layout of our Base64 serialized cookies
  TCookieContent = packed record
    head: packed record
      cryptnonce: cardinal; // ctr to cipher following bytes
      crc: cardinal;        // = 32-bit digital signature (CrcAlgo)
      session: integer;     // = jti claim
      issued: cardinal;     // = iat claim (from UnixTimeUtc-UNIXTIME_MINIMAL)
      expires: cardinal;    // = exp claim
    end;
    data: array[0..2047] of byte; // optional record binary serialization
  end;

function TBinaryCookieGenerator.Generate(out Cookie: RawUtf8;
  TimeOutMinutes: cardinal; PRecordData: pointer;
  PRecordTypeInfo: PRttiInfo): TBinaryCookieGeneratorSessionID;
var
  cc: TCookieContent; // local working buffer
  crc: THasher;
  tmp: TSynTempBuffer;
begin
  tmp.Init(0);
  try
    result := InterlockedIncrement(integer(SessionSequence));
    if (PRecordData <> nil) and
       (PRecordTypeInfo <> nil) then
    begin
      BinarySave(PRecordData, tmp, PRecordTypeInfo, rkRecordTypes);
      if tmp.len > SizeOf(cc.data) then
        // all cookies storage should be < 4K so a single 2K cookie seems huge
        raise ECrypt.Create('TBinaryCookieGenerator: Too Big Too Fat');
    end;
    cc.head.cryptnonce := Random32;
    cc.head.session := result;
    cc.head.issued := UnixTimeUtc - UNIXTIME_MINIMAL;
    if TimeOutMinutes = 0 then
      TimeOutMinutes := DefaultTimeOutMinutes;
    if TimeOutMinutes = 0 then
      // 1 month expiration is a reasonable high value for "never expires"
      TimeOutMinutes := 31 * 24 * 60;
    cc.head.expires := cc.head.issued + TimeOutMinutes * 60;
    if tmp.len > 0 then
      MoveFast(tmp.buf^, cc.data, tmp.len);
    inc(tmp.len, SizeOf(cc.head));
    crc := CryptCrc32(CrcAlgo); // in 2 steps for Delphi 7/2007
    cc.head.crc := crc(Secret, @cc.head.session, tmp.len - 8);
    XorMemoryCtr(@cc.head.crc, tmp.len - 4,
      {ctr=}CryptNonce xor cc.head.cryptnonce, @Crypt);
    Cookie := BinToBase64Uri(@cc, tmp.len);
  finally
    tmp.Done;
  end;
end;

function TBinaryCookieGenerator.Validate(const Cookie: RawUtf8;
  PRecordData: pointer; PRecordTypeInfo: PRttiInfo;
  PExpires, PIssued: PUnixTime): TBinaryCookieGeneratorSessionID;
var
  clen, len: integer;
  now: cardinal;
  ccend: PAnsiChar;
  crc: THasher;
  cc: TCookieContent;
begin
  result := 0; // parsing/crc/timeout error
  if Cookie = '' then
    exit;
  clen := length(Cookie);
  len := Base64uriToBinLength(clen);
  if (len >= SizeOf(cc.head)) and
     (len <= SizeOf(cc)) and
     Base64uriDecode(pointer(Cookie), @cc, clen) then
  begin
    crc := CryptCrc32(CrcAlgo); // in 2 steps for Delphi 7/2007
    XorMemoryCtr(@cc.head.crc, len - SizeOf(cc.head.cryptnonce),
      {ctr=}CryptNonce xor cc.head.cryptnonce, @Crypt);
    if (cardinal(cc.head.session) >= cardinal(SessionSequenceStart)) and
       (cardinal(cc.head.session) <= cardinal(SessionSequence)) and
       (crc(Secret, @cc.head.session, len - 8) = cc.head.crc) then
    begin
      if PExpires <> nil then
        PExpires^ := cc.head.expires + UNIXTIME_MINIMAL;
      if PIssued <> nil then
        PIssued^ := cc.head.issued + UNIXTIME_MINIMAL;
      now := UnixTimeUtc - UNIXTIME_MINIMAL;
      if (cc.head.issued <= now) and
         (cc.head.expires >= now) then
        if (PRecordData = nil) or
           (PRecordTypeInfo = nil) then
          result := cc.head.session
        else if len > SizeOf(cc.head) then
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
  if not Assigned(CryptCrc32(CrcAlgo)) then
    raise ECrypt.CreateUtf8(
      'Unsupported TBinaryCookieGenerator.Load(%)', [ToText(CrcAlgo)^]);
end;



{ ************* Rnd/Hash/Sign/Cipher/Asym/Cert/Store High-Level Algorithms Factories }

var
  GlobalCryptAlgo: TRawUtf8List; // Objects[] are TCryptAlgo instances

procedure GlobalCryptAlgoInit; forward;


{ TCryptAlgo }

constructor TCryptAlgo.Create(const name: RawUtf8);
begin
  if name = '' then
    raise ECrypt.CreateUtf8('Unexpected %.Create('''')', [self]);
  fName := name;
  GlobalCryptAlgo.AddOrReplaceObject(name, self);
end;

class function TCryptAlgo.InternalFind(
  const name: RawUtf8; var Last: TCryptAlgo): pointer;
begin
  if name = '' then
  begin
    result := nil;
    exit;
  end;
  result := Last; // simple but efficient cache
  if (result <> nil) and
     IdemPropNameU(TCryptAlgo(result).fName, name) then
    exit;
  result := GlobalCryptAlgo.GetObjectFrom(name); // thread-safe lookup
  if result <> nil then
    if TCryptAlgo(result).InheritsFrom(self) then
      Last := result
    else
      result := nil;
end;

class function TCryptAlgo.InternalResolve(
  const name: RawUtf8; CSV: PUtf8Char): integer;
begin
  result := FindCsvIndex(CSV, name, ',', {casesens=}false);
  if result < 0 then
    raise ECrypt.CreateUtf8('%.Create(''%''): unknown algorithm - try %',
      [self, name, CSV]);
end;

class function TCryptAlgo.Implements(const name: array of RawUtf8): pointer;
var
  i: PtrInt;
begin
  if GlobalCryptAlgo = nil then
    GlobalCryptAlgoInit;
  result := nil;
  for i := 0 to high(name) do
    if name[i] <> '' then
      result := Create(name[i]);
end;

class procedure TCryptAlgo.Implements(csv: PUtf8Char; const suffix: RawUtf8);
var
  name: RawUtf8;
begin
  if GlobalCryptAlgo = nil then
    GlobalCryptAlgoInit;
  while csv <> nil do
  begin
    GetNextItem(csv, ',', name);
    if name <> '' then
      Create(name + suffix);
  end;
end;

class function TCryptAlgo.Instances: TCryptAlgos;
var
  i: PtrInt;
  n: integer;
  o: PObjectArray;
begin
  if GlobalCryptAlgo = nil then
    GlobalCryptAlgoInit;
  result := nil;
  n := 0;
  GlobalCryptAlgo.Safe.ReadOnlyLock;
  try
    o := pointer(GlobalCryptAlgo.ObjectPtr);
    for i := 0 to GlobalCryptAlgo.Count - 1 do
      if o[i].InheritsFrom(self) then
        ObjArrayAddCount(result, o[i], n);
  finally
    GlobalCryptAlgo.Safe.ReadOnlyUnLock;
  end;
  if n <> 0 then
    DynArrayFakeLength(result, n);
end;

class function TCryptAlgo.Names: TRawUtf8DynArray;
var
  i: PtrInt;
  n: integer;
  o: PObjectArray;
begin
  if GlobalCryptAlgo = nil then
    GlobalCryptAlgoInit;
  result := nil;
  n := 0;
  GlobalCryptAlgo.Safe.ReadOnlyLock;
  try
    o := pointer(GlobalCryptAlgo.ObjectPtr);
    for i := 0 to GlobalCryptAlgo.Count - 1 do
      if o[i].InheritsFrom(self) then
        AddRawUtf8(result, n, TCryptAlgo(o[i]).fName);
  finally
    GlobalCryptAlgo.Safe.ReadOnlyUnLock;
  end;
  if n <> 0 then
    DynArrayFakeLength(result, n);
end;


{ TCryptInstance }

constructor TCryptInstance.Create(algo: TCryptAlgo);
begin
  if algo = nil then
    raise ECrypt.CreateUtf8('Unexpected %.Create(nil)', [self]);
  fCryptAlgo := algo;
end;

var
  LastAlgoInstance: TCryptAlgo;

constructor TCryptInstance.Create(const name: RawUtf8);
var
  algo: TCryptAlgo;
begin
  algo := TCryptAlgo.InternalFind(name, LastAlgoInstance);
  if algo = nil then
    raise ECrypt.CreateUtf8('Unexpected %.Create(''%'')', [self, name]);
  Create(algo);
end;


{ TCryptRandom }

function TCryptRandom.Get(len: PtrInt): RawByteString;
begin
  FastSetRawByteString(result, nil, len);
  Get(pointer(result), len);
end;

function TCryptRandom.GetBytes(len: PtrInt): TBytes;
begin
  result := nil;
  SetLength(result, len);
  Get(pointer(result), len);
end;

function TCryptRandom.Get32: cardinal;
begin
  Get(@result, 4);
end;

function TCryptRandom.Get32(max: cardinal): cardinal;
begin
  result := (QWord(Get32) * max) shr 32;
end;

function TCryptRandom.GetDouble: double;
const
  COEFF32: double = 1.0 / (Int64(1) shl 32);
begin
  result := Get32 * COEFF32; // 32-bit resolution is enough for our purpose
end;


{ TCryptRandomEntropy }

type
  TCryptRandomEntropy = class(TCryptRandom)
  protected
    fSource: TAesPrngGetEntropySource;
  public
    constructor Create(const name: RawUtf8); override;
    function Get(len: PtrInt): RawByteString; override;
    procedure Get(dst: pointer; dstlen: PtrInt); override;
  end;

const
  /// CSV text of TAesPrngGetEntropySource items
  RndAlgosText: PUTF8Char =
    'rnd-entropy,rnd-entropysys,rnd-entropysysblocking,rnd-entropyuser';

constructor TCryptRandomEntropy.Create(const name: RawUtf8);
begin
  fSource := TAesPrngGetEntropySource(InternalResolve(name, RndAlgosText));
  inherited Create(name); // should be done after InternalResolve()
end;

function TCryptRandomEntropy.Get(len: PtrInt): RawByteString;
begin
  result := TAesPrng.GetEntropy(len, fSource);
end;

procedure TCryptRandomEntropy.Get(dst: pointer; dstlen: PtrInt);
var
  tmp: RawByteString;
begin
  tmp := TAesPrng.GetEntropy(dstlen, fSource);
  MoveFast(pointer(tmp)^, dst^, dstlen);
  FillZero(tmp);
end;


{ TCryptRandomAesPrng }

type
  TCryptRandomAesPrng = class(TCryptRandom)
  public
    procedure Get(dst: pointer; dstlen: PtrInt); override;
  end;

procedure TCryptRandomAesPrng.Get(dst: pointer; dstlen: PtrInt);
begin
  MainAesPrng.FillRandom(dst, dstlen);
end;


{ TCryptRandomSysPrng }

type
  TCryptRandomSysPrng = class(TCryptRandom)
  public
    procedure Get(dst: pointer; dstlen: PtrInt); override;
  end;

procedure TCryptRandomSysPrng.Get(dst: pointer; dstlen: PtrInt);
begin // 'rnd-system,rnd-systemblocking'
  FillSystemRandom(dst, dstlen, length(fName) > 10);
end;


{ TCryptRandomLecuyerPrng }

type
  TCryptRandomLecuyerPrng = class(TCryptRandom)
  public
    procedure Get(dst: pointer; dstlen: PtrInt); override;
    function Get32: cardinal; override;
  end;

procedure TCryptRandomLecuyerPrng.Get(dst: pointer; dstlen: PtrInt);
begin
  RandomBytes(dst, dstlen); // use Lecuyer's gsl_rng_taus2 generator
end;

function TCryptRandomLecuyerPrng.Get32: cardinal;
begin
  result := Random32;
end;


{ TCryptHash }

function TCryptHash.Final(digest: pointer; digestlen: PtrInt): PtrInt;
var
  dig: THash512Rec;
begin
  result := InternalFinal(dig);
  if result <= digestlen then
    MoveFast(dig, digest^, result)
  else
    result := 0;
  FillZero(dig.b);
end;

function TCryptHash.Final: RawUtf8;
var
  dig: THash512Rec;
begin
  BinToHexLower(@dig, InternalFinal(dig), result);
  FillZero(dig.b);
end;

function TCryptHash.Update(const buf: RawByteString): ICryptHash;
begin
  result := Update(pointer(buf), length(buf));
end;

procedure TCryptHash.UpdateFile(const filename: TFileName);
var
  temp: RawByteString;
  F: THandle;
  size, tempsize: Int64;
  read: integer;
begin
  if filename = '' then
    exit;
  F := FileOpenSequentialRead(filename);
  if ValidHandle(F) then
  try
    size := FileSize(F);
    tempsize := 1 shl 20; // 1MB temporary buffer for reading
    if tempsize > size then
      tempsize := size;
    SetLength(temp, tempsize);
    while size > 0 do
    begin
      read := FileRead(F, pointer(temp)^, tempsize);
      if read <= 0 then
        exit;
      Update(pointer(temp), read);
      dec(size, read);
    end;
  finally
    FileClose(F);
  end;
end;


{ TCryptHasher }

function TCryptHasher.Full(buf: pointer; buflen: PtrInt): RawUtf8;
var
  h: ICryptHash;
begin
  h := New;
  h.Update(buf, buflen);
  result := h.Final;
end;

function TCryptHasher.Full(const buf: RawByteString): RawUtf8;
begin
  result := Full(pointer(buf), length(buf));
end;

function TCryptHasher.Full(buf: pointer; buflen: PtrInt;
  out digest: THash512Rec): PtrInt;
var
  h: ICryptHash;
begin
  h := New;
  h.Update(buf, buflen);
  result := h.Final(@digest, SizeOf(digest));
end;

function TCryptHasher.FullFile(const filename: TFileName): RawUtf8;
var
  h: ICryptHash;
begin
  h := New;
  h.UpdateFile(filename);
  result := h.Final;
end;



type
  TCryptCrc32Internal = class(TCryptHasher)
  protected
    fAlgo: TCrc32Algo;
  public
    constructor Create(const name: RawUtf8); override;
    function New: ICryptHash; override;
  end;

  TCryptCrcInternal = class(TCryptHash)
  protected
    fCrc: cardinal;
    fFunc: THasher;
    function InternalFinal(out dig: THash512Rec): PtrInt; override;
  public
    constructor Create(algo: TCryptCrc32Internal); reintroduce;
    function Update(buf: pointer; buflen: PtrInt): ICryptHash; override;
  end;


{ TCryptCrc32Internal }

const
  /// CSV text of TCrc32Algo items
  CrcAlgosText: PUtf8Char = 'CRC32,CRC32C,XXHASH32,ADLER32,FNV32';

constructor TCryptCrc32Internal.Create(const name: RawUtf8);
begin
  fAlgo := TCrc32Algo(InternalResolve(name, CrcAlgosText));
  inherited Create(name); // should be done after InternalResolve()
end;

function TCryptCrc32Internal.New: ICryptHash;
begin
  result := TCryptCrcInternal.Create(self);
end;


{ TCryptCrcInternal }

constructor TCryptCrcInternal.Create(algo: TCryptCrc32Internal);
begin
  // resolve fFunc in New/Create, since crc32/adler functions may be set later
  fFunc := CryptCrc32(algo.fAlgo);
  if not Assigned(fFunc) then
    raise ECrypt.CreateUtf8('%.New: unavailable ''%'' function', [self, algo.fName]);
  inherited Create(algo);
end;

function TCryptCrcInternal.Update(buf: pointer; buflen: PtrInt): ICryptHash;
begin
  fCrc := fFunc(fCrc, buf, buflen);
  result := self;
end;

function TCryptCrcInternal.InternalFinal(out dig: THash512Rec): PtrInt;
begin
  dig.c[0] := fCrc;
  result := SizeOf(fCrc);
end;


{ TCryptHasherInternal }

type
  TCryptHasherInternal = class(TCryptHasher)
  protected
    fAlgo: THashAlgo;
  public
    constructor Create(const name: RawUtf8); override;
    function New: ICryptHash; override;
  end;

  TCryptHashInternal = class(TCryptHash)
  protected
    fAlgo: TSynHasher;
    function InternalFinal(out dig: THash512Rec): PtrInt; override;
  public
    destructor Destroy; override;
    function Update(buf: pointer; buflen: PtrInt): ICryptHash; override;
  end;

const
  /// CSV text of THashAlgo items, as recognized by Hasher/Hash factories
  HashAlgosText: PUtf8Char = 'MD5,SHA1,SHA256,SHA384,SHA512,SHA3_256,SHA3_512';

constructor TCryptHasherInternal.Create(const name: RawUtf8);
begin
  fAlgo := THashAlgo(InternalResolve(name, HashAlgosText));
  inherited Create(name); // should be done after InternalResolve()
end;

function TCryptHasherInternal.New: ICryptHash;
var
  h: TCryptHashInternal;
begin
  h := TCryptHashInternal.Create(self);
  h.fAlgo.Init(fAlgo);
  result := h;
end;


{ TCryptHashInternal }

destructor TCryptHashInternal.Destroy;
begin
  inherited Destroy;
  FillCharFast(fAlgo, SizeOf(fAlgo), 0); // override memory
end;

function TCryptHashInternal.Update(buf: pointer; buflen: PtrInt): ICryptHash;
begin
  fAlgo.Update(buf, buflen);
  result := self;
end;

function TCryptHashInternal.InternalFinal(out dig: THash512Rec): PtrInt;
begin
  fAlgo.Final(dig);
  result := fAlgo.HashSize;
end;


{ TCryptSigner }

function TCryptSigner.Full(key, buf: pointer; keylen, buflen: PtrInt): RawUtf8;
var
  h: ICryptHash;
begin
  h := New(key, keylen);
  h.Update(buf, buflen);
  result := h.Final;
end;

function TCryptSigner.Full(const key, buf: RawByteString): RawUtf8;
begin
  result := Full(pointer(key), pointer(buf), length(key), length(buf));
end;

function TCryptSigner.FullFile(key: pointer; keylen: PtrInt;
  const filename: TFileName): RawUtf8;
var
  h: ICryptHash;
begin
  h := New(key, keylen);
  h.UpdateFile(filename);
  result := h.Final;
end;

function TCryptSigner.NewPbkdf2(const secret, salt: RawUtf8; rounds: integer): ICryptHash;
var
  key: THash512Rec;
begin
  result := New(@key, Pbkdf2(secret, salt, rounds, key));
end;


{ TCryptSignerInternal }

type
  TCryptSignerInternal = class(TCryptSigner)
  protected
    fAlgo: TSignAlgo;
  public
    constructor Create(const name: RawUtf8); override;
    function New(key: pointer; keylen: PtrInt): ICryptHash; override;
    function Pbkdf2(const secret, salt: RawUtf8; rounds: integer;
      out key: THash512Rec): integer; override;
  end;

  TCryptSignInternal = class(TCryptHash)
  protected
    fAlgo: TSynSigner;
    function InternalFinal(out dig: THash512Rec): PtrInt; override;
  public
    constructor Create(const signer: TSynSigner; const key: THash512Rec); overload;
    function Update(buf: pointer; buflen: PtrInt): ICryptHash; override;
  end;

const
  /// CSV text of TSignAlgo items, as recognized by Signer/Sign factories
  SignAlgosText: PUtf8Char = 'HMAC-SHA1,HMAC-SHA256,HMAC-SHA384,HMAC-SHA512,' +
    'SHA3-224,SHA3-256,SHA3-384,SHA3-512,SHA3-S128,SHA3-S256';

constructor TCryptSignerInternal.Create(const name: RawUtf8);
begin
  fAlgo := TSignAlgo(InternalResolve(name, SignAlgosText));
  inherited Create(name); // should be done after InternalResolve()
end;

function TCryptSignerInternal.New(key: pointer; keylen: PtrInt): ICryptHash;
var
  s: TCryptSignInternal;
begin
  s := TCryptSignInternal.Create(self);
  s.fAlgo.Init(fAlgo, key, keylen);
  result := s;
end;

function TCryptSignerInternal.Pbkdf2(const secret, salt: RawUtf8;
  rounds: integer; out key: THash512Rec): integer;
var
  s: TSynSigner;
begin
  s.Pbkdf2(fAlgo, secret, salt, rounds, key);
  result := s.SignatureSize;
end;


{ TCryptSignInternal }

constructor TCryptSignInternal.Create(const signer: TSynSigner;
  const key: THash512Rec);
begin
  // directly called by Sign(paramsjson) factory - fCryptAlgo is ignored here
  fAlgo.Init(signer.Algo, @key, signer.SignatureSize);
end;

function TCryptSignInternal.Update(buf: pointer; buflen: PtrInt): ICryptHash;
begin
  fAlgo.Update(buf, buflen);
  result := self;
end;

function TCryptSignInternal.InternalFinal(out dig: THash512Rec): PtrInt;
begin
  fAlgo.Final(dig, {noinit=}false);
  result := fAlgo.SignatureSize;
end;


{ TCryptCipherAlgo }

function TCryptCipherAlgo.Encrypt(key: pointer): ICryptCipher;
begin
  result := New(key, {encrypt=}true);
end;

function TCryptCipherAlgo.Decrypt(key: pointer): ICryptCipher;
begin
  result := New(key, {encrypt=}false);
end;

function TCryptCipherAlgo.New(const hash, secret, salt: RawUtf8;
  rounds: integer; encrypt: boolean): ICryptCipher;
var
  s: TCryptSigner;
  key: THash512Rec;
begin
  s := Signer(hash);
  if s = nil then
    raise ECrypt.CreateUtf8('%.New: unknown ''%'' hash', [self, hash]);
  FillZero(key.b); // s.Pbkdf2 may generate less bits than the cipher consumes
  s.Pbkdf2(secret, salt, rounds, key);
  result := New(@key, encrypt);
end;

function TCryptCipherAlgo.Encrypt(const sign, secret, salt: RawUtf8;
  rounds: integer): ICryptCipher;
begin
  result := New(sign, secret, salt, rounds, {encrypt=}true);
end;

function TCryptCipherAlgo.Decrypt(const sign, secret, salt: RawUtf8;
  rounds: integer): ICryptCipher;
begin
  result := New(sign, secret, salt, rounds, {encrypt=}false);
end;



{ TCryptAesInternal }

type
  TCryptAesInternal = class(TCryptCipherAlgo)
  protected
    fMode: TAesMode;
    fBits: integer;
    fEngines: TAesAbstractClasses;
  public
    constructor Create(const name: RawUtf8; mode: TAesMode; bits: integer;
      const engines: TAesAbstractClasses); reintroduce;
    function New(key: pointer; encrypt: boolean; iv: pointer): ICryptCipher; override;
    function IsAead: boolean; override;
  end;

  TCryptAesCipher = class(TCryptCipher)
  protected
    fAes: TAesAbstract;
    fFlags: set of (fEncrypt, fIVAtBeg, fAesGcm, fAesAead);
  public
    constructor Create(algo: TCryptAesInternal; key, iv: pointer;
      encrypt: boolean; const engines: TAesAbstractClasses); overload;
    destructor Destroy; override;
    function Clone: ICryptCipher; override;
    function Process(const src: RawByteString; out dst: RawByteString;
      const aeadinfo: RawByteString): boolean; overload; override;
    function Process(const src: TBytes; out dst: TBytes;
      const aeadinfo: TByteDynArray): boolean; overload; override;
    procedure RawProcess(src, dst: pointer; srclen, dstlen: PtrInt); override;
    function RawFinal(var gmac: TAesBlock): boolean; override;
  end;

constructor TCryptAesInternal.Create(const name: RawUtf8; mode: TAesMode;
  bits: integer; const engines: TAesAbstractClasses);
begin
  inherited Create(name);
  fMode := mode;
  fBits := bits;
  fEngines := engines;
end;

function TCryptAesInternal.New(key: pointer; encrypt: boolean; iv: pointer): ICryptCipher;
begin
  result := TCryptAesCipher.Create(self, key, iv, encrypt, fEngines);
end;

function TCryptAesInternal.IsAead: boolean;
begin
  result := fMode in AES_AEAD; // mCfc, mOfc, mCtc, mGcm
  if (fMode = mGcm) and
     (fEngines[mGcm] <> TAesInternal[mGcm]) then
    result := false; //
end;


{ TCryptAesCipher }

constructor TCryptAesCipher.Create(algo: TCryptAesInternal; key, iv: pointer;
  encrypt: boolean; const engines: TAesAbstractClasses);
begin
  inherited Create(algo);
  fAes := engines[algo.fMode].Create(key^, algo.fBits);
  if iv <> nil then
    fAes.IV := PAesBlock(iv)^
  else
    include(fFlags, fIVAtBeg);
  if encrypt then
    include(fFlags, fEncrypt);
  if algo.fMode = mGcm then
    include(fFlags, fAesGcm)
  else if algo.fMode in AES_AEAD then
    include(fFlags, fAesAead);
end;

destructor TCryptAesCipher.Destroy;
begin
  inherited Destroy;
  fAes.Free;
end;

function TCryptAesCipher.Clone: ICryptCipher;
var
  c: TCryptAesCipher;
begin
  c := TCryptAesCipher.Create(fCryptAlgo);
  c.fAes := fAes.Clone;
  c.fFlags := fFlags;
  if not (fIVAtBeg in fFlags) then
    c.fAes.IV := fAes.IV;
  result := c;
end;

function TCryptAesCipher.Process(const src: RawByteString; out dst: RawByteString;
  const aeadinfo: RawByteString): boolean;
begin
  result := false;
  if src = '' then
    exit;
  if fAesGcm in fFlags then
  begin
    // standard GCM algorithm with trailing 128-bit GMAC
    if (aeadinfo <> '') and
       not fAes.InheritsFrom(TAesGcm) then
      raise ECrypt.CreateUtf8('% does not properly support AEAD information: ' +
        'use AES-%-GCM-INT instead', [fAes, TCryptAesInternal(fCryptAlgo).fBits]);
    if fEncrypt in fFlags then
    begin
      dst := fAes.EncryptPkcs7(src, fIVAtBeg in fFlags, GMAC_SIZE);
      if aeadinfo <> '' then
        TAesGcmAbstract(fAes).AesGcmAad(pointer(aeadinfo), length(aeadinfo));
      result := (dst <> '') and
                TAesGcmAbstract(fAes).AesGcmFinal( // append GMAC to dst
                   PAesBlock(@PByteArray(dst)[length(dst) - GMAC_SIZE])^)
    end
    else
    begin
      dst := fAes.DecryptPkcs7(src, fIVAtBeg in fFlags, false, GMAC_SIZE);
      if aeadinfo <> '' then
        TAesGcmAbstract(fAes).AesGcmAad(pointer(aeadinfo), length(aeadinfo));
      result := (dst <> '') and
                TAesGcmAbstract(fAes).AesGcmFinal( // validate GMAC from src
                  PAesBlock(@PByteArray(src)[length(src) - GMAC_SIZE])^);
    end;
    exit;
  end
  else if fAesAead in fFlags then
    // our proprietary mCfc,mOfc,mCtc AEAD algorithms using 256-bit crc32c
    dst := fAes.MacAndCrypt(src, fEncrypt in fFlags, aeadinfo)
  // standard encryption with no AEAD/checksum
  else if fEncrypt in fFlags then
    dst := fAes.EncryptPkcs7(src, fIVAtBeg in fFlags)
  else
    dst := fAes.DecryptPkcs7(src, fIVAtBeg in fFlags);
  result := dst <> '';
end;

function TCryptAesCipher.Process(const src: TBytes; out dst: TBytes;
  const aeadinfo: TByteDynArray): boolean;
begin
  result := false;
  if src = nil then
    exit;
  if fAesAead in fFlags then
    raise ECrypt.CreateUtf8('%.Process(TBytes) is unsupported for %',
      [self, fCryptAlgo.AlgoName]); // MacAndCrypt() requires RawByteString
  if fAesGcm in fFlags then
  begin
    // standard GCM algorithm with trailing 128-bit GMAC
    if aeadinfo <> nil then
      TAesGcmAbstract(fAes).AesGcmAad(pointer(aeadinfo), length(aeadinfo));
    if fEncrypt in fFlags then
    begin
      dst := fAes.EncryptPkcs7(src, fIVAtBeg in fFlags, GMAC_SIZE);
      if dst <> nil then
        result := TAesGcmAbstract(fAes).AesGcmFinal( // append GMAC to dst
          PAesBlock(@PByteArray(dst)[length(dst) - GMAC_SIZE])^);
    end
    else
    begin
      dst := fAes.DecryptPkcs7(src, fIVAtBeg in fFlags, false, GMAC_SIZE);
      if dst <> nil then
        result := TAesGcmAbstract(fAes).AesGcmFinal( // validate GMAC from src
          PAesBlock(@PByteArray(src)[length(src) - GMAC_SIZE])^);
    end;
    exit;
  end
  // standard encryption with no AEAD/checksum
  else if fEncrypt in fFlags then
    dst := fAes.EncryptPkcs7(src, fIVAtBeg in fFlags)
  else
    dst := fAes.DecryptPkcs7(src, fIVAtBeg in fFlags);
  result := dst <> nil;
end;

procedure TCryptAesCipher.RawProcess(src, dst: pointer; srclen, dstlen: PtrInt);
begin
  if (dst = nil) and
     (fAesGcm in fFlags) then
    TAesGcmAbstract(fAes).AesGcmAad(src, srclen)
  else if fEncrypt in fFlags then
    fAes.Encrypt(src, dst, srclen)
  else
    fAes.Decrypt(src, dst, srclen)
end;

function TCryptAesCipher.RawFinal(var gmac: TAesBlock): boolean;
begin
  result := (fAesGcm in fFlags) and
            TAesGcmAbstract(fAes).AesGcmFinal(gmac);
end;


{ TCryptAsym }

procedure TCryptAsym.GeneratePem(out pub, priv: RawUtf8;
  const privpwd: RawUtf8);
var
  derpub, derpriv: RawByteString;
begin // inherited classes should override at least one of those Generate*()
  GenerateDer(derpub, derpriv, privpwd);
  pub := DerToPem(pointer(derpub), length(derpub), pemPublicKey);
  priv := DerToPem(pointer(derpriv), length(derpriv), pemPrivateKey);
end;

procedure TCryptAsym.GenerateDer(out pub, priv: RawByteString; const privpwd: RawUtf8);
var
  pempub, pempriv: RawUtf8;
begin // inherited classes should override at least one of those Generate*()
  GeneratePem(pempub, pempriv, privpwd);
  pub := PemToDer(pempub);
  priv := PemToDer(pempriv);
end;

function TCryptAsym.Sign(const msg, priv: RawByteString; out sig: RawByteString;
  const hashername, privpwd: RawUtf8): boolean;
begin
  result := Sign(Hasher(hashername), pointer(msg), length(msg), priv, sig, privpwd);
end;

function TCryptAsym.Sign(const msg, priv: TBytes; out sig: TBytes;
  const hashername, privpwd: RawUtf8): boolean;
var
  p, s: RawByteString;
begin
  BytesToRawByteString(priv, p);
  result := Sign(Hasher(hashername), pointer(msg), length(msg), p, s, privpwd);
  if result then
    RawByteStringToBytes(s, sig);
end;

function TCryptAsym.Verify(const msg, pub, sig: RawByteString;
  const hashername: RawUtf8): boolean;
begin
  result := Verify(Hasher(hashername), pointer(msg), length(msg), pub, sig);
end;

function TCryptAsym.Verify(const msg, pub, sig: TBytes;
  const hashername: RawUtf8): boolean;
var
  p, s: RawByteString;
begin
  BytesToRawByteString(pub, p);
  BytesToRawByteString(sig, s);
  result := Verify(Hasher(hashername), pointer(msg), length(msg), p, s);
end;

function TCryptAsym.SharedSecret(const pub, priv: RawByteString): RawByteString;
begin
  result := ''; // unsupported
end;


{ TCryptCertAlgo }

function TCryptCertAlgo.Generate(Usages: TCryptCertUsages;
  const Subjects: RawUtf8; const Authority: ICryptCert;
  ExpireDays, ValidDays: integer; Fields: PCryptCertFields): ICryptCert;
begin
  result := New;
  result.Generate(Usages, Subjects, Authority, ExpireDays, ValidDays, Fields);
end;


{ TCryptCert }

procedure TCryptCert.RaiseVoid(Instance: pointer; const Msg: shortstring);
begin
  if Instance = nil then
    RaiseError('% is not set', [Msg]);
end;

procedure TCryptCert.RaiseError(const Msg: shortstring);
begin
  raise ECryptCert.CreateUtf8('%.%', [self, Msg]);
end;

procedure TCryptCert.RaiseError(const Fmt: RawUtf8;
  const Args: array of const);
var
  msg: shortstring;
begin
  FormatShort(Fmt, Args, msg);
  RaiseError(msg);
end;

function TCryptCert.GetDigest(Algo: THashAlgo): RawUtf8;
begin
  result := HashFull(Algo, Save('', ccfBinary));
end;

function TCryptCert.Save(const PrivatePassword: RawUtf8;
  Format: TCryptCertFormat): RawByteString;
begin
  result := Save(PrivatePassword, ccfBinary);
  case Format of
    ccfHexa:
      result := BinToHex(result);
    ccfBase64:
      result := BinToBase64(result);
    ccfBase64Uri:
      result := BinToBase64uri(result);
  else
    result := '';
  end;
end;

function TCryptCert.IsEqual(const another: ICryptCert): boolean;
begin
  // HasPrivateKey is not part of the comparison
  result := Assigned(another) and
            (Save('', ccfBinary) = another.Save('', ccfBinary));
end;

function TCryptCert.Sign(const Data: RawByteString): RawByteString;
begin
  result := Sign(pointer(Data), length(Data));
end;

function TCryptCert.Verify(const Signature, Data: RawByteString): TCryptCertValidity;
begin
  result := Verify(pointer(Signature), pointer(Data),
                   length(Signature), length(Data));
end;

function TCryptCert.Instance: TCryptCert;
begin
  result := self;
end;


{ TCryptStore }

function TCryptStore.IsRevoked(const cert: ICryptCert): TCryptCertRevocationReason;
begin
  if Assigned(cert) then
    result := IsRevoked(cert.GetSerial)
  else
    result := crrNotRevoked;
end;

function TCryptStore.AddFromFile(const FileName: TFileName): TRawUtf8DynArray;
var
  tmp: RawByteString;
begin
  tmp := StringFromFile(FileName);
  if tmp <> '' then
    result := AddFromBuffer(tmp)
  else
    result := nil;
end;

function TCryptStore.AddFromFolder(const Folder, Mask: TFileName;
  Recursive: boolean): TRawUtf8DynArray;
var
  n: integer;

  procedure SearchFolder(const DirName: TFileName);
  var
    F: TSearchRec;
  begin
    if FindFirst(DirName + Mask, faAnyfile - faDirectory, F) = 0 then
    begin
      repeat
        if SearchRecValidFile(F) and
           (F.Size < 65535) then // certificate files are expected to be < 64KB
          AddRawUtf8(result, n, AddFromFile(DirName + F.Name));
      until FindNext(F) <> 0;
      FindClose(F);
    end;
    if Recursive and
       (FindFirst(DirName + '*', faDirectory, F) = 0) then
    begin
      repeat
        if SearchRecValidFolder(F) then
          SearchFolder(IncludeTrailingPathDelimiter(DirName + F.Name));
      until FindNext(F) <> 0;
      FindClose(F);
    end;
  end;

begin
  n := 0;
  SearchFolder(IncludeTrailingPathDelimiter(Folder));
  SetLength(result, n);
end;



{ TCryptStoreAlgo }

function TCryptStoreAlgo.NewFrom(const Binary: RawByteString): ICryptStore;
begin
  result := New;
  if not result.FromBinary(Binary) then
    result := nil;
end;



function ToText(r: TCryptCertRevocationReason): PShortString;
begin
  result := GetEnumName(TypeInfo(TCryptCertRevocationReason), ord(r));
end;

function ToText(u: TCryptCertUsage): PShortString;
begin
  result := GetEnumName(TypeInfo(TCryptCertUsage), ord(u));
end;

function ToText(u: TCryptCertUsages): ShortString;
begin
  GetSetNameShort(TypeInfo(TCryptCertUsages), u, result, {trim=}true);
end;

function ToText(v: TCryptCertValidity): PShortString;
begin
  result := GetEnumName(TypeInfo(TCryptCertValidity), ord(v));
end;



{ Register mormot.crypt.core and mormot.crypt.secure Algorithms }

procedure GlobalCryptAlgoInit;
var
  m: TAesMode;
  b, bits: integer;
  n: RawUtf8;
begin
  TAesPrng.Main; // initialize MainAesPrng
  GlobalLock; // RegisterGlobalShutdownRelease() will use it anyway
  try
    if GlobalCryptAlgo <> nil then
      exit;
    GlobalCryptAlgo := RegisterGlobalShutdownRelease(
      TRawUtf8List.CreateEx([fObjectsOwned, fNoDuplicate]));
    // register mormot.crypt.core engines into our factories
    TCryptRandomEntropy.Implements(RndAlgosText);
    TCryptRandomAesPrng.Implements('rnd-default,rnd-aes');
    TCryptRandomLecuyerPrng.Implements('rnd-lecuyer');
    TCryptRandomSysPrng.Implements('rnd-system,rnd-systemblocking');
    TCryptHasherInternal.Implements(HashAlgosText);
    TCryptCrc32Internal.Implements(CrcAlgosText);
    TCryptSignerInternal.Implements(SignAlgosText);
    for m := low(m) to high(m) do // register all known modes and sizes
      for b := 0 to 2 do
      begin
        bits := 128 + b * 64;
        n := AesAlgoNameEncode(m, bits);
        TCryptAesInternal.Create(n, m, bits, TAesFast); // fastest
        TCryptAesInternal.Create(n + '-int', m, bits, TAesInternal); // internal
      end;
  finally
   GlobalUnlock;
 end;
end;


{ Actual High-Level Factory Functions }

var // cache the last used algorithm for each factory function
  LastRnd, LastHasher, LastSigner, LastCipher,
  LastAsym, LastCert, LastStore: TCryptAlgo;

function Rnd(const name: RawUtf8): TCryptRandom;
begin
  result := TCryptRandom.InternalFind(name, LastRnd);
end;

function Hasher(const name: RawUtf8): TCryptHasher;
begin
  result := TCryptHasher.InternalFind(name, LastHasher);
end;

function Hash(const name: RawUtf8): ICryptHash;
var
  h: TCryptHasher;
begin
  h := Hasher(name);
  if h = nil then
    result := nil
  else
    result := h.New;
end;

function Signer(const name: RawUtf8): TCryptSigner;
begin
  result := TCryptSigner.InternalFind(name, LastSigner);
end;

function Sign(key: pointer; keylen: PtrInt; const name: RawUtf8): ICryptHash;
var
  s: TCryptSigner;
begin
  s := Signer(name);
  if s = nil then
    result := nil
  else
    result := s.New(key, keylen);
end;

function Sign(const name, secret, salt: RawUtf8; rounds: integer): ICryptHash;
var
  s: TCryptSigner;
begin
  s := Signer(name);
  if s = nil then
    result := nil
  else
    result := s.NewPbkdf2(secret, salt, rounds);
end;

function Sign(const paramsjson, defaultsalt: RawUtf8): ICryptHash;
var
  s: TSynSigner;
  key: THash512Rec;
begin
  s.Pbkdf2(paramsjson, key, defaultsalt, saSha256);
  result := TCryptSignInternal.Create(s, key);
end;

function CipherAlgo(const name: RawUtf8): TCryptCipherAlgo;
begin
  result := TCryptCipherAlgo.InternalFind(name, LastCipher);
end;

function Cipher(const name: RawUtf8; key: pointer; encrypt: boolean): ICryptCipher;
var
  c: TCryptCipherAlgo;
begin
  c := CipherAlgo(name);
  if c = nil then
    result := nil
  else
    result := c.New(key, encrypt);
end;

function Encrypt(const name: RawUtf8; key: pointer): ICryptCipher;
begin
  result := Cipher(name, key, {encrypt=}true);
end;

function Decrypt(const name: RawUtf8; key: pointer): ICryptCipher;
begin
  result := Cipher(name, key, {encrypt=}false);
end;

function Cipher(const name, hash, secret, salt: RawUtf8; rounds: integer;
  encrypt: boolean): ICryptCipher;
var
  c: TCryptCipherAlgo;
begin
  c := CipherAlgo(name);
  if c = nil then
    result := nil
  else
    result := c.New(hash, secret, salt, rounds, encrypt);
end;

function Encrypt(const name, hash, secret, salt: RawUtf8; rounds: integer): ICryptCipher;
begin
  result := Cipher(name, hash, secret, salt, rounds, {encrypt=}true);
end;

function Decrypt(const name, hash, secret, salt: RawUtf8; rounds: integer): ICryptCipher;
begin
  result := Cipher(name, hash, secret, salt, rounds, {encrypt=}false);
end;

function Asym(const name: RawUtf8): TCryptAsym;
begin
  result := TCryptAsym.InternalFind(name, LastAsym);
end;

function CertAlgo(const name: RawUtf8): TCryptCertAlgo;
begin
  result := TCryptCertAlgo.InternalFind(name, LastCert);
end;

function Cert(const name: RawUtf8): ICryptCert;
var
  c: TCryptCertAlgo;
begin
  c := CertAlgo(name);
  if c = nil then
    result := nil
  else
    result := c.New;
end;

function StoreAlgo(const name: RawUtf8): TCryptStoreAlgo;
begin
  result := TCryptStoreAlgo.InternalFind(name, LastStore);
end;

function Store(const name: RawUtf8): ICryptStore;
var
  c: TCryptStoreAlgo;
begin
  c := StoreAlgo(name);
  if c = nil then
    result := nil
  else
    result := c.New;
end;


{ ************************** Minimal PEM/DER Encoding/Decoding }

function DerToPem(der: pointer; len: PtrInt; kind: TPemKind): TCertPem;
begin
  result := BinToBase64Line(der, len, PEM_BEGIN[kind], PEM_END[kind]);
end;

function DerToPem(const der: TCertDer; kind: TPemKind): TCertPem;
begin
  result := DerToPem(pointer(der), length(der), kind);
end;

function IsPem(const pem: RawUtf8): boolean;
var
  i: PtrUInt;
begin
  i := PosEx('-----BEGIN', pem); // ignore e.g. any trailing comments
  result := (i <> 0) and
            (PosEx('-----END', pem, i + 10) <> 0);
end;

function IsPemEncrypted(const pem: TCertPem): boolean;
begin
  result := IsPem(pem) and
            (PosEx('ENCRYPTED', pem) <> 0);
{ e.g.
    -----BEGIN RSA PRIVATE KEY-----
    Proc-Type: 4,ENCRYPTED
  or
    -----BEGIN ENCRYPTED PRIVATE KEY-----  }
end;

function PemHeader(lab: PUtf8Char): TPemKind;
begin
  for result := succ(low(result)) to high(result) do
    if IdemPropNameUSameLenNotNull(@PEM_BEGIN[result][12], lab, 10) then
      exit;
  result := low(result);
end;

function GotoMarker(P: PUtf8Char): PUtf8Char;
begin
  result := nil;
  repeat
    if P = nil then
      exit;
    if PCardinal(P)^ = $2d2d2d2d then
      break;
    P := GotoNextLine(P);
  until false;
  result := P;
end;

function ParsePem(var P: PUtf8Char; Kind: PPemKind; var Len: PtrInt;
  ExcludeMarkers: boolean): PUtf8Char;
var
  start: PUtf8Char;
begin
  result := nil;
  start := GotoMarker(P);
  if start = nil then
    exit;
  if Kind <> nil then
    Kind^ := PemHeader(start + 11);   // label just after '-----BEGIN '
  P := GotoMarker(GotoNextLine(start));
  if P = nil  then
    exit;  // no trailing '-----END '
  if ExcludeMarkers then
    start := GotoNextLine(start)
  else
    P := GotoNextLine(P);
  result := start;
  Len := P - start;
  if ExcludeMarkers then
    P := GotoNextLine(P);
end;

function Base64IgnoreLineFeeds(s, d: PUtf8Char): PUtf8Char;
var
  c: AnsiChar;
begin
  repeat
    c := s^;
    inc(s);
    if c <= ' ' then // do not append any space or line feed
      continue
    else if c = '-' then
      break; // no need to check #0 since -----END... will eventually appear
    d^ := c;
    inc(d); // keep only Base64 chars
  until false;
  result := d;
end;

function PemToDer(const pem: TCertPem; kind: PPemKind): TCertDer;
var
  P: PUtf8Char;
  len: PtrInt;
  base64: TSynTempBuffer; // pem is small, so a 4KB temp buffer is fine enough
begin
  P := pointer(pem);
  P := ParsePem(P, kind, len, {excludemarkers=}true);
  if P <> nil then
  begin
    base64.Init(len);
    len := Base64IgnoreLineFeeds(P, base64.buf) - base64.buf;
    result := Base64ToBinSafe(base64.buf, len);
    base64.Done;
    if result <> '' then
      exit;
  end;
  result := pem;
end;

function NextPem(var P: PUtf8Char; Kind: PPemKind): TCertPem;
var
  len: PtrInt;
  pem: PUtf8Char;
begin
  pem := ParsePem(P, Kind, len, {excludemarkers=}false);
  if pem = nil then
    result := ''
  else
    FastSetString(result, pem, len);
end;

const
  DER_INTEGER  = #$02;

function DerAppend(P: PAnsiChar; buf: PByteArray; buflen: PtrUInt): PAnsiChar;
var
  pos, prefix: PtrUInt;
begin
  pos := 0;
  while buf[pos] = 0 do
    // ignore trailing zeros
    inc(pos);
  dec(buflen, pos);
  prefix := buf[pos] shr 7; // two's complement?
  P[0] := DER_INTEGER;
  P[1] := AnsiChar(buflen + prefix);
  P[2] := #$00; // prepend 0 for negative number (if prefix=1)
  inc(P, 2 + prefix);
  MoveFast(buf[pos], P^, buflen);
  result := P + buflen;
end;

function DerParse(P: PAnsiChar; buf: PByteArray; buflen: PtrInt): PAnsiChar;
var
  pos: PtrUInt;
begin
  result := nil;
  FillZero(buf^, buflen);
  if (P = nil) or
     (P[0] <> DER_INTEGER) then
    exit;
  pos := buflen - ord(P[1]);
  inc(P, 2);
  if P^ = #0 then
  begin
    inc(P); // negative number appended
    inc(pos);
  end;
  dec(buflen, pos);
  if buflen < 0 then
    exit; // avoid buffer overflow
  MoveFast(P^, buf[pos], buflen);
  result := P + buflen;
end;


procedure InitializeUnit;
begin
  Rtti.RegisterType(TypeInfo(TSignAlgo));
  Rtti.RegisterFromText(TypeInfo(TSynSignerParams),
    'algo:TSignAlgo secret,salt:RawUtf8 rounds:integer');
  // Rnd/Sign/Hash/Cipher/Asym/Cert/Store are registered in GlobalCryptAlgoInit
end;

procedure FinalizeUnit;
begin
end;


initialization
  InitializeUnit;

finalization
  FinalizeUnit;
  
end.

