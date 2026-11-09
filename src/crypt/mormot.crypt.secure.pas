/// Framework Core Authentication and Security Features
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.crypt.secure;

{
  *****************************************************************************

   Authentication and Security types shared by all framework units
    - Password-Safe and TSynConnectionDefinition Classes
    - Reusable Authentication Classes
    - High-Level TSynSigner/TSynHasher Multi-Algorithm Wrappers
    - HMAC/PBKDF2 MCF SCRAM SCRAM-MCF TOTP High-Level Protocols
    - Client and Server HTTP Access Authentication
    - 64-bit TSynUniqueIdentifier and its efficient Generator
    - IProtocol Safe Communication with Unilateral or Mutual Authentication
    - TBinaryCookieGenerator Simple Cookie Generator
    - Minimal PEM/DER Encoding/Decoding
    - TSynMustache Cryptographic Expression Helpers

   Uses optimized mormot.crypt.core.pas for its actual cryptographic process.

  *****************************************************************************

   Legal Notice: as stated by our LICENSE.md terms, make sure that you comply
   to any restriction about the use of cryptographic software in your country.
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.os.security,
  mormot.core.rtti,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.buffers,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.search,
  mormot.crypt.core;


{ ***************** Password-Safe and TSynConnectionDefinition Classes }

const
  /// set this to TObjectWithPassword.Key to disable password obfuscation
  // - i.e. trigger PasswordPlain = Password
  OBJECTPASSWORD_PLAIN = cardinal(-1);

type
  /// abstract class allowing safe storage of a password in a published property
  // - the associated Password, e.g. for storage or transmission encryption
  // will be persisted encrypted with a private key (which can be customized)
  // - if default simple symmetric encryption is not enough, it will also
  // read passwords strongly obfuscated for a given user using
  // mormot.crypt.core.pas' CryptDataForCurrentUser()
  // - a published property should be defined as such in inherited class:
  // ! property PasswordPropertyName: RawUtf8 read fPassword write fPassword;
  // - use the PasswordPlain property to access to its uncyphered value
  TObjectWithPassword = class(TSynPersistent)
  protected
    fPassWord: SpiUtf8;
    fKey: cardinal;
    procedure XorKey(var Value: RawByteString);
    function GetPasswordPlain: SpiUtf8;
    procedure SetPasswordPlain(const Value: SpiUtf8);
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
    // - may trigger a ESynCrypto if the password was stored using hardened
    // CryptDataForCurrentUser, and the current user doesn't match the
    // expected user stored in the field
    class function ComputePlainPassword(const CypheredPassword: SpiUtf8;
      CustomKey: cardinal = 0; const AppSecret: RawUtf8 = ''): SpiUtf8;
    /// append the password encoded via CryptDataForCurrentUser()
    // - would store it as 'username:passwordbase64' CSV values
    // - each Executable.User would have its own encrypted value: so we store
    // each username with its associated encrypted password
    // - you could remove the current user password by setting Value = ''
    // - you can use this method at runtime to safely obfucate a password in
    // memory using a local private key and Windows DPAPI for the current user
    procedure SetPassWordPlainCurrentUser(const Value: SpiUtf8;
      AppSecret: RawUtf8 = '');
    /// getter for PasswordPlain property to eventually call FillZero(Value)
    procedure GetPasswordSafe(var Value: SpiUtf8; const AppSecret: RawUtf8 = '');
    /// the private key used to cypher the password storage on serialization
    // - application can override the default 0 value at runtime
    // - set OBJECTPASSWORD_PLAIN would disable obfuscation
    property Key: cardinal
      read fKey write fKey;
    /// access to the associated unencrypted Password value
    // - may trigger a ESynCrypto if the password was stored using hardened
    // CryptDataForCurrentUser, and the current user doesn't match the
    // expected user stored in the field
    // - equals fPassword field if Key is set to OBJECTPASSWORD_PLAIN
    property PasswordPlain: SpiUtf8
      read GetPassWordPlain write SetPassWordPlain;
  end;

  {$ifndef PUREMORMOT2}
  TSynPersistentWithPassword = TObjectWithPassword;
  {$endif PUREMORMOT2}

type
  /// could be used to store a credential pair, as user name and password
  // - password will be stored with TObjectWithPassword encryption
  TSynUserPassword = class(TObjectWithPassword)
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
  // defined by TObjectWithPassword
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
  TSynConnectionDefinition = class(TObjectWithPassword)
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
    /// the associated database name (if any), or URI-encoded additional options
    // - e.g. TRestHttpClientGeneric.DefinitionTo() URI-encoded HTTP parameters
    property DatabaseName: RawUtf8
      read fDatabaseName write fDatabaseName;
    /// the associated User Identifier (if any)
    property User: RawUtf8
      read fUser write fUser;
    /// the associated Password, e.g. for storage or transmission encryption
    // - will be persisted encrypted with a private key
    // - use the PasswordPlain property to access to its uncyphered value
    property Password: SpiUtf8
      read fPassword write fPassword;
  end;


{ ***************** Reusable Authentication Classes }

type
  /// class-reference type (metaclass) of an authentication class
  TSynAuthenticationClass = class of TSynAuthenticationAbstract;

  /// abstract authentication class, implementing safe token/challenge security
  // and a list of active sessions
  // - do not use this class, but plain TSynAuthentication
  TSynAuthenticationAbstract = class
  protected
    fSafe: TOSLock;
    fSessions: TIntegerDynArray;
    fSessionsCount: integer;
    fSessionGenerator: integer;
    fTokenSeed: Int64;
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
    // - this abstract method will raise ESynCrypto: inherited classes should
    // implement them as expected
    procedure AuthenticateUser(const aName, aPassword: RawUtf8); virtual;
    /// unregister one credential for a given user
    // - this abstract method will raise ESynCrypto: inherited classes should
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
  /// optimized thread-safe storage of a list of IP v4 addresses
  // - can be used e.g. as white-list or black-list of clients
  // - will maintain internally a sorted list of 32-bit integers for fast lookup
  // - with optional binary persistence
  // - as used by TRestServer.BanIP/JwtForUnauthenticatedRequestWhiteIP
  // - see also more efficient and lower level THttpAcceptBan in mormot.net.http
  TIPBan = class(TObjectStore)
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
  // - bits 0..14 map a 15-bit increasing counter (collision-free)
  // - bits 15..30 map a 16-bit process identifier
  // - bits 31..63 map a 33-bit UTC time, encoded as seconds since Unix epoch
  // - may be used on client side for something similar to a MongoDB ObjectID,
  // but compatible with TOrm.ID: TID properties
  TSynUniqueIdentifier = type TID;

  /// 16-bit unique process identifier, used to compute TSynUniqueIdentifier
  // - each TSynUniqueIdentifierGenerator instance is expected to have
  // its own unique process identifier, stored as a 16-bit integer 0..65535 value
  // - when used with TSynUnique53, should be kept in [0..255] range
  TSynUniqueIdentifierProcess = type word;

  /// 53-bit integer unique identifier, as computed by TSynUniqueIdentifierGenerator
  // - could be used as JavaScript-compatible TID value e.g. for TOrm.ID
  // - bits 0..14 map a 15-bit increasing counter (collision-free)
  // - bits 15..22 map a 8-bit process identifier (0..255)
  // - bits 23..53 map a 31-bit UTC time, encoded as seconds since 1/1/2025,
  // therefore valid until 2093 (when I hope we will be done with JavaScript)
  // or until 2171 if we use the [-2^53+1 .. 0] range of negative numbers -
  // but BigInt would be usable at that time for sure for TSynUniqueIdentifier
  TSynUnique53 = type Int53;

  /// map 64-bit integer unique identifier internal memory structure
  // - as stored in TSynUniqueIdentifier = TID = Int64 values, and computed by
  // TSynUniqueIdentifierGenerator
  // - bits 0..14 map a 15-bit increasing counter (collision-free)
  // - bits 15..30 map a 16-bit process identifier
  // - bits 31..63 map a 33-bit UTC time, encoded as seconds since Unix epoch
  {$ifdef USERECORDWITHMETHODS}
  TSynUniqueIdentifierBits = record
  {$else}
  TSynUniqueIdentifierBits = object
  {$endif USERECORDWITHMETHODS}
  private
    function GetJavaScriptID: TSynUnique53;
    procedure SetJavaScriptID(const aJavaScriptID: TSynUnique53);
  public
    /// the actual 64-bit storage value
    // - in practice, only first 63 bits are used
    Value: TSynUniqueIdentifier;
    /// extract the 15-bit counter (0..32767), starting with a random value
    function Counter: word;
      {$ifdef HASINLINE}inline;{$endif}
    /// extract the 16-bit unique process identifier
    // - as specified to TSynUniqueIdentifierGenerator constructor
    // - will be in range (0..255) when stored in a TSynUnique53 identifier
    function ProcessID: TSynUniqueIdentifierProcess;
      {$ifdef HASINLINE}inline;{$endif}
    /// extract the UTC generation timestamp as seconds since the Unix epoch
    // - time is expressed in Coordinated Universal Time (UTC), not local time
    // - it uses in fact an unsigned 33-bit resolution, so is "Year 2038"
    // bug-free and would overflow only in year 2242
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
    // !  "Value":3137644716930138113,"Hex":"2B8B273F00008001"}
    function AsVariant: variant;
      {$ifdef HASINLINE}inline;{$endif}
    /// convert this identifier to an explicit TDocVariant JSON object
    // - returns e.g.
    // ! {"Created":"2016-04-19T15:27:58","Identifier":1,"Counter":1,
    // ! "Value":3137644716930138113,"Hex":"2B8B273F00008001"}
    procedure ToVariant(out Result: variant);
    /// convert to/from a JavaScript-compatible 53-bit integer value
    // - would accept only ProcessID in (0..255) range, or raise ESynCrypto
    property JavaScriptID: TSynUnique53
      read GetJavaScriptID write SetJavaScriptID;
  end;

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
  TSynUniqueIdentifierGenerator = class(TObjectLightLock)
  protected
    fLastUnixCreateTime: cardinal;
    fIdentifier: TSynUniqueIdentifierProcess;
    fIdentifierShifted: cardinal;
    fLastCounter: cardinal;
    fComputedCount: Int64;
    fCollisions: cardinal;
    fCryptoCRC: cardinal;
    fCrypto: TBlock256; // only fCrypto[6..7] are used in practice
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
    /// persist the current state (counter and create time) into a single value
    function SaveTo: Int64;
    /// read the current state (counter and create time) from a SaveTo value
    procedure LoadFrom(aSaved: Int64);
    /// some 32-bit value, derivated from aSharedObfuscationKey as supplied
    // to the class constructor
    // - FromObfuscated and ToObfuscated methods will validate their hexadecimal
    // content with this value to secure the associated CRC
    // - may be used e.g. as system-depending salt
    property CryptoCRC: cardinal
      read fCryptoCRC;
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
    property LastUnixCreateTime: cardinal
      read fLastUnixCreateTime;
    /// low-level access to the last generated counter
    property LastCounter: cardinal
      read fLastCounter;
  end;

  /// hold a dynamic array of TSynUniqueIdentifierGenerator instances
  TSynUniqueIdentifierGenerators = array of TSynUniqueIdentifierGenerator;



{ **************** High-Level TSynSigner/TSynHasher Multi-Algorithm Wrappers }

{ implemented in this unit and not in mormot.crypt.core, since TSynSignerParams
  expects JSON support, which requires mormot.core.json }

type
  /// algorithms available for TSynHasher wrapper and HashFile/HashFull
  // - hfSHA256_128 and hfSHA256_160 compute SHA-256 and return the leftmost
  // 128 or 160 bits as non-standard convenience algorithms for shorter digests,
  // offering a safer alternative to MD5 and SHA-1 while benefiting from SHA
  // hardware acceleration on newer Intel/AMD or ARM processors
  THashAlgo = (
    hfMD5,
    hfSHA1,
    hfSHA256,
    hfSHA384,
    hfSHA512,
    hfSHA512_256,
    hfSHA3_256,
    hfSHA3_512,
    hfSHA224,
    hfSHA3_224,
    hfSHA3_384,
    hfShake128,
    hfShake256,
    hfSHA256_128,
    hfSHA256_160);
  /// a pointer to one of our hash algorithms
  PHashAlgo = ^THashAlgo;

  /// set of algorithms available for TSynHasher and HashFile/HashFull
  THashAlgos = set of THashAlgo;

  /// store a hash value and its algorithm, e.g. for THttpPeerCacheMessage.Hash
  // - we store and compare in our implementation the algorithm in addition to
  // the hash, to avoid any potential attack about (unlikely) hash collisions
  // between algorithms, and allow any change of algo restrictions in the future
  THashDigest = packed record
    /// the algorithm used for hashing the input into Bin
    Algo: THashAlgo;
    /// up to 512-bit of raw binary hash, according to HASH_SIZE[Algo]
    Bin: THash512Rec;
  end;
  /// a pointer to one hash value and its algorithm
  PHashDigest = ^THashDigest;
  /// store a dynamic array of hash value and its algorithm
  THashDigests = array of THashDigest;

  /// convenient multi-algorithm hashing wrapper
  // - as used e.g. by HashFile/HashFull functions
  // - we defined a record instead of a class, to allow stack allocation and
  // thread-safe reuse of one initialized instance: copying the (414 bytes of)
  // record content will copy the whole current hashing state
  {$ifdef USERECORDWITHMETHODS}
  TSynHasher = record
  {$else}
  TSynHasher = object
  {$endif USERECORDWITHMETHODS}
  private
    ctxt: TSha3;      // enough space for all algorithms
    fAlgo: THashAlgo; // ctxt is better aligned if put first
    procedure CopyTo(out aHasher: TSynHasher); {$ifdef FPC} inline; {$endif}
    procedure Clear;                           {$ifdef FPC} inline; {$endif}
  public
    /// initialize the internal hashing structure for a specific algorithm
    // - returns false on unknown/unsupported algorithm
    function Init(aAlgo: THashAlgo): boolean;
    /// hash the supplied memory buffer
    procedure Update(aBuffer: pointer; aLen: integer); overload;
    /// hash the supplied memory buffer with uppercase ASCII conversion
    procedure UpdateUpper(aBuffer: pointer; aLen: PtrInt);
    /// hash the supplied string content
    procedure Update(const aBuffer: RawByteString); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// hash the supplied strings content
    procedure Update(const aBuffer: array of RawByteString); overload;
    /// hash 32-bit encoded integer as big endian
    procedure UpdateBigEndian(aValue: cardinal);
    /// returns the resulting hash as lowercase hexadecimal string
    procedure Final(var aResult: RawUtf8); overload;
    /// set the resulting hash into a binary buffer, and the size as result
    function Final(out aDigest: THash512Rec; aNoInit: boolean = false): integer; overload;
    /// returns the resulting hash as raw binary string, with optional replication
    procedure FinalBin(var aResult: RawByteString; aLen: PtrInt = 0);
    /// one-step hash computation of a buffer as lowercase hexadecimal string
    function Full(aAlgo: THashAlgo; aBuffer: pointer; aLen: integer): RawUtf8; overload;
    /// one-step hash computation of a buffer as lowercase hexadecimal string
    function Full(aAlgo: THashAlgo; const aBuffer: RawByteString): RawUtf8; overload;
    /// one-step hash computation of several buffers as lowercase hexadecimal string
    procedure Full(aAlgo: THashAlgo; const aBuffer: array of RawByteString;
      var aResult: RawUtf8); overload;
    /// one-step hash computation of a buffer as a binary buffer
    // - returns the written aDigest size in bytes
    function Full(aAlgo: THashAlgo; aBuffer: pointer; aLen: integer;
      out aDigest: THash512Rec): integer; overload;
    /// one-step hash computation of several buffers as a binary buffer
    // - returns the written aDigest size in bytes
    function Full(aAlgo: THashAlgo; const aBuffer: array of RawByteString;
      out aDigest: THash512Rec): integer; overload;
    /// fill a buffer with the MGF1 seed deriviation, following RFC 2437
    // - a Mask Generation Function expands aSeed/aSeedLen into aDestLen buffer
    function Mgf1(aAlgo: THashAlgo; aSeed: pointer; aSeedLen, aDestLen: PtrUInt): RawByteString;
    /// compute the Unix crypt hash of a given password as '$algo$salt$checksum'
    // - currently implements safe SHA-256-CRYPT and SHA-512-CRYPT with hfSHA256
    // and hfSHA512, as defined in https://www.akkadia.org/drepper/SHA-crypt.txt
    // - deprecated MD5-CRYPT can also be generated (and verified) with hfMD5
    // - any other algorithm is unsupported, and will fail and return ''
    // - see ModularCryptVerify() for the associated verification function
    function UnixCryptHash(aAlgo: THashAlgo; const aPassword: RawUtf8;
      aRounds: cardinal = 0; aSaltSize: cardinal = 8;
      const aSalt: RawUtf8 = ''; aHashPos: PInteger = nil): RawUtf8;
    /// returns the number of bytes of the hash of the current Algo
    function HashSize: integer;
    /// the hash algorithm used by this instance
    property Algo: THashAlgo
      read fAlgo;
  end;

  /// TStreamRedirect with TSynHasher cryptographic hashing
  // - do not use this abstract class but inherited types with overriden GetAlgo
  TStreamRedirectSynHasher = class(TStreamRedirect)
  protected
    fHash, fHashAppend: TSynHasher;
    procedure DoHash(data: pointer; len: integer); override;
    procedure AfterAppend; override;
    procedure ResetHash; override;
  public
    // proper implement TStreamRedirect (abstract) virtual methods
    constructor Create(aDestination: TStream; aRead: boolean = false); override;
    function GetHash: RawUtf8; override;
    class function GetHashFileExt: RawUtf8; override;
    /// return the current state of the hash as its raw binary and algorithm
    function GetHashDigest(out Digest: THashDigest): boolean; overload;
    /// will decode an hexadecimal hash into its raw binary and algorithm
    class function GetHashDigest(const HexaHash: RawUtf8;
      out Digest: THashDigest): boolean; overload;
    /// inherited classes will properly return the hash algorithm
    class function GetAlgo: THashAlgo; virtual; abstract;
  end;

  /// meta-class of TStreamRedirectSynHasher
  // - to access e.g. GetAlgo/GetHashFileExt class methods
  TStreamRedirectSynHasherClass = class of TStreamRedirectSynHasher;

  /// TStreamRedirect with MD5 cryptographic hashing
  TStreamRedirectMd5 = class(TStreamRedirectSynHasher)
  public
    class function GetAlgo: THashAlgo; override;
  end;

  /// TStreamRedirect with SHA-1 cryptographic hashing
  TStreamRedirectSha1 = class(TStreamRedirectSynHasher)
  public
    class function GetAlgo: THashAlgo; override;
  end;

  /// TStreamRedirect with SHA-224 cryptographic hashing
  TStreamRedirectSha224 = class(TStreamRedirectSynHasher)
  public
    class function GetAlgo: THashAlgo; override;
  end;

  /// TStreamRedirect with SHA-256/128 cryptographic hashing
  TStreamRedirectSha256_128 = class(TStreamRedirectSynHasher)
  public
    class function GetAlgo: THashAlgo; override;
  end;

  /// TStreamRedirect with SHA-256/160 cryptographic hashing
  TStreamRedirectSha256_160 = class(TStreamRedirectSynHasher)
  public
    class function GetAlgo: THashAlgo; override;
  end;

  /// TStreamRedirect with SHA-256 cryptographic hashing
  TStreamRedirectSha256 = class(TStreamRedirectSynHasher)
  public
    class function GetAlgo: THashAlgo; override;
  end;

  /// TStreamRedirect with SHA-384 cryptographic hashing
  TStreamRedirectSha384 = class(TStreamRedirectSynHasher)
  public
    class function GetAlgo: THashAlgo; override;
  end;

  /// TStreamRedirect with SHA-512 cryptographic hashing
  TStreamRedirectSha512 = class(TStreamRedirectSynHasher)
  public
    class function GetAlgo: THashAlgo; override;
  end;

  /// TStreamRedirect with SHA-512/256 cryptographic hashing
  TStreamRedirectSha512_256 = class(TStreamRedirectSynHasher)
  public
    class function GetAlgo: THashAlgo; override;
  end;

  /// TStreamRedirect with SHA-3-224 cryptographic hashing
  TStreamRedirectSha3_224 = class(TStreamRedirectSynHasher)
  public
    class function GetAlgo: THashAlgo; override;
  end;

  /// TStreamRedirect with SHA-3-256 cryptographic hashing
  TStreamRedirectSha3_256 = class(TStreamRedirectSynHasher)
  public
    class function GetAlgo: THashAlgo; override;
  end;

  /// TStreamRedirect with SHA-3-384 cryptographic hashing
  TStreamRedirectSha3_384 = class(TStreamRedirectSynHasher)
  public
    class function GetAlgo: THashAlgo; override;
  end;

  /// TStreamRedirect with SHA-3-512 cryptographic hashing
  TStreamRedirectSha3_512 = class(TStreamRedirectSynHasher)
  public
    class function GetAlgo: THashAlgo; override;
  end;

  /// TStreamRedirect with SHA-3 Shake128 cryptographic hashing
  TStreamRedirectShake128 = class(TStreamRedirectSynHasher)
  public
    class function GetAlgo: THashAlgo; override;
  end;

  /// TStreamRedirect with SHA-3 Shake256 cryptographic hashing
  TStreamRedirectShake256 = class(TStreamRedirectSynHasher)
  public
    class function GetAlgo: THashAlgo; override;
  end;

const
  /// convert a THashAlgo into a TStreamRedirectSynHasher class
  HASH_STREAMREDIRECT: array[THashAlgo] of TStreamRedirectClass = (
    TStreamRedirectMd5,         // hfMD5
    TStreamRedirectSha1,        // hfSHA1
    TStreamRedirectSha256,      // hfSHA256
    TStreamRedirectSha384,      // hfSHA384
    TStreamRedirectSha512,      // hfSHA512
    TStreamRedirectSha512_256,  // hfSHA512_256
    TStreamRedirectSha3_256,    // hfSHA3_256
    TStreamRedirectSha3_512,    // hfSHA3_512
    TStreamRedirectSha224,      // hfSHA224
    TStreamRedirectSha3_224,    // hfSHA3_224
    TStreamRedirectSha3_384,    // hfSHA3_384
    TStreamRedirectShake128,    // hfShake128
    TStreamRedirectShake256,    // hfShake256
    TStreamRedirectSha256_128,  // hfSHA256_128
    TStreamRedirectSha256_160); // hfSHA256_160

  /// the NIST/FIPS standard text of a THashAlgo (in uppercase characters)
  HASH_TXT: array[THashAlgo] of RawUtf8 = (
    'MD5', 'SHA-1', 'SHA-256', 'SHA-384', 'SHA-512', 'SHA-512/256',
    'SHA3-256', 'SHA3-512', 'SHA-224', 'SHA3-224', 'SHA3-384',
    'SHAKE128', 'SHAKE256', 'SHA-256/128', 'SHA-256/160');
  /// the standard text of a THashAlgo (in lowercase characters)
  HASH_TXT_LOWER: array[THashAlgo] of RawUtf8 = (
    'md5', 'sha-1', 'sha-256', 'sha-384', 'sha-512', 'sha-512/256',
    'sha3-256', 'sha3-512', 'sha-224', 'sha3-224', 'sha3-384',
    'shake128', 'shake256', 'sha-256/128', 'sha-256/160');

type
  /// the HMAC/SHA-1 HMAC/SHA-2 and SHA-3 algorithms known by TSynSigner
  // - HMAC/SHA-1 is considered unsafe, HMAC/SHA-2 are well proven, and
  // SHA-3 is newer and strong, including HMAC, so a good candidate for safety
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
    saSha3S256,
    saSha224);
  PSignAlgo = ^TSignAlgo;

  /// the algorithms known by ModularCryptIdentify/ModularCryptVerify
  // - mcfMd5Crypt, mcfSha256Crypt and mcfSha512Crypt are handled by
  // TSynHasher.UnixCryptVerify() and match $1$ $5$ $6$ common Unix Hashes
  // - mcfPbkdf2Sha1, mcfPbkdf2Sha256 and mcfPbkdf2Sha512 match Python's PassLib
  // specific-but-useful '$pbkdf2-{digest}${rounds}${salt}${checksum}' format
  // - mcfPbkdf2Sha3 is our own SHA3-512 hash algorithm (with no HMAC) extension
  // - mcfBCrypt is the BCrypt hashing algorithm, as developed for BSD systems -
  // please include the mormot.crypt.other.pas unit to your project to enable it
  // - mcfBCryptSha256 will first hash the password with HMAC-SHA-256 to support
  // any password, e.g. > than 72 bytes (following passlib.hash.bcrypt_sha256)
  // - mcfSCrypt is the SCrypt memory-intensive hashing algorithm, implemented in
  // mormot.crypt.other.pas or in mormot.crypt.openssl.pas via global SCrypt()
  // - in practice: use safest mcfBCryptSha256 or mcfSCrypt if possible
  TModularCryptFormat = (
    mcfInvalid,
    mcfUnknown,
    mcfMd5Crypt,
    mcfSha256Crypt,
    mcfSha512Crypt,
    mcfPbkdf2Sha1,
    mcfPbkdf2Sha256,
    mcfPbkdf2Sha512,
    mcfPbkdf2Sha3,
    mcfBCrypt,
    mcfBCryptSha256,
    mcfSCrypt);
  PModularCryptFormat = ^TModularCryptFormat;
  /// allow to specify several ModularCryptIdentify/ModularCryptVerify algorithms
  TModularCryptFormats = set of TModularCryptFormat;

const
  SIGN_SIZE: array[TSignAlgo] of byte = (
    20, 32, 48, 64, 28, 32, 48, 64, 32, 64, 28);
  /// the NIST/FIPS standard text of each TSignAlgo
  SIGNER_TXT: array[TSignAlgo] of RawUtf8 = (
    'SHA-1',    'SHA-256',  'SHA-384',  'SHA-512',  'SHA3-224', 'SHA3-256',
    'SHA3-384', 'SHA3-512', 'SHAKE128', 'SHAKE256', 'SHA-224');
  /// alternative TSignAlgo identifiers as used at API level (e.g. OpenSSL)
  SIGNER_API: array[TSignAlgo] of RawUtf8 = (
    'SHA1',     'SHA256',   'SHA384',   'SHA512',   'SHA3-224', 'SHA3-256',
    'SHA3-384', 'SHA3-512', 'SHAKE128', 'SHAKE256', 'SHA224');
  SIGNER_SHA3 = [saSha3224 .. saSha3S256];
  SIGNER_DEFAULT_SALT = 'I6sWioAidNnhXO9BK';
  SIGNER_DEFAULT_ALGO = saSha3S128;
  /// which ModularCryptIdentify/ModularCryptVerify() results are correct
  mcfValid = [mcfMd5Crypt .. high(TModularCryptFormat)];
  /// the maximum number of PBKDF2 rounds which may trigger a DoS attack
  // - 5 millions = 1-5 seconds with SHA-NI is noticeable to be painful
  MAX_PBKDF2_ROUNDS = 5000000;
var
  /// default number of rounds for PBKDF2 "Modular Crypt" functions
  // - numbers adjusted on 2025, and align with OWASP Password Storage Cheat
  // Sheet, NIST SP 800-63B and RFC 8018, and are higher than "$pbkdf2" passlib
  // - typical values on my Core i5-13500 PC with SHA-NI are Pbkdf2Sha1=68.28ms
  // Pbkdf2Sha256=35.89ms Pbkdf2Sha512=110.55ms and Pbkdf2Sha3=112.58ms
  // - made as a global variable, since you can adjust/tune those values for your
  // own purpose, since they are published with the MCF prefix text itself
  MCF_ROUNDS: array[mcfMd5Crypt .. mcfPbkdf2Sha3] of cardinal = (
    1000, 535000, 535000, 600000, 310000, 210000, 200000);

type
  /// JSON-serializable object as used by TSynSigner.Pbkdf2() overloaded methods
  // - default value for unspecified parameters will be SIGNER_DEFAULT_ALGO
  // (SHAKE_128) with  rounds=1000 and a fixed salt
  // - a typical (extended) JSON to supply to TSynSigner.Pbkdf2() may be
  // ${algo:"sha-512",secret:"StrongPassword",salt:"FixedSalt",rounds:10000}
  TSynSignerParams = packed record
    algo: TSignAlgo;
    secret, salt: RawUtf8;
    rounds: integer;
  end;

  /// a generic wrapper object to handle digital HMAC of any SHA algorithm
  // - used e.g. to implement TJwtSynSignerAbstract
  {$ifdef USERECORDWITHMETHODS}
  TSynSigner = record
  {$else}
  TSynSigner = object
  {$endif USERECORDWITHMETHODS}
  private
    fAlgo: TSignAlgo;
    fSignatureSize, fBlockMax, fBlockSize: byte;
    fHasher: TSynHasher;    // raw hash algorithm for the HMAC process
    fStep7data: TBlock1024; // pre-computed salt for Final() step
  public
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
      {$ifdef HASINLINE}inline;{$endif}
    /// process some message content supplied as string
    procedure Update(const aBuffer: RawByteString); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// hash 32-bit encoded integer as big endian
    procedure UpdateBigEndian(aValue: cardinal);
      {$ifdef HASINLINE}inline;{$endif}
    /// returns the computed digital signature as lowercase hexadecimal text
    function Final: RawUtf8; overload;
    /// returns the raw computed digital signature
    // - SignatureSize bytes will be written: use Signature.Lo/h0/b3/b accessors
    function Final(aSignature: PHash512Rec; aNoInit: boolean = false): integer; overload;
    /// one-step digital signature of a buffer as lowercase hexadecimal string
    function Full(aAlgo: TSignAlgo; const aSecret: RawUtf8;
      aBuffer: pointer; aLen: integer): RawUtf8; overload;
    /// one-step digital signature of a buffer with PBKDF2 derivation
    function Full(aAlgo: TSignAlgo; const aSecret, aSalt: RawUtf8;
      aSecretPbkdf2Round: integer; aBuffer: pointer; aLen: integer): RawUtf8; overload;
    /// one-step binary digital signature of a buffer with PBKDF2 derivation
    function Full(aAlgo: TSignAlgo; aSecret: pointer; aSecretLen: PtrInt;
      const aMessage: RawByteString; aHmac: PHash512Rec): integer; overload;
    /// one-step hash computation of a buffer as a binary buffer
    // - returns the written aDigest size in bytes
    function Hash(aAlgo: TSignAlgo; aBuffer: pointer; aLen: integer;
      out aDigest: THash512Rec): integer;
    /// convenient wrapper to perform PBKDF2 safe iterative key derivation
    function Pbkdf2(aAlgo: TSignAlgo; const aSecret, aSalt: RawUtf8;
      aSecretPbkdf2Round: integer; aDerivatedKey: PHash512Rec;
      aPartNumber: integer = 1): PtrInt; overload;
    /// convenient wrapper to perform PBKDF2 safe iterative key derivation
    procedure Pbkdf2(const aParams: TSynSignerParams;
      out aDerivatedKey: THash512Rec); overload;
    /// convenient wrapper to perform PBKDF2 safe iterative key derivation
    // - accept as input a TSynSignerParams serialized as JSON object e.g.
    // ${algo:"saSha512",secret:"StrongPassword",salt:"FixedSalt",rounds:10000}
    procedure Pbkdf2(aParamsJson: PUtf8Char; aParamsJsonLen: integer;
      out aDerivatedKey: THash512Rec;
      const aDefaultSalt: RawUtf8 = SIGNER_DEFAULT_SALT;
      aDefaultAlgo: TSignAlgo = SIGNER_DEFAULT_ALGO); overload;
    /// convenient wrapper to perform PBKDF2 safe iterative key derivation
    // - accept as input a TSynSignerParams serialized as JSON object e.g.
    // ${algo:"saSha512",secret:"StrongPassword",salt:"FixedSalt",rounds:10000}
    procedure Pbkdf2(const aParamsJson: RawUtf8;
      out aDerivatedKey: THash512Rec;
      const aDefaultSalt: RawUtf8 = SIGNER_DEFAULT_SALT;
      aDefaultAlgo: TSignAlgo = SIGNER_DEFAULT_ALGO); overload;
    /// fill a buffer with the PBKDF2 deriviation, following RFC 2898 5.2
    // - in respect to other Pbkdf2() methods, the length of the derived
    // key is unbounded and could be bigger than the TSignAlgo digest size
    function Pbkdf2(aAlgo: TSignAlgo; const aSecret, aSalt: RawUtf8;
      aSecretPbkdf2Round, aDestLen: PtrUInt): RawByteString; overload;
    /// compute the Modular Crypt hash of a given password as computed by passlib
    // pbkdf2.py - i.e. in '$pbkdf2-{digest}${rounds}${salt}${checksum}' format
    // - see ModularCryptVerify() for the associated verification function
    // - in addition to official passlib format, will include our '$pbkdf2-sha3$'
    function Pbkdf2ModularCrypt(aAlgo: TModularCryptFormat; const aPassword: RawUtf8;
      aRounds: cardinal = 0; aSaltSize: cardinal = 16;
      const aSalt: RawUtf8 = ''; aHashPos: PInteger = nil): RawUtf8;
    /// compute NIST SP800-108 KDF in counter mode (section 5.1)
    // - as used e.g. by RFC 8009 for Kerberos AES-CTS HMAC-SHA2 modes
    function KdfSP800(aAlgo: TSignAlgo; aDestLen: cardinal;
      const aKey, aLabel: RawByteString; const aContext: RawByteString = ''): RawByteString;
    /// prepare a TAes object with the key derivated via a Pbkdf2() call
    // - aDerivatedKey is defined as "var", since it will be zeroed after use
    procedure AssignTo(var aDerivatedKey: THash512Rec;
      out aAes: TAes; aEncrypt: boolean);
    /// fill the internal context with zeros, for security
    procedure Done;
    /// the size, in bytes, of the digital signature of this algorithm
    // - potential values are 20 (for SHA-1), 28, 32, 48 and 64 (for SHA-512)
    property SignatureSize: byte
      read fSignatureSize;
    /// the algorithm used for digitial signature
    property Algo: TSignAlgo
      read fAlgo;
  end;

  /// reference to a TSynSigner wrapper object
  PSynSigner = ^TSynSigner;

const
  /// map the size in bytes (16..64) of any THashAlgo digest
  // - note that some algorithms share the same size with other algos
  HASH_SIZE: array[THashAlgo] of byte = (
    SizeOf(TMd5Digest),    // 16 bytes for hfMD5
    SizeOf(TSHA1Digest),   // 20 bytes for hfSHA1
    SizeOf(TSHA256Digest), // 32 bytes for hfSHA256
    SizeOf(TSHA384Digest), // 48 bytes for hfSHA384
    SizeOf(TSHA512Digest), // 64 bytes for hfSHA512
    SizeOf(THash256),      // 32 bytes for hfSHA512_256
    SizeOf(THash256),      // 32 bytes for hfSHA3_256
    SizeOf(THash512),      // 64 bytes for hfSHA3_512
    SizeOf(THash224),      // 28 bytes for hfSHA224
    SizeOf(THash224),      // 28 bytes for hfSHA3_224
    SizeOf(THash384),      // 48 bytes for hfSHA3_384
    SizeOf(THash128),      // 16 bytes for hfShake128
    SizeOf(THash256),      // 32 bytes for hfShake256
    SizeOf(THash128),      // 16 bytes for hfSHA256_128
    SizeOf(THash160));     // 20 bytes for hfSHA256_160

  /// map the file extension text of any THashAlgo digest
  // - TextToHashAlgo() is able to recognize those values
  HASH_EXT: array[THashAlgo] of RawUtf8 = (
    '.md5',         // hfMD5
    '.sha1',        // hfSHA1
    '.sha256',      // hfSHA256
    '.sha384',      // hfSHA384
    '.sha512',      // hfSHA512
    '.sha512-256',  // hfSHA512_256
    '.sha3-256',    // hfSHA3_256
    '.sha3-512',    // hfSHA3_512
    '.sha224',      // hfSHA224
    '.sha3-224',    // hfSHA3_224
    '.sha3-384',    // hfSHA3_384
    '.shake128',    // hfShake128
    '.shake256',    // hfShake256
    '.sha256-128',  // hfSHA256_128
    '.sha256-160'); // hfSHA256_160

  /// convert a TSignAlgo / TSynSigner algorithm into a THashAlgo / TSynHasher
  SIGN_HASH: array[TSignAlgo] of THashAlgo = (
    hfSha1,     // saSha1
    hfSha256,   // saSha256
    hfSha384,   // saSha384
    hfSha512,   // saSha512
    hfSha3_224, // saSha3224
    hfSha3_256, // saSha3256
    hfSha3_384, // saSha3384
    hfSha3_512, // saSha3512
    hfShake128, // saSha3S128
    hfShake256, // saSha3S256
    hfSha224);  // saSha224

type
  /// the known 32-bit crc algorithms as returned by CryptCrc32()
  // - caAdler32 requires mormot.lib.z.pas to be included
  // - caDefault may be AesNiHash32(), therefore not persistable between
  // executions, since is randomly seeded at process startup
  // - some cryptographic-level hashes are truncated to 32-bit - caSha1/caSha256
  // could leverage Intel SHA HW opcodes to achieve good enough performance
  TCrc32Algo = (
    caCrc32c,
    caCrc32,
    caAdler32,
    caxxHash32,
    caFnv32,
    caDefault,
    caMd5,
    caSha1,
    caSha256);

/// returns the 32-bit crc function for a given algorithm
// - may return nil, e.g. for caAdler32 when mormot.lib.z is not loaded
// - caSha1/caSha256 have cryptographic level, with good performance on latest
// SHA-NI CPUs, but digest truncation to 32-bit won't make it secure any more
function CryptCrc32(algo: TCrc32Algo): THasher;

function ToText(algo: TSignAlgo): PShortString; overload;
function ToUtf8(algo: TSignAlgo): RawUtf8; overload; {$ifdef HASINLINE} inline; {$endif}
function ToText(algo: THashAlgo): PShortString; overload;
function ToUtf8(algo: THashAlgo): RawUtf8; overload; {$ifdef HASINLINE} inline; {$endif}
function ToText(algo: TCrc32Algo): PShortString; overload;
function ToText(const Digest: THashDigest): RawUtf8; overload; // 'hexhash' w/o algo
function ToText(fmt: TModularCryptFormat): PShortString; overload;

/// recognize a TSignAlgo from a text, e.g. 'SHAKE-128', 'saSha256' or 'SHA-3/256'
function TextToSignAlgo(const Text: RawUtf8; out Algo: TSignAlgo): boolean; overload;
function TextToSignAlgo(P: PUtf8Char; Len: PtrInt; out Algo: TSignAlgo): boolean; overload;

/// recognize a THashAlgo from a text, e.g. 'SHA1', 'hfSHA3_256' or 'SHA-512/256'
function TextToHashAlgo(const Text: RawUtf8; out Algo: THashAlgo): boolean; overload;
function TextToHashAlgo(P: PUtf8Char; Len: PtrInt; out Algo: THashAlgo): boolean; overload;

/// decode and recognize an hexadecimal hash from its size - but not SHA-3
function HashDetect(const Hash: RawUtf8; out Digest: THashDigest): boolean;

/// fast compare two hash digest and their associated algorithm
function HashDigestEqual(a, b: PPtrIntArray): boolean;

/// anti-forensic method for a hash digest
procedure FillZero(var Digest: THashDigest); overload;

/// compute the hexadecimal hash of any (big) file
// - using a temporary buffer of 1MB for the sequential reading
function HashFile(const aFileName: TFileName; aAlgo: THashAlgo): RawUtf8; overload;

/// compute one or several hexadecimal hash(es) of any (big) file
// - using a temporary buffer of 1MB for the sequential one-pass reading
// - returns the hash in THashAlgo type definition order in aAlgos set
function HashFileRaw(const aFileName: TFileName; aAlgos: THashAlgos;
  aFileSize: PInt64 = nil): TRawUtf8DynArray;

/// compute the hexadecimal hashe(s) of one file, as external .md5/.sha256/.. files
// - generate the text hash files in the very same folder
procedure HashFile(const aFileName: TFileName; aAlgos: THashAlgos); overload;

/// one-step hash computation of a buffer as lowercase hexadecimal string
function HashFull(aAlgo: THashAlgo; aBuffer: pointer; aLen: integer): RawUtf8; overload;

/// one-step hash computation of a buffer as lowercase hexadecimal string
function HashFull(aAlgo: THashAlgo; const aBuffer: RawByteString): RawUtf8; overload;

/// compute the MD5 checksum of a given file
// - this function maps the THashFile signature as defined in mormot.core.buffers
function HashFileMd5(const FileName: TFileName): RawUtf8;

/// compute the SHA-1 checksum of a given file
// - this function maps the THashFile signature as defined in mormot.core.buffers
function HashFileSha1(const FileName: TFileName): RawUtf8;

/// compute the SHA-224 checksum of a given file
// - this function maps the THashFile signature as defined in mormot.core.buffers
function HashFileSha224(const FileName: TFileName): RawUtf8;

/// compute the SHA-256 checksum of a given file
// - this function maps the THashFile signature as defined in mormot.core.buffers
function HashFileSha256(const FileName: TFileName): RawUtf8;

/// compute the SHA-384 checksum of a given file
// - this function maps the THashFile signature as defined in mormot.core.buffers
function HashFileSha384(const FileName: TFileName): RawUtf8;

/// compute the SHA-512 checksum of a given file
// - this function maps the THashFile signature as defined in mormot.core.buffers
function HashFileSha512(const FileName: TFileName): RawUtf8;

/// compute the SHA-512/256 checksum of a given file
// - this function maps the THashFile signature as defined in mormot.core.buffers
function HashFileSha512_256(const FileName: TFileName): RawUtf8;

/// compute the SHA-3-256 checksum of a given file
// - this function maps the THashFile signature as defined in mormot.core.buffers
function HashFileSha3_256(const FileName: TFileName): RawUtf8;

/// compute the SHA-3-512 checksum of a given file
// - this function maps the THashFile signature as defined in mormot.core.buffers
function HashFileSha3_512(const FileName: TFileName): RawUtf8;


{ **************** HMAC/PBKDF2 MCF SCRAM SCRAM-MCF TOTP High-Level Protocols }

{ some HMAC/PBKDF2 common wrappers defined here to redirect to TSynSigner }

/// compute the HMAC message authentication code using any hash function
procedure Hmac(algo: TSignAlgo; key, msg: pointer; keylen, msglen: integer;
  result: PHash512Rec);

/// compute the PBKDF2 derivation of a password using HMAC over any hash function
function Pbkdf2(algo: TSignAlgo; const password, salt: RawByteString;
  count: integer; digest: PHash512Rec): integer; overload;

/// compute the PBKDF2 derivation of a password using HMAC over any hash function
// - this overloaded function will return any size of the derived password
function Pbkdf2(algo: TSignAlgo; const password, salt: RawByteString;
  count, destlen: integer): RawByteString; overload;

/// compute the HMAC message authentication code using SHA-1 as hash function
procedure HmacSha1(const key, msg: RawByteString;
  out result: TSha1Digest); overload;

/// compute the HMAC message authentication code using SHA-1 as hash function
procedure HmacSha1(const key: TSha1Digest; const msg: RawByteString;
  out result: TSha1Digest); overload;

/// compute the PBKDF2 derivation of a password using HMAC over SHA-1
// - this function expect the resulting key length to match SHA-1 digest size
procedure Pbkdf2HmacSha1(const password, salt: RawByteString;
  count: integer; out result: TSha1Digest);

/// compute the HMAC message authentication code using SHA-384 as hash function
procedure HmacSha384(const key, msg: RawByteString;
  out result: TSha384Digest); overload;

/// compute the HMAC message authentication code using SHA-384 as hash function
procedure HmacSha384(const key: TSha384Digest; const msg: RawByteString;
  out result: TSha384Digest); overload;

/// compute the PBKDF2 derivation of a password using HMAC over SHA-384
// - this function expect the resulting key length to match SHA-384 digest size
procedure Pbkdf2HmacSha384(const password, salt: RawByteString;
  count: integer; out result: TSha384Digest);

/// compute the HMAC message authentication code using SHA-512 as hash function
procedure HmacSha512(const key, msg: RawByteString;
  out result: TSha512Digest); overload;

/// compute the HMAC message authentication code using SHA-512 as hash function
procedure HmacSha512(const key: TSha512Digest; const msg: RawByteString;
  out result: TSha512Digest); overload;

/// compute the PBKDF2 derivation of a password using HMAC over SHA-512
// - this function expect the resulting key length to match SHA-512 digest size
procedure Pbkdf2HmacSha512(const password, salt: RawByteString;
  count: integer; out result: TSha512Digest);

{ Extended "Modular Crypt" support }

const
  MCF_ALGO: array[TModularCryptFormat] of THashAlgo = (hfShake128, hfShake128,
    hfMD5, hfSHA256, hfSHA512, hfSHA1, hfSHA256, hfSHA512, hfSHA3_512,
    hfShake128, hfSHA256, hfSHA256);

/// compute the "Modular Crypt" hash of a given password
// - as returned by the python passlib library
// - see associated ModularCryptIdentify/ModularCryptVerify() functions
// - for mcfBCrypt, rounds is the 2^Cost value, so should be in 4..31 range
// - for mcfSCrypt, rounds is <logN:5-bit:1..31><R:14-bit:1..16384><P:13-bit:1..8192>
// - with default rounds=0 timings on a Core i5-13500 with SHA-NI are Md5Crypt=77us
// Sha256Crypt=35.91ms Sha512Crypt=94.18ms Pbkdf2Sha1=68.28ms Pbkdf2Sha256=35.89ms
// Pbkdf2Sha512=110.55ms Pbkdf2Sha3=112.58ms BCrypt=169.59ms BCryptSha256=170.85ms
// and SCrypt=143.66ms - consider mcfSha256Crypt for fast and safe hashing, and
// mcfBCrypt/mcfSCrypt for proven password storage, and align with OWASP/NIST
// recommendations for password hashing (100-250 ms) - note that SCrypt consumes
// 64MB of RAM and BCrypt always 4KB - see also MCF_ROUNDS[] global variable
function ModularCryptHash(format: TModularCryptFormat; const password: RawUtf8;
  rounds: cardinal = 0; saltsize: cardinal = 0; const salt: RawUtf8 = ''): RawUtf8; overload;

/// compute the "Modular Crypt" hash of a given password from expected format
// - the format is the value returned by info^ in ModularCryptIdentify(), able
// to identify the algorithm, its parameters, and the associated salt, e.g.
// !ModularCryptHash('$1$gV5s/FALJ/0x8nyo$', 'password') = '$1$gV5s/FALJ/0x8nyo$6yO.DIuu/ZF/eJaK5oHu90'
// - the format can e.g. be send back to the client to make the proper modular
// crypt hashing on its side, and send back the hash (or nonced proof) to the server
function ModularCryptHash(const format, password: RawUtf8;
  decodedFormat: PModularCryptFormat = nil): RawUtf8; overload;

/// identify if a given hash matches any "Modular Crypt" format
// - e.g. returns true and mcfMd5Crypt for '$1${salt}${checksum}' or
// mcfSha256Crypt for '$5$rounds={rounds}${salt}${checksum}'
// - just check the '${ident}$' prefix, without actually checking the content
// - can optionally return the header without the checksum, e.g. to notify a
// client side for the expected hash algorithm and parameters, i.e. the
// '${ident}${params}${salt}$' part excluding ending '{checksum}'
function ModularCryptIdentify(const hash: RawUtf8;
  info: PRawUtf8 = nil): TModularCryptFormat;

/// decode and check a password against a hash in "Modular Crypt" format
// - if allowed is not default [], it would return mcfUnknown if not in this set
// - you can also specify maxrounds, if you want to avoid any potential DoS
// from forged hash values with artificially high number of rounds
function ModularCryptVerify(const password, hash: RawUtf8;
  allowed: TModularCryptFormats = []; maxrounds: cardinal = 0): TModularCryptFormat;

/// low-level parsing function used by TSynHasher.UnixCryptVerify() and
// ModularCryptIdentify/ModularCryptVerify
// - for mcfSCrypt, rounds is <logN:5-bit:1..31><R:14-bit:1..16384><P:13-bit:1..8192>
function ModularCryptParse(var P: PUtf8Char; var rounds: cardinal;
  var salt: RawUtf8): TModularCryptFormat;

/// return a "Modular Crypt" text format with fake salt computed from an ID
// - without {checksum} - as returned by ModularCryptIdentify() info^ parameter
// - used e.g. by TRestServer.ReturnNonce() with an unknown UserName, to avoid
// the client being able to guess by fuzzing that this UserName is unknown
// - use SystemEntropy.Startup as SHA-256 seed for consistent results between
// calls, but eventually reset when the process is restarted
function ModularCryptFakeInfo(const id: RawUtf8;
  format: TModularCryptFormat = mcfUnknown): RawUtf8;

/// compute the fake "rounds" value for ModularCryptHash(mcfSCrypt)
// - i.e. <logN:5-bit:1..31><R:14-bit:1..16384><P:13-bit:1..8192>
// - accepts LogN as [1..31], BlockSize=R as [1..16384] and Parallel=P as [1..8192]
// - default is LogN=16, R=8, P=2 for safe interactive login (140ms and 64MB RAM)
function SCryptRounds(LogN: cardinal = 16; BlockSize: cardinal = 8;
  Parallel: cardinal = 2): cardinal;

/// decode the fake "rounds" value for ModularCryptHash(mcfSCrypt) into LogN/R/P
// - is the reverse function of SCryptRounds()
// - for Rounds = 0, returns default LogN=16 BlockSize=R=8 Parallel=P=2 values,
// which seems fine for interactive login (148ms and 64MB RAM):
procedure SCryptRoundsDecode(Rounds: cardinal; out LogN, BlockSize, Parallel: cardinal);

/// SCrypt password hashing function compatible with passlib.hash.scrypt output
// - as defined by https://www.tarsnap.com/scrypt/scrypt.pdf and RFC 7914
// - returning '$scrypt$ln=<log2(N)>,r=<r>,p=<p>$salt$<checksum>' passlib format
// - SCrypt is more memory sensitive than BCrypt, so may be preferred if you
// have enough RAM on your server, or you use challenge-like algorithm like S
// - in SCrypt terms, LogN will generate N = 2^LogN work factor scale, BlockSize
// is the R parameter (to match CPU cache line), and Parallel the P parameter
// - default is LogN=16, R=8, P=2 for safe interactive login (148ms and 64MB RAM):
// memory and CPU will scale linearly (BCrypt uses 4KB) - see SCryptMemoryUse()
// - P will increase the CPU time without affecting memory requirements
// - will call the available SCrypt() function defined - or a custom API
function SCryptHash(const Password: RawUtf8; const Salt: RawUtf8 = '';
  LogN: PtrUInt = 16; BlockSize: PtrUInt = 8; Parallel: PtrUInt = 2;
  HashPos: PInteger = nil; Api: TSCriptRaw = nil): RawUtf8;

{ SCRAM Client/Server mutual authentication as defined in RFC 5802/7677 }

type
  /// RFC 5802/7677 SCRAM client, as used e.g. by MongoDB
  // - can optionally implement SCRAM-MCF as Key Derivation Function, as
  // defined by draft-bouchez-kitten-scram-mcf-01 upcoming RFC proposal
  TScramClient = class
  protected
    fClientNonce, fAuthMessage, fServerProof, fLastError: RawUtf8;
    fAlgo: TSignAlgo;
    fSize: byte;
    fMcfSupport: boolean;
    fSigner: TSynSigner;
  public
    /// initialize the SCRAM client protocol
    // - with an associated Hash function, typically saSha1, saSha256 or saSha512
    // - you can also enable the SCRAM-MCF extension support
    constructor Create(aAlgo: TSignAlgo; aMcfSupport: boolean = false); reintroduce;
    /// finalize this instannce, and release all sensitive internal buffers
    destructor Destroy; override;
    /// generate a client nonce, and return the first client step
    // - returns e.g. 'n,,n=user,r=...' or 'n,,n=user,r=...,f=y'
    function ComputeFirstMessage(const User: RawUtf8; out Mechanism: RawUtf8;
      const TestForceNonce: RawUtf8 = ''): RawUtf8;
    /// compute the second and final client step
    // - ServerResponse is e.g. 'r=...,s=...,i=4096' or 'r=...,f=...'
    // - returns e.g. 'c=biws,r=...,p=...'
    function ComputeFinalMessage(const ServerResponse, Password: RawUtf8): RawUtf8;
    /// check the server proof to complete mutual authentication
    function CheckFinalResponse(const ServerResponse: RawUtf8): boolean;
    /// human-readable information about any failed ComputeFinalMessage() = ''
    // or CheckFinalResponse() = false
    property LastError: RawUtf8
      read fLastError;
    /// the size, in bytes, of the internal hash algorithm e.g. 32 for SHA-256
    property Size: byte
      read fSize;
    /// true if this client supports our SCRAM-MCF extension
    property McfSupport: boolean
      read fMcfSupport write fMcfSupport;
  end;

{ our own SCRAM-MCF client/server pattern, using ModularCryptHash() }

/// compute the SCRAM challenge to be stored on DB for a given "Modular Crypt" hash
// - expects a true KDF/MCF Hash in '$MCF prefix$hash' format as input
// - the result is bound to the User logon, so that it can't be reassigned to
// a more sensitive user on a compromised database ("key swap" attack)
// - returns '#MCF prefix' + base64uri(StoredKey + ServerKey) as output
function ScramPersistedKey(const Hash, User: RawUtf8): RawUtf8; overload;

/// compute the SCRAM challenge to be stored for a given "Modular Crypt" hash
function ScramPersistedKey(Mcf: TModularCryptFormat;
  const Password, User: RawUtf8; Rounds: cardinal = 0): RawUtf8; overload;

/// compute the SCRAM challenge to be stored for a given "Modular Crypt" hash
// - this overload expects the MCF format to be supplied as text
function ScramPersistedKey(const McfFormat, Password, User: RawUtf8): RawUtf8; overload;

/// compute the SCRAM client proof for a given "Modular Crypt" hash
// - the Server should have returned a  ModularCryptIdentify(info^) MCF prefix
// of the algorithm, parameters and salt, suitable for Hash computation via
// ModularCryptHash()
function ScramClientProof(const Hash, User: RawUtf8; var ClientSignature: THash256;
  const Msg: array of RawByteString): RawUtf8;

/// compute the SCRAM server proof for a given "Modular Crypt" challenge
// - and also verify a given ScramClientProof() value
function ScramServerProof(const PersistedKey, ClientProof: RawUtf8;
  const Msg: array of RawByteString): RawUtf8;

/// verify a given ScramServerProof() value on client side
// - can optionally return the server-side persisted value in DB (if needed
// to compute the URI secret)
function ScramClientServerAuth(const Hash, User, ServerProof: RawUtf8;
  var ClientSignature: THash256): boolean;

{ User 2FA authentication via TOTP as defined in RFC 6238 }

const
  TOTP_DIGITS = 6;      // 6 digits seems fine as default
  TOTP_PERIOD = 30;     // our code fixes the period to default 30 seconds
  TOTP_SECRET_MIN = 16; // 128-bit is the bare minimum

/// compute the RFC 6238 TOTP user code from its base32 (or binary) secret
// - supports digits in [4..8] and at least 128-bit = 16-byte of binary secret
function TotpGenerate(const secret: RawUtf8; digits: integer = TOTP_DIGITS;
  algo: TSignAlgo = saSha1; binary: boolean = false; timestep: Int64 = -1): RawUtf8;

/// compute a base32-encoded RFC 6238 TOTP secret to share with the client
// - any len < 16 bytes would be rounded up to 16 (i.e. 128-bit)
function TotpGenerateSecret(len: PtrInt = 20): RawUtf8;

/// verify a RFC 6238 TOTP user code from its base32 (or binary) secret
function TotpValidate(const secret, usercode: RawUtf8; window: integer = 1;
  algo: TSignAlgo = saSha1; timestep: Int64 = -1; binary: boolean = false): boolean;

/// encode Google Authenticator otpauth:// URI scheme value
function TotpUrl(const issuer, account, b32secret: RawUtf8;
  digits: integer = TOTP_DIGITS; algo: TSignAlgo = saSha1;
  period: integer = TOTP_PERIOD): RawUtf8;


{ **************** Client and Server HTTP Access Authentication }

type
  /// the exception class raised during Digest access authentication
  EDigest = class(ESynException);

  /// the Digest access authentication supported algorithms
  // - match the three official algorithms as registered by RFC 7616, with the
  // addition of the unstandard (but safe) SHA3-256 algorithm
  TDigestAlgo = (
    daUndefined,
    daMD5,
    daMD5_Sess,
    daSHA256,
    daSHA256_Sess,
    daSHA512_256,
    daSHA512_256_Sess,
    daSHA3_256,
    daSHA3_256_Sess);

/// compute the Digest access authentication client code for a given algorithm
// - as defined in https://en.wikipedia.org/wiki/Digest_access_authentication
// - FromServer is the 'xxx' encoded value from 'WWW-Authenticate: Digest xxx'
// - may return '' if Algo does not match algorithm=... value (MD5 or SHA-256)
// - DigestUriName is customized as "digest-uri" e.g. for LDAP digest auth
function DigestClient(Algo: TDigestAlgo;
  const FromServer, DigestMethod, DigestUri, UserName: RawUtf8;
  const Password: SpiUtf8; const DigestUriName: RawUtf8 = 'uri'): RawUtf8;

/// extract the Digest access authentication realm on client side
// - FromServer is the 'xxx' encoded value from 'WWW-Authenticate: Digest xxx'
// - could be proposed to the user interation UI to specify the auth context
function DigestRealm(const FromServer: RawUtf8): RawUtf8;

/// compute the Basic access authentication client header
// - as defined in https://en.wikipedia.org/wiki/Basic_access_authentication
// - for safety, caller should better call FillZero(Result) once done
procedure BasicClient(const UserName: RawUtf8; const Password: SpiUtf8;
  out Result: SpiUtf8; const Prefix: RawUtf8 = 'Authorization: Basic ');

/// extract the Basic access authentication realm on client side
// - FromServer is the 'xxx' encoded value from 'WWW-Authenticate: Basic xxx'
// - could be proposed to the user interation UI to specify the auth context
function BasicRealm(const FromServer: RawUtf8): RawUtf8;

/// compute the HA0 for a given set of Digest access credentials
// - i.e. HA0 = Hash(username:realm:password)
// - return the number of binary hash bytes stored into HA0
function DigestHA0(Algo: TDigestAlgo; const UserName, Realm: RawUtf8;
  const Password: SpiUtf8; out HA0: THash512Rec): integer;

const
  /// the Digest access authentication processing cryptographic algorithms
  DIGEST_ALGO: array[daMD5.. high(TDigestAlgo)] of THashAlgo = (
    hfMD5,          // daMD5
    hfMD5,          // daMD5_Sess
    hfSHA256,       // daSHA256
    hfSHA256,       // daSHA256_Sess
    hfSHA512_256,   // daSHA512_256
    hfSHA512_256,   // daSHA512_256_Sess
    hfSHA3_256,     // daSHA3_256
    hfSHA3_256);    // daSHA3_256_Sess

  /// the Digest access authentication algorithm name as used during handshake
  DIGEST_NAME: array[daMD5.. high(TDigestAlgo)] of RawUtf8 = (
    'MD5',                // daMD5
    'MD5-sess',           // daMD5_Sess
    'SHA-256',            // daSHA256
    'SHA-256-sess',       // daSHA256_Sess
    'SHA-512-256',        // daSHA512_256
    'SHA-512-256-sess',   // daSHA512_256_Sess
    'SHA3-256',           // daSHA3_256
    'SHA3-256-sess');     // daSHA3_256_Sess

  /// the Digest access authentication algorithms which hashes the session info
  // - i.e. includes nonce, cnonce (and authzid) to the hashed response
  // - it is slightly slower, but much safer, and recommended in our use case
  // of authentication, not authorization
  DIGEST_SESS = [daMD5_Sess, daSHA256_Sess, daSHA512_256_Sess, daSHA3_256_Sess];

/// initiate a Digest access authentication from server side for a given algorithm
// - Opaque could be e.g. an obfuscated HTTP connection ID to avoid MiM attacks
// - Prefix/Suffix are typically 'WWW-Authenticate: Digest ' and #13#10 to
// construct a HTTP header
function DigestServerInit(Algo: TDigestAlgo;
  const QuotedRealm, Prefix, Suffix: RawUtf8; Opaque: Int64; Tix64: Int64 = 0): RawUtf8;

type
  /// the result of IBasicAuthServer.CreckCredential() internal method
  TAuthServerResult = (
    asrUnknownUser,
    asrIncorrectPassword,
    asrRejected,
    asrMatch);

  /// callback event able to return the HA0 binary from a username
  // - called by DigestServerAuth() e.g. to lookup from a local .htdigest file
  // - is typically implemented via DigestHA0() wrapper function
  TOnDigestServerAuthGetUserHash = function(
    const User, Realm: RawUtf8; out HA0: THash512Rec): TAuthServerResult of object;

/// validate a Digest access authentication on server side
// - returns true and the user/uri from a valid input token, or false on error
function DigestServerAuth(Algo: TDigestAlgo; const Realm, Method: RawUtf8;
  FromClient: PUtf8Char; Opaque: Int64;
  const OnSearchUser: TOnDigestServerAuthGetUserHash;
  out User, Url: RawUtf8; NonceExpSec: PtrUInt; Tix64: Qword = 0): TAuthServerResult;

/// parse a Basic access authentication on server side
// - returns true and the user/password from a valid input, or false on error
function BasicServerAuth(FromClient: PUtf8Char;
  out User, Password: RawUtf8): boolean;

type
  /// callback event used by TBasicAuthServer.OnBeforeAuth/OnAfterAuth
  // - allow to reject an user before or after its credentials are checked
  // - should return true to continue, or false to abort the authentication
  // and let TBasicAuthServer.CheckCredential return asrRejected
  TOnAuthServer = function(Sender: TObject; const User: RawUtf8): boolean of object;

  /// parent abstract HTTP access authentication on server side
  // - as used e.g. by THttpServerSocketGeneric for its optional authentication
  // - you should use inherited IBasicAuthServer or IDigestAuthServer interfaces
  IHttpAuthServer = interface
    ['{036B2802-56BE-422F-9146-773702C86387}']
    /// the realm associated with this access authentication
    function Realm: RawUtf8;
    /// retrieve the implementation class instance
    function Instance: TObject;
  end;

  /// HTTP BASIC access authentication on server side
  // - as used e.g. by THttpServerSocketGeneric for its BASIC authentication
  IBasicAuthServer = interface(IHttpAuthServer)
    ['{5C301470-39BB-4CBB-8366-01A7F23032F2}']
    /// compute a Basic server authentication request header
    // - return e.g. 'WWW-Authenticate: Basic realm="Realm"'#13#10
    function BasicInit: RawUtf8;
    /// validate a Basic client authentication response
    // - FromClient typically follow 'Authorization: Basic ' header text
    function BasicAuth(FromClient: PUtf8Char; out ClientUser: RawUtf8): boolean;
    /// check the stored credentials as for the TOnHttpServerBasicAuth callback
    // - used for the BASIC authentication scheme in THttpServerSocketGeneric
    function OnBasicAuth(aSender: TObject;
      const aUser: RawUtf8; const aPassword: SpiUtf8): boolean;
    /// check the credentials stored for a given user
    // - returns true if supplied aUser/aPassword are correct, false otherwise
    function CheckCredential(const aUser: RawUtf8;
      const aPassword: SpiUtf8): TAuthServerResult;
  end;

  /// HTTP DIGEST access authentication on server side
  // - as used e.g. by THttpServerSocketGeneric for its DIGEST authentication
  IDigestAuthServer = interface(IHttpAuthServer)
    ['{75B1B7B8-4981-4C09-BD8C-E938A2802ED1}']
    /// compute a Digest server authentication request
    // - used for the DIGEST authentication scheme in THttpServerSocketGeneric
    // - returns the standard HTTP header with the default Prefix/Suffix
    // - Opaque is a 64-bit number, typically the THttpServerConnectionID
    // - properly implemented in TDigestAuthServer: THttpAuthServer raise EDigest
    function DigestInit(Opaque, Tix64: Int64;
      const Prefix: RawUtf8 = 'WWW-Authenticate: Digest ';
      const Suffix: RawUtf8 = EOL): RawUtf8;
    /// validate a Digest client authentication response
    // - used for the DIGEST authentication scheme in THttpServerSocketGeneric
    // - FromClient typically follow 'Authorization: Digest ' header text
    // - Opaque should match the value supplied on previous ServerInit() call
    // - properly implemented in TDigestAuthServer: THttpAuthServer raise EDigest
    function DigestAuth(FromClient: PUtf8Char; const Method: RawUtf8;
      Opaque, Tix64: Int64; out ClientUser, ClientUrl: RawUtf8): TAuthServerResult;
    /// quickly check if the supplied client response is likely to be compatible
    // - FromClient is typically a HTTP header
    // - will just search for the 'algorithm=xxx,' text pattern
    function DigestAlgoMatch(const FromClient: RawUtf8): boolean;
  end;

  /// abstract BASIC access authentication on server side
  // - don't use this class but e.g. TDigestAuthServerMem or TDigestAuthServerFile
  // - will implement the IBasicAuthServer process in an abstract way
  TBasicAuthServer = class(TInterfacedPersistent, IBasicAuthServer)
  protected
    fRealm, fQuotedRealm, fBasicInit: RawUtf8;
    fOnBeforeAuth, fOnAfterAuth: TOnAuthServer;
    fOnAfterAuthDelayed: boolean;
    function BeforeAuth(Sender: TObject; const User: RawUtf8): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    function AfterAuth(Sender: TObject; const User: RawUtf8): boolean;
      {$ifdef HASINLINE}inline;{$endif}
  public
    /// initialize the HTTP access authentication engine
    constructor Create(const aRealm: RawUtf8); reintroduce;
    /// check the credentials stored for a given user
    // - this is the main abstract virtual method to override for BASIC auth
    // - will also trigger OnBeforeAuth/OnAfterAuth callbacks
    // - returns true if supplied aUser/aPassword are correct, false otherwise
    function CheckCredential(const aUser: RawUtf8;
      const aPassword: SpiUtf8): TAuthServerResult; virtual; abstract;
    { -- IHttpAuthServer and IBasicAuthServer methods }
    /// retrieve the realm associated with this access authentication
    // - a good practice is to use the server host name or UUID as realm
    function Realm: RawUtf8;
    /// retrieve the implementation class instance
    function Instance: TObject;
    /// compute a Basic server authentication request header
    // - return e.g. 'WWW-Authenticate: Basic realm="Realm"'#13#10
    function BasicInit: RawUtf8;
    /// validate a Basic client authentication response
    function BasicAuth(FromClient: PUtf8Char; out ClientUser: RawUtf8): boolean;
    /// check the stored credentials as for the TOnHttpServerBasicAuth callback
    function OnBasicAuth(aSender: TObject;
      const aUser: RawUtf8; const aPassword: SpiUtf8): boolean;
    /// allow to reject an user before its credentials are checked
    // - can implement e.g. the "search and bind" pattern on a slow LDAP server
    property OnBeforeAuth: TOnAuthServer
      read fOnBeforeAuth write fOnBeforeAuth;
    /// allow to reject an user after its credentials are checked
    property OnAfterAuth: TOnAuthServer
      read fOnAfterAuth write fOnAfterAuth;
  end;

  /// abstract DIGEST and BASIC access authentication on server side
  // - should be inherited with proper persistence of users credentials
  // - notice: won't maintain sessions in memory, just check the credentials each
  // time so these classes should be used for authentication not authorization;
  // a typical usage is with our TRestServer sessions or to generate a JWT bearer
  // - the RFC expects in-memory sessions, especially for nonce counters but we
  // store a THttpServerConnectionID to the Opaque parameter to compensate, and
  // we implement an expiration delay with each ServerInit request
  // - BasicInit and BasicAuth methods could be used to implement Basic access
  // authentication calling the very same GetUserHash() virtual method
  TDigestAuthServer = class(TBasicAuthServer, IDigestAuthServer)
  protected
    fAlgo: TDigestAlgo;
    fAlgoSize: byte;
    fRequestExpSec: integer;
    fOpaqueObfuscate: Int64;
    // this is the main abstract virtual method to override for DIGEST auth
    function GetUserHash(const aUser, aRealm: RawUtf8;
      out aDigest: THash512Rec): TAuthServerResult; virtual; abstract;
    // TOnDigestServerAuthGetUserHash signature
    function GetUserHashWithCallback(const aUser, aRealm: RawUtf8;
      out aDigest: THash512Rec): TAuthServerResult;
    procedure ComputeDigest(const aUser: RawUtf8; const aPassword: SpiUtf8;
      out Digest: THash512Rec);
  public
    /// initialize the Digest access authentication engine
    constructor Create(const aRealm: RawUtf8; aAlgo: TDigestAlgo); reintroduce; virtual;
    /// check the credentials stored for a given user
    function CheckCredential(const aUser: RawUtf8;
      const aPassword: SpiUtf8): TAuthServerResult; override;
    { -- IDigestAuthServer methods }
    /// compute a Digest server authentication request
    function DigestInit(Opaque, Tix64: Int64;
      const Prefix, Suffix: RawUtf8): RawUtf8;
    /// validate a Digest client authentication response
    function DigestAuth(FromClient: PUtf8Char; const Method: RawUtf8;
      Opaque, Tix64: Int64; out ClientUser, ClientUrl: RawUtf8): TAuthServerResult;
    /// quickly check if the supplied client response is likely to be compatible
    function DigestAlgoMatch(const FromClient: RawUtf8): boolean;
  published
    /// the Digest algorithm used with this instance
    property Algo: TDigestAlgo
      read fAlgo;
    /// how many seconds a ServerInit() request is valid for ServerAuth()
    // - default is 60 seconds
    property RequestExpSec: integer
      read fRequestExpSec write fRequestExpSec;
  end;

  /// meta-class of server side Digest access authentication
  TDigestAuthServerClass = class of TDigestAuthServer;

  /// Digest access authentication on server side using in-memory storage
  TDigestAuthServerMem = class(TDigestAuthServer)
  protected
    fUsers: TSynDictionary; // UserName:RawUtf8 / HA0:TDigestAuthHash
    fModified: boolean;
    function GetUserHash(const aUser, aRealm: RawUtf8;
      out aDigest: THash512Rec): TAuthServerResult; override; // main method
    function GetCount: integer;
  public
    /// initialize the Digest access authentication engine
    constructor Create(const aRealm: RawUtf8; aAlgo: TDigestAlgo); override;
    /// finalize the Digest access authentication engine
    destructor Destroy; override;
    /// retrieve all user names as a single array
    // - could be used e.g. to display a user list in the UI
    function GetUsers: TRawUtf8DynArray;
    /// change the credentials of a given user
    // - if aUser does not exist, the credential will be added
    // - if aUser does exist, the credential will be modified
    // - if aPassword is '', the credential will be deleted
    procedure SetCredential(const aUser: RawUtf8; const aPassword: SpiUtf8);
    /// safely delete all stored credentials
    // - also fill TDigestAuthHash stored memory with zeros, against forensic
    procedure ClearCredentials;
    /// low-level access to the internal TSynDictionary storage
    // - to set e.g. Users.TimeOutSeconds or call Users.DeleteAll if this class
    // is used as cache
    property Users: TSynDictionary
      read fUsers;
  published
    /// how many items are currently stored in memory
    property Count: integer
      read GetCount;
    /// flag set if SetCredential() was called but not persisted yet
    property Modified: boolean
      read fModified;
  end;

  /// Digest access authentication on server side using a .htdigest file
  // - can also add, delete or update credentials
  // - file content is refreshed from disk when it has been modified
  // - only a single Realm is allowed per .htdigest file
  // - this class is thread-safe, with an efficient R/W lock
  // - file can be AES256-GCM encrypted on disk (non-standard but much safer)
  TDigestAuthServerFile = class(TDigestAuthServerMem)
  protected
    fFileName: TFileName;
    fFileLastTime: TUnixTime;
    fAesKey: TSha256Digest;
    function GetAes: TAesAbstract;
    function GetEncrypted: boolean;
  public
    /// initialize the Digest access authentication engine from a .htdigest file
    // - aFilePassword can optionally encrypt the .htdigest file using AES256-GCM
    // - default algorithm is MD5 with sessions - as used by NGINX auth_digest module
    constructor Create(const aRealm: RawUtf8; const aFileName: TFileName;
      const aFilePassword: SpiUtf8 = ''; aAlgo: TDigestAlgo = daMD5_Sess); reintroduce;
    /// finalize the Digest access authentication engine
    destructor Destroy; override;
    /// force (re)reading the .htdigest file content
    procedure LoadFromFile;
    // save any SetCredential() pending modification to the .htdigest file
    procedure SaveToFile;
    /// update or refresh file if needed
    // - typically called every few seconds in a background thread
    // - write any pending SetCredential() new/updated values
    // - reload the file if it has been modified on disk - on-disk modifications
    // will be ignored if SetCredential() has been called in-between
    function RefreshFile: boolean;
  published
    /// the .htdigest file name associated with this instance
    property FileName: TFileName
      read fFileName;
    /// flag if aFilePassword was specified at create, i.e. AES256-GCM is used
    property Encrypted: boolean
      read GetEncrypted;
  end;

function ToText(res: TAuthServerResult): PShortString; overload;


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
    fSafe: TLightLock; // no need of whole TRTLCriticalSection / TOSLock
    fAheadMode: boolean;
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

  /// TBinaryCookieGenerator execution context - may be persisted on disk
  TBinaryCookieContext = packed record
    /// identify this data structure on disk - should equal 1 now
    Version: cardinal;
    /// 31-bit increasing counter, to implement unique session ID
    SessionSequence: TBinaryCookieGeneratorSessionID;
    /// the random initial value of the SessionSequence counter
    SessionSequenceStart: TBinaryCookieGeneratorSessionID;
    /// 32-bit random salt used for anti-fuzzing via crc32c()
    CrcSalt: cardinal;
    /// identify deleted cookies as 32-bit low = sessionid, 32-bit high = expires
    // - used to avoid cookie replay attacks
    // - so to clean up, we just remove the smallest items against current time
    Invalid: TInt64DynArray;
    /// 128-bit secret information, used for AES-GCM encoding of the cookie
    CryptKey: THash128;
  end;

  /// efficient thread-safe cookie generation
  // - you can see it as a JWT-Of-The-Poor: faster to parse and validate
  // its content, and with very efficience binary-based serialization of
  // an (optional) associated record
  // - cookie is digitally signed, and any associated record is encrypted, using
  // the cryptographically secure AES-GCM-128 algorithm
  TBinaryCookieGenerator = class(TObjectLightLock)
  protected
    fContext: TBinaryCookieContext;
    fCookieName: RawUtf8;
    fDefaultTimeOutMinutes: cardinal;
    fInvalidCount: integer; // how many items are currently in fContext.Invalid[]
    fNextUnixTimeMinimalInvalidateCheck: cardinal;
    fAes: TAesGcmEngine;
  public
    /// initialize ephemeral temporary cookie generation
    // - a new secret key is generated each time this constructor is called
    // - you can use Load/Save methods to persist a key e.g. between reboots
    constructor Create(const Name: RawUtf8 = 'mORMot';
      DefaultSessionTimeOutMinutes: cardinal = 0); reintroduce;
    /// finalize this generator
    destructor Destroy; override;
    /// will initialize a new Base64Uri-encoded session cookie
    // - with an optional record data
    // - will return the 32-bit internal session ID and a Base64Uri cookie,
    // ready to be used as HTTP cookie or a temporary URI
    // - you can supply a time period, after which the session will expire -
    // default 0 will use DefaultTimeOutMinutes as supplied to Create()
    function Generate(out Cookie: RawUtf8; TimeOutMinutes: cardinal = 0;
      PRecordData: pointer = nil;
      PRecordTypeInfo: PRttiInfo = nil): TBinaryCookieGeneratorSessionID;
    /// decode a base64uri cookie and optionally fill an associated record
    // - return the associated session/sequence number, 0 on error
    // - Invalidate=true would force this cookie to be rejected in the future,
    // adding it into an internal in-memory list, and avoid cookie replay attacks
    function Validate(const Cookie: RawUtf8; PRecordData: pointer = nil;
      PRecordTypeInfo: PRttiInfo = nil; PExpires: PUnixTime = nil;
      PIssued: PUnixTime = nil; Invalidate: boolean = false;
      Now32: TUnixTimeMinimal = 0): TBinaryCookieGeneratorSessionID; overload;
    /// decode a base64uri cookie buffer and optionally fill an associated record
    // - input Cookie/CookieLen could be obtained via CookieFromHeaders() or
    // via THttpCookies from mormot.core.text
    function Validate(Cookie: PUtf8Char; CookieLen: PtrInt; PRecordData: pointer;
      PRecordTypeInfo: PRttiInfo; PExpires: PUnixTime = nil;
      PIssued: PUnixTime = nil; Invalidate: boolean = false;
      Now32: TUnixTimeMinimal = 0): TBinaryCookieGeneratorSessionID; overload;
    /// allow currently available cookies to be recognized after server restart
    function Save: RawUtf8;
    /// unserialize the cookie generation context as serialized by Save
    function Load(const Saved: RawUtf8): boolean;
    /// current execution context, able to persisted
    property Context: TBinaryCookieContext
      read fContext;
  published
    /// default value used when Generate() has TimeOutMinutes=0
    // - if equals 0, one month delay is used as "never expire"
    property DefaultTimeOutMinutes: cardinal
      read fDefaultTimeOutMinutes write fDefaultTimeOutMinutes;
    /// the cookie name, used for storage in the client side HTTP headers
    // - is not part of the Generate/Validate content, but could be used
    // when the cookie is actually stored in HTTP headers
    property CookieName: RawUtf8
      read fCookieName write fCookieName;
  end;


{ ************************** Minimal PEM/DER Encoding/Decoding }

type
  /// a certificate (typically X.509) encoded as PEM / text
  TCertPem = type RawUtf8;

  /// a certificate (typically X.509) encoded as binary
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
    pemEcPublicKey,
    pemEncryptedPrivateKey,
    pemCertificateRequest,
    pemDhParameters,
    pemEcParameters,
    pemSsh2EncryptedPrivateKey,
    pemSsh2PublicKey,
    pemSynopseSignature,
    pemSynopseCertificate,
    pemSynopseUnencryptedPrivateKey,
    pemSynopseEccEncryptedPrivateKey,
    pemSynopseRsaEncryptedPrivateKey,
    pemSynopsePrivateKeyAndCertificate);
  PPemKind = ^TPemKind;

const
  /// the supported trailer markers of a PEM text instance
  // - use chars after '-----BEGIN' and before '----' for recognition
  PEM_BEGIN: array[TPemKind] of RawUtf8 = (
    '-----BEGIN PRIVACY-ENHANCED MESSAGE-----'#13#10,
    '-----BEGIN CERTIFICATE-----'#13#10,
    '-----BEGIN X509 CRL-----'#13#10,
    '-----BEGIN PRIVATE KEY-----'#13#10,
    '-----BEGIN PUBLIC KEY-----'#13#10,
    '-----BEGIN RSA PRIVATE KEY-----'#13#10,
    '-----BEGIN RSA PUBLIC KEY-----'#13#10,
    '-----BEGIN EC PRIVATE KEY-----'#13#10,
    '-----BEGIN EC PUBLIC KEY-----'#13#10,
    '-----BEGIN ENCRYPTED PRIVATE KEY-----'#13#10,
    '-----BEGIN CERTIFICATE REQUEST-----'#13#10,
    '-----BEGIN DH PARAMETERS-----'#13#10,
    '-----BEGIN EC PARAMETERS-----'#13#10,
    '-----BEGIN SSH2 ENCRYPTED PRIVATE KEY-----'#13#10,
    '-----BEGIN SSH2 PUBLIC KEY-----'#13#10,
    '-----BEGIN SYNECC SIGNATURE-----'#13#10,
    '-----BEGIN SYNECC CERTIFICATE-----'#13#10,
    '-----BEGIN SYNECC PRIVATE KEY-----'#13#10,
    '-----BEGIN SYNECC ENCRYPTED PRIVATE KEY-----'#13#10,
    '-----BEGIN SYNRSA ENCRYPTED PRIVATE KEY-----'#13#10,
    '-----BEGIN SYNECC BOUNDED CERTIFICATE-----'#13#10);

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
    '-----END EC PUBLIC KEY-----'#13#10,
    '-----END ENCRYPTED PRIVATE KEY-----'#13#10,
    '-----END CERTIFICATE REQUEST-----'#13#10,
    '-----END DH PARAMETERS-----'#13#10,
    '-----END EC PARAMETERS-----'#13#10,
    '-----END SSH2 ENCRYPTED PRIVATE KEY-----'#13#10,
    '-----END SSH2 PUBLIC KEY-----'#13#10,
    '-----END SYNECC SIGNATURE-----'#13#10,
    '-----END SYNECC CERTIFICATE-----'#13#10,
    '-----END SYNECC PRIVATE KEY-----'#13#10,
    '-----END SYNECC ENCRYPTED PRIVATE KEY-----'#13#10,
    '-----END SYNRSA ENCRYPTED PRIVATE KEY-----'#13#10,
    '-----END SYNECC BOUNDED CERTIFICATE-----'#13#10);

  /// our proprietary SYNECC TPemKind supported formats
  PEM_SYNECC =  [pemSynopseSignature,
                 pemSynopseCertificate,
                 pemSynopseUnencryptedPrivateKey,
                 pemSynopsePrivateKeyAndCertificate,
                 pemSynopseEccEncryptedPrivateKey];

  /// the OID of the supported hash algorithms, decoded as text
  ASN1_OID_HASH: array[THashAlgo] of RawUtf8 = (
    '1.2.840.113549.2.5',       // hfMD5
    '1.3.14.3.2.26',            // hfSHA1
    '2.16.840.1.101.3.4.2.1',   // hfSHA256
    '2.16.840.1.101.3.4.2.2',   // hfSHA384
    '2.16.840.1.101.3.4.2.3',   // hfSHA512
    '2.16.840.1.101.3.4.2.6',   // hfSHA512_256
    '2.16.840.1.101.3.4.2.8',   // hfSHA3_256
    '2.16.840.1.101.3.4.2.10',  // hfSHA3_512
    '2.16.840.1.101.3.4.2.4',   // hfSHA224
    '2.16.840.1.101.3.4.2.7',   // hfSHA3_224
    '2.16.840.1.101.3.4.2.9',   // hfSHA3_384
    '2.16.840.1.101.3.4.2.11',  // hfShake128
    '2.16.840.1.101.3.4.2.12',  // hfShake256
    '2.16.840.1.101.3.4.2.1',   // hfSHA256_128 is SHA-256 truncated to 128-bit
    '2.16.840.1.101.3.4.2.1');  // hfSHA256_160 is SHA-256 truncated to 160-bit

  /// the OID of all ECC public keys (X962)
  // - is stored as prefix to CKA_OID[ckaEcc256..ckaEcc256k] parameter
  ASN1_OID_X962_PUBLICKEY  = '1.2.840.10045.2.1';

  ASN1_OID_CDP         = '2.5.29.31';
  ASN1_OID_AIA         = '1.3.6.1.5.5.7.1.1';
  ASN1_OID_AIA_OCSP    = '1.3.6.1.5.5.7.48.1';
  ASN1_OID_AIA_ISSUERS = '1.3.6.1.5.5.7.48.2';


/// convert a binary DER content into a single-instance PEM text
function DerToPem(der: pointer; len: PtrInt; kind: TPemKind): TCertPem; overload;

/// convert a binary DER content into a single-instance PEM text
function DerToPem(const der: TCertDer; kind: TPemKind): TCertPem; overload;

/// convert a single-instance PEM text file into a binary DER
// - if the supplied buffer doesn't start with '-----BEGIN .... -----'
// trailer, will expect the input to be plain DER binary and directly return it
function PemToDer(const pem: TCertPem; const kind: PPemKind = nil): TCertDer;
  {$ifdef HASINLINE} inline; {$endif}

/// parse a multi-PEM text input and return the next PEM content
// - search and identify any PEM_BEGIN/PEM_END markers
// - optionally returning the recognized TPemKind (maybe pemUnspecified)
// - see also NextPemToDer()
function NextPem(var P: PUtf8Char; Kind: PPemKind = nil): TCertPem;

/// parse a multi-PEM text input and return the next PEM content as binary DER
// - search and identify any PEM_BEGIN/PEM_END markers
// - optionally returning the recognized TPemKind (maybe pemUnspecified)
function NextPemToDer(var P: PUtf8Char; Kind: PPemKind = nil): TCertDer;

/// quickly check the begin/end of a single-instance PEM text
// - do not validate the internal Base64 encoding, just the trailer/ending lines
// so may (hardly) give some false positive
// - expects at least a single-instance PEM, with ----BEGIN and -----END markers
function IsPem(const pem: RawUtf8): boolean;

/// quickcly check if a PEM text is likely to be encrypted
// - search for a PEM format, with ENCRYPTED keyword
// - won't decode the Base64 encoded binary, so may return some false negative
function IsPemEncrypted(const pem: TCertPem): boolean;

/// extract pemCertificate and a private key concatenated in a PEM text file
// - used e.g. by TCryptCertX509/TCryptCertOpenSsl.Load(cccCertWithPrivateKey)
function PemToCertAndPrivKey(const MultiPartPem: RawUtf8;
  out Cert, PrivKey: RawByteString): boolean;

/// extract the first pemCertificate concatenated in a PEM text file
// - used e.g. by TX509.HashForChannelBinding
function PemToCert(const MultiPartPem: RawUtf8): RawByteString;

/// low-level binary-to-DER encoder of small buf with buflen < 127
function DerAppend(P: PAnsiChar; buf: PByteArray; buflen: PtrUInt): PAnsiChar;

/// low-level DER sequence to binary decoding
// - only support a single DER_INTEGER sequence format as generated by DerAppend()
function DerParse(P: PAnsiChar; buf: PByteArray; buflen: PtrInt): PAnsiChar;

/// cipher any private key buffer into safe binary
// - encryption uses safe PBKDF2 HMAC-SHA256 AES-CTR-128 and AF-32 algorithms
// - as used by pemSynopseEccEncryptedPrivateKey format and EccPrivateKeyEncrypt()
// or TCryptPrivateKey.Save and TCryptCertX509.Save
function PrivateKeyEncrypt(const Input, Salt: RawByteString;
  const PrivatePassword: SpiUtf8; AfSplitRounds: integer = 31;
  Pbkdf2Rounds: integer = 1000): RawByteString;

/// uncipher some binary into a raw private key buffer
// - encryption uses safe PBKDF2 HMAC-SHA256 AES-CTR-128 and AF-32 algorithms
// - as used by pemSynopseEccEncryptedPrivateKey format and EccPrivateKeyDecrypt()
// or TCryptPrivateKey.Load and TCryptCertX509.Load
function PrivateKeyDecrypt(const Input, Salt: RawByteString;
  const PrivatePassword: SpiUtf8; AfSplitRounds: integer = 31;
  Pbkdf2Rounds: integer = 1000): RawByteString;


/// uncipher a standard PKCS#8 encryptedPrivateKeyInfo into its PrivateKeyInfo
// - as generated e.g. by "openssl genpkey -aes256" or "openssl pkcs8 -topk8"
// i.e. '-----BEGIN ENCRYPTED PRIVATE KEY-----' PEM content
// - Input can be either PEM text or DER binary
// - supports PBES2 (RFC 8018) with PBKDF2 over HMAC-SHA1/SHA224/SHA256/SHA384/
// SHA512 and AES-128/192/256-CBC, i.e. all OpenSSL 1.1/3.x default formats -
// but not the legacy PBES1 or 3DES/DES encryption schemes
// - returns the decrypted PKCS#8 PrivateKeyInfo DER binary, or '' if Input is
// not a supported encryptedPrivateKeyInfo or if Password is wrong
// - used by TCryptPrivateKey.Load and TCryptCertX509.Load to load OpenSSL
// encrypted private keys without OpenSSL
// - warning: don't forget FillZero() once done with the sensitive result
function Pkcs8DecryptPrivateKey(const Input: RawByteString;
  const Password: SpiUtf8): RawByteString;

/// cipher a PKCS#8 PrivateKeyInfo into a standard encryptedPrivateKeyInfo
// - use PBES2 + PBKDF2 HMAC-SHA256 + AES-256-CBC with random salt and IV, i.e.
// the same algorithms than "openssl pkcs8 -topk8 -v2 aes256"
// - default 100000 PBKDF2 Rounds is safer than OpenSSL 3.x 2048 default
// - PrivateKeyInfo is expected to be a PKCS#8 PrivateKeyInfo DER binary, not
// a PKCS#1 RSAPrivateKey as returned by TCryptPrivateKeyRsa.ToDer
// - returns the encryptedPrivateKeyInfo DER binary, or '' on invalid input:
// use DerToPem(result, pemEncryptedPrivateKey) for the PEM text
// - can be decrypted by Pkcs8DecryptPrivateKey() or any PKCS#8 library
function Pkcs8EncryptPrivateKey(const PrivateKeyInfo: RawByteString;
  const Password: SpiUtf8; Rounds: integer = 100000): RawByteString;

var
  /// if TCryptPrivateKey.Save with a Password should use standard PKCS#8
  // - true by default, generating '-----BEGIN ENCRYPTED PRIVATE KEY-----' PEM
  // as Pkcs8EncryptPrivateKey(), readable e.g. by OpenSSL
  // - set to false to use our proprietary PrivateKeyEncrypt() format, which
  // could be needed if the key should be loaded by older mORMot 2 versions
  // - TCryptPrivateKey.Load will always recognize both formats
  PrivateKeySavePkcs8: boolean = true;


/// compute the hash of a certificate as expected by Kerberos Channel Binding
// - use the SignatureHashAlgo, forcing SHA-256 for 'MD5' or 'SHA1'
// - returns the size of the corresponding Hash in bytes, or 0 on error
// - as defined by RFC 5929 - side note: we follow this RFC, which does not take
// into account SHA-224 substitution to SHA-256 as it should
// - see also TX509.HashForChannelBinding() class method in mormot.crypt.X509
function HashForChannelBinding(const CertRaw: TCertDer;
  const SignatureHashAlgo: RawUtf8; out Hash: THash512Rec): integer;

/// compute a Kerberos raw Intermediate Key according to RFC 3962
// - to be used for testing purpose only, with official test vectors
function MakeKerberosKeySeed(const PassPhrase, Salt: RawUtf8;
  EncType: integer = ENCTYPE_AES256_CTS_HMAC_SHA1_96;
  Iterations: integer = 0; Hmac: PSignAlgo = nil): RawByteString;

/// compute a Kerberos raw Derived Key according to RFC 3962
// - EncType could be either ENCTYPE_AES256_CTS_HMAC_SHA1_96 or
// ENCTYPE_AES256_CTS_HMAC_SHA1_96
function MakeKerberosKey(const PassPhrase, Salt: RawUtf8;
  EncType: integer = ENCTYPE_AES256_CTS_HMAC_SHA1_96;
  Iterations: integer = 0): RawByteString;

/// internal RFC 3961 derivation function - published only for testing
function Rfc3961Nfold(const input: RawByteString; olen: cardinal): RawByteString;

/// internal RFC 3962 derivation function - published only for testing
function Rfc3962SeedtoKey(const Seed: RawByteString; EncType: integer;
  DK: pointer = nil): RawByteString;

/// internal RFC 8009 derivation function - published only for testing
function Rfc8009SeedtoKey(const Seed: RawByteString; EncType: integer;
  const KdfLabel: RawByteString = 'kerberos'): RawByteString;

/// internal KeyTab derivation function - published only for testing
function MakeKerberosKeyEntry(var aEntry: TKerberosKeyEntry;
  const aPrincipal, aSalt: RawUtf8; const aPassword: SpiUtf8;
  aIsComputer: boolean; aEncType, aIterations: integer): boolean;

type
  /// Kerberos KeyTab file full read/write support
  // - the AddNew() method is able to generate a new key from credentials
  TKerberosKeyTabGenerator = class(TKerberosKeyTab)
  public
    /// generate and append a new key to the KeyTab entries
    // - returns true if was added, or false if it would have been duplicated
    // - aPrincipal is in the form 'user@my.lan' and will be normalized
    function AddNew(const aPrincipal: RawUtf8; const aPassword: SpiUtf8;
      aIsComputer: boolean = false; const aSalt: RawUtf8 = '';
      aEncType: integer = ENCTYPE_AES256_CTS_HMAC_SHA1_96;
      aIterations: integer = 0): boolean;
    /// compute a binary KeyTab with one entry with supplied credentials
    class function Generate(const aPrincipal: RawUtf8; const aPassword: SpiUtf8;
      aIsComputer: boolean = false; const aSalt: RawUtf8 = '';
      aEncType: integer = ENCTYPE_AES256_CTS_HMAC_SHA1_96): RawByteString;
  end;


/// compute the raw binary as stored in X.509 certificate from a ASN.1 public key
// - i.e. extract the ASN1_BITSTR raw section as encoded by X509PubKeyToDer()
// - will return the raw public key with no algorithm check whatsoever - consider
// calling SeqToEccPubKey() to parse for an expected ECC algorithm
function X509PubKeyFromDer(const PkcsDer: RawByteString): RawByteString;

/// return the number of bits of a X.509 certificate SubjectPublicKey ASN1_BITSTR
// - will recognize RSA ASN1_SEQ and ECC uncompressed keys as stored in X.509
// - returns typically 2048 for RSA, or 256 for ecc256r1
// - can format the key as its hexa members for ParsedToText(TX509Parsed)
function X509PubKeyBits(const SubjectPublicKey: RawByteString;
  PubText: PRawUtf8 = nil): integer;


/// create an ASN.1 block from some date/time value
// - according to X.509 profile, use UTCTime up to 2049 then GeneralizedTime
// - dt = 0 will be converted as '99991231235959Z' GeneralizedTime - could be
// used e.g. with X.509 NotAfter field when no good expiration date can be
// assigned (see RFC 5280 #4.1.2.5)
// - defined here to include mormot.core.datetime and mormot.core.text
function AsnTime(dt: TDateTime): TAsnObject;

/// parse the next ASN1_UTCTIME ASN1_GENTIME value as TDateTime
// - defined here to include mormot.core.datetime
function AsnNextTime(var Pos: integer; const Buffer: TAsnObject;
  out Value: TDateTime): boolean;

/// parse the next GeneralName value within a ASN1_SEQ
function AsnNextGeneralName(var Pos: integer; const Buffer: TAsnObject;
  var Name: RawUtf8): boolean;

/// decode an OID ASN.1 IP Address buffer into human-readable text
procedure AsnDecIp(p: PAnsiChar; len: integer; var text: RawUtf8);

/// decode Authority Information Access (1.3.6.1.5.5.7.1.1) extension content
function AsnDecAia(const ext: TAsnObject; var ocsp, issuers: TRawUtf8DynArray;
  nofilter: boolean = false): boolean;

/// decode Crl Distribution Points (2.5.29.31) extension content
function AsnDecCdp(const ext: TAsnObject; nofilter: boolean = false): TRawUtf8DynArray;

var
  /// abstract function placeholder to retrieve an AIA remote resource
  // - mormot.net.client would implement http:// and https:// protocols
  CryptRemoteResource: function(const Uri: RawUtf8; var Res: RawByteString): boolean;

/// serialize a TSecurityDescriptor instance into JSON
function SecurityDescriptorToJson(const SD: TSecurityDescriptor): RawUtf8;

/// unserialize a TSecurityDescriptor instance from JSON
function SecurityDescriptorFromJson(const Json: RawUtf8;
  out SD: TSecurityDescriptor): boolean;


{ **************** TSynMustache Cryptographic Expression Helpers }

type
  /// the published properties of this class implement cryptographic expression
  // helpers via TSynMustache.HelperAddMethods(helpers, TSynMustacheCryptoHelpers)
  // - could be registered individually using TSynMustache.HelperAdd()
  // - used e.g. by TMvcViewsMustache.RegisterExpressionHelpersForCrypto
  // - would allow e.g. to compute a Gravatar URI via:
  // ! <img src=http://www.gravatar.com/avatar/{{md5 email}}?s=200></img>
  TSynMustacheCryptoHelpers = class(TSynPersistent)
  published
    class procedure Md5(const Value: variant; out Result: variant);
    class procedure Sha1(const Value: variant; out Result: variant);
    class procedure Sha256(const Value: variant; out Result: variant);
    class procedure Sha512(const Value: variant; out Result: variant);
    class procedure PasswordGenerate(const Value: variant; out Result: variant);
  end;


implementation



{ **************** High-Level TSynSigner/TSynHasher Multi-Algorithm Wrappers }

{ TSynHasher }

const
  HASH_SHA3: array[hfSHA3_256 .. hfShake256] of TSha3Algo = (
    SHA3_256, SHA3_512, SHA3_256, SHA3_224, SHA3_384, SHAKE_128, SHAKE_256);

function TSynHasher.Init(aAlgo: THashAlgo): boolean;
begin
  fAlgo := aAlgo;
  result := true;
  case aAlgo of
    hfMD5:
      PMd5(@ctxt)^.Init;
    hfSHA1:
      PSha1(@ctxt)^.Init;
    hfSHA224:
      PSha256(@ctxt)^.Init224; // specific init vectors
    hfSHA256_128,
    hfSHA256_160,
    hfSHA256:
      PSha256(@ctxt)^.Init;
    hfSHA384:
      PSha384(@ctxt)^.Init;
    hfSHA512:
      PSha512(@ctxt)^.Init;
    hfSHA512_256:
      PSha512_256(@ctxt)^.Init;
    hfSHA3_256 .. hfSHA3_512,
    hfSHA3_224 .. hfShake256:
      PSha3(@ctxt)^.Init(HASH_SHA3[aAlgo]);
  else
    result := false;
  end;
end;

const
  HASH_INSTANCE: array[THashAlgo] of word = (
    SizeOf(TMd5),    SizeOf(TSha1),       SizeOf(TSha256), SizeOf(TSha384),
    SizeOf(TSha512), SizeOf(TSha512_256), SizeOf(TSha3),   SizeOf(TSha3),
    SizeOf(TSha256), SizeOf(TSha3),       SizeOf(TSha3),   SizeOf(TSha3),
    SizeOf(TSha3),   SizeOf(TSha256),     SizeOf(TSha256));

procedure TSynHasher.CopyTo(out aHasher: TSynHasher);
begin
  aHasher.fAlgo := fAlgo;
  MoveFast(ctxt, aHasher.ctxt, HASH_INSTANCE[fAlgo]); // copy what is needed
end;

procedure TSynHasher.Clear;
begin
  FillCharFast(ctxt, HASH_INSTANCE[fAlgo], 0); // only clear what is needed
end;

procedure TSynHasher.Update(aBuffer: pointer; aLen: integer);
begin
  if aLen > 0 then
    case fAlgo of
      hfMD5:
        PMd5(@ctxt)^.Update(aBuffer^, aLen);
      hfSHA1:
        PSha1(@ctxt)^.Update(aBuffer, aLen);
      hfSHA256_128,
      hfSHA256_160,
      hfSHA224,
      hfSHA256:
        PSha256(@ctxt)^.Update(aBuffer, aLen);
      hfSHA384:
        PSha384(@ctxt)^.Update(aBuffer, aLen);
      hfSHA512:
        PSha512(@ctxt)^.Update(aBuffer, aLen);
      hfSHA512_256:
        PSha512_256(@ctxt)^.Update(aBuffer, aLen);
    else
      PSha3(@ctxt)^.Update(aBuffer, aLen);
    end;
end;

procedure TSynHasher.UpdateUpper(aBuffer: pointer; aLen: PtrInt);
var
  up: TByteToAnsiChar; // normalize aBuffer content into uppercase ASCII
  l: PtrInt;
begin
  while aLen > 0 do // normalize on stack until all input buffer is hashed
  begin
    l := UpperCopy255Buf(@up, aBuffer, MinPtrInt(192, aLen)) - PAnsiChar(@up);
    Update(@up, l);
    inc(PByte(aBuffer), l);
    dec(aLen, l);
  end
end;

procedure TSynHasher.Update(const aBuffer: RawByteString);
begin
  Update(pointer(aBuffer), length(aBuffer));
end;

procedure TSynHasher.Update(const aBuffer: array of RawByteString);
var
  i: PtrInt;
begin
  for i := 0 to length(aBuffer) - 1 do
    Update(pointer(aBuffer[i]), length(aBuffer[i]));
end;

procedure TSynHasher.UpdateBigEndian(aValue: cardinal);
begin
  aValue := bswap32(aValue);
  Update(@aValue, 4);
end;

procedure TSynHasher.Final(var aResult: RawUtf8);
var
  dig: THash512Rec;
begin
  BinToHexLower(@dig, Final(dig), aResult);
  FillZero(dig.b);
end;

function TSynHasher.HashSize: integer;
begin
  result := HASH_SIZE[fAlgo];
end;

function TSynHasher.Final(out aDigest: THash512Rec; aNoInit: boolean): integer;
begin
  case fAlgo of
    hfMD5:
      PMd5(@ctxt)^.Final(aDigest.h0, aNoInit);
    hfSHA1:
      PSha1(@ctxt)^.Final(aDigest.b160, aNoInit);
    hfSHA224: // SHA-224 is just a truncated SHA-256 result with other Init
      begin
        PSha256(@ctxt)^.Final(aDigest.Lo, {aNoInit=}true);
        if not aNoInit then
          PSha256(@ctxt)^.Init224; // but it needs its own re-initialization
      end;
    hfSHA256_128, // plain SHA-256 truncated to 128/160-bit (no RFC existing)
    hfSHA256_160,
    hfSHA256:
      PSha256(@ctxt)^.Final(aDigest.Lo, aNoInit);
    hfSHA384:
      PSha384(@ctxt)^.Final(aDigest.b384, aNoInit);
    hfSHA512:
      PSha512(@ctxt)^.Final(aDigest.b, aNoInit);
    hfSHA512_256:
      PSha512_256(@ctxt)^.Final(aDigest.Lo, aNoInit);
  else
    PSha3(@ctxt)^.Final(@aDigest, 0, aNoInit);
  end;
  result := HASH_SIZE[fAlgo];
end;

procedure TSynHasher.FinalBin(var aResult: RawByteString; aLen: PtrInt);
var
  siz: PtrInt;
  p: PAnsiChar;
  dig: THash512Rec;
begin
  Final(dig);
  siz := HASH_SIZE[fAlgo];
  if aLen <= 0 then
    aLen := siz;
  p := FastNewRawByteString(aResult, aLen);
  while aLen > siz do
  begin
    MoveFast(dig, p^, siz); // duplicate hash until aLen is reached
    inc(p, siz);
    dec(aLen, siz);
  end;
  MoveFast(dig, p^, aLen);
end;

function TSynHasher.Full(aAlgo: THashAlgo; aBuffer: pointer; aLen: integer): RawUtf8;
begin
  FastAssignNew(result);
  if Init(aAlgo) then
  begin
    Update(aBuffer, aLen);
    Final(result);
  end;
end;

function TSynHasher.Full(aAlgo: THashAlgo; const aBuffer: RawByteString): RawUtf8;
begin
  Init(aAlgo);
  Update(aBuffer);
  Final(result);
end;

procedure TSynHasher.Full(aAlgo: THashAlgo;
  const aBuffer: array of RawByteString; var aResult: RawUtf8);
begin
  Init(aAlgo);
  Update(aBuffer);
  Final(aResult);
end;

function TSynHasher.Full(aAlgo: THashAlgo; aBuffer: pointer; aLen: integer;
  out aDigest: THash512Rec): integer;
begin
  Init(aAlgo);
  Update(aBuffer, aLen);
  result := Final(aDigest);
end;

function TSynHasher.Full(aAlgo: THashAlgo; const aBuffer: array of RawByteString;
  out aDigest: THash512Rec): integer;
begin
  Init(aAlgo);
  Update(aBuffer);
  result := Final(aDigest);
end;

function TSynHasher.Mgf1(aAlgo: THashAlgo;
  aSeed: pointer; aSeedLen, aDestLen: PtrUInt): RawByteString;
var
  dig: PHash512Rec;
  diglen, counter: cardinal;
begin
  FastAssignNew(result);
  if (aSeed = nil) or
     (aSeedLen <= 0) or
     (aDestLen <= 0) then
    exit;
  diglen := HASH_SIZE[aAlgo];
  SetLength(result, ((aDestLen div diglen) + 1) * diglen);
  dig := pointer(result);
  counter := 0;
  repeat
    Init(aAlgo);
    Update(aSeed, aSeedLen);
    UpdateBigEndian(counter);
    inc(PByte(dig), Final(dig^));
    inc(counter);
  until PtrUInt(dig) - PtrUInt(result) >= aDestLen;
  FakeLength(result, aDestLen);
end;

const
  // https://github.com/besser82/libxcrypt
  HASH64_CHARS: TChar64 =
   './0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
  HASH64_128: THash128 = (
    12, 6, 0, 13, 7, 1, 14, 8, 2, 15, 9, 3, 5, 10, 4, 11);
  // https://www.akkadia.org/drepper/SHA-crypt.txt
  HASH64_256: THash256 = (
    20, 10, 0, 11, 1, 21, 2, 22, 12, 23, 13, 3, 14, 4, 24,
    5, 25, 15, 26, 16, 6, 17, 7, 27, 8, 28, 18, 29, 19, 9, 30, 31);
  HASH64_512: THash512 = (
    42, 21, 0, 1, 43, 22, 23, 2, 44, 45, 24, 3, 4, 46, 25, 26, 5, 47,
    48, 27, 6, 7, 49, 28, 29, 8, 50, 51, 30, 9, 10, 52, 31, 32, 11, 53,
    54, 33, 12, 13, 55, 34, 35, 14, 56, 57, 36, 15, 16, 58, 37,
    38, 17, 59, 60, 39, 18, 19, 61, 40, 41, 20, 62, 63);

function b64enc(p: PUtf8Char; b, mask: PByteArray; n: cardinal): PUtf8Char;
var
  enc: PUtf8Char;
  c: PtrUInt;
begin
  enc := @HASH64_CHARS; // custom Base64uriEncode() with shuffled bytes
  repeat
    c := b[mask[0]] or (PtrUInt(b[mask[1]]) shl 8) or (PtrUInt(b[mask[2]]) shl 16);
    mask := @mask[3];
    p[0] := enc[c and $3f];
    p[1] := enc[(c shr 6) and $3f];
    p[2] := enc[(c shr 12) and $3f];
    p[3] := enc[c shr 18];
    p := @p[4];
    dec(n);
  until n = 0;
  result := p;
end;

procedure b64enclast(p: PUtf8Char; b1, b0, n: PtrUInt);
var
  enc: PUtf8Char;
begin
  inc(b0, b1 shl 8);
  enc := HASH64_CHARS;
  p[0] := enc[b0 and $3f];
  p[1] := enc[(b0 shr 6) and $3f];
  if n = SizeOf(THash256) then
    p[2] := enc[(b0 shr 12) and $3f]; // 3 trailing chars for SHA-256
end;

function b64append(var hash: RawUtf8; n: cardinal; pos: PInteger): pointer;
var
  i: PtrUInt;
begin
  i := length(hash);
  SetLength(hash, i + BinToBase64uriLength(n));
  if pos <> nil then
    pos^ := i + 1;
  result := PAnsiChar(pointer(hash)) + i;
end;

function b64valid(p: PUtf8Char): boolean;
begin
  result := false;
  if p = nil then
    exit;
  while true do
    case p^ of
      #0:
        break;
      '.', '/', '0'..'9', 'A'..'Z', 'a'..'z':
        inc(p); // valid HASH64_CHARS
    else
      exit;
    end;
  result := true;
end;

function TSynHasher.UnixCryptHash(aAlgo: THashAlgo; const aPassword: RawUtf8;
  aRounds, aSaltSize: cardinal; const aSalt: RawUtf8; aHashPos: PInteger): RawUtf8;
var
  p: PUtf8Char;
  c: AnsiChar;
  siz, n, aPasswordSize: PtrUInt;
  sbin, sb64, dp, ds: RawByteString;
  prepared: array of RawByteString;
  prep: PRawByteString;
  alt: THash512Rec;
begin
  FastAssignNew(result);
  if aAlgo <> hfMD5 then // fixed to 1000 rounds (sooo weak!) for MD5-CRYPT
    if aRounds = 0 then
      aRounds := MCF_ROUNDS[mcfSha256Crypt] // same default for mcfSha512Crypt
    else
      aRounds := MaxPtrUInt(1000, aRounds); // >= 1000
  case aAlgo of
    hfMD5:
      begin
        aRounds := 1000; // fixed
        aSaltSize := MinPtrUInt(8, aSaltSize); // lower range - default 8
        result := '$1$';
      end;
    hfSha256:
      Make(['$5$rounds=', aRounds, '$'], result);
    hfSha512:
      Make(['$6$rounds=', aRounds, '$'], result);
  else
    exit;
  end;
  aPasswordSize := length(aPassword);
  if aSalt = '' then
  begin
    if (aSaltSize = 0) or
       (aSaltSize > 16) then
      aSaltSize := 16; // for hfSha256/hfSha512
    TAesPrng.Main.RandomSalt(sbin, sb64, aSaltSize, '', @HASH64_CHARS, nil);
    FakeLength(sb64, aSaltSize); // aSaltSize is in base-64 chars, not bytes
    FillZero(sbin);
  end
  else
    sb64 := aSalt;
  aSaltSize := length(sb64);
  Append(result, sb64, '$');
  siz := Full(aAlgo, [aPassword, sb64, aPassword], alt);
  Init(aAlgo);
  Update(pointer(aPassword), aPasswordSize);
  if aAlgo = hfMD5 then
    Update('$1$');
  Update(pointer(sb64), aSaltSize);
  n := aPasswordSize;
  while n >= siz do
  begin
    Update(@alt, siz);
    dec(n, siz);
  end;
  Update(@alt, n);
  case aAlgo of
    hfMD5:
      begin
        n := aPasswordSize;
        while n > 0 do
        begin
          if (n and 1) <> 0 then
            c := #0
          else
            c := aPassword[1];
          Update(@c, 1); // weird
          n := n shr 1;
        end;
        Final(alt);
        dp := aPassword; // hash raw key/salt
        ds := sb64;
      end;
    hfSha256,
    hfSha512:
      begin
        n := aPasswordSize;
        while n > 0 do
        begin
          if (n and 1) <> 0 then
            Update(@alt, siz)
          else
            Update(pointer(aPassword), aPasswordSize);
          n := n shr 1;
        end;
        Final(alt);
        for n := 1 to aPasswordSize do // hash digest of key/salt
          Update(pointer(aPassword), aPasswordSize);
        FinalBin(dp, aPasswordSize);
        for n := 1 to 16 + alt.b[0] do
          Update(pointer(sb64), aSaltSize);
        FinalBin(ds, aSaltSize);
      end;
  end;
  SetLength(prepared, 42); // pre-compute all possible rounds combinations
  prep := pointer(prepared);
  for n := 0 to 41 do
  begin
    if (n and 1) <> 0 then // add key or last result
      Append(prep^, pointer(dp), aPasswordSize);
    if (n mod 3) <> 0 then // add salt for numbers not divisible by 3
      Append(prep^, pointer(ds), aSaltSize);
    if (n mod 7) <> 0 then // add key for numbers not divisible by 7
      Append(prep^, pointer(dp), aPasswordSize);
    if (n and 1) = 0 then  // add key or last result
      Append(prep^, pointer(dp), aPasswordSize);
    inc(prep);             // will avoid some memory copy in the main loop
  end;
  for n := 0 to aRounds - 1 do
  begin
    if (n and 1) = 0 then       // add last result first every even round
      Update(@alt, siz);
    Update(prepared[n mod 42]);
    if (n and 1) <> 0 then      // add last result last every odd round
      Update(@alt, siz);
    Final(alt);
  end;
  p := b64append(result, siz, aHashPos);
  case siz of // shuffled Base64uriEncode() with custom HASH64_CHARS
    SizeOf(HASH64_128):
      b64enclast(b64enc(p, @alt.b, @HASH64_128, 5), 0, alt.b[11], siz);
    SizeOf(HASH64_256):
      b64enclast(b64enc(p, @alt.b, @HASH64_256, 10), alt.b[31], alt.b[30], siz);
    SizeOf(HASH64_512):
      b64enclast(b64enc(p, @alt.b, @HASH64_512, 21), 0, alt.b[63], siz);
  end;
  FillZero(alt.b);
  FillZero(dp);
  FillZero(ds);
end;


{ TStreamRedirectSynHasher }

constructor TStreamRedirectSynHasher.Create(aDestination: TStream; aRead: boolean);
begin
  inherited Create(aDestination, aRead);
  fHash.Init(GetAlgo);
  fHashAppend := fHash; // save initial state for ResetHash
end;

procedure TStreamRedirectSynHasher.AfterAppend;
begin
  fHashAppend := fHash; // save appended state for ResetHash
end;

procedure TStreamRedirectSynHasher.DoHash(data: pointer; len: integer);
begin
  fHash.Update(data, len);
end;

procedure TStreamRedirectSynHasher.ResetHash;
begin
  fHash := fHashAppend; // restore after e.g. Seek(0, soBeginning)
end;

function TStreamRedirectSynHasher.GetHash: RawUtf8;
var
  tmp: TSynHasher; // local copy to return current state but continue processing
begin
  tmp := fHash;
  tmp.Final(result);
end;

class function TStreamRedirectSynHasher.GetHashFileExt: RawUtf8;
begin
  result := HASH_EXT[GetAlgo];
end;

function TStreamRedirectSynHasher.GetHashDigest(out Digest: THashDigest): boolean;
var
  tmp: TSynHasher;
begin
  Digest.Algo := GetAlgo;
  tmp := fHash;
  tmp.Final(Digest.Bin); // hash the current state
  result := true;
end;

class function TStreamRedirectSynHasher.GetHashDigest(const HexaHash: RawUtf8;
  out Digest: THashDigest): boolean;
begin
  Digest.Algo := GetAlgo;
  result := mormot.core.text.HexToBin(pointer(HexaHash), @Digest.Bin, HASH_SIZE[Digest.Algo]);
end;

{ TStreamRedirect* inherited classes }

class function TStreamRedirectShake256.GetAlgo: THashAlgo;
begin
  result := hfShake256;
end;

class function TStreamRedirectShake128.GetAlgo: THashAlgo;
begin
  result := hfShake128;
end;

class function TStreamRedirectSha3_512.GetAlgo: THashAlgo;
begin
  result := hfSHA3_512;
end;

class function TStreamRedirectSha3_224.GetAlgo: THashAlgo;
begin
  result := hfSHA3_224;
end;

class function TStreamRedirectSha3_256.GetAlgo: THashAlgo;
begin
  result := hfSHA3_256;
end;

class function TStreamRedirectSha3_384.GetAlgo: THashAlgo;
begin
  result := hfSHA3_384;
end;

class function TStreamRedirectSha512.GetAlgo: THashAlgo;
begin
  result := hfSHA512;
end;

class function TStreamRedirectSha512_256.GetAlgo: THashAlgo;
begin
  result := hfSHA512_256;
end;

class function TStreamRedirectSha384.GetAlgo: THashAlgo;
begin
  result := hfSHA384;
end;

class function TStreamRedirectSha256.GetAlgo: THashAlgo;
begin
  result := hfSHA256;
end;

class function TStreamRedirectSha256_128.GetAlgo: THashAlgo;
begin
  result := hfSHA256_128;
end;

class function TStreamRedirectSha256_160.GetAlgo: THashAlgo;
begin
  result := hfSHA256_160;
end;

class function TStreamRedirectSha224.GetAlgo: THashAlgo;
begin
  result := hfSHA224;
end;

class function TStreamRedirectSha1.GetAlgo: THashAlgo;
begin
  result := hfSHA1;
end;

class function TStreamRedirectMd5.GetAlgo: THashAlgo;
begin
  result := hfMD5;
end;

function md5hash32(crc: cardinal; buffer: pointer; len: cardinal): cardinal;
var
  md5: TMd5;
  dig: THash128Rec;
begin
  md5.Init;
  md5.Update(crc, SizeOf(crc));
  md5.Update(buffer^, len);
  md5.Final(dig.b);
  result := dig.c0 xor dig.c1 xor dig.c2 xor dig.c3;
end;

function sha1hash32(crc: cardinal; buffer: pointer; len: cardinal): cardinal;
var
  sha: TSha1;
  dig: THash256Rec;
begin
  sha.Init;
  sha.Update(@crc, SizeOf(crc));
  sha.Update(buffer, len);
  sha.Final(dig.sha1);
  result := dig.c[0] xor dig.c[1] xor dig.c[2] xor dig.c[3] xor dig.c[4];
end;

function sha256hash32(crc: cardinal; buffer: pointer; len: cardinal): cardinal;
var
  sha: TSha256;
  dig: THash256Rec;
begin
  sha.Init;
  sha.Update(@crc, SizeOf(crc));
  sha.Update(buffer, len);
  sha.Final(dig.b); // dig.c[0] would have been enough anyway with a crypto hash
  result := dig.c[0] xor dig.c[1] xor dig.c[2] xor dig.c[3] xor
            dig.c[4] xor dig.c[5] xor dig.c[6] xor dig.c[7];
end;

function CryptCrc32(algo: TCrc32Algo): THasher;
begin
  case algo of
    caCrc32c:
      result := crc32c;
    caCrc32:
      result := crc32;
    caAdler32:
      result := adler32; // from mormot.lib.z - nil if unit was not included
    caxxHash32:
      result := @xxHash32;
    caFnv32:
      result := @fnv32;
    caDefault:
      result := DefaultHasher; // may use AES-NI with process-specific seed
    caMd5:
      result := @md5hash32;
    caSha1:
      result := @sha1hash32;   // may use Intel SHA HW opcodes
    caSha256:
      result := @sha256hash32; // may use Intel SHA HW opcodes
  else
    result := nil;
  end;
end;


function HashFull(aAlgo: THashAlgo; aBuffer: pointer; aLen: integer): RawUtf8;
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

function HashFileRaw(const aFileName: TFileName; aAlgos: THashAlgos;
  aFileSize: PInt64): TRawUtf8DynArray;
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
  if aFileSize <> nil then
    aFileSize^ := 0;
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
    if aFileSize <> nil then
      aFileSize^ := size;
    tempsize := 1 shl 20; // 1MB temporary buffer for reading seems good enough
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
    SetLength(result, n + 1); // don't return any partial hash result
    for h := 0 to n do
      hasher[h].Final(result[h]);
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
    FastAssignNew(result)
  else
    result := h[0];
end;

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
      fn := FormatString('%%', [efn, HASH_EXT[a]]);
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

function HashFileSha224(const FileName: TFileName): RawUtf8;
begin
  result := HashFile(FileName, hfSHA224);
end;

function HashFileSha384(const FileName: TFileName): RawUtf8;
begin
  result := HashFile(FileName, hfSHA384);
end;

function HashFileSha512(const FileName: TFileName): RawUtf8;
begin
  result := HashFile(FileName, hfSHA512);
end;

function HashFileSha512_256(const FileName: TFileName): RawUtf8;
begin
  result := HashFile(FileName, hfSHA512_256);
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

const
  BLOCK_SIZE: array[TSignAlgo] of byte = (
    15, 15, 31, 31, 0, 0, 0, 0, 0, 0, 15);

procedure TSynSigner.Init(aAlgo: TSignAlgo; aSecret: pointer; aSecretLen: integer);
var
  k0: TBlock1024;
  a: THashAlgo;
begin
  fAlgo := aAlgo;
  a := SIGN_HASH[Algo];
  fSignatureSize := SIGN_SIZE[fAlgo];
  fBlockMax := BLOCK_SIZE[fAlgo]; // typically 15 (256-bit) or 31 (512-bit)
  fBlockSize := (fBlockMax + 1) shl 2;
  if fBlockMax = 0 then
  begin // we estimate that the HMAC pattern is part of the SHA-3 sponge design
    fHasher.Init(a);
    fHasher.Update(aSecret, aSecretLen);
    exit;
  end;
  FillCharFast(k0, fBlockSize, 0);
  if aSecretLen > fBlockSize then
    fHasher.Full(a, aSecret, aSecretLen, PHash512Rec(@k0)^)
  else
    MoveFast(aSecret^, k0, aSecretLen);
  Xor32By128(@fStep7data, @k0, fBlockMax, $5c5c5c5c);
  Xor32By128(@k0, @k0, fBlockMax, $36363636);
  fHasher.Init(a);
  fHasher.Update(@k0, fBlockSize);
  FillCharFast(k0, fBlockSize, 0);
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
    FillZero(temp.b);
    Pbkdf2(aAlgo, aSecret, aSalt, aSecretPbkdf2Round, @temp);
    Init(aAlgo, @temp, fSignatureSize);
    if aPbkdf2Secret <> nil then
      aPbkdf2Secret^ := temp;
    FillZero(temp.b);
  end
  else
    Init(aAlgo, aSecret);
end;

procedure TSynSigner.Update(const aBuffer: RawByteString);
begin
  fHasher.Update(pointer(aBuffer), length(aBuffer));
end;

procedure TSynSigner.Update(aBuffer: pointer; aLen: integer);
begin
  fHasher.Update(aBuffer, aLen);
end;

procedure TSynSigner.UpdateBigEndian(aValue: cardinal);
begin
  fHasher.UpdateBigEndian(aValue);
end;

function TSynSigner.Final(aSignature: PHash512Rec; aNoInit: boolean): integer;
begin
  result := fHasher.Final(aSignature^);
  if fBlockMax = 0 then
    exit; // SHA-3 needs no HMAC
  fHasher.Update(@fStep7data, fBlockSize);
  fHasher.Update(aSignature, fSignatureSize);
  fHasher.Final(aSignature^, aNoInit);
  if not aNoInit then
    FillCharFast(fStep7data, fBlockSize, 0);
end;

function TSynSigner.Final: RawUtf8;
var
  sig: THash512Rec;
begin
  Final(@sig);
  result := BinToHexLower(@sig, fSignatureSize);
end;

function TSynSigner.Full(aAlgo: TSignAlgo; const aSecret: RawUtf8;
  aBuffer: pointer; aLen: integer): RawUtf8;
begin
  Init(aAlgo, aSecret);
  Update(aBuffer, aLen);
  result := Final;
end;

function TSynSigner.Full(aAlgo: TSignAlgo; const aSecret, aSalt: RawUtf8;
  aSecretPbkdf2Round: integer; aBuffer: pointer; aLen: integer): RawUtf8;
begin
  Init(aAlgo, aSecret, aSalt, aSecretPbkdf2Round);
  Update(aBuffer, aLen);
  result := Final;
end;

function TSynSigner.Full(aAlgo: TSignAlgo; aSecret: pointer; aSecretLen: PtrInt;
  const aMessage: RawByteString; aHmac: PHash512Rec): integer;
begin
  Init(aAlgo, aSecret, aSecretLen);
  Update(aMessage);
  result := Final(aHMac);
end;

function TSynSigner.Hash(aAlgo: TSignAlgo; aBuffer: pointer; aLen: integer;
  out aDigest: THash512Rec): integer;
begin
  result := fHasher.Full(SIGN_HASH[aAlgo], aBuffer, aLen, aDigest);
end;

function TSynSigner.Pbkdf2(aAlgo: TSignAlgo; const aSecret, aSalt: RawUtf8;
  aSecretPbkdf2Round: integer; aDerivatedKey: PHash512Rec; aPartNumber: integer): PtrInt;
var
  bak: TSynHasher;
  tmp: THash512Rec;
begin
  Init(aAlgo, aSecret); // = PRF(secret)
  dec(aSecretPbkdf2Round);
  if aSecretPbkdf2Round <> 0 then
    fHasher.CopyTo(bak); // save initial PRF(secret) state
  Update(aSalt);
  if not (fAlgo in SIGNER_SHA3) then // padding + XOF mode are part of SHA-3
    // U1 = PRF(secret, salt + INT_32_BE(part))
    UpdateBigEndian(aPartNumber);  // is a 1-based index
  Final(aDerivatedKey, {noinit=}true);
  if aSecretPbkdf2Round <> 0 then
  begin
    // F(secret, salt, c, i) = U1 ^ U2 ^ .. ^ Uc  with Uc = PRF(secret, Uc-1)
    MoveFast(aDerivatedKey^, tmp, fSignatureSize);
    repeat
      MoveFast({%H-}bak.ctxt, fHasher.ctxt, HASH_INSTANCE[fHasher.fAlgo]); // restore
      Update(@tmp, fSignatureSize);
      Final(@tmp, {noinit=}true);
      XorMemory(pointer(aDerivatedKey), @tmp, fSignatureSize);
      dec(aSecretPbkdf2Round);
    until aSecretPbkdf2Round = 0;
    bak.Clear;
    FillZero(tmp.b);
  end;
  Done;
  result := fSignatureSize;
end;

procedure TSynSigner.Pbkdf2(const aParams: TSynSignerParams;
  out aDerivatedKey: THash512Rec);
begin
  Pbkdf2(aParams.algo, aParams.secret, aParams.salt, aParams.rounds, @aDerivatedKey);
end;

procedure TSynSigner.Pbkdf2(aParamsJson: PUtf8Char; aParamsJsonLen: integer;
  out aDerivatedKey: THash512Rec; const aDefaultSalt: RawUtf8;
  aDefaultAlgo: TSignAlgo);
var
  k: TSynSignerParams;
  tmp: TSynTempBuffer;

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
  else if GotoNextNotSpace(aParamsJson)^ <> '{' then
    FastSetString(k.secret, aParamsJson, aParamsJsonLen)
  else
  begin
    tmp.Init(aParamsJson, aParamsJsonLen);
    try
      if (RecordLoadJsonInPlace(k, tmp.buf, TypeInfo(TSynSignerParams)) = nil) or
         (ord(k.algo) > ord(high(k.algo))) or
         (k.secret = '') or
         (k.salt = '') then
      begin
        SetDefault;
        FastSetString(k.secret, aParamsJson, aParamsJsonLen);
      end;
    finally
      FillCharFast(tmp.buf^, tmp.len, 0); // anti-forensic
      tmp.Done;
    end;
  end;
  Pbkdf2(k.algo, k.secret, k.salt, k.rounds, @aDerivatedKey);
  FillZero(k.secret);
end;

procedure TSynSigner.Pbkdf2(const aParamsJson: RawUtf8;
  out aDerivatedKey: THash512Rec; const aDefaultSalt: RawUtf8;
  aDefaultAlgo: TSignAlgo);
begin
  Pbkdf2(pointer(aParamsJson), length(aParamsJson),
    aDerivatedKey, aDefaultSalt, aDefaultAlgo);
end;

function TSynSigner.Pbkdf2(aAlgo: TSignAlgo; const aSecret, aSalt: RawUtf8;
  aSecretPbkdf2Round, aDestLen: PtrUInt): RawByteString;
var
  hlen, l, r, part: cardinal;
  p: PHash512Rec;
begin
  // see https://www.rfc-editor.org/rfc/rfc2898#section-5.2
  FastAssignNew(result);
  if (aSecret = '') or
     (aSecretPbkdf2Round = 0) or
     (aSecretPbkdf2Round > 1 shl 20) or
     (aDestLen = 0) or
     (aDestLen > 1 shl 20) then
    exit;
  hlen := SIGN_SIZE[aAlgo];
  l := aDestLen div hlen;
  r := aDestLen - (l * hlen); // mod
  if r <> 0 then
    inc(l); // ceil()
  if (aAlgo in SIGNER_SHA3) and
     (l > 1) then
    ESynCrypto.RaiseUtf8('TSynSigner.Pbkdf2(%) with DestLen=%: use SHAKE instead',
      [ToText(aAlgo)^, aDestLen]);
  // DK = T1 + T2 + .. + Tl with Ti = F(secret, salt, round, part)
  p := FastNewString(l * hlen); // pre-allocate destination buffer
  pointer(result) := p;
  for part := 1 to l do
  begin
    Pbkdf2(aAlgo, aSecret, aSalt, aSecretPbkdf2Round, p, part);
    inc(PByte(p), hlen); // just concatenate each Ti
  end;
  if r <> 0 then
    FakeLength(result, aDestLen); // truncate to the expected destination size
end;

const
  MCF_IDENT: array[mcfMd5Crypt.. high(TModularCryptFormat)] of RawUtf8 = (
    '1', '5', '6', 'pbkdf2', 'pbkdf2-sha256', 'pbkdf2-sha512', 'pbkdf2-sha3',
    '2b', 'bcrypt-sha256', 'scrypt');
  MCF_SIGN: array[mcfPbkdf2Sha1 .. mcfPbkdf2Sha3] of TSignAlgo = (
    saSHA1, saSHA256, saSHA512, saSha3512);
  HASH64_ENC: TChar64 = // the current encoding used by "$pbkdf2" passlib
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789./';
var
  HASH64_DEC: TAnsiCharDec;

function TSynSigner.Pbkdf2ModularCrypt(aAlgo: TModularCryptFormat;
  const aPassword: RawUtf8; aRounds, aSaltSize: cardinal;
  const aSalt: RawUtf8; aHashPos: PInteger): RawUtf8;
var
  siz: PtrUInt;
  bin, b64: RawByteString;
  dig: THash512;
begin
  FastAssignNew(result);
  if (aPassword = '') or
     not (aAlgo in [low(MCF_SIGN) .. high(MCF_SIGN)]) then
    exit;
  if aRounds = 0 then
    aRounds := MCF_ROUNDS[aAlgo] // use default of each algorithm
  else if aRounds > MAX_PBKDF2_ROUNDS then
    exit; // avoid naive DoS attacks
  if aSaltSize = 0 then
    aSaltSize := 16;
  if HASH64_DEC[#255] = 0 then // check the last byte for thread-safe init
    FillBaseDecoder(@HASH64_ENC, @HASH64_DEC);
  if not TAesPrng.Main.RandomSalt(bin, b64, aSaltSize, aSalt, @HASH64_ENC, @HASH64_DEC) then
    exit;
  Make(['$', MCF_IDENT[aAlgo], '$', aRounds, '$', b64, '$'], result);
  siz := Pbkdf2(MCF_SIGN[aAlgo], aPassword, bin, aRounds, @dig);
  Base64uriEncode(b64append(result, siz, aHashPos), @dig, siz, @HASH64_ENC);
  FillZero(bin);
  FillZero(b64);
end;

function TSynSigner.KdfSP800(aAlgo: TSignAlgo; aDestLen: cardinal;
  const aKey, aLabel, aContext: RawByteString): RawByteString;
var
  dig: PHash512Rec;
  diglen, counter: cardinal;
begin
  FastAssignNew(result);
  if (aKey = '') or
     (aLabel = '') or
     (aDestLen = 0) then
    exit;
  diglen := SIGN_SIZE[aAlgo];
  dig := FastNewRawByteString(result, ((aDestLen div diglen) + 1) * diglen);
  counter := 1;
  repeat
    Init(aAlgo, aKey);
    // Ki = HMAC-SHA-###(key, i | label | 0 | context | bits)
    UpdateBigEndian(counter);
    Update(pointer(aLabel), length(aLabel) + 1); // include ending #0
    Update(aContext);
    UpdateBigEndian(aDestLen shl 3); // bits
    Final(dig);
    inc(PByte(dig), diglen);
    inc(counter);
  until PtrUInt(dig) - PtrUInt(result) >= aDestLen;
  FakeLength(result, aDestLen); // k-truncate
end;

procedure TSynSigner.AssignTo(var aDerivatedKey: THash512Rec;
  out aAes: TAes; aEncrypt: boolean);
var
  ks: integer;
begin
  case fAlgo of
    saSha3S128:
      ks := 128; // truncate to Keccak sponge precision
    saSha3S256:
      ks := 256;
  else
    case SignatureSize of
      SizeOf(THash160): // e.g. SHA-1
        begin
          ks := 128;
          aDerivatedKey.i0 := aDerivatedKey.i0 xor aDerivatedKey.i4;
        end;
      SizeOf(THash224):
        ks := 192;
      SizeOf(THash256):
        ks := 256;
      SizeOf(THash384):
        begin
          ks := 256;
          aDerivatedKey.d0 := aDerivatedKey.d0 xor aDerivatedKey.d4;
          aDerivatedKey.d1 := aDerivatedKey.d1 xor aDerivatedKey.d5;
        end;
      SizeOf(THash512):
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
  FillCharFast(fHasher.ctxt, HASH_INSTANCE[fHasher.fAlgo], 0);
  FillCharFast(fStep7data, fBlockSize, 0);
end;


function ToText(algo: TSignAlgo): PShortString;
begin
  result := GetEnumName(TypeInfo(TSignAlgo), ord(algo));
end;

function ToUtf8(algo: TSignAlgo): RawUtf8;
begin
  result := SIGNER_TXT[algo];
end;

function ToText(algo: THashAlgo): PShortString;
begin
  result := GetEnumName(TypeInfo(THashAlgo), ord(algo));
end;

function ToUtf8(algo: THashAlgo): RawUtf8;
begin
  result := HASH_TXT[algo];
end;

function ToText(algo: TCrc32Algo): PShortString;
begin
  result := GetEnumName(TypeInfo(TCrc32Algo), ord(algo));
end;

function ToText(const Digest: THashDigest): RawUtf8;
begin
  if Digest.Algo <= high(THashAlgo) then // don't include the algorithm
    BinToHexLower(PAnsiChar(@Digest.Bin), HASH_SIZE[Digest.Algo], result)
  else
    FastAssignNew(result);
end;

function ToText(fmt: TModularCryptFormat): PShortString; overload;
begin
  result := GetEnumName(TypeInfo(TModularCryptFormat), ord(fmt));
end;

function SanitizeAlgoName(P: PUtf8Char; L: PtrInt; var tmp: TShort15;
  trimprefix: cardinal; onlyalphanum: boolean): boolean;
begin
  tmp[0] := #0;
  result := false;
  if (L < 3) or
     (L > 20) then
    exit;
  if PWord(P)^ = trimprefix then
    inc(P, 2); // recognize plain un-trimmed ToText() e.g. 'hfMD5'
  repeat
    case P^ of
      #0:
        break;
      'A' .. 'Z', '0' .. '9', 'a' .. 'z':
        AppendShortChar(P^, @tmp);
      '_':
        if not onlyalphanum then
          AppendShortChar('_', @tmp);
      '-', '/':
        if not onlyalphanum then
          if ((tmp[0] = #4) and // '.sha3-256' -> 'sha3_256'
              (PCardinal(@tmp[1])^ and $ffdfdfdf = SHA_HI + ord('3') shl 24)) or
             ((tmp[0] = #6) and // '.sha512-256' or '.sha256-160'
              (PCardinal(@tmp[1])^ and $80dfdfdf = SHA_HI)) then
            AppendShortChar('_', @tmp);
    end;
    inc(P);
  until tmp[0] > #10;
  result := tmp[0] in [#3 .. #10];
end;

function TextToSignAlgo(const Text: RawUtf8; out Algo: TSignAlgo): boolean;
begin
  result := TextToSignAlgo(pointer(Text), length(Text), Algo);
end;

function TextToSignAlgo(P: PUtf8Char; Len: PtrInt; out Algo: TSignAlgo): boolean;
var
  tmp: TShort15;
  i: integer;
begin
  result := false;
  if not SanitizeAlgoName(P, Len, tmp, ord('s') + ord('a') shl 8, true) then
    exit;
  i := GetEnumNameValueTrimmed(TypeInfo(TSignAlgo), @tmp[1], ord(tmp[0]));
  if i >= 0 then
    Algo := TSignAlgo(i)
  else if IdemPropName(tmp, 'SHAKE128') then
    Algo := saSha3S128
  else if IdemPropName(tmp, 'SHAKE256') then
    Algo := saSha3S256
  else
    exit;
  result := true;
end;

function TextToHashAlgo(const Text: RawUtf8; out Algo: THashAlgo): boolean;
begin
  result := TextToHashAlgo(pointer(Text), length(Text), Algo);
end;

function TextToHashAlgo(P: PUtf8Char; Len: PtrInt; out Algo: THashAlgo): boolean;
var
  tmp: TShort15;
  i: integer;
begin
  result := false;
  if not SanitizeAlgoName(P, Len, tmp, ord('h') + ord('f') shl 8, false) then
    exit;
  i := GetEnumNameValueTrimmed(TypeInfo(THashAlgo), @tmp[1], ord(tmp[0]));
  if i < 0 then
    exit;
  Algo := THashAlgo(i);
  result := true;
end;

function HashDetect(const Hash: RawUtf8; out Digest: THashDigest): boolean;
var
  s: byte;
  a: THashAlgo;
begin
  result := false;
  FillCharFast(Digest, SizeOf(Digest), 0);
  s := length(Hash) shr 1;
  if (s >= SizeOf(TMd5Digest)) and
     (s <= SizeOf(Digest.Bin)) then
    for a := low(a) to high(a) do // would miss SHA-3 or SHA-512-256 for sure
      if HASH_SIZE[a] = s then
      begin
        Digest.Algo := a;
        result := mormot.core.text.HexToBin(pointer(Hash), @Digest.Bin, s);
        break;
      end;
end;

function HashDigestEqual(a, b: PPtrIntArray): boolean;
begin
  result := (a[0] = b[0]) and // compare first 4/8 bytes (much faster in loops)
            (PHashAlgo(a)^ <= high(THashAlgo)) and // avoid buffer overflow
     (MemCmp(@a[1], @b[1], HASH_SIZE[PHashAlgo(a)^] - (SizeOf(a[0]) - 1)) = 0);
end;

procedure FillZero(var Digest: THashDigest);
begin
  if Digest.Algo <= high(THashAlgo) then // avoid buffer overflow
    FillZero(Digest.Bin, HASH_SIZE[Digest.Algo]);
  Digest.Algo := low(Digest.Algo);
end;


{ **************** HMAC/PBKDF2 MCF SCRAM SCRAM-MCF TOTP High-Level Protocols}

{ some HMAC/PBKDF2 common wrappers defined here to redirect to TSynSigner }

procedure Hmac(algo: TSignAlgo; key, msg: pointer; keylen, msglen: integer;
  result: PHash512Rec);
var
  signer: TSynSigner;
begin
  signer.Init(algo, key, keylen);
  signer.Update(msg, msglen);
  signer.Final(result);
end;

function Pbkdf2(algo: TSignAlgo; const password, salt: RawByteString;
  count: integer; digest: PHash512Rec): integer;
var
  signer: TSynSigner;
begin
  result := signer.Pbkdf2(algo, password, salt, count, digest);
end;

function Pbkdf2(algo: TSignAlgo; const password, salt: RawByteString;
  count, destlen: integer): RawByteString;
var
  signer: TSynSigner;
begin
  result := signer.Pbkdf2(algo, password, salt, count, destlen);
end;

procedure HmacSha1(const key, msg: RawByteString;
  out result: TSha1Digest);
begin
  Hmac(saSha1, pointer(key), pointer(msg), length(key), length(msg), @result);
end;

procedure HmacSha1(const key: TSha1Digest; const msg: RawByteString;
  out result: TSha1Digest);
begin
  Hmac(saSha1, @key, pointer(msg), SizeOf(key), length(msg), @result);
end;

procedure Pbkdf2HmacSha1(const password, salt: RawByteString;
  count: integer; out result: TSha1Digest);
begin
  Pbkdf2(saSha1, password, salt, count, @result);
end;

procedure HmacSha384(const key, msg: RawByteString;
  out result: TSha384Digest);
begin
  Hmac(saSha384, pointer(key), pointer(msg), length(key), length(msg), @result);
end;

procedure HmacSha384(const key: TSha384Digest; const msg: RawByteString;
  out result: TSha384Digest);
begin
  Hmac(saSha384, @key, pointer(msg), SizeOf(key), length(msg), @result);
end;

procedure Pbkdf2HmacSha384(const password, salt: RawByteString;
  count: integer; out result: TSha384Digest);
begin
  Pbkdf2(saSha384, password, salt, count, @result);
end;

procedure HmacSha512(const key, msg: RawByteString;
  out result: TSha512Digest);
begin
  Hmac(saSha512, pointer(key), pointer(msg), length(key), length(msg), @result);
end;

procedure HmacSha512(const key: TSha512Digest; const msg: RawByteString;
  out result: TSha512Digest);
begin
  Hmac(saSha512, @key, pointer(msg), SizeOf(key), length(msg), @result);
end;

procedure Pbkdf2HmacSha512(const password, salt: RawByteString;
  count: integer; out result: TSha512Digest);
begin
  Pbkdf2(saSha512, password, salt, count, @result);
end;

{ Extended "Modular Crypt" support }

function ModularCryptParse(var P: PUtf8Char; var rounds: cardinal;
  var salt: RawUtf8): TModularCryptFormat;
var
  sLogN, sR, sP: cardinal;
begin
  result := mcfInvalid;
  if (P = nil) or
     not (P^ in ['$', '#']) then // allow # prefix for SCRAM-like auth
    exit;
  inc(P);
  GetNextItem(P, '$', salt);
  if salt = '' then
    exit;
  case GetCardinal(pointer(salt)) of
    1:
      result := mcfMd5Crypt;
    2: // $2a$ $2b$ $2x$ $2y$ $2z$ ...
      result := mcfBCrypt;
    5:
      result := mcfSha256Crypt;
    6:
      result := mcfSha512Crypt;
    7:
      result := mcfSCrypt; // as in Unix crypt utility (but not identical)
  else
    result := TModularCryptFormat(FindNonVoidRawUtf8(@MCF_IDENT,
      pointer(salt), length(salt), length(MCF_IDENT)) + ord(low(MCF_IDENT)));
  end;
  case result of
    mcfMd5Crypt:      // '$1${salt}${checksum}'
      rounds := 1000; // fixed
    mcfSha256Crypt .. mcfSha512Crypt:
      begin // '$5$rounds={rounds}${salt}${checksum}'
        if IdemPChar(P, 'ROUNDS=') then
          begin
            inc(P, 7);
            rounds := GetNextItemCardinal(P, '$');
          end
          else
            rounds := 5000; // default for SHA-256 Crypt and SHA-512 Crypt
      end;
    mcfPbkdf2Sha1 .. mcfBCrypt:
      begin // '$pbkdf2{-digest}${rounds}${salt}${checksum}'
        rounds := GetNextItemCardinal(P, '$');
        if rounds = 0 then
        begin
          result := mcfInvalid;
          exit;
        end;
      end; // for mcfBCrypt: '$2a$rounds$saltchecksum' - rounds = cost (4..31)
    mcfBCryptSha256:
      begin // '$bcrypt-sha256$v=2,t=2b,r=12$n79VH.0Q2TMWmt3Oqt9uk...'
        result := mcfInvalid;
        if not IdemPChar(P, 'V=2,T=2') then // version 1 without HMAC was unsafe
          exit;
        inc(P, 7);
        if P^ <> ',' then
          if P^ in ['a'..'z'] then
            inc(P)
          else
            exit;
        if not IdemPChar(P, ',R=') then
          exit;
        inc(P, 3);
        rounds := GetNextItemCardinal(P, '$');
        if not (rounds in [4 .. 31]) then
          exit;
        result := mcfBCryptSha256;
      end;
    mcfSCrypt:
      begin // '$scrypt$ln=<log2(N)>,r=<r>,p=<p>$salt$<checksum>'
        result := mcfInvalid;
        if not IdemPChar(P, 'LN=') then
          exit;
        inc(P, 3);
        sLogN := GetNextItemCardinal(P, ','); // logN in [1..31] - default 16
        if (sLogN = 0) or
           (sLogN > 31) or
           not IdemPChar(P, 'R=') then
          exit;
        inc(P, 2);
        sR := GetNextItemCardinal(P, ',');     // R in [1..16384] - default 8
        if (sR = 0) or
           (sR > 16384) or
           not IdemPChar(P, 'P=') then
          exit;
        inc(P, 2);
        sP := GetNextItemCardinal(P, '$');     // P in [1..8191] - default 2
        if (sP = 0) or
           (sP > 8192) then
          exit;
        // rounds := <logN:5-bit:1..31><R:14-bit:1..16384><P:13-bit:1..8192>
        rounds := SCryptRounds(sLogN, sR, sP);
        GetNextItem(P, '$', salt);
        if not Base64uriValid(pointer(salt), @ConvertBase64ToBin) then
          exit;
        result := mcfSCrypt;
        exit; // $scrypt$ charset is standard base64 and not passlib b64valid()
      end;
  else
    begin
      result := mcfUnknown;
      exit;
    end;
  end;
  if result = mcfBCrypt then // mcfBCrypt: '$2a$rounds$saltchecksum'
  begin
    FastSetString(salt, P, 22); // fixed 22 chars salt
    inc(P, 22);                 // fixed 31 chars checksum (may be > with OPRF)
  end
  else
    GetNextItem(P, '$', salt);
  if not b64valid(pointer(salt)) then
    result := mcfInvalid;
end; // on success, P^ points to the {checksum} part - its encoding is unchecked

function ModularCryptIdentify(const hash: RawUtf8; info: PRawUtf8): TModularCryptFormat;
var
  dummyrounds: cardinal;
  dummysalt: RawUtf8;
  P: PUtf8Char;
begin
  P := pointer(hash);
  result := ModularCryptParse(P, dummyrounds, dummysalt);
  if (info <> nil) and
     (result in mcfValid) and
     (P <> nil) then
    FastSetString(info^, pointer(hash), P);
end;

function ModularCryptHash(format: TModularCryptFormat; const password: RawUtf8;
  rounds, saltsize: cardinal; const salt: RawUtf8): RawUtf8;
var
  signer: TSynSigner;
  logN, R, P: cardinal;
  hasher: TSynHasher absolute signer;
  dig: THash256 absolute signer;
{%H-}begin
  case format of
    mcfMd5Crypt .. mcfSha512Crypt:
      result := hasher.UnixCryptHash(MCF_ALGO[format], password, rounds, saltsize, salt);
    mcfPbkdf2Sha1 .. mcfPbkdf2Sha3:
      result := signer.Pbkdf2ModularCrypt(format, password, rounds, saltsize, salt);
    mcfBCrypt, mcfBCryptSha256:
      if Assigned(BCrypt) then
        result := BCrypt(password, salt, rounds, nil, format = mcfBCryptSha256);
    mcfSCrypt:
      begin // rounds=<logN:5-bit:1..31><R:14-bit:1..16384><P:13-bit:1..8192>
        SCryptRoundsDecode(rounds, logN, R, P);
        result := SCryptHash(password, salt, logN, R, P);
      end;
  else
    FastAssignNew(result);
  end;
end;

function ModularCryptHash(const format, password: RawUtf8;
  decodedFormat: PModularCryptFormat): RawUtf8;
var
  mcf: TModularCryptFormat;
  P: PUtf8char;
  rounds: cardinal;
  salt: RawUtf8;
begin
  FastAssignNew(result);
  P := pointer(format);
  mcf := ModularCryptParse(P, rounds, salt);
  if decodedFormat <> nil then
    decodedFormat^ := mcf;
  if mcf in mcfValid then
    result := ModularCryptHash(mcf, password, rounds, 0, salt);
end;

function ModularCryptVerify(const password, hash: RawUtf8;
  allowed: TModularCryptFormats; maxrounds: cardinal): TModularCryptFormat;
var
  rounds, pos, logN, R, P: cardinal;
  salt, h: RawUtf8;
  checksum: PUtf8Char;
  signer: TSynSigner;
  hasher: TSynHasher absolute signer;
begin
  checksum := pointer(hash);
  result := ModularCryptParse(checksum, rounds, salt);
  if not (result in mcfValid) then
    exit;
  if (allowed <> []) and
     not (result in allowed) then
  begin
    result := mcfUnknown;
    exit;
  end;
  if (maxrounds <> 0) and
     (rounds > maxrounds) then
  begin
    result := mcfInvalid;
    exit;
  end;
  pos := 0;
  case result of
    mcfMd5Crypt .. mcfSha512Crypt:
      h := hasher.UnixCryptHash(MCF_ALGO[result], password, rounds, 0, salt, @pos);
    mcfPbkdf2Sha1 .. mcfPbkdf2Sha3:
      h := signer.Pbkdf2ModularCrypt(result, password, rounds, 0, salt, @pos);
    mcfBCrypt .. mcfBCryptSha256:
      if (rounds in [4 .. 31]) and
         (length(salt) = 22) then
        h := BCrypt(password, salt, rounds, @pos, result = mcfBCryptSha256)
      else
        result := mcfInvalid;
    mcfScrypt:
      begin // rounds=<logN:5-bit:1..31><R:14-bit:1..16384><P:13-bit:1..8192>
        SCryptRoundsDecode(rounds, logN, R, P);
        h := SCryptHash(password, salt, logN, R, P, @pos);
      end;
  else
    begin
      result := mcfUnknown;
      exit;
    end;
  end;
  if (pos = 0) or
     (mormot.core.base.StrComp(checksum, PUtf8Char(pointer({%H-}h)) + pos - 1) <> 0) then
    result := mcfInvalid;
end;

function ModularCryptFakeInfo(const id: RawUtf8; format: TModularCryptFormat): RawUtf8;
var
  h: THash256Rec; // always return the same fake content for the same id
  enc: PChar64;
  sha: TSha256;
  salt: TShort23 absolute sha;
const
  RANGE = cardinal(high(TModularCryptFormat)) - cardinal(mcfMd5Crypt); // = 9
begin
  sha.Init;
  sha.Update(@SystemEntropy.Startup, SizeOf(SystemEntropy.Startup));
  sha.Update(id);
  sha.Final(h.b, {noinit=}true);
  if format <= mcfMd5Crypt then // compute consistent format if none supplied
    format := TModularCryptFormat(h.b[0] mod RANGE + byte(succ(mcfMd5Crypt)));
  Join(['$', MCF_IDENT[format], '$'], result);
  enc := @HASH64_CHARS;
  if format = mcfSCrypt then
    enc := @ConvertToBase64;
  salt[0] := #22;  // return consistent salt between calls for the same id
  Base64uriEncode(@salt[1], @h.Hi, 16, enc);
  case format of   // as if they were created with our default parameters
    mcfSha256Crypt .. mcfSha512Crypt:
      Append(result, ['rounds=', MCF_ROUNDS[format], '$', salt, '$']);
    mcfPbkdf2Sha1 .. mcfPbkdf2Sha3:
     Append(result, [MCF_ROUNDS[format], '$', salt, '$']);
    mcfBCrypt:
      Append(result, ['12$', salt]);
    mcfBCryptSha256:
      Append(result, ['v=2,t=2b,r=12$', salt, '$']);
    mcfSCrypt:
      Append(result, ['ln=16,r=8,p=2$', salt, '$']);
  end;
end;

function SCryptRounds(LogN, BlockSize, Parallel: cardinal): cardinal;
begin // = <logN:5-bit:1..31><R:14-bit:1..16384><P:13-bit:1..8192>
  if LogN = 0 then
    LogN := 16
  else if LogN > 31 then
    LogN := 31;
  if BlockSize = 0 then
    BlockSize := 8
  else if BlockSize > 16384 then
    BlockSize := 16384;
  if Parallel = 0 then
    Parallel := 1
  else if Parallel > 8192 then
    Parallel := 8192;
  result := (LogN shl 27) + ((BlockSize - 1) shl 13) + (Parallel - 1);
end;

procedure SCryptRoundsDecode(Rounds: cardinal; out LogN, BlockSize, Parallel: cardinal);
begin
  if Rounds = 0 then
  begin
    LogN := 16;
    BlockSize := 8;
    Parallel := 2; // those default values uses 64MB and 140ms - close to BCrypt
    exit;
  end;
  LogN := Rounds shr 27;
  if LogN = 0 then
    LogN := 16;     // LogN never 0, 5-bit up to 31
  BlockSize :=  (Rounds shr 13) and 16383;
  if BlockSize = 0 then
    BlockSize := 8
  else
    inc(BlockSize); // R never 0, 14-bit up to 16384
  Parallel := Rounds and 8191;
  if Parallel = 0 then
    Parallel := 1
  else
    inc(Parallel); // P never 0, 13-bit up to 8192
end;

const
  SCRYPT_KEYLEN  = 32; // 32-byte output key = 43 chars
  SCRYPT_SALTLEN = 16; // 16-byte default salt = 22 chars

function SCryptHash(const Password: RawUtf8; const Salt: RawUtf8; LogN: PtrUInt;
  BlockSize: PtrUInt; Parallel: PtrUInt; HashPos: PInteger; Api: TSCriptRaw): RawUtf8;
var
  saltbin, saltb64, hash: RawByteString;
begin
  FastAssignNew(result);
  if not Assigned(Api) then
    Api := @SCrypt; // from mormot.crypt.other or mormot.crypt.openssl
  if not Assigned(Api) or
     (LogN <= 1) or
     (LogN > 31) or
     (BlockSize = 0) or
     (Parallel = 0) then
    exit;
  if not TAesPrng.Main.RandomSalt(saltbin, saltb64, SCRYPT_SALTLEN, Salt,
           @ConvertToBase64, @ConvertBase64ToBin) then
    exit; // expects standard base64 but without trailing '='
  hash := Api(Password, saltbin, 1 shl LogN, BlockSize, Parallel, SCRYPT_KEYLEN);
  if hash = '' then
    exit;
  Make(['$scrypt$ln=', LogN, ',r=', BlockSize, ',p=', Parallel, '$',
        saltb64, '$'], result);
  if HashPos <> nil then
    HashPos^ := length(result) + 1;
  Append(result, BinToBase64uri(hash, @ConvertToBase64));
end;


// SCRAM-like mutual auth - see https://github.com/synopse/mORMot2/issues/405

function ScramPersistedKey(const Hash, User: RawUtf8): RawUtf8;
var
  clientkey: THash256;
  stored: THash512Rec; // Lo=StoredKey, Hi=ServerKey
begin
  FastAssignNew(result); // error
  if (Hash = '') or
     (Hash[1] <> '$') or // should be a true KDF/MCF result
     not (ModularCryptIdentify(Hash, @result) in mcfValid) then
    exit;
  HmacSha256U(pointer(Hash), length(Hash), [User, 'Client Key'], clientkey, '|');
  Sha256Digest(stored.Lo, clientkey); // store H(clientkey) for ScramClientProof
  HmacSha256U(pointer(Hash), length(Hash), [User, 'Server Key'], stored.Hi, '|');
  result[1] := '#'; // "#MCF prefix" + base64uri(StoredKey + ServerKey)
  Append(result, BinToBase64uri(stored.b));
  FillZero(clientkey);
  FillZero(stored.b);
end;

function ScramPersistedKey(Mcf: TModularCryptFormat; const Password, User: RawUtf8;
  Rounds: cardinal): RawUtf8;
begin
  result := ScramPersistedKey(ModularCryptHash(mcf, Password, Rounds), User);
end;

function ScramPersistedKey(const McfFormat, Password, User: RawUtf8): RawUtf8;
begin
  result := ScramPersistedKey(ModularCryptHash(McfFormat, Password), User);
end;

function ScramClientProof(const Hash, User: RawUtf8; var ClientSignature: THash256;
  const Msg: array of RawByteString): RawUtf8;
var
  clientkey, storedkey: THash256;
begin
  FastAssignNew(result); // error
  if (Hash = '') or
     (Hash[1] <> '$') or // should be a true KDF/MCF result
     not (ModularCryptIdentify(Hash) in mcfValid) then
    exit;
  HmacSha256U(pointer(Hash), length(Hash), [User, 'Client Key'], clientkey, '|');
  Sha256Digest(storedkey, clientkey);
  HmacSha256U(@storedkey, SizeOf(storedkey), Msg, ClientSignature, '|');
  Xor256(@clientkey, @ClientSignature);
  result := BinToBase64uri(clientkey);
  FillZero(clientkey);
  FillZero(storedkey);
end;

function ScramServerProof(const PersistedKey, ClientProof: RawUtf8;
  const Msg: array of RawByteString): RawUtf8;
var
  l: PtrInt;
  k: PAnsiChar;
  clientsig, clientkey: THash256;
  stored: THash512Rec; // Lo=StoredKey, Hi=ServerKey
begin
  FastAssignNew(result); // error
  if not Base64uriToBin(ClientProof, @clientkey, SizeOf(clientkey)) then
    exit;
  l := length(PersistedKey) - 86;
  k := pointer(PersistedKey);
  if (l < 0) or // should end with base64-uri encoded 2*256-bit = 86 chars
     (k[0] <> '#') or
     not Base64uriToBin(k + l, @stored, 86, SizeOf(stored)) then
    exit;
  HmacSha256U(@stored.Lo, SizeOf(stored.Lo), Msg, clientsig, '|');
  Xor256(@clientkey, @clientsig);
  Sha256Digest(clientkey, clientkey);
  if IsEqual(clientkey, stored.Lo) then // H(candidate_ClientKey) = StoredKey
  begin
    Xor256(@stored.Hi, @clientsig);
    result := BinToBase64uri(stored.Hi); // proof on success
  end;
  FillZero(clientkey);
  FillZero(stored.b);
end;

function ScramClientServerAuth(const Hash, User, ServerProof: RawUtf8;
  var ClientSignature: THash256): boolean;
var
  serverkey, proof: THash256;
begin
  result := false;
  if (Hash <> '') and
     (Hash[1] = '$') and // should be a true KDF/MCF result
     Base64uriToBin(ServerProof, @proof, SizeOf(proof)) then
  begin
    HmacSha256U(pointer(Hash), length(Hash), [User, 'Server Key'], serverkey, '|');
    Xor256(@proof, @ClientSignature);
    result := IsEqual(proof, serverkey);
    FillZero(proof);
    FillZero(serverkey);
  end;
  FillZero(ClientSignature);
end;


{ TScramClient }

constructor TScramClient.Create(aAlgo: TSignAlgo; aMcfSupport: boolean);
begin
  fAlgo := aAlgo;
  fSize := SIGN_SIZE[aAlgo];
  fMcfSupport := aMcfSupport;
end;

destructor TScramClient.Destroy;
begin
  FillZero(fAuthMessage);
  FillZero(fServerProof);
  inherited Destroy;
end;

function TScramClient.ComputeFirstMessage(const User: RawUtf8;
  out Mechanism: RawUtf8; const TestForceNonce: RawUtf8): RawUtf8;
var
  usr: RawUtf8;
begin
  Join(['SCRAM-', SIGNER_TXT[fAlgo]], Mechanism);
  if TestForceNonce <> '' then
    fClientNonce := TestForceNonce // used against reference vectors
  else
  begin
    Random128(@fSigner); // unpredictable - use fSigner as transient storage
    fClientNonce := BinToBase64(@fSigner, SizeOf(THash128));
  end;
  usr := StringReplaceAll(User, ['=', '=3D', ',', '=2C']);
  FormatUtf8('n=%,r=%', [usr, fClientNonce], fAuthMessage);
  if fMcfSupport then
    Append(fAuthMessage, ',f=y');
  Join(['n,,', fAuthMessage], result);
end;

function TScramClient.ComputeFinalMessage(
  const ServerResponse, Password: RawUtf8): RawUtf8;
var
  resp: TDocVariantData;
  fullnonce, s, i, mcf, key, msg: RawUtf8;
  salt: RawByteString;
  iterations: integer;
  salted, client, stored, server: THash512Rec;
begin
  // decode input parameters
  result := ''; // FastAssignNew() makes false positive on Delphi 2007
  fServerProof := '';
  if (fAuthMessage = '') or
     (fClientNonce = '') then
  begin
    fLastError := 'out of order ComputeFinalMessage() usage';
    exit;
  end;
  iterations := 0;
  fLastError := 'invalid Server initial response';
  resp.InitFromPairs(ServerResponse, JSON_FAST, '=', ',');
  if not resp.GetAsRawUtf8('r', fullnonce) or
     not StartWithExact(fullnonce{%H-}, fClientNonce) then
    exit;
  if fMcfSupport and
     resp.GetAsRawUtf8('f', mcf) then // SCRAM-MCF extension
  begin
    if fAlgo = saSha1 then
      exit; // this weak algo is rejected by the draft RFC
    mcf := ModularCryptHash(Base64ToBin(mcf), Password); // ignore i=..,s=..
    if mcf = '' then
      exit; // unsupported Modular Crypt algorithm or invalid prefix
  end
  else if not resp.GetAsRawUtf8('s', s) or
          not Base64ToBin(pointer({%H-}s), length({%H-}s), salt) or
          not resp.GetAsRawUtf8('i', i) or
          not ToInteger(i{%H-}, iterations) or
          (iterations <= 0) or
          (iterations > MAX_PBKDF2_ROUNDS) then // avoid DoS attacks
    // invalid s=... and i=... standard SCRAM parameters
    exit;
  // hash password according to server expectations and compute client signature
  Join(['c=biws,r=', fullnonce], key);
  Join([fAuthMessage, ',', ServerResponse, ',', key], msg);
  if mcf = '' then
  begin
    fSigner.Pbkdf2(fAlgo, Password, salt{%H-}, iterations, @salted);
    fSigner.Full(fAlgo, @salted, fSize, 'Client Key', @client);
    fSigner.Full(fAlgo, @salted, fSize, 'Server Key', @server);
    FillZero(salted, fSize);
  end
  else
  begin
    fSigner.Full(fAlgo, pointer(mcf), length(mcf), 'Client Key', @client);
    fSigner.Full(fAlgo, pointer(mcf), length(mcf), 'Server Key', @server);
    FillZero(mcf);
  end;
  // compute client and server proofs
  fSigner.Hash(fAlgo, @client, fSize, stored);
  fSigner.Full(fAlgo, @stored, fSize, msg, @stored);
  XorMemory(@client, @stored, fSize);
  Join([key, ',p=', BinToBase64(@client, fSize)], result); // client proof
  fSigner.Full(fAlgo, @server, fSize, msg, @server);
  fServerProof := BinToBase64(@server, fSize);             // server proof
  fLastError := '';
  FillZero(key);
  FillZero(client, fSize);
  FillZero(stored, fSize);
  FillZero(server, fSize);
end;

function TScramClient.CheckFinalResponse(const ServerResponse: RawUtf8): boolean;
var
  resp: TDocVariantData;
  proof: RawUtf8;
begin
  result := false;
  if fServerProof = '' then
  begin
    fLastError := 'out of order CheckFinalResponse() usage';
    exit;
  end;
  resp.InitFromPairs(ServerResponse, JSON_FAST, '=', ',');
  if resp.GetAsRawUtf8('v', proof) then
    if proof = fServerProof then
    begin
      fLastError := '';
      result := true;
    end
    else
      fLastError := 'invalid Server proof'
  else
    fLastError := 'invalid Server last response';
end;

{ User 2FA authentication via TOTP as defined in RFC 6238 }

const
  TOTP_MOD: array[4..8] of cardinal = (10000, 100000, 1000000, 10000000, 100000000);

function TotpGenerate(const secret: RawUtf8; digits: integer; algo: TSignAlgo;
  binary: boolean; timestep: Int64): RawUtf8;
var
  sec: RawByteString;
  signer: TSynSigner;
  h: THash512;
  v: cardinal;
begin
  FastAssignNew(result);
  if binary then
    sec := secret
  else
    sec := Base32ToBin(secret);
  if (length(sec) < TOTP_SECRET_MIN) or
     not (digits in [low(TOTP_MOD) .. high(TOTP_MOD)]) then
    exit;
  if timestep < 0 then
    timestep := UnixTimeUtc div TOTP_PERIOD;
  timestep := bswap64(timestep);
  signer.Init(algo, pointer(sec), length(sec));
  signer.Update(@timestep, SizeOf(timestep));
  signer.Final(@h);
  v := bswap32(PCardinal(@h[h[signer.SignatureSize - 1] and 15])^) and $7fffffff;
  UInt32DigitsToUtf8(v mod TOTP_MOD[digits], digits, result);
end;

function TotpGenerateSecret(len: PtrInt): RawUtf8;
var
  bin: RawByteString;
begin
  TAesPrng.Main.FillRandomVar(bin, MaxPtrInt(len, TOTP_SECRET_MIN));
  result := BinToBase32(bin);
  FillZero(bin);
end;

function TotpValidate(const secret, usercode: RawUtf8; window: integer;
  algo: TSignAlgo; timestep: Int64; binary: boolean): boolean;
var
  sec: RawByteString;
  digits, key: cardinal;
  w: PtrInt;
begin
  result := false;
  digits := length(usercode);
  if (cardinal(window) > 100) or
     not (digits in [low(TOTP_MOD) .. high(TOTP_MOD)]) or
     not ToCardinal(usercode, key) then
    exit; // reject weak usercode ASAP
  if binary then
    sec := secret
  else
    sec := Base32ToBin(secret); // decode once into binary
  if length(sec) < TOTP_SECRET_MIN then
    exit;
  if timestep < 0 then
    timestep := UnixTimeUtc div TOTP_PERIOD;
  result := true;
  if TotpGenerate(sec, digits, algo, {bin=}true, timestep) = userCode then
    exit; // first check current time step (most common)
  for w := -window to +window do
    if (w <> 0) and
       (TotpGenerate(sec, digits, algo, {bin=}true, timestep + w) = userCode) then
      exit; // verify previous and next time steps
  result := false;
end;

function TotpUrl(const issuer, account, b32secret: RawUtf8; digits: integer;
  algo: TSignAlgo; period: integer): RawUtf8;
var
  iss, acc: RawUtf8;
begin
  iss := UrlEncodeName(issuer); // favor %20 for spaces here, not +
  acc := UrlEncodeName(account);
  FormatUtf8('otpauth://totp/%:%?secret=%&issuer=%&digits=%&period=%',
    [iss, acc, Split(b32secret, '='), iss, digits, period], result);
  if algo <> saSha1 then
    Append(result, '&algorithm=', SIGNER_API[algo]);
end;


{ **************** Client and Server HTTP Access Authentication }

function ToText(res: TAuthServerResult): PShortString;
begin
  result := GetEnumName(TypeInfo(TAuthServerResult), ord(res));
end;

type
  // reusable state machine for DIGEST on both client and server sides
  {$ifdef USERECORDWITHMETHODS}
  TDigestProcess = record
  {$else}
  TDigestProcess = object
  {$endif USERECORDWITHMETHODS}
  public
    Algo: TDigestAlgo;
    Hash: THashAlgo;
    HashLen: byte;
    UserName: RawUtf8;
    Password: SpiUtf8;
    Realm: RawUtf8;
    Nonce: RawUtf8;
    NC: RawUtf8;
    CNonce: RawUtf8;
    Qop: RawUtf8;
    AuthzID: RawUtf8;
    Opaque: RawUtf8;
    Algorithm: RawUtf8;
    Url: RawUtf8;
    HA1: RawUtf8;
    HA2: RawUtf8;
    Response: RawUtf8;
    tmp: RawByteString;
    Hasher: TSynHasher; // instance used internally by methods
    HA0: THash512Rec;
    procedure Init(DigestAlgo: TDigestAlgo); {$ifdef HASINLINE} inline; {$endif}
    function Parse(var p: PUtf8Char): boolean;
    procedure DigestHa0;
    procedure DigestResponse(const Method: RawUtf8);
    function ClientResponse(const UriName: RawUtf8): RawUtf8;
  end;

procedure TDigestProcess.Init(DigestAlgo: TDigestAlgo);
begin
  Algo := DigestAlgo;
  Algorithm := DIGEST_NAME[DigestAlgo];
  Hash := DIGEST_ALGO[DigestAlgo]; // caller ensured DigestAlgo <> daUndefined
  HashLen := HASH_SIZE[Hash];
end;

function TDigestProcess.Parse(var p: PUtf8Char): boolean;
var
  n: PUtf8Char;
begin
  result := false;
  n := GotoNextNotSpace(p);
  p := PosChar(n, '=');
  if p = nil then
    exit;
  inc(p);
  GetNextItem(p, ',', '"', RawUtf8(tmp)); // tmp='' is valid e.g. for opaque=""
  case IdemPCharSep(n, 'REALM=|QOP=|URI=|ALGORITHM=|NONCE=|NC=|CNONCE=|' +
                       'RESPONSE=|OPAQUE=|USERNAME=|AUTHZID=|') of
    0: // realm="http-auth@example.org"
      FastAssignUtf8(Realm, tmp);
    1: // qop=auth
      FastAssignUtf8(Qop, tmp);
    2: // uri="/dir/index.html"
      FastAssignUtf8(Url, tmp);
    3: // algorithm=MD5
      if PropNameEquals(DIGEST_NAME[Algo], tmp) then
        FastAssignUtf8(Algorithm, tmp) // reuse exact name from server
      else
        exit;
    4: // nonce="xxx"
      FastAssignUtf8(Nonce, tmp);
    5: // nc=xxx
      FastAssignUtf8(NC, tmp);
    6: // cnonce="xxx"
      FastAssignUtf8(CNonce, tmp);
    7: // response="xxx"
      FastAssignUtf8(Response, tmp);
    8: // opaque="xxx"
      FastAssignUtf8(Opaque, tmp);
    9: // username="xxx"'
      FastAssignUtf8(UserName, tmp);
    10: // authzid="xxx"
      FastAssignUtf8(AuthzID, tmp);
  end;
  if (p <> nil) and
     (p^ in [#10, #13]) then
    p := nil; // also end at line feed (e.g. if input is from HTTP headers)
  result := true;
end;

procedure TDigestProcess.DigestHa0;
begin
  Hasher.Init(Hash);
  Hasher.Update([UserName, ':', Realm, ':', Password]);
  Hasher.Final(HA0);
end;

procedure TDigestProcess.DigestResponse(const Method: RawUtf8);
begin
  BinToHexLower(@HA0, HashLen, HA1); // into lowercase hexadecimal
  if Algo in DIGEST_SESS then
  begin
    Hasher.Init(Hash);
    Hasher.Update([HA1, ':', Nonce, ':', CNonce]);
    if AuthzID <> '' then
      Hasher.Update([':', AuthzID]);
    Hasher.Final(HA1);
  end;
  Hasher.Init(Hash);
  if Method = '' then
    Hasher.Update('AUTHENTICATE')
  else
    Hasher.Update(Method);
  Hasher.Update([':', Url]);
  Hasher.Final(HA2);
  Hasher.Full(Hash,
    [HA1, ':', nonce, ':', nc, ':', cnonce, ':', qop, ':', HA2], Response);
end;

function TDigestProcess.ClientResponse(const UriName: RawUtf8): RawUtf8;
begin
  FormatUtf8('username="%",realm=%,nonce="%",cnonce="%",nc=%,qop=%,' +
    'algorithm=%,%="%",response="%"',
    [UserName, QuotedStr(Realm, '"'), Nonce, CNonce, NC, Qop,
     Algorithm, UriName, Url, Response], result);
  if Opaque <> '' then
    Append(result, [',opaque="', Opaque, '"']);
end;


function DigestHA0(Algo: TDigestAlgo; const UserName, Realm: RawUtf8;
  const Password: SpiUtf8; out HA0: THash512Rec): integer;
var
  h: TSynHasher;
begin
  result := 0;
  if (Algo = daUndefined) or
     (UserName = '') or
     (Realm = '') then
    exit;
  h.Init(DIGEST_ALGO[Algo]);
  h.Update([UserName, ':', Realm, ':', Password]);
  result := h.Final(HA0);
end;

function DigestRealm(const FromServer: RawUtf8): RawUtf8;
var
  p: PUtf8Char;
  dp: TDigestProcess;
begin
  FastAssignNew(result);
  dp.Algo := daMD5; // something is needed 
  p := pointer(FromServer);
  while p <> nil do
    dp.Parse(p); // ignore algorithm error
  result := dp.Realm;
end;

function DigestClient(Algo: TDigestAlgo;
  const FromServer, DigestMethod, DigestUri, UserName: RawUtf8;
  const Password: SpiUtf8; const DigestUriName: RawUtf8): RawUtf8;
var
  p: PUtf8Char;
  dp: TDigestProcess;
begin
  FastAssignNew(result);
  if Algo = daUndefined then
    exit;
  // parse server token
  {%H-}dp.Init(Algo);
  p := pointer(FromServer);
  while p <> nil do
    if not dp.Parse(p) then // invalid algorithm
      exit;
  if (dp.Realm = '') or
     (dp.Nonce = '') then
    exit;
  // compute the client response
  dp.Url := DigestUri;
  dp.UserName := UserName;
  dp.Password := Password;
  dp.CNonce := Int64ToHexLower(Random64);
  dp.NC := '00000001';
  dp.Qop := 'auth';
  dp.DigestHa0;
  dp.DigestResponse(DigestMethod);
  result := dp.ClientResponse(DigestUriName);
  FillZero(dp.HA0.b); // Ha0 is a sensitive value
  FillZero(dp.HA1);
end;

procedure BasicClient(const UserName: RawUtf8; const Password: SpiUtf8;
  out Result: SpiUtf8; const Prefix: RawUtf8);
var
  ha: RawUtf8;
begin
  Join([UserName, ':', Password], ha);
  Result := BinToBase64(ha, Prefix, '', false);
  FillZero(ha);
end;

function BasicRealm(const FromServer: RawUtf8): RawUtf8;
var
  p: PUtf8Char;
begin
  FastAssignNew(result);
  p := pointer(FromServer);
  if IdemPChar(p, 'REALM="') then
    UnQuoteSqlStringVar(p + 6, result);
end;

function DigestServerInit(Algo: TDigestAlgo;
  const QuotedRealm, Prefix, Suffix: RawUtf8; Opaque, Tix64: Int64): RawUtf8;
var
  h: THash128Rec;
  noncehex, opaquehex: TShort32;
begin
  FastAssignNew(result);
  if (Algo = daUndefined) or
     (QuotedRealm = '') then
    exit; // missing some mandatory context
  if Tix64 = 0 then
    Tix64 := GetTickCount64; // ms resolution, update period of 4..16 ms
  h.L := Tix64;
  h.w[3] := Opaque xor 7777; // upper 16-bit of the nonce map lowest Opaque bits
  h.H := Random64;
  h.L := h.L xor bswap64(h.H); // 48-bit ms would overflow after 8900 years
  noncehex[0] := #32;
  BinToHexLower(@h, @noncehex[1], SizeOf(h));
  DefaultHasher128(@h, @Opaque, SizeOf(Opaque)); // likely to be AesNiHash128()
  opaquehex[0] := #32;
  BinToHexLower(@h, @opaquehex[1], SizeOf(h));
  FormatUtf8('%realm=%,qop="auth",algorithm=%,nonce="%",opaque="%"%',
    [Prefix, QuotedRealm, DIGEST_NAME[Algo], noncehex, opaquehex, Suffix],
    result);
end;

function DigestServerAuth(Algo: TDigestAlgo; const Realm, Method: RawUtf8;
  FromClient: PUtf8Char; Opaque: Int64;
  const OnSearchUser: TOnDigestServerAuthGetUserHash;
  out User, Url: RawUtf8; NonceExpSec: PtrUInt; Tix64: Qword): TAuthServerResult;
var
  resp: RawUtf8;
  dp: TDigestProcess;
  created: QWord;
  nonce128, opaque128: THash128Rec;
begin
  result := asrRejected;
  if (FromClient = nil) or
     (Algo = daUndefined) or
     (Realm = '') or
     not Assigned(OnSearchUser) then
    exit;
  // parse the input parameters
  {%H-}dp.Init(Algo);
  while FromClient <> nil do
    if not dp.Parse(FromClient) then
      exit; // invalid input (e.g. unexpected algorithm)
  // validate the parameters
  if (dp.UserName = '') or
     (dp.Realm  <> Realm) or
     (dp.Url = '') or
     (dp.Nonce = '') or
     (dp.NC = '') or
     (dp.Qop = '') or
     (dp.CNonce = '') or
     (dp.Opaque = '') or
     (dp.Response = '') or
     not mormot.core.text.HexToBin(
       pointer(dp.Nonce), @nonce128, SizeOf(nonce128)) or
     not mormot.core.text.HexToBin(
       pointer(dp.Opaque), @opaque128, SizeOf(opaque128)) then
    exit;
  // verify the nonce is not deprecated, and matches lowest 16-bit of Opaque
  created := nonce128.L xor bswap64(nonce128.H);
  if ((created shr 48) xor 7777) and $ffff <> Opaque and $ffff then
    exit;
  if NonceExpSec = 0 then
    NonceExpSec := 1;
  if Tix64 = 0 then
    Tix64 := GetTickCount64;
  created := created and pred(QWord(1) shl 48);
  if Tix64 - created > NonceExpSec * MilliSecsPerSec then
    exit;
  // fast challenge against the 64-bit Opaque value (typically a connection ID)
  DefaultHasher128(@nonce128, @Opaque, SizeOf(Opaque)); // see DigestServerInit
  if not IsEqual(nonce128.b, opaque128.b) then
    exit;
  result := OnSearchUser(dp.UserName, dp.Realm, dp.HA0);
  if result <> asrMatch then
    exit;
  // validate the cryptographic challenge
  resp := dp.Response;
  dp.DigestResponse(Method);
  FillZero(dp.HA0.b);
  result := asrIncorrectPassword;
  if not PropNameEquals(dp.Response, resp) then
    exit;
  // successfully authenticated
  User := dp.UserName;
  Url := dp.Url;
  result := asrMatch;
end;

function BasicServerAuth(FromClient: PUtf8Char;
  out User, Password: RawUtf8): boolean;
var
  l: PtrInt;
begin
  result := false;
  if FromClient = nil then
    exit;
  while FromClient^ = ' ' do
    inc(FromClient);
  l := 0;
  while FromClient[l] > ' ' do
    inc(l);
  if l < 4 then
    exit;
  Split(Base64ToBin(PAnsiChar(FromClient), l), ':', User, Password);
  result := (User <> '') and
            (Password <> '') and
            (PosExChar(':', User) = 0);
end;


{ TBasicAuthServer }

function TBasicAuthServer.BeforeAuth(
  Sender: TObject; const User: RawUtf8): boolean;
begin
  result := not Assigned(fOnBeforeAuth) or
            fOnBeforeAuth(Sender, User);
end;

function TBasicAuthServer.AfterAuth(
  Sender: TObject; const User: RawUtf8): boolean;
begin
  result := not Assigned(fOnAfterAuth) or
            fOnAfterAuthDelayed or
            fOnAfterAuth(Sender, User);
end;

constructor TBasicAuthServer.Create(const aRealm: RawUtf8);
begin
  if aRealm = '' then
    EDigest.RaiseUtf8('%.Create: void Realm', [self]);
  fRealm := aRealm;
  QuotedStr(fRealm, '"', fQuotedRealm);
  FormatUtf8('WWW-Authenticate: Basic realm="%"'#13#10, [fRealm], fBasicInit);
end;

function TBasicAuthServer.Realm: RawUtf8;
begin
  result := fRealm;
end;

function TBasicAuthServer.Instance: TObject;
begin
  result := self;
end;

function TBasicAuthServer.BasicInit: RawUtf8;
begin
  result := fBasicInit;
end;

function TBasicAuthServer.BasicAuth(FromClient: PUtf8Char;
  out ClientUser: RawUtf8): boolean;
var
  user, pass: RawUtf8;
begin
  result := BasicServerAuth(FromClient, user, pass) and
            OnBasicAuth(self, user{%H-}, pass{%H-});
  if not result then
    exit;
  ClientUser := user;
  FillZero(pass);
end;

function TBasicAuthServer.OnBasicAuth(aSender: TObject;
  const aUser: RawUtf8; const aPassword: SpiUtf8): boolean;
begin
  result := CheckCredential(aUser, aPassword) = asrMatch;
end;


{ TDigestAuthServer }

type
  // storing 256-bit in memory is enough to match current TDigestAlgo
  TDigestAuthHash = THash256;
  TDigestAuthHashs = array of TDigestAuthHash;

constructor TDigestAuthServer.Create(const aRealm: RawUtf8; aAlgo: TDigestAlgo);
begin
  if aAlgo = daUndefined then
    EDigest.RaiseUtf8('%.Create: undefined Algo', [self]);
  inherited Create(aRealm);
  fAlgo := aAlgo;
  fAlgoSize := HASH_SIZE[DIGEST_ALGO[aAlgo]];
  if fAlgoSize > SizeOf(TDigestAuthHash) then // paranoid
    EDigest.RaiseUtf8('%.Create: % %-bit digest is too big',
      [self, fAlgoSize shl 3, DIGEST_NAME[aAlgo]]);
  fRequestExpSec := 60;
  fOpaqueObfuscate := Random64; // changes at each server restart
end;

function TDigestAuthServer.GetUserHashWithCallback(const aUser,
  aRealm: RawUtf8; out aDigest: THash512Rec): TAuthServerResult;
begin
  if (self <> nil) and
     BeforeAuth(self, aUser) then
    result := GetUserHash(aUser, aRealm, aDigest)
  else
    result := asrRejected;
end;

procedure TDigestAuthServer.ComputeDigest(const aUser: RawUtf8;
  const aPassword: SpiUtf8; out Digest: THash512Rec);
begin
  if PosExChar(':', aUser) <> 0 then
    EDigest.RaiseUtf8('%.ComputeDigest: unexpected '':'' in user=%', [self, aUser]);
  if DigestHA0(fAlgo, aUser, fRealm, aPassword, Digest) <> fAlgoSize then
    EDigest.RaiseUtf8('%.ComputeDigest: DigestHA0?', [self]);
end;

function TDigestAuthServer.DigestInit(Opaque, Tix64: Int64;
  const Prefix, Suffix: RawUtf8): RawUtf8;
begin
  Opaque := Opaque xor fOpaqueObfuscate;
  result := DigestServerInit(fAlgo, fQuotedRealm, Prefix, Suffix, Opaque, Tix64);
end;

function TDigestAuthServer.DigestAlgoMatch(const FromClient: RawUtf8): boolean;
var
  p, a: PUtf8Char;
  l: PtrInt;
begin
  result := false;
  p := StrPosI('ALGORITHM=', pointer(FromClient));
  if p = nil then
    exit;
  inc(p, 10);
  l := GetNextItemTrimedBuffer(p, ',', a);
  result := IdemPropNameU(DIGEST_NAME[fAlgo], a, l);
end;

function TDigestAuthServer.DigestAuth(FromClient: PUtf8Char;
  const Method: RawUtf8; Opaque, Tix64: Int64;
  out ClientUser, ClientUrl: RawUtf8): TAuthServerResult;
begin
  Opaque := Opaque xor fOpaqueObfuscate;
  result := DigestServerAuth(fAlgo, fRealm, Method, FromClient, Opaque,
    GetUserHashWithCallback, ClientUser, ClientUrl, fRequestExpSec, Tix64);
end;

function TDigestAuthServer.CheckCredential(const aUser: RawUtf8;
  const aPassword: SpiUtf8): TAuthServerResult;
var
  dig, stored: THash512Rec;
begin
  result := GetUserHashWithCallback(aUser, fRealm, stored);
  if result <> asrMatch then
    exit;
  if (DigestHA0(fAlgo, aUser, fRealm, aPassword, dig) = fAlgoSize) and
     mormot.core.base.CompareMem(@dig, @stored, fAlgoSize) then
    if AfterAuth(self, aUser) then
      result := asrMatch
    else
      result := asrRejected
  else
    result := asrIncorrectPassword;
  FillZero(dig.b);
  FillZero(stored.b);
end;


{ TDigestAuthServerMem }

constructor TDigestAuthServerMem.Create(const aRealm: RawUtf8;
  aAlgo: TDigestAlgo);
begin
  inherited Create(aRealm, aAlgo);
  fUsers := TSynDictionary.Create(
    TypeInfo(TRawUtf8DynArray), TypeInfo(TDigestAuthHashs));
  fUsers.ThreadUse := uRWLock; // multi-read / single-write thread-safe access
end;

destructor TDigestAuthServerMem.Destroy;
begin
  inherited Destroy;
  FreeAndNil(fUsers);
end;

function TDigestAuthServerMem.GetUserHash(const aUser, aRealm: RawUtf8;
  out aDigest: THash512Rec): TAuthServerResult;
begin
  // no need to validate aRealm: DigestServerAuth caller already dit it
  if fUsers.FindAndCopy(aUser, aDigest, {updtimeout=}false) then
    result := asrMatch
  else
    result := asrUnknownUser;
end;

function TDigestAuthServerMem.GetCount: integer;
begin
  if self = nil then
    result := 0
  else
    result := fUsers.Count;
end;

function TDigestAuthServerMem.GetUsers: TRawUtf8DynArray;
begin
  result := nil;
  if self = nil then
    exit;
  fUsers.Safe.ReadLock;
  try
    PDynArray(@fUsers.Keys)^.CopyTo(result);
  finally
    fUsers.Safe.ReadUnLock;
  end;
end;

procedure TDigestAuthServerMem.SetCredential(const aUser: RawUtf8;
  const aPassword: SpiUtf8);
var
  dig: THash512Rec;
begin
  if (aUser = '') or
     (self = nil) then
    exit;
  if aPassword = '' then
  begin
    if fUsers.Delete(aUser) >= 0 then
      fModified := true;
  end
  else
  begin
    ComputeDigest(aUser, aPassword, dig);
    fUsers.AddOrUpdate(aUser, dig);
    fModified := true;
  end;
  FillZero(dig.b);
end;

procedure TDigestAuthServerMem.ClearCredentials;
begin
  if (self = nil) or
     (fUsers = nil) then
    exit;
  fUsers.Safe.Lock; // within write lock
  try
    fUsers.Values.FillZero; // TDigestAuthHash anti-forensic
    fUsers.DeleteAll;
  finally
    fUsers.Safe.UnLock;
  end;
end;


{ TDigestAuthServerFile }

constructor TDigestAuthServerFile.Create(const aRealm: RawUtf8;
  const aFileName: TFileName; const aFilePassword: SpiUtf8; aAlgo: TDigestAlgo);
begin
  inherited Create(aRealm, aAlgo);
  if PosExChar(':', aRealm) <> 0 then
    EDigest.RaiseUtf8('%.Create: unexpected '':'' in realm=%', [self, aRealm]);
  if aFileName = '' then
    EDigest.RaiseUtf8('%.Create: void filename', [self]);
  fFileName := ExpandFileName(aFileName);
  if aFilePassword <> '' then
    Pbkdf2HmacSha256(aFilePassword, aRealm, 1000, fAesKey);
  LoadFromFile;
end;

destructor TDigestAuthServerFile.Destroy;
begin
  if fModified then // persist any pending changes
    SaveToFile;
  ClearCredentials; // safely fill in memory against forensic
  FillZero(fAesKey);   // anti-forensic
  inherited Destroy;
end;

function TDigestAuthServerFile.GetEncrypted: boolean;
begin
  result := not IsZero(fAesKey);
end;

function TDigestAuthServerFile.GetAes: TAesAbstract;
begin
  if IsZero(fAesKey) then
    result := nil
  else
    result := TAesFast[mGCM].Create(fAesKey);
end;

procedure TDigestAuthServerFile.LoadFromFile;
var
  tmp1, tmp2: RawByteString;
  p, l: PUtf8Char;
  u, r: RawUtf8;
  h: TDigestAuthHash;
  aes: TAesAbstract;
begin
  ClearCredentials;
  fUsers.Safe.Lock; // within write lock
  try
    fFileLastTime := FileAgeToUnixTimeUtc(fFileName);
    if fFileLastTime <> 0 then
    begin
      tmp1 := StringFromFile(fFileName);
      aes := GetAes;
      try
        if aes = nil then
          tmp2 := tmp1
        else
          tmp2 := aes.DecryptPkcs7(tmp1, {iv=}true, {raise=}true);
        p := pointer(tmp2);
        while p <> nil do
        begin
          l := pointer(GetNextLine(p, p, {trim=}true));
          if l <> nil then
          begin
            // line format is username:realm:5f111290c3bea272bc72f98218fe4e15
            GetNextItemTrimed(l, ':', u);
            if aes = nil then // no realm stored in encrypted file
              GetNextItemTrimed(l, ':', r);
            if (u <> '') and
               ((aes <> nil) or (r = fRealm)) and
               mormot.core.text.HexToBin(pointer(l), @h, fAlgoSize) then
              fUsers.Add(u, h);
          end;
        end;
      finally
        aes.Free;
      end;
    end;
  finally
    fUsers.Safe.UnLock;
    // anti-forensic
    FillZero(tmp1);
    FillZero(tmp2);
    FillZero(h);
  end;
end;

procedure TDigestAuthServerFile.SaveToFile;
var
  i: PtrInt;
  w: TTextWriter;
  tmp: TTextWriterStackBuffer; // 8KB work buffer on stack
  middle, txt1, txt2: RawUtf8;
  u: PRawUtf8;
  d: ^TDigestAuthHash;
  aes: TAesAbstract;
begin
  aes := GetAes;
  try
    fUsers.Safe.ReadLock;
    try
      if not fModified then
        exit;
      fModified := false;
      if aes = nil then
        middle := ':' + fRealm + ':'
      else
        middle := ':'; // no need to store the realm in the encrypted file
      w := TTextWriter.CreateOwnedStream(tmp);
      try
        u := fUsers.Keys.Value^;
        d := fUsers.Values.Value^;
        for i := 0 to fUsers.Count - 1 do
        begin
          w.AddString(u^);
          w.AddString(middle);
          w.AddBinToHex(d, fAlgoSize, {lowerhex=}true);
          w.Add(#10);
          inc(u);
          inc(d);
        end;
        w.SetText(txt1);
      finally
        w.Free;
      end;
    finally
      fUsers.Safe.ReadUnLock;
    end;
    if aes = nil then
      txt2 := txt1
    else
      txt2 := aes.EncryptPkcs7(txt1, {iv=}true);
    FileFromString(txt2, fFileName);
    fFileLastTime := FileAgeToUnixTimeUtc(fFileName);
  finally
    aes.Free;
    // anti-forensic
    FillZero(txt1);
    FillZero(txt2);
  end;
end;

function TDigestAuthServerFile.RefreshFile: boolean;
var
  ondisk: TUnixTime;
begin
  result := false;
  if fModified then
  begin
    SaveToFile;
    exit;
  end;
  ondisk := FileAgeToUnixTimeUtc(fFileName);
  if (ondisk = 0) or
     (ondisk = fFileLastTime) then
    exit;
  LoadFromFile;
  result := true;
end;


{ ***************** Password-Safe and TSynConnectionDefinition Classes }

{ TObjectWithPassword }

destructor TObjectWithPassword.Destroy;
begin
  FillZero(fPassword);
  fKey := 0; // this is also a sensitive value
  inherited Destroy;
end;

class function TObjectWithPassword.ComputePassword(
  const PlainPassword: SpiUtf8; CustomKey: cardinal): SpiUtf8;
var
  instance: TObjectWithPassword;
begin
  instance := TObjectWithPassword.Create;
  try
    instance.Key := CustomKey;
    instance.SetPassWordPlain(PlainPassword);
    result := instance.fPassWord;
  finally
    instance.Free;
  end;
end;

class function TObjectWithPassword.ComputePassword(PlainPassword: pointer;
  PlainPasswordLen: integer; CustomKey: cardinal): SpiUtf8;
begin
  result := ComputePassword(
    BinToBase64uri(PlainPassword, PlainPasswordLen), CustomKey);
end;

class function TObjectWithPassword.ComputePlainPassword(
  const CypheredPassword: SpiUtf8; CustomKey: cardinal;
  const AppSecret: RawUtf8): SpiUtf8;
var
  instance: TObjectWithPassword;
begin
  instance := TObjectWithPassword.Create;
  try
    instance.Key := CustomKey;
    instance.fPassWord := CypheredPassword;
    instance.GetPasswordSafe(result, AppSecret);
  finally
    instance.Free;
  end;
end;

procedure TObjectWithPassword.XorKey(var Value: RawByteString);
begin
  SymmetricEncrypt(fKey xor $A5abba5A, Value);
end;

function TObjectWithPassword.GetPassWordPlain: SpiUtf8;
begin
  if (self = nil) or
     (fPassWord = '') then
    FastAssignNew(result)
  else if fKey = OBJECTPASSWORD_PLAIN then
    result := fPassword
  else
    GetPasswordSafe(result, '');
end;

procedure TObjectWithPassword.GetPasswordSafe(var Value: SpiUtf8;
  const AppSecret: RawUtf8);
var
  pwd: RawByteString;
  app, usr: RawUtf8;
  i, j: PtrInt;
begin
  if (fPassword = '') or
     (fKey = OBJECTPASSWORD_PLAIN) then
  begin
    Value := fPassWord;
    exit;
  end;
  FastAssignNew(Value);
  if AppSecret = '' then
    ClassToText(ClassType, app)
  else
    app := AppSecret;
  Join([Executable.User, ':'], usr);
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
    Base64ToBin(@fPassword[i], j - i, pwd);
    if pwd <> '' then
    begin
      Value := CryptDataForCurrentUser(pwd, app, false);
      if Value <> '' then
        exit;
    end;
  end
  else
  begin
    i := PosExChar(':', fPassword);
    if i > 0 then
      ESynCrypto.RaiseUtf8('%.GetPasswordSafe unable to retrieve the stored ' +
        'v: current user is [%], but password in % was encoded for [%]',
        [self, Executable.User, app, copy(fPassword, 1, i - 1)]);
  end;
  Base64ToBinSafe(pointer(fPassword), length(fPassword), RawByteString(Value));
  XorKey(RawByteString(Value));
end;

procedure TObjectWithPassword.SetPassWordPlainCurrentUser(const Value: SpiUtf8;
  AppSecret: RawUtf8);
var
  list: TSynNameValue;
begin // follow GetPasswordSafe() encoding logic
  if PosExChar(':', fPassword) = 0 then
    FillZero(fPassword); // both formats are incompatible
  list.InitFromCsv(pointer(fPassWord), ':', ',');
  FillZero(fPassword);
  list.Delete(Executable.User);
  if Value <> '' then
  begin
    if AppSecret = '' then
      ClassToText(ClassType, AppSecret);
    list.Add(Executable.User,
      BinToBase64(CryptDataForCurrentUser(Value, AppSecret, true)));
    if fKey = OBJECTPASSWORD_PLAIN then
      fKey := 0; // disable plain password storage from now on
  end;
  fPassWord := list.AsCsv(':', ',');
end;

procedure TObjectWithPassword.SetPassWordPlain(const Value: SpiUtf8);
var
  tmp: RawByteString;
begin
  if self = nil then
    exit;
  FillZero(fPassword);
  if (value = '') or
     (fKey = OBJECTPASSWORD_PLAIN) then
  begin
    fPassWord := value;
    exit;
  end;
  FastSetRawByteString(tmp, pointer(value), Length(value)); // private copy
  XorKey(tmp);
  fPassWord := BinToBase64(tmp);
  FillZero(tmp);
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
  fTokenSeed := Random32Not0;
  fSessionGenerator := abs(fTokenSeed * PPtrInt(self)^);
  fTokenSeed := fTokenSeed * Random31Not0;
end;

destructor TSynAuthenticationAbstract.Destroy;
begin
  inherited;
  fSafe.Done;
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
  ESynCrypto.RaiseUtf8('%.AuthenticateUser() is not implemented', [self]);
end;

procedure TSynAuthenticationAbstract.DisauthenticateUser(const aName: RawUtf8);
begin
  ESynCrypto.RaiseUtf8('%.DisauthenticateUser() is not implemented', [self]);
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
var
  bits: TTimeLogBits; // safer with an explicit local variable
begin
  bits.From(UnixTimeToDateTime(Value shr 31));
  result := bits.Value;
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

const
  SYNID_53_UNIXMINIMAL = 1735689600; // Wed Jan 01 2025 00:00:00 UTC

function TSynUniqueIdentifierBits.GetJavaScriptID: TSynUnique53;
begin
  if ProcessID > 255 then
    ESynCrypto.RaiseUtf8('TSynUniqueIdentifierBits.AsJavaScriptID: ProcessID=%',
      [ProcessID]);
  result := (Value and $7fffff) + Int64((Value shr 31) - SYNID_53_UNIXMINIMAL) shl 23;
end;

procedure TSynUniqueIdentifierBits.SetJavaScriptID(const aJavaScriptID: TSynUnique53);
begin
  Value := (aJavaScriptID and $7fffff) +
           ((aJavaScriptID shr 23) + SYNID_53_UNIXMINIMAL) shl 31;
end; // in the far future, may overlap to go upon year 2093 (as was done for Y2K)


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
    if currentTime > fLastUnixCreateTime then // time may have been tweaked
    begin
      fLastUnixCreateTime := currentTime;
      fLastCounter := 0; // reset
    end;
    if fLastCounter = $7fff then
    begin
      // collide if more than 32768 per second (unlikely) -> tweak the timestamp
      inc(fLastUnixCreateTime);
      inc(fCollisions);
      fLastCounter := 0;
    end
    else
      inc(fLastCounter);
    result.Value := Int64(fLastCounter or fIdentifierShifted) or
                    (Int64(fLastUnixCreateTime) shl 31);
    inc(fComputedCount);
  {$ifdef HASFASTTRYFINALLY}
  finally
  {$endif HASFASTTRYFINALLY}
    fSafe.UnLock;
  end;
end;

function TSynUniqueIdentifierGenerator.ComputeNew: Int64;
var
  b: TSynUniqueIdentifierBits;
begin
  ComputeNew(b);
  result := b.Value;
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
  tab: PCrc32tab;
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
    FillZero(key.b);
    // fCrypto[] is not used if fCryptoAes*.Initialized are set
  end
  else
  begin
    // due to the weakness of the hash algorithms used, this approach is a bit
    // naive and would be broken easily with brute force - but point here is to
    // hide/obfuscate public values at end-user level (e.g. when publishing URIs),
    // not implement strong security, so it sounds good enough for our purpose
    tab := crc32ctab;
    crc := tab^[0, len and 1023];
    for i := 0 to high(fCrypto) + 1 do
    begin
      crc := tab^[0, crc and 1023] xor
             tab^[3, i] xor
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
    crc: cardinal;                 // 32-bit
    id: TSynUniqueIdentifierBits;  // 64-bit
  end;

function TSynUniqueIdentifierGenerator.ToObfuscated(
  const aIdentifier: TSynUniqueIdentifier): TSynUniqueIdentifierObfuscated;
var
  block: THash128Rec; // 128-bit
  bits: TSynUniqueIdentifierObfuscatedBits absolute block; // 64+32 = 96-bit
  key: cardinal;
begin
  FastAssignNew(result);
  if aIdentifier = 0 then
    exit;
  bits.id.Value := aIdentifier;
  if self = nil then
    key := 0
  else
    key := crc32ctab^[0, bits.id.ProcessID and 1023] xor fCryptoCRC;
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
    key := crc32ctab^[0, bits.id.ProcessID and 1023] xor fCryptoCRC;
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
      key := crc32ctab^[0, bits.id.ProcessID and 1023] xor fCryptoCRC;
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
    if UnixTimeUtc >= fLastUnixCreateTime then
      break;
    SleepHiRes(100);
  until GetTickCount64 > tix;
end;

function TSynUniqueIdentifierGenerator.SaveTo: Int64;
begin
  result := (Int64(fLastUnixCreateTime) shl 15) + fLastCounter;
end;

procedure TSynUniqueIdentifierGenerator.LoadFrom(aSaved: Int64);
begin
  fLastCounter := aSaved and $7fff;
  fLastUnixCreateTime := aSaved shr 15;
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
  if aClass = nil then
    aClass := TAesFast[mCtr]; // fastest on x86_64 or OpenSSL - server friendly
  fAes[false] := aClass.Create(aKey, aKeySize);
  fAes[true]  := fAes[false].CloneEncryptDecrypt;
  fAheadMode  := fAes[false].AlgoMode in [mCfc, mOfc, mCtc, mGcm];
end;

constructor TProtocolAes.CreateFrom(aAnother: TProtocolAes);
begin
  inherited Create;
  fAes[false] := aAnother.fAes[false].Clone;
  fAes[true]  := fAes[false].CloneEncryptDecrypt;
  fAheadMode  := aAnother.fAheadMode;
end;

destructor TProtocolAes.Destroy;
begin
  fAes[false].Free;
  if fAes[true] <> fAes[false] then
    fAes[true].Free; // fAes[false].CloneEncryptDecrypt may return self
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
  fSafe.Lock;
  try
    try
      if fAheadMode then
        aPlain := fAes[false].MacAndCrypt(aEncrypted, {enc=}false, {iv=}true, '')
      else
        aPlain := fAes[false].DecryptPkcs7(aEncrypted, {iv=}true, {raise=}false);
      if aPlain = '' then
        result := sprBadRequest
      else
        result := sprSuccess;
    except
      result := sprInvalidMAC;
    end;
  finally
    fSafe.UnLock;
  end;
end;

procedure TProtocolAes.Encrypt(const aPlain: RawByteString;
  out aEncrypted: RawByteString);
begin
  fSafe.Lock;
  try
    if fAheadMode then
      aEncrypted := fAes[true].MacAndCrypt(aPlain, {enc=}true, {iv=}true, '')
    else
      aEncrypted := fAes[true].EncryptPkcs7(aPlain, {iv=}true);
  finally
    fSafe.UnLock;
  end;
end;

function TProtocolAes.Clone: IProtocol;
begin
  result := TProtocolAesClass(ClassType).CreateFrom(self);
end;



{ ******* TBinaryCookieGenerator Simple Cookie Generator }

{ TBinaryCookieGenerator }

constructor TBinaryCookieGenerator.Create(const Name: RawUtf8;
  DefaultSessionTimeOutMinutes: cardinal);
begin
  fCookieName := Name;
  fDefaultTimeOutMinutes := DefaultSessionTimeOutMinutes;
  fContext.Version := 1;
  repeat // small enough to remain 31-bit > 0
    fContext.SessionSequence := Random32 shr 9;
  until fContext.SessionSequence <> 0;
  fContext.SessionSequenceStart := fContext.SessionSequence;
  fContext.CrcSalt := Random32Not0; // from TLecuyer gsl_rng_taus2 generator
  Random128(@fContext.CryptKey);    // unpredictable
  fAes.Init(fContext.CryptKey, 128, {avx=}false);
end;

destructor TBinaryCookieGenerator.Destroy;
begin
  Finalize(fContext);
  FillCharFast(fContext, SizeOf(fContext), 0); // anti-forensic
  fAes.Done;
  inherited Destroy;
end;

type
  // map the binary layout of our Base64 serialized cookies
  TCookieContent = packed record
    head: packed record   // 256-bit header (minimum cookie size if no record)
      crc: cardinal;             // = 32-bit naive anti-fuzzing crc32c
      session: cardinal;         // = jti claim sequence (genuine number)
      expires: TUnixTimeMinimal; // = exp claim - after session for Invalidate[]
      issued: TUnixTimeMinimal;  // = iat claim (UnixTimeMinimalUtc)
      gmac: THash128;            // = 128-bit AES-GCM tag
    end;
    data: TBuffer2K;             // optional AES-CTR record serialization
  end;

function TBinaryCookieGenerator.Generate(out Cookie: RawUtf8;
  TimeOutMinutes: cardinal; PRecordData: pointer;
  PRecordTypeInfo: PRttiInfo): TBinaryCookieGeneratorSessionID;
var
  cc: TCookieContent;
  tmp: TSynTempBuffer;
  pad: integer;
begin
  result := 0;
  tmp.Init(0);
  try
    if (PRecordData <> nil) and
       (PRecordTypeInfo <> nil) then
    begin
      BinarySave(PRecordData, tmp, PRecordTypeInfo, rkRecordTypes);
      pad := tmp.len and AES_MOD;
      if pad <> 0 then // AES-CTR is easier if padded on 16 bytes
        inc(tmp.len, 16 - pad);
      if tmp.len > SizeOf(cc.data) then
        // all cookies storage should be < 4K so a single 2K cookie seems huge
        ESynCrypto.RaiseU('TBinaryCookieGenerator: Too Big Too Fat');
      MoveFast(tmp.buf^, cc.data, tmp.len);
    end;
    cc.head.issued := UnixTimeMinimalUtc; // valid until year 2152
    if TimeOutMinutes = 0 then
      TimeOutMinutes := fDefaultTimeOutMinutes;
    if TimeOutMinutes = 0 then // max 1 month expiration seems good enough
      TimeOutMinutes := 31 * 24 * 60;
    cc.head.expires := cc.head.issued + TimeOutMinutes * 60;
    fSafe.Lock; // protect fAes instance and SessionSequence
    try
      inc(fContext.SessionSequence);
      inc(result, fContext.SessionSequence); // inc() to circumvent Delphi hint
      cc.head.session := result;
      fAes.Reset(@cc.head.session, 12); // IV should be unique, not random
      fAes.Add_AAD(@cc.head.session, 3 * SizeOf(cardinal));
      if tmp.len > 0 then
        fAes.Encrypt(@cc.data, @cc.data, tmp.len); // optional AES-CTR + GMAC
      fAes.Final(cc.head.gmac, {done=}false);
    finally
      fSafe.UnLock;
    end;
    cc.head.crc := crc32c(fContext.CrcSalt,
                     @cc.head.session, (SizeOf(cc.head) - 4) + tmp.len);
    Cookie := BinToBase64Uri(@cc, SizeOf(cc.head) + tmp.len);
  finally
    tmp.Done;
  end;
end;

function TBinaryCookieGenerator.Validate(const Cookie: RawUtf8;
  PRecordData: pointer; PRecordTypeInfo: PRttiInfo; PExpires, PIssued: PUnixTime;
  Invalidate: boolean; Now32: TUnixTimeMinimal): TBinaryCookieGeneratorSessionID;
begin
  result := Validate(pointer(Cookie), length(Cookie), PRecordData,
    PRecordTypeInfo, PExpires, PIssued, Invalidate, Now32)
end;

function TBinaryCookieGenerator.Validate(Cookie: PUtf8Char; CookieLen: PtrInt;
  PRecordData: pointer; PRecordTypeInfo: PRttiInfo; PExpires, PIssued: PUnixTime;
  Invalidate: boolean; Now32: TUnixTimeMinimal): TBinaryCookieGeneratorSessionID;
var
  len: integer;
  ccend: PAnsiChar;
  cc: TCookieContent; // local working buffer on stack (no memory allocation)
begin
  result := 0; // parsing/crc/timeout error
  len := Base64uriToBinLength(CookieLen);
  if (self = nil) or
     (len < SizeOf(cc.head)) or
     (len > SizeOf(cc)) or
     (not Base64uriDecode(pointer(Cookie), @cc, CookieLen)) or
     (cc.head.session < cardinal(fContext.SessionSequenceStart)) or
     (cc.head.session > cardinal(fContext.SessionSequence)) or
     (cc.head.issued = 0) or
     (cc.head.expires <= cc.head.issued) or
     (crc32c(fContext.CrcSalt, @cc.head.session, len - 4) <> cc.head.crc) then
    exit; // simple but efficient anti-fuzzing detection
  if Now32 = 0 then
    Now32 := UnixTimeMinimalUtc;
  if (cc.head.issued > Now32) or
     (cc.head.expires <= Now32) then
    exit;
  dec(len, SizeOf(cc.head));
  fSafe.Lock; // protect fAes instance
  try
    if not fAes.Reset(@cc.head.session, 12) or // unique sequence-based IV
       not fAes.Add_AAD(@cc.head.session, 3 * SizeOf(cardinal)) or
       not fAes.Decrypt(@cc.data, @cc.data, len, @cc.head.gmac, SizeOf(THash128)) then
      exit;
    if Invalidate then // expired cookies are stored as 64-bit session + expires
    begin
      if AddSortedInt64(fContext.Invalid, fInvalidCount, PInt64(@cc.head.session)^) < 0 then
        exit; // this session was already invalidated
    end
    else if fInvalidCount <> 0 then
    begin
      if Now32 >= fNextUnixTimeMinimalInvalidateCheck then
      begin // cleanup of deprecated invalid sessions once per minute
        fNextUnixTimeMinimalInvalidateCheck := Now32 + SecsPerMin;
        RemoveSortedInt64SmallerThan(fContext.Invalid, fInvalidCount,
          Int64(Now32) shl 32);
      end;
      if FastFindInt64Sorted(pointer(fContext.Invalid), fInvalidCount - 1,
           PInt64(@cc.head.session)^) >= 0 then // branchless O(log(n)) search
        exit; // this cookie was marked as closed/invalid
    end;
  finally
    fSafe.UnLock;
  end;
  if PExpires <> nil then
    PExpires^ := QWord(cc.head.expires) + UNIXTIME_MINIMAL;
  if PIssued <> nil then
    PIssued^ := QWord(cc.head.issued) + UNIXTIME_MINIMAL;
  if (PRecordData <> nil) and
     (PRecordTypeInfo <> nil) then
  begin
    if len = 0 then
      exit; // expects a record, but no associated data
    ccend := PAnsiChar(@cc.data) + len;
    if PtrUInt(ccend - BinaryLoad(PRecordData, @cc.data, PRecordTypeInfo, nil,
         ccend, rkRecordTypes)) > AES_MOD then
      exit;
  end;
  result := cc.head.session;
end;

function TBinaryCookieGenerator.Save: RawUtf8;
begin
  SetLength(fContext.Invalid, fInvalidCount);
  result := RecordSaveBase64(fContext, TypeInfo(TBinaryCookieContext));
end;

function TBinaryCookieGenerator.Load(const Saved: RawUtf8): boolean;
begin
  Finalize(fContext);
  result := RecordLoadBase64(pointer(Saved), length(Saved), fContext,
      TypeInfo(TBinaryCookieContext), false, @JSON_[mFast]) and
    (fContext.Version = 1) and
    (fContext.SessionSequenceStart <> 0) and
    (fContext.SessionSequence >= fContext.SessionSequenceStart) and
    (fContext.CrcSalt <> 0) and
    not IsZero(fContext.CryptKey);
  fInvalidCount := length(fContext.Invalid);
  if result then
    fAes.Init(fContext.CryptKey, 128, {avx=}false);
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
            (PosEx('-----END', pem, i + 10) <> 0); // rough analysis
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

var
  PEM_LEN: array[TPemKind] of byte; // number of meaningful chars

function PemHeader(lab: PUtf8Char): TPemKind;
begin
  if PEM_LEN[high(TPemKind)] = 0 then // compute once
    for result := low(result) to high(result) do
      PEM_LEN[result] := PosEx('--', PEM_BEGIN[result], 14) - 12;
  for result := succ(low(result)) to high(result) do
    if IdemPropNameUSameLenNotNull(@PEM_BEGIN[result][12], lab, PEM_LEN[result]) then
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
  if kind <> nil then
    kind^ := pemUnspecified;
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

function PemToDer(const pem: TCertPem; const kind: PPemKind): TCertDer;
var
  P: PUtf8Char;
begin
  P := pointer(pem);
  result := NextPemToDer(P, kind);
  if result = '' then
    result := pem; // if content is not PEM, assume its a DER binary
end;

function NextPemToDer(var P: PUtf8Char; Kind: PPemKind): TCertDer;
var
  len: PtrInt;
  pem: PUtf8Char;
  base64: TSynTempBuffer; // PEM are small, so a 4KB temp buffer is fine enough
begin
  pem := ParsePem(P, kind, len, {excludemarkers=}true);
  if pem <> nil then
  begin
    base64.Init(len);
    len := Base64IgnoreLineFeeds(pem, base64.buf) - base64.buf;
    result := Base64ToBinSafe(base64.buf, len);
    base64.Done;
  end
  else
    FastAssignNew(result);
end;

function NextPem(var P: PUtf8Char; Kind: PPemKind): TCertPem;
var
  len: PtrInt;
  pem: PUtf8Char;
begin
  pem := ParsePem(P, Kind, len, {excludemarkers=}false);
  if pem = nil then
    FastAssignNew(result)
  else
    FastSetString(RawUtf8(result), pem, len);
end;

function PemToCertAndPrivKey(const MultiPartPem: RawUtf8;
  out Cert, PrivKey: RawByteString): boolean;
var
  P: PUtf8Char;
  pem: RawUtf8;
  k: TPemKind;
begin
  result := false;
  P := pointer(MultiPartPem);
  repeat
    pem := NextPem(P, @k);
    if pem = '' then
      break;
    if k = pemCertificate then // pemSynopseCertificate is out-of-scope here
      if {%H-}Cert <> '' then
        exit                   // should contain a single Certificate
      else
        Cert := PemToDer(pem)
    else
      PrivKey := pem; // private key may be with several TPemKind markers
    FillZero(pem);
  until false;
  result := ({%H-}Cert <> '') and
            ({%H-}PrivKey <> '');
end;

function PemToCert(const MultiPartPem: RawUtf8): RawByteString;
var
  P: PUtf8Char;
  pem: RawUtf8;
  k: TPemKind;
begin
  FastAssignNew(result);
  P := pointer(MultiPartPem);
  repeat
    pem := NextPem(P, @k);
    if pem = '' then
      break;
    if k = pemCertificate then // pemSynopseCertificate is out-of-scope here
      result := PemToDer(pem);
    FillZero(pem);
  until result <> ''; // stop at the first valid certificate (assume leaf)
end;

function DerAppend(P: PAnsiChar; buf: PByteArray; buflen: PtrUInt): PAnsiChar;
var
  pos, prefix: PtrUInt;
begin
  pos := 0;
  while buf[pos] = 0 do // ignore leading zeros
  begin
    inc(pos);
    dec(buflen);
    if buflen = 0 then
      break;
  end;
  prefix := buf[pos] shr 7; // detect if need to avoid two's complement storage
  P[0] := AnsiChar(ASN1_INT);
  P[1] := AnsiChar(buflen + prefix);
  P[2] := #$00; // prepend 0 to prevent stored as negative number (if prefix=1)
  inc(P, 2 + prefix);
  MoveFast(buf[pos], P^, buflen);
  result := P + buflen;
end;

function DerParse(P: PAnsiChar; buf: PByteArray; buflen: PtrInt): PAnsiChar;
var
  pos: PtrInt;
begin
  result := nil;
  FillZero(buf^, buflen);
  if (P = nil) or
     (P[0] <> AnsiChar(ASN1_INT)) or
     (P[1] = #0) then
    exit;
  pos := buflen - ord(P[1]);
  inc(P, 2);
  if P^ = #0 then
  begin
    inc(P); // positive INTEGER with leading zero
    inc(pos);
  end;
  if pos < 0 then
    exit; // INTEGER larger than destination buffer
  dec(buflen, pos);
  MoveFast(P^, buf[pos], buflen);
  result := P + buflen;
end;

function PrivateKeyEncrypt(const Input, Salt: RawByteString;
  const PrivatePassword: SpiUtf8; AfSplitRounds, Pbkdf2Rounds: integer): RawByteString;
var
  pks: RawByteString;
begin
  if PrivatePassword = '' then
    result := Input
  else
  begin
    pks := TAesPrng.Main.AFSplit(Input, AfSplitRounds);
    result := AesPkcs7(pks, {encrypt=}true, PrivatePassword, Salt, Pbkdf2Rounds);
    FillZero(pks);
  end;
end;

function PrivateKeyDecrypt(const Input, Salt: RawByteString;
  const PrivatePassword: SpiUtf8; AfSplitRounds, Pbkdf2Rounds: integer): RawByteString;
var
  pks: RawByteString;
begin
  if PrivatePassword = '' then
    result := Input
  else
  begin
    pks := AesPkcs7(Input, {encrypt=}false, PrivatePassword, Salt, Pbkdf2Rounds);
    result := TAesPrng.AFUnSplit(pks, AfSplitRounds);
    FillZero(pks);
  end;
end;


const
  ASN1_OID_PBES2      = '1.2.840.113549.1.5.13';
  ASN1_OID_PBKDF2     = '1.2.840.113549.1.5.12';
  ASN1_OID_HMACSHA1   = '1.2.840.113549.2.7';
  ASN1_OID_HMACSHA224 = '1.2.840.113549.2.8';
  ASN1_OID_HMACSHA256 = '1.2.840.113549.2.9';
  ASN1_OID_HMACSHA384 = '1.2.840.113549.2.10';
  ASN1_OID_HMACSHA512 = '1.2.840.113549.2.11';
  ASN1_OID_AES128CBC  = '2.16.840.1.101.3.4.1.2';
  ASN1_OID_AES192CBC  = '2.16.840.1.101.3.4.1.22';
  ASN1_OID_AES256CBC  = '2.16.840.1.101.3.4.1.42';
  PBKDF2_MAXROUNDS    = 10000000; // avoid DoS from malicious input

function Pkcs8DecryptPrivateKey(const Input: RawByteString;
  const Password: SpiUtf8): RawByteString;
var
  der, oid, salt, iv, enc, key: RawByteString;
  pos, kdfend, t: integer;
  rounds, keylen: Int64;
  keybits: cardinal;
  prf: TSignAlgo;
begin
  FastAssignNew(result);
  if (Input = '') or
     (Password = '') then
    exit;
  der := PemToDer(Input);
  try
    // encryptedPrivateKeyInfo ::= SEQUENCE {
    //   encryptionAlgorithm AlgorithmIdentifier {{ PBES2 }},
    //   encryptedData OCTET STRING }
    pos := 1;
    if (AsnNext(pos, der) <> ASN1_SEQ) or
       (AsnNext(pos, der) <> ASN1_SEQ) or
       (AsnNext(pos, der, @oid) <> ASN1_OBJID) or
       (oid <> ASN1_OID_PBES2) or
       // PBES2-params ::= SEQUENCE {
       //   keyDerivationFunc AlgorithmIdentifier {{ PBKDF2 }},
       //   encryptionScheme AlgorithmIdentifier {{ AES-CBC }} }
       (AsnNext(pos, der) <> ASN1_SEQ) or
       (AsnNext(pos, der) <> ASN1_SEQ) or
       (AsnNext(pos, der, @oid) <> ASN1_OBJID) or
       (oid <> ASN1_OID_PBKDF2) or
       // PBKDF2-params ::= SEQUENCE {
       //   salt OCTET STRING, iterationCount INTEGER,
       //   keyLength INTEGER OPTIONAL, prf AlgorithmIdentifier DEFAULT SHA1 }
       (AsnNext(pos, der, nil, @kdfend) <> ASN1_SEQ) or
       (AsnNextRaw(pos, der, salt) <> ASN1_OCTSTR) then
      exit;
    rounds := AsnNextInteger(pos, der, t);
    if (t <> ASN1_INT) or
       (rounds <= 0) or
       (rounds > PBKDF2_MAXROUNDS) then
      exit;
    keylen := 0;
    if (pos < kdfend) and
       (ord(der[pos]) = ASN1_INT) then
    begin
      keylen := AsnNextInteger(pos, der, t);
      if t <> ASN1_INT then
        exit;
    end;
    prf := saSha1; // default value
    if pos < kdfend then
    begin
      if (AsnNext(pos, der) <> ASN1_SEQ) or
         (AsnNext(pos, der, @oid) <> ASN1_OBJID) then
        exit;
      if oid = ASN1_OID_HMACSHA256 then
        prf := saSha256
      else if oid = ASN1_OID_HMACSHA384 then
        prf := saSha384
      else if oid = ASN1_OID_HMACSHA512 then
        prf := saSha512
      else if oid = ASN1_OID_HMACSHA224 then
        prf := saSha224
      else if oid <> ASN1_OID_HMACSHA1 then
        exit;
    end;
    pos := kdfend; // ignore any optional NULL parameter of prf
    if (AsnNext(pos, der) <> ASN1_SEQ) or
       (AsnNext(pos, der, @oid) <> ASN1_OBJID) then
      exit;
    if oid = ASN1_OID_AES256CBC then
      keybits := 256
    else if oid = ASN1_OID_AES128CBC then
      keybits := 128
    else if oid = ASN1_OID_AES192CBC then
      keybits := 192
    else
      exit; // e.g. legacy des-ede3-cbc
    if ((keylen <> 0) and
        (keylen <> keybits shr 3)) or
       (AsnNextRaw(pos, der, iv) <> ASN1_OCTSTR) or
       (length(iv) <> SizeOf(TAesBlock)) or
       (AsnNextRaw(pos, der, enc) <> ASN1_OCTSTR) or
       (enc = '') or
       (length(enc) and (SizeOf(TAesBlock) - 1) <> 0) then
      exit;
    // derivate the AES key from the password, then decrypt the PrivateKeyInfo
    key := Pbkdf2(prf, Password, salt, integer(rounds), keybits shr 3);
    result := AesPkcs7(enc, {encrypt=}false, pointer(key)^, keybits, mCbc,
      pointer(iv));
    if (result <> '') and
       (ord(result[1]) <> ASN1_SEQ) then
    begin
      FillZero(result); // wrong password with (unlikely) valid padding
      FastAssignNew(result);
    end;
  finally
    FillZero(key);
    FillZero(der);
  end;
end;

function Pkcs8EncryptPrivateKey(const PrivateKeyInfo: RawByteString;
  const Password: SpiUtf8; Rounds: integer): RawByteString;
var
  salt, iv, key, enc: RawByteString;
begin
  FastAssignNew(result);
  if (PrivateKeyInfo = '') or
     (ord(PrivateKeyInfo[1]) <> ASN1_SEQ) or
     (Password = '') or
     (Rounds <= 0) or
     (Rounds > PBKDF2_MAXROUNDS) then
    exit;
  // random salt and IV, then PBKDF2-HMAC-SHA256 key derivation
  salt := TAesPrng.Fill(16);
  iv := TAesPrng.Fill(SizeOf(TAesBlock));
  key := Pbkdf2(saSha256, Password, salt, Rounds, 32);
  try
    enc := AesPkcs7(PrivateKeyInfo, {encrypt=}true, pointer(key)^, 256, mCbc,
      pointer(iv));
    // encryptedPrivateKeyInfo with PBES2-params, as OpenSSL does
    result := Asn(ASN1_SEQ, [
      Asn(ASN1_SEQ, [
        AsnOid(ASN1_OID_PBES2),
        Asn(ASN1_SEQ, [
          Asn(ASN1_SEQ, [
            AsnOid(ASN1_OID_PBKDF2),
            Asn(ASN1_SEQ, [
              AsnOctStr(salt),
              Asn(Rounds),
              Asn(ASN1_SEQ, [
                AsnOid(ASN1_OID_HMACSHA256),
                ASN1_NULL_VALUE
              ])
            ])
          ]),
          Asn(ASN1_SEQ, [
            AsnOid(ASN1_OID_AES256CBC),
            AsnOctStr(iv)
          ])
        ])
      ]),
      AsnOctStr(enc)
    ]);
  finally
    FillZero(key);
  end;
end;


function HashForChannelBinding(const CertRaw: TCertDer;
  const SignatureHashAlgo: RawUtf8; out Hash: THash512Rec): integer;
var
  h: THashAlgo;
  hasher: TSynHasher;
begin // see https://datatracker.ietf.org/doc/html/rfc5929#section-4.1
  result := 0;
  if (CertRaw = '') or
     not TextToHashAlgo(SignatureHashAlgo, h) then
    exit;
  if h in [hfMD5, hfSHA1] then
    h := hfSHA256; // avoid weak algorithm (as per RFC - but keep hfSHA224)
  result := hasher.Full(h, pointer(CertRaw), length(CertRaw), Hash);
end;

function Rfc3961Nfold(const input: RawByteString; olen: cardinal): RawByteString;
var
  ilen, lcm, ibits, msbit, divi, c: cardinal;
  i: PtrUInt;
  res: PByteArray;
  p: PByte;
begin
  // prepare the result string for 13-bit right rotation as in RFC 3161
  res := FastNewRawByteString(result, olen);
  FillCharFast(res^, olen, 0);
  ilen := length(input);
  ibits := ilen shl 3;
  lcm := (ilen * olen) div gcd(ilen, olen);
  c := 0;
  for i := lcm - 1 downto 0 do
  begin
    // compute the msbit in k which gets added into this byte
    divi := i div ilen;
    msbit := (
       // first, start with the msbit in the first, unrotated byte
       (ibits - 1)
       // then, for each byte, shift to the right for each repetition
       + ((ibits + 13) * divi)
       // last, pick out the correct byte within that shifted repetition
       + ((ilen - (i - (divi * ilen))) shl 3) ) mod ibits;
    // pull out the byte value itself
    inc(c, (
       ((PByteArray(input)[((ilen - 1) - (msbit shr 3)) mod ilen] shl 8) or
         PByteArray(input)[(ilen - (msbit shr 3)) mod ilen])
       shr ((msbit and 7) + 1) ) and 255);
    // do the addition and truncate to 255-bit
    p := @res[i mod olen];
    inc(c, p^);
    p^ := c;
    // keep around the carry bit, if any
    c := c shr 8;
  end;
  // if there's a carry bit left over, add it back in
  if c <> 0 then
    for i := olen - 1 downto 0 do
    begin
      // do the addition and truncate to 255-bit
      inc(c, res[i]);
      res[i] := c;
      // keep around the carry bit, if any
      c := c shr 8;
      if c = 0 then
        break;
    end;
end;

// see https://www.rfc-editor.org/rfc/rfc3962 and
//     https://www.rfc-editor.org/rfc/rfc8009#section-4
function MakeKerberosKeySeed(const PassPhrase, Salt: RawUtf8;
  EncType, Iterations: integer; Hmac: PSignAlgo): RawByteString;
var
  keysize: integer; // in bytes
  algo: TSignAlgo;
  finalSalt: RawByteString;
begin
  FastAssignNew(result);
  if (PassPhrase = '') or
     (Salt = '') or
     (Iterations < 0) or
     (Iterations > 50000) then
    exit;
  case EncType of
    ENCTYPE_AES128_CTS_HMAC_SHA1_96:
      begin
        algo := saSha1;
        keysize := SizeOf(THash128);
      end;
    ENCTYPE_AES256_CTS_HMAC_SHA1_96:
      begin
        algo := saSha1;
        keysize := SizeOf(THash256);
      end;
    ENCTYPE_AES128_CTS_HMAC_SHA256_128:
      begin
        algo := saSha256;
        keysize := SizeOf(THash128);
      end;
    ENCTYPE_AES256_CTS_HMAC_SHA384_192:
      begin
        algo := saSha384;
        keysize := SizeOf(THash256);
      end;
  else
    exit; // unsupported EncType (yet)
  end;
  if Hmac <> nil then
    Hmac^ := algo;
  if Iterations = 0 then // use the RFC default
    if algo = saSha1 then
      Iterations := 4096
    else
      Iterations := 32768;
  if algo = saSha1 then
    finalSalt := Salt
  else
    finalSalt := Join([ENCTYPE_NAME[EncType], #0, Salt]);
  result := Pbkdf2(algo, PassPhrase, finalSalt, Iterations, keysize);
end;

const // = pre-computed Rfc3961Nfold('kerberos', 32)
  KERBEROS_NFOLD: array[0 .. 7] of cardinal = ($6272656b, $736f7265,
   $2b5b9b7b, $932b1393, $dadc9b5c, $99985cd9, $dee4cac4, $e4cad6e6);

function Rfc3962SeedtoKey(const Seed: RawByteString; EncType: integer;
  DK: pointer): RawByteString;
var
  keysize: integer;
  aes: TAesCbc;
  p: PHash256Rec;
  h0, h1: THash256Rec;
begin
  FastAssignNew(result);
  case EncType of
    ENCTYPE_AES128_CTS_HMAC_SHA1_96:
      keysize := SizeOf(THash128);
    ENCTYPE_AES256_CTS_HMAC_SHA1_96:
      keysize := SizeOf(THash256);
  else
    exit;
  end;
  if length(Seed) <> keysize then
    exit;
  if DK = nil then
    DK := @KERBEROS_NFOLD; // e.g. from MakeKerberosKey()
  p := FastNewRawByteString(result, keysize);
  aes := TAesCbc.Create(pointer(Seed)^, keysize shl 3);
  try // DK(seed, 'kerberos') with AES-128 or AES-256 as in RFC 3962 section 4
    aes.Encrypt(DK, @h0, keysize); // no AES-CTS here! :(
    p^.Lo := h0.Lo;
    if keysize = SizeOf(THash256) then
    begin
      aes.IVFillZero;
      aes.Encrypt(@h0, @h1, keysize);
      p^.Hi := h1.Lo;
    end;
  finally
    aes.Free;
    FillZero(h0.b);
    FillZero(h1.b)
  end;
end;

function Rfc8009SeedtoKey(const Seed: RawByteString; EncType: integer;
  const KdfLabel: RawByteString): RawByteString;
var
  hmac: TSynSigner;
  algo: TSignAlgo;
  keysize: cardinal;
begin
  FastAssignNew(result);
  case EncType of
    ENCTYPE_AES128_CTS_HMAC_SHA256_128:
      begin
        keysize := SizeOf(THash128);
        algo := saSha256;
      end;
    ENCTYPE_AES256_CTS_HMAC_SHA384_192:
      begin
        keysize := SizeOf(THash256);
        algo := saSha384;
      end
  else
    exit;
  end;
  result := hmac.KdfSP800(algo, keysize, Seed, KdfLabel);
end;

function MakeKerberosKey(const PassPhrase, Salt: RawUtf8;
  EncType, Iterations: integer): RawByteString;
var
  tkey: RawByteString;
begin
  FastAssignNew(result);
  tkey := MakeKerberosKeySeed(PassPhrase, Salt, EncType, Iterations);
  if tkey = '' then
    exit;
  case EncType of
    ENCTYPE_AES128_CTS_HMAC_SHA1_96,
    ENCTYPE_AES256_CTS_HMAC_SHA1_96:
      result := Rfc3962SeedtoKey(tkey, EncType);
    ENCTYPE_AES128_CTS_HMAC_SHA256_128,
    ENCTYPE_AES256_CTS_HMAC_SHA384_192:
      result := Rfc8009SeedtoKey(tkey, EncType);
  end;
  FillZero(tkey);
end;

function MakeKerberosKeyEntry(var aEntry: TKerberosKeyEntry;
  const aPrincipal, aSalt: RawUtf8; const aPassword: SpiUtf8;
  aIsComputer: boolean; aEncType, aIterations: integer): boolean;
var
  realm, name, salt: RawUtf8;
begin
  result := false;
  if not Split(aPrincipal, '@', name, realm) then
    exit;
  UpperCaseSelf(realm);
  aEntry.Timestamp := UnixTimeUtc;
  aEntry.EncType := aEncType;
  aEntry.KeyVersion := 1;
  aEntry.NameType := 1;
  aEntry.Principal := Join([name, '@', realm]); // normalize
  if aSalt <> '' then
    salt := aSalt
  else if aIsComputer then // see [MS-KILE] 3.1.1.2 Cryptographic Material
  begin
    if EndWithExact(name, '$') then
      SetLength(name, Length(name) - 1);
    Join([realm, 'host', LowerCaseU(name), '.', LowerCaseU(realm)], salt);
  end
  else
    Join([realm, name], salt);
  aEntry.Key := MakeKerberosKey(aPassword, salt, aEncType, aIterations);
  result := aEntry.Key <> '';
end;

{ TKerberosKeyTabGenerator }

function TKerberosKeyTabGenerator.AddNew(const aPrincipal: RawUtf8;
  const aPassword: SpiUtf8; aIsComputer: boolean; const aSalt: RawUtf8;
  aEncType, aIterations: integer): boolean;
var
  e: TKerberosKeyEntry;
begin
  result := MakeKerberosKeyEntry(e, aPrincipal, aSalt, aPassword,
              aIsComputer, aEncType, aIterations) and
            Add(e);
  FillZero(e.Key); // anti-forensic
end;

class function TKerberosKeyTabGenerator.Generate(const aPrincipal: RawUtf8;
  const aPassword: SpiUtf8; aIsComputer: boolean; const aSalt: RawUtf8;
  aEncType: integer): RawByteString;
var
  gen: TKerberosKeyTabGenerator;
begin
  FastAssignNew(result);
  gen := TKerberosKeyTabGenerator.Create;
  try
    if gen.AddNew(aPrincipal, aPassword, aIsComputer, aSalt, aEncType) then
      result := gen.SaveToBinary;
  finally
    gen.Free;
  end;
end;


function X509PubKeyFromDer(const PkcsDer: RawByteString): RawByteString;
var
  pos: integer;
  algoseq: TAsnBuffer; // algorithm OID(s) as encoded by CkaToSeq()
begin
  pos := 1;
  if (AsnNext(pos, PkcsDer) <> ASN1_SEQ) or
     (AsnNextBuffer(pos, PkcsDer, algoseq) <> ASN1_SEQ) or // CkaToSeq()
     (AsnNextRaw(pos, PkcsDer, result) <> ASN1_BITSTR) then
    FastAssignNew(result);
end;

function X509PubKeyBits(const SubjectPublicKey: RawByteString;
  PubText: PRawUtf8): integer;
var
  pub: PByte;
  pos, publen: integer;
  modulo, exp: RawByteString;
  name, bits: RawUtf8;
begin
  pub := pointer(SubjectPublicKey);
  publen := length(SubjectPublicKey);
  result := publen;
  if result <> 0 then
    case SubjectPublicKey[1] of
      #$04:
        begin
          // ECC uncompressed key
          inc(pub);
          dec(publen);
          result := publen shr 1;
          if PubText <> nil then
            name := 'ECC ';
        end;
      #$30:
        begin
          // RSA sequence
          pos := 1;
          if (AsnNext(pos, SubjectPublicKey) = ASN1_SEQ) and
             AsnNextBigInt(pos, SubjectPublicKey, modulo) and
             AsnNextBigInt(pos, SubjectPublicKey, exp) then
          begin
            result := length(modulo);
            if PubText <> nil then
            begin
              name := 'RSA ';
              Join(['      Modulus:'#13#10,
                BinToHumanHex(pointer(modulo), length(modulo), 16, 8),
                '      Exponent: 0x', BinToHex(exp), #13#10], bits);
            end;
          end;
        end;
    end;
  result := result shl 3; // from bytes to bits
  if PubText = nil then
    exit;
  if {%H-}bits = '' then
    bits := BinToHumanHex(pub, publen, 16, 6);
  FormatUtf8('    %Public Key: (% bit)'#13#10'%', [{%H-}name, result, bits], PubText^);
end;


function AsnTime(dt: TDateTime): TAsnObject;
var
  t: TSynSystemTime;
{%H-}begin
  if dt = 0 then
  begin
    result := AsnTyped('99991231235959Z', ASN1_GENTIME);
    exit;
  end;
  t.FromDateTime(dt);
  if t.Year > 1900 then
    if (t.Year <= 2000) or
       (t.Year >= 2050) then
       result := Asn(ASN1_GENTIME, [Make([
        UInt4DigitsToShort(t.Year),
        UInt2DigitsToShortFast(t.Month),
        UInt2DigitsToShortFast(t.Day),
        UInt2DigitsToShortFast(t.Hour),
        UInt2DigitsToShortFast(t.Minute),
        UInt2DigitsToShortFast(t.Second),
        'Z'])])
    else
      result := Asn(ASN1_UTCTIME, [Make([
        UInt2DigitsToShortFast(t.Year - 2000),
        UInt2DigitsToShortFast(t.Month),
        UInt2DigitsToShortFast(t.Day),
        UInt2DigitsToShortFast(t.Hour),
        UInt2DigitsToShortFast(t.Minute),
        UInt2DigitsToShortFast(t.Second),
        'Z'])])
  else
    ESynCrypto.RaiseUtf8('Invalid AsnTime(%)', [dt]);
end;

function AsnNextTime(var Pos: integer; const Buffer: TAsnObject;
  out Value: TDateTime): boolean;
var
  vt: PtrInt;
  raw: TAsnBuffer;
  tmp: TShort63;
begin
  vt := AsnNextBuffer(pos, Buffer, raw);
  result := false;
  if (raw.Len < 12) or (raw.Len > 60) then
    exit;
  tmp[0] := #0;
  case vt of
    ASN1_UTCTIME:
      PCardinal(@tmp)^ := 2 + ord('2') shl 8 + ord('0') shl 16; // YY -> '20YY'
    ASN1_GENTIME:
      if EqualBuf('99991231235959Z', raw.Data, raw.Len) then
      begin
        Value := 0; // special value for unspecified NotAfter
        result := true;
        exit;
      end;
  else
    exit;
  end;
  vt := 8 - ord(tmp[0]);
  AppendShortBuffer(raw.Data, vt, high(tmp), @tmp);
  AppendShortChar('T', @tmp); // make ISO-8601 compatible 'YYYYMMDDThhmmss'
  AppendShortBuffer(PAnsiChar(raw.Data) + vt, raw.Len - vt, high(tmp), @tmp);
  Iso8601ToDateTimePUtf8CharVar(@tmp[1], ord(tmp[0]), Value);
  result := Value <> 0;
end;

function AsnNextGeneralName(var Pos: integer; const Buffer: TAsnObject;
  var Name: RawUtf8): boolean;
var
  v: TAsnBuffer;
begin
  case AsnNextBuffer(Pos, Buffer, v) of // most GeneralName types decoding
    ASN1_NULL: // no more items
      begin
        result := false;
        exit;
      end;
    ASN1_CTX1, // rfc8722Name
    ASN1_CTX2, // dnsName
    ASN1_CTX6: // uri
      FastSetString(Name, v.Data, v.Len); // was stored as IA5String
    ASN1_CTX7: // ip
      AsnDecIp(v.Data, v.Len, Name);
    // ASN1_CTX4 = directoryName = TXName
  else
    FastAssignNew(Name);  // unsupported value type
  end;
  result := true;
end;

procedure AsnDecIp(p: PAnsiChar; len: integer; var text: RawUtf8);
begin
  case len of
    4:
      with PDWordRec(p)^ do
        FormatUtf8('%.%.%.%', [B[0], B[1], B[2], B[3]], text);
   16:
     // expanded IPv6 xx:xx:xx:...:xx content (no mormot.net.sock dependency)
     ToHumanHex(text, pointer(p), len);
  else
    BinToHexLower(p, len, text);
  end;
end;

function AsnDecAia(const ext: TAsnObject; var ocsp, issuers: TRawUtf8DynArray;
  nofilter: boolean): boolean;
var
  pos: integer;
  oid, v: RawUtf8;
begin // see xeAuthorityInformationAccess in TXTbsCertificate.AddNextExtensions
  result := false;
  ocsp := nil;
  issuers := nil;
  pos := 1;
  if AsnNext(pos, ext) = ASN1_SEQ then
    while (AsnNext(pos, ext) = ASN1_SEQ) and
          (AsnNext(pos, ext, @oid) = ASN1_OBJID) and // accessMethod
          AsnNextGeneralName(pos, ext, v) do
    begin
      result := true;
      if oid{%H-} = ASN1_OID_AIA_OCSP then
      begin
        if nofilter or
           IsHttp(v{%H-}) then
          AddRawUtf8(ocsp, v);
      end
      else if oid = ASN1_OID_AIA_ISSUERS then
        if nofilter or
           IsHttp(v) or
           IsLdap(v) then
          AddRawUtf8(issuers, v);
    end;
end;

function AsnDecCdp(const ext: TAsnObject; nofilter: boolean): TRawUtf8DynArray;
var
  point, fullname, c0: RawByteString;
  one: RawUtf8;
  pos, ppoint, p: integer;
begin // see xeCrlDistributionPoints in TXTbsCertificate.AddNextExtensions
  result := nil;
  pos := 1;
  if AsnNext(pos, ext) = ASN1_SEQ then
    while AsnNextRaw(pos, ext, point) = ASN1_SEQ do // DistributionPoint SEQ
    begin
      ppoint := 1;
      while ppoint <= Length(point) do
        if AsnNextRaw(ppoint, point, fullname) = ASN1_CTC0 then
        begin // distributionPoint [0] DistributionPointName OPTIONAL
          p := 1;
          if AsnNextRaw(p, fullname, c0) <> ASN1_CTC0 then
            continue; // nameRelativeToCRLIssuer [1] is intentionally ignored
          p:= 1;  // parse fullName [0] GeneralNames
          while AsnNextGeneralName(p, c0, one) do
            if nofilter or
               IsHttp(one) then
              AddRawUtf8(result, one);
        end; // reasons [1] / cRLIssuer [2] are intentionally ignored
    end;
end;

function SecurityDescriptorToJson(const SD: TSecurityDescriptor): RawUtf8;
begin
  SaveJson(SD, TypeInfo(TSecurityDescriptor), [twoIgnoreDefaultInRecord,
    twoEnumSetsAsTextInRecord, twoTrimLeftEnumSets], result);
end;

function SecurityDescriptorFromJson(const Json: RawUtf8;
  out SD: TSecurityDescriptor): boolean;
begin
  SD.Clear;
  result := RecordLoadJson(SD, Json, TypeInfo(TSecurityDescriptor));
end;


{ **************** TSynMustache Cryptographic Expression Helpers }

class procedure TSynMustacheCryptoHelpers.Md5(const Value: variant;
  out Result: variant);
begin
  RawUtf8ToVariant(mormot.crypt.core.Md5(ToUtf8(Value)), Result);
end;

class procedure TSynMustacheCryptoHelpers.Sha1(const Value: variant;
  out Result: variant);
begin
  RawUtf8ToVariant(mormot.crypt.core.Sha1(ToUtf8(Value)), Result);
end;

class procedure TSynMustacheCryptoHelpers.Sha256(const Value: variant;
  out Result: variant);
begin
  RawUtf8ToVariant(mormot.crypt.core.Sha256(ToUtf8(Value)), Result);
end;

class procedure TSynMustacheCryptoHelpers.Sha512(const Value: variant;
  out Result: variant);
begin
  RawUtf8ToVariant(mormot.crypt.core.Sha512(ToUtf8(Value)), Result);
end;

class procedure TSynMustacheCryptoHelpers.PasswordGenerate(const Value: variant;
  out Result: variant);
begin // {{ passwordgenerate len }}
  RawUtf8ToVariant(TAesPrng.Main.RandomPassword(AnyVariantToIntegerDef(Value, 24)));
end;

// some callbacks for custom JSON serialization of security related types

procedure _JL_RawSid(Data: PRawSid; var Ctxt: TJsonParserContext);
var
  tmp: TSid;
begin
  if Ctxt.ParseNext then
    if Ctxt.Value = nil then // null
      Data^ := ''
    else if not Ctxt.WasString or
            not TextToSid(Ctxt.Get.Value, tmp) then
      Ctxt.Valid := false
    else
      ToRawSid(@tmp, Data^);
end;

procedure _JS_RawSid(Data: PSid; const Ctxt: TJsonSaveContext);
var
  tmp: ShortString;
begin
  Ctxt.W.Add('"');
  Data := PPointer(Data)^;
  if Data <> nil then
  begin
    tmp[0] := #0;
    SidAppendShort(Data, tmp); // fast enough
    Ctxt.W.AddShort(tmp);
  end;
  Ctxt.W.AddDirect('"');
end;

procedure _JL_SD(Data: PRawSecurityDescriptor; var Ctxt: TJsonParserContext);
var
  tmp: TSecurityDescriptor;
begin
  if Ctxt.ParseNext then
    if Ctxt.Value = nil then // null
      Data^ := ''
    else if not Ctxt.WasString or
            (tmp.FromText(Ctxt.Get.Value) <> atpSuccess) then
      Ctxt.Valid := false
    else
      Data^ := tmp.ToBinary;
end;

procedure _JS_SD(Data: PRawSecurityDescriptor; const Ctxt: TJsonSaveContext);
var
  tmp: TSecurityDescriptor;
begin
  Ctxt.W.Add('"');
  if (Data^ <> '') and
     tmp.FromBinary(Data^) then
    Ctxt.W.AddJsonEscape(pointer(tmp.ToText)); // write as JSON string of SDDL
  Ctxt.W.AddDirect('"');
end;

procedure _JL_Mask(Data: PSecAccessMask; var Ctxt: TJsonParserContext);
begin
  if Ctxt.ParseNext then
    if Ctxt.Value = nil then // null
      Data^ := []
    else
      Ctxt.Valid := Ctxt.WasString and
                    SddlNextMask(Ctxt.Get.Value, Data^);
end;

procedure _JS_Mask(Data: PSecAccessMask; const Ctxt: TJsonSaveContext);
var
  tmp: ShortString;
begin
  Ctxt.W.Add('"');
  tmp[0] := #0;
  SddlAppendMask(tmp, Data^);
  Ctxt.W.AddShort(tmp);
  Ctxt.W.AddDirect('"');
end;

procedure _JL_SignAlgo(Data: PByte; var Ctxt: TJsonParserContext);
begin
  if Ctxt.ParseNext then
    if Ctxt.WasString then // more tolerant than plain RTTI
      if TextToSignAlgo(Ctxt.Value, Ctxt.ValueLen, TSignAlgo(Data^)) then
        exit
      else
        Ctxt.Valid := jpoIgnoreUnknownEnum in Ctxt.Options
    else
      Ctxt.ValueEnumNotString(Data);
end;

procedure _JL_HashAlgo(Data: PByte; var Ctxt: TJsonParserContext);
begin
  if Ctxt.ParseNext then
    if Ctxt.WasString then // more tolerant than plain RTTI
      if TextToHashAlgo(Ctxt.Value, Ctxt.ValueLen, THashAlgo(Data^)) then
        exit
      else
        Ctxt.Valid := jpoIgnoreUnknownEnum in Ctxt.Options
    else
      Ctxt.ValueEnumNotString(Data);
end;

const
  _TSynSignerParams = 'algo:TSignAlgo secret,salt:RawUtf8 rounds:integer';
  _TSecAce = 'type:TSecAceType flags:TSecAceFlags raw:word mask:TSecAccessMask ' +
    'sid:RawSid opaque:RawUtf8 obj,inh:TGuid';
  _TSecurityDescriptor = 'owner,group:RawSid dacl,sacl:TSecAcl ' +
    'flags:TSecControls modified:TSecurityDescriptorInfos';

procedure InitializeUnit;
begin
  // register proper JSON serialization of security related types
  Rtti.RegisterType(TypeInfo(TSignAlgo)).JsonLoad := @_JL_SignAlgo;
  Rtti.RegisterType(TypeInfo(THashAlgo)).JsonLoad := @_JL_HashAlgo;
  Rtti.RegisterTypes([
    TypeInfo(TSecAceType),
    TypeInfo(TSecAceFlags),
    TypeInfo(TSecAccessMask),
    TypeInfo(TSecControls),
    TypeInfo(TSecurityDescriptorInfos)]);
  TRttiJson.RegisterCustomSerializerFunctions([
    TypeInfo(RawSid),                @_JL_RawSid, @_JS_RawSid,
    TypeInfo(RawSecurityDescriptor), @_JL_SD,     @_JS_SD,
    TypeInfo(TSecAccessMask),        @_JL_Mask,   @_JS_Mask]);
  Rtti.RegisterFromText([
    TypeInfo(TSynSignerParams),    _TSynSignerParams,
    TypeInfo(TSecAcl),             _TSecAce,
    TypeInfo(TSecurityDescriptor), _TSecurityDescriptor]);
end;

procedure FinalizeUnit;
begin
end;


initialization
  InitializeUnit;

finalization
  FinalizeUnit;
  
end.

