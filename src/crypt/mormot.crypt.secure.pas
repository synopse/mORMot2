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
    - Client and Server HTTP Access Authentication
    - 64-bit TSynUniqueIdentifier and its efficient Generator
    - IProtocol Safe Communication with Unilateral or Mutual Authentication
    - TBinaryCookieGenerator Simple Cookie Generator
    - Rnd/Hash/Sign/Cipher/Asym/Cert/Store High-Level Algorithms Factories
    - Minimal PEM/DER Encoding/Decoding

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
  mormot.lib.sspi,   // for WinCertDecode() - void unit on POSIX
  mormot.crypt.core;


{ ***************** Password-Safe and TSynConnectionDefinition Classes }

type
  /// abstract class allowing safe storage of a password in a published property
  // - the associated Password, e.g. for storage or transmission encryption
  // will be persisted encrypted with a private key (which can be customized)
  // - if default simple symmetric encryption is not enough, it will also
  // read passwords strongly obfuscated for a given user using
  // mormot.crypt.core.pas' CryptDataForCurrentUser()
  // - a published property should be defined as such in inherited class:
  // ! property PasswordPropertyName: RawUtf8 read fPassword write fPassword;
  // - use the PassWordPlain property to access to its uncyphered value
  TObjectWithPassword = class(TSynPersistent)
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
// - used e.g. by TObjectWithPassword and mormot.db.proxy to obfuscate
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
    // ! "Value":3137644716930138113,"Hex":"2B8B273F00008001"}
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
  TSynUniqueIdentifierGenerator = class(TSynPersistent)
  protected
    fSafe: TLightLock;
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
  /// hash algorithms available for HashFile/HashFull functions
  // and TSynHasher object
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
    hfShake256);
  /// a pointer to one of our hash algorithms
  PHashAlgo = ^THashAlgo;

  /// set of algorithms available for HashFile/HashFull functions and TSynHasher object
  THashAlgos = set of THashAlgo;

  /// store a hash value and its algorithm, e.g. for THttpPeerCacheMessage.Hash
  // - we store and compare in our implementation the algorithm in addition to
  // the hash, to avoid any potential attack about (unlikely) hash collisions
  // between algorithms, and allow any change of algo restrictions in the future
  THashDigest = packed record
    /// the algorithm used for Hash
    Algo: THashAlgo;
    /// up to 512-bit of raw binary hash, according to Algo
    Bin: THash512Rec;
  end;
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
    ctxt: array[1..SHA3_CONTEXT_SIZE] of byte; // enough space for all algorithms
    fAlgo: THashAlgo; // ctxt is better aligned if put first
    procedure CopyTo(out aHasher: TSynHasher); {$ifdef FPC} inline; {$endif}
    procedure Clear;                           {$ifdef FPC} inline; {$endif}
  public
    /// initialize the internal hashing structure for a specific algorithm
    // - returns false on unknown/unsupported algorithm
    function Init(aAlgo: THashAlgo): boolean;
    /// hash the supplied memory buffer
    procedure Update(aBuffer: pointer; aLen: integer); overload;
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
    TStreamRedirectMd5,        // hfMD5
    TStreamRedirectSha1,       // hfSHA1
    TStreamRedirectSha256,     // hfSHA256
    TStreamRedirectSha384,     // hfSHA384
    TStreamRedirectSha512,     // hfSHA512
    TStreamRedirectSha512_256, // hfSHA512_256
    TStreamRedirectSha3_256,   // hfSHA3_256
    TStreamRedirectSha3_512,   // hfSHA3_512
    TStreamRedirectSha224,     // hfSHA224
    TStreamRedirectSha3_224,   // hfSHA3_224
    TStreamRedirectSha3_384,   // hfSHA3_384
    TStreamRedirectShake128,   // hfShake128
    TStreamRedirectShake256);  // hfShake256)

  /// the standard text of a THashAlgo (in uppercase characters)
  HASH_TXT: array[THashAlgo] of RawUtf8 = (
    'MD5', 'SHA-1', 'SHA-256', 'SHA-384', 'SHA-512', 'SHA-512/256',
    'SHA3-256', 'SHA3-512', 'SHA-224', 'SHA3-224', 'SHA3-384',
    'SHAKE128', 'SHAKE256');
  /// the standard text of a THashAlgo (in lowercase characters)
  HASH_TXT_LOWER: array[THashAlgo] of RawUtf8 = (
    'md5', 'sha-1', 'sha-256', 'sha-384', 'sha-512', 'sha-512/256',
    'sha3-256', 'sha3-512', 'sha-224', 'sha3-224', 'sha3-384',
    'shake128', 'shake256');

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
  /// allow to specify several ModularCryptIdentify/ModularCryptVerify algorithms
  TModularCryptFormats = set of TModularCryptFormat;

const
  /// the standard text of a TSignAlgo
  SIGNER_TXT: array[TSignAlgo] of RawUtf8 = (
    'SHA-1',    'SHA-256',  'SHA-384',  'SHA-512', 'SHA3-224', 'SHA3-256',
    'SHA3-384', 'SHA3-512', 'SHAKE128', 'SHAKE256', 'SHA-224');
  SIGNER_SHA3 = [saSha3224 .. saSha3S256];
  SIGNER_DEFAULT_SALT = 'I6sWioAidNnhXO9BK';
  SIGNER_DEFAULT_ALGO = saSha3S128;
  /// which ModularCryptIdentify/ModularCryptVerify() results are correct
  mcfValid = [mcfMd5Crypt .. high(TModularCryptFormat)];
var
  /// default number of rounds for PBKDF2 "Modular Crypt" functions
  // - numbers adjusted on 2025, and align with OWASP Password Storage Cheat
  // Sheet, NIST SP 800-63B and RFC 8018, and are higher than "$pbkdf2" passlib
  // - typical values on my Core i5-13500 PC with SHA-NI are Pbkdf2Sha1=68.28ms
  // Pbkdf2Sha256=35.89ms Pbkdf2Sha512=110.55ms and Pbkdf2Sha3=112.58ms
  // - made as a global variable, since you can adjust those values for your
  // own purpose, as they are part of the hash text itself
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
    procedure Final(aSignature: PHash512Rec; aNoInit: boolean = false); overload;
    /// one-step digital signature of a buffer as lowercase hexadecimal string
    function Full(aAlgo: TSignAlgo; const aSecret: RawUtf8;
      aBuffer: pointer; aLen: integer): RawUtf8; overload;
    /// one-step digital signature of a buffer with PBKDF2 derivation
    function Full(aAlgo: TSignAlgo; const aSecret, aSalt: RawUtf8;
      aSecretPbkdf2Round: integer; aBuffer: pointer; aLen: integer): RawUtf8; overload;
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
  // - note that SHA-3 or SHA512-256 share the same size with other algos
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
    SizeOf(THash256));     // 32 bytes for hfShake256

  /// map the file extension text of any THashAlgo digest
  // - TextToHashAlgo() is able to recognize those values
  HASH_EXT: array[THashAlgo] of RawUtf8 = (
    '.md5',        // hfMD5
    '.sha1',       // hfSHA1
    '.sha256',     // hfSHA256
    '.sha384',     // hfSHA384
    '.sha512',     // hfSHA512
    '.sha512-256', // hfSHA512_256
    '.sha3-256',   // hfSHA3_256
    '.sha3-512',   // hfSHA3_512
    '.sha224',     // hfSHA224
    '.sha3-224',   // hfSHA3_224
    '.sha3-384',   // hfSHA3_384
    '.shake128',   // hfShake128
    '.shake256');  // hfShake256

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
function HashDigestEqual(const a, b: THashDigest): boolean;
  {$ifdef HASINLINE} inline; {$endif}

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
function ModularCryptHash(const format, password: RawUtf8): RawUtf8; overload;

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
// - use SystemEntropy.Startup as seed for consistent results between calls, but
// eventually reset when the process is restarted
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


{ some HMAC/PBKDF2 common wrappers defined here to redirect to TSynSigner }

/// compute the HMAC message authentication code using any hash function
procedure Hmac(algo: TSignAlgo; key, msg: pointer; keylen, msglen: integer;
  result: PHash512Rec);

/// compute the PBKDF2 derivation of a password using HMAC over any hash function
procedure Pbkdf2(algo: TSignAlgo; const password, salt: RawByteString;
  count: integer; result: PHash512Rec); overload;

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

  /// TBinaryCookieGenerator execution context - to be persisted on disk
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
  TBinaryCookieGenerator = class(TSynPersistent)
  protected
    fSafe: TLightLock;
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
  TCryptAlgo = class(TSynPersistent)
  protected
    fName: RawUtf8;
    // case-insensitive quick lookup of the algorithms into a TCryptAlgo instance
    class function InternalFind(const name: RawUtf8; var Last: TCryptAlgo): pointer;
    class function InternalResolve(const name: RawUtf8; CSV: PUtf8Char): integer;
  public
    /// inherited classes should properly initialize this kind of process
    constructor Create(const name: RawUtf8); reintroduce; virtual;
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
  published
    /// process-wide case-insensitive identifier for quick lookup of the algorithms
    // - typical values may follow OpenSSL naming, e.g. 'MD5', 'AES-128-GCM' or
    // 'prime256v1'
    property AlgoName: RawUtf8
      read fName;
  end;

  /// interface as implemented e.g. by TCryptHash and resolved by Hash() factory
  ICryptHash = interface
    /// iterative process of a memory buffer
    function Update(buf: pointer; buflen: PtrInt): ICryptHash; overload;
    /// iterative process of a memory buffer
    function Update(const buf: RawByteString): ICryptHash; overload;
    /// iterative process of a file content
    procedure UpdateFile(const filename: TFileName);
    /// iterative process of a stream content
    // - return the number of bytes hashed from the input TStream
    function UpdateStream(stream: TStream): Int64;
    /// compute the digest, and return it in a memory buffer
    function Final(digest: pointer; digestlen: PtrInt): PtrInt; overload;
    /// compute the digest, and return it as UTF-8 hexadecimal text
    function Final: RawUtf8; overload;
  end;

  {$M+}
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
  {$M-}

  /// randomness generator parent class, as resolved by Rnd() factory function
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
    /// retrieve a random floating point value in the [0..1) range calling Get32
    function GetDouble: double;
  end;

  /// an abstract TCryptRandom class which will call Get32 as its random source
  TCryptRandom32 = class(TCryptRandom)
  public
    /// will call the Get32 virtual method to fill
    procedure Get(dst: pointer; dstlen: PtrInt); override;
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
    function UpdateStream(stream: TStream): Int64;
    function Final(digest: pointer; digestlen: PtrInt): PtrInt; overload;
    function Final: RawUtf8; overload;
  end;

  /// hashing parent class, as resolved by Hasher() factory function
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
    /// return the THashAlgo equivalency of this hasher
    // - this default implementation returns false meaning that it is unknown
    function HashAlgo(out hasher: THashAlgo): boolean; virtual;
  end;

  /// signing parent class, as resolved by Signer() factory function
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

  /// interface as implemented e.g. by TCryptCipher from TCryptCipherAlgo.New
  // - as resolved by Cipher() Encrypt() or Decrypt() factory functions
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
    // - use TByteDynArray for aeadinfo because TBytes raises a Delphi XE
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

  /// symmetric encryption class, as resolved by CipherAlgo() factory function
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

  /// the algorithms supported by a ICryptPublicKey/ICryptPrivateKey
  // - does not match TCryptAsymAlgo because ckaRsa/ckaRsaPss do not define the
  // hash algorithm needed, so dedicated caaRSxxx/caaPSxxx items are needed
  TCryptKeyAlgo = (
    ckaNone,
    ckaRsa,
    ckaRsaPss,
    ckaEcc256,
    ckaEcc384,
    ckaEcc512,
    ckaEcc256k,
    ckaEdDSA);

  /// asymmetric public-key cryptography parent class, as returned by Asym()
  TCryptAsym = class(TCryptAlgo)
  protected
    fPemPublic, fPemPrivate: byte; // TPemKind as defined below
  public
    /// the high-level asymmetric algorithm used for this certificate
    function KeyAlgo: TCryptKeyAlgo; virtual; abstract;
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
  end;

  /// the supported asymmetric algorithms, following the JWT high-level naming
  // - caaES256, caaES384, caaES512 and caaES256K match OpenSSL EVP_PKEY_EC with
  // prime256v1, NID_secp384r1, NID_secp521r1 and NID_secp256k1 curves
  // - caaRS256, caaRS384 and caaRS512 match OpenSSL EVP_PKEY_RSA with
  // SHA-256, SHA-384 and SHA-512 digest method
  // - caaPS256, caaPS384 and caaPS512 match OpenSSL EVP_PKEY_RSA_PSS with
  // SHA-256, SHA-384 and SHA-512 digest method
  // - caaEdDSA match OpenSSL EVP_PKEY_ED25519 curve
  // - mormot.crypt.ecc unit implements caaES256 with native pascal
  // - mormot.crypt.rsa unit implements all caaRS256 .. caaPS512 algorithms
  // - mormot.crypt.openssl unit implements all those algorithms
  // - our RSA wrappers generate with RSA_DEFAULT_GENERATION_BITS = 2048-bit,
  // but our units can read and manage any other size of existing certificates
  // generated e.g. by OpenSSL or our mormot.crypt.rsa unit
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

  /// set of supported asymmetric algorithms
  TCryptAsymAlgos = set of TCryptAsymAlgo;

  TCryptAbstractKey = class;

  /// abstract interface to a Public Key, as returned by CryptPublicKey[] factory
  ICryptPublicKey = interface
    /// unserialize a public key content
    // - this instance should be void, i.e. just created with no prior Load
    // - will first try from X.509 SubjectPublicKey raw binary, then the main
    // known PEM or DER usual serialization formats
    function Load(Algorithm: TCryptKeyAlgo;
      const PublicKeySaved: RawByteString): boolean;
    /// verify the RSA or ECC signature of a memory buffer
    function Verify(Algorithm: TCryptAsymAlgo;
      Data, Sig: pointer; DataLen, SigLen: integer): boolean; overload;
    /// verify the RSA or ECC signature of a memory buffer
    function Verify(Algorithm: TCryptAsymAlgo;
      const Data, Sig: RawByteString): boolean; overload;
    /// return raw key information as used by TCryptCert.GetKeyParams
    // - for ECC, returns the x,y coordinates
    // - for RSA, x is set to the Exponent (e), and y to the Modulus (n)
    // - return false if there is no compliant key information in the provider
    function GetParams(out x, y: RawByteString): boolean;
    /// use EciesSeal or RSA sealing, i.e. encryption with this public key
    function Seal(const Message: RawByteString;
      const Cipher: RawUtf8): RawByteString;
    /// the high-level asymmetric algorithm used for this public key
    function KeyAlgo: TCryptKeyAlgo;
    /// direct access to the class instance implementing this interface
    function Instance: TCryptAbstractKey;
  end;

  /// abstract interface to a Private Key, as returned by CryptPrivateKey[] factory
  ICryptPrivateKey = interface
    /// unserialized the private key from DER binary or PEM text
    // - this instance should be void, i.e. just created with no prior Load
    // - also ensure the private key do match an associated public key (if not nil)
    // - is able to decode and potentially decrypt a serialized key, with a
    // PKCS#8 Password for OpenSSL, and our proprietary PrivateKeyDecrypt()
    function Load(Algorithm: TCryptKeyAlgo; const AssociatedKey: ICryptPublicKey;
      const PrivateKeySaved: RawByteString; const Password: SpiUtf8): boolean;
    /// create a new private / public key pair
    // - this instance should be void, i.e. just created with no prior Load
    // - returns the associated public key binary in X.509 SubjectPublicKey format
    function Generate(Algorithm: TCryptAsymAlgo): RawByteString;
    /// return the private key as raw binary
    // - follow PKCS#8 PrivateKeyInfo encoding for RSA and prime256v1
    function ToDer: RawByteString;
    /// return the associated public key as stored in a X509 certificate
    function ToSubjectPublicKey: RawByteString;
    /// return the private key in the TCryptCertX509.Save expected format
    // - is able to encode and potentially encrypt a serialized key, with a
    // PKCS#8 Password for OpenSSL, and our proprietary PrivateKeyEncrypt()
    function Save(AsPem: boolean; const Password: SpiUtf8): RawByteString;
    /// sign a memory buffer with RSA or ECC using the stored private key
    function Sign(Algorithm: TCryptAsymAlgo;
      Data: pointer; DataLen: integer): RawByteString; overload;
    /// sign a memory buffer with RSA or ECC using the stored private key
    function Sign(Algorithm: TCryptAsymAlgo;
      const Data: RawByteString): RawByteString; overload;
    /// use EciesSeal or RSA un-sealing, i.e. decryption with this private key
    function Open(const Message: RawByteString;
      const Cipher: RawUtf8): RawByteString;
    /// compute the shared-secret with another public key
    // - by design, ECDHE is only available for ECC
    function SharedSecret(const PeerKey: ICryptPublicKey): RawByteString;
    /// the high-level asymmetric algorithm used for this private key
    function KeyAlgo: TCryptKeyAlgo;
    /// direct access to the class instance implementing this interface
    function Instance: TCryptAbstractKey;
  end;

  /// abstract parent class to TCryptPublicKey and TCryptPrivateKey
  TCryptAbstractKey = class(TInterfacedPersistent)
  protected
    fKeyAlgo: TCryptKeyAlgo;
  public
    // ICryptPublicKey methods
    function KeyAlgo: TCryptKeyAlgo;
    function Instance: TCryptAbstractKey;
  end;

  /// abstract public key parent class, as returned by CryptPublicKey[] factory
  TCryptPublicKey = class(TCryptAbstractKey, ICryptPublicKey)
  protected
    /// verify the signature of a given hash using this public key
    function VerifyDigest(Sig: pointer; Dig: THash512Rec; SigLen, DigLen: integer;
      Hash: THashAlgo): boolean; virtual;
  public
    // ICryptPublicKey methods
    function Load(Algorithm: TCryptKeyAlgo;
      const PublicKeySaved: RawByteString): boolean; virtual; abstract;
    function Verify(Algorithm: TCryptAsymAlgo; Data, Sig: pointer;
      DataLen, SigLen: integer): boolean; overload; virtual;
    function Verify(Algorithm: TCryptAsymAlgo;
      const Data, Sig: RawByteString): boolean; overload;
    function GetParams(out x, y: RawByteString): boolean; virtual; abstract;
    function Seal(const Message: RawByteString;
      const Cipher: RawUtf8): RawByteString; virtual; abstract;
  end;

  /// abstract public key metaclass, as stored by the CryptPublicKey[] factory
  TCryptPublicKeyClass = class of TCryptPublicKey;

  /// abstract private key parent class, as returned by the CryptPrivateKey[] factory
  TCryptPrivateKey = class(TCryptAbstractKey, ICryptPrivateKey)
  protected
    /// default Load() will call PrivateKeyDecrypt() then FromDer()
    function FromDer(algo: TCryptKeyAlgo; const der: RawByteString;
      pub: TCryptPublicKey): boolean; virtual;
    /// sign a memory buffer digest with the stored private key
    function SignDigest(const Dig: THash512Rec; DigLen: integer;
      DigAlgo: TCryptAsymAlgo): RawByteString; virtual;
  public
    // ICryptPrivateKey methods
    function Load(Algorithm: TCryptKeyAlgo; const AssociatedKey: ICryptPublicKey;
      const PrivateKeySaved: RawByteString; const Password: SpiUtf8): boolean; virtual;
    function Generate(Algorithm: TCryptAsymAlgo): RawByteString; virtual; abstract;
    function ToDer: RawByteString; virtual; abstract;
    function ToSubjectPublicKey: RawByteString; virtual; abstract;
    function Save(AsPem: boolean; const Password: SpiUtf8): RawByteString; virtual;
    function Sign(Algorithm: TCryptAsymAlgo;
      Data: pointer; DataLen: integer): RawByteString; overload; virtual;
    function Sign(Algorithm: TCryptAsymAlgo;
      const Data: RawByteString): RawByteString; overload;
    function Open(const Message: RawByteString;
      const Cipher: RawUtf8): RawByteString; virtual; abstract;
    function SharedSecret(
      const PeerKey: ICryptPublicKey): RawByteString; virtual;
  end;

  /// abstract public key metaclass class, as stored by the CryptPrivateKey factory
  TCryptPrivateKeyClass = class of TCryptPrivateKey;

  /// exception class raised by our High-Level Certificates Process
  ECryptCert = class(ESynException);

  /// the known Key Usages for a given Certificate
  // - is an exact match of TX509Usage enumerate in mormot.lib.openssl11.pas
  // and TWinCertUsage in mormot.lib.sspi
  // - usually stored as a 16-bit set
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
  // - stored as its own 16-bit value, with CU_ALL = 65535
  TCryptCertUsages = set of TCryptCertUsage;

  /// the RFC5280-compatible reasons why a Certificate could be revoked
  // - used for each item in a Certificate Revocation List (CRL)
  // - crrNotRevoked (item 7) is not used in the RFC, and used internally here
  TCryptCertRevocationReason = (
    crrUnspecified,            // (0)
    crrCompromised,            // (1)
    crrAuthorityCompromised,   // (2)
    crrUnAffiliated,           // (3)
    crrSuperseded,             // (4)
    crrReplaced,               // (5)
    crrTempHold,               // (6)
    crrNotRevoked,             // (7) - not used in the RFC, but used internally
    crrRemoved,                // (8)
    crrWithdrawn,              // (9)
    crrServerCompromised);     // (10)

  /// the Digital Signature results for a given Certificate
  // - is an exact match of TEccValidity enumerate in mormot.crypt.ecc256r1.pas
  // - see CV_VALIDSIGN constant for verification success
  TCryptCertValidity = (
    cvUnknown,                // (0)
    cvValidSigned,            // (1)
    cvValidSelfSigned,        // (2)
    cvNotSupported,           // (3)
    cvBadParameter,           // (4)
    cvCorrupted,              // (5)
    cvInvalidDate,            // (6)
    cvUnknownAuthority,       // (7)
    cvDeprecatedAuthority,    // (8)
    cvInvalidSignature,       // (9)
    cvRevoked,                // (10)
    cvWrongUsage);            // (11)

  /// a set of Digital Signature results
  TCryptCertValidities = set of TCryptCertValidity;

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

  /// convenient wrapper of X.509 Certificate subject name X.501 fields
  // - not always implemented - mainly our 'syn-es256' certificate won't
  // - as defined in RFC 5280 Appendix A.1
  TCryptCertFields = record
    /// countryName field (OID 2.5.4.6)
    Country: RawUtf8;
    /// stateOrProvinceName field (OID 2.5.4.8)
    State: RawUtf8;
    /// localityName field (OID 2.5.4.7)
    Locality: RawUtf8;
    /// organizationName field (OID 2.5.4.10)
    Organization: RawUtf8;
    /// organizationalUnitName field (OID 2.5.4.11)
    OrgUnit: RawUtf8;
    /// commonName field (OID 2.5.4.3)
    CommonName: RawUtf8;
    /// email field (OID 1.2.840.113549.1.9.1)
    EmailAddress: RawUtf8;
    /// surname field (OID 2.5.4.4)
    SurName: RawUtf8;
    /// givenName field (OID 2.5.4.42)
    GivenName: RawUtf8;
    /// serialNumber field (OID 2.5.4.5)
    // - note that is not the main X.509 certificate serial, but e.g. a Tax Number
    SerialNumber: RawUtf8;
    /// netscapeComment extension (not a field - OID 2.16.840.1.113730.1.13)
    Comment: RawUtf8;
  end;
  PCryptCertFields = ^TCryptCertFields;

  /// the ICryptCert.Load/Save content
  // - cccCertOnly will store the certificate as PEM or DER with its public key
  // - cccCertWithPrivateKey will include the private key to the output,
  // possibly protected by a password
  // - cccPrivateKeyOnly will export the raw private key with no password
  TCryptCertContent = (
    cccCertOnly,
    cccCertWithPrivateKey,
    cccPrivateKeyOnly);

  /// how ICryptCert.Compare() should compare two certificates
  // - ccmSubjectCN/ccmIssuerCN/ccmSubjectAltName/ccmIssuerAltName lookup is
  // case-insensitive
  TCryptCertComparer = (
    ccmInstance,
    ccmSerialNumber,
    ccmSubjectName,
    ccmIssuerName,
    ccmSubjectCN,
    ccmIssuerCN,
    ccmSubjectKey,
    ccmAuthorityKey,
    ccmSubjectAltName,
    ccmIssuerAltName,
    ccmUsage,
    ccmBinary,
    ccmSha1,
    ccmSha256);

  TCryptCert = class;
  TCryptCertAlgo = class;

  /// abstract interface to a Certificate, as returned by Cert() factory
  // - may be X.509 or not, OpenSSL implemented or not, e.g. for syn-es256
  // - note: features and serialization are not fully compatible between engines,
  // but those high-level methods work as expected within each TCryptCertAlgo
  ICryptCert = interface
    /// create a new Certificate instance with its genuine private key
    // - Subjects is given as a CSV text, e.g. 'synopse.info,www.synopse.info'
    // - if Authority is nil, will generate a self-signed certificate, otherwise
    // will use this Authority private key to sign the certificate
    // - ValidDays and ExpireDays are relative to the current time - ValidDays
    // is -1 by default to avoid most clock synch issues
    // - additional information can be passed into Fields (e.g. common name)
    // - return self to be used as a fluent interface, e.g. calling
    // Save(cccPrivateKeyOnly) to persist the newly created private key
    function Generate(Usages: TCryptCertUsages; const Subjects: RawUtf8 = '';
      const Authority: ICryptCert = nil; ExpireDays: integer = 365;
      ValidDays: integer = -1; Fields: PCryptCertFields = nil): ICryptCert;
    /// create a new Certificate instance from a supplied CSR
    // - will first unserialize and verify a self-signed CSR (PEM) content, e.g.
    // as generated by a former TCryptCertAlgo.CreateSelfSignedCsr() call
    // - retrieve the Subjects and Usages as previously set to the CSR
    // - expect either a self-signed certificate as CSR, or a PKCS#10 CSR
    // by the X.509 engines (OpenSSL or mormot.crypt.x509)
    // - if Authority is set, it will sign the certificate, and keep the public
    // key stored in the CSR so that the requester could keep its private key
    // - if Authority is not set, a new key pair is generated to self-sign
    // the certificate, and the public key is stored in the certificate, and
    // the generated keys (public and private) can be retrieved using Save
    // - return self to be used as a fluent interface, or nil if unsupported
    function GenerateFromCsr(const Csr: RawByteString;
      const Authority: ICryptCert = nil; ExpireDays: integer = 365;
      ValidDays: integer = -1): ICryptCert;
    /// the Certificate Genuine Serial Number
    // - e.g. '04:f9:25:39:39:f8:ce:79:1a:a4:0e:b3:fa:72:e3:bc:9e:d6'
    function GetSerial: RawUtf8;
    /// the High-Level Certificate Subject
    // - actual text output depend on the provider used; for instance
    // '/C=US/O=Let''s Encrypt/CN=R3' with OpenSSL, 'CN=R3, C=US, O=Let''s Encrypt'
    // with mormot.crypt.x509 or some Baudot-encoded text with mormot.crypt.ecc
    function GetSubjectName: RawUtf8;
    /// the Low-Level Certificate Main Subject
    // - returns by default the CommonName, e.g. 'synopse.info' from a X.509
    // CN= subject field
    // - can search another Relative Distinguished Name (RDN) e.g. 'O' or 'OU'
    // - if Rdn is a hash, e.g. 'SHA1'/'SHA256', will return the subject digest
    // - if Rdn is 'DER', will return the raw DER issuer value of this subject
    function GetSubject(const Rdn: RawUtf8 = 'CN'): RawUtf8;
    /// an array of all Subject names covered by this Certificate
    // - e.g. ['synopse.info', 'www.synopse.info']
    // - e.g. read from X.509 v3 Subject Alternative Names extension
    function GetSubjects: TRawUtf8DynArray;
    /// the High-Level Certificate Issuer
    // - actual text output depend on the provider used; for instance
    // '/C=US/O=Let''s Encrypt/CN=R3' with OpenSSL, 'CN=R3, C=US, O=Let''s Encrypt'
    // with mormot.crypt.x509 or some Baudot-encoded text with mormot.crypt.ecc
    function GetIssuerName: RawUtf8;
    /// the Low-Level Certificate Main Issuer
    // - returns by default the CommonName, e.g. 'R3' from a X.509 CN= subject field
    // - can search another Relative Distinguished Name (RDN) e.g. 'O' or 'OU'
    // - if Rdn is a hash, e.g. 'SHA1'/'SHA256', will return the issuer digest
    // - if Rdn is 'DER', will return the raw DER issuer value of this certificate
    function GetIssuer(const Rdn: RawUtf8 = 'CN'): RawUtf8;
    /// an array of all Subject names covered by the issuer of this Certificate
    // - e.g. read from X.509 v3 Issuer Alternative Names extension
    function GetIssuers: TRawUtf8DynArray;
    /// the Subject Key Identifier (SKID) of this Certificate
    // - e.g. '14:2E:B3:17:B7:58:56:CB:AE:50:09:40:E6:1F:AF:9D:8B:14:C2:C6'
    // - match the SKID on X.509, or the serial number for syn-es256
    function GetSubjectKey: RawUtf8;
    /// the signing Authority Key Identifier (AKID) of this Certificate
    // - match the AKID on X.509 (so may be '' for a self-signed certificate),
    // or the authority serial number for syn-es256 (so equals GetSubjectKey
    // for a self-signed certificate)
    function GetAuthorityKey: RawUtf8;
    /// check if this certificate has been self-signed
    function IsSelfSigned: boolean;
    /// check if this certificate has been issued by the specified certificate
    // - e.g. on X.509 will efficiently check the certificate AKID with the
    // Authority SKID
    function IsAuthorizedBy(const Authority: ICryptCert): boolean;
    /// compare one Certificate instance with another
    function Compare(const Another: ICryptCert; Method: TCryptCertComparer): integer;
    /// compare two Certificates, which should share the same algorithm
    // - will compare the internal properties and the public key, not the
    // private key: you could e.g. use it to verify that a ICryptCert with
    // HasPrivateSecret=false matches another with HasPrivateSecret=true
    function IsEqual(const Another: ICryptCert): boolean;
    /// the minimum Validity timestamp of this Certificate
    function GetNotBefore: TDateTime;
    /// the maximum Validity timestamp of this Certificate
    function GetNotAfter: TDateTime;
    /// check GetNotBefore/GetNotAfter validity
    // - validate against current UTC date/time if none is specified
    // - a grace period of CERT_DEPRECATION_THRESHOLD (half a day) is applied
    function IsValidDate(date: TDateTime = 0): boolean;
    /// returns true e.g. after TCryptCertAlgo.New but before Generate()
    function IsVoid: boolean;
    /// the Key Usages of this Certificate
    function GetUsage: TCryptCertUsages;
    /// verbose Certificate information, returned as huge text/JSON blob
    function GetPeerInfo: RawUtf8;
    /// the signature algorithm as engine-specific plain text
    // - the first value is the effective security bits of this algorithm
    // - e.g. '128 ecdsa-with-SHA256', '128 RSA-SHA256' or '128 ED25519' on
    // OpenSSL, or '128 syn-es256' for our cryptography
    function GetSignatureInfo: RawUtf8;
    /// compute the hexadecimal fingerprint of this Certificate
    // - is usually the hash of its binary (e.g. DER) serialization
    function GetDigest(Algo: THashAlgo = hfSHA256): RawUtf8;
    /// load a Certificate from a Save() content
    // - use Content to specify the extent of the loaded value
    // - PrivatePassword is used for cccCertWithPrivateKey and cccPrivateKeyOnly
    // - warning: don't forget FillZero() once done with any sensitive input
    function Load(const Saved: RawByteString;
      Content: TCryptCertContent = cccCertOnly;
      const PrivatePassword: SpiUtf8 = ''): boolean;
    /// load a Certificate from a SaveToFile() content
    // - just a wrapper around the Load() method, reading a file from disk
    // and setting the GetFileName method result value
    function LoadFromFile(const Source: TFileName;
      Content: TCryptCertContent = cccCertOnly;
      const PrivatePassword: SpiUtf8 = ''): boolean;
    /// the last Source file name of LoadFromFile()
    function GetFileName: TFileName;
    /// serialize the Certificate as reusable content
    // - use Content to specify the extent of the returned value; e.g. after
    // Generate, this ICryptCert instance will contain both the public and
    // private key, so cccCertWithPrivateKey and cccPrivateKeyOnly content could
    // be used, with an optional PrivatePassword, to save the private key
    // - will use binary by default, but you can set e.g. ccfPem if needed
    // - note that OpenSSL ccfBinary will use PKCS12 encoding, which default
    // encoding did change with OpenSSL 3.X: you may start the PrivatePassword
    // with PKCS12_3DES_PREFIX '3des=' prefix (which will be trimmed) to force
    // legacy PBE-SHA1-3DES as compatible with Windows Server 2012 or MacOS/iOS,
    // or start with PKCS12_AES_PREFIX 'aes=' prefix to force the newer (and
    // safer) AES-256-CBC algorithm on OpenSSL 1.x
    // - warning: don't forget FillZero() once done with any sensitive result
    function Save(Content: TCryptCertContent = cccCertOnly;
      const PrivatePassword: SpiUtf8 = '';
      Format: TCryptCertFormat = ccfBinary): RawByteString;
    /// serialize the Certificate as reusable file content
    // - just a wrapper to store the Save() method result as a file
    // - if Dest is '' then GetFileName value from last LoadFromFile() is used
    procedure SaveToFile(const Dest: TFileName = '';
      Content: TCryptCertContent = cccCertOnly;
      const PrivatePassword: SpiUtf8 = '';
      Format: TCryptCertFormat = ccfBinary);
    /// compute a digital signature of some digital content
    // - memory buffer will be hashed then signed using the private secret key
    // of this certificate instance
    // - you could later verify this text signature according to the public key
    // of this certificate, using ICryptCert.Verify() or ICryptStore.Verify()
    // - this certificate should have the cuDigitalSignature usage
    // - returns '' on failure, e.g. if this Certificate has no private key
    // - returns the binary signature of the Data buffer on success
    function Sign(Data: pointer; Len: integer;
      Usage: TCryptCertUsage = cuDigitalSignature): RawByteString; overload;
    /// compute a digital signature of some digital content
    // - will use the private key of this certificate
    // - just a wrapper around the overloaded Sign() function
    function Sign(const Data: RawByteString;
      Usage: TCryptCertUsage = cuDigitalSignature): RawByteString; overload;
    /// sign this certificate with the private key of one CA
    // - Authority certificate should have the cuKeyCertSign usage
    procedure Sign(const Authority: ICryptCert); overload;
    /// verify a digital signature of some digital content
    // - will use the public key of this certificate
    // - this certificate should have the cuDigitalSignature usage
    // - see ICryptStore.Verify() for a complete CA chain validation
    // - depending on the engine, some errors can be ignored, e.g.
    // cvWrongUsage and cvDeprecatedAuthority with X.509 certificates
    // - certificate expiration date can be specified instead of current time
    function Verify(Sign, Data: pointer; SignLen, DataLen: integer;
      IgnoreError: TCryptCertValidities = [];
      TimeUtc: TDateTime = 0): TCryptCertValidity; overload;
    /// verify a digital signature of some digital content
    // - just a wrapper around the overloaded Verify() function
    function Verify(const Signature, Data: RawByteString;
      IgnoreError: TCryptCertValidities = [];
      TimeUtc: TDateTime = 0): TCryptCertValidity; overload;
    /// verify another certificate signature with this certificate public key
    // (if self-signed), or a supplied Authority reference
    // - Authority certificate should have the cuKeyCertSign usage
    // - mormot.crypt.x509 will cache the last valid Authority for fast process
    function Verify(const Authority: ICryptCert;
      IgnoreError: TCryptCertValidities = [];
      TimeUtc: TDateTime = 0): TCryptCertValidity; overload;
    /// compute a new JWT for a given payload using this certificate private key
    // - will use the private key and Sign() to compute the signature
    // - this certificate should have the cuDigitalSignature usage
    // - same signature than the reusable TJwtAbstract.Compute() method
    // - returns '' on error, e.g. if HasPrivateSecret is false
    function JwtCompute(const DataNameValue: array of const;
      const Issuer: RawUtf8 = ''; const Subject: RawUtf8 = '';
      const Audience: RawUtf8 = ''; NotBefore: TDateTime = 0;
      ExpirationMinutes: integer = 0; Signature: PRawByteString = nil): RawUtf8;
    /// verify a JWT signature from the public key of this certificate
    // - this certificate should have the cuDigitalSignature usage
    // - can optionally return the payload fields and/or the signature
    function JwtVerify(const Jwt: RawUtf8; Issuer, Subject, Audience: PRawUtf8;
      Payload: PDocVariantData = nil; Signature: PRawByteString = nil;
      IgnoreError: TCryptCertValidities = [];
      TimeUtc: TDateTime = 0): TCryptCertValidity;
    /// returns the JSON Web Key (JWT) corresponding to the public key of this
    // certificate
    // - the returned JWK is computed with no whitespace or line breaks before
    // or after any syntaxic elements, and the required members are ordered
    // lexicographically, as expected for a direct thumbprint
    // - typical pattern is '{"crv":..,"kty":"EC","x":..,"y":.. }' for ECC
    // or '{"e":..,"kty":"RSA","n":..}' for RSA
    // - is implemented by default as a wrapper to GetKeyParams() results
    function JwkCompute: RawUtf8;
    /// encrypt a message using the public key of this certificate
    // - only RSA and ES256 algorithms do support this method by now
    // - 'x509-rs*' and 'x509-ps*' RSA algorithms use an OpenSSL Envelope key
    // transport then our EVP_PKEY.RsaSeal encoding (or TRsa.Seal)
    // - both 'x509-es256' and 'syn-es256' use our EciesSeal() ES256 encoding
    // - returns '' if this feature is not supported
    // - certificate should have cuDataEncipherment or cuEncipherOnly usage
    function Encrypt(const Message: RawByteString;
      const Cipher: RawUtf8 = 'aes-128-ctr'): RawByteString;
    /// decrypt a message using the private key of this certificate
    // - not all algorithms support key transport, only RSA and ES256 by now
    // - 'x509-rs*' and 'x509-ps*' RSA algorithms use an OpenSSL Envelope key
    // transport then our EVP_PKEY.RsaOpen decoding (or TRsa.Open)
    // - both 'x509-es256' and 'syn-es256' use our EciesOpen() ES256 decoding
    // - returns '' if this feature is not supported, or Message is incorrect
    // - certificate should have cuDataEncipherment or cuDecipherOnly usage
    function Decrypt(const Message: RawByteString;
      const Cipher: RawUtf8 = 'aes-128-ctr'): RawByteString;
    /// compute a shared secret from the private key of this certificate and the
    // public key of another certificate
    // - used e.g. to initialize network encryption with no key transmission
    // - returns '' if this algorithm doesn't support this feature (e.g. RSA)
    // - both current and pub certificates should have cuKeyAgreement usage
    // - the caller should always apply a cryptographic hash over the result
    function SharedSecret(const pub: ICryptCert): RawByteString;
    /// returns true if this ICryptCert instance holds a private key secret
    function HasPrivateSecret: boolean;
    /// retrieve the public key as raw binary
    // - actual format depend on the TCryptCert class and algorithm involved,
    // but is usually using a DER format
    function GetPublicKey: RawByteString;
    /// retrieve the private key as raw binary, or '' if none
    // - actual format depend on the TCryptCert class and algorithm involved,
    // but is usually using a DER format
    // - warning: don't forget FillZero() once done with this sensitive result
    function GetPrivateKey: RawByteString;
    /// include the raw private key as saved by GetPrivateKey
    // - the private key should match with the public key of the Certificate
    // - any previously stored private key will first be erased, therefore
    // SetPrivateKey('') will wipe any private key currently stored in memory
    // - warning: don't forget FillZero() once done with this sensitive input
    function SetPrivateKey(const saved: RawByteString): boolean;
    /// the high-level asymmetric algorithm used for this certificate
    function AsymAlgo: TCryptAsymAlgo;
    /// the high-level asymmetric algorithm class used for this certificate
    // - i.e. the factory associated with this ICryptCert instance
    function CertAlgo: TCryptCertAlgo;
    /// access to the low-level implementation class instance
    // - used internally to quickly retrieve the TCryptCert from an ICryptCert
    function Instance: TCryptCert;
    /// access to the low-level implementation handle of the certificate
    // - e.g. a PX509 for OpenSsl, a TEccCertificate class for mormot.crypt.ecc,
    // or a TX509 class for mormot.crypt.x509
    // - equals nil if there is no associated certificate yet, e.g. after New
    // - can be assigned e.g. to TNetTlsContext.CertificateRaw for OpenSSL
    function Handle: pointer;
    /// access to the low-level implementation handle of the stored private key
    // - e.g. a PEVP_PKEY for OpenSsl, a PEccPrivateKey for mormot.crypt.ecc,
    // or a ICryptPrivateKey weak reference for mormot.crypt.x509
    // - equals nil if there is no associated private key
    // - can be assigned e.g. to TNetTlsContext.PrivateKeyRaw for OpenSSL
    function PrivateKeyHandle: pointer;
    /// return the public BigInt values associated to the stored private key
    // - as BigInt binaries, ready e.g. for JWS / JSON Web Key responses
    // - for ECC, returns the x,y coordinates
    // - for RSA, x is set to the Exponent (e), and y to the Modulus (n)
    // - return false if there is no compliant key information in the provider
    function GetKeyParams(out x, y: RawByteString): boolean;
  end;

  /// a dynamic array of Certificate interface instances
  ICryptCerts = array of ICryptCert;
  /// holds a Certificate chain, the first being the main certificate
  ICryptCertChain = ICryptCerts;
  /// a pointer to a Certificate interface instance
  PICryptCert = ^ICryptCert;

  /// abstract parent class to implement ICryptCert, as returned by Cert() factory
  // - you should never use this class, but the ICryptCert instances
  // - type is only defined here to be inherited with the actual provider units
  TCryptCert = class(TCryptInstance, ICryptCert)
  protected
    fLastLoadFromFileName: TFileName;
    fIndexer: TObject; // a TCryptCertAbstractList owner for EnsureCanWrite
    procedure RaiseError(const Msg: ShortString); overload; virtual;
    procedure RaiseError(const Fmt: RawUtf8; const Args: array of const); overload;
    procedure RaiseErrorGenerate(const api: ShortString);
    procedure EnsureCanWrite(const Context: ShortString); virtual;
    // used by TCryptCertList.Find and TCryptCertCache.Find
    class procedure InternalFind(Cert: PICryptCert; const Value: RawByteString;
      Method: TCryptCertComparer; Count, MaxCount: integer;
      out Chain: ICryptCerts); virtual;
  public
    // ICryptCert methods
    function Generate(Usages: TCryptCertUsages; const Subjects: RawUtf8;
      const Authority: ICryptCert; ExpireDays, ValidDays: integer;
      Fields: PCryptCertFields): ICryptCert; virtual; abstract;
    function GenerateFromCsr(const Csr: RawByteString;
      const Authority: ICryptCert; ExpireDays, ValidDays: integer): ICryptCert; virtual;
    function GetSerial: RawUtf8; virtual; abstract;
    function GetSubjectName: RawUtf8; virtual; abstract;
    function GetSubject(const Rdn: RawUtf8): RawUtf8; virtual; abstract;
    function GetSubjects: TRawUtf8DynArray; virtual; abstract;
    function GetIssuerName: RawUtf8; virtual; abstract;
    function GetIssuer(const Rdn: RawUtf8): RawUtf8; virtual; abstract;
    function GetIssuers: TRawUtf8DynArray; virtual; abstract;
    function GetSubjectKey: RawUtf8; virtual; abstract;
    function GetAuthorityKey: RawUtf8; virtual; abstract;
    function IsSelfSigned: boolean; virtual; abstract;
    function IsAuthorizedBy(const Authority: ICryptCert): boolean; virtual;
    function Compare(const Another: ICryptCert; Method: TCryptCertComparer): integer; virtual;
    function IsEqual(const Another: ICryptCert): boolean; virtual;
    function GetNotBefore: TDateTime; virtual; abstract;
    function GetNotAfter: TDateTime; virtual; abstract;
    function IsValidDate(date: TDateTime): boolean; virtual;
    function IsVoid: boolean; virtual;
    function GetUsage: TCryptCertUsages; virtual; abstract;
    function GetPeerInfo: RawUtf8; virtual; abstract;
    function GetSignatureInfo: RawUtf8; virtual; abstract;
    function GetDigest(Algo: THashAlgo): RawUtf8; virtual;
    function Load(const Saved: RawByteString; Content: TCryptCertContent;
      const PrivatePassword: SpiUtf8): boolean; virtual; abstract;
    function LoadFromFile(const Source: TFileName; Content: TCryptCertContent;
      const PrivatePassword: SpiUtf8): boolean; virtual;
    function GetFileName: TFileName; virtual;
    function Save(Content: TCryptCertContent; const PrivatePassword: SpiUtf8;
      Format: TCryptCertFormat): RawByteString; virtual;
    procedure SaveToFile(const Dest: TFileName; Content: TCryptCertContent;
      const PrivatePassword: SpiUtf8; Format: TCryptCertFormat);
    function HasPrivateSecret: boolean; virtual; abstract;
    function GetPublicKey: RawByteString; virtual; abstract;
    function GetPrivateKey: RawByteString; virtual; abstract;
    function SetPrivateKey(const saved: RawByteString): boolean; virtual; abstract;
    function Sign(Data: pointer; Len: integer; Usage: TCryptCertUsage): RawByteString;
      overload; virtual; abstract;
    function Sign(const Data: RawByteString;
      Usage: TCryptCertUsage = cuDigitalSignature): RawByteString;
      overload; virtual;
    procedure Sign(const Authority: ICryptCert); overload; virtual; abstract;
    function Verify(Sign, Data: pointer; SignLen, DataLen: integer;
      IgnoreError: TCryptCertValidities; TimeUtc: TDateTime): TCryptCertValidity;
      overload; virtual; abstract;
    function Verify(const Signature, Data: RawByteString;
      IgnoreError: TCryptCertValidities; TimeUtc: TDateTime): TCryptCertValidity;
      overload; virtual;
    function Verify(const Authority: ICryptCert;
      IgnoreError: TCryptCertValidities; TimeUtc: TDateTime): TCryptCertValidity;
      overload; virtual; abstract;
    function JwtCompute(const DataNameValue: array of const;
      const Issuer, Subject, Audience: RawUtf8; NotBefore: TDateTime;
      ExpirationMinutes: integer; Signature: PRawByteString): RawUtf8; virtual;
    function JwtVerify(const Jwt: RawUtf8; Issuer, Subject, Audience: PRawUtf8;
      Payload: PDocVariantData; Signature: PRawByteString;
      IgnoreError: TCryptCertValidities; TimeUtc: TDateTime): TCryptCertValidity; virtual;
    function JwkCompute: RawUtf8; virtual;
    function Encrypt(const Message: RawByteString;
      const Cipher: RawUtf8): RawByteString; virtual; abstract;
    function Decrypt(const Message: RawByteString;
      const Cipher: RawUtf8): RawByteString; virtual; abstract;
    function SharedSecret(const pub: ICryptCert): RawByteString; virtual;
    function AsymAlgo: TCryptAsymAlgo; virtual;
    function CertAlgo: TCryptCertAlgo; virtual;
    function Instance: TCryptCert;
    function Handle: pointer; virtual; abstract;
    function PrivateKeyHandle: pointer; virtual;
    function GetKeyParams(out x, y: RawByteString): boolean; virtual;
  end;

  /// meta-class of the abstract parent to implement ICryptCert interface
  TCryptCertClass = class of TCryptCert;

  /// abstract parent class for ICryptCert factories
  TCryptCertAlgo = class(TCryptAlgo)
  protected
    fCaa: TCryptAsymAlgo; // should be set by the overriden constructor
  public
    /// main factory to create a new Certificate instance with this algorithm
    // - return a new void instance, ready to call e.g. ICryptCert.Load
    function New: ICryptCert; virtual; abstract;
    /// low-level factory directly from the raw implementation handle
    // - e.g. a PX509 for OpenSsl, or TEccCertificate for mormot.crypt.ecc
    // - warning: ensure Handle is of the expected type, otherwise it will GPF
    // - includes the private key for TEccCertificate, or not for OpenSsl
    // - note that this Handle will be owned by the new ICryptCert instance,
    // so you should not make FromHandle(another.Handle)
    function FromHandle(Handle: pointer): ICryptCert; virtual; abstract;
    /// factory to load a Certificate from a ICryptCert.Save() content
    // - PrivatePassword is needed if the input contains a private key
    // - will only recognize and support the ccfBinary and ccfPem formats
    // - return nil if Saved content was not in the expected format/algorithm
    function Load(const Saved: RawByteString;
      Content: TCryptCertContent = cccCertOnly;
      const PrivatePassword: SpiUtf8 = ''): ICryptCert;
    /// factory to generate a new Certificate instance
    // - just a wrapper around New and ICryptCert.Generate()
    // - Subjects is a mandatory field as with X.509
    function Generate(Usages: TCryptCertUsages; const Subjects: RawUtf8;
      const Authority: ICryptCert = nil; ExpireDays: integer = 365;
      ValidDays: integer = -1; Fields: PCryptCertFields = nil): ICryptCert;
    /// factory for a new Certificate Signing Request over a set of (DNS) names
    // - if PrivateKeyPem is void, will generate a new public/private key pair,
    // then forge a request with this new public key
    // - if PrivateKeyPem is supplied, will use it as public key
    // - you can optionally specify the expected usages and information fields
    // - returns both the private key and the self-signed CSR as PEM
    // - by default, this class returns a self-signed certificate as CSR, but
    // will be overriden by our X.509 engines (OpenSSL and mormot.crypt.x509) to
    // return a proper PKCS#10 standard CSR, in a Let's Encrypt compatible way
    function CreateSelfSignedCsr(const Subjects: RawUtf8;
      const PrivateKeyPassword: SpiUtf8; var PrivateKeyPem: RawUtf8;
      Usages: TCryptCertUsages = [];
      Fields: PCryptCertFields = nil): RawUtf8; virtual;
    /// factory to generate a new Certificate instance from a supplied CSR
    // - will first unserialize and verify a self-self CSR PEM content, as
    // generated by a former CreateSelfSignedCsr() call
    // - retrieve the Subjects and Usages as previously set to the CSR, enabling
    // only UsagesFilter items if set
    // - this default implementation expects the CSR to be a self-signed certificate
    function GenerateFromCsr(const Csr: RawByteString;
      const Authority: ICryptCert = nil; ExpireDays: integer = 365;
      ValidDays: integer = -1): ICryptCert; virtual;
    /// return the corresponding JWT algorithm name, computed from AsymAlgo
    // - e.g. 'ES256' for 'x509-es256' or 'syn-es256-v1'
    function JwtName: RawUtf8;
  published
    /// the asymmetric algorithm used for these certificates
    property AsymAlgo: TCryptAsymAlgo
      read fCaa;
  end;

  TCryptCertCache = class;

  /// abstract interface to a Certificates Store, as returned by Store() factory
  // - may be X.509 or not, OpenSSL implemented or not
  ICryptStore = interface
    /// delete all stored Certificates or CRL information
    procedure Clear;
    /// load a Certificates Store from a ICryptStore.Save memory buffer content
    function Load(const Saved: RawByteString): boolean;
    /// serialize the Certificates Store into a memory buffer
    // - may be our TEccCertificateChain proprietary binary, or a chain of
    // X.509 Certificates and CRLs in PEM text format
    function Save: RawByteString;
    /// get the associated ICryptCert instances cache
    // - use Cache.Load() to retrieve a ICryptCert from its DER/PEM content
    function Cache: TCryptCertCache;
    /// search for a trusted certificate from its (hexadecimal) identifier
    // - note that in the X.509 context, serial may be duplicated, so
    // it is safer to use GetBySubjectKey()
    function GetBySerial(const Serial: RawUtf8): ICryptCert;
    /// search for a trusted certificate from its (hexadecimal) Subject Key Identifier
    // - e.g. '14:2E:B3:17:B7:58:56:CB:AE:50:09:40:E6:1F:AF:9D:8B:14:C2:C6'
    // - search the SKID on X.509, or the serial number for syn-es256
    function GetBySubjectKey(const Key: RawUtf8): ICryptCert;
    /// search for a trusted certificate from a given attribute
    // - return the first certificate matching a given value
    // - warning: some engines don't support this advanced search feature
    function FindOne(const Value: RawByteString;
      Method: TCryptCertComparer): ICryptCert;
    /// quickly check if a given certificate is part of the internal CRL
    // - returns crrNotRevoked is the serial is not known as part of the CRL
    // - returns the reason why this certificate has been revoked otherwise
    function IsRevoked(const cert: ICryptCert): TCryptCertRevocationReason;
    /// register a certificate in the internal certificate chain
    // - returns false e.g. if the certificate was not valid, or its serial was
    // already part of the internal list, or not of a compatible class
    // - self-signed certificates could be included - but add them with caution
    // because they will become root CA, or "trust anchors" in X.509 terminology
    // - the Certificate should have cuCA or cuKeyCertSign typical usages
    function Add(const cert: ICryptCert): boolean; overload;
    /// register several certificates in the internal certificate chain
    // - returns the serials of added certificate(s)
    function Add(const cert: array of ICryptCert): TRawUtf8DynArray; overload;
    /// load and register a certificate or certificate chain from a memory buffer
    // - returns the serials of added certificate(s)
    // - if there are any valid CRL, they will also be loaded to the store
    function AddFromBuffer(const Content: RawByteString): TRawUtf8DynArray;
    /// load and register a certificate file or file chain
    // - returns the serials of added certificate(s)
    // - the Certificate(s) should have cuCA or cuKeyCertSign typical usages
    // - if there are any valid CRL, they will also be loaded to the store
    function AddFromFile(const FileName: TFileName): TRawUtf8DynArray;
    /// search and register all certificate files from a given folder
    // - returns the serials of added certificate(s)
    // - the Certificate(s) should have cuCA or cuKeyCertSign typical usages
    // - if there are any valid CRL, they will also be loaded to the store
    function AddFromFolder(const Folder: TFileName;
      const Mask: TFileName = FILES_ALL; Recursive: boolean = false): TRawUtf8DynArray;
    /// add a Certificate information to the global Certificate Revocation List
    // - on some engines (our internal ECC, but not OpenSSL), Reason=crrNotRevoked
    // could be used to unregister a certificate revocation
    function Revoke(const Cert: ICryptCert; Reason: TCryptCertRevocationReason;
      RevocationDate: TDateTime = 0): boolean;
    /// check if the certificate is valid, against known certificates chain
    // - will check internal properties of the certificate (e.g. validity dates),
    // and validate the stored digital signature according to the public key of
    // the associated signing authority, as found within the store, for as
    // many level as needed until a self-signed "root anchor" is reached
    function IsValid(const cert: ICryptCert;
      date: TDateTime = 0): TCryptCertValidity;
    /// check a certificate against its supplied chain and known certificates
    // - with a large PKI as on the Internet, a certificates chain is usually
    // supplied for authentication with only some "trust anchors" certificates
    // - this overloaded method accept a chain as input, so that the first
    // item is to be validated against the other members of the chain as
    // intermediates (not part of the store), then eventually validating the
    // last items of the chain with the store trusted certificates
    function IsValidChain(const chain: ICryptCertChain;
      date: TDateTime = 0): TCryptCertValidity;
    /// verify the digital signature of a given memory buffer
    // - this signature should have come from a previous ICryptCert.Sign() call
    // - will check internal properties of the certificate (e.g. validity dates),
    // and validate the stored signature according to the public key of
    // the associated signing authority (which should be in this Store)
    // - warning: only supported by our 'syn-store' algorithm: OpenSSL Store
    // has no way to lookup the X.509 certificate which actually signed the buffer
    function Verify(const Signature: RawByteString; Data: pointer; Len: integer;
      IgnoreError: TCryptCertValidities = [];
      TimeUtc: TDateTime = 0): TCryptCertValidity; overload;
    /// verify the digital signature of a given memory string
    function Verify(const Signature, Data: RawByteString;
      IgnoreError: TCryptCertValidities = [];
      TimeUtc: TDateTime = 0): TCryptCertValidity; overload;
    /// how many trusted certificates are currently stored
    function Count: integer;
    /// how many CRLs are currently stored
    function CrlCount: integer;
    /// return the prefered algo to be used with this store
    // - call e.g. CertAlgo.New to prepare a new ICryptCert to add to this store
    function DefaultCertAlgo: TCryptCertAlgo;
  end;

  /// abstract parent class to implement ICryptCert, as returned by Cert() factory
  TCryptStore = class(TCryptInstance, ICryptStore)
  protected
    fCache: TCryptCertCache;
  public
    destructor Destroy; override;
    // ICryptStore methods
    procedure Clear; virtual; abstract;
    function Load(const Saved: RawByteString): boolean; virtual;
    function Save: RawByteString; virtual; abstract;
    function Cache: TCryptCertCache; virtual;
    function GetBySerial(const Serial: RawUtf8): ICryptCert; virtual; abstract;
    function GetBySubjectKey(const Key: RawUtf8): ICryptCert; virtual; abstract;
    function FindOne(const Value: RawByteString;
      Method: TCryptCertComparer): ICryptCert; virtual;
    function IsRevoked(const cert: ICryptCert): TCryptCertRevocationReason; virtual; abstract;
    function Add(const cert: ICryptCert): boolean; overload; virtual; abstract;
    function Add(const cert: array of ICryptCert): TRawUtf8DynArray; overload; virtual;
    function AddFromBuffer(const Content: RawByteString): TRawUtf8DynArray; virtual; abstract;
    function AddFromFile(const FileName: TFileName): TRawUtf8DynArray; virtual;
    function AddFromFolder(const Folder, Mask: TFileName;
       Recursive: boolean): TRawUtf8DynArray; virtual;
    function Revoke(const Cert: ICryptCert; Reason: TCryptCertRevocationReason;
       RevocationDate: TDateTime): boolean; virtual; abstract;
    function IsValid(const cert: ICryptCert;
      date: TDateTime): TCryptCertValidity; virtual; abstract;
    function IsValidChain(const chain: ICryptCertChain;
      date: TDateTime): TCryptCertValidity; virtual;
    function Verify(const Signature: RawByteString; Data: pointer; Len: integer;
      IgnoreError: TCryptCertValidities; TimeUtc: TDateTime): TCryptCertValidity;
        overload; virtual; abstract;
    function Verify(const Signature, Data: RawByteString;
      IgnoreError: TCryptCertValidities = [];
      TimeUtc: TDateTime = 0): TCryptCertValidity; overload;
    function Count: integer; virtual; abstract;
    function CrlCount: integer; virtual; abstract;
    function DefaultCertAlgo: TCryptCertAlgo; virtual;
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
    /// return the prefered algo to be used with this store
    // - should be the same class as ICryptStore.DefaultCertAlgo
    function DefaultCertAlgo: TCryptCertAlgo; virtual; abstract;
  end;

  /// abstract parent of TCryptCertList and TCryptCertCache storage classes
  TCryptCertAbstractList = class(TSynPersistent)
  protected
    fList: TSynDictionary; // thread-safe RawByteString(SKID/DER)/ICryptCert
    fCryptCertClass: TCryptCertClass;
    procedure SetCryptCertClass(c: TCryptCertClass); // from TCryptStore.Create
    function GetCount: integer;
      {$ifdef HASINLINE} inline; {$endif}
  public
    // finalize the ICryptCert storage
    destructor Destroy; override;
    /// search the internal list for a given attribute
    // - return all the certificates matching a given value
    // - will use brute-force O(n) search algorithm with lockfree multi-read
    function Find(const Value: RawByteString;
      Method: TCryptCertComparer = ccmSerialNumber;
      MaxCount: integer = 0): ICryptCerts; virtual;
    /// search the internal list for a given attribute
    // - return the first certificate matching a given value
    function FindOne(const Value: RawByteString;
      Method: TCryptCertComparer = ccmSerialNumber): ICryptCert; virtual;
    /// return a copy of the internal list items
    function List: ICryptCerts;
    /// persist all stored Certificates in PEM format
    procedure SaveToPem(W: TTextWriter; WithExplanatoryText: boolean = false);
    /// direct low-level to the internal raw dictionary
    // - store a hash table of ICryptCert values
    // - for TCryptCertCache, RawByteString keys are DER certificates content
    // - for TCryptCertList, RawByteString keys are SKID/GetSubjectKey binary
    // - use rather the List function if you just want to access the stored values
    property RawList: TSynDictionary
      read fList;
  published
    /// the class of TCryptCert currently stored in this list
    // - is either set in the overriden constructor, or retrieved at runtime
    property CryptCertClass: TCryptCertClass
      read fCryptCertClass;
    /// how many instances are currently stored in this instance
    property Count: integer
      read GetCount;
  end;

  /// store several ICryptCert instances
  // - those instances are likely to come from a TCryptCertCache holder
  // - maintain a hashed index of ICryptCert.GetSubjectKey values for
  // fast certification path validation e.g. during ICryptStore.IsValid
  TCryptCertList = class(TCryptCertAbstractList)
  public
    /// initialize the ICryptCert storage
    constructor Create; override;
    /// include once a X.509 Certificate instance to the internal list
    // - return false if its GetSubjectKey was already present
    function Add(const Cert: ICryptCert): boolean; overload;
    /// include once several X.509 Certificate instances to the internal list
    procedure Add(const Cert: array of ICryptCert); overload;
    /// search the list for a ICryptCert.GetSubjectKey using a hashed index
    // - i.e. the Subject Key Identifier (SKID) of a X.509 Certificate or
    // the serial number for syn-ecc
    function FindBySubjectKey(const Key: RawUtf8): ICryptCert;
    /// search the list for binary ICryptCert.GetSubjectKey using a hashed index
    // - could be used instead of FindBySubjectKey() if the key is already
    // decoded into its HumanHexToBin() raw binary format (as stored internally)
    function FindBySubjectKeyRaw(const Key: RawByteString): ICryptCert;
      {$ifdef HASINLINE} inline; {$endif}
    /// search the internal list for a given attribute
    // - overriden to let ccmSubjectKey search use the hashed index
    function Find(const Value: RawByteString;
      Method: TCryptCertComparer = ccmSerialNumber;
      MaxCount: integer = 0): ICryptCerts; override;
    /// remove a ICryptCert from the list using its indexed GetSubjectKey
    function DeleteBySubjectKey(const Key: RawUtf8): boolean;
  end;

  /// abstract class to cache ICryptCert instances, from their DER/binary
  // - should be overriden to let its InternalLoad() method be implemented
  // - to speed up typical PKI process, no DER parsing would be necessary
  // - this class is thread-safe and will flush its oldest entries automatically
  // - use TCryptCertCacheX509 or TCryptCertCacheOpenSsl, not this abstract class
  TCryptCertCache = class(TCryptCertAbstractList)
  protected
    function OnDelete(const aKey, aValue; aIndex: integer): boolean;
    // this abstract method should be properly overriden to load a DER buffer
    function InternalLoad(const Cert: RawByteString): ICryptCert; virtual; abstract;
  public
    /// instantiate a ICryptCert instances cache
    // - you can have several TCryptCertCache, dedicated to each bounded context
    // - by default, internal cache will clean up instances with RefCnt = 1
    // after 10 min of inactivity
    constructor Create(TimeOutSeconds: integer = 10 * 6); reintroduce;
    /// retrieve a potentially shared ICryptCert instance from DER or PEM input
    // - returns nil if the input is not correct or not supported
    // - will guess the proper TCryptCertAlgoX509 to use for the ICryptCert
    function Load(const Cert: RawByteString): ICryptCert; overload;
    /// retrieve a chain of ICryptCert instances from an array of DER input
    // - any invalid Cert[] will just be ignored and not part of the result
    function Load(const Cert: array of RawByteString): ICryptCerts; overload;
    /// retrieve a chain of ICryptCert instances from a PEM input
    // - any invalid chunk in the PEM will be ignored and not part of the result
    function LoadPem(const Pem: RawUtf8): ICryptCerts;
    /// search the internal list for a given attribute
    // - overriden to let ccmBinary search use the hashed index
    function Find(const Value: RawByteString;
      Method: TCryptCertComparer = ccmSerialNumber;
      MaxCount: integer = 0): ICryptCerts; override;
    /// allocate a new TCryptCertList instance in the context of this cache
    function NewList: TCryptCertList; overload; virtual;
    /// allocate a new TCryptCertList instance in the context of this cache
    // - filling the list with certificates from some PEM input
    function NewList(const Pem: RawUtf8): TCryptCertList; overload;
  end;


/// append a ICryptCert to a certificates chain
procedure ChainAdd(var chain: ICryptCertChain; const cert: ICryptCert);
  {$ifdef HASINLINE} inline; {$endif}

/// search for a ICryptCert to a certificates chain
// - will search for the ICryptCert instance itself, or by TCryptCertComparer
function ChainFind(var chain: ICryptCertChain; const cert: ICryptCert;
  comparer: TCryptCertComparer = ccmInstance): PtrInt;

/// sort a certificate chain by mutual authentication
// - returns the certificates in IsAuthorizedBy() order
function ChainConsolidate(const chain: ICryptCertChain): ICryptCertChain;


type
  /// maintains a list of ICryptCert, easily reachable per TCryptCertUsage
  // - could be seen as a basic certificates store or "PKI of the poor" (tm)
  // - per usage lookup is in O(1) so faster than iterative ICryptCert.GetUsage
  // - also features simple PEM / binary serialization methods
  // - no CRL nor complex intermediate certificates lookup are available
  // - should be initialized by Clear at startup, or zeroed as a class field
  {$ifdef USERECORDWITHMETHODS}
  TCryptCertPerUsage = record
  {$else}
  TCryptCertPerUsage = object
  {$endif USERECORDWITHMETHODS}
  public
    /// the stored ICryptCert Instances
    List: ICryptCerts;
    /// all usages currently stored in this list
    Usages: TCryptCertUsages;
    /// lookup table used by GetUsage()/PerUsage()
    // - 0 means no certificate, or store the index in List[] + 1
    Index: array[TCryptCertUsage] of byte;
    /// reset all storage and indexes
    procedure Clear;
    /// quickly check if there is no stored certificate
    function IsVoid: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// register the certificate to the internal list
    // - returns the duplicated usages
    // - if no usage(s) was already set as in the added one, returns []
    // - if another certificate has already an usage, it is overwritten and
    // the duplicated usage(s) are returned
    // - so the typical pattern is to add the certificate in inverse order of
    // authority, i.e. first the CA as root cuKeyCertSign, then the less
    // specialized cuKeyCertSign certificates - so that the weaker certificate
    // is returned by PerUsage/GetUsage for the actual process
    function Add(const cert: ICryptCert): TCryptCertUsages;
    /// fast lookup of a certificate per its usage
    // - i.e. returns the last/weakest certificate having the supplied usage
    function GetUsage(u: TCryptCertUsage; var cert: ICryptCert): boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// fast lookup of a certificate per its usage
    // - i.e. returns the last/weakest certificate having the supplied usage
    function PerUsage(u: TCryptCertUsage): ICryptCert;
      {$ifdef HASINLINE} inline; {$endif}
    /// save all items as a cccCertOnly CRLF separated list of PEM certificates
    function AsPem: RawUtf8;
    /// clear and load a CRLF separated list of PEM certificates
    // - returns the duplicated usages found during adding certificates
    function FromPem(algo: TCryptCertAlgo; const pem: RawUtf8): TCryptCertUsages;
    /// save all items as a cccCertOnly binary blob of certificates
    // - binary layout is TBufferWriter.WriteVar() of all DER serialization
    function AsBinary: RawByteString;
    /// clear and load a binary blob of certificates saved by AsBinary
    // - returns the duplicated usages found during adding certificates
    function FromBinary(algo: TCryptCertAlgo; const bin: RawByteString): TCryptCertUsages;
  end;


const
  /// our units generate RSA keypairs with 2048-bit by default
  // - anything lower than 2048-bit is unsafe and should not be used
  // - 2048-bit is today's norm, creating 112-bit of security
  // - 3072-bit is supposed to be supported up to 2030, with 128-bit of security
  // - 4096-bit has no security advantage, just slower process
  // - 7680-bit is highly impractical (e.g. generation can be more than 30 secs)
  // and offers only 192-bit of security, so other algorithms may be preferred
  RSA_DEFAULT_GENERATION_BITS = 2048;

  /// the JWT algorithm names according to our known asymmetric algorithms
  // - as implemented e.g. by mormot.crypt.jwt
  CAA_JWT: array[TCryptAsymAlgo] of RawUtf8 = (
    'ES256',      // caaES256
    'ES384',      // caaES384
    'ES512',      // caaES512
    'ES256K',     // caaES256K
    'RS256',      // caaRS256
    'RS384',      // caaRS384
    'RS512',      // caaRS512
    'PS256',      // caaPS256
    'PS384',      // caaPS384
    'PS512',      // caaPS512
    'EdDSA');     // caaEdDSA

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

  /// the THashAlgo according to our known asymmetric algorithms
  CAA_HF: array[TCryptAsymAlgo] of THashAlgo = (
    hfSHA256,     // caaES256
    hfSHA384,     // caaES384
    hfSHA512,     // caaES512
    hfSHA256,     // caaES256K
    hfSHA256,     // caaRS256
    hfSHA384,     // caaRS384
    hfSHA512,     // caaRS512
    hfSHA256,     // caaPS256
    hfSHA384,     // caaPS384
    hfSHA512,     // caaPS512
    hfSHA512);    // caaEdDSA - SHA-512 is included in the algorithm

  /// the TCryptKeyAlgo according to our known asymmetric algorithms
  CAA_CKA: array[TCryptAsymAlgo] of TCryptKeyAlgo = (
    ckaEcc256,    // caaES256
    ckaEcc384,    // caaES384
    ckaEcc512,    // caaES512
    ckaEcc256K,   // caaES256K
    ckaRsa,       // caaRS256
    ckaRsa,       // caaRS384
    ckaRsa,       // caaRS512
    ckaRsaPss,    // caaPS256
    ckaRsaPss,    // caaPS384
    ckaRsaPss,    // caaPS512
    ckaEdDSA);    // caaEdDSA

  /// the known asymmetric algorithms which implement ECC cryptography
  CAA_ECC = [caaES256, caaES384, caaES512, caaES256K, caaEdDSA];

  /// the known asymmetric algorithms which implement RSA cryptography
  CAA_RSA = [caaRS256, caaRS384, caaRS512, caaPS256, caaPS384, caaPS512];

  /// the known asymmetric algorithms which expects no ASN1_SEQ in JWT/JWS
  CAA_RAWSIGNATURE = CAA_RSA + [caaEdDSA];

  /// the known key algorithms which implement ECC cryptography
  CKA_ECC = [ckaEcc256, ckaEcc384, ckaEcc512, ckaEcc256k, ckaEdDSA];

  /// the known key algorithms which implement RSA cryptography
  CKA_RSA = [ckaRsa, ckaRsaPss];

  /// such a Certificate could be used for anything
  CU_ALL = [low(TCryptCertUsage) .. high(TCryptCertUsage)];

  /// such a Certificate could be used for a TLS server authentication
  CU_TLS_SERVER = [cuTlsServer, cuKeyAgreement, cuKeyEncipherment];

  /// such a Certificate could be used for a TLS client authentication
  CU_TLS_CLIENT = [cuTlsClient, cuKeyAgreement, cuKeyEncipherment];

  /// TCryptCertValidity results indicating a valid digital signature
  CV_VALIDSIGN =
    [cvValidSigned, cvValidSelfSigned];

  /// a two-char identifier of Certificate usage
  // - as used by ToText(u: TCryptCertUsages, from_cu_text=true)
  CU_TEXT: array[TCryptCertUsage, 0..1] of AnsiChar = (
    'ca',  //  cuCA
    'eo',  //  cuEncipherOnly
    'rs',  //  cuCrlSign
    'ks',  //  cuKeyCertSign
    'ka',  //  cuKeyAgreement
    'de',  //  cuDataEncipherment
    'ke',  //  cuKeyEncipherment
    'nr',  //  cuNonRepudiation
    'ds',  //  cuDigitalSignature
    'do',  //  cuDecipherOnly
    'ts',  //  cuTlsServer
    'tc',  //  cuTlsClient
    'em',  //  cuEmail
    'cs',  //  cuCodeSign
    'os',  //  cuOcspSign
    'tm'); //  cuTimestamp

  /// standard long identifier of Certificate usage
  // - i.e. match OpenSSL PX509.ExtendedKeyUsage/KeyUsage text
  CU_FULLTEXT: array[TCryptCertUsage] of RawUtf8 = (
    'CA',                            // cuCA
    'Encipher Only',                 // cuEncipherOnly
    'CRL Sign',                      // cuCrlSign
    'Certificate Sign',              // cuKeyCertSign
    'Key Agreement',                 // cuKeyAgreement
    'Data Encipherment',             // cuDataEncipherment
    'Key Encipherment',              // cuKeyEncipherment
    'Non Repudiation',               // cuNonRepudiation
    'Digital Signature',             // cuDigitalSignature
    'Decipher Only',                 // cuDecipherOnly
    'TLS Web Server Authentication', // cuTlsServer
    'TLS Web Client Authentication', // cuTlsClient
    'E-mail Protection',             // cuEmail
    'Code Signing',                  // cuCodeSign
    'OCSP Signing',                  // cuOcspSign
    'Time Stamping');                // cuTimestamp

function ToText(a: TCryptAsymAlgo): PShortString; overload;
function ToText(a: TCryptKeyAlgo): PShortString; overload;
function ToText(r: TCryptCertRevocationReason): PShortString; overload;
function ToText(u: TCryptCertUsage): PShortString; overload;
function ToText(u: TCryptCertUsages; from_cu_text: boolean = false): ShortString; overload;
function ToText(v: TCryptCertValidity): PShortString; overload;

/// return the first usage set, or cuKeyCertSign if [] was supplied
function GetFirstUsage(u: TCryptCertUsages): TCryptCertUsage;

/// fast case-insensitive check of the 'CN' Relative Distinguished Name identifier
function IsCN(const Rdn: RawUtf8): boolean;
  {$ifdef HASINLINE} inline; {$endif}

/// fast case-insensitive check of the 'DER' fake RDNidentifier
function IsDer(const Rdn: RawUtf8): boolean;
  {$ifdef HASINLINE} inline; {$endif}

/// main resolver of the randomness generators
// - a shared TCryptRandom instance is returned: caller should NOT free it
// - e.g. Rnd.GetBytes(100) to get 100 random bytes from 'rnd-default' engine,
// which redirects in fact to our secure TAesPrng.Main generator
// - alternative generators are 'rnd-aes' (same as 'rnd-default'), 'rnd-lecuyer'
// (fast on small content), 'rnd-system'/'rnd-systemblocking' (mapping
// FillSystemRandom, which may be slow and even blocking), 'rnd-delphi' which
// follows the weak but known Delphi RTL Random(), and 'rnd-rdrand' which calls
// the homonymous CPU HW opcode (if cfRAND in CpuFeatures)
// - call Rnd('rnd-entropy').Get() to gather OS entropy (which may be slow),
// optionally as 'rnd-entropysys', 'rnd-entropysysblocking', 'rnd-entropyuser'
function Rnd(const name: RawUtf8 = 'rnd-default'): TCryptRandom;

/// main resolver of the registered hashers
// - hashers ensure a content as not been tempered: use Signer()
// to compute a digital signature from a given secret
// - the shared TCryptHasher of this algorithm is returned: caller should NOT free it
// - if not nil, you could call New or Full/FullFile methods
// - this unit supports 'md5', 'sha1', 'sha256', 'sha384', 'sha512', 'sha3_256',
// 'sha3_512' and 32-bit non-cryptographic 'crc32', 'crc32c', 'xxhash32',
// 'adler32', 'fnv32', 'aesni32', 'md5-32' and 'sha1-32'
function Hasher(const name: RawUtf8): TCryptHasher;

/// main factory of the hashers instances as returned by Hasher()
// - if not nil, caller should call Update then Final
function Hash(const name: RawUtf8): ICryptHash;

/// main resolver of the registered signers
// - in respect to Hasher(), will require a secret for safe digital signature
// - the shared TCryptSigner of this algorithm is returned: caller should NOT free it
// - if not nil, you could call New or Full/FullFile methods
// - this unit supports 'hmac-sha1','hmac-sha256','hmac-sha384','hmac-sha512',
// and 'sha3-224','sha3-256','sha3-384','sha3-512','sha3-s128','sha3-s256'
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
// - name is typically 'syn-es256' or 'x509-es256'
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
  /// the prefered/default algorithm to be used wth X.509 certificates
  // - caaES256 (aka prime256v1 or NISTP-256) seems the new default (faster
  // and with 128-bit of security), even if RSA-2048 (i.e. caaRS256) may still
  // be used for compatiblity with legacy systems (but much slower signing and
  // generation, with only 112-bit of security)
  // - used e.g. by 'x509-store' or 'x509-pki' for its DefaultCertAlgo method
  CryptAlgoDefault: TCryptAsymAlgo = caaES256;


  (* TCryptAsym factories *)

  /// direct access to the internal TCryptAsym factories
  // - may be nil if mormot.crypt.ecc.pas or mormot.crypt.rsa.pas units
  // were not included
  // - you may use rather CryptAsymOpenSsl[] if OpenSSL is available
  CryptAsym: array[TCryptAsymAlgo] of TCryptAsym;

  /// direct access to the mormot.crypt.openssl.pas TCryptAsym factories
  // - may be nil if this unit was not included or if OpenSSL is not available
  // - call RegisterOpenSsl once to initialize this lookup table
  CryptAsymOpenSsl: array[TCryptAsymAlgo] of TCryptAsym;


  (* ICryptPublicKey / ICryptPrivateKey factories *)

  /// RSA/ECC public key factory
  // - implemented e.g. by mormot.crypt.ecc with TCryptPublicKeyEcc,
  // mormot.crypt.rsa with TCryptPublicKeyRsa or mormot.crypt.opensssl with
  // with TCryptPublicKeyOpenSsl
  // - use as such:
  // ! var key: ICryptPublicKey;
  // ! ...
  // !   key := CryptPublicKey[ckaEcc].Create;
  // !   if key.Load(...) then ...
  CryptPublicKey: array[TCryptKeyAlgo] of TCryptPublicKeyClass;

  /// RSA/ECC private key factory
  // - implemented e.g. by mormot.crypt.ecc with TCryptPrivateKeyEcc,
  // mormot.crypt.rsa with TCryptPrivateKeyRsa or mormot.crypt.opensssl with
  // with TCryptPrivateKeyOpenSsl
  // - use as such:
  // ! var key: ICryptPrivateKey;
  // ! ...
  // !   key := CryptPrivateKey[ckaEcc].Create;
  // !   if key.Load(...) then ...
  CryptPrivateKey: array[TCryptKeyAlgo] of TCryptPrivateKeyClass;


  (* ICryptCert factories *)

  /// direct access to the mormot.crypt.ecc.pas 'syn-ecc' algorithm
  // - may be nil if this unit was not included
  CryptCertSyn: TCryptCertAlgo;

  /// direct access to the mormot.crypt.x509.pas ICryptCert factories
  // - may be nil if this unit was not included
  // - to get a new ICryptCert using OpenSSL RSA 2048 key over SHA-256, use e.g.
  // ! CryptCertX509[caaRS256].New
  CryptCertX509: array[TCryptAsymAlgo] of TCryptCertAlgo;

  /// direct access to the mormot.crypt.openssl.pas ICryptCert factories
  // - may be nil if this unit was not included or if OpenSSL is not available
  // - to return a ICryptCert instance using OpenSSL RSA 2048 key, use e.g.
  // ! CryptCertOpenSsl[caaRS256].New
  // - call RegisterOpenSsl once to initialize this lookup table
  CryptCertOpenSsl: array[TCryptAsymAlgo] of TCryptCertAlgo;

  /// direct access to best known X.509 ICryptCert factories
  // - may map to CryptCertX509[] if only mormot.crypt.x509.pas is included
  // - but more standard CryptCertOpenSsl[] will be stored here if available
  CryptCert: array[TCryptAsymAlgo] of TCryptCertAlgo;


  (* ICryptStore factories *)

  /// direct access to the mormot.crypt.ecc.pas 'syn-store' algorithm
  // - may be nil if this unit was not included
  CryptStoreSyn: TCryptStoreAlgo;

  /// direct access to the mormot.crypt.ecc.pas 'syn-store-nocache' algorithm
  // - may be nil if this unit was not included
  CryptStoreSynNoCache: TCryptStoreAlgo;

  /// direct access to the mormot.crypt.x509.pas ICryptStore factory
  CryptStoreX509: TCryptStoreAlgo;

  /// direct access to the mormot.crypt.openssl.pas 'x509-store' algorithm
  // - may be nil if this unit was not included or if OpenSSL is not available
  // - is currently nil because TCryptStoreOpenSsl is not stable yet
  // - call RegisterOpenSsl once to initialize this lookup table
  CryptStoreOpenSsl: TCryptStoreAlgo;



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
    '2.16.840.1.101.3.4.2.12'); // hfShake256

  /// the OID of all ECC public keys (X962)
  // - is stored as prefix to CKA_OID[ckaEcc256..ckaEcc256k] parameter
  ASN1_OID_X962_PUBLICKEY  = '1.2.840.10045.2.1';

  /// the OID of all supported ICryptPublicKey/ICryptPrivateKey algorithms
  CKA_OID: array[TCryptKeyAlgo] of RawUtf8 = (
    '',                       // ckaNone
    '1.2.840.113549.1.1.1',   // ckaRsa
    '1.2.840.113549.1.1.10',  // ckaRsaPss
    '1.2.840.10045.3.1.7',    // ckaEcc256  (with ASN1_OID_X962_PUBLICKEY)
    '1.3.132.0.34',           // ckaEcc384  (with ASN1_OID_X962_PUBLICKEY)
    '1.3.132.0.35',           // ckaEcc512  (with ASN1_OID_X962_PUBLICKEY)
    '1.3.132.0.10',           // ckaEcc256k (with ASN1_OID_X962_PUBLICKEY)
    '1.3.101.112');           // ckaEdDSA

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
function PemToCertAndPrivKey(const MultiPartPem: RawUtf8;
  out Cert, PrivKey: RawByteString): boolean;

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


/// compute the number of security bits of a digital signature
// - RSA security depends on the signature size, not the hash size
// - ECC security size is half of its X,Y coordinates storage size
// - e.g. 112 for RSA-2048, 128 for ECC-256
function GetSignatureSecurityBits(a: TCryptAsymAlgo; len: integer): integer;

/// compute the base-64 encoding of the raw binary of a ASN.1/DER digital signature
// - input comes e.g. from ICryptCert.Sign or ICryptPrivateKey.Sign content
// - base-64 encoded output can be used e.g. for JSON Web Token/Signature (JWT/JWS)
// - RSA and EdDSA signatures are not encoded, so are returnde directly
// - ECC are decoded from their ASN1_SEQ into their raw xy coordinates concatenation
function GetSignatureSecurityRaw(algo: TCryptAsymAlgo;
  const signature: RawByteString): RawUtf8;

/// encode a raw digital signature into ASN.1/DER
// - incoming comes e.g. from base-64 decoded JSON Web Token/Signature (JWT/JWS)
// - output is compatible e.g. with ICryptCert.Verify or ICryptPublicKey.Verify
// - ECC are encoded from their raw x/y coordinates concatenation into ASN1_SEQ
function SetSignatureSecurityRaw(algo: TCryptAsymAlgo;
  const rawsignature: RawUtf8): RawByteString;

/// compute the hash of a certificate as expected by Kerberos Channel Binding
// - use the SignatureHashAlgo, forcing SHA-256 for 'MD5' or 'SHA1'
// - returns the size of the corresponding Hash in bytes, or 0 on error
// - as defined by RFC 5929 - side note: we follow this RFC, which does not take
// into account SHA-224 substitution to SHA-256 as it should
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
  end;

/// raw function to recognize the OID(s) of a public key ASN1_SEQ definition
function OidToCka(const oid, oid2: RawUtf8): TCryptKeyAlgo;

/// raw function to generate a public key ASN1_SEQ definition with its OID(s)
function CkaToSeq(cka: TCryptKeyAlgo): RawByteString;

/// raw function to encode a PKCS#8 PrivateKeyInfo from its raw binary number
// - e.g. as generated by OpenSSL
function EccPrivKeyToSeq(cka: TCryptKeyAlgo; const rawecc: RawByteString): RawByteString;

/// raw function to decode a PKCS#8 PrivateKeyInfo into its raw binary
// - e.g. as generated by OpenSSL
// - is also able to decode RFC 5915 Elliptic Curve key pair alternate format,
// and its optional public key field
function SeqToEccPrivKey(cka: TCryptKeyAlgo; const seq: RawByteString;
  rfcpub: PRawByteString = nil): RawByteString;

/// raw function to decode a specific PKCS#8 PublicKeyInfo into its raw binary
// - e.g. as generated by OpenSSL or X509PubKeyToDer()
// - behaves like X509PubKeyFromDer() but checking for one expected ECC algorithm
function SeqToEccPubKey(cka: TCryptKeyAlgo; const seq: RawByteString): RawByteString;

/// compute the public key ASN.1 from a raw binary as stored in X.509 certificate
// - see e.g. "A.1.1. RSA Public Key Syntax" of RFC 8017
function X509PubKeyToDer(Algorithm: TCryptKeyAlgo;
  const SubjectPublicKey: RawByteString): RawByteString;

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

type
  /// output of the X509Parse() function
  // - contains X.509 certificate main properties and binary public key
  TX509Parsed = record
    /// the certificate Serial Number, stored as 'xx:xx:xx:xx...' hexa text
    Serial: RawUtf8;
    /// the certificate Subject, decoded as RFC 1779 text, with X500 key names
    SubjectDN: RawUtf8;
    /// the certificate Issuer, decoded as RFC 1779 text, with X500 key names
    IssuerDN: RawUtf8;
    /// the certificate Subject ID, stored as 'xx:xx:xx:xx...' hexa text
    SubjectID: RawUtf8;
    /// the certificate Issuer ID, stored as 'xx:xx:xx:xx...' hexa text
    IssuerID: RawUtf8;
    /// the algorithm name of this certificate's digital signature
    SigAlg: RawUtf8;
    /// the algorithm name of this certificate's public key
    PubAlg: RawUtf8;
    /// the certificate Alternate Subject names as a CSV array
    SubjectAltNames: RawUtf8;
    /// human-friendly multi-line text of this certificate main fields
    PeerInfo: RawUtf8;
    /// the main key usages of this certificate
    Usage: TCryptCertUsages;
    /// the certificate validity start date
    NotBefore: TDateTime;
    /// the certificate validity end date
    NotAfter: TDateTime;
    /// the certificate public key ASN1 raw binary as stored in the certificate
    PubKey: RawByteString;
  end;

/// return some multi-line text of the main TX509Parsed fields
// - in a layout similar to X509_print() OpenSSL formatting
// - is used by its own TX509Parsed.PeerInfo field
function ParsedToText(const c: TX509Parsed): RawUtf8;

/// high-level function to decode X.509 Certificate main properties
// - properly implemented by mormot.crypt.openssl or mormot.crypt.x509, but
// this unit will register our mormot.lib.sspi parser as failover on Windows
var
  X509Parse: function(const Cert: RawByteString; out Info: TX509Parsed): boolean;

{$ifdef OSWINDOWS}

/// the raw mormot.lib.sspi parser - published for testing
function WinX509Parse(const Cert: RawByteString; out Info: TX509Parsed): boolean;

/// internal conversion fuction, as used e.g. by WinX509Parse()
procedure WinInfoToParse(const c: TWinCertInfo; out Info: TX509Parsed);

{$endif OSWINDOWS}

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

/// decode an OID ASN.1 IP Address buffer into human-readable text
function AsnDecIp(p: PAnsiChar; len: integer): RawUtf8;

/// human-readable display of a ASN.1 value binary
// - used e.g. by the ASNDEBUG conditional
function AsnDump(const Value: TAsnObject): RawUtf8;

/// serialize a TSecurityDescriptor instance into JSON
function SecurityDescriptorToJson(const SD: TSecurityDescriptor): RawUtf8;

/// unserialize a TSecurityDescriptor instance from JSON
function SecurityDescriptorFromJson(const Json: RawUtf8;
  out SD: TSecurityDescriptor): boolean;


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
      PSha256(@ctxt)^.Init224;
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
    SizeOf(TSha3));

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
    hfSHA224: // SHA-224 is just a truncated SHA-256 result
      begin
        PSha256(@ctxt)^.Final(aDigest.Lo, {aNoInit=}true);
        if not aNoInit then
          PSha256(@ctxt)^.Init224; // but it needs its own re-initialization
      end;
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
  result := '';
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
  result := '';
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
  result := '';
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

{ TStreamRedirectShake256 }

class function TStreamRedirectShake256.GetAlgo: THashAlgo;
begin
  result := hfShake256;
end;

{ TStreamRedirectShake128 }

class function TStreamRedirectShake128.GetAlgo: THashAlgo;
begin
  result := hfShake128;
end;

{ TStreamRedirectSha3_512 }

class function TStreamRedirectSha3_512.GetAlgo: THashAlgo;
begin
  result := hfSHA3_512;
end;

{ TStreamRedirectSha3_224 }

class function TStreamRedirectSha3_224.GetAlgo: THashAlgo;
begin
  result := hfSHA3_224;
end;

{ TStreamRedirectSha3_256 }

class function TStreamRedirectSha3_256.GetAlgo: THashAlgo;
begin
  result := hfSHA3_256;
end;

{ TStreamRedirectSha3_384 }

class function TStreamRedirectSha3_384.GetAlgo: THashAlgo;
begin
  result := hfSHA3_384;
end;

{ TStreamRedirectSha512 }

class function TStreamRedirectSha512.GetAlgo: THashAlgo;
begin
  result := hfSHA512;
end;

{ TStreamRedirectSha512_256 }

class function TStreamRedirectSha512_256.GetAlgo: THashAlgo;
begin
  result := hfSHA512_256;
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

{ TStreamRedirectSha224 }

class function TStreamRedirectSha224.GetAlgo: THashAlgo;
begin
  result := hfSHA224;
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
    SetLength(result, n + 1);
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
    result := ''
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

const
  MCF_IDENT: array[mcfMd5Crypt.. high(TModularCryptFormat)] of RawUtf8 = (
    '1', '5', '6', 'pbkdf2', 'pbkdf2-sha256', 'pbkdf2-sha512', 'pbkdf2-sha3',
    '2b', 'bcrypt-sha256', 'scrypt');

function ModularCryptParse(var P: PUtf8Char; var rounds: cardinal;
  var salt: RawUtf8): TModularCryptFormat;
var
  sLogN, sR, sP: cardinal;
begin
  result := mcfInvalid;
  if (P = nil) or
     (P^ <> '$') then
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
        if not Base64uriValid(pointer(salt), @ConvertBase64ToBin) or
           not Base64uriValid(P, @ConvertBase64ToBin) then
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
  if not b64valid(pointer(salt)) or
     not b64valid(P) then
    result := mcfInvalid;
end; // on success, P points to the {checksum} part

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
    FastSetString(info^, pointer(hash), P - pointer(hash));
end;

function ModularCryptHash(format: TModularCryptFormat; const password: RawUtf8;
  rounds, saltsize: cardinal; const salt: RawUtf8): RawUtf8;
var
  signer: TSynSigner;
  logN, R, P: cardinal;
  hasher: TSynHasher absolute signer;
  dig: THash256 absolute signer;
begin
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
    result := '';
  end;
end;

function ModularCryptHash(const format, password: RawUtf8): RawUtf8;
var
  mcf: TModularCryptFormat;
  P: PUtf8char;
  rounds: cardinal;
  salt: RawUtf8;
begin
  FastAssignNew(result);
  P := pointer(format);
  mcf := ModularCryptParse(P, rounds, salt);
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
  end;
  if (pos = 0) or
     (mormot.core.base.StrComp(checksum, PUtf8Char(pointer(h)) + pos - 1) <> 0) then
    result := mcfInvalid;
end;

function ModularCryptFakeInfo(const id: RawUtf8; format: TModularCryptFormat): RawUtf8;
var
  h: THash256Rec; // always return the same fake content for the same id
  enc: PChar64;
  salt: TShort23;
const
  RANGE = cardinal(high(TModularCryptFormat)) - cardinal(mcfMd5Crypt); // = 9
begin
  HmacSha256(@SystemEntropy.Startup, pointer(id), 16, length(id), h.b);
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
  result := '';
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


{ TSynSigner }

const
  SIGN_SIZE: array[TSignAlgo] of byte = (
    20, 32, 48, 64, 28, 32, 48, 64, 32, 64, 28);
  BLOCK_SIZE: array[TSignAlgo] of byte = (
    15, 15, 31, 31, 0, 0, 0, 0, 0, 0, 15);

procedure TSynSigner.Init(aAlgo: TSignAlgo; aSecret: pointer; aSecretLen: integer);
var
  k0: TBlock1024;
  a: THashAlgo;
begin
  fAlgo := aAlgo;
  a := SIGN_HASH[Algo];
  fSignatureSize := SIGN_SIZE[Algo];
  fBlockMax := BLOCK_SIZE[Algo]; // typically 15 (256-bit) or 31 (512-bit)
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

procedure TSynSigner.Final(aSignature: PHash512Rec; aNoInit: boolean);
begin
  fHasher.Final(aSignature^);
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
  if not (Algo in SIGNER_SHA3) then // padding + XOF mode are part of SHA-3
    // U1 = PRF(secret, salt + INT_32_BE(part))
    UpdateBigEndian(aPartNumber);  // is a 1-based index
  Final(aDerivatedKey, {noinit=}true);
  if aSecretPbkdf2Round <> 0 then
  begin
    // F(secret, salt, c, i) = U1 ^ U2 ^ .. ^ Uc  with Uc = PRF(secret, Uc-1)
    MoveFast(aDerivatedKey^, tmp, fSignatureSize);
    repeat
      MoveFast(bak.ctxt, fHasher.ctxt, HASH_INSTANCE[fHasher.fAlgo]); // restore
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
  result := '';
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
  if (Algo in SIGNER_SHA3) and
     (l > 1) then
    ESynCrypto.RaiseUtf8('TSynSigner.Pbkdf2(%) with DestLen=%: use SHAKE instead',
      [ToText(algo)^, aDestLen]);
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
  result := '';
  if (aPassword = '') or
     not (aAlgo in [low(MCF_SIGN) .. high(MCF_SIGN)]) then
    exit;
  if aRounds = 0 then
    aRounds := MCF_ROUNDS[aAlgo]; // use default of each algorithm
  if aSaltSize = 0 then
    aSaltSize := 16;
  if HASH64_DEC[#255] = 0 then // check the last byte for thread-safe init
    FillBaseDecoder(@HASH64_ENC, @HASH64_DEC, high(HASH64_ENC));
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
  result := '';
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
  case algo of
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
              (PCardinal(@tmp[1])^ and $ffdfdfdf =
                ord('S') + ord('H') shl 8 + ord('A') shl 16 + ord('3') shl 24)) or
             ((tmp[0] = #6) and // '.sha512-256' -> 'sha512_256'
              (PCardinal(@tmp[1])^ and $ffdfdfdf =
                ord('S') + ord('H') shl 8 + ord('A') shl 16 + ord('5') shl 24)) then
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

function HashDigestEqual(const a, b: THashDigest): boolean;
begin
  result := (a.Algo <= high(THashAlgo)) and
            mormot.core.base.CompareMem(@a, @b, HASH_SIZE[a.Algo] + 1);
end;

procedure Hmac(algo: TSignAlgo; key, msg: pointer; keylen, msglen: integer;
  result: PHash512Rec);
var
  signer: TSynSigner;
begin
  signer.Init(algo, key, keylen);
  signer.Update(msg, msglen);
  signer.Final(result);
end;

procedure Pbkdf2(algo: TSignAlgo; const password, salt: RawByteString;
  count: integer; result: PHash512Rec);
var
  signer: TSynSigner;
begin
  signer.Pbkdf2(algo, password, salt, count, result);
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
  GetNextItem(p, ',', '"', RawUtf8(tmp));
  if tmp = '' then
    exit;
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
  result := '';
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
  result := '';
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
  result := '';
  p := pointer(FromServer);
  if IdemPChar(p, 'REALM="') then
    UnQuoteSqlStringVar(p + 6, result);
end;

function DigestServerInit(Algo: TDigestAlgo;
  const QuotedRealm, Prefix, Suffix: RawUtf8; Opaque, Tix64: Int64): RawUtf8;
var
  h: THash128Rec;
  noncehex, opaquehex: string[32];
begin
  result := '';
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
  p: PUtf8Char;
  alg: ShortString;
begin
  result := false;
  p := StrPosI('ALGORITHM=', pointer(FromClient));
  if p = nil then
    exit;
  inc(p, 10);
  GetNextItemShortString(p, @alg);
  result := IdemPropNameU(DIGEST_NAME[fAlgo], @alg[1], ord(alg[0]));
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
  tab := @crc32ctab; // use first 1KB of this 8KB table generated at startup
  for i := 0 to (len shr 2) - 1 do
  begin
    key := key xor tab[0, (cardinal(i) xor key) and 1023];
    d^ := d^ xor key;
    inc(d);
  end;
  for i := 0 to (len and 3) - 1 do
    PByteArray(d)^[i] := PByteArray(d)^[i] xor key xor tab[0, 17 shl i];
end;


{ TObjectWithPassword }

destructor TObjectWithPassword.Destroy;
begin
  FillZero(fPassword);
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
    result := instance.GetPassWordPlainInternal(AppSecret);
  finally
    instance.Free;
  end;
end;

function TObjectWithPassword.GetKey: cardinal;
begin
  if self = nil then
    result := 0
  else
    result := fKey xor $A5abba5A;
end;

function TObjectWithPassword.GetPassWordPlain: SpiUtf8;
begin
  if (self = nil) or
     (fPassWord = '') then
    result := ''
  else
    result := GetPassWordPlainInternal('');
end;

function TObjectWithPassword.GetPassWordPlainInternal(
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
      ECrypt.RaiseUtf8('%.GetPassWordPlain unable to retrieve the ' +
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

procedure TObjectWithPassword.SetPassWordPlain(const Value: SpiUtf8);
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
  ECrypt.RaiseUtf8('%.AuthenticateUser() is not implemented', [self]);
end;

procedure TSynAuthenticationAbstract.DisauthenticateUser(const aName: RawUtf8);
begin
  ECrypt.RaiseUtf8('%.DisauthenticateUser() is not implemented', [self]);
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
    FillZero(key.b);
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
  FillCharFast(fContext, SizeOf(fContext), 0); // anti-forensic measures
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
    data: array[0..2047] of byte; // optional AES-CTR record serialization
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
      pad := tmp.len and AesBlockMod;
      if pad <> 0 then // AES-CTR is easier if padded on 16 bytes
        inc(tmp.len, 16 - pad);
      if tmp.len > SizeOf(cc.data) then
        // all cookies storage should be < 4K so a single 2K cookie seems huge
        ECrypt.RaiseU('TBinaryCookieGenerator: Too Big Too Fat');
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
         ccend, rkRecordTypes)) > AesBlockMod then
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


{ ************* Rnd/Hash/Sign/Cipher/Asym/Cert/Store High-Level Algorithms Factories }

var
  GlobalCryptAlgo: TRawUtf8List; // Objects[] are TCryptAlgo instances

procedure GlobalCryptAlgoInit; forward;


{ TCryptAlgo }

constructor TCryptAlgo.Create(const name: RawUtf8);
begin
  if name = '' then
    ECrypt.RaiseUtf8('Unexpected %.Create('''')', [self]);
  fName := LowerCase(name);
  RegisterGlobalShutdownRelease(self);
  GlobalCryptAlgo.AddOrReplaceObject(fName, self);
end;

class function TCryptAlgo.InternalFind(
  const name: RawUtf8; var Last: TCryptAlgo): pointer;
begin
  if name = '' then
  begin
    result := nil;
    exit;
  end;
  if GlobalCryptAlgo = nil then
    GlobalCryptAlgoInit;
  result := Last; // simple but efficient cache
  if (result <> nil) and
     PropNameEquals(TCryptAlgo(result).fName, name) then
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
    ECrypt.RaiseUtf8('%.Create(''%''): unknown algorithm in %', [self, name, CSV]);
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
    ECrypt.RaiseUtf8('Unexpected %.Create(nil)', [self]);
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
    ECrypt.RaiseUtf8('Unexpected %.Create(''%'')', [self, name]);
  Create(algo);
end;


{ TCryptRandom }

function TCryptRandom.Get(len: PtrInt): RawByteString;
begin
  Get(FastNewRawByteString(result, len), len);
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

{ TCryptRandomAesPrng }

type
  TCryptRandomAesPrng = class(TCryptRandom) // 'rnd-default,rnd-aes'
  public
    procedure Get(dst: pointer; dstlen: PtrInt); override;
    function Get32: cardinal; override;
  end;

procedure TCryptRandomAesPrng.Get(dst: pointer; dstlen: PtrInt);
begin
  TAesPrng.Main.FillRandom(dst, dstlen);
end;

function TCryptRandomAesPrng.Get32: cardinal;
begin
  result := TAesPrng.Main.Random32; // a CSPRNG is pointless for 32-bit anyway
end;

{ TCryptRandomEntropy }

const
  // CSV text of TAesPrngGetEntropySource items as used for Rnd() factory naming
  RndAlgosText: PUtf8Char =
    'rnd-entropy,rnd-entropysys,rnd-entropysysblocking,rnd-entropyuser';

type
  TCryptRandomEntropy = class(TCryptRandom)
  protected
    fSource: TAesPrngGetEntropySource;
  public
    constructor Create(const name: RawUtf8); override;
    function Get(len: PtrInt): RawByteString; override;
    procedure Get(dst: pointer; dstlen: PtrInt); override;
  end;

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
  tmp := TAesPrng.GetEntropy(dstlen, fSource); // may be slow for a few bytes
  MoveFast(pointer(tmp)^, dst^, dstlen);
  FillZero(tmp);
end;

{ TCryptRandomSysPrng }

type
  TCryptRandomSysPrng = class(TCryptRandom) // 'rnd-system,rnd-systemblocking'
  public
    procedure Get(dst: pointer; dstlen: PtrInt); override;
  end;

procedure TCryptRandomSysPrng.Get(dst: pointer; dstlen: PtrInt);
begin
  FillSystemRandom(dst, dstlen, {allowblocking=}length(fName) > 10);
end;

{ TCryptRandomLecuyerPrng }

type
  TCryptRandomLecuyerPrng = class(TCryptRandom) // 'rnd-lecuyer'
  public
    procedure Get(dst: pointer; dstlen: PtrInt); override;
    function Get32: cardinal; override;
  end;

procedure TCryptRandomLecuyerPrng.Get(dst: pointer; dstlen: PtrInt);
begin
  SharedRandom.Fill(dst, dstlen); // global TLecuyer gsl_rng_taus2 generator
end;

function TCryptRandomLecuyerPrng.Get32: cardinal;
begin
  result := SharedRandom.Next;
end;

{ TCryptRandom32 }

procedure TCryptRandom32.Get(dst: pointer; dstlen: PtrInt);
var
  c: cardinal;
begin
  repeat
    if dstlen < 4 then
      break;
    PCardinal(dst)^ := PCardinal(dst)^ xor Get32; // e.g. Get32 = RdRand32
    inc(PCardinal(dst));
    dec(dstlen, 4);
  until false;
  if dstlen <= 0 then
    exit;
  c := Get32;
  repeat
    PByte(dst)^ := PByte(dst)^ xor c;
    inc(PByte(dst));
    c := c shr 8;
    dec(dstlen);
  until dstlen = 0;
end;

{ TCryptRandomDelphi }

type
  TCryptRandomDelphi = class(TCryptRandom32) // 'rnd-delphi'
  protected
    fSafe: TLightLock;
    fSeed: cardinal;
  public
    function Get32: cardinal; override;
  end;

function TCryptRandomDelphi.Get32: cardinal;
begin
  fSafe.Lock; // we make this generator thread-safe - whereas Delphi's is not
  if fSeed = 0 then
    fSeed := Random32; // good enough - better than Delphi/FPC Randomize anyway
  // the Delphi RTL uses such a weak deterministic linear congruential generator
  result := fSeed * 134775813 + 1;
  fSeed := result;
  fSafe.UnLock;
end;

// note: the FPC RTL has a better Mersenne Twister algorithm, but its 32-bit
// core is not published outside of the system unit, it consumes 2KB from a weak
// 32-bit seed from GetTickCount/fptime, and is not thread-safe either

{$ifdef CPUINTEL}

{ TCryptRandomRdRand }

type
  TCryptRandomRdRand = class(TCryptRandom32) // 'rnd-rdrand'
  public
    function Get32: cardinal; override;
  end;

function TCryptRandomRdRand.Get32: cardinal;
begin
  result := RdRand32; // class is only registered if cfRAND in CpuFeatures
end;

{$endif CPUINTEL}

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

function TCryptHash.UpdateStream(stream: TStream): Int64;
var
  temp: array[word] of word; // 128KB temporary buffer
  read: integer;
begin
  result := 0;
  if stream <> nil then
    // we don't use stream.Size since some TStream classes don't support it
    repeat
      read := stream.Read(temp, SizeOf(temp)); // read until the end
      if read <= 0 then
        break;
      Update(@temp, read);
      inc(result, read);
    until false;
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

function TCryptHasher.HashAlgo(out hasher: THashAlgo): boolean;
begin
  result := false; // unspecified
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
  // CSV text of TCrc32Algo items as used for Hasher/Hash factories naming
  CrcAlgosText: PUtf8Char =
    'crc32,crc32c,xxhash32,adler32,fnv32,default32,md5-32,sha1-32,sha256-32';

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
    ECrypt.RaiseUtf8('%.New: unavailable ''%'' function', [self, algo.fName]);
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
    function HashAlgo(out hasher: THashAlgo): boolean; override;
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
  // CSV text of THashAlgo items, as recognized by Hasher/Hash factories naming
  HashAlgosText: PUtf8Char =
    'md5,sha1,sha256,sha384,sha512,sha3_256,sha3_512,sha224';

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

function TCryptHasherInternal.HashAlgo(out hasher: THashAlgo): boolean;
begin
  hasher := fAlgo;
  result := true;
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
  // CSV text of TSignAlgo items, as used for Signer/Sign factories naming
  SignAlgosText: PUtf8Char = 'hmac-sha1,hmac-sha256,hmac-sha384,hmac-sha512,' +
    'sha3-224,sha3-256,sha3-384,sha3-512,sha3-s128,sha3-s256';

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
  s.Pbkdf2(fAlgo, secret, salt, rounds, @key);
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
  fAlgo.Final(@dig, {noinit=}false);
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
    ECrypt.RaiseUtf8('%.New: unknown ''%'' hash', [self, hash]);
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
    include(fFlags, fAesGcm);
  if algo.fMode in AES_AEAD then
    include(fFlags, fAesAead); // mCfc,mOfc,mCtc,mGcm
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

function TCryptAesCipher.Process(const src: RawByteString;
  out dst: RawByteString; const aeadinfo: RawByteString): boolean;
begin
  result := false;
  if src = '' then
    exit;
  if fAesAead in fFlags then
    // mCfc/mOfc/mCtc/mGcm AEAD algorithms using 128-bit GMAC or 256-bit crc32c
    dst := fAes.MacAndCrypt(src, fEncrypt in fFlags, fIVAtBeg in fFlags, aeadinfo)
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
  else if fAesAead in fFlags then
    ECrypt.RaiseUtf8('%.Process(TBytes) is unsupported for %',
      [self, fCryptAlgo.AlgoName]) // MacAndCrypt() requires RawByteString
  // standard encryption with no AEAD/checksum
  else if fEncrypt in fFlags then
    dst := fAes.EncryptPkcs7(src, fIVAtBeg in fFlags)
  else
    dst := fAes.DecryptPkcs7(src, fIVAtBeg in fFlags);
  result := {%H-}dst <> nil;
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
  pempub, pempriv: TPemKind;
begin // inherited classes should override at least one of those Generate*()
  GenerateDer(derpub, derpriv, privpwd);
  pempub := TPemKind(fPemPublic);
  if pempub = pemUnspecified then
    pempub := pemPublicKey;
  pempriv := TPemKind(fPemPrivate);
  if pempriv = pemUnspecified then
    pempriv := pemPrivateKey;
  pub := DerToPem(pointer(derpub), length(derpub), pempub);
  priv := DerToPem(pointer(derpriv), length(derpriv), pempriv);
  FillZero(derpriv); // anti-forensic
end;

procedure TCryptAsym.GenerateDer(out pub, priv: RawByteString; const privpwd: RawUtf8);
var
  pempub, pempriv: RawUtf8;
begin // inherited classes should override at least one of those Generate*()
  GeneratePem(pempub, pempriv, privpwd);
  pub := PemToDer(pempub);
  priv := PemToDer(pempriv);
  FillZero(pempriv); // anti-forensic
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


const
  /// per algorithm PrivateKeyEncrypt/PrivateKeyDecrypt salt
  // - ckaRsa/ckaRsaPss share the same public/private key files by definition
  // - ckaEcc256 matches EccPrivateKeyEncrypt/EccPrivateKeyDecrypt encoding
  CKA_SALT: array[TCryptKeyAlgo] of RawUtf8 = (
    '',           // ckaNone
    'synrsa',     // ckaRsa
    'synrsa',     // ckaRsaPss
    'synecc',     // ckaEcc256
    'syne384',    // ckaEcc384
    'syne512',    // ckaEcc512
    'synecck',    // ckaEcc256k
    'syneddsa');  // ckaEdDSA

  /// per algorithm PrivateKeyEncrypt/PrivateKeyDecrypt AF-32 rounds
  // - ckaRsa/ckaRsaPss share the same public/private key files by definition
  // - ckaEcc256 matches EccPrivateKeyEncrypt/EccPrivateKeyDecrypt encoding
  CKA_ROUNDS: array[TCryptKeyAlgo] of byte = (
    0,    // ckaNone
    3,    // ckaRsa
    3,    // ckaRsaPss
    31,    // ckaEcc256
    23,    // ckaEcc384
    15,    // ckaEcc512
    31,    // ckaEcc256k
    31);   // ckaEdDSA

{ TCryptAbstractKey }

function TCryptAbstractKey.KeyAlgo: TCryptKeyAlgo;
begin
  result := fKeyAlgo;
end;

function TCryptAbstractKey.Instance: TCryptAbstractKey;
begin
  result := self;
end;


{ TCryptPublicKey }

function TCryptPublicKey.VerifyDigest(Sig: pointer; Dig: THash512Rec;
  SigLen, DigLen: integer; Hash: THashAlgo): boolean;
begin
  result := false; // to be overriden if needed (not for OpenSSL)
end;

function TCryptPublicKey.Verify(Algorithm: TCryptAsymAlgo; Data, Sig: pointer;
  DataLen, SigLen: integer): boolean;
var
  hasher: TSynHasher;
  dig: THash512Rec;
  diglen: PtrInt;
begin
  diglen := hasher.Full(CAA_HF[Algorithm], Data, DataLen, dig);
  result := (diglen <> 0) and
            VerifyDigest(Sig, dig, SigLen, diglen, CAA_HF[Algorithm]);
end;

function TCryptPublicKey.Verify(Algorithm: TCryptAsymAlgo;
  const Data, Sig: RawByteString): boolean;
begin
  result := Verify(Algorithm,
    pointer(Data), pointer(Sig), length(Data), length(Sig));
end;


{ TCryptPrivateKey }

function TCryptPrivateKey.Save(AsPem: boolean;
  const Password: SpiUtf8): RawByteString;
var
  der, bin: RawByteString;
  k: TPemKind;
begin
  // use our proprietary mormot.core.secure encryption, not standard PKCS#8
  // - overriden in mormot.crypt.openssl to use PEVP_PKEY standard serialization
  if self = nil then
    result := ''
  else
  try
    // call overriden TCryptPrivateKeyEcc.ToDer and TCryptPrivateKeyRsa.ToDer
    der := ToDer;
    // persist in the expected (may be encrypted) format
    if Password = '' then
      // save as plain unencrypted PEM/DER
      if AsPem then
        if fKeyAlgo in CKA_RSA then
          k := pemRsaPrivateKey
        else
          k := pemEcPrivateKey
      else
        k := pemUnspecified // save as ccfBinary
    else
    begin
      bin := der; // for FillZero()
      der := PrivateKeyEncrypt(
               bin, CKA_SALT[fKeyAlgo], Password, CKA_ROUNDS[fKeyAlgo]);
      if AsPem then
        if fKeyAlgo in CKA_RSA then
          k := pemSynopseRsaEncryptedPrivateKey
        else
          k := pemSynopseEccEncryptedPrivateKey
        else
          k := pemUnspecified;
    end;
    if k = pemUnspecified then
      result := der
    else
      result := DerToPem(der, k);
  finally
    FillZero(der);
    FillZero(bin);
  end;
end;

function TCryptPrivateKey.FromDer(algo: TCryptKeyAlgo; const der: RawByteString;
  pub: TCryptPublicKey): boolean;
begin
  result := false; // to be overriden if needed (not for OpenSSL)
end;

function TCryptPrivateKey.SignDigest(const Dig: THash512Rec; DigLen: integer;
  DigAlgo: TCryptAsymAlgo): RawByteString;
begin
  result := ''; // to be overriden if needed (not for OpenSSL)
end;

function TCryptPrivateKey.Load(Algorithm: TCryptKeyAlgo;
  const AssociatedKey: ICryptPublicKey; const PrivateKeySaved: RawByteString;
  const Password: SpiUtf8): boolean;
var
  saved, der: RawByteString;
  pub: TCryptPublicKey;
begin
  // use our proprietary mormot.core.secure encryption, not standard PKCS#8
  // - overriden in mormot.crypt.openssl to use PEVP_PKEY standard serialization
  result := false;
  if (self = nil) or
     (fKeyAlgo <> ckaNone) or
     (Algorithm = ckaNone) or
     (PrivateKeySaved = '') then
    exit;
  try
    // compute the raw DER content (may be decrypt)
    saved := PrivateKeySaved;
    if Password <> '' then
    begin
      der := PemToDer(saved); // see also TCryptCertX509.Load
      saved := PrivateKeyDecrypt(
        der, CKA_SALT[Algorithm], Password, CKA_ROUNDS[Algorithm]);
      if saved = '' then
        exit;
    end;
    if Assigned(AssociatedKey) then
      pub := AssociatedKey.Instance as TCryptPublicKey
    else
      pub := nil;
    // call overriden TCryptPrivateKeyEcc.FromDer and TCryptPrivateKeyRsa.FromDer
    if not FromDer(Algorithm, saved, pub) then
      exit;
    fKeyAlgo := Algorithm;
    result := true;
  finally
    FillZero(saved);
    FillZero(der);
  end;
end;

function TCryptPrivateKey.Sign(Algorithm: TCryptAsymAlgo;
  Data: pointer; DataLen: integer): RawByteString;
var
  hasher: TSynHasher;
  dig: THash512Rec;
  diglen: PtrInt;
begin
  diglen := hasher.Full(CAA_HF[Algorithm], Data, DataLen, dig);
  result := SignDigest(dig, diglen, Algorithm);
end;

function TCryptPrivateKey.Sign(Algorithm: TCryptAsymAlgo;
  const Data: RawByteString): RawByteString;
begin
  result := Sign(Algorithm, pointer(Data), length(Data));
end;

function TCryptPrivateKey.SharedSecret(
  const PeerKey: ICryptPublicKey): RawByteString;
begin
  result := ''; // unsupported by this algorithm (only ECC by now)
end;



{ TCryptCertAlgo }

function TCryptCertAlgo.Load(const Saved: RawByteString;
  Content: TCryptCertContent; const PrivatePassword: SpiUtf8): ICryptCert;
begin
  result := New;
  if not result.Load(Saved, Content, PrivatePassword) then
    result := nil;
end;

function TCryptCertAlgo.Generate(Usages: TCryptCertUsages;
  const Subjects: RawUtf8; const Authority: ICryptCert; ExpireDays: integer;
  ValidDays: integer; Fields: PCryptCertFields): ICryptCert;
begin
  result := New.
    Generate(Usages, Subjects, Authority, ExpireDays, ValidDays, Fields);
end;

function TCryptCertAlgo.CreateSelfSignedCsr(const Subjects: RawUtf8;
  const PrivateKeyPassword: SpiUtf8; var PrivateKeyPem: RawUtf8;
  Usages: TCryptCertUsages; Fields: PCryptCertFields): RawUtf8;
var
  csr: ICryptCert;
begin
  if PrivateKeyPem <> '' then
    ECryptCert.RaiseUtf8('%.CreateSelfSignedCsr % does not support ' +
      'a custom private key', [self, AlgoName]);
  // by default, just generate a self-signed certificate as CSR
  csr := New;
  csr.Generate(Usages, Subjects, nil, 365, -1, Fields);
  PrivateKeyPem := csr.Save(cccPrivateKeyOnly, PrivateKeyPassword);
  result := csr.Save(cccCertOnly, '', ccfPem);
  // fields are ignored with our syn-ecc encoding anyway
end;

function TCryptCertAlgo.GenerateFromCsr(const Csr: RawByteString;
  const Authority: ICryptCert; ExpireDays, ValidDays: integer): ICryptCert;
begin
  result := New.GenerateFromCsr(Csr, Authority, ExpireDays, ValidDays);
end;

function TCryptCertAlgo.JwtName: RawUtf8;
begin
  result := CAA_JWT[fCaa];
end;


{ TCryptCert }

procedure TCryptCert.RaiseError(const Msg: ShortString);
begin
  raise ECryptCert.CreateUtf8('%.%', [self, Msg]);
end;

procedure TCryptCert.RaiseError(const Fmt: RawUtf8;
  const Args: array of const);
var
  msg: ShortString;
begin
  FormatShort(Fmt, Args, msg);
  RaiseError(msg);
end;

procedure TCryptCert.RaiseErrorGenerate(const api: ShortString);
begin
  RaiseError('Generate: % error', [api]); // raise ECryptCert
end;

procedure TCryptCert.EnsureCanWrite(const Context: ShortString);
begin
  if (fIndexer <> nil) and
     not IsVoid then
    RaiseError('% not allowed: currently indexed by a %', [Context, fIndexer]);
end;

class procedure TCryptCert.InternalFind(Cert: PICryptCert;
  const Value: RawByteString; Method: TCryptCertComparer;
  Count, MaxCount: integer; out Chain: ICryptCerts);
var
  found: boolean;
  res: integer;
begin
  // O(n) efficient search loop with some temporary memory allocation
  res := 0;
  while Count <> 0 do
  begin
    case Method of
      ccmSerialNumber:
        found := HumanHexCompare(Cert^.GetSerial, Value) = 0;
      ccmSubjectName:
        found := EqualBuf(Cert^.GetSubjectName, Value);
      ccmIssuerName:
        found := EqualBuf(Cert^.GetIssuerName, Value);
      ccmSubjectCN:
        found := IdemPropNameU(Cert^.GetSubject('CN'), Value);
      ccmIssuerCN:
        found := IdemPropNameU(Cert^.GetIssuer('CN'), Value);
      ccmSubjectKey:
        found := HumanHexCompare(Cert^.GetSubjectKey, Value) = 0;
      ccmAuthorityKey:
        found := CsvContains(Cert^.GetAuthorityKey, Value);
      ccmSubjectAltName:
        found := FindRawUtf8(Cert^.GetSubjects, Value, {casesens=}false) >= 0;
      ccmIssuerAltName:
        found := FindRawUtf8(Cert^.GetIssuers, Value, {casesens=}false) >= 0;
      ccmBinary:
        found := EqualBuf(Cert^.Save, Value);
      ccmSha1:
        found := IdemPropNameU(Cert^.GetDigest(hfSha1), Value);
      ccmSha256:
        found := IdemPropNameU(Cert^.GetDigest(hfSha256), Value);
    else
      found := false; // unsupported search method (e.g. ccmUsage)
    end;
    if found then
    begin
      InterfaceArrayAddCount(Chain, res, Cert^);
      dec(MaxCount);
      if MaxCount = 0 then
        break;
    end;
    inc(Cert);
    dec(Count);
  end;
  if res <> length({%H-}Chain) then
    DynArrayFakeLength(Chain, res);
end;

function TCryptCert.GenerateFromCsr(const Csr: RawByteString;
  const Authority: ICryptCert; ExpireDays, ValidDays: integer): ICryptCert;
var
  x: ICryptCert;
begin
  EnsureCanWrite('GenerateFromCsr');
  // by default, CreateSelfSignedCsr generates a self-signed certificate as CSR
  // - mormot.crypt.openssl and mormot.crypt.x509 will generate a proper CSR
  result := nil;
  if (Csr = '') or
     (fCryptAlgo = nil) then
    exit;
  x := CertAlgo.Load(Csr);
  if (x <> nil) and
     (x.Verify(x) = cvValidSelfSigned) then
    result := Generate(x.GetUsage, RawUtf8ArrayToCsv(x.GetSubjects),
      Authority, ExpireDays, ValidDays, {Fields=}nil);
    // note: Fields=nil since TCryptCertInternal does not support them
end;

function TCryptCert.IsAuthorizedBy(const Authority: ICryptCert): boolean;
begin
  result := (Authority <> nil) and
            IdemPropNameU(GetAuthorityKey, Authority.GetSubjectKey);
end;

function TCryptCert.Compare(const Another: ICryptCert;
  Method: TCryptCertComparer): integer;
begin
  if Assigned(Another) and
     (Another.Handle <> nil) then
    case Method of
      ccmSerialNumber:
        result := HumanHexCompare(GetSerial, Another.GetSerial);
      ccmSubjectName:
        result := CompareBuf(GetSubjectName, Another.GetSubjectName);
      ccmIssuerName:
        result := CompareBuf(GetIssuerName, Another.GetIssuerName);
      ccmSubjectCN:
        result := CompareBuf(GetSubject('CN'), Another.GetSubject('CN'));
      ccmIssuerCN:
        result := CompareBuf(GetIssuer('CN'), Another.GetIssuer('CN'));
      ccmSubjectKey:
        result := HumanHexCompare(GetSubjectKey, Another.GetSubjectKey);
      ccmAuthorityKey:
        result := HumanHexCompare(GetAuthorityKey, Another.GetAuthorityKey);
      ccmSubjectAltName:
        result := CompareBuf(RawUtf8ArrayToCsv(GetSubjects),
                    RawUtf8ArrayToCsv(Another.GetSubjects));
      ccmIssuerAltName:
        result := CompareBuf(RawUtf8ArrayToCsv(GetIssuers),
                    RawUtf8ArrayToCsv(Another.GetIssuers));
      ccmUsage:
        result := word(GetUsage) - word(Another.GetUsage);
      ccmBinary:
        result := CompareBuf(Save(cccCertOnly, '', ccfBinary),
                             Another.Save(cccCertOnly, '', ccfBinary));
      ccmSha1:
        result := CompareBuf(GetDigest(hfSHA1), Another.GetDigest(hfSHA1));
      ccmSha256:
        result := CompareBuf(GetDigest(hfSHA256), Another.GetDigest(hfSHA256));
    else // e.g. ccmInstance
      result := ComparePointer(pointer(self), pointer(Another));
    end
  else
    result := 1;
end;

function TCryptCert.IsEqual(const Another: ICryptCert): boolean;
begin
  result := Compare(Another, ccmBinary) = 0;
end;

function TCryptCert.IsValidDate(date: TDateTime): boolean;
var
  na, nb: TDateTime;
begin
  if date = 0 then
    date := NowUtc;
  na := GetNotAfter;
  nb := GetNotBefore;
  result := (not IsVoid) and
            ((na <= 0) or (na + CERT_DEPRECATION_THRESHOLD > date)) and
            ((nb <= 0) or (nb < date + CERT_DEPRECATION_THRESHOLD));
end;

function TCryptCert.IsVoid: boolean;
begin
  result := Handle = nil;
end;

function TCryptCert.GetDigest(Algo: THashAlgo): RawUtf8;
begin
  result := HashFull(Algo, Save(cccCertOnly, '', ccfBinary));
end;

function TCryptCert.LoadFromFile(const Source: TFileName;
  Content: TCryptCertContent; const PrivatePassword: SpiUtf8): boolean;
var
  s: RawByteString;
begin
  EnsureCanWrite('LoadFromFile');
  fLastLoadFromFileName := Source;
  s := StringFromFile(Source);
  result := Load(s, Content, PrivatePassword);
  FillZero(s); // may be a private key with no password :(
end;

function TCryptCert.GetFileName: TFileName;
begin
  result := fLastLoadFromFileName;
end;

function TCryptCert.Save(Content: TCryptCertContent;
  const PrivatePassword: SpiUtf8; Format: TCryptCertFormat): RawByteString;
begin
  // overriden call only for ccfHexa, ccfBase64 and ccfBase64Uri encoding
  result := Save(Content, PrivatePassword, ccfBinary);
  case Format of
    ccfHexa:
      result := BinToHex(result);
    ccfBase64:
      result := BinToBase64(result);
    ccfBase64Uri:
      result := BinToBase64uri(result);
  else
    ECryptCert.RaiseUtf8('Unexpected %.Save', [self]); // paranoid
  end;
end;

procedure TCryptCert.SaveToFile(const Dest: TFileName; Content: TCryptCertContent;
  const PrivatePassword: SpiUtf8; Format: TCryptCertFormat);
var
  s: RawByteString;
  fn: TFileName;
begin
  fn := Dest;
  if fn = '' then
    fn := fLastLoadFromFileName;
  s := Save(Content, PrivatePassword, Format);
  FileFromString(s, fn);
  FillZero(s); // may be a private key with no password :(
end;

function TCryptCert.Sign(const Data: RawByteString;
  Usage: TCryptCertUsage): RawByteString;
begin
  result := Sign(pointer(Data), length(Data), Usage);
end;

function TCryptCert.Verify(const Signature, Data: RawByteString;
  IgnoreError: TCryptCertValidities; TimeUtc: TDateTime): TCryptCertValidity;
begin
  result := Verify(pointer(Signature), pointer(Data),
                   length(Signature), length(Data), IgnoreError, TimeUtc);
end;

function TCryptCert.JwtCompute(const DataNameValue: array of const;
  const Issuer, Subject, Audience: RawUtf8; NotBefore: TDateTime;
  ExpirationMinutes: integer; Signature: PRawByteString): RawUtf8;
var
  payload: TDocVariantData;
  headpayload: RawUtf8;
  sig: RawByteString;
begin
  result := '';
  if not HasPrivateSecret then
    exit;
  // cut-down version of TJwtAbstract.PayloadToJson
  payload.InitObject(DataNameValue, JSON_FAST);
  if Issuer <> '' then
    payload.AddValueText('iss', Issuer);
  if Subject <> '' then
    payload.AddValueText('sub', Subject);
  if Audience <> '' then
    payload.AddValueText('aud', Audience);
  if NotBefore > 0 then
    payload.AddValue('nbf', DateTimeToUnixTime(NotBefore));
  if ExpirationMinutes > 0 then
    payload.AddValue('exp', UnixTimeUtc + ExpirationMinutes * 60);
  if payload.Count = 0 then
    exit; // we need something to sign
  // see TJwtAbstract.Compute
  headpayload := BinToBase64Uri(FormatUtf8('{"alg":"%"}', [CertAlgo.JwtName])) +
           '.' + BinToBase64Uri(payload.ToJson);
  sig := self.Sign(headpayload);
  if sig = '' then
    exit;
  if Signature <> nil then
    Signature^ := sig;
  result := headpayload + '.' + BinToBase64Uri(sig);
end;

function TCryptCert.JwtVerify(const Jwt: RawUtf8;
  Issuer, Subject, Audience: PRawUtf8;
  Payload: PDocVariantData; Signature: PRawByteString;
  IgnoreError: TCryptCertValidities; TimeUtc: TDateTime): TCryptCertValidity;
var
  P, S: PUtf8Char;
  pl: TDocVariantData;
  ms: Int64;
  head, payl, sig: RawUtf8;
begin
  // same logic than TJwtAbstract.Verify, but (slower and) with no cache
  // -> TJwtAbstract is to be preferred if the ICryptCert is reused
  result := cvWrongUsage;
  P := PosCharU(Jwt, '.');
  if P = nil then
    exit;
  S := PosChar(P + 1, '.');
  if S = nil then
    exit;
  head := Base64UriToBin(pointer(Jwt), P - pointer(Jwt));
  if JsonDecode(head, 'alg') <> CertAlgo.JwtName then
    exit;
  inc(P);
  payl := Base64UriToBin(pointer(P), S - P);
  inc(S);
  sig := Base64UriToBin(PAnsiChar(S), StrLen(S));
  if (payl = '') or
     (sig = '') then
    exit;
  if Signature <> nil then
    Signature^ := sig;
  if pl.InitJsonInPlace(pointer(payl), JSON_FAST) = nil then
    exit;
  result := cvInvalidDate;
  if pl.GetAsInt64('exp', ms) and
     (UnixTimeUtc > ms) then
    exit;
  if pl.GetAsInt64('nbf', ms) and
     (UnixTimeUtc + 15 < ms) then
    exit;
  result := Verify(pointer(sig), pointer(Jwt),
              length(sig), S - 1 - pointer(Jwt), IgnoreError, TimeUtc);
  if not (result in CV_VALIDSIGN) then
    exit;
  if Issuer <> nil then
    Issuer^ := pl.U['iss'];
  if Subject <> nil then
    Subject^ := pl.U['sub'];
  if Audience <> nil then
    Audience^ := pl.U['aud'];
  if Payload <> nil then
    Payload^ := pl;
end;

function TCryptCert.JwkCompute: RawUtf8;
var
  x, y: RawByteString;
  bx, by: RawUtf8;
  caa: TCryptAsymAlgo;
begin
  // retrieve raw public key parameters
  result := '';
  if not GetKeyParams(x, y) then
    exit;
  bx := BinToBase64uri(x);
  by := BinToBase64uri(y);
  // parameters are ordered lexicographically, as expected for thumbprints
  caa := AsymAlgo;
  if caa in CAA_ECC then
    // for ECC, GetKeyParams() returned the x,y coordinates
    FormatUtf8('{"crv":"%","kty":"EC","x":"%","y":"%"}',
                  [CAA_CRV[caa], bx, by], result)
  else
    // for RSA, x was set to the Exponent (e), and y to the Modulus (n)
    FormatUtf8('{"e":"%","kty":"RSA","n":"%"}', [bx, by], result);
end;

function TCryptCert.SharedSecret(const pub: ICryptCert): RawByteString;
begin
  result := ''; // unsupported by default
end;

function TCryptCert.AsymAlgo: TCryptAsymAlgo;
begin
  result := (fCryptAlgo as TCryptCertAlgo).AsymAlgo;
end;

function TCryptCert.CertAlgo: TCryptCertAlgo;
begin
  if fCryptAlgo = nil then
    result := nil
  else
    result := fCryptAlgo as TCryptCertAlgo;
end;

function TCryptCert.Instance: TCryptCert;
begin
  result := self;
end;

function TCryptCert.PrivateKeyHandle: pointer;
begin
  result := nil; // unsupported
end;

function TCryptCert.GetKeyParams(out x, y: RawByteString): boolean;
begin
  result := false; // unsupported
end;


{ TCryptStore }

destructor TCryptStore.Destroy;
begin
  inherited Destroy;
  fCache.Free;
end;

function TCryptStore.Load(const Saved: RawByteString): boolean;
begin
  Clear;
  result := AddFromBuffer(Saved) <> nil; // expect chain of PEM Cert + CRLs
end;

function TCryptStore.Cache: TCryptCertCache;
begin
  result := fCache;
end;

function TCryptStore.FindOne(const Value: RawByteString;
  Method: TCryptCertComparer): ICryptCert;
begin
  case Method of
    ccmSerialNumber:
      result := GetBySerial(Value);
    ccmSubjectKey:
      result := GetBySubjectKey(Value);
  else
    result := nil; // other methods are unsupported by default
  end;
end;

function TCryptStore.Add(const cert: array of ICryptCert): TRawUtf8DynArray;
var
  i: PtrInt;
begin
  result := nil;
  for i := 0 to high(cert) do
    if Add(cert[i]) then
      AddRawUtf8(result, cert[i].GetSerial);
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

type
  TDirectoryCryptStore = class(TDirectoryBrowser) // support MAX_PATH on Windows
  protected
    fStore: TCryptStore;
    fSerials: TRawUtf8DynArray;
    fCount: integer;
    function OnFile(const FileInfo: TSearchRec;
      const FullFileName: TFileName): boolean; override;
  end;

function TDirectoryCryptStore.OnFile(const FileInfo: TSearchRec;
  const FullFileName: TFileName): boolean;
begin
  if FileInfo.Size < 65535 then // certificate files are expected to be < 64KB
     AddRawUtf8(fSerials, fCount, fStore.AddFromFile(FullFileName));
  result := true; // continue;
end;

function TCryptStore.AddFromFolder(const Folder, Mask: TFileName;
  Recursive: boolean): TRawUtf8DynArray;
var
  browser: TDirectoryCryptStore;
begin
  browser := TDirectoryCryptStore.Create(Folder, [Mask], Recursive);
  try
    browser.fStore := self;
    browser.Run;
    if browser.fCount <> 0 then
      DynArrayFakeLength(browser.fSerials, browser.fCount);
    result := browser.fSerials;
  finally
    browser.Free;
  end;
end;

function TCryptStore.IsValidChain(const chain: ICryptCertChain;
  date: TDateTime): TCryptCertValidity;
var
  i, n: PtrInt;
  c: ICryptCertChain;
begin
  // we need something to validate
  result := cvBadParameter;
  if (chain = nil) or
     (chain[0] = nil) then
    exit;
  // ensure main certificate is not deprecated
  result := cvInvalidDate;
  if not chain[0].IsValidDate(date) then
    exit;
  // compute the exact authority sequence (if not supplied in proper order)
  result := cvUnknownAuthority;
  c := ChainConsolidate(chain);
  n := length(c);
  if (n = 0) or
     ((n = 1) and
      not c[0].IsSelfSigned) then
    exit;
  // check the usages of all intermediate certificates
  result := cvWrongUsage;
  for i := 1 to n - 1 do
    if c[i].GetUsage * [cuKeyCertSign, cuCA] = [] then
      exit;
  // ensure no certificate in the sequence has been explicitly revoked
  result := cvRevoked;
  for i := 0 to n - 1 do
    if IsRevoked(c[i]) <> crrNotRevoked then
      exit;
  // check the cascaded dates (before any digital signature verification)
  result := cvDeprecatedAuthority;
  for i := 1 to n - 1 do
    if not c[i].IsValidDate(c[i - 1].GetNotBefore) then
      exit;
  // check the cascaded digital signatures
  for i := 0 to n - 2 do
  begin
    result := c[i].Verify(c[i + 1], [cvWrongUsage, cvDeprecatedAuthority]);
    // note: TCryptCertX509.Verify has a per-authority cache so is very fast
    if result <> cvValidSigned then
      exit;
  end;
  // eventually check the trusted anchor of the chain
  if n > 1 then
    date := c[n - 2].GetNotBefore; // anchor is not main: adjust date
  result := IsValid(c[n - 1], date);
end;

function TCryptStore.Verify(const Signature, Data: RawByteString;
  IgnoreError: TCryptCertValidities; TimeUtc: TDateTime): TCryptCertValidity;
begin
  result := Verify(Signature, pointer(Data), length(Data), IgnoreError, TimeUtc);
end;

function TCryptStore.DefaultCertAlgo: TCryptCertAlgo;
begin
  result := (fCryptAlgo as TCryptStoreAlgo).DefaultCertAlgo;
end;


{ TCryptStoreAlgo }

function TCryptStoreAlgo.NewFrom(const Binary: RawByteString): ICryptStore;
begin
  result := New;
  if not result.Load(Binary) then
    result := nil;
end;


{ TCryptCertAbstractList }

procedure TCryptCertAbstractList.SetCryptCertClass(c: TCryptCertClass);
begin
  fCryptCertClass := c;
end;

function TCryptCertAbstractList.GetCount: integer;
begin
  result := fList.Count;
end;

destructor TCryptCertAbstractList.Destroy;
begin
  fList.Free;
  inherited Destroy;
end;

function TCryptCertAbstractList.Find(const Value: RawByteString;
  Method: TCryptCertComparer; MaxCount: integer): ICryptCerts;
begin
  result := nil;
  if (self = nil) or
     (fList.Count = 0) or
     (Value = '') then
    exit;
  fList.Safe.ReadLock;
  try
    // non-blocking O(n) search - overriden in TCryptCertX509 for performance
    if fCryptCertClass <> nil then
      fCryptCertClass.InternalFind(
        fList.Values.Value^, Value, Method, fList.Count, MaxCount, result);
  finally
    fList.Safe.ReadUnLock;
  end;
end;

function TCryptCertAbstractList.FindOne(const Value: RawByteString;
  Method: TCryptCertComparer): ICryptCert;
var
  res: ICryptCerts;
begin
  res := Find(Value, Method, 1);
  if res = nil then
    result := nil
  else
    result := res[0];
end;

function TCryptCertAbstractList.List: ICryptCerts;
begin
  fList.CopyValues(result);
end;

procedure TCryptCertAbstractList.SaveToPem(W: TTextWriter;
  WithExplanatoryText: boolean);
var
  i: PtrInt;
  c: ^ICryptCert;
begin
  fList.Safe.ReadLock;
  try
    c := fList.Values.Value^;
    for i := 1 to fList.Count do
    begin
      if WithExplanatoryText then
        // see https://datatracker.ietf.org/doc/html/rfc7468#section-5.2
        W.Add('Subject: %'#13#10'Issuer: %'#13#10'Validity: from % to %'#13#10,
         [c^.GetSubjectName, c^.GetIssuerName, DateTimeToIso8601Short(
            c^.GetNotBefore), DateTimeToIso8601Short(c^.GetNotAfter)]);
      W.AddString(c^.Save(cccCertOnly, '', ccfPem));
      W.AddCR;
      inc(c);
    end;
  finally
    fList.Safe.ReadUnLock;
  end;
end;


{ TCryptCertCache }

function TCryptCertCache.OnDelete(const aKey, aValue; aIndex: integer): boolean;
begin
  // return true to delete the deprecated item - only if not currently in use
  result := ICryptCert(aValue).Instance.RefCount = 1;
end;

constructor TCryptCertCache.Create(TimeOutSeconds: integer);
begin
  fList := TSynDictionary.Create(TypeInfo(TRawByteStringDynArray),
    TypeInfo(ICryptCerts), {caseins=}false, TimeOutSeconds);
  fList.OnCanDeleteDeprecated := OnDelete;
  fList.ThreadUse := uRWLock; // non-blocking Load() and Find()
end;

function TCryptCertCache.Load(const Cert: RawByteString): ICryptCert;
var
  der: RawByteString;
  inst: TCryptCert;
begin
  result := nil;
  // normalize and validate input
  if AsnDecChunk(Cert) then
    der := Cert
  else
  begin
    der := PemToDer(Cert);
    if not AsnDecChunk(der) then
      exit;
  end;
  // try to retrieve and share an existing instance
  if fList.FindAndCopy(der, result) then
    exit;
  // we need to create a new TCryptCert instance
  result := InternalLoad(der);
  if result = nil then
    exit;
  // ensure it has a coherent implementation class
  inst := result.Instance;
  if pointer(fCryptCertClass) <> PPointer(inst)^ then
    if fCryptCertClass <> nil then
      exit // return the instance, but don't cache it
    else
      fCryptCertClass := PPointer(inst)^;
  inst.fIndexer := self; // don't touch once indexed
  // add this new instance to the internal cache
  if fList.Count > 128 then
    fList.DeleteDeprecated; // make some room (once a second and if RefCount=1)
  fList.Add(der, result);   // der key will be shared with TX509.fCachedDer
end;

function TCryptCertCache.Load(const Cert: array of RawByteString): ICryptCerts;
var
  i, n: PtrInt;
begin
  result := nil;
  SetLength(result, length(Cert));
  n := 0;
  for i := 0 to high(Cert) do
  begin
    result[n] := Load(Cert[i]);
    if Assigned(result[n]) then
      inc(n);
  end;
  SetLength(result, n);
end;

function TCryptCertCache.LoadPem(const Pem: RawUtf8): ICryptCerts;
var
  p: PUtf8Char;
  k: TPemKind;
  der: TCertDer;
  c: ICryptCert;
begin
  result := nil;
  p := pointer(Pem);
  if p <> nil then
    repeat
      der := NextPemToDer(p, @k);
      if der = '' then
        break;
      if not (k in [pemUnspecified, pemCertificate]) then
        continue; // no need to try loading something which is not a X.509 cert
      c := Load(der);
      if c <> nil then
        ChainAdd(result, c);
    until false;
end;

function TCryptCertCache.Find(const Value: RawByteString;
  Method: TCryptCertComparer; MaxCount: integer): ICryptCerts;
begin
  if Method = ccmBinary then
  begin
    SetLength(result, 1);
    if not fList.FindAndCopy(Value, result) then
      result := nil;
  end
  else
    result := inherited Find(Value, Method, MaxCount);
end;

function TCryptCertCache.NewList: TCryptCertList;
begin
  result := TCryptCertList.Create;
  result.fCryptCertClass := fCryptCertClass; // propagate the class
end;

function TCryptCertCache.NewList(const Pem: RawUtf8): TCryptCertList;
begin
  result := NewList;
  result.Add(LoadPem(Pem));
end;


{ TCryptCertList }

constructor TCryptCertList.Create;
begin
  inherited Create;
  fList := TSynDictionary.Create(
    TypeInfo(TRawByteStringDynArray), TypeInfo(ICryptCerts));
  fList.ThreadUse := uRWLock; // non-blocking Find()
end;

function TCryptCertList.Add(const Cert: ICryptCert): boolean;
var
  bin: RawByteString;
  inst: TCryptCert;
begin
  result := false;
  if not Assigned(Cert) then
    exit;
  inst := Cert.Instance;
  if pointer(fCryptCertClass) <> PPointer(inst)^ then
    if fCryptCertClass = nil then
      fCryptCertClass := PPointer(inst)^
    else
      ECryptCert.RaiseUtf8('%.Add(%) but we already store %',
        [self, inst, fCryptCertClass]);
  inst.fIndexer := self; // don't touch once indexed
  result := HumanHexToBin(inst.GetSubjectKey, bin) and
            (fList.Add(bin, Cert) >= 0);
end;

procedure TCryptCertList.Add(const Cert: array of ICryptCert);
var
  i: PtrInt;
begin
  for i := 0 to high(Cert) do
    Add(Cert[i]);
end;

function TCryptCertList.FindBySubjectKey(const Key: RawUtf8): ICryptCert;
var
  bin: RawByteString;
begin
  result := nil;
  if HumanHexToBin(Key, bin) then
    fList.FindAndCopy(bin, result);
end;

function TCryptCertList.FindBySubjectKeyRaw(const Key: RawByteString): ICryptCert;
begin
  fList.FindAndCopy(Key, result);
end;

function TCryptCertList.Find(const Value: RawByteString;
  Method: TCryptCertComparer; MaxCount: integer): ICryptCerts;
begin
  if Method = ccmSubjectKey then
  begin
    SetLength(result, 1);
    result[0] := FindBySubjectKey(Value); // use the TSynDictionary hash table
    if result[0] = nil then
      result := nil;
  end
  else
    result := inherited Find(Value, Method, MaxCount);
end;

function TCryptCertList.DeleteBySubjectKey(const Key: RawUtf8): boolean;
var
  bin: RawByteString;
begin
  result := HumanHexToBin(Key, bin) and
            (fList.Delete(Key) >= 0);
end;


procedure ChainAdd(var chain: ICryptCertChain; const cert: ICryptCert);
begin
  InterfaceArrayAdd(chain, cert);
end;

function ChainFind(var chain: ICryptCertChain; const cert: ICryptCert;
  comparer: TCryptCertComparer): PtrInt;
begin
  if comparer = ccmInstance then
    result := InterfaceArrayFind(chain, cert)
  else
  begin
    for result := 0 to length(chain) - 1 do
      if cert.Compare(chain[result], comparer) = 0 then
        exit;
    result := -1;
  end;
end;

function ChainConsolidate(const chain: ICryptCertChain): ICryptCertChain;
var
  ref: array[0..127] of pointer; // weak references of ICryptCert
  count, n: PtrInt;

  procedure RecursiveCompute(var one: pointer);
  var
    i: PtrInt;
    r: PPointer;
  begin
    result[n] := ICryptCert(one);
    inc(n);
    if n = count then
      exit; // paranoid
    r := @ref[1];
    for i := 1 to count - 1 do
      if (r^ <> nil) and
         ICryptCert(one).IsAuthorizedBy(ICryptCert(r^)) then
      begin
        one := nil; // faster and avoid endless loop on circular references
        RecursiveCompute(r^);
        break;
      end
      else
        inc(r);
  end;

begin
  result := nil;
  count := length(chain);
  if (chain = nil) or
     (chain[0] = nil) or
     (count >= high(ref)) then // a typical chain has 2 or 3 certificates
    exit;
  n := 0;
  SetLength(result, count);
  MoveFast(pointer(chain)^, ref[0], count * SizeOf(ref[0]));
  RecursiveCompute(ref[0]); // fill result[0..n-1] in auth order
  DynArrayFakeLength(result, n);
end;


{ TCryptCertPerUsage }

procedure TCryptCertPerUsage.Clear;
begin
  List := nil;
  FillCharFast(self, SizeOf(self), 0);
end;

function TCryptCertPerUsage.IsVoid: boolean;
begin
  result := List = nil;
end;

function TCryptCertPerUsage.Add(const cert: ICryptCert): TCryptCertUsages;
var
  u: TCryptCertUsage;
  n: PtrInt;
begin
  result := [];
  if cert = nil then
    exit;
  result := cert.GetUsage;
  if result = [] then
    exit;
  n := length(List);
  if n >= 255 then
    ECryptCert.RaiseU('TCryptCertPerUsage.Add overflow'); // paranoid
  SetLength(List, n + 1);
  List[n] := cert;
  inc(n); // CertPerUsage[u] stores index + 1, i.e. in 1..255 range
  for u := low(u) to high(u) do
    if u in result then
    begin
      include(Usages, u);
      if Index[u] = 0 then
        exclude(result, u);
      Index[u] := n; // replace any existing certificate
    end;
end;

function TCryptCertPerUsage.GetUsage(u: TCryptCertUsage;
  var cert: ICryptCert): boolean;
var
  i: PtrInt;
begin
  if List = nil then
    i := 0
  else
    i := Index[u]; // contains index + 1
  if i = 0 then
  begin
    cert := nil; // circumvent FPC inlining bug for "out cert"
    result := false;
  end
  else
  begin
    cert := List[i - 1];
    result := true;
  end;
end;

function TCryptCertPerUsage.PerUsage(u: TCryptCertUsage): ICryptCert;
begin
  GetUsage(u, result);
end;

function TCryptCertPerUsage.AsPem: RawUtf8;
var
  i: PtrInt;
begin
  result := '';
  for i := 0 to length(List) - 1 do
    result  := result + List[i].Save(cccCertOnly, '', ccfPem) + (CRLF + CRLF);
end;

function TCryptCertPerUsage.FromPem(
  algo: TCryptCertAlgo; const pem: RawUtf8): TCryptCertUsages;
var
  P: PUtf8Char;
  one: RawUtf8;
  c: ICryptCert;
begin
  Clear;
  result := [];
  if algo = nil then
    exit;
  P := pointer(pem);
  while P <> nil do
  begin
    one := NextPem(P);
    if one = '' then
      break;
    c := algo.Load(one);
    if c <> nil then
      result := result + Add(c);
  end;
end;

function TCryptCertPerUsage.AsBinary: RawByteString;
var
  i: PtrInt;
  tmp: TTextWriterStackBuffer; // 8KB work buffer on stack
  s: TBufferWriter;
begin
  s := TBufferWriter.Create(tmp{%H-});
  try
    for i := 0 to length(List) - 1 do
      s.Write(List[i].Save(cccCertOnly, '', ccfBinary));
    result := s.FlushTo;
  finally
    s.Free;
  end;
end;

function TCryptCertPerUsage.FromBinary(algo: TCryptCertAlgo;
  const bin: RawByteString): TCryptCertUsages;
var
  s: TFastReader;
  c: ICryptCert;
begin
  Clear;
  result := [];
  if (algo = nil) or
     (bin = '') then
    exit;
  s.Init(bin);
  while not s.EOF do
  begin
    c := algo.Load(s.VarString);
    if c <> nil then
      result := result + Add(c);
  end;
end;


function ToText(a: TCryptAsymAlgo): PShortString; overload;
begin
  result := GetEnumName(TypeInfo(TCryptAsymAlgo), ord(a));
end;

function ToText(a: TCryptKeyAlgo): PShortString; overload;
begin
  result := GetEnumName(TypeInfo(TCryptKeyAlgo), ord(a));
end;

function ToText(r: TCryptCertRevocationReason): PShortString;
begin
  result := GetEnumName(TypeInfo(TCryptCertRevocationReason), ord(r));
end;

function ToText(u: TCryptCertUsage): PShortString;
begin
  result := GetEnumName(TypeInfo(TCryptCertUsage), ord(u));
end;

function ToText(u: TCryptCertUsages; from_cu_text: boolean): ShortString;
var
  cu: TCryptCertUsage;
begin
  if from_cu_text then
  begin
    result := '';
    for cu := low(cu) to high(cu) do
      if cu in u then
        AppendShortTwoChars(@CU_TEXT[cu], @result);
  end
  else
    GetSetNameShort(TypeInfo(TCryptCertUsages), u, result, {trim=}true);
end;

function ToText(v: TCryptCertValidity): PShortString;
begin
  result := GetEnumName(TypeInfo(TCryptCertValidity), ord(v));
end;

function GetFirstUsage(u: TCryptCertUsages): TCryptCertUsage;
begin
  for result := low(result) to high(result) do
    if result in u then
      exit;
  result := cuKeyCertSign;
end;

function IsCN(const Rdn: RawUtf8): boolean;
begin
  result := (length(Rdn) = 2) and
            (PWord(Rdn)^ and $dfdf = ord('C') + ord('N') shl 8);
end;

function IsDer(const Rdn: RawUtf8): boolean;
begin
  result := (length(Rdn) = 3) and
            (PCardinal(Rdn)^ and $dfdfdf =
               ord('D') + ord('E') shl 8 + ord('R') shl 16);
end;


{ Register mormot.crypt.core and mormot.crypt.secure Algorithms }

procedure GlobalCryptAlgoInit;
var
  m: TAesMode;
  b, bits: integer;
  n: RawUtf8;
begin
  // don't call TAesPrng.Main to initialize MainAesPrng yet, because
  // OpenSslRandBytes() may not be already set and gathering OS entropy
  // may not be needed at all
  GlobalLock; // RegisterGlobalShutdownRelease() will use it anyway
  try
    if GlobalCryptAlgo <> nil then
      exit;
    GlobalCryptAlgo := RegisterGlobalShutdownRelease(
      TRawUtf8List.CreateEx([fNoDuplicate, fThreadSafe])); // no fObjectsOwned
    // register mormot.crypt.core engines into our factories
    TCryptRandomAesPrng.Implements('rnd-default,rnd-aes');
    TCryptRandomLecuyerPrng.Implements('rnd-lecuyer');
    TCryptRandomDelphi.Implements('rnd-delphi');
    {$ifdef CPUINTEL}
    if cfRAND in CpuFeatures then
      TCryptRandomRdRand.Implements('rnd-rdrand');
    {$endif CPUINTEL}
    TCryptRandomEntropy.Implements(RndAlgosText);
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
        if not (m in AES_INTERNAL) then // if exists as OpenSSL standard
          TCryptAesInternal.Create(n + '-int', m, bits, TAesInternal);
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
    result := '';
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
    if k = pemCertificate then
      if {%H-}Cert <> '' then
        exit // should contain a single Certificate
      else
        Cert := PemToDer(pem)
    else
      PrivKey := pem; // private key may be with several TPemKind markers
    FillZero(pem);
  until false;
  result := ({%H-}Cert <> '') and
            ({%H-}PrivKey <> '');
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
  pos: PtrUInt;
begin
  result := nil;
  FillZero(buf^, buflen);
  if (P = nil) or
     (P[0] <> AnsiChar(ASN1_INT)) then
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
  CAA_SIZE: array[TCryptAsymAlgo] of byte = (
    32,  // caaES256
    48,  // caaES384
    66,  // caaES512
    32,  // caaES256K
    0,   // caaRS256
    0,   // caaRS384
    0,   // caaRS512
    0,   // caaPS256
    0,   // caaPS384
    0,   // caaPS512
    32); // caaEdDSA

function GetSignatureSecurityBits(a: TCryptAsymAlgo; len: integer): integer;
begin
  result := 0;
  len := len shl 3; // into bits
  if len < 128 then
    exit;
  result := CAA_SIZE[a];
  if result <> 0 then
    // ECC security size is half of its X,Y coordinates storage size
    result := result shl 2
  else
    // RSA security depends on the signature size, not the hash size
    if len < 1024 then
      result := 30           // 512-bit
    else if len < 2048 then
      result := 80           // 1024-bit
    else if len < 3072 then
      result := 112          // 2048-bit
    else if len < 7680 then
      result := 128          // 3072-bit
    else if len < 15360 then
      result := 192          // 7680-bit: very unlikely since very slow
    else
      result := 256; // the lower RS256 hash has 256-bit of security anyway
end;

function GetSignatureSecurityRaw(algo: TCryptAsymAlgo;
  const signature: RawByteString): RawUtf8;
var
  derlen: cardinal;
  der: PByteArray;
  eccbytes, len: PtrUInt;
  buf: array [0..131] of AnsiChar;
begin
  if algo in CAA_RAWSIGNATURE then
  begin
    // no need to be decoded, since RSA and EdDSA have no SEQ
    result := BinToBase64uri(pointer(signature), length(signature));
    exit;
  end;
  result := '';
  derlen := length(signature);
  der := pointer(signature);
  if (derlen < 50) or
     (der[0] <> ASN1_SEQ) or
     (der[1] > derlen - 2) then
    exit;
  eccbytes := CAA_SIZE[algo];
  if der[1] and $80 <> 0 then
  begin
    if (der[1] and $7f) <> 1 then // 2-byte length (e.g. ES512)
      exit;
    len := der[2]; // length in 2nd byte
    if DerParse(DerParse(@der[3], @buf[0], eccbytes),
        @buf[eccbytes], eccbytes) <> PAnsiChar(@der[len + 3]) then
      exit;
  end
  else
  begin
    len := der[1]; // 1-byte length
    if DerParse(DerParse(@der[2], @buf[0], eccbytes),
        @buf[eccbytes], eccbytes) <> PAnsiChar(@der[len + 2]) then
      exit;
  end;
  result := BinToBase64uri(@buf[0], eccbytes * 2);
end;

function SetSignatureSecurityRaw(algo: TCryptAsymAlgo;
  const rawsignature: RawUtf8): RawByteString;
var
  eccbytes: PtrInt;
begin
  result := rawsignature;
  if (result = '') or
     (algo in CAA_RAWSIGNATURE) then
     // no need to be encoded, since RSA and EdDSA have no SEQ
    exit;
  eccbytes := CAA_SIZE[algo];
  if length(result) = eccbytes * 2 then
    result := Asn(ASN1_SEQ, [
      AsnEncInt(@PByteArray(result)[0], eccbytes),
      AsnEncInt(@PByteArray(result)[eccbytes], eccbytes)
      ]);
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
  result := '';
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
  result := '';
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
  result := '';
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
  result := '';
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
    salt := Join([realm, 'host', name, '.', LowerCaseU(realm)])
  else
    salt := Join([realm, name]);
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

function OidToCka(const oid, oid2: RawUtf8): TCryptKeyAlgo;
begin
  if oid = CKA_OID[ckaRsa] then
    result := ckaRsa
  else if oid = CKA_OID[ckaRsaPss] then
    result := ckaRsaPss
  else if oid = ASN1_OID_X962_PUBLICKEY then
  begin
    for result := ckaEcc256 to ckaEcc256k do
      if oid2 = CKA_OID[result] then
        exit;
    result := ckaNone;
  end
  else if oid = CKA_OID[ckaEdDSA] then
    result := ckaEdDSA
  else
    result := ckaNone;
end;

function CkaToSeq(cka: TCryptKeyAlgo): RawByteString;
begin
  case cka of
    ckaRsa:
      result := Asn(ASN1_SEQ, [
                  AsnOid(pointer(CKA_OID[ckaRsa])),
                  ASN1_NULL_VALUE // optional
                ]);
    ckaRsaPss,
    ckaEdDSA:
      result := Asn(ASN1_SEQ, [
                  AsnOid(pointer(CKA_OID[cka]))
                ]);
    ckaEcc256 .. ckaEcc256k:
      result := Asn(ASN1_SEQ, [
                  AsnOid(ASN1_OID_X962_PUBLICKEY),
                  AsnOid(pointer(CKA_OID[cka]))
                ]);
  else
    begin
      result := ''; // make compiler happy
      ECrypt.RaiseUtf8('Unexpected CkaToSeq(%)', [ToText(cka)^]);
    end;
  end;
end;

function EccPrivKeyToSeq(
  cka: TCryptKeyAlgo; const rawecc: RawByteString): RawByteString;
var
  oct: RawByteString;
begin
  // see PemDerRawToEcc() secp256r1/prime256v1 PKCS#8 PrivateKeyInfo
  oct := AsnSafeOct([Asn(1),
                     Asn(ASN1_OCTSTR, [rawecc])]);
  result := Asn(ASN1_SEQ, [
              Asn(0), // version
              CkaToSeq(cka),
              oct
            ]);
  FillZero(oct);
end;

function SeqToEccPrivKey(cka: TCryptKeyAlgo; const seq: RawByteString;
  rfcpub: PRawByteString): RawByteString;
var
  oid, oct, key: RawByteString;
  pos, posoct, vt, vers: integer;
begin
  result := '';
  if rfcpub <> nil then
    rfcpub^ := '';
  // initial sequence decoding
  pos := 1;
  if AsnNext(pos, seq) <> ASN1_SEQ then
    exit;
  vers := AsnNextInteger(pos, seq, vt);
  if vt = ASN1_INT then
    case vers of
      0: // PKCS#8 format
        if (AsnNext(pos, seq) = ASN1_SEQ) and // privateKeyAlgorithm
           (AsnNext(pos, seq, @oid) = ASN1_OBJID) then
        begin
          // CkaToSeq() decoding
          case cka of
            ckaEcc256 .. ckaEcc256k:
              if (oid <> ASN1_OID_X962_PUBLICKEY) or
                 (AsnNext(pos, seq, @oid) <> ASN1_OBJID) then
                exit;
            ckaEdDSA:
              ;
          else
            exit; // this function is dedicated to ECC
          end;
          if oid <> CKA_OID[cka] then
            exit;
          // private key raw binary extraction
          posoct := 1;
          if (AsnNextRaw(pos, seq, oct) = ASN1_OCTSTR) and // privateKey
             (AsnNext(posoct, oct{%H-}) = ASN1_SEQ) and
             (AsnNext(posoct, oct) = ASN1_INT) and
             (AsnNextRaw(posoct, oct, key) = ASN1_OCTSTR) then
            result := key;
        end;
      1: // https://www.rfc-editor.org/rfc/rfc5915 EC key pair alternate format
       if (cka in CKA_ECC) and                           // Elliptic Curve only
          (AsnNextRaw(pos, seq, key) = ASN1_OCTSTR) then // privateKey
       begin
         vt := AsnNext(pos, seq);
         if vt = ASN1_NULL then
           result := key // just privateKey, without optional constructed fields
         else if (vt = ASN1_CTC0) and  // [0] ECparameters (optional)
                 (AsnNext(pos, seq, @oid) = ASN1_OBJID) and
                 {%H-}(oid = CKA_OID[cka]) then
         begin
           result := key;
           if (rfcpub <> nil) and       // [1] publicKey (optional)
              (AsnNext(pos, seq) = ASN1_CTC1) and
              (AsnNextRaw(pos, seq, key) = ASN1_BITSTR) then
             rfcpub^ := key;
        end;
      end;
    end;
  FillZero(oct);
  FillZero(key);
end;

function SeqToEccPubKey(cka: TCryptKeyAlgo; const seq: RawByteString): RawByteString;
var
  oid: RawByteString;
  pos: integer;
begin
  result := '';
  // PKCS#8 sequence decoding
  pos := 1;
  if (AsnNext(pos, seq) <> ASN1_SEQ) or
     (AsnNext(pos, seq) <> ASN1_SEQ) or // algorithm
     (AsnNext(pos, seq, @oid) <> ASN1_OBJID) then
    exit;
  // CkaToSeq() decoding and validating
  case cka of
    ckaEcc256 .. ckaEcc256k:
      if (oid <> ASN1_OID_X962_PUBLICKEY) or
         (AsnNext(pos, seq, @oid) <> ASN1_OBJID) then
        exit;
    ckaEdDSA:
      ;
  else
    exit; // this function is dedicated to ECC: use X509PubKeyFromDer() instead
  end;
  if oid = CKA_OID[cka] then
    // public key raw binary extraction
    if AsnNextRaw(pos, seq, result) <> ASN1_BITSTR then
      result := '';
end;

function X509PubKeyToDer(Algorithm: TCryptKeyAlgo;
  const SubjectPublicKey: RawByteString): RawByteString;
begin
  result := Asn(ASN1_SEQ, [
              CkaToSeq(Algorithm),
              Asn(ASN1_BITSTR, [
                SubjectPublicKey
              ])
            ]);
end;

function X509PubKeyFromDer(const PkcsDer: RawByteString): RawByteString;
var
  pos: integer;
  algoseq: RawByteString; // algorithm OID(s) as encoded by CkaToSeq()
begin
  pos := 1;
  if (AsnNext(pos, PkcsDer) <> ASN1_SEQ) or
     (AsnNextRaw(pos, PkcsDer, algoseq) <> ASN1_SEQ) or
     (AsnNextRaw(pos, PkcsDer, result) <> ASN1_BITSTR) then
    result := '';
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
              bits := '      Modulus:'#13#10 +
                BinToHumanHex(pointer(modulo), length(modulo), 16, 8) +
                '      Exponent: 0x' + BinToHex(exp) + #13#10 ;
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

function ParsedToText(const c: TX509Parsed): RawUtf8;

  procedure KeyUsage(l, h: TCryptCertUsage; const ext: RawUtf8);
  var
    cu: TCryptCertUsage;
    usage: RawUtf8;
  begin
    for cu := l to h do
      if cu in c.Usage then
        AddToCsv(CU_FULLTEXT[cu], usage, ', ');
    if usage <> '' then
      result := result +   '    X509v3 ' + ext + #13#10 +
                           '      ' + usage + #13#10;
  end;

var
  bits: RawUtf8;
  version: integer;
begin
  // somewhat follows X509_print() OpenSSL formatting
  if (c.Usage <> []) or
     (c.SubjectID <> '') or
     (c.IssuerID <> '') then
    version := 2   // X.509 v3
  else
    version := 1;
  X509PubKeyBits(c.PubKey, @bits);
  result := 'Certificate:'#13#10 +
            '  Version: ' + SmallUInt32Utf8[version + 1] +
                   ' (0x' + SmallUInt32Utf8[version] + ')'#13#10 +
            '  Serial Number:'#13#10 +
            '    ' + c.Serial + #13#10 +
            '  Signature Algorithm: ' + c.SigAlg + #13#10 +
            '  Issuer: ' + c.IssuerDN + #13#10 +
            '  Validity'#13#10 +
            '    Not Before: ' + DateTimeToHttpDate(c.NotBefore) + #13#10 +
            '    Not After : ' + DateTimeToHttpDate(c.NotAfter)  + #13#10 +
            '  Subject: ' + c.SubjectDN + #13#10 +
            '  Subject Public Key Info:'#13#10 +
            '    Public Key Algorithm: ' + c.PubAlg + #13#10 +
            bits;
  if version = 1 then
    exit;
  // append the X.509 v3 known extensions
  result := result + '  X509v3 extensions:'#13#10;
  KeyUsage(cuCrlSign, cuDigitalSignature, 'Key Usage: critical');
  KeyUsage(cuTlsServer, cuTimestamp, 'Extended Key Usage:');
  if cuCA in c.Usage then
    bits := 'TRUE'
  else
    bits := 'FALSE';
  result := result + '    X509v3 Basic Constraints: critical'#13#10 +
                     '      CA:' + bits + #13#10;
  if c.SubjectID <> '' then
    result := result + '    X509v3 Subject Key Identifier:'#13#10 +
                       '      ' + c.SubjectID + #13#10;
  if c.IssuerID <> '' then
    result := result + '    X509v3 Authority Key Identifier:'#13#10 +
                       '      ' + c.IssuerID + #13#10;
  if c.SubjectAltNames <> '' then
    result := result + '    X509v3 Subject Alternative Name:'#13#10 +
                       '      ' + c.SubjectAltNames + #13#10;
end;

{$ifdef OSWINDOWS}

procedure WinInfoToParse(const c: TWinCertInfo; out Info: TX509Parsed);
begin
  Info.Serial          := c.Serial;
  Info.SubjectDN       := c.SubjectName;
  Info.IssuerDN        := c.IssuerName;
  Info.SubjectAltNames := c.SubjectAltNames;
  Info.SubjectID       := c.SubjectID;
  Info.IssuerID        := c.IssuerID;
  Info.SigAlg          := c.AlgorithmName;
  Info.PubAlg          := c.PublicKeyAlgorithmName;
  Info.Usage           := TCryptCertUsages(c.Usage); // match TWinCertUsages
  Info.NotBefore       := c.NotBefore;
  Info.NotAfter        := c.NotAfter;
  Info.PubKey          := c.PublicKeyContent;
  Info.PeerInfo        := ParsedToText(Info); // should be the last
end;

function WinX509Parse(const Cert: RawByteString; out Info: TX509Parsed): boolean;
var
  c: TWinCertInfo;
begin
  result := WinCertDecode(PemToDer(Cert), c);
  if result then
    WinInfoToParse(c, Info);
end;

function Win2Text(const c: TWinCertInfo): RawUtf8;
var
  nfo: TX509Parsed;
begin
  WinInfoToParse(c, nfo);
  result := nfo.PeerInfo;
end;

{$endif OSWINDOWS}

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
    ECrypt.RaiseUtf8('Invalid AsnTime(%)', [dt]);
end;

function AsnNextTime(var Pos: integer; const Buffer: TAsnObject;
  out Value: TDateTime): boolean;
var
  vt: integer;
  raw: RawByteString;
begin
  vt := AsnNextRaw(pos, Buffer, raw);
  result := false;
  if length(raw) < 12 then
    exit;
  case vt of
    ASN1_UTCTIME:
      Prepend(raw, '20'); // YY -> YYYY
    ASN1_GENTIME:
      if raw = '99991231235959Z' then
      begin
        Value := 0; // special value for unspecified NotAfter
        result := true;
        exit;
      end;
  else
    exit;
  end;
  insert('T', raw, 9); // make ISO-8601 compatible 'YYYYMMDDThhmmss'
  Iso8601ToDateTimePUtf8CharVar(pointer(raw), length(raw), Value);
  result := Value <> 0;
end;

function AsnDecIp(p: PAnsiChar; len: integer): RawUtf8;
begin
  case len of
    4:
      with PDWordRec(p)^ do
        FormatUtf8('%.%.%.%', [B[0], B[1], B[2], B[3]], result);
   16:
     // expanded IPv6 xx:xx:xx:...:xx content (no mormot.net.sock dependency)
     ToHumanHex(result, pointer(p), len);
  else
    BinToHexLower(p, len, result);
  end;
end;

function IsBinaryString(var Value: RawByteString): boolean;
var
  n: PtrInt;
begin
  result := true;
  for n := 1 to length(Value) do
    case ord(Value[n]) of
      0:
        if n <> length(value) then
          exit
        else
          // consider null-terminated strings as non-binary, but truncate
          SetLength(Value, n - 1);
      1..8, // consider TAB (#9) char as text
      10..31:
        exit;
    end;
  result := false;
end;

procedure DumpClass(at: integer; w: TTextWriter);
begin
  if at and ASN1_CL_APP <> 0 then
    w.AddShorter('APP ');
  if at and ASN1_CL_CTX <> 0 then
    w.AddShorter('CTX ');
  if at and ASN1_CL_PRI = ASN1_CL_PRI then
    w.AddShorter('PRI ');
  if at < ASN1_CL_APP then
    w.AddShorter('unknown')
  else
    w.AddByteToHex(at and $0f);
end;

function AsnDump(const Value: TAsnObject): RawUtf8;
var
  i, at, x, n, indent, ilcount: integer;
  s: RawByteString;
  il: TIntegerDynArray;
  w: TTextWriter;
  tmp: TTextWriterStackBuffer;
begin
  w := TTextWriter.CreateOwnedStream(tmp);
  try
    i := 1;
    ilcount := 0;
    indent := 0;
    while i < length(Value) do
    begin
      for n := ilcount - 1 downto 0 do
        if il[n] <= i then
        begin
          DeleteInteger(il, ilcount, n);
          dec(indent, 2);
        end;
      at := AsnNext(i, Value, @s);
      w.AddChars(' ', indent);
      w.Add('$');
      w.AddByteToHexLower(at);
      if (at and ASN1_CL_CTR) <> 0 then
      begin
        w.Add(' ');
        case at of
          ASN1_SEQ:
            w.AddShorter('SEQ');
          ASN1_SETOF:
            w.AddShorter('SETOF');
        else
          DumpClass(at, w);
        end;
        x := length(s);
        w.Add(' CTR: length %', [x]);
        inc(indent, 2);
        AddInteger(il, ilcount, x + i - 1);
      end
      else
      begin
        w.Add(' ');
        case at of
          // base ASN.1 types
          ASN1_BOOL:
            w.AddShorter('BOOL');
          ASN1_INT:
            w.AddShorter('INT');
          ASN1_BITSTR:
            w.AddShorter('BITSTR');
          ASN1_OCTSTR:
            w.AddShorter('OCTSTR');
          ASN1_NULL:
            w.AddShorter('NULL');
          ASN1_OBJID:
            w.AddShorter('OBJID');
          ASN1_ENUM:
            w.AddShorter('ENUM');
          ASN1_UTF8STRING:
            w.AddShorter('UTF8');
          ASN1_PRINTSTRING:
            w.AddShorter('PRINT');
          ASN1_IA5STRING:
            w.AddShorter('IA5');
          ASN1_UTCTIME:
            w.AddShorter('UTC');
          ASN1_GENTIME:
            w.AddShorter('GEN');
        else
          DumpClass(at, w);
        end;
        w.Add(':', ' ');
        if IsBinaryString(s) then
        begin
          w.Add('binary len=% ', [length(s)]);
          w.AddShort(EscapeToShort(s));
        end
        else if at in ASN1_NUMBERS then
          w.AddString(s) // not quoted value
        else if PosExChar('"', s) = 0 then
        begin
          w.Add('"');
          w.AddString(s);
          w.AddDirect('"');
        end
        else
        begin
          w.Add('''');
          w.AddString(s); // alternate output layout for quoted text
          w.AddDirect('''');
        end;
      end;
      w.AddDirectNewLine; // adapted to the current console output
    end;
    w.SetText(result);
  finally
    w.Free;
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
  // Rnd/Sign/Hash/Cipher/Asym/Cert/Store are registered in GlobalCryptAlgoInit
  {$ifdef OSWINDOWS}
  X509Parse         := @WinX509Parse; // use mormot.lib.sspi.pas WinCertDecode()
  WinCertInfoToText := @Win2Text;
  {$endif OSWINDOWS}
end;

procedure FinalizeUnit;
begin
end;


initialization
  InitializeUnit;

finalization
  FinalizeUnit;
  
end.

