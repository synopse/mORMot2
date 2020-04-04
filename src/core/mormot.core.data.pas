/// Framework Core Low-Level Data Processing Functions
// - this unit is a part of the freeware Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.data;

{
  *****************************************************************************

   Low-Level Data Processing Functions shared by all framework units
    - RTL TPersistent / TInterfacedObject with Custom Constructor
    - TSynPersistent* / TSyn*List / TSynLocker classes
    - Variable Length Integer Encoding / Decoding
    - TFastReader / TBufferWriter Binary Streams
    - Base64, Base64URI, URL and Baudot Encoding / Decoding
    - INI Files and In-memory Access
    - TAlgoCompress Compression/Decompression Classes - with AlgoSynLZ
    - Efficient RTTI Values Binary Serialization and Comparison
  TODO:
    - TDynArray wrapper
    - RawUTF8 String Values Interning
    - TSynNameValue Name/Value Storage


  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  classes,
  contnrs,
  types,
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.text; // for INI process


{ ************ RTL TPersistent / TInterfacedObject with Custom Constructor }

type
    /// abstract parent class with a virtual constructor, ready to be overridden
  // to initialize the instance
  // - you can specify such a class if you need an object including published
  // properties (like TPersistent) with a virtual constructor (e.g. to
  // initialize some nested class properties)
  TPersistentWithCustomCreate = class(TPersistent)
  public
    /// this virtual constructor will be called at instance creation
    // - this constructor does nothing, but is declared as virtual so that
    // inherited classes may safely override this default void implementation
    constructor Create; virtual;
  end;

  {$M+}
  /// abstract parent class with threadsafe implementation of IInterface and
  // a virtual constructor
  // - you can specify e.g. such a class to TSQLRestServer.ServiceRegister() if
  // you need an interfaced object with a virtual constructor, ready to be
  // overridden to initialize the instance
  TInterfacedObjectWithCustomCreate = class(TInterfacedObject)
  public
    /// this virtual constructor will be called at instance creation
    // - this constructor does nothing, but is declared as virtual so that
    // inherited classes may safely override this default void implementation
    constructor Create; virtual;
    /// used to mimic TInterfacedObject reference counting
    // - Release=true will call TInterfacedObject._Release
    // - Release=false will call TInterfacedObject._AddRef
    // - could be used to emulate proper reference counting of the instance
    // via interfaces variables, but still storing plain class instances
    // (e.g. in a global list of instances)
    procedure RefCountUpdate(Release: boolean); virtual;
  end;
  {$M-}

  /// used to determine the exact class type of a TInterfacedObjectWithCustomCreate
  // - could be used to create instances using its virtual constructor
  TInterfacedObjectWithCustomCreateClass = class of TInterfacedObjectWithCustomCreate;

  /// used to determine the exact class type of a TPersistentWithCustomCreateClass
  // - could be used to create instances using its virtual constructor
  TPersistentWithCustomCreateClass = class of TPersistentWithCustomCreate;


{ ************ TSynPersistent* / TSyn*List / TSynLocker classes }

type
  {$M+}
  /// our own empowered TPersistent-like parent class
  // - TPersistent has an unexpected speed overhead due a giant lock introduced
  // to manage property name fixup resolution (which we won't use outside the VCL)
  // - this class has a virtual constructor, so is a preferred alternative
  // to both TPersistent and TPersistentWithCustomCreate classes
  // - for best performance, any type inheriting from this class will bypass
  // some regular steps: do not implement interfaces or use TMonitor with them!
  // - this class also features some protected methods to customize the
  // instance JSON serialization
  TSynPersistent = class(TObject)
  protected
    // this default implementation will call AssignError()
    procedure AssignTo(Dest: TSynPersistent); virtual;
    procedure AssignError(Source: TSynPersistent);
    // called before TTextWriter.WriteObject() serialize this instance as JSON
    // - you can return true if your method made the serialization
    // - this default implementation just returns false, to continue serializing
    // - TSynMonitor will change the serialization Options for this instance
    // - TSynPersistentWithID will serialize its ID field
    function BeforeWriteObject(W: TBaseWriter;
      var Options: TTextWriterWriteObjectOptions): boolean; virtual;
    // called by TTextWriter.WriteObject() to serialize one published property value
    // - is overriden in TSQLRecord/TSQLRecordMany to detect "fake" instances
    // or by TSynPersistentWithPassword to hide the password field value
    // - should return true if a property has been written, false (which is the
    // default) if the property is to be serialized as usual
    function WritePropertyValue(W: TBaseWriter; Prop: PRttiProp;
      Options: TTextWriterWriteObjectOptions): boolean; virtual;
    // called after TTextWriter.WriteObject() serialized this instance as JSON
    procedure AfterWriteObject(W: TBaseWriter); virtual;
  public
    /// this virtual constructor will be called at instance creation
    // - this constructor does nothing, but is declared as virtual so that
    // inherited classes may safely override this default void implementation
    constructor Create; virtual;
    /// allows to implement a TPersistent-like assignement mechanism
    // - inherited class should override AssignTo() protected method
    // to implement the proper assignment
    procedure Assign(Source: TSynPersistent); virtual;
    /// optimized initialization code
    // - somewhat faster than the regular RTL implementation
    // - warning: this optimized version won't initialize the vmtIntfTable
    // for this class hierarchy: as a result, you would NOT be able to
    // implement an interface with a TSynPersistent descendent (but you should
    // not need to, but inherit from TInterfacedObject)
    // - warning: under FPC, it won't initialize fields management operators
    class function NewInstance: TObject; override;
  end;
  {$M-}

  /// simple and efficient TList, without any notification
  // - regular TList has an internal notification mechanism which slows down
  // basic process, and can't be easily inherited
  // - stateless methods (like Add/Clear/Exists/Remove) are defined as virtual
  // since can be overriden e.g. by TSynObjectListLocked to add a TSynLocker
  TSynList = class(TSynPersistent)
  protected
    fCount: integer;
    fList: TPointerDynArray;
    function Get(index: Integer): pointer; {$ifdef HASINLINE} inline; {$endif}
  public
    /// add one item to the list
    function Add(item: pointer): integer; virtual;
    /// delete all items of the list
    procedure Clear; virtual;
    /// delete one item from the list
    procedure Delete(index: integer); virtual;
    /// fast retrieve one item in the list
    function IndexOf(item: pointer): integer; virtual;
    /// fast check if one item exists in the list
    function Exists(item: pointer): boolean; virtual;
    /// fast delete one item in the list
    function Remove(item: pointer): integer; virtual;
    /// how many items are stored in this TList instance
    property Count: integer read fCount;
    /// low-level access to the items stored in this TList instance
    property List: TPointerDynArray read fList;
    /// low-level array-like access to the items stored in this TList instance
    // - warning: if index is out of range, will return nil and won't raise
    // any exception
    property Items[index: Integer]: pointer read Get; default;
  end;

  /// simple and efficient TObjectList, without any notification
  TSynObjectList = class(TSynList)
  protected
    fOwnObjects: boolean;
  public
    /// initialize the object list
    constructor Create(aOwnObjects: boolean=true); reintroduce;
    /// delete one object from the list
    procedure Delete(index: integer); override;
    /// delete all objects of the list
    procedure Clear; override;
    /// delete all objects of the list in reverse order
    // - for some kind of processes, owned objects should be removed from the
    // last added to the first
    procedure ClearFromLast; virtual;
    /// finalize the store items
    destructor Destroy; override;
  end;

  /// allow to add cross-platform locking methods to any class instance
  // - typical use is to define a Safe: TSynLocker property, call Safe.Init
  // and Safe.Done in constructor/destructor methods, and use Safe.Lock/UnLock
  // methods in a try ... finally section
  // - in respect to the TCriticalSection class, fix a potential CPU cache line
  // conflict which may degrade the multi-threading performance, as reported by
  // @http://www.delphitools.info/2011/11/30/fixing-tcriticalsection
  // - internal padding is used to safely store up to 7 values protected
  // from concurrent access with a mutex, so that SizeOf(TSynLocker)>128
  // - for object-level locking, see TSynPersistentLock which owns one such
  // instance, or call low-level fSafe := NewSynLocker in your constructor,
  // then fSafe^.DoneAndFreemem in your destructor
  TSynLocker = object
  protected
    fSection: TRTLCriticalSection;
    fSectionPadding: PtrInt; // paranoid to avoid FUTEX_WAKE_PRIVATE=EAGAIN
    fLocked, fInitialized: boolean;
    function GetVariant(Index: integer): Variant;
    procedure SetVariant(Index: integer; const Value: Variant);
    function GetInt64(Index: integer): Int64;
    procedure SetInt64(Index: integer; const Value: Int64);
    function GetBool(Index: integer): boolean;
    procedure SetBool(Index: integer; const Value: boolean);
    function GetUnlockedInt64(Index: integer): Int64;
    procedure SetUnlockedInt64(Index: integer; const Value: Int64);
    function GetPointer(Index: integer): Pointer;
    procedure SetPointer(Index: integer; const Value: Pointer);
    function GetUTF8(Index: integer): RawUTF8;
    procedure SetUTF8(Index: integer; const Value: RawUTF8);
  public
    /// internal padding data, also used to store up to 7 variant values
    // - this memory buffer will ensure no CPU cache line mixup occurs
    // - you should not use this field directly, but rather the Locked[],
    // LockedInt64[], LockedUTF8[] or LockedPointer[] methods
    // - if you want to access those array values, ensure you protect them
    // using a Safe.Lock; try ... Padding[n] ... finally Safe.Unlock structure,
    // and maintain the PaddingUsedCount field accurately
    Padding: array[0..6] of TVarData;
    /// number of values stored in the internal Padding[] array
    // - equals 0 if no value is actually stored, or a 1..7 number otherwise
    // - you should not have to use this field, but for optimized low-level
    // direct access to Padding[] values, within a Lock/UnLock safe block
    PaddingUsedCount: integer;
    /// initialize the mutex
    // - calling this method is mandatory (e.g. in the class constructor owning
    // the TSynLocker instance), otherwise you may encounter unexpected
    // behavior, like access violations or memory leaks
    procedure Init;
    /// finalize the mutex
    // - calling this method is mandatory (e.g. in the class destructor owning
    // the TSynLocker instance), otherwise you may encounter unexpected
    // behavior, like access violations or memory leaks
    procedure Done;
    /// finalize the mutex, and call FreeMem() on the pointer of this instance
    // - should have been initiazed with a NewSynLocker call
    procedure DoneAndFreeMem;
    /// lock the instance for exclusive access
    // - this method is re-entrant from the same thread (you can nest Lock/UnLock
    // calls in the same thread), but would block any other Lock attempt in
    // another thread
    // - use as such to avoid race condition (from a Safe: TSynLocker property):
    // ! Safe.Lock;
    // ! try
    // !   ...
    // ! finally
    // !   Safe.Unlock;
    // ! end;
    procedure Lock; {$ifdef FPC} inline; {$endif}
    /// will try to acquire the mutex
    // - use as such to avoid race condition (from a Safe: TSynLocker property):
    // ! if Safe.TryLock then
    // !   try
    // !     ...
    // !   finally
    // !     Safe.Unlock;
    // !   end;
    function TryLock: boolean; {$ifdef FPC} inline; {$endif}
    /// will try to acquire the mutex for a given time
    // - use as such to avoid race condition (from a Safe: TSynLocker property):
    // ! if Safe.TryLockMS(100) then
    // !   try
    // !     ...
    // !   finally
    // !     Safe.Unlock;
    // !   end;
    function TryLockMS(retryms: integer): boolean;
    /// release the instance for exclusive access
    // - each Lock/TryLock should have its exact UnLock opposite, so a
    // try..finally block is mandatory for safe code
    procedure UnLock; {$ifdef FPC} inline; {$endif}
    /// will enter the mutex until the IUnknown reference is released
    // - could be used as such under Delphi:
    // !begin
    // !  ... // unsafe code
    // !  Safe.ProtectMethod;
    // !  ... // thread-safe code
    // !end; // local hidden IUnknown will release the lock for the method
    // - warning: under FPC, you should assign its result to a local variable -
    // see bug http://bugs.freepascal.org/view.php?id=26602
    // !var LockFPC: IUnknown;
    // !begin
    // !  ... // unsafe code
    // !  LockFPC := Safe.ProtectMethod;
    // !  ... // thread-safe code
    // !end; // LockFPC will release the lock for the method
    // or
    // !begin
    // !  ... // unsafe code
    // !  with Safe.ProtectMethod do begin
    // !    ... // thread-safe code
    // !  end; // local hidden IUnknown will release the lock for the method
    // !end;
    function ProtectMethod: IUnknown;
    /// returns true if the mutex is currently locked by another thread
    property IsLocked: boolean read fLocked;
    /// returns true if the Init method has been called for this mutex
    // - is only relevant if the whole object has been previously filled with 0,
    // i.e. as part of a class or as global variable, but won't be accurate
    // when allocated on stack
    property IsInitialized: boolean read fInitialized;
    /// safe locked access to a Variant value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // LockedBool, LockedInt64, LockedPointer and LockedUTF8 array properties
    // - returns null if the Index is out of range
    property Locked[Index: integer]: Variant read GetVariant write SetVariant;
    /// safe locked access to a Int64 value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked and LockedUTF8 array properties
    // - Int64s will be stored internally as a varInt64 variant
    // - returns nil if the Index is out of range, or does not store a Int64
    property LockedInt64[Index: integer]: Int64 read GetInt64 write SetInt64;
    /// safe locked access to a boolean value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked, LockedInt64, LockedPointer and LockedUTF8 array properties
    // - value will be stored internally as a varBoolean variant
    // - returns nil if the Index is out of range, or does not store a boolean
    property LockedBool[Index: integer]: boolean read GetBool write SetBool;
    /// safe locked access to a pointer/TObject value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked, LockedBool, LockedInt64 and LockedUTF8 array properties
    // - pointers will be stored internally as a varUnknown variant
    // - returns nil if the Index is out of range, or does not store a pointer
    property LockedPointer[Index: integer]: Pointer read GetPointer write SetPointer;
    /// safe locked access to an UTF-8 string value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked and LockedPointer array properties
    // - UTF-8 string will be stored internally as a varString variant
    // - returns '' if the Index is out of range, or does not store a string
    property LockedUTF8[Index: integer]: RawUTF8 read GetUTF8 write SetUTF8;
    /// safe locked in-place increment to an Int64 value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked and LockedUTF8 array properties
    // - Int64s will be stored internally as a varInt64 variant
    // - returns the newly stored value
    // - if the internal value is not defined yet, would use 0 as default value
    function LockedInt64Increment(Index: integer; const Increment: Int64): Int64;
    /// safe locked in-place exchange of a Variant value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked and LockedUTF8 array properties
    // - returns the previous stored value, or null if the Index is out of range
    function LockedExchange(Index: integer; const Value: variant): variant;
    /// safe locked in-place exchange of a pointer/TObject value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked and LockedUTF8 array properties
    // - pointers will be stored internally as a varUnknown variant
    // - returns the previous stored value, nil if the Index is out of range,
    // or does not store a pointer
    function LockedPointerExchange(Index: integer; Value: pointer): pointer;
    /// unsafe access to a Int64 value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked and LockedUTF8 array properties
    // - Int64s will be stored internally as a varInt64 variant
    // - returns nil if the Index is out of range, or does not store a Int64
    // - you should rather call LockedInt64[] property, or use this property
    // with a Lock; try ... finally UnLock block
    property UnlockedInt64[Index: integer]: Int64 read GetUnlockedInt64 write SetUnlockedInt64;
  end;
  PSynLocker = ^TSynLocker;

  /// adding locking methods to a TSynPersistent with virtual constructor
  // - you may use this class instead of the RTL TCriticalSection, since it
  // would use a TSynLocker which does not suffer from CPU cache line conflit
  TSynPersistentLock = class(TSynPersistent)
  protected
    fSafe: PSynLocker; // TSynLocker would increase inherited fields offset
    // will lock/unlock the instance during JSON serialization of its properties
    function BeforeWriteObject(W: TBaseWriter;
      var Options: TTextWriterWriteObjectOptions): boolean; override;
    procedure AfterWriteObject(W: TBaseWriter); override;
  public
    /// initialize the instance, and its associated lock
    constructor Create; override;
    /// finalize the instance, and its associated lock
    destructor Destroy; override;
    /// access to the associated instance critical section
    // - call Safe.Lock/UnLock to protect multi-thread access on this storage
    property Safe: PSynLocker read fSafe;
  end;

  /// used for backward compatibility only with existing code
  TSynPersistentLocked = class(TSynPersistentLock);

  /// adding locking methods to a TInterfacedObject with virtual constructor
  TInterfacedObjectLocked = class(TInterfacedObjectWithCustomCreate)
  protected
    fSafe: PSynLocker; // TSynLocker would increase inherited fields offset
  public
    /// initialize the object instance, and its associated lock
    constructor Create; override;
    /// release the instance (including the locking resource)
    destructor Destroy; override;
    /// access to the locking methods of this instance
    // - use Safe.Lock/TryLock with a try ... finally Safe.Unlock block
    property Safe: PSynLocker read fSafe;
  end;

  /// add locking methods to a TSynObjectList
  // - this class overrides the regular TSynObjectList, and do not share any
  // code with the TObjectListHashedAbstract/TObjectListHashed classes
  // - you need to call the Safe.Lock/Unlock methods by hand to protect the
  // execution of index-oriented methods (like Delete/Items/Count...): the
  // list content may change in the background, so using indexes is thread-safe
  // - on the other hand, Add/Clear/ClearFromLast/Remove stateless methods have
  // been overriden in this class to call Safe.Lock/Unlock, and therefore are
  // thread-safe and protected to any background change
  TSynObjectListLocked = class(TSynObjectList)
  protected
    fSafe: TSynLocker;
  public
    /// initialize the list instance
    // - the stored TObject instances will be owned by this TSynObjectListLocked,
    // unless AOwnsObjects is set to false
    constructor Create(aOwnsObjects: boolean=true); reintroduce;
    /// release the list instance (including the locking resource)
    destructor Destroy; override;
    /// add one item to the list using the global critical section
    function Add(item: pointer): integer; override;
    /// delete all items of the list using the global critical section
    procedure Clear; override;
    /// delete all items of the list in reverse order, using the global critical section
    procedure ClearFromLast; override;
    /// fast delete one item in the list
    function Remove(item: pointer): integer; override;
    /// check an item using the global critical section
    function Exists(item: pointer): boolean; override;
    /// the critical section associated to this list instance
    // - could be used to protect shared resources within the internal process,
    // for index-oriented methods like Delete/Items/Count...
    // - use Safe.Lock/TryLock with a try ... finally Safe.Unlock block
    property Safe: TSynLocker read fSafe;
  end;

  /// a 64-bit identifier, defined for TSynPersistentWithID
  // - type used for our ORM primary key, i.e. TSQLRecord.ID
  // - it maps the SQLite3 64-bit RowID definition
  TID = type Int64;

  /// a pointer to TSynPersistentWithID.ID, i.e. our ORM primary key
  PID = ^TID;

  /// used to store a dynamic array of ORM primary keys, i.e. TSQLRecord.ID
  TIDDynArray = array of TID;

  /// pointer to a dynamic array of ORM primary keys, i.e. TSQLRecord.ID
  PIDDynArray = ^TIDDynArray;

  /// abstract persistent class with a 64-bit TID field
  // - is e.g. the parent of our TSQLRecord ORM classes
  // - is defined here for proper class serialization in mormot.core.json.pas,
  //  without the need of linking the ORM code to the executable
  TSynPersistentWithID = class(TSynPersistent)
  protected
    fID: TID;
    /// copy the TID field value
    procedure AssignTo(Dest: TSynPersistent); override;
    /// will append the ID field value
    function BeforeWriteObject(W: TBaseWriter;
      var Options: TTextWriterWriteObjectOptions): boolean; override;
  public
    /// this property gives direct access to the class instance ID
    // - not defined as "published" since TTextWriter.WriteObject will identify
    // this class definition, and serialize it as expected
    property IDValue: TID read fID write fID;
  end;

  /// abstract TSynPersistent class allowing safe storage of a password
  // - the associated Password, e.g. for storage or transmission encryption
  // will be persisted encrypted with a private key (which can be customized)
  // - if default simple symmetric encryption is not enough, you may define
  // a custom TSynPersistentWithPasswordUserCrypt callback, e.g. to
  // SynCrypto's CryptDataForCurrentUser, for hardened password storage
  // - a published property should be defined as such in inherited class:
  // ! property PasswordPropertyName: RawUTF8 read fPassword write fPassword;
  // - use the PassWordPlain property to access to its uncyphered value
  TSynPersistentWithPassword = class(TSynPersistent)
  protected
    fPassWord: RawUTF8;
    fKey: cardinal;
    function GetKey: cardinal; {$ifdef HASINLINE}inline;{$endif}
    function GetPassWordPlain: RawUTF8;
    function GetPassWordPlainInternal(AppSecret: RawUTF8): RawUTF8;
    procedure SetPassWordPlain(const Value: RawUTF8);
    // overriden to implement woHideSynPersistentPassword option
    function WritePropertyValue(W: TBaseWriter; Prop: PRttiProp;
      Options: TTextWriterWriteObjectOptions): boolean; override;
  public
    /// finalize the instance
    destructor Destroy; override;
    /// this class method could be used to compute the encrypted password,
    // ready to be stored as JSON, according to a given private key
    class function ComputePassword(const PlainPassword: RawUTF8;
      CustomKey: cardinal = 0): RawUTF8; overload;
    /// this class method could be used to compute the encrypted password from
    // a binary digest, ready to be stored as JSON, according to a given private key
    // - just a wrapper around ComputePassword(BinToBase64URI())
    class function ComputePassword(PlainPassword: pointer; PlainPasswordLen: integer;
      CustomKey: cardinal = 0): RawUTF8; overload;
    /// this class method could be used to decrypt a password, stored as JSON,
    // according to a given private key
    // - may trigger a ESynException if the password was stored using a custom
    // TSynPersistentWithPasswordUserCrypt callback, and the current user
    // doesn't match the expected user stored in the field
    class function ComputePlainPassword(const CypheredPassword: RawUTF8;
      CustomKey: cardinal = 0; const AppSecret: RawUTF8 = ''): RawUTF8;
    /// the private key used to cypher the password storage on serialization
    // - application can override the default 0 value at runtime
    property Key: cardinal read GetKey write fKey;
    /// access to the associated unencrypted Password value
    // - read may trigger a ESynException if the password was stored using a
    // custom TSynPersistentWithPasswordUserCrypt callback, and the current user
    // doesn't match the expected user stored in the field
    property PasswordPlain: RawUTF8 read GetPassWordPlain write SetPassWordPlain;
  end;

  /// used to determine the exact class type of a TSynPersistent
  // - could be used to create instances using its virtual constructor
  TSynPersistentClass = class of TSynPersistent;

var
  /// function prototype to customize TSynPersistent class password storage
  // - is called when 'user1:base64pass1,user2:base64pass2' layout is found,
  // and the current user logged on the system is user1 or user2
  // - you should not call this low-level method, but assign e.g. from SynCrypto:
  // $ TSynPersistentWithPasswordUserCrypt := CryptDataForCurrentUser;
  TSynPersistentWithPasswordUserCrypt:
    function(const Data,AppServer: RawByteString; Encrypt: boolean): RawByteString;

/// naive symmetric encryption scheme using a 32-bit key
// - fast, but not very secure, since uses crc32ctab[] content as master cypher
// key: consider using SynCrypto proven AES-based algorithms instead
// - used e.g. by TSynPersistentWithPassword if global
// TSynPersistentWithPasswordUserCrypt has not been defined
procedure SymmetricEncrypt(key: cardinal; var data: RawByteString);



{ ************ TAlgoCompress Compression/Decompression Classes }

type
  /// define the implementation used by TAlgoCompress.Decompress()
  TAlgoCompressLoad = (aclNormal, aclSafeSlow, aclNoCrcFast);

  /// abstract low-level parent class for generic compression/decompression algorithms
  // - will encapsulate the compression algorithm with crc32c hashing
  // - all Algo* abtract methods should be overriden by inherited classes
  TAlgoCompress = class(TSynPersistent)
  public
    /// should return a genuine byte identifier
    // - 0 is reserved for stored, 1 for TAlgoSynLz, 2/3 for TAlgoDeflate/Fast
    // (in mORMot.pas), 4/5/6 for TAlgoLizard/Fast/Huffman (in SynLizard.pas)
    function AlgoID: byte; virtual; abstract;
    /// computes by default the crc32c() digital signature of the buffer
    function AlgoHash(Previous: cardinal;
      Data: pointer; DataLen: integer): cardinal; virtual;
    /// get maximum possible (worse) compressed size for the supplied length
    function AlgoCompressDestLen(PlainLen: integer): integer; virtual; abstract;
    /// this method will compress the supplied data
    function AlgoCompress(Plain: pointer; PlainLen: integer;
      Comp: pointer): integer; virtual; abstract;
    /// this method will return the size of the decompressed data
    function AlgoDecompressDestLen(Comp: pointer): integer; virtual; abstract;
    /// this method will decompress the supplied data
    function AlgoDecompress(Comp: pointer; CompLen: integer;
      Plain: pointer): integer; virtual; abstract;
    /// this method will partially and safely decompress the supplied data
    // - expects PartialLen <= result < PartialLenMax, depending on the algorithm
    function AlgoDecompressPartial(Comp: pointer; CompLen: integer;
      Partial: pointer; PartialLen, PartialLenMax: integer): integer; virtual; abstract;
  public
    /// will register AlgoID in the global list, for Algo() class methods
    // - no need to free this instance, since it will be owned by the global list
    // - raise a ESynException if the class or its AlgoID are already registered
    // - you should never have to call this constructor, but define a global
    // variable holding a reference to a shared instance
    constructor Create; override;
    /// get maximum possible (worse) compressed size for the supplied length
    // - including the crc32c + algo 9 bytes header
    function CompressDestLen(PlainLen: integer): integer;
      {$ifdef HASINLINE}inline;{$endif}
    /// compress a memory buffer with crc32c hashing to a RawByteString
    function Compress(const Plain: RawByteString; CompressionSizeTrigger: integer = 100;
      CheckMagicForCompressed: boolean = false; BufferOffset: integer = 0): RawByteString; overload;
      {$ifdef HASINLINE} inline;{$endif}
    /// compress a memory buffer with crc32c hashing to a RawByteString
    function Compress(Plain: PAnsiChar; PlainLen: integer; CompressionSizeTrigger: integer = 100;
      CheckMagicForCompressed: boolean = false; BufferOffset: integer = 0): RawByteString; overload;
    /// compress a memory buffer with crc32c hashing
    // - supplied Comp buffer should contain at least CompressDestLen(PlainLen) bytes
    function Compress(Plain, Comp: PAnsiChar; PlainLen, CompLen: integer;
      CompressionSizeTrigger: integer = 100; CheckMagicForCompressed: boolean = false): integer; overload;
    /// compress a memory buffer with crc32c hashing to a TByteDynArray
    function CompressToBytes(const Plain: RawByteString; CompressionSizeTrigger: integer = 100;
      CheckMagicForCompressed: boolean = false): TByteDynArray; overload;
      {$ifdef HASINLINE} inline;{$endif}
    /// compress a memory buffer with crc32c hashing to a TByteDynArray
    function CompressToBytes(Plain: PAnsiChar; PlainLen: integer;
      CompressionSizeTrigger: integer = 100; CheckMagicForCompressed: boolean = false): TByteDynArray; overload;
    /// uncompress a RawByteString memory buffer with crc32c hashing
    function Decompress(const Comp: RawByteString; Load: TAlgoCompressLoad = aclNormal;
      BufferOffset: integer = 0): RawByteString; overload;
      {$ifdef HASINLINE} inline;{$endif}
    /// uncompress a RawByteString memory buffer with crc32c hashing
    // - returns TRUE on success
    function TryDecompress(const Comp: RawByteString; out Dest: RawByteString;
      Load: TAlgoCompressLoad = aclNormal): boolean;
    /// uncompress a memory buffer with crc32c hashing
    procedure Decompress(Comp: PAnsiChar; CompLen: integer; out result: RawByteString;
      Load: TAlgoCompressLoad = aclNormal; BufferOffset: integer = 0); overload;
    /// uncompress a RawByteString memory buffer with crc32c hashing
    function Decompress(const Comp: TByteDynArray): RawByteString; overload;
      {$ifdef HASINLINE} inline;{$endif}
    /// uncompress a RawByteString memory buffer with crc32c hashing
    // - returns nil if crc32 hash failed, i.e. if the supplied Comp is not correct
    // - returns a pointer to the uncompressed data and fill PlainLen variable,
    // after crc32c hash
    // - avoid any memory allocation in case of a stored content - otherwise, would
    // uncompress to the tmp variable, and return pointer(tmp) and length(tmp)
    function Decompress(const Comp: RawByteString; out PlainLen: integer; var tmp: RawByteString;
      Load: TAlgoCompressLoad = aclNormal): pointer; overload;
      {$ifdef HASINLINE} inline;{$endif}
    /// uncompress a RawByteString memory buffer with crc32c hashing
    // - returns nil if crc32 hash failed, i.e. if the supplied Data is not correct
    // - returns a pointer to an uncompressed data buffer of PlainLen bytes
    // - avoid any memory allocation in case of a stored content - otherwise, would
    // uncompress to the tmp variable, and return pointer(tmp) and length(tmp)
    function Decompress(Comp: PAnsiChar; CompLen: integer; out PlainLen: integer;
      var tmp: RawByteString; Load: TAlgoCompressLoad = aclNormal): pointer; overload;
    /// decode the header of a memory buffer compressed via the Compress() method
    // - validates the crc32c of the compressed data (unless Load=aclNoCrcFast),
    // then return the uncompressed size in bytes, or 0 if the crc32c does not match
    // - should call DecompressBody() later on to actually retrieve the content
    function DecompressHeader(Comp: PAnsiChar; CompLen: integer;
      Load: TAlgoCompressLoad = aclNormal): integer;
    /// decode the content of a memory buffer compressed via the Compress() method
    // - PlainLen has been returned by a previous call to DecompressHeader()
    function DecompressBody(Comp, Plain: PAnsiChar; CompLen, PlainLen: integer;
      Load: TAlgoCompressLoad = aclNormal): boolean;
    /// partial decoding of a memory buffer compressed via the Compress() method
    // - returns 0 on error, or how many bytes have been written to Partial
    // - will call virtual AlgoDecompressPartial() which is slower, but expected
    // to avoid any buffer overflow on the Partial destination buffer
    // - some algorithms (e.g. Lizard) may need some additional bytes in the
    // decode buffer, so PartialLenMax bytes should be allocated in Partial^,
    // with PartialLenMax > expected PartialLen, and returned bytes may be >
    // PartialLen, but always <= PartialLenMax
    function DecompressPartial(Comp, Partial: PAnsiChar; CompLen,
      PartialLen, PartialLenMax: integer): integer;
    /// get the TAlgoCompress instance corresponding to the AlgoID stored
    // in the supplied compressed buffer
    // - returns nil if no algorithm was identified
    class function Algo(Comp: PAnsiChar; CompLen: integer): TAlgoCompress; overload;
      {$ifdef HASINLINE} inline;{$endif}
    /// get the TAlgoCompress instance corresponding to the AlgoID stored
    // in the supplied compressed buffer
    // - returns nil if no algorithm was identified
    // - also identifies "stored" content in IsStored variable
    class function Algo(Comp: PAnsiChar; CompLen: integer;
      out IsStored: boolean): TAlgoCompress; overload;
    /// get the TAlgoCompress instance corresponding to the AlgoID stored
    // in the supplied compressed buffer
    // - returns nil if no algorithm was identified
    class function Algo(const Comp: RawByteString): TAlgoCompress; overload;
      {$ifdef HASINLINE} inline;{$endif}
    /// get the TAlgoCompress instance corresponding to the AlgoID stored
    // in the supplied compressed buffer
    // - returns nil if no algorithm was identified
    class function Algo(const Comp: TByteDynArray): TAlgoCompress; overload;
      {$ifdef HASINLINE} inline;{$endif}
    /// get the TAlgoCompress instance corresponding to the supplied AlgoID
    // - returns nil if no algorithm was identified
    // - stored content is identified as TAlgoSynLZ
    class function Algo(AlgoID: byte): TAlgoCompress; overload;
    /// quickly validate a compressed buffer content, without uncompression
    // - extract the TAlgoCompress, and call DecompressHeader() to check the
    // hash of the compressed data, and return then uncompressed size
    // - returns 0 on error (e.g. unknown algorithm or incorrect hash)
    class function UncompressedSize(const Comp: RawByteString): integer;
    /// returns the algorithm name, from its classname
    // - e.g. TAlgoSynLZ->'synlz' TAlgoLizard->'lizard' nil->'none'
    function AlgoName: TShort16;
  end;

  /// implement our fast SynLZ compression as a TAlgoCompress class
  // - please use the AlgoSynLZ global variable methods instead of the deprecated
  // SynLZCompress/SynLZDecompress wrapper functions
  TAlgoSynLZ = class(TAlgoCompress)
  public
    /// returns 1 as genuine byte identifier for SynLZ
    function AlgoID: byte; override;
    /// get maximum possible (worse) SynLZ compressed size for the supplied length
    function AlgoCompressDestLen(PlainLen: integer): integer; override;
    /// compress the supplied data using SynLZ
    function AlgoCompress(Plain: pointer; PlainLen: integer; Comp: pointer): integer; override;
    /// return the size of the SynLZ decompressed data
    function AlgoDecompressDestLen(Comp: pointer): integer; override;
    /// decompress the supplied data using SynLZ
    function AlgoDecompress(Comp: pointer; CompLen: integer; Plain: pointer): integer; override;
    /// partial (and safe) decompression of the supplied data using SynLZ
    function AlgoDecompressPartial(Comp: pointer; CompLen: integer;
      Partial: pointer; PartialLen, PartialLenMax: integer): integer; override;
  end;

  TAlgoCompressWithNoDestLenProcess = (doCompress, doUnCompress, doUncompressPartial);

  /// abstract class storing the plain length before calling compression API
  // - some libraries (e.g. Deflate or Lizard) don't provide the uncompressed
  // length from its output buffer - inherit from this class to store this value
  // as ToVarUInt32, and override the RawProcess abstract protected method
  TAlgoCompressWithNoDestLen = class(TAlgoCompress)
  protected
    /// inherited classes should implement this single method for the actual process
    // - dstMax is oinly used for doUncompressPartial
    function RawProcess(src, dst: pointer; srcLen, dstLen, dstMax: integer;
      process: TAlgoCompressWithNoDestLenProcess): integer; virtual; abstract;
  public
    /// performs the compression, storing PlainLen and calling protected RawProcess
    function AlgoCompress(Plain: pointer; PlainLen: integer; Comp: pointer): integer; override;
    /// return the size of the decompressed data (using FromVarUInt32)
    function AlgoDecompressDestLen(Comp: pointer): integer; override;
    /// performs the decompression, retrieving PlainLen and calling protected RawProcess
    function AlgoDecompress(Comp: pointer; CompLen: integer; Plain: pointer): integer; override;
    /// performs the decompression, retrieving PlainLen and calling protected RawProcess
    function AlgoDecompressPartial(Comp: pointer; CompLen: integer;
      Partial: pointer; PartialLen, PartialLenMax: integer): integer; override;
  end;

var
  /// acccess to our fast SynLZ compression as a TAlgoCompress class
  // - please use this global variable methods instead of the deprecated
  // SynLZCompress/SynLZDecompress wrapper functions
  AlgoSynLZ: TAlgoCompress;

const
  /// CompressionSizeTrigger parameter SYNLZTRIG[true] will disable then
  // SynLZCompress() compression
  SYNLZTRIG: array[boolean] of integer = (100, maxInt);
  /// used e.g. as when ALGO_SAFE[SafeDecompression] for TAlgoCompress.Decompress
  ALGO_SAFE: array[boolean] of TAlgoCompressLoad = (aclNormal, aclSafeSlow);



{ ************ Variable Length Integer Encoding / Decoding }

/// convert a cardinal into a 32-bit variable-length integer buffer
function ToVarUInt32(Value: cardinal; Dest: PByte): PByte;

/// return the number of bytes necessary to store a 32-bit variable-length integer
// - i.e. the ToVarUInt32() buffer size
function ToVarUInt32Length(Value: PtrUInt): PtrUInt;
  {$ifdef HASINLINE}inline;{$endif}

/// return the number of bytes necessary to store some data with a its
// 32-bit variable-length integer legnth
function ToVarUInt32LengthWithData(Value: PtrUInt): PtrUInt;
  {$ifdef HASINLINE}inline;{$endif}

/// convert an integer into a 32-bit variable-length integer buffer
// - store negative values as cardinal two-complement, i.e.
// 0=0,1=1,2=-1,3=2,4=-2...
function ToVarInt32(Value: PtrInt; Dest: PByte): PByte;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a 32-bit variable-length integer buffer into a cardinal
// - fast inlined process for any number < 128
// - use overloaded FromVarUInt32() or FromVarUInt32Safe() with a SourceMax
// pointer to avoid any potential buffer overflow
function FromVarUInt32(var Source: PByte): cardinal; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// safely convert a 32-bit variable-length integer buffer into a cardinal
// - slower but safer process checking out of boundaries memory access in Source
// - SourceMax is expected to be not nil, and to point to the first byte
// just after the Source memory buffer
// - returns nil on error, or point to next input data on successful decoding
function FromVarUInt32Safe(Source, SourceMax: PByte; out Value: cardinal): PByte;

/// convert a 32-bit variable-length integer buffer into a cardinal
// - will call FromVarUInt32() if SourceMax=nil, or FromVarUInt32Safe() if set
// - returns false on error, true if Value has been set properly
function FromVarUInt32(var Source: PByte; SourceMax: PByte; out Value: cardinal): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a 32-bit variable-length integer buffer into a cardinal
// - this version could be called if number is likely to be > $7f, so it
// inlining the first byte won't make any benefit
function FromVarUInt32Big(var Source: PByte): cardinal;

/// convert a 32-bit variable-length integer buffer into a cardinal
// - used e.g. when inlining FromVarUInt32()
// - this version must be called if Source^ has already been checked to be > $7f
// ! result := Source^;
// ! inc(Source);
// ! if result>$7f then
// !   result := (result and $7F) or FromVarUInt32Up128(Source);
function FromVarUInt32Up128(var Source: PByte): cardinal;

/// convert a 32-bit variable-length integer buffer into a cardinal
// - this version must be called if Source^ has already been checked to be > $7f
function FromVarUInt32High(var Source: PByte): cardinal;

/// convert a 32-bit variable-length integer buffer into an integer
// - decode negative values from cardinal two-complement, i.e.
// 0=0,1=1,2=-1,3=2,4=-2...
function FromVarInt32(var Source: PByte): integer;

/// convert a UInt64 into a 64-bit variable-length integer buffer
function ToVarUInt64(Value: QWord; Dest: PByte): PByte;

/// convert a 64-bit variable-length integer buffer into a UInt64
function FromVarUInt64(var Source: PByte): QWord; overload;

/// safely convert a 64-bit variable-length integer buffer into a UInt64
// - slower but safer process checking out of boundaries memory access in Source
// - SourceMax is expected to be not nil, and to point to the first byte
// just after the Source memory buffer
// - returns nil on error, or point to next input data on successful decoding
function FromVarUInt64Safe(Source, SourceMax: PByte; out Value: QWord): PByte;

/// convert a 64-bit variable-length integer buffer into a UInt64
// - will call FromVarUInt64() if SourceMax=nil, or FromVarUInt64Safe() if set
// - returns false on error, true if Value has been set properly
function FromVarUInt64(var Source: PByte; SourceMax: PByte; out Value: Qword): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a Int64 into a 64-bit variable-length integer buffer
function ToVarInt64(Value: Int64; Dest: PByte): PByte; {$ifdef HASINLINE}inline;{$endif}

/// convert a 64-bit variable-length integer buffer into a Int64
function FromVarInt64(var Source: PByte): Int64;

/// convert a 64-bit variable-length integer buffer into a Int64
// - this version won't update the Source pointer
function FromVarInt64Value(Source: PByte): Int64;

/// jump a value in the 32-bit or 64-bit variable-length integer buffer
function GotoNextVarInt(Source: PByte): pointer; {$ifdef HASINLINE}inline;{$endif}

/// convert a RawUTF8 into an UTF-8 encoded variable-length buffer
function ToVarString(const Value: RawUTF8; Dest: PByte): PByte;

/// jump a value in variable-length text buffer
function GotoNextVarString(Source: PByte): pointer; {$ifdef HASINLINE}inline;{$endif}

/// retrieve a variable-length UTF-8 encoded text buffer in a newly allocation RawUTF8
function FromVarString(var Source: PByte): RawUTF8; overload;

/// safe retrieve a variable-length UTF-8 encoded text buffer in a newly allocation RawUTF8
// - supplied SourceMax value will avoid any potential buffer overflow
function FromVarString(var Source: PByte; SourceMax: PByte): RawUTF8; overload;

/// retrieve a variable-length text buffer
// - this overloaded function will set the supplied code page to the AnsiString
procedure FromVarString(var Source: PByte; var Value: RawByteString;
  CodePage: integer); overload;

/// retrieve a variable-length text buffer
// - this overloaded function will set the supplied code page to the AnsiString
// and will also check for the SourceMax end of buffer
// - returns TRUE on success, or FALSE on any buffer overload detection
function FromVarString(var Source: PByte; SourceMax: PByte;
  var Value: RawByteString; CodePage: integer): boolean; overload;

/// retrieve a variable-length UTF-8 encoded text buffer in a temporary buffer
// - caller should call Value.Done after use of the Value.buf memory
// - this overloaded function would include a trailing #0, so Value.buf could
// be parsed as a valid PUTF8Char buffer (e.g. containing JSON)
procedure FromVarString(var Source: PByte; var Value: TSynTempBuffer); overload;

/// retrieve a variable-length UTF-8 encoded text buffer in a temporary buffer
// - caller should call Value.Done after use of the Value.buf memory
// - this overloaded function will also check for the SourceMax end of buffer,
// returning TRUE on success, or FALSE on any buffer overload detection
function FromVarString(var Source: PByte; SourceMax: PByte;
  var Value: TSynTempBuffer): boolean; overload;

type
  /// kind of result returned by FromVarBlob() function
  TValueResult = record
    /// start of data value
    Ptr: PAnsiChar;
    /// value length (in bytes)
    Len: PtrInt;
  end;

/// retrieve pointer and length to a variable-length text/blob buffer
function FromVarBlob(Data: PByte): TValueResult; {$ifdef HASINLINE}inline;{$endif}


{ ****************** TFastReader / TBufferWriter Binary Streams }

type
  /// exception raised during TFastReader decoding
  EFastReader = class(ESynException);

  /// safe decoding of a TBufferWriter content
  // - similar to TFileBufferReader, but faster and only for in-memory buffer
  // - it is also safer, since will check for reaching end of buffer
  // - raise a EFastReader exception on decoding error (e.g. if a buffer
  // overflow may occur) or call OnErrorOverflow/OnErrorData event handlers
  {$ifdef USERECORDWITHMETHODS}
  TFastReader = record {$else}TFastReader = object {$endif}
  public
    /// the current position in the memory
    P: PAnsiChar;
    /// the last position in the buffer
    Last: PAnsiChar;
    /// use this event to customize the ErrorOverflow process
    OnErrorOverflow: procedure of object;
    /// use this event to customize the ErrorData process
    OnErrorData: procedure(const fmt: RawUTF8; const args: array of const) of object;
    /// some opaque value, which may be a version number to define the binary layout
    Tag: PtrInt;
    /// initialize the reader from a memory block
    procedure Init(Buffer: pointer; Len: integer); overload;
    /// initialize the reader from a RawByteString content
    procedure Init(const Buffer: RawByteString); overload;
    /// raise a EFastReader with an "overflow" error message
    procedure ErrorOverflow;
    /// raise a EFastReader with an "incorrect data" error message
    procedure ErrorData(const fmt: RawUTF8; const args: array of const);
    /// read the next 32-bit signed value from the buffer
    function VarInt32: integer;    {$ifdef HASINLINE}inline;{$endif}
    /// read the next 32-bit unsigned value from the buffer
    function VarUInt32: cardinal;
    /// try to read the next 32-bit signed value from the buffer
    // - don't change the current position
    function PeekVarInt32(out value: PtrInt): boolean; {$ifdef HASINLINE}inline;{$endif}
    /// try to read the next 32-bit unsigned value from the buffer
    // - don't change the current position
    function PeekVarUInt32(out value: PtrUInt): boolean;
    /// read the next 32-bit unsigned value from the buffer
    // - this version won't call ErrorOverflow, but return false on error
    // - returns true on read success
    function VarUInt32Safe(out Value: cardinal): boolean;
    /// read the next 64-bit signed value from the buffer
    function VarInt64: Int64; {$ifdef HASINLINE}inline;{$endif}
    /// read the next 64-bit unsigned value from the buffer
    function VarUInt64: QWord;
    /// read the next RawUTF8 value from the buffer
    function VarUTF8: RawUTF8; overload; {$ifdef HASINLINE}inline;{$endif}
    /// read the next RawUTF8 value from the buffer
    procedure VarUTF8(out result: RawUTF8); overload;
    /// read the next RawUTF8 value from the buffer
    // - this version won't call ErrorOverflow, but return false on error
    // - returns true on read success
    function VarUTF8Safe(out Value: RawUTF8): boolean;
    /// read the next RawByteString value from the buffer
    function VarString: RawByteString; {$ifdef HASINLINE}inline;{$endif}
    /// read the next pointer and length value from the buffer
    procedure VarBlob(out result: TValueResult); overload; {$ifdef HASINLINE} inline;{$endif}
    /// read the next pointer and length value from the buffer
    function VarBlob: TValueResult; overload;  {$ifdef HASINLINE} inline;{$endif}
    /// copy the next VarBlob value from the buffer into a TSynTempBuffer
    procedure VarBlob(out Value: TSynTempBuffer); overload;
    /// read the next ShortString value from the buffer
    function VarShortString: shortstring; {$ifdef HASINLINE}inline;{$endif}
    /// fast ignore the next VarUInt32/VarInt32/VarUInt64/VarInt64 value
    // - don't raise any exception, so caller could check explicitly for any EOF
    procedure VarNextInt; overload; {$ifdef HASINLINE} inline;{$endif}
    /// fast ignore the next count VarUInt32/VarInt32/VarUInt64/VarInt64 values
    // - don't raise any exception, so caller could check explicitly for any EOF
    procedure VarNextInt(count: integer); overload;
    /// read the next byte from the buffer
    function NextByte: byte; {$ifdef HASINLINE}inline;{$endif}
    /// read the next byte from the buffer, checking
    function NextByteSafe(dest: pointer): boolean; {$ifdef HASINLINE}inline;{$endif}
    /// read the next 4 bytes from the buffer as a 32-bit unsigned value
    function Next4: cardinal; {$ifdef HASINLINE}inline;{$endif}
    /// read the next 8 bytes from the buffer as a 64-bit unsigned value
    function Next8: Qword; {$ifdef HASINLINE}inline;{$endif}
    /// consumes the next byte from the buffer, if matches a given value
    function NextByteEquals(Value: byte): boolean; {$ifdef HASINLINE}inline;{$endif}
    /// returns the current position, and move ahead the specified bytes
    function Next(DataLen: PtrInt): pointer;   {$ifdef HASINLINE}inline;{$endif}
    /// returns the current position, and move ahead the specified bytes
    function NextSafe(out Data: Pointer; DataLen: PtrInt): boolean; {$ifdef HASINLINE}inline;{$endif}
    /// copy data from the current position, and move ahead the specified bytes
    procedure Copy(Dest: Pointer; DataLen: PtrInt); {$ifdef HASINLINE}inline;{$endif}
    /// copy data from the current position, and move ahead the specified bytes
    // - this version won't call ErrorOverflow, but return false on error
    // - returns true on read success
    function CopySafe(Dest: Pointer; DataLen: PtrInt): boolean;
    /// retrieved cardinal values encoded with TBufferWriter.WriteVarUInt32Array
    // - only supports wkUInt32, wkVarInt32, wkVarUInt32 kind of encoding
    function ReadVarUInt32Array(var Values: TIntegerDynArray): PtrInt;
    /// retrieve some TAlgoCompress buffer, appended via Write()
    // - BufferOffset could be set to reserve some bytes before the uncompressed buffer
    function ReadCompressed(Load: TAlgoCompressLoad = aclNormal;
      BufferOffset: integer = 0): RawByteString;
    /// returns TRUE if the current position is the end of the input stream
    function EOF: boolean; {$ifdef HASINLINE}inline;{$endif}
    /// returns remaining length (difference between Last and P)
    function RemainingLength: PtrUInt; {$ifdef HASINLINE}inline;{$endif}
  end;

  /// available kind of integer array storage, corresponding to the data layout
  // of TBufferWriter
  // - wkUInt32 will write the content as "plain" 4 bytes binary (this is the
  // prefered way if the integers can be negative)
  // - wkVarUInt32 will write the content using our 32-bit variable-length integer
  // encoding
  // - wkVarInt32 will write the content using our 32-bit variable-length integer
  // encoding and the by-two complement (0=0,1=1,2=-1,3=2,4=-2...)
  // - wkSorted will write an increasing array of integers, handling the special
  // case of a difference of similar value (e.g. 1) between two values - note
  // that this encoding is efficient only if the difference is mainly < 253
  // - wkOffsetU and wkOffsetI will write the difference between two successive
  // values, with detection of any constant difference (Unsigned or Integer)
  // - wkFakeMarker won't be used by WriteVarUInt32Array, but to notify a
  // custom encoding
  TBufferWriterKind = (wkUInt32, wkVarUInt32, wkVarInt32, wkSorted,
    wkOffsetU, wkOffsetI, wkFakeMarker);

  /// this class can be used to speed up writing to a file or a stream
  // - big speed up if data is written in small blocks
  // - also handle optimized storage of any Integer/Int64/RawUTF8 values
  // - use TFileBufferReader or TFastReader for decoding of the stored binary
  TBufferWriter = class
  private
    fPos: PtrInt;
    fBufLen, fBufLen16: PtrInt;
    fBuffer: PByteArray;
    fStream: TStream;
    fTotalFlushed: Int64;
    fBufferInternal: pointer;
    fInternalStream: boolean;
    fTag: PtrInt;
    procedure InternalFlush;
    function GetTotalWritten: Int64; {$ifdef HASINLINE} inline; {$endif}
    procedure FlushAndWrite(Data: pointer; DataLen: PtrInt);
  public
    /// initialize the buffer, and specify a file handle to use for writing
    // - define an internal buffer of the specified size
    constructor Create(aFile: THandle; BufLen: integer = 65536); overload;
    /// initialize the buffer, and specify a TStream to use for writing
    // - define an internal buffer of the specified size
    constructor Create(aStream: TStream; BufLen: integer = 65536); overload;
    /// initialize the buffer, and specify a file to use for writing
    // - define an internal buffer of the specified size
    // - would replace any existing file by default, unless Append is TRUE
    constructor Create(const aFileName: TFileName; BufLen: integer = 65536;
      Append: boolean = false); overload;
    /// initialize with a specified buffer and an existing TStream instance
    // - use a specified external buffer (which may be allocated on stack),
    // to avoid a memory allocation
    constructor Create(aStream: TStream; aTempBuf: pointer; aTempLen: integer); overload;
    /// initialize the buffer, using an owned TStream instance
    // - parameter could be e.g. TMemoryStream or TRawByteStringStream
    // - use Flush then TMemoryStream(Stream) to retrieve its content, or
    // FlushTo if TRawByteStringStream was used
    constructor Create(aClass: TStreamClass; BufLen: integer = 4096); overload;
    /// initialize with a specified buffer and an owned TStream
    // - use a specified external buffer (which may be allocated on stack),
    // to avoid a memory allocation
    // - aClass could be e.g. TMemoryStream or TRawByteStringStream
    constructor Create(aClass: TStreamClass; aTempBuf: pointer; aTempLen: integer); overload;
    /// initialize with a stack-allocated 4KB of buffer
    // - destination stream is an owned TRawByteStringStream - so you can
    // call FlushTo to retrieve all written data
    // - convenient to reduce heap presure, when writing a few KB of data
    constructor Create(const aStackBuffer: TSynTempBuffer); overload;
    /// release internal TStream (after AssignToHandle call)
    // - warning: an explicit call to Flush is needed to write the data pending
    // in internal buffer
    destructor Destroy; override;
    /// append 1 byte of data at the current position
    procedure Write1(Data: Byte); {$ifdef HASINLINE}inline;{$endif}
    /// append 2 bytes of data at the current position
    procedure Write2(Data: cardinal); {$ifdef HASINLINE}inline;{$endif}
    /// append 4 bytes of data at the current position
    procedure Write4(Data: integer); {$ifdef HASINLINE}inline;{$endif}
    /// append 4 bytes of data, encoded as BigEndian, at the current position
    procedure Write4BigEndian(Data: integer); {$ifdef HASINLINE}inline;{$endif}
    /// append 8 bytes of data at the current position
    procedure Write8(Data8Bytes: pointer); {$ifdef HASINLINE}inline;{$endif}
    /// append 8 bytes of 64-bit integer at the current position
    procedure WriteI64(Data: Int64); {$ifdef HASINLINE}inline;{$endif}
    /// append the same byte a given number of occurences at the current position
    procedure WriteN(Data: Byte; Count: integer);
    /// append some content (may be text or binary) prefixed by its encoded length
    // - will write DataLen as VarUInt32, then the Data content, as expected
    // by FromVarString/FromVarBlob functions
    procedure WriteVar(Data: pointer; DataLen: PtrInt);
    /// append some UTF-8 encoded text at the current position
    // - will write the string length (as VarUInt32), then the string content
    // - is just a wrapper around WriteVar()
    procedure WriteShort(const Text: ShortString); {$ifdef HASINLINE} inline;{$endif}
    /// append some length-prefixed UTF-8 text at the current position
    // - will write the string length (as VarUInt32), then the string content, as expected
    // by the FromVarString() function
    // - is just a wrapper around WriteVar()
    procedure Write(const Text: RawByteString); overload; {$ifdef HASINLINE} inline;{$endif}
    /// append some data at the current position
    // - will be inlined as a MoveFast() most of the time
    procedure Write(Data: pointer; DataLen: PtrInt); overload; {$ifdef HASINLINE} inline;{$endif}
    /// append some content at the current position
    // - will write the binary data, without any length prefix
    procedure WriteBinary(const Data: RawByteString);
    /// append "New[0..Len-1] xor Old[0..Len-1]" bytes
    // - as used e.g. by ZeroCompressXor/TSynBloomFilterDiff.SaveTo
    procedure WriteXor(New, Old: PAnsiChar; Len: PtrInt; crc: PCardinal = nil);
    /// append a cardinal value using 32-bit variable-length integer encoding
    procedure WriteVarUInt32(Value: PtrUInt);
    /// append an integer value using 32-bit variable-length integer encoding of
    // the by-two complement of the given value
    procedure WriteVarInt32(Value: PtrInt);
    /// append an integer value using 64-bit variable-length integer encoding of
    // the by-two complement of the given value
    procedure WriteVarInt64(Value: Int64);
    /// append an unsigned integer value using 64-bit variable-length encoding
    procedure WriteVarUInt64(Value: QWord);
    /// append cardinal values (NONE must be negative!) using 32-bit
    // variable-length integer encoding or other specialized algorithm,
    // depending on the data layout
    procedure WriteVarUInt32Array(const Values: TIntegerDynArray; ValuesCount: integer;
      DataLayout: TBufferWriterKind);
    /// append cardinal values (NONE must be negative!) using 32-bit
    // variable-length integer encoding or other specialized algorithm,
    // depending on the data layout
    procedure WriteVarUInt32Values(Values: PIntegerArray; ValuesCount: integer;
      DataLayout: TBufferWriterKind);
    /// append UInt64 values using 64-bit variable length integer encoding
    // - if Offset is TRUE, then it will store the difference between
    // two values using 64-bit variable-length integer encoding (in this case,
    // a fixed-sized record storage is also handled separately)
    // - could be decoded later on via TFileBufferReader.ReadVarUInt64Array
    procedure WriteVarUInt64DynArray(const Values: TInt64DynArray; ValuesCount: integer;
      Offset: Boolean);
    /// append the RawUTF8 dynamic array
    // - handled the fixed size strings array case in a very efficient way
    procedure WriteRawUTF8DynArray(const Values: TRawUTF8DynArray; ValuesCount: integer);
    /// append a RawUTF8 array of values, from its low-level memory pointer
    // - handled the fixed size strings array case in a very efficient way
    procedure WriteRawUTF8Array(Values: PPtrUIntArray; ValuesCount: integer);
    /// append a TStream content
    // - is StreamSize is left as -1, the Stream.Size is used
    // - the size of the content is stored in the resulting stream
    procedure WriteStream(aStream: TCustomMemoryStream; aStreamSize: Integer = -1);
    /// allows to write directly to a memory buffer
    // - caller should specify the maximum possible number of bytes to be written
    // - then write the data to the returned pointer, and call DirectWriteFlush
    // - if len is bigger than the internal buffer, tmp will be used instead
    function DirectWritePrepare(len: PtrInt; var tmp: RawByteString): PAnsiChar;
    /// finalize a direct write to a memory buffer
    // - by specifying the number of bytes written to the buffer
    procedure DirectWriteFlush(len: PtrInt; const tmp: RawByteString);
    /// write any pending data in the internal buffer to the stream
    // - after a Flush, it's possible to call FileSeek64(aFile,....)
    // - returns the number of bytes written between two FLush method calls
    function Flush: Int64;
    /// write any pending data, then create a RawByteString from the content
    // - raise an exception if internal Stream is not a TRawByteStringStream
    function FlushTo: RawByteString;
    /// write any pending data, then call algo.Compress() on the buffer
    // - if algo is left to its default nil, will use global AlgoSynLZ
    // - features direct compression from internal buffer, if stream was not used
    // - BufferOffset could be set to reserve some bytes before the compressed buffer
    // - raise an exception if internal Stream is not a TRawByteStringStream
    function FlushAndCompress(nocompression: boolean = false; algo: TAlgoCompress = nil;
      BufferOffset: integer = 0): RawByteString;
    /// rewind the Stream to the position when Create() was called
    // - note that this does not clear the Stream content itself, just
    // move back its writing position to its initial place
    procedure CancelAll; virtual;
    /// the associated writing stream
    property Stream: TStream read fStream;
    /// get the byte count written since last Flush
    property TotalWritten: Int64 read GetTotalWritten;
    /// simple property used to store some integer content
    property Tag: PtrInt read fTag write fTag;
  end;

  /// deprecated name of TBufferWriter binary serializer
  TFileBufferWriter = TBufferWriter;
  TFileBufferWriterKind = TBufferWriterKind;


{ ************ Base64, Base64URI, URL and Baudot Encoding / Decoding }

const
  /// UTF-8 encoded \uFFF0 special code to mark Base64 binary content in JSON
  // - Unicode special char U+FFF0 is UTF-8 encoded as EF BF B0 bytes
  // - as generated by BinToBase64WithMagic() functions, and expected by
  // SQLParamContent() and ExtractInlineParameters() functions
  // - used e.g. when transmitting TDynArray.SaveTo() content
  JSON_BASE64_MAGIC = $b0bfef;

  /// '"' + UTF-8 encoded \uFFF0 special code to mark Base64 binary in JSON
  JSON_BASE64_MAGIC_QUOTE = ord('"')+cardinal(JSON_BASE64_MAGIC) shl 8;

  /// '"' + UTF-8 encoded \uFFF0 special code to mark Base64 binary in JSON
  // - defined as a cardinal variable to be used as:
  // ! AddNoJSONEscape(@JSON_BASE64_MAGIC_QUOTE_VAR,4);
  JSON_BASE64_MAGIC_QUOTE_VAR: cardinal = JSON_BASE64_MAGIC_QUOTE;

/// just a wrapper around Base64ToBin() for in-place decode of JSON_BASE64_MAGIC
// '\uFFF0base64encodedbinary' content into binary
// - input ParamValue shall have been checked to match the expected pattern
procedure Base64MagicDecode(var ParamValue: RawUTF8);

/// check and decode '\uFFF0base64encodedbinary' content into binary
// - this method will check the supplied value to match the expected
// JSON_BASE64_MAGIC pattern, decode and set Blob and return TRUE
function Base64MagicCheckAndDecode(Value: PUTF8Char; var Blob: RawByteString): boolean; overload;

/// check and decode '\uFFF0base64encodedbinary' content into binary
// - this method will check the supplied value to match the expected
// JSON_BASE64_MAGIC pattern, decode and set Blob and return TRUE
function Base64MagicCheckAndDecode(Value: PUTF8Char; ValueLen: Integer;
  var Blob: RawByteString): boolean; overload;

/// check and decode '\uFFF0base64encodedbinary' content into binary
// - this method will check the supplied value to match the expected
// JSON_BASE64_MAGIC pattern, decode and set Blob and return TRUE
function Base64MagicCheckAndDecode(Value: PUTF8Char; var Blob: TSynTempBuffer): boolean; overload;

/// fast conversion from binary data into Base64 encoded UTF-8 text
function BinToBase64(const s: RawByteString): RawUTF8; overload;

/// fast conversion from binary data into Base64 encoded UTF-8 text
function BinToBase64(Bin: PAnsiChar; BinBytes: integer): RawUTF8; overload;

/// fast conversion from a small binary data into Base64 encoded UTF-8 text
function BinToBase64Short(const s: RawByteString): shortstring; overload;

/// fast conversion from a small binary data into Base64 encoded UTF-8 text
function BinToBase64Short(Bin: PAnsiChar; BinBytes: integer): shortstring; overload;

/// fast conversion from binary data into prefixed/suffixed Base64 encoded UTF-8 text
// - with optional JSON_BASE64_MAGIC prefix (UTF-8 encoded \uFFF0 special code)
function BinToBase64(const data, Prefix, Suffix: RawByteString; WithMagic: boolean): RawUTF8; overload;

/// fast conversion from binary data into Base64 encoded UTF-8 text
// with JSON_BASE64_MAGIC prefix (UTF-8 encoded \uFFF0 special code)
function BinToBase64WithMagic(const data: RawByteString): RawUTF8; overload;

/// fast conversion from binary data into Base64 encoded UTF-8 text
// with JSON_BASE64_MAGIC prefix (UTF-8 encoded \uFFF0 special code)
function BinToBase64WithMagic(Data: pointer; DataLen: integer): RawUTF8; overload;

/// raw function for efficient binary to Base64 encoding of the main block
// - don't use this function, but rather the BinToBase64() overloaded functions
function Base64EncodeMain(rp, sp: PAnsiChar; len: cardinal): integer;

/// raw function for efficient binary to Base64 encoding of the last bytes
// - don't use this function, but rather the BinToBase64() overloaded functions
procedure Base64EncodeTrailing(rp, sp: PAnsiChar; len: cardinal);
  {$ifdef FPC}inline;{$endif}

/// fast conversion from Base64 encoded text into binary data
// - is now just an alias to Base64ToBinSafe() overloaded function
// - returns '' if s was not a valid Base64-encoded input
function Base64ToBin(const s: RawByteString): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from Base64 encoded text into binary data
// - is now just an alias to Base64ToBinSafe() overloaded function
// - returns '' if sp/len buffer was not a valid Base64-encoded input
function Base64ToBin(sp: PAnsiChar; len: PtrInt): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from Base64 encoded text into binary data
// - is now just an alias to Base64ToBinSafe() overloaded function
// - returns false and data='' if sp/len buffer was invalid
function Base64ToBin(sp: PAnsiChar; len: PtrInt; var data: RawByteString): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from Base64 encoded text into binary data
// - returns TRUE on success, FALSE if sp/len buffer was invvalid
function Base64ToBin(sp: PAnsiChar; len: PtrInt; var Blob: TSynTempBuffer): boolean; overload;

/// fast conversion from Base64 encoded text into binary data
// - returns TRUE on success, FALSE if base64 does not match binlen
// - nofullcheck is deprecated and not used any more, since nofullcheck=false
// is now processed with no performance cost
function Base64ToBin(base64, bin: PAnsiChar; base64len, binlen: PtrInt;
  nofullcheck: boolean=true): boolean; overload;

/// fast conversion from Base64 encoded text into binary data
// - returns TRUE on success, FALSE if base64 does not match binlen
// - nofullcheck is deprecated and not used any more, since nofullcheck=false
// is now processed with no performance cost
function Base64ToBin(const base64: RawByteString; bin: PAnsiChar; binlen: PtrInt;
  nofullcheck: boolean=true): boolean; overload;

/// fast conversion from Base64 encoded text into binary data
// - will check supplied text is a valid Base64 encoded stream
function Base64ToBinSafe(const s: RawByteString): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from Base64 encoded text into binary data
// - will check supplied text is a valid Base64 encoded stream
function Base64ToBinSafe(sp: PAnsiChar; len: PtrInt): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from Base64 encoded text into binary data
// - will check supplied text is a valid Base64 encoded stream
function Base64ToBinSafe(sp: PAnsiChar; len: PtrInt; var data: RawByteString): boolean; overload;

/// check if the supplied text is a valid Base64 encoded stream
function IsBase64(const s: RawByteString): boolean; overload;

/// check if the supplied text is a valid Base64 encoded stream
function IsBase64(sp: PAnsiChar; len: PtrInt): boolean; overload;

/// retrieve the expected encoded length after Base64 process
function BinToBase64Length(len: PtrUInt): PtrUInt;
  {$ifdef HASINLINE}inline;{$endif}

/// retrieve the expected undecoded length of a Base64 encoded buffer
// - here len is the number of bytes in sp
function Base64ToBinLength(sp: PAnsiChar; len: PtrInt): PtrInt;

/// retrieve the expected undecoded length of a Base64 encoded buffer
// - here len is the number of bytes in sp
// - will check supplied text is a valid Base64 encoded stream
function Base64ToBinLengthSafe(sp: PAnsiChar; len: PtrInt): PtrInt;

/// direct low-level decoding of a Base64 encoded buffer
// - here len is the number of 4 chars chunks in sp input
// - deprecated low-level function: use Base64ToBin/Base64ToBinSafe instead
function Base64Decode(sp,rp: PAnsiChar; len: PtrInt): boolean;

/// fast conversion from binary data into Base64-like URI-compatible encoded text
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
function BinToBase64uri(const s: RawByteString): RawUTF8; overload;

/// fast conversion from a binary buffer into Base64-like URI-compatible encoded text
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
function BinToBase64uri(Bin: PAnsiChar; BinBytes: integer): RawUTF8; overload;

/// fast conversion from a binary buffer into Base64-like URI-compatible encoded shortstring
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
// - returns '' if BinBytes void or too big for the resulting shortstring
function BinToBase64uriShort(Bin: PAnsiChar; BinBytes: integer): shortstring;

/// conversion from any Base64 encoded value into URI-compatible encoded text
// - warning: will modify the supplied base64 string in-place
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
procedure Base64ToURI(var base64: RawUTF8);

/// low-level conversion from a binary buffer into Base64-like URI-compatible encoded text
// - you should rather use the overloaded BinToBase64uri() functions
procedure Base64uriEncode(rp, sp: PAnsiChar; len: cardinal);

/// retrieve the expected encoded length after Base64-URI process
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
function BinToBase64uriLength(len: PtrUInt): PtrUInt;
  {$ifdef HASINLINE}inline;{$endif}

/// retrieve the expected undecoded length of a Base64-URI encoded buffer
// - here len is the number of bytes in sp
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
function Base64uriToBinLength(len: PtrInt): PtrInt;

/// fast conversion from Base64-URI encoded text into binary data
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
function Base64uriToBin(sp: PAnsiChar; len: PtrInt): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from Base64-URI encoded text into binary data
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
procedure Base64uriToBin(sp: PAnsiChar; len: PtrInt; var result: RawByteString); overload;

/// fast conversion from Base64-URI encoded text into binary data
// - caller should always execute temp.Done when finished with the data
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
function Base64uriToBin(sp: PAnsiChar; len: PtrInt; var temp: TSynTempBuffer): boolean; overload;

/// fast conversion from Base64-URI encoded text into binary data
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
function Base64uriToBin(const s: RawByteString): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from Base64-URI encoded text into binary data
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
// - will check supplied text is a valid Base64-URI encoded stream
function Base64uriToBin(base64, bin: PAnsiChar; base64len, binlen: PtrInt): boolean; overload;

/// fast conversion from Base64-URI encoded text into binary data
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
// - will check supplied text is a valid Base64-URI encoded stream
function Base64uriToBin(const base64: RawByteString;
  bin: PAnsiChar; binlen: PtrInt): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// direct low-level decoding of a Base64-URI encoded buffer
// - the buffer is expected to be at least Base64uriToBinLength() bytes long
// - returns true if the supplied sp[] buffer has been successfully decoded
// into rp[] - will break at any invalid character, so is always safe to use
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
// - you should better not use this, but Base64uriToBin() overloaded functions
function Base64uriDecode(sp, rp: PAnsiChar; len: PtrInt): boolean;

/// convert some ASCII-7 text into binary, using Emile Baudot code
// - as used in telegraphs, covering #10 #13 #32 a-z 0-9 - ' , ! : ( + ) $ ? @ . / ;
// charset, following a custom static-huffman-like encoding with 5-bit masks
// - any upper case char will be converted into lowercase during encoding
// - other characters (e.g. UTF-8 accents, or controls chars) will be ignored
// - resulting binary will consume 5 (or 10) bits per character
// - reverse of the BaudotToAscii() function
// - the "baud" symbol rate measurement comes from Emile's name ;)
function AsciiToBaudot(P: PAnsiChar; len: PtrInt): RawByteString; overload;

/// convert some ASCII-7 text into binary, using Emile Baudot code
// - as used in telegraphs, covering #10 #13 #32 a-z 0-9 - ' , ! : ( + ) $ ? @ . / ;
// charset, following a custom static-huffman-like encoding with 5-bit masks
// - any upper case char will be converted into lowercase during encoding
// - other characters (e.g. UTF-8 accents, or controls chars) will be ignored
// - resulting binary will consume 5 (or 10) bits per character
// - reverse of the BaudotToAscii() function
// - the "baud" symbol rate measurement comes from Emile's name ;)
function AsciiToBaudot(const Text: RawUTF8): RawByteString; overload;

/// convert some Baudot code binary, into ASCII-7 text
// - reverse of the AsciiToBaudot() function
// - any uppercase character would be decoded as lowercase - and some characters
// may have disapeared
// - the "baud" symbol rate measurement comes from Emile's name ;)
function BaudotToAscii(Baudot: PByteArray; len: PtrInt): RawUTF8; overload;

/// convert some Baudot code binary, into ASCII-7 text
// - reverse of the AsciiToBaudot() function
// - any uppercase character would be decoded as lowercase - and some characters
// may have disapeared
// - the "baud" symbol rate measurement comes from Emile's name ;)
function BaudotToAscii(const Baudot: RawByteString): RawUTF8; overload;


/// encode a string to be compatible with URI encoding
function UrlEncode(const svar: RawUTF8): RawUTF8; overload;

/// encode a string to be compatible with URI encoding
function UrlEncode(Text: PUTF8Char): RawUTF8; overload;

/// encode supplied parameters to be compatible with URI encoding
// - parameters must be supplied two by two, as Name,Value pairs, e.g.
// ! url := UrlEncode(['select','*','where','ID=12','offset',23,'object',aObject]);
// - parameters names should be plain ASCII-7 RFC compatible identifiers
// (0..9a..zA..Z_.~), otherwise their values are skipped
// - parameters values can be either textual, integer or extended, or any TObject
// - TObject serialization into UTF-8 will be processed by the ObjectToJSON()
// function
function UrlEncode(const NameValuePairs: array of const): RawUTF8; overload;

/// decode a string compatible with URI encoding into its original value
// - you can specify the decoding range (as in copy(s,i,len) function)
function UrlDecode(const s: RawUTF8; i: PtrInt = 1; len: PtrInt = -1): RawUTF8; overload;

/// decode a string compatible with URI encoding into its original value
function UrlDecode(U: PUTF8Char): RawUTF8; overload;

/// decode a specified parameter compatible with URI encoding into its original
// textual value
// - UrlDecodeValue('select=%2A&where=LastName%3D%27M%C3%B4net%27','SELECT=',V,@Next)
// will return Next^='where=...' and V='*'
// - if Upper is not found, Value is not modified, and result is FALSE
// - if Upper is found, Value is modified with the supplied content, and result is TRUE
function UrlDecodeValue(U: PUTF8Char; const Upper: RawUTF8; var Value: RawUTF8;
  Next: PPUTF8Char = nil): boolean;

/// decode a specified parameter compatible with URI encoding into its original
// integer numerical value
// - UrlDecodeInteger('offset=20&where=LastName%3D%27M%C3%B4net%27','OFFSET=',O,@Next)
// will return Next^='where=...' and O=20
// - if Upper is not found, Value is not modified, and result is FALSE
// - if Upper is found, Value is modified with the supplied content, and result is TRUE
function UrlDecodeInteger(U: PUTF8Char; const Upper: RawUTF8; var Value: integer;
  Next: PPUTF8Char = nil): boolean;

/// decode a specified parameter compatible with URI encoding into its original
// cardinal numerical value
// - UrlDecodeCardinal('offset=20&where=LastName%3D%27M%C3%B4net%27','OFFSET=',O,@Next)
// will return Next^='where=...' and O=20
// - if Upper is not found, Value is not modified, and result is FALSE
// - if Upper is found, Value is modified with the supplied content, and result is TRUE
function UrlDecodeCardinal(U: PUTF8Char; const Upper: RawUTF8; var Value: Cardinal;
  Next: PPUTF8Char = nil): boolean;

/// decode a specified parameter compatible with URI encoding into its original
// Int64 numerical value
// - UrlDecodeInt64('offset=20&where=LastName%3D%27M%C3%B4net%27','OFFSET=',O,@Next)
// will return Next^='where=...' and O=20
// - if Upper is not found, Value is not modified, and result is FALSE
// - if Upper is found, Value is modified with the supplied content, and result is TRUE
function UrlDecodeInt64(U: PUTF8Char; const Upper: RawUTF8; var Value: Int64;
  Next: PPUTF8Char = nil): boolean;

/// decode a specified parameter compatible with URI encoding into its original
// floating-point value
// - UrlDecodeExtended('price=20.45&where=LastName%3D%27M%C3%B4net%27','PRICE=',P,@Next)
// will return Next^='where=...' and P=20.45
// - if Upper is not found, Value is not modified, and result is FALSE
// - if Upper is found, Value is modified with the supplied content, and result is TRUE
function UrlDecodeExtended(U: PUTF8Char; const Upper: RawUTF8; var Value: TSynExtended;
  Next: PPUTF8Char = nil): boolean;

/// decode a specified parameter compatible with URI encoding into its original
// floating-point value
// - UrlDecodeDouble('price=20.45&where=LastName%3D%27M%C3%B4net%27','PRICE=',P,@Next)
// will return Next^='where=...' and P=20.45
// - if Upper is not found, Value is not modified, and result is FALSE
// - if Upper is found, Value is modified with the supplied content, and result is TRUE
function UrlDecodeDouble(U: PUTF8Char; const Upper: RawUTF8; var Value: double;
  Next: PPUTF8Char = nil): boolean;

/// returns TRUE if all supplied parameters do exist in the URI encoded text
// - CSVNames parameter shall provide as a CSV list of names
// - e.g. UrlDecodeNeedParameters('price=20.45&where=LastName%3D','price,where')
// will return TRUE
function UrlDecodeNeedParameters(U, CSVNames: PUTF8Char): boolean;

/// decode the next Name=Value&.... pair from input URI
// - Name is returned directly (should be plain ASCII 7 bit text)
// - Value is returned after URI decoding (from %.. patterns)
// - if a pair is decoded, return a PUTF8Char pointer to the next pair in
// the input buffer, or points to #0 if all content has been processed
// - if a pair is not decoded, return nil
function UrlDecodeNextNameValue(U: PUTF8Char; var Name,Value: RawUTF8): PUTF8Char;

/// decode a URI-encoded Value from an input buffer
// - decoded value is set in Value out variable
// - returns a pointer just after the decoded value (may points e.g. to
// #0 or '&') - it is up to the caller to continue the process or not
function UrlDecodeNextValue(U: PUTF8Char; out Value: RawUTF8): PUTF8Char;

/// decode a URI-encoded Name from an input buffer
// - decoded value is set in Name out variable
// - returns a pointer just after the decoded name, after the '='
// - returns nil if there was no name=... pattern in U
function UrlDecodeNextName(U: PUTF8Char; out Name: RawUTF8): PUTF8Char;

/// checks if the supplied UTF-8 text don't need URI encoding
// - returns TRUE if all its chars are non-void plain ASCII-7 RFC compatible
// identifiers (0..9a..zA..Z-_.~)
function IsUrlValid(P: PUTF8Char): boolean;

/// checks if the supplied UTF-8 text values don't need URI encoding
// - returns TRUE if all its chars of all strings are non-void plain ASCII-7 RFC
// compatible identifiers (0..9a..zA..Z-_.~)
function AreUrlValid(const Url: array of RawUTF8): boolean;

/// ensure the supplied URI contains a trailing '/' charater
function IncludeTrailingURIDelimiter(const URI: RawByteString): RawByteString;



{ ********** RTTI Values Binary Serialization and Comparison }

type
  /// internal function handler for binary persistence of any RTTI type value
  // - i.e. the kind of functions called via _BINARYSAVE[] lookup table
  // - work with managed and unmanaged types
  // - persist Data^ into Dest, returning the size in Data^ as bytes
  TRttiBinarySave = function(Data: pointer; Dest: TBufferWriter;
    Info: PRttiInfo): PtrInt;

  /// the type of _BINARYSAVE[] efficient lookup table
  TRttiBinarySaves = array[TRttiKind] of TRttiBinarySave;
  PRttiBinarySaves = ^TRttiBinarySaves;

  /// internal function handler for binary persistence of any RTTI type value
  // - i.e. the kind of functions called via _BINARYLOAD[] lookup table
  // - work with managed and unmanaged types
  // - fill Data^ from Source, returning the size in Data^ as bytes
  TRttiBinaryLoad = function(Data: pointer; var Source: TFastReader;
    Info: PRttiInfo): PtrInt;

  /// the type of _BINARYLOAD[] efficient lookup table
  TRttiBinaryLoads = array[TRttiKind] of TRttiBinaryLoad;
  PRttiBinaryLoads = ^TRttiBinaryLoads;

  /// internal function handler for fast comparison of any RTTI type value
  // - i.e. the kind of functions called via _BINARYCOMPARE[] lookup table
  // - work with managed and unmanaged types
  // - returns the size in Data1/Data2^ as bytes, and the result in Compared
  TRttiBinaryCompare = function(Data1, Data2: pointer; Info: PRttiInfo;
    out Compared: integer): PtrInt;

  /// the type of _BINARYCOMPARE[] efficient lookup table
  TRttiBinaryCompares = array[TRttiKind] of TRttiBinaryCompare;
  PRttiBinaryCompares = ^TRttiBinaryCompares;

var
  /// lookup table for binary persistence of any RTTI type value
  // - for efficient persistence into binary of managed and unmanaged types
  _BINARYSAVE: TRttiBinarySaves;

  /// lookup table for binary persistence of any RTTI type value
  // - for efficient retrieval from binary of managed and unmanaged types
  _BINARYLOAD: TRttiBinaryLoads;

  /// lookup table for case-sensitive comparison of any RTTI type value
  // - for efficient search or sorting of managed and unmanaged types
  _BINARYCOMPARE: TRttiBinaryCompares;

  /// lookup table for case-insensitive comparison of any RTTI type value
  // - for efficient search or sorting of managed and unmanaged types
  _BINARYCOMPAREI: TRttiBinaryCompares;


/// raw binary serialization of a dynamic array
// - as called e.g. by TDynArray.SaveTo, using ExternalCount optional parameter
// - _BINARYSAVE[rkDynArray] is a wrapper to this function, with ExternalCount=nil
procedure DynArraySave(Data: PAnsiChar; ExternalCount: PInteger;
  Dest: TBufferWriter; Info: PRttiInfo);

/// raw comparison of two dynamic arrays
// - as called e.g. by TDynArray.Equals, using ExternalCountA/B optional parameter
// - _BINARYCOMPARE[rkDynArray] and _BINARYCOMPAREI[rkDynArray] are wrappers
// to this function, with ExternalCountA/B=nil
function DynArrayCompare(A, B: PAnsiChar; ExternalCountA, ExternalCountB: PInteger;
  Info: PRttiInfo; CaseSensitive: boolean): integer;

/// raw comparison of two records
// - _BINARYCOMPARE[rkRecord] and _BINARYCOMPAREI[rkRecord] are wrappers to this
function RecordCompare(A, B: PUTF8Char; Info: PRttiInfo; CaseSensitive: boolean): integer;

/// raw comparison of two static arrays
// - _BINARYCOMPARE[rkArray] and _BINARYCOMPAREI[rkArray] are wrappers to this
function ArrayCompare(A, B: PUTF8Char; Info: PRttiInfo; CaseSensitive: boolean;
  out ArraySize: PtrInt): integer;


{ ************ INI Files and In-memory Access }

/// find a Name= Value in a [Section] of a INI RawUTF8 Content
// - this function scans the Content memory buffer, and is
// therefore very fast (no temporary TMemIniFile is created)
// - if Section equals '', find the Name= value before any [Section]
function FindIniEntry(const Content, Section, Name: RawUTF8): RawUTF8;

/// find a Name= Value in a [Section] of a INI WinAnsi Content
// - same as FindIniEntry(), but the value is converted from WinAnsi into UTF-8
function FindWinAnsiIniEntry(const Content, Section, Name: RawUTF8): RawUTF8;

/// find a Name= numeric Value in a [Section] of a INI RawUTF8 Content and
// return it as an integer, or 0 if not found
// - this function scans the Content memory buffer, and is
// therefore very fast (no temporary TMemIniFile is created)
// - if Section equals '', find the Name= value before any [Section]
function FindIniEntryInteger(const Content, Section, Name: RawUTF8): integer;
  {$ifdef HASINLINE}inline;{$endif}

/// find a Name= Value in a [Section] of a .INI file
// - if Section equals '', find the Name= value before any [Section]
// - use internaly fast FindIniEntry() function above
function FindIniEntryFile(const FileName: TFileName; const Section, Name: RawUTF8): RawUTF8;

/// update a Name= Value in a [Section] of a INI RawUTF8 Content
// - this function scans and update the Content memory buffer, and is
// therefore very fast (no temporary TMemIniFile is created)
// - if Section equals '', update the Name= value before any [Section]
procedure UpdateIniEntry(var Content: RawUTF8; const Section,Name,Value: RawUTF8);

/// update a Name= Value in a [Section] of a .INI file
// - if Section equals '', update the Name= value before any [Section]
// - use internaly fast UpdateIniEntry() function above
procedure UpdateIniEntryFile(const FileName: TFileName; const Section,Name,Value: RawUTF8);

/// find the position of the [SEARCH] section in source
// - return true if [SEARCH] was found, and store pointer to the line after it in source
function FindSectionFirstLine(var source: PUTF8Char; search: PAnsiChar): boolean;

/// find the position of the [SEARCH] section in source
// - return true if [SEARCH] was found, and store pointer to the line after it in source
// - this version expects source^ to point to an Unicode char array
function FindSectionFirstLineW(var source: PWideChar; search: PUTF8Char): boolean;

/// retrieve the whole content of a section as a string
// - SectionFirstLine may have been obtained by FindSectionFirstLine() function above
function GetSectionContent(SectionFirstLine: PUTF8Char): RawUTF8; overload;

/// retrieve the whole content of a section as a string
// - use SectionFirstLine() then previous GetSectionContent()
function GetSectionContent(const Content, SectionName: RawUTF8): RawUTF8; overload;

/// delete a whole [Section]
// - if EraseSectionHeader is TRUE (default), then the [Section] line is also
// deleted together with its content lines
// - return TRUE if something was changed in Content
// - return FALSE if [Section] doesn't exist or is already void
function DeleteSection(var Content: RawUTF8; const SectionName: RawUTF8;
  EraseSectionHeader: boolean=true): boolean; overload;

/// delete a whole [Section]
// - if EraseSectionHeader is TRUE (default), then the [Section] line is also
// deleted together with its content lines
// - return TRUE if something was changed in Content
// - return FALSE if [Section] doesn't exist or is already void
// - SectionFirstLine may have been obtained by FindSectionFirstLine() function above
function DeleteSection(SectionFirstLine: PUTF8Char; var Content: RawUTF8;
  EraseSectionHeader: boolean=true): boolean; overload;

/// replace a whole [Section] content by a new content
// - create a new [Section] if none was existing
procedure ReplaceSection(var Content: RawUTF8; const SectionName,
  NewSectionContent: RawUTF8); overload;

/// replace a whole [Section] content by a new content
// - create a new [Section] if none was existing
// - SectionFirstLine may have been obtained by FindSectionFirstLine() function above
procedure ReplaceSection(SectionFirstLine: PUTF8Char;
  var Content: RawUTF8; const NewSectionContent: RawUTF8); overload;

/// return TRUE if Value of UpperName does exist in P, till end of current section
// - expect UpperName as 'NAME='
function ExistsIniName(P: PUTF8Char; UpperName: PAnsiChar): boolean;

/// find the Value of UpperName in P, till end of current section
// - expect UpperName as 'NAME='
function FindIniNameValue(P: PUTF8Char; UpperName: PAnsiChar): RawUTF8;

/// return TRUE if one of the Value of UpperName exists in P, till end of
// current section
// - expect UpperName e.g. as 'CONTENT-TYPE: '
// - expect UpperValues to be any upper value with left side matching, e.g. as
// used by IsHTMLContentTypeTextual() function:
// ! result := ExistsIniNameValue(htmlHeaders,HEADER_CONTENT_TYPE_UPPER,
// !  ['TEXT/','APPLICATION/JSON','APPLICATION/XML']);
// - warning: this function calls IdemPCharArray(), so expects UpperValues[]
/// items to have AT LEAST TWO CHARS (it will use fast initial 2 bytes compare)
function ExistsIniNameValue(P: PUTF8Char; const UpperName: RawUTF8;
  const UpperValues: array of PAnsiChar): boolean;

/// find the integer Value of UpperName in P, till end of current section
// - expect UpperName as 'NAME='
// - return 0 if no NAME= entry was found
function FindIniNameValueInteger(P: PUTF8Char; const UpperName: RawUTF8): PtrInt;

/// replace a value from a given set of name=value lines
// - expect UpperName as 'UPPERNAME=', otherwise returns false
// - if no UPPERNAME= entry was found, then Name+NewValue is added to Content
// - a typical use may be:
// ! UpdateIniNameValue(headers,HEADER_CONTENT_TYPE,HEADER_CONTENT_TYPE_UPPER,contenttype);
function UpdateIniNameValue(var Content: RawUTF8; const Name, UpperName, NewValue: RawUTF8): boolean;


(*

{ ************ RawUTF8 String Values Interning }

type
  /// used to store one list of hashed RawUTF8 in TRawUTF8Interning pool
  // - Delphi "object" is buggy on stack -> also defined as record with methods
  {$ifdef USERECORDWITHMETHODS}TRawUTF8InterningSlot = record
    {$else}TRawUTF8InterningSlot = object{$endif}
  public
    /// actual RawUTF8 storage
    Value: TRawUTF8DynArray;
    /// hashed access to the Value[] list
    Values: TDynArrayHashed;
    /// associated mutex for thread-safe process
    Safe: TSynLocker;
    /// initialize the RawUTF8 slot (and its Safe mutex)
    procedure Init;
    /// finalize the RawUTF8 slot - mainly its associated Safe mutex
    procedure Done;
    /// returns the interned RawUTF8 value
    procedure Unique(var aResult: RawUTF8; const aText: RawUTF8; aTextHash: cardinal);
    /// ensure the supplied RawUTF8 value is interned
    procedure UniqueText(var aText: RawUTF8; aTextHash: cardinal);
    /// delete all stored RawUTF8 values
    procedure Clear;
    /// reclaim any unique RawUTF8 values
    function Clean(aMaxRefCount: integer): integer;
    /// how many items are currently stored in Value[]
    function Count: integer;
  end;

  /// allow to store only one copy of distinct RawUTF8 values
  // - thanks to the Copy-On-Write feature of string variables, this may
  // reduce a lot the memory overhead of duplicated text content
  // - this class is thread-safe and optimized for performance
  TRawUTF8Interning = class(TSynPersistent)
  protected
    fPool: array of TRawUTF8InterningSlot;
    fPoolLast: integer;
  public
    /// initialize the storage and its internal hash pools
    // - aHashTables is the pool size, and should be a power of two <= 512
    constructor Create(aHashTables: integer=4); reintroduce;
    /// finalize the storage
    destructor Destroy; override;
    /// return a RawUTF8 variable stored within this class
    // - if aText occurs for the first time, add it to the internal string pool
    // - if aText does exist in the internal string pool, return the shared
    // instance (with its reference counter increased), to reduce memory usage
    function Unique(const aText: RawUTF8): RawUTF8; overload;
    /// return a RawUTF8 variable stored within this class from a text buffer
    // - if aText occurs for the first time, add it to the internal string pool
    // - if aText does exist in the internal string pool, return the shared
    // instance (with its reference counter increased), to reduce memory usage
    function Unique(aText: PUTF8Char; aTextLen: PtrInt): RawUTF8; overload;
    /// return a RawUTF8 variable stored within this class
    // - if aText occurs for the first time, add it to the internal string pool
    // - if aText does exist in the internal string pool, return the shared
    // instance (with its reference counter increased), to reduce memory usage
    procedure Unique(var aResult: RawUTF8; const aText: RawUTF8); overload;
    /// return a RawUTF8 variable stored within this class from a text buffer
    // - if aText occurs for the first time, add it to the internal string pool
    // - if aText does exist in the internal string pool, return the shared
    // instance (with its reference counter increased), to reduce memory usage
    procedure Unique(var aResult: RawUTF8; aText: PUTF8Char; aTextLen: PtrInt); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// ensure a RawUTF8 variable is stored within this class
    // - if aText occurs for the first time, add it to the internal string pool
    // - if aText does exist in the internal string pool, set the shared
    // instance (with its reference counter increased), to reduce memory usage
    procedure UniqueText(var aText: RawUTF8);
    /// return a variant containing a RawUTF8 stored within this class
    // - similar to RawUTF8ToVariant(), but with string interning
    procedure UniqueVariant(var aResult: variant; const aText: RawUTF8); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// return a variant containing a RawUTF8 stored within this class
    // - similar to RawUTF8ToVariant(StringToUTF8()), but with string interning
    // - this method expects the text to be supplied as a VCL string, which will
    // be converted into a variant containing a RawUTF8 varString instance
    procedure UniqueVariantString(var aResult: variant; const aText: string);
    /// return a variant, may be containing a RawUTF8 stored within this class
    // - similar to TextToVariant(), but with string interning
    // - first try with GetNumericVariantFromJSON(), then fallback to
    // RawUTF8ToVariant() with string variable interning
    procedure UniqueVariant(var aResult: variant; aText: PUTF8Char; aTextLen: PtrInt;
      aAllowVarDouble: boolean=false); overload;
    /// ensure a variant contains only RawUTF8 stored within this class
    // - supplied variant should be a varString containing a RawUTF8 value
    procedure UniqueVariant(var aResult: variant); overload; {$ifdef HASINLINE}inline;{$endif}
    /// delete any previous storage pool
    procedure Clear;
    /// reclaim any unique RawUTF8 values
    // - i.e. run a garbage collection process of all values with RefCount=1
    // by default, i.e. all string which are not used any more; you may set
    // aMaxRefCount to a higher value, depending on your expecations, i.e. 2 to
    // delete all string which are referenced only once outside of the pool
    // - returns the number of unique RawUTF8 cleaned from the internal pool
    // - to be executed on a regular basis - but not too often, since the
    // process can be time consumming, and void the benefit of interning
    function Clean(aMaxRefCount: integer=1): integer;
    /// how many items are currently stored in this instance
    function Count: integer;
  end;


{ ************ TSynNameValue Name/Value Storage }

type
  /// store one Name/Value pair, as used by TSynNameValue class
  TSynNameValueItem = record
    /// the name of the Name/Value pair
    // - this property is hashed by TSynNameValue for fast retrieval
    Name: RawUTF8;
    /// the value of the Name/Value pair
    Value: RawUTF8;
    /// any associated Pointer or numerical value
    Tag: PtrInt;
  end;

  /// Name/Value pairs storage, as used by TSynNameValue class
  TSynNameValueItemDynArray = array of TSynNameValueItem;

  /// event handler used to convert on the fly some UTF-8 text content
  TOnSynNameValueConvertRawUTF8 = function(const text: RawUTF8): RawUTF8 of object;

  /// callback event used by TSynNameValue
  TOnSynNameValueNotify = procedure(const Item: TSynNameValueItem; Index: PtrInt) of object;

  /// pseudo-class used to store Name/Value RawUTF8 pairs
  // - use internaly a TDynArrayHashed instance for fast retrieval
  // - is therefore faster than TRawUTF8List
  // - is defined as an object, not as a class: you can use this in any
  // class, without the need to destroy the content
  // - Delphi "object" is buggy on stack -> also defined as record with methods
  {$ifdef USERECORDWITHMETHODS}TSynNameValue = record private
  {$else}TSynNameValue = object protected{$endif}
    fOnAdd: TOnSynNameValueNotify;
    function GetBlobData: RawByteString;
    procedure SetBlobData(const aValue: RawByteString);
    function GetStr(const aName: RawUTF8): RawUTF8; {$ifdef HASINLINE}inline;{$endif}
    function GetInt(const aName: RawUTF8): Int64; {$ifdef HASINLINE}inline;{$endif}
    function GetBool(const aName: RawUTF8): Boolean; {$ifdef HASINLINE}inline;{$endif}
  public
    /// the internal Name/Value storage
    List: TSynNameValueItemDynArray;
    /// the number of Name/Value pairs
    Count: integer;
    /// low-level access to the internal storage hasher
    DynArray: TDynArrayHashed;
    /// initialize the storage
    // - will also reset the internal List[] and the internal hash array
    procedure Init(aCaseSensitive: boolean);
    /// add an element to the array
    // - if aName already exists, its associated Value will be updated
    procedure Add(const aName, aValue: RawUTF8; aTag: PtrInt=0);
    /// reset content, then add all name=value pairs from a supplied .ini file
    // section content
    // - will first call Init(false) to initialize the internal array
    // - Section can be retrieved e.g. via FindSectionFirstLine()
    procedure InitFromIniSection(Section: PUTF8Char; OnTheFlyConvert: TOnSynNameValueConvertRawUTF8=nil;
      OnAdd: TOnSynNameValueNotify=nil);
    /// reset content, then add all name=value; CSV pairs
    // - will first call Init(false) to initialize the internal array
    // - if ItemSep=#10, then any kind of line feed (CRLF or LF) will be handled
    procedure InitFromCSV(CSV: PUTF8Char; NameValueSep: AnsiChar='=';
      ItemSep: AnsiChar=#10);
    /// reset content, then add all fields from an JSON object
    // - will first call Init() to initialize the internal array
    // - then parse the incoming JSON object, storing all its field values
    // as RawUTF8, and returning TRUE if the supplied content is correct
    // - warning: the supplied JSON buffer will be decoded and modified in-place
    function InitFromJSON(JSON: PUTF8Char; aCaseSensitive: boolean=false): boolean;
    /// reset content, then add all name, value pairs
    // - will first call Init(false) to initialize the internal array
    procedure InitFromNamesValues(const Names, Values: array of RawUTF8);
    /// search for a Name, return the index in List
    // - using fast O(1) hash algoritm
    function Find(const aName: RawUTF8): integer;
    /// search for the first chars of a Name, return the index in List
    // - using O(n) calls of IdemPChar() function
    // - here aUpperName should be already uppercase, as expected by IdemPChar()
    function FindStart(const aUpperName: RawUTF8): integer;
    /// search for a Value, return the index in List
    // - using O(n) brute force algoritm with case-sensitive aValue search
    function FindByValue(const aValue: RawUTF8): integer;
    /// search for a Name, and delete its entry in the List if it exists
    function Delete(const aName: RawUTF8): boolean;
    /// search for a Value, and delete its entry in the List if it exists
    // - returns the number of deleted entries
    // - you may search for more than one match, by setting a >1 Limit value
    function DeleteByValue(const aValue: RawUTF8; Limit: integer=1): integer;
    /// search for a Name, return the associated Value as a UTF-8 string
    function Value(const aName: RawUTF8; const aDefaultValue: RawUTF8=''): RawUTF8;
    /// search for a Name, return the associated Value as integer
    function ValueInt(const aName: RawUTF8; const aDefaultValue: Int64=0): Int64;
    /// search for a Name, return the associated Value as boolean
    // - returns true only if the value is exactly '1'
    function ValueBool(const aName: RawUTF8): Boolean;
    /// search for a Name, return the associated Value as an enumerate
    // - returns true and set aEnum if aName was found, and associated value
    // matched an aEnumTypeInfo item
    // - returns false if no match was found
    function ValueEnum(const aName: RawUTF8; aEnumTypeInfo: pointer; out aEnum;
      aEnumDefault: byte=0): boolean; overload;
    /// returns all values, as CSV or INI content
    function AsCSV(const KeySeparator: RawUTF8='=';
      const ValueSeparator: RawUTF8=#13#10; const IgnoreKey: RawUTF8=''): RawUTF8;
    /// returns all values as a JSON object of string fields
    function AsJSON: RawUTF8;
    /// fill the supplied two arrays of RawUTF8 with the stored values
    procedure AsNameValues(out Names,Values: TRawUTF8DynArray);
    /// search for a Name, return the associated Value as variant
    // - returns null if the name was not found
    function ValueVariantOrNull(const aName: RawUTF8): variant;
    /// compute a TDocVariant document from the stored values
    // - output variant will be reset and filled as a TDocVariant instance,
    // ready to be serialized as a JSON object
    // - if there is no value stored (i.e. Count=0), set null
    procedure AsDocVariant(out DocVariant: variant;
      ExtendedJson: boolean=false; ValueAsString: boolean=true;
      AllowVarDouble: boolean=false); overload;
    /// compute a TDocVariant document from the stored values
    function AsDocVariant(ExtendedJson: boolean=false; ValueAsString: boolean=true): variant; overload; {$ifdef HASINLINE}inline;{$endif}
    /// merge the stored values into a TDocVariant document
    // - existing properties would be updated, then new values will be added to
    // the supplied TDocVariant instance, ready to be serialized as a JSON object
    // - if ValueAsString is TRUE, values would be stored as string
    // - if ValueAsString is FALSE, numerical values would be identified by
    // IsString() and stored as such in the resulting TDocVariant
    // - if you let ChangedProps point to a TDocVariantData, it would contain
    // an object with the stored values, just like AsDocVariant
    // - returns the number of updated values in the TDocVariant, 0 if
    // no value was changed
    function MergeDocVariant(var DocVariant: variant;
      ValueAsString: boolean; ChangedProps: PVariant=nil;
      ExtendedJson: boolean=false; AllowVarDouble: boolean=false): integer;
    /// returns true if the Init() method has been called
    function Initialized: boolean;
    /// can be used to set all data from one BLOB memory buffer
    procedure SetBlobDataPtr(aValue: pointer);
    /// can be used to set or retrieve all stored data as one BLOB content
    property BlobData: RawByteString read GetBlobData write SetBlobData;
    /// event triggerred after an item has just been added to the list
    property OnAfterAdd: TOnSynNameValueNotify read fOnAdd write fOnAdd;
    /// search for a Name, return the associated Value as a UTF-8 string
    // - returns '' if aName is not found in the stored keys
    property Str[const aName: RawUTF8]: RawUTF8 read GetStr; default;
    /// search for a Name, return the associated Value as integer
    // - returns 0 if aName is not found, or not a valid Int64 in the stored keys
    property Int[const aName: RawUTF8]: Int64 read GetInt;
    /// search for a Name, return the associated Value as boolean
    // - returns true if aName stores '1' as associated value
    property Bool[const aName: RawUTF8]: Boolean read GetBool;
  end;

  /// a reference pointer to a Name/Value RawUTF8 pairs storage
  PSynNameValue = ^TSynNameValue;

*)

implementation

{$ifdef ISDELPHI}
uses
  typinfo; // circumvent Delphi inlining problem of mormot.core.rtti methods
{$endif ISDELPHI}


{ ************ RTL TPersistent / TInterfacedObject with Custom Constructor }

{ TPersistentWithCustomCreate }

constructor TPersistentWithCustomCreate.Create;
begin // nothing to do by default - overridden constructor may add custom code
end;


{ TInterfacedObjectWithCustomCreate }

constructor TInterfacedObjectWithCustomCreate.Create;
begin // nothing to do by default - overridden constructor may add custom code
end;

procedure TInterfacedObjectWithCustomCreate.RefCountUpdate(Release: boolean);
begin
  if Release then
    _Release
  else
    _AddRef;
end;


{ ************ TSynPersistent* / TSyn*List / TSynLocker classes }

function NewSynLocker: PSynLocker;
begin
  GetMem(result, SizeOf(TSynLocker));
  result^.Init;
end;


{ TSynPersistent }

constructor TSynPersistent.Create;
begin // nothing to do by default - overridden constructor may add custom code
end;

procedure TSynPersistent.AssignError(Source: TSynPersistent);
var
  SourceName: string;
begin
  if Source <> nil then
    SourceName := Source.ClassName
  else
    SourceName := 'nil';
  raise EConvertError.CreateFmt('Cannot assign a %s to a %s', [SourceName, ClassName]);
end;

function TSynPersistent.BeforeWriteObject(W: TBaseWriter;
  var Options: TTextWriterWriteObjectOptions): boolean;
begin
  result := false; // default JSON serialization
end;

function TSynPersistent.WritePropertyValue(W: TBaseWriter; Prop: PRttiProp;
  Options: TTextWriterWriteObjectOptions): boolean;
begin
  result := false; // default JSON serializaiton
end;

procedure TSynPersistent.AfterWriteObject(W: TBaseWriter);
begin // nothing to do
end;

procedure TSynPersistent.AssignTo(Dest: TSynPersistent);
begin
  Dest.AssignError(Self);
end;

procedure TSynPersistent.Assign(Source: TSynPersistent);
begin
  if Source <> nil then
    Source.AssignTo(Self)
  else
    AssignError(nil);
end;

class function TSynPersistent.NewInstance: TObject;
begin // bypass vmtIntfTable and vmt^.vInitTable (FPC management operators)
  GetMem(pointer(result), InstanceSize); // InstanceSize is inlined
  FillCharFast(pointer(result)^, InstanceSize, 0);
  PPointer(result)^ := pointer(self); // store VMT
end; // no benefit of rewriting FreeInstance/CleanupInstance


{ TSynList }

function TSynList.Add(item: pointer): integer;
begin
  result := ObjArrayAddCount(fList, item, fCount);
end;

procedure TSynList.Clear;
begin
  fList := nil;
  fCount := 0;
end;

procedure TSynList.Delete(index: integer);
begin
  PtrArrayDelete(fList, index, @fCount);
  if (fCount > 64) and (length(fList) > fCount * 2) then
    SetLength(fList, fCount); // reduce capacity when half list is void
end;

function TSynList.Exists(item: pointer): boolean;
begin
  result := PtrUIntScanExists(pointer(fList), fCount, PtrUInt(item));
end;

function TSynList.Get(index: Integer): pointer;
begin
  if cardinal(index) < cardinal(fCount) then
    result := fList[index]
  else
    result := nil;
end;

function TSynList.IndexOf(item: pointer): integer;
begin
  result := PtrUIntScanIndex(pointer(fList), fCount, PtrUInt(item));
end;

function TSynList.Remove(item: Pointer): integer;
begin
  result := PtrUIntScanIndex(pointer(fList), fCount, PtrUInt(item));
  if result >= 0 then
    Delete(result);
end;


{ TSynObjectList }

constructor TSynObjectList.Create(aOwnObjects: boolean);
begin
  fOwnObjects := aOwnObjects;
  inherited Create;
end;

procedure TSynObjectList.Delete(index: integer);
begin
  if cardinal(index) >= cardinal(fCount) then
    exit;
  if fOwnObjects then
    TObject(fList[index]).Free;
  inherited Delete(index);
end;

procedure TSynObjectList.Clear;
begin
  if fOwnObjects then
    RawObjectsClear(pointer(fList), fCount);
  inherited Clear;
end;

procedure TSynObjectList.ClearFromLast;
var
  i: PtrInt;
begin
  if fOwnObjects then
    for i := fCount - 1 downto 0 do // call Free in reverse order
      TObject(fList[i]).Free;
  inherited Clear;
end;

destructor TSynObjectList.Destroy;
begin
  Clear;
  inherited Destroy;
end;


{ TAutoLock }

type
  /// used by TAutoLocker.ProtectMethod and TSynLocker.ProtectMethod
  TAutoLock = class(TInterfacedObject)
  protected
    fLock: PSynLocker;
  public
    constructor Create(aLock: PSynLocker);
    destructor Destroy; override;
  end;

constructor TAutoLock.Create(aLock: PSynLocker);
begin
  fLock := aLock;
  fLock^.Lock;
end;

destructor TAutoLock.Destroy;
begin
  fLock^.UnLock;
end;


{ TSynLocker }

procedure TSynLocker.Init;
begin
  fSectionPadding := 0;
  PaddingUsedCount := 0;
  InitializeCriticalSection(fSection);
  fLocked := false;
  fInitialized := true;
end;

procedure TSynLocker.Done;
var
  i: PtrInt;
begin
  for i := 0 to PaddingUsedCount - 1 do
    if not (integer(Padding[i].VType) in VTYPE_SIMPLE) then
      VarClear(variant(Padding[i]));
  DeleteCriticalSection(fSection);
  fInitialized := false;
end;

procedure TSynLocker.DoneAndFreeMem;
begin
  Done;
  FreeMem(@self);
end;

procedure TSynLocker.Lock;
begin
  EnterCriticalSection(fSection);
  fLocked := true;
end;

procedure TSynLocker.UnLock;
begin
  fLocked := false;
  LeaveCriticalSection(fSection);
end;

function TSynLocker.TryLock: boolean;
begin
  result := not fLocked and (TryEnterCriticalSection(fSection) <> 0);
end;

function TSynLocker.TryLockMS(retryms: integer): boolean;
begin
  repeat
    result := TryLock;
    if result or (retryms <= 0) then
      break;
    SleepHiRes(1);
    dec(retryms);
  until false;
end;

function TSynLocker.ProtectMethod: IUnknown;
begin
  result := TAutoLock.Create(@self);
end;

function TSynLocker.GetVariant(Index: integer): Variant;
begin
  if cardinal(Index) < cardinal(PaddingUsedCount) then
  try
    EnterCriticalSection(fSection);
    fLocked := true;
    result := variant(Padding[Index]);
  finally
    fLocked := false;
    LeaveCriticalSection(fSection);
  end
  else
    VarClear(result);
end;

procedure TSynLocker.SetVariant(Index: integer; const Value: Variant);
begin
  if cardinal(Index) <= high(Padding) then
  try
    EnterCriticalSection(fSection);
    fLocked := true;
    if Index >= PaddingUsedCount then
      PaddingUsedCount := Index + 1;
    variant(Padding[Index]) := Value;
  finally
    fLocked := false;
    LeaveCriticalSection(fSection);
  end;
end;

function TSynLocker.GetInt64(Index: integer): Int64;
begin
  if cardinal(Index) < cardinal(PaddingUsedCount) then
  try
    EnterCriticalSection(fSection);
    fLocked := true;
    if not VariantToInt64(variant(Padding[Index]), result) then
      result := 0;
  finally
    fLocked := false;
    LeaveCriticalSection(fSection);
  end
  else
    result := 0;
end;

procedure TSynLocker.SetInt64(Index: integer; const Value: Int64);
begin
  SetVariant(Index, Value);
end;

function TSynLocker.GetBool(Index: integer): boolean;
begin
  if cardinal(Index) < cardinal(PaddingUsedCount) then
  try
    EnterCriticalSection(fSection);
    fLocked := true;
    if not VariantToBoolean(variant(Padding[Index]), result) then
      result := false;
  finally
    fLocked := false;
    LeaveCriticalSection(fSection);
  end
  else
    result := false;
end;

procedure TSynLocker.SetBool(Index: integer; const Value: boolean);
begin
  SetVariant(Index, Value);
end;

function TSynLocker.GetUnLockedInt64(Index: integer): Int64;
begin
  if (cardinal(Index) >= cardinal(PaddingUsedCount)) or not VariantToInt64(variant(Padding[Index]), result) then
    result := 0;
end;

procedure TSynLocker.SetUnlockedInt64(Index: integer; const Value: Int64);
begin
  if cardinal(Index) <= high(Padding) then
  begin
    if Index >= PaddingUsedCount then
      PaddingUsedCount := Index + 1;
    variant(Padding[Index]) := Value;
  end;
end;

function TSynLocker.GetPointer(Index: integer): Pointer;
begin
  if cardinal(Index) < cardinal(PaddingUsedCount) then
  try
    EnterCriticalSection(fSection);
    fLocked := true;
    with Padding[Index] do
      if VType = varUnknown then
        result := VUnknown
      else
        result := nil;
  finally
    fLocked := false;
    LeaveCriticalSection(fSection);
  end
  else
    result := nil;
end;

procedure TSynLocker.SetPointer(Index: integer; const Value: Pointer);
begin
  if cardinal(Index) <= high(Padding) then
  try
    EnterCriticalSection(fSection);
    fLocked := true;
    if Index >= PaddingUsedCount then
      PaddingUsedCount := Index + 1;
    with Padding[Index] do
    begin
      if not (integer(VType) in VTYPE_SIMPLE) then
        VarClear(PVariant(@VType)^);
      VType := varUnknown;
      VUnknown := Value;
    end;
  finally
    fLocked := false;
    LeaveCriticalSection(fSection);
  end;
end;

function TSynLocker.GetUTF8(Index: integer): RawUTF8;
begin
  if cardinal(Index) < cardinal(PaddingUsedCount) then
  try
    EnterCriticalSection(fSection);
    fLocked := true;
    VariantStringToUTF8(variant(Padding[Index]), result);
  finally
    fLocked := false;
    LeaveCriticalSection(fSection);
  end
  else
    result := '';
end;

procedure TSynLocker.SetUTF8(Index: integer; const Value: RawUTF8);
begin
  if cardinal(Index) <= high(Padding) then
  try
    EnterCriticalSection(fSection);
    fLocked := true;
    if Index >= PaddingUsedCount then
      PaddingUsedCount := Index + 1;
    RawUTF8ToVariant(Value, variant(Padding[Index]));
  finally
    fLocked := false;
    LeaveCriticalSection(fSection);
  end;
end;

function TSynLocker.LockedInt64Increment(Index: integer; const Increment: Int64): Int64;
begin
  if cardinal(Index) <= high(Padding) then
  try
    EnterCriticalSection(fSection);
    fLocked := true;
    result := 0;
    if Index < PaddingUsedCount then
      VariantToInt64(variant(Padding[Index]), result)
    else
      PaddingUsedCount := Index + 1;
    variant(Padding[Index]) := Int64(result + Increment);
  finally
    fLocked := false;
    LeaveCriticalSection(fSection);
  end
  else
    result := 0;
end;

function TSynLocker.LockedExchange(Index: integer; const Value: Variant): Variant;
begin
  if cardinal(Index) <= high(Padding) then
  try
    EnterCriticalSection(fSection);
    fLocked := true;
    with Padding[Index] do
    begin
      if Index < PaddingUsedCount then
        result := PVariant(@VType)^
      else
      begin
        PaddingUsedCount := Index + 1;
        VarClear(result);
      end;
      PVariant(@VType)^ := Value;
    end;
  finally
    fLocked := false;
    LeaveCriticalSection(fSection);
  end
  else
    VarClear(result);
end;

function TSynLocker.LockedPointerExchange(Index: integer; Value: pointer): pointer;
begin
  if cardinal(Index) <= high(Padding) then
  try
    EnterCriticalSection(fSection);
    fLocked := true;
    with Padding[Index] do
    begin
      if Index < PaddingUsedCount then
        if VType = varUnknown then
          result := VUnknown
        else
        begin
          VarClear(PVariant(@VType)^);
          result := nil;
        end
      else
      begin
        PaddingUsedCount := Index + 1;
        result := nil;
      end;
      VType := varUnknown;
      VUnknown := Value;
    end;
  finally
    fLocked := false;
    LeaveCriticalSection(fSection);
  end
  else
    result := nil;
end;


{ TSynPersistentLock }

constructor TSynPersistentLock.Create;
begin
  inherited Create;
  fSafe := NewSynLocker;
end;

destructor TSynPersistentLock.Destroy;
begin
  inherited Destroy;
  fSafe^.DoneAndFreeMem;
end;

function TSynPersistentLock.BeforeWriteObject(W: TBaseWriter;
  var Options: TTextWriterWriteObjectOptions): boolean;
begin
  fSafe.Lock;
  result := false; // default JSON serialization
end;

procedure TSynPersistentLock.AfterWriteObject(W: TBaseWriter);
begin
  fSafe.UnLock;
end;


{ TInterfacedObjectLocked }

constructor TInterfacedObjectLocked.Create;
begin
  inherited Create;
  fSafe := NewSynLocker;
end;

destructor TInterfacedObjectLocked.Destroy;
begin
  inherited Destroy;
  fSafe^.DoneAndFreeMem;
end;


{ TSynObjectListLocked }

constructor TSynObjectListLocked.Create(AOwnsObjects: Boolean);
begin
  inherited Create(AOwnsObjects);
  fSafe.Init;
end;

destructor TSynObjectListLocked.Destroy;
begin
  inherited Destroy;
  fSafe.Done;
end;

function TSynObjectListLocked.Add(item: pointer): integer;
begin
  Safe.Lock;
  try
    result := inherited Add(item);
  finally
    Safe.UnLock;
  end;
end;

function TSynObjectListLocked.Remove(item: pointer): integer;
begin
  Safe.Lock;
  try
    result := inherited Remove(item);
  finally
    Safe.UnLock;
  end;
end;

function TSynObjectListLocked.Exists(item: pointer): boolean;
begin
  Safe.Lock;
  try
    result := inherited Exists(item);
  finally
    Safe.UnLock;
  end;
end;

procedure TSynObjectListLocked.Clear;
begin
  Safe.Lock;
  try
    inherited Clear;
  finally
    Safe.UnLock;
  end;
end;

procedure TSynObjectListLocked.ClearFromLast;
begin
  Safe.Lock;
  try
    inherited ClearFromLast;
  finally
    Safe.UnLock;
  end;
end;


{ TSynPersistentWithID }

procedure TSynPersistentWithID.AssignTo(Dest: TSynPersistent);
begin
  if Dest.InheritsFrom(TSynPersistentWithID) then
    TSynPersistentWithID(Dest).fID := fID
  else
    Dest.AssignError(Self);
end;

function TSynPersistentWithID.BeforeWriteObject(W: TBaseWriter;
  var Options: TTextWriterWriteObjectOptions): boolean;
begin
  Exclude(Options, woStorePointer); // ID is enough to identify it
  W.WriteObjectPropName('ID', Options);
  W.AddQ(fID);
  W.Add(',');
  if woIDAsIDstr in Options then
  begin
    W.AddPropName('ID_str');
    W.Add('"');
    W.AddQ(fID);
    W.Add('"', ',');
  end;
  result := false; // continue serialization as usual
end;


{ TSynPersistentWithPassword }

destructor TSynPersistentWithPassword.Destroy;
begin
  UniqueRawUTF8(fPassword);
  FillZero(fPassword);
  inherited Destroy;
end;

class function TSynPersistentWithPassword.ComputePassword(
  const PlainPassword: RawUTF8; CustomKey: cardinal): RawUTF8;
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
  PlainPasswordLen: integer; CustomKey: cardinal): RawUTF8;
begin
  result := ComputePassword(BinToBase64uri(PlainPassword, PlainPasswordLen));
end;

class function TSynPersistentWithPassword.ComputePlainPassword(
  const CypheredPassword: RawUTF8; CustomKey: cardinal; const AppSecret: RawUTF8): RawUTF8;
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

function TSynPersistentWithPassword.GetPassWordPlain: RawUTF8;
begin
  result := GetPassWordPlainInternal('');
end;

function TSynPersistentWithPassword.GetPassWordPlainInternal(AppSecret: RawUTF8): RawUTF8;
var
  value, pass: RawByteString;
  usr: RawUTF8;
  i, j: integer;
begin
  result := '';
  if (self = nil) or (fPassWord = '') then
    exit;
  if Assigned(TSynPersistentWithPasswordUserCrypt) then
  begin
    if AppSecret = '' then
      ClassToText(ClassType, AppSecret);
    usr := ExeVersion.User + ':';
    i := PosEx(usr, fPassword);
    if (i = 1) or ((i > 0) and (fPassword[i - 1] = ',')) then
    begin
      inc(i, length(usr));
      j := PosEx(',', fPassword, i);
      if j = 0 then
        j := length(fPassword) + 1;
      Base64ToBin(@fPassword[i], j - i, pass);
      if pass <> '' then
        result := TSynPersistentWithPasswordUserCrypt(pass, AppSecret, false);
    end
    else
    begin
      i := PosExChar(':', fPassword);
      if i > 0 then
        raise ESynException.CreateUTF8('%.GetPassWordPlain unable to retrieve the ' +
          'stored value: current user is [%], but password in % was encoded for [%]',
          [self, ExeVersion.User, AppSecret, copy(fPassword, 1, i - 1)]);
    end;
  end;
  if result = '' then
  begin
    value := Base64ToBin(fPassWord);
    SymmetricEncrypt(GetKey, value);
    result := value;
  end;
end;

procedure TSynPersistentWithPassword.SetPassWordPlain(const Value: RawUTF8);
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

function TSynPersistentWithPassword.WritePropertyValue(W: TBaseWriter;
  Prop: PRttiProp; Options: TTextWriterWriteObjectOptions): boolean;
begin
  if (woHideSynPersistentPassword in Options) and
     (Prop^.TypeInfo^.Kind = rkLString) and
     Prop^.GetterIsField and (Prop^.GetterAddr(self) = @fPassWord) then
  begin
    W.AddShort('"***"');
    result := true;
  end
  else
    result := false; // default value serialization
end;


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


{ ************ Variable Length Integer Encoding / Decoding }

function ToVarInt32(Value: PtrInt; Dest: PByte): PByte;
begin // 0=0,1=1,2=-1,3=2,4=-2...
  if Value < 0 then
    // -1->2, -2->4..
    Value := (-Value) shl 1
  else if Value > 0 then
    // 1->1, 2->3..
    Value := (Value shl 1) - 1;
    // 0->0
  result := ToVarUInt32(Value, Dest);
end;

function ToVarUInt32(Value: cardinal; Dest: PByte): PByte;
label
  _1, _2, _3; // ugly but fast
begin
  if Value > $7f then
  begin
    if Value < $80 shl 7 then
      goto _1
    else if Value < $80 shl 14 then
      goto _2
    else if Value < $80 shl 21 then
      goto _3;
    Dest^ := (Value and $7F) or $80;
    Value := Value shr 7;
    inc(Dest);
_3: Dest^ := (Value and $7F) or $80;
    Value := Value shr 7;
    inc(Dest);
_2: Dest^ := (Value and $7F) or $80;
    Value := Value shr 7;
    inc(Dest);
_1: Dest^ := (Value and $7F) or $80;
    Value := Value shr 7;
    inc(Dest);
  end;
  Dest^ := Value;
  inc(Dest);
  result := Dest;
end;

function ToVarUInt32Length(Value: PtrUInt): PtrUInt;
begin
  if Value <= $7f then
    result := 1
  else if Value < $80 shl 7 then
    result := 2
  else if Value < $80 shl 14 then
    result := 3
  else if Value < $80 shl 21 then
    result := 4
  else
    result := 5;
end;

function ToVarUInt32LengthWithData(Value: PtrUInt): PtrUInt;
begin
  if Value <= $7f then
    result := Value + 1
  else if Value < $80 shl 7 then
    result := Value + 2
  else if Value < $80 shl 14 then
    result := Value + 3
  else if Value < $80 shl 21 then
    result := Value + 4
  else
    result := Value + 5;
end;

function FromVarUInt32(var Source: PByte): cardinal;
begin
  result := Source^;
  inc(Source);
  if result > $7f then
    result := (result and $7F) or FromVarUInt32Up128(Source);
end;

function FromVarUInt32Big(var Source: PByte): cardinal;
var
  c: cardinal;
  p: PByte;
begin
  p := Source;
  result := p^;
  inc(p);
  if result > $7f then
  begin // Values between 128 and 16256
    c := p^;
    c := c shl 7;
    result := result and $7F or c;
    inc(p);
    if c > $7f shl 7 then
    begin // Values between 16257 and 2080768
      c := p^;
      c := c shl 14;
      inc(p);
      result := result and $3FFF or c;
      if c > $7f shl 14 then
      begin // Values between 2080769 and 266338304
        c := p^;
        c := c shl 21;
        inc(p);
        result := result and $1FFFFF or c;
        if c > $7f shl 21 then
        begin
          c := p^;
          c := c shl 28;
          inc(p);
          result := result and $FFFFFFF or c;
        end;
      end;
    end;
  end;
  Source := p;
end;

function FromVarUInt32Up128(var Source: PByte): cardinal;
var
  c: cardinal;
  p: PByte;
begin // Values above 128
  p := Source;
  result := p^ shl 7;
  inc(p);
  if result > $7f shl 7 then
  begin // Values above 16257
    c := p^;
    c := c shl 14;
    inc(p);
    result := result and $3FFF or c;
    if c > $7f shl 14 then
    begin
      c := p^;
      c := c shl 21;
      inc(p);
      result := result and $1FFFFF or c;
      if c > $7f shl 21 then
      begin
        c := p^;
        c := c shl 28;
        inc(p);
        result := result and $FFFFFFF or c;
      end;
    end;
  end;
  Source := p;
end;

function FromVarUInt32(var Source: PByte; SourceMax: PByte; out Value: cardinal): boolean;
begin
  if SourceMax = nil then
  begin
    Value := FromVarUInt32(Source);
    result := true;
  end
  else
  begin
    Source := FromVarUInt32Safe(Source, SourceMax, Value);
    result := Source <> nil;
  end;
end;

function FromVarUInt32Safe(Source, SourceMax: PByte; out Value: cardinal): PByte;
var
  c: cardinal;
begin
  result := nil; // error
  if PAnsiChar(Source) >= PAnsiChar(SourceMax) then
    exit;
  c := Source^;
  inc(Source);
  Value := c;
  if c > $7f then
  begin // Values between 128 and 16256
    if PAnsiChar(Source) >= PAnsiChar(SourceMax) then
      exit;
    c := Source^;
    c := c shl 7;
    Value := Value and $7F or c;
    inc(Source);
    if c > $7f shl 7 then
    begin // Values between 16257 and 2080768
      if PAnsiChar(Source) >= PAnsiChar(SourceMax) then
        exit;
      c := Source^;
      c := c shl 14;
      inc(Source);
      Value := Value and $3FFF or c;
      if c > $7f shl 14 then
      begin // Values between 2080769 and 266338304
        if PAnsiChar(Source) >= PAnsiChar(SourceMax) then
          exit;
        c := Source^;
        c := c shl 21;
        inc(Source);
        Value := Value and $1FFFFF or c;
        if c > $7f shl 21 then
        begin
          if PAnsiChar(Source) >= PAnsiChar(SourceMax) then
            exit;
          c := Source^;
          c := c shl 28;
          inc(Source);
          Value := Value and $FFFFFFF or c;
        end;
      end;
    end;
  end;
  result := Source; // safely decoded
end;

function FromVarInt32(var Source: PByte): integer;
var
  c: cardinal;
  p: PByte;
begin // fast stand-alone function with no FromVarUInt32 call
  p := Source;
  result := p^;
  inc(p);
  if result > $7f then
  begin
    c := p^;
    c := c shl 7;
    result := result and $7F or integer(c);
    inc(p);
    if c > $7f shl 7 then
    begin
      c := p^;
      c := c shl 14;
      inc(p);
      result := result and $3FFF or integer(c);
      if c > $7f shl 14 then
      begin
        c := p^;
        c := c shl 21;
        inc(p);
        result := result and $1FFFFF or integer(c);
        if c > $7f shl 21 then
        begin
          c := p^;
          c := c shl 28;
          inc(p);
          result := result and $FFFFFFF or integer(c);
        end;
      end;
    end;
  end;
  Source := p;
  // 0=0,1=1,2=-1,3=2,4=-2...
  if result and 1 <> 0 then
    // 1->1, 3->2..
    result := result shr 1 + 1
  else
    // 0->0, 2->-1, 4->-2..
    result := -(result shr 1);
end;

function FromVarUInt32High(var Source: PByte): cardinal;
var
  c: cardinal;
begin
  result := Source^;
  inc(Source);
  c := Source^ shl 7;
  inc(Source);
  result := result and $7F or c;
  if c <= $7f shl 7 then
    exit;
  c := Source^ shl 14;
  inc(Source);
  result := result and $3FFF or c;
  if c <= $7f shl 14 then
    exit;
  c := Source^ shl 21;
  inc(Source);
  result := result and $1FFFFF or c;
  if c <= $7f shl 21 then
    exit;
  c := Source^ shl 28;
  inc(Source);
  result := result and $FFFFFFF or c;
end;

function ToVarInt64(Value: Int64; Dest: PByte): PByte;
begin // 0=0,1=1,2=-1,3=2,4=-2...
{$ifdef CPU32}
  if Value <= 0 then
    // 0->0, -1->2, -2->4..
    result := ToVarUInt64((-Value) shl 1, Dest)
  else
     // 1->1, 2->3..
    result := ToVarUInt64((Value shl 1) - 1, Dest);
{$else}
  if Value <= 0 then
    // 0->0, -1->2, -2->4..
    Value := (-Value) shl 1
  else
    // 1->1, 2->3..
    Value := (Value shl 1) - 1;
  result := ToVarUInt64(Value, Dest);
{$endif CPU32}
end;

function ToVarUInt64(Value: QWord; Dest: PByte): PByte;
var
  c: cardinal;
label
  _1, _2, _4; // ugly but fast
begin
  repeat
    c := Value;
    {$ifdef CPU32}
    if PCardinalArray(@Value)^[1] = 0 then
    {$else}
    if Value shr 32 = 0 then
    {$endif CPU32}
      begin
        if c > $7f then
        begin
          if c < $80 shl 7 then
            goto _1
          else if c < $80 shl 14 then
            goto _2
          else if c >= $80 shl 21 then
            goto _4;
          Dest^ := (c and $7F) or $80;
          c := c shr 7;
          inc(Dest);
_2:       Dest^ := (c and $7F) or $80;
          c := c shr 7;
          inc(Dest);
_1:       Dest^ := (c and $7F) or $80;
          c := c shr 7;
          inc(Dest);
        end;
        Dest^ := c;
        inc(Dest);
        result := Dest;
        exit;
      end;
_4: PCardinal(Dest)^ := (c and $7F) or (((c shr 7) and $7F) shl 8) or
      (((c shr 14) and $7F) shl 16) or (((c shr 21) and $7F) shl 24) or $80808080;
    inc(Dest, 4);
    Value := Value shr 28;
  until false;
end;

function FromVarUInt64(var Source: PByte): QWord;
var
  c, n: PtrUInt;
  p: PByte;
begin
  p := Source;
  {$ifdef CPU64}
  result := p^;
  if result > $7f then
  begin
    result := result and $7F;
  {$else}
  if p^ > $7f then
  begin
    result := PtrUInt(p^) and $7F;
  {$endif}
    n := 0;
    inc(p);
    repeat
      c := p^;
      inc(n, 7);
      if c <= $7f then
        break;
      result := result or (QWord(c and $7f) shl n);
      inc(p);
    until false;
    result := result or (QWord(c) shl n);
  end
  {$ifndef CPU64}
  else
    result := p^
  {$endif};
  inc(p);
  Source := p;
end;

function FromVarUInt64Safe(Source, SourceMax: PByte; out Value: QWord): PByte;
var
  c, n: PtrUInt;
begin
  result := nil; // error
  if PAnsiChar(Source) >= PAnsiChar(SourceMax) then
    exit;
  c := Source^;
  inc(Source);
  if c > $7f then
  begin
    Value := c and $7F;
    n := 7;
    repeat
      if PAnsiChar(Source) >= PAnsiChar(SourceMax) then
        exit;
      c := Source^;
      inc(Source);
      if c <= $7f then
        break;
      c := c and $7f;
      Value := Value or (QWord(c) shl n);
      inc(n, 7);
    until false;
    Value := Value or (QWord(c) shl n);
  end
  else
    Value := c;
  result := Source; // safely decoded
end;

function FromVarUInt64(var Source: PByte; SourceMax: PByte; out Value: QWord): boolean;
begin
  if SourceMax = nil then
  begin
    Value := FromVarUInt64(Source);
    result := true;
  end
  else
  begin
    Source := FromVarUInt64Safe(Source, SourceMax, Value);
    result := Source <> nil;
  end;
end;

function FromVarInt64(var Source: PByte): Int64;
var
  c, n: PtrUInt;
begin // 0=0,1=1,2=-1,3=2,4=-2...
{$ifdef CPU64}
  result := Source^;
  if result > $7f then
  begin
    result := result and $7F;
    n := 0;
    inc(Source);
    repeat
      c := Source^;
      inc(n, 7);
      if c <= $7f then
        break;
      result := result or (Int64(c and $7f) shl n);
      inc(Source);
    until false;
    result := result or (Int64(c) shl n);
  end;
  if result and 1 <> 0 then
    // 1->1, 3->2..
    result := result shr 1 + 1
  else
    // 0->0, 2->-1, 4->-2..
    result := -(result shr 1);
{$else}
  c := Source^;
  if c > $7f then
  begin
    result := c and $7F;
    n := 0;
    inc(Source);
    repeat
      c := Source^;
      inc(n, 7);
      if c <= $7f then
        break;
      result := result or (Int64(c and $7f) shl n);
      inc(Source);
    until false;
    result := result or (Int64(c) shl n);
    if PCardinal(@result)^ and 1 <> 0 then
      // 1->1, 3->2..
      result := result shr 1 + 1
    else
      // 0->0, 2->-1, 4->-2..
      result := -(result shr 1);
  end
  else
  begin
    if c = 0 then
      result := 0
    else if c and 1 = 0 then
      // 0->0, 2->-1, 4->-2..
      result := -Int64(c shr 1)
    else
      // 1->1, 3->2..
      result := (c shr 1) + 1;
  end;
{$endif CPU64}
  inc(Source);
end;

function FromVarInt64Value(Source: PByte): Int64;
var
  c, n: PtrUInt;
begin // 0=0,1=1,2=-1,3=2,4=-2...
  c := Source^;
  if c > $7f then
  begin
    result := c and $7F;
    n := 0;
    inc(Source);
    repeat
      c := Source^;
      inc(n, 7);
      if c <= $7f then
        break;
      result := result or (Int64(c and $7f) shl n);
      inc(Source);
    until false;
    result := result or (Int64(c) shl n);
    {$ifdef CPU64}
    if result and 1 <> 0 then
    {$else}
    if PCardinal(@result)^ and 1 <> 0 then
    {$endif}
      // 1->1, 3->2..
      result := result shr 1 + 1
    else
      // 0->0, 2->-1, 4->-2..
      result := -Int64(result shr 1);
  end
  else if c = 0 then
    result := 0
  else if c and 1 = 0 then
    // 0->0, 2->-1, 4->-2..
    result := -Int64(c shr 1)
  else
    // 1->1, 3->2..
    result := (c shr 1) + 1;
end;

function GotoNextVarInt(Source: PByte): pointer;
begin
  if Source <> nil then
  begin
    if Source^ > $7f then
      repeat
        inc(Source)
      until Source^ <= $7f;
    inc(Source);
  end;
  result := Source;
end;


function ToVarString(const Value: RawUTF8; Dest: PByte): PByte;
var
  Len: integer;
begin
  Len := Length(Value);
  Dest := ToVarUInt32(Len, Dest);
  if Len > 0 then
  begin
    MoveFast(pointer(Value)^, Dest^, Len);
    result := pointer(PAnsiChar(Dest) + Len);
  end
  else
    result := Dest;
end;

function GotoNextVarString(Source: PByte): pointer;
begin
  result := Pointer(PtrUInt(Source) + FromVarUInt32(Source));
end;

function FromVarString(var Source: PByte): RawUTF8;
var
  len: PtrUInt;
begin
  len := FromVarUInt32(Source);
  FastSetStringCP(Result, Source, len, CP_UTF8);
  inc(Source, len);
end;

function FromVarString(var Source: PByte; SourceMax: PByte): RawUTF8;
var
  len: cardinal;
begin
  Source := FromVarUInt32Safe(Source, SourceMax, len);
  if (Source = nil) or (PAnsiChar(Source) + len > PAnsiChar(SourceMax)) then
    len := 0;
  FastSetStringCP(Result, Source, len, CP_UTF8);
  inc(Source, len);
end;

procedure FromVarString(var Source: PByte; var Value: TSynTempBuffer);
var
  len: integer;
begin
  len := FromVarUInt32(Source);
  Value.Init(Source, len);
  PByteArray(Value.buf)[len] := 0; // include trailing #0
  inc(Source, len);
end;

function FromVarString(var Source: PByte; SourceMax: PByte;
  var Value: TSynTempBuffer): boolean;
var
  len: cardinal;
begin
  if SourceMax = nil then
    len := FromVarUInt32(Source)
  else
  begin
    Source := FromVarUInt32Safe(Source, SourceMax, len);
    if (Source = nil) or
       (PAnsiChar(Source) + len > PAnsiChar(SourceMax)) then
    begin
      result := false;
      exit;
    end;
  end;
  Value.Init(Source, len);
  PByteArray(Value.buf)[len] := 0; // include trailing #0
  inc(Source, len);
  result := true;
end;

procedure FromVarString(var Source: PByte; var Value: RawByteString;
  CodePage: integer);
var
  Len: PtrUInt;
begin
  Len := FromVarUInt32(Source);
  FastSetStringCP(Value, Source, Len, CodePage);
  inc(Source, Len);
end;

function FromVarString(var Source: PByte; SourceMax: PByte;
  var Value: RawByteString; CodePage: integer): boolean;
var
  len: cardinal;
begin
  if SourceMax = nil then
    len := FromVarUInt32(Source)
  else
  begin
    Source := FromVarUInt32Safe(Source, SourceMax, len);
    if (Source = nil) or (PAnsiChar(Source) + len > PAnsiChar(SourceMax)) then
    begin
      result := false;
      exit;
    end;
  end;
  FastSetStringCP(Value, Source, len, CodePage);
  inc(Source, len);
  result := true;
end;

function FromVarBlob(Data: PByte): TValueResult;
begin
  Result.Len := FromVarUInt32(Data);
  Result.Ptr := pointer(Data);
end;



{ ****************** TFastReader / TBufferWriter Binary Streams }

{ TFastReader }

procedure TFastReader.Init(Buffer: pointer; Len: integer);
begin
  P := Buffer;
  Last := P + Len;
  OnErrorOverflow := nil;
  OnErrorData := nil;
  Tag := 0;
end;

procedure TFastReader.Init(const Buffer: RawByteString);
begin
  Init(pointer(Buffer), length(Buffer));
end;

procedure TFastReader.ErrorOverflow;
begin
  if Assigned(OnErrorOverflow) then
    OnErrorOverflow
  else
    raise EFastReader.Create('Reached End of Input');
end;

procedure TFastReader.ErrorData(const fmt: RawUTF8; const args: array of const);
begin
  if Assigned(OnErrorData) then
    OnErrorData(fmt, args)
  else
    raise EFastReader.CreateUTF8('Incorrect Data: ' + fmt, args);
end;

function TFastReader.EOF: boolean;
begin
  result := P >= Last;
end;

function TFastReader.RemainingLength: PtrUInt;
begin
  result := PtrUInt(Last) - PtrUInt(P);
end;

function TFastReader.NextByte: byte;
begin
  if P >= Last then
    ErrorOverflow;
  result := ord(P^);
  inc(P);
end;

function TFastReader.NextByteSafe(dest: pointer): boolean;
begin
  if P >= Last then
    result := false
  else
  begin
    PAnsiChar(dest)^ := P^;
    inc(P);
    result := true;
  end;
end;

function TFastReader.Next4: cardinal;
begin
  if P + 3 >= Last then
    ErrorOverflow;
  result := PCardinal(P)^;
  inc(P, 4);
end;

function TFastReader.Next8: QWord;
begin
  if P + 7 >= Last then
    ErrorOverflow;
  result := PQWord(P)^;
  inc(P, 8);
end;

function TFastReader.NextByteEquals(Value: byte): boolean;
begin
  if P >= Last then
    ErrorOverflow;
  if ord(P^) = Value then
  begin
    inc(P);
    result := true;
  end
  else
    result := false;
end;

function TFastReader.Next(DataLen: PtrInt): pointer;
begin
  if P + DataLen > Last then
    ErrorOverflow;
  result := P;
  inc(P, DataLen);
end;

function TFastReader.NextSafe(out Data: Pointer; DataLen: PtrInt): boolean;
begin
  if P + DataLen > Last then
    result := false
  else
  begin
    Data := P;
    inc(P, DataLen);
    result := true;
  end;
end;

procedure TFastReader.Copy(Dest: pointer; DataLen: PtrInt);
begin
  if P + DataLen > Last then
    ErrorOverflow;
  MoveFast(P^, Dest^, DataLen);
  inc(P, DataLen);
end;

function TFastReader.CopySafe(Dest: pointer; DataLen: PtrInt): boolean;
begin
  if P + DataLen > Last then
    result := false
  else
  begin
    MoveFast(P^, Dest^, DataLen);
    inc(P, DataLen);
    result := true;
  end;
end;

function TFastReader.VarInt32: integer;
begin
  result := VarUInt32;
  if result and 1 <> 0 then
    // 1->1, 3->2..
    result := result shr 1 + 1
  else    // 0->0, 2->-1, 4->-2..
    result := -(result shr 1);
end;

function TFastReader.VarInt64: Int64;
begin
  result := VarUInt64;
  if result and 1 <> 0 then
    // 1->1, 3->2..
    result := result shr 1 + 1
  else    // 0->0, 2->-1, 4->-2..
    result := -(result shr 1);
end;

{$ifdef CPUX86} // not enough CPU registers

function TFastReader.VarUInt32: cardinal;
var
  c: cardinal;
label
  e;
begin
  if P >= Last then
    goto e;
  result := ord(P^);
  inc(P);
  if result <= $7f then
    exit;
  if P >= Last then
    goto e;
  c := ord(P^) shl 7;
  inc(P);
  result := result and $7F or c;
  if c <= $7f shl 7 then
    exit; // Values between 128 and 16256
  if P >= Last then
    goto e;
  c := ord(P^) shl 14;
  inc(P);
  result := result and $3FFF or c;
  if c <= $7f shl 14 then
    exit; // Values between 16257 and 2080768
  if P >= Last then
    goto e;
  c := ord(P^) shl 21;
  inc(P);
  result := result and $1FFFFF or c;
  if c <= $7f shl 21 then
    exit; // Values between 2080769 and 266338304
  if P >= Last then
e:begin
    {$ifdef ISDELPHI}
    result := 0; // avoid hint
    {$endif}
    ErrorOverflow;
  end;
  c := ord(P^) shl 28;
  inc(P);
  result := result {%H-}and $FFFFFFF or c;
end;

procedure TFastReader.VarNextInt;
begin
  repeat
    if P >= Last then
      break;  // reached end of input
    if P^ <= #$7f then
      break; // reached end of VarUInt32/VarUInt64
    inc(P);
  until false;
  inc(P);
end;

procedure TFastReader.VarNextInt(count: integer);
begin
  if count = 0 then
    exit;
  repeat
    if P >= Last then
      break;  // reached end of input
    if P^ > #$7f then
    begin
      inc(P);
      continue; // didn't reach end of VarUInt32/VarUInt64
    end;
    inc(P);
    dec(count);
    if count = 0 then
      break;
  until false;
end;

{$else not CPUX86} // on x86_64 and ARM, use registers for P/Last values

function TFastReader.VarUInt32: cardinal;
var
  c: cardinal;
  s, l: PByte;
label
  e, f;
begin
  s := pointer(P);
  l := pointer(Last);
  if PAnsiChar(s) >= PAnsiChar(l) then
    goto e;
  result := s^;
  inc(s);
  if result <= $7f then
    goto f;
  if PAnsiChar(s) >= PAnsiChar(l) then
    goto e;
  c := s^ shl 7;
  inc(s);
  result := result and $7F or c;
  if c <= $7f shl 7 then
    goto f; // Values between 128 and 16256
  if PAnsiChar(s) >= PAnsiChar(l) then
    goto e;
  c := s^ shl 14;
  inc(s);
  result := result and $3FFF or c;
  if c <= $7f shl 14 then
    goto f; // Values between 16257 and 2080768
  if PAnsiChar(s) >= PAnsiChar(l) then
    goto e;
  c := s^ shl 21;
  inc(s);
  result := result and $1FFFFF or c;
  if c <= $7f shl 21 then
    goto f; // Values between 2080769 and 266338304
  if PAnsiChar(s) >= PAnsiChar(l) then
e:begin
    {$ifdef ISDELPHI}
    result := 0; // avoid hint
    {$endif}
    ErrorOverflow;
  end;
  c := s^ shl 28;
  inc(s);
  result := result {%H-}and $FFFFFFF or c;
f:P := pointer(s);
end;

procedure TFastReader.VarNextInt;
var
  s, l: PAnsiChar;
begin
  s := P;
  l := Last;
  repeat
    if s >= l then
      break;  // reached end of input
    if s^ <= #$7f then
      break; // reached end of VarUInt32/VarUInt64
    inc(s);
  until false;
  P := s + 1;
end;

procedure TFastReader.VarNextInt(count: integer);
var
  s, l: PAnsiChar;
begin
  if count = 0 then
    exit;
  s := P;
  l := Last;
  repeat
    if s >= l then
      break;  // reached end of input
    if s^ > #$7f then
    begin
      inc(s);
      continue; // didn't reach end of VarUInt32/VarUInt64
    end;
    inc(s);
    dec(count);
    if count = 0 then
      break;
  until false;
  P := s;
end;

{$endif CPUX86}

function TFastReader.PeekVarInt32(out value: PtrInt): boolean;
begin
  result := PeekVarUInt32(PtrUInt(value));
  if result then
    if value and 1 <> 0 then
      // 1->1, 3->2..
      value := value shr 1 + 1
    else      // 0->0, 2->-1, 4->-2..
      value := -(value shr 1);
end;

function TFastReader.PeekVarUInt32(out value: PtrUInt): boolean;
var
  s: PAnsiChar;
begin
  result := false;
  s := P;
  repeat
    if s >= Last then
      exit  // reached end of input -> returns false
    else if s^ <= #$7f then
      break; // reached end of VarUInt32
    inc(s);
  until false;
  s := P;
  value := VarUInt32; // fast value decode
  P := s; // rewind
  result := true;
end;

function TFastReader.VarUInt32Safe(out Value: cardinal): boolean;
var
  c, n, v: cardinal;
begin
  result := false;
  if P >= Last then
    exit;
  v := ord(P^);
  inc(P);
  if v > $7f then
  begin
    n := 0;
    v := v and $7F;
    repeat
      if P >= Last then
        exit;
      c := ord(P^);
      inc(P);
      inc(n, 7);
      if c <= $7f then
        break;
      v := v or ((c and $7f) shl n);
    until false;
    v := v or (c shl n);
  end;
  Value := v;
  result := true; // success
end;

function TFastReader.VarUInt64: QWord;
label
  e;
var
  c, n: PtrUInt;
begin
  if P >= Last then
e:  ErrorOverflow;
  c := ord(P^);
  inc(P);
  if c > $7f then
  begin
    result := c and $7F;
    n := 0;
    repeat
      if P >= Last then
        goto e;
      c := ord(P^);
      inc(P);
      inc(n, 7);
      if c <= $7f then
        break;
      result := result or (QWord(c and $7f) shl n);
    until false;
    result := result or (QWord(c) shl n);
  end
  else
    result := c;
end;

procedure TFastReader.VarBlob(out result: TValueResult);
var
  len: PtrUInt;
begin
  len := VarUInt32;
  if P + len > Last then
    ErrorOverflow;
  result.Ptr := P;
  result.Len := len;
  inc(P, len);
end;

procedure TFastReader.VarBlob(out Value: TSynTempBuffer);
var
  len: PtrUInt;
begin
  len := VarUInt32;
  if P + len > Last then
    ErrorOverflow;
  Value.Init(P, len);
  inc(P, len);
end;

function TFastReader.VarBlob: TValueResult;
var
  len: PtrUInt;
label
  e;
{%H-}begin
  if P >= Last then
    goto e;
  len := ord(P^);
  if len < $80 then
    inc(P)
  else
    len := VarUInt32;
  result.Ptr := P;
  result.Len := len;
  if P + len <= Last then
    inc(P, len)
  else
e:  ErrorOverflow;
end;

function TFastReader.VarString: RawByteString;
begin
  with VarBlob do
    SetString(result, Ptr, Len);
end;

procedure TFastReader.VarUTF8(out result: RawUTF8);
var
  len: PtrUInt;
label
  e;
begin
  if P >= Last then
    goto e;
  len := ord(P^);
  if len < $80 then
    inc(P)
  else
    len := VarUInt32;
  if P + len <= Last then
  begin
    FastSetString(result, P, len);
    inc(P, len);
  end
  else
e:  ErrorOverflow;
end;

function TFastReader.VarUTF8: RawUTF8;
begin
  VarUTF8(result);
end;

function TFastReader.VarShortString: shortstring;
var
  len: cardinal;
  s: PAnsiChar;
label
  e, r;
{%H-}begin
  s := P;
  if s >= Last then
    goto e;
  len := ord(s^);
  if len <= $7f then
  begin
    inc(s);
r:  P := s;
    inc(s, len);
    if s >= Last then
      goto e;
    result[0] := AnsiChar(len);
    MoveFast(P^, result[1], len);
    P := s;
    exit;
  end;
  len := (len and $7F) or (ord(s^) shl 7); // 2nd byte of VarUInt32 decoding
  inc(s);
  if len <= 255 then
    goto r;
e:ErrorOverflow;
end;

function TFastReader.VarUTF8Safe(out Value: RawUTF8): boolean;
var
  len: cardinal;
begin
  if VarUInt32Safe(len) then
    if len = 0 then
      result := true
    else if P + len <= Last then
    begin
      FastSetString(Value, P, len);
      inc(P, len);
      result := true;
    end
    else
      result := false
  else
    result := false;
end;

function TFastReader.ReadVarUInt32Array(var Values: TIntegerDynArray): PtrInt;
var
  i: PtrInt;
  k: TBufferWriterKind;
begin
  result := VarUInt32;
  SetLength(Values, result);
  k := TBufferWriterKind(NextByte);
  if k = wkUInt32 then
  begin
    Copy(pointer(Values), result * 4);
    exit;
  end;
  Next(4); // format: Isize+varUInt32s
  case k of
    wkVarInt32:
      for i := 0 to result - 1 do
        Values[i] := VarInt32;
    wkVarUInt32:
      for i := 0 to result - 1 do
        Values[i] := VarUInt32;
  else
    ErrorData('ReadVarUInt32Array: unhandled kind=%', [ord(k)]);
  end;
end;

function TFastReader.ReadCompressed(Load: TAlgoCompressLoad;
  BufferOffset: integer): RawByteString;
var
  comp: PAnsiChar;
  complen: PtrUInt;
begin
  complen := VarUInt32;
  comp := Next(complen);
  TAlgoCompress.Algo(comp, complen).Decompress(
    comp, complen, result, Load, BufferOffset);
end;


{ TBufferWriter }

constructor TBufferWriter.Create(aFile: THandle; BufLen: integer);
begin
  Create(THandleStream.Create(aFile), BufLen);
  fInternalStream := true;
end;

constructor TBufferWriter.Create(const aFileName: TFileName;
  BufLen: integer; Append: boolean);
var
  s: TStream;
begin
  if Append and FileExists(aFileName) then
  begin
    s := TFileStream.Create(aFileName, fmOpenWrite);
    s.Seek(0, soFromEnd);
  end
  else
    s := TFileStream.Create(aFileName, fmCreate);
  Create(s, BufLen);
  fInternalStream := true;
end;

constructor TBufferWriter.Create(aStream: TStream; BufLen: integer);
begin
  if BufLen > 1 shl 22 then
    fBufLen := 1 shl 22 // 4 MB sounds right enough
  else if BufLen < 256 then
    raise ESynException.CreateUTF8('%.Create(BufLen=%)', [self, BufLen]);
  fBufLen := BufLen;
  fBufLen16 := fBufLen - 16;
  fStream := aStream;
  GetMem(fBufferInternal, fBufLen);
  fBuffer := fBufferInternal;
end;

constructor TBufferWriter.Create(aStream: TStream;
  aTempBuf: pointer; aTempLen: integer);
begin
  fBufLen := aTempLen;
  fBuffer := aTempBuf;
  fStream := aStream;
end;

constructor TBufferWriter.Create(aClass: TStreamClass; BufLen: integer);
begin
  Create(aClass.Create, BufLen);
  fInternalStream := true;
end;

constructor TBufferWriter.Create(aClass: TStreamClass;
  aTempBuf: pointer; aTempLen: integer);
begin
  Create(aClass.Create, aTempBuf, aTempLen);
  fInternalStream := true;
end;

constructor TBufferWriter.Create(const aStackBuffer: TSynTempBuffer);
begin
  Create(TRawByteStringStream, @aStackBuffer, SizeOf(aStackBuffer));
end;

destructor TBufferWriter.Destroy;
begin
  if fInternalStream then
    fStream.Free;
  if fBufferInternal <> nil then
    FreeMem(fBufferInternal);
  inherited;
end;

procedure TBufferWriter.InternalFlush;
begin
  if fPos = 0 then
    exit;
  fStream.WriteBuffer(fBuffer^, fPos);
  inc(fTotalFlushed, fPos);
  fPos := 0;
end;

function TBufferWriter.GetTotalWritten: Int64;
begin
  result := fTotalFlushed + fPos;
end;

function TBufferWriter.Flush: Int64;
begin
  if fPos > 0 then
    InternalFlush;
  result := GetTotalWritten;
  fTotalFlushed := 0;
end;

procedure TBufferWriter.CancelAll;
begin
  fTotalFlushed := 0;
  fPos := 0;
  if fStream.ClassType = TRawByteStringStream then
    TRawByteStringStream(fStream).Size := 0
  else
    fStream.Seek(0, soBeginning);
end;

procedure TBufferWriter.FlushAndWrite(Data: pointer; DataLen: PtrInt);
begin
  if DataLen < 0 then
    exit;
  if fPos > 0 then
    InternalFlush;
  if DataLen > fBufLen then
    fStream.WriteBuffer(Data^, DataLen)
  else
  begin
    MoveFast(Data^, fBuffer^[fPos], DataLen);
    inc(fPos, DataLen);
  end;
end;

procedure TBufferWriter.Write(Data: pointer; DataLen: PtrInt);
var
  p: PtrUInt;
begin
  p := fPos;
  if p + PtrUInt(DataLen) <= PtrUInt(fBufLen) then
  begin
    MoveFast(Data^, fBuffer^[p], DataLen);
    inc(fPos, DataLen);
  end
  else
    FlushAndWrite(Data, DataLen); // will also handle DataLen<0
end;

procedure TBufferWriter.WriteN(Data: Byte; Count: integer);
var
  len: integer;
begin
  while Count > 0 do
  begin
    if Count > fBufLen then
      len := fBufLen
    else
      len := Count;
    if fPos + len > fBufLen then
      InternalFlush;
    FillCharFast(fBuffer^[fPos], len, Data);
    inc(fPos, len);
    dec(Count, len);
  end;
end;

procedure TBufferWriter.Write1(Data: Byte);
begin
  if fPos > fBufLen16 then
    InternalFlush;
  fBuffer^[fPos] := Data;
  inc(fPos);
end;

procedure TBufferWriter.Write2(Data: cardinal);
begin
  if fPos > fBufLen16 then
    InternalFlush;
  PWord(@fBuffer^[fPos])^ := Data;
  inc(fPos, SizeOf(Word));
end;

procedure TBufferWriter.Write4(Data: integer);
begin
  if fPos > fBufLen16 then
    InternalFlush;
  PInteger(@fBuffer^[fPos])^ := Data;
  inc(fPos, SizeOf(integer));
end;

procedure TBufferWriter.Write4BigEndian(Data: integer);
begin
  Write4(bswap32(Data));
end;

procedure TBufferWriter.Write8(Data8Bytes: pointer);
begin
  if fPos > fBufLen16 then
    InternalFlush;
  PInt64(@fBuffer^[fPos])^ := PInt64(Data8Bytes)^;
  inc(fPos, SizeOf(Int64));
end;

procedure TBufferWriter.WriteI64(Data: Int64);
begin
  if fPos > fBufLen16 then
    InternalFlush;
  PInt64(@fBuffer^[fPos])^ := Data;
  inc(fPos, SizeOf(Data));
end;

procedure TBufferWriter.WriteVar(Data: pointer; DataLen: PtrInt);
label
  wr;
begin
  if fPos + DataLen <= fBufLen16 then // inlined most common cases
  begin
    if DataLen < $80 then // e.g. small strings
    begin
      fBuffer^[fPos] := DataLen;
      inc(fPos);
      if DataLen = 0 then
        exit;
wr:   MoveFast(Data^, fBuffer^[fPos], DataLen);
      inc(fPos, DataLen);
      exit;
    end;
    fPos := PtrUInt(ToVarUInt32(DataLen, @fBuffer^[fPos])) - PtrUInt(fBuffer);
    goto wr;
  end;
  // Data wouldn't fit in memory buffer -> direct write
  WriteVarUInt32(DataLen);
  Write(Data, DataLen);
end;

procedure TBufferWriter.Write(const Text: RawByteString);
begin
  WriteVar(pointer(Text), length(Text));
end;

procedure TBufferWriter.WriteShort(const Text: ShortString);
begin
  WriteVar(@Text[1], ord(Text[0]));
end;

procedure TBufferWriter.WriteBinary(const Data: RawByteString);
begin
  Write(pointer(Data), Length(Data));
end;

function TBufferWriter.DirectWritePrepare(len: PtrInt;
  var tmp: RawByteString): PAnsiChar;
begin
  if (len <= fBufLen) and (fPos + len > fBufLen) then
    InternalFlush;
  if fPos + len > fBufLen then
  begin
    if len > length(tmp) then
      SetString(tmp, nil, len); // don't reallocate buffer, but reuse big enough
    result := pointer(tmp);
  end
  else
    result := @fBuffer^[fPos]; // write directly into the buffer
end;

procedure TBufferWriter.DirectWriteFlush(len: PtrInt; const tmp: RawByteString);
begin
  if tmp = '' then
    inc(fPos, len)
  else
    Write(pointer(tmp), len);
end;

procedure TBufferWriter.WriteXor(New, Old: PAnsiChar; Len: PtrInt;
  crc: PCardinal);
var
  L: integer;
  Dest: PAnsiChar;
begin
  if (New = nil) or (Old = nil) then
    exit;
  while Len > 0 do
  begin
    Dest := pointer(fBuffer);
    if fPos + Len > fBufLen then
      InternalFlush
    else
      inc(Dest, fPos);
    if Len > fBufLen then
      L := fBufLen
    else
      L := Len;
    XorMemory(pointer(Dest), pointer(New), pointer(Old), L);
    if crc <> nil then
      crc^ := crc32c(crc^, Dest, L);
    inc(Old, L);
    inc(New, L);
    dec(Len, L);
    inc(fPos, L);
  end;
end;

procedure TBufferWriter.WriteRawUTF8DynArray(const Values: TRawUTF8DynArray;
  ValuesCount: integer);
begin
  WriteRawUTF8Array(pointer(Values), ValuesCount);
end;

procedure TBufferWriter.WriteRawUTF8Array(Values: PPtrUIntArray;
  ValuesCount: integer);
var
  n, i: integer;
  fixedsize, len: PtrUInt;
  P, PEnd: PByte;
  PBeg: PAnsiChar;
begin
  WriteVarUInt32(ValuesCount);
  if ValuesCount = 0 then
    exit;
  fixedsize := Values^[0];
  if fixedsize <> 0 then
  begin
    fixedsize := {%H-}PStrLen(fixedsize - _STRLEN)^;
    for i := 1 to ValuesCount - 1 do
      if (Values^[i] = 0) or
         ({%H-}PStrLen(Values^[i] - _STRLEN)^ <> TStrLen(fixedsize)) then
      begin
        fixedsize := 0;
        break;
      end;
  end;
  WriteVarUInt32(fixedsize);
  repeat
    P := @fBuffer^[fPos];
    PEnd := @fBuffer^[fBufLen16];
    if PtrUInt(P) < PtrUInt(PEnd) then
    begin
      n := ValuesCount;
      PBeg := PAnsiChar(P); // leave space for chunk size
      inc(P, 4);
      if fixedsize = 0 then
        for i := 0 to ValuesCount - 1 do
          if Values^[i] = 0 then
          begin
            P^ := 0; // store length=0
            inc(P);
            if PtrUInt(P) >= PtrUInt(PEnd) then
            begin
              n := i + 1;
              break; // avoid buffer overflow
            end;
          end
          else
          begin
            len := {%H-}PStrLen(Values^[i] - _STRLEN)^;
            if PtrUInt(PEnd) - PtrUInt(P) <= len then
            begin
              n := i;
              break; // avoid buffer overflow
            end;
            P := ToVarUInt32(len, P);
            MoveFast(pointer(Values^[i])^, P^, len); // here len>0
            inc(P, len);
          end
      else // fixedsize<>0:
        for i := 0 to ValuesCount - 1 do
        begin
          if PtrUInt(PEnd) - PtrUInt(P) <= fixedsize then
          begin
            n := i;
            break; // avoid buffer overflow
          end;
          MoveFast(pointer(Values^[i])^, P^, fixedsize);
          inc(P, fixedsize);
        end;
      len := PAnsiChar(P) - PBeg; // format: Isize+varUInt32s*strings
      PInteger(PBeg)^ := len - 4;
      inc(fPos, len);
      inc(PByte(Values), n * SizeOf(PtrInt));
      dec(ValuesCount, n);
      if ValuesCount = 0 then
        break;
    end;
    InternalFlush;
  until false;
end;

procedure TBufferWriter.WriteStream(aStream: TCustomMemoryStream;
  aStreamSize: Integer);
begin
  if aStreamSize < 0 then
    if aStream = nil then
      aStreamSize := 0
    else
      aStreamSize := aStream.Size;
  WriteVarUInt32(aStreamSize);
  if aStreamSize > 0 then
    Write(aStream.Memory, aStreamSize);
end;

procedure TBufferWriter.WriteVarInt32(Value: PtrInt);
begin
  if Value <= 0 then
    // 0->0, -1->2, -2->4..
    Value := (-Value) shl 1
  else    // 1->1, 2->3..
    Value := (Value shl 1) - 1;
  WriteVarUInt32(Value);
end;

procedure TBufferWriter.WriteVarUInt32(Value: PtrUInt);
begin
  if fPos > fBufLen16 then
    InternalFlush;
  fPos := PtrUInt(ToVarUInt32(Value, @fBuffer^[fPos])) - PtrUInt(fBuffer);
end;

procedure TBufferWriter.WriteVarInt64(Value: Int64);
begin
  if fPos > fBufLen16 then
    InternalFlush;
  fPos := PtrUInt(ToVarInt64(Value, @fBuffer^[fPos])) - PtrUInt(fBuffer);
end;

procedure TBufferWriter.WriteVarUInt64(Value: QWord);
begin
  if fPos > fBufLen16 then
    InternalFlush;
  fPos := PtrUInt(ToVarUInt64(Value, @fBuffer^[fPos])) - PtrUInt(fBuffer);
end;

function CleverStoreInteger(p: PInteger; V, VEnd: PAnsiChar; pCount: integer;
  var StoredCount: integer): PAnsiChar;
// Clever = store Values[i+1]-Values[i] (with special diff=1 count)
// format:  Integer: firstValue, then:
//          B:0 W:difference with previous
//          B:1..253 = difference with previous
//          B:254 W:byOne
//          B:255 B:byOne
var
  i, d, byOne: integer;
begin
  StoredCount := pCount;
  if pCount <= 0 then
  begin
    result := V;
    exit;
  end;
  i := p^;
  PInteger(V)^ := p^;
  inc(V, 4);
  dec(pCount);
  inc(p);
  byOne := 0;
  if pCount > 0 then
    repeat
      d := p^ - i;
      i := p^;
      inc(p);
      if d = 1 then
      begin
        dec(pCount);
        inc(byOne);
        if pCount > 0 then
          continue;
      end
      else if d < 0 then
      begin
        result := nil;
        exit;
      end;
      if byOne <> 0 then
      begin
        case byOne of
          1:
            begin
              V^ := #1;
              inc(V);
            end; // B:1..253 = difference with previous
          2:
            begin
              PWord(V)^ := $0101;
              inc(V, 2);
            end; // B:1..253 = difference
        else
          if byOne > 255 then
          begin
            while byOne > 65535 do
            begin
              PInteger(V)^ := $fffffe;
              inc(V, 3); // store as many len=$ffff as necessary
              dec(byOne, $ffff);
            end;
            PInteger(V)^ := byOne shl 8 + $fe;
            inc(V, 3); // B:254 W:byOne
          end
          else
          begin
            PWord(V)^ := byOne shl 8 + $ff;
            inc(V, 2); // B:255 B:byOne
          end;
        end; // case byOne of
        if pCount = 0 then
          break;
        byOne := 0;
      end;
      if (d = 0) or (d > 253) then
      begin
        while cardinal(d) > 65535 do
        begin
          PInteger(V)^ := $ffff00;
          inc(V, 3); // store as many len=$ffff as necessary
          dec(cardinal(d), $ffff);
        end;
        dec(pCount);
        PInteger(V)^ := d shl 8;
        inc(V, 3); // B:0 W:difference with previous
        if (V < VEnd) and (pCount > 0) then
          continue
        else
          break;
      end
      else
      begin
        dec(pCount);
        V^ := AnsiChar(d);
        inc(V); // B:1..253 = difference with previous
        if (V < VEnd) and (pCount > 0) then
          continue
        else
          break;
      end;
      if V >= VEnd then
        break; // avoid GPF
    until false;
  dec(StoredCount, pCount);
  result := V;
end;

procedure TBufferWriter.WriteVarUInt32Array(const Values: TIntegerDynArray;
  ValuesCount: integer; DataLayout: TBufferWriterKind);
begin
  WriteVarUInt32Values(pointer(Values), ValuesCount, DataLayout);
end;

procedure TBufferWriter.WriteVarUInt32Values(Values: PIntegerArray;
  ValuesCount: integer; DataLayout: TBufferWriterKind);
var
  diff, v, vp, n: integer;
  i: PtrInt;
  P: PByte;
  PBeg, PEnd: PAnsiChar;
begin
  WriteVarUInt32(ValuesCount);
  if ValuesCount = 0 then
    exit;
  fBuffer^[fPos] := ord(DataLayout);
  inc(fPos);
  vp := Values^[0];
  if DataLayout in [wkOffsetU, wkOffsetI] then
  begin
    fPos := PtrUInt(ToVarUInt32(vp, @fBuffer^[fPos])) - PtrUInt(fBuffer);
    diff := Values^[1] - vp;
    inc(PInteger(Values));
    dec(ValuesCount);
    if ValuesCount = 0 then
      exit;
    if diff > 0 then
    begin
      for i := 1 to ValuesCount - 1 do
        if Values^[i] - Values^[i - 1] <> diff then
        begin
          diff := 0; // not always the same offset
          break;
        end;
    end
    else
      diff := 0;
    fPos := PtrUInt(ToVarUInt32(diff, @fBuffer^[fPos])) - PtrUInt(fBuffer);
    if diff <> 0 then
      exit; // same offset for all items (fixed sized records) -> quit now
  end;
  repeat
    P := @fBuffer^[fPos];
    PEnd := @fBuffer^[fBufLen16];
    if PtrUInt(P) < PtrUInt(PEnd) then
    begin
      case DataLayout of
        wkUInt32:
          begin
            n := (fBufLen - fPos) shr 2;
            if ValuesCount < n then
              n := ValuesCount;
            MoveFast(Values^, P^, n * 4);
            inc(P, n * 4);
          end;
        wkVarInt32, wkVarUInt32, wkOffsetU, wkOffsetI:
          begin
            PBeg := PAnsiChar(P); // leave space for chunk size
            inc(P, 4);
            n := ValuesCount;
            for i := 0 to ValuesCount - 1 do
            begin
              v := Values^[i];
              case DataLayout of
                wkVarInt32:
                  P := ToVarInt32(v, P);
                wkVarUInt32:
                  P := ToVarUInt32(v, P);
                wkOffsetU:
                  P := ToVarUInt32(v - vp, P);
                wkOffsetI:
                  P := ToVarInt32(v - vp, P);
              end;
              vp := v;
              if PtrUInt(P) >= PtrUInt(PEnd) then
              begin
                n := i + 1;
                break; // avoid buffer overflow
              end;
            end;
            PInteger(PBeg)^ := PAnsiChar(P) - PBeg - 4; // format: Isize+varUInt32s
          end;
        wkSorted:
          begin
            PBeg := PAnsiChar(P) + 4; // leave space for chunk size
            P := PByte(CleverStoreInteger(pointer(Values), PBeg, PEnd, ValuesCount, n));
            if P = nil then
              raise ESynException.CreateUTF8('%.WriteVarUInt32Array: data not sorted', [self]);
            PInteger(PBeg - 4)^ := PAnsiChar(P) - PBeg; // format: Isize+cleverStorage
          end;
      end;
      inc(PByte(Values), n * 4);
      fPos := PtrUInt(P) - PtrUInt(fBuffer);
      dec(ValuesCount, n);
      if ValuesCount = 0 then
        break;
    end;
    InternalFlush;
  until false;
end;

procedure TBufferWriter.WriteVarUInt64DynArray(const Values: TInt64DynArray;
  ValuesCount: integer; Offset: Boolean);
var
  n: integer;
  i: PtrInt;
  diff: Int64;
  P, PEnd: PByte;
  PI: PInt64Array;
  PBeg: PAnsiChar;
begin
  WriteVarUInt32(ValuesCount);
  if ValuesCount = 0 then
    exit;
  PI := pointer(Values);
  if Offset then
  begin
    fBuffer^[fPos] := 1;
    fPos := PtrUInt(ToVarUInt64(PI^[0], @fBuffer^[fPos + 1])) - PtrUInt(fBuffer);
    diff := PI^[1] - PI^[0];
    inc(PByte(PI), 8);
    dec(ValuesCount);
    if ValuesCount = 0 then
      exit;
    if (diff > 0) and (diff < MaxInt) then
    begin
      for i := 1 to ValuesCount - 1 do
        if PI^[i] - PI^[i - 1] <> diff then
        begin
          diff := 0; // not always the same offset
          break;
        end;
    end
    else
      diff := 0;
    fPos := PtrUInt(ToVarUInt32(diff, @fBuffer^[fPos])) - PtrUInt(fBuffer);
    if diff <> 0 then
      exit; // same offset for all items (fixed sized records) -> quit now
  end
  else
  begin
    fBuffer^[fPos] := 0;
    inc(fPos);
  end;
  repeat
    P := @fBuffer^[fPos];
    PEnd := @fBuffer^[fBufLen16];
    if PtrUInt(P) < PtrUInt(PEnd) then
    begin
      PBeg := PAnsiChar(P); // leave space for chunk size
      inc(P, 4);
      n := ValuesCount;
      for i := 0 to ValuesCount - 1 do
      begin
        if Offset then
          P := ToVarUInt64(PI^[i] - PI^[i - 1], P) // store diffs
        else
          P := ToVarUInt64(PI^[i], P);
        if PtrUInt(P) >= PtrUInt(PEnd) then
        begin
          n := i + 1;
          break; // avoid buffer overflow
        end;
      end;
      PInteger(PBeg)^ := PAnsiChar(P) - PBeg - 4; // format: Isize+varUInt32/64s
      inc(PByte(PI), n * 8);
      fPos := PtrUInt(P) - PtrUInt(fBuffer);
      dec(ValuesCount, n);
      if ValuesCount = 0 then
        break;
    end;
    InternalFlush;
  until false;
end;

function TBufferWriter.FlushTo: RawByteString;
begin
  Flush;
  result := (fStream as TRawByteStringStream).DataString;
end;

function TBufferWriter.FlushAndCompress(nocompression: boolean;
  algo: TAlgoCompress; BufferOffset: integer): RawByteString;
var
  trig: integer;
begin
  if algo = nil then
    algo := AlgoSynLZ;
  trig := SYNLZTRIG[nocompression];
  if fStream.Position = 0 then
    // direct compression from internal buffer
    result := algo.Compress(PAnsiChar(fBuffer), fPos, trig, false, BufferOffset)
  else
    // from temporary allocation in TRawByteStringStream.DataString
    result := algo.Compress(FlushTo, trig, false, BufferOffset);
end;



{ ************ Base64, Base64URI, URL and Baudot Encoding / Decoding }

type
  TBase64Enc = array[0..63] of AnsiChar;
  PBase64Enc = ^TBase64Enc;
  TBase64Dec = array[AnsiChar] of shortint;
  PBase64Dec = ^TBase64Dec;

const
  b64enc:    TBase64Enc =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  b64URIenc: TBase64Enc =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_';

var
  /// a conversion table from Base64 text into binary data
  // - used by Base64ToBin/IsBase64 functions
  // - contains -1 for invalid char, -2 for '=', 0..63 for b64enc[] chars
  ConvertBase64ToBin, ConvertBase64URIToBin: TBase64Dec;

  
{ --------- Base64 encoding/decoding }

function Base64AnyDecode(const decode: TBase64Dec; sp, rp: PAnsiChar; len: PtrInt): boolean;
var
  c, ch: PtrInt;
begin
  result := false;
  while len >= 4 do
  begin
    c := decode[sp[0]];
    if c < 0 then
      exit;
    c := c shl 6;
    ch := decode[sp[1]];
    if ch < 0 then
      exit;
    c := (c or ch) shl 6;
    ch := decode[sp[2]];
    if ch < 0 then
      exit;
    c := (c or ch) shl 6;
    ch := decode[sp[3]];
    if ch < 0 then
      exit;
    c := c or ch;
    rp[2] := AnsiChar(c);
    c := c shr 8;
    rp[1] := AnsiChar(c);
    c := c shr 8;
    rp[0] := AnsiChar(c);
    dec(len, 4);
    inc(rp, 3);
    inc(sp, 4);
  end;
  if len >= 2 then
  begin
    c := decode[sp[0]];
    if c < 0 then
      exit;
    c := c shl 6;
    ch := decode[sp[1]];
    if ch < 0 then
      exit;
    if len = 2 then
      rp[0] := AnsiChar((c or ch) shr 4)
    else
    begin
      c := (c or ch) shl 6;
      ch := decode[sp[2]];
      if ch < 0 then
        exit;
      c := (c or ch) shr 2;
      rp[1] := AnsiChar(c);
      rp[0] := AnsiChar(c shr 8);
    end;
  end;
  result := true;
end;

function Base64Decode(sp, rp: PAnsiChar; len: PtrInt): boolean;
{$ifdef FPC} inline;{$endif}
var
  tab: PBase64Dec; // use local register
begin
  tab := @ConvertBase64ToBin;
  len := len shl 2; // len was the number of 4 chars chunks in sp
  if (len > 0) and (tab[sp[len - 2]] >= 0) then
    if tab[sp[len - 1]] >= 0 then

    else
      dec(len)
  else
    dec(len, 2); // Base64AnyDecode() algorithm ignores the trailing '='
  result := Base64AnyDecode(tab^, sp, rp, len);
end;

{$ifdef ASMX86}

function Base64EncodeMain(rp, sp: PAnsiChar; len: cardinal): integer;
asm // eax=rp edx=sp ecx=len - pipeline optimized version by AB
        push    ebx
        push    esi
        push    edi
        push    ebp
        mov     ebx, edx
        mov     esi, eax
        mov     eax, ecx
        mov     edx, 1431655766 // faster eax=len div 3 using reciprocal
        sar     ecx, 31
        imul    edx
        mov     eax, edx
        sub     eax, ecx
        mov     edi, offset b64enc
        mov     ebp, eax
        push    eax
        jz      @z
        // edi=b64enc[] ebx=sp esi=rp ebp=len div 3
        xor     eax, eax
        @1:     // read 3 bytes from sp
        movzx   edx, byte ptr[ebx]
        shl     edx, 16
        mov     al, [ebx + 2]
        mov     ah, [ebx + 1]
        add     ebx, 3
        or      eax, edx
        // encode as Base64
        mov     ecx, eax
        mov     edx, eax
        shr     ecx, 6
        and     edx, $3f
        and     ecx, $3f
        mov     dh, [edi + edx]
        mov     dl, [edi + ecx]
        mov     ecx, eax
        shr     eax, 12
        shr     ecx, 18
        shl     edx, 16
        and     ecx, $3f
        and     eax, $3f
        mov     cl, [edi + ecx]
        mov     ch, [edi + eax]
        or      ecx, edx
        // write the 4 encoded bytes into rp
        mov     [esi], ecx
        add     esi, 4
        dec     ebp
        jnz     @1
@z:     pop     eax // result := len div 3
        pop     ebp
        pop     edi
        pop     esi
        pop     ebx
end;

{$else}

function Base64EncodeMain(rp, sp: PAnsiChar; len: cardinal): integer;
var
  c: cardinal;
  enc: PBase64Enc; // use local register
begin
  enc := @b64enc;
  len := len div 3;
  result := len;
  if len <> 0 then
    repeat
      c := (ord(sp[0]) shl 16) or (ord(sp[1]) shl 8) or ord(sp[2]);
      rp[0] := enc[(c shr 18) and $3f];
      rp[1] := enc[(c shr 12) and $3f];
      rp[2] := enc[(c shr 6) and $3f];
      rp[3] := enc[c and $3f];
      inc(rp, 4);
      inc(sp, 3);
      dec(len)
    until len = 0;
end;

{$endif ASMX86}

procedure Base64EncodeTrailing(rp, sp: PAnsiChar; len: cardinal);
var
  c: cardinal;
  enc: PBase64Enc; // use local register
begin
  enc := @b64enc;
  case len of
    1:
      begin
        c := ord(sp[0]) shl 4;
        rp[0] := enc[(c shr 6) and $3f];
        rp[1] := enc[c and $3f];
        PWord(rp + 2)^ := ord('=') + ord('=') shl 8;
      end;
    2:
      begin
        c := (ord(sp[0]) shl 10) or (ord(sp[1]) shl 2);
        rp[0] := enc[(c shr 12) and $3f];
        rp[1] := enc[(c shr 6) and $3f];
        rp[2] := enc[c and $3f];
        rp[3] := '=';
      end;
  end;
end;

procedure Base64Encode(rp, sp: PAnsiChar; len: cardinal);
var
  main: cardinal;
begin
  main := Base64EncodeMain(rp, sp, len);
  Base64EncodeTrailing(rp + main * 4, sp + main * 3, len - main * 3);
end;

function BinToBase64Length(len: PtrUInt): PtrUInt;
begin
  result := ((len + 2) div 3) * 4;
end;

function BinToBase64(const s: RawByteString): RawUTF8;
var
  len: integer;
begin
  result := '';
  len := length(s);
  if len = 0 then
    exit;
  FastSetString(result, nil, BinToBase64Length(len));
  Base64Encode(pointer(result), pointer(s), len);
end;

function BinToBase64Short(Bin: PAnsiChar; BinBytes: integer): shortstring;
var
  destlen: integer;
begin
  result := '';
  if BinBytes = 0 then
    exit;
  destlen := BinToBase64Length(BinBytes);
  if destlen > 255 then
    exit; // avoid buffer overflow
  result[0] := AnsiChar(destlen);
  Base64Encode(@result[1], Bin, BinBytes);
end;

function BinToBase64Short(const s: RawByteString): shortstring;
begin
  result := BinToBase64Short(pointer(s), length(s));
end;

function BinToBase64(Bin: PAnsiChar; BinBytes: integer): RawUTF8;
begin
  result := '';
  if BinBytes = 0 then
    exit;
  FastSetString(result, nil, BinToBase64Length(BinBytes));
  Base64Encode(pointer(result), Bin, BinBytes);
end;

function BinToBase64(const data, Prefix, Suffix: RawByteString; WithMagic: boolean): RawUTF8;
var
  lendata, lenprefix, lensuffix, len: integer;
  res: PByteArray absolute result;
begin
  result := '';
  lendata := length(data);
  lenprefix := length(Prefix);
  lensuffix := length(Suffix);
  if lendata + lenprefix + lensuffix = 0 then
    exit;
  len := ((lendata + 2) div 3) * 4 + lenprefix + lensuffix;
  if WithMagic then
    inc(len, 3);
  FastSetString(result, nil, len);
  if lenprefix > 0 then
    MoveSmall(pointer(Prefix), res, lenprefix);
  if WithMagic then
  begin
    PInteger(@res[lenprefix])^ := JSON_BASE64_MAGIC;
    inc(lenprefix, 3);
  end;
  Base64Encode(@res[lenprefix], pointer(data), lendata);
  if lensuffix > 0 then
    MoveSmall(pointer(Suffix), @res[len - lensuffix], lensuffix);
end;

function BinToBase64WithMagic(const data: RawByteString): RawUTF8;
var
  len: integer;
begin
  result := '';
  len := length(data);
  if len = 0 then
    exit;
  FastSetString(result, nil, ((len + 2) div 3) * 4 + 3);
  PInteger(pointer(result))^ := JSON_BASE64_MAGIC;
  Base64Encode(PAnsiChar(pointer(result)) + 3, pointer(data), len);
end;

function BinToBase64WithMagic(Data: pointer; DataLen: integer): RawUTF8;
begin
  result := '';
  if DataLen <= 0 then
    exit;
  FastSetString(result, nil, ((DataLen + 2) div 3) * 4 + 3);
  PInteger(pointer(result))^ := JSON_BASE64_MAGIC;
  Base64Encode(PAnsiChar(pointer(result)) + 3, Data, DataLen);
end;

function IsBase64Internal(sp: PAnsiChar; len: PtrInt; dec: PBase64Dec): boolean;
var
  i: PtrInt;
begin
  result := false;
  if (len = 0) or (len and 3 <> 0) then
    exit;
  for i := 0 to len - 5 do
    if dec[sp[i]] < 0 then
      exit;
  inc(sp, len - 4);
  if (dec[sp[0]] = -1) or (dec[sp[1]] = -1) or (dec[sp[2]] = -1) or (dec[sp[3]] = -1) then
    exit;
  result := true; // layout seems correct
end;

function IsBase64(sp: PAnsiChar; len: PtrInt): boolean;
begin
  result := IsBase64Internal(sp, len, @ConvertBase64ToBin);
end;

function IsBase64(const s: RawByteString): boolean;
begin
  result := IsBase64Internal(pointer(s), length(s), @ConvertBase64ToBin);
end;

function Base64ToBinLengthSafe(sp: PAnsiChar; len: PtrInt): PtrInt;
var
  dec: PBase64Dec;
begin
  dec := @ConvertBase64ToBin;
  if IsBase64Internal(sp, len, dec) then
  begin
    if dec[sp[len - 2]] >= 0 then
      if dec[sp[len - 1]] >= 0 then
        result := 0
      else
        result := 1
    else
      result := 2;
    result := (len shr 2) * 3 - result;
  end
  else
    result := 0;
end;

function Base64ToBinLength(sp: PAnsiChar; len: PtrInt): PtrInt;
var
  dec: PBase64Dec;
begin
  result := 0;
  if (len = 0) or (len and 3 <> 0) then
    exit;
  dec := @ConvertBase64ToBin;
  if dec[sp[len - 2]] >= 0 then
    if dec[sp[len - 1]] >= 0 then
      result := 0
    else
      result := 1
  else
    result := 2;
  result := (len shr 2) * 3 - result;
end;

function Base64ToBin(const s: RawByteString): RawByteString;
begin
  Base64ToBinSafe(pointer(s), length(s), result);
end;

function Base64ToBin(sp: PAnsiChar; len: PtrInt): RawByteString;
begin
  Base64ToBinSafe(sp, len, result);
end;

function Base64ToBin(sp: PAnsiChar; len: PtrInt; var data: RawByteString): boolean;
begin
  result := Base64ToBinSafe(sp, len, data);
end;

function Base64ToBinSafe(const s: RawByteString): RawByteString;
begin
  Base64ToBinSafe(pointer(s), length(s), result);
end;

function Base64ToBinSafe(sp: PAnsiChar; len: PtrInt): RawByteString;
begin
  Base64ToBinSafe(sp, len, result);
end;

function Base64ToBinSafe(sp: PAnsiChar; len: PtrInt; var data: RawByteString): boolean;
var
  resultLen: PtrInt;
begin
  resultLen := Base64ToBinLength(sp, len);
  if resultLen <> 0 then
  begin
    SetString(data, nil, resultLen);
    if ConvertBase64ToBin[sp[len - 2]] >= 0 then
      if ConvertBase64ToBin[sp[len - 1]] >= 0 then
        // keep len as it is
      else
        dec(len)
    else
      dec(len, 2); // adjust for Base64AnyDecode() algorithm
    result := Base64AnyDecode(ConvertBase64ToBin, sp, pointer(data), len);
    if not result then
      data := '';
  end
  else
  begin
    result := false;
    data := '';
  end;
end;

function Base64ToBin(sp: PAnsiChar; len: PtrInt; var blob: TSynTempBuffer): boolean;
begin
  blob.Init(Base64ToBinLength(sp, len));
  result := (blob.len > 0) and Base64Decode(sp, blob.buf, len shr 2);
end;

function Base64ToBin(base64, bin: PAnsiChar; base64len, binlen: PtrInt; nofullcheck: boolean): boolean;
begin // nofullcheck is just ignored and deprecated
  result := (bin <> nil) and (Base64ToBinLength(base64, base64len) = binlen) and Base64Decode(base64, bin, base64len shr 2);
end;

function Base64ToBin(const base64: RawByteString; bin: PAnsiChar; binlen: PtrInt; nofullcheck: boolean): boolean;
begin
  result := Base64ToBin(pointer(base64), bin, length(base64), binlen, nofullcheck);
end;

{ --------- Base64 URI encoding/decoding }

{$ifdef ASMX86}

function Base64uriEncodeMain(rp, sp: PAnsiChar; len: cardinal): integer;
asm // eax=rp edx=sp ecx=len - pipeline optimized version by AB
        push    ebx
        push    esi
        push    edi
        push    ebp
        mov     ebx, edx
        mov     esi, eax
        mov     eax, ecx
        mov     edx, 1431655766 // faster eax=len div 3 using reciprocal
        sar     ecx, 31
        imul    edx
        mov     eax, edx
        sub     eax, ecx
        mov     edi, offset b64urienc
        mov     ebp, eax
        push    eax
        jz      @z
        // edi=b64urienc[] ebx=sp esi=rp ebp=len div 3
        xor     eax, eax
@1:    // read 3 bytes from sp
        movzx   edx, byte ptr[ebx]
        shl     edx, 16
        mov     al, [ebx + 2]
        mov     ah, [ebx + 1]
        add     ebx, 3
        or      eax, edx
        // encode as Base64uri
        mov     ecx, eax
        mov     edx, eax
        shr     ecx, 6
        and     edx, $3f
        and     ecx, $3f
        mov     dh, [edi + edx]
        mov     dl, [edi + ecx]
        mov     ecx, eax
        shr     eax, 12
        shr     ecx, 18
        shl     edx, 16
        and     ecx, $3f
        and     eax, $3f
        mov     cl, [edi + ecx]
        mov     ch, [edi + eax]
        or      ecx, edx
        // write the 4 encoded bytes into rp
        mov     [esi], ecx
        add     esi, 4
        dec     ebp
        jnz     @1
@z:     pop     eax // result := len div 3
        pop     ebp
        pop     edi
        pop     esi
        pop     ebx
end;

procedure Base64uriEncodeTrailing(rp, sp: PAnsiChar; len: cardinal);
  {$ifdef HASINLINE} inline;{$endif}
var
  c: cardinal;
begin
  case len of
    1:
      begin
        c := ord(sp[0]) shl 4;
        rp[0] := b64urienc[(c shr 6) and $3f];
        rp[1] := b64urienc[c and $3f];
      end;
    2:
      begin
        c := ord(sp[0]) shl 10 + ord(sp[1]) shl 2;
        rp[0] := b64urienc[(c shr 12) and $3f];
        rp[1] := b64urienc[(c shr 6) and $3f];
        rp[2] := b64urienc[c and $3f];
      end;
  end;
end;

procedure Base64uriEncode(rp, sp: PAnsiChar; len: cardinal);
var
  main: cardinal;
begin
  main := Base64uriEncodeMain(rp, sp, len);
  Base64uriEncodeTrailing(rp + main * 4, sp + main * 3, len - main * 3);
end;

{$else}

procedure Base64uriEncode(rp, sp: PAnsiChar; len: cardinal);
var
  main, c: cardinal;
  enc: PBase64Enc; // faster especially on x86_64 and PIC
begin
  enc := @b64URIenc;
  main := len div 3;
  if main <> 0 then
  begin
    dec(len, main * 3); // fast modulo
    repeat
      c := (ord(sp[0]) shl 16) or (ord(sp[1]) shl 8) or ord(sp[2]);
      rp[0] := enc[(c shr 18) and $3f];
      rp[1] := enc[(c shr 12) and $3f];
      rp[2] := enc[(c shr 6) and $3f];
      rp[3] := enc[c and $3f];
      inc(rp, 4);
      inc(sp, 3);
      dec(main)
    until main = 0;
  end;
  case len of
    1:
      begin
        c := ord(sp[0]) shl 4;
        rp[0] := enc[(c shr 6) and $3f];
        rp[1] := enc[c and $3f];
      end;
    2:
      begin
        c := (ord(sp[0]) shl 10) or (ord(sp[1]) shl 2);
        rp[0] := enc[(c shr 12) and $3f];
        rp[1] := enc[(c shr 6) and $3f];
        rp[2] := enc[c and $3f];
      end;
  end;
end;

{$endif ASMX86}

function BinToBase64uriLength(len: PtrUInt): PtrUInt;
begin
  result := (len div 3) * 4;
  case len - (result shr 2) * 3 of // fast len mod 3
    1:
      inc(result, 2);
    2:
      inc(result, 3);
  end;
end;

function BinToBase64uri(const s: RawByteString): RawUTF8;
var
  len: integer;
begin
  result := '';
  len := length(s);
  if len = 0 then
    exit;
  FastSetString(result, nil, BinToBase64uriLength(len));
  Base64uriEncode(pointer(result), pointer(s), len);
end;

function BinToBase64uri(Bin: PAnsiChar; BinBytes: integer): RawUTF8;
begin
  result := '';
  if BinBytes <= 0 then
    exit;
  FastSetString(result, nil, BinToBase64uriLength(BinBytes));
  Base64uriEncode(pointer(result), Bin, BinBytes);
end;

function BinToBase64uriShort(Bin: PAnsiChar; BinBytes: integer): shortstring;
var
  len: integer;
begin
  result := '';
  if BinBytes <= 0 then
    exit;
  len := BinToBase64uriLength(BinBytes);
  if len > 255 then
    exit;
  byte(result[0]) := len;
  Base64uriEncode(@result[1], Bin, BinBytes);
end;

function Base64uriToBinLength(len: PtrInt): PtrInt;
begin
  if len = 0 then
    result := 0
  else
  begin
    result := (len shr 2) * 3;
    case len and 3 of
      1:
        result := 0;
      2:
        inc(result, 1);
      3:
        inc(result, 2);
    end;
  end;
end;

function Base64uriDecode(sp, rp: PAnsiChar; len: PtrInt): boolean;
begin
  result := Base64AnyDecode(ConvertBase64URIToBin, sp, rp, len);
end;

function Base64uriToBin(sp: PAnsiChar; len: PtrInt): RawByteString;
begin
  Base64uriToBin(sp, len, result);
end;

function Base64uriToBin(const s: RawByteString): RawByteString;
begin
  Base64uriToBin(pointer(s), length(s), result);
end;

procedure Base64uriToBin(sp: PAnsiChar; len: PtrInt; var result: RawByteString);
var
  resultLen: PtrInt;
begin
  resultLen := Base64uriToBinLength(len);
  if resultLen <> 0 then
  begin
    SetString(result, nil, resultLen);
    if Base64AnyDecode(ConvertBase64URIToBin, sp, pointer(result), len) then
      exit;
  end;
  result := '';
end;

function Base64uriToBin(sp: PAnsiChar; len: PtrInt; var temp: TSynTempBuffer): boolean;
begin
  temp.Init(Base64uriToBinLength(len));
  result := (temp.len > 0) and
    Base64AnyDecode(ConvertBase64URIToBin, sp, temp.buf, len);
end;

function Base64uriToBin(const base64: RawByteString; bin: PAnsiChar; binlen: PtrInt): boolean;
begin
  result := Base64uriToBin(pointer(base64), bin, length(base64), binlen);
end;

function Base64uriToBin(base64, bin: PAnsiChar; base64len, binlen: PtrInt): boolean;
var
  resultLen: PtrInt;
begin
  resultLen := Base64uriToBinLength(base64len);
  result := (resultLen = binlen) and
    Base64AnyDecode(ConvertBase64URIToBin, base64, bin, base64len);
end;

procedure Base64ToURI(var base64: RawUTF8);
var
  P: PUTF8Char;
begin
  P := UniqueRawUTF8(base64);
  if P <> nil then
    repeat
      case P^ of
        #0:
          break;
        '+':
          P^ := '-';
        '/':
          P^ := '_';
        '=':
          begin // trim unsignificant trailing '=' characters
            SetLength(base64, P - pointer(base64));
            break;
          end;
      end;
      inc(P);
    until false;
end;

procedure Base64MagicDecode(var ParamValue: RawUTF8);
var
  tmp: RawUTF8;
begin // '\uFFF0base64encodedbinary' decode into binary (input shall have been checked)
  tmp := ParamValue;
  if not Base64ToBinSafe(PAnsiChar(pointer(tmp)) + 3, length(tmp) - 3,
          RawByteString(ParamValue)) then
    ParamValue := '';
end;

function Base64MagicCheckAndDecode(Value: PUTF8Char; var Blob: RawByteString): boolean;
var
  ValueLen: integer;
begin // '\uFFF0base64encodedbinary' checked and decode into binary
  if (Value = nil) or (Value[0] = #0) or (Value[1] = #0) or (Value[2] = #0) or
     (PCardinal(Value)^ and $ffffff <> JSON_BASE64_MAGIC) then
    result := false
  else
  begin
    ValueLen := StrLen(Value) - 3;
    if ValueLen > 0 then
      result := Base64ToBinSafe(PAnsiChar(Value) + 3, ValueLen, Blob)
    else
      result := false;
  end;
end;

function Base64MagicCheckAndDecode(Value: PUTF8Char; var Blob: TSynTempBuffer): boolean;
var
  ValueLen: integer;
begin // '\uFFF0base64encodedbinary' checked and decode into binary
  if (Value = nil) or (Value[0] = #0) or (Value[1] = #0) or (Value[2] = #0) or
     (PCardinal(Value)^ and $ffffff <> JSON_BASE64_MAGIC) then
    result := false
  else
  begin
    ValueLen := StrLen(Value) - 3;
    if ValueLen > 0 then
      result := Base64ToBin(PAnsiChar(Value) + 3, ValueLen, Blob)
    else
      result := false;
  end;
end;

function Base64MagicCheckAndDecode(Value: PUTF8Char; ValueLen: integer;
  var Blob: RawByteString): boolean;
begin // '\uFFF0base64encodedbinary' checked and decode into binary
  if (ValueLen < 4) or (PCardinal(Value)^ and $ffffff <> JSON_BASE64_MAGIC) then
    result := false
  else
    result := Base64ToBinSafe(PAnsiChar(Value) + 3, ValueLen - 3, Blob);
end;


{ --------- Baudot encoding/decoding }

const
  // see https://en.wikipedia.org/wiki/Baudot_code
  Baudot2Char: array[0..63] of AnsiChar =
   #0'e'#10'a siu'#13'drjnfcktzlwhypqobg'#254'mxv'#255+
   #0'3'#10'- ''87'#13#0'4'#0',!:(5+)2$6019?@'#254'./;'#255;
var
  Char2Baudot: array[AnsiChar] of byte;

function AsciiToBaudot(const Text: RawUTF8): RawByteString;
begin
  result := AsciiToBaudot(pointer(Text), length(Text));
end;

function AsciiToBaudot(P: PAnsiChar; len: PtrInt): RawByteString;
var
  i: PtrInt;
  c, d, bits: integer;
  shift: boolean;
  dest: PByte;
  tmp: TSynTempBuffer;
begin
  result := '';
  if (P = nil) or (len = 0) then
    exit;
  shift := false;
  dest := tmp.Init((len * 10) shr 3);
  d := 0;
  bits := 0;
  for i := 0 to len - 1 do
  begin
    c := Char2Baudot[P[i]];
    if c > 32 then
    begin
      if not shift then
      begin
        d := (d shl 5) or 27;
        inc(bits, 5);
        shift := true;
      end;
      d := (d shl 5) or (c - 32);
      inc(bits, 5);
    end
    else if c > 0 then
    begin
      if shift and (P[i] >= ' ') then
      begin
        d := (d shl 5) or 31;
        inc(bits, 5);
        shift := false;
      end;
      d := (d shl 5) or c;
      inc(bits, 5);
    end;
    while bits >= 8 do
    begin
      dec(bits, 8);
      dest^ := d shr bits;
      inc(dest);
    end;
  end;
  if bits > 0 then
  begin
    dest^ := d shl (8 - bits);
    inc(dest);
  end;
  SetString(result, PAnsiChar(tmp.buf), PAnsiChar(dest) - PAnsiChar(tmp.buf));
  tmp.Done;
end;

function BaudotToAscii(const Baudot: RawByteString): RawUTF8;
begin
  result := BaudotToAscii(pointer(Baudot), length(Baudot));
end;

function BaudotToAscii(Baudot: PByteArray; len: PtrInt): RawUTF8;
var
  i: PtrInt;
  c, b, bits, shift: integer;
  tmp: TSynTempBuffer;
  dest: PAnsiChar;
begin
  result := '';
  if (Baudot = nil) or (len <= 0) then
    exit;
  dest := tmp.Init((len shl 3) div 5);
  try
    shift := 0;
    b := 0;
    bits := 0;
    for i := 0 to len - 1 do
    begin
      b := (b shl 8) or Baudot[i];
      inc(bits, 8);
      while bits >= 5 do
      begin
        dec(bits, 5);
        c := (b shr bits) and 31;
        case c of
          27:
            if shift <> 0 then
              exit
            else
              shift := 32;
          31:
            if shift <> 0 then
              shift := 0
            else
              exit;
        else
          begin
            c := ord(Baudot2Char[c + shift]);
            if c = 0 then
              if Baudot[i + 1] = 0 then // allow triming of last 5 bits
                break
              else
                exit;
            dest^ := AnsiChar(c);
            inc(dest);
          end;
        end;
      end;
    end;
  finally
    tmp.Done(dest, result);
  end;
end;

{ --------- URL encoding/decoding }

function UrlEncode(const svar: RawUTF8): RawUTF8;
begin
  result := UrlEncode(pointer(svar));
end;

procedure _UrlEncode_Write(s, p: PByte; tab: PTextByteSet);
var
  c: cardinal;
  hex: ^TByteToWord;
begin
  hex := @TwoDigitsHexWB;
  repeat
    c := s^;
    inc(s);
    if tcURIUnreserved in tab[c] then
    begin
      p^ := c;
      inc(p);
    end
    else if c = 0 then
      exit
    else if c = 32 then
    begin
      p^ := ord('+');
      inc(p);
    end
    else
    begin
      p^ := ord('%');
      inc(p);
      PWord(p)^ := hex[c];
      inc(p, 2);
    end;
  until false;
end;

function _UrlEncode_ComputeLen(s: PByte; tab: PTextByteSet): PtrInt;
var
  c: cardinal;
begin
  result := 0;
  repeat
    c := s^;
    inc(s);
    if (tcURIUnreserved in tab[c]) or (c = 32) then
    begin
      inc(result);
      continue;
    end;
    if c = 0 then
      exit;
    inc(result, 3);
  until false;
end;

function UrlEncode(Text: PUTF8Char): RawUTF8;
begin
  result := '';
  if Text = nil then
    exit;
  FastSetString(result, nil, _UrlEncode_ComputeLen(pointer(Text), @TEXT_CHARS));
  _UrlEncode_Write(pointer(Text), pointer(result), @TEXT_BYTES);
end;

function UrlEncode(const NameValuePairs: array of const): RawUTF8;
// (['select','*','where','ID=12','offset',23,'object',aObject]);
var
  A, n: PtrInt;
  name, value: RawUTF8;
begin
  result := '';
  n := high(NameValuePairs);
  if (n > 0) and (n and 1 = 1) then
  begin
    for A := 0 to n shr 1 do
    begin
      VarRecToUTF8(NameValuePairs[A * 2], name);
      if not IsUrlValid(pointer(name)) then
        continue; // just skip invalid names
      with NameValuePairs[A * 2 + 1] do
        if VType = vtObject then
          value := ObjectToJSON(VObject, [])
        else
          VarRecToUTF8(NameValuePairs[A * 2 + 1], value);
      result := result + '&' + name + '=' + UrlEncode(value);
    end;
    result[1] := '?';
  end;
end;

function IsUrlValid(P: PUTF8Char): boolean;
var
  tab: PTextCharSet;
begin
  result := false;
  if P = nil then
    exit;
  tab := @TEXT_CHARS;
  repeat
    if tcURIUnreserved in tab[P^] then
      inc(P)
    else
      exit;
  until P^ = #0;
  result := true;
end;

function AreUrlValid(const Url: array of RawUTF8): boolean;
var
  i: integer;
begin
  result := false;
  for i := 0 to high(Url) do
    if not IsUrlValid(pointer(Url[i])) then
      exit;
  result := true;
end;

function IncludeTrailingURIDelimiter(const URI: RawByteString): RawByteString;
begin
  if (URI <> '') and (URI[length(URI)] <> '/') then
    result := URI + '/'
  else
    result := URI;
end;

function UrlDecode(const s: RawUTF8; i, len: PtrInt): RawUTF8;
var
  L: PtrInt;
  P: PUTF8Char;
  tmp: TSynTempBuffer;
begin
  result := '';
  L := PtrInt(s);
  if L = 0 then
    exit;
  L := PStrLen(L - _STRLEN)^;
  if len < 0 then
    len := L;
  if i > L then
    exit;
  dec(i);
  if len = i then
    exit;
  P := tmp.Init(len - i);  // reserve enough space for result
  while i < len do
  begin
    case s[i + 1] of
      #0:
        break; // reached end of s
      '%':
        if not HexToChar(PAnsiChar(pointer(s)) + i + 1, P) then
          P^ := s[i + 1]
        else
          inc(i, 2); // browsers may not follow the RFC (e.g. encode % as % !)
      '+':
        P^ := ' ';
    else
      P^ := s[i + 1];
    end; // case s[i] of
    inc(i);
    inc(P);
  end;
  tmp.Done(P, result);
end;

function UrlDecode(U: PUTF8Char): RawUTF8;
var
  P: PUTF8Char;
  L: integer;
  tmp: TSynTempBuffer;
begin
  result := '';
  L := StrLen(U);
  if L = 0 then
    exit;
  P := tmp.Init(L);
  repeat
    case U^ of
      #0:
        break; // reached end of URI
      '%':
        if not HexToChar(PAnsiChar(U + 1), P) then
          P^ := U^
        else
          inc(U, 2); // browsers may not follow the RFC (e.g. encode % as % !)
      '+':
        P^ := ' ';
    else
      P^ := U^;
    end; // case s[i] of
    inc(U);
    inc(P);
  until false;
  tmp.Done(P, result);
end;

function UrlDecodeNextValue(U: PUTF8Char; out Value: RawUTF8): PUTF8Char;
var
  Beg, V: PUTF8Char;
  len: PtrInt;
begin
  if U <> nil then
  begin
    // compute resulting length of value
    Beg := U;
    len := 0;
    while (U^ <> #0) and (U^ <> '&') do
    begin
      if (U^ = '%') and HexToCharValid(PAnsiChar(U + 1)) then
        inc(U, 3)
      else
        inc(U);
      inc(len);
    end;
    // decode value content
    if len <> 0 then
    begin
      FastSetString(Value, nil, len);
      V := pointer(Value);
      U := Beg;
      repeat
        if (U^ = '%') and HexToChar(PAnsiChar(U + 1), V) then
        begin
          inc(V);
          inc(U, 3);
        end
        else
        begin
          if U^ = '+' then
            V^ := ' '
          else
            V^ := U^;
          inc(V);
          inc(U);
        end;
        dec(len);
      until len = 0;
    end;
  end;
  result := U;
end;

function UrlDecodeNextName(U: PUTF8Char; out Name: RawUTF8): PUTF8Char;
var
  Beg, V: PUTF8Char;
  len: PtrInt;
begin
  result := nil;
  if U = nil then
    exit;
  // compute resulting length of name
  Beg := U;
  len := 0;
  repeat
    case U^ of
      #0:
        exit;
      '=':
        begin
          result := U + 1;
          break;
        end;
      '%':
        if (U[1] = '3') and (U[2] in ['D', 'd']) then
        begin
          result := U + 3;
          break;  // %3d means = according to the RFC
        end
        else if HexToCharValid(PAnsiChar(U + 1)) then
          inc(U, 3)
        else
          inc(U);
    else
      inc(U);
    end;
    inc(len);
  until false;
  if len = 0 then
    exit;
  // decode name content
  FastSetString(Name, nil, len);
  V := pointer(Name);
  U := Beg;
  repeat
    if (U^ = '%') and HexToChar(PAnsiChar(U + 1), V) then
    begin
      inc(V);
      inc(U, 3);
    end
    else
    begin
      if U^ = '+' then
        V^ := ' '
      else
        V^ := U^;
      inc(V);
      inc(U);
    end;
    dec(len);
  until len = 0;
end;

function UrlDecodeNextNameValue(U: PUTF8Char; var Name, Value: RawUTF8): PUTF8Char;
begin
  result := nil;
  if U = nil then
    exit;
  U := UrlDecodeNextName(U, Name);
  if U = nil then
    exit;
  U := UrlDecodeNextValue(U, Value);
  if U^ = #0 then
    result := U
  else
    result := U + 1; // jump '&' to let decode the next name=value pair
end;

function UrlDecodeValue(U: PUTF8Char; const Upper: RawUTF8; var Value: RawUTF8; Next: PPUTF8Char): boolean;
begin
  // UrlDecodeValue('select=%2A&where=LastName%3D%27M%C3%B4net%27','SELECT=',V,@U)
  // -> U^='where=...' and V='*'
  result := false; // mark value not modified by default
  if U = nil then
  begin
    if Next <> nil then
      Next^ := U;
    exit;
  end;
  if IdemPChar(U, pointer(Upper)) then
  begin
    result := true;
    inc(U, length(Upper));
    U := UrlDecodeNextValue(U, Value);
  end;
  if Next = nil then
    exit;
  while not (U^ in [#0, '&']) do
    inc(U);
  if U^ = #0 then
    Next^ := nil
  else
    Next^ := U + 1; // jump '&'
end;

function UrlDecodeInteger(U: PUTF8Char; const Upper: RawUTF8; var Value: integer; Next: PPUTF8Char): boolean;
var
  V: PtrInt;
  SignNeg: boolean;
begin
  // UrlDecodeInteger('offset=20&where=LastName%3D%27M%C3%B4net%27','OFFSET=',O,@Next)
  // -> Next^='where=...' and O=20
  result := false; // mark value not modified by default
  if U = nil then
  begin
    if Next <> nil then
      Next^ := U;
    exit;
  end;
  if IdemPChar(U, pointer(Upper)) then
  begin
    inc(U, length(Upper));
    if U^ = '-' then
    begin
      SignNeg := True;
      Inc(U);
    end
    else
      SignNeg := false;
    if U^ in ['0'..'9'] then
    begin
      V := 0;
      repeat
        V := (V * 10) + ord(U^) - 48;
        inc(U);
      until not (U^ in ['0'..'9']);
      if SignNeg then
        Value := -V
      else
        Value := V;
      result := true;
    end;
  end;
  if Next = nil then
    exit;
  while not (U^ in [#0, '&']) do
    inc(U);
  if U^ = #0 then
    Next^ := nil
  else
    Next^ := U + 1; // jump '&'
end;

function UrlDecodeCardinal(U: PUTF8Char; const Upper: RawUTF8; var Value: Cardinal; Next: PPUTF8Char): boolean;
var
  V: PtrInt;
begin
  // UrlDecodeInteger('offset=20&where=LastName%3D%27M%C3%B4net%27','OFFSET=',O,@Next)
  // -> Next^='where=...' and O=20
  result := false; // mark value not modified by default
  if U = nil then
  begin
    if Next <> nil then
      Next^ := U;
    exit;
  end;
  if IdemPChar(U, pointer(Upper)) then
  begin
    inc(U, length(Upper));
    if U^ in ['0'..'9'] then
    begin
      V := 0;
      repeat
        V := (V * 10) + ord(U^) - 48;
        inc(U);
      until not (U^ in ['0'..'9']);
      Value := V;
      result := true;
    end;
  end;
  if Next = nil then
    exit;
  while not (U^ in [#0, '&']) do
    inc(U);
  if U^ = #0 then
    Next^ := nil
  else
    Next^ := U + 1; // jump '&'
end;

function UrlDecodeInt64(U: PUTF8Char; const Upper: RawUTF8; var Value: Int64; Next: PPUTF8Char): boolean;
var
  tmp: RawUTF8;
begin
  result := UrlDecodeValue(U, Upper, tmp, Next);
  if result then
    SetInt64(pointer(tmp), Value);
end;

function UrlDecodeExtended(U: PUTF8Char; const Upper: RawUTF8; var Value: TSynExtended; Next: PPUTF8Char): boolean;
var
  tmp: RawUTF8;
  err: integer;
begin
  result := UrlDecodeValue(U, Upper, tmp, Next);
  if result then
  begin
    Value := GetExtended(pointer(tmp), err);
    if err <> 0 then
      result := false;
  end;
end;

function UrlDecodeDouble(U: PUTF8Char; const Upper: RawUTF8; var Value: double; Next: PPUTF8Char): boolean;
var
  tmp: RawUTF8;
  err: integer;
begin
  result := UrlDecodeValue(U, Upper, tmp, Next);
  if result then
  begin
    Value := GetExtended(pointer(tmp), err);
    if err <> 0 then
      result := false;
  end;
end;

function UrlDecodeNeedParameters(U, CSVNames: PUTF8Char): boolean;
var
  tmp: array[byte] of AnsiChar;
  L: integer;
  Beg: PUTF8Char;
// UrlDecodeNeedParameters('price=20.45&where=LastName%3D','price,where') will
// return TRUE
begin
  result := (CSVNames = nil);
  if result then
    exit; // no parameter to check -> success
  if U = nil then
    exit; // no input data -> error
  repeat
    L := 0;
    while (CSVNames^ <> #0) and (CSVNames^ <> ',') do
    begin
      tmp[L] := NormToUpper[CSVNames^];
      if L = high(tmp) then
        exit
      else // invalid CSV parameter
        inc(L);
      inc(CSVNames);
    end;
    if L = 0 then
      exit; // invalid CSV parameter
    PWord(@tmp[L])^ := ord('=');
    Beg := U;
    repeat
      if IdemPChar(U, tmp) then
        break;
      while not (U^ in [#0, '&']) do
        inc(U);
      if U^ = #0 then
        exit
      else // didn't find tmp in U
        inc(U); // Jump &
    until false;
    U := Beg;
    if CSVNames^ = #0 then
      Break
    else // no more parameter to check
      inc(CSVNames); // jump &
  until false;
  result := true; // all parameters found
end;


{ ************ INI Files and In-memory Access }

function IdemPChar2(table: PNormTable; p: PUTF8Char; up: PAnsiChar): boolean;
  {$ifdef HASINLINE} inline;{$endif}
var
  u: AnsiChar;
begin // here p and up are expected to be <> nil
  result := false;
  dec(PtrUInt(p), PtrUInt(up));
  repeat
    u := up^;
    if u = #0 then
      break;
    if table^[up[PtrUInt(p)]] <> u then
      exit;
    inc(up);
  until false;
  result := true;
end;

function FindSectionFirstLine(var source: PUTF8Char; search: PAnsiChar): boolean;
var
  table: PNormTable;
  charset: PTextCharSet;
begin
  result := false;
  if (source = nil) or (search = nil) then
    exit;
  table := @NormToUpperAnsi7;
  charset := @TEXT_CHARS;
  repeat
    if source^ = '[' then
    begin
      inc(source);
      result := IdemPChar2(table, source, search);
    end;
    while tcNot01013 in charset[source^] do
      inc(source);
    while tc1013 in charset[source^] do
      inc(source);
    if result then
      exit; // found
  until source^ = #0;
  source := nil;
end;

function FindSectionFirstLineW(var source: PWideChar; search: PUTF8Char): boolean;
begin
  result := false;
  if source = nil then
    exit;
  repeat
    if source^ = '[' then
    begin
      inc(source);
      result := IdemPCharW(source, search);
    end;
    while not (cardinal(source^) in [0, 10, 13]) do
      inc(source);
    while cardinal(source^) in [10, 13] do
      inc(source);
    if result then
      exit; // found
  until source^ = #0;
  source := nil;
end;

function FindIniNameValue(P: PUTF8Char; UpperName: PAnsiChar): RawUTF8;
var
  u, PBeg: PUTF8Char;
  by4: cardinal;
  {$ifdef CPUX86NOTPIC}
  table: TNormTable absolute NormToUpperAnsi7;
  {$else}
  table: PNormTable;
  {$endif}
begin // expect UpperName as 'NAME='
  if (P <> nil) and (P^ <> '[') and (UpperName <> nil) then
  begin
    {$ifndef CPUX86NOTPIC} table := @NormToUpperAnsi7; {$endif}
    PBeg := nil;
    u := P;
    repeat
      while u^ = ' ' do
        inc(u); // trim left ' '
      if u^ = #0 then
        break;
      if table[u^] = UpperName[0] then
        PBeg := u;
      repeat
        by4 := PCardinal(u)^;
        if ToByte(by4) > 13 then
          if ToByte(by4 shr 8) > 13 then
            if ToByte(by4 shr 16) > 13 then
              if ToByte(by4 shr 24) > 13 then
              begin
                inc(u, 4);
                continue;
              end
              else
                inc(u, 3)
            else
              inc(u, 2)
          else
            inc(u);
        if u^ in [#0, #10, #13] then
          break;
        inc(u);
      until false;
      if PBeg <> nil then
      begin
        inc(PBeg);
        P := u;
        u := pointer(UpperName + 1);
        repeat
          if u^ <> #0 then
            if table[PBeg^] <> u^ then
              break
            else
            begin
              inc(u);
              inc(PBeg);
            end
          else
          begin
            FastSetString(result, PBeg, P - PBeg);
            exit;
          end;
        until false;
        PBeg := nil;
        u := P;
      end;
      if u^ = #13 then
        inc(u);
      if u^ = #10 then
        inc(u);
    until u^ in [#0, '['];
  end;
  result := '';
end;

function ExistsIniName(P: PUTF8Char; UpperName: PAnsiChar): boolean;
var
  table: PNormTable;
begin
  result := false;
  if (P <> nil) and (P^ <> '[') then
  begin
    table := @NormToUpperAnsi7;
    repeat
      if P^ = ' ' then
      begin
        repeat
          inc(P)
        until P^ <> ' '; // trim left ' '
        if P^ = #0 then
          break;
      end;
      if IdemPChar2(table, P, UpperName) then
      begin
        result := true;
        exit;
      end;
      repeat
        if P[0] > #13 then
          if P[1] > #13 then
            if P[2] > #13 then
              if P[3] > #13 then
              begin
                inc(P, 4);
                continue;
              end
              else
                inc(P, 3)
            else
              inc(P, 2)
          else
            inc(P);
        case P^ of
          #0:
            exit;
          #10:
            begin
              inc(P);
              break;
            end;
          #13:
            begin
              if P[1] = #10 then
                inc(P, 2)
              else
                inc(P);
              break;
            end;
        else
          inc(P);
        end;
      until false;
    until P^ = '[';
  end;
end;

function ExistsIniNameValue(P: PUTF8Char; const UpperName: RawUTF8;
  const UpperValues: array of PAnsiChar): boolean;
var
  PBeg: PUTF8Char;
  table: PNormTable;
begin
  result := true;
  if (high(UpperValues) >= 0) and (UpperName <> '') then
  begin
    table := @NormToUpperAnsi7;
    while (P <> nil) and (P^ <> '[') do
    begin
      if P^ = ' ' then
        repeat
          inc(P)
        until P^ <> ' '; // trim left ' '
      PBeg := P;
      if IdemPChar2(table, PBeg, pointer(UpperName)) then
      begin
        inc(PBeg, length(UpperName));
        if IdemPCharArray(PBeg, UpperValues) >= 0 then
          exit; // found one value
        break;
      end;
      P := GotoNextLine(P);
    end;
  end;
  result := false;
end;

function GetSectionContent(SectionFirstLine: PUTF8Char): RawUTF8;
var
  PBeg: PUTF8Char;
begin
  PBeg := SectionFirstLine;
  while (SectionFirstLine <> nil) and (SectionFirstLine^ <> '[') do
    SectionFirstLine := GotoNextLine(SectionFirstLine);
  if SectionFirstLine = nil then
    result := PBeg
  else
    FastSetString(result, PBeg, SectionFirstLine - PBeg);
end;

function GetSectionContent(const Content, SectionName: RawUTF8): RawUTF8;
var
  P: PUTF8Char;
  UpperSection: array[byte] of AnsiChar;
begin
  P := pointer(Content);
  PWord(UpperCopy255(UpperSection{%H-}, SectionName))^ := ord(']');
  if FindSectionFirstLine(P, UpperSection) then
    result := GetSectionContent(P)
  else
    result := '';
end;

function DeleteSection(var Content: RawUTF8; const SectionName: RawUTF8;
  EraseSectionHeader: boolean): boolean;
var
  P: PUTF8Char;
  UpperSection: array[byte] of AnsiChar;
begin
  result := false; // no modification
  P := pointer(Content);
  PWord(UpperCopy255(UpperSection{%H-}, SectionName))^ := ord(']');
  if FindSectionFirstLine(P, UpperSection) then
    result := DeleteSection(P, Content, EraseSectionHeader);
end;

function DeleteSection(SectionFirstLine: PUTF8Char; var Content: RawUTF8;
  EraseSectionHeader: boolean): boolean;
var
  PEnd: PUTF8Char;
  IndexBegin: PtrInt;
begin
  result := false;
  PEnd := SectionFirstLine;
  if EraseSectionHeader then // erase [Section] header line
    while (PtrUInt(SectionFirstLine) > PtrUInt(Content)) and (SectionFirstLine^ <> '[') do
      dec(SectionFirstLine);
  while (PEnd <> nil) and (PEnd^ <> '[') do
    PEnd := GotoNextLine(PEnd);
  IndexBegin := SectionFirstLine - pointer(Content);
  if IndexBegin = 0 then
    exit; // no modification
  if PEnd = nil then
    SetLength(Content, IndexBegin)
  else
    delete(Content, IndexBegin + 1, PEnd - SectionFirstLine);
  result := true; // Content was modified
end;

procedure ReplaceSection(SectionFirstLine: PUTF8Char; var Content: RawUTF8;
  const NewSectionContent: RawUTF8);
var
  PEnd: PUTF8Char;
  IndexBegin: PtrInt;
begin
  if SectionFirstLine = nil then
    exit;
  // delete existing [Section] content
  PEnd := SectionFirstLine;
  while (PEnd <> nil) and (PEnd^ <> '[') do
    PEnd := GotoNextLine(PEnd);
  IndexBegin := SectionFirstLine - pointer(Content);
  if PEnd = nil then
    SetLength(Content, IndexBegin)
  else
    delete(Content, IndexBegin + 1, PEnd - SectionFirstLine);
  // insert section content
  insert(NewSectionContent, Content, IndexBegin + 1);
end;

procedure ReplaceSection(var Content: RawUTF8; const SectionName, NewSectionContent: RawUTF8);
var
  UpperSection: array[byte] of AnsiChar;
  P: PUTF8Char;
begin
  P := pointer(Content);
  PWord(UpperCopy255(UpperSection{%H-}, SectionName))^ := ord(']');
  if FindSectionFirstLine(P, UpperSection) then
    ReplaceSection(P, Content, NewSectionContent)
  else
    Content := Content + '[' + SectionName + ']'#13#10 + NewSectionContent;
end;

function FindIniNameValueInteger(P: PUTF8Char; const UpperName: RawUTF8): PtrInt;
var
  table: PNormTable;
begin
  result := 0;
  if (P = nil) or (UpperName = '') then
    exit;
  table := @NormToUpperAnsi7;
  repeat
    if IdemPChar2(table, P, pointer(UpperName)) then
      break;
    P := GotoNextLine(P);
    if P = nil then
      exit;
  until false;
  result := GetInteger(P + length(UpperName));
end;

function FindIniEntry(const Content, Section, Name: RawUTF8): RawUTF8;
var
  P: PUTF8Char;
  UpperSection, UpperName: array[byte] of AnsiChar;
begin
  result := '';
  P := pointer(Content);
  if P = nil then
    exit;
  // UpperName := UpperCase(Name)+'=';
  PWord(UpperCopy255(UpperName{%H-}, Name))^ := ord('=');
  if Section = '' then
    // find the Name= entry before any [Section]
    result := FindIniNameValue(P, UpperName)
  else
  begin
    // find the Name= entry in the specified [Section]
    PWord(UpperCopy255(UpperSection{%H-}, Section))^ := ord(']');
    if FindSectionFirstLine(P, UpperSection) then
      result := FindIniNameValue(P, UpperName);
  end;
end;

function FindWinAnsiIniEntry(const Content, Section, Name: RawUTF8): RawUTF8;
begin
  result := WinAnsiToUtf8(WinAnsiString(FindIniEntry(Content, Section, Name)));
end;

function FindIniEntryInteger(const Content, Section, Name: RawUTF8): integer;
begin
  result := GetInteger(pointer(FindIniEntry(Content, Section, Name)));
end;

function FindIniEntryFile(const FileName: TFileName; const Section, Name: RawUTF8): RawUTF8;
var
  Content: RawUTF8;
begin
  Content := StringFromFile(FileName);
  if Content = '' then
    result := ''
  else
    result := FindIniEntry(Content, Section, Name);
end;

function UpdateIniNameValueInternal(var Content: RawUTF8; const NewValue, NewValueCRLF: RawUTF8;
  var P: PUTF8Char; UpperName: PAnsiChar; UpperNameLength: integer): boolean;
var
  PBeg: PUTF8Char;
  i: integer;
begin
  if UpperName <> nil then
    while (P <> nil) and (P^ <> '[') do
    begin
      while P^ = ' ' do
        inc(P);   // trim left ' '
      PBeg := P;
      P := GotoNextLine(P);
      if IdemPChar2(@NormToUpperAnsi7, PBeg, UpperName) then
      begin
       // update Name=Value entry
        result := true;
        inc(PBeg, UpperNameLength);
        i := (PBeg - pointer(Content)) + 1;
        if (i = length(NewValue)) and CompareMem(PBeg, pointer(NewValue), i) then
          exit; // new Value is identical to the old one -> no change
        if P = nil then // avoid last line (P-PBeg) calculation error
          SetLength(Content, i - 1)
        else
          delete(Content, i, P - PBeg); // delete old Value
        insert(NewValueCRLF, Content, i); // set new value
        exit;
      end;
    end;
  result := false;
end;

function UpdateIniNameValue(var Content: RawUTF8; const Name, UpperName, NewValue: RawUTF8): boolean;
var
  P: PUTF8Char;
begin
  if UpperName = '' then
    result := false
  else
  begin
    P := pointer(Content);
    result := UpdateIniNameValueInternal(Content, NewValue, NewValue + #13#10,
      P, pointer(UpperName), length(UpperName));
    if result or (Name = '') then
      exit;
    if Content <> '' then
      Content := Content + #13#10;
    Content := Content + Name + NewValue;
    result := true;
  end;
end;

procedure UpdateIniEntry(var Content: RawUTF8; const Section, Name, Value: RawUTF8);
const
  CRLF = #13#10;
var
  P: PUTF8Char;
  SectionFound: boolean;
  i, UpperNameLength: PtrInt;
  V: RawUTF8;
  UpperSection, UpperName: array[byte] of AnsiChar;
begin
  UpperNameLength := length(Name);
  PWord(UpperCopy255Buf(UpperName{%H-}, pointer(Name), UpperNameLength))^ := ord('=');
  inc(UpperNameLength);
  V := Value + CRLF;
  P := pointer(Content);
  // 1. find Section, and try update within it
  if Section = '' then
    SectionFound := true // find the Name= entry before any [Section]
  else
  begin
    PWord(UpperCopy255(UpperSection{%H-}, Section))^ := ord(']');
    SectionFound := FindSectionFirstLine(P, UpperSection);
  end;
  if SectionFound and
     UpdateIniNameValueInternal(Content, Value, V, P, @UpperName, UpperNameLength) then
      exit;
  // 2. section or Name= entry not found: add Name=Value
  V := Name + '=' + V;
  if not SectionFound then
    // create not existing [Section]
    V := '[' + Section + (']' + CRLF) + V;
  // insert Name=Value at P^ (end of file or end of [Section])
  if P = nil then
    // insert at end of file
    Content := Content + V
  else
  begin
    // insert at end of [Section]
    i := (P - pointer(Content)) + 1;
    insert(V, Content, i);
  end;
end;

procedure UpdateIniEntryFile(const FileName: TFileName; const Section, Name, Value: RawUTF8);
var
  Content: RawUTF8;
begin
  Content := StringFromFile(FileName);
  UpdateIniEntry(Content, Section, Name, Value);
  FileFromString(Content, FileName);
end;



{ ************ TAlgoCompress Compression/Decompression Classes }

{ TAlgoCompress }

const
  COMPRESS_STORED = #0;
  COMPRESS_SYNLZ = 1;

var
  SynCompressAlgos: TSynObjectList;

constructor TAlgoCompress.Create;
var
  existing: TAlgoCompress;
begin
  inherited Create;
  existing := Algo(AlgoID);
  if existing <> nil then
    raise ESynException.CreateUTF8('%.Create: AlgoID=% already registered by %',
      [self, AlgoID, existing.ClassType]);
  SynCompressAlgos.Add(self);
end;

class function TAlgoCompress.Algo(const Comp: RawByteString): TAlgoCompress;
begin
  result := Algo(Pointer(Comp), Length(Comp));
end;

class function TAlgoCompress.Algo(const Comp: TByteDynArray): TAlgoCompress;
begin
  result := Algo(Pointer(Comp), Length(Comp));
end;

class function TAlgoCompress.Algo(Comp: PAnsiChar; CompLen: integer): TAlgoCompress;
begin
  if (Comp <> nil) and (CompLen > 9) then
    if ord(Comp[4]) <= 1 then // inline-friendly Comp[4]<=COMPRESS_SYNLZ
      result := AlgoSynLZ
    else // COMPRESS_STORED is also handled as SynLZ
      result := Algo(ord(Comp[4]))
  else
    result := nil;
end;

class function TAlgoCompress.Algo(Comp: PAnsiChar; CompLen: integer;
  out IsStored: boolean): TAlgoCompress;
begin
  if (Comp <> nil) and (CompLen > 9) then
  begin
    IsStored := Comp[4] = COMPRESS_STORED;
    result := Algo(ord(Comp[4]));
  end
  else
  begin
    IsStored := false;
    result := nil;
  end;
end;

class function TAlgoCompress.Algo(AlgoID: byte): TAlgoCompress;
var
  i: integer;
  ptr: ^TAlgoCompress;
begin
  if AlgoID <= COMPRESS_SYNLZ then // COMPRESS_STORED is handled as SynLZ
    result := AlgoSynLZ
  else
  begin
    if SynCompressAlgos <> nil then
    begin
      ptr := pointer(SynCompressAlgos.List);
      inc(ptr); // ignore List[0] = AlgoSynLZ
      for i := 2 to SynCompressAlgos.Count do
        if ptr^.AlgoID = AlgoID then
        begin
          result := ptr^;
          exit;
        end
        else
          inc(ptr);
    end;
    result := nil;
  end;
end;

class function TAlgoCompress.UncompressedSize(const Comp: RawByteString): integer;
begin
  result := Algo(Comp).DecompressHeader(pointer(Comp), length(Comp));
end;

function TAlgoCompress.AlgoName: TShort16;
var
  s: PShortString;
  i: integer;
begin
  if self = nil then
    result := 'none'
  else
  begin
    s := ClassNameShort(self);
    if IdemPChar(@s^[1], 'TALGO') then
    begin
      result[0] := AnsiChar(ord(s^[0]) - 5);
      inc(PByte(s), 5);
    end
    else
      result[0] := s^[0];
    if result[0] > #16 then
      result[0] := #16;
    for i := 1 to ord(result[0]) do
      result[i] := NormToLower[s^[i]];
  end;
end;

function TAlgoCompress.AlgoHash(Previous: cardinal; Data: pointer; DataLen: integer): cardinal;
begin
  result := crc32c(Previous, Data, DataLen);
end;

function TAlgoCompress.Compress(const Plain: RawByteString;
  CompressionSizeTrigger: integer; CheckMagicForCompressed: boolean;
  BufferOffset: integer): RawByteString;
begin
  result := Compress(pointer(Plain), Length(Plain), CompressionSizeTrigger,
    CheckMagicForCompressed, BufferOffset);
end;

function TAlgoCompress.Compress(Plain: PAnsiChar; PlainLen: integer;
  CompressionSizeTrigger: integer; CheckMagicForCompressed: boolean;
  BufferOffset: integer): RawByteString;
var
  len: integer;
  R: PAnsiChar;
  crc: cardinal;
  tmp: array[0..16383] of AnsiChar;  // big enough to resize result in-place
begin
  if (self = nil) or (PlainLen = 0) or (Plain = nil) then
  begin
    result := '';
    exit;
  end;
  crc := AlgoHash(0, Plain, PlainLen);
  if (PlainLen < CompressionSizeTrigger) or
     (CheckMagicForCompressed and IsContentCompressed(Plain, PlainLen)) then
  begin
    SetString(result, nil, PlainLen + BufferOffset + 9);
    R := pointer(result);
    inc(R, BufferOffset);
    PCardinal(R)^ := crc;
    R[4] := COMPRESS_STORED;
    PCardinal(R + 5)^ := crc;
    MoveFast(Plain^, R[9], PlainLen);
  end
  else
  begin
    len := CompressDestLen(PlainLen) + BufferOffset;
    if len > SizeOf(tmp) then
    begin
      SetString(result, nil, len);
      R := pointer(result);
    end
    else
      R := @tmp;
    inc(R, BufferOffset);
    PCardinal(R)^ := crc;
    len := AlgoCompress(Plain, PlainLen, R + 9);
    if len + 64 >= PlainLen then
    begin // store if compression was not worth it
      R[4] := COMPRESS_STORED;
      PCardinal(R + 5)^ := crc;
      MoveFast(Plain^, R[9], PlainLen);
      len := PlainLen;
    end
    else
    begin
      R[4] := AnsiChar(AlgoID);
      PCardinal(R + 5)^ := AlgoHash(0, R + 9, len);
    end;
    if R = @tmp[BufferOffset] then
      SetString(result, PAnsiChar(@tmp), len + BufferOffset + 9)
    else
      SetLength(result, len + BufferOffset + 9); // MM may not move the data
  end;
end;

function TAlgoCompress.Compress(Plain, Comp: PAnsiChar; PlainLen, CompLen: integer;
  CompressionSizeTrigger: integer; CheckMagicForCompressed: boolean): integer;
var
  len: integer;
begin
  result := 0;
  if (self = nil) or (PlainLen = 0) or (CompLen < PlainLen + 9) then
    exit;
  PCardinal(Comp)^ := AlgoHash(0, Plain, PlainLen);
  if (PlainLen >= CompressionSizeTrigger) and
     not (CheckMagicForCompressed and IsContentCompressed(Plain, PlainLen)) then
  begin
    len := CompressDestLen(PlainLen);
    if CompLen < len then
      exit;
    len := AlgoCompress(Plain, PlainLen, Comp + 9);
    if len < PlainLen then
    begin
      Comp[4] := AnsiChar(AlgoID);
      PCardinal(Comp + 5)^ := AlgoHash(0, Comp + 9, len);
      result := len + 9;
      exit;
    end;
  end;
  Comp[4] := COMPRESS_STORED;
  PCardinal(Comp + 5)^ := PCardinal(Comp)^;
  MoveFast(Plain^, Comp[9], PlainLen);
  result := PlainLen + 9;
end;

function TAlgoCompress.CompressDestLen(PlainLen: integer): integer;
begin
  if self = nil then
    result := 0
  else
    result := AlgoCompressDestLen(PlainLen) + 9;
end;

function TAlgoCompress.CompressToBytes(Plain: PAnsiChar; PlainLen: integer;
  CompressionSizeTrigger: integer; CheckMagicForCompressed: boolean): TByteDynArray;
var
  len: integer;
  R: PAnsiChar;
  crc: cardinal;
begin
  Finalize(result);
  if (self = nil) or (PlainLen = 0) then
    exit;
  crc := AlgoHash(0, Plain, PlainLen);
  if PlainLen < CompressionSizeTrigger then
  begin
    SetLength(result, PlainLen + 9);
    R := pointer(result);
    PCardinal(R)^ := crc;
    R[4] := COMPRESS_STORED;
    PCardinal(R + 5)^ := crc;
    MoveFast(Plain^, R[9], PlainLen);
  end
  else
  begin
    SetLength(result, CompressDestLen(PlainLen));
    R := pointer(result);
    PCardinal(R)^ := crc;
    len := AlgoCompress(Plain, PlainLen, R + 9);
    if len >= PlainLen then
    begin // store if compression not worth it
      R[4] := COMPRESS_STORED;
      PCardinal(R + 5)^ := crc;
      MoveFast(Plain^, R[9], PlainLen);
      len := PlainLen;
    end
    else
    begin
      R[4] := AnsiChar(AlgoID);
      PCardinal(R + 5)^ := AlgoHash(0, R + 9, len);
    end;
    SetLength(result, len + 9);
  end;
end;

function TAlgoCompress.CompressToBytes(const Plain: RawByteString;
  CompressionSizeTrigger: integer; CheckMagicForCompressed: boolean): TByteDynArray;
begin
  result := CompressToBytes(pointer(Plain), Length(Plain),
    CompressionSizeTrigger, CheckMagicForCompressed);
end;

function TAlgoCompress.Decompress(const Comp: TByteDynArray): RawByteString;
begin
  Decompress(pointer(Comp), length(Comp), result);
end;

procedure TAlgoCompress.Decompress(Comp: PAnsiChar; CompLen: integer;
  out result: RawByteString; Load: TAlgoCompressLoad; BufferOffset: integer);
var
  len: integer;
  dec: PAnsiChar;
begin
  len := DecompressHeader(Comp, CompLen, Load);
  if len = 0 then
    exit;
  SetString(result, nil, len + BufferOffset);
  dec := pointer(result);
  if not DecompressBody(Comp, dec + BufferOffset, CompLen, len, Load) then
    result := '';
end;

function TAlgoCompress.Decompress(const Comp: RawByteString; Load: TAlgoCompressLoad;
  BufferOffset: integer): RawByteString;
begin
  Decompress(pointer(Comp), length(Comp), result, Load, BufferOffset);
end;

function TAlgoCompress.TryDecompress(const Comp: RawByteString; out Dest: RawByteString;
  Load: TAlgoCompressLoad): boolean;
var
  len: integer;
begin
  result := Comp = '';
  if result then
    exit;
  len := DecompressHeader(pointer(Comp), length(Comp), Load);
  if len = 0 then
    exit; // invalid crc32c
  SetString(Dest, nil, len);
  if DecompressBody(pointer(Comp), pointer(Dest), length(Comp), len, Load) then
    result := true
  else
    Dest := '';
end;

function TAlgoCompress.Decompress(const Comp: RawByteString; out PlainLen: integer;
  var tmp: RawByteString; Load: TAlgoCompressLoad): pointer;
begin
  result := Decompress(pointer(Comp), length(Comp), PlainLen, tmp, Load);
end;

function TAlgoCompress.Decompress(Comp: PAnsiChar; CompLen: integer;
  out PlainLen: integer; var tmp: RawByteString; Load: TAlgoCompressLoad): pointer;
begin
  result := nil;
  PlainLen := DecompressHeader(Comp, CompLen, Load);
  if PlainLen = 0 then
    exit;
  if Comp[4] = COMPRESS_STORED then
    result := Comp + 9
  else
  begin
    if PlainLen > length(tmp) then
      SetString(tmp, nil, PlainLen);
    if DecompressBody(Comp, pointer(tmp), CompLen, PlainLen, Load) then
      result := pointer(tmp);
  end;
end;

function TAlgoCompress.DecompressPartial(Comp, Partial: PAnsiChar;
  CompLen, PartialLen, PartialLenMax: integer): integer;
var
  BodyLen: integer;
begin
  result := 0;
  if (self = nil) or (CompLen <= 9) or (Comp = nil) or
     (PartialLenMax < PartialLen) then
    exit;
  if Comp[4] = COMPRESS_STORED then
    if PCardinal(Comp)^ = PCardinal(Comp + 5)^ then
      BodyLen := CompLen - 9
    else
      exit
  else if Comp[4] = AnsiChar(AlgoID) then
    BodyLen := AlgoDecompressDestLen(Comp + 9)
  else
    exit;
  if PartialLen > BodyLen then
    PartialLen := BodyLen;
  if Comp[4] = COMPRESS_STORED then
    MoveFast(Comp[9], Partial[0], PartialLen)
  else if AlgoDecompressPartial(Comp + 9, CompLen - 9,
           Partial, PartialLen, PartialLenMax) < PartialLen then
    exit;
  result := PartialLen;
end;

function TAlgoCompress.DecompressHeader(Comp: PAnsiChar; CompLen: integer;
  Load: TAlgoCompressLoad): integer;
begin
  result := 0;
  if (self = nil) or (CompLen <= 9) or (Comp = nil) or
     ((Load <> aclNoCrcFast) and
      (AlgoHash(0, Comp + 9, CompLen - 9) <> PCardinal(Comp + 5)^)) then
    exit;
  if Comp[4] = COMPRESS_STORED then
  begin
    if PCardinal(Comp)^ = PCardinal(Comp + 5)^ then
      result := CompLen - 9;
  end
  else if Comp[4] = AnsiChar(AlgoID) then
    result := AlgoDecompressDestLen(Comp + 9);
end;

function TAlgoCompress.DecompressBody(Comp, Plain: PAnsiChar;
  CompLen, PlainLen: integer; Load: TAlgoCompressLoad): boolean;
begin
  result := false;
  if (self = nil) or (PlainLen <= 0) then
    exit;
  if Comp[4] = COMPRESS_STORED then
    MoveFast(Comp[9], Plain[0], PlainLen)
  else if Comp[4] = AnsiChar(AlgoID) then
    case Load of
      aclNormal:
        if (AlgoDecompress(Comp + 9, CompLen - 9, Plain) <> PlainLen) or
           (AlgoHash(0, Plain, PlainLen) <> PCardinal(Comp)^) then
          exit;
      aclSafeSlow:
        if (AlgoDecompressPartial(Comp + 9, CompLen - 9,
            Plain, PlainLen, PlainLen) <> PlainLen) or
           (AlgoHash(0, Plain, PlainLen) <> PCardinal(Comp)^) then
          exit;
      aclNoCrcFast:
        if (AlgoDecompress(Comp + 9, CompLen - 9, Plain) <> PlainLen) then
          exit;
    end;
  result := true;
end;


{ TAlgoSynLZ }

function TAlgoSynLZ.AlgoID: byte;
begin
  result := COMPRESS_SYNLZ; // =1
end;

function TAlgoSynLZ.AlgoCompress(Plain: pointer; PlainLen: integer; Comp: pointer): integer;
begin
  result := SynLZcompress1(Plain, PlainLen, Comp);
end;

function TAlgoSynLZ.AlgoCompressDestLen(PlainLen: integer): integer;
begin
  result := SynLZcompressdestlen(PlainLen);
end;

function TAlgoSynLZ.AlgoDecompress(Comp: pointer; CompLen: integer; Plain: pointer): integer;
begin
  result := SynLZdecompress1(Comp, CompLen, Plain);
end;

function TAlgoSynLZ.AlgoDecompressDestLen(Comp: pointer): integer;
begin
  result := SynLZdecompressdestlen(Comp);
end;

function TAlgoSynLZ.AlgoDecompressPartial(Comp: pointer; CompLen: integer;
  Partial: pointer; PartialLen, PartialLenMax: integer): integer;
begin
  result := SynLZdecompress1partial(Comp, CompLen, Partial, PartialLen);
end;


{ TAlgoCompressWithNoDestLen }

function TAlgoCompressWithNoDestLen.AlgoCompress(Plain: pointer; PlainLen: integer;
  Comp: pointer): integer;
begin
  Comp := ToVarUInt32(PlainLen, Comp); // deflate don't store PlainLen
  result := RawProcess(Plain, Comp, PlainLen, AlgoCompressDestLen(PlainLen), 0, doCompress);
  if result > 0 then
    inc(result, ToVarUInt32Length(PlainLen));
end;

function TAlgoCompressWithNoDestLen.AlgoDecompress(Comp: pointer;
  CompLen: integer; Plain: pointer): integer;
var
  start: PAnsiChar;
begin
  start := Comp;
  result := FromVarUInt32(PByte(Comp));
  if RawProcess(Comp, Plain, CompLen + (start - Comp), result, 0, doUnCompress) <> result then
    result := 0;
end;

function TAlgoCompressWithNoDestLen.AlgoDecompressDestLen(Comp: pointer): integer;
begin
  if Comp = nil then
    result := 0
  else
    result := FromVarUInt32(PByte(Comp));
end;

function TAlgoCompressWithNoDestLen.AlgoDecompressPartial(Comp: pointer;
  CompLen: integer; Partial: pointer; PartialLen, PartialLenMax: integer): integer;
var
  start: PAnsiChar;
begin
  start := Comp;
  result := FromVarUInt32(PByte(Comp));
  if PartialLenMax > result then
    PartialLenMax := result;
  result := RawProcess(Comp, Partial, CompLen + (start - Comp),
    PartialLen, PartialLenMax, doUncompressPartial);
end;



{ ************ RawUTF8 String Values Interning }

{  TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO }

{  TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO }


{ ************ TSynNameValue Name/Value Storage }

{  TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO }

{  TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO }



{ ********** RTTI Values Binary Serialization and Comparison }

function _BS_Ord(Data: pointer; Dest: TBufferWriter; Info: PRttiInfo): PtrInt;
begin
  result := ORDTYPE_SIZE[Info^.RttiOrd];
  Dest.Write(Data, result);
end;

function _BL_Ord(Data: pointer; var Source: TFastReader; Info: PRttiInfo): PtrInt;
begin
  result := ORDTYPE_SIZE[Info^.RttiOrd];
  Source.Copy(Data, result);
end;

function _BC_Ord(A, B: pointer; Info: PRttiInfo; out Compared: integer): PtrInt;
var
  ro: TRttiOrd;
begin
  ro := Info^.RttiOrd;
  case ro of // branchless comparison
    roSByte:
      Compared := ord(PShortInt(A)^ > PShortInt(B)^) - ord(PShortInt(A)^ < PShortInt(B)^);
    roUByte:
      Compared := ord(PByte(A)^ > PByte(B)^) - ord(PByte(A)^ < PByte(B)^);
    roSWord:
      Compared := ord(PSmallInt(A)^ > PSmallInt(B)^) - ord(PSmallInt(A)^ < PSmallInt(B)^);
    roUWord:
      Compared := ord(PWord(A)^ > PWord(B)^) - ord(PWord(A)^ < PWord(B)^);
    roSLong:
      Compared := ord(PInteger(A)^ > PInteger(B)^) - ord(PInteger(A)^ < PInteger(B)^);
    roULong:
      Compared := ord(PCardinal(A)^ > PCardinal(B)^) - ord(PCardinal(A)^ < PCardinal(B)^);
    {$ifdef FPC_NEWRTTI}
    roSQWord:
      Compared := ord(PInt64(A)^ > PInt64(B)^) - ord(PInt64(A)^ < PInt64(B)^);
    roUQWord:
      Compared := ord(PQWord(A)^ > PQWord(B)^) - ord(PQWord(A)^ < PQWord(B)^);
    {$endif}
  end;
  result := ORDTYPE_SIZE[ro];
end;

function _BS_Float(Data: pointer; Dest: TBufferWriter; Info: PRttiInfo): PtrInt;
begin
  result := FLOATTYPE_SIZE[Info^.RttiFloat];
  Dest.Write(Data, result);
end;

function _BL_Float(Data: pointer; var Source: TFastReader; Info: PRttiInfo): PtrInt;
begin
  result := FLOATTYPE_SIZE[Info^.RttiFloat];
  Source.Copy(Data, result);
end;

function _BC_Float(A, B: pointer; Info: PRttiInfo; out Compared: integer): PtrInt;
var
  rf: TRttiFloat;
begin
  rf := Info^.RttiFloat;
  case rf of // branchless comparison
    rfSingle:
      Compared := ord(PSingle(A)^ > PSingle(B)^) - ord(PSingle(A)^ < PSingle(B)^);
    rfDouble:
      Compared := ord(PDouble(A)^ > PDouble(B)^) - ord(PDouble(A)^ < PDouble(B)^);
    rfExtended:
      Compared := ord(PExtended(A)^ > PExtended(B)^) - ord(PExtended(A)^ < PExtended(B)^);
    rfComp, rfCurr:
      Compared := ord(PInt64(A)^ > PInt64(B)^) - ord(PInt64(A)^ < PInt64(B)^);
  end;
  result := FLOATTYPE_SIZE[rf];
end;

function _BS_64(Data: PInt64; Dest: TBufferWriter; Info: PRttiInfo): PtrInt;
begin
  {$ifdef CPU32}
  Dest.Write8(Data);
  {$else}
  Dest.WriteI64(Data^);
  {$endif CPU32}
  result := 8;
end;

function _BL_64(Data: PQWord; var Source: TFastReader; Info: PRttiInfo): PtrInt;
begin
  Data^ := Source.Next8;
  result := 8;
end;

function _BC_64(A, B: pointer; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  if Info^.IsQWord then
    Compared := ord(PQWord(A)^ > PQWord(B)^) - ord(PQWord(A)^ < PQWord(B)^)
  else
    Compared := ord(PInt64(A)^ > PInt64(B)^) - ord(PInt64(A)^ < PInt64(B)^);
  result := 8;
end;

function _BS_String(Data: PAnsiChar; Dest: TBufferWriter; Info: PRttiInfo): PtrInt;
var
  Len: TStrLen;
begin
  Data := PPointer(Data)^;
  if Data = nil then
    Len := 0
  else
  begin
    Len := PStrLen(Data - _STRLEN)^;
    {$ifdef HASVARUSTRING}
    if Info^.Kind = rkUString then
      Len := Len * 2; // UnicodeString length in WideChars
    {$endif HASVARUSTRING}
  end;
  Dest.WriteVar(Data, Len);
  result := SizeOf(pointer);
end;

function _BL_LString(Data: PRawByteString; var Source: TFastReader; Info: PRttiInfo): PtrInt;
begin
  with Source.VarBlob do
    {$ifdef HASCODEPAGE}
    FastSetStringCP(Data^, Ptr, Len, Info^.AnsiStringCodePageStored);
    {$else}
    SetString(Data^, Ptr, Len);
    {$endif HASCODEPAGE}
  result := SizeOf(pointer);
end;

{$ifdef HASVARUSTRING}

function _BL_UString(Data: PUnicodeString; var Source: TFastReader; Info: PRttiInfo): PtrInt;
begin
  with Source.VarBlob do
    SetString(Data^, PWideChar(Ptr), Len shr 1); // length in bytes was stored
  result := SizeOf(pointer);
end;

{$endif HASVARUSTRING}

function _BS_WString(Data: PWideString; Dest: TBufferWriter; Info: PRttiInfo): PtrInt;
begin // PStrLen() doesn't match WideString header
  Dest.WriteVar(pointer(Data^), length(Data^) * 2);
  result := SizeOf(pointer);
end;

function _BL_WString(Data: PWideString; var Source: TFastReader; Info: PRttiInfo): PtrInt;
begin
  with Source.VarBlob do
    SetString(Data^, PWideChar(Ptr), Len shr 1); // length in bytes was stored
  result := SizeOf(pointer);
end;

function _BC_PUTF8Char(A, B: PUTF8Char; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  compared := StrComp(A, B);
  result := SizeOf(pointer);
end;

function _BC_PWideChar(A, B: PWideChar; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  compared := StrCompW(A, B);
  result := SizeOf(pointer);
end;

function _BCI_PUTF8Char(A, B: PUTF8Char; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  compared := StrIComp(A, B);
  result := SizeOf(pointer);
end;

function _BCI_PWideChar(A, B: PWideChar; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  compared := AnsiICompW(A, B);
  result := SizeOf(pointer);
end;

function DelphiType(Info: PRttiInfo): integer; {$ifdef HASINLINE} inline; {$endif}
begin // compatible with legacy TDynArray.SaveTo() format
  if Info = nil then
    result := 0
  else
    {$ifdef FPC}
    result := ord(FPCTODELPHI[Info^.Kind]);
    {$else}
    result := ord(Info^.Kind);
    {$endif FPC}
end;

procedure DynArraySave(Data: PAnsiChar; ExternalCount: PInteger;
  Dest: TBufferWriter; Info: PRttiInfo);
var
  n, itemsize: PtrInt;
  sav: TRttiBinarySave;
begin
  Info := Info^.DynArrayItemType(itemsize);
  Dest.Write1(0); // no stored itemsize for 32-bit/64-bit compatibility
  Dest.Write1(DelphiType(Info));
  Data := PPointer(Data)^; // de-reference pointer to array data
  if Data = nil then
    Dest.Write1(0) // store dynamic array count of 0
  else
  begin
    if ExternalCount <> nil then
      n := ExternalCount^ // e.g. from TDynArray with external count
    else
      n := PDynArrayRec(Data - SizeOf(TDynArrayRec))^.length;
    Dest.WriteVarUInt32(n);
    Dest.Write4(0); // warning: we don't store any Hash32 checksum any more
    if Info = nil then
      Dest.Write(Data, itemsize * n)
    else
    begin
      sav := _BINARYSAVE[Info^.Kind];
      if Assigned(sav) then // paranoid check
        repeat
          inc(Data, sav(Data, Dest, Info));
          dec(n);
        until n = 0;
    end;
  end;
end;

function _BS_DynArray(Data: PAnsiChar; Dest: TBufferWriter; Info: PRttiInfo): PtrInt;
begin
  DynArraySave(Data, nil, Dest, Info);
  result := SizeOf(pointer);
end;

function _BL_DynArray(Data: PAnsiChar; var Source: TFastReader; Info: PRttiInfo): PtrInt;
var
  n, itemsize: PtrInt;
  iteminfo: PRttiInfo;
  load: TRttiBinaryLoad;
begin
  iteminfo := Info^.DynArrayItemType(itemsize);
  Source.VarNextInt; // ignore stored itemsize for 32-bit/64-bit compatibility
  if Source.NextByte <> DelphiType(iteminfo) then
    Source.ErrorData('_BINARYLOAD[rkDynArray] failed for %', [Info.Name^]);
  if PPointer(Data)^ <> nil then
    FastDynArrayClear(pointer(Data), iteminfo);
  n := Source.VarUInt32;
  Source.Next4; // ignore legacy Hash32 checksum (was to avoid buffer overflow)
  if n > 0 then
  begin
    DynArraySetLength(pointer(Data), Info, 1, @n); // allocate memory
    Data := PPointer(Data)^; // point to first item
    if iteminfo = nil then
      Source.Copy(Data, itemsize * n)
    else
    begin
      load := _BINARYLOAD[iteminfo^.Kind];
      if Assigned(load) then
        repeat
          inc(Data, load(Data, Source, iteminfo));
          dec(n);
        until n = 0;
    end;
  end;
  result := SizeOf(pointer);
end;

function DynArrayCompare(A, B: PAnsiChar; ExternalCountA, ExternalCountB: PInteger;
  Info: PRttiInfo; CaseSensitive: boolean): integer;
var
  n1, n2, itemsize: PtrInt;
  comps: PRttiBinaryCompares;
  comp: TRttiBinaryCompare;
begin
  A := PPointer(A)^;
  B := PPointer(B)^;
  if A = B then
  begin
    result := 0;
    exit;
  end
  else if A = nil then
  begin
    result := -1;
    exit;
  end
  else if B = nil then
  begin
    result := 1;
    exit;
  end;
  Info := Info^.DynArrayItemType;
  if ExternalCountA <> nil then
    n1 := ExternalCountA^ // e.g. from TDynArray with external count
  else
    n1 := PDynArrayRec(A - SizeOf(TDynArrayRec))^.length;
  if ExternalCountB <> nil then
    n2 := ExternalCountB^
  else
    n2 := PDynArrayRec(B - SizeOf(TDynArrayRec))^.length;
  if CaseSensitive then
    comps := @_BINARYCOMPARE
  else
    comps := @_BINARYCOMPAREI;
  comp := comps^[Info^.Kind];
  repeat
    itemsize := comp(A, B, Info, result);
    inc(A, itemsize);
    inc(B, itemsize);
    if result <> 0 then
      exit;
    dec(n1); // both items are equal -> continue to next items
    dec(n2);
  until (n1 = 0) or (n2 = 0);
  result := n1 - n2;
end;

function _BC_DynArray(A, B: pointer; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := DynArrayCompare(A, B, nil, nil, Info, {casesens=}true);
  result := SizeOf(pointer);
end;

function _BCI_DynArray(A, B: pointer; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := DynArrayCompare(A, B, nil, nil, Info, {casesens=}false);
  result := SizeOf(pointer);
end;

function _BS_Record(Data: PAnsiChar; Dest: TBufferWriter; Info: PRttiInfo): PtrInt;
var
  fields: TRttiRecordManagedFields; // Size/Count/Fields
  offset: PtrUInt;
  f: PRttiRecordField;
begin
  Info^.RecordManagedFields(fields);
  f := fields.Fields;
  fields.Fields := @_BINARYSAVE; // reuse pointer slot on stack
  offset := 0;
  while fields.Count <> 0 do
  begin
    dec(fields.Count);
    Info := f^.{$ifdef HASDIRECTTYPEINFO}TypeInfo{$else}TypeInfoRef^{$endif};
    {$ifdef FPC_OLDRTTI}
    if Info^.Kind in rkManagedTypes then
    {$endif FPC_OLDRTTI}
    begin
      offset := f^.Offset - offset;
      if offset <> 0 then
      begin
        Dest.Write(Data, offset);
        inc(Data, offset);
      end;
      inc(Data, PRttiBinarySaves(fields.Fields)[Info^.Kind](Data, Dest, Info));
      inc(offset, f^.Offset);
    end;
    inc(f);
  end;
  offset := PtrUInt(fields.Size) - offset;
  if offset > 0 then
    Dest.Write(Data, offset);
  result := fields.Size;
end;

function _BL_Record(Data: PAnsiChar; var Source: TFastReader; Info: PRttiInfo): PtrInt;
var
  fields: TRttiRecordManagedFields; // Size/Count/Fields
  offset: PtrUInt;
  f: PRttiRecordField;
begin
  Info^.RecordManagedFields(fields);
  f := fields.Fields;
  fields.Fields := @_BINARYLOAD; // reuse pointer slot on stack
  offset := 0;
  while fields.Count <> 0 do
  begin
    dec(fields.Count);
    Info := f^.{$ifdef HASDIRECTTYPEINFO}TypeInfo{$else}TypeInfoRef^{$endif};
    {$ifdef FPC_OLDRTTI}
    if Info^.Kind in rkManagedTypes then
    {$endif FPC_OLDRTTI}
    begin
      offset := f^.Offset - offset;
      if offset <> 0 then
      begin
        Source.Copy(Data, offset);
        inc(Data, offset);
      end;
      inc(Data, PRttiBinaryLoads(fields.Fields)[Info^.Kind](Data, Source, Info));
      inc(offset, f^.Offset);
    end;
    inc(f);
  end;
  offset := PtrUInt(fields.Size) - offset;
  if offset > 0 then
    Source.Copy(Data, offset);
  result := fields.Size;
end;

function RecordCompare(A, B: PUTF8Char; Info: PRttiInfo; CaseSensitive: boolean): integer;
var
  fields: TRttiRecordManagedFields; // Size/Count/Fields
  offset, fieldsize: PtrUInt;
  f: PRttiRecordField;
begin
  Info^.RecordManagedFields(fields);
  f := fields.Fields;
  if CaseSensitive then
    fields.Fields := @_BINARYCOMPARE // reuse pointer slot on stack
  else
    fields.Fields := @_BINARYCOMPAREI;
  offset := 0;
  while fields.Count <> 0 do
  begin
    dec(fields.Count);
    Info := f^.{$ifdef HASDIRECTTYPEINFO}TypeInfo{$else}TypeInfoRef^{$endif};
    {$ifdef FPC_OLDRTTI}
    if Info^.Kind in rkManagedTypes then
    {$endif FPC_OLDRTTI}
    begin
      offset := f^.Offset - offset;
      if offset <> 0 then
      begin
        result := StrCompL(A, B, offset); // binary comparison with length
        if result <> 0 then
          exit;
        inc(A, offset);
        inc(B, offset);
      end;
      fieldsize := PRttiBinaryCompares(fields.Fields)[Info^.Kind](A, B, Info, result);
      inc(A, fieldsize);
      inc(B, fieldsize);
      if result <> 0 then
        exit;
      inc(offset, f^.Offset);
    end;
    inc(f);
  end;
  offset := PtrUInt(fields.Size) - offset;
  if offset > 0 then
    result := StrCompL(A, B, offset);
end;

function _BC_Record(A, B: pointer; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := RecordCompare(A, B, Info, {casesens=}true);
  result := Info^.RecordSize;
end;

function _BCI_Record(A, B: pointer; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := RecordCompare(A, B, Info, {casesens=}false);
  result := Info^.RecordSize;
end;

function _BS_Array(Data: PAnsiChar; Dest: TBufferWriter; Info: PRttiInfo): PtrInt;
var
  n: PtrInt;
  sav: TRttiBinarySave;
begin
  Info := Info^.ArrayItemType(n, result);
  if Info = nil then
    Dest.Write(Data, result)
  else
  begin
    sav := _BINARYSAVE[Info^.Kind];
    if Assigned(sav) then // paranoid check
      repeat
        inc(Data, sav(Data, Dest, Info));
        dec(n);
      until n = 0;
  end;
end;

function _BL_Array(Data: PAnsiChar; var Source: TFastReader; Info: PRttiInfo): PtrInt;
var
  n: PtrInt;
  load: TRttiBinaryLoad;
begin
  Info := Info^.ArrayItemType(n, result);
  if Info = nil then
    Source.Copy(Data, result)
  else
  begin
    load := _BINARYLOAD[Info^.Kind];
    if Assigned(load) then // paranoid check
      repeat
        inc(Data, load(Data, Source, Info));
        dec(n);
      until n = 0;
  end;
end;

function ArrayCompare(A, B: PUTF8Char; Info: PRttiInfo; CaseSensitive: boolean;
  out ArraySize: PtrInt): integer;
var
  n, itemsize: PtrInt;
  cmp: TRttiBinaryCompare;
begin
  Info := Info^.ArrayItemType(n, ArraySize);
  if Info = nil then
    result := StrCompL(A, B, ArraySize)
  else
  begin
    if CaseSensitive then
      cmp := _BINARYCOMPARE[Info^.Kind]
    else
      cmp := _BINARYCOMPAREI[Info^.Kind];
    if Assigned(cmp) then // paranoid check
      repeat
        itemsize := cmp(A, B, Info, result);
        inc(A, itemsize);
        inc(B, itemsize);
        if result <> 0 then
          exit;
        dec(n);
      until n = 0
    else
      result := A - B;
  end;
end;

function _BC_Array(A, B: pointer; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := ArrayCompare(A, B, Info, {casesens=}true, result);
end;

function _BCI_Array(A, B: pointer; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := ArrayCompare(A, B, Info, {casesens=}false, result);
end;

procedure _BS_VariantComplex(Data: PVariant; Dest: TBufferWriter);
var
  temp: TTextWriterStackBuffer;
  tempstr: RawUTF8;
begin // not very fast, but creates valid JSON - see also VariantSaveJSON()
  with DefaultTextWriterSerializer.CreateOwnedStream(temp) do
  try
    AddVariant(Data^, twJSONEscape);
    if WrittenBytes = 0 then
      Dest.WriteVar(@temp, PendingBytes) // no tempstr allocation needed
    else
    begin
      SetText(tempstr);
      Dest.Write(tempstr);
    end;
  finally
    Free;
  end;
end;

procedure _BL_VariantComplex(Data: PVariant; var Source: TFastReader);
var
  temp: TSynTempBuffer;
begin
  Source.VarBlob(temp); // load and make a private copy for in-place JSON parsing
  try
    BinaryVariantLoadAsJSON(Data^, temp.buf);
  finally
    temp.Done;
  end;
end;

const
  // 0 for unserialized VType, 255 for valOleStr
  VARIANT_SIZE: array[varEmpty .. varWord64] of byte = (
    0, 0, 2, 4, 4, 8, 8, 8, 255, 0, 0, 2, 0, 0, 0, 0, 1, 1, 2, 4, 8, 8);

function _BS_Variant(Data: PVarData; Dest: TBufferWriter; Info: PRttiInfo): PtrInt;
var
  vt: cardinal;
begin
  Data := VarDataFromVariant(PVariant(Data)^); // handle varByRef
  vt := Data^.VType;
  Dest.Write2(vt);
  if vt <= high(VARIANT_SIZE) then
  begin
    vt := VARIANT_SIZE[vt];
    if vt <> 0 then
      if vt = 255 then
        Dest.WriteVar(Data^.vAny, length(PWideString(Data^.vAny)^) * 2)
      else
        Dest.Write(@Data^.VInt64, vt); // simple types
  end
  else if vt = varString then // expect only RawUTF8
    Dest.WriteVar(Data^.vAny, length(PRawByteString(Data^.vAny)^))
  {$ifdef HASVARUSTRING}
  else if vt = varUString then
    Dest.WriteVar(Data^.vAny, length(PUnicodeString(Data^.vAny)^))
  {$endif HASVARUSTRING}
  else
    _BS_VariantComplex(pointer(Data), Dest);
  result := SizeOf(Data^);
end;

function _BL_Variant(Data: PVarData; var Source: TFastReader; Info: PRttiInfo): PtrInt;
var
  vt: cardinal;
begin
  VarClear(PVariant(Data)^);
  Source.Copy(@Data^.VType, 2);
  Data^.VAny := nil; // to avoid GPF below
  vt := Data^.VType;
  if vt <= high(VARIANT_SIZE) then
  begin
    vt := VARIANT_SIZE[vt];
    if vt <> 0 then
      if vt = 255 then
        with Source.VarBlob do
          SetString(PWideString(Data^.vAny)^, PWideChar(Ptr), Len shr 1)
      else
        Source.Copy(@Data^.VInt64, vt); // simple types
  end
  else if vt = varString then
    with Source.VarBlob do
      FastSetString(PRawUTF8(Data^.vAny)^, Ptr, Len) // expect only RawUTF8
  {$ifdef HASVARUSTRING}
  else if vt = varUString then
    with Source.VarBlob do
      SetString(PUnicodeString(Data^.vAny)^, PWideChar(Ptr), Len shr 1)
  {$endif HASVARUSTRING}
  else if Assigned(BinaryVariantLoadAsJSON) then
    _BL_VariantComplex(pointer(Data), Source)
  else
    Source.ErrorData('_BINARYLOAD[tkVariant]: missing mormot.core.variants.pas', []);
  result := SizeOf(Data^);
end;

function _BC_Variant(A, B: PVarData; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := SortDynArrayVariantComp(A^, B^, {caseinsens=}false);
  result := SizeOf(variant);
end;

function _BCI_Variant(A, B: PVarData; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := SortDynArrayVariantComp(A^, B^, {caseinsens=}true);
  result := SizeOf(variant);
end;


procedure InitializeConstants;
var
  i: PtrInt;
  k: TRttiKind;
begin
  for k := low(k) to high(k) do
    case k of
      rkInteger, rkEnumeration, rkSet, rkChar, rkWChar {$ifdef FPC}, rkBool{$endif}:
      begin
        _BINARYSAVE[k] := @_BS_Ord;
        _BINARYLOAD[k] := @_BL_Ord;
        _BINARYCOMPARE[k] := @_BC_Ord;
        _BINARYCOMPAREI[k] := @_BC_Ord;
      end;
      {$ifdef FPC} rkQWord, {$endif} rkInt64:
      begin
        _BINARYSAVE[k] := @_BS_64;
        _BINARYLOAD[k] := @_BL_64;
        _BINARYCOMPARE[k] := @_BC_64;
        _BINARYCOMPAREI[k] := @_BC_64;
      end;
      rkFloat:
      begin
        _BINARYSAVE[k] := @_BS_Float;
        _BINARYLOAD[k] := @_BS_Float;
        _BINARYCOMPARE[k] := @_BC_Float;
        _BINARYCOMPAREI[k] := @_BC_Float;
      end;
      {$ifdef HASVARUSTRING} rkUString, {$endif} rkLString:
      begin
        _BINARYSAVE[k] := @_BS_String;
        if k = rkLString then
        begin
          _BINARYLOAD[k] := @_BL_LString;
          _BINARYCOMPARE[k] := @_BC_PUTF8Char;
          _BINARYCOMPAREI[k] := @_BCI_PUTF8Char;
        end
        {$ifdef HASVARUSTRING}
        else if k = rkUString then
        begin
          _BINARYLOAD[k] := @_BL_UString;
          _BINARYCOMPARE[k] := @_BC_PWideChar;
          _BINARYCOMPAREI[k] := @_BCI_PWideChar;
        end;
        {$endif HASVARUSTRING}
      end; // rkLStringOld not generated any more
      rkWString:
      begin
        _BINARYSAVE[k] := @_BS_WString;
        _BINARYLOAD[k] := @_BL_WString;
        _BINARYCOMPARE[k] := @_BC_PWideChar;
        _BINARYCOMPAREI[k] := @_BCI_PWideChar;
      end;
      {$ifdef FPC} rkObject, {$endif} rkRecord:
      begin
        _BINARYSAVE[k] := @_BS_Record;
        _BINARYLOAD[k] := @_BL_Record;
        _BINARYCOMPARE[k] := @_BC_Record;
        _BINARYCOMPAREI[k] := @_BCI_Record;
      end;
      rkDynArray:
      begin
        _BINARYSAVE[k] := @_BS_DynArray;
        _BINARYLOAD[k] := @_BL_DynArray;
        _BINARYCOMPARE[k] := @_BC_DynArray;
        _BINARYCOMPAREI[k] := @_BCI_DynArray;
      end;
      rkArray:
      begin
        _BINARYSAVE[k] := @_BS_Array;
        _BINARYLOAD[k] := @_BL_Array;
        _BINARYCOMPARE[k] := @_BC_Array;
        _BINARYCOMPAREI[k] := @_BCI_Array;
      end;
      rkVariant:
      begin
        _BINARYSAVE[k] := @_BS_Variant;
        _BINARYLOAD[k] := @_BL_Variant;
        _BINARYCOMPARE[k] := @_BC_Variant;
        _BINARYCOMPAREI[k] := @_BCI_Variant;
      end;
    end; // other unsupported types will return 0
  FillcharFast(ConvertBase64ToBin, SizeOf(ConvertBase64ToBin), 255); // -1 = invalid
  for i := 0 to high(b64enc) do
    ConvertBase64ToBin[b64enc[i]] := i;
  ConvertBase64ToBin['='] := -2; // special value for '='
  for i := 0 to high(b64urienc) do
    ConvertBase64uriToBin[b64urienc[i]] := i;
  for i := high(Baudot2Char) downto 0 do
    if Baudot2Char[i]<#128 then
      Char2Baudot[Baudot2Char[i]] := i;
  for i := ord('a') to ord('z') do
    Char2Baudot[AnsiChar(i - 32)] := Char2Baudot[AnsiChar(i)]; // A-Z -> a-z
end;


initialization
  InitializeConstants;
  SynCompressAlgos := TSynObjectList.Create;
  AlgoSynLZ := TAlgoSynLZ.Create;

finalization
  SynCompressAlgos.Free;

end.

