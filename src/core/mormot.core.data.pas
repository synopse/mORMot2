/// Framework Core Low-Level Data Processing Functions
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.data;

{
  *****************************************************************************

   Low-Level Data Processing Functions shared by all framework units
    - RTL TPersistent / TInterfacedObject with Custom Constructor
    - TSynPersistent* / TSyn*List classes
    - TSynPersistentStore with proper Binary Serialization
    - INI Files and In-memory Access
    - Efficient RTTI Values Binary Serialization and Comparison
    - TDynArray, TDynArrayHashed and TSynQueue Wrappers
    - RawUTF8 String Values Interning and TRawUTF8List
    - Cross-Platform Time Zones

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
  mormot.core.datetime,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers;


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

  /// an abstract ancestor, for implementing a custom TInterfacedObject like class
  // - by default, will do nothing: no instance would be retrieved by
  // QueryInterface unless the VirtualQueryInterface protected method is
  // overriden, and _AddRef/_Release methods would call VirtualAddRef and
  // VirtualRelease pure abstract methods
  // - using this class will leverage the signature difference between Delphi
  // and FPC, among all supported platforms
  // - the class includes a RefCount integer field
  TSynInterfacedObject = class(TObject,IUnknown)
  protected
    fRefCount: integer;
    // returns E_NOINTERFACE
    function VirtualQueryInterface(const IID: TGUID; out Obj): HResult; virtual;
    // always return 1 for a "non allocated" instance (0 triggers release)
    function VirtualAddRef: Integer; virtual; abstract;
    function VirtualRelease: Integer; virtual; abstract;
    {$ifdef FPC}
    function QueryInterface(
      {$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif} IID: TGUID;
      out Obj): longint; {$ifndef WINDOWS}cdecl{$else}stdcall{$endif};
    function _AddRef: longint; {$ifndef WINDOWS}cdecl{$else}stdcall{$endif};
    function _Release: longint; {$ifndef WINDOWS}cdecl{$else}stdcall{$endif};
    {$else}
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    {$endif FPC}
  public
    /// the associated reference count
    property RefCount: integer read fRefCount write fRefCount;
  end;

  /// any TCollection used between client and server shall inherit from this class
  // - you should override the GetClass virtual method to provide the
  // expected collection item class to be used on server side
  // - another possibility is to register a TCollection/TCollectionItem pair
  // via a call to Rtti.RegisterCollection()
  TInterfacedCollection = class(TCollection)
  protected
    /// you shall override this abstract method
    class function GetClass: TCollectionItemClass; virtual; abstract;
  public
    /// this constructor will call GetClass to initialize the collection
    constructor Create; reintroduce; virtual;
  end;

  /// used to determine the exact class type of a TInterfacedObjectWithCustomCreate
  // - could be used to create instances using its virtual constructor
  TInterfacedObjectWithCustomCreateClass = class of TInterfacedObjectWithCustomCreate;

  /// used to determine the exact class type of a TPersistentWithCustomCreateClass
  // - could be used to create instances using its virtual constructor
  TPersistentWithCustomCreateClass = class of TPersistentWithCustomCreate;

  /// class-reference type (metaclass) of a TInterfacedCollection kind
  TInterfacedCollectionClass = class of TInterfacedCollection;


  /// interface for TAutoFree to register another TObject instance
  // to an existing IAutoFree local variable
  IAutoFree = interface
    procedure Another(var objVar; obj: TObject);
  end;

  /// simple reference-counted storage for local objects
  // - be aware that it won't implement a full ARC memory model, but may be
  // just used to avoid writing some try ... finally blocks on local variables
  // - use with caution, only on well defined local scope
  TAutoFree = class(TInterfacedObject, IAutoFree)
  protected
    fObject: TObject;
    fObjectList: array of TObject;
  public
    /// initialize the TAutoFree class for one local variable
    // - do not call this constructor, but class function One() instead
    constructor Create(var localVariable; obj: TObject); reintroduce; overload;
    /// initialize the TAutoFree class for several local variables
    // - do not call this constructor, but class function Several() instead
    constructor Create(const varObjPairs: array of pointer); reintroduce; overload;
    /// protect one local TObject variable instance life time
    // - for instance, instead of writing:
    // !var myVar: TMyClass;
    // !begin
    // !  myVar := TMyClass.Create;
    // !  try
    // !    ... use myVar
    // !  finally
    // !    myVar.Free;
    // !  end;
    // !end;
    // - you may write:
    // !var myVar: TMyClass;
    // !begin
    // !  TAutoFree.One(myVar,TMyClass.Create);
    // !  ... use myVar
    // !end; // here myVar will be released
    // - warning: under FPC, you should assign the result of this method to a local
    // IAutoFree variable - see bug http://bugs.freepascal.org/view.php?id=26602
    class function One(var localVariable; obj: TObject): IAutoFree;
    /// protect several local TObject variable instances life time
    // - specified as localVariable/objectInstance pairs
    // - you may write:
    // !var var1,var2: TMyClass;
    // !begin
    // !  TAutoFree.Several([
    // !    @var1,TMyClass.Create,
    // !    @var2,TMyClass.Create]);
    // !  ... use var1 and var2
    // !end; // here var1 and var2 will be released
    // - warning: under FPC, you should assign the result of this method to a local
    // IAutoFree variable - see bug http://bugs.freepascal.org/view.php?id=26602
     class function Several(const varObjPairs: array of pointer): IAutoFree;
    /// protect another TObject variable to an existing IAutoFree instance life time
    // - you may write:
    // !var var1,var2: TMyClass;
    // !    auto: IAutoFree;
    // !begin
    // !  auto := TAutoFree.One(var1,TMyClass.Create);,
    // !  .... do something
    // !  auto.Another(var2,TMyClass.Create);
    // !  ... use var1 and var2
    // !end; // here var1 and var2 will be released
    procedure Another(var localVariable; obj: TObject);
    /// will finalize the associated TObject instances
    // - note that releasing the TObject instances won't be protected, so
    // any exception here may induce a memory leak: use only with "safe"
    // simple objects, e.g. mORMot's TSQLRecord
    destructor Destroy; override;
  end;


  /// an interface used by TAutoLocker to protect multi-thread execution
  IAutoLocker = interface
    ['{97559643-6474-4AD3-AF72-B9BB84B4955D}']
    /// enter the mutex
    // - any call to Enter should be ended with a call to Leave, and
    // protected by a try..finally block, as such:
    // !begin
    // !  ... // unsafe code
    // !  fSharedAutoLocker.Enter;
    // !  try
    // !    ... // thread-safe code
    // !  finally
    // !    fSharedAutoLocker.Leave;
    // !  end;
    // !end;
    procedure Enter;
    /// leave the mutex
    // - any call to Leave should be preceded with a call to Enter
    procedure Leave;
    /// will enter the mutex until the IUnknown reference is released
    // - using an IUnknown interface to let the compiler auto-generate a
    // try..finally block statement to release the lock for the code block
    // - could be used as such under Delphi:
    // !begin
    // !  ... // unsafe code
    // !  fSharedAutoLocker.ProtectMethod;
    // !  ... // thread-safe code
    // !end; // local hidden IUnknown will release the lock for the method
    // - warning: under FPC, you should assign its result to a local variable -
    // see bug http://bugs.freepascal.org/view.php?id=26602
    // !var LockFPC: IUnknown;
    // !begin
    // !  ... // unsafe code
    // !  LockFPC := fSharedAutoLocker.ProtectMethod;
    // !  ... // thread-safe code
    // !end; // LockFPC will release the lock for the method
    // or
    // !begin
    // !  ... // unsafe code
    // !  with fSharedAutoLocker.ProtectMethod do begin
    // !    ... // thread-safe code
    // !  end; // local hidden IUnknown will release the lock for the method
    // !end;
    function ProtectMethod: IUnknown;
    /// gives an access to the internal low-level TSynLocker instance used
    function Safe: PSynLocker;
  end;

  /// reference-counted block code critical section
  // - you can use one instance of this to protect multi-threaded execution
  // - the main class may initialize a IAutoLocker property in Create, then call
  // IAutoLocker.ProtectMethod in any method to make its execution thread safe
  // - this class inherits from TInterfacedObjectWithCustomCreate so you
  // could define one published property of a mORMot.pas' TInjectableObject
  // as IAutoLocker so that this class may be automatically injected
  // - you may use the inherited TAutoLockerDebug class, as defined in SynLog.pas,
  // to debug unexpected race conditions due to such critical sections
  // - consider inherit from high-level TSynPersistentLock or call low-level
  // fSafe := NewSynLocker / fSafe^.DoneAndFreemem instead
  TAutoLocker = class(TInterfacedObjectWithCustomCreate, IAutoLocker)
  protected
    fSafe: TSynLocker;
  public
    /// initialize the mutex
    constructor Create; override;
    /// finalize the mutex
    destructor Destroy; override;
    /// will enter the mutex until the IUnknown reference is released
    // - as expected by IAutoLocker interface
    // - could be used as such under Delphi:
    // !begin
    // !  ... // unsafe code
    // !  fSharedAutoLocker.ProtectMethod;
    // !  ... // thread-safe code
    // !end; // local hidden IUnknown will release the lock for the method
    // - warning: under FPC, you should assign its result to a local variable -
    // see bug http://bugs.freepascal.org/view.php?id=26602
    // !var LockFPC: IUnknown;
    // !begin
    // !  ... // unsafe code
    // !  LockFPC := fSharedAutoLocker.ProtectMethod;
    // !  ... // thread-safe code
    // !end; // LockFPC will release the lock for the method
    // or
    // !begin
    // !  ... // unsafe code
    // !  with fSharedAutoLocker.ProtectMethod do begin
    // !    ... // thread-safe code
    // !  end; // local hidden IUnknown will release the lock for the method
    // !end;
    function ProtectMethod: IUnknown;
    /// enter the mutex
    // - as expected by IAutoLocker interface
    // - any call to Enter should be ended with a call to Leave, and
    // protected by a try..finally block, as such:
    // !begin
    // !  ... // unsafe code
    // !  fSharedAutoLocker.Enter;
    // !  try
    // !    ... // thread-safe code
    // !  finally
    // !    fSharedAutoLocker.Leave;
    // !  end;
    // !end;
    procedure Enter; virtual;
    /// leave the mutex
    // - as expected by IAutoLocker interface
    procedure Leave; virtual;
    /// access to the locking methods of this instance
    // - as expected by IAutoLocker interface
    function Safe: PSynLocker;
    /// direct access to the locking methods of this instance
    // - faster than IAutoLocker.Safe function
    property Locker: TSynLocker read fSafe;
  end;



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
    /// called by TRttiJson.SetParserType when this class is registered
    // - used e.g. by TSynPersistentWithID to register the "ID" field;
    // you can also change the Rtti.JsonSave callback if needed, or
    // set the rcfSynPersistentHook flag to call RttiBeforeWriteObject,
    // RttiWritePropertyValue and RttiAfterWriteObject methods (disabled by
    // default not to slow down the serialization process)
    class procedure RttiCustomSet(Rtti: TRttiCustom); virtual;
    // called before TTextWriter.WriteObject() serialize this instance as JSON
    // - triggered only if RttiCustomSet defined the rcfSynPersistentHook flag
    // - you can return true if your method made the serialization
    // - this default implementation just returns false, to continue serializing
    // - TSynMonitor will change the serialization Options for this instance
    function RttiBeforeWriteObject(W: TBaseWriter;
      var Options: TTextWriterWriteObjectOptions): boolean; virtual;
    // called by TTextWriter.WriteObject() to serialize one published property value
    // - triggered only if RttiCustomSet defined the rcfSynPersistentHook flag
    // - is overriden in TSQLRecord/TSQLRecordMany to detect "fake" instances
    // or by TSynPersistentWithPassword to hide the password field value
    // - should return true if a property has been written, false (which is the
    // default) if the property is to be serialized as usual
    function RttiWritePropertyValue(W: TBaseWriter; Prop: PRttiCustomProp;
      Options: TTextWriterWriteObjectOptions): boolean; virtual;
    /// called after TTextWriter.WriteObject() serialized this instance as JSON
    // - triggered only if RttiCustomSet defined the rcfSynPersistentHook flag
    // - execute just before W.BlockEnd('}')
    procedure RttiAfterWriteObject(W: TBaseWriter;
      Options: TTextWriterWriteObjectOptions); virtual;
    /// called to unserialize this instance from JSON
    // - triggered only if RttiCustomSet defined the rcfSynPersistentHook flag
    // - you can return true if your method made the unserialization
    // - this default implementation just returns false, to continue processing
    // - opaque Ctxt is a PJsonParserContext instance
    function RttiBeforeReadObject(Ctxt: pointer): boolean; virtual;
    /// called after this instance as been unserialized from JSON
    // - triggered only if RttiCustomSet defined the rcfSynPersistentHook flag
    procedure RttiAfterReadObject; virtual;
  public
    /// virtual constructor called at instance creation
    // - this constructor also registers the class type to the Rtti global list
    // - is declared as virtual so that inherited classes may have a root
    // constructor to override
    constructor Create; virtual;
    /// very efficiently retrieve the TRttiCustom associated with this class
    // - since Create did register it, just return the first vmtAutoTable slot
    class function RttiCustom: TRttiCustom; {$ifdef HASINLINE} inline; {$endif}
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

  /// used to determine the exact class type of a TSynPersistent
  // - could be used to create instances using its virtual constructor
  TSynPersistentClass = class of TSynPersistent;

  /// simple and efficient TList, without any notification
  // - regular TList has an internal notification mechanism which slows down
  // basic process, and can't be easily inherited
  // - stateless methods (like Add/Clear/Exists/Remove) are defined as virtual
  // since can be overriden e.g. by TSynObjectListLocked to add a TSynLocker
  TSynList = class(TObject)
  protected
    fCount: integer;
    fList: TPointerDynArray;
    function Get(index: Integer): pointer; {$ifdef HASINLINE} inline; {$endif}
  public
    /// virtual constructor called at instance creation
    constructor Create; virtual;
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
  PSynList = ^TSynList;

  {$M-}

  /// simple and efficient TObjectList, without any notification
  TSynObjectList = class(TSynList)
  protected
    fOwnObjects: boolean;
    fItemClass: TClass;
  public
    /// initialize the object list
    // - can optionally specify an item class for efficient JSON serialization
    constructor Create(aOwnObjects: boolean = true;
      aItemClass: TClass = nil); reintroduce; virtual;
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
    /// optional class of the stored items
    // - could be used when unserializing from JSON
    property ItemClass: TClass read fItemClass write fItemClass;
  end;
  PSynObjectList = ^TSynObjectList;

  /// meta-class of TSynObjectList type
  TSynObjectListClass = class of TSynObjectList;

  /// adding locking methods to a TSynPersistent with virtual constructor
  // - you may use this class instead of the RTL TCriticalSection, since it
  // would use a TSynLocker which does not suffer from CPU cache line conflit,
  // and is cross-compiler whereas TMonitor is Delphi-specific and buggy (at
  // least before XE5)
  TSynPersistentLock = class(TSynPersistent)
  protected
    // TSynLocker would increase inherited fields offset -> managed PSynLocker
    fSafe: PSynLocker;
    // will lock/unlock the instance during JSON serialization of its properties
    function RttiBeforeWriteObject(W: TBaseWriter;
      var Options: TTextWriterWriteObjectOptions): boolean; override;
    procedure RttiAfterWriteObject(W: TBaseWriter;
      Options: TTextWriterWriteObjectOptions); override;
  public
    /// initialize the instance, and its associated lock
    constructor Create; override;
    /// finalize the instance, and its associated lock
    destructor Destroy; override;
    /// access to the associated instance critical section
    // - call Safe.Lock/UnLock to protect multi-thread access on this storage
    property Safe: PSynLocker read fSafe;
    /// could be used as a short-cut to Safe.Lock
    procedure Lock;   {$ifdef HASINLINE}inline;{$endif}
    /// could be used as a short-cut to Safe.UnLock
    procedure Unlock; {$ifdef HASINLINE}inline;{$endif}
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

  /// abstract persistent class with a 64-bit TID field
  // - class is e.g. the parent of our TSQLRecord ORM classes
  // - defined here for proper class serialization in mormot.core.json.pas,
  //  without the need of linking the ORM code to the executable
  TSynPersistentWithID = class(TSynPersistent)
  protected
    fID: TID;
    /// copy the TID field value
    procedure AssignTo(Dest: TSynPersistent); override;
    /// will register the ID field value for proper JSON serialization
    class procedure RttiCustomSet(Rtti: TRttiCustom); override;
  public
    /// this property gives direct access to the class instance ID
    // - not defined as "published" since RttiCustomSet did register it
    property IDValue: TID read fID write fID;
  end;



{ ************ TSynPersistentStore with proper Binary Serialization }

type
  /// abstract high-level handling of (SynLZ-)compressed persisted storage
  // - LoadFromReader/SaveToWriter abstract methods should be overriden
  // with proper binary persistence implementation
  TSynPersistentStore = class(TSynLocked)
  protected
    fName: RawUTF8;
    fReader: TFastReader;
    fReaderTemp: PRawByteString;
    fLoadFromLastUncompressed, fSaveToLastUncompressed: integer;
    fLoadFromLastAlgo: TAlgoCompress;
    /// low-level virtual methods implementing the persistence reading
    procedure LoadFromReader; virtual;
    procedure SaveToWriter(aWriter: TBufferWriter); virtual;
  public
    /// initialize a void storage with the supplied name
    constructor Create(const aName: RawUTF8); reintroduce; overload; virtual;
    /// initialize a storage from a SaveTo persisted buffer
    // - raise a EFastReader exception on decoding error
    constructor CreateFrom(const aBuffer: RawByteString;
      aLoad: TAlgoCompressLoad = aclNormal);
    /// initialize a storage from a SaveTo persisted buffer
    // - raise a EFastReader exception on decoding error
    constructor CreateFromBuffer(aBuffer: pointer; aBufferLen: integer;
      aLoad: TAlgoCompressLoad = aclNormal);
    /// initialize a storage from a SaveTo persisted buffer
    // - raise a EFastReader exception on decoding error
    constructor CreateFromFile(const aFileName: TFileName;
      aLoad: TAlgoCompressLoad = aclNormal);
    /// fill the storage from a SaveTo persisted buffer
    // - actually call the LoadFromReader() virtual method for persistence
    // - raise a EFastReader exception on decoding error
    procedure LoadFrom(const aBuffer: RawByteString;
      aLoad: TAlgoCompressLoad = aclNormal); overload;
    /// initialize the storage from a SaveTo persisted buffer
    // - actually call the LoadFromReader() virtual method for persistence
    // - raise a EFastReader exception on decoding error
    procedure LoadFrom(aBuffer: pointer; aBufferLen: integer;
      aLoad: TAlgoCompressLoad = aclNormal); overload; virtual;
    /// initialize the storage from a SaveToFile content
    // - actually call the LoadFromReader() virtual method for persistence
    // - returns false if the file is not found, true if the file was loaded
    // without any problem, or raise a EFastReader exception on decoding error
    function LoadFromFile(const aFileName: TFileName;
      aLoad: TAlgoCompressLoad = aclNormal): boolean;
    /// persist the content as a SynLZ-compressed binary blob
    // - to be retrieved later on via LoadFrom method
    // - actually call the SaveToWriter() protected virtual method for persistence
    // - you can specify ForcedAlgo if you want to override the default AlgoSynLZ
    // - BufferOffset could be set to reserve some bytes before the compressed buffer
    procedure SaveTo(out aBuffer: RawByteString; nocompression: boolean = false;
      BufLen: integer = 65536; ForcedAlgo: TAlgoCompress = nil;
      BufferOffset: integer = 0); overload; virtual;
    /// persist the content as a SynLZ-compressed binary blob
    // - just an overloaded wrapper
    function SaveTo(nocompression: boolean = false; BufLen: integer = 65536;
      ForcedAlgo: TAlgoCompress = nil; BufferOffset: integer = 0): RawByteString; overload;
      {$ifdef HASINLINE} inline;{$endif}
    /// persist the content as a SynLZ-compressed binary file
    // - to be retrieved later on via LoadFromFile method
    // - returns the number of bytes of the resulting file
    // - actually call the SaveTo method for persistence
    function SaveToFile(const aFileName: TFileName; nocompression: boolean = false;
      BufLen: integer = 65536; ForcedAlgo: TAlgoCompress = nil): PtrUInt;
    /// one optional text associated with this storage
    // - you can define this field as published to serialize its value in log/JSON
    property Name: RawUTF8 read fName;
    /// after a LoadFrom(), contains the uncompressed data size read
    property LoadFromLastUncompressed: integer read fLoadFromLastUncompressed;
    /// after a SaveTo(), contains the uncompressed data size written
    property SaveToLastUncompressed: integer read fSaveToLastUncompressed;
  end;



{ ********** RTTI Values Binary Serialization and Comparison }

  type
    /// possible options for a TDocVariant JSON/BSON document storage
    // - defined in this unit to avoid circular reference with mormot.core.variants
    // - dvoIsArray and dvoIsObject will store the "Kind: TDocVariantKind" state -
    // you should never have to define these two options directly
    // - dvoNameCaseSensitive will be used for every name lookup - here
    // case-insensitivity is restricted to a-z A-Z 0-9 and _ characters
    // - dvoCheckForDuplicatedNames will be used for method
    // TDocVariantData.AddValue(), but not when setting properties at
    // variant level: for consistency, "aVariant.AB := aValue" will replace
    // any previous value for the name "AB"
    // - dvoReturnNullForUnknownProperty will be used when retrieving any value
    // from its name (for dvObject kind of instance), or index (for dvArray or
    // dvObject kind of instance)
    // - by default, internal values will be copied by-value from one variant
    // instance to another, to ensure proper safety - but it may be too slow:
    // if you set dvoValueCopiedByReference, the internal
    // TDocVariantData.VValue/VName instances will be copied by-reference,
    // to avoid memory allocations, BUT it may break internal process if you change
    // some values in place (since VValue/VName and VCount won't match) - as such,
    // if you set this option, ensure that you use the content as read-only
    // - any registered custom types may have an extended JSON syntax (e.g.
    // TBSONVariant does for MongoDB types), and will be searched during JSON
    // parsing, unless dvoJSONParseDoNotTryCustomVariants is set (slightly faster)
    // - by default, it will only handle direct JSON [array] of {object}: but if
    // you define dvoJSONObjectParseWithinString, it will also try to un-escape
    // a JSON string first, i.e. handle "[array]" or "{object}" content (may be
    // used e.g. when JSON has been retrieved from a database TEXT column) - is
    // used for instance by VariantLoadJSON()
    // - JSON serialization will follow the standard layout, unless
    // dvoSerializeAsExtendedJson is set so that the property names would not
    // be escaped with double quotes, writing '{name:"John",age:123}' instead of
    // '{"name":"John","age":123}': this extended json layout is compatible with
    // http://docs.mongodb.org/manual/reference/mongodb-extended-json and with
    // TDocVariant JSON unserialization, also our SynCrossPlatformJSON unit, but
    // NOT recognized by most JSON clients, like AJAX/JavaScript or C#/Java
    // - by default, only integer/Int64/currency number values are allowed, unless
    // dvoAllowDoubleValue is set and 32-bit floating-point conversion is tried,
    // with potential loss of precision during the conversion
    // - dvoInternNames and dvoInternValues will use shared TRawUTF8Interning
    // instances to maintain a list of RawUTF8 names/values for all TDocVariant,
    // so that redundant text content will be allocated only once on heap
    TDocVariantOption =
      (dvoIsArray, dvoIsObject,
       dvoNameCaseSensitive, dvoCheckForDuplicatedNames,
       dvoReturnNullForUnknownProperty,
       dvoValueCopiedByReference, dvoJSONParseDoNotTryCustomVariants,
       dvoJSONObjectParseWithinString, dvoSerializeAsExtendedJson,
       dvoAllowDoubleValue, dvoInternNames, dvoInternValues);

    /// set of options for a TDocVariant storage
    // - defined in this unit to avoid circular reference with mormot.core.variants
    // - you can use JSON_OPTIONS[true] if you want to create a fast by-reference
    // local document as with _ObjFast/_ArrFast/_JsonFast - i.e.
    // [dvoReturnNullForUnknownProperty,dvoValueCopiedByReference]
    // - when specifying the options, you should not include dvoIsArray nor
    // dvoIsObject directly in the set, but explicitly define TDocVariantDataKind
    TDocVariantOptions = set of TDocVariantOption;

    /// pointer to a set of options for a TDocVariant storage
    // - defined in this unit to avoid circular reference with mormot.core.variants
    // - you may use e.g. @JSON_OPTIONS[true], @JSON_OPTIONS[false],
    // @JSON_OPTIONS_FAST_STRICTJSON or @JSON_OPTIONS_FAST_EXTENDED
    PDocVariantOptions = ^TDocVariantOptions;


type
  /// internal function handler for binary persistence of any RTTI type value
  // - i.e. the kind of functions called via RTTI_BINARYSAVE[] lookup table
  // - work with managed and unmanaged types
  // - persist Data^ into Dest, returning the size in Data^ as bytes
  TRttiBinarySave = function(Data: pointer; Dest: TBufferWriter;
    Info: PRttiInfo): PtrInt;

  /// the type of RTTI_BINARYSAVE[] efficient lookup table
  TRttiBinarySaves = array[TRttiKind] of TRttiBinarySave;
  PRttiBinarySaves = ^TRttiBinarySaves;

  /// internal function handler for binary persistence of any RTTI type value
  // - i.e. the kind of functions called via RTTI_BINARYLOAD[] lookup table
  // - work with managed and unmanaged types
  // - fill Data^ from Source, returning the size in Data^ as bytes
  TRttiBinaryLoad = function(Data: pointer; var Source: TFastReader;
    Info: PRttiInfo): PtrInt;

  /// the type of RTTI_BINARYLOAD[] efficient lookup table
  TRttiBinaryLoads = array[TRttiKind] of TRttiBinaryLoad;
  PRttiBinaryLoads = ^TRttiBinaryLoads;

  /// internal function handler for fast comparison of any RTTI type value
  // - i.e. the kind of functions called via RTTI_COMPARE[] lookup table
  // - work with managed and unmanaged types
  // - returns the size in Data1/Data2^ as bytes, and the result in Compared
  TRttiCompare = function(Data1, Data2: pointer; Info: PRttiInfo;
    out Compared: integer): PtrInt;

  /// the type of RTTI_COMPARE[] efficient lookup table
  TRttiCompares = array[TRttiKind] of TRttiCompare;
  PRttiCompares = ^TRttiCompares;

  TRttiComparers = array[{CaseInSensitive=}boolean] of TRttiCompares;

var
  /// lookup table for binary persistence of any RTTI type value
  // - for efficient persistence into binary of managed and unmanaged types
  RTTI_BINARYSAVE: TRttiBinarySaves;

  /// lookup table for binary persistence of any RTTI type value
  // - for efficient retrieval from binary of managed and unmanaged types
  RTTI_BINARYLOAD: TRttiBinaryLoads;

  /// lookup table for comparison of any RTTI type value
  // - for efficient search or sorting of managed and unmanaged types
  // - RTTI_COMPARE[false] for case-sensitive comparison
  // - RTTI_COMPARE[true] for case-insensitive comparison
  RTTI_COMPARE: TRttiComparers;


/// raw binary serialization of a dynamic array
// - as called e.g. by TDynArray.SaveTo, using ExternalCount optional parameter
// - RTTI_BINARYSAVE[rkDynArray] is a wrapper to this function, with ExternalCount=nil
procedure DynArraySave(Data: PAnsiChar; ExternalCount: PInteger;
  Dest: TBufferWriter; Info: PRttiInfo); overload;

/// serialize a dynamic array content as binary, ready to be loaded by
// DynArrayLoad() / TDynArray.Load()
// - Value shall be set to the source dynamic arry field
// - is a wrapper around BinarySave(rkDynArray)
function DynArraySave(var Value; TypeInfo: pointer): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fill a dynamic array content from a binary serialization as saved by
// DynArraySave() / TDynArray.Save()
// - Value shall be set to the target dynamic array field
// - is a wrapper around BinaryLoad(rkDynArray)
function DynArrayLoad(var Value; Source: PAnsiChar; TypeInfo: pointer;
  TryCustomVariants: PDocVariantOptions = nil): PAnsiChar;
  {$ifdef HASINLINE}inline;{$endif}

/// raw comparison of two dynamic arrays
// - as called e.g. by TDynArray.Equals, using ExternalCountA/B optional parameter
// - RTTI_COMPARE[true/false,rkDynArray] are wrappers to this, with ExternalCount=nil
function DynArrayCompare(A, B: PAnsiChar; ExternalCountA, ExternalCountB: PInteger;
  Info: PRttiInfo; CaseInSensitive: boolean): integer;

/// compare two dynamic arrays by calling TDynArray.Equals
function DynArrayEquals(TypeInfo: pointer; var Array1, Array2;
  Array1Count: PInteger = nil; Array2Count: PInteger = nil): boolean;
  {$ifdef HASINLINE}inline;{$endif}


/// check equality of two values by content, using RTTI
// - optionally returns the known in-memory PSize of the value
function BinaryEquals(A, B: pointer; Info: PRttiInfo; PSize: PInteger;
  Kinds: TRttiKinds; CaseInSensitive: boolean): boolean;

/// comparison of two values by content, using RTTI
function BinaryCompare(A, B: pointer; Info: PRttiInfo; CaseInSensitive: boolean): integer;

/// comparison of two TObject published properties, using RTTI
function ObjectCompare(A, B: TObject; CaseInSensitive: boolean): integer;

/// case-sensitive comparison of two TObject published properties, using RTTI
function ObjectEquals(A, B: TObject): integer;
  {$ifdef HASINLINE}inline;{$endif}

/// case-insensitive comparison of two TObject published properties, using RTTI
function ObjectEqualsI(A, B: TObject): integer;
  {$ifdef HASINLINE}inline;{$endif}

{$ifndef PUREMORMOT2}

/// how many bytes a BinarySave() may return
// - deprecated function - use overloaded BinarySave() functions instead
function BinarySaveLength(Data: pointer; Info: PRttiInfo; Len: PInteger;
  Kinds: TRttiKinds): integer; deprecated;

/// binary persistence of any value using RTTI, into a memory buffer
// - deprecated function - use overloaded BinarySave() functions instead
function BinarySave(Data: pointer; Dest: PAnsiChar; Info: PRttiInfo;
  out Len: integer; Kinds: TRttiKinds): PAnsiChar; overload; deprecated;

{$endif PUREMORMOT2}

/// binary persistence of any value using RTTI, into a RawByteString buffer
function BinarySave(Data: pointer; Info: PRttiInfo; Kinds: TRttiKinds): RawByteString; overload;

/// binary persistence of any value using RTTI, into a TBytes buffer
function BinarySaveBytes(Data: pointer; Info: PRttiInfo; Kinds: TRttiKinds): TBytes;

/// binary persistence of any value using RTTI, into a TBufferWriter stream
procedure BinarySave(Data: pointer; Info: PRttiInfo; Dest: TBufferWriter); overload;
  {$ifdef HASINLINE}inline;{$endif}

/// binary persistence of any value using RTTI, into a TSynTempBuffer buffer
procedure BinarySave(Data: pointer; var Dest: TSynTempBuffer;
  Info: PRttiInfo; Kinds: TRttiKinds); overload;

/// binary persistence of any value using RTTI, into a Base64-encoded text
// - contains a trailing crc32c hash before the actual data - so is not
// the WrBase64() regular format - unless NoCrc32Trailer is set to true
function BinarySaveBase64(Data: pointer; Info: PRttiInfo; UriCompatible: boolean;
  Kinds: TRttiKinds; NoCrc32Trailer: boolean = false): RawUTF8;

/// unserialize any value from BinarySave() memory buffer, using RTTI
function BinaryLoad(Data: pointer; Source: PAnsiChar; Info: PRttiInfo;
  Len: PInteger; SourceMax: PAnsiChar; Kinds: TRttiKinds;
  TryCustomVariants: PDocVariantOptions = nil): PAnsiChar; overload;

/// unserialize any value from BinarySave() RawByteString, using RTTI
function BinaryLoad(Data: pointer; const Source: RawByteString; Info: PRttiInfo;
  Kinds: TRttiKinds; TryCustomVariants: PDocVariantOptions = nil): boolean; overload;

/// unserialize any value from BinarySaveBase64() encoding, using RTTI
// - contains a trailing crc32c hash before the actual data - so is not
// the WrBase64() regular format, unless NoCrc32Trailer is true
function BinaryLoadBase64(Source: PAnsiChar; Len: PtrInt; Data: pointer;
  Info: PRttiInfo; UriCompatible: boolean; Kinds: TRttiKinds;
  NoCrc32Trailer: boolean = false; TryCustomVariants: PDocVariantOptions = nil): boolean;


/// check equality of two records by content
// - will handle packed records, with binaries (byte, word, integer...) and
// string types properties
// - will use binary-level comparison: it could fail to match two floating-point
// values because of rounding issues (Currency won't have this problem)
// - is a wrapper around BinaryEquals(rkRecordTypes)
function RecordEquals(const RecA, RecB; TypeInfo: pointer;
  PRecSize: PInteger = nil; CaseInSensitive: boolean = false): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// save a record content into a RawByteString
// - will handle packed records, with binaries (byte, word, integer...) and
// string types properties (but not with internal raw pointers, of course)
// - will use a proprietary binary format, with some variable-length encoding
// of the string length - note that if you change the type definition, any
// previously-serialized content will fail, maybe triggering unexpected GPF: you
// may use TypeInfoToHash() if you share this binary data accross executables
// - warning: will encode generic string fields as AnsiString (one byte per char)
// prior to Delphi 2009, and as UnicodeString (two bytes per char) since Delphi
// 2009: if you want to use this function between UNICODE and NOT UNICODE
// versions of Delphi, you should use some explicit types like RawUTF8,
// WinAnsiString, SynUnicode or even RawUnicode/WideString
// - is a wrapper around BinarySave(rkRecordTypes)
function RecordSave(const Rec; TypeInfo: pointer): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// save a record content into a TBytes dynamic array
// - could be used as an alternative to RawByteString's RecordSave()
// - is a wrapper around BinarySaveBytes(rkRecordTypes)
function RecordSaveBytes(const Rec; TypeInfo: pointer): TBytes;
  {$ifdef HASINLINE}inline;{$endif}

{$ifndef PUREMORMOT2}

/// compute the number of bytes needed to save a record content
// using the RecordSave() function
// - deprecated function - use overloaded BinarySave() functions instead
function RecordSaveLength(const Rec; TypeInfo: pointer;
  Len: PInteger = nil): integer; deprecated;
  {$ifdef HASINLINE}inline;{$endif}

/// save a record content into a destination memory buffer
// - Dest must be at least RecordSaveLength() bytes long
// - deprecated function - use overloaded BinarySave() functions instead
function RecordSave(const Rec; Dest: PAnsiChar; TypeInfo: pointer;
  out Len: integer): PAnsiChar; overload; deprecated;
  {$ifdef HASINLINE}inline;{$endif}

/// save a record content into a destination memory buffer
// - Dest must be at least RecordSaveLength() bytes long
// - deprecated function - use overloaded BinarySave() functions instead
function RecordSave(const Rec; Dest: PAnsiChar; TypeInfo: pointer): PAnsiChar;
  overload; deprecated; {$ifdef HASINLINE}inline;{$endif}

{$endif PUREMORMOT2}

/// save a record content into a destination memory buffer
// - caller should make Dest.Done once finished with Dest.buf/Dest.len buffer
// - is a wrapper around BinarySave(rkRecordTypes)
procedure RecordSave(const Rec; var Dest: TSynTempBuffer; TypeInfo: pointer); overload;
  {$ifdef HASINLINE}inline;{$endif}

/// save a record content into a Base-64 encoded UTF-8 text content
// - will use RecordSave() format, with a left-sided binary CRC
// - is a wrapper around BinarySaveBase64(rkRecordTypes)
function RecordSaveBase64(const Rec; TypeInfo: pointer;
  UriCompatible: boolean = false): RawUTF8;
  {$ifdef HASINLINE}inline;{$endif}

/// fill a record content from a memory buffer as saved by RecordSave()
// - return nil if the Source buffer is incorrect
// - in case of success, return the memory buffer pointer just after the
// read content, and set the Rec size, in bytes, into Len reference variable
// - will use a proprietary binary format, with some variable-length encoding
// of the string length - note that if you change the type definition, any
// previously-serialized content will fail, maybe triggering unexpected GPF: you
// may use TypeInfoToHash() if you share this binary data accross executables
// - you can optionally provide in SourceMax the first byte after the input
// memory buffer, which will be used to avoid any unexpected buffer overflow -
// would be mandatory when decoding the content from any external process
// (e.g. a maybe-forged client) - only with slightly performance penalty
// - is a wrapper around BinaryLoad(rkRecordTypes)
function RecordLoad(var Rec; Source: PAnsiChar; TypeInfo: pointer;
  Len: PInteger = nil; SourceMax: PAnsiChar = nil;
  TryCustomVariants: PDocVariantOptions = nil): PAnsiChar; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fill a record content from a memory buffer as saved by RecordSave()
// - will use the Source length to detect and avoid any buffer overlow
// - returns false if the Source buffer was incorrect, true on success
// - is a wrapper around BinaryLoad(rkRecordTypes)
function RecordLoad(var Rec; const Source: RawByteString;
  TypeInfo: pointer; TryCustomVariants: PDocVariantOptions = nil): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// read a record content from a Base-64 encoded content
// - expects RecordSaveBase64() format, with a left-sided binary CRC32C
// - is a wrapper around BinaryLoadBase64(rkRecordTypes)
function RecordLoadBase64(Source: PAnsiChar; Len: PtrInt; var Rec; TypeInfo: pointer;
  UriCompatible: boolean = false; TryCustomVariants: PDocVariantOptions = nil): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// crc32c-based hash of a variant value
// - complex string types will make up to 255 uppercase characters conversion
// if CaseInsensitive is true
// - you can specify your own hashing function if crc32c is not what you expect
function VariantHash(const value: variant; CaseInsensitive: boolean;
  Hasher: THasher = nil): cardinal;


{ ************ TDynArray, TDynArrayHashed and TSynQueue Wrappers }

var
  /// low-level JSON unserialization function
  // - defined in this unit to avoid circular reference with mormot.core.json,
  // and be called by TDynArray.LoadFromJSON method
  // - this unit will just set a wrapper raising an ERttiException
  // - link mormot.core.json.pas to have a working implementation
  GetDataFromJSON: procedure(Data: pointer; var JSON: PUTF8Char; EndOfObject: PUTF8Char;
    TypeInfo: PRttiInfo; CustomVariantOptions: PDocVariantOptions; Tolerant: boolean);

type
  /// function prototype to be used for TDynArray Sort and Find method
  // - common functions exist for base types: see e.g. SortDynArrayboolean,
  // SortDynArrayByte, SortDynArrayWord, SortDynArrayInteger, SortDynArrayCardinal,
  // SortDynArrayInt64, SortDynArrayQWord, SordDynArraySingle, SortDynArrayDouble,
  // SortDynArrayAnsiString, SortDynArrayAnsiStringI, SortDynArrayUnicodeString,
  // SortDynArrayUnicodeStringI, SortDynArrayString, SortDynArrayStringI
  // - any custom type (even records) can be compared then sort by defining
  // such a custom function
  // - must return 0 if A=B, -1 if A<B, 1 if A>B
  TDynArraySortCompare = function(const A, B): integer;

  /// event oriented version of TDynArraySortCompare
  TEventDynArraySortCompare = function(const A, B): integer of object;

{$ifndef PUREMORMOT2}

type
  /// internal enumeration used to specify some standard arrays
  // - mORMot 1.18 did have two serialization engines - we unified it
  // - defined only for backward compatible code; use TRttiParserType instead
  TDynArrayKind = TRttiParserType;
  TDynArrayKinds = TRttiParserTypes;

const
  /// deprecated TDynArrayKind enumerate mapping
  // - defined only for backward compatible code; use TRttiParserType instead
  djNone = ptNone;
  djboolean = ptboolean;
  djByte = ptByte;
  djWord = ptWord;
  djInteger = ptInteger;
  djCardinal = ptCardinal;
  djSingle = ptSingle;
  djInt64 = ptInt64;
  djQWord = ptQWord;
  djDouble = ptDouble;
  djCurrency = ptCurrency;
  djTimeLog = ptTimeLog;
  djDateTime = ptDateTime;
  djDateTimeMS = ptDateTimeMS;
  djRawUTF8 = ptRawUTF8;
  djRawJSON = ptRawJSON;
  djWinAnsi = ptWinAnsi;
  djString = ptString;
  djRawByteString = ptRawByteString;
  djWideString = ptWideString;
  djSynUnicode = ptSynUnicode;
  djHash128 = ptHash128;
  djHash256 = ptHash256;
  djHash512 = ptHash512;
  djVariant = ptVariant;
  djCustom = ptCustom;
  djPointer = ptPtrInt;
  djObject = ptPtrInt;
  djUnmanagedTypes = ptUnmanagedTypes;
  djStringTypes = ptStringTypes;

{$endif PUREMORMOT2}

var
  /// helper array to get the comparison function corresponding to a given
  // standard array type
  // - e.g. as PT_SORT[CaseInSensitive,ptRawUTF8]
  // - not to be used as such, but e.g. when inlining TDynArray methods
  PT_SORT: array[boolean, TRttiParserType] of TDynArraySortCompare = (
    (nil, nil, SortDynArrayboolean, SortDynArrayByte, SortDynArrayCardinal,
     SortDynArrayInt64, SortDynArrayDouble, SortDynArrayExtended,
     SortDynArrayInt64, SortDynArrayInteger, SortDynArrayQWord,
     SortDynArrayRawByteString, SortDynArrayAnsiString, SortDynArrayAnsiString,
     nil, SortDynArraySingle, SortDynArrayString, SortDynArrayUnicodeString,
     SortDynArrayDouble, SortDynArrayDouble, SortDynArray128, SortDynArray128,
     SortDynArray256, SortDynArray512, SortDynArrayInt64, SortDynArrayInt64,
     SortDynArrayUnicodeString, SortDynArrayInt64, SortDynArrayInt64, SortDynArrayVariant,
     SortDynArrayUnicodeString, SortDynArrayAnsiString, SortDynArrayWord,
     nil, nil, nil, nil, nil, nil),
   (nil, nil, SortDynArrayboolean, SortDynArrayByte, SortDynArrayCardinal,
    SortDynArrayInt64, SortDynArrayDouble, SortDynArrayExtended,
    SortDynArrayInt64, SortDynArrayInteger, SortDynArrayQWord,
    SortDynArrayRawByteString, SortDynArrayAnsiStringI, SortDynArrayAnsiStringI,
    nil, SortDynArraySingle, SortDynArrayStringI, SortDynArrayUnicodeStringI,
    SortDynArrayDouble, SortDynArrayDouble, SortDynArray128, SortDynArray128,
    SortDynArray256, SortDynArray512, SortDynArrayInt64, SortDynArrayInt64,
    SortDynArrayUnicodeStringI, SortDynArrayInt64, SortDynArrayInt64, SortDynArrayVariantI,
    SortDynArrayUnicodeStringI, SortDynArrayAnsiStringI, SortDynArrayWord,
    nil, nil, nil, nil, nil, nil));

type
  TDynArrayObjArray = (oaUnknown, oaFalse, oaTrue);

  /// the kind of exceptions raised during TDynArray/TDynArrayHashed process
  EDynArray = class(ESynException);

  /// a pointer to a TDynArray Wrapper instance
  PDynArray = ^TDynArray;

  /// a wrapper around a dynamic array with one dimension
  // - provide TList-like methods using fast RTTI information
  // - can be used to fast save/retrieve all memory content to a TStream
  // - note that the "const Item" is not checked at compile time nor runtime:
  // you must ensure that Item matchs the element type of the dynamic array;
  // all Item*() methods will use pointers for safety
  // - can use external Count storage to make Add() and Delete() much faster
  // (avoid most reallocation of the memory buffer)
  // - Note that TDynArray is just a wrapper around an existing dynamic array:
  // methods can modify the content of the associated variable but the TDynArray
  // doesn't contain any data by itself. It is therefore aimed to initialize
  // a TDynArray wrapper on need, to access any existing dynamic array.
  // - is defined as an object or as a record, due to a bug
  // in Delphi 2009/2010 compiler (at least): this structure is not initialized
  // if defined as an object on the stack, but will be as a record :(
  {$ifdef UNDIRECTDYNARRAY}
  TDynArray = record
  {$else}
  TDynArray = object
  {$endif UNDIRECTDYNARRAY}
  private
    fValue: PPointer;
    fInfo: TRttiCustom;
    fCountP: PInteger;
    fCompare: TDynArraySortCompare;
    fElemSize: cardinal; // equals fInfo.Cache.ItemSize
    fSorted: boolean;
    function GetCount: PtrInt; {$ifdef HASINLINE}inline;{$endif}
    procedure SetCount(aCount: PtrInt);
    function GetCapacity: PtrInt; {$ifdef HASINLINE}inline;{$endif}
    procedure SetCapacity(aCapacity: PtrInt);
    procedure SetCompare(const aCompare: TDynArraySortCompare);
      {$ifdef HASINLINE}inline;{$endif}
    function FindIndex(const Item; aIndex: PIntegerDynArray;
      aCompare: TDynArraySortCompare): PtrInt;
    /// faster than RTL + handle T*ObjArray + ensure unique
    procedure InternalSetLength(OldLength, NewLength: PtrUInt);
  public
    /// initialize the wrapper with a one-dimension dynamic array
    // - the dynamic array must have been defined with its own type
    // (e.g. TIntegerDynArray = array of Integer)
    // - if aCountPointer is set, it will be used instead of length() to store
    // the dynamic array items count - it will be much faster when adding
    // items to the array, because the dynamic array won't need to be
    // resized each time - but in this case, you should use the Count property
    // instead of length(array) or high(array) when accessing the data: in fact
    // length(array) will store the memory size reserved, not the items count
    // - if aCountPointer is set, its content will be set to 0, whatever the
    // array length is, or the current aCountPointer^ value is
    // - a sample usage may be:
    // !var DA: TDynArray;
    // !    A: TIntegerDynArray;
    // !begin
    // !  DA.Init(TypeInfo(TIntegerDynArray),A);
    // ! (...)
    // - a sample usage may be (using a count variable):
    // !var DA: TDynArray;
    // !    A: TIntegerDynArray;
    // !    ACount: integer;
    // !    i: integer;
    // !begin
    // !  DA.Init(TypeInfo(TIntegerDynArray),A,@ACount);
    // !  for i := 1 to 100000 do
    // !    DA.Add(i); // MUCH faster using the ACount variable
    // ! (...)   // now you should use DA.Count or Count instead of length(A)
    procedure Init(aTypeInfo: PRttiInfo; var aValue; aCountPointer: PInteger = nil);
    /// initialize the wrapper with a one-dimension dynamic array
    // - also set the Compare() function from a supplied TRttiParserType
    // - ptNone and ptCustom are too vague, and will raise an exception
    // - no RTTI check is made over the corresponding array layout: you shall
    // ensure that the aKind parameter matches at least the first field of
    // the dynamic array item definition
    // - aCaseInsensitive will be used for ptStringTypes
    procedure InitSpecific(aTypeInfo: PRttiInfo; var aValue; aKind: TRttiParserType;
      aCountPointer: PInteger = nil; aCaseInsensitive: boolean = false);
    /// initialize the wrapper with a one-dimension dynamic array
    // - low-level method, as called by Init() and InitSpecific()
    // - can be called directly for a very fast TDynArray initialization
    procedure InitRtti(aInfo: TRttiCustom; var aValue; aCountPointer: PInteger = nil);
      {$ifdef HASINLINE}inline;{$endif}
    /// fast initialize a wrapper for an existing dynamic array of the same type
    // - is slightly faster than
    // ! Init(aAnother.ArrayType,aValue,nil);
    procedure InitFrom(aAnother: PDynArray; var aValue);
      {$ifdef HASINLINE}inline;{$endif}
    /// define the reference to an external count integer variable
    // - Init and InitSpecific methods will reset the aCountPointer to 0: you
    // can use this method to set the external count variable without overriding
    // the current value
    procedure UseExternalCount(var aCountPointer: Integer);
      {$ifdef HASINLINE}inline;{$endif}
    /// initialize the wrapper to point to no dynamic array
    procedure Void;
    /// check if the wrapper points to a dynamic array
    function IsVoid: boolean;
    /// add an element to the dynamic array
    // - warning: Item must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write Add(i+10) e.g.)
    // - returns the index of the added element in the dynamic array
    // - note that because of dynamic array internal memory managment, adding
    // may reallocate the list every time a record is added, unless an external
    // count variable has been specified in Init(...,@Count) method
    function Add(const Item): PtrInt;
    /// add an element to the dynamic array
    // - this version add a void element to the array, and returns its index
    // - note: if you use this method to add a new item with a reference to the
    // dynamic array, using a local variable is needed under FPC:
    // !    i := DynArray.New;
    // !    with Values[i] do begin // otherwise Values is nil -> GPF
    // !      Field1 := 1;
    // !      ...
    function New: integer;
    /// add an element to the dynamic array at the position specified by Index
    // - warning: Item must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write Insert(10,i+10) e.g.)
    procedure Insert(Index: PtrInt; const Item);
    /// get and remove the last element stored in the dynamic array
    // - Add + Pop/Peek will implement a LIFO (Last-In-First-Out) stack
    // - warning: Dest must be of the same exact type than the dynamic array
    // - returns true if the item was successfully copied and removed
    // - use Peek() if you don't want to remove the item
    function Pop(var Dest): boolean;
    /// get the last element stored in the dynamic array
    // - Add + Pop/Peek will implement a LIFO (Last-In-First-Out) stack
    // - warning: Dest must be of the same exact type than the dynamic array
    // - returns true if the item was successfully copied into Dest
    // - use Pop() if you also want to remove the item
    function Peek(var Dest): boolean;
    /// delete the whole dynamic array content
    // - this method will recognize T*ObjArray types and free all instances
    procedure Clear; {$ifdef HASINLINE}inline;{$endif}
    /// delete the whole dynamic array content, ignoring exceptions
    // - returns true if no exception occured when calling Clear, false otherwise
    // - you should better not call this method, which will catch and ignore
    // all exceptions - but it may somewhat make sense in a destructor
    // - this method will recognize T*ObjArray types and free all instances
    function ClearSafe: boolean;
    /// delete one item inside the dynamic array
    // - the deleted element is finalized if necessary
    // - this method will recognize T*ObjArray types and free all instances
    function Delete(aIndex: PtrInt): boolean;
    /// search for an element value inside the dynamic array
    // - return the index found (0..Count-1), or -1 if Item was not found
    // - will search for all properties content of Item: TList.IndexOf()
    // searches by address, this method searches by content using the RTTI
    // element description (and not the Compare property function)
    // - use the Find() method if you want the search via the Compare property
    // function, or e.g. to search only with some part of the element content
    // - will work with simple types: binaries (byte, word, integer, Int64,
    // Currency, array[0..255] of byte, packed records with no reference-counted
    // type within...), string types (e.g. array of string), and packed records
    // with binary and string types within (like TFileVersion)
    // - won't work with not packed types (like a shorstring, or a record
    // with byte or word fields with {$A+}): in this case, the padding data
    // (i.e. the bytes between the aligned fields) can be filled as random, and
    // there is no way with standard RTTI to identify randomness from values
    // - warning: Item must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write IndexOf(i+10) e.g.)
    function IndexOf(const Item; CaseInSensitive: boolean = true): PtrInt;
    /// search for an element value inside the dynamic array
    // - this method will use the Compare property function, or the supplied
    // aCompare for the search; call IndexOf() if you want to use RTTI search
    // - return the index found (0..Count-1), or -1 if Item was not found
    // - if the array is sorted, it will use fast O(log(n)) binary search
    // - if the array is not sorted, it will use slower O(n) iterating search
    // - warning: Item must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write Find(i+10) e.g.)
    function Find(const Item; aCompare: TDynArraySortCompare = nil): PtrInt; overload;
    /// search for an element value inside the dynamic array, from an external
    // indexed lookup table
    // - return the index found (0..Count-1), or -1 if Item was not found
    // - this method will use a custom comparison function, with an external
    // integer table, as created by the CreateOrderedIndex() method: it allows
    // multiple search orders in the same dynamic array content
    // - if an indexed lookup is supplied, it must already be sorted:
    // this function will then use fast O(log(n)) binary search
    // - if an indexed lookup is not supplied (i.e aIndex=nil),
    // this function will use slower but accurate O(n) iterating search
    // - warning; the lookup index should be synchronized if array content
    // is modified (in case of adding or deletion)
    function Find(const Item; const aIndex: TIntegerDynArray;
      aCompare: TDynArraySortCompare): PtrInt; overload;
    /// search for an element value, then fill all properties if match
    // - this method will use the Compare property function for the search,
    // or the supplied indexed lookup table and its associated compare function
    // - if Item content matches, all Item fields will be filled with the record
    // - can be used e.g. as a simple dictionary: if Compare will match e.g. the
    // first string field (i.e. set to SortDynArrayString), you can fill the
    // first string field with the searched value (if returned index is >= 0)
    // - return the index found (0..Count-1), or -1 if Item was not found
    // - if the array is sorted, it will use fast O(log(n)) binary search
    // - if the array is not sorted, it will use slower O(n) iterating search
    // - warning: Item must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write Find(i+10) e.g.)
    function FindAndFill(var Item; aIndex: PIntegerDynArray = nil;
      aCompare: TDynArraySortCompare = nil): integer;
    /// search for an element value, then delete it if match
    // - this method will use the Compare property function for the search,
    // or the supplied indexed lookup table and its associated compare function
    // - if Item content matches, this item will be deleted from the array
    // - can be used e.g. as a simple dictionary: if Compare will match e.g. the
    // first string field (i.e. set to SortDynArrayString), you can fill the
    // first string field with the searched value (if returned index is >= 0)
    // - return the index deleted (0..Count-1), or -1 if Item was not found
    // - if the array is sorted, it will use fast O(log(n)) binary search
    // - if the array is not sorted, it will use slower O(n) iterating search
    // - warning: Item must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write Find(i+10) e.g.)
    function FindAndDelete(const Item; aIndex: PIntegerDynArray = nil;
      aCompare: TDynArraySortCompare = nil): integer;
    /// search for an element value, then update the item if match
    // - this method will use the Compare property function for the search,
    // or the supplied indexed lookup table and its associated compare function
    // - if Item content matches, this item will be updated with the supplied value
    // - can be used e.g. as a simple dictionary: if Compare will match e.g. the
    // first string field (i.e. set to SortDynArrayString), you can fill the
    // first string field with the searched value (if returned index is >= 0)
    // - return the index found (0..Count-1), or -1 if Item was not found
    // - if the array is sorted, it will use fast O(log(n)) binary search
    // - if the array is not sorted, it will use slower O(n) iterating search
    // - warning: Item must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write Find(i+10) e.g.)
    function FindAndUpdate(const Item; aIndex: PIntegerDynArray = nil;
      aCompare: TDynArraySortCompare = nil): integer;
    /// search for an element value, then add it if none matched
    // - this method will use the Compare property function for the search,
    // or the supplied indexed lookup table and its associated compare function
    // - if no Item content matches, the item will added to the array
    // - can be used e.g. as a simple dictionary: if Compare will match e.g. the
    // first string field (i.e. set to SortDynArrayString), you can fill the
    // first string field with the searched value (if returned index is >= 0)
    // - return the index found (0..Count-1), or -1 if Item was not found and
    // the supplied element has been succesfully added
    // - if the array is sorted, it will use fast O(log(n)) binary search
    // - if the array is not sorted, it will use slower O(n) iterating search
    // - warning: Item must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write Find(i+10) e.g.)
    function FindAndAddIfNotExisting(const Item; aIndex: PIntegerDynArray = nil;
      aCompare: TDynArraySortCompare = nil): integer;
    /// sort the dynamic array items, using the Compare property function
    // - it will change the dynamic array content, and exchange all items
    // in order to be sorted in increasing order according to Compare function
    procedure Sort(aCompare: TDynArraySortCompare = nil); overload;
    /// sort some dynamic array items, using the Compare property function
    // - this method allows to sort only some part of the items
    // - it will change the dynamic array content, and exchange all items
    // in order to be sorted in increasing order according to Compare function
    procedure SortRange(aStart, aStop: integer; aCompare: TDynArraySortCompare = nil);
    /// sort the dynamic array items, using a Compare method (not function)
    // - it will change the dynamic array content, and exchange all items
    // in order to be sorted in increasing order according to Compare function,
    // unless aReverse is true
    // - it won't mark the array as Sorted, since the comparer is local
    procedure Sort(const aCompare: TEventDynArraySortCompare; aReverse: boolean = false); overload;
    /// search the items range which match a given value in a sorted dynamic array
    // - this method will use the Compare property function for the search
    // - returns TRUE and the matching indexes, or FALSE if none found
    // - if the array is not sorted, returns FALSE
    function FindAllSorted(const Item; out FirstIndex, LastIndex: Integer): boolean;
    /// search for an element value inside a sorted dynamic array
    // - this method will use the Compare property function for the search
    // - will be faster than a manual FindAndAddIfNotExisting+Sort process
    // - returns TRUE and the index of existing Item, or FALSE and the index
    // where the Item is to be inserted so that the array remains sorted
    // - you should then call FastAddSorted() later with the returned Index
    // - if the array is not sorted, returns FALSE and Index=-1
    // - warning: Item must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (no FastLocateSorted(i+10) e.g.)
    function FastLocateSorted(const Item; out Index: Integer): boolean;
    /// insert a sorted element value at the proper place
    // - the index should have been computed by FastLocateSorted(): false
    // - you may consider using FastLocateOrAddSorted() instead
    procedure FastAddSorted(Index: Integer; const Item);
    /// search and add an element value inside a sorted dynamic array
    // - this method will use the Compare property function for the search
    // - will be faster than a manual FindAndAddIfNotExisting+Sort process
    // - returns the index of the existing Item and wasAdded^=false
    // - returns the sorted index of the inserted Item and wasAdded^=true
    // - if the array is not sorted, returns -1 and wasAdded^=false
    // - is just a wrapper around FastLocateSorted+FastAddSorted
    function FastLocateOrAddSorted(const Item; wasAdded: Pboolean = nil): integer;
    /// delete a sorted element value at the proper place
    // - plain Delete(Index) would reset the fSorted flag to FALSE, so use
    // this method with a FastLocateSorted/FastAddSorted array
    procedure FastDeleteSorted(Index: Integer);
    /// will reverse all array items, in place
    procedure Reverse;
    /// sort the dynamic array items using a lookup array of indexes
    // - in comparison to the Sort method, this CreateOrderedIndex won't change
    // the dynamic array content, but only create (or update) the supplied
    // integer lookup array, using the specified comparison function
    // - if aCompare is not supplied, the method will use fCompare (if defined)
    // - you should provide either a void either a valid lookup table, that is
    // a table with one to one lookup (e.g. created with FillIncreasing)
    // - if the lookup table has less items than the main dynamic array,
    // its content will be recreated
    procedure CreateOrderedIndex(var aIndex: TIntegerDynArray;
      aCompare: TDynArraySortCompare); overload;
    /// sort the dynamic array items using a lookup array of indexes
    // - this overloaded method will use the supplied TSynTempBuffer for
    // index storage, so use PIntegerArray(aIndex.buf) to access the values
    // - caller should always make aIndex.Done once done
    procedure CreateOrderedIndex(out aIndex: TSynTempBuffer;
      aCompare: TDynArraySortCompare); overload;
    /// sort using a lookup array of indexes, after a Add()
    // - will resize aIndex if necessary, and set aIndex[Count-1] := Count-1
    procedure CreateOrderedIndexAfterAdd(var aIndex: TIntegerDynArray;
      aCompare: TDynArraySortCompare);
    /// save the dynamic array content into a (memory) stream
    // - will handle array of binaries values (byte, word, integer...), array of
    // strings or array of packed records, with binaries and string properties
    // - will use a proprietary binary format, with some variable-length encoding
    // of the string length - note that if you change the type definition, any
    // previously-serialized content will fail, maybe triggering unexpected GPF:
    // use SaveToTypeInfoHash if you share this binary data accross executables
    // - Stream position will be set just after the added data
    // - is optimized for memory streams, but will work with any kind of TStream
    procedure SaveToStream(Stream: TStream);
    /// load the dynamic array content from a (memory) stream
    // - stream content must have been created using SaveToStream method
    // - will handle array of binaries values (byte, word, integer...), array of
    // strings or array of packed records, with binaries and string properties
    // - will use a proprietary binary format, with some variable-length encoding
    // of the string length - note that if you change the type definition, any
    // previously-serialized content will fail, maybe triggering unexpected GPF:
    // use SaveToTypeInfoHash if you share this binary data accross executables
    procedure LoadFromStream(Stream: TCustomMemoryStream);
    /// save the dynamic array content using our binary serialization
    // - will use a proprietary binary format, with some variable-length encoding
    // of the string length - note that if you change the type definition, any
    // previously-serialized content will fail, maybe triggering unexpected GPF
    // - this method will raise an ESynException for T*ObjArray types
    // - use TDynArray.LoadFrom to decode the saved buffer
    // - warning: legacy Hash32 checksum will be stored as 0, so may be refused
    // by mORMot TDynArray.LoadFrom before 1.18.5966
    procedure SaveTo(W: TBufferWriter); overload;
    /// save the dynamic array content into a RawByteString
    // - will use a proprietary binary format, with some variable-length encoding
    // of the string length - note that if you change the type definition, any
    // previously-serialized content will fail, maybe triggering unexpected GPF:
    // use SaveToTypeInfoHash if you share this binary data accross executables
    // - this method will raise an ESynException for T*ObjArray types
    // - use TDynArray.LoadFrom to decode the saved buffer
    // - warning: legacy Hash32 checksum will be stored as 0, so may be refused
    // by mORMot TDynArray.LoadFrom before 1.18.5966
    function SaveTo: RawByteString; overload;
    /// unserialize dynamic array content from binary written by TDynArray.SaveTo
    // - return nil if the Source buffer is incorrect: invalid type, wrong
    // checksum, or optional SourceMax overflow
    // - return a non nil pointer just after the Source content on success
    // - this method will raise an ESynException for T*ObjArray types
    function LoadFrom(Source: PAnsiChar; SourceMax: PAnsiChar = nil): PAnsiChar;
    /// unserialize the dynamic array content from a TDynArray.SaveTo binary string
    // - same as LoadFrom, and will check for any buffer overflow since we
    // know the actual end of input buffer
    // - will read mORMot 1.18 binary content, but will ignore the Hash32
    // stored checksum which is not needed any more
    function LoadFromBinary(const Buffer: RawByteString): boolean;
    /// serialize the dynamic array content as JSON
    // - is just a wrapper around TTextWriter.AddTypedJSON()
    // - this method will therefore recognize T*ObjArray types
    function SaveToJSON(EnumSetsAsText: boolean = false;
      reformat: TTextWriterJSONFormat = jsonCompact): RawUTF8; overload;
      {$ifdef HASINLINE} inline;{$endif}
    /// serialize the dynamic array content as JSON
    // - is just a wrapper around TTextWriter.AddTypedJSON()
    // - this method will therefore recognize T*ObjArray types
    procedure SaveToJSON(out result: RawUTF8; EnumSetsAsText: boolean = false;
      reformat: TTextWriterJSONFormat = jsonCompact); overload;
    /// serialize the dynamic array content as JSON
    // - is just a wrapper around TTextWriter.AddTypedJSON()
    // - this method will therefore recognize T*ObjArray types
    procedure SaveToJSON(W: TBaseWriter); overload;
    /// load the dynamic array content from an UTF-8 encoded JSON buffer
    // - expect the format as saved by TTextWriter.AddDynArrayJSON method, i.e.
    // handling TbooleanDynArray, TIntegerDynArray, TInt64DynArray, TCardinalDynArray,
    // TDoubleDynArray, TCurrencyDynArray, TWordDynArray, TByteDynArray,
    // TRawUTF8DynArray, TWinAnsiDynArray, TRawByteStringDynArray,
    // TStringDynArray, TWideStringDynArray, TSynUnicodeDynArray,
    // TTimeLogDynArray and TDateTimeDynArray as JSON array - or any customized
    // valid JSON serialization as set by TTextWriter.RegisterCustomJSONSerializer
    // - or any other kind of array as Base64 encoded binary stream precessed
    // via JSON_BASE64_MAGIC (UTF-8 encoded \uFFF0 special code)
    // - typical handled content could be
    // ! '[1,2,3,4]' or '["\uFFF0base64encodedbinary"]'
    // - return a pointer at the end of the data read from P, nil in case
    // of an invalid input buffer
    // - this method will recognize T*ObjArray types, and will first free
    // any existing instance before unserializing, to avoid memory leak
    // - warning: the content of P^ will be modified during parsing: please
    // make a local copy if it will be needed later (using e.g. TSynTempBufer)
    function LoadFromJSON(P: PUTF8Char; EndOfObject: PUTF8Char = nil;
      CustomVariantOptions: PDocVariantOptions = nil;
      Tolerant: boolean = false): PUTF8Char;
    ///  select a sub-section (slice) of a dynamic array content
    procedure Slice(var Dest; aCount: Cardinal; aFirstIndex: cardinal = 0);
    /// add items from a given dynamic array variable
    // - the supplied source DynArray MUST be of the same exact type as the
    // current used for this TDynArray - warning: pass here a reference to
    // a "array of ..." variable, not another TDynArray instance; if you
    // want to add another TDynArray, use AddDynArray() method
    // - you can specify the start index and the number of items to take from
    // the source dynamic array (leave as -1 to add till the end)
    // - returns the number of items added to the array
    function AddArray(const DynArrayVar; aStartIndex: integer = 0;
      aCount: integer = -1): integer;
    /// add items from a given TDynArray
    // - the supplied source TDynArray MUST be of the same exact type as the
    // current used for this TDynArray, otherwise it won't do anything
    // - you can specify the start index and the number of items to take from
    // the source dynamic array (leave as -1 to add till the end)
    procedure AddDynArray(aSource: PDynArray; aStartIndex: integer = 0;
      aCount: integer = -1);
    /// compare the content of the two arrays, returning TRUE if both match
    // - use any supplied Compare property (unless ignorecompare=true), or
    // following the RTTI element description on all array items
    // - T*ObjArray kind of arrays will properly compare their properties
    function Equals(B: PDynArray; IgnoreCompare: boolean = false;
      CaseSensitive: boolean = true): boolean;
    /// compare the content of the two arrays
    // - use any supplied Compare property (unless ignorecompare=true), or
    // following the RTTI element description on all array items
    // - T*ObjArray kind of arrays will properly compare their properties
    function Compares(B: PDynArray; IgnoreCompare: boolean = false;
      CaseSensitive: boolean = true): integer;
    /// set all content of one dynamic array to the current array
    // - both must be of the same exact type
    // - T*ObjArray will be reallocated and copied by content (using a temporary
    // JSON serialization), unless ObjArrayByRef is true and pointers are copied
    procedure Copy(Source: PDynArray; ObjArrayByRef: boolean = false);
    /// set all content of one dynamic array to the current array
    // - both must be of the same exact type
    // - T*ObjArray will be reallocated and copied by content (using a temporary
    // JSON serialization), unless ObjArrayByRef is true and pointers are copied
    procedure CopyFrom(const Source; MaxItem: integer;
      ObjArrayByRef: boolean = false);
    /// set all content of the current dynamic array to another array variable
    // - both must be of the same exact type
    // - resulting length(Dest) will match the exact items count, even if an
    // external Count integer variable is used by this instance
    // - T*ObjArray will be reallocated and copied by content (using a temporary
    // JSON serialization), unless ObjArrayByRef is true and pointers are copied
    procedure CopyTo(out Dest; ObjArrayByRef: boolean = false);
    /// returns a pointer to an element of the array
    // - returns nil if aIndex is out of range
    // - since TDynArray is just a wrapper around an existing array, you should
    // better use direct access to its wrapped variable, and not this (slightly)
    // slower and more error prone method (such pointer access lacks of strong
    // typing abilities), which is designed for TDynArray abtract/internal use
    function ItemPtr(index: PtrInt): pointer; {$ifdef HASINLINE}inline;{$endif}
    /// will copy one element content from its index into another variable
    // - do nothing if index is out of range
    procedure ItemCopyAt(index: PtrInt; Dest: pointer); {$ifdef HASINLINE}inline;{$endif}
    /// will move one element content from its index into another variable
    // - will erase the internal item after copy
    // - do nothing if index is out of range
    procedure ItemMoveTo(index: PtrInt; Dest: pointer);
    /// will copy one variable content into an indexed element
    // - do nothing if index is out of range
    // - ClearBeforeCopy will call ItemClear() before the copy, which may be safer
    // if the source item is a copy of Values[index] with some dynamic arrays
    procedure ItemCopyFrom(Source: pointer; index: PtrInt;
      ClearBeforeCopy: boolean = false); {$ifdef HASINLINE}inline;{$endif}
    /// compare the content of two items, returning TRUE if both values equal
    // - use the Compare() property function (if set) or using Info.Cache.ItemInfo
    // if available - and fallbacks to binary comparison
    function ItemEquals(A, B: pointer; CaseInSensitive: boolean = false): boolean;
    /// compare the content of two items, returning -1, 0 or +1s
    // - use the Compare() property function (if set) or using Info.Cache.ItemInfo
    // if available - and fallbacks to binary comparison
    function ItemCompare(A, B: pointer; CaseInSensitive: boolean = false): integer;
    /// will reset the element content
    procedure ItemClear(Item: pointer);  {$ifdef HASINLINE}inline;{$endif}
    /// will copy one element content
    procedure ItemCopy(Source, Dest: pointer); {$ifdef HASINLINE}inline;{$endif}
    /// will copy the first field value of an array element
    // - will use the array KnownType to guess the copy routine to use
    // - returns false if the type information is not enough for a safe copy
    function ItemCopyFirstField(Source, Dest: Pointer): boolean;
    /// save an array element into a serialized binary content
    // - use the same layout as TDynArray.SaveTo, but for a single item
    // - you can use ItemLoad method later to retrieve its content
    // - warning: Item must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write ItemSave(i+10) e.g.)
    function ItemSave(Item: pointer): RawByteString;
    /// load an array element as saved by the ItemSave method into Item variable
    // - warning: Item must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write ItemLoad(P,i+10) e.g.)
    procedure ItemLoad(Source: PAnsiChar; Item: pointer;
      SourceMax: PAnsiChar = nil); overload;
    /// load an array element as saved by the ItemSave method
    // - this overloaded method will retrieve the element as a memory buffer,
    // which should be cleared by ItemLoadClear() before release
    function ItemLoad(Source: PAnsiChar;
      SourceMax: PAnsiChar = nil): RawByteString; overload;
    /// search for an array element as saved by the ItemSave method
    // - same as ItemLoad() + Find()/IndexOf() + ItemLoadClear()
    // - will call Find() method if Compare property is set
    // - will call generic IndexOf() method if no Compare property is set
    function ItemLoadFind(Source: PAnsiChar;
      SourceMax: PAnsiChar = nil): integer;
    /// finalize a temporary buffer used to store an element via ItemLoad()
    // - will release any managed type referenced inside the RawByteString,
    // then void the variable
    // - is just a wrapper around ItemClear(pointer(ItemTemp)) + ItemTemp := ''
    procedure ItemLoadClear(var ItemTemp: RawByteString);

    /// retrieve or set the number of items of the dynamic array
    // - same as length(DynArray) or SetLength(DynArray)
    // - this property will recognize T*ObjArray types, so will free any stored
    // instance if the array is sized down
    property Count: PtrInt read GetCount write SetCount;
    /// the internal buffer capacity
    // - if no external Count pointer was set with Init, is the same as Count
    // - if an external Count pointer is set, you can set a value to this
    // property before a massive use of the Add() method e.g.
    // - if no external Count pointer is set, set a value to this property
    // will affect the Count value, i.e. Add() will append after this count
    // - this property will recognize T*ObjArray types, so will free any stored
    // instance if the array is sized down
    property Capacity: PtrInt read GetCapacity write SetCapacity;
    /// the compare function to be used for Sort and Find methods
    // - by default, no comparison function is set
    // - common functions exist for base types: e.g. SortDynArrayByte, SortDynArrayboolean,
    // SortDynArrayWord, SortDynArrayInteger, SortDynArrayCardinal, SortDynArraySingle,
    // SortDynArrayInt64, SortDynArrayDouble, SortDynArrayAnsiString,
    // SortDynArrayAnsiStringI, SortDynArrayString, SortDynArrayStringI,
    // SortDynArrayUnicodeString, SortDynArrayUnicodeStringI
    property Compare: TDynArraySortCompare read fCompare write SetCompare;
    /// must be TRUE if the array is currently in sorted order according to
    // the compare function
    // - Add/Delete/Insert/Load* methods will reset this property to false
    // - Sort method will set this property to true
    // - you MUST set this property to false if you modify the dynamic array
    // content in your code, so that Find() won't try to wrongly use binary
    // search in an unsorted array, and miss its purpose
    property Sorted: boolean read fSorted write fSorted;

    /// low-level direct access to the storage variable
    property Value: PPointer read fValue;
    /// low-level extended RTTI access
    // - use e.g. Info.ArrayRtti to access the item RTTI, or Info.Cache.ItemInfo
    // to get the managed item TypeInfo()
    property Info: TRttiCustom read fInfo;
    /// just a convenient copy of Info.Cache.ItemSize
    property ElemSize: cardinal read fElemSize;
    /// low-level direct access to the external count (if defined at Init)
    property CountExternal: PInteger read fCountP;
  end;

  /// function prototype to be used for hashing of a dynamic array element
  // - this function must use the supplied hasher on the Item data
  TDynArrayHashOne = function(const Item; Hasher: THasher): cardinal;

  /// event handler to be used for hashing of a dynamic array element
  // - can be set as an alternative to TDynArrayHashOne
  TEventDynArrayHashOne = function(const Item): cardinal of object;

  {.$define DYNARRAYHASHCOLLISIONCOUNT}

  /// implements O(1) lookup to any dynamic array content
  // - this won't handle the storage process (like add/update), just efficiently
  // maintain a hash table over an existing dynamic array: several TDynArrayHasher
  // could be applied to a single TDynArray wrapper
  // - TDynArrayHashed will use a TDynArrayHasher on its own storage
  {$ifdef USERECORDWITHMETHODS}
  TDynArrayHasher = record
  {$else}
  TDynArrayHasher = object
  {$endif USERECORDWITHMETHODS}
  private
    DynArray: PDynArray;
    HashItem: TDynArrayHashOne;
    EventHash: TEventDynArrayHashOne;
    HashTable: TIntegerDynArray; // store 0 for void entry, or Index+1
    HashTableSize: integer;
    ScanCounter: integer; // Scan()>=0 up to CountTrigger*2
    State: set of (hasHasher, canHash);
    function HashTableIndex(aHashCode: PtrUInt): PtrUInt; {$ifdef HASINLINE}inline;{$endif}
    procedure HashAdd(aHashCode: cardinal; var result: integer);
    procedure HashDelete(aArrayIndex, aHashTableIndex: integer; aHashCode: cardinal);
    procedure RaiseFatalCollision(const caller: RawUTF8; aHashCode: cardinal);
  public
    /// associated item comparison - may differ from DynArray^.Compare
    Compare: TDynArraySortCompare;
    /// custom method-based comparison function
    EventCompare: TEventDynArraySortCompare;
    /// associated item hasher
    Hasher: THasher;
    /// after how many FindBeforeAdd() or Scan() the hashing starts - default 32
    CountTrigger: integer;
    {$ifdef DYNARRAYHASHCOLLISIONCOUNT}
    /// low-level access to an hash collisions counter
    FindCollisions: cardinal;
    {$endif DYNARRAYHASHCOLLISIONCOUNT}
    /// initialize the hash table for a given dynamic array storage
    // - you can call this method several times, e.g. if aCaseInsensitive changed
    procedure Init(aDynArray: PDynArray; aHashItem: TDynArrayHashOne;
     aEventHash: TEventDynArrayHashOne; aHasher: THasher; aCompare: TDynArraySortCompare;
     aEventCompare: TEventDynArraySortCompare; aCaseInsensitive: boolean);
    /// initialize a known hash table for a given dynamic array storage
    // - you can call this method several times, e.g. if aCaseInsensitive changed
    procedure InitSpecific(aDynArray: PDynArray; aKind: TRttiParserType;
      aCaseInsensitive: boolean; aHasher: THasher);
    /// allow custom hashing via a method event
    procedure SetEventHash(const event: TEventDynArrayHashOne);
    /// search for an element value inside the dynamic array without hashing
    // - trigger hashing if ScanCounter reaches CountTrigger*2
    function Scan(Item: pointer): integer;
    /// search for an element value inside the dynamic array with hashing
    function Find(Item: pointer): integer; overload;
    /// search for a hashed element value inside the dynamic array with hashing
    function Find(Item: pointer; aHashCode: cardinal): integer; overload;
    /// search for a hash position inside the dynamic array with hashing
    function Find(aHashCode: cardinal; aForAdd: boolean): integer; overload;
    /// returns position in array, or next void index in HashTable[] as -(index+1)
    function FindOrNew(aHashCode: cardinal; Item: pointer;
      aHashTableIndex: PInteger = nil): integer;
    /// search an hashed element value for adding, updating the internal hash table
    // - trigger hashing if Count reaches CountTrigger
    function FindBeforeAdd(Item: pointer; out wasAdded: boolean; aHashCode: cardinal): integer;
    /// search and delete an element value, updating the internal hash table
    function FindBeforeDelete(Item: pointer): integer;
    /// reset the hash table - no rehash yet
    procedure Clear;
    /// full computation of the internal hash table
    // - returns the number of duplicated values found
    function ReHash(forced, forceGrow: boolean): integer;
    /// compute the hash of a given item
    function HashOne(Item: pointer): cardinal; {$ifdef FPC_OR_DELPHIXE4}inline;{$endif}
      { not inlined to circumvent Delphi 2007=C1632, 2010=C1872, XE3=C2130 }
    /// retrieve the low-level hash of a given item
    function GetHashFromIndex(aIndex: PtrInt): cardinal;
  end;

  /// pointer to a TDynArrayHasher instance
  PDynArrayHasher = ^TDynArrayHasher;

type
  /// used to access any dynamic arrray items using fast hash
  // - by default, binary sort could be used for searching items for TDynArray:
  // using a hash is faster on huge arrays for implementing a dictionary
  // - in this current implementation, modification (update or delete) of an
  // element is not handled yet: you should rehash all content - only
  // TDynArrayHashed.FindHashedForAdding / FindHashedAndUpdate /
  // FindHashedAndDelete will refresh the internal hash
  // - this object extends the TDynArray type, since presence of Hashs[] dynamic
  // array will increase code size if using TDynArrayHashed instead of TDynArray
  // - in order to have the better performance, you should use an external Count
  // variable, AND set the Capacity property to the expected maximum count (this
  // will avoid most ReHash calls for FindHashedForAdding+FindHashedAndUpdate)
  // - consider using TSynDictionary from mormot.core.json for a thread-safe
  // stand-alone storage of key/value pairs
  {$ifdef UNDIRECTDYNARRAY}
  TDynArrayHashed = record
  // pseudo inheritance for most used methods
  private
    function GetCount: PtrInt; inline;
    procedure SetCount(aCount: PtrInt); inline;
    procedure SetCapacity(aCapacity: PtrInt); inline;
    function GetCapacity: PtrInt; inline;
  public
    InternalDynArray: TDynArray;
    function Value: PPointer; inline;
    function ElemSize: PtrUInt; inline;
    function Info: TRttiCustom; inline;
    procedure Clear; inline;
    procedure ItemCopy(Source, Dest: pointer); inline;
    function ItemPtr(index: PtrInt): pointer; inline;
    procedure ItemCopyAt(index: PtrInt; Dest: pointer); inline;
    // warning: you shall call ReHash() after manual Add/Delete
    function Add(const Item): integer; inline;
    procedure Delete(aIndex: PtrInt); inline;
    function SaveTo: RawByteString; overload; inline;
    procedure SaveTo(W: TBufferWriter); overload; inline;
    procedure Sort(aCompare: TDynArraySortCompare = nil); inline;
    function SaveToJSON(EnumSetsAsText: boolean = false;
      reformat: TTextWriterJSONFormat = jsonCompact): RawUTF8; overload; inline;
    procedure SaveToJSON(out result: RawUTF8; EnumSetsAsText: boolean = false;
      reformat: TTextWriterJSONFormat = jsonCompact); overload; inline;
    procedure SaveToJSON(W: TBaseWriter); overload; inline;
    function LoadFromJSON(P: PUTF8Char; aEndOfObject: PUTF8Char = nil;
      CustomVariantOptions: PDocVariantOptions = nil): PUTF8Char; inline;
    function LoadFrom(Source: PAnsiChar; SourceMax: PAnsiChar = nil): PAnsiChar; inline;
    function LoadFromBinary(const Buffer: RawByteString): boolean; inline;
    procedure CreateOrderedIndex(var aIndex: TIntegerDynArray;
      aCompare: TDynArraySortCompare);
    property Count: PtrInt read GetCount write SetCount;
    property Capacity: PtrInt read GetCapacity write SetCapacity;
  private
  {$else UNDIRECTDYNARRAY}
  TDynArrayHashed = object(TDynArray)
  protected
  {$endif UNDIRECTDYNARRAY}
    fHash: TDynArrayHasher;
    procedure SetEventHash(const event: TEventDynArrayHashOne);
      {$ifdef HASINLINE}inline;{$endif}
    function GetHashFromIndex(aIndex: PtrInt): Cardinal;
      {$ifdef HASINLINE}inline;{$endif}
  public
    /// initialize the wrapper with a one-dimension dynamic array
    // - this version accepts some hash-dedicated parameters: aHashItem to
    // set how to hash each element, aCompare to handle hash collision
    // - if no aHashItem is supplied, it will hash according to the RTTI, i.e.
    // strings or binary types, and the first field for records (strings included)
    // - if no aCompare is supplied, it will use default Equals() method
    // - if no THasher function is supplied, it will use the one supplied in
    // DefaultHasher global variable, set to crc32c() by default - using
    // SSE4.2 instruction if available
    // - if CaseInsensitive is set to TRUE, it will ignore difference in 7 bit
    // alphabetic characters (e.g. compare 'a' and 'A' as equal)
    procedure Init(aTypeInfo: pointer; var aValue; aHashItem: TDynArrayHashOne = nil;
      aCompare: TDynArraySortCompare = nil; aHasher: THasher = nil;
      aCountPointer: PInteger = nil; aCaseInsensitive: boolean = false);
    /// initialize the wrapper with a one-dimension dynamic array
    // - this version accepts to specify how both hashing and comparison should
    // occur, setting the TRttiParserType kind of first/hashed field
    // - djNone and djCustom are too vague, and will raise an exception
    // - no RTTI check is made over the corresponding array layout: you shall
    // ensure that aKind matches the dynamic array element definition
    // - aCaseInsensitive will be used for djRawUTF8..djHash512 text comparison
    procedure InitSpecific(aTypeInfo: pointer; var aValue; aKind: TRttiParserType;
      aCountPointer: PInteger = nil; aCaseInsensitive: boolean = false;
      aHasher: THasher = nil);
    /// will compute all hash from the current items of the dynamic array
    // - is called within the TDynArrayHashed.Init method to initialize the
    // internal hash array
    // - can be called on purpose, when modifications have been performed on
    // the dynamic array content (e.g. in case of element deletion or update,
    // or after calling LoadFrom/Clear method) - this is not necessary after
    // FindHashedForAdding / FindHashedAndUpdate / FindHashedAndDelete methods
    // - returns the number of duplicated items found - which won't be available
    // by hashed FindHashed() by definition
    function ReHash(forAdd: boolean = false; forceGrow: boolean = false): integer;
    /// search for an element value inside the dynamic array using hashing
    // - Item should be of the type expected by both the hash function and
    // Equals/Compare methods: e.g. if the searched/hashed field in a record is
    // a string as first field, you can safely use a string variable as Item
    // - Item must refer to a variable: e.g. you can't write FindHashed(i+10)
    // - will call fHashItem(Item,fHasher) to compute the needed hash
    // - returns -1 if not found, or the index in the dynamic array if found
    function FindHashed(const Item): integer;
    /// search for an element value inside the dynamic array using its hash
    // - returns -1 if not found, or the index in the dynamic array if found
    // - aHashCode parameter constains an already hashed value of the item,
    // to be used e.g. after a call to HashFind()
    function FindFromHash(const Item; aHashCode: cardinal): integer;
    /// search for an element value inside the dynamic array using hashing, and
    // fill ItemToFill with the found content
    // - return the index found (0..Count-1), or -1 if Item was not found
    // - ItemToFill should be of the type expected by the dynamic array, since
    // all its fields will be set on match
    function FindHashedAndFill(var ItemToFill): integer;
    /// search for an element value inside the dynamic array using hashing, and
    // add a void entry to the array if was not found (unless noAddEntry is set)
    // - this method will use hashing for fast retrieval
    // - Item should be of the type expected by both the hash function and
    // Equals/Compare methods: e.g. if the searched/hashed field in a record is
    // a string as first field, you can safely use a string variable as Item
    // - returns either the index in the dynamic array if found (and set wasAdded
    // to false), either the newly created index in the dynamic array (and set
    // wasAdded to true)
    // - for faster process (avoid ReHash), please set the Capacity property
    // - warning: in contrast to the Add() method, if an entry is added to the
    // array (wasAdded=true), the entry is left VOID: you must set the field
    // content to expecting value - in short, Item is used only for searching,
    // not copied to the newly created entry in the array  - check
    // FindHashedAndUpdate() for a method actually copying Item fields
    function FindHashedForAdding(const Item; out wasAdded: boolean;
      noAddEntry: boolean = false): integer; overload;
    /// search for an element value inside the dynamic array using hashing, and
    // add a void entry to the array if was not found (unless noAddEntry is set)
    // - overloaded method acepting an already hashed value of the item, to be used
    // e.g. after a call to HashFind()
    function FindHashedForAdding(const Item; out wasAdded: boolean;
      aHashCode: cardinal; noAddEntry: boolean = false): integer; overload;
    /// ensure a given element name is unique, then add it to the array
    // - expected element layout is to have a RawUTF8 field at first position
    // - the aName is searched (using hashing) to be unique, and if not the case,
    // an ESynException.CreateUTF8() is raised with the supplied arguments
    // - use internaly FindHashedForAdding method
    // - this version will set the field content with the unique value
    // - returns a pointer to the newly added element (to set other fields)
    function AddUniqueName(const aName: RawUTF8; const ExceptionMsg: RawUTF8;
      const ExceptionArgs: array of const; aNewIndex: PInteger = nil): pointer; overload;
    /// ensure a given element name is unique, then add it to the array
    // - just a wrapper to AddUniqueName(aName,'',[],aNewIndex)
    function AddUniqueName(const aName: RawUTF8; aNewIndex: PInteger = nil): pointer; overload;
    /// search for a given element name, make it unique, and add it to the array
    // - expected element layout is to have a RawUTF8 field at first position
    // - the aName is searched (using hashing) to be unique, and if not the case,
    // some suffix is added to make it unique
    // - use internaly FindHashedForAdding method
    // - this version will set the field content with the unique value
    // - returns a pointer to the newly added element (to set other fields)
    function AddAndMakeUniqueName(aName: RawUTF8): pointer;
    /// search for an element value inside the dynamic array using hashing, then
    // update any matching item, or add the item if none matched
    // - by design, hashed field shouldn't have been modified by this update,
    // otherwise the method won't be able to find and update the old hash: in
    // this case, you should first call FindHashedAndDelete(OldItem) then
    // FindHashedForAdding(NewItem) to properly handle the internal hash table
    // - if AddIfNotExisting is FALSE, returns the index found (0..Count-1),
    // or -1 if Item was not found - update will force slow rehash all content
    // - if AddIfNotExisting is TRUE, returns the index found (0..Count-1),
    // or the index newly created/added is the Item value was not matching -
    // add won't rehash all content - for even faster process (avoid ReHash),
    // please set the Capacity property
    // - Item should be of the type expected by the dynamic array, since its
    // content will be copied into the dynamic array, and it must refer to a
    // variable: e.g. you can't write FindHashedAndUpdate(i+10)
    function FindHashedAndUpdate(const Item; AddIfNotExisting: boolean): integer;
    /// search for an element value inside the dynamic array using hashing, and
    // delete it if matchs
    // - return the index deleted (0..Count-1), or -1 if Item was not found
    // - can optionally copy the deleted item to FillDeleted^ before erased
    // - Item should be of the type expected by both the hash function and
    // Equals/Compare methods, and must refer to a variable: e.g. you can't
    // write FindHashedAndDelete(i+10)
    // - it won't call slow ReHash but refresh the hash table as needed
    function FindHashedAndDelete(const Item; FillDeleted: pointer = nil;
      noDeleteEntry: boolean = false): integer;
    /// will search for an element value inside the dynamic array without hashing
    // - is used internally when Count < HashCountTrigger
    // - is preferred to Find(), since EventCompare would be used if defined
    // - Item should be of the type expected by both the hash function and
    // Equals/Compare methods, and must refer to a variable: e.g. you can't
    // write Scan(i+10)
    // - returns -1 if not found, or the index in the dynamic array if found
    // - an internal algorithm can switch to hashing if Scan() is called often,
    // even if the number of items is lower than HashCountTrigger
    function Scan(const Item): integer;
    /// retrieve the hash value of a given item, from its index
    property Hash[aIndex: PtrInt]: Cardinal read GetHashFromIndex;
    /// alternative event-oriented Compare function to be used for Sort and Find
    // - will be used instead of Compare, to allow object-oriented callbacks
    property EventCompare: TEventDynArraySortCompare
      read fHash.EventCompare write fHash.EventCompare;
    /// custom hash function to be used for hashing of a dynamic array element
    property HashItem: TDynArrayHashOne read fHash.HashItem;
    /// alternative event-oriented Hash function for ReHash
    // - this object-oriented callback will be used instead of HashItem()
    // on each dynamic array entries - HashItem will still be used on
    // const Item values, since they may be just a sub part of the stored entry
    property EventHash: TEventDynArrayHashOne read fHash.EventHash write SetEventHash;
    /// after how many items the hashing take place
    // - for smallest arrays, O(n) search if faster than O(1) hashing, since
    // maintaining internal hash table has some CPU and memory costs
    // - internal search is able to switch to hashing if it founds out that it
    // may have some benefit, e.g. if Scan() is called 2*HashCountTrigger times
    // - equals 32 by default, i.e. start hashing when Count reaches 32 or
    // manual Scan() is called 64 times
    property HashCountTrigger: integer read fHash.CountTrigger write fHash.CountTrigger;
    /// access to the internal hash table
    // - you can call e.g. Hasher.Clear to invalidate the whole hash table
    property Hasher: TDynArrayHasher read fHash;
  end;


/// initialize the structure with a one-dimension dynamic array
// - the dynamic array must have been defined with its own type
// (e.g. TIntegerDynArray = array of Integer)
// - if aCountPointer is set, it will be used instead of length() to store
// the dynamic array items count - it will be much faster when adding
// elements to the array, because the dynamic array won't need to be
// resized each time - but in this case, you should use the Count property
// instead of length(array) or high(array) when accessing the data: in fact
// length(array) will store the memory size reserved, not the items count
// - if aCountPointer is set, its content will be set to 0, whatever the
// array length is, or the current aCountPointer^ value is
// - a typical usage could be:
// !var IntArray: TIntegerDynArray;
// !begin
// !  with DynArray(TypeInfo(TIntegerDynArray),IntArray) do
// !  begin
// !    (...)
// !  end;
// ! (...)
// ! DynArray(TypeInfo(TIntegerDynArray),IntArrayA).SaveTo
function DynArray(aTypeInfo: pointer; var aValue;
  aCountPointer: PInteger = nil): TDynArray;
  {$ifdef HASINLINE}inline;{$endif}

/// sort any dynamic array, via an external array of indexes
// - this function will use the supplied TSynTempBuffer for index storage,
// so use PIntegerArray(Indexes.buf) to access the values
// - caller should always make Indexes.Done once done
procedure DynArraySortIndexed(Values: pointer; ElemSize, Count: Integer;
  out Indexes: TSynTempBuffer; Compare: TDynArraySortCompare);

var
  /// helper array to get the hash function corresponding to a given
  // standard array type
  // - e.g. as PT_HASH[CaseInSensitive,ptRawUTF8]
  // - not to be used as such, but e.g. when inlining TDynArray methods
  PT_HASH: array[{caseinsensitive=}boolean, TRttiParserType] of TDynArrayHashOne;

{$ifdef CPU32DELPHI}
const
  /// defined for inlining bitwise division in TDynArrayHasher.HashTableIndex
  // - HashTableSize<=HASH_PO2 is expected to be a power of two (fast binary op);
  // limit is set to 262,144 hash table slots (=1MB), for Capacity=131,072 items
  // - above this limit, a set of increasing primes is used; using a prime as
  // hashtable modulo enhances its distribution, especially for a weak hash function
  // - 64-bit CPU and FPC can efficiently compute a prime reduction using Lemire
  // algorithm, so no power of two is defined on those targets
  HASH_PO2 = 1 shl 18;
{$endif CPU32DELPHI}

type
  /// thread-safe FIFO (First-In-First-Out) in-order queue of records
  // - uses internally a TDynArray storage, with a sliding algorithm, more
  // efficient than the FPC or Delphi TQueue, or a naive TDynArray.Add/Delete
  // - supports efficient binary persistence, if needed
  // - this structure is also thread-safe by design
  TSynQueue = class(TSynPersistentStore)
  protected
    fValues: TDynArray;
    fValueVar: pointer;
    fCount, fFirst, fLast: integer;
    fWaitPopFlags: set of (wpfDestroying);
    fWaitPopCounter: integer;
    procedure InternalGrow;
    function InternalDestroying(incPopCounter: integer): boolean;
    function InternalWaitDone(endtix: Int64; const idle: TThreadMethod): boolean;
    /// low-level virtual methods implementing the persistence
    procedure LoadFromReader; override;
    procedure SaveToWriter(aWriter: TBufferWriter); override;
  public
    /// initialize the queue storage
    // - aTypeInfo should be a dynamic array TypeInfo() RTTI pointer, which
    // would store the values within this TSynQueue instance
    // - a name can optionally be assigned to this instance
    constructor Create(aTypeInfo: PRttiInfo;
      const aName: RawUTF8 = ''); reintroduce; virtual;
    /// finalize the storage
    // - would release all internal stored values, and call WaitPopFinalize
    destructor Destroy; override;
    /// store one item into the queue
    // - this method is thread-safe, since it will lock the instance
    procedure Push(const aValue);
    /// extract one item from the queue, as FIFO (First-In-First-Out)
    // - returns true if aValue has been filled with a pending item, which
    // is removed from the queue (use Peek if you don't want to remove it)
    // - returns false if the queue is empty
    // - this method is thread-safe, since it will lock the instance
    function Pop(out aValue): boolean;
    /// extract one matching item from the queue, as FIFO (First-In-First-Out)
    // - the current pending item is compared with aAnother value
    function PopEquals(aAnother: pointer; aCompare: TDynArraySortCompare;
      out aValue): boolean;
    /// lookup one item from the queue, as FIFO (First-In-First-Out)
    // - returns true if aValue has been filled with a pending item, without
    // removing it from the queue (as Pop method does)
    // - returns false if the queue is empty
    // - this method is thread-safe, since it will lock the instance
    function Peek(out aValue): boolean;
    /// waiting extract of one item from the queue, as FIFO (First-In-First-Out)
    // - returns true if aValue has been filled with a pending item within the
    // specified aTimeoutMS time
    // - returns false if nothing was pushed into the queue in time, or if
    // WaitPopFinalize has been called
    // - aWhenIdle could be assigned e.g. to VCL/LCL Application.ProcessMessages
    // - you can optionally compare the pending item before returning it (could
    // be used e.g. when several threads are putting items into the queue)
    // - this method is thread-safe, but will lock the instance only if needed
    function WaitPop(aTimeoutMS: integer; const aWhenIdle: TThreadMethod;
      out aValue; aCompared: pointer = nil;
      aCompare: TDynArraySortCompare = nil): boolean;
    /// waiting lookup of one item from the queue, as FIFO (First-In-First-Out)
    // - returns a pointer to a pending item within the specified aTimeoutMS
    // time - the Safe.Lock is still there, so that caller could check its content,
    // then call Pop() if it is the expected one, and eventually always call Safe.Unlock
    // - returns nil if nothing was pushed into the queue in time
    // - this method is thread-safe, but will lock the instance only if needed
    function WaitPeekLocked(aTimeoutMS: integer;
      const aWhenIdle: TThreadMethod): pointer;
    /// ensure any pending or future WaitPop() returns immediately as false
    // - is always called by Destroy destructor
    // - could be also called e.g. from an UI OnClose event to avoid any lock
    // - this method is thread-safe, but will lock the instance only if needed
    procedure WaitPopFinalize(aTimeoutMS: integer=100);
    /// delete all items currently stored in this queue, and void its capacity
    // - this method is thread-safe, since it will lock the instance
    procedure Clear;
    /// initialize a dynamic array with the stored queue items
    // - aDynArrayValues should be a variable defined as aTypeInfo from Create
    // - you can retrieve an optional TDynArray wrapper, e.g. for binary or JSON
    // persistence
    // - this method is thread-safe, and will make a copy of the queue data
    procedure Save(out aDynArrayValues; aDynArray: PDynArray = nil); overload;
    /// returns how many items are currently stored in this queue
    // - this method is thread-safe
    function Count: Integer;
    /// returns how much slots is currently reserved in memory
    // - the queue has an optimized auto-sizing algorithm, you can use this
    // method to return its current capacity
    // - this method is thread-safe
    function Capacity: integer;
    /// returns true if there are some items currently pending in the queue
    // - slightly faster than checking Count=0, and much faster than Pop or Peek
    function Pending: boolean;
  end;



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
function UpdateIniNameValue(var Content: RawUTF8;
  const Name, UpperName, NewValue: RawUTF8): boolean;

/// returns TRUE if the supplied HTML Headers contains 'Content-Type: text/...',
// 'Content-Type: application/json' or 'Content-Type: application/xml'
function IsHTMLContentTypeTextual(Headers: PUTF8Char): boolean;


{ ************ RawUTF8 String Values Interning and TRawUTF8List }

type
  /// used to store one list of hashed RawUTF8 in TRawUTF8Interning pool
  // - Delphi "object" is buggy on stack -> also defined as record with methods
  {$ifdef USERECORDWITHMETHODS}
  TRawUTF8InterningSlot = record
  {$else}
  TRawUTF8InterningSlot = object
  {$endif USERECORDWITHMETHODS}
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
    // - any string with an usage count <= aMaxRefCount will be removed
    function Clean(aMaxRefCount: TRefCnt): integer;
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
    // (1, 2, 4, 8, 16, 32, 64, 128, 256, 512)
    constructor Create(aHashTables: integer = 4); reintroduce;
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
    // - see also UniqueVariant() from mormot.core.variants if you want to
    // intern only non-numerical values
    procedure UniqueVariant(var aResult: variant; const aText: RawUTF8); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// return a variant containing a RawUTF8 stored within this class
    // - similar to RawUTF8ToVariant(StringToUTF8()), but with string interning
    // - this method expects the text to be supplied as a VCL string, which will
    // be converted into a variant containing a RawUTF8 varString instance
    procedure UniqueVariantString(var aResult: variant; const aText: string);
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
    function Clean(aMaxRefCount: TRefCnt = 1): integer;
    /// how many items are currently stored in this instance
    function Count: integer;
  end;

  /// possible values used by TRawUTF8List.Flags
  TRawUTF8ListFlags = set of (
    fObjectsOwned, fCaseSensitive, fNoDuplicate, fOnChangeTrigerred);

  /// TStringList-class optimized to work with our native UTF-8 string type
  // - can optionally store associated some TObject instances
  // - high-level methods of this class are thread-safe
  // - if fNoDuplicate flag is defined, an internal hash table will be
  // maintained to perform IndexOf() lookups in O(1) linear way
  TRawUTF8List = class(TSynLocked)
  protected
    fCount: PtrInt;
    fValue: TRawUTF8DynArray;
    fValues: TDynArrayHashed;
    fObjects: TObjectDynArray;
    fFlags: TRawUTF8ListFlags;
    fNameValueSep: AnsiChar;
    fOnChange, fOnChangeBackupForBeginUpdate: TNotifyEvent;
    fOnChangeLevel: integer;
    function GetCount: PtrInt; {$ifdef HASINLINE}inline;{$endif}
    procedure SetCapacity(const capa: PtrInt);
    function GetCapacity: PtrInt;
    function Get(Index: PtrInt): RawUTF8; {$ifdef HASINLINE}inline;{$endif}
    procedure Put(Index: PtrInt; const Value: RawUTF8);
    function GetObject(Index: PtrInt): pointer; {$ifdef HASINLINE}inline;{$endif}
    procedure PutObject(Index: PtrInt; Value: pointer);
    function GetName(Index: PtrInt): RawUTF8;
    function GetValue(const Name: RawUTF8): RawUTF8;
    procedure SetValue(const Name, Value: RawUTF8);
    function GetTextCRLF: RawUTF8;
    procedure SetTextCRLF(const Value: RawUTF8);
    procedure SetTextPtr(P,PEnd: PUTF8Char; const Delimiter: RawUTF8);
    function GetTextPtr: PPUtf8CharArray; {$ifdef HASINLINE}inline;{$endif}
    function GetNoDuplicate: boolean;     {$ifdef HASINLINE}inline;{$endif}
    function GetObjectPtr: PPointerArray; {$ifdef HASINLINE}inline;{$endif}
    function GetCaseSensitive: boolean;   {$ifdef HASINLINE}inline;{$endif}
    procedure SetCaseSensitive(Value: boolean); virtual;
    procedure Changed; virtual;
    procedure InternalDelete(Index: PtrInt);
    procedure OnChangeHidden(Sender: TObject);
  public
    /// initialize the RawUTF8/Objects storage with [fCaseSensitive] flags
    constructor Create; overload; override;
    /// initialize the RawUTF8/Objects storage
    // - by default, any associated Objects[] are just weak references;
    // you may supply fOwnObjects flag to force object instance management
    // - if you want the stored text items to be unique, set fNoDuplicate
    // and then an internal hash table will be maintained for fast IndexOf()
    // - you can unset fCaseSensitive to let the UTF-8 lookup be case-insensitive
    constructor Create(aFlags: TRawUTF8ListFlags); reintroduce; overload;
    {$ifndef PUREMORMOT2}
    /// backward compatiliby overloaded constructor
    // - please rather use the overloaded Create(TRawUTF8ListFlags)
    constructor Create(aOwnObjects: boolean; aNoDuplicate: boolean = false;
      aCaseSensitive: boolean = true); reintroduce; overload;
    {$endif PUREMORMOT2}
    /// finalize the internal objects stored
    // - if instance was created with fOwnObjects flag
    destructor Destroy; override;
    /// get a stored Object item by its associated UTF-8 text
    // - returns nil and raise no exception if aText doesn't exist
    // - thread-safe method, unless returned TObject is deleted in the background
    function GetObjectFrom(const aText: RawUTF8): pointer;
    /// store a new RawUTF8 item
    // - without the fNoDuplicate flag, it will always add the supplied value
    // - if fNoDuplicate was set and aText already exists (using the internal
    // hash table), it will return -1 unless aRaiseExceptionIfExisting is forced
    // - thread-safe method
    function Add(const aText: RawUTF8;
      aRaiseExceptionIfExisting: boolean = false): PtrInt; {$ifdef HASINLINE}inline;{$endif}
    /// store a new RawUTF8 item, and its associated TObject
    // - without the fNoDuplicate flag, it will always add the supplied value
    // - if fNoDuplicate was set and aText already exists (using the internal hash
    // table), it will return -1 unless aRaiseExceptionIfExisting is forced;
    // optionally freeing the supplied aObject if aFreeAndReturnExistingObject
    // is true, in which pointer the existing Objects[] is copied (see
    // AddObjectUnique as a convenient wrapper around this behavior)
    // - thread-safe method
    function AddObject(const aText: RawUTF8; aObject: TObject;
      aRaiseExceptionIfExisting: boolean = false;
      aFreeAndReturnExistingObject: PPointer = nil): PtrInt;
    /// try to store a new RawUTF8 item and its associated TObject
    // - fNoDuplicate should have been specified in the list flags
    // - if aText doesn't exist, will add the values
    // - if aText exist, will call aObjectToAddOrFree.Free and set the value
    // already stored in Objects[] into aObjectToAddOrFree - allowing dual
    // commit thread-safe update of the list, e.g. after a previous unsuccessful
    // call to GetObjectFrom(aText)
    // - thread-safe method, using an internal Hash Table to speedup IndexOf()
    // - in fact, this method is just a wrapper around
    // ! AddObject(aText,aObjectToAddOrFree^,false,@aObjectToAddOrFree);
    procedure AddObjectUnique(const aText: RawUTF8; aObjectToAddOrFree: PPointer);
      {$ifdef HASINLINE}inline;{$endif}
    /// append a specified list to the current content
    // - thread-safe method
    procedure AddRawUTF8List(List: TRawUTF8List);
    /// delete a stored RawUTF8 item, and its associated TObject
    // - raise no exception in case of out of range supplied index
    // - this method is not thread-safe: use Safe.Lock/UnLock if needed
    procedure Delete(Index: PtrInt); overload;
    /// delete a stored RawUTF8 item, and its associated TObject
    // - will search for the value using IndexOf(aText), and returns its index
    // - returns -1 if no entry was found and deleted
    // - thread-safe method, using the internal Hash Table if fNoDuplicate is set
    function Delete(const aText: RawUTF8): PtrInt; overload;
    /// delete a stored RawUTF8 item, and its associated TObject, from
    // a given Name when stored as 'Name=Value' pairs
    // - raise no exception in case of out of range supplied index
    // - thread-safe method, but not using the internal Hash Table
    // - consider using TSynNameValue if you expect efficient name/value process
    function DeleteFromName(const Name: RawUTF8): PtrInt; virtual;
    /// find the index of a given Name when stored as 'Name=Value' pairs
    // - search on Name is case-insensitive with 'Name=Value' pairs
    // - this method is not thread-safe, and won't use the internal Hash Table
    // - consider using TSynNameValue if you expect efficient name/value process
    function IndexOfName(const Name: RawUTF8): PtrInt;
    /// access to the Value of a given 'Name=Value' pair at a given position
    // - this method is not thread-safe
    // - consider using TSynNameValue if you expect efficient name/value process
    function GetValueAt(Index: PtrInt): RawUTF8;
    /// retrieve Value from an existing Name=Value, then optinally delete the entry
    // - if Name is found, will fill Value with the stored content and return true
    // - if Name is not found, Value is not modified, and false is returned
    // - thread-safe method, but not using the internal Hash Table
    // - consider using TSynNameValue if you expect efficient name/value process
    function UpdateValue(const Name: RawUTF8; var Value: RawUTF8;
      ThenDelete: boolean): boolean;
    /// retrieve and delete the first RawUTF8 item in the list
    // - could be used as a FIFO, calling Add() as a "push" method
    // - thread-safe method
    function PopFirst(out aText: RawUTF8; aObject: PObject = nil): boolean;
    /// retrieve and delete the last RawUTF8 item in the list
    // - could be used as a FILO, calling Add() as a "push" method
    // - thread-safe method
    function PopLast(out aText: RawUTF8; aObject: PObject = nil): boolean;
    /// erase all stored RawUTF8 items
    // - and corresponding objects (if aOwnObjects was true at constructor)
    // - thread-safe method, also clearing the internal Hash Table
    procedure Clear; virtual;
    /// find a RawUTF8 item in the stored Strings[] list
    // - this search is case sensitive if fCaseSensitive flag was set (which
    // is the default)
    // - this method is not thread-safe since the internal list may change
    // and the returned index may not be accurate any more
    // - see also GetObjectFrom()
    // - uses the internal Hash Table if fNoDuplicate was set
    function IndexOf(const aText: RawUTF8): PtrInt;
    /// find a TObject item index in the stored Objects[] list
    // - this method is not thread-safe since the internal list may change
    // and the returned index may not be accurate any more
    // - aObject lookup won't use the internal Hash Table
    function IndexOfObject(aObject: TObject): PtrInt;
    /// search for any RawUTF8 item containing some text
    // - uses PosEx() on the stored lines
    // - this method is not thread-safe since the internal list may change
    // and the returned index may not be accurate any more
    // - by design, aText lookup can't use the internal Hash Table
    function Contains(const aText: RawUTF8; aFirstIndex: integer = 0): PtrInt;
    /// retrieve the all lines, separated by the supplied delimiter
    // - this method is thread-safe
    function GetText(const Delimiter: RawUTF8 = #13#10): RawUTF8;
    /// the OnChange event will be raised only when EndUpdate will be called
    // - this method will also call Safe.Lock for thread-safety
    procedure BeginUpdate;
    /// call the OnChange event if changes occured
    // - this method will also call Safe.UnLock for thread-safety
    procedure EndUpdate;
    /// set low-level text and objects from existing arrays
    procedure SetFrom(const aText: TRawUTF8DynArray; const aObject: TObjectDynArray);
    /// set all lines, separated by the supplied delimiter
    // - this method is thread-safe
    procedure SetText(const aText: RawUTF8; const Delimiter: RawUTF8 = #13#10);
    /// set all lines from an UTF-8 text file
    // - expect the file is explicitly an UTF-8 file
    // - will ignore any trailing UTF-8 BOM in the file content, but will not
    // expect one either
    // - this method is thread-safe
    procedure LoadFromFile(const FileName: TFileName);
    /// write all lines into the supplied stream
    // - this method is thread-safe
    procedure SaveToStream(Dest: TStream; const Delimiter: RawUTF8 = #13#10);
    /// write all lines into a new file
    // - this method is thread-safe
    procedure SaveToFile(const FileName: TFileName; const Delimiter: RawUTF8 = #13#10);
    /// return the count of stored RawUTF8
    // - reading this property is not thread-safe, since size may change
    property Count: PtrInt read GetCount;
    /// set or retrieve the current memory capacity of the RawUTF8 list
    // - reading this property is not thread-safe, since size may change
    property Capacity: PtrInt read GetCapacity write SetCapacity;
    /// set if IndexOf() shall be case sensitive or not
    // - default is TRUE
    // - matches fCaseSensitive in Flags
    property CaseSensitive: boolean read GetCaseSensitive write SetCaseSensitive;
    /// set if the list doesn't allow duplicated UTF-8 text
    // - if true, an internal hash table is maintained for faster IndexOf()
    // - matches fNoDuplicate in Flags
    property NoDuplicate: boolean read GetNoDuplicate;
    /// access to the low-level flags of this list
    property Flags: TRawUTF8ListFlags read fFlags write fFlags;
    /// get or set a RawUTF8 item
    // - returns '' and raise no exception in case of out of range supplied index
    // - if you want to use it with the VCL, use UTF8ToString() function
    // - reading this property is not thread-safe, since content may change
    property Strings[Index: PtrInt]: RawUTF8 read Get write Put; default;
    /// get or set a Object item
    // - returns nil and raise no exception in case of out of range supplied index
    // - reading this property is not thread-safe, since content may change
    property Objects[Index: PtrInt]: pointer read GetObject write PutObject;
    /// retrieve the corresponding Name when stored as 'Name=Value' pairs
    // - reading this property is not thread-safe, since content may change
    // - consider TSynNameValue if you expect more efficient name/value process
    property Names[Index: PtrInt]: RawUTF8 read GetName;
    /// access to the corresponding 'Name=Value' pairs
    // - search on Name is case-insensitive with 'Name=Value' pairs
    // - reading this property is thread-safe, but won't use the hash table
    // - consider TSynNameValue if you expect more efficient name/value process
    property Values[const Name: RawUTF8]: RawUTF8 read GetValue write SetValue;
    /// the char separator between 'Name=Value' pairs
    // - equals '=' by default
    // - consider TSynNameValue if you expect more efficient name/value process
    property NameValueSep: AnsiChar read fNameValueSep write fNameValueSep;
    /// set or retrieve all items as text lines
    // - lines are separated by #13#10 (CRLF) by default; use GetText and
    // SetText methods if you want to use another line delimiter (even a comma)
    // - this property is thread-safe
    property Text: RawUTF8 read GetTextCRLF write SetTextCRLF;
    /// Event triggered when an entry is modified
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    /// direct access to the memory of the TRawUTF8DynArray items
    // - reading this property is not thread-safe, since content may change
    property TextPtr: PPUtf8CharArray read GetTextPtr;
    /// direct access to the memory of the TObjectDynArray items
    // - reading this property is not thread-safe, since content may change
    property ObjectPtr: PPointerArray read GetObjectPtr;
    /// direct access to the TRawUTF8DynArray items dynamic array wrapper
    // - using this property is not thread-safe, since content may change
    property ValuesArray: TDynArrayHashed read fValues;
  end;

  PRawUTF8List = ^TRawUTF8List;

{$ifndef PUREMORMOT2}

  // some declarations used for backward compatibility only
  TRawUTF8ListLocked = type TRawUTF8List;
  TRawUTF8ListHashed = type TRawUTF8List;
  TRawUTF8ListHashedLocked = type TRawUTF8ListHashed;

  // deprecated TRawUTF8MethodList should be replaced by a TSynDictionary

{$endif PUREMORMOT2}

/// sort a dynamic array of PUTF8Char items, via an external array of indexes
// - you can use FastFindIndexedPUTF8Char() for fast O(log(n)) binary search
procedure QuickSortIndexedPUTF8Char(Values: PPUtf8CharArray; Count: Integer;
  var SortedIndexes: TCardinalDynArray; CaseSensitive: boolean = false);


{ ***************** Cross-Platform Time Zones }

type
  {$A-}

  /// used to store Time Zone bias in TSynTimeZone
  // - map how low-level information is stored in the Windows Registry
  TTimeZoneInfo = record
    Bias: integer;
    bias_std: integer;
    bias_dlt: integer;
    change_time_std: TSynSystemTime;
    change_time_dlt: TSynSystemTime;
  end;
  PTimeZoneInfo = ^TTimeZoneInfo;

  /// text identifier of a Time Zone, following Microsoft Windows naming
  TTimeZoneID = type RawUTF8;

  /// used to store Time Zone information for a single area in TSynTimeZone
  // - Delphi "object" is buggy on stack -> also defined as record with methods
  {$ifdef USERECORDWITHMETHODS}
  TTimeZoneData = record
  {$else}
  TTimeZoneData = object
  {$endif USERECORDWITHMETHODS}
  public
    id: TTimeZoneID;
    display: RawUTF8;
    tzi: TTimeZoneInfo;
    dyn: array of packed record
      year: integer;
      tzi: TTimeZoneInfo;
    end;
    function GetTziFor(year: integer): PTimeZoneInfo;
  end;

  /// used to store the Time Zone information of a TSynTimeZone class
  TTimeZoneDataDynArray = array of TTimeZoneData;

  {$A+}

  /// handle cross-platform time conversions, following Microsoft time zones
  // - is able to retrieve accurate information from the Windows registry,
  // or from a binary compressed file on other platforms (which should have been
  // saved from a Windows system first)
  // - each time zone will be idendified by its TzId string, as defined by
  // Microsoft for its Windows Operating system
  TSynTimeZone = class
  protected
    fZone: TTimeZoneDataDynArray;
    fZones: TDynArrayHashed;
    fLastZone: TTimeZoneID;
    fLastIndex: integer;
    fIds: TStringList;
    fDisplays: TStringList;
  public
    /// will retrieve the default shared TSynTimeZone instance
    // - locally created via the CreateDefault constructor
    // - this is the usual entry point for time zone process, calling e.g.
    // $ aLocalTime := TSynTimeZone.Default.NowToLocal(aTimeZoneID);
    class function Default: TSynTimeZone;
    /// initialize the internal storage
    // - but no data is available, until Load* methods are called
    constructor Create;
    /// retrieve the time zones from Windows registry, or from a local file
    // - under Linux, the file should be located with the executable, renamed
    // with a .tz extension - may have been created via SaveToFile(''), or
    // from a 'TSynTimeZone' bound resource
    // "dummy" parameter exists only to disambiguate constructors for C++
    constructor CreateDefault(dummy: integer = 0);
    /// finalize the instance
    destructor Destroy; override;
    {$ifdef MSWINDOWS}
    /// read time zone information from the Windows registry
    procedure LoadFromRegistry;
    {$endif MSWINDOWS}
    /// read time zone information from a compressed file
    // - if no file name is supplied, a ExecutableName.tz file would be used
    procedure LoadFromFile(const FileName: TFileName = '');
    /// read time zone information from a compressed memory buffer
    procedure LoadFromBuffer(const Buffer: RawByteString);
    /// read time zone information from a 'TSynTimeZone' resource
    // - the resource should contain the SaveToBuffer compressed binary content
    // - is no resource matching the TSynTimeZone class name and ResType=10
    // do exist, nothing would be loaded
    // - the resource could be created as such, from a Windows system:
    // ! TSynTimeZone.Default.SaveToFile('TSynTimeZone.data');
    // then compile the resource as expected, with a brcc32 .rc entry:
    // ! TSynTimeZone 10 "TSynTimeZone.data"
    // - you can specify a library (dll) resource instance handle, if needed
    procedure LoadFromResource(Instance: THandle = 0);
    /// write then time zone information into a compressed file
    // - if no file name is supplied, a ExecutableName.tz file would be created
    procedure SaveToFile(const FileName: TFileName);
    /// write then time zone information into a compressed memory buffer
    function SaveToBuffer: RawByteString;
    /// retrieve the time bias (in minutes) for a given date/time on a TzId
    function GetBiasForDateTime(const Value: TDateTime; const TzId: TTimeZoneID;
      out Bias: integer; out HaveDaylight: boolean): boolean;
    /// retrieve the display text corresponding to a TzId
    // - returns '' if the supplied TzId is not recognized
    function GetDisplay(const TzId: TTimeZoneID): RawUTF8;
    /// compute the UTC date/time corrected for a given TzId
    function UtcToLocal(const UtcDateTime: TDateTime; const TzId: TTimeZoneID): TDateTime;
    /// compute the current date/time corrected for a given TzId
    function NowToLocal(const TzId: TTimeZoneID): TDateTime;
    /// compute the UTC date/time for a given local TzId value
    // - by definition, a local time may correspond to two UTC times, during the
    // time biais period, so the returned value is informative only, and any
    // stored value should be following UTC
    function LocalToUtc(const LocalDateTime: TDateTime; const TzID: TTimeZoneID): TDateTime;
    /// direct access to the low-level time zone information
    property Zone: TTimeZoneDataDynArray read fZone;
    /// direct access to the wrapper over the time zone information array
    property Zones: TDynArrayHashed read fZones;
    /// returns a TStringList of all TzID values
    // - could be used to fill any VCL component to select the time zone
    // - order in Ids[] array follows the Zone[].id information
    function Ids: TStrings;
    /// returns a TStringList of all Display text values
    // - could be used to fill any VCL component to select the time zone
    // - order in Displays[] array follows the Zone[].display information
    function Displays: TStrings;
  end;


implementation

{$ifdef ISDELPHI}
uses
  TypInfo; // avoid Delphi compiler to complain about inlining issues
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


{ TInterfacedCollection }

constructor TInterfacedCollection.Create;
begin
  inherited Create(GetClass);
end;


{ TSynInterfacedObject }

function TSynInterfacedObject._AddRef: {$ifdef FPC}longint{$else}integer{$endif};
begin
  result := VirtualAddRef;
end;

function TSynInterfacedObject._Release: {$ifdef FPC}longint{$else}integer{$endif};
begin
  result := VirtualRelease;
end;

{$ifdef FPC}
function TSynInterfacedObject.QueryInterface(
  {$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif} IID: TGUID;
  out Obj): longint; {$ifndef WINDOWS}cdecl{$else}stdcall{$endif};
{$else}
function TSynInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
{$endif FPC}
begin
  result := VirtualQueryInterface(IID, Obj);
end;

function TSynInterfacedObject.VirtualQueryInterface(const IID: TGUID; out Obj): HResult;
begin
  result := E_NOINTERFACE;
end;


{ TAutoFree }

constructor TAutoFree.Create(var localVariable; obj: TObject);
begin
  fObject := obj;
  TObject(localVariable) := obj;
end;

class function TAutoFree.One(var localVariable; obj: TObject): IAutoFree;
begin
  result := Create(localVariable,obj);
end;

class function TAutoFree.Several(const varObjPairs: array of pointer): IAutoFree;
begin
  result := Create(varObjPairs);
end;

constructor TAutoFree.Create(const varObjPairs: array of pointer);
var
  n, i: integer;
begin
  n := length(varObjPairs);
  if (n = 0) or (n and 1 = 1) then
    exit;
  n := n shr 1;
  if n = 0 then
    exit;
  SetLength(fObjectList, n);
  for i := 0 to n - 1 do begin
    fObjectList[i] := varObjPairs[i * 2 + 1];
    PPointer(varObjPairs[i * 2])^ := fObjectList[i];
  end;
end;

procedure TAutoFree.Another(var localVariable; obj: TObject);
var
  n: integer;
begin
  n := length(fObjectList);
  SetLength(fObjectList, n + 1);
  fObjectList[n] := obj;
  TObject(localVariable) := obj;
end;

destructor TAutoFree.Destroy;
var
  i: integer;
begin
  if fObjectList <> nil then
    for i := high(fObjectList) downto 0 do // release FILO
      fObjectList[i].Free;
  fObject.Free;
  inherited;
end;


{ TAutoLocker }

constructor TAutoLocker.Create;
begin
  fSafe.Init;
end;

destructor TAutoLocker.Destroy;
begin
  fSafe.Done;
  inherited;
end;

function TAutoLocker.ProtectMethod: IUnknown;
begin
  result := TAutoLock.Create(@fSafe);
end;

procedure TAutoLocker.Enter;
begin
  fSafe.Lock;
end;

procedure TAutoLocker.Leave;
begin
  fSafe.UnLock;
end;

function TAutoLocker.Safe: PSynLocker;
begin
  result := @fSafe;
end;


{ ************ TSynPersistent* / TSyn*List / TSynLocker classes }

{ TSynPersistent }

constructor TSynPersistent.Create;
begin
  // quick check if this class type is already registered
  if PPointer(PPAnsiChar(self)^ + vmtAutoTable)^ = nil then
    // use RegisterClasses() since we don't need to inline RegisterClass()
    Rtti.RegisterClasses([PClass(self)^]);
end;

class function TSynPersistent.RttiCustom: TRttiCustom;
begin // faster than ClassPropertiesGet: we know it is the first slot
  result := PPPointer(PAnsiChar(self) + vmtAutoTable)^^;
  // assert(result.InheritsFrom(TRttiCustom));
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

class procedure TSynPersistent.RttiCustomSet(Rtti: TRttiCustom);
begin
  // do nothing by default
end;

function TSynPersistent.RttiBeforeWriteObject(W: TBaseWriter;
  var Options: TTextWriterWriteObjectOptions): boolean;
begin
  result := false; // default JSON serialization
end;

function TSynPersistent.RttiWritePropertyValue(W: TBaseWriter; Prop: PRttiCustomProp;
  Options: TTextWriterWriteObjectOptions): boolean;
begin
  result := false; // default JSON serializaiton
end;

procedure TSynPersistent.RttiAfterWriteObject(W: TBaseWriter;
  Options: TTextWriterWriteObjectOptions);
begin // nothing to do
end;

function TSynPersistent.RttiBeforeReadObject(Ctxt: pointer): boolean;
begin
  result := false; // default JSON unserialization
end;

procedure TSynPersistent.RttiAfterReadObject;
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

constructor TSynList.Create;
begin
  // nothing to do
end;

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

constructor TSynObjectList.Create(aOwnObjects: boolean; aItemClass: TClass);
begin
  fOwnObjects := aOwnObjects;
  fItemClass := aItemClass;
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

procedure TSynPersistentLock.Lock;
begin
  if self <> nil then
    fSafe^.Lock;
end;

procedure TSynPersistentLock.Unlock;
begin
  if self <> nil then
    fSafe^.UnLock;
end;

function TSynPersistentLock.RttiBeforeWriteObject(W: TBaseWriter;
  var Options: TTextWriterWriteObjectOptions): boolean;
begin
  if woPersistentLock in Options then
    fSafe.Lock;
  result := false; // continue with default JSON serialization
end;

procedure TSynPersistentLock.RttiAfterWriteObject(W: TBaseWriter;
  Options: TTextWriterWriteObjectOptions);
begin
  if woPersistentLock in Options then
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

constructor TSynObjectListLocked.Create(AOwnsObjects: boolean);
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

class procedure TSynPersistentWithID.RttiCustomSet(Rtti: TRttiCustom);
begin
  // will be recognized as a TID property with all associated options
  Rtti.Props.Add(TypeInfo(TID), PtrInt(@TSynPersistentWithID(nil).fID), 'ID');
end;



{ ************ TSynPersistentStore with proper Binary Serialization }

{ TSynPersistentStore }

constructor TSynPersistentStore.Create(const aName: RawUTF8);
begin
  Create;
  fName := aName;
end;

constructor TSynPersistentStore.CreateFrom(const aBuffer: RawByteString;
  aLoad: TAlgoCompressLoad);
begin
  CreateFromBuffer(pointer(aBuffer), length(aBuffer), aLoad);
end;

constructor TSynPersistentStore.CreateFromBuffer(
  aBuffer: pointer; aBufferLen: integer; aLoad: TAlgoCompressLoad);
begin
  Create('');
  LoadFrom(aBuffer, aBufferLen, aLoad);
end;

constructor TSynPersistentStore.CreateFromFile(const aFileName: TFileName;
  aLoad: TAlgoCompressLoad);
begin
  Create('');
  LoadFromFile(aFileName, aLoad);
end;

procedure TSynPersistentStore.LoadFromReader;
begin
  fReader.VarUTF8(fName);
end;

procedure TSynPersistentStore.SaveToWriter(aWriter: TBufferWriter);
begin
  aWriter.Write(fName);
end;

procedure TSynPersistentStore.LoadFrom(const aBuffer: RawByteString;
  aLoad: TAlgoCompressLoad);
begin
  if aBuffer <> '' then
    LoadFrom(pointer(aBuffer), length(aBuffer), aLoad);
end;

procedure TSynPersistentStore.LoadFrom(aBuffer: pointer; aBufferLen: integer;
  aLoad: TAlgoCompressLoad);
var
  localtemp: RawByteString;
  p: pointer;
  temp: PRawByteString;
begin
  if (aBuffer = nil) or (aBufferLen <= 0) then
    exit; // nothing to load
  fLoadFromLastAlgo := TAlgoCompress.Algo(aBuffer, aBufferLen);
  if fLoadFromLastAlgo = nil then
    fReader.ErrorData('%.LoadFrom unknown TAlgoCompress AlgoID=%',
      [self, PByteArray(aBuffer)[4]]);
  temp := fReaderTemp;
  if temp = nil then
    temp := @localtemp;
  p := fLoadFromLastAlgo.Decompress(aBuffer, aBufferLen,
    fLoadFromLastUncompressed, temp^, aLoad);
  if p = nil then
    fReader.ErrorData('%.LoadFrom %.Decompress failed',
      [self, fLoadFromLastAlgo]);
  fReader.Init(p, fLoadFromLastUncompressed);
  LoadFromReader;
end;

function TSynPersistentStore.LoadFromFile(const aFileName: TFileName;
  aLoad: TAlgoCompressLoad): boolean;
var
  temp: RawByteString;
begin
  temp := StringFromFile(aFileName);
  result := temp <> '';
  if result then
    LoadFrom(temp, aLoad);
end;

procedure TSynPersistentStore.SaveTo(out aBuffer: RawByteString;
  nocompression: boolean; BufLen: integer; ForcedAlgo: TAlgoCompress;
  BufferOffset: integer);
var
  writer: TBufferWriter;
  temp: array[word] of byte;
begin
  if BufLen <= SizeOf(temp) then
    writer := TBufferWriter.Create(TRawByteStringStream, @temp, SizeOf(temp))
  else
    writer := TBufferWriter.Create(TRawByteStringStream, BufLen);
  try
    SaveToWriter(writer);
    fSaveToLastUncompressed := writer.TotalWritten;
    aBuffer := writer.FlushAndCompress(nocompression, ForcedAlgo, BufferOffset);
  finally
    writer.Free;
  end;
end;

function TSynPersistentStore.SaveTo(nocompression: boolean; BufLen: integer;
  ForcedAlgo: TAlgoCompress; BufferOffset: integer): RawByteString;
begin
  SaveTo(result, nocompression, BufLen, ForcedAlgo, BufferOffset);
end;

function TSynPersistentStore.SaveToFile(const aFileName: TFileName;
  nocompression: boolean; BufLen: integer; ForcedAlgo: TAlgoCompress): PtrUInt;
var
  temp: RawByteString;
begin
  SaveTo(temp, nocompression, BufLen, ForcedAlgo);
  if FileFromString(temp, aFileName) then
    result := length(temp)
  else
    result := 0;
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
  {$endif CPUX86NOTPIC}
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

function IsHTMLContentTypeTextual(Headers: PUTF8Char): boolean;
begin
  result := ExistsIniNameValue(Headers, HEADER_CONTENT_TYPE_UPPER,
    [JSON_CONTENT_TYPE_UPPER, 'TEXT/', 'APPLICATION/XML',
     'APPLICATION/JAVASCRIPT', 'APPLICATION/X-JAVASCRIPT', 'IMAGE/SVG+XML']);
end;


{ ************ RawUTF8 String Values Interning and TRawUTF8List }


{ TRawUTF8InterningSlot }

procedure TRawUTF8InterningSlot.Init;
begin
  Safe.Init;
  Safe.LockedInt64[0] := 0;
  Values.InitSpecific(TypeInfo(TRawUTF8DynArray), Value, ptRawUTF8,
    @Safe.Padding[0].VInteger, false, InterningHasher);
end;

procedure TRawUTF8InterningSlot.Done;
begin
  Safe.Done;
end;

function TRawUTF8InterningSlot.Count: integer;
begin
  result := Safe.LockedInt64[0];
end;

procedure TRawUTF8InterningSlot.Unique(var aResult: RawUTF8;
  const aText: RawUTF8; aTextHash: cardinal);
var
  i: PtrInt;
  added: boolean;
begin
  Safe.Lock;
  try
    i := Values.FindHashedForAdding(aText, added, aTextHash);
    if added then
    begin
      Value[i] := aText;   // copy new value to the pool
      aResult := aText;
    end
    else
      aResult := Value[i]; // return unified string instance
  finally
    Safe.UnLock;
  end;
end;

procedure TRawUTF8InterningSlot.UniqueText(var aText: RawUTF8; aTextHash: cardinal);
var
  i: PtrInt;
  added: boolean;
begin
  Safe.Lock;
  try
    i := Values.FindHashedForAdding(aText, added, aTextHash);
    if added then
      Value[i] := aText
    else  // copy new value to the pool
      aText := Value[i];      // return unified string instance
  finally
    Safe.UnLock;
  end;
end;

procedure TRawUTF8InterningSlot.Clear;
begin
  Safe.Lock;
  try
    Values.SetCount(0); // Values.Clear
    Values.Hasher.Clear;
  finally
    Safe.UnLock;
  end;
end;

function TRawUTF8InterningSlot.Clean(aMaxRefCount: TRefCnt): integer;
var
  i: integer;
  s, d: PPtrUInt; // points to RawUTF8 values
begin
  result := 0;
  Safe.Lock;
  try
    if Safe.Padding[0].VInteger = 0 then // len = 0 ?
      exit;
    s := pointer(Value);
    d := s;
    for i := 1 to Safe.Padding[0].VInteger do
    begin
      if PRefCnt(PAnsiChar(s^) - _STRREFCNT)^ <= aMaxRefCount then
      begin
        {$ifdef FPC}
        Finalize(PRawUTF8(s)^);
        {$else}
        PRawUTF8(s)^ := '';
        {$endif FPC}
        inc(result);
      end
      else
      begin
        if s <> d then
        begin
          d^ := s^; // bypass COW assignments
          s^ := 0;  // avoid GPF
        end;
        inc(d);
      end;
      inc(s);
    end;
    if result > 0 then
    begin
      Values.SetCount((PtrUInt(d) - PtrUInt(Value)) div SizeOf(d^));
      Values.ReHash;
    end;
  finally
    Safe.UnLock;
  end;
end;


{ TRawUTF8Interning }

constructor TRawUTF8Interning.Create(aHashTables: integer);
var
  p: integer;
  i: PtrInt;
begin
  for p := 0 to 9 do
    if aHashTables = 1 shl p then
    begin
      SetLength(fPool, aHashTables);
      fPoolLast := aHashTables - 1;
      for i := 0 to fPoolLast do
        fPool[i].Init;
      exit;
    end;
  raise ESynException.CreateUTF8('%.Create(%) not allowed: should be a power of 2',
    [self, aHashTables]);
end;

destructor TRawUTF8Interning.Destroy;
var
  i: PtrInt;
begin
  for i := 0 to fPoolLast do
    fPool[i].Done;
  inherited Destroy;
end;

procedure TRawUTF8Interning.Clear;
var
  i: PtrInt;
begin
  if self <> nil then
    for i := 0 to fPoolLast do
      fPool[i].Clear;
end;

function TRawUTF8Interning.Clean(aMaxRefCount: TRefCnt): integer;
var
  i: PtrInt;
begin
  result := 0;
  if self <> nil then
    for i := 0 to fPoolLast do
      inc(result, fPool[i].Clean(aMaxRefCount));
end;

function TRawUTF8Interning.Count: integer;
var
  i: PtrInt;
begin
  result := 0;
  if self <> nil then
    for i := 0 to fPoolLast do
      inc(result, fPool[i].Count);
end;

procedure TRawUTF8Interning.Unique(var aResult: RawUTF8; const aText: RawUTF8);
var
  hash: cardinal;
begin
  if aText = '' then
    aResult := ''
  else if self = nil then
    aResult := aText
  else
  begin // inlined fPool[].Values.HashElement
    hash := InterningHasher(0, pointer(aText), length(aText));
    fPool[hash and fPoolLast].Unique(aResult, aText, hash);
  end;
end;

procedure TRawUTF8Interning.UniqueText(var aText: RawUTF8);
var
  hash: cardinal;
begin
  if (self <> nil) and (aText <> '') then
  begin // inlined fPool[].Values.HashElement
    hash := InterningHasher(0, pointer(aText), length(aText));
    fPool[hash and fPoolLast].UniqueText(aText, hash);
  end;
end;

function TRawUTF8Interning.Unique(const aText: RawUTF8): RawUTF8;
var
  hash: cardinal;
begin
  if aText = '' then
    result := ''
  else if self = nil then
    result := aText
  else
  begin // inlined fPool[].Values.HashElement
    hash := InterningHasher(0, pointer(aText), length(aText));
    fPool[hash and fPoolLast].Unique(result, aText, hash);
  end;
end;

function TRawUTF8Interning.Unique(aText: PUTF8Char; aTextLen: PtrInt): RawUTF8;
begin
  FastSetString(result, aText, aTextLen);
  UniqueText(result);
end;

procedure TRawUTF8Interning.Unique(var aResult: RawUTF8;
  aText: PUTF8Char; aTextLen: PtrInt);
begin
  FastSetString(aResult, aText, aTextLen);
  UniqueText(aResult);
end;

procedure TRawUTF8Interning.UniqueVariant(var aResult: variant; const aText: RawUTF8);
begin
  ClearVariantForString(aResult);
  Unique(RawUTF8(TVarData(aResult).VAny), aText);
end;

procedure TRawUTF8Interning.UniqueVariantString(var aResult: variant;
  const aText: string);
var
  tmp: RawUTF8;
begin
  StringToUTF8(aText, tmp);
  UniqueVariant(aResult, tmp);
end;

procedure TRawUTF8Interning.UniqueVariant(var aResult: variant);
var
  vd: TVarData absolute aResult;
  vt: cardinal;
begin
  vt := vd.VType;
  if vt = varString then
    UniqueText(RawUTF8(vd.VString))
  else if vt = varVariant or varByRef then
    UniqueVariant(PVariant(vd.VPointer)^)
  else if vt = varString or varByRef then
    UniqueText(PRawUTF8(vd.VPointer)^);
end;


{ TRawUTF8List }

constructor TRawUTF8List.Create;
begin
  Create([fCaseSensitive]);
end;

{$ifndef PUREMORMOT2}
constructor TRawUTF8List.Create(aOwnObjects, aNoDuplicate, aCaseSensitive: boolean);
begin
  if aOwnObjects then
    include(fFlags, fObjectsOwned);
  if aNoDuplicate then
    include(fFlags, fNoDuplicate);
  if aCaseSensitive then
    include(fFlags, fCaseSensitive);
  Create(fFlags);
end;
{$endif PUREMORMOT2}

constructor TRawUTF8List.Create(aFlags: TRawUTF8ListFlags);
begin
  inherited Create;
  fNameValueSep := '=';
  fFlags := aFlags;
  fValues.InitSpecific(TypeInfo(TRawUTF8DynArray), fValue, ptRawUTF8, @fCount,
    not (fCaseSensitive in aFlags));
end;

destructor TRawUTF8List.Destroy;
begin
  SetCapacity(0);
  inherited Destroy;
end;

procedure TRawUTF8List.SetCaseSensitive(Value: boolean);
begin
  if (self = nil) or (fCaseSensitive in fFlags = Value) then
    exit;
  fSafe.Lock;
  try
    if Value then
      include(fFlags, fCaseSensitive)
    else
      exclude(fFlags, fCaseSensitive);
    fValues.Hasher.InitSpecific(@fValues, ptRawUTF8, not Value, nil);
    Changed;
  finally
    fSafe.UnLock;
  end;
end;

procedure TRawUTF8List.SetCapacity(const capa: PtrInt);
begin
  if self <> nil then
  begin
    fSafe.Lock;
    try
      if capa <= 0 then
      begin // clear
        if fObjects <> nil then
        begin
          if fObjectsOwned in fFlags then
            RawObjectsClear(pointer(fObjects), fCount);
          fObjects := nil;
        end;
        fValues.Clear;
        if fNoDuplicate in fFlags then
          fValues.Hasher.Clear;
        Changed;
      end
      else
      begin // resize
        if capa < fCount then
        begin // resize down
          if fObjects <> nil then
          begin
            if fObjectsOwned in fFlags then
              RawObjectsClear(@fObjects[capa], fCount - capa - 1);
            SetLength(fObjects, capa);
          end;
          fValues.Count := capa;
          if fNoDuplicate in fFlags then
            fValues.ReHash;
          Changed;
        end;
        if capa > length(fValue) then
        begin // resize up
          SetLength(fValue, capa);
          if fObjects <> nil then
            SetLength(fObjects, capa);
        end;
      end;
    finally
      fSafe.UnLock;
    end;
  end;
end;

function TRawUTF8List.Add(const aText: RawUTF8; aRaiseExceptionIfExisting: boolean): PtrInt;
begin
  result := AddObject(aText, nil, aRaiseExceptionIfExisting);
end;

function TRawUTF8List.AddObject(const aText: RawUTF8; aObject: TObject;
  aRaiseExceptionIfExisting: boolean; aFreeAndReturnExistingObject: PPointer): PtrInt;
var
  added: boolean;
  obj: TObject;
begin
  result := -1;
  if self = nil then
    exit;
  fSafe.Lock;
  try
    if fNoDuplicate in fFlags then
    begin
      result := fValues.FindHashedForAdding(aText, added, {noadd=}true);
      if not added then
      begin
        obj := GetObject(result);
        if (obj = aObject) and (obj <> nil) then
          exit; // found identical aText/aObject -> behave as if added
        if aFreeAndReturnExistingObject <> nil then
        begin
          aObject.Free;
          aFreeAndReturnExistingObject^ := obj;
        end;
        if aRaiseExceptionIfExisting then
          raise ESynException.CreateUTF8('%.Add duplicate [%]', [self, aText]);
        result := -1;
        exit;
      end;
    end;
    result := fValues.Add(aText);
    if (fObjects <> nil) or (aObject <> nil) then
    begin
      if result >= length(fObjects) then
        SetLength(fObjects, length(fValue)); // same capacity
      if aObject <> nil then
        fObjects[result] := aObject;
    end;
    if Assigned(fOnChange) then
      Changed;
  finally
    fSafe.UnLock;
  end;
end;

procedure TRawUTF8List.AddObjectUnique(const aText: RawUTF8;
  aObjectToAddOrFree: PPointer);
begin
  if fNoDuplicate in fFlags then
    AddObject(aText, aObjectToAddOrFree^, {raiseexc=}false,
      {freeandreturnexisting=}aObjectToAddOrFree);
end;

procedure TRawUTF8List.AddRawUTF8List(List: TRawUTF8List);
var
  i: PtrInt;
begin
  if List <> nil then
  begin
    BeginUpdate; // includes Safe.Lock
    try
      for i := 0 to List.fCount - 1 do
        AddObject(List.fValue[i], List.GetObject(i));
    finally
      EndUpdate;
    end;
  end;
end;

procedure TRawUTF8List.BeginUpdate;
begin
  if InterLockedIncrement(fOnChangeLevel) > 1 then
    exit;
  fSafe.Lock;
  fOnChangeBackupForBeginUpdate := fOnChange;
  fOnChange := OnChangeHidden;
  exclude(fFlags, fOnChangeTrigerred);
end;

procedure TRawUTF8List.EndUpdate;
begin
  if (fOnChangeLevel <= 0) or (InterLockedDecrement(fOnChangeLevel) > 0) then
    exit; // allows nested BeginUpdate..EndUpdate calls
  fOnChange := fOnChangeBackupForBeginUpdate;
  if (fOnChangeTrigerred in fFlags) and Assigned(fOnChange) then
    Changed;
  exclude(fFlags, fOnChangeTrigerred);
  fSafe.UnLock;
end;

procedure TRawUTF8List.Changed;
begin
  if Assigned(fOnChange) then
  try
    fOnChange(self);
  except // ignore any exception in user code (may not trigger fSafe.UnLock)
  end;
end;

procedure TRawUTF8List.Clear;
begin
  SetCapacity(0); // will also call Changed
end;

procedure TRawUTF8List.InternalDelete(Index: PtrInt);
begin // caller ensured Index is correct
  fValues.Delete(Index); // includes dec(fCount)
  if PtrUInt(Index) < PtrUInt(length(fObjects)) then
  begin
    if fObjectsOwned in fFlags then
      fObjects[Index].Free;
    if fCount > Index then
      MoveFast(fObjects[Index + 1], fObjects[Index],
        (fCount - Index) * SizeOf(pointer));
    fObjects[fCount] := nil;
  end;
  if Assigned(fOnChange) then
    Changed;
end;

procedure TRawUTF8List.Delete(Index: PtrInt);
begin
  if (self <> nil) and (PtrUInt(Index) < PtrUInt(fCount)) then
    if fNoDuplicate in fFlags then // force update the hash table
      Delete(fValue[Index])
    else
      InternalDelete(Index);
end;

function TRawUTF8List.Delete(const aText: RawUTF8): PtrInt;
begin
  fSafe.Lock;
  try
    if fNoDuplicate in fFlags then
      result := fValues.FindHashedAndDelete(aText, nil, {nodelete=}true)
    else
      result := FindRawUTF8(pointer(fValue), aText, fCount, fCaseSensitive in fFlags);
    if result >= 0 then
      InternalDelete(result);
  finally
    fSafe.UnLock;
  end;
end;

function TRawUTF8List.DeleteFromName(const Name: RawUTF8): PtrInt;
begin
  fSafe.Lock;
  try
    result := IndexOfName(Name);
    Delete(result);
  finally
    fSafe.UnLock;
  end;
end;

function TRawUTF8List.IndexOf(const aText: RawUTF8): PtrInt;
begin
  if self <> nil then
  begin
    fSafe.Lock;
    try
      if fNoDuplicate in fFlags then
        result := fValues.FindHashed(aText)
      else
        result := FindRawUTF8(pointer(fValue), aText, fCount, fCaseSensitive in fFlags);
    finally
      fSafe.UnLock;
    end;
  end
  else
    result := -1;
end;

function TRawUTF8List.Get(Index: PtrInt): RawUTF8;
begin
  if (self = nil) or (PtrUInt(Index) >= PtrUInt(fCount)) then
    result := ''
  else
    result := fValue[Index];
end;

function TRawUTF8List.GetCapacity: PtrInt;
begin
  if self = nil then
    result := 0
  else
    result := length(fValue);
end;

function TRawUTF8List.GetCount: PtrInt;
begin
  if self = nil then
    result := 0
  else
    result := fCount;
end;

function TRawUTF8List.GetTextPtr: PPUtf8CharArray;
begin
  if self = nil then
    result := nil
  else
    result := pointer(fValue);
end;

function TRawUTF8List.GetObjectPtr: PPointerArray;
begin
  if self = nil then
    result := nil
  else
    result := pointer(fObjects);
end;

function TRawUTF8List.GetName(Index: PtrInt): RawUTF8;
begin
  result := Get(Index);
  if result = '' then
    exit;
  Index := PosExChar(NameValueSep, result);
  if Index = 0 then
    result := ''
  else
    SetLength(result, Index - 1);
end;

function TRawUTF8List.GetObject(Index: PtrInt): pointer;
begin
  if (self <> nil) and (fObjects <> nil) and (PtrUInt(Index) < PtrUInt(fCount)) then
    result := fObjects[Index]
  else
    result := nil;
end;

function TRawUTF8List.GetObjectFrom(const aText: RawUTF8): pointer;
var
  ndx: PtrUInt;
begin
  result := nil;
  if (self <> nil) and (fObjects <> nil) then
  begin
    fSafe.Lock;
    try
      ndx := IndexOf(aText);
      if ndx < PtrUInt(fCount) then
        result := fObjects[ndx];
    finally
      fSafe.UnLock;
    end;
  end;
end;

function TRawUTF8List.GetText(const Delimiter: RawUTF8): RawUTF8;
var
  DelimLen, i, Len: PtrInt;
  P: PUTF8Char;
begin
  result := '';
  if (self = nil) or (fCount = 0) then
    exit;
  fSafe.Lock;
  try
    DelimLen := length(Delimiter);
    Len := DelimLen * (fCount - 1);
    for i := 0 to fCount - 1 do
      inc(Len, length(fValue[i]));
    FastSetString(result, nil, Len);
    P := pointer(result);
    i := 0;
    repeat
      Len := length(fValue[i]);
      if Len > 0 then
      begin
        MoveFast(pointer(fValue[i])^, P^, Len);
        inc(P, Len);
      end;
      inc(i);
      if i >= fCount then
        Break;
      if DelimLen > 0 then
      begin
        MoveSmall(pointer(Delimiter), P, DelimLen);
        inc(P, DelimLen);
      end;
    until false;
  finally
    fSafe.UnLock;
  end;
end;

procedure TRawUTF8List.SaveToStream(Dest: TStream; const Delimiter: RawUTF8);
var
  W: TBaseWriter;
  i: PtrInt;
  temp: TTextWriterStackBuffer;
begin
  if (self = nil) or (fCount = 0) then
    exit;
  fSafe.Lock;
  try
    W := TBaseWriter.Create(Dest, @temp, SizeOf(temp));
    try
      i := 0;
      repeat
        W.AddString(fValue[i]);
        inc(i);
        if i >= fCount then
          Break;
        W.AddString(Delimiter);
      until false;
      W.FlushFinal;
    finally
      W.Free;
    end;
  finally
    fSafe.UnLock;
  end;
end;

procedure TRawUTF8List.SaveToFile(const FileName: TFileName; const Delimiter: RawUTF8);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(FS, Delimiter);
  finally
    FS.Free;
  end;
end;

function TRawUTF8List.GetTextCRLF: RawUTF8;
begin
  result := GetText;
end;

function TRawUTF8List.GetValue(const Name: RawUTF8): RawUTF8;
begin
  fSafe.Lock;
  try
    result := GetValueAt(IndexOfName(Name));
  finally
    fSafe.UnLock;
  end;
end;

function TRawUTF8List.GetValueAt(Index: PtrInt): RawUTF8;
begin
  result := Get(Index);
  if result = '' then
    exit;
  Index := PosExChar(NameValueSep, result);
  if Index = 0 then
    result := ''
  else
    result := copy(result, Index + 1, maxInt);
end;

function TRawUTF8List.IndexOfName(const Name: RawUTF8): PtrInt;
var
  UpperName: array[byte] of AnsiChar;
  table: PNormTable;
begin
  if self <> nil then
  begin
    PWord(UpperCopy255(UpperName{%H-}, Name))^ := ord(NameValueSep);
    table := @NormToUpperAnsi7;
    for result := 0 to fCount - 1 do
      if IdemPChar(Pointer(fValue[result]), UpperName, table) then
        exit;
  end;
  result := -1;
end;

function TRawUTF8List.IndexOfObject(aObject: TObject): PtrInt;
begin
  if (self <> nil) and (fObjects <> nil) then
  begin
    fSafe.Lock;
    try
      result := PtrUIntScanIndex(pointer(fObjects), fCount, PtrUInt(aObject));
    finally
      fSafe.UnLock;
    end
  end
  else
    result := -1;
end;

function TRawUTF8List.Contains(const aText: RawUTF8; aFirstIndex: integer): PtrInt;
var
  i: PtrInt; // use a temp variable to make oldest Delphi happy :(
begin
  result := -1;
  if self <> nil then
  begin
    fSafe.Lock;
    try
      for i := aFirstIndex to fCount - 1 do
        if PosEx(aText, fValue[i]) > 0 then
        begin
          result := i;
          exit;
        end;
    finally
      fSafe.UnLock;
    end;
  end;
end;

procedure TRawUTF8List.OnChangeHidden(Sender: TObject);
begin
  if self <> nil then
    include(fFlags, fOnChangeTrigerred);
end;

procedure TRawUTF8List.Put(Index: PtrInt; const Value: RawUTF8);
begin
  if (self <> nil) and (PtrUInt(Index) < PtrUInt(fCount)) then
  begin
    fValue[Index] := Value;
    if Assigned(fOnChange) then
      Changed;
  end;
end;

procedure TRawUTF8List.PutObject(Index: PtrInt; Value: pointer);
begin
  if (self <> nil) and (PtrUInt(Index) < PtrUInt(fCount)) then
  begin
    if fObjects = nil then
      SetLength(fObjects, Length(fValue));
    fObjects[Index] := Value;
    if Assigned(fOnChange) then
      Changed;
  end;
end;

procedure TRawUTF8List.SetText(const aText: RawUTF8; const Delimiter: RawUTF8);
begin
  SetTextPtr(pointer(aText), PUTF8Char(pointer(aText)) + length(aText), Delimiter);
end;

procedure TRawUTF8List.LoadFromFile(const FileName: TFileName);
var
  Map: TMemoryMap;
  P: PUTF8Char;
  tmp: RawUTF8;
begin
  if Map.Map(FileName) then
  try
    P := pointer(Map.Buffer);
    if Map.Size <> 0 then
      case Map.TextFileKind of
      isUTF8:
          // ignore UTF-8 BOM
          SetTextPtr(P + 3, P + Map.Size, #13#10);
      isUnicode:
        begin
          // conversion from UTF-16 content (mainly on Windows) into UTF-8
          RawUnicodeToUtf8(PWideChar(P + 2), (Map.Size - 2) shr 1, tmp);
          SetText(tmp, #13#10);
        end;
      else
        // assume text file with no BOM is already UTF-8 encoded
        SetTextPtr(P, P + Map.Size, #13#10);
      end;
  finally
    Map.UnMap;
  end;
end;

procedure TRawUTF8List.SetTextPtr(P, PEnd: PUTF8Char; const Delimiter: RawUTF8);
var
  DelimLen: PtrInt;
  DelimFirst: AnsiChar;
  PBeg, DelimNext: PUTF8Char;
  Line: RawUTF8;
begin
  DelimLen := length(Delimiter);
  BeginUpdate; // also makes fSafe.Lock
  try
    Clear;
    if (P <> nil) and (DelimLen > 0) and (P < PEnd) then
    begin
      DelimFirst := Delimiter[1];
      DelimNext := PUTF8Char(pointer(Delimiter)) + 1;
      repeat
        PBeg := P;
        while P < PEnd do
        begin
          if (P^ = DelimFirst) and
             CompareMemSmall(P + 1, DelimNext, DelimLen - 1) then
            break;
          inc(P);
        end;
        FastSetString(Line, PBeg, P - PBeg);
        AddObject(Line, nil);
        if P >= PEnd then
          break;
        inc(P, DelimLen);
      until P >= PEnd;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TRawUTF8List.SetTextCRLF(const Value: RawUTF8);
begin
  SetText(Value, #13#10);
end;

procedure TRawUTF8List.SetFrom(const aText: TRawUTF8DynArray;
  const aObject: TObjectDynArray);
var
  n: integer;
begin
  BeginUpdate; // also makes fSafe.Lock
  try
    Clear;
    n := length(aText);
    if n = 0 then
      exit;
    SetCapacity(n);
    fCount := n;
    fValue := aText;
    fObjects := aObject;
    if fNoDuplicate in fFlags then
      fValues.ReHash;
  finally
    EndUpdate;
  end;
end;

procedure TRawUTF8List.SetValue(const Name, Value: RawUTF8);
var
  i: PtrInt;
  txt: RawUTF8;
begin
  txt := Name + RawUTF8(NameValueSep) + Value;
  fSafe.Lock;
  try
    i := IndexOfName(Name);
    if i < 0 then
      AddObject(txt, nil)
    else if fValue[i] <> txt then
    begin
      fValue[i] := txt;
      if fNoDuplicate in fFlags then
        fValues.Hasher.Clear; // invalidate internal hash table
      Changed;
    end;
  finally
    fSafe.UnLock;
  end;
end;

function TRawUTF8List.GetCaseSensitive: boolean;
begin
  result := (self <> nil) and (fCaseSensitive in fFlags);
end;

function TRawUTF8List.GetNoDuplicate: boolean;
begin
  result := (self <> nil) and (fNoDuplicate in fFlags);
end;

function TRawUTF8List.UpdateValue(const Name: RawUTF8; var Value: RawUTF8;
  ThenDelete: boolean): boolean;
var
  i: PtrInt;
begin
  result := false;
  fSafe.Lock;
  try
    i := IndexOfName(Name);
    if i >= 0 then
    begin
      Value := GetValueAt(i); // copy value
      if ThenDelete then
        Delete(i); // optionally delete
      result := true;
    end;
  finally
    fSafe.UnLock;
  end;
end;

function TRawUTF8List.PopFirst(out aText: RawUTF8; aObject: PObject): boolean;
begin
  result := false;
  if fCount = 0 then
    exit;
  fSafe.Lock;
  try
    if fCount > 0 then
    begin
      aText := fValue[0];
      if aObject <> nil then
        if fObjects <> nil then
          aObject^ := fObjects[0]
        else
          aObject^ := nil;
      Delete(0);
      result := true;
    end;
  finally
    fSafe.UnLock;
  end;
end;

function TRawUTF8List.PopLast(out aText: RawUTF8; aObject: PObject): boolean;
var
  last: PtrInt;
begin
  result := false;
  if fCount = 0 then
    exit;
  fSafe.Lock;
  try
    last := fCount - 1;
    if last >= 0 then
    begin
      aText := fValue[last];
      if aObject <> nil then
        if fObjects <> nil then
          aObject^ := fObjects[last]
        else
          aObject^ := nil;
      Delete(last);
      result := true;
    end;
  finally
    fSafe.UnLock;
  end;
end;


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
      n := PDALen(Data - _DALEN)^ + _DAOFF;
    Dest.WriteVarUInt32(n);
    Dest.Write4(0); // warning: we don't store any Hash32 checksum any more
    if Info = nil then
      Dest.Write(Data, itemsize * n)
    else
    begin
      sav := RTTI_BINARYSAVE[Info^.Kind];
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

function DynArrayLoad(var Value; Source: PAnsiChar; TypeInfo: pointer;
  TryCustomVariants: PDocVariantOptions): PAnsiChar;
begin
  result := BinaryLoad(@Value, source, TypeInfo, nil, nil, [rkDynArray], TryCustomVariants);
end;

function DynArraySave(var Value; TypeInfo: pointer): RawByteString;
begin
  result := BinarySave(@Value, TypeInfo, [rkDynArray]);
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
    Source.ErrorData('RTTI_BINARYLOAD[rkDynArray] failed for %', [Info.Name^]);
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
      load := RTTI_BINARYLOAD[iteminfo^.Kind];
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
  Info: PRttiInfo; CaseInSensitive: boolean): integer;
var
  n1, n2, itemsize: PtrInt;
  comp: TRttiCompare;
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
    n1 := PDALen(A - _DALEN)^ + _DAOFF;
  if ExternalCountB <> nil then
    n2 := ExternalCountB^
  else
    n2 := PDALen(B - _DALEN)^ + _DAOFF;
  comp := RTTI_COMPARE[CaseInSensitive, Info^.Kind];
  if Assigned(comp) then
    repeat
      itemsize := comp(A, B, Info, result);
      inc(A, itemsize);
      inc(B, itemsize);
      if result <> 0 then
        exit;
      dec(n1); // both items are equal -> continue to next items
      dec(n2);
    until (n2 = 0) or (n1 = 0);
  result := n1 - n2;
end;

function DynArrayEquals(TypeInfo: pointer; var Array1, Array2;
  Array1Count, Array2Count: PInteger): boolean;
begin
  result := DynArrayCompare(@Array1, @Array2, Array1Count, Array2Count,
    TypeInfo, {CaseSensitive=}true) = 0;
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
  fields.Fields := @RTTI_BINARYSAVE; // reuse pointer slot on stack
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
      offset := PRttiBinarySaves(fields.Fields)[Info^.Kind](Data, Dest, Info);
      inc(Data, offset);
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
  fields.Fields := @RTTI_BINARYLOAD; // reuse pointer slot on stack
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
      offset := PRttiBinaryLoads(fields.Fields)[Info^.Kind](Data, Source, Info);
      inc(Data, offset);
      inc(offset, f^.Offset);
    end;
    inc(f);
  end;
  offset := PtrUInt(fields.Size) - offset;
  if offset > 0 then
    Source.Copy(Data, offset);
  result := fields.Size;
end;

function _RecordCompare(A, B: PUTF8Char; Info: PRttiInfo;
 CaseInSensitive: boolean): integer;
var
  fields: TRttiRecordManagedFields; // Size/Count/Fields
  offset: PtrUInt;
  f: PRttiRecordField;
begin
  Info^.RecordManagedFields(fields);
  f := fields.Fields;
  fields.Fields := @RTTI_COMPARE[CaseInSensitive]; // reuse pointer slot on stack
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
      offset := PRttiCompares(fields.Fields)[Info^.Kind](A, B, Info, result);
      inc(A, offset);
      inc(B, offset);
      if result <> 0 then
        exit;
      inc(offset, f^.Offset);
    end;
    inc(f);
  end;
  offset := PtrUInt(fields.Size) - offset;
  if offset > 0 then
    result := StrCompL(A, B, offset); // compare trailing binary
end;

function _BC_Record(A, B: pointer; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := _RecordCompare(A, B, Info, {caseinsens=}false);
  result := Info^.RecordSize;
end;

function _BCI_Record(A, B: pointer; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := _RecordCompare(A, B, Info, {caseinsens=}true);
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
    sav := RTTI_BINARYSAVE[Info^.Kind];
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
    load := RTTI_BINARYLOAD[Info^.Kind];
    if Assigned(load) then // paranoid check
      repeat
        inc(Data, load(Data, Source, Info));
        dec(n);
      until n = 0;
  end;
end;

function _ArrayCompare(A, B: PUTF8Char; Info: PRttiInfo; CaseInSensitive: boolean;
  out ArraySize: PtrInt): integer;
var
  n, itemsize: PtrInt;
  cmp: TRttiCompare;
begin
  Info := Info^.ArrayItemType(n, ArraySize);
  if Info = nil then
    result := StrCompL(A, B, ArraySize)
  else
  begin
    cmp := RTTI_COMPARE[CaseInSensitive, Info^.Kind];
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
  Compared := _ArrayCompare(A, B, Info, {caseinsens=}false, result);
end;

function _BCI_Array(A, B: pointer; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := _ArrayCompare(A, B, Info, {caseinsens=}true, result);
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
  Source.VarBlob(temp); // load into a private copy for in-place JSON parsing
  try
    BinaryVariantLoadAsJSON(Data^, temp.buf, Source.CustomVariants);
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
      if vt = 255 then // valOleStr
        Dest.WriteVar(Data^.vAny, length(PWideString(Data^.vAny)^) * 2)
      else
        Dest.Write(@Data^.VInt64, vt); // simple types
  end
  else if vt = varString then // expect only RawUTF8
    Dest.WriteVar(Data^.vAny, length(PRawByteString(Data^.vAny)^))
  {$ifdef HASVARUSTRING}
  else if vt = varUString then
    Dest.WriteVar(Data^.vAny, length(PUnicodeString(Data^.vAny)^) * 2)
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
        with Source.VarBlob do // valOleStr
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
    Source.ErrorData('RTTI_BINARYLOAD[tkVariant] missing mormot.core.json.pas', []);
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

function ObjectCompare(A, B: TObject; CaseInSensitive: boolean): integer;
var
  rA, rB: TRttiCustom;
  pA, pB: PRttiCustomProp;
  i: integer;
begin
  if (A = nil) or (B = nil) or (A = B) then
  begin
    result := ComparePointer(A, B);
    exit;
  end;
  result := 0;
  rA := Rtti.RegisterClass(PClass(A)^); // faster than RegisterType(Info)
  pA := pointer(rA.Props.List);
  if PClass(B)^.InheritsFrom(PClass(A)^) then
    // same (or similar/inherited) class -> compare per exact properties
    for i := 1 to rA.Props.Count do
    begin
      result := pA^.CompareValue(A, B, pA^, CaseInSensitive);
      if result <> 0 then
        exit;
      inc(pA);
    end
  else
  begin
    // compare properties by name
    rB := Rtti.RegisterClass(PPointer(B)^);
    for i := 1 to rA.Props.Count do
    begin
      pB := rB.Props.Find(pA^.Name^);
      if pB <> nil then
      begin
        result := pA^.CompareValue(A, B, pB^, CaseInSensitive);
        if result <> 0 then
          exit;
      end;
      inc(pA);
    end;
  end;
end;

function _BC_Object(A, B: pointer; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := ObjectCompare(PPointer(A)^, PPointer(B)^, {caseinsens=}false);
  result := SizeOf(pointer);
end;

function _BCI_Object(A, B: pointer; Info: PRttiInfo; out Compared: integer): PtrInt;
begin
  Compared := ObjectCompare(PPointer(A)^, PPointer(B)^, {caseinsens=}true);
  result := SizeOf(pointer);
end;

function ObjectEquals(A, B: TObject): integer;
begin
  result := ObjectCompare(A, B, {caseinsensitive=}false);
end;

function ObjectEqualsI(A, B: TObject): integer;
begin
  result := ObjectCompare(A, B, {caseinsensitive=}true);
end;


function BinaryEquals(A, B: pointer; Info: PRttiInfo; PSize: PInteger;
  Kinds: TRttiKinds; CaseInSensitive: boolean): boolean;
var
  size, comp: integer;
  cmp: TRttiCompare;
begin
  if A <> B then
  begin
    cmp := RTTI_COMPARE[CaseInSensitive, Info^.Kind];
    if Assigned(cmp) and (Info^.Kind in Kinds) then
    begin
      size := cmp(A, B, Info, comp);
      if PSize <> nil then
        PSize^ := size; // warning: PSize not set if RTTI_COMPARE[] not available
      result := comp = 0;
    end
    else
      result := false; // no fair comparison possible
  end
  else
    result := true; // A=B at pointer level
end;

function BinaryCompare(A, B: pointer; Info: PRttiInfo;
  CaseInSensitive: boolean): integer;
var
  cmp: TRttiCompare;
begin
  result := 0; // A=B at pointer level, or no fair comparison possible
  if (A = B) or (Info = nil) then
    exit;
  cmp := RTTI_COMPARE[CaseInSensitive, Info^.Kind];
  if Assigned(cmp) then
    cmp(A, B, Info, result);
end;

{$ifndef PUREMORMOT2}

function BinarySaveLength(Data: pointer; Info: PRttiInfo; Len: PInteger;
  Kinds: TRttiKinds): integer;
var
  size: integer;
  W: TBufferWriter; // not very fast, but good enough (RecordSave don't use it)
  temp: array[byte] of byte; // will use mostly TFakeWriterStream.Write()
  save: TRttiBinarySave;
begin
  save := RTTI_BINARYSAVE[Info^.Kind];
  if Assigned(save) and (Info^.Kind in Kinds) then
  begin
    W := TBufferWriter.Create(TFakeWriterStream, @temp, SizeOf(temp));
    try
      size := save(Data, W, Info);
      result := W.TotalWritten;
      if Len <> nil then
        Len^ := size;
    finally
      W.Free;
    end;
  end
  else
    result := 0;
end;

function BinarySave(Data: pointer; Dest: PAnsiChar; Info: PRttiInfo;
  out Len: integer; Kinds: TRttiKinds): PAnsiChar;
var
  W: TBufferWriter;
  save: TRttiBinarySave;
begin
  save := RTTI_BINARYSAVE[Info^.Kind];
  if Assigned(save) and (Info^.Kind in Kinds) then
  begin
    W := TBufferWriter.Create(TFakeWriterStream, Dest, 1 shl 30);
    try
      Len := save(Data, W, Info);
      result := Dest + W.BufferPosition; // Dest was a 1GB temporary buffer :)
    finally
      W.Free;
    end;
  end
  else
    result := nil;
end;

{$endif PUREMORMOT2}

function BinarySave(Data: pointer; Info: PRttiInfo; Kinds: TRttiKinds): RawByteString;
var
  W: TBufferWriter;
  temp: TTextWriterStackBuffer; // 8KB
  save: TRttiBinarySave;
begin
  save := RTTI_BINARYSAVE[Info^.Kind];
  if Assigned(save) and (Info^.Kind in Kinds) then
  begin
    W := TBufferWriter.Create(temp{%H-});
    try
      save(Data, W, Info);
      result := W.FlushTo;
    finally
      W.Free;
    end;
  end
  else
    result := '';
end;

procedure BinarySave(Data: pointer; Info: PRttiInfo; Dest: TBufferWriter);
var
  save: TRttiBinarySave;
begin
  save := RTTI_BINARYSAVE[Info^.Kind];
  if Assigned(save) then
    save(Data, Dest, Info);
end;

function BinarySaveBytes(Data: pointer; Info: PRttiInfo; Kinds: TRttiKinds): TBytes;
var
  W: TBufferWriter;
  temp: TTextWriterStackBuffer; // 8KB
  save: TRttiBinarySave;
begin
  save := RTTI_BINARYSAVE[Info^.Kind];
  if Assigned(save) and (Info^.Kind in Kinds) then
  begin
    W := TBufferWriter.Create(temp{%H-});
    try
      save(Data, W, Info);
      result := W.FlushToBytes;
    finally
      W.Free;
    end;
  end
  else
    result := nil;
end;

procedure BinarySave(Data: pointer; var Dest: TSynTempBuffer; Info: PRttiInfo;
  Kinds: TRttiKinds);
var
  W: TBufferWriter;
  save: TRttiBinarySave;
begin
  save := RTTI_BINARYSAVE[Info^.Kind];
  if Assigned(save) and (Info^.Kind in Kinds) then
  begin
    W := TBufferWriter.Create(TRawByteStringStream, @Dest.tmp,
      SizeOf(Dest.tmp) - 16); // Dest.Init() reserves 16 additional bytes
    try
      save(Data, W, Info);
      if W.Stream.Position = 0 then
        Dest.Init(W.TotalWritten)  // only Dest.tmp buffer was used
      else
        Dest.Init(W.FlushTo);      // more than 4KB -> temporary allocation
    finally
      W.Free;
    end;
  end
  else
    Dest.Init(0);
end;

function BinarySaveBase64(Data: pointer; Info: PRttiInfo; UriCompatible: boolean;
  Kinds: TRttiKinds; NoCrc32Trailer: boolean): RawUTF8;
var
  W: TBufferWriter;
  temp: TTextWriterStackBuffer; // 8KB
  tmp: RawByteString;
  P: PAnsiChar;
  len: integer;
  save: TRttiBinarySave;
begin
  save := RTTI_BINARYSAVE[Info^.Kind];
  if Assigned(save) and (Info^.Kind in Kinds) then
  begin
    W := TBufferWriter.Create(temp{%H-});
    try
      if not NoCrc32Trailer then
        W.Write4(0); // placeholder for the trailing crc32c
      save(Data, W, Info);
      len := W.TotalWritten;
      if W.Stream.Position = 0 then
        P := pointer(@temp) // only temp buffer was used
      else
      begin
        tmp := W.FlushTo; // more than 8KB -> temporary allocation
        P := pointer(tmp);
      end;
      if not NoCrc32Trailer then
        PCardinal(P)^ := crc32c(0, P + 4, len - 4);
      if UriCompatible then
        result := BinToBase64uri(P, len)
      else
        result := BinToBase64(P, len);
    finally
      W.Free;
    end;
  end
  else
    result := '';
end;

function BinaryLoad(Data: pointer; Source: PAnsiChar; Info: PRttiInfo;
  Len: PInteger; SourceMax: PAnsiChar; Kinds: TRttiKinds;
  TryCustomVariants: PDocVariantOptions): PAnsiChar;
var
  size: integer;
  read: TFastReader;
  load: TRttiBinaryLoad;
begin
  load := RTTI_BINARYLOAD[Info^.Kind];
  if Assigned(load) and (Info^.Kind in Kinds) then
  begin
    read.Init(Source, SourceMax - Source);
    read.CustomVariants := TryCustomVariants;
    size := load(Data, read, Info);
    if Len <> nil then
      Len^ := size;
    result := read.P;
  end
  else
    result := nil;
end;

function BinaryLoad(Data: pointer; const Source: RawByteString; Info: PRttiInfo;
  Kinds: TRttiKinds; TryCustomVariants: PDocVariantOptions): boolean;
var
  P: PAnsiChar;
begin
  if Info^.Kind in Kinds then
  begin
    P := pointer(Source);
    P := BinaryLoad(Data, P, Info, nil, P + length(Source), Kinds, TryCustomVariants);
    result := (P <> nil) and (P - pointer(Source) = length(Source));
  end
  else
    result := false;
end;

function BinaryLoadBase64(Source: PAnsiChar; Len: PtrInt; Data: pointer;
  Info: PRttiInfo; UriCompatible: boolean; Kinds: TRttiKinds;
  NoCrc32Trailer: boolean; TryCustomVariants: PDocVariantOptions): boolean;
var
  temp: TSynTempBuffer;
  tempend: pointer;
begin
  if (Len > 6) and (Info^.Kind in Kinds) then
  begin
    if UriCompatible then
      result := Base64uriToBin(Source, Len, temp)
    else
      result := Base64ToBin(Source, Len, temp);
    tempend := PAnsiChar(temp.buf) + temp.len;
    if result then
      if NoCrc32Trailer then
        result := (BinaryLoad(Data, temp.buf, Info, nil, tempend,
            Kinds, TryCustomVariants) = tempend)
      else
        result := (temp.len >= 4) and
          (crc32c(0, PAnsiChar(temp.buf) + 4, temp.len - 4) = PCardinal(temp.buf)^) and
          (BinaryLoad(Data, PAnsiChar(temp.buf) + 4, Info, nil, tempend,
            Kinds, TryCustomVariants) = tempend);
    temp.Done;
  end
  else
    result := false;
end;


function RecordEquals(const RecA, RecB; TypeInfo: pointer; PRecSize: PInteger;
  CaseInSensitive: boolean): boolean;
begin
  result := BinaryEquals(@RecA, @RecB, TypeInfo, PRecSize,
    rkRecordTypes, CaseInSensitive);
end;

{$ifndef PUREMORMOT2}

function RecordSaveLength(const Rec; TypeInfo: pointer; Len: PInteger): integer;
begin
 result := {%H-}BinarySaveLength(@Rec, TypeInfo, Len, rkRecordTypes);
end;

function RecordSave(const Rec; Dest: PAnsiChar; TypeInfo: pointer;
  out Len: integer): PAnsiChar;
begin
  result := {%H-}BinarySave(@Rec, Dest, TypeInfo, Len, rkRecordTypes);
end;

function RecordSave(const Rec; Dest: PAnsiChar; TypeInfo: pointer): PAnsiChar;
var
  dummylen: integer;
begin
  result := {%H-}BinarySave(@Rec, Dest, TypeInfo, dummylen, rkRecordTypes);
end;

{$endif PUREMORMOT2}

function RecordSave(const Rec; TypeInfo: pointer): RawByteString;
begin
  result := BinarySave(@Rec, TypeInfo, rkRecordTypes);
end;

function RecordSaveBytes(const Rec; TypeInfo: pointer): TBytes;
begin
 result := BinarySaveBytes(@Rec, TypeInfo, rkRecordTypes);
end;

procedure RecordSave(const Rec; var Dest: TSynTempBuffer; TypeInfo: pointer);
begin
  BinarySave(@Rec, Dest, TypeInfo, rkRecordTypes);
end;

function RecordSaveBase64(const Rec; TypeInfo: pointer; UriCompatible: boolean): RawUTF8;
begin
  result := BinarySaveBase64(@Rec, TypeInfo, UriCompatible, rkRecordTYpes);
end;

function RecordLoad(var Rec; Source: PAnsiChar; TypeInfo: pointer;
  Len: PInteger; SourceMax: PAnsiChar; TryCustomVariants: PDocVariantOptions): PAnsiChar;
begin
  result := BinaryLoad(@Rec, Source, TypeInfo, Len, SourceMax,
    rkRecordTypes, TryCustomVariants);
end;

function RecordLoad(var Rec; const Source: RawByteString; TypeInfo: pointer;
  TryCustomVariants: PDocVariantOptions): boolean;
begin
  result := BinaryLoad(@Rec, Source, TypeInfo, rkRecordTypes, TryCustomVariants);
end;

function RecordLoadBase64(Source: PAnsiChar; Len: PtrInt; var Rec;
  TypeInfo: pointer; UriCompatible: boolean; TryCustomVariants: PDocVariantOptions): boolean;
begin
  result := BinaryLoadBase64(Source, Len, @Rec, TypeInfo, UriCompatible,
    rkRecordTypes, {notrailer=}false, TryCustomVariants);
end;



{ ************ TDynArray, TDynArrayHashed and TSynQueue Wrappers }

{ TDynArray }

procedure TDynArray.InitRtti(aInfo: TRttiCustom; var aValue;
  aCountPointer: PInteger);
begin
  fInfo := aInfo;
  fValue := @aValue;
  fElemSize := fInfo.Cache.ItemSize;
  fCountP := aCountPointer;
  if fCountP <> nil then
    fCountP^ := 0;
  fCompare := nil;
  fSorted := false;
end;

procedure TDynArray.Init(aTypeInfo: PRttiInfo; var aValue;
  aCountPointer: PInteger);
begin
  if aTypeInfo^.Kind <> rkDynArray then
    raise EDynArray.CreateUTF8('TDynArray.Init: % is %, expected rkDynArray',
      [aTypeInfo.RawName, ToText(aTypeInfo.Kind)^]);
  InitRtti(Rtti.RegisterType(aTypeInfo), aValue, aCountPointer);
end;

procedure TDynArray.InitSpecific(aTypeInfo: PRttiInfo; var aValue;
  aKind: TRttiParserType; aCountPointer: PInteger; aCaseInsensitive: boolean);
begin
  if aTypeInfo^.Kind <> rkDynArray then
    raise EDynArray.CreateUTF8('TDynArray.InitSpecific: % is %, expected rkDynArray',
      [aTypeInfo.RawName, ToText(aTypeInfo.Kind)^]);
  InitRtti(Rtti.RegisterType(aTypeInfo), aValue, aCountPointer);
  fCompare := PT_SORT[aCaseInsensitive, aKind];
  if not Assigned(fCompare) then
    if aKind = ptVariant then
      raise EDynArray.CreateUTF8('TDynArray.InitSpecific(%): missing mormot.core.json',
        [Info.Name, ToText(aKind)^])
    else
      raise EDynArray.CreateUTF8('TDynArray.InitSpecific(%) unsupported %',
        [Info.Name, ToText(aKind)^]);
end;

function TDynArray.GetCount: PtrInt;
begin
  result := PtrUInt(fCountP);
  if result <> 0 then
    result := PInteger(result)^
  else
  begin
    result := PtrUInt(fValue);
    if result <> 0 then
    begin
      result := PPtrInt(result)^;
      if result <> 0 then
        result := PDALen(result - _DALEN)^ + _DAOFF;
    end;
  end;
end;

procedure TDynArray.ItemCopy(Source, Dest: pointer);
begin
  if fInfo.ArrayRtti <> nil then
    fInfo.ArrayRtti.ValueCopy(Dest, Source)
  else
    MoveFast(Source^, Dest^, fElemSize);
end;

procedure TDynArray.ItemClear(Item: pointer);
begin
  if Item = nil then
    exit;
  if fInfo.ArrayRtti <> nil then
    fInfo.ArrayRtti.ValueFinalize(Item);
  FillCharFast(Item^, fElemSize, 0); // always
end;

function TDynArray.ItemEquals(A, B: pointer; CaseInSensitive: boolean): boolean;
var
  comp: TRttiCompare;
  rtti: PRttiInfo;
  cmp: integer;
label
  bin;
begin
  if Assigned(fCompare) then
    result := fCompare(A^, B^) = 0
  else if not(rcfArrayItemManaged in fInfo.Flags) then
bin:result := CompareMemFixed(@A, @B, fElemSize) // binary comparison
  else
  begin
    rtti := fInfo.Cache.ItemInfo;
    comp := RTTI_COMPARE[CaseInsensitive, rtti.Kind];
    if Assigned(comp) then
    begin
      comp(A, B, rtti, cmp);
      result := cmp = 0;
    end
    else
      goto bin;
  end;
end;

function TDynArray.ItemCompare(A, B: pointer; CaseInSensitive: boolean = false): integer;
var
  comp: TRttiCompare;
  rtti: PRttiInfo;
label
  bin;
begin
  if Assigned(fCompare) then
    result := fCompare(A^, B^)
  else if not(rcfArrayItemManaged in fInfo.Flags) then
bin:result := StrCompL(A, B, fElemSize) // binary comparison
  else
  begin
    rtti := fInfo.Cache.ItemInfo;
    comp := RTTI_COMPARE[CaseInsensitive, rtti.Kind];
    if Assigned(comp) then
      comp(A, B, rtti, result)
    else
      goto bin;
  end;
end;

function TDynArray.Add(const Item): PtrInt;
begin
  result := GetCount;
  if fValue = nil then
    exit; // avoid GPF if void
  SetCount(result + 1);
  ItemCopy(@Item, PAnsiChar(fValue^) + PtrUInt(result) * fElemSize);
end;

function TDynArray.New: integer;
begin
  result := GetCount;
  if fValue = nil then
    exit; // avoid GPF if void
  SetCount(result + 1);
end;

function TDynArray.Peek(var Dest): boolean;
var
  index: PtrInt;
begin
  index := GetCount - 1;
  result := index >= 0;
  if result then
    ItemCopy(PAnsiChar(fValue^) + PtrUInt(index) * fElemSize, @Dest);
end;

function TDynArray.Pop(var Dest): boolean;
var
  index: integer;
begin
  index := GetCount - 1;
  result := index >= 0;
  if result then
  begin
    ItemMoveTo(index, @Dest);
    SetCount(index);
  end;
end;

procedure TDynArray.Insert(Index: PtrInt; const Item);
var
  n: PtrInt;
  P: PAnsiChar;
begin
  if fValue = nil then
    exit; // avoid GPF if void
  n := GetCount;
  SetCount(n + 1);
  if PtrUInt(Index) < PtrUInt(n) then
  begin
    // reserve space for the new item
    P := PAnsiChar(fValue^) + PtrUInt(Index) * ElemSize;
    MoveFast(P[0], P[ElemSize], PtrUInt(n - Index) * ElemSize);
    if rcfArrayItemManaged in fInfo.Flags then // avoid GPF in ItemCopy() below
      FillCharFast(P^, ElemSize, 0);
  end
  else
    // Index>=Count -> add at the end
    P := PAnsiChar(fValue^) + PtrUInt(n) * ElemSize;
  ItemCopy(@Item, P);
end;

procedure TDynArray.Clear;
begin
  SetCount(0);
end;

function TDynArray.ClearSafe: boolean;
begin
  try
    SetCount(0);
    result := true;
  except // weak code, but may be a good idea in a destructor
    result := false;
  end;
end;

function TDynArray.Delete(aIndex: PtrInt): boolean;
var
  n, len: PtrInt;
  P: PAnsiChar;
begin
  result := false;
  if fValue = nil then
    exit; // avoid GPF if void
  n := GetCount;
  if PtrUInt(aIndex) >= PtrUInt(n) then
    exit; // out of range
  if PRefCnt(PAnsiChar(fValue^) - _DAREFCNT)^ > 1 then
    InternalSetLength(n, n); // unique
  dec(n);
  P := PAnsiChar(fValue^) + PtrUInt(aIndex) * ElemSize;
  if fInfo.ArrayRtti <> nil then
    fInfo.ArrayRtti.ValueFinalize(P);
  if n > aIndex then
  begin
    len := PtrUInt(n - aIndex) * ElemSize;
    MoveFast(P[ElemSize], P[0], len);
    FillCharFast(P[len], ElemSize, 0);
  end
  else
    FillCharFast(P^, ElemSize, 0);
  SetCount(n);
  result := true;
end;

function TDynArray.ItemPtr(index: PtrInt): pointer;
label
  ok;
var
  c: PtrUInt;
begin // very efficient code on FPC and modern Delphi
  result := pointer(fValue);
  if result = nil then
    exit;
  result := PPointer(result)^;
  if result = nil then
    exit;
  c := PtrUInt(fCountP);
  if c <> 0 then
  begin
    if PtrUInt(index) < PCardinal(c)^ then
ok:   inc(PByte(result), PtrUInt(index) * ElemSize)
    else
      result := nil
  end
  else
    {$ifdef FPC} // FPC stores high() in TDALen=PtrInt
    if PtrUInt(index) <= PPtrUInt(PAnsiChar(result) - _DALEN)^ then
    {$else}     // Delphi stores length() in TDALen=NativeInt
    if PtrUInt(index) < PPtrUInt(PtrUInt(result) - _DALEN)^ then
    {$endif FPC}
      goto ok
    else
      result := nil;
end;

procedure TDynArray.ItemCopyAt(index: PtrInt; Dest: pointer);
var
  p: pointer;
begin
  p := ItemPtr(index);
  if p <> nil then
    ItemCopy(p, Dest);
end;

procedure TDynArray.ItemMoveTo(index: PtrInt; Dest: pointer);
var
  p: pointer;
begin
  p := ItemPtr(index);
  if (p = nil) or (Dest = nil) then
    exit;
  if fInfo.ArrayRtti <> nil then
    fInfo.ArrayRtti.ValueFinalize(Dest); // also handle T*ObjArray
  MoveFast(p^, Dest^, ElemSize);
  FillCharFast(p^, ElemSize, 0);
end;

procedure TDynArray.ItemCopyFrom(Source: pointer; index: PtrInt;
  ClearBeforeCopy: boolean);
var
  p: pointer;
begin
  p := ItemPtr(index);
  if p <> nil then
  begin
    if ClearBeforeCopy then // safer if Source is a copy of p^
      ItemClear(p);
    ItemCopy(Source, p);
  end;
end;

{$ifdef CPU64}
procedure Exchg16(P1, P2: PPtrIntArray); inline;
var
  c: PtrInt;
begin
  c := P1[0];
  P1[0] := P2[0];
  P2[0] := c;
  c := P1[1];
  P1[1] := P2[1];
  P2[1] := c;
end;
{$endif CPU64}

procedure TDynArray.Reverse;
var
  n, siz: PtrInt;
  P1, P2: PAnsiChar;
  c: AnsiChar;
  i32: integer;
  i64: Int64;
begin
  n := GetCount - 1;
  if n > 0 then
  begin
    siz := ElemSize;
    P1 := fValue^;
    case siz of
      1:
        begin // optimized version for TByteDynArray and such
          P2 := P1 + n;
          while P1 < P2 do
          begin
            c := P1^;
            P1^ := P2^;
            P2^ := c;
            inc(P1);
            dec(P2);
          end;
        end;
      4:
        begin // optimized version for TIntegerDynArray and such
          P2 := P1 + n * SizeOf(Integer);
          while P1 < P2 do
          begin
            i32 := PInteger(P1)^;
            PInteger(P1)^ := PInteger(P2)^;
            PInteger(P2)^ := i32;
            inc(P1, 4);
            dec(P2, 4);
          end;
        end;
      8:
        begin // optimized version for TInt64DynArray + TDoubleDynArray and such
          P2 := P1 + n * SizeOf(Int64);
          while P1 < P2 do
          begin
            i64 := PInt64(P1)^;
            PInt64(P1)^ := PInt64(P2)^;
            PInt64(P2)^ := i64;
            inc(P1, 8);
            dec(P2, 8);
          end;
        end;
      16:
        begin // optimized version for 32-bit TVariantDynArray and such
          P2 := P1 + n * 16;
          while P1 < P2 do
          begin
            {$ifdef CPU64}Exchg16{$else}ExchgVariant{$endif}(Pointer(P1),Pointer(P2));
            inc(P1, 16);
            dec(P2, 16);
          end;
        end;
    {$ifdef CPU64}
      24:
        begin // optimized version for 64-bit TVariantDynArray and such
          P2 := P1 + n * 24;
          while P1 < P2 do
          begin
            ExchgVariant(Pointer(P1), Pointer(P2));
            inc(P1, 24);
            dec(P2, 24);
          end;
        end;
    {$endif CPU64}
    else
      begin // generic version
        P2 := P1 + n * siz;
        while P1 < P2 do
        begin
          Exchg(P1, P2, siz);
          inc(P1, siz);
          dec(P2, siz);
        end;
      end;
    end;
  end;
end;

procedure TDynArray.SaveTo(W: TBufferWriter);
begin
  DynArraySave(pointer(fValue), fCountP, W, Info.Cache.ItemInfo);
end;

procedure TDynArray.SaveToStream(Stream: TStream);
var
  W: TBufferWriter;
  tmp: TTextWriterStackBuffer; // 8KB buffer
begin
  if (fValue = nil) or (Stream = nil) then
    exit; // avoid GPF if void
  W := TBufferWriter.Create(Stream, @tmp, SizeOf(tmp));
  try
    SaveTo(W);
    W.Flush;
  finally
    W.Free;
  end;
end;

function TDynArray.SaveTo: RawByteString;
var
  W: TRawByteStringStream;
begin
  W := TRawByteStringStream.Create;
  try
    SaveToStream(W);
    result := W.DataString;
  finally
    W.Free;
  end;
end;

function TDynArray.LoadFrom(Source: PAnsiChar; SourceMax: PAnsiChar): PAnsiChar;
begin
  result := BinaryLoad(fValue, Source, Info.Cache.ItemInfo, nil, SourceMax, [rkDynArray]);
end;

procedure TDynArray.LoadFromStream(Stream: TCustomMemoryStream);
var
  S, P: PAnsiChar;
begin
  S := PAnsiChar(Stream.Memory);
  P := S + Stream.Position;
  P := BinaryLoad(fValue, P, Info.Cache.ItemInfo, nil, S + Stream.Size, [rkDynArray]);
  Stream.Seek(P - S, soFromBeginning);
end;

function TDynArray.LoadFromBinary(const Buffer: RawByteString): boolean;
begin
  result := BinaryLoad(fValue, Buffer, Info.Cache.ItemInfo, [rkDynArray]);
end;

function TDynArray.SaveToJSON(EnumSetsAsText: boolean; reformat: TTextWriterJSONFormat): RawUTF8;
begin
  SaveToJSON(result, EnumSetsAsText, reformat);
end;

procedure TDynArray.SaveToJSON(out result: RawUTF8; EnumSetsAsText: boolean;
  reformat: TTextWriterJSONFormat);
var
  W: TBaseWriter;
  temp: TTextWriterStackBuffer;
begin
  if GetCount = 0 then
    result := '[]'
  else
  begin
    W := DefaultTextWriterSerializer.CreateOwnedStream(temp);
    try
      if EnumSetsAsText then
        W.CustomOptions := W.CustomOptions + [twoEnumSetsAsTextInRecord];
      SaveToJSON(W);
      W.SetText(result, reformat);
    finally
      W.Free;
    end;
  end;
end;

procedure TDynArray.SaveToJSON(W: TBaseWriter);
var
  len, backup: PtrInt;
  hacklen: PDALen;
begin
  len := GetCount;
  if len = 0 then
    W.Add('[', ']')
  else
  begin
    hacklen := PDALen(PAnsiChar(fValue^) - _DALEN);
    backup := hacklen^;
    try
      hacklen^ := len - _DAOFF; // may use ExternalCount
      W.AddTypedJSON(fValue, Info.Info); // serialization from mormot.core.json
    finally
      hacklen^ := backup;
    end;
  end;
end;

procedure _GetDataFromJSON(Data: pointer; var JSON: PUTF8Char; EndOfObject: PUTF8Char;
  TypeInfo: PRttiInfo; CustomVariantOptions: PDocVariantOptions; Tolerant: boolean);
begin
  raise ERttiException.Create('GetDataFromJSON() not available - include ' +
    'mormot.core.json.pas in your uses clause');
end;

function TDynArray.LoadFromJSON(P: PUTF8Char; EndOfObject: PUTF8Char;
  CustomVariantOptions: PDocVariantOptions; Tolerant: boolean): PUTF8Char;
begin
  SetCount(0); // faster to use our own routine now
  GetDataFromJSON(fValue, P, EndOfObject, Info.Cache.ItemInfo,
    CustomVariantOptions, Tolerant);
  result := P;
end;

function TDynArray.ItemCopyFirstField(Source, Dest: Pointer): boolean;
var
  rtti: PRttiInfo;
begin
  result := false;
  if fInfo.ArrayFirstField in ptUnmanagedTypes then
    MoveFast(Source^, Dest^, PT_SIZE[fInfo.ArrayFirstField])
  else
    begin
      rtti := PT_INFO[fInfo.ArrayFirstField];
      if rtti = nil then
        exit; // ptNone, ptInterface, ptCustom
      rtti^.Copy(Dest, Source);
    end;
  result := true;
end;

function TDynArray.Find(const Item; const aIndex: TIntegerDynArray;
  aCompare: TDynArraySortCompare): PtrInt;
var
  n, L: PtrInt;
  cmp: integer;
  P: PAnsiChar;
begin
  n := GetCount;
  if Assigned(aCompare) and (n > 0) then
  begin
    dec(n);
    P := fValue^;
    if (n > 10) and (length(aIndex) >= n) then
    begin
      // array should be sorted via aIndex[] -> use fast O(log(n)) binary search
      L := 0;
      repeat
        result := (L + n) shr 1;
        cmp := aCompare(P[cardinal(aIndex[result]) * ElemSize], Item);
        if cmp = 0 then
        begin
          result := aIndex[result]; // returns index in TDynArray
          exit;
        end;
        if cmp < 0 then
          L := result + 1
        else
          n := result - 1;
      until L > n;
    end
    else // array is not sorted, or aIndex=nil -> use O(n) iterating search
      for result := 0 to n do
        if aCompare(P^, Item) = 0 then
          exit
        else
          inc(P, ElemSize);
  end;
  result := -1;
end;

function TDynArray.FindIndex(const Item; aIndex: PIntegerDynArray;
  aCompare: TDynArraySortCompare): PtrInt;
begin
  if aIndex <> nil then
    result := Find(Item, aIndex^, aCompare)
  else
    result := Find(Item, aCompare);
end;

function TDynArray.FindAndFill(var Item; aIndex: PIntegerDynArray;
  aCompare: TDynArraySortCompare): integer;
begin
  result := FindIndex(Item, aIndex, aCompare);
  if result >= 0 then // if found, fill Item with the matching item
    ItemCopy(PAnsiChar(fValue^) + (cardinal(result) * ElemSize), @Item);
end;

function TDynArray.FindAndDelete(const Item; aIndex: PIntegerDynArray;
  aCompare: TDynArraySortCompare): integer;
begin
  result := FindIndex(Item, aIndex, aCompare);
  if result >= 0 then
    Delete(result);
end;

function TDynArray.FindAndUpdate(const Item; aIndex: PIntegerDynArray;
  aCompare: TDynArraySortCompare): integer;
begin
  result := FindIndex(Item, aIndex, aCompare);
  if result >= 0 then // if found, fill Elem with the matching item
    ItemCopy(@Item, PAnsiChar(fValue^) + (cardinal(result) * ElemSize));
end;

function TDynArray.FindAndAddIfNotExisting(const Item; aIndex: PIntegerDynArray;
  aCompare: TDynArraySortCompare): integer;
begin
  result := FindIndex(Item, aIndex, aCompare);
  if result < 0 then
    Add(Item); // -1 will mark success
end;

function TDynArray.Find(const Item; aCompare: TDynArraySortCompare): PtrInt;
var
  n, L: PtrInt;
  cmp: integer;
  P: PAnsiChar;
begin
  n := GetCount;
  if not Assigned(aCompare) then
    aCompare := fCompare;
  if Assigned(aCompare) and (n > 0) then
  begin
    dec(n);
    P := fValue^;
    if fSorted and (@aCompare = @fCompare) and (n > 10) then
    begin
      // array is sorted -> use fast O(log(n)) binary search
      L := 0;
      repeat
        result := (L + n) shr 1;
        cmp := aCompare(P[cardinal(result) * ElemSize], Item);
        if cmp = 0 then
          exit;
        if cmp < 0 then
          L := result + 1
        else
          n := result - 1;
      until L > n;
    end
    else // array is very small, or not sorted
      for result := 0 to n do
        if aCompare(P^, Item) = 0 then // O(n) search
          exit
        else
          inc(P, ElemSize);
  end;
  result := -1;
end;

function TDynArray.FindAllSorted(const Item; out FirstIndex, LastIndex: Integer): boolean;
var
  found, last: integer;
  P: PAnsiChar;
begin
  result := FastLocateSorted(Item, found);
  if not result then
    exit;
  FirstIndex := found;
  P := fValue^;
  while (FirstIndex > 0) and
        (fCompare(P[cardinal(FirstIndex - 1) * ElemSize], Item) = 0) do
    dec(FirstIndex);
  last := GetCount - 1;
  LastIndex := found;
  while (LastIndex < last) and
        (fCompare(P[cardinal(LastIndex + 1) * ElemSize], Item) = 0) do
    inc(LastIndex);
end;

function TDynArray.FastLocateSorted(const Item; out Index: Integer): boolean;
var
  n, i, cmp: integer;
  P: PAnsiChar;
begin
  result := False;
  n := GetCount;
  if Assigned(fCompare) then
    if n = 0 then // a void array is always sorted
      Index := 0
    else if fSorted then
    begin
      P := fValue^;
      dec(n);
      cmp := fCompare(Item, P[cardinal(n) * ElemSize]);
      if cmp >= 0 then
      begin // greater than last sorted item
        Index := n;
        if cmp = 0 then
          result := true
        else // returns true + index of existing Elem
          inc(Index); // returns false + insert after last position
        exit;
      end;
      Index := 0;
      while Index <= n do
      begin // O(log(n)) binary search of the sorted position
        i := (Index + n) shr 1;
        cmp := fCompare(P[cardinal(i) * ElemSize], Item);
        if cmp = 0 then
        begin
          Index := i; // returns true + index of existing Elem
          result := True;
          exit;
        end
        else if cmp < 0 then
          Index := i + 1
        else
          n := i - 1;
      end;
      // Elem not found: returns false + the index where to insert
    end
    else
      Index := -1
  else // not Sorted
    Index := -1; // no fCompare()
end;

procedure TDynArray.FastAddSorted(Index: Integer; const Item);
begin
  Insert(Index, Item);
  fSorted := true; // Insert -> SetCount -> fSorted := false
end;

procedure TDynArray.FastDeleteSorted(Index: Integer);
begin
  Delete(Index);
  fSorted := true; // Delete -> SetCount -> fSorted := false
end;

function TDynArray.FastLocateOrAddSorted(const Item; wasAdded: Pboolean): integer;
var
  toInsert: boolean;
begin
  toInsert := not FastLocateSorted(Item, result) and (result >= 0);
  if toInsert then
  begin
    Insert(result, Item);
    fSorted := true; // Insert -> SetCount -> fSorted := false
  end;
  if wasAdded <> nil then
    wasAdded^ := toInsert;
end;

type
  // internal structure used to make QuickSort faster & with less stack usage
  TDynArrayQuickSort = object
    Compare: TDynArraySortCompare;
    CompareEvent: TEventDynArraySortCompare;
    Pivot: pointer;
    index: PCardinalArray;
    ElemSize: cardinal;
    p: PtrInt;
    Value: PAnsiChar;
    IP, JP: PAnsiChar;
    procedure QuickSort(L, R: PtrInt);
    procedure QuickSortIndexed(L, R: PtrInt);
    procedure QuickSortEvent(L, R: PtrInt);
    procedure QuickSortEventReverse(L, R: PtrInt);
  end;

procedure QuickSortIndexedPUTF8Char(Values: PPUtf8CharArray; Count: Integer;
  var SortedIndexes: TCardinalDynArray; CaseSensitive: boolean);
var
  QS: TDynArrayQuickSort;
begin
  if CaseSensitive then
    QS.Compare := SortDynArrayPUTF8Char
  else
    QS.Compare := SortDynArrayPUTF8CharI;
  QS.Value := pointer(Values);
  QS.ElemSize := SizeOf(PUTF8Char);
  SetLength(SortedIndexes, Count);
  FillIncreasing(pointer(SortedIndexes), 0, Count);
  QS.Index := pointer(SortedIndexes);
  QS.QuickSortIndexed(0, Count - 1);
end;

procedure DynArraySortIndexed(Values: pointer; ElemSize, Count: Integer;
  out Indexes: TSynTempBuffer; Compare: TDynArraySortCompare);
var
  QS: TDynArrayQuickSort;
begin
  QS.Compare := Compare;
  QS.Value := Values;
  QS.ElemSize := ElemSize;
  QS.Index := pointer(Indexes.InitIncreasing(Count));
  QS.QuickSortIndexed(0, Count - 1);
end;

procedure TDynArrayQuickSort.QuickSort(L, R: PtrInt);
var
  I, J: PtrInt;
begin
  if L < R then
    repeat
      I := L;
      J := R;
      p := (L + R) shr 1;
      repeat
        Pivot := Value + PtrUInt(p) * ElemSize;
        IP := Value + PtrUInt(I) * ElemSize;
        JP := Value + PtrUInt(J) * ElemSize;
        while Compare(IP^, Pivot^) < 0 do
        begin
          inc(I);
          inc(IP, ElemSize);
        end;
        while Compare(JP^, Pivot^) > 0 do
        begin
          dec(J);
          dec(JP, ElemSize);
        end;
        if I <= J then
        begin
          if I <> J then
            Exchg(IP, JP, ElemSize);
          if p = I then
            p := J
          else if p = J then
            p := I;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if J - L < R - I then
      begin // use recursion only for smaller range
        if L < J then
          QuickSort(L, J);
        L := I;
      end
      else
      begin
        if I < R then
          QuickSort(I, R);
        R := J;
      end;
    until L >= R;
end;

procedure TDynArrayQuickSort.QuickSortEvent(L, R: PtrInt);
var
  I, J: PtrInt;
begin
  if L < R then
    repeat
      I := L;
      J := R;
      p := (L + R) shr 1;
      repeat
        Pivot := Value + PtrUInt(p) * ElemSize;
        IP := Value + PtrUInt(I) * ElemSize;
        JP := Value + PtrUInt(J) * ElemSize;
        while CompareEvent(IP^, Pivot^) < 0 do
        begin
          inc(I);
          inc(IP, ElemSize);
        end;
        while CompareEvent(JP^, Pivot^) > 0 do
        begin
          dec(J);
          dec(JP, ElemSize);
        end;
        if I <= J then
        begin
          if I <> J then
            Exchg(IP, JP, ElemSize);
          if p = I then
            p := J
          else if p = J then
            p := I;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if J - L < R - I then
      begin // use recursion only for smaller range
        if L < J then
          QuickSortEvent(L, J);
        L := I;
      end
      else
      begin
        if I < R then
          QuickSortEvent(I, R);
        R := J;
      end;
    until L >= R;
end;

procedure TDynArrayQuickSort.QuickSortEventReverse(L, R: PtrInt);
var
  I, J: PtrInt;
begin
  if L < R then
    repeat
      I := L;
      J := R;
      p := (L + R) shr 1;
      repeat
        Pivot := Value + PtrUInt(p) * ElemSize;
        IP := Value + PtrUInt(I) * ElemSize;
        JP := Value + PtrUInt(J) * ElemSize;
        while CompareEvent(IP^, Pivot^) > 0 do
        begin
          inc(I);
          inc(IP, ElemSize);
        end;
        while CompareEvent(JP^, Pivot^) < 0 do
        begin
          dec(J);
          dec(JP, ElemSize);
        end;
        if I <= J then
        begin
          if I <> J then
            Exchg(IP, JP, ElemSize);
          if p = I then
            p := J
          else if p = J then
            p := I;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if J - L < R - I then
      begin // use recursion only for smaller range
        if L < J then
          QuickSortEventReverse(L, J);
        L := I;
      end
      else
      begin
        if I < R then
          QuickSortEventReverse(I, R);
        R := J;
      end;
    until L >= R;
end;

procedure TDynArrayQuickSort.QuickSortIndexed(L, R: PtrInt);
var
  I, J: PtrInt;
  tmp: integer;
begin
  if L < R then
    repeat
      I := L;
      J := R;
      p := (L + R) shr 1;
      repeat
        Pivot := Value + index[p] * ElemSize;
        while Compare(Value[index[I] * ElemSize], Pivot^) < 0 do
          inc(I);
        while Compare(Value[index[J] * ElemSize], Pivot^) > 0 do
          dec(J);
        if I <= J then
        begin
          if I <> J then
          begin
            tmp := index[I];
            index[I] := index[J];
            index[J] := tmp;
          end;
          if p = I then
            p := J
          else if p = J then
            p := I;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if J - L < R - I then
      begin // use recursion only for smaller range
        if L < J then
          QuickSortIndexed(L, J);
        L := I;
      end
      else
      begin
        if I < R then
          QuickSortIndexed(I, R);
        R := J;
      end;
    until L >= R;
end;

procedure TDynArray.Sort(aCompare: TDynArraySortCompare);
begin
  SortRange(0, Count - 1, aCompare);
  fSorted := true;
end;

procedure QuickSortPtr(L, R: PtrInt; Compare: TDynArraySortCompare; V: PPointerArray);
var
  I, J, P: PtrInt;
  tmp: pointer;
begin
  if L < R then
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        while Compare(V[I], V[P]) < 0 do
          inc(I);
        while Compare(V[J], V[P]) > 0 do
          dec(J);
        if I <= J then
        begin
          tmp := V[I];
          V[I] := V[J];
          V[J] := tmp;
          if P = I then
            P := J
          else if P = J then
            P := I;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if J - L < R - I then
      begin // use recursion only for smaller range
        if L < J then
          QuickSortPtr(L, J, Compare, V);
        L := I;
      end
      else
      begin
        if I < R then
          QuickSortPtr(I, R, Compare, V);
        R := J;
      end;
    until L >= R;
end;

procedure TDynArray.SortRange(aStart, aStop: integer; aCompare: TDynArraySortCompare);
var
  QuickSort: TDynArrayQuickSort;
begin
  if aStop <= aStart then
    exit; // nothing to sort
  if Assigned(aCompare) then
    QuickSort.Compare := aCompare
  else
    QuickSort.Compare := @fCompare;
  if Assigned(QuickSort.Compare) and (fValue <> nil) and (fValue^ <> nil) then
    if ElemSize = SizeOf(pointer) then
      // dedicated function for pointers - e.g. T*ObjArray
      QuickSortPtr(aStart, aStop, QuickSort.Compare, fValue^)
    else
    begin
      // generic process for any size of array items
      QuickSort.Value := fValue^;
      QuickSort.ElemSize := ElemSize;
      QuickSort.QuickSort(aStart, aStop);
    end;
end;

procedure TDynArray.Sort(const aCompare: TEventDynArraySortCompare; aReverse: boolean);
var
  QuickSort: TDynArrayQuickSort;
  R: PtrInt;
begin
  if not Assigned(aCompare) or (fValue = nil) or (fValue^ = nil) then
    exit; // nothing to sort
  QuickSort.CompareEvent := aCompare;
  QuickSort.Value := fValue^;
  QuickSort.ElemSize := ElemSize;
  R := Count - 1;
  if aReverse then
    QuickSort.QuickSortEventReverse(0, R)
  else
    QuickSort.QuickSortEvent(0, R);
end;

procedure TDynArray.CreateOrderedIndex(var aIndex: TIntegerDynArray;
  aCompare: TDynArraySortCompare);
var
  QuickSort: TDynArrayQuickSort;
  n: integer;
begin
  if Assigned(aCompare) then
    QuickSort.Compare := aCompare
  else
    QuickSort.Compare := @fCompare;
  if Assigned(QuickSort.Compare) and (fValue <> nil) and (fValue^ <> nil) then
  begin
    n := GetCount;
    if length(aIndex) < n then
    begin
      SetLength(aIndex, n);
      FillIncreasing(pointer(aIndex), 0, n);
    end;
    QuickSort.Value := fValue^;
    QuickSort.ElemSize := ElemSize;
    QuickSort.Index := pointer(aIndex);
    QuickSort.QuickSortIndexed(0, n - 1);
  end;
end;

procedure TDynArray.CreateOrderedIndex(out aIndex: TSynTempBuffer;
  aCompare: TDynArraySortCompare);
var
  QuickSort: TDynArrayQuickSort;
  n: integer;
begin
  if Assigned(aCompare) then
    QuickSort.Compare := aCompare
  else
    QuickSort.Compare := @fCompare;
  if Assigned(QuickSort.Compare) and (fValue <> nil) and (fValue^ <> nil) then
  begin
    n := GetCount;
    QuickSort.Value := fValue^;
    QuickSort.ElemSize := ElemSize;
    QuickSort.Index := PCardinalArray(aIndex.InitIncreasing(n));
    QuickSort.QuickSortIndexed(0, n - 1);
  end
  else
    aIndex.buf := nil; // avoid GPF in aIndex.Done
end;

procedure TDynArray.CreateOrderedIndexAfterAdd(var aIndex: TIntegerDynArray;
  aCompare: TDynArraySortCompare);
var
  ndx: integer;
begin
  ndx := GetCount - 1;
  if ndx < 0 then
    exit;
  if aIndex <> nil then
  begin // whole FillIncreasing(aIndex[]) for first time
    if ndx >= length(aIndex) then
      SetLength(aIndex, NextGrow(ndx)); // grow aIndex[] if needed
    aIndex[ndx] := ndx;
  end;
  CreateOrderedIndex(aIndex, aCompare);
end;

procedure TDynArray.InitFrom(aAnother: PDynArray; var aValue);
begin
  self := aAnother^; // raw RTTI fields copy
  fValue := @aValue; // points to the new value
  fCountP := nil;
end;

procedure TDynArray.AddDynArray(aSource: PDynArray; aStartIndex: integer; aCount: integer);
var
  SourceCount: integer;
begin
  if (aSource <> nil) and (aSource^.fValue <> nil) and
     (fInfo.Cache.ItemInfo = aSource^.fInfo.Cache.ItemInfo) then
  begin
    SourceCount := aSource^.Count;
    if (aCount < 0) or (aCount > SourceCount) then
      aCount := SourceCount; // force use of external Source.Count, if any
    AddArray(aSource.fValue^, aStartIndex, aCount);
  end;
end;

function TDynArray.Equals(B: PDynArray; IgnoreCompare, CaseSensitive: boolean): boolean;
var
  i, n: integer;
  P1, P2: PAnsiChar;
begin
  result := false;
  n := GetCount;
  if (n <> B.Count) or (fInfo.Cache.ItemInfo <> B.fInfo.Cache.ItemInfo) then
    exit;
  if Assigned(fCompare) and not ignorecompare then
  begin // use customized comparison
    P1 := fValue^;
    P2 := B.fValue^;
    for i := 1 to n do
      if fCompare(P1^, P2^) <> 0 then
        exit
      else
      begin
        inc(P1, ElemSize);
        inc(P2, ElemSize);
      end;
    result := true;
  end
  else if not(rcfArrayItemManaged in fInfo.Flags) then
    result := CompareMem(fValue^, B.fValue^, ElemSize * cardinal(n))
  else
    result := DynArrayCompare(pointer(fValue), pointer(B.fValue),
      fCountP, B.fCountP, fInfo.Cache.ItemInfo, casesensitive) = 0;
end;

function TDynArray.Compares(B: PDynArray; IgnoreCompare, CaseSensitive: boolean): integer;
var
  i, n: integer;
  P1, P2: PAnsiChar;
begin
  n := GetCount;
  result := n - B.Count;
  if result <> 0 then
    exit;
  if fInfo.Cache.ItemInfo <> B.fInfo.Cache.ItemInfo then
  begin
    result := ComparePointer(fValue^, B.fValue^);
    exit;
  end;
  if Assigned(fCompare) and not ignorecompare then
  begin // use customized comparison
    P1 := fValue^;
    P2 := B.fValue^;
    for i := 1 to n do
    begin
      result := fCompare(P1^, P2^);
      if result <> 0 then
        exit;
      inc(P1, ElemSize);
      inc(P2, ElemSize);
    end;
  end
  else if not(rcfArrayItemManaged in fInfo.Flags) then
    result := StrCompL(fValue^, B.fValue^, ElemSize * cardinal(n))
  else
    result := DynArrayCompare(pointer(fValue), pointer(B.fValue),
      fCountP, B.fCountP, fInfo.Cache.ItemInfo, casesensitive);
end;

procedure TDynArray.Copy(Source: PDynArray; ObjArrayByRef: boolean);
begin
  if (fValue = nil) or (fInfo.Cache.ItemInfo <> Source.fInfo.Cache.ItemInfo) then
    exit;
  if not ObjArrayByRef and (rcfObjArray in fInfo.Flags) then
     LoadFromJSON(pointer(Source.SaveToJSON))
  else
  begin
    DynArrayCopy(fValue^, Source.fValue^, fInfo.Cache.ItemInfo, Source.fCountP);
    if fCountP <> nil then
      fCountP^ := GetCapacity;
  end;
end;

procedure TDynArray.CopyFrom(const Source; MaxItem: integer; ObjArrayByRef: boolean);
var
  SourceDynArray: TDynArray;
begin
  SourceDynArray.InitRtti(fInfo, pointer(@Source)^);
  SourceDynArray.fCountP := @MaxItem; // would set Count=0 at Init()
  Copy(@SourceDynArray, ObjArrayByRef);
end;

procedure TDynArray.CopyTo(out Dest; ObjArrayByRef: boolean);
var
  DestDynArray: TDynArray;
begin
  DestDynArray.InitRtti(fInfo, Dest);
  DestDynArray.Copy(@self, ObjArrayByRef);
end;

function TDynArray.IndexOf(const Item; CaseInSensitive: boolean): PtrInt;
var
  rtti: PRttiInfo;
  cmp: TRttiCompare;
  comp: integer;
  P: PAnsiChar;
begin
  if (fValue <> nil) and (@Item <> nil) then
    if not(rcfArrayItemManaged in fInfo.Flags) then
      result := AnyScanIndex(fValue^, @Item, GetCount, fElemSize)
    else
    begin
      rtti := fInfo.Cache.ItemInfo;
      cmp := RTTI_COMPARE[CaseInSensitive, rtti.Kind];
      if Assigned(cmp) then
      begin
        P := fValue^;
        for result := 0 to GetCount - 1 do
        begin
          inc(P, cmp(P, @Item, rtti, comp));
          if comp = 0 then
            exit;
        end;
      end;
      result := -1;
    end
  else
    result := -1;
end;

procedure TDynArray.UseExternalCount(var aCountPointer: Integer);
begin
  fCountP := @aCountPointer;
end;

procedure TDynArray.Void;
begin
  fValue := nil;
end;

function TDynArray.IsVoid: boolean;
begin
  result := fValue = nil;
end;

procedure TDynArray.InternalSetLength(OldLength, NewLength: PtrUInt);
var
  p: PDynArrayRec;
  NeededSize, minLength: PtrUInt;
begin // this method is faster than default System.DynArraySetLength() function
  p := fValue^;
  // check that new array length is not just a finalize in disguise
  if NewLength = 0 then
  begin
    if p <> nil then
    begin // FastDynArrayClear() with ObjArray support
      dec(p);
      if (p^.refCnt >= 0) and RefCntDecFree(p^.refCnt) then
      begin
        if OldLength <> 0 then
          if rcfArrayItemManaged in fInfo.Flags then
            FastFinalizeArray(fValue^, fInfo.Cache.ItemInfo, OldLength)
          else if rcfObjArray in fInfo.Flags then
            RawObjectsClear(fValue^, OldLength);
        FreeMem(p);
      end;
      fValue^ := nil;
    end;
    exit;
  end;
  // calculate the needed size of the resulting memory structure on heap
  NeededSize := NewLength * ElemSize + SizeOf(TDynArrayRec);
  {$ifndef CPU64}
  if NeededSize > 1 shl 30 then // max workable memory block is 1 GB
    raise EDynArray.CreateFmt('TDynArray SetLength(%s,%d) size concern',
      [fInfo.ArrayRtti.Name, NewLength]);
  {$endif CPU64}
  // if not shared (refCnt=1), resize; if shared, create copy (not thread safe)
  if p = nil then
  begin
    p := AllocMem(NeededSize); // RTL/OS will return zeroed memory
    OldLength := NewLength;    // no FillcharFast() below
  end
  else
  begin
    dec(PtrUInt(p), SizeOf(TDynArrayRec)); // p^ = start of heap object
    if (p^.refCnt >= 0) and RefCntDecFree(p^.refCnt) then
    begin
      // we own the dynamic array instance -> direct reallocation
      if NewLength < OldLength then // reduce array in-place
        if rcfArrayItemManaged in fInfo.Flags then // in trailing items
          FastFinalizeArray(pointer(PAnsiChar(p) + NeededSize),
            fInfo.Cache.ItemInfo, OldLength - NewLength)
        else if rcfObjArray in fInfo.Flags then // FreeAndNil() of resized objects list
          RawObjectsClear(pointer(PAnsiChar(p) + NeededSize), OldLength - NewLength);
      ReallocMem(p, NeededSize);
    end
    else
    begin
      // dynamic array already referenced elsewhere -> create copy
      GetMem(p, NeededSize);
      minLength := OldLength;
      if minLength > NewLength then
        minLength := NewLength;
      CopySeveral(@PByteArray(p)[SizeOf(TDynArrayRec)], fValue^,
        minLength, fInfo.Cache.ItemInfo, ElemSize);
    end;
  end;
  // set refCnt=1 and new length to the heap header
  with p^ do
  begin
    refCnt := 1;
    length := NewLength;
  end;
  inc(p); // start of dynamic aray items
  fValue^ := p;
  // reset new allocated items content to zero
  if NewLength > OldLength then
  begin
    OldLength := OldLength * ElemSize;
    FillCharFast(PAnsiChar(p)[OldLength], NewLength * ElemSize - OldLength, 0);
  end;
end;

procedure TDynArray.SetCount(aCount: PtrInt);
const
  MINIMUM_SIZE = 64;
var
  oldlen, extcount, arrayptr, capa, delta: PtrInt;
begin
  arrayptr := PtrInt(fValue);
  extcount := PtrInt(fCountP);
  fSorted := false;
  if arrayptr = 0 then
    exit; // avoid GPF if void
  arrayptr := PPtrInt(arrayptr)^;
  if extcount <> 0 then
  begin
    // fCountP^ as external capacity
    oldlen := PInteger(extcount)^;
    delta := aCount - oldlen;
    if delta = 0 then
      exit;
    PInteger(extcount)^ := aCount; // store new length
    if arrayptr = 0 then
    begin
      // void array
      if (delta > 0) and (aCount < MINIMUM_SIZE) then
        // reserve some minimal (64) items for Add()
        aCount := MINIMUM_SIZE;
    end
    else
    begin
      // non void array: check new count with existing capacity
      capa := PDALen(arrayptr - _DALEN)^ + _DAOFF;
      if delta > 0 then
      begin
        // size-up
        if capa >= aCount then
          exit; // no need to grow
        capa := NextGrow(capa);
        if capa > aCount then
          aCount := capa; // grow by chunks
      end
      else
      // size-down
      if (aCount > 0) and
         ((capa <= MINIMUM_SIZE) or (capa - aCount < capa shr 3)) then
        // reallocate memory only if worth it (for faster Delete)
        exit;
    end;
  end
  else
    // no external capacity: use length()
    if arrayptr = 0 then
      oldlen := arrayptr
    else
    begin
      oldlen := PDALen(arrayptr - _DALEN)^ + _DAOFF;
      if oldlen = aCount then
        exit; // InternalSetLength(samecount) would have made a private copy
    end;
  // no external Count, array size-down or array up-grow -> realloc
  InternalSetLength(oldlen, aCount);
end;

function TDynArray.GetCapacity: PtrInt;
begin
  result := PtrInt(fValue);
  if result <> 0 then
  begin
    result := PPtrInt(result)^;
    if result <> 0 then
      result := PDALen(result - _DALEN)^ + _DAOFF; // capacity = length()
  end;
end;

procedure TDynArray.SetCapacity(aCapacity: PtrInt);
var
  oldlen, capa: PtrInt;
begin
  if fValue = nil then
    exit;
  capa := GetCapacity;
  if fCountP <> nil then
  begin
    oldlen := fCountP^;
    if oldlen > aCapacity then
      fCountP^ := aCapacity;
  end
  else
    oldlen := capa;
  if capa <> aCapacity then
    InternalSetLength(oldlen, aCapacity);
end;

procedure TDynArray.SetCompare(const aCompare: TDynArraySortCompare);
begin
  if @aCompare <> @fCompare then
  begin
    @fCompare := @aCompare;
    fSorted := false;
  end;
end;

procedure TDynArray.Slice(var Dest; aCount, aFirstIndex: cardinal);
var
  n: Cardinal;
  dst: TDynArray;
begin
  if fValue = nil then
    exit; // avoid GPF if void
  n := GetCount;
  if aFirstIndex >= n then
    aCount := 0
  else if aCount >= n - aFirstIndex then
    aCount := n - aFirstIndex;
  dst.InitRtti(fInfo, Dest);
  dst.SetCapacity(aCount);
  CopySeveral(PPointer(Dest)^, @(PByteArray(fValue^)[aFirstIndex * ElemSize]),
    aCount, fInfo.Cache.ItemInfo, ElemSize);
end;

function TDynArray.AddArray(const DynArrayVar; aStartIndex, aCount: integer): integer;
var
  c: PtrInt;
  n: integer;
  PS, PD: pointer;
begin
  result := 0;
  if fValue = nil then
    exit; // avoid GPF if void
  c := PtrInt(DynArrayVar);
  if c <> 0 then
    c := PDALen(c - _DALEN)^ + _DAOFF;
  if aStartIndex >= c then
    exit; // nothing to copy
  if (aCount < 0) or (cardinal(aStartIndex + aCount) > cardinal(c)) then
    aCount := c - aStartIndex;
  if aCount <= 0 then
    exit;
  result := aCount;
  n := GetCount;
  SetCount(n + aCount);
  PS := PAnsiChar(DynArrayVar) + cardinal(aStartIndex) * ElemSize;
  PD := PAnsiChar(fValue^) + cardinal(n) * ElemSize;
  CopySeveral(PD, PS, aCount, fInfo.Cache.ItemInfo, ElemSize);
end;

function TDynArray.ItemLoad(Source: PAnsiChar; SourceMax: PAnsiChar): RawByteString;
begin
  if (Source <> nil) and (fInfo.Cache.ItemInfo = nil) then
    SetString(result, Source, ElemSize)
  else
  begin
    SetString(result, nil, ElemSize);
    FillCharFast(pointer(result)^, ElemSize, 0);
    ItemLoad(Source, pointer(result), SourceMax);
  end;
end;

procedure TDynArray.ItemLoadClear(var ItemTemp: RawByteString);
begin
  ItemClear(pointer(ItemTemp));
  ItemTemp := '';
end;

procedure TDynArray.ItemLoad(Source: PAnsiChar; Item: pointer;
  SourceMax: PAnsiChar);
begin
  if Source <> nil then // avoid GPF
    if fInfo.Cache.ItemInfo = nil then
    begin
      if (SourceMax = nil) or (Source + ElemSize <= SourceMax) then
        MoveFast(Source^, Item^, ElemSize);
    end
    else
      BinaryLoad(Item, Source, fInfo.Cache.ItemInfo, nil, SourceMax, rkAllTypes);
end;

function TDynArray.ItemSave(Item: pointer): RawByteString;
begin
  if fInfo.Cache.ItemInfo = nil then
    SetString(result, PAnsiChar(Item), ElemSize)
  else
    result := BinarySave(Item, fInfo.Cache.ItemInfo, rkAllTypes);
end;

function TDynArray.ItemLoadFind(Source, SourceMax: PAnsiChar): integer;
var
  tmp: array[0..2047] of byte;
  data: pointer;
begin
  result := -1;
  if (Source = nil) or (ElemSize > SizeOf(tmp)) then
    exit;
  if fInfo.Cache.ItemInfo = nil then
    data := Source
  else
  begin
    FillCharFast(tmp, ElemSize, 0);
    BinaryLoad(@tmp, Source, fInfo.Cache.ItemInfo, nil, SourceMax, rkAllTypes);
    if Source = nil then
      exit;
    data := @tmp;
  end;
  try
    if Assigned(fCompare) then
      result := Find(data^) // use specific comparer
    else
      result := IndexOf(data^); // use RTTI
  finally
    if data = @tmp then
      fInfo.ArrayRtti.ValueFinalize(data);
  end;
end;


{ ************ TDynArrayHasher }

function HashAnsiString(Item: PAnsiChar; Hasher: THasher): cardinal;
begin
  Item := PPointer(Item)^; // passed by reference
  if Item = nil then
    result := 0
  else
    result := Hasher(0, Item, PStrLen(Item - _STRLEN)^);
end;

function HashAnsiStringI(Item: PUTF8Char; Hasher: THasher): cardinal;
var
  tmp: array[byte] of AnsiChar; // avoid slow heap allocation
begin
  Item := PPointer(Item)^;
  if Item = nil then
    result := 0
  else
    result := Hasher(0, tmp{%H-},
      UpperCopy255Buf(tmp{%H-}, Item, PStrLen(Item - _STRLEN)^) - {%H-}tmp);
end;

function HashSynUnicode(const Item: SynUnicode; Hasher: THasher): cardinal;
begin
  if PtrUInt(Item) = 0 then
    result := 0
  else
    result := Hasher(0, Pointer(Item), length(Item) * 2);
end;

function HashSynUnicodeI(const Item: SynUnicode; Hasher: THasher): cardinal;
var
  tmp: array[byte] of AnsiChar; // avoid slow heap allocation
begin
  if PtrUInt(Item) = 0 then
    result := 0
  else
    result := Hasher(0, tmp{%H-}, UpperCopy255W(tmp{%H-}, Item) - {%H-}tmp);
end;

function HashWideString(const Item: WideString; Hasher: THasher): cardinal;
begin // WideString internal size is in bytes, not WideChar
  if PtrUInt(Item) = 0 then
    result := 0
  else
    result := Hasher(0, Pointer(Item), Length(Item) * 2);
end;

function HashWideStringI(const Item: WideString; Hasher: THasher): cardinal;
var
  tmp: array[byte] of AnsiChar; // avoid slow heap allocation
begin
  if PtrUInt(Item) = 0 then
    result := 0
  else
    result := Hasher(0, tmp{%H-},
      UpperCopy255W(tmp{%H-}, pointer(Item), Length(Item)) - {%H-}tmp);
end;

function HashPtrUInt(Item: pointer; Hasher: THasher): cardinal;
begin
  result := Hasher(0, Item, SizeOf(PtrUInt));
end;

function HashPointer(Item: pointer; Hasher: THasher): cardinal;
begin
  result := Hasher(0, Item, SizeOf(pointer));
end;

function HashByte(Item: pointer; Hasher: THasher): cardinal;
begin
  result := Hasher(0, Item, SizeOf(byte));
end;

function HashWord(Item: pointer; Hasher: THasher): cardinal;
begin
  result := Hasher(0, Item, SizeOf(word));
end;

function HashInteger(Item: pointer; Hasher: THasher): cardinal;
begin
  result := Hasher(0, Item, SizeOf(integer));
end;

function HashInt64(Item: pointer; Hasher: THasher): cardinal;
begin
  result := Hasher(0, Item, SizeOf(Int64));
end;

function HashExtended(Item: pointer; Hasher: THasher): cardinal;
begin
  result := Hasher(0, Item, SizeOf(TSynExtended));
end;

function Hash128(Item: pointer; Hasher: THasher): cardinal;
begin
  result := Hasher(0, Item, SizeOf(THash128));
end;

function Hash256(Item: pointer; Hasher: THasher): cardinal;
begin
  result := Hasher(0, Item, SizeOf(THash256));
end;

function Hash512(Item: pointer; Hasher: THasher): cardinal;
begin
  result := Hasher(0, Item, SizeOf(THash512));
end;

function VariantHash(const value: variant; CaseInsensitive: boolean;
  Hasher: THasher): cardinal;
var
  tmp: array[byte] of AnsiChar; // avoid heap allocation
  vt: cardinal;
  S: TStream;
  W: TBaseWriter;
  P: pointer;
  len: integer;
begin
  if not Assigned(Hasher) then
    Hasher := DefaultHasher;
  with TVarData(value) do
  begin
    vt := VType;
    P := @VByte;
    case vt of
      varNull, varEmpty:
        len := 0; // good enough for void values
      varShortInt, varByte:
        len := 1;
      varSmallint, varWord, varboolean:
        len := 2;
      varLongWord, varInteger, varSingle:
        len := 4;
      varInt64, varDouble, varDate, varCurrency, varWord64:
        len := 8;
      varString:
        begin
          len := length(RawUTF8(VAny));
          P := VAny;
        end;
      varOleStr:
        begin
          len := length(WideString(VAny));
          P := VAny;
        end;
      {$ifdef HASVARUSTRING}
      varUString:
        begin
          len := length(UnicodeString(VAny));
          P := VAny;
        end;
      {$endif}
      else
      begin
        S := TFakeWriterStream.Create;
        W := DefaultTextWriterSerializer.Create(S, @tmp, SizeOf(tmp));
        try
          W.AddVariant(value, twJSONEscape);
          len := W.WrittenBytes;
          if len > 255 then
            len := 255;
          P := @tmp; // big JSON won't be hasheable anyway -> use only buffer
        finally
          W.Free;
          S.Free;
        end;
      end;
    end;
    if CaseInsensitive then
    begin
      len := UpperCopy255Buf(tmp, P, len) - tmp;
      P := @tmp;
    end;
    result := Hasher(vt, P, len)
  end;
end;

function HashVariant(Item: PVariant; Hasher: THasher): cardinal;
begin
  result := VariantHash(Item^, false, Hasher);
end;

function HashVariantI(Item: PVariant; Hasher: THasher): cardinal;
begin
  result := VariantHash(Item^, true, Hasher);
end;

const
  _PT_HASH: array[{caseinsensitive=}boolean, TRttiParserType] of pointer = (
   (nil, nil, @HashByte, @HashByte, @HashInteger, @HashInt64, @HashInt64,
    @HashExtended, @HashInt64, @HashInteger, @HashInt64, @HashAnsiString,
    @HashAnsiString, @HashAnsiString, nil, @HashInteger,
    {$ifdef UNICODE} @HashSynUnicode {$else} @HashAnsiString {$endif},
    @HashSynUnicode, @HashInt64, @HashInt64, @Hash128, @Hash128, @Hash256, @Hash512,
    @HashInt64, @HashInt64, @HashSynUnicode, @HashInt64, @HashInt64,
    @HashVariant, @HashWideString, @HashAnsiString, @HashWord,
    nil, nil, nil, nil, nil, nil),
   (nil, nil, @HashByte, @HashByte, @HashInteger, @HashInt64, @HashInt64,
    @HashExtended, @HashInt64, @HashInteger, @HashInt64, @HashAnsiString,
    @HashAnsiStringI, @HashAnsiString, nil, @HashInteger,
    {$ifdef UNICODE} @HashSynUnicodeI {$else} @HashAnsiStringI {$endif},
    @HashSynUnicodeI, @HashInt64, @HashInt64, @Hash128, @Hash128, @Hash256, @Hash512,
    @HashInt64, @HashInt64, @HashSynUnicodeI, @HashInt64, @HashInt64,
    @HashVariantI, @HashWideStringI, @HashAnsiStringI, @HashWord,
    nil, nil, nil, nil, nil, nil));

procedure TDynArrayHasher.Init(aDynArray: PDynArray; aHashItem: TDynArrayHashOne;
  aEventHash: TEventDynArrayHashOne; aHasher: THasher;
  aCompare: TDynArraySortCompare; aEventCompare: TEventDynArraySortCompare;
  aCaseInsensitive: boolean);
begin
  DynArray := aDynArray;
  if @aHasher = nil then
    Hasher := DefaultHasher
  else
    Hasher := aHasher;
  HashItem := aHashItem;
  EventHash := aEventHash;
  if (@HashItem = nil) and (@EventHash = nil) then
    HashItem := _PT_HASH[aCaseInsensitive, DynArray^.Info.ArrayFirstField];
  Compare := aCompare;
  EventCompare := aEventCompare;
  if (@Compare = nil) and (@EventCompare = nil) then
    Compare := PT_SORT[aCaseInsensitive, DynArray^.Info.ArrayFirstField];
  CountTrigger := 32;
  Clear;
end;

procedure TDynArrayHasher.InitSpecific(aDynArray: PDynArray; aKind: TRttiParserType;
  aCaseInsensitive: boolean; aHasher: THasher);
var
  cmp: TDynArraySortCompare;
  hsh: TDynArrayHashOne;
begin
  cmp := PT_SORT[aCaseInsensitive, aKind];
  hsh := _PT_HASH[aCaseInsensitive, aKind];
  if (@hsh = nil) or (@cmp = nil) then
    raise EDynArray.CreateUTF8('TDynArrayHasher.InitSpecific: %?', [ToText(aKind)^]);
  Init(aDynArray, hsh, nil, aHasher, cmp, nil, aCaseInsensitive)
end;

procedure TDynArrayHasher.Clear;
begin
  HashTable := nil;
  HashTableSize := 0;
  ScanCounter := 0;
  if Assigned(HashItem) or Assigned(EventHash) then
    State := [hasHasher]
  else
    byte(State) := 0;
end;

function TDynArrayHasher.HashOne(Item: pointer): cardinal;
begin
  if Assigned(EventHash) then
    result := EventHash(Item^)
  else if Assigned(HashItem) then
    result := HashItem(Item^, Hasher)
  else
    result := 0; // will be ignored afterwards for sure
end;

const
  // reduces memory consumption and enhances distribution at hash table growing
  _PRIMES: array[0..38 {$ifndef CPU32DELPHI} + 15 {$endif}] of integer = (
    {$ifndef CPU32DELPHI}
    31, 127, 251, 499, 797, 1259, 2011, 3203, 5087,
    8089, 12853, 20399, 81649, 129607, 205759,
    {$endif CPU32DELPHI}
    // start after HASH_PO2=2^18=262144 for Delphi Win32 (poor 64-bit mul)
    326617, 411527, 518509, 653267, 823117, 1037059, 1306601, 1646237,
    2074129, 2613229, 3292489, 4148279, 5226491, 6584983, 8296553, 10453007,
    13169977, 16593127, 20906033, 26339969, 33186281, 41812097, 52679969,
    66372617, 83624237, 105359939, 132745199, 167248483, 210719881, 265490441,
    334496971, 421439783, 530980861, 668993977, 842879579, 1061961721,
    1337987929, 1685759167, 2123923447);

// as used internally by TDynArrayHasher.ReHash()
function NextPrime(v: integer): integer; {$ifdef HASINLINE} inline;{$endif}
var
  i: PtrInt;
  P: PIntegerArray;
begin
  P := @_PRIMES;
  for i := 0 to high(_PRIMES) do
  begin
    result := P^[i];
    if result > v then
      exit;
  end;
end;

function TDynArrayHasher.HashTableIndex(aHashCode: PtrUInt): PtrUInt;
begin
  result := HashTableSize;
  {$ifdef CPU32DELPHI}
  // Delphi Win32 is not efficient with 64-bit multiplication
  if result > HASH_PO2 then
    result := aHashCode mod result
  else
    result := aHashCode and (result - 1);
  {$else}
  // FPC or dcc64 compile next line as very optimized asm
  result := (QWord(aHashCode) * result) shr 32;
  // see https://lemire.me/blog/2016/06/27/a-fast-alternative-to-the-modulo-reduction
  {$endif CPU32DELPHI}
end;

function TDynArrayHasher.Find(aHashCode: cardinal; aForAdd: boolean): integer;
var
  first, last: integer;
  ndx, siz: PtrInt;
  P: PAnsiChar;
begin
  P := DynArray^.Value^;
  siz := DynArray^.ElemSize;
  if not (canHash in State) then
  begin
    // Count=0 or Count<CountTrigger
    if hasHasher in State then
      // O(n) linear search via hashing
      for result := 0 to DynArray^.Count - 1 do
        if HashOne(P) = aHashCode then
          exit
        else
          inc(P, siz);
    result := -1;
    exit;
  end;
  result := HashTableIndex(aHashCode);
  first := result;
  last := HashTableSize;
  repeat
    ndx := HashTable[result] - 1; // index+1 was stored
    if ndx < 0 then
    begin
      // found void entry
      result := -(result + 1);
      exit;
    end
    else if not aForAdd and (HashOne(P + ndx * siz) = aHashCode) then
    begin
      result := ndx;
      exit;
    end;
    inc(result); // try next entry on hash collision
    if result = last then
      // reached the end -> search once from HashTable[0] to HashTable[first-1]
      if result = first then
        break
      else
      begin
        result := 0;
        last := first;
      end;
  until false;
  RaiseFatalCollision('Find', aHashCode);
end;

function TDynArrayHasher.FindOrNew(aHashCode: cardinal; Item: pointer;
  aHashTableIndex: PInteger): integer;
var
  first, last, ndx, cmp: integer;
  P: PAnsiChar;
begin
  if not (canHash in State) then
  begin // e.g. Count<CountTrigger
    result := Scan(Item);
    exit;
  end;
  result := HashTableIndex(aHashCode);
  first := result;
  last := HashTableSize;
  repeat
    ndx := HashTable[result] - 1;  // index+1 was stored
    if ndx < 0 then
    begin
      result := -(result + 1);
      exit; // returns void index in HashTable[]
    end;
    with DynArray^ do
      P := PAnsiChar(Value^) + cardinal(ndx) * ElemSize;
    if Assigned(EventCompare) then
      cmp := EventCompare(P^, Item^)
    else if Assigned(Compare) then
      cmp := Compare(P^, Item^)
    else
      cmp := 1;
    if cmp = 0 then
    begin
      // faster than hash e.g. for huge strings
      if aHashTableIndex <> nil then
        aHashTableIndex^ := result;
      result := ndx;
      exit;
    end;
    // hash or slot collision -> search next item
    {$ifdef DYNARRAYHASHCOLLISIONCOUNT}
    inc(FindCollisions);
    {$endif DYNARRAYHASHCOLLISIONCOUNT}
    inc(result);
    if result = last then
      // reached the end -> search once from HashTable[0] to HashTable[first-1]
      if result = first then
        break
      else
      begin
        result := 0;
        last := first;
      end;
  until false;
  RaiseFatalCollision('FindOrNew', aHashCode);
end;

procedure TDynArrayHasher.HashAdd(aHashCode: cardinal; var result: integer);
var
  n: integer;
begin // on input: HashTable[result] slot is already computed
  n := DynArray^.Count;
  if HashTableSize < n then
    RaiseFatalCollision('HashAdd HashTableSize', aHashCode);
  if HashTableSize - n < n shr 2 then
  begin
    // grow hash table when 25% void
    ReHash({forced=}true, {grow=}true);
    result := Find(aHashCode, {foradd=}true); // recompute position
    if result >= 0 then
      RaiseFatalCollision('HashAdd', aHashCode);
  end;
  HashTable[-result - 1] := n + 1; // store Index+1 (0 means void slot)
  result := n;
end; // on output: result holds the position in fValue[]


procedure TDynArrayHasher.HashDelete(aArrayIndex, aHashTableIndex: integer;
  aHashCode: cardinal);
var
  first, next, last, ndx, i, n: PtrInt;
  P: PAnsiChar;
  indexes: array[0..511] of cardinal; // to be rehashed  (seen always < 32)
begin
  // retrieve hash table entries to be recomputed
  first := aHashTableIndex;
  last := HashTableSize;
  next := first;
  n := 0;
  repeat
    HashTable[next] := 0; // Clear slots
    inc(next);
    if next = last then
      if next = first then
        RaiseFatalCollision('HashDelete down', aHashCode)
      else
      begin
        next := 0;
        last := first;
      end;
    ndx := HashTable[next] - 1; // stored index+1
    if ndx < 0 then
      break; // stop at void entry
    if n = high(indexes) then // paranoid (typical 0..23 range)
      RaiseFatalCollision('HashDelete indexes overflow', aHashCode);
    indexes[n] := ndx;
    inc(n);
  until false;
  // ReHash collided entries - note: item is not yet deleted in Value^[]
  for i := 0 to n - 1 do
  begin
    P := PAnsiChar(DynArray^.Value^) + {%H-}indexes[i] * DynArray^.ElemSize;
    ndx := FindOrNew(HashOne(P), P, nil);
    if ndx < 0 then
      HashTable[-ndx - 1] := indexes[i] + 1; // ignore ndx>=0 dups (like ReHash)
  end;
  // adjust all stored indexes (using SSE2/AVX2 on x86_64)
  DynArrayHashTableAdjust(pointer(HashTable), aArrayIndex, HashTableSize);
end;

function TDynArrayHasher.FindBeforeAdd(Item: pointer; out wasAdded: boolean;
  aHashCode: cardinal): integer;
var
  n: integer;
begin
  wasAdded := false;
  if not (canHash in State) then
  begin
    n := DynArray^.count;
    if n < CountTrigger then
    begin
      result := Scan(Item); // may trigger ReHash and set canHash
      if result >= 0 then
        exit; // item found
      if not (canHash in State) then
      begin
        wasAdded := true;
        result := n;
        exit;
      end;
    end;
  end;
  if not (canHash in State) then
    ReHash({forced=}true, {grow=}false); // hash previous CountTrigger items
  result := FindOrNew(aHashCode, Item, nil);
  if result < 0 then
  begin // found no matching item
    wasAdded := true;
    HashAdd(aHashCode, result);
  end;
end;

function TDynArrayHasher.FindBeforeDelete(Item: pointer): integer;
var
  hc: cardinal;
  ht: integer;
begin
  if canHash in State then
  begin
    hc := HashOne(Item);
    result := FindOrNew(hc, Item, @ht);
    if result < 0 then
      result := -1
    else
      HashDelete(result, ht, hc);
  end
  else
    result := Scan(Item);
end;

procedure TDynArrayHasher.RaiseFatalCollision(const caller: RawUTF8;
  aHashCode: cardinal);
begin // a dedicated sub-procedure reduces code size
  raise EDynArray.CreateUTF8('TDynArrayHasher.% fatal collision: ' +
    'aHashCode=% HashTableSize=% Count=% Capacity=% Array=% Parser=%',
    [caller, CardinalToHexShort(aHashCode), HashTableSize, DynArray^.Count,
     DynArray^.Capacity, DynArray^.Info.Name, ToText(DynArray^.Info.Parser)^]);
end;

function TDynArrayHasher.GetHashFromIndex(aIndex: PtrInt): cardinal;
var
  P: pointer;
begin
  P := DynArray^.ItemPtr(aIndex);
  if P <> nil then
    result := HashOne(P)
  else
    result := 0;
end;

procedure TDynArrayHasher.SetEventHash(const event: TEventDynArrayHashOne);
begin
  EventHash := event;
  Clear;
end;

function TDynArrayHasher.Scan(Item: pointer): integer;
var
  P: PAnsiChar;
  i, max: integer;
  siz: PtrInt;
begin
  result := -1;
  max := DynArray^.count - 1;
  P := DynArray^.Value^;
  siz := DynArray^.ElemSize;
  if Assigned(EventCompare) then // custom comparison
    for i := 0 to max do
      if EventCompare(P^, Item^) = 0 then
      begin
        result := i;
        break;
      end
      else
        inc(P, siz)
  else if Assigned(Compare) then
    for i := 0 to max do
      if Compare(P^, Item^) = 0 then
      begin
        result := i;
        break;
      end
      else
        inc(P, siz);
  // enable hashing if Scan() called 2*CountTrigger
  if hasHasher in State then
    if max > CountTrigger then
      // e.g. after Init() without explicit ReHash
      ReHash({forced=}false, {grow=}false) // set HashTable[] and canHash
  else if max > 7 then      
  begin
    inc(ScanCounter);
    if ScanCounter >= CountTrigger * 2 then
    begin
      CountTrigger := 2; // rather use hashing from now on
      ReHash({forced=}false, {grow=}false);
    end;
  end;
end;

function TDynArrayHasher.Find(Item: pointer): integer;
begin
  result := Find(Item, HashOne(Item));
end;

function TDynArrayHasher.Find(Item: pointer; aHashCode: cardinal): integer;
begin
  result := FindOrNew(aHashCode, Item, nil); // fallback to Scan() if needed
  if result < 0 then
    result := -1; // for coherency with most search methods
end;

function TDynArrayHasher.ReHash(forced, forceGrow: boolean): integer;
var
  i, n, cap, siz, ndx: integer;
  P: PAnsiChar;
  hc: cardinal;
begin
  result := 0;
  // initialize a new void HashTable[]=0
  siz := HashTableSize;
  Clear;
  if not (hasHasher in State) then
    exit;
  n := DynArray^.count;
  if not forced and ((n = 0) or (n < CountTrigger)) then
    exit; // hash only if needed, and avoid GPF after TDynArray.Clear (Count=0)
  if forceGrow and (siz > 0) then
    // next power of two or next prime
    {$ifdef CPU32DELPHI}
    if siz < HASH_PO2 then
      siz := siz shl 1
    else
    {$endif CPU32DELPHI}
      siz := NextPrime(siz)
  else
  begin
    cap := DynArray^.Capacity * 2; // Capacity better than Count - *2 for void slots
    {$ifdef CPU32DELPHI}
    if cap <= HASH_PO2 then
    begin
      siz := 256; // find nearest power of two for fast bitwise division
      while siz < cap do
        siz := siz shl 1;
    end
    else
    {$endif CPU32DELPHI}
      siz := NextPrime(cap);
  end;
  HashTableSize := siz;
  SetLength(HashTable, siz); // fill with 0 (void slot)
  // fill HashTable[]=index+1 from all existing items
  include(State, canHash);   // needed before Find() below
  P := DynArray^.Value^;
  for i := 1 to n do
  begin
    if Assigned(EventHash) then
      hc := EventHash(P^)
    else
      hc := HashItem(P^, Hasher);
    ndx := FindOrNew(hc, P, nil);
    if ndx >= 0 then
      inc(result)
    else         // found duplicated value
      HashTable[-ndx - 1] := i;  // store index+1 (0 means void entry)
    inc(P, DynArray^.ElemSize);
  end;
end;

{ ************ TDynArrayHashed }

{ TDynArrayHashed }

{$ifdef UNDIRECTDYNARRAY} // some Delphi 2009+ wrapper definitions

function TDynArrayHashed.GetCount: PtrInt;
begin
  result := InternalDynArray.GetCount;
end;

procedure TDynArrayHashed.SetCount(aCount: PtrInt);
begin
  InternalDynArray.SetCount(aCount);
end;

function TDynArrayHashed.GetCapacity: PtrInt;
begin
  result := InternalDynArray.GetCapacity;
end;

procedure TDynArrayHashed.SetCapacity(aCapacity: PtrInt);
begin
  InternalDynArray.SetCapacity(aCapacity);
end;

function TDynArrayHashed.Value: PPointer;
begin
  result := InternalDynArray.fValue;
end;

function TDynArrayHashed.Info: TRttiCustom;
begin
  result := InternalDynArray.fInfo;
end;

function TDynArrayHashed.ElemSize: PtrUInt;
begin
  result := InternalDynArray.fElemSize;
end;

procedure TDynArrayHashed.ItemCopy(Source, Dest: pointer);
begin
  InternalDynArray.ItemCopy(Source, Dest);
end;

function TDynArrayHashed.ItemPtr(index: PtrInt): pointer;
begin
  result := InternalDynArray.ItemPtr(index);
end;

procedure TDynArrayHashed.ItemCopyAt(index: PtrInt; Dest: pointer);
begin
  InternalDynArray.ItemCopyAt(index, Dest);
end;

procedure TDynArrayHashed.Clear;
begin
  InternalDynArray.SetCount(0);
end;

function TDynArrayHashed.Add(const Item): integer;
begin
  result := InternalDynArray.Add(Item);
end;

procedure TDynArrayHashed.Delete(aIndex: PtrInt);
begin
  InternalDynArray.Delete(aIndex);
end;

function TDynArrayHashed.SaveTo: RawByteString;
begin
  result := InternalDynArray.SaveTo;
end;

function TDynArrayHashed.LoadFrom(Source, SourceMax: PAnsiChar): PAnsiChar;
begin
  result := InternalDynArray.LoadFrom(Source, SourceMax);
end;

function TDynArrayHashed.LoadFromBinary(const Buffer: RawByteString): boolean;
begin
  result := InternalDynArray.LoadFromBinary(Buffer);
end;

procedure TDynArrayHashed.SaveTo(W: TBufferWriter);
begin
  InternalDynArray.SaveTo(W);
end;

procedure TDynArrayHashed.Sort(aCompare: TDynArraySortCompare);
begin
  InternalDynArray.Sort(aCompare);
end;

procedure TDynArrayHashed.CreateOrderedIndex(var aIndex: TIntegerDynArray;
  aCompare: TDynArraySortCompare);
begin
  InternalDynArray.CreateOrderedIndex(aIndex, aCompare);
end;

function TDynArrayHashed.SaveToJSON(EnumSetsAsText: boolean;
  reformat: TTextWriterJSONFormat): RawUTF8;
begin
  result := InternalDynArray.SaveToJSON(EnumSetsAsText, reformat);
end;

procedure TDynArrayHashed.SaveToJSON(out result: RawUTF8; EnumSetsAsText: boolean;
  reformat: TTextWriterJSONFormat);
begin
  InternalDynArray.SaveToJSON(result, EnumSetsAsText, reformat);
end;

procedure TDynArrayHashed.SaveToJSON(W: TBaseWriter);
begin
  InternalDynArray.SaveToJSON(W);
end;

function TDynArrayHashed.LoadFromJSON(P: PUTF8Char; aEndOfObject: PUTF8Char;
  CustomVariantOptions: PDocVariantOptions): PUTF8Char;
begin
  result := InternalDynArray.LoadFromJSON(P, aEndOfObject, CustomVariantOptions);
end;

{$endif UNDIRECTDYNARRAY}

procedure TDynArrayHashed.Init(aTypeInfo: pointer; var aValue;
  aHashItem: TDynArrayHashOne; aCompare: TDynArraySortCompare;
  aHasher: THasher; aCountPointer: PInteger; aCaseInsensitive: boolean);
begin
  {$ifdef UNDIRECTDYNARRAY}InternalDynArray.{$else}inherited{$endif}
    Init(aTypeInfo, aValue, aCountPointer);
  fHash.Init(@self, aHashItem, nil, aHasher, aCompare, nil, aCaseInsensitive);
  {$ifdef UNDIRECTDYNARRAY}InternalDynArray.{$endif}SetCompare(fHash.Compare);
end;

procedure TDynArrayHashed.InitSpecific(aTypeInfo: pointer; var aValue;
  aKind: TRttiParserType; aCountPointer: PInteger; aCaseInsensitive: boolean;
  aHasher: THasher);
begin
  {$ifdef UNDIRECTDYNARRAY}InternalDynArray.{$else}inherited{$endif}
    Init(aTypeInfo, aValue, aCountPointer);
  fHash.InitSpecific(@self, aKind, aCaseInsensitive, aHasher);
  {$ifdef UNDIRECTDYNARRAY} with InternalDynArray do {$endif}
    fCompare := fHash.Compare;
end;

function TDynArrayHashed.Scan(const Item): integer;
begin
  result := fHash.Scan(@Item);
end;

function TDynArrayHashed.FindHashed(const Item): integer;
begin
  result := fHash.FindOrNew(fHash.HashOne(@Item), @Item);
  if result < 0 then
    result := -1; // for coherency with most methods
end;

function TDynArrayHashed.FindFromHash(const Item; aHashCode: cardinal): integer;
begin // overload FindHashed() trigger F2084 Internal Error: C2130 on Delphi XE3
  result := fHash.FindOrNew(aHashCode, @Item); // fallback to Scan() if needed
  if result < 0 then
    result := -1; // for coherency with most methods
end;

function TDynArrayHashed.FindHashedForAdding(const Item; out wasAdded: boolean;
  noAddEntry: boolean): integer;
begin
  result := FindHashedForAdding(Item, wasAdded, fHash.HashOne(@Item), noAddEntry);
end;

function TDynArrayHashed.FindHashedForAdding(const Item; out wasAdded: boolean;
  aHashCode: cardinal; noAddEntry: boolean): integer;
begin
  result := fHash.FindBeforeAdd(@Item, wasAdded, aHashCode);
  if wasAdded and not noAddEntry then
    SetCount(result + 1); // reserve space for a void element in array
end;

function TDynArrayHashed.AddAndMakeUniqueName(aName: RawUTF8): pointer;
var
  ndx, j: integer;
  added: boolean;
  aName_: RawUTF8;
begin
  if aName = '' then
    aName := '_';
  ndx := FindHashedForAdding(aName, added);
  if not added then
  begin // force unique column name
    aName_ := aName + '_';
    j := 1;
        repeat
      aName := aName_ + UInt32ToUTF8(j);
      ndx := FindHashedForAdding(aName, added);
      inc(j);
    until added;
  end;
  result := PAnsiChar(Value^) + cardinal(ndx) * ElemSize;
  PRawUTF8(result)^ := aName; // store unique name at 1st elem position
end;

function TDynArrayHashed.AddUniqueName(const aName: RawUTF8;
  aNewIndex: PInteger): pointer;
begin
  result := AddUniqueName(aName, '', [], aNewIndex);
end;

function TDynArrayHashed.AddUniqueName(const aName: RawUTF8; const ExceptionMsg: RawUTF8;
  const ExceptionArgs: array of const; aNewIndex: PInteger): pointer;
var
  ndx: integer;
  added: boolean;
begin
  ndx := FindHashedForAdding(aName, added);
  if added then
  begin
    if aNewIndex <> nil then
      aNewIndex^ := ndx;
    result := PAnsiChar(Value^) + cardinal(ndx) * ElemSize;
    PRawUTF8(result)^ := aName; // store unique name at 1st elem position
  end
  else if ExceptionMsg = '' then
    raise EDynArray.CreateUTF8('TDynArrayHashed: Duplicated [%] name', [aName])
  else
    raise EDynArray.CreateUTF8(ExceptionMsg, ExceptionArgs);
end;

function TDynArrayHashed.FindHashedAndFill(var ItemToFill): integer;
begin
  result := fHash.FindOrNew(fHash.HashOne(@ItemToFill), @ItemToFill);
  if result < 0 then
    result := -1
  else
    ItemCopy(PAnsiChar(Value^) + cardinal(result) * ElemSize, @ItemToFill);
end;

procedure TDynArrayHashed.SetEventHash(const event: TEventDynArrayHashOne);
begin
  fHash.SetEventHash(event);
end;

function TDynArrayHashed.FindHashedAndUpdate(const Item;
  AddIfNotExisting: boolean): integer;
var
  hc: cardinal;
label
  doh;
begin
  if canHash in fHash.State then
  begin
doh:hc := fHash.HashOne(@Item);
    result := fHash.FindOrNew(hc, @Item);
    if (result < 0) and AddIfNotExisting then
    begin
      fHash.HashAdd(hc, result); // ReHash only if necessary
      SetCount(result + 1); // add new item
    end;
  end
  else
  begin
    result := fHash.Scan(@Item);
    if result < 0 then
    begin
      if AddIfNotExisting then
        if canHash in fHash.State then // Scan triggered ReHash
          goto doh
        else
        begin
          result := Add(Item); // regular Add
          exit;
        end;
    end;
  end;
  if result >= 0 then
    ItemCopy(@Item, PAnsiChar(Value^) + cardinal(result) * ElemSize); // update
end;

function TDynArrayHashed.FindHashedAndDelete(const Item; FillDeleted: pointer;
  noDeleteEntry: boolean): integer;
begin
  result := fHash.FindBeforeDelete(@Item);
  if result >= 0 then
  begin
    if FillDeleted <> nil then
      ItemCopyAt(result, FillDeleted);
    if not noDeleteEntry then
      Delete(result);
  end;
end;

function TDynArrayHashed.GetHashFromIndex(aIndex: PtrInt): Cardinal;
begin
  result := fHash.GetHashFromIndex(aIndex);
end;

function TDynArrayHashed.ReHash(forAdd: boolean; forceGrow: boolean): integer;
begin
  result := fHash.ReHash(forAdd, forceGrow);
end;



function DynArray(aTypeInfo: pointer; var aValue;
  aCountPointer: PInteger): TDynArray;
begin
  result.Init(aTypeInfo, aValue, aCountPointer);
end;


{ TSynQueue }

constructor TSynQueue.Create(aTypeInfo: PRttiInfo; const aName: RawUTF8);
begin
  inherited Create(aName);
  fFirst := -1;
  fLast := -2;
  fValues.Init(aTypeInfo, fValueVar, @fCount);
end;

destructor TSynQueue.Destroy;
begin
  WaitPopFinalize;
  fValues.Clear;
  inherited Destroy;
end;

procedure TSynQueue.Clear;
begin
  fSafe.Lock;
  try
    fValues.Clear;
    fFirst := -1;
    fLast := -2;
  finally
    fSafe.UnLock;
  end;
end;

function TSynQueue.Count: Integer;
begin
  if self = nil then
    result := 0
  else
  begin
    fSafe.Lock;
    try
      if fFirst < 0 then
        result := 0
      else if fFirst <= fLast then
        result := fLast - fFirst + 1
      else
        result := fCount - fFirst + fLast + 1;
    finally
      fSafe.UnLock;
    end;
  end;
end;

function TSynQueue.Capacity: integer;
begin
  if self = nil then
    result := 0
  else
  begin
    fSafe.Lock;
    try
      result := fValues.Capacity;
    finally
      fSafe.UnLock;
    end;
  end;
end;

function TSynQueue.Pending: boolean;
begin
  // allow some false positive in heavily multi-threaded context
  result := (self <> nil) and (fFirst >= 0);
end;

procedure TSynQueue.Push(const aValue);
begin
  fSafe.Lock;
  try
    if fFirst < 0 then
    begin
      fFirst := 0; // start from the bottom of the void queue
      fLast := 0;
      if fCount = 0 then
        fValues.Count := 64;
    end
    else if fFirst <= fLast then
    begin // stored in-order
      inc(fLast);
      if fLast = fCount then
        InternalGrow;
    end
    else
    begin
      inc(fLast);
      if fLast = fFirst then
      begin
        // collision -> arrange
        fValues.AddArray(fValueVar, 0, fLast); // move 0..fLast to the end
        fLast := fCount;
        InternalGrow;
      end;
    end;
    fValues.ItemCopyFrom(@aValue, fLast);
  finally
    fSafe.UnLock;
  end;
end;

procedure TSynQueue.InternalGrow;
var
  cap: integer;
begin
  cap := fValues.Capacity;
  if fFirst > cap - fCount then
    // use leading space if worth it
    fLast := 0
  else
  // append at the end
  if fCount = cap then
    // reallocation needed
    fValues.Count := cap + cap shr 3 + 64
  else
    // fill trailing memory as much as possible
    fCount := cap;
end;

function TSynQueue.Peek(out aValue): boolean;
begin
  fSafe.Lock;
  try
    result := fFirst >= 0;
    if result then
      fValues.ItemCopyAt(fFirst, @aValue);
  finally
    fSafe.UnLock;
  end;
end;

function TSynQueue.Pop(out aValue): boolean;
begin
  fSafe.Lock;
  try
    result := fFirst >= 0;
    if result then
    begin
      fValues.ItemMoveTo(fFirst, @aValue);
      if fFirst = fLast then
      begin
        fFirst := -1; // reset whole store (keeping current capacity)
        fLast := -2;
      end
      else
      begin
        inc(fFirst);
        if fFirst = fCount then
          // will retrieve from leading items
          fFirst := 0;
      end;
    end;
  finally
    fSafe.UnLock;
  end;
end;

function TSynQueue.PopEquals(aAnother: pointer; aCompare: TDynArraySortCompare;
  out aValue): boolean;
begin
  fSafe.Lock;
  try
    result := (fFirst >= 0) and Assigned(aCompare) and Assigned(aAnother) and
      (aCompare(fValues.ItemPtr(fFirst)^, aAnother^) = 0) and Pop(aValue);
  finally
    fSafe.UnLock;
  end;
end;

function TSynQueue.InternalDestroying(incPopCounter: integer): boolean;
begin
  fSafe.Lock;
  try
    result := wpfDestroying in fWaitPopFlags;
    inc(fWaitPopCounter, incPopCounter);
  finally
    fSafe.UnLock;
  end;
end;

function TSynQueue.InternalWaitDone(endtix: Int64; const idle: TThreadMethod): boolean;
begin
  SleepHiRes(1);
  if Assigned(idle) then
    idle; // e.g. Application.ProcessMessages
  result := InternalDestroying(0) or (GetTickCount64 > endtix);
end;

function TSynQueue.WaitPop(aTimeoutMS: integer; const aWhenIdle: TThreadMethod;
  out aValue; aCompared: pointer; aCompare: TDynArraySortCompare): boolean;
var
  endtix: Int64;
begin
  result := false;
  if not InternalDestroying(+1) then
  try
    endtix := GetTickCount64 + aTimeoutMS;
    repeat
      if Assigned(aCompared) and Assigned(aCompare) then
        result := PopEquals(aCompared, aCompare, aValue)
      else
        result := Pop(aValue);
    until result or InternalWaitDone(endtix, aWhenIdle);
  finally
    InternalDestroying(-1);
  end;
end;

function TSynQueue.WaitPeekLocked(aTimeoutMS: integer;
  const aWhenIdle: TThreadMethod): pointer;
var
  endtix: Int64;
begin
  result := nil;
  if not InternalDestroying(+1) then
  try
    endtix := GetTickCount64 + aTimeoutMS;
    repeat
      fSafe.Lock;
      try
        if fFirst >= 0 then
          result := fValues.ItemPtr(fFirst);
      finally
        if result = nil then
          fSafe.UnLock; // caller should always Unlock once done
      end;
    until (result <> nil) or InternalWaitDone(endtix, aWhenIdle);
  finally
    InternalDestroying(-1);
  end;
end;

procedure TSynQueue.WaitPopFinalize(aTimeoutMS: integer);
var
  endtix: Int64; // never wait forever
begin
  fSafe.Lock;
  try
    include(fWaitPopFlags, wpfDestroying);
    if fWaitPopCounter = 0 then
      exit;
  finally
    fSafe.UnLock;
  end;
  endtix := GetTickCount64 + aTimeoutMS;
  repeat
    SleepHiRes(1); // ensure WaitPos() is actually finished
  until (fWaitPopCounter = 0) or (GetTickCount64 > endtix);
end;

procedure TSynQueue.Save(out aDynArrayValues; aDynArray: PDynArray);
var
  n: integer;
  DA: TDynArray;
begin
  DA.Init(fValues.Info.Info, aDynArrayValues, @n);
  fSafe.Lock;
  try
    DA.Capacity := Count; // pre-allocate whole array, and set its length
    if fFirst >= 0 then
      if fFirst <= fLast then
        DA.AddArray(fValueVar, fFirst, fLast - fFirst + 1)
      else
      begin
        DA.AddArray(fValueVar, fFirst, fCount - fFirst);
        DA.AddArray(fValueVar, 0, fLast + 1);
      end;
  finally
    fSafe.UnLock;
  end;
  if aDynArray <> nil then
    aDynArray^.Init(fValues.Info.Info, aDynArrayValues);
end;

procedure TSynQueue.LoadFromReader;
var
  n: cardinal;
  info: PRttiInfo;
  load: TRttiBinaryLoad;
  p: PAnsiChar;
begin
  fSafe.Lock;
  try
    Clear;
    inherited LoadFromReader;
    n := fReader.VarUInt32;
    if n = 0 then
      exit;
    fFirst := 0;
    fLast := n - 1;
    fValues.Count := n;
    p := fValues.Value^;
    info := fValues.Info.Cache.ItemInfo;
    if info <> nil then
    begin
      load := RTTI_BINARYLOAD[info^.Kind];
      repeat
        inc(p, load(p, fReader, info));
        dec(n);
      until n = 0;
    end
    else
      fReader.Copy(p, n * fValues.ElemSize);
  finally
    fSafe.UnLock;
  end;
end;

procedure TSynQueue.SaveToWriter(aWriter: TBufferWriter);
var
  n: integer;
  info: PRttiInfo;
  sav: TRttiBinarySave;

  procedure WriteItems(start, count: cardinal);
  var
    p: PAnsiChar;
  begin
    if count = 0 then
      exit;
    p := fValues.ItemPtr(start);
    if info = nil then
      aWriter.Write(p, count * fValues.ElemSize)
    else
      repeat
        inc(p, sav(p, aWriter, info));
        dec(count);
      until count = 0;
  end;

begin
  fSafe.Lock;
  try
    inherited SaveToWriter(aWriter);
    n := Count;
    aWriter.WriteVarUInt32(n);
    if n = 0 then
      exit;
    info := fValues.Info.Cache.ItemInfo;
    if info <> nil then
      sav := RTTI_BINARYSAVE[info^.Kind]
    else
      sav := nil;
    if fFirst <= fLast then
      WriteItems(fFirst, fLast - fFirst + 1)
    else
    begin
      WriteItems(fFirst, fCount - fFirst);
      WriteItems(0, fLast + 1);
    end;
  finally
    fSafe.UnLock;
  end;
end;


{ ***************** Cross-Platform Time Zones }


{ TTimeZoneData }

function TTimeZoneData.GetTziFor(year: integer): PTimeZoneInfo;
var
  i, last: PtrInt;
begin
  if dyn = nil then
    result := @tzi
  else if year <= dyn[0].year then
    result := @dyn[0].tzi
  else
  begin
    last := high(dyn);
    if year >= dyn[last].year then
      result := @dyn[last].tzi
    else
    begin
      for i := 1 to last do
        if year < dyn[i].year then
        begin
          result := @dyn[i - 1].tzi;
          exit;
        end;
      result := @tzi; // should never happen, but makes compiler happy
    end;
  end;
end;


{ TTimeZoneInformation }

constructor TSynTimeZone.Create;
begin
  fZones.InitSpecific(TypeInfo(TTimeZoneDataDynArray), fZone, ptRawUTF8);
end;

constructor TSynTimeZone.CreateDefault;
begin
  Create;
  {$ifdef MSWINDOWS}
  LoadFromRegistry;
  {$else}
  LoadFromFile;
  if fZones.Count = 0 then
    LoadFromResource; // if no .tz file is available, try if bound to executable
  {$endif MSWINDOWS}
end;

destructor TSynTimeZone.Destroy;
begin
  inherited Destroy;
  fIds.Free;
  fDisplays.Free;
end;

var
  SharedSynTimeZone: TSynTimeZone;

class function TSynTimeZone.Default: TSynTimeZone;
begin
  if SharedSynTimeZone = nil then
  begin
    GlobalLock;
    try
      if SharedSynTimeZone = nil then
        SharedSynTimeZone := TSynTimeZone.CreateDefault;
    finally
      GlobalUnLock;
    end;
  end;
  result := SharedSynTimeZone;
end;

function TSynTimeZone.SaveToBuffer: RawByteString;
begin
  result := AlgoSynLZ.Compress(fZones.SaveTo);
end;

procedure TSynTimeZone.SaveToFile(const FileName: TFileName);
var
  FN: TFileName;
begin
  if FileName = '' then
    FN := ChangeFileExt(ExeVersion.ProgramFileName, '.tz')
  else
    FN := FileName;
  FileFromString(SaveToBuffer, FN);
end;

procedure TSynTimeZone.LoadFromBuffer(const Buffer: RawByteString);
begin
  fZones.LoadFromBinary(AlgoSynLZ.Decompress(Buffer));
  fZones.ReHash;
  FreeAndNil(fIds);
  FreeAndNil(fDisplays);
end;

procedure TSynTimeZone.LoadFromFile(const FileName: TFileName);
var
  FN: TFileName;
begin
  if FileName = '' then
    FN := ChangeFileExt(ExeVersion.ProgramFileName, '.tz')
  else
    FN := FileName;
  LoadFromBuffer(StringFromFile(FN));
end;

procedure TSynTimeZone.LoadFromResource(Instance: THandle);
var
  buf: RawByteString;
begin
  ResourceToRawByteString(ClassName, PChar(10), buf, Instance);
  if buf <> '' then
    LoadFromBuffer(buf);
end;

{$ifdef MSWINDOWS}

procedure TSynTimeZone.LoadFromRegistry;
const
  REGKEY = 'Software\Microsoft\Windows NT\CurrentVersion\Time Zones\';
var
  reg: TWinRegistry;
  keys: TRawUTF8DynArray;
  i, first, last, year, n: integer;
  item: TTimeZoneData;
begin
  fZones.Clear;
  if reg.ReadOpen(wrLocalMachine, REGKEY) then
    keys := reg.ReadEnumEntries
  else
    keys := nil; // make Delphi 6 happy
  n := length(keys);
  fZones.Capacity := n;
  for i := 0 to n - 1 do
  begin
    Finalize(item);
    FillcharFast(item.tzi, SizeOf(item.tzi), 0);
    if reg.ReadOpen(wrLocalMachine, REGKEY + keys[i], {reopen=}true) then
    begin
      item.id := keys[i];
      item.Display := reg.ReadString('Display');
      reg.ReadBuffer('TZI', @item.tzi, SizeOf(item.tzi));
      if reg.ReadOpen(wrLocalMachine, REGKEY + keys[i] + '\Dynamic DST', true) then
      begin
        // warning: never defined on XP/2003, and not for all entries
        first := reg.ReadDword('FirstEntry');
        last := reg.ReadDword('LastEntry');
        if (first > 0) and (last >= first) then
        begin
          n := 0;
          SetLength(item.dyn, last - first + 1);
          for year := first to last do
            if reg.ReadBuffer(UTF8ToSynUnicode(UInt32ToUTF8(year)),
              @item.dyn[n].tzi, SizeOf(TTimeZoneInfo)) then
            begin
              item.dyn[n].year := year;
              inc(n);
            end;
          SetLength(item.dyn, n);
        end;
      end;
      fZones.Add(item);
    end;
  end;
  reg.Close;
  fZones.ReHash;
  FreeAndNil(fIds);
  FreeAndNil(fDisplays);
end;

{$endif MSWINDOWS}

function TSynTimeZone.GetDisplay(const TzId: TTimeZoneID): RawUTF8;
var
  ndx: integer;
begin
  if self = nil then
    ndx := -1
  else
    ndx := fZones.FindHashed(TzId);
  if ndx < 0 then
    if TzId = 'UTC' then // e.g. on XP
      result := TzId
    else
      result := ''
  else
    result := fZone[ndx].display;
end;

function TSynTimeZone.GetBiasForDateTime(const Value: TDateTime;
  const TzId: TTimeZoneID; out Bias: integer; out HaveDaylight: boolean): boolean;
var
  ndx: integer;
  d: TSynSystemTime;
  tzi: PTimeZoneInfo;
  std, dlt: TDateTime;
begin
  if (self = nil) or (TzId = '') then
    ndx := -1
  else if TzId = fLastZone then
    ndx := fLastIndex
  else
  begin
    ndx := fZones.FindHashed(TzId);
    fLastZone := TzId;
    flastIndex := ndx;
  end;
  if ndx < 0 then
  begin
    Bias := 0;
    HaveDaylight := false;
    result := TzId = 'UTC'; // e.g. on XP
    exit;
  end;
  d.FromDate(Value); // faster than DecodeDate
  tzi := fZone[ndx].GetTziFor(d.Year);
  if tzi.change_time_std.IsZero then
  begin
    HaveDaylight := false;
    Bias := tzi.Bias + tzi.bias_std;
  end
  else
  begin
    HaveDaylight := true;
    std := tzi.change_time_std.EncodeForTimeChange(d.Year);
    dlt := tzi.change_time_dlt.EncodeForTimeChange(d.Year);
    if std < dlt then
      if (std <= Value) and (Value < dlt) then
        Bias := tzi.Bias + tzi.bias_std
      else
        Bias := tzi.Bias + tzi.bias_dlt
    else if (dlt <= Value) and (Value < std) then
      Bias := tzi.Bias + tzi.bias_dlt
    else
      Bias := tzi.Bias + tzi.bias_std;
  end;
  result := true;
end;

function TSynTimeZone.UtcToLocal(const UtcDateTime: TDateTime;
  const TzId: TTimeZoneID): TDateTime;
var
  Bias: integer;
  HaveDaylight: boolean;
begin
  if (self = nil) or (TzId = '') then
    result := UtcDateTime
  else
  begin
    GetBiasForDateTime(UtcDateTime, TzId, Bias, HaveDaylight);
    result := ((UtcDateTime * MinsPerDay) - Bias) / MinsPerDay;
  end;
end;

function TSynTimeZone.NowToLocal(const TzId: TTimeZoneID): TDateTime;
begin
  result := UtcToLocal(NowUtc, TzId);
end;

function TSynTimeZone.LocalToUtc(const LocalDateTime: TDateTime;
  const TzID: TTimeZoneID): TDateTime;
var
  Bias: integer;
  HaveDaylight: boolean;
begin
  if (self = nil) or (TzID = '') then
    result := LocalDateTime
  else
  begin
    GetBiasForDateTime(LocalDateTime, TzID, Bias, HaveDaylight);
    result := ((LocalDateTime * MinsPerDay) + Bias) / MinsPerDay;
  end;
end;

function TSynTimeZone.Ids: TStrings;
var
  i: PtrInt;
begin
  if fIDs = nil then
  begin
    fIDs := TStringList.Create;
    for i := 0 to length(fZone) - 1 do
      fIDs.Add(UTF8ToString(fZone[i].id));
  end;
  result := fIDs;
end;

function TSynTimeZone.Displays: TStrings;
var
  i: PtrInt;
begin
  if fDisplays = nil then
  begin
    fDisplays := TStringList.Create;
    for i := 0 to length(fZone) - 1 do
      fDisplays.Add(UTF8ToString(fZone[i].Display));
  end;
  result := fDisplays;
end;



procedure InitializeUnit;
var
  k: TRttiKind;
begin
  // initialize RTTI binary persistence and comparison
  MoveFast(_PT_HASH, PT_HASH, SizeOf(PT_HASH));
  for k := succ(low(k)) to high(k) do
    case k of
      rkInteger, rkEnumeration, rkSet, rkChar, rkWChar {$ifdef FPC}, rkBool{$endif}:
        begin
          RTTI_BINARYSAVE[k] := @_BS_Ord;
          RTTI_BINARYLOAD[k] := @_BL_Ord;
          RTTI_COMPARE[false, k] := @_BC_Ord;
          RTTI_COMPARE[true, k] := @_BC_Ord;
        end;
        {$ifdef FPC} rkQWord, {$endif} rkInt64:
        begin
          RTTI_BINARYSAVE[k] := @_BS_64;
          RTTI_BINARYLOAD[k] := @_BL_64;
          RTTI_COMPARE[false, k] := @_BC_64;
          RTTI_COMPARE[true, k] := @_BC_64;
        end;
      rkFloat:
        begin
          RTTI_BINARYSAVE[k] := @_BS_Float;
          RTTI_BINARYLOAD[k] := @_BS_Float;
          RTTI_COMPARE[false, k] := @_BC_Float;
          RTTI_COMPARE[true, k] := @_BC_Float;
        end;
      {$ifdef HASVARUSTRING} rkUString, {$endif} rkLString:
        begin
          RTTI_BINARYSAVE[k] := @_BS_String;
          if k = rkLString then
          begin
            RTTI_BINARYLOAD[k] := @_BL_LString;
            RTTI_COMPARE[false, k] := @_BC_PUTF8Char;
            RTTI_COMPARE[true, k] := @_BCI_PUTF8Char;
          end
          {$ifdef HASVARUSTRING}
          else if k = rkUString then
          begin
            RTTI_BINARYLOAD[k] := @_BL_UString;
            RTTI_COMPARE[false, k] := @_BC_PWideChar;
            RTTI_COMPARE[true, k] := @_BCI_PWideChar;
          end;
          {$endif HASVARUSTRING}
        end; // rkLStringOld not generated any more
      rkWString:
        begin
          RTTI_BINARYSAVE[k] := @_BS_WString;
          RTTI_BINARYLOAD[k] := @_BL_WString;
          RTTI_COMPARE[false, k] := @_BC_PWideChar;
          RTTI_COMPARE[true, k] := @_BCI_PWideChar;
        end;
      {$ifdef FPC} rkObject, {$endif} rkRecord:
        begin
          RTTI_BINARYSAVE[k] := @_BS_Record;
          RTTI_BINARYLOAD[k] := @_BL_Record;
          RTTI_COMPARE[false, k] := @_BC_Record;
          RTTI_COMPARE[true, k] := @_BCI_Record;
        end;
      rkDynArray:
        begin
          RTTI_BINARYSAVE[k] := @_BS_DynArray;
          RTTI_BINARYLOAD[k] := @_BL_DynArray;
          RTTI_COMPARE[false, k] := @_BC_DynArray;
          RTTI_COMPARE[true, k] := @_BCI_DynArray;
        end;
      rkArray:
        begin
          RTTI_BINARYSAVE[k] := @_BS_Array;
          RTTI_BINARYLOAD[k] := @_BL_Array;
          RTTI_COMPARE[false, k] := @_BC_Array;
          RTTI_COMPARE[true, k] := @_BCI_Array;
        end;
      rkVariant:
        begin
          RTTI_BINARYSAVE[k] := @_BS_Variant;
          RTTI_BINARYLOAD[k] := @_BL_Variant;
          RTTI_COMPARE[false, k] := @_BC_Variant;
          RTTI_COMPARE[true, k] := @_BCI_Variant;
        end;
      rkClass:
        begin
          RTTI_COMPARE[false, k] := @_BC_Object;
          RTTI_COMPARE[true, k] := @_BCI_Object;
        end;
        // unsupported types will contain nil
    end;
  // setup internal function wrappers
  GetDataFromJSON := _GetDataFromJSON;
end;


initialization
  InitializeUnit;

finalization

end.

