/// Framework Core Low-Level Generics Collection Process
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.collections;

{
  *****************************************************************************

   Generics Collections as used by all framework units
   - JSON-aware Generics TSynKeyValue<> Dictionary Storage

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

{$ifdef HASGENERICS} // do-nothing unit on oldest compilers (e.g. < Delphi 2010)

uses
  classes,
  contnrs,
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.data,
  mormot.core.rtti,
  mormot.core.json;


{ ************** JSON-aware Generics TSynKeyValue<> Dictionary Storage }

type
  /// exception class raised by TSynKeyValue<TKey, TValue>
  EKeyValue = class(ESynException);

  /// gives access to a generics-based dictionary holding key/value pairs
  IKeyValue<TKey, TValue> = interface
    // some property accessors
    function GetItem(const key: TKey): TValue;
    procedure SetItem(const key: TKey; const value: TValue);
    function GetCapacity: integer;
    procedure SetCapacity(value: integer);
    function GetTimeOutSeconds: cardinal;
    procedure SetTimeOutSeconds(value: cardinal);
    /// add a key/value pair to be unique
    // - raise an EKeyValue if key was already set
    // - use default Items[] property to add or replace a key/value pair
    procedure Add(const key: TKey; const value: TValue);
    /// add a key/value pair if key is not existing
    // - returns true if was added, false if key was already set
    // - use default Items[] property to add or replace a key/value pair
    function TryAdd(const key: TKey; const value: TValue): boolean;
    /// search a key and return the associated key pair
    // - returns true if the key was found, false otherwise
    function TryGetValue(const key: TKey; var value: TValue): boolean;
    /// search a key and return the associated key pair or its default value
    function GetValueOrDefault(const key: TKey): TValue; overload;
    /// search a key and return the associated key pair or a supplied default value
    function GetValueOrDefault(const key: TKey; const defaultValue: TValue): TValue; overload;
    /// remove a key/value pair
    // - returns true if the entry was deleted, false if key was not found
    function Remove(const key: TKey): boolean;
    /// search a key, get the associated value, then delete the key/value pair
    function Extract(const key: TKey; var value: TValue): boolean;
    /// search for a key/value pair from a key
    // - returns true if the key was found, false otherwise
    function ContainsKey(const key: TKey): boolean;
    /// search for a key/value pair from a value
    // - returns true if the value was found, false otherwise
    function ContainsValue(const value: TValue): boolean;
    /// search and delete all deprecated items according to TimeoutSeconds
    // - returns how many items have been deleted
    // - you can call this method very often: it will ensure that the
    // search process will take place at most once every second
    function DeleteDeprecated: integer;
    /// delete all stored key/value pairs
    procedure Clear; overload;
    /// delete one stored key/value pairs from its key
    function Clear(const key: TKey): boolean; overload;
    /// high-level access to the stored values from their associated keys
    // - raise an EKeyValue if the key is not available, unless
    // kvoDefaultIfNotFound option was set
    // - use TryGetValue() if you want to detect non available key
    property Items[const key: TKey]: TValue
      read GetItem write SetItem; default;
    /// returns the number of key/value pairs actually stored
    function Count: integer;
    /// returns the internal TSynDictionary capacity
    property Capacity: integer
      read GetCapacity write SetCapacity;
    /// returns the aTimeOutSeconds parameter value, as specified to Create()
    // - warning: setting a new timeout will clear all previous content
    property TimeOutSeconds: cardinal
      read GetTimeOutSeconds write SetTimeOutSeconds;
    /// low-level access to the internal TSynDictionary storage
    // - since this class is thread-safe, you could use this method to
    // manually access its content
    // - you can use e.g. Data.SaveToJson/SaveToBinary and
    // Data.LoadFromJson/LoadFromBinary
    function Data: TSynDictionary;
  end;

  /// how TSynKeyValue<TKey, TValue>.Create() will allocate its storage
  TSynKeyValueOptions = set of (
    kvoKeyCaseInsensitive, kvoThreadSafe, kvoDefaultIfNotFound);

  /// thread-safe generics-based dictionary holding key/value pairs
  // - is a high level wrapper around our regular TSynDictionary
  // - could be accessed from an IKeyValue<TKey, TValue> interface for
  // automatic reference counting and memory management
  TSynKeyValue<TKey, TValue> = class(TInterfacedObject, IKeyValue<TKey, TValue>)
  protected
    fData: TSynDictionary;
    fOptions: TSynKeyValueOptions;
    procedure RaiseException(const msg: ShortString); overload;
    procedure RaiseException(const fmt: RawUtf8; const args: array of const); overload;
    // some property accessors
    function GetItem(const key: TKey): TValue;
    procedure SetItem(const key: TKey; const value: TValue); inline;
    function GetCapacity: integer; inline;
    procedure SetCapacity(value: integer); inline;
    function GetTimeOutSeconds: cardinal; inline;
    procedure SetTimeOutSeconds(value: cardinal); inline;
  public
    /// initialize the dictionary storage, specifyng dynamic array keys/values
    // - you will need to provide the dynamic arrays TypeInfo() of TKey/TValue
    // - by default, this instance won't be thread-safe unless the kvoThreadSafe
    // option is forced, so that process is protected with a TSynLocker mutex
    // - by default, string keys would be searched following exact case, unless
    // the kvoKeyCaseInsensitive option is set
    // - you can set an optional timeout period, in seconds - you should call
    // DeleteDeprecated periodically to search for deprecated items
    constructor Create(aKeyDynArrayTypeInfo, aValueDynArrayTypeInfo: PRttiInfo;
      aOptions: TSynKeyValueOptions = [];
      aTimeoutSeconds: cardinal = 0; aCompressAlgo: TAlgoCompress = nil;
      aHasher: THasher = nil); reintroduce; virtual;
    /// finalize the dictionary storage
    destructor Destroy; override;
  public
    { IKeyValue<TKey, TValue> methods }
    /// add a key/value pair to be unique
    // - raise an EKeyValue if key was already set
    // - use default Items[] property to add or replace a key/value pair
    procedure Add(const key: TKey; const value: TValue); inline;
    /// add a key/value pair if key is not existing
    // - returns true if was added, false if key was already set
    // - use default Items[] property to add or replace a key/value pair
    function TryAdd(const key: TKey; const value: TValue): boolean; inline;
    /// search a key and return the associated key pair
    // - returns true if the key was found, false otherwise
    function TryGetValue(const key: TKey; var value: TValue): boolean; inline;
    /// search a key and return the associated key pair or its default value
    function GetValueOrDefault(const key: TKey): TValue; overload;
    /// search a key and return the associated key pair or a supplied default value
    function GetValueOrDefault(const key: TKey;
      const defaultValue: TValue): TValue; overload;
    /// remove a key/value pair
    // - returns true if the entry was deleted, false if key was not found
    function Remove(const key: TKey): boolean; inline;
    /// search a key, get the associated value, then delete the key/value pair
    function Extract(const key: TKey; var value: TValue): boolean; inline;
    /// search for a key/value pair from a key
    // - returns true if the key was found, false otherwise
    function ContainsKey(const key: TKey): boolean; inline;
    /// search for a key/value pair from a value
    // - returns true if the value was found, false otherwise
    function ContainsValue(const value: TValue): boolean; inline;
    /// search and delete all deprecated items according to TimeoutSeconds
    // - returns how many items have been deleted
    // - you can call this method very often: it will ensure that the
    // search process will take place at most once every second
    function DeleteDeprecated: integer; inline;
    /// delete all stored key/value pairs
    procedure Clear; overload;
    /// delete one stored key/value pairs from its key
    function Clear(const key: TKey): boolean; overload;
    /// high-level access to the stored values from their associated keys
    // - raise an EKeyValue if the key is not available, unless
    // kvoDefaultIfNotFound option was set
    // - use TryGetValue/GetValueOrDefault to detect non available key
    property Items[const key: TKey]: TValue
      read GetItem write SetItem; default;
    /// returns the number of key/value pairs actually stored
    function Count: integer; inline;
    /// returns the internal TSynDictionary capacity
    property Capacity: integer
      read GetCapacity write SetCapacity;
    /// returns the aTimeOutSeconds parameter value, as supplied to Create()
    // - warning: setting a new timeout will clear all previous content
    property TimeOutSeconds: cardinal
      read GetTimeOutSeconds write SetTimeOutSeconds;
    /// low-level access to the internal TSynDictionary storage
    // - since this class is thread-safe, you could use this method to
    // manually access its content
    // - you can use e.g. Data.SaveToJson/SaveToBinary and
    // Data.LoadFromJson/LoadFromBinary
    function Data: TSynDictionary; inline;
    /// low-level access to the TSynKeyValueOptions as supplied to Create()
    property Options: TSynKeyValueOptions
      read fOptions;
  end;

  // some convenient aliases to most useful TSynKeyValue<TKey, TValue> types
  IIntegerRawUtf8 = IKeyValue<Integer, RawUtf8>;
  IInt64RawUtf8 = IKeyValue<Int64, RawUtf8>;

  IRawUtf8Integer = IKeyValue<RawUtf8, Integer>;
  IRawUtf8Int64 = IKeyValue<RawUtf8, Int64>;
  IRawUtf8RawUtf8 = IKeyValue<RawUtf8, RawUtf8>;
  IRawUtf8String = IKeyValue<RawUtf8, String>;

  IGuidInteger = IKeyValue<TGuid, Integer>;
  IGuidInt64 = IKeyValue<TGuid, Int64>;
  IGuidRawUtf8 = IKeyValue<TGuid, RawUtf8>;


/// generate a new TSynKeyValue<integer, RawUtf8> dictionary instance
function NewIntegerRawUtf8(options: TSynKeyValueOptions = []): IIntegerRawUtf8;

/// generate a new TSynKeyValue<Int64, RawUtf8> dictionary instance
function NewInt64RawUtf8(options: TSynKeyValueOptions = []): IInt64RawUtf8;

/// generate a new TSynKeyValue<RawUtf8, Integer> dictionary instance
function NewRawUtf8Integer(options: TSynKeyValueOptions = []): IRawUtf8Integer;

/// generate a new TSynKeyValue<RawUtf8, Int64> dictionary instance
function NewRawUtf8Int64(options: TSynKeyValueOptions = []): IRawUtf8Int64;

/// generate a new TSynKeyValue<RawUtf8, RawUtf8> dictionary instance
function NewRawUtf8RawUtf8(options: TSynKeyValueOptions = []): IRawUtf8RawUtf8;

/// generate a new TSynKeyValue<RawUtf8, String> dictionary instance
function NewRawUtf8String(options: TSynKeyValueOptions = []): IRawUtf8String;

/// generate a new TSynKeyValue<TGuid, Integer> dictionary instance
function NewGuidInteger(options: TSynKeyValueOptions = []): IGuidInteger;

/// generate a new TSynKeyValue<TGuid, Int64> dictionary instance
function NewGuidInt64(options: TSynKeyValueOptions = []): IGuidInt64;

/// generate a new TSynKeyValue<TGuid, RawUtf8> dictionary instance
function NewGuidRawUtf8(options: TSynKeyValueOptions = []): IGuidRawUtf8;



implementation


{ ************** JSON-aware Generics TSynKeyValue<> Dictionary Storage }

{ TSynKeyValue<TKey, TValue> }

constructor TSynKeyValue<TKey, TValue>.Create(
  aKeyDynArrayTypeInfo, aValueDynArrayTypeInfo: PRttiInfo;
  aOptions: TSynKeyValueOptions; aTimeoutSeconds: cardinal;
  aCompressAlgo: TAlgoCompress; aHasher: THasher);
begin
  fOptions := aOptions;
  if (aKeyDynArrayTypeInfo = nil) or
     (aKeyDynArrayTypeInfo^.Kind <> rkDynArray) then
     RaiseException('Create: TKey should be a dynamic array');
  if (aValueDynArrayTypeInfo = nil) or
     (aValueDynArrayTypeInfo^.Kind <> rkDynArray) then
     RaiseException('Create: TValue should be a dynamic array');
  fData := TSynDictionary.Create(aKeyDynArrayTypeInfo, aValueDynArrayTypeInfo,
    kvoKeyCaseInsensitive in aOptions, aTimeoutSeconds, aCompressAlgo, aHasher);
  if fData.Keys.Info.Cache.ItemInfo <> TypeInfo(TKey) then
    RaiseException('Create: TKey does not match %',
      [aValueDynArrayTypeInfo^.RawName]);
  if fData.Values.Info.Cache.ItemInfo <> TypeInfo(TValue) then
    RaiseException('Create: TValue does not match %',
      [aValueDynArrayTypeInfo^.RawName]);
  if kvoThreadSafe in aOptions then
    fData.Options := [doSingleThreaded];
end;

destructor TSynKeyValue<TKey, TValue>.Destroy;
begin
  inherited Destroy;
  fData.Free;
end;

procedure TSynKeyValue<TKey, TValue>.RaiseException(const msg: ShortString);
begin
  raise EKeyValue.CreateUtf8('TSynKeyValue<%, %>.%',
    [PRttiInfo(TypeInfo(TKey))^.Name, PRttiInfo(TypeInfo(TValue))^.Name, msg]);
end;

procedure TSynKeyValue<TKey, TValue>.RaiseException(const fmt: RawUtf8;
  const args: array of const);
var
  msg: ShortString;
begin
  FormatShort(fmt, args, msg);
  RaiseException(msg);
end;

procedure TSynKeyValue<TKey, TValue>.Add(const key: TKey; const value: TValue);
begin
  if fData.Add(key, value) < 0 then
    RaiseException('Add: duplicated key');
end;

function TSynKeyValue<TKey, TValue>.TryAdd(const key: TKey;
  const value: TValue): boolean;
begin
  result := fData.Add(key, value) >= 0;
end;

function TSynKeyValue<TKey, TValue>.TryGetValue(const key: TKey;
  var value: TValue): boolean;
begin // defined as "var value" instead of "out value" to avoid caller finalize
  result := fData.FindAndCopy(key, value);
end;

function TSynKeyValue<TKey, TValue>.GetValueOrDefault(const key: TKey): TValue;
begin
  if not fData.FindAndCopy(key, result) then
    fData.Values.ItemClear(@result);
end;

function TSynKeyValue<TKey, TValue>.GetValueOrDefault(const key: TKey;
  const defaultValue: TValue): TValue;
begin
  if not fData.FindAndCopy(key, result) then
    result := defaultValue;
end;

function TSynKeyValue<TKey, TValue>.Remove(const key: TKey): boolean;
begin
  result := fData.Delete(key) >= 0;
end;

function TSynKeyValue<TKey, TValue>.Extract(const key: TKey;
  var value: TValue): boolean;
begin
  result := fData.FindAndExtract(key, value);
end;

function TSynKeyValue<TKey, TValue>.ContainsKey(const key: TKey): boolean;
begin
  result := fData.Exists(key);
end;

function TSynKeyValue<TKey, TValue>.ContainsValue(const value: TValue): boolean;
begin
  result := fData.ExistsValue(value);
end;

function TSynKeyValue<TKey, TValue>.DeleteDeprecated: integer;
begin
  result := fData.DeleteDeprecated;
end;

procedure TSynKeyValue<TKey, TValue>.Clear;
begin
  fData.DeleteAll;
end;

function TSynKeyValue<TKey, TValue>.Clear(const key: TKey): boolean;
begin
  result := fData.Clear(key) >= 0;
end;

function TSynKeyValue<TKey, TValue>.Count: integer;
begin
  result := fData.Count;
end;

function TSynKeyValue<TKey, TValue>.GetTimeOutSeconds: cardinal;
begin
  result := fData.TimeOutSeconds;
end;

procedure TSynKeyValue<TKey, TValue>.SetTimeOutSeconds(value: cardinal);
begin
  fData.TimeOutSeconds := value;
end;

function TSynKeyValue<TKey, TValue>.Data: TSynDictionary;
begin
  result := fData;
end;

function TSynKeyValue<TKey, TValue>.GetItem(const key: TKey): TValue;
begin
  if not fData.FindAndCopy(key, result) then
    if kvoDefaultIfNotFound in fOptions then
      fData.Values.ItemClear(@result)
    else
      RaiseException('GetItem: key not found');
end;

procedure TSynKeyValue<TKey, TValue>.SetItem(const key: TKey;
  const value: TValue);
begin
  fData.AddOrUpdate(key, value);
end;

function TSynKeyValue<TKey, TValue>.GetCapacity: integer;
begin
  result := fData.Capacity;
end;

procedure TSynKeyValue<TKey, TValue>.SetCapacity(value: integer);
begin
  fData.Capacity := value;
end;


function NewIntegerRawUtf8(options: TSynKeyValueOptions): IIntegerRawUtf8;
begin
  result := TSynKeyValue<Integer, RawUtf8>.Create(TypeInfo(TIntegerDynArray),
    TypeInfo(TRawUtf8DynArray), options);
end;

function NewInt64RawUtf8(options: TSynKeyValueOptions): IInt64RawUtf8;
begin
  result := TSynKeyValue<Int64, RawUtf8>.Create(TypeInfo(TInt64DynArray),
    TypeInfo(TRawUtf8DynArray), options);
end;

function NewRawUtf8RawUtf8(options: TSynKeyValueOptions): IRawUtf8RawUtf8;
begin
  result := TSynKeyValue<RawUtf8, RawUtf8>.Create(TypeInfo(TRawUtf8DynArray),
    TypeInfo(TRawUtf8DynArray), options);
end;

function NewRawUtf8Integer(options: TSynKeyValueOptions): IRawUtf8Integer;
begin
  result := TSynKeyValue<RawUtf8, Integer>.Create(TypeInfo(TRawUtf8DynArray),
    TypeInfo(TIntegerDynArray), options);
end;

function NewRawUtf8Int64(options: TSynKeyValueOptions): IRawUtf8Int64;
begin
  result := TSynKeyValue<RawUtf8, Int64>.Create(TypeInfo(TRawUtf8DynArray),
    TypeInfo(TInt64DynArray), options);
end;

function NewRawUtf8String(options: TSynKeyValueOptions): IRawUtf8String;
begin
  result := TSynKeyValue<RawUtf8, String>.Create(TypeInfo(TRawUtf8DynArray),
    TypeInfo(TStringDynArray), options);
end;

function NewGuidInteger(options: TSynKeyValueOptions): IGuidInteger;
begin
  result := TSynKeyValue<TGuid, Integer>.Create(TypeInfo(TGuidDynArray),
    TypeInfo(TIntegerDynArray), options);
end;

function NewGuidInt64(options: TSynKeyValueOptions): IGuidInt64;
begin
  result := TSynKeyValue<TGuid, Int64>.Create(TypeInfo(TGuidDynArray),
    TypeInfo(TInt64DynArray), options);
end;

function NewGuidRawUtf8(options: TSynKeyValueOptions): IGuidRawUtf8;
begin
  result := TSynKeyValue<TGuid, RawUtf8>.Create(TypeInfo(TGuidDynArray),
    TypeInfo(TRawUtf8DynArray), options);
end;

{$else}
implementation
{$endif HASGENERICS} // do-nothing unit on oldest compilers


end.

