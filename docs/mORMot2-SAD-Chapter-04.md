# 4. Core Units (mormot.core.*)

*The Foundation Bricks*

The mORMot 2 framework uses custom low-level types, classes, and functions instead of relying solely on the standard Delphi RTL. This design choice provides:

- **Cross-platform and cross-compiler support** (Delphi 7 through 12.2, FPC 3.2+)
- **Unicode support** via native UTF-8 encoding for all versions
- **Optimized performance** for speed, multi-threading, and memory efficiency
- **Consistent KISS design** with shared common features

In mORMot 1, most of this functionality resided in a single 2.3MB `SynCommons.pas` file. In mORMot 2, this has been split into **24 focused units** in the `mormot.core.*` namespace, following SOLID principles.

---

## 4.1. Conditional Defines

A global `mormot.defines.inc` include file appears in all framework units:

```pascal
{$I mormot.defines.inc}
```

This defines key conditionals for portable and efficient code:

| Define | Purpose |
|--------|---------|
| `PUREMORMOT2` | Disable mORMot 1.18 compatibility aliases (recommended for new code) |
| `FPC_X64MM` | Use custom x64 memory manager (FPC only, Linux/Windows) |
| `FPCMM_BOOST` / `FPCMM_SERVER` | Memory manager threading modes |
| `NEWRTTINOTUSED` | Exclude Delphi 2010+ enhanced RTTI (smaller EXE) |
| `USE_OPENSSL` | Enable OpenSSL integration (required on POSIX) |

**Best Practice**: Set these in project options, not in unit source.

---

## 4.2. Unicode and UTF-8

mORMot 2 has 100% Unicode compatibility across all Delphi and FPC versions. From its core to its uppermost features, the framework is **natively UTF-8**, which is the de-facto character encoding for JSON, SQLite3, and most supported database engines.

### 4.2.1. String Types

The following string types are used throughout the framework:

| Type | Purpose | Location |
|------|---------|----------|
| `RawUtf8` | Primary type for all internal data (UTF-8 encoded) | `mormot.core.base` |
| `RawByteString` | Binary byte storage | `mormot.core.base` |
| `WinAnsiString` | WinAnsi-encoded text (code page 1252) | `mormot.core.base` |
| `SynUnicode` | Fastest native Unicode (`WideString` pre-2009, `UnicodeString` after) | `mormot.core.base` |
| `string` | Generic VCL/UI text (use only at presentation layer) | RTL |

**Key Recommendation**: Use `RawUtf8` for all business logic and data processing. Convert to `string` only at the UI layer using `Utf8ToString()` / `StringToUtf8()` from `mormot.core.unicode`:

```pascal
uses
  mormot.core.base,
  mormot.core.unicode;

var
  utf8: RawUtf8;
  display: string;
begin
  utf8 := 'Hello UTF-8 World';
  display := Utf8ToString(utf8);  // Convert for UI display
  utf8 := StringToUtf8(display);  // Convert back for storage/processing
end;
```

### 4.2.2. Why UTF-8?

- **JSON native**: All JSON is UTF-8 encoded
- **SQLite3 native**: SQLite3 stores text as UTF-8
- **Network efficient**: UTF-8 is compact for ASCII-heavy content
- **Memory efficient**: No temporary string conversions during parsing
- **Consistent**: Same encoding across all Delphi versions

---

## 4.3. Numeric Types

### 4.3.1. Currency Handling

The `currency` type is the standard Delphi type for monetary values, avoiding rounding errors with exact 4-decimal precision. It safely stores numbers in the range -922,337,203,685,477.5808 to 922,337,203,685,477.5807.

mORMot provides fast currency-to-text conversion functions in `mormot.core.text` that avoid FPU rounding issues:

```pascal
uses
  mormot.core.text;

var
  c: currency;
  s: RawUtf8;
begin
  c := 123.45;
  s := CurrencyToStr(c);  // Fast, no FPU rounding
  c := StrToCurrency(s);  // Safe conversion back
end;
```

The `Int64` binary representation of `currency` (i.e., `value * 10000`) is used internally for maximum performance and precision.

### 4.3.2. Cross-Platform Integer Types

| Type | Purpose |
|------|---------|
| `PtrInt` | Signed pointer-size integer (32 or 64 bit) |
| `PtrUInt` | Unsigned pointer-size integer |
| `TID` | 64-bit record ID (Int64) |

---

## 4.4. TDynArray: Dynamic Array Wrapper

> **Note:** `TDynArray` is a record/object type (stack-allocated), not a class. It wraps dynamic arrays without heap allocation overhead.

`TDynArray` (in `mormot.core.data`) provides `TList`-like functionality for any dynamic array:

### 4.4.1. Basic Usage

```pascal
uses
  mormot.core.base,
  mormot.core.data;

type
  TIntegerArray = array of Integer;

var
  arr: TIntegerArray;
  da: TDynArray;
  v: Integer;
begin
  da.Init(TypeInfo(TIntegerArray), arr);  // Associate wrapper with array

  for v := 1 to 1000 do
    da.Add(v);  // TList-like Add method

  da.Sort(SortDynArrayInteger);  // In-place sorting

  v := 500;
  if da.Find(v) >= 0 then  // Binary search (after sorting)
    WriteLn('Found 500');

  da.Delete(0);  // Delete by index
  WriteLn('Count: ', da.Count);
end;
```

### 4.4.2. External Count for Performance

For high-performance scenarios, use an external count variable to avoid reallocation on every Add/Delete:

```pascal
var
  arr: TIntegerArray;
  da: TDynArray;
  count: Integer;
begin
  da.Init(TypeInfo(TIntegerArray), arr, @count);  // External count
  da.Capacity := 10000;  // Pre-allocate memory

  // Now Add/Delete modify 'count' without reallocating 'arr'
  for i := 1 to 10000 do
    da.Add(i);  // Much faster with external count
end;
```

### 4.4.3. Serialization

`TDynArray` supports both binary and JSON serialization:

```pascal
var
  binary: RawByteString;
  json: RawUtf8;
begin
  // Binary (fast, compact)
  binary := da.SaveTo;
  da.LoadFromBinary(binary);

  // JSON (interoperable)
  json := da.SaveToJson;
  da.LoadFromJson(json);
end;
```

### 4.4.4. TDynArrayHashed for Dictionary-Like Access

`TDynArrayHashed` adds O(1) hash-based lookup:

```pascal
type
  TNameValue = record
    Name: RawUtf8;
    Value: Integer;
  end;
  TNameValueArray = array of TNameValue;

var
  arr: TNameValueArray;
  hash: TDynArrayHashed;
  added: Boolean;
  idx: Integer;
begin
  hash.Init(TypeInfo(TNameValueArray), arr);  // Auto-detects RawUtf8 key

  // Add or find existing
  idx := hash.FindHashedForAdding('MyKey', added);
  if added then
  begin
    arr[idx].Name := 'MyKey';
    arr[idx].Value := 42;
  end;

  // Fast lookup
  idx := hash.FindHashed('MyKey');  // O(1) instead of O(n)
end;
```

### 4.4.5. TSynDictionary

`TSynDictionary` (in `mormot.core.json`) is a **thread-safe** dictionary storing key-value pairs as two dynamic arrays:

```pascal
uses
  mormot.core.json;

var
  dict: TSynDictionary;
  key: RawUtf8;
  val, v: Integer;
begin
  dict := TSynDictionary.Create(TypeInfo(TRawUtf8DynArray), TypeInfo(TIntegerDynArray));
  try
    // Add() takes const parameters, so use variables
    key := 'key1'; val := 100;
    dict.Add(key, val);
    key := 'key2'; val := 200;
    dict.Add(key, val);

    key := 'key1';
    if dict.Exists(key) then
      dict.FindAndCopy(key, v);

    // Thread-safe by default (no external locking needed)
  finally
    dict.Free;
  end;
end;
```

---

## 4.5. TDocVariant: Schema-less Documents

`TDocVariant` (in `mormot.core.variants`) is a custom `variant` type for storing JSON-like documents:

### 4.5.1. Creating Documents

```pascal
uses
  mormot.core.variants;

var
  doc: Variant;
begin
  // Object document
  doc := _Obj(['name', 'John', 'age', 30]);
  // or from JSON
  doc := _Json('{"name":"John","age":30}');

  // Array document
  doc := _Arr(['apple', 'banana', 'cherry']);
  // or from JSON
  doc := _Json('["apple","banana","cherry"]');
end;
```

### 4.5.2. Late-Binding Access

```pascal
var
  doc: Variant;
begin
  doc := _Json('{"name":"John","address":{"city":"NYC","zip":"10001"}}');

  // Read properties via late-binding
  WriteLn(doc.name);           // 'John'
  WriteLn(doc.address.city);   // 'NYC'

  // Modify properties
  doc.name := 'Jane';
  doc.address.state := 'NY';   // Add new property

  // Convert to JSON
  WriteLn(doc);  // '{"name":"Jane","address":{"city":"NYC","zip":"10001","state":"NY"}}'
end;
```

### 4.5.3. Direct TDocVariantData Access

For better performance, use direct transtyping:

```pascal
var
  doc: Variant;
begin
  doc := _Json('{"a":1,"b":2,"c":3}');

  // Safe access via _Safe()
  with _Safe(doc)^ do
  begin
    WriteLn('Count: ', Count);
    for i := 0 to Count - 1 do
      WriteLn(Names[i], '=', Values[i]);
  end;

  // Typed property access
  WriteLn(_Safe(doc)^.I['a']);  // Integer access
  WriteLn(_Safe(doc)^.U['b']);  // RawUtf8 access
end;
```

### 4.5.4. Per-Value vs Per-Reference

By default, `_Obj()/_Arr()/_Json()` create **per-value** documents (deep copy on assignment):

```pascal
var
  v1, v2: Variant;
begin
  v1 := _Obj(['name', 'John']);
  v2 := v1;        // Creates a copy
  v2.name := 'Jane';
  WriteLn(v1.name);  // 'John' (unchanged)
  WriteLn(v2.name);  // 'Jane'
end;
```

Use `_ObjFast()/_ArrFast()/_JsonFast()` for **per-reference** documents (shared):

```pascal
var
  v1, v2: Variant;
begin
  v1 := _ObjFast(['name', 'John']);
  v2 := v1;        // Reference, not copy
  v2.name := 'Jane';
  WriteLn(v1.name);  // 'Jane' (both changed!)
  WriteLn(v2.name);  // 'Jane'
end;
```

---

## 4.6. Date and Time

### 4.6.1. ISO 8601 Encoding

Dates are encoded as ISO 8601 text (`YYYY-MM-DDThh:mm:ss`), which provides:
- Lexicographical ordering equals chronological ordering
- Natural sorting in file systems and databases
- Cross-platform compatibility

### 4.6.2. Time Types

| Type | Resolution | Storage | Use Case |
|------|------------|---------|----------|
| `TDateTime` | Seconds | TEXT (ISO 8601) | General purpose |
| `TDateTimeMS` | Milliseconds | TEXT (ISO 8601.sss) | High precision |
| `TTimeLog` | Seconds | INTEGER (bit-packed) | Fast comparison, compact storage |
| `TUnixTime` | Seconds | INTEGER | Unix timestamp since 1970 |
| `TUnixMSTime` | Milliseconds | INTEGER | JavaScript-compatible |

### 4.6.3. TTimeLog

`TTimeLog` (in `mormot.core.datetime`) is a proprietary 64-bit format optimized for fast comparison and compact storage:

```pascal
uses
  mormot.core.datetime;

var
  t, t1, t2: TTimeLog;
  dt: TDateTime;
begin
  t := TimeLogNow;                    // Current time
  t := TimeLogFromDateTime(Now);      // From TDateTime
  dt := TimeLogToDateTime(t);         // Back to TDateTime

  t1 := TimeLogNow;
  t2 := TimeLogNow;
  // Direct comparison works (chronological order)
  if t1 > t2 then
    WriteLn('t1 is later');

  // ISO 8601 conversion via TTimeLogBits
  WriteLn(PTimeLogBits(@t)^.Text(true, 'T'));  // '2025-01-15T10:30:45'
end;
```

### 4.6.4. Time Zones

`TSynTimeZone` (in `mormot.core.search`) handles time zone conversions:

```pascal
uses
  mormot.core.search;

var
  local: TDateTime;
begin
  // Convert UTC to local time for a specific zone
  local := TSynTimeZone.Default.UtcToLocal(NowUtc, 'Eastern Standard Time');

  // Get current local time for a zone
  local := TSynTimeZone.Default.NowToLocal('Pacific Standard Time');
end;
```

---

## 4.7. Thread Safety: TSynLocker

> **Note:** `TSynLocker` is a record/object type (stack-allocated), not a class. It provides lightweight synchronization primitives without heap allocation.

`TSynLocker` (in `mormot.core.os`) provides CPU cache-friendly critical sections:

### 4.7.1. Basic Usage

```pascal
uses
  mormot.core.os;

var
  Lock: TSynLocker;
  Counter: Integer;
begin
  Lock.Init;
  try
    // In thread code:
    Lock.Lock;
    try
      Inc(Counter);  // Protected access
    finally
      Lock.UnLock;
    end;
  finally
    Lock.Done;
  end;
end;
```

### 4.7.2. Automatic Unlocking

Use `ProtectMethod` for RAII-style protection:

```pascal
procedure TMyClass.ThreadSafeMethod;
begin
  fLock.ProtectMethod;  // Returns IUnknown, auto-unlocks at method end
  // ... protected code ...
end;  // Automatically unlocks here
```

### 4.7.3. Built-in Storage

`TSynLocker` includes 7 variant slots for thread-safe value storage:

```pascal
var
  Lock: TSynLocker;
begin
  Lock.Init;
  // Thread-safe integer storage
  Lock.LockedInt64[0] := 100;
  Lock.LockedInt64Increment(0, 1);  // Atomic increment

  // Thread-safe string storage
  Lock.LockedUtf8[1] := 'value';

  // Thread-safe variant storage
  Lock.Locked[2] := _Obj(['count', 42]);
end;
```

### 4.7.4. Thread-Safe Base Classes

Inherit from these for built-in `TSynLocker`:

| Class | Inherits From |
|-------|---------------|
| `TSynPersistentLock` | `TSynLocked` |
| `TInterfacedObjectLocked` | `TInterfacedPersistent` |
| `TSynObjectListLocked` | `TSynObjectList` |
| `TRawUtf8ListLocked` | `TRawUtf8List` |

---

## 4.8. Core Units Reference

### 4.8.1. Foundation Layer

| Unit | Purpose | Key Types |
|------|---------|-----------|
| `mormot.core.base` | Foundation types, ASM stubs | `RawUtf8`, `PtrInt`, `TDynArray` basics |
| `mormot.core.os` | OS abstraction | `TSynLocker`, `GetTickCount64`, file/process APIs |
| `mormot.core.unicode` | Charset conversion | `Utf8ToString`, `WinAnsiToUtf8` |
| `mormot.core.text` | Text processing | `FormatUtf8`, CSV parsing, currency |
| `mormot.core.datetime` | Date/time handling | `TTimeLog`, ISO 8601, `TSynTimeZone` |

### 4.8.2. Data Layer

| Unit | Purpose | Key Types |
|------|---------|-----------|
| `mormot.core.rtti` | RTTI abstraction | `TRttiCustom`, `PRttiInfo` |
| `mormot.core.buffers` | Compression, streams | SynLZ, Base64, `TBufferWriter` |
| `mormot.core.data` | Data structures | `TDynArray`, `TDynArrayHashed` |
| `mormot.core.json` | JSON handling | `TJsonWriter`, `GetJsonField`, `TSynDictionary` |
| `mormot.core.variants` | Dynamic documents | `TDocVariant`, `IDocDict`, `IDocList` |

### 4.8.3. Application Layer

| Unit | Purpose | Key Types |
|------|---------|-----------|
| `mormot.core.log` | Logging framework | `TSynLog`, `ISynLog` |
| `mormot.core.perf` | Performance monitoring | `TSynMonitor`, timing |
| `mormot.core.threads` | Threading utilities | `TSynBackgroundThread` (Abstract, Method, Event variants), `TSynParallelProcess` |
| `mormot.core.search` | Search and filtering | Full-text search helpers |
| `mormot.core.test` | Testing framework | `TSynTestCase` |
| `mormot.core.mustache` | Template engine | `TSynMustache` |
| `mormot.core.interfaces` | Interface support | DI/IoC container |
| `mormot.core.zip` | ZIP compression | Archive handling |

> **Note:** The tables above list commonly used core units (18 of 24 total). Specialized units not shown include:
> - `mormot.core.collections` - Additional collection types
> - `mormot.core.os.security` - OS security APIs
> - `mormot.core.os.mac` - macOS-specific functionality
> - `mormot.core.mvc` - Model-View-Controller patterns
> - `mormot.core.fpclibcmm` - FPC LibC memory manager
> - `mormot.core.fpcx64mm` - FPC x64 memory manager

---

## 4.9. Dependency Order

Core units have strict dependencies (lower never depends on higher):

```
mormot.core.base (RTL types, ASM - no dependencies)
  └─► mormot.core.os (OS abstraction)
      └─► mormot.core.unicode (encoding)
          └─► mormot.core.text (parsing)
              └─► mormot.core.datetime (dates)
                  └─► mormot.core.rtti (RTTI)
                      └─► mormot.core.buffers (compression)
                          └─► mormot.core.data (TDynArray)
                              └─► mormot.core.json (JSON)
                                  └─► [variants, log, threads, etc.]
```

**Critical Rule**: When modifying units, respect this hierarchy. Adding references that create circular dependencies will break compilation.

---

## 4.10. Migration from SynCommons.pas

### 4.10.1. Type Mapping

| mORMot 1 | mORMot 2 | Notes |
|----------|----------|-------|
| `RawUTF8` | `RawUtf8` | Case change only |
| `SynCommons.pas` | `mormot.core.*` | Split into 24 units |
| `FormatUTF8()` | `FormatUtf8()` | Same function, case change |
| `TDocVariant` | `TDocVariant` | Now in `mormot.core.variants` |
| `TSynLog` | `TSynLog` | Now in `mormot.core.log` |
| `TDynArray` | `TDynArray` | Now in `mormot.core.data` |

### 4.10.2. Backward Compatibility

By default, mORMot 2 provides compatibility aliases. Define `PUREMORMOT2` to disable them and use only new names.

### 4.10.3. Minimal Uses Clause

```pascal
uses
  mormot.core.base,      // Foundation
  mormot.core.os,        // OS abstraction
  mormot.core.text,      // Text utilities
  mormot.core.json,      // JSON
  mormot.core.variants;  // TDocVariant
```

---

*Next Chapter: Object-Relational Mapping (TOrm, TOrmModel)*

---

## Navigation

| Previous | Index | Next |
|----------|-------|------|
| [Chapter 3: Meet mORMot 2](mORMot2-SAD-Chapter-03.md) | [Index](mORMot2-SAD-Index.md) | [Chapter 5: Object-Relational Mapping](mORMot2-SAD-Chapter-05.md) |
