# 10. JSON and RESTful Fundamentals

*The Language of mORMot*

Before exploring the Client-Server architecture, we need to understand the two key standards that mORMot builds upon: JSON for data interchange and REST for API design.

---

## 10.1. JSON in mORMot

### 10.1.1. Why JSON?

mORMot uses JSON (JavaScript Object Notation) as its primary data format:

| Advantage | Description |
|-----------|-------------|
| Human-readable | Easy to debug and inspect |
| Compact | Smaller than XML for most use cases |
| Fast parsing | In-place parsing without memory allocation |
| Native UTF-8 | Matches SQLite3 and web standards |
| Universal | Supported by all languages/platforms |
| AJAX-ready | Native to JavaScript/browser apps |

### 10.1.2. JSON Types

```json
{
  "string": "Hello UTF-8 World",
  "number": 42,
  "float": 3.14159,
  "boolean": true,
  "null": null,
  "array": [1, 2, 3],
  "object": {"nested": "value"}
}
```

### 10.1.3. mORMot JSON Extensions

mORMot follows the JSON standard with some extensions:

- **Extended syntax**: Unquoted ASCII property names (MongoDB-style)
- **64-bit integers**: Full `Int64` support (no JavaScript 53-bit limit)
- **Binary data**: Base64 encoding for BLOBs
- **Custom types**: `TDateTime`, `TTimeLog`, `Currency` serialization

```pascal
// Extended syntax (valid in mORMot)
{name: "John", age: 30}

// Standard JSON (always valid)
{"name": "John", "age": 30}
```

---

## 10.2. JSON Serialization

### 10.2.1. Basic Types

```pascal
uses
  mormot.core.json;

var
  i: Integer;
  d: Double;
  s: RawUtf8;
  json: RawUtf8;
begin
  // To JSON
  json := FormatJson('{name:?,age:?,score:?}', [], ['John', 30, 95.5]);
  // Result: {"name":"John","age":30,"score":95.5}

  // From JSON
  JsonDecode(pointer(json), ['name', 'age', 'score'], @Values);
end;
```

### 10.2.2. Record Serialization

Records are automatically serialized via RTTI (Delphi 2010+):

```pascal
type
  TPerson = record
    Name: RawUtf8;
    Age: Integer;
    Email: RawUtf8;
  end;

var
  Person: TPerson;
  json: RawUtf8;
begin
  Person.Name := 'John';
  Person.Age := 30;
  Person.Email := 'john@example.com';

  // Record to JSON
  json := RecordSaveJson(Person, TypeInfo(TPerson));
  // Result: {"Name":"John","Age":30,"Email":"john@example.com"}

  // JSON to Record
  RecordLoadJson(Person, pointer(json), TypeInfo(TPerson));
end;
```

### 10.2.3. Dynamic Arrays

```pascal
type
  TPersonArray = array of TPerson;

var
  People: TPersonArray;
  json: RawUtf8;
begin
  SetLength(People, 2);
  People[0].Name := 'John';
  People[1].Name := 'Jane';

  // Array to JSON
  json := DynArraySaveJson(People, TypeInfo(TPersonArray));
  // Result: [{"Name":"John",...},{"Name":"Jane",...}]

  // JSON to Array
  DynArrayLoadJson(People, pointer(json), TypeInfo(TPersonArray));
end;
```

### 10.2.4. TDocVariant (Schema-less)

For flexible JSON handling:

```pascal
var
  doc: Variant;
begin
  // Create from JSON
  doc := _JsonFast('{"name":"John","tags":["admin","user"]}');

  // Access via late-binding
  WriteLn(doc.name);           // 'John'
  WriteLn(doc.tags._Count);    // 2 (underscore convention for properties)

  // Modify
  doc.email := 'john@example.com';
  doc.tags.Add('moderator');   // No underscore for methods

  // Back to JSON
  WriteLn(doc);  // Full JSON string
end;
```

### 10.2.5. Class Serialization

Classes are serialized via `published` properties:

```pascal
type
  TMyClass = class(TSynPersistent)
  private
    fName: RawUtf8;
    fValue: Integer;
  published
    property Name: RawUtf8 read fName write fName;
    property Value: Integer read fValue write fValue;
  end;

var
  Obj: TMyClass;
  json: RawUtf8;
begin
  Obj := TMyClass.Create;
  Obj.Name := 'Test';
  Obj.Value := 42;

  json := ObjectToJson(Obj);
  // Result: {"Name":"Test","Value":42}

  JsonToObject(Obj, pointer(json), Valid);
end;
```

---

## 10.3. REST Architecture

### 10.3.1. What is REST?

REST (Representational State Transfer) defines how resources are addressed and manipulated over HTTP:

| HTTP Method | CRUD | ORM Method | Description |
|-------------|------|------------|-------------|
| `GET` | Read | `Retrieve()` | Get resource(s) |
| `POST` | Create | `Add()` | Create new resource |
| `PUT` | Update | `Update()` | Update existing resource |
| `DELETE` | Delete | `Delete()` | Remove resource |

### 10.3.2. mORMot URI Structure

```
http://server:port/root/TableName/ID
                   │     │         │
                   │     │         └── Record ID (optional)
                   │     └── TOrm class name
                   └── Model.Root
```

**Examples:**
```
GET    /api/Customer          → List all customers
GET    /api/Customer/123      → Get customer #123
POST   /api/Customer          → Create new customer (body = JSON)
PUT    /api/Customer/123      → Update customer #123
DELETE /api/Customer/123      → Delete customer #123
```

### 10.3.3. REST vs RPC in mORMot

mORMot is **REST-oriented** but supports both paradigms:

| Feature | REST Style | RPC Style |
|---------|------------|-----------|
| ORM operations | URI + HTTP verbs | URI + HTTP verbs |
| Method services | `GET/POST /root/Method` | `POST /root/Method` |
| Interface services | `/root/Interface.Method` | JSON-RPC body |

**Important**: mORMot prefers interface-based services (RPC-style) for business logic, using REST primarily for ORM operations.

---

## 10.4. JSON in ORM

### 10.4.1. TOrm to JSON

```pascal
var
  Customer: TOrmCustomer;
  json: RawUtf8;
begin
  Customer := TOrmCustomer.Create;
  Customer.Name := 'ACME Corp';
  Customer.Email := 'contact@acme.com';

  // Single record
  json := Customer.GetJsonValues(True, True, ooSelect);
  // Result: {"ID":0,"Name":"ACME Corp","Email":"contact@acme.com"}

  // Selected fields only
  json := Customer.GetJsonValues(True, True, ooSelect, 'Name,Email');
end;
```

### 10.4.2. JSON to TOrm

```pascal
var
  Customer: TOrmCustomer;
begin
  Customer := TOrmCustomer.Create;
  Customer.FillFrom('{"Name":"ACME Corp","Email":"contact@acme.com"}');
end;
```

### 10.4.3. Query Results as JSON

```pascal
var
  json: RawUtf8;
begin
  // Direct JSON from query
  json := Server.Orm.RetrieveListJson(TOrmCustomer,
    'Country = ?', ['USA'], 'Name,Email');
  // Result: [{"Name":"John","Email":"john@..."},...]
end;
```

---

## 10.5. JSON in Services

### 10.5.1. Method-Based Services

```pascal
type
  TMyServer = class(TRestServerDB)
  published
    procedure Sum(Ctxt: TRestServerUriContext);
  end;

procedure TMyServer.Sum(Ctxt: TRestServerUriContext);
var
  A, B: Integer;
begin
  // InputInt[] returns Int64, use Integer() cast for smaller types
  A := Integer(Ctxt.InputInt['a']);
  B := Integer(Ctxt.InputInt['b']);
  Ctxt.Returns(['result', A + B]);
end;

// Client call: GET /api/Sum?a=10&b=20
// Response: {"result":30}
```

### 10.5.2. Interface-Based Services

```pascal
type
  ICalculator = interface(IInvokable)
    ['{...}']
    function Add(A, B: Integer): Integer;
  end;

// Server registration
Server.ServiceDefine(TCalculator, [ICalculator], sicShared);

// Client call (automatic JSON marshalling)
var
  Calc: ICalculator;
begin
  Client.Services.Resolve(ICalculator, Calc);
  Result := Calc.Add(10, 20);  // JSON: {"result":30}
end;
```

---

## 10.6. Binary Alternatives

### 10.6.1. When to Use Binary

| Format | Use Case | Trade-off |
|--------|----------|-----------|
| JSON | Interoperability, debugging | Larger, slower parsing |
| Binary | Internal, large data | Smaller, faster |
| SynLZ+JSON | Compression over network | Best of both |

### 10.6.2. SynLZ Compression

mORMot clients automatically negotiate compression:

```pascal
// Client-side compression settings
Client := TRestHttpClient.Create(...);
Client.Compression := [hcSynLZ, hcDeflate];  // Client property, not server

// Server automatically handles compression negotiation
// SynLZ is 20x faster than Deflate for compression
// Delphi clients use SynLZ automatically
// AJAX clients fall back to Deflate
```

### 10.6.3. Binary Serialization

```pascal
// Binary record save (faster than JSON)
Binary := RecordSave(Person, TypeInfo(TPerson));
RecordLoad(Person, pointer(Binary), TypeInfo(TPerson));

// Binary with Base64 encoding (URI-safe format)
Binary := RecordSaveBase64(Person, TypeInfo(TPerson), True);  // UriCompatible=True
```

---

## 10.7. JSON Performance Tips

### 10.7.1. Avoid Unnecessary Conversions

```pascal
// SLOW: Multiple conversions
s := Utf8ToString(json);
json2 := StringToUtf8(s);

// FAST: Stay in UTF-8
ProcessRawUtf8(json);
```

### 10.7.2. Use Typed Helpers

```pascal
// SLOW: Variant access
value := doc.field;

// FAST: Direct typed access
value := _Safe(doc)^.I['field'];  // Integer
value := _Safe(doc)^.U['field'];  // RawUtf8
```

### 10.7.3. Reuse Objects

```pascal
// SLOW: Create/Free per iteration
for i := 1 to 1000 do
begin
  Obj := TMyClass.Create;
  try
    JsonToObject(Obj, pointer(json[i]), Valid);
    Process(Obj);
  finally
    Obj.Free;
  end;
end;

// FAST: Reuse single instance
Obj := TMyClass.Create;
try
  for i := 1 to 1000 do
  begin
    Obj.ClearProperties;  // Reset fields
    JsonToObject(Obj, pointer(json[i]), Valid);
    Process(Obj);
  end;
finally
  Obj.Free;
end;
```

---

## 10.8. Migration from mORMot 1

### 10.8.1. Function Renames

| mORMot 1 | mORMot 2 |
|----------|----------|
| `JSONToObject()` | `JsonToObject()` |
| `ObjectToJSON()` | `ObjectToJson()` |
| `RecordSaveJSON()` | `RecordSaveJson()` |
| `RecordLoadJSON()` | `RecordLoadJson()` |
| `DynArraySaveJSON()` | `DynArraySaveJson()` |
| `JSONDecode()` | `JsonDecode()` |

### 10.8.2. Unit Locations

| Feature | mORMot 2 Unit |
|---------|---------------|
| JSON parsing | `mormot.core.json` |
| TDocVariant | `mormot.core.variants` |
| Record serialization | `mormot.core.rtti` |
| REST HTTP client/server | `mormot.rest.http.client` / `mormot.rest.http.server` |

---

*Next Chapter: Client-Server Architecture*

---

## Navigation

| Previous | Index | Next |
|----------|-------|------|
| [Chapter 9: External NoSQL Database Access](mORMot2-SAD-Chapter-09.md) | [Index](mORMot2-SAD-Index.md) | [Chapter 11: Client-Server Architecture](mORMot2-SAD-Chapter-11.md) |
