# 31-simple_api_using_datasets - Dataset-Based Customer API

**Port of**: DMVCFramework `samples/simple_api_using_datasets`
**Difficulty**: Low
**Demonstrates**: Direct SQL queries, dataset-like approach without ORM

## Overview

This sample demonstrates a dataset-based approach to building REST APIs, similar to traditional Delphi database programming with SQL queries. Instead of using ORM entities, operations are performed using direct SQL statements.

## Key Differences from ORM Approach

| Aspect | ORM (Sample 30) | Dataset (Sample 31) |
|--------|----------------|---------------------|
| **Entity Usage** | TOrm descendants | No entities (SQL only) |
| **CRUD** | `Add()`, `Update()`, `Delete()` | `INSERT`, `UPDATE`, `DELETE` |
| **Queries** | `RetrieveListObjArray()` | `ExecuteJson()` |
| **Flexibility** | Type-safe, validated | Direct SQL, flexible |
| **Learning Curve** | Learn ORM concepts | Familiar SQL |

## Building & Running

```bash
# Compile
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe 31-simple_api_using_datasets.dproj

# Run
31-simple_api_using_datasets.exe
```

## API Endpoints

### Basic CRUD Operations

```bash
# Get all customers
curl http://localhost:8080/customerdata/CustomerDataApi/GetCustomers

# Get customer by ID
curl http://localhost:8080/customerdata/CustomerDataApi/GetCustomerById?id=1

# Create customer
curl -X POST http://localhost:8080/customerdata/CustomerDataApi/CreateCustomer \
  -H "Content-Type: application/json" \
  -d '{"code":"TEST","description":"Test Corp","city":"Boston","rating":4,"note":"New"}'

# Update customer
curl -X POST http://localhost:8080/customerdata/CustomerDataApi/UpdateCustomer \
  -H "Content-Type: application/json" \
  -d '{"id":1,"code":"TEST","description":"Updated","city":"Boston","rating":5,"note":"Updated"}'

# Delete customer
curl -X DELETE http://localhost:8080/customerdata/CustomerDataApi/DeleteCustomer?id=1
```

### TDocVariant Demonstration Endpoints

```bash
# Get customer statistics with nested objects/arrays
curl http://localhost:8080/customerdata/CustomerDataApi/GetCustomerStatistics

# Get customers with computed fields and nested data
curl http://localhost:8080/customerdata/CustomerDataApi/GetCustomersWithDetails

# View TDocVariant manipulation patterns reference
curl http://localhost:8080/customerdata/CustomerDataApi/DemoDocVariantPatterns
```

## DMVC → mORMot2 Mapping

| DMVC | mORMot2 |
|------|---------|
| `lQuery.Open('SELECT ...')` | `Server.Orm.ExecuteJson([], 'SELECT ...')` |
| `lQuery.ExecSQL('INSERT ...')` | `Server.Orm.ExecuteNoResult([], 'INSERT ...')` |
| `lQuery.ParamByName('x').AsString` | Bind parameters: `[value1, value2]` |
| `lQuery.FieldByName('x').AsInteger` | Extract from variant: `DocVariantData(v)^.I['x']` |
| `Render(ObjectDict().Add('data', lQuery))` | Return `TVariantDynArray` |

## Implementation Highlights

### Direct SQL Queries

```pascal
// Get all customers
json := Server.Orm.ExecuteJson([],
  'SELECT RowID as ID, Code, CompanyName, City, Rating, Note ' +
  'FROM CustomerOrm ORDER BY Code');
Result := JsonArrayToVariants(json);

// Get customer by ID with parameter
json := Server.Orm.ExecuteJson([],
  'SELECT ... FROM CustomerOrm WHERE RowID=?', [], [id]);
```

### SQL INSERT with Parameters

```pascal
Server.Orm.ExecuteNoResult([],
  'INSERT INTO CustomerOrm (Code, CompanyName, City, Rating, Note) ' +
  'VALUES (?, ?, ?, ?, ?)', [], [code, description, city, rating, note]);

// Get last inserted ID
json := Server.Orm.ExecuteJson([],
  'SELECT last_insert_rowid() as ID', []);
```

### SQL UPDATE/DELETE

```pascal
// Update
Server.Orm.ExecuteNoResult([],
  'UPDATE CustomerOrm SET Code=?, CompanyName=? WHERE RowID=?',
  [], [code, description, id]);

// Delete
Server.Orm.ExecuteNoResult([],
  'DELETE FROM CustomerOrm WHERE RowID=?', [], [id]);
```

## When to Use This Approach

**Use Dataset/SQL approach when:**
- ✅ You're migrating from traditional Delphi database code
- ✅ Complex SQL queries with joins, aggregates, etc.
- ✅ Need full control over SQL
- ✅ Working with legacy database schemas
- ✅ Team is more familiar with SQL than ORM

**Use ORM approach (Sample 30) when:**
- ✅ Starting new project
- ✅ Simple CRUD operations
- ✅ Want type safety and validation
- ✅ Need cross-database portability
- ✅ Prefer object-oriented approach

## TDocVariant Manipulation Patterns

This example includes comprehensive demonstrations of TDocVariant manipulation beyond basic JSON parsing.

### Pattern 1: Building Complex Objects with TDocVariantData

```pascal
function GetCustomerStatistics: Variant;
var
  doc: TDocVariantData;
begin
  doc.InitFast;  // Stack-allocated, faster than variant

  // Set properties using typed accessors (faster than variant access)
  doc.I['totalCustomers'] := 100;
  doc.U['generatedAt'] := DateTimeToIso8601Text(NowUtc);
  doc.B['success'] := true;

  // Create nested object
  doc.O_['countsByRating']^.I['rating_5'] := 25;

  // Create nested array
  doc.A_['topCities']^.InitFast(dvArray);
  doc.A_['topCities']^.AddItem(cityObject);

  Result := variant(doc);
end;
```

**Key Benefits:**
- ✅ Stack-allocated for better performance
- ✅ Typed property access (`I[]`, `U[]`, `B[]`, `D[]`) faster than variant late-binding
- ✅ `O_[]` and `A_[]` properties auto-create nested objects/arrays

### Pattern 2: Enhancing Query Results

```pascal
function GetCustomersWithDetails: TVariantDynArray;
var
  arr: TVariantDynArray;
begin
  // Get data from SQL
  arr := JsonToVariantDynArray(
    Server.Orm.ExecuteJson([], 'SELECT ...'));

  // Enhance each item
  for i := 0 to High(arr) do
    with DocVariantData(arr[i])^ do
    begin
      // Add computed fields
      U['displayName'] := FormatUtf8('% - %', [U['Code'], U['CompanyName']]);
      B['isPremium'] := I['Rating'] >= 4;

      // Add nested arrays
      A_['tags']^.AddItem('premium');

      // Add nested objects
      O_['metadata']^.U['source'] := 'dataset-api';
    end;

  Result := arr;
end;
```

**Key Benefits:**
- ✅ Modify existing JSON results without re-parsing
- ✅ Add computed fields efficiently
- ✅ Build complex nested structures on-the-fly

### Pattern 3: Property Access Comparison

| Method | Type | Speed | Use Case |
|--------|------|-------|----------|
| `Value[name]` | variant | Slow | Late binding, generic access |
| `I[name]` | Int64 | Fast | Integer values |
| `U[name]` | RawUtf8 | Fast | String values (UTF-8) |
| `B[name]` | Boolean | Fast | Boolean values |
| `D[name]` | Double | Fast | Float values |
| `O[name]` | PDocVariantData | Fast | Nested objects (read) |
| `O_[name]` | PDocVariantData | Fast | Nested objects (read/create) |
| `A[name]` | PDocVariantData | Fast | Nested arrays (read) |
| `A_[name]` | PDocVariantData | Fast | Nested arrays (read/create) |

### Pattern 4: Direct Array Access (Maximum Performance)

```pascal
// Slow - uses variant dispatch
for i := 0 to doc.Count-1 do
  writeln(doc.Value[i]);

// Fast - direct array access
for i := 0 to doc.Count-1 do
  writeln(doc.Values[i]);

// Fastest - access both names and values
with _Safe(v)^ do
  for i := 0 to Count-1 do
    writeln(Names[i], '=', Values[i]);
```

### Pattern 5: Safe Operations with _Safe()

```pascal
// Unsafe - raises exception if v is not TDocVariant
with DocVariantData(v)^ do
  result := I['count'];

// Safe - returns fake empty instance if not TDocVariant
with _Safe(v)^ do
  result := I['count'];  // Returns 0 if property doesn't exist
```

### Pattern 6: Initialization Patterns

```pascal
// From JSON string
v := _Json('{"name":"John"}');
v := _JsonFast('{"arr":[1,2,3]}');  // By-reference, faster

// From name/value pairs
v := TDocVariant.NewObject(['name','John','age',42]);

// Stack-allocated (fastest)
var doc: TDocVariantData;
doc.InitFast;
doc.InitObject(['name','John','age',42]);
doc.InitArray([1, 2, 3]);
```

### When to Use Each Pattern

**Use `TDocVariantData` (stack-allocated) when:**
- ✅ Building response objects from scratch
- ✅ Performance is critical
- ✅ Working in tight loops
- ✅ Memory allocation is a concern

**Use `variant` (heap-allocated) when:**
- ✅ Parsing JSON from ExecuteJson
- ✅ Need to pass around objects
- ✅ Working with existing JSON data
- ✅ Interface requires variant return type

**Use typed properties (`I[]`, `U[]`, etc) when:**
- ✅ You know the field type
- ✅ Performance matters
- ✅ Reading from ExecuteJson results

**Use direct array access when:**
- ✅ Iterating over large datasets
- ✅ Maximum performance required
- ✅ Accessing both names and values

### Demo Endpoint

The `DemoDocVariantPatterns` endpoint returns a comprehensive reference showing all these patterns with code examples and recommendations.

```bash
curl http://localhost:8080/customerdata/CustomerDataApi/DemoDocVariantPatterns | jq
```

## See Also

- **Sample 30** - ORM-based approach (recommended for new projects)
- **Sample 28** - Full ActiveRecord CRUD
- **Sample 05** - Dataset rendering examples
- **mormot.core.variants.pas** - TDocVariant implementation (900MB/s JSON)

---

**Created**: 2025-12-20
**Updated**: 2025-12-23 (Added TDocVariant manipulation patterns)
**Note**: This approach is closer to traditional Delphi database programming
