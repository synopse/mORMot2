# 9. External NoSQL Database Access

*MongoDB and Object-Document Mapping*

mORMot provides native access to NoSQL databases, with MongoDB as the primary supported engine. The ORM seamlessly transforms into an ODM (Object-Document Mapping) when working with document stores.

---

## 9.1. NoSQL Overview

### 9.1.1. Supported NoSQL Engines

| Engine | Unit | Description |
|--------|------|-------------|
| MongoDB | `mormot.db.nosql.mongodb` | Full ODM support |
| In-Memory | `mormot.orm.storage` | `TObjectList` with JSON/binary persistence |

### 9.1.2. MongoDB Advantages

- **Schema flexibility**: Documents can have varying structures
- **Horizontal scaling**: Built-in sharding and replication
- **Document model**: Natural fit for mORMot's `TDocVariant`
- **High performance**: Excellent for write-heavy workloads
- **JSON native**: Direct integration with mORMot's JSON handling

---

## 9.2. MongoDB Client

### 9.2.1. Unit Structure

```
mormot.db.nosql.bson.pas     → BSON encoding/decoding
        ↓
mormot.db.nosql.mongodb.pas  → MongoDB wire protocol client
        ↓
mormot.orm.mongodb.pas       → ORM/ODM integration
```

### 9.2.2. Connecting to MongoDB

**Basic connection:**
```pascal
uses
  mormot.db.nosql.mongodb;

var
  Client: TMongoClient;
  DB: TMongoDatabase;
begin
  Client := TMongoClient.Create('localhost', 27017);
  try
    DB := Client.Database['mydb'];
    // Use DB...
  finally
    Client.Free;
  end;
end;
```

**With authentication (SCRAM-SHA-1):**
```pascal
var
  Client: TMongoClient;
  DB: TMongoDatabase;
begin
  Client := TMongoClient.Create('localhost', 27017);
  try
    // Authenticate and get database
    DB := Client.OpenAuth('mydb', 'username', 'password');
    // Use DB...
  finally
    Client.Free;
  end;
end;
```

**Replica set connection:**
```pascal
Client := TMongoClient.Create(
  'mongodb://host1:27017,host2:27017,host3:27017/?replicaSet=myReplicaSet');
```

### 9.2.3. Connection Options

```pascal
Client := TMongoClient.Create('localhost', 27017);

// Write concern settings
Client.WriteConcern := wcAcknowledged;      // Default - wait for ack
Client.WriteConcern := wcUnacknowledged;    // Fire and forget (fastest)
Client.WriteConcern := wcMajority;          // Wait for majority

// Read preference
Client.ReadPreference := rpPrimary;         // Always read from primary
Client.ReadPreference := rpSecondary;       // Read from secondaries
Client.ReadPreference := rpNearest;         // Nearest server
```

---

## 9.3. BSON and TDocVariant

### 9.3.1. BSON Types

MongoDB uses BSON (Binary JSON), which extends JSON with additional types:

| BSON Type | Delphi Representation |
|-----------|----------------------|
| Double | `Double` |
| String | `RawUtf8` |
| Document | `TDocVariant` |
| Array | `TDocVariant` (array mode) |
| Binary | `RawByteString` |
| ObjectId | `TBsonObjectID` |
| Boolean | `Boolean` |
| DateTime | `TDateTime` |
| Null | `Null` variant |
| Int32 | `Integer` |
| Int64 | `Int64` |
| Decimal128 | `TDecimal128` |

### 9.3.2. TDocVariant Integration

`TDocVariant` seamlessly maps to MongoDB documents:

```pascal
var
  Doc: Variant;
begin
  // Create document with late-binding
  TDocVariant.New(Doc);
  Doc.name := 'John Doe';
  Doc.email := 'john@example.com';
  Doc.age := 30;
  Doc.tags := _Arr(['developer', 'delphi', 'mongodb']);
  Doc.address := _Obj([
    'street', '123 Main St',
    'city', 'New York',
    'zip', '10001'
  ]);

  // Save to MongoDB
  Coll.Insert(Doc);
end;
```

### 9.3.3. ObjectID Generation

```pascal
var
  ID: TBsonObjectID;
begin
  // Generate new ObjectID (client-side)
  ID.ComputeNew;  // Use ComputeNew method on record

  // ObjectID contains timestamp
  WriteLn('Created at: ', DateTimeToStr(ID.CreateDateTime));

  // Use in document
  Doc._id := ID.ToVariant;
  Coll.Insert(Doc);
end;
```

---

## 9.4. Collection Operations

### 9.4.1. Getting a Collection

```pascal
var
  Coll: TMongoCollection;
begin
  // Get or create collection
  Coll := DB.CollectionOrCreate['customers'];

  // Get existing collection
  Coll := DB.Collection['customers'];
end;
```

### 9.4.2. Insert Operations

**Single document:**
```pascal
var
  Doc: Variant;
begin
  Doc := _ObjFast([
    'name', 'John Doe',
    'email', 'john@example.com',
    'age', 30
  ]);

  Coll.Insert(Doc);
  WriteLn('Inserted with _id: ', Doc._id);  // Auto-generated ObjectID
end;
```

**Bulk insert (much faster):**
```pascal
var
  Docs: TVariantDynArray;
  i: Integer;
begin
  SetLength(Docs, 10000);
  for i := 0 to High(Docs) do
  begin
    ID.ComputeNew;  // Generate new ObjectID
    Docs[i] := _ObjFast([
      '_id', ID.ToVariant,
      'index', i,
      'data', FormatUtf8('Record %', [i])
    ]);
  end;

  Coll.Insert(Docs);  // Single network roundtrip!
end;
```

### 9.4.3. Find Operations

**Find one document:**
```pascal
var
  Doc: Variant;
begin
  // By _id
  Doc := Coll.FindOne(ObjectID);
  Doc := Coll.FindOne(123);  // Integer _id

  // By query
  Doc := Coll.FindDoc('{name:?}', ['John']);
  Doc := Coll.FindDoc('{age:{$gt:?}}', [21]);
end;
```

**Find multiple documents:**
```pascal
var
  Docs: TVariantDynArray;
  Doc: Variant;
begin
  // Get all matching documents
  Coll.FindDocs('{status:?}', ['active'], Docs);

  for Doc in Docs do
    WriteLn(Doc.name);
end;
```

**Find with projection:**
```pascal
var
  Json: RawUtf8;
begin
  // Return only specific fields
  Json := Coll.FindJson(
    '{status:?}',        // Query
    ['active'],          // Parameters
    '{name:1,email:1}'   // Projection (include name and email)
  );
end;
```

**Iterating multiple documents:**
```pascal
var
  Docs: TVariantDynArray;
  Doc: Variant;
begin
  // FindDocs fills array with all matching documents
  Coll.FindDocs('{age:{$gte:?}}', [18], Docs, Null);
  for Doc in Docs do
    WriteLn(Doc.name);
end;
```

> **Note**: mORMot2 MongoDB uses array-based retrieval (`FindDocs`) rather than cursor iteration. For large result sets, use pagination with `NumberToReturn` and `NumberToSkip` parameters.

### 9.4.4. Update Operations

**Replace document:**
```pascal
var
  Doc: Variant;
begin
  Doc := Coll.FindOne(123);
  Doc.status := 'updated';
  Coll.Save(Doc);  // Replace entire document
end;
```

**Partial update ($set):**
```pascal
// Update specific fields only
Coll.Update(
  '{_id:?}', [123],           // Query
  '{$set:{status:?,updated:?}}', ['active', Now]  // Update
);
```

**Update multiple documents:**
```pascal
// Set all inactive users to archived
Coll.UpdateMany(
  '{status:?}', ['inactive'],
  '{$set:{archived:?}}', [True]
);
```

**Upsert (insert if not exists):**
```pascal
Coll.Update(
  '{email:?}', ['john@example.com'],
  '{$set:{name:?,lastSeen:?}}', ['John', Now],
  [mufUpsert]  // Create if not found
);
```

### 9.4.5. Delete Operations

**Delete one:**
```pascal
Coll.Remove('{_id:?}', [ObjectID]);
Coll.RemoveOne(123);  // By _id
```

**Delete many:**
```pascal
Coll.Remove('{status:?}', ['deleted']);  // Delete all matching
```

**Bulk delete:**
```pascal
var
  IDs: TBsonObjectIDDynArray;
begin
  // Much faster than individual deletes
  Coll.Remove('{_id:{$in:?}}', [IDs]);
end;
```

---

## 9.5. MongoDB Query Language

### 9.5.1. Comparison Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `$eq` | Equal | `{age:{$eq:30}}` |
| `$ne` | Not equal | `{status:{$ne:'deleted'}}` |
| `$gt` | Greater than | `{age:{$gt:21}}` |
| `$gte` | Greater or equal | `{age:{$gte:18}}` |
| `$lt` | Less than | `{price:{$lt:100}}` |
| `$lte` | Less or equal | `{stock:{$lte:10}}` |
| `$in` | In array | `{status:{$in:['active','pending']}}` |
| `$nin` | Not in array | `{role:{$nin:['admin']}}` |

### 9.5.2. Logical Operators

```pascal
// AND (implicit)
Coll.FindDocs('{age:{$gte:?},status:?}', [18, 'active'], Docs);

// AND (explicit)
Coll.FindDocs('{$and:[{age:{$gte:?}},{age:{$lt:?}}]}', [18, 65], Docs);

// OR
Coll.FindDocs('{$or:[{status:?},{priority:{$gt:?}}]}', ['urgent', 5], Docs);

// NOT
Coll.FindDocs('{age:{$not:{$lt:?}}}', [18], Docs);
```

### 9.5.3. Element Operators

```pascal
// Field exists
Coll.FindDocs('{email:{$exists:true}}', [], Docs);

// Type check
Coll.FindDocs('{age:{$type:"int"}}', [], Docs);
```

### 9.5.4. Array Operators

```pascal
// Element in array
Coll.FindDocs('{tags:?}', ['mongodb'], Docs);

// All elements match
Coll.FindDocs('{tags:{$all:?}}', [_Arr(['mongodb','delphi'])], Docs);

// Array size
Coll.FindDocs('{tags:{$size:?}}', [3], Docs);

// Element match
Coll.FindDocs('{items:{$elemMatch:{qty:{$gt:?},price:{$lt:?}}}}', [10, 100], Docs);
```

### 9.5.5. Text Search

```pascal
// Create text index first
Coll.EnsureIndex('{content:"text"}');

// Text search
Coll.FindDocs('{$text:{$search:?}}', ['mongodb tutorial'], Docs);
```

---

## 9.6. ORM/ODM Integration

### 9.6.1. Mapping TOrm to MongoDB

```pascal
uses
  mormot.orm.mongodb,
  mormot.db.nosql.mongodb;

var
  Client: TMongoClient;
  Model: TOrmModel;
  Server: TRestServerDB;
begin
  // Connect to MongoDB
  Client := TMongoClient.Create('localhost', 27017);

  // Create model
  Model := TOrmModel.Create([TOrmCustomer, TOrmOrder]);

  // Create server first
  Server := TRestServerDB.Create(Model, ':memory:');

  // Map classes to MongoDB (signature: aClass, aServer, aMongoDatabase, aCollectionName)
  OrmMapMongoDB(TOrmCustomer, Server, Client.Database['mydb'], 'customers');
  OrmMapMongoDB(TOrmOrder, Server, Client.Database['mydb'], 'orders');
end;
```

### 9.6.2. TOrm with MongoDB

```pascal
type
  TOrmArticle = class(TOrm)
  private
    fTitle: RawUtf8;
    fContent: RawUtf8;
    fTags: TRawUtf8DynArray;
    fMetadata: Variant;  // TDocVariant for flexible schema
  published
    property Title: RawUtf8 read fTitle write fTitle;
    property Content: RawUtf8 read fContent write fContent;
    property Tags: TRawUtf8DynArray read fTags write fTags;
    property Metadata: Variant read fMetadata write fMetadata;
  end;
```

### 9.6.3. Using the ORM

Once mapped, use standard ORM methods:

```pascal
var
  Article: TOrmArticle;
begin
  // Create
  Article := TOrmArticle.Create;
  Article.Title := 'Introduction to MongoDB';
  Article.Content := 'MongoDB is a document database...';
  Article.Tags := ['mongodb', 'nosql', 'database'];
  Article.Metadata := _ObjFast(['author', 'John', 'views', 0]);
  Server.Orm.Add(Article, True);

  // Read
  Article := TOrmArticle.Create(Server.Orm, ArticleID);

  // Update
  Article.Metadata.views := Article.Metadata.views + 1;
  Server.Orm.Update(Article);

  // Delete
  Server.Orm.Delete(TOrmArticle, ArticleID);

  // Query
  Article := TOrmArticle.CreateAndFillPrepare(Server.Orm,
    'Tags = ?', ['mongodb']);
  while Article.FillOne do
    WriteLn(Article.Title);
end;
```

### 9.6.4. MongoDB-Specific Queries

For advanced queries, use direct MongoDB access:

```pascal
var
  Coll: TMongoCollection;
  Docs: TVariantDynArray;
begin
  // Get underlying collection
  Coll := TRestStorageMongoDB(
    Server.StaticDataServer[TOrmArticle]).Collection;

  // Complex aggregation
  Coll.AggregateJson([
    '{$match:{status:"published"}}',
    '{$group:{_id:"$author",count:{$sum:1}}}',
    '{$sort:{count:-1}}'
  ], Docs);
end;
```

---

## 9.7. Aggregation Framework

### 9.7.1. Pipeline Operations

```pascal
var
  Results: TVariantDynArray;
begin
  Coll.AggregateDoc([
    // Stage 1: Filter
    _ObjFast(['$match', _Obj(['status', 'active'])]),

    // Stage 2: Group and count
    _ObjFast(['$group', _Obj([
      '_id', '$category',
      'total', _Obj(['$sum', 1]),
      'avgPrice', _Obj(['$avg', '$price'])
    ])]),

    // Stage 3: Sort
    _ObjFast(['$sort', _Obj(['total', -1])])
  ], Results);

  for Doc in Results do
    WriteLn(Doc._id, ': ', Doc.total, ' items, avg $', Doc.avgPrice);
end;
```

### 9.7.2. Common Aggregation Operators

| Operator | Description |
|----------|-------------|
| `$match` | Filter documents |
| `$group` | Group by field |
| `$sort` | Sort results |
| `$project` | Reshape documents |
| `$limit` | Limit results |
| `$skip` | Skip documents |
| `$unwind` | Deconstruct arrays |
| `$lookup` | Left outer join |

---

## 9.8. Indexes

### 9.8.1. Creating Indexes

```pascal
// Single field index
Coll.EnsureIndex('{email:1}');  // 1 = ascending

// Compound index
Coll.EnsureIndex('{status:1,created:-1}');

// Unique index
Coll.EnsureIndex('{email:1}', [ifoUnique]);

// Text index
Coll.EnsureIndex('{title:"text",content:"text"}');

// TTL index (auto-delete after time)
Coll.EnsureIndex('{createdAt:1}', [ifoExpireAfterSeconds], 3600);
```

### 9.8.2. Index Hints

```pascal
// Force index usage
Coll.FindJson('{status:?}', ['active'], '',
  '{$hint:{status:1}}');  // Use status index
```

---

## 9.9. Performance Tips

### 9.9.1. Write Performance

```pascal
// 1. Use bulk inserts
Coll.Insert(DocsArray);  // Single call for many documents

// 2. Use unacknowledged writes for non-critical data
Client.WriteConcern := wcUnacknowledged;
try
  // Fast writes (no server confirmation)
  Coll.Insert(LogEntries);
finally
  Client.WriteConcern := wcAcknowledged;
end;

// 3. Pre-generate ObjectIDs
for i := 0 to High(Docs) do
begin
  ID.ComputeNew;
  Docs[i]._id := ID.ToVariant;
end;
```

### 9.9.2. Read Performance

```pascal
// 1. Use projections to limit returned fields
Coll.FindJson('{status:?}', ['active'], '{name:1,email:1}');

// 2. Use covered queries (all fields in index)
Coll.EnsureIndex('{email:1,name:1}');
Coll.FindJson('{email:?}', ['john@example.com'], '{email:1,name:1,_id:0}');

// 3. Use cursor batching for large result sets
Cursor := Coll.Find(Query, nil, [mqfNoCursorTimeout]);
Cursor.BatchSize := 1000;
```

### 9.9.3. Index Strategies

- Index fields used in `$match` stages
- Index fields used in sorts
- Compound indexes for multi-field queries
- Cover queries with indexes when possible
- Monitor with `explain()` equivalent

---

## 9.10. Migration from mORMot 1

### 9.10.1. Unit Renames

| mORMot 1 | mORMot 2 |
|----------|----------|
| `SynMongoDB.pas` | `mormot.db.nosql.mongodb.pas` |
| `mORMotMongoDB.pas` | `mormot.orm.mongodb.pas` |

### 9.10.2. Class/Function Renames

| mORMot 1 | mORMot 2 |
|----------|----------|
| `StaticMongoDBRegister` | `OrmMapMongoDB` |
| `TMongoClient` | `TMongoClient` (unchanged) |
| `TMongoDatabase` | `TMongoDatabase` (unchanged) |
| `TMongoCollection` | `TMongoCollection` (unchanged) |

### 9.10.3. Protocol Changes

mORMot 2 uses the new MongoDB wire protocol (OP_MSG) introduced in MongoDB 3.6:

```pascal
// For older MongoDB versions (< 3.6), define:
{$DEFINE MONGO_OLDPROTOCOL}
```

---

## 9.11. Summary

MongoDB integration in mORMot 2 provides:

- **Full ODM support**: Use `TOrm` classes with MongoDB
- **Direct client access**: Low-level `TMongoClient` for advanced operations
- **TDocVariant integration**: Natural document handling
- **Query flexibility**: Full MongoDB query language support
- **Performance**: Bulk operations, connection pooling, index management
- **Mixing backends**: Combine MongoDB with SQL in same application

---

*Next Chapter: JSON RESTful Client-Server*

---

## Navigation

| Previous | Index | Next |
|----------|-------|------|
| [Chapter 8: External SQL Database Access](mORMot2-SAD-Chapter-08.md) | [Index](mORMot2-SAD-Index.md) | [Chapter 10: JSON and RESTful Fundamentals](mORMot2-SAD-Chapter-10.md) |
