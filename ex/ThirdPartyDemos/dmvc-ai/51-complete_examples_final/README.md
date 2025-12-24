# Sample 51: Complete Examples - Final Showcase

## Overview

This comprehensive sample integrates multiple mORMot2 features into a production-ready REST API server, demonstrating best practices and real-world patterns. It combines concepts from DMVC's articles_crud_vcl_client and jsonrpc samples into a unified mORMot2 implementation.

## Features Integrated

### 1. ORM with SQLite Backend
- **TArticle entity** with full CRUD operations
- **In-memory database** for fast testing
- **Automatic schema creation**
- **Database indexing** for performance
- **RESTful endpoints** for all operations

### 2. Interface-based Services (JSON-RPC style)
- **ICompleteApi** service contract
- **Type-safe method calls**
- **Automatic JSON serialization**
- **Request/response validation**
- **Service dependency injection**

### 3. Comprehensive CRUD Operations
- **Create**: Single and batch article creation
- **Read**: Get by ID, filter by author, search by keyword
- **Update**: Increment view count
- **Delete**: Remove articles
- **List**: Get all articles with filtering

### 4. Advanced Features
- **Batch operations** with transactions
- **Database statistics** aggregation
- **Full-text search** (simple implementation)
- **Performance monitoring** with TPrecisionTimer
- **Comprehensive logging** at all levels

### 5. Production Patterns
- **Error handling** at all levels
- **Input validation**
- **Transaction management** for batch operations
- **Resource cleanup** (proper Free in destructors)
- **Structured logging** (trace, debug, info, warning, error)

## Architecture

```
┌─────────────────────────────────────────────────┐
│            HTTP Server (:8080)                  │
│         TRestHttpServer                         │
└────────────────┬────────────────────────────────┘
                 │
        ┌────────┴────────┐
        │                 │
        ▼                 ▼
┌──────────────┐  ┌──────────────────┐
│  ORM REST    │  │  Service Layer   │
│  /Article    │  │  /CompleteApi/*  │
│  (CRUD)      │  │  (Business Logic)│
└──────┬───────┘  └────────┬─────────┘
       │                   │
       └─────────┬─────────┘
                 ▼
         ┌──────────────┐
         │  TRestServer │
         │   + Model    │
         └──────┬───────┘
                │
                ▼
         ┌──────────────┐
         │   SQLite     │
         │  (:memory:)  │
         └──────────────┘
```

## API Endpoints

### ORM REST Endpoints (Automatic)

```bash
# List all articles
GET /root/Article

# Get article by ID
GET /root/Article/1

# Create new article
POST /root/Article
Content-Type: application/json
{
  "Title": "My Article",
  "Author": "John Doe",
  "Content": "Article content here...",
  "Published": true
}

# Update article
PUT /root/Article
Content-Type: application/json
{
  "ID": 1,
  "Title": "Updated Title",
  "ViewCount": 100
}

# Delete article
DELETE /root/Article/1
```

### Service Endpoints (Interface-based)

```bash
# Get server version
curl -X POST http://localhost:8080/root/CompleteApi/GetVersion

# Process data with validation
curl -X POST http://localhost:8080/root/CompleteApi/ProcessData \
  -H "Content-Type: application/json" \
  -d '{"data":"test input"}'

# Get articles by author
curl -X POST http://localhost:8080/root/CompleteApi/GetArticlesByAuthor \
  -H "Content-Type: application/json" \
  -d '{"author":"Author 1"}'

# Batch create articles (transaction)
curl -X POST http://localhost:8080/root/CompleteApi/BatchCreateArticles \
  -H "Content-Type: application/json" \
  -d '{"count":5,"prefix":"Batch Test"}'

# Get database statistics
curl -X POST http://localhost:8080/root/CompleteApi/GetStatistics

# Increment view count
curl -X POST http://localhost:8080/root/CompleteApi/IncrementViewCount \
  -H "Content-Type: application/json" \
  -d '{"aArticleID":1}'

# Search articles
curl -X POST http://localhost:8080/root/CompleteApi/SearchArticles \
  -H "Content-Type: application/json" \
  -d '{"keyword":"sample"}'
```

## Building

```bash
# Compile for Win32
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe \
  /mnt/w/mORMot2/ex/dmvc/51-complete_examples_final/51-complete_examples_final.dproj \
  --platform=Win32

# Compile for Win64
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe \
  /mnt/w/mORMot2/ex/dmvc/51-complete_examples_final/51-complete_examples_final.dproj \
  --platform=Win64
```

## Running

```bash
./51-complete_examples_final.exe
```

The server will:
1. Create in-memory SQLite database
2. Initialize schema with indexes
3. Create 10 sample articles
4. Start HTTP server on port 8080
5. Log all operations to `CompleteExamplesFinal.log`

## Testing Workflow

### 1. Test ORM CRUD Operations

```bash
# List initial articles (10 samples)
curl http://localhost:8080/root/Article

# Create a new article
curl -X POST http://localhost:8080/root/Article \
  -H "Content-Type: application/json" \
  -d '{
    "Title":"My Test Article",
    "Author":"Test User",
    "Content":"This is a test",
    "Published":true
  }'

# Get article by ID
curl http://localhost:8080/root/Article/11

# Update article
curl -X PUT http://localhost:8080/root/Article \
  -H "Content-Type: application/json" \
  -d '{
    "ID":11,
    "Title":"Updated Title",
    "ViewCount":50
  }'

# Delete article
curl -X DELETE http://localhost:8080/root/Article/11
```

### 2. Test Service Operations

```bash
# Get server info
curl -X POST http://localhost:8080/root/CompleteApi/GetVersion

# Test data processing
curl -X POST http://localhost:8080/root/CompleteApi/ProcessData \
  -H "Content-Type: application/json" \
  -d '{"data":"hello world"}'

# Filter by author
curl -X POST http://localhost:8080/root/CompleteApi/GetArticlesByAuthor \
  -H "Content-Type: application/json" \
  -d '{"author":"Author 1"}'

# Batch create
curl -X POST http://localhost:8080/root/CompleteApi/BatchCreateArticles \
  -H "Content-Type: application/json" \
  -d '{"count":3,"prefix":"Batch"}'

# Get statistics
curl -X POST http://localhost:8080/root/CompleteApi/GetStatistics

# Search articles
curl -X POST http://localhost:8080/root/CompleteApi/SearchArticles \
  -H "Content-Type: application/json" \
  -d '{"keyword":"sample"}'
```

### 3. Monitor Logs

```bash
tail -f CompleteExamplesFinal.log
```

Watch structured logging showing:
- Request/response traces
- Performance timings
- Database operations
- Validation results
- Error handling

## Code Organization

```
51-complete_examples_final/
├── 51-complete_examples_final.dpr    # Main program
├── 51-complete_examples_final.dproj  # Project file
├── README.md                          # This file
└── src/
    ├── entities.pas                   # ORM entity (TArticle)
    ├── api.interfaces.pas             # Service contracts
    ├── api.impl.pas                   # Service implementation
    └── server.pas                     # Server setup & initialization
```

## Key Patterns Demonstrated

### 1. Clean Architecture
- **Entities**: Pure data models (entities.pas)
- **Interfaces**: Service contracts (api.interfaces.pas)
- **Implementation**: Business logic (api.impl.pas)
- **Infrastructure**: Server setup (server.pas)

### 2. Dependency Injection
```pascal
fServer.ServiceDefine(TCompleteApiService, [ICompleteApi], sicShared);
```

### 3. Transaction Management
```pascal
batch := TRestBatch.Create(fServer.Orm, TArticle, 1000);
try
  // Add multiple items
  batch.Add(article, true);
  // Execute as single transaction
  fServer.Orm.BatchSend(batch);
finally
  batch.Free;
end;
```

### 4. Performance Monitoring
```pascal
timer.Start;
// ... operation ...
Result.ProcessingTimeMs := timer.TimeInMicroSec div 1000;
```

### 5. Structured Logging
```pascal
TSynLog.Add.Log(sllDebug, 'BatchCreate: created % articles', [count]);
```

## Production Benefits

1. **Type Safety**
   - Interface-based contracts
   - Compile-time validation
   - No string-based routing

2. **Performance**
   - In-memory SQLite for speed
   - Indexed queries
   - Batch operations
   - Zero-copy JSON

3. **Maintainability**
   - Clean separation of concerns
   - Comprehensive logging
   - Self-documenting interfaces
   - Reusable patterns

4. **Testability**
   - Interface mocking
   - In-memory database
   - Isolated services
   - Reproducible tests

## DMVC vs mORMot2 Comparison

| Feature | DMVC | mORMot2 | Notes |
|---------|------|---------|-------|
| ORM | ActiveRecord | TOrm | mORMot2 more lightweight |
| Services | Controllers | Interfaces | mORMot2 type-safe |
| Routing | Attributes | Automatic | mORMot2 convention-based |
| JSON | Multiple serializers | Built-in | mORMot2 zero-config |
| Database | Multiple backends | SQLite/others | Both flexible |
| Logging | Extensible | Built-in | Both comprehensive |

## Related Samples

- **28-activerecord_restful_crud**: ActiveRecord CRUD patterns
- **31-simple_api_using_datasets**: Dataset-based API
- **32-jsonwebtoken_livevaliditywindow**: Authentication
- **36-logging**: Advanced logging patterns
- **50-utilities_batch**: Batch processing utilities

## References

- mORMot2 documentation: https://synopse.info/files/html/Synopse%20mORMot%202%20Framework%20SAD%201.18.html
- ORM tutorial: mORMot2 source `ex/orm-tutorial/`
- Interface services: `mormot.rest.core.pas`
- REST server: `mormot.rest.http.server.pas`
