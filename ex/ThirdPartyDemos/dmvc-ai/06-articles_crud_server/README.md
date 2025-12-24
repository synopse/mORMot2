# Articles CRUD Server - mORMot2 Port

Port of DelphiMVCFramework `articles_crud_server` sample to mORMot2.

## Overview

This example demonstrates a complete CRUD (Create, Read, Update, Delete) REST API server using mORMot2's interface-based services pattern with SQLite ORM backend.

**Original DMVC Sample**: `/mnt/w/DMVCframework/samples/articles_crud_server/`

## Features

- **Complete CRUD Operations**: Create, Read, Update, Delete articles
- **Search Functionality**: Search articles by description
- **Field Metadata API**: Returns article field definitions
- **Bulk Operations**: Create multiple articles in a single transaction
- **Business Rules Validation**:
  - Article code must match format `CXX`, `CXXX`, or `CXXXX` (e.g., C01, C123, C1234)
  - Price must be > 2 for updates
  - Price must be > 5 for deletes
- **SQLite Database**: Persistent storage with automatic table creation
- **Sample Data**: Auto-populated with 5 sample articles on first run
- **JSON-RPC Style API**: All endpoints use POST with JSON bodies

## Architecture

### Files Structure

```
06-articles_crud_server/
├── src/
│   ├── entities.pas         - ORM entity (TOrmArticle)
│   ├── api.interfaces.pas   - Service interface (IArticlesApi) and DTOs
│   ├── api.impl.pas         - Service implementation (TArticlesApi)
│   └── server.pas           - HTTP server setup (TArticlesCrudServer)
├── 06-articles_crud_server.dpr    - Main program
├── 06-articles_crud_server.dproj  - Delphi project
└── README.md
```

### Key Components

**TOrmArticle** (entities.pas):
- ORM entity mapping to SQLite table
- Fields: ID, Code, Description, Price, CreatedAt, UpdatedAt

**IArticlesApi** (api.interfaces.pas):
- Interface-based service contract
- Methods: GetAll, Search, GetMeta, GetById, Create, CreateBulk, Update, Delete

**TArticlesApi** (api.impl.pas):
- Service implementation with business logic
- Validates article codes (regex pattern)
- Enforces price rules
- Converts between ORM entities and DTOs

**TArticlesCrudServer** (server.pas):
- HTTP server wrapper
- Initializes SQLite database
- Registers API service
- Populates sample data

## API Endpoints

All endpoints use **POST** with JSON bodies (mORMot2 interface-based services use JSON-RPC style):

### Get All Articles
```bash
curl -X POST http://localhost:8080/ArticlesApi.GetAll
```

Response:
```json
{
  "result": [
    {
      "id": 1,
      "code": "C001",
      "description": "Margherita Pizza",
      "price": 6.50,
      "createdat": "2025-12-19T12:00:00",
      "updatedat": "2025-12-19T12:00:00"
    }
  ]
}
```

### Search Articles
```bash
curl -X POST http://localhost:8080/ArticlesApi.Search \
  -H "Content-Type: application/json" \
  -d '{"query":"Pizza"}'
```

### Get Article Metadata
```bash
curl -X POST http://localhost:8080/ArticlesApi.GetMeta
```

### Get Article by ID
```bash
curl -X POST http://localhost:8080/ArticlesApi.GetById \
  -H "Content-Type: application/json" \
  -d '{"id":1}'
```

### Create Article
```bash
curl -X POST http://localhost:8080/ArticlesApi.Create \
  -H "Content-Type: application/json" \
  -d '{
    "article": {
      "code": "C999",
      "description": "Deluxe Pizza",
      "price": 12.50
    }
  }'
```

Response: Returns the ID of the created article.

### Create Multiple Articles (Bulk)
```bash
curl -X POST http://localhost:8080/ArticlesApi.CreateBulk \
  -H "Content-Type: application/json" \
  -d '{
    "articles": [
      {"code":"C200","description":"Pizza 1","price":8.00},
      {"code":"C201","description":"Pizza 2","price":9.00}
    ]
  }'
```

Response: Returns count of created articles.

### Update Article
```bash
curl -X POST http://localhost:8080/ArticlesApi.Update \
  -H "Content-Type: application/json" \
  -d '{
    "id": 1,
    "article": {
      "code": "C001",
      "description": "Updated Margherita",
      "price": 7.00
    }
  }'
```

### Delete Article
```bash
curl -X POST http://localhost:8080/ArticlesApi.Delete \
  -H "Content-Type: application/json" \
  -d '{"id":1}'
```

## Business Rules

### Article Code Validation
- Format: Must start with 'C' followed by 2-4 digits
- Valid: `C01`, `C123`, `C1234`
- Invalid: `A01`, `C1`, `C12345`, `C1A`

### Price Validation
- **Create**: Any positive price allowed
- **Update**: Price must be > 2 (error: "We cannot sell so low cost pizzas!")
- **Delete**: Price must be > 5 (error: "Cannot delete an article with a price below 5 euros")

## Compilation

```bash
# Using Delphi IDE
# Open 06-articles_crud_server.dproj and build

# Or compile directly
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe \
  /mnt/w/mORMot2/ex/dmvc/06-articles_crud_server/06-articles_crud_server.dproj
```

## Running

```bash
# Start server
./06-articles_crud_server

# Server will:
# 1. Create articles.db SQLite database (if not exists)
# 2. Create ARTICOLI table (if not exists)
# 3. Populate with 5 sample articles (if empty)
# 4. Start HTTP server on port 8080
# 5. Wait for requests (press Enter to quit)
```

## Sample Data

On first run, the database is populated with:
1. C001 - Margherita Pizza (€6.50)
2. C002 - Pepperoni Pizza (€8.50)
3. C003 - Hawaiian Pizza (€9.00)
4. C100 - Quattro Formaggi Pizza (€10.50)
5. C1000 - Vegetariana Pizza (€7.50)

## DMVC vs mORMot2 Differences

### DMVC (Original)
- **Pattern**: ActiveRecord with DMVC controllers
- **Routing**: RESTful paths (`GET /articles`, `POST /articles`, etc.)
- **Database**: Firebird with FireDAC
- **Controller**: `TArticlesController` with method attributes
- **Service**: `TArticlesService` with ActiveRecord operations
- **Validation**: In `TArticle.OnBeforeInsertOrUpdate`, `OnBeforeUpdate`, `OnBeforeDelete`

### mORMot2 (Port)
- **Pattern**: Interface-based services with ORM
- **Routing**: JSON-RPC style (`POST /ArticlesApi.GetAll`, etc.)
- **Database**: SQLite with mORMot2 ORM
- **Service**: `IArticlesApi` interface + `TArticlesApi` implementation
- **Validation**: In `TArticlesApi` methods (ValidateArticleCode, ValidatePriceForUpdate, etc.)
- **Transactions**: Automatic via `TRestBatch` for bulk operations

### Key Advantages of mORMot2 Approach
1. **Type-Safe**: Interface contracts enforce method signatures
2. **Automatic Serialization**: DTOs auto-convert to/from JSON
3. **Exception Handling**: Service exceptions auto-map to HTTP errors
4. **Batch Operations**: Built-in batch support for bulk inserts
5. **Logging**: Integrated TSynLog for all operations

## Client Example

A simple Delphi client to test the API:

```pascal
uses
  mormot.rest.client,
  mormot.rest.http.client,
  api.interfaces;

var
  Client: TRestHttpClient;
  ArticlesApi: IArticlesApi;
  Articles: TArticleDtos;
  NewArticle: TArticleDto;
  NewId: TID;
begin
  // Create HTTP client
  Client := TRestHttpClientWinHttp.Create('localhost', '8080', TOrmModel.Create([]));
  try
    // Get service proxy
    Client.ServiceDefine([IArticlesApi], sicShared);
    if not Client.Services.Resolve(IArticlesApi, ArticlesApi) then
      raise Exception.Create('Failed to resolve IArticlesApi');

    // Get all articles
    Articles := ArticlesApi.GetAll;
    WriteLn('Found ', Length(Articles), ' articles');

    // Create new article
    NewArticle.Code := 'C555';
    NewArticle.Description := 'Test Pizza';
    NewArticle.Price := 11.50;
    NewId := ArticlesApi.Create(NewArticle);
    WriteLn('Created article ID: ', NewId);

    // Search
    Articles := ArticlesApi.Search('Pizza');
    WriteLn('Search found ', Length(Articles), ' articles');

    // Update
    NewArticle.Description := 'Updated Pizza';
    ArticlesApi.Update(NewId, NewArticle);

    // Delete
    ArticlesApi.Delete(NewId);
  finally
    Client.Free;
  end;
end;
```

## Error Handling

The API returns proper HTTP status codes:
- **200 OK**: Successful operation
- **400 Bad Request**: Validation error (invalid code, price too low, etc.)
- **404 Not Found**: Article ID not found
- **500 Internal Server Error**: Unexpected server error

Example error response:
```json
{
  "errorCode": 400,
  "errorText": "Article code must be in the format \"CXX or CXXX or CXXXX\""
}
```

## Testing

Use curl, Postman, or any HTTP client to test the API. The server includes CORS support (`Access-Control-Allow-Origin: *`) for browser-based clients.

## Related Examples

- **01-basicdemo_server**: Simple hello world API
- **02-console_sample**: Console application basics
- **03-routing**: Advanced routing patterns
- **04-renders**: Different response types

## References

- **DMVC Original**: `/mnt/w/DMVCframework/samples/articles_crud_server/`
- **mORMot2 ORM**: `/mnt/w/mORMot2/src/orm/`
- **Interface Services**: `/mnt/w/mORMot2/src/rest/mormot.rest.server.pas`
- **Conversion Guide**: `/mnt/w/mORMot2/ex/dmvc/CONVERSION-GUIDE.md`

---

**Status**: ✅ Complete - Fully functional CRUD API server with validation and sample data
**Last Updated**: 2025-12-19
