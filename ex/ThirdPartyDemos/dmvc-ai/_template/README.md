# mORMot2 Template Sample

This is a template project for creating mORMot2 REST API samples following Clean Architecture patterns.

## Project Structure

```
_template/
├── src/
│   ├── entities.pas          # Domain entities (ORM classes)
│   ├── api.interfaces.pas    # API interface definitions and DTOs
│   ├── api.impl.pas          # API implementation
│   └── server.pas            # Server initialization and HTTP setup
├── TemplateSample.dpr        # Main program
├── TemplateSample.dproj      # Delphi project file (D12)
└── README.md                 # This file
```

## Features

- **Clean Architecture**: Separation of concerns with entities, interfaces, and implementation
- **TDD-Ready**: Structure designed for test-driven development
- **RESTful API**: HTTP server with JSON interface methods
- **SQLite Database**: Embedded database using mORMot2 ORM
- **DTO Mapping**: Automatic mapping between ORM entities and Data Transfer Objects
- **CORS Enabled**: Cross-origin resource sharing for web clients

## Quick Start

### 1. Copy the Template

Create a new sample by copying this template:

```bash
cp -r _template ../my-new-sample
cd ../my-new-sample
```

### 2. Customize the Files

Replace the following in all files:

- `TemplateSample` → Your sample name (e.g., `BlogSample`)
- `TOrmSample` → Your entity name (e.g., `TOrmPost`)
- `ITemplateSample` → Your API interface (e.g., `IBlogApi`)
- `TTemplateSample` → Your API implementation (e.g., `TBlogApi`)
- Generate new GUID for API interface (Ctrl+Shift+G in Delphi)

### 3. Update the Entity (entities.pas)

Define your domain entity fields:

```pascal
type
  TOrmYourEntity = class(TOrm)
  protected
    fYourField: RawUtf8;
    // Add more fields...
  published
    property YourField: RawUtf8 read fYourField write fYourField;
  end;
```

### 4. Update the API Interface (api.interfaces.pas)

Define your API methods:

```pascal
type
  IYourApi = interface(IInvokable)
    ['{NEW-GUID-HERE}']  // Generate new GUID!
    function YourMethod(param: RawUtf8): TYourID;
    // Add more methods...
  end;
```

### 5. Update the Implementation (api.impl.pas)

Implement your API methods:

```pascal
function TYourApi.YourMethod(param: RawUtf8): TYourID;
begin
  // Your implementation...
end;
```

### 6. Update Server (server.pas)

Update the server class name and ORM entities:

```pascal
fRestServer := TRestServerDB.CreateSqlite3([TOrmYourEntity], aDbFileName);
```

### 7. Update Main Program (*.dpr)

Update the uses clause and program name.

### 8. Rename Files

```bash
mv TemplateSample.dpr YourSample.dpr
mv TemplateSample.dproj YourSample.dproj
```

### 9. Update .dproj File

- Open `YourSample.dproj` in a text editor
- Replace `TemplateSample` with `YourSample`
- Generate new GUID for `<ProjectGuid>` (Tools → GUID Generator in Delphi)

### 10. Build and Run

```bash
dcc32 YourSample.dpr
./YourSample.exe
```

## API Endpoints

The template creates these endpoints:

- `POST /TemplateSample/CreateSample` - Create new sample
- `GET /TemplateSample/GetSample?id=123` - Get sample by ID
- `GET /TemplateSample/GetAllSamples` - Get all samples
- `POST /TemplateSample/UpdateSample` - Update existing sample
- `POST /TemplateSample/DeleteSample` - Delete sample by ID

## Testing with curl

```bash
# Create a sample
curl -X POST http://localhost:8080/TemplateSample/CreateSample \
  -H "Content-Type: application/json" \
  -d '{"name":"Test","description":"My test sample"}'

# Get all samples
curl http://localhost:8080/TemplateSample/GetAllSamples

# Get sample by ID
curl http://localhost:8080/TemplateSample/GetSample?id=1

# Update sample
curl -X POST http://localhost:8080/TemplateSample/UpdateSample \
  -H "Content-Type: application/json" \
  -d '{"id":1,"name":"Updated","description":"Updated description"}'

# Delete sample
curl -X POST http://localhost:8080/TemplateSample/DeleteSample \
  -H "Content-Type: application/json" \
  -d '{"id":1}'
```

## Patterns Used

### Clean Architecture Layers

1. **Domain Core** (`entities.pas`):
   - Pure domain entities
   - Business rules and validation
   - No external dependencies

2. **Application Layer** (`api.interfaces.pas`, `api.impl.pas`):
   - API interfaces (contracts)
   - DTOs for data transfer
   - Implementation using domain entities

3. **Infrastructure** (`server.pas`):
   - HTTP server setup
   - Database configuration
   - Service registration

### ORM to DTO Mapping

The template uses `TRttiMap` for automatic mapping between:
- `TOrmSample` (database entity) ↔ `TSampleDTO` (API data transfer)

This allows:
- Clean separation of concerns
- Database schema changes don't affect API
- Optimized data transfer (only needed fields)

### Dependency Injection

The API implementation receives `IRestOrm` interface in constructor:

```pascal
constructor TTemplateSample.Create(const aRest: IRestOrm);
```

This allows:
- Easy testing with mocks
- Flexible database backends
- Clear dependencies

## Learn More

- [mORMot2 Documentation](https://synopse.info/fossil/wiki?name=SQLite3+Framework)
- [TDD Service Example](../tdd-service/README.md)
- [mORMot2 GitHub](https://github.com/synopse/mORMot2)

## License

Same as mORMot2 framework (MPL/GPL/LGPL triple license).
