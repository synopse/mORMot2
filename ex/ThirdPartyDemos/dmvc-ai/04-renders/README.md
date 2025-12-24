# mORMot2 Renders Sample

This sample demonstrates different types of HTTP responses and content serialization in mORMot2, equivalent to DMVCframework's renders sample.

## Features Demonstrated

### 1. JSON Rendering

- **Single Objects**: Return individual entities as JSON
- **Arrays**: Return collections as JSON arrays
- **Complex Objects**: Objects with nested data and metadata
- **Automatic Serialization**: mORMot2 handles JSON serialization transparently

### 2. Plain Text Responses

- Custom text formatting
- Alternative to JSON for simple data display

### 3. CSV Format

- Export data as CSV
- Demonstrates custom content formatting

### 4. Binary Data

- File download (binary data handling)
- File upload with base64 encoding for JSON transport

### 5. Metadata and Processing Time

- Response enrichment with processing metadata
- Timing information

## Project Structure

```
04-renders/
├── src/
│   ├── entities.pas          # Domain entities (TOrmPerson, TOrmCustomer)
│   ├── api.interfaces.pas    # API interface and DTOs
│   ├── api.impl.pas          # API implementation
│   └── server.pas            # HTTP server setup
├── RendersSample.dpr         # Main program
├── RendersSample.dproj       # Delphi project file
└── README.md                 # This file
```

## Building and Running

### Compile

```bash
cd /mnt/w/mORMot2/ex/dmvc/04-renders
dcc32 RendersSample.dpr
```

Or using the Delphi compiler utility:

```bash
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe RendersSample.dproj
```

### Run

```bash
./RendersSample.exe
```

The server will start on `http://localhost:8080`

## API Endpoints

### JSON Responses

#### Get Single Person
```bash
curl http://localhost:8080/RendersSample/GetPerson?id=1
```

Response:
```json
{
  "id": 1,
  "firstname": "Daniele",
  "lastname": "Teti",
  "dob": "1979-11-04T00:00:00",
  "married": true
}
```

#### Get All People
```bash
curl http://localhost:8080/RendersSample/GetPeople
```

Response:
```json
{
  "result": [
    {
      "id": 1,
      "firstname": "Daniele",
      "lastname": "Teti",
      "dob": "1979-11-04T00:00:00",
      "married": true
    },
    {
      "id": 2,
      "firstname": "John",
      "lastname": "Doe",
      "dob": "1879-10-02T00:00:00",
      "married": false
    }
  ]
}
```

#### Get Customer
```bash
curl http://localhost:8080/RendersSample/GetCustomer?id=1
```

Response:
```json
{
  "id": 1,
  "name": "bit Time Professionals",
  "contactfirst": "Daniele",
  "contactlast": "Teti",
  "addressline1": "Via di Valle Morta 10",
  "city": "Rome, IT"
}
```

#### Get Customers
```bash
curl http://localhost:8080/RendersSample/GetCustomers
```

Response: Array of customer objects

#### Get People with Metadata
```bash
curl http://localhost:8080/RendersSample/GetPeopleWithMetadata
```

Response:
```json
{
  "items": [
    { "id": 1, "firstname": "Daniele", ... },
    { "id": 2, "firstname": "John", ... }
  ],
  "metadata": {
    "startProcessing": "2024-01-15T10:30:00",
    "stopProcessing": "2024-01-15T10:30:00.123",
    "customData": "There are 2 people in the list"
  }
}
```

#### Get Simple Arrays
```bash
curl http://localhost:8080/RendersSample/GetSimpleArrays
```

Response:
```json
{
  "integers": [0, 1, 2, 3, 4],
  "strings": ["Value 0", "Value 1", "Value 2", "Value 3", "Value 4"],
  "doubles": [0, 1.1, 4.4, 29.7, 282.4]
}
```

### Plain Text Response

```bash
curl http://localhost:8080/RendersSample/GetPersonAsText?id=1
```

Response (plain text):
```
ID        : 1
FirstName : Daniele
LastName  : Teti
DOB       : 1979-11-04
Married   : True
```

### CSV Response

#### Wrong Way - Wrapped in JSON (Anti-Pattern)

```bash
curl http://localhost:8080/RendersSample/GetPeopleAsCSV
```

Response (application/json - **WRONG**):
```json
"first_name;last_name;dob;married\r\nDaniele;Teti;1979-11-04;True\r\nJohn;Doe;1879-10-02;False\r\nJane;Doe;1883-01-05;True\r\n"
```

**Why This Is Wrong:**
- CSV content is wrapped in a JSON string envelope
- Content-Type is `application/json` instead of `text/csv`
- Client has to parse JSON first, then parse CSV
- Not idiomatic - doesn't follow HTTP semantics

#### Correct Way - Raw CSV with Proper Content-Type

```bash
curl http://localhost:8080/RendersSample/GetPeopleAsRawCSV
```

Response (text/csv):
```csv
first_name;last_name;dob;married
Daniele;Teti;1979-11-04;True
John;Doe;1879-10-02;False
Jane;Doe;1883-01-05;True
```

**Why This Is Correct:**
- Raw CSV content in the response body
- Content-Type is `text/csv` (proper HTTP semantics)
- Client can use CSV directly without extra parsing
- Works with standard CSV tools and libraries
- Uses `Ctxt.Returns()` to set custom content type

### Binary Data

#### Wrong Way - Base64 in JSON (Anti-Pattern)

```bash
curl http://localhost:8080/RendersSample/GetBinaryData?filename=myfile.png
```

Response (application/json - **WRONG**):
```json
"iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg=="
```

**Why This Is Wrong:**
- Binary data is base64-encoded and wrapped in JSON
- Increases payload size by ~33% (base64 overhead)
- Content-Type is `application/json` instead of proper mime type
- Client must decode base64 before using the data
- Doesn't work with standard HTTP tools (browsers, wget, etc.)

#### Correct Way - Raw Binary with Proper Content-Type

```bash
curl http://localhost:8080/RendersSample/GetRawBinaryData?filename=myfile.png
```

Response: Raw binary data with `Content-Type: image/png`

**Why This Is Correct:**
- Raw binary bytes in the response body
- Content-Type matches actual file type (`image/png`, `image/jpeg`, `application/pdf`, etc.)
- No base64 encoding overhead (smaller payload)
- Works with browsers (can display images directly)
- Works with standard tools (wget, curl -O, etc.)
- Uses `Ctxt.Returns()` to set custom content type

#### Upload Binary File
```bash
curl -X POST http://localhost:8080/RendersSample/UploadBinaryData \
  -H "Content-Type: application/json" \
  -d '{
    "fieldname": "file",
    "filename": "test.dat",
    "contenttype": "application/octet-stream",
    "data": "SGVsbG8gV29ybGQh"
  }'
```

Response:
```json
{
  "filename": "abc123.dat",
  "ref": "/files/abc123.dat"
}
```

## Key Concepts

### 1. Content-Type Handling

mORMot2 automatically handles content types based on the response data:

- **JSON**: Default for objects and arrays (DTOs, dynamic arrays)
- **Text**: For `RawUtf8` string responses (but still wrapped in JSON)
- **Custom**: For raw content, use `Ctxt.Returns()` with custom headers

**IMPORTANT:** When returning non-JSON content (CSV, binary, XML, etc.), you must use `Ctxt.Returns()` to:
1. Return raw content (not wrapped in JSON)
2. Set proper `Content-Type` header
3. Follow HTTP semantics correctly

### 2. Using Ctxt.Returns for Raw Output

For raw (non-JSON) responses, use this pattern:

```pascal
procedure TMyService.GetRawContent;
var
  content: RawUtf8;
  ctxt: TServiceRunningContext;
begin
  // Build your content
  content := 'your,raw,csv,data';

  // Get current service context
  ctxt := CurrentServiceContext;

  // Return raw content with custom content-type
  if ctxt.Request <> nil then
    ctxt.Request.Returns(
      content,                           // Raw content
      HTTP_SUCCESS,                      // HTTP status code
      'Content-Type: text/csv'          // Custom header(s)
    );
end;
```

**When to use Ctxt.Returns:**
- ✅ CSV exports (`text/csv`)
- ✅ Binary downloads (`image/png`, `application/pdf`, etc.)
- ✅ XML responses (`application/xml`)
- ✅ Plain text (`text/plain`)
- ✅ HTML pages (`text/html`)
- ❌ JSON responses (use function return values - automatic)

**Anti-Pattern to Avoid:**
```pascal
// DON'T DO THIS - Returns JSON-wrapped CSV
function GetPeopleAsCSV: RawUtf8;
begin
  Result := 'name,age\nJohn,30'; // Returns: "name,age\nJohn,30" as JSON string
end;

// DO THIS - Returns raw CSV
procedure GetPeopleAsCSV;
var
  ctxt: TServiceRunningContext;
begin
  ctxt := CurrentServiceContext;
  if ctxt.Request <> nil then
    ctxt.Request.Returns('name,age'#13#10'John,30', HTTP_SUCCESS, 'Content-Type: text/csv');
end;
```

### 3. DTO Pattern

All API methods use DTOs (Data Transfer Objects) instead of ORM entities:

```pascal
// DTO (used in API)
TPersonDTO = packed record
  id: TID;
  firstname: RawUtf8;
  lastname: RawUtf8;
  dob: TDateTime;
  married: Boolean;
end;

// ORM Entity (internal)
TOrmPerson = class(TOrm)
  fFirstName: RawUtf8;
  // ...
end;
```

This provides:
- Clean separation between database and API layers
- Flexibility to change database schema without breaking API
- Optimized data transfer

### 4. Automatic JSON Serialization

mORMot2 uses RTTI to automatically serialize:

- Records (DTOs)
- Dynamic arrays
- Nested structures
- Standard types (integers, strings, dates, booleans)

### 5. Error Handling

Use `ERestException` for API errors:

```pascal
if person.ID = 0 then
  raise ERestException.CreateUtf8('Person with ID % not found', [id]);
```

This returns proper HTTP error responses with JSON error details.

### 6. Sample Data

The server automatically creates sample data on first run:

- 3 sample persons
- 3 sample customers

## Comparison with DMVCframework

### Similarities

- Clean separation of concerns
- RESTful API design
- Multiple response formats
- Automatic serialization

### Differences

| Feature | DMVCframework | mORMot2 |
|---------|---------------|---------|
| Response type | Attribute-based (`[MVCProduces]`) | Type-based (automatic) |
| Serialization | Manual `Render()` calls | Automatic via RTTI |
| DTOs | Classes | Records (lightweight) |
| Attributes | Extensive use | Minimal (cleaner) |
| Binary data | Direct stream | Base64 in JSON (or custom) |

### mORMot2 Advantages

1. **Type-Safe**: Compile-time checking of interfaces
2. **Lightweight DTOs**: Records instead of classes
3. **Automatic Serialization**: No manual render calls needed
4. **Performance**: Faster record serialization
5. **Less Boilerplate**: No attributes for basic scenarios

## Testing

### Automated Tests

See the test suite for automated testing examples:

```bash
cd /mnt/w/mORMot2/ex/dmvc/04-renders/test
dcc32 RendersSample.Tests.dpr
./RendersSample.Tests.exe
```

### Manual Testing with curl

```bash
# JSON object
curl http://localhost:8080/RendersSample/GetPerson?id=1

# JSON array
curl http://localhost:8080/RendersSample/GetPeople

# Plain text
curl http://localhost:8080/RendersSample/GetPersonAsText?id=1

# CSV - WRONG (JSON-wrapped)
curl http://localhost:8080/RendersSample/GetPeopleAsCSV

# CSV - CORRECT (raw with proper content-type)
curl http://localhost:8080/RendersSample/GetPeopleAsRawCSV

# Binary - WRONG (base64 in JSON)
curl http://localhost:8080/RendersSample/GetBinaryData?filename=test.png

# Binary - CORRECT (raw with proper content-type)
curl http://localhost:8080/RendersSample/GetRawBinaryData?filename=test.png

# With metadata
curl http://localhost:8080/RendersSample/GetPeopleWithMetadata

# Arrays
curl http://localhost:8080/RendersSample/GetSimpleArrays
```

## Learn More

- [mORMot2 Documentation](https://synopse.info/fossil/wiki?name=SQLite3+Framework)
- [DMVCframework Renders Sample](https://github.com/danieleteti/delphimvcframework/tree/master/samples/renders)
- [mORMot2 Interface-based Services](https://synopse.info/fossil/wiki?name=Interface+based+services)

## License

Same as mORMot2 framework (MPL/GPL/LGPL triple license).
