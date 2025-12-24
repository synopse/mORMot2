# Sample 19: Basic Demo VCL Client

**Ported from**: DMVCframework `basicdemo_vclclient`

## Purpose

Simple VCL client application that demonstrates consuming REST API endpoints from the **01-basicdemo_server** using mORMot2's REST client capabilities.

## Features

- **Simple HTTP REST Client**: Uses `TRestHttpClientGeneric` to make HTTP calls
- **Three API endpoints**:
  - `GET /root/BasicDemoApi/HelloWorld` - Returns a hello message with current time
  - `GET /root/BasicDemoApi/Divide?par1=X&par2=Y` - Divides two numbers
  - `POST /root/BasicDemoApi/HelloWorldPost` - Echoes back posted JSON with modification
- **Interactive UI**: Buttons for each endpoint with editable parameters
- **Response Display**: Shows both parsed and raw JSON responses

## Key Components

### MainFormU.pas
The main form with:
- **TRestHttpClientGeneric**: mORMot2 HTTP client
- **Three buttons**: One for each API endpoint
- **Input fields**: For divide operation (num1, num2) and POST data
- **Response memo**: Displays API responses and errors

## API Endpoints Used

All endpoints are from the `IBasicDemoApi` interface (01-basicdemo_server):

1. **HelloWorld** (GET)
   - URL: `/root/BasicDemoApi/HelloWorld`
   - Returns: `{"message": "...", "time": "..."}`

2. **Divide** (GET)
   - URL: `/root/BasicDemoApi/Divide?par1=10&par2=20`
   - Returns: `{"result": 0.5}`

3. **HelloWorldPost** (POST)
   - URL: `/root/BasicDemoApi/HelloWorldPost`
   - Body: `{"name": "Bob"}`
   - Returns: `{"data": "...", "modified": "from server"}`

## How to Use

1. **Start the server**:
   ```bash
   cd ..\01-basicdemo_server
   01-basicdemo_server.exe
   ```
   Server will start on `http://localhost:8080`

2. **Run the client**:
   ```bash
   19-basicdemo_vclclient.exe
   ```

3. **Test the endpoints**:
   - Click "GET /hello" to call HelloWorld endpoint
   - Enter two numbers and click "GET /div/{num1}/{num2}" for division
   - Modify JSON and click "POST /hello" to test POST endpoint

## Technical Details

### mORMot2 REST Client Approach

Unlike DMVCframework's `IMVCRESTClient`, this sample uses mORMot2's lower-level HTTP client:

```pascal
// Create client
fClient := TRestHttpClientGeneric.Create('localhost', '8080', TOrmModel.Create([]));

// Make GET request
status := fClient.CallBackGet('root/BasicDemoApi/HelloWorld', [], response);

// Make POST request
status := fClient.CallBackPost('root/BasicDemoApi/HelloWorldPost',
  postData, response, true, nil, 'application/json');
```

### JSON Parsing

Responses are parsed using mORMot2's `TDocVariantData`:

```pascal
var
  doc: TDocVariantData;
begin
  doc.InitJson(response, JSON_FAST);
  message := doc.U['message'];  // Access field
  time := doc.U['time'];
end;
```

## Differences from DMVC Version

| Aspect | DMVCframework | mORMot2 |
|--------|---------------|---------|
| **Client Type** | `IMVCRESTClient` (fluent interface) | `TRestHttpClientGeneric` (class-based) |
| **URL Building** | `BaseURL().Get('/endpoint')` | `CallBackGet('endpoint', [], response)` |
| **JSON Parsing** | Built into REST client | Manual using `TDocVariantData` |
| **Error Handling** | Exceptions from REST client | HTTP status codes + exceptions |

## Compilation

```bash
# From WSL
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe W:\mORMot2\ex\dmvc\19-basicdemo_vclclient\19-basicdemo_vclclient.dproj
```

## Expected Output

When running successfully:
- Client shows "Ready" status
- Clicking "GET /hello" displays greeting message and time
- Clicking "GET /div" shows division result (e.g., "10 / 20 = 0.5")
- Clicking "POST /hello" echoes the JSON with "modified" field added

## Notes

- Server must be running on `localhost:8080` before starting client
- All UTF-8 text is properly handled through mORMot2's `RawUtf8` type
- Error messages are displayed in both the memo and a message dialog
- Response memo accumulates all responses for easy comparison

## Port Status

✅ **Complete** - Fully functional VCL client for basicdemo_server

### What Was Ported
- All three REST endpoints from DMVC client
- Input/output UI for interactive testing
- JSON parsing and display
- Error handling

### Key Changes
- Replaced `IMVCRESTClient` with `TRestHttpClientGeneric`
- Added manual JSON parsing with `TDocVariantData`
- Changed URL structure to match mORMot2 interface routing
- Enhanced error messages and response display
