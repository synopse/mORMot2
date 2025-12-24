# mORMot2 File Upload Sample

This sample demonstrates how to handle file uploads in mORMot2, equivalent to the DMVCFramework `file_upload` sample.

## Features

- **File Upload**: Handle multipart/form-data file uploads via HTTP POST
- **File Validation**: Filename validation and duplicate checking
- **File Listing**: Display uploaded files on the main page
- **Static Files**: Serve CSS files for styling
- **REST API**: Service endpoint for getting uploaded file information

## Architecture

### Components

1. **TFileUploadHttpServer**: Custom HTTP server that extends `TRestHttpServer`
   - Uses `MultiPartFormDataDecode` for idiomatic multipart parsing
   - Serves static CSS files
   - Generates dynamic HTML with file list
   - Routes requests to appropriate handlers

2. **TFileUploadRestServer**: Custom REST server extending `TRestServerFullMemory`
   - Manages the upload folder
   - Provides REST API endpoints

3. **IFileUploadApi**: REST API service interface
   - `GetUploadedFiles`: Returns list of uploaded files with metadata

4. **TFileUploadApiService**: Implementation of the file upload API

### File Upload Flow

1. User selects a file on the HTML form
2. Browser sends POST to `/upload` with `multipart/form-data`
3. `TFileUploadHttpServer.HandleMultipartUpload` uses `MultiPartFormDataDecode` to parse
4. Filename is validated and sanitized (security checks)
5. File is saved to `uploadedfiles/` folder
6. Server redirects to main page with updated file list

### Security Features

- **Directory Traversal Prevention**: Rejects paths containing ".."
- **Filename Sanitization**: Extracts only the filename, ignoring path components
- **Duplicate Detection**: Prevents overwriting existing files
- **Content-Type Validation**: Only processes multipart/form-data requests

## Endpoints

- `GET /` - Main page with upload form and file list
- `POST /upload` - File upload endpoint (multipart/form-data)
- `GET /static/milligram.min.css` - Static CSS file
- `GET /root/FileUploadApi.GetUploadedFiles` - REST API for file list

## Directory Structure

```
18-file_upload/
├── FileUploadSample.dpr          # Main program
├── FileUploadSample.dproj        # Delphi project file
├── README.md                     # This file
├── src/
│   ├── server.pas                # Server implementation with upload handling
│   ├── api.interfaces.pas        # REST API interface definition
│   └── api.impl.pas             # REST API implementation
├── www/
│   └── milligram.min.css        # CSS framework for styling
└── uploadedfiles/               # Upload destination (created at runtime)
```

## Building and Running

### Prerequisites

- Delphi 12 (RAD Studio 12 Athens) or later
- mORMot2 source code (parent directories)

### Compilation

```bash
# Using dcc32 (Win32)
dcc32 -B FileUploadSample.dpr

# Using dcc64 (Win64)
dcc64 -B FileUploadSample.dpr

# Or use Delphi IDE to open FileUploadSample.dproj
```

### Running

```bash
# Win32
Win32\Debug\FileUploadSample.exe

# Win64
Win64\Debug\FileUploadSample.exe
```

The server will start on port **3000** by default.

Open your browser to: `http://localhost:3000`

## Usage

1. Click **Choose File** button to select a file
2. Click **Upload** to upload the file
3. The page will refresh showing the uploaded file in the list
4. Multiple files can be uploaded (one at a time)

## Testing the REST API

You can query the file list via the REST API:

```bash
# Using curl
curl http://localhost:3000/root/FileUploadApi.GetUploadedFiles

# Using browser
http://localhost:3000/root/FileUploadApi.GetUploadedFiles
```

Response format:
```json
{
  "result": true,
  "Files": [
    {
      "FileName": "example.txt",
      "Size": 1234
    }
  ]
}
```

## Key Differences from DMVCFramework

### DMVCFramework Approach

- Uses `ReqMulti` unit for multipart handling
- Controller with `TMVCController` base class
- Mustache templates for HTML rendering
- Middleware for static file serving
- Automatic file access via `Context.Request.Files[0]`

### mORMot2 Approach

- Built-in `MultiPartFormDataDecode` helper from `mormot.core.buffers`
- Interface-based service architecture
- HTML generated directly in code (or could use templates)
- Static file serving integrated in HTTP server override
- Idiomatic multipart handling using `TMultiPartDynArray`

### Advantages of mORMot2 Approach

1. **No external dependencies**: Self-contained multipart parsing
2. **High performance**: Direct HTTP server override, no middleware overhead
3. **Type-safe REST API**: Interface-based services with automatic JSON serialization
4. **Fine-grained control**: Full control over request/response handling
5. **Integrated logging**: Built-in TSynLog for comprehensive logging

## Error Handling

The sample includes validation for:

- Empty filenames
- Duplicate files (returns HTTP 400)
- Invalid Content-Type (only accepts multipart/form-data)
- File I/O errors (returns HTTP 500)
- Directory traversal attempts (rejects ".." in paths)

## Logging

All operations are logged using mORMot2's `TSynLog`:

- File upload successes with filename
- Errors with detailed messages
- Static file serving (trace level)
- Server start/stop events

Log files are created in the executable directory.

## Customization

### Change Upload Folder

Edit `server.pas`:

```pascal
fUploadFolder := IncludeTrailingPathDelimiter(
  Executable.ProgramFilePath + '..\myuploads\');
```

### Change Port

Edit `FileUploadSample.dpr`:

```pascal
srv := TFileUploadSampleServer.Create('8080');  // Change from '3000'
```

### Modify HTML Template

Edit `TFileUploadHttpServer.GenerateMainPage` in `server.pas`.

### Add File Size Limits

In `HandleMultipartUpload`, add:

```pascal
const MAX_FILE_SIZE = 10 * 1024 * 1024;  // 10 MB

if Length(body) > MAX_FILE_SIZE then
begin
  result := HTTP_PAYLOADTOOLARGE;
  Ctxt.OutContent := 'File too large';
  Exit;
end;
```

## Related Samples

- **15-middleware_staticfiles**: Static file serving middleware
- **04-renders**: HTML rendering techniques
- **08-basicauth**: Adding authentication to file uploads
- **12-middleware**: General middleware patterns

## License

This sample is part of the mORMot2 framework and follows the same license terms.

## See Also

- [mORMot2 Documentation](https://synopse.info/fossil/wiki/Synopse+OpenSource)
- [DMVCFramework Conversion Guide](../CONVERSION-GUIDE.md)
- [Original DMVCFramework Sample](https://github.com/danieleteti/delphimvcframework/tree/master/samples/file_upload)
