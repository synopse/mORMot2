# 49 - Render Binary Contents

## Overview

This sample demonstrates serving binary files through mORMot2 with automatic content-type detection, equivalent to DMVCFramework's binary content rendering.

## DMVC Framework Original

The original DMVC sample shows two approaches for serving binary content:

### Functional Approach (Function Returns Stream)
```pascal
function TMyController.GetFileByName(const FileName: String): TStream;
begin
  // ... path validation ...
  Context.Response.ContentType := TMVCMediaType.IMAGE_JPEG;
  StatusCode := HTTP_STATUS.OK;
  Result := TFileStream.Create(lPath, fmOpenRead, fmShareDenyNone)
end;
```

### Procedural Approach (SendFile)
```pascal
procedure TMyController.GetStreamByFileName(const FileName: String);
begin
  // ... path validation ...
  Context.Response.ContentType := TMVCMediaType.IMAGE_JPEG;
  StatusCode := HTTP_STATUS.OK;
  SendFile(lPath);
end;
```

## mORMot2 Approach

mORMot2 uses JSON-RPC style interfaces with base64 encoding for binary transport:

```pascal
type
  TFileDownloadDTO = packed record
    FileName: RawUtf8;
    ContentType: RawUtf8;
    Data: RawByteString; // Base64 encoded
  end;

function TBinaryContentSample.GetFileByName(const FileName: RawUtf8): TFileDownloadDTO;
var
  filePath: TFileName;
  fileContent: RawByteString;
begin
  filePath := GetFilePath(FileName);
  ValidateFilePath(filePath);

  fileContent := StringFromFile(filePath);

  result.FileName := FileName;
  result.ContentType := GetContentType(FileName);
  result.Data := BinToBase64(fileContent);
end;
```

## Key Differences

| Aspect | DMVC | mORMot2 |
|--------|------|---------|
| Transport | Raw binary stream | Base64 in JSON |
| Content-Type | Set via Context | Returned in DTO |
| Method Style | Both functional & procedural | Interface-based |
| Path Handling | Manual validation | Centralized helper |
| Security | Directory traversal check | Same security approach |
| File Reading | TFileStream | StringFromFile |

## Running the Sample

```bash
# Start the server
49-render_binary_contents.exe

# Test file download
curl "http://localhost:8080/root/BinaryContentSample.GetFileByName?FileName=test.txt"

# Test file upload
curl -X POST http://localhost:8080/root/BinaryContentSample.UploadBinaryData \
  -H "Content-Type: application/json" \
  -d '{"fieldname":"file","filename":"test.txt","contenttype":"text/plain","data":"SGVsbG8gV29ybGQ="}'
```

## Implementation Notes

### File Download

1. **Path Validation**: Both samples implement directory traversal protection
2. **Content-Type Detection**: Automatic based on file extension (.jpg, .pdf, .txt, etc.)
3. **Base64 Encoding**: Required for JSON transport (mORMot2's JSON-RPC style)
4. **Error Handling**: Proper HTTP status codes (404 for not found, 400 for invalid path)

### File Upload

```pascal
function UploadBinaryData(const fieldname, filename, contenttype: RawUtf8;
  const data: RawByteString): TFileUploadResponseDTO;
```

Returns structured response:
```json
{
  "Success": true,
  "Message": "File uploaded successfully",
  "SavedFileName": "2024-12-20_test.txt",
  "Reference": "files/2024-12-20_test.txt"
}
```

### Security Features

Both implementations include:
- Directory traversal protection
- File existence validation
- Path normalization
- Safe filename generation (with timestamps)

### Content Types

Supported extensions:
- `.jpg` → `image/jpeg`
- `.png` → `image/png`
- `.pdf` → `application/pdf`
- `.txt` → `text/plain`
- `.html` → `text/html`
- Others → `application/octet-stream`

## Files

- `src/api.interfaces.pas` - Service interface and DTOs
- `src/api.impl.pas` - Implementation with file handling
- `src/server.pas` - HTTP server setup
- `49-render_binary_contents.dpr` - Main program

## Directory Structure

```
49-render_binary_contents/
├── bin/                          # Compiled executable
│   └── Win32/Debug/
│       └── 49-render_binary_contents.exe
└── files_repository/            # File storage (create manually)
    ├── test.txt
    ├── sample.jpg
    └── document.pdf
```

**Note**: Create the `files_repository` folder one level up from the executable directory.

## See Also

- Sample 04 (Renders) - JSON and text rendering
- Sample 48 (JSON Writer) - Custom JSON construction
