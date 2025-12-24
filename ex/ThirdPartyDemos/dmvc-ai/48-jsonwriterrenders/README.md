# 48 - JSON Writer Custom Renders

## Overview

This sample demonstrates how mORMot2 handles custom JSON rendering as an equivalent to DMVCFramework's `TJsonTextWriter` approach.

## DMVC Framework Original

The original DMVC sample shows manual JSON construction using `System.JSON.Writers.TJsonTextWriter`:

```pascal
var lJSONWriter := TJsonTextWriter.Create(TStringWriter.Create(), True);
try
  lJSONWriter.Formatting := TJsonFormatting.Indented;
  lJSONWriter.WriteStartObject;
  lJSONWriter.WritePropertyName('Users');
  lJSONWriter.WriteStartArray;
  var Arr := ['Daniele','Peter','Scott'];
  for var oUser in Arr do
  begin
    lJSONWriter.WriteStartObject;
    lJSONWriter.WritePropertyName('UserName');
    lJSONWriter.WriteValue(oUser);
    lJSONWriter.WriteEndObject;
  end;
  lJSONWriter.WriteEndArray;
  lJSONWriter.WriteEndObject;
  Result := lJSONWriter.Writer.ToString;
finally
  lJSONWriter.Free;
end;
```

## mORMot2 Approach

mORMot2 uses automatic JSON serialization through record types, eliminating the need for manual JSON construction:

```pascal
type
  TUserDTO = packed record
    UserName: RawUtf8;
  end;

  TUsersListDTO = packed record
    Users: TUserDTODynArray;
  end;

function TJSONWriterSample.GetUsers: TUsersListDTO;
begin
  SetLength(result.Users, 3);
  result.Users[0].UserName := 'Daniele';
  result.Users[1].UserName := 'Peter';
  result.Users[2].UserName := 'Scott';
end;
```

The mORMot2 framework automatically serializes the result to JSON with proper formatting.

## Key Differences

| Aspect | DMVC | mORMot2 |
|--------|------|---------|
| JSON Construction | Manual with TJsonTextWriter | Automatic via RTTI |
| Code Verbosity | High (explicit write calls) | Low (just populate data) |
| Type Safety | Runtime strings | Compile-time records |
| Performance | Good | Excellent (optimized RTTI) |
| Maintainability | More code to maintain | Less code, clearer intent |

## Running the Sample

```bash
# Start the server
48-jsonwriterrenders.exe

# Test the endpoint
curl http://localhost:8080/root/JSONWriterSample.GetUsers
```

Expected output:
```json
{
  "Users": [
    {"UserName": "Daniele"},
    {"UserName": "Peter"},
    {"UserName": "Scott"}
  ]
}
```

## Implementation Notes

1. **No Manual JSON Writing**: mORMot2's RTTI-based serialization handles all JSON construction
2. **Record Types**: Using `packed record` provides optimal performance and memory layout
3. **Type Safety**: Compile-time checking prevents runtime JSON construction errors
4. **Automatic Formatting**: mORMot2 handles indentation and formatting automatically
5. **Zero Memory Management**: No need to manually free JSON writers or builders

## Files

- `src/api.interfaces.pas` - Service interface and DTO definitions
- `src/api.impl.pas` - Service implementation
- `src/server.pas` - HTTP server setup
- `48-jsonwriterrenders.dpr` - Main program

## See Also

- Sample 04 (Renders) - More comprehensive rendering examples
- Sample 49 (Binary Content) - Binary data handling
