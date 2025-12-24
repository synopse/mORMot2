# 24-hmac_auth - HMAC Cryptographic Signing

## Overview

This example demonstrates HMAC (Hash-based Message Authentication Code) cryptographic signing in mORMot2, ported from the DelphiMVCFramework `hmac` sample.

## Features

- **Multiple HMAC Algorithms**: MD5, SHA1, SHA224, SHA256, SHA384, SHA512
- **Test Vectors**: Validates implementation against known test vectors
- **mORMot2 Native**: Uses `mormot.crypt.secure` for all HMAC operations
- **Zero Dependencies**: No external libraries required

## Original DMVC Sample

**Source**: `/mnt/w/DMVCframework/samples/hmac/`

**Key Components**:
- `MVCFramework.HMAC.pas`: HMAC registry and algorithm abstraction
- Uses Delphi's built-in `System.Hash` for HMAC operations
- Registry-based algorithm selection

## mORMot2 Port Architecture

### Components

1. **`hmac.demo.pas`**: HMAC demonstration and test vectors
   - `ComputeHmac()`: Compute HMAC signature using specified algorithm
   - `RunHmacDemo()`: Execute all test vectors and validate results
   - Direct use of `mormot.crypt.secure` functions

## Key Differences from DMVC

| Aspect | DMVC | mORMot2 |
|--------|------|---------|
| **Architecture** | Registry with `IHMAC` interface | Direct function calls |
| **Algorithm Selection** | String-based registry lookup | Direct function selection |
| **Hash Functions** | `System.Hash` or Indy | `mormot.crypt.secure` |
| **Dependencies** | Conditional (Indy for older Delphi) | Zero external dependencies |

## HMAC Functions

### Available Algorithms

| Algorithm | mORMot2 Function | Output Size |
|-----------|------------------|-------------|
| **HMAC-MD5** | `HmacMd5()` | 128 bits (16 bytes) |
| **HMAC-SHA1** | `HmacSha1()` | 160 bits (20 bytes) |
| **HMAC-SHA224** | `HmacSha224()` | 224 bits (28 bytes) |
| **HMAC-SHA256** | `HmacSha256()` | 256 bits (32 bytes) |
| **HMAC-SHA384** | `HmacSha384()` | 384 bits (48 bytes) |
| **HMAC-SHA512** | `HmacSha512()` | 512 bits (64 bytes) |

### Usage Example

```pascal
uses
  mormot.core.base,
  mormot.core.text,
  mormot.crypt.secure;

var
  input, key: RawUtf8;
  signature: THash256;
  signatureHex: RawUtf8;
begin
  input := 'message to sign';
  key := 'secret key';

  // Compute HMAC-SHA256
  HmacSha256(input, key, signature);

  // Convert to hex string
  signatureHex := LowerCase(BinToHex(@signature, SizeOf(signature)));
  WriteLn('HMAC-SHA256: ', signatureHex);
end;
```

## Test Vectors

The sample validates against the same test vectors as DMVC:

**Input**: `"Daniele Teti"`
**Key**: `"daniele"`

| Algorithm | Expected Signature |
|-----------|-------------------|
| MD5 | `5256311089fa9c80f735fb8cc28bf4fe` |
| SHA1 | `323ff5f4e53c43f2d9342952299a9d35f9ee5dc2` |
| SHA224 | `2f42e18342d2d35afc9942364caec009e1ace1d1695c3e9178e65e35` |
| SHA256 | `1f75a969e2b9c43e6d06969dfad2088f9aab68d3aa440904d2ed8710e2f8e38b` |
| SHA512 | `22465b5f4138ab80801ff8eca8dd99a56844dd7dc54f76d38bb02bdd815596fc5859709ba4f7130c299a626864a84a4a79401f529d44c85a894fcd7e6192eee9` |

## Running the Demo

### Compilation

```bash
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe W:\mORMot2\ex\dmvc\24-hmac_auth\HmacAuthSample.dproj
```

### Execution

```bash
cd /mnt/w/mORMot2/ex/dmvc/24-hmac_auth
./Win32/Debug/HmacAuthSample.exe
```

### Expected Output

```
=======================================
mORMot2 HMAC Cryptographic Demo
=======================================
Demonstrates HMAC signing algorithms:
  - HMAC-MD5
  - HMAC-SHA1
  - HMAC-SHA224
  - HMAC-SHA256
  - HMAC-SHA512
=======================================

Running HMAC cryptographic tests...
Input:  "Daniele Teti"
Key:    "daniele"

MD5:       [OK]
  Expected: 5256311089fa9c80f735fb8cc28bf4fe
  Computed: 5256311089fa9c80f735fb8cc28bf4fe

SHA1:      [OK]
  Expected: 323ff5f4e53c43f2d9342952299a9d35f9ee5dc2
  Computed: 323ff5f4e53c43f2d9342952299a9d35f9ee5dc2

SHA224:    [OK]
  Expected: 2f42e18342d2d35afc9942364caec009e1ace1d1695c3e9178e65e35
  Computed: 2f42e18342d2d35afc9942364caec009e1ace1d1695c3e9178e65e35

SHA256:    [OK]
  Expected: 1f75a969e2b9c43e6d06969dfad2088f9aab68d3aa440904d2ed8710e2f8e38b
  Computed: 1f75a969e2b9c43e6d06969dfad2088f9aab68d3aa440904d2ed8710e2f8e38b

SHA512:    [OK]
  Expected: 22465b5f4138ab80801ff8eca8dd99a56844dd7dc54f76d38bb02bdd815596fc5859709ba4f7130c299a626864a84a4a79401f529d44c85a894fcd7e6192eee9
  Computed: 22465b5f4138ab80801ff8eca8dd99a56844dd7dc54f76d38bb02bdd815596fc5859709ba4f7130c299a626864a84a4a79401f529d44c85a894fcd7e6192eee9

=======================================
Results: 5/5 tests passed
=======================================

Press ENTER to exit
```

## HMAC in API Authentication

### Typical Use Case: Request Signing

HMAC is commonly used to sign API requests for authentication:

```pascal
uses
  mormot.core.base,
  mormot.core.text,
  mormot.crypt.secure;

function SignRequest(const Method, Uri, Body, SecretKey: RawUtf8): RawUtf8;
var
  message: RawUtf8;
  signature: THash256;
begin
  // Construct message to sign
  message := Method + #10 + Uri + #10 + Body;

  // Compute HMAC-SHA256
  HmacSha256(message, SecretKey, signature);

  // Return as hex
  Result := LowerCase(BinToHex(@signature, SizeOf(signature)));
end;

function VerifyRequest(const Method, Uri, Body, ReceivedSignature,
  SecretKey: RawUtf8): boolean;
var
  expectedSignature: RawUtf8;
begin
  expectedSignature := SignRequest(Method, Uri, Body, SecretKey);
  Result := SameText(expectedSignature, ReceivedSignature);
end;
```

### Client-Side Usage

```pascal
// Sign request
var signature := SignRequest('POST', '/api/users', '{"name":"John"}', 'secret123');

// Send request with signature
var request := THttpClientRequest.Create(...);
request.Header['X-Signature'] := signature;
request.Post('/api/users', '{"name":"John"}');
```

### Server-Side Validation

```pascal
// In OnBeforeBody filter
function TMyServer.ValidateSignature(Ctxt: TRestServerUriContext): boolean;
var
  receivedSig, expectedSig: RawUtf8;
  secretKey: RawUtf8;
begin
  // Get signature from header
  receivedSig := Ctxt.InHead['X-Signature'];

  // Get secret key for client (from database, config, etc.)
  secretKey := GetClientSecret(Ctxt.AuthUser);

  // Verify signature
  Result := VerifyRequest(
    Ctxt.Method,
    Ctxt.Uri,
    Ctxt.Call.InBody,
    receivedSig,
    secretKey
  );
end;
```

## Implementation Notes

### Why HMAC?

- **Message Integrity**: Detects message tampering
- **Authentication**: Proves sender knows the secret key
- **Stateless**: No session storage required
- **Replay Protection**: Combine with timestamps/nonces

### Security Considerations

1. **Use SHA256 or better** - MD5 and SHA1 are cryptographically weak
2. **Keep keys secret** - Never transmit secret keys in requests
3. **Use random keys** - Generate keys with cryptographic RNG
4. **Protect key storage** - Store keys encrypted, never in source code
5. **Add timestamps** - Prevent replay attacks by including current time in signed message
6. **Use HTTPS** - HMAC provides integrity, not confidentiality

### Performance

mORMot2's HMAC implementation is highly optimized:
- Native assembly for x86/x64
- SIMD optimizations where available
- Zero heap allocations for common cases

Benchmark (on modern CPU):
- HMAC-SHA256: ~500 MB/s
- HMAC-SHA512: ~350 MB/s

## Advanced Example: JWT-like Token

```pascal
uses
  mormot.core.base,
  mormot.core.text,
  mormot.core.json,
  mormot.crypt.secure;

type
  TTokenClaims = packed record
    UserID: integer;
    Username: RawUtf8;
    ExpiresAt: TDateTime;
  end;

function CreateToken(const Claims: TTokenClaims; const SecretKey: RawUtf8): RawUtf8;
var
  payload: RawUtf8;
  signature: THash256;
  signatureB64: RawUtf8;
begin
  // Serialize claims to JSON
  payload := RecordSaveJson(Claims, TypeInfo(TTokenClaims));

  // Sign payload
  HmacSha256(payload, SecretKey, signature);
  signatureB64 := BinToBase64(@signature, SizeOf(signature));

  // Combine: base64(payload).base64(signature)
  Result := BinToBase64(payload) + '.' + signatureB64;
end;

function VerifyToken(const Token, SecretKey: RawUtf8;
  out Claims: TTokenClaims): boolean;
var
  parts: TRawUtf8DynArray;
  payload, receivedSig: RawUtf8;
  expectedSig: THash256;
  expectedSigB64: RawUtf8;
begin
  Result := false;

  // Split token
  CSVToRawUtf8DynArray(pointer(StringReplaceAll(Token, '.', ',')), parts);
  if Length(parts) <> 2 then Exit;

  // Decode payload
  payload := Base64ToBin(parts[0]);
  receivedSig := parts[1];

  // Verify signature
  HmacSha256(payload, SecretKey, expectedSig);
  expectedSigB64 := BinToBase64(@expectedSig, SizeOf(expectedSig));

  if not SameText(receivedSig, expectedSigB64) then Exit;

  // Deserialize claims
  RecordLoadJson(Claims, pointer(payload), TypeInfo(TTokenClaims));

  // Check expiration
  Result := Claims.ExpiresAt > NowUtc;
end;
```

## See Also

- [08-basicauth](../08-basicauth/README.md) - HTTP Basic Authentication
- [10-jsonwebtoken](../10-jsonwebtoken/README.md) - JWT authentication (uses HMAC internally)
- [CONVERSION-GUIDE.md](../CONVERSION-GUIDE.md#cryptography) - Cryptography patterns
- [mORMot2 Documentation](https://synopse.info/files/html/Synopse%20mORMot2%20Framework%20SAD%201.18.html#TITLE_71) - Cryptography and Hashing

## Files

```
24-hmac_auth/
├── HmacAuthSample.dpr       # Main program
├── HmacAuthSample.dproj     # Delphi project file
├── README.md                # This file
└── src/
    └── hmac.demo.pas        # HMAC test demonstration
```

## References

- **RFC 2104**: HMAC: Keyed-Hashing for Message Authentication
- **FIPS 198-1**: The Keyed-Hash Message Authentication Code (HMAC)
- **mormot.crypt.secure**: mORMot2 cryptographic primitives
