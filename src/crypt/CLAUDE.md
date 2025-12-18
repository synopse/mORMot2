# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

The mORMot2 cryptographic units (`/mnt/w/mORMot2/src/crypt`) provide a comprehensive, high-performance cryptographic library for Delphi and FPC. The implementation emphasizes **stand-alone performance** and **cross-platform compatibility** with optional OpenSSL acceleration.

### Key Characteristics

- **Stand-alone by default**: Pure Pascal with optimized assembly (x86_64/x86/ARM)
- **Faster than OpenSSL**: Native implementation outperforms OpenSSL for most algorithms on x86_64 (except AES-GCM)
- **OpenSSL optional**: Can delegate to OpenSSL 1.1/3.x for algorithms like RSA/ECC where OpenSSL is faster
- **Factory-based architecture**: High-level `Rnd()`/`Hash()`/`Sign()`/`Cipher()`/`Asym()`/`Cert()`/`Store()` factories hide complexity
- **Validated against OpenSSL**: Correctness verified against OpenSSL reference implementation

## Architecture Layers

### Layer 1: Low-Level Primitives (`mormot.crypt.core.pas`)

**Purpose**: Optimized cryptographic primitives with assembly acceleration

**Key Features**:
- AES-256 with AES-NI/CLMUL support (x86_64/x86)
- SHA-2 (SHA-256, SHA-512) and SHA-3 (Keccak) with SSE/AVX2
- HMAC, PBKDF2 key derivation
- AES-based CSPRNG (`TAesPrng`)
- Deprecated: MD5, SHA-1

**Assembly Files**:
- `mormot.crypt.core.asmx64.inc` - x86_64 optimizations
- `mormot.crypt.core.asmx86.inc` - x86 optimizations

**Performance Notes**:
- Stand-alone AES-CTR/CFB/OFB: **Faster than OpenSSL**
- AES-GCM: ~20% slower than OpenSSL (but faster than OpenSSL 3.0)
- SHA-256/SHA-512: Comparable to OpenSSL with SSE4

### Layer 2: High-Level Abstractions (`mormot.crypt.secure.pas`)

**Purpose**: Authentication, key management, and **factory functions**

**Critical Factories** (use these for simplicity):
```pascal
function Rnd(name: RawUtf8 = 'rnd-default'): TCryptRandom;
function Hash(name: RawUtf8): ICryptHash;
function Sign(key: pointer; keylen: PtrInt; name: RawUtf8): ICryptHash;
function Cipher(name: RawUtf8; key: pointer; encrypt: boolean): ICryptCipher;
function Asym(name: RawUtf8): TCryptAsym;
function Cert(name: RawUtf8): ICryptCert;
function Store(name: RawUtf8): ICryptStore;
```

**Key Classes**:
- `TObjectWithPassword` - Encrypted password storage
- `TSynSigner`/`TSynHasher` - Multi-algorithm wrappers
- `TSynUniqueIdentifier` - 64-bit unique ID generator
- `IProtocol` - Safe communication with authentication
- Digest authentication (client/server)

### Layer 3: Algorithm-Specific Units

#### `mormot.crypt.ecc256r1.pas` + `mormot.crypt.ecc.pas`
- **Elliptic Curve Cryptography** (secp256r1/NISTP-256/prime256v1)
- ECDSA signatures, ECDH key exchange
- Certificate-based public key cryptography
- Registers `caaES256` to `Asym()` factory

#### `mormot.crypt.rsa.pas`
- **RSA asymmetric cryptography** (default: 2048-bit keys)
- RS256/RS384/RS512 and PS256/PS384/PS512 (PSS padding)
- Registers `caaRS256`..`caaPS512` to `Asym()` factory
- **Note**: Slower than OpenSSL, consider using OpenSSL for production

#### `mormot.crypt.x509.pas`
- **X.509 certificate** implementation (RFC 5280)
- CSR (Certificate Signing Request), CRL (Revocation Lists)
- PKI (Private Key Infrastructure)
- Registers to `Cert()`/`Store()` factories

#### `mormot.crypt.jwt.pas`
- **JSON Web Tokens** (RFC 7797)
- Symmetric: HS256, HS384, HS512, S3224, S3256, etc.
- Asymmetric: ES256 (ECC), RS256/RS384/RS512 (RSA)
- **`TJwtCrypt`** - Recommended high-level class using factories

#### `mormot.crypt.openssl.pas`
- **OpenSSL 1.1/3.x bindings** (optional)
- **CRITICAL**: Define `USE_OPENSSL` in project options on Windows
- Registers faster implementations to factories via `RegisterOpenSsl`
- Classes: `TAesPrngOsl`, `TAesEcbOsl`, `TJwtOpenSsl`, etc.

#### `mormot.crypt.pkcs11.pas`
- **Hardware Security Module** (HSM) support via PKCS#11
- Registers to `Asym()`/`Cert()` factories

#### `mormot.crypt.other.pas`
- **Deprecated/legacy** algorithms: MD4, RC4
- BlowFish encryption
- BCrypt, SCrypt password hashing

## Common Patterns

### Using High-Level Factories (Recommended)

```pascal
uses mormot.crypt.secure;

// Random number generation
var rnd := Rnd('rnd-default');
var randomBytes := rnd.Random(16);

// Hashing
var hasher := Hash('sha256');
var digest := hasher.Full(data, length);

// Signing (HMAC)
var signer := Sign(@secret[1], length(secret), 'hmac-sha256');
var signature := signer.Full(data, length);

// Encryption
var cipher := Cipher('aes-256-ctr', @key[1], true); // encrypt=true
cipher.Process(plaintext, ciphertext, '');

// Asymmetric (ECC/RSA)
var asym := Asym('es256'); // or 'rs256', 'ps256'
asym.GeneratePem(publicKey, privateKey, '');
asym.Sign(message, privateKey, signature);
asym.Verify(message, publicKey, signature);

// Certificates
var cert := Cert('x509-es256'); // or 'x509-rs256'
cert.Generate(...);

// JWT
uses mormot.crypt.jwt;
var jwt := TJwtCrypt.Create;
jwt.Compute(payload, privateKey);
jwt.Verify(token, publicKey);
```

### Performance Optimization

1. **Default choice**: Use stand-alone implementation (no dependencies)
   - Faster for AES-CTR/CFB/OFB, comparable for hashing

2. **For RSA/ECC intensive workloads**:
   ```pascal
   uses mormot.crypt.openssl; // Add to uses

   initialization
     RegisterOpenSsl; // Registers faster OpenSSL implementations
   ```

3. **For AES-GCM only**: OpenSSL is ~20% faster
   ```pascal
   var cipher := Cipher('aes-256-gcm-osl', @key[1], true);
   ```

## OpenSSL Integration

### Conditional Compilation (Windows)
- **CRITICAL**: Define `USE_OPENSSL` in project options (Project → Options → Delphi Compiler → Conditional defines)
- Without this, `mormot.crypt.openssl.pas` compiles as a void unit

### Registration Pattern
```pascal
uses mormot.crypt.openssl, mormot.crypt.x509;

initialization
  RegisterOpenSsl;  // Registers OpenSSL to factories
  RegisterX509;     // Extends X.509 support (after OpenSSL)
```

### Effect of Registration
- `Asym('es256')` → Uses OpenSSL ECC instead of native Pascal
- `Asym('rs256')` → Uses OpenSSL RSA instead of native Pascal
- `Cipher('aes-256-gcm')` → Uses OpenSSL AES-GCM
- `Rnd()` → Can optionally use OpenSSL RAND_bytes

## Testing

### Regression Tests
- Location: `/mnt/w/mORMot2/test/`
- Policy: Framework validated against OpenSSL for correctness
- Coverage: Unit tests for all cryptographic primitives

### Performance Benchmarks
- Built into test suite
- Compare stand-alone vs OpenSSL implementations
- Platform-specific results (x86_64 vs x86 vs ARM)

## Cross-Platform Considerations

### Assembly Optimizations
- **x86_64**: AES-NI, CLMUL (pclmulqdq), SSE4, AVX2
  - Conditionals: `USEAESNI64`, `USECLMUL`, `USEGCMAVX`
- **x86**: AES-NI (4x interleaved), SSE4
  - Conditionals: `USEAESNI32`, `USECLMUL`
- **ARM64**: ARMv8 Crypto Extensions
  - Conditional: `USEARMCRYPTO` (validated on Linux/Android only)

### Platform-Specific External Libraries
- **Windows/Linux x86_64**: External CRC32C, SHA-512 (`.o`/`.obj` files)
- **Windows/Linux x86**: External SHA-512 (`.o`/`.obj` files)

## Legal Notice

As stated in LICENSE.md: **Ensure compliance with cryptographic software restrictions in your country.**

## Key Files Summary

| File | Purpose | Performance |
|------|---------|-------------|
| `mormot.crypt.core.pas` | Low-level AES, SHA, HMAC | Faster than OpenSSL (most) |
| `mormot.crypt.secure.pas` | Factories, auth, key mgmt | N/A (abstraction) |
| `mormot.crypt.ecc256r1.pas` | Native ECC secp256r1 | Slower than OpenSSL |
| `mormot.crypt.ecc.pas` | High-level ECC | Uses ecc256r1 |
| `mormot.crypt.rsa.pas` | Native RSA | Slower than OpenSSL |
| `mormot.crypt.x509.pas` | X.509 certificates | N/A (data structures) |
| `mormot.crypt.jwt.pas` | JSON Web Tokens | N/A (protocol) |
| `mormot.crypt.openssl.pas` | OpenSSL bindings | Faster RSA/ECC |
| `mormot.crypt.pkcs11.pas` | HSM support | Hardware-dependent |
| `mormot.crypt.other.pas` | Deprecated algorithms | Avoid in new code |

## Anti-Patterns to Avoid

1. **Don't instantiate low-level classes directly** - Use factories instead
   ```pascal
   // BAD
   var aes := TAes.Create(key, 256);

   // GOOD
   var cipher := Cipher('aes-256-ctr', @key[1], true);
   ```

2. **Don't use deprecated algorithms** (MD5, SHA-1, RC4) for security
   - Only acceptable for legacy compatibility (e.g., digest auth)

3. **Don't use native RSA/ECC for production** without benchmarking
   - Consider `RegisterOpenSsl` for performance-critical workloads

4. **Don't forget `USE_OPENSSL`** conditional on Windows
   - Silent failure: code compiles but OpenSSL features unavailable

## Documentation

**📖 SAD Chapters**:
- [Chapter 21: Security](/mnt/w/mORMot2/DOCS/mORMot2-SAD-Chapter-21.md) - Authentication, hashing, encryption
- [Chapter 23: Asymmetric Encryption](/mnt/w/mORMot2/DOCS/mORMot2-SAD-Chapter-23.md) - ECC, RSA, X.509 certificates

## Version Compatibility

- **mORMot 2**: Current version (2025)
- **Delphi**: 7 to 12.2 Athenes (Windows only for crypto units)
- **FPC**: 3.2.2 fixes branch (3.2.2 stable has variant regression)
- **OpenSSL**: 1.1.x and 3.x supported

---

**Last Updated**: 2025-10-10
**Framework**: Synopse mORMot 2
**License**: MPL 1.1 / GPL 2.0 / LGPL 2.1 (disjunctive three-license)
