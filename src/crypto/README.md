# mORMot Cryptographic Units

## Folder Content

This folder hosts the Cryptographic Units of the *mORMot* Open Source framework, version 2.

## Cryptography

Those units implement hashing, message digests, encryption, and asymetric cryptography.

They are written in a cross-platform way in mind, with a very efficient stand-alone version (using optimized pascal code and assembly), or via an external OpenSSL library.

Legal Notice: as stated by [our licensing terms](../../LICENCE.md), make sure that you comply to any restriction about the use of cryptographic software in your country.


## Units Presentation

### mormot.crypto.core

High-Performance Cryptographic features shared by all framework units
- Low-Level Memory Buffers Helper Functions
- AES Encoding/Decoding with optimized asm and AES-NI/CLMUL support
- AES-256 Cryptographic Pseudorandom Number Generator (CSPRNG)
- SHA-2 SHA-3 Secure Hashing
- HMAC Authentication over SHA and CRC32C
- PBKDF2 Key Derivation over SHA and CRC32C
- Digest/Hash to Hexadecimal Text Conversion
- Deprecated MD5 RC4 SHA-1 Algorithms
- Deprecated Weak AES/SHA Process

This unit is validated against OpenSSL for correctness.
Optimized assembly is located in separated `mormot.crypto.core.asmx64.inc` and `mormot.crypto.core.asmx86.inc` files.
It is fully stand-alone, and faster than OpenSSL on x86_64 (but AES-GCM).

### mormot.crypto.secure

Authentication and Security types shared by all framework units.
- `TSyn*Password` and `TSynConnectionDefinition` Classes
- Reusable Authentication Classes
- High-Level `TSynSigner`/`TSynHasher` Multi-Algorithm Wrappers
- 64-bit `TSynUniqueIdentifier` and its efficient Generator
- `IProtocol` Safe Communication with Unilateral or Mutual Authentication

### mormot.crypto.ecc256r1

High-Performance *secp256r1/NISTP-256/prime256v1* Elliptic-Curve Cryptography
- Low-Level ECC *secp256r1* ECDSA and ECDH Functions
- Middle-Level Certificate-based Public Key Cryptography

Pascal and optimized gcc static binaries are included.
If `mormot.crypto.openssl.RegisterOpenSsl` is called, faster *OpenSSL* library will be used.

### mormot.crypto.ecc

Certificate-based Public Key Cryptography Classes
- High-Level Certificate-based Public Key Cryptography
- `IProtocol` Implemented using Public Key Cryptography

### mormot.crypto.jwt

JSON Web Tokens (JWT) Implementation - see RFC 7797
- Abstract JWT Parsing and Computation
- JWT Implementation of `HS*` and `S3*` Symmetric Algorithms
- JWT Implementation of `ES256` Asymmetric Algorithm

### mormot.crypto.openssl

High-Performance Cryptographic Features using *OpenSSL* 1.1.1
- *OpenSSL* Cryptographic Pseudorandom Number Generator (CSPRNG)
- AES Cypher/Uncypher in various Modes
- Hashers and Signers OpenSSL Wrappers
- *OpenSSL* Asymetric Cryptography
- JWT Implementation using any OpenSSL Algorithm
- Register *OpenSSL* to our General Cryptography Catalog

TL;DR: On x86_64, our `mormot.crypto.pas` asm is stand-alone and faster than *OpenSSL* for most algorithms, and only 20% slower for `AES-GCM`.
For `ECC`, our `mormot.crypto.ecc256r1` unit is noticeably slower than *OpenSSL*.
