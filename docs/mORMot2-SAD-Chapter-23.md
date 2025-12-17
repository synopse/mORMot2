# 23. Asymmetric Encryption

*Elliptic Curve Cryptography and Public Key Infrastructure*

mORMot provides a complete asymmetric cryptography stack based on Elliptic Curve Cryptography (ECC), enabling digital signatures, secure encryption, and certificate management without external dependencies.

---

## 23.1. Public-Key Cryptography Fundamentals

### 23.1.1. Asymmetric vs Symmetric Encryption

| Aspect | Symmetric | Asymmetric |
|--------|-----------|------------|
| Keys | Single shared secret | Public/private pair |
| Speed | Fast | Slower |
| Key distribution | Challenging | Easy (public keys shared freely) |
| Use case | Bulk data encryption | Key exchange, signatures |

### 23.1.2. Public Key Operations

With a public/private key pair, you can:

1. **Sign** - Create a digital signature with your private key
2. **Verify** - Confirm a signature with the signer's public key
3. **Encrypt** - Encrypt data with recipient's public key
4. **Decrypt** - Decrypt data with your private key

```
┌──────────────────────────────────────────────────────────────────┐
│                     Key Pair Operations                          │
├──────────────────────────────────────────────────────────────────┤
│                                                                  │
│  SIGNING (Authentication)           ENCRYPTION (Confidentiality) │
│  ─────────────────────────          ──────────────────────────── │
│                                                                  │
│  Alice → Private Key → Sign         Bob → Alice's Public Key     │
│          ↓                                   ↓                   │
│      Signature                          Encrypted Message        │
│          ↓                                   ↓                   │
│  Bob ← Alice's Public Key ← Verify  Alice ← Private Key ← Decrypt│
│                                                                  │
└──────────────────────────────────────────────────────────────────┘
```

---

## 23.2. Elliptic Curve Cryptography (ECC)

### 23.2.1. Why ECC?

mORMot uses ECC (specifically secp256r1/NIST P-256) for several advantages:

| Advantage | Explanation |
|-----------|-------------|
| **Smaller keys** | 256-bit ECC ≈ 3072-bit RSA security |
| **Faster operations** | Lower CPU usage per operation |
| **Perfect forward secrecy** | Fresh key per encryption |
| **No external dependencies** | Stand-alone implementation |
| **Future-proof** | Endorsed by NIST/NSA |

### 23.2.2. Key Units

| Unit | Purpose |
|------|---------|
| `mormot.crypt.ecc256r1` | Low-level ECC primitives (secp256r1) |
| `mormot.crypt.ecc` | High-level certificates, encryption, signing |
| `mormot.crypt.secure` | Factory functions (`Asym()`, `Cert()`) |
| `mormot.crypt.x509` | X.509 certificate support |

---

## 23.3. High-Level Factory API

### 23.3.1. Using the Asym() Factory

The recommended approach uses the factory pattern:

```pascal
uses
  mormot.crypt.secure,
  mormot.crypt.ecc;

var
  Asym: TCryptAsym;
  PublicKey, PrivateKey: RawByteString;
  Message, Signature: RawByteString;
begin
  // Get ECC asymmetric instance
  Asym := mormot.crypt.secure.Asym('es256');  // secp256r1 ECDSA

  // Generate key pair
  Asym.GeneratePem(PublicKey, PrivateKey, '');

  // Sign a message
  Message := 'Hello, World!';
  Asym.Sign(Message, PrivateKey, Signature);

  // Verify signature
  if Asym.Verify(Message, PublicKey, Signature) then
    Writeln('Signature valid!')
  else
    Writeln('Signature INVALID!');
end;
```

### 23.3.2. Using the Cert() Factory

For certificate-based operations:

```pascal
uses
  mormot.crypt.secure;

var
  Cert: ICryptCert;
  PublicPem, PrivatePem: RawUtf8;
begin
  // Create certificate (mORMot's proprietary format)
  Cert := mormot.crypt.secure.Cert('syn-es256');

  // Generate with subject info
  Cert.Generate([
    cuDigitalSignature,
    cuKeyEncipherment
  ], 365, nil, nil, 'CN=My Certificate');

  // Export
  PublicPem := Cert.Save(cccCertOnly, '', ccfPem);
  PrivatePem := Cert.Save(cccCertWithPrivateKey, 'password', ccfPem);
end;
```

---

## 23.4. TEccCertificate Classes

### 23.4.1. Class Hierarchy

```
TEccCertificate (public certificate)
└── TEccCertificateSecret (public + private key)

TEccCertificateChain (PKI trust chain)
```

### 23.4.2. Creating a Self-Signed Certificate

```pascal
uses
  mormot.crypt.ecc;

var
  Secret: TEccCertificateSecret;
begin
  // Create new self-signed certificate
  Secret := TEccCertificateSecret.CreateNew(
    nil,              // No authority (self-signed)
    'MyApp',          // Issuer identifier
    365               // Valid for 365 days
  );
  try
    // Save public certificate
    Secret.SaveToFile('myapp.public');

    // Save private key (password protected)
    Secret.SaveToSecureFile('myapp.private', 'MySecretPassword');
  finally
    Secret.Free;
  end;
end;
```

### 23.4.3. Loading Certificates

```pascal
var
  PublicCert: TEccCertificate;
  SecretCert: TEccCertificateSecret;
begin
  // Load public certificate
  PublicCert := TEccCertificate.Create;
  PublicCert.LoadFromFile('myapp.public');

  // Load private certificate
  SecretCert := TEccCertificateSecret.Create;
  SecretCert.LoadFromSecureFile('myapp.private', 'MySecretPassword');
end;
```

---

## 23.5. Digital Signatures

### 23.5.1. Signing Data

```pascal
uses
  mormot.crypt.ecc;

var
  Secret: TEccCertificateSecret;
  Data: RawByteString;
  Signature: TEccSignature;
begin
  Secret := TEccCertificateSecret.Create;
  Secret.LoadFromSecureFile('signer.private', 'password');
  try
    Data := 'Important document content';

    // Sign the data
    if Secret.Sign(Data, Signature) then
      Writeln('Data signed successfully');
  finally
    Secret.Free;
  end;
end;
```

### 23.5.2. Verifying Signatures

```pascal
var
  PublicCert: TEccCertificate;
  Data: RawByteString;
  Signature: TEccSignature;
begin
  PublicCert := TEccCertificate.Create;
  PublicCert.LoadFromFile('signer.public');
  try
    // Verify signature
    if PublicCert.Verify(Data, Signature) then
      Writeln('Signature is VALID')
    else
      Writeln('Signature is INVALID!');
  finally
    PublicCert.Free;
  end;
end;
```

### 23.5.3. Signing Files

```pascal
var
  Secret: TEccCertificateSecret;
begin
  Secret := TEccCertificateSecret.Create;
  Secret.LoadFromSecureFile('signer.private', 'password');
  try
    // Creates document.pdf.sign file
    Secret.SignFile('document.pdf');
  finally
    Secret.Free;
  end;
end;
```

---

## 23.6. Encryption

### 23.6.1. ECIES Encryption

mORMot uses ECIES (Elliptic Curve Integrated Encryption Scheme):

```
┌─────────────────────────────────────────────────────────────────┐
│                    ECIES Encryption Flow                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  1. Generate ephemeral ECC key pair                             │
│  2. Derive shared secret via ECDH                               │
│  3. Use PBKDF2 to derive AES key from shared secret             │
│  4. Encrypt data with AES-256                                   │
│  5. Include ephemeral public key with ciphertext                │
│                                                                 │
│  Benefits:                                                      │
│  • Perfect forward secrecy (new key per encryption)             │
│  • Combines ECC efficiency with AES speed                       │
│  • Authenticated encryption                                     │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 23.6.2. Encrypting Data

```pascal
uses
  mormot.crypt.ecc;

var
  RecipientCert: TEccCertificate;
  PlainText, Encrypted: RawByteString;
begin
  // Load recipient's public certificate
  RecipientCert := TEccCertificate.Create;
  RecipientCert.LoadFromFile('recipient.public');
  try
    PlainText := 'Secret message for recipient only';

    // Encrypt (only recipient can decrypt with their private key)
    Encrypted := RecipientCert.Encrypt(PlainText);
  finally
    RecipientCert.Free;
  end;
end;
```

### 23.6.3. Decrypting Data

```pascal
var
  SecretCert: TEccCertificateSecret;
  Encrypted, Decrypted: RawByteString;
begin
  // Load your private certificate
  SecretCert := TEccCertificateSecret.Create;
  SecretCert.LoadFromSecureFile('recipient.private', 'password');
  try
    // Decrypt
    Decrypted := SecretCert.Decrypt(Encrypted);
  finally
    SecretCert.Free;
  end;
end;
```

### 23.6.4. ECIES Algorithm Options

```pascal
type
  TEciesAlgo = (
    ecaPBKDF2_HMAC_SHA256_AES256_CFB,     // Default
    ecaPBKDF2_HMAC_SHA256_AES256_CBC,
    ecaPBKDF2_HMAC_SHA256_AES256_CTR,
    ecaPBKDF2_HMAC_SHA256_AES256_CFB_SYNLZ,  // With compression
    ecaPBKDF2_AES256_GCM,                  // Authenticated encryption
    // ... more options
  );
```

### 23.6.5. File Encryption

```pascal
var
  RecipientCert: TEccCertificate;
begin
  RecipientCert := TEccCertificate.Create;
  RecipientCert.LoadFromFile('recipient.public');
  try
    // Creates document.pdf.synecc encrypted file
    RecipientCert.EncryptFile('document.pdf');
  finally
    RecipientCert.Free;
  end;
end;
```

---

## 23.7. Certificate Chain (PKI)

### 23.7.1. Creating a Certificate Authority

```pascal
uses
  mormot.crypt.ecc;

var
  CA: TEccCertificateSecret;
begin
  // Create root CA
  CA := TEccCertificateSecret.CreateNew(
    nil,                          // Self-signed (root)
    'MyCompany Root CA',          // Issuer
    3650,                         // 10 years validity
    365,                          // 1 year signature validity
    [cuCA, cuDigitalSignature]    // CA usage
  );
  try
    CA.SaveToFile('ca.public');
    CA.SaveToSecureFile('ca.private', 'CaSecretPassword', 10000);
  finally
    CA.Free;
  end;
end;
```

### 23.7.2. Issuing Certificates

```pascal
var
  CA: TEccCertificateSecret;
  UserCert: TEccCertificateSecret;
begin
  // Load CA
  CA := TEccCertificateSecret.Create;
  CA.LoadFromSecureFile('ca.private', 'CaSecretPassword');
  try
    // Create user certificate signed by CA
    UserCert := TEccCertificateSecret.CreateNew(
      CA,                           // Signed by CA
      'John Doe',                   // Issuer/subject
      365,                          // 1 year validity
      0,                            // Default signature validity
      [cuDigitalSignature, cuKeyEncipherment]
    );
    try
      UserCert.SaveToFile('john.public');
      UserCert.SaveToSecureFile('john.private', 'JohnPassword');
    finally
      UserCert.Free;
    end;
  finally
    CA.Free;
  end;
end;
```

### 23.7.3. Validating Certificate Chain

```pascal
var
  Chain: TEccCertificateChain;
  UserCert: TEccCertificate;
  ValidationResult: TEccValidity;
begin
  // Build trust chain
  Chain := TEccCertificateChain.Create;
  try
    // Add trusted CA
    Chain.Add(TEccCertificate.CreateFromFile('ca.public'));

    // Load certificate to validate
    UserCert := TEccCertificate.Create;
    UserCert.LoadFromFile('john.public');
    try
      // Validate
      ValidationResult := Chain.IsValid(UserCert);
      case ValidationResult of
        ecvValidSigned:
          Writeln('Certificate is valid and signed by trusted CA');
        ecvValidSelfSigned:
          Writeln('Certificate is self-signed (not in chain)');
        ecvNotValidYet:
          Writeln('Certificate not yet valid');
        ecvExpired:
          Writeln('Certificate has expired');
        ecvRevoked:
          Writeln('Certificate has been revoked');
        ecvUnknownAuthority:
          Writeln('Unknown signing authority');
      else
        Writeln('Certificate validation failed');
      end;
    finally
      UserCert.Free;
    end;
  finally
    Chain.Free;
  end;
end;
```

---

## 23.8. Secure Communication Protocol

### 23.8.1. IProtocol Interface

mORMot provides `IProtocol` for encrypted communication:

```pascal
uses
  mormot.crypt.secure;

type
  IProtocol = interface
    function ProcessHandshake(const Input: RawUtf8;
      out Output: RawUtf8): TProtocolResult;
    procedure Encrypt(const Plain: RawByteString;
      out Encrypted: RawByteString);
    procedure Decrypt(const Encrypted: RawByteString;
      out Plain: RawByteString);
  end;
```

### 23.8.2. Using TProtocolEcc

```pascal
uses
  mormot.crypt.ecc;

var
  ServerProtocol, ClientProtocol: IProtocol;
  ServerCert: TEccCertificateSecret;
  ClientCert: TEccCertificateSecret;
  Handshake1, Handshake2, Handshake3: RawUtf8;
begin
  // Server setup
  ServerCert := TEccCertificateSecret.CreateFromFile('server.private', 'pwd');
  ServerProtocol := TProtocolEcc.Create(ServerCert, nil);

  // Client setup
  ClientCert := TEccCertificateSecret.CreateFromFile('client.private', 'pwd');
  ClientProtocol := TProtocolEcc.Create(ClientCert,
    TEccCertificate.CreateFromFile('server.public'));

  // Three-way handshake
  ClientProtocol.ProcessHandshake('', Handshake1);       // Client hello
  ServerProtocol.ProcessHandshake(Handshake1, Handshake2); // Server response
  ClientProtocol.ProcessHandshake(Handshake2, Handshake3); // Client finish

  // Now encrypted communication is possible
  var Plain := 'Secret message';
  var Encrypted: RawByteString;

  ClientProtocol.Encrypt(Plain, Encrypted);
  ServerProtocol.Decrypt(Encrypted, Plain);
end;
```

---

## 23.9. X.509 Certificates

### 23.9.1. X.509 Support

mORMot also supports standard X.509 certificates:

```pascal
uses
  mormot.crypt.x509;

var
  Cert: ICryptCert;
begin
  // Create X.509 certificate with ECC
  Cert := Cert('x509-es256');
  Cert.Generate([cuDigitalSignature], 365, nil, nil,
    'CN=My Server,O=My Company,C=US');

  // Save in standard formats
  Cert.Save(cccCertOnly, '', ccfPem);          // PEM format
  Cert.Save(cccCertWithPrivateKey, 'pwd', ccfBinary);  // DER/PKCS12
end;
```

### 23.9.2. Loading X.509 from PEM/DER

```pascal
var
  Cert: ICryptCert;
  PemContent: RawUtf8;
begin
  // Load PEM certificate
  PemContent := StringFromFile('server.crt');
  Cert := Cert('x509-es256');
  Cert.LoadFromPem(PemContent, '');
end;
```

---

## 23.10. Command-Line ECC Tool

### 23.10.1. Overview

mORMot includes an `ecc` command-line tool for certificate operations:

```bash
# Key generation
ecc new -auth "My Authority" -pass "secret" -days 3650

# Sign a certificate
ecc sign user.public -auth authority.private -pass "secret"

# Encrypt a file
ecc crypt document.pdf -pub recipient.public

# Decrypt a file
ecc decrypt document.pdf.synecc -priv recipient.private -pass "secret"

# Verify signature
ecc verify document.pdf.sign -pub signer.public
```

### 23.10.2. Sample Location

The ECC tool source is at:
```
/mnt/w/mORMot2/ex/ecc/
```

---

## 23.11. Integration with REST Services

### 23.11.1. Signing Service Requests

```pascal
uses
  mormot.crypt.ecc,
  mormot.rest.client;

procedure SignRequest(Client: TRestHttpClient; const Body: RawUtf8);
var
  Secret: TEccCertificateSecret;
  Signature: RawByteString;
begin
  Secret := TEccCertificateSecret.CreateFromFile('client.private', 'pwd');
  try
    Secret.Sign(Body, Signature);
    Client.SessionHttpHeader := 'X-Signature: ' + BinToBase64(Signature);
  finally
    Secret.Free;
  end;
end;
```

### 23.11.2. Server-Side Verification

```pascal
procedure VerifyRequest(Ctxt: TRestServerUriContext);
var
  PublicCert: TEccCertificate;
  SignatureB64: RawUtf8;
  Signature: RawByteString;
begin
  SignatureB64 := Ctxt.InHeader['X-Signature'];
  Signature := Base64ToBin(SignatureB64);

  PublicCert := TEccCertificate.CreateFromFile('client.public');
  try
    if not PublicCert.Verify(Ctxt.Call^.InBody, Signature) then
      Ctxt.Error('Invalid signature', HTTP_FORBIDDEN);
  finally
    PublicCert.Free;
  end;
end;
```

---

## 23.12. Performance Considerations

### 23.12.1. Stand-Alone vs OpenSSL

| Operation | Stand-alone | OpenSSL |
|-----------|-------------|---------|
| Key generation | Slower | Faster |
| ECDSA sign | Comparable | Faster |
| ECDSA verify | Comparable | Faster |
| ECDH | Comparable | Faster |

For production with heavy ECC workloads:

```pascal
uses
  mormot.crypt.openssl;

initialization
  RegisterOpenSsl;  // Use OpenSSL for ECC operations
```

### 23.12.2. Caching Keys

```pascal
var
  CachedCert: TEccCertificateSecret;

procedure InitializeCrypto;
begin
  // Load once at startup
  CachedCert := TEccCertificateSecret.CreateFromFile('server.private', 'pwd');
end;

procedure FinalizeCrypto;
begin
  CachedCert.Free;
end;
```

---

## 23.13. Security Best Practices

### 23.13.1. Private Key Protection

```pascal
// ✓ Use password-protected storage
Secret.SaveToSecureFile('key.private', 'StrongPassword', 100000);  // High PBKDF2 rounds

// ✓ Clear sensitive data after use
Secret.Free;  // Securely clears memory

// ❌ Never log or display private keys
Writeln(Secret.PrivateKey);  // NEVER DO THIS!
```

### 23.13.2. Certificate Validation

```pascal
// ✓ Always validate certificates
if Chain.IsValid(Cert) <> ecvValidSigned then
  raise Exception.Create('Invalid certificate');

// ✓ Check expiration
if Cert.IsExpired then
  raise Exception.Create('Certificate expired');

// ❌ Don't skip validation
// ProcessData(Cert);  // Without validation - DANGEROUS
```

### 23.13.3. Secure Random Generation

mORMot uses `TAesPrng` for cryptographically secure random numbers:

```pascal
uses
  mormot.crypt.core;

var
  Random: TAesPrng;
  Entropy: THash256;
begin
  Random := TAesPrng.Main;  // Thread-safe singleton
  Random.FillRandom(Entropy);  // Cryptographically secure
end;
```

---

## 23.14. Summary

### 23.14.1. Quick Reference

| Need | Solution |
|------|----------|
| Generate keys | `TEccCertificateSecret.CreateNew()` |
| Sign data | `Secret.Sign()` |
| Verify signature | `PublicCert.Verify()` |
| Encrypt data | `PublicCert.Encrypt()` |
| Decrypt data | `Secret.Decrypt()` |
| Certificate chain | `TEccCertificateChain` |
| X.509 support | `mormot.crypt.x509` |
| Secure protocol | `TProtocolEcc` |

### 23.14.2. Key Units

| Unit | Purpose |
|------|---------|
| `mormot.crypt.ecc256r1` | Low-level ECC (secp256r1) |
| `mormot.crypt.ecc` | High-level ECC certificates |
| `mormot.crypt.x509` | X.509 certificate support |
| `mormot.crypt.secure` | Factory functions |
| `mormot.crypt.openssl` | OpenSSL acceleration |

---

*Next: Chapter 24 covers Domain-Driven Design patterns with mORMot.*

---

## Navigation

| Previous | Index | Next |
|----------|-------|------|
| [Chapter 22: Scripting Engine](mORMot2-SAD-Chapter-22.md) | [Index](mORMot2-SAD-Index.md) | [Chapter 24: Domain-Driven Design](mORMot2-SAD-Chapter-24.md) |
