# mORMot Development Tools

## Folder Content

This folder holds some command-line or visual tools, to be used with the *mORMot* Open Source framework, version 2.

## Tools Presentation

Each Tool will have its own dedicated sub-folder.

### mormot.tools.ecc

The `ecc` command-line tool manages certificate-based public-key cryptography using ECC-secp256r1
- Public/private key pairs generation using `new`/`rekey`/`source`/`infopriv`
- Safe key chaining using `chain`/`chainall`
- ECDSA digital signature using `sign`/`verify`
- ECIES encryption using `crypt`/`decrypt`/`infocrypt`
- Symetric encryption via `aeadcrypt`/`aeaddecrypt`
- Centralized passwords management via `cheatinit`/`cheat`

