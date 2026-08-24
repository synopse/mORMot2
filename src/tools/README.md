# mORMot Development Tools

## Folder Content

This folder holds some command-line or visual tools, to be used with the *mORMot* Open Source framework, version 2.

## Tools Presentation

Each Tool will have its own dedicated sub-folder.

### ecc

[The `ecc` command-line tool](./ecc) manages certificate-based public-key cryptography using ECC-secp256r1
- Public/private key pairs generation using `new`/`rekey`/`source`/`infopriv`;
- Safe key chaining using `chain`/`chainall`;
- ECDSA digital signature using `sign`/`verify`;
- ECIES encryption using `crypt`/`decrypt`/`infocrypt`;
- Symmetric encryption via `aeadcrypt`/`aeaddecrypt`;
- Centralized passwords management via `cheatinit`/`cheat`.

### Angelize (agl)

[The *Angelize* (`agl`) tool](./agl) is able to run one or several executables as daemon/services
- Implemented as a main standard OS service (Windows) or daemon (Linux);
- Launches and stops sub-processes defined in JSON setting files;
- A WatchDog can check the availibility of a sub-service on regular basis;
- Can redirect the console output, restart on problem, notify issues;
- Command line switches are available for status listing or main actions.

### mab

[The `mab` command-line Tool](./mab) can generate `.mab` files from existing `.map` or `.dbg` files
- With Delphi, enable `.map` file by setting "Detailed" debug information in Project Options;
- With FPC, will use DWARF debugging information instead of the `.map` file; a good idea is to generate `-Xg` external `.dbg` info to keep the executable small;
- If some `.map`/`.dbg` file name is specified (you can use wild chars), it will process all those files, then create the corresponding `.mab` files;
- If some `.exe`/`.dll` file name is specified (you can use wild chars), will process all matching `.exe`/`.dll` files with associated debug info, and will create the `.mab` files, then embedd the `.mab` content to the `.exe`/`.dll`;

```
Usage: mab  <source> [options]

   <source>           exe or .dbg source filename or mask

Options:
  -s, --nosymbol      include only line info for production (smaller and safer)
  -m, --nomab         only embed to exe, no external .mab file
```

### mORMot GET (mget)

[The *mORMot GET* (`mget`) command-line tool](./mget) can retrieve files using HTTP or HTTPS, similar to the well-known homonymous GNU WGet tool, but with some unique features provided by *mORMot 2*:
- Can resume aborted downloads, using `RANGE` headers;
- Can compute and verify the hash of the downloaded content;
- Can brodcast and download from a local network peer cache.
Those last two features are quite unique and efficient.

### mopenapi

[The *mORMot OpenAPI* (`mopenapi`) command-Line tool](./mopenapi) can generate Delphi/FPC client .pas units from OpenAPI/Swagger .json specifications.

Example of usage:
```
   ./mopenapi --help
   mopenapi swagger.json PetStore
   mopenapi OpenApiAuth.json /concise
   ./mopenapi test.json --options=DtoNoExample,DtoNoPattern
```
See [our corresponding Blog entry](https://blog.synopse.info/?post/2024/09/06/Swagger/OpenAPI-Client-Generator-for-Delphi-and-FPC).
