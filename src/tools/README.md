# mORMot Development Tools

## Folder Content

This folder holds some command-line or visual tools, to be used with the *mORMot* Open Source framework, version 2.

## Tools Presentation

Each Tool will have its own dedicated sub-folder.

### ecc

The `ecc` command-line tool manages certificate-based public-key cryptography using ECC-secp256r1
- Public/private key pairs generation using `new`/`rekey`/`source`/`infopriv`
- Safe key chaining using `chain`/`chainall`
- ECDSA digital signature using `sign`/`verify`
- ECIES encryption using `crypt`/`decrypt`/`infocrypt`
- Symmetric encryption via `aeadcrypt`/`aeaddecrypt`
- Centralized passwords management via `cheatinit`/`cheat`

### agl

Angelize (`agl`) tool is able to run one or several executables as daemon/services
- Implemented as a main standard OS service or daemon
- Launches and stops sub-processes defined in JSON setting files
- A WatchDog can check the availibility of a service on regular basis
- Can redirect the console output, restart on problem, notify issues
- Command line switches are available for status listing or main actions

### mab

Command-Line Tool to Generate `.mab` files from existing `.map` or `.dbg` files
- if some `.map` file name is specified (you can use wild chars), it will
process all those `.map` files, then create the corresponding `.mab` files
- if some `.exe`/`.dll` file name is specified (you can use wild chars), will
process all matching `.exe`/`.dll` files with an associated `.map` file, and will
create the `.mab` files, then embedd the `.mab` content to the `.exe`/`.dll`
- if no file name is specified, will process `*.map` into `*.mab` from the
current directory
- with FPC, will use DWARF debugging information instead of the `.map` file

