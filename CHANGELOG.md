# Changelog

All notable changes to the *mORMot Open Source Framework* project will be documented in this file.
Details are available [on out GitHub repository](https://github.com/synopse/mORMot2/commits/master)

## [2.1.stable] - 2023-08-24
This is the last stable release of *mORMot* v2.
### Added
- (C)LDAP, DNS, (S)NTP clients
- Command Line Parser
- Native digest/basic HTTP servers authentication
- Angelize services/daemons managers
- TTunnelLocal TCP port forwarding
- SHA-1/SHA-256 HW opcodes asm
- 7Zip dll wrapper
- OpenSSL CSR support
- PostgreSQL async DB with HTTP async backend (for TFB)
- LUTI continous integration cross-platform farm
### Changed
- Upgraded SQLite3 to 3.42.0
- Stabilized Mac x86_64/aarch64 platforms
- Lots of bug fixes and enhancements

## [2.0.stable] - 2023-03-06
This was the initial stable release of *mORMot* v2.
In respect to previous 1.18 revision - from https://github/synopse/mormot:
### Added
- Asynchronous High-Performance HTTP/WebSockets server.
- Native TLS/HTTPS client ans server, potentially with Let's Encrypt.
- OpenSSL 1.1/3.0, QuickJS and LibDeflate support.
- Optional modern syntax with generics and enumerators.
### Changed
- Major source code refactoring into smaller and more refined units.
- Rewrite of all the core process (e.g. RTTI and JSON), new AVX2 asm.
### Removed
- DDD and cross-platform units were not converted yet.
- Dropped Kylix, Delphi 5-6 support.

