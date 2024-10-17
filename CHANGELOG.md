# Changelog

All notable changes to the *mORMot Open Source Framework* project will be documented in this file.
Details are available [on out GitHub repository](https://github.com/synopse/mORMot2/commits/master)

## [2.3.stable] - 2024-10-16
This is the latest stable release of *mORMot* v2.
### Added
- [Swagger/OpenAPI Client Generator](https://blog.synopse.info/?post/2024/09/06/Swagger/OpenAPI-Client-Generator-for-Delphi-and-FPC)
- [IDocList/IDocDict Containers](https://blog.synopse.info/?post/2024/02/01/Easy-JSON-with-Delphi-and-FPC)
- [SID/DACL/SACL/SDDL/ACE Security Objects](https://github.com/synopse/mORMot2/blob/master/src/core/mormot.core.os.security.pas)
- async web server: IOCP support on Windows, metrics gathering and standard logging
- `TSynMustache` can work on plain data via RTTI, in addition to `TDocVariant`
- introducing `TRttiMap` for DTO process.
### Changed
- Upgraded SQLite3 to 3.46.1
- Enhancements to the LDAP client, HTTP/HTTPS client, Kerberos auth, Peer Cache, ORM.
- Lots other bug fixes, optimisations and enhancements.

## [2.2.stable] - 2024-01-02
### Added
- [OpenSSL 3.0/3.1 direct support in addition to 1.1](https://blog.synopse.info/?post/2023/09/08/End-Of-Live-OpenSSL-1.1-vs-Slow-OpenSSL-3.0)
- [Native X.509, RSA and HSM support](https://blog.synopse.info/?post/2023/12/09/Native-X.509-and-RSA-Support)
- [`mget` utility with `THttpPeerCache` peer-to-peer caching](https://blog.synopse.info/?post/2024/01/01/Happy-New-Year-2024-and-Welcome-MGET)
### Changed
- Upgraded SQLite3 to 3.44.2
- Lots of bug fixes and enhancements (especially about cryptography and networking)
- A lot of optimizations, so that we eventually reached [in top 12 ranking of all frameworks tested by TFB](https://blog.synopse.info/?post/2023/10/31/Pascal-in-the-race%3A-TFB-Challenge-Benchmarks)

## [2.1.stable] - 2023-08-24
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

