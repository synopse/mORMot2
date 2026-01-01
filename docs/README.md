# mORMot 2 Software Architecture Design Documentation

This folder contains the complete **Software Architecture Design (SAD)** documentation for the **Synopse mORMot 2 Framework**.

## Overview

The SAD is a comprehensive guide covering all aspects of mORMot 2, from architecture principles to practical implementation patterns. It has been adapted from the original mORMot 1.18 SAD by Arnaud Bouchez, completely updated for mORMot 2's new APIs, unit organization, and features.

## Quick Start

**Start here:**
1. [**Foreword**](mORMot2-SAD-Foreword.md) - Document overview and resources
2. [**Index**](mORMot2-SAD-Index.md) - Complete table of contents with topic navigation
3. [**Chapter 1: Overview**](mORMot2-SAD-Chapter-01.md) - Framework introduction

## Document Structure

### Core Chapters (1-26)

| Part | Chapters | Topics |
|------|----------|--------|
| **I. Introduction** | 1-4 | Overview, Architecture, Unit Structure, Core Units |
| **II. ORM** | 5-6 | TOrm, TOrmModel, Daily Patterns |
| **III. Database** | 7-9 | SQLite3, External SQL, MongoDB |
| **IV. REST** | 10-13 | JSON/REST, Client-Server Architecture, ORM Operations |
| **V. Services/SOA** | 14-17 | Method Services, Interfaces, SOA, Cross-Platform |
| **VI. Web** | 18-20 | MVC Pattern, Web Applications, Hosting |
| **VII. Advanced** | 21-25 | Security, Scripting, ECC, DDD, Testing |
| **VIII. Reference** | 26 | Installation and Compilation |

### Files

| File Pattern | Description |
|--------------|-------------|
| `mORMot2-SAD-Foreword.md/html` | Introduction and resources |
| `mORMot2-SAD-Index.md/html` | Complete index with navigation |
| `mORMot2-SAD-Chapter-NN.md/html` | Individual chapters (01-26) |
| `mORMot2-SAD-Chapters-27-30-Obsolete.md/html` | Status of deprecated mORMot 1 chapters |

### Formats

- **Markdown** (`.md`) - Source format, editable
- **HTML** (`.html`) - Rendered format for viewing

## Working Files

The `_working/` subfolder contains:
- Source extraction scripts
- mORMot 1 chapter extracts
- Migration mapping documentation
- Code validation projects

## Key Topics by Chapter

### Getting Started
- **Installation**: Chapter 26
- **Unit organization**: Chapter 3
- **Core utilities**: Chapter 4

### ORM/Database
- **TOrm basics**: Chapter 5
- **SQLite3**: Chapter 7
- **External databases**: Chapter 8
- **MongoDB**: Chapter 9

### Client-Server
- **REST/JSON fundamentals**: Chapter 10
- **Architecture**: Chapter 11
- **ORM operations**: Chapter 12

### Services/SOA
- **Method-based services**: Chapter 14
- **Interface-based services**: Chapter 16
- **Cross-platform clients**: Chapter 17

### Security & Advanced
- **Authentication/Authorization**: Chapter 21
- **ECC encryption**: Chapter 23
- **Domain-Driven Design**: Chapter 24
- **Testing/Logging**: Chapter 25

## Migration from mORMot 1

Key changes are documented throughout, with dedicated migration sections in:
- Chapter 3 (Units)
- Chapter 4 (Core)
- Chapter 5 (ORM)
- Chapter 7 (Database)
- Chapter 8 (External DB)
- Chapter 11 (REST)
- Chapter 26 (Installation)

### Quick Reference

| mORMot 1 | mORMot 2 |
|----------|----------|
| `TSQLRecord` | `TOrm` |
| `TSQLRest` | `TRest` |
| `TSQLModel` | `TOrmModel` |
| `SynCommons.pas` | `mormot.core.*.pas` |
| `mORMot.pas` | `mormot.orm.*` + `mormot.rest.*` |

## Building HTML

HTML files are generated from Markdown using the conversion script in `_working/md_to_html.py`:

```bash
cd _working
python3 md_to_html.py
```

## Contributors

**Documentation Conversion & CLAUDE.md files**
  - Javier Tarí Agulló - Conversion of SAD documentation to mORMot2 and AI-oriented CLAUDE.md files (2025)

## License

**Synopse mORMot 2 Framework Documentation**
Copyright (C) 2008-2026 Arnaud Bouchez
Synopse Informatique - https://synopse.info

Released under GPL 3.0 License.

## External Resources

- **GitHub**: https://github.com/synopse/mORMot2
- **Forum**: https://synopse.info/forum/viewforum.php?id=24
- **Official Docs**: https://synopse.info/files/doc/mORMot2.html
- **Blog**: https://blog.synopse.info

---

*Last updated: December 2025*
