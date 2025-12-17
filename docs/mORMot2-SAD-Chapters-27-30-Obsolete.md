# Chapters 27-30: Status Note

The following chapters from the original mORMot1 Software Architecture Design document have been marked as **obsolete** for mORMot2:

## Chapter 27: mORMot Framework Source

**Status:** Obsolete / Superseded

The original chapter provided a detailed listing of mORMot1 units and their dependencies. This information has been superseded by:
- **Chapter 3**: Meet mORMot2 - Unit Structure (complete unit organization)
- **Chapter 4**: Core Units (detailed core unit documentation)
- **Chapter 26**: Source Code (repository structure and folder layout)

The mORMot2 source code is organized in a completely different manner than mORMot1, with clear folder-based separation by functionality. Creating a redundant unit listing chapter is not necessary.

## Chapter 28: SynFile Application

**Status:** Obsolete / No mORMot2 Equivalent

The SynFile demo application from mORMot1 relied heavily on deprecated components:
- `mORMotUI.pas` - UI Grid (deprecated)
- `mORMotToolBar.pas` - ORM-driven toolbar (deprecated)
- `mORMotUIEdit.pas` - Record edition dialogs (deprecated)
- `mORMoti18n.pas` - Internationalization (deprecated)
- `mORMotReport.pas` - Reporting engine (deprecated)

These RTTI-based UI generation features were removed in mORMot2. Modern mORMot2 applications typically use:
- Standard VCL/LCL forms with manual UI design
- Web-based frontends with MVC/MVVM patterns
- REST API clients with any UI framework

For learning mORMot2, refer to:
- The `ex/` folder sample applications
- Thomas Tutorials (`ex/ThirdPartyDemos/tbo/`)
- The MVC/MVVM chapters (18-19)

## Chapter 29: Main SynFile Demo Source

**Status:** Obsolete / No mORMot2 Equivalent

This chapter documented the unit dependencies of the SynFile demo. Since SynFile does not have a mORMot2 equivalent, this chapter is obsolete.

## Chapter 30: SWRS Implications

**Status:** Obsolete / Historical Reference Only

The Software Requirements Specification (SWRS) implications chapter was a formal requirements traceability matrix linking design specifications to implementation units. While the design principles still apply to mORMot2:
- All unit references are outdated (mORMot1 unit names)
- All class references are outdated (TSQLRecord → TOrm, etc.)
- The formal SWRS document structure is not used in mORMot2 development

The concepts documented in Chapter 30 are covered throughout the mORMot2 documentation:
- DI-2.1.1 (Client-Server): Chapters 10-13
- DI-2.1.1.1 (RESTful): Chapter 10
- DI-2.1.2 (UTF-8 JSON): Chapter 10
- DI-2.1.3 (ORM/RTTI): Chapters 5-6
- DI-2.1.4 (Cross-Cutting): Chapters 21, 25
- DI-2.1.5 (SOA): Chapters 15-16
- DI-2.2.1 (SQLite3): Chapter 7
- DI-2.2.2 (Unit Testing): Chapter 25
- DI-2.3.x (UI): Chapters 18-19 (web-based alternatives)

---

## Summary

| Chapter | Original Title | mORMot2 Status |
|---------|---------------|----------------|
| 27 | mORMot Framework Source | Superseded by Chapters 3, 4, 26 |
| 28 | SynFile Application | Obsolete (deprecated UI components) |
| 29 | Main SynFile Demo Source | Obsolete (SynFile not ported) |
| 30 | SWRS Implications | Obsolete (outdated references) |

For mORMot2 development, please refer to:
- The official samples in `ex/` folder
- Thomas Tutorials for step-by-step learning
- The complete Software Architecture Design chapters 1-26
