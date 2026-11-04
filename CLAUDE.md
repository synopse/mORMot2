# CLAUDE.md - mORMot 2

Guidance for every Claude Code session in this repository. Folder-specific notes live in
`src/*/CLAUDE.md` (e.g. `src/db/CLAUDE.md` for the database layer and the DuckDB TODOs).

## Code quality: mORMot intent, not only mORMot style

From Arnaud Bouchez's review of the AI-written DuckDB PR #666
(https://github.com/synopse/mORMot2/issues/423#issuecomment-6218806358, 2026-10-16):
the code was correct and well tested and *looked* like mORMot, but on closer look it was
"correct but sloppy". Upstream expects performance-minded, non-redundant code. Rules:

- **Parse once, reuse.** Never query/parse the same metadata repeatedly (e.g. a column's
  logical type). Compute it once at prepare/first-step time and cache it.
- **No duplicated code paths.** Factor shared logic into one function instead of
  near-identical copies per call site or per type.
- **Reuse existing framework code** instead of re-implementing it. Before writing any
  converter, search the core units: date/time via `mormot.core.datetime.pas`
  (`TSynSystemTime`, ISO-8601 helpers), GUID/UUID via the existing `TGuid` functions,
  number/text via `mormot.core.base` / `mormot.core.text` / `mormot.core.unicode`.
- **Avoid transient `RawUtf8`** (and other temporary managed strings) in hot paths such as
  per-row / per-column loops: write directly to a `TJsonWriter`/`TTextWriter`, use a stack
  buffer (`ShortString`, `TTemp*`), or pass `PUtf8Char` + length. Favor explicit
  `PUtf8Char` when talking to C APIs.
- **Specialized fast conversions** for hot types (e.g. 128-bit HugeInt to text with an
  internal scale parameter) instead of generic, slow arithmetic.
- **No `packed record`** for C API struct mirrors unless the C side is packed: it is much
  slower on arm32/aarch64 and can break alignment.
- **Benchmark against the naive path** before keeping an "optimization" or a convenience
  feature. Example: DuckDB `BindArray` still executed one statement per row and was slower
  than plain inserts - pure bloat. Prefer the mechanisms the ORM already uses
  (e.g. `TSqlDBConnectionProperties.OnBatchInsert`).
- **Keep the footprint small.** Do not add API surface, options or helper classes that the
  framework does not need; less code that reuses the core beats more "complete" code.
- **Tests:** no exception should be logged during a normal test run; skip tests cleanly when
  an optional library is missing (and on unsupported bitness).

## mORMot defaults (details: `docs/mORMot2-SAD-Recommended-Patterns.md`, Part A.1/A.2)

- Use the framework types on every path it serializes: `RawUtf8` (not `string`),
  `RawByteString` (not `TBytes`), `TDynArray`/`TDynArrayHashed` over `array of T`,
  `TRawUtf8List` (not `TStringList`), `TDocVariant`/`IDocDict` for schema-less JSON.
  `string` only at the UI/RTL boundary.
- Build text/JSON/SQL with `TTextWriter`/`TJsonWriter` or `FormatUtf8`, never by repeated
  concatenation or `Format`.
- Respect the unit layering in `src/FOLDER-DEPENDENCIES.md` (core -> lib/crypt -> net -> db ->
  orm/rest/soa -> app/ui/...). Conditional defines come only from `src/mormot.defines.inc`.
- Diagnostics via `TSynLog`, never `WriteLn`. No swallowed exceptions / bare `except`.
- Match the neighbouring code: `///` documentation comments on public API, existing
  naming, existing helper functions. Read recent upstream commits on the same unit to see
  how Arnaud refactors - that is the target style.

## Verification before a PR

- Compile with Delphi 7 (Win32), a recent Delphi (Win32 + Win64) and FPC when possible.
- Run `test/mormot2tests` (`/tests` lists classes, `/test <class>` restricts). New code
  needs a `TSynTestCase` that fails without it.
- Sources (`.pas`, `.inc`, `.dpr`) are ANSI / Windows-1252: keep new comments and strings
  pure ASCII and check `git diff` shows no line outside the intended change.

## References

- Architecture docs: `docs/mORMot2-SAD-Index.md` (chapter per topic), folder notes in
  `src/*/CLAUDE.md`.
- mormot2-superpowers Claude Code plugin (https://github.com/flydev-fr/mormot2-superpowers):
  skills `mormot2-core`, `mormot2-db`, `mormot2-orm`, `mormot2-net`, ... and its
  `code-reviewer` agent checklist.
