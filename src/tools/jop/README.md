# JOP: Generating Better JSON Configuration

> In the land of IT there lived a tool whose name was `jop`.
> This tool was blameless and upright; it feared complexity and shunned evil — and behold, it brought forth order from the chaos of JSON, generating better configurations for all who called upon it.
> (Book of `jop 1:1`)

Configuration files start simple.

A small JSON file with a few settings is easy to read, easy to edit, and universally supported. But as applications grow, configuration tends to follow the same pattern: duplication starts appearing everywhere, similar objects are repeated with small variations, and environment-specific settings start spreading across multiple files.

At that point, plain JSON starts to show its limits.

The job of **JOP ("JSON Open PreProcessor")** is not to define another JSON-like format, but to **generate better JSON**.

This `README.md` file documents the `jop` tool in its initial state of 20 November 2026.

## Command line

```sh
jop convert input.json output.yaml
jop convert catalog.json catalog.xml
jop convert catalog.xml catalog.json --compact
jop convert input.json output.json --compact --no-expand
jop convert input.json tables.md --to md
jop convert input.json tables-pipe.md --to md-pipe
jop batch "input/*.json" output --to yaml --recursive
jop generate sample.json --depth 4 --items 100 --seed 42
jop compare first.json second.json5
```

Put filenames before switches, following `TExecutableCommandLine` parsing.
Use `jop --help` or `jop <command> --help` for the corresponding parameters.

The two-filename form is an alias for `convert` so the following are all valid:
```sh
jop input.json output.yaml
jop input.hjson output.json
jop input.json output.json --compact --no-expand
```

| Command | Behavior |
| --- | --- |
| `convert source destination` | Infer formats from extensions unless `--from` or `--to` is supplied. Replace the destination after successful processing. |
| `batch mask output-folder` | Preserve relative paths, preflight collisions, and retain individual results. Quote wildcard masks for JOP to expand them. |
| `generate destination.json` | Write random compact JSON with `--depth`, `--items`, and an optional reproducible `--seed`. |
| `compare left right` | Compare native compact `JsonReformat()` outputs, without JSON validation or macro expansion. |

Exit codes are **0** for success or equal documents, **1** for different
documents, and **2** for usage or processing errors.

Conversion and batch options include `--from`, `--to`, `--include-root`, repeated
`--define name=value` (or `-D name=value`), `--delimiter` (one character or `tab`),
`--table-root`, and `--stats`.

The processing flags have matching switches: `--validate-utf8`,
`--validate-json`, `--no-expand`, `--no-comments`, `--no-namespaces`,
`--preprocess-input`, `--debug-comments`, `--absolute-paths`,
`--allow-missing-includes`, `--compact`, `--recursive` and `--dry-run`.
Native table options use `--first-row-fields`, `--strict-fields`,
`--strict-types`, `--mixed-as-json`, `--null-as-text` and `--keep-array-index`.

### Random JSON

`--depth` is the maximum number of nested containers, from 0 to 64:

- **0:** one scalar value; the item count has no effect.
- **1:** a flat root array containing `--items` scalar values.
- **2 and above:** a root array whose values contain nested arrays or objects.

Each nested container has one nested branch and up to two scalar siblings.
Output grows proportionally to `items * depth`. Values include strings,
integers, decimal numbers, booleans and null. Zero items writes an empty root
array when depth is positive.

Defaults are depth 3 and 10 items. `--seed` accepts an unsigned 32-bit value;
without it, a random seed is used. A local `TLecuyer` gives
repeatable output per seed without changing the process random state. Transient strings
use `TShort31`, and values go straight to `TJsonWriter`. Generation writes to
a temporary sibling and finishes with native `ReplaceFile`.

```pascal
function JopGenerate(W: TJsonWriter; Depth, Items: integer;
  Seed: cardinal): TJopError;
```

This entry point neither owns nor flushes the caller's writer. Discard partial
output if it returns an error.

### JSON data comparison

```pascal
function JopCompareFiles(const Left, Right: TFileName; out Equal: boolean;
  Info: PRawUtf8 = nil): TJopError;
```

Each file is loaded with `RawUtf8FromFile` and normalized with
`JsonReformat(source, jsonCompact)`. The first raw input buffer is released
before the other file is loaded; only the two normalized documents remain
together. There is no variant tree, member sorting, UTF-8 validation,
structural check or additional semantic equality pass.

Equality means **equality of native compact output**. Comments and layout
are ignored as the formatter removes them. Object property order remains
significant, as do number/string spelling differences retained by the native
formatter. Changed values compare different.

Comparison ignores extensions and applies exactly the syntax understood by
`JsonReformat`; it does not infer YAML/CSV or expand JOP directives. Document
inputs are assumed to be usable by the native formatter; malformed content is
not independently diagnosed. Missing files return `jeNoSource`, and empty,
BOM-only or unreadable content returns `jeRead`. `Equal` is false on I/O failure.


## Calling the unit

Add `mormot.jop` from `src/tools/jop` to the application uses clause.

Processing functions return `TJopError`; `jeSuccess` means that output is
complete. A required `out Error: RawUtf8` and Boolean result are no longer part
of the API. `JOP_ERROR[error]` is the static `RawUtf8` description.
`JOP_FORMAT_NAMES` also contains `RawUtf8` values, while
`JOP_FORMAT_EXTENSIONS` contains `TFileName` values.

```pascal
var
  options: TJopOptions;
  error: TJopError;
  source, output: RawUtf8;
begin
  source := RawUtf8FromFile('input.hjson');
  options := JopOptions(jfHjson, jfJson, [jofCompact]);
  error := JopProcess(source, options, output);
  // output is complete only when error = jeSuccess.
  // Otherwise JOP_ERROR[error] describes the failure.
end;
```

The optional final `Info: PRawUtf8` parameter receives contextual information,
such as a YAML/XML parser location, an include failure, a file operation exception,
or native table remarks. It is cleared on entry. A routine enum error does not
need an allocated description. Remarks accompany `jeSuccess`; they do not turn
completed output into a failure.

```pascal
error := JopProcess(source, options, output, nil, @info);
```

There is also a string-returning convenience overload, matching the formatters'
usual calling style. This overload raises `EJopException` on error:

```pascal
source := RawUtf8FromFile('input.json');
output := JopProcess(source, jfJson, jfYaml);
```

### Mutable input buffers

All in-memory entry points now take **`var Source: RawUtf8`**. Source must be a
private writable buffer and is consumed by the call: it may be replaced during
conversion, then modified by native in-place JSON decoding. Do not reuse it as
the original input afterward. File APIs retain `const TFileName` arguments and
load their own private buffer.

`var` does not detach a reference-counted string. Assigning a literal or shared
string to a variable does not make its storage private. JOP deliberately makes
no hidden `UniqueString` call or full-buffer copy. For a shared value or test
fixture, create the writable input once:

```pascal
const
  JSON: RawUtf8 = '{"answer":42}';
var
  source, output: RawUtf8;
begin
  FastSetString(source, pointer(JSON), length(JSON));
  output := JopProcess(source, jfJson, jfYaml);
end;
```

Both `JopProcess` overloads have this contract, as do writer, stream and CSV
collection calls. Source and destination must be distinct variables.

### Entry points

| Function | Destination and ownership |
| --- | --- |
| `JopProcess(Source, Options, Dest, ...)` | Returns text in `Dest`; clears it on failure. Source and destination variables must be distinct. |
| `JopProcessToWriter(Source, Writer, Options, ...)` | Appends to a caller-owned `TJsonWriter`; does not flush or free it. |
| `JopProcessToStream(Source, Stream, Options, ...)` | Appends at the current stream position and flushes completed output; does not close, seek or truncate the stream. |
| `JopProcessToCsv(Source, Options, Tables, ...)` | Returns all named CSV tables in native `TTextPairs`; clears the array on failure. |
| `JopProcessFile(Source, Dest, Options, ...)` | Converts through a temporary sibling and replaces `Dest` after success. |
| `JopProcessFiles(Mask, Folder, Options, Files, ...)` | Plans deterministic batch conversion and returns per-file status, enum error and optional detail. |

The optional arguments after the required parameters are
`Statistics: PJopStatistics = nil` and `Info: PRawUtf8 = nil`.
`JopProcessFiles` takes its `out Files` argument before these two parameters.
No application, command-line parser, logging configuration, or public processing
class is required.

`TJsonWriter` inherits from `TTextWriter`. Using that concrete capability in the
writer signature removes the old runtime class check and temporary adapter for
plain `TTextWriter` instances. It can still receive text, XML, CSV and Markdown bytes.

## Formats and encoding

| Format | Input | Output |
| --- | --- | --- |
| `jfJson` | Native relaxed JSON parser | Standard compact or indented JSON layout |
| `jfJsonC` | JSON with comments | Comment-preserving JSON |
| `jfJson5` | Native extended JSON syntax | JSON5 layout |
| `jfHjson` | Also permits an implicit root object | Hjson layout |
| `jfMorml` | Also permits an implicit root object | mORMl layout |
| `jfYaml` | Native YAML 1.2 core-schema subset | Native YAML serializer |
| `jfXml` | Native XML-to-variant mapping, string values | Native variant-to-XML mapping with UTF-8 declaration |
| `jfCsv` | Headered CSV, all cells retained as strings | Native relational CSV, one table for a single destination |
| `jfText` | Text preprocessor | Text |
| `jfMd` | Unsupported | Markdown sections with fenced CSV (`ttfMarkdownCsv`) |
| `jfMdPipe` | Unsupported | Markdown pipe tables (`ttfMarkdownPipe`) |

`jfAuto` uses filename extensions for file calls. For in-memory calls it chooses
JSON input; automatic output is text for text input and JSON otherwise.
Aliases include `yml`, `txt`, `jop`, `markdown`, `md-csv`, `markdown-pipe` and `md-pipe`.
`.ini`, `.cnf` and `.jop` are treated as text. `.xml` selects `jfXml`.
`.md` and `.markdown` select
**`jfMd`**; choose `jfMdPipe` or `--to md-pipe` explicitly for pipe tables. Both
Markdown modes use `.md` as their output extension and are output only.

In-memory input is **opaque 8-bit text by default**, with an optional leading
UTF-8 BOM. File input uses `RawUtf8FromFile`: a UTF-8 BOM is stripped, and a
UTF-16LE/BE BOM selects native decoding to UTF-8. BOMless bytes remain unchanged;
there is no ANSI code-page detection or conversion.

`jofValidateUtf8` explicitly checks the loaded processing content, including
active definitions and included content. It is not an additional validation
policy for exporter labels. Physical embedded NUL is rejected at the content
boundary because native parsers are zero-terminated; this is separate from
JSON's escaped `\u0000` representation and from an OS filename.

## Processing and validation

The normal JSON-family-to-JSON-family path calls the shared native preprocessor
directly into the destination writer. It does not create an intermediate JSON
document, build a variant tree, or run an extra validation pass.

`jofValidateJson` explicitly checks the expanded compact JSON using
`IsValidJson(..., true)`. This is mORMot's structural check, not an RFC 8259
validator. Native handling of duplicate names, string escapes and extended
input remains applicable. When validation and comment-preserving output are
both requested, a temporary compact copy is checked while the comments remain
available for final formatting.

The old private JSON grammar/state machine, generic escaped-NUL scanner and
generic-writer adapter have been removed. Native syntax failures still report
an error, and the shared formatter checks that containers were closed.

For XML, YAML and table export, ordinary JSON is decoded directly in the supplied
buffer through `TDocVariantData(doc).InitJsonInPlace()` on a local
`doc: variant`. Scalar roots retain native `JsonToAnyVariant` support.
Native quoted-string skipping keeps URLs, apostrophes and literal dollar signs
on the direct path. Dialect conversion, actual comments, DSL expansion and
explicit validation may first replace `Source` with normalized JSON.

YAML input uses the native parser's returned JSON buffer; CSV input writes
compact JSON through the core converter. Conversion replaces `Source`, and
later document decoding uses that same buffer. No additional copy is made for
variant loading. Default parsing remains relaxed: there is no extra
whole-input/end-pointer check around native document decoding.

JSON-to-JSON processing preserves escaped `\u0000`. YAML and table export use
native variant decoding, which maps that escape to `?`. Empty property names,
duplicate-name lookup and numeric representation likewise follow native
variant semantics. JOP does not add a second content policy around those
formatters.

### Preprocessor options

`Defines` contains literal `name=value` pairs. Later entries win, and source
definitions may override them. Names are case-sensitive identifiers. JSON
constants and numbers retain their type; other values remain plain strings.
Definition values are registered through the core preprocessor, never
concatenated as executable DSL source.

`jofNoExpand` bypasses definitions, namespace lookup, includes and substitutions.
The shared writer API formats JSON without constructing a preprocessor in that
case. `jofNoNamespaces` disables external namespace fallback while retaining
local definitions. `jofNoComments` removes comments while keeping the selected
JSON output layout.

YAML, CSV and XML source text is expanded only when `jofPreprocessInput` is enabled.
Converted data is never expanded a second time. `jofDebugComments` requires
preprocessing and a compatible JSONC, JSON5, Hjson or text output; incompatible
flag combinations return `jeInvalidOptions`.

Includes require an explicit `IncludeRoot`. `TPreproc.DoInclude()` has one
read path: **`FileExists()` followed by `RawUtf8FromFile()`**. Empty, BOM-only or
unreadable content is an include error for JOP, even with allow-missing enabled.
`jofAllowMissingIncludes` follows `FileExists` semantics and skips files it does
not find, without trying to distinguish every OS error. Native BOM handling
applies to includes too.

Root/path rules, nesting limits, callbacks, content NUL handling and optional
UTF-8 checks remain. `jofAbsolutePaths` enables absolute include names. Include
and insert remain distinct: insert does not expand its content. Include byte
counters measure successfully loaded decoded bytes, including repeated loads.

## XML conversion

XML uses the existing `mormot.core.fmt` code and `TDocVariant` storage.
`XmlToVariant()` builds the tree once; XML, YAML, CSV and Markdown output
reuse that tree directly. JSON output serializes the tree once before its
selected layout is applied. XML output appends `XMLUTF8_HEADER` and calls
`AddVariantToXml()` on the destination writer with `JXO_ENABLED`, avoiding
an intermediate XML string. `--compact` only changes JSON output.

The native mapping is:

| Document value | XML representation |
| --- | --- |
| Object property | Element with that name |
| Array-valued property | Repeated elements with that name |
| Scalar `@name` property | Attribute of the enclosing element |
| Scalar `#text` property | Text content of the enclosing element |

Attributes may appear after child properties in JSON; the variant writer
emits them in the start tag. XML input retains values as strings, including
`007`, `true` and `null`. Names are case-sensitive. Namespace prefixes and
`xmlns` attributes are retained, without namespace URI resolution;
`--no-namespaces` only controls preprocessor namespaces.

For example, save this as `catalog.json`:

```json
{
  "catalog": {
    "@version": "1",
    "book": [
      { "@id": "007", "title": "A & B" },
      { "@id": "008", "title": "Second" }
    ]
  }
}
```

```sh
jop convert catalog.json catalog.xml
jop convert catalog.xml catalog.yaml
jop convert catalog.xml restored.json --compact
jop batch "input/*.xml" output --to json --recursive
```

The first command emits the XML declaration followed by:

```xml
<catalog version="1"><book id="007"><title>A &amp; B</title></book><book id="008"><title>Second</title></book></catalog>
```

The same conversion is available from the reusable unit:

```pascal
source := RawUtf8FromFile('catalog.json');
output := JopProcess(source, jfJson, jfXml);
```

**Supply an object with one root element and XML-valid names when a complete
XML document is required.** The native exporter also supports fragments:
multiple root properties produce sibling elements. It does not invent a root
wrapper, validate names or encode arbitrary JSON keys into XML names. A bare
scalar emits no element; a root array uses numeric element names, which are
not valid XML names. Put an array under a named property inside a root object,
as in the example. Empty arrays emit no elements; null and empty string values
both produce empty elements. Attribute values must be scalar: structured
`@name` values are omitted by the native writer. Keep `#text` scalar as well.

This is a data conversion, not an XML document editor. CDATA is stored as text;
comments and processing instructions are discarded. Mixed text is gathered
into `#text`, and repeated siblings are grouped by name, so their original
interleaving is not preserved. Native escaping and character handling apply.
The input parser decodes predefined entities and numeric references without
DTD processing or schema validation. It is not a full XML conformance
validator. Parser failures return `jeInvalidXml`, with the native line/context
diagnostic in `Info`.

XML preprocessing is opt-in through `--preprocess-input`, as for YAML and CSV.
Expansion runs on the source text before XML parsing, so replacement values
must already be suitable XML text or markup. `--no-expand` takes precedence.
Parsed strings are never expanded a second time.

## CSV and Markdown exporters

JOP calls the trunk's `VariantToCsv` and `VariantToMd` functions. Its own code
does not reproduce their schema discovery, flattening, quoting or Markdown
escaping. The JSON convenience wrappers in the trunk always validate UTF-8;
using the variant entry points after JOP's own decoding preserves the explicit
UTF-8 option.

Table options use the existing `TTableOptions` directly:

| Option | Native effect |
| --- | --- |
| `TABLE_DEFAULT` / `[]` | Union of fields in first-appearance order; missing values are empty cells; nonuniform schemas may produce remarks. |
| `tboFirstRowFields` | Use fields found in the first object at each path. |
| `tboStrictFields` | Reject missing or additional fields. |
| `tboStrictTypes` | Reject incompatible non-null scalar types. |
| `tboMixedAsJson` | Permit mixed arrays by serializing container cells as compact JSON. |
| `tboNullAsText` | Render explicit null as `null`. |
| `tboKeepArrayIndex` | Add the zero-based `__index` column. |
| `TABLE_STRICT` | Combination of strict fields and strict types. |

`TableRoot` defaults to `root`. Every exported table has `__rowid`; child
tables also have `__parentid`. Nested objects become dotted columns, and nested
arrays become linked child tables. CSV uses comma separators and LF record
endings. Both Markdown modes include all tables: `jfMd` emits `### name.csv`
sections with fenced CSV, and `jfMdPipe` emits escaped pipe tables. The current
native exporter handles fence selection and embedded line breaks. Its public
declarations and implementation remain unchanged from trunk.

Table labels use the native exporter's escaping rules, including control
characters; JOP does not add a separate label-validation layer.

### Preserve every CSV table

```pascal
var
  options: TJopOptions;
  tables: TTextPairs;
  source: RawUtf8;
  error: TJopError;
begin
  source := RawUtf8FromFile('orders.json');
  options := JopOptions(jfJson, jfCsv);
  options.TableRoot := 'orders';
  error := JopProcessToCsv(source, options, tables);
end;
```

For rows with an `items` array, this returns `orders` and `orders.items`,
including the generated parent/child IDs. The collection API deliberately ignores `Options.OutputFormat`.

A single-string, writer, stream or file call with `jfCsv` succeeds only when
the native exporter produces one table. Multiple tables return
`jeMultipleTables`. This keeps the entire relational result available through
`JopProcessToCsv`, without discarding children or inventing a file layout.

The former flat-object-only CSV exporter has been removed. Relational CSV
export is not a JSON-to-CSV-to-JSON round trip: generated IDs are real exported
columns, and CSV import retains every data cell as a string.

### Reusable CSV import

The new core `CsvToJson` overloads return `TCsvError`. The writer overload
accepts a read-only `PUtf8Char` plus `TJsonWriter`; the string overload takes
`const Csv: RawUtf8; out Json: RawUtf8`. Both use an optional input separator.

The importer supports doubled quotes and embedded CR/LF in quoted cells,
rejects duplicate or empty headers and unequal row widths, and preserves spaces,
empty strings and leading zeroes. Only header names persist as strings;
transient cells reuse a `TSynTempAdder`. `CsvDelimiter` configures this importer
only and does not alter the native exporter's comma/LF output.

## Files, batches and statistics

Single-file conversion loads the source with `RawUtf8FromFile`, writes a
temporary sibling, closes it, and calls `mormot.core.os.ReplaceFile` after
successful conversion. File input is still fully buffered. There is no private
input stream reader or JOP-specific Windows/POSIX replacement implementation.

The native reader returns an empty string for empty/BOM-only input and for a
read failure. `JopProcessFile` reports `jeRead` for that result and leaves an
existing destination intact. A missing source is reported as `jeNoSource`.
Empty in-memory text is still supported; empty includes are now errors.

Existing output survives conversion failures, including `jeMultipleTables`
and `jeInvalidXml`.
Source/destination comparison uses normalized lexical paths. The core helper
owns platform-specific replacement semantics; supported platforms still
require their own build verification.

JOP delegates file-size bounds to the reader, file-versus-directory detection
to `FileExists`, and directory filtering to `FileNames`. It does not scan OS
filenames for embedded NUL. Empty required paths, same-file protection,
option compatibility and planned output collisions remain checked.

Batches enumerate deterministically, preserve relative subdirectories, check
all planned names for collisions before writing, and exclude an output subtree
from recursive input enumeration. Directory input skips unknown and output-only
formats; an explicit file mask reports them as per-file errors. Processing
continues after individual file failures, with the first error returned and
every result retained in `TJopFileResults`.

`jofDryRun` checks file paths, format choices and options without opening input
contents or creating output directories. It records skipped files.

`TJopStatistics` accumulates document outcomes, processed main input bytes,
include/insert counts and bytes, successful output bytes, and elapsed
microseconds. `InputBytes` is captured before mutable conversion changes Source and
measures the loaded content after BOM removal or UTF-16 decoding, not the
physical file size. No extra file-size
query is made for this counter. The full CSV collection counts the sum of all
CSV values. `JopThroughput` computes decimal MB/s from these processing bytes. Failed caller-owned writer/stream operations may have emitted
partial bytes; their output count is zero and the caller must discard the
unfinished output.

## Building, applying and testing

Apply `jop-processing.patch` to the pinned trunk commit. The patch contains the
new main unit, `jop.dpr`, `jop.lpi`, README and `TTestJop`, shared preprocessor
writer/settings hooks, reusable CSV import and the focused parser corrections
needed by the regression suite. The trunk's relational exporters are unchanged.
This full patch is for a clean baseline; do not apply it over an older JOP patch.

- **Lazarus:** open `src/tools/jop/jop.lpi` with the framework's `mormot2`
  package available.
- **Delphi:** open `src/tools/jop/jop.dpr` in a configured mORMot environment.
  It follows the framework includes, console setup and Windows manifest
  conventions, with no missing project-specific resource dependency.
- **FPC:** build the same DPR using the framework unit/include paths and usual
  platform support libraries. The bundle includes the exact Linux build script.

Delphi and Lazarus IDE execution are prepared but were not verified here; see
the validation record for the compiler and platform actually used.

Add `src/tools/jop` and `test` to the unit search path, then register
`TTestJop` with `AddCase(TTestJop)`. The bundle includes a small `.pas` regression
runner, CLI build/check scripts, source digests and validation logs. See `VALIDATION.md` for exact commands, results
and the limits of verification.
