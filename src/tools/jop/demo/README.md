# EKON 30 — JOP demonstration suite for mORMot 2

Ten small, **executed and validated** terminal demonstrations for **Beyond INI & JSON: Smart Configuration Files**. Intended to be committed as `src/tools/jop/demo/` in the mORMot 2 trunk. The code and fixtures are teaching material; the DHCP-shaped example is **not a production deployment configuration**.

## Installation on Debian 13

Build `src/tools/jop/jop.dpr` with your current mORMot trunk first. From this `demo/` directory, expose the executable as `demo/jop` so that each numbered folder can call `../exe/jop`:

```sh
cd src/tools/jop/demo
ln -s ../exe/jop jop                   # once, if your binary is src/tools/jop/exe
./02-json-flavors/run.sh               # first live demo
./04-xml-attributes/run.sh             # XML attributes and array mapping
./06-dhcp-smart/run.sh                 # preprocessing and includes
./01-generate-compare/run.sh           # generation / conversion / comparison
```

If the compiled binary lives elsewhere, instead link it by an **absolute path**: `ln -s /absolute/path/to/jop jop`. Check `./jop --help` first: current JOP includes XML support, `convert`, `batch`, `generate` and `compare`.

The scripts `cd` into their own folders, `cat` the input, show the exact JOP command, `cat` the output, and assert critical results. They require a POSIX shell, the JOP binary and basic Debian utilities. **No script creates an `out/` folder.** Generated files remain beside each numbered `run.sh` (batch demo 09 also preserves one nested relative path).

## One-command rehearsal and verification

```sh
./run-all.sh          # run all ten demos; exits nonzero at the first failed check
./verify.sh           # run all ten, then 71+ semantic checks (requires Python 3 stdlib)
./clean.sh            # remove generated outputs; preserve all source fixtures and scripts
```

`./verify_outputs.py` can also be run after `run-all.sh` without rerunning the binary. Python is **optional** and used only by `verify.sh` / `verify_outputs.py` for machine-readable JSON, CSV and XML assertions. `run-all.sh` is also optional; each folder is self-contained.

| Demo | What is demonstrated | Presenter suggestion |
| --- | --- | --- |
| `01-generate-compare` | Seeded generation, pretty JSON, compact comparison | **Live**, last |
| `02-json-flavors` | Hjson → JSON / JSON5 / JSONC / MorML and normalization | **Live**, first |
| `03-sax-vs-dom` | Direct JSON-family processing vs `TDocVariantData` output path | Backup / IDE discussion |
| `04-xml-attributes` | XML `@name`, `#text`, repeated siblings, YAML, string values | **Live**, second |
| `05-preprocess` | `$$`, `$(...)`, `$"..."`, conditionals and `-D` | Backup / slide reference |
| `06-dhcp-smart` | Definition includes → ordinary JSON → YAML → JSON | **Live**, third |
| `07-input-preprocess` | YAML/XML preprocessing is opt-in before parsing | Backup |
| `08-csv-markdown` | CSV string cells; relational `__rowid` / `__parentid` | Backup |
| `09-batch` | Dry run, recursive batch, relative folder layout | Backup |
| `10-error-cases` | Exit codes 0/1/2, order-sensitive compare, safe output replacement | Backup |

**Each numbered folder contains its own `README.md`** with talking points, expected observations, relevant source units, limitations and operational tips. Read those before the session.

## Core ideas worth emphasizing

1. **Native UTF-8 engine:** `mormot.core.unicode` and `mormot.core.text` supply low-level text operations and writers. No application-wide mORMot ORM/REST setup is required.
2. **Fast SAX-style layout:** normal JSON-family → JSON-family output uses the `mormot.core.json` token parser with `TJsonWriter`, not an allocated DOM tree for every node.
3. **Structured DOM layout:** YAML/XML/CSV/Markdown paths can reuse `TDocVariantData` and native exporters. `InitJsonInPlace` may modify the caller-owned private writable `RawUtf8` buffer.
4. **Human-editable formats + controlled DSL:** `mormot.core.fmt` adds comments, relaxed JSON flavors, `$$` definitions, `$(...)`, conditionals and include/insert. The DSL has no loops, a flat namespace, and last assignment wins. It generates ordinary JSON or text.
5. **Tool and reusable API:** `mormot.tools.jop` and `jop.dpr` expose `convert`, `generate`, `compare`, `batch`, and per-file/stream/writer APIs. It is **not** a new configuration standard.

## Caveats and best practices

- `compare` is normalized **compact-text equality**, not unordered JSON object equality; order and numeric spelling can matter.
- XML input preserves scalar spelling as **strings** (e.g. `"007"` / `"true"`), and XML conversion is not a lossless round-trip editor. Names must be valid XML element names; trailing text/whitespace can become `#text`.
- CSV input preserves all cells as **strings** and relational exports add technical IDs. Don't silently infer numbers, bools or arbitrary nested relationships.
- YAML/XML/CSV input preprocessing is **opt-in** through `--preprocess-input`; ensure substitutions create valid source text and validate the result.
- JOP's `--validate-json` is native **structural validation**, not a guarantee of RFC 8259 or application schema compliance. Validate production configs in the application too.
- Set `--include-root` and review the source files before expanding includes; avoid absolute paths unless necessary.
- `--stats` on a tiny document is not a credible performance benchmark. Use large workloads and multiple runs to discuss throughput.

## Test status and source references

See [`TESTED.md`](TESTED.md) for the exact Debian version, uploaded JOP binary SHA-256, executed runs, 71-check verification result, and cleanup verification. **The compiled JOP binary is not included in this repository-ready package.**

- [`mormot.core.unicode.pas`](https://github.com/synopse/mORMot2/blob/master/src/core/mormot.core.unicode.pas)
- [`mormot.core.text.pas`](https://github.com/synopse/mORMot2/blob/master/src/core/mormot.core.text.pas)
- [`mormot.core.json.pas`](https://github.com/synopse/mORMot2/blob/master/src/core/mormot.core.json.pas)
- [`mormot.core.fmt.pas`](https://github.com/synopse/mORMot2/blob/master/src/core/mormot.core.fmt.pas)
- [`JOP tool and documentation`](https://github.com/synopse/mORMot2/tree/master/src/tools/jop)
- [`TTestCoreJop` regression tests](https://github.com/synopse/mORMot2/blob/master/test/test.core.fmt.pas)
