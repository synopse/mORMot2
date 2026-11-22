# 03 — Two processing data layouts: SAX-style and DOM

**Run:** `./run.sh`. The single `flat.json` array converts to JSON5, YAML and CSV; each command includes `--stats`.

## What to watch

1. **JSON → JSON5:** native token/reformat path, writing directly with `TJsonWriter` rather than constructing an object per JSON node.
2. **JSON → YAML and CSV:** the structured output path can use `TDocVariantData`, with `InitJsonInPlace()` reading an owned writable JSON buffer, followed by native exporters.
3. The YAML output quotes `"007"` and `"008"` to preserve them as strings. The relational CSV output adds `__rowid`.
4. The script converts YAML back to JSON and verifies compact native equality with the original source.

## Best practices and tricks

- **Use the correct performance claim:** direct JSON-family conversion avoids an intermediate DOM; *not every possible JOP conversion is DOM-free*. Some dialect conversion or preprocessing may first normalize a buffer.
- The in-place variant decoder may mutate `RawUtf8` input. When calling the Pascal API, supply a **private writable** source buffer, not a shared string.
- `--stats` reports bytes and elapsed time for the actual run. These tiny fixtures are educational, **not meaningful throughput benchmarks**. Run larger warmed-up tests for comparisons.
- CSV cells are text on import; converting JSON booleans into CSV and back does not automatically reconstruct the original types.

**Source:** `flat.json`. **Generated:** JSON5, YAML, CSV and the JSON round-trip file.

**Relevant source:** `mormot.core.unicode`, `mormot.core.text`, `mormot.core.json`, `mormot.core.fmt`.
