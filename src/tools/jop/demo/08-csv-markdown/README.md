# 08 — CSV import and relational Markdown export

**Run:** `./run.sh`. This example demonstrates why a table is not the same thing as a JSON document tree.

## What to watch

1. `people.csv` becomes a JSON array. The cells `007`, `true`, `false` become **JSON strings**: `"007"`, `"true"`, `"false"`.
2. Exporting to CSV adds the `__rowid` column for the relational table layout.
3. `orders.json` has nested `orders` and `items` arrays. Markdown output contains several related tables; `__parentid` associates each child row with its parent.
4. Compare `--to md` (fenced CSV sections) and `--to md-pipe` (pipe tables).

## Best practices and tricks

- **Never infer financial codes, IDs or postal codes as numbers** merely because a CSV cell looks numeric. `007` is not `7`.
- Nested JSON may produce **multiple relational tables**. A single flat `.csv` destination cannot faithfully encode arbitrary nested arrays. Use `JopProcessToCsv()` if your Pascal code needs the named table collection.
- Relational `__rowid` and `__parentid` are exporter keys, not fields originally present in the JSON.
- The JOP CLI delegates to the existing `VariantToCsv` and `VariantToMd` code rather than inventing separate table semantics.
- Markdown is **output-only** in JOP. For round-trip interchange, choose a structured format with compatible types.

**Sources:** `people.csv`, `orders.json`. **Generated:** normalized JSON, relational CSV, and two Markdown formats.

**Relevant source:** `mormot.core.fmt` (`VariantToCsv`, `VariantToMd`), `mormot.tools.jop`.
