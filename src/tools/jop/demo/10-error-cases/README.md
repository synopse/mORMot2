# 10 — Comparing documents and preserving outputs on errors

**Run:** `./run.sh`. The script treats its expected error cases as **passes**.

## What to watch

- Comparing `one.json` with `different.json` prints **Different** and returns exit code **1**.
- Comparing with `reordered.json` also returns **1**: native compact JSON comparison preserves **property order**.
- `broken.xml` has mismatched tags. Conversion returns **2** and prints a parser error.
- The script first writes `{"kept":"yes"}` to `generated-keep.json` and proves that failed conversion does **not** overwrite it.

## Best practices and tricks

- JOP CLI exit codes: **0** success/equal, **1** different documents, **2** usage/processing failure. In shell scripts, handle code 1 deliberately when a mismatch is an expected business result.
- `compare` is **textual canonicalization**, not deep unordered-object equality. It can distinguish `1` from `1.0`, and equal-looking JSON documents can still differ in property order.
- `JopProcessFile` uses a temporary sibling plus safe replacement after success; the original output survives a conversion error. This does **not** mean that a multi-file batch is a single transaction.
- Keep validation separate from the intended application schema; successful JSON parsing alone cannot prove a configuration is safe or meaningful.

**Sources:** `one.json`, `different.json`, `reordered.json`, `broken.xml`. **Generated:** the protected destination `generated-keep.json`.

**Relevant source:** `JopCompareFiles`, `JopProcessFile` and XML parser regression tests.
