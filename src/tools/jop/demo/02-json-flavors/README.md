# 02 — One document, several JSON-family syntaxes

**Run:** `./run.sh`. The fixture `config.hjson` is intentionally relaxed and intended for human editing.

## What to watch

- The Hjson input uses `#` comments, unquoted names and an unquoted `localhost` value.
- Strict compact JSON (`generated-config.json`) contains normal quoted keys and strings and has no comments.
- JSON5 can use unquoted keys, trailing commas and comments; JSONC retains comments while retaining quoted property names.
- MorML (`generated-config.morml`) is **mORMot's very compact JSON-family rendering**, not a universal JSON standard.
- The script performs native comparisons with the JSON5 and JSONC outputs, converts MorML back to JSON and compares again. All must report **Equal**.

## Best practices and tricks

- The direct **SAX-style** JSON-family path uses `TJsonParser` / `TJsonWriter`, without building a `TDocVariantData` document tree for normal format conversion.
- Convert from the original, commented source if you want comments in a comment-capable output; once you convert through strict JSON the comments are already gone.
- Choose the format for its consumer: a relaxed dialect is convenient to edit, but an external client may demand strict JSON.
- `jop compare` is **order-sensitive**, even when two JSON objects could be considered semantically equivalent.

**Source:** `config.hjson`. **Generated here:** five `generated-*` files, including the MorML round-trip output.

**Relevant source:** `mormot.core.json`, `mormot.core.fmt` and `mormot.tools.jop`.
