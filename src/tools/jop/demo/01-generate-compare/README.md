# 01 — Seeded JSON generation, conversion and comparison

**Run:** `./run.sh` from this folder, or `./01-generate-compare/run.sh` from `demo/`.
The script prints each generated file. This is the final, short live demo in the JOP CLI chapter.

## What to watch

1. `../jop generate generated-random.json --depth 3 --items 5 --seed 42` produces an array of five reproducible sample items with nested structures.
2. `convert ... --no-expand` formats that data as indented JSON, with no DSL substitution.
3. `compare` prints **Equal** and exits **0**, although the files have completely different whitespace.

## Best practices and tricks

- A fixed `--seed` is excellent for slide rehearsals and reproducing test failures; don't present this generator as cryptographically secure random data.
- `compare` normalizes native compact JSON text. It ignores irrelevant layout and comments, **but member order and numeric spelling remain significant**. It is not unordered DOM equality.
- Use `--no-expand` when only formatting data that might contain literal preprocessor-like strings.
- The `--depth` and `--items` arguments constrain what the generator produces. Keep the output small enough to fit on a projector.

**Generated here:** `generated-random.json`, `generated-random.pretty.json`.
`../clean.sh` removes these files. The script checks both output files exist and exits nonzero on comparison failure.

**Relevant source:** `src/tools/jop/jop.dpr` (`generate`, `compare`) and `mormot.tools.jop`.
