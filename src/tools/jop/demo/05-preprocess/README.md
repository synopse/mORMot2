# 05 — Smart JSON: definitions, interpolation and conditions

**Run:** `./run.sh`. This is an optional live demo and the main reusable-DSL example.

## What to watch

```text
$$                       <-- definition section
mode = staging
mode = production        <-- last assignment wins
port = 8080
$$
mode: $(mode)             <-- value insertion: JSON string
port: $(port)             <-- value insertion: JSON number
message: $"Deploy $(mode)" <-- explicit interpolated string
$if enabled$ ... $else$ ... $endif$  <-- conditional output
```

The script passes two command-line definitions: `region=Dusseldorf` and `mode=preview`. The **source** `mode=production` wins, yielding `"mode":"production"`. The output has an **integer** port, a populated message, and `"features":["logging"]`.

## Best practices and tricks

- JOP is a **small preprocessor**, not a scripting language: no loop construct, no nested variable scopes, no implicit network access. Its shared definition dictionary is intentionally simple.
- **Later definitions win.** Treat `-D` values as defaults if the source may override them, not as an enforcement mechanism.
- Use `$(name)` for a typed value; use `$"... $(name) ..."` when you really want interpolated text. An ordinary quoted `"$(name)"` need not expand.
- `$if` chooses what is emitted; it is not an application runtime condition.
- Prefer generating **ordinary JSON** as a build/deployment step and loading that file from the application. Disable expansion via `--no-expand` when processing literal data.

**Source:** `smart.hjson`. **Generated:** `generated-smart.json`.

**Relevant source:** `TPreproc` in `mormot.core.fmt`; `TTestCoreJop.Definitions` and `TTestCoreJop.Text`.
