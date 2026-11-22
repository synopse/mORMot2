# 07 — YAML and XML preprocessing is opt-in

**Run:** `./run.sh`. Compare each input with its two generated JSON outputs.

## What to watch

- YAML `name: "$(name)"` produces the **literal** value `"$(name)"` without expansion.
- Adding `--preprocess-input --define name=Alice` produces `"name":"Alice"`.
- XML `<answer>$(answer)</answer>` behaves the same way. With `--preprocess-input --define answer=42`, its JSON string becomes `"42"`, **not** a JSON number.
- The script checks that the two modes really differ.

## Best practices and tricks

- Source preprocessing happens **before** YAML/XML/CSV parsing and is explicitly opt-in; converted values are not then recursively expanded a second time.
- For XML, inserted text must already be suitable XML text or markup. A substituted value containing `&` or `<` can break XML unless escaped correctly at the source or insertion boundary.
- Use `--no-expand` to bypass expansion even when other flags/definitions are supplied.
- Avoid preprocessing untrusted XML/YAML as a shortcut to validation. Validate the generated data for its destination consumer.
- In this supplied build, a trailing newline after the XML root may be represented as an additional `#text` property. That is a reminder that this is an XML **data** mapping.

**Sources:** `template.yaml`, `template.xml`. **Generated:** literal and expanded JSON for each.

**Relevant source:** `mormot.tools.jop` (`jofPreprocessInput`), `mormot.core.fmt`, native XML/YAML tests.
