# 04 — XML attributes, text content and repeated elements

**Run:** `./run.sh`. It converts a hand-curated JSON catalog to XML, returns it to JSON, compares, exports YAML, and parses a separate XML file.

## What to watch

| JSON convention | XML layout |
| --- | --- |
| `"@id": "007"` | `id="007"` attribute |
| `"#text": "Hello"` | Text inside the element |
| `"book": [{…}, {…}]` | Repeated `<book>` siblings |
| `"title": "A & B"` | `<title>A &amp; B</title>` |

The curated catalog round trip compares **Equal**. `values.xml` also demonstrates that XML parses `007`, `00123` and `true` as **strings**, not inferred integers/booleans.

## Best practices and tricks

- Supply **one named root element** with valid XML names when requesting a complete XML document. JOP does not invent a wrapper or sanitize arbitrary JSON property names into XML names.
- This is **data-model conversion**, not a lossless XML document editor: comments, processing instructions, CDATA identity, mixed-content ordering, and interleaving of repeated sibling names may not survive.
- The native converter can expose XML whitespace as `#text`. In the supplied build, the newline after the document root in `values.xml` appears as a separate root-level `"#text":"\n"` field. Do not assume all XML whitespace disappears.
- XML attribute/text values stay strings, and special characters are escaped when JOP writes XML.
- `compare` ignores formatting but remains sensitive to property order; the sample was designed for a successful normalized comparison.

**Source:** `catalog.json`, `values.xml`. **Generated:** XML, round-tripped JSON, YAML and values JSON.

**Relevant source:** `mormot.core.fmt` (`XmlToVariant`, XML writer), `mormot.tools.jop`.
