# 06 — DHCP-shaped configuration with reusable includes

**Run:** `./run.sh`. This is the main suggested live demonstration of smart configuration.

## What to watch

1. The script shows `common.def`, then `dhcp.hjson`. The `$$ include common.def $$` section loads reusable values for the lease period, DNS, and gateway.
2. JOP emits ordinary compact JSON with **two scopes** and the shared `lease-time-seconds` value as an **integer** (`3600`). IP-address-like fields remain strings.
3. It exports YAML, converts it back to JSON, and confirms native compact equality. No `out/` folder is created.

## Best practices and tricks

- Set an explicit, trusted `--include-root .`; relative includes are path-checked and include nesting is bounded. Avoid `--absolute-paths` unless explicitly required.
- Separate **human-maintained templates** from the **generated machine configuration**. That makes review/diff and reproducible deployment easier.
- `--validate-json` requests mORMot's native **structural** JSON check, **not full RFC 8259 conformance or application-schema validation**. Validate your own settings before deploying.
- The fixture intentionally borrows field names from the mORMot DHCP example (`scope`, `dns-servers`, `lease-time-seconds`, `main`, `static`). It is **teaching data, not an audited production DHCP configuration**.
- If you add more networks, reuse values only where that makes sense: for example, one shared DNS server may be valid, but default gateways are often specific to each subnet.

**Sources:** `common.def`, `dhcp.hjson`. **Generated:** compact JSON, YAML and round-trip JSON.

**Relevant source:** `mormot.core.fmt` (DSL/includes), `mormot.tools.jop`, mORMot DHCP settings source.
