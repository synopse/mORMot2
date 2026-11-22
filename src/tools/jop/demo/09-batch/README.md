# 09 — Recursive batch conversion and dry-run

**Run:** `./run.sh`. This is a good post-session demonstration of operational tooling.

## What to watch

- Source files are `input/site-a.json` and `input/sub/site-b.json`. The script prints both.
- It removes *only the two expected prior generated YAML outputs* before a dry run. `--dry-run` reports the planned destinations and creates **neither** file.
- The actual `batch input . --to yaml --recursive --stats` command generates `site-a.yaml` and `sub/site-b.yaml` next to this folder's `run.sh`, preserving the relative subdirectory.
- No `out/` folder is ever created. The script checks the dry-run and final content.

## Best practices and tricks

- On a production collection, **always start with `--dry-run`** to inspect the destination plan. JOP detects path collisions before committing any output.
- Quote wildcard masks if you use them (`'input/*.json'`) so the shell does not expand them prematurely; this demo instead passes the source folder name.
- `--recursive` keeps the relative hierarchy. Avoid output paths that overlap your source files, and review auto-detected extensions.
- Batch output is **not one global filesystem transaction**; one failed file may not roll back another successful conversion. Check the command result, individual statuses and backups where appropriate.
- Run `../clean.sh` to remove generated YAML files while keeping the `input/` fixture tree.

**Sources:** `input/site-a.json`, `input/sub/site-b.json`. **Generated:** `site-a.yaml`, `sub/site-b.yaml`.

**Relevant source:** `JopProcessFiles`, `JopProcessFile`, `jop.dpr`.
