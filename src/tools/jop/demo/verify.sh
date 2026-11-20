#!/bin/sh
# Optional full smoke test: requires Python 3 only for JSON/XML/CSV assertions.
# Normal numbered run.sh files require only JOP and basic POSIX utilities.
set -eu
cd "$(dirname "$0")"
if ! command -v python3 >/dev/null 2>&1; then
  echo 'ERROR: verify.sh needs Python 3 (standard library only).' >&2
  exit 2
fi
./run-all.sh
python3 ./verify_outputs.py
