#!/usr/bin/env bash
# Build JOP from an mORMot checkout, with an isolated compiler-output folder.
# Usage: FPC=/path/to/fpc JOP_STATIC_DIR=/path/to/static \
#        JOP_GCC_DIR=/path/to/gcc/lib ./build_cli.sh /path/to/mormot2 [output-dir]
set -euo pipefail
jop_repo="${1:?Usage: build_cli.sh mormot-root [output-dir]}"
jop_build="${2:-$jop_repo/build/jop}"
jop_compiler="${FPC:-fpc}"
mkdir -p "$jop_build/units" "$jop_build/bin"
jop_args=(
  -MDelphi -Scgi -O2 -gl
  "-Fu$jop_repo/src/tools/jop" "-Fu$jop_repo/src/core"
  "-Fu$jop_repo/src/crypt" "-Fu$jop_repo/src/net" "-Fu$jop_repo/src/lib"
  "-Fi$jop_repo/src" "-Fi$jop_repo/src/core"
  "-FU$jop_build/units" "-FE$jop_build/bin"
  -dNOLIBCSTATIC -dNOLIBDEFLATESTATIC
)
if [[ -n "${JOP_STATIC_DIR:-}" ]]; then
  jop_args+=("-Fl$JOP_STATIC_DIR")
fi
if [[ -n "${JOP_GCC_DIR:-}" ]]; then
  jop_args+=("-Fl$JOP_GCC_DIR")
fi
cd "$jop_repo"
"$jop_compiler" "${jop_args[@]}" src/tools/jop/jop.dpr
