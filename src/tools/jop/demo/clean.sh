#!/bin/sh
# Remove generated outputs from all 10 JOP demos. Preserve input fixtures,
# run.sh, clean.sh, README files and the compiled-tool symlink (demo/jop).
set -eu
cd "$(dirname "$0")"
for d in [0-9][0-9]-*; do
  [ -d "$d" ] || continue
  find "$d" -maxdepth 1 -type f -name 'generated-*' ! -name '*.sh' -exec rm -f -- {} +
done
# The batch demo must keep relative directory paths, so its two outputs
# cannot both use a generated-* basename.
rm -f -- 09-batch/site-a.yaml 09-batch/sub/site-b.yaml
rmdir 09-batch/sub 2>/dev/null || :
printf '%s
' 'Generated demo outputs cleaned; source fixtures and scripts preserved.'
