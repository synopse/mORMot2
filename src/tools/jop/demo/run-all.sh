#!/bin/sh
set -eu
cd "$(dirname "$0")"
for d in 01-* 02-* 03-* 04-* 05-* 06-* 07-* 08-* 09-* 10-*; do
  [ -d "$d" ] || continue
  printf '\n\n===========================================\n'
  printf 'DEMO: %s\n' "$d"
  printf '===========================================\n'
  (cd "$d" && ./run.sh)
done
printf '\nALL DEMOS COMPLETED.\n'
