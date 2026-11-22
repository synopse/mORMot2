#!/bin/sh
set -eu
cd "$(dirname "$0")"
. ../_demo.sh
heading '10 - Compare mismatch and safe destination preservation'
show one.json
show different.json
if ../jop compare one.json different.json; then
  echo 'ERROR: differing documents unexpectedly compared equal' >&2
  exit 1
else
  rc=$?
  [ "$rc" -eq 1 ] || { echo "Unexpected compare error: $rc" >&2; exit 1; }
  printf 'Expected compare exit code: %s (different)\n' "$rc"
fi
show reordered.json
if ../jop compare one.json reordered.json; then
  echo 'ERROR: reordered members unexpectedly compared equal' >&2
  exit 1
else
  rc=$?
  [ "$rc" -eq 1 ] || { echo "Unexpected reordered compare error: $rc" >&2; exit 1; }
  printf 'Expected compare exit code: %s (property order matters)\n' "$rc"
fi
printf '{"kept":"yes"}\n' > generated-keep.json
show broken.xml
if ../jop convert broken.xml generated-keep.json; then
  echo 'ERROR: malformed XML unexpectedly converted' >&2
  exit 1
else
  rc=$?
  [ "$rc" -eq 2 ] || { echo "Unexpected XML parse error code: $rc" >&2; exit 1; }
  printf 'Expected malformed input exit code: %s\n' "$rc"
fi
show generated-keep.json
[ "$(cat generated-keep.json)" = '{"kept":"yes"}' ]
heading 'An existing output survives failed conversion'
