#!/bin/sh
# Shared POSIX-shell helpers for EKON 30 JOP demos. Sourced from each run.sh.
set -eu
if [ ! -x ../jop ]; then
  printf '%s\n' 'ERROR: ../jop is not executable.' >&2
  printf '%s\n' 'From demo/: ln -s ../jop jop   (or link to your actual JOP executable).' >&2
  exit 2
fi
heading() { printf '\n\n========== %s ==========\n' "$*"; }
show() { printf '\n--- cat %s ---\n' "$1"; cat "$1"; printf '\n'; }
run() {
  printf '\n+ ';
  for arg do printf '%s ' "$arg"; done
  printf '\n';
  "$@"
}
expect_file() {
  if [ ! -s "$1" ]; then
    printf 'CHECK FAILED: expected a nonempty file: %s\n' "$1" >&2
    exit 1
  fi
  printf 'PASS: %s exists and is nonempty\n' "$1"
}
expect_no_file() {
  if [ -e "$1" ]; then
    printf 'CHECK FAILED: unexpected file exists: %s\n' "$1" >&2
    exit 1
  fi
  printf 'PASS: %s was not created\n' "$1"
}
expect_contains() {
  # Exact literal substring, no regular expression or shell expansion of pattern.
  if ! LC_ALL=C grep -F -- "$2" "$1" >/dev/null; then
    printf 'CHECK FAILED: file %s does not contain: %s\n' "$1" "$2" >&2
    exit 1
  fi
  printf 'PASS: %s contains expected text\n' "$1"
}
