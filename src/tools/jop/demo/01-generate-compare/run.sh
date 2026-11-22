#!/bin/sh
set -eu
cd "$(dirname "$0")"
. ../_demo.sh
heading '01 - Seeded generation + JSON normalization + compare'
run ../jop generate generated-random.json --depth 3 --items 5 --seed 42
show generated-random.json
run ../jop convert generated-random.json generated-random.pretty.json --no-expand
show generated-random.pretty.json
run ../jop compare generated-random.json generated-random.pretty.json
expect_file generated-random.json
expect_file generated-random.pretty.json
heading 'A seeded generator and two different layouts, same compact JSON'
