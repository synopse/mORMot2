#!/bin/sh
set -eu
cd "$(dirname "$0")"
. ../_demo.sh
heading '03 - SAX-style JSON output versus TDocVariantData DOM output'
show flat.json
run ../jop convert flat.json generated-flat.json5 --stats
show generated-flat.json5
run ../jop convert flat.json generated-flat.yaml --stats
show generated-flat.yaml
run ../jop convert flat.json generated-flat.csv --stats
show generated-flat.csv
run ../jop convert generated-flat.yaml generated-flat.back.json --compact
run ../jop compare flat.json generated-flat.back.json
expect_contains generated-flat.csv '__rowid,name,code,active'
expect_contains generated-flat.csv '1,Alice,007,true'
heading 'JSON-family: direct writer; YAML/CSV: document-aware exporter'
