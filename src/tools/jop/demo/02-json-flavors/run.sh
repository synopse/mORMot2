#!/bin/sh
set -eu
cd "$(dirname "$0")"
. ../_demo.sh
heading '02 - Hjson -> JSON / JSON5 / JSONC / MorML'
show config.hjson
run ../jop convert config.hjson generated-config.json --compact
show generated-config.json
run ../jop convert config.hjson generated-config.json5
show generated-config.json5
run ../jop convert config.hjson generated-config.jsonc
show generated-config.jsonc
run ../jop convert config.hjson generated-config.morml
show generated-config.morml
run ../jop compare generated-config.json generated-config.json5
run ../jop compare generated-config.json generated-config.jsonc
run ../jop convert generated-config.morml generated-config.from-morml.json --compact
run ../jop compare generated-config.json generated-config.from-morml.json
expect_contains generated-config.json '"host":"localhost"'
expect_contains generated-config.jsonc '// Human-edited configuration'
heading 'Four outputs, one small JSON data model'
