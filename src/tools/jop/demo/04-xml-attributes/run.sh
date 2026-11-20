#!/bin/sh
set -eu
cd "$(dirname "$0")"
. ../_demo.sh
heading '04 - Native XML / JSON mapping: attributes and text'
show catalog.json
run ../jop convert catalog.json generated-catalog.xml
show generated-catalog.xml
run ../jop convert generated-catalog.xml generated-catalog.restored.json --compact
show generated-catalog.restored.json
run ../jop compare catalog.json generated-catalog.restored.json
run ../jop convert generated-catalog.xml generated-catalog.yaml
show generated-catalog.yaml
show values.xml
run ../jop convert values.xml generated-values.json --compact
show generated-values.json
expect_contains generated-catalog.xml '<book id="007">'
expect_contains generated-values.json '"@id":"007"'
expect_contains generated-values.json '"active":"true"'
heading 'Look for @id, #text, repeated book[] and string-valued 007/true'
