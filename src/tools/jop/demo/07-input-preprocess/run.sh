#!/bin/sh
set -eu
cd "$(dirname "$0")"
. ../_demo.sh
heading '07 - Opt-in YAML/XML preprocessing before parsing'
show template.yaml
run ../jop convert template.yaml generated-yaml.literal.json --compact --define name=Alice
show generated-yaml.literal.json
run ../jop convert template.yaml generated-yaml.expanded.json --compact --define name=Alice --preprocess-input
show generated-yaml.expanded.json
show template.xml
run ../jop convert template.xml generated-xml.literal.json --compact --define answer=42
show generated-xml.literal.json
run ../jop convert template.xml generated-xml.expanded.json --compact --define answer=42 --preprocess-input
show generated-xml.expanded.json
expect_contains generated-yaml.literal.json '$(name)'
expect_contains generated-yaml.expanded.json '"name":"Alice"'
expect_contains generated-xml.literal.json '$(answer)'
expect_contains generated-xml.expanded.json '"answer":"42"'
heading 'Preprocess input only when explicitly requested'
