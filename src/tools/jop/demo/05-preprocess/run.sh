#!/bin/sh
set -eu
cd "$(dirname "$0")"
. ../_demo.sh
heading '05 - Definitions, interpolation, conditionals, CLI --define'
show smart.hjson
run ../jop convert smart.hjson generated-smart.json --from hjson --compact --define region=Dusseldorf --define mode=preview
show generated-smart.json
expect_contains generated-smart.json '"mode":"production"'
expect_contains generated-smart.json '"port":8080'
expect_contains generated-smart.json '"features":["logging"]'
heading 'Expanded, ordinary JSON; the application needs no JOP at runtime'
