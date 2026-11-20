#!/bin/sh
set -eu
cd "$(dirname "$0")"
. ../_demo.sh
heading '08 - CSV import, relational CSV, Markdown tables'
show people.csv
run ../jop convert people.csv generated-people.json --compact
show generated-people.json
run ../jop convert generated-people.json generated-people-export.csv
show generated-people-export.csv
show orders.json
run ../jop convert orders.json generated-orders.md --to md
show generated-orders.md
run ../jop convert orders.json generated-orders-pipe.md --to md-pipe
show generated-orders-pipe.md
expect_contains generated-people.json '"code":"007"'
expect_contains generated-people-export.csv '__rowid,name,code,active'
expect_contains generated-orders.md '__parentid'
expect_contains generated-orders-pipe.md '__parentid'
heading 'No custom table logic in JOP: delegates to native fmt exporters'
