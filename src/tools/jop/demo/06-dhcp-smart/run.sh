#!/bin/sh
set -eu
cd "$(dirname "$0")"
. ../_demo.sh
heading '06 - Include shared DHCP definitions, generate final JSON'
show common.def
show dhcp.hjson
run ../jop convert dhcp.hjson generated-dhcp.json --include-root . --compact --validate-json
show generated-dhcp.json
run ../jop convert generated-dhcp.json generated-dhcp.yaml
show generated-dhcp.yaml
run ../jop convert generated-dhcp.yaml generated-dhcp.back.json --compact
run ../jop compare generated-dhcp.json generated-dhcp.back.json
expect_contains generated-dhcp.json '"lease-time-seconds":3600'
heading 'Human-edited sources -> normalized application settings'
