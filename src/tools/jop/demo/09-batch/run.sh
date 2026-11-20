#!/bin/sh
set -eu
cd "$(dirname "$0")"
. ../_demo.sh
heading '09 - Recursive batch conversion with --dry-run'
show input/site-a.json
show input/sub/site-b.json
# Reset only the known generated outputs so --dry-run visibly creates nothing.
rm -f -- site-a.yaml sub/site-b.yaml
run ../jop batch input . --to yaml --recursive --dry-run
expect_no_file site-a.yaml
expect_no_file sub/site-b.yaml
run ../jop batch input . --to yaml --recursive --stats
show site-a.yaml
show sub/site-b.yaml
expect_contains site-a.yaml 'site: A'
expect_contains sub/site-b.yaml 'port: 8081'
heading 'Batch preflights destinations and preserves relative paths'
