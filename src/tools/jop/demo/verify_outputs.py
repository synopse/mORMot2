#!/usr/bin/env python3
"""Optional semantic checks for the ten EKON 30 JOP examples (stdlib only).

Use `./verify.sh` to execute JOP first. Normal run.sh demos do NOT require Python.
This file intentionally does not generate or alter output documents.
"""
from __future__ import annotations

import csv
import json
from pathlib import Path
import xml.etree.ElementTree as ET

ROOT = Path(__file__).resolve().parent
checks = 0


def check(condition: bool, message: str) -> None:
    global checks
    if not condition:
        raise RuntimeError('CHECK FAILED: ' + message)
    checks += 1


def text(path: str) -> str:
    f = ROOT / path
    check(f.is_file() and f.stat().st_size > 0, path + ' exists and is nonempty')
    return f.read_text(encoding='utf-8')


def data(path: str):
    return json.loads(text(path))


# 01: generator produces same data regardless of output pretty-printing.
generated = data('01-generate-compare/generated-random.json')
check(isinstance(generated, list) and len(generated) == 5, 'seeded generation yielded 5 items')
check(generated == data('01-generate-compare/generated-random.pretty.json'), 'pretty JSON is equivalent')

# 02: dialects are genuinely equivalent to strict JSON under native reformatting.
config = data('02-json-flavors/generated-config.json')
check(config['server'] == {'host': 'localhost', 'port': 8080, 'tls': True}, 'Hjson server fields/types')
check(config['paths'] == ['/srv/data', '/var/log/jop'], 'Hjson paths array')
check(config['limits'] == {'retry': 3, 'timeout': 30}, 'Hjson nested limits')
check(config == data('02-json-flavors/generated-config.from-morml.json'), 'MorML converted back without loss')
check('// Human-edited configuration' in text('02-json-flavors/generated-config.jsonc'), 'JSONC retained comment')
check('server:' in text('02-json-flavors/generated-config.json5'), 'JSON5 unquoted key')

# 03: a SAX-path output and DOM-path outputs; 007 is a string.
original = data('03-sax-vs-dom/flat.json')
check(original == data('03-sax-vs-dom/generated-flat.back.json'), 'YAML round trip preserves demo values')
check(original[0]['code'] == '007' and original[0]['active'] is True, 'original JSON types')
with (ROOT / '03-sax-vs-dom/generated-flat.csv').open(newline='', encoding='utf-8') as f:
    table = list(csv.DictReader(f))
check([r['code'] for r in table] == ['007', '008'], 'CSV retained zero-padded codes')
check(table[0]['__rowid'] == '1' and table[1]['__rowid'] == '2', 'CSV row IDs')
check(table[0]['active'] == 'true', 'CSV scalar serialization')

# 04: XML native @attributes, repeated book elements and #text mapping.
expected_catalog = data('04-xml-attributes/catalog.json')
restored = data('04-xml-attributes/generated-catalog.restored.json')
check(restored == expected_catalog, 'curated catalog XML -> JSON document equality')
root = ET.fromstring(text('04-xml-attributes/generated-catalog.xml'))
check(root.tag == 'catalog' and root.attrib.get('version') == '1', 'XML root and version attribute')
books = root.findall('book')
check(len(books) == 2, 'XML book array became repeated sibling elements')
check(books[0].attrib['id'] == '007' and books[0].findtext('title') == 'A & B', 'XML preserved 007 and escaped ampersand')
notice = root.find('notice')
check(notice is not None and notice.attrib['lang'] == 'en' and notice.text == 'Hello & welcome', 'XML #text and @lang')
values = data('04-xml-attributes/generated-values.json')
check(values['settings']['@id'] == '007', 'XML attribute is a string')
check(values['settings']['active'] == 'true' and values['settings']['code'] == '00123', 'XML text retains spelling and type')
# Native XML parser in supplied build may preserve trailing root-level whitespace in #text.
check('#text' not in values or isinstance(values['#text'], str), 'native trailing XML text is represented as string')

# 05: definitions, CLI values, source override, and conditional generation.
smart = data('05-preprocess/generated-smart.json')
check(smart['mode'] == 'production', 'later source definition overrides earlier and CLI -D')
check(smart['region'] == 'Dusseldorf', 'CLI -D region injected')
check(smart['server']['port'] == 8080 and type(smart['server']['port']) is int, 'typed integer substitution')
check(smart['message'] == 'Deploy production in Dusseldorf', 'interpolated string')
check(smart['features'] == ['logging'], 'conditional included branch')

# 06: reusable definitions in a DHCP-shaped example, JSON -> YAML -> JSON.
dhcp = data('06-dhcp-smart/generated-dhcp.json')
check(dhcp == data('06-dhcp-smart/generated-dhcp.back.json'), 'DHCP YAML roundtrip equality')
check(len(dhcp['scope']) == 2, 'DHCP has two scopes')
check([s['lease-time-seconds'] for s in dhcp['scope']] == [3600, 3600], 'shared lease is numeric')
check([s['dns-servers'] for s in dhcp['scope']] == ['192.168.50.1'] * 2, 'shared DNS substitution')
check(dhcp['scope'][0]['main']['static'] == ['02:11:22:33:44:55=192.168.50.30'], 'static host field preserved')

# 07: opt-in preprocessing changes values before YAML/XML parsing.
y_literal = data('07-input-preprocess/generated-yaml.literal.json')
y_expand = data('07-input-preprocess/generated-yaml.expanded.json')
check(y_literal['service']['name'] == '$(name)', 'YAML literal substitution remains inert by default')
check(y_expand['service']['name'] == 'Alice', 'YAML input expanded with explicit flag')
x_literal = data('07-input-preprocess/generated-xml.literal.json')
x_expand = data('07-input-preprocess/generated-xml.expanded.json')
check(x_literal['root']['answer'] == '$(answer)', 'XML literal substitution remains inert by default')
check(x_expand['root']['answer'] == '42', 'XML input expanded with explicit flag')
check(isinstance(x_expand['root']['answer'], str), 'XML text remains string after expansion')

# 08: CSV cells are strings; relational Markdown creates linked child tables.
people = data('08-csv-markdown/generated-people.json')
check(len(people) == 2 and people[0]['code'] == '007', 'CSV import retains zero-padded code')
check(people[0]['active'] == 'true', 'CSV import does not guess boolean types')
with (ROOT / '08-csv-markdown/generated-people-export.csv').open(newline='', encoding='utf-8') as f:
    csvrows = list(csv.DictReader(f))
check(csvrows[0]['__rowid'] == '1' and csvrows[1]['__rowid'] == '2', 'CSV exporter emits row IDs')
check(csvrows[0]['code'] == '007', 'CSV export retains code')
md = text('08-csv-markdown/generated-orders.md')
check('root.orders.items.csv' in md and '__parentid' in md, 'fenced Markdown has related child tables')
mdpipe = text('08-csv-markdown/generated-orders-pipe.md')
check('__rowid' in mdpipe and '__parentid' in mdpipe and '| A001 | Alice |' in mdpipe, 'pipe Markdown has linked rows')

# 09: output stayed in run.sh folder with original subfolder hierarchy.
site_a = text('09-batch/site-a.yaml')
site_b = text('09-batch/sub/site-b.yaml')
check('site: A' in site_a and 'port: 8080' in site_a, 'site A YAML contents')
check('site: B' in site_b and 'port: 8081' in site_b, 'site B nested YAML contents')
check(not (ROOT/'09-batch/out').exists(), 'batch did not create an out directory')

# 10: the shell asserted negative exit codes, Python independently verifies data.
check(data('10-error-cases/generated-keep.json') == {'kept': 'yes'}, 'failed XML conversion preserved destination')

print(f'PASS: {checks} semantic/file checks across all ten JOP demos.')
