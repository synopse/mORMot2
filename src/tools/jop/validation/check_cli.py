from pathlib import Path
import subprocess, json, sys
import xml.etree.ElementTree as ET
base=Path(__file__).resolve().parent
exe=Path(sys.argv[1]) if len(sys.argv)>1 else base/'bin/jop'
work=base/'cases'
work.mkdir(exist_ok=True)
checks=[]
def run(args, expected=0):
    p=subprocess.run([str(exe), *map(str,args)], text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=work)
    assert p.returncode==expected, (args,p.returncode,p.stdout,p.stderr)
    checks.append({'args':list(map(str,args)),'exit':p.returncode})
    return p.stdout

def dep(o):
    if isinstance(o,dict): return 1+max([dep(v) for v in o.values()] or [0])
    if isinstance(o,list): return 1+max([dep(v) for v in o] or [0])
    return 0
help_text=run(['--help'])
assert 'Exit codes' in help_text and 'xml' in help_text
run([])
assert '0 scalar' in run(['generate','--help'])
for depth in (0,1,3,64):
    p=work/f'depth{depth}.json'
    run(['generate',p,'--depth',depth,'--items',3,'--seed',42])
    assert dep(json.loads(p.read_text()))==depth
run(['generate','repeat.json','--depth',3,'--items',3,'--seed',42])
assert (work/'repeat.json').read_bytes()==(work/'depth3.json').read_bytes()
run(['generate','different.json','--depth',3,'--items',3,'--seed',43])
assert (work/'different.json').read_bytes()!=(work/'repeat.json').read_bytes()
run(['generate','empty-array.json','--items',0,'--seed',4294967295])
assert (work/'empty-array.json').read_bytes()==b'[]'
(work/'keep.json').write_text('unchanged')
for opts in (['--depth',65],['--depth=-1'],['--items','bad'],['--seed',4294967296],['--seed='],['--unknown'],['extra']):
    run(['generate','keep.json',*opts],2)
    assert (work/'keep.json').read_text()=='unchanged'
run(['generate'],2)
(work/'a.json').write_text('{"a":1,"b":[true,"x"]}')
(work/'b.json5').write_text('// comment\n{ a: 1, b: [true, "x",], }')
run(['compare','a.json','b.json5'])
(work/'different-order.json').write_text('{"b":[true,"x"],"a":1}')
run(['compare','a.json','different-order.json'],1)
run(['compare','repeat.json','different.json'],1)
run(['compare','a.json','missing.json'],2)
(work/'blank.json').write_text('')
run(['compare','a.json','blank.json'],2)
run(['convert','a.json','a.yaml','--no-expand'])
run(['convert','a.yaml','roundtrip.json','--compact'])
run(['compare','a.json','roundtrip.json'])
run(['a.json','implicit.json','--compact','--stats'])
run(['compare','a.json','implicit.json'])
(work/'table.json').write_text('[{"name":"A","count":1},{"name":"B","count":2}]')
run(['convert','table.json','table.md'])
assert '```csv' in (work/'table.md').read_text()
run(['convert','table.json','pipe.md','--to','md-pipe'])
assert '```csv' not in (work/'pipe.md').read_text()
assert '| name |' in (work/'pipe.md').read_text() or '| __rowid |' in (work/'pipe.md').read_text()
run(['convert','table.json','alias.md','--to','md-csv'])
assert (work/'alias.md').read_bytes()==(work/'table.md').read_bytes()
run(['convert','table.json','table.csv'])
assert 'count' in (work/'table.csv').read_text()
# XML uses the native @attribute/#text mapping and retains all input scalars as strings.
catalog={'catalog': {'book': [
    {'title': 'A & B', '@id': '007'},
    {'title': 'Second', '@id': '008'}], '@version': '1'}}
(work/'catalog.json').write_text(json.dumps(catalog))
run(['convert','catalog.json','catalog.XML'])
xml=(work/'catalog.XML').read_bytes()
assert xml.startswith(b'<?xml version="1.0" encoding="UTF-8"?>\r\n')
root=ET.fromstring(xml)
assert root.tag=='catalog' and root.attrib=={'version': '1'}
assert [(b.attrib['id'],b.findtext('title')) for b in root.findall('book')]==[
    ('007','A & B'),('008','Second')]
run(['convert','catalog.XML','catalog-restored.json','--compact'])
assert json.loads((work/'catalog-restored.json').read_text())==catalog
run(['convert','catalog.XML','catalog.yaml'])
run(['convert','catalog.yaml','catalog-yaml.xml'])
assert ET.tostring(ET.parse(work/'catalog-yaml.xml').getroot())==ET.tostring(root)
run(['convert','catalog.json','xml-output.data','--to','xml'])
run(['convert','xml-output.data','xml-restored.data','--from','xml','--to','json','--compact'])
assert json.loads((work/'xml-restored.data').read_text())==catalog
(work/'xml-template.xml').write_text('<root>$(answer)</root>')
run(['convert','xml-template.xml','xml-literal.json','--define','answer=42','--compact'])
assert json.loads((work/'xml-literal.json').read_text())=={'root':'$(answer)'}
run(['convert','xml-template.xml','xml-expanded.json','--preprocess-input','--define','answer=42','--compact'])
assert json.loads((work/'xml-expanded.json').read_text())=={'root':'42'}
run(['convert','xml-template.xml','xml-noexpand.json','--preprocess-input','--no-expand','--define','answer=42','--compact'])
assert json.loads((work/'xml-noexpand.json').read_text())=={'root':'$(answer)'}
(work/'xml-broken.xml').write_text('<root><item></root>')
assert 'Invalid XML' in run(['convert','xml-broken.xml','keep.json'],2)
assert (work/'keep.json').read_text()=='unchanged'
(work/'xml-unknown-entity.xml').write_text('<root>&unknown;</root>')
run(['convert','xml-unknown-entity.xml','keep.json'],2)
assert (work/'keep.json').read_text()=='unchanged'
(work/'xml-batch-in'/'nested').mkdir(parents=True,exist_ok=True)
(work/'xml-batch-in'/'a.xml').write_text('<root>007</root>')
(work/'xml-batch-in'/'nested'/'b.xml').write_text('<root>true</root>')
(work/'xml-batch-in'/'ignored.bin').write_text('not a document')
run(['batch','xml-batch-in','xml-batch-json','--recursive','--compact'])
assert json.loads((work/'xml-batch-json'/'a.json').read_text())=={'root':'007'}
assert json.loads((work/'xml-batch-json'/'nested'/'b.json').read_text())=={'root':'true'}
assert not (work/'xml-batch-json'/'ignored.json').exists()
run(['batch','xml-batch-json','xml-batch-out','--to','xml','--recursive'])
assert ET.parse(work/'xml-batch-out'/'a.xml').getroot().text=='007'
assert ET.parse(work/'xml-batch-out'/'nested'/'b.xml').getroot().text=='true'
run(['generate','buffered.json','--items',2000,'--depth',6,'--seed',1234])
assert len(json.loads((work/'buffered.json').read_text()))==2000
run(['compare','buffered.json','buffered.json'])
(base/'cli-results.json').write_text(json.dumps(checks,indent=2)+'\n')
print(f'{len(checks)} command checks passed; valid deterministic JSON at depths 0/1/3/64, stream buffer rollover, native XML/Markdown formats, XML batch/preprocessing and error destination preservation')
