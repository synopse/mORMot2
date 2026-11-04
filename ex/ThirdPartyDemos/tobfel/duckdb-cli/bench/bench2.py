import subprocess, time, statistics, sys
URI='quack:127.0.0.1:9500'
Q={'top10 x20':("select station_name, count(*) as calls from services group by 1 order by 2 desc limit 10",20),
   'all_rows x5':("select * from services",5)}
N=5
print('%-12s %-22s %9s %9s'%('scenario','client','median ms','min ms'))
for qn,(q,reps) in Q.items():
    qq=q.replace("'","''")
    open('s_plain.sql','w').write((q+';\n')*reps)
    open('s_quack.sql','w').write(("SELECT * FROM quack_query('%s','%s',token='tok1234');\n"%(URI,qq))*reps)
    cl={
     'duckdb-cli embedded':(['./duckdb','-readonly','rail_catalog.duckdb','-csv'],'s_plain.sql'),
     'duckdbcli embedded':(['./duckdbcli','rail_catalog.duckdb','-r','-m','csv','-f','s_plain.sql'],None),
     'duckdb-cli quack':(['./duckdb','-csv'],'s_quack.sql'),
     'duckdbcli quack':(['./duckdbcli','-q',URI,'-w','tok1234','-m','csv','-f','s_plain.sql'],None),
     'duckdbcli proxy':(['./duckdbcli','-p','127.0.0.1:8898','--root','rail','-u','user','-w','pass','-m','csv','-f','s_plain.sql'],None)}
    for cn,(cmd,stdin) in cl.items():
        ts=[]
        for i in range(N+1):
            f=open(stdin,'rb') if stdin else None
            t=time.perf_counter()
            r=subprocess.run(cmd,stdin=f,stdout=subprocess.DEVNULL,stderr=subprocess.PIPE)
            dt=(time.perf_counter()-t)*1000
            if r.returncode: print('ERR',cn,r.stderr[:200]); break
            if i: ts.append(dt)
        if ts: print('%-12s %-22s %9.1f %9.1f'%(qn,cn,statistics.median(ts),min(ts)))
