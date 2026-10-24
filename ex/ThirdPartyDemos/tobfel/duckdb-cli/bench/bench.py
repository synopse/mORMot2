import subprocess, time, statistics, hashlib, sys, os
Q = {
 'count':    "select count(*) from services",
 'top10':    "select station_name, count(*) as calls from services group by 1 order by 2 desc limit 10",
 'by_type':  "select type, count(*) n, min(departure_time) first_dep, max(arrival_time) last_arr from services group by type order by 1",
 'filter':   "select service_id, station_name, departure_time from services where train_number = 1410 order by departure_time",
 'all_rows': "select * from services",
}
URI='quack:127.0.0.1:9500'
def clients(q):
    qq=q.replace("'","''")
    return {
     'duckdb-cli embedded': ['./duckdb','-readonly','rail_catalog.duckdb','-csv','-c',q],
     'duckdbcli embedded':  ['./duckdbcli','rail_catalog.duckdb','-r','-m','csv','-c',q],
     'duckdb-cli quack':    ['./duckdb','-csv','-c',"SELECT * FROM quack_query('%s','%s',token='tok1234')"%(URI,qq)],
     'duckdbcli quack':     ['./duckdbcli','-q',URI,'-w','tok1234','-m','csv','-c',q],
     'duckdbcli proxy':     ['./duckdbcli','-p','127.0.0.1:8898','--root','rail','-u','user','-w','pass','-m','csv','-c',q],
    }
for k,reps in (('top10',20),('all_rows',5)):
    Q[k+' x%d'%reps]=None
    Q.pop(k+' x%d'%reps)
N=int(sys.argv[1]) if len(sys.argv)>1 else 7
res={}
for qn,q in Q.items():
    for cn,cmd in clients(q).items():
        times=[]; h=None; size=0
        for i in range(N+1):
            t=time.perf_counter()
            r=subprocess.run(cmd,capture_output=True)
            dt=(time.perf_counter()-t)*1000
            if r.returncode!=0:
                print('ERR',qn,cn,r.stderr[:200]); break
            if i==0: 
                size=len(r.stdout); h=hashlib.md5(r.stdout).hexdigest()[:8]; continue
            times.append(dt)
        if times: res[(qn,cn)]=(statistics.median(times),min(times),size,h)
print('%-9s %-22s %9s %9s %10s %s'%('query','client','median ms','min ms','out bytes','md5'))
for (qn,cn),(m,mi,size,h) in res.items():
    print('%-9s %-22s %9.1f %9.1f %10d %s'%(qn,cn,m,mi,size,h))
