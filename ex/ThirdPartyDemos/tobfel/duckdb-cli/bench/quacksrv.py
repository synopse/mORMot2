import duckdb,time,sys
c=duckdb.connect('rail_catalog.duckdb', read_only=True)
c.sql('LOAD quack'); c.sql("CALL quack_serve('quack:127.0.0.1:9500', token='tok1234')")
time.sleep(int(sys.argv[1]))
