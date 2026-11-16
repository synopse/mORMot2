-- primary keys and secondary indexes, created after the bulk load
ALTER TABLE region ADD PRIMARY KEY (r_regionkey);
ALTER TABLE nation ADD PRIMARY KEY (n_nationkey);
ALTER TABLE part ADD PRIMARY KEY (p_partkey);
ALTER TABLE supplier ADD PRIMARY KEY (s_suppkey);
ALTER TABLE partsupp ADD PRIMARY KEY (ps_partkey, ps_suppkey);
ALTER TABLE customer ADD PRIMARY KEY (c_custkey);
ALTER TABLE orders ADD PRIMARY KEY (o_orderkey);
ALTER TABLE lineitem ADD PRIMARY KEY (l_orderkey, l_linenumber);
-- lineitem_shipdate covers TPC-H Q6 (index-only scan after VACUUM)
CREATE INDEX lineitem_shipdate ON lineitem(l_shipdate)
  INCLUDE (l_discount, l_quantity, l_extendedprice);
CREATE INDEX lineitem_partkey ON lineitem(l_partkey);
CREATE INDEX lineitem_suppkey ON lineitem(l_suppkey);
CREATE INDEX orders_custkey ON orders(o_custkey);
CREATE INDEX orders_orderdate ON orders(o_orderdate);
CREATE INDEX customer_nationkey ON customer(c_nationkey);
CREATE INDEX customer_mktsegment ON customer(c_mktsegment);
CREATE INDEX supplier_nationkey ON supplier(s_nationkey);
-- visibility map (index-only scans) and planner statistics
VACUUM ANALYZE;
