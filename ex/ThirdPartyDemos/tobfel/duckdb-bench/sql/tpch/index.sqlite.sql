-- secondary indexes, created after the bulk load, then planner statistics
-- lineitem_shipdate covers TPC-H Q6 (index-only range scan)
CREATE INDEX lineitem_shipdate ON lineitem(l_shipdate, l_discount, l_quantity,
  l_extendedprice);
CREATE INDEX lineitem_partkey ON lineitem(l_partkey);
CREATE INDEX lineitem_suppkey ON lineitem(l_suppkey);
CREATE INDEX orders_custkey ON orders(o_custkey);
CREATE INDEX orders_orderdate ON orders(o_orderdate);
CREATE INDEX customer_nationkey ON customer(c_nationkey);
CREATE INDEX customer_mktsegment ON customer(c_mktsegment);
CREATE INDEX supplier_nationkey ON supplier(s_nationkey);
ANALYZE;
