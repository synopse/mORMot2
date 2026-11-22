-- TPC-H tables for PostgreSQL, filled by DuckDB via ATTACH (TYPE postgres)
-- NUMERIC(15,2) as in the TPC-H specification, VARCHAR instead of CHAR(n)
-- (PostgreSQL wiki "Don't Do This"), keys added after the load
DROP TABLE IF EXISTS lineitem, orders, customer, partsupp, supplier, part,
  nation, region;
CREATE TABLE region(r_regionkey INTEGER NOT NULL, r_name VARCHAR(25) NOT NULL,
  r_comment VARCHAR(152));
CREATE TABLE nation(n_nationkey INTEGER NOT NULL, n_name VARCHAR(25) NOT NULL,
  n_regionkey INTEGER NOT NULL, n_comment VARCHAR(152));
CREATE TABLE part(p_partkey INTEGER NOT NULL, p_name VARCHAR(55) NOT NULL,
  p_mfgr VARCHAR(25) NOT NULL, p_brand VARCHAR(10) NOT NULL,
  p_type VARCHAR(25) NOT NULL, p_size INTEGER NOT NULL,
  p_container VARCHAR(10) NOT NULL, p_retailprice NUMERIC(15,2) NOT NULL,
  p_comment VARCHAR(23) NOT NULL);
CREATE TABLE supplier(s_suppkey INTEGER NOT NULL, s_name VARCHAR(25) NOT NULL,
  s_address VARCHAR(40) NOT NULL, s_nationkey INTEGER NOT NULL,
  s_phone VARCHAR(15) NOT NULL, s_acctbal NUMERIC(15,2) NOT NULL,
  s_comment VARCHAR(101) NOT NULL);
CREATE TABLE partsupp(ps_partkey INTEGER NOT NULL, ps_suppkey INTEGER NOT NULL,
  ps_availqty INTEGER NOT NULL, ps_supplycost NUMERIC(15,2) NOT NULL,
  ps_comment VARCHAR(199) NOT NULL);
CREATE TABLE customer(c_custkey INTEGER NOT NULL, c_name VARCHAR(25) NOT NULL,
  c_address VARCHAR(40) NOT NULL, c_nationkey INTEGER NOT NULL,
  c_phone VARCHAR(15) NOT NULL, c_acctbal NUMERIC(15,2) NOT NULL,
  c_mktsegment VARCHAR(10) NOT NULL, c_comment VARCHAR(117) NOT NULL);
CREATE TABLE orders(o_orderkey INTEGER NOT NULL, o_custkey INTEGER NOT NULL,
  o_orderstatus VARCHAR(1) NOT NULL, o_totalprice NUMERIC(15,2) NOT NULL,
  o_orderdate DATE NOT NULL, o_orderpriority VARCHAR(15) NOT NULL,
  o_clerk VARCHAR(15) NOT NULL, o_shippriority INTEGER NOT NULL,
  o_comment VARCHAR(79) NOT NULL);
CREATE TABLE lineitem(l_orderkey INTEGER NOT NULL, l_partkey INTEGER NOT NULL,
  l_suppkey INTEGER NOT NULL, l_linenumber INTEGER NOT NULL,
  l_quantity NUMERIC(15,2) NOT NULL, l_extendedprice NUMERIC(15,2) NOT NULL,
  l_discount NUMERIC(15,2) NOT NULL, l_tax NUMERIC(15,2) NOT NULL,
  l_returnflag VARCHAR(1) NOT NULL, l_linestatus VARCHAR(1) NOT NULL,
  l_shipdate DATE NOT NULL, l_commitdate DATE NOT NULL,
  l_receiptdate DATE NOT NULL, l_shipinstruct VARCHAR(25) NOT NULL,
  l_shipmode VARCHAR(10) NOT NULL, l_comment VARCHAR(44) NOT NULL);
