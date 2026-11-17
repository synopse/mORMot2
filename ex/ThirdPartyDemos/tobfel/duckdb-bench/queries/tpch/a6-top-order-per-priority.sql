-- the most expensive order of each priority
-- DuckDB: arg_max() returns the key of the row with the maximum value
SELECT o_orderpriority, arg_max(o_orderkey, o_totalprice) AS orderkey,
  max(o_totalprice) AS totalprice
FROM orders
GROUP BY ALL
ORDER BY o_orderpriority
