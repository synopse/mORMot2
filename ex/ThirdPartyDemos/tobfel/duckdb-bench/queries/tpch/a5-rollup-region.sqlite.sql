-- no ROLLUP in SQLite: one UNION ALL branch per grouping level
WITH detail AS (
  SELECT r_name, n_name, sum(o_totalprice) AS revenue, count(*) AS orders
  FROM orders
  JOIN customer ON c_custkey = o_custkey
  JOIN nation ON n_nationkey = c_nationkey
  JOIN region ON r_regionkey = n_regionkey
  GROUP BY r_name, n_name)
SELECT r_name, n_name, revenue, orders FROM detail
UNION ALL
SELECT r_name, NULL, sum(revenue), sum(orders) FROM detail GROUP BY r_name
UNION ALL
SELECT NULL, NULL, sum(revenue), sum(orders) FROM detail
ORDER BY r_name NULLS LAST, n_name NULLS LAST
