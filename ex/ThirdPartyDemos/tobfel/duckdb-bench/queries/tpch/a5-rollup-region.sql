-- revenue per region and nation with subtotals and a grand total
-- DuckDB and PostgreSQL: GROUP BY ROLLUP
SELECT r_name, n_name, sum(o_totalprice) AS revenue, count(*) AS orders
FROM orders
JOIN customer ON c_custkey = o_custkey
JOIN nation ON n_nationkey = c_nationkey
JOIN region ON r_regionkey = n_regionkey
GROUP BY ROLLUP (r_name, n_name)
ORDER BY r_name NULLS LAST, n_name NULLS LAST
