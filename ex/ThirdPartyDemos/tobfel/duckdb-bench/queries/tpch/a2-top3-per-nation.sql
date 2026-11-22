-- the 3 best customers of each nation
-- DuckDB: QUALIFY filters on the window function, no subquery needed
SELECT n_name, c_name, sum(o_totalprice) AS revenue
FROM orders
JOIN customer ON c_custkey = o_custkey
JOIN nation ON n_nationkey = c_nationkey
GROUP BY n_name, c_name
QUALIFY row_number() OVER (PARTITION BY n_name ORDER BY revenue DESC) <= 3
ORDER BY n_name, revenue DESC
