SELECT n_name, c_name, revenue
FROM (
  SELECT n_name, c_name, sum(o_totalprice) AS revenue,
    row_number() OVER (PARTITION BY n_name
      ORDER BY sum(o_totalprice) DESC) AS rn
  FROM orders
  JOIN customer ON c_custkey = o_custkey
  JOIN nation ON n_nationkey = c_nationkey
  GROUP BY n_name, c_name) ranked
WHERE rn <= 3
ORDER BY n_name, revenue DESC
