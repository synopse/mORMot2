SELECT cast(substr(o_orderdate, 1, 4) AS INTEGER) AS year, c_mktsegment,
  count(DISTINCT o_custkey) AS customers
FROM orders
JOIN customer ON c_custkey = o_custkey
GROUP BY 1, 2
ORDER BY 1, 2
