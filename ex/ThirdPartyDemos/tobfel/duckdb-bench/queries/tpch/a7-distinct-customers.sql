-- distinct active customers per year and market segment
SELECT year(o_orderdate) AS year, c_mktsegment,
  count(DISTINCT o_custkey) AS customers
FROM orders
JOIN customer ON c_custkey = o_custkey
GROUP BY ALL
ORDER BY year, c_mktsegment
