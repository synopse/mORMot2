SELECT o_orderpriority, count(*) AS orders,
  percentile_cont(0.5) WITHIN GROUP (ORDER BY o_totalprice) AS median_price,
  percentile_cont(0.9) WITHIN GROUP (ORDER BY o_totalprice) AS p90_price
FROM orders
GROUP BY o_orderpriority
ORDER BY o_orderpriority
