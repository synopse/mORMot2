-- median and 90th percentile of the order price per priority
-- DuckDB: median() and quantile_cont() aggregates
SELECT o_orderpriority, count(*) AS orders,
  median(o_totalprice) AS median_price,
  quantile_cont(o_totalprice, 0.9) AS p90_price
FROM orders
GROUP BY ALL
ORDER BY o_orderpriority
