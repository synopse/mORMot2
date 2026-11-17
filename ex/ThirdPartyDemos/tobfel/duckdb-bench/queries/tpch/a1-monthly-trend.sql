-- monthly revenue with a 3-month moving average
-- DuckDB: GROUP BY and the window refer to the "month" alias
SELECT date_trunc('month', o_orderdate) AS month,
  sum(o_totalprice) AS revenue,
  avg(sum(o_totalprice)) OVER (ORDER BY month
    ROWS BETWEEN 2 PRECEDING AND CURRENT ROW) AS moving_avg
FROM orders
GROUP BY month
ORDER BY month
