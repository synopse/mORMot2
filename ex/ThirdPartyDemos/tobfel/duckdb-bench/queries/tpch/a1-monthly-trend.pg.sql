SELECT date_trunc('month', o_orderdate) AS month,
  sum(o_totalprice) AS revenue,
  avg(sum(o_totalprice)) OVER (ORDER BY date_trunc('month', o_orderdate)
    ROWS BETWEEN 2 PRECEDING AND CURRENT ROW) AS moving_avg
FROM orders
GROUP BY date_trunc('month', o_orderdate)
ORDER BY month
