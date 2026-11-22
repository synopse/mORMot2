SELECT substr(o_orderdate, 1, 7) AS month,
  sum(o_totalprice) AS revenue,
  avg(sum(o_totalprice)) OVER (ORDER BY substr(o_orderdate, 1, 7)
    ROWS BETWEEN 2 PRECEDING AND CURRENT ROW) AS moving_avg
FROM orders
GROUP BY substr(o_orderdate, 1, 7)
ORDER BY month
