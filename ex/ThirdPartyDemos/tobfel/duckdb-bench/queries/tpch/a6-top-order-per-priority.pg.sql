-- DISTINCT ON (o_orderpriority) ... ORDER BY o_orderpriority, o_totalprice DESC
-- is shorter, but sorts all the orders: 18 times slower than this join
WITH best AS (
  SELECT o_orderpriority AS prio, max(o_totalprice) AS totalprice
  FROM orders
  GROUP BY o_orderpriority)
SELECT prio AS o_orderpriority, o_orderkey AS orderkey, totalprice
FROM best
JOIN orders ON o_orderpriority = prio AND o_totalprice = totalprice
ORDER BY 1
