-- TPC-H Q12 for SQLite: CROSS JOIN forces the join order - scanning lineitem
-- first is twice faster than the plan chosen by the query planner
SELECT l_shipmode,
  sum(CASE WHEN o_orderpriority = '1-URGENT' OR o_orderpriority = '2-HIGH'
    THEN 1 ELSE 0 END) AS high_line_count,
  sum(CASE WHEN o_orderpriority <> '1-URGENT' AND o_orderpriority <> '2-HIGH'
    THEN 1 ELSE 0 END) AS low_line_count
FROM lineitem CROSS JOIN orders
WHERE o_orderkey = l_orderkey
  AND l_shipmode IN ('MAIL', 'SHIP')
  AND l_commitdate < l_receiptdate
  AND l_shipdate < l_commitdate
  AND l_receiptdate >= '1994-01-01'
  AND l_receiptdate < '1995-01-01'
GROUP BY l_shipmode
ORDER BY l_shipmode
