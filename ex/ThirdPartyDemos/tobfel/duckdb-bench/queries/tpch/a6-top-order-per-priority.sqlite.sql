-- SQLite: a bare column next to max() comes from the row of the maximum
SELECT o_orderpriority, o_orderkey AS orderkey, max(o_totalprice) AS totalprice
FROM orders
GROUP BY o_orderpriority
ORDER BY o_orderpriority
