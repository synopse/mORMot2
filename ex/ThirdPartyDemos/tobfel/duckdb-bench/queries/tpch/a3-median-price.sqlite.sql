-- no percentile aggregate in a default SQLite build: number the rows, then
-- interpolate between the two rows around each wanted position
WITH ranked AS (
  SELECT o_orderpriority AS prio, o_totalprice AS price,
    row_number() OVER (PARTITION BY o_orderpriority
      ORDER BY o_totalprice) - 1 AS rn,
    count(*) OVER (PARTITION BY o_orderpriority) AS n
  FROM orders),
pos AS (
  SELECT prio, price, rn, n,
    (n - 1) * 0.5 AS p50, (n - 1) * 0.9 AS p90
  FROM ranked)
SELECT prio AS o_orderpriority, max(n) AS orders,
  sum(CASE WHEN rn = cast(p50 AS INTEGER) THEN price * (1 - (p50 - rn))
           WHEN rn = cast(p50 AS INTEGER) + 1 THEN price * (p50 - rn + 1)
      END) AS median_price,
  sum(CASE WHEN rn = cast(p90 AS INTEGER) THEN price * (1 - (p90 - rn))
           WHEN rn = cast(p90 AS INTEGER) + 1 THEN price * (p90 - rn + 1)
      END) AS p90_price
FROM pos
GROUP BY prio
ORDER BY prio
