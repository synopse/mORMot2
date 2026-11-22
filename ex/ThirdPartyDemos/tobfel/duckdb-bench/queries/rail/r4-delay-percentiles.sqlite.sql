-- no percentile aggregate in a default SQLite build: number the rows, then
-- interpolate between the two rows around each wanted position
WITH ranked AS (
  SELECT type, arrival_delay AS delay,
    row_number() OVER (PARTITION BY type ORDER BY arrival_delay) - 1 AS rn,
    count(*) OVER (PARTITION BY type) AS n
  FROM stops
  JOIN services USING (service_id)
  WHERE arrival_delay IS NOT NULL),
pos AS (
  SELECT type, delay, rn, n, (n - 1) * 0.5 AS p50, (n - 1) * 0.95 AS p95
  FROM ranked)
SELECT type, max(n) AS arrivals,
  sum(CASE WHEN rn = cast(p50 AS INTEGER) THEN delay * (1 - (p50 - rn))
           WHEN rn = cast(p50 AS INTEGER) + 1 THEN delay * (p50 - rn + 1)
      END) AS median_delay,
  sum(CASE WHEN rn = cast(p95 AS INTEGER) THEN delay * (1 - (p95 - rn))
           WHEN rn = cast(p95 AS INTEGER) + 1 THEN delay * (p95 - rn + 1)
      END) AS p95_delay
FROM pos
GROUP BY type
HAVING max(n) >= 1000
ORDER BY type
