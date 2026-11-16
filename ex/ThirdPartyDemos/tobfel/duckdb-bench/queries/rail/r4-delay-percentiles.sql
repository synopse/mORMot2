-- median and 95th percentile of the arrival delay per train type
-- DuckDB: median() and quantile_cont()
SELECT type, count(*) AS arrivals,
  median(arrival_delay) AS median_delay,
  quantile_cont(arrival_delay, 0.95) AS p95_delay
FROM stops
JOIN services USING (service_id)
WHERE arrival_delay IS NOT NULL
GROUP BY ALL
HAVING count(*) >= 1000
ORDER BY type
