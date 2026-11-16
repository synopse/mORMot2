SELECT type, count(*) AS arrivals,
  percentile_cont(0.5) WITHIN GROUP (ORDER BY arrival_delay) AS median_delay,
  percentile_cont(0.95) WITHIN GROUP (ORDER BY arrival_delay) AS p95_delay
FROM stops
JOIN services USING (service_id)
WHERE arrival_delay IS NOT NULL
GROUP BY type
HAVING count(*) >= 1000
ORDER BY type
