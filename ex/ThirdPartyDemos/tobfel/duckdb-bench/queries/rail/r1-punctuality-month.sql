-- arrival punctuality per month and company: share of arrivals less than
-- 3 minutes late, the KPI published by the Dutch railways
-- DuckDB: GROUP BY ALL, count(*) FILTER
SELECT date_trunc('month', service_date) AS month, company,
  count(*) AS arrivals,
  avg(arrival_delay) AS avg_delay,
  100.0 * count(*) FILTER (WHERE arrival_delay < 3) / count(*) AS punctuality
FROM stops
JOIN services USING (service_id)
WHERE arrival_delay IS NOT NULL AND NOT arrival_cancelled
GROUP BY ALL
ORDER BY month, company
