-- share of completely cancelled services per day of the week (0 = Sunday)
-- DuckDB: dayofweek()
SELECT dayofweek(service_date) AS weekday, count(*) AS services,
  100.0 * count(*) FILTER (WHERE completely_cancelled) / count(*) AS cancelled
FROM services
GROUP BY ALL
ORDER BY weekday
