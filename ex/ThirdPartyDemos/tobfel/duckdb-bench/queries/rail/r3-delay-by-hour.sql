-- average departure delay and share of departures 5+ minutes late,
-- per hour of the day and train type
-- DuckDB: hour() and GROUP BY ALL
SELECT hour(departure_time) AS hour, type,
  count(*) AS departures,
  avg(departure_delay) AS avg_delay,
  100.0 * count(*) FILTER (WHERE departure_delay >= 5) / count(*) AS late
FROM stops
JOIN services USING (service_id)
WHERE departure_time IS NOT NULL AND NOT departure_cancelled
GROUP BY ALL
HAVING count(*) >= 1000
ORDER BY hour, type
