-- the 20 most delayed services, with their first and last station
-- DuckDB: arg_min() / arg_max() return the station of the first / last stop
WITH top AS (
  SELECT service_id, service_date, train_number, max_delay
  FROM services
  ORDER BY max_delay DESC, service_id
  LIMIT 20)
SELECT service_id, service_date, train_number, max_delay,
  arg_min(station_code, coalesce(departure_time, arrival_time)) AS origin,
  arg_max(station_code, coalesce(arrival_time, departure_time)) AS destination
FROM top
JOIN stops USING (service_id)
GROUP BY ALL
ORDER BY max_delay DESC, service_id
