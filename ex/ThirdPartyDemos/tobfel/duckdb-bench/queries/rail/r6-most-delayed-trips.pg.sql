-- correlated sub-queries, each one using the stops_service index
SELECT service_id, service_date, train_number, max_delay,
  (SELECT station_code FROM stops s WHERE s.service_id = t.service_id
   ORDER BY coalesce(departure_time, arrival_time) LIMIT 1) AS origin,
  (SELECT station_code FROM stops s WHERE s.service_id = t.service_id
   ORDER BY coalesce(arrival_time, departure_time) DESC LIMIT 1) AS destination
FROM (
  SELECT service_id, service_date, train_number, max_delay
  FROM services
  ORDER BY max_delay DESC, service_id
  LIMIT 20) t
ORDER BY max_delay DESC, service_id
