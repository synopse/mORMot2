-- the 10 stations with the highest average departure delay
-- (at least 10,000 departures), with the station name
-- portable SQL: the same text for all engines - aggregating before the join
-- allows SQLite3 to use the covering stops_station index
SELECT code, name, departures, avg_delay
FROM (
  SELECT station_code, count(*) AS departures,
    avg(departure_delay) AS avg_delay
  FROM stops
  WHERE departure_delay IS NOT NULL
  GROUP BY station_code
  HAVING count(*) >= 10000) delays
JOIN stations ON code = station_code
ORDER BY avg_delay DESC
LIMIT 10
