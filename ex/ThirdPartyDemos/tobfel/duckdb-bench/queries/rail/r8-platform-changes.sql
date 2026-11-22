-- the 10 stations with the most platform changes (at least 10,000 stops)
-- portable SQL: the same text for all engines
SELECT code, name, stops, changed
FROM (
  SELECT station_code, count(*) AS stops,
    100.0 * count(*) FILTER (WHERE platform_change) / count(*) AS changed
  FROM stops
  GROUP BY station_code
  HAVING count(*) >= 10000) changes
JOIN stations ON code = station_code
ORDER BY changed DESC, code
LIMIT 10
