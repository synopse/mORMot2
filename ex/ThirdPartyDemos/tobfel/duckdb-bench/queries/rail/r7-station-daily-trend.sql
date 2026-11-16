-- daily departures at Utrecht Centraal with a 7-day moving average
-- portable SQL: the same text for all engines, selective on one station
SELECT service_date, count(*) AS departures,
  avg(count(*)) OVER (ORDER BY service_date
    ROWS BETWEEN 6 PRECEDING AND CURRENT ROW) AS moving_avg
FROM stops
JOIN services USING (service_id)
WHERE station_code = 'UT' AND departure_time IS NOT NULL
GROUP BY service_date
ORDER BY service_date
