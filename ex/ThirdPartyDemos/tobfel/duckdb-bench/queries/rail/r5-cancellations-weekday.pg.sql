SELECT extract(dow FROM service_date) AS weekday, count(*) AS services,
  100.0 * count(*) FILTER (WHERE completely_cancelled) / count(*) AS cancelled
FROM services
GROUP BY 1
ORDER BY 1
