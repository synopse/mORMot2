SELECT extract(year FROM l_shipdate) AS year,
  sum(l_extendedprice) FILTER (WHERE l_shipmode = 'AIR') AS "AIR",
  sum(l_extendedprice) FILTER (WHERE l_shipmode = 'FOB') AS "FOB",
  sum(l_extendedprice) FILTER (WHERE l_shipmode = 'MAIL') AS "MAIL",
  sum(l_extendedprice) FILTER (WHERE l_shipmode = 'RAIL') AS "RAIL",
  sum(l_extendedprice) FILTER (WHERE l_shipmode = 'REG AIR') AS "REG AIR",
  sum(l_extendedprice) FILTER (WHERE l_shipmode = 'SHIP') AS "SHIP",
  sum(l_extendedprice) FILTER (WHERE l_shipmode = 'TRUCK') AS "TRUCK"
FROM lineitem
GROUP BY 1
ORDER BY 1
