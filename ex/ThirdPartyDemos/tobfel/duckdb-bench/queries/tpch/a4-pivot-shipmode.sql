-- revenue per year (rows) and ship mode (columns)
-- DuckDB: PIVOT statement
PIVOT (SELECT year(l_shipdate) AS year, l_shipmode, l_extendedprice
       FROM lineitem)
ON l_shipmode IN ('AIR', 'FOB', 'MAIL', 'RAIL', 'REG AIR', 'SHIP', 'TRUCK')
USING sum(l_extendedprice)
GROUP BY year
ORDER BY year
