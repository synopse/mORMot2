-- count(DISTINCT) sorts all the rows in PostgreSQL: a DISTINCT sub-query
-- allows a parallel hash aggregate, and is twice faster
SELECT year, c_mktsegment, count(*) AS customers
FROM (
  SELECT DISTINCT extract(year FROM o_orderdate) AS year, c_mktsegment,
    o_custkey
  FROM orders
  JOIN customer ON c_custkey = o_custkey) active
GROUP BY 1, 2
ORDER BY 1, 2
