-- Dutch railways open data of https://www.rijdendetreinen.nl (CC BY 4.0)
-- one CSV row per stop of a train service: normalized into the services
-- and stops tables, as an application would store them
-- times are converted from TIMESTAMPTZ to the local Amsterdam time
CREATE TABLE stations AS
  SELECT code, name_long AS name, country, type, geo_lat AS lat, geo_lng AS lng
  FROM read_csv('$URLstations/stations-2023-09.csv')
  ORDER BY code;
CREATE TEMP TABLE raw AS
  FROM read_csv($FILES, union_by_name = true);
CREATE TABLE services AS
  SELECT "Service:RDT-ID" AS service_id,
    any_value("Service:Date") AS service_date,
    any_value("Service:Type") AS type,
    any_value("Service:Company") AS company,
    any_value("Service:Train number") AS train_number,
    any_value("Service:Completely cancelled") AS completely_cancelled,
    any_value("Service:Partly cancelled") AS partly_cancelled,
    any_value("Service:Maximum delay") AS max_delay
  FROM raw
  GROUP BY "Service:RDT-ID"
  ORDER BY service_id;
CREATE TABLE stops AS
  SELECT "Stop:RDT-ID" AS stop_id, "Service:RDT-ID" AS service_id,
    "Stop:Station code" AS station_code,
    "Stop:Arrival time" AT TIME ZONE 'Europe/Amsterdam' AS arrival_time,
    "Stop:Arrival delay" AS arrival_delay,
    "Stop:Arrival cancelled" AS arrival_cancelled,
    "Stop:Departure time" AT TIME ZONE 'Europe/Amsterdam' AS departure_time,
    "Stop:Departure delay" AS departure_delay,
    "Stop:Departure cancelled" AS departure_cancelled,
    "Stop:Platform change" AS platform_change
  FROM raw
  ORDER BY stop_id;
DROP TABLE raw;
