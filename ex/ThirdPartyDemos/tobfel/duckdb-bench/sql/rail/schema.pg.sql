-- rail tables for PostgreSQL, filled by DuckDB via ATTACH (TYPE postgres)
DROP TABLE IF EXISTS stops, services, stations;
CREATE TABLE stations(code VARCHAR(10) NOT NULL, name VARCHAR(50),
  country VARCHAR(2), type VARCHAR(30), lat DOUBLE PRECISION,
  lng DOUBLE PRECISION);
CREATE TABLE services(service_id BIGINT NOT NULL, service_date DATE NOT NULL,
  type VARCHAR(40), company VARCHAR(30), train_number INTEGER,
  completely_cancelled BOOLEAN, partly_cancelled BOOLEAN, max_delay INTEGER);
CREATE TABLE stops(stop_id BIGINT NOT NULL, service_id BIGINT NOT NULL,
  station_code VARCHAR(10) NOT NULL, arrival_time TIMESTAMP,
  arrival_delay INTEGER, arrival_cancelled BOOLEAN, departure_time TIMESTAMP,
  departure_delay INTEGER, departure_cancelled BOOLEAN,
  platform_change BOOLEAN);
