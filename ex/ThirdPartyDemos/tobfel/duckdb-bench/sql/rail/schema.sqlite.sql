-- rail tables for SQLite, filled by DuckDB via ATTACH (TYPE sqlite)
-- BOOLEAN is stored as 0/1, TIMESTAMP as ISO-8601 text
PRAGMA page_size = 8192;
PRAGMA journal_mode = WAL;
CREATE TABLE stations(code TEXT PRIMARY KEY, name TEXT, country TEXT,
  type TEXT, lat REAL, lng REAL);
CREATE TABLE services(service_id INTEGER PRIMARY KEY, service_date DATE NOT NULL,
  type TEXT, company TEXT, train_number INTEGER,
  completely_cancelled BOOLEAN, partly_cancelled BOOLEAN, max_delay INTEGER);
CREATE TABLE stops(stop_id INTEGER PRIMARY KEY, service_id INTEGER NOT NULL,
  station_code TEXT NOT NULL, arrival_time TIMESTAMP, arrival_delay INTEGER,
  arrival_cancelled BOOLEAN, departure_time TIMESTAMP, departure_delay INTEGER,
  departure_cancelled BOOLEAN, platform_change BOOLEAN);
