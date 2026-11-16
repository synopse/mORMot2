-- secondary indexes, created after the bulk load, then planner statistics
CREATE INDEX stops_service ON stops(service_id);
-- covers the per-station aggregates (r2, r8) and one station (r7)
CREATE INDEX stops_station ON stops(station_code, departure_delay,
  platform_change);
CREATE INDEX services_date ON services(service_date);
ANALYZE;
