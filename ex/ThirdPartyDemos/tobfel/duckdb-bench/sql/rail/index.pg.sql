-- primary keys and secondary indexes, created after the bulk load
ALTER TABLE stations ADD PRIMARY KEY (code);
ALTER TABLE services ADD PRIMARY KEY (service_id);
ALTER TABLE stops ADD PRIMARY KEY (stop_id);
CREATE INDEX stops_service ON stops(service_id);
-- covers the per-station aggregates (r2, r8) and one station (r7)
CREATE INDEX stops_station ON stops(station_code, departure_delay,
  platform_change);
CREATE INDEX services_date ON services(service_date);
-- visibility map (index-only scans) and planner statistics
VACUUM ANALYZE;
