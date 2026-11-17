-- DuckDB needs no index for analytics: min/max zonemaps of each row group
-- skip the data; an ART index is only worth it for point lookups (p1)
ALTER TABLE orders ADD PRIMARY KEY (o_orderkey);
