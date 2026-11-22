program proxysrv2;
{$I mormot.defines.inc}
uses
  {$I mormot.uses.inc}
  sysutils, mormot.core.base, mormot.core.os, mormot.db.sql, mormot.db.proxy,
  mormot.db.raw.duckdb, mormot.db.sql.duckdb;
var
  p: TSqlDBDuckDBConnectionProperties;
  s: TSqlDBServerSockets;
begin
  p := TSqlDBDuckDBConnectionProperties.Create('./rail_catalog.duckdb', '', '', '');
  p.ReadOnly := true;
  s := TSqlDBServerSockets.Create(p, 'rail', '8898', 'user', 'pass');
  sleep(3600000);
  s.Free;
end.
