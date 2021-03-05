unit server;

interface

{$I mormot.defines.inc}
uses
  mormot.core.base,
  mormot.orm.core,
  mormot.rest.sqlite3,
  mormot.db.raw.sqlite3.static;

type
  TSampleServer = class(TRestServerDB)
  end;

implementation


end.
