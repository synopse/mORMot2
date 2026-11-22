/// use DuckDB from any process bitness through a local Quack server
// - this program is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
// - DuckDB is only available as a 64-bit library: this demo launches the
// official 64-bit duckdb CLI executable via TExternalProcess, makes it serve
// the database with the Quack protocol, then uses it with the pure Pascal
// TSqlDBQuackConnectionProperties client - which works on Win32 and Delphi 7
// - no 64-bit Pascal proxy is needed, and other Quack clients (in any
// language) can connect to the same server
program duckdbproxy;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$apptype console}
{$endif OSWINDOWS}

uses
  {$I mormot.uses.inc}
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.crypt.core,
  mormot.net.sock,
  mormot.db.core,
  mormot.db.sql,
  mormot.db.raw.duckdb,
  mormot.db.sql.duckdb;

const
  /// the default duckdb CLI executable name
  DUCKDB_CLI = {$ifdef OSWINDOWS} 'duckdb.exe' {$else} 'duckdb' {$endif};
  /// stdin is used to send SQL to the CLI, and the CLI is killed with us
  QUACK_RUN = RUN_PROCESS + [roStdinPipe, roCloseChildren];

type
  /// run the duckdb CLI as a local Quack server
  // - the token is sent via stdin, so does not appear on the command line
  // - roCloseChildren ensures that the server process is killed if this
  // process ends without calling Stop
  TDuckDBQuackProcess = class
  protected
    fProcess: TExternalProcess;
    fPort: RawUtf8;
    fToken: RawUtf8;
  public
    /// launch the duckdb CLI on DatabaseFile ('' for an in-memory database),
    // and wait until it accepts connections on 127.0.0.1:Port
    // - raise ESqlDBQuack if the server could not start within TimeoutMS
    constructor Create(const DuckDBExe, DatabaseFile: TFileName;
      Port: integer; TimeoutMS: integer = 30000);
    /// gracefully stop the server - with a checkpoint of the database file
    destructor Destroy; override;
    /// create a new client connection to this server
    function NewClient: TSqlDBQuackConnectionProperties;
    /// the random token of this server
    property Token: RawUtf8
      read fToken;
  end;

constructor TDuckDBQuackProcess.Create(const DuckDBExe, DatabaseFile: TFileName;
  Port: integer; TimeoutMS: integer);
var
  key: TAesBlock;
  sock: TNetSocket;
  endtix: Int64;
  started: boolean;
begin
  Random128(@key); // cryptographic random token, for this server only
  fToken := BinToHexLower(@key, SizeOf(key));
  FillZero(key);
  fPort := Int32ToUtf8(Port);
  fProcess := TExternalProcess.Create;
  if DatabaseFile = '' then
    started := fProcess.Start([DuckDBExe], '', '', QUACK_RUN) // in-memory
  else
    started := fProcess.Start([DuckDBExe, DatabaseFile], '', '', QUACK_RUN);
  if not started then
    ESqlDBQuack.RaiseUtf8('%.Create: cannot execute %', [self, DuckDBExe]);
  // the CLI executes each statement read from stdin, and runs until stdin EOF
  fProcess.Write(FormatUtf8('CALL quack_serve(''quack:127.0.0.1:%'', ' +
    'token = ''%'');'#10, [fPort, fToken]));
  endtix := GetTickCount64 + TimeoutMS;
  repeat
    if NewSocket('127.0.0.1', fPort, nlTcp, {bind=}false, 200, 0, 0, 0,
         sock) = nrOk then
    begin
      sock.ShutdownAndClose(false);
      exit;
    end;
    if not fProcess.Running then
      ESqlDBQuack.RaiseUtf8('%.Create: % failed: %',
        [self, DuckDBExe, fProcess.ReadAvailable]);
    if GetTickCount64 > endtix then
      ESqlDBQuack.RaiseUtf8('%.Create: no Quack server on port % after %ms',
        [self, fPort, TimeoutMS]);
    SleepHiRes(100);
  until false;
end;

destructor TDuckDBQuackProcess.Destroy;
begin
  if fProcess <> nil then
  begin
    fProcess.CloseStdin; // stdin EOF: the CLI closes the database and exits
    if fProcess.WaitFor(10000) < 0 then
      fProcess.Terminate;
    fProcess.Free;
  end;
  inherited Destroy;
end;

function TDuckDBQuackProcess.NewClient: TSqlDBQuackConnectionProperties;
begin
  result := TSqlDBQuackConnectionProperties.Create(
    Join(['quack:127.0.0.1:', fPort]), '', 'duckdbproxy', fToken);
end;


procedure Demo(Props: TSqlDBConnectionProperties);
var
  conn: TSqlDBConnection;
  rows: ISqlDBRows;
begin
  conn := Props.ThreadSafeConnection;
  // the rows are generated on the server side
  Props.ExecuteNoResult('create or replace table sales as select ' +
    'i as id, ''Region '' || (i % 5) as region, ' +
    '(i * 1.25)::DECIMAL(19,4) as amount from range(1, ?) t(i)', [100001]);
  // a transaction is executed on the server connection
  conn.StartTransaction;
  try
    Props.ExecuteNoResult('update sales set amount = amount * ? where region = ?',
      [2, 'Region 0']);
    conn.Commit;
  except
    conn.Rollback;
    raise;
  end;
  // an analytical query with a bound parameter
  rows := Props.Execute('select region, count(*), sum(amount) from sales ' +
    'where amount > ? group by region order by region', [1000]);
  while rows.Step do
    ConsoleWrite('% % %', [rows.ColumnUtf8(0), rows.ColumnInt(1),
      rows.ColumnUtf8(2)]);
  rows := nil;
  ConsoleWrite('as JSON: %', [Props.Execute(
    'select version() as version, current_database() as db', []).FetchAllAsJson(true)]);
end;

procedure Run(const DuckDBExe, DatabaseFile: TFileName; Port: integer);
var
  server: TDuckDBQuackProcess;
  client: TSqlDBConnectionProperties;
begin
  server := TDuckDBQuackProcess.Create(DuckDBExe, DatabaseFile, Port);
  try
    ConsoleWrite('Quack server running on 127.0.0.1:% (pid %)',
      [Port, server.fProcess.Pid], ccLightGreen);
    client := server.NewClient;
    try
      Demo(client); // all interfaces are released when Demo() returns
    finally
      client.Free; // release the client before the server
    end;
  finally
    server.Free;
  end;
  ConsoleWrite('Quack server stopped', ccLightGreen);
end;

var
  cli, db: TFileName;
  port: integer;

begin
  with Executable.Command do
  begin
    ExeDescription := 'Use DuckDB via the duckdb CLI as local Quack server';
    cli := ParamS('cli', 'the 64-bit duckdb CLI #executable', DUCKDB_CLI);
    db := ParamS(['d', 'db'], 'the DuckDB database #file - default is in-memory');
    port := Param(['p', 'port'], 9494, 'the local TCP #port of the Quack server');
    if ConsoleHelpFailed('DuckDB via a local Quack server') then
      exit;
  end;
  try
    Run(cli, db, port);
  except
    on E: Exception do
    begin
      ConsoleShowFatalException(E, false);
      ExitCode := 1;
    end;
  end;
end.
