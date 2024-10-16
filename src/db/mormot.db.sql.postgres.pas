/// Database Framework Direct PostgreSQL Connnection via libpq
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.sql.postgres;

{
  *****************************************************************************

   Direct PostgreSQL Client Access using the libpq Library
    - TSqlDBPostgresConnection* and TSqlDBPostgreStatement Classes
    - TSqlDBPostgresAsync Asynchronous Execution via Pipelines

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  variants,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.datetime,
  mormot.core.data,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.log,
  mormot.core.threads,
  mormot.net.sock,
  mormot.db.core,
  mormot.db.sql;


{ ************ TSqlDBPostgreConnection* and TSqlDBPostgreStatement Classes }

type
  TSqlDBPostgresAsync = class;

  /// connection properties which will implement an internal Thread-Safe
  // connection pool for PostgreSQL using the official libpq API
  TSqlDBPostgresConnectionProperties = class(TSqlDBConnectionPropertiesThreadSafe)
  protected
    fOids: TWordDynArray; // O(n) search in L1 cache - use SSE2 on FPC x86_64
    fOidsFieldTypes: TSqlDBFieldTypeDynArray;
    fOidsCount: integer;
    fArrayParamsAsBinary: boolean;
    procedure GetForeignKeys; override;
    /// fill mapping of standard OID
    // - at runtime mapping can be defined using Oid2FieldType() method
    // - OIDs defined in DB can be retrieved using query
    //  "select oid, typname from pg_type where typtype = 'b' order by oid"
    procedure FillOidMapping; virtual;
  public
    /// initialize the properties
    // - raise an exception in case libpg is not thead-safe
    // - aDatabaseName can be a Connection URI - see
    // https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING
    // - if aDatabaseName contains a connection URI with password, we recommend
    // to repeat the password in aPassword parameter to prevent logging it
    // (see TSqlDBConnectionProperties.DatabaseNameSafe)
    // - it may be better to use environment variables and postgres config file
    // for connection parameters
    constructor Create(
      const aServerName, aDatabaseName, aUserID, aPassword: RawUtf8); override;
    /// create a new connection
    // - caller is responsible of freeing this instance
    // - this overridden method will create an TSqlDBPostgresConnection instance
    function NewConnection: TSqlDBConnection; override;
    /// add or replace mapping of OID into TSqlDBFieldType
    // - in case mapping for OID is not defined, returns ftUtf8
    function Oid2FieldType(cOID: cardinal): TSqlDBFieldType;
      {$ifdef HASINLINE}inline;{$endif}
    /// add new (or override existed) OID to FieldType mapping
    procedure MapOid(cOid: cardinal; fieldType: TSqlDBFieldType);
    /// get the asynchronous/pipelined engine associated with the current thread
    // - to be used as Async.Prepare/PrepareLocked factory
    function Async: TSqlDBPostgresAsync;
    /// by default, array parameters will be sent as TEXT
    // - set this property to true so that binary is sent over the wire for
    // INT4ARRAYOID/INT8ARRAYOID parameters
    property ArrayParamsAsBinary: boolean
      read fArrayParamsAsBinary write fArrayParamsAsBinary;
  end;

  /// implements a connection via the libpq access layer
  // - is accessible from TSqlDBPostgresConnectionProperties
  // - some additional PostgreSQL-specific pipelining methods are included
  TSqlDBPostgresConnection = class(TSqlDBConnectionThreadSafe)
  protected
    // SQL of server-side prepared statements - name is index as hexadecimal
    // - statements are already cached in TSqlDBConnection.NewStatementPrepared
    fPrepared: TRawUtf8List;
    fPGConn: pointer; // the associated low-level provider connection
    fAsync: TSqlDBPostgresAsync;
    // return statement index in fPrepared cache array
    function PrepareCached(const aSql: RawUtf8; aParamCount: integer;
      out aName: RawUtf8): integer;
    /// direct execution of SQL statement what do not returns a result
    // - statement should not contains parameters
    // - raise an ESqlDBPostgres on error
    procedure DirectExecSql(const SQL: RawUtf8); overload;
    /// direct execution of SQL statement what do not returns a result
    // - overloaded method to return a single value e.g. from a SELECT
    procedure DirectExecSql(const SQL: RawUtf8; out Value: RawUtf8); overload;
    /// query the pg_settings table for a given setting
    function GetServerSetting(const Name: RawUtf8): RawUtf8;
  public
    /// finalize this connection
    destructor Destroy; override;
    /// connect to the specified server
    // - should raise an ESqlDBPostgres on error
    procedure Connect; override;
    /// stop connection to the specified PostgreSQL database server
    // - should raise an ESqlDBPostgres on error
    procedure Disconnect; override;
    /// return TRUE if Connect has been already successfully called
    function IsConnected: boolean; override;
    /// create a new statement instance
    function NewStatement: TSqlDBStatement; override;
    /// begin a Transaction for this connection
    procedure StartTransaction; override;
    /// commit changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Commit; override;
    /// discard changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Rollback; override;
    /// enter Pipelining mode
    // - *Warning* - connection is in blocking mode, see notes about possible deadlock
    // https://www.postgresql.org/docs/current/libpq-pipeline-mode.html#LIBPQ-PIPELINE-USING
    procedure EnterPipelineMode;
    /// exit Pipelining mode, raising an ESqlDBPostgres on error
    procedure ExitPipelineMode;
    /// exit Pipelining mode with no ESqlDBPostgres on error, but returning false
    // - allow to retry later if needed
    function TryExitPipelineMode: boolean;
    /// marks a synchronization point in a pipeline by sending a sync message
    // and flushing the send buffer
    procedure PipelineSync;
    /// flush any queued output data to the server
    procedure Flush;
    /// sends a request for the server to flush its output buffer
    procedure SendFlushRequest;
    /// return current pipeline status
    function PipelineStatus: integer;
    /// read PipelineSync result and check it's OK
    procedure CheckPipelineSync;
    /// direct access to the associated PPGconn connection
    property Direct: pointer
      read fPGConn;
    /// how many prepared statements are currently cached for this connection
    function PreparedCount: integer;
    /// access to the raw socket of this connection
    // - warning: over TLS, the socket state may not match the actual data state
    function Socket: TNetSocket;
    /// check if there is some pending input at the raw socket of this connection
    // - warning: over TLS, the socket state may not match the actual data state
    function SocketHasData: boolean;
  end;

  /// implements a statement via a Postgres database connection
  TSqlDBPostgresStatement = class(TSqlDBStatementWithParamsAndColumns)
  protected
    fPreparedStmtName: RawUtf8; // = hexadecimal of the SQL cached index
    fRes: pointer;
    fResStatus: integer;
    fPreparedParamsCount: integer;
    // pointers to query parameters: allocated in Prepare, filled in BindParams
    fPGParams: TPointerDynArray;
    // PGFMT_TEXT or PGFMT_BIN: allocated in Prepare, filled in BindParams
    fPGParamFormats: TIntegerDynArray;
    // non zero for PGFMT_BIN params
    fPGParamLengths: TIntegerDynArray;
    /// define the result columns name and content
    procedure BindColumns;
    /// set parameters as expected by PostgresSQL
    procedure BindParams;
    /// raise an exception if Col is out of range according to fColumnCount
    // or rowset is not initialized
    procedure CheckColAndRowset(Col: integer);
      {$ifdef HASINLINE} inline; {$endif}
  public
    /// finalize the statement for a given connection
    destructor Destroy; override;
    /// prepare an UTF-8 encoded SQL statement
    // - parameters marked as ? will be bound later, before ExecutePrepared call
    // - if ExpectResults is TRUE, then Step() and Column*() methods are available
    // to retrieve the data rows
    // - raise an ESqlDBPostgres on any error
    procedure Prepare(const aSql: RawUtf8; ExpectResults: boolean = False); overload; override;
    /// execute a prepared SQL statement
    // - parameters marked as ? should have been already bound with Bind*() functions
    // - this implementation will also handle bound array of values (if any)
    // - this overridden method will log the SQL statement if sllSQL has been
    // enabled in SynDBLog.Family.Level
    // - raise an ESqlDBPostgres on any error
    procedure ExecutePrepared; override;
    /// execute a prepared SQL statement, for connection in pipelining mode
    // - sends a request to execute a prepared statement with given parameters, 
    // without waiting for the result(s)
    // - after all statements are sent, conn.SendFlushRequest should be called,
    // then GetPipelineResult is able to read results in order they were sent
    procedure SendPipelinePrepared;
    /// retrieve next result for pipelined statement
    procedure GetPipelineResult;
    /// bind an array of 64-bit integer values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - overriden for direct assignment to the PostgreSQL client as fake JSON
    procedure BindArray(Param: integer;
      const Values: array of Int64); overload; override;
    /// bind an array of 32-bit integer values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - for direct assignment to the PostgreSQL client as fake JSON
    procedure BindArrayInt32(Param: integer; const Values: TIntegerDynArray);
    /// bind an array of JSON values to a parameter
    // - overriden for direct assignment to the PostgreSQL client
    // - warning: input JSON should already be in the expected format (ftDate)
    procedure BindArrayJson(Param: integer; ParamType: TSqlDBFieldType;
      var JsonArray: RawUtf8; ValuesCount: integer); override;
    /// gets a number of updates made by latest executed statement
    function UpdateCount: integer; override;
    /// reset the previous prepared statement
    // - this overridden implementation will reset all bindings and the cursor state
    // - raise an ESqlDBPostgres on any error
    procedure Reset; override;

    /// access the next or first row of data from the SQL Statement result
    // - return true on success, with data ready to be retrieved by Column*() methods
    // - return false if no more row is available (e.g. if the SQL statement
    // is not a SELECT but an UPDATE or INSERT command)
    // - if SeekFirst is TRUE, will put the cursor on the first row of results
    // - raise an ESqlDBPostgres on any error
    function Step(SeekFirst: boolean = False): boolean; override;
    /// clear(fRes) when ISqlDBStatement is back in cache
    procedure ReleaseRows; override;
    /// return a Column integer value of the current Row, first Col is 0
    function ColumnInt(Col: integer): int64; override;
    /// returns TRUE if the column contains NULL
    function ColumnNull(Col: integer): boolean; override;
    /// return a Column floating point value of the current Row, first Col is 0
    function ColumnDouble(Col: integer): double; override;
    /// return a Column date and time value of the current Row, first Col is 0
    function ColumnDateTime(Col: integer): TDateTime; override;
    /// return a Column currency value of the current Row, first Col is 0
    function ColumnCurrency(Col: integer): currency; override;
    /// return a Column UTF-8 encoded text value of the current Row, first Col is 0
    function ColumnUtf8(Col: integer): RawUtf8; override;
    /// return a Column UTF-8 text buffer of the current Row, first Col is 0
    // - returned pointer is likely to last only until next Reset call
    function ColumnPUtf8(Col: integer): PUtf8Char; override;
    /// return a Column as a blob value of the current Row, first Col is 0
    function ColumnBlob(Col: integer): RawByteString; override;
    /// return one column value into JSON content
    procedure ColumnToJson(Col: integer; W: TJsonWriter); override;
    /// how many parameters founded during prepare stage
    property PreparedParamsCount: integer
      read fPreparedParamsCount;
  end;


{ ************ TSqlDBPostgresAsync Asynchronous Execution via Pipelines }

  TSqlDBPostgresAsyncStatement = class;

  ESqlDBPostgresAsync = class(ESynException);

  /// tune TSqlDBPostgresConnectionProperties.NewAsyncStatementPrepared()
  // - asoForcePipelineSync will call PipelineSync for each ExecuteAsync
  // - asoForceConnectionFlush will call Connection.Flush (for modified libpq)
  TSqlDBPostgresAsyncStatementOptions = set of (
    asoForcePipelineSync,
    asoForceConnectionFlush);
  PSqlDBPostgresAsyncStatementOptions = ^TSqlDBPostgresAsyncStatementOptions;

  /// event signature for TSqlDBPostgresAsyncStatement.ExecuteAsync() callback
  // - implementation should retrieve the data from Statement.Column*(), then
  // process it using the opaque Context
  // - is called with Statement = nil on any DB fatal error
  TOnSqlDBPostgresAsyncEvent = procedure(
    Statement: TSqlDBPostgresAsyncStatement; Context: TObject) of object;

  TSqlDBPostgresAsyncTask = record
    Statement: TSqlDBPostgresAsyncStatement;
    Context: TObject;
    OnFinished: TOnSqlDBPostgresAsyncEvent;
    Options: TSqlDBPostgresAsyncStatementOptions;
  end;
  TSqlDBPostgresAsyncTasks = array of TSqlDBPostgresAsyncTask;

  /// one asynchronous SQL statement as owned by TSqlDBPostgresAsync
  // - this is the main entry point for pipelined process on PostgreSQL
  // - within PrepareLocked/Lock and Unlock, should call Bind() then ExecuteAsync()
  TSqlDBPostgresAsyncStatement = class(TSqlDBPostgresStatement)
  protected
    fOwner: TSqlDBPostgresAsync;
    fAsyncOptions: TSqlDBPostgresAsyncStatementOptions;
  public
    /// ExecutePrepared-like method for asynchronous process
    // - to be called within PrepareLocked/Lock and UnLock, after Bind():
    // - typical usecase is e.g. from ex/techempower-bench/raw.pas
    // ! function TRawAsyncServer.asyncdb(ctxt: THttpServerRequest): cardinal;
    // ! begin
    // !   with fDbPool.Async.PrepareLocked(WORLD_READ_SQL) do
    // !   try
    // !     Bind(1, ComputeRandomWorld);
    // !     ExecuteAsync(ctxt, OnAsyncDb);
    // !   finally
    // !     UnLock;
    // !   end;
    // !   result := ctxt.SetAsyncResponse;
    // ! end;
    procedure ExecuteAsync(Context: TObject;
      const OnFinished: TOnSqlDBPostgresAsyncEvent;
      ForcedOptions: PSqlDBPostgresAsyncStatementOptions = nil);
    /// ExecutePrepared-like method for asynchronous process
    // - just wrap ExecuteAsync + UnLock
    procedure ExecuteAsyncNoParam(Context: TObject;
      const OnFinished: TOnSqlDBPostgresAsyncEvent;
      ForcedOptions: PSqlDBPostgresAsyncStatementOptions = nil);
    /// could be used as a short-cut to Owner.Safe.Lock
    procedure Lock;
      {$ifdef HASINLINE}inline;{$endif}
    /// could be used as a short-cut to Owner.Safe.UnLock
    procedure Unlock;
      {$ifdef HASINLINE}inline;{$endif}
    /// the TSqlDBPostgresAsync instance owner
    property Owner: TSqlDBPostgresAsync
      read fOwner;
    /// how this statement should behave in asynchronous mode
    property AsyncOptions: TSqlDBPostgresAsyncStatementOptions
      read fAsyncOptions;
  end;

  /// background thread in which all TSqlDBPostgresAsync results are processed
  TSqlDBPostgresAsyncThread = class(TSynThread)
  protected
    fOwner: TSqlDBPostgresAsync;
    fName: RawUtf8;
    fOnThreadStart: TOnNotifyThread;
    fProcessing: boolean;
    procedure Execute; override;
  public
    /// initialize the thread
    constructor Create(aOwner: TSqlDBPostgresAsync); reintroduce;
    /// some event which will be called then nil in the main Execute loop
    property OnThreadStart: TOnNotifyThread
      read fOnThreadStart write fOnThreadStart;
  end;

  /// asynchronous execution engine
  // - allow to execute several statements within an PostgreSQL pipeline, and
  // return the results using asynchronous callbacks from a background thread
  // - inherits from TSynLocked so you can use Lock/UnLock
  TSqlDBPostgresAsync = class(TSynLocked)
  protected
    fConnection: TSqlDBPostgresConnection;
    fStatements: array of TSqlDBPostgresAsyncStatement;
    fThread: TSqlDBPostgresAsyncThread;
    fTasks: TSynQueue;
    procedure DoExecuteAsyncError;
  public
    /// initialize the execution engine
    constructor Create(Owner: TSqlDBPostgresConnection); reintroduce;
    /// finalize the execution engine
    destructor Destroy; override;
    /// return a TSqlDBPostgresAsyncStatement instance tied to this engine
    // - the returned instance will be cached and owned by this TSqlDBPostgresAsync
    function Prepare(const Sql: RawUtf8; ExpectResults: boolean = true;
      Options: TSqlDBPostgresAsyncStatementOptions = []): TSqlDBPostgresAsyncStatement;
    /// lock and return a TSqlDBPostgresAsyncStatement instance tied to this engine
    // - the returned instance will be cached and owned by this TSqlDBPostgresAsync
    function PrepareLocked(const Sql: RawUtf8; ExpectResults: boolean = true;
      Options: TSqlDBPostgresAsyncStatementOptions = []): TSqlDBPostgresAsyncStatement;
    /// the TSqlDBPostgresConnection in pipelined mode owned by this engine
    // - a single dedicated connection will be used for all async statements
    property Connection: TSqlDBPostgresConnection
      read fConnection;
    /// raw level to the background thread processing the results
    property Thread: TSqlDBPostgresAsyncThread
      read fThread;
  end;


implementation

uses
  mormot.db.raw.postgres; // raw libpq library API access


{ ************ TSqlDBPostgreConnection* and TSqlDBPostgreStatement Classes }

{ TSqlDBPostgresConnection }

destructor TSqlDBPostgresConnection.Destroy;
begin
  FreeAndNil(fAsync);
  inherited Destroy;
  fPrepared.Free;
end;

function TSqlDBPostgresConnection.PrepareCached(
  const aSql: RawUtf8; aParamCount: integer; out aName: RawUtf8): integer;
begin
  if fPrepared = nil then
  begin
    fPrepared := TRawUtf8List.CreateEx([fCaseSensitive, fNoDuplicate]);
    result := -1;
  end
  else
  begin
    result := fPrepared.IndexOf(aSql);
    if result >= 0 then
    begin
      // never called in practice: already cached in TSqlDBConnection
      aName := Int64ToHexLower(result); // statement name is index as hexa
      exit; // already prepared -> we will just give the statement name to PQ
    end;
  end;
  result := fPrepared.Add(aSql);
  aName := Int64ToHexLower(result);
  PQ.Check(fPGConn, 'Prepare',
    PQ.Prepare(fPGConn, pointer(aName), pointer(aSql), aParamCount, nil));
end;

procedure TSqlDBPostgresConnection.DirectExecSql(const SQL: RawUtf8);
begin
  PQ.Check(fPGConn, 'Exec',
    PQ.Exec(fPGConn, pointer(SQL)));
end;

procedure TSqlDBPostgresConnection.DirectExecSql(
  const SQL: RawUtf8; out Value: RawUtf8);
var
  res: PPGresult;
begin
  res := PQ.Exec(fPGConn, pointer(SQL));
  PQ.Check(fPGConn, 'Exec', res, nil, {andclear=}false);
  PQ.GetRawUtf8(res, 0, 0, Value);
  PQ.Clear(res);
end;

function TSqlDBPostgresConnection.GetServerSetting(const Name: RawUtf8): RawUtf8;
var
  sql: RawUtf8;
begin
  FormatUtf8('select setting from pg_settings where name=''%''', [Name], sql);
  DirectExecSql(sql, result);
end;

// our conversion is faster than PQUnescapeByteA - which requires libpq 8.3+
//  and calls malloc()
// https://github.com/postgres/postgres/blob/master/src/interfaces/libpq/fe-exec.c

// checking \x for hexadecimal encoding is what UnescapeByteA() does
// -> no need to ask server settings
// note: bytea_output is HEX by default (at least since PostgreSQL 9.0)

function BlobInPlaceDecode(P: PAnsiChar; PLen: integer): integer;
begin
  if (P = nil) or
     (PLen <= 0) then
    result := 0
  else if PWord(P)^ = ord('\') + ord('x') shl 8 then {ssByteAasHex in fServerSettings}
  begin
    result := (PLen - 2) shr 1; // skip trailing \x and compute number of bytes
    if result > 0 then
      HexToBinFast(P + 2, PByte(P), result); // in-place conversion
  end
  else
    // oldest PostgreSQL versions may stil use octal encoding (unlikely)
    result := OctToBin(P, pointer(P)); // in-place conversion
end;

procedure SynLogNoticeProcessor({%H-}arg: pointer; message: PUtf8Char); cdecl;
begin
  SynDBLog.Add.Log(sllTrace, 'PGINFO: %', [message], TObject(arg));
end;

procedure DummyNoticeProcessor({%H-}arg: pointer; message: PUtf8Char); cdecl;
begin
end;

procedure TSqlDBPostgresConnection.Connect;
var
  log: ISynLog;
  host, port: RawUtf8;
begin
  log := SynDBLog.Enter(self, 'Connect');
  Disconnect; // force fTrans=fError=fServer=fContext=nil
  try
    Split(Properties.ServerName, ':', host, port);
    fPGConn := PQ.SetDBLogin(pointer(host), pointer(port), nil, nil,
      pointer(Properties.DatabaseName), pointer(Properties.UserID),
      pointer(Properties.PassWord));
    if PQ.Status(fPGConn) = CONNECTION_BAD then
      ESqlDBPostgres.RaiseUtf8('Connection to database % failed [%]',
        [Properties.DatabaseNameSafe, PQ.ErrorMessage(fPGConn)]);
    // if GetServerSetting('bytea_output') = 'HEX' then
    //   include(fServerSettings, ssByteAasHex);
    if log <> nil then
    begin
      PQ.SetNoticeProcessor(fPGConn, SynLogNoticeProcessor, pointer(self));
      log.Log(sllDB, 'Connected to % % using % v%', [fProperties.ServerName,
        fProperties.DatabaseNameSafe, PQ.LibraryPath, PQ.LibVersion], self);
    end
    else
      // to ensure no performance drop due to notice to console
      PQ.SetNoticeProcessor(fPGConn, DummyNoticeProcessor, nil);
    inherited Connect; // notify any re-connection
  except
    on E: Exception do
    begin
      if log <> nil then
        log.Log(sllError, 'Connect: % on %',
          [E, Properties.DatabaseNameSafe], self);
      Disconnect; // clean up on fail
      raise;
    end;
  end;
end;

procedure TSqlDBPostgresConnection.Disconnect;
begin
  try
    inherited Disconnect;
  finally
    // any prepared statements will be released with this connection
    if fPrepared <> nil then
      fPrepared.Clear;
    // let PG driver finish the connection
    if fPGConn <> nil then
    begin
      PQ.Finish(fPGConn);
      fPGConn := nil;
    end;
  end;
end;

function TSqlDBPostgresConnection.IsConnected: boolean;
begin
  result := (fPGConn <> nil);
end;

function TSqlDBPostgresConnection.NewStatement: TSqlDBStatement;
begin
  result := TSqlDBPostgresStatement.Create(self);
end;

procedure TSqlDBPostgresConnection.StartTransaction;
var
  log: ISynLog;
begin
  log := SynDBLog.Enter(self, 'StartTransaction');
  if TransactionCount > 0 then
    ESqlDBPostgres.RaiseUtf8('Invalid %.StartTransaction: nested transactions' +
      ' are not supported by Postgres - use SAVEPOINT instead', [self]);
  try
    inherited StartTransaction;
    DirectExecSql('START TRANSACTION');
  except
    on E: Exception do
    begin
      if log <> nil then
        log.Log(sllError, 'StartTransaction: % on %',
          [E, Properties.DatabaseNameSafe], self);
      if fTransactionCount > 0 then
        Dec(fTransactionCount);
      raise;
    end;
  end;
end;

procedure TSqlDBPostgresConnection.Commit;
begin
  inherited Commit;
  try
    DirectExecSql('COMMIT');
  except
    inc(fTransactionCount); // the transaction is still active
    raise;
  end;
end;

procedure TSqlDBPostgresConnection.Rollback;
begin
  inherited;
  DirectExecSql('ROLLBACK');
end;

procedure TSqlDBPostgresConnection.EnterPipelineMode;
begin
  if not Assigned(PQ.enterPipelineMode) then
    ESqlDBPostgres.RaiseUtf8('%.EnterPipelineMonde: pipelining unsupported in % v%',
      [self, PQ.LibraryPath, PQ.LibVersion]);
  if PQ.enterPipelineMode(fPGConn) <> PGRES_COMMAND_OK then
    PQ.RaiseError(fPGConn, 'EnterPipelineMonde');
end;

procedure TSqlDBPostgresConnection.ExitPipelineMode;
begin
  if PQ.exitPipelineMode(fPGConn) <> PGRES_COMMAND_OK then
    PQ.RaiseError(fPGConn, 'ExitPipelineMode');
  if PQ.pipelineStatus(fPGConn) <> PQ_PIPELINE_OFF then
    PQ.RaiseError(fPGConn, 'ExitPipelineMode status');
end;

function TSqlDBPostgresConnection.TryExitPipelineMode: boolean;
begin
  result := (PQ.exitPipelineMode(fPGConn) = PGRES_COMMAND_OK) and
            (PQ.pipelineStatus(fPGConn) = PQ_PIPELINE_OFF);
end;

procedure TSqlDBPostgresConnection.PipelineSync;
begin
  if PQ.pipelineSync(fPGConn) <> PGRES_COMMAND_OK then
    PQ.RaiseError(fPGConn, 'PipelineSync');
end;

procedure TSqlDBPostgresConnection.Flush;
begin
  PQ.flush(fPGConn);
end;

procedure TSqlDBPostgresConnection.SendFlushRequest;
begin
  if PQ.sendFlushRequest(fPGConn) <> PGRES_COMMAND_OK then
    PQ.RaiseError(fPGConn, 'SendFlushRequest');
end;

function TSqlDBPostgresConnection.PipelineStatus: integer;
begin
  Result := PQ.pipelineStatus(fPGConn);
end;

procedure TSqlDBPostgresConnection.CheckPipelineSync;
var
  res: pointer;
  err: integer;
begin
  res := PQ.getResult(fPGConn);
  PQ.Check(fPGConn, 'GetResult', res, @res, {andclear=}false);
  err := PQ.ResultStatus(res);
  if err <> PGRES_PIPELINE_SYNC then
    ESqlDBPostgres.RaiseUtf8(
      '%.CheckPipelineSync returned % instead of PGRES_PIPELINE_SYNC [%] ',
      [self, err, PQ.ErrorMessage(fPGConn)])
  else
    PQ.Clear(res);
end;

function TSqlDBPostgresConnection.PreparedCount: integer;
begin
  if (self = nil) or
     (fPrepared = nil) then
    result := 0
  else
    result := fPrepared.Count;
end;

function TSqlDBPostgresConnection.Socket: TNetSocket;
begin
  if (self = nil) or
     not Assigned(PQ.socket) then
    result := nil
  else
    result := pointer(PtrUInt(PQ.socket(fPGConn))); // transtype to our wrapper
end;

function TSqlDBPostgresConnection.SocketHasData: boolean;
begin
  result := Socket.HasData > 0;
end;


{ TSqlDBPostgresConnectionProperties }

procedure TSqlDBPostgresConnectionProperties.GetForeignKeys;
begin
  // TODO - how to get field we reference to? (currently consider this is "ID")
  with Execute('SELECT ct.conname as foreign_key_name, ' +
      '  case when ct.condeferred then 1 else 0 end as is_disabled, ' +
        '(SELECT tc.relname from pg_class tc ' +
          'where tc.oid = ct.conrelid) || ''.'' || ' +
           '(SELECT a.attname FROM pg_attribute a WHERE a.attnum = ' +
             'ct.conkey[1] AND a.attrelid = ct.conrelid) as from_ref, ' +
        '(SELECT tc.relname from pg_class tc where tc.oid = ' +
          'ct.confrelid) || ''.id'' as referenced_object ' +
      'FROM  pg_constraint ct WHERE contype = ''f''', []) do
    while Step do
      fForeignKeys.Add(ColumnUtf8(2), ColumnUtf8(3));
end;

procedure TSqlDBPostgresConnectionProperties.FillOidMapping;
begin
  // see pg_type.h (most used first)
  MapOid(INT4OID, ftInt64);
  MapOid(INT8OID, ftInt64);
  MapOid(TEXTOID, ftUtf8); // other char types will be ftUtf8 as fallback
  MapOid(FLOAT8OID, ftDouble);
  MapOid(TIMESTAMPOID, ftDate);
  MapOid(BYTEAOID, ftBlob);
  MapOid(NUMERICOID, ftCurrency); // our ORM uses NUMERIC(19,4) for currency
  MapOid(BOOLOID, ftInt64);
  MapOid(INT2OID, ftInt64);
  MapOid(CASHOID, ftCurrency);
  MapOid(TIMESTAMPTZOID, ftDate);
  MapOid(ABSTIMEOID, ftDate);
  MapOid(DATEOID, ftDate);
  MapOid(TIMEOID, ftDate);
  MapOid(TIMETZOID, ftDate);
  MapOid(REGPROCOID, ftInt64);
  MapOid(OIDOID, ftInt64);
  MapOid(FLOAT4OID, ftDouble);
  // note: any other unregistered OID will be handled as ftUtf8 to keep the data
end;

constructor TSqlDBPostgresConnectionProperties.Create(
  const aServerName, aDatabaseName, aUserID, aPassword: RawUtf8);
begin
  PostgresLibraryInitialize; // raise an ESqlDBPostgres on loading failure
  if PQ.IsThreadSafe <> 1 then
    raise ESqlDBPostgres.Create('libpq should be compiled in threadsafe mode');
  fDbms := dPostgreSQL;
  FillOidMapping;
  inherited Create(aServerName, aDatabaseName, aUserID, aPassWord);
  // JsonDecodedPrepareToSql will detect cPostgreBulkArray and set
  // DecodedFieldTypesToUnnest -> fast bulk insert/delete/update
  fBatchSendingAbilities := [cCreate, cDelete, cUpdate, cPostgreBulkArray];
  fNoBlobBindArray := true; // no BindArray() on ftBlob
  // disable MultiInsert SQL and rely on cPostgreBulkArray process for cCreate
  fOnBatchInsert := nil; // see TRestStorageExternal.InternalBatchStop
end;

function TSqlDBPostgresConnectionProperties.NewConnection: TSqlDBConnection;
var
  conn: TSqlDBPostgresConnection;
begin
  conn := TSqlDBPostgresConnection.Create(self);
  conn.InternalProcess(speCreated);
  result := conn;
end;

function TSqlDBPostgresConnectionProperties.Oid2FieldType(
  cOID: cardinal): TSqlDBFieldType;
var
  i: PtrInt;
begin
  if cOID <= 65535 then
  begin
    // fast brute force search within L1 CPU cache (use SSE2 asm on Intel/AMD)
    i := WordScanIndex(pointer(fOids), fOidsCount, cOID);
    if i >= 0 then
      result := fOidsFieldTypes[i]
    else
      result := ftUtf8;
  end
  else
    result := ftUtf8;
end;

procedure TSqlDBPostgresConnectionProperties.MapOid(cOid: cardinal;
  fieldType: TSqlDBFieldType);
var
  i: PtrInt;
begin
  if cOID > 65535 then
    ESqlDBPostgres.RaiseUtf8('Out of range %.MapOid(%)', [self, cOID]);
  i := WordScanIndex(pointer(fOids), fOidsCount, cOID);
  if i < 0 then
  begin
    i := FOidsCount;
    inc(FOidsCount);
    if i = length(FOids) then
    begin
      SetLength(fOids, i + 32);
      SetLength(fOidsFieldTypes, i + 32);
    end;
    fOids[i] := cOid;
  end;
  fOidsFieldTypes[i] := fieldType // set or replace
end;

function TSqlDBPostgresConnectionProperties.Async: TSqlDBPostgresAsync;
var
  main: TSqlDBPostgresConnection;
begin
  main := pointer(ThreadSafeConnection);
  if main.fAsync = nil then // no lock needed since it is a per-thread instance
    main.fAsync := TSqlDBPostgresAsync.Create(main); // it is time to setup
  result := main.fAsync;
end;

procedure TSqlDBPostgresStatement.BindColumns;
var
  nCols, c: integer;
  cName: RawUtf8;
  p: PUtf8Char;
begin
  ClearColumns;
  nCols := PQ.nfields(fRes);
  fColumn.Capacity := nCols;
  for c := 0 to nCols - 1 do
  begin
    p := PQ.fname(fRes, c);
    FastSetString(cName, p, StrLen(p));
    with AddColumn(cName)^ do
    begin
      ColumnAttr := PQ.ftype(fRes, c);
      ColumnType := TSqlDBPostgresConnectionProperties(Connection.Properties).
        Oid2FieldType(ColumnAttr);
    end;
  end;
end;

var // fake VArray markers
  _BindArrayJson, _BindArrayBin4, _BindArrayBin8: TRawUtf8DynArray;

function ComputeBinaryArray(p: PSqlDBParam; size: integer): boolean;
var
  bin: RawByteString;
begin
  result := ToArrayOid(pointer(p^.VData), p^.VDBType, p^.VInt64, size, bin);
  if result then
    p^.VData := bin;
end;

procedure TSqlDBPostgresStatement.BindParams;
var
  i: PtrInt;
  p: PSqlDBParam;
begin
  // mark parameter as textual by default, with no blob length
  FillCharFast(pointer(fPGParams)^, fParamCount shl POINTERSHR, 0);
  FillCharFast(pointer(fPGParamFormats)^, fParamCount shl 2, PGFMT_TEXT);
  FillCharFast(pointer(fPGParamLengths)^, fParamCount shl 2, 0);
  // bind fParams[] as expected by PostgreSQL - potentially as array
  p := pointer(fParams);
  for i := 0 to fParamCount - 1 do 
  begin
    if p^.VArray <> nil then
    begin
      // convert array parameter values into p^.VData text or bin
      if not (p^.VType in [
           ftInt64,
           ftDouble,
           ftCurrency,
           ftDate,
           ftUtf8]) then
        ESqlDBPostgres.RaiseUtf8('%.ExecutePrepared: Invalid array ' +
          'type % on bound parameter #%', [self, ToText(p^.VType)^, i]);
      if p^.VArray[0] <> _BindArrayJson[0] then
        // p^.VData is not the array encoded as PostgreSQL pseudo-JSON {....}
        if (p^.VArray[0] = _BindArrayBin4[0]) and
           ComputeBinaryArray(p, ord(p^.VArray[1][1]) - ord('0')) then
        begin
          // p^.VData is the raw integer/Int64 array as Postgres binary
          fPGParamFormats[i] := PGFMT_BIN;
          fPGParamLengths[i] := length(p^.VData);
        end
        else
          // p^.VData was not already set by BindArrayJson() -> convert now
          BoundArrayToJsonArray(p^.VArray, RawUtf8(p^.VData)); // e.g. '{1,2,3}'
    end
    else
      // single value parameter
      case p^.VType of
        ftNull:
          p^.VData := '';
        ftInt64:
          case p^.VDBType of
            INT4OID: // ORM create such fields for 32-bit values (ftUnknown)
              begin
                fPGParamFormats[i] := PGFMT_BIN;
                fPGParamLengths[i] := 4;
                p^.VInt64 := bswap32(p^.VInt64); // libpq expects network order
                fPGParams[i] := @p^.VInt64;
              end;
            INT8OID: // ORM create such fields for 64-bit values (ftInt64)
              begin
                fPGParamFormats[i] := PGFMT_BIN;
                fPGParamLengths[i] := 8;
                p^.VInt64 := bswap64(p^.VInt64);
                fPGParams[i] := @p^.VInt64;
              end;
          else
            Int64ToUtf8(p^.VInt64, RawUtf8(p^.VData));
          end;
        ftCurrency:
          Curr64ToStr(p^.VInt64, RawUtf8(p^.VData));
        ftDouble:
          if p^.VDBType = FLOAT8OID then // ORM create such fields for ftDouble
          begin
            fPGParamFormats[i] := PGFMT_BIN;
            fPGParamLengths[i] := 8;
            p^.VInt64 := bswap64(p^.VInt64); // double also in network order!
            fPGParams[i] := @p^.VInt64;
          end
          else
            DoubleToStr(PDouble(@p^.VInt64)^, RawUtf8(p^.VData));
        ftDate:
          // libpq expects space instead of T in ISO-8601 expanded format
          DateTimeToIso8601Var(PDateTime(@p^.VInt64)^,
            {expand=}true, fForceDateWithMS, ' ', #0, RawUtf8(p^.VData));
        ftUtf8:
          ; // UTF-8 text already in p^.VData buffer
        ftBlob:
          begin
            fPGParamFormats[i] := PGFMT_BIN;
            fPGParamLengths[i] := length(p^.VData);
          end;
      else
        ESqlDBPostgres.RaiseUtf8('%.ExecutePrepared: cannot bind ' +
          'parameter #% of type %', [self, i, ToText(p^.VType)^]);
      end;
    if fPGParams[i] = nil then
      fPGParams[i] := pointer(p^.VData);
    inc(p);
  end;
end;

procedure TSqlDBPostgresStatement.CheckColAndRowset(Col: integer);
begin
  if (cardinal(Col) >= cardinal(fColumnCount)) or
     (fRes = nil) or
     (fResStatus <> PGRES_TUPLES_OK) then
    CheckColInvalid(Col);
end;

destructor TSqlDBPostgresStatement.Destroy;
begin
  try
    Reset; // close result if any
  finally
    inherited;
  end;
end;

// see https://www.postgresql.org/docs/current/libpq-exec.html

procedure TSqlDBPostgresStatement.Prepare(
  const aSql: RawUtf8; ExpectResults: boolean);
var
  i: PtrInt;
  res: PPGresult;
  c: TSqlDBPostgresConnection;
begin
  // it is called once: already cached in TSqlDBConnection.NewStatementPrepared
  SqlLogBegin(sllDB);
  if aSql = '' then
    ESqlDBPostgres.RaiseUtf8('%.Prepare: empty statement', [self]);
  inherited Prepare(aSql, ExpectResults); // will strip last ;
  fPreparedParamsCount := ReplaceParamsByNumbers(fSql, fSqlPrepared, '$');
  if scPossible in fCache then
  begin
    // preparable statements will be cached server-side by index hexa as name
    include(fCache, scOnServer);
    c := TSqlDBPostgresConnection(fConnection);
    c.PrepareCached(fSqlPrepared, fPreparedParamsCount, fPreparedStmtName);
    // get param types into VDBType for possible binary binding in BindParams
    if fPreparedParamsCount > 0 then
    begin
      fParam.Count := fPreparedParamsCount;
      res := PQ.DescribePrepared(c.fPGConn, pointer(fPreparedStmtName));
      PQ.Check(c.fPGConn, 'DescribePrepared', res, nil, {andclear=}false);
      for i := 0 to fPreparedParamsCount - 1 do
        fParams[i].VDBType := PQ.ParamType(res, i);
      PQ.Clear(res);
    end;
    SqlLogEnd(' c=%', [fPreparedStmtName]);
  end
  else
    SqlLogEnd;
  // allocate libpq parameter buffers as dynamic arrays
  SetLength(fPGParams, fPreparedParamsCount);
  SetLength(fPGParamFormats, fPreparedParamsCount);
  SetLength(fPGParamLengths, fPreparedParamsCount);
end;

procedure TSqlDBPostgresStatement.ExecutePrepared;
var
  c: TSqlDBPostgresConnection;
begin
  SqlLogBegin(sllSQL);
  if fRes <> nil then
  begin
    PQ.Clear(fRes); // if forgot to call ReleaseRows
    fRes := nil;
  end;
  if fSqlPrepared = '' then
    ESqlDBPostgres.RaiseUtf8('%.ExecutePrepared: Statement not prepared', [self]);
  if fParamCount <> fPreparedParamsCount then
    ESqlDBPostgres.RaiseUtf8('%.ExecutePrepared: Query expects % ' +
      'parameters but % bound', [self, fPreparedParamsCount, fParamCount]);
  inherited ExecutePrepared;
  BindParams;
  c := TSqlDBPostgresConnection(fConnection);
  if fPreparedStmtName <> '' then
    fRes := PQ.ExecPrepared(c.fPGConn, pointer(fPreparedStmtName),
      fPreparedParamsCount, pointer(fPGParams), pointer(fPGParamLengths),
      pointer(fPGParamFormats), PGFMT_TEXT)
  else if fPreparedParamsCount = 0 then
    // PQexec handles multiple SQL commands
    fRes := PQ.Exec(c.fPGConn, pointer(fSqlPrepared))
  else
    fRes := PQ.ExecParams(c.fPGConn, pointer(fSqlPrepared),
      fPreparedParamsCount, nil, pointer(fPGParams), pointer(fPGParamLengths),
      pointer(fPGParamFormats), PGFMT_TEXT);
  PQ.Check(c.fPGConn, 'Exec', fRes, @fRes, {andclear=}false);
  fResStatus := PQ.ResultStatus(fRes);
  if fExpectResults then
  begin
    if fResStatus <> PGRES_TUPLES_OK then
    begin
      // paranoid check
      PQ.Clear(fRes);
      fRes := nil;
      ESqlDBPostgres.RaiseUtf8('%.ExecutePrepared: result expected ' +
        'but statement did not return tuples', [self]);
    end;
    fTotalRowsRetrieved := PQ.ntuples(fRes);
    fCurrentRow := -1;
    if fColumn.Count = 0 then // columns exist when statement was cached
      BindColumns;
    if fSqlLogLog <> nil then
      SqlLogEnd(' c=% r=%', [fPreparedStmtName, fTotalRowsRetrieved]);
  end
  else if fSqlLogLog <> nil then
    SqlLogEnd(' c=%', [fPreparedStmtName]);
end;

procedure TSqlDBPostgresStatement.SendPipelinePrepared;
var
  c: TSqlDBPostgresConnection;
  res: integer;
begin
  SqlLogBegin(sllSQL);
  if fSqlPrepared = '' then
    ESqlDBPostgres.RaiseUtf8('%.SendPipelinePrepared: Statement not prepared', [self]);
  if fParamCount <> fPreparedParamsCount then
    ESqlDBPostgres.RaiseUtf8('%.SendPipelinePrepared: Query expects % ' +
      'parameters but % bound', [self, fPreparedParamsCount, fParamCount]);
  inherited ExecutePrepared;
  BindParams;
  c := TSqlDBPostgresConnection(fConnection);
  if fPreparedStmtName <> '' then
    res := PQ.sendQueryPrepared(c.fPGConn, pointer(fPreparedStmtName),
      fPreparedParamsCount, pointer(fPGParams), pointer(fPGParamLengths),
      pointer(fPGParamFormats), PGFMT_TEXT)
  else
    res := PQ.sendQueryParams(c.fPGConn, pointer(fSqlPrepared),
      fPreparedParamsCount, nil, pointer(fPGParams), pointer(fPGParamLengths),
      pointer(fPGParamFormats), PGFMT_TEXT);
  if res <> 1 then
    PQ.RaiseError(c.fPGConn, 'SendPipelinePrepared');
  if fSqlLogLog <> nil then
    SqlLogEnd(' c=%', [fPreparedStmtName]);
end;

procedure TSqlDBPostgresStatement.GetPipelineResult;
var
  c: TSqlDBPostgresConnection;
  endRes: pointer;
begin
  SqlLogBegin(sllResult);
  c := TSqlDBPostgresConnection(fConnection);
  if fRes <> nil then
  begin
    PQ.Clear(fRes); // if forgot to call ReleaseRows
    fRes := nil;
  end;
  fRes := PQ.getResult(c.fPGConn);
  PQ.Check(c.fPGConn, 'GetPipelineResult', fRes, @fRes, {andclear=}false);
  fResStatus := PQ.ResultStatus(fRes);
  if fExpectResults then
  begin
    if fResStatus <> PGRES_TUPLES_OK then
    begin
      PQ.Clear(fRes);
      fRes := nil;
      ESqlDBPostgres.RaiseUtf8('%.GetPipelineResult: result expected ' +
        'but statement did not return tuples (status=%)', [self, fResStatus]);
    end;
    fTotalRowsRetrieved := PQ.ntuples(fRes);
    fCurrentRow := -1;
    if fColumn.Count = 0 then // columns exist when statement was cached
      BindColumns;
    if fSqlLogLog <> nil then
      SqlLogEnd(' c=% r=%', [fPreparedStmtName, fTotalRowsRetrieved]);
  end
  else if fSqlLogLog <> nil then
    SqlLogEnd(' c=%', [fPreparedStmtName]);
  endRes := PQ.getResult(c.fPGConn);
  if endRes <> nil then
    // nil represents end of the result set
    ESqlDBPostgres.RaiseUtf8('%.GetPipelineResult: returned something extra', [self]);
end;

procedure TSqlDBPostgresStatement.BindArray(Param: integer;
  const Values: array of Int64);
var
  p: PSqlDBParam;
begin
  // PostgreSQL has its own JSON-like syntax, which is '{1,2,3}' for integers
  if high(Values) < 0 then
    ESqlDBPostgres.RaiseUtf8('%.BindArray([])', [self]);
  p := CheckParam(Param, ftInt64, paramIn, 0);
  fParamsArrayCount := length(Values);
  p^.VInt64 := fParamsArrayCount;
  if TSqlDBPostgresConnectionProperties(fConnection.Properties).
       ArrayParamsAsBinary then
  begin
    p^.VArray := _BindArrayBin8; // fake marker
    FastSetRawByteString(p^.VData, @Values[0], fParamsArrayCount * 8);
  end
  else
  begin
    p^.VArray := _BindArrayJson; // fake marker
    p^.VData := Int64DynArrayToCsv(@Values[0], fParamsArrayCount, '{', '}');
  end;
end;

procedure TSqlDBPostgresStatement.BindArrayInt32(Param: integer;
  const Values: TIntegerDynArray);
var
  p: PSqlDBParam;
begin
  // PostgreSQL has its own JSON-like syntax, which is '{1,2,3}' for integers
  if Values = nil then
    ESqlDBPostgres.RaiseUtf8('%.BindArrayInt32([])', [self]);
  p := CheckParam(Param, ftInt64, paramIn, 0);
  fParamsArrayCount := length(Values);
  p^.VInt64 := fParamsArrayCount;
  if TSqlDBPostgresConnectionProperties(fConnection.Properties).
       ArrayParamsAsBinary then
  begin
    p^.VArray := _BindArrayBin4; // fake marker
    FastSetRawByteString(p^.VData, pointer(Values), fParamsArrayCount * 4);
  end
  else
  begin
    p^.VArray := _BindArrayJson; // fake marker
    p^.VData := IntegerDynArrayToCsv(pointer(Values), fParamsArrayCount, '{', '}');
  end;
end;

procedure TSqlDBPostgresStatement.BindArrayJson(Param: integer;
  ParamType: TSqlDBFieldType; var JsonArray: RawUtf8; ValuesCount: integer);
var
  p: PSqlDBParam;
begin
  // PostgreSQL has its own JSON-like syntax, easy to convert from true JSON
  if (ValuesCount <= 0) or
     (JsonArray = '') or
     (JsonArray[1] <> '[') or
     (JsonArray[length(JsonArray)] <> ']') then
    ParamType := ftUnknown;    // to raise exception
  p := CheckParam(Param, ParamType, paramIn, 0);
  p^.VArray := _BindArrayJson; // fake marker
  p^.VInt64 := ValuesCount;
  JsonArray[1] := '{';         // convert to PostgreSQL weird syntax
  JsonArray[length(JsonArray)] := '}';
  p^.VData := JsonArray;       // ExecutePrepared will use directly this
  fParamsArrayCount := ValuesCount;
end;

function TSqlDBPostgresStatement.UpdateCount: integer;
begin
  result := GetCardinalDef(PQ.cmdTuples(fRes), 0);
end;

procedure TSqlDBPostgresStatement.Reset;
begin
  ReleaseRows;
  fResStatus := PGRES_EMPTY_QUERY;
  inherited Reset;
end;

function TSqlDBPostgresStatement.Step(SeekFirst: boolean): boolean;
begin
  if (fRes = nil) or
     (fResStatus <> PGRES_TUPLES_OK) then
    ESqlDBPostgres.RaiseUtf8('%.Execute should be called before Step', [self]);
  if SeekFirst then
    fCurrentRow := -1;
  result := fCurrentRow + 1 < fTotalRowsRetrieved;
  if not result then
    exit;
  inc(fCurrentRow);
end;

procedure TSqlDBPostgresStatement.ReleaseRows;
begin
  if fRes <> nil then
  begin
    PQ.clear(fRes);
    fRes := nil;
  end;
  inherited ReleaseRows;
end;

function TSqlDBPostgresStatement.ColumnInt(Col: integer): int64;
begin
  CheckColAndRowset(Col);
  result := GetInt64(PQ.GetValue(fRes, fCurrentRow, Col));
end;

function TSqlDBPostgresStatement.ColumnNull(Col: integer): boolean;
begin
  CheckColAndRowset(Col);
  result := (PQ.GetIsNull(fRes, fCurrentRow, Col) = 1);
end;

function TSqlDBPostgresStatement.ColumnDouble(Col: integer): double;
begin
  CheckColAndRowset(Col);
  result := GetExtended(PQ.GetValue(fRes, fCurrentRow, Col));
end;

function TSqlDBPostgresStatement.ColumnDateTime(Col: integer): TDateTime;
var
  P: PUtf8Char;
begin
  CheckColAndRowset(Col);
  P := PQ.GetValue(fRes, fCurrentRow, Col);
  Iso8601ToDateTimePUtf8CharVar(P, StrLen(P), result);
end;

function TSqlDBPostgresStatement.ColumnCurrency(Col: integer): currency;
begin
  CheckColAndRowset(Col);
  PInt64(@result)^ := StrToCurr64(PQ.GetValue(fRes, fCurrentRow, Col));
end;

function TSqlDBPostgresStatement.ColumnUtf8(Col: integer): RawUtf8;
begin
  CheckColAndRowset(Col);
  PQ.GetRawUtf8(fRes, fCurrentRow, Col, result);
end;

function TSqlDBPostgresStatement.ColumnPUtf8(Col: integer): PUtf8Char;
begin
  CheckColAndRowset(Col);
  result := PQ.GetValue(fRes, fCurrentRow, Col);
end;

function TSqlDBPostgresStatement.ColumnBlob(Col: integer): RawByteString;
var
  P: pointer;
begin
  // PGFMT_TEXT was used -> need to convert into binary
  CheckColAndRowset(Col);
  P := PQ.GetValue(fRes, fCurrentRow, Col);
  FastSetRawByteString(result, P,
    BlobInPlaceDecode(P, PQ.GetLength(fRes, fCurrentRow, col)));
end;

procedure TSqlDBPostgresStatement.ColumnToJson(Col: integer; W: TJsonWriter);
var
  P: pointer;
begin
  if (fRes = nil) or
     (fResStatus <> PGRES_TUPLES_OK) or
     (fCurrentRow < 0) then
    ESqlDBPostgres.RaiseUtf8('Unexpected %.ColumnToJson', [self]);
  with fColumns[Col] do
  begin
    P := PQ.GetValue(fRes, fCurrentRow, Col);
    if (PUtf8Char(P)^ = #0) and
       (PQ.GetIsNull(fRes, fCurrentRow, Col) = 1) then
      W.AddNull
    else
    begin
      case ColumnType of
        ftNull:
          W.AddNull;
        ftInt64,
        ftDouble,
        ftCurrency:
          if ColumnAttr = BOOLOID then // = PQ.ftype(fRes, Col)
            W.Add((P <> nil) and (PUtf8Char(P)^ = 't'))
          else
            // note: StrLen slightly faster than PQ.GetLength for small content
            W.AddShort(P, StrLen(P));
        ftUtf8:
          if (ColumnAttr = JSONOID) or
             (ColumnAttr = JSONBOID) then
            W.AddShort(P, PQ.GetLength(fRes, fCurrentRow, Col))
          else
          begin
            W.Add('"');
            W.AddJsonEscape(P, 0); // Len=0 is faster than StrLen/GetLength
            W.AddDirect('"');
          end;
        ftDate:
          begin
            W.Add('"');
            if (StrLen(P) > 10) and
               (PAnsiChar(P)[10] = ' ') then
              PAnsiChar(P)[10] := 'T'; // ensure strict ISO-8601 encoding
            W.AddJsonEscape(P);
            W.AddDirect('"');
          end;
        ftBlob:
          if fForceBlobAsNull then
            W.AddNull
          else
            W.WrBase64(P, BlobInPlaceDecode(P,
              PQ.GetLength(fRes, fCurrentRow, Col)), {withmagic=}true);
      else
        ESqlDBPostgres.RaiseUtf8('%.ColumnToJson: ColumnType=%?',
          [self, ord(ColumnType)]);
      end;
    end;
  end;
end;


{ ************ TSqlDBPostgresAsync Asynchronous Execution via Pipelines }

{ TSqlDBPostgresAsyncStatement }

procedure TSqlDBPostgresAsyncStatement.ExecuteAsync(Context: TObject;
  const OnFinished: TOnSqlDBPostgresAsyncEvent;
  ForcedOptions: PSqlDBPostgresAsyncStatementOptions);
var
  task: TSqlDBPostgresAsyncTask;
begin
  // caller did protect this method with Lock/UnLock
  if not Assigned(OnFinished) then
    ESqlDBPostgresAsync.RaiseUtf8(
      '%.ExecuteAsync with OnFinished=nil [%]', [self, fSql]);
  // create a new task
  task.Statement := self;
  task.Context := Context;
  task.OnFinished := OnFinished;
  if ForcedOptions <> nil then
    task.Options := ForcedOptions^
  else
    task.Options := fAsyncOptions;
  fOwner.fTasks.Push(task);
  try
    // pipeline the SQL request to the server
    SendPipelinePrepared;
    if asoForcePipelineSync in task.Options then
      fOwner.Connection.PipelineSync; // required e.g. for TFB benchmarks
    if asoForceConnectionFlush in task.Options then
      fOwner.Connection.Flush; // may be needed on modified libpq
  except
    // on fatal error don't go any further and notify the pending callbacks
    fOwner.DoExecuteAsyncError;
  end;
end;

procedure TSqlDBPostgresAsyncStatement.ExecuteAsyncNoParam(Context: TObject;
  const OnFinished: TOnSqlDBPostgresAsyncEvent;
  ForcedOptions: PSqlDBPostgresAsyncStatementOptions);
begin
  try
    ExecuteAsync(Context, OnFinished, ForcedOptions);
  finally
    fOwner.UnLock;
  end;
end;

procedure TSqlDBPostgresAsyncStatement.Lock;
begin
  fOwner.Lock;
end;

procedure TSqlDBPostgresAsyncStatement.Unlock;
begin
  fOwner.Unlock;
end;


{ TSqlDBPostgresAsyncThread }

constructor TSqlDBPostgresAsyncThread.Create(aOwner: TSqlDBPostgresAsync);
begin
  fOwner := aOwner;
  FormatUtf8('db%', [CurrentThreadNameShort^], fName);
  inherited Create({suspended=}false);
end;

procedure TSqlDBPostgresAsyncThread.Execute;
var
  res: TNetEvents;
  task: TSqlDBPostgresAsyncTask;
  {%H-}log: ISynLog;
begin
  fProcessing := true;
  SetCurrentThreadName(fName);
  log := SynDBLog.Enter(self, 'Execute');
  try
    repeat
      // notify if needed
      if Assigned(fOnThreadStart) then
        try
          fOnThreadStart(self);
          fOnThreadStart := nil;
        except
          fOnThreadStart := nil;
        end;
      // wait to have some data pending on the input socket for a task
      repeat
        if fOwner.fConnection = nil then
          res := [neClosed]
        else
          res := fOwner.fConnection.Socket.WaitFor(-1, [neRead, neError]);
        if Terminated or
           (neRead in res) then
          break;
        SleepHiRes(10); // loop on broken or not yet established connection
      until Terminated;
      if Terminated then
        break;
      if not fOwner.fTasks.Pending then // happens e.g. during statement parsing
        continue; // no sleep(): just loop to WaitFor() syscall again
      // handle incoming responses from PostgreSQL
      task.OnFinished := nil;
      try
        fOwner.Lock; // faster to maintain the lock over the whole loop
        try
          while not Terminated and
                fOwner.fTasks.Pop(task) do
          begin
            task.Statement.GetPipelineResult;
            if Terminated then
              break;
            try
              task.OnFinished(task.Statement, task.Context);
            except
              // continue anyway on error in the end-user callback (paranoid)
            end;
            task.OnFinished := nil; // was notified
            task.Statement.ReleaseRows;
            if asoForcePipelineSync in task.Options then
              fOwner.fConnection.CheckPipelineSync;
            // warning: won't check for the socket state witin the loop: some
            // results may be pending at TLS level, but not at socket/TCP level
          end;
        finally
          fOwner.Unlock;
        end;
      except
        on E: Exception do
        begin
          // on fatal DB error don't go any further and notify the callbacks
          if Assigned(log) then
            log.Log(sllWarning, 'Execute: % during %',
              [E.ClassType, task.Statement.Sql], self);
          if Assigned(task.OnFinished) then
            fOwner.fTasks.Push(task); // task.OnFinished() was never called
          fOwner.DoExecuteAsyncError;
        end;
      end;
    until Terminated;
  except
    on E: Exception do
      SynDBLog.Add.Log(sllWarning, 'Execute raised a % -> terminate thread %',
          [E.ClassType, fName], self);
  end;
  fProcessing := false;
  log := nil;
  TSynLog.Add.NotifyThreadEnded;
end;


{ TSqlDBPostgresAsync }

constructor TSqlDBPostgresAsync.Create(Owner: TSqlDBPostgresConnection);
begin
  inherited Create;
  fTasks := TSynQueue.Create(TypeInfo(TSqlDBPostgresAsyncTasks));
  fConnection := TSqlDBPostgresConnection.Create(Owner.Properties);
end;

destructor TSqlDBPostgresAsync.Destroy;
begin
  if Assigned(fThread) then
    fThread.Terminate;
  if fTasks.Pending then
  begin
    // notify shutdown (paranoid)
    SynDBLog.Add.Log(sllWarning,
      'Destroy: tasks queue not void - missing FlushIfNeeded call?', self);
    DoExecuteAsyncError;
  end;
  inherited Destroy;
  ObjArrayClear(fStatements, {continueonexcept=}true);
  if fTasks.Pending then // should never happen
    SynDBLog.Add.Log(sllWarning, 'Destroy: tasks=%', [fTasks.Count], self);
  if Assigned(fConnection) then
  begin
    fConnection.ExitPipelineMode;
    FreeAndNil(fConnection); // will break PQ.socket and release fThread.Execute
  end;
  if Assigned(fThread) then
  begin
    SleepHiRes(5500, fThread.fProcessing, false);
    fThread.Free;
  end;
  FreeAndNil(fTasks);
end;

function TSqlDBPostgresAsync.Prepare(const Sql: RawUtf8; ExpectResults: boolean;
  Options: TSqlDBPostgresAsyncStatementOptions): TSqlDBPostgresAsyncStatement;
var
  tix, endtix: Int64;
  i: PtrInt;
begin
  // initialize the background thread and connection if needed
  if fThread = nil then
  begin
    Lock;
    try
      if fThread = nil then
      begin
        fConnection.Connect;
        fConnection.EnterPipelineMode;
        fThread := TSqlDBPostgresAsyncThread.Create(self);
      end;
    finally
      UnLock;
    end;
  end;
  // check if not already prepared for this thread (no lock needed)
  for i := 0 to length(fStatements) - 1 do
  begin
    result := fStatements[i];
    if result.Sql = Sql then
      exit;
  end;
  // ensure we can initialize a new statement now in non-pipelined mode
  endtix := 0;
  repeat
    if not fTasks.Pending then
    begin
      Lock;
      if fConnection.TryExitPipelineMode then
        break;
      Unlock; // there may be some tasks pending in the background thread
    end;
    SleepHiRes(1);
    tix := GetTickCount64;
    if endtix = 0 then
      endtix := tix + 5000 // never wait forever
    else if tix > endtix then
      ESqlDBPostgresAsync.RaiseUtf8('%.NewStatement timeout', [self]);
  until false;
  // initialize the new statement within the acquired lock
  try
    try
      result := TSqlDBPostgresAsyncStatement.Create(fConnection);
      result.fOwner := self;
      result.fAsyncOptions := Options;
      include(result.fCache, scPossible); // always cached
      result.Prepare(Sql, ExpectResults);
    finally
      fConnection.EnterPipelineMode; // back to async state
    end;
  finally
    Unlock;
  end;
  ObjArrayAdd(fStatements, result); // statements are owned by this class
end;

function TSqlDBPostgresAsync.PrepareLocked(const Sql: RawUtf8; ExpectResults: boolean;
  Options: TSqlDBPostgresAsyncStatementOptions): TSqlDBPostgresAsyncStatement;
begin
  result := Prepare(Sql, ExpectResults, Options);
  Lock;
end;

procedure TSqlDBPostgresAsync.DoExecuteAsyncError;
var
  task: TSqlDBPostgresAsyncTask;
  n: PtrInt;
begin
  // caller did protect this method with Lock/UnLock
  n := fTasks.Count;
  if n <> 0 then
    with SynDBLog.Enter('ExecuteAsyncError aborted=%', [n], self) do
      while fTasks.Pop(task) do
        try
          task.OnFinished({statement=}nil, task.Context); // notify error
        except
        end;
end;



initialization
  TSqlDBPostgresConnectionProperties.RegisterClassNameForDefinition;
  SetLength(_BindArrayJson, 1);
  _BindArrayJson[0] := 'json'; // impossible SQL value (should be '''json''')
  SetLength(_BindArrayBin4, 2);
  _BindArrayBin4[0] := 'bin';
  _BindArrayBin4[1] := '4'; // item size
  SetLength(_BindArrayBin8, 2);
  _BindArrayBin8[0] := 'bin';
  _BindArrayBin8[1] := '8';

end.

