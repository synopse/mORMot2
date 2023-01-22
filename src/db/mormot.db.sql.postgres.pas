/// Database Framework Direct PostgreSQL Connnection via libpq
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.sql.postgres;

{
  *****************************************************************************

   Direct PostgreSQL Client Access using the libpq Library
    -  TSqlDBPostgreConnection* and TSqlDBPostgreStatement Classes

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
  mormot.db.core,
  mormot.db.sql;


{ ************ TSqlDBPostgreConnection* and TSqlDBPostgreStatement Classes }

type
  /// connection properties which will implement an internal Thread-Safe
  // connection pool
  TSqlDBPostgresConnectionProperties = class(TSqlDBConnectionPropertiesThreadSafe)
  private
    fOids: TWordDynArray; // O(n) search in L1 cache - use SSE2 on FPC x86_64
    fOidsFieldTypes: TSqlDBFieldTypeDynArray;
    fOidsCount: integer;
  protected
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
  end;


  /// implements a connection via the libpq access layer
  TSqlDBPostgresConnection = class(TSqlDBConnectionThreadSafe)
  protected
    // SQL of server-side prepared statements - name is index as hexadecimal
    // - statements are already cached in TSqlDBConnection.NewStatementPrepared
    fPrepared: TRawUtf8List;
    // the associated low-level provider connection
    fPGConn: pointer;
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
    /// Enter Pipelining mode
    // - *Warning* - connection is in blocking mode, see notes about possible deadlock
    // https://www.postgresql.org/docs/current/libpq-pipeline-mode.html#LIBPQ-PIPELINE-USING
    procedure EnterPipelineMode;
    /// Exit Pipelining mode
    procedure ExitPipelineMode;
    /// Marks a synchronization point in a pipeline by sending a sync message
    // and flushing the send buffer
    procedure PipelineSync;
    /// Flush any queued output data to the server
    procedure Flush;
    /// Sends a request for the server to flush its output buffer
    procedure SendFlushRequest;
    /// Return current pipeline status
    function PipelineStatus: integer;
    /// direct access to the associated PPGconn connection
    property Direct: pointer
      read fPGConn;
    /// how many prepared statements are currently cached for this connection
    function PreparedCount: integer;
  end;


  /// implements a statement via a Postgres database connection
  TSqlDBPostgresStatement = class(TSqlDBStatementWithParamsAndColumns)
  protected
    fPreparedStmtName: RawUtf8; // = hexadecimal of the SQL cached index
    fRes: pointer;
    fResStatus: integer;
    fPreparedParamsCount: integer;
    // pointers to query parameters; initialized by Prepare, filled in Executeprepared
    fPGParams: TPointerDynArray;
    // 0 - text, 1 - binary; initialized by Prepare, filled in Executeprepared
    fPGParamFormats: TIntegerDynArray;
    // non zero for binary params
    fPGParamLengths: TIntegerDynArray;
    /// define the result columns name and content
    procedure BindColumns;
    /// set parameters as expected by PostgresSQL
    procedure BindParams;
    /// raise an exception if Col is out of range according to fColumnCount
    // or rowset is not initialized
    procedure CheckColAndRowset(const Col: integer);
  public
    /// finalize the statement for a given connection
    destructor Destroy; override;
    /// Prepare an UTF-8 encoded SQL statement
    // - parameters marked as ? will be bound later, before ExecutePrepared call
    // - if ExpectResults is TRUE, then Step() and Column*() methods are available
    // to retrieve the data rows
    // - raise an ESqlDBPostgres on any error
    procedure Prepare(const aSql: RawUtf8; ExpectResults: boolean = False); overload; override;
    /// Execute a prepared SQL statement
    // - parameters marked as ? should have been already bound with Bind*() functions
    // - this implementation will also handle bound array of values (if any)
    // - this overridden method will log the SQL statement if sllSQL has been
    // enabled in SynDBLog.Family.Level
    // - raise an ESqlDBPostgres on any error
    procedure ExecutePrepared; override;
    /// For connection in pipelining mode
    // - sends a request to execute a prepared statement with given parameters, 
    // without waiting for the result(s)
    // - after all statements are sent, conn.SendFlushRequest should be called,
    // then GetPipelineResult is able to read results in order they were sent
    procedure SendPipelinePrepared;
    /// Retrieve next result for pipelined statement
    procedure GetPipelineResult;
    /// bind an array of JSON values to a parameter
    // - overloaded for direct assignment to the PostgreSQL client
    // - warning: input JSON should already be in the expected format (ftDate)
    procedure BindArrayJson(Param: integer; ParamType: TSqlDBFieldType;
      var JsonArray: RawUtf8; ValuesCount: integer); override;
    /// gets a number of updates made by latest executed statement
    function UpdateCount: integer; override;
    /// Reset the previous prepared statement
    // - this overridden implementation will reset all bindings and the cursor state
    // - raise an ESqlDBPostgres on any error
    procedure Reset; override;

    /// Access the next or first row of data from the SQL Statement result
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
    /// return a Column as a blob value of the current Row, first Col is 0
    function ColumnBlob(Col: integer): RawByteString; override;
    /// return one column value into JSON content
    procedure ColumnToJson(Col: integer; W: TJsonWriter); override;
    /// how many parameters founded during prepare stage
    property PreparedParamsCount: integer
      read fPreparedParamsCount;
  end;


implementation

uses
  mormot.db.raw.postgres; // raw libpq library API access


{ ************ TSqlDBPostgreConnection* and TSqlDBPostgreStatement Classes }

{ TSqlDBPostgresConnection }

destructor TSqlDBPostgresConnection.Destroy;
begin
  inherited Destroy;
  fPrepared.Free;
end;

function TSqlDBPostgresConnection.PrepareCached(
  const aSql: RawUtf8; aParamCount: integer; out aName: RawUtf8): integer;
begin
  if fPrepared = nil then
  begin
    fPrepared := TRawUtf8List.CreateEx(
      [fCaseSensitive, fNoDuplicate, fNoThreadLock]);
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
  PQ.Check(fPGConn,
    PQ.Prepare(fPGConn, pointer(aName), pointer(aSql), aParamCount, nil));
end;

procedure TSqlDBPostgresConnection.DirectExecSql(const SQL: RawUtf8);
begin
  PQ.Check(fPGConn,
    PQ.Exec(fPGConn, pointer(SQL)));
end;

procedure TSqlDBPostgresConnection.DirectExecSql(
  const SQL: RawUtf8; out Value: RawUtf8);
var
  res: PPGresult;
begin
  res := PQ.Exec(fPGConn, pointer(SQL));
  PQ.Check(fPGConn, res, nil, {andclear=}false);
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
  else
  if PWord(P)^ = ord('\') + ord('x') shl 8 then {ssByteAasHex in fServerSettings}
  begin
    result := (PLen - 2) shr 1; // skip trailing \x and compute number of bytes
    if result > 0 then
      HexToBinFast(P + 2, PByte(P), result); // in-place conversion
  end
  else
    // oldest PostgreSQL versions may stil use octal encoding (unlikely)
    result := OctToBin(P, pointer(P)); // in-place conversion
end;

procedure SynLogNoticeProcessor({%H-}arg: Pointer; message: PUtf8Char); cdecl;
begin
  SynDBLog.Add.Log(sllTrace, 'PGINFO: %', [message], TObject(arg));
end;

procedure DummyNoticeProcessor({%H-}arg: Pointer; message: PUtf8Char); cdecl;
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
      raise ESqlDBPostgres.CreateUtf8('Connection to database % failed [%]',
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
    raise ESqlDBPostgres.CreateUtf8('Invalid %.StartTransaction: nested ' +
      'transactions are not supported by the Postgres - use SAVEPOINT instead',
      [self]);
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
    raise ESqlDBPostgres.CreateUtf8(
      'EnterPipelineMonde(%): pipelining unsupported in % v%',
      [Properties.DatabaseNameSafe, PQ.LibraryPath, PQ.LibVersion]);
  if PQ.enterPipelineMode(fPGConn) <> 1 then
    raise ESqlDBPostgres.CreateUtf8('Enter pipeline mode % failed [%]',
      [Properties.DatabaseNameSafe, PQ.ErrorMessage(fPGConn)]);
end;

procedure TSqlDBPostgresConnection.ExitPipelineMode;
var
  res: pointer;
  err: integer;
begin
  if PQ.exitPipelineMode(fPGConn) <> 1 then
    raise ESqlDBPostgres.CreateUtf8(
      '%.ExitPipelineMode: attempt to exit pipeline mode failed when it should''ve succeeded [%]',
      [self, PQ.ErrorMessage(fPGConn)]);
  if (PQ.pipelineStatus(fPGConn) <> PQ_PIPELINE_OFF) then
    raise ESqlDBPostgres.CreateUtf8(
      '%.ExitPipelineMode: exiting pipeline mode didn''t seem to work',
      [self]);
end;

procedure TSqlDBPostgresConnection.PipelineSync;
begin
  if PQ.pipelineSync(fPGConn) <> 1 then
    raise ESqlDBPostgres.CreateUtf8('Pipeline sync % failed [%]',
      [Properties.DatabaseNameSafe, PQ.ErrorMessage(fPGConn)]);
end;

procedure TSqlDBPostgresConnection.Flush;
begin
  PQ.flush(fPGConn);
end;

procedure TSqlDBPostgresConnection.SendFlushRequest;
begin
  if PQ.sendFlushRequest(fPGConn) <> 1 then
    raise ESqlDBPostgres.CreateUtf8('sendFlushRequest % failed [%]',
      [Properties.DatabaseNameSafe, PQ.ErrorMessage(fPGConn)]);
end;

function TSqlDBPostgresConnection.PipelineStatus: integer;
begin
  Result := PQ.pipelineStatus(fPGConn);
end;

function TSqlDBPostgresConnection.PreparedCount: integer;
begin
  if (self = nil) or
     (fPrepared = nil) then
    result := 0
  else
    result := fPrepared.Count;
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
  // disable MultiInsert SQL and rely on cPostgreBulkArray process for cCreate
  fOnBatchInsert := nil; // see TRestStorageExternal.InternalBatchStop
end;

function TSqlDBPostgresConnectionProperties.NewConnection: TSqlDBConnection;
begin
  result := TSqlDBPostgresConnection.Create(self);
  TSqlDBPostgresConnection(result).InternalProcess(speCreated);
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
    raise ESqlDBPostgres.CreateUtf8('Out of range %.MapOid(%)', [self, cOID]);
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

var
  _BindArrayJson: TRawUtf8DynArray; // fake variable as VArray marker

procedure TSqlDBPostgresStatement.BindParams;
var
  i: PtrInt;
  p: PSqlDBParam;
begin
  // bind fParams[] as expected by PostgreSQL - potentially as array
  for i := 0 to fParamCount - 1 do 
  begin
    // mark parameter as textual by default, with no blob len
    fPGParamFormats[i] := 0;
    fPGParamLengths[i] := 0;
    // convert parameter value as text stored in p^.VData
    p := @fParams[i];
    if p^.VArray <> nil then
    begin
      if not (p^.VType in [
           ftInt64,
           ftDouble,
           ftCurrency,
           ftDate,
           ftUtf8]) then
        raise ESqlDBPostgres.CreateUtf8('%.ExecutePrepared: Invalid array ' +
          'type % on bound parameter #%', [Self, ToText(p^.VType)^, i]);
      if p^.VArray[0] <> _BindArrayJson[0] then
        // p^.VData was not already set by BindArrayJson() -> convert now
        p^.VData := BoundArrayToJsonArray(p^.VArray); // e.g. '{1,2,3}'
    end
    else
    begin
      case p^.VType of
        ftNull:
          p^.VData := '';
        ftInt64:
          Int64ToUtf8(p^.VInt64, RawUtf8(p^.VData));
        ftCurrency:
          Curr64ToStr(p^.VInt64, RawUtf8(p^.VData));
        ftDouble:
          DoubleToStr(PDouble(@p^.VInt64)^, RawUtf8(p^.VData));
        ftDate:
          // Postgres expects space instead of T in ISO-8601 expanded format
          DateTimeToIso8601Var(PDateTime(@p^.VInt64)^,
            {expand=}true, fForceDateWithMS, ' ', #0, RawUtf8(p^.VData));
        ftUtf8:
          ; // UTF-8 text already in p^.VData buffer
        ftBlob:
          begin
            fPGParamFormats[i] := 1; // binary
            fPGParamLengths[i] := length(p^.VData);
          end;
      else
        raise ESqlDBPostgres.CreateUtf8('%.ExecutePrepared: cannot bind ' +
          'parameter #% of type %', [self, i, ToText(p^.VType)^]);
      end;
    end;
    fPGParams[i] := pointer(p^.VData);
  end;
end;

procedure TSqlDBPostgresStatement.CheckColAndRowset(const Col: integer);
begin
  CheckCol(Col);
  if (fRes = nil) or
     (fResStatus <> PGRES_TUPLES_OK) then
    raise ESqlDBPostgres.CreateUtf8(
      '%.Execute not called before Column*', [self]);
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
begin
  // it is called once: already cached in TSqlDBConnection.NewStatementPrepared
  SqlLogBegin(sllDB);
  if aSql = '' then
    raise ESqlDBPostgres.CreateUtf8('%.Prepare: empty statement', [self]);
  inherited Prepare(aSql, ExpectResults); // will strip last ;
  fPreparedParamsCount := ReplaceParamsByNumbers(fSql, fSqlPrepared, '$');
  if scPossible in fCache then
  begin
    // preparable statements will be cached server-side by index hexa as name
    include(fCache, scOnServer);
    TSqlDBPostgresConnection(fConnection).PrepareCached(
      fSqlPrepared, fPreparedParamsCount, fPreparedStmtName);
    SqlLogEnd(' c=%', [fPreparedStmtName]);
  end
  else
    SqlLogEnd;
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
    raise ESqlDBPostgres.CreateUtf8(
      '%.ExecutePrepared: Statement not prepared', [self]);
  if fParamCount <> fPreparedParamsCount then
    raise ESqlDBPostgres.CreateUtf8('%.ExecutePrepared: Query expects % ' +
      'parameters but % bound', [self, fPreparedParamsCount, fParamCount]);
  inherited ExecutePrepared;
  BindParams;
  c := TSqlDBPostgresConnection(Connection);
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
  PQ.Check(c.fPGConn, fRes, @fRes, {forceClean=}false);
  fResStatus := PQ.ResultStatus(fRes);
  if fExpectResults then
  begin
    if fResStatus <> PGRES_TUPLES_OK then
    begin
      // paranoid check
      PQ.Clear(fRes);
      fRes := nil;
      raise ESqlDBPostgres.CreateUtf8('%.ExecutePrepared: result expected ' +
        'but statement did not return tuples', [self]);
    end;
    fTotalRowsRetrieved := PQ.ntuples(fRes);
    fCurrentRow := -1;
    if fColumn.Count = 0 then // if columns exist then statement is already cached
      BindColumns;
    SqlLogEnd(' c=% r=%', [fPreparedStmtName, fTotalRowsRetrieved]);
  end
  else
    SqlLogEnd(' c=%', [fPreparedStmtName]);
end;

procedure TSqlDBPostgresStatement.SendPipelinePrepared;
var
  c: TSqlDBPostgresConnection;
  res: integer;
begin
  SqlLogBegin(sllSQL);
  if fSqlPrepared = '' then
    raise ESqlDBPostgres.CreateUtf8(
      '%.ExecutePrepared: Statement not prepared', [self]);
  if fParamCount <> fPreparedParamsCount then
    raise ESqlDBPostgres.CreateUtf8('%.ExecutePrepared: Query expects % ' +
      'parameters but % bound', [self, fPreparedParamsCount, fParamCount]);
  inherited ExecutePrepared;
  BindParams;
  c := TSqlDBPostgresConnection(Connection);
  if fPreparedStmtName <> '' then
    res := PQ.sendQueryPrepared(c.fPGConn, pointer(fPreparedStmtName),
      fPreparedParamsCount, pointer(fPGParams), pointer(fPGParamLengths),
      pointer(fPGParamFormats), PGFMT_TEXT)
  else
    res := PQ.sendQueryParams(c.fPGConn, pointer(fSqlPrepared),
      fPreparedParamsCount, nil, pointer(fPGParams), pointer(fPGParamLengths),
      pointer(fPGParamFormats), PGFMT_TEXT);
  if res <> 1 then
    raise ESqlDBPostgres.CreateUtf8(
      '%.SendPrepared: %', [self, PQ.ErrorMessage(c.fPGConn)]);
  SqlLogEnd(' c=%', [fPreparedStmtName]);
end;

procedure TSqlDBPostgresStatement.GetPipelineResult;
var
  c: TSqlDBPostgresConnection;
  endRes: pointer;
begin
  SqlLogBegin(sllSQL);
  c := TSqlDBPostgresConnection(Connection);
  if fRes <> nil then
  begin
    PQ.Clear(fRes); // if forgot to call ReleaseRows
    fRes := nil;
  end;
  fRes := PQ.getResult(c.fPGConn);
  PQ.Check(c.fPGConn, fRes, @fRes, {forceClean=}false);
  fResStatus := PQ.ResultStatus(fRes);
  if fExpectResults then
  begin
    if fResStatus <> PGRES_TUPLES_OK then
    begin
      PQ.Clear(fRes);
      fRes := nil;
      raise ESqlDBPostgres.CreateUtf8('%.GetPipelineResult: result expected ' +
        'but statement did not return tuples', [self]);
    end;
    fTotalRowsRetrieved := PQ.ntuples(fRes);
    fCurrentRow := -1;
    if fColumn.Count = 0 then // if columns exist then statement is already cached
      BindColumns;
    SqlLogEnd(' c=% r=%', [fPreparedStmtName, fTotalRowsRetrieved]);
  end
  else
    SqlLogEnd(' c=%', [fPreparedStmtName]);

  endRes := PQ.getResult(c.fPGConn);
  if endRes <> nil then
    // NULL represent end of the result set
    raise ESqlDBPostgres.CreateUtf8(
      '%.GetPipelineResult: returned something extra', [self]);
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
    raise ESqlDBPostgres.CreateUtf8(
      '%.Execute should be called before Step', [self]);
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
    raise ESqlDBPostgres.CreateUtf8('%.ColumnToJson unexpected', [self]);
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
          // note: StrLen is slightly faster than PQ.GetLength for small content
          W.AddNoJsonEscape(P, StrLen(P));
        ftUtf8:
          if (ColumnAttr = JSONOID) or
             (ColumnAttr = JSONBOID) then
            W.AddNoJsonEscape(P, PQ.GetLength(fRes, fCurrentRow, Col))
          else
          begin
            W.Add('"');
            W.AddJsonEscape(P, 0); // Len=0 is faster than StrLen/GetLength
            W.Add('"');
          end;
        ftDate:
          begin
            W.Add('"');
            if (StrLen(P) > 10) and
               (PAnsiChar(P)[10] = ' ') then
              PAnsiChar(P)[10] := 'T'; // ensure strict ISO-8601 encoding
            W.AddJsonEscape(P);
            W.Add('"');
          end;
        ftBlob:
          if fForceBlobAsNull then
            W.AddNull
          else
            W.WrBase64(P, BlobInPlaceDecode(P,
              PQ.GetLength(fRes, fCurrentRow, Col)), {withmagic=}true);
      else
        raise ESqlDBPostgres.CreateUtf8('%.ColumnToJson: ColumnType=%?',
          [self, ord(ColumnType)]);
      end;
    end;
  end;
end;


initialization
  TSqlDBPostgresConnectionProperties.RegisterClassNameForDefinition;
  SetLength(_BindArrayJson, 1);
  _BindArrayJson[0] := 'json'; // impossible SQL value (should be 'json')

end.

