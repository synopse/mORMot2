/// Database Framework Direct SQlite3 Connnection
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.sql.sqlite3;

{
  *****************************************************************************

   Direct SQlite3 Client Access using our mormot.db.raw.sqlite3 Wrapper
    -  TSqlDBSQLite3Connection* and TSqlDBSQlite3Statement Classes

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
  mormot.core.datetime,
  mormot.core.data,
  mormot.core.json,
  mormot.core.rtti,
  mormot.core.perf,
  mormot.core.log,
  mormot.db.core,
  mormot.db.sql,
  mormot.db.raw.sqlite3;


{ ************ TSqlDBSQLite3Connection* and TSqlDBSQlite3Statement Classes }

type
  /// will implement properties shared by the SQLite3 engine
  TSqlDBSQLite3ConnectionProperties = class(TSqlDBConnectionProperties)
  private
    fUseMormotCollations: boolean;
    fExistingDB: TSqlDatabase;
    procedure SetUseMormotCollations(const Value: boolean);
    function GetMainDB: TSqlDataBase;
  protected
    /// initialize fForeignKeys content with all foreign keys of this DB
    // - used by GetForeignKey method
    procedure GetForeignKeys; override;
  public
    /// initialize access to a SQLite3 engine with some properties
    // - only used parameter is aServerName, which should point to the SQLite3
    // database file to be opened (one will be created if none exists)
    // - if specified, the password will be used to cypher this file on disk
    // (the main SQLite3 database file is encrypted, not the wal file during run);
    // the password may be a JSON-serialized TSynSignerParams object, or will use
    // AES-OFB-128 after SHAKE_128 with rounds=1000 and a fixed salt on plain password text
    // - other parameters (DataBaseName, UserID) are ignored
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUtf8);
      overload; override;
    /// initialize access to an existing SQLite3 engine
    // - this overloaded constructor allows to access via SynDB methods to an
    // existing SQLite3 database, e.g. TRestServerDB.DB (from mormot.orm.sqlite3.pas)
    constructor Create(aDB: TSqlDatabase); reintroduce; overload;
    /// create a new connection
    // - call this method if the shared MainConnection is not enough (e.g. for
    // multi-thread access)
    // - the caller is responsible of freeing this instance
    function NewConnection: TSqlDBConnection; override;
    /// direct access to the main SQlite3 DB instance
    // - can be used to tune directly the database properties
    property MainSQLite3DB: TSqlDataBase
      read GetMainDB;
  published
    /// TRUE if you want the SQL creation fields to use mORMot collation
    // - default value is TRUE for use within the mORMot framework, to use
    // dedicated UTF-8 collation and full Unicode support, and Iso8601 handling
    // - when set to FALSE, SqlCreate() method will return standard ASCII
    // SQLite collations for TEXT: it will make interaction with other programs
    // more compatible, at database file level
    property UseMormotCollations: boolean
      read fUseMormotCollations write SetUseMormotCollations;
  end;

  /// implements a direct connection to the SQLite3 engine
  TSqlDBSQLite3Connection = class(TSqlDBConnection)
  protected
    fDB: TSqlDataBase;
    function GetSynchronous: TSqlSynchronousMode;
    procedure SetSynchronous(Value: TSqlSynchronousMode);
    procedure SetLockingMode(Value: TSqlLockingMode);
    function GetLockingMode: TSqlLockingMode;
  public
    /// connect to the SQLite3 engine, i.e. create the DB instance
    // - should raise an Exception on error
    procedure Connect; override;
    /// stop connection to the SQLite3 engine, i.e. release the DB instance
    // - should raise an Exception on error
    procedure Disconnect; override;
    /// return TRUE if Connect has been already successfully called
    function IsConnected: boolean; override;
    /// initialize a new SQL query statement for the given connection
    // - the caller should free the instance after use
    function NewStatement: TSqlDBStatement; override;
    /// begin a Transaction for this connection
    // - current implementation do not support nested transaction with those
    // methods: exception will be raised in such case
    procedure StartTransaction; override;
    /// commit changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Commit; override;
    /// discard changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Rollback; override;
    /// the associated SQLite3 DB instance
    // - assigned to not nil after successfull connection
    property DB: TSqlDataBase
      read fDB;
    /// query or change the SQlite3 file-based syncrhonization mode, i.e. the
    // way it waits for the data to be flushed on hard drive
    // - default smFull is very slow, but achieve 100% ACID behavior
    // - smNormal is faster, and safe until a catastrophic hardware failure occurs
    // - smOff is the fastest, data should be safe if the application crashes,
    // but database file may be corrupted in case of failure at the wrong time
    property Synchronous: TSqlSynchronousMode
      read GetSynchronous write SetSynchronous;
    /// query or change the SQlite3 file-based locking mode, i.e. the
    // way it locks the file
    // - default lmNormal is ACID and safe
    // - lmExclusive gives better performance in case of a number of write
    // transactions, so can be used to release a mORMot server power: but you
    // won't be able to access the database file from outside the process (like
    // a "normal" database engine)
    property LockingMode: TSqlLockingMode
      read GetLockingMode write SetLockingMode;
  end;

  /// implements a statement using the SQLite3 engine
  TSqlDBSQLite3Statement = class(TSqlDBStatement)
  protected
    fStatement: TSqlRequest;
    fLogSQLValues: TVariantDynArray;
    fUpdateCount: integer;
    fShouldLogSQL: boolean; // sllSQL in SynDBLog.Level -> set fLogSQLValues[]
    // retrieve the inlined value of a given parameter, e.g. 1 or 'name'
    procedure AddParamValueAsText(Param: integer; Dest: TTextWriter;
      MaxCharCount: integer); override;
  public
    /// create a SQLite3 statement instance, from an existing SQLite3 connection
    // - the Execute method can be called once per TSqlDBSQLite3Statement instance,
    // but you can use the Prepare once followed by several ExecutePrepared methods
    // - if the supplied connection is not of TOleDBConnection type, will raise
    // an exception
    constructor Create(aConnection: TSqlDBConnection); override;
    /// release all associated memory and SQLite3 handles
    destructor Destroy; override;

    /// bind a NULL value to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindNull(Param: integer; IO: TSqlDBParamInOutType = paramIn;
      BoundType: TSqlDBFieldType = ftNull); override;
    /// bind an integer value to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure Bind(Param: integer; Value: Int64;
      IO: TSqlDBParamInOutType = paramIn); overload; override;
    /// bind a double value to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure Bind(Param: integer; Value: double;
      IO: TSqlDBParamInOutType = paramIn); overload; override;
    /// bind a TDateTime value to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindDateTime(Param: integer; Value: TDateTime;
      IO: TSqlDBParamInOutType = paramIn); overload; override;
    /// bind a currency value to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindCurrency(Param: integer; Value: currency;
      IO: TSqlDBParamInOutType = paramIn); overload; override;
    /// bind a UTF-8 encoded string to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindTextU(Param: integer; const Value: RawUtf8;
      IO: TSqlDBParamInOutType = paramIn); overload; override;
    /// bind a UTF-8 encoded buffer text (#0 ended) to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindTextP(Param: integer; Value: PUtf8Char;
      IO: TSqlDBParamInOutType = paramIn); overload; override;
    /// bind a UTF-8 encoded string to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindTextS(Param: integer; const Value: string;
      IO: TSqlDBParamInOutType = paramIn); overload; override;
    /// bind a UTF-8 encoded string to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindTextW(Param: integer; const Value: WideString;
      IO: TSqlDBParamInOutType = paramIn); overload; override;
    /// bind a Blob buffer to a  parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindBlob(Param: integer; Data: pointer; Size: integer;
      IO: TSqlDBParamInOutType = paramIn); overload; override;
    /// bind a Blob buffer to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindBlob(Param: integer; const Data: RawByteString;
      IO: TSqlDBParamInOutType = paramIn); overload; override;

    /// Prepare an UTF-8 encoded SQL statement
    // - parameters marked as ? will be bound later, before ExecutePrepared call
    // - if ExpectResults is TRUE, then Step() and Column*() methods are available
    // to retrieve the data rows
    // - raise an ESqlDBException on any error
    procedure Prepare(const aSql: RawUtf8; ExpectResults: boolean = false);
      overload; override;
    /// Execute a prepared SQL statement
    // - parameters marked as ? should have been already bound with Bind*() functions
    // - raise an ESqlDBException on any error
    procedure ExecutePrepared; override;
    /// gets a number of updates made by latest executed statement
    function UpdateCount: integer; override;

    /// After a statement has been prepared via Prepare() + ExecutePrepared() or
    // Execute(), this method must be called one or more times to evaluate it
    // - you shall call this method before calling any Column*() methods
    // - return TRUE on success, with data ready to be retrieved by Column*()
    // - return FALSE if no more row is available (e.g. if the SQL statement
    // is not a SELECT but an UPDATE or INSERT command)
    // - access the first or next row of data from the SQL Statement result:
    // if SeekFirst is TRUE, will put the cursor on the first row of results,
    // otherwise, it will fetch one row of data, to be called within a loop
    // - raise an ESqlite3Exception exception on any error
    function Step(SeekFirst: boolean = false): boolean; override;
    /// finalize the cursor
    procedure ReleaseRows; override;
    /// retrieve a column name of the current Row
    // - Columns numeration (i.e. Col value) starts with 0
    // - it's up to the implementation to ensure than all column names are unique
    function ColumnName(Col: integer): RawUtf8; override;
    /// returns the Column index of a given Column name
    // - Columns numeration (i.e. Col value) starts with 0
    // - returns -1 if the Column name is not found (via case insensitive search)
    function ColumnIndex(const aColumnName: RawUtf8): integer; override;
    /// the Column type of the current Row
    // - ftCurrency type should be handled specificaly, for faster process and
    // avoid any rounding issue, since currency is a standard OleDB type
    function ColumnType(Col: integer;
      FieldSize: PInteger = nil): TSqlDBFieldType; override;
    /// Reset the previous prepared statement
    procedure Reset; override;
    /// returns TRUE if the column contains NULL
    function ColumnNull(Col: integer): boolean; override;
    /// return a Column integer value of the current Row, first Col is 0
    function ColumnInt(Col: integer): Int64; override;
    /// return a Column floating point value of the current Row, first Col is 0
    function ColumnDouble(Col: integer): double; override;
    /// return a Column floating point value of the current Row, first Col is 0
    function ColumnDateTime(Col: integer): TDateTime; override;
    /// return a Column currency value of the current Row, first Col is 0
    // - should retrieve directly the 64 bit Currency content, to avoid
    // any rounding/conversion error from floating-point types
    function ColumnCurrency(Col: integer): currency; override;
    /// return a Column UTF-8 encoded text value of the current Row, first Col is 0
    function ColumnUtf8(Col: integer): RawUtf8; override;
    /// return a Column as a blob value of the current Row, first Col is 0
    // - ColumnBlob() will return the binary content of the field is was not ftBlob,
    // e.g. a 8 bytes RawByteString for a vtInt64/vtDouble/vtDate/vtCurrency,
    // or a direct mapping of the RawUnicode
    function ColumnBlob(Col: integer): RawByteString; override;
    /// append all columns values of the current Row to a JSON stream
    // - will use WR.Expand to guess the expected output format
    // - fast overridden implementation with no temporary variable
    // - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"
    // format and contains true BLOB data
    procedure ColumnsToJson(WR: TJsonWriter); override;
  end;


/// direct export of a DB statement rows into a SQLite3 database
// - the corresponding table will be created within the specified DB file
// - is a wrapper around TSqlDBConnection.NewTableFromRows()
function RowsToSqlite3(const Dest: TFileName; const TableName: RawUtf8;
  Rows: TSqlDBStatement; UseMormotCollations: boolean): integer;


implementation

{ ************ TSqlDBSQLite3Connection* and TSqlDBSQlite3Statement Classes }

function RowsToSqlite3(const Dest: TFileName; const TableName: RawUtf8;
  Rows: TSqlDBStatement; UseMormotCollations: boolean): integer;
var
  DB: TSqlDBSQLite3ConnectionProperties;
  Conn: TSqlDBSQLite3Connection;
begin
  result := 0;
  if (Dest = '') or
     (Rows = nil) or
     (Rows.ColumnCount = 0) then
    exit;
  // we do not call DeleteFile(Dest) since DB may be completed on purpose
  DB := TSqlDBSQLite3ConnectionProperties.Create(StringToUtf8(Dest), '', '', '');
  try
    DB.UseMormotCollations := UseMormotCollations;
    Conn := DB.MainConnection as TSqlDBSQLite3Connection;
    Conn.Connect;
    result := Conn.NewTableFromRows(TableName, Rows, true);
    Conn.Disconnect;
  finally
    DB.Free;
  end;
end;



{ TSqlDBSQLite3ConnectionProperties }

procedure TSqlDBSQLite3ConnectionProperties.SetUseMormotCollations(const Value: boolean);
const
  SQLITE3_FIELDS: array[boolean] of TSqlDBFieldTypeDefinition = (
   (' INTEGER',                       // ftUnknown = int32
    ' TEXT',                          // ftNull    = UTF-8
    ' INTEGER',                       // ftInt64
    ' FLOAT',                         // ftDouble
    ' FLOAT',                         // ftCurrency
    ' TEXT',                          // ftDate
    ' TEXT',                          // ftUtf8
    ' BLOB'),                         // ftBlob
   (' INTEGER',                       // ftUnknown = int32
    ' TEXT COLLATE SYSTEMNOCASE',     // ftNull    = UTF-8
    ' INTEGER',                       // ftInt64
    ' FLOAT',                         // ftDouble
    ' FLOAT',                         // ftCurrency
    ' TEXT COLLATE ISO8601',          // ftDate
    ' TEXT COLLATE SYSTEMNOCASE',     // ftUtf8
    ' BLOB'));                        // ftBlob
begin
  fUseMormotCollations := Value;
  fSqlCreateField := SQLITE3_FIELDS[Value];
end;

function TSqlDBSQLite3ConnectionProperties.GetMainDB: TSqlDataBase;
var
  conn: TSqlDBSQLite3Connection;
begin
  if self = nil then
    result := nil
  else if fExistingDB <> nil then
    result := fExistingDB
  else
  begin
    conn := MainConnection as TSqlDBSQLite3Connection;
    if not conn.IsConnected then
      conn.Connect; // we expect the SQLite3 instance to be created if needed
    result := conn.DB;
  end;
end;

constructor TSqlDBSQLite3ConnectionProperties.Create(const aServerName,
  aDatabaseName, aUserID, aPassWord: RawUtf8);
begin
  fDbms := dSQLite;
  inherited Create(aServerName, aDatabaseName, aUserID, aPassWord);
  UseMormotCollations := true;
end;

constructor TSqlDBSQLite3ConnectionProperties.Create(aDB: TSqlDatabase);
begin
  if aDB = nil then
    raise ESqlDBException.CreateUtf8('%.Create(DB=nil)', [self]);
  fExistingDB := aDB;
  Create('', StringToUtf8(aDB.FileName), '', aDB.Password);
end;

procedure TSqlDBSQLite3ConnectionProperties.GetForeignKeys;
begin
  // do nothing (yet)
end;

function TSqlDBSQLite3ConnectionProperties.NewConnection: TSqlDBConnection;
begin
  result := TSqlDBSQLite3Connection.Create(self);
end;


{ TSqlDBSQLite3Connection }

procedure TSqlDBSQLite3Connection.Commit;
begin
  inherited Commit;
  try
    fDB.Commit;
  except
    inc(fTransactionCount); // the transaction is still active
    raise;
  end;
end;

procedure TSqlDBSQLite3Connection.Connect;
var
  {%H-}log: ISynLog;
begin
  log := SynDBLog.Enter;
  Disconnect; // force fTrans=fError=fServer=fContext=nil
  fDB := (Properties as TSqlDBSQLite3ConnectionProperties).fExistingDB;
  if fDB = nil then
    fDB := TSqlDatabase.Create(Utf8ToString(Properties.ServerName), Properties.PassWord);
  //fDB.SetWalMode(true); // slower INSERT in WAL mode for huge number of rows
  inherited Connect; // notify any re-connection
end;

procedure TSqlDBSQLite3Connection.Disconnect;
begin
  inherited Disconnect; // flush any cached statement
  if (Properties as TSqlDBSQLite3ConnectionProperties).fExistingDB = fDB then
    fDB := nil
  else
    FreeAndNil(fDB);
end;

function TSqlDBSQLite3Connection.GetLockingMode: TSqlLockingMode;
begin
  if IsConnected then
    result := fDB.LockingMode
  else
    result := lmNormal;
end;

function TSqlDBSQLite3Connection.GetSynchronous: TSqlSynchronousMode;
begin
  if IsConnected then
    result := fDB.Synchronous
  else
    result := smFull;
end;

function TSqlDBSQLite3Connection.IsConnected: boolean;
begin
  result := fDB <> nil;
end;

function TSqlDBSQLite3Connection.NewStatement: TSqlDBStatement;
begin
  result := TSqlDBSQLite3Statement.Create(self);
end;

procedure TSqlDBSQLite3Connection.Rollback;
begin
  inherited;
  fDB.RollBack;
end;

procedure TSqlDBSQLite3Connection.SetLockingMode(Value: TSqlLockingMode);
begin
  if self = nil then
    exit;
  if fDB = nil then
    Connect;
  fDB.LockingMode := Value;
end;

procedure TSqlDBSQLite3Connection.SetSynchronous(Value: TSqlSynchronousMode);
begin
  if self = nil then
    exit;
  if fDB = nil then
    Connect;
  fDB.Synchronous := Value;
end;

procedure TSqlDBSQLite3Connection.StartTransaction;
begin
  inherited;
  fDB.TransactionBegin;
end;


{ TSqlDBSQLite3Statement }

procedure TSqlDBSQLite3Statement.Bind(Param: integer; Value: double;
  IO: TSqlDBParamInOutType);
begin
  if fShouldLogSQL and
     (cardinal(Param - 1) < cardinal(length(fLogSQLValues))) then
    fLogSQLValues[Param - 1] := Value;
  fStatement.Bind(Param, Value);
end;

procedure TSqlDBSQLite3Statement.Bind(Param: integer; Value: Int64;
  IO: TSqlDBParamInOutType);
begin
  if fShouldLogSQL and
     (cardinal(Param - 1) < cardinal(length(fLogSQLValues))) then
    fLogSQLValues[Param - 1] := Value;
  fStatement.Bind(Param, Value);
end;

procedure TSqlDBSQLite3Statement.BindBlob(Param: integer; Data: pointer;
  Size: integer; IO: TSqlDBParamInOutType);
begin
  if fShouldLogSQL and
     (cardinal(Param - 1) < cardinal(length(fLogSQLValues))) then
    fLogSQLValues[Param - 1] := Size;
  fStatement.Bind(Param, Data, Size);
end;

procedure TSqlDBSQLite3Statement.BindBlob(Param: integer; const Data:
  RawByteString; IO: TSqlDBParamInOutType);
begin
  if fShouldLogSQL and
     (cardinal(Param - 1) < cardinal(length(fLogSQLValues))) then
    fLogSQLValues[Param - 1] := length(Data);
  fStatement.BindBlob(Param, Data);
end;

procedure TSqlDBSQLite3Statement.BindCurrency(Param: integer; Value: currency;
  IO: TSqlDBParamInOutType);
begin
  if fShouldLogSQL and
     (cardinal(Param - 1) < cardinal(length(fLogSQLValues))) then
    fLogSQLValues[Param - 1] := Value;
  fStatement.Bind(Param, Value);
end;

procedure TSqlDBSQLite3Statement.BindDateTime(Param: integer; Value: TDateTime;
  IO: TSqlDBParamInOutType);
begin
  // see http://www.sqlite.org/lang_datefunc.html
  BindTextU(Param, DateTimeToIso8601Text(Value, 'T'));
end;

procedure TSqlDBSQLite3Statement.BindNull(Param: integer;
  IO: TSqlDBParamInOutType; BoundType: TSqlDBFieldType);
begin
  fStatement.BindNull(Param);
end;

procedure TSqlDBSQLite3Statement.BindTextP(Param: integer; Value: PUtf8Char;
  IO: TSqlDBParamInOutType);
var
  V: RawUtf8;
begin
  FastSetString(V, Value, StrLen(Value));
  BindTextU(Param, V);
end;

procedure TSqlDBSQLite3Statement.BindTextS(Param: integer; const Value: string;
  IO: TSqlDBParamInOutType);
begin
  BindTextU(Param, StringToUtf8(Value));
end;

procedure TSqlDBSQLite3Statement.BindTextU(Param: integer; const Value: RawUtf8;
  IO: TSqlDBParamInOutType);
begin
  if fShouldLogSQL and
     (cardinal(Param - 1) < cardinal(length(fLogSQLValues))) then
    RawUtf8ToVariant(Value, fLogSQLValues[Param - 1]);
  fStatement.Bind(Param, Value);
end;

procedure TSqlDBSQLite3Statement.BindTextW(Param: integer;
  const Value: WideString; IO: TSqlDBParamInOutType);
begin
  BindTextU(Param, WideStringToUtf8(Value));
end;

function TSqlDBSQLite3Statement.ColumnBlob(Col: integer): RawByteString;
begin
  result := fStatement.FieldBlob(Col);
end;

function TSqlDBSQLite3Statement.ColumnCurrency(Col: integer): currency;
begin
  result := fStatement.FieldDouble(Col);
end;

function TSqlDBSQLite3Statement.ColumnDateTime(Col: integer): TDateTime;
begin
  case ColumnType(Col) of
    ftUtf8:
      result := Iso8601ToDateTime(fStatement.FieldUtf8(Col));
    ftInt64:
      result := TimeLogToDateTime(fStatement.FieldInt(Col));
  else
    result := 0;
  end;
end;

function TSqlDBSQLite3Statement.ColumnDouble(Col: integer): double;
begin
  result := fStatement.FieldDouble(Col);
end;

function TSqlDBSQLite3Statement.ColumnIndex(const aColumnName: RawUtf8): integer;
begin
  result := fStatement.FieldIndex(aColumnName);
end;

function TSqlDBSQLite3Statement.ColumnInt(Col: integer): Int64;
begin
  result := fStatement.FieldInt(Col);
end;

function TSqlDBSQLite3Statement.ColumnName(Col: integer): RawUtf8;
begin
  result := fStatement.FieldName(Col);
end;

function TSqlDBSQLite3Statement.ColumnNull(Col: integer): boolean;
begin
  result := fStatement.FieldNull(Col);
end;

procedure TSqlDBSQLite3Statement.ColumnsToJson(WR: TJsonWriter);
begin
  fStatement.FieldsToJson(WR, fForceBlobAsNull);
end;

function TSqlDBSQLite3Statement.ColumnType(Col: integer;
  FieldSize: PInteger): TSqlDBFieldType;
begin
  if fCurrentRow <= 0 then
    // before any TSqlDBSQLite3Statement.Step call
    result := fConnection.Properties.ColumnTypeNativeToDB(
      fStatement.FieldDeclaredType(Col), 8)
  else
    case fStatement.FieldType(Col) of
      SQLITE_NULL:
        result := ftNull;
      SQLITE_INTEGER:
        result := ftInt64;
      SQLITE_FLOAT:
        result := ftDouble;
      SQLITE_TEXT:
        result := ftUtf8;
      SQLITE_BLOB:
        result := ftBlob;
    else
      result := ftUnknown;
    end;
  if FieldSize <> nil then
    FieldSize^ := 0; // no column size in SQLite3
end;

function TSqlDBSQLite3Statement.ColumnUtf8(Col: integer): RawUtf8;
begin
  result := fStatement.FieldUtf8(Col);
end;

constructor TSqlDBSQLite3Statement.Create(aConnection: TSqlDBConnection);
begin
  if not aConnection.InheritsFrom(TSqlDBSQLite3Connection) then
    raise ESqlDBException.CreateUtf8('%.Create(%)', [self, aConnection]);
  inherited Create(aConnection);
  if (SynDBLog <> nil) and
     (sllSQL in SynDBLog.Family.Level) then
    fShouldLogSQL := true;
end;

destructor TSqlDBSQLite3Statement.Destroy;
begin
  try
    fStatement.Close; // release statement
  finally
    inherited Destroy;
  end;
end;

procedure TSqlDBSQLite3Statement.ExecutePrepared;
var
  DB: TSqlDataBase;
begin
  fCurrentRow := 0; // mark cursor on the first row
  inherited ExecutePrepared; // set fConnection.fLastAccessTicks
  DB := TSqlDBSQLite3Connection(Connection).DB;
  if fExpectResults then
    exit; // execution done in Step()
  if fShouldLogSQL then
    SqlLogBegin(sllSQL);
  try
    // INSERT/UPDATE/DELETE (i.e. not SELECT) -> try to execute directly now
    repeat // Execute all steps of the first statement
    until fStatement.Step <> SQLITE_ROW;
    fUpdateCount := DB.LastChangeCount;
  finally
    if fShouldLogSQL then
      SqlLogEnd;
  end;
end;

function TSqlDBSQLite3Statement.UpdateCount: integer;
begin
  result := fUpdateCount;
end;

procedure TSqlDBSQLite3Statement.AddParamValueAsText(Param: integer;
  Dest: TTextWriter; MaxCharCount: integer);
var
  v: PVarData;
begin
  dec(Param);
  if fShouldLogSQL and
     (cardinal(Param) < cardinal(length(fLogSQLValues))) then
  begin
    v := @fLogSQLValues[Param];
    if v^.vtype = varString then
      Dest.AddQuotedStr(v^.VAny, length(RawUtf8(v^.VAny)), '''', MaxCharCount)
    else
      Dest.AddVariant(PVariant(v)^);
  end;
end;

procedure TSqlDBSQLite3Statement.Prepare(const aSql: RawUtf8; ExpectResults: boolean);
begin
  if fShouldLogSQL then
    SqlLogBegin(sllDB);
  inherited Prepare(aSql, ExpectResults); // set fSql + Connect if necessary
  fStatement.Prepare(TSqlDBSQLite3Connection(Connection).fDB.DB, aSql);
  fColumnCount := fStatement.FieldCount;
  if fShouldLogSQL then
  begin
    fParamCount := fStatement.ParamCount;
    SetLength(fLogSQLValues, fParamCount);
    SqlLogEnd(' %', [TSqlDBSQLite3Connection(Connection).fDB.FileNameWithoutPath]);
  end;
end;

procedure TSqlDBSQLite3Statement.Reset;
begin
  fStatement.Reset; // should be done now
  fUpdateCount := 0;
  // fStatement.BindReset; // slow down the process, and is not mandatory
  ReleaseRows;
  if fShouldLogSQL then
    SetLength(fLogSQLValues, fParamCount);
  inherited Reset;
end;

procedure TSqlDBSQLite3Statement.ReleaseRows;
begin
  if fShouldLogSQL then
    VariantDynArrayClear(fLogSQLValues);
  inherited ReleaseRows;
end;

function TSqlDBSQLite3Statement.Step(SeekFirst: boolean): boolean;
begin
  if SeekFirst then
  begin
    if fCurrentRow > 0 then
      raise ESqlDBException.CreateUtf8('%.Step(SeekFirst=true) not implemented', [self]);
    fCurrentRow := 0;
    //fStatement.Reset;
  end;
  try
    result := fStatement.Step = SQLITE_ROW;
  except
    on E: Exception do
    begin
      if fShouldLogSQL then
        SynDBLog.Add.Log(sllError, 'Error % on % for [%] as [%]', [E,
          TSqlDBSQLite3Connection(Connection).DB.FileNameWithoutPath, SQL,
          SqlWithInlinedParams], self);
      raise;
    end;
  end;
  if result then
  begin
    inc(fTotalRowsRetrieved);
    inc(fCurrentRow);
  end
  else
    fCurrentRow := 0;
end;


initialization
  TSqlDBSQLite3ConnectionProperties.RegisterClassNameForDefinition;

end.

