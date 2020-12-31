/// Database Framework Direct ODBC Connection
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.sql.odbc;

{
  *****************************************************************************

   Efficient SQL Database Connection via ODBC 
    -  TSqlDBODBCConnection* and TSqlDBODBCStatement Classes

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
  mormot.core.perf,
  mormot.core.log,
  mormot.db.core,
  mormot.db.sql;


{ ************  TSqlDBODBCConnection* and TSqlDBODBCStatement Classes }

type
  /// will implement properties shared by the ODBC library
  TSqlDBODBCConnectionProperties = class(TSqlDBConnectionPropertiesThreadSafe)
  protected
    fDriverDoesNotHandleUnicode: boolean;
    fSQLDriverConnectPrompt: boolean;
    /// this overridden method will hide de DATABASE/PWD fields in ODBC connection string
    function GetDatabaseNameSafe: RawUtf8; override;
    /// this overridden method will retrieve the kind of DBMS from the main connection
    function GetDBMS: TSqlDBDefinition; override;
  public
    /// initialize the connection properties
    // - will raise an exception if the ODBC library is not available
    // - SQLConnect() API will be used if aServerName is set: it should contain
    // the ODBC Data source name as defined in "ODBC Data Source Administrator"
    // tool (C:\Windows\SysWOW64\odbcad32.exe for 32bit app on Win64) - in this
    // case, aDatabaseName will be ignored
    // - SQLDriverConnect() API will be used if aServerName is '' and
    // aDatabaseName is set - in this case, aDatabaseName should contain a
    // full connection string like (e.g. for a local SQLEXPRESS instance):
    // ! 'DRIVER=SQL Server Native Client 10.0;UID=.;server=.\SQLEXPRESS;'+
    // !   'Trusted_Connection=Yes;MARS_Connection=yes'
    // see @http://msdn.microsoft.com/en-us/library/ms715433
    // or when using Firebird ODBC:
    // ! 'DRIVER=Firebird/InterBase(r) driver;CHARSET=UTF8;UID=SYSDBA;PWD=masterkey;'
    // !   'DBNAME=MyServer/3051:C:\database\myData.fdb'
    // ! 'DRIVER=Firebird/InterBase(r) driver;CHARSET=UTF8;DBNAME=dbfile.fdb;'+
    // !   'CLIENT=fbembed.dll'
    // for IBM DB2 and its official driver:
    // !  'Driver=IBM DB2 ODBC DRIVER;Database=SAMPLE;'+
    // !    'Hostname=localhost;Port=50000;UID=db2admin;Pwd=db2Password'
    // for PostgreSQL - driver from http://ftp.postgresql.org/pub/odbc/versions/msi
    // ! 'Driver=PostgreSQL Unicode;Database=postgres;'+
    // !   'Server=localhost;Port=5432;UID=postgres;Pwd=postgresPassword'
    // for MySQL - driver from https://dev.mysql.com/downloads/connector/odbc
    // (note: 5.2.6 and 5.3.1 driver seems to be slow in ODBC.FreeHandle)
    // ! 'Driver=MySQL ODBC 5.2 UNICODE Driver;Database=test;'+
    // !   'Server=localhost;Port=3306;UID=root;Pwd='
    // for IBM Informix and its official driver:
    // ! 'Driver=IBM INFORMIX ODBC DRIVER;Database=SAMPLE;'+
    // !   'Host=localhost;Server=<instance name on host>;Service=<service name
    // !   in ../drivers/etc/services>;Protocol=olsoctcp;UID=<Windows/Linux user account>;
    // !   Pwd=<Windows/Linux user account password>'
    constructor Create(const aServerName, aDatabaseName,
      aUserID, aPassWord: RawUtf8); override;
    /// create a new connection
    // - call this method if the shared MainConnection is not enough (e.g. for
    // multi-thread access)
    // - the caller is responsible of freeing this instance
    // - this overridden method will create an TSqlDBODBCConnection instance
    function NewConnection: TSqlDBConnection; override;
    /// get all table names
    // - will retrieve the corresponding metadata from ODBC library if SQL
    // direct access was not defined
    procedure GetTableNames(out Tables: TRawUtf8DynArray); override;
    /// get all view names
    // - will retrieve the corresponding metadata from ODBC library if SQL
    // direct access was not defined
    procedure GetViewNames(out Views: TRawUtf8DynArray); override;
    /// retrieve the column/field layout of a specified table
    // - will also check if the columns are indexed
    // - will retrieve the corresponding metadata from ODBC library if SQL
    // direct access was not defined (e.g. for dDB2)
    procedure GetFields(const aTableName: RawUtf8;
      out Fields: TSqlDBColumnDefineDynArray); override;
    /// initialize fForeignKeys content with all foreign keys of this DB
    // - used by GetForeignKey method
    procedure GetForeignKeys; override;
    /// retrieve a list of stored procedure names from current connection
    procedure GetProcedureNames(out Procedures: TRawUtf8DynArray); override;
    /// retrieve procedure input/output parameter information
    // - aProcName: stored procedure name to retrieve parameter infomation.
    // - Parameters: parameter list info (name, datatype, direction, default)
    procedure GetProcedureParameters(const aProcName: RawUtf8;
      out Parameters: TSqlDBProcColumnDefineDynArray); override;
    /// if full connection string may prompt the user for additional information
    // - property used only with SQLDriverConnect() API (i.e. when aServerName
    // is '' and aDatabaseName contains a full connection string)
    // - set to TRUE to allow UI prompt if needed
    property SQLDriverConnectPrompt: boolean
      read fSQLDriverConnectPrompt write fSQLDriverConnectPrompt;
  end;

  /// implements a direct connection to the ODBC library
  TSqlDBODBCConnection = class(TSqlDBConnectionThreadSafe)
  protected
    fODBCProperties: TSqlDBODBCConnectionProperties;
    fEnv: pointer;
    fDbc: pointer;
    fDBMS: TSqlDBDefinition;
    fDBMSName, fDriverName, fDBMSVersion, fSQLDriverFullString: RawUtf8;
  public
    /// connect to a specified ODBC database
    constructor Create(aProperties: TSqlDBConnectionProperties); override;
    /// release memory and connection
    destructor Destroy; override;
    /// connect to the ODBC library, i.e. create the DB instance
    // - should raise an Exception on error
    procedure Connect; override;
    /// stop connection to the ODBC library, i.e. release the DB instance
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

    /// the remote DBMS type, as retrieved at ODBC connection opening
    property DBMS: TSqlDBDefinition
      read fDBMS;
    /// the full connection string (expanded from ServerName)
    property SQLDriverFullString: RawUtf8
      read fSQLDriverFullString;
  published
    /// the remote DBMS name, as retrieved at ODBC connection opening
    property DBMSName: RawUtf8
      read fDBMSName;
    /// the remote DBMS version, as retrieved at ODBC connection opening
    property DBMSVersion: RawUtf8
      read fDBMSVersion;
    /// the local driver name, as retrieved at ODBC connection opening
    property DriverName: RawUtf8
      read fDriverName;
  end;

  /// implements a statement using a ODBC connection
  TSqlDBODBCStatement = class(TSqlDBStatementWithParamsAndColumns)
  protected
    fStatement: pointer;
    fColData: TRawByteStringDynArray;
    fSQLW: RawUnicode;
    procedure AllocStatement;
    procedure DeallocStatement;
    procedure BindColumns;
    procedure GetData(var Col: TSqlDBColumnProperty; ColIndex: integer);
    function GetCol(Col: integer; ExpectedType: TSqlDBFieldType): TSqlDBStatementGetCol;
    function MoreResults: boolean;
  public
    /// create a ODBC statement instance, from an existing ODBC connection
    // - the Execute method can be called once per TSqlDBODBCStatement instance,
    //   but you can use the Prepare once followed by several ExecutePrepared methods
    // - if the supplied connection is not of TOleDBConnection type, will raise
    //   an exception
    constructor Create(aConnection: TSqlDBConnection); override;
    // release all associated memory and ODBC handles
    destructor Destroy; override;

    /// Prepare an UTF-8 encoded SQL statement
    // - parameters marked as ? will be bound later, before ExecutePrepared call
    // - if ExpectResults is TRUE, then Step() and Column*() methods are available
    //   to retrieve the data rows
    // - raise an EODBCException or ESqlDBException on any error
    procedure Prepare(const aSQL: RawUtf8; ExpectResults: boolean = false);
      overload; override;
    /// Execute a prepared SQL statement
    // - parameters marked as ? should have been already bound with Bind*() functions
    // - this overridden method will log the SQL statement if sllSQL has been
    //   enabled in SynDBLog.Family.Level
    // - raise an EODBCException or ESqlDBException on any error
    procedure ExecutePrepared; override;
    /// Reset the previous prepared statement
    // - this overridden implementation will reset all bindings and the cursor state
    // - raise an EODBCException on any error
    procedure Reset; override;

    /// After a statement has been prepared via Prepare() + ExecutePrepared() or
    //   Execute(), this method must be called one or more times to evaluate it
    // - you shall call this method before calling any Column*() methods
    // - return TRUE on success, with data ready to be retrieved by Column*()
    // - return FALSE if no more row is available (e.g. if the SQL statement
    //  is not a SELECT but an UPDATE or INSERT command)
    // - access the first or next row of data from the SQL Statement result:
    //   if SeekFirst is TRUE, will put the cursor on the first row of results,
    //   otherwise, it will fetch one row of data, to be called within a loop
    // - raise an EODBCException or ESqlDBException exception on any error
    function Step(SeekFirst: boolean = false): boolean; override;
    /// close the ODBC statement cursor resources
    procedure ReleaseRows; override;
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
    //  e.g. a 8 bytes RawByteString for a vtInt64/vtDouble/vtDate/vtCurrency,
    //  or a direct mapping of the RawUnicode
    function ColumnBlob(Col: integer): RawByteString; override;
    /// append all columns values of the current Row to a JSON stream
    // - will use WR.Expand to guess the expected output format
    // - fast overridden implementation with no temporary variable
    // - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"
    // format and contains true BLOB data
    procedure ColumnsToJson(WR: TJsonWriter); override;
    /// returns the number of rows updated by the execution of this statement
    function UpdateCount: integer; override;
  end;


{$ifndef PUREMORMOT2}
// backward compatibility types redirections

type
  TODBCConnectionProperties = TSqlDBODBCConnectionProperties;
  TODBCConnection = TSqlDBODBCConnection;
  TODBCStatement = TSqlDBODBCStatement;

{$endif PUREMORMOT2}


implementation

uses
  mormot.db.raw.odbc; // define raw ODBC library API


{ ************  TSqlDBODBCConnection* and TSqlDBODBCStatement Classes }

{ TSqlDBODBCConnection }

procedure TSqlDBODBCConnection.Connect;
const
  DBMS_NAMES: array[0..8] of PAnsiChar = (
    'ORACLE', 'MICROSOFT SQL', 'ACCESS', 'MYSQL', 'SQLITE',
    'FIREBIRD', 'INTERBASE', 'POSTGRE', 'INFORMIX');
  DBMS_TYPES: array[-1..high(DBMS_NAMES)] of TSqlDBDefinition = (
    dDefault, dOracle, dMSSQL, dJet, dMySQL, dSQLite,
    dFirebird, dFirebird, dPostgreSql, dInformix);
  DRIVER_NAMES: array[0..21] of PAnsiChar = (
    'SQLSRV', 'LIBTDSODBC', 'IVSS', 'IVMSSS', 'PBSS', 'DB2CLI', 'LIBDB2',
    'IVDB2', 'PBDB2', 'MSDB2', 'CWBODBC', 'MYODBC', 'SQORA', 'MSORCL', 'PBOR',
    'IVOR', 'ODBCFB', 'IB', 'SQLITE', 'PSQLODBC', 'NXODBCDRIVER', 'ICLIT09B');
  DRIVER_TYPES: array[-1..high(DRIVER_NAMES)] of TSqlDBDefinition = (
    dDefault, dMSSQL, dMSSQL, dMSSQL, dMSSQL, dMSSQL, dDB2, dDB2, dDB2, dDB2,
    dDB2, dDB2, dMySQL, dOracle, dOracle, dOracle, dOracle, dFirebird,
    dFirebird, dSQLite, dPostgreSQL, dNexusDB, dInformix);
  DRIVERCOMPLETION: array[boolean] of SqlUSmallint = (
    SQL_DRIVER_NOPROMPT, SQL_DRIVER_PROMPT);
var
  Log: ISynLog;
  Len: SqlSmallint;
begin
  Log := SynDBLog.Enter(self, 'Connect');
  Disconnect; // force fDbc=nil
  if fEnv = nil then
    if (ODBC = nil) or
       (ODBC.AllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, fEnv) = SQL_ERROR) then
      raise EODBCException.CreateUtf8('%: Unable to allocate an environment handle', [self]);
  with ODBC do
  try
    // connect
    Check(self, nil,
      SetEnvAttr(fEnv, SQL_ATTR_ODBC_VERSION, SQL_OV_ODBC3, 0),
      SQL_HANDLE_ENV, fEnv);
    Check(self, nil,
      AllocHandle(SQL_HANDLE_DBC, fEnv, fDbc),
      SQL_HANDLE_ENV, fEnv);
    with fODBCProperties do
      if fServerName <> '' then
        Check(self, nil,
          ConnectA(fDbc, pointer(fServerName), length(fServerName),
          pointer(fUserID), length(fUserID), pointer(fPassWord), length(fPassWord)),
        SQL_HANDLE_DBC, fDbc)
      else if fDatabaseName = '' then
        raise EODBCException.Create(
          'Need ServerName=DataSourceName or DataBaseName=FullConnectString')
      else
      begin
        SetString(fSQLDriverFullString, nil, 1024);
        fSQLDriverFullString[1] := #0;
        Len := 0;
        Check(self, nil,
          SQLDriverConnectA(fDbc, GetDesktopWindow, Pointer(fDatabaseName),
            length(fDatabaseName), pointer(fSQLDriverFullString), length(fSQLDriverFullString),
            Len, DRIVERCOMPLETION[fODBCProperties.fSQLDriverConnectPrompt]),
          SQL_HANDLE_DBC, fDbc);
        SetLength(fSQLDriverFullString, Len);
      end;
    // retrieve information of the just created connection
    GetInfoString(fDbc, SQL_DRIVER_NAME, fDriverName);
    GetInfoString(fDbc, SQL_DBMS_NAME, fDBMSName);
    GetInfoString(fDbc, SQL_DBMS_VER, fDBMSVersion);
    // guess DBMS type from driver name or DBMS name
    fDBMS := DRIVER_TYPES[IdemPCharArray(pointer(fDriverName), DRIVER_NAMES)];
    if fDBMS = dDefault then
      fDBMS := DBMS_TYPES[IdemPCharArray(pointer(fDBMSName), DBMS_NAMES)];
    if fDBMS = dDefault then
      raise EODBCException.CreateUtf8(
        '%.Connect: unrecognized provider DBMSName=% DriverName=% DBMSVersion=%',
        [self, DBMSName, DriverName, DBMSVersion]);
    if Log <> nil then
      Log.Log(sllDebug, 'Connected to % using % % recognized as %', [DBMSName,
        DriverName, DBMSVersion, fProperties.DBMSEngineName]);
    // notify any re-connection
    inherited Connect;
  except
    on E: Exception do
    begin
      self.Disconnect; // clean up on fail
      raise;
    end;
  end;
end;

constructor TSqlDBODBCConnection.Create(aProperties: TSqlDBConnectionProperties);
var
  Log: ISynLog;
begin
  Log := SynDBLog.Enter(self, 'Create');
  if not aProperties.InheritsFrom(TSqlDBODBCConnectionProperties) then
    raise EODBCException.CreateUtf8('Invalid %.Create(%)', [self, aProperties]);
  fODBCProperties := TSqlDBODBCConnectionProperties(aProperties);
  inherited Create(aProperties);
end;

destructor TSqlDBODBCConnection.Destroy;
begin
  inherited Destroy;
  if (ODBC <> nil) and
     (fEnv <> nil) then
    ODBC.FreeHandle(SQL_HANDLE_ENV, fEnv);
end;

procedure TSqlDBODBCConnection.Disconnect;
var
  log: ISynLog;
begin
  try
    inherited Disconnect; // flush any cached statement
  finally
    if (ODBC <> nil) and
       (fDbc <> nil) then
      with ODBC do
      begin
        log := SynDBLog.Enter(self, 'Disconnect');
        Disconnect(fDbc);
        FreeHandle(SQL_HANDLE_DBC, fDbc);
        fDbc := nil;
      end;
  end;
end;

function TSqlDBODBCConnection.IsConnected: boolean;
begin
  result := fDbc <> nil;
end;

function TSqlDBODBCConnection.NewStatement: TSqlDBStatement;
begin
  result := TSqlDBODBCStatement.Create(self);
end;

procedure TSqlDBODBCConnection.Commit;
begin
  inherited Commit; // dec(fTransactionCount)
  with ODBC do
  try
    Check(self, nil,
      EndTran(SQL_HANDLE_DBC, fDBc, SQL_COMMIT),
      SQL_HANDLE_DBC, fDBc);
    Check(self, nil,
      SetConnectAttrW(fDBc, SQL_AUTOCOMMIT, SQL_AUTOCOMMIT_ON, 0),
      SQL_HANDLE_DBC, fDBc); // back to default AUTO COMMIT ON mode
  except
    inc(fTransactionCount); // the transaction is still active
    raise;
  end;
end;

procedure TSqlDBODBCConnection.Rollback;
begin
  inherited RollBack;
  with ODBC do
  begin
    Check(self, nil,
      EndTran(SQL_HANDLE_DBC, fDBc, SQL_ROLLBACK),
      SQL_HANDLE_DBC, fDBc);
    Check(self, nil,
      SetConnectAttrW(fDBc, SQL_AUTOCOMMIT, SQL_AUTOCOMMIT_ON, 0),
      SQL_HANDLE_DBC, fDBc); // back to default AUTO COMMIT ON mode
  end;
end;

procedure TSqlDBODBCConnection.StartTransaction;
var
  log: ISynLog;
begin
  log := SynDBLog.Enter(self, 'StartTransaction');
  if TransactionCount > 0 then
    raise EODBCException.CreateUtf8('% do not support nested transactions', [self]);
  inherited StartTransaction;
  ODBC.Check(self, nil,
    ODBC.SetConnectAttrW(fDBc, SQL_AUTOCOMMIT, SQL_AUTOCOMMIT_OFF, 0),
    SQL_HANDLE_DBC, fDBc);
end;


{ TSqlDBODBCStatement }

procedure TSqlDBODBCStatement.AllocStatement;
var
  hDbc: SqlHDbc;
begin
  if fStatement <> nil then
    raise EODBCException.CreateUtf8('%.AllocStatement called twice', [self]);
  fCurrentRow := 0;
  fTotalRowsRetrieved := 0;
  if not fConnection.Connected then
    fConnection.Connect;
  hDbc := (fConnection as TSqlDBODBCConnection).fDbc;
  with ODBC do
    Check(nil, self,
      AllocHandle(SQL_HANDLE_STMT, hDbc, fStatement),
      SQL_HANDLE_DBC, hDbc);
end;

procedure TSqlDBODBCStatement.DeallocStatement;
begin
  if fStatement <> nil then
    // avoid Informix exception and log exception race condition
  try
    try
      ODBC.Check(nil, self,
        ODBC.FreeHandle(SQL_HANDLE_STMT, fStatement),
        SQL_HANDLE_DBC, (fConnection as TSqlDBODBCConnection).fDbc);
    except
    end;
  finally
    fStatement := Nil;
  end;
end;

function ODBCColumnToFieldType(DataType, ColumnPrecision, ColumnScale: integer):
  TSqlDBFieldType;
begin // ftUnknown, ftNull, ftInt64, ftDouble, ftCurrency, ftDate, ftUtf8, ftBlob
  case DataType of
    SQL_DECIMAL, SQL_NUMERIC, SQL_FLOAT:
      begin
        result := ftDouble;
        if ColumnPrecision = 10 then
          case ColumnScale of
            0:
              result := ftInt64;
            1..4:
              result := ftCurrency;
          end;
      end;
    SQL_REAL, SQL_DOUBLE:
      result := ftDouble;
    SQL_SMALLINT, SQL_INTEGER, SQL_TINYINT, SQL_BIT, SQL_BIGINT:
      result := ftInt64;
    SQL_BINARY, SQL_VARBINARY, SQL_LONGVARBINARY:
      result := ftBlob;
    SQL_TIME, SQL_DATETIME, SQL_TYPE_DATE, SQL_TYPE_TIME, SQL_TYPE_TIMESTAMP:
      result := ftDate;
  else // all other types will be converted to text
    result := ftUtf8;
  end;
end;

const
  /// internal mapping to handled GetData() type for Column*() methods
  // - numerical values (integer or floating-point) are converted to SQL_C_CHAR
  // - date/time to SQL_C_TYPE_TIMESTAMP object
  // - text columns to SQL_C_WCHAR (for proper UTF-8 data retrieval)
  // - BLOB columns to SQL_C_BINARY
  ODBC_TYPE_TOC: array[TSqlDBFieldType] of ShortInt = (
    SQL_C_CHAR, SQL_C_CHAR, SQL_C_CHAR, SQL_C_CHAR, SQL_C_CHAR,
    SQL_C_TYPE_TIMESTAMP, SQL_C_WCHAR, SQL_C_BINARY);
   // ftUnknown, ftNull, ftInt64, ftDouble, ftCurrency, ftDate, ftUtf8, ftBlob

procedure TSqlDBODBCStatement.BindColumns;
var
  nCols, NameLength, DataType, DecimalDigits, Nullable: SqlSmallint;
  ColumnSize: SqlULen;
  c, siz: integer;
  Name: array[byte] of WideChar;
begin
  ReleaseRows;
  with ODBC do
  begin
    Check(nil, self,
      NumResultCols(fStatement, nCols),
      SQL_HANDLE_STMT, fStatement);
    SetLength(fColData, nCols);
    fColumn.Capacity := nCols;
    for c := 1 to nCols do
    begin
      Check(nil, self,
        DescribeColW(fStatement, c, Name{%H-}, 256, NameLength, DataType,
          ColumnSize, DecimalDigits, Nullable),
        SQL_HANDLE_STMT, fStatement);
      with PSqlDBColumnProperty(
        fColumn.AddAndMakeUniqueName(RawUnicodeToUtf8(Name, NameLength)))^ do
      begin
        ColumnValueInlined := true;
        ColumnValueDBType := DataType;
        if ColumnSize > 65535 then
          ColumnSize := 0; // avoid out of memory error for BLOBs
        ColumnValueDBSize := ColumnSize;
        ColumnNonNullable := (Nullable = SQL_NO_NULLS);
        ColumnType := ODBCColumnToFieldType(DataType, 10, DecimalDigits);
        if ColumnType = ftUtf8 then
          if ColumnSize = 0 then
            siz := 256
          else
            siz := ColumnSize * 2 + 16
        else // guess max size as WideChar buffer
          siz := ColumnSize;
        if siz < 64 then
          siz := 64; // ODBC never truncates fixed-length data: ensure minimum
        if siz > Length(fColData[c - 1]) then
          SetLength(fColData[c - 1], siz);
      end;
    end;
    assert(fColumnCount = nCols);
  end;
end;

procedure TSqlDBODBCStatement.GetData(var Col: TSqlDBColumnProperty; ColIndex: integer);
var
  ExpectedDataType: ShortInt;
  ExpectedDataLen: integer;
  Status: SqlReturn;
  Indicator: SqlLen;
  P: PAnsiChar;

  function IsTruncated: boolean;
  begin
    result := (Indicator > 0) and
              (ODBC.GetDiagField(fStatement) = '01004');
  end;

  procedure CheckStatus;
  begin
    if Status <> SQL_SUCCESS then
      ODBC.HandleError(nil, self, Status, SQL_HANDLE_STMT, fStatement, false, sllNone);
  end;

  procedure RaiseError;
  begin
    raise EODBCException.CreateUtf8('%.GetCol: [%] column had Indicator=%', [self,
      Col.ColumnName, Indicator]);
  end;

begin
  ExpectedDataType := ODBC_TYPE_TOC[Col.ColumnType];
  ExpectedDataLen := length(fColData[ColIndex]);
  //FillcharFast(pointer(fColData[ColIndex])^,ExpectedDataLen,ord('~'));
  Status := ODBC.GetData(fStatement, ColIndex + 1, ExpectedDataType,
    pointer(fColData[ColIndex]), ExpectedDataLen, @Indicator);
  Col.ColumnDataSize := Indicator;
  if Status <> SQL_SUCCESS then
    if Status = SQL_SUCCESS_WITH_INFO then
      if Col.ColumnType in FIXEDLENGTH_SqlDBFIELDTYPE then
        Status := SQL_SUCCESS
      else // allow rounding problem
      if IsTruncated then
      begin
        if Col.ColumnType <> ftBlob then
        begin
          dec(ExpectedDataLen, SizeOf(WideChar)); // ignore null termination
          inc(Indicator, SizeOf(WideChar)); // always space for additional #0
        end;
        SetLength(fColData[ColIndex], Indicator);
        P := pointer(fColData[ColIndex]);
        inc(P, ExpectedDataLen);
        ExpectedDataLen := Indicator - ExpectedDataLen;
        //FillcharFast(P^,ExpectedDataLen,ord('~'));
        Status := ODBC.GetData(fStatement, ColIndex + 1, ExpectedDataType, P,
          ExpectedDataLen, @Indicator);
        CheckStatus;
      end
      else
        CheckStatus
    else
      CheckStatus;
  if Indicator >= 0 then
    case Status of
      SQL_SUCCESS, SQL_NO_DATA:
        Col.ColumnDataState := colDataFilled;
    else
      RaiseError;
    end
  else
    case Indicator of
      SQL_NULL_DATA:
        Col.ColumnDataState := colNull;
      SQL_NO_TOTAL:
        if Col.ColumnType in FIXEDLENGTH_SqlDBFIELDTYPE then
          Col.ColumnDataState := colDataFilled
        else
          raise EODBCException.CreateUtf8('%.GetCol: [%] column has no size', [self,
            Col.ColumnName]);
    else
      RaiseError;
    end;
end;

function TSqlDBODBCStatement.GetCol(Col: integer;
  ExpectedType: TSqlDBFieldType): TSqlDBStatementGetCol;
var
  c: integer;
begin // colNull, colWrongType, colTmpUsed, colTmpUsedTruncated
  CheckCol(Col); // check Col<fColumnCount
  if not Assigned(fStatement) or
     (fColData = nil) then
    raise EODBCException.CreateUtf8('%.Column*() with no prior Execute', [self]);
  // get all fColData[] (driver may be without SQL_GD_ANY_ORDER)
  for c := 0 to fColumnCount - 1 do
    if fColumns[c].ColumnDataState = colNone then
      GetData(fColumns[c], c);
  // retrieve information for the specified column
  if (ExpectedType = ftNull) or
     (fColumns[Col].ColumnType = ExpectedType) or
     (fColumns[Col].ColumnDataState = colNull) then
    result := fColumns[Col].ColumnDataState
  else
    result := colWrongType;
end;

function TSqlDBODBCStatement.MoreResults: boolean;
var
  R: SqlReturn;
begin
  R := ODBC.MoreResults(fStatement);
  case R of
    SQL_NO_DATA:
      result := false; // no more results
    SQL_SUCCESS, SQL_SUCCESS_WITH_INFO:
      result := true; // got next
  else
    begin
      ODBC.Check(nil, self, R, SQL_HANDLE_STMT, fStatement); // error
      result := false; // makes compiler happy
    end;
  end;
end;

function TSqlDBODBCStatement.ColumnBlob(Col: integer): RawByteString;
var
  res: TSqlDBStatementGetCol;
begin
  res := GetCol(Col, ftBlob);
  case res of
    colNull:
      result := '';
    colWrongType:
      ColumnToTypedValue(Col, ftBlob, result);
  else
    result := copy(fColData[Col], 1, fColumns[Col].ColumnDataSize);
  end;
end;

function TSqlDBODBCStatement.ColumnUtf8(Col: integer): RawUtf8;
var
  res: TSqlDBStatementGetCol;
begin
  res := GetCol(Col, ftUtf8);
  case res of
    colNull:
      result := '';
    colWrongType:
      ColumnToTypedValue(Col, ftUtf8, result);
  else
    RawUnicodeToUtf8(pointer(fColData[Col]), fColumns[Col].ColumnDataSize shr 1, result);
  end;
end;

function TSqlDBODBCStatement.ColumnCurrency(Col: integer): currency;
begin
  case GetCol(Col, ftCurrency) of
    colNull:
      result := 0;
    colWrongType:
      ColumnToTypedValue(Col, ftCurrency, result);
  else
    PInt64(@result)^ := StrToCurr64(Pointer(fColData[Col])); // as SQL_C_CHAR
  end;
end;

function TSqlDBODBCStatement.ColumnDateTime(Col: integer): TDateTime;
begin
  case GetCol(Col, ftDate) of
    colNull:
      result := 0;
    colWrongType:
      ColumnToTypedValue(Col, ftDate, result);
  else
    result := PSql_TIMESTAMP_STRUCT(Pointer(fColData[Col]))^.ToDateTime(fColumns
      [Col].ColumnValueDBType);
  end;
end;

function TSqlDBODBCStatement.ColumnDouble(Col: integer): double;
begin
  case GetCol(Col, ftDouble) of
    colNull:
      result := 0;
    colWrongType:
      ColumnToTypedValue(Col, ftDouble, result);
  else
    result := GetExtended(Pointer(fColData[Col])); // encoded as SQL_C_CHAR
  end;
end;

function TSqlDBODBCStatement.ColumnInt(Col: integer): Int64;
begin
  case GetCol(Col, ftInt64) of
    colNull:
      result := 0;
    colWrongType:
      ColumnToTypedValue(Col, ftInt64, result);
  else
    SetInt64(Pointer(fColData[Col]), result); // encoded as SQL_C_CHAR
  end;
end;

function TSqlDBODBCStatement.ColumnNull(Col: integer): boolean;
begin // will check for NULL but never returns colWrongType
  result := GetCol(Col, ftNull) = colNull;
end;

procedure TSqlDBODBCStatement.ColumnsToJson(WR: TJsonWriter);
var
  res: TSqlDBStatementGetCol;
  col: integer;
  tmp: array[0..31] of AnsiChar;
begin
  if not Assigned(fStatement) or
     (CurrentRow <= 0) then
    raise EODBCException.CreateUtf8('%.ColumnsToJson() with no prior Step', [self]);
  if WR.Expand then
    WR.Add('{');
  for col := 0 to fColumnCount - 1 do // fast direct conversion from OleDB buffer
    with fColumns[col] do
    begin
      if WR.Expand then
        WR.AddFieldName(ColumnName); // add '"ColumnName":'
      res := GetCol(col, ColumnType);
      if res = colNull then
        WR.AddNull
      else
        case ColumnType of
          ftInt64:
            WR.AddNoJsonEscape(Pointer(fColData[col]));  // already as SQL_C_CHAR
          ftDouble, ftCurrency:
            WR.AddFloatStr(Pointer(fColData[col]));      // already as SQL_C_CHAR
          ftDate:
            WR.AddNoJsonEscape(@tmp,
              PSql_TIMESTAMP_STRUCT(Pointer(fColData[col]))^.ToIso8601(
                tmp{%H-}, ColumnValueDBType, fForceDateWithMS));
          ftUtf8:
            begin
              WR.Add('"');
              if ColumnDataSize > 1 then
                WR.AddJsonEscapeW(Pointer(fColData[col]), ColumnDataSize shr 1);
              WR.Add('"');
            end;
          ftBlob:
            if fForceBlobAsNull then
              WR.AddNull
            else
              WR.WrBase64(pointer(fColData[col]), ColumnDataSize, true);
        else
          assert(false);
        end;
      WR.Add(',');
    end;
  WR.CancelLastComma; // cancel last ','
  if WR.Expand then
    WR.Add('}');
end;

constructor TSqlDBODBCStatement.Create(aConnection: TSqlDBConnection);
begin
  if not aConnection.InheritsFrom(TSqlDBODBCConnection) then
    raise EODBCException.CreateUtf8('%.Create(%)', [self, aConnection]);
  inherited Create(aConnection);
end;

destructor TSqlDBODBCStatement.Destroy;
begin
  try
    DeallocStatement;
  finally
    inherited Destroy;
  end;
end;

const
  NULWCHAR: WideChar = #0;

procedure TSqlDBODBCStatement.ExecutePrepared;
const
  ODBC_IOTYPE_TO_PARAM: array[TSqlDBParamInOutType] of ShortInt = (
    SQL_PARAM_INPUT, SQL_PARAM_OUTPUT, SQL_PARAM_INPUT_OUTPUT);
  IDList_type: WideString = 'IDList';
  StrList_type: WideString = 'StrList';

  function CType2SQL(CDataType: integer): integer;
  begin
    case CDataType of
      SQL_C_CHAR:
        case fDBMS of
          dInformix:
            result := SQL_INTEGER;
        else
          result := SQL_VARCHAR;
        end;
      SQL_C_TYPE_DATE:
        result := SQL_TYPE_DATE;
      SQL_C_TYPE_TIMESTAMP:
        result := SQL_TYPE_TIMESTAMP;
      SQL_C_WCHAR:
        case fDBMS of
          dInformix:
            result := SQL_VARCHAR;
        else
          result := SQL_WVARCHAR;
        end;
      SQL_C_BINARY:
        result := SQL_VARBINARY;
      SQL_C_SBIGINT:
        result := SQL_BIGINT;
      SQL_C_DOUBLE:
        result := SQL_DOUBLE;
    else
      raise EODBCException.CreateUtf8('%.ExecutePrepared: Unexpected ODBC C type %',
        [self, CDataType]);
    end;
  end;

var
  p, k: integer;
  status: SqlReturn;
  InputOutputType, CValueType, ParameterType, DecimalDigits: SqlSmallint;
  ColumnSize: SqlULen;
  ParameterValue: SqlPointer;
  ItemSize, BufferSize: SqlLen;
  ItemPW: PWideChar;
  timestamp: SQL_TIMESTAMP_STRUCT;
  ansitext: boolean;
  StrLen_or_Ind: array of PtrInt;
  ArrayData: array of record
    StrLen_or_Ind: array of PtrInt;
    WData: RawUnicode;
  end;
label
  retry;
begin
  SQLLogBegin(sllSQL);
  if fStatement = nil then
    raise EODBCException.CreateUtf8('%.ExecutePrepared called without previous Prepare',
      [self]);
  inherited ExecutePrepared; // set fConnection.fLastAccessTicks
  ansitext := TSqlDBODBCConnection(fConnection).fODBCProperties.
    fDriverDoesNotHandleUnicode;
  try
    // 1. bind parameters
    if (fParamsArrayCount > 0) and
       (fDBMS <> dMSSQL) then
      raise EODBCException.CreateUtf8('%.BindArray() not supported', [self]);
    if fParamCount > 0 then
    begin
      SetLength(StrLen_or_Ind, fParamCount);
      if fParamsArrayCount > 0 then
        SetLength(ArrayData, fParamCount);
      for p := 0 to fParamCount - 1 do
        with fParams[p] do
        begin
          StrLen_or_Ind[p] := SQL_NTS;
          ParameterValue := nil;
          CValueType := ODBC_TYPE_TOC[VType];
          ParameterType := CType2SQL(CValueType);
          InputOutputType := ODBC_IOTYPE_TO_PARAM[VInOut];
          ColumnSize := 0;
          DecimalDigits := 0;
          if (fDBMS = dMSSQL) and
             (VArray <> nil) then
          begin
            // bind an array as one object - metadata only at the moment
            if VInOut <> paramIn then
              raise EODBCException.CreateUtf8(
                '%.ExecutePrepared: Unsupported array parameter direction #%',
                [self, p + 1]);
            CValueType := SQL_C_DEFAULT;
            ParameterType := SQL_SS_TABLE;
            ColumnSize := Length(VArray);
            // Type name must be specified as a Unicode value,
            // even in applications which are built as ANSI applications
            case VType of
              ftInt64:
                ParameterValue := pointer(IDList_type);
              ftUtf8:
                ParameterValue := pointer(StrList_type);
            else
              raise EODBCException.CreateUtf8(
                '%.ExecutePrepared: Unsupported array parameter type #%',
                [self, p + 1]);
            end;
            BufferSize := Length(WideString(ParameterValue)) * SizeOf(WideChar);
            StrLen_or_Ind[p] := Length(VArray);
          end
          else
          begin
            // Bind one simple parameter value
            case VType of
              ftNull:
                StrLen_or_Ind[p] := SQL_NULL_DATA;
              ftInt64:
                if VInOut = paramIn then
                  VData := Int64ToUtf8(VInt64)
                else
                begin
                  CValueType := SQL_C_SBIGINT;
                  ParameterValue := pointer(@VInt64);
                end;
              ftDouble:
                begin
                  CValueType := SQL_C_DOUBLE;
	          // in case of "Invalid character value for cast specification" error
                  // for small digits like 0.01, -0.0001 under Linux msodbcsql17 should
                  // be updated to >= 17.5.2
                  ParameterValue := pointer(@VInt64);
                end;
              ftCurrency:
                if VInOut = paramIn then
                  VData := Curr64ToStr(VInt64)
                else
                begin
                  CValueType := SQL_C_DOUBLE;
                  unaligned(PDouble(@VInt64)^) := PCurrency(@VInt64)^;
                  ParameterValue := pointer(@VInt64);
                end;
              ftDate:
                begin
                  CValueType := timestamp.From(PDateTime(@VInt64)^, BufferSize);
                  SetString(VData, PAnsiChar(@timestamp), BufferSize);
                  // A workaround for "[ODBC Driver 13 for SQL Server]Datetime field overflow.
                  // Fractional second precision exceeds the scale specified in the parameter binding"
                  // Implemented according to http://rightondevelopment.blogspot.com/2009/10/sql-server-native-client-100-datetime.html
                  if fDBMS = dMSSQL then
                    // Starting from MSSQL 2008 client DecimalDigits must not be 0
                    // Possibly can be set to either 3 (datetime) or 7 (datetime2)
                    DecimalDigits := 3;
                end;
              ftUtf8:
                if ansitext then
                begin
retry:            VData := CurrentAnsiConvert.Utf8ToAnsi(VData);
                  CValueType := SQL_C_CHAR;
                end
                else
                begin
                  VData := Utf8DecodeToRawUnicode(VData);
                  if fDBMS = dMSSQL then
                  begin // CONTAINS(field, ?) do not accept NVARCHAR(max)
                    ColumnSize := length(VData) shr 1; // length in characters
                    if ColumnSize > 4000 then // > 8000 bytes - use varchar(max)
                      ColumnSize := 0;
                  end;
                end;
              ftBlob:
                StrLen_or_Ind[p] := length(VData);
            else
              raise EODBCException.CreateUtf8('%.ExecutePrepared: invalid bound parameter #%',
                [self, p + 1]);
            end;
            if ParameterValue = nil then
            begin
              if pointer(VData) = nil then
                ParameterValue := @NULWCHAR
              else
                ParameterValue := pointer(VData);
              BufferSize := length(VData);
              if CValueType = SQL_C_CHAR then
                inc(BufferSize)
              else // include last #0
              if CValueType = SQL_C_WCHAR then
                BufferSize := BufferSize shr 1;
            end
            else
              BufferSize := SizeOf(Int64);
          end;
          status := ODBC.BindParameter(fStatement, p + 1, InputOutputType,
            CValueType, ParameterType, ColumnSize, DecimalDigits, ParameterValue,
            BufferSize, StrLen_or_Ind[p]);
          if (status = SQL_ERROR) and
             not ansitext and
             (ODBC.GetDiagField(fStatement) = 'HY004') then
          begin
            TSqlDBODBCConnection(fConnection).fODBCProperties.
              fDriverDoesNotHandleUnicode := true;
            ansitext := true;
            VData := RawUnicodeToUtf8(pointer(VData), StrLenW(pointer(VData)));
            goto retry; // circumvent restriction of non-Unicode ODBC drivers
          end;
          ODBC.Check(nil, self, status, SQL_HANDLE_STMT, fStatement);
          // populate array data
          if (fDBMS = dMSSQL) and
             (VArray <> nil) then
          begin
            // first set focus on param
            status := ODBC.SetStmtAttrA(fStatement, SQL_SOPT_SS_PARAM_FOCUS,
              SQLPOINTER(p + 1), SQL_IS_INTEGER);
            if not (status in [SQL_SUCCESS, SQL_NO_DATA]) then
              ODBC.HandleError(nil, self, status, SQL_HANDLE_STMT, fStatement,
                false, sllNone);
            // bind the only column
            SetLength(ArrayData[p].StrLen_or_Ind, Length(VArray));
            CValueType := SQL_C_WCHAR;
            ParameterType := SQL_WVARCHAR;
            // all data should be passed in a single buffer of fixed size records
            // so find out the size of a record, i.e. maximum string length
            BufferSize := 0;
            for k := 0 to high(VArray) do
            begin
              if VType = ftUtf8 then
                VArray[k] := UnQuoteSqlString(VArray[k]);
              ItemSize := Utf8ToUnicodeLength(pointer(VArray[k]));
              if ItemSize > BufferSize then
                BufferSize := ItemSize;
            end;
            inc(BufferSize); // add space for #0
            SetLength(ArrayData[p].WData, BufferSize * Length(VArray) * SizeOf(WideChar));
            ItemPW := pointer(ArrayData[p].WData);
            for k := 0 to high(VArray) do
            begin
              ArrayData[p].StrLen_or_Ind[k] := Utf8ToWideChar(
                ItemPW, pointer(VArray[k]), BufferSize, length(VArray[k]));
              inc(ItemPW, BufferSize);
            end;
            status := ODBC.BindParameter(fStatement, 1, SQL_PARAM_INPUT,
              CValueType, ParameterType, 0, 0, pointer(ArrayData[p].WData),
              BufferSize * SizeOf(WideChar), ArrayData[p].StrLen_or_Ind[0]);
            if not (status in [SQL_SUCCESS, SQL_NO_DATA]) then
              ODBC.HandleError(nil, self, status, SQL_HANDLE_STMT, fStatement,
                false, sllNone);
            // Reset param focus
            status := ODBC.SetStmtAttrA(fStatement, SQL_SOPT_SS_PARAM_FOCUS,
              SQLPOINTER(0), SQL_IS_INTEGER);
            if not (status in [SQL_SUCCESS, SQL_NO_DATA]) then
              ODBC.HandleError(nil, self, status, SQL_HANDLE_STMT, fStatement,
                false, sllNone);
          end;
        end;
    end;
    // 2. execute prepared statement
    status := ODBC.Execute(fStatement);
    if not (status in [SQL_SUCCESS, SQL_NO_DATA]) then
      ODBC.HandleError(nil, self, status, SQL_HANDLE_STMT, fStatement, false, sllNone);
    if fExpectResults then
      BindColumns;
  finally
    // 3. release and/or retrieve OUT bound parameters
    for p := 0 to fParamCount - 1 do
      with fParams[p] do
        case VType of
          ftCurrency:
            if VInOut <> paramIn then
              PCurrency(@VInt64)^ := unaligned(PDouble(@VInt64)^);
          ftDate:
            if VInOut <> paramIn then
              PDateTime(@VInt64)^ := PSql_TIMESTAMP_STRUCT(VData)^.ToDateTime;
          ftUtf8:
            if ansitext then
              VData := CurrentAnsiConvert.AnsiBufferToRawUtf8(pointer(VData),
                StrLen(pointer(VData)))
            else
              VData := RawUnicodeToUtf8(pointer(VData), StrLenW(pointer(VData)));
        end;
  end;
  SQLLogEnd;
end;

procedure TSqlDBODBCStatement.Reset;
begin
  if fStatement <> nil then
  begin
    ReleaseRows;
    if fParamCount > 0 then
      ODBC.Check(nil, self,
        ODBC.FreeStmt(fStatement, SQL_RESET_PARAMS),
        SQL_HANDLE_STMT, fStatement);
  end;
  inherited Reset;
end;

procedure TSqlDBODBCStatement.ReleaseRows;
begin
  fColData := nil;
  if fColumnCount > 0 then
  begin
    if fStatement <> nil then
      ODBC.CloseCursor(fStatement); // no check needed
    fColumn.Clear;
    fColumn.ReHash;
  end;
  inherited ReleaseRows;
end;

function TSqlDBODBCStatement.UpdateCount: integer;
var
  RowCount: SqlLen;
begin
  if (fStatement <> nil) and
     not fExpectResults then
    ODBC.Check(nil, self,
      ODBC.RowCount(fStatement, RowCount),
      SQL_HANDLE_STMT, fStatement)
  else
    RowCount := 0;
  result := RowCount;
end;

procedure TSqlDBODBCStatement.Prepare(const aSQL: RawUtf8; ExpectResults: boolean);
begin
  SQLLogBegin(sllDB);
  if (fStatement <> nil) or
     (fColumnCount > 0) then
    raise EODBCException.CreateUtf8(
      '%.Prepare should be called only once', [self]);
  // 1. process SQL
  inherited Prepare(aSQL, ExpectResults); // set fSQL + Connect if necessary
  fSQLW := Utf8DecodeToRawUnicode(fSQL);
  // 2. prepare statement and bind result columns (if any)
  AllocStatement;
  try
    ODBC.Check(nil, self,
      ODBC.PrepareW(fStatement, pointer(fSQLW), length(fSQLW) shr 1),
      SQL_HANDLE_STMT, fStatement);
    SQLLogEnd;
  except
    on E: Exception do
    begin
      ODBC.FreeHandle(SQL_HANDLE_STMT, fStatement);
      fStatement := nil;
      raise;
    end;
  end;
end;

function TSqlDBODBCStatement.Step(SeekFirst: boolean): boolean;
const
  CMD: array[boolean] of smallint = (
    SQL_FETCH_NEXT, SQL_FETCH_FIRST);
var
  status: SqlReturn;
  i, sav: integer;
begin
  result := false;
  sav := fCurrentRow;
  fCurrentRow := 0;
  if not Assigned(fStatement) or
     (fColumnCount = 0) then
    exit; // no row available at all (e.g. for SQL UPDATE) -> return false
  for i := 0 to fColumnCount - 1 do
    fColumns[i].ColumnDataState := colNone; // force load all fColData[]
  with ODBC do
  begin
    status := FetchScroll(fStatement, CMD[SeekFirst], 0);
    case status of
      SQL_NO_DATA:
        exit;
      SQL_SUCCESS, SQL_SUCCESS_WITH_INFO:
        begin // ignore WITH_INFO messages
          fCurrentRow := sav + 1;
          inc(fTotalRowsRetrieved);
          result := true; // mark data available for Column*() methods
        end;
    else
      HandleError(nil, self, status, SQL_HANDLE_STMT, fStatement, false, sllNone);
    end;
  end;
end;


{ TSqlDBODBCConnectionProperties }

constructor TSqlDBODBCConnectionProperties.Create(const aServerName,
  aDatabaseName, aUserID, aPassWord: RawUtf8);
begin
  if ODBC = nil then
    ODBC := TODBCLib.Create;
  inherited Create(aServerName, aDatabaseName, aUserID, aPassWord);
  // stored UserID is used by SQLSplitProcedureName
  if aUserID = '' then
    FUserID := FindIniNameValue(pointer(UpperCase(StringReplaceAll(
      aDatabaseName, ';', sLineBreak))), 'UID=');
end;

function TSqlDBODBCConnectionProperties.NewConnection: TSqlDBConnection;
begin
  result := TSqlDBODBCConnection.Create(self);
end;

procedure TSqlDBODBCConnectionProperties.GetFields(const aTableName: RawUtf8;
  out Fields: TSqlDBColumnDefineDynArray);
var
  Schema, Table: RawUtf8;
  F: TSqlDBColumnDefine;
  i, n, DataType: integer;
  status: SqlReturn;
  FA: TDynArray;
begin
  inherited; // first try from SQL, if any (faster)
  if Fields <> nil then
    exit; // already retrieved directly from engine
  Split(aTableName, '.', Schema, Table);
  if Table = '' then
  begin
    Table := Schema;
    Schema := '%';
  end;
  Table := UpperCase(Table);
  Schema := UpperCase(Schema);
  try
    // get column definitions
    with TSqlDBODBCStatement.Create(MainConnection) do
    try
      AllocStatement;
      status := ODBC.ColumnsA(fStatement, nil, 0, pointer(Schema), SQL_NTS,
        pointer(Table), SQL_NTS, nil, 0);
      if status = SQL_SUCCESS then
      begin
        BindColumns;
        if not Step then
          status := SQL_NO_DATA; // no info -> retry without schema
      end;
      if status <> SQL_SUCCESS then
      begin
        DeallocStatement;
        AllocStatement;
        status := ODBC.ColumnsA(fStatement, nil, 0, nil, 0, pointer(Table),
          SQL_NTS, nil, 0);
        ODBC.Check(Connection, nil, status, SQL_HANDLE_STMT, fStatement);
        BindColumns;
        Step;
      end;
      FA.Init(TypeInfo(TSqlDBColumnDefineDynArray), Fields, @n);
      FA.Compare := SortDynArrayAnsiStringI; // FA.Find() case insensitive
      FillcharFast(F, SizeOf(F), 0);
      if fCurrentRow > 0 then // Step done above
        repeat
          F.ColumnName := TrimU(ColumnUtf8(3)); // Column*() should be done in order
          DataType := ColumnInt(4);
          F.ColumnTypeNative := TrimU(ColumnUtf8(5));
          F.ColumnLength := ColumnInt(6);
          F.ColumnScale := ColumnInt(8);
          F.ColumnPrecision := ColumnInt(9);
          F.ColumnType := ODBCColumnToFieldType(DataType, F.ColumnPrecision, F.ColumnScale);
          F.ColumnIndexed := (fDBMS in [dFirebird, dDB2]) and IsRowID(pointer(F.ColumnName));
          // ID UNIQUE field create an implicit index
          FA.Add(F);
        until not Step;
      SetLength(Fields, n);
    finally
      Free; // TSqlDBODBCStatement release
    end;
    // get indexes
    if n > 0 then
      with TSqlDBODBCStatement.Create(MainConnection) do
      try
        AllocStatement;
        status := ODBC.StatisticsA(fStatement, nil, 0, pointer(Schema), SQL_NTS,
          pointer(Table), SQL_NTS, SQL_INDEX_ALL, SQL_QUICK);
        if status <> SQL_SUCCESS then // e.g. driver does not support schema
          status := ODBC.StatisticsA(fStatement, nil, 0, nil, 0,
            pointer(Table), SQL_NTS, SQL_INDEX_ALL, SQL_QUICK);
        ODBC.Check(Connection, nil, status, SQL_HANDLE_STMT, fStatement);
        BindColumns;
        while Step do
        begin
          F.ColumnName := TrimU(ColumnUtf8(8));
          i := FA.Find(F);
          if i >= 0 then
            Fields[i].ColumnIndexed := true;
        end;
      finally
        Free; // TSqlDBODBCStatement release
      end;
  except
    on Exception do
      Fields := nil;
  end;
end;

procedure TSqlDBODBCConnectionProperties.GetTableNames(out Tables: TRawUtf8DynArray);
var
  n: integer;
  schema, tablename: RawUtf8;
begin
  inherited; // first try from SQL, if any (faster)
  if Tables <> nil then
    exit; // already retrieved directly from engine
  try
    with TSqlDBODBCStatement.Create(MainConnection) do
    try
      AllocStatement;
      ODBC.Check(Connection, nil,
        ODBC.TablesA(fStatement, nil, 0, nil, 0, nil, 0, 'TABLE', SQL_NTS),
        SQL_HANDLE_STMT, fStatement);
      BindColumns;
      n := 0;
      while Step do
      begin
        schema := TrimU(ColumnUtf8(1));
        tablename := TrimU(ColumnUtf8(2));
        if schema <> '' then
          tablename := schema + '.' + tablename;
        AddSortedRawUtf8(Tables, n, tablename);
      end;
      SetLength(Tables, n);
    finally
      Free; // TSqlDBODBCStatement release
    end;
  except
    on Exception do
      SetLength(Tables, 0);
  end;
end;

procedure TSqlDBODBCConnectionProperties.GetViewNames(out Views: TRawUtf8DynArray);
var
  n: integer;
  schema, tablename: RawUtf8;
begin
  inherited; // first try from SQL, if any (faster)
  if Views <> nil then
    exit; // already retrieved directly from engine
  try
    with TSqlDBODBCStatement.Create(MainConnection) do
    try
      AllocStatement;
      ODBC.Check(Connection, nil,
        ODBC.TablesA(fStatement, nil, 0, nil, 0, nil, 0, 'VIEW', SQL_NTS),
        SQL_HANDLE_STMT, fStatement);
      BindColumns;
      n := 0;
      while Step do
      begin
        schema := TrimU(ColumnUtf8(1));
        tablename := TrimU(ColumnUtf8(2));
        if schema <> '' then
          tablename := schema + '.' + tablename;
        AddSortedRawUtf8(Views, n, tablename);
      end;
      SetLength(Views, n);
    finally
      Free; // TSqlDBODBCStatement release
    end;
  except
    on Exception do
      SetLength(Views, 0);
  end;
end;

procedure TSqlDBODBCConnectionProperties.GetForeignKeys;
begin
  try
    with TSqlDBODBCStatement.Create(MainConnection) do
    try
      AllocStatement;
      ODBC.Check(Connection, nil,
        ODBC.ForeignKeysA(fStatement, nil, 0, nil, 0, nil, 0, nil, 0, nil, 0, '%', SQL_NTS),
        SQL_HANDLE_STMT, fStatement);
      BindColumns;
      while Step do
        fForeignKeys.Add(TrimU(ColumnUtf8(5)) + '.' +
                         TrimU(ColumnUtf8(6)) + '.' +
                         TrimU(ColumnUtf8(7)),
                         TrimU(ColumnUtf8(1)) + '.' +
                         TrimU(ColumnUtf8(2)) + '.' +
                         TrimU(ColumnUtf8(3)));
    finally
      Free; // TSqlDBODBCStatement release
    end;
  except
    on Exception do
      ; // just ignore errors here
  end;
end;

procedure TSqlDBODBCConnectionProperties.GetProcedureNames(
  out Procedures: TRawUtf8DynArray);
var
  Schema: RawUtf8;
  n: integer;
  status: SqlReturn;
  Stmt: TSqlDBODBCStatement;
begin
  inherited; // first try from SQL, if any (faster)
  if Procedures <> nil then
    exit; // already retrieved directly from engine
  SetSchemaNameToOwner(Schema);
  Schema := UpperCase(Schema);
  try
    // get procedure list
    Stmt := TSqlDBODBCStatement.Create(MainConnection);
    try
      Stmt.AllocStatement;
      status := ODBC.SQLProcedures(Stmt.fStatement, nil, 0, pointer(Schema),
        SQL_NTS, nil, 0);
      ODBC.Check(Stmt.Connection, nil, status, SQL_HANDLE_STMT, Stmt.fStatement);
      Stmt.BindColumns;
      n := 0;
      while Stmt.Step do
        AddSortedRawUtf8(Procedures, n,
          TrimU(Stmt.ColumnUtf8(2))); // PROCEDURE_NAME column
      SetLength(Procedures, n);
    finally
      Stmt.Free; // TSqlDBODBCStatement release
    end;
  except
    on Exception do
      Procedures := nil;
  end;
end;

procedure TSqlDBODBCConnectionProperties.GetProcedureParameters(
  const aProcName: RawUtf8; out Parameters: TSqlDBProcColumnDefineDynArray);
var
  schem, pack, proc: RawUtf8;
  P: TSqlDBProcColumnDefine;
  PA: TDynArray;
  n, DataType: integer;
  status: SqlReturn;
  Stmt: TSqlDBODBCStatement;
begin
  inherited; // first try from SQL, if any (faster)
  if Parameters <> nil then
    exit; // already retrieved directly from engine
  SQLSplitProcedureName(aProcName, schem, pack, proc);
  proc := UpperCase(proc);
  pack := UpperCase(pack);
  schem := UpperCase(schem);
  if pack <> '' then
    proc := pack + '.' + proc;
  try
    // get column definitions
    Stmt := TSqlDBODBCStatement.Create(MainConnection);
    try
      Stmt.AllocStatement;
      status := ODBC.SQLProcedureColumnsA(Stmt.fStatement, nil, 0,
        pointer(schem), SQL_NTS, pointer(proc), SQL_NTS, nil, 0);
      if status = SQL_SUCCESS then
      begin
        Stmt.BindColumns;
        if not Stmt.Step then
          status := SQL_NO_DATA; // no info -> retry without schema
      end;
      if status <> SQL_SUCCESS then
      begin
        Stmt.DeallocStatement;
        Stmt.AllocStatement;
        status := ODBC.SQLProcedureColumnsA(Stmt.fStatement, nil, 0, nil, 0,
          pointer(proc), SQL_NTS, nil, 0);
        ODBC.Check(Stmt.Connection, nil, status, SQL_HANDLE_STMT, Stmt.fStatement);
        Stmt.BindColumns;
        Stmt.Step;
      end;
      PA.Init(TypeInfo(TSqlDBColumnDefineDynArray), Parameters, @n);
      FillcharFast(P, SizeOf(P), 0);
      if Stmt.fCurrentRow > 0 then // Step done above
        repeat
          P.ColumnName := TrimU(Stmt.ColumnUtf8(3)); // Column*() should be in order
          case Stmt.ColumnInt(4) of
            SQL_PARAM_INPUT:
              P.ColumnParamType := paramIn;
            SQL_PARAM_INPUT_OUTPUT:
              P.ColumnParamType := paramInOut;
          else
            P.ColumnParamType := paramOut;
          end;
          DataType := Stmt.ColumnInt(5);
          P.ColumnTypeNative := TrimU(Stmt.ColumnUtf8(6));
          P.ColumnLength := Stmt.ColumnInt(7);
          P.ColumnScale := Stmt.ColumnInt(8);
          P.ColumnPrecision := Stmt.ColumnInt(9);
          P.ColumnType := ODBCColumnToFieldType(
            DataType, P.ColumnPrecision, P.ColumnScale);
          PA.Add(P);
        until not Stmt.Step;
      SetLength(Parameters, n);
    finally
      Stmt.Free; // TSqlDBODBCStatement release
    end;
  except
    on Exception do
      Parameters := nil;
  end;
end;

function TSqlDBODBCConnectionProperties.GetDatabaseNameSafe: RawUtf8;
var
  pwd: RawUtf8;
begin
  pwd := FindIniNameValue(pointer(StringReplaceAll(
    fDatabaseName, ';', sLineBreak)), 'PWD=');
  result := StringReplaceAll(fDatabaseName, pwd, '***');
end;

function TSqlDBODBCConnectionProperties.GetDBMS: TSqlDBDefinition;
begin
  if fDBMS = dUnknown then
    with MainConnection as TSqlDBODBCConnection do
    begin
      if not IsConnected then
        Connect; // retrieve DBMS property
      self.fDBMS := DBMS;
    end;
  result := fDBMS;
end;

initialization
  TSqlDBODBCConnectionProperties.RegisterClassNameForDefinition;
  {$ifndef PUREMORMOT2}
  // backward compatibility class registration
  TODBCConnectionProperties.RegisterClassNameForDefinition;
  {$endif PUREMORMOT2}

end.

