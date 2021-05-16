/// Database Framework Zeos/ZDBC Connection
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.sql.zeos;

{
  *****************************************************************************

   Efficient SQL Database Connection using the Direct ZDBC 7.3 layer
    -  TSqlDBZeosConnection* and TSqlDBZeosStatement Classes

  *****************************************************************************
}

interface

{$ifdef NOSYNDBZEOS}
// NOSYNDBZEOS from mormot2.lpk Lazarus package > Custom Options > Defines

implementation // compile a void unit if NOSYNDBZEOS conditional is set

{$else}

{$I Zeos.inc} // define conditionals like ZEOS72UP and ENABLE_*

// for best performance: tune your project options or Zeos.inc
// by defining MORMOT2 and leverage best of mORMot and ZEOS !

{$I ..\mormot.defines.inc}

uses
  types,
  sysutils,
  classes,
  variants,

  // load ZDBC physical providers as defined by ENABLE_* in Zeos.inc
  // -> you can patch your local Zeos.inc and comment these defines to
  // exclude database engines you don't need
  // or (since 7.2) you simply add the ZEOS_DISABLE_XXX defines to your
  // project options. The units are used but no line code will be compiled.
  {$if defined(ENABLE_ADO) and not defined(ZEOS_DISABLE_ADO)}
  ZDbcAdo,
  {$ifend}
  {$if defined(ENABLE_DBLIB) and not defined(ZEOS_DISABLE_DBLIB)}
  ZDbcDbLib,
  {$ifend}
  {$if defined(ENABLE_MYSQL) and not defined(ZEOS_DISABLE_MYSQL)}
  ZDbcMySql,
  {$ifend}
  {$if defined(ENABLE_POSTGRESQL) and not defined(ZEOS_DISABLE_POSTGRESQL)}
  ZDbcPostgreSql,
  {$ifend}
  {$if defined(ENABLE_INTERBASE) and not defined(ZEOS_DISABLE_INTERBASE)}
  ZDbcInterbase6,
  {$ifend}
  {$if defined(ENABLE_FIREBIRD) and not defined(ZEOS_DISABLE_FIREBIRD)}
  ZDbcFirebird,
  {$ifend}
  {$if defined(ENABLE_SQLITE) and not defined(ZEOS_DISABLE_SQLITE)}
  ZDbcSqLite,
  {$ifend}
  {$if defined(ENABLE_ORACLE) and not defined(ZEOS_DISABLE_ORACLE)}
  ZDbcOracle,
  {$ifend}
  {$if defined(ENABLE_ASA) and not defined(ZEOS_DISABLE_ASA)}
  ZDbcASA,
  {$ifend}
  {$if defined(ENABLE_SQLANY) and not defined(ZEOS_DISABLE_SQLANY)}
  ZDbcSQLAnywhere,
  {$ifend}
  {$if defined(ENABLE_POOLED) and not defined(ZEOS_DISABLE_POOLED)}
  ZDbcPooled,
  {$ifend}
  {$if defined(ENABLE_OLEDB) and not defined(ZEOS_DISABLE_OLEDB)}
  ZDbcOleDB,
  {$ifend}
  {$if defined(ENABLE_ODBC) and not defined(ZEOS_DISABLE_ODBC)}
  ZDbcODBCCon,
  {$ifend}

  // main ZDBC units
  ZCompatibility,
  ZVariant,
  {$ifndef ZEOS80UP}
  ZURL,
  {$endif ZEOS80UP}
  ZDbcIntfs,
  ZDbcResultSet,
  ZDbcMetadata,
  
  // mORMot units after ZDBC due to some name conflicts (e.g. Utf8ToString)
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.data,
  mormot.core.perf,
  mormot.core.rtti,
  mormot.core.log,
  mormot.db.core,
  mormot.db.sql;


{ ************  TSqlDBZeosConnection* and TSqlDBZeosStatement Classes }

type
  /// Exception type associated to the ZEOS database components
  ESqlDBZeos = class(ESqlDBException);

  /// implement properties shared by ZEOS connections
  TSqlDBZeosConnectionProperties = class(TSqlDBConnectionPropertiesThreadSafe)
  protected
    fURL: TZURL;
    fStatementParams: TStrings;
    fDBMSName: RawUtf8;
    fSupportsArrayBindings: boolean;
    /// initialize fForeignKeys content with all foreign keys of this DB
    // - do nothing by now (ZEOS metadata may be used in the future)
    procedure GetForeignKeys; override;
    /// convert ZDBC field type into mORMot fieldtype
    function TZSQLTypeToTSqlDBFieldType(aNativeType: TZSQLType): TSqlDBFieldType;
  public
    /// initialize the properties to connect to the ZEOS engine
    // - aServerName shall contain the ZEOS URI, e.g:
    // $ zdbc:firebird-2.0://127.0.0.1:3050/model?username=sysdba;password=masterkey
    // $ zdbc:mysql://192.168.2.60:3306/world?username=root;password=dev
    // $ sqlite
    // i.e. '[zdbc:]PROTOCOL://HOST:PORT[/DATABASE][?paramname=value]'
    // - you can define the TZConnection.LibraryLocation property by setting a
    // '?LibLocation=...' parameter within the aServerName URL value
    // - or simply use TSqlDBZeosConnectionProperties.URI() class method
    // - aDatabaseName, aUserID, aPassword are used if not already set as URI
    // in aServerName value
    // - you can use Protocols property to retrieve all available protocol names
    // - note that when run from mORMot's ORM, this class will by default
    // create one connection per thread, which makes some clients (e.g.
    // PostgreSQL) unstable and consuming a lot of resources - you should better
    // maintain one single connection, by setting after Create:
    // ! aExternalDBProperties.ThreadingMode := tmMainConnection;
    // or by adding 'syndb_singleconnection=true' as URI property
    constructor Create(const aServerName, aDatabaseName,
      aUserID, aPassWord: RawUtf8); override;
    /// initialize raw properties to connect to the ZEOS engine
    // - using Zeos' TZURL detailed class - see: src\core\ZURL.pas
    // - this gives all possibilities to add Properties before a connection is opened
    // - you can define the protocol by hand eg. "odbc_w"/"OleDB" and define
    // TSqlDBDefinition to describe the server syntax mormot.db.sql and the ORM
    // use behind the abstract driver
    constructor CreateWithZURL(const aURL: TZURL; aDBMS: TSqlDBDefinition;
      aOwnsURL: boolean); virtual;
    /// finalize properties internal structures
    destructor Destroy; override;
    /// create a new connection
    // - caller is responsible of freeing this instance
    // - this overridden method will create an TSqlDBZeosConnection instance
    function NewConnection: TSqlDBConnection; override;

    /// retrieve the column/field layout of a specified table
    // - this overridden method will use ZDBC metadata to retrieve the information
    procedure GetFields(const aTableName: RawUtf8;
      out Fields: TSqlDBColumnDefineDynArray); override;
    /// get all table names
    // - this overridden method will use ZDBC metadata to retrieve the information
    // - PostgreSQL note: it was reported that some table names expects to be
    // quoted for this DB engine - and ZDBC won't do it for yourself - please
    // ensure you specify the correct quoted table name e.g. when you register
    // the external PostgreSQL table via function VirtualTableExternalRegister()
    procedure GetTableNames(out Tables: TRawUtf8DynArray); override;
    /// access to the database metadata, as retrieved by ZEOS
    // - returns TRUE if metadata interface has been retrieved
    function GetDatabaseMetadata(out meta: IZDatabaseMetadata): boolean;
    /// compute the ZEOS URI for a given database engine
    // - the optional server name can contain a port number, specified after ':'
    // - you can set an optional full path to the client library name,
    // to be completed on the left side with the executable path
    // - possible use may be:
    // ! PropsOracle := TSqlDBZeosConnectionProperties.Create(
    // !   TSqlDBZeosConnectionProperties.URI(dOracle,'','oci64\oci.dll'),
    // !   'tnsname','user',pass');
    // ! PropsFirebird := TSqlDBZeosConnectionProperties.Create(
    // !   TSqlDBZeosConnectionProperties.URI(dFirebird,'','Firebird\fbembed.dll'),
    // !   'databasefilename','',');
    // ! PropsFirebird := TSqlDBZeosConnectionProperties.Create(
    // !   TSqlDBZeosConnectionProperties.URI(dFirebird,'192.168.1.10:3055',
    // !     'c:\Firebird_2_5\bin\fbclient.dll',false),
    // !  '3camadas', 'sysdba', 'masterkey');
    class function URI(aServer: TSqlDBDefinition;
      const aServerName: RawUtf8; const aLibraryLocation: TFileName = '';
      aLibraryLocationAppendExePath: boolean = true): RawUtf8; overload;
    /// compute the ZEOS URI for a given protocol
    // - if a TSQSLDBDefinition may have several protocols (e.g. MSSQL), you
    // can use this overloaded method to select the exact protocol to use if the
    // default one fixed by TSqlDBDefinition does not match your needs
    // - the protocol name should contain the trailing : character, e.g.
    // 'firebird-2.0:' if the default 'firebird-2.5:' is not correct
    class function URI(const aProtocol, aServerName: RawUtf8;
      const aLibraryLocation: TFileName = '';
      aLibraryLocationAppendExePath: boolean = true): RawUtf8; overload;
  published
    /// the remote DBMS name, as retrieved from ServerName, i.e. ZEOS URL
    property DBMSName: RawUtf8
      read fDBMSName;
    /// direct access to the internal TZURL connection parameters
    property ZeosURL: TZURL
      read fURL;
    /// direct access to the internal statement parameters
    // - i.e. will be used by IZConnection.PrepareStatementWithParams()
    // - default values (set in Create method) try to achieve best permormance
    property ZeosStatementParams: TStrings
      read fStatementParams;
    /// if the associated ZDBC provider supports parameters array binding
    // - you should use the BindArray() methods only if this property is TRUE
    property SupportsArrayBindings: boolean
      read fSupportsArrayBindings;
  end;


  /// implements a connection via the ZEOS access layer
  TSqlDBZeosConnection = class(TSqlDBConnectionThreadSafe)
  protected
    fDatabase: IZConnection;
  public
    /// prepare a connection to a specified ZEOS database server
    constructor Create(aProperties: TSqlDBConnectionProperties); override;
    /// connect to the specified ZEOS server
    // - should raise an ESqlDBZeos on error
    procedure Connect; override;
    /// stop connection to the specified ZEOS database server
    // - should raise an ESqlDBZeos on error
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
    /// access to the associated ZEOS connection instance
    property Database: IZConnection
      read fDatabase;
  end;

  /// implements a statement via a ZEOS database connection
  TSqlDBZeosStatement = class(TSqlDBStatementWithParamsAndColumns)
  protected
    fStatement: IZPreparedStatement;
    fResultSet: IZResultSet;
    fResultInfo: IZResultSetMetaData;
    {$if defined(ZEOS73UP) and defined(MORMOT2)}
    fJSONComposeOptions: TZJSONComposeOptions;
    {$ifend}
  public
    {$if defined(ZEOS73UP) and defined(MORMOT2)}
    procedure AfterConstruction; override;
    {$ifend}
    /// Prepare an UTF-8 encoded SQL statement
    // - parameters marked as ? will be bound later, before ExecutePrepared call
    // - if ExpectResults is TRUE, then Step() and Column*() methods are available
    // to retrieve the data rows
    // - raise an ESqlDBZeos on any error
    procedure Prepare(const aSQL: RawUtf8;
      ExpectResults: boolean = false); overload; override;
    /// Execute a prepared SQL statement
    // - parameters marked as ? should have been already bound with Bind*() functions
    // - this implementation will also handle bound array of values (if any),
    // if IZDatabaseInfo.SupportsArrayBindings is true for this provider
    // - this overridden method will log the SQL statement if sllSQL has been
    // enabled in SynDBLog.Family.Level
    // - raise an ESqlDBZeos on any error
    procedure ExecutePrepared; override;
    {$ifdef ZEOS72UP}
    /// append all columns values of the current Row to a JSON stream
    // - will use WR.Expand to guess the expected output format
    // - this overriden implementation will call fReultSet methods to avoid
    // creating most temporary variable
    procedure ColumnsToJson(WR: TJsonWriter); override;
    {$endif ZEOS72UP}
    /// gets a number of updates made by latest executed statement
    function UpdateCount: integer; override;
    /// Reset the previous prepared statement
    // - this overridden implementation will reset all bindings and the cursor state
    // - raise an ESqlDBZeos on any error
    procedure Reset; override;

    /// Access the next or first row of data from the SQL Statement result
    // - return true on success, with data ready to be retrieved by Column*() methods
    // - return false if no more row is available (e.g. if the SQL statement
    // is not a SELECT but an UPDATE or INSERT command)
    // - if SeekFirst is TRUE, will put the cursor on the first row of results
    // - raise an ESqlDBZeos on any error
    function Step(SeekFirst: boolean = false): boolean; override;
    /// free IZResultSet/IZResultSetMetaData when ISqlDBStatement is back in cache
    procedure ReleaseRows; override;
    /// return a Column integer value of the current Row, first Col is 0
    function ColumnInt(Col: integer): Int64; override;
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
  {$if defined(ZEOS73UP) and defined(MORMOT2)}
  public
    /// the ColumnsToJson options provided by ZDBC
    // - jcoEndJsonObject:
    // cancels last comma, adds the close object bracket '}' and add's the next comma.
    // If not set you can continue writting some custom data into your
    // object json but you also have to finalize each row-object then.
    // - jcoMongoISODate:
    // formats the date,time,datetime values as mongo
    // ISODate("YYYY-MM-DDTHH:NN:ssZ"). Milliseconds are included.
    // So the values are recognized as date type by mongodb.
    // Otherwise mongo threads them as strings.
    // This option might be usefull if you export sql rows using a
    // json streamed file which will be used as an import-file with your mongodb
    // If set the options jcoDATETIME_MAGIC and jcoMilliseconds are ignored
    // - jcoDATETIME_MAGIC add the JSON_BASE64_MAGIC_C on top of data
    // before adding the ISO date,time,datetime value quoted strings
    // - jcoMilliseconds compose the time/datetime values with milliseconds
    // - jcsSkipNulls ignore null columns. So neither fieldname nor the null
    // value will be composed into your JSON. For real big JSON contents
    // it saves loads of space. e.g. if you import a JSON into a mongo cluster
    // you'll have a significant space difference if null's are simply ignored.
    property JSONComposeOptions: TZJSONComposeOptions
      read fJSONComposeOptions write fJSONComposeOptions
      default [jcoEndJsonObject];
  {$ifend}
  end;

var
  /// list of all available ZEOS protocols
  // - you have to call SetZEOSProtocols before using it, to update this
  // global list with all initialized ZPlain*Driver units
  // - to be used e.g. within ZEOS URI, as TSqlDBZeosConnectionProperties.ServerName
  ZEOSProtocols: TRawUtf8DynArray;

/// to be called in order to populate the global ZEOSProtocols list
procedure SetZEOSProtocols;


implementation


{ ************  TSqlDBZeosConnection* and TSqlDBZeosStatement Classes }

{ TSqlDBZeosConnectionProperties }

constructor TSqlDBZeosConnectionProperties.Create(const aServerName,
  aDatabaseName, aUserID, aPassWord: RawUtf8);
const
  PCHARS: array[0..8] of PAnsiChar = ('ORACLE', 'FREETDS_MSSQL', 'MSSQL',
    'INTERBASE', 'FIREBIRD', 'MYSQL', 'SQLITE', 'POSTGRESQL', 'JET');
  TYPES: array[-1..high(PCHARS)] of TSqlDBDefinition = (
    dDefault, dOracle, dMSSQL, dMSSQL, dFirebird, dFirebird, dMySQL,
    dSQLite, dPostgreSQL, dJet {e.g. ADO[JET]} );
    // expecting Sybase + ASA support in TSqlDBDefinition
var
  BrakedPos: integer;
begin
  // return e.g. mysql://192.168.2.60:3306/world?username=root;password=dev
  // make syntax like "ADO[ORACLE]"/"ADO[MSSQL]:"/"ADO[JET]" etc... possible
  BrakedPos := PosExChar('[', aServerName);
  if (BrakedPos > 0) and
     ((aServerName[Length(aServerName)] = ']') or
      (aServerName[Length(aServerName) - 1] = ']')) then
  begin
    fServerName := Copy(aServerName, 1, BrakedPos - 1);
    fDBMSName := Copy(aServerName, BrakedPos + 1,
      PosExChar(']', aServerName) - 1 - BrakedPos);
  end
  else
    fServerName := aServerName;
  if (fServerName <> '') and
     (PosExChar(':', fServerName) = 0) then
    fServerName := fServerName + ':';
  if not IdemPChar(Pointer(aServerName), 'ZDBC:') then
    fServerName := 'zdbc:' + fServerName;
  fURL := TZURL.Create(Utf8ToString(fServerName));
  if fURL.Database = '' then
    fURL.Database := Utf8ToString(aDatabaseName);
  if fURL.UserName = '' then
    fURL.UserName := Utf8ToString(aUserID);
  if fURL.Password = '' then
    fURL.Password := Utf8ToString(aPassWord);
  if fDBMSName = '' then
    StringToUtf8(fURL.Protocol, fDBMSName);
  CreateWithZURL(fURL, TYPES[IdemPCharArray(pointer(fDBMSName), PCHARS)], true);
end;

procedure TSqlDBZeosConnectionProperties.GetForeignKeys;
begin
  { TODO : get FOREIGN KEYS from ZEOS metadata ? }
end;

function TSqlDBZeosConnectionProperties.NewConnection: TSqlDBConnection;
begin
  result := TSqlDBZeosConnection.Create(self);
end;

constructor TSqlDBZeosConnectionProperties.CreateWithZURL(const aURL: TZURL;
  aDBMS: TSqlDBDefinition; aOwnsURL: boolean);
{$ifdef ZEOS73UP}
var
  protocol: RawUtf8;
{$endif ZEOS73UP}
begin
  // return e.g. mysql://192.168.2.60:3306/world?username=root;password=dev
  if aOwnsURL then
    fURL := aURL
  else
    fURL := TZURL.Create(aURL);
  fDBMS := aDBMS;
  {$ifndef UNICODE}
  fURL.Properties.Values['controls_cp'] := 'CP_UTF8';
  {$endif UNICODE}
  { Implementation/Enhancements Notes About settings below (from Michael):
    ConnectionProperties:
    Since 7.3 you should look to ZDbcProperties.pas
    All connection Properties including driver dependencies are listed in this file.

    otherwise:
    Make it possible to Assign Parameters on the fly e.g.:
    FireBird:
     - add custom TIL's by hand if tiNone was set. or some more custom settings:
        see ZDbcInterbaseUtils.pas: TransactionParams and DatabaseParams
     - "hard_commit=true" False by default
    PostgreSQL:
     - "oidasblob=true" - False by default
        notify Zeos should use Oid fields as BLob's
     - "CheckFieldVisibility=True/False"
        notify Zeos should determine temporary tables field meta informations too
        required for mORMot?
     - "NoTableInfoCache=True/False" - False by default
        notify Zeos it should use internal TableInfo-cache.
        Set the value to true to save memory
    ADO:
      7.3up
     - "internal_buffer_size=X" in bytes default are 128KB
       this is the max size in bytes you allow Zeos to use for batch-ole array_bindings
       This parameter is only used if "use_ole_update_params=True"
     - "use_ole_update_params=True/False" default = False
       bypassing slow MSADO15.DLL and direct use OleDB parameters for all kind
       of updates including batch Note: current code is also able to handle Out/InOut
       params except for out/inout lob's.
      note: both internal_buffer_size and use_ole_update_params can be used as
       statement parameters as well
    Oracle:
     - "row_prefetch_size=x! in bytes
       this value will be send to OCI and indicates Oracle which
       row_prefetch_size you allow to execute a query
  }
  {$ifdef ZEOS73UP}
  if fDBMS = dMSSQL then
  begin
    protocol := LowerCase(StringToUtf8(FURL.Protocol));
    //EH: switch off tds support in any kind -> deprecated and our 64bit lib isn't compiled with libiconv support
    //users permanently run into the encoding issues which can't be resolved using the dblib tds-types:
    //tdsNVarchar/tdsNText is defined but will never be used by DBLIB + SQLServer)
    if {$ifdef OSWINDOWS}
       (protocol <> 'ado') and
       (protocol <> 'oledb') and
       {$endif OSWINDOWS}
       (protocol <> 'odbc_w') and
       (protocol <> 'odbc_a') then
      {$ifdef OSWINDOWS}
      FURL.Protocol := 'OleDB';
      {$else}
      FURL.Protocol := 'odbc_w'
      {$endif OSWINDOWS}
  end;
  {$endif ZEOS73UP}
  inherited Create(StringToUtf8(FURL.HostName), StringToUtf8(FURL.Database),
    StringToUtf8(FURL.UserName), StringToUtf8(FURL.Password));
  if StrToBoolDef(fURL.Properties.Values['syndb_singleconnection'], false) then
    ThreadingMode := tmMainConnection;
  // caching disabled by default - enabled if stable enough
  fUseCache := {$ifdef ZEOS72UP}true{$else}false{$endif ZEOS72UP};
  case fDBMS of
    dSQLite:
      begin
        {$ifndef ZEOS72UP}
        fSQLCreateField[ftInt64] := ' BIGINT'; // SQLite3 INTEGER = 32bit for ZDBC!
        {$endif ZEOS72UP}
      end;
    dFirebird:
      begin
        if (fURL.HostName = '') and // Firebird embedded
           (fURL.Database <> '') then
        begin
          ThreadingMode := tmMainConnection; // force SINGLE connection
          if not FileExists(fURL.Database) then // create local DB file if needed
            fURL.Properties.Add('createNewDatabase=' + Utf8ToString(
              SQLCreateDatabase(StringToUtf8(fURL.Database))));
        end;
        fURL.Properties.Add('codepage=UTF8');
        fUseCache := true; // caching rocks with Firebird ZDBC provider :)
      end;
    dOracle, dPostgreSQL, dMySQL:
      begin
        fURL.Properties.Add('codepage=UTF8');
        fUseCache := true;
      end;
  end;
  fStatementParams := TStringList.Create;
  case fDBMS of
    dOracle:
      begin
        {$ifndef ZEOS72UP} // fixed since 7.2up
        // sets OCI_ATTR_PREFETCH_ROWS on prepare a fetch
        // default = 100 on 7.1down
        // since 7.3 the option is ignored
        fStatementParams.Add('prefetch_count=1000');
        {$else}
        //max mem in bytes which OCI(Server) can use for a result on Server-Side
        //fStatementParams.Add('row_prefetch_size=131072');
        //max mem in bytes which Zeos can for batch or resultset buffers
        //fStatementParams.Add('internal_buffer_size=131072');
        {$endif ZEOS72UP}
      end;
    dSQLite:
      begin
        {$ifdef ZEOS72UP} // new since 7.2up
        // Bind double values instead of ISO formated DateTime-strings
        //fStatementParams.Add('BindDoubleDateTimeValues=True');
        {$endif ZEOS72UP}
      end;
    dMySQL:
      begin
        // use mysql real-prepared api instead of string based once
        // actually it's not realy faster.. just a hint:
        // http://dev.mysql.com/doc/refman/5.0/en/c-api-prepared-statement-problems.html
        //fStatementParams.Add('preferprepared=True');
        // https://mariadb.com/kb/en/library/bulk-insert-row-wise-binding/
        // https://mariadb.com/kb/en/library/bulk-insert-column-wise-binding/
        // if you run into some mysql known issues,
        // since 7.3 you've to explicit unset real_prepared by
        //fStatementParams.Add('emulate_prepares=True'); or
        //fStatementParams.Add('MinExecCountBeforePrepare=-1');
      end;
    dPostgreSQL:
      begin
        // see https://synopse.info/forum/viewtopic.php?pid=13260#p13260
        fURL.Properties.Add('NoTableInfoCache=true');
        {$ifdef ZEOS73UP}
        //fURL.Properties.Add(DSProps_BinaryWireResultMode+'=False');
        {$endif ZEOS73UP}
      end;
    dMSSQL:
      begin
        fUseCache := true;
        fStatementParams.Add('enhanced_column_info=false');
        //fStatementParams.Add('use_ole_update_params=True'); //see 'ADO'
        //fStatementParams.Add('internal_buffer_size=131072'); //see 'ADO'
        // EH: hooking a mormot.db.sql limitation. Void strings are not
        // accepted by SQLServer if: fixed-width columns are used and
        // option ANSI_PADDING is set to !off!.
        StoreVoidStringAsNull := False;
      end;
  end;
  if fDBMS in [dOracle, dPostgreSQL, dMySQL, dMSSQL] then
  begin
    // let's set 1024KB / chunk for synopse  or more?
    // retrieving/submitting lob's in chunks. Default is 4096Bytes / Chunk
    // it's depending to your local network speed e.g. bad WLAN or so
    // for Firebird we always using the blob-segment size
    fStatementParams.Add('chunk_size=1048576');
  end;
  if fDBMS in [dPostgreSQL,dFireBird] then
  begin
    {$ifdef ZEOS72UP} // new since 7.2up
    // Always load the lobs? Or just on accessing them?
    // if you allways copy the data by fetching the row than it doesn't make sense.
    fStatementParams.Add('cachedlob=false'); //default = False
    {$endif ZEOS72UP}
  end;
end;

destructor TSqlDBZeosConnectionProperties.Destroy;
begin
  FreeAndNil(fURL);
  FreeAndNil(fStatementParams);
  inherited;
end;

procedure SetZEOSProtocols;
var
  List: TStringList;
  i, j: integer;
  Protocols: Types.TStringDynArray;
begin
  List := TStringList.Create;
  try
    with DriverManager.GetDrivers do
      for i := 0 to Count - 1 do
      begin
        Protocols := (Items[i] as IZDriver).GetSupportedProtocols;
        for j := 0 to high(Protocols) do
          List.Add(Protocols[j]);
      end;
    List.Sort;
    StringListToRawUtf8DynArray(List, ZEOSProtocols);
  finally
    List.Free;
  end;
end;

function TSqlDBZeosConnectionProperties.GetDatabaseMetadata(out meta:
  IZDatabaseMetadata): boolean;
var
  conn: IZConnection;
begin
  conn := (MainConnection as TSqlDBZeosConnection).fDatabase;
  result := conn.UseMetadata;
  meta := conn.GetMetadata;
  if result then
    meta.ClearCache; // we need to retrieve the actual metadata
  {$ifdef ZEOS72UP} // new since 7.2up
  if result and meta.GetDatabaseInfo.SupportsArrayBindings then
  begin
    case GetDBMS of
      dPostgreSQL:
        fBatchSendingAbilities := [cCreate, cDelete] // EH: array logic isn't ready 4updates yet
    else
      fBatchSendingAbilities := [cCreate, cUpdate, cDelete];
    end;
    OnBatchInsert := nil;
    fSupportsArrayBindings := True;
  end;
  {$endif ZEOS72UP}
end;

procedure TSqlDBZeosConnectionProperties.GetTableNames(out Tables: TRawUtf8DynArray);
var
  meta: IZDatabaseMetadata;
  res: IZResultSet;
  TableTypes: Types.TStringDynArray;
  n: integer;
begin
  if GetDatabaseMetadata(meta) then
  begin
    SetLength(TableTypes, 1);
    TableTypes[0] := 'TABLE';
    res := meta.GetTables('', '', '', TableTypes);
    n := 0;
    while res.Next do
      {$ifdef ZEOS72UP}
      AddSortedRawUtf8(Tables, n,
        res.GetUTF8String(TableNameIndex));
      {$else}
      AddSortedRawUtf8(Tables, n,
        SynUnicodeToUtf8(res.GetUnicodeString(TableNameIndex)));
      {$endif ZEOS72UP}
    SetLength(Tables, n);
  end
  else
    inherited;
end;

procedure TSqlDBZeosConnectionProperties.GetFields(const aTableName: RawUtf8;
  out Fields: TSqlDBColumnDefineDynArray);
var
  meta: IZDatabaseMetadata;
  res: IZResultSet;
  n, i: integer;
  Schema, TableName: RawUtf8;
  sSchema, sTableName: string;
  F: TSqlDBColumnDefine;
  FA: TDynArray;
begin
  if GetDatabaseMetadata(meta) then
  begin
    SQLSplitTableName(aTableName, Schema, TableName);
    sSchema := Utf8ToString(Schema);
    { mormot does not create the Tables casesensitive but gives mixed cased
     strings as tablename so we normalize the identifiers to database defaults }
    sTableName :=  {$ifdef ZEOS80UP}
                   meta.GetIdentifierConverter
                   {$else}
                   meta.GetIdentifierConvertor
                   {$endif}.ExtractQuote(Utf8ToString(TableName));
    // do not escape https://synopse.info/forum/viewtopic.php?pid=34896#p34896
    res := meta.GetColumns('', sSchema,
      meta.AddEscapeCharToWildcards(sTableName), '');
    FA.InitSpecific(TypeInfo(TSqlDBColumnDefineDynArray), Fields,
      ptRawUtf8, @n, {caseinsens=}true);
    FillCharFast(F, sizeof(F), 0);
    while res.Next do
    begin
      {$ifdef ZEOS72UP}
      F.ColumnName := res.GetUTF8String(ColumnNameIndex);
      F.ColumnTypeNative := res.GetUTF8String(TableColColumnTypeNameIndex);
      {$else}
      F.ColumnName := SynUnicodeToUtf8(
        res.GetUnicodeString(ColumnNameIndex));
      F.ColumnTypeNative := SynUnicodeToUtf8(
        res.GetUnicodeString(TableColColumnTypeNameIndex));
      {$endif ZEOS72UP}
      F.ColumnType := TZSQLTypeToTSqlDBFieldType(
        TZSQLType(res.GetInt(TableColColumnTypeIndex)));
      F.ColumnLength := res.GetInt(TableColColumnSizeIndex);  // for char or date types this is the maximum number of characters
      F.ColumnPrecision := res.GetInt(TableColColumnSizeIndex);  // for numeric or decimal types this is precision
      F.ColumnScale := res.GetInt(TableColColumnDecimalDigitsIndex);  // the number of fractional digits
      FA.Add(F);
    end;
    if n > 0 then
    begin
      res := meta.GetIndexInfo('', sSchema, sTableName, false, true);
      while res.Next do
      begin
        {$ifdef ZEOS72UP}
        F.ColumnName := res.GetUTF8String(IndexInfoColColumnNameIndex);
        {$else}
        F.ColumnName := SynUnicodeToUtf8(
          res.GetUnicodeString(IndexInfoColColumnNameIndex));
        {$endif ZEOS72UP}
        i := FA.Find(F);
        if i >= 0 then
          Fields[i].ColumnIndexed := true;
      end;
      SetLength(Fields, n);
    end;
  end;
end;

function TSqlDBZeosConnectionProperties.TZSQLTypeToTSqlDBFieldType(
  aNativeType: TZSQLType): TSqlDBFieldType;
begin
  case aNativeType of
    stBoolean, stByte, stShort, stInteger, stLong
    {$ifdef ZEOS72UP}, stSmall, stWord, stLongWord, stULong {$endif ZEOS72UP}:
      result := ftInt64;
    stFloat, stDouble:
      result := ftDouble;
    stBigDecimal
    {$ifdef ZEOS72UP}, stCurrency {$endif ZEOS72UP}:
      result := ftCurrency;
    stDate, stTime, stTimestamp:
      result := ftDate;
    {$ifdef ZEOS72UP}stGUID, {$endif ZEOS72UP}
    stString, stUnicodeString, stAsciiStream, stUnicodeStream:
      result := ftUtf8;
    stBytes, stBinaryStream:
      result := ftBlob;
  else
    raise ESqlDBZeos.CreateUtf8('%: unexpected TZSQLType "%"', [self,
      GetEnumName(Typeinfo(TZSQLType), ord(aNativeType))^]);
  end;
end;

class function TSqlDBZeosConnectionProperties.URI(aServer: TSqlDBDefinition;
  const aServerName: RawUtf8; const aLibraryLocation: TFileName;
  aLibraryLocationAppendExePath: boolean): RawUtf8;
const
  /// ZDBC provider names corresponding to mormot.db.sql recognized SQL engines
  ZEOS_PROVIDER: array[TSqlDBDefinition] of RawUtf8 = ('', '', 'oracle:',
    'mssql:', '', 'mysql:', 'sqlite:', 'firebird:', '', 'postgresql:', '', '');
begin
  result := URI(ZEOS_PROVIDER[aServer], aServerName, aLibraryLocation,
    aLibraryLocationAppendExePath);
end;

class function TSqlDBZeosConnectionProperties.URI(const aProtocol, aServerName:
  RawUtf8; const aLibraryLocation: TFileName; aLibraryLocationAppendExePath:
  boolean): RawUtf8;
begin
  // return e.g. mysql://192.168.2.60:3306/world?username=root;password=dev
  result := TrimU(aProtocol);
  if result = '' then
    exit;
  if aServerName <> '' then
    result := result + '//' + aServerName;
  if aLibraryLocation <> '' then
  begin
    if (aServerName <> '') and
       (aServerName[1] = '?') then
      result := result + ';LibLocation='
    else
      result := result + '?LibLocation=';

    if aLibraryLocationAppendExePath then
      result := result + StringToUtf8(Executable.ProgramFilePath);
    result := result + StringToUtf8(aLibraryLocation);
  end;
end;


{ TSqlDBZeosConnection }

constructor TSqlDBZeosConnection.Create(aProperties: TSqlDBConnectionProperties);
var
  url: TZURL;
begin
  inherited Create(aProperties);
  url := (fProperties as TSqlDBZeosConnectionProperties).fURL;
  fDatabase := DriverManager.GetConnectionWithParams(url.URL, url.Properties);
  // EG: setup the connection transaction behavior now, not once Opened in Connect
  //fDatabase.SetReadOnly(false); // is default
  // about transactions, see https://synopse.info/forum/viewtopic.php?id=2209
  //fDatabase.SetAutoCommit(true); // is default
  fDatabase.SetTransactionIsolation(tiReadCommitted); // will be swapped to tiSerialiable for SQLite
end;

procedure TSqlDBZeosConnection.Connect;
var
  log: ISynLog;
begin
  if fDatabase = nil then
    raise ESqlDBZeos.CreateUtf8('%.Connect() on % failed: Database=nil', [self,
      fProperties.ServerName]);
  log := SynDBLog.Enter(self, 'Connect');
  if log <> nil then
    with (fProperties as TSqlDBZeosConnectionProperties).fURL do
      log.Log(sllTrace, 'Connect to % % for % at %:%',
        [Protocol, Database, HostName, Port]);
  try
    fDatabase.Open;
    if log <> nil then
      log.log(sllDB, 'Connected to % using % %', [fProperties.ServerName,
        fProperties.DatabaseNameSafe, fDatabase.GetClientVersion]);
    inherited Connect; // notify any re-connection
  except
    on E: Exception do
    begin
      Disconnect; // clean up on fail
      raise;
    end;
  end;
end;
procedure TSqlDBZeosConnection.Disconnect;
begin
  try
    inherited Disconnect; // flush any cached statement
  finally
    if (fDatabase <> nil) and
       not fDatabase.IsClosed then
      fDatabase.Close;
  end;
end;

function TSqlDBZeosConnection.IsConnected: boolean;
begin
  result := Assigned(fDatabase) and
            not fDatabase.IsClosed;
end;

function TSqlDBZeosConnection.NewStatement: TSqlDBStatement;
begin
  if not IsConnected then
    Connect;
  result := TSqlDBZeosStatement.Create(self);
end;

procedure TSqlDBZeosConnection.StartTransaction;
var
  log: ISynLog;
begin
  log := SynDBLog.Enter(self, 'StartTransaction');
  inherited StartTransaction;
  {$ifdef ZEOS73UP}
  fDatabase.StartTransaction; //returns the txn level
  {$else}
  fDatabase.SetAutoCommit(false);
  {$endif ZEOS73UP}
end;
procedure TSqlDBZeosConnection.Commit;
begin
  inherited Commit;
  try
    fDatabase.Commit;
  except
    inc(fTransactionCount); // the transaction is still active
    raise;
  end;
  {$ifndef ZEOS73UP} //no longer required zdbc falls back to AC automatically
  fDatabase.SetAutoCommit(true);
  {$endif ZEOS73UP}
end;

procedure TSqlDBZeosConnection.Rollback;
begin
  inherited Rollback;
  fDatabase.Rollback;
  {$ifndef ZEOS73UP} //no longer required zdbc falls back to AC automatically
  fDatabase.SetAutoCommit(true);
  {$endif ZEOS73UP}
end;


{ TSqlDBZeosStatement }

procedure TSqlDBZeosStatement.Prepare(const aSQL: RawUtf8; ExpectResults: boolean);
begin
  SQLLogBegin(sllDB);
  if (fStatement <> nil) or
     (fResultSet <> nil) then
    raise ESqlDBZeos.CreateUtf8('%.Prepare() shall be called once', [self]);
  inherited Prepare(aSQL, ExpectResults); // connect if necessary
  fStatement := (fConnection as TSqlDBZeosConnection).fDatabase.
    PrepareStatementWithParams(
      {$ifdef UNICODE}Utf8ToString(fSQL){$else}fSQL{$endif}, // see controls_cp=CP_UTF8
      (fConnection.Properties as TSqlDBZeosConnectionProperties).fStatementParams);
  SQLLogEnd;
end;


{$ifdef ZEOS72UP}

type
  // see https://synopse.info/forum/viewtopic.php?pid=11946#p11946
  TZeosArrayBinding = class
  protected
    // ZDBC uses pointer references to arrays -> allocated with the class
    fNullArray:    array of TBooleanDynArray;
    fInt64Array:   array of TInt64DynArray;
    fDoubleArray:  array of TDoubleDynArray;
    fCurDynArray:  array of TCurrencyDynArray;
    fDateDynArray: array of TDateTimeDynArray;
    fUtf8DynArray: array of TRawUtf8DynArray;
    fBlobDynArray: array of TInterfaceDynArray;
    fDynArraySize: array[ftInt64..ftBlob] of integer;
  public
    constructor Create(aStatement: TSqlDBZeosStatement);
  end;

constructor TZeosArrayBinding.Create(aStatement: TSqlDBZeosStatement);
var
  p, j, n: integer;
  ndx: array[ftInt64 .. ftBlob] of integer;
  kind: TSqlDBFieldType;
begin
  with aStatement do
  begin
    SetLength(fNullArray, fParamCount);
    for p := 0 to fParamCount - 1 do
      if fParams[p].VType in [ftInt64 .. ftBlob] then
        inc(fDynArraySize[fParams[p].VType]);
    SetLength(fInt64Array, fDynArraySize[ftInt64]);
    SetLength(fDoubleArray, fDynArraySize[ftDouble]);
    SetLength(fCurDynArray, fDynArraySize[ftCurrency]);
    SetLength(fDateDynArray, fDynArraySize[ftDate]);
    SetLength(fUtf8DynArray, fDynArraySize[ftUtf8]);
    SetLength(fBlobDynArray, fDynArraySize[ftBlob]);
    FillcharFast(ndx, sizeof(ndx), 0);
    for p := 0 to fParamCount - 1 do
    begin
      if fParams[p].VInt64 <> fParamsArrayCount then
        raise ESqlDBZeos.CreateUtf8(
          '%.ExecutePrepared: #% parameter expected array count %, got %',
          [aStatement, p, fParamsArrayCount, fParams[p].VInt64]);
      SetLength(fNullArray[p], fParamsArrayCount);
      with fParams[p] do
      begin
        case VType of
          ftUnknown:
            raise ESqlDBZeos.CreateUtf8(
              '%.ExecutePrepared: Unknown type array parameter #%',
              [aStatement, p + FirstDbcIndex]);
          ftNull:
            begin
              // handle null column
              for j := 0 to fParamsArrayCount - 1 do
                fNullArray[p][j] := True;
              fStatement.SetDataArray(p + FirstDbcIndex, '', stString, vtUTF8String);
            end;
        else
          begin
            // array binding of ftInt64..ftBlob values from fParams[p].VArray[]
            for j := 0 to fParamsArrayCount - 1 do
              fNullArray[p][j] := VArray[j] = 'null';
            n := ndx[VType];
            case VType of
              ftInt64:
                begin
                  SetLength(fInt64Array[n], fParamsArrayCount);
                  for j := 0 to fParamsArrayCount - 1 do
                    if not fNullArray[p][j] then
                      SetInt64(pointer(VArray[j]), fInt64Array[n][j]);
                  fStatement.SetDataArray(p + FirstDbcIndex, fInt64Array[n], stLong);
                end;
              ftDouble:
                begin
                  SetLength(fDoubleArray[n], fParamsArrayCount);
                  for j := 0 to fParamsArrayCount - 1 do
                    if not fNullArray[p][j] then
                      fDoubleArray[n][j] := GetExtended(pointer(VArray[j]));
                  fStatement.SetDataArray(p + FirstDbcIndex, fDoubleArray[n], stDouble);
                end;
              ftCurrency:
                begin
                  SetLength(fCurDynArray[n], fParamsArrayCount);
                  for j := 0 to fParamsArrayCount - 1 do
                    if not fNullArray[p][j] then
                      fCurDynArray[n][j] := StrToCurrency(pointer(VArray[j]));
                  fStatement.SetDataArray(p + FirstDbcIndex, fCurDynArray[n], stCurrency);
                end;
              ftDate:
                begin
                  SetLength(fDateDynArray[n], fParamsArrayCount);
                  for j := 0 to fParamsArrayCount - 1 do
                    if not fNullArray[p][j] then
                      fDateDynArray[n][j] := Iso8601ToDateTimePUtf8Char(
                        PUtf8Char(pointer(VArray[j])) + 1, Length(VArray[j]) - 2);
                  fStatement.SetDataArray(p + FirstDbcIndex, fDateDynArray[n], stTimestamp);
                end;
              ftUtf8:
                begin
                  SetLength(fUtf8DynArray[n], fParamsArrayCount);
                  for j := 0 to fParamsArrayCount - 1 do
                    if not fNullArray[p][j] then
                      UnQuoteSqlStringVar(pointer(VArray[j]), fUtf8DynArray[n][j]);
                  fStatement.SetDataArray(p + FirstDbcIndex, fUtf8DynArray[n],
                    stString, vtUTF8String);
                end;
              ftBlob:
                begin
                  SetLength(fBlobDynArray[n], fParamsArrayCount);
                  for j := 0 to fParamsArrayCount - 1 do
                    if not fNullArray[p][j] then
                      fBlobDynArray[n][j] := TZAbstractBlob.CreateWithData(
                        Pointer(VArray[j]), length(VArray[j]));
                  fStatement.SetDataArray(p + FirstDbcIndex, fBlobDynArray[n],
                    stBinaryStream);
                end;
            end;
            inc(ndx[VType]);
          end;
        end;
      end;
      fStatement.SetNullArray(p + FirstDbcIndex, stBoolean, fNullArray[p]);
    end;
    for kind := low(ndx) to high(ndx) do
      assert(ndx[kind] = fDynArraySize[kind]);
  end;
end;

{$endif ZEOS72UP}

procedure TSqlDBZeosStatement.ExecutePrepared;
var
  i, n: integer;
  Props: TSqlDBZeosConnectionProperties;
  name: string;
  {$ifdef ZEOS72UP}
  arrayBinding: TZeosArrayBinding;
  {$endif ZEOS72UP}
begin
  SQLLogBegin(sllSQL);
  inherited ExecutePrepared; // set fConnection.fLastAccessTicks
  if fStatement = nil then
    raise ESqlDBZeos.CreateUtf8('%.ExecutePrepared() invalid call', [self]);
  {$ifndef ZEOS72UP}
  //commenting this makes it possible to seek cursor pos to 0 and use the interface again -> e.g. ReadOneByOneRate
  if fResultSet <> nil then
    raise ESqlDBZeos.CreateUtf8('%.ExecutePrepared() miss a Reset', [self]);
  {$endif ZEOS72UP}
  // 1. bind parameters in fParams[] to fQuery.Params
  {$ifdef ZEOS72UP}
  arrayBinding := nil;
  if fParamsArrayCount > 0 then
    with fConnection.Properties as TSqlDBZeosConnectionProperties do
      if fSupportsArrayBindings then
        arrayBinding := TZeosArrayBinding.Create(self)
      else if not fExpectResults then
        raise ESqlDBZeos.CreateUtf8(
          '%.BindArray() not supported by % provider', [self, DBMSName]);
  try
    if arrayBinding=nil then
  {$else}
  if (fParamsArrayCount>0) and
     not fExpectResults then
    raise ESqlDBZeos.CreateUtf8('%.BindArray() not supported', [self])
  else
  {$endif ZEOS72UP}
    for i := fParamCount-1 downto 0 do // EG: downto minimize memallocs
    with fParams[i] do
    begin
      if (Length(VArray) > 0) and
         (fConnection.Properties.DBMS = dPostgreSQL) then
      begin
        if VType in [ftInt64, ftCurrency, ftDouble, ftUtf8] then
          VData := BoundArrayToJsonArray(VArray)
        else
          raise ESqlDBZeos.CreateUtf8('%.ExecutePrepared: Invalid array type % ' +
            'on bound parameter #%', [self, ToText(VType)^, i]);
        VType := ftUtf8;
      end;
      case VType of
        ftNull:
          fStatement.SetNull(i + FirstDbcIndex, stUnknown);
        ftInt64:
          fStatement.SetLong(i + FirstDbcIndex, VInt64);
        ftDouble:
          fStatement.SetDouble(i + FirstDbcIndex, unaligned(PDouble(@VInt64)^));
        ftCurrency:
          {$ifdef ZEOS72UP}
          fStatement.SetCurrency(i + FirstDbcIndex, PCurrency(@VInt64)^);
          {$else}
          fStatement.SetBigDecimal(i + FirstDbcIndex, PCurrency(@VInt64)^);
          {$endif ZEOS72UP}
        ftDate:
          fStatement.SetTimestamp(i + FirstDbcIndex, PDateTime(@VInt64)^);
        ftUtf8:
          {$ifdef ZEOS72UP}
          fStatement.SetUTF8String(i + FirstDbcIndex, VData);
          {$else}
            {$ifdef UNICODE}  // ZWideString = SynUnicode in fact
            fStatement.SetString(i + FirstDbcIndex, Utf8ToSynUnicode(VData));
            {$else}
            fStatement.SetString(i + FirstDbcIndex, VData); // see controls_cp=CP_UTF8
            {$endif UNICODE}
          {$endif ZEOS72UP}
        ftBlob:
          fStatement.SetBlob(i + FirstDbcIndex,stBinaryStream,
            TZAbstractBlob.CreateWithData(Pointer(VData), length(VData)
            {$ifndef ZEOS72UP} ,fStatement.GetConnection{$endif ZEOS72UP}));
      else
        raise ESqlDBZeos.CreateUtf8(
          '%.ExecutePrepared: Invalid type parameter #%', [self, i]);
      end;
    end;
    // 2. Execute query
    if fExpectResults then
    begin
      fColumnCount := 0;
      fColumn.ReHash;
      fCurrentRow := -1;
      fResultSet := fStatement.ExecuteQueryPrepared;
      if fResultSet = nil then
      begin
        // e.g. PRAGMA in TZSQLiteCAPIPreparedStatement.ExecuteQueryPrepared
        SynDBLog.Add.Log(sllWarning,'ZDBC.ExecutePrepared returned nil %',
          [fSQL], self);
      end
      else
      begin
        Props := fConnection.Properties as TSqlDBZeosConnectionProperties;
        fResultInfo := fResultSet.GetMetadata;
        n := fResultInfo.GetColumnCount;
        fColumn.Capacity := n;
        for i := 0 to n - 1 do
        begin
          name := fResultInfo.GetColumnLabel(i + FirstDbcIndex);
          if name = '' then
            name := fResultInfo.GetColumnName(i + FirstDbcIndex);
          PSqlDBColumnProperty(fColumn.AddAndMakeUniqueName(
            // Delphi<2009: already UTF-8 encoded due to controls_cp=CP_UTF8
            {$ifdef UNICODE}StringToUtf8{$endif}(name)))^.ColumnType :=
              Props.TZSQLTypeToTSqlDBFieldType(fResultInfo.GetColumnType(i + FirstDbcIndex));
        end;
      end;
    end
    else
      fStatement.ExecuteUpdatePrepared; //ExecutePrepared allways trys to determine a possible LastResultSet
  // 3. handle out parameters
  // -> TODO (fStatement is IZCallableStatement)
  // EH: that will propably also be supported with the normal prepared stmts
  // on 7.3 (base implementation in classe is ready for most drivers 01.03.2019
  // but not complete!)
  {$ifdef ZEOS72UP}
  finally
    arrayBinding.Free;
  end;
  {$endif ZEOS72UP}
  SQLLogEnd;
end;

procedure TSqlDBZeosStatement.Reset;
begin
  ReleaseRows;
  if fStatement <> nil then
    fStatement.ClearParameters;
  inherited Reset;
end;

procedure TSqlDBZeosStatement.ReleaseRows;
begin
  if fResultSet <> nil then
    fResultSet.ResetCursor;
  inherited ReleaseRows;
end;

function TSqlDBZeosStatement.Step(SeekFirst: boolean): boolean;
begin
  if fColumnCount = 0 then // no row returned
    result := false
  else if fResultSet = nil then
    raise ESqlDBZeos.CreateUtf8('%.Step() invalid self', [self])
  else if SeekFirst then
  begin
    result := fResultSet.First;
    if result then
      fCurrentRow := 1
    else
      fCurrentRow := 0;
  end
  else
  begin
    result := fResultSet.Next;
    if result then
      inc(fCurrentRow);
  end;
end;

{$if defined(ZEOS73UP) and defined(MORMOT2)}
procedure TSqlDBZeosStatement.AfterConstruction;
begin
  inherited;
  fJSONComposeOptions := [jcoEndJsonObject];
end;
{$ifend}

function TSqlDBZeosStatement.ColumnBlob(Col: integer): RawByteString;
var
  blob: IZBlob;
begin
  if (fResultSet = nil) or
     (cardinal(Col) >= cardinal(fColumnCount)) then
    raise ESqlDBZeos.CreateUtf8('%.ColumnBlob(%) ResultSet=%',
      [self, Col, fResultSet]);
  blob := fResultSet.GetBlob(Col + FirstDbcIndex);
  if (blob = nil) or
     blob.IsEmpty then
    result := ''
  else
    result := blob.GetString; // ZAnsiString = RawByteString
end;

function TSqlDBZeosStatement.ColumnCurrency(Col: integer): currency;
begin
  if (fResultSet = nil) or
     (cardinal(Col) >= cardinal(fColumnCount)) then
    raise ESqlDBZeos.CreateUtf8('%.ColumnCurrency(%) ResultSet=%',
      [self, Col, fResultSet]);
  {$ifdef ZEOS72UP}
  result := fResultSet.GetCurrency(Col + FirstDbcIndex);
  {$else}
  result := fResultSet.GetBigDecimal(Col + FirstDbcIndex);
  {$endif ZEOS72UP}
end;

function TSqlDBZeosStatement.ColumnDateTime(Col: integer): TDateTime;
begin
  if (fResultSet = nil) or
     (cardinal(Col) >= cardinal(fColumnCount)) then
    raise ESqlDBZeos.CreateUtf8('%.ColumnDateTime(%) ResultSet=%',
      [self, Col, fResultSet]);
  result := fResultSet.GetTimestamp(Col + FirstDbcIndex);
end;

function TSqlDBZeosStatement.ColumnDouble(Col: integer): double;
begin
  if (fResultSet = nil) or
     (cardinal(Col) >= cardinal(fColumnCount)) then
    raise ESqlDBZeos.CreateUtf8('%.ColumnDouble(%) ResultSet=%',
      [self, Col, fResultSet]);
  result := fResultSet.GetDouble(Col + FirstDbcIndex);
end;

function TSqlDBZeosStatement.ColumnInt(Col: integer): Int64;
begin
  if (fResultSet = nil) or
     (cardinal(Col) >= cardinal(fColumnCount)) then
    raise ESqlDBZeos.CreateUtf8('%.ColumnInt(%) ResultSet=%',
      [self, Col, fResultSet]);
  result := fResultSet.GetLong(Col + FirstDbcIndex);
end;

function TSqlDBZeosStatement.ColumnNull(Col: integer): boolean;
begin
  if (fResultSet = nil) or
     (cardinal(Col) >= cardinal(fColumnCount)) then
    raise ESqlDBZeos.CreateUtf8('%.ColumnNull(%) ResultSet=%', [self, Col, fResultSet]);
  result := fResultSet.IsNull(Col + FirstDbcIndex);
end;

function TSqlDBZeosStatement.ColumnUtf8(Col: integer): RawUtf8;
begin
  if (fResultSet = nil) or
     (cardinal(Col) >= cardinal(fColumnCount)) then
    raise ESqlDBZeos.CreateUtf8('%.ColumnUtf8(%) ResultSet=%',
      [self, Col, fResultSet]);
  {$ifdef ZEOS72UP}
  result := fResultSet.GetUTF8String(Col + FirstDbcIndex);
  {$else}
    {$ifdef UNICODE}
    StringToUtf8(fResultSet.GetString(Col + FirstDbcIndex), result);
    {$else}
    result := fResultSet.GetString(Col + FirstDbcIndex); // thanks to controls_cp=CP_UTF8
    {$endif UNICODE}
  {$endif ZEOS72UP}
end;

function TSqlDBZeosStatement.UpdateCount: integer;
begin
  if fStatement <> nil then
    result := fStatement.GetUpdateCount
  else
    result := 0;
end;

{$ifdef ZEOS72UP}

procedure TSqlDBZeosStatement.ColumnsToJson(WR: TJsonWriter);

{$if not (defined(ZEOS73UP) and defined(MORMOT2))}
var
  col: integer;
  P: PAnsiChar;
  Len: NativeUInt; // required by Zeos for GetPAnsiChar out param (not PtrUInt)

  procedure WriteIZBlob;
  var
    blob: IZBlob;
    raw: RawByteString;
  begin
    blob := fResultSet.GetBlob(col + FirstDbcIndex);
    raw := blob.GetString;
    WR.WrBase64(pointer(raw), length(raw), {withmagic=}true); // withMagic=true
  end;
{$ifend}

begin
  // take care of the layout of internal ZDBC buffers for each provider
  {$if defined(ZEOS73UP) and defined(MORMOT2)}
  fResultSet.ColumnsToJson(WR, fJSONComposeOptions);
  {$else}
  if WR.Expand then
    WR.Add('{');
  for col := 0 to fColumnCount - 1 do
  begin
    if WR.Expand then
      WR.AddFieldName(fColumns[col].ColumnName); // add '"ColumnName":'
    if fResultSet.IsNull(col + FirstDbcIndex) then
      WR.AddNull
    else
    begin
      case fColumns[col].ColumnType of
        ftNull:
          WR.AddNull;
        ftInt64:
          if fDBMS in [dMySQL, dPostgreSQL] then
          begin
            P := fResultSet.GetPAnsiChar(col + FirstDbcIndex, Len);
            WR.AddNoJsonEscape(P, Len);
          end
          else
            WR.Add(fResultSet.GetLong(col + FirstDbcIndex));
        ftDouble:
          if fDBMS in [dMySQL, dPostgreSQL] then
          begin
            P := fResultSet.GetPAnsiChar(col + FirstDbcIndex, Len);
            WR.AddNoJsonEscape(P, Len);
          end
          else
            WR.AddDouble(fResultSet.GetDouble(col + FirstDbcIndex));
        ftCurrency:
          if fDBMS = dSQLite then
            WR.AddDouble(fResultSet.GetDouble(col + FirstDbcIndex))
          else
            WR.AddCurr(fResultSet.GetCurrency(col + FirstDbcIndex));
        ftDate:
          begin
            WR.Add('"');
            WR.AddDateTime(fResultSet.GetTimestamp(col + FirstDbcIndex),
              fForceDateWithMS);
            WR.Add('"');
          end;
        ftUtf8:
          begin
            WR.Add('"');
            if fDBMS = dMSSQL then
            begin
              P := Pointer(fResultSet.GetPWideChar(col + FirstDbcIndex, Len));
              WR.AddJsonEscapeW(Pointer(P), Len);
            end
            else
            begin
              P := fResultSet.GetPAnsiChar(col + FirstDbcIndex, Len);
              WR.AddJsonEscape(P, Len);
            end;
            WR.Add('"');
          end;
        ftBlob:
          if fForceBlobAsNull then
            WR.AddNull
          else if fDBMS in [dMySQL, dSQLite] then
          begin
            P := fResultSet.GetPAnsiChar(col + FirstDbcIndex, Len);
            WR.WrBase64(P, Len, true); // withMagic=true
          end
          else
            WriteIZBlob;
      else
        raise ESqlDBException.CreateUtf8(
          '%.ColumnsToJson: invalid ColumnType(#% "%")=%',
          [self, col, fColumns[col].ColumnName, ord(fColumns[col].ColumnType)]);
      end;
    end;
    WR.AddComma;
  end;
  WR.CancelLastComma; // cancel last ','
  if WR.Expand then
    WR.Add('}');
  {$ifend}
end;

{$endif ZEOS72UP}

initialization
  TSqlDBZeosConnectionProperties.RegisterClassNameForDefinition;

{$endif NOSYNDBZEOS}
// defined in mormot2.lpk Lazarus package > Custom Options > Defines

end.

