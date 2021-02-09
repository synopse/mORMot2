/// Database Framework Direct OleDB Connection
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.sql.oledb;

{
  *****************************************************************************

   Efficient SQL Database Connection via OleDB 
    - TSqlDBOleDBConnection* and TSqlDBOleDBStatement Classes
    - Database Engine Specific OleDB Connection Classes

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

{$ifdef OSWINDOWS} // compiles as void unit for non-Windows - allow Lazarus package

uses
  sysutils,
  classes,
  variants,
  Windows, // OleDB is a Windows-specific protocol
  ActiveX,
  ComObj,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.datetime,
  mormot.core.data,
  mormot.core.rtti,
  mormot.core.perf,
  mormot.core.log,
  mormot.db.core,
  mormot.db.sql,
  mormot.db.raw.oledb;


{ ************ TSqlDBOleDBConnection* and TSqlDBOleDBStatement Classes }

type
  TSqlDBOleDBConnection = class;

  TSqlDBOleDBOnCustomError = function(Connection: TSqlDBOleDBConnection;
    ErrorRecords: IErrorRecords; RecordNum: cardinal): boolean of object;

  /// will implement properties shared by OleDB connections
  TSqlDBOleDBConnectionProperties = class(TSqlDBConnectionPropertiesThreadSafe)
  protected
    fProviderName: RawUtf8;
    fConnectionString: SynUnicode;
    fOnCustomError: TSqlDBOleDBOnCustomError;
    fSchemaRec: array of TDBSchemaRec;
    fSupportsOnlyIRowset: boolean;
    function GetSchema(const aUID: TGUID; const Fields: array of RawUtf8;
      var aResult: IRowSet): boolean;
    /// will create the generic fConnectionString from supplied parameters
    procedure SetInternalProperties; override;
    /// initialize fForeignKeys content with all foreign keys of this DB
    // - used by GetForeignKey method
    procedure GetForeignKeys; override;
    /// create the database
    // - shall be called only if necessary (e.g. for file-based database, if
    // the file does not exist yet)
    function CreateDatabase: boolean; virtual;
  public
    /// create a new connection
    // - call this method if the shared MainConnection is not enough (e.g. for
    // multi-thread access)
    // - the caller is responsible of freeing this instance
    // - this overridden method will create an TSqlDBOleDBConnection instance
    function NewConnection: TSqlDBConnection; override;
    /// display the OleDB/ADO Connection Settings dialog to customize the
    // OleDB connection string
    // - returns TRUE if the connection string has been modified
    // - Parent is an optional GDI Window Handle for modal display
    function ConnectionStringDialogExecute(Parent: HWND = 0): boolean;
    /// get all table names
    // - will retrieve the corresponding metadata from OleDB interfaces if SQL
    // direct access was not defined
    procedure GetTableNames(out Tables: TRawUtf8DynArray); override;
    /// retrieve the column/field layout of a specified table
    // - will retrieve the corresponding metadata from OleDB interfaces if SQL
    // direct access was not defined
    procedure GetFields(const aTableName: RawUtf8; out Fields: TSqlDBColumnDefineDynArray); override;
    /// convert a textual column data type, as retrieved e.g. from SqlGetField,
    // into our internal primitive types
    function ColumnTypeNativeToDB(const aNativeType: RawUtf8; aScale: integer): TSqlDBFieldType; override;
    /// the associated OleDB connection string
    // - is set by the Create() constructor most of the time from the supplied
    // server name, user id and password, according to the database provider
    // corresponding to the class
    // - you may want to customize it via the ConnectionStringDialogExecute
    // method, or to provide some additional parameters
    property ConnectionString: SynUnicode
      read fConnectionString write fConnectionString;
    /// custom Error handler for OleDB COM objects
    // - returns TRUE if specific error was retrieved and has updated
    // ErrorMessage and InfoMessage
    // - default implementation just returns false
    property OnCustomError: TSqlDBOleDBOnCustomError
      read fOnCustomError write fOnCustomError;
  published { to be loggged as JSON }
    /// the associated OleDB provider name, as set for each class
    property ProviderName: RawUtf8
      read fProviderName;
  end;

  /// implements an OleDB connection
  // - will retrieve the remote DataBase behavior from a supplied
  // TSqlDBConnectionProperties class, shared among connections
  TSqlDBOleDBConnection = class(TSqlDBConnectionThreadSafe)
  protected
    fMalloc: IMalloc;
    fDBInitialize: IDBInitialize;
    fTransaction: ITransactionLocal;
    fSession: IUnknown;
    fOleDBProperties: TSqlDBOleDBConnectionProperties;
    fOleDBErrorMessage, fOleDBInfoMessage: string;
    /// Error handler for OleDB COM objects
    // - will update ErrorMessage and InfoMessage
    procedure OleDBCheck(aStmt: TSqlDBStatement; aResult: HRESULT;
      const aStatus: TCardinalDynArray = nil); virtual;
    /// called just after fDBInitialize.Initialized: could add parameters
    procedure OnDBInitialized; virtual;
  public
    /// connect to a specified OleDB database
    constructor Create(aProperties: TSqlDBConnectionProperties); override;
    /// release all associated memory and OleDB COM objects
    destructor Destroy; override;
    /// initialize a new SQL query statement for the given connection
    // - the caller should free the instance after use
    function NewStatement: TSqlDBStatement; override;
    /// connect to the specified database
    // - should raise an EOleDBException on error
    procedure Connect; override;
    /// stop connection to the specified database
    // - should raise an EOleDBException on error
    procedure Disconnect; override;
    /// return TRUE if Connect has been already successfully called
    function IsConnected: boolean; override;
    /// begin a Transaction for this connection
    // - be aware that not all OleDB provider support nested transactions
    // see http://msdn.microsoft.com/en-us/library/ms716985(v=vs.85).aspx
    procedure StartTransaction; override;
    /// commit changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Commit; override;
    /// discard changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Rollback; override;
    /// the associated OleDB database properties
    property OleDBProperties: TSqlDBOleDBConnectionProperties
      read fOleDBProperties;
    /// internal error message, as retrieved from the OleDB provider
    property OleDBErrorMessage: string
      read fOleDBErrorMessage;
    /// internal information message, as retrieved from the OleDB provider
    property OleDBInfoMessage: string
      read fOleDBInfoMessage;
  end;

  /// used to store properties and value about one TSqlDBOleDBStatement Param
  // - we don't use a Variant, not the standard TSqlDBParam record type,
  // but manual storage for better performance
  // - whole memory block of a TSqlDBOleDBStatementParamDynArray will be used as the
  // source Data for the OleDB parameters - so we should align data carefully
  {$ifdef CPU64}
    {$A8} // un-packed records
  {$else}
    {$A-} // packed records
  {$endif}
  TSqlDBOleDBStatementParam = record
    /// storage used for BLOB (ftBlob) values
    // - will be refered as DBTYPE_BYREF when sent as OleDB parameters, to
    // avoid unnecessary memory copy
    VBlob: RawByteString;
    /// storage used for TEXT (ftUtf8) values
    // - we store TEXT here as WideString, and not RawUtf8, since OleDB
    // expects the text to be provided with Unicode encoding
    // - for some providers (like Microsoft SQL Server 2008 R2, AFAIK), using
    // DBTYPE_WSTR value (i.e. what the doc. says) will raise an OLEDB Error
    // 80040E1D (DB_E_UNSUPPORTEDCONVERSION, i.e. 'Requested conversion is not
    // supported'): we found out that only DBTYPE_BSTR type (i.e. OLE WideString)
    // does work... so we'll use it here! Shame on Microsoft!
    // - what's fine with DBTYPE_BSTR is that it can be resized by the provider
    // in case of VInOut in [paramOut, paramInOut] - so let it be
    VText: WideString;
    /// storage used for ftInt64, ftDouble, ftDate and ftCurrency value
    VInt64: Int64;
    /// storage used for table variables
    VIUnknown: IUnknown;
    /// storage used for table variables
    VArray: TRawUtf8DynArray;
    /// storage used for the OleDB status field
    // - if VStatus=ord(stIsNull), then it will bind a NULL with the type
    // as set by VType (to avoid conversion error like in [e8c211062e])
    VStatus: integer;
    /// the column/parameter Value type
    VType: TSqlDBFieldType;
    /// define if parameter can be retrieved after a stored procedure execution
    VInOut: TSqlDBParamInOutType;
    // so that VInt64 will be 8 bytes aligned
    VFill: array[sizeof(TSqlDBFieldType)+sizeof(TSqlDBParamInOutType)+sizeof(integer)..
      SizeOf(Int64)-1] of byte;
  end;
  {$ifdef CPU64}
    {$A-} // packed records
  {$endif}
  POleDBStatementParam = ^TSqlDBOleDBStatementParam;

  /// used to store properties about TSqlDBOleDBStatement Parameters
  // - whole memory block of a TSqlDBOleDBStatementParamDynArray will be used as the
  // source Data for the OleDB parameters
  TSqlDBOleDBStatementParamDynArray = array of TSqlDBOleDBStatementParam;

  /// implements an OleDB SQL query statement
  // - this statement won't retrieve all rows of data, but will allow direct
  // per-row access using the Step() and Column*() methods
  TSqlDBOleDBStatement = class(TSqlDBStatement)
  protected
    fParams: TSqlDBOleDBStatementParamDynArray;
    fColumns: TSqlDBColumnPropertyDynArray;
    fParam: TDynArray;
    fColumn: TDynArrayHashed;
    fCommand: ICommandText;
    fRowSet: IRowSet;
    fRowSetAccessor: HACCESSOR;
    fRowSize: integer;
    fRowStepResult: HRESULT;
    fRowStepHandleRetrieved: PtrUInt;
    fRowStepHandleCurrent: PtrUInt;
    fRowStepHandles: TPtrUIntDynArray;
    fRowSetData: array of byte;
    fParamBindings: TDBBindingDynArray;
    fColumnBindings: TDBBindingDynArray;
    fHasColumnValueInlined: boolean;
    fOleDBConnection: TSqlDBOleDBConnection;
    fDBParams: TDBParams;
    fRowBufferSize: integer;
    fUpdateCount: integer;
    fAlignBuffer: boolean;
    procedure SetRowBufferSize(Value: integer);
    /// resize fParams[] if necessary, set the VType and return pointer to
    // the corresponding entry in fParams[]
    // - first parameter has Param=1
    function CheckParam(Param: integer; NewType: TSqlDBFieldType;
      IO: TSqlDBParamInOutType): POleDBStatementParam; overload;
    function CheckParam(Param: integer; NewType: TSqlDBFieldType;
      IO: TSqlDBParamInOutType; ArrayCount: integer): POleDBStatementParam; overload;
    /// raise an exception if Col is incorrect or no IRowSet is available
    // - set Column to the corresponding fColumns[] item
    // - return a pointer to status-data[-length] in fRowSetData[], or
    // nil if status states this column is NULL
    function GetCol(Col: integer; out Column: PSqlDBColumnProperty): pointer;
    procedure GetCol64(Col: integer; DestType: TSqlDBFieldType; var Dest);
      {$ifdef HASINLINE}inline;{$endif}
    procedure FlushRowSetData;
    procedure ReleaseRowSetDataAndRows;
    procedure CloseRowSet;
    ///  retrieve column information, and initialize Bindings[]
    // - add the high-level column information in Column[], initializes
    // OleDB Bindings array and returns the row size (in bytes)
    function BindColumns(ColumnInfo: IColumnsInfo; var Column: TDynArrayHashed;
      out Bindings: TDBBindingDynArray): integer;
    procedure LogStatusError(Status: integer; Column: PSqlDBColumnProperty);
  public
    /// create an OleDB statement instance, from an OleDB connection
    // - the Execute method can be called only once per TSqlDBOleDBStatement instance
    // - if the supplied connection is not of TSqlDBOleDBConnection type, will raise
    // an exception
    constructor Create(aConnection: TSqlDBConnection); override;
    /// release all associated memory and COM objects
    destructor Destroy; override;
    /// retrieve column information from a supplied IRowSet
    // - is used e.g. by TSqlDBOleDBStatement.Execute or to retrieve metadata columns
    // - raise an exception on error
    procedure FromRowSet(RowSet: IRowSet);

    /// bind a NULL value to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - OleDB during MULTI INSERT statements expect BoundType to be set in
    // TSqlDBOleDBStatementParam, and its VStatus set to ord(stIsNull)
    // - raise an EOleDBException on any error
    procedure BindNull(Param: integer; IO: TSqlDBParamInOutType = paramIn;
      BoundType: TSqlDBFieldType = ftNull); override;
    /// bind an array of Int64 values to a parameter
    // - using TABLE variable (MSSQl 2008 & UP). Must be created in the database as:
    // $ CREATE TYPE dbo.IDList AS TABLE(id bigint NULL)
    // - Internally BindArray(0, [1, 2,3]) is the same as:
    // $ declare @a dbo.IDList;
    // $ insert into @a (id) values (1), (2), (3);
    // $ SELECT usr.ID   FROM user usr WHERE usr.ID IN  (select id from @a)
    procedure BindArray(Param: integer;
      const Values: array of Int64); overload; override;
    /// bind a array of RawUtf8 (255 length max) values to a parameter
    // - using TABLE variable (MSSQl 2008 & UP). Must be created in the database as:
    // $ CREATE TYPE dbo.StrList AS TABLE(id nvarchar(255) NULL)
    // - must be declareded in the database
    procedure BindArray(Param: integer;
      const Values: array of RawUtf8); overload; override;
    /// bind an integer value to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an EOleDBException on any error
    procedure Bind(Param: integer; Value: Int64;
      IO: TSqlDBParamInOutType = paramIn); overload; override;
    /// bind a double value to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an EOleDBException on any error
    procedure Bind(Param: integer; Value: double;
      IO: TSqlDBParamInOutType = paramIn); overload; override;
    /// bind a TDateTime value to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an EOleDBException on any error
    procedure BindDateTime(Param: integer; Value: TDateTime;
      IO: TSqlDBParamInOutType = paramIn); overload; override;
    /// bind a currency value to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an EOleDBException on any error
    procedure BindCurrency(Param: integer; Value: currency;
      IO: TSqlDBParamInOutType = paramIn); overload; override;
    /// bind a UTF-8 encoded string to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an EOleDBException on any error
    procedure BindTextU(Param: integer; const Value: RawUtf8;
      IO: TSqlDBParamInOutType = paramIn); overload; override;
    /// bind a UTF-8 encoded buffer text (#0 ended) to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an EOleDBException on any error
    procedure BindTextP(Param: integer; Value: PUtf8Char;
      IO: TSqlDBParamInOutType = paramIn); overload; override;
    /// bind a VCL string to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an EOleDBException on any error
    procedure BindTextS(Param: integer; const Value: string;
      IO: TSqlDBParamInOutType = paramIn); overload; override;
    /// bind an OLE WideString to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an EOleDBException on any error
    procedure BindTextW(Param: integer; const Value: WideString;
      IO: TSqlDBParamInOutType = paramIn); overload; override;
    /// bind a Blob buffer to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an EOleDBException on any error
    procedure BindBlob(Param: integer; Data: pointer; Size: integer;
      IO: TSqlDBParamInOutType = paramIn); overload; override;
    /// bind a Blob buffer to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an EOleDBException on any error
    procedure BindBlob(Param: integer; const Data: RawByteString;
      IO: TSqlDBParamInOutType = paramIn); overload; override;

    /// Prepare an UTF-8 encoded SQL statement
    // - parameters marked as ? will be bound later, before ExecutePrepared call
    // - if ExpectResults is TRUE, then Step() and Column*() methods are available
    // to retrieve the data rows
    // - raise an EOleDBException on any error
    procedure Prepare(const aSql: RawUtf8; ExpectResults: boolean = false); overload; override;
    /// Execute an UTF-8 encoded SQL statement
    // - parameters marked as ? should have been already bound with Bind*()
    // functions above
    // - raise an EOleDBException on any error
    procedure ExecutePrepared; override;
    /// Reset the previous prepared statement
    // - this overridden implementation will reset all bindings and the cursor state
    // - raise an EOleDBException on any error
    procedure Reset; override;
    /// gets a number of updates made by latest executed statement
    function UpdateCount: integer; override;

    /// retrieve the parameter content, after SQL execution
    // - the leftmost SQL parameter has an index of 1
    // - to be used e.g. with stored procedures
    // - any TEXT parameter will be retrieved as WideString Variant (i.e. as
    // stored in TSqlDBOleDBStatementParam)
    function ParamToVariant(Param: integer; var Value: Variant;
      CheckIsOutParameter: boolean = true): TSqlDBFieldType; override;

    /// after a statement has been prepared via Prepare() + ExecutePrepared() or
    // Execute(), this method must be called one or more times to evaluate it
    // - you shall call this method before calling any Column*() methods
    // - return TRUE on success, with data ready to be retrieved by Column*()
    // - return FALSE if no more row is available (e.g. if the SQL statement
    // is not a SELECT but an UPDATE or INSERT command)
    // - access the first or next row of data from the SQL Statement result:
    // if SeekFirst is TRUE, will put the cursor on the first row of results,
    // otherwise, it will fetch one row of data, to be called within a loop
    // - raise an EOleDBException on any error
    function Step(SeekFirst: boolean = false): boolean; override;
    /// clear result rowset when ISqlDBStatement is back in cache
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
    // - FieldSize can be set to store the size in chars of a ftUtf8 column
    // (0 means BLOB kind of TEXT column)
    function ColumnType(Col: integer;
      FieldSize: PInteger = nil): TSqlDBFieldType; override;
    /// returns TRUE if the column contains NULL
    function ColumnNull(Col: integer): boolean; override;
    /// return a Column integer value of the current Row, first Col is 0
    function ColumnInt(Col: integer): Int64; override;
    /// return a Column floating point value of the current Row, first Col is 0
    function ColumnDouble(Col: integer): double; override;
    /// return a Column date and time value of the current Row, first Col is 0
    function ColumnDateTime(Col: integer): TDateTime; override;
    /// return a Column currency value of the current Row, first Col is 0
    // - should retrieve directly the 64 bit Currency content, to avoid
    // any rounding/conversion error from floating-point types
    function ColumnCurrency(Col: integer): currency; override;
    /// return a Column UTF-8 encoded text value of the current Row, first Col is 0
    function ColumnUtf8(Col: integer): RawUtf8; override;
    /// return a Column text generic VCL string value of the current Row, first Col is 0
    function ColumnString(Col: integer): string; override;
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
    /// return a Column as a variant
    // - this implementation will retrieve the data with no temporary variable
    // (since TQuery calls this method a lot, we tried to optimize it)
    // - a ftUtf8 content will be mapped into a generic WideString variant
    // for pre-Unicode version of Delphi, and a generic UnicodeString (=string)
    // since Delphi 2009: you may not loose any data during charset conversion
    // - a ftBlob content will be mapped into a TBlobData AnsiString variant
    function ColumnToVariant(Col: integer; var Value: Variant): TSqlDBFieldType; override;
    /// just map the original Collection into a TSqlDBOleDBConnection class
    property OleDBConnection: TSqlDBOleDBConnection
      read fOleDBConnection;
    /// if TRUE, the data will be 8 bytes aligned in OleDB internal buffers
    // - it's recommended by official OleDB documentation for faster process
    // - is enabled by default, and should not be modified in most cases
    property AlignDataInternalBuffer: boolean
      read fAlignBuffer write fAlignBuffer;
    /// size in bytes of the internal OleDB buffer used to fetch rows
    // - several rows are retrieved at once into the internal buffer
    // - default value is 16384 bytes, minimal allowed size is 8192
    property RowBufferSize: integer
      read fRowBufferSize write SetRowBufferSize;
  end;


{ ************ Database Engine Specific OleDB Connection Classes }

type
  /// OleDB connection properties to an Oracle database using Oracle's Provider
  // - this will use the native OleDB provider supplied by Oracle
  // see @http://download.oracle.com/docs/cd/E11882_01/win.112/e17726/toc.htm
  TSqlDBOleDBOracleConnectionProperties = class(TSqlDBOleDBConnectionProperties)
  protected
    /// will set the appropriate provider name, i.e. 'OraOLEDB.Oracle.1'
    procedure SetInternalProperties; override;
  end;

  /// OleDB connection properties to an Oracle database using Microsoft's Provider
  // - this will use the generic (older) OleDB provider supplied by Microsoft
  // which would not be used any more:
  // "This feature will be removed in a future version of Windows. Avoid
  // using this feature in new development work, and plan to modify applications
  // that currently use this feature. Instead, use Oracle's OLE DB provider."
  // see http://msdn.microsoft.com/en-us/library/ms675851
  TSqlDBOleDBMSOracleConnectionProperties = class(TSqlDBOleDBOracleConnectionProperties)
  protected
    /// will set the appropriate provider name, i.e. 'MSDAORA'
    procedure SetInternalProperties; override;
  end;

  /// OleDB connection properties to Microsoft SQL Server 2008-2012, via
  // SQL Server Native Client 10.0 (SQL Server 2008)
  // - this will use the native OleDB provider supplied by Microsoft
  // see http://msdn.microsoft.com/en-us/library/ms677227
  // - is aUserID='' at Create, it will use Windows Integrated Security
  // for the connection
  // - will use the SQLNCLI10 provider, which will work on Windows XP;
  // if you want all features, especially under MS SQL 2012, use the
  // inherited class TSqlDBOleDBMSSQL2012ConnectionProperties; if, on the other
  // hand, you need to connect to a old MS SQL Server 2005, use
  // TSqlDBOleDBMSSQL2005ConnectionProperties, or set your own provider string
  TSqlDBOleDBMSSQLConnectionProperties = class(TSqlDBOleDBConnectionProperties)
  protected
    /// will set the appropriate provider name, i.e. 'SQLNCLI10'
    procedure SetInternalProperties; override;
    /// custom Error handler for OleDB COM objects
    // - will handle Microsoft SQL Server error messages (if any)
    function MSOnCustomError(Connection: TSqlDBOleDBConnection;
      ErrorRecords: IErrorRecords; RecordNum: cardinal): boolean;
  public
  end;

  /// OleDB connection properties to Microsoft SQL Server 2005, via
  // SQL Server Native Client (SQL Server 2005)
  // - this overridden version will use the SQLNCLI provider, which is
  // deprecated but may be an alternative with MS SQL Server 2005
  // - is aUserID='' at Create, it will use Windows Integrated Security
  // for the connection
  TSqlDBOleDBMSSQL2005ConnectionProperties = class(TSqlDBOleDBMSSQLConnectionProperties)
  protected
    /// will set the appropriate provider name, i.e. 'SQLNCLI'
    procedure SetInternalProperties; override;
  public
    /// initialize the connection properties
    // - this overridden version will disable the MultipleValuesInsert()
    // optimization as defined in TSqlDBConnectionProperties.Create(),
    // since INSERT with multiple VALUES (..),(..),(..) is available only
    // since SQL Server 2008
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUtf8); override;
  end;

  /// OleDB connection properties to Microsoft SQL Server 2008, via
  // SQL Server Native Client 10.0 (SQL Server 2008)
  // - just maps default TSqlDBOleDBMSSQLConnectionProperties type
  TSqlDBOleDBMSSQL2008ConnectionProperties = TSqlDBOleDBMSSQLConnectionProperties;

  /// OleDB connection properties to Microsoft SQL Server 2008/2012, via
  // SQL Server Native Client 11.0 (Microsoft SQL Server 2012 Native Client)
  // - from http://www.microsoft.com/en-us/download/details.aspx?id=29065 get
  // the sqlncli.msi package corresponding to your Operating System: note that
  // the "X64 Package" will also install the 32-bit version of the client
  // - this overridden version will use newer SQLNCLI11 provider, but won't work
  // under Windows XP - in this case, it will fall back to SQLNCLI10 - see
  // http://msdn.microsoft.com/en-us/library/ms131291
  // - if aUserID='' at Create, it will use Windows Integrated Security
  // for the connection
  // - for SQL Express LocalDB edition, just use aServerName='(localdb)\v11.0'
  TSqlDBOleDBMSSQL2012ConnectionProperties = class(TSqlDBOleDBMSSQLConnectionProperties)
  protected
    /// will set the appropriate provider name, i.e. 'SQLNCLI11'
    // - will leave older 'SQLNCLI10' on Windows XP
    procedure SetInternalProperties; override;
  end;

  /// OleDB connection properties to MySQL Server
  TSqlDBOleDBMySQLConnectionProperties = class(TSqlDBOleDBConnectionProperties)
  protected
    /// will set the appropriate provider name, i.e. 'MySqlProv'
    procedure SetInternalProperties; override;
  end;

{$ifndef CPU64} // Jet is not available on Win64

  /// OleDB connection properties to Jet/MSAccess .mdb files
  // - the server name should be the .mdb file name
  // - note that the Jet OleDB driver is not available under Win64 platform
  TSqlDBOleDBJetConnectionProperties = class(TSqlDBOleDBConnectionProperties)
  protected
    /// will set the appropriate provider name, i.e. 'Microsoft.Jet.OLEDB.4.0'
    procedure SetInternalProperties; override;
  end;

{$endif CPU64}

  /// OleDB connection properties to Microsoft Access Database
  TSqlDBOleDBACEConnectionProperties = class(TSqlDBOleDBConnectionProperties)
  protected
    /// will set the appropriate provider name, i.e. 'Microsoft.ACE.OLEDB.12.0'
    procedure SetInternalProperties; override;
  end;

  /// OleDB connection properties to IBM AS/400
  TSqlDBOleDBAS400ConnectionProperties = class(TSqlDBOleDBConnectionProperties)
  protected
    /// will set the appropriate provider name, i.e. 'IBMDA400.DataSource.1'
    procedure SetInternalProperties; override;
  end;

  /// OleDB connection properties to Informix Server
  TSqlDBOleDBInformixConnectionProperties = class(TSqlDBOleDBConnectionProperties)
  protected
    /// will set the appropriate provider name, i.e. 'Ifxoledbc'
    procedure SetInternalProperties; override;
  end;

  /// OleDB connection properties via Microsoft Provider for ODBC
  // - this will use the ODBC provider supplied by Microsoft
  // see http://msdn.microsoft.com/en-us/library/ms675326(v=VS.85).aspx
  // - an ODBC Driver should be specified at creation
  // - you should better use direct connection classes, like
  // TSqlDBOleDBMSSQLConnectionProperties or TSqlDBOleDBOracleConnectionProperties
  // as defined in mormot.db.sql.odbc.pas
  TSqlDBOleDBOdbcSQLConnectionProperties = class(TSqlDBOleDBConnectionProperties)
  protected
    fDriver: RawUtf8;
    /// will set the appropriate provider name, i.e. 'MSDASQL'
    procedure SetInternalProperties; override;
  public
    /// initialize the properties
    // - an additional parameter is available to set the ODBC driver to use
    // - you may also set aDriver='' and modify the connection string directly,
    // e.g. adding '{ DSN=name | FileDSN=filename };'
    constructor Create(const aDriver, aServerName, aDatabaseName,
      aUserID, aPassWord: RawUtf8); reintroduce;
  published { to be logged as JSON }
    /// the associated ODBC Driver name, as specified at creation
    property Driver: RawUtf8
      read fDriver;
  end;


{$ifndef PUREMORMOT2}
// backward compatibility types redirections

type
  TOleDBConnectionProperties = TSqlDBOleDBConnectionProperties;
  TOleDBOracleConnectionProperties = TSqlDBOleDBOracleConnectionProperties;
  TOleDBMSOracleConnectionProperties = TSqlDBOleDBMSOracleConnectionProperties;
  TOleDBMSSQLConnectionProperties = TSqlDBOleDBMSSQLConnectionProperties;
  TOleDBMSSQL2005ConnectionProperties = TSqlDBOleDBMSSQL2005ConnectionProperties;
  TOleDBMSSQL2008ConnectionProperties = TSqlDBOleDBMSSQL2008ConnectionProperties;
  TOleDBMSSQL2012ConnectionProperties = TSqlDBOleDBMSSQL2012ConnectionProperties;
  TOleDBMySQLConnectionProperties = TSqlDBOleDBMySQLConnectionProperties;
  {$ifndef CPU64} // Jet is not available on Win64
  TOleDBJetConnectionProperties = TSqlDBOleDBJetConnectionProperties;
  {$endif}
  TOleDBACEConnectionProperties = TSqlDBOleDBACEConnectionProperties;
  TOleDBAS400ConnectionProperties = TSqlDBOleDBAS400ConnectionProperties;
  TOleDBOdbcSQLConnectionProperties = TSqlDBOleDBOdbcSQLConnectionProperties;

{$endif PUREMORMOT2}


implementation

{ ************ TSqlDBOleDBConnection* and TSqlDBOleDBStatement Classes }

{ TSqlDBOleDBStatement }

procedure TSqlDBOleDBStatement.BindTextU(Param: integer; const Value: RawUtf8;
  IO: TSqlDBParamInOutType);
begin
  if (Value = '') and
     fConnection.Properties.StoreVoidStringAsNull then
    CheckParam(Param, ftNull, IO)
  else
    Utf8ToWideString(Value, CheckParam(Param, ftUtf8, IO)^.VText);
end;

procedure TSqlDBOleDBStatement.BindTextP(Param: integer; Value: PUtf8Char;
  IO: TSqlDBParamInOutType);
begin
  if (Value = '') and
     fConnection.Properties.StoreVoidStringAsNull then
    CheckParam(Param, ftNull, IO)
  else
    Utf8ToWideString(Value, StrLen(Value), CheckParam(Param, ftUtf8, IO)^.VText);
end;

procedure TSqlDBOleDBStatement.BindTextS(Param: integer; const Value: string;
  IO: TSqlDBParamInOutType);
begin
  if (Value = '') and
     fConnection.Properties.StoreVoidStringAsNull then
    CheckParam(Param, ftNull, IO)
  else
    CheckParam(Param, ftUtf8, IO)^.VText := StringToSynUnicode(Value);
end;

procedure TSqlDBOleDBStatement.BindTextW(Param: integer; const Value: WideString;
  IO: TSqlDBParamInOutType);
begin
  if (Value = '') and
     fConnection.Properties.StoreVoidStringAsNull then
    CheckParam(Param, ftNull, IO)
  else
    CheckParam(Param, ftUtf8, IO)^.VText := Value;
end;

procedure TSqlDBOleDBStatement.BindBlob(Param: integer;
  const Data: RawByteString; IO: TSqlDBParamInOutType);
begin
  CheckParam(Param, ftBlob, IO)^.VBlob := Data;
end;

procedure TSqlDBOleDBStatement.BindBlob(Param: integer; Data: pointer;
  Size: integer; IO: TSqlDBParamInOutType);
begin
  SetString(CheckParam(Param, ftBlob, IO)^.VBlob, PAnsiChar(Data), Size);
end;

procedure TSqlDBOleDBStatement.Bind(Param: integer; Value: double;
  IO: TSqlDBParamInOutType);
begin
  CheckParam(Param, ftDouble, IO)^.VInt64 := PInt64(@Value)^;
end;

procedure TSqlDBOleDBStatement.BindArray(Param: integer; const Values: array of Int64);
var
  i: integer;
begin
  with CheckParam(Param, ftInt64, paramIn, length(Values))^ do
    for i := 0 to high(Values) do
      VArray[i] := Int64ToUtf8(Values[i]);
end;

procedure TSqlDBOleDBStatement.BindArray(Param: integer; const Values: array of RawUtf8);
var
  i: integer;
  StoreVoidStringAsNull: boolean;
begin
  StoreVoidStringAsNull := fConnection.Properties.StoreVoidStringAsNull;
  with CheckParam(Param, ftUtf8, paramIn, length(Values))^ do
    for i := 0 to high(Values) do
      if StoreVoidStringAsNull and
         (Values[i] = '') then
        VArray[i] := 'null'
      else
        QuotedStr(Values[i], '''', VArray[i]);
end;

procedure TSqlDBOleDBStatement.Bind(Param: integer; Value: Int64;
  IO: TSqlDBParamInOutType);
begin
  CheckParam(Param, ftInt64, IO)^.VInt64 := Value;
end;

procedure TSqlDBOleDBStatement.BindCurrency(Param: integer; Value: currency;
  IO: TSqlDBParamInOutType);
begin
  CheckParam(Param, ftCurrency, IO)^.VInt64 := PInt64(@Value)^;
end;

procedure TSqlDBOleDBStatement.BindDateTime(Param: integer; Value: TDateTime;
  IO: TSqlDBParamInOutType);
begin
  CheckParam(Param, ftDate, IO)^.VInt64 := PInt64(@Value)^;
end;

procedure TSqlDBOleDBStatement.BindNull(Param: integer; IO: TSqlDBParamInOutType;
  BoundType: TSqlDBFieldType);
begin
  CheckParam(Param, BoundType, IO)^.VStatus := ord(stIsNull);
end;

function TSqlDBOleDBStatement.CheckParam(Param: integer;
  NewType: TSqlDBFieldType; IO: TSqlDBParamInOutType): POleDBStatementParam;
begin
  if Param <= 0 then
    raise EOleDBException.CreateUtf8('%.Bind*() called with Param=% should be >= 1',
      [self, Param]);
  if Param > fParamCount then
    fParam.Count := Param; // resize fParams[] dynamic array if necessary
  result := @fParams[Param - 1];
  result^.VType := NewType;
  result^.VInOut := IO;
  result^.VStatus := 0;
end;

function TSqlDBOleDBStatement.CheckParam(Param: integer; NewType: TSqlDBFieldType;
  IO: TSqlDBParamInOutType; ArrayCount: integer): POleDBStatementParam;
begin
  result := CheckParam(Param, NewType, IO);
  if (NewType in [ftUnknown, ftNull]) or
     (fConnection.Properties.BatchSendingAbilities *
       [cCreate, cUpdate, cDelete] = []) then
    raise ESqlDBException.CreateUtf8(
      'Invalid call to %s.BindArray(Param=%d,Type=%s)',
      [self, Param, TSqlDBFieldTypeToString(NewType)]);
  SetLength(result^.VArray, ArrayCount);
  result^.VInt64 := ArrayCount;
end;

constructor TSqlDBOleDBStatement.Create(aConnection: TSqlDBConnection);
begin
  if not aConnection.InheritsFrom(TSqlDBOleDBConnection) then
    raise EOleDBException.CreateUtf8('%.Create(%) expects a TSqlDBOleDBConnection',
      [self, aConnection]);
  inherited Create(aConnection);
  fOleDBConnection := TSqlDBOleDBConnection(aConnection);
  fParam.Init(TypeInfo(TSqlDBOleDBStatementParamDynArray), fParams, @fParamCount);
  fColumn.InitSpecific(TypeInfo(TSqlDBColumnPropertyDynArray), fColumns,
    ptRawUtf8, @fColumnCount, {caseinsens=}true);
  fRowBufferSize := 16384;
  fAlignBuffer := true;
end;

type
  TColumnValue = packed record
    Status: PtrInt;
    Length: PtrUInt; // ignored for alignment
    case integer of
      0:
        (Int64: Int64);
      1:
        (Double: double);
      2:
        (        case integer of
          0:
            (VData: array[0..0] of byte);
          1:
            (VWideChar: PWideChar);
          2:
            (VAnsiChar: PAnsiChar)
        );
  end;

  PColumnValue = ^TColumnValue;

procedure TSqlDBOleDBStatement.LogStatusError(Status: integer;
  Column: PSqlDBColumnProperty);
var
  msg: RawUtf8;
begin
  {$ifndef PUREPASCAL}
  if cardinal(Status) <= cardinal(ord(high(TSqlDBOleDBStatus))) then
    msg := UnCamelCase(TrimLeftLowerCaseShort(
     GetEnumName(TypeInfo(TSqlDBOleDBStatus), Status)))
  else
  {$else}
    Int32ToUtf8(Status, msg);
  {$endif}
  SynDBLog.Add.Log(sllError, 'Invalid [%] status for column [%] at row % for %',
    [{%H-}msg, Column^.ColumnName, fCurrentRow, fSql], self);
end;

function TSqlDBOleDBStatement.GetCol(Col: integer;
  out Column: PSqlDBColumnProperty): pointer;
begin
  CheckCol(Col); // check Col value
  if not Assigned(fRowSet) or
     (fColumnCount = 0) then
    raise EOleDBException.CreateUtf8('%.Column*() with no prior Execute', [self]);
  if CurrentRow <= 0 then
    raise EOleDBException.CreateUtf8('%.Column*() with no prior Step', [self]);
  Column := @fColumns[Col];
  result := @fRowSetData[Column^.ColumnAttr];
  case TSqlDBOleDBStatus(PColumnValue(result)^.Status) of
    stOk:
      exit; // valid content
    stIsNull:
      result := nil;
    stTruncated:
      LogTruncatedColumn(Column^);
  else
    LogStatusError(PColumnValue(result)^.Status, Column);
  end;
end;

procedure TSqlDBOleDBStatement.GetCol64(Col: integer; DestType: TSqlDBFieldType;
  var Dest);
var
  C: PSqlDBColumnProperty;
  V: PColumnValue;
begin
  V := GetCol(Col, C);
  if V = nil then
    // column is NULL
    Int64(Dest) := 0
  else if C^.ColumnType = DestType then
    // types match -> fast direct retrieval
    Int64(Dest) := V^.Int64
  else
    // need conversion to destination type
    ColumnToTypedValue(Col, DestType, Dest);
end;

function TSqlDBOleDBStatement.ColumnBlob(Col: integer): RawByteString;
// ColumnBlob will return the binary content of the field
var
  C: PSqlDBColumnProperty;
  V: PColumnValue;
  P: PAnsiChar;
begin
  V := GetCol(Col, C);
  if V = nil then // column is NULL
    result := ''
  else
    case C^.ColumnType of
      ftBlob:
        begin
          if C^.ColumnValueInlined then
            P := @V^.VData
          else
            P := V^.VAnsiChar;
          SetString(result, P, V^.Length);
        end;
      ftUtf8:
        if V^.Length = 0 then
          result := ''
        else
        begin
          if C^.ColumnValueInlined then
            P := @V^.VData
          else
            P := V^.VAnsiChar;
          // +1 below for trailing WideChar(#0) in the resulting RawUnicode
          SetString(result, P, V^.Length + 1);
        end;
    else
      SetString(result, PAnsiChar(@V^.Int64), sizeof(Int64));
    end;
end;

function TSqlDBOleDBStatement.ColumnCurrency(Col: integer): currency;
begin
  GetCol64(Col, ftCurrency, result);
end;

function TSqlDBOleDBStatement.ColumnDateTime(Col: integer): TDateTime;
begin
  GetCol64(Col, ftDate, result);
end;

function TSqlDBOleDBStatement.ColumnDouble(Col: integer): double;
begin
  GetCol64(Col, ftDouble, result);
end;

function TSqlDBOleDBStatement.ColumnIndex(const aColumnName: RawUtf8): integer;
begin
  result := fColumn.FindHashed(aColumnName);
end;

function TSqlDBOleDBStatement.ColumnNull(Col: integer): boolean;
var
  C: PSqlDBColumnProperty;
begin
  result := GetCol(Col, C) = nil;
end;

function TSqlDBOleDBStatement.ColumnInt(Col: integer): Int64;
begin
  GetCol64(Col, ftInt64, result);
end;

function TSqlDBOleDBStatement.ColumnName(Col: integer): RawUtf8;
begin
  CheckCol(Col);
  result := fColumns[Col].ColumnName;
end;

function TSqlDBOleDBStatement.ColumnType(Col: integer;
  FieldSize: PInteger): TSqlDBFieldType;
begin
  CheckCol(Col);
  with fColumns[Col] do
  begin
    result := ColumnType;
    if FieldSize <> nil then
      if ColumnValueInlined then
        FieldSize^ := ColumnValueDBSize
      else
        FieldSize^ := 0;
  end;
end;

function TSqlDBOleDBStatement.ColumnUtf8(Col: integer): RawUtf8;
var
  C: PSqlDBColumnProperty;
  V: PColumnValue;
  P: pointer;
begin
  V := GetCol(Col, C);
  if V = nil then // column is NULL
    result := ''
  else
    case C^.ColumnType of // fast direct conversion from OleDB buffer
      ftInt64:
        Int64ToUtf8(V^.Int64, result);
      ftDate:
        result := DateTimeToIso8601Text(V^.Double);
      ftUtf8:
        begin
          if C^.ColumnValueInlined then
            P := @V^.VData
          else
            P := V^.VWideChar;
          RawUnicodeToUtf8(P, V^.Length shr 1, result);
        end;
      ftBlob:
        begin
          if C^.ColumnValueInlined then
            P := @V^.VData
          else
            P := V^.VAnsiChar;
          result := BinToBase64WithMagic(P, V^.Length);
        end;
      ftCurrency:
        Curr64ToStr(V^.Int64, result);
      ftDouble:
        if V^.Int64 = 0 then
          result := SmallUInt32Utf8[0]
        else
          DoubleToStr(V^.Double, result);
    end;
end;

function TSqlDBOleDBStatement.ColumnString(Col: integer): string;
var
  C: PSqlDBColumnProperty;
  V: PColumnValue;
  P: pointer;
begin
  V := GetCol(Col, C);
  if V = nil then // column is NULL
    result := ''
  else
    case C^.ColumnType of // fast direct conversion from OleDB buffer
      ftInt64:
        result := IntToString(V^.Int64);
      ftDouble:
        if V^.Int64 = 0 then
          result := '0'
        else
          result := DoubleToString(V^.Double);
      ftCurrency:
        result := Curr64ToString(V^.Int64);
      ftDate:
        result := Ansi7ToString(DateTimeToIso8601Text(V^.Double));
      ftUtf8:
        begin
          if C^.ColumnValueInlined then
            P := @V^.VData
          else
            P := V^.VWideChar;
          result := RawUnicodeToString(P, V^.Length shr 1);
        end;
      ftBlob:
        begin
          if C^.ColumnValueInlined then
            P := @V^.VData
          else
            P := V^.VAnsiChar;
          result := Ansi7ToString(BinToBase64WithMagic(P, V^.Length));
        end;
    end;
end;

function TSqlDBOleDBStatement.ColumnToVariant(Col: integer; var Value: Variant):
  TSqlDBFieldType;
var
  C: PSqlDBColumnProperty;
  V: PColumnValue;
  P: pointer;
begin
  // dedicated version to avoid as much memory allocation than possible
  V := GetCol(Col, C);
  if V = nil then
    result := ftNull
  else
    result := C^.ColumnType;
  VarClear(Value);
  with TVarData(Value) do
  begin
    VType := MAP_FIELDTYPE2VARTYPE[result];
    case result of
      ftInt64, ftDouble, ftCurrency, ftDate:
        VInt64 := V^.Int64; // copy 64 bit content
      ftUtf8:
        begin
          VAny := nil;
          if C^.ColumnValueInlined then
            P := @V^.VData
          else
            P := V^.VAnsiChar;
          {$ifndef UNICODE}
          if not Connection.Properties.VariantStringAsWideString then
          begin
            VType := varString;
            RawUnicodeToString(P, V^.Length shr 1, AnsiString(VAny));
          end
          else
          {$endif UNICODE}
            SetString(SynUnicode(VAny), PWideChar(P), V^.Length shr 1);
        end;
      ftBlob:
        if fForceBlobAsNull then
          VType := varNull
        else
        begin
          VAny := nil;
          if C^.ColumnValueInlined then
            P := @V^.VData
          else
            P := V^.VAnsiChar;
          SetString(RawByteString(VAny), PAnsiChar(P), V^.Length);
        end;
    end;
  end;
end;

procedure TSqlDBOleDBStatement.ColumnsToJson(WR: TJsonWriter);
var
  col: integer;
  V: PColumnValue;
  P: Pointer;
label
  Write;
begin
  // dedicated version to avoid as much memory allocation than possible
  if CurrentRow <= 0 then
    raise EOleDBException.CreateUtf8('%.ColumnsToJson() with no prior Step', [self]);
  if WR.Expand then
    WR.Add('{');
  for col := 0 to fColumnCount - 1 do // fast direct conversion from OleDB buffer
    with fColumns[col] do
    begin
      if WR.Expand then
        WR.AddFieldName(ColumnName); // add '"ColumnName":'
      V := @fRowSetData[ColumnAttr];
      case TSqlDBOleDBStatus(V^.Status) of
        stOK:
Write:    case ColumnType of
            ftInt64:
              WR.Add(V^.Int64);
            ftDouble:
              WR.AddDouble(V^.Double);
            ftCurrency:
              WR.AddCurr64(@V^.Int64);
            ftDate:
              begin
                WR.Add('"');
                WR.AddDateTime(@V^.Double, 'T', #0, fForceDateWithMS);
                WR.Add('"');
              end;
            ftUtf8:
              begin
                WR.Add('"');
                if ColumnValueInlined then
                  P := @V^.VData
                else
                  P := V^.VWideChar;
                WR.AddJsonEscapeW(P, V^.Length shr 1);
                WR.Add('"');
              end;
            ftBlob:
              if fForceBlobAsNull then
                WR.AddNull
              else
              begin
                if ColumnValueInlined then
                  P := @V^.VData
                else
                  P := V^.VAnsiChar;
                WR.WrBase64(P, V^.Length, true); // withMagic=true
              end;
          else
            WR.AddNull;
          end;
        stIsNull:
          WR.AddNull;
        stTruncated:
          begin
            LogTruncatedColumn(fColumns[col]);
            goto Write;
          end;
      else
        begin
          WR.AddNull;
          LogStatusError(V^.Status, @fColumns[col]);
        end;
      end;
      WR.AddComma;
    end;
  WR.CancelLastComma; // cancel last ','
  if WR.Expand then
    WR.Add('}');
end;

function TSqlDBOleDBStatement.ParamToVariant(Param: integer; var Value: Variant;
  CheckIsOutParameter: boolean): TSqlDBFieldType;
begin
  inherited ParamToVariant(Param, Value); // raise exception if Param incorrect
  dec(Param); // start at #1
  if CheckIsOutParameter and
     (fParams[Param].VInOut = paramIn) then
    raise EOleDBException.CreateUtf8('%.ParamToVariant expects an [In]Out parameter',
      [self]);
  // OleDB provider should have already modified the parameter in-place, i.e.
  // in our fParams[] buffer, especialy for TEXT parameters (OleStr/WideString)
  // -> we have nothing to do but return the current value :)
  with fParams[Param] do
  begin
    result := VType;
    case VType of
      ftInt64:
        Value := VInt64;
      ftDouble:
        Value := unaligned(PDouble(@VInt64)^);
      ftCurrency:
        Value := PCurrency(@VInt64)^;
      ftDate:
        Value := PDateTime(@VInt64)^;
      ftUtf8:
        Value := VText; // returned as WideString/OleStr variant
      ftBlob:
        RawByteStringToVariant(VBlob, Value);
    else
      SetVariantNull(Value);
    end;
  end;
end;

const
  PARAMTYPE2OLEDB: array[TSqlDBParamInOutType] of DBPARAMIO = (
    DBPARAMIO_INPUT, DBPARAMIO_OUTPUT, DBPARAMIO_INPUT or DBPARAMIO_OUTPUT);

  FIELDTYPE2OLEDB: array[TSqlDBFieldType] of DBTYPE = (
    DBTYPE_EMPTY, DBTYPE_I4, DBTYPE_I8, DBTYPE_R8, DBTYPE_CY, DBTYPE_DATE,
    DBTYPE_WSTR or DBTYPE_BYREF, DBTYPE_BYTES or DBTYPE_BYREF);

  FIELDTYPE2OLEDBTYPE_NAME: array[TSqlDBFieldType] of WideString = (
     '', 'DBTYPE_I4', 'DBTYPE_I8', 'DBTYPE_R8', 'DBTYPE_CY', 'DBTYPE_DATE',
    'DBTYPE_WVARCHAR', 'DBTYPE_BINARY');
// ftUnknown, ftNull, ftInt64, ftDouble, ftCurrency, ftDate, ftUtf8, ftBlob

  TABLE_PARAM_DATASOURCE: WideString = 'table';

procedure TSqlDBOleDBStatement.Prepare(const aSql: RawUtf8; ExpectResults: boolean);
var
  L: integer;
  SQLW: RawUnicode;
begin
  SqlLogBegin(sllDB);
  if Assigned(fCommand) or
     Assigned(fRowSet) or
     (fColumnCount > 0) or
     (fColumnBindings <> nil) or
     (fParamBindings <> nil) then
    raise EOleDBException.CreateUtf8('%.Prepare should be called once', [self]);
  inherited;
  with OleDBConnection do
  begin
    if not IsConnected then
      Connect;
    OleDBCheck(self, (fSession as IDBCreateCommand).CreateCommand(nil,
      IID_ICommandText, ICommand(fCommand)));
  end;
  L := Length(fSql);
  if StripSemicolon then
    while (L > 0) and
          (fSql[L] in [#1..' ', ';']) do
      dec(L); // trim ' ' or ';' right (last ';' could be found incorrect)
  SetLength(SQLW, L * 2 + 1);
  Utf8ToWideChar(pointer(SQLW), pointer(fSql), L);
  fCommand.SetCommandText(DBGUID_DEFAULT, pointer(SQLW));
  SqlLogEnd;
end;

procedure TSqlDBOleDBStatement.ExecutePrepared;
var
  i: integer;
  P: POleDBStatementParam;
  B: PDBBinding;
  ParamsStatus: TCardinalDynArray;
  RowSet: IRowSet;
  mr: IMultipleResults;
  res: HResult;
  fParamBindInfo: TDBParamBindInfoDynArray;
  BI: PDBParamBindInfo;
  fParamOrdinals: TPtrUIntDynArray;
  PO: PPtrUInt;
  dbObjTVP: TDBObject;
  ssPropParamIDList: TDBPROP;
  ssPropsetParamIDList: TDBPROPSET;
  ssPropParamStrList: TDBPROP;
  ssPropsetParamStrList: TDBPROPSET;
  ssParamProps: TSSPARAMPROPSDynArray;
  ssParamPropsCount: integer;
  IDLists: array of TIDListRowset;
begin
  SqlLogBegin(sllSQL);
  // 1. check execution context
  if not Assigned(fCommand) then
    raise EOleDBException.CreateUtf8('%s.Prepare should have been called', [self]);
  if Assigned(fRowSet) or
     (fColumnCount > 0) or
     (fColumnBindings <> nil) or
     (fParamBindings <> nil) then
    raise EOleDBException.CreateUtf8('Missing call to %.Reset', [self]);
  inherited ExecutePrepared; // set fConnection.fLastAccessTicks
  // 2. bind parameters
  SetLength(IDLists, fParamCount);
  try
    if fParamCount = 0 then
      // no parameter to bind
      fDBParams.cParamSets := 0
    else
    begin
      // bind supplied parameters, with direct mapping to fParams[]
      for i := 0 to fParamCount - 1 do
        case fParams[i].VType of
          ftUnknown:
            raise EOleDBException.CreateUtf8('%.Execute: missing #% bound parameter for [%]',
              [self, i + 1, fSql]);
        end;
      P := pointer(fParams);
      SetLength(fParamBindings, fParamCount);
      B := pointer(fParamBindings);
      SetLength(fParamBindInfo, fParamCount);
      BI := pointer(fParamBindInfo);
      SetLength(fParamOrdinals, fParamCount);
      PO := pointer(fParamOrdinals);
      dbObjTVP.dwFlags := STGM_READ;
      dbObjTVP.iid := IID_IRowset;
      FillcharFast(ssPropParamIDList, SizeOf(ssPropParamIDList), 0);
      ssPropParamIDList.dwPropertyID := SSPROP_PARAM_TYPE_TYPENAME;
      ssPropParamIDList.vValue := IDList_TYPE;
      ssPropsetParamIDList.cProperties := 1;
      ssPropsetParamIDList.guidPropertySet := DBPROPSET_SQLSERVERPARAMETER;
      ssPropsetParamIDList.rgProperties := @ssPropParamIDList;
      FillcharFast(ssPropParamStrList, SizeOf(ssPropParamStrList), 0);
      ssPropParamStrList.dwPropertyID := SSPROP_PARAM_TYPE_TYPENAME;
      ssPropParamStrList.vValue := StrList_TYPE;
      ssPropsetParamStrList.cProperties := 1;
      ssPropsetParamStrList.guidPropertySet := DBPROPSET_SQLSERVERPARAMETER;
      ssPropsetParamStrList.rgProperties := @ssPropParamStrList;
      SetLength(ssParamProps, fParamCount);
      ssParamPropsCount := 0;
      for i := 1 to fParamCount do
      begin
        B^.iOrdinal := i; // parameter index (starting at 1)
        B^.eParamIO := PARAMTYPE2OLEDB[P^.VInOut]; // parameter direction
        B^.wType := FIELDTYPE2OLEDB[P^.VType];     // parameter data type
        B^.dwPart := DBPART_VALUE or DBPART_STATUS;
        B^.obValue := PAnsiChar(@P^.VInt64) - pointer(fParams);
        B^.obStatus := PAnsiChar(@P^.VStatus) - pointer(fParams);
        BI^.dwFlags := PARAMTYPE2OLEDB[P^.VInOut]; // parameter direction
        BI^.pwszName := nil; //unnamed parameters
        BI^.pwszDataSourceType := Pointer(FIELDTYPE2OLEDBTYPE_NAME[P^.VType]);
        BI^.ulParamSize := 0;
        PO^ := i;
        // check array binding
        if P.VArray <> nil then
        begin
          BI^.pwszDataSourceType := Pointer(TABLE_PARAM_DATASOURCE);
          B^.wType := DBTYPE_TABLE;
          B^.cbMaxLen := sizeof(IUnknown);
          B^.pObject := @dbObjTVP;
          B^.obValue := PAnsiChar(@P^.VIUnknown) - pointer(fParams);
          case P^.VType of
            ftInt64:
              ssParamProps[ssParamPropsCount].rgPropertySets := @ssPropsetParamIDList;
            ftUtf8:
              ssParamProps[ssParamPropsCount].rgPropertySets := @ssPropsetParamStrList;
          else
            raise EOleDBException.Create('Unsupported array parameter type');
          end;
          ssParamProps[ssParamPropsCount].cPropertySets := 1;
          ssParamProps[ssParamPropsCount].iOrdinal := i;
          inc(ssParamPropsCount);
          IDLists[i - 1] := TIDListRowset.Create(P.VArray, P^.VType);
          IDLists[i - 1].Initialize(OleDBConnection.fSession as IOpenRowset);
          P^.VIUnknown := IDLists[i - 1];
        end
        else
        begin
          P^.VIUnknown := nil;
          case P^.VType of
            ftNull:
              begin
                P^.VStatus := ord(stIsNull);
                BI.pwszDataSourceType := 'DBTYPE_WVARCHAR';
                BI.dwFlags := BI^.dwFlags or DBPARAMFLAGS_ISNULLABLE;
              end;
            ftInt64, ftDouble, ftCurrency, ftDate:            // those types match the VInt64 binary representation :)
              B^.cbMaxLen := sizeof(Int64);
            ftBlob:
              begin
                // sent as DBTYPE_BYREF mapping directly RawByteString VBlob content
                B^.dwPart := DBPART_VALUE or DBPART_LENGTH or DBPART_STATUS;
                B^.obValue := PAnsiChar(@P^.VBlob) - pointer(fParams);
                B^.cbMaxLen := length(P^.VBlob);
                P^.VInt64 := length(P^.VBlob); // store length in unused VInt64 property
                B^.obLength := PAnsiChar(@P^.VInt64) - pointer(fParams);
              end;
            ftUtf8:
              begin
                B^.obValue := PAnsiChar(@P^.VText) - pointer(fParams);
                if P^.VText = '' then
                begin
                  B^.wType := DBTYPE_WSTR; // '' -> bind one #0 wide char
                  B^.cbMaxLen := sizeof(WideChar);
                end
                else
                begin
                  // mapping directly the WideString VText content
                  B^.wType := DBTYPE_BSTR; // DBTYPE_WSTR just doesn't work :(
                  B^.cbMaxLen := sizeof(Pointer);
                  BI^.ulParamSize := length(P^.VText);
                end;
              end;
          end;
          if BI^.ulParamSize = 0 then
            BI^.ulParamSize := B^.cbMaxLen;
        end;
        inc(P);
        inc(B);
        inc(BI);
        inc(PO);
      end;
      if not OleDBConnection.OleDBProperties.fSupportsOnlyIRowset then
      begin
        OleDBConnection.OleDBCheck(self,
         (fCommand as ICommandWithParameters).SetParameterInfo(
          fParamCount, pointer(fParamOrdinals), pointer(fParamBindInfo)));
        if ssParamPropsCount > 0 then
          OleDBConnection.OleDBCheck(self,
            (fCommand as ISSCommandWithParameters).SetParameterProperties(
              ssParamPropsCount, pointer(ssParamProps)));
      end;
      SetLength(ParamsStatus, fParamCount);
      OleDBConnection.OleDBCheck(self,
        (fCommand as IAccessor).CreateAccessor(
         DBACCESSOR_PARAMETERDATA, fParamCount, Pointer(fParamBindings), 0,
         fDBParams.HACCESSOR, pointer(ParamsStatus)), ParamsStatus);
      fDBParams.cParamSets := 1;
      fDBParams.pData := pointer(fParams);
    end;
    // 3. Execute SQL
    if fExpectResults then
    try
      // 3.1 SELECT will allow access to resulting rows data from fRowSet
      res := E_UNEXPECTED; // makes compiler happy
      if not OleDBConnection.OleDBProperties.fSupportsOnlyIRowset then
      begin
        // use IMultipleResults for 'insert into table1 values (...); select ... from table2 where ...'
        res := fCommand.Execute(nil, IID_IMultipleResults, fDBParams, @fUpdateCount, @mr);
        if res = E_NOINTERFACE then
          OleDBConnection.OleDBProperties.fSupportsOnlyIRowset := true
        else if Assigned(mr) then
          repeat
            res := mr.GetResult(nil, 0, IID_IRowset, @fUpdateCount, @RowSet);
          until Assigned(RowSet) or
                (res <> S_OK);
      end;
      if OleDBConnection.OleDBProperties.fSupportsOnlyIRowset then
        res := fCommand.Execute(nil, IID_IRowset, fDBParams, nil, @RowSet);
      OleDBConnection.OleDBCheck(self, res, ParamsStatus);
      FromRowSet(RowSet);
    except
      on E: Exception do
      begin
        CloseRowSet; // force fRowSet=nil
        raise;
      end;
    end
    else      // 3.2 ExpectResults=false (e.g. SQL UPDATE) -> leave fRowSet=nil
      OleDBConnection.OleDBCheck(self, fCommand.Execute(nil, DB_NULLGUID,
        fDBParams, @fUpdateCount, nil));
  finally
    for i := 0 to fParamCount - 1 do
      if Assigned(IDLists[i]) then
      begin
        fParams[i].VIUnknown := nil;
        IDLists[i].Free;
      end;
  end;
  SqlLogEnd;
end;

procedure TSqlDBOleDBStatement.FromRowSet(RowSet: IRowSet);
begin
  if fRowSet <> nil then
    EOleDBException.CreateUtf8('%.FromRowSet twice', [self]);
  if not Assigned(RowSet) then
    exit; // no row returned
  fRowSet := RowSet;
  fRowSize := BindColumns(fRowSet as IColumnsInfo, fColumn, fColumnBindings);
  SetLength(fRowSetData, fRowSize);
  if fRowSize > RowBufferSize then
    RowBufferSize := fRowSize; // enforce at least one row in OleDB buffer
  SetLength(fRowStepHandles, RowBufferSize div fRowSize);
end;

procedure TSqlDBOleDBStatement.FlushRowSetData;
var
  c: PtrInt;
begin
  if fHasColumnValueInlined then
    for c := 0 to fColumnCount - 1 do
      with fColumns[c] do
        if not ColumnValueInlined then // release DBTYPE_BYREF memory
          with PColumnValue(@fRowSetData[ColumnAttr])^ do
            if VWideChar <> nil then
              OleDBConnection.fMalloc.Free(VWideChar);
  FillcharFast(fRowSetData[0], fRowSize, 0);
end;

function TSqlDBOleDBStatement.Step(SeekFirst: boolean): boolean;
var
  Status: TCardinalDynArray;
  sav: integer;
begin
{  if not Assigned(fCommand) then
    raise EOleDBException.CreateUtf8('%.Execute should be called before Step',[self]); }
  result := false;
  sav := fCurrentRow;
  fCurrentRow := 0;
  if not Assigned(fRowSet) or
     (fColumnCount = 0) then
    exit; // no row available at all (e.g. for SQL UPDATE) -> return false
  if fRowSetAccessor = 0 then
  begin
    // first time called -> need to init accessor from fColumnBindings[]
    SetLength(Status, fColumnCount);
    OleDBConnection.OleDBCheck(self,
      (fRowSet as IAccessor).CreateAccessor(DBACCESSOR_ROWDATA or DBACCESSOR_OPTIMIZED,
       fColumnCount, pointer(fColumnBindings), fRowSize, fRowSetAccessor, pointer(Status)), Status);
    fRowStepHandleRetrieved := 0;
    fRowStepHandleCurrent := 0;
    fRowStepResult := 0;
  end
  else if SeekFirst then
  begin
    // rewind to first row
    ReleaseRowSetDataAndRows;
    OleDBConnection.OleDBCheck(self, fRowSet.RestartPosition(DB_NULL_HCHAPTER));
    fRowStepResult := 0;
  end
  else
    FlushRowSetData;
  if fRowStepHandleCurrent >= fRowStepHandleRetrieved then
  begin
    ReleaseRowSetDataAndRows;
    if fRowStepResult = DB_S_ENDOFROWSET then
      exit; // no more row available -> return false
    fRowStepResult := fRowSet.GetNextRows(DB_NULL_HCHAPTER, 0, length(fRowStepHandles),
      fRowStepHandleRetrieved, pointer(fRowStepHandles));
    OleDBConnection.OleDBCheck(self, fRowStepResult);
    fRowStepHandleCurrent := 0;
    if fRowStepHandleRetrieved = 0 then
      exit; // no more row available
  end;
  // here we have always fRowStepHandleCurrent<fRowStepHandleRetrieved
  OleDBConnection.OleDBCheck(self,
    fRowSet.GetData(fRowStepHandles[fRowStepHandleCurrent], fRowSetAccessor, pointer(fRowSetData)));
  inc(fRowStepHandleCurrent);
  fCurrentRow := sav + 1;
  inc(fTotalRowsRetrieved);
  result := true; // mark data available in fRowSetData
end;

destructor TSqlDBOleDBStatement.Destroy;
begin
  try
    CloseRowSet;
  finally
    fCommand := nil;
    inherited;
  end;
end;

procedure TSqlDBOleDBStatement.SetRowBufferSize(Value: integer);
begin
  if Value < 4096 then
    Value := 4096;
  fRowBufferSize := Value;
end;

procedure TSqlDBOleDBStatement.ReleaseRowSetDataAndRows;
begin
  FlushRowSetData;
  if fRowStepHandleRetrieved <> 0 then
  begin
    fRowSet.ReleaseRows(fRowStepHandleRetrieved, Pointer(fRowStepHandles), nil, nil, nil);
    fRowStepHandleRetrieved := 0;
  end;
  fCurrentRow := 0;
end;

procedure TSqlDBOleDBStatement.CloseRowSet;
begin
  if not Assigned(fRowSet) then
    exit;
  ReleaseRowSetDataAndRows;
  if fRowSetAccessor <> 0 then
  begin
    (fRowSet as IAccessor).ReleaseAccessor(fRowSetAccessor, nil);
    fRowSetAccessor := 0;
  end;
  fRowSet := nil;
end;

procedure TSqlDBOleDBStatement.Reset;
begin
  ReleaseRows;
  if fColumnCount > 0 then
  begin
    fColumn.Clear;
    fColumn.ReHash;
    // faster if full command is re-prepared!
    fCommand := nil;
    Prepare(fSql, fExpectResults);
  end;
  fUpdateCount := 0;
  inherited Reset;
end;

procedure TSqlDBOleDBStatement.ReleaseRows;
begin
  if fParamCount > 0 then
    fParam.Clear;
  fParamBindings := nil;
  CloseRowSet;
  fColumnBindings := nil;
  inherited ReleaseRows;
end;

function TSqlDBOleDBStatement.UpdateCount: integer;
begin
  if not fExpectResults then
    result := fUpdateCount
  else
    result := 0;
end;

function TSqlDBOleDBStatement.BindColumns(ColumnInfo: IColumnsInfo;
  var Column: TDynArrayHashed; out Bindings: TDBBindingDynArray): integer;
const
  // column content is inlined up to 4 KB, otherwise will be stored as DBTYPE_BYREF
  MAXCOLUMNSIZE = 4000;
var
  i, len: integer;
  B: PDBBinding;
  Cols, nfo: PDBColumnInfo;
  Col: PSqlDBColumnProperty;
  nCols: PtrUInt;
  ColsNames: PWideChar;
  aName: RawUtf8;
begin
  nCols := 0;
  Cols := nil;
  ColsNames := nil;
  OleDBConnection.OleDBCheck(self, ColumnInfo.GetColumnInfo(nCols, Cols, ColsNames));
  try
    nfo := Cols;
    SetLength(fColumnBindings, nCols);
    B := pointer(fColumnBindings);
    result := 0; // resulting buffer will map TColumnValue layout
    fColumn.Capacity := nCols;
    for i := 1 to nCols do
    begin
      if (nfo^.pwszName = nil) or
         (nfo^.pwszName^ = #0) then
        aName := 'col_' + Int32ToUtf8(i)
      else
        aName := RawUnicodeToUtf8(nfo^.pwszName, StrLenW(nfo^.pwszName));
      Col := fColumn.AddAndMakeUniqueName(aName); // set ColumnName := aName
      Col^.ColumnType := OleDBColumnToFieldType(nfo^.wType, nfo^.bScale);
      Col^.ColumnNonNullable := nfo^.dwFlags and DBCOLUMNFLAGS_MAYBENULL = 0;
      Col^.ColumnAttr := result; // offset of status[-length]-value in fRowSetData[]
      Col^.ColumnValueInlined := true;
      B^.iOrdinal := nfo^.iOrdinal;
      B^.eParamIO := DBPARAMIO_NOTPARAM;
      B^.obStatus := result;
      inc(result, sizeof(PtrInt)); // TColumnValue.Status
      B^.wType := FIELDTYPE2OLEDB[Col^.ColumnType];
      case Col^.ColumnType of
        ftInt64, ftDouble, ftCurrency, ftDate:
          begin
            inc(result, sizeof(PtrUInt)); // ignore TColumnValue.Length
            B^.dwPart := DBPART_STATUS or DBPART_VALUE;
            B^.obValue := result;
            B^.cbMaxLen := sizeof(Int64);
            inc(result, sizeof(Int64));
          end;
        ftUtf8, ftBlob:
          begin
            B^.dwPart := DBPART_STATUS or DBPART_VALUE or DBPART_LENGTH;
            B^.obLength := result; // need length field in fRowSetData[]
            inc(result, sizeof(PtrUInt)); // TColumnValue.Length
            B^.obValue := result;
            if nfo^.ulColumnSize < MAXCOLUMNSIZE then
            begin
              // inline up to 4 KB
              B^.wType := B^.wType and not DBTYPE_BYREF;
              len := nfo^.ulColumnSize;
              Col^.ColumnValueDBSize := len;
              if Col^.ColumnType = ftUtf8 then
              begin
                case nfo^.wType of
                  DBTYPE_STR, DBTYPE_BSTR, DBTYPE_WSTR:
                    len := len * 2; // ulColumnSize = WideChar count
                  DBTYPE_GUID:
                    len := 78;
                else
                  len := 62; // 31 widechars will fit any type converted
                end;
                inc(len, 2); // reserve memory for trailing WideChar(#0)
              end;
              if AlignDataInternalBuffer then // 8 bytes alignment
                len := ((len - 1) shr 3 + 1) shl 3;
              inc(result, len);
              B^.cbMaxLen := len;
            end
            else
            begin
              // get huge content by pointer (includes DBTYPE_BYREF)
              fHasColumnValueInlined := true;
              Col^.ColumnValueInlined := false;
              B^.cbMaxLen := sizeof(Pointer); // value=pointer in fRowSetData[]
              if AlignDataInternalBuffer then
                inc(result, 8)
              else
                inc(result, sizeof(Pointer));
            end;
          end;
      else
        raise EOleDBException.CreateUtf8(
          '%.Execute: wrong column [%] (%) for [%]', [self, aName,
           GetEnumName(TypeInfo(TSqlDBFieldType), ord(Col^.ColumnType))^, fSql]);
      end;
      inc(nfo);
      inc(B);
      if AlignDataInternalBuffer then
        Assert((result and 7) = 0);
    end;
    assert(not AlignDataInternalBuffer or (result and 7 = 0));
    assert(fColumnCount = integer(nCols));
  finally
    OleDBConnection.fMalloc.Free(Cols);
    OleDBConnection.fMalloc.Free(ColsNames);
  end;
end;


{ TSqlDBOleDBConnection }

procedure TSqlDBOleDBConnection.Connect;
var
  DataInitialize: IDataInitialize;
  unknown: IUnknown;
  log: ISynLog;
begin
  Log := SynDBLog.Enter(self, 'Connect');
  // check context
  if Connected then
    Disconnect;
  if OleDBProperties.ConnectionString = '' then
    raise EOleDBException.CreateUtf8('%.Connect excepts a ConnectionString', [self]);
  try
    // retrieve initialization parameters from connection string
    OleCheck(CoCreateInstance(CLSID_MSDAINITIALIZE, nil, CLSCTX_INPROC_SERVER,
      IID_IDataInitialize, DataInitialize));
    OleCheck(DataInitialize.GetDataSource(nil, CLSCTX_INPROC_SERVER, pointer(OleDBProperties.ConnectionString),
      IID_IDBInitialize, IUnknown(fDBInitialize)));
    DataInitialize := nil;
    // open the connection to the DB
    OleDBCheck(nil, fDBInitialize.Initialize);
    OnDBInitialized; // optionaly set parameters
    OleDBCheck(nil, (fDBInitialize as IDBCreateSession).CreateSession(nil,
      IID_IOpenRowset, fSession));
    // check if DB handle transactions
    if fSession.QueryInterface(IID_ITransactionLocal, unknown) = S_OK then
      fTransaction := unknown as ITransactionLocal
    else
      fTransaction := nil;
    inherited Connect; // notify any re-connection
  except
    on E: Exception do
    begin
      fSession := nil; // mark not connected
      fDBInitialize := nil;
      DataInitialize := nil;
      raise;
    end;
  end;
end;

constructor TSqlDBOleDBConnection.Create(aProperties: TSqlDBConnectionProperties);
var
  log: ISynLog;
begin
  Log := SynDBLog.Enter(self, 'Create');
  if not aProperties.InheritsFrom(TSqlDBOleDBConnectionProperties) then
    raise EOleDBException.CreateUtf8('Invalid %.Create(%)', [self, aProperties]);
  fOleDBProperties := TSqlDBOleDBConnectionProperties(aProperties);
  inherited;
  CoInit;
  OleCheck(CoGetMalloc(1, fMalloc));
end;

destructor TSqlDBOleDBConnection.Destroy;
var
  log: ISynLog;
begin
  Log := SynDBLog.Enter(self, 'Destroy');
  try
    inherited Destroy; // call Disconnect;
    fMalloc := nil;
    CoUninit;
  except
    on E: Exception do
      if Log <> nil then
        Log.Log(sllError, E);
  end;
end;

procedure TSqlDBOleDBConnection.Disconnect;
var
  log: ISynLog;
begin
  Log := SynDBLog.Enter(self, 'Disconnect');
  try
    inherited Disconnect; // flush any cached statement
  finally
    if Connected then
    begin
      fTransaction := nil;
      fSession := nil;
      OleDBCheck(nil, fDBInitialize.Uninitialize);
      fDBInitialize := nil;
    end;
  end;
end;

function TSqlDBOleDBConnection.IsConnected: boolean;
begin
  result := fSession <> nil;
end;

function TSqlDBOleDBConnection.NewStatement: TSqlDBStatement;
begin
  result := TSqlDBOleDBStatement.Create(self);
end;

procedure TSqlDBOleDBConnection.OleDBCheck(aStmt: TSqlDBStatement; aResult:
  HRESULT; const aStatus: TCardinalDynArray);

  procedure EnhancedTest;
  var
    ErrorInfo, ErrorInfoDetails: IErrorInfo;
    ErrorRecords: IErrorRecords;
    i: integer;
    Desc: WideString;
    ErrorCount: cardinal;
    E: Exception;
    s: string;
  begin
    // get OleDB specific error information
    GetErrorInfo(0, ErrorInfo);
    if Assigned(ErrorInfo) then
    begin
      ErrorRecords := ErrorInfo as IErrorRecords;
      ErrorRecords.GetRecordCount(ErrorCount);
      for i := 0 to ErrorCount - 1 do
        if not Assigned(OleDBProperties.OnCustomError) or
           not OleDBProperties.OnCustomError(self, ErrorRecords, i) then
        begin
          // retrieve generic error info if OnCustomError() didn't handle it
          OleCheck(ErrorRecords.GetErrorInfo(i, GetSystemDefaultLCID, ErrorInfoDetails));
          OleCheck(ErrorInfoDetails.GetDescription(Desc));
          if fOleDBErrorMessage <> '' then
            fOleDBErrorMessage := fOleDBErrorMessage + '  ';
          fOleDBErrorMessage := fOleDBErrorMessage + UnicodeBufferToString(pointer(Desc));
          ErrorInfoDetails := nil;
        end;
    end;
    // get generic HRESULT error
    if not Succeeded(aResult) or
           (fOleDBErrorMessage <> '') then
    begin
      s := SysErrorMessage(aResult);
      if s = '' then
        s := 'OLEDB Error ' + IntToHex(aResult, 8);
      if s <> fOleDBErrorMessage then
        fOleDBErrorMessage := s + ' - ' + fOleDBErrorMessage;
    end;
    if fOleDBErrorMessage = '' then
      exit;
    // retrieve binding information from Status[]
    s := '';
    for i := 0 to high(aStatus) do
      if TSqlDBOleDBBindStatus(aStatus[i]) <> bsOK then
      begin
        if aStatus[i] <= cardinal(high(TSqlDBOleDBBindStatus)) then
          s := FormatString('% Status[%]="%"', [s, i,
            GetCaptionFromEnum(TypeInfo(TSqlDBOleDBBindStatus), aStatus[i])])
        else
          s := FormatString('% Status[%]=%', [s, i, aStatus[i]]);

      end;
    if s <> '' then
      fOleDBErrorMessage := fOleDBErrorMessage + s;
    StringToUtf8(fOleDBErrorMessage, fErrorMessage);
    // raise exception
    if aStmt = nil then
      E := EOleDBException.Create(fOleDBErrorMessage)
    else
      E := EOleDBException.CreateUtf8('%: %', [self, StringToUtf8(fOleDBErrorMessage)]);
    SynDBLog.Add.Log(sllError, E);
    raise E;
  end;

begin
  fOleDBErrorMessage := '';
  fOleDBInfoMessage := '';
  if not Succeeded(aResult) or Assigned(OleDBProperties.OnCustomError) then
    EnhancedTest;
end;

procedure TSqlDBOleDBConnection.OnDBInitialized;
begin
  // do nothing by default
end;

procedure TSqlDBOleDBConnection.Commit;
var
  log: ISynLog;
begin
  Log := SynDBLog.Enter(self, 'Commit');
  if assigned(fTransaction) then
  begin
    inherited Commit;
    try
      OleDbCheck(nil, fTransaction.Commit(False, XACTTC_SYNC, 0));
    except
      inc(fTransactionCount); // the transaction is still active
      raise;
    end;
  end;
end;

procedure TSqlDBOleDBConnection.Rollback;
var
  log: ISynLog;
begin
  Log := SynDBLog.Enter(self, 'Rollback');
  if assigned(fTransaction) then
  begin
    inherited Rollback;
    OleDbCheck(nil, fTransaction.Abort(nil, False, False));
  end;
end;

procedure TSqlDBOleDBConnection.StartTransaction;
var
  log: ISynLog;
begin
  Log := SynDBLog.Enter(self, 'StartTransaction');
  if assigned(fTransaction) then
  begin
    inherited StartTransaction;
    OleDbCheck(nil,
      fTransaction.StartTransaction(ISOLATIONLEVEL_READCOMMITTED, 0, nil, nil));
  end;
end;


{ TSqlDBOleDBConnectionProperties }

function TSqlDBOleDBConnectionProperties.ConnectionStringDialogExecute(
  Parent: HWND): boolean;
var
  DataInitialize: IDataInitialize;
  DBPromptInitialize: IDBPromptInitialize;
  DBInitialize: IUnknown;
  res: HRESULT;
  tmp: PWideChar;
  log: ISynLog;
begin
  Log := SynDBLog.Enter(self, 'ConnectionStringDialog');
  result := false;
  if self <> nil then
  try
    CoInit; // if not already done
    try
      OleCheck(CoCreateInstance(CLSID_DATALINKS, nil, CLSCTX_INPROC_SERVER,
        IID_IDBPromptInitialize, DBPromptInitialize));
      OleCheck(CoCreateInstance(CLSID_MSDAINITIALIZE, nil, CLSCTX_INPROC_SERVER,
        IID_IDataInitialize, DataInitialize));
      if fConnectionString <> '' then
        DataInitialize.GetDataSource(nil, CLSCTX_INPROC_SERVER, Pointer(fConnectionString),
          IID_IDBInitialize, DBInitialize)
      else
        DBInitialize := nil;
      res := DBPromptInitialize.PromptDataSource(nil, Parent,
        DBPROMPTOPTIONS_PROPERTYSHEET, 0, nil, nil, IID_IDBInitialize, DBInitialize);
      case res of
        S_OK:
          begin
            OleCheck(DataInitialize.GetInitializationString(DBInitialize, True, tmp));
            fConnectionString := tmp;
            if tmp <> nil then
              CoTaskMemFree(tmp);
            if Log <> nil then
              Log.Log(sllDB, 'New connection settings set', self);
            result := true;
          end;
        DB_E_CANCELED:
          if Log <> nil then
            Log.Log(sllDB, 'Canceled', self);
      else
        OleCheck(res);
      end;
    finally
      CoUninit;
    end;
  except
    result := false;
  end;
end;

function TSqlDBOleDBConnectionProperties.CreateDatabase: boolean;
var
  Catalog: _Catalog;
  DB: OleVariant;
begin
  result := false;
  if ConnectionString <> '' then
  try
    CoInit;
    if Succeeded(CoCreateInstance(CLASS_Catalog, nil, CLSCTX_INPROC_SERVER,
      IID__Catalog, Catalog)) then
    try
      DB := Catalog.Create(ConnectionString);
      result := true;
    except
      result := false;
    end;
    SynDBLog.Add.Log(sllDB, 'CreateDatabase for [%] returned %',
      [ConnectionString, ord(result)], self);
  finally
    DB := null;
    Catalog := nil;
    CoUninit;
  end;
end;

procedure TSqlDBOleDBConnectionProperties.GetTableNames(out Tables: TRawUtf8DynArray);
var
  Rows: IRowset;
  count, schemaCol, nameCol: integer;
  schema, tablename: RawUtf8;
begin
  inherited; // first try from SQL, if any (faster)
  if Tables <> nil then
    exit; // already retrieved directly from engine
  try
    // see http://msdn.microsoft.com/en-us/library/ms716980(v=VS.85).aspx
    // Restriction columns: TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME, TABLE_TYPE
    if GetSchema(DBSCHEMA_TABLES, ['', '', '', 'TABLE'], Rows) then
      with TSqlDBOleDBStatement.Create(MainConnection) do
      try
        FromRowSet(Rows);
        count := 0;
        schemaCol := ColumnIndex('TABLE_SCHEMA');
        nameCol := ColumnIndex('TABLE_NAME');
        if (schemaCol >= 0) and
           (nameCol >= 0) then
          while Step do
          begin
            schema := TrimU(ColumnUtf8(schemaCol));
            tablename := TrimU(ColumnUtf8(nameCol));
            if schema <> '' then
              tablename := schema + '.' + tablename;
            AddSortedRawUtf8(Tables, count, tablename);
          end;
        SetLength(Tables, count);
      finally
        Free;
      end;
  except
    on Exception do
      SetLength(Tables, 0);
  end;
end;

procedure TSqlDBOleDBConnectionProperties.GetFields(const aTableName: RawUtf8;
  out Fields: TSqlDBColumnDefineDynArray);
var
  Owner, Table, Column: RawUtf8;
  Rows: IRowset;
  n, i: integer;
  F: TSqlDBColumnDefine;
  FA: TDynArray;
const
  DBTYPE_DISPLAY: array[TSqlDBFieldType] of RawUtf8 = ('???', 'null', 'int',
    'double', 'currency', 'date', 'nvarchar', 'blob');
begin
  inherited; // first try from SQL, if any (faster)
  if Fields <> nil then
    exit; // already retrieved directly from engine
  try
    Split(aTableName, '.', Owner, Table);
    if Table = '' then
    begin
      Table := Owner;
      Owner := '';
    end;
    // see http://msdn.microsoft.com/en-us/library/ms723052(v=VS.85).aspx
    if GetSchema(DBSCHEMA_COLUMNS, ['', Owner, Table, ''], Rows) then
      // Restriction columns: TABLE_CATALOG,TABLE_SCHEMA,TABLE_NAME,COLUMN_NAME
      with TSqlDBOleDBStatement.Create(MainConnection) do
      try
        FromRowSet(Rows);
        FA.Init(TypeInfo(TSqlDBColumnDefineDynArray), Fields, @n);
        while Step do
        begin
          F.ColumnName := TrimU(ColumnUtf8('COLUMN_NAME'));
          F.ColumnLength := ColumnInt('CHARACTER_MAXIMUM_LENGTH');
          F.ColumnPrecision := ColumnInt('NUMERIC_PRECISION');
          F.ColumnScale := ColumnInt('NUMERIC_SCALE');
          F.ColumnType := OleDBColumnToFieldType(ColumnInt('DATA_TYPE'), F.ColumnScale);
          F.ColumnTypeNative := DBTYPE_DISPLAY[F.ColumnType];
          FA.Add(F);
        end;
        SetLength(Fields, n);
      finally
        Free;
      end;
    // now we have Fields[] with the column information -> get indexes and foreign keys
    if GetSchema(DBSCHEMA_INDEXES, ['', Owner, '', '', Table], Rows) then
      // Restriction columns: TABLE_CATALOG,TABLE_SCHEMA,INDEX_NAME,TYPE,TABLE_NAME
      with TSqlDBOleDBStatement.Create(MainConnection) do
      try
        FromRowSet(Rows);
        while Step do
        begin
          Column := TrimU(ColumnUtf8('COLUMN_NAME'));
          for i := 0 to high(Fields) do
            with Fields[i] do
              if IdemPropNameU(ColumnName, Column) then
              begin
                ColumnIndexed := true;
                break;
              end;
        end;
      finally
        Free;
      end;
  except
    on Exception do
      SetLength(Fields, 0);
  end;
end;

procedure TSqlDBOleDBConnectionProperties.GetForeignKeys;
var
  Rows: IRowset;
begin
  // retrieve all foreign keys into fForeignKeys list
  try
    if GetSchema(DBSCHEMA_FOREIGN_KEYS, ['', '', '', '', '', ''], Rows) then
    // PK_TABLE_CATALOG,PK_TABLE_SCHEMA,PK_TABLE_NAME,FK_TABLE_CATALOG,FK_TABLE_SCHEMA,FK_TABLE_NAME
      with TSqlDBOleDBStatement.Create(MainConnection) do
      try
        FromRowSet(Rows);
        while Step do
          fForeignKeys.Add(TrimU(ColumnUtf8('FK_TABLE_SCHEMA')) + '.' +
            TrimU(ColumnUtf8('FK_TABLE_NAME')) + '.' +
              TrimU(ColumnUtf8('FK_COLUMN_NAME')),
            TrimU(ColumnUtf8('PK_TABLE_SCHEMA')) + '.' +
              TrimU(ColumnUtf8('PK_TABLE_NAME')) + '.' +
              TrimU(ColumnUtf8('PK_COLUMN_NAME')));
      finally
        Free;
      end;
  except
    on Exception do
      ; // just ignore errors here
  end;
end;

function TSqlDBOleDBConnectionProperties.GetSchema(const aUID: TGUID;
  const Fields: array of RawUtf8; var aResult: IRowset): boolean;
var
  i, res, n: integer;
  C: TSqlDBOleDBConnection;
  SRS: IDBSchemaRowset;
  PG, OG: PGUID;
  PI, OI: PInteger;
  Args: array of Variant;
begin
  result := false;
  if (self = nil) or
     (high(Fields) < 0) then
    exit;
  C := MainConnection as TSqlDBOleDBConnection;
  if C.fSession = nil then
    C.Connect;
  C.fSession.QueryInterface(IDBSchemaRowset, SRS);
  if not Assigned(SRS) then
    exit; // provider do not support this interface
  if fSchemaRec = nil then
  begin
    SRS.GetSchemas(n, OG, OI);
    if n > 0 then
    try
      SetLength(fSchemaRec, n);
      PG := OG;
      PI := OI;
      for i := 0 to n - 1 do
        with fSchemaRec[i] do
        begin
          SchemaGuid := PG^;
          SupportedRestrictions := PI^;
          inc(PG);
          inc(PI);
        end;
    finally
      C.fMalloc.Free(OG);
      C.fMalloc.Free(OI);
    end;
  end;
  res := 0;
  for i := 0 to high(fSchemaRec) do
    if IsEqualGuid(@fSchemaRec[i].SchemaGuid, @aUID) then
    begin
      res := fSchemaRec[i].SupportedRestrictions;
      break;
    end;
  if res = 0 then
    exit;
  SetLength(Args, length(Fields));
  for i := 0 to high(Fields) do
    if ((res and (1 shl i)) <> 0) and
       (Fields[i] <> '') then
        // '' will leave VT_EMPTY parameter = no restriction
        Args[i] := Utf8ToWideString(Fields[i]); // expect parameter as BSTR
  aResult := nil;
  try
    C.OleDBCheck(nil,
      SRS.GetRowset(nil, aUID, length(Args), Args, IID_IRowset, 0, nil, aResult));
    result := aResult <> nil; // mark some rows retrieved
  except
    result := false;
  end;
end;

function TSqlDBOleDBConnectionProperties.NewConnection: TSqlDBConnection;
begin
  result := TSqlDBOleDBConnection.Create(self);
end;

procedure TSqlDBOleDBConnectionProperties.SetInternalProperties;
var
  tmp: RawUtf8;
begin
  if fProviderName <> '' then
    tmp := 'Provider=' + fProviderName + ';';
  if fServerName <> '' then
    tmp := {%H-}tmp + 'Data Source=' + fServerName + ';';
  if fDatabaseName <> '' then
    tmp := tmp + 'Initial Catalog=' + fDatabaseName + ';';
  fConnectionString := Utf8ToSynUnicode(tmp + 'User Id=' + fUserID +
    ';Password=' + fPassWord + ';');
end;

function TSqlDBOleDBConnectionProperties.ColumnTypeNativeToDB(
  const aNativeType: RawUtf8; aScale: integer): TSqlDBFieldType;
var
  native, err: integer;
begin
  native := GetInteger(pointer(aNativeType), err);
  if err = 0 then
    // type directly retrieved from OleDB as integer
    result := OleDBColumnToFieldType(native, aScale)
  else
    // type retrieved via a SELECT from INFORMATION_SCHEMA.COLUMNS
    result := inherited ColumnTypeNativeToDB(aNativeType, aScale);
end;


{ ************ Database Engine Specific OleDB Connection Classes }

{ TSqlDBOleDBOracleConnectionProperties }

procedure TSqlDBOleDBOracleConnectionProperties.SetInternalProperties;
begin
  if fProviderName = '' then
    fProviderName := 'OraOLEDB.Oracle.1';
  fDbms := dOracle;
  inherited SetInternalProperties;
end;


{ TSqlDBOleDBMSOracleConnectionProperties }

procedure TSqlDBOleDBMSOracleConnectionProperties.SetInternalProperties;
begin
  fProviderName := 'MSDAORA';
  fDbms := dOracle;
  inherited SetInternalProperties;
end;


{ TSqlDBOleDBMSSQLConnectionProperties }

function TSqlDBOleDBMSSQLConnectionProperties.MSOnCustomError(
  Connection: TSqlDBOleDBConnection; ErrorRecords: IErrorRecords;
  RecordNum: cardinal): boolean;
var
  SQLServerErrorInfo: ISQLServerErrorInfo;
  SSErrorInfo: PSSERRORINFO;
  SSErrorMsg: PWideChar;
  msg, tmp: string;
  u: RawUtf8;
begin
  result := False;
  if (self = nil) or
     (Connection = nil) then
    exit;
  ErrorRecords.GetCustomErrorObject(RecordNum, IID_ISQLServerErrorInfo, IUnknown
    (SQLServerErrorInfo));
  if Assigned(SQLServerErrorInfo) then
  try
    if (SQLServerErrorInfo.GetErrorInfo(SSErrorInfo, SSErrorMsg) = S_OK) and
       (SSErrorInfo <> nil) then
      with SSErrorInfo^ do
      try
        msg := UnicodeBufferToString(pwszMessage) + #13#10;
        if bClass <= 10 then
        begin
          Connection.fOleDBInfoMessage := Connection.fOleDBInfoMessage + msg;
          RawUnicodeToUtf8(pwszMessage, StrLenW(pwszMessage), u);
          SynDBLog.Add.Log(sllDB, u, self);
          with Connection.Properties do
            if Assigned(OnStatementInfo) then
              OnStatementInfo(nil, u);
        end
        else
        begin
          if pwszProcedure <> nil then
            tmp := UnicodeBufferToString(pwszProcedure)
          else
            tmp := 'Error ' + IntToStr(lNative);
          Connection.fOleDBErrorMessage := FormatString('% % (line %): %',
            [Connection.fOleDBErrorMessage, tmp, wLineNumber, msg]);
        end;
      finally
        Connection.fMalloc.Free(SSErrorInfo);
        Connection.fMalloc.Free(SSErrorMsg);
      end;
    result := true;
  finally
    SQLServerErrorInfo := nil;
  end;
end;

procedure TSqlDBOleDBMSSQLConnectionProperties.SetInternalProperties;
begin
  OnCustomError := MSOnCustomError;
  if fProviderName = '' then
    fProviderName := 'SQLNCLI10';
  fDbms := dMSSQL;
  inherited SetInternalProperties;
  if fUserID = '' then
    fConnectionString := fConnectionString +
      'Integrated Security=SSPI;Persist Security Info=False;';
end;


{ TSqlDBOleDBMSSQL2005ConnectionProperties }

procedure TSqlDBOleDBMSSQL2005ConnectionProperties.SetInternalProperties;
begin
  fProviderName := 'SQLNCLI';
  inherited SetInternalProperties;
end;

constructor TSqlDBOleDBMSSQL2005ConnectionProperties.Create(const aServerName,
  aDatabaseName, aUserID, aPassWord: RawUtf8);
begin
  inherited;
  fBatchSendingAbilities := [];
  fOnBatchInsert := nil; // MultipleValuesInsert() does not work with SQL 2005
end;


{ TSqlDBOleDBMSSQL2012ConnectionProperties }

procedure TSqlDBOleDBMSSQL2012ConnectionProperties.SetInternalProperties;
begin
  if OSVersion > wVista then
    fProviderName := 'SQLNCLI11';
  inherited SetInternalProperties;
end;


{ TSqlDBOleDBOdbcSQLConnectionProperties }

constructor TSqlDBOleDBOdbcSQLConnectionProperties.Create(const aDriver,
  aServerName, aDatabaseName, aUserID, aPassWord: RawUtf8);
begin
  fDriver := aDriver;
  inherited Create(aServerName, aDatabaseName, aUserID, aPassWord);
end;

procedure TSqlDBOleDBOdbcSQLConnectionProperties.SetInternalProperties;
begin
  fProviderName := 'MSDASQL'; // we could have left it void - never mind
  inherited SetInternalProperties;
  if fDriver <> '' then
    fConnectionString := Utf8ToSynUnicode('Driver=' + fDriver + ';') + fConnectionString;
end;


{ TSqlDBOleDBMySQLConnectionProperties }

procedure TSqlDBOleDBMySQLConnectionProperties.SetInternalProperties;
begin
  fProviderName := 'MYSQLPROV';
  fDbms := dMySQL;
  inherited;
end;


{ TSqlDBOleDBAS400ConnectionProperties }

procedure TSqlDBOleDBAS400ConnectionProperties.SetInternalProperties;
begin
  fProviderName := 'IBMDA400.DataSource.1';
  inherited SetInternalProperties;
end;


{ TSqlDBOleDBInformixConnectionProperties }

procedure TSqlDBOleDBInformixConnectionProperties.SetInternalProperties;
begin
  fProviderName := 'Ifxoledbc';
  fDbms := dInformix;
  inherited SetInternalProperties;
end;

{$ifndef CPU64}

{ TSqlDBOleDBJetConnectionProperties }

procedure TSqlDBOleDBJetConnectionProperties.SetInternalProperties;
begin
  fProviderName := 'Microsoft.Jet.OLEDB.4.0';
  fDbms := dJet;
  inherited SetInternalProperties;
  if not FileExists(Utf8ToString(ServerName)) then
    CreateDatabase;
end;

{$endif CPU64}


{ TSqlDBOleDBACEConnectionProperties }

procedure TSqlDBOleDBACEConnectionProperties.SetInternalProperties;
begin
  fProviderName := 'Microsoft.ACE.OLEDB.12.0';
  fDbms := dJet;
  inherited SetInternalProperties;
  if not FileExists(Utf8ToString(ServerName)) then
    CreateDatabase;
end;


initialization
  TSqlDBOleDBConnectionProperties.RegisterClassNameForDefinition;
  TSqlDBOleDBConnectionProperties.RegisterClassNameForDefinition;
  TSqlDBOleDBOracleConnectionProperties.RegisterClassNameForDefinition;
  TSqlDBOleDBMSOracleConnectionProperties.RegisterClassNameForDefinition;
  TSqlDBOleDBMSSQLConnectionProperties.RegisterClassNameForDefinition;
  TSqlDBOleDBMSSQL2005ConnectionProperties.RegisterClassNameForDefinition;
  TSqlDBOleDBMSSQL2008ConnectionProperties.RegisterClassNameForDefinition;
  TSqlDBOleDBMSSQL2012ConnectionProperties.RegisterClassNameForDefinition;
  TSqlDBOleDBMySQLConnectionProperties.RegisterClassNameForDefinition;
  {$ifndef CPU64} // Jet is not available on Win64
  TSqlDBOleDBJetConnectionProperties.RegisterClassNameForDefinition;
  {$endif}
  TSqlDBOleDBACEConnectionProperties.RegisterClassNameForDefinition;
  TSqlDBOleDBAS400ConnectionProperties.RegisterClassNameForDefinition;
  TSqlDBOleDBOdbcSQLConnectionProperties.RegisterClassNameForDefinition;

  {$ifndef PUREMORMOT2}
  // backward compatibility types registration
  TOleDBConnectionProperties.RegisterClassNameForDefinition;
  TOleDBOracleConnectionProperties.RegisterClassNameForDefinition;
  TOleDBMSOracleConnectionProperties.RegisterClassNameForDefinition;
  TOleDBMSSQLConnectionProperties.RegisterClassNameForDefinition;
  TOleDBMSSQL2005ConnectionProperties.RegisterClassNameForDefinition;
  TOleDBMSSQL2008ConnectionProperties.RegisterClassNameForDefinition;
  TOleDBMSSQL2012ConnectionProperties.RegisterClassNameForDefinition;
  TOleDBMySQLConnectionProperties.RegisterClassNameForDefinition;
  {$ifndef CPU64} // Jet is not available on Win64
  TOleDBJetConnectionProperties.RegisterClassNameForDefinition;
  {$endif}
  TOleDBACEConnectionProperties.RegisterClassNameForDefinition;
  TOleDBAS400ConnectionProperties.RegisterClassNameForDefinition;
  TOleDBOdbcSQLConnectionProperties.RegisterClassNameForDefinition;
  {$endif PUREMORMOT2}


{$else}

implementation

{$endif OSWINDOWS} // compiles as void unit for non-Windows - allow Lazarus package

end.

