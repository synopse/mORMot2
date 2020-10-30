/// Database Framework Low-Level ODBC Access
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.raw.odbc;

{
  *****************************************************************************

   Efficient Direct ODBC API Access
    - Native ODBC Constants
    - Native ODBC Memory Structures
    - ODBC Library Loading

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.core.log,
  mormot.db.core,
  mormot.db.sql;


{ ************ Native ODBC Constants }

const
  SQL_NULL_DATA = -1;
  SQL_DATA_AT_EXEC = -2;
  SQL_NO_TOTAL = -4;

  // return values from functions
  SQL_SUCCESS = 0;
  SQL_SUCCESS_WITH_INFO = 1;

  SQL_NO_DATA = 100;

  SQL_PARAM_TYPE_UNKNOWN = 0;
  SQL_PARAM_INPUT = 1;
  SQL_PARAM_INPUT_OUTPUT = 2;
  SQL_RESULT_COL = 3;
  SQL_PARAM_OUTPUT = 4;
  SQL_RETURN_VALUE = 5;
  SQL_PARAM_DATA_AVAILABLE = 101;

  SQL_ERROR = -1;
  SQL_INVALID_HANDLE = -2;

  SQL_STILL_EXECUTING = 2;
  SQL_NEED_DATA = 99;

  // flags for null-terminated string
  SQL_NTS = -3;
  SQL_NTSL = -3;

  // maximum message length
  SQL_MAX_MESSAGE_LENGTH = 512;

  // date/time length constants
  SQL_DATE_LEN = 10;
  // add P+1 if precision is nonzero
  SQL_TIME_LEN = 8;
  // add P+1 if precision is nonzero
  SQL_TIMESTAMP_LEN = 19;

  // handle type identifiers
  SQL_HANDLE_ENV = 1;
  SQL_HANDLE_DBC = 2;
  SQL_HANDLE_STMT = 3;
  SQL_HANDLE_DESC = 4;

  // env attribute
  SQL_ATTR_ODBC_VERSION = 200;
  SQL_ATTR_CONNECTION_POOLING = 201;
  SQL_ATTR_CP_MATCH = 202;
  SQL_ATTR_OUTPUT_NTS = 10001;
  SQL_OV_ODBC3 = pointer(3);

  // values for SQLStatistics()
  SQL_INDEX_UNIQUE = 0;
  SQL_INDEX_ALL = 1;
  SQL_QUICK = 0;
  SQL_ENSURE = 1;

  // connection attributes
  SQL_ACCESS_MODE = 101;
  SQL_AUTOCOMMIT = 102;
  SQL_LOGIN_TIMEOUT = 103;
  SQL_OPT_TRACE = 104;
  SQL_OPT_TRACEFILE = 105;
  SQL_TRANSLATE_DLL = 106;
  SQL_TRANSLATE_OPTION = 107;
  SQL_TXN_ISOLATION = 108;
  SQL_CURRENT_QUALIFIER = 109;
  SQL_ODBC_CURSORS = 110;
  SQL_QUIET_MODE = 111;
  SQL_PACKET_SIZE = 112;
  SQL_ATTR_AUTO_IPD = 10001;
  SQL_ATTR_METADATA_ID = 10014;

  // statement attributes
  SQL_ATTR_APP_ROW_DESC = 10010;
  SQL_ATTR_APP_PARAM_DESC = 10011;
  SQL_ATTR_IMP_ROW_DESC = 10012;
  SQL_ATTR_IMP_PARAM_DESC = 10013;
  SQL_ATTR_CURSOR_SCROLLABLE = -1;
  SQL_ATTR_CURSOR_SENSITIVITY = -2;

  // SQL_ATTR_CURSOR_SCROLLABLE values
  SQL_NONSCROLLABLE = 0;
  SQL_SCROLLABLE = 1;

	// SQL_AUTOCOMMIT options
  SQL_AUTOCOMMIT_OFF = pointer(0);
  SQL_AUTOCOMMIT_ON = pointer(1);

  // identifiers of fields in the SQL descriptor
  SQL_DESC_COUNT = 1001;
  SQL_DESC_TYPE = 1002;
  SQL_DESC_LENGTH = 1003;
  SQL_DESC_OCTET_LENGTH_PTR = 1004;
  SQL_DESC_PRECISION = 1005;
  SQL_DESC_SCALE = 1006;
  SQL_DESC_DATETIME_INTERVAL_CODE = 1007;
  SQL_DESC_NULLABLE = 1008;
  SQL_DESC_INDICATOR_PTR = 1009;
  SQL_DESC_DATA_PTR = 1010;
  SQL_DESC_NAME = 1011;
  SQL_DESC_UNNAMED = 1012;
  SQL_DESC_OCTET_LENGTH = 1013;
  SQL_DESC_ALLOC_TYPE = 1099;

  // identifiers of fields in the diagnostics area
  SQL_DIAG_RETURNCODE = 1;
  SQL_DIAG_NUMBER = 2;
  SQL_DIAG_ROW_COUNT = 3;
  SQL_DIAG_SQLSTATE = 4;
  SQL_DIAG_NATIVE = 5;
  SQL_DIAG_MESSAGE_TEXT = 6;
  SQL_DIAG_DYNAMIC_FUNCTION = 7;
  SQL_DIAG_CLASS_ORIGIN = 8;
  SQL_DIAG_SUBCLASS_ORIGIN = 9;
  SQL_DIAG_CONNECTION_NAME = 10;
  SQL_DIAG_SERVER_NAME = 11;
  SQL_DIAG_DYNAMIC_FUNCTION_CODE = 12;

  // SQL data type codes
  SQL_UNKNOWN_TYPE = 0;
  SQL_CHAR = 1;
  SQL_NUMERIC = 2;
  SQL_DECIMAL = 3;
  SQL_INTEGER = 4;
  SQL_SMALLINT = 5;
  SQL_FLOAT = 6;
  SQL_REAL = 7;
  SQL_DOUBLE = 8;
  SQL_DATETIME = 9;
  SQL_DATE = 9;
  SQL_INTERVAL = 10;
  SQL_TIME = 10;
  SQL_TIMESTAMP = 11;
  SQL_VARCHAR = 12;
  SQL_LONGVARCHAR = -1;
  SQL_BINARY = -2;
  SQL_VARBINARY = -3;
  SQL_LONGVARBINARY = -4;
  SQL_BIGINT = -5;
  SQL_TINYINT = -6;
  SQL_BIT = -7;
  SQL_WCHAR = -8;
  SQL_WVARCHAR = -9;
  SQL_WLONGVARCHAR = -10;
  SQL_GUID = -11;

  // One-parameter shortcuts for date/time data types
  SQL_TYPE_DATE = 91;
  SQL_TYPE_TIME = 92;
  SQL_TYPE_TIMESTAMP = 93;

  // C datatype to SQL datatype mapping
  SQL_C_CHAR = SQL_CHAR;
  SQL_C_WCHAR = SQL_WCHAR;
  SQL_C_LONG = SQL_INTEGER;
  SQL_C_SHORT = SQL_SMALLINT;
  SQL_C_FLOAT = SQL_REAL;
  SQL_C_DOUBLE = SQL_DOUBLE;
  SQL_C_NUMERIC = SQL_NUMERIC;
  SQL_C_DEFAULT = 99;
  SQL_SIGNED_OFFSET = -20;
  SQL_UNSIGNED_OFFSET = -22;
  SQL_C_DATE = SQL_DATE;
  SQL_C_TIME = SQL_TIME;
  SQL_C_TIMESTAMP = SQL_TIMESTAMP;
  SQL_C_TYPE_DATE = SQL_TYPE_DATE;
  SQL_C_TYPE_TIME = SQL_TYPE_TIME;
  SQL_C_TYPE_TIMESTAMP = SQL_TYPE_TIMESTAMP;
  SQL_C_BINARY = SQL_BINARY;
  SQL_C_BIT = SQL_BIT;
  SQL_C_SBIGINT = SQL_BIGINT + SQL_SIGNED_OFFSET;
  SQL_C_UBIGINT = SQL_BIGINT + SQL_UNSIGNED_OFFSET;
  SQL_C_TINYINT = SQL_TINYINT;
  SQL_C_SLONG = SQL_C_LONG + SQL_SIGNED_OFFSET;
  SQL_C_SSHORT = SQL_C_SHORT + SQL_SIGNED_OFFSET;
  SQL_C_STINYINT = SQL_TINYINT + SQL_SIGNED_OFFSET;
  SQL_C_ULONG = SQL_C_LONG + SQL_UNSIGNED_OFFSET;
  SQL_C_USHORT = SQL_C_SHORT + SQL_UNSIGNED_OFFSET;
  SQL_C_UTINYINT = SQL_TINYINT  +  SQL_UNSIGNED_OFFSET;

  // Driver specific SQL data type defines.
  // Microsoft has -150 thru -199 reserved for Microsoft SQL Server Native Client driver usage.
  SQL_SS_VARIANT = -150;
  SQL_SS_UDT = -151;
  SQL_SS_XML = -152;
  SQL_SS_TABLE = -153;
  SQL_SS_TIME2 = -154;
  SQL_SS_TIMESTAMPOFFSET = -155;

  // Statement attribute values for cursor sensitivity
  SQL_UNSPECIFIED = 0;
  SQL_INSENSITIVE = 1;
  SQL_SENSITIVE = 2;

  // GetTypeInfo() request for all data types
  SQL_ALL_TYPES = 0;

  // Default conversion code for SQLBindCol(), SQLBindParam() and SQLGetData()
  SQL_DEFAULT = 99;

  // SQLSQLLEN GetData() code indicating that the application row descriptor
  // specifies the data type
  SQL_ARD_TYPE = -99;

  SQL_APD_TYPE = -100;

  // SQL date/time type subcodes
  SQL_CODE_DATE = 1;
  SQL_CODE_TIME = 2;
  SQL_CODE_TIMESTAMP = 3;

  // CLI option values
  SQL_FALSE = 0;
  SQL_TRUE = 1;

  // values of NULLABLE field in descriptor
  SQL_NO_NULLS = 0;
  SQL_NULLABLE = 1;

  // Value returned by SQLGetTypeInfo() to denote that it is
  // not known whether or not a data type supports null values.
  SQL_NULLABLE_UNKNOWN = 2;

  // Values returned by SQLGetTypeInfo() to show WHERE clause supported
  SQL_PRED_NONE = 0;
  SQL_PRED_CHAR = 1;
  SQL_PRED_BASIC = 2;

  // values of UNNAMED field in descriptor
  SQL_NAMED = 0;
  SQL_UNNAMED = 1;

  // values of ALLOC_TYPE field in descriptor
  SQL_DESC_ALLOC_AUTO = 1;
  SQL_DESC_ALLOC_USER = 2;

  // FreeStmt() options
  SQL_CLOSE = 0;
  SQL_DROP = 1;
  SQL_UNBIND = 2;
  SQL_RESET_PARAMS = 3;

  // Codes used for FetchOrientation in SQLFetchScroll() and SQLDataSources()
  SQL_FETCH_NEXT = 1;
  SQL_FETCH_FIRST = 2;

  // Other codes used for FetchOrientation in SQLFetchScroll()
  SQL_FETCH_LAST = 3;
  SQL_FETCH_PRIOR = 4;
  SQL_FETCH_ABSOLUTE = 5;
  SQL_FETCH_RELATIVE = 6;

  // SQLEndTran() options
  SQL_COMMIT = 0;
  SQL_ROLLBACK = 1;

  // null handles returned by SQLAllocHandle()
  SQL_NULL_HENV = 0;
  SQL_NULL_HDBC = 0;
  SQL_NULL_HSTMT = 0;
  SQL_NULL_HDESC = 0;

  // null handle used in place of parent handle when allocating HENV
  SQL_NULL_HANDLE = nil;

  // Information requested by SQLGetInfo()
  SQL_MAX_DRIVER_CONNECTIONS = 0;
  SQL_MAXIMUM_DRIVER_CONNECTIONS = SQL_MAX_DRIVER_CONNECTIONS;
  SQL_MAX_CONCURRENT_ACTIVITIES = 1;
  SQL_MAXIMUM_CONCURRENT_ACTIVITIES = SQL_MAX_CONCURRENT_ACTIVITIES;
  SQL_DATA_SOURCE_NAME = 2;
  SQL_FETCH_DIRECTION = 8;
  SQL_SERVER_NAME = 13;
  SQL_SEARCH_PATTERN_ESCAPE = 14;
  SQL_DRIVER_NAME = 6;
  SQL_DBMS_NAME = 17;
  SQL_DBMS_VER = 18;
  SQL_ACCESSIBLE_TABLES = 19;
  SQL_ACCESSIBLE_PROCEDURES = 20;
  SQL_CURSOR_COMMIT_BEHAVIOR = 23;
  SQL_DATA_SOURCE_READ_ONLY = 25;
  SQL_DEFAULT_TXN_ISOLATION = 26;
  SQL_IDENTIFIER_CASE = 28;
  SQL_IDENTIFIER_QUOTE_CHAR = 29;
  SQL_MAX_COLUMN_NAME_LEN = 30;
  SQL_MAXIMUM_COLUMN_NAME_LENGTH = SQL_MAX_COLUMN_NAME_LEN;
  SQL_MAX_CURSOR_NAME_LEN = 31;
  SQL_MAXIMUM_CURSOR_NAME_LENGTH = SQL_MAX_CURSOR_NAME_LEN;
  SQL_MAX_SCHEMA_NAME_LEN = 32;
  SQL_MAXIMUM_SCHEMA_NAME_LENGTH = SQL_MAX_SCHEMA_NAME_LEN;
  SQL_MAX_CATALOG_NAME_LEN = 34;
  SQL_MAXIMUM_CATALOG_NAME_LENGTH = SQL_MAX_CATALOG_NAME_LEN;
  SQL_MAX_TABLE_NAME_LEN = 35;
  SQL_SCROLL_CONCURRENCY = 43;
  SQL_TXN_CAPABLE = 46;
  SQL_TRANSACTION_CAPABLE = SQL_TXN_CAPABLE;
  SQL_USER_NAME = 47;
  SQL_TXN_ISOLATION_OPTION = 72;
  SQL_TRANSACTION_ISOLATION_OPTION = SQL_TXN_ISOLATION_OPTION;
  SQL_INTEGRITY = 73;
  SQL_GETDATA_EXTENSIONS = 81;
  SQL_NULL_COLLATION = 85;
  SQL_ALTER_TABLE = 86;
  SQL_ORDER_BY_COLUMNS_IN_SELECT = 90;
  SQL_SPECIAL_CHARACTERS = 94;
  SQL_MAX_COLUMNS_IN_GROUP_BY = 97;
  SQL_MAXIMUM_COLUMNS_IN_GROUP_BY = SQL_MAX_COLUMNS_IN_GROUP_BY;
  SQL_MAX_COLUMNS_IN_INDEX = 98;
  SQL_MAXIMUM_COLUMNS_IN_INDEX = SQL_MAX_COLUMNS_IN_INDEX;
  SQL_MAX_COLUMNS_IN_ORDER_BY = 99;
  SQL_MAXIMUM_COLUMNS_IN_ORDER_BY = SQL_MAX_COLUMNS_IN_ORDER_BY;
  SQL_MAX_COLUMNS_IN_SELECT = 100;
  SQL_MAXIMUM_COLUMNS_IN_SELECT = SQL_MAX_COLUMNS_IN_SELECT;
  SQL_MAX_COLUMNS_IN_TABLE = 101;
  SQL_MAX_INDEX_SIZE = 102;
  SQL_MAXIMUM_INDEX_SIZE = SQL_MAX_INDEX_SIZE;
  SQL_MAX_ROW_SIZE = 104;
  SQL_MAXIMUM_ROW_SIZE = SQL_MAX_ROW_SIZE;
  SQL_MAX_STATEMENT_LEN = 105;
  SQL_MAXIMUM_STATEMENT_LENGTH = SQL_MAX_STATEMENT_LEN;
  SQL_MAX_TABLES_IN_SELECT = 106;
  SQL_MAXIMUM_TABLES_IN_SELECT = SQL_MAX_TABLES_IN_SELECT;
  SQL_MAX_USER_NAME_LEN = 107;
  SQL_MAXIMUM_USER_NAME_LENGTH = SQL_MAX_USER_NAME_LEN;
  SQL_OJ_CAPABILITIES = 115;
  SQL_OUTER_JOIN_CAPABILITIES = SQL_OJ_CAPABILITIES;

  // Options for SQLDriverConnect
  SQL_DRIVER_NOPROMPT = 0;
  SQL_DRIVER_COMPLETE = 1;
  SQL_DRIVER_PROMPT = 2;
  SQL_DRIVER_COMPLETE_REQUIRED = 3;

  // SQLSetStmtAttr SQL Server Native Client driver specific defines.
  // Statement attributes
  SQL_SOPT_SS_BASE                      = 1225;
  SQL_SOPT_SS_TEXTPTR_LOGGING           = SQL_SOPT_SS_BASE + 0; // Text pointer logging
  SQL_SOPT_SS_CURRENT_COMMAND           = SQL_SOPT_SS_BASE + 1; // dbcurcmd SQLGetStmtOption only
  SQL_SOPT_SS_HIDDEN_COLUMNS            = SQL_SOPT_SS_BASE + 2; // Expose FOR BROWSE hidden columns
  SQL_SOPT_SS_NOBROWSETABLE             = SQL_SOPT_SS_BASE + 3; // Set NOBROWSETABLE option
  SQL_SOPT_SS_REGIONALIZE               = SQL_SOPT_SS_BASE + 4; // Regionalize output character conversions
  SQL_SOPT_SS_CURSOR_OPTIONS            = SQL_SOPT_SS_BASE + 5; // Server cursor options
  SQL_SOPT_SS_NOCOUNT_STATUS            = SQL_SOPT_SS_BASE + 6; // Real vs. Not Real row count indicator
  SQL_SOPT_SS_DEFER_PREPARE             = SQL_SOPT_SS_BASE + 7; // Defer prepare until necessary
  SQL_SOPT_SS_QUERYNOTIFICATION_TIMEOUT = SQL_SOPT_SS_BASE + 8; // Notification timeout
  SQL_SOPT_SS_QUERYNOTIFICATION_MSGTEXT = SQL_SOPT_SS_BASE + 9; // Notification message text
  SQL_SOPT_SS_QUERYNOTIFICATION_OPTIONS = SQL_SOPT_SS_BASE + 10;// SQL service broker name
  SQL_SOPT_SS_PARAM_FOCUS               = SQL_SOPT_SS_BASE + 11;// Direct subsequent calls to parameter related methods to set properties on constituent columns/parameters of container types
  SQL_SOPT_SS_NAME_SCOPE                = SQL_SOPT_SS_BASE + 12;// Sets name scope for subsequent catalog function calls
  SQL_SOPT_SS_MAX_USED                  = SQL_SOPT_SS_NAME_SCOPE;

  SQL_IS_POINTER                        = -4;
  SQL_IS_UINTEGER                       = -5;
  SQL_IS_INTEGER                        = -6;
  SQL_IS_USMALLINT                      = -7;
  SQL_IS_SMALLINT                       = -8;

type
  SqlSmallint = Smallint;
  SqlDate = Byte;
  SqlTime = Byte;
  SqlDecimal = Byte;
  SqlDouble = Double;
  SqlFloat = Double;
  SqlInteger = integer;
  SqlUInteger = cardinal;
  SqlNumeric = Byte;
  SqlPointer = Pointer;
  SqlReal = Single;
  SqlUSmallint = Word;
  SqlTimestamp = Byte;
  SqlVarchar = Byte;
  PSqlSmallint = ^SqlSmallint;
  PSqlInteger = ^SqlInteger;

  SqlReturn = SqlSmallint;
  SqlLen = PtrInt;
  SqlULen = PtrUInt;
  {$ifdef CPU64}
  SqlSetPosIRow = PtrUInt;
  {$else}
  SqlSetPosIRow = Word;
  {$endif}
  PSqlLen = ^SqlLen;

  SqlHandle = Pointer;
  SqlHEnv = SqlHandle;
  SqlHDbc = SqlHandle;
  SqlHStmt = SqlHandle;
  SqlHDesc = SqlHandle;
  SqlHWnd = LongWord;


{ ************ Native ODBC Memory Structures }

type
  {$A-}

  /// memory structure used to store SQL_C_TYPE_TIMESTAMP values
  {$ifdef USERECORDWITHMETHODS}
  SQL_TIMESTAMP_STRUCT = record
  {$else}
  SQL_TIMESTAMP_STRUCT = object
  {$endif USERECORDWITHMETHODS}
    Year:     SqlSmallint;
    Month:    SqlUSmallint;
    Day:      SqlUSmallint;
    Hour:     SqlUSmallint;
    Minute:   SqlUSmallint;
    Second:   SqlUSmallint;
    Fraction: SqlUInteger;
    /// convert an ODBC date and time into TDateTime
    // - depending on the original column data type specified, it will return
    // either a TDate (for SQL_TYPE_DATE), either a TTime (for SQL_TYPE_TIME),
    // either a TDateTime content (for SQL_TYPE_TIMESTAMP)
    function ToDateTime(DataType: SqlSmallint = SQL_TYPE_TIMESTAMP): TDateTime;
    /// convert an ODBC date and time into its textual expanded ISO-8601
    // - will fill up to 21 characters, including double quotes
    // - depending on the column data type specified, it will return either an
    // ISO-8601 date (for SQL_TYPE_DATE), either a time (for SQL_TYPE_TIME),
    // either a full date + time ISO-8601 content (for SQL_TYPE_TIMESTAMP)
    function ToIso8601(Dest: PUTF8Char; DataType: SqlSmallint;
      WithMS: boolean = false): integer;
    /// convert a TDateTime into ODBC date or timestamp
    // - returns the corresponding C type, i.e. either SQL_C_TYPE_DATE,
    // either SQL_C_TYPE_TIMESTAMP and the corresponding size in bytes
    function From(DateTime: TDateTime; var ColumnSize: SqlLen): SqlSmallint;
  end;

  PSQL_TIMESTAMP_STRUCT = ^SQL_TIMESTAMP_STRUCT;

  SQL_TIME_STRUCT = record
    Hour:     SqlUSmallint;
    Minute:   SqlUSmallint;
    Second:   SqlUSmallint;
  end;

  SQL_DATE_STRUCT = record
    year:	SQLSMALLINT;
    month:	SQLUSMALLINT;
    day:	SQLUSMALLINT;
  end;

  {$A+}


{ ************ ODBC Library Loading }

type
  /// generic Exception type, generated for ODBC connection
  EODBCException = class(ESQLDBException);

  /// direct access to the ODBC library
  // - this wrapper will initialize both Ansi and Wide versions of the ODBC
  // driver functions, and will work with 32 bit and 64 bit version of the
  // interfaces, on Windows or POSIX platforms
  // - within this unit, we will only use Wide version, and UTF-8 conversion
  TODBCLib = class(TSynLibrary)
  public
    AllocEnv: function (var EnvironmentHandle: SqlHEnv): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    AllocHandle: function(HandleType: SqlSmallint; InputHandle: SqlHandle;
      var OutputHandle: SqlHandle): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    AllocStmt: function(ConnectionHandle: SqlHDbc; var StatementHandle: SqlHStmt): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    BindCol: function(StatementHandle: SqlHStmt; ColumnNumber: SqlUSmallint;
      TargetType: SqlSmallint; TargetValue: SqlPointer;
      BufferLength: SqlLen; StrLen_or_Ind: PSqlLen): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    BindParameter: function (StatementHandle: SqlHStmt; ParameterNumber: SqlUSmallint;
      InputOutputType, ValueType, ParameterType: SqlSmallint; ColumnSize: SqlULen;
      DecimalDigits: SqlSmallint; ParameterValue: SqlPointer; BufferLength: SqlLen;
      var StrLen_or_Ind: SqlLen): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    Cancel: function(StatementHandle: SqlHStmt): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    CloseCursor: function(StatementHandle: SqlHStmt): SqlReturn;
     {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ColAttributeA: function(StatementHandle: SqlHStmt; ColumnNumber: SqlUSmallint;
      FieldIdentifier: SqlUSmallint; CharacterAttribute: PAnsiChar;
      BufferLength: SqlSmallint; StringLength: PSqlSmallint; NumericAttributePtr: PSqlLen): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ColAttributeW: function(StatementHandle: SqlHStmt; ColumnNumber: SqlUSmallint;
      FieldIdentifier: SqlUSmallint; CharacterAttribute: PWideChar;
      BufferLength: SqlSmallint; StringLength: PSqlSmallint; NumericAttributePtr: PSqlLen): SqlReturn;
       {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ColumnsA: function(StatementHandle: SqlHStmt;
      CatalogName: PAnsiChar; NameLength1: SqlSmallint;
      SchemaName: PAnsiChar;  NameLength2: SqlSmallint;
      TableName: PAnsiChar;   NameLength3: SqlSmallint;
      ColumnName: PAnsiChar;  NameLength4: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ColumnsW: function(StatementHandle: SqlHStmt;
      CatalogName: PWideChar; NameLength1: SqlSmallint;
      SchemaName: PWideChar;  NameLength2: SqlSmallint;
      TableName: PWideChar;   NameLength3: SqlSmallint;
      ColumnName: PWideChar;  NameLength4: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    StatisticsA: function(StatementHandle: SqlHStmt;
      CatalogName: PAnsiChar; NameLength1: SqlSmallint;
      SchemaName: PAnsiChar;  NameLength2: SqlSmallint;
      TableName: PAnsiChar;   NameLength3: SqlSmallint;
      Unique, Reserved: SqlUSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    StatisticsW: function(StatementHandle: SqlHStmt;
      CatalogName: PWideChar; NameLength1: SqlSmallint;
      SchemaName: PWideChar;  NameLength2: SqlSmallint;
      TableName: PWideChar;   NameLength3: SqlSmallint;
      Unique, Reserved: SqlUSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ConnectA: function(ConnectionHandle: SqlHDbc;
      ServerName: PAnsiChar; NameLength1: SqlSmallint;
      UserName: PAnsiChar; NameLength2: SqlSmallint;
      Authentication: PAnsiChar; NameLength3: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ConnectW: function(ConnectionHandle: SqlHDbc;
      ServerName: PWideChar; NameLength1: SqlSmallint;
      UserName: PWideChar; NameLength2: SqlSmallint;
      Authentication: PWideChar; NameLength3: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    CopyDesc: function(SourceDescHandle, TargetDescHandle: SqlHDesc): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    DataSourcesA: function(EnvironmentHandle: SqlHEnv; Direction: SqlUSmallint;
      ServerName: PAnsiChar;  BufferLength1: SqlSmallint; var NameLength1: SqlSmallint;
      Description: PAnsiChar; BufferLength2: SqlSmallint; var NameLength2: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    DataSourcesW: function(EnvironmentHandle: SqlHEnv; Direction: SqlUSmallint;
      ServerName: PWideChar;  BufferLength1: SqlSmallint; var NameLength1: SqlSmallint;
      Description: PWideChar; BufferLength2: SqlSmallint; var NameLength2: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    DescribeColA: function(StatementHandle: SqlHStmt; ColumnNumber: SqlUSmallint;
      ColumnName: PAnsiChar; BufferLength: SqlSmallint; var NameLength: SqlSmallint;
      var DataType: SqlSmallint; var ColumnSize: SqlULen; var DecimalDigits: SqlSmallint;
      var Nullable: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    DescribeColW: function(StatementHandle: SqlHStmt; ColumnNumber: SqlUSmallint;
      ColumnName: PWideChar; BufferLength: SqlSmallint; var NameLength: SqlSmallint;
      var DataType: SqlSmallint; var ColumnSize: SqlULen; var DecimalDigits: SqlSmallint;
      var Nullable: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    Disconnect: function(ConnectionHandle: SqlHDbc): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    EndTran: function(HandleType: SqlSmallint; Handle: SqlHandle;
      CompletionType: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ErrorA: function(EnvironmentHandle: SqlHEnv; ConnectionHandle: SqlHDbc; StatementHandle: SqlHStmt;
      Sqlstate: PAnsiChar; var NativeError: SqlInteger;
      MessageText: PAnsiChar; BufferLength: SqlSmallint; var TextLength: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ErrorW: function(EnvironmentHandle: SqlHEnv; ConnectionHandle: SqlHDbc; StatementHandle: SqlHStmt;
      Sqlstate: PWideChar; var NativeError: SqlInteger;
      MessageText: PWideChar; BufferLength: SqlSmallint; var TextLength: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ExecDirectA: function(StatementHandle: SqlHStmt;
      StatementText: PAnsiChar; TextLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ExecDirectW: function(StatementHandle: SqlHStmt;
      StatementText: PWideChar; TextLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    Execute: function(StatementHandle: SqlHStmt): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    Fetch: function(StatementHandle: SqlHStmt): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    FetchScroll: function(StatementHandle: SqlHStmt;
      FetchOrientation: SqlSmallint; FetchOffset: SqlLen): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    FreeConnect: function(ConnectionHandle: SqlHDbc): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    FreeEnv: function(EnvironmentHandle: SqlHEnv): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    FreeHandle: function(HandleType: SqlSmallint; Handle: SqlHandle): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    FreeStmt: function(StatementHandle: SqlHStmt; Option: SqlUSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetConnectAttrA: function(ConnectionHandle: SqlHDbc; Attribute: SqlInteger;
      ValuePtr: SqlPointer; BufferLength: SqlInteger; pStringLength: pSqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetConnectAttrW: function(ConnectionHandle: SqlHDbc; Attribute: SqlInteger;
      ValuePtr: SqlPointer; BufferLength: SqlInteger; pStringLength: pSqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetCursorNameA: function(StatementHandle: SqlHStmt;
      CursorName: PAnsiChar; BufferLength: SqlSmallint; var NameLength: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetCursorNameW: function(StatementHandle: SqlHStmt;
      CursorName: PWideChar; BufferLength: SqlSmallint; var NameLength: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetData: function(StatementHandle: SqlHStmt; ColumnNumber: SqlUSmallint;
      TargetType: SqlSmallint; TargetValue: SqlPointer; BufferLength: SqlLen;
      StrLen_or_Ind: PSqlLen): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetDescFieldA: function(DescriptorHandle: SqlHDesc; RecNumber: SqlSmallint;
      FieldIdentifier: SqlSmallint; Value: SqlPointer; BufferLength: SqlInteger;
      var StringLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetDescFieldW: function(DescriptorHandle: SqlHDesc; RecNumber: SqlSmallint;
      FieldIdentifier: SqlSmallint; Value: SqlPointer; BufferLength: SqlInteger;
      var StringLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetDescRecA: function(DescriptorHandle: SqlHDesc; RecNumber: SqlSmallint;
      Name: PAnsiChar; BufferLength: SqlSmallint; var StringLength: SqlSmallint;
      var _Type, SubType: SqlSmallint; var Length: SqlLen;
      var Precision, Scale, Nullable: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetDescRecW: function(DescriptorHandle: SqlHDesc; RecNumber: SqlSmallint;
      Name: PWideChar; BufferLength: SqlSmallint; var StringLength: SqlSmallint;
      var _Type, SubType: SqlSmallint; var Length: SqlLen;
      var Precision, Scale, Nullable: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetDiagFieldA: function(HandleType: SqlSmallint; Handle: SqlHandle;
      RecNumber, DiagIdentifier: SqlSmallint;
      DiagInfo: SqlPointer; BufferLength: SqlSmallint; var StringLength: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetDiagFieldW: function(HandleType: SqlSmallint; Handle: SqlHandle;
      RecNumber, DiagIdentifier: SqlSmallint;
      DiagInfo: SqlPointer; BufferLength: SqlSmallint; var StringLength: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetDiagRecA: function(HandleType: SqlSmallint; Handle: SqlHandle; RecNumber: SqlSmallint;
      Sqlstate: PAnsiChar; var NativeError: SqlInteger;
      MessageText: PAnsiChar; BufferLength: SqlSmallint; var TextLength: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetDiagRecW: function(HandleType: SqlSmallint; Handle: SqlHandle; RecNumber: SqlSmallint;
      Sqlstate: PWideChar; var NativeError: SqlInteger;
      MessageText: PWideChar; BufferLength: SqlSmallint; var TextLength: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    MoreResults: function(StatementHandle: SqlHStmt): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    PrepareA: function(StatementHandle: SqlHStmt;
      StatementText: PAnsiChar; TextLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    PrepareW: function(StatementHandle: SqlHStmt;
      StatementText: PWideChar; TextLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    RowCount: function(StatementHandle: SqlHStmt; var RowCount: SqlLen): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    NumResultCols: function(StatementHandle: SqlHStmt; var ColumnCount: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetInfoA: function(ConnectionHandle: SqlHDbc; InfoType: SqlUSmallint;
      InfoValuePtr: SqlPointer; BufferLength: SqlSmallint; StringLengthPtr: PSqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetInfoW: function(ConnectionHandle: SqlHDbc; InfoType: SqlUSmallint;
      InfoValuePtr: SqlPointer; BufferLength: SqlSmallint; StringLengthPtr: PSqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    SetStmtAttrA: function(StatementHandle: SqlHStmt; Attribute: SqlInteger;
      Value: SqlPointer; StringLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    SetStmtAttrW: function(StatementHandle: SqlHStmt; Attribute: SqlInteger;
      Value: SqlPointer; StringLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    SetEnvAttr: function(EnvironmentHandle: SqlHEnv; Attribute: SqlInteger;
      ValuePtr: SqlPointer; StringLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    SetConnectAttrA: function(ConnectionHandle: SqlHDbc; Attribute: SqlInteger;
      ValuePtr: SqlPointer; StringLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    SetConnectAttrW: function(ConnectionHandle: SqlHDbc; Attribute: SqlInteger;
      ValuePtr: SqlPointer; StringLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    TablesA: function(StatementHandle: SqlHStmt;
      CatalogName: PAnsiChar; NameLength1: SqlSmallint;
      SchemaName: PAnsiChar; NameLength2: SqlSmallint;
      TableName: PAnsiChar; NameLength3: SqlSmallint;
      TableType: PAnsiChar; NameLength4: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    TablesW: function(StatementHandle: SqlHStmt;
      CatalogName: PWideChar; NameLength1: SqlSmallint;
      SchemaName: PWideChar; NameLength2: SqlSmallint;
      TableName: PWideChar; NameLength3: SqlSmallint;
      TableType: PWideChar; NameLength4: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ForeignKeysA: function(StatementHandle: SqlHStmt;
      PKCatalogName: PAnsiChar; NameLength1: SqlSmallint;
      PKSchemaName: PAnsiChar; NameLength2: SqlSmallint;
      PKTableName: PAnsiChar; NameLength3: SqlSmallint;
      FKCatalogName: PAnsiChar; NameLength4: SqlSmallint;
      FKSchemaName: PAnsiChar; NameLength5: SqlSmallint;
      FKTableName: PAnsiChar; NameLength6: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ForeignKeysW: function(StatementHandle: SqlHStmt;
      PKCatalogName: PWideChar; NameLength1: SqlSmallint;
      PKSchemaName: PWideChar; NameLength2: SqlSmallint;
      PKTableName: PWideChar; NameLength3: SqlSmallint;
      FKCatalogName: PWideChar; NameLength4: SqlSmallint;
      FKSchemaName: PWideChar; NameLength5: SqlSmallint;
      FKTableName: PWideChar; NameLength6: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    SQLDriverConnectA: function(ConnectionHandle: SqlHDbc; WindowHandle: SQLHWnd;
      InConnectionString: PAnsiChar; StringLength1: SqlSmallint;
      OutConnectionString: PAnsiChar; BufferLength: SqlSmallint;
      var StringLength2Ptr: SqlSmallint; DriverCompletion: SqlUSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    SQLDriverConnectW: function(ConnectionHandle: SqlHDbc; WindowHandle: SQLHWnd;
      InConnectionString: PWideChar; StringLength1: SqlSmallint;
      OutConnectionString: PWideChar; BufferLength: SqlSmallint;
      var StringLength2Ptr: SqlSmallint; DriverCompletion: SqlUSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    SQLProcedureColumnsA: function(StatementHandle: SqlHStmt;
      CatalogName: PAnsiChar; NameLength1: SqlSmallint;
      SchemaName: PAnsiChar;  NameLength2: SqlSmallint;
      ProcName: PAnsiChar;   NameLength3: SqlSmallint;
      ColumnName: PAnsiChar;  NameLength4: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    SQLProcedureColumnsW: function(StatementHandle: SqlHStmt;
      CatalogName: PWideChar; NameLength1: SqlSmallint;
      SchemaName: PWideChar;  NameLength2: SqlSmallint;
      ProcName: PWideChar;   NameLength3: SqlSmallint;
      ColumnName: PWideChar;  NameLength4: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    SQLProcedures: function(StatementHandle: SqlHStmt;
      CatalogName: PWideChar; NameLength1: SqlSmallint;
      SchemaName: PWideChar;  NameLength2: SqlSmallint;
      ProcName: PWideChar;   NameLength3: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
  public
    /// load the ODBC library
    // - and retrieve all SQL*() addresses for ODBC_ENTRIES[] items
    constructor Create;
    /// raise an exception on error
    procedure Check(Conn: TSQLDBConnection; Stmt: TSQLDBStatement; Status: SqlReturn;
      HandleType: SqlSmallint; Handle: SqlHandle; InfoRaiseException: boolean = false;
      LogLevelNoRaise: TSynLogInfo = sllNone);
      {$ifdef HASINLINE}inline;{$endif}
    /// generic process of error handle
    procedure HandleError(Conn: TSQLDBConnection; Stmt: TSQLDBStatement;
      Status: SqlReturn; HandleType: SqlSmallint; Handle: SqlHandle;
      InfoRaiseException: boolean; LogLevelNoRaise: TSynLogInfo);
    /// wrapper around SQLGetDiagField() API call
    function GetDiagField(StatementHandle: SqlHStmt): RawUTF8;
    /// wrapper around GetInfo() API call
    procedure GetInfoString(ConnectionHandle: SqlHDbc; InfoType: SqlUSmallint;
      var Dest: RawUTF8);
  end;

var
  ODBC: TODBCLib = nil;


{$ifdef MSWINDOWS}

/// List all ODBC drivers installed, by reading the Windows Registry
// - aDrivers is the output driver list container, which should be either nil (to
// create a new TStringList), or any existing TStrings instance (may be from VCL
// - aIncludeVersion: include the DLL driver version as <driver name>=<dll version>
// in aDrivers (somewhat slower)
function ODBCInstalledDriversList(const aIncludeVersion: boolean; var aDrivers: TStrings): boolean;

{$endif MSWINDOWS}


implementation

{$ifdef MSWINDOWS}
uses
  Windows,
  Registry; // for ODBCInstalledDriversList
{$endif MSWINDOWS}


{ ************ Native ODBC Memory Structures }

{ SQL_TIMESTAMP_STRUCT }

function SQL_TIMESTAMP_STRUCT.From(DateTime: TDateTime; var ColumnSize: SqlLen): SqlSmallint;
var
  Y, MS: word;
begin
  DecodeDate(DateTime, Y, Month, Day);
  Year := Y;
  if frac(DateTime) = 0 then
  begin
    PInt64(@Hour)^ := 0;
    Fraction := 0;
  end
  else
  begin
    DecodeTime(DateTime, Hour, Minute, Second, MS);
    Fraction := SqlUInteger(MS) * 1000000;
  end;
  if PInt64(@Hour)^ = 0 then
  begin
    result := SQL_C_TYPE_DATE;
    ColumnSize := SizeOf(SqlUSmallint) * 3;
  end
  else
  begin
    result := SQL_C_TYPE_TIMESTAMP;
    ColumnSize := SizeOf(SQL_TIMESTAMP_STRUCT);
  end;
end;

function SQL_TIMESTAMP_STRUCT.ToDateTime(DataType: SqlSmallint): TDateTime;
var
  time: TDateTime;
begin
  if DataType = SQL_TYPE_TIME then
    result := 0
  else
    result := EncodeDate(Year, Month, Day);
  if (DataType <> SQL_TYPE_DATE) and
     (PInt64(@Hour)^ <> 0)  and
     TryEncodeTime(Hour, Minute, Second, Fraction div 1000000, time) then
    result := result  +  time;
end;

function SQL_TIMESTAMP_STRUCT.ToIso8601(Dest: PUTF8Char; DataType: SqlSmallint;
  WithMS: boolean): integer;
begin
  Dest^ := '"';
  inc(Dest);
  if DataType <> SQL_TYPE_TIME then
  begin
    DateToIso8601PChar(Dest, true, Year, Month, Day);
    inc(Dest, 10);
  end;
  if (DataType <> SQL_TYPE_DATE) and
     (PInt64(@Hour)^ <> 0) and
     (Hour < 24) and
     (Minute < 60) and
     (Second < 60) then
  begin // we use 'T' as TTextWriter.AddDateTime
    TimeToIso8601PChar(Dest, true, Hour, Minute, Second, Fraction div 1000000,
      'T', WithMS);
    if WithMS then
    begin
      inc(Dest, 13);
      result := 25;
    end
    else
    begin
      inc(Dest, 9);
      result := 21;
    end;
  end
  else
    result := 12; // only date
  Dest^ := '"';
end;


{ ************ ODBC Library Loading }

const
  {$ifdef MSWINDOWS}
  ODBC_LIB = 'odbc32.dll';
  {$else}
  ODBC_LIB = 'libodbc.so.1';
  {$endif MSWINDOWS}

  ODBC_ENTRIES: array[0..66] of PChar = (
    'SQLAllocEnv', 'SQLAllocHandle', 'SQLAllocStmt',
    'SQLBindCol', 'SQLBindParameter', 'SQLCancel', 'SQLCloseCursor',
    'SQLColAttribute', 'SQLColAttributeW', 'SQLColumns',
    'SQLColumnsW', 'SQLStatistics', 'SQLStatisticsW', 'SQLConnect',
    'SQLConnectW', 'SQLCopyDesc', 'SQLDataSources', 'SQLDataSourcesW',
    'SQLDescribeCol', 'SQLDescribeColW', 'SQLDisconnect', 'SQLEndTran',
    'SQLError', 'SQLErrorW', 'SQLExecDirect', 'SQLExecDirectW', 'SQLExecute',
    'SQLFetch', 'SQLFetchScroll', 'SQLFreeConnect', 'SQLFreeEnv',
    'SQLFreeHandle', 'SQLFreeStmt', 'SQLGetConnectAttr', 'SQLGetConnectAttrW',
    'SQLGetCursorName', 'SQLGetCursorNameW', 'SQLGetData', 'SQLGetDescField',
    'SQLGetDescFieldW', 'SQLGetDescRec', 'SQLGetDescRecW', 'SQLGetDiagField',
    'SQLGetDiagFieldW', 'SQLGetDiagRec', 'SQLGetDiagRecW', 'SQLMoreResults',
    'SQLPrepare', 'SQLPrepareW', 'SQLRowCount', 'SQLNumResultCols', 'SQLGetInfo',
    'SQLGetInfoW', 'SQLSetStmtAttr', 'SQLSetStmtAttrW', 'SQLSetEnvAttr',
    'SQLSetConnectAttr', 'SQLSetConnectAttrW', 'SQLTables', 'SQLTablesW',
    'SQLForeignKeys', 'SQLForeignKeysW', 'SQLDriverConnect', 'SQLDriverConnectW',
    'SQLProcedureColumnsA', 'SQLProcedureColumnsW', 'SQLProcedures');


{$ifdef MSWINDOWS}
function ODBCInstalledDriversList(const aIncludeVersion: boolean;
  var aDrivers: TStrings): boolean;

  function GetFullFileVersion(const aFileName: TFileName): string;
  begin
    with TFileVersion.Create(aFileName, 0, 0, 0, 0) do
    try // five digits by section for easy version number comparison as string
      result := Format('%0.5d.%0.5d.%0.5d.%0.5d', [Major, Minor, Release, Build]);
    finally
      Free;
    end;
  end;

var
  I: PtrInt;
  lDriver: string;
begin
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    result := OpenKeyReadOnly('Software\ODBC\ODBCINST.INI\ODBC Drivers') or
      OpenKeyReadOnly('Software\ODBC\ODBCINST.INI');
    if result then
    begin
      if not Assigned(aDrivers) then
        aDrivers := TStringList.Create;
      GetValueNames(aDrivers);
      if aIncludeVersion then
        for I := 0 to aDrivers.Count - 1 do
        begin
          CloseKey;
          result := OpenKeyReadOnly('Software\ODBC\ODBCINST.INI\'  +  aDrivers[I]);
          if result then
          begin
            // expand environment variable, i.e %windir%
            lDriver := ExpandEnvVars(ReadString('Driver'));
            aDrivers[I] := aDrivers[I]  +  '='  +  GetFullFileVersion(lDriver);
          end;
        end;
    end;
  finally
    Free;
  end;
end;
{$else}
// TODO: ODBCInstalledDriversList for Linux
{$endif MSWINDOWS}


{ TODBCLib }

procedure TODBCLib.Check(Conn: TSQLDBConnection; Stmt: TSQLDBStatement; Status:
  SqlReturn; HandleType: SqlSmallint; Handle: SqlHandle;
  InfoRaiseException: boolean; LogLevelNoRaise: TSynLogInfo);
begin
  if Status <> SQL_SUCCESS then
    HandleError(Conn, Stmt, Status, HandleType, Handle, InfoRaiseException,
      LogLevelNoRaise);
end;

constructor TODBCLib.Create;
var
  P: PPointerArray;
  i: integer;
begin
  TryLoadLibrary([ODBC_LIB], EODBCException);
  P := @@AllocEnv;
  for i := 0 to High(ODBC_ENTRIES) do
    GetProc(ODBC_ENTRIES[i], @P[i], EODBCException); // raise EODBCException on error
end;

function TODBCLib.GetDiagField(StatementHandle: SqlHStmt): RawUTF8;
var
  Status: array[0..7] of AnsiChar;
  StringLength: SqlSmallint;
begin
  if ODBC.GetDiagFieldA(SQL_HANDLE_STMT, StatementHandle, 1, SQL_DIAG_SQLSTATE,
     @Status, SizeOf(Status), StringLength) = 0 then
    SetString(result, PAnsiChar(@Status), StringLength)
  else
    result := '';
end;

procedure TODBCLib.GetInfoString(ConnectionHandle: SqlHDbc;
  InfoType: SqlUSmallint; var Dest: RawUTF8);
var
  Len: SqlSmallint;
  Info: array[byte] of WideChar;
begin
  Len := 0;
  Check(nil, nil,
    GetInfoW(ConnectionHandle, InfoType, @Info, SizeOf(Info) shr 1, @Len),
    SQL_HANDLE_DBC, ConnectionHandle);
  Dest := RawUnicodeToUtf8(Info, Len shr 1);
end;

procedure TODBCLib.HandleError(Conn: TSQLDBConnection; Stmt: TSQLDBStatement;
  Status: SqlReturn; HandleType: SqlSmallint; Handle: SqlHandle;
  InfoRaiseException: boolean; LogLevelNoRaise: TSynLogInfo);
const
  FMT: PUTF8Char = '%[%] % (%)'#13#10;
var
  Sqlstate: array[0..6] of WideChar;
  MessageText: array[0..1023] of WideChar;
  RecNum, NativeError: SqlInteger;
  TextLength: SqlSmallint;
  msg: RawUTF8;
begin
  if (Handle = nil) or
     (Status = SQL_INVALID_HANDLE) then
    msg := 'Invalid handle'
  else
  begin
    RecNum := 1;
    while ODBC.GetDiagRecW(HandleType, Handle, RecNum, Sqlstate{%H-},
      NativeError, MessageText{%H-}, 1024, TextLength) and (not 1) = 0 do
    begin
      while (TextLength > 0) and
            (MessageText[TextLength - 1] < ' ') do
      begin
        dec(TextLength);
        MessageText[TextLength] := #0; // trim #13/#10 right of MessageText
      end;
      msg := FormatUTF8(FMT, [{%H-}msg, Sqlstate, MessageText, NativeError]);
      inc(RecNum);
    end;
    if msg = '' then
      msg := 'Unspecified error';
    if (Status = SQL_SUCCESS_WITH_INFO) and
       not InfoRaiseException then
    begin
      LogLevelNoRaise := sllInfo;
      if (Conn = nil) and
         (Stmt <> nil) then
        Conn := Stmt.Connection;
      if Conn <> nil then
        with Conn.Properties do
          if Assigned(OnStatementInfo) then
            OnStatementInfo(Stmt, msg);
    end;
  end;
  if LogLevelNoRaise <> sllNone then
    SynDBLog.Add.Log(LogLevelNoRaise, msg)
  else if Stmt = nil then
    raise EODBCException.CreateUTF8('% error: %', [self, msg])
  else
    raise EODBCException.CreateUTF8('% - % error: %', [Stmt, self, msg]);
end;


initialization

finalization
  FreeAndNil(ODBC);
end.

