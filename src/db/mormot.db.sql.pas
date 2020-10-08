/// Database Framework Shared Abstract SQL Types and Classes
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.sql;

{
  *****************************************************************************

   Shared Types and Definitions for SQL Database Access
    - SQL Fields and Columns Definitions
    - Define Database Engine Specific Behavior
    - General SQL Processing Functions
    - Abstract SQL DB Classes and Interfaces
    - Parent Classes for Thread-Safe and Parametrized Connections

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
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.perf,
  mormot.core.datetime,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.secure,
  mormot.core.rtti,
  mormot.core.log,
  mormot.db.core;

{.$define SYNDB_SILENCE}
// if defined, this unit won't log the statement execution to SynDBLog


{ ************ SQL Fields and Columns Definitions }

type
  /// an array of RawUTF8, for each existing column type
  // - used e.g. by SQLCreate method
  // - ftUnknown maps int32 field (e.g. boolean), ftNull maps RawUTF8 index # field,
  // ftUTF8 maps RawUTF8 blob field, other types map their default kind
  // - for UTF-8 text, ftUTF8 will define the BLOB field, whereas ftNull will
  // expect to be formated with an expected field length in ColumnAttr
  // - the RowID definition will expect the ORM to create an unique identifier,
  // and will use the ftInt64 type definition for this
  // and send it with the INSERT statement (some databases, like Oracle, do not
  // support standard's IDENTITY attribute) - see http://troels.arvin.dk/db/rdbms
  TSQLDBFieldTypeDefinition = array[TSQLDBFieldType] of RawUTF8;

  /// the diverse type of bound parameters during a statement execution
  // - will be paramIn by default, which is the case 90% of time
  // - could be set to paramOut or paramInOut if must be refereshed after
  // execution (for calling a stored procedure expecting such parameters)
  TSQLDBParamInOutType = (
    paramIn, paramOut, paramInOut);

  /// used to define a field/column layout in a table schema
  // - for TSQLDBConnectionProperties.SQLCreate to describe the new table
  // - for TSQLDBConnectionProperties.GetFields to retrieve the table layout
  TSQLDBColumnDefine = packed record
    /// the Column name
    ColumnName: RawUTF8;
    /// the Column type, as retrieved from the database provider
    // - returned as plain text by GetFields method, to be used e.g. by
    // TSQLDBConnectionProperties.GetFieldDefinitions method
    // - SQLCreate will check for this value to override the default type
    ColumnTypeNative: RawUTF8;
    /// the Column default width (in chars or bytes) of ftUTF8 or ftBlob
    // - can be set to value <0 for CLOB or BLOB column type, i.e. for
    // a value without any maximal length
    ColumnLength: PtrInt;
    /// the Column data precision
    // - used e.g. for numerical values
    ColumnPrecision: PtrInt;
    /// the Column data scale
    // - used e.g. for numerical values
    // - may be -1 if the metadata SQL statement returned NULL
    ColumnScale: PtrInt;
    /// the Column type, as recognized by our mormot.db.sql classes
    // - should not be ftUnknown nor ftNull
    ColumnType: TSQLDBFieldType;
    /// specify if column is indexed
    ColumnIndexed: boolean;
  end;

  /// used to define the column layout of a table schema
  // - e.g. for TSQLDBConnectionProperties.GetFields
  TSQLDBColumnDefineDynArray = array of TSQLDBColumnDefine;

  /// used to describe extended Index definition of a table schema
  TSQLDBIndexDefine = packed record
    /// name of the index
    IndexName: RawUTF8;
    /// description of the index type
    // - for MS SQL possible values are:
    // $ HEAP | CLUSTERED | NONCLUSTERED | XML |SPATIAL
    //  - for Oracle:
    // $ NORMAL | BITMAP | FUNCTION-BASED NORMAL | FUNCTION-BASED BITMAP | DOMAIN
    // see @http://docs.oracle.com/cd/B19306_01/server.102/b14237/statviews_1069.htm
    TypeDesc: RawUTF8;
    /// Expression for the subset of rows included in the filtered index
    // - only set for MS SQL - not retrieved for other DB types yet
    Filter: RawUTF8;
    /// comma separated list of indexed column names, in order of their definition
    KeyColumns: RawUTF8;
    /// comma separaded list of a nonkey column added to the index by using the CREATE INDEX INCLUDE clause
    // - only set for MS SQL - not retrieved for other DB types yet
    IncludedColumns: RawUTF8;
    /// if Index is unique
    IsUnique: boolean;
    /// if Index is part of a PRIMARY KEY constraint
    // - only set for MS SQL - not retrieved for other DB types yet
    IsPrimaryKey: boolean;
    /// if Index is part of a UNIQUE constraint
    // - only set for MS SQL - not retrieved for other DB types yet
    IsUniqueConstraint: boolean;
  end;

  /// used to describe extended Index definition of a table schema
  // - e.g. for TSQLDBConnectionProperties.GetIndexes
  TSQLDBIndexDefineDynArray = array of TSQLDBIndexDefine;

  /// used to define a parameter/column layout in a stored procedure schema
  // - for TSQLDBConnectionProperties.GetProcedureParameters to retrieve the stored procedure parameters
  // - can be extended according to https://msdn.microsoft.com/en-us/library/ms711701(v=vs.85).aspx
  TSQLDBProcColumnDefine = packed record
    /// the Column name
    ColumnName: RawUTF8;
    /// the Column type, as retrieved from the database provider
    // - used e.g. by TSQLDBConnectionProperties.GetProcedureParameters method
    ColumnTypeNative: RawUTF8;
    /// the Column default width (in chars or bytes) of ftUTF8 or ftBlob
    // - can be set to value <0 for CLOB or BLOB column type, i.e. for
    // a value without any maximal length
    ColumnLength: PtrInt;
    /// the Column data precision
    // - used e.g. for numerical values
    ColumnPrecision: PtrInt;
    /// the Column data scale
    // - used e.g. for numerical values
    // - may be -1 if the metadata SQL statement returned NULL
    ColumnScale: PtrInt;
    /// the Column type, as recognized by our mormot.db.sql classes
    // - should not be ftUnknown nor ftNull
    ColumnType: TSQLDBFieldType;
    /// defines the procedure column as a parameter or a result set column
    ColumnParamType: TSQLDBParamInOutType;
  end;

  /// used to define the parameter/column layout of a stored procedure schema
  // - e.g. for TSQLDBConnectionProperties.GetProcedureParameters
  TSQLDBProcColumnDefineDynArray = array of TSQLDBProcColumnDefine;

  /// possible column retrieval patterns
  // - used by TSQLDBColumnProperty.ColumnValueState
  TSQLDBStatementGetCol = (
    colNone, colNull, colWrongType, colDataFilled, colDataTruncated);

  /// used to define a field/column layout
  // - for TSQLDBConnectionProperties.SQLCreate to describe the table
  // - for T*Statement.Execute/Column*() methods to map the IRowSet content
  TSQLDBColumnProperty = packed record
    /// the Column name
    ColumnName: RawUTF8;
    /// a general purpose integer value
    // - for SQLCreate: default width (in WideChars or Bytes) of ftUTF8 or ftBlob;
    // if set to 0, a CLOB or BLOB column type will be created - note that
    // UTF-8 encoding is expected when calculating the maximum column byte size
    // for the CREATE TABLE statement (e.g. for Oracle 1333=4000/3 is used)
    // - for TOleDBStatement: the offset of this column in the IRowSet data,
    // starting with a DBSTATUSENUM, the data, then its length (for inlined
    // sftUTF8 and sftBlob only)
    // - for TSQLDBOracleStatement: contains an offset to this column values
    // inside fRowBuffer[] internal buffer
    // - for TSQLDBDatasetStatement: maps TField pointer value
    // - for TSQLDBPostgresStatement: contains the column type OID
    ColumnAttr: PtrUInt;
    /// the Column type, used for storage
    // - for SQLCreate: should not be ftUnknown nor ftNull
    // - for TOleDBStatement: should not be ftUnknown
    // - for mormot.db.sql.oracle: never ftUnknown, may be ftNull (for SQLT_RSET)
    ColumnType: TSQLDBFieldType;
    /// set if the Column must exists (i.e. should not be null)
    ColumnNonNullable: boolean;
    /// set if the Column shall have unique value (add the corresponding constraint)
    ColumnUnique: boolean;
    /// set if the Column data is inlined within the main rows buffer
    // - for TOleDBStatement: set if column was NOT defined as DBTYPE_BYREF
    // which is the most common case, when column data < 4 KB
    // - for TSQLDBOracleStatement: FALSE if column is an array of
    // POCILobLocator (SQLT_CLOB/SQLT_BLOB) or POCIStmt (SQLT_RSET)
    // - for TSQLDBODBCStatement: FALSE if bigger than 255 WideChar (ftUTF8) or
    // 255 bytes (ftBlob)
    ColumnValueInlined: boolean;
    /// expected column data size
    // - for TSQLDBOracleStatement/TOleDBStatement/TODBCStatement: used to store
    // one column size (in bytes)
    ColumnValueDBSize: cardinal;
    /// optional character set encoding for ftUTF8 columns
    // - for SQLT_STR/SQLT_CLOB (mormot.db.sql.oracle): equals to the OCI char set
    ColumnValueDBCharSet: integer;
    /// internal DB column data type
    // - for TSQLDBOracleStatement: used to store the DefineByPos() TypeCode,
    // can be SQLT_STR/SQLT_CLOB, SQLT_FLT, SQLT_INT, SQLT_DAT, SQLT_BLOB,
    // SQLT_BIN and SQLT_RSET
    // - for TSQLDBODBCStatement: used to store the DataType as returned
    // by ODBC.DescribeColW() - use private ODBC_TYPE_TO[ColumnType] to
    // retrieve the marshalled type used during column retrieval
    // - for TSQLDBFirebirdStatement: used to store XSQLVAR.sqltype
    // - for TSQLDBDatasetStatement: indicates the TField class type, i.e.
    // 0=TField, 1=TLargeIntField, 2=TWideStringField
    ColumnValueDBType: smallint;
    /// driver-specific encoding information
    // - for mormot.db.sql.oracle: used to store the ftUTF8 column encoding, i.e.
    // for SQLT_CLOB, equals either to SQLCS_NCHAR or SQLCS_IMPLICIT
    ColumnValueDBForm: byte;
    /// may contain the current status of the column value
    // - for mormot.db.sql.odbc: state of the latest SQLGetData() call
    ColumnDataState: TSQLDBStatementGetCol;
    /// may contain the current column size for not FIXEDLENGTH_SQLDBFIELDTYPE
    // - for mormot.db.sql.odbc: size (in bytes) in corresponding fColData[]
    // - TSQLDBProxyStatement: the actual maximum column size
    ColumnDataSize: integer;
  end;

  PSQLDBColumnProperty = ^TSQLDBColumnProperty;

  /// used to define a table/field column layout
  TSQLDBColumnPropertyDynArray = array of TSQLDBColumnProperty;

  /// used to define how a column to be created
  TSQLDBColumnCreate = record
    /// the data type
    // - here, ftUnknown is used for Int32 values, ftInt64 for Int64 values,
    // as expected by TSQLDBFieldTypeDefinition
    DBType: TSQLDBFieldType;
    /// the column name
    Name: RawUTF8;
    /// the width, e.g. for VARCHAR() types
    Width: cardinal;
    /// if the column should be unique
    Unique: boolean;
    /// if the column should be non null
    NonNullable: boolean;
    /// if the column is the ID primary key
    PrimaryKey: boolean;
  end;

  /// used to define how a table is to be created
  TSQLDBColumnCreateDynArray = array of TSQLDBColumnCreate;

  /// identify a CRUD mode of a statement
  // - in addition to CRUD states, cPostgreBulkArray would identify if the ORM
  // should generate unnested/any bound array statements - currently only
  // supported by mormot.db.sql.postgres for bulk insert/update/delete
  TSQLDBStatementCRUD = (
    cCreate, cRead, cUpdate, cDelete, cPostgreBulkArray);

  /// identify the CRUD modes of a statement
  // - used e.g. for batch send abilities of a DB engine
  TSQLDBStatementCRUDs = set of TSQLDBStatementCRUD;


const
  /// a magic constant used e.g. by TSQLDBStatement.FetchAllToBinary
  FETCHALLTOBINARY_MAGIC = 1;


{ ************ Define Database Engine Specific Behavior }

type
  /// the known database definitions
  // - will be used e.g. for TSQLDBConnectionProperties.SQLFieldCreate(), or
  // for OleDB/ODBC/ZDBC tuning according to the connected database engine
  TSQLDBDefinition = (
    dUnknown, dDefault, dOracle, dMSSQL, dJet, dMySQL, dSQLite,
    dFirebird, dNexusDB, dPostgreSQL, dDB2, dInformix);

  /// set of the available database definitions
  TSQLDBDefinitions = set of TSQLDBDefinition;

  /// where the LIMIT clause should be inserted for a given SQL syntax
  // - used by TSQLDBDefinitionLimitClause and SQLLimitClause() method
  TSQLDBDefinitionLimitPosition = (
    posNone, posWhere, posSelect, posAfter, posOuter);

  /// defines the LIMIT clause to be inserted for a given SQL syntax
  // - used by TSQLDBDefinitionLimitClause and SQLLimitClause() method
  TSQLDBDefinitionLimitClause = record
    Position: TSQLDBDefinitionLimitPosition;
    InsertFmt: PUTF8Char;
  end;

const
  /// the known column data types corresponding to our TSQLDBFieldType types
  // - will be used e.g. for TSQLDBConnectionProperties.SQLFieldCreate()
  // - see TSQLDBFieldTypeDefinition documentation to find out the mapping
  DB_FIELDS: array[TSQLDBDefinition] of TSQLDBFieldTypeDefinition = (
  // ftUnknown=int32, ftNull=UTF8, ftInt64, ftDouble, ftCurrency, ftDate, ftUTF8, ftBlob
  // dUnknown
    (' INT', ' NVARCHAR(%)', ' BIGINT', ' DOUBLE', ' NUMERIC(19,4)',
    ' TIMESTAMP', ' CLOB', ' BLOB'),

  // dDefault
    (' INT', ' NVARCHAR(%)', ' BIGINT', ' DOUBLE', ' NUMERIC(19,4)',
    ' TIMESTAMP', ' CLOB', ' BLOB'),

  // dOracle
    (' NUMBER(22,0)', ' NVARCHAR2(%)', ' NUMBER(22,0)', ' BINARY_DOUBLE',
    ' NUMBER(19,4)', ' DATE', ' NCLOB', ' BLOB'),
    // NCLOB (National Character Large Object) is an Oracle data type that can hold
    // up to 4 GB of character data. It's similar to a CLOB, but characters are
    // stored in a NLS or multibyte national character set (like NVARCHAR2)

  // dMSSQL
    (' int', ' nvarchar(%)', ' bigint', ' float', ' money', ' datetime',
    ' nvarchar(max)', ' varbinary(max)'),

  // dJet
    (' Long', ' VarChar(%)', ' Decimal(19,0)', ' Double', ' Currency',
    ' DateTime', ' LongText', ' LongBinary'),

  // dMySQL
    (' int', ' varchar(%) character set UTF8', ' bigint', ' double',
    ' decimal(19,4)', ' datetime', ' mediumtext character set UTF8', ' mediumblob'),

  // dSQLite
    (' INTEGER', ' TEXT', ' INTEGER', ' FLOAT', ' FLOAT', ' TEXT', ' TEXT', ' BLOB'),

  // dFirebird
    (' INTEGER', ' VARCHAR(%) CHARACTER SET UTF8', ' BIGINT', ' FLOAT',
    ' DECIMAL(18,4)', ' TIMESTAMP',
    ' BLOB SUB_TYPE 1 SEGMENT SIZE 2000 CHARACTER SET UTF8',
    ' BLOB SUB_TYPE 0 SEGMENT SIZE 2000'),
   // about BLOB: http://www.ibphoenix.com/resources/documents/general/doc_54

  // dNexusDB
    (' INTEGER', ' NVARCHAR(%)', ' LARGEINT', ' REAL', ' MONEY', ' DATETIME',
    ' NCLOB', ' BLOB'),
    // VARCHAR(%) CODEPAGE 65001 just did not work well with Delphi<2009

  // dPostgreSQL
    (' INTEGER', ' TEXT', ' BIGINT', ' DOUBLE PRECISION', ' NUMERIC(19,4)',
    ' TIMESTAMP', ' TEXT', ' BYTEA'),
    // like SQLite3, we will create TEXT column instead of VARCHAR(%), as stated
    // by http://www.postgresql.org/docs/current/static/datatype-character.html

  // dDB2 (for CCSID Unicode tables)
    (' int', ' varchar(%)', ' bigint', ' real', ' decimal(19,4)', ' timestamp',
    ' clob', ' blob'),
    { note: bigint needs 9.1 and up }

  // dInformix
    (' int', ' lvarchar(%)', ' bigint', ' smallfloat', ' decimal(19,4)',
    ' datetime year to fraction(3)', ' clob', ' blob'));

  /// the known column data types corresponding to our TSQLDBFieldType types
  // - will be used e.g. for TSQLDBConnectionProperties.SQLFieldCreate()
  // - SQLite3 doesn't expect any field length, neither PostgreSQL, so set to 0
  DB_FIELDSMAX: array[TSQLDBDefinition] of cardinal = (
    1000, 1000, 1333, { =4000/3 since WideChar is up to 3 bytes in UTF-8 }
    4000, 255, 4000, 0, 32760, 32767, 0, 32700, 32700);

  /// the known SQL statement to retrieve the server date and time
  DB_SERVERTIME: array[TSQLDBDefinition] of RawUTF8 = (
    '', '', // return local server time by default
    'select sysdate from dual', 'select GETDATE()', '', // Jet is local -> return local time
    'SELECT NOW()', '', // SQlite is local -> return local time
    'select current_timestamp from rdb$database', 'SELECT CURRENT_TIMESTAMP',
    'SELECT LOCALTIMESTAMP', 'select current timestamp from sysibm.sysdummy1',
    'select CURRENT YEAR TO FRACTION(3) from SYSTABLES where tabid = 1');

const
  /// the known SQL syntax to limit the number of returned rows in a SELECT
  // - Positon indicates if should be included within the WHERE clause,
  // at the beginning of the SQL statement, or at the end of the SQL statement
  // - InsertFmt will replace '%' with the maximum number of lines to be retrieved
  // - used by TSQLDBConnectionProperties.AdaptSQLLimitForEngineList()
  DB_SQLLIMITCLAUSE: array[TSQLDBDefinition] of TSQLDBDefinitionLimitClause = (
    ( // dUnknown
    Position: posNone;
    InsertFmt: nil
  ),
    ( // dDefault
    Position: posNone;
    InsertFmt: nil
  ),
    ( // dOracle
    Position: posWhere;
    InsertFmt: 'rownum<=%'
  ),
    ( // dMSSQL
    Position: posSelect;
    InsertFmt: 'top(%) '
  ),
    ( // dJet
    Position: posSelect;
    InsertFmt: 'top % '
  ),
    ( // dMySQL
    Position: posAfter;
    InsertFmt: ' limit %'
  ),
    ( // dSQLite
    Position: posAfter;
    InsertFmt: ' limit %'
  ),
    ( // dFireBird
    Position: posSelect;
    InsertFmt: 'first % '
  ),
    ( // dNexusDB
    Position: posSelect;
    InsertFmt: 'top % '
  ),
    ( // dPostgreSQL
    Position: posAfter;
    InsertFmt: ' limit %'
  ),
    ( // dDB2
    Position: posAfter;
    InsertFmt: ' fetch first % rows only'
  ),
    ( // dInformix
    Position: posAfter;
    InsertFmt: ' first % '
  ));

  /// the known database engines handling CREATE INDEX IF NOT EXISTS statement
  DB_HANDLECREATEINDEXIFNOTEXISTS = [dSQLite];

  /// the known database engines handling CREATE INDEX on BLOB columns
  // - SQLite3 does not have any issue about indexing any column
  // - PostgreSQL is able to index TEXT columns, which are some kind of CLOB
  DB_HANDLEINDEXONBLOBS = [dSQLite, dPostgreSQL];

  /// where the DESC clause shall be used for a CREATE INDEX statement
  // - only identified syntax exception is for FireBird
  DB_SQLDESENDINGINDEXPOS: array[TSQLDBDefinition] of (posWithColumn, posGlobalBefore) =
    (posWithColumn, posWithColumn, posWithColumn, posWithColumn, posWithColumn,
     posWithColumn, posWithColumn, posGlobalBefore, posWithColumn,
     posWithColumn, posWithColumn, posWithColumn);

  /// the SQL text corresponding to the identified WHERE operators for a SELECT
  DB_SQLOPERATOR: array[opEqualTo..opLike] of RawUTF8 = (
    '=', '<>', '<', '<=', '>', '>=',
    ' in ', ' is null', ' is not null', ' like ');


/// retrieve the text of a given Database SQL dialect enumeration
// - see also TSQLDBConnectionProperties.GetDBMSName() method
function ToText(DBMS: TSQLDBDefinition): PShortString; overload;


{ ************ General SQL Processing Functions }

/// function helper logging some column truncation information text
procedure LogTruncatedColumn(const Col: TSQLDBColumnProperty);

/// retrieve a table name without any left schema
// - e.g. TrimLeftSchema('SCHEMA.TABLENAME')='TABLENAME'
function TrimLeftSchema(const TableName: RawUTF8): RawUTF8;

/// replace all '?' in the SQL statement with named parameters like :AA :AB..
// - returns the number of ? parameters found within aSQL
// - won't generate any SQL keyword parameters (e.g. :AS :OF :BY), to be
// compliant with Oracle OCI expectations
// - any ending ';' character is deleted, unless aStripSemicolon is unset
function ReplaceParamsByNames(const aSQL: RawUTF8; var aNewSQL: RawUTF8;
  aStripSemicolon: boolean = true): integer;

/// replace all '?' in the SQL statement with indexed parameters like $1 $2 ...
// - returns the number of ? parameters found within aSQL
// - as used e.g. by PostgreSQL & Oracle (:1 :2) library
// - if AllowSemicolon is false (by default), reject any statement with ;
// (Postgres do not allow ; inside prepared statement); it should be
// true for Oracle
function ReplaceParamsByNumbers(const aSQL: RawUTF8; var aNewSQL: RawUTF8;
  IndexChar: AnsiChar = '$'; AllowSemicolon: boolean = false): integer;

/// create a JSON array from an array of UTF-8 bound values
// - as generated during array binding, i.e. with quoted strings
// 'one','t"wo' -> '{"one","t\"wo"}'   and  1,2,3 -> '{1,2,3}'
// - as used e.g. by PostgreSQL library
function BoundArrayToJSONArray(const Values: TRawUTF8DynArray): RawUTF8;


{ ************ Abstract SQL DB Classes and Interfaces }

var
  /// the TSynLog class used for logging for all our mormot.db.sql related units
  // - you may override it with TSQLLog, if available from mORMot.pas
  // - since not all exceptions are handled specificaly by this unit, you
  // may better use a common TSynLog class for the whole application or module
  SynDBLog: TSynLogClass = TSynLog;

type
  /// a custom variant type used to have direct access to a result row content
  // - use ISQLDBRows.RowData method to retrieve such a Variant
  TSQLDBRowVariantType = class(TSynInvokeableVariantType)
  protected
    function IntGet(var Dest: TVarData; const Instance: TVarData;
      Name: PAnsiChar; NameLen: PtrInt): boolean; override;
  end;

  {$M+}
  TSQLDBStatement = class;
  TSQLDBConnection = class;
  TSQLDBConnectionProperties = class;
  {$M-}

  /// generic Exception type, as used by mormot.db.sql and mormot.db.sql.* units
  ESQLDBException = class(ESynException)
  protected
    fStatement: TSQLDBStatement;
  public
    /// constructor which will use FormatUTF8() instead of Format()
    // - if the first Args[0] is a TSQLDBStatement class instance, the current
    // SQL statement will be part of the exception message
    constructor CreateUTF8(const Format: RawUTF8; const Args: array of const);
  published
    /// associated TSQLDBStatement instance, if supplied as first parameter
    property Statement: TSQLDBStatement read fStatement;
  end;

  /// generic interface to access a SQL query result rows
  // - not all TSQLDBStatement methods are available, but only those to retrieve
  // data from a statement result: the purpose of this interface is to make
  // easy access to result rows, not provide all available features - therefore
  // you only have access to the Step() and Column*() methods
  ISQLDBRows = interface
    ['{11291095-9C15-4984-9118-974F1926DB9F}']
    /// after a prepared statement has been prepared returning a ISQLDBRows
    // interface, this method must be called one or more times to evaluate it
    // - you shall call this method before calling any Column*() methods
    // - return TRUE on success, with data ready to be retrieved by Column*()
    // - return FALSE if no more row is available (e.g. if the SQL statement
    // is not a SELECT but an UPDATE or INSERT command)
    // - access the first or next row of data from the SQL Statement result:
    // if SeekFirst is TRUE, will put the cursor on the first row of results,
    // otherwise, it will fetch one row of data, to be called within a loop
    // - should raise an Exception on any error
    // - typical use may be:
    // ! var Customer: Variant;
    // ! begin
    // !   with Props.Execute( 'select * from Sales.Customer where AccountNumber like ?',
    // !       ['AW000001%'],@Customer) do begin
    // !     while Step do //  loop through all matching data rows
    // !       assert(Copy(Customer.AccountNumber,1,8)='AW000001');
    // !     ReleaseRows;
    // !   end;
    // ! end;
    function Step(SeekFirst: boolean = false): boolean;
    /// release cursor memory and resources once Step loop is finished
    // - this method call is optional, but is better be used if the ISQLDBRows
    // statement from taken from cache, and returned a lot of content which
    // may still be in client (and server) memory
    // - will also free all temporary memory used for optional logging
    procedure ReleaseRows;

    /// the column/field count of the current Row
    function ColumnCount: integer;
    /// the Column name of the current Row
    // - Columns numeration (i.e. Col value) starts with 0
    // - it's up to the implementation to ensure than all column names are unique
    function ColumnName(Col: integer): RawUTF8;
    /// returns the Column index of a given Column name
    // - Columns numeration (i.e. Col value) starts with 0
    // - returns -1 if the Column name is not found (via case insensitive search)
    function ColumnIndex(const aColumnName: RawUTF8): integer;
    /// the Column type of the current Row
    // - FieldSize can be set to store the size in chars of a ftUTF8 column
    // (0 means BLOB kind of TEXT column)
    function ColumnType(Col: integer; FieldSize: PInteger = nil): TSQLDBFieldType;
    /// returns TRUE if the column contains NULL
    function ColumnNull(Col: integer): boolean;
    /// return a Column integer value of the current Row, first Col is 0
    function ColumnInt(Col: integer): Int64; overload;
    /// return a Column floating point value of the current Row, first Col is 0
    function ColumnDouble(Col: integer): double; overload;
    /// return a Column floating point value of the current Row, first Col is 0
    function ColumnDateTime(Col: integer): TDateTime; overload;
    /// return a column date and time value of the current Row, first Col is 0
    function ColumnTimestamp(Col: integer): TTimeLog; overload;
    /// return a Column currency value of the current Row, first Col is 0
    function ColumnCurrency(Col: integer): currency; overload;
    /// return a Column UTF-8 encoded text value of the current Row, first Col is 0
    function ColumnUTF8(Col: integer): RawUTF8; overload;
    /// return a Column text value as generic VCL string of the current Row, first Col is 0
    function ColumnString(Col: integer): string; overload;
    /// return a Column as a blob value of the current Row, first Col is 0
    function ColumnBlob(Col: integer): RawByteString; overload;
    /// return a Column as a blob value of the current Row, first Col is 0
    function ColumnBlobBytes(Col: integer): TBytes; overload;
    /// read a blob Column into the Stream parameter
    procedure ColumnBlobToStream(Col: integer; Stream: TStream); overload;
    /// write a blob Column into the Stream parameter
    // - expected to be used with 'SELECT .. FOR UPDATE' locking statements
    procedure ColumnBlobFromStream(Col: integer; Stream: TStream); overload;
    /// return a Column as a TSQLVar value, first Col is 0
    // - the specified Temp variable will be used for temporary storage of
    // svtUTF8/svtBlob values
    procedure ColumnToSQLVar(Col: Integer; var Value: TSQLVar; var Temp: RawByteString);
    /// return a Column as a variant
    // - a ftUTF8 TEXT content will be mapped into a generic WideString variant
    // for pre-Unicode version of Delphi, and a generic UnicodeString (=string)
    // since Delphi 2009: you may not loose any data during charset conversion
    // - a ftBlob BLOB content will be mapped into a TBlobData AnsiString variant
    function ColumnVariant(Col: integer): Variant; overload;
    /// return a Column as a variant, first Col is 0
    // - this default implementation will call Column*() method above
    // - a ftUTF8 TEXT content will be mapped into a generic WideString variant
    // for pre-Unicode version of Delphi, and a generic UnicodeString (=string)
    // since Delphi 2009: you may not loose any data during charset conversion
    // - a ftBlob BLOB content will be mapped into a TBlobData AnsiString variant
    function ColumnToVariant(Col: integer; var Value: Variant): TSQLDBFieldType; overload;
    /// return a special CURSOR Column content as a mormot.db.sql result set
    // - Cursors are not handled internally by mORMot, but some databases (e.g.
    // Oracle) usually use such structures to get data from stored procedures
    // - such columns are mapped as ftNull internally - so this method is the only
    // one giving access to the data rows
    // - see also BoundCursor() if you want to access a CURSOR out parameter
    function ColumnCursor(Col: integer): ISQLDBRows; overload;

    /// return a Column integer value of the current Row, from a supplied column name
    function ColumnInt(const ColName: RawUTF8): Int64; overload;
    /// return a Column floating point value of the current Row, from a supplied column name
    function ColumnDouble(const ColName: RawUTF8): double; overload;
    /// return a Column floating point value of the current Row, from a supplied column name
    function ColumnDateTime(const ColName: RawUTF8): TDateTime; overload;
    /// return a column date and time value of the current Row, from a supplied column name
    function ColumnTimestamp(const ColName: RawUTF8): TTimeLog; overload;
    /// return a Column currency value of the current Row, from a supplied column name
    function ColumnCurrency(const ColName: RawUTF8): currency; overload;
    /// return a Column UTF-8 encoded text value of the current Row, from a supplied column name
    function ColumnUTF8(const ColName: RawUTF8): RawUTF8; overload;
    /// return a Column text value as generic VCL string of the current Row, from a supplied column name
    function ColumnString(const ColName: RawUTF8): string; overload;
    /// return a Column as a blob value of the current Row, from a supplied column name
    function ColumnBlob(const ColName: RawUTF8): RawByteString; overload;
    /// return a Column as a blob value of the current Row, from a supplied column name
    function ColumnBlobBytes(const ColName: RawUTF8): TBytes; overload;
    /// read a blob Column into the Stream parameter
    procedure ColumnBlobToStream(const ColName: RawUTF8; Stream: TStream); overload;
    /// write a blob Column into the Stream parameter
    procedure ColumnBlobFromStream(const ColName: RawUTF8; Stream: TStream); overload;
    /// return a Column as a variant, from a supplied column name
    function ColumnVariant(const ColName: RawUTF8): Variant; overload;
    /// return a Column as a variant, from a supplied column name
    // - since a property getter can't be an overloaded method, we define one
    // for the Column[] property
    function GetColumnVariant(const ColName: RawUTF8): Variant;
    /// return a special CURSOR Column content as a mormot.db.sql result set
    // - Cursors are not handled internally by mORMot, but some databases (e.g.
    // Oracle) usually use such structures to get data from strored procedures
    // - such columns are mapped as ftNull internally - so this method is the only
    // one giving access to the data rows
    function ColumnCursor(const ColName: RawUTF8): ISQLDBRows; overload;
    /// return a Column as a variant
    // - this default property can be used to write simple code like this:
    // ! procedure WriteFamily(const aName: RawUTF8);
    // ! var I: ISQLDBRows;
    // ! begin
    // !   I := MyConnProps.Execute('select * from table where name=?',[aName]);
    // !   while I.Step do
    // !     writeln(I['FirstName'],' ',DateToStr(I['BirthDate']));
    // !   I.ReleaseRows;
    // ! end;
    // - of course, using a variant and a column name will be a bit slower than
    // direct access via the Column*() dedicated methods, but resulting code
    // is fast in practice
    property Column[const ColName: RawUTF8]: Variant read GetColumnVariant; default;
    /// create a TSQLDBRowVariantType able to access any field content via late binding
    // - i.e. you can use Data.Name to access the 'Name' column of the current row
    // - this Variant will point to the corresponding TSQLDBStatement instance,
    // so it's not necessary to retrieve its value for each row; but once the
    // associated ISQLDBRows instance is released, you won't be able to access
    // its data - use RowDocVariant instead
    // - typical use is:
    // ! var Row: Variant;
    // ! (...)
    // !  with MyConnProps.Execute('select * from table where name=?',[aName]) do begin
    // !    Row := RowData;
    // !    while Step do
    // !      writeln(Row.FirstName,Row.BirthDate);
    // !    ReleaseRows;
    // !  end;
    function RowData: Variant;
    /// create a TDocVariant custom variant containing all columns values
    // - will create a "fast" TDocVariant object instance with all fields
    procedure RowDocVariant(out aDocument: variant;
      aOptions: TDocVariantOptions = JSON_OPTIONS_FAST);
    /// return the associated statement instance
    function Instance: TSQLDBStatement;
    // return all rows content as a JSON string
    // - JSON data is retrieved with UTF-8 encoding
    // - if Expanded is true, JSON data is an array of objects, for direct use
    // with any Ajax or .NET client:
    // & [ {"col1":val11,"col2":"val12"},{"col1":val21,... ]
    // - if Expanded is false, JSON data is serialized (used in TSQLTableJSON)
    // & { "FieldCount":1,"Values":["col1","col2",val11,"val12",val21,..] }
    // - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"'
    // format and contains true BLOB data
    // - if ReturnedRowCount points to an integer variable, it will be filled with
    // the number of row data returned (excluding field names)
    // - similar to corresponding TSQLRequest.Execute method in the
    // mormot.db.raw.sqlite3 unit
    function FetchAllAsJSON(Expanded: boolean; ReturnedRowCount: PPtrInt = nil): RawUTF8;
    // append all rows content as a JSON stream
    // - JSON data is added to the supplied TStream, with UTF-8 encoding
    // - if Expanded is true, JSON data is an array of objects, for direct use
    // with any Ajax or .NET client:
    // & [ {"col1":val11,"col2":"val12"},{"col1":val21,... ]
    // - if Expanded is false, JSON data is serialized (used in TSQLTableJSON)
    // & { "FieldCount":1,"Values":["col1","col2",val11,"val12",val21,..] }
    // - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"'
    // format and contains true BLOB data
    // - similar to corresponding TSQLRequest.Execute method in the
    // mormot.db.raw.sqlite3 unit
    // - returns the number of row data returned (excluding field names)
    function FetchAllToJSON(JSON: TStream; Expanded: boolean): PtrInt;
    /// append all rows content as binary stream
    // - will save the column types and name, then every data row in optimized
    // binary format (faster and smaller than JSON)
    // - you can specify a LIMIT for the data extent (default 0 meaning all data)
    // - generates the format expected by TSQLDBProxyStatement
    function FetchAllToBinary(Dest: TStream; MaxRowCount: cardinal = 0;
      DataRowPosition: PCardinalDynArray = nil): cardinal;
  end;

  /// generic interface to bind to prepared SQL query
  // - inherits from ISQLDBRows, so gives access to the result columns data
  // - not all TSQLDBStatement methods are available, but only those to bind
  // parameters and retrieve data after execution
  // - reference counting mechanism of this interface will feature statement
  // cache (if available) for NewThreadSafeStatementPrepared() or PrepareInlined()
  ISQLDBStatement = interface(ISQLDBRows)
    ['{EC27B81C-BD57-47D4-9711-ACFA27B583D7}']
    /// bind a NULL value to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - some providers (e.g. OleDB during MULTI INSERT statements) expect the
    // proper column type to be set in BoundType, even for NULL values
    procedure BindNull(Param: Integer; IO: TSQLDBParamInOutType = paramIn;
      BoundType: TSQLDBFieldType = ftNull);
    /// bind an integer value to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure Bind(Param: Integer; Value: Int64;
      IO: TSQLDBParamInOutType = paramIn); overload;
    /// bind a double value to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure Bind(Param: Integer; Value: double;
      IO: TSQLDBParamInOutType = paramIn); overload;
    /// bind a TDateTime value to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindDateTime(Param: Integer; Value: TDateTime;
      IO: TSQLDBParamInOutType = paramIn); overload;
    /// bind a currency value to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindCurrency(Param: Integer; Value: currency;
      IO: TSQLDBParamInOutType = paramIn); overload;
    /// bind a UTF-8 encoded string to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindTextU(Param: Integer; const Value: RawUTF8;
      IO: TSQLDBParamInOutType = paramIn); overload;
    /// bind a UTF-8 encoded buffer text (#0 ended) to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindTextP(Param: Integer; Value: PUTF8Char;
      IO: TSQLDBParamInOutType = paramIn); overload;
    /// bind a UTF-8 encoded string to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindTextS(Param: Integer; const Value: string;
      IO: TSQLDBParamInOutType = paramIn); overload;
    /// bind a UTF-8 encoded string to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindTextW(Param: Integer; const Value: WideString;
      IO: TSQLDBParamInOutType = paramIn); overload;
    /// bind a Blob buffer to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindBlob(Param: Integer; Data: pointer; Size: integer;
      IO: TSQLDBParamInOutType = paramIn); overload;
    /// bind a Blob buffer to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindBlob(Param: Integer; const Data: RawByteString;
      IO: TSQLDBParamInOutType = paramIn); overload;
    /// bind a Variant value to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - will call all virtual Bind*() methods from the Data type
    // - if DataIsBlob is TRUE, will call BindBlob(RawByteString(Data)) instead
    // of BindTextW(WideString(Variant)) - used e.g. by TQuery.AsBlob/AsBytes
    procedure BindVariant(Param: Integer; const Data: Variant;
      DataIsBlob: boolean; IO: TSQLDBParamInOutType = paramIn);
    /// bind one TSQLVar value
    // - the leftmost SQL parameter has an index of 1
    procedure Bind(Param: Integer; const Data: TSQLVar;
      IO: TSQLDBParamInOutType = paramIn); overload;
    /// bind one RawUTF8 encoded value
    // - the leftmost SQL parameter has an index of 1
    // - the value should match the BindArray() format, i.e. be stored as in SQL
    // (i.e. number, 'quoted string', 'YYYY-MM-DD hh:mm:ss', null)
    procedure Bind(Param: Integer; ParamType: TSQLDBFieldType; const Value: RawUTF8;
      ValueAlreadyUnquoted: boolean; IO: TSQLDBParamInOutType = paramIn); overload;
    /// bind an array of const values
    // - parameters marked as ? should be specified as method parameter in Params[]
    // - BLOB parameters can be bound with this method, when set after encoding
    // via BinToBase64WithMagic() call
    // - TDateTime parameters can be bound with this method, when encoded via
    // a DateToSQL() or DateTimeToSQL() call
    procedure Bind(const Params: array of const;
      IO: TSQLDBParamInOutType = paramIn); overload;
    /// bind an array of fields from an existing SQL statement
    // - can be used e.g. after ColumnsToSQLInsert() method call for fast data
    // conversion between tables
    procedure BindFromRows(const Fields: TSQLDBFieldTypeDynArray; Rows: TSQLDBStatement);
    /// bind a special CURSOR parameter to be returned as a mormot.db.sql result set
    // - Cursors are not handled internally by mORMot, but some databases (e.g.
    // Oracle) usually use such structures to get data from strored procedures
    // - such parameters are mapped as ftUnknown
    // - use BoundCursor() method to retrieve the corresponding ISQLDBRows after
    // execution of the statement
    procedure BindCursor(Param: integer);
    /// return a special CURSOR parameter content as a mormot.db.sql result set
    // - this method is not about a column, but a parameter defined with
    // BindCursor() before method execution
    // - Cursors are not handled internally by mORMot, but some databases (e.g.
    // Oracle) usually use such structures to get data from strored procedures
    // - this method allow direct access to the data rows after execution
    function BoundCursor(Param: Integer): ISQLDBRows;

    /// bind an array of values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - values are stored as in SQL (i.e. number, 'quoted string',
    // 'YYYY-MM-DD hh:mm:ss', null)
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    procedure BindArray(Param: Integer; ParamType: TSQLDBFieldType;
      const Values: TRawUTF8DynArray; ValuesCount: integer); overload;
    /// bind an array of integer values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    procedure BindArray(Param: Integer; const Values: array of Int64); overload;
    /// bind an array of double values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    procedure BindArray(Param: Integer; const Values: array of double); overload;
    /// bind an array of TDateTime values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - values are stored as in SQL (i.e. 'YYYY-MM-DD hh:mm:ss')
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    procedure BindArrayDateTime(Param: Integer; const Values: array of TDateTime);
    /// bind an array of currency values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    procedure BindArrayCurrency(Param: Integer; const Values: array of currency);
    /// bind an array of RawUTF8 values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - values are stored as in SQL (i.e. 'quoted string')
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    procedure BindArray(Param: Integer; const Values: array of RawUTF8); overload;

    /// retrieve the parameter content, after SQL execution
    // - the leftmost SQL parameter has an index of 1
    // - to be used e.g. with stored procedures:
    // ! query :=  'BEGIN TEST_PKG.DUMMY(?, ?, ?, ?, ?); END;';
    // ! stmt := Props.NewThreadSafeStatementPrepared(query, false);
    // ! stmt.Bind(1, in1, paramIn);
    // ! stmt.BindTextU(2, in2, paramIn);
    // ! stmt.BindTextU(3, in3, paramIn);
    // ! stmt.BindTextS(4, '', paramOut); //  to be retrieved with out1: string
    // ! stmt.Bind(5, 0, paramOut);       //  to be retrieved with out2: integer
    // ! stmt.ExecutePrepared;
    // ! stmt.ParamToVariant(4, out1, true);
    // ! stmt.ParamToVariant(5, out2, true);
    // - the parameter should have been bound with IO=paramOut or IO=paramInOut
    // if CheckIsOutParameter is TRUE
    function ParamToVariant(Param: Integer; var Value: Variant;
      CheckIsOutParameter: boolean = true): TSQLDBFieldType;

    /// execute a prepared SQL statement
    // - parameters marked as ? should have been already bound with Bind*() functions
    // - should raise an Exception on any error
    // - after execution, you can access any returned data via ISQLDBRows methods
    procedure ExecutePrepared;
    // execute a prepared SQL statement and return all rows content as a JSON string
    // - JSON data is retrieved with UTF-8 encoding
    // - if Expanded is true, JSON data is an array of objects, for direct use
    // with any Ajax or .NET client:
    // & [ {"col1":val11,"col2":"val12"},{"col1":val21,... ]
    // - if Expanded is false, JSON data is serialized (used in TSQLTableJSON)
    // & { "FieldCount":1,"Values":["col1","col2",val11,"val12",val21,..] }
    // - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"'
    // format and contains true BLOB data
    procedure ExecutePreparedAndFetchAllAsJSON(Expanded: boolean; out JSON: RawUTF8);
    function GetForceBlobAsNull: boolean;
    procedure SetForceBlobAsNull(value: boolean);
    /// if set, any BLOB field won't be retrieved, and forced to be null
    // - this may be used to speed up fetching the results for SQL requests
    // with * statements
    property ForceBlobAsNull: boolean read GetForceBlobAsNull write SetForceBlobAsNull;
    function GetForceDateWithMS: boolean;
    procedure SetForceDateWithMS(value: boolean);
    /// if set, any ftDate field will contain the milliseconds information
    // when serialized into ISO-8601 text
    // - this setting is private to each statement, since may vary depending
    // on data definition (e.g. ORM TDateTime/TDateTimeMS)
    property ForceDateWithMS: boolean read GetForceDateWithMS write SetForceDateWithMS;
    /// gets a number of updates made by latest executed statement
    function UpdateCount: Integer;
  end;

  /// possible events notified to TOnSQLDBProcess callback method
  // - event handler is specified by TSQLDBConnectionProperties.OnProcess or
  // TSQLDBConnection.OnProcess properties
  // - speConnected / speDisconnected will notify TSQLDBConnection.Connect
  // and TSQLDBConnection.Disconnect calls
  // - speNonActive / speActive will be used to notify external DB blocking
  // access, so can be used e.g. to change the mouse cursor shape (this trigger
  // is re-entrant, i.e. it will be executed only once in case of nested calls)
  // - speReconnected will be called if TSQLDBConnection did successfully
  // recover its database connection (on error, TQuery will call
  // speConnectionLost): this event will be called by TSQLDBConnection.Connect
  // after a regular speConnected notification
  // - speConnectionLost will be called by TQuery in case of broken connection,
  // and if Disconnect/Reconnect did not restore it as expected (i.e. speReconnected)
  // - speStartTransaction / speCommit / speRollback will notify the
  // corresponding TSQLDBConnection.StartTransaction, TSQLDBConnection.Commit
  // and TSQLDBConnection.Rollback methods
  TOnSQLDBProcessEvent = (
    speConnected, speDisconnected, speNonActive, speActive, speConnectionLost,
    speReconnected, speStartTransaction, speCommit, speRollback);

  /// event handler called during all external DB process
  // - event handler is specified by TSQLDBConnectionProperties.OnProcess or
  // TSQLDBConnection.OnProperties properties
  TOnSQLDBProcess = procedure(Sender: TSQLDBConnection; Event:
    TOnSQLDBProcessEvent) of object;

  /// event handler called when the low-level driver send some warning information
  // - errors will trigger Exceptions, but sometimes the database driver returns
  // some non critical information, which is logged and may be intercepted using
  // the TSQLDBConnectionProperties.OnStatementInfo property
  // - may be used e.g. to track ORA-28001 or ORA-28002 about account expire
  // - is currently implemented by mormot.db.sql.oracle, mormot.db.sql.odbc
  // and mormot.db.sql.oledb units
  TOnSQLDBInfo = procedure(Sender: TSQLDBStatement; const Msg: RawUTF8) of object;

  /// actions implemented by TSQLDBConnectionProperties.SharedTransaction()
  TSQLDBSharedTransactionAction = (
    transBegin, transCommitWithoutException, transCommitWithException,
    transRollback);

  /// defines a callback signature able to handle multiple INSERT
  // - may execute e.g. for 2 fields and 3 data rows on a database engine
  // implementing INSERT with multiple VALUES (like MySQL, PostgreSQL, NexusDB,
  // MSSQL or SQlite3), as implemented by
  // TSQLDBConnectionProperties.MultipleValuesInsert() :
  // $ INSERT INTO TableName(FieldNames[0],FieldNames[1]) VALUES
  // $   (FieldValues[0][0],FieldValues[1][0]),
  // $   (FieldValues[0][1],FieldValues[1][1]),
  // $   (FieldValues[0][2],FieldValues[1][2]);
  // - for other kind of DB which do not support multi values INSERT, may
  // execute a dedicated driver command, like MSSQL "bulk insert" or Firebird
  // "execute block"
  TOnBatchInsert = procedure(Props: TSQLDBConnectionProperties;
    const TableName: RawUTF8; const FieldNames: TRawUTF8DynArray;
    const FieldTypes: TSQLDBFieldTypeArray; RowCount: integer;
    const FieldValues: TRawUTF8DynArrayDynArray) of object;

  /// specify the class of TSQLDBConnectionProperties
  // - sometimes used to create connection properties instances, from a set
  // of available classes (see e.g. SynDBExplorer or sample 16)
  TSQLDBConnectionPropertiesClass = class of TSQLDBConnectionProperties;

  /// abstract class used to set Database-related properties
  // - handle e.g. the Database server location and connection parameters (like
  // UserID and password)
  // - should also provide some Database-specific generic SQL statement creation
  // (e.g. how to create a Table), to be used e.g. by the mORMot layer
  // - this class level will handle a single "main connection" - you may inherit
  // from TSQLDBConnectionThreadSafe to maintain one connection per thread
  TSQLDBConnectionProperties = class
  protected
    fServerName: RawUTF8;
    fDatabaseName: RawUTF8;
    fPassWord: RawUTF8;
    fUserID: RawUTF8;
    fForcedSchemaName: RawUTF8;
    fMainConnection: TSQLDBConnection;
    fBatchSendingAbilities: TSQLDBStatementCRUDs;
    fBatchMaxSentAtOnce: integer;
    fLoggedSQLMaxSize: integer;
    fOnBatchInsert: TOnBatchInsert;
    fDBMS: TSQLDBDefinition;
    fUseCache, fStoreVoidStringAsNull, fLogSQLStatementOnException,
      fRollbackOnDisconnect, fReconnectAfterConnectionError,
      fFilterTableViewSchemaName: boolean;
    fDateTimeFirstChar: AnsiChar;
    {$ifndef UNICODE}
    fVariantWideString: boolean;
    {$endif UNICODE}
    fForeignKeys: TSynNameValue;
    fSQLCreateField: TSQLDBFieldTypeDefinition;
    fSQLCreateFieldMax: cardinal;
    fSQLGetServerTimestamp: RawUTF8;
    fEngineName: RawUTF8;
    fOnProcess: TOnSQLDBProcess;
    fOnStatementInfo: TOnSQLDBInfo;
    fStatementCacheReplicates: integer;
    fConnectionTimeOutTicks: Int64;
    fSharedTransactions: array of record
      SessionID: cardinal;
      RefCount: integer;
      Connection: TSQLDBConnection;
    end;
    fExecuteWhenConnected: TRawUTF8DynArray;
    procedure SetConnectionTimeOutMinutes(minutes: cardinal);
    function GetConnectionTimeOutMinutes: cardinal;
    // this default implementation just returns the fDBMS value or dDefault
    // (never returns dUnknwown)
    function GetDBMS: TSQLDBDefinition; virtual;
    function GetDBMSName: RawUTF8; virtual;
    function GetForeignKeysData: RawByteString;
    procedure SetForeignKeysData(const Value: RawByteString);
    function FieldsFromList(const aFields: TSQLDBColumnDefineDynArray;
      aExcludeTypes: TSQLDBFieldTypes): RawUTF8;
    function GetMainConnection: TSQLDBConnection; virtual;
    function GetDatabaseNameSafe: RawUTF8; virtual;
    /// any overriden TSQLDBConnectionProperties class should call it in the
    // initialization section of its implementation unit to be recognized
    class procedure RegisterClassNameForDefinition;
    /// will be called at the end of constructor
    // - this default implementation will do nothing
    procedure SetInternalProperties; virtual;
    /// Assign schema name to owner from ForceSchemaName or UserID or Database name
    procedure SetSchemaNameToOwner(out Owner: RawUTF8); virtual;
    /// SQL statement to get all field/column names for a specified Table
    // - used by GetFieldDefinitions public method
    // - should return a SQL "SELECT" statement with the field names as first
    // column, a textual field type as 2nd column, then field length, then
    // numeric precision and scale as 3rd, 4th and 5th columns, and the index
    // count in 6th column
    // - this default implementation just returns nothing
    // - if this method is overridden, the ColumnTypeNativeToDB() method should
    // also be overridden in order to allow conversion from native column
    // type into the corresponding TSQLDBFieldType
    function SQLGetField(const aTableName: RawUTF8): RawUTF8; virtual;
    /// SQL statement to get advanced information about all indexes for a Table
    // - should return a SQL "SELECT" statement with the index names as first
    function SQLGetIndex(const aTableName: RawUTF8): RawUTF8; virtual;
    /// SQL statement to get all parameter for a specified Stored Procedure
    // - used by GetProcedureParameters public method
    // - should return a SQL "SELECT" statement with the parameter names as first
    // column, a textual field type as 2nd column, then parameter length as 3rd, then
    // parameter direction as 4th
    // - this default implementation just returns nothing
    // - if this method is overridden, the ColumnTypeNativeToDB() method should
    // also be overridden in order to allow conversion from native column
    // type into the corresponding TSQLDBFieldType
    function SQLGetParameter(const aProcName: RawUTF8): RawUTF8; virtual;
    /// SQL statement to get all stored procedure names for current connection
    // - used by GetProcedureNames public method
    // - should return a SQL "SELECT" statement with the procedure names as unique column
    // - this default implementation just returns nothing
    // - if this method is overridden, the ColumnTypeNativeToDB() method should
    // also be overridden in order to allow conversion from native column
    // type into the corresponding TSQLDBFieldType
    function SQLGetProcedure: RawUTF8; virtual;
    /// SQL statement to get all table names
    // - used by GetTableNames public method
    // - should return a SQL "SELECT" statement with the table names as
    // first column (any other columns will be ignored)
    // - this default implementation just returns nothing
    function SQLGetTableNames: RawUTF8; virtual;
    /// SQL statement to get all view names
    // - used by GetViewNames public method
    // - should return a SQL "SELECT" statement with the view names as
    // first column (any other columns will be ignored)
    // - this default implementation just returns nothing
    function SQLGetViewNames: RawUTF8; virtual;
    /// should initialize fForeignKeys content with all foreign keys of this
    // database
    // - used by GetForeignKey method
    procedure GetForeignKeys; virtual; abstract;
    /// will use fSQLCreateField[Max] to create the SQL column definition
    // - this default virtual implementation will handle properly all supported
    // database engines, assuming aField.ColumnType as in TSQLDBFieldTypeDefinition
    // - if the field is a primary key, aAddPrimaryKey may be modified to contain
    // some text to be appended at the end of the ALTER/CREATE TABLE statement
    function SQLFieldCreate(const aField: TSQLDBColumnCreate;
      var aAddPrimaryKey: RawUTF8): RawUTF8; virtual;
    /// wrapper around GetIndexes() + set Fields[].ColumnIndexed in consequence
    // - used by some overridden versions of GetFields() method
    procedure GetIndexesAndSetFieldsColumnIndexed(const aTableName: RawUTF8;
      var Fields: TSQLDBColumnDefineDynArray);
    /// check if the exception or its error message is about DB connection error
    // - will be used by TSQLDBConnection.LastErrorWasAboutConnection method
    // - default method will check for the 'conne' sub-string in the message text
    // - should be overridden depending on the error message returned by the DB
    function ExceptionIsAboutConnection(aClass: ExceptClass;
      const aMessage: RawUTF8): boolean; virtual;
    /// generic method able to implement OnBatchInsert() with parameters
    // - for MySQL, PostgreSQL, MSSQL2008, NexusDB or SQlite3, will execute
    // (with parameters) the extended standard syntax:
    // $ INSERT INTO TableName(FieldNames[0],FieldNames[1]) VALUES
    // $   (FieldValues[0][0],FieldValues[1][0]),
    // $   (FieldValues[0][1],FieldValues[1][1]),
    // $   (FieldValues[0][2],FieldValues[1][2]);
    // - for Firebird, will run the corresponding EXECUTE BLOCK() statement
    // with parameters - but Firebird sounds slower than without any parameter
    // (as tested with ZDBC/ZEOS or UniDAC)
    // - for Oracle, will run (with parameters for values):
    // $ INSERT ALL
    // $  INTO TableName(FieldNames[0],FieldNames[1]) VALUES (?,?)
    // $  INTO TableName(FieldNames[0],FieldNames[1]) VALUES (?,?)
    // $  INTO TableName(FieldNames[0],FieldNames[1]) VALUES (?,?)
    // $ SELECT 1 FROM DUAL;
    procedure MultipleValuesInsert(Props: TSQLDBConnectionProperties;
      const TableName: RawUTF8; const FieldNames: TRawUTF8DynArray;
      const FieldTypes: TSQLDBFieldTypeArray; RowCount: integer;
      const FieldValues: TRawUTF8DynArrayDynArray);
    /// Firebird-dedicated method able to implement OnBatchInsert()
    // - will run an EXECUTE BLOCK statement without any parameters, but
    // including inlined values - sounds to be faster on ZEOS/ZDBC!
    procedure MultipleValuesInsertFirebird(Props: TSQLDBConnectionProperties;
      const TableName: RawUTF8; const FieldNames: TRawUTF8DynArray;
      const FieldTypes: TSQLDBFieldTypeArray; RowCount: integer;
      const FieldValues: TRawUTF8DynArrayDynArray);
  public
    /// initialize the properties
    // - children may optionaly handle the fact that no UserID or Password
    // is supplied here, by displaying a corresponding Dialog box
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8); virtual;
    /// release related memory, and close MainConnection
    destructor Destroy; override;
    /// save the properties into a persistent storage object
    // - you can use TSQLDBConnectionPropertiesDescription.CreateFrom()
    // later on to instantiate the proper TSQLDBConnectionProperties class
    // - current Definition.Key value will be used for the password encryption
    procedure DefinitionTo(Definition: TSynConnectionDefinition); virtual;
    /// save the properties into a JSON file
    // - you could use TSQLDBConnectionPropertiesDescription.CreateFromJSON()
    // later on to instantiate the proper TSQLDBConnectionProperties class
    // - you can specify a custom Key, if the default is not enough for you
    function DefinitionToJSON(Key: cardinal = 0): RawUTF8; virtual;
    /// save the properties into a JSON file
    // - you could use TSQLDBConnectionPropertiesDescription.CreateFromFile()
    // later on to instantiate the proper TSQLDBConnectionProperties class
    // - you can specify a custom Key, if the default is not enough for you
    procedure DefinitionToFile(const aJSONFile: TFileName; Key: cardinal = 0);
    /// create a new TSQLDBConnectionProperties instance from the stored values
    class function CreateFrom(aDefinition: TSynConnectionDefinition): TSQLDBConnectionProperties; virtual;
    /// create a new TSQLDBConnectionProperties instance from a JSON content
    // - as previously serialized with TSQLDBConnectionProperties.DefinitionToJSON
    // - you can specify a custom Key, if the default is not safe enough for you
    class function CreateFromJSON(const aJSONDefinition: RawUTF8; aKey: cardinal = 0): TSQLDBConnectionProperties; virtual;
    /// create a new TSQLDBConnectionProperties instance from a JSON file
    // - as previously serialized with TSQLDBConnectionProperties.DefinitionToFile
    // - you can specify a custom Key, if the default is not safe enough for you
    class function CreateFromFile(const aJSONFile: TFileName; aKey: cardinal = 0):
      TSQLDBConnectionProperties;
    /// retrieve the registered class from the aDefinition.Kind string
    class function ClassFrom(aDefinition: TSynConnectionDefinition): TSQLDBConnectionPropertiesClass;

    /// create a new connection
    // - call this method if the shared MainConnection is not enough (e.g. for
    // multi-thread access)
    // - the caller is responsible of freeing this instance
    function NewConnection: TSQLDBConnection; virtual;
    /// get a thread-safe connection
    // - this default implementation will return the MainConnection shared
    // instance, so the provider should be thread-safe by itself
    // - TSQLDBConnectionPropertiesThreadSafe will implement a per-thread
    // connection pool, via an internal TSQLDBConnection pool, per thread
    // if necessary (e.g. for OleDB, which expect one TOleDBConnection instance
    // per thread)
    function ThreadSafeConnection: TSQLDBConnection; virtual;
    /// release all existing connections
    // - can be called e.g. after a DB connection problem, to purge the
    // connection pool, and allow automatic reconnection
    // - is called automatically if ConnectionTimeOutMinutes property is set
    // - warning: no connection shall still be used on the background (e.g. in
    // multi-threaded applications), or some unexpected border effects may occur
    procedure ClearConnectionPool; virtual;
    /// specify a maximum period of inactivity after which all connections will
    // be flushed and recreated, to avoid potential broken connections issues
    // - in practice, recreating the connections after a while is safe and
    // won't slow done the process - on the contrary, it may help reducing the
    // consumpted resources, and stabilize long running n-Tier servers
    // - ThreadSafeConnection method will check for the last activity on this
    // TSQLDBConnectionProperties instance, then call ClearConnectionPool
    // to release all active connections if the idle time elapsed was too long
    // - warning: no connection shall still be used on the background (e.g. in
    // multi-threaded applications), or some unexpected issues may occur - for
    // instance, ensure that your mORMot ORM server runs all its statements in
    // blocking mode for both read and write:
    // ! aServer.AcquireExecutionMode[execORMGet] := am***;
    // ! aServer.AcquireExecutionMode[execORMWrite] := am***;
    // here, safe blocking am*** modes are any mode but amUnlocked, i.e. either
    // amLocked, amBackgroundThread or amMainThread
    property ConnectionTimeOutMinutes: cardinal read GetConnectionTimeOutMinutes
      write SetConnectionTimeOutMinutes;
    /// intercept connection errors at statement preparation and try to reconnect
    // - i.e. detect TSQLDBConnection.LastErrorWasAboutConnection in
    // TSQLDBConnection.NewStatementPrepared
    // - warning: no connection shall still be used on the background (e.g. in
    // multi-threaded applications), or some unexpected issues may occur - see
    // AcquireExecutionMode[] recommendations in ConnectionTimeOutMinutes
    property ReconnectAfterConnectionError: boolean
      read fReconnectAfterConnectionError write fReconnectAfterConnectionError;
    /// create a new thread-safe statement
    // - this method will call ThreadSafeConnection.NewStatement
    function NewThreadSafeStatement: TSQLDBStatement;
    /// create a new thread-safe statement from an internal cache (if any)
    // - will call ThreadSafeConnection.NewStatementPrepared
    // - this method should return a prepared statement instance on success
    // - on error, returns nil and you can check Connnection.LastErrorMessage /
    // Connection.LastErrorException to retrieve corresponding error information
    // (if RaiseExceptionOnError is left to default FALSE value, otherwise, it will
    // raise an exception)
    function NewThreadSafeStatementPrepared(const aSQL: RawUTF8;
      ExpectResults: boolean; RaiseExceptionOnError: boolean = false): ISQLDBStatement; overload;
    /// create a new thread-safe statement from an internal cache (if any)
    // - this method will call the overloaded NewThreadSafeStatementPrepared method
    // - here Args[] array does not refer to bound parameters, but to values
    // to be changed within SQLFormat in place of '%' characters (this method
    // will call FormatUTF8() internaly); parameters will be bound directly
    // on the returned TSQLDBStatement instance
    // - this method should return a prepared statement instance on success
    // - on error, returns nil and you can check Connnection.LastErrorMessage /
    // Connection.LastErrorException to retrieve correspnding error information
    // (if RaiseExceptionOnError is left to default FALSE value, otherwise, it will
    // raise an exception)
    function NewThreadSafeStatementPrepared(const SQLFormat: RawUTF8;
      const Args: array of const; ExpectResults: boolean;
      RaiseExceptionOnError: boolean = false): ISQLDBStatement; overload;
    /// create, prepare and bound inlined parameters to a thread-safe statement
    // - this implementation will call the NewThreadSafeStatement virtual method,
    // then bound inlined parameters as :(1234): and return the resulting statement
    // - raise an exception on error
    // - consider using ExecuteInlined() for direct execution
    function PrepareInlined(const aSQL: RawUTF8; ExpectResults: boolean): ISQLDBStatement; overload;
    /// create, prepare and bound inlined parameters to a thread-safe statement
    // - overloaded method using FormatUTF8() and inlined parameters
    // - consider using ExecuteInlined() for direct execution
    function PrepareInlined(const SQLFormat: RawUTF8; const Args: array of const;
      ExpectResults: boolean): ISQLDBStatement; overload;
    /// execute a SQL query, returning a statement interface instance to retrieve
    // the result rows corresponding to the supplied SELECT statement
    // - will call NewThreadSafeStatement method to retrieve a thread-safe
    // statement instance, then run the corresponding Execute() method
    // - raise an exception on error
    // - returns an ISQLDBRows to access any resulting rows (if ExpectResults is
    // TRUE), and provide basic garbage collection, as such:
    // ! procedure WriteFamily(const aName: RawUTF8);
    // ! var I: ISQLDBRows;
    // ! begin
    // !   I := MyConnProps.Execute('select * from table where name=?',[aName]);
    // !   while I.Step do
    // !     writeln(I['FirstName'],' ',DateToStr(I['BirthDate']));
    // !   I.ReleaseRows;
    // ! end;
    // - if RowsVariant is set, you can use it to row column access via late
    // binding, as such:
    // ! procedure WriteFamily(const aName: RawUTF8);
    // ! var R: Variant;
    // ! begin
    // !   with MyConnProps.Execute('select * from table where name=?',[aName],@R) do begin
    // !     while Step do
    // !       writeln(R.FirstName,' ',DateToStr(R.BirthDate));
    // !     ReleaseRows;
    // !   end;
    // ! end;
    // - you can any BLOB field to be returned as null with the ForceBlobAsNull
    // optional parameter
    function Execute(const aSQL: RawUTF8; const Params: array of const;
      RowsVariant: PVariant = nil; ForceBlobAsNull: boolean = false): ISQLDBRows;
    /// execute a SQL query, without returning any rows
    // - can be used to launch INSERT, DELETE or UPDATE statement, e.g.
    // - will call NewThreadSafeStatement method to retrieve a thread-safe
    // statement instance, then run the corresponding Execute() method
    // - return the number of modified rows, i.e. the ISQLDBStatement.UpdateCount
    // value (or 0 if the DB driver does not supply this value)
    function ExecuteNoResult(const aSQL: RawUTF8; const Params: array of const): integer;
    /// create, prepare, bound inlined parameters and execute a thread-safe statement
    // - this implementation will call the NewThreadSafeStatement virtual method,
    // then bound inlined parameters as :(1234): and call its Execute method
    // - raise an exception on error
    function ExecuteInlined(const aSQL: RawUTF8; ExpectResults: boolean):
      ISQLDBRows; overload;
    /// create, prepare, bound inlined parameters and execute a thread-safe statement
    // - overloaded method using FormatUTF8() and inlined parameters
    function ExecuteInlined(const SQLFormat: RawUTF8; const Args: array of const;
      ExpectResults: boolean): ISQLDBRows; overload;
    /// handle a transaction process common to all associated connections
    // - could be used to share a single transaction among several connections,
    // or to run nested transactions even on DB engines which do not allow them
    // - will use a simple reference counting mechanism to allow nested
    // transactions, identified by a session identifier
    // - will fail if the same connection is not used for the whole process,
    // which would induce a potentially incorrect behavior
    // - returns the connection corresponding to the session, nil on error
    function SharedTransaction(SessionID: cardinal; action:
      TSQLDBSharedTransactionAction): TSQLDBConnection; virtual;

    /// convert a textual column data type, as retrieved e.g. from SQLGetField,
    // into our internal primitive types
    // - default implementation will always return ftUTF8
    function ColumnTypeNativeToDB(const aNativeType: RawUTF8; aScale: integer):
      TSQLDBFieldType; virtual;
    /// returns the SQL statement used to create a Table
    // - should return the SQL "CREATE" statement needed to create a table with
    // the specified field/column names and types
    // - if aAddID is TRUE, "ID Int64 PRIMARY KEY" column is added as first,
    // and will expect the ORM to create an unique RowID value sent at INSERT
    // (could use "select max(ID) from table" to retrieve the last value) -
    // note that 'ID' is used instead of 'RowID' since it fails on Oracle e.g.
    // - this default implementation will use internal fSQLCreateField and
    // fSQLCreateFieldMax protected values, which contains by default the
    // ANSI SQL Data Types and maximum 1000 inlined WideChars: inherited classes
    // may change the default fSQLCreateField* content or override this method
    function SQLCreate(const aTableName: RawUTF8; const aFields:
      TSQLDBColumnCreateDynArray; aAddID: boolean): RawUTF8; virtual;
    /// returns the SQL statement used to add a column to a Table
    // - should return the SQL "ALTER TABLE" statement needed to add a column to
    // an existing table
    // - this default implementation will use internal fSQLCreateField and
    // fSQLCreateFieldMax protected values, which contains by default the
    // ANSI SQL Data Types and maximum 1000 inlined WideChars: inherited classes
    // may change the default fSQLCreateField* content or override this method
    function SQLAddColumn(const aTableName: RawUTF8; const aField:
      TSQLDBColumnCreate): RawUTF8; virtual;
    /// returns the SQL statement used to add an index to a Table
    // - should return the SQL "CREATE INDEX" statement needed to add an index
    // to the specified column names of an existing table
    // - index will expect UNIQUE values in the specified columns, if Unique
    // parameter is set to true
    // - this default implementation will return the standard SQL statement, i.e.
    // 'CREATE [UNIQUE] INDEX index_name ON table_name (column_name[s])'
    function SQLAddIndex(const aTableName: RawUTF8; const aFieldNames: array of
      RawUTF8; aUnique: boolean; aDescending: boolean = false; const aIndexName:
      RawUTF8 = ''): RawUTF8; virtual;
    /// used to compute a SELECT statement for the given fields
    // - should return the SQL "SELECT ... FROM ..." statement to retrieve
    // the specified column names of an existing table
    // - by default, all columns specified in aFields[] will be available:
    // it will return "SELECT * FROM TableName"
    // - but if you specify a value in aExcludeTypes, it will compute the
    // matching column names to ignore those kind of content (e.g. [stBlob] to
    // save time and space)
    function SQLSelectAll(const aTableName: RawUTF8; const aFields:
      TSQLDBColumnDefineDynArray; aExcludeTypes: TSQLDBFieldTypes): RawUTF8; virtual;
    /// SQL statement to create the corresponding database
    // - this default implementation will only handle dFirebird by now
    function SQLCreateDatabase(const aDatabaseName: RawUTF8; aDefaultPageSize:
      integer = 0): RawUTF8; virtual;
    /// convert an ISO-8601 encoded time and date into a date appropriate to
    // be pasted in the SQL request
    // - this default implementation will return the quoted ISO-8601 value, i.e.
    // 'YYYY-MM-DDTHH:MM:SS' (as expected by Microsoft SQL server e.g.)
    // - returns  to_date('....','YYYY-MM-DD HH24:MI:SS')  for Oracle
    function SQLIso8601ToDate(const Iso8601: RawUTF8): RawUTF8; virtual;
    /// convert a TDateTime into a ISO-8601 encoded time and date, as expected
    // by the database provider
    // - e.g. SQLite3, DB2 and PostgreSQL will use non-standard ' ' instead of 'T'
    function SQLDateToIso8601Quoted(DateTime: TDateTime): RawUTF8; virtual;
    /// split a table name to its OWNER.TABLE full name (if applying)
    // - will use ForcedSchemaName property (if applying), or the OWNER. already
    // available within the supplied table name
    procedure SQLSplitTableName(const aTableName: RawUTF8; out Owner, Table:
      RawUTF8); virtual;
    /// split a procedure name to its OWNER.PACKAGE.PROCEDURE full name (if applying)
    // - will use ForcedSchemaName property (if applying), or the OWNER. already
    // available within the supplied table name
    procedure SQLSplitProcedureName(const aProcName: RawUTF8; out Owner, package,
      ProcName: RawUTF8); virtual;
    /// return the fully qualified SQL table name
    // - will use ForcedSchemaName property (if applying), or return aTableName
    // - you can override this method to force the expected format
    function SQLFullTableName(const aTableName: RawUTF8): RawUTF8; virtual;
    /// return a SQL table name with quotes if necessary
    // - can be used e.g. with SELECT statements
    // - you can override this method to force the expected format
    function SQLTableName(const aTableName: RawUTF8): RawUTF8; virtual;

    /// retrieve the column/field layout of a specified table
    // - this default implementation will use protected SQLGetField virtual
    // method to retrieve the field names and properties
    // - used e.g. by GetFieldDefinitions
    // - will call ColumnTypeNativeToDB protected virtual method to guess the
    // each mORMot TSQLDBFieldType
    procedure GetFields(const aTableName: RawUTF8; out Fields:
      TSQLDBColumnDefineDynArray); virtual;
    /// retrieve the advanced indexed information of a specified Table
    //  - this default implementation will use protected SQLGetIndex virtual
    // method to retrieve the index names and properties
    // - currently only MS SQL and Oracle are supported
    procedure GetIndexes(const aTableName: RawUTF8; out Indexes:
      TSQLDBIndexDefineDynArray); virtual;
    /// get all field/column definition for a specified Table as text
    // - call the GetFields method and retrieve the column field name and
    // type as 'Name [Type Length Precision Scale]'
    // - if WithForeignKeys is set, will add external foreign keys as '% tablename'
    procedure GetFieldDefinitions(const aTableName: RawUTF8; out Fields:
      TRawUTF8DynArray; WithForeignKeys: boolean);
    /// get one field/column definition as text
    // - return column type as 'Name [Type Length Precision Scale]'
    class function GetFieldDefinition(const Column: TSQLDBColumnDefine): RawUTF8;
    /// get one field/column definition as text, targeting a TSQLRecord
    // published property
    // - return e.g. property type information as:
    // ! 'Name: RawUTF8 read fName write fName index 20;';
    class function GetFieldORMDefinition(const Column: TSQLDBColumnDefine): RawUTF8;
    /// check if the supplied text word is not a keyword for a given database engine
    class function IsSQLKeyword(aDB: TSQLDBDefinition; aWord: RawUTF8): boolean;
      overload; virtual;
    /// check if the supplied text word is not a keyword for the current database engine
    // - just a wrapper around the overloaded class function
    function IsSQLKeyword(aWord: RawUTF8): boolean; overload;
    /// retrieve a list of stored procedure names from current connection
    procedure GetProcedureNames(out Procedures: TRawUTF8DynArray); virtual;
    /// retrieve procedure input/output parameter information
    // - aProcName: stored procedure name to retrieve parameter infomation.
    // - Parameters: parameter list info (name, datatype, direction, default)
    procedure GetProcedureParameters(const aProcName: RawUTF8; out Parameters:
      TSQLDBProcColumnDefineDynArray); virtual;
    /// get all table names
    // - this default implementation will use protected SQLGetTableNames virtual
    // method to retrieve the table names
    procedure GetTableNames(out Tables: TRawUTF8DynArray); virtual;
    /// get all view names
    // - this default implementation will use protected SQLGetViewNames virtual
    // method to retrieve the view names
    procedure GetViewNames(out Views: TRawUTF8DynArray); virtual;
    /// retrieve a foreign key for a specified table and column
    // - first time it is called, it will retrieve all foreign keys from the
    // remote database using virtual protected GetForeignKeys method into
    // the protected fForeignKeys list: this may be slow, depending on the
    // database access (more than 10 seconds waiting is possible)
    // - any further call will use this internal list, so response will be
    // immediate
    // - the whole foreign key list is shared by all connections
    function GetForeignKey(const aTableName, aColumnName: RawUTF8): RawUTF8;

    /// returns the information to adapt the LIMIT # clause in the SQL SELECT
    // statement to a syntax matching the underlying DBMS
    // - e.g. TSQLRestStorageExternal.AdaptSQLForEngineList() calls this
    // to let TSQLRestServer.URI by-pass virtual table mechanism
    function SQLLimitClause(AStmt: TSynTableStatement):
      TSQLDBDefinitionLimitClause; virtual;
    /// determine if the SQL statement can be cached
    // - used by TSQLDBConnection.NewStatementPrepared() for handling cache
    function IsCachable(P: PUTF8Char): boolean; virtual;
    /// return the database engine name, as computed from the class name
    // - 'TSQLDBConnectionProperties' will be trimmed left side of the class name
    class function EngineName: RawUTF8;

    /// return a shared connection, corresponding to the given database
    // - call the ThreadSafeConnection method instead e.g. for multi-thread
    // access, or NewThreadSafeStatement for direct retrieval of a new statement
    property MainConnection: TSQLDBConnection read GetMainConnection;
    /// the associated User Password, as specified at creation
    // - not published, for security reasons (may be serialized otherwise)
    property PassWord: RawUTF8 read fPassWord;
    /// the associated database name, as specified at creation
    // - not published, for security reasons (may be serialized otherwise)
    // - DatabaseNameSafe will be published, and delete any matching
    // PasswordValue in DatabaseName
    property DatabaseName: RawUTF8 read fDatabaseName;
    /// can be used to store the fForeignKeys[] data in an external BLOB
    // - since GetForeignKeys can be (somewhat) slow, could save a lot of time
    property ForeignKeysData: RawByteString read GetForeignKeysData write
      SetForeignKeysData;
    /// this event handler will be called during all process
    // - can be used e.g. to change the desktop cursor, or be notified
    // on connection/disconnection/reconnection
    // - you can override this property directly in the TSQLDBConnection
    property OnProcess: TOnSQLDBProcess read fOnProcess write fOnProcess;
    /// this event handler will be called when statements trigger some low-level
    // information
    property OnStatementInfo: TOnSQLDBInfo read fOnStatementInfo write fOnStatementInfo;
    /// you can define a callback method able to handle multiple INSERT
    // - may execute e.g. INSERT with multiple VALUES (like MySQL, MSSQL, NexusDB,
    // PostgreSQL or SQlite3), as defined by MultipleValuesInsert() callback
    property OnBatchInsert: TOnBatchInsert read fOnBatchInsert write fOnBatchInsert;
  published { to be logged as JSON - no UserID nor Password for security :) }
    /// return the database engine name, as computed from the class name
    // - 'TSQLDBConnectionProperties' will be trimmed left side of the class name
    property Engine: RawUTF8 read fEngineName;
    /// the associated server name, as specified at creation
    property ServerName: RawUTF8 read fServerName;
    /// the associated database name, safely trimmed from the password
    // - would replace any matching Password value content from DatabaseName
    // by '***' for security reasons, e.g. before serialization
    property DatabaseNameSafe: RawUTF8 read GetDatabaseNameSafe;
    /// the associated User Identifier, as specified at creation
    property UserID: RawUTF8 read fUserID;
    /// the remote DBMS type, as stated by the inheriting class itself, or
    //  retrieved at connecton time (e.g. for ODBC)
    property DBMS: TSQLDBDefinition read GetDBMS;
    /// the remote DBMS type name, retrieved as text from the DBMS property
    property DBMSEngineName: RawUTF8 read GetDBMSName;
    /// the abilities of the database for batch sending
    // - e.g. Oracle will handle array DML binds, or MS SQL bulk insert
    property BatchSendingAbilities: TSQLDBStatementCRUDs read fBatchSendingAbilities;
    /// the maximum number of rows to be transmitted at once for batch sending
    // - e.g. Oracle handles array DML operation with iters <= 32767 at best
    // - if OnBatchInsert points to MultipleValuesInsert(), this value is
    // ignored, and the maximum number of parameters is guessed per DBMS type
    property BatchMaxSentAtOnce: integer read fBatchMaxSentAtOnce write
      fBatchMaxSentAtOnce;
    /// the maximum size, in bytes, of logged SQL statements
    // - setting 0 will log statement and parameters with no size limit
    // - setting -1 will log statement without any parameter value (just ?)
    // - setting any value >0 will log statement and parameters up to the
    // number of bytes (default set to 2048 to log up to 2KB per statement)
    property LoggedSQLMaxSize: integer read fLoggedSQLMaxSize write fLoggedSQLMaxSize;
    /// allow to log the SQL statement when any low-level ESQLDBException is raised
    property LogSQLStatementOnException: boolean read
      fLogSQLStatementOnException write fLogSQLStatementOnException;
    /// an optional Schema name to be used for SQLGetField() instead of UserID
    // - by default, UserID will be used as schema name, if none is specified
    // (i.e. if table name is not set as SCHEMA.TABLE)
    // - depending on the DBMS identified, the class may also set automatically
    // the default 'dbo' for MS SQL or 'public' for PostgreSQL
    // - you can set a custom schema to be used instead
    property ForcedSchemaName: RawUTF8 read fForcedSchemaName write fForcedSchemaName;
    /// if GetTableNames/GetViewNames should only return the table names
    // starting with 'ForcedSchemaName.' prefix
    property FilterTableViewSchemaName: boolean read fFilterTableViewSchemaName
      write fFilterTableViewSchemaName;
    /// TRUE if an internal cache of SQL statement should be used
    // - cache will be accessed for NewStatementPrepared() method only, by
    // returning ISQLDBStatement interface instances
    // - default value is TRUE for faster process (e.g. TTestSQLite3ExternalDB
    // regression tests will be two times faster with statement caching)
    // - will cache only statements containing ? parameters or a SELECT with no
    // WHERE clause within
    property UseCache: boolean read fUseCache write fUseCache;
    /// if UseCache is true, how many statement replicates can be generated
    // if the cached ISQLDBStatement is already used
    // - such replication is normally not needed in a per-thread connection,
    // unless ISQLDBStatement are not released as soon as possible
    // - above this limit, no cache will be made, and a dedicated single-time
    // statement will be prepared
    // - default is 0 to cache statements once - but you may try to increase
    // this value if you run identical SQL with long-standing ISQLDBStatement;
    // or you can set -1 if you don't want the warning log to appear
    property StatementCacheReplicates: integer read fStatementCacheReplicates
      write fStatementCacheReplicates;
    /// defines if TSQLDBConnection.Disconnect shall Rollback any pending
    // transaction
    // - some engines executes a COMMIT when the client is disconnected, others
    // do raise an exception: this parameter ensures that any pending transaction
    // is roll-backed before disconnection
    // - is set to TRUE by default
    property RollbackOnDisconnect: boolean read fRollbackOnDisconnect write
      fRollbackOnDisconnect;
    /// defines if '' string values are to be stored as SQL null
    // - by default, '' will be stored as ''
    // - but some DB engines (e.g. Jet or MS SQL) does not allow by default to
    // store '' values, but expect NULL to be stored instead
    property StoreVoidStringAsNull: boolean read fStoreVoidStringAsNull write
      fStoreVoidStringAsNull;
    /// customize the ISO-8601 text format expected by the database provider
    // - is 'T' by default, as expected by the ISO-8601 standard
    // - will be changed e.g. for PostgreSQL, which expects ' ' instead
    // - as used by SQLDateToIso8601Quoted() and BindArray()
    property DateTimeFirstChar: AnsiChar read fDateTimeFirstChar write
      fDateTimeFirstChar;
    {$ifndef UNICODE}
    /// set to true to force all variant conversion to WideString instead of
    // the default faster AnsiString, for pre-Unicode version of Delphi
    // - by default, the conversion to Variant will create an AnsiString kind
    // of variant: for pre-Unicode Delphi, avoiding WideString/OleStr content
    // will speed up the process a lot, if you are sure that the current
    // charset matches the expected one (which is very likely)
    // - set this property to TRUE so that the conversion to Variant will
    // create a WideString kind of variant, to avoid any character data loss:
    // the access to the property will be slower, but you won't have any
    // potential data loss
    // - starting with Delphi 2009, the TEXT content will be stored as an
    // UnicodeString in the variant, so this property is not necessary
    // - the Variant conversion is mostly used for the TQuery wrapper, or for
    // the ISQLDBRows.Column[] property or ISQLDBRows.ColumnVariant() method;
    // this won't affect other Column*() methods, or JSON production
    property VariantStringAsWideString: boolean read fVariantWideString write
      fVariantWideString;
    {$endif}
    /// SQL statements what will be executed for each new connection
    // usage scenarios examples:
    // - Oracle: force case-insensitive like
    // $  ['ALTER SESSION SET NLS_COMP=LINGUISTIC', 'ALTER SESSION SET NLS_SORT=BINARY_CI']
    //  - Postgres: disable notices and warnings
    // $  ['SET client_min_messages to ERROR']
    // - SQLite3: turn foreign keys ON
    // $  ['PRAGMA foreign_keys = ON']
    property ExecuteWhenConnected: TRawUTF8DynArray read fExecuteWhenConnected
      write fExecuteWhenConnected;
  end;

  /// abstract connection created from TSQLDBConnectionProperties
  // - more than one TSQLDBConnection instance can be run for the same
  // TSQLDBConnectionProperties
  TSQLDBConnection = class
  protected
    fProperties: TSQLDBConnectionProperties;
    fErrorException: ExceptClass;
    fErrorMessage: RawUTF8;
    fTransactionCount: integer;
    fServerTimestampOffset: TDateTime;
    fServerTimestampAtConnection: TDateTime;
    fCache: TRawUTF8List;
    fOnProcess: TOnSQLDBProcess;
    fTotalConnectionCount: integer;
    fInternalProcessActive: integer;
    fRollbackOnDisconnect: boolean;
    fLastAccessTicks: Int64;
    function IsOutdated(tix: Int64): boolean; // do not make virtual
    function GetInTransaction: boolean; virtual;
    function GetServerTimestamp: TTimeLog;
    function GetServerDateTime: TDateTime; virtual;
    function GetLastErrorWasAboutConnection: boolean;
    /// raise an exception if IsConnected returns false
    procedure CheckConnection;
    /// call OnProcess() call back event, if needed
    procedure InternalProcess(Event: TOnSQLDBProcessEvent);
  public
    /// connect to a specified database engine
    constructor Create(aProperties: TSQLDBConnectionProperties); virtual;
    /// release memory and connection
    destructor Destroy; override;

    /// connect to the specified database
    // - should raise an Exception on error
    // - this default implementation will notify OnProgress callback for
    // sucessfull re-connection: it should be called in overridden methods
    // AFTER actual connection process
    procedure Connect; virtual;
    /// stop connection to the specified database
    // - should raise an Exception on error
    // - this default implementation will release all cached statements: so it
    // should be called in overridden methods BEFORE actual disconnection
    procedure Disconnect; virtual;
    /// return TRUE if Connect has been already successfully called
    function IsConnected: boolean; virtual; abstract;
    /// initialize a new SQL query statement for the given connection
    // - the caller should free the instance after use
    function NewStatement: TSQLDBStatement; virtual; abstract;
    /// initialize a new SQL query statement for the given connection
    // - this default implementation will call the NewStatement method, and
    // implement handle statement caching is UseCache=true - in this case,
    // the TSQLDBStatement.Reset method shall have been overridden to allow
    // binding and execution of the very same prepared statement
    // - the same aSQL can cache up to 9 statements in this TSQLDBConnection
    // - this method should return a prepared statement instance on success
    // - on error, if RaiseExceptionOnError=false (by default), it returns nil
    // and you can check LastErrorMessage and LastErrorException properties to
    // retrieve corresponding error information
    // - if TSQLDBConnectionProperties.ReconnectAfterConnectionError is set,
    // any connection error will be trapped, unless AllowReconnect is false
    // - on error, if RaiseExceptionOnError=true, an exception is raised
    function NewStatementPrepared(const aSQL: RawUTF8; ExpectResults: boolean;
      RaiseExceptionOnError: boolean = false;
      AllowReconnect: boolean = true): ISQLDBStatement; virtual;
    /// begin a Transaction for this connection
    // - this default implementation will check and set TransactionCount
    procedure StartTransaction; virtual;
    /// commit changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    // - this default implementation will check and set TransactionCount
    procedure Commit; virtual;
    /// discard changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    // - this default implementation will check and set TransactionCount
    procedure Rollback; virtual;
    /// allows to change the password of the current connected user
    // - do nothing method by default, returning false
    // - properly overriden e.g. by TSQLDBOracleConnection
    function PasswordChange: boolean; virtual;

    /// direct export of a DB statement rows into a new table of this database
    // - the corresponding table will be created within the current connection,
    // if it does not exist
    // - if the column types are not set, they will be identified from the
    // first row of data
    // - INSERTs will be nested within a transaction if WithinTransaction is TRUE
    // - will raise an Exception in case of error
    function NewTableFromRows(const TableName: RawUTF8; Rows: TSQLDBStatement;
      WithinTransaction: boolean; ColumnForcedTypes: TSQLDBFieldTypeDynArray = nil): integer;

    /// the current Date and Time, as retrieved from the server
    // - note that this value is the DB_SERVERTIME[] constant SQL value, so
    // will most likely return a local time, not an UTC time
    // - this property will return the timestamp in TTimeLog / TTimeLogBits /
    // Int64 value
    property ServerTimestamp: TTimeLog read GetServerTimestamp;
    /// the current Date and Time, as retrieved from the server
    // - note that this value is the DB_SERVERTIME[] constant SQL value, so
    // will most likely return a local time, not an UTC time
    // - this property will return the value as regular TDateTime
    property ServerDateTime: TDateTime read GetServerDateTime;
    /// this event handler will be called during all process
    // - can be used e.g. to change the desktop cursor
    // - by default, will follow TSQLDBConnectionProperties.OnProcess property
    property OnProcess: TOnSQLDBProcess read fOnProcess write fOnProcess;
  published { to be logged as JSON }
    /// returns TRUE if the connection was set
    property Connected: boolean read IsConnected;
    /// the time returned by the server when the connection occurred
    property ServerTimestampAtConnection: TDateTime read fServerTimestampAtConnection;
    /// number of sucessfull connections for this instance
    // - can be greater than 1 in case of re-connection via Disconnect/Connect
    property TotalConnectionCount: integer read fTotalConnectionCount;
    /// number of nested StartTransaction calls
    // - equals 0 if no transaction is active
    property TransactionCount: integer read fTransactionCount;
    /// TRUE if StartTransaction has been called
    // - check if TransactionCount>0
    property InTransaction: boolean read GetInTransaction;
    /// defines if Disconnect shall Rollback any pending transaction
    // - some engines executes a COMMIT when the client is disconnected, others
    // do raise an exception: this parameter ensures that any pending transaction
    // is roll-backed before disconnection
    // - is set to TRUE by default
    property RollbackOnDisconnect: boolean read fRollbackOnDisconnect write
      fRollbackOnDisconnect;
    /// some error message, e.g. during execution of NewStatementPrepared
    property LastErrorMessage: RawUTF8 read fErrorMessage write fErrorMessage;
    /// some error exception, e.g. during execution of NewStatementPrepared
    property LastErrorException: ExceptClass read fErrorException;
    /// TRUE if last error is a broken connection, e.g. during execution of
    // NewStatementPrepared
    // - i.e. LastErrorException/LastErrorMessage concerns the database connection
    // - will use TSQLDBConnectionProperties.ExceptionIsAboutConnection virtual method
    property LastErrorWasAboutConnection: boolean read GetLastErrorWasAboutConnection;
    /// the associated database properties
    property Properties: TSQLDBConnectionProperties read fProperties;
  end;

  /// generic abstract class to implement a prepared SQL query
  // - inherited classes should implement the DB-specific connection in its
  // overridden methods, especially Bind*(), Prepare(), ExecutePrepared, Step()
  // and Column*() methods
  TSQLDBStatement = class(TInterfacedObject, ISQLDBRows, ISQLDBStatement)
  protected
    fStripSemicolon: boolean;
    fConnection: TSQLDBConnection;
    fSQL: RawUTF8;
    fExpectResults: boolean;
    fParamCount: integer;
    fColumnCount: integer;
    fTotalRowsRetrieved: Integer;
    fCurrentRow: Integer;
    fForceBlobAsNull: boolean;
    fForceDateWithMS: boolean;
    fDBMS: TSQLDBDefinition;
    {$ifndef SYNDB_SILENCE}
    fSQLLogLog: TSynLog;
    fSQLLogLevel: TSynLogInfo;
    {$endif SYNDB_SILENCE}
    fSQLWithInlinedParams: RawUTF8;
    fSQLLogTimer: TPrecisionTimer;
    fCacheIndex: integer;
    fSQLPrepared: RawUTF8;
    function GetSQLCurrent: RawUTF8;
    function GetSQLWithInlinedParams: RawUTF8;
    procedure ComputeSQLWithInlinedParams;
    function GetForceBlobAsNull: boolean;
    procedure SetForceBlobAsNull(value: boolean);
    function GetForceDateWithMS: boolean;
    procedure SetForceDateWithMS(value: boolean);
    /// raise an exception if Col is out of range according to fColumnCount
    procedure CheckCol(Col: integer); {$ifdef HASINLINE}inline;{$endif}
    /// will set a Int64/Double/Currency/TDateTime/RawUTF8/TBlobData Dest variable
    // from a given column value
    // - internal conversion will use a temporary Variant and ColumnToVariant method
    // - expects Dest to be of the exact type (e.g. Int64, not Integer)
    function ColumnToTypedValue(Col: integer; DestType: TSQLDBFieldType;
      var Dest): TSQLDBFieldType;
    /// append the inlined value of a given parameter, mainly for GetSQLWithInlinedParams
    // - optional MaxCharCount will truncate the text to a given number of chars
    procedure AddParamValueAsText(Param: integer; Dest: TTextWriter;
      MaxCharCount: integer); virtual;
    /// return a Column as a variant
    function GetColumnVariant(const ColName: RawUTF8): Variant;
    /// return the associated statement instance for a ISQLDBRows interface
    function Instance: TSQLDBStatement;
    /// wrappers to compute sllSQL/sllDB SQL context with a local timer
    function SQLLogBegin(level: TSynLogInfo): TSynLog;
    function SQLLogEnd(const Fmt: RawUTF8; const Args: array of const): Int64; overload;
    function SQLLogEnd(msg: PShortString = nil): Int64; overload;
  public
    /// create a statement instance
    constructor Create(aConnection: TSQLDBConnection); virtual;

    /// bind a NULL value to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - some providers (e.g. OleDB during MULTI INSERT statements) expect the
    // proper column type to be set in BoundType, even for NULL values
    procedure BindNull(Param: Integer; IO: TSQLDBParamInOutType = paramIn;
      BoundType: TSQLDBFieldType = ftNull); virtual; abstract;
    /// bind an integer value to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure Bind(Param: Integer; Value: Int64;
      IO: TSQLDBParamInOutType = paramIn); overload; virtual; abstract;
    /// bind a double value to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure Bind(Param: Integer; Value: double;
      IO: TSQLDBParamInOutType = paramIn); overload; virtual; abstract;
    /// bind a TDateTime value to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindDateTime(Param: Integer; Value: TDateTime;
      IO: TSQLDBParamInOutType = paramIn); overload; virtual; abstract;
    /// bind a currency value to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindCurrency(Param: Integer; Value: currency;
      IO: TSQLDBParamInOutType = paramIn); overload; virtual; abstract;
    /// bind a UTF-8 encoded string to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindTextU(Param: Integer; const Value: RawUTF8;
      IO: TSQLDBParamInOutType = paramIn); overload; virtual; abstract;
    /// bind a UTF-8 encoded buffer text (#0 ended) to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindTextP(Param: Integer; Value: PUTF8Char;
      IO: TSQLDBParamInOutType = paramIn); overload; virtual; abstract;
    /// bind a UTF-8 encoded string to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindTextS(Param: Integer; const Value: string;
      IO: TSQLDBParamInOutType = paramIn); overload; virtual; abstract;
    /// bind a UTF-8 encoded string to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindTextW(Param: Integer; const Value: WideString;
      IO: TSQLDBParamInOutType = paramIn); overload; virtual; abstract;
    /// bind a Blob buffer to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindBlob(Param: Integer; Data: pointer; Size: integer;
      IO: TSQLDBParamInOutType = paramIn); overload; virtual; abstract;
    /// bind a Blob buffer to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindBlob(Param: Integer; const Data: RawByteString;
      IO: TSQLDBParamInOutType = paramIn); overload; virtual; abstract;
    /// bind a Variant value to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - will call all virtual Bind*() methods from the Data type
    // - if DataIsBlob is TRUE, will call BindBlob(RawByteString(Data)) instead
    // of BindTextW(WideString(Variant)) - used e.g. by TQuery.AsBlob/AsBytes
    procedure BindVariant(Param: Integer; const Data: Variant;
      DataIsBlob: boolean; IO: TSQLDBParamInOutType = paramIn); virtual;
    /// bind one TSQLVar value
    // - the leftmost SQL parameter has an index of 1
    // - this default implementation will call corresponding Bind*() method
    procedure Bind(Param: Integer; const Data: TSQLVar;
      IO: TSQLDBParamInOutType = paramIn); overload; virtual;
    /// bind one RawUTF8 encoded value
    // - the leftmost SQL parameter has an index of 1
    // - the value should match the BindArray() format, i.e. be stored as in SQL
    // (i.e. number, 'quoted string', 'YYYY-MM-DD hh:mm:ss', null) - e.g. as
    // computed by TJSONObjectDecoder.Decode()
    procedure Bind(Param: Integer; ParamType: TSQLDBFieldType;
      const Value: RawUTF8; ValueAlreadyUnquoted: boolean;
      IO: TSQLDBParamInOutType = paramIn); overload; virtual;
    /// bind an array of const values
    // - parameters marked as ? should be specified as method parameter in Params[]
    // - BLOB parameters can be bound with this method, when set after encoding
    // via BinToBase64WithMagic() call
    // - TDateTime parameters can be bound with this method, when encoded via
    // a DateToSQL() or DateTimeToSQL() call
    // - any variant parameter will be bound with BindVariant(i,VVariant^,true,IO)
    // i.e. with DataIsBlob=true
    // - this default implementation will call corresponding Bind*() method
    procedure Bind(const Params: array of const;
      IO: TSQLDBParamInOutType = paramIn); overload; virtual;
    /// bind an array of fields from an existing SQL statement
    // - can be used e.g. after ColumnsToSQLInsert() method call for fast data
    // conversion between tables
    procedure BindFromRows(const Fields: TSQLDBFieldTypeDynArray; Rows: TSQLDBStatement);
    /// bind a special CURSOR parameter to be returned as a mormot.db.sql result set
    // - Cursors are not handled internally by mORMot, but some databases (e.g.
    // Oracle) usually use such structures to get data from strored procedures
    // - such parameters are mapped as ftUnknown
    // - use BoundCursor() method to retrieve the corresponding ISQLDBRows after
    // execution of the statement
    // - this default method will raise an exception about unexpected behavior
    procedure BindCursor(Param: integer); virtual;
    /// return a special CURSOR parameter content as a mormot.db.sql result set
    // - this method is not about a column, but a parameter defined with
    // BindCursor() before method execution
    // - Cursors are not handled internally by mORMot, but some databases (e.g.
    // Oracle) usually use such structures to get data from strored procedures
    // - this method allow direct access to the data rows after execution
    // - this default method will raise an exception about unexpected behavior
    function BoundCursor(Param: Integer): ISQLDBRows; virtual;

    /// bind an array of values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - values are stored as in SQL (i.e. number, 'quoted string',
    // 'YYYY-MM-DD hh:mm:ss', null)
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    procedure BindArray(Param: Integer; ParamType: TSQLDBFieldType;
      const Values: TRawUTF8DynArray; ValuesCount: integer); overload; virtual;
    /// bind an array of integer values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    procedure BindArray(Param: Integer;
      const Values: array of Int64); overload; virtual;
    /// bind an array of double values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    procedure BindArray(Param: Integer;
      const Values: array of double); overload; virtual;
    /// bind an array of TDateTime values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - values are stored as in SQL (i.e. 'YYYY-MM-DD hh:mm:ss')
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    procedure BindArrayDateTime(Param: Integer;
      const Values: array of TDateTime); virtual;
    /// bind an array of currency values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    procedure BindArrayCurrency(Param: Integer;
      const Values: array of currency); virtual;
    /// bind an array of RawUTF8 values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - values are stored as in SQL (i.e. 'quoted string')
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    procedure BindArray(Param: Integer;
      const Values: array of RawUTF8); overload; virtual;

    /// Prepare an UTF-8 encoded SQL statement
    // - parameters marked as ? will be bound later, before ExecutePrepared call
    // - if ExpectResults is TRUE, then Step() and Column*() methods are available
    // to retrieve the data rows
    // - should raise an Exception on any error
    // - this default implementation will just store aSQL content and the
    // ExpectResults parameter, and connect to the remote server is was not
    // already connected
    procedure Prepare(const aSQL: RawUTF8; ExpectResults: boolean); overload; virtual;
    /// Execute a prepared SQL statement
    // - parameters marked as ? should have been already bound with Bind*() functions
    // - should raise an Exception on any error
    // - this void default implementation will call set fConnection.fLastAccess
    procedure ExecutePrepared; virtual;
    /// release cursor memory and resources once Step loop is finished
    // - this method call is optional, but is better be used if the ISQLDBRows
    // statement from taken from cache, and returned a lot of content which
    // may still be in client (and server) memory
    // - override to free cursor memory when ISQLDBStatement is back in cache
    procedure ReleaseRows; virtual;
    /// Reset the previous prepared statement
    // - some drivers expect an explicit reset before binding parameters and
    // executing the statement another time
    // - this default implementation will just do nothing
    procedure Reset; virtual;
    /// Prepare and Execute an UTF-8 encoded SQL statement
    // - parameters marked as ? should have been already bound with Bind*()
    //  functions above
    // - if ExpectResults is TRUE, then Step() and Column*() methods are available
    //  to retrieve the data rows
    // - should raise an Exception on any error
    // - this method will call Prepare then ExecutePrepared methods
    procedure Execute(const aSQL: RawUTF8; ExpectResults: boolean); overload;
    /// Prepare and Execute an UTF-8 encoded SQL statement
    // - parameters marked as ? should be specified as method parameter in Params[]
    // - BLOB parameters could not be bound with this method, but need an explicit
    // call to BindBlob() method
    // - if ExpectResults is TRUE, then Step() and Column*() methods are available
    // to retrieve the data rows
    // - should raise an Exception on any error
    // - this method will bind parameters, then call Excecute() virtual method
    procedure Execute(const aSQL: RawUTF8; ExpectResults: boolean;
      const Params: array of const); overload;
    /// Prepare and Execute an UTF-8 encoded SQL statement
    // - parameters marked as % will be replaced by Args[] value in the SQL text
    // - parameters marked as ? should be specified as method parameter in Params[]
    // - so could be used as such, mixing both % and ? parameters:
    // ! Statement.Execute('SELECT % FROM % WHERE RowID=?',true,[FieldName,TableName],[ID])
    // - BLOB parameters could not be bound with this method, but need an explicit
    // call to BindBlob() method
    // - if ExpectResults is TRUE, then Step() and Column*() methods are available
    // to retrieve the data rows
    // - should raise an Exception on any error
    // - this method will bind parameters, then call Excecute() virtual method
    procedure Execute(const SQLFormat: RawUTF8; ExpectResults: boolean;
      const Args, Params: array of const); overload;
    /// execute a prepared SQL statement and return all rows content as a JSON string
    // - JSON data is retrieved with UTF-8 encoding
    // - if Expanded is true, JSON data is an array of objects, for direct use
    // with any Ajax or .NET client:
    // & [ {"col1":val11,"col2":"val12"},{"col1":val21,... ]
    // - if Expanded is false, JSON data is serialized (used in TSQLTableJSON)
    // & { "FieldCount":1,"Values":["col1","col2",val11,"val12",val21,..] }
    // - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"'
    // format and contains true BLOB data
    // - this virtual implementation calls ExecutePrepared then FetchAllAsJSON()
    procedure ExecutePreparedAndFetchAllAsJSON(Expanded: boolean;
      out JSON: RawUTF8); virtual;
    /// gets a number of updates made by latest executed statement
    // - default implementation returns 0
    function UpdateCount: integer; virtual;
    /// retrieve the parameter content, after SQL execution
    // - the leftmost SQL parameter has an index of 1
    // - to be used e.g. with stored procedures:
    // ! query :=  'BEGIN TEST_PKG.DUMMY(?, ?, ?, ?, ?); END;';
    // ! stmt := Props.NewThreadSafeStatementPrepared(query, false);
    // ! stmt.Bind(1, in1, paramIn);
    // ! stmt.BindTextU(2, in2, paramIn);
    // ! stmt.BindTextU(3, in3, paramIn);
    // ! stmt.BindTextS(4, '', paramOut); //  to be retrieved with out1: string
    // ! stmt.Bind(5, 0, paramOut);       //  to be retrieved with out2: integer
    // ! stmt.ExecutePrepared;
    // ! stmt.ParamToVariant(4, out1, true);
    // ! stmt.ParamToVariant(5, out2, true);
    // - the parameter should have been bound with IO=paramOut or IO=paramInOut
    // if CheckIsOutParameter is TRUE
    // - this implementation just check that Param is correct: overridden method
    // should fill Value content
    function ParamToVariant(Param: Integer; var Value: Variant;
      CheckIsOutParameter: boolean = true): TSQLDBFieldType; virtual;

    /// After a statement has been prepared via Prepare() + ExecutePrepared() or
    // Execute(), this method must be called one or more times to evaluate it
    // - you shall call this method before calling any Column*() methods
    // - return TRUE on success, with data ready to be retrieved by Column*()
    // - return FALSE if no more row is available (e.g. if the SQL statement
    // is not a SELECT but an UPDATE or INSERT command)
    // - access the first or next row of data from the SQL Statement result:
    // if SeekFirst is TRUE, will put the cursor on the first row of results,
    // otherwise, it will fetch one row of data, to be called within a loop
    // - should raise an Exception on any error
    // - typical use may be (see also e.g. the mORMotDB unit):
    // ! var Query: ISQLDBStatement;
    // ! begin
    // !   Query := Props.NewThreadSafeStatementPrepared(
    // !    'select AccountNumber from Sales.Customer where AccountNumber like ?',
    // !    ['AW000001%'], true);
    // !   if Query <> nil then
    // !   begin
    // !     assert(SameTextU(Query.ColumnName(0), 'AccountNumber'));
    // !     while Query.Step do //  loop through all matching data rows
    // !       assert(Copy(Query.ColumnUTF8(0), 1, 8) = 'AW000001');
    // !     Query.ReleaseRows;
    // !   end;
    // ! end;
    function Step(SeekFirst: boolean = false): boolean; virtual; abstract;
    /// the column/field count of the current Row
    function ColumnCount: integer;
    /// the Column name of the current Row
    // - Columns numeration (i.e. Col value) starts with 0
    // - it's up to the implementation to ensure than all column names are unique
    function ColumnName(Col: integer): RawUTF8; virtual; abstract;
    /// returns the Column index of a given Column name
    // - Columns numeration (i.e. Col value) starts with 0
    // - returns -1 if the Column name is not found (via case insensitive search)
    function ColumnIndex(const aColumnName: RawUTF8): integer; virtual; abstract;
    /// the Column type of the current Row
    // - FieldSize can be set to store the size in chars of a ftUTF8 column
    // (0 means BLOB kind of TEXT column)
    function ColumnType(Col: integer;
      FieldSize: PInteger = nil): TSQLDBFieldType; virtual; abstract;
    /// returns TRUE if the column contains NULL
    function ColumnNull(Col: integer): boolean; virtual; abstract;
    /// return a Column integer value of the current Row, first Col is 0
    function ColumnInt(Col: integer): Int64; overload; virtual; abstract;
    /// return a Column floating point value of the current Row, first Col is 0
    function ColumnDouble(Col: integer): double; overload; virtual; abstract;
    /// return a Column date and time value of the current Row, first Col is 0
    function ColumnDateTime(Col: integer): TDateTime; overload; virtual; abstract;
    /// return a column date and time value of the current Row, first Col is 0
    // - call ColumnDateTime or ColumnUTF8 to convert into TTimeLogBits/Int64 time
    // stamp from a TDateTime or text
    function ColumnTimestamp(Col: integer): TTimeLog; overload;
    /// return a Column currency value of the current Row, first Col is 0
    function ColumnCurrency(Col: integer): currency; overload; virtual; abstract;
    /// return a Column UTF-8 encoded text value of the current Row, first Col is 0
    function ColumnUTF8(Col: integer): RawUTF8; overload; virtual; abstract;
    /// return a Column text value as generic VCL string of the current Row, first Col is 0
    // - this default implementation will call ColumnUTF8
    function ColumnString(Col: integer): string; overload; virtual;
    /// return a Column as a blob value of the current Row, first Col is 0
    function ColumnBlob(Col: integer): RawByteString; overload; virtual; abstract;
    /// return a Column as a blob value of the current Row, first Col is 0
    // - this function will return the BLOB content as a TBytes
    // - this default virtual method will call ColumnBlob()
    function ColumnBlobBytes(Col: integer): TBytes; overload; virtual;
    /// read a blob Column into the Stream parameter
    // - default implementation will just call ColumnBlob(), whereas some
    // providers (like mormot.db.sql.oracle) may implement direct support
    procedure ColumnBlobToStream(Col: integer; Stream: TStream); overload; virtual;
    /// write a blob Column into the Stream parameter
    // - expected to be used with 'SELECT .. FOR UPDATE' locking statements
    // - default implementation will through an exception, since it is highly
    // provider-specific; mormot.db.sql.oracle e.g. implements it properly
    procedure ColumnBlobFromStream(Col: integer; Stream: TStream); overload; virtual;
    /// return a Column as a variant, first Col is 0
    // - this default implementation will call ColumnToVariant() method
    // - a ftUTF8 TEXT content will be mapped into a generic WideString variant
    // for pre-Unicode version of Delphi, and a generic UnicodeString (=string)
    // since Delphi 2009: you may not loose any data during charset conversion
    // - a ftBlob BLOB content will be mapped into a TBlobData AnsiString variant
    function ColumnVariant(Col: integer): Variant; overload;
    /// return a Column as a variant, first Col is 0
    // - this default implementation will call Column*() method above
    // - a ftUTF8 TEXT content will be mapped into a generic WideString variant
    // for pre-Unicode version of Delphi, and a generic UnicodeString (=string)
    // since Delphi 2009: you may not loose any data during charset conversion
    // - a ftBlob BLOB content will be mapped into a TBlobData AnsiString variant
    function ColumnToVariant(Col: integer; var Value: Variant): TSQLDBFieldType; virtual;
    /// return a Column as a TSQLVar value, first Col is 0
    // - the specified Temp variable will be used for temporary storage of
    // svtUTF8/svtBlob values
    procedure ColumnToSQLVar(Col: Integer; var Value: TSQLVar; var Temp:
      RawByteString); virtual;
    /// return a special CURSOR Column content as a mormot.db.sql result set
    // - Cursors are not handled internally by mORMot, but some databases (e.g.
    // Oracle) usually use such structures to get data from strored procedures
    // - such columns are mapped as ftNull internally - so this method is the only
    // one giving access to the data rows
    // - this default method will raise an exception about unexpected behavior
    function ColumnCursor(Col: integer): ISQLDBRows; overload; virtual;
    /// return a Column integer value of the current Row, from a supplied column name
    function ColumnInt(const ColName: RawUTF8): Int64; overload;
    /// return a Column floating point value of the current Row, from a supplied column name
    function ColumnDouble(const ColName: RawUTF8): double; overload;
    /// return a Column date and time value of the current Row, from a supplied column name
    function ColumnDateTime(const ColName: RawUTF8): TDateTime; overload;
    /// return a column date and time value of the current Row, from a supplied column name
    // - call ColumnDateTime or ColumnUTF8 to convert into TTimeLogBits/Int64 time
    // stamp from a TDateTime or text
    function ColumnTimestamp(const ColName: RawUTF8): TTimeLog; overload;
    /// return a Column currency value of the current Row, from a supplied column name
    function ColumnCurrency(const ColName: RawUTF8): currency; overload;
    /// return a Column UTF-8 encoded text value of the current Row, from a supplied column name
    function ColumnUTF8(const ColName: RawUTF8): RawUTF8; overload;
    /// return a Column text value as generic VCL string of the current Row, from a supplied column name
    function ColumnString(const ColName: RawUTF8): string; overload;
    /// return a Column as a blob value of the current Row, from a supplied column name
    function ColumnBlob(const ColName: RawUTF8): RawByteString; overload;
    /// return a Column as a blob value of the current Row, from a supplied column name
    function ColumnBlobBytes(const ColName: RawUTF8): TBytes; overload;
    /// read a blob Column into the Stream parameter
    procedure ColumnBlobToStream(const ColName: RawUTF8; Stream: TStream); overload;
    /// write a blob Column into the Stream parameter
    // - expected to be used with 'SELECT .. FOR UPDATE' locking statements
    procedure ColumnBlobFromStream(const ColName: RawUTF8; Stream: TStream); overload;
    /// return a Column as a variant, from a supplied column name
    function ColumnVariant(const ColName: RawUTF8): Variant; overload;
    /// create a TSQLDBRowVariantType able to access any field content via late binding
    // - i.e. you can use Data.Name to access the 'Name' column of the current row
    // - this Variant will point to the corresponding TSQLDBStatement instance,
    // so it's not necessary to retrieve its value for each row
    // - typical use is:
    // ! var Row: Variant;
    // ! (...)
    // !  with MyConnProps.Execute('select * from table where name=?',[aName]) do begin
    // !    Row := RowDaa;
    // !    while Step do
    // !      writeln(Row.FirstName,Row.BirthDate);
    // !    ReleaseRows;
    // !  end;
    function RowData: Variant; virtual;
    /// create a TDocVariant custom variant containing all columns values
    // - will create a "fast" TDocVariant object instance with all fields
    procedure RowDocVariant(out aDocument: variant;
      aOptions: TDocVariantOptions = JSON_OPTIONS_FAST); virtual;
    /// return a special CURSOR Column content as a mormot.db.sql result set
    // - Cursors are not handled internally by mORMot, but some databases (e.g.
    // Oracle) usually use such structures to get data from strored procedures
    // - such columns are mapped as ftNull internally - so this method is the only
    // one giving access to the data rows
    // - this default method will raise an exception about unexpected behavior
    function ColumnCursor(const ColName: RawUTF8): ISQLDBRows; overload;
    /// append all columns values of the current Row to a JSON stream
    // - will use WR.Expand to guess the expected output format
    // - this default implementation will call Column*() methods above, but you
    // should also implement a custom version with no temporary variable
    // - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"
    // format and contains true BLOB data (unless ForceBlobAsNull property was set)
    procedure ColumnsToJSON(WR: TJSONWriter); virtual;
    /// compute the SQL INSERT statement corresponding to this columns row
    // - and populate the Fields[] array with columns information (type and name)
    // - if the current column value is NULL, will return ftNull: it is up to the
    // caller to set the proper field type
    // - the SQL statement is prepared with bound parameters, e.g.
    // $ insert into TableName (Col1,Col2) values (?,N)
    // - used e.g. to convert some data on the fly from one database to another,
    // via the TSQLDBConnection.NewTableFromRows method
    function ColumnsToSQLInsert(const TableName: RawUTF8;
      var Fields: TSQLDBColumnCreateDynArray): RawUTF8; virtual;
    // append all rows content as a JSON stream
    // - JSON data is added to the supplied TStream, with UTF-8 encoding
    // - if Expanded is true, JSON data is an array of objects, for direct use
    // with any Ajax or .NET client:
    // & [ {"col1":val11,"col2":"val12"},{"col1":val21,... ]
    // - if Expanded is false, JSON data is serialized (used in TSQLTableJSON)
    // & { "FieldCount":1,"Values":["col1","col2",val11,"val12",val21,..] }
    // - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"'
    // format and contains true BLOB data
    // - similar to corresponding TSQLRequest.Execute method in the
    // mormot.db.raw.sqlite3 unit
    // - returns the number of row data returned (excluding field names)
    // - warning: TSQLRestStorageExternal.EngineRetrieve in mORMotDB unit
    // expects the Expanded=true format to return '[{...}]'#10
    function FetchAllToJSON(JSON: TStream; Expanded: boolean): PtrInt;
    // Append all rows content as a CSV stream
    // - CSV data is added to the supplied TStream, with UTF-8 encoding
    // - if Tab=TRUE, will use TAB instead of ',' between columns
    // - you can customize the ',' separator - use e.g. the global ListSeparator
    // variable (from SysUtils) to reflect the current system definition (some
    // country use ',' as decimal separator, for instance our "douce France")
    // - AddBOM will add a UTF-8 Byte Order Mark at the beginning of the content
    // - BLOB fields will be appended as "blob" with no data
    // - returns the number of row data returned
    function FetchAllToCSVValues(Dest: TStream; Tab: boolean;
      CommaSep: AnsiChar = ','; AddBOM: boolean = true): PtrInt;
    // return all rows content as a JSON string
    // - JSON data is retrieved with UTF-8 encoding
    // - if Expanded is true, JSON data is an array of objects, for direct use
    // with any Ajax or .NET client:
    // & [ {"col1":val11,"col2":"val12"},{"col1":val21,... ]
    // - if Expanded is false, JSON data is serialized (used in TSQLTableJSON)
    // & { "FieldCount":1,"Values":["col1","col2",val11,"val12",val21,..] }
    // - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"'
    // format and contains true BLOB data
    // - if ReturnedRowCount points to an integer variable, it will be filled with
    // the number of row data returned (excluding field names)
    // - similar to corresponding TSQLRequest.Execute method in the
    // mormot.db.raw.sqlite3 unit
    function FetchAllAsJSON(Expanded: boolean; ReturnedRowCount: PPtrInt = nil): RawUTF8;
    /// append all rows content as binary stream
    // - will save the column types and name, then every data row in optimized
    // binary format (faster and smaller than JSON)
    // - you can specify a LIMIT for the data extent (default 0 meaning all data)
    // - generates the format expected by TSQLDBProxyStatement
    function FetchAllToBinary(Dest: TStream; MaxRowCount: cardinal = 0;
      DataRowPosition: PCardinalDynArray = nil): cardinal; virtual;
    /// append current row content as binary stream
    // - will save one data row in optimized binary format (if not in Null)
    // - virtual method called by FetchAllToBinary()
    // - follows the format expected by TSQLDBProxyStatement
    procedure ColumnsToBinary(W: TFileBufferWriter; Null: pointer;
      const ColTypes: TSQLDBFieldTypeDynArray); virtual;
    /// low-level access to the Timer used for last DB operation
    property SQLLogTimer: TPrecisionTimer read fSQLLogTimer;
    /// after a call to Prepare(), contains the query text to be passed to the DB
    // - depending on the DB, parameters placeholders are replaced by ?, :1, $1 etc
    // - this SQL is ready to be used in any DB tool, e.g. to check the real
    // execution plan/timing
    property SQLPrepared: RawUTF8 read fSQLPrepared;
    /// the prepared SQL statement, in its current state
    // - if statement is prepared, then equals SQLPrepared, otherwise, contains
    // the raw SQL property content
    // - used internally by the implementation units, e.g. for errors logging
    property SQLCurrent: RawUTF8 read GetSQLCurrent;
    /// low-level access to the statement cache index, after a call to Prepare()
    // - contains >= 0 if the database supports prepared statement cache
    //(Oracle, Postgres) and query plan is cached; contains -1 in other cases
    property CacheIndex: integer read fCacheIndex;
  published
    /// the prepared SQL statement, as supplied to Prepare() method
    property SQL: RawUTF8 read fSQL;
    /// the prepared SQL statement, with all '?' changed into the supplied
    // parameter values
    // - such statement query plan usually differ from a real execution plan
    // for prepared statements with parameters - see SQLPrepared property instead
    property SQLWithInlinedParams: RawUTF8 read GetSQLWithInlinedParams;
    /// the current row after Execute/Step call, corresponding to Column*() methods
    // - contains 0 before initial Step call, or a number >=1 during data retrieval
    property CurrentRow: Integer read fCurrentRow;
    /// the total number of data rows retrieved by this instance
    // - is not reset when there is no more row of available data (Step returns
    // false), or when Step() is called with SeekFirst=true
    property TotalRowsRetrieved: Integer read fTotalRowsRetrieved;
    /// the associated database connection
    property Connection: TSQLDBConnection read fConnection;
    /// strip last semicolon in query
    // - expectation may vary, depending on the SQL statement and the engine
    // - default is true
    property StripSemicolon: boolean read fStripSemicolon write fStripSemicolon;
  end;


{ ************ Parent Classes for Thread-Safe and Parametrized Connections }

type
  /// abstract connection created from TSQLDBConnectionProperties
  // - this overridden class will defined an hidden thread ID, to ensure
  // that one connection will be create per thread
  // - e.g. OleDB, ODBC and Oracle connections will inherit from this class
  TSQLDBConnectionThreadSafe = class(TSQLDBConnection)
  protected
    fThreadID: TThreadID;
  end;

  /// threading modes set to TSQLDBConnectionPropertiesThreadSafe.ThreadingMode
  // - default mode is to use a Thread Pool, i.e. one connection per thread
  // - or you can force to use the main connection
  // - or you can use a shared background thread process (not implemented yet)
  // - last two modes could be used for embedded databases (SQLite3/FireBird),
  // when multiple connections may break stability, consume too much resources
  // and/or decrease performance
  TSQLDBConnectionPropertiesThreadSafeThreadingMode = (
    tmThreadPool, tmMainConnection, tmBackgroundThread);

  /// connection properties which will implement an internal Thread-Safe
  // connection pool
  TSQLDBConnectionPropertiesThreadSafe = class(TSQLDBConnectionProperties)
  protected
    fConnectionPool: TSynObjectListLocked;
    fLatestConnectionRetrievedInPool: integer;
    fThreadingMode: TSQLDBConnectionPropertiesThreadSafeThreadingMode;
    /// returns -1 if none was defined yet
    function CurrentThreadConnectionIndex: Integer;
    /// overridden method to properly handle multi-thread
    function GetMainConnection: TSQLDBConnection; override;
  public
    /// initialize the properties
    // - this overridden method will initialize the internal per-thread connection pool
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8); override;
    /// release related memory, and all per-thread connections
    destructor Destroy; override;
    /// get a thread-safe connection
    // - this overridden implementation will define a per-thread TSQLDBConnection
    // connection pool, via an internal  pool
    function ThreadSafeConnection: TSQLDBConnection; override;
    /// release all existing connections
    // - this overridden implementation will release all per-thread
    // TSQLDBConnection internal connection pool
    // - warning: no connection shall still be used on the background (e.g. in
    // multi-threaded applications), or some unexpected border effects may occur
    procedure ClearConnectionPool; override;
    /// you can call this method just before a thread is finished to ensure
    // that the associated Connection will be released
    // - could be used e.g. in a try...finally block inside a TThread.Execute
    // overridden method
    // - could be used e.g. to call CoUnInitialize from thread in which
    // CoInitialize was made, for instance via a method defined as such:
    // ! procedure TMyServer.OnHttpThreadTerminate(Sender: TObject);
    // ! begin
    // !   fMyConnectionProps.EndCurrentThread;
    // ! end;
    // - this method shall be called from the thread about to be terminated: e.g.
    // if you call it from the main thread, it may fail to release resources
    // - within the mORMot server, mORMotDB unit will call this method
    // for every terminating thread created for TSQLRestServerNamedPipeResponse
    // or TSQLHttpServer multi-thread process
    procedure EndCurrentThread; virtual;
    /// set this property if you want to disable the per-thread connection pool
    // - to be used e.g. in database embedded mode (SQLite3/FireBird), when
    // multiple connections may break stability and decrease performance
    // - see TSQLDBConnectionPropertiesThreadSafeThreadingMode for the
    // possible values
    property ThreadingMode: TSQLDBConnectionPropertiesThreadSafeThreadingMode
      read fThreadingMode write fThreadingMode;
  end;

  /// a structure used to store a standard binding parameter
  // - you can use your own internal representation of parameters
  // (TOleDBStatement use its own TOleDBStatementParam type), but
  // this type can be used to implement a generic parameter
  // - used e.g. by TSQLDBStatementWithParams as a dynamic array
  // (and its inherited TSQLDBOracleStatement)
  // - don't change this structure, since it will be serialized as binary
  // for TSQLDBProxyConnectionCommandExecute
  TSQLDBParam = packed record
    /// storage used for TEXT (ftUTF8) and BLOB (ftBlob) values
    // - ftBlob are stored as RawByteString
    // - ftUTF8 are stored as RawUTF8
    // - sometimes, may be ftInt64 or ftCurrency provided as SQLT_AVC text,
    // or ftDate value converted to SQLT_TIMESTAMP
    VData: RawByteString;
    /// storage used for bound array values
    // - number of items in array is stored in VInt64
    // - values are stored as in SQL (i.e. number, 'quoted string',
    // 'YYYY-MM-DD hh:mm:ss', null)
    VArray: TRawUTF8DynArray;
    /// the column/parameter Value type
    VType: TSQLDBFieldType;
    /// define if parameter can be retrieved after a stored procedure execution
    VInOut: TSQLDBParamInOutType;
    /// used e.g. by TSQLDBOracleStatement
    VDBType: word;
    /// storage used for ftInt64, ftDouble, ftDate and ftCurrency value
    VInt64: Int64;
  end;

  PSQLDBParam = ^TSQLDBParam;

  /// dynamic array used to store standard binding parameters
  // - used e.g. by TSQLDBStatementWithParams (and its
  // inherited TSQLDBOracleStatement)
  TSQLDBParamDynArray = array of TSQLDBParam;

  /// generic abstract class handling prepared statements with binding
  // - will provide protected fields and methods for handling standard
  // TSQLDBParam parameters
  TSQLDBStatementWithParams = class(TSQLDBStatement)
  protected
    fParams: TSQLDBParamDynArray;
    fParam: TDynArray;
    fParamsArrayCount: integer;
    function CheckParam(Param: Integer; NewType: TSQLDBFieldType;
      IO: TSQLDBParamInOutType): PSQLDBParam; overload;
    function CheckParam(Param: Integer; NewType: TSQLDBFieldType;
      IO: TSQLDBParamInOutType; ArrayCount: integer): PSQLDBParam; overload;
    /// append the inlined value of a given parameter
    // - faster overridden method
    procedure AddParamValueAsText(Param: integer; Dest: TTextWriter;
      MaxCharCount: integer); override;
  public
    /// create a statement instance
    // - this overridden version will initialize the internal fParam* fields
    constructor Create(aConnection: TSQLDBConnection); override;
    /// bind a NULL value to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an Exception on any error
    // - some providers (only OleDB during MULTI INSERT statements, so never used
    // in this class) expect the  proper column type to be set in BoundType
    procedure BindNull(Param: Integer; IO: TSQLDBParamInOutType = paramIn;
      BoundType: TSQLDBFieldType = ftNull); override;
    /// bind an integer value to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an Exception on any error
    procedure Bind(Param: Integer; Value: Int64;
      IO: TSQLDBParamInOutType = paramIn); overload; override;
    /// bind a double value to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an Exception on any error
    procedure Bind(Param: Integer; Value: double;
      IO: TSQLDBParamInOutType = paramIn); overload; override;
    /// bind a TDateTime value to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an Exception on any error
    procedure BindDateTime(Param: Integer; Value: TDateTime;
      IO: TSQLDBParamInOutType = paramIn); overload; override;
    /// bind a currency value to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an Exception on any error
    procedure BindCurrency(Param: Integer; Value: currency;
      IO: TSQLDBParamInOutType = paramIn); overload; override;
    /// bind a UTF-8 encoded string to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an Exception on any error
    procedure BindTextU(Param: Integer; const Value: RawUTF8;
      IO: TSQLDBParamInOutType = paramIn); overload; override;
    /// bind a UTF-8 encoded buffer text (#0 ended) to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindTextP(Param: Integer; Value: PUTF8Char;
      IO: TSQLDBParamInOutType = paramIn); overload; override;
    /// bind a VCL string to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an Exception on any error
    procedure BindTextS(Param: Integer; const Value: string;
      IO: TSQLDBParamInOutType = paramIn); overload; override;
    /// bind an OLE WideString to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an Exception on any error }
    procedure BindTextW(Param: Integer; const Value: WideString;
      IO: TSQLDBParamInOutType = paramIn); overload; override;
    /// bind a Blob buffer to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an Exception on any error
    procedure BindBlob(Param: Integer; Data: pointer; Size: integer;
      IO: TSQLDBParamInOutType = paramIn); overload; override;
    /// bind a Blob buffer to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an Exception on any error M
    procedure BindBlob(Param: Integer; const Data: RawByteString;
      IO: TSQLDBParamInOutType = paramIn); overload; override;

    /// bind an array of values to a parameter using OCI bind array feature
    // - the leftmost SQL parameter has an index of 1
    // - values are stored as in SQL (i.e. number, 'quoted string',
    // 'YYYY-MM-DD hh:mm:ss', null)
    // - values are stored as in SQL (i.e. 'YYYY-MM-DD hh:mm:ss')
    procedure BindArray(Param: Integer; ParamType: TSQLDBFieldType;
      const Values: TRawUTF8DynArray; ValuesCount: integer); overload; override;
    /// bind an array of integer values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - this default implementation will call BindArray() after conversion into
    // RawUTF8 items, stored in TSQLDBParam.VArray
    procedure BindArray(Param: Integer;
      const Values: array of Int64); overload; override;
    /// bind an array of double values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    // - this default implementation will call BindArray() after conversion into
    // RawUTF8 items, stored in TSQLDBParam.VArray
    procedure BindArray(Param: Integer;
      const Values: array of double); overload; override;
    /// bind an array of TDateTime values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - values are stored as in SQL (i.e. 'YYYY-MM-DD hh:mm:ss')
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    // - this default implementation will call BindArray() after conversion into
    // RawUTF8 items, stored in TSQLDBParam.VArray
    procedure BindArrayDateTime(Param: Integer;
      const Values: array of TDateTime); override;
    /// bind an array of currency values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    // - this default implementation will call BindArray() after conversion into
    // RawUTF8 items, stored in TSQLDBParam.VArray
    procedure BindArrayCurrency(Param: Integer;
      const Values: array of currency); override;
    /// bind an array of RawUTF8 values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - values are stored as 'quoted string'
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    procedure BindArray(Param: Integer;
      const Values: array of RawUTF8); overload; override;

    /// start parameter array binding per-row process
    // - BindArray*() methods expect the data to be supplied "verticaly": this
    // method allow-per row binding
    // - call this method, then BindArrayRow() with the corresponding values for
    // one statement row, then Execute to send the query
    procedure BindArrayRowPrepare(const aParamTypes: array of TSQLDBFieldType;
      aExpectedMinimalRowCount: integer = 0);
    /// bind a set of parameters for further array binding
    // - supplied parameters shall follow the BindArrayRowPrepare() supplied
    // types (i.e. RawUTF8, Integer/Int64, double);  you can also bind directly
    // a TDateTime value if the corresponding binding has been defined as ftDate
    // by BindArrayRowPrepare()
    procedure BindArrayRow(const aValues: array of const);
    /// bind an array of fields from an existing SQL statement for array binding
    // - supplied Rows columns shall follow the BindArrayRowPrepare() supplied
    // types (i.e. RawUTF8, Integer/Int64, double, date)
    // - can be used e.g. after ColumnsToSQLInsert() method call for fast data
    // conversion between tables
    procedure BindFromRows(Rows: TSQLDBStatement); virtual;

    /// retrieve the parameter content, after SQL execution
    // - the leftmost SQL parameter has an index of 1
    // - to be used e.g. with stored procedures
    // - this overridden function will retrieve the value stored in the protected
    // fParams[] array: the ExecutePrepared method should have updated its
    // content as exepcted
    function ParamToVariant(Param: Integer; var Value: Variant;
      CheckIsOutParameter: boolean = true): TSQLDBFieldType; override;

    /// Reset the previous prepared statement
    // - this overridden implementation will just do reset the internal fParams[]
    procedure Reset; override;
    /// Release used memory
    // - this overridden implementation will free the fParams[] members (e.g.
    // VData) but not the parameters themselves
    procedure ReleaseRows; override;
  end;

  /// generic abstract class handling prepared statements with binding
  // and column description
  // - will provide protected fields and methods for handling both TSQLDBParam
  // parameters and standard TSQLDBColumnProperty column description
  TSQLDBStatementWithParamsAndColumns = class(TSQLDBStatementWithParams)
  protected
    fColumns: TSQLDBColumnPropertyDynArray;
    fColumn: TDynArrayHashed;
  public
    /// create a statement instance
    // - this overridden version will initialize the internal fColumn* fields
    constructor Create(aConnection: TSQLDBConnection); override;
    /// retrieve a column name of the current Row
    // - Columns numeration (i.e. Col value) starts with 0
    // - it's up to the implementation to ensure than all column names are unique
    function ColumnName(Col: integer): RawUTF8; override;
    /// returns the Column index of a given Column name
    // - Columns numeration (i.e. Col value) starts with 0
    // - returns -1 if the Column name is not found (via case insensitive search)
    function ColumnIndex(const aColumnName: RawUTF8): integer; override;
    /// the Column type of the current Row
    // - ftCurrency type should be handled specifically, for faster process and
    // avoid any rounding issue, since currency is a standard OleDB type
    // - FieldSize can be set to store the size in chars of a ftUTF8 column
    // (0 means BLOB kind of TEXT column) - this implementation will store
    // fColumns[Col].ColumnValueDBSize if ColumnValueInlined=true
    function ColumnType(Col: integer;
      FieldSize: PInteger = nil): TSQLDBFieldType; override;
    /// direct access to the columns description
    // - gives more details than the default ColumnType() function
    property Columns: TSQLDBColumnPropertyDynArray read fColumns;
  end;



implementation


{ ************ General SQL Processing Functions }

procedure LogTruncatedColumn(const Col: TSQLDBColumnProperty);
begin
  SynDBLog.Add.Log(sllDB, 'Truncated column %', Col.ColumnName);
end;

function TrimLeftSchema(const TableName: RawUTF8): RawUTF8;
var
  i, j: integer;
begin
  j := 1;
  repeat
    i := PosEx('.', TableName, j);
    if i = 0 then
      break;
    j := i + 1;
  until false;
  if j = 1 then
    result := TableName
  else
    result := copy(TableName, j, maxInt);
end;

function ReplaceParamsByNames(const aSQL: RawUTF8; var aNewSQL: RawUTF8;
  aStripSemicolon: boolean): integer;
var
  i, j, B, L: PtrInt;
  P: PAnsiChar;
  c: array[0..3] of AnsiChar;
  tmp: RawUTF8;
const
  SQL_KEYWORDS: array[0..19] of AnsiChar = 'ASATBYIFINISOFONORTO';
begin
  result := 0;
  L := Length(aSQL);
  if aStripSemicolon then
    while (L > 0) and (aSQL[L] in [#1..' ', ';']) do
      if (aSQL[L] = ';') and (L > 5) and IdemPChar(@aSQL[L - 3], 'END') then
        break
      else // allows 'END;' at the end of a statement
        dec(L);    // trim ' ' or ';' right (last ';' could be found incorrect)
  if PosExChar('?', aSQL) > 0 then
  begin
    aNewSQL := '';
    // change ? into :AA :BA ..
    c := ':AA';
    i := 0;
    P := pointer(aSQL);
    if P <> nil then
      repeat
        B := i;
        while (i < L) and (P[i] <> '?') do
        begin
          if P[i] = '''' then
          begin
            repeat // ignore chars inside ' quotes
              inc(i);
            until (i = L) or ((P[i] = '''') and (P[i + 1] <> ''''));
            if i = L then
              break;
          end;
          inc(i);
        end;
        FastSetString(tmp, P + B, i - B);
        aNewSQL := aNewSQL + tmp;
        if i = L then
          break;
        // store :AA :BA ..
        j := length(aNewSQL);
        SetLength(aNewSQL, j + 3);
        PCardinal(PtrInt(aNewSQL) + j)^ := PCardinal(@c)^;
        repeat
          if c[1] = 'Z' then
          begin
            if c[2] = 'Z' then
              raise ESQLDBException.Create('Parameters :AA to :ZZ');
            c[1] := 'A';
            inc(c[2]);
          end
          else
            inc(c[1]);
        until WordScanIndex(@SQL_KEYWORDS, length(SQL_KEYWORDS) shr 1, PWord(@c[1])^) < 0;
        inc(result);
        inc(i); // jump '?'
      until i = L;
  end
  else
    aNewSQL := copy(aSQL, 1, L); // trim right ';' if any
end;

function ReplaceParamsByNumbers(const aSQL: RawUTF8; var aNewSQL: RawUTF8;
  IndexChar: AnsiChar; AllowSemicolon: boolean): integer;
var
  ndx, L: PtrInt;
  s, d: PUTF8Char;
  c: AnsiChar;
begin
  aNewSQL := aSQL;
  result := 0;
  ndx := 0;
  L := Length(aSQL);
  s := pointer(aSQL);
  if (s = nil) or (PosExChar('?', aSQL) = 0) then
    exit;
  // calculate ? parameters count, check for ;
  while s^ <> #0 do
  begin
    c := s^;
    if c = '?' then
    begin
      inc(ndx);
      if ndx > 9 then  // ? will be replaced by $n $nn $nnn
        if ndx > 99 then
          if ndx > 999 then
            exit
          else
            inc(L, 3)
        else
          inc(L, 2)
        else
          inc(L);
    end
    else if c = '''' then
    begin
      repeat
        inc(s);
        c := s^;
        if c = #0 then
          exit; // quote without proper ending -> reject
        if c = '''' then
          if s[1] = c then
            inc(s) // ignore double quotes between single quotes
          else
            break;
      until false;
    end else if (c = ';') and not AllowSemicolon then
      exit; // complex expression can not be prepared
    inc(s);
  end;
  if ndx = 0 then // no ? parameter
    exit;
  result := ndx;
  // parse SQL and replace ? into $n $nn $nnn
  FastSetString(aNewSQL, nil, L);
  s := pointer(aSQL);
  d := pointer(aNewSQL);
  ndx := 0;
  repeat
    c := s^;
    if c = '?' then
    begin
      d^ := IndexChar; // e.g. '$'
      inc(d);
      inc(ndx);
      d := Append999ToBuffer(d, ndx);
    end
    else if c = '''' then
    begin
      repeat // ignore double quotes between single quotes
        d^ := c;
        inc(d);
        inc(s);
        c := s^;
        if c = '''' then
          if s[1] = c then
          begin
            d^ := c;
            inc(d);
            inc(s) // ignore double quotes between single quotes
          end
          else
            break;
      until false;
      d^ := c; // store last '''
      inc(d);
    end
    else
    begin
      d^ := c;
      inc(d);
    end;
    inc(s);
  until s^ = #0;
  //assert(d - pointer(aNewSQL) = length(aNewSQL)); // until stabilized
end;

function BoundArrayToJSONArray(const Values: TRawUTF8DynArray): RawUTF8;
//  'one', 't"wo' -> '{"one","t\"wo"}'  and  1,2,3 -> '{1,2,3}'
var
  V: ^RawUTF8;
  s, d: PUTF8Char;
  L, vl, n: PtrInt;
  c: AnsiChar;
label
  _dq;
begin
  result := '';
  n := length(Values);
  if n = 0 then
    exit;
  L := 1; // trailing '{'
  inc(L, n); // ',' after each element - and ending '}'
  v := pointer(Values);
  repeat
    vl := length(v^);
    if vl <> 0 then
    begin
      inc(L, vl);
      s := pointer(v^);
      if s^ = '''' then
      begin // quoted ftUTF8
        dec(vl, 2);
        if vl > 0 then
          repeat
            inc(s);
            c := s^;
            if c = '''' then
            begin
              if s[1] = '''' then
                dec(L); // double ' into single '
            end
            else if (c = '"') or (c = '\') then
              inc(L); // escape \ before "
            dec(vl);
          until vl = 0;
      end;
    end;
    inc(v);
    dec(n);
  until n = 0;
  FastSetString(result, nil, L);
  d := pointer(result);
  d^ := '{';
  inc(d);
  v := pointer(Values);
  n := length(Values);
  repeat
    vl := length(v^);
    if vl <> 0 then
    begin
      s := pointer(v^);
      if s^ = '''' then // quoted ftUTF8
      begin
        d^ := '"';
        inc(d);
        dec(vl, 2);
        if vl > 0 then
          repeat
            inc(s);
            c := s^;
            if c = '''' then
            begin
              if s[1] = '''' then
                goto _dq; // double ' into single '
            end
            else if (c = '"') or (c = '\') then
            begin
              d^ := '\'; // escape \ before "
              inc(d);
            end;
            d^ := c;
            inc(d);
_dq:        dec(vl);
          until vl = 0;
        d^ := '"';
        inc(d);
      end
      else
        repeat // regular content
          d^ := s^;
          inc(d);
          inc(s);
          dec(vl);
        until vl = 0;
    end;
    d^ := ',';
    inc(d);
    inc(v);
    dec(n);
  until n = 0;
  d[-1] := '}'; // replace last ',' by '}'
  //assert(d - pointer(result) = length(result)); // until stabilized
end;


{ ************ Define Database Engine Specific Behavior }

function ToText(DBMS: TSQLDBDefinition): PShortString;
begin
  result := GetEnumName(TypeInfo(TSQLDBDefinition), ord(DBMS));
end;


{ ************ Abstract SQL DB Classes and Interfaces }

{ ESQLDBException }

constructor ESQLDBException.CreateUTF8(const Format: RawUTF8; const Args: array of const);
var
  msg {$ifndef SYNDB_SILENCE}, sql{$endif}: RawUTF8;
begin
  msg := FormatUTF8(Format, Args);
  {$ifndef SYNDB_SILENCE}
  if (length(Args) > 0) and (Args[0].VType = vtObject) and (Args[0].VObject <> nil) then
    if Args[0].VObject.InheritsFrom(TSQLDBStatement) then
    begin
      fStatement := TSQLDBStatement(Args[0].VObject);
      if fStatement.Connection.Properties.LogSQLStatementOnException then
      begin
        try
          sql := fStatement.GetSQLWithInlinedParams;
        except
          sql := fStatement.SQL; // if parameter access failed -> append with ?
        end;
        msg := msg + ' - ' + sql;
      end;
    end;
  {$endif SYNDB_SILENCE}
  inherited Create(UTF8ToString(msg));
end;


{ TSQLDBRowVariantType }

function TSQLDBRowVariantType.IntGet(var Dest: TVarData; const Instance:
  TVarData; Name: PAnsiChar; NameLen: PtrInt): boolean;
var
  Rows: TSQLDBStatement;
  col: RawUTF8;
  ndx: integer;
begin
  Rows := TSQLDBStatement(Instance.VPointer);
  if Rows = nil then
    raise ESQLDBException.CreateUTF8('Invalid % call', [self]);
  FastSetString(col, Name, NameLen);
  ndx := Rows.ColumnIndex(col);
  result := ndx >= 0;
  if ndx >= 0 then
    Rows.ColumnToVariant(ndx, Variant(Dest));
end;


{ TSQLDBConnectionProperties }

constructor TSQLDBConnectionProperties.Create(const aServerName, aDatabaseName,
  aUserID, aPassWord: RawUTF8);
var
  aDBMS: TSQLDBDefinition;
begin
  fServerName := aServerName;
  fDatabaseName := aDatabaseName;
  fUserID := aUserID;
  fPassWord := aPassWord;
  fEngineName := EngineName;
  fRollbackOnDisconnect := true; // enabled by default
  fUseCache := true;
  fLoggedSQLMaxSize := 2048; // log up to 2KB of inlined SQL by default
  SetInternalProperties; // virtual method used to override default parameters
  aDBMS := GetDBMS;
  if aDBMS in [dSQLite, dDB2, dPostgreSQL] then // for SQLDateToIso8601Quoted()
    fDateTimeFirstChar := ' '
  else
    fDateTimeFirstChar := 'T';
  if fForcedSchemaName = '' then
    case aDBMS of // should make every one life's easier
      dMSSQL:
        fForcedSchemaName := 'dbo';
      dPostgreSql:
        fForcedSchemaName := 'public';
    end;
  if fSQLCreateField[ftUnknown] = '' then
    fSQLCreateField := DB_FIELDS[aDBMS];
  if fSQLCreateFieldMax = 0 then
    fSQLCreateFieldMax := DB_FIELDSMAX[aDBMS];
  if fSQLGetServerTimestamp = '' then
    fSQLGetServerTimestamp := DB_SERVERTIME[aDBMS];
  case aDBMS of
    dMSSQL, dJet:
      fStoreVoidStringAsNull := true;
  end;
  if byte(fBatchSendingAbilities) = 0 then // if not already handled by driver
    case aDBMS of
      dSQlite, dMySQL, dPostgreSQL, dNexusDB, dMSSQL, dDB2, // INSERT with multi VALUES
      //dFirebird,  EXECUTE BLOCK with params is slower (at least for embedded)
      dOracle:
        begin // Oracle expects weird INSERT ALL INTO ... statement
          fBatchSendingAbilities := [cCreate];
          fOnBatchInsert := MultipleValuesInsert;
          fBatchMaxSentAtOnce := 4096; // MultipleValuesInsert will do chunking
        end;
      dFirebird:
        begin // will run EXECUTE BLOCK without parameters
          fBatchSendingAbilities := [cCreate];
          fOnBatchInsert := MultipleValuesInsertFirebird;
          fBatchMaxSentAtOnce := 4096; // MultipleValuesInsert will do chunking
        end;
    end;
end;

destructor TSQLDBConnectionProperties.Destroy;
begin
  fMainConnection.Free;
  inherited;
end;

function TSQLDBConnectionProperties.Execute(const aSQL: RawUTF8;
  const Params: array of const; RowsVariant: PVariant; ForceBlobAsNull: boolean): ISQLDBRows;
var
  Stmt: ISQLDBStatement;
begin
  Stmt := NewThreadSafeStatementPrepared(aSQL, true, true);
  Stmt.ForceBlobAsNull := ForceBlobAsNull;
  Stmt.Bind(Params);
  Stmt.ExecutePrepared;
  result := Stmt;
  if RowsVariant <> nil then
    if result = nil then
      SetVariantNull(RowsVariant^)
    else
      RowsVariant^ := result.RowData;
end;

function TSQLDBConnectionProperties.ExecuteNoResult(const aSQL: RawUTF8;
  const Params: array of const): integer;
var
  Stmt: ISQLDBStatement;
begin
  Stmt := NewThreadSafeStatementPrepared(aSQL, false, true);
  Stmt.Bind(Params);
  Stmt.ExecutePrepared;
  try
    result := Stmt.UpdateCount;
  except // may occur e.g. for Firebird's CREATE DATABASE
    result := 0;
  end;
end;

function TSQLDBConnectionProperties.PrepareInlined(const aSQL: RawUTF8;
  ExpectResults: boolean): ISQLDBStatement;
var
  Query: ISQLDBStatement;
  i, maxParam: integer;
  Types: TSQLParamTypeDynArray;
  Nulls: TSQLFieldBits;
  Values: TRawUTF8DynArray;
  GenericSQL: RawUTF8;
begin
  result := nil; // returns nil interface on error
  if self = nil then
    exit;
  // convert inlined :(1234): parameters into Values[] for Bind*() calls
  GenericSQL := ExtractInlineParameters(aSQL, Types, Values, maxParam, Nulls);
  Query := NewThreadSafeStatementPrepared(GenericSQL, ExpectResults, true);
  if Query = nil then
    exit;
  for i := 0 to maxParam - 1 do
    if i in Nulls then
      Query.BindNull(i + 1)
    else
      case Types[i] of // returned sftInteger,sftFloat,sftUTF8Text,sftBlob,sftUnknown
        sptInteger:
          Query.Bind(i + 1, GetInt64(pointer(Values[i])));
        sptFloat:
          Query.Bind(i + 1, GetExtended(pointer(Values[i])));
        sptText:
          Query.BindTextU(i + 1, Values[i]);
        sptBlob:
          if Values[i] = '' then
            Query.BindNull(i + 1)
          else
            Query.BindBlob(i + 1, pointer(Values[i]), length(Values[i]));
        sptDateTime:
          Query.BindDateTime(i + 1, Iso8601ToDateTime(Values[i]));
      else
        raise ESQLDBException.CreateUTF8('%.PrepareInlined: Unrecognized parameter Type[%] = % in [%]',
          [self, i + 1, ord(Types[i]), aSQL]);
      end;
  result := Query;
end;

function TSQLDBConnectionProperties.PrepareInlined(const SQLFormat: RawUTF8;
  const Args: array of const; ExpectResults: boolean): ISQLDBStatement;
begin
  result := PrepareInlined(FormatUTF8(SQLFormat, Args), ExpectResults);
end;

function TSQLDBConnectionProperties.ExecuteInlined(const aSQL: RawUTF8;
  ExpectResults: boolean): ISQLDBRows;
var
  Query: ISQLDBStatement;
begin
  result := nil; // returns nil interface on error
  if self = nil then
    exit;
  Query := PrepareInlined(aSQL, ExpectResults);
  if Query = nil then
    exit; // e.g. invalid aSQL
  Query.ExecutePrepared;
  result := Query;
end;

function TSQLDBConnectionProperties.ExecuteInlined(const SQLFormat: RawUTF8;
  const Args: array of const; ExpectResults: boolean): ISQLDBRows;
begin
  result := ExecuteInlined(FormatUTF8(SQLFormat, Args), ExpectResults);
end;

procedure TSQLDBConnectionProperties.SetConnectionTimeOutMinutes(minutes: cardinal);
begin
  fConnectionTimeOutTicks := minutes * 60000; // minutes to ms conversion
end;

function TSQLDBConnectionProperties.GetConnectionTimeOutMinutes: cardinal;
begin
  result := fConnectionTimeOutTicks div 60000;
end;

function TSQLDBConnectionProperties.GetMainConnection: TSQLDBConnection;
begin
  if fMainConnection.IsOutdated(GetTickCount64) then
    FreeAndNil(fMainConnection);
  if fMainConnection = nil then
    fMainConnection := NewConnection;
  result := fMainConnection;
end;

function TSQLDBConnectionProperties.{%H-}NewConnection: TSQLDBConnection;
begin
  raise ESQLDBException.CreateUTF8('%.NewConnection', [self]);
end;

function TSQLDBConnectionProperties.ThreadSafeConnection: TSQLDBConnection;
begin
  result := MainConnection; // provider should be thread-safe
end;

procedure TSQLDBConnectionProperties.ClearConnectionPool;
begin
  FreeAndNil(fMainConnection);
end;

function TSQLDBConnectionProperties.NewThreadSafeStatement: TSQLDBStatement;
begin
  result := ThreadSafeConnection.NewStatement;
end;

function TSQLDBConnectionProperties.NewThreadSafeStatementPrepared(const aSQL:
  RawUTF8; ExpectResults, RaiseExceptionOnError: boolean): ISQLDBStatement;
begin
  result := ThreadSafeConnection.NewStatementPrepared(aSQL, ExpectResults,
    RaiseExceptionOnError);
end;

function TSQLDBConnectionProperties.NewThreadSafeStatementPrepared(const
  SQLFormat: RawUTF8; const Args: array of const; ExpectResults,
  RaiseExceptionOnError: boolean): ISQLDBStatement;
begin
  result := NewThreadSafeStatementPrepared(FormatUTF8(SQLFormat, Args),
    ExpectResults, RaiseExceptionOnError);
end;

function TSQLDBConnectionProperties.SharedTransaction(SessionID: cardinal;
  action: TSQLDBSharedTransactionAction): TSQLDBConnection;

  procedure SetResultToSameConnection(index: integer);
  begin
    result := ThreadSafeConnection;
    if result <> fSharedTransactions[index].Connection then
      raise ESQLDBException.CreateUTF8('%.SharedTransaction(sessionID=%) with mixed thread connections: % and %',
        [self, SessionID, result, fSharedTransactions[index].Connection]);
  end;

var
  i, n: integer;
begin
  n := Length(fSharedTransactions);
  try
    for i := 0 to n - 1 do
      if fSharedTransactions[i].SessionID = SessionID then
      begin
        SetResultToSameConnection(i);
        case action of
          transBegin: // nested StartTransaction
            InterlockedIncrement(fSharedTransactions[i].RefCount);
        else
          begin  // (nested) commit/rollback
            if InterlockedDecrement(fSharedTransactions[i].RefCount) = 0 then
            begin
              dec(n);
              MoveFast(fSharedTransactions[i + 1], fSharedTransactions[i],
                (n - i) * sizeof(fSharedTransactions[0]));
              SetLength(fSharedTransactions, n);
              case action of
                transCommitWithException, transCommitWithoutException:
                  result.Commit;
                transRollback:
                  result.Rollback;
              end;
            end;
          end;
        end;
        exit;
      end;
    case action of
      transBegin:
        begin
          result := ThreadSafeConnection;
          for i := 0 to n - 1 do
            if fSharedTransactions[i].Connection = result then
              raise ESQLDBException.CreateUTF8(
                '%.SharedTransaction(sessionID=%) already started for sessionID=%',
                [self, SessionID, fSharedTransactions[i].SessionID]);
          if not result.Connected then
            result.Connect;
          result.StartTransaction;
          SetLength(fSharedTransactions, n + 1);
          fSharedTransactions[n].SessionID := SessionID;
          fSharedTransactions[n].RefCount := 1;
          fSharedTransactions[n].Connection := result;
        end
    else
      raise ESQLDBException.CreateUTF8('Unexpected %.SharedTransaction(%,%)',
        [self, SessionID, ord(action)]);
    end;
  except
    on Exception do
    begin
      result := nil; // result.StartTransaction/Commit/Rollback failed
      if action = transCommitWithException then
        raise;
    end;
  end;
end;

procedure TSQLDBConnectionProperties.SetInternalProperties;
begin
  // nothing to do yet
end;

procedure TSQLDBConnectionProperties.SetSchemaNameToOwner(out Owner: RawUTF8);
begin
  if fForcedSchemaName = '' then
    case fDBMS of
      dMySql:
        Owner := DatabaseName;
      dInformix:
        Owner := '';
    else
      Owner := UserID;
    end
  else
    Owner := fForcedSchemaName;
end;

function TSQLDBConnectionProperties.IsCachable(P: PUTF8Char): boolean;
var
  NoWhere: boolean;
begin // cachable if with ? parameter or SELECT without WHERE clause
  if (P <> nil) and fUseCache then
  begin
    while P^ in [#1..' '] do
      inc(P);
    NoWhere := IdemPChar(P, 'SELECT ');
    if NoWhere or not (IdemPChar(P, 'CREATE ') or IdemPChar(P, 'ALTER ')) then
    begin
      result := true;
      while P^ <> #0 do
      begin
        if P^ = '"' then
        begin // ignore chars within quotes
          repeat
            inc(P)
          until P^ in [#0, '"'];
          if P^ = #0 then
            break;
        end
        else if P^ = '?' then
          exit
        else if (P^ = ' ') and IdemPChar(P + 1, 'WHERE ') then
          NoWhere := false;
        inc(P);
      end;
    end;
    result := NoWhere;
  end
  else
    result := false;
end;

class function TSQLDBConnectionProperties.GetFieldDefinition(
  const Column: TSQLDBColumnDefine): RawUTF8;
begin
  with Column do
  begin
    FormatUTF8('% [%', [ColumnName, ColumnTypeNative], result);
    if (ColumnLength <> 0) or (Column.ColumnPrecision <> 0) or
       (Column.ColumnScale <> 0) then
      result := FormatUTF8('% % % %]',
        [result, ColumnLength, ColumnPrecision, ColumnScale])
    else
      result := result + ']';
    if ColumnIndexed then
      result := result + ' *';
  end;
end;

class function TSQLDBConnectionProperties.GetFieldORMDefinition(const Column:
  TSQLDBColumnDefine): RawUTF8;
begin // 'Name: RawUTF8 index 20 read fName write fName;';
  with Column do
  begin
    FormatUTF8('property %: %',
      [ColumnName, SQLDBFIELDTYPE_TO_DELPHITYPE[ColumnType]], result);
    if (ColumnType = ftUTF8) and (ColumnLength > 0) then
      result := FormatUTF8('% index %', [result, ColumnLength]);
    result := FormatUTF8('% read f% write f%;', [result, ColumnName, ColumnName]);
  end;
end;

var
  DB_KEYWORDS: array[TSQLDBDefinition] of TRawUTF8DynArray;

class function TSQLDBConnectionProperties.IsSQLKeyword(aDB: TSQLDBDefinition;
  aWord: RawUTF8): boolean;
const
  /// CSV of the known reserved keywords per database engine, in alphabetic order
  DB_KEYWORDS_CSV: array[TSQLDBDefinition] of PUTF8Char = (
    '',  // dUnknown
    // dDefault = ODBC / SQL-92 keywords (always checked first)
    'absolute,action,ada,add,all,allocate,alter,and,any,are,as,asc,assertion,at,authorization,' +
    'avg,begin,between,bit,bit_length,both,by,cascade,cascaded,case,cast,catalog,char,' +
    'char_length,character,character_length,check,close,coalesce,collate,collation,' +
    'column,commit,connect,connection,constraint,constraints,continue,convert,' +
    'corresponding,count,create,cross,current,current_date,current_time,' +
    'current_timestamp,current_user,cursor,date,day,deallocate,dec,decimal,declare,' +
    'default,deferrable,deferred,delete,desc,describe,descriptor,diagnostics,disconnect,' +
    'distinct,domain,double,drop,else,end,end-exec,escape,except,exception,exec,execute,' +
    'exists,external,extract,false,fetch,first,float,for,foreign,fortran,found,from,full,get,' +
    'global,go,goto,grant,group,having,hour,identity,immediate,in,include,index,indicator,' +
    'initially,inner,input,insensitive,insert,int,integer,intersect,interval,into,is,' +
    'isolation,join,key,language,last,leading,left,level,like,local,lower,match,max,min,minute,' +
    'module,month,n,names,national,natural,nchar,next,no,none,not,null,nullif,numeric,' +
    'octet_length,of,on,only,open,option,or,order,outer,output,overlaps,pad,partial,pascal,' +
    'position,precision,prepare,preserve,primary,prior,privileges,procedure,public,read,' +
    'real,references,relative,restrict,revoke,right,rollback,rows,schema,scroll,second,' +
    'section,select,session,session_user,set,size,smallint,some,space,sql,sqlca,sqlcode,' +
    'sqlerror,sqlstate,sqlwarning,substring,sum,system_user,table,temporary,then,time,' +
    'timestamp,timezone_hour,timezone_minute,to,trailing,transaction,translate,' +
    'translation,trim,true,union,unique,unknown,update,upper,usage,user,using,value,values,' +
    'varchar,varying,view,when,whenever,where,with,work,write,year,zone',
  // dOracle specific keywords (in addition to dDefault)
    'access,audit,cluster,comment,compress,exclusive,file,identified,increment,initial,' +
    'lock,long,maxextents,minus,mode,noaudit,nocompress,nowait,number,offline,online,' +
    'pctfree',
  // dMSSQL specific keywords (in addition to dDefault)
    'admin,after,aggregate,alias,array,asensitive,asymmetric,atomic,backup,before,binary,' +
    'blob,boolean,breadth,break,browse,bulk,call,called,cardinality,checkpoint,class,clob,' +
    'clustered,collect,completion,compute,condition,constructor,contains,containstable,' +
    'corr,covar_pop,covar_samp,cube,cume_dist,current_catalog,' +
    'current_default_transform_group,current_path,current_role,current_schema,' +
    'current_transform_group_for_type,cycle,data,database,dbcc,deny,depth,deref,destroy,' +
    'destructor,deterministic,dictionary,disk,distributed,dump,dynamic,each,element,' +
    'equals,errlvl,every,exit,file,fillfactor,filter,free,freetext,freetexttable,' +
    'fulltexttable,function,fusion,general,grouping,hold,holdlock,host,identity_insert,' +
    'identitycol,if,ignore,initialize,inout,intersection,iterate,kill,large,lateral,less,' +
    'like_regex,limit,lineno,ln,load,localtime,localtimestamp,locator,map,member,merge,' +
    'method,mod,modifies,modify,multiset,nclob,new,nocheck,nonclustered,normalize,object,' +
    'occurrences_regex,off,offsets,old,opendatasource,openquery,openrowset,openxml,' +
    'operation,ordinality,out,over,overlay,parameter,parameters,partition,path,percent,' +
    'percent_rank,percentile_cont,percentile_disc,pivot,plan,position_regex,postfix,' +
    'prefix,preorder,print,proc,raiserror,range,reads,readtext,reconfigure,recursive,ref,' +
    'referencing,regr_avgx,regr_avgy,regr_count,regr_intercept,regr_r2,regr_slope,' +
    'regr_sxx,regr_sxy,regr_syy,release,replication,restore,result,return,returns,revert,' +
    'role,rollup,routine,row,rowcount,rowguidcol,rule,save,savepoint,scope,search,' +
    'securityaudit,semantickeyphrasetable,semanticsimilaritydetailstable,' +
    'semanticsimilaritytable,sensitive,sequence,sets,setuser,shutdown,similar,specific,' +
    'specifictype,sqlexception,start,state,statement,static,statistics,stddev_pop,' +
    'stddev_samp,structure,submultiset,substring_regex,symmetric,system,tablesample,' +
    'terminate,textsize,than,top,tran,translate_regex,treat,trigger,truncate,try_convert,' +
    'tsequal,uescape,under,unnest,unpivot,updatetext,use,var_pop,var_samp,variable,waitfor,' +
    'while,width_bucket,window,within,within group,without,writetext,xmlagg,' +
    'xmlattributes,xmlbinary,xmlcast,xmlcomment,xmlconcat,xmldocument,xmlelement,' +
    'xmlexists,xmlforest,xmliterate,xmlnamespaces,xmlparse,xmlpi,xmlquery,xmlserialize,' +
    'xmltable,xmltext,xmlvalidate',
  // dJet specific keywords (in addition to dDefault)
    'longtext,memo,money,note,number,oleobject,owneraccess,parameters,percent,pivot,short,' +
    'single,singlefloat,stdev,stdevp,string,tableid,text,top,transform,unsignedbyte,var,' +
    'varbinary,varp,yesno',
  // dMySQL specific keywords (in addition to dDefault)
    'accessible,analyze,asensitive,auto_increment,before,bigint,binary,blob,call,change,' +
    'condition,database,databases,day_hour,day_microsecond,day_minute,day_second,' +
    'delayed,deterministic,distinctrow,div,dual,each,elseif,enclosed,enum,escaped,exit,' +
    'explain,float4,float8,force,fulltext,general,high_priority,hour_microsecond,' +
    'hour_minute,hour_second,if,ignore,ignore_server_ids,infile,inout,int1,int2,int3,int4,' +
    'int8,iterate,keys,kill,leave,limit,linear,linear,lines,load,localtime,localtimestamp,' +
    'lock,long,longblob,longtext,loop,low_priority,master_heartbeat_period,' +
    'master_ssl_verify_server_cert,master_ssl_verify_server_cert,maxvalue,' +
    'mediumblob,mediumint,mediumtext,middleint,minute_microsecond,minute_second,mod,' +
    'modifies,no_write_to_binlog,optimize,optionally,out,outfile,purge,range,range,' +
    'read_only,read_only,read_write,read_write,reads,regexp,release,rename,repeat,replace,' +
    'require,resignal signal,return,rlike,schemas,second_microsecond,sensitive,' +
    'separator,show,slow,spatial,specific,sql_big_result,sql_calc_found_rows,' +
    'sql_small_result,sqlexception,ssl,starting,straight_join,terminated,text,tinyblob,' +
    'tinyint,tinytext,trigger,undo,unlock,unsigned,use,utc_date,utc_time,utc_timestamp,' +
    'varbinary,varcharacter,while,x509,xor,year_month,zerofillaccessible',
  // dSQLite keywords (dDefault is not added to this list)
    'abort,after,and,attach,before,cluster,conflict,copy,database,delete,delimiters,detach,' +
    'each,explain,fail,from,glob,ignore,insert,instead,isnull,limit,not,notnull,offset,or,' +
    'pragma,raise,replace,row,select,statement,temp,trigger,vacuum,where',
  // dFirebird specific keywords (in addition to dDefault)
    'active,after,ascending,base_name,before,blob,cache,check_point_length,computed,' +
    'conditional,containing,cstring,currency,database,debug,descending,deterministic,do,' +
    'entry_point,exit,file,filter,function,gdscode,gen_id,generator,' +
    'group_commit_wait_time,if,inactive,input_type,log_buffer_size,logfile,manual,' +
    'maximum_segment,merge,message,module_name,num_log_buffers,output_type,over,' +
    'overflow,page,page_size,pages,parameter,parent,password,plan,post_event,protected,' +
    'raw_partitions,rdb$db_key,record_version,reserv,reserving,retain,return,' +
    'returning_values,returns,segment,shadow,shared,singular,snapshot,sort,stability,' +
    'start,starting,starts,statistics,sub_type,suspend,trigger,type,variable,wait,while',
  // dNexusDB specific keywords (in addition to dDefault)
    'abs,achar,assert,astring,autoinc,blob,block,blocksize,bool,boolean,byte,bytearray,' +
    'ceiling,chr,datetime,dword,empty,exp,floor,grow,growsize,ignore,image,initial,' +
    'initialsize,kana,largeint,locale,log,money,nullstring,nvarchar,percent,power,rand,' +
    'round,shortint,sort,string,symbols,text,tinyint,top,type,use,width,word',
  // dPostgreSQL specific keywords (in addition to dDefault)
    'abort,access,admin,after,aggregate,also,always,analyse,analyze,array,assignment,' +
    'asymmetric,backward,before,bigint,binary,boolean,cache,called,chain,characteristics,' +
    'checkpoint,class,cluster,comment,committed,concurrently,configuration,content,' +
    'conversion,copy,cost,createdb,createrole,createuser,csv,current_role,cycle,database,' +
    'defaults,definer,delimiter,delimiters,dictionary,disable,discard,do,document,each,' +
    'enable,encoding,encrypted,enum,excluding,exclusive,explain,family,force,forward,' +
    'freeze,function,granted,greatest,handler,header,hold,if,ilike,immutable,implicit,' +
    'including,increment,indexes,inherit,inherits,inout,instead,invoker,isnull,' +
    'lancompiler,large,least,limit,listen,load,localtime,localtimestamp,location,lock,' +
    'login,mapping,maxvalue,minvalue,mode,move,new,nocreatedb,nocreaterole,nocreateuser,' +
    'noinherit,nologin,nosuperuser,nothing,notify,notnull,nowait,nulls,object,off,offset,' +
    'oids,old,operator,out,overlay,owned,owner,parser,password,placing,plans,prepared,' +
    'procedural,quote,reassign,recheck,reindex,release,rename,repeatable,replace,replica,' +
    'reset,restart,returning,returns,role,row,rule,savepoint,search,security,sequence,' +
    'serializable,setof,share,show,similar,simple,stable,standalone,start,statement,' +
    'statistics,stdin,stdout,storage,strict,strip,superuser,symmetric,sysid,system,' +
    'tablespace,temp,template,text,treat,trigger,truncate,trusted,type,uncommitted,' +
    'unencrypted,unlisten,until,vacuum,valid,validator,verbose,version,volatile,' +
    'whitespace,without,xml,xmlattributes,xmlconcat,xmlelement,xmlforest,xmlparse,xmlpi,' +
    'xmlroot,xmlserialize,yes',
  // dDB2 specific keywords (in addition to dDefault)
    'activate,document,dssize,dynamic,each,editproc,elseif,enable,encoding,encryption,' +
    'ending,erase,every,excluding,exclusive,exit,explain,fenced,fieldproc,file,final,free,' +
    'function,general,generated,graphic,handler,hash,hashed_value,hint,hold,hours,if,' +
    'including,inclusive,increment,inf,infinity,inherit,inout,integrity,isobid,iterate,jar,' +
    'java,keep,label,lateral,lc_ctype,leave,linktype,localdate,locale,localtime,' +
    'localtimestamp,locator,locators,lock,lockmax,locksize,long,loop,maintained,' +
    'materialized,maxvalue,microsecond,microseconds,minutes,minvalue,mode,modifies,' +
    'months,nan,new,new_table,nextval,nocache,nocycle,nodename,nodenumber,nomaxvalue,' +
    'nominvalue,noorder,normalized,nulls,numparts,obid,old,old_table,optimization,' +
    'optimize,out,over,overriding,package,padded,pagesize,parameter,part,partition,' +
    'partitioned,partitioning,partitions,password,path,piecesize,plan,prevval,priqty,' +
    'program,psid,query,queryno,range,rank,reads,recovery,referencing,refresh,release,' +
    'rename,repeat,reset,resignal,restart,result,result_set_locator,return,returns,role,' +
    'round_ceilingadd,round_downafter,round_flooralias,round_half_downall,' +
    'round_half_evenallocate,round_half_upallow,round_upalter,routineand,' +
    'row_numberas,rowany,rownumberasensitive,rowsassociate,rowsetasutime,rrnat,' +
    'runattributes,savepointaudit,schemaauthorization,scratchpadaux,scrollauxiliary,' +
    'searchbefore,secondbegin,secondsbetween,secqtybinary,securitybufferpool,selectby,' +
    'sensitivecache,sequencecall,session_usercapture,sessioncalled,setcardinality,' +
    'signalcascaded,simplecase,snancast,someccsid,sourcechar,specificcharacter,' +
    'sqlcheck,sqlidclone,stackedclose,standardcluster,startcollection,startingcollid,' +
    'statementcolumn,staticcomment,statmentcommit,stayconcat,stogroupcondition,' +
    'storesconnect,styleconnection,substringconstraint,summarycontains,' +
    'synonymcontinue,sysfuncount,sysibmcount_big,sysproccreate,system_usercurrent,' +
    'systemcross,tablecurrent_date,tablespacecurrent_lc_ctype,thencurrent_path,' +
    'timecurrent_schema,timestampcurrent_server,tocurrent_time,' +
    'transactioncurrent_timestamp,triggercurrent_timezone,trimcurrent_user,' +
    'truncatecursor,typecycle,undodata,uniondatabase,uniquedatapartitionname,' +
    'untildatapartitionnum,updatedate,usageday,userdays,usingdb2general,' +
    'validprocdb2genrl,valuedb2sql,valuesdbinfo,variabledbpartitionname,' +
    'variantdbpartitionnum,vcatdeallocate,versiondeclare,viewdefault,' +
    'volatiledefaults,volumesdefinition,whendelete,wheneverdense_rank,wheredenserank,' +
    'whiledescribe,withdescriptor,withoutdeter',
  // dInformix specific keywords (in addition to dDefault)
    '');
var
  db: TSQLDBDefinition;
begin
  // search using fast binary lookup in the alphabetic ordered arrays
  if DB_KEYWORDS[dDefault] = nil then
    for db := Low(DB_KEYWORDS) to high(DB_KEYWORDS) do
      CSVToRawUTF8DynArray(DB_KEYWORDS_CSV[db], DB_KEYWORDS[db]);
  aWord := Trim(LowerCase(aWord));
  if (aDB = dSQLite) or
     (FastFindPUTF8CharSorted(pointer(DB_KEYWORDS[dDefault]),
       high(DB_KEYWORDS[dDefault]), pointer(aWord)) < 0) then
    if aDB <= dDefault then
      result := false
    else
      result := FastFindPUTF8CharSorted(pointer(DB_KEYWORDS[aDB]),
        high(DB_KEYWORDS[aDB]), pointer(aWord)) >= 0
  else
    result := true;
end;

function TSQLDBConnectionProperties.IsSQLKeyword(aWord: RawUTF8): boolean;
begin
  result := IsSQLKeyword(DBMS, aWord);
end;

procedure TSQLDBConnectionProperties.GetFieldDefinitions(
  const aTableName: RawUTF8; out Fields: TRawUTF8DynArray; WithForeignKeys: boolean);
var
  F: TSQLDBColumnDefineDynArray;
  Ref: RawUTF8;
  i: integer;
begin
  GetFields(aTableName, F);
  SetLength(Fields, length(F));
  for i := 0 to high(F) do
  begin
    Fields[i] := GetFieldDefinition(F[i]);
    if WithForeignKeys then
    begin
      Ref := GetForeignKey(aTableName, F[i].ColumnName);
      if Ref <> '' then
        Fields[i] := Fields[i] + ' % ' + Ref;
    end;
  end;
end;

procedure TSQLDBConnectionProperties.GetFields(const aTableName: RawUTF8;
  out Fields: TSQLDBColumnDefineDynArray);
var
  SQL: RawUTF8;
  n, i: integer;
  F: TSQLDBColumnDefine;
  FA: TDynArray;
begin
  FA.Init(TypeInfo(TSQLDBColumnDefineDynArray), Fields, @n);
  FA.Compare := SortDynArrayAnsiStringI; // FA.Find() case insensitive
  FillCharFast(F, sizeof(F), 0);
  if fDBMS = dSQLite then
  begin // SQLite3 has a specific PRAGMA metadata query
    try
      with Execute('PRAGMA table_info(`' + aTableName + '`)', []) do
        while Step do
        begin
        // cid=0,name=1,type=2,notnull=3,dflt_value=4,pk=5
          F.ColumnName := ColumnUTF8(1);
          F.ColumnTypeNative := ColumnUTF8(2);
          F.ColumnType := ColumnTypeNativeToDB(F.ColumnTypeNative, 0);
          F.ColumnIndexed := (ColumnInt(5) = 1); // by definition for SQLite3
          FA.Add(F);
        end;
    except
      on Exception do
        n := 0; // external SQLite3 providers (e.g. UniDAC) are buggy
    end;
    try
      with Execute('PRAGMA index_list(`' + aTableName + '`)', []) do
        while Step do        // seq=0,name=1,unique=2
          with Execute('PRAGMA index_info(' + ColumnUTF8(1) + ')', []) do
            while Step do
            begin
              F.ColumnName := ColumnUTF8(2); // seqno=0,cid=1,name=2
              i := FA.Find(F);
              if i >= 0 then
                Fields[i].ColumnIndexed := true;
            end;
    except
      on Exception do
        ; // ignore any exception if no index is defined
    end;
  end
  else
  begin
    SQL := SQLGetField(aTableName);
    if SQL = '' then
      exit;
    with Execute(SQL, []) do
      while Step do
      begin
        F.ColumnName := trim(ColumnUTF8(0));
        F.ColumnTypeNative := trim(ColumnUTF8(1));
        F.ColumnLength := ColumnInt(2);
        F.ColumnPrecision := ColumnInt(3);
        if ColumnNull(4) then // e.g. for plain NUMERIC in Oracle
          F.ColumnScale := -1
        else
          F.ColumnScale := ColumnInt(4);
        F.ColumnType := ColumnTypeNativeToDB(F.ColumnTypeNative, F.ColumnScale);
        if ColumnInt(5) > 0 then
          F.ColumnIndexed := true;
        FA.Add(F);
      end;
  end;
  SetLength(Fields, n);
end;

procedure TSQLDBConnectionProperties.GetIndexes(const aTableName: RawUTF8;
  out Indexes: TSQLDBIndexDefineDynArray);
var
  SQL: RawUTF8;
  n: integer;
  F: TSQLDBIndexDefine;
  FA: TDynArray;
begin
  SQL := SQLGetIndex(aTableName);
  if SQL = '' then
    exit;
  FA.Init(TypeInfo(TSQLDBIndexDefineDynArray), Indexes, @n);
  with Execute(SQL, []) do
    while Step do
    begin
      F.IndexName := trim(ColumnUTF8(0));
      F.IsUnique := ColumnInt(1) > 0;
      F.TypeDesc := trim(ColumnUTF8(2));
      F.IsPrimaryKey := ColumnInt(3) > 0;
      F.IsUniqueConstraint := ColumnInt(4) > 0;
      F.Filter := trim(ColumnUTF8(5));
      F.KeyColumns := trim(ColumnUTF8(6));
      F.IncludedColumns := trim(ColumnUTF8(7));
      FA.Add(F);
    end;
  SetLength(Indexes, n);
end;

procedure TSQLDBConnectionProperties.GetProcedureNames(out Procedures: TRawUTF8DynArray);
var
  SQL: RawUTF8;
  count: integer;
begin
  SQL := SQLGetProcedure;
  if SQL <> '' then
  try
    with Execute(SQL, []) do
    begin
      count := 0;
      while Step do
        AddSortedRawUTF8(Procedures, count, trim(ColumnUTF8(0)));
      SetLength(Procedures, count);
    end;
  except
    on Exception do
      SetLength(Procedures, 0); // if the supplied SQL query is wrong, just ignore
  end;
end;

procedure TSQLDBConnectionProperties.GetProcedureParameters(
  const aProcName: RawUTF8; out Parameters: TSQLDBProcColumnDefineDynArray);
var
  SQL: RawUTF8;
  n: integer;
  F: TSQLDBProcColumnDefine;
  FA: TDynArray;
begin
  FA.Init(TypeInfo(TSQLDBColumnDefineDynArray), Parameters, @n);
  FA.Compare := SortDynArrayAnsiStringI; // FA.Find() case insensitive
  FillcharFast(F, sizeof(F), 0);
  SQL := SQLGetParameter(aProcName);
  if SQL = '' then
    exit;
  with Execute(SQL, []) do
    while Step do
    begin
      F.ColumnName := trim(ColumnUTF8(0));
      F.ColumnTypeNative := trim(ColumnUTF8(1));
      F.ColumnLength := ColumnInt(2);
      F.ColumnPrecision := ColumnInt(3);
      if ColumnNull(4) then // e.g. for plain NUMERIC in Oracle
        F.ColumnScale := -1
      else
        F.ColumnScale := ColumnInt(4);
      F.ColumnType := ColumnTypeNativeToDB(F.ColumnTypeNative, F.ColumnScale);
      case FindCSVIndex('IN,OUT,INOUT', ColumnUTF8(5), ',', false) of
        0:
          F.ColumnParamType := paramIn;
        2:
          F.ColumnParamType := paramInOut;
      else // any other is assumed as out
        F.ColumnParamType := paramOut;
      end;
      FA.Add(F);
    end;
  SetLength(Parameters, n);
end;

procedure TSQLDBConnectionProperties.GetTableNames(out Tables: TRawUTF8DynArray);
var
  SQL, table, checkschema: RawUTF8;
  count: integer;
begin
  SQL := SQLGetTableNames;
  if SQL <> '' then
  try
    if FilterTableViewSchemaName and (fForcedSchemaName <> '') then
      checkschema := UpperCase(fForcedSchemaName) + '.';
    with Execute(SQL, []) do
    begin
      count := 0;
      while Step do
      begin
        table := trim(ColumnUTF8(0));
        if (checkschema = '') or IdemPChar(pointer(table), pointer(checkschema)) then
          AddSortedRawUTF8(Tables, count, table);
      end;
      SetLength(Tables, count);
    end;
  except
    on Exception do
      SetLength(Tables, 0); // if the supplied SQL query is wrong, just ignore
  end;
end;

procedure TSQLDBConnectionProperties.GetViewNames(out Views: TRawUTF8DynArray);
var
  SQL, table, checkschema: RawUTF8;
  count: integer;
begin
  SQL := SQLGetViewNames;
  if SQL <> '' then
  try
    if FilterTableViewSchemaName and (fForcedSchemaName <> '') then
      checkschema := UpperCase(fForcedSchemaName) + '.';
    with Execute(SQL, []) do
    begin
      count := 0;
      while Step do
      begin
        table := trim(ColumnUTF8(0));
        if (checkschema = '') or IdemPChar(pointer(table), pointer(checkschema)) then
          AddSortedRawUTF8(Views, count, table);
      end;
      SetLength(Views, count);
    end;
  except
    on Exception do
      SetLength(Views, 0); // if the supplied SQL query is wrong, just ignore
  end;
end;

procedure TSQLDBConnectionProperties.SQLSplitTableName(const aTableName: RawUTF8;
  out Owner, Table: RawUTF8);
begin
  case fDBMS of
    dSQLite:
      Table := aTableName;
  else
    begin
      Split(aTableName, '.', Owner, Table);
      if Table = '' then
      begin
        Table := Owner;
        if fForcedSchemaName = '' then
          case fDBMS of
            dMySql:
              Owner := DatabaseName;
          else
            Owner := UserID;
          end
        else
          Owner := fForcedSchemaName;
      end;
    end;
  end;
end;

procedure TSQLDBConnectionProperties.SQLSplitProcedureName(
  const aProcName: RawUTF8; out Owner, package, ProcName: RawUTF8);
var
  lOccur, i: Integer;
begin
  lOccur := 0;
  for i := 1 to length(aProcName) do
    if aProcName[i] = '.' then
      inc(lOccur);
  if lOccur = 0 then
  begin
    ProcName := aProcName;
    SetSchemaNameToOwner(Owner);
    Exit;
  end;
  case fDBMS of
    dSQLite:
      ProcName := aProcName;
    dOracle, dFirebird:
      begin // Firebird 3 has packages
        if lOccur = 2 then
        begin // OWNER.PACKAGE.PROCNAME
          Split(aProcName, '.', Owner, package);
          Split(package, '.', package, ProcName);
        end
        else
        begin // PACKAGE.PROCNAME
          Split(aProcName, '.', package, ProcName);
          Owner := UserID;
        end;
      end
  else
    begin  // OWNER.PROCNAME
      Split(aProcName, '.', Owner, ProcName);
      if ProcName = '' then
      begin
        ProcName := Owner;
        SetSchemaNameToOwner(Owner);
      end
      else if fDBMS = dMSSQL then
      // discard ;1 when MSSQL stored procedure name is ProcName;1
        Split(ProcName, ';', ProcName);
    end;
  end;
end;

function TSQLDBConnectionProperties.SQLFullTableName(const aTableName: RawUTF8): RawUTF8;
begin
  if (aTableName <> '') and (fForcedSchemaName <> '') and (PosExChar('.',
    aTableName) = 0) then
    result := fForcedSchemaName + '.' + aTableName
  else
    result := aTableName;
end;

function TSQLDBConnectionProperties.SQLGetField(const aTableName: RawUTF8): RawUTF8;
var
  Owner, Table: RawUTF8;
  FMT: RawUTF8;
begin
  result := '';
  case DBMS of
    dOracle:
      FMT :=
        'select c.column_name, c.data_type, c.data_length, c.data_precision, c.data_scale, ' +
        ' (select count(*) from sys.all_indexes a, sys.all_ind_columns b' +
        '  where a.table_owner=c.owner and a.table_name=c.table_name and b.column_name=c.column_name' +
        '  and a.owner=b.index_owner and a.index_name=b.index_name and' +
        '  a.table_owner=b.table_owner and a.table_name=b.table_name) index_count' +
        ' from sys.all_tab_columns c' +
        ' where c.owner like ''%'' and c.table_name like ''%'';';
    dMSSQL, dMySQL, dPostgreSQL:
      FMT :=
        'select COLUMN_NAME, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH, NUMERIC_PRECISION,' +
        ' NUMERIC_SCALE, 0 INDEX_COUNT' + // INDEX_COUNT=0 here (done via OleDB)
        ' from INFORMATION_SCHEMA.COLUMNS' +
        ' where UPPER(TABLE_SCHEMA) = ''%'' and UPPER(TABLE_NAME) = ''%''';
    dFirebird:
      begin
        result := // see http://edn.embarcadero.com/article/25259
          'select a.rdb$field_name, b.rdb$field_type || coalesce(b.rdb$field_sub_type, '''') as rdb$field_type,'
          + ' b.rdb$field_length, b.rdb$field_length, abs(b.rdb$field_scale) as rdb$field_scale,'
          + ' (select count(*) from rdb$indices i, rdb$index_segments s' +
          ' where i.rdb$index_name=s.rdb$index_name and i.rdb$index_name not like ''RDB$%''' +
          ' and i.rdb$relation_name=a.rdb$relation_name) as index_count ' +
          'from rdb$relation_fields a left join rdb$fields b on a.rdb$field_source=b.rdb$field_name' +
          ' left join rdb$relations c on a.rdb$relation_name=c.rdb$relation_name ' +
          'where a.rdb$relation_name=''' + UpperCase(aTableName) + '''';
        exit;
      end;
    dNexusDB:
      begin
        result :=
          'select FIELD_NAME, FIELD_TYPE_SQL, FIELD_LENGTH, FIELD_UNITS,' +
          ' FIELD_DECIMALS, FIELD_INDEX from #fields where TABLE_NAME = '''
          + aTableName + '''';
        exit;
      end;
  else
    exit; // others (e.g. dDB2) will retrieve info from (ODBC) driver
  end;
  SQLSplitTableName(aTableName, Owner, Table);
  FormatUTF8(FMT, [UpperCase(Owner), UpperCase(Table)], result);
end;

function TSQLDBConnectionProperties.SQLGetIndex(const aTableName: RawUTF8): RawUTF8;
var
  Owner, Table: RawUTF8;
  FMT: RawUTF8;
begin
  result := '';
  case DBMS of
    dOracle:
      FMT :=
        'select  index_name, decode(max(uniqueness),''NONUNIQUE'',0,1) as is_unique, ' +
        ' max(index_type) as type_desc, 0 as is_primary_key, 0 as unique_constraint, ' +
        ' cast(null as varchar(100)) as index_filter, ' +
        ' ltrim(max(sys_connect_by_path(column_name, '', '')), '', '') as key_columns, ' +
        ' cast(null as varchar(100)) as included_columns ' + 'from ' +
        '( select c.index_name as index_name, i.index_type, i.uniqueness, c.column_name, ' +
        '   row_number() over (partition by c.table_name, c.index_name order by c.column_position) as rn ' +
        '  from user_ind_columns c inner join user_indexes i on c.table_name = i.table_name and c.index_name = i.index_name ' +
        '  where c.table_name = ''%'' ' +
        ') start with rn = 1 connect by prior rn = rn - 1 and prior index_name = index_name ' +
        'group by index_name order by index_name';
    dMSSQL:
      FMT :=
        'select i.name as index_name, i.is_unique, i.type_desc, is_primary_key, is_unique_constraint, ' +
        '  i.filter_definition as index_filter, key_columns, included_columns as included_columns, ' +
        '  t.name as table_name ' + 'from ' +
        ' sys.tables t inner join sys.indexes i on i.object_id = t.object_id ' +
        ' cross apply(select STUFF(' +
        '   (select '',''+c.name from sys.index_columns ic ' +
        '   inner join sys.columns c on c.object_id = t.object_id and c.column_id = ic.column_id ' +
        '   where i.index_id = ic.index_id and i.object_id = ic.object_id and ic.is_included_column = 0 ' +
        '   order by ic.key_ordinal for xml path('''') ' +
        '   ),1,1,'''') as key_columns) AS c ' + ' cross apply(select STUFF( ' +
        '   (select '','' + c.name  from sys.index_columns ic ' +
        '   inner join	sys.columns c on	c.object_id = t.object_id	and c.column_id = ic.column_id ' +
        '   where i.index_id = ic.index_id and i.object_id = ic.object_id and ic.is_included_column = 1 ' +
        '   order by ic.key_ordinal for xml path('''') ' +
        '   ),1,1,'''') as included_columns) AS ic ' +
        'where t.type = ''U'' and t.name like ''%''';
    dFirebird:
      FMT :=
        'select i.rdb$index_name, i.rdb$unique_flag, i.rdb$index_type, case rc.rdb$constraint_type ' +
        ' when ''PRIMARY KEY'' then 1 else 0 end as is_primary_key, 0 as unique_constraint, ' +
        ' null as index_filter, (select list(trim(rdb$field_name), '', '') from ' +
        '  (select * from rdb$index_segments where rdb$index_name = i.rdb$index_name ' +
        '  order by rdb$field_position)) as key_columns, null as included_columns ' +
        'from rdb$indices i ' +
        'left outer join rdb$relation_constraints rc on rc.rdb$index_name = i.rdb$index_name and ' +
        '  rc.rdb$constraint_type=''PRIMARY KEY'' ' +
        'where exists(select * from rdb$index_segments where rdb$index_name = i.rdb$index_name) and ' +
        ' i.rdb$relation_name = ''%''';
  else
    exit; // others (e.g. dMySQL or dDB2) will retrieve info from (ODBC) driver
  end;
  Split(aTableName, '.', Owner, Table);
  if Table = '' then
  begin
    Table := Owner;
    Owner := UserID;
  end;
  FormatUTF8(FMT, [UpperCase(Table)], result);
end;

function TSQLDBConnectionProperties.SQLGetParameter(const aProcName: RawUTF8): RawUTF8;
var
  Owner, package, Proc: RawUTF8;
  FMT: RawUTF8;
begin
  result := '';
  SQLSplitProcedureName(aProcName, Owner, package, Proc);
  case DBMS of
    dOracle:
      FMT :=
        'select a.argument_name, a.data_type, a.char_length, a.data_precision, a.data_scale, a.in_out ' +
        'from   sys.all_arguments a ' + 'where  a.owner like ''%''' +
        '  and  a.package_name like ''' + UpperCase(package) + '''' +
        '  and  a.object_name like ''%''' + ' order by position';
    dMSSQL, dMySQL, dPostgreSQL:
      FMT :=
        'select PARAMETER_NAME, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH, NUMERIC_PRECISION, NUMERIC_SCALE, PARAMETER_MODE ' +
        'from INFORMATION_SCHEMA.PARAMETERS ' +
        'where UPPER(SPECIFIC_SCHEMA) = ''%'' and UPPER(SPECIFIC_NAME) = ''%''' +
        ' order by ORDINAL_POSITION';
    dFirebird:
      begin
        if (package = '') then
          result :=
            'select a.rdb$parameter_name, b.rdb$field_type || coalesce(b.rdb$field_sub_type, '''') as rdb$field_type,' +
            ' b.rdb$field_length, b.rdb$field_precision, b.rdb$field_scale,' +
            ' case a.rdb$parameter_type when 0 then ''IN'' else ''OUT'' end ' +
            'from rdb$procedure_parameters a, rdb$fields b ' +
            'where b.rdb$field_name = a.rdb$field_source and a.rdb$procedure_name = ''' +
            UpperCase(Proc) + ''' ' + 'order by a.rdb$parameter_number'
        else
          result :=
            'select a.rdb$parameter_name, b.rdb$field_type || coalesce(b.rdb$field_sub_type, '''') as rdb$field_type,' +
            ' b.rdb$field_length, b.rdb$field_precision, b.rdb$field_scale,' +
            ' case a.rdb$parameter_type when 0 then ''IN'' else ''OUT'' end ' +
            'from rdb$procedure_parameters a, rdb$fields b ' +
            'where b.rdb$field_name = a.rdb$field_source and a.rdb$package_name = ''' +
            UpperCase(package) + ''' ' + '  and a.rdb$procedure_name = ''' +
            UpperCase(Proc) + ''' ' + 'order by a.rdb$parameter_number';
        exit;
      end;
    dNexusDB:
      begin // NOT TESTED !!!
        result :=
          'select PROCEDURE_ARGUMENT_NAME, PROCEDURE_ARGUMENT_TYPE, PROCEDURE_ARGUMENT_UNITS,' +
          ' PROCEDURE_ARGUMENT_UNITS, PROCEDURE_ARGUMENT_DECIMALS, PROCEDURE_ARGUMENT_KIND,' +
          ' from #procedure_arguments where PROCEDURE_NAME = ''' + aProcName +
          '''' + ' order by PROCEDURE_ARGUMENT_INDEX';
        exit;
      end;
  else
    exit; // others (e.g. dDB2) will retrieve info from (ODBC) driver
  end;
  FormatUTF8(FMT, [UpperCase(Owner), UpperCase(Proc)], result);
end;

function TSQLDBConnectionProperties.SQLGetProcedure: RawUTF8;
var
  FMT, Owner: RawUTF8;
begin
  result := '';
  case DBMS of
    dOracle:
      FMT := 'select' + '  case P.OBJECT_TYPE' +
        '  when ''PACKAGE'' then P.OBJECT_NAME || ''.'' || P.PROCEDURE_NAME' +
        '  else P.OBJECT_NAME end NAME_ROUTINE ' + 'from SYS.ALL_PROCEDURES P '
        + 'where P.OWNER = ''%'' and P.SUBPROGRAM_ID > 0 ' + 'order by NAME_ROUTINE';
    dMSSQL, dMySQL, dPostgreSQL:
      FMT := 'select R.SPECIFIC_NAME NAME_ROUTINE ' +
        'from INFORMATION_SCHEMA.ROUTINES R ' +
        'where UPPER(R.SPECIFIC_SCHEMA) = ''%'' ' + 'order by NAME_ROUTINE';
    dFirebird:
      FMT := 'select P.RDB$PROCEDURE_NAME NAME_ROUTINE ' +
        'from RDB$PROCEDURES P ' + 'where P.RDB$OWNER_NAME = ''%'' ' +
        'order by NAME_ROUTINE';
    dNexusDB:
      begin // NOT TESTED !!!
        result := 'select P.PROCEDURE_NAME NAME_ROUTINE ' +
          'from #PROCEDURES P ' + 'order by NAME_ROUTINE';
        exit;
      end;
  else
    exit; // others (e.g. dDB2) will retrieve info from (ODBC) driver
  end;
  SetSchemaNameToOwner(Owner);
  FormatUTF8(FMT, [UpperCase(Owner)], result);
end;

function TSQLDBConnectionProperties.SQLGetTableNames: RawUTF8;
begin
  case DBMS of
    dOracle:
      result := 'select owner||''.''||table_name name ' +
        'from sys.all_tables order by owner, table_name';
    dMSSQL:
      result := 'select (TABLE_SCHEMA + ''.'' + TABLE_NAME) as name ' +
        'from INFORMATION_SCHEMA.TABLES where TABLE_TYPE=''BASE TABLE'' order by name';
    dMySQL:
      result := 'select concat(TABLE_SCHEMA,''.'',TABLE_NAME) as name ' +
        'from INFORMATION_SCHEMA.TABLES where TABLE_TYPE=''BASE TABLE'' order by name';
    dPostgreSQL:
      result := 'select (TABLE_SCHEMA||''.''||TABLE_NAME) as name ' +
        'from INFORMATION_SCHEMA.TABLES where TABLE_TYPE=''BASE TABLE'' order by name';
    dSQLite:
      result := 'select name from sqlite_master where type=''table'' ' +
        'and name not like ''sqlite_%''';
    dFirebird:
      result := 'select rdb$relation_name from rdb$relations ' +
        'where rdb$view_blr is null and (rdb$system_flag is null or rdb$system_flag=0)';
    dNexusDB:
      result := 'select table_name name from #tables order by table_name';
  else
    result := ''; // others (e.g. dDB2) will retrieve info from (ODBC) driver
  end;
end;

function TSQLDBConnectionProperties.SQLGetViewNames: RawUTF8;
begin
  case DBMS of
    dOracle:
      result := 'select owner||''.''||view_name name ' +
        'from sys.all_views order by owner, view_name';
    dMSSQL:
      result := 'select (TABLE_SCHEMA + ''.'' + TABLE_NAME) as name ' +
        'from INFORMATION_SCHEMA.VIEWS order by name';
    dMySQL:
      result := 'select concat(TABLE_SCHEMA,''.'',TABLE_NAME) as name ' +
        'from INFORMATION_SCHEMA.VIEWS order by name';
    dPostgreSQL:
      result := 'select (TABLE_SCHEMA||''.''||TABLE_NAME) as name ' +
        'from INFORMATION_SCHEMA.VIEWS order by name';
    dSQLite:
      result := 'select name from sqlite_master where type=''view'' ' +
        'and name not like ''sqlite_%''';
    dFirebird:
      result := 'select rdb$relation_name from rdb$relations ' +
        'where rdb$view_blr is not null and (rdb$system_flag is null or rdb$system_flag=0)';
    dNexusDB:
      result := 'select view_name name from #views order by view_name'; // NOT TESTED !!!
  else
    result := ''; // others (e.g. dDB2) will retrieve info from (ODBC) driver
  end;
end;

function TSQLDBConnectionProperties.SQLCreateDatabase(const aDatabaseName:
  RawUTF8; aDefaultPageSize: integer): RawUTF8;
begin
  case DBMS of
    dFirebird:
      begin
        if (aDefaultPageSize <> 8192) or (aDefaultPageSize <> 16384) then
          aDefaultPageSize := 4096;
        FormatUTF8('create database ''%'' user ''sysdba'' password ''masterkey'''
          + ' page_size % default character set utf8;', [aDatabaseName,
          aDefaultPageSize], result);
      end;
  else
    result := '';
  end;
end;

function TSQLDBConnectionProperties.ColumnTypeNativeToDB(const aNativeType:
  RawUTF8; aScale: integer): TSQLDBFieldType;

  function ColumnTypeNativeDefault: TSQLDBFieldType;
  const
    DECIMAL = 18; // change it if you update PCHARS[] below before 'DECIMAL'
    NUMERIC = DECIMAL + 1;
    PCHARS: array[0..55] of PAnsiChar = (
      'TEXT COLLATE ISO8601', // should be before plain 'TEXT'
      'TEXT', 'CHAR', 'NCHAR', 'VARCHAR', 'NVARCHAR', 'CLOB', 'NCLOB', 'DBCLOB',
      'BIT', 'INT', 'BIGINT', 'DOUBLE', 'NUMBER', 'FLOAT', 'REAL', 'DECFLOAT',
      'CURR', 'DECIMAL', 'NUMERIC', 'BLOB SUB_TYPE 1', 'BLOB', 'DATE',
      'SMALLDATE', 'TIME', 'TINYINT', 'BOOL', 'SMALLINT', 'MEDIUMINT', 'SERIAL',
      'YEAR', 'TINYTEXT', 'MEDIUMTEXT', 'NTEXT', 'XML', 'ENUM', 'SET',
      'UNIQUEIDENTIFIER', 'MONEY', 'SMALLMONEY', 'NUM', 'VARRAW', 'RAW',
      'LONG RAW', 'LONG VARRAW', 'TINYBLOB', 'MEDIUMBLOB', 'BYTEA', 'VARBIN',
      'IMAGE', 'LONGBLOB', 'BINARY', 'VARBINARY', 'GRAPHIC', 'VARGRAPHIC', 'NULL');
    Types: array[-1..high(PCHARS)] of TSQLDBFieldType = (
      ftUnknown, ftDate,
      ftUTF8, ftUTF8, ftUTF8, ftUTF8, ftUTF8, ftUTF8, ftUTF8, ftUTF8, ftInt64,
      ftInt64, ftInt64, ftDouble, ftDouble, ftDouble, ftDouble, ftDouble,
      ftCurrency, ftCurrency, ftCurrency, ftUTF8, ftBlob, ftDate, ftDate, ftDate,
      ftInt64, ftInt64, ftInt64, ftInt64, ftInt64, ftInt64, ftUTF8, ftUTF8,
      ftUTF8, ftUTF8, ftUTF8, ftUTF8, ftUTF8, ftCurrency, ftCurrency, ftCurrency,
      ftBlob, ftBlob, ftBlob, ftBlob, ftBlob, ftBlob, ftBlob, ftBlob, ftBlob,
      ftBlob, ftBlob, ftBlob, ftBlob, ftBlob, ftNull);
  var
    ndx: integer;
  begin
    //assert(StrComp(PCHARS[DECIMAL],'DECIMAL')=0);
    ndx := IdemPCharArray(pointer(aNativeType), PCHARS);
    if (aScale = 0) and (ndx in [DECIMAL, NUMERIC]) then
      result := ftInt64
    else
      result := Types[ndx];
  end;

  function ColumnTypeNativeToDBOracle: TSQLDBFieldType;
  begin
    if PosEx('CHAR', aNativeType) > 0 then
      result := ftUTF8
    else if IdemPropNameU(aNativeType, 'NUMBER') then
      case aScale of
        0:
          result := ftInt64;
        1..4:
          result := ftCurrency;
      else
        result := ftDouble;
      end
    else if (PosEx('RAW', aNativeType) > 0) or
          IdemPropNameU(aNativeType, 'BLOB') or
          IdemPropNameU(aNativeType, 'BFILE') then
      result := ftBlob
    else if IdemPChar(pointer(aNativeType), 'BINARY_') or
            IdemPropNameU(aNativeType, 'FLOAT') then
      result := ftDouble
    else if IdemPropNameU(aNativeType, 'DATE') or IdemPChar(pointer(aNativeType),
      'TIMESTAMP') then
      result := ftDate
    else    // all other types will be converted to text
      result := ftUTF8;
  end;

  function ColumnTypeNativeToDBFirebird: TSQLDBFieldType;
  var
    i, err: integer;
  begin
    i := GetInteger(pointer(aNativeType), err);
    if err <> 0 then
      result := ColumnTypeNativeDefault
    else
      case i of // see blr_* definitions
        10, 11, 27:
          result := ftDouble;
        12, 13, 35, 120:
          result := ftDate;
        7, 8, 9, 16, 23, 70, 80, 160:
          result := ftInt64;
        161..169:
          case abs(aScale) of
            0:
              result := ftInt64;
            1..4:
              result := ftCurrency;
          else
            result := ftDouble;
          end;
        2610:
          result := ftBlob;
      else
        result := ftUTF8;
      end;
  end;

begin
  case DBMS of
    dOracle:
      result := ColumnTypeNativeToDBOracle;
    dFireBird:
      result := ColumnTypeNativeToDBFirebird;
  else
    result := ColumnTypeNativeDefault;
  end;
end;

function TSQLDBConnectionProperties.GetForeignKey(
  const aTableName, aColumnName: RawUTF8): RawUTF8;
begin
  if not fForeignKeys.Initialized then
  begin
    fForeignKeys.Init(false);
    GetForeignKeys;
  end;
  result := fForeignKeys.Value(aTableName + '.' + aColumnName);
end;

function TSQLDBConnectionProperties.GetForeignKeysData: RawByteString;
begin
  if not fForeignKeys.Initialized then
  begin
    fForeignKeys.Init(false);
    GetForeignKeys;
  end;
  result := fForeignKeys.BlobData;
end;

procedure TSQLDBConnectionProperties.SetForeignKeysData(const Value: RawByteString);
begin
  if not fForeignKeys.Initialized then
    fForeignKeys.Init(false);
  fForeignKeys.BlobData := Value;
end;

function TSQLDBConnectionProperties.SQLIso8601ToDate(const Iso8601: RawUTF8): RawUTF8;

  function TrimTInIso: RawUTF8;
  begin
    result := Iso8601;
    if (length(result) > 10) and (result[11] = 'T') then
      result[11] := ' '; // 'T' -> ' '
  end;

begin
  case DBMS of
    dSQLite:
      result := TrimTInIso;
    dOracle:
      result := 'to_date(''' + TrimTInIso + ''',''YYYY-MM-DD HH24:MI:SS'')';
    dNexusDB:
      result := 'DATE ' + Iso8601;
    dDB2:
      result := 'TIMESTAMP ''' + TrimTInIso + '''';
    dPostgreSQL:
      result := '''' + TrimTInIso + '''';
  else
    result := '''' + Iso8601 + '''';
  end;
end;

function TSQLDBConnectionProperties.SQLDateToIso8601Quoted(DateTime: TDateTime): RawUTF8;
begin
  result := DateTimeToIso8601(DateTime, true, DateTimeFirstChar, false, '''');
end;

function TSQLDBConnectionProperties.SQLCreate(const aTableName: RawUTF8;
  const aFields: TSQLDBColumnCreateDynArray; aAddID: boolean): RawUTF8;
var
  i: integer;
  F: RawUTF8;
  FieldID: TSQLDBColumnCreate;
  AddPrimaryKey: RawUTF8;
begin // use 'ID' instead of 'RowID' here since some DB (e.g. Oracle) use it
  result := '';
  if high(aFields) < 0 then
    exit; // nothing to create
  if aAddID then
  begin
    FieldID.DBType := ftInt64;
    FieldID.Name := 'ID';
    FieldID.Unique := true;
    FieldID.NonNullable := true;
    FieldID.PrimaryKey := true;
    result := SQLFieldCreate(FieldID, AddPrimaryKey) + ',';
  end;
  for i := 0 to high(aFields) do
  begin
    F := SQLFieldCreate(aFields[i], AddPrimaryKey);
    if i <> high(aFields) then
      F := F + ',';
    result := result + F;
  end;
  if AddPrimaryKey <> '' then
    result := result + ', PRIMARY KEY(' + AddPrimaryKey + ')';
  result := 'CREATE TABLE ' + aTableName + ' (' + result + ')';
  case DBMS of
    dDB2:
      result := result + ' CCSID Unicode';
  end;
end;

function TSQLDBConnectionProperties.SQLFieldCreate(const aField:
  TSQLDBColumnCreate; var aAddPrimaryKey: RawUTF8): RawUTF8;
begin
  if (aField.DBType = ftUTF8) and
     (cardinal(aField.Width - 1) < fSQLCreateFieldMax) then
    FormatUTF8(fSQLCreateField[ftNull], [aField.Width], result)
  else
    result := fSQLCreateField[aField.DBType];
  if aField.NonNullable or aField.Unique or aField.PrimaryKey then
    result := result + ' NOT NULL';
  if aField.Unique and not aField.PrimaryKey then
    result := result + ' UNIQUE'; // see http://www.w3schools.com/sql/sql_unique.asp
  if aField.PrimaryKey then
    case DBMS of
      dSQLite, dMSSQL, dOracle, dJet, dPostgreSQL, dFirebird, dNexusDB, dInformix:
        result := result + ' PRIMARY KEY';
      dDB2, dMySQL:
        aAddPrimaryKey := aField.Name;
    end;
  result := aField.Name + result;
end;

function TSQLDBConnectionProperties.SQLAddColumn(const aTableName: RawUTF8;
  const aField: TSQLDBColumnCreate): RawUTF8;
var
  AddPrimaryKey: RawUTF8;
begin
  FormatUTF8('ALTER TABLE % ADD %', [aTableName,
    SQLFieldCreate(aField, AddPrimaryKey)], result);
end;

function TSQLDBConnectionProperties.SQLAddIndex(const aTableName: RawUTF8;
  const aFieldNames: array of RawUTF8; aUnique, aDescending: boolean;
  const aIndexName: RawUTF8): RawUTF8;
const
  CREATNDXIFNE: array[boolean] of RawUTF8 = (
    '', 'IF NOT EXISTS ');
var
  IndexName, FieldsCSV, ColsDesc, Owner, Table: RawUTF8;
begin
  result := '';
  if (self = nil) or (aTableName = '') or (high(aFieldNames) < 0) then
    exit;
  if aUnique then
    result := 'UNIQUE ';
  if aIndexName = '' then
  begin
    SQLSplitTableName(aTableName, Owner, Table);
    if (Owner <> '') and
       not (fDBMS in [dMSSQL, dPostgreSQL, dMySQL, dFirebird, dDB2, dInformix]) then
      // some DB engines do not expect any schema in the index name
      IndexName := Owner + '.';
    FieldsCSV := RawUTF8ArrayToCSV(aFieldNames, '');
    if length(FieldsCSV) + length(Table) > 27 then
      // sounds like if some DB limit the identifier length to 32 chars
      IndexName := {%H-}IndexName + 'INDEX' + crc32cUTF8ToHex(Table) +
        crc32cUTF8ToHex(FieldsCSV)
    else
      IndexName := IndexName + 'NDX' + Table + FieldsCSV;
  end
  else
    IndexName := aIndexName;
  if aDescending then
    case DB_SQLDESENDINGINDEXPOS[DBMS] of
      posGlobalBefore:
        result := result + 'DESC ';
      posWithColumn:
        ColsDesc := RawUTF8ArrayToCSV(aFieldNames, ' DESC,') + ' DESC';
    end;
  if {%H-}ColsDesc = '' then
    ColsDesc := RawUTF8ArrayToCSV(aFieldNames, ',');
  result := FormatUTF8('CREATE %INDEX %% ON %(%)', [result,
    CREATNDXIFNE[DBMS in DB_HANDLECREATEINDEXIFNOTEXISTS],
    IndexName, aTableName, ColsDesc]);
end;

function TSQLDBConnectionProperties.SQLTableName(const aTableName: RawUTF8): RawUTF8;
var
  BeginQuoteChar, EndQuoteChar: RawUTF8;
  UseQuote: boolean;
begin
  BeginQuoteChar := '"';
  EndQuoteChar := '"';
  UseQuote := PosExChar(' ', aTableName) > 0;
  case fDBMS of
    dPostgresql:
      if PosExChar('.', aTableName) = 0 then
        UseQuote := true; // quote if not schema.identifier format
    dMySQL:
      begin
        BeginQuoteChar := '`';  // backtick/grave accent
        EndQuoteChar := '`';
      end;
    dJet:
      begin  // note: dMSSQL may SET IDENTIFIER ON to use doublequotes
        BeginQuoteChar := '[';
        EndQuoteChar := ']';
      end;
    dSQLite:
      begin
        if PosExChar('.', aTableName) > 0 then
          UseQuote := true;
        BeginQuoteChar := '`';  // backtick/grave accent
        EndQuoteChar := '`';
      end;
  end;
  if UseQuote and (PosEx(BeginQuoteChar, aTableName) = 0) then
    result := BeginQuoteChar + aTableName + EndQuoteChar
  else
    result := aTableName;
end;

procedure TSQLDBConnectionProperties.GetIndexesAndSetFieldsColumnIndexed(
  const aTableName: RawUTF8; var Fields: TSQLDBColumnDefineDynArray);
var
  i, j: integer;
  ColName: RawUTF8;
  Indexes: TSQLDBIndexDefineDynArray;
begin
  if Fields = nil then
    exit;
  GetIndexes(aTableName, Indexes);
  for i := 0 to high(Indexes) do
  begin
    ColName := Trim(GetCSVItem(pointer(Indexes[i].KeyColumns), 0));
    if ColName <> '' then
      for j := 0 to high(Fields) do
        if IdemPropNameU(Fields[j].ColumnName, ColName) then
        begin
          Fields[j].ColumnIndexed := true;
          break;
        end;
  end;
end;

function TSQLDBConnectionProperties.ExceptionIsAboutConnection(
  aClass: ExceptClass; const aMessage: RawUTF8): boolean;

  function PosErrorNumber(const aMessage: RawUTF8; aSepChar: AnsiChar): PUTF8Char;
  begin // search aSepChar followed by a number
    result := pointer(aMessage);
    repeat
      result := PosChar(result, aSepChar);
      if result = nil then
        exit;
      inc(result);
    until result^ in ['0'..'9'];
  end;

begin // see more complete list in feature request [f024266c0839]
  case fDBMS of
    dOracle:
      result := IdemPCharArray(PosErrorNumber(aMessage, '-'), ['00028', '01012',
        '01017', '01033', '01089', '02396', '03113', '03114', '03135', '12152',
        '12154', '12157', '12514', '12520', '12537', '12545', '12560', '12571']) >= 0;
    dInformix:
      // error codes based on {IBM INFORMIX ODBC DRIVER} tested with wrong data connection
      result := IdemPCharArray(PosErrorNumber(aMessage, '-'), ['329', '761',
        '902', '908', '930', '931', '951', '11017', '23101', '23104', '25567',
        '25582', '27002']) >= 0;
    dMSSQL:
      // error codes based on {SQL Server Native Client 11.0} tested with wrong
      // data connection using general error codes because MS SQL SERVER has
      // multiple error codes in the error message
      result := IdemPCharArray(PosErrorNumber(aMessage, '['),
        ['08001', '08S01', '08007', '28000', '42000']) >= 0;
    dMySQL:
      result := (PosEx('Lost connection to MySQL server', aMessage) > 0) or
                (PosEx('MySQL server has gone away', aMessage) > 0);
  else
    result := PosI(' CONNE', aMessage) > 0;
  end;
end;

procedure TSQLDBConnectionProperties.MultipleValuesInsert(
  Props: TSQLDBConnectionProperties; const TableName: RawUTF8;
  const FieldNames: TRawUTF8DynArray; const FieldTypes: TSQLDBFieldTypeArray;
  RowCount: integer; const FieldValues: TRawUTF8DynArrayDynArray);
var
  SQL: RawUTF8;
  SQLCached: boolean;
  prevrowcount: integer;
  maxf: integer;

  procedure ComputeSQL(rowcount, offset: integer);
  var
    f, r, p, len: integer;
    tmp: TTextWriterStackBuffer;
  begin
    if (fDBMS <> dFireBird) and (rowcount = prevrowcount) then
      exit;
    prevrowcount := rowcount;
    with TTextWriter.CreateOwnedStream(tmp) do
    try
      case Props.fDBMS of
        dFirebird:
          begin
            AddShort('execute block('#10);
            p := 0;
            for r := offset to offset + rowcount - 1 do
            begin
              for f := 0 to maxf do
              begin
                Add('i');
                inc(p);
                AddU(p);
                if FieldValues[f, r] = 'null' then
                  AddShort(' CHAR(1)')
                else
                  case FieldTypes[f] of
                    ftNull:
                      AddShort(' CHAR(1)');
                    ftUTF8:
                      begin
                        len := length(FieldValues[f, r]) - 2; // unquoted UTF-8 text length
                        if len < 1 then
                          len := 1;
                        AddShort(' VARCHAR(');
                        AddU(len);
                        AddShort(') CHARACTER SET UTF8');
                      end;
                  else
                    AddString(DB_FIELDS[dFirebird, FieldTypes[f]]);
                  end;
                AddShort('=?,');
              end;
              CancelLastComma;
              Add(#10, ',');
            end;
            CancelLastComma;
            AddShort(') as begin'#10);
            p := 0;
            for r := 1 to rowcount do
            begin
              AddShort('INSERT INTO ');
              AddString(TableName);
              Add(' ', '(');
              for f := 0 to maxf do
              begin
                AddString(FieldNames[f]);
                Add(',');
              end;
              CancelLastComma;
              AddShort(') VALUES (');
              for f := 0 to maxf do
              begin
                inc(p);
                Add(':', 'i');
                AddU(p);
                Add(',');
              end;
              CancelLastComma;
              AddShort(');'#10);
            end;
            AddShort('end');
            if TextLength > 32700 then
              raise ESQLDBException.CreateUTF8('%.MultipleValuesInsert: Firebird Execute Block length=%',
                [self, TextLength]);
            SQLCached := false; // ftUTF8 values will have varying field length
          end;
        dOracle:
          begin // INSERT ALL INTO ... VALUES ... SELECT 1 FROM DUAL
            AddShort('insert all'#10); // see http://stackoverflow.com/a/93724
            for r := 1 to rowcount do
            begin
              AddShort('into ');
              AddString(TableName);
              Add(' ', '(');
              for f := 0 to maxf do
              begin
                AddString(FieldNames[f]);
                Add(',');
              end;
              CancelLastComma;
              AddShort(') VALUES (');
              for f := 0 to maxf do
                Add('?', ',');
              CancelLastComma;
              AddShort(')'#10);
            end;
            AddShort('select 1 from dual');
            SQLCached := true;
          end;
      else
        begin //  e.g. NexusDB/SQlite3/MySQL/PostgreSQL/MSSQL2008/DB2/INFORMIX
          AddShort('INSERT INTO '); // INSERT .. VALUES (..),(..),(..),..
          AddString(TableName);
          Add(' ', '(');
          for f := 0 to maxf do
          begin
            AddString(FieldNames[f]);
            Add(',');
          end;
          CancelLastComma;
          AddShort(') VALUES');
          for r := 1 to rowcount do
          begin
            Add(' ', '(');
            for f := 0 to maxf do
              Add('?', ',');
            CancelLastComma;
            Add(')', ',');
          end;
          CancelLastComma;
          SQLCached := true;
        end;
      end;
      SetText(SQL);
    finally
      Free;
    end;
  end;

var
  batchRowCount, paramCountLimit: integer;
  currentRow, f, p, i, sqllen: integer;
  Stmt: TSQLDBStatement;
  Query: ISQLDBStatement;
begin
  maxf := length(FieldNames);     // e.g. 2 fields
  if (Props = nil) or (FieldNames = nil) or (TableName = '') or (length(FieldValues)
    <> maxf) then
    raise ESQLDBException.CreateUTF8('Invalid %.MultipleValuesInsert(%) call', [self,
      TableName]);
  batchRowCount := 0;
  paramCountLimit := 0;
  case Props.fDBMS of
    // values below were done empirically, assuring < 667 (maximum :AA..:ZZ)
    // see http://stackoverflow.com/a/6582902 for theoritical high limits
    dSQlite:
      paramCountLimit := 200;  // theoritical=999
    dMySQL:
      paramCountLimit := 500;  // theoritical=60000
    dPostgreSQL:
      paramCountLimit := 500;  // theoritical=34000
    dOracle:
      paramCountLimit := 500;  // empirical value (from ODBC)
    dMSSQL:
      paramCountLimit := 500;  // theoritical=2100
    dDB2:
      paramCountLimit := 500;  // empirical value (from ODBC)
    dNexusDB:
      paramCountLimit := 100;  // empirical limit (above is slower)
    dFirebird:
      begin // compute from max SQL statement size of 32KB
        sqllen := maxf * 48; // worse case (with BLOB param)
        for f := 0 to maxf - 1 do
          inc(sqllen, Length(FieldNames[f]));
        batchRowCount := 32000 div sqllen;
        if batchRowCount > RowCount then
          batchRowCount := RowCount;
      end;
  end;
  if paramCountLimit <> 0 then
    if RowCount * maxf > paramCountLimit then
      batchRowCount := paramCountLimit div maxf
    else
      batchRowCount := RowCount;
  if batchRowCount = 0 then
    raise ESQLDBException.CreateUTF8('%.MultipleValuesInsert(%) with # params = %>%',
      [self, TableName, RowCount * maxf, paramCountLimit]);
  dec(maxf);
  prevrowcount := 0;
  SQLCached := false;
  currentRow := 0;
  repeat
    if RowCount - currentRow > batchRowCount then
      ComputeSQL(batchRowCount, currentRow) // max number of params -> try cache
    else
    begin
      ComputeSQL(RowCount - currentRow, currentRow);
      SQLCached := false; // truncate number of parameters should not be unique
    end;
    if SQLCached then
      Query := Props.NewThreadSafeStatementPrepared(SQL, false)
    else
    begin
      Stmt := Props.NewThreadSafeStatement;
      try
        Stmt.Prepare(SQL, false);
        Query := Stmt; // Stmt will be released by Query := nil below
      except
        on Exception do
          Stmt.Free; // avoid memory leak in case of invalid SQL statement
      end; // exception leaves Query=nil to raise exception
    end;
    if Query = nil then
      raise ESQLDBException.CreateUTF8('%.MultipleValuesInsert: Query=nil for [%]',
        [self, SQL]);
    try
      p := 1;
      for i := 1 to prevrowcount do
      begin
        for f := 0 to maxf do
        begin
          Query.Bind(p, FieldTypes[f], FieldValues[f, currentRow], false);
          inc(p);
        end;
        inc(currentRow);
      end;
      Query.ExecutePrepared;
    finally
      Query := nil; // will release the uncached local Stmt, if applying
    end;
  until currentRow = RowCount;
end;

procedure TSQLDBConnectionProperties.MultipleValuesInsertFirebird(
  Props: TSQLDBConnectionProperties; const TableName: RawUTF8;
  const FieldNames: TRawUTF8DynArray; const FieldTypes: TSQLDBFieldTypeArray;
  RowCount: integer; const FieldValues: TRawUTF8DynArrayDynArray);
var
  W: TTextWriter;
  maxf, sqllenwitoutvalues, sqllen, r, f, i: PtrInt;
  v: RawUTF8;
begin
  maxf := length(FieldNames);     // e.g. 2 fields
  if (Props = nil) or (FieldNames = nil) or (TableName = '') or
     (length(FieldValues) <> maxf) or (Props.fDBMS <> dFirebird) then
    raise ESQLDBException.CreateUTF8('Invalid %.MultipleValuesInsertFirebird(%,%)',
      [self, Props, TableName]);
  sqllenwitoutvalues := 3 * maxf + 24;
  dec(maxf);
  for f := 0 to maxf do
    case FieldTypes[f] of
      ftBlob:
        begin // not possible to inline BLOBs -> fallback to regular
          MultipleValuesInsert(Props, TableName, FieldNames, FieldTypes,
            RowCount, FieldValues);
          exit;
        end;
      ftDate:
        inc(sqllenwitoutvalues, Length(FieldNames[f]) + 20); // 'timestamp '
    else
      inc(sqllenwitoutvalues, Length(FieldNames[f]));
    end;
  W := TTextWriter.CreateOwnedStream(49152);
  try
    r := 0;
    repeat
      W.AddShort('execute block as begin'#10);
      sqllen := sqllenwitoutvalues;
      repeat
        for f := 0 to maxf do
          inc(sqllen, length(FieldValues[f, r]));
        if sqllen + PtrInt(W.TextLength) > 30000 then
          break;
        W.AddShort('INSERT INTO ');
        W.AddString(TableName);
        W.Add(' ', '(');
        for f := 0 to maxf do
        begin
          W.AddString(FieldNames[f]);
          W.Add(',');
        end;
        W.CancelLastComma;
        W.AddShort(') VALUES (');
        for f := 0 to maxf do
        begin
          v := FieldValues[f, r]; // includes single quotes (#39)
          if (v = '') or (v = 'null') then
            W.AddShort('null')
          else if FieldTypes[f] = ftDate then
            if v = #39#39 then
              W.AddShort('null')
            else
            begin
              W.AddShort('timestamp ');
              if length(v) > 12 then
              begin // not 'CCYY-MM-DD' -> fix needed?
                if v[12] = 'T' then // handle 'CCYY-MM-DDTHH:MM:SS' common case
                  v[12] := ' '
                else
                begin
                  i := PosExChar('T', v);
                  if i > 0 then
                    v[i] := ' ';
                end; // see https://firebirdsql.org/en/firebird-date-literals
              end;
              W.AddString(v)
            end
          else
            W.AddString(v);
          W.Add(',');
        end;
        W.CancelLastComma;
        W.AddShort(');'#10);
        inc(r);
      until r = RowCount;
      W.AddShort('end');
      with Props.NewThreadSafeStatement do
        try
          Execute(W.Text, false);
        finally
          Free;
        end;
      if r = RowCount then
        break;
      W.CancelAll;
    until false;
  finally
    W.Free;
  end;
end;

function TSQLDBConnectionProperties.FieldsFromList(
  const aFields: TSQLDBColumnDefineDynArray; aExcludeTypes: TSQLDBFieldTypes): RawUTF8;
var
  i, n: integer;
begin
  result := '';
  if byte(aExcludeTypes) <> 0 then
  begin
    n := length(aFields);
    for i := 0 to n - 1 do
      with aFields[i] do
        if not (ColumnType in aExcludeTypes) then
        begin
          dec(n);
          if result = '' then
            result := ColumnName
          else
            result := result + ',' + ColumnName;
        end;
    if n = 0 then
      result := '*';
  end
  else
    result := '*';
end;

function TSQLDBConnectionProperties.SQLSelectAll(const aTableName: RawUTF8;
  const aFields: TSQLDBColumnDefineDynArray; aExcludeTypes: TSQLDBFieldTypes): RawUTF8;
begin
  if (self = nil) or (aTableName = '') then
    result := ''
  else
    result := 'select ' + FieldsFromList(aFields, aExcludeTypes) + ' from ' +
      SQLTableName(aTableName);
end;

class function TSQLDBConnectionProperties.EngineName: RawUTF8;
var
  L: integer;
begin
  if self = nil then
    result := ''
  else
  begin
    result := RawUTF8(ClassName);
    if IdemPChar(pointer(result), 'TSQLDB') then
      Delete(result, 1, 6)
    else if result[1] = 'T' then
      Delete(result, 1, 1);
    L := length(result);
    if (L > 20) and IdemPropName('ConnectionProperties', @result[L - 19], 20) then
      SetLength(result, L - 20);
    if (L > 5) and IdemPropName('OleDB', pointer(result), 5) then
      Delete(result, 1, 5);
  end;
end;

function TSQLDBConnectionProperties.GetDBMS: TSQLDBDefinition;
begin
  if fDBMS = dUnknown then
    result := dDefault
  else
    result := fDBMS;
end;

function TSQLDBConnectionProperties.GetDBMSName: RawUTF8;
var
  PS: PShortString;
begin
  PS := ToText(DBMS);
  FastSetString(result, @PS^[2], ord(PS^[0]) - 1);
end;

function TSQLDBConnectionProperties.GetDatabaseNameSafe: RawUTF8;
begin
  result := StringReplaceAll(fDatabaseName, PassWord, '***');
end;

function TSQLDBConnectionProperties.SQLLimitClause(AStmt: TSynTableStatement):
  TSQLDBDefinitionLimitClause;
begin
  result := DB_SQLLIMITCLAUSE[DBMS];
end;

var
  GlobalDefinitions: array of TSQLDBConnectionPropertiesClass;

class procedure TSQLDBConnectionProperties.RegisterClassNameForDefinition;
begin
  ObjArrayAddOnce(GlobalDefinitions, TObject(self)); // TClass stored as TObject
end;

procedure TSQLDBConnectionProperties.DefinitionTo(Definition: TSynConnectionDefinition);
begin
  if Definition = nil then
    exit;
  Definition.Kind := ClassName;
  Definition.ServerName := ServerName;
  Definition.DatabaseName := DatabaseName;
  Definition.User := UserID;
  Definition.PassWordPlain := PassWord;
end;

function TSQLDBConnectionProperties.DefinitionToJSON(Key: cardinal): RawUTF8;
var
  Definition: TSynConnectionDefinition;
begin
  Definition := TSynConnectionDefinition.Create;
  try
    Definition.Key := Key;
    DefinitionTo(Definition);
    result := Definition.SaveToJSON;
  finally
    Definition.Free;
  end;
end;

procedure TSQLDBConnectionProperties.DefinitionToFile(
  const aJSONFile: TFileName; Key: cardinal);
begin
  FileFromString(JSONReformat(DefinitionToJSON(Key)), aJSONFile);
end;

class function TSQLDBConnectionProperties.ClassFrom(
  aDefinition: TSynConnectionDefinition): TSQLDBConnectionPropertiesClass;
var
  ndx: integer;
begin
  for ndx := 0 to length(GlobalDefinitions) - 1 do
    if GlobalDefinitions[ndx].ClassNameIs(aDefinition.Kind) then
    begin
      result := GlobalDefinitions[ndx];
      exit;
    end;
  result := nil;
end;

class function TSQLDBConnectionProperties.CreateFrom(
  aDefinition: TSynConnectionDefinition): TSQLDBConnectionProperties;
var
  C: TSQLDBConnectionPropertiesClass;
begin
  C := ClassFrom(aDefinition);
  if C = nil then
    raise ESQLDBException.CreateUTF8('%.CreateFrom: unknown % class - please ' +
      'add a reference to its implementation unit', [self, aDefinition.Kind]);
  result := C.Create(aDefinition.ServerName, aDefinition.DatabaseName,
    aDefinition.User, aDefinition.PassWordPlain);
end;

class function TSQLDBConnectionProperties.CreateFromJSON(
  const aJSONDefinition: RawUTF8; aKey: cardinal): TSQLDBConnectionProperties;
var
  Definition: TSynConnectionDefinition;
begin
  Definition := TSynConnectionDefinition.CreateFromJSON(aJSONDefinition, aKey);
  try
    result := CreateFrom(Definition);
  finally
    Definition.Free;
  end;
end;

class function TSQLDBConnectionProperties.CreateFromFile(
  const aJSONFile: TFileName; aKey: cardinal): TSQLDBConnectionProperties;
begin
  result := CreateFromJSON(AnyTextFileToRawUTF8(aJSONFile, true), aKey);
end;


{ TSQLDBStatement }

procedure TSQLDBStatement.Bind(Param: Integer; const Data: TSQLVar;
  IO: TSQLDBParamInOutType);
begin
  with Data do
    case VType of
      ftNull:
        BindNull(Param, IO);
      ftInt64:
        Bind(Param, VInt64, IO);
      ftDate:
        BindDateTime(Param, VDateTime, IO);
      ftDouble:
        Bind(Param, VDouble, IO);
      ftCurrency:
        BindCurrency(Param, VCurrency, IO);
      ftUTF8:
        BindTextP(Param, VText, IO);
      ftBlob:
        BindBlob(Param, VBlob, VBlobLen, IO);
    else
      raise ESQLDBException.CreateUTF8('%.Bind(Param=%,VType=%)', [self, Param,
        ord(VType)]);
    end;
end;

procedure TSQLDBStatement.Bind(Param: Integer; ParamType: TSQLDBFieldType;
  const Value: RawUTF8; ValueAlreadyUnquoted: boolean; IO: TSQLDBParamInOutType);
var
  tmp: RawUTF8;
begin
  if not ValueAlreadyUnquoted and (Value = 'null') then
    // bind null (ftUTF8 should be '"null"')
    BindNull(Param, IO)
  else
    case ParamType of
      ftNull:
        BindNull(Param, IO);
      ftInt64:
        Bind(Param, GetInt64(pointer(Value)), IO);
      ftDouble:
        Bind(Param, GetExtended(pointer(Value)), IO);
      ftCurrency:
        BindCurrency(Param, StrToCurrency(pointer(Value)), IO);
      ftBlob:
        BindBlob(Param, Value, IO); // already decoded
      ftDate:
        begin
          if ValueAlreadyUnquoted then
            tmp := Value
          else
            UnQuoteSQLStringVar(pointer(Value), tmp);
          BindDateTime(Param, Iso8601ToDateTime(tmp), IO);
        end;
      ftUTF8:
        if (fConnection <> nil) and fConnection.fProperties.StoreVoidStringAsNull and
          ((Value = '') or // check if '' or '""' should be stored as null
           ((PInteger(Value)^ and $ffffff = $2727) and not ValueAlreadyUnquoted)) then
          BindNull(Param, IO, ftUTF8)
        else
        begin
          if ValueAlreadyUnquoted then
            tmp := Value
          else
            UnQuoteSQLStringVar(pointer(Value), tmp);
          BindTextU(Param, tmp, IO);
        end;
    else
      raise ESQLDBException.CreateUTF8('Invalid %.Bind(%,TSQLDBFieldType(%),%)',
        [self, Param, ord(ParamType), Value]);
    end;
end;

function VariantIsBlob(const V: variant): boolean;
begin
  with TVarData(V) do
    result := (VType = varNull) or ((VType = varString) and (VString <> nil) and
      (PCardinal(VString)^ and $ffffff = JSON_BASE64_MAGIC));
end;

procedure TSQLDBStatement.Bind(const Params: array of const; IO: TSQLDBParamInOutType);
var
  i, c: integer;
begin
  for i := 1 to high(Params) + 1 do
    with Params[i - 1] do // bind parameter index starts at 1
      case VType of
        vtString:     // expect WinAnsi String for ShortString
          BindTextU(i, WinAnsiToUtf8(@VString^[1], ord(VString^[0])), IO);
        vtAnsiString:
          if VAnsiString = nil then
            BindTextU(i, '', IO)
          else
          begin
            c := PInteger(VAnsiString)^ and $00ffffff;
            if c = JSON_BASE64_MAGIC then
              BindBlob(i, Base64ToBin(PAnsiChar(VAnsiString) + 3, length(RawUTF8
                (VAnsiString)) - 3))
            else if c = JSON_SQLDATE_MAGIC then
              BindDateTime(i, Iso8601ToDateTimePUTF8Char(PUTF8Char(VAnsiString)
                + 3, length(RawUTF8(VAnsiString)) - 3))
            else          // expect UTF-8 content only for AnsiString, i.e. RawUTF8 variables
          {$ifdef HASCODEPAGE}
              BindTextU(i, AnyAnsiToUTF8(RawByteString(VAnsiString)), IO);
          {$else}
              BindTextU(i, RawUTF8(VAnsiString), IO);
          {$endif}
          end;
        vtPChar:
          BindTextP(i, PUTF8Char(VPChar), IO);
        vtChar:
          BindTextU(i, RawUTF8(VChar), IO);
        vtWideChar:
          BindTextU(i, RawUnicodeToUtf8(@VWideChar, 1), IO);
        vtPWideChar:
          BindTextU(i, RawUnicodeToUtf8(VPWideChar, StrLenW(VPWideChar)), IO);
        vtWideString:
          BindTextW(i, WideString(VWideString), IO);
    {$ifdef HASVARUSTRING}
    {$ifdef UNICODE}
        vtUnicodeString:
          BindTextS(i, string(VUnicodeString), IO);
    {$else}
        vtUnicodeString:
          BindTextU(i, UnicodeStringToUtf8(UnicodeString(VUnicodeString)), IO);
    {$endif}
    {$endif}
        vtboolean:
          Bind(i, integer(Vboolean), IO);
        vtInteger:
          Bind(i, VInteger, IO);
        vtInt64:
          Bind(i, VInt64^, IO);
    {$ifdef FPC}
        vtQWord:
          Bind(i, VQWord^, IO);
    {$endif}
        vtCurrency:
          BindCurrency(i, VCurrency^, IO);
        vtExtended:
          Bind(i, VExtended^, IO);
        vtPointer:
          if VPointer = nil then
            BindNull(i, IO)
          else
            raise ESQLDBException.CreateUTF8('Unexpected %.Bind() pointer', [self]);
        vtVariant:
          BindVariant(i, VVariant^, VariantIsBlob(VVariant^), IO);
      else
        raise ESQLDBException.CreateUTF8('%.BindArrayOfConst(Param=%,Type=%)', [self,
          i, VType]);
      end;
end;

procedure TSQLDBStatement.BindVariant(Param: Integer; const Data: Variant;
  DataIsBlob: boolean; IO: TSQLDBParamInOutType);
var
  I64: Int64Rec;
begin
  with TVarData(Data) do
    case VType of
      varNull:
        BindNull(Param, IO);
      varboolean:
        if Vboolean then
          Bind(Param, 1, IO)
        else
          Bind(Param, 0, IO);
      varByte:
        Bind(Param, VInteger, IO);
      varSmallint:
        Bind(Param, VSmallInt, IO);
      varShortInt:
        Bind(Param, VShortInt, IO);
      varWord:
        Bind(Param, VWord, IO);
      varLongWord:
        begin
          I64.Lo := VLongWord;
          I64.Hi := 0;
          Bind(Param, Int64(I64), IO);
        end;
      varInteger:
        Bind(Param, VInteger, IO);
      varInt64, varWord64:
        Bind(Param, VInt64, IO);
      varSingle:
        Bind(Param, VSingle, IO);
      varDouble:
        Bind(Param, VDouble, IO);
      varDate:
        BindDateTime(Param, VDate, IO);
      varCurrency:
        BindCurrency(Param, VCurrency, IO);
      varOleStr: // handle special case if was bound explicitely as WideString
        BindTextW(Param, WideString(VAny), IO);
      {$ifdef HASVARUSTRING}
      varUString:
        if DataIsBlob then
          raise ESQLDBException.CreateUTF8(
            '%.BindVariant: BLOB should not be UnicodeString', [self])
        else
          BindTextU(Param, UnicodeStringToUtf8(UnicodeString(VAny)), IO);
      {$endif}
      varString:
        if DataIsBlob then
          if (VAny <> nil) and (PInteger(VAny)^ and $00ffffff = JSON_BASE64_MAGIC) then
            // recognized as Base64 encoded text
            BindBlob(Param, Base64ToBin(PAnsiChar(VAny) + 3,
              length(RawByteString(VAny)) - 3))
          else
            // no conversion if was set via TQuery.AsBlob property e.g.
            BindBlob(Param, RawByteString(VAny), IO)
        else
          // direct bind of AnsiString as UTF-8 value
          {$ifdef HASCODEPAGE}
          BindTextU(Param, AnyAnsiToUTF8(RawByteString(VAny)), IO);
          {$else} // on older Delphi, we assume AnsiString = RawUTF8
          BindTextU(Param, RawUTF8(VAny), IO);
          {$endif}
    else
      if VType = varByRef or varVariant then
        BindVariant(Param, PVariant(VPointer)^, DataIsBlob, IO)
      else if VType = varByRef or varOleStr then
        BindTextW(Param, PWideString(VAny)^, IO)
      else
        // also use TEXT for any non native VType parameter
        BindTextU(Param, VariantToUTF8(Data), IO);
    end;
end;

procedure TSQLDBStatement.BindArray(Param: Integer; ParamType: TSQLDBFieldType;
  const Values: TRawUTF8DynArray; ValuesCount: integer);
begin
  if (Param <= 0) or (ParamType in [ftUnknown, ftNull]) or (ValuesCount <= 0) or
    (length(Values) < ValuesCount) or (fConnection = nil) or
    (fConnection.fProperties.BatchSendingAbilities * [cCreate, cUpdate, cDelete] = []) then
    raise ESQLDBException.CreateUTF8('Invalid call to %.BindArray(Param=%,Type=%)',
      [self, Param, ToText(ParamType)^]);
end;

procedure TSQLDBStatement.BindArray(Param: Integer; const Values: array of Int64);
begin
  BindArray(Param, ftInt64, nil, 0); // will raise an exception (Values=nil)
end;

procedure TSQLDBStatement.BindArray(Param: Integer; const Values: array of RawUTF8);
begin
  BindArray(Param, ftUTF8, nil, 0); // will raise an exception (Values=nil)
end;

procedure TSQLDBStatement.BindArray(Param: Integer; const Values: array of double);
begin
  BindArray(Param, ftDouble, nil, 0); // will raise an exception (Values=nil)
end;

procedure TSQLDBStatement.BindArrayCurrency(Param: Integer;
  const Values: array of currency);
begin
  BindArray(Param, ftCurrency, nil, 0); // will raise an exception (Values=nil)
end;

procedure TSQLDBStatement.BindArrayDateTime(Param: Integer;
  const Values: array of TDateTime);
begin
  BindArray(Param, ftDate, nil, 0); // will raise an exception (Values=nil)
end;

procedure TSQLDBStatement.CheckCol(Col: integer);
begin
  if (self = nil) or (cardinal(Col) >= cardinal(fColumnCount)) then
    raise ESQLDBException.CreateUTF8('Invalid call to %.Column*(Col=%)', [self, Col]);
end;

function TSQLDBStatement.GetForceBlobAsNull: boolean;
begin
  result := fForceBlobAsNull;
end;

procedure TSQLDBStatement.SetForceBlobAsNull(value: boolean);
begin
  fForceBlobAsNull := value;
end;

function TSQLDBStatement.GetForceDateWithMS: boolean;
begin
  result := fForceDateWithMS;
end;

procedure TSQLDBStatement.SetForceDateWithMS(value: boolean);
begin
  fForceDateWithMS := value;
end;

constructor TSQLDBStatement.Create(aConnection: TSQLDBConnection);
begin
  inherited Create;
  fConnection := aConnection;
  fStripSemicolon := true;
  fCacheIndex := -1;
  if aConnection <> nil then
    fDBMS := aConnection.fProperties.DBMS;
end;

function TSQLDBStatement.ColumnCount: integer;
begin
  if self = nil then
    result := 0
  else
    result := fColumnCount;
end;

function TSQLDBStatement.ColumnBlobBytes(Col: integer): TBytes;
begin
  RawByteStringToBytes(ColumnBlob(Col), result);
end;

procedure TSQLDBStatement.ColumnBlobToStream(Col: integer; Stream: TStream);
var
  tmp: RawByteString;
begin
  tmp := ColumnBlob(Col); // default implementation
  Stream.WriteBuffer(pointer(tmp)^, Length(tmp));
end;

procedure TSQLDBStatement.ColumnBlobFromStream(Col: integer; Stream: TStream);
begin
  raise ESQLDBException.CreateUTF8('%.ColumnBlobFromStream not implemented', [self]);
end;

function TSQLDBStatement.ColumnVariant(Col: integer): Variant;
begin
  ColumnToVariant(Col, result);
end;

function TSQLDBStatement.ColumnToVariant(Col: integer; var Value: Variant):
  TSQLDBFieldType;
var
  tmp: RawByteString;
  V: TSQLVar;
begin
  ColumnToSQLVar(Col, V, tmp);
  result := V.VType;
  VarClear(Value);
  with TVarData(Value) do
  begin
    VType := MAP_FIELDTYPE2VARTYPE[V.VType];
    case result of
      ftNull:
        ; // do nothing
      ftInt64:
        VInt64 := V.VInt64;
      ftDouble:
        VDouble := V.VDouble;
      ftDate:
        VDate := V.VDateTime;
      ftCurrency:
        VCurrency := V.VCurrency;
      ftBlob:
        begin
          VAny := nil;
          if V.VBlob <> nil then
            if V.VBlob = pointer(tmp) then
              RawByteString(VAny) := tmp
            else
              SetString(RawByteString(VAny), PAnsiChar(V.VBlob), V.VBlobLen);
        end;
      ftUTF8:
        begin
          VAny := nil; // avoid GPF below
          if V.VText <> nil then
          begin
            if V.VText = pointer(tmp) then
              V.VBlobLen := length(tmp)
            else
              V.VBlobLen := StrLen(V.VText);
          {$ifndef UNICODE}
            if (fConnection <> nil) and
               not fConnection.Properties.VariantStringAsWideString then
            begin
              VType := varString;
              if (CurrentAnsiConvert.CodePage = CP_UTF8) and
                 (V.VText = pointer(tmp)) then
                RawByteString(VAny) := tmp
              else
                CurrentAnsiConvert.UTF8BufferToAnsi(
                  V.VText, V.VBlobLen, RawByteString(VAny));
            end
            else
          {$endif UNICODE}
              UTF8ToSynUnicode(V.VText, V.VBlobLen, SynUnicode(VAny));
          end
          else
            VType := varString; // avoid obscure "Invalid variant type" in FPC
        end;
    else
      raise ESQLDBException.CreateUTF8('%.ColumnToVariant: Invalid ColumnType(%)=%',
        [self, Col, ord(result)]);
    end;
  end;
end;

function TSQLDBStatement.ColumnTimestamp(Col: integer): TTimeLog;
begin
  case ColumnType(Col) of // will call GetCol() to check Col
    ftNull:
      result := 0;
    ftInt64:
      result := ColumnInt(Col);
    ftDate:
      PTimeLogBits(@result)^.From(ColumnDateTime(Col));
  else
    PTimeLogBits(@result)^.From(Trim(ColumnUTF8(Col)));
  end;
end;

function TSQLDBStatement.ColumnTimestamp(const ColName: RawUTF8): TTimeLog;
begin
  result := ColumnTimestamp(ColumnIndex(ColName));
end;

procedure TSQLDBStatement.ColumnsToJSON(WR: TJSONWriter);
var
  col: integer;
  blob: RawByteString;
begin
  if WR.Expand then
    WR.Add('{');
  for col := 0 to fColumnCount - 1 do
  begin
    if WR.Expand then
      WR.AddFieldName(ColumnName(col)); // add '"ColumnName":'
    if ColumnNull(col) then
      WR.AddShort('null')
    else
      case ColumnType(col) of
        ftNull:
          WR.AddShort('null');
        ftInt64:
          WR.Add(ColumnInt(col));
        ftDouble:
          WR.AddDouble(ColumnDouble(col));
        ftCurrency:
          WR.AddCurr64(ColumnCurrency(col));
        ftDate:
          begin
            WR.Add('"');
            WR.AddDateTime(ColumnDateTime(col), fForceDateWithMS);
            WR.Add('"');
          end;
        ftUTF8:
          begin
            WR.Add('"');
            WR.AddJSONEscape(pointer(ColumnUTF8(col)));
            WR.Add('"');
          end;
        ftBlob:
          if fForceBlobAsNull then
            WR.AddShort('null')
          else
          begin
            blob := ColumnBlob(col);
            WR.WrBase64(pointer(blob), length(blob), {withMagic=}true);
          end;
      else
        raise ESQLDBException.CreateUTF8('%.ColumnsToJSON: invalid ColumnType(%)=%',
          [self, col, ord(ColumnType(col))]);
      end;
    WR.Add(',');
  end;
  WR.CancelLastComma; // cancel last ','
  if WR.Expand then
    WR.Add('}');
end;

procedure TSQLDBStatement.ColumnToSQLVar(Col: Integer; var Value: TSQLVar;
  var Temp: RawByteString);
begin
  Value.Options := [];
  if ColumnNull(Col) then // will call GetCol() to check Col
    Value.VType := ftNull
  else
    Value.VType := ColumnType(Col);
  case Value.VType of
    ftInt64:
      Value.VInt64 := ColumnInt(Col);
    ftDouble:
      Value.VDouble := ColumnDouble(Col);
    ftDate:
      Value.VDateTime := ColumnDateTime(Col);
    ftCurrency:
      Value.VCurrency := ColumnCurrency(Col);
    ftUTF8:
      begin
        Temp := ColumnUTF8(Col);
        Value.VText := pointer(Temp);
      end;
    ftBlob:
      if fForceBlobAsNull then
      begin
        Value.VBlob := nil;
        Value.VBlobLen := 0;
        Value.VType := ftNull;
      end
      else
      begin
        Temp := ColumnBlob(Col);
        Value.VBlob := pointer(Temp);
        Value.VBlobLen := length(Temp);
      end;
  end;
end;

function TSQLDBStatement.ColumnToTypedValue(Col: integer;
  DestType: TSQLDBFieldType; var Dest): TSQLDBFieldType;
var
  Temp: Variant; // rely on a temporary variant value for the conversion
begin
  result := ColumnToVariant(Col, Temp);
  case DestType of
    ftInt64:
      Int64(Dest) := Temp;
    ftDouble:
      Double(Dest) := Temp;
    ftCurrency:
      currency(Dest) := Temp;
    ftDate:
      TDateTime(Dest) := Temp;
    ftUTF8:
      VariantToUTF8(Temp, RawUTF8(Dest));
    ftBlob:
      VariantToRawByteString(Temp, RawByteString(Dest));
  else
    raise ESQLDBException.CreateUTF8('%.ColumnToTypedValue: Invalid Type [%]', [self,
      ToText(result)^]);
  end;
end;

function TSQLDBStatement.ParamToVariant(Param: Integer; var Value: Variant;
  CheckIsOutParameter: boolean = true): TSQLDBFieldType;
begin
  dec(Param); // start at #1
  if (self = nil) or (cardinal(Param) >= cardinal(fParamCount)) then
    raise ESQLDBException.CreateUTF8('%.ParamToVariant(%)', [self, Param]);
  // overridden method should fill Value with proper data
  result := ftUnknown;
end;

procedure TSQLDBStatement.Execute(const aSQL: RawUTF8; ExpectResults: boolean);
begin
  Connection.InternalProcess(speActive);
  try
    Prepare(aSQL, ExpectResults);
    SetForceBlobAsNull(true);
    ExecutePrepared;
  finally
    Connection.InternalProcess(speNonActive);
  end;
end;

function TSQLDBStatement.FetchAllToJSON(JSON: TStream; Expanded: boolean): PtrInt;
var
  W: TJSONWriter;
  col: integer;
  tmp: TTextWriterStackBuffer;
begin
  result := 0;
  W := TJSONWriter.Create(JSON, Expanded, false, nil, 0, @tmp);
  try
    Connection.InternalProcess(speActive);
    // get col names and types
    SetLength(W.ColNames, ColumnCount);
    for col := 0 to ColumnCount - 1 do
      W.ColNames[col] := ColumnName(col);
    W.AddColumns; // write or init field names for appropriate JSON Expand
    if Expanded then
      W.Add('[');
    // write rows data
    {$ifdef SYNDB_SILENCE}
    fSQLLogTimer.Resume; // log fetch duration
    {$endif}
    while Step do
    begin
      ColumnsToJSON(W);
      W.Add(',');
      inc(result);
    end;
    {$ifdef SYNDB_SILENCE}
    fSQLLogTimer.Pause;
    {$endif}
    ReleaseRows;
    if (result = 0) and W.Expand then
    begin
      // we want the field names at least, even with no data (RowCount=0)
      W.Expand := false; //  {"FieldCount":2,"Values":["col1","col2"]}
      W.CancelAll;
      for col := 0 to ColumnCount - 1 do
        W.ColNames[col] := ColumnName(col); // previous W.AddColumns did add ""
      W.AddColumns;
    end;
    W.EndJSONObject(0, result);
  finally
    W.Free;
    Connection.InternalProcess(speNonActive);
  end;
end;

function TSQLDBStatement.FetchAllToCSVValues(Dest: TStream; Tab: boolean;
  CommaSep: AnsiChar; AddBOM: boolean): PtrInt;
const
  NULL: array[boolean] of string[7] = (
    '"null"', 'null');
  blob: array[boolean] of string[7] = (
    '"blob"', 'blob');
var
  F, FMax: integer;
  W: TTextWriter;
  tmp: RawByteString;
  V: TSQLVar;
begin
  result := 0;
  if (Dest = nil) or (self = nil) or (ColumnCount = 0) then
    exit;
  fForceBlobAsNull := true;
  if Tab then
    CommaSep := #9;
  FMax := ColumnCount - 1;
  W := TTextWriter.Create(Dest, 65536);
  try
    if AddBOM then
      W.AddShort(#$ef#$bb#$bf); // add UTF-8 Byte Order Mark
    // add CSV header
    for F := 0 to FMax do
    begin
      if not Tab then
        W.Add('"');
      W.AddString(ColumnName(F));
      if Tab then
        W.Add(#9)
      else
        W.Add('"', CommaSep);
    end;
    W.CancelLastChar;
    W.AddCR;
    // add CSV rows
    {$ifdef SYNDB_SILENCE}
    fSQLLogTimer.Resume;
    {$endif}
    while Step do
    begin
      for F := 0 to FMax do
      begin
        ColumnToSQLVar(F, V, tmp);
        case V.VType of
          ftNull:
            W.AddShort(NULL[Tab]);
          ftInt64:
            W.Add(V.VInt64);
          ftDouble:
            W.AddDouble(V.VDouble);
          ftCurrency:
            W.AddCurr64(V.VCurrency);
          ftDate:
            begin
              if not Tab then
                W.Add('"');
              W.AddDateTime(V.VDateTime, svoDateWithMS in V.Options);
              if not Tab then
                W.Add('"');
            end;
          ftUTF8:
            begin
              if not Tab then
              begin
                W.Add('"');
                W.AddJSONEscape(V.VText);
                W.Add('"');
              end
              else
                W.AddNoJSONEscape(V.VText);
            end;
          ftBlob:
            W.AddShort(blob[Tab]);  // ForceBlobAsNull should be true
        else
          raise ESQLDBException.CreateUTF8('%.FetchAllToCSVValues: Invalid ColumnType(%) %',
            [self, F, ToText(ColumnType(F))^]);
        end;
        if F = FMax then
          W.AddCR
        else
          W.Add(CommaSep);
      end;
      inc(result);
    end;
    {$ifdef SYNDB_SILENCE}
    fSQLLogTimer.Pause;
    {$endif}
    ReleaseRows;
    W.FlushFinal;
  finally
    W.Free;
  end;
end;

function TSQLDBStatement.FetchAllAsJSON(Expanded: boolean; ReturnedRowCount:
  PPtrInt): RawUTF8;
var
  Stream: TRawByteStringStream;
  RowCount: PtrInt;
begin
  Stream := TRawByteStringStream.Create;
  try
    RowCount := FetchAllToJSON(Stream, Expanded);
    if ReturnedRowCount <> nil then
      ReturnedRowCount^ := RowCount;
    result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

procedure TSQLDBStatement.ColumnsToBinary(W: TFileBufferWriter; Null: pointer;
  const ColTypes: TSQLDBFieldTypeDynArray);
var
  F: integer;
  VDouble: double;
  VCurrency: currency absolute VDouble;
  VDateTime: TDateTime absolute VDouble;
  ft: TSQLDBFieldType;
begin
  for F := 0 to length(ColTypes) - 1 do
    if not GetBitPtr(Null, F) then
    begin
      ft := ColTypes[F];
      if ft < ftInt64 then
      begin // ftUnknown,ftNull
        ft := ColumnType(F); // per-row column type (SQLite3 only)
        W.Write1(ord(ft));
      end;
      case ft of
        ftInt64:
          W.WriteVarInt64(ColumnInt(F));
        ftDouble:
          begin
            VDouble := ColumnDouble(F);
            W.Write(@VDouble, sizeof(VDouble));
          end;
        ftCurrency:
          begin
            VCurrency := ColumnCurrency(F);
            W.Write(@VCurrency, sizeof(VCurrency));
          end;
        ftDate:
          begin
            VDateTime := ColumnDateTime(F);
            W.Write(@VDateTime, sizeof(VDateTime));
          end;
        ftUTF8:
          W.Write(ColumnUTF8(F));
        ftBlob:
          W.Write(ColumnBlob(F));
      else
        raise ESQLDBException.CreateUTF8('%.ColumnsToBinary: Invalid ColumnType(%)=%',
          [self, ColumnName(F), ord(ft)]);
      end;
    end;
end;

function TSQLDBStatement.FetchAllToBinary(Dest: TStream; MaxRowCount: cardinal;
  DataRowPosition: PCardinalDynArray): cardinal;
var
  F, FMax, FieldSize, NullRowSize: integer;
  StartPos: Int64;
  W: TFileBufferWriter;
  ft: TSQLDBFieldType;
  ColTypes: TSQLDBFieldTypeDynArray;
  Null: TByteDynArray;
begin
  result := 0;
  W := TFileBufferWriter.Create(Dest);
  try
    W.WriteVarUInt32(FETCHALLTOBINARY_MAGIC);
    FMax := ColumnCount;
    W.WriteVarUInt32(FMax);
    if FMax > 0 then
    begin
      // write column description
      SetLength(ColTypes, FMax);
      dec(FMax);
      for F := 0 to FMax do
      begin
        W.Write(ColumnName(F));
        ft := ColumnType(F, @FieldSize);
        if (ft = ftUnknown) and (currentRow = 0) and Step then
          ft := ColumnType(F, @FieldSize); // e.g. SQLite3 -> fetch and guess
        ColTypes[F] := ft;
        W.Write1(ord(ft));
        W.WriteVarUInt32(FieldSize);
      end;
      // initialize null handling
      SetLength(Null, (FMax shr 3) + 1);
      NullRowSize := 0;
      // save all data rows
      StartPos := W.TotalWritten;
      if (currentRow = 1) or Step then // Step may already be done (e.g. TQuery.Open)
        repeat
          // save row position in DataRowPosition[] (if any)
          if DataRowPosition <> nil then
          begin
            if Length(DataRowPosition^) <= integer(result) then
              SetLength(DataRowPosition^, NextGrow(result));
            DataRowPosition^[result] := W.TotalWritten - StartPos;
          end;
          // first write null columns flags
          if NullRowSize > 0 then
          begin
            FillCharFast(Null[0], NullRowSize, 0);
            NullRowSize := 0;
          end;
          for F := 0 to FMax do
            if ColumnNull(F) then
            begin
              SetBitPtr(pointer(Null), F);
              NullRowSize := (F shr 3) + 1;
            end;
          if NullRowSize > 0 then
          begin
            W.WriteVarUInt32(NullRowSize);
            W.Write(pointer(Null), NullRowSize);
          end
          else
            W.Write1(0); // = W.WriteVarUInt32(0)
          // then write data values
          ColumnsToBinary(W, pointer(Null), ColTypes);
          inc(result);
          if (MaxRowCount > 0) and (result >= MaxRowCount) then
            break;
        until not Step;
      ReleaseRows;
    end;
    W.Write(@result, SizeOf(result)); // fixed size at the end for row count
    W.Flush;
  finally
    W.Free;
  end;
end;

procedure TSQLDBStatement.Execute(const aSQL: RawUTF8; ExpectResults: boolean;
  const Params: array of const);
begin
  Connection.InternalProcess(speActive);
  try
    Prepare(aSQL, ExpectResults);
    Bind(Params);
    ExecutePrepared;
  finally
    Connection.InternalProcess(speNonActive);
  end;
end;

procedure TSQLDBStatement.Execute(const SQLFormat: RawUTF8;
  ExpectResults: boolean; const Args, Params: array of const);
begin
  Execute(FormatUTF8(SQLFormat, Args), ExpectResults, Params);
end;

function TSQLDBStatement.UpdateCount: integer;
begin
  result := 0;
end;

procedure TSQLDBStatement.ExecutePreparedAndFetchAllAsJSON(Expanded: boolean;
  out JSON: RawUTF8);
begin
  ExecutePrepared;
  JSON := FetchAllAsJSON(Expanded);
end;

function TSQLDBStatement.ColumnString(Col: integer): string;
begin
  result := UTF8ToString(ColumnUTF8(Col));
end;

function TSQLDBStatement.ColumnString(const ColName: RawUTF8): string;
begin
  result := ColumnString(ColumnIndex(ColName));
end;

function TSQLDBStatement.ColumnBlob(const ColName: RawUTF8): RawByteString;
begin
  result := ColumnBlob(ColumnIndex(ColName));
end;

function TSQLDBStatement.ColumnBlobBytes(const ColName: RawUTF8): TBytes;
begin
  result := ColumnBlobBytes(ColumnIndex(ColName));
end;

procedure TSQLDBStatement.ColumnBlobToStream(const ColName: RawUTF8; Stream: TStream);
begin
  ColumnBlobToStream(ColumnIndex(ColName), Stream);
end;

procedure TSQLDBStatement.ColumnBlobFromStream(const ColName: RawUTF8; Stream: TStream);
begin
  ColumnBlobFromStream(ColumnIndex(ColName), Stream);
end;

function TSQLDBStatement.ColumnCurrency(const ColName: RawUTF8): currency;
begin
  result := ColumnCurrency(ColumnIndex(ColName));
end;

function TSQLDBStatement.ColumnDateTime(const ColName: RawUTF8): TDateTime;
begin
  result := ColumnDateTime(ColumnIndex(ColName));
end;

function TSQLDBStatement.ColumnDouble(const ColName: RawUTF8): double;
begin
  result := ColumnDouble(ColumnIndex(ColName));
end;

function TSQLDBStatement.ColumnInt(const ColName: RawUTF8): Int64;
begin
  result := ColumnInt(ColumnIndex(ColName));
end;

function TSQLDBStatement.ColumnUTF8(const ColName: RawUTF8): RawUTF8;
begin
  result := ColumnUTF8(ColumnIndex(ColName));
end;

function TSQLDBStatement.ColumnVariant(const ColName: RawUTF8): Variant;
begin
  ColumnToVariant(ColumnIndex(ColName), result);
end;

function TSQLDBStatement.GetColumnVariant(const ColName: RawUTF8): Variant;
begin
  ColumnToVariant(ColumnIndex(ColName), result);
end;

function TSQLDBStatement.ColumnCursor(const ColName: RawUTF8): ISQLDBRows;
begin
  result := ColumnCursor(ColumnIndex(ColName));
end;

function TSQLDBStatement.{%H-}ColumnCursor(Col: integer): ISQLDBRows;
begin
  raise ESQLDBException.CreateUTF8('% does not support CURSOR columns', [self]);
end;

function TSQLDBStatement.Instance: TSQLDBStatement;
begin
  result := Self;
end;

function TSQLDBStatement.SQLLogBegin(level: TSynLogInfo): TSynLog;
begin
  if level = sllDB then // prepare
    fSQLLogTimer.Start
  else
    fSQLLogTimer.Resume;
  {$ifdef SYNDB_SILENCE}
  result := nil;
  {$else}
  result := SynDBLog.Add;
  if result <> nil then
    if level in result.Family.Level then
    begin
      fSQLLogLevel := level;
      if level = sllSQL then
        ComputeSQLWithInlinedParams;
    end
    else
      result := nil;
  fSQLLogLog := result;
  {$endif SYNDB_SILENCE}
end;

function TSQLDBStatement.SQLLogEnd(msg: PShortString): Int64;
{$ifndef SYNDB_SILENCE}
var
  tmp: TShort16;
{$endif}
begin
  fSQLLogTimer.Pause;
  {$ifdef SYNDB_SILENCE}
  result := fSQLLogTimer.LastTimeInMicroSec;
  {$else}
  result := 0;
  if fSQLLogLog = nil then
    exit;
  tmp[0] := #0;
  if fSQLLogLevel = sllSQL then
  begin
    if msg = nil then
    begin
      if not fExpectResults then
        FormatShort16(' wr=%', [UpdateCount], tmp);
      msg := @tmp;
    end;
    fSQLLogLog.Log(fSQLLogLevel, 'ExecutePrepared %% %',
      [fSQLLogTimer.Time, msg^, fSQLWithInlinedParams], self)
  end
  else
  begin
    if msg = nil then
      msg := @tmp;
    fSQLLogLog.Log(fSQLLogLevel, 'Prepare %% %', [fSQLLogTimer.Stop, msg^, fSQL], self);
  end;
  result := fSQLLogTimer.LastTimeInMicroSec;
  fSQLLogLog := nil;
  {$endif}
end;

function TSQLDBStatement.SQLLogEnd(const Fmt: RawUTF8; const Args: array of const): Int64;
var
  tmp: shortstring;
begin
  tmp[0] := #0;
  {$ifndef SYNDB_SILENCE}
  result := 0;
  if fSQLLogLog = nil then
    exit;
  if Fmt <> '' then
    FormatShort(Fmt, Args, tmp);
  {$endif}
  result := SQLLogEnd(@tmp);
end;

function TSQLDBStatement.GetSQLCurrent: RawUTF8;
begin
  if fSQLPrepared <> '' then
    result := fSQLPrepared
  else
    result := fSQL;
end;

function TSQLDBStatement.GetSQLWithInlinedParams: RawUTF8;
begin
  if fSQL = '' then
    result := ''
  else
  begin
    if fSQLWithInlinedParams = '' then
      ComputeSQLWithInlinedParams;
    result := fSQLWithInlinedParams;
  end;
end;

function GotoNextParam(P: PUTF8Char): PUTF8Char;
  {$ifdef HASINLINE} inline; {$endif}
var
  c: AnsiChar;
begin
  repeat
    c := P^;
    if (c = #0) or (c = '?') then
      break;
    if (c = '''') and (P[1] <> '''') then
    begin
      repeat // ignore ? inside ' quotes
        inc(P);
        c := P^;
      until (c = #0) or ((c = '''') and (P[1] <> ''''));
      if c = #0 then
        break;
    end;
    inc(P);
  until false;
  result := P;
end;

procedure TSQLDBStatement.ComputeSQLWithInlinedParams;
var
  P, B: PUTF8Char;
  num: integer;
  maxSize, maxAllowed: cardinal;
  W: TTextWriter;
  tmp: TTextWriterStackBuffer;
begin
  fSQLWithInlinedParams := fSQL;
  if fConnection = nil then
    maxSize := 0
  else
    maxSize := fConnection.fProperties.fLoggedSQLMaxSize;
  if (integer(maxSize) < 0) or (PosExChar('?', fSQL) = 0) then
    // maxsize=-1 -> log statement without any parameter value (just ?)
    exit;
  P := pointer(fSQL);
  num := 1;
  W := nil;
  try
    repeat
      B := P;
      P := GotoNextParam(P);
      if W = nil then
        if P^ = #0 then
          exit
        else
          W := TTextWriter.CreateOwnedStream(tmp);
      W.AddNoJSONEscape(B, P - B);
      if P^ = #0 then
        break;
      inc(P); // jump P^='?'
      if maxSize > 0 then
        maxAllowed := W.TextLength - maxSize
      else
        maxAllowed := maxInt;
      AddParamValueAsText(num, W, maxAllowed);
      inc(num);
    until (P^ = #0) or ((maxSize > 0) and (W.TextLength >= maxSize));
    W.SetText(fSQLWithInlinedParams);
  finally
    W.Free;
  end;
end;

procedure TSQLDBStatement.AddParamValueAsText(Param: integer; Dest: TTextWriter;
  MaxCharCount: integer);

  procedure AppendUnicode(W: PWideChar; WLen: integer);
  var
    tmp: TSynTempBuffer;
  begin
    if MaxCharCount < WLen then
      WLen := MaxCharCount;
    tmp.Init(WLen);
    try
      RawUnicodeToUtf8(tmp.buf, tmp.Len, W, WLen, [ccfNoTrailingZero]);
      Dest.AddQuotedStr(tmp.buf, '''', MaxCharCount);
    finally
      tmp.Done;
    end;
  end;

var
  v: variant;
  ft: TSQLDBFieldType;
begin
  ft := ParamToVariant(Param, v, false);
  with TVarData(v) do
    case cardinal(VType) of
      varString:
        if ft = ftBlob then
          Dest.AddU(length(RawByteString(VString)))
        else
          Dest.AddQuotedStr(VString, '''', MaxCharCount);
      varOleStr:
        AppendUnicode(VString, length(WideString(VString)));
      {$ifdef HASVARUSTRING}
      varUString:
        AppendUnicode(VString, length(UnicodeString(VString)));
      {$endif}
    else
      if (ft = ftDate) and (cardinal(VType) in [varDouble, varDate]) then
        Dest.AddDateTime(vdate)
      else
        Dest.AddVariant(v);
    end;
end;

var
  SQLDBRowVariantType: TCustomVariantType = nil;

function TSQLDBStatement.RowData: Variant;
begin
  if SQLDBRowVariantType = nil then
    SQLDBRowVariantType := SynRegisterCustomVariantType(TSQLDBRowVariantType);
  VarClear(result);
  with TVarData(result) do
  begin
    VType := SQLDBRowVariantType.VarType;
    VPointer := self;
  end;
end;

procedure TSQLDBStatement.RowDocVariant(out aDocument: variant;
  aOptions: TDocVariantOptions);
var
  n, F: integer;
  names: TRawUTF8DynArray;
  values: TVariantDynArray;
begin
  n := ColumnCount;
  SetLength(names, n); // faster to assign internal arrays per reference
  SetLength(values, n);
  for F := 0 to n - 1 do
  begin
    names[F] := ColumnName(F);
    ColumnToVariant(F, values[F]);
  end;
  TDocVariantData(aDocument).InitObjectFromVariants(names, values, aOptions);
end;

procedure TSQLDBStatement.Prepare(const aSQL: RawUTF8; ExpectResults: boolean);
var
  L: integer;
begin
  Connection.InternalProcess(speActive);
  try
    L := length(aSQL);
    if StripSemicolon then
      if (L > 5) and (aSQL[L] = ';') and // avoid syntax error for some drivers
        not IdemPChar(@aSQL[L - 4], ' END') then
        fSQL := copy(aSQL, 1, L - 1)
      else
        fSQL := aSQL
    else
      fSQL := aSQL;
    fExpectResults := ExpectResults;
    if (fConnection <> nil) and not fConnection.IsConnected then
      fConnection.Connect;
  finally
    Connection.InternalProcess(speNonActive);
  end;
end;

procedure TSQLDBStatement.ExecutePrepared;
begin
  if fConnection <> nil then
    fConnection.fLastAccessTicks := GetTickCount64;
  // a do-nothing default method
end;

procedure TSQLDBStatement.Reset;
begin
  fSQLWithInlinedParams := '';
  fSQLLogTimer.Init; // reset timer (for cached statement for example)
end;

procedure TSQLDBStatement.ReleaseRows;
begin
  fSQLWithInlinedParams := '';
end;

function TSQLDBStatement.ColumnsToSQLInsert(const TableName: RawUTF8;
  var Fields: TSQLDBColumnCreateDynArray): RawUTF8;
var
  F, size: integer;
begin
  result := '';
  if (self = nil) or (TableName = '') then
    exit;
  SetLength(Fields, ColumnCount);
  if Fields = nil then
    exit;
  result := 'insert into ' + TableName + ' (';
  for F := 0 to high(Fields) do
  begin
    Fields[F].Name := ColumnName(F);
    Fields[F].DBType := ColumnType(F, @size);
    Fields[F].Width := size;
    case Fields[F].DBType of
      ftNull:
        Fields[F].DBType := ftBlob; // if not identified, assume it is a BLOB
      ftUnknown:
        raise ESQLDBException.CreateUTF8('%.ColumnsToSQLInsert: Invalid column %',
          [self, Fields[F].Name]);
    end;
    result := result + Fields[F].Name + ',';
  end;
  result[length(result)] := ')';
  result := result + ' values (';
  for F := 0 to high(Fields) do
    result := result + '?,'; // MUCH faster with a prepared statement
  result[length(result)] := ')';
end;

procedure TSQLDBStatement.BindFromRows(const Fields: TSQLDBFieldTypeDynArray;
  Rows: TSQLDBStatement);
var
  F: integer;
begin
  if (self <> nil) and (Fields <> nil) and (Rows <> nil) then
    for F := 0 to high(Fields) do
      if Rows.ColumnNull(F) then
        BindNull(F + 1)
      else
        case Fields[F] of
          ftNull:
            BindNull(F + 1);
          ftInt64:
            Bind(F + 1, Rows.ColumnInt(F));
          ftDouble:
            Bind(F + 1, Rows.ColumnDouble(F));
          ftCurrency:
            BindCurrency(F + 1, Rows.ColumnCurrency(F));
          ftDate:
            BindDateTime(F + 1, Rows.ColumnDateTime(F));
          ftUTF8:
            BindTextU(F + 1, Rows.ColumnUTF8(F));
          ftBlob:
            BindBlob(F + 1, Rows.ColumnBlob(F));
        end;
end;

procedure TSQLDBStatement.BindCursor(Param: integer);
begin
  raise ESQLDBException.CreateUTF8('% does not support CURSOR parameter', [self]);
end;

function TSQLDBStatement.{%H-}BoundCursor(Param: Integer): ISQLDBRows;
begin
  raise ESQLDBException.CreateUTF8('% does not support CURSOR parameter', [self]);
end;


{ TSQLDBConnection }

procedure TSQLDBConnection.CheckConnection;
begin
  if self = nil then
    raise ESQLDBException.Create('TSQLDBConnection not created');
  if not Connected then
    raise ESQLDBException.CreateUTF8('% on %/% should be connected',
      [self, Properties.ServerName, Properties.DataBaseName]);
end;

procedure TSQLDBConnection.InternalProcess(Event: TOnSQLDBProcessEvent);
begin
  if (self = nil) or not Assigned(OnProcess) then
    exit;
  case Event of // thread-safe handle of speActive/peNonActive nested calls
    speActive:
      if InterlockedIncrement(fInternalProcessActive) = 1 then
        OnProcess(self, Event);
    speNonActive:
      if InterlockedDecrement(fInternalProcessActive) = 0 then
        OnProcess(self, Event);
  else
    OnProcess(self, Event);
  end;
end;

procedure TSQLDBConnection.Commit;
begin
  CheckConnection;
  if TransactionCount <= 0 then
    raise ESQLDBException.CreateUTF8('Invalid %.Commit call', [self]);
  dec(fTransactionCount);
  InternalProcess(speCommit);
end;

constructor TSQLDBConnection.Create(aProperties: TSQLDBConnectionProperties);
begin
  fProperties := aProperties;
  if aProperties <> nil then
  begin
    fOnProcess := aProperties.OnProcess;
    fRollbackOnDisconnect := aProperties.RollbackOnDisconnect;
  end;
end;

procedure TSQLDBConnection.Connect;
var
  i: integer;
begin
  inc(fTotalConnectionCount);
  InternalProcess(speConnected);
  if fTotalConnectionCount > 1 then
    InternalProcess(speReconnected);
  if fServerTimestampAtConnection = 0 then
  try
    fServerTimestampAtConnection := ServerDateTime;
  except
    fServerTimestampAtConnection := Now;
  end;
  for i := 0 to length(fProperties.ExecuteWhenConnected) - 1 do
    with NewStatement do
    try
      Execute(fProperties.ExecuteWhenConnected[i], false);
    finally
      Free;
    end;
end;

procedure TSQLDBConnection.Disconnect;
var
  i: PtrInt;
  Obj: PPointerArray;
begin
  InternalProcess(speDisconnected);
  if fCache <> nil then
  begin
    InternalProcess(speActive);
    try
      Obj := fCache.ObjectPtr;
      if Obj <> nil then
        for i := 0 to fCache.Count - 1 do
          TSQLDBStatement(Obj[i]).FRefCount := 0; // force clean release
      FreeAndNil(fCache); // release all cached statements
    finally
      InternalProcess(speNonActive);
    end;
  end;
  if InTransaction then
  try
    if RollbackOnDisconnect then
    begin
      fTransactionCount := 1; // flush transaction nesting level
      Rollback;
    end;
  finally
    fTransactionCount := 0; // flush transaction nesting level
  end;
end;

destructor TSQLDBConnection.Destroy;
begin
  try
    Disconnect;
  except
    on E: Exception do
      SynDBLog.Add.Log(sllError, E);
  end;
  inherited;
end;

function TSQLDBConnection.IsOutdated(tix: Int64): boolean;
begin
  result := false;
  if (self = nil) or (fProperties.fConnectionTimeOutTicks = 0) then
    exit;
  if fLastAccessTicks < 0 then
  begin // was forced by ClearConnectionPool
    result := true;
    exit;
  end;
  if (fLastAccessTicks = 0) or
     (tix - fLastAccessTicks < fProperties.fConnectionTimeOutTicks) then
    // brand new connection, or active enough connection
    fLastAccessTicks := tix
  else    // notify connection is clearly outdated
    result := true;
end;

function TSQLDBConnection.GetInTransaction: boolean;
begin
  result := TransactionCount > 0;
end;

function TSQLDBConnection.GetServerTimestamp: TTimeLog;
begin
  PTimeLogBits(@result)^.From(GetServerDateTime);
end;

function TSQLDBConnection.GetServerDateTime: TDateTime;
var
  Current: TDateTime;
begin
  Current := NowUTC; // so won't conflict with any potential time zone change
  if (fServerTimestampOffset = 0) and (fProperties.fSQLGetServerTimestamp <> '') then
  begin
    with fProperties do
      with Execute(fSQLGetServerTimestamp, []) do
        if Step then
          fServerTimestampOffset := ColumnDateTime(0) - Current;
    if fServerTimestampOffset = 0 then
      fServerTimestampOffset := 0.000001; // request server only once
  end;
  result := Current + fServerTimestampOffset;
end;

function TSQLDBConnection.GetLastErrorWasAboutConnection: boolean;
begin
  result := (self <> nil) and (Properties <> nil) and (fErrorMessage <> '') and
    Properties.ExceptionIsAboutConnection(fErrorException, fErrorMessage);
end;

function TSQLDBConnection.NewStatementPrepared(const aSQL: RawUTF8;
  ExpectResults: boolean; RaiseExceptionOnError: boolean;
  AllowReconnect: boolean): ISQLDBStatement;
var
  Stmt: TSQLDBStatement;
  ToCache: boolean;
  ndx, altern: integer;
  cachedSQL: RawUTF8;

  procedure TryPrepare(doraise: boolean);
  var
    Stmt: TSQLDBStatement;
  begin
    Stmt := nil;
    try
      InternalProcess(speActive);
      try
        Stmt := NewStatement;
        Stmt.Prepare(aSQL, ExpectResults);
        if ToCache then
        begin
          if fCache = nil then
            fCache := TRawUTF8List.Create([fObjectsOwned, fNoDuplicate, fCaseSensitive]);
          if fCache.AddObject(cachedSQL, Stmt) >= 0 then
            Stmt._AddRef
          else // will be owned by fCache.Objects[]
            SynDBLog.Add.Log(sllWarning, 'NewStatementPrepared: unexpected ' +
              'cache duplicate for %', [Stmt.SQLWithInlinedParams], self);
        end;
        result := Stmt;
      finally
        InternalProcess(speNonActive);
      end;
    except
      on E: Exception do
      begin
        {$ifndef SYNDB_SILENCE}
        with SynDBLog.Add do
          if [sllSQL, sllDB, sllException, sllError] * Family.Level <> [] then
            LogLines(sllSQL, pointer(Stmt.SQLWithInlinedParams), self, '--');
        {$endif}
        Stmt.Free;
        result := nil;
        StringToUTF8(E.Message, fErrorMessage);
        fErrorException := PPointer(E)^;
        if doraise then
          raise;
      end;
    end;
  end;

begin
  result := nil;
  fErrorMessage := '';
  fErrorException := nil;
  if length(aSQL) < 5 then
    exit;
  // first check if could be retrieved from cache
  cachedSQL := aSQL;
  ToCache := fProperties.IsCachable(Pointer(aSQL));
  if ToCache and (fCache <> nil) then
  begin
    ndx := fCache.IndexOf(cachedSQL);
    if ndx >= 0 then
    begin
      Stmt := fCache.Objects[ndx];
      if Stmt.RefCount = 1 then
      begin // ensure statement is not currently in use
        result := Stmt; // acquire the statement
        Stmt.Reset;
        exit;
      end
      else
      begin // in use -> create cached alternatives
        ToCache := false; // if all slots are used, won't cache this statement
        if fProperties.StatementCacheReplicates = 0 then
          SynDBLog.Add.Log(sllWarning,
            'NewStatementPrepared: cached statement still in use ' +
            '-> you should release ISQLDBStatement ASAP [%]', [cachedSQL], self)
        else
          for altern := 1 to fProperties.StatementCacheReplicates do
          begin
            cachedSQL := aSQL + RawUTF8(AnsiChar(altern)); // safe SQL duplicate
            ndx := fCache.IndexOf(cachedSQL);
            if ndx >= 0 then
            begin
              Stmt := fCache.Objects[ndx];
              if Stmt.RefCount = 1 then
              begin
                result := Stmt;
                Stmt.Reset;
                exit;
              end;
            end
            else
            begin
              ToCache := true; // cache the statement in this void slot
              break;
            end;
          end;
      end;
    end;
  end;
  // not in cache (or not cachable) -> prepare now
  if fProperties.ReconnectAfterConnectionError and AllowReconnect then
  begin
    TryPrepare({doraise=}false);
    if result <> nil then
      exit; // success
    if LastErrorWasAboutConnection then
    try
      SynDBLog.Add.Log(sllDB, 'NewStatementPrepared: reconnect after %',
        [fErrorException], self);
      Disconnect;
      Connect;
      TryPrepare(RaiseExceptionOnError);
      if result = nil then
      begin
        SynDBLog.Add.Log(sllDB, 'NewStatementPrepared: unable to reconnect', self);
        InternalProcess(speConnectionLost);
      end;
    except
      if RaiseExceptionOnError then
        raise
      else
        result := nil;
    end
    else if RaiseExceptionOnError and (fErrorException <> nil) then
      // propagate error not related to connection (e.g. SQL syntax error)
      raise fErrorException.Create(UTF8ToString(fErrorMessage));
  end
  else
    // regular preparation, with no connection error interception
    TryPrepare(RaiseExceptionOnError);
end;

procedure TSQLDBConnection.Rollback;
begin
  CheckConnection;
  if TransactionCount <= 0 then
    raise ESQLDBException.CreateUTF8('Invalid %.Rollback call', [self]);
  dec(fTransactionCount);
  InternalProcess(speRollback);
end;

function TSQLDBConnection.PasswordChange: boolean;
begin
  result := false;
end;

procedure TSQLDBConnection.StartTransaction;
begin
  CheckConnection;
  inc(fTransactionCount);
  InternalProcess(speStartTransaction);
end;

function TSQLDBConnection.NewTableFromRows(const TableName: RawUTF8;
  Rows: TSQLDBStatement; WithinTransaction: boolean;
  ColumnForcedTypes: TSQLDBFieldTypeDynArray): integer;
var
  Fields: TSQLDBColumnCreateDynArray;
  aTableName, SQL: RawUTF8;
  Tables: TRawUTF8DynArray;
  Ins: TSQLDBStatement;
  i, n: integer;
begin
  result := 0;
  if (self = nil) or (Rows = nil) or (Rows.ColumnCount = 0) then
    exit;
  aTableName := Properties.SQLTableName(TableName);
  if WithinTransaction then
    StartTransaction; // MUCH faster within a transaction
  try
    Ins := nil;
    InternalProcess(speActive);
    try
      while Rows.Step do
      begin
        // init when first row of data is available
        if Ins = nil then
        begin
          SQL := Rows.ColumnsToSQLInsert(aTableName, Fields);
          n := length(Fields);
          if Length(ColumnForcedTypes) <> n then
          begin
            SetLength(ColumnForcedTypes, n);
            for i := 0 to n - 1 do
              case Fields[i].DBType of
                ftUnknown:
                  ColumnForcedTypes[i] := ftInt64;
                ftNull:
                  ColumnForcedTypes[i] := ftBlob; // assume NULL is a BLOB
              else
                ColumnForcedTypes[i] := Fields[i].DBType;
              end;
          end;
          Properties.GetTableNames(Tables);
          if FindRawUTF8(Tables, TableName, false) < 0 then
            with Properties do
              ExecuteNoResult(SQLCreate(aTableName, Fields, false), []);
          Ins := NewStatement;
          Ins.Prepare(SQL, false);
        end;
        Rows.ReleaseRows;
        // write row data
        Ins.BindFromRows(ColumnForcedTypes, Rows);
        Ins.ExecutePrepared;
        Ins.Reset;
        inc(result);
      end;
      if WithinTransaction then
        Commit;
    finally
      Ins.Free;
      InternalProcess(speNonActive);
    end;
  except
    on Exception do
    begin
      if WithinTransaction then
        Rollback;
      raise;
    end;
  end;
end;



{ ************ Parent Classes for Thread-Safe and Parametrized Connections }

{ TSQLDBConnectionPropertiesThreadSafe }

procedure TSQLDBConnectionPropertiesThreadSafe.ClearConnectionPool;
var
  i: PtrInt;
begin
  fConnectionPool.Safe.Lock;
  try
    if fMainConnection <> nil then
      fMainConnection.fLastAccessTicks := -1; // force IsOutdated to return true
    for i := 0 to fConnectionPool.Count - 1 do
      TSQLDBConnectionThreadSafe(fConnectionPool.List[i]).fLastAccessTicks := -1;
    fLatestConnectionRetrievedInPool := -1;
  finally
    fConnectionPool.Safe.UnLock;
  end;
end;

constructor TSQLDBConnectionPropertiesThreadSafe.Create(const aServerName,
  aDatabaseName, aUserID, aPassWord: RawUTF8);
begin
  fConnectionPool := TSynObjectListLocked.Create;
  fLatestConnectionRetrievedInPool := -1;
  inherited Create(aServerName, aDatabaseName, aUserID, aPassWord);
end;

function TSQLDBConnectionPropertiesThreadSafe.CurrentThreadConnectionIndex: Integer;
var
  id: TThreadID;
  tix: Int64;
  conn: TSQLDBConnectionThreadSafe;
begin // caller made fConnectionPool.Safe.Lock
  if self <> nil then
  begin
    id := GetCurrentThreadId;
    tix := GetTickCount64;
    result := fLatestConnectionRetrievedInPool;
    if result >= 0 then
    begin
      conn := fConnectionPool.List[result];
      if (conn.fThreadID = id) and not conn.IsOutdated(tix) then
        exit;
    end;
    result := 0;
    while result < fConnectionPool.Count do
    begin
      conn := fConnectionPool.List[result];
      if conn.IsOutdated(tix) then // to guarantee reconnection
        fConnectionPool.Delete(result)
      else
      begin
        if conn.fThreadID = id then
        begin
          fLatestConnectionRetrievedInPool := result;
          exit;
        end;
        inc(result);
      end;
    end;
  end;
  result := -1;
end;

destructor TSQLDBConnectionPropertiesThreadSafe.Destroy;
begin
  inherited Destroy; // do MainConnection.Free
  fConnectionPool.Free;
end;

procedure TSQLDBConnectionPropertiesThreadSafe.EndCurrentThread;
var
  i: integer;
begin
  fConnectionPool.Safe.Lock;
  try
    i := CurrentThreadConnectionIndex;
    if i >= 0 then
    begin // do nothing if this thread has no active connection
      fConnectionPool.Delete(i); // release thread's TSQLDBConnection instance
      if i = fLatestConnectionRetrievedInPool then
        fLatestConnectionRetrievedInPool := -1;
    end;
  finally
    fConnectionPool.Safe.UnLock;
  end;
end;

function TSQLDBConnectionPropertiesThreadSafe.GetMainConnection: TSQLDBConnection;
begin
  result := ThreadSafeConnection;
end;

function TSQLDBConnectionPropertiesThreadSafe.ThreadSafeConnection: TSQLDBConnection;
var
  i: integer;
begin
  case fThreadingMode of
    tmThreadPool:
      begin
        fConnectionPool.Safe.Lock;
        try
          i := CurrentThreadConnectionIndex;
          if i >= 0 then
          begin
            result := fConnectionPool.List[i];
            exit;
          end;
          result := NewConnection; // no need to release the lock (fast method)
          (result as TSQLDBConnectionThreadSafe).fThreadID := GetCurrentThreadId;
          fLatestConnectionRetrievedInPool := fConnectionPool.Add(result)
        finally
          fConnectionPool.Safe.UnLock;
        end;
      end;
    tmMainConnection:
      result := inherited GetMainConnection;
  else
    result := nil;
  end;
end;


{ TSQLDBStatementWithParams }

function TSQLDBStatementWithParams.CheckParam(Param: Integer;
  NewType: TSQLDBFieldType; IO: TSQLDBParamInOutType): PSQLDBParam;
begin
  if self = nil then
    raise ESQLDBException.Create('self=nil for TSQLDBStatement.Bind*()');
  if Param > fParamCount then
    fParam.Count := Param; // resize fParams[] dynamic array if necessary
  result := @fParams[Param - 1];
  result^.VType := NewType;
  result^.VInOut := IO;
end;

function TSQLDBStatementWithParams.CheckParam(Param: Integer; NewType:
  TSQLDBFieldType; IO: TSQLDBParamInOutType; ArrayCount: integer): PSQLDBParam;
begin
  result := CheckParam(Param, NewType, IO);
  if (NewType in [ftUnknown, ftNull]) or (fConnection = nil) or
     (fConnection.fProperties.BatchSendingAbilities * [cCreate, cUpdate, cDelete] = []) then
    raise ESQLDBException.CreateUTF8('Invalid call to %.BindArray(Param=%,Type=%)',
      [self, Param, ToText(NewType)^]);
  SetLength(result^.VArray, ArrayCount);
  result^.VInt64 := ArrayCount;
  fParamsArrayCount := ArrayCount;
end;

constructor TSQLDBStatementWithParams.Create(aConnection: TSQLDBConnection);
begin
  inherited Create(aConnection);
  fParam.Init(TypeInfo(TSQLDBParamDynArray), fParams, @fParamCount);
end;

procedure TSQLDBStatementWithParams.Bind(Param: Integer; Value: double;
  IO: TSQLDBParamInOutType);
begin
  CheckParam(Param, ftDouble, IO)^.VInt64 := PInt64(@Value)^;
end;

procedure TSQLDBStatementWithParams.Bind(Param: Integer; Value: Int64;
  IO: TSQLDBParamInOutType);
begin
  CheckParam(Param, ftInt64, IO)^.VInt64 := Value;
end;

procedure TSQLDBStatementWithParams.BindBlob(Param: Integer;
  const Data: RawByteString; IO: TSQLDBParamInOutType);
begin
  CheckParam(Param, ftBlob, IO)^.VData := Data;
end;

procedure TSQLDBStatementWithParams.BindBlob(Param: Integer; Data: pointer;
  Size: integer; IO: TSQLDBParamInOutType);
begin
  SetString(CheckParam(Param, ftBlob, IO)^.VData, PAnsiChar(Data), Size);
end;

procedure TSQLDBStatementWithParams.BindCurrency(Param: Integer;
  Value: currency; IO: TSQLDBParamInOutType);
begin
  CheckParam(Param, ftCurrency, IO)^.VInt64 := PInt64(@Value)^;
end;

procedure TSQLDBStatementWithParams.BindDateTime(Param: Integer;
  Value: TDateTime; IO: TSQLDBParamInOutType);
begin
  CheckParam(Param, ftDate, IO)^.VInt64 := PInt64(@Value)^;
end;

procedure TSQLDBStatementWithParams.BindNull(Param: Integer;
  IO: TSQLDBParamInOutType; BoundType: TSQLDBFieldType);
begin
  CheckParam(Param, ftNull, IO);
end;

procedure TSQLDBStatementWithParams.BindTextS(Param: Integer;
  const Value: string; IO: TSQLDBParamInOutType);
begin
  if (Value = '') and (fConnection <> nil) and
     fConnection.fProperties.StoreVoidStringAsNull then
    CheckParam(Param, ftNull, IO)
  else
    CheckParam(Param, ftUTF8, IO)^.VData := StringToUTF8(Value);
end;

procedure TSQLDBStatementWithParams.BindTextU(Param: Integer;
  const Value: RawUTF8; IO: TSQLDBParamInOutType);
begin
  if (Value = '') and (fConnection <> nil) and
     fConnection.fProperties.StoreVoidStringAsNull
    then
    CheckParam(Param, ftNull, IO)
  else
    CheckParam(Param, ftUTF8, IO)^.VData := Value;
end;

procedure TSQLDBStatementWithParams.BindTextP(Param: Integer; Value: PUTF8Char;
  IO: TSQLDBParamInOutType);
begin
  if (Value = nil) and (fConnection <> nil) and
     fConnection.fProperties.StoreVoidStringAsNull then
    CheckParam(Param, ftNull, IO)
  else
    FastSetString(RawUTF8(CheckParam(Param, ftUTF8, IO)^.VData), Value, StrLen(Value));
end;

procedure TSQLDBStatementWithParams.BindTextW(Param: Integer; const Value:
  WideString; IO: TSQLDBParamInOutType);
begin
  if (Value = '') and (fConnection <> nil) and
     fConnection.fProperties.StoreVoidStringAsNull then
    CheckParam(Param, ftNull, IO)
  else
    CheckParam(Param, ftUTF8, IO)^.VData := RawUnicodeToUtf8(pointer(Value),
      length(Value));
end;

function TSQLDBStatementWithParams.ParamToVariant(Param: Integer;
  var Value: Variant; CheckIsOutParameter: boolean): TSQLDBFieldType;
begin
  inherited ParamToVariant(Param, Value); // raise exception if Param incorrect
  dec(Param); // start at #1
  if CheckIsOutParameter and (fParams[Param].VInOut = paramIn) then
    raise ESQLDBException.CreateUTF8('%.ParamToVariant expects an [In]Out parameter',
      [self]);
  // OleDB provider should have already modified the parameter in-place, i.e.
  // in our fParams[] buffer, especialy for TEXT parameters (OleStr/WideString)
  // -> we have nothing to do but return the current value! :)
  with fParams[Param] do
  begin
    result := VType;
    if VArray = nil then
      case VType of
        ftInt64:
          Value := VInt64;
        ftDouble:
          Value := unaligned(PDouble(@VInt64)^);
        ftCurrency:
          Value := PCurrency(@VInt64)^;
        ftDate:
          Value := PDateTime(@VInt64)^;
        ftUTF8:
          RawUTF8ToVariant(RawUTF8(VData), Value);
        ftBlob:
          RawByteStringToVariant(VData, Value);
      else
        SetVariantNull(Value)
      end
    else
      SetVariantNull(Value);
  end;
end;

procedure TSQLDBStatementWithParams.AddParamValueAsText(Param: integer;
  Dest: TTextWriter; MaxCharCount: integer);
begin
  dec(Param);
  if cardinal(Param) >= cardinal(fParamCount) then
    Dest.AddShort('null')
  else
    with fParams[Param] do
      if VArray = nil then
        case VType of
          ftInt64:
            Dest.Add(VInt64);
          ftDouble:
            Dest.AddDouble(unaligned(PDouble(@VInt64)^));
          ftCurrency:
            Dest.AddCurr64(VInt64);
          ftDate:
            Dest.AddDateTime(PDateTime(@VInt64), ' ', '''');
          ftUTF8:
            Dest.AddQuotedStr(pointer(VData), '''', MaxCharCount);
          ftBlob:
            Dest.AddU(length(VData));
        else
          Dest.AddShort('null');
        end
      else
        Dest.AddString(VArray[0]); // first item is enough in the logs
end;

procedure TSQLDBStatementWithParams.BindArray(Param: Integer;
  const Values: array of double);
var
  i: PtrInt;
begin
  with CheckParam(Param, ftDouble, paramIn, length(Values))^ do
    for i := 0 to high(Values) do
      VArray[i] := DoubleToStr(Values[i]);
end;

procedure TSQLDBStatementWithParams.BindArray(Param: Integer;
  const Values: array of Int64);
var
  i: PtrInt;
begin
  with CheckParam(Param, ftInt64, paramIn, length(Values))^ do
    for i := 0 to high(Values) do
      VArray[i] := Int64ToUtf8(Values[i]);
end;

procedure TSQLDBStatementWithParams.BindArray(Param: Integer;
  ParamType: TSQLDBFieldType; const Values: TRawUTF8DynArray; ValuesCount: integer);
var
  i: PtrInt;
  ChangeFirstChar: AnsiChar;
  p: PSQLDBParam;
  v: TTimeLogBits; // faster than TDateTime
begin
  inherited; // raise an exception in case of invalid parameter
  if fConnection = nil then
    ChangeFirstChar := 'T'
  else
    ChangeFirstChar := Connection.Properties.DateTimeFirstChar;
  p := CheckParam(Param, ParamType, paramIn);
  p^.VInt64 := ValuesCount;
  p^.VArray := Values; // immediate COW reference-counted assignment
  if (ParamType = ftDate) and (ChangeFirstChar <> 'T') then
    for i := 0 to ValuesCount - 1 do // fix e.g. for PostgreSQL
      if (p^.VArray[i] <> '') and (p^.VArray[i][1] = '''') then
      begin
        v.From(PUTF8Char(pointer(p^.VArray[i])) + 1, length(p^.VArray[i]) - 2);
        p^.VArray[i] := v.FullText({expanded=}true, ChangeFirstChar, '''');
      end;
  fParamsArrayCount := ValuesCount;
end;

procedure TSQLDBStatementWithParams.BindArray(Param: Integer;
  const Values: array of RawUTF8);
var
  i: PtrInt;
  StoreVoidStringAsNull: boolean;
begin
  StoreVoidStringAsNull := (fConnection <> nil) and
    fConnection.Properties.StoreVoidStringAsNull;
  with CheckParam(Param, ftUTF8, paramIn, length(Values))^ do
    for i := 0 to high(Values) do
      if StoreVoidStringAsNull and (Values[i] = '') then
        VArray[i] := 'null'
      else
        QuotedStr(Values[i], '''', VArray[i]);
end;

procedure TSQLDBStatementWithParams.BindArrayCurrency(Param: Integer;
  const Values: array of currency);
var
  i: PtrInt;
begin
  with CheckParam(Param, ftCurrency, paramIn, length(Values))^ do
    for i := 0 to high(Values) do
      VArray[i] := Curr64ToStr(PInt64(@Values[i])^);
end;

procedure TSQLDBStatementWithParams.BindArrayDateTime(Param: Integer;
  const Values: array of TDateTime);
var
  i: PtrInt;
begin
  with CheckParam(Param, ftDate, paramIn, length(Values))^ do
    for i := 0 to high(Values) do
      VArray[i] := Connection.Properties.SQLDateToIso8601Quoted(Values[i]);
end;

procedure TSQLDBStatementWithParams.BindArrayRowPrepare(
  const aParamTypes: array of TSQLDBFieldType; aExpectedMinimalRowCount: integer);
var
  i: PtrInt;
begin
  fParam.Count := 0;
  for i := 0 to high(aParamTypes) do
    CheckParam(i + 1, aParamTypes[i], paramIn, aExpectedMinimalRowCount);
  fParamsArrayCount := 0;
end;

procedure TSQLDBStatementWithParams.BindArrayRow(const aValues: array of const);
var
  i: PtrInt;
begin
  if length(aValues) <> fParamCount then
    raise ESQLDBException.CreateFmt('Invalid %.BindArrayRow call', [self]);
  for i := 0 to high(aValues) do
    with fParams[i] do
    begin
      if length(VArray) <= fParamsArrayCount then
        SetLength(VArray, NextGrow(fParamsArrayCount));
      VInt64 := fParamsArrayCount;
      if (VType = ftDate) and (aValues[i].VType = vtExtended) then
        VArray[fParamsArrayCount] := // direct binding of TDateTime value
          Connection.Properties.SQLDateToIso8601Quoted(aValues[i].VExtended^)
      else
      begin
        VarRecToUTF8(aValues[i], VArray[fParamsArrayCount]);
        case VType of
          ftUTF8:
            if (VArray[fParamsArrayCount] = '') and (fConnection <> nil) and
              fConnection.Properties.StoreVoidStringAsNull then
              VArray[fParamsArrayCount] := 'null'
            else
              VArray[fParamsArrayCount] := QuotedStr(VArray[fParamsArrayCount]);
          ftDate:
            VArray[fParamsArrayCount] := QuotedStr(VArray[fParamsArrayCount]);
        end;
      end;
    end;
  inc(fParamsArrayCount);
end;

procedure TSQLDBStatementWithParams.BindFromRows(Rows: TSQLDBStatement);
var
  F: PtrInt;
  U: RawUTF8;
begin
  if Rows <> nil then
    if Rows.ColumnCount <> fParamCount then
      raise ESQLDBException.CreateUTF8('Invalid %.BindFromRows call', [self])
    else
      for F := 0 to fParamCount - 1 do
        with fParams[F] do
        begin
          if length(VArray) <= fParamsArrayCount then
            SetLength(VArray, NextGrow(fParamsArrayCount));
          if Rows.ColumnNull(F) then
            VArray[fParamsArrayCount] := 'null'
          else
            case Rows.ColumnType(F) of
              ftNull:
                VArray[fParamsArrayCount] := 'null';
              ftInt64:
                VArray[fParamsArrayCount] := Int64ToUtf8(Rows.ColumnInt(F));
              ftDouble:
                VArray[fParamsArrayCount] := DoubleToStr(Rows.ColumnDouble(F));
              ftCurrency:
                VArray[fParamsArrayCount] := CurrencyToStr(Rows.ColumnCurrency(F));
              ftDate:
                VArray[fParamsArrayCount] :=
                  '''' + DateTimeToSQL(Rows.ColumnDateTime (F)) + '''';
              ftUTF8:
                begin
                  U := Rows.ColumnUTF8(F);
                  if (U = '') and (fConnection <> nil) and
                     fConnection.Properties.StoreVoidStringAsNull then
                    VArray[fParamsArrayCount] := 'null'
                  else
                    VArray[fParamsArrayCount] := QuotedStr(U, '''');
                end;
              ftBlob:
                VArray[fParamsArrayCount] := Rows.ColumnBlob(F);
            end;
        end;
  inc(fParamsArrayCount);
end;

procedure TSQLDBStatementWithParams.Reset;
begin
  fParam.Clear;
  fParamsArrayCount := 0;
  inherited Reset;
end;

procedure TSQLDBStatementWithParams.ReleaseRows;
var
  i: PtrInt;
  p: PSQLDBParam;
begin
  p := pointer(fParams);
  if p <> nil then
    for i := 1 to fParamCount do
    begin
      if p^.VData <> '' then
        p^.VData := ''; // release bound value, but keep fParams[] reusable
      if p^.VArray <> nil then
        RawUTF8DynArrayClear(p^.VArray);
      inc(p);
    end;
  inherited ReleaseRows;
end;


{ TSQLDBStatementWithParamsAndColumns }

constructor TSQLDBStatementWithParamsAndColumns.Create(aConnection: TSQLDBConnection);
begin
  inherited Create(aConnection);
  fColumn.InitSpecific(TypeInfo(TSQLDBColumnPropertyDynArray),
    fColumns, ptRawUTF8, @fColumnCount, True);
end;

function TSQLDBStatementWithParamsAndColumns.ColumnIndex(
  const aColumnName: RawUTF8): integer;
begin
  result := fColumn.FindHashed(aColumnName);
end;

function TSQLDBStatementWithParamsAndColumns.ColumnName(Col: integer): RawUTF8;
begin
  CheckCol(Col);
  result := fColumns[Col].ColumnName;
end;

function TSQLDBStatementWithParamsAndColumns.ColumnType(
  Col: integer; FieldSize: PInteger): TSQLDBFieldType;
begin
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


const
  __TSQLDBColumnDefine = 'ColumnName,ColumnTypeNative RawUTF8 ' +
    'ColumnLength,ColumnPrecision,ColumnScale PtrInt ' +
    'ColumnType TSQLDBFieldType ColumnIndexed boolean';

initialization
  assert(SizeOf(TSQLDBColumnProperty) = sizeof(PtrUInt) * 2 + 20);
  Rtti.RegisterType(TypeInfo(TSQLDBFieldType));
  Rtti.RegisterFromText(TypeInfo(TSQLDBColumnDefine), __TSQLDBColumnDefine);
end.

