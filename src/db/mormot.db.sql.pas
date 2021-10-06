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
  mormot.crypt.secure,
  mormot.core.rtti,
  mormot.core.log,
  mormot.db.core;

{.$define SYNDB_SILENCE}
// if defined, this unit won't log the statement execution to SynDBLog


{ ************ SQL Fields and Columns Definitions }

type
  /// an array of RawUtf8, for each existing column type
  // - used e.g. by SqlCreate method
  // - ftUnknown maps int32 field (e.g. boolean), ftNull maps RawUtf8 index # field,
  // ftUtf8 maps RawUtf8 blob field, other types map their default kind
  // - for UTF-8 text, ftUtf8 will define the BLOB field, whereas ftNull will
  // expect to be formated with an expected field length in ColumnAttr
  // - the RowID definition will expect the ORM to create an unique identifier,
  // and will use the ftInt64 type definition for this
  // and send it with the INSERT statement (some databases, like Oracle, do not
  // support standard's IDENTITY attribute) - see http://troels.arvin.dk/db/rdbms
  TSqlDBFieldTypeDefinition = array[TSqlDBFieldType] of RawUtf8;

  /// the diverse type of bound parameters during a statement execution
  // - will be paramIn by default, which is the case 90% of time
  // - could be set to paramOut or paramInOut if must be refereshed after
  // execution (for calling a stored procedure expecting such parameters)
  TSqlDBParamInOutType = (
    paramIn,
    paramOut,
    paramInOut);

  /// used to define a field/column layout in a table schema
  // - for TSqlDBConnectionProperties.SqlCreate to describe the new table
  // - for TSqlDBConnectionProperties.GetFields to retrieve the table layout
  TSqlDBColumnDefine = packed record
    /// the Column name
    ColumnName: RawUtf8;
    /// the Column type, as retrieved from the database provider
    // - returned as plain text by GetFields method, to be used e.g. by
    // TSqlDBConnectionProperties.GetFieldDefinitions method
    // - SqlCreate will check for this value to override the default type
    ColumnTypeNative: RawUtf8;
    /// the Column default width (in chars or bytes) of ftUtf8 or ftBlob
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
    ColumnType: TSqlDBFieldType;
    /// specify if column is indexed
    ColumnIndexed: boolean;
  end;

  /// used to define the column layout of a table schema
  // - e.g. for TSqlDBConnectionProperties.GetFields
  TSqlDBColumnDefineDynArray = array of TSqlDBColumnDefine;

  /// used to describe extended Index definition of a table schema
  TSqlDBIndexDefine = packed record
    /// name of the index
    IndexName: RawUtf8;
    /// description of the index type
    // - for MS SQL possible values are:
    // $ HEAP | CLUSTERED | NONCLUSTERED | XML |SPATIAL
    //  - for Oracle:
    // $ NORMAL | BITMAP | FUNCTION-BASED NORMAL | FUNCTION-BASED BITMAP | DOMAIN
    // see @http://docs.oracle.com/cd/B19306_01/server.102/b14237/statviews_1069.htm
    TypeDesc: RawUtf8;
    /// Expression for the subset of rows included in the filtered index
    // - only set for MS SQL - not retrieved for other DB types yet
    Filter: RawUtf8;
    /// comma separated list of indexed column names, in order of their definition
    KeyColumns: RawUtf8;
    /// comma separaded list of a nonkey column added to the index by using the CREATE INDEX INCLUDE clause
    // - only set for MS SQL - not retrieved for other DB types yet
    IncludedColumns: RawUtf8;
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
  // - e.g. for TSqlDBConnectionProperties.GetIndexes
  TSqlDBIndexDefineDynArray = array of TSqlDBIndexDefine;

  /// used to define a parameter/column layout in a stored procedure schema
  // - for TSqlDBConnectionProperties.GetProcedureParameters to retrieve the stored procedure parameters
  // - can be extended according to https://msdn.microsoft.com/en-us/library/ms711701(v=vs.85).aspx
  TSqlDBProcColumnDefine = packed record
    /// the Column name
    ColumnName: RawUtf8;
    /// the Column type, as retrieved from the database provider
    // - used e.g. by TSqlDBConnectionProperties.GetProcedureParameters method
    ColumnTypeNative: RawUtf8;
    /// the Column default width (in chars or bytes) of ftUtf8 or ftBlob
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
    ColumnType: TSqlDBFieldType;
    /// defines the procedure column as a parameter or a result set column
    ColumnParamType: TSqlDBParamInOutType;
  end;

  /// used to define the parameter/column layout of a stored procedure schema
  // - e.g. for TSqlDBConnectionProperties.GetProcedureParameters
  TSqlDBProcColumnDefineDynArray = array of TSqlDBProcColumnDefine;

  /// possible column retrieval patterns
  // - used by TSqlDBColumnProperty.ColumnValueState
  TSqlDBStatementGetCol = (
    colNone,
    colNull,
    colWrongType,
    colDataFilled,
    colDataTruncated);

  /// used to define a field/column layout
  // - for TSqlDBConnectionProperties.SqlCreate to describe the table
  // - for T*Statement.Execute/Column*() methods to map the IRowSet content
  TSqlDBColumnProperty = packed record
    /// the Column name
    ColumnName: RawUtf8;
    /// a general purpose integer value
    // - for SqlCreate: default width (in WideChars or Bytes) of ftUtf8 or ftBlob;
    // if set to 0, a CLOB or BLOB column type will be created - note that
    // UTF-8 encoding is expected when calculating the maximum column byte size
    // for the CREATE TABLE statement (e.g. for Oracle 1333=4000/3 is used)
    // - for TOleDBStatement: the offset of this column in the IRowSet data,
    // starting with a DBSTATUSENUM, the data, then its length (for inlined
    // oftUtf8 and oftBlob only)
    // - for TSqlDBOracleStatement: contains an offset to this column values
    // inside fRowBuffer[] internal buffer
    // - for TSqlDBDatasetStatement: maps TField pointer value
    // - for TSqlDBPostgresStatement: contains the column type OID
    ColumnAttr: PtrUInt;
    /// the Column type, used for storage
    // - for SqlCreate: should not be ftUnknown nor ftNull
    // - for TOleDBStatement: should not be ftUnknown
    // - for mormot.db.sql.oracle: never ftUnknown, may be ftNull (for SQLT_RSET)
    ColumnType: TSqlDBFieldType;
    /// set if the Column must exists (i.e. should not be null)
    ColumnNonNullable: boolean;
    /// set if the Column shall have unique value (add the corresponding constraint)
    ColumnUnique: boolean;
    /// set if the Column data is inlined within the main rows buffer
    // - for TOleDBStatement: set if column was NOT defined as DBTYPE_BYREF
    // which is the most common case, when column data < 4 KB
    // - for TSqlDBOracleStatement: FALSE if column is an array of
    // POCILobLocator (SQLT_CLOB/SQLT_BLOB) or POCIStmt (SQLT_RSET)
    // - for TSqlDBOdbcStatement: FALSE if bigger than 255 WideChar (ftUtf8) or
    // 255 bytes (ftBlob)
    ColumnValueInlined: boolean;
    /// expected column data size
    // - for TSqlDBOracleStatement/TOleDBStatement/TODBCStatement: used to store
    // one column size (in bytes)
    ColumnValueDBSize: cardinal;
    /// optional character set encoding for ftUtf8 columns
    // - for SQLT_STR/SQLT_CLOB (mormot.db.sql.oracle): equals to the OCI char set
    ColumnValueDBCharSet: integer;
    /// internal DB column data type
    // - for TSqlDBOracleStatement: used to store the DefineByPos() TypeCode,
    // can be SQLT_STR/SQLT_CLOB, SQLT_FLT, SQLT_INT, SQLT_DAT, SQLT_BLOB,
    // SQLT_BIN and SQLT_RSET
    // - for TSqlDBOdbcStatement: used to store the DataType as returned
    // by ODBC.DescribeColW() - use private ODBC_TYPE_TO[ColumnType] to
    // retrieve the marshalled type used during column retrieval
    // - for TSqlDBFirebirdStatement: used to store XSQLVAR.sqltype
    // - for TSqlDBDatasetStatement: indicates the TField class type, i.e.
    // 0=TField, 1=TLargeIntField, 2=TWideStringField
    ColumnValueDBType: smallint;
    /// driver-specific encoding information
    // - for mormot.db.sql.oracle: used to store the ftUtf8 column encoding, i.e.
    // for SQLT_CLOB, equals either to SQLCS_NCHAR or SQLCS_IMPLICIT
    ColumnValueDBForm: byte;
    /// may contain the current status of the column value
    // - for mormot.db.sql.odbc: state of the latest SqlGetData() call
    ColumnDataState: TSqlDBStatementGetCol;
    /// may contain the current column size for not FIXEDLENGTH_SQLDBFIELDTYPE
    // - for mormot.db.sql.odbc: size (in bytes) in corresponding fColData[]
    // - TSqlDBProxyStatement: the actual maximum column size
    ColumnDataSize: integer;
  end;

  PSqlDBColumnProperty = ^TSqlDBColumnProperty;

  /// used to define a table/field column layout
  TSqlDBColumnPropertyDynArray = array of TSqlDBColumnProperty;

  /// used to define how a column to be created
  TSqlDBColumnCreate = record
    /// the data type
    // - here, ftUnknown is used for Int32 values, ftInt64 for Int64 values,
    // as expected by TSqlDBFieldTypeDefinition
    DBType: TSqlDBFieldType;
    /// the column name
    Name: RawUtf8;
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
  TSqlDBColumnCreateDynArray = array of TSqlDBColumnCreate;

  /// identify a CRUD mode of a statement
  // - in addition to CRUD states, cPostgreBulkArray would identify if the ORM
  // should generate unnested/any bound array statements - currently only
  // supported by mormot.db.sql.postgres for bulk insert/update/delete
  TSqlDBStatementCRUD = (
    cCreate,
    cRead,
    cUpdate,
    cDelete,
    cPostgreBulkArray);

  /// identify the CRUD modes of a statement
  // - used e.g. for batch send abilities of a DB engine
  TSqlDBStatementCRUDs = set of TSqlDBStatementCRUD;


const
  /// a magic constant used e.g. by TSqlDBStatement.FetchAllToBinary
  FETCHALLTOBINARY_MAGIC = 1;


{ ************ Define Database Engine Specific Behavior }

type
  /// the known database definitions
  // - will be used e.g. for TSqlDBConnectionProperties.SqlFieldCreate(), or
  // for OleDB/ODBC/ZDBC tuning according to the connected database engine
  TSqlDBDefinition = (
    dUnknown,
    dDefault,
    dOracle,
    dMSSQL,
    dJet,
    dMySQL,
    dSQLite,
    dFirebird,
    dNexusDB,
    dPostgreSQL,
    dDB2,
    dInformix);

  /// set of the available database definitions
  TSqlDBDefinitions = set of TSqlDBDefinition;

  /// where the LIMIT clause should be inserted for a given SQL syntax
  // - used by TSqlDBDefinitionLimitClause and SqlLimitClause() method
  TSqlDBDefinitionLimitPosition = (
    posNone,
    posWhere,
    posSelect,
    posAfter,
    posOuter);

  /// defines the LIMIT clause to be inserted for a given SQL syntax
  // - used by TSqlDBDefinitionLimitClause and SqlLimitClause() method
  TSqlDBDefinitionLimitClause = record
    Position: TSqlDBDefinitionLimitPosition;
    InsertFmt: PUtf8Char;
  end;

const
  /// the known column data types corresponding to our TSqlDBFieldType types
  // - will be used e.g. for TSqlDBConnectionProperties.SqlFieldCreate()
  // - see TSqlDBFieldTypeDefinition documentation to find out the mapping:
  // ftUnknown will be used for 32-bit integers, and ftNull for UTF-8 text
  DB_FIELDS: array[TSqlDBDefinition] of TSqlDBFieldTypeDefinition = (

  // dUnknown
    (' INT',             // ftUnknown = int32
     ' NVARCHAR(%)',     // ftNull    = UTF-8
     ' BIGINT',          // ftInt64
     ' DOUBLE',          // ftDouble
     ' NUMERIC(19,4)',   // ftCurrency
     ' TIMESTAMP',       // ftDate
     ' CLOB',            // ftUtf8
     ' BLOB'),           // ftBlob

  // dDefault
    (' INT',              // ftUnknown = int32
     ' NVARCHAR(%)',      // ftNull    = UTF-8
     ' BIGINT',           // ftInt64
     ' DOUBLE',           // ftDouble
     ' NUMERIC(19,4)',    // ftCurrency
     ' TIMESTAMP',        // ftDate
     ' CLOB',             // ftUtf8
     ' BLOB'),            // ftBlob

  // dOracle
    (' NUMBER(22,0)',      // ftUnknown = int32
     ' NVARCHAR2(%)',      // ftNull    = UTF-8
     ' NUMBER(22,0)',      // ftInt64
     ' BINARY_DOUBLE',     // ftDouble
     ' NUMBER(19,4)',      // ftCurrency
     ' DATE',              // ftDate
     ' NCLOB',             // ftUtf8
     ' BLOB'),             // ftBlob
    // NCLOB (National Character Large Object) is an Oracle data type that can hold
    // up to 4 GB of character data. It's similar to a CLOB, but characters are
    // stored in a NLS or multibyte national character set (like NVARCHAR2)

  // dMSSQL
    (' int',                // ftUnknown = int32
     ' nvarchar(%)',        // ftNull    = UTF-8
     ' bigint',             // ftInt64
     ' float',              // ftDouble
     ' money',              // ftCurrency
     ' datetime',           // ftDate
     ' nvarchar(max)',      // ftUtf8
     ' varbinary(max)'),    // ftBlob

  // dJet
    (' Long',                // ftUnknown = int32
     ' VarChar(%)',          // ftNull    = UTF-8
     ' Decimal(19,0)',       // ftInt64
     ' Double',              // ftDouble
     ' Currency',            // ftCurrency
     ' DateTime',            // ftDate
     ' LongText',            // ftUtf8
     ' LongBinary'),         // ftBlob

  // dMySQL
    (' int',                             // ftUnknown = int32
     ' varchar(%) character set utf8',   // ftNull    = UTF-8
     ' bigint',                          // ftInt64
     ' double',                          // ftDouble
     ' decimal(19,4)',                   // ftCurrency
     ' datetime',                        // ftDate
     ' mediumtext character set utf8',   // ftUtf8
     ' mediumblob'),                     // ftBlob

  // dSQLite
    (' INTEGER',              // ftUnknown = int32
     ' TEXT',                 // ftNull    = UTF-8
     ' INTEGER',              // ftInt64
     ' FLOAT',                // ftDouble
     ' FLOAT',                // ftCurrency
     ' TEXT',                 // ftDate
     ' TEXT',                 // ftUtf8
     ' BLOB'),                // ftBlob

  // dFirebird
    (' INTEGER',                                // ftUnknown = int32
     ' VARCHAR(%) CHARACTER SET UTF8',          // ftNull    = UTF-8
     ' BIGINT',                                 // ftInt64
     ' FLOAT',                                  // ftDouble
     ' DECIMAL(18,4)',                          // ftCurrency
     ' TIMESTAMP',                              // ftDate
     ' BLOB SUB_TYPE 1 SEGMENT SIZE 2000 ' +    // ftUtf8
        'CHARACTER SET UTF8',
     ' BLOB SUB_TYPE 0 SEGMENT SIZE 2000'),     // ftBlob
   // about BLOB: http://www.ibphoenix.com/resources/documents/general/doc_54

  // dNexusDB
    (' INTEGER',               // ftUnknown = int32
     ' NVARCHAR(%)',           // ftNull    = UTF-8
     ' LARGEINT',              // ftInt64
     ' REAL',                  // ftDouble
     ' MONEY',                 // ftCurrency
     ' DATETIME',              // ftDate
     ' NCLOB',                 // ftUtf8
     ' BLOB'),                 // ftBlob
    // VARCHAR(%) CODEPAGE 65001 just did not work well with Delphi<2009

  // dPostgreSQL
    (' INTEGER',                // ftUnknown = int32
     ' TEXT',                   // ftNull    = UTF-8
     ' BIGINT',                 // ftInt64
     ' DOUBLE PRECISION',       // ftDouble
     ' NUMERIC(19,4)',          // ftCurrency
     ' TIMESTAMP',              // ftDate
     ' TEXT',                   // ftUtf8
     ' BYTEA'),                 // ftBlob
    // like SQLite3, we will create TEXT column instead of VARCHAR(%), as stated
    // by http://www.postgresql.org/docs/current/static/datatype-character.html

  // dDB2 (for CCSID Unicode tables)
    (' int',                     // ftUnknown = int32
     ' varchar(%)',              // ftNull    = UTF-8
     ' bigint',                  // ftInt64
     ' real',                    // ftDouble
     ' decimal(19,4)',           // ftCurrency
     ' timestamp',               // ftDate
     ' clob',                    // ftUtf8
     ' blob'),                   // ftBlob
    // note: bigint needs 9.1 and up

  // dInformix
    (' int',                            // ftUnknown = int32
     ' lvarchar(%)',                    // ftNull    = UTF-8
     ' bigint',                         // ftInt64
     ' smallfloat',                     // ftDouble
     ' decimal(19,4)',                  // ftCurrency
     ' datetime year to fraction(3)',   // ftDate
     ' clob',                           // ftUtf8
     ' blob'));                         // ftBlob

  /// the known column data types corresponding to our TSqlDBFieldType types
  // - will be used e.g. for TSqlDBConnectionProperties.SqlFieldCreate()
  // - SQLite3 doesn't expect any field length, neither PostgreSQL, so set to 0
  DB_FIELDSMAX: array[TSqlDBDefinition] of cardinal = (
    1000,   // dUnknown
    1000,   // dDefault
    1333,   // dOracle        =4000/3 since WideChar is up to 3 bytes in UTF-8
    4000,   // dMSSQL
    255,    // dJet
    4000,   // dMySQL
    0,      // dSQLite
    32760,  // dFirebird
    32767,  // dNexusDB
    0,      // dPostgreSQL
    32700,  // dDB2
    32700); // dInformix

  /// the maximum number of bound parameters to a SQL statement
  // - will be used e.g. for Batch process multi-insert
  // - those values were done empirically, assuring total count is < 656,
  // which is the maximum within :AA..:ZZ range, excuding 20 reserved keywords
  // - see http://stackoverflow.com/a/6582902 for theoritical high limits
  DB_PARAMSMAX: array[TSqlDBDefinition] of cardinal = (
    0,              // dUnknown
    0,              // dDefault
    500,            // dOracle       empirical value (from ODBC)
    500,            // dMSSQL        theoritical=2100
    0,              // dJet
    500,            // dMySQL        theoritical=60000
    MAX_SQLPARAMS,  // dSQLite       theoritical=999 - see mormot.orm.sqlite3
    0,              // dFirebird
    100,            // dNexusDB      empirical limit (above is slower)
    500,            // dPostgreSQL   theoritical=34000
    500,            // dDB2          empirical value (from ODBC)
    0);             // dInformix

  /// the known SQL statement to retrieve the server date and time
  // - contains '' for the engines with local time
  DB_SERVERTIME: array[TSqlDBDefinition] of RawUtf8 = (
    '',                                                // dUnknown
    '',                                                // dDefault
    'select sysdate from dual',                        // dOracle
    'select GETDATE()',                                // dMSSQL
    '',                                                // dJet
    'SELECT NOW()',                                    // dMySQL
    '',                                                // dSQLite
    'select current_timestamp from rdb$database',      // dFirebird
    'SELECT CURRENT_TIMESTAMP',                        // dNexusDB
    'SELECT LOCALTIMESTAMP',                           // dPostgreSQL
    'select current timestamp from sysibm.sysdummy1',  // dDB2
    'select CURRENT YEAR TO FRACTION(3) ' +            // dInformix
       'from SYSTABLES where tabid = 1');

const
  /// the known SQL syntax to limit the number of returned rows in a SELECT
  // - Position indicates if should be included within the WHERE clause,
  // at the beginning of the SQL statement, or at the end of the SQL statement
  // - InsertFmt will replace '%' with the maximum number of lines to be retrieved
  // - used by TSqlDBConnectionProperties.AdaptSqlLimitForEngineList()
  DB_SQLLIMITCLAUSE: array[TSqlDBDefinition] of TSqlDBDefinitionLimitClause = (
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
  DB_HANDLECREATEINDEXIFNOTEXISTS = [dSQLite, dPostgreSQL];

  /// the known database engines handling CREATE INDEX on BLOB columns
  // - SQLite3 does not have any issue about indexing any column
  // - PostgreSQL is able to index TEXT columns, which are some kind of CLOB
  DB_HANDLEINDEXONBLOBS = [dSQLite, dPostgreSQL];

  /// where the DESC clause shall be used for a CREATE INDEX statement
  // - only identified syntax exception is for FireBird
  DB_SQLDESENDINGINDEXPOS: array[TSqlDBDefinition] of
    (posWithColumn, posGlobalBefore) =
    (posWithColumn,   // dUnknown
     posWithColumn,   // dDefault
     posWithColumn,   // dOracle
     posWithColumn,   // dMSSQL
     posWithColumn,   // dJet
     posWithColumn,   // dMySQL
     posWithColumn,   // dSQLite
     posGlobalBefore, // dFirebird
     posWithColumn,   // dNexusDB
     posWithColumn,   // dPostgreSQL
     posWithColumn,   // dDB2
     posWithColumn);  // dInformix

  /// the SQL text corresponding to the identified WHERE operators for a SELECT
  DB_SQLOPERATOR: array[opEqualTo..opLike] of RawUtf8 = (
    '=',            // opEqualTo
    '<>',           // opNotEqualTo
    '<',            // opLessThan
    '<=',           // opLessThanOrEqualTo
    '>',            // opGreaterThan
    '>=',           // opGreaterThanOrEqualTo
    ' in ',         // opIn
    ' is null',     // opIsNull
    ' is not null', // opIsNotNull
    ' like ');      // opLike


/// retrieve the text of a given Database SQL dialect enumeration
// - see also TSqlDBConnectionProperties.GetDbmsName() method
function ToText(Dbms: TSqlDBDefinition): PShortString; overload;


{ ************ General SQL Processing Functions }

/// function helper logging some column truncation information text
procedure LogTruncatedColumn(const Col: TSqlDBColumnProperty);

/// retrieve a table name without any left schema
// - e.g. TrimLeftSchema('SCHEMA.TABLENAME')='TABLENAME'
function TrimLeftSchema(const TableName: RawUtf8): RawUtf8;

/// replace all '?' in the SQL statement with named parameters like :AA :AB..
// - returns the number of ? parameters found within aSql
// - won't generate any SQL keyword parameters (e.g. :AS :OF :BY), to be
// compliant with Oracle OCI expectations - allow up to 656 parameters
// - any ending ';' character is deleted, unless aStripSemicolon is unset
function ReplaceParamsByNames(const aSql: RawUtf8; var aNewSql: RawUtf8;
  aStripSemicolon: boolean = true): integer;

/// replace all '?' in the SQL statement with indexed parameters like $1 $2 ...
// - returns the number of ? parameters found within aSql
// - as used e.g. by PostgreSQL & Oracle (:1 :2) library
// - if AllowSemicolon is false (by default), reject any statement with ;
// (Postgres do not allow ; inside prepared statement); it should be
// true for Oracle
function ReplaceParamsByNumbers(const aSql: RawUtf8; var aNewSql: RawUtf8;
  IndexChar: AnsiChar = '$'; AllowSemicolon: boolean = false): integer;

/// create a JSON array from an array of UTF-8 bound values
// - as generated during array binding, i.e. with quoted strings
// 'one','t"wo' -> '{"one","t\"wo"}'   and  1,2,3 -> '{1,2,3}'
// - as used e.g. by PostgreSQL library
function BoundArrayToJsonArray(const Values: TRawUtf8DynArray): RawUtf8;


{ ************ Abstract SQL DB Classes and Interfaces }

var
  /// the TSynLog class used for logging for all our mormot.db.sql related units
  // - since not all exceptions are handled specificaly by this unit, you
  // may better use a common TSynLog class for the whole application or module
  SynDBLog: TSynLogClass = TSynLog;

type
  /// a custom variant type used to have direct access to a result row content
  // - use ISqlDBRows.RowData method to retrieve such a Variant
  TSqlDBRowVariantType = class(TSynInvokeableVariantType)
  protected
    function IntGet(var Dest: TVarData; const Instance: TVarData;
      Name: PAnsiChar; NameLen: PtrInt; NoException: boolean): boolean; override;
  end;

  {$M+}
  TSqlDBStatement = class;
  TSqlDBConnection = class;
  TSqlDBConnectionProperties = class;
  {$M-}

  /// generic Exception type, as used by mormot.db.sql and mormot.db.sql.* units
  ESqlDBException = class(ESynException)
  protected
    fStatement: TSqlDBStatement;
  public
    /// constructor which will use FormatUtf8() instead of Format()
    // - if the first Args[0] is a TSqlDBStatement class instance, the current
    // SQL statement will be part of the exception message
    constructor CreateUtf8(const Format: RawUtf8; const Args: array of const);
  published
    /// associated TSqlDBStatement instance, if supplied as first parameter
    property Statement: TSqlDBStatement read fStatement;
  end;

  /// generic interface to access a SQL query result rows
  // - not all TSqlDBStatement methods are available, but only those to retrieve
  // data from a statement result: the purpose of this interface is to make
  // easy access to result rows, not provide all available features - therefore
  // you only have access to the Step() and Column*() methods
  ISqlDBRows = interface
    ['{11291095-9C15-4984-9118-974F1926DB9F}']
    /// after a prepared statement has been prepared returning a ISqlDBRows
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
    // !   with Props.Execute(
    // !       'select * from Sales.Customer where AccountNumber like ?',
    // !       ['AW000001%'], @Customer) do
    // !   begin
    // !     while Step do //  loop through all matching data rows
    // !       assert(Copy(Customer.AccountNumber, 1, 8)='AW000001');
    // !     ReleaseRows;
    // !   end;
    // ! end;
    function Step(SeekFirst: boolean = false): boolean;
    /// release cursor memory and resources once Step loop is finished
    // - this method call is optional, but is better be used if the ISqlDBRows
    // statement from taken from cache, and returned a lot of content which
    // may still be in client (and server) memory
    // - will also free all temporary memory used for optional logging
    procedure ReleaseRows;

    /// the column/field count of the current Row
    function ColumnCount: integer;
    /// the Column name of the current Row
    // - Columns numeration (i.e. Col value) starts with 0
    // - it's up to the implementation to ensure than all column names are unique
    function ColumnName(Col: integer): RawUtf8;
    /// returns the Column index of a given Column name
    // - Columns numeration (i.e. Col value) starts with 0
    // - returns -1 if the Column name is not found (via case insensitive search)
    function ColumnIndex(const aColumnName: RawUtf8): integer;
    /// the Column type of the current Row
    // - FieldSize can be set to store the size in chars of a ftUtf8 column
    // (0 means BLOB kind of TEXT column)
    function ColumnType(Col: integer; FieldSize: PInteger = nil): TSqlDBFieldType;
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
    function ColumnUtf8(Col: integer): RawUtf8; overload;
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
    /// return a Column as a TSqlVar value, first Col is 0
    // - the specified Temp variable will be used for temporary storage of
    // ftUtf8/ftBlob values
    procedure ColumnToSqlVar(Col: integer; var Value: TSqlVar; var Temp: RawByteString);
    /// return a Column as a variant
    // - a ftUtf8 TEXT content will be mapped into a generic WideString variant
    // for pre-Unicode version of Delphi, and a generic UnicodeString (=string)
    // since Delphi 2009: you may not loose any data during charset conversion
    // - a ftBlob BLOB content will be mapped into a TBlobData AnsiString variant
    function ColumnVariant(Col: integer): Variant; overload;
    /// return a Column as a variant, first Col is 0
    // - this default implementation will call Column*() method above
    // - a ftUtf8 TEXT content will be mapped into a generic WideString variant
    // for pre-Unicode version of Delphi, and a generic UnicodeString (=string)
    // since Delphi 2009: you may not loose any data during charset conversion
    // - a ftBlob BLOB content will be mapped into a TBlobData AnsiString variant
    function ColumnToVariant(Col: integer; var Value: Variant): TSqlDBFieldType; overload;
    /// return a special CURSOR Column content as a mormot.db.sql result set
    // - Cursors are not handled internally by mORMot, but some databases (e.g.
    // Oracle) usually use such structures to get data from stored procedures
    // - such columns are mapped as ftNull internally - so this method is the only
    // one giving access to the data rows
    // - see also BoundCursor() if you want to access a CURSOR out parameter
    function ColumnCursor(Col: integer): ISqlDBRows; overload;

    /// return a Column integer value of the current Row, from a supplied column name
    function ColumnInt(const ColName: RawUtf8): Int64; overload;
    /// return a Column floating point value of the current Row, from a supplied column name
    function ColumnDouble(const ColName: RawUtf8): double; overload;
    /// return a Column floating point value of the current Row, from a supplied column name
    function ColumnDateTime(const ColName: RawUtf8): TDateTime; overload;
    /// return a column date and time value of the current Row, from a supplied column name
    function ColumnTimestamp(const ColName: RawUtf8): TTimeLog; overload;
    /// return a Column currency value of the current Row, from a supplied column name
    function ColumnCurrency(const ColName: RawUtf8): currency; overload;
    /// return a Column UTF-8 encoded text value of the current Row, from a supplied column name
    function ColumnUtf8(const ColName: RawUtf8): RawUtf8; overload;
    /// return a Column text value as generic VCL string of the current Row, from a supplied column name
    function ColumnString(const ColName: RawUtf8): string; overload;
    /// return a Column as a blob value of the current Row, from a supplied column name
    function ColumnBlob(const ColName: RawUtf8): RawByteString; overload;
    /// return a Column as a blob value of the current Row, from a supplied column name
    function ColumnBlobBytes(const ColName: RawUtf8): TBytes; overload;
    /// read a blob Column into the Stream parameter
    procedure ColumnBlobToStream(const ColName: RawUtf8; Stream: TStream); overload;
    /// write a blob Column into the Stream parameter
    procedure ColumnBlobFromStream(const ColName: RawUtf8; Stream: TStream); overload;
    /// return a Column as a variant, from a supplied column name
    function ColumnVariant(const ColName: RawUtf8): Variant; overload;
    /// return a Column as a variant, from a supplied column name
    // - since a property getter can't be an overloaded method, we define one
    // for the Column[] property
    function GetColumnVariant(const ColName: RawUtf8): Variant;
    /// return a special CURSOR Column content as a mormot.db.sql result set
    // - Cursors are not handled internally by mORMot, but some databases (e.g.
    // Oracle) usually use such structures to get data from strored procedures
    // - such columns are mapped as ftNull internally - so this method is the only
    // one giving access to the data rows
    function ColumnCursor(const ColName: RawUtf8): ISqlDBRows; overload;
    /// return a Column as a variant
    // - this default property can be used to write simple code like this:
    // ! procedure WriteFamily(const aName: RawUtf8);
    // ! var I: ISqlDBRows;
    // ! begin
    // !   I := MyConnProps.Execute('select * from table where name=?',[aName]);
    // !   while I.Step do
    // !     writeln(I['FirstName'],' ',DateToStr(I['BirthDate']));
    // !   I.ReleaseRows;
    // ! end;
    // - of course, using a variant and a column name will be a bit slower than
    // direct access via the Column*() dedicated methods, but resulting code
    // is fast in practice
    property Column[const ColName: RawUtf8]: Variant
      read GetColumnVariant; default;
    /// create a TSqlDBRowVariantType able to access any field content via late binding
    // - i.e. you can use Data.Name to access the 'Name' column of the current row
    // - this Variant will point to the corresponding TSqlDBStatement instance,
    // so it's not necessary to retrieve its value for each row; but once the
    // associated ISqlDBRows instance is released, you won't be able to access
    // its data - use RowDocVariant instead
    // - typical use is:
    // ! var Row: Variant;
    // ! (...)
    // !  with MyConnProps.Execute('select * from table where name=?',[aName]) do
    // !  begin
    // !    Row := RowData;
    // !    while Step do
    // !      writeln(Row.FirstName,Row.BirthDate);
    // !    ReleaseRows;
    // !  end;
    function RowData: Variant;
    /// create a TDocVariant custom variant containing all columns values
    // - will create a "fast" TDocVariant object instance with all fields
    procedure RowDocVariant(out aDocument: variant;
      aOptions: TDocVariantOptions = JSON_FAST);
    /// return the associated statement instance
    function Instance: TSqlDBStatement;
    // return all rows content as a JSON string
    // - JSON data is retrieved with UTF-8 encoding
    // - if Expanded is true, JSON data is an array of objects, for direct use
    // with any Ajax or .NET client:
    // & [ {"col1":val11,"col2":"val12"},{"col1":val21,... ]
    // - if Expanded is false, JSON data is serialized (used in TOrmTableJson)
    // & { "FieldCount":1,"Values":["col1","col2",val11,"val12",val21,..] }
    // - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"'
    // format and contains true BLOB data
    // - if ReturnedRowCount points to an integer variable, it will be filled with
    // the number of row data returned (excluding field names)
    // - similar to corresponding TSqlRequest.Execute method in the
    // mormot.db.raw.sqlite3 unit
    function FetchAllAsJson(Expanded: boolean; ReturnedRowCount: PPtrInt = nil): RawUtf8;
    // append all rows content as a JSON stream
    // - JSON data is added to the supplied TStream, with UTF-8 encoding
    // - if Expanded is true, JSON data is an array of objects, for direct use
    // with any Ajax or .NET client:
    // & [ {"col1":val11,"col2":"val12"},{"col1":val21,... ]
    // - if Expanded is false, JSON data is serialized (used in TOrmTableJson)
    // & { "FieldCount":1,"Values":["col1","col2",val11,"val12",val21,..] }
    // - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"'
    // format and contains true BLOB data
    // - similar to corresponding TSqlRequest.Execute method in the
    // mormot.db.raw.sqlite3 unit
    // - returns the number of row data returned (excluding field names)
    function FetchAllToJson(Json: TStream; Expanded: boolean): PtrInt;
    /// append all rows content as binary stream
    // - will save the column types and name, then every data row in optimized
    // binary format (faster and smaller than JSON)
    // - you can specify a LIMIT for the data extent (default 0 meaning all data)
    // - generates the format expected by TSqlDBProxyStatement
    function FetchAllToBinary(Dest: TStream; MaxRowCount: cardinal = 0;
      DataRowPosition: PCardinalDynArray = nil): cardinal;
  end;

  /// generic interface to bind to prepared SQL query
  // - inherits from ISqlDBRows, so gives access to the result columns data
  // - not all TSqlDBStatement methods are available, but only those to bind
  // parameters and retrieve data after execution
  // - reference counting mechanism of this interface will feature statement
  // cache (if available) for NewThreadSafeStatementPrepared() or PrepareInlined()
  ISqlDBStatement = interface(ISqlDBRows)
    ['{EC27B81C-BD57-47D4-9711-ACFA27B583D7}']
    // some raw properties getter/setter
    function GetForceBlobAsNull: boolean;
    procedure SetForceBlobAsNull(value: boolean);
    function GetForceDateWithMS: boolean;
    procedure SetForceDateWithMS(value: boolean);
    /// bind a NULL value to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - some providers (e.g. OleDB during MULTI INSERT statements) expect the
    // proper column type to be set in BoundType, even for NULL values
    procedure BindNull(Param: integer; IO: TSqlDBParamInOutType = paramIn;
      BoundType: TSqlDBFieldType = ftNull);
    /// bind an integer value to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure Bind(Param: integer; Value: Int64;
      IO: TSqlDBParamInOutType = paramIn); overload;
    /// bind a double value to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure Bind(Param: integer; Value: double;
      IO: TSqlDBParamInOutType = paramIn); overload;
    /// bind a TDateTime value to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindDateTime(Param: integer; Value: TDateTime;
      IO: TSqlDBParamInOutType = paramIn); overload;
    /// bind a currency value to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindCurrency(Param: integer; Value: currency;
      IO: TSqlDBParamInOutType = paramIn); overload;
    /// bind a UTF-8 encoded string to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindTextU(Param: integer; const Value: RawUtf8;
      IO: TSqlDBParamInOutType = paramIn); overload;
    /// bind a UTF-8 encoded buffer text (#0 ended) to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindTextP(Param: integer; Value: PUtf8Char;
      IO: TSqlDBParamInOutType = paramIn); overload;
    /// bind a UTF-8 encoded string to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindTextS(Param: integer; const Value: string;
      IO: TSqlDBParamInOutType = paramIn); overload;
    /// bind a UTF-8 encoded string to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindTextW(Param: integer; const Value: WideString;
      IO: TSqlDBParamInOutType = paramIn); overload;
    /// bind a Blob buffer to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindBlob(Param: integer; Data: pointer; Size: integer;
      IO: TSqlDBParamInOutType = paramIn); overload;
    /// bind a Blob buffer to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindBlob(Param: integer; const Data: RawByteString;
      IO: TSqlDBParamInOutType = paramIn); overload;
    /// bind a Variant value to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - will call all virtual Bind*() methods from the Data type
    // - if DataIsBlob is TRUE, will call BindBlob(RawByteString(Data)) instead
    // of BindTextW(WideString(Variant)) - used e.g. by TQuery.AsBlob/AsBytes
    procedure BindVariant(Param: integer; const Data: Variant;
      DataIsBlob: boolean; IO: TSqlDBParamInOutType = paramIn);
    /// bind one TSqlVar value
    // - the leftmost SQL parameter has an index of 1
    procedure Bind(Param: integer; const Data: TSqlVar;
      IO: TSqlDBParamInOutType = paramIn); overload;
    /// bind one RawUtf8 encoded value
    // - the leftmost SQL parameter has an index of 1
    // - the value should match the BindArray() format, i.e. be stored as in SQL
    // (i.e. number, 'quoted string', 'YYYY-MM-DD hh:mm:ss', null)
    procedure Bind(Param: integer; ParamType: TSqlDBFieldType; const Value: RawUtf8;
      ValueAlreadyUnquoted: boolean; IO: TSqlDBParamInOutType = paramIn); overload;
    /// bind an array of const values
    // - parameters marked as ? should be specified as method parameter in Params[]
    // - BLOB parameters can be bound with this method, when set after encoding
    // via BinToBase64WithMagic() call
    // - TDateTime parameters can be bound with this method, when encoded via
    // a DateToSql() or DateTimeToSql() call
    procedure Bind(const Params: array of const;
      IO: TSqlDBParamInOutType = paramIn); overload;
    /// bind an array of fields from an existing SQL statement
    // - can be used e.g. after ColumnsToSqlInsert() method call for fast data
    // conversion between tables
    procedure BindFromRows(const Fields: TSqlDBFieldTypeDynArray; Rows: TSqlDBStatement);
    /// bind a special CURSOR parameter to be returned as a mormot.db.sql result set
    // - Cursors are not handled internally by mORMot, but some databases (e.g.
    // Oracle) usually use such structures to get data from strored procedures
    // - such parameters are mapped as ftUnknown
    // - use BoundCursor() method to retrieve the corresponding ISqlDBRows after
    // execution of the statement
    procedure BindCursor(Param: integer);
    /// return a special CURSOR parameter content as a mormot.db.sql result set
    // - this method is not about a column, but a parameter defined with
    // BindCursor() before method execution
    // - Cursors are not handled internally by mORMot, but some databases (e.g.
    // Oracle) usually use such structures to get data from strored procedures
    // - this method allow direct access to the data rows after execution
    function BoundCursor(Param: integer): ISqlDBRows;

    /// bind an array of values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - values are stored as in SQL (i.e. number, 'quoted string',
    // 'YYYY-MM-DD hh:mm:ss', null)
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    procedure BindArray(Param: integer; ParamType: TSqlDBFieldType;
      const Values: TRawUtf8DynArray; ValuesCount: integer); overload;
    /// bind an array of integer values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    procedure BindArray(Param: integer; const Values: array of Int64); overload;
    /// bind an array of double values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    procedure BindArray(Param: integer; const Values: array of double); overload;
    /// bind an array of TDateTime values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - values are stored as in SQL (i.e. 'YYYY-MM-DD hh:mm:ss')
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    procedure BindArrayDateTime(Param: integer; const Values: array of TDateTime);
    /// bind an array of currency values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    procedure BindArrayCurrency(Param: integer; const Values: array of currency);
    /// bind an array of RawUtf8 values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - values are stored as in SQL (i.e. 'quoted string')
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    procedure BindArray(Param: integer; const Values: array of RawUtf8); overload;

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
    function ParamToVariant(Param: integer; var Value: Variant;
      CheckIsOutParameter: boolean = true): TSqlDBFieldType;

    /// execute a prepared SQL statement
    // - parameters marked as ? should have been already bound with Bind*() functions
    // - should raise an Exception on any error
    // - after execution, you can access any returned data via ISqlDBRows methods
    procedure ExecutePrepared;
    // execute a prepared SQL statement and return all rows content as a JSON string
    // - JSON data is retrieved with UTF-8 encoding
    // - if Expanded is true, JSON data is an array of objects, for direct use
    // with any Ajax or .NET client:
    // & [ {"col1":val11,"col2":"val12"},{"col1":val21,... ]
    // - if Expanded is false, JSON data is serialized (used in TOrmTableJson)
    // & { "FieldCount":1,"Values":["col1","col2",val11,"val12",val21,..] }
    // - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"'
    // format and contains true BLOB data
    procedure ExecutePreparedAndFetchAllAsJson(Expanded: boolean; out Json: RawUtf8);
    /// if set, any BLOB field won't be retrieved, and forced to be null
    // - this may be used to speed up fetching the results for SQL requests
    // with * statements
    property ForceBlobAsNull: boolean
      read GetForceBlobAsNull write SetForceBlobAsNull;
    /// if set, any ftDate field will contain the milliseconds information
    // when serialized into ISO-8601 text
    // - this setting is private to each statement, since may vary depending
    // on data definition (e.g. ORM TDateTime/TDateTimeMS)
    property ForceDateWithMS: boolean
      read GetForceDateWithMS write SetForceDateWithMS;
    /// gets a number of updates made by latest executed statement
    function UpdateCount: integer;
  end;

  /// possible events notified to TOnSqlDBProcess callback method
  // - event handler is specified by TSqlDBConnectionProperties.OnProcess or
  // TSqlDBConnection.OnProcess properties
  // - speConnected / speDisconnected will notify TSqlDBConnection.Connect
  // and TSqlDBConnection.Disconnect calls
  // - speNonActive / speActive will be used to notify external DB blocking
  // access, so can be used e.g. to change the mouse cursor shape (this trigger
  // is re-entrant, i.e. it will be executed only once in case of nested calls)
  // - speReconnected will be called if TSqlDBConnection did successfully
  // recover its database connection (on error, TQuery will call
  // speConnectionLost): this event will be called by TSqlDBConnection.Connect
  // after a regular speConnected notification
  // - speConnectionLost will be called by TQuery in case of broken connection,
  // and if Disconnect/Reconnect did not restore it as expected (i.e. speReconnected)
  // - speStartTransaction / speCommit / speRollback will notify the
  // corresponding TSqlDBConnection.StartTransaction, TSqlDBConnection.Commit
  // and TSqlDBConnection.Rollback methods
  TOnSqlDBProcessEvent = (
    speConnected,
    speDisconnected,
    speNonActive,
    speActive,
    speConnectionLost,
    speReconnected,
    speStartTransaction,
    speCommit,
    speRollback);

  /// event handler called during all external DB process
  // - event handler is specified by TSqlDBConnectionProperties.OnProcess or
  // TSqlDBConnection.OnProperties properties
  TOnSqlDBProcess = procedure(Sender: TSqlDBConnection; Event:
    TOnSqlDBProcessEvent) of object;

  /// event triggered when an expired password is detected
  // - will allow to provide a new password
  TOnPasswordExpired = function (Sender: TSqlDBConnection;
    var APassword: RawUtf8): boolean of object;

  /// event handler called when the low-level driver send some warning information
  // - errors will trigger Exceptions, but sometimes the database driver returns
  // some non critical information, which is logged and may be intercepted using
  // the TSqlDBConnectionProperties.OnStatementInfo property
  // - may be used e.g. to track ORA-28001 or ORA-28002 about account expire
  // - is currently implemented by mormot.db.sql.oracle, mormot.db.sql.odbc
  // and mormot.db.sql.oledb units
  TOnSqlDBInfo = procedure(Sender: TSqlDBStatement; const Msg: RawUtf8) of object;

  /// actions implemented by TSqlDBConnectionProperties.SharedTransaction()
  TSqlDBSharedTransactionAction = (
    transBegin,
    transCommitWithoutException,
    transCommitWithException,
    transRollback);

  /// defines a callback signature able to handle multiple INSERT
  // - may execute e.g. for 2 fields and 3 data rows on a database engine
  // implementing INSERT with multiple VALUES (like MySQL, PostgreSQL, NexusDB,
  // MSSQL or SQlite3), as implemented by
  // TSqlDBConnectionProperties.MultipleValuesInsert() :
  // $ INSERT INTO TableName(FieldNames[0],FieldNames[1]) VALUES
  // $   (FieldValues[0][0],FieldValues[1][0]),
  // $   (FieldValues[0][1],FieldValues[1][1]),
  // $   (FieldValues[0][2],FieldValues[1][2]);
  // - for other kind of DB which do not support multi values INSERT, may
  // execute a dedicated driver command, like MSSQL "bulk insert" or Firebird
  // "execute block"
  TOnBatchInsert = procedure(Props: TSqlDBConnectionProperties;
    const TableName: RawUtf8; const FieldNames: TRawUtf8DynArray;
    const FieldTypes: TSqlDBFieldTypeArray; RowCount: integer;
    const FieldValues: TRawUtf8DynArrayDynArray) of object;

  /// specify the class of TSqlDBConnectionProperties
  // - sometimes used to create connection properties instances, from a set
  // of available classes (see e.g. SynDBExplorer or sample 16)
  TSqlDBConnectionPropertiesClass = class of TSqlDBConnectionProperties;

  /// abstract class used to set Database-related properties
  // - handle e.g. the Database server location and connection parameters (like
  // UserID and password)
  // - should also provide some Database-specific generic SQL statement creation
  // (e.g. how to create a Table), to be used e.g. by the mORMot layer
  // - this class level will handle a single "main connection" - you may inherit
  // from TSqlDBConnectionThreadSafe to maintain one connection per thread
  TSqlDBConnectionProperties = class
  protected
    fServerName: RawUtf8;
    fDatabaseName: RawUtf8;
    fPassWord: RawUtf8;
    fUserID: RawUtf8;
    fForcedSchemaName: RawUtf8;
    fMainConnection: TSqlDBConnection;
    fBatchMaxSentAtOnce: integer;
    fLoggedSqlMaxSize: integer;
    fConnectionTimeOutTicks: Int64;
    fOnBatchInsert: TOnBatchInsert;
    fDbms: TSqlDBDefinition;
    fBatchSendingAbilities: TSqlDBStatementCRUDs;
    fUseCache, fStoreVoidStringAsNull, fLogSqlStatementOnException,
      fRollbackOnDisconnect, fReconnectAfterConnectionError,
      fFilterTableViewSchemaName: boolean;
    fDateTimeFirstChar: AnsiChar;
    {$ifndef UNICODE}
    fVariantWideString: boolean;
    {$endif UNICODE}
    fStatementMaxMemory: Int64;
    fSqlGetServerTimestamp: RawUtf8;
    fEngineName: RawUtf8;
    fOnProcess: TOnSqlDBProcess;
    fOnStatementInfo: TOnSqlDBInfo;
    fStatementCacheReplicates: integer;
    fSqlCreateField: TSqlDBFieldTypeDefinition;
    fSqlCreateFieldMax: cardinal;
    fSharedTransactions: array of record
      SessionID: cardinal;
      RefCount: integer;
      Connection: TSqlDBConnection;
    end;
    fExecuteWhenConnected: TRawUtf8DynArray;
    fForeignKeys: TSynNameValue;
    procedure SetConnectionTimeOutMinutes(minutes: cardinal);
    function GetConnectionTimeOutMinutes: cardinal;
    // this default implementation just returns the fDbms value or dDefault
    // (never returns dUnknwown)
    function GetDbms: TSqlDBDefinition; virtual;
    function GetDbmsName: RawUtf8; virtual;
    function GetForeignKeysData: RawByteString;
    procedure SetForeignKeysData(const Value: RawByteString);
    function FieldsFromList(const aFields: TSqlDBColumnDefineDynArray;
      aExcludeTypes: TSqlDBFieldTypes): RawUtf8;
    function GetMainConnection: TSqlDBConnection; virtual;
    function GetDatabaseNameSafe: RawUtf8; virtual;
    /// any overriden TSqlDBConnectionProperties class should call it in the
    // initialization section of its implementation unit to be recognized
    class procedure RegisterClassNameForDefinition;
    /// will be called at the end of constructor
    // - this default implementation will do nothing
    procedure SetInternalProperties; virtual;
    /// Assign schema name to owner from ForceSchemaName or UserID or Database name
    procedure SetSchemaNameToOwner(out Owner: RawUtf8); virtual;
    /// SQL statement to get all field/column names for a specified Table
    // - used by GetFieldDefinitions public method
    // - should return a SQL "SELECT" statement with the field names as first
    // column, a textual field type as 2nd column, then field length, then
    // numeric precision and scale as 3rd, 4th and 5th columns, and the index
    // count in 6th column
    // - this default implementation just returns nothing
    // - if this method is overridden, the ColumnTypeNativeToDB() method should
    // also be overridden in order to allow conversion from native column
    // type into the corresponding TSqlDBFieldType
    function SqlGetField(const aTableName: RawUtf8): RawUtf8; virtual;
    /// SQL statement to get advanced information about all indexes for a Table
    // - should return a SQL "SELECT" statement with the index names as first
    function SqlGetIndex(const aTableName: RawUtf8): RawUtf8; virtual;
    /// SQL statement to get all parameter for a specified Stored Procedure
    // - used by GetProcedureParameters public method
    // - should return a SQL "SELECT" statement with the parameter names as first
    // column, a textual field type as 2nd column, then parameter length as 3rd, then
    // parameter direction as 4th
    // - this default implementation just returns nothing
    // - if this method is overridden, the ColumnTypeNativeToDB() method should
    // also be overridden in order to allow conversion from native column
    // type into the corresponding TSqlDBFieldType
    function SqlGetParameter(const aProcName: RawUtf8): RawUtf8; virtual;
    /// SQL statement to get all stored procedure names for current connection
    // - used by GetProcedureNames public method
    // - should return a SQL "SELECT" statement with the procedure names as unique column
    // - this default implementation just returns nothing
    // - if this method is overridden, the ColumnTypeNativeToDB() method should
    // also be overridden in order to allow conversion from native column
    // type into the corresponding TSqlDBFieldType
    function SqlGetProcedure: RawUtf8; virtual;
    /// SQL statement to get all table names
    // - used by GetTableNames public method
    // - should return a SQL "SELECT" statement with the table names as
    // first column (any other columns will be ignored)
    // - this default implementation just returns nothing
    function SqlGetTableNames: RawUtf8; virtual;
    /// SQL statement to get all view names
    // - used by GetViewNames public method
    // - should return a SQL "SELECT" statement with the view names as
    // first column (any other columns will be ignored)
    // - this default implementation just returns nothing
    function SqlGetViewNames: RawUtf8; virtual;
    /// should initialize fForeignKeys content with all foreign keys of this
    // database
    // - used by GetForeignKey method
    procedure GetForeignKeys; virtual; abstract;
    /// will use fSqlCreateField[Max] to create the SQL column definition
    // - this default virtual implementation will handle properly all supported
    // database engines, assuming aField.ColumnType as in TSqlDBFieldTypeDefinition
    // - if the field is a primary key, aAddPrimaryKey may be modified to contain
    // some text to be appended at the end of the ALTER/CREATE TABLE statement
    function SqlFieldCreate(const aField: TSqlDBColumnCreate;
      var aAddPrimaryKey: RawUtf8): RawUtf8; virtual;
    /// wrapper around GetIndexes() + set Fields[].ColumnIndexed in consequence
    // - used by some overridden versions of GetFields() method
    procedure GetIndexesAndSetFieldsColumnIndexed(const aTableName: RawUtf8;
      var Fields: TSqlDBColumnDefineDynArray);
    /// check if the exception or its error message is about DB connection error
    // - will be used by TSqlDBConnection.LastErrorWasAboutConnection method
    // - default method will check for the 'conne' sub-string in the message text
    // - should be overridden depending on the error message returned by the DB
    function ExceptionIsAboutConnection(aClass: ExceptClass;
      const aMessage: RawUtf8): boolean; virtual;
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
    procedure MultipleValuesInsert(Props: TSqlDBConnectionProperties;
      const TableName: RawUtf8; const FieldNames: TRawUtf8DynArray;
      const FieldTypes: TSqlDBFieldTypeArray; RowCount: integer;
      const FieldValues: TRawUtf8DynArrayDynArray);
    /// Firebird-dedicated method able to implement OnBatchInsert()
    // - will run an EXECUTE BLOCK statement without any parameters, but
    // including inlined values - sounds to be faster on ZEOS/ZDBC!
    procedure MultipleValuesInsertFirebird(Props: TSqlDBConnectionProperties;
      const TableName: RawUtf8; const FieldNames: TRawUtf8DynArray;
      const FieldTypes: TSqlDBFieldTypeArray; RowCount: integer;
      const FieldValues: TRawUtf8DynArrayDynArray);
  public
    /// initialize the properties
    // - children may optionaly handle the fact that no UserID or Password
    // is supplied here, by displaying a corresponding Dialog box
    constructor Create(
      const aServerName, aDatabaseName, aUserID, aPassWord: RawUtf8); virtual;
    /// release related memory, and close MainConnection
    destructor Destroy; override;
    /// save the properties into a persistent storage object
    // - you can use TSqlDBConnectionPropertiesDescription.CreateFrom()
    // later on to instantiate the proper TSqlDBConnectionProperties class
    // - current Definition.Key value will be used for the password encryption
    procedure DefinitionTo(Definition: TSynConnectionDefinition); virtual;
    /// save the properties into a JSON file
    // - you could use TSqlDBConnectionPropertiesDescription.CreateFromJson()
    // later on to instantiate the proper TSqlDBConnectionProperties class
    // - you can specify a custom Key, if the default is not enough for you
    function DefinitionToJson(Key: cardinal = 0): RawUtf8; virtual;
    /// save the properties into a JSON file
    // - you could use TSqlDBConnectionPropertiesDescription.CreateFromFile()
    // later on to instantiate the proper TSqlDBConnectionProperties class
    // - you can specify a custom Key, if the default is not enough for you
    procedure DefinitionToFile(const aJsonFile: TFileName; Key: cardinal = 0);
    /// create a new TSqlDBConnectionProperties instance from the stored values
    class function CreateFrom(
      aDefinition: TSynConnectionDefinition): TSqlDBConnectionProperties; virtual;
    /// create a new TSqlDBConnectionProperties instance from a JSON content
    // - as previously serialized with TSqlDBConnectionProperties.DefinitionToJson
    // - you can specify a custom Key, if the default is not safe enough for you
    class function CreateFromJson(
      const aJsonDefinition: RawUtf8; aKey: cardinal = 0): TSqlDBConnectionProperties; virtual;
    /// create a new TSqlDBConnectionProperties instance from a JSON file
    // - as previously serialized with TSqlDBConnectionProperties.DefinitionToFile
    // - you can specify a custom Key, if the default is not safe enough for you
    class function CreateFromFile(
      const aJsonFile: TFileName; aKey: cardinal = 0): TSqlDBConnectionProperties;
    /// retrieve the registered class from the aDefinition.Kind string
    class function ClassFrom(
      aDefinition: TSynConnectionDefinition): TSqlDBConnectionPropertiesClass;

    /// create a new connection
    // - call this method if the shared MainConnection is not enough (e.g. for
    // multi-thread access)
    // - the caller is responsible of freeing this instance
    function NewConnection: TSqlDBConnection; virtual;
    /// get a thread-safe connection
    // - this default implementation will return the MainConnection shared
    // instance, so the provider should be thread-safe by itself
    // - TSqlDBConnectionPropertiesThreadSafe will implement a per-thread
    // connection pool, via an internal TSqlDBConnection pool, per thread
    // if necessary (e.g. for OleDB, which expect one TOleDBConnection instance
    // per thread)
    function ThreadSafeConnection: TSqlDBConnection; virtual;
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
    // TSqlDBConnectionProperties instance, then call ClearConnectionPool
    // to release all active connections if the idle time elapsed was too long
    // - warning: no connection shall still be used on the background (e.g. in
    // multi-threaded applications), or some unexpected issues may occur - for
    // instance, ensure that your mORMot ORM server runs all its statements in
    // blocking mode for both read and write:
    // ! aServer.AcquireExecutionMode[execOrmGet] := am***;
    // ! aServer.AcquireExecutionMode[execOrmWrite] := am***;
    // here, safe blocking am*** modes are any mode but amUnlocked, i.e. either
    // amLocked, amBackgroundThread or amMainThread
    property ConnectionTimeOutMinutes: cardinal
      read GetConnectionTimeOutMinutes write SetConnectionTimeOutMinutes;
    /// intercept connection errors at statement preparation and try to reconnect
    // - i.e. detect TSqlDBConnection.LastErrorWasAboutConnection in
    // TSqlDBConnection.NewStatementPrepared
    // - warning: no connection shall still be used on the background (e.g. in
    // multi-threaded applications), or some unexpected issues may occur - see
    // AcquireExecutionMode[] recommendations in ConnectionTimeOutMinutes
    property ReconnectAfterConnectionError: boolean
      read fReconnectAfterConnectionError write fReconnectAfterConnectionError;
    /// create a new thread-safe statement
    // - this method will call ThreadSafeConnection.NewStatement
    function NewThreadSafeStatement: TSqlDBStatement;
    /// create a new thread-safe statement from an internal cache (if any)
    // - will call ThreadSafeConnection.NewStatementPrepared
    // - this method should return a prepared statement instance on success
    // - on error, returns nil and you can check Connnection.LastErrorMessage /
    // Connection.LastErrorException to retrieve corresponding error information
    // (if RaiseExceptionOnError is left to default FALSE value, otherwise, it will
    // raise an exception)
    function NewThreadSafeStatementPrepared(const aSql: RawUtf8;
      ExpectResults: boolean; RaiseExceptionOnError: boolean = false): ISqlDBStatement; overload;
    /// create a new thread-safe statement from an internal cache (if any)
    // - this method will call the overloaded NewThreadSafeStatementPrepared method
    // - here Args[] array does not refer to bound parameters, but to values
    // to be changed within SqlFormat in place of '%' characters (this method
    // will call FormatUtf8() internally); parameters will be bound directly
    // on the returned TSqlDBStatement instance
    // - this method should return a prepared statement instance on success
    // - on error, returns nil and you can check Connnection.LastErrorMessage /
    // Connection.LastErrorException to retrieve correspnding error information
    // (if RaiseExceptionOnError is left to default FALSE value, otherwise, it will
    // raise an exception)
    function NewThreadSafeStatementPrepared(const SqlFormat: RawUtf8;
      const Args: array of const; ExpectResults: boolean;
      RaiseExceptionOnError: boolean = false): ISqlDBStatement; overload;
    /// create, prepare and bound inlined parameters to a thread-safe statement
    // - this implementation will call the NewThreadSafeStatement virtual method,
    // then bound inlined parameters as :(1234): and return the resulting statement
    // - raise an exception on error
    // - consider using ExecuteInlined() for direct execution
    function PrepareInlined(const aSql: RawUtf8; ExpectResults: boolean): ISqlDBStatement; overload;
    /// create, prepare and bound inlined parameters to a thread-safe statement
    // - overloaded method using FormatUtf8() and inlined parameters
    // - consider using ExecuteInlined() for direct execution
    function PrepareInlined(const SqlFormat: RawUtf8; const Args: array of const;
      ExpectResults: boolean): ISqlDBStatement; overload;
    /// execute a SQL query, returning a statement interface instance to retrieve
    // the result rows corresponding to the supplied SELECT statement
    // - will call NewThreadSafeStatement method to retrieve a thread-safe
    // statement instance, then run the corresponding Execute() method
    // - raise an exception on error
    // - returns an ISqlDBRows to access any resulting rows (if ExpectResults is
    // TRUE), and provide basic garbage collection, as such:
    // ! procedure WriteFamily(const aName: RawUtf8);
    // ! var I: ISqlDBRows;
    // ! begin
    // !   I := MyConnProps.Execute('select * from table where name=?',[aName]);
    // !   while I.Step do
    // !     writeln(I['FirstName'],' ',DateToStr(I['BirthDate']));
    // !   I.ReleaseRows;
    // ! end;
    // - if RowsVariant is set, you can use it to row column access via late
    // binding, as such:
    // ! procedure WriteFamily(const aName: RawUtf8);
    // ! var R: Variant;
    // ! begin
    // !   with MyConnProps.Execute('select * from table where name=?',[aName],@R) do
    // !   begin
    // !     while Step do
    // !       writeln(R.FirstName,' ',DateToStr(R.BirthDate));
    // !     ReleaseRows;
    // !   end;
    // ! end;
    // - you can any BLOB field to be returned as null with the ForceBlobAsNull
    // optional parameter
    function Execute(const aSql: RawUtf8; const Params: array of const;
      RowsVariant: PVariant = nil; ForceBlobAsNull: boolean = false): ISqlDBRows;
    /// execute a SQL query, without returning any rows
    // - can be used to launch INSERT, DELETE or UPDATE statement, e.g.
    // - will call NewThreadSafeStatement method to retrieve a thread-safe
    // statement instance, then run the corresponding Execute() method
    // - return the number of modified rows, i.e. the ISqlDBStatement.UpdateCount
    // value (or 0 if the DB driver does not supply this value)
    function ExecuteNoResult(const aSql: RawUtf8; const Params: array of const): integer;
    /// create, prepare, bound inlined parameters and execute a thread-safe statement
    // - this implementation will call the NewThreadSafeStatement virtual method,
    // then bound inlined parameters as :(1234): and call its Execute method
    // - raise an exception on error
    function ExecuteInlined(const aSql: RawUtf8;
      ExpectResults: boolean): ISqlDBRows; overload;
    /// create, prepare, bound inlined parameters and execute a thread-safe statement
    // - overloaded method using FormatUtf8() and inlined parameters
    function ExecuteInlined(const SqlFormat: RawUtf8; const Args: array of const;
      ExpectResults: boolean): ISqlDBRows; overload;
    /// handle a transaction process common to all associated connections
    // - could be used to share a single transaction among several connections,
    // or to run nested transactions even on DB engines which do not allow them
    // - will use a simple reference counting mechanism to allow nested
    // transactions, identified by a session identifier
    // - will fail if the same connection is not used for the whole process,
    // which would induce a potentially incorrect behavior
    // - returns the connection corresponding to the session, nil on error
    function SharedTransaction(SessionID: cardinal; action:
      TSqlDBSharedTransactionAction): TSqlDBConnection; virtual;

    /// convert a textual column data type, as retrieved e.g. from SqlGetField,
    // into our internal primitive types
    // - default implementation will always return ftUtf8
    function ColumnTypeNativeToDB(const aNativeType: RawUtf8;
      aScale: integer): TSqlDBFieldType; virtual;
    /// returns the SQL statement used to create a Table
    // - should return the SQL "CREATE" statement needed to create a table with
    // the specified field/column names and types
    // - if aAddID is TRUE, "ID Int64 PRIMARY KEY" column is added as first,
    // and will expect the ORM to create an unique RowID value sent at INSERT
    // (could use "select max(ID) from table" to retrieve the last value) -
    // note that 'ID' is used instead of 'RowID' since it fails on Oracle e.g.
    // - this default implementation will use internal fSqlCreateField and
    // fSqlCreateFieldMax protected values, which contains by default the
    // ANSI SQL Data Types and maximum 1000 inlined WideChars: inherited classes
    // may change the default fSqlCreateField* content or override this method
    function SqlCreate(const aTableName: RawUtf8;
      const aFields: TSqlDBColumnCreateDynArray; aAddID: boolean): RawUtf8; virtual;
    /// returns the SQL statement used to add a column to a Table
    // - should return the SQL "ALTER TABLE" statement needed to add a column to
    // an existing table
    // - this default implementation will use internal fSqlCreateField and
    // fSqlCreateFieldMax protected values, which contains by default the
    // ANSI SQL Data Types and maximum 1000 inlined WideChars: inherited classes
    // may change the default fSqlCreateField* content or override this method
    function SqlAddColumn(const aTableName: RawUtf8;
      const aField: TSqlDBColumnCreate): RawUtf8; virtual;
    /// returns the SQL statement used to add an index to a Table
    // - should return the SQL "CREATE INDEX" statement needed to add an index
    // to the specified column names of an existing table
    // - index will expect UNIQUE values in the specified columns, if Unique
    // parameter is set to true
    // - this default implementation will return the standard SQL statement, i.e.
    // 'CREATE [UNIQUE] INDEX index_name ON table_name (column_name[s])'
    function SqlAddIndex(const aTableName: RawUtf8;
      const aFieldNames: array of RawUtf8; aUnique: boolean;
      aDescending: boolean = false; const aIndexName: RawUtf8 = ''): RawUtf8; virtual;
    /// used to compute a SELECT statement for the given fields
    // - should return the SQL "SELECT ... FROM ..." statement to retrieve
    // the specified column names of an existing table
    // - by default, all columns specified in aFields[] will be available:
    // it will return "SELECT * FROM TableName"
    // - but if you specify a value in aExcludeTypes, it will compute the
    // matching column names to ignore those kind of content (e.g. [stBlob] to
    // save time and space)
    function SqlSelectAll(const aTableName: RawUtf8;
      const aFields: TSqlDBColumnDefineDynArray;
      aExcludeTypes: TSqlDBFieldTypes): RawUtf8; virtual;
    /// SQL statement to create the corresponding database
    // - this default implementation will only handle dFirebird by now
    function SqlCreateDatabase(const aDatabaseName: RawUtf8;
      aDefaultPageSize: integer = 0): RawUtf8; virtual;
    /// convert an ISO-8601 encoded time and date into a date appropriate to
    // be pasted in the SQL request
    // - this default implementation will return the quoted ISO-8601 value, i.e.
    // 'YYYY-MM-DDTHH:MM:SS' (as expected by Microsoft SQL server e.g.)
    // - returns  to_date('....','YYYY-MM-DD HH24:MI:SS')  for Oracle
    function SqlIso8601ToDate(const Iso8601: RawUtf8): RawUtf8; virtual;
    /// convert a TDateTime into a ISO-8601 encoded time and date, as expected
    // by the database provider
    // - e.g. SQLite3, DB2 and PostgreSQL will use non-standard ' ' instead of 'T'
    function SqlDateToIso8601Quoted(DateTime: TDateTime): RawUtf8; virtual;
    /// split a table name to its OWNER.TABLE full name (if applying)
    // - will use ForcedSchemaName property (if applying), or the OWNER. already
    // available within the supplied table name
    procedure SqlSplitTableName(const aTableName: RawUtf8;
      out Owner, Table: RawUtf8); virtual;
    /// split a procedure name to its OWNER.PACKAGE.PROCEDURE full name (if applying)
    // - will use ForcedSchemaName property (if applying), or the OWNER. already
    // available within the supplied table name
    procedure SqlSplitProcedureName(const aProcName: RawUtf8;
      out Owner, package, ProcName: RawUtf8); virtual;
    /// return the fully qualified SQL table name
    // - will use ForcedSchemaName property (if applying), or return aTableName
    // - you can override this method to force the expected format
    function SqlFullTableName(const aTableName: RawUtf8): RawUtf8; virtual;
    /// return a SQL table name with quotes if necessary
    // - can be used e.g. with SELECT statements
    // - you can override this method to force the expected format
    function SqlTableName(const aTableName: RawUtf8): RawUtf8; virtual;

    /// retrieve the column/field layout of a specified table
    // - this default implementation will use protected SqlGetField virtual
    // method to retrieve the field names and properties
    // - used e.g. by GetFieldDefinitions
    // - will call ColumnTypeNativeToDB protected virtual method to guess the
    // each mORMot TSqlDBFieldType
    procedure GetFields(const aTableName: RawUtf8;
      out Fields: TSqlDBColumnDefineDynArray); virtual;
    /// retrieve the advanced indexed information of a specified Table
    //  - this default implementation will use protected SqlGetIndex virtual
    // method to retrieve the index names and properties
    // - currently only MS SQL and Oracle are supported
    procedure GetIndexes(const aTableName: RawUtf8;
      out Indexes: TSqlDBIndexDefineDynArray); virtual;
    /// get all field/column definition for a specified Table as text
    // - call the GetFields method and retrieve the column field name and
    // type as 'Name [Type Length Precision Scale]'
    // - if WithForeignKeys is set, will add external foreign keys as '% tablename'
    procedure GetFieldDefinitions(const aTableName: RawUtf8;
      out Fields: TRawUtf8DynArray; WithForeignKeys: boolean);
    /// get one field/column definition as text
    // - return column type as 'Name [Type Length Precision Scale]'
    class function GetFieldDefinition(
      const Column: TSqlDBColumnDefine): RawUtf8;
    /// get one field/column definition as text, targeting a TOrm
    // published property
    // - return e.g. property type information as:
    // ! 'Name: RawUtf8 read fName write fName index 20;';
    class function GetFieldORMDefinition(const Column: TSqlDBColumnDefine): RawUtf8;
    /// check if the supplied text word is not a keyword for a given database engine
    class function IsSqlKeyword(aDB: TSqlDBDefinition; aWord: RawUtf8): boolean;
      overload; virtual;
    /// check if the supplied text word is not a keyword for the current database engine
    // - just a wrapper around the overloaded class function
    function IsSqlKeyword(aWord: RawUtf8): boolean; overload;
    /// retrieve a list of stored procedure names from current connection
    procedure GetProcedureNames(out Procedures: TRawUtf8DynArray); virtual;
    /// retrieve procedure input/output parameter information
    // - aProcName: stored procedure name to retrieve parameter infomation.
    // - Parameters: parameter list info (name, datatype, direction, default)
    procedure GetProcedureParameters(const aProcName: RawUtf8;
      out Parameters: TSqlDBProcColumnDefineDynArray); virtual;
    /// get all table names
    // - this default implementation will use protected SqlGetTableNames virtual
    // method to retrieve the table names
    procedure GetTableNames(out Tables: TRawUtf8DynArray); virtual;
    /// get all view names
    // - this default implementation will use protected SqlGetViewNames virtual
    // method to retrieve the view names
    procedure GetViewNames(out Views: TRawUtf8DynArray); virtual;
    /// retrieve a foreign key for a specified table and column
    // - first time it is called, it will retrieve all foreign keys from the
    // remote database using virtual protected GetForeignKeys method into
    // the protected fForeignKeys list: this may be slow, depending on the
    // database access (more than 10 seconds waiting is possible)
    // - any further call will use this internal list, so response will be
    // immediate
    // - the whole foreign key list is shared by all connections
    function GetForeignKey(const aTableName, aColumnName: RawUtf8): RawUtf8;

    /// returns the information to adapt the LIMIT # clause in the SQL SELECT
    // statement to a syntax matching the underlying DBMS
    // - e.g. TRestStorageExternal.AdaptSqlForEngineList() calls this
    // to let TRestServer.Uri by-pass virtual table mechanism
    function SqlLimitClause(
      AStmt: TSelectStatement): TSqlDBDefinitionLimitClause; virtual;
    /// determine if the SQL statement can be cached
    // - used by TSqlDBConnection.NewStatementPrepared() for handling cache
    function IsCachable(P: PUtf8Char): boolean; virtual;
    /// check if a primary key has already an index
    // - can specify if it is ascending only, which is not the case for Firebird
    function IsPrimaryKeyIndexed(var AscendingOnly: boolean): boolean; virtual;
    /// return the database engine name, as computed from the class name
    // - 'TSqlDBConnectionProperties' will be trimmed left side of the class name
    class function EngineName: RawUtf8;

    /// return a shared connection, corresponding to the given database
    // - call the ThreadSafeConnection method instead e.g. for multi-thread
    // access, or NewThreadSafeStatement for direct retrieval of a new statement
    property MainConnection: TSqlDBConnection
      read GetMainConnection;
    /// the associated User Password, as specified at creation
    // - not published, for security reasons (may be serialized otherwise)
    property PassWord: RawUtf8
      read fPassWord;
    /// the associated database name, as specified at creation
    // - not published, for security reasons (may be serialized otherwise)
    // - DatabaseNameSafe will be published, and delete any matching
    // PasswordValue in DatabaseName
    property DatabaseName: RawUtf8
      read fDatabaseName;
    /// can be used to store the fForeignKeys[] data in an external BLOB
    // - since GetForeignKeys can be (somewhat) slow, could save a lot of time
    property ForeignKeysData: RawByteString
      read GetForeignKeysData write SetForeignKeysData;
    /// this event handler will be called during all process
    // - can be used e.g. to change the desktop cursor, or be notified
    // on connection/disconnection/reconnection
    // - you can override this property directly in the TSqlDBConnection
    property OnProcess: TOnSqlDBProcess
      read fOnProcess write fOnProcess;
    /// this event handler will be called when statements trigger some low-level
    // information
    property OnStatementInfo: TOnSqlDBInfo
      read fOnStatementInfo write fOnStatementInfo;
    /// you can define a callback method able to handle multiple INSERT
    // - may execute e.g. INSERT with multiple VALUES (like MySQL, MSSQL, NexusDB,
    // PostgreSQL or SQlite3), as defined by MultipleValuesInsert() callback
    property OnBatchInsert: TOnBatchInsert
      read fOnBatchInsert write fOnBatchInsert;
  published { to be logged as JSON - no UserID nor Password for security :) }
    /// return the database engine name, as computed from the class name
    // - 'TSqlDBConnectionProperties' will be trimmed left side of the class name
    property Engine: RawUtf8
      read fEngineName;
    /// the associated server name, as specified at creation
    property ServerName: RawUtf8
      read fServerName;
    /// the associated database name, safely trimmed from the password
    // - would replace any matching Password value content from DatabaseName
    // by '***' for security reasons, e.g. before serialization
    property DatabaseNameSafe: RawUtf8
      read GetDatabaseNameSafe;
    /// the associated User Identifier, as specified at creation
    property UserID: RawUtf8
      read fUserID;
    /// the remote DBMS type, as stated by the inheriting class itself, or
    //  retrieved at connecton time (e.g. for ODBC)
    property Dbms: TSqlDBDefinition
      read GetDbms;
    /// the remote DBMS type name, retrieved as text from the DBMS property
    property DbmsEngineName: RawUtf8
      read GetDbmsName;
    /// the abilities of the database for batch sending
    // - e.g. Oracle will handle array DML binds, or MS SQL bulk insert
    property BatchSendingAbilities: TSqlDBStatementCRUDs
      read fBatchSendingAbilities;
    /// the maximum number of rows to be transmitted at once for batch sending
    // - e.g. Oracle handles array DML operation with iters <= 32767 at best
    // - if OnBatchInsert points to MultipleValuesInsert(), this value is
    // ignored, and the maximum number of parameters is guessed per DBMS type
    property BatchMaxSentAtOnce: integer
      read fBatchMaxSentAtOnce write fBatchMaxSentAtOnce;
    /// the maximum size, in bytes, of logged SQL statements
    // - setting 0 will log statement and parameters with no size limit
    // - setting -1 will log statement without any parameter value (just ?)
    // - setting any value >0 will log statement and parameters up to the
    // number of bytes (default set to 2048 to log up to 2KB per statement)
    property LoggedSqlMaxSize: integer
      read fLoggedSqlMaxSize write fLoggedSqlMaxSize;
    /// allow to log the SQL statement when any low-level ESqlDBException is raised
    property LogSqlStatementOnException: boolean read
      fLogSqlStatementOnException write fLogSqlStatementOnException;
    /// an optional Schema name to be used for SqlGetField() instead of UserID
    // - by default, UserID will be used as schema name, if none is specified
    // (i.e. if table name is not set as SCHEMA.TABLE)
    // - depending on the DBMS identified, the class may also set automatically
    // the default 'dbo' for MS SQL or 'public' for PostgreSQL
    // - you can set a custom schema to be used instead
    property ForcedSchemaName: RawUtf8
      read fForcedSchemaName write fForcedSchemaName;
    /// if GetTableNames/GetViewNames should only return the table names
    // starting with 'ForcedSchemaName.' prefix
    property FilterTableViewSchemaName: boolean
      read fFilterTableViewSchemaName write fFilterTableViewSchemaName;
    /// TRUE if an internal cache of SQL statement should be used
    // - cache will be accessed for NewStatementPrepared() method only, by
    // returning ISqlDBStatement interface instances
    // - default value is TRUE for faster process (e.g. TTestSQLite3ExternalDB
    // regression tests will be two times faster with statement caching)
    // - will cache only statements containing ? parameters or a SELECT with no
    // WHERE clause within
    property UseCache: boolean
      read fUseCache write fUseCache;
    /// maximum bytes allowed for FetchAllToJSON/FetchAllToBinary methods
    // - if a result set exceeds this limit, an ESQLDBException is raised
    // - default is 512 shl 20, i.e. 512MB which is very high
    // - avoid unexpected OutOfMemory errors when incorrect statement is run
    property StatementMaxMemory: Int64
      read fStatementMaxMemory write fStatementMaxMemory;
    /// if UseCache is true, how many statement replicates can be generated
    // if the cached ISqlDBStatement is already used
    // - such replication is normally not needed in a per-thread connection,
    // unless ISqlDBStatement are not released as soon as possible
    // - above this limit, no cache will be made, and a dedicated single-time
    // statement will be prepared
    // - default is 0 to cache statements once - but you may try to increase
    // this value if you run identical SQL with long-standing ISqlDBStatement;
    // or you can set -1 if you don't want the warning log to appear
    property StatementCacheReplicates: integer
      read fStatementCacheReplicates write fStatementCacheReplicates;
    /// defines if TSqlDBConnection.Disconnect shall Rollback any pending
    // transaction
    // - some engines executes a COMMIT when the client is disconnected, others
    // do raise an exception: this parameter ensures that any pending transaction
    // is roll-backed before disconnection
    // - is set to TRUE by default
    property RollbackOnDisconnect: boolean
      read fRollbackOnDisconnect write fRollbackOnDisconnect;
    /// defines if '' string values are to be stored as SQL null
    // - by default, '' will be stored as ''
    // - but some DB engines (e.g. Jet or MS SQL) does not allow by default to
    // store '' values, but expect NULL to be stored instead
    property StoreVoidStringAsNull: boolean
       read fStoreVoidStringAsNull write fStoreVoidStringAsNull;
    /// customize the ISO-8601 text format expected by the database provider
    // - is 'T' by default, as expected by the ISO-8601 standard
    // - will be changed e.g. for PostgreSQL, which expects ' ' instead
    // - as used by SqlDateToIso8601Quoted() and BindArray()
    property DateTimeFirstChar: AnsiChar
      read fDateTimeFirstChar write fDateTimeFirstChar;
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
    // the ISqlDBRows.Column[] property or ISqlDBRows.ColumnVariant() method;
    // this won't affect other Column*() methods, or JSON production
    property VariantStringAsWideString: boolean
      read fVariantWideString write fVariantWideString;
    {$endif UNICODE}
    /// SQL statements what will be executed for each new connection
    // usage scenarios examples:
    // - Oracle: force case-insensitive like
    // $  ['ALTER SESSION SET NLS_COMP=LINGUISTIC', 'ALTER SESSION SET NLS_SORT=BINARY_CI']
    //  - Postgres: disable notices and warnings
    // $  ['SET client_min_messages to ERROR']
    // - SQLite3: turn foreign keys ON
    // $  ['PRAGMA foreign_keys = ON']
    property ExecuteWhenConnected: TRawUtf8DynArray
      read fExecuteWhenConnected write fExecuteWhenConnected;
  end;

  /// abstract connection created from TSqlDBConnectionProperties
  // - more than one TSqlDBConnection instance can be run for the same
  // TSqlDBConnectionProperties
  TSqlDBConnection = class
  protected
    fProperties: TSqlDBConnectionProperties;
    fErrorException: ExceptClass;
    fErrorMessage: RawUtf8;
    fTransactionCount: integer;
    fServerTimestampOffset: TDateTime;
    fServerTimestampAtConnection: TDateTime;
    fCache: TRawUtf8List;
    fOnProcess: TOnSqlDBProcess;
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
    procedure InternalProcess(Event: TOnSqlDBProcessEvent);
  public
    /// connect to a specified database engine
    constructor Create(aProperties: TSqlDBConnectionProperties); virtual;
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
    function NewStatement: TSqlDBStatement; virtual; abstract;
    /// initialize a new SQL query statement for the given connection
    // - this default implementation will call the NewStatement method, and
    // implement handle statement caching is UseCache=true - in this case,
    // the TSqlDBStatement.Reset method shall have been overridden to allow
    // binding and execution of the very same prepared statement
    // - the same aSql can cache up to 9 statements in this TSqlDBConnection
    // - this method should return a prepared statement instance on success
    // - on error, if RaiseExceptionOnError=false (by default), it returns nil
    // and you can check LastErrorMessage and LastErrorException properties to
    // retrieve corresponding error information
    // - if TSqlDBConnectionProperties.ReconnectAfterConnectionError is set,
    // any connection error will be trapped, unless AllowReconnect is false
    // - on error, if RaiseExceptionOnError=true, an exception is raised
    function NewStatementPrepared(const aSql: RawUtf8; ExpectResults: boolean;
      RaiseExceptionOnError: boolean = false;
      AllowReconnect: boolean = true): ISqlDBStatement; virtual;
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
    // - properly overriden e.g. by TSqlDBOracleConnection
    function PasswordChange: boolean; virtual;

    /// direct export of a DB statement rows into a new table of this database
    // - the corresponding table will be created within the current connection,
    // if it does not exist
    // - if the column types are not set, they will be identified from the
    // first row of data
    // - INSERTs will be nested within a transaction if WithinTransaction is TRUE
    // - will raise an Exception in case of error
    function NewTableFromRows(const TableName: RawUtf8; Rows: TSqlDBStatement;
      WithinTransaction: boolean; ColumnForcedTypes: TSqlDBFieldTypeDynArray = nil): integer;

    /// the current Date and Time, as retrieved from the server
    // - note that this value is the DB_SERVERTIME[] constant SQL value, so
    // will most likely return a local time, not an UTC time
    // - this property will return the timestamp in TTimeLog / TTimeLogBits /
    // Int64 value
    property ServerTimestamp: TTimeLog
      read GetServerTimestamp;
    /// the current Date and Time, as retrieved from the server
    // - note that this value is the DB_SERVERTIME[] constant SQL value, so
    // will most likely return a local time, not an UTC time
    // - this property will return the value as regular TDateTime
    property ServerDateTime: TDateTime
      read GetServerDateTime;
    /// this event handler will be called during all process
    // - can be used e.g. to change the desktop cursor
    // - by default, will follow TSqlDBConnectionProperties.OnProcess property
    property OnProcess: TOnSqlDBProcess
      read fOnProcess write fOnProcess;
  published { to be logged as JSON }
    /// returns TRUE if the connection was set
    property Connected: boolean
      read IsConnected;
    /// the time returned by the server when the connection occurred
    property ServerTimestampAtConnection: TDateTime
      read fServerTimestampAtConnection;
    /// number of sucessfull connections for this instance
    // - can be greater than 1 in case of re-connection via Disconnect/Connect
    property TotalConnectionCount: integer
      read fTotalConnectionCount;
    /// number of nested StartTransaction calls
    // - equals 0 if no transaction is active
    property TransactionCount: integer
      read fTransactionCount;
    /// TRUE if StartTransaction has been called
    // - check if TransactionCount>0
    property InTransaction: boolean
      read GetInTransaction;
    /// defines if Disconnect shall Rollback any pending transaction
    // - some engines executes a COMMIT when the client is disconnected, others
    // do raise an exception: this parameter ensures that any pending transaction
    // is roll-backed before disconnection
    // - is set to TRUE by default
    property RollbackOnDisconnect: boolean
      read fRollbackOnDisconnect write fRollbackOnDisconnect;
    /// some error message, e.g. during execution of NewStatementPrepared
    property LastErrorMessage: RawUtf8
      read fErrorMessage write fErrorMessage;
    /// some error exception, e.g. during execution of NewStatementPrepared
    property LastErrorException: ExceptClass
      read fErrorException;
    /// TRUE if last error is a broken connection, e.g. during execution of
    // NewStatementPrepared
    // - i.e. LastErrorException/LastErrorMessage concerns the database connection
    // - will use TSqlDBConnectionProperties.ExceptionIsAboutConnection virtual method
    property LastErrorWasAboutConnection: boolean
      read GetLastErrorWasAboutConnection;
    /// the associated database properties
    property Properties: TSqlDBConnectionProperties
      read fProperties;
  end;

  /// generic abstract class to implement a prepared SQL query
  // - inherited classes should implement the DB-specific connection in its
  // overridden methods, especially Bind*(), Prepare(), ExecutePrepared, Step()
  // and Column*() methods
  TSqlDBStatement = class(TInterfacedObject, ISqlDBRows, ISqlDBStatement)
  protected
    fConnection: TSqlDBConnection;
    fSql: RawUtf8;
    fParamCount: integer;
    fColumnCount: integer;
    fTotalRowsRetrieved: integer;
    fCurrentRow: integer;
    fStripSemicolon: boolean;
    fExpectResults: boolean;
    fForceBlobAsNull: boolean;
    fForceDateWithMS: boolean;
    fDbms: TSqlDBDefinition;
    {$ifndef SYNDB_SILENCE}
    fSqlLogLevel: TSynLogInfo;
    fSqlLogLog: TSynLog;
    {$endif SYNDB_SILENCE}
    fSqlWithInlinedParams: RawUtf8;
    fSqlLogTimer: TPrecisionTimer;
    fCacheIndex: integer;
    fSqlPrepared: RawUtf8;
    function GetSqlCurrent: RawUtf8;
    function GetSqlWithInlinedParams: RawUtf8;
    procedure ComputeSqlWithInlinedParams;
    function GetForceBlobAsNull: boolean;
    procedure SetForceBlobAsNull(value: boolean);
    function GetForceDateWithMS: boolean;
    procedure SetForceDateWithMS(value: boolean);
    /// raise an exception if Col is out of range according to fColumnCount
    procedure CheckCol(Col: integer);
      {$ifdef HASINLINE}inline;{$endif}
    procedure CheckColInvalid(Col: integer);
    /// will set a Int64/Double/Currency/TDateTime/RawUtf8/TBlobData Dest variable
    // from a given column value
    // - internal conversion will use a temporary Variant and ColumnToVariant method
    // - expects Dest to be of the exact type (e.g. Int64, not integer)
    function ColumnToTypedValue(Col: integer; DestType: TSqlDBFieldType;
      var Dest): TSqlDBFieldType;
    /// append the inlined value of a given parameter, mainly for GetSqlWithInlinedParams
    // - optional MaxCharCount will truncate the text to a given number of chars
    procedure AddParamValueAsText(Param: integer; Dest: TTextWriter;
      MaxCharCount: integer); virtual;
    /// return a Column as a variant
    function GetColumnVariant(const ColName: RawUtf8): Variant;
    /// return the associated statement instance for a ISqlDBRows interface
    function Instance: TSqlDBStatement;
    /// wrappers to compute sllSQL/sllDB SQL context with a local timer
    function SqlLogBegin(level: TSynLogInfo): TSynLog;
    function SqlLogEnd(const Fmt: RawUtf8; const Args: array of const): Int64; overload;
    function SqlLogEnd(msg: PShortString = nil): Int64; overload;
  public
    /// create a statement instance
    constructor Create(aConnection: TSqlDBConnection); virtual;

    /// bind a NULL value to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - some providers (e.g. OleDB during MULTI INSERT statements) expect the
    // proper column type to be set in BoundType, even for NULL values
    procedure BindNull(Param: integer; IO: TSqlDBParamInOutType = paramIn;
      BoundType: TSqlDBFieldType = ftNull); virtual; abstract;
    /// bind an integer value to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure Bind(Param: integer; Value: Int64;
      IO: TSqlDBParamInOutType = paramIn); overload; virtual; abstract;
    /// bind a double value to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure Bind(Param: integer; Value: double;
      IO: TSqlDBParamInOutType = paramIn); overload; virtual; abstract;
    /// bind a TDateTime value to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindDateTime(Param: integer; Value: TDateTime;
      IO: TSqlDBParamInOutType = paramIn); overload; virtual; abstract;
    /// bind a currency value to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindCurrency(Param: integer; Value: currency;
      IO: TSqlDBParamInOutType = paramIn); overload; virtual; abstract;
    /// bind a UTF-8 encoded string to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindTextU(Param: integer; const Value: RawUtf8;
      IO: TSqlDBParamInOutType = paramIn); overload; virtual; abstract;
    /// bind a UTF-8 encoded buffer text (#0 ended) to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindTextP(Param: integer; Value: PUtf8Char;
      IO: TSqlDBParamInOutType = paramIn); overload; virtual; abstract;
    /// bind a UTF-8 encoded string to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindTextS(Param: integer; const Value: string;
      IO: TSqlDBParamInOutType = paramIn); overload; virtual; abstract;
    /// bind a UTF-8 encoded string to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindTextW(Param: integer; const Value: WideString;
      IO: TSqlDBParamInOutType = paramIn); overload; virtual; abstract;
    /// bind a Blob buffer to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindBlob(Param: integer; Data: pointer; Size: integer;
      IO: TSqlDBParamInOutType = paramIn); overload; virtual; abstract;
    /// bind a Blob buffer to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindBlob(Param: integer; const Data: RawByteString;
      IO: TSqlDBParamInOutType = paramIn); overload; virtual; abstract;
    /// bind a Variant value to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - will call all virtual Bind*() methods from the Data type
    // - if DataIsBlob is TRUE, will call BindBlob(RawByteString(Data)) instead
    // of BindTextW(WideString(Variant)) - used e.g. by TQuery.AsBlob/AsBytes
    procedure BindVariant(Param: integer; const Data: Variant;
      DataIsBlob: boolean; IO: TSqlDBParamInOutType = paramIn); virtual;
    /// bind one TSqlVar value
    // - the leftmost SQL parameter has an index of 1
    // - this default implementation will call corresponding Bind*() method
    procedure Bind(Param: integer; const Data: TSqlVar;
      IO: TSqlDBParamInOutType = paramIn); overload; virtual;
    /// bind one RawUtf8 encoded value
    // - the leftmost SQL parameter has an index of 1
    // - the value should match the BindArray() format, i.e. be stored as in SQL
    // (i.e. number, 'quoted string', 'YYYY-MM-DD hh:mm:ss', null) - e.g. as
    // computed by TJsonObjectDecoder.Decode()
    procedure Bind(Param: integer; ParamType: TSqlDBFieldType;
      const Value: RawUtf8; ValueAlreadyUnquoted: boolean;
      IO: TSqlDBParamInOutType = paramIn); overload; virtual;
    /// bind an array of const values
    // - parameters marked as ? should be specified as method parameter in Params[]
    // - BLOB parameters can be bound with this method, when set after encoding
    // via BinToBase64WithMagic() call
    // - TDateTime parameters can be bound with this method, when encoded via
    // a DateToSql() or DateTimeToSql() call
    // - any variant parameter will be bound with BindVariant(i,VVariant^,true,IO)
    // i.e. with DataIsBlob=true
    // - this default implementation will call corresponding Bind*() method
    procedure Bind(const Params: array of const;
      IO: TSqlDBParamInOutType = paramIn); overload; virtual;
    /// bind an array of fields from an existing SQL statement
    // - can be used e.g. after ColumnsToSqlInsert() method call for fast data
    // conversion between tables
    procedure BindFromRows(const Fields: TSqlDBFieldTypeDynArray; Rows: TSqlDBStatement);
    /// bind a special CURSOR parameter to be returned as a mormot.db.sql result set
    // - Cursors are not handled internally by mORMot, but some databases (e.g.
    // Oracle) usually use such structures to get data from strored procedures
    // - such parameters are mapped as ftUnknown
    // - use BoundCursor() method to retrieve the corresponding ISqlDBRows after
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
    function BoundCursor(Param: integer): ISqlDBRows; virtual;

    /// bind an array of values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - values are stored as in SQL (i.e. number, 'quoted string',
    // 'YYYY-MM-DD hh:mm:ss', null)
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    procedure BindArray(Param: integer; ParamType: TSqlDBFieldType;
      const Values: TRawUtf8DynArray; ValuesCount: integer); overload; virtual;
    /// bind an array of integer values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    procedure BindArray(Param: integer;
      const Values: array of Int64); overload; virtual;
    /// bind an array of double values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    procedure BindArray(Param: integer;
      const Values: array of double); overload; virtual;
    /// bind an array of TDateTime values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - values are stored as in SQL (i.e. 'YYYY-MM-DD hh:mm:ss')
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    procedure BindArrayDateTime(Param: integer;
      const Values: array of TDateTime); virtual;
    /// bind an array of currency values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    procedure BindArrayCurrency(Param: integer;
      const Values: array of currency); virtual;
    /// bind an array of RawUtf8 values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - values are stored as in SQL (i.e. 'quoted string')
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    procedure BindArray(Param: integer;
      const Values: array of RawUtf8); overload; virtual;

    /// Prepare an UTF-8 encoded SQL statement
    // - parameters marked as ? will be bound later, before ExecutePrepared call
    // - if ExpectResults is TRUE, then Step() and Column*() methods are available
    // to retrieve the data rows
    // - should raise an Exception on any error
    // - this default implementation will just store aSql content and the
    // ExpectResults parameter, and connect to the remote server is was not
    // already connected
    procedure Prepare(const aSql: RawUtf8; ExpectResults: boolean); overload; virtual;
    /// Execute a prepared SQL statement
    // - parameters marked as ? should have been already bound with Bind*() functions
    // - should raise an Exception on any error
    // - this void default implementation will call set fConnection.fLastAccess
    procedure ExecutePrepared; virtual;
    /// release cursor memory and resources once Step loop is finished
    // - this method call is optional, but is better be used if the ISqlDBRows
    // statement from taken from cache, and returned a lot of content which
    // may still be in client (and server) memory
    // - override to free cursor memory when ISqlDBStatement is back in cache
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
    procedure Execute(const aSql: RawUtf8; ExpectResults: boolean); overload;
    /// Prepare and Execute an UTF-8 encoded SQL statement
    // - parameters marked as ? should be specified as method parameter in Params[]
    // - BLOB parameters could not be bound with this method, but need an explicit
    // call to BindBlob() method
    // - if ExpectResults is TRUE, then Step() and Column*() methods are available
    // to retrieve the data rows
    // - should raise an Exception on any error
    // - this method will bind parameters, then call Excecute() virtual method
    procedure Execute(const aSql: RawUtf8; ExpectResults: boolean;
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
    procedure Execute(const SqlFormat: RawUtf8; ExpectResults: boolean;
      const Args, Params: array of const); overload;
    /// execute a prepared SQL statement and return all rows content as a JSON string
    // - JSON data is retrieved with UTF-8 encoding
    // - if Expanded is true, JSON data is an array of objects, for direct use
    // with any Ajax or .NET client:
    // & [ {"col1":val11,"col2":"val12"},{"col1":val21,... ]
    // - if Expanded is false, JSON data is serialized (used in TOrmTableJson)
    // & { "FieldCount":1,"Values":["col1","col2",val11,"val12",val21,..] }
    // - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"'
    // format and contains true BLOB data
    // - this virtual implementation calls ExecutePrepared then FetchAllAsJson()
    procedure ExecutePreparedAndFetchAllAsJson(Expanded: boolean;
      out Json: RawUtf8); virtual;
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
    function ParamToVariant(Param: integer; var Value: Variant;
      CheckIsOutParameter: boolean = true): TSqlDBFieldType; virtual;

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
    // - typical use may be (see also e.g. the mormot.orm.sql unit):
    // ! var Query: ISqlDBStatement;
    // ! begin
    // !   Query := Props.NewThreadSafeStatementPrepared(
    // !    'select AccountNumber from Sales.Customer where AccountNumber like ?',
    // !    ['AW000001%'], true);
    // !   if Query <> nil then
    // !   begin
    // !     assert(SameTextU(Query.ColumnName(0), 'AccountNumber'));
    // !     while Query.Step do //  loop through all matching data rows
    // !       assert(Copy(Query.ColumnUtf8(0), 1, 8) = 'AW000001');
    // !     Query.ReleaseRows;
    // !   end;
    // ! end;
    function Step(SeekFirst: boolean = false): boolean; virtual; abstract;
    /// the column/field count of the current Row
    function ColumnCount: integer;
    /// the Column name of the current Row
    // - Columns numeration (i.e. Col value) starts with 0
    // - it's up to the implementation to ensure than all column names are unique
    function ColumnName(Col: integer): RawUtf8; virtual; abstract;
    /// returns the Column index of a given Column name
    // - Columns numeration (i.e. Col value) starts with 0
    // - returns -1 if the Column name is not found (via case insensitive search)
    function ColumnIndex(const aColumnName: RawUtf8): integer; virtual; abstract;
    /// the Column type of the current Row
    // - FieldSize can be set to store the size in chars of a ftUtf8 column
    // (0 means BLOB kind of TEXT column)
    function ColumnType(Col: integer;
      FieldSize: PInteger = nil): TSqlDBFieldType; virtual; abstract;
    /// returns TRUE if the column contains NULL
    function ColumnNull(Col: integer): boolean; virtual; abstract;
    /// return a Column integer value of the current Row, first Col is 0
    function ColumnInt(Col: integer): Int64; overload; virtual; abstract;
    /// return a Column floating point value of the current Row, first Col is 0
    function ColumnDouble(Col: integer): double; overload; virtual; abstract;
    /// return a Column date and time value of the current Row, first Col is 0
    function ColumnDateTime(Col: integer): TDateTime; overload; virtual; abstract;
    /// return a column date and time value of the current Row, first Col is 0
    // - call ColumnDateTime or ColumnUtf8 to convert into TTimeLogBits/Int64 time
    // stamp from a TDateTime or text
    function ColumnTimestamp(Col: integer): TTimeLog; overload;
    /// return a Column currency value of the current Row, first Col is 0
    function ColumnCurrency(Col: integer): currency; overload; virtual; abstract;
    /// return a Column UTF-8 encoded text value of the current Row, first Col is 0
    function ColumnUtf8(Col: integer): RawUtf8; overload; virtual; abstract;
    /// return a Column text value as generic VCL string of the current Row, first Col is 0
    // - this default implementation will call ColumnUtf8
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
    // - a ftUtf8 TEXT content will be mapped into a generic WideString variant
    // for pre-Unicode version of Delphi, and a generic UnicodeString (=string)
    // since Delphi 2009: you may not loose any data during charset conversion
    // - a ftBlob BLOB content will be mapped into a TBlobData AnsiString variant
    function ColumnVariant(Col: integer): Variant; overload;
    /// return a Column as a variant, first Col is 0
    // - this default implementation will call Column*() method above
    // - a ftUtf8 TEXT content will be mapped into a generic WideString variant
    // for pre-Unicode version of Delphi, and a generic UnicodeString (=string)
    // since Delphi 2009: you may not loose any data during charset conversion
    // - a ftBlob BLOB content will be mapped into a TBlobData AnsiString variant
    function ColumnToVariant(Col: integer; var Value: Variant): TSqlDBFieldType; virtual;
    /// return a Column as a TSqlVar value, first Col is 0
    // - the specified Temp variable will be used for temporary storage of
    // ftUtf8/ftBlob values
    procedure ColumnToSqlVar(Col: integer; var Value: TSqlVar; var Temp:
      RawByteString); virtual;
    /// return a special CURSOR Column content as a mormot.db.sql result set
    // - Cursors are not handled internally by mORMot, but some databases (e.g.
    // Oracle) usually use such structures to get data from strored procedures
    // - such columns are mapped as ftNull internally - so this method is the only
    // one giving access to the data rows
    // - this default method will raise an exception about unexpected behavior
    function ColumnCursor(Col: integer): ISqlDBRows; overload; virtual;
    /// return a Column integer value of the current Row, from a supplied column name
    function ColumnInt(const ColName: RawUtf8): Int64; overload;
    /// return a Column floating point value of the current Row, from a supplied column name
    function ColumnDouble(const ColName: RawUtf8): double; overload;
    /// return a Column date and time value of the current Row, from a supplied column name
    function ColumnDateTime(const ColName: RawUtf8): TDateTime; overload;
    /// return a column date and time value of the current Row, from a supplied column name
    // - call ColumnDateTime or ColumnUtf8 to convert into TTimeLogBits/Int64 time
    // stamp from a TDateTime or text
    function ColumnTimestamp(const ColName: RawUtf8): TTimeLog; overload;
    /// return a Column currency value of the current Row, from a supplied column name
    function ColumnCurrency(const ColName: RawUtf8): currency; overload;
    /// return a Column UTF-8 encoded text value of the current Row, from a supplied column name
    function ColumnUtf8(const ColName: RawUtf8): RawUtf8; overload;
    /// return a Column text value as generic VCL string of the current Row, from a supplied column name
    function ColumnString(const ColName: RawUtf8): string; overload;
    /// return a Column as a blob value of the current Row, from a supplied column name
    function ColumnBlob(const ColName: RawUtf8): RawByteString; overload;
    /// return a Column as a blob value of the current Row, from a supplied column name
    function ColumnBlobBytes(const ColName: RawUtf8): TBytes; overload;
    /// read a blob Column into the Stream parameter
    procedure ColumnBlobToStream(const ColName: RawUtf8; Stream: TStream); overload;
    /// write a blob Column into the Stream parameter
    // - expected to be used with 'SELECT .. FOR UPDATE' locking statements
    procedure ColumnBlobFromStream(const ColName: RawUtf8; Stream: TStream); overload;
    /// return a Column as a variant, from a supplied column name
    function ColumnVariant(const ColName: RawUtf8): Variant; overload;
    /// create a TSqlDBRowVariantType able to access any field content via late binding
    // - i.e. you can use Data.Name to access the 'Name' column of the current row
    // - this Variant will point to the corresponding TSqlDBStatement instance,
    // so it's not necessary to retrieve its value for each row
    // - typical use is:
    // ! var Row: Variant;
    // ! (...)
    // !  with MyConnProps.Execute('select * from table where name=?',[aName]) do
    // !  begin
    // !    Row := RowDaa;
    // !    while Step do
    // !      writeln(Row.FirstName,Row.BirthDate);
    // !    ReleaseRows;
    // !  end;
    function RowData: Variant; virtual;
    /// create a TDocVariant custom variant containing all columns values
    // - will create a "fast" TDocVariant object instance with all fields
    procedure RowDocVariant(out aDocument: variant;
      aOptions: TDocVariantOptions = JSON_FAST); virtual;
    /// return a special CURSOR Column content as a mormot.db.sql result set
    // - Cursors are not handled internally by mORMot, but some databases (e.g.
    // Oracle) usually use such structures to get data from strored procedures
    // - such columns are mapped as ftNull internally - so this method is the only
    // one giving access to the data rows
    // - this default method will raise an exception about unexpected behavior
    function ColumnCursor(const ColName: RawUtf8): ISqlDBRows; overload;
    /// append all columns values of the current Row to a JSON stream
    // - will use WR.Expand to guess the expected output format
    // - this default implementation will call Column*() methods above, but you
    // should also implement a custom version with no temporary variable
    // - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"
    // format and contains true BLOB data (unless ForceBlobAsNull property was set)
    procedure ColumnsToJson(WR: TJsonWriter); virtual;
    /// compute the SQL INSERT statement corresponding to this columns row
    // - and populate the Fields[] array with columns information (type and name)
    // - if the current column value is NULL, will return ftNull: it is up to the
    // caller to set the proper field type
    // - the SQL statement is prepared with bound parameters, e.g.
    // $ insert into TableName (Col1,Col2) values (?,N)
    // - used e.g. to convert some data on the fly from one database to another,
    // via the TSqlDBConnection.NewTableFromRows method
    function ColumnsToSqlInsert(const TableName: RawUtf8;
      var Fields: TSqlDBColumnCreateDynArray): RawUtf8; virtual;
    // append all rows content as a JSON stream
    // - JSON data is added to the supplied TStream, with UTF-8 encoding
    // - if Expanded is true, JSON data is an array of objects, for direct use
    // with any Ajax or .NET client:
    // & [ {"col1":val11,"col2":"val12"},{"col1":val21,... ]
    // - if Expanded is false, JSON data is serialized (used in TOrmTableJson)
    // & { "FieldCount":1,"Values":["col1","col2",val11,"val12",val21,..] }
    // - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"'
    // format and contains true BLOB data
    // - similar to corresponding TSqlRequest.Execute method in the
    // mormot.db.raw.sqlite3 unit
    // - returns the number of row data returned (excluding field names)
    // - warning: TRestStorageExternal.EngineRetrieve in mormot.orm.sql unit
    // expects the Expanded=true format to return '[{...}]'#10
    function FetchAllToJson(Json: TStream; Expanded: boolean): PtrInt;
    // Append all rows content as a CSV stream
    // - CSV data is added to the supplied TStream, with UTF-8 encoding
    // - if Tab=TRUE, will use TAB instead of ',' between columns
    // - you can customize the ',' separator - use e.g. the global ListSeparator
    // variable (from SysUtils) to reflect the current system definition (some
    // country use ',' as decimal separator, for instance our "douce France")
    // - AddBOM will add a UTF-8 byte Order Mark at the beginning of the content
    // - BLOB fields will be appended as "blob" with no data
    // - returns the number of row data returned
    function FetchAllToCsvValues(Dest: TStream; Tab: boolean;
      CommaSep: AnsiChar = ','; AddBOM: boolean = true): PtrInt;
    // return all rows content as a JSON string
    // - JSON data is retrieved with UTF-8 encoding
    // - if Expanded is true, JSON data is an array of objects, for direct use
    // with any Ajax or .NET client:
    // & [ {"col1":val11,"col2":"val12"},{"col1":val21,... ]
    // - if Expanded is false, JSON data is serialized (used in TOrmTableJson)
    // & { "FieldCount":1,"Values":["col1","col2",val11,"val12",val21,..] }
    // - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"'
    // format and contains true BLOB data
    // - if ReturnedRowCount points to an integer variable, it will be filled with
    // the number of row data returned (excluding field names)
    // - similar to corresponding TSqlRequest.Execute method in the
    // mormot.db.raw.sqlite3 unit
    function FetchAllAsJson(Expanded: boolean; ReturnedRowCount: PPtrInt = nil): RawUtf8;
    /// append all rows content as binary stream
    // - will save the column types and name, then every data row in optimized
    // binary format (faster and smaller than JSON)
    // - you can specify a LIMIT for the data extent (default 0 meaning all data)
    // - generates the format expected by TSqlDBProxyStatement
    function FetchAllToBinary(Dest: TStream; MaxRowCount: cardinal = 0;
      DataRowPosition: PCardinalDynArray = nil): cardinal; virtual;
    /// append current row content as binary stream
    // - will save one data row in optimized binary format (if not in Null)
    // - virtual method called by FetchAllToBinary()
    // - follows the format expected by TSqlDBProxyStatement
    procedure ColumnsToBinary(W: TBufferWriter; Null: pointer;
      const ColTypes: TSqlDBFieldTypeDynArray); virtual;
    /// low-level access to the Timer used for last DB operation
    property SqlLogTimer: TPrecisionTimer
      read fSqlLogTimer;
    /// after a call to Prepare(), contains the query text to be passed to the DB
    // - depending on the DB, parameters placeholders are replaced by ?, :1, $1 etc
    // - this SQL is ready to be used in any DB tool, e.g. to check the real
    // execution plan/timing
    property SqlPrepared: RawUtf8
      read fSqlPrepared;
    /// the prepared SQL statement, in its current state
    // - if statement is prepared, then equals SqlPrepared, otherwise, contains
    // the raw SQL property content
    // - used internally by the implementation units, e.g. for errors logging
    property SqlCurrent: RawUtf8
      read GetSqlCurrent;
    /// low-level access to the statement cache index, after a call to Prepare()
    // - contains >= 0 if the database supports prepared statement cache
    //(Oracle, Postgres) and query plan is cached; contains -1 in other cases
    property CacheIndex: integer
      read fCacheIndex;
  published
    /// the prepared SQL statement, as supplied to Prepare() method
    property Sql: RawUtf8
      read fSql;
    /// the prepared SQL statement, with all '?' changed into the supplied
    // parameter values
    // - such statement query plan usually differ from a real execution plan
    // for prepared statements with parameters - see SqlPrepared property instead
    property SqlWithInlinedParams: RawUtf8
      read GetSqlWithInlinedParams;
    /// the current row after Execute/Step call, corresponding to Column*() methods
    // - contains 0 before initial Step call, or a number >=1 during data retrieval
    property CurrentRow: integer
      read fCurrentRow;
    /// the total number of data rows retrieved by this instance
    // - is not reset when there is no more row of available data (Step returns
    // false), or when Step() is called with SeekFirst=true
    property TotalRowsRetrieved: integer
      read fTotalRowsRetrieved;
    /// the associated database connection
    property Connection: TSqlDBConnection
      read fConnection;
    /// strip last semicolon in query
    // - expectation may vary, depending on the SQL statement and the engine
    // - default is true
    property StripSemicolon: boolean
      read fStripSemicolon write fStripSemicolon;
  end;


{ ************ Parent Classes for Thread-Safe and Parametrized Connections }

type
  /// abstract connection created from TSqlDBConnectionProperties
  // - this overridden class will defined an hidden thread ID, to ensure
  // that one connection will be create per thread
  // - e.g. OleDB, ODBC and Oracle connections will inherit from this class
  TSqlDBConnectionThreadSafe = class(TSqlDBConnection)
  protected
    fThreadID: TThreadID;
  end;

  /// threading modes set to TSqlDBConnectionPropertiesThreadSafe.ThreadingMode
  // - default mode is to use a Thread Pool, i.e. one connection per thread
  // - or you can force to use the main connection
  // - or you can use a shared background thread process (not implemented yet)
  // - last two modes could be used for embedded databases (SQLite3/FireBird),
  // when multiple connections may break stability, consume too much resources
  // and/or decrease performance
  TSqlDBConnectionPropertiesThreadSafeThreadingMode = (
    tmThreadPool,
    tmMainConnection,
    tmBackgroundThread);

  /// connection properties which will implement an internal Thread-Safe
  // connection pool
  TSqlDBConnectionPropertiesThreadSafe = class(TSqlDBConnectionProperties)
  protected
    fConnectionPool: TSynObjectListLocked;
    fLatestConnectionRetrievedInPool: PtrInt;
    fThreadingMode: TSqlDBConnectionPropertiesThreadSafeThreadingMode;
    /// returns -1 if none was defined yet
    function CurrentThreadConnectionIndex: PtrInt;
    /// overridden method to properly handle multi-thread
    function GetMainConnection: TSqlDBConnection; override;
  public
    /// initialize the properties
    // - this overridden method will initialize the internal per-thread connection pool
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUtf8); override;
    /// release related memory, and all per-thread connections
    destructor Destroy; override;
    /// get a thread-safe connection
    // - this overridden implementation will define a per-thread TSqlDBConnection
    // connection pool, via an internal  pool
    function ThreadSafeConnection: TSqlDBConnection; override;
    /// release all existing connections
    // - this overridden implementation will release all per-thread
    // TSqlDBConnection internal connection pool
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
    // - within the mORMot server, mormot.orm.sql unit will call this method
    // for every terminating thread created for TRestServerNamedPipeResponse
    // or TRestHttpServer multi-thread process
    procedure EndCurrentThread; virtual;
    /// set this property if you want to disable the per-thread connection pool
    // - to be used e.g. in database embedded mode (SQLite3/FireBird), when
    // multiple connections may break stability and decrease performance
    // - see TSqlDBConnectionPropertiesThreadSafeThreadingMode for the
    // possible values
    property ThreadingMode: TSqlDBConnectionPropertiesThreadSafeThreadingMode
      read fThreadingMode write fThreadingMode;
  end;

  /// a structure used to store a standard binding parameter
  // - you can use your own internal representation of parameters
  // (TOleDBStatement use its own TOleDBStatementParam type), but
  // this type can be used to implement a generic parameter
  // - used e.g. by TSqlDBStatementWithParams as a dynamic array
  // (and its inherited TSqlDBOracleStatement)
  // - don't change this structure, since it will be serialized as binary
  // for TSqlDBProxyConnectionCommandExecute
  TSqlDBParam = packed record
    /// storage used for TEXT (ftUtf8) and BLOB (ftBlob) values
    // - ftBlob are stored as RawByteString
    // - ftUtf8 are stored as RawUtf8
    // - sometimes, may be ftInt64 or ftCurrency provided as SQLT_AVC text,
    // or ftDate value converted to SQLT_TIMESTAMP
    VData: RawByteString;
    /// storage used for bound array values
    // - number of items in array is stored in VInt64
    // - values are stored as in SQL (i.e. number, 'quoted string',
    // 'YYYY-MM-DD hh:mm:ss', null)
    VArray: TRawUtf8DynArray;
    /// storage used for ftInt64, ftDouble, ftDate and ftCurrency value
    VInt64: Int64;
    /// the column/parameter Value type
    VType: TSqlDBFieldType;
    /// define if parameter can be retrieved after a stored procedure execution
    VInOut: TSqlDBParamInOutType;
    /// used e.g. by TSqlDBOracleStatement
    VDBType: word;
  end;

  PSqlDBParam = ^TSqlDBParam;

  /// dynamic array used to store standard binding parameters
  // - used e.g. by TSqlDBStatementWithParams (and its
  // inherited TSqlDBOracleStatement)
  TSqlDBParamDynArray = array of TSqlDBParam;

  /// generic abstract class handling prepared statements with binding
  // - will provide protected fields and methods for handling standard
  // TSqlDBParam parameters
  TSqlDBStatementWithParams = class(TSqlDBStatement)
  protected
    fParams: TSqlDBParamDynArray;
    fParam: TDynArray;
    fParamsArrayCount: integer;
    function CheckParam(Param: integer; NewType: TSqlDBFieldType;
      IO: TSqlDBParamInOutType): PSqlDBParam; overload;
    function CheckParam(Param: integer; NewType: TSqlDBFieldType;
      IO: TSqlDBParamInOutType; ArrayCount: integer): PSqlDBParam; overload;
    /// append the inlined value of a given parameter
    // - faster overridden method
    procedure AddParamValueAsText(Param: integer; Dest: TTextWriter;
      MaxCharCount: integer); override;
  public
    /// create a statement instance
    // - this overridden version will initialize the internal fParam* fields
    constructor Create(aConnection: TSqlDBConnection); override;
    /// bind a NULL value to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an Exception on any error
    // - some providers (only OleDB during MULTI INSERT statements, so never used
    // in this class) expect the  proper column type to be set in BoundType
    procedure BindNull(Param: integer; IO: TSqlDBParamInOutType = paramIn;
      BoundType: TSqlDBFieldType = ftNull); override;
    /// bind an integer value to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an Exception on any error
    procedure Bind(Param: integer; Value: Int64;
      IO: TSqlDBParamInOutType = paramIn); overload; override;
    /// bind a double value to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an Exception on any error
    procedure Bind(Param: integer; Value: double;
      IO: TSqlDBParamInOutType = paramIn); overload; override;
    /// bind a TDateTime value to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an Exception on any error
    procedure BindDateTime(Param: integer; Value: TDateTime;
      IO: TSqlDBParamInOutType = paramIn); overload; override;
    /// bind a currency value to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an Exception on any error
    procedure BindCurrency(Param: integer; Value: currency;
      IO: TSqlDBParamInOutType = paramIn); overload; override;
    /// bind a UTF-8 encoded string to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an Exception on any error
    procedure BindTextU(Param: integer; const Value: RawUtf8;
      IO: TSqlDBParamInOutType = paramIn); overload; override;
    /// bind a UTF-8 encoded buffer text (#0 ended) to a parameter
    // - the leftmost SQL parameter has an index of 1
    procedure BindTextP(Param: integer; Value: PUtf8Char;
      IO: TSqlDBParamInOutType = paramIn); overload; override;
    /// bind a VCL string to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an Exception on any error
    procedure BindTextS(Param: integer; const Value: string;
      IO: TSqlDBParamInOutType = paramIn); overload; override;
    /// bind an OLE WideString to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an Exception on any error }
    procedure BindTextW(Param: integer; const Value: WideString;
      IO: TSqlDBParamInOutType = paramIn); overload; override;
    /// bind a Blob buffer to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an Exception on any error
    procedure BindBlob(Param: integer; Data: pointer; Size: integer;
      IO: TSqlDBParamInOutType = paramIn); overload; override;
    /// bind a Blob buffer to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - raise an Exception on any error M
    procedure BindBlob(Param: integer; const Data: RawByteString;
      IO: TSqlDBParamInOutType = paramIn); overload; override;

    /// bind an array of values to a parameter using OCI bind array feature
    // - the leftmost SQL parameter has an index of 1
    // - values are stored as in SQL (i.e. number, 'quoted string',
    // 'YYYY-MM-DD hh:mm:ss', null)
    // - values are stored as in SQL (i.e. 'YYYY-MM-DD hh:mm:ss')
    procedure BindArray(Param: integer; ParamType: TSqlDBFieldType;
      const Values: TRawUtf8DynArray; ValuesCount: integer); overload; override;
    /// bind an array of integer values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - this default implementation will call BindArray() after conversion into
    // RawUtf8 items, stored in TSqlDBParam.VArray
    procedure BindArray(Param: integer;
      const Values: array of Int64); overload; override;
    /// bind an array of double values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    // - this default implementation will call BindArray() after conversion into
    // RawUtf8 items, stored in TSqlDBParam.VArray
    procedure BindArray(Param: integer;
      const Values: array of double); overload; override;
    /// bind an array of TDateTime values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - values are stored as in SQL (i.e. 'YYYY-MM-DD hh:mm:ss')
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    // - this default implementation will call BindArray() after conversion into
    // RawUtf8 items, stored in TSqlDBParam.VArray
    procedure BindArrayDateTime(Param: integer;
      const Values: array of TDateTime); override;
    /// bind an array of currency values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    // - this default implementation will call BindArray() after conversion into
    // RawUtf8 items, stored in TSqlDBParam.VArray
    procedure BindArrayCurrency(Param: integer;
      const Values: array of currency); override;
    /// bind an array of RawUtf8 values to a parameter
    // - the leftmost SQL parameter has an index of 1
    // - values are stored as 'quoted string'
    // - this default implementation will raise an exception if the engine
    // does not support array binding
    procedure BindArray(Param: integer;
      const Values: array of RawUtf8); overload; override;

    /// start parameter array binding per-row process
    // - BindArray*() methods expect the data to be supplied "verticaly": this
    // method allow-per row binding
    // - call this method, then BindArrayRow() with the corresponding values for
    // one statement row, then Execute to send the query
    procedure BindArrayRowPrepare(const aParamTypes: array of TSqlDBFieldType;
      aExpectedMinimalRowCount: integer = 0);
    /// bind a set of parameters for further array binding
    // - supplied parameters shall follow the BindArrayRowPrepare() supplied
    // types (i.e. RawUtf8, integer/Int64, double);  you can also bind directly
    // a TDateTime value if the corresponding binding has been defined as ftDate
    // by BindArrayRowPrepare()
    procedure BindArrayRow(const aValues: array of const);
    /// bind an array of fields from an existing SQL statement for array binding
    // - supplied Rows columns shall follow the BindArrayRowPrepare() supplied
    // types (i.e. RawUtf8, integer/Int64, double, date)
    // - can be used e.g. after ColumnsToSqlInsert() method call for fast data
    // conversion between tables
    procedure BindFromRows(Rows: TSqlDBStatement); virtual;

    /// retrieve the parameter content, after SQL execution
    // - the leftmost SQL parameter has an index of 1
    // - to be used e.g. with stored procedures
    // - this overridden function will retrieve the value stored in the protected
    // fParams[] array: the ExecutePrepared method should have updated its
    // content as exepcted
    function ParamToVariant(Param: integer; var Value: Variant;
      CheckIsOutParameter: boolean = true): TSqlDBFieldType; override;

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
  // - will provide protected fields and methods for handling both TSqlDBParam
  // parameters and standard TSqlDBColumnProperty column description
  TSqlDBStatementWithParamsAndColumns = class(TSqlDBStatementWithParams)
  protected
    fColumns: TSqlDBColumnPropertyDynArray;
    fColumn: TDynArrayHashed;
  public
    /// create a statement instance
    // - this overridden version will initialize the internal fColumn* fields
    constructor Create(aConnection: TSqlDBConnection); override;
    /// retrieve a column name of the current Row
    // - Columns numeration (i.e. Col value) starts with 0
    // - it's up to the implementation to ensure than all column names are unique
    function ColumnName(Col: integer): RawUtf8; override;
    /// returns the Column index of a given Column name
    // - Columns numeration (i.e. Col value) starts with 0
    // - returns -1 if the Column name is not found (via case insensitive search)
    function ColumnIndex(const aColumnName: RawUtf8): integer; override;
    /// the Column type of the current Row
    // - ftCurrency type should be handled specifically, for faster process and
    // avoid any rounding issue, since currency is a standard OleDB type
    // - FieldSize can be set to store the size in chars of a ftUtf8 column
    // (0 means BLOB kind of TEXT column) - this implementation will store
    // fColumns[Col].ColumnValueDBSize if ColumnValueInlined=true
    function ColumnType(Col: integer;
      FieldSize: PInteger = nil): TSqlDBFieldType; override;
    /// direct access to the columns description
    // - gives more details than the default ColumnType() function
    property Columns: TSqlDBColumnPropertyDynArray
      read fColumns;
  end;



implementation


{ ************ General SQL Processing Functions }

procedure LogTruncatedColumn(const Col: TSqlDBColumnProperty);
begin
  SynDBLog.Add.Log(sllDB, 'Truncated column %', Col.ColumnName);
end;

function TrimLeftSchema(const TableName: RawUtf8): RawUtf8;
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

function ReplaceParamsByNames(const aSql: RawUtf8; var aNewSql: RawUtf8;
  aStripSemicolon: boolean): integer;
var
  i, j, B, L: PtrInt;
  P: PAnsiChar;
  c: array[0..3] of AnsiChar;
  tmp: RawUtf8;
const
  SQL_KEYWORDS: array[0..19] of AnsiChar = 'ASATBYIFINISOFONORTO';
begin
  result := 0;
  L := Length(aSql);
  if aStripSemicolon then
    while (L > 0) and
          (aSql[L] in [#1..' ', ';']) do
      if (aSql[L] = ';') and
         (L > 5) and
         IdemPChar(@aSql[L - 3], 'END') then
        break
      else // allows 'END;' at the end of a statement
        dec(L);    // trim ' ' or ';' right (last ';' could be found incorrect)
  if PosExChar('?', aSql) > 0 then
  begin
    aNewSql := '';
    // change ? into :AA :BA ..
    c := ':AA';
    i := 0;
    P := pointer(aSql);
    if P <> nil then
      repeat
        B := i;
        while (i < L) and
              (P[i] <> '?') do
        begin
          if P[i] = '''' then
          begin
            repeat // ignore chars inside ' quotes
              inc(i);
            until (i = L) or
                  ((P[i] = '''') and
                   (P[i + 1] <> ''''));
            if i = L then
              break;
          end;
          inc(i);
        end;
        FastSetString(tmp, P + B, i - B);
        aNewSql := aNewSql + tmp;
        if i = L then
          break;
        // store :AA :BA ..
        j := length(aNewSql);
        SetLength(aNewSql, j + 3);
        PCardinal(PtrInt(aNewSql) + j)^ := PCardinal(@c)^;
        repeat
          if c[1] = 'Z' then
          begin
            if c[2] = 'Z' then
              raise ESqlDBException.Create(
                'Only 656 parameters in :AA to :ZZ range');
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
    aNewSql := copy(aSql, 1, L); // trim right ';' if any
end;

function ReplaceParamsByNumbers(const aSql: RawUtf8; var aNewSql: RawUtf8;
  IndexChar: AnsiChar; AllowSemicolon: boolean): integer;
var
  ndx, L: PtrInt;
  s, d: PUtf8Char;
  c: AnsiChar;
begin
  aNewSql := aSql;
  result := 0;
  ndx := 0;
  L := Length(aSql);
  s := pointer(aSql);
  if (s = nil) or
     (PosExChar('?', aSql) = 0) then
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
    end
    else if (c = ';') and
                not AllowSemicolon then
      exit; // complex expression can not be prepared
    inc(s);
  end;
  if ndx = 0 then // no ? parameter
    exit;
  result := ndx;
  // parse SQL and replace ? into $n $nn $nnn
  FastSetString(aNewSql, nil, L);
  s := pointer(aSql);
  d := pointer(aNewSql);
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
  //assert(d - pointer(aNewSql) = length(aNewSql)); // until stabilized
end;

function BoundArrayToJsonArray(const Values: TRawUtf8DynArray): RawUtf8;
//  'one', 't"wo' -> '{"one","t\"wo"}'  and  1,2,3 -> '{1,2,3}'
var
  V: ^RawUtf8;
  s, d: PUtf8Char;
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
      begin
        // quoted ftUtf8
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
            else if (c = '"') or
                    (c = '\') then
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
      if s^ = '''' then // quoted ftUtf8
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
            else if (c = '"') or
                    (c = '\') then
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

function ToText(Dbms: TSqlDBDefinition): PShortString;
begin
  result := GetEnumName(TypeInfo(TSqlDBDefinition), ord(Dbms));
end;


{ ************ Abstract SQL DB Classes and Interfaces }

{ ESqlDBException }

constructor ESqlDBException.CreateUtf8(const Format: RawUtf8;
  const Args: array of const);
var
  msg {$ifndef SYNDB_SILENCE}, sql{$endif}: RawUtf8;
begin
  msg := FormatUtf8(Format, Args);
  {$ifndef SYNDB_SILENCE}
  if (length(Args) > 0) and
     (Args[0].VType = vtObject) and
     (Args[0].VObject <> nil) then
    if Args[0].VObject.InheritsFrom(TSqlDBStatement) then
    begin
      fStatement := TSqlDBStatement(Args[0].VObject);
      if fStatement.Connection.Properties.LogSqlStatementOnException then
      begin
        try
          sql := fStatement.GetSqlWithInlinedParams;
        except
          sql := fStatement.SQL; // if parameter access failed -> append with ?
        end;
        msg := msg + ' - ' + sql;
      end;
    end;
  {$endif SYNDB_SILENCE}
  inherited Create(Utf8ToString(msg));
end;


{ TSqlDBRowVariantType }

function TSqlDBRowVariantType.IntGet(var Dest: TVarData;
  const Instance: TVarData; Name: PAnsiChar; NameLen: PtrInt;
  NoException: boolean): boolean;
var
  smt: TSqlDBStatement;
  col: RawUtf8;
  ndx: integer;
begin
  smt := TSqlDBStatement(Instance.VPointer);
  if smt = nil then
    raise ESqlDBException.CreateUtf8('Invalid % call', [self]);
  FastSetString(col, Name, NameLen);
  ndx := smt.ColumnIndex(col);
  result := ndx >= 0;
  if ndx >= 0 then
    smt.ColumnToVariant(ndx, Variant(Dest));
end;


{ TSqlDBConnectionProperties }

constructor TSqlDBConnectionProperties.Create(const aServerName, aDatabaseName,
  aUserID, aPassWord: RawUtf8);
var
  db: TSqlDBDefinition;
begin
  fServerName := aServerName;
  fDatabaseName := aDatabaseName;
  fUserID := aUserID;
  fPassWord := aPassWord;
  fEngineName := EngineName;
  fRollbackOnDisconnect := true; // enabled by default
  fUseCache := true;
  fLoggedSqlMaxSize := 2048; // log up to 2KB of inlined SQL by default
  fStatementMaxMemory := 512 shl 20; // fetch to JSON/Binary up to 512MB
  SetInternalProperties; // virtual method used to override default parameters
  db := GetDbms;
  if db in [dSQLite, dDB2, dPostgreSQL] then // for SqlDateToIso8601Quoted()
    fDateTimeFirstChar := ' '
  else
    fDateTimeFirstChar := 'T';
  if fForcedSchemaName = '' then
    case db of // should make every one life's easier
      dMSSQL:
        fForcedSchemaName := 'dbo';
      dPostgreSql:
        fForcedSchemaName := 'public';
    end;
  if fSqlCreateField[ftUnknown] = '' then
    fSqlCreateField := DB_FIELDS[db];
  if fSqlCreateFieldMax = 0 then
    fSqlCreateFieldMax := DB_FIELDSMAX[db];
  if fSqlGetServerTimestamp = '' then
    fSqlGetServerTimestamp := DB_SERVERTIME[db];
  case db of
    dMSSQL,
    dJet:
      fStoreVoidStringAsNull := true;
  end;
  if byte(fBatchSendingAbilities) = 0 then // if not already handled by driver
    case db of
      dSQlite,
      dMySQL,
      dPostgreSQL,
      dNexusDB,
      dMSSQL,
      dDB2, // INSERT with multi VALUES
      //dFirebird,  EXECUTE BLOCK with params is slower (at least for embedded)
      dOracle:
        begin
          // Oracle expects weird INSERT ALL INTO ... statement
          fBatchSendingAbilities := [cCreate];
          fOnBatchInsert := MultipleValuesInsert;
          fBatchMaxSentAtOnce := 4096; // MultipleValuesInsert will do chunking
        end;
      dFirebird:
        begin
          // will run EXECUTE BLOCK without parameters
          fBatchSendingAbilities := [cCreate];
          fOnBatchInsert := MultipleValuesInsertFirebird;
          fBatchMaxSentAtOnce := 4096; // MultipleValuesInsert will do chunking
        end;
    end;
end;

destructor TSqlDBConnectionProperties.Destroy;
begin
  fMainConnection.Free;
  inherited;
end;

function TSqlDBConnectionProperties.Execute(const aSql: RawUtf8;
  const Params: array of const; RowsVariant: PVariant; ForceBlobAsNull: boolean): ISqlDBRows;
var
  Stmt: ISqlDBStatement;
begin
  Stmt := NewThreadSafeStatementPrepared(aSql, {expectres=}true, true);
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

function TSqlDBConnectionProperties.ExecuteNoResult(const aSql: RawUtf8;
  const Params: array of const): integer;
var
  Stmt: ISqlDBStatement;
begin
  Stmt := NewThreadSafeStatementPrepared(aSql, {expectres=}false, true);
  Stmt.Bind(Params);
  Stmt.ExecutePrepared;
  try
    result := Stmt.UpdateCount;
  except // may occur e.g. for Firebird's CREATE DATABASE
    result := 0;
  end;
end;

function TSqlDBConnectionProperties.PrepareInlined(const aSql: RawUtf8;
  ExpectResults: boolean): ISqlDBStatement;
var
  Query: ISqlDBStatement;
  i: PtrInt;
  decoder: TExtractInlineParameters;
begin
  result := nil; // returns nil interface on error
  if self = nil then
    exit;
  // convert inlined :(1234): parameters into Values[] for Bind*() calls
  decoder.Parse(aSql);
  Query := NewThreadSafeStatementPrepared(decoder.GenericSql, ExpectResults, true);
  if Query = nil then
    exit;
  for i := 0 to decoder.Count - 1 do
    case decoder.Types[i] of
      sptNull:
        Query.BindNull(i + 1);
      sptInteger:
        Query.Bind(i + 1, GetInt64(pointer(decoder.Values[i])));
      sptFloat:
        Query.Bind(i + 1, GetExtended(pointer(decoder.Values[i])));
      sptText:
        Query.BindTextU(i + 1, decoder.Values[i]);
      sptBlob:
        if decoder.Values[i] = '' then
          Query.BindNull(i + 1)
        else
          Query.BindBlob(i + 1, pointer(decoder.Values[i]), length(decoder.Values[i]));
      sptDateTime:
        Query.BindDateTime(i + 1, Iso8601ToDateTime(decoder.Values[i]));
    end;
  result := Query;
end;

function TSqlDBConnectionProperties.PrepareInlined(const SqlFormat: RawUtf8;
  const Args: array of const; ExpectResults: boolean): ISqlDBStatement;
begin
  result := PrepareInlined(FormatUtf8(SqlFormat, Args), ExpectResults);
end;

function TSqlDBConnectionProperties.ExecuteInlined(const aSql: RawUtf8;
  ExpectResults: boolean): ISqlDBRows;
var
  Query: ISqlDBStatement;
begin
  result := nil; // returns nil interface on error
  if self = nil then
    exit;
  Query := PrepareInlined(aSql, ExpectResults);
  if Query = nil then
    exit; // e.g. invalid aSql
  Query.ExecutePrepared;
  result := Query;
end;

function TSqlDBConnectionProperties.ExecuteInlined(const SqlFormat: RawUtf8;
  const Args: array of const; ExpectResults: boolean): ISqlDBRows;
begin
  result := ExecuteInlined(FormatUtf8(SqlFormat, Args), ExpectResults);
end;

procedure TSqlDBConnectionProperties.SetConnectionTimeOutMinutes(minutes: cardinal);
begin
  fConnectionTimeOutTicks := minutes * 60000; // minutes to ms conversion
end;

function TSqlDBConnectionProperties.GetConnectionTimeOutMinutes: cardinal;
begin
  result := fConnectionTimeOutTicks div 60000;
end;

function TSqlDBConnectionProperties.GetMainConnection: TSqlDBConnection;
begin
  if fMainConnection.IsOutdated(GetTickCount64) then
    FreeAndNilSafe(fMainConnection);
  if fMainConnection = nil then
    fMainConnection := NewConnection;
  result := fMainConnection;
end;

function TSqlDBConnectionProperties.{%H-}NewConnection: TSqlDBConnection;
begin
  raise ESqlDBException.CreateUtf8('%.NewConnection', [self]);
end;

function TSqlDBConnectionProperties.ThreadSafeConnection: TSqlDBConnection;
begin
  result := MainConnection; // provider should be thread-safe
end;

procedure TSqlDBConnectionProperties.ClearConnectionPool;
begin
  FreeAndNilSafe(fMainConnection);
end;

function TSqlDBConnectionProperties.NewThreadSafeStatement: TSqlDBStatement;
begin
  result := ThreadSafeConnection.NewStatement;
end;

function TSqlDBConnectionProperties.NewThreadSafeStatementPrepared(const aSql:
  RawUtf8; ExpectResults, RaiseExceptionOnError: boolean): ISqlDBStatement;
begin
  result := ThreadSafeConnection.NewStatementPrepared(aSql, ExpectResults,
    RaiseExceptionOnError);
end;

function TSqlDBConnectionProperties.NewThreadSafeStatementPrepared(const
  SqlFormat: RawUtf8; const Args: array of const; ExpectResults,
  RaiseExceptionOnError: boolean): ISqlDBStatement;
begin
  result := NewThreadSafeStatementPrepared(FormatUtf8(SqlFormat, Args),
    ExpectResults, RaiseExceptionOnError);
end;

function TSqlDBConnectionProperties.SharedTransaction(SessionID: cardinal;
  action: TSqlDBSharedTransactionAction): TSqlDBConnection;

  procedure SetResultToSameConnection(index: PtrInt);
  begin
    result := ThreadSafeConnection;
    if result <> fSharedTransactions[index].Connection then
      raise ESqlDBException.CreateUtf8('%.SharedTransaction(sessionID=%) with mixed thread connections: % and %',
        [self, SessionID, result, fSharedTransactions[index].Connection]);
  end;

var
  i, n: PtrInt;
begin
  n := Length(fSharedTransactions);
  try
    for i := 0 to n - 1 do
      if fSharedTransactions[i].SessionID = SessionID then
      begin
        SetResultToSameConnection(i);
        case action of
          transBegin: // nested StartTransaction
            LockedInc32(@fSharedTransactions[i].RefCount);
        else
          begin  // (nested) commit/rollback
            if InterlockedDecrement(fSharedTransactions[i].RefCount) = 0 then
            begin
              dec(n);
              MoveFast(fSharedTransactions[i + 1], fSharedTransactions[i],
                (n - i) * sizeof(fSharedTransactions[0]));
              SetLength(fSharedTransactions, n);
              case action of
                transCommitWithException,
                transCommitWithoutException:
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
              raise ESqlDBException.CreateUtf8(
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
      raise ESqlDBException.CreateUtf8('Unexpected %.SharedTransaction(%,%)',
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

procedure TSqlDBConnectionProperties.SetInternalProperties;
begin
  // nothing to do yet
end;

procedure TSqlDBConnectionProperties.SetSchemaNameToOwner(out Owner: RawUtf8);
begin
  if fForcedSchemaName = '' then
    case fDbms of
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

function TSqlDBConnectionProperties.IsCachable(P: PUtf8Char): boolean;
var
  NoWhere: boolean;
begin
  // cachable if with ? parameter or SELECT without WHERE clause
  if (P <> nil) and
     fUseCache then
  begin
    while P^ in [#1..' '] do
      inc(P);
    NoWhere := IdemPChar(P, 'SELECT ');
    if NoWhere or
       not (IdemPChar(P, 'CREATE ') or
            IdemPChar(P, 'ALTER ')) then
    begin
      result := true;
      while P^ <> #0 do
      begin
        if P^ = '"' then
        begin
          // ignore chars within quotes
          repeat
            inc(P)
          until P^ in [#0, '"'];
          if P^ = #0 then
            break;
        end
        else if P^ = '?' then
          exit
        else if (P^ = ' ') and
                IdemPChar(P + 1, 'WHERE ') then
          NoWhere := false;
        inc(P);
      end;
    end;
    result := NoWhere;
  end
  else
    result := false;
end;

function TSqlDBConnectionProperties.IsPrimaryKeyIndexed(
  var AscendingOnly: boolean): boolean;
begin
  // our ORM expects an index to quickly run "select max(RowID)"
  result := false;
  case GetDbms of
    dSQLite,
    dPostgreSQL,
    dMSSQL,
    dMySQL,
    dOracle,
    dNexusDB:
      // most DB create an implicit index on their primary key,
      // which is fine for max(RowID)
      result := true;
    dFirebird:
      // Firebird only creates an ASC index by default on its primary key
      // so max(RowID) is slow - see http://www.firebirdfaq.org/faq205
      // -> need to create a separated DESC index
      AscendingOnly := true;
      // note: TSqlDBIbxConnectionProperties overrides this method
      // and support a CreateDescendingPK property to create a DESC index only
  end;
end;

class function TSqlDBConnectionProperties.GetFieldDefinition(
  const Column: TSqlDBColumnDefine): RawUtf8;
begin
  with Column do
  begin
    FormatUtf8('% [%', [ColumnName, ColumnTypeNative], result);
    if (ColumnLength <> 0) or
       (Column.ColumnPrecision <> 0) or
       (Column.ColumnScale <> 0) then
      result := FormatUtf8('% % % %]',
        [result, ColumnLength, ColumnPrecision, ColumnScale])
    else
      result := result + ']';
    if ColumnIndexed then
      result := result + ' *';
  end;
end;

class function TSqlDBConnectionProperties.GetFieldORMDefinition(const Column:
  TSqlDBColumnDefine): RawUtf8;
begin
  // 'Name: RawUtf8 index 20 read fName write fName;';
  with Column do
  begin
    FormatUtf8('property %: %',
      [ColumnName, SqlDBFIELDTYPE_TO_DELPHITYPE[ColumnType]], result);
    if (ColumnType = ftUtf8) and
       (ColumnLength > 0) then
      result := FormatUtf8('% index %', [result, ColumnLength]);
    result := FormatUtf8('% read f% write f%;', [result, ColumnName, ColumnName]);
  end;
end;

var
  DB_KEYWORDS: array[TSqlDBDefinition] of TRawUtf8DynArray;

const
  /// CSV of the known reserved keywords per database engine, in alphabetic order
  DB_KEYWORDS_CSV: array[TSqlDBDefinition] of PUtf8Char = (
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

class function TSqlDBConnectionProperties.IsSqlKeyword(aDB: TSqlDBDefinition;
  aWord: RawUtf8): boolean;
var
  db: TSqlDBDefinition;
begin
  // prepare the keywords arrays from the per-DB CSV references
  if DB_KEYWORDS[dDefault] = nil then
    for db := low(DB_KEYWORDS) to high(DB_KEYWORDS) do
      CsvToRawUtf8DynArray(DB_KEYWORDS_CSV[db], DB_KEYWORDS[db]);
  // search using fast binary lookup in the alphabetic ordered arrays
  aWord := TrimU(LowerCase(aWord));
  if (aDB = dSQLite) or
     (FastFindPUtf8CharSorted(pointer(DB_KEYWORDS[dDefault]),
       high(DB_KEYWORDS[dDefault]), pointer(aWord)) < 0) then
    if aDB <= dDefault then
      result := false
    else
      result := FastFindPUtf8CharSorted(pointer(DB_KEYWORDS[aDB]),
        high(DB_KEYWORDS[aDB]), pointer(aWord)) >= 0
  else
    result := true;
end;

function TSqlDBConnectionProperties.IsSqlKeyword(aWord: RawUtf8): boolean;
begin
  result := IsSqlKeyword(GetDbms, aWord);
end;

procedure TSqlDBConnectionProperties.GetFieldDefinitions(
  const aTableName: RawUtf8; out Fields: TRawUtf8DynArray; WithForeignKeys: boolean);
var
  F: TSqlDBColumnDefineDynArray;
  Ref: RawUtf8;
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

procedure TSqlDBConnectionProperties.GetFields(const aTableName: RawUtf8;
  out Fields: TSqlDBColumnDefineDynArray);
var
  SQL: RawUtf8;
  n, i: integer;
  F: TSqlDBColumnDefine;
  FA: TDynArray;
begin
  FA.Init(TypeInfo(TSqlDBColumnDefineDynArray), Fields, @n);
  FA.Compare := SortDynArrayAnsiStringI; // FA.Find() case insensitive
  FillCharFast(F, sizeof(F), 0);
  if fDbms = dSQLite then
  begin
    // SQLite3 has a specific PRAGMA metadata query
    try
      with Execute('PRAGMA table_info(`' + aTableName + '`)', []) do
        while Step do
        begin
          // cid=0,name=1,type=2,notnull=3,dflt_value=4,pk=5
          F.ColumnName := ColumnUtf8(1);
          F.ColumnTypeNative := ColumnUtf8(2);
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
          with Execute('PRAGMA index_info(' + ColumnUtf8(1) + ')', []) do
            while Step do
            begin
              F.ColumnName := ColumnUtf8(2); // seqno=0,cid=1,name=2
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
    SQL := SqlGetField(aTableName);
    if SQL = '' then
      exit;
    with Execute(SQL, []) do
    begin
      while Step do
      begin
        F.ColumnName := TrimU(ColumnUtf8(0));
        F.ColumnTypeNative := TrimU(ColumnUtf8(1));
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
      ReleaseRows;
    end;
  end;
  SetLength(Fields, n);
end;

procedure TSqlDBConnectionProperties.GetIndexes(const aTableName: RawUtf8;
  out Indexes: TSqlDBIndexDefineDynArray);
var
  SQL: RawUtf8;
  n: integer;
  F: TSqlDBIndexDefine;
  FA: TDynArray;
begin
  SQL := SqlGetIndex(aTableName);
  if SQL = '' then
    exit;
  FA.Init(TypeInfo(TSqlDBIndexDefineDynArray), Indexes, @n);
  with Execute(SQL, []) do
  begin
    while Step do
    begin
      F.IndexName := TrimU(ColumnUtf8(0));
      F.IsUnique := ColumnInt(1) > 0;
      F.TypeDesc := TrimU(ColumnUtf8(2));
      F.IsPrimaryKey := ColumnInt(3) > 0;
      F.IsUniqueConstraint := ColumnInt(4) > 0;
      F.Filter := TrimU(ColumnUtf8(5));
      F.KeyColumns := TrimU(ColumnUtf8(6));
      F.IncludedColumns := TrimU(ColumnUtf8(7));
      FA.Add(F);
    end;
    ReleaseRows;
  end;
  SetLength(Indexes, n);
end;

procedure TSqlDBConnectionProperties.GetProcedureNames(out Procedures: TRawUtf8DynArray);
var
  SQL: RawUtf8;
  count: integer;
begin
  SQL := SqlGetProcedure;
  if SQL <> '' then
  try
    with Execute(SQL, []) do
    begin
      count := 0;
      while Step do
        AddSortedRawUtf8(Procedures, count, TrimU(ColumnUtf8(0)));
      SetLength(Procedures, count);
      ReleaseRows;
    end;
  except
    on Exception do
      SetLength(Procedures, 0); // if the supplied SQL query is wrong, just ignore
  end;
end;

procedure TSqlDBConnectionProperties.GetProcedureParameters(
  const aProcName: RawUtf8; out Parameters: TSqlDBProcColumnDefineDynArray);
var
  SQL: RawUtf8;
  n: integer;
  F: TSqlDBProcColumnDefine;
  FA: TDynArray;
begin
  FA.Init(TypeInfo(TSqlDBColumnDefineDynArray), Parameters, @n);
  FA.Compare := SortDynArrayAnsiStringI; // FA.Find() case insensitive
  FillcharFast(F, sizeof(F), 0);
  SQL := SqlGetParameter(aProcName);
  if SQL = '' then
    exit;
  with Execute(SQL, []) do
  begin
    while Step do
    begin
      F.ColumnName := TrimU(ColumnUtf8(0));
      F.ColumnTypeNative := TrimU(ColumnUtf8(1));
      F.ColumnLength := ColumnInt(2);
      F.ColumnPrecision := ColumnInt(3);
      if ColumnNull(4) then // e.g. for plain NUMERIC in Oracle
        F.ColumnScale := -1
      else
        F.ColumnScale := ColumnInt(4);
      F.ColumnType := ColumnTypeNativeToDB(F.ColumnTypeNative, F.ColumnScale);
      case FindCsvIndex('IN,OUT,INOUT', ColumnUtf8(5), ',', false) of
        0:
          F.ColumnParamType := paramIn;
        2:
          F.ColumnParamType := paramInOut;
      else // any other is assumed as out
        F.ColumnParamType := paramOut;
      end;
      FA.Add(F);
    end;
    ReleaseRows;
  end;
  SetLength(Parameters, n);
end;

procedure TSqlDBConnectionProperties.GetTableNames(out Tables: TRawUtf8DynArray);
var
  SQL, table, checkschema: RawUtf8;
  count: integer;
begin
  SQL := SqlGetTableNames;
  if SQL <> '' then
  try
    if FilterTableViewSchemaName and
       (fForcedSchemaName <> '') then
      checkschema := UpperCase(fForcedSchemaName) + '.';
    with Execute(SQL, []) do
    begin
      count := 0;
      while Step do
      begin
        table := TrimU(ColumnUtf8(0));
        if (checkschema = '') or
           IdemPChar(pointer(table), pointer(checkschema)) then
          AddSortedRawUtf8(Tables, count, table);
      end;
      SetLength(Tables, count);
      ReleaseRows;
    end;
  except
    on Exception do
      SetLength(Tables, 0); // if the supplied SQL query is wrong, just ignore
  end;
end;

procedure TSqlDBConnectionProperties.GetViewNames(out Views: TRawUtf8DynArray);
var
  SQL, table, checkschema: RawUtf8;
  count: integer;
begin
  SQL := SqlGetViewNames;
  if SQL <> '' then
  try
    if FilterTableViewSchemaName and
       (fForcedSchemaName <> '') then
      checkschema := UpperCase(fForcedSchemaName) + '.';
    with Execute(SQL, []) do
    begin
      count := 0;
      while Step do
      begin
        table := TrimU(ColumnUtf8(0));
        if (checkschema = '') or
           IdemPChar(pointer(table), pointer(checkschema)) then
          AddSortedRawUtf8(Views, count, table);
      end;
      SetLength(Views, count);
      ReleaseRows;
    end;
  except
    on Exception do
      SetLength(Views, 0); // if the supplied SQL query is wrong, just ignore
  end;
end;

procedure TSqlDBConnectionProperties.SqlSplitTableName(const aTableName: RawUtf8;
  out Owner, Table: RawUtf8);
begin
  case fDbms of
    dSQLite:
      Table := aTableName;
  else
    begin
      Split(aTableName, '.', Owner, Table);
      if Table = '' then
      begin
        Table := Owner;
        if fForcedSchemaName = '' then
          case fDbms of
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

procedure TSqlDBConnectionProperties.SqlSplitProcedureName(
  const aProcName: RawUtf8; out Owner, package, ProcName: RawUtf8);
var
  lOccur, i: integer;
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
  case fDbms of
    dSQLite:
      ProcName := aProcName;
    dOracle,
    dFirebird:
      begin
        // Firebird 3 has packages
        if lOccur = 2 then
        begin
          // OWNER.PACKAGE.PROCNAME
          Split(aProcName, '.', Owner, package);
          Split(package, '.', package, ProcName);
        end
        else
        begin
          // PACKAGE.PROCNAME
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
      else if fDbms = dMSSQL then
      // discard ;1 when MSSQL stored procedure name is ProcName;1
        Split(ProcName, ';', ProcName);
    end;
  end;
end;

function TSqlDBConnectionProperties.SqlFullTableName(const aTableName: RawUtf8): RawUtf8;
begin
  if (aTableName <> '') and
     (fForcedSchemaName <> '') and
     (PosExChar('.', aTableName) = 0) then
    result := fForcedSchemaName + '.' + aTableName
  else
    result := aTableName;
end;

function TSqlDBConnectionProperties.SqlGetField(const aTableName: RawUtf8): RawUtf8;
var
  Owner, Table: RawUtf8;
  FMT: RawUtf8;
begin
  result := '';
  case GetDbms of
    dOracle:
      FMT :=
        'select c.column_name, c.data_type, c.data_length, c.data_precision, c.data_scale, ' +
        ' (select count(*) from sys.all_indexes a, sys.all_ind_columns b' +
        '  where a.table_owner=c.owner and a.table_name=c.table_name and b.column_name=c.column_name' +
        '  and a.owner=b.index_owner and a.index_name=b.index_name and' +
        '  a.table_owner=b.table_owner and a.table_name=b.table_name) index_count' +
        ' from sys.all_tab_columns c' +
        ' where c.owner like ''%'' and c.table_name like ''%'';';
    dMSSQL,
    dMySQL,
    dPostgreSQL:
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
  SqlSplitTableName(aTableName, Owner, Table);
  FormatUtf8(FMT, [UpperCase(Owner), UpperCase(Table)], result);
end;

function TSqlDBConnectionProperties.SqlGetIndex(const aTableName: RawUtf8): RawUtf8;
var
  Owner, Table: RawUtf8;
  FMT: RawUtf8;
begin
  result := '';
  case GetDbms of
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
  FormatUtf8(FMT, [UpperCase(Table)], result);
end;

function TSqlDBConnectionProperties.SqlGetParameter(const aProcName: RawUtf8): RawUtf8;
var
  Owner, package, Proc: RawUtf8;
  FMT: RawUtf8;
begin
  result := '';
  SqlSplitProcedureName(aProcName, Owner, package, Proc);
  case GetDbms of
    dOracle:
      FMT :=
        'select a.argument_name, a.data_type, a.char_length, a.data_precision, a.data_scale, a.in_out ' +
        'from   sys.all_arguments a ' + 'where  a.owner like ''%''' +
        '  and  a.package_name like ''' + UpperCase(package) + '''' +
        '  and  a.object_name like ''%''' + ' order by position';
    dMSSQL,
    dMySQL,
    dPostgreSQL:
      FMT :=
        'select PARAMETER_NAME, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH, NUMERIC_PRECISION, NUMERIC_SCALE, PARAMETER_MODE ' +
        'from INFORMATION_SCHEMA.PARAMETERS ' +
        'where UPPER(SPECIFIC_SCHEMA) = ''%'' and UPPER(SPECIFIC_NAME) = ''%''' +
        ' order by ORDINAL_POSITION';
    dFirebird:
      begin
        if package = '' then
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
      begin
        // NOT TESTED !!!
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
  FormatUtf8(FMT, [UpperCase(Owner), UpperCase(Proc)], result);
end;

function TSqlDBConnectionProperties.SqlGetProcedure: RawUtf8;
var
  FMT, Owner: RawUtf8;
begin
  result := '';
  case GetDbms of
    dOracle:
      FMT := 'select' + '  case P.OBJECT_TYPE' +
        '  when ''PACKAGE'' then P.OBJECT_NAME || ''.'' || P.PROCEDURE_NAME' +
        '  else P.OBJECT_NAME end NAME_ROUTINE ' + 'from SYS.ALL_PROCEDURES P '
        + 'where P.OWNER = ''%'' and P.SUBPROGRAM_ID > 0 ' + 'order by NAME_ROUTINE';
    dMSSQL,
    dMySQL,
    dPostgreSQL:
      FMT := 'select R.SPECIFIC_NAME NAME_ROUTINE ' +
        'from INFORMATION_SCHEMA.ROUTINES R ' +
        'where UPPER(R.SPECIFIC_SCHEMA) = ''%'' ' + 'order by NAME_ROUTINE';
    dFirebird:
      FMT := 'select P.RDB$PROCEDURE_NAME NAME_ROUTINE ' +
        'from RDB$PROCEDURES P ' + 'where P.RDB$OWNER_NAME = ''%'' ' +
        'order by NAME_ROUTINE';
    dNexusDB:
      begin
        // NOT TESTED !!!
        result := 'select P.PROCEDURE_NAME NAME_ROUTINE ' +
          'from #PROCEDURES P ' + 'order by NAME_ROUTINE';
        exit;
      end;
  else
    exit; // others (e.g. dDB2) will retrieve info from (ODBC) driver
  end;
  SetSchemaNameToOwner(Owner);
  FormatUtf8(FMT, [UpperCase(Owner)], result);
end;

function TSqlDBConnectionProperties.SqlGetTableNames: RawUtf8;
begin
  case GetDbms of
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

function TSqlDBConnectionProperties.SqlGetViewNames: RawUtf8;
begin
  case GetDbms of
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

function TSqlDBConnectionProperties.SqlCreateDatabase(const aDatabaseName:
  RawUtf8; aDefaultPageSize: integer): RawUtf8;
begin
  case GetDbms of
    dFirebird:
      begin
        if (aDefaultPageSize <> 8192) or
           (aDefaultPageSize <> 16384) then
          aDefaultPageSize := 4096;
        FormatUtf8('create database ''%'' user ''sysdba'' password ''masterkey''' +
          ' page_size % default character set utf8;',
          [aDatabaseName, aDefaultPageSize], result);
      end;
  else
    result := '';
  end;
end;

function TSqlDBConnectionProperties.ColumnTypeNativeToDB(const aNativeType:
  RawUtf8; aScale: integer): TSqlDBFieldType;

  function ColumnTypeNativeDefault: TSqlDBFieldType;
  const
    DECIMAL = 18; // change it if you update PCHARS[] below before 'DECIMAL'
    NUMERIC = DECIMAL + 1;
    PCHARS: array[0..56] of PAnsiChar = (
      'TEXT COLLATE ISO8601', // should be before plain 'TEXT'
      'TEXT', 'CHAR', 'NCHAR', 'VARCHAR', 'NVARCHAR', 'CLOB', 'NCLOB', 'DBCLOB',
      'BIT', 'INT', 'BIGINT', 'DOUBLE', 'NUMBER', 'FLOAT', 'REAL', 'DECFLOAT',
      'CURR', 'DECIMAL', 'NUMERIC', 'BLOB SUB_TYPE 1', 'BLOB', 'DATE',
      'SMALLDATE', 'TIME', 'TINYINT', 'BOOL', 'SMALLINT', 'MEDIUMINT', 'SERIAL',
      'YEAR', 'TINYTEXT', 'MEDIUMTEXT', 'NTEXT', 'XML', 'ENUM', 'SET',
      'UNIQUEIDENTIFIER', 'MONEY', 'SMALLMONEY', 'NUM', 'VARRAW', 'RAW',
      'LONG RAW', 'LONG VARRAW', 'TINYBLOB', 'MEDIUMBLOB', 'BYTEA', 'VARBIN',
      'IMAGE', 'LONGBLOB', 'BINARY', 'VARBINARY', 'GRAPHIC', 'VARGRAPHIC',
      'NULL', nil);
    Types: array[-1 .. high(PCHARS) - 1] of TSqlDBFieldType = (
      ftUnknown, ftDate,
      ftUtf8, ftUtf8, ftUtf8, ftUtf8, ftUtf8, ftUtf8, ftUtf8, ftUtf8, ftInt64,
      ftInt64, ftInt64, ftDouble, ftDouble, ftDouble, ftDouble, ftDouble,
      ftCurrency, ftCurrency, ftCurrency, ftUtf8, ftBlob, ftDate, ftDate, ftDate,
      ftInt64, ftInt64, ftInt64, ftInt64, ftInt64, ftInt64, ftUtf8, ftUtf8,
      ftUtf8, ftUtf8, ftUtf8, ftUtf8, ftUtf8, ftCurrency, ftCurrency, ftCurrency,
      ftBlob, ftBlob, ftBlob, ftBlob, ftBlob, ftBlob, ftBlob, ftBlob, ftBlob,
      ftBlob, ftBlob, ftBlob, ftBlob, ftBlob, ftNull);
  var
    ndx: integer;
  begin
    //assert(StrComp(PCHARS[DECIMAL],'DECIMAL')=0);
    ndx := IdemPPChar(pointer(aNativeType), @PCHARS);
    if (aScale = 0) and
       (ndx in [DECIMAL, NUMERIC]) then
      result := ftInt64
    else
      result := Types[ndx]; // Types[-1]=ftUnknown
  end;

  function ColumnTypeNativeToDBOracle: TSqlDBFieldType;
  begin
    if PosEx('CHAR', aNativeType) > 0 then
      result := ftUtf8
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
    else if IdemPropNameU(aNativeType, 'DATE') or
            IdemPChar(pointer(aNativeType), 'TIMESTAMP') then
      result := ftDate
    else    // all other types will be converted to text
      result := ftUtf8;
  end;

  function ColumnTypeNativeToDBFirebird: TSqlDBFieldType;
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
        result := ftUtf8;
      end;
  end;

begin
  case GetDbms of
    dOracle:
      result := ColumnTypeNativeToDBOracle;
    dFireBird:
      result := ColumnTypeNativeToDBFirebird;
  else
    result := ColumnTypeNativeDefault;
  end;
end;

function TSqlDBConnectionProperties.GetForeignKey(
  const aTableName, aColumnName: RawUtf8): RawUtf8;
begin
  if not fForeignKeys.Initialized then
  begin
    fForeignKeys.Init(false);
    GetForeignKeys;
  end;
  result := fForeignKeys.Value(aTableName + '.' + aColumnName);
end;

function TSqlDBConnectionProperties.GetForeignKeysData: RawByteString;
begin
  if not fForeignKeys.Initialized then
  begin
    fForeignKeys.Init(false);
    GetForeignKeys;
  end;
  result := fForeignKeys.BlobData;
end;

procedure TSqlDBConnectionProperties.SetForeignKeysData(const Value: RawByteString);
begin
  if not fForeignKeys.Initialized then
    fForeignKeys.Init(false);
  fForeignKeys.BlobData := Value;
end;

function TSqlDBConnectionProperties.SqlIso8601ToDate(const Iso8601: RawUtf8): RawUtf8;

  function TrimTInIso: RawUtf8;
  begin
    result := Iso8601;
    if (length(result) > 10) and
       (result[11] = 'T') then
      result[11] := ' '; // 'T' -> ' '
  end;

begin
  case GetDbms of
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

function TSqlDBConnectionProperties.SqlDateToIso8601Quoted(DateTime: TDateTime): RawUtf8;
begin
  result := DateTimeToIso8601(DateTime, true, DateTimeFirstChar, false, '''');
end;

function TSqlDBConnectionProperties.SqlCreate(const aTableName: RawUtf8;
  const aFields: TSqlDBColumnCreateDynArray; aAddID: boolean): RawUtf8;
var
  i: integer;
  F: RawUtf8;
  FieldID: TSqlDBColumnCreate;
  AddPrimaryKey: RawUtf8;
begin
  // use 'ID' instead of 'RowID' here since some DB (e.g. Oracle) use it
  result := '';
  if high(aFields) < 0 then
    exit; // nothing to create
  if aAddID then
  begin
    FieldID.DBType := ftInt64;
    FieldID.Name := ID_TXT;
    FieldID.Unique := true;
    FieldID.NonNullable := true;
    FieldID.PrimaryKey := true;
    result := SqlFieldCreate(FieldID, AddPrimaryKey) + ',';
  end;
  for i := 0 to high(aFields) do
  begin
    F := SqlFieldCreate(aFields[i], AddPrimaryKey);
    if i <> high(aFields) then
      F := F + ',';
    result := result + F;
  end;
  if AddPrimaryKey <> '' then
    result := result + ', PRIMARY KEY(' + AddPrimaryKey + ')';
  result := 'CREATE TABLE ' + aTableName + ' (' + result + ')';
  case GetDbms of
    dDB2:
      result := result + ' CCSID Unicode';
  end;
end;

function TSqlDBConnectionProperties.SqlFieldCreate(const aField:
  TSqlDBColumnCreate; var aAddPrimaryKey: RawUtf8): RawUtf8;
begin
  if (aField.DBType = ftUtf8) and
     (cardinal(aField.Width - 1) < fSqlCreateFieldMax) then
    FormatUtf8(fSqlCreateField[ftNull], [aField.Width], result)
  else
    result := fSqlCreateField[aField.DBType];
  if aField.NonNullable or aField.Unique or aField.PrimaryKey then
    result := result + ' NOT NULL';
  if aField.Unique and
     not aField.PrimaryKey then
    result := result + ' UNIQUE'; // see http://www.w3schools.com/sql/sql_unique.asp
  if aField.PrimaryKey then
    case GetDbms of
      dSQLite,
      dMSSQL,
      dOracle,
      dJet,
      dPostgreSQL,
      dFirebird,
      dNexusDB,
      dInformix:
        result := result + ' PRIMARY KEY';
      dDB2,
      dMySQL:
        aAddPrimaryKey := aField.Name;
    end;
  result := aField.Name + result;
end;

function TSqlDBConnectionProperties.SqlAddColumn(const aTableName: RawUtf8;
  const aField: TSqlDBColumnCreate): RawUtf8;
var
  AddPrimaryKey: RawUtf8;
begin
  FormatUtf8('ALTER TABLE % ADD %', [aTableName,
    SqlFieldCreate(aField, AddPrimaryKey)], result);
end;

function TSqlDBConnectionProperties.SqlAddIndex(const aTableName: RawUtf8;
  const aFieldNames: array of RawUtf8; aUnique, aDescending: boolean;
  const aIndexName: RawUtf8): RawUtf8;
const
  CREATNDXIFNE: array[boolean] of RawUtf8 = (
    '', 'IF NOT EXISTS ');
var
  IndexName, FieldsCsv, ColsDesc, Owner, Table: RawUtf8;
begin
  result := '';
  if (self = nil) or
     (aTableName = '') or
     (high(aFieldNames) < 0) then
    exit;
  if aUnique then
    result := 'UNIQUE ';
  if aIndexName = '' then
  begin
    SqlSplitTableName(aTableName, Owner, Table);
    if (Owner <> '') and
       not (fDbms in [dMSSQL, dPostgreSQL, dMySQL, dFirebird, dDB2, dInformix]) then
      // some DB engines do not expect any schema in the index name
      IndexName := Owner + '.';
    FieldsCsv := RawUtf8ArrayToCsv(aFieldNames, '');
    if length(FieldsCsv) + length(Table) > 27 then
      // sounds like if some DB limit the identifier length to 32 chars
      IndexName := {%H-}IndexName + 'INDEX' + crc32cUtf8ToHex(Table) +
        crc32cUtf8ToHex(FieldsCsv)
    else
      IndexName := IndexName + 'NDX' + Table + FieldsCsv;
  end
  else
    IndexName := aIndexName;
  if aDescending then
    case DB_SQLDESENDINGINDEXPOS[GetDbms] of
      posGlobalBefore:
        result := result + 'DESC ';
      posWithColumn:
        ColsDesc := RawUtf8ArrayToCsv(aFieldNames, ' DESC,') + ' DESC';
    end;
  if {%H-}ColsDesc = '' then
    ColsDesc := RawUtf8ArrayToCsv(aFieldNames, ',');
  result := FormatUtf8('CREATE %INDEX %% ON %(%)', [result,
    CREATNDXIFNE[GetDbms in DB_HANDLECREATEINDEXIFNOTEXISTS],
    IndexName, aTableName, ColsDesc]);
end;

function TSqlDBConnectionProperties.SqlTableName(const aTableName: RawUtf8): RawUtf8;
var
  BeginQuoteChar, EndQuoteChar: RawUtf8;
  UseQuote: boolean;
begin
  BeginQuoteChar := '"';
  EndQuoteChar := '"';
  UseQuote := PosExChar(' ', aTableName) > 0;
  case fDbms of
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
  if UseQuote and
     (PosEx(BeginQuoteChar, aTableName) = 0) then
    result := BeginQuoteChar + aTableName + EndQuoteChar
  else
    result := aTableName;
end;

procedure TSqlDBConnectionProperties.GetIndexesAndSetFieldsColumnIndexed(
  const aTableName: RawUtf8; var Fields: TSqlDBColumnDefineDynArray);
var
  i, j: integer;
  ColName: RawUtf8;
  Indexes: TSqlDBIndexDefineDynArray;
begin
  if Fields = nil then
    exit;
  GetIndexes(aTableName, Indexes);
  for i := 0 to high(Indexes) do
  begin
    ColName := TrimU(GetCsvItem(pointer(Indexes[i].KeyColumns), 0));
    if ColName <> '' then
      for j := 0 to high(Fields) do
        if IdemPropNameU(Fields[j].ColumnName, ColName) then
        begin
          Fields[j].ColumnIndexed := true;
          break;
        end;
  end;
end;

function TSqlDBConnectionProperties.ExceptionIsAboutConnection(
  aClass: ExceptClass; const aMessage: RawUtf8): boolean;

  function PosErrorNumber(const aMessage: RawUtf8; aSepChar: AnsiChar): PUtf8Char;
  begin
    // search aSepChar followed by a number
    result := pointer(aMessage);
    repeat
      result := PosChar(result, aSepChar);
      if result = nil then
        exit;
      inc(result);
    until result^ in ['0'..'9'];
  end;

begin
  // see more complete list in feature request [f024266c0839]
  case fDbms of
    dOracle:
      result := IdemPCharArray(PosErrorNumber(aMessage, '-'),
        ['00028', '01012', '01017', '01033', '01089', '02396', '03113', '03114',
        '03135', '12152', '12154', '12157', '12514', '12520', '12537', '12545',
        '12560', '12571']) >= 0;
    dInformix:
      // error codes based on {IBM INFORMIX ODBC DRIVER} tested with wrong data connection
      result := IdemPCharArray(PosErrorNumber(aMessage, '-'),
        ['329', '761', '902', '908', '930', '931', '951', '11017', '23101',
         '23104', '25567', '25582', '27002']) >= 0;
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

procedure TSqlDBConnectionProperties.MultipleValuesInsert(
  Props: TSqlDBConnectionProperties; const TableName: RawUtf8;
  const FieldNames: TRawUtf8DynArray; const FieldTypes: TSqlDBFieldTypeArray;
  RowCount: integer; const FieldValues: TRawUtf8DynArrayDynArray);
var
  sql: RawUtf8;
  sqlcached: boolean;
  prevrowcount: integer;
  maxf: PtrInt;

  procedure ComputeSql(rowcount, offset: integer);
  var
    f, r, p, len: PtrInt;
    tmp: TTextWriterStackBuffer;
  begin
    if (fDbms <> dFireBird) and
       (rowcount = prevrowcount) then
      exit;
    prevrowcount := rowcount;
    with TTextWriter.CreateOwnedStream(tmp) do
    try
      case Props.fDbms of
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
                  AddShorter(' CHAR(1)')
                else
                  case FieldTypes[f] of
                    ftNull:
                      AddShorter(' CHAR(1)');
                    ftUtf8:
                      begin
                        len := length(FieldValues[f, r]) - 2;
                        if len < 1 then
                          len := 1; // unquoted UTF-8 text length
                        AddShort(' VARCHAR(');
                        AddU(len);
                        AddShort(') CHARACTER SET UTF8');
                      end;
                  else
                    AddString(DB_FIELDS[dFirebird, FieldTypes[f]]);
                  end;
                AddShorter('=?,');
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
                AddComma;
              end;
              CancelLastComma;
              AddShort(') VALUES (');
              for f := 0 to maxf do
              begin
                inc(p);
                Add(':', 'i');
                AddU(p);
                AddComma;
              end;
              CancelLastComma;
              AddShorter(');'#10);
            end;
            AddShorter('end');
            if TextLength > 32700 then
              raise ESqlDBException.CreateUtf8(
                '%.MultipleValuesInsert: Firebird Execute Block length=%',
                [self, TextLength]);
            sqlcached := false; // ftUtf8 values will have varying field length
          end;
        dOracle:
          begin
            // INSERT ALL INTO ... VALUES ... SELECT 1 FROM DUAL
            AddShort('insert all'#10); // see http://stackoverflow.com/a/93724
            for r := 1 to rowcount do
            begin
              AddShorter('into ');
              AddString(TableName);
              Add(' ', '(');
              for f := 0 to maxf do
              begin
                AddString(FieldNames[f]);
                AddComma;
              end;
              CancelLastComma;
              AddShort(') VALUES (');
              for f := 0 to maxf do
                Add('?', ',');
              CancelLastComma;
              Add(')', #10);
            end;
            AddShort('select 1 from dual');
            sqlcached := true;
          end;
      else
        begin
          //  e.g. NexusDB/SQlite3/MySQL/PostgreSQL/MSSQL2008/DB2/INFORMIX
          AddShort('INSERT INTO '); // INSERT .. VALUES (..),(..),(..),..
          AddString(TableName);
          Add(' ', '(');
          for f := 0 to maxf do
          begin
            AddString(FieldNames[f]);
            AddComma;
          end;
          CancelLastComma;
          AddShort(') VALUES ');
          for r := 1 to rowcount do
          begin
            Add('(');
            for f := 0 to maxf do
              Add('?', ',');
            CancelLastComma;
            Add(')', ',');
          end;
          CancelLastComma;
          sqlcached := true;
        end;
      end;
      SetText(sql);
    finally
      Free;
    end;
  end;

var
  batchRowCount, paramCountLimit, currentRow, sqllen, p: integer;
  f, i: PtrInt;
  Stmt: TSqlDBStatement;
  Query: ISqlDBStatement;
begin
  maxf := length(FieldNames);     // e.g. 2 fields
  if (Props = nil) or
     (FieldNames = nil) or
     (TableName = '') or
     (length(FieldValues) <> maxf) then
    raise ESqlDBException.CreateUtf8('Invalid %.MultipleValuesInsert(%) call',
      [self, TableName]);
  batchRowCount := 0;
  paramCountLimit := DB_PARAMSMAX[Props.fDbms];
  case Props.fDbms of
    dFirebird:
      begin
        // compute from max sql statement size of 32KB
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
    raise ESqlDBException.CreateUtf8(
      '%.MultipleValuesInsert(%) with # params = %>%',
      [self, TableName, RowCount * maxf, paramCountLimit]);
  dec(maxf);
  prevrowcount := 0;
  sqlcached := false;
  currentRow := 0;
  repeat
    if RowCount - currentRow > batchRowCount then
      // max number of params -> try cache
      ComputeSql(batchRowCount, currentRow)
    else
    begin
      ComputeSql(RowCount - currentRow, currentRow);
      sqlcached := false; // truncate number of parameters should not be unique
    end;
    if sqlcached then
      Query := Props.NewThreadSafeStatementPrepared(sql, false)
    else
    begin
      Stmt := Props.NewThreadSafeStatement;
      try
        Stmt.Prepare(sql, false);
        Query := Stmt; // Stmt will be released by Query := nil below
      except
        on Exception do
          Stmt.Free; // avoid memory leak in case of invalid sql statement
        // exception leaves Query=nil to raise exception
      end;
    end;
    if Query = nil then
      raise ESqlDBException.CreateUtf8(
        '%.MultipleValuesInsert: Query=nil for [%]', [self, sql]);
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

procedure TSqlDBConnectionProperties.MultipleValuesInsertFirebird(
  Props: TSqlDBConnectionProperties; const TableName: RawUtf8;
  const FieldNames: TRawUtf8DynArray; const FieldTypes: TSqlDBFieldTypeArray;
  RowCount: integer; const FieldValues: TRawUtf8DynArrayDynArray);
var
  W: TTextWriter;
  maxf, sqllenwitoutvalues, sqllen, r, f, i: PtrInt;
  v: RawUtf8;
begin
  maxf := length(FieldNames);     // e.g. 2 fields
  if (Props = nil) or
     (FieldNames = nil) or
     (TableName = '') or
     (length(FieldValues) <> maxf) or
     (Props.fDbms <> dFirebird) then
    raise ESqlDBException.CreateUtf8(
      'Invalid %.MultipleValuesInsertFirebird(%,%)', [self, Props, TableName]);
  sqllenwitoutvalues := 3 * maxf + 24;
  dec(maxf);
  for f := 0 to maxf do
    case FieldTypes[f] of
      ftBlob:
        begin
          // not possible to inline BLOBs -> fallback to regular
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
          W.AddComma;
        end;
        W.CancelLastComma;
        W.AddShort(') VALUES (');
        for f := 0 to maxf do
        begin
          v := FieldValues[f, r]; // includes single quotes (#39)
          if (v = '') or
             (v = 'null') then
            W.AddNull
          else if FieldTypes[f] = ftDate then
            if v = #39#39 then
              W.AddNull
            else
            begin
              W.AddShort('timestamp ');
              if length(v) > 12 then
              begin
                // not 'CCYY-MM-DD' -> fix needed?
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
          W.AddComma;
        end;
        W.CancelLastComma;
        W.AddShorter(');'#10);
        inc(r);
      until r = RowCount;
      W.AddShorter('end');
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

function TSqlDBConnectionProperties.FieldsFromList(
  const aFields: TSqlDBColumnDefineDynArray; aExcludeTypes: TSqlDBFieldTypes): RawUtf8;
var
  i, n: PtrInt;
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

function TSqlDBConnectionProperties.SqlSelectAll(const aTableName: RawUtf8;
  const aFields: TSqlDBColumnDefineDynArray; aExcludeTypes: TSqlDBFieldTypes): RawUtf8;
begin
  if (self = nil) or
     (aTableName = '') then
    result := ''
  else
    result := 'select ' + FieldsFromList(aFields, aExcludeTypes) +
              ' from ' + SqlTableName(aTableName);
end;

{$ifdef ISDELPHI20062007}
  {$warnings off} // avoid paranoid Delphi 2007 warning
{$endif ISDELPHI20062007}

class function TSqlDBConnectionProperties.EngineName: RawUtf8;
var
  L: integer;
begin
  result := '';
  if self = nil then
    exit;
  ClassToText(self, result);
  if IdemPChar(pointer(result), 'TSqlDB') then
    Delete(result, 1, 6)
  else if result[1] = 'T' then
    Delete(result, 1, 1);
  L := length(result);
  if (L > 20) and
     IdemPropName('ConnectionProperties', @result[L - 19], 20) then
    SetLength(result, L - 20);
  if (L > 5) and
     IdemPropName('OleDB', pointer(result), 5) then
    Delete(result, 1, 5)
  else if (L > 4) and
       IdemPropName('ODBC', pointer(result), 4) then
      Delete(result, 1, 4);
end;

{$ifdef ISDELPHI20062007}
  {$warnings on}
{$endif ISDELPHI20062007}

function TSqlDBConnectionProperties.GetDbms: TSqlDBDefinition;
begin
  if fDbms = dUnknown then
    result := dDefault
  else
    result := fDbms;
end;

function TSqlDBConnectionProperties.GetDbmsName: RawUtf8;
var
  PS: PShortString;
begin
  PS := ToText(GetDbms);
  FastSetString(result, @PS^[2], ord(PS^[0]) - 1);
end;

function TSqlDBConnectionProperties.GetDatabaseNameSafe: RawUtf8;
begin
  result := StringReplaceAll(fDatabaseName, PassWord, '***');
end;

function TSqlDBConnectionProperties.SqlLimitClause(AStmt: TSelectStatement):
  TSqlDBDefinitionLimitClause;
begin
  result := DB_SQLLIMITCLAUSE[GetDbms];
end;

var
  GlobalDefinitions: array of TSqlDBConnectionPropertiesClass;

class procedure TSqlDBConnectionProperties.RegisterClassNameForDefinition;
begin
  ObjArrayAddOnce(GlobalDefinitions, TObject(self)); // TClass stored as TObject
end;

procedure TSqlDBConnectionProperties.DefinitionTo(Definition: TSynConnectionDefinition);
begin
  if Definition = nil then
    exit;
  Definition.Kind := ClassName;
  Definition.ServerName := ServerName;
  Definition.DatabaseName := DatabaseName;
  Definition.User := UserID;
  Definition.PassWordPlain := PassWord;
end;

function TSqlDBConnectionProperties.DefinitionToJson(Key: cardinal): RawUtf8;
var
  Definition: TSynConnectionDefinition;
begin
  Definition := TSynConnectionDefinition.Create;
  try
    Definition.Key := Key;
    DefinitionTo(Definition);
    result := Definition.SaveToJson;
  finally
    Definition.Free;
  end;
end;

procedure TSqlDBConnectionProperties.DefinitionToFile(
  const aJsonFile: TFileName; Key: cardinal);
begin
  FileFromString(JsonReformat(DefinitionToJson(Key)), aJsonFile);
end;

class function TSqlDBConnectionProperties.ClassFrom(
  aDefinition: TSynConnectionDefinition): TSqlDBConnectionPropertiesClass;
var
  ndx: PtrInt;
begin
  for ndx := 0 to length(GlobalDefinitions) - 1 do
    if GlobalDefinitions[ndx].ClassNameIs(aDefinition.Kind) then
    begin
      result := GlobalDefinitions[ndx];
      exit;
    end;
  result := nil;
end;

class function TSqlDBConnectionProperties.CreateFrom(
  aDefinition: TSynConnectionDefinition): TSqlDBConnectionProperties;
var
  C: TSqlDBConnectionPropertiesClass;
begin
  C := ClassFrom(aDefinition);
  if C = nil then
    raise ESqlDBException.CreateUtf8('%.CreateFrom: unknown % class - please ' +
      'add a reference to its implementation unit', [self, aDefinition.Kind]);
  result := C.Create(aDefinition.ServerName, aDefinition.DatabaseName,
    aDefinition.User, aDefinition.PassWordPlain);
end;

class function TSqlDBConnectionProperties.CreateFromJson(
  const aJsonDefinition: RawUtf8; aKey: cardinal): TSqlDBConnectionProperties;
var
  Definition: TSynConnectionDefinition;
begin
  Definition := TSynConnectionDefinition.CreateFromJson(aJsonDefinition, aKey);
  try
    result := CreateFrom(Definition);
  finally
    Definition.Free;
  end;
end;

class function TSqlDBConnectionProperties.CreateFromFile(
  const aJsonFile: TFileName; aKey: cardinal): TSqlDBConnectionProperties;
begin
  result := CreateFromJson(AnyTextFileToRawUtf8(aJsonFile, true), aKey);
end;


{ TSqlDBStatement }

procedure TSqlDBStatement.Bind(Param: integer; const Data: TSqlVar;
  IO: TSqlDBParamInOutType);
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
      ftUtf8:
        BindTextP(Param, VText, IO);
      ftBlob:
        BindBlob(Param, VBlob, VBlobLen, IO);
    else
      raise ESqlDBException.CreateUtf8('%.Bind(Param=%,VType=%)',
        [self, Param, ord(VType)]);
    end;
end;

procedure TSqlDBStatement.Bind(Param: integer; ParamType: TSqlDBFieldType;
  const Value: RawUtf8; ValueAlreadyUnquoted: boolean; IO: TSqlDBParamInOutType);
var
  tmp: RawUtf8;
begin
  if not ValueAlreadyUnquoted and
     (Value = 'null') then
    // bind null (ftUtf8 should be '"null"')
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
            UnQuoteSqlStringVar(pointer(Value), tmp);
          BindDateTime(Param, Iso8601ToDateTime(tmp), IO);
        end;
      ftUtf8:
        if (fConnection <> nil) and
           fConnection.fProperties.StoreVoidStringAsNull and
           ((Value = '') or // check if '' or '""' should be stored as null
            ((PInteger(Value)^ and $ffffff = $2727) and
             not ValueAlreadyUnquoted)) then
          BindNull(Param, IO, ftUtf8)
        else
        begin
          if ValueAlreadyUnquoted then
            tmp := Value
          else
            UnQuoteSqlStringVar(pointer(Value), tmp);
          BindTextU(Param, tmp, IO);
        end;
    else
      raise ESqlDBException.CreateUtf8(
        'Invalid %.Bind(%,TSqlDBFieldType(%),%)',
        [self, Param, ord(ParamType), Value]);
    end;
end;

function VariantIsBlob(const V: variant): boolean;
begin
  with TVarData(V) do
    result := (VType = varNull) or
              ((VType = varString) and
               (VString <> nil) and
               (PCardinal(VString)^ and $ffffff = JSON_BASE64_MAGIC_C));
end;

procedure TSqlDBStatement.Bind(const Params: array of const; IO: TSqlDBParamInOutType);
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
            if c = JSON_BASE64_MAGIC_C then
              BindBlob(i, Base64ToBin(PAnsiChar(VAnsiString) + 3,
                length(RawUtf8(VAnsiString)) - 3))
            else if c = JSON_SQLDATE_MAGIC_C then
              BindDateTime(i, Iso8601ToDateTimePUtf8Char(PUtf8Char(VAnsiString) + 3,
                length(RawUtf8(VAnsiString)) - 3))
            else
              {$ifdef HASCODEPAGE}
              BindTextU(i, AnyAnsiToUtf8(RawByteString(VAnsiString)), IO);
              {$else}
              // expect UTF-8 content only for AnsiString, i.e. RawUtf8 values
              BindTextU(i, RawUtf8(VAnsiString), IO);
              {$endif HASCODEPAGE}
          end;
        vtPChar:
          BindTextP(i, PUtf8Char(VPChar), IO);
        vtChar:
          BindTextU(i, RawUtf8(VChar), IO);
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
        {$endif UNICODE}
        {$endif HASVARUSTRING}
        vtBoolean:
          if VBoolean then // normalize
            Bind(i, 1, IO)
          else
            Bind(i, 0, IO);
        vtInteger:
          Bind(i, VInteger, IO);
        vtInt64:
          Bind(i, VInt64^, IO);
        {$ifdef FPC}
        vtQWord:
          Bind(i, VQWord^, IO);
        {$endif FPC}
        vtCurrency:
          BindCurrency(i, VCurrency^, IO);
        vtExtended:
          Bind(i, VExtended^, IO);
        vtPointer:
          if VPointer = nil then
            BindNull(i, IO)
          else
            raise ESqlDBException.CreateUtf8(
              'Unexpected %.Bind() pointer', [self]);
        vtVariant:
          BindVariant(i, VVariant^, VariantIsBlob(VVariant^), IO);
      else
        raise ESqlDBException.CreateUtf8(
          '%.BindArrayOfConst(Param=%,Type=%)', [self, i, VType]);
      end;
end;

procedure TSqlDBStatement.BindVariant(Param: integer; const Data: Variant;
  DataIsBlob: boolean; IO: TSqlDBParamInOutType);
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
      varInt64,
      varWord64:
        Bind(Param, VInt64, IO);
      varSingle:
        Bind(Param, VSingle, IO);
      varDouble:
        Bind(Param, VDouble, IO);
      varDate:
        BindDateTime(Param, VDate, IO);
      varCurrency:
        BindCurrency(Param, VCurrency, IO);
      varOleStr:
        // handle special case if was bound explicitly as WideString
        BindTextW(Param, WideString(VAny), IO);
      {$ifdef HASVARUSTRING}
      varUString:
        if DataIsBlob then
          raise ESqlDBException.CreateUtf8(
            '%.BindVariant: BLOB should not be UnicodeString', [self])
        else
          BindTextU(Param, UnicodeStringToUtf8(UnicodeString(VAny)), IO);
      {$endif HASVARUSTRING}
      varString:
        if DataIsBlob then
          if (VAny <> nil) and
             (PInteger(VAny)^ and $00ffffff = JSON_BASE64_MAGIC_C) then
            // recognized as Base64 encoded text
            BindBlob(Param, Base64ToBin(PAnsiChar(VAny) + 3,
              length(RawByteString(VAny)) - 3))
          else
            // no conversion if was set via TQuery.AsBlob property e.g.
            BindBlob(Param, RawByteString(VAny), IO)
        else
          // direct bind of AnsiString as UTF-8 value
          {$ifdef HASCODEPAGE}
          BindTextU(Param, AnyAnsiToUtf8(RawByteString(VAny)), IO);
          {$else} // on older Delphi, we assume AnsiString = RawUtf8
          BindTextU(Param, RawUtf8(VAny), IO);
          {$endif HASCODEPAGE}
    else
      if VType = varVariantByRef then
        BindVariant(Param, PVariant(VPointer)^, DataIsBlob, IO)
      else if VType = varOleStrByRef then
        BindTextW(Param, PWideString(VAny)^, IO)
      else
        // also use TEXT for any non native VType parameter
        BindTextU(Param, VariantToUtf8(Data), IO);
    end;
end;

procedure TSqlDBStatement.BindArray(Param: integer; ParamType: TSqlDBFieldType;
  const Values: TRawUtf8DynArray; ValuesCount: integer);
begin
  if (Param <= 0) or
     (ParamType in [ftUnknown, ftNull]) or
     (ValuesCount <= 0) or
     (length(Values) < ValuesCount) or
     (fConnection = nil) or
     (fConnection.fProperties.BatchSendingAbilities *
      [cCreate, cUpdate, cDelete] = []) then
    raise ESqlDBException.CreateUtf8(
      'Invalid call to %.BindArray(Param=%,Type=%)',
      [self, Param, ToText(ParamType)^]);
end;

procedure TSqlDBStatement.BindArray(Param: integer; const Values: array of Int64);
begin
  BindArray(Param, ftInt64, nil, 0); // will raise an exception (Values=nil)
end;

procedure TSqlDBStatement.BindArray(Param: integer; const Values: array of RawUtf8);
begin
  BindArray(Param, ftUtf8, nil, 0); // will raise an exception (Values=nil)
end;

procedure TSqlDBStatement.BindArray(Param: integer; const Values: array of double);
begin
  BindArray(Param, ftDouble, nil, 0); // will raise an exception (Values=nil)
end;

procedure TSqlDBStatement.BindArrayCurrency(Param: integer;
  const Values: array of currency);
begin
  BindArray(Param, ftCurrency, nil, 0); // will raise an exception (Values=nil)
end;

procedure TSqlDBStatement.BindArrayDateTime(Param: integer;
  const Values: array of TDateTime);
begin
  BindArray(Param, ftDate, nil, 0); // will raise an exception (Values=nil)
end;

procedure TSqlDBStatement.CheckCol(Col: integer);
begin
  if (self = nil) or
     (cardinal(Col) >= cardinal(fColumnCount)) then
    CheckColInvalid(Col);
end;

procedure TSqlDBStatement.CheckColInvalid(Col: integer);
begin
  raise ESqlDBException.CreateUtf8(
    'Invalid call to %.Column*(Col=%)', [self, Col]);
end;

function TSqlDBStatement.GetForceBlobAsNull: boolean;
begin
  result := fForceBlobAsNull;
end;

procedure TSqlDBStatement.SetForceBlobAsNull(value: boolean);
begin
  fForceBlobAsNull := value;
end;

function TSqlDBStatement.GetForceDateWithMS: boolean;
begin
  result := fForceDateWithMS;
end;

procedure TSqlDBStatement.SetForceDateWithMS(value: boolean);
begin
  fForceDateWithMS := value;
end;

constructor TSqlDBStatement.Create(aConnection: TSqlDBConnection);
begin
  inherited Create;
  fConnection := aConnection;
  fStripSemicolon := true;
  fCacheIndex := -1;
  if aConnection <> nil then
    fDbms := aConnection.fProperties.GetDbms;
end;

function TSqlDBStatement.ColumnCount: integer;
begin
  if self = nil then
    result := 0
  else
    result := fColumnCount;
end;

function TSqlDBStatement.ColumnBlobBytes(Col: integer): TBytes;
begin
  RawByteStringToBytes(ColumnBlob(Col), result);
end;

procedure TSqlDBStatement.ColumnBlobToStream(Col: integer; Stream: TStream);
var
  tmp: RawByteString;
begin
  tmp := ColumnBlob(Col); // default implementation
  Stream.WriteBuffer(pointer(tmp)^, Length(tmp));
end;

procedure TSqlDBStatement.ColumnBlobFromStream(Col: integer; Stream: TStream);
begin
  raise ESqlDBException.CreateUtf8(
    '%.ColumnBlobFromStream not implemented', [self]);
end;

function TSqlDBStatement.ColumnVariant(Col: integer): Variant;
begin
  ColumnToVariant(Col, result);
end;

function TSqlDBStatement.ColumnToVariant(Col: integer;
  var Value: Variant): TSqlDBFieldType;
var
  tmp: RawByteString;
  V: TSqlVar;
begin
  ColumnToSqlVar(Col, V, tmp);
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
      ftUtf8:
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
                CurrentAnsiConvert.Utf8BufferToAnsi(
                  V.VText, V.VBlobLen, RawByteString(VAny));
            end
            else
            {$endif UNICODE}
              Utf8ToSynUnicode(V.VText, V.VBlobLen, SynUnicode(VAny));
          end
          else
            // avoid obscure "Invalid variant type" in FPC
            VType := varString;
        end;
    else
      raise ESqlDBException.CreateUtf8('%.ColumnToVariant: Invalid ColumnType(%)=%',
        [self, Col, ord(result)]);
    end;
  end;
end;

function TSqlDBStatement.ColumnTimestamp(Col: integer): TTimeLog;
begin
  case ColumnType(Col) of // will call GetCol() to check Col
    ftNull:
      result := 0;
    ftInt64:
      result := ColumnInt(Col);
    ftDate:
      PTimeLogBits(@result)^.From(ColumnDateTime(Col));
  else
    PTimeLogBits(@result)^.From(TrimU(ColumnUtf8(Col)));
  end;
end;

function TSqlDBStatement.ColumnTimestamp(const ColName: RawUtf8): TTimeLog;
begin
  result := ColumnTimestamp(ColumnIndex(ColName));
end;

procedure TSqlDBStatement.ColumnsToJson(WR: TJsonWriter);
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
      WR.AddNull
    else
      case ColumnType(col) of
        ftNull:
          WR.AddNull;
        ftInt64:
          WR.Add(ColumnInt(col));
        ftDouble:
          WR.AddDouble(ColumnDouble(col));
        ftCurrency:
          WR.AddCurr(ColumnCurrency(col));
        ftDate:
          begin
            WR.Add('"');
            WR.AddDateTime(ColumnDateTime(col), fForceDateWithMS);
            WR.Add('"');
          end;
        ftUtf8:
          begin
            WR.Add('"');
            WR.AddJsonEscape(pointer(ColumnUtf8(col)));
            WR.Add('"');
          end;
        ftBlob:
          if fForceBlobAsNull then
            WR.AddNull
          else
          begin
            blob := ColumnBlob(col);
            WR.WrBase64(pointer(blob), length(blob), {withMagic=}true);
          end;
      else
        raise ESqlDBException.CreateUtf8(
          '%.ColumnsToJson: invalid ColumnType(%)=%',
          [self, col, ord(ColumnType(col))]);
      end;
    WR.AddComma;
  end;
  WR.CancelLastComma; // cancel last ','
  if WR.Expand then
    WR.Add('}');
end;

procedure TSqlDBStatement.ColumnToSqlVar(Col: integer; var Value: TSqlVar;
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
    ftUtf8:
      begin
        Temp := ColumnUtf8(Col);
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

function TSqlDBStatement.ColumnToTypedValue(Col: integer;
  DestType: TSqlDBFieldType; var Dest): TSqlDBFieldType;
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
    ftUtf8:
      VariantToUtf8(Temp, RawUtf8(Dest));
    ftBlob:
      VariantToRawByteString(Temp, RawByteString(Dest));
  else
    raise ESqlDBException.CreateUtf8('%.ColumnToTypedValue: Invalid Type [%]',
      [self, ToText(result)^]);
  end;
end;

function TSqlDBStatement.ParamToVariant(Param: integer; var Value: Variant;
  CheckIsOutParameter: boolean): TSqlDBFieldType;
begin
  dec(Param); // start at #1
  if (self = nil) or
     (cardinal(Param) >= cardinal(fParamCount)) then
    raise ESqlDBException.CreateUtf8('%.ParamToVariant(%)', [self, Param]);
  // overridden method should fill Value with proper data
  result := ftUnknown;
end;

procedure TSqlDBStatement.Execute(const aSql: RawUtf8; ExpectResults: boolean);
begin
  Connection.InternalProcess(speActive);
  try
    Prepare(aSql, ExpectResults);
    SetForceBlobAsNull(true);
    ExecutePrepared;
  finally
    Connection.InternalProcess(speNonActive);
  end;
end;

function TSqlDBStatement.FetchAllToJson(Json: TStream; Expanded: boolean): PtrInt;
var
  W: TJsonWriter;
  col: integer;
  maxmem: PtrUInt;
  tmp: TTextWriterStackBuffer;
begin
  result := 0;
  W := TJsonWriter.Create(Json, Expanded, false, nil, 0, @tmp);
  try
    Connection.InternalProcess(speActive);
    maxmem := Connection.Properties.StatementMaxMemory;
    // get col names and types
    SetLength(W.ColNames, ColumnCount);
    for col := 0 to ColumnCount - 1 do
      W.ColNames[col] := ColumnName(col);
    W.AddColumns; // write or init field names for appropriate JSON Expand
    if Expanded then
      W.Add('[');
    // write rows data
    {$ifdef SYNDB_SILENCE}
    fSqlLogTimer.Resume; // log fetch duration
    {$endif SYNDB_SILENCE}
    while Step do
    begin
      ColumnsToJson(W);
      W.AddComma;
      inc(result);
      if (maxmem > 0) and
         (W.WrittenBytes > maxmem) then // TextLength is slower
        raise ESQLDBException.CreateUTF8('%.FetchAllToJson: overflow %',
          [self, KB(maxmem)]);
    end;
    {$ifdef SYNDB_SILENCE}
    fSqlLogTimer.Pause;
    {$endif SYNDB_SILENCE}
    ReleaseRows;
    if (result = 0) and
       W.Expand then
    begin
      // we want the field names at least, even with no data (RowCount=0)
      W.Expand := false; //  {"FieldCount":2,"Values":["col1","col2"]}
      W.CancelAll;
      for col := 0 to ColumnCount - 1 do
        W.ColNames[col] := ColumnName(col); // previous W.AddColumns did nothing
      W.AddColumns;
    end;
    W.EndJsonObject(0, result);
  finally
    W.Free;
    Connection.InternalProcess(speNonActive);
  end;
end;

function TSqlDBStatement.FetchAllToCsvValues(Dest: TStream; Tab: boolean;
  CommaSep: AnsiChar; AddBOM: boolean): PtrInt;
const
  NULL: array[boolean] of string[7] = (
    '"null"', 'null');
  BLOB: array[boolean] of string[7] = (
    '"blob"', 'blob');
var
  F, FMax: integer;
  maxmem: PtrUInt;
  W: TTextWriter;
  tmp: RawByteString;
  V: TSqlVar;
begin
  result := 0;
  if (Dest = nil) or
     (self = nil) or
     (ColumnCount = 0) then
    exit;
  fForceBlobAsNull := true;
  if Tab then
    CommaSep := #9;
  FMax := ColumnCount - 1;
  maxmem := Connection.Properties.StatementMaxMemory;
  W := TTextWriter.Create(Dest, 65536);
  try
    // add optional/deprecated/Windows-centric UTF-8 Byte Order Mark
    if AddBOM then
      W.AddShorter(#$ef#$bb#$bf);
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
    fSqlLogTimer.Resume;
    {$endif SYNDB_SILENCE}
    while Step do
    begin
      for F := 0 to FMax do
      begin
        ColumnToSqlVar(F, V, tmp);
        case V.VType of
          ftNull:
            W.AddShorter(NULL[Tab]);
          ftInt64:
            W.Add(V.VInt64);
          ftDouble:
            W.AddDouble(V.VDouble);
          ftCurrency:
            W.AddCurr(V.VCurrency);
          ftDate:
            begin
              if not Tab then
                W.Add('"');
              W.AddDateTime(V.VDateTime, svoDateWithMS in V.Options);
              if not Tab then
                W.Add('"');
            end;
          ftUtf8:
            begin
              if not Tab then
              begin
                W.Add('"');
                W.AddJsonEscape(V.VText);
                W.Add('"');
              end
              else
                W.AddNoJsonEscape(V.VText);
            end;
          ftBlob:
            W.AddShorter(BLOB[Tab]);  // ForceBlobAsNull should be true
        else
          raise ESqlDBException.CreateUtf8(
            '%.FetchAllToCsvValues: Invalid ColumnType(%) %',
            [self, F, ToText(ColumnType(F))^]);
        end;
        if F = FMax then
          W.AddCR
        else
          W.Add(CommaSep);
      end;
      inc(result);
      if (maxmem > 0) and
         (W.WrittenBytes > maxmem) then // TextLength is slower
        raise ESQLDBException.CreateUTF8('%.FetchAllToCsvValues: overflow %',
          [self, KB(maxmem)]);
    end;
    {$ifdef SYNDB_SILENCE}
    fSqlLogTimer.Pause;
    {$endif SYNDB_SILENCE}
    ReleaseRows;
    W.FlushFinal;
  finally
    W.Free;
  end;
end;

function TSqlDBStatement.FetchAllAsJson(Expanded: boolean; ReturnedRowCount:
  PPtrInt): RawUtf8;
var
  Stream: TRawByteStringStream;
  RowCount: PtrInt;
begin
  Stream := TRawByteStringStream.Create;
  try
    RowCount := FetchAllToJson(Stream, Expanded);
    if ReturnedRowCount <> nil then
      ReturnedRowCount^ := RowCount;
    result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

procedure TSqlDBStatement.ColumnsToBinary(W: TBufferWriter; Null: pointer;
  const ColTypes: TSqlDBFieldTypeDynArray);
var
  F: integer;
  VDouble: double;
  VCurrency: currency absolute VDouble;
  VDateTime: TDateTime absolute VDouble;
  ft: TSqlDBFieldType;
begin
  for F := 0 to length(ColTypes) - 1 do
    if not GetBitPtr(Null, F) then
    begin
      ft := ColTypes[F];
      if ft < ftInt64 then
      begin
        // ftUnknown,ftNull
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
        ftUtf8:
          W.Write(ColumnUtf8(F));
        ftBlob:
          W.Write(ColumnBlob(F));
      else
        raise ESqlDBException.CreateUtf8(
          '%.ColumnsToBinary: Invalid ColumnType(%)=%',
          [self, ColumnName(F), ord(ft)]);
      end;
    end;
end;

function TSqlDBStatement.FetchAllToBinary(Dest: TStream; MaxRowCount: cardinal;
  DataRowPosition: PCardinalDynArray): cardinal;
var
  F, FMax, FieldSize, NullRowSize: integer;
  StartPos: Int64;
  maxmem: PtrUInt;
  W: TBufferWriter;
  ft: TSqlDBFieldType;
  ColTypes: TSqlDBFieldTypeDynArray;
  Null: TByteDynArray;
  tmp: TTextWriterStackBuffer;
begin
  result := 0;
  maxmem := Connection.Properties.StatementMaxMemory;
  W := TBufferWriter.Create(Dest, @tmp, SizeOf(tmp));
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
        if (ft = ftUnknown) and
           (currentRow = 0) and
           Step then
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
      if (currentRow = 1) or
         Step then // Step may already be done (e.g. TQuery.Open)
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
          if (MaxRowCount > 0) and
             (result >= MaxRowCount) then
            break;
          if (maxmem > 0) and
             (W.TotalWritten > maxmem) then // Dest.Position is slower
            raise ESQLDBException.CreateUTF8('%.FetchAllToBinary: overflow %',
              [self, KB(maxmem)]);
        until not Step;
      ReleaseRows;
    end;
    W.Write(@result, SizeOf(result)); // fixed size at the end for row count
    W.Flush;
  finally
    W.Free;
  end;
end;

procedure TSqlDBStatement.Execute(const aSql: RawUtf8; ExpectResults: boolean;
  const Params: array of const);
begin
  Connection.InternalProcess(speActive);
  try
    Prepare(aSql, ExpectResults);
    Bind(Params);
    ExecutePrepared;
  finally
    Connection.InternalProcess(speNonActive);
  end;
end;

procedure TSqlDBStatement.Execute(const SqlFormat: RawUtf8;
  ExpectResults: boolean; const Args, Params: array of const);
begin
  Execute(FormatUtf8(SqlFormat, Args), ExpectResults, Params);
end;

function TSqlDBStatement.UpdateCount: integer;
begin
  result := 0;
end;

procedure TSqlDBStatement.ExecutePreparedAndFetchAllAsJson(Expanded: boolean;
  out Json: RawUtf8);
begin
  ExecutePrepared;
  Json := FetchAllAsJson(Expanded);
end;

function TSqlDBStatement.ColumnString(Col: integer): string;
begin
  Utf8ToStringVar(ColumnUtf8(Col), result);
end;

function TSqlDBStatement.ColumnString(const ColName: RawUtf8): string;
begin
  result := ColumnString(ColumnIndex(ColName));
end;

function TSqlDBStatement.ColumnBlob(const ColName: RawUtf8): RawByteString;
begin
  result := ColumnBlob(ColumnIndex(ColName));
end;

function TSqlDBStatement.ColumnBlobBytes(const ColName: RawUtf8): TBytes;
begin
  result := ColumnBlobBytes(ColumnIndex(ColName));
end;

procedure TSqlDBStatement.ColumnBlobToStream(
  const ColName: RawUtf8; Stream: TStream);
begin
  ColumnBlobToStream(ColumnIndex(ColName), Stream);
end;

procedure TSqlDBStatement.ColumnBlobFromStream(
  const ColName: RawUtf8; Stream: TStream);
begin
  ColumnBlobFromStream(ColumnIndex(ColName), Stream);
end;

function TSqlDBStatement.ColumnCurrency(const ColName: RawUtf8): currency;
begin
  result := ColumnCurrency(ColumnIndex(ColName));
end;

function TSqlDBStatement.ColumnDateTime(const ColName: RawUtf8): TDateTime;
begin
  result := ColumnDateTime(ColumnIndex(ColName));
end;

function TSqlDBStatement.ColumnDouble(const ColName: RawUtf8): double;
begin
  result := ColumnDouble(ColumnIndex(ColName));
end;

function TSqlDBStatement.ColumnInt(const ColName: RawUtf8): Int64;
begin
  result := ColumnInt(ColumnIndex(ColName));
end;

function TSqlDBStatement.ColumnUtf8(const ColName: RawUtf8): RawUtf8;
begin
  result := ColumnUtf8(ColumnIndex(ColName));
end;

function TSqlDBStatement.ColumnVariant(const ColName: RawUtf8): Variant;
begin
  ColumnToVariant(ColumnIndex(ColName), result);
end;

function TSqlDBStatement.GetColumnVariant(const ColName: RawUtf8): Variant;
begin
  ColumnToVariant(ColumnIndex(ColName), result);
end;

function TSqlDBStatement.ColumnCursor(const ColName: RawUtf8): ISqlDBRows;
begin
  result := ColumnCursor(ColumnIndex(ColName));
end;

function TSqlDBStatement.{%H-}ColumnCursor(Col: integer): ISqlDBRows;
begin
  raise ESqlDBException.CreateUtf8('% does not support CURSOR columns', [self]);
end;

function TSqlDBStatement.Instance: TSqlDBStatement;
begin
  result := Self;
end;

function TSqlDBStatement.SqlLogBegin(level: TSynLogInfo): TSynLog;
begin
  if level = sllDB then // prepare
    fSqlLogTimer.Start
  else
    fSqlLogTimer.Resume;
  {$ifdef SYNDB_SILENCE}
  result := nil;
  {$else SYNDB_SILENCE}
  result := SynDBLog.Add;
  if result <> nil then
    if level in result.Family.Level then
    begin
      fSqlLogLevel := level;
      if level = sllSQL then
        ComputeSqlWithInlinedParams;
    end
    else
      result := nil; // fSqlLogLog=nil if this level is disabled
  fSqlLogLog := result;
  {$endif SYNDB_SILENCE}
end;

function TSqlDBStatement.SqlLogEnd(msg: PShortString): Int64;
{$ifndef SYNDB_SILENCE}
var
  tmp: TShort16;
{$endif SYNDB_SILENCE}
begin
  fSqlLogTimer.Pause;
  {$ifdef SYNDB_SILENCE}
  result := fSqlLogTimer.LastTimeInMicroSec;
  {$else}
  result := 0;
  if fSqlLogLog = nil then // fSqlLogLog=nil if this level is disabled
    exit;
  tmp[0] := #0;
  if fSqlLogLevel = sllSQL then
  begin
    if msg = nil then
    begin
      if not fExpectResults then
        FormatShort16(' wr=%', [UpdateCount], tmp);
      msg := @tmp;
    end;
    fSqlLogLog.Log(fSqlLogLevel, 'ExecutePrepared %% %',
      [fSqlLogTimer.Time, msg^, fSqlWithInlinedParams], self)
  end
  else
  begin
    if msg = nil then
      msg := @tmp;
    fSqlLogLog.Log(fSqlLogLevel, 'Prepare %% %',
      [fSqlLogTimer.Stop, msg^, fSql], self);
  end;
  result := fSqlLogTimer.LastTimeInMicroSec;
  fSqlLogLog := nil;
  {$endif SYNDB_SILENCE}
end;

function TSqlDBStatement.SqlLogEnd(const Fmt: RawUtf8;
  const Args: array of const): Int64;
var
  tmp: shortstring;
begin
  tmp[0] := #0;
  {$ifndef SYNDB_SILENCE}
  result := 0;
  if fSqlLogLog = nil then
    exit;
  if Fmt <> '' then
    FormatShort(Fmt, Args, tmp);
  {$endif SYNDB_SILENCE}
  result := SqlLogEnd(@tmp);
end;

function TSqlDBStatement.GetSqlCurrent: RawUtf8;
begin
  if fSqlPrepared <> '' then
    result := fSqlPrepared
  else
    result := fSql;
end;

function TSqlDBStatement.GetSqlWithInlinedParams: RawUtf8;
begin
  if fSql = '' then
    result := ''
  else
  begin
    if fSqlWithInlinedParams = '' then
      ComputeSqlWithInlinedParams;
    result := fSqlWithInlinedParams;
  end;
end;

function GotoNextParam(P: PUtf8Char): PUtf8Char;
  {$ifdef HASINLINE}inline;{$endif}
var
  c: AnsiChar;
begin
  repeat
    c := P^;
    if (c = #0) or
       (c = '?') then
      break;
    if (c = '''') and
       (P[1] <> '''') then
    begin
      repeat // ignore ? inside ' quotes
        inc(P);
        c := P^;
      until (c = #0) or
            ((c = '''') and
             (P[1] <> ''''));
      if c = #0 then
        break;
    end;
    inc(P);
  until false;
  result := P;
end;

procedure TSqlDBStatement.ComputeSqlWithInlinedParams;
var
  P, B: PUtf8Char;
  num: integer;
  maxSize, maxAllowed: cardinal;
  W: TTextWriter;
  tmp: TTextWriterStackBuffer;
begin
  fSqlWithInlinedParams := fSql;
  if fConnection = nil then
    maxSize := 0
  else
    maxSize := fConnection.fProperties.fLoggedSqlMaxSize;
  if (integer(maxSize) < 0) or
     (PosExChar('?', fSql) = 0) then
    // maxsize=-1 -> log statement without any parameter value (just ?)
    exit;
  P := pointer(fSql);
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
      W.AddNoJsonEscape(B, P - B);
      if P^ = #0 then
        break;
      inc(P); // jump P^='?'
      if maxSize > 0 then
        maxAllowed := W.TextLength - maxSize
      else
        maxAllowed := maxInt;
      AddParamValueAsText(num, W, maxAllowed);
      inc(num);
    until (P^ = #0) or
          ((maxSize > 0) and
           (W.TextLength >= maxSize));
    W.SetText(fSqlWithInlinedParams);
  finally
    W.Free;
  end;
end;

procedure TSqlDBStatement.AddParamValueAsText(Param: integer; Dest: TTextWriter;
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
      Dest.AddQuotedStr(tmp.buf, tmp.Len, '''', MaxCharCount);
    finally
      tmp.Done;
    end;
  end;

var
  v: variant;
  ft: TSqlDBFieldType;
begin
  ft := ParamToVariant(Param, v, false);
  with TVarData(v) do
    case cardinal(VType) of
      varString:
        if ft = ftBlob then
          Dest.AddU(length(RawByteString(VString)))
        else
          Dest.AddQuotedStr(
            VString, length(RawByteString(VString)), '''', MaxCharCount);
      varOleStr:
        AppendUnicode(VString, length(WideString(VString)));
      {$ifdef HASVARUSTRING}
      varUString:
        AppendUnicode(VString, length(UnicodeString(VString)));
      {$endif HASVARUSTRING}
    else
      if (ft = ftDate) and
         (cardinal(VType) in [varDouble, varDate]) then
        Dest.AddDateTime(vdate)
      else
        Dest.AddVariant(v);
    end;
end;

var
  SqlDBRowVariantType: TCustomVariantType = nil;

function TSqlDBStatement.RowData: Variant;
begin
  if SqlDBRowVariantType = nil then
    SqlDBRowVariantType := SynRegisterCustomVariantType(TSqlDBRowVariantType);
  VarClear(result);
  with TVarData(result) do
  begin
    VType := SqlDBRowVariantType.VarType;
    VPointer := self;
  end;
end;

procedure TSqlDBStatement.RowDocVariant(out aDocument: variant;
  aOptions: TDocVariantOptions);
var
  n, F: integer;
  names: TRawUtf8DynArray;
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

procedure TSqlDBStatement.Prepare(const aSql: RawUtf8; ExpectResults: boolean);
var
  L: integer;
begin
  Connection.InternalProcess(speActive);
  try
    L := length(aSql);
    if StripSemicolon then
      if (L > 5) and
         (aSql[L] = ';') and // avoid syntax error for some drivers
         not IdemPChar(@aSql[L - 4], ' END') then
        fSql := copy(aSql, 1, L - 1)
      else
        fSql := aSql
    else
      fSql := aSql;
    fExpectResults := ExpectResults;
    if (fConnection <> nil) and
       not fConnection.IsConnected then
      fConnection.Connect;
  finally
    Connection.InternalProcess(speNonActive);
  end;
end;

procedure TSqlDBStatement.ExecutePrepared;
begin
  if fConnection <> nil then
    fConnection.fLastAccessTicks := GetTickCount64;
  // a do-nothing default method
end;

procedure TSqlDBStatement.Reset;
begin
  fSqlWithInlinedParams := '';
  fSqlLogTimer.Init; // reset timer (for cached statement for example)
end;

procedure TSqlDBStatement.ReleaseRows;
begin
  fSqlWithInlinedParams := '';
end;

function TSqlDBStatement.ColumnsToSqlInsert(const TableName: RawUtf8;
  var Fields: TSqlDBColumnCreateDynArray): RawUtf8;
var
  F, size: integer;
begin
  result := '';
  if (self = nil) or
     (TableName = '') then
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
        raise ESqlDBException.CreateUtf8(
          '%.ColumnsToSqlInsert: Invalid column %',
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

procedure TSqlDBStatement.BindFromRows(const Fields: TSqlDBFieldTypeDynArray;
  Rows: TSqlDBStatement);
var
  F: integer;
begin
  if (self <> nil) and
     (Fields <> nil) and
     (Rows <> nil) then
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
          ftUtf8:
            BindTextU(F + 1, Rows.ColumnUtf8(F));
          ftBlob:
            BindBlob(F + 1, Rows.ColumnBlob(F));
        end;
end;

procedure TSqlDBStatement.BindCursor(Param: integer);
begin
  raise ESqlDBException.CreateUtf8('% does not support CURSOR parameter', [self]);
end;

function TSqlDBStatement.{%H-}BoundCursor(Param: integer): ISqlDBRows;
begin
  raise ESqlDBException.CreateUtf8('% does not support CURSOR parameter', [self]);
end;


{ TSqlDBConnection }

procedure TSqlDBConnection.CheckConnection;
begin
  if self = nil then
    raise ESqlDBException.Create('CheckConnection: TSqlDBConnection=nil');
  if not Connected then
    raise ESqlDBException.CreateUtf8('% on %/% should be connected',
      [self, Properties.ServerName, Properties.DataBaseName]);
end;

procedure TSqlDBConnection.InternalProcess(Event: TOnSqlDBProcessEvent);
begin
  if (self = nil) or
     not Assigned(OnProcess) then
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

procedure TSqlDBConnection.Commit;
begin
  CheckConnection;
  if TransactionCount <= 0 then
    raise ESqlDBException.CreateUtf8('Invalid %.Commit call', [self]);
  dec(fTransactionCount);
  InternalProcess(speCommit);
end;

constructor TSqlDBConnection.Create(aProperties: TSqlDBConnectionProperties);
begin
  fProperties := aProperties;
  if aProperties <> nil then
  begin
    fOnProcess := aProperties.OnProcess;
    fRollbackOnDisconnect := aProperties.RollbackOnDisconnect;
  end;
end;

procedure TSqlDBConnection.Connect;
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

procedure TSqlDBConnection.Disconnect;
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
          TSqlDBStatement(Obj[i]).FRefCount := 0; // force clean release
      FreeAndNilSafe(fCache); // release all cached statements
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

destructor TSqlDBConnection.Destroy;
begin
  try
    Disconnect;
  except
    on E: Exception do
      SynDBLog.Add.Log(sllError, E);
  end;
  inherited;
end;

function TSqlDBConnection.IsOutdated(tix: Int64): boolean;
label
  ok;
begin
  if (self = nil) or
     (fProperties.fConnectionTimeOutTicks = 0) then
    result := false
  else if fLastAccessTicks < 0 then
    // connection release was forced by ClearConnectionPool
    goto ok
  else if (fLastAccessTicks = 0) or
          (tix - fLastAccessTicks < fProperties.fConnectionTimeOutTicks) then
  begin
    // brand new connection, or active enough connection
    fLastAccessTicks := tix;
    result := false;
  end
  else
    // notify connection is clearly outdated
ok: result := true;
end;

function TSqlDBConnection.GetInTransaction: boolean;
begin
  result := TransactionCount > 0;
end;

function TSqlDBConnection.GetServerTimestamp: TTimeLog;
begin
  PTimeLogBits(@result)^.From(GetServerDateTime);
end;

function TSqlDBConnection.GetServerDateTime: TDateTime;
var
  Current: TDateTime;
begin
  Current := NowUtc; // so won't conflict with any potential time zone change
  if (fServerTimestampOffset = 0) and
     (fProperties.fSqlGetServerTimestamp <> '') then
  begin
    with fProperties do
      with Execute(fSqlGetServerTimestamp, []) do
      begin
        if Step then
          fServerTimestampOffset := ColumnDateTime(0) - Current;
        ReleaseRows;
      end;
    if fServerTimestampOffset = 0 then
      fServerTimestampOffset := 0.000001; // request server only once
  end;
  result := Current + fServerTimestampOffset;
end;

function TSqlDBConnection.GetLastErrorWasAboutConnection: boolean;
begin
  result := (self <> nil) and
            (Properties <> nil) and
            (fErrorMessage <> '') and
    Properties.ExceptionIsAboutConnection(fErrorException, fErrorMessage);
end;

function TSqlDBConnection.NewStatementPrepared(const aSql: RawUtf8;
  ExpectResults: boolean; RaiseExceptionOnError: boolean;
  AllowReconnect: boolean): ISqlDBStatement;
var
  Stmt: TSqlDBStatement;
  ToCache: boolean;
  ndx, altern: integer;
  cachedSql: RawUtf8;

  procedure TryPrepare(doraise: boolean);
  var
    Stmt: TSqlDBStatement;
  begin
    Stmt := nil;
    try
      InternalProcess(speActive);
      try
        Stmt := NewStatement;
        Stmt.Prepare(aSql, ExpectResults);
        if ToCache then
        begin
          if fCache = nil then
            fCache := TRawUtf8List.CreateEx(
              [fObjectsOwned, fNoDuplicate, fCaseSensitive]);
          if fCache.AddObject(cachedSql, Stmt) >= 0 then
            Stmt._AddRef
          else // will be owned by fCache.Objects[]
            SynDBLog.Add.Log(sllWarning, 'NewStatementPrepared: unexpected ' +
              'cache duplicate for %', [Stmt.SqlWithInlinedParams], self);
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
            LogLines(sllSQL, pointer(Stmt.SqlWithInlinedParams), self, '--');
        {$endif SYNDB_SILENCE}
        Stmt.Free;
        result := nil;
        StringToUtf8(E.Message, fErrorMessage);
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
  if length(aSql) < 5 then
    exit;
  // first check if could be retrieved from cache
  cachedSql := aSql;
  ToCache := fProperties.IsCachable(Pointer(aSql));
  if ToCache and
     (fCache <> nil) then
  begin
    ndx := fCache.IndexOf(cachedSql);
    if ndx >= 0 then
    begin
      Stmt := fCache.Objects[ndx];
      if Stmt.RefCount = 1 then
      begin
        // ensure statement is not currently in use
        result := Stmt; // acquire the statement
        Stmt.Reset;
        exit;
      end
      else
      begin
        // in use -> create cached alternatives
        ToCache := false; // if all slots are used, won't cache this statement
        if fProperties.StatementCacheReplicates = 0 then
          SynDBLog.Add.Log(sllWarning,
            'NewStatementPrepared: cached statement still in use ' +
            '-> you should release ISqlDBStatement ASAP [%]', [cachedSql], self)
        else
          for altern := 1 to fProperties.StatementCacheReplicates do
          begin
            cachedSql := aSql + RawUtf8(AnsiChar(altern)); // safe SQL duplicate
            ndx := fCache.IndexOf(cachedSql);
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
    else if RaiseExceptionOnError and
            (fErrorException <> nil) then
      // propagate error not related to connection (e.g. SQL syntax error)
      raise fErrorException.Create(Utf8ToString(fErrorMessage));
  end
  else
    // regular preparation, with no connection error interception
    TryPrepare(RaiseExceptionOnError);
end;

procedure TSqlDBConnection.Rollback;
begin
  CheckConnection;
  if TransactionCount <= 0 then
    raise ESqlDBException.CreateUtf8('Invalid %.Rollback call', [self]);
  dec(fTransactionCount);
  InternalProcess(speRollback);
end;

function TSqlDBConnection.PasswordChange: boolean;
begin
  result := false;
end;

procedure TSqlDBConnection.StartTransaction;
begin
  CheckConnection;
  inc(fTransactionCount);
  InternalProcess(speStartTransaction);
end;

function TSqlDBConnection.NewTableFromRows(const TableName: RawUtf8;
  Rows: TSqlDBStatement; WithinTransaction: boolean;
  ColumnForcedTypes: TSqlDBFieldTypeDynArray): integer;
var
  Fields: TSqlDBColumnCreateDynArray;
  aTableName, sql: RawUtf8;
  Tables: TRawUtf8DynArray;
  Ins: TSqlDBStatement;
  i, n: integer;
begin
  result := 0;
  if (self = nil) or
     (Rows = nil) or
     (Rows.ColumnCount = 0) then
    exit;
  aTableName := Properties.SqlTableName(TableName);
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
          sql := Rows.ColumnsToSqlInsert(aTableName, Fields);
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
          if FindRawUtf8(Tables, TableName, false) < 0 then
            with Properties do
              ExecuteNoResult(SqlCreate(aTableName, Fields, false), []);
          Ins := NewStatement;
          Ins.Prepare(sql, false);
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

{ TSqlDBConnectionPropertiesThreadSafe }

procedure TSqlDBConnectionPropertiesThreadSafe.ClearConnectionPool;
var
  i: PtrInt;
begin
  fConnectionPool.Safe.Lock;
  try
    if fMainConnection <> nil then
      fMainConnection.fLastAccessTicks := -1; // force IsOutdated to return true
    for i := 0 to fConnectionPool.Count - 1 do
      TSqlDBConnectionThreadSafe(fConnectionPool.List[i]).fLastAccessTicks := -1;
    fLatestConnectionRetrievedInPool := -1;
  finally
    fConnectionPool.Safe.UnLock;
  end;
end;

constructor TSqlDBConnectionPropertiesThreadSafe.Create(const aServerName,
  aDatabaseName, aUserID, aPassWord: RawUtf8);
begin
  fConnectionPool := TSynObjectListLocked.Create;
  fLatestConnectionRetrievedInPool := -1;
  inherited Create(aServerName, aDatabaseName, aUserID, aPassWord);
end;

function TSqlDBConnectionPropertiesThreadSafe.CurrentThreadConnectionIndex: PtrInt;
var
  id: TThreadID;
  tix: Int64;
  conn: TSqlDBConnectionThreadSafe;
begin
  // caller made fConnectionPool.Safe.Lock
  if self <> nil then
  begin
    id := GetCurrentThreadId;
    tix := GetTickCount64;
    result := fLatestConnectionRetrievedInPool;
    if result >= 0 then
    begin
      conn := fConnectionPool.List[result];
      if (conn.fThreadID = id) and
         not conn.IsOutdated(tix) then
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

destructor TSqlDBConnectionPropertiesThreadSafe.Destroy;
begin
  inherited Destroy; // do MainConnection.Free
  fConnectionPool.Free;
end;

procedure TSqlDBConnectionPropertiesThreadSafe.EndCurrentThread;
var
  i: integer;
begin
  fConnectionPool.Safe.Lock;
  try
    i := CurrentThreadConnectionIndex;
    if i >= 0 then
    begin
      // do nothing if this thread has no active connection
      fConnectionPool.Delete(i); // release thread's TSqlDBConnection instance
      if i = fLatestConnectionRetrievedInPool then
        fLatestConnectionRetrievedInPool := -1;
    end;
  finally
    fConnectionPool.Safe.UnLock;
  end;
end;

function TSqlDBConnectionPropertiesThreadSafe.GetMainConnection: TSqlDBConnection;
begin
  result := ThreadSafeConnection;
end;

function TSqlDBConnectionPropertiesThreadSafe.ThreadSafeConnection: TSqlDBConnection;
var
  i: PtrInt;
begin
  case fThreadingMode of
    tmThreadPool:
      begin
        fConnectionPool.Safe.Lock;
        {$ifdef HASFASTTRYFINALLY}
        try
        {$endif HASFASTTRYFINALLY}
          i := CurrentThreadConnectionIndex;
          if i >= 0 then
          begin
            result := fConnectionPool.List[i];
            {$ifndef HASFASTTRYFINALLY}
            fConnectionPool.Safe.UnLock;
            {$endif HASFASTTRYFINALLY}
            exit;
          end;
        {$ifndef HASFASTTRYFINALLY}
        try
        {$endif HASFASTTRYFINALLY}
          result := NewConnection; // no need to release the lock (fast method)
          (result as TSqlDBConnectionThreadSafe).fThreadID := GetCurrentThreadId;
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


{ TSqlDBStatementWithParams }

function TSqlDBStatementWithParams.CheckParam(Param: integer;
  NewType: TSqlDBFieldType; IO: TSqlDBParamInOutType): PSqlDBParam;
begin
  if self = nil then
    raise ESqlDBException.Create('self=nil for TSqlDBStatement.Bind*()');
  if Param > fParamCount then
    fParam.Count := Param; // resize fParams[] dynamic array if necessary
  result := @fParams[Param - 1];
  result^.VType := NewType;
  result^.VInOut := IO;
end;

function TSqlDBStatementWithParams.CheckParam(Param: integer;
  NewType: TSqlDBFieldType; IO: TSqlDBParamInOutType;
  ArrayCount: integer): PSqlDBParam;
begin
  result := CheckParam(Param, NewType, IO);
  if (NewType in [ftUnknown, ftNull]) or
     (fConnection = nil) or
     (fConnection.fProperties.BatchSendingAbilities *
       [cCreate, cUpdate, cDelete] = []) then
    raise ESqlDBException.CreateUtf8(
      'Invalid call to %.BindArray(Param=%,Type=%)',
      [self, Param, ToText(NewType)^]);
  SetLength(result^.VArray, ArrayCount);
  result^.VInt64 := ArrayCount;
  fParamsArrayCount := ArrayCount;
end;

constructor TSqlDBStatementWithParams.Create(aConnection: TSqlDBConnection);
begin
  inherited Create(aConnection);
  fParam.Init(TypeInfo(TSqlDBParamDynArray), fParams, @fParamCount);
end;

procedure TSqlDBStatementWithParams.Bind(Param: integer; Value: double;
  IO: TSqlDBParamInOutType);
begin
  CheckParam(Param, ftDouble, IO)^.VInt64 := PInt64(@Value)^;
end;

procedure TSqlDBStatementWithParams.Bind(Param: integer; Value: Int64;
  IO: TSqlDBParamInOutType);
begin
  CheckParam(Param, ftInt64, IO)^.VInt64 := Value;
end;

procedure TSqlDBStatementWithParams.BindBlob(Param: integer;
  const Data: RawByteString; IO: TSqlDBParamInOutType);
begin
  CheckParam(Param, ftBlob, IO)^.VData := Data;
end;

procedure TSqlDBStatementWithParams.BindBlob(Param: integer; Data: pointer;
  Size: integer; IO: TSqlDBParamInOutType);
begin
  SetString(CheckParam(Param, ftBlob, IO)^.VData, PAnsiChar(Data), Size);
end;

procedure TSqlDBStatementWithParams.BindCurrency(Param: integer;
  Value: currency; IO: TSqlDBParamInOutType);
begin
  CheckParam(Param, ftCurrency, IO)^.VInt64 := PInt64(@Value)^;
end;

procedure TSqlDBStatementWithParams.BindDateTime(Param: integer;
  Value: TDateTime; IO: TSqlDBParamInOutType);
begin
  CheckParam(Param, ftDate, IO)^.VInt64 := PInt64(@Value)^;
end;

procedure TSqlDBStatementWithParams.BindNull(Param: integer;
  IO: TSqlDBParamInOutType; BoundType: TSqlDBFieldType);
begin
  CheckParam(Param, ftNull, IO);
end;

procedure TSqlDBStatementWithParams.BindTextS(Param: integer;
  const Value: string; IO: TSqlDBParamInOutType);
begin
  if (Value = '') and
     (fConnection <> nil) and
     fConnection.fProperties.StoreVoidStringAsNull then
    CheckParam(Param, ftNull, IO)
  else
    CheckParam(Param, ftUtf8, IO)^.VData := StringToUtf8(Value);
end;

procedure TSqlDBStatementWithParams.BindTextU(Param: integer;
  const Value: RawUtf8; IO: TSqlDBParamInOutType);
begin
  if (Value = '') and
     (fConnection <> nil) and
     fConnection.fProperties.StoreVoidStringAsNull then
    CheckParam(Param, ftNull, IO)
  else
    CheckParam(Param, ftUtf8, IO)^.VData := Value;
end;

procedure TSqlDBStatementWithParams.BindTextP(Param: integer; Value: PUtf8Char;
  IO: TSqlDBParamInOutType);
begin
  if (Value = nil) and
     (fConnection <> nil) and
     fConnection.fProperties.StoreVoidStringAsNull then
    CheckParam(Param, ftNull, IO)
  else
    FastSetString(
      RawUtf8(CheckParam(Param, ftUtf8, IO)^.VData), Value, StrLen(Value));
end;

procedure TSqlDBStatementWithParams.BindTextW(Param: integer; const Value:
  WideString; IO: TSqlDBParamInOutType);
begin
  if (Value = '') and
     (fConnection <> nil) and
     fConnection.fProperties.StoreVoidStringAsNull then
    CheckParam(Param, ftNull, IO)
  else
    CheckParam(Param, ftUtf8, IO)^.VData := RawUnicodeToUtf8(pointer(Value),
      length(Value));
end;

function TSqlDBStatementWithParams.ParamToVariant(Param: integer;
  var Value: Variant; CheckIsOutParameter: boolean): TSqlDBFieldType;
begin
  inherited ParamToVariant(Param, Value); // raise exception if Param incorrect
  dec(Param); // start at #1
  if CheckIsOutParameter and
     (fParams[Param].VInOut = paramIn) then
    raise ESqlDBException.CreateUtf8(
      '%.ParamToVariant expects an [In]Out parameter', [self]);
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
        ftUtf8:
          RawUtf8ToVariant(RawUtf8(VData), Value);
        ftBlob:
          RawByteStringToVariant(VData, Value);
      else
        SetVariantNull(Value)
      end
    else
      SetVariantNull(Value);
  end;
end;

procedure TSqlDBStatementWithParams.AddParamValueAsText(Param: integer;
  Dest: TTextWriter; MaxCharCount: integer);
begin
  dec(Param);
  if cardinal(Param) >= cardinal(fParamCount) then
    Dest.AddNull
  else
    with fParams[Param] do
      if VArray = nil then
        case VType of
          ftInt64:
            Dest.Add(VInt64);
          ftDouble:
            Dest.AddDouble(unaligned(PDouble(@VInt64)^));
          ftCurrency:
            Dest.AddCurr64(@VInt64);
          ftDate:
            Dest.AddDateTime(PDateTime(@VInt64), ' ', '''');
          ftUtf8:
            Dest.AddQuotedStr(pointer(VData), length(RawUtf8(VData)), '''',
              MaxCharCount);
          ftBlob:
            Dest.AddU(length(VData));
        else
          Dest.AddNull;
        end
      else
        Dest.AddString(VArray[0]); // first item is enough in the logs
end;

procedure TSqlDBStatementWithParams.BindArray(Param: integer;
  const Values: array of double);
var
  i: PtrInt;
begin
  with CheckParam(Param, ftDouble, paramIn, length(Values))^ do
    for i := 0 to high(Values) do
      VArray[i] := DoubleToStr(Values[i]);
end;

procedure TSqlDBStatementWithParams.BindArray(Param: integer;
  const Values: array of Int64);
var
  i: PtrInt;
begin
  with CheckParam(Param, ftInt64, paramIn, length(Values))^ do
    for i := 0 to high(Values) do
      VArray[i] := Int64ToUtf8(Values[i]);
end;

procedure TSqlDBStatementWithParams.BindArray(Param: integer;
  ParamType: TSqlDBFieldType; const Values: TRawUtf8DynArray; ValuesCount: integer);
var
  i: PtrInt;
  ChangeFirstChar: AnsiChar;
  p: PSqlDBParam;
begin
  inherited; // raise an exception in case of invalid parameter
  if fConnection = nil then
    ChangeFirstChar := 'T'
  else
    ChangeFirstChar := Connection.Properties.DateTimeFirstChar;
  p := CheckParam(Param, ParamType, paramIn);
  p^.VInt64 := ValuesCount;
  p^.VArray := Values; // immediate COW reference-counted assignment
  if (ParamType = ftDate) and
     (ChangeFirstChar <> 'T') then
    for i := 0 to ValuesCount - 1 do // fix e.g. for PostgreSQL
      if (p^.VArray[i] <> '') and
         (p^.VArray[i][1] = '''') then
        // not only replace 'T'->ChangeFirstChar, but force expanded format
        DateTimeToIso8601(Iso8601ToDateTime(p^.VArray[i]),
          {expanded=}true, ChangeFirstChar, {ms=}fForceDateWithMS, '''');
  fParamsArrayCount := ValuesCount;
end;

procedure TSqlDBStatementWithParams.BindArray(Param: integer;
  const Values: array of RawUtf8);
var
  i: PtrInt;
  StoreVoidStringAsNull: boolean;
begin
  StoreVoidStringAsNull := (fConnection <> nil) and
    fConnection.Properties.StoreVoidStringAsNull;
  with CheckParam(Param, ftUtf8, paramIn, length(Values))^ do
    for i := 0 to high(Values) do
      if StoreVoidStringAsNull and
         (Values[i] = '') then
        VArray[i] := 'null'
      else
        QuotedStr(Values[i], '''', VArray[i]);
end;

procedure TSqlDBStatementWithParams.BindArrayCurrency(Param: integer;
  const Values: array of currency);
var
  i: PtrInt;
begin
  with CheckParam(Param, ftCurrency, paramIn, length(Values))^ do
    for i := 0 to high(Values) do
      VArray[i] := Curr64ToStr(PInt64(@Values[i])^);
end;

procedure TSqlDBStatementWithParams.BindArrayDateTime(Param: integer;
  const Values: array of TDateTime);
var
  i: PtrInt;
begin
  with CheckParam(Param, ftDate, paramIn, length(Values))^ do
    for i := 0 to high(Values) do
      VArray[i] := Connection.Properties.SqlDateToIso8601Quoted(Values[i]);
end;

procedure TSqlDBStatementWithParams.BindArrayRowPrepare(
  const aParamTypes: array of TSqlDBFieldType; aExpectedMinimalRowCount: integer);
var
  i: PtrInt;
begin
  fParam.Count := 0;
  for i := 0 to high(aParamTypes) do
    CheckParam(i + 1, aParamTypes[i], paramIn, aExpectedMinimalRowCount);
  fParamsArrayCount := 0;
end;

procedure TSqlDBStatementWithParams.BindArrayRow(const aValues: array of const);
var
  i: PtrInt;
begin
  if length(aValues) <> fParamCount then
    raise ESqlDBException.CreateFmt('Invalid %.BindArrayRow call', [self]);
  for i := 0 to high(aValues) do
    with fParams[i] do
    begin
      if length(VArray) <= fParamsArrayCount then
        SetLength(VArray, NextGrow(fParamsArrayCount));
      VInt64 := fParamsArrayCount;
      if (VType = ftDate) and
         (aValues[i].VType = vtExtended) then
        VArray[fParamsArrayCount] := // direct binding of TDateTime value
          Connection.Properties.SqlDateToIso8601Quoted(aValues[i].VExtended^)
      else
      begin
        VarRecToUtf8(aValues[i], VArray[fParamsArrayCount]);
        case VType of
          ftUtf8:
            if (VArray[fParamsArrayCount] = '') and
               (fConnection <> nil) and
              fConnection.Properties.StoreVoidStringAsNull then
              VArray[fParamsArrayCount] := 'null'
            else
              QuotedStr(VArray[fParamsArrayCount], '''', VArray[fParamsArrayCount]);
          ftDate:
            QuotedStr(VArray[fParamsArrayCount], '''', VArray[fParamsArrayCount]);
        end;
      end;
    end;
  inc(fParamsArrayCount);
end;

procedure TSqlDBStatementWithParams.BindFromRows(Rows: TSqlDBStatement);
var
  F: PtrInt;
  U: RawUtf8;
begin
  if Rows <> nil then
    if Rows.ColumnCount <> fParamCount then
      raise ESqlDBException.CreateUtf8('Invalid %.BindFromRows call', [self])
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
                  '''' + DateTimeToSql(Rows.ColumnDateTime (F)) + '''';
              ftUtf8:
                begin
                  U := Rows.ColumnUtf8(F);
                  if (U = '') and
                     (fConnection <> nil) and
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

procedure TSqlDBStatementWithParams.Reset;
begin
  fParam.Clear;
  fParamsArrayCount := 0;
  inherited Reset;
end;

procedure TSqlDBStatementWithParams.ReleaseRows;
var
  i: PtrInt;
  p: PSqlDBParam;
begin
  p := pointer(fParams);
  if p <> nil then
    for i := 1 to fParamCount do
    begin
      if p^.VData <> '' then
        p^.VData := ''; // release bound value, but keep fParams[] reusable
      if p^.VArray <> nil then
        RawUtf8DynArrayClear(p^.VArray);
      inc(p);
    end;
  inherited ReleaseRows;
end;


{ TSqlDBStatementWithParamsAndColumns }

constructor TSqlDBStatementWithParamsAndColumns.Create(
  aConnection: TSqlDBConnection);
begin
  inherited Create(aConnection);
  fColumn.InitSpecific(TypeInfo(TSqlDBColumnPropertyDynArray),
    fColumns, ptRawUtf8, @fColumnCount, {caseinsens=}true);
end;

function TSqlDBStatementWithParamsAndColumns.ColumnIndex(
  const aColumnName: RawUtf8): integer;
begin
  result := fColumn.FindHashed(aColumnName);
end;

function TSqlDBStatementWithParamsAndColumns.ColumnName(Col: integer): RawUtf8;
begin
  CheckCol(Col);
  result := fColumns[Col].ColumnName;
end;

function TSqlDBStatementWithParamsAndColumns.ColumnType(
  Col: integer; FieldSize: PInteger): TSqlDBFieldType;
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
  __TSqlDBColumnDefine = 'ColumnName,ColumnTypeNative RawUtf8 ' +
    'ColumnLength,ColumnPrecision,ColumnScale PtrInt ' +
    'ColumnType TSqlDBFieldType ColumnIndexed boolean';

initialization
  assert(SizeOf(TSqlDBColumnProperty) = sizeof(PtrUInt) * 2 + 20);
  Rtti.RegisterType(TypeInfo(TSqlDBFieldType));
  Rtti.RegisterFromText(TypeInfo(TSqlDBColumnDefine), __TSqlDBColumnDefine);

end.

