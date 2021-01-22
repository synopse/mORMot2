/// Database Framework Low-Level Oracle Client Interface Access
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.raw.oracle;

{
  *****************************************************************************

   Efficient Direct OCI API Access
    - Native OCI Constants
    - Oracle Date/Time Process
    - OCI Library Loading
    - Some Global Types and Variables

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


{ ************ Native OCI Constants }

type
  { Generic Oracle Types }
  sword   = integer;
  eword   = integer;
  uword   = cardinal;
  sb4     = integer;
  ub4     = cardinal;
  sb2     = SmallInt;
  ub2     = Word;
  sb1     = ShortInt;
  ub1     = byte;
  dvoid   = Pointer;
  text    = PAnsiChar; // this conflicts with the standard text definition in FPC (and Delphi perhaps)
  OraText = PAnsiChar;
  size_T  = PtrUInt;

  pub1 = ^ub1;
  psb1 = ^sb1;
  pub2 = ^ub2;
  psb2 = ^sb2;
  pub4 = ^ub4;
  psb4 = ^sb4;
  pdvoid = ^dvoid;

  { Handle Types }
  POCIHandle = Pointer;
  PPOCIHandle = ^Pointer;
  POCIEnv = POCIHandle;
  POCIServer = POCIHandle;
  POCIError = POCIHandle;
  POCISvcCtx = POCIHandle;
  POCIStmt = POCIHandle;
  POCIDefine = POCIHandle;
  POCISession = POCIHandle;
  POCIBind = POCIHandle;
  POCIDescribe = POCIHandle;
  POCITrans = POCIHandle;

  { Descriptor Types }
  POCIDescriptor = Pointer;
  PPOCIDescriptor = ^POCIDescriptor;
  POCISnapshot = POCIDescriptor;
  POCILobLocator = POCIDescriptor;
  POCIParam = POCIDescriptor;
  POCIRowid = POCIDescriptor;
  POCIComplexObjectComp = POCIDescriptor;
  POCIAQEnqOptions = POCIDescriptor;
  POCIAQDeqOptions = POCIDescriptor;
  POCIAQMsgProperties = POCIDescriptor;
  POCIAQAgent = POCIDescriptor;
  POCIDate = POCIDescriptor;
  POCIDateTime = POCIDescriptor;
  POCIString = POCIDescriptor;
  POCIType = POCIDescriptor;
  POCIArray = POCIDescriptor;
  POCIColl = POCIDescriptor;

  /// OCIDuration - OCI object duration
  // - A client can specify the duration of which an object is pinned (pin
  // duration) and the duration of which the object is in memory (allocation
  // duration).  If the objects are still pinned at the end of the pin duration,
  // the object cache manager will automatically unpin the objects for the
  // client. If the objects still exist at the end of the allocation duration,
  // the object cache manager will automatically free the objects for the client.
  // - Objects that are pinned with the option OCI_DURATION_TRANS will get unpinned
  // automatically at the end of the current transaction.
  // - Objects that are pinned with the option OCI_DURATION_SESSION will get
  // unpinned automatically at the end of the current session (connection).
  // - The option OCI_DURATION_NULL is used when the client does not want to set
  // the pin duration.  If the object is already loaded into the cache, then the
  // pin duration will remain the same.  If the object is not yet loaded, the
  // pin duration of the object will be set to OCI_DURATION_DEFAULT.
  OCIDuration = ub2;
  /// The OCITypeCode type is interchangeable with the existing SQLT type which is a ub2
  OCITypeCode = ub2;

const
  { OCI Handle Types }
  OCI_HTYPE_FIRST               = 1;
  OCI_HTYPE_ENV                 = 1;
  OCI_HTYPE_ERROR               = 2;
  OCI_HTYPE_SVCCTX              = 3;
  OCI_HTYPE_STMT                = 4;
  OCI_HTYPE_BIND                = 5;
  OCI_HTYPE_DEFINE              = 6;
  OCI_HTYPE_DESCRIBE            = 7;
  OCI_HTYPE_SERVER              = 8;
  OCI_HTYPE_SESSION             = 9;
  OCI_HTYPE_TRANS               = 10;
  OCI_HTYPE_COMPLEXOBJECT       = 11;
  OCI_HTYPE_SECURITY            = 12;
  OCI_HTYPE_SUBSCRIPTION        = 13;
  OCI_HTYPE_DIRPATH_CTX         = 14;
  OCI_HTYPE_DIRPATH_COLUMN_ARRAY = 15;
  OCI_HTYPE_DIRPATH_STREAM      = 16;
  OCI_HTYPE_PROC                = 17;
  OCI_HTYPE_LAST                = 17;

  { OCI Descriptor Types }
  OCI_DTYPE_FIRST               = 50;
  OCI_DTYPE_LOB                 = 50;
  OCI_DTYPE_SNAP                = 51;
  OCI_DTYPE_RSET                = 52;
  OCI_DTYPE_PARAM               = 53;
  OCI_DTYPE_ROWID               = 54;
  OCI_DTYPE_COMPLEXOBJECTCOMP   = 55;
  OCI_DTYPE_FILE                = 56;
  OCI_DTYPE_AQENQ_OPTIONS       = 57;
  OCI_DTYPE_AQDEQ_OPTIONS       = 58;
  OCI_DTYPE_AQMSG_PROPERTIES    = 59;
  OCI_DTYPE_AQAGENT             = 60;
  OCI_DTYPE_LOCATOR             = 61;
  OCI_DTYPE_DATETIME            = 62;
  OCI_DTYPE_INTERVAL            = 63;
  OCI_DTYPE_AQNFY_DESCRIPTOR    = 64;
  OCI_DTYPE_LAST                = 64;
  OCI_DTYPE_DATE                = 65;  { Date }
  OCI_DTYPE_TIME                = 66;  { Time }
  OCI_DTYPE_TIME_TZ             = 67;  { Time with timezone }
  OCI_DTYPE_TIMESTAMP           = 68;  { Timestamp }
  OCI_DTYPE_TIMESTAMP_TZ        = 69;  { Timestamp with timezone }
  OCI_DTYPE_TIMESTAMP_LTZ       = 70;  { Timestamp with local tz }

  { OCI Attributes Types }
  OCI_ATTR_FNCODE               = 1;   // the OCI function code
  OCI_ATTR_OBJECT               = 2;   // is the environment initialized in object mode
  OCI_ATTR_NONBLOCKING_MODE     = 3;   // non blocking mode
  OCI_ATTR_SQLCODE              = 4;   // the SQL verb
  OCI_ATTR_ENV                  = 5;   // the environment handle
  OCI_ATTR_SERVER               = 6;   // the server handle
  OCI_ATTR_SESSION              = 7;   // the user session handle
  OCI_ATTR_TRANS                = 8;   // the transaction handle
  OCI_ATTR_ROW_COUNT            = 9;   // the rows processed so far
  OCI_ATTR_SQLFNCODE            = 10;  // the SQL verb of the statement
  OCI_ATTR_PREFETCH_ROWS        = 11;  // sets the number of rows to prefetch
  OCI_ATTR_NESTED_PREFETCH_ROWS = 12;  // the prefetch rows of nested table
  OCI_ATTR_PREFETCH_MEMORY      = 13;  // memory limit for rows fetched
  OCI_ATTR_NESTED_PREFETCH_MEMORY = 14;// memory limit for nested rows
  OCI_ATTR_CHAR_COUNT           = 15;  // this specifies the bind and define size in characters
  OCI_ATTR_PDSCL                = 16;  // packed decimal scale
  OCI_ATTR_FSPRECISION          = OCI_ATTR_PDSCL; // fs prec for datetime data types
  OCI_ATTR_PDPRC                = 17;  // packed decimal format
  OCI_ATTR_LFPRECISION          = OCI_ATTR_PDPRC; // fs prec for datetime data types
  OCI_ATTR_PARAM_COUNT          = 18;  // number of column in the select list
  OCI_ATTR_ROWID                = 19;  // the rowid
  OCI_ATTR_CHARSET              = 20;  // the character set value
  OCI_ATTR_NCHAR                = 21;  // NCHAR type
  OCI_ATTR_USERNAME             = 22;  // username attribute
  OCI_ATTR_PASSWORD             = 23;  // password attribute
  OCI_ATTR_STMT_TYPE            = 24;  // statement type
  OCI_ATTR_INTERNAL_NAME        = 25;  // user friendly global name
  OCI_ATTR_EXTERNAL_NAME        = 26;  // the internal name for global txn
  OCI_ATTR_XID                  = 27;  // XOPEN defined global transaction id
  OCI_ATTR_TRANS_LOCK           = 28;  //
  OCI_ATTR_TRANS_NAME           = 29;  // string to identify a global transaction
  OCI_ATTR_HEAPALLOC            = 30;  // memory allocated on the heap
  OCI_ATTR_CHARSET_ID           = 31;  // Character Set ID
  OCI_ATTR_CHARSET_FORM         = 32;  // Character Set Form
  OCI_ATTR_MAXDATA_SIZE         = 33;  // Maximumsize of data on the server
  OCI_ATTR_CACHE_OPT_SIZE       = 34;  // object cache optimal size
  OCI_ATTR_CACHE_MAX_SIZE       = 35;  // object cache maximum size percentage
  OCI_ATTR_PINOPTION            = 36;  // object cache default pin option
  OCI_ATTR_ALLOC_DURATION       = 37;  // object cache default allocation duration
  OCI_ATTR_PIN_DURATION         = 38;  // object cache default pin duration
  OCI_ATTR_FDO                  = 39;  // Format Descriptor object attribute
  OCI_ATTR_POSTPROCESSING_CALLBACK = 40;  // Callback to process outbind data
  OCI_ATTR_POSTPROCESSING_CONTEXT = 41; // Callback context to process outbind data
  OCI_ATTR_ROWS_RETURNED        = 42;  // Number of rows returned in current iter - for Bind handles
  OCI_ATTR_FOCBK                = 43;  // Failover Callback attribute
  OCI_ATTR_IN_V8_MODE           = 44;  // is the server/service context in V8 mode
  OCI_ATTR_LOBEMPTY             = 45;  // empty lob ?
  OCI_ATTR_SESSLANG             = 46;  // session language handle

  OCI_ATTR_VISIBILITY           = 47;  // visibility
  OCI_ATTR_RELATIVE_MSGID       = 48;  // relative message id
  OCI_ATTR_SEQUENCE_DEVIATION   = 49;  // sequence deviation

  OCI_ATTR_CONSUMER_NAME        = 50;  // consumer name
  OCI_ATTR_DEQ_MODE             = 51;  // dequeue mode
  OCI_ATTR_NAVIGATION           = 52;  // navigation
  OCI_ATTR_WAIT                 = 53;  // wait
  OCI_ATTR_DEQ_MSGID            = 54;  // dequeue message id

  OCI_ATTR_PRIORITY             = 55;  // priority
  OCI_ATTR_DELAY                = 56;  // delay
  OCI_ATTR_EXPIRATION           = 57;  // expiration
  OCI_ATTR_CORRELATION          = 58;  // correlation id
  OCI_ATTR_ATTEMPTS             = 59;  // # of attempts
  OCI_ATTR_RECIPIENT_LIST       = 60;  // recipient list
  OCI_ATTR_EXCEPTION_QUEUE      = 61;  // exception queue name
  OCI_ATTR_ENQ_TIME             = 62;  // enqueue time (only OCIAttrGet)
  OCI_ATTR_MSG_STATE            = 63;  // message state (only OCIAttrGet)
                                       // NOTE: 64-66 used below
  OCI_ATTR_AGENT_NAME           = 64;  // agent name
  OCI_ATTR_AGENT_ADDRESS        = 65;  // agent address
  OCI_ATTR_AGENT_PROTOCOL       = 66;  // agent protocol

  OCI_ATTR_SENDER_ID            = 68;  // sender id
  OCI_ATTR_ORIGINAL_MSGID       = 69;  // original message id

  OCI_ATTR_QUEUE_NAME           = 70;  // queue name
  OCI_ATTR_NFY_MSGID            = 71;  // message id
  OCI_ATTR_MSG_PROP             = 72;  // message properties

  OCI_ATTR_NUM_DML_ERRORS       = 73;  // num of errs in array DML
  OCI_ATTR_DML_ROW_OFFSET       = 74;  // row offset in the array

  OCI_ATTR_DATEFORMAT           = 75;  // default date format string
  OCI_ATTR_BUF_ADDR             = 76;  // buffer address
  OCI_ATTR_BUF_SIZE             = 77;  // buffer size
  OCI_ATTR_DIRPATH_MODE         = 78;  // mode of direct path operation
  OCI_ATTR_DIRPATH_NOLOG        = 79;  // nologging option
  OCI_ATTR_DIRPATH_PARALLEL     = 80;  // parallel (temp seg) option
  OCI_ATTR_NUM_ROWS             = 81;  // number of rows in column array
                                       // NOTE that OCI_ATTR_NUM_COLS is a column
                                       // array attribute too.

  OCI_ATTR_COL_COUNT            = 82;  // columns of column array processed so far.
  OCI_ATTR_STREAM_OFFSET        = 83;  // str off of last row processed
  OCI_ATTR_SHARED_HEAPALLOC     = 84;  // Shared Heap Allocation Size

  OCI_ATTR_SERVER_GROUP         = 85;  // server group name

  OCI_ATTR_MIGSESSION           = 86;  // migratable session attribute

  OCI_ATTR_NOCACHE              = 87;  // Temporary LOBs

  OCI_ATTR_MEMPOOL_SIZE         = 88;  // Pool Size
  OCI_ATTR_MEMPOOL_INSTNAME     = 89;  // Instance name
  OCI_ATTR_MEMPOOL_APPNAME      = 90;  // Application name
  OCI_ATTR_MEMPOOL_HOMENAME     = 91;  // Home Directory name
  OCI_ATTR_MEMPOOL_MODEL        = 92;  // Pool Model (proc,thrd,both)
  OCI_ATTR_MODES                = 93;  // Modes

  OCI_ATTR_SUBSCR_NAME          = 94;  // name of subscription
  OCI_ATTR_SUBSCR_CALLBACK      = 95;  // associated callback
  OCI_ATTR_SUBSCR_CTX           = 96;  // associated callback context
  OCI_ATTR_SUBSCR_PAYLOAD       = 97;  // associated payload
  OCI_ATTR_SUBSCR_NAMESPACE     = 98;  // associated namespace

  OCI_ATTR_PROXY_CREDENTIALS    = 99;  // Proxy user credentials
  OCI_ATTR_INITIAL_CLIENT_ROLES = 100; // Initial client role list

  OCI_ATTR_UNK                  = 101; // unknown attribute
  OCI_ATTR_NUM_COLS             = 102; // number of columns
  OCI_ATTR_LIST_COLUMNS         = 103; // parameter of the column list
  OCI_ATTR_RDBA                 = 104; // DBA of the segment header
  OCI_ATTR_CLUSTERED            = 105; // whether the table is clustered
  OCI_ATTR_PARTITIONED          = 106; // whether the table is partitioned
  OCI_ATTR_INDEX_ONLY           = 107; // whether the table is index only
  OCI_ATTR_LIST_ARGUMENTS       = 108; // parameter of the argument list
  OCI_ATTR_LIST_SUBPROGRAMS     = 109; // parameter of the subprogram list
  OCI_ATTR_REF_TDO              = 110; // REF to the type descriptor
  OCI_ATTR_LINK                 = 111; // the database link name
  OCI_ATTR_MIN                  = 112; // minimum value
  OCI_ATTR_MAX                  = 113; // maximum value
  OCI_ATTR_INCR                 = 114; // increment value
  OCI_ATTR_CACHE                = 115; // number of sequence numbers cached
  OCI_ATTR_ORDER                = 116; // whether the sequence is ordered
  OCI_ATTR_HW_MARK              = 117; // high-water mark
  OCI_ATTR_TYPE_SCHEMA          = 118; // type's schema name
  OCI_ATTR_TIMESTAMP            = 119; // timestamp of the object
  OCI_ATTR_NUM_ATTRS            = 120; // number of sttributes
  OCI_ATTR_NUM_PARAMS           = 121; // number of parameters
  OCI_ATTR_OBJID                = 122; // object id for a table or view
  OCI_ATTR_PTYPE                = 123; // type of info described by
  OCI_ATTR_PARAM                = 124; // parameter descriptor
  OCI_ATTR_OVERLOAD_ID          = 125; // overload ID for funcs and procs
  OCI_ATTR_TABLESPACE           = 126; // table name space
  OCI_ATTR_TDO                  = 127; // TDO of a type
  OCI_ATTR_LTYPE                = 128; // list type
  OCI_ATTR_PARSE_ERROR_OFFSET   = 129; // Parse Error offset
  OCI_ATTR_IS_TEMPORARY         = 130; // whether table is temporary
  OCI_ATTR_IS_TYPED             = 131; // whether table is typed
  OCI_ATTR_DURATION             = 132; // duration of temporary table
  OCI_ATTR_IS_INVOKER_RIGHTS    = 133; // is invoker rights
  OCI_ATTR_OBJ_NAME             = 134; // top level schema obj name
  OCI_ATTR_OBJ_SCHEMA           = 135; // schema name
  OCI_ATTR_OBJ_ID               = 136; // top level schema object id
  OCI_ATTR_STMTCACHESIZE        = 176; // size of the stm cache
  OCI_ATTR_ROWS_FETCHED         = 197; // rows fetched in last call
  OCI_ATTR_DEFAULT_LOBPREFETCH_SIZE = 438; // default prefetch size

  { OCI Error Return Values }
  OCI_SUCCESS             = 0;
  OCI_SUCCESS_WITH_INFO   = 1;
  OCI_NO_DATA             = 100;
  OCI_ERROR               = -1;
  OCI_INVALID_HANDLE      = -2;
  OCI_NEED_DATA           = 99;
  OCI_STILL_EXECUTING     = -3123;
  OCI_CONTINUE            = -24200;
  OCI_PASSWORD_INFO       = 28002; // the password will expire within ... days

  { Generic Default Value for Modes, .... }
  OCI_DEFAULT     = $0;

  { OCI Init Mode }
  OCI_THREADED    = $1;
  OCI_OBJECT      = $2;
  OCI_EVENTS      = $4;
  OCI_SHARED      = $10;
  OCI_NO_UCB      = $40;
  OCI_NO_MUTEX    = $80;

  { OCI Credentials }
  OCI_CRED_RDBMS  = 1;
  OCI_CRED_EXT    = 2;
  OCI_CRED_PROXY  = 3;

  { OCI Authentication Mode }
  OCI_MIGRATE     = $0001;             // migratable auth context
  OCI_SYSDBA      = $0002;             // for SYSDBA authorization
  OCI_SYSOPER     = $0004;             // for SYSOPER authorization
  OCI_PRELIM_AUTH = $0008;             // for preliminary authorization

  { OCIPasswordChange }
  OCI_AUTH        = $08;               // Change the password but do not login

  { OCI Data Types }
  SQLT_CHR = 1;
  SQLT_NUM = 2;
  SQLT_INT = 3;
  SQLT_FLT = 4;
  SQLT_STR = 5;
  SQLT_VNU = 6;
  SQLT_PDN = 7;
  SQLT_LNG = 8;
  SQLT_VCS = 9;
  SQLT_NON = 10;
  SQLT_RID = 11;
  SQLT_DAT = 12;
  SQLT_VBI = 15;
  SQLT_BFLOAT = 21;
  SQLT_BDOUBLE = 22;
  SQLT_BIN = 23;
  SQLT_LBI = 24;
  _SQLT_PLI = 29;
  SQLT_UIN = 68;
  SQLT_SLS = 91;
  SQLT_LVC = 94;
  SQLT_LVB = 95;
  SQLT_AFC = 96;
  SQLT_AVC = 97;
  SQLT_IBFLOAT = 100;
  SQLT_IBDOUBLE = 101;
  SQLT_CUR = 102;
  SQLT_RDD = 104;
  SQLT_LAB = 105;
  SQLT_OSL = 106;
  SQLT_NTY = 108;
  SQLT_REF = 110;
  SQLT_CLOB = 112;
  SQLT_BLOB = 113;
  SQLT_BFILEE = 114;
  SQLT_CFILEE = 115;
  SQLT_RSET = 116;
  SQLT_NCO = 122;
  SQLT_VST = 155;
  SQLT_ODT = 156;
  SQLT_DATE = 184;
  SQLT_TIME = 185;
  SQLT_TIME_TZ = 186;
  SQLT_TIMESTAMP = 187;
  SQLT_TIMESTAMP_TZ = 188;
  SQLT_INTERVAL_YM = 189;
  SQLT_INTERVAL_DS = 190;
  SQLT_TIMESTAMP_LTZ = 232;

  _SQLT_REC = 250;
  _SQLT_TAB = 251;
  _SQLT_BOL = 252;

  { OCI Statement Types }
  OCI_STMT_SELECT  = 1;   // select statement
  OCI_STMT_UPDATE  = 2;   // update statement
  OCI_STMT_DELETE  = 3;   // delete statement
  OCI_STMT_INSERT  = 4;   // Insert Statement
  OCI_STMT_CREATE  = 5;   // create statement
  OCI_STMT_DROP    = 6;   // drop statement
  OCI_STMT_ALTER   = 7;   // alter statement
  OCI_STMT_BEGIN   = 8;   // begin ... (pl/sql statement)
  OCI_STMT_DECLARE = 9;   // declare .. (pl/sql statement)

  { OCI Statement language }
  OCI_NTV_SYNTAX  = 1;    // Use what so ever is the native lang of server
  OCI_V7_SYNTAX   = 2;    // V7 language
  OCI_V8_SYNTAX   = 3;    // V8 language

  { OCI Statement Execute mode }
  OCI_BATCH_MODE        = $01;    // batch the oci statement for execution
  OCI_EXACT_FETCH       = $02;    // fetch the exact rows specified
  OCI_SCROLLABLE_CURSOR = $08;    // cursor scrollable
  OCI_DESCRIBE_ONLY     = $10;    // only describe the statement
  OCI_COMMIT_ON_SUCCESS = $20;    // commit, if successful execution
  OCI_NON_BLOCKING      = $40;    // non-blocking
  OCI_BATCH_ERRORS      = $80;    // batch errors in array dmls
  OCI_PARSE_ONLY        = $100;   // only parse the statement

  { Enable OCI Server-Side Statement Caching }
  OCI_STMT_CACHE       = $40;
  OCI_STMTCACHE_DELETE = $10;

  OCI_DATA_AT_EXEC    = $02;      // data at execute time
  OCI_DYNAMIC_FETCH   = $02;      // fetch dynamically
  OCI_PIECEWISE       = $04;      // piecewise DMLs or fetch

  { OCI Transaction modes }
  OCI_TRANS_NEW          = $00000001; // starts a new transaction branch
  OCI_TRANS_JOIN         = $00000002; // join an existing transaction
  OCI_TRANS_RESUME       = $00000004; // resume this transaction
  OCI_TRANS_STARTMASK    = $000000ff;

  OCI_TRANS_READONLY     = $00000100; // starts a readonly transaction
  OCI_TRANS_READWRITE    = $00000200; // starts a read-write transaction
  OCI_TRANS_SERIALIZABLE = $00000400; // starts a serializable transaction
  OCI_TRANS_ISOLMASK     = $0000ff00;

  OCI_TRANS_LOOSE        = $00010000; // a loosely coupled branch
  OCI_TRANS_TIGHT        = $00020000; // a tightly coupled branch
  OCI_TRANS_TYPEMASK     = $000f0000;

  OCI_TRANS_NOMIGRATE    = $00100000; // non migratable transaction
  OCI_TRANS_TWOPHASE     = $01000000; // use two phase commit

  { OCI pece wise fetch }
  OCI_ONE_PIECE       = 0; // one piece
  OCI_FIRST_PIECE     = 1; // the first piece
  OCI_NEXT_PIECE      = 2; // the next of many pieces
  OCI_LAST_PIECE      = 3; // the last piece

  { OCI fetch modes }
  OCI_FETCH_NEXT      = $02;  // next row
  OCI_FETCH_FIRST     = $04;  // first row of the result set
  OCI_FETCH_LAST      = $08;  // the last row of the result set
  OCI_FETCH_PRIOR     = $10;  // the previous row relative to current
  OCI_FETCH_ABSOLUTE  = $20;  // absolute offset from first
  OCI_FETCH_RELATIVE  = $40;  // offset relative to current

  {****************** Describe Handle Parameter Attributes *****************}

  { Attributes common to Columns and Stored Procs }
  OCI_ATTR_DATA_SIZE      = 1;    // maximum size of the data
  OCI_ATTR_DATA_TYPE      = 2;    // the SQL type of the column/argument
  OCI_ATTR_DISP_SIZE      = 3;    // the display size
  OCI_ATTR_NAME           = 4;    // the name of the column/argument
  OCI_ATTR_PRECISION      = 5;    // precision if number type
  OCI_ATTR_SCALE          = 6;    // scale if number type
  OCI_ATTR_IS_NULL        = 7;    // is it null ?
  OCI_ATTR_TYPE_NAME      = 8;    // name of the named data type or a package name for package private types
  OCI_ATTR_SCHEMA_NAME    = 9;    // the schema name
  OCI_ATTR_SUB_NAME       = 10;   // type name if package private type
  OCI_ATTR_POSITION       = 11;   // relative position of col/arg in the list of cols/args

  { complex object retrieval parameter attributes }
  OCI_ATTR_COMPLEXOBJECTCOMP_TYPE         = 50;
  OCI_ATTR_COMPLEXOBJECTCOMP_TYPE_LEVEL   = 51;
  OCI_ATTR_COMPLEXOBJECT_LEVEL            = 52;
  OCI_ATTR_COMPLEXOBJECT_COLL_OUTOFLINE   = 53;

  { Only Columns }
  OCI_ATTR_DISP_NAME                 = 100;  // the display name

  { Only Stored Procs }
  OCI_ATTR_OVERLOAD                  = 210;  // is this position overloaded
  OCI_ATTR_LEVEL                     = 211;  // level for structured types
  OCI_ATTR_HAS_DEFAULT               = 212;  // has a default value
  OCI_ATTR_IOMODE                    = 213;  // in, out inout
  OCI_ATTR_RADIX                     = 214;  // returns a radix
  OCI_ATTR_NUM_ARGS                  = 215;  // total number of arguments

  { only named type attributes }
  OCI_ATTR_TYPECODE                  = 216;   // object or collection
  OCI_ATTR_COLLECTION_TYPECODE       = 217;   // varray or nested table
  OCI_ATTR_VERSION                   = 218;   // user assigned version
  OCI_ATTR_IS_INCOMPLETE_TYPE        = 219;   // is this an incomplete type
  OCI_ATTR_IS_SYSTEM_TYPE            = 220;   // a system type
  OCI_ATTR_IS_PREDEFINED_TYPE        = 221;   // a predefined type
  OCI_ATTR_IS_TRANSIENT_TYPE         = 222;   // a transient type
  OCI_ATTR_IS_SYSTEM_GENERATED_TYPE  = 223;   // system generated type
  OCI_ATTR_HAS_NESTED_TABLE          = 224;   // contains nested table attr
  OCI_ATTR_HAS_LOB                   = 225;   // has a lob attribute
  OCI_ATTR_HAS_FILE                  = 226;   // has a file attribute
  OCI_ATTR_COLLECTION_ELEMENT        = 227;   // has a collection attribute
  OCI_ATTR_NUM_TYPE_ATTRS            = 228;   // number of attribute types
  OCI_ATTR_LIST_TYPE_ATTRS           = 229;   // list of type attributes
  OCI_ATTR_NUM_TYPE_METHODS          = 230;   // number of type methods
  OCI_ATTR_LIST_TYPE_METHODS         = 231;   // list of type methods
  OCI_ATTR_MAP_METHOD                = 232;   // map method of type
  OCI_ATTR_ORDER_METHOD              = 233;   // order method of type

  { only collection element }
  OCI_ATTR_NUM_ELEMS                 = 234;   // number of elements

  { only type methods }
  OCI_ATTR_ENCAPSULATION             = 235;   // encapsulation level
  OCI_ATTR_IS_SELFISH                = 236;   // method selfish
  OCI_ATTR_IS_VIRTUAL                = 237;   // virtual
  OCI_ATTR_IS_INLINE                 = 238;   // inline
  OCI_ATTR_IS_CONSTANT               = 239;   // constant
  OCI_ATTR_HAS_RESULT                = 240;   // has result
  OCI_ATTR_IS_CONSTRUCTOR            = 241;   // constructor
  OCI_ATTR_IS_DESTRUCTOR             = 242;   // destructor
  OCI_ATTR_IS_OPERATOR               = 243;   // operator
  OCI_ATTR_IS_MAP                    = 244;   // a map method
  OCI_ATTR_IS_ORDER                  = 245;   // order method
  OCI_ATTR_IS_RNDS                   = 246;   // read no data state method
  OCI_ATTR_IS_RNPS                   = 247;   // read no process state
  OCI_ATTR_IS_WNDS                   = 248;   // write no data state method
  OCI_ATTR_IS_WNPS                   = 249;   // write no process state

  OCI_ATTR_DESC_PUBLIC               = 250;   // public object

  { Object Cache Enhancements : attributes for User Constructed Instances }
  OCI_ATTR_CACHE_CLIENT_CONTEXT      = 251;
  OCI_ATTR_UCI_CONSTRUCT             = 252;
  OCI_ATTR_UCI_DESTRUCT              = 253;
  OCI_ATTR_UCI_COPY                  = 254;
  OCI_ATTR_UCI_PICKLE                = 255;
  OCI_ATTR_UCI_UNPICKLE              = 256;
  OCI_ATTR_UCI_REFRESH               = 257;

  { for type inheritance }
  OCI_ATTR_IS_SUBTYPE                = 258;
  OCI_ATTR_SUPERTYPE_SCHEMA_NAME     = 259;
  OCI_ATTR_SUPERTYPE_NAME            = 260;

  { for schemas }
  OCI_ATTR_LIST_OBJECTS              = 261;   // list of objects in schema

  { for database }
  OCI_ATTR_NCHARSET_ID               = 262;   // char set id
  OCI_ATTR_LIST_SCHEMAS              = 263;   // list of schemas
  OCI_ATTR_MAX_PROC_LEN              = 264;   // max procedure length
  OCI_ATTR_MAX_COLUMN_LEN            = 265;   // max column name length
  OCI_ATTR_CURSOR_COMMIT_BEHAVIOR    = 266;   // cursor commit behavior
  OCI_ATTR_MAX_CATALOG_NAMELEN       = 267;   // catalog namelength
  OCI_ATTR_CATALOG_LOCATION          = 268;   // catalog location
  OCI_ATTR_SAVEPOINT_SUPPORT         = 269;   // savepoint support
  OCI_ATTR_NOWAIT_SUPPORT            = 270;   // nowait support
  OCI_ATTR_AUTOCOMMIT_DDL            = 271;   // autocommit DDL
  OCI_ATTR_LOCKING_MODE              = 272;   // locking mode

  OCI_ATTR_CACHE_ARRAYFLUSH          = $40;
  OCI_ATTR_OBJECT_NEWNOTNULL         = $10;
  OCI_ATTR_OBJECT_DETECTCHANGE       = $20;

  { Piece Information }
  OCI_PARAM_IN                       = $01;  // in parameter
  OCI_PARAM_OUT                      = $02;  // out parameter

  { LOB Buffering Flush Flags }
  OCI_LOB_BUFFER_FREE     = 1;
  OCI_LOB_BUFFER_NOFREE   = 2;

  { FILE open modes }
  OCI_FILE_READONLY   = 1;    // readonly mode open for FILE types
  { LOB open modes }
  OCI_LOB_READONLY    = 1;    // readonly mode open for ILOB types
  OCI_LOB_READWRITE   = 2;    // read write mode open for ILOBs
  { LOB types }
  OCI_TEMP_BLOB       = 1;    // LOB type - BLOB
  OCI_TEMP_CLOB       = 2;    // LOB type - CLOB

  { CHAR/NCHAR/VARCHAR2/NVARCHAR2/CLOB/NCLOB char set "form" information
    (used e.g. by OCI_ATTR_CHARSET_FORM attribute) }
  SQLCS_IMPLICIT = 1;     // for CHAR, VARCHAR2, CLOB w/o a specified set
  SQLCS_NCHAR    = 2;     // for NCHAR, NCHAR VARYING, NCLOB
  SQLCS_EXPLICIT = 3;     // for CHAR, etc, with "CHARACTER SET ..." syntax
  SQLCS_FLEXIBLE = 4;     // for PL/SQL "flexible" parameters
  SQLCS_LIT_NULL = 5;     // for typecheck of NULL and empty_clob() lits

  { OCI_NUMBER }
  OCI_NUMBER_SIZE     = 22;
  OCI_NUMBER_UNSIGNED = 0;
  OCI_NUMBER_SIGNED   = 2;

  { OBJECT Duration }
  OCI_DURATION_BEGIN_                     = 10;
  OCI_DURATION_CALLOUT_                   = OCI_DURATION_BEGIN_ + 4;

  OCI_DURATION_INVALID: OCIDuration       = $FFFF;                   // Invalid duration
  OCI_DURATION_BEGIN: OCIDuration         = OCI_DURATION_BEGIN_;     // beginning sequence of duration
  OCI_DURATION_NULL: OCIDuration          = OCI_DURATION_BEGIN_ - 1; // null duration
  OCI_DURATION_DEFAULT: OCIDuration       = OCI_DURATION_BEGIN_ - 2; // default
  OCI_DURATION_USER_CALLBACK: OCIDuration = OCI_DURATION_BEGIN_ - 3;
  OCI_DURATION_NEXT: OCIDuration          = OCI_DURATION_BEGIN_ - 4; // next special duration
  OCI_DURATION_SESSION: OCIDuration       = OCI_DURATION_BEGIN_;     // the end of user session
  OCI_DURATION_TRANS: OCIDuration         = OCI_DURATION_BEGIN_ + 1; // the end of user transaction
  // DO NOT USE OCI_DURATION_CALL. IT  IS UNSUPPORTED
  // WILL BE REMOVED/CHANGED IN A FUTURE RELEASE
  OCI_DURATION_CALL: OCIDuration          = OCI_DURATION_BEGIN_ + 2; // the end of user client/server call
  OCI_DURATION_STATEMENT: OCIDuration     = OCI_DURATION_BEGIN_ + 3;
  // This is to be used only during callouts.  It is similar to that
  // of OCI_DURATION_CALL, but lasts only for the duration of a callout.
  // Its heap is from PGA
  OCI_DURATION_CALLOUT: OCIDuration       = OCI_DURATION_CALLOUT_;
  OCI_DURATION_LAST: OCIDuration          = OCI_DURATION_CALLOUT_;   // last of predefined durations
  // This is not being treated as other predefined durations such as
  // SESSION, CALL etc, because this would not have an entry in the duration
  // table and its functionality is primitive such that only allocate, free,
  // resize memory are allowed, but one cannot create subduration out of this
  OCI_DURATION_PROCESS: OCIDuration       = OCI_DURATION_BEGIN_ - 5; // next special duration

  { TYPE CODE }
  /// Type manager typecodes
  // - These are typecodes designed to be used with the type manager;
  // they also include longer, more readable versions of existing SQLT names
  // - Those types that are directly related to existing SQLT types are #define'd
  // to their SQLT equivalents
  // - The type manager typecodes are designed to be useable for all OCI calls.
  // They are in the range from 192 to 320 for typecodes, so as not to conflict
  // with existing OCI SQLT typecodes (see ocidfn.h)
  OCI_TYPECODE_REF             = SQLT_REF;      // SQL/OTS OBJECT REFERENCE
  OCI_TYPECODE_DATE            = SQLT_DAT;      // SQL DATE  OTS DATE
  OCI_TYPECODE_SIGNED8         = 27;            // SQL SIGNED INTEGER(8)  OTS SINT8
  OCI_TYPECODE_SIGNED16        = 28;            // SQL SIGNED INTEGER(16)  OTS SINT16
  OCI_TYPECODE_SIGNED32        = 29;            // SQL SIGNED INTEGER(32)  OTS SINT32
  OCI_TYPECODE_REAL            = 21;            // SQL REAL  OTS SQL_REAL
  OCI_TYPECODE_DOUBLE          = 22;            // SQL DOUBLE PRECISION  OTS SQL_DOUBLE
  OCI_TYPECODE_BFLOAT          = SQLT_IBFLOAT;  // Binary float
  OCI_TYPECODE_BDOUBLE         = SQLT_IBDOUBLE; // Binary double
  OCI_TYPECODE_FLOAT           = SQLT_FLT;      // SQL FLOAT(P)  OTS FLOAT(P)
  OCI_TYPECODE_NUMBER          = SQLT_NUM;      // SQL NUMBER(P S)  OTS NUMBER(P S)
  OCI_TYPECODE_DECIMAL         = SQLT_PDN;      // SQL DECIMAL(P S)  OTS DECIMAL(P S)
  OCI_TYPECODE_UNSIGNED8       = SQLT_BIN;      // SQL UNSIGNED INTEGER(8)  OTS UINT8
  OCI_TYPECODE_UNSIGNED16      = 25;            // SQL UNSIGNED INTEGER(16)  OTS UINT16
  OCI_TYPECODE_UNSIGNED32      = 26;            // SQL UNSIGNED INTEGER(32)  OTS UINT32
  OCI_TYPECODE_OCTET           = 245;           // SQL ???  OTS OCTET
  OCI_TYPECODE_SMALLINT        = 246;           // SQL SMALLINT  OTS SMALLINT
  OCI_TYPECODE_INTEGER         = SQLT_INT;      // SQL INTEGER  OTS INTEGER
  OCI_TYPECODE_RAW             = SQLT_LVB;      // SQL RAW(N)  OTS RAW(N)
  OCI_TYPECODE_PTR             = 32;            // SQL POINTER  OTS POINTER
  OCI_TYPECODE_VARCHAR2        = SQLT_VCS;      // SQL VARCHAR2(N)  OTS SQL_VARCHAR2(N)
  OCI_TYPECODE_CHAR            = SQLT_AFC;      // SQL CHAR(N)  OTS SQL_CHAR(N)
  OCI_TYPECODE_VARCHAR         = SQLT_CHR;      // SQL VARCHAR(N)  OTS SQL_VARCHAR(N)
  OCI_TYPECODE_MLSLABEL        = SQLT_LAB;      // OTS MLSLABEL
  OCI_TYPECODE_VARRAY          = 247;           // SQL VARRAY  OTS PAGED VARRAY
  OCI_TYPECODE_TABLE           = 248;           // SQL TABLE  OTS MULTISET
  OCI_TYPECODE_OBJECT          = SQLT_NTY;      // SQL/OTS NAMED OBJECT TYPE
  OCI_TYPECODE_OPAQUE          = 58;            //  SQL/OTS Opaque Types
  OCI_TYPECODE_NAMEDCOLLECTION = SQLT_NCO;      // SQL/OTS NAMED COLLECTION TYPE
  OCI_TYPECODE_BLOB            = SQLT_BLOB;     // SQL/OTS BINARY LARGE OBJECT
  OCI_TYPECODE_BFILE           = SQLT_BFILEE;   // SQL/OTS BINARY FILE OBJECT
  OCI_TYPECODE_CLOB            = SQLT_CLOB;     // SQL/OTS CHARACTER LARGE OBJECT
  OCI_TYPECODE_CFILE           = SQLT_CFILEE;   // SQL/OTS CHARACTER FILE OBJECT

  // the following are ANSI datetime datatypes added in 8.1
  OCI_TYPECODE_TIME            = SQLT_TIME;          // SQL/OTS TIME
  OCI_TYPECODE_TIME_TZ         = SQLT_TIME_TZ;       // SQL/OTS TIME_TZ
  OCI_TYPECODE_TIMESTAMP       = SQLT_TIMESTAMP;     // SQL/OTS TIMESTAMP
  OCI_TYPECODE_TIMESTAMP_TZ    = SQLT_TIMESTAMP_TZ;  // SQL/OTS TIMESTAMP_TZ

  OCI_TYPECODE_TIMESTAMP_LTZ   = SQLT_TIMESTAMP_LTZ; // TIMESTAMP_LTZ

  OCI_TYPECODE_INTERVAL_YM     = SQLT_INTERVAL_YM;   // SQL/OTS INTRVL YR-MON
  OCI_TYPECODE_INTERVAL_DS     = SQLT_INTERVAL_DS;   // SQL/OTS INTRVL DAY-SEC
  OCI_TYPECODE_UROWID          = SQLT_RDD;           // Urowid type

  OCI_TYPECODE_OTMFIRST        = 228;     // first Open Type Manager typecode
  OCI_TYPECODE_OTMLAST         = 320;     // last OTM typecode
  OCI_TYPECODE_SYSFIRST        = 228;     // first OTM system type (internal)
  OCI_TYPECODE_SYSLAST         = 235;     // last OTM system type (internal)
  OCI_TYPECODE_PLS_INTEGER     = 266;     // type code for PLS_INTEGER

  //// the following are PL/SQL-only internal. They should not be used
  //  OCI_TYPECODE_ITABLE          = SQLT_TAB;    // PLSQL indexed table
  //  OCI_TYPECODE_RECORD          = SQLT_REC;    // PLSQL record
  //  OCI_TYPECODE_boolean         = SQLT_BOL;    // PLSQL boolean

  // NOTE : The following NCHAR related codes are just short forms for saying
  // OCI_TYPECODE_VARCHAR2 with a charset form of SQLCS_NCHAR. These codes are
  // intended for use in the OCIAnyData API only and nowhere else.
  OCI_TYPECODE_NCHAR           = 286;
  OCI_TYPECODE_NVARCHAR2       = 287;
  OCI_TYPECODE_NCLOB           = 288;

  // To indicate absence of typecode being specified
  OCI_TYPECODE_NONE            = 0;
  // To indicate error has to be taken from error handle - reserved for sqlplus use
  OCI_TYPECODE_ERRHP           = 283;

  { TYPEGET options }
  OCI_TYPEGET_HEADER = 0;
  OCI_TYPEGET_ALL = 1;

  { OBJECT FREE OPTION }
  /// OCIObjectFreeFlag - Object free flag
  // - If OCI_OBJECTCOPY_FORCE is specified when freeing an instance, the instance
  // is freed regardless it is pinned or diritied.
  // If OCI_OBJECTCOPY_NONULL is specified when freeing an instance, the null
  // structure is not freed.
  OCI_OBJECTFREE_FORCE : ub2 = $0001;
  OCI_OBJECTFREE_NONULL: ub2 = $0002;
  OCI_OBJECTFREE_HEADER: ub2 = $0004;

  OCI_PREP2_CACHE_SEARCHONLY: ub4 = $0010;

type
  /// Oracle native number low-level representation
  OCINumber = packed record
    OCINumberPart: array [0..OCI_NUMBER_SIZE-1] of ub1;
  end;


{ ************ Oracle Date/Time Process }

type
  {$A-}
  /// memory structure used to store a date and time in native Oracle format
  // - follow the SQLT_DAT column type layout
  {$ifdef USERECORDWITHMETHODS}
  TOracleDate = record
  {$else}
  TOracleDate = object
  {$endif USERECORDWITHMETHODS}
    Cent, Year, Month, Day, Hour, Min, Sec: byte;
    /// convert an Oracle date and time into Delphi TDateTime
    // - this method will ignore any date before 30 Dec 1899 (i.e. any
    // TDateTime result < 0), to avoid e.g. wrong DecodeTime() computation from
    // retrieved value: if you need to retrieve dates before 1899, you should
    // better retrieve the content using ISO-8601 text encoding
    function ToDateTime: TDateTime;
    /// convert an Oracle date and time into its textual expanded ISO-8601
    // - will fill up to 21 characters, including double quotes
    function ToIso8601(Dest: PUtf8Char): integer; overload;
    /// convert an Oracle date and time into its textual expanded ISO-8601
    // - return the ISO-8601 text, without double quotes
    procedure ToIso8601(var aIso8601: RawByteString); overload;
    /// convert Delphi TDateTime into native Oracle date and time format
    procedure From(const aValue: TDateTime); overload;
    /// convert textual ISO-8601 into native Oracle date and time format
    procedure From(const aIso8601: RawUtf8); overload;
    /// convert textual ISO-8601 into native Oracle date and time format
    procedure From(aIso8601: PUtf8Char; Length: integer); overload;
  end;
  {$A+}
  POracleDate = ^TOracleDate;

  /// wrapper to an array of TOracleDate items
  TOracleDateArray = array[0..(maxInt div sizeof(TOracleDate)) - 1] of TOracleDate;
  POracleDateArray = ^TOracleDateArray;


{ ************ OCI Library Loading }

type
  /// direct access to the native Oracle Client Interface (OCI)
  TSqlDBOracleLib = class(TSynLibrary)
  protected
    procedure HandleError(Conn: TSqlDBConnection; Stmt: TSqlDBStatement;
      Status: integer; ErrorHandle: POCIError; InfoRaiseException: boolean = false;
      LogLevelNoRaise: TSynLogInfo = sllNone);
    function BlobOpen(Stmt: TSqlDBStatement; svchp: POCISvcCtx;
      errhp: POCIError; locp: POCIDescriptor): ub4;
    function BlobRead(Stmt: TSqlDBStatement; svchp: POCISvcCtx;
      errhp: POCIError; locp: POCIDescriptor; Blob: PByte; BlobLen: ub4;
      csid: ub2 = 0; csfrm: ub1 = SQLCS_IMPLICIT): ub4;
    function BlobReadToStream(Stmt: TSqlDBStatement; svchp: POCISvcCtx;
      errhp: POCIError; locp: POCIDescriptor; stream: TStream; BlobLen: ub4;
      csid: ub2 = 0; csfrm: ub1 = SQLCS_IMPLICIT): ub4;
    function BlobWriteFromStream(Stmt: TSqlDBStatement; svchp: POCISvcCtx;
      errhp: POCIError; locp: POCIDescriptor; stream: TStream; BlobLen: ub4;
      csid: ub2 = 0; csfrm: ub1 = SQLCS_IMPLICIT): ub4;
  public
    ClientVersion: function(var major_version, minor_version,
      update_num, patch_num, port_update_num: sword): sword; cdecl;
    EnvNlsCreate: function(var envhpp: pointer; mode: ub4; ctxp: Pointer;
      malocfp: Pointer; ralocfp: Pointer; mfreefp: Pointer; xtramemsz: size_T;
      usrmempp: PPointer; charset, ncharset: ub2): sword; cdecl;
    HandleAlloc: function(parenth: POCIHandle; var hndlpp: pointer;
      atype: ub4; xtramem_sz: size_T = 0; usrmempp: PPointer = nil): sword; cdecl;
    HandleFree: function(hndlp: Pointer; atype: ub4): sword; cdecl;
    ServerAttach: function(srvhp: POCIServer; errhp: POCIError; dblink: text;
      dblink_len: sb4; mode: ub4): sword; cdecl;
    ServerDetach: function(srvhp: POCIServer; errhp: POCIError;
      mode: ub4): sword; cdecl;
    AttrGet: function(trgthndlp: POCIHandle; trghndltyp: ub4;
      attributep: Pointer; sizep: Pointer; attrtype: ub4;
      errhp: POCIError): sword; cdecl;
    AttrSet: function(trgthndlp: POCIHandle; trghndltyp: ub4;
      attributep: Pointer; size: ub4; attrtype: ub4; errhp: POCIError): sword; cdecl;
    SessionBegin: function(svchp: POCISvcCtx; errhp: POCIError;
      usrhp: POCISession; credt: ub4; mode: ub4): sword; cdecl;
    SessionEnd: function(svchp: POCISvcCtx; errhp: POCIError;
      usrhp: POCISession; mode: ub4): sword; cdecl;
    ErrorGet: function(hndlp: Pointer; recordno: ub4; sqlstate: text;
      var errcodep: sb4; bufp: text; bufsiz: ub4; atype: ub4): sword; cdecl;
    StmtPrepare: function(stmtp: POCIStmt; errhp: POCIError; stmt: text;
      stmt_len: ub4; language:ub4; mode: ub4): sword; cdecl;
    StmtExecute: function(svchp: POCISvcCtx; stmtp: POCIStmt;
      errhp: POCIError; iters: ub4; rowoff: ub4; snap_in: POCISnapshot;
      snap_out: POCISnapshot; mode: ub4): sword; cdecl;
    StmtFetch: function(stmtp: POCIStmt; errhp: POCIError; nrows: ub4;
      orientation: ub2; mode: ub4): sword; cdecl;
    BindByPos: function(stmtp: POCIStmt; var bindpp: POCIBind;
      errhp: POCIError; position: ub4; valuep: Pointer; value_sz: sb4; dty: ub2;
      indp: Pointer; alenp: Pointer; rcodep: Pointer; maxarr_len: ub4;
      curelep: Pointer; mode: ub4): sword; cdecl;
    ParamGet: function(hndlp: Pointer; htype: ub4; errhp: POCIError;
      var parmdpp: Pointer; pos: ub4): sword; cdecl;
    TransStart: function(svchp: POCISvcCtx; errhp: POCIError; timeout: word;
      flags: ub4): sword; cdecl;
    TransRollback: function(svchp:POCISvcCtx; errhp:POCIError;
      flags: ub4): sword; cdecl;
    TransCommit: function(svchp: POCISvcCtx; errhp: POCIError;
      flags: ub4) :sword; cdecl;
    DescriptorAlloc: function(parenth: POCIEnv; var descpp: pointer;
      htype: ub4; xtramem_sz: integer; usrmempp: Pointer): sword; cdecl;
    DescriptorFree: function(descp: Pointer; htype: ub4): sword; cdecl;
    DateTimeConstruct: function(hndl: POCIEnv; err: POCIError;
      datetime: POCIDateTime; year: sb2; month: ub1; day: ub1; hour: ub1;
      min: ub1; sec: ub1; fsec: ub4; timezone: text;
      timezone_length: size_t): sword; cdecl;
    DateTimeGetDate: function(hndl: POCIEnv; err: POCIError;
      const date: POCIDateTime; var year: sb2; var month: ub1;
      var day: ub1): sword; cdecl;
    DefineByPos: function(stmtp: POCIStmt; var defnpp: POCIDefine;
      errhp: POCIError; position: ub4; valuep: Pointer; value_sz: sb4; dty: ub2;
      indp: Pointer; rlenp: Pointer; rcodep: Pointer; mode: ub4): sword; cdecl;
    LobGetLength: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; var lenp: ub4): sword; cdecl;
    LobGetChunkSize: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; var chunk_size: ub4): sword; cdecl;
    LobOpen: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; mode: ub1): sword; cdecl;
    LobRead: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; var amtp: ub4; offset: ub4; bufp: Pointer; bufl: ub4;
      ctxp: Pointer = nil; cbfp: Pointer = nil; csid: ub2 = 0;
      csfrm: ub1 = SQLCS_IMPLICIT): sword; cdecl;
    LobClose: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator): sword; cdecl;
    LobWrite: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; var amtp: ub4; offset: ub4; bufp: Pointer; buflen: ub4;
      piece: ub1; ctxp: Pointer = nil; cbfp: Pointer = nil; csid: ub2 = 0;
      csfrm: ub1 = SQLCS_IMPLICIT): sword; cdecl;
    NlsCharSetNameToID: function(env: POCIEnv; name: PUtf8Char): sword; cdecl;
    StmtPrepare2: function(svchp: POCISvcCtx; var stmtp: POCIStmt; errhp: POCIError;
      stmt: text; stmt_len: ub4; key: text; key_len: ub4;
      language:ub4; mode: ub4): sword; cdecl;
    StmtRelease: function(stmtp: POCIStmt; errhp: POCIError; key: text; key_len: ub4;
      mode: ub4): sword; cdecl;
    TypeByName: function(env: POCIEnv; errhp: POCIError; svchp: POCISvcCtx;
      schema_name: text; s_length: ub4; type_name: text; t_length: ub4; version_name: text; v_length: ub4;
      pin_duration: OCIDuration; get_option: ub4; var tdo: POCIType): sword; cdecl;
    ObjectNew: function(env: POCIEnv; errhp: POCIError; svchp: POCISvcCtx; typecode: OCITypeCode;
      tdo: POCIType; table: dvoid; duration: OCIDuration; value: boolean; var instance: dvoid): sword; cdecl;
    ObjectFree: function(env: POCIEnv; errhp: POCIError; instance: dvoid; flag: ub2): sword; cdecl;
    NumberFromInt: function(errhp: POCIError; inum: dvoid; inum_length: uword; inum_s_flag: uword;
      var number: OCINumber): sword; cdecl;
    StringAssignText : function(env: POCIEnv; errhp: POCIError; rhs: OraText; rhs_len: ub4;
      var lhs: POCIString): sword; cdecl;
    CollAppend: function(env: POCIEnv; errhp: POCIError; elem: dvoid; elemind: dvoid;
      coll: POCIColl): sword; cdecl;
    BindObject: function(bindp: POCIBind; errhp: POCIError; type_: POCIType; var pgvpp: dvoid;
      pvszsp: pub4; indpp: pdvoid; indszp: pub4): sword; cdecl;
    PasswordChange: function(svchp: POCISvcCtx; errhp: POCIError; const user_name: text; usernm_len: ub4;
      const opasswd: text; opasswd_len: ub4; const npasswd: text; npasswd_len: sb4; mode: ub4): sword; cdecl;
  public
    // the client verion numbers
    major_version, minor_version, update_num, patch_num, port_update_num: sword;
    /// if OCI handles directly Int64 bound parameters (revision >= 11.2)
    SupportsInt64Params: boolean;
    /// OCI will call OCILobGetChunkSize when retrieving BLOB/CLOB content
    // - is enabled by default, to avoid ORA-2481 errors when reading more than
    // 96 MB of data, but you may disable chunking if you prefer by setting false
    UseLobChunks: boolean;
    /// load the oci.dll library
    // - and retrieve all Oci*() addresses for OCI_ENTRIES[] items
    constructor Create;
    /// retrieve the client version as 'oci.dll rev. 11.2.0.1'
    function ClientRevision: RawUtf8;
    /// retrieve the OCI charset ID from a Windows Code Page
    // - will only handle most known Windows Code Page
    // - if aCodePage=0, will use the NLS_LANG environment variable
    // - will use 'WE8MSWIN1252' (CODEPAGE_US) if the Code Page is unknown
    function CodePageToCharSetID(env: pointer; aCodePage: cardinal): cardinal;
    /// raise an exception on error
    procedure Check(Conn: TSqlDBConnection; Stmt: TSqlDBStatement;
      Status: integer; ErrorHandle: POCIError;
      InfoRaiseException: boolean = false; LogLevelNoRaise: TSynLogInfo = sllNone);
      {$ifdef HASINLINE}inline;{$endif}
    procedure CheckSession(Conn: TSqlDBConnection; Stmt: TSqlDBStatement;
      Status: integer; ErrorHandle: POCIError;
      InfoRaiseException: boolean = false; LogLevelNoRaise: TSynLogInfo = sllNone);
    /// retrieve some BLOB content
    procedure BlobFromDescriptor(Stmt: TSqlDBStatement; svchp: POCISvcCtx;
      errhp: POCIError; locp: POCIDescriptor; out result: RawByteString); overload;
    /// retrieve some BLOB content
    procedure BlobFromDescriptor(Stmt: TSqlDBStatement; svchp: POCISvcCtx;
      errhp: POCIError; locp: POCIDescriptor; out result: TBytes); overload;
    /// retrieve some BLOB content, save it to the stream
    procedure BlobFromDescriptorToStream(Stmt: TSqlDBStatement; svchp: POCISvcCtx;
      errhp: POCIError; locp: POCIDescriptor; stream: TStream);
    /// write some BLOB content, read it from the stream
    procedure BlobToDescriptorFromStream(Stmt: TSqlDBStatement; svchp: POCISvcCtx;
      errhp: POCIError; locp: POCIDescriptor; stream: TStream);
    /// retrieve some CLOB/NCLOB content as UTF-8 text
    function ClobFromDescriptor(Stmt: TSqlDBStatement; svchp: POCISvcCtx;
      errhp: POCIError; locp: POCIDescriptor; ColumnDBForm: integer;
      out Text: RawUtf8; TextResize: boolean = true): ub4;
  end;


{ *************** Some Global Types and Variables }

type
  /// exception type associated to the native Oracle Client Interface (OCI)
  ESqlDBOracle = class(ESqlDBException);

  /// Oracle VARNUM memory structure
  TSqlT_VNU = array[0..21] of byte;
  /// points to a Oracle VARNUM memory structure
  PSqlT_VNU = ^TSQLT_VNU;

  
/// conversion from a 64-bit integer to a raw VARNUM memory structure
procedure Int64ToSqlT_VNU(Value: Int64; OutData: PSqlT_VNU);


var
  /// global variable used to access the Oracle Client Library once loaded
  OCI: TSqlDBOracleLib = nil;

  /// optional folder where the Oracle Client Library is to be searched
  // - by default, the oci.dll library is searched in the system PATH, then
  // in %ORACLE_HOME%\bin
  // - you can specify here a folder name in which the oci.dll is to be found
  SynDBOracleOCIpath: TFileName;

const
  // defined here for overriding OCI_CHARSET_UTF8/OCI_CHARSET_WIN1252 if needed
  OCI_UTF8 = $367;
  OCI_AL32UTF8 = $369;
  OCI_UTF16ID = 1000;
  OCI_WE8MSWIN1252 = 178;

var
  /// the OCI charset used for UTF-8 encoding
  // - OCI_UTF8 is a deprecated encoding, and OCI_AL32UTF8 should be preferred
  // - but you can fallback for OCI_UTF8 for compatibility purposes
  OCI_CHARSET_UTF8: cardinal = OCI_AL32UTF8;

  /// the OCI charset used for WinAnsi encoding
  OCI_CHARSET_WIN1252: cardinal = OCI_WE8MSWIN1252;

  /// how many blob chunks should be handled at once
  SynDBOracleBlobChunksCount: ub4 = 250;

/// check if two Oracle Charset codes are similar
function SimilarCharSet(aCharset1, aCharset2: cardinal): boolean;

/// return the text name from an Oracle Charset code
function OracleCharSetName(aCharsetID: cardinal): PUtf8Char;

/// return the system code page corresponding to an Oracle Charset code
function CharSetIDToCodePage(aCharSetID: cardinal): cardinal;


implementation


{ ************ Oracle Date/Time Process }

{ TOracleDate }

// see http://download.oracle.com/docs/cd/B28359_01/appdev.111/b28395/oci03typ.htm#sthref389

function TOracleDate.ToDateTime: TDateTime;
begin
  if (PInteger(@self)^ = 0) and
     (PInteger(PtrUInt(@self) + 3)^ = 0) then
    // Cent=Year=Month=Day=Hour=Main=Sec=0 -> returns 0
    result := 0
  else
  begin
    if Cent <= 100 then
      // avoid TDateTime values < 0 (generates wrong DecodeTime)
      result := 0
    else
      result := EncodeDate((Cent - 100) * 100 + Year - 100, Month, Day);
    if (Hour > 1) or
       (Min > 1) or
       (Sec > 1) then
      result := result + EncodeTime(Hour - 1, Min - 1, Sec - 1, 0);
  end;
end;

procedure TOracleDate.ToIso8601(var aIso8601: RawByteString);
var
  tmp: array[0..23] of AnsiChar;
begin
  if (PInteger(@self)^ = 0) and
     (PInteger(PtrUInt(@self) + 3)^ = 0) then
    // Cent=Year=Month=Day=Hour=Main=Sec=0 -> stored as ""
    aIso8601 := ''
  else
  begin
    DateToIso8601PChar(tmp{%H-}, true, (Cent - 100) * 100 + Year - 100, Month, Day);
    if (Hour > 1) or
       (Min > 1) or
       (Sec > 1) then
    begin
      TimeToIso8601PChar(@tmp[10], true, Hour - 1, Min - 1, Sec - 1, 0, 'T');
      SetString(aIso8601, tmp, 19); // we use 'T' as TTextWriter.AddDateTime
    end
    else
      SetString(aIso8601, tmp, 10); // only date
  end;
end;

function TOracleDate.ToIso8601(Dest: PUtf8Char): integer;
var
  Y: cardinal;
begin
  Dest^ := '"';
  if (PInteger(@self)^ = 0) and
     (PInteger(PtrUInt(@self) + 3)^ = 0) then
    // Cent=Year=Month=Day=Hour=Main=Sec=0 -> stored as ""
    result := 2
  else
  begin
    Y := (Cent - 100) * 100 + Year - 100;
    if Y > 9999 then
      // avoid integer overflow -> stored as ""
      result := 2
    else
    begin
      DateToIso8601PChar(Dest + 1, true, Y, Month, Day);
      if (Hour > 1) or
         (Min > 1) or
         (Sec > 1) then
      begin
        TimeToIso8601PChar(Dest + 11, true, Hour - 1, Min - 1, Sec - 1, 0, 'T');
        result := 21; // we use 'T' as TTextWriter.AddDateTime
      end
      else
        result := 12; // only date
    end;
  end;
  Dest[result - 1] := '"';
end;

procedure TOracleDate.From(const aValue: TDateTime);
var
  T: TSynSystemTime;
begin
  if aValue <= 0 then
  begin
    PInteger(@self)^ := 0;
    PInteger(PtrUInt(@self) + 3)^ := 0; // set Day=Hour=Min=Sec to 0
    exit; // supplied TDateTime value = 0 -> store as null date
  end;
  T.FromDateTime(aValue);
  Cent := (T.Year div 100) + 100;
  Year := (T.Year mod 100) + 100;
  Month := T.Month;
  Day := T.Day;
  if (T.Hour <> 0) or
     (T.Minute <> 0) or
     (T.Second <> 0) then
  begin
    Hour := T.Hour + 1;
    Min := T.Minute + 1;
    Sec := T.Second + 1;
  end
  else
  begin
    Hour := 1;
    Min := 1;
    Sec := 1;
  end;
end;

procedure TOracleDate.From(const aIso8601: RawUtf8);
begin
  From(pointer(aIso8601), length(aIso8601));
end;

procedure TOracleDate.From(aIso8601: PUtf8Char; Length: integer);
var
  Value: QWord;
  Value32: cardinal absolute Value;
  Y: cardinal;
  NoTime: boolean;
begin
  Value := Iso8601ToTimeLogPUtf8Char(aIso8601, Length, @NoTime);
  if Value = 0 then
  begin
    PInteger(@self)^ := 0;
    PInteger(PtrUInt(@self) + 3)^ := 0;  // set Day=Hour=Min=Sec to 0
    exit; // invalid ISO-8601 text -> store as null date
  end;
  Y := Value shr (6 + 6 + 5 + 5 + 4);
  Cent := (Y div 100) + 100;
  Year := (Y mod 100) + 100;
  Month := ((Value32 shr (6 + 6 + 5 + 5)) and 15) + 1;
  Day := ((Value32 shr (6 + 6 + 5)) and 31) + 1;
  if NoTime then
  begin
    Hour := 1;
    Min := 1;
    Sec := 1;
    exit;
  end;
  Hour := ((Value32 shr (6 + 6)) and 31) + 1;
  Min := ((Value32 shr 6) and 63) + 1;
  Sec := (Value32 and 63) + 1;
end;


{ ************ OCI Library Loading }

const
  // http://download.oracle.com/docs/cd/B19306_01/server.102/b14225/applocaledata.htm#i635016
  // http://www.mydul.net/charsets.html
  CODEPAGES: array[0..26] of record
    Num: cardinal;
    Charset: cardinal;
    Text: PUtf8Char
  end = ((
    Num: 1252;
    Charset: OCI_WE8MSWIN1252;
    Text: 'WE8MSWIN1252'
  ), (
    Num: 1250;
    Charset: 170;
    Text: 'EE8MSWIN1250'
  ), (
    Num: 1251;
    Charset: 171;
    Text: 'CL8MSWIN1251'
  ), (
    Num: 1253;
    Charset: 174;
    Text: 'EL8MSWIN1253'
  ), (
    Num: 1254;
    Charset: 177;
    Text: 'TR8MSWIN1254'
  ), (
    Num: 1255;
    Charset: 175;
    Text: 'IW8MSWIN1255'
  ), (
    Num: 1256;
    Charset: 560;
    Text: 'AR8MSWIN1256'
  ), (
    Num: 1257;
    Charset: 179;
    Text: 'BLT8MSWIN1257'
  ), (
    Num: 874;
    Charset: 41;
    Text: 'TH8TISASCII'
  ), (
    Num: 932;
    Charset: 832;
    Text: 'JA16SJIS'
  ), (
    Num: 949;
    Charset: 846;
    Text: 'KO16MSWIN949'
  ), (
    Num: 936;
    Charset: 852;
    Text: 'ZHS16GBK'
  ), (
    Num: 950;
    Charset: 867;
    Text: 'ZHT16MSWIN950'
  ), (
    Num: 1258;
    Charset: 45;
    Text: 'VN8MSWIN1258'
  ), (
    Num: CP_UTF8;
    CharSet: OCI_UTF8;
    Text: 'UTF8'
  ), (
    Num: CP_UTF16;
    CharSet: OCI_UTF16ID;
    Text: 'UTF16'
  ), (
    Num: 437;
    CharSet: 4;
    Text: 'US8PC437'
  ), (
    Num: 850;
    CharSet: 10;
    Text: 'WE8PC850'
  ), (
    Num: 858;
    CharSet: 28;
    Text: 'WE8PC858'
  ), (
    Num: 921;
    Charset: 176;
    Text: 'LT8MSWIN921'
  ), (
    Num: 923;
    Charset: 172;
    Text: 'ET8MSWIN923'
  ),
    // handle some aliases of code page Num values
    (
    Num: CP_UTF8;
    CharSet: OCI_AL32UTF8;
    Text: 'AL32UTF8'
  ), (
    Num: CP_UTF16;
    CharSet: 2000;
    Text: 'AL16UTF16'
  ), (
    Num: CP_UTF16;
    CharSet: 2002;
    Text: 'AL16UTF16LE'
  ),
    // wrong approximation (to be fixed)
    (
    Num: 932;
    Charset: 830;
    Text: 'JA16EUC'
  ), (
    Num: 1252;
    Charset: 46;
    Text: 'WE8ISO8859P15'
  ), (
    Num: 1252;
    Charset: 31;
    Text: 'WE8ISO8859P1'
  ));

  OCI_ENTRIES: array[0..40] of PAnsiChar = (
    'OCIClientVersion', 'OCIEnvNlsCreate',
    'OCIHandleAlloc', 'OCIHandleFree', 'OCIServerAttach', 'OCIServerDetach',
    'OCIAttrGet', 'OCIAttrSet', 'OCISessionBegin', 'OCISessionEnd',
    'OCIErrorGet', 'OCIStmtPrepare', 'OCIStmtExecute', 'OCIStmtFetch',
    'OCIBindByPos', 'OCIParamGet', 'OCITransStart', 'OCITransRollback',
    'OCITransCommit', 'OCIDescriptorAlloc', 'OCIDescriptorFree',
    'OCIDateTimeConstruct', 'OCIDateTimeGetDate', 'OCIDefineByPos',
    'OCILobGetLength', 'OCILobGetChunkSize', 'OCILobOpen', 'OCILobRead',
    'OCILobClose', 'OCILobWrite', 'OCINlsCharSetNameToId', 'OCIStmtPrepare2',
    'OCIStmtRelease', 'OCITypeByName', 'OCIObjectNew', 'OCIObjectFree',
    'OCINumberFromInt', 'OCIStringAssignText', 'OCICollAppend', 'OCIBindObject',
    'OCIPasswordChange');


{ TSqlDBOracleLib }

function TSqlDBOracleLib.BlobOpen(Stmt: TSqlDBStatement; svchp: POCISvcCtx;
  errhp: POCIError; locp: POCIDescriptor): ub4;
begin
  result := 0;
  Check(nil, Stmt, LobOpen(svchp, errhp, locp, OCI_LOB_READONLY), errhp);
  try
    Check(nil, Stmt, LobGetLength(svchp, errhp, locp, result), errhp);
  except
    Check(nil, Stmt, LobClose(svchp, errhp, locp), errhp);
    raise;
  end;
end;

function TSqlDBOracleLib.BlobRead(Stmt: TSqlDBStatement; svchp: POCISvcCtx;
  errhp: POCIError; locp: POCIDescriptor; Blob: PByte; BlobLen: ub4; csid: ub2;
  csfrm: ub1): ub4;
var
  Read, ChunkSize: ub4;
  Status: sword;
begin
  result := BlobLen;
  if BlobLen = 0 then
    exit; // nothing to read
  if UseLobChunks then
  begin
    Check(nil, Stmt, LobGetChunkSize(svchp, errhp, locp, ChunkSize), errhp);
    result := 0;
    repeat
      Read := BlobLen;
      Status := LobRead(svchp, errhp, locp, Read, 1, Blob, ChunkSize, nil, nil,
        csid, csfrm);
      inc(Blob, Read);
      inc(result, Read);
    until Status <> OCI_NEED_DATA;
    Check(nil, Stmt, Status, errhp);
  end
  else
    Check(nil, Stmt, LobRead(svchp, errhp, locp, result, 1, Blob, result, nil,
      nil, csid, csfrm), errhp);
end;

function TSqlDBOracleLib.BlobReadToStream(Stmt: TSqlDBStatement;
  svchp: POCISvcCtx; errhp: POCIError; locp: POCIDescriptor; stream: TStream;
  BlobLen: ub4; csid: ub2; csfrm: ub1): ub4;
var
  Read, ChunkSize: ub4;
  Status: sword;
  tmp: RawByteString;
begin
  result := BlobLen;
  if BlobLen = 0 then
    exit; // nothing to read
  if UseLobChunks then
  begin
    Check(nil, Stmt, LobGetChunkSize(svchp, errhp, locp, ChunkSize), errhp);
    SetLength(tmp, ChunkSize * SynDBOracleBlobChunksCount);
    result := 0;
    repeat
      Read := BlobLen;
      Status := LobRead(svchp, errhp, locp, Read, 1, pointer(tmp), Length(tmp),
        nil, nil, csid, csfrm);
      stream.WriteBuffer(pointer(tmp)^, Read);
      inc(result, Read);
    until Status <> OCI_NEED_DATA;
    Check(nil, Stmt, Status, errhp);
  end
  else
  begin
    SetLength(tmp, BlobLen);
    Check(nil, Stmt, LobRead(svchp, errhp, locp, result, 1, pointer(tmp), result,
      nil, nil, csid, csfrm), errhp);
    stream.WriteBuffer(pointer(tmp)^, result);
  end;
end;

procedure TSqlDBOracleLib.BlobFromDescriptor(Stmt: TSqlDBStatement;
  svchp: POCISvcCtx; errhp: POCIError; locp: POCIDescriptor;
  out result: RawByteString);
var
  Len, Read: ub4;
begin
  Len := BlobOpen(Stmt, svchp, errhp, locp);
  try
    SetLength(result, Len);
    Read := BlobRead(Stmt, svchp, errhp, locp, pointer(result), Len);
    if Read <> Len then
      SetLength(result, Read);
  finally
    Check(nil, Stmt, LobClose(svchp, errhp, locp), errhp);
  end;
end;

procedure TSqlDBOracleLib.BlobFromDescriptor(Stmt: TSqlDBStatement;
  svchp: POCISvcCtx; errhp: POCIError; locp: POCIDescriptor; out result: TBytes);
var
  Len, Read: ub4;
begin
  Len := BlobOpen(Stmt, svchp, errhp, locp);
  try
    SetLength(result, Len);
    Read := BlobRead(Stmt, svchp, errhp, locp, pointer(result), Len);
    if Read <> Len then
      SetLength(result, Read);
  finally
    Check(nil, Stmt, LobClose(svchp, errhp, locp), errhp);
  end;
end;

procedure TSqlDBOracleLib.BlobFromDescriptorToStream(Stmt: TSqlDBStatement;
  svchp: POCISvcCtx; errhp: POCIError; locp: POCIDescriptor; stream: TStream);
var
  Len: ub4;
begin
  Len := BlobOpen(Stmt, svchp, errhp, locp);
  try
    BlobReadToStream(Stmt, svchp, errhp, locp, stream, Len);
  finally
    Check(nil, Stmt, LobClose(svchp, errhp, locp), errhp);
  end;
end;

procedure TSqlDBOracleLib.BlobToDescriptorFromStream(Stmt: TSqlDBStatement;
  svchp: POCISvcCtx; errhp: POCIError; locp: POCIDescriptor; stream: TStream);
begin
  BlobWriteFromStream(Stmt, svchp, errhp, locp, stream, stream.Size);
end;

function TSqlDBOracleLib.BlobWriteFromStream(Stmt: TSqlDBStatement;
  svchp: POCISvcCtx; errhp: POCIError; locp: POCIDescriptor; stream: TStream;
  BlobLen: ub4; csid: ub2; csfrm: ub1): ub4;
var
  ChunkSize, l_Read, l_Write, l_Offset: ub4;
  tmp: RawByteString;
begin
  Check(nil, Stmt, LobGetChunkSize(svchp, errhp, locp, ChunkSize), errhp);
  SetLength(tmp, ChunkSize * SynDBOracleBlobChunksCount);
  l_Offset := 1;
  while stream.Position < stream.Size do
  begin
    l_Read := stream.Read(pointer(tmp)^, Length(tmp));
    l_Write := l_Read;
    Check(nil, Stmt, LobWrite(svchp, errhp, locp, l_Write, l_Offset, pointer(tmp),
      l_Read, OCI_ONE_PIECE), errhp);
    inc(l_Offset, l_Write);
  end;
  result := l_Offset;
end;

function TSqlDBOracleLib.ClobFromDescriptor(Stmt: TSqlDBStatement;
  svchp: POCISvcCtx; errhp: POCIError; locp: POCIDescriptor; ColumnDBForm: integer;
  out Text: RawUtf8; TextResize: boolean): ub4;
var
  Len: ub4;
begin
  Len := BlobOpen(Stmt, svchp, errhp, locp);
  try
    if Len > 0 then
    begin
      Len := Len * 3; // max UTF-8 size according to number of characters
      SetLength(Text, Len);
      result := BlobRead(Stmt, svchp, errhp, locp, pointer(Text), Len,
        OCI_CHARSET_UTF8, ColumnDBForm);
      if TextResize then
        SetLength(Text, result)
      else
        Text[result + 1] := #0; // ensure ASCIIZ (e.g. when escaping to JSON)
    end
    else
      result := 0;
  finally
    Check(nil, Stmt, LobClose(svchp, errhp, locp), errhp);
  end;
end;

procedure TSqlDBOracleLib.HandleError(Conn: TSqlDBConnection;
  Stmt: TSqlDBStatement; Status: integer; ErrorHandle: POCIError;
  InfoRaiseException: boolean; LogLevelNoRaise: TSynLogInfo);
var
  msg: RawUtf8;
  tmp: array[0..3071] of AnsiChar;
  L, ErrNum: integer;
begin
  case Status of
    OCI_ERROR, OCI_SUCCESS_WITH_INFO:
      begin
        tmp[0] := #0;
        ErrorGet(ErrorHandle, 1, nil, ErrNum, tmp, sizeof(tmp), OCI_HTYPE_ERROR);
        L := mormot.core.base.StrLen(@tmp);
        while (L > 0) and
              (tmp[L - 1] < ' ') do
        begin
          tmp[L - 1] := #0; // trim right #10
          dec(L);
        end;
        msg := CurrentAnsiConvert.AnsiBufferToRawUtf8(tmp, L);
        if (Status = OCI_SUCCESS_WITH_INFO) and
           not InfoRaiseException then
        begin
          if LogLevelNoRaise = sllNone then // may be e.g. sllWarning
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
    OCI_NEED_DATA:
      msg := 'OCI_NEED_DATA';
    OCI_NO_DATA:
      msg := 'OCI_NO_DATA';
    OCI_INVALID_HANDLE:
      msg := 'OCI_INVALID_HANDLE';
    OCI_STILL_EXECUTING:
      msg := 'OCI_STILL_EXECUTING';
    OCI_CONTINUE:
      msg := 'OCI_CONTINUE';
  end;
  if LogLevelNoRaise <> sllNone then
    SynDBLog.Add.Log(LogLevelNoRaise, msg{%H-}, self)
  else if Stmt = nil then
    raise ESqlDBOracle.CreateUtf8('% error: %', [self, msg])
  else
    raise ESqlDBOracle.CreateUtf8('% error: %', [Stmt, msg]);
end;

procedure TSqlDBOracleLib.Check(Conn: TSqlDBConnection; Stmt: TSqlDBStatement;
  Status: integer; ErrorHandle: POCIError; InfoRaiseException: boolean;
  LogLevelNoRaise: TSynLogInfo);
begin
  if Status <> OCI_SUCCESS then
    HandleError(Conn, Stmt, Status, ErrorHandle, InfoRaiseException, LogLevelNoRaise);
end;

procedure TSqlDBOracleLib.CheckSession(Conn: TSqlDBConnection;
  Stmt: TSqlDBStatement; Status: integer; ErrorHandle: POCIError;
  InfoRaiseException: boolean; LogLevelNoRaise: TSynLogInfo);
var
  msg: RawUtf8;
  tmp: array[0..3071] of AnsiChar;
  L, ErrNum: integer;
begin
  if Status <> OCI_ERROR then
    Check(Conn, Stmt, Status, ErrorHandle, InfoRaiseException, LogLevelNoRaise)
  else
  begin
    tmp[0] := #0;
    ErrorGet(ErrorHandle, 1, nil, ErrNum, tmp, sizeof(tmp), OCI_HTYPE_ERROR);
    L := mormot.core.base.StrLen(@tmp);
    while (L > 0) and
          (tmp[L - 1] < ' ') do
    begin
      tmp[L - 1] := #0; // trim right #10
      dec(L);
    end;
    msg := CurrentAnsiConvert.AnsiBufferToRawUtf8(tmp, L);
    if ErrNum = 28001 then
      if Conn <> nil then
        if Conn.PasswordChange then
          Exit;
    if LogLevelNoRaise <> sllNone then
      SynDBLog.Add.Log(LogLevelNoRaise, msg, self)
    else if Stmt = nil then
      raise ESqlDBOracle.CreateUtf8('% error: %', [self, msg])
    else
      raise ESqlDBOracle.CreateUtf8('% error: %', [Stmt, msg]);
  end;
end;

function TSqlDBOracleLib.ClientRevision: RawUtf8;
begin
  if self = nil then
    result := ''
  else
    result := FormatUtf8('% rev. %.%.%.%',
      [fLibraryPath, major_version, minor_version, update_num, patch_num]);
end;

function TSqlDBOracleLib.CodePageToCharSetID(env: pointer; aCodePage: cardinal): cardinal;
var
  ocp: PUtf8Char;
  i: integer;
  nlslang: AnsiString;
begin
  case aCodePage of
    0:
      begin
        nlslang := AnsiString(GetEnvironmentVariable('NLS_LANG'));
        if nlslang <> '' then
          result := NlsCharSetNameToID(env, pointer(nlslang))
        else
          result := CodePageToCharSetID(env, Unicode_CodePage);
      end;
    CP_UTF8:
      result := OCI_CHARSET_UTF8;
    CP_UTF16:
      result := OCI_UTF16ID;
  else
    begin
      ocp := CODEPAGES[0].Text; // default is MS Windows Code Page 1252
      for i := 0 to high(CODEPAGES) do
        if aCodePage = CODEPAGES[i].Num then
        begin
          ocp := CODEPAGES[i].Text;
          break;
        end;
      result := NlsCharSetNameToID(env, ocp);
    end;
  end;
  if result = 0 then
    result := OCI_WE8MSWIN1252;
end;

const
{$ifdef MSWINDOWS}
  LIBNAME = 'oci.dll';
{$else}
  LIBNAME = 'libclntsh.so';
{$endif MSWINDOWS}

constructor TSqlDBOracleLib.Create;
var
  P: PPointerArray;
  i: PtrInt;
  l1, l2, l3: TFileName;
begin
  if (SynDBOracleOCIpath <> '') and
     DirectoryExists(SynDBOracleOCIpath) then
    l1 := ExtractFilePath(ExpandFileName(SynDBOracleOCIpath + PathDelim)) + LIBNAME;
  l2 := ExeVersion.ProgramFilePath + LIBNAME;
  if not FileExists(l2) then
  begin
    l2 := ExeVersion.ProgramFilePath + 'OracleInstantClient';
    if not DirectoryExists(l2) then
    begin
      l2 := ExeVersion.ProgramFilePath + 'OCI';
      if not DirectoryExists(l2) then
        l2 := ExeVersion.ProgramFilePath + 'Oracle';
    end;
    l2 := l2 + PathDelim + LIBNAME;
  end;
  l3 := GetEnvironmentVariable('ORACLE_HOME');
  if l3 <> '' then
    l3 := IncludeTrailingPathDelimiter(l3) + 'bin' + PathDelim + LIBNAME;
  TryLoadLibrary([{%H-}l1, l2, l3, LIBNAME], ESqlDBOracle);
  P := @@ClientVersion;
  for i := 0 to High(OCI_ENTRIES) do
    Resolve(OCI_ENTRIES[i], @P[i], {raiseonfailure=}ESqlDBOracle);
  ClientVersion(major_version, minor_version, update_num, patch_num, port_update_num);
  SupportsInt64Params := (major_version > 11) or
                         ((major_version = 11) and
                          (minor_version > 1));
  UseLobChunks := true; // by default
end;


{ *************** Some Global Types and Variables }

procedure Int64ToSqlT_VNU(Value: Int64; OutData: PSqlT_VNU);
var
  V, Exp: byte;
  minus: boolean; // True, if the sign is positive
  Size, i: PtrInt;
  Mant: array[0..19] of byte;
begin
  FillcharFast(Mant, sizeof(Mant), 0);
  Exp := 0;
  Size := 1;
  minus := Value >= 0;
  if not minus then
    Value := not Value;
  while Value > 0 do
  begin
    if Value >= 100 then
    begin
      V := Value mod 100;
      Value := Value div 100;
      inc(Exp);
    end
    else
    begin
      V := Value;
      Value := 0;
    end;
    if (V <> 0) or
       (Size > 1) then
    begin
      if minus then
        inc(V)
      else
        V := (100 + 1) - V;
      Mant[Size - 1] := V;
      inc(Size);
    end;
  end;
  if Size > 1 then
    for i := 0 to Size - 1 do
      OutData[Size - i] := Mant[i];
  Exp := (Exp + 65) or $80;
  if not minus and
     (Size < high(TSQLT_VNU)) then
  begin
    Exp := not Exp;
    inc(Size);
    OutData[Size] := (100 + 2);
  end;
  OutData[1] := Exp;
  OutData[0] := Size;
end;

function SimilarCharSet(aCharset1, aCharset2: cardinal): boolean;
var
  i1, i2: integer;
begin
  result := true;
  if aCharset1 = aCharset2 then
    exit;
  for i1 := 0 to high(CODEPAGES) do
    if CODEPAGES[i1].Charset = aCharset1 then
      for i2 := 0 to High(CODEPAGES) do
        if (CODEPAGES[i2].Charset = aCharset2) and
           (CODEPAGES[i1].Num = CODEPAGES[i2].Num) then
          exit; // aliases are allowed
  result := false;
end;

function OracleCharSetName(aCharsetID: cardinal): PUtf8Char;
var
  i: integer;
begin
  for i := 0 to high(CODEPAGES) do
    with CODEPAGES[i] do
      if Charset = aCharsetID then
      begin
        result := Text;
        exit;
      end;
  result := '?';
end;

function CharSetIDToCodePage(aCharSetID: cardinal): cardinal;
var
  i: integer;
begin
  for i := 0 to high(CODEPAGES) do
    with CODEPAGES[i] do
      if Charset = aCharSetID then
      begin
        result := Num;
        exit;
      end;
  result := Unicode_CodePage; // return the default OS code page if not found
end;



initialization

finalization
  FreeAndNil(OCI);
  
end.

