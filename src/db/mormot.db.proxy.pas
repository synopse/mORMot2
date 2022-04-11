/// Database Framework Remote HTTP Access Using Binary Proxy Communication
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.proxy;

{
  *****************************************************************************

   Allow Remote HTTP Access of any mormot.db.sql connections via a Proxy
    - Shared Proxy Information
    - Server-Side Proxy Remote Protocol
    - Client-Side Proxy Remote Protocol
    - HTTP Server Classes for Remote Access
    - HTTP Client Classes for Remote Access

   This unit contains the communication-abstracted logic to handle a
   remote DB connection using an efficient proprietary binary protocol.

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
  mormot.core.data,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.rtti,
  mormot.crypt.secure,
  mormot.db.core,
  mormot.db.sql,
  mormot.net.sock,
  mormot.net.http,
  mormot.net.client,
  mormot.net.server;


{ ************ Shared Proxy Information }

type
  /// exception raised during remote connection process
  ESqlDBRemote = class(ESqlDBException);

  /// proxy commands implemented by TSqlDBProxyConnectionProperties.Process()
  // - method signature expect "const Input" and "var Output" arguments
  // - Input is not used for cConnect, cDisconnect, cGetForeignKeys,
  // cTryStartTransaction, cCommit, cRollback and cServerTimestamp
  // - Input is the TSqlDBProxyConnectionProperties instance for cInitialize
  // - Input is the RawUtf8 table name for most cGet* metadata commands
  // - Input is the SQL statement and associated bound parameters for cExecute,
  // cExecuteToBinary, cExecuteToJson, and cExecuteToExpandedJson, encoded as
  // TSqlDBProxyConnectionCommandExecute record
  // - Output is not used for cConnect, cDisconnect, cCommit, cRollback and cExecute
  // - Output is TSqlDBDefinition (i.e. DBMS type) for cInitialize
  // - Output is TTimeLog for cServerTimestamp
  // - Output is boolean for cTryStartTransaction
  // - Output is TSqlDBColumnDefineDynArray for cGetFields
  // - Output is TSqlDBIndexDefineDynArray for cGetIndexes
  // - Output is TSynNameValue (fForeignKeys) for cGetForeignKeys
  // - Output is TRawUtf8DynArray for cGetTableNames
  // - Output is RawByteString result data for cExecuteToBinary
  // - Output is UpdateCount: integer text for cExecute
  // - Output is RawUtf8 result data for cExecuteToJson and cExecuteToExpandedJson
  // - calls could be declared as such:
  // ! Process(cGetToken,?,result: Int64);
  // ! Process(cGetDbms,User#1Hash: RawUtf8,fDbms: TSqlDBDefinition);
  // ! Process(cConnect,?,?);
  // ! Process(cDisconnect,?,?);
  // ! Process(cTryStartTransaction,?,started: boolean);
  // ! Process(cCommit,?,?);
  // ! Process(cRollback,?,?);
  // ! Process(cServerTimestamp,?,result: TTimeLog);
  // ! Process(cGetFields,aTableName: RawUtf8,Fields: TSqlDBColumnDefineDynArray);
  // ! Process(cGetIndexes,aTableName: RawUtf8,Indexes: TSqlDBIndexDefineDynArray);
  // ! Process(cGetTableNames,?,Tables: TRawUtf8DynArray);
  // ! Process(cGetForeignKeys,?,fForeignKeys: TSynNameValue);
  // ! Process(cExecute,Request: TSqlDBProxyConnectionCommandExecute,UpdateCount: integer);
  // ! Process(cExecuteToBinary,Request: TSqlDBProxyConnectionCommandExecute,Data: RawByteString);
  // ! Process(cExecuteToJson,Request: TSqlDBProxyConnectionCommandExecute,JSON: RawUtf8);
  // ! Process(cExecuteToExpandedJson,Request: TSqlDBProxyConnectionCommandExecute,JSON: RawUtf8);
  // - cExceptionRaised is a pseudo-command, used only for sending an exception
  // to the client in case of execution problem on the server side
  TSqlDBProxyConnectionCommand = (
    cGetToken,
    cGetDbms,
    cConnect,
    cDisconnect,
    cTryStartTransaction,
    cCommit,
    cRollback,
    cServerTimestamp,
    cGetFields,
    cGetIndexes,
    cGetTableNames,
    cGetForeignKeys,
    cExecute,
    cExecuteToBinary,
    cExecuteToJson,
    cExecuteToExpandedJson,
    cQuit,
    cExceptionRaised);

  /// server-side process flags for TSqlDBProxyConnectionCommandExecute.Force
  TSqlDBProxyConnectionCommandExecuteForce = set of (
    fBlobAsNull,
    fDateWithMS,
    fNoUpdateCount);

  /// structure to embedd all needed parameters to execute a SQL statement
  // - used for cExecute, cExecuteToBinary, cExecuteToJson and cExecuteToExpandedJson
  // commands of TSqlDBProxyConnectionProperties.Process()
  // - set by TSqlDBProxyStatement.ParamsToCommand() protected method
  TSqlDBProxyConnectionCommandExecute = packed record
    /// the associated SQL statement
    SQL: RawUtf8;
    /// input parameters
    // - trunked to the exact number of parameters
    Params: TSqlDBParamDynArray;
    /// if input parameters expected BindArray() process
    ArrayCount: integer;
    /// how server side would handle statement execution
    // - fBlobAsNull and fDateWithMS do match ForceBlobAsNull and ForceDateWithMS
    // ISqlDBStatement properties
    // - fNoUpdateCount avoids to call ISqlDBStatement.UpdateCount method, e.g.
    // for performance reasons
    Force: TSqlDBProxyConnectionCommandExecuteForce;
  end;


/// retrieve the ready-to-be displayed text of proxy commands implemented by
// TSqlDBProxyConnectionProperties.Process()
function ToText(cmd: TSqlDBProxyConnectionCommand): PShortString; overload;



{ ************ Server-Side Proxy Remote Protocol }

type
  /// server-side implementation of a proxy connection to any mormot.db.sql engine
  // - this default implementation will send the data without compression,
  // digital signature, nor encryption
  // - inherit from this class to customize the transmission layer content
  TSqlDBProxyConnectionProtocol = class
  protected
    fAuthenticate: TSynAuthenticationAbstract;
    fTransactionSessionID: integer;
    fTransactionRetryTimeout: Int64;
    fTransactionActiveTimeout: Int64;
    fTransactionActiveAutoReleaseTicks: Int64;
    fLock: TRTLCriticalSection;
    function GetAuthenticate: TSynAuthenticationAbstract;
    /// default Handle*() will just return the incoming value
    function HandleInput(const input: RawByteString): RawByteString; virtual;
    function HandleOutput(const output: RawByteString): RawByteString; virtual;
    /// default trial transaction
    function TransactionStarted(connection: TSqlDBConnection;
      sessionID: integer): boolean; virtual;
    procedure TransactionEnd(sessionID: integer); virtual;
  public
    /// initialize a protocol, with a given authentication scheme
    // - if no authentication is given, none will be processed
    constructor Create(aAuthenticate: TSynAuthenticationAbstract); reintroduce;
    /// release associated authentication class
    destructor Destroy; override;
    /// server-side implementation of a remote connection to any mormot.db.sql engine
    // - follow the compressed binary message format expected by the
    // TSqlDBRemoteConnectionPropertiesAbstract.ProcessMessage method
    // - any transmission protocol could call this method to execute the
    // corresponding TSqlDBProxyConnectionCommand on the current connection
    // - replaces TSqlDBConnection.RemoteProcessMessage from mORMot 1.18
    procedure RemoteProcessMessage(const Input: RawUtf8;
      out Output: RawUtf8; Connection: TSqlDBConnection); virtual;
    /// the associated authentication information
    // - you can manage users via AuthenticateUser/DisauthenticateUser methods
    property Authenticate: TSynAuthenticationAbstract
      read GetAuthenticate write fAuthenticate;
  end;

  /// server-side implementation of a remote connection to any mormot.db.sql engine
  // - implements digitally signed SynLZ-compressed binary message format,
  // with simple symmetric encryption, as expected by this unit
  TSqlDBRemoteConnectionProtocol = class(TSqlDBProxyConnectionProtocol)
  protected
    /// SynLZ decompression + digital signature + encryption
    function HandleInput(const input: RawByteString): RawByteString; override;
    /// SynLZ compression + digital signature + encryption
    function HandleOutput(const output: RawByteString): RawByteString; override;
  public
  end;

  /// specify the class of a proxy/remote connection to any mormot.db.sql engine
  TSqlDBProxyConnectionProtocolClass = class of TSqlDBProxyConnectionProtocol;


{ ************ Client-Side Proxy Remote Protocol }

type
  /// implements a proxy-like virtual connection statement to a DB engine
  // - will generate TSqlDBProxyConnection kind of connection
  TSqlDBProxyConnectionPropertiesAbstract = class(TSqlDBConnectionProperties)
  protected
    fHandleConnection: boolean;
    fProtocol: TSqlDBProxyConnectionProtocol;
    fCurrentSession: integer;
    fStartTransactionTimeOut: Int64;
    /// abstract process of internal commands
    // - one rough unique method is used, in order to make easier several
    // implementation schemes and reduce data marshalling as much as possible
    // - should raise an exception on error
    // - returns the session ID (if any)
    function Process(Command: TSqlDBProxyConnectionCommand;
      const Input; var Output): integer; virtual; abstract;
    /// calls Process(cGetToken) + Process(cGetDbms)
    // - override this method and set fProtocol before calling inherited
    procedure SetInternalProperties; override;
    /// calls Process(cGetForeignKeys,self,fForeignKeys)
    procedure GetForeignKeys; override;
  public
    /// will notify for proxy disconnection
    destructor Destroy; override;
    /// create a new TSqlDBProxyConnection instance
    // - the caller is responsible of freeing this instance
    function NewConnection: TSqlDBConnection; override;
    /// retrieve the column/field layout of a specified table
    // - calls Process(cGetFields,aTableName,Fields)
    procedure GetFields(const aTableName: RawUtf8; out Fields: TSqlDBColumnDefineDynArray); override;
    /// retrieve the advanced indexed information of a specified Table
    // - calls Process(cGetIndexes,aTableName,Indexes)
    procedure GetIndexes(const aTableName: RawUtf8; out Indexes: TSqlDBIndexDefineDynArray); override;
    /// get all table names
    // - this default implementation will use protected SqlGetTableNames virtual
    // - calls Process(cGetTableNames,self,Tables)
    procedure GetTableNames(out Tables: TRawUtf8DynArray); override;
    /// determine if the SQL statement can be cached
    // - always returns false, to force a new fake statement to be created
    function IsCachable(P: PUtf8Char): boolean; override;
  published
    /// Connect and Disconnect won't really connect nor disconnect the
    // remote connection
    // - you can set this property to TRUE if you expect the remote connection
    // by in synch with the remote proxy connection (should not be used in
    // most cases, unless you are sure you have only one single client at a time
    property HandleConnection: boolean
      read fHandleConnection write fHandleConnection;
    /// milliseconds to way until StartTransaction is allowed by the server
    // - in the current implementation, there should be a single transaction
    // at once on the server side: this is the time to try before reporting
    // an ESqlDBRemote exception failure
    property StartTransactionTimeOut: Int64
      read fStartTransactionTimeOut write fStartTransactionTimeOut;
  end;

  /// implements an abstract proxy-like virtual connection to a DB engine
  // - can be used e.g. for remote access or execution in a background thread
  TSqlDBProxyConnection = class(TSqlDBConnection)
  protected
    fConnected: boolean;
    fProxy: TSqlDBProxyConnectionPropertiesAbstract;
    function GetServerDateTime: TDateTime; override;
  public
    /// connect to a specified database engine
    constructor Create(aProperties: TSqlDBConnectionProperties); override;
    /// connect to the specified database
    procedure Connect; override;
    /// stop connection to the specified database
    procedure Disconnect; override;
    /// return TRUE if Connect has been already successfully called
    function IsConnected: boolean; override;
    /// initialize a new SQL query statement for the given connection
    function NewStatement: TSqlDBStatement; override;
    /// begin a Transaction for this connection
    procedure StartTransaction; override;
    /// commit changes of a Transaction for this connection
    procedure Commit; override;
    /// discard changes of a Transaction for this connection
    procedure Rollback; override;
  end;

  /// implements a proxy-like virtual connection statement to a DB engine
  // - abstract class, with no corresponding kind of connection, but allowing
  // access to the mapped data via Column*() methods
  // - will handle an internal binary buffer when the statement returned rows
  // data, as generated by TSqlDBStatement.FetchAllToBinary()
  TSqlDBProxyStatementAbstract = class(TSqlDBStatementWithParamsAndColumns)
  protected
    fDataRowCount: integer;
    fDataRowReaderOrigin, fDataRowReader: PByte;
    fDataRowNullSize: cardinal;
    fDataCurrentRowNullLen: cardinal;
    fDataCurrentRowNull: TByteDynArray;
    fDataCurrentRowValues: array of pointer;
    fDataCurrentRowValuesStart: pointer;
    fDataCurrentRowValuesSize: cardinal;
    // per-row column type (SQLite3 only) e.g. select coalesce(column,0) from ..
    fDataCurrentRowColTypes: array of TSqlDBFieldType;
    function InternalColumnType(Col: integer; out Data: PByte): TSqlDBFieldType;
      {$ifdef HASINLINE}inline;{$endif}
    procedure InternalHeaderProcess(Data: PByte; DataLen: PtrInt);
    procedure InternalFillDataCurrent(
      var Reader: PByte; IgnoreColumnDataSize: boolean);
  public
    /// the Column type of the current Row
    function ColumnType(Col: integer;
      FieldSize: PInteger = nil): TSqlDBFieldType; override;
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
    /// return a Column text value as generic VCL string of the current Row, first Col is 0
    function ColumnString(Col: integer): string; override;
    /// return a Column as a blob value of the current Row, first Col is 0
    function ColumnBlob(Col: integer): RawByteString; override;
    /// return all columns values into JSON content
    procedure ColumnsToJson(WR: TResultsWriter); override;
    /// direct access to the data buffer of the current row
    // - points to Double/Currency value, or variable-length Int64/UTF-8/Blob
    // - points to nil if the column value is NULL
    function ColumnData(Col: integer): pointer;
    /// append current row content as binary stream
    // - will save one data row in optimized binary format (if not in Null)
    // - virtual method called by FetchAllToBinary()
    // - follows the format expected by TSqlDBProxyStatement
    procedure ColumnsToBinary(W: TBufferWriter; Null: pointer;
      const ColTypes: TSqlDBFieldTypeDynArray); override;

    /// read-only access to the number of data rows stored
    property DataRowCount: integer
      read fDataRowCount;
  end;

  /// implements a proxy-like virtual connection statement to a DB engine
  // - is generated by TSqlDBProxyConnection kind of connection
  // - will use an internal binary buffer when the statement returned rows data,
  // as generated by TSqlDBStatement.FetchAllToBinary() or JSON for
  // ExecutePreparedAndFetchAllAsJson() method (as expected by our ORM)
  TSqlDBProxyStatement = class(TSqlDBProxyStatementAbstract)
  protected
    fDataInternalCopy: RawByteString;
    fUpdateCount: integer;
    fForceNoUpdateCount: boolean;
    procedure ParamsToCommand(var Input: TSqlDBProxyConnectionCommandExecute);
  public
    /// Execute a SQL statement
    // - for TSqlDBProxyStatement, preparation and execution are processed in
    // one step, when this method is executed - as such, Prepare() won't call
    // the remote process, but will just set fSql
    // - this overridden implementation will use out optimized binary format
    //  as generated by TSqlDBStatement.FetchAllToBinary(), and not JSON
    procedure ExecutePrepared; override;
    /// execute a prepared SQL statement and return all rows content as a JSON string
    // - JSON data is retrieved with UTF-8 encoding
    // - if Expanded is true, JSON output is a standard array of objects, for
    // direct use with any Ajax or .NET client:
    // & [{"f1":"1v1","f2":1v2},{"f2":"2v1","f2":2v2}...]
    // - if Expanded is false, JSON data is serialized in non-expanded format:
    // & {"fieldCount":2,"values":["f1","f2","1v1",1v2,"2v1",2v2...],"rowCount":20}
    // resulting in lower space use and faster process - it could be parsed by
    // TOrmTableJson or TDocVariantData.InitArrayFromResults
    // - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"'
    // format and contains true BLOB data
    // - this overridden implementation will use JSON for transmission, and
    // binary encoding only for parameters (to avoid unneeded conversions, e.g.
    // when called from mormot.orm.sql.pas)
    procedure ExecutePreparedAndFetchAllAsJson(Expanded: boolean;
      out Json: RawUtf8; ReturnedRowCount: PPtrInt = nil); override;
    /// append all rows content as binary stream
    // - will save the column types and name, then every data row in optimized
    // binary format (faster and smaller than JSON)
    // - you can specify a LIMIT for the data extent (default 0 meaning all data)
    // - generates the format expected by TSqlDBProxyStatement
    // - this overriden method will use the internal data copy of the binary
    // buffer retrieved by ExecutePrepared, so would be almost immediate,
    // and would allow e.g. direct consumption via our TSynSqlStatementDataSet
    // - note that DataRowPosition won't be set by this method: will be done
    // e.g. in TSqlDBProxyStatementRandomAccess.Create
    function FetchAllToBinary(Dest: TStream; MaxRowCount: cardinal = 0;
      DataRowPosition: PCardinalDynArray = nil): cardinal; override;
    /// gets a number of updates made by latest executed statement
    // - this overriden method will return the integer value returned by
    // cExecute command
    function UpdateCount: integer; override;
    /// force no UpdateCount method call on server side
    // - may be needed to reduce server load, if this information is not needed
    property ForceNoUpdateCount: boolean
      read fForceNoUpdateCount write fForceNoUpdateCount;

    /// after a statement has been prepared via Prepare() + ExecutePrepared() or
    //   Execute(), this method must be called one or more times to evaluate it
    function Step(SeekFirst: boolean = false): boolean; override;
  end;

  /// client-side implementation of a remote connection to any mormot.db.sql engine
  // - will compute binary compressed messages for the remote processing,
  // ready to be served e.g. over HTTP
  // - abstract class which should override its protected ProcessMessage() method
  // e.g. by TSqlDBRemoteConnectionPropertiesTest or
  TSqlDBRemoteConnectionPropertiesAbstract = class(TSqlDBProxyConnectionPropertiesAbstract)
  protected
    /// will build and interpret binary messages to be served with ProcessMessage
    // - would raise an exception in case of error, even on the server side
    function Process(Command: TSqlDBProxyConnectionCommand;
      const Input; var Output): integer; override;
    /// abstract method to override for the expected transmission protocol
    // - could raise an exception on transmission error
    procedure ProcessMessage(const Input: RawUtf8; out Output: RawUtf8);
      virtual; abstract;
  end;

  /// fake proxy class for testing the remote connection to any mormot.db.sql engine
  // - resulting overhead due to our binary messaging: unnoticeable :)
  TSqlDBRemoteConnectionPropertiesTest = class(TSqlDBRemoteConnectionPropertiesAbstract)
  protected
    fProps: TSqlDBConnectionProperties;
    // this overriden method will just call fProtocol.RemoteProcessMessage()
    procedure ProcessMessage(const Input: RawUtf8; out Output: RawUtf8); override;
  public
    /// create a test redirection to an existing local connection property
    // - you can specify a User/Password credential pair to also test the
    // authentication via TSynAuthentication
    constructor Create(aProps: TSqlDBConnectionProperties;
      const aUserID, aPassword: RawUtf8;
      aProtocol: TSqlDBProxyConnectionProtocolClass); reintroduce;
  end;


  /// implements a virtual statement with direct data access
  // - is generated with no connection, but allows direct random access to any
  // data row retrieved from TSqlDBStatement.FetchAllToBinary() binary data
  // - GotoRow() method allows direct access to a row data via Column*()
  // - is used e.g. by TSynSqlStatementDataSet of SynDBVCL unit
  TSqlDBProxyStatementRandomAccess = class(TSqlDBProxyStatementAbstract)
  protected
    fRowData: TCardinalDynArray;
    fLastGotoRow: integer;
  public
    /// initialize the internal structure from a given memory buffer
    // - by default, ColumnDataSize would be computed from the supplied data,
    // unless you set IgnoreColumnDataSize=true to set the value to 0 (and
    // force e.g. SynDBVCL TSynBinaryDataSet.InternalInitFieldDefs define the
    // field as ftDefaultMemo)
    constructor Create(Data: PByte; DataLen: integer;
      DataRowPosition: PCardinalDynArray = nil;
      IgnoreColumnDataSize: boolean = false); reintroduce;

    /// Execute a prepared SQL statement
    // - this unexpected overridden method will raise a ESqlDBRemote
    procedure ExecutePrepared; override;
    /// Change cursor position to the next available row
    // - this unexpected overridden method will raise a ESqlDBRemote
    function Step(SeekFirst: boolean = false): boolean; override;

    /// change the current data Row
    // - if Index<DataRowCount, returns TRUE and you can access to the data
    // via regular Column*() methods
    // - can optionally raise an ESqlDBRemote if Index is not correct
    function GotoRow(Index: integer; RaiseExceptionOnWrongIndex: boolean = false): boolean;
    /// search for a value within the internal binary stream
    // - used to implement e.g. TDataSet.Locate
    function ColumnSearch(Col: integer; const Value: variant;
      CaseInsensitive: boolean): integer;
  end;



{ ************ HTTP Server Classes for Remote Access }

const
  /// default HTTP port to be used for mormot.db.proxy remote access if none is specified
  SYNDB_DEFAULT_HTTP_PORT = '8092';

type
  /// used to define the HTTP server class for publishing a mormot.db.proxy connection
  TSqlDBServerClass = class of TSqlDBServerAbstract;

  /// implements a generic HTTP server, able to publish any mormot.db.proxy connection
  // - do not instantiate this class, but rather use TSqlDBServerHttpApi or
  // TSqlDBServerSockets - this abstract class won't set any HTTP server
  TSqlDBServerAbstract = class
  protected
    fServer: THttpServerGeneric;
    fThreadPoolCount: integer;
    fPort, fDatabaseName: RawUtf8;
    fHttps: boolean;
    fProperties: TSqlDBConnectionProperties;
    fProtocol: TSqlDBProxyConnectionProtocol;
    fSafe: TSynLocker;
    fProcessLocked: boolean;
    // this is where the process would take place
    function Process(Ctxt: THttpServerRequestAbstract): cardinal;
  public
    /// publish the mormot.db.sql connection on a given HTTP port and URI
    // - this generic constructor won't initialize the HTTP server itself:
    // use overriden constructors instead
    // - URI would follow the supplied aDatabaseName parameter on the given port
    // e.g. http://serverip:8092/remotedb for
    // ! Create(aProps,'remotedb');
    // - you can optionally register one user credential, or change the
    // transmission Protocol which is TSqlDBRemoteConnectionProtocol by default
    // - aProperties.ThreadingMode will be set to the optional aThreadMode
    // parameter tmMainConnection by default, which would also set ProcessLocked
    // to TRUE - in fact, you should better use a single thread for the process,
    // but you may define a small thread pool for the process IF the provider
    // supports it
    constructor Create(aProperties: TSqlDBConnectionProperties;
      const aDatabaseName: RawUtf8; const aPort: RawUtf8 = SYNDB_DEFAULT_HTTP_PORT;
      const aUserName: RawUtf8 = ''; const aPassword: RawUtf8 = '';
      aHttps: boolean = false; aThreadPoolCount: integer = 1;
      aProtocol: TSqlDBProxyConnectionProtocolClass = nil;
      aThreadMode: TSqlDBConnectionPropertiesThreadSafeThreadingMode = tmMainConnection;
      aAuthenticate: TSynAuthenticationAbstract = nil); virtual;
    /// released used memory
    destructor Destroy; override;
    /// the associated database connection properties
    property Properties: TSqlDBConnectionProperties
      read fProperties write fProperties;
    /// the associated port number
    property Port: RawUtf8
      read fPort;
    /// the associated database name
    property DatabaseName: RawUtf8
      read fDatabaseName;
    /// the associated communication protocol
    // - to manage user authentication, use AuthenticateUser/DisauthenticateUser
    // methods of Protocol.Authenticate
    property Protocol: TSqlDBProxyConnectionProtocol
      read fProtocol write fProtocol;
    /// if the internal Process() method would be protected by a critical section
    // - set to TRUE if constructor's aThreadMode is left to its default
    // tmMainConnection value
    property ProcessLocked: boolean
      read fProcessLocked write fProcessLocked;
  end;

  /// implements a mormot.db.proxy HTTP server via the user-land Sockets API
  TSqlDBServerSockets = class(TSqlDBServerAbstract)
  protected
  public
    /// publish the mormot.db.sql connection on a given HTTP port and URI using sockets
    // - URI would follow the supplied aDatabaseName parameter on the given port
    // e.g. http://serverip:8092/remotedb for
    // ! Create(aProps,'remotedb');
    // - you can optionally register one user credential
    // - parameter aHttps is ignored by this class
    // - is implemented via a THttpServer instance, which will maintain one
    // thread per client connection, which is as expected by some DB drivers e.g.
    // for transaction consistency
    constructor Create(aProperties: TSqlDBConnectionProperties;
      const aDatabaseName: RawUtf8; const aPort: RawUtf8 = SYNDB_DEFAULT_HTTP_PORT;
      const aUserName: RawUtf8 = ''; const aPassword: RawUtf8 = '';
      aHttps: boolean = false; aThreadPoolCount: integer = 1;
      aProtocol: TSqlDBProxyConnectionProtocolClass = nil;
      aThreadMode: TSqlDBConnectionPropertiesThreadSafeThreadingMode = tmMainConnection;
      aAuthenticate: TSynAuthenticationAbstract = nil); override;
  end;

  {$ifdef USEHTTPSYS}

  /// implements a mormot.db.proxy HTTP server using fast http.sys kernel-mode server
  // - under Windows, this class is faster and more stable than TSqlDBServerSockets
  TSqlDBServerHttpApi = class(TSqlDBServerAbstract)
  protected
  public
    /// publish the mormot.db.sql connection on a given HTTP port and URI using http.sys
    // - URI would follow the supplied aDatabaseName parameter on the given port
    // e.g. http://serverip:8092/remotedb for
    // ! Create(aProps,'remotedb');
    // - you can optionally register one user credential
    constructor Create(aProperties: TSqlDBConnectionProperties;
      const aDatabaseName: RawUtf8; const aPort: RawUtf8 = SYNDB_DEFAULT_HTTP_PORT;
      const aUserName: RawUtf8 = ''; const aPassword: RawUtf8 = '';
      aHttps: boolean = false; aThreadPoolCount: integer = 1;
      aProtocol: TSqlDBProxyConnectionProtocolClass = nil;
      aThreadMode: TSqlDBConnectionPropertiesThreadSafeThreadingMode = tmMainConnection;
      aAuthenticate: TSynAuthenticationAbstract = nil); override;
  end;

  /// the default mormot.db.proxy HTTP server class on each platform
  TSqlDBServerRemote = TSqlDBServerHttpApi;

  {$else}

  TSqlDBServerRemote = TSqlDBServerSockets;

  {$endif USEHTTPSYS}


{ ************ HTTP Client Classes for Remote Access }

type
  /// implements a generic HTTP client, able to access remotely any mormot.db.sql
  // - do not instantiate this class, but rather use TSqlDBSocketConnectionProperties
  //  TSqlDBWinHttpConnectionProperties TSqlDBWinINetConnectionProperties
  TSqlDBHttpConnectionPropertiesAbstract = class(TSqlDBRemoteConnectionPropertiesAbstract)
  protected
    fKeepAliveMS: cardinal;
    fUri: TUri;
    function GetServer: RawByteString;
      {$ifdef HASINLINE}inline;{$endif}
    function GetPort: RawByteString;
      {$ifdef HASINLINE}inline;{$endif}
    /// you could inherit from it and set your custom fProtocol instance
    procedure SetInternalProperties; override;
    procedure SetServerName(const aServerName: RawUtf8);
    // this overriden method will just call InternalRequest
    procedure ProcessMessage(const Input: RawUtf8; out Output: RawUtf8); override;
    /// to be overriden to process low-level HTTP/1.1 request
    function InternalRequest(var Data, DataType: RawByteString): integer; virtual; abstract;
  published
    /// the associated server IP address or name
    property Server: RawByteString
      read GetServer;
    /// the associated port number
    property Port: RawByteString
      read GetPort;
    /// time (in milliseconds) to keep the connection alive with the server
    // - default is 60000, i.e. one minute
    property KeepAliveMS: cardinal
      read fKeepAliveMS write fKeepAliveMS;
  end;

  /// implements a HTTP client via sockets, able to access remotely any mormot.db.sql
  TSqlDBSocketConnectionProperties = class(TSqlDBHttpConnectionPropertiesAbstract)
  protected
    fSocket: THttpClientSocket;
    function InternalRequest(var Data, DataType: RawByteString): integer; override;
  public
    /// initialize the properties for remote access via HTTP using sockets
    // - aServerName should be the HTTP server address as 'server:port'
    // - aDatabaseName would be used to compute the URI as in TSqlDBServerAbstract
    // - the user/password credential should match server-side authentication
    constructor Create(const aServerName,aDatabaseName, aUserID,aPassWord: RawUtf8); override;
    /// released used memory
    destructor Destroy; override;
    /// low-level direct access to the Socket implementation instance
    property Socket: THttpClientSocket
      read fSocket;
  end;


  /// implements an abstract HTTP client via THttpRequest abstract class,
  // able to access remotely any mormot.db.sql
  // - never instantiate this class, but rather TSqlDBWinHttpConnectionProperties
  // or TSqlDBWinINetConnectionProperties
  TSqlDBHttpRequestConnectionProperties = class(TSqlDBHttpConnectionPropertiesAbstract)
  protected
    fClient: THttpRequest;
    function InternalRequest(var Data, DataType: RawByteString): integer; override;
  public
    /// released used memory
    destructor Destroy; override;
    /// low-level direct access to the WinHttp implementation instance
    property Client: THttpRequest
      read fClient;
  end;

  {$ifdef USELIBCURL}

  /// implements a HTTP client via the libcurl API, able to access remotely
  // any mormot.db.sql
  TSqlDBCurlConnectionProperties = class(TSqlDBHttpRequestConnectionProperties)
  public
    /// initialize the properties for remote access via HTTP using libcurl
    // - aServerName should be the HTTP server address as 'server:port'
    // - aDatabaseName would be used to compute the URI as in TSqlDBServerAbstract
    // - the user/password credential should match server-side authentication
    constructor Create(const aServerName,aDatabaseName, aUserID,aPassWord: RawUtf8); override;
  end;

  {$endif USELIBCURL}

  {$ifdef USEWININET}

  /// implements a HTTP client via WinHttp API, able to access remotely
  // any mormot.db.sql
  TSqlDBWinHttpConnectionProperties = class(TSqlDBHttpRequestConnectionProperties)
  public
    /// initialize the properties for remote access via HTTP using WinHttp
    // - aServerName should be the HTTP server address as 'server:port'
    // - aDatabaseName would be used to compute the URI as in TSqlDBServerAbstract
    // - the user/password credential should match server-side authentication
    constructor Create(const aServerName,aDatabaseName, aUserID,aPassWord: RawUtf8); override;
  end;

  /// implements a HTTP client via WinINet API, able to access remotely
  // any mormot.db.sql
  TSqlDBWinINetConnectionProperties = class(TSqlDBHttpRequestConnectionProperties)
  public
    /// initialize the properties for remote access via HTTP using WinINet
    // - aServerName should be the HTTP server address as 'server:port'
    // - aDatabaseName would be used to compute the URI as in TSqlDBServerAbstract
    // - the user/password credential should match server-side authentication
    constructor Create(const aServerName,aDatabaseName, aUserID,aPassWord: RawUtf8); override;
  end;

  {$endif USEWININET}



implementation


{ ************ Shared Proxy Information }

function ToText(cmd: TSqlDBProxyConnectionCommand): PShortString;
begin
  result := GetEnumName(TypeInfo(TSqlDBProxyConnectionCommand), ord(cmd));
end;


{ ************ Server-Side Proxy Remote Protocol }

const
  REMOTE_MAGIC = 1;

type
  TRemoteMessageHeader = packed record
    Magic: byte;
    SessionID: integer;
    Command: TSqlDBProxyConnectionCommand;
  end;
  PRemoteMessageHeader = ^TRemoteMessageHeader;

constructor TSqlDBProxyConnectionProtocol.Create(
  aAuthenticate: TSynAuthenticationAbstract);
begin
  fAuthenticate := aAuthenticate;
  fTransactionRetryTimeout := 100;
  fTransactionActiveTimeout := 120000; // after 2 minutes, clear any transaction
  InitializeCriticalSection(fLock);
end;

function TSqlDBProxyConnectionProtocol.GetAuthenticate: TSynAuthenticationAbstract;
begin
  if self = nil then
    result := nil
  else
    result := fAuthenticate;
end;

function TSqlDBProxyConnectionProtocol.HandleInput(
  const input: RawByteString): RawByteString;
begin
  result := input;
end;

function TSqlDBProxyConnectionProtocol.HandleOutput(
  const output: RawByteString): RawByteString;
begin
  result := output;
end;

function TSqlDBProxyConnectionProtocol.TransactionStarted(
  connection: TSqlDBConnection; sessionID: integer): boolean;
var
  tixend, tix: Int64;
begin
  if sessionID = 0 then
    raise ESqlDBRemote.CreateUtf8(
      '%.TransactionStarted: Remote transaction expects authentication/session',
      [self]);
  if connection.Properties.InheritsFrom(TSqlDBConnectionPropertiesThreadSafe) and
     (TSqlDBConnectionPropertiesThreadSafe(connection.Properties).
       ThreadingMode = tmThreadPool) then
    raise ESqlDBRemote.CreateUtf8(
      '%.TransactionStarted: Remote transaction expects %.ThreadingMode<>tmThreadPool: ' +
      'commit/execute/rollback should be in the same thread/connection',
      [self, connection.Properties]);
  tix := GetTickCount64;
  tixend := tix + fTransactionRetryTimeout;
  repeat
    EnterCriticalSection(fLock);
    try
      if (fTransactionActiveAutoReleaseTicks <> 0) and
         (tix > fTransactionActiveAutoReleaseTicks) then
        try
          connection.Rollback;
        finally
          fTransactionSessionID := 0;
          fTransactionActiveAutoReleaseTicks := 0;
        end;
      result := fTransactionSessionID = 0;
      if result then
      begin
        fTransactionSessionID := sessionID;
        fTransactionActiveAutoReleaseTicks := tix + fTransactionActiveTimeout;
        connection.StartTransaction;
      end;
    finally
      LeaveCriticalSection(fLock);
    end;
    if result or
       (tix > tixend) then
      break;
    SleepHiRes(1);
    tix := GetTickCount64;
  until tix > tixend;
end;

procedure TSqlDBProxyConnectionProtocol.TransactionEnd(sessionID: integer);
begin
  if sessionID = 0 then
    raise ESqlDBRemote.CreateUtf8(
      '%: Remote transaction expects authentication/session', [self]);
  EnterCriticalSection(fLock);
  try
    if sessionID <> fTransactionSessionID then
      raise ESqlDBRemote.CreateUtf8('Invalid %.TransactionEnd(%) - expected %',
        [self, sessionID, fTransactionSessionID]);
    fTransactionSessionID := 0;
    fTransactionActiveAutoReleaseTicks := 0;
  finally
    LeaveCriticalSection(fLock);
  end;
end;

destructor TSqlDBProxyConnectionProtocol.Destroy;
begin
  fAuthenticate.Free;
  DeleteCriticalSection(fLock);
  inherited Destroy;
end;

function TSqlDBRemoteConnectionProtocol.HandleInput(
  const input: RawByteString): RawByteString;
begin
  result := input;
  SymmetricEncrypt(REMOTE_MAGIC, result);
  result := AlgoSynLZ.Decompress(result); // also check crc32c
end;

function TSqlDBRemoteConnectionProtocol.HandleOutput(
  const output: RawByteString): RawByteString;
begin
  result := AlgoSynLZ.Compress(output); // includes cr32c hashing
  SymmetricEncrypt(REMOTE_MAGIC, result);
end;

procedure TSqlDBProxyConnectionProtocol.RemoteProcessMessage(
  const Input: RawUtf8; out Output: RawUtf8; Connection: TSqlDBConnection);
var
  stmt: ISqlDBStatement;
  data: TRawByteStringStream;
  msgin, msgout: RawUtf8;
  header: PRemoteMessageHeader;
  P: PAnsiChar;
  i, session: integer;
  user: RawUtf8;
  exec: TSqlDBProxyConnectionCommandExecute;
  execwithres: boolean;
  colarr: TSqlDBColumnDefineDynArray;
  defarr: TSqlDBIndexDefineDynArray;
  outarr: TRawUtf8DynArray;

  procedure AppendOutput(value: Int64);
  var
    len: PtrInt;
  begin
    len := Length(msgout);
    SetLength(msgout, len + SizeOf(Int64));
    PInt64(@PByteArray(msgout)[len])^ := value;
  end;

begin
  // follow TSqlDBRemoteConnectionPropertiesAbstract.Process binary layout
  if self = nil then
    raise ESqlDBRemote.Create('RemoteProcessMessage: unexpected self=nil');
  if Connection = nil then
    raise ESqlDBRemote.CreateUtf8(
      '%.RemoteProcessMessage(connection=nil)', [self]);
  msgin := HandleInput(Input);
  header := pointer(msgin);
  if (header = nil) or
     (header.Magic <> REMOTE_MAGIC) then
    raise ESqlDBRemote.CreateUtf8(
      'Incorrect %.RemoteProcessMessage() input magic/version', [self]);
  if (Authenticate <> nil) and
     (Authenticate.UsersCount > 0) and
     not (header.Command in [cGetToken, cGetDbms]) then
    if not Authenticate.SessionExists(header.SessionID) then
      raise ESqlDBRemote.Create('You do not have the right to be here');
  P := pointer(msgin);
  inc(P, SizeOf(header^));
  try
    FastSetString(msgout, pointer(msgin), SizeOf(header^));
    case header.Command of
      cGetToken:
        AppendOutput(Authenticate.CurrentToken);
      cGetDbms:
        begin
          session := 0;
          if (Authenticate <> nil) and
             (Authenticate.UsersCount > 0) then
          begin
            GetNextItem(PUtf8Char(P), #1, user);
            session := Authenticate.CreateSession(user, PCardinal(P)^);
            if session = 0 then
              raise ESqlDBRemote.CreateUtf8('%.RemoteProcessMessage: ' +
                'CreateSession failed - check connection and User/Password',
                [self]);
          end;
          PRemoteMessageHeader(msgout)^.sessionID := session;
          AppendCharToRawUtf8(msgout, AnsiChar(Connection.Properties.Dbms));
        end;
      cConnect:
        Connection.Connect;
      cDisconnect:
        Connection.Disconnect;
      cTryStartTransaction:
        AppendCharToRawUtf8(msgout,
          AnsiChar(TransactionStarted(Connection, header.SessionID)));
      cCommit:
        begin
          TransactionEnd(header.SessionID);
          Connection.Commit;
        end;
      cRollback:
        begin
          TransactionEnd(header.SessionID);
          Connection.Rollback;
        end;
      cServerTimestamp:
        AppendOutput(Connection.ServerTimestamp);
      cGetFields:
        begin
          Connection.Properties.GetFields(P, colarr);
          AppendToRawUtf8(msgout, DynArraySave(
            colarr, TypeInfo(TSqlDBColumnDefineDynArray)));
        end;
      cGetIndexes:
        begin
          Connection.Properties.GetIndexes(P, defarr);
          AppendToRawUtf8(msgout, DynArraySave(
            defarr, TypeInfo(TSqlDBIndexDefineDynArray)));
        end;
      cGetTableNames:
        begin
          Connection.Properties.GetTableNames(outarr);
          AppendToRawUtf8(msgout, DynArraySave(
            outarr, TypeInfo(TRawUtf8DynArray)));
        end;
      cGetForeignKeys:
        begin
          Connection.Properties.GetForeignKey('', ''); // ensure Dest.fForeignKeys exists
          AppendToRawUtf8(msgout, Connection.Properties.ForeignKeysData);
        end;
      cExecute,
      cExecuteToBinary,
      cExecuteToJson,
      cExecuteToExpandedJson:
        begin
          RecordLoad(exec, P, TypeInfo(TSqlDBProxyConnectionCommandExecute));
          execwithres := header.Command <> cExecute;
          stmt := Connection.NewStatementPrepared(exec.SQL,
            execwithres, true);
          if fBlobAsNull in exec.Force then
            stmt.ForceBlobAsNull := true;
          if fDateWithMS in exec.Force then
            stmt.ForceDateWithMS := true;
          for i := 1 to Length(exec.Params) do
            with exec.Params[i - 1] do
              if exec.ArrayCount = 0 then
                case VType of
                  ftNull:
                    stmt.BindNull(i, VInOut);
                  ftInt64:
                    stmt.Bind(i, VInt64, VInOut);
                  ftDouble:
                    stmt.Bind(i, unaligned(PDouble(@VInt64)^), VInOut);
                  ftCurrency:
                    stmt.Bind(i, PCurrency(@VInt64)^, VInOut);
                  ftDate:
                    stmt.BindDateTime(i, PDateTime(@VInt64)^, VInOut);
                  ftUtf8:
                    stmt.BindTextU(i, VData, VInOut);
                  ftBlob:
                    stmt.BindBlob(i, VData, VInOut);
                else
                  raise ESqlDBRemote.CreateUtf8(
                    'Invalid VType=% parameter #% in %.ProcessExec(%)',
                    [ord(VType), i, self, ToText(header.Command)^]);
                end
              else
                stmt.BindArray(i, VType, VArray, exec.ArrayCount);
          stmt.ExecutePrepared;
          if execwithres then
          begin
            data := TRawByteStringStream.Create(msgout);
            try
              data.Seek(0, soEnd); // include header
              case header.Command of
                cExecuteToBinary:
                  stmt.FetchAllToBinary(data);
                cExecuteToJson:
                  stmt.FetchAllToJson(data, false);
                cExecuteToExpandedJson:
                  stmt.FetchAllToJson(data, true);
              end;
              msgout := data.DataString;
            finally
              data.Free;
            end;
          end
          else if not (fNoUpdateCount in exec.Force) then
            AppendToRawUtf8(msgout, UInt32ToUtf8(stmt.UpdateCount));
        end;
      cQuit:
        begin
          if header.SessionID = fTransactionSessionID then
            TransactionEnd(header.SessionID);
          Authenticate.RemoveSession(header.SessionID);
        end;
    else
      raise ESqlDBRemote.CreateUtf8(
        'Unknown %.RemoteProcessMessage() command %',
        [self, ord(header.Command)]);
    end;
  except
    on E: Exception do
    begin
      PRemoteMessageHeader(msgout)^.Command := cExceptionRaised;
      AppendToRawUtf8(msgout, StringToUtf8(E.ClassName + #0 + E.Message));
    end;
  end;
  Output := HandleOutput(msgout);
end;


{ ************ Client-Side Proxy Remote Protocol }

{ TSqlDBProxyConnectionPropertiesAbstract }

procedure TSqlDBProxyConnectionPropertiesAbstract.SetInternalProperties;
var
  credential: RawUtf8;
  token: Int64;
begin
  if fStartTransactionTimeOut = 0 then
    fStartTransactionTimeOut := 2000;
  if fProtocol = nil then
    // override this method and set fProtocol before calling inherited
    fProtocol := TSqlDBProxyConnectionProtocol.Create(nil);
  Process(cGetToken, self, token);
  SetLength(credential, 4);
  PCardinal(credential)^ := fProtocol.Authenticate.ComputeHash(
    token, UserID, PassWord);
  credential := UserID + #1 + credential;
  fCurrentSession := Process(cGetDbms, credential, fDbms);
end;

destructor TSqlDBProxyConnectionPropertiesAbstract.Destroy;
begin
  try
    inherited Destroy;
    Process(cQuit, self, self);
  finally
    fProtocol.Free;
  end;
end;

procedure TSqlDBProxyConnectionPropertiesAbstract.GetForeignKeys;
begin
  Process(cGetForeignKeys, self, fForeignKeys);
end;

function TSqlDBProxyConnectionPropertiesAbstract.NewConnection: TSqlDBConnection;
begin
  result := TSqlDBProxyConnection.Create(self);
  TSqlDBProxyConnection(result).InternalProcess(speCreated);
end;

procedure TSqlDBProxyConnectionPropertiesAbstract.GetFields(
  const aTableName: RawUtf8; out Fields: TSqlDBColumnDefineDynArray);
begin
  Process(cGetFields, aTableName, Fields);
end;

procedure TSqlDBProxyConnectionPropertiesAbstract.GetIndexes(
  const aTableName: RawUtf8; out Indexes: TSqlDBIndexDefineDynArray);
begin
  Process(cGetIndexes, aTableName, Indexes);
end;

procedure TSqlDBProxyConnectionPropertiesAbstract.GetTableNames(
  out Tables: TRawUtf8DynArray);
begin
  Process(cGetTableNames, self, Tables);
end;

function TSqlDBProxyConnectionPropertiesAbstract.IsCachable(P: PUtf8Char): boolean;
begin
  result := False;
end;


{ TSqlDBRemoteConnectionPropertiesAbstract }

function TSqlDBRemoteConnectionPropertiesAbstract.Process(
  Command: TSqlDBProxyConnectionCommand; const Input; var Output): integer;
var
  msgin, msgout, msgRaw: RawUtf8;
  header: TRemoteMessageHeader;
  outheader: PRemoteMessageHeader;
  intext: RawUtf8                             absolute Input;
  inexec: TSqlDBProxyConnectionCommandExecute absolute Input;
  msg: PAnsiChar;
  outdef: TSqlDBDefinition                    absolute Output;
  outint64: Int64                             absolute Output;
  outboolean: boolean                         absolute Output;
  outcolarr: TSqlDBColumnDefineDynArray       absolute Output;
  outindexarr: TSqlDBIndexDefineDynArray      absolute Output;
  outarr: TRawUtf8DynArray                    absolute Output;
  oututf8: RawUtf8                            absolute Output;
  outnamevalue: TSynNameValue                 absolute Output;
begin
  // use our optimized RecordLoadSave/DynArrayLoadSave binary serialization
  header.Magic := REMOTE_MAGIC;
  header.SessionID := fCurrentSession;
  header.Command := Command;
  FastSetString(msgin, @header, SizeOf(header));
  case Command of
    cGetToken,
    cConnect,
    cDisconnect,
    cTryStartTransaction,
    cCommit,
    cRollback,
    cServerTimestamp,
    cGetTableNames,
    cGetForeignKeys,
    cQuit:
      ; // no input parameters here, just the command
    cGetDbms,
    cGetFields,
    cGetIndexes:
      AppendToRawUtf8(msgin, intext);
    cExecute,
    cExecuteToBinary,
    cExecuteToJson,
    cExecuteToExpandedJson:
      AppendToRawUtf8(msgin,
        RecordSave(inexec, TypeInfo(TSqlDBProxyConnectionCommandExecute)));
  else
    raise ESqlDBRemote.CreateUtf8('Unknown %.Process() input command % (%)',
      [self, ToText(Command)^, ord(Command)]);
  end;
  ProcessMessage(fProtocol.HandleOutput(msgin), msgRaw);
  msgout := fProtocol.HandleInput(msgRaw);
  outheader := pointer(msgout);
  if (outheader = nil) or
     (outheader.Magic <> REMOTE_MAGIC) then
    raise ESqlDBRemote.CreateUtf8('Incorrect %.Process() magic/version', [self]);
  msg := pointer(msgout);
  inc(msg, SizeOf(header));
  case outheader.Command of
    cGetToken,
    cServerTimestamp:
      outint64 := PInt64(msg)^;
    cGetDbms:
      outdef := TSqlDBDefinition(msg^);
    cConnect,
    cDisconnect,
    cCommit,
    cRollback,
    cQuit:
      ; // no output parameters here
    cTryStartTransaction:
      outboolean := boolean(msg^);
    cGetFields:
      DynArrayLoad(outcolarr, msg, TypeInfo(TSqlDBColumnDefineDynArray));
    cGetIndexes:
      DynArrayLoad(outindexarr, msg, TypeInfo(TSqlDBIndexDefineDynArray));
    cGetTableNames:
      DynArrayLoad(outarr, msg, TypeInfo(TRawUtf8DynArray));
    cGetForeignKeys:
      outnamevalue.SetBlobDataPtr(msg);
    cExecute,
    cExecuteToBinary,
    cExecuteToJson,
    cExecuteToExpandedJson:
      FastSetString(oututf8, msg, length(msgout) - SizeOf(header));
    cExceptionRaised: // msgout is ExceptionClassName+#0+ExceptionMessage
      raise ESqlDBRemote.CreateUtf8('%.Process(%): server raised % with ''%''',
        [self, ToText(Command)^, msg, msg + StrLen(msg) + 1]);
  else
    raise ESqlDBRemote.CreateUtf8('Unknown %.Process() output command % (%)',
      [self, ToText(outheader.Command)^, ord(outheader.Command)]);
  end;
  result := outheader.SessionID;
end;


{ TSqlDBRemoteConnectionPropertiesTest }

constructor TSqlDBRemoteConnectionPropertiesTest.Create(
  aProps: TSqlDBConnectionProperties; const aUserID, aPassword: RawUtf8;
  aProtocol: TSqlDBProxyConnectionProtocolClass);
begin
  fProps := aProps;
  fProtocol := aProtocol.Create(TSynAuthentication.Create(aUserID, aPassword));
  inherited Create('', '', aUserID, aPassword);
end;

procedure TSqlDBRemoteConnectionPropertiesTest.ProcessMessage(
  const Input: RawUtf8; out Output: RawUtf8);
begin
  fProtocol.RemoteProcessMessage(Input, Output, fProps.ThreadSafeConnection);
end;


{ TSqlDBProxyConnection }

constructor TSqlDBProxyConnection.Create(aProperties: TSqlDBConnectionProperties);
begin
  fProxy := aProperties as TSqlDBProxyConnectionPropertiesAbstract;
  inherited Create(aProperties);
end;

procedure TSqlDBProxyConnection.Commit;
begin
  inherited Commit; // dec(fTransactionCount)
  try
    fProxy.Process(cCommit, self, self);
  except
    inc(fTransactionCount); // the transaction is still active
    raise;
  end;
end;

procedure TSqlDBProxyConnection.Connect;
begin
  inherited Connect;
  if fProxy.HandleConnection then
    fProxy.Process(cConnect, self, self);
  fConnected := true;
end;

procedure TSqlDBProxyConnection.Disconnect;
begin
  inherited Disconnect;
  if fProxy.HandleConnection then
    fProxy.Process(cDisconnect, self, self);
  fConnected := false;
end;

function TSqlDBProxyConnection.GetServerDateTime: TDateTime;
var
  timestamp: TTimeLogBits;
begin
  fProxy.Process(cServerTimestamp, self, timestamp);
  result := timestamp.ToDateTime;
end;

function TSqlDBProxyConnection.IsConnected: boolean;
begin
  result := fConnected;
end;

function TSqlDBProxyConnection.NewStatement: TSqlDBStatement;
begin
  // always create a new proxy statement instance (cached on remote side)
  result := TSqlDBProxyStatement.Create(self);
end;

procedure TSqlDBProxyConnection.Rollback;
begin
  inherited Rollback;
  fProxy.Process(cRollback, self, self);
end;

procedure TSqlDBProxyConnection.StartTransaction;
var
  started: boolean;
  endtix: Int64;
begin
  inherited StartTransaction;
  started := false;
  endtix := GetTickCount64 + fProxy.StartTransactionTimeOut;
  repeat
    fProxy.Process(cTryStartTransaction, self, started);
    if started or
       (GetTickCount64 > endtix) then
      break;
    SleepHiRes(10); // retry every 10 ms
  until false;
  if not started then
  begin
    inherited Rollback; // dec(fTransactionCount)
    raise ESqlDBRemote.CreateUtf8('Reached %("%/%").StartTransactionTimeOut=% ms',
      [self, fProxy.ServerName, fProxy.DatabaseName, fProxy.StartTransactionTimeOut]);
  end;
end;


{ TSqlDBProxyStatementAbstract }

procedure TSqlDBProxyStatementAbstract.InternalHeaderProcess(Data: PByte; DataLen: PtrInt);
var
  magic, F, colcount: integer;
  prop: PSqlDBColumnProperty;
begin
  fDataCurrentRowValuesStart := nil;
  fDataCurrentRowValuesSize := 0;
  fDataCurrentRowNull := nil;
  fDataCurrentRowNullLen := 0;
  repeat
    if DataLen <= 5 then
      break; // to raise ESqlDBRemote
    fDataRowCount := PInteger(PAnsiChar(Data) + (DataLen - SizeOf(integer)))^;
    magic := FromVarUInt32(Data);
    if magic <> FETCHALLTOBINARY_MAGIC then
      break; // corrupted
    colcount := FromVarUInt32(Data);
    SetLength(fDataCurrentRowColTypes, colcount);
    SetLength(fDataCurrentRowValues, colcount);
    fColumn.Capacity := colcount;
    for F := 0 to colcount - 1 do
    begin
      prop := fColumn.AddAndMakeUniqueName(FromVarString(Data));
      prop^.ColumnType := TSqlDBFieldType(Data^);
      inc(Data);
      prop^.ColumnValueDBSize := FromVarUInt32(Data);
      fDataCurrentRowColTypes[F] := prop^.ColumnType;
    end;
    if fColumnCount = 0 then
      exit; // no data returned
    if cardinal(fDataRowCount) >= cardinal(DataLen) then
      break; // obviously truncated
    fDataRowReaderOrigin := Data;
    fDataRowReader := Data;
    fDataRowNullSize := ((fColumnCount - 1) shr 3) + 1;
    SetLength(fDataCurrentRowNull, fDataRowNullSize);
    exit; // success
  until false;
  // raise ESqlDBRemote on invalid input
  fDataRowCount := 0;
  fColumnCount := 0;
  raise ESqlDBRemote.CreateUtf8('Invalid %.InternalHeaderProcess', [self]);
end;

procedure TSqlDBProxyStatementAbstract.InternalFillDataCurrent(
  var Reader: PByte; IgnoreColumnDataSize: boolean);
var
  F, len: PtrInt;
  ft: TSqlDBFieldType;
begin
  // match TSqlDBStatement.FetchAllToBinary() format
  if fDataCurrentRowNullLen > 0 then
    FillCharFast(fDataCurrentRowNull[0], fDataCurrentRowNullLen, 0);
  fDataCurrentRowNullLen := FromVarUInt32(Reader);
  if fDataCurrentRowNullLen > fDataRowNullSize then
    raise ESqlDBRemote.CreateUtf8(
      '%.InternalFillDataCurrent: Invalid rownull %>%',
      [self, fDataCurrentRowNullLen, fDataRowNullSize]);
  if fDataCurrentRowNullLen > 0 then
  begin
    MoveFast(Reader^, fDataCurrentRowNull[0], fDataCurrentRowNullLen);
    inc(Reader, fDataCurrentRowNullLen);
  end;
  fDataCurrentRowValuesStart := Reader;
  for F := 0 to fColumnCount - 1 do
    if GetBitPtr(pointer(fDataCurrentRowNull), F) then
      fDataCurrentRowValues[F] := nil
    else
    begin
      ft := fColumns[F].ColumnType;
      if ft < ftInt64 then
      begin
        // per-row column type (SQLite3 only)
        ft := TSqlDBFieldType(Reader^);
        inc(Reader);
      end;
      fDataCurrentRowColTypes[F] := ft;
      fDataCurrentRowValues[F] := Reader;
      case ft of
        ftInt64:
          Reader := GotoNextVarInt(Reader);
        ftDouble,
        ftCurrency,
        ftDate:
          inc(Reader, SizeOf(Int64));
        ftUtf8,
        ftBlob:
          begin
            len := FromVarUInt32(Reader);
            if not IgnoreColumnDataSize then
              if len > fColumns[F].ColumnDataSize then
                fColumns[F].ColumnDataSize := len;
            inc(Reader, len); // jump string/blob content
          end;
      else
        raise ESqlDBRemote.CreateUtf8(
          '%.InternalFillDataCurrent: Invalid ColumnType(%)=%',
          [self, fColumns[F].ColumnName, ord(ft)]);
      end;
    end;
  fDataCurrentRowValuesSize :=
    PtrUInt(Reader) - PtrUInt(fDataCurrentRowValuesStart);
end;

procedure TSqlDBProxyStatementAbstract.ColumnsToJson(WR: TResultsWriter);
var
  col, len: PtrInt;
  data: PByte;
begin
  if WR.Expand then
    WR.Add('{');
  for col := 0 to fColumnCount - 1 do
  begin
    if WR.Expand then
      WR.AddFieldName(fColumns[col].ColumnName); // add '"ColumnName":'
    data := fDataCurrentRowValues[col];
    if data = nil then
      WR.AddNull
    else
      case fDataCurrentRowColTypes[col] of
        ftInt64:
          WR.Add(FromVarInt64Value(data));
        ftDouble:
          WR.AddDouble(unaligned(PDouble(data)^));
        ftCurrency:
          WR.AddCurr64(PInt64(data));
        ftDate:
          begin
            WR.Add('"');
            WR.AddDateTime(PDateTime(data)^);
            WR.Add('"');
          end;
        ftUtf8:
          begin
            WR.Add('"');
            len := FromVarUInt32(data);
            WR.AddJsonEscape(data, len);
            WR.Add('"');
          end;
        ftBlob:
          if fForceBlobAsNull then
            WR.AddNull
          else
          begin
            len := FromVarUInt32(data);
            WR.WrBase64(PAnsiChar(data), len, {withMagic=}true);
          end;
      end;
    WR.AddComma;
  end;
  WR.CancelLastComma;
  if WR.Expand then
    WR.Add('}');
end;

procedure TSqlDBProxyStatementAbstract.ColumnsToBinary(W: TBufferWriter;
  Null: pointer; const ColTypes: TSqlDBFieldTypeDynArray);
begin
  // just transmit the existing raw binary, which is in the expected format
  W.Write(fDataCurrentRowValuesStart, fDataCurrentRowValuesSize);
end;

function TSqlDBProxyStatementAbstract.ColumnData(Col: integer): pointer;
begin
  if (fDataCurrentRowValues <> nil) and
     (cardinal(Col) < cardinal(fColumnCount)) then
    result := fDataCurrentRowValues[Col]
  else
    result := nil;
end;

function TSqlDBProxyStatementAbstract.ColumnType(Col: integer;
  FieldSize: PInteger): TSqlDBFieldType;
begin
  if (fDataRowCount > 0) and
     (cardinal(Col) < cardinal(fColumnCount)) then
    if GetBitPtr(pointer(fDataCurrentRowNull), Col) then
      result := ftNull
    else
      with fColumns[Col] do
      begin
        if FieldSize <> nil then
          FieldSize^ := ColumnDataSize; // true max size as computed at loading
        result := fDataCurrentRowColTypes[Col]; // per-row column type (SQLite3)
      end
  else
    raise ESqlDBRemote.CreateUtf8('Invalid %.ColumnType()', [self]);
end;

function TSqlDBProxyStatementAbstract.InternalColumnType(Col: integer;
  out Data: PByte): TSqlDBFieldType;
begin
  if (cardinal(Col) >= cardinal(fColumnCount)) or
     (fDataCurrentRowValues = nil) then
    result := ftUnknown
  else
  begin
    Data := fDataCurrentRowValues[Col];
    if Data = nil then
      result := ftNull
    else
      result := fDataCurrentRowColTypes[Col]; // per-row column type (SQLite3)
  end;
end;

function TSqlDBProxyStatementAbstract.ColumnCurrency(Col: integer): currency;
var
  data: PByte;
begin
  case InternalColumnType(Col, data) of
    ftNull:
      result := 0;
    ftInt64:
      result := FromVarInt64Value(data{%H-});
    ftDouble,
    ftDate:
      result := unaligned(PDouble(data)^);
    ftCurrency:
      result := PCurrency(data)^;
  else
    raise ESqlDBRemote.CreateUtf8('%.ColumnCurrency()', [self]);
  end;
end;

function TSqlDBProxyStatementAbstract.ColumnDateTime(Col: integer): TDateTime;
var
  data: PByte;
begin
  case InternalColumnType(Col, data) of
    ftNull:
      result := 0;
    ftInt64:
      result := FromVarInt64Value(data{%H-});
    ftDouble,
    ftDate:
      result := unaligned(PDouble(data)^);
    ftUtf8:
      with FromVarBlob(data) do
        result := Iso8601ToDateTimePUtf8Char(PUtf8Char(Ptr), len);
  else
    raise ESqlDBRemote.CreateUtf8('%.ColumnDateTime()', [self]);
  end;
end;

function TSqlDBProxyStatementAbstract.ColumnDouble(Col: integer): double;
var
  data: PByte;
begin
  case InternalColumnType(Col, data) of
    ftNull:
      result := 0;
    ftInt64:
      result := FromVarInt64Value(data{%H-});
    ftDouble,
    ftDate:
      result := unaligned(PDouble(data)^);
    ftCurrency:
      result := PCurrency(data)^;
  else
    raise ESqlDBRemote.CreateUtf8('%.ColumnDouble()', [self]);
  end;
end;

function TSqlDBProxyStatementAbstract.ColumnInt(Col: integer): Int64;
var
  data: PByte;
begin
  case InternalColumnType(Col, data) of
    ftNull:
      result := 0;
    ftInt64:
      result := FromVarInt64Value(data{%H-});
    ftDouble,
    ftDate:
      result := Trunc(unaligned(PDouble(data)^));
    ftCurrency:
      result := PInt64(data)^ div 10000;
  else
    raise ESqlDBRemote.CreateUtf8('%.ColumnInt()', [self]);
  end;
end;

function TSqlDBProxyStatementAbstract.ColumnNull(Col: integer): boolean;
begin
  result := (cardinal(Col) >= cardinal(fColumnCount)) or
            GetBitPtr(pointer(fDataCurrentRowNull), Col);
end;

function TSqlDBProxyStatementAbstract.ColumnBlob(Col: integer): RawByteString;
var
  data: PByte;
begin
  case InternalColumnType(Col, data) of
    ftNull:
      result := '';
    ftDouble,
    ftCurrency,
    ftDate:
      FastSetRawByteString(result, {%H-}data, SizeOf(Int64));
    ftBlob,
    ftUtf8:
      with FromVarBlob(data) do
        FastSetRawByteString(result, Ptr, len);
  else
    raise ESqlDBRemote.CreateUtf8('%.ColumnBlob()', [self]);
  end;
end;

function TSqlDBProxyStatementAbstract.ColumnUtf8(Col: integer): RawUtf8;
var
  data: PByte;
begin
  case InternalColumnType(Col, data) of
    ftNull:
      result := '';
    ftInt64:
      result := Int64ToUtf8(FromVarInt64Value(data{%H-}));
    ftDouble:
      result := DoubleToStr(unaligned(PDouble(data)^));
    ftCurrency:
      result := Curr64ToStr(PInt64(data)^);
    ftDate:
      DateTimeToIso8601TextVar(PDateTime(data)^, 'T', result);
    ftBlob,
    ftUtf8:
      with FromVarBlob(data) do
        FastSetString(result, Ptr, len);
  else
    raise ESqlDBRemote.CreateUtf8('%.ColumnUtf8()', [self]);
  end;
end;

function TSqlDBProxyStatementAbstract.ColumnString(Col: integer): string;
var
  data: PByte;
begin
  case InternalColumnType(Col, data) of
    ftNull:
      result := '';
    ftInt64:
      result := IntToString(FromVarInt64Value(data{%H-}));
    ftDouble:
      result := DoubleToString(unaligned(PDouble(data)^));
    ftCurrency:
      result := Curr64ToString(PInt64(data)^);
    ftDate:
      DateTimeToIso8601StringVar(PDateTime(data)^, 'T', result);
    ftUtf8:
      with FromVarBlob(data) do
        Utf8DecodeToString(PUtf8Char(Ptr), len, result);
    ftBlob:
      with FromVarBlob(data) do
        SetString(result, Ptr, len shr 1);
  else
    raise ESqlDBRemote.CreateUtf8('%.ColumnString()', [self]);
  end;
end;


{ TSqlDBProxyStatement }

procedure TSqlDBProxyStatement.ParamsToCommand(
  var Input: TSqlDBProxyConnectionCommandExecute);
begin
  if (fColumnCount > 0) or
     (fDataInternalCopy <> '') then
    raise ESqlDBRemote.CreateUtf8('Invalid %.ExecutePrepared* call', [self]);
  Input.SQL := fSql;
  if length(fParams) <> fParamCount then // strip to only needed memory
    SetLength(fParams, fParamCount);
  Input.Params := fParams;
  Input.ArrayCount := fParamsArrayCount;
  if fForceBlobAsNull then
    Input.Force := [fBlobAsNull]
  else
    Input.Force := [];
  if fForceDateWithMS then
    include(Input.Force, fDateWithMS);
  if fForceNoUpdateCount then
    include(Input.Force, fNoUpdateCount);
end;

const
  EXECUTE_PREPARED_BIN: array[boolean] of TSqlDBProxyConnectionCommand = (
    cExecute,
    cExecuteToBinary);
  EXECUTE_PREPARED_JSON: array[boolean] of TSqlDBProxyConnectionCommand = (
    cExecuteToJson,
    cExecuteToExpandedJson);

procedure TSqlDBProxyStatement.ExecutePrepared;
var
  exec: TSqlDBProxyConnectionCommandExecute;
begin
  inherited ExecutePrepared; // set fConnection.fLastAccessTicks
  // execute the statement
  ParamsToCommand(exec);
  TSqlDBProxyConnectionPropertiesAbstract(fConnection.Properties).Process(
    EXECUTE_PREPARED_BIN[fExpectResults], exec, fDataInternalCopy);
  if fExpectResults then
    // retrieve columns information from TSqlDBStatement.FetchAllToBinary() format
    InternalHeaderProcess(pointer(fDataInternalCopy), Length(fDataInternalCopy))
  else
    // retrieve UpdateCount value for plain cExecute command
    fUpdateCount := GetInteger(pointer(fDataInternalCopy));
end;

function TSqlDBProxyStatement.UpdateCount: integer;
begin
  result := fUpdateCount;
end;

procedure TSqlDBProxyStatement.ExecutePreparedAndFetchAllAsJson(
  Expanded: boolean; out Json: RawUtf8; ReturnedRowCount: PPtrInt);
var
  exec: TSqlDBProxyConnectionCommandExecute;
begin
  if ReturnedRowCount <> nil then
    raise ESqlDBRemote.CreateUtf8('%.ExecutePreparedAndFetchAllAsJson() ' +
      'does not support ReturnedRowCount', [self]);
  ParamsToCommand(exec);
  TSqlDBProxyConnectionPropertiesAbstract(fConnection.Properties).Process(
    EXECUTE_PREPARED_JSON[Expanded], exec, Json);
end;

function TSqlDBProxyStatement.FetchAllToBinary(Dest: TStream;
  MaxRowCount: cardinal; DataRowPosition: PCardinalDynArray): cardinal;
begin
  if (MaxRowCount > 0) and
     (MaxRowCount < cardinal(fDataRowCount)) then
  begin
    result := inherited FetchAllToBinary(Dest, MaxRowCount, DataRowPosition);
    exit;
  end;
  Dest.WriteBuffer(pointer(fDataInternalCopy)^, Length(fDataInternalCopy));
  if DataRowPosition <> nil then
    // TSqlDBProxyStatementRandomAccess.Create() will recompute it fast enough
    DataRowPosition^ := nil;
  result := fDataRowCount;
end;

function TSqlDBProxyStatement.Step(SeekFirst: boolean): boolean;
begin
  // retrieve one row of data from TSqlDBStatement.FetchAllToBinary() format
  if SeekFirst then
    fCurrentRow := 0;
  if cardinal(fCurrentRow) >= cardinal(fDataRowCount) then
  begin
    result := false; // no data was retrieved
    exit;
  end;
  if fCurrentRow = 0 then
  begin
    fDataRowReader := fDataRowReaderOrigin;     // rewind TFileBufferReader
    fDataCurrentRowNullLen := fDataRowNullSize; // reset null
  end;
  InternalFillDataCurrent(fDataRowReader, false);
  inc(fCurrentRow);
  result := true;
end;


{ TSqlDBProxyStatementRandomAccess }

constructor TSqlDBProxyStatementRandomAccess.Create(Data: PByte; DataLen: integer;
  DataRowPosition: PCardinalDynArray; IgnoreColumnDataSize: boolean);
var
  i, f: PtrInt;
  reader: PByte;
begin
  fLastGotoRow := -1;
  inherited Create(nil);
  InternalHeaderProcess(Data, DataLen);
  reader := fDataRowReaderOrigin;
  if (DataRowPosition <> nil) and
     (DataRowPosition^ <> nil) then
  begin
    fRowData := DataRowPosition^; // fast copy-on-write
    if not IgnoreColumnDataSize then
      for f := 0 to fColumnCount - 1 do
        with fColumns[f] do
          if ColumnType in [ftUtf8, ftBlob] then
            if ColumnValueDBSize = 0 then
            begin
              // unknown size -> compute ColumnDataSize
              for i := 0 to DataRowCount - 1 do
                // parse and set ColumnDataSize
                InternalFillDataCurrent(reader, false);
              break;
            end
            else
              ColumnDataSize := ColumnValueDBSize; // use declared maximum size
  end
  else
  begin
    SetLength(fRowData, DataRowCount);
    for i := 0 to DataRowCount - 1 do
    begin
      fRowData[i] := PtrUInt(reader) - PtrUInt(fDataRowReaderOrigin);
      InternalFillDataCurrent(reader, IgnoreColumnDataSize); // set ColumnDataSize
    end;
  end;
end;

function TSqlDBProxyStatementRandomAccess.GotoRow(Index: integer;
  RaiseExceptionOnWrongIndex: boolean): boolean;
var
  reader: PByte;
begin
  result := (cardinal(Index) < cardinal(fDataRowCount)) and
            (fColumnCount > 0);
  if not result then
    if RaiseExceptionOnWrongIndex then
      raise ESqlDBRemote.CreateUtf8('Invalid %.GotoRow(%)', [self, Index])
    else
      exit;
  if fLastGotoRow <> Index then
  begin
    // compute only if changed :)
    reader := @PAnsiChar(fDataRowReaderOrigin)[fRowData[Index]];
    InternalFillDataCurrent(reader, false);
    fLastGotoRow := Index;
  end;
end;

function TSqlDBProxyStatementRandomAccess.ColumnSearch(Col: integer;
  const Value: variant; CaseInsensitive: boolean): integer;
var
  v: variant;
begin
  if cardinal(Col) < cardinal(fColumnCount) then
    for result := 1 to fDataRowCount do
      if GotoRow(result - 1) then
      begin
        ColumnToVariant(Col, v); // fast enough for client-side lookup
        if SortDynArrayVariantComp(
             TVarData(v), TVarData(Value), CaseInsensitive) = 0 then
          exit;
      end;
  result := 0;
end;

procedure TSqlDBProxyStatementRandomAccess.ExecutePrepared;
begin
  raise ESqlDBRemote.CreateUtf8('Unexpected %.ExecutePrepared', [self]);
end;

function TSqlDBProxyStatementRandomAccess.{%H-}Step(SeekFirst: boolean): boolean;
begin
  raise ESqlDBRemote.CreateUtf8('Unexpected %.Step', [self]);
end;


{ ************ HTTP Server Classes for Remote Access }

{ TSqlDBServerAbstract }

constructor TSqlDBServerAbstract.Create(aProperties: TSqlDBConnectionProperties;
  const aDatabaseName, aPort, aUserName, aPassword: RawUtf8; aHttps: boolean;
  aThreadPoolCount: integer; aProtocol: TSqlDBProxyConnectionProtocolClass;
  aThreadMode: TSqlDBConnectionPropertiesThreadSafeThreadingMode;
  aAuthenticate: TSynAuthenticationAbstract);
begin
  fProperties := aProperties;
  if fProperties.InheritsFrom(TSqlDBConnectionPropertiesThreadSafe) then
  begin
    TSqlDBConnectionPropertiesThreadSafe(fProperties).
      ThreadingMode := aThreadMode;
    if aThreadMode = tmMainConnection then
      fProcessLocked := true;
  end;
  fDatabaseName := aDatabaseName;
  fSafe.Init;
  fPort := aPort;
  fHttps := aHttps;
  fThreadPoolCount := aThreadPoolCount;
  if aProtocol = nil then
    aProtocol := TSqlDBRemoteConnectionProtocol;
  if aAuthenticate = nil then
    aAuthenticate := TSynAuthentication.Create(aUserName, aPassword);
  fProtocol := aProtocol.Create(aAuthenticate);
end;

destructor TSqlDBServerAbstract.Destroy;
begin
  inherited;
  fServer.Free;
  fProtocol.Free;
  fSafe.Done;
end;

function TSqlDBServerAbstract.Process(Ctxt: THttpServerRequestAbstract): cardinal;
var
  o: RawUtf8;
begin
  if (Ctxt.Method <> 'POST') or
     (Ctxt.InContent = '') or
     not IdemPropNameU(TrimU(Ctxt.InContentType), BINARY_CONTENT_TYPE) then
  begin
    result := HTTP_NOTFOUND;
    exit;
  end;
  try
    if fProcessLocked then
      fSafe.Lock;
    fProtocol.RemoteProcessMessage(Ctxt.InContent, o, fProperties.ThreadSafeConnection);
  finally
    if fProcessLocked then
      fSafe.UnLock;
  end;
  Ctxt.OutContent := o;
  Ctxt.OutContentType := BINARY_CONTENT_TYPE;
  result := HTTP_SUCCESS;
end;


{$ifdef USEHTTPSYS}

{ TSqlDBServerHttpApi }

constructor TSqlDBServerHttpApi.Create(aProperties: TSqlDBConnectionProperties;
  const aDatabaseName, aPort, aUserName, aPassword: RawUtf8; aHttps: boolean;
  aThreadPoolCount: integer; aProtocol: TSqlDBProxyConnectionProtocolClass;
  aThreadMode: TSqlDBConnectionPropertiesThreadSafeThreadingMode;
  aAuthenticate: TSynAuthenticationAbstract);
var
  status: integer;
begin
  inherited;
  fServer := THttpApiServer.Create('', nil, nil, '', []);
  status := THttpApiServer(fServer).AddUrl(
    fDatabaseName, fPort, fHttps, '+', true);
  if status <> NO_ERROR then
    if status = ERROR_ACCESS_DENIED then
      raise ESqlDBRemote.CreateUtf8(
        '%.Create: administrator rights needed to register URI % on port %',
        [self, fDatabaseName, fPort])
    else
      raise ESqlDBRemote.CreateUtf8(
        '%.Create: error registering URI % on port %: is not another server ' +
        'instance running on this port?', [self, fDatabaseName, fPort]);
  fServer.OnRequest := Process;
  if fThreadPoolCount > 1 then
    THttpApiServer(fServer).Clone(fThreadPoolCount - 1);
end;

{$endif USEHTTPSYS}


{ TSqlDBServerSockets }

constructor TSqlDBServerSockets.Create(aProperties: TSqlDBConnectionProperties;
  const aDatabaseName, aPort, aUserName, aPassword: RawUtf8; aHttps: boolean;
  aThreadPoolCount: integer; aProtocol: TSqlDBProxyConnectionProtocolClass;
  aThreadMode: TSqlDBConnectionPropertiesThreadSafeThreadingMode;
  aAuthenticate: TSynAuthenticationAbstract);
var
  ident: RawUtf8;
begin
  inherited;
  FormatUtf8('DBRemote %', [aDatabaseName], ident);
  // good old THttpServer will maintain one thread per connection as expected
  fServer := THttpServer.Create(aPort, nil, nil, ident, fThreadPoolCount);
  THttpServer(fServer).WaitStarted;
  fServer.OnRequest := Process;
end;



{ ************ HTTP Client Classes for Remote Access }

{ TSqlDBHttpConnectionPropertiesAbstract }

function TSqlDBHttpConnectionPropertiesAbstract.GetServer: RawByteString;
begin
  result := fUri.Server;
end;

function TSqlDBHttpConnectionPropertiesAbstract.GetPort: RawByteString;
begin
  result := fUri.Port;
end;

procedure TSqlDBHttpConnectionPropertiesAbstract.SetServerName(
  const aServerName: RawUtf8);
begin
  fKeepAliveMS := 60000;
  if not fUri.From(aServerName) then
    raise ESqlDBRemote.CreateUtf8(
      '%.Create: expect a valid URI in aServerName=[%]',
      [self, aServerName]);
  if fUri.Port = '' then
    fUri.Port := SYNDB_DEFAULT_HTTP_PORT;
end;

procedure TSqlDBHttpConnectionPropertiesAbstract.ProcessMessage(
  const Input: RawUtf8; out Output: RawUtf8);
var
  content, contenttype: RawByteString;
  status: integer;
begin
  content := Input;
  contenttype := BINARY_CONTENT_TYPE;
  status := InternalRequest(content, contenttype);
  if status <> HTTP_SUCCESS then
    raise ESqlDBRemote.CreateUtf8('%.ProcessMessage: Error % from %',
      [self, status, fUri.Uri]);
  if contenttype <> BINARY_CONTENT_TYPE then
    raise ESqlDBRemote.CreateUtf8(
      '%.ProcessMessage: Unsupported content type [%] from %',
      [self, contenttype, fUri.Uri]);
  Output := content;
end;

procedure TSqlDBHttpConnectionPropertiesAbstract.SetInternalProperties;
begin
  if fProtocol = nil then
    fProtocol := TSqlDBRemoteConnectionProtocol.Create(
      TSynAuthentication.Create(UserID, PassWord));
  inherited;
end;


{ TSqlDBSocketConnectionProperties }

constructor TSqlDBSocketConnectionProperties.Create(const aServerName,
  aDatabaseName, aUserID, aPassWord: RawUtf8);
begin
  SetServerName(aServerName);
  fSocket := THttpClientSocket.Open(Server, Port);
  inherited;
end;

destructor TSqlDBSocketConnectionProperties.Destroy;
begin
  try
    inherited;
  finally
    fSocket.Free;
  end;
end;

function TSqlDBSocketConnectionProperties.InternalRequest(
  var Data, DataType: RawByteString): integer;
begin
  result := fSocket.Request(
    fDatabaseName, 'POST', fKeepAliveMS, '', Data, DataType, false);
  Data := fSocket.Http.Content;
  DataType := fSocket.Http.ContentType;
end;


{ TSqlDBHttpRequestConnectionProperties }

destructor TSqlDBHttpRequestConnectionProperties.Destroy;
begin
  try
    inherited Destroy;
  finally
    fClient.Free;
  end;
end;

function TSqlDBHttpRequestConnectionProperties.InternalRequest(
  var Data, DataType: RawByteString): integer;
var
  input: RawByteString;
  inputtype, head: RawUtf8;
begin
  input := Data; // Data/DataType are used as output parameters
  inputtype := DataType;
  result := fClient.Request(
    fDatabaseName, 'POST', fKeepAliveMS, '', input, inputtype, head, Data);
  FindNameValue(head, HEADER_CONTENT_TYPE_UPPER, RawUtf8(DataType));
end;


{$ifdef USEWININET}

{ TSqlDBWinHttpConnectionProperties }

constructor TSqlDBWinHttpConnectionProperties.Create(
  const aServerName, aDatabaseName, aUserID, aPassWord: RawUtf8);
begin
  SetServerName(aServerName);
  fClient := TWinHttp.Create(Server, Port, fUri.Https);
  inherited;
end;

{ TSqlDBWinINetConnectionProperties }

constructor TSqlDBWinINetConnectionProperties.Create(
  const aServerName, aDatabaseName, aUserID, aPassWord: RawUtf8);
begin
  SetServerName(aServerName);
  fClient := TWinINet.Create(Server, Port, fUri.Https);
  inherited;
end;

{$endif USEWININET}

{$ifdef USELIBCURL}

{ TSqlDBCurlConnectionProperties }

constructor TSqlDBCurlConnectionProperties.Create(
  const aServerName, aDatabaseName, aUserID, aPassWord: RawUtf8);
begin
  SetServerName(aServerName);
  fClient := TCurlHttp.Create(Server, Port, fUri.Https);
  inherited;
end;

{$endif USELIBCURL}


initialization
  TSqlDBSocketConnectionProperties.RegisterClassNameForDefinition;
  {$ifdef USEWININET}
  TSqlDBWinHttpConnectionProperties.RegisterClassNameForDefinition;
  TSqlDBWinINetConnectionProperties.RegisterClassNameForDefinition;
  {$endif USEWININET}
  {$ifdef USELIBCURL}
  TSqlDBCurlConnectionProperties.RegisterClassNameForDefinition;
  {$endif USELIBCURL}
  
end.
