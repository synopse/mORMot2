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
  mormot.core.secure,
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
  // - Input is the RawUTF8 table name for most cGet* metadata commands
  // - Input is the SQL statement and associated bound parameters for cExecute,
  // cExecuteToBinary, cExecuteToJSON, and cExecuteToExpandedJSON, encoded as
  // TSqlDBProxyConnectionCommandExecute record
  // - Output is not used for cConnect, cDisconnect, cCommit, cRollback and cExecute
  // - Output is TSqlDBDefinition (i.e. DBMS type) for cInitialize
  // - Output is TTimeLog for cServerTimestamp
  // - Output is boolean for cTryStartTransaction
  // - Output is TSqlDBColumnDefineDynArray for cGetFields
  // - Output is TSqlDBIndexDefineDynArray for cGetIndexes
  // - Output is TSynNameValue (fForeignKeys) for cGetForeignKeys
  // - Output is TRawUTF8DynArray for cGetTableNames
  // - Output is RawByteString result data for cExecuteToBinary
  // - Output is UpdateCount: integer text for cExecute
  // - Output is RawUTF8 result data for cExecuteToJSON and cExecuteToExpandedJSON
  // - calls could be declared as such:
  // ! Process(cGetToken,?,result: Int64);
  // ! Process(cGetDBMS,User#1Hash: RawUTF8,fDBMS: TSqlDBDefinition);
  // ! Process(cConnect,?,?);
  // ! Process(cDisconnect,?,?);
  // ! Process(cTryStartTransaction,?,started: boolean);
  // ! Process(cCommit,?,?);
  // ! Process(cRollback,?,?);
  // ! Process(cServerTimestamp,?,result: TTimeLog);
  // ! Process(cGetFields,aTableName: RawUTF8,Fields: TSqlDBColumnDefineDynArray);
  // ! Process(cGetIndexes,aTableName: RawUTF8,Indexes: TSqlDBIndexDefineDynArray);
  // ! Process(cGetTableNames,?,Tables: TRawUTF8DynArray);
  // ! Process(cGetForeignKeys,?,fForeignKeys: TSynNameValue);
  // ! Process(cExecute,Request: TSqlDBProxyConnectionCommandExecute,UpdateCount: integer);
  // ! Process(cExecuteToBinary,Request: TSqlDBProxyConnectionCommandExecute,Data: RawByteString);
  // ! Process(cExecuteToJSON,Request: TSqlDBProxyConnectionCommandExecute,JSON: RawUTF8);
  // ! Process(cExecuteToExpandedJSON,Request: TSqlDBProxyConnectionCommandExecute,JSON: RawUTF8);
  // - cExceptionRaised is a pseudo-command, used only for sending an exception
  // to the client in case of execution problem on the server side
  TSqlDBProxyConnectionCommand = (
    cGetToken,
    cGetDBMS,
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
    cExecuteToJSON,
    cExecuteToExpandedJSON,
    cQuit,
    cExceptionRaised);

  /// server-side process flags for TSqlDBProxyConnectionCommandExecute.Force
  TSqlDBProxyConnectionCommandExecuteForce = set of (
    fBlobAsNull,
    fDateWithMS,
    fNoUpdateCount);

  /// structure to embedd all needed parameters to execute a SQL statement
  // - used for cExecute, cExecuteToBinary, cExecuteToJSON and cExecuteToExpandedJSON
  // commands of TSqlDBProxyConnectionProperties.Process()
  // - set by TSqlDBProxyStatement.ParamsToCommand() protected method
  TSqlDBProxyConnectionCommandExecute = packed record
    /// the associated SQL statement
    SQL: RawUTF8;
    /// input parameters
    // - trunked to the exact number of parameters
    Params: TSqlDBParamDynArray;
    /// if input parameters expected BindArray() process
    ArrayCount: integer;
    /// how server side would handle statement execution
    // - fBlobAsNull and fDateWithMS do match ForceBlobAsNull and ForceDateWithMS
    // ISQLDBStatement properties
    // - fNoUpdateCount avoids to call ISQLDBStatement.UpdateCount method, e.g.
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
    procedure RemoteProcessMessage(const Input: RawByteString;
      out Output: RawByteString; Connection: TSqlDBConnection); virtual;
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
    /// calls Process(cGetToken) + Process(cGetDBMS)
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
    procedure GetFields(const aTableName: RawUTF8; out Fields: TSqlDBColumnDefineDynArray); override;
    /// retrieve the advanced indexed information of a specified Table
    // - calls Process(cGetIndexes,aTableName,Indexes)
    procedure GetIndexes(const aTableName: RawUTF8; out Indexes: TSqlDBIndexDefineDynArray); override;
    /// get all table names
    // - this default implementation will use protected SQLGetTableNames virtual
    // - calls Process(cGetTableNames,self,Tables)
    procedure GetTableNames(out Tables: TRawUTF8DynArray); override;
    /// determine if the SQL statement can be cached
    // - always returns false, to force a new fake statement to be created
    function IsCachable(P: PUTF8Char): boolean; override;
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
    fDataCurrentRowIndex: integer;
    fDataCurrentRowNullLen: cardinal;
    fDataCurrentRowNull: TByteDynArray;
    fDataCurrentRowValues: array of pointer;
    fDataCurrentRowValuesStart: pointer;
    fDataCurrentRowValuesSize: cardinal;
    // per-row column type (SQLite3 only) e.g. select coalesce(column,0) from ..
    fDataCurrentRowColTypes: array of TSqlDBFieldType;
    function IntColumnType(Col: integer; out Data: PByte): TSqlDBFieldType;
      {$ifdef HASINLINE}inline;{$endif}
    procedure IntHeaderProcess(Data: PByte; DataLen: PtrInt);
    procedure IntFillDataCurrent(var Reader: PByte; IgnoreColumnDataSize: boolean);
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
    function ColumnUTF8(Col: integer): RawUTF8; override;
    /// return a Column text value as generic VCL string of the current Row, first Col is 0
    function ColumnString(Col: integer): string; override;
    /// return a Column as a blob value of the current Row, first Col is 0
    function ColumnBlob(Col: integer): RawByteString; override;
    /// return all columns values into JSON content
    procedure ColumnsToJSON(WR: TJSONWriter); override;
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
  // ExecutePreparedAndFetchAllAsJSON() method (as expected by our ORM)
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
    // the remote process, but will just set fSQL
    // - this overridden implementation will use out optimized binary format
    //  as generated by TSqlDBStatement.FetchAllToBinary(), and not JSON
    procedure ExecutePrepared; override;
    /// execute a prepared SQL statement and return all rows content as a JSON string
    // - JSON data is retrieved with UTF-8 encoding
    // - if Expanded is true, JSON data is an array of objects, for direct use
    // with any Ajax or .NET client:
    // & [ {"col1":val11,"col2":"val12"},{"col1":val21,... ]
    // - if Expanded is false, JSON data is serialized (used in TOrmTableJSON)
    // & { "FieldCount":1,"Values":["col1","col2",val11,"val12",val21,..] }
    // - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"'
    // format and contains true BLOB data
    // - this overridden implementation will use JSON for transmission, and
    // binary encoding only for parameters (to avoid unneeded conversions, e.g.
    // when called from mormot.orm.sql.pas)
    procedure ExecutePreparedAndFetchAllAsJSON(Expanded: boolean; out JSON: RawUTF8); override;
    /// append all rows content as binary stream
    // - will save the column types and name, then every data row in optimized
    // binary format (faster and smaller than JSON)
    // - you can specify a LIMIT for the data extent (default 0 meaning all data)
    // - generates the format expected by TSqlDBProxyStatement
    // - this overriden method will use the internal data copy of the binary
    // buffer retrieved by ExecutePrepared, so would be almost immediate,
    // and would allow e.g. direct consumption via our TSynSQLStatementDataSet
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
    procedure ProcessMessage(const Input: RawByteString; out Output: RawByteString);
      virtual; abstract;
  end;

  /// fake proxy class for testing the remote connection to any mormot.db.sql engine
  // - resulting overhead due to our binary messaging: unnoticeable :)
  TSqlDBRemoteConnectionPropertiesTest = class(TSqlDBRemoteConnectionPropertiesAbstract)
  protected
    fProps: TSqlDBConnectionProperties;
    // this overriden method will just call fProtocol.RemoteProcessMessage()
    procedure ProcessMessage(const Input: RawByteString; out Output: RawByteString); override;
  public
    /// create a test redirection to an existing local connection property
    // - you can specify a User/Password credential pair to also test the
    // authentication via TSynAuthentication
    constructor Create(aProps: TSqlDBConnectionProperties;
      const aUserID,aPassword: RawUTF8; aProtocol: TSqlDBProxyConnectionProtocolClass); reintroduce;
  end;


  /// implements a virtual statement with direct data access
  // - is generated with no connection, but allows direct random access to any
  // data row retrieved from TSqlDBStatement.FetchAllToBinary() binary data
  // - GotoRow() method allows direct access to a row data via Column*()
  // - is used e.g. by TSynSQLStatementDataSet of SynDBVCL unit
  TSqlDBProxyStatementRandomAccess = class(TSqlDBProxyStatementAbstract)
  protected
    fRowData: TCardinalDynArray;
  public
    /// initialize the internal structure from a given memory buffer
    // - by default, ColumnDataSize would be computed from the supplied data,
    // unless you set IgnoreColumnDataSize=true to set the value to 0 (and
    // force e.g. SynDBVCL TSynBinaryDataSet.InternalInitFieldDefs define the
    // field as ftDefaultMemo)
    constructor Create(Data: PByte; DataLen: integer;
      DataRowPosition: PCardinalDynArray = nil; IgnoreColumnDataSize: boolean = false); reintroduce;

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
    fPort, fDatabaseName: RawUTF8;
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
      const aDatabaseName: RawUTF8; const aPort: RawUTF8 = SYNDB_DEFAULT_HTTP_PORT;
      const aUserName: RawUTF8 = ''; const aPassword: RawUTF8 = '';
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
    property Port: RawUTF8
      read fPort;
    /// the associated database name
    property DatabaseName: RawUTF8
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
    constructor Create(aProperties: TSqlDBConnectionProperties;
      const aDatabaseName: RawUTF8; const aPort: RawUTF8 = SYNDB_DEFAULT_HTTP_PORT;
      const aUserName: RawUTF8 = ''; const aPassword: RawUTF8 = '';
      aHttps: boolean = false; aThreadPoolCount: integer = 1;
      aProtocol: TSqlDBProxyConnectionProtocolClass = nil;
      aThreadMode: TSqlDBConnectionPropertiesThreadSafeThreadingMode = tmMainConnection;
      aAuthenticate: TSynAuthenticationAbstract = nil); override;
  end;

  {$ifdef ONLYUSEHTTPSOCKET}

  TSqlDBServerRemote = TSqlDBServerSockets;

  {$else}

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
      const aDatabaseName: RawUTF8; const aPort: RawUTF8 = SYNDB_DEFAULT_HTTP_PORT;
      const aUserName: RawUTF8 = ''; const aPassword: RawUTF8 = '';
      aHttps: boolean = false; aThreadPoolCount: integer = 1;
      aProtocol: TSqlDBProxyConnectionProtocolClass = nil;
      aThreadMode: TSqlDBConnectionPropertiesThreadSafeThreadingMode = tmMainConnection;
      aAuthenticate: TSynAuthenticationAbstract = nil); override;
  end;

  /// the default mormot.db.proxy HTTP server class on each platform
  TSqlDBServerRemote = TSqlDBServerHttpApi;

  {$endif ONLYUSEHTTPSOCKET}


{ ************ HTTP Client Classes for Remote Access }

type
  /// implements a generic HTTP client, able to access remotely any mormot.db.sql
  // - do not instantiate this class, but rather use TSqlDBSocketConnectionProperties
  //  TSqlDBWinHTTPConnectionProperties TSqlDBWinINetConnectionProperties
  TSqlDBHTTPConnectionPropertiesAbstract = class(TSqlDBRemoteConnectionPropertiesAbstract)
  protected
    fKeepAliveMS: cardinal;
    fURI: TURI;
    function GetServer: RawByteString;
      {$ifdef HASINLINE}inline;{$endif}
    function GetPort: RawByteString;
      {$ifdef HASINLINE}inline;{$endif}
    /// you could inherit from it and set your custom fProtocol instance
    procedure SetInternalProperties; override;
    procedure SetServerName(const aServerName: RawUTF8);
    // this overriden method will just call InternalRequest
    procedure ProcessMessage(const Input: RawByteString; out Output: RawByteString); override;
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
  TSqlDBSocketConnectionProperties = class(TSqlDBHTTPConnectionPropertiesAbstract)
  protected
    fSocket: THttpClientSocket;
    function InternalRequest(var Data, DataType: RawByteString): integer; override;
  public
    /// initialize the properties for remote access via HTTP using sockets
    // - aServerName should be the HTTP server address as 'server:port'
    // - aDatabaseName would be used to compute the URI as in TSqlDBServerAbstract
    // - the user/password credential should match server-side authentication
    constructor Create(const aServerName,aDatabaseName, aUserID,aPassWord: RawUTF8); override;
    /// released used memory
    destructor Destroy; override;
    /// low-level direct access to the Socket implementation instance
    property Socket: THttpClientSocket
      read fSocket;
  end;


  /// implements an abstract HTTP client via THttpRequest abstract class,
  // able to access remotely any mormot.db.sql
  // - never instantiate this class, but rather TSqlDBWinHTTPConnectionProperties
  // or TSqlDBWinINetConnectionProperties
  TSqlDBHttpRequestConnectionProperties = class(TSqlDBHTTPConnectionPropertiesAbstract)
  protected
    fClient: THttpRequest;
    function InternalRequest(var Data, DataType: RawByteString): integer; override;
  public
    /// released used memory
    destructor Destroy; override;
    /// low-level direct access to the WinHTTP implementation instance
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
    constructor Create(const aServerName,aDatabaseName, aUserID,aPassWord: RawUTF8); override;
  end;

  {$endif USELIBCURL}

  {$ifdef USEWININET}

  /// implements a HTTP client via WinHTTP API, able to access remotely
  // any mormot.db.sql
  TSqlDBWinHTTPConnectionProperties = class(TSqlDBHttpRequestConnectionProperties)
  public
    /// initialize the properties for remote access via HTTP using WinHTTP
    // - aServerName should be the HTTP server address as 'server:port'
    // - aDatabaseName would be used to compute the URI as in TSqlDBServerAbstract
    // - the user/password credential should match server-side authentication
    constructor Create(const aServerName,aDatabaseName, aUserID,aPassWord: RawUTF8); override;
  end;

  /// implements a HTTP client via WinINet API, able to access remotely
  // any mormot.db.sql
  TSqlDBWinINetConnectionProperties = class(TSqlDBHttpRequestConnectionProperties)
  public
    /// initialize the properties for remote access via HTTP using WinINet
    // - aServerName should be the HTTP server address as 'server:port'
    // - aDatabaseName would be used to compute the URI as in TSqlDBServerAbstract
    // - the user/password credential should match server-side authentication
    constructor Create(const aServerName,aDatabaseName, aUserID,aPassWord: RawUTF8); override;
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

constructor TSqlDBProxyConnectionProtocol.Create(aAuthenticate:
  TSynAuthenticationAbstract);
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

function TSqlDBProxyConnectionProtocol.HandleInput(const input: RawByteString):
  RawByteString;
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
  endTrial, tix: Int64;
begin
  if sessionID = 0 then
    raise ESqlDBRemote.CreateUTF8('%: Remote transaction expects authentication/session', [self]);
  if connection.Properties.InheritsFrom(TSqlDBConnectionPropertiesThreadSafe) and
     (TSqlDBConnectionPropertiesThreadSafe(connection.Properties).ThreadingMode = tmThreadPool) then
    raise ESqlDBRemote.CreateUTF8('%: Remote transaction expects %.ThreadingMode<>tmThreadPool: ' +
      'commit/execute/rollback should be in the same thread/connection', [self, connection.Properties]);
  tix := GetTickCount64;
  endTrial := tix + fTransactionRetryTimeout;
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
       (tix > endTrial) then
      break;
    SleepHiRes(1);
    tix := GetTickCount64;
  until tix > endTrial;
end;

procedure TSqlDBProxyConnectionProtocol.TransactionEnd(sessionID: integer);
begin
  if sessionID = 0 then
    raise ESqlDBRemote.CreateUTF8(
      '%: Remote transaction expects authentication/session', [self]);
  EnterCriticalSection(fLock);
  try
    if sessionID <> fTransactionSessionID then
      raise ESqlDBRemote.CreateUTF8('Invalid %.TransactionEnd(%) - expected %',
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

function TSqlDBRemoteConnectionProtocol.HandleInput(const input: RawByteString):
  RawByteString;
begin
  result := input;
  SymmetricEncrypt(REMOTE_MAGIC, result);
  result := AlgoSynLZ.Decompress(result);
end;

function TSqlDBRemoteConnectionProtocol.HandleOutput(const output: RawByteString):
  RawByteString;
begin
  result := AlgoSynLZ.Compress(output);
  SymmetricEncrypt(REMOTE_MAGIC, result);
end;

procedure TSqlDBProxyConnectionProtocol.RemoteProcessMessage(
  const Input: RawByteString; out Output: RawByteString; Connection: TSqlDBConnection);
var
  Stmt: ISQLDBStatement;
  Data: TRawByteStringStream;
  msgInput, msgOutput: RawByteString;
  header: PRemoteMessageHeader;
  O: PAnsiChar;
  i, session: integer;
  user: RawUTF8;
  InputExecute: TSqlDBProxyConnectionCommandExecute;
  ExecuteWithResults: boolean;
  OutputSQLDBColumnDefineDynArray: TSqlDBColumnDefineDynArray;
  OutputSQLDBIndexDefineDynArray: TSqlDBIndexDefineDynArray;
  OutputRawUTF8DynArray: TRawUTF8DynArray;

  procedure AppendOutput(value: Int64);
  var
    len: integer;
  begin
    len := Length(msgOutput);
    SetLength(msgOutput, len + sizeof(Int64));
    PInt64(@PByteArray(msgOutput)[len])^ := value;
  end;

begin
  // follow TSqlDBRemoteConnectionPropertiesAbstract.Process binary layout
  if self = nil then
    raise ESqlDBRemote.Create('RemoteProcessMessage(protocol=nil)');
  if Connection = nil then
    raise ESqlDBRemote.CreateUTF8('%.RemoteProcessMessage(connection=nil)', [self]);
  msgInput := HandleInput(Input);
  header := pointer(msgInput);
  if (header = nil) or
     (header.Magic <> REMOTE_MAGIC) then
    raise ESqlDBRemote.CreateUTF8('Wrong %.RemoteProcessMessage() input', [self]);
  if (Authenticate <> nil) and
     (Authenticate.UsersCount > 0) and
     not (header.Command in [cGetToken, cGetDBMS]) then
    if not Authenticate.SessionExists(header.SessionID) then
      raise ESqlDBRemote.Create('You do not have the right to be here');
  O := pointer(msgInput);
  inc(O, sizeof(header^));
  try
    msgOutput := copy(msgInput, 1, SizeOf(header^));
    case header.Command of
      cGetToken:
        AppendOutput(Authenticate.CurrentToken);
      cGetDBMS:
        begin
          session := 0;
          if (Authenticate <> nil) and
             (Authenticate.UsersCount > 0) then
          begin
            GetNextItem(PUTF8Char(O), #1, user);
            session := Authenticate.CreateSession(user, PCardinal(O)^);
            if session = 0 then
              raise ESqlDBRemote.Create('Impossible to Open a Session - ' +
                'check connection and User/Password');
          end;
          PRemoteMessageHeader(msgOutput)^.sessionID := session;
          msgOutput := msgOutput + AnsiChar(Connection.Properties.DBMS);
        end;
      cConnect:
        Connection.Connect;
      cDisconnect:
        Connection.Disconnect;
      cTryStartTransaction:
        msgOutput := msgOutput + AnsiChar(TransactionStarted(Connection, header.SessionID));
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
          Connection.Properties.GetFields(O, OutputSQLDBColumnDefineDynArray);
          msgOutput := msgOutput + DynArraySave(
            OutputSQLDBColumnDefineDynArray, TypeInfo(TSqlDBColumnDefineDynArray));
        end;
      cGetIndexes:
        begin
          Connection.Properties.GetIndexes(O, OutputSQLDBIndexDefineDynArray);
          msgOutput := msgOutput + DynArraySave(
            OutputSQLDBIndexDefineDynArray, TypeInfo(TSqlDBIndexDefineDynArray));
        end;
      cGetTableNames:
        begin
          Connection.Properties.GetTableNames(OutputRawUTF8DynArray);
          msgOutput := msgOutput + DynArraySave(
            OutputRawUTF8DynArray, TypeInfo(TRawUTF8DynArray));
        end;
      cGetForeignKeys:
        begin
          Connection.Properties.GetForeignKey('', ''); // ensure Dest.fForeignKeys exists
          msgOutput := msgOutput + Connection.Properties.ForeignKeysData;
        end;
      cExecute, cExecuteToBinary, cExecuteToJSON, cExecuteToExpandedJSON:
        begin
          RecordLoad(InputExecute, O, TypeInfo(TSqlDBProxyConnectionCommandExecute));
          ExecuteWithResults := header.Command <> cExecute;
          Stmt := Connection.NewStatementPrepared(InputExecute.SQL,
            ExecuteWithResults, true);
          if fBlobAsNull in InputExecute.Force then
            Stmt.ForceBlobAsNull := true;
          if fDateWithMS in InputExecute.Force then
            Stmt.ForceDateWithMS := true;
          for i := 1 to Length(InputExecute.Params) do
            with InputExecute.Params[i - 1] do
              if InputExecute.ArrayCount = 0 then
                case VType of
                  ftNull:
                    Stmt.BindNull(i, VInOut);
                  ftInt64:
                    Stmt.Bind(i, VInt64, VInOut);
                  ftDouble:
                    Stmt.Bind(i, unaligned(PDouble(@VInt64)^), VInOut);
                  ftCurrency:
                    Stmt.Bind(i, PCurrency(@VInt64)^, VInOut);
                  ftDate:
                    Stmt.BindDateTime(i, PDateTime(@VInt64)^, VInOut);
                  ftUTF8:
                    Stmt.BindTextU(i, VData, VInOut);
                  ftBlob:
                    Stmt.BindBlob(i, VData, VInOut);
                else
                  raise ESqlDBRemote.CreateUTF8('Invalid VType=% parameter #% in %.ProcessExec(%)',
                    [ord(VType), i, self, ToText(header.Command)^]);
                end
              else
                Stmt.BindArray(i, VType, VArray, InputExecute.ArrayCount);
          Stmt.ExecutePrepared;
          if ExecuteWithResults then
          begin
            Data := TRawByteStringStream.Create(msgOutput);
            try
              Data.Seek(0, soFromEnd); // include header
              case header.Command of
                cExecuteToBinary:
                  Stmt.FetchAllToBinary(Data);
                cExecuteToJSON:
                  Stmt.FetchAllToJSON(Data, false);
                cExecuteToExpandedJSON:
                  Stmt.FetchAllToJSON(Data, true);
              end;
              msgOutput := Data.DataString;
            finally
              Data.Free;
            end;
          end
          else if not (fNoUpdateCount in InputExecute.Force) then
            msgOutput := msgOutput + ToUTF8(Stmt.UpdateCount);
        end;
      cQuit:
        begin
          if header.SessionID = fTransactionSessionID then
            TransactionEnd(header.SessionID);
          Authenticate.RemoveSession(header.SessionID);
        end;
    else
      raise ESqlDBRemote.CreateUTF8('Unknown %.RemoteProcessMessage() command %',
        [self, ord(header.Command)]);
    end;
  except
    on E: Exception do
    begin
      PRemoteMessageHeader(msgOutput)^.Command := cExceptionRaised;
      msgOutput := msgOutput + StringToUTF8(E.ClassName + #0 + E.Message);
    end;
  end;
  Output := HandleOutput(msgOutput);
end;


{ ************ Client-Side Proxy Remote Protocol }

{ TSqlDBProxyConnectionPropertiesAbstract }

procedure TSqlDBProxyConnectionPropertiesAbstract.SetInternalProperties;
var
  InputCredential: RawUTF8;
  token: Int64;
begin
  if fStartTransactionTimeOut = 0 then
    fStartTransactionTimeOut := 2000;
  if fProtocol = nil then
    // override this method and set fProtocol before calling inherited
    fProtocol := TSqlDBProxyConnectionProtocol.Create(nil);
  Process(cGetToken, self, token);
  SetLength(InputCredential, 4);
  PCardinal(InputCredential)^ := fProtocol.Authenticate.ComputeHash(
    token, UserID, PassWord);
  InputCredential := UserID + #1 + InputCredential;
  fCurrentSession := Process(cGetDBMS, InputCredential, fDBMS);
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
end;

procedure TSqlDBProxyConnectionPropertiesAbstract.GetFields(
  const aTableName: RawUTF8; out Fields: TSqlDBColumnDefineDynArray);
begin
  Process(cGetFields, aTableName, Fields);
end;

procedure TSqlDBProxyConnectionPropertiesAbstract.GetIndexes(
  const aTableName: RawUTF8; out Indexes: TSqlDBIndexDefineDynArray);
begin
  Process(cGetIndexes, aTableName, Indexes);
end;

procedure TSqlDBProxyConnectionPropertiesAbstract.GetTableNames(
  out Tables: TRawUTF8DynArray);
begin
  Process(cGetTableNames, self, Tables);
end;

function TSqlDBProxyConnectionPropertiesAbstract.IsCachable(P: PUTF8Char): boolean;
begin
  result := False;
end;


{ TSqlDBRemoteConnectionPropertiesAbstract }

function TSqlDBRemoteConnectionPropertiesAbstract.Process(
  Command: TSqlDBProxyConnectionCommand; const Input; var Output): integer;
var
  msgInput, msgOutput, msgRaw: RawByteString;
  header: TRemoteMessageHeader;
  outheader: PRemoteMessageHeader;
  InputText: RawUTF8 absolute Input;
  InputExecute: TSqlDBProxyConnectionCommandExecute absolute Input;
  O: PAnsiChar;
  OutputSQLDBDefinition: TSqlDBDefinition absolute Output;
  OutputInt64: Int64 absolute Output;
  Outputboolean: boolean absolute Output;
  OutputSQLDBColumnDefineDynArray: TSqlDBColumnDefineDynArray absolute Output;
  OutputSQLDBIndexDefineDynArray: TSqlDBIndexDefineDynArray absolute Output;
  OutputRawUTF8DynArray: TRawUTF8DynArray absolute Output;
  OutputRawUTF8: RawUTF8 absolute Output;
  OutputSynNameValue: TSynNameValue absolute Output;
begin // use our optimized RecordLoadSave/DynArrayLoadSave binary serialization
  header.Magic := REMOTE_MAGIC;
  header.SessionID := fCurrentSession;
  header.Command := Command;
  SetString(msgInput, PAnsiChar(@header), sizeof(header));
  case Command of
    cGetToken, cConnect, cDisconnect, cTryStartTransaction, cCommit, cRollback,
      cServerTimestamp, cGetTableNames, cGetForeignKeys, cQuit:
      ; // no input parameters here, just the command
    cGetDBMS, cGetFields, cGetIndexes:
      msgInput := msgInput + InputText;
    cExecute, cExecuteToBinary, cExecuteToJSON, cExecuteToExpandedJSON:
      msgInput := msgInput +
        RecordSave(InputExecute, TypeInfo(TSqlDBProxyConnectionCommandExecute));
  else
    raise ESqlDBRemote.CreateUTF8('Unknown %.Process() input command % (%)', [self,
      ToText(Command)^, ord(Command)]);
  end;
  ProcessMessage(fProtocol.HandleOutput(msgInput), msgRaw);
  msgOutput := fProtocol.HandleInput(msgRaw);
  outheader := pointer(msgOutput);
  if (outheader = nil) or
     (outheader.Magic <> REMOTE_MAGIC) then
    raise ESqlDBRemote.CreateUTF8('Wrong %.Process() returned content', [self]);
  O := pointer(msgOutput);
  inc(O, sizeof(header));
  case outheader.Command of
    cGetToken, cServerTimestamp:
      OutputInt64 := PInt64(O)^;
    cGetDBMS:
      OutputSQLDBDefinition := TSqlDBDefinition(O^);
    cConnect, cDisconnect, cCommit, cRollback, cQuit:
      ; // no output parameters here
    cTryStartTransaction:
      Outputboolean := boolean(O^);
    cGetFields:
      DynArrayLoad(OutputSQLDBColumnDefineDynArray, O, TypeInfo(TSqlDBColumnDefineDynArray));
    cGetIndexes:
      DynArrayLoad(OutputSQLDBIndexDefineDynArray, O, TypeInfo(TSqlDBIndexDefineDynArray));
    cGetTableNames:
      DynArrayLoad(OutputRawUTF8DynArray, O, TypeInfo(TRawUTF8DynArray));
    cGetForeignKeys:
      OutputSynNameValue.SetBlobDataPtr(O);
    cExecute, cExecuteToBinary, cExecuteToJSON, cExecuteToExpandedJSON:
      FastSetString(OutputRawUTF8, O, length(msgOutput) - sizeof(header));
    cExceptionRaised: // msgOutput is ExceptionClassName+#0+ExceptionMessage
      raise ESqlDBRemote.CreateUTF8('%.Process(%): server raised % with ''%''',
        [self, ToText(Command)^, O, O + StrLen(O) + 1]);
  else
    raise ESqlDBRemote.CreateUTF8('Unknown %.Process() output command % (%)', [self,
      ToText(outheader.Command)^, ord(outheader.Command)]);
  end;
  result := outheader.SessionID;
end;


{ TSqlDBRemoteConnectionPropertiesTest }

constructor TSqlDBRemoteConnectionPropertiesTest.Create(aProps:
  TSqlDBConnectionProperties; const aUserID, aPassword: RawUTF8; aProtocol:
  TSqlDBProxyConnectionProtocolClass);
begin
  fProps := aProps;
  fProtocol := aProtocol.Create(TSynAuthentication.Create(aUserID, aPassword));
  inherited Create('', '', aUserID, aPassword);
end;

procedure TSqlDBRemoteConnectionPropertiesTest.ProcessMessage(const Input:
  RawByteString; out Output: RawByteString);
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
begin // always create a new proxy statement instance (cached on remote side)
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
  endTrial: Int64;
begin
  inherited StartTransaction;
  started := false;
  endTrial := GetTickCount64 + fProxy.StartTransactionTimeOut;
  repeat
    fProxy.Process(cTryStartTransaction, self, started);
    if started or
       (GetTickCount64 > endTrial) then
      break;
    SleepHiRes(10); // retry every 10 ms
  until false;
  if not started then
  begin
    inherited Rollback; // dec(fTransactionCount)
    raise ESqlDBRemote.CreateUTF8('Reached %("%/%").StartTransactionTimeOut=% ms',
      [self, fProxy.ServerName, fProxy.DatabaseName, fProxy.StartTransactionTimeOut]);
  end;
end;


{ TSqlDBProxyStatementAbstract }

procedure TSqlDBProxyStatementAbstract.IntHeaderProcess(Data: PByte; DataLen: PtrInt);
var
  Magic, F, colCount: integer;
  p: PSqlDBColumnProperty;
begin
  fDataCurrentRowValuesStart := nil;
  fDataCurrentRowValuesSize := 0;
  fDataCurrentRowIndex := -1;
  fDataCurrentRowNull := nil;
  fDataCurrentRowNullLen := 0;
  repeat
    if DataLen <= 5 then
      break; // to raise ESqlDBRemote
    fDataRowCount := PInteger(PAnsiChar(Data) + (DataLen - sizeof(integer)))^;
    Magic := FromVarUInt32(Data);
    if Magic <> FETCHALLTOBINARY_MAGIC then
      break; // corrupted
    colCount := FromVarUInt32(Data);
    SetLength(fDataCurrentRowColTypes, colCount);
    SetLength(fDataCurrentRowValues, colCount);
    fColumn.Capacity := colCount;
    for F := 0 to colCount - 1 do
    begin
      p := fColumn.AddAndMakeUniqueName(FromVarString(Data));
      p^.ColumnType := TSqlDBFieldType(Data^);
      inc(Data);
      p^.ColumnValueDBSize := FromVarUInt32(Data);
      fDataCurrentRowColTypes[F] := p^.ColumnType;
    end;
    if fColumnCount = 0 then
      exit; // no data returned
    if cardinal(fDataRowCount) >= cardinal(DataLen) then
      break; // obviously truncated
    fDataRowReaderOrigin := Data;
    fDataRowReader := Data;
    fDataRowNullSize := ((fColumnCount - 1) shr 3) + 1;
    SetLength(fDataCurrentRowNull, fDataRowNullSize);
    exit;
  until false;
  fDataRowCount := 0;
  fColumnCount := 0;
  raise ESqlDBRemote.CreateUTF8('Invalid %.IntHeaderProcess', [self]);
end;

procedure TSqlDBProxyStatementAbstract.IntFillDataCurrent(var Reader: PByte;
  IgnoreColumnDataSize: boolean);
var
  F, Len: integer;
  ft: TSqlDBFieldType;
begin
  // format match TSqlDBStatement.FetchAllToBinary()
  if fDataCurrentRowNullLen > 0 then
    FillCharFast(fDataCurrentRowNull[0], fDataCurrentRowNullLen, 0);
  fDataCurrentRowNullLen := FromVarUInt32(Reader);
  if fDataCurrentRowNullLen > fDataRowNullSize then
    raise ESqlDBRemote.CreateUTF8('Invalid %.IntFillDataCurrent %>%', [self,
      fDataCurrentRowNullLen, fDataRowNullSize]);
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
      begin // per-row column type (SQLite3 only)
        ft := TSqlDBFieldType(Reader^);
        inc(Reader);
      end;
      fDataCurrentRowColTypes[F] := ft;
      fDataCurrentRowValues[F] := Reader;
      case ft of
        ftInt64:
          Reader := GotoNextVarInt(Reader);
        ftDouble, ftCurrency, ftDate:
          inc(Reader, SizeOf(Int64));
        ftUTF8, ftBlob:
          begin
            Len := FromVarUInt32(Reader);
            if not IgnoreColumnDataSize then
              if Len > fColumns[F].ColumnDataSize then
                fColumns[F].ColumnDataSize := Len;
            inc(Reader, Len); // jump string/blob content
          end;
      else
        raise ESqlDBRemote.CreateUTF8('%.IntStep: Invalid ColumnType(%)=%', [self,
          fColumns[F].ColumnName, ord(ft)]);
      end;
    end;
  fDataCurrentRowValuesSize := PtrUInt(Reader) - PtrUInt(fDataCurrentRowValuesStart);
end;

procedure TSqlDBProxyStatementAbstract.ColumnsToJSON(WR: TJSONWriter);
var
  col, DataLen: integer;
  Data: PByte;
begin
  if WR.Expand then
    WR.Add('{');
  for col := 0 to fColumnCount - 1 do
  begin
    if WR.Expand then
      WR.AddFieldName(fColumns[col].ColumnName); // add '"ColumnName":'
    Data := fDataCurrentRowValues[col];
    if Data = nil then
      WR.AddNull
    else
      case fDataCurrentRowColTypes[col] of
        ftInt64:
          WR.Add(FromVarInt64Value(Data));
        ftDouble:
          WR.AddDouble(unaligned(PDouble(Data)^));
        ftCurrency:
          WR.AddCurr64(PInt64(Data));
        ftDate:
          begin
            WR.Add('"');
            WR.AddDateTime(PDateTime(Data)^);
            WR.Add('"');
          end;
        ftUTF8:
          begin
            WR.Add('"');
            DataLen := FromVarUInt32(Data);
            WR.AddJSONEscape(Data, DataLen);
            WR.Add('"');
          end;
        ftBlob:
          if fForceBlobAsNull then
            WR.AddNull
          else
          begin
            DataLen := FromVarUInt32(Data);
            WR.WrBase64(PAnsiChar(Data), DataLen, {withMagic=}true);
          end;
      end;
    WR.Add(',');
  end;
  WR.CancelLastComma; // cancel last ','
  if WR.Expand then
    WR.Add('}');
end;

procedure TSqlDBProxyStatementAbstract.ColumnsToBinary(W: TBufferWriter;
  Null: pointer; const ColTypes: TSqlDBFieldTypeDynArray);
begin
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
    raise ESqlDBRemote.CreateUTF8('Invalid %.ColumnType()', [self]);
end;

function TSqlDBProxyStatementAbstract.IntColumnType(Col: integer;
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
  Data: PByte;
begin
  case IntColumnType(Col, Data) of
    ftNull:
      result := 0;
    ftInt64:
      result := FromVarInt64Value(Data{%H-});
    ftDouble, ftDate:
      result := unaligned(PDouble(Data)^);
    ftCurrency:
      result := PCurrency(Data)^;
  else
    raise ESqlDBRemote.CreateUTF8('%.ColumnCurrency()', [self]);
  end;
end;

function TSqlDBProxyStatementAbstract.ColumnDateTime(Col: integer): TDateTime;
var
  Data: PByte;
begin
  case IntColumnType(Col, Data) of
    ftNull:
      result := 0;
    ftInt64:
      result := FromVarInt64Value(Data{%H-});
    ftDouble, ftDate:
      result := unaligned(PDouble(Data)^);
    ftUTF8:
      with FromVarBlob(Data) do
        result := Iso8601ToDateTimePUTF8Char(PUTF8Char(Ptr), len);
  else
    raise ESqlDBRemote.CreateUTF8('%.ColumnDateTime()', [self]);
  end;
end;

function TSqlDBProxyStatementAbstract.ColumnDouble(Col: integer): double;
var
  Data: PByte;
begin
  case IntColumnType(Col, Data) of
    ftNull:
      result := 0;
    ftInt64:
      result := FromVarInt64Value(Data{%H-});
    ftDouble, ftDate:
      result := unaligned(PDouble(Data)^);
    ftCurrency:
      result := PCurrency(Data)^;
  else
    raise ESqlDBRemote.CreateUTF8('%.ColumnDouble()', [self]);
  end;
end;

function TSqlDBProxyStatementAbstract.ColumnInt(Col: integer): Int64;
var
  Data: PByte;
begin
  case IntColumnType(Col, Data) of
    ftNull:
      result := 0;
    ftInt64:
      result := FromVarInt64Value(Data{%H-});
    ftDouble, ftDate:
      result := Trunc(unaligned(PDouble(Data)^));
    ftCurrency:
      result := PInt64(Data)^ div 10000;
  else
    raise ESqlDBRemote.CreateUTF8('%.ColumnInt()', [self]);
  end;
end;

function TSqlDBProxyStatementAbstract.ColumnNull(Col: integer): boolean;
begin
  result := (cardinal(Col) >= cardinal(fColumnCount)) or
            GetBitPtr(pointer(fDataCurrentRowNull), Col);
end;

function TSqlDBProxyStatementAbstract.ColumnBlob(Col: integer): RawByteString;
var
  Data: PByte;
begin
  case IntColumnType(Col, Data) of
    ftNull:
      result := '';
    ftDouble, ftCurrency, ftDate:
      SetString(result, PAnsiChar({%H-}Data), sizeof(Int64));
    ftBlob, ftUTF8:
      with FromVarBlob(Data) do
        SetString(result, Ptr, len);
  else
    raise ESqlDBRemote.CreateUTF8('%.ColumnBlob()', [self]);
  end;
end;

function TSqlDBProxyStatementAbstract.ColumnUTF8(Col: integer): RawUTF8;
var
  Data: PByte;
begin
  case IntColumnType(Col, Data) of
    ftNull:
      result := '';
    ftInt64:
      result := Int64ToUtf8(FromVarInt64Value(Data{%H-}));
    ftDouble:
      result := DoubleToStr(unaligned(PDouble(Data)^));
    ftCurrency:
      result := Curr64ToStr(PInt64(Data)^);
    ftDate:
      DateTimeToIso8601TextVar(PDateTime(Data)^, 'T', result);
    ftBlob, ftUTF8:
      with FromVarBlob(Data) do
        FastSetString(result, Ptr, len);
  else
    raise ESqlDBRemote.CreateUTF8('%.ColumnUTF8()', [self]);
  end;
end;

function TSqlDBProxyStatementAbstract.ColumnString(Col: integer): string;
var
  Data: PByte;
begin
  case IntColumnType(Col, Data) of
    ftNull:
      result := '';
    ftInt64:
      result := IntToString(FromVarInt64Value(Data{%H-}));
    ftDouble:
      result := DoubleToString(unaligned(PDouble(Data)^));
    ftCurrency:
      result := Curr64ToString(PInt64(Data)^);
    ftDate:
      DateTimeToIso8601StringVar(PDateTime(Data)^, 'T', result);
    ftUTF8:
      with FromVarBlob(Data) do
        UTF8DecodeToString(PUTF8Char(Ptr), len, result);
    ftBlob:
      with FromVarBlob(Data) do
        SetString(result, Ptr, len shr 1);
  else
    raise ESqlDBRemote.CreateUTF8('%.ColumnString()', [self]);
  end;
end;


{ TSqlDBProxyStatement }

procedure TSqlDBProxyStatement.ParamsToCommand(
  var Input: TSqlDBProxyConnectionCommandExecute);
begin
  if (fColumnCount > 0) or
     (fDataInternalCopy <> '') then
    raise ESqlDBRemote.CreateUTF8('Invalid %.ExecutePrepared* call', [self]);
  Input.SQL := fSQL;
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

procedure TSqlDBProxyStatement.ExecutePrepared;
var
  Input: TSqlDBProxyConnectionCommandExecute;
const
  cmd: array[boolean] of TSqlDBProxyConnectionCommand = (
    cExecute, cExecuteToBinary);
begin
  inherited ExecutePrepared; // set fConnection.fLastAccessTicks
  // execute the statement
  ParamsToCommand(Input);
  TSqlDBProxyConnectionPropertiesAbstract(fConnection.Properties).Process(
    cmd[fExpectResults], Input, fDataInternalCopy);
  if fExpectResults then
    // retrieve columns information from TSqlDBStatement.FetchAllToBinary() format
    IntHeaderProcess(pointer(fDataInternalCopy), Length(fDataInternalCopy))
  else
    // retrieve UpdateCount value for plain cExecute command
    fUpdateCount := GetInteger(pointer(fDataInternalCopy));
end;

function TSqlDBProxyStatement.UpdateCount: integer;
begin
  result := fUpdateCount;
end;

procedure TSqlDBProxyStatement.ExecutePreparedAndFetchAllAsJSON(
  Expanded: boolean; out JSON: RawUTF8);
var
  Input: TSqlDBProxyConnectionCommandExecute;
const
  cmd: array[boolean] of TSqlDBProxyConnectionCommand = (
    cExecuteToJSON, cExecuteToExpandedJSON);
begin
  ParamsToCommand(Input);
  TSqlDBProxyConnectionPropertiesAbstract(fConnection.Properties).Process(cmd[Expanded],
    Input, JSON);
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
begin // retrieve one row of data from TSqlDBStatement.FetchAllToBinary() format
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
  IntFillDataCurrent(fDataRowReader, false);
  inc(fCurrentRow);
  result := true;
end;


{ TSqlDBProxyStatementRandomAccess }

constructor TSqlDBProxyStatementRandomAccess.Create(Data: PByte; DataLen: integer;
  DataRowPosition: PCardinalDynArray; IgnoreColumnDataSize: boolean);
var
  i, f: PtrInt;
  Reader: PByte;
begin
  inherited Create(nil);
  IntHeaderProcess(Data, DataLen);
  Reader := fDataRowReaderOrigin;
  if (DataRowPosition <> nil) and
     (DataRowPosition^ <> nil) then
  begin
    fRowData := DataRowPosition^; // fast copy-on-write
    if not IgnoreColumnDataSize then
      for f := 0 to fColumnCount - 1 do
        with fColumns[f] do
          if ColumnType in [ftUTF8, ftBlob] then
            if ColumnValueDBSize = 0 then
            begin // unknown size -> compute
              for i := 0 to DataRowCount - 1 do
                IntFillDataCurrent(Reader, false); // will compute ColumnDataSize
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
      fRowData[i] := PtrUInt(Reader) - PtrUInt(fDataRowReaderOrigin);
      IntFillDataCurrent(Reader, IgnoreColumnDataSize); // will also compute ColumnDataSize
    end;
  end;
end;

function TSqlDBProxyStatementRandomAccess.GotoRow(Index: integer;
  RaiseExceptionOnWrongIndex: boolean): boolean;
var
  Reader: PByte;
begin
  result := (cardinal(Index) < cardinal(fDataRowCount)) and
            (fColumnCount > 0);
  if not result then
    if RaiseExceptionOnWrongIndex then
      raise ESqlDBRemote.CreateUTF8('Invalid %.GotoRow(%)', [self, Index])
    else
      exit;
  if fDataCurrentRowIndex <> Index then
  begin // compute only if changed :)
    Reader := @PAnsiChar(fDataRowReaderOrigin)[fRowData[Index]];
    IntFillDataCurrent(Reader, false);
    fDataCurrentRowIndex := Index;
  end;
end;

procedure TSqlDBProxyStatementRandomAccess.ExecutePrepared;
begin
  raise ESqlDBRemote.CreateUTF8('Unexpected %.ExecutePrepared', [self]);
end;

function TSqlDBProxyStatementRandomAccess.{%H-}Step(SeekFirst: boolean): boolean;
begin
  raise ESqlDBRemote.CreateUTF8('Unexpected %.Step', [self]);
end;


{ ************ HTTP Server Classes for Remote Access }

{ TSqlDBServerAbstract }

constructor TSqlDBServerAbstract.Create(aProperties: TSqlDBConnectionProperties;
  const aDatabaseName, aPort, aUserName, aPassword: RawUTF8; aHttps: boolean;
  aThreadPoolCount: integer; aProtocol: TSqlDBProxyConnectionProtocolClass;
  aThreadMode: TSqlDBConnectionPropertiesThreadSafeThreadingMode;
  aAuthenticate: TSynAuthenticationAbstract);
begin
  fProperties := aProperties;
  if fProperties.InheritsFrom(TSqlDBConnectionPropertiesThreadSafe) then
  begin
    TSqlDBConnectionPropertiesThreadSafe(fProperties).ThreadingMode := aThreadMode;
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
  o: RawByteString;
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


{$ifndef ONLYUSEHTTPSOCKET}

{ TSqlDBServerHttpApi }

constructor TSqlDBServerHttpApi.Create(aProperties: TSqlDBConnectionProperties;
  const aDatabaseName, aPort, aUserName, aPassword: RawUTF8; aHttps: boolean;
  aThreadPoolCount: integer; aProtocol: TSqlDBProxyConnectionProtocolClass;
  aThreadMode: TSqlDBConnectionPropertiesThreadSafeThreadingMode;
  aAuthenticate: TSynAuthenticationAbstract);
var
  status: integer;
begin
  inherited;
  fServer := THttpApiServer.Create(false, '');
  status := THttpApiServer(fServer).AddUrl(fDatabaseName, fPort, fHttps, '+', true);
  if status <> NO_ERROR then
    if status = ERROR_ACCESS_DENIED then
      raise ESqlDBRemote.CreateUTF8(
        '%.Create: administrator rights needed to register URI % on port %',
        [self, fDatabaseName, fPort])
    else
      raise ESqlDBRemote.CreateUTF8(
        '%.Create: error registering URI % on port %: is not another server ' +
        'instance running on this port?', [self, fDatabaseName, fPort]);
  fServer.OnRequest := Process;
  if fThreadPoolCount > 1 then
    THttpApiServer(fServer).Clone(fThreadPoolCount - 1);
end;

{$endif ONLYUSEHTTPSOCKET}


{ TSqlDBServerSockets }

constructor TSqlDBServerSockets.Create(aProperties: TSqlDBConnectionProperties;
  const aDatabaseName, aPort, aUserName, aPassword: RawUTF8; aHttps: boolean;
  aThreadPoolCount: integer; aProtocol: TSqlDBProxyConnectionProtocolClass;
  aThreadMode: TSqlDBConnectionPropertiesThreadSafeThreadingMode;
  aAuthenticate: TSynAuthenticationAbstract);
var
  ident: RawUTF8;
begin
  inherited;
  FormatUTF8('DBRemote %', [aDatabaseName], ident);
  fServer := THttpServer.Create(aPort, nil, nil, ident, fThreadPoolCount);
  THttpServer(fServer).WaitStarted;
  fServer.OnRequest := Process;
end;



{ ************ HTTP Client Classes for Remote Access }

{ TSqlDBHTTPConnectionPropertiesAbstract }

function TSqlDBHTTPConnectionPropertiesAbstract.GetServer: RawByteString;
begin
  result := fURI.Server;
end;

function TSqlDBHTTPConnectionPropertiesAbstract.GetPort: RawByteString;
begin
  result := fURI.Port;
end;

procedure TSqlDBHTTPConnectionPropertiesAbstract.SetServerName(
  const aServerName: RawUTF8);
begin
  fKeepAliveMS := 60000;
  if not fURI.From(aServerName) then
    raise ESqlDBRemote.CreateUTF8('%.Create: expect a valid URI in aServerName="%"',
      [self, aServerName]);
  if fURI.Port = '' then
    fURI.Port := SYNDB_DEFAULT_HTTP_PORT;
end;

procedure TSqlDBHTTPConnectionPropertiesAbstract.ProcessMessage(const Input:
  RawByteString; out Output: RawByteString);
var
  Content, ContentType: RawByteString;
  status: integer;
begin
  Content := Input;
  ContentType := BINARY_CONTENT_TYPE;
  status := InternalRequest(Content, ContentType);
  if status <> HTTP_SUCCESS then
    raise ESqlDBRemote.CreateUTF8('%.ProcessMessage: Error % from %',
      [self, status, fURI.URI]);
  if ContentType <> BINARY_CONTENT_TYPE then
    raise ESqlDBRemote.CreateUTF8('%.ProcessMessage: Invalid content type [%] from %',
      [self, ContentType, fURI.URI]);
  Output := Content;
end;

procedure TSqlDBHTTPConnectionPropertiesAbstract.SetInternalProperties;
begin
  if fProtocol = nil then
    fProtocol := TSqlDBRemoteConnectionProtocol.Create(
      TSynAuthentication.Create(UserID, PassWord));
  inherited;
end;


{ TSqlDBSocketConnectionProperties }

constructor TSqlDBSocketConnectionProperties.Create(const aServerName,
  aDatabaseName, aUserID, aPassWord: RawUTF8);
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
  result := fSocket.Request(fDatabaseName, 'POST', fKeepAliveMS, '', Data,
    DataType, false);
  Data := fSocket.Content;
  DataType := fSocket.ContentType;
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
  inData: RawByteString;
  inDataType, head: RawUTF8;
begin
  inData := Data;
  inDataType := DataType;
  result := fClient.Request(fDatabaseName, 'POST', fKeepAliveMS, '', inData,
    inDataType, head, Data);
  FindNameValue(head, HEADER_CONTENT_TYPE_UPPER, RawUTF8(DataType));
end;


{$ifdef USEWININET}

{ TSqlDBWinHTTPConnectionProperties }

constructor TSqlDBWinHTTPConnectionProperties.Create(const aServerName,
  aDatabaseName, aUserID, aPassWord: RawUTF8);
begin
  SetServerName(aServerName);
  fClient := TWinHTTP.Create(Server, Port, fURI.Https);
  inherited;
end;

{ TSqlDBWinINetConnectionProperties }

constructor TSqlDBWinINetConnectionProperties.Create(const aServerName,
  aDatabaseName, aUserID, aPassWord: RawUTF8);
begin
  SetServerName(aServerName);
  fClient := TWinINet.Create(Server, Port, fURI.Https);
  inherited;
end;

{$endif USEWININET}

{$ifdef USELIBCURL}

{ TSqlDBCurlConnectionProperties }

constructor TSqlDBCurlConnectionProperties.Create(const aServerName,
  aDatabaseName, aUserID, aPassWord: RawUTF8);
begin
  SetServerName(aServerName);
  fClient := TCurlHTTP.Create(Server, Port, fURI.Https);
  inherited;
end;

{$endif USELIBCURL}

initialization
  TSqlDBSocketConnectionProperties.RegisterClassNameForDefinition;
  {$ifdef USEWININET}
  TSqlDBWinHTTPConnectionProperties.RegisterClassNameForDefinition;
  TSqlDBWinINetConnectionProperties.RegisterClassNameForDefinition;
  {$endif USEWININET}
  {$ifdef USELIBCURL}
  TSqlDBCurlConnectionProperties.RegisterClassNameForDefinition;
  {$endif USELIBCURL}
  
end.
