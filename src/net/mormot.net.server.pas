/// HTTP/HTTPS Server Classes
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.server;

{
  *****************************************************************************

   HTTP/UDP Server Classes
   - Abstract UDP Server
   - Custom URI Routing using an efficient Radix Tree
   - Shared Server-Side HTTP Process
   - THttpServerSocket/THttpServer HTTP/1.1 Server
   - THttpPeerCache Local Peer-to-peer Cache
   - THttpApiServer HTTP/1.1 Server Over Windows http.sys Module
   - THttpApiWebSocketServer Over Windows http.sys Module

  *****************************************************************************

}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.os.security,
  mormot.core.data,
  mormot.core.threads,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.datetime,
  mormot.core.variants,
  mormot.core.zip,
  mormot.core.log,
  mormot.core.search,
  mormot.net.sock,
  mormot.net.http,
  {$ifdef USEWININET}
  mormot.lib.winhttp,
  {$endif USEWININET}
  mormot.lib.sspi,   // void unit on POSIX
  mormot.lib.gssapi, // void unit on Windows
  mormot.net.client,
  mormot.crypt.core,
  mormot.crypt.secure;


{ ******************** Abstract UDP Server }

type
  EUdpServer = class(ENetSock);

  /// work memory buffer of the maximum size of UDP frame (64KB)
  TUdpFrame = array[word] of byte;

  /// pointer to a memory buffer of the maximum size of UDP frame
  PUdpFrame = ^TUdpFrame;

  /// abstract UDP server thread
  TUdpServerThread = class(TLoggedThread)
  protected
    fSock: TNetSocket;
    fSockAddr: TNetAddr;
    fFrame: PUdpFrame;
    fReceived: integer;
    fBound: boolean;
    function GetIPWithPort: RawUtf8;
    procedure AfterBind; virtual;
    /// will loop for any pending UDP frame, and execute FrameReceived method
    procedure DoExecute; override;
    // this is the main processing method for all incoming frames
    procedure OnFrameReceived(len: integer; var remote: TNetAddr); virtual; abstract;
    procedure OnIdle(tix64: Int64); virtual; // called every 512 ms at most
    procedure OnShutdown; virtual; abstract;
  public
    /// initialize and bind the server instance, in non-suspended state
    constructor Create(LogClass: TSynLogClass;
      const BindAddress, BindPort, ProcessName: RawUtf8;
      TimeoutMS: integer); reintroduce;
    /// finalize the processing thread
    destructor Destroy; override;
  published
    property IPWithPort: RawUtf8
      read GetIPWithPort;
    property Received: integer
      read fReceived;
  end;

const
  /// the UDP frame content as sent by TUdpServerThread.Destroy
  UDP_SHUTDOWN: RawUtf8 = 'shutdown';


{ ******************** Custom URI Routing using an efficient Radix Tree }

type
  /// one HTTP method supported by TUriRouter
  // - only supports RESTful GET/POST/PUT/DELETE/OPTIONS/HEAD by default
  // - each method would have its dedicated TUriTree parser in TUriRouter
  TUriRouterMethod = (
    urmGet,
    urmPost,
    urmPut,
    urmDelete,
    urmOptions,
    urmHead,
    urmPatch);

  /// the HTTP methods supported by TUriRouter
  TUriRouterMethods = set of TUriRouterMethod;

  /// context information, as cloned by TUriTreeNode.Split()
  TUriTreeNodeData = record
    /// the Rewrite() URI text
    ToUri: RawUtf8;
    /// [pos1,len1,valndx1,pos2,len2,valndx2,...] trios from ToUri content
    ToUriPosLen: TIntegerDynArray;
    /// the size of all ToUriPosLen[] static content
    ToUriStaticLen: integer;
    /// the URI method to be used after ToUri rewrite
    ToUriMethod: TUriRouterMethod;
    /// the HTTP error code for a Rewrite() with an integer ToUri (e.g. '404')
    ToUriErrorStatus: {$ifdef CPU32} word {$else} cardinal {$endif};
    /// the callback registered by Run() for this URI
    Execute: TOnHttpServerRequest;
    /// an additional pointer value, assigned to Ctxt.RouteOpaque of Execute()
    ExecuteOpaque: pointer;
  end;

  /// implement a Radix Tree node to hold one URI registration
  TUriTreeNode = class(TRadixTreeNodeParams)
  protected
    function LookupParam(Ctxt: TObject; Pos: PUtf8Char; Len: integer): boolean;
      override;
    procedure RewriteUri(Ctxt: THttpServerRequestAbstract);
  public
    /// all context information, as cloned by Split()
    Data: TUriTreeNodeData;
    /// overriden to support the additional Data fields
    function Split(const Text: RawUtf8): TRadixTreeNode; override;
  end;

  /// implement a Radix Tree to hold all registered URI for a given HTTP method
  TUriTree = class(TRadixTreeParams)
  public
    /// access to the root node of this tree
    function Root: TUriTreeNode;
      {$ifdef HASINLINE}inline;{$endif}
  end;

  /// exception class raised during TUriRouter.Rewrite/Run registration
  EUriRouter = class(ERadixTree);

  /// store per-method URI multiplexing Radix Tree in TUriRouter
  // - each HTTP method would have its dedicated TUriTree parser in TUriRouter
  TUriRouterTree = array[urmGet .. high(TUriRouterMethod)] of TUriTree;

  /// efficient server-side URI routing for THttpServerGeneric
  // - Process() is done with no memory allocation for a static route,
  // using a very efficient Radix Tree for path lookup, over a thread-safe
  // non-blocking URI parsing with values extractions for rewrite or execution
  // - here are some numbers from TNetworkProtocols._TUriTree on my laptop:
  // $ 1000 URI lookups in 37us i.e. 25.7M/s, aver. 37ns
  // $ 1000 URI static rewrites in 80us i.e. 11.9M/s, aver. 80ns
  // $ 1000 URI parametrized rewrites in 117us i.e. 8.1M/s, aver. 117ns
  // $ 1000 URI static execute in 91us i.e. 10.4M/s, aver. 91ns
  // $ 1000 URI parametrized execute in 162us i.e. 5.8M/s, aver. 162ns
  TUriRouter = class(TObjectRWLightLock)
  protected
    fTree: TUriRouterTree;
    fTreeOptions: TRadixTreeOptions;
    fEntries: array[urmGet .. high(TUriRouterMethod)] of integer;
    fTreeNodeClass: TRadixTreeNodeClass;
    procedure Setup(aFrom: TUriRouterMethod; const aFromUri: RawUtf8;
      aTo: TUriRouterMethod; const aToUri: RawUtf8;
      const aExecute: TOnHttpServerRequest; aExecuteOpaque: pointer);
  public
    /// initialize this URI routing engine
    constructor Create(aNodeClass: TRadixTreeNodeClass;
      aOptions: TRadixTreeOptions = []); reintroduce;
    /// finalize this URI routing engine
    destructor Destroy; override;

    /// register an URI rewrite with optional <param> place holders
    // - <param> will be replaced in aToUri
    // - if aToUri is an '200'..'599' integer, it will return it as HTTP error
    // - otherwise, the URI will be rewritten into aToUri, e.g.
    // ! Rewrite(urmGet, '/info', urmGet, 'root/timestamp/info');
    // ! Rewrite(urmGet, '/path/from/<from>/to/<to>', urmPost,
    // !  '/root/myservice/convert?from=<from>&to=<to>'); // for IMyService.Convert
    // ! Rewrite(urmGet, '/index.php', '400'); // to avoid fuzzing
    // ! Rewrite(urmGet, '/*', '/static/*' // '*' synonymous to '<path:path>'
    procedure Rewrite(aFrom: TUriRouterMethod; const aFromUri: RawUtf8;
      aTo: TUriRouterMethod; const aToUri: RawUtf8);
    /// just a wrapper around Rewrite(urmGet, aFrom, aToMethod, aTo)
    // - e.g. Route.Get('/info', 'root/timestamp/info');
    // - e.g. Route.Get('/user/<id>', '/root/userservice/new?id=<id>'); will
    // rewrite internally '/user/1234' URI as '/root/userservice/new?id=1234'
    // - e.g. Route.Get('/user/<int:id>', '/root/userservice/new?id=<id>');
    // to ensure id is a real integer before redirection
    // - e.g. Route.Get('/admin.php', '403');
    // - e.g. Route.Get('/*', '/static/*'); with '*' synonymous to '<path:path>'
    procedure Get(const aFrom, aTo: RawUtf8;
      aToMethod: TUriRouterMethod = urmGet); overload;
    /// just a wrapper around Rewrite(urmPost, aFrom, aToMethod, aTo)
    // - e.g. Route.Post('/doconvert', '/root/myservice/convert');
    procedure Post(const aFrom, aTo: RawUtf8;
      aToMethod: TUriRouterMethod = urmPost); overload;
    /// just a wrapper around Rewrite(urmPut, aFrom, aToMethod, aTo)
    // - e.g. Route.Put('/domodify', '/root/myservice/update', urmPost);
    procedure Put(const aFrom, aTo: RawUtf8;
      aToMethod: TUriRouterMethod = urmPut); overload;
    /// just a wrapper around Rewrite(urmPatch, aFrom, aToMethod, aTo)
    // - e.g. Route.Patch('/domodify', '/root/myservice/update', urmPatch);
    procedure Patch(const aFrom, aTo: RawUtf8;
      aToMethod: TUriRouterMethod = urmPatch); overload;
    /// just a wrapper around Rewrite(urmDelete, aFrom, aToMethod, aTo)
    // - e.g. Route.Delete('/doremove', '/root/myservice/delete', urmPost);
    procedure Delete(const aFrom, aTo: RawUtf8;
      aToMethod: TUriRouterMethod = urmDelete); overload;
    /// just a wrapper around Rewrite(urmOptions, aFrom, aToMethod, aTo)
    // - e.g. Route.Options('/doremove', '/root/myservice/Options', urmPost);
    procedure Options(const aFrom, aTo: RawUtf8;
      aToMethod: TUriRouterMethod = urmOptions); overload;
    /// just a wrapper around Rewrite(urmHead, aFrom, aToMethod, aTo)
    // - e.g. Route.Head('/doremove', '/root/myservice/Head', urmPost);
    procedure Head(const aFrom, aTo: RawUtf8;
      aToMethod: TUriRouterMethod = urmHead); overload;

    /// assign a TOnHttpServerRequest callback with a given URI
    // - <param> place holders will be parsed and available in callback
    // as Ctxt['param'] default property or Ctxt.RouteInt64/RouteEquals methods
    // - could be used e.g. for standard REST process as
    // ! Route.Run([urmGet], '/user/<user>/pic', DoUserPic) // retrieve a list
    // ! Route.Run([urmGet, urmPost, urmPut, urmDelete],
    // !    '/user/<user>/pic/<id>', DoUserPic) // CRUD picture access
    procedure Run(aFrom: TUriRouterMethods; const aFromUri: RawUtf8;
      const aExecute: TOnHttpServerRequest; aExecuteOpaque: pointer = nil);
    /// just a wrapper around Run([urmGet], aUri, aExecute) registration method
    // - e.g. Route.Get('/plaintext', DoPlainText);
    procedure Get(const aUri: RawUtf8; const aExecute: TOnHttpServerRequest;
      aExecuteOpaque: pointer = nil); overload;
    /// just a wrapper around Run([urmPost], aUri, aExecute) registration method
    procedure Post(const aUri: RawUtf8; const aExecute: TOnHttpServerRequest;
      aExecuteOpaque: pointer = nil); overload;
    /// just a wrapper around Run([urmPut], aUri, aExecute) registration method
    procedure Put(const aUri: RawUtf8; const aExecute: TOnHttpServerRequest;
      aExecuteOpaque: pointer = nil); overload;
    /// just a wrapper around Run([urmPatch], aUri, aExecute) registration method
    procedure Patch(const aUri: RawUtf8; const aExecute: TOnHttpServerRequest;
      aExecuteOpaque: pointer = nil); overload;
    /// just a wrapper around Run([urmDelete], aUri, aExecute) registration method
    procedure Delete(const aUri: RawUtf8; const aExecute: TOnHttpServerRequest;
      aExecuteOpaque: pointer = nil); overload;
    /// just a wrapper around Run([urmOptions], aUri, aExecute) registration method
    procedure Options(const aUri: RawUtf8; const aExecute: TOnHttpServerRequest;
      aExecuteOpaque: pointer = nil); overload;
    /// just a wrapper around Run([urmHead], aUri, aExecute) registration method
    procedure Head(const aUri: RawUtf8; const aExecute: TOnHttpServerRequest;
      aExecuteOpaque: pointer = nil); overload;
    /// assign the published methods of a class instance to their URI via RTTI
    // - the signature of each method should match TOnHttpServerRequest
    // - the method name is used for the URI, e.g. Instance.user as '/user',
    // with exact case matching, and replacing _ in the method name by '-', e.g.
    // Instance.cached_query as '/cached-query'
    procedure RunMethods(RouterMethods: TUriRouterMethods; Instance: TObject;
      const Prefix: RawUtf8 = '/');

    /// perform URI parsing and rewrite/execution within HTTP server Ctxt members
    // - should return 0 to continue the process, on a HTTP status code to abort
    // if the request has been handled by a TOnHttpServerRequest callback
    // - this method is thread-safe
    function Process(Ctxt: THttpServerRequestAbstract): integer;
    /// search for a given URI match
    // - could be used e.g. in OnBeforeBody() to quickly reject an invalid URI
    // - this method is thread-safe
    function Lookup(const aUri, aUriMethod: RawUtf8): TUriTreeNode;
    /// erase all previous registrations, optionally for a given HTTP method
    // - currently, there is no way to delete a route once registered, to
    // optimize the process thread-safety: use Clear then re-register
    procedure Clear(aMethods: TUriRouterMethods = [urmGet .. high(TUriRouterMethod)]);
    /// access to the internal per-method TUriTree instance
    // - some Tree[] may be nil if the HTTP method has not been registered yet
    // - used only for testing/validation purpose
    property Tree: TUriRouterTree
      read fTree;
    /// how the TUriRouter instance should be created
    // - should be set before calling Run/Rewrite registration methods
    property TreeOptions: TRadixTreeOptions
      read fTreeOptions write fTreeOptions;
  published
    /// how many GET rules have been registered
    property Gets: integer
      read fEntries[urmGet];
    /// how many POST rules have been registered
    property Posts: integer
      read fEntries[urmPost];
    /// how many PUT rules have been registered
    property Puts: integer
      read fEntries[urmPut];
    /// how many PATCH rules have been registered
    property Patchs: integer
      read fEntries[urmPatch];
    /// how many DELETE rules have been registered
    property Deletes: integer
      read fEntries[urmDelete];
    /// how many HEAD rules have been registered
    property Heads: integer
      read fEntries[urmHead];
    /// how many OPTIONS rules have been registered
    property Optionss: integer
      read fEntries[urmOptions];
  end;

const
  /// convert TUriRouterMethod into its standard HTTP text
  // - see UriMethod() function for the reverse conversion
  URIROUTERMETHOD: array[TUriRouterMethod] of RawUtf8 = (
    'GET',     // urmGet
    'POST',    // urmPost
    'PUT',     // urmPut
    'DELETE',  // urmDelete
    'OPTIONS', // urmOptions
    'HEAD',    // urmHead
    'PATCH');  // urmPatch

/// quickly recognize most HTTP text methods into a TUriRouterMethod enumeration
// - may replace cascaded IsGet() IsPut() IsPost() IsDelete() function calls
// - see URIROUTERMETHOD[] constant for the reverse conversion
function UriMethod(const Text: RawUtf8; out Method: TUriRouterMethod): boolean;

/// check if the supplied text contains only valid characters for a root URI
// - excluding the parameters, i.e. rejecting the ? and % characters
// - but allowing <param> place holders as recognized by TUriRouter
function IsValidUriRoute(p: PUtf8Char): boolean;



{ ******************** Shared Server-Side HTTP Process }

type
  /// exception raised during HTTP process
  EHttpServer = class(ESynException);

  {$M+} // to have existing RTTI for published properties
  THttpServerGeneric = class;
  {$M-}

  /// 32-bit sequence value used to identify one asynchronous connection
  // - will start from 1, and increase during the server live-time
  // - THttpServerConnectionID may be retrieved from nginx reverse proxy
  // - used e.g. for Server.AsyncResponse() delayed call with HTTP_ASYNCRESPONSE
  TConnectionAsyncHandle = type integer;

  /// a dynamic array of TConnectionAsyncHandle identifiers
  TConnectionAsyncHandleDynArray = array of TConnectionAsyncHandle;

  /// a generic input/output structure used for HTTP server requests
  // - URL/Method/InHeaders/InContent properties are input parameters
  // - OutContent/OutContentType/OutCustomHeader are output parameters
  THttpServerRequest = class(THttpServerRequestAbstract)
  protected
    fServer: THttpServerGeneric;
    fConnectionAsyncHandle: TConnectionAsyncHandle;
    fErrorMessage: string;
    fTempWriter: TJsonWriter; // reused between SetOutJson() calls
    {$ifdef USEWININET}
    fHttpApiRequest: PHTTP_REQUEST;
    function GetFullUrl: SynUnicode;
    {$endif USEWININET}
  public
    /// initialize the context, associated to a HTTP server instance
    constructor Create(aServer: THttpServerGeneric;
      aConnectionID: THttpServerConnectionID; aConnectionThread: TSynThread;
      aConnectionAsyncHandle: TConnectionAsyncHandle;
      aConnectionFlags: THttpServerRequestFlags;
      aConnectionOpaque: PHttpServerConnectionOpaque); virtual;
    /// could be called before Prepare() to reuse an existing instance
    procedure Recycle(
      aConnectionID: THttpServerConnectionID; aConnectionThread: TSynThread;
      aConnectionAsyncHandle: TConnectionAsyncHandle;
      aConnectionFlags: THttpServerRequestFlags;
      aConnectionOpaque: PHttpServerConnectionOpaque);
    /// finalize this execution context
    destructor Destroy; override;
    /// prepare one reusable HTTP State Machine for sending the response
    function SetupResponse(var Context: THttpRequestContext;
      CompressGz, MaxSizeAtOnce: integer): PRawByteStringBuffer;
    /// just a wrapper around fErrorMessage := FormatString()
    procedure SetErrorMessage(const Fmt: RawUtf8; const Args: array of const);
    /// serialize a given value as JSON into OutContent and OutContentType fields
    // - this function returns HTTP_SUCCESS
    function SetOutJson(Value: pointer; TypeInfo: PRttiInfo): cardinal; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// serialize a given TObject as JSON into OutContent and OutContentType fields
    // - this function returns HTTP_SUCCESS
    function SetOutJson(Value: TObject): cardinal; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// low-level initialization of the associated TJsonWriter instance
    // - will reset and reuse an TJsonWriter associated to this execution context
    // - as called by SetOutJson() overloaded methods using RTTI
    // - a local TTextWriterStackBuffer should be provided as temporary buffer
    function TempJsonWriter(var temp: TTextWriterStackBuffer): TJsonWriter;
      {$ifdef HASINLINE} inline; {$endif}
    /// an additional custom parameter, as provided to TUriRouter.Setup
    function RouteOpaque: pointer; override;
    /// return the low-level internal handle for Server.AsyncResponse() delayed call
    // - to be used in conjunction with a HTTP_ASYNCRESPONSE internal status code
    // - raise an EHttpServer exception if async responses are not available
    function AsyncHandle: TConnectionAsyncHandle;
    /// save the URI parameters (or POST content) as a TDocVariant
    // - Url/Method/InContent are stored as aParams.U['url'/'method'/'content']
    procedure ToDocVariant(out Dest: TDocVariantData);
    /// the associated server instance
    // - may be a THttpServer or a THttpApiServer class
    property Server: THttpServerGeneric
      read fServer;
    /// optional error message which will be used by SetupResponse
    property ErrorMessage: string
      read fErrorMessage write fErrorMessage;
    {$ifdef USEWININET}
    /// for THttpApiServer, input parameter containing the caller full URL
    property FullUrl: SynUnicode
      read GetFullUrl;
    /// for THttpApiServer, points to a PHTTP_REQUEST structure
    property HttpApiRequest: PHTTP_REQUEST
      read fHttpApiRequest;
    {$endif USEWININET}
  end;
  /// meta-class of HTTP server requests instances
  THttpServerRequestClass = class of THttpServerRequest;

  /// available HTTP server options
  // - some THttpServerGeneric classes may have only partial support of them
  // - hsoHeadersUnfiltered will store all headers, not only relevant (i.e.
  // include raw Content-Length, Content-Type and Content-Encoding entries)
  // - hsoHeadersInterning triggers TRawUtf8Interning to reduce memory usage
  // - hsoNoStats will disable low-level statistic counters
  // - hsoNoXPoweredHeader excludes 'X-Powered-By: mORMot 2 synopse.info' header
  // - hsoCreateSuspended won't start the server thread immediately
  // - hsoLogVerbose could be used to debug a server in production
  // - hsoIncludeDateHeader will always include a "Date: ..." HTTP header
  // - hsoEnableTls enables TLS support for THttpServer socket server, using
  // Windows SChannel API or OpenSSL - call WaitStarted() to set the certificates
  // - hsoBan40xIP will reject any IP for a few seconds after a 4xx error code
  // is returned (but 401/403) - only implemented by socket servers for now
  // - either hsoThreadCpuAffinity or hsoThreadSocketAffinity could be set: to
  // force thread affinity to one CPU logic core, or CPU HW socket; see
  // TNotifiedThread corresponding methods - not available on http.sys
  // - hsoReusePort will set SO_REUSEPORT on POSIX, allowing to bind several
  // THttpServerGeneric on the same port, either within the same process, or as
  // separated processes (e.g. to set process affinity to one CPU HW socket)
  // - hsoThreadSmooting will change the TAsyncConnections.ThreadPollingWakeup()
  // algorithm to focus the process on the first threads of the pool - by design,
  // this will disable both hsoThreadCpuAffinity and hsoThreadSocketAffinity
  // - hsoEnablePipelining enable HTTP pipelining (unsafe) on THttpAsyncServer
  // - hsoEnableLogging enable an associated THttpServerGeneric.Logger instance
  // - hsoTelemetryCsv and hsoTelemetryJson will enable CSV or JSON consolidated
  // per-minute metrics logging via an associated THttpServerGeneric.Analyzer
  // - hsoContentTypeNoGuess will disable content-type detection from small
  // content buffers via GetMimeContentTypeFromBuffer()
  // - hsoRejectBotUserAgent identifies and rejects Bots via IsHttpUserAgentBot()
  THttpServerOption = (
    hsoHeadersUnfiltered,
    hsoHeadersInterning,
    hsoNoXPoweredHeader,
    hsoNoStats,
    hsoCreateSuspended,
    hsoLogVerbose,
    hsoIncludeDateHeader,
    hsoEnableTls,
    hsoBan40xIP,
    hsoThreadCpuAffinity,
    hsoThreadSocketAffinity,
    hsoReusePort,
    hsoThreadSmooting,
    hsoEnablePipelining,
    hsoEnableLogging,
    hsoTelemetryCsv,
    hsoTelemetryJson,
    hsoContentTypeNoGuess,
    hsoRejectBotUserAgent);

  /// how a THttpServerGeneric class is expected to process incoming requests
  THttpServerOptions = set of THttpServerOption;

  /// abstract parent class to implement a HTTP server
  // - do not use it, but rather THttpServer/THttpAsyncServer or THttpApiServer
  THttpServerGeneric = class(TLoggedThread)
  protected
    fShutdownInProgress, fFavIconRouted: boolean;
    fOptions: THttpServerOptions;
    fDefaultRequestOptions: THttpRequestOptions;
    fRoute: TUriRouter;
    /// optional event handlers for process interception
    fOnRequest: TOnHttpServerRequest;
    fOnBeforeBody: TOnHttpServerBeforeBody;
    fOnBeforeRequest: TOnHttpServerRequest;
    fOnAfterRequest: TOnHttpServerRequest;
    fOnAfterResponse: TOnHttpServerAfterResponse;
    fMaximumAllowedContentLength: Int64;
    fCurrentConnectionID: integer;  // 31-bit NextConnectionID sequence
    fCurrentProcess: integer;
    fCompressList: THttpSocketCompressList; /// set by RegisterCompress method
    fServerName: RawUtf8;
    fRequestHeaders: RawUtf8; // pre-computed headers with 'Server: xxxx'
    fCallbackSendDelay: PCardinal;
    fCallbackOutgoingCount: PCardinal; //TODO
    fRemoteIPHeader, fRemoteIPHeaderUpper: RawUtf8;
    fRemoteConnIDHeader, fRemoteConnIDHeaderUpper: RawUtf8;
    fOnSendFile: TOnHttpServerSendFile;
    fFavIcon: RawByteString;
    fRouterClass: TRadixTreeNodeClass;
    fLogger: THttpLogger;
    fAnalyzer: THttpAnalyzer;
    function GetApiVersion: RawUtf8; virtual; abstract;
    procedure SetRouterClass(aRouter: TRadixTreeNodeClass);
    procedure SetServerName(const aName: RawUtf8); virtual;
    procedure SetOptions(opt: THttpServerOptions);
    procedure SetOnRequest(const aRequest: TOnHttpServerRequest); virtual;
    procedure SetOnBeforeBody(const aEvent: TOnHttpServerBeforeBody); virtual;
    procedure SetOnBeforeRequest(const aEvent: TOnHttpServerRequest); virtual;
    procedure SetOnAfterRequest(const aEvent: TOnHttpServerRequest); virtual;
    procedure SetOnAfterResponse(const aEvent: TOnHttpServerAfterResponse); virtual;
    procedure SetMaximumAllowedContentLength(aMax: Int64); virtual;
    procedure SetRemoteIPHeader(const aHeader: RawUtf8); virtual;
    procedure SetRemoteConnIDHeader(const aHeader: RawUtf8); virtual;
    function GetHttpQueueLength: cardinal; virtual; abstract;
    procedure SetHttpQueueLength(aValue: cardinal); virtual; abstract;
    function GetConnectionsActive: cardinal; virtual; abstract;
    function DoBeforeRequest(Ctxt: THttpServerRequest): cardinal;
      {$ifdef HASINLINE}inline;{$endif}
    function DoAfterRequest(Ctxt: THttpServerRequest): cardinal;
      {$ifdef HASINLINE}inline;{$endif}
    function NextConnectionID: integer; // 31-bit internal sequence
    procedure ParseRemoteIPConnID(const Headers: RawUtf8;
      var RemoteIP: RawUtf8; var RemoteConnID: THttpServerConnectionID);
      {$ifdef HASINLINE}inline;{$endif}
    procedure AppendHttpDate(var Dest: TRawByteStringBuffer); virtual;
    function StatusCodeToText(Code: cardinal): PRawUtf8; virtual; // e.g. i18n
    function GetFavIcon(Ctxt: THttpServerRequestAbstract): cardinal;
  public
    /// initialize the server instance
    constructor Create(const OnStart, OnStop: TOnNotifyThread;
      const ProcessName: RawUtf8; ProcessOptions: THttpServerOptions;
      aLog: TSynLogClass); reintroduce; virtual;
    /// release all memory and handlers used by this server
    destructor Destroy; override;
    /// specify URI routes for internal URI rewrites or callback execution
    // - rules registered here will be processed before main Request/OnRequest
    // - URI rewrites allow to extend the default routing, e.g. from TRestServer
    // - callbacks execution allow efficient server-side processing with parameters
    // - static routes could be defined e.g. Route.Get('/', '/root/default')
    // - <param> place holders could be defined for proper URI rewrite
    // e.g. Route.Post('/user/<id>', '/root/userservice/new?id=<id>') will
    // rewrite internally '/user/1234' URI as '/root/userservice/new?id=1234'
    // - could be used e.g. for standard REST process via event callbacks with
    // Ctxt['user'] or Ctxt.RouteInt64('id') parameter extraction in DoUserPic:
    // ! Route.Run([urmGet], '/user/<user>/pic', DoUserPic) // retrieve a list
    // ! Route.Run([urmGet, urmPost, urmPut, urmDelete],
    // !    '/user/<user>/pic/<id>', DoUserPic) // CRUD picture access
    // - warning: with the THttpApiServer, URIs will be limited by the actual
    // root URI registered at http.sys level - there is no such limitation with
    // the socket servers, which bind to a port, so handle all URIs on it
    function Route: TUriRouter;
    /// thread-safe replace the TUriRouter instance
    // - returns the existing instance: caller should keep it for a few seconds
    // untouched prior to Free it, to let finish any pending background process
    function ReplaceRoute(another: TUriRouter): TUriRouter;
    /// will route a GET to /favicon.ico to the given .ico file content
    // - if none is supplied, the default Synopse/mORMot icon is used
    // - if '' is supplied, /favicon.ico will return a 404 error status
    // - warning: with THttpApiServer, may require a proper URI registration
    procedure SetFavIcon(const FavIconContent: RawByteString = 'default');
    /// used e.g. by TAcmeLetsEncryptServer to redirect TLS server name requests
    // - do nothing by default
    procedure SetTlsServerNameCallback(const OnAccept: TOnNetTlsAcceptServerName); virtual;
    /// override this function to customize your http server
    // - InURL/InMethod/InContent properties are input parameters
    // - OutContent/OutContentType/OutCustomHeader are output parameters
    // - result of the function is the HTTP error code (200 if OK, e.g.),
    // - OutCustomHeader is available to handle Content-Type/Location
    // - if OutContentType is STATICFILE_CONTENT_TYPE (i.e. '!STATICFILE'),
    // then OutContent is the UTF-8 filename of a file to be sent directly
    // to the client via http.sys or NGINX's X-Accel-Redirect; the
    // OutCustomHeader should contain the eventual 'Content-type: ....' value
    // - default implementation is to call the OnRequest event (if existing),
    // and will return HTTP_NOTFOUND if OnRequest was not set
    // - warning: this process must be thread-safe (can be called by several
    // threads simultaneously, but with a given Ctxt instance for each)
    function Request(Ctxt: THttpServerRequestAbstract): cardinal; virtual;
    /// send a request back to the client, if the connection has been upgraded
    // e.g. to WebSockets
    // - InURL/InMethod/InContent properties are input parameters
    // (InContentType is ignored)
    // - OutContent/OutContentType/OutCustomHeader are output parameters
    // - Ctxt.ConnectionID should be set, so that the method could know
    // which connnection is to be used - returns HTTP_NOTFOUND (404) if unknown
    // - result of the function is the HTTP error code (200 if OK, e.g.)
    // - warning: this void implementation will raise an EHttpServer exception -
    // inherited classes should override it, e.g. as in TWebSocketServerRest
    function Callback(Ctxt: THttpServerRequest; aNonBlocking: boolean): cardinal; virtual;
    /// send an asynchronous response to the client, when a slow process (e.g.
    // DB request) has been executed
    // - warning: this void implementation will raise an EHttpServer exception -
    // inherited classes should override it, e.g. as in THttpAsyncServer
    procedure AsyncResponse(Connection: TConnectionAsyncHandle;
      const Content, ContentType: RawUtf8; Status: cardinal = HTTP_SUCCESS); virtual;
    /// send an asynchronous (JSON by default) response to the client
    procedure AsyncResponseFmt(Connection: TConnectionAsyncHandle;
      const ContentFmt: RawUtf8; const Args: array of const;
      const ContentType: RawUtf8 = JSON_CONTENT_TYPE;
      Status: cardinal = HTTP_SUCCESS);
    /// send an asynchronous RTTI-serialized JSON response to the client
    procedure AsyncResponseJson(Connection: TConnectionAsyncHandle;
      Value: pointer; TypeInfo: PRttiInfo; Status: cardinal = HTTP_SUCCESS);
    /// send an asynchronous text error response to the client
    procedure AsyncResponseError(Connection: TConnectionAsyncHandle;
      const Message: RawUtf8; Status: cardinal = HTTP_SERVERERROR);

    /// will register a compression algorithm
    // - used e.g. to compress on the fly the data, with standard gzip/deflate
    // or custom (synlz) protocols
    // - you can specify a minimal size (in bytes) before which the content won't
    // be compressed (1024 by default, corresponding to a MTU of 1500 bytes)
    // - the first registered algorithm will be the prefered one for compression
    // within each priority level (the lower aPriority first)
    procedure RegisterCompress(aFunction: THttpSocketCompress;
      aCompressMinSize: integer = 1024; aPriority: integer = 10); virtual;
    /// you can call this method to prepare the HTTP server for shutting down
    procedure Shutdown;
    /// allow to customize the Route() implementation Radix Tree node class
    // - if not set, will use TUriTreeNode as defined in this unit
    // - raise an Exception if set twice, or after Route() is called
    property RouterClass: TRadixTreeNodeClass
      read fRouterClass write SetRouterClass;
    /// main event handler called by the default implementation of the
    // virtual Request method to process a given request
    // - OutCustomHeader will handle Content-Type/Location
    // - if OutContentType is STATICFILE_CONTENT_TYPE (i.e. '!STATICFILE'),
    // then OutContent is the UTF-8 filename of a file to be sent directly
    // to the client via http.sys or NGINX's X-Accel-Redirect; the
    // OutCustomHeader should contain the eventual 'Content-type: ....' value
    // - warning: this process must be thread-safe (can be called by several
    // threads simultaneously)
    property OnRequest: TOnHttpServerRequest
      read fOnRequest write SetOnRequest;
    /// event handler called just before the body is retrieved from the client
    // - should return HTTP_SUCCESS=200 to continue the process, or an HTTP
    // error code to reject the request immediately, and close the connection
    property OnBeforeBody: TOnHttpServerBeforeBody
      read fOnBeforeBody write SetOnBeforeBody;
    /// event handler called after HTTP body has been retrieved, before OnRequest
    // - may be used e.g. to return a HTTP_ACCEPTED (202) status to client and
    // continue a long-term job inside the OnRequest handler in the same thread;
    // or to modify incoming information before passing it to main business logic,
    // (header preprocessor, body encoding etc...)
    // - if the handler returns > 0 server will send a response immediately,
    // unless return code is HTTP_ACCEPTED (202), then OnRequest will be called
    // - warning: this handler must be thread-safe (could be called from several
    // threads), and is NOT called before Route() callbacks execution
    property OnBeforeRequest: TOnHttpServerRequest
      read fOnBeforeRequest write SetOnBeforeRequest;
    /// event handler called after request is processed but before response
    // is sent back to client
    // - main purpose is to apply post-processor, not part of request logic
    // - if handler returns value > 0 it will override the OnRequest response code
    // - warning: this handler must be thread-safe (could be called from several
    // threads), and is NOT called after Route() callbacks execution
    property OnAfterRequest: TOnHttpServerRequest
      read fOnAfterRequest write SetOnAfterRequest;
    /// event handler called after response is sent back to client
    // - main purpose is to apply post-response analysis, logging, etc...
    // - warning: this handler must be thread-safe (could be called from several
    // threads), and IS called after Route() callbacks execution
    property OnAfterResponse: TOnHttpServerAfterResponse
      read fOnAfterResponse write SetOnAfterResponse;
    /// event handler called after each working Thread is just initiated
    // - called in the thread context at first place in THttpServerGeneric.Execute
    property OnHttpThreadStart: TOnNotifyThread
      read fOnThreadStart write fOnThreadStart;
    /// event handler called when a working Thread is terminating
    // - called in the corresponding thread context
    // - the TThread.OnTerminate event will be called within a Synchronize()
    // wrapper, so it won't fit our purpose
    // - is used e.g. by TRest.EndCurrentThread for proper multi-threading
    property OnHttpThreadTerminate: TOnNotifyThread
      read fOnThreadTerminate write SetOnTerminate;
    /// reject any incoming request with a body size bigger than this value
    // - default to 0, meaning any input size is allowed
    // - returns HTTP_PAYLOADTOOLARGE = 413 error if "Content-Length" incoming
    // header overflow the supplied number of bytes
    property MaximumAllowedContentLength: Int64
      read fMaximumAllowedContentLength write SetMaximumAllowedContentLength;
    /// custom event handler used to send a local file for STATICFILE_CONTENT_TYPE
    // - see also NginxSendFileFrom() method
    property OnSendFile: TOnHttpServerSendFile
      read fOnSendFile write fOnSendFile;
    /// defines request/response internal queue length
    // - default value if 1000, which sounds fine for most use cases
    // - for THttpApiServer, will return 0 if the system does not support HTTP
    // API 2.0 (i.e. under Windows XP or Server 2003)
    // - for THttpServer or THttpAsyncServer, will shutdown any incoming accepted
    // socket if the internal number of pending requests exceed this limit
    // - increase this value if you don't have any load-balancing in place, and
    // in case of e.g. many 503 HTTP answers or if many "QueueFull" messages
    // appear in HTTP.sys log files (normally in
    // C:\Windows\System32\LogFiles\HTTPERR\httperr*.log) - may appear with
    // thousands of concurrent clients accessing at once the same server -
    // see @http://msdn.microsoft.com/en-us/library/windows/desktop/aa364501
    // - you can use this property with a reverse-proxy as load balancer, e.g.
    // with nginx configured as such:
    // $ location / {
    // $       proxy_pass              http://balancing_upstream;
    // $       proxy_next_upstream     error timeout invalid_header http_500 http_503;
    // $       proxy_connect_timeout   2;
    // $       proxy_set_header        Host            $host;
    // $       proxy_set_header        X-Real-IP       $remote_addr;
    // $       proxy_set_header        X-Forwarded-For $proxy_add_x_forwarded_for;
    // $       proxy_set_header        X-Conn-ID       $connection
    // $ }
    // see https://synopse.info/forum/viewtopic.php?pid=28174#p28174
    property HttpQueueLength: cardinal
      read GetHttpQueueLength write SetHttpQueueLength;
    /// returns the number of current HTTP connections
    // - may not include HTTP/1.0 short-living connections
    property ConnectionsActive: cardinal
      read GetConnectionsActive;
    /// returns the number of HTTP responses currently being transmitted
    // - is increased when the request is processed on the server side, or
    // transmitted back to the client
    // - incoming headers and body retrieval, or idle kept-alive HTTP/1.1
    // connections won't be included in this number
    property CurrentProcess: integer
      read fCurrentProcess;
    /// TRUE if the inherited class is able to handle callbacks
    // - only TWebSocketServer/TWebSocketAsyncServer have this ability by now
    function CanNotifyCallback: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// the value of a custom HTTP header containing the real client IP
    // - by default, the RemoteIP information will be retrieved from the socket
    // layer - but if the server runs behind some proxy service, you should
    // define here the HTTP header name which indicates the true remote client
    // IP value, mostly as 'X-Real-IP' or 'X-Forwarded-For'
    property RemoteIPHeader: RawUtf8
      read fRemoteIPHeader write SetRemoteIPHeader;
    /// the value of a custom HTTP header containing the real client connection ID
    // - by default, Ctxt.ConnectionID information will be retrieved from our
    // socket layer - but if the server runs behind some proxy service, you should
    // define here the HTTP header name which indicates the real remote connection,
    // for example as 'X-Conn-ID', setting in nginx config:
    //  $ proxy_set_header      X-Conn-ID       $connection
    property RemoteConnIDHeader: RawUtf8
      read fRemoteConnIDHeader write SetRemoteConnIDHeader;
  published
    /// the Server name, UTF-8 encoded, e.g. 'mORMot2 (Linux)'
    // - will be served as "Server: ..." HTTP header
    property ServerName: RawUtf8
      read fServerName write SetServerName;
    /// the associated process name
    property ProcessName: RawUtf8
      read fProcessName write fProcessName;
    /// returns the API version used by the inherited implementation
    property ApiVersion: RawUtf8
      read GetApiVersion;
    /// allow to customize this HTTP server instance
    // - some inherited classes may have only partial support of those options
    property Options: THttpServerOptions
      read fOptions write SetOptions;
    /// read access to the URI router, as published property (e.g. for logs)
    // - use the Route function to actually setup the routing
    // - may be nil if Route has never been accessed, i.e. no routing was set
    property Router: TUriRouter
      read fRoute;
    /// access to the HTTP logger initialized with hsoEnableLogging option
    // - you can customize the logging process via Logger.Format,
    // Logger.DestFolder, Logger.DefaultRotate, Logger.DefaultRotateFiles
    // properties and Logger.DefineHost() method
    // - equals nil if hsoEnableLogging was not set in the constructor
    property Logger: THttpLogger
      read fLogger;
    /// access to the HTTP analyzer initialized with hsoTelemetryCsv or
    // hsoTelemetryJson options
    // - you can customize this process via Analyzer.DestFolder
    property Analyzer: THttpAnalyzer
      read fAnalyzer;
  end;


const
  /// used to compute the request ConnectionFlags from the socket TLS state
  HTTP_TLS_FLAGS: array[{tls=}boolean] of THttpServerRequestFlags = (
    [],
    [hsrHttps, hsrSecured]);

  /// used to compute the request ConnectionFlags from connection: upgrade header
  HTTP_UPG_FLAGS: array[{tls=}boolean] of THttpServerRequestFlags = (
    [],
    [hsrConnectionUpgrade]);

  /// used to compute the request ConnectionFlags from HTTP/1.0 command
  HTTP_10_FLAGS: array[{http10=}boolean] of THttpServerRequestFlags = (
    [],
    [hsrHttp10]);

/// some pre-computed CryptCertOpenSsl[caaRS256].New key for Windows
// - the associated password is 'pass'
// - was generated via X509.ToPkcs12(p12Legacy), i.e. encoded as SHA1-3DES
// - as used e.g. by THttpServerSocketGeneric.WaitStartedHttps
function PrivKeyCertPfx: RawByteString;

/// initialize a server-side TLS structure with a self-signed algorithm
// - as used e.g. by THttpServerSocketGeneric.WaitStartedHttps
// - if OpenSSL is available and UsePreComputed is false, will
// generate a temporary pair of key files via
// Generate(CU_TLS_SERVER, '127.0.0.1', nil, 3650) with a random password
// - if UsePreComputed=true or on pure SChannel, will use the PrivKeyCertPfx
// pre-computed constant
procedure InitNetTlsContextSelfSignedServer(var TLS: TNetTlsContext;
  Algo: TCryptAsymAlgo = caaRS256; UsePreComputed: boolean = true);

/// used by THttpServerGeneric.SetFavIcon to return a nice /favicon.ico
function FavIconBinary: RawByteString;

type
  /// define how GetMacAddress() makes its sorting choices
  // - used e.g. for THttpPeerCacheSettings.InterfaceFilter property
  // - mafEthernetOnly will only select TMacAddress.Kind = makEthernet
  // - mafLocalOnly will only select makEthernet or makWifi adapters
  // - mafRequireBroadcast won't return any TMacAddress with Broadcast = ''
  // - mafIgnoreGateway won't put the TMacAddress.Gateway <> '' first
  // - mafIgnoreKind and mafIgnoreSpeed will ignore Kind or Speed properties
  TMacAddressFilter = set of (
    mafEthernetOnly,
    mafLocalOnly,
    mafRequireBroadcast,
    mafIgnoreGateway,
    mafIgnoreKind,
    mafIgnoreSpeed);

/// pickup the most suitable network according to some preferences
// - will sort GetMacAddresses() results according to its Kind and Speed
// to select the most suitable local interface e.g. for THttpPeerCache
function GetMainMacAddress(out Mac: TMacAddress;
  Filter: TMacAddressFilter = []): boolean; overload;

/// get a network interface from its TMacAddress main fields
// - search is case insensitive for TMacAddress.Name and Address fields or as
// exact IP, and eventually as CIDR pattern (e.g. '192.168.1.0/24')
function GetMainMacAddress(out Mac: TMacAddress;
  const InterfaceNameAddressOrIP: RawUtf8;
  UpAndDown: boolean = false): boolean; overload;



{ ******************** THttpServerSocket/THttpServer HTTP/1.1 Server }

type
  /// results of THttpServerSocket.GetRequest virtual method
  // - grClosed is returned if the socket was disconnected/closed by the client
  // - grException is returned if any exception occurred during the process
  // - grOversizedPayload is returned when MaximumAllowedContentLength is reached
  // - grRejected on invalid input, or when OnBeforeBody returned not 200
  // - grIntercepted is returned e.g. from OnHeaderParsed as valid result
  // - grTimeout is returned when HeaderRetrieveAbortDelay is reached
  // - grHeaderReceived is returned for GetRequest({withbody=}false)
  // - grBodyReceived is returned for GetRequest({withbody=}true)
  // - grWwwAuthenticate is returned if GetRequest() did send a 401 response
  // - grUpgraded indicates that this connection was upgraded e.g. as WebSockets
  // - grBanned is triggered by the hsoBan40xIP option or from Banned.BlackList
  THttpServerSocketGetRequestResult = (
    grClosed,
    grException,
    grOversizedPayload,
    grRejected,
    grIntercepted,
    grTimeout,
    grHeaderReceived,
    grBodyReceived,
    grWwwAuthenticate,
    grUpgraded,
    grBanned);

  {$M+} // to have existing RTTI for published properties
  THttpServer = class;
  {$M-}

  /// Socket API based HTTP/1.1 server class used by THttpServer Threads
  THttpServerSocket = class(THttpSocket)
  protected
    fRemoteConnectionID: THttpServerConnectionID;
    fServer: THttpServer;
    fKeepAliveClient: boolean;
    fAuthorized: THttpServerRequestAuthentication;
    fRequestFlags: THttpServerRequestFlags;
    fAuthTix32: cardinal; // 403 reject for 4 seconds, then 401 and retry login
    fConnectionOpaque: THttpServerConnectionOpaque; // two PtrUInt tags
    fResponseHeader: RawUtf8;
    // from TSynThreadPoolTHttpServer.Task
    procedure TaskProcess(aCaller: TSynThreadPoolWorkThread); virtual;
    function TaskProcessBody(aCaller: TSynThreadPoolWorkThread;
      aHeaderResult: THttpServerSocketGetRequestResult): boolean;
  public
    /// create the socket according to a server
    // - will register the THttpSocketCompress functions from the server
    // - once created, caller should call AcceptRequest() to accept the socket
    // - if TLS is enabled, ensure server certificates are initialized once
    constructor Create(aServer: THttpServer); reintroduce;
    /// main object function called after aClientSock := Accept + Create:
    // - get Command, Method, URL, Headers and Body (if withBody is TRUE)
    // - get sent data in Content (if withBody=true and ContentLength<>0)
    // - returned enumeration will indicates the processing state
    function GetRequest(withBody: boolean;
      headerMaxTix: Int64): THttpServerSocketGetRequestResult; virtual;
    /// access to the internal two PtrUInt tags of this connection
    // - may be nil behind a reverse proxy (i.e. Server.RemoteConnIDHeader<>'')
    function GetConnectionOpaque: PHttpServerConnectionOpaque;
      {$ifdef HASINLINE} inline; {$endif}
    /// contains the method ('GET','POST'.. e.g.) after GetRequest()
    property Method: RawUtf8
      read Http.CommandMethod;
    /// contains the URL ('/' e.g.) after GetRequest()
    property URL: RawUtf8
      read Http.CommandUri;
    /// true if the client is HTTP/1.1 and 'Connection: Close' is not set
    // - default HTTP/1.1 behavior is "keep alive", unless 'Connection: Close'
    // is specified, cf. RFC 2068 page 108: "HTTP/1.1 applications that do not
    // support persistent connections MUST include the "close" connection option
    // in every message"
    property KeepAliveClient: boolean
      read fKeepAliveClient write fKeepAliveClient;
    /// the recognized connection ID, after a call to GetRequest()
    // - identifies either the raw connection on the current server, or
    // the custom header value as set by a local proxy, e.g.
    // THttpServerGeneric.RemoteConnIDHeader='X-Conn-ID' for nginx
    property RemoteConnectionID: THttpServerConnectionID
      read fRemoteConnectionID;
    /// the associated HTTP Server instance - may be nil
    property Server: THttpServer
      read fServer;
  end;

  /// per-client HTTP response Thread as used by THttpServer Socket API Server
  // - Execute procedure get the request and calculate the answer, dedicating
  // this thread for one particular client connection, until it is closed
  // - you don't have to overload the protected THttpServerResp Execute method:
  // override THttpServer.Request() function or, if you need a lower-level access
  // (change the protocol, e.g.) THttpServer.Process() method itself
  THttpServerResp = class(TSynThread)
  protected
    fConnectionID: THttpServerConnectionID;
    fServer: THttpServer;
    fServerSock: THttpServerSocket;
    fClientSock: TNetSocket;
    fClientSin: TNetAddr;
    /// main thread loop: read request from socket, send back answer
    procedure Execute; override;
  public
    /// initialize the response thread for the corresponding incoming socket
    // - this version will get the request directly from an incoming socket
    constructor Create(aSock: TNetSocket; const aSin: TNetAddr;
      aServer: THttpServer); reintroduce; overload;
    /// initialize the response thread for the corresponding incoming socket
    // - this version will handle KeepAlive, for such an incoming request
    constructor Create(aServerSock: THttpServerSocket; aServer: THttpServer);
      reintroduce; overload; virtual;
    /// called by THttpServer.Destroy on existing connections
    // - set Terminate and close the socket
    procedure Shutdown; virtual;
    /// the associated socket to communicate with the client
    property ServerSock: THttpServerSocket
      read fServerSock;
    /// the associated main HTTP server instance
    property Server: THttpServer
      read fServer;
    /// the unique identifier of this connection
    property ConnectionID: THttpServerConnectionID
      read fConnectionID;
  end;

  /// metaclass of HTTP response Thread
  THttpServerRespClass = class of THttpServerResp;

  /// a simple Thread Pool, used for fast handling HTTP requests of a THttpServer
  // - will handle multi-connection with less overhead than creating a thread
  // for each incoming request
  // - will create a THttpServerResp response thread, if the incoming request is
  // identified as HTTP/1.1 keep alive, or HTTP body length is bigger than 1 MB
  TSynThreadPoolTHttpServer = class(TSynThreadPool)
  protected
    fServer: THttpServer;
    fBigBodySize: integer;
    fMaxBodyThreadCount: integer;
    {$ifndef USE_WINIOCP}
    function QueueLength: integer; override;
    {$endif USE_WINIOCP}
    // here aContext is a THttpServerSocket instance
    procedure Task(aCaller: TSynThreadPoolWorkThread;
      aContext: pointer); override;
    procedure TaskAbort(aContext: pointer); override;
  public
    /// initialize a thread pool with the supplied number of threads
    // - Task() overridden method processs the HTTP request set by Push()
    // - up to 256 threads can be associated to a Thread Pool
    constructor Create(Server: THttpServer;
      NumberOfThreads: integer = 32); reintroduce;
    /// when Content-Length is bigger than this value, by-pass the threadpool
    // and creates a dedicated THttpServerResp thread
    // - default is THREADPOOL_BIGBODYSIZE = 16 MB, but can set a bigger value
    // e.g. behind a buffering proxy if you trust the client not to make DOD
    property BigBodySize: integer
      read fBigBodySize write fBigBodySize;
    /// how many stand-alone THttpServerResp threads can be initialized when a
    // HTTP request comes in
    // - default is THREADPOOL_MAXWORKTHREADS = 512, but since each thread
    // consume system memory, you should not go so high
    // - above this value, the thread pool will be used
    property MaxBodyThreadCount: integer
      read fMaxBodyThreadCount write fMaxBodyThreadCount;
  end;

  /// meta-class of the THttpServerSocket process
  // - used to override THttpServerSocket.GetRequest for instance
  THttpServerSocketClass = class of THttpServerSocket;

  /// callback used by THttpServerSocketGeneric.SetAuthorizeBasic
  // - should return true if supplied aUser/aPassword pair is valid
  TOnHttpServerBasicAuth = function(Sender: TObject;
    const aUser: RawUtf8; const aPassword: SpiUtf8): boolean of object;

  /// THttpServerSocketGeneric current state
  THttpServerExecuteState = (
    esNotStarted,
    esBinding,
    esRunning,
    esFinished);

  /// abstract parent class for both THttpServer and THttpAsyncServer
  THttpServerSocketGeneric = class(THttpServerGeneric)
  protected
    fSafe: TLightLock; // topmost to ensure proper aarch64 alignment
    fServerKeepAliveTimeOut: cardinal;    // in ms - 0 to disable keep alive
    fServerKeepAliveTimeOutSec: cardinal; // in seconds - for THttpAsyncServer
    fHeaderRetrieveAbortDelay: cardinal;
    fCompressGz: integer; // >=0 if GZ is activated
    fSockPort: RawUtf8;
    fSock: TCrtSocket; // the server socket, setup after Bind()
    fExecuteMessage: RawUtf8;
    fNginxSendFileFrom: array of TFileName;
    fAuthorize: THttpServerRequestAuthentication;
    fAuthorizerBasic: IBasicAuthServer;
    fAuthorizerDigest: IDigestAuthServer;
    fAuthorizeBasic: TOnHttpServerBasicAuth;
    fAuthorizeBasicRealm: RawUtf8;
    fStats: array[THttpServerSocketGetRequestResult] of integer;
    fBlackListUriReloadMin: integer;
    fBlackListUriNextTix, fBlackListUriCrc: cardinal;
    fBlackListUri: RawUtf8;
    fProgressiveRequests: THttpPartials;
    {$ifdef OSPOSIX} // keytab files is a POSIX/GSSAPI specific feature
    fSspiKeyTab: TServerSspiKeyTab;
    procedure SetKeyTab(const aKeyTab: TFileName);
    function GetKeyTab: TFileName;
    {$endif OSPOSIX}
    function HeaderRetrieveAbortTix: Int64;
    function DoRequest(Ctxt: THttpServerRequest): boolean; // fRoute or Request()
    function DoProcessBody(var Ctxt: THttpRequestContext;
      var Dest: TRawByteStringBuffer; MaxSize: PtrInt): THttpRequestProcessBody;
    procedure DoProgressiveRequestFree(var Ctxt: THttpRequestContext);
    procedure SetServerKeepAliveTimeOut(Value: cardinal);
    function GetStat(one: THttpServerSocketGetRequestResult): integer;
    procedure IncStat(one: THttpServerSocketGetRequestResult);
      {$ifdef HASINLINE} inline; {$endif}
    function OnNginxAllowSend(Context: THttpServerRequestAbstract;
      const LocalFileName: TFileName): boolean;
    // this overridden version will return e.g. 'Winsock 2.514'
    function GetApiVersion: RawUtf8; override;
    function GetExecuteState: THttpServerExecuteState; virtual; abstract;
    function GetBanned: THttpAcceptBan; virtual; abstract;
    function GetRegisterCompressGzStatic: boolean;
    procedure SetRegisterCompressGzStatic(Value: boolean);
    function ComputeWwwAuthenticate(Opaque: Int64): RawUtf8;
    function ComputeRejectBody(var Body: RawByteString; Opaque: Int64;
      Status: integer): boolean; virtual; // return true for grWwwAuthenticate
    function Authorization(var Http: THttpRequestContext;
      Opaque: Int64): TAuthServerResult;
    procedure SetBlackListUri(const Uri: RawUtf8);
    procedure SetBlackListUriReloadMin(Minutes: integer);
    procedure RefreshBlackListUri(tix32: cardinal);
    procedure RefreshBlackListUriExecute(Sender: TObject);
  public
    /// create a Server Thread, ready to be bound and listening on a port
    // - this constructor will raise a EHttpServer exception if binding failed
    // - expects the port to be specified as string, e.g. '1234'; you can
    // optionally specify a server address to bind to, e.g. '1.2.3.4:1234'
    // - can listed to local Unix Domain Sockets file in case port is prefixed
    // with 'unix:', e.g. 'unix:/run/myapp.sock' - faster and safer than TCP
    // - on Linux in case aPort is empty string will check if external fd
    // is passed by systemd and use it (so called systemd socked activation)
    // - you can specify a number of threads to be initialized to handle
    // incoming connections. Default is 32, which may be sufficient for most
    // cases, maximum is 256. If you set 0, the thread pool will be disabled
    // and one thread will be created for any incoming connection
    // - you can also tune (or disable with 0) HTTP/1.1 keep alive delay and
    // how incoming request Headers[] are pushed to the processing method
    // - this constructor won't actually do the port binding, which occurs in
    // the background thread: caller should therefore call WaitStarted after
    // THttpServer.Create()
    constructor Create(const aPort: RawUtf8; const OnStart, OnStop: TOnNotifyThread;
      const ProcessName: RawUtf8; ServerThreadPoolCount: integer = 32;
      KeepAliveTimeOut: integer = 30000; ProcessOptions: THttpServerOptions = [];
      aLog: TSynLogClass = nil); reintroduce; virtual;
    /// finalize this server instance
    destructor Destroy; override;
    /// defines the WebSockets protocols to be used for this Server
    // - this default implementation will raise an exception
    // - returns the associated PWebSocketProcessSettings reference on success
    function WebSocketsEnable(
      const aWebSocketsURI, aWebSocketsEncryptionKey: RawUtf8;
      aWebSocketsAjax: boolean = false;
      aWebSocketsBinaryOptions: TWebSocketProtocolBinaryOptions =
        [pboSynLzCompress]): pointer; virtual;
    /// ensure the HTTP server thread is actually bound to the specified port
    // - TCrtSocket.Bind() occurs in the background in the Execute method: you
    // should call and check this method result just after THttpServer.Create
    // - initial THttpServer design was to call Bind() within Create, which
    // works fine on Delphi + Windows, but fails with a EThreadError on FPC/Linux
    // - raise a EHttpServer if binding failed within the specified period (if
    // port is free, it would be almost immediate)
    // - calling this method is optional, but if the background thread didn't
    // actually bind the port, the server will be stopped and unresponsive with
    // no explicit error message, until it is terminated
    // - for hsoEnableTls support, you should either specify the private key
    // and certificate here, or set TLS.PrivateKeyFile/CertificateFile fields -
    // the benefit of this method parameters is that the certificates are
    // loaded and checked now by calling InitializeTlsAfterBind, not at the
    // first client connection (which may be too late)
    procedure WaitStarted(Seconds: integer; const CertificateFile: TFileName;
      const PrivateKeyFile: TFileName = ''; const PrivateKeyPassword: RawUtf8 = '';
      const CACertificatesFile: TFileName = ''); overload;
    /// ensure the HTTP server thread is actually bound to the specified port
    // - for hsoEnableTls support, allow to specify all server-side TLS
    // events, including callbacks, as supported by OpenSSL
    // - will raise EHttpServer if the server did not start properly, e.g.
    // could not bind the port within the supplied time
    procedure WaitStarted(Seconds: integer = 30; TLS: PNetTlsContext = nil); overload;
    /// ensure the server thread is bound as self-signed HTTPS server
    // - wrap InitNetTlsContextSelfSignedServer() and WaitStarted() with
    // some temporary key files, which are deleted once started
    // - as used e.g. by TRestHttpServer for secTLSSelfSigned
    procedure WaitStartedHttps(Seconds: integer = 30;
      UsePreComputed: boolean = true);
    /// could be called after WaitStarted(seconds,'','','') to setup TLS
    // - validate Sock.TLS.CertificateFile/PrivateKeyFile/PrivatePassword
    // - otherwise TLS is initialized at first incoming connection, which
    // could be too late in case of invalid Sock.TLS parameters
    procedure InitializeTlsAfterBind;
    /// overriden to properly support TLS Server Name lookup
    procedure SetTlsServerNameCallback(const OnAccept: TOnNetTlsAcceptServerName); override;
    /// remove any previous authorization, i.e. any previous SetAuthorizeBasic /
    // SetAuthorizeDigest / SetAuthorizeKerberos call
    procedure SetAuthorizeNone;
    /// allow optional BASIC authentication for some URIs via a callback
    // - if OnBeforeBody returns 401, the OnBasicAuth callback will be executed
    // to negotiate Basic authentication with the client
    procedure SetAuthorizeBasic(const BasicRealm: RawUtf8;
      const OnBasicAuth: TOnHttpServerBasicAuth); overload;
    /// allow optional BASIC authentication for some URIs via IBasicAuthServer
    // - if OnBeforeBody returns 401, Digester.OnCheckCredential will
    // be called to negotiate Basic authentication with the client
    // - the supplied Digester will be owned by this instance: it could be
    // either a TDigestAuthServerFile with its own storage, or a
    // TDigestAuthServerMem instance expecting manual SetCredential() calls
    procedure SetAuthorizeBasic(const Basic: IBasicAuthServer); overload;
    /// allow optional DIGEST authentication for some URIs
    // - if OnBeforeBody returns 401, Digester.ServerInit and ServerAuth will
    // be called to negotiate Digest authentication with the client
    // - the supplied Digester will be owned by this instance - typical
    // use is with a TDigestAuthServerFile
    procedure SetAuthorizeDigest(const Digest: IDigestAuthServer);
    /// allow optional NEGOTIATE authentication for some URIs via Kerberos
    // - will use mormot.lib.sspi or mormot.lib.gssapi
    // - if OnBeforeBody returns 401, Kerberos will be used to authenticate
    procedure SetAuthorizeNegotiate;
    /// set after a call to SetAuthDigest/SetAuthBasic/SetAuthorizeNegotiate
    property Authorize: THttpServerRequestAuthentication
      read fAuthorize;
    /// set after a call to SetAuthDigest/SetAuthBasic
    // - return nil if no such call was made, or not with a TDigestAuthServerMem
    // - return a TDigestAuthServerMem so that SetCredential/GetUsers are available
    function AuthorizeServerMem: TDigestAuthServerMem;
    /// enable NGINX X-Accel internal redirection for STATICFILE_CONTENT_TYPE
    // - will define internally a matching OnSendFile event handler
    // - generating "X-Accel-Redirect: " header, trimming any supplied left
    // case-sensitive file name prefix, e.g. with NginxSendFileFrom('/var/www'):
    // $ # Will serve /var/www/protected_files/myfile.tar.gz
    // $ # When passed URI /protected_files/myfile.tar.gz
    // $ location /protected_files {
    // $  internal;
    // $  root /var/www;
    // $ }
    // - call this method several times to register several folders
    procedure NginxSendFileFrom(const FileNameLeftTrim: TFileName);
    /// milliseconds delay to reject a connection due to too long header retrieval
    // - default is 0, i.e. not checked (typical behind a reverse proxy)
    property HeaderRetrieveAbortDelay: cardinal
      read fHeaderRetrieveAbortDelay write fHeaderRetrieveAbortDelay;
    /// the low-level thread execution thread
    property ExecuteState: THttpServerExecuteState
      read GetExecuteState;
    /// access to the main server low-level Socket
    // - it's a raw TCrtSocket, which only need a socket to be bound, listening
    // and accept incoming request
    // - for THttpServer inherited class, will own its own instance, then
    // THttpServerSocket/THttpServerResp are created for every connection
    // - for THttpAsyncServer inherited class, redirect to TAsyncServer.fServer
    property Sock: TCrtSocket
      read fSock;
    /// access to the the IPv4 banning list, via hsoBan40xIP option or BlackList
    property Banned: THttpAcceptBan
      read GetBanned;
    /// a public URI containing a black list of IP or CIDR as text
    // - typically set to 'https://www.spamhaus.org/drop/drop.txt' or
    // 'https://raw.githubusercontent.com/firehol/blocklist-ipsets/refs/heads/master/firehol_level1.netset'
    // - would be assigned to Banned.BlackList, and checked daily
    property BlackListUri: RawUtf8
      read fBlackListUri write SetBlackListUri;
    /// the period to reload and activate a remote BlackListUri list
    // - default is 1440 minutes, i.e. get and refresh on a daily basis
    property BlaclListUriReloadMin: integer
      read fBlackListUriReloadMin write SetBlackListUriReloadMin;
  published
    /// the bound TCP port, as specified to Create() constructor
    property SockPort: RawUtf8
      read fSockPort;
    /// time, in milliseconds, for the HTTP/1.1 connections to be kept alive
    // - default is 30000 ms, i.e. 30 seconds
    // - setting 0 here (or in KeepAliveTimeOut constructor parameter) will
    // disable keep-alive, and fallback to HTTP.1/0 for all incoming requests
    // (may be a good idea e.g. behind a NGINX reverse proxy)
    // - see THttpApiServer.SetTimeOutLimits(aIdleConnection) parameter
    property ServerKeepAliveTimeOut: cardinal
      read fServerKeepAliveTimeOut write SetServerKeepAliveTimeOut;
    /// internal error message set by the processing thread, e.g. at binding
    property ExecuteMessage: RawUtf8
      read fExecuteMessage;
    /// if we should search for local .gz cached file when serving static files
    property RegisterCompressGzStatic: boolean
      read GetRegisterCompressGzStatic write SetRegisterCompressGzStatic;
    /// how many HTTP connections have been closed
    property StatHeaderClosed: integer
      index grClosed read GetStat;
    /// how many invalid HTTP headers raised an exception
    property StatHeaderException: integer
      index grException read GetStat;
    /// how many HTTP requests pushed more than MaximumAllowedContentLength bytes
    property StatOversizedPayloads: integer
      index grOversizedPayload read GetStat;
    /// how many HTTP requests were rejected by the OnBeforeBody event handler
    property StatRejected: integer
      index grRejected read GetStat;
    /// how many HTTP requests were intercepted by the OnHeaderParser event handler
    property StatIntercepted: integer
      index grIntercepted read GetStat;
    /// how many HTTP requests were rejected after HeaderRetrieveAbortDelay timeout
    property StatHeaderTimeout: integer
      index grTimeout read GetStat;
    /// how many HTTP headers have been processed
    property StatHeaderProcessed: integer
      index grHeaderReceived read GetStat;
    /// how many HTTP bodies have been processed
    property StatBodyProcessed: integer
      index grBodyReceived read GetStat;
    /// how many HTTP 401 "WWW-Authenticate:" responses have been returned
    property StatWwwAuthenticate: integer
      index grWwwAuthenticate read GetStat;
    /// how many HTTP connections were upgraded e.g. to WebSockets
    property StatUpgraded: integer
      index grUpgraded read GetStat;
    /// how many HTTP connections have been not accepted by hsoBan40xIP option
    // or from Banned.BlackList registered IPv4/CIDR
    property StatBanned: integer
      index grBanned read GetStat;
    {$ifdef OSPOSIX}
    /// the keytab file name propagated to all server threads (POSIX only)
    property KeyTab: TFileName
      read GetKeyTab write SetKeyTab;
    {$endif OSPOSIX}
  end;

  /// meta-class of our THttpServerSocketGeneric classes
  // - typically implemented by THttpServer, TWebSocketServer,
  // TWebSocketServerRest or THttpAsyncServer classes
  THttpServerSocketGenericClass = class of THttpServerSocketGeneric;

  /// called from THttpServerSocket.GetRequest before OnBeforeBody
  // - this THttpServer-specific callback allow quick and dirty action on the
  // raw socket, to bypass the whole THttpServer.Process high-level action
  // - should return grRejected/grIntercepted if the action has been handled as
  // error or success, and response has been sent directly via
  // ClientSock.SockSend/SockSendFlush (as HTTP/1.0) by this handler
  // - should return grHeaderReceived to continue as usual with THttpServer.Process
  TOnHttpServerHeaderParsed = function(
    ClientSock: THttpServerSocket): THttpServerSocketGetRequestResult of object;

  /// main HTTP server Thread using the standard Sockets API (e.g. WinSock)
  // - bind to a port and listen to incoming requests
  // - assign this requests to THttpServerResp threads from a ThreadPool
  // - it implements a HTTP/1.1 compatible server, according to RFC 2068 specifications
  // - if the client is also HTTP/1.1 compatible, KeepAlive connection is handled:
  //  multiple requests will use the existing connection and thread;
  //  this is faster and uses less resources, especialy under Windows
  // - a Thread Pool is used internally to speed up HTTP/1.0 connections - a
  // typical use, under Linux, is to run this class behind a NGINX frontend,
  // configured as https reverse proxy, leaving default "proxy_http_version 1.0"
  // and "proxy_request_buffering on" options for best performance, and
  // setting KeepAliveTimeOut=0 in the THttpServer.Create constructor
  // - consider using THttpAsyncServer from mormot.net.async if a very high
  // number of concurrent connections is expected, e.g. if using HTTP/1.0 behind
  // a https reverse proxy is not an option
  // - under Windows, will trigger the firewall UAC popup at first run
  // - don't forget to use Free method when you are finished
  // - a typical HTTPS server usecase could be:
  // ! fHttpServer := THttpServer.Create('443', nil, nil, '', 32, 30000, [hsoEnableTls]);
  // ! fHttpServer.WaitStarted('cert.pem', 'privkey.pem', '');  // cert.pfx for SSPI
  // ! // now certificates will be initialized and used
  THttpServer = class(THttpServerSocketGeneric)
  protected
    fThreadPool: TSynThreadPoolTHttpServer;
    fInternalHttpServerRespList: TSynObjectListLocked;
    fSocketClass: THttpServerSocketClass;
    fThreadRespClass: THttpServerRespClass;
    fHttpQueueLength: cardinal;
    fServerConnectionCount: integer;
    fServerConnectionActive: integer;
    fServerSendBufferSize: integer;
    fBanSec: integer;
    fExecuteState: THttpServerExecuteState;
    fMonoThread: boolean;
    fOnHeaderParsed: TOnHttpServerHeaderParsed;
    fBanned: THttpAcceptBan; // for hsoBan40xIP or BlackList
    fOnAcceptIdle: TOnPollSocketsIdle;
    function GetExecuteState: THttpServerExecuteState; override;
    function GetBanned: THttpAcceptBan; override;
    function GetHttpQueueLength: cardinal; override;
    procedure SetHttpQueueLength(aValue: cardinal); override;
    function GetConnectionsActive: cardinal; override;
    procedure DoCallbacks(tix64: Int64; sec32: integer);
    /// server main loop - don't change directly
    procedure DoExecute; override;
    /// this method is called on every new client connection, i.e. every time
    // a THttpServerResp thread is created with a new incoming socket
    procedure OnConnect; virtual;
    /// this method is called on every client disconnection to update stats
    procedure OnDisconnect; virtual;
    /// override this function in order to low-level process the request;
    // default process is to get headers, and call public function Request
    procedure Process(ClientSock: THttpServerSocket;
      ConnectionID: THttpServerConnectionID; ConnectionThread: TSynThread); virtual;
  public
    /// create a socket-based HTTP Server, ready to be bound and listening on a port
    // - ServerThreadPoolCount < 0 would use a single thread to rule them all
    // - ServerThreadPoolCount = 0 would create one thread per connection
    // - ServerThreadPoolCount > 0 would leverage the thread pool, and create
    // one thread only for kept-alive or upgraded connections
    constructor Create(const aPort: RawUtf8;
      const OnStart, OnStop: TOnNotifyThread; const ProcessName: RawUtf8;
      ServerThreadPoolCount: integer = 32; KeepAliveTimeOut: integer = 30000;
      ProcessOptions: THttpServerOptions = []; aLog: TSynLogClass = nil); override;
    /// release all memory and handlers
    destructor Destroy; override;
    /// low-level callback called before OnBeforeBody and allow quick execution
    // directly from THttpServerSocket.GetRequest
    property OnHeaderParsed: TOnHttpServerHeaderParsed
      read fOnHeaderParsed write fOnHeaderParsed;
    /// low-level callback called every few seconds of inactive Accept()
    // - is called every 5 seconds by default, but could be every second
    // if hsoBan40xIP option (i.e. the Banned property) has been set
    // - on Windows, requires some requests to trigger the event, because it
    // seems that accept() has timeout only on POSIX systems
    property OnAcceptIdle: TOnPollSocketsIdle
      read fOnAcceptIdle write fOnAcceptIdle;
  published
    /// will contain the current number of connections to the server
    property ServerConnectionActive: integer
      read fServerConnectionActive write fServerConnectionActive;
    /// will contain the total number of connections to the server
    // - it's the global count since the server started
    property ServerConnectionCount: integer
      read fServerConnectionCount write fServerConnectionCount;
    /// the associated thread pool
    // - may be nil if ServerThreadPoolCount was 0 on constructor
    property ThreadPool: TSynThreadPoolTHttpServer
      read fThreadPool;
    /// access Banned.BlackList or if hsoBan40xIP has been defined
    // - handle efficient accept() IPv4 ASAP filtering
    // - you can customize its behavior once the server is started by resetting
    // its Seconds/Max/WhiteIP properties, before any connections are made
    property Banned: THttpAcceptBan
      read fBanned;
  end;


const
  // kept-alive or big HTTP requests will create a dedicated THttpServerResp
  // - each thread reserves 2 MB of memory so it may break the server
  // - keep the value to a decent number, to let resources be constrained up to 1GB
  // - is the default value to TSynThreadPoolTHttpServer.MaxBodyThreadCount
  THREADPOOL_MAXWORKTHREADS = 512;

  /// if HTTP body length is bigger than 16 MB, creates a dedicated THttpServerResp
  // - is the default value to TSynThreadPoolTHttpServer.BigBodySize
  THREADPOOL_BIGBODYSIZE = 16 * 1024 * 1024;

function ToText(res: THttpServerSocketGetRequestResult): PShortString; overload;
function ToText(state: THttpServerExecuteState): PShortString; overload;

/// create an ephemeral socket-based HTTP Server, for a single request
// - typical usage is for Single-Sign On credential entering
// - raise an Exception on binding error
// - returns false on timeout
// - returns true on success, with encoded parameters as aParams - and the
// received URL/Method/Content values as aParams.U['url'/'method'/'content']
function EphemeralHttpServer(const aPort: RawUtf8; out aParams: TDocVariantData;
  aTimeOutSecs: integer = 60; aLogClass: TSynLogClass = nil;
  const aResponse: RawUtf8 = 'You can close this window.';
  aMethods: TUriMethods = [mGET, mPOST]; aOptions: THttpServerOptions = []): boolean;



{ ******************** THttpPeerCache Local Peer-to-peer Cache }

{
  TODO:
  - Daemon/Service background mode for the mget tool
  - Asymmetric security using ECDH shared secret? use HTTPS instead?
  - Frame signature using ECDHE with ephemeral keys? use HTTPS instead?
  - Allow binding to several network interfaces? (e.g. wifi to/from ethernet)
}

type
  /// the content of a binary THttpPeerCacheMessage
  // - could eventually be extended in the future for frame versioning
  THttpPeerCacheMessageKind = (
    pcfPing,
    pcfPong,
    pcfRequest,
    pcfResponseNone,
    pcfResponseOverloaded,
    pcfResponsePartial,
    pcfResponseFull,
    pcfBearer,
    pcfBearerDirect,
    pcfBearerDirectPermanent);

  /// one UDP request frame used during THttpPeerCache discovery
  // - requests and responses have the same binary layout
  // - some fields may be void or irrelevant, and the structure is padded
  // with random up to 192 bytes
  // - over the wire, packets are encrypted and authenticated via AES-GCM-128
  // with an ending salted checksum for quick anti-fuzzing (as 212 bytes blocks)
  THttpPeerCacheMessage = packed record
    /// the content of this binary frame
    Kind: THttpPeerCacheMessageKind;
    /// 32-bit sequence number
    Seq: cardinal;
    /// the UUID of the Sender
    Uuid: TGuid;
    /// the Operating System of the Sender
    Os: TOperatingSystemVersion;
    /// the local IPv4 which sent this frame
    // - e.g. 192.168.1.1
    IP4: cardinal;
    /// the destination IPv4 of this frame
    // - contains 0 for a broadcast
    // - allows to filter response frames when broadcasted on POSIX
    DestIP4: cardinal;
    /// the IPv4 network mask of the local network interface
    // - e.g. 255.255.255.0
    MaskIP4: cardinal;
    /// the IPv4 broadcast address the local network interface
    // - e.g. 192.168.1.255
    BroadcastIP4: cardinal;
    /// the link speed (in Mbits per second) of the local network interface
    Speed: cardinal;
    /// the hardware model of this network interface
    Hardware: TMacAddressKind;
    /// the local UnixTimeMinimalUtc value
    Timestamp: TUnixTimeMinimal;
    /// number of background download connections currently on this server
    Connections: word;
    /// up to 512-bit of binary Hash (and algo) of the requested file content
    Hash: THashDigest;
    /// the known full size of this file
    Size: Int64;
    /// the Range offset of the requested file content
    RangeStart: Int64;
    /// the Range ending position of the file content (included)
    RangeEnd: Int64;
    /// some internal state representation, e.g. sent back as pcfBearer
    Opaque: QWord;
    /// define the kind of content in the padding block - always 0 by now
    PaddingVersion: byte;
    /// some random padding up to 192 bytes, used for future content revisions
    // - e.g. for a TEccPublicKey (ECDHE) and additional fields
    // - using random enhances AES-GCM obfuscation by making it unpredictable
    Padding: array[0 .. 41] of byte;
  end;
  PHttpPeerCacheMessage = ^THttpPeerCacheMessage;
  THttpPeerCacheMessageDynArray = array of THttpPeerCacheMessage;

  /// AES-GCM encoded binary frame of THttpPeerCacheMessage
  THttpPeerCacheMessageEncoded = packed record
    iv: THash128;
    msg: THttpPeerCacheMessage;
    padding, mac: THash128;
    crc: cardinal;
  end;
  PHttpPeerCacheMessageEncoded = ^THttpPeerCacheMessageEncoded;

  /// each THttpPeerCacheSettings.Options item
  // - pcoCacheTempSubFolders will create 16 sub-folders (from first 0-9/a-f
  // hash nibble) within CacheTempPath to reduce filesystem fragmentation
  // - pcoCacheTempNoCheckSize will ignore checking CacheTempMaxMB ratio on disk
  // - pcoUseFirstResponse will accept the first positive response, and don't
  // wait for the BroadcastTimeoutMS delay for all responses to be received
  // - pcoTryLastPeer will first check the latest peer with HTTP/TCP before
  // making any broadcast - to be used if the files are likely to come in batch;
  // can be forced by TWGetAlternateOptions from a given WGet() call
  // - pcoTryAllPeers will try up to the best 10 braodcast responses, before
  // falling back to the main server
  // - pcoBroadcastNotAlone will disable broadcasting for up to one second if
  // no response at all was received within BroadcastTimeoutMS delay
  // - pcoNoServer disable the local UDP/HTTP servers and acts as a pure client
  // - pcoNoBanIP disable the 4 seconds IP banishment mechanism at HTTP level;
  // set RejectInstablePeersMin = 0 to disable banishment at UDP level
  // - pcoSelfSignedHttps enables HTTPS communication with a self-signed server
  // (warning: this option should be set on all peers, clients and servers) -
  // as an alternative, you could set THttpPeerCache.ServerTls/ClientTls props
  // - pcoVerboseLog will log all details, e.g. raw UDP frames
  // - pcoHttpDirect extends the HTTP endpoint to initiate a download process
  // from localhost, and return the cached content (used e.g. as proxy + cache)
  // using a URI + Bearer returned by HttpDirectUri() overloaded methods
  // - pcoHttpDirectNoBroadcast would disable UDP peer search for pcoHttpDirect
  // - pcoHttpDirectTryLastPeer could make pcoTryLastPeer for pcoHttpDirect
  // - pcoHttpReprDigest would include a 'Repr-Digest:' header as per RFC 9530
  THttpPeerCacheOption = (
    pcoCacheTempSubFolders,
    pcoCacheTempNoCheckSize,
    pcoUseFirstResponse,
    pcoTryLastPeer,
    pcoTryAllPeers,
    pcoBroadcastNotAlone,
    pcoNoServer,
    pcoNoBanIP,
    pcoSelfSignedHttps,
    pcoVerboseLog,
    pcoHttpDirect,
    pcoHttpDirectNoBroadcast,
    pcoHttpDirectTryLastPeer,
    pcoHttpReprDigest);

  /// THttpPeerCacheSettings.Options values
  THttpPeerCacheOptions = set of THttpPeerCacheOption;

  /// define how THttpPeerCache handles its process
  THttpPeerCacheSettings = class(TSynPersistent)
  protected
    fPort: TNetPort;
    fInterfaceFilter: TMacAddressFilter;
    fOptions: THttpPeerCacheOptions;
    fLimitMBPerSec, fLimitClientCount,
    fBroadcastTimeoutMS, fBroadcastMaxResponses, fBroadCastDirectMinBytes,
    fTryAllPeersCount, fHttpTimeoutMS, fRejectInstablePeersMin: integer;
    fCacheTempMaxMB, fCacheTempMaxMin,
    fCacheTempMinBytes, fCachePermMinBytes: integer;
    fCacheTempPath, fCachePermPath: TFileName;
    fInterfaceName, fUuid: RawUtf8;
  public
    /// set the default settings
    // - i.e. Port=8099, LimitMBPerSec=10, LimitClientCount=32,
    // RejectInstablePeersMin=4, CacheTempMaxMB=1000, CacheTempMaxMin=60,
    // CacheTempMinBytes=CachePermMinBytes=2048, BroadCastDirectMinBytes=65536,
    // BroadcastTimeoutMS=10 HttpTimeoutMS=500 and BroadcastMaxResponses=24
    constructor Create; override;
    /// retrieve the network interface fulfilling these settings
    // - network layout may change in real time: this method allows to renew
    // the peer cache instance when a better interface is available
    // - returns '' on success, or an error message
    function GuessInterface(out Mac: TMacAddress): RawUtf8; virtual;
    /// encode a remote URI for pcoHttpDirect download at localhost
    // - returns aDirectUri e.g. as 'http://1.2.3.4:8099/https/microsoft.com/...'
    // (if port is 8099 and Mac.IP is 1.2.3.4) and its aDirectHeaderBearer
    // - aForceTls should map ServerTls.Enabled
    function HttpDirectUri(const aSharedSecret: RawByteString;
      const aRemoteUri, aRemoteHash: RawUtf8;
      out aDirectUri, aDirectHeaderBearer: RawUtf8;
      aForceTls: boolean = false; aPermanent: boolean = false;
      aOptions: PHttpRequestExtendedOptions = nil): boolean;
  published
    /// the local port used for UDP and TCP process
    // - value should match on all peers for proper discovery
    // - UDP for discovery, TCP for HTTP/HTTPS content delivery
    // - is 8099 by default, which is unassigned by IANA
    property Port: TNetPort
      read fPort write fPort;
    /// allow to customize the process
    property Options: THttpPeerCacheOptions
      read fOptions write fOptions;
    /// local TMacAddress.Name, Address or IP to be used for UDP and TCP
    // - Name and Address will be searched case-insensitive
    // - IP could be exact or eventually a bitmask pattern (e.g. 192.168.1.255)
    // - if not set, will fallback to the best local makEthernet/makWifi network
    // with broadcasting abilities
    // - matching TMacAddress.IP will be used with the Port property value to
    // bind the TCP/HTTP server and broadcast the UDP discovery packets, so that
    // only this network interface will be involved to find cache peers
    property InterfaceName: RawUtf8
      read fInterfaceName write fInterfaceName;
    /// how GetMacAddress() should find the network, if InterfaceName is not set
    property InterfaceFilter: TMacAddressFilter
      read fInterfaceFilter write fInterfaceFilter;
    /// can limit the peer bandwidth used, in data MegaBytes per second
    // - will be assigned to each TStreamRedirect.LimitPerSecond instance
    // - default is 10 MB/s of data, i.e. aroung 100-125 MBit/s on network
    // - you may set 0 to disable any bandwidth limitation
    // - you may set -1 to use the default TStreamRedirect.LimitPerSecond value
    property LimitMBPerSec: integer
      read fLimitMBPerSec write fLimitMBPerSec;
    /// can limit how many peer clients can be served content at the same time
    // - would prevent any overload, to avoid Denial of Service
    // - default is 32, which means 32 threads with the default THttpServer
    property LimitClientCount: integer
      read fLimitClientCount write fLimitClientCount;
    /// RejectInstablePeersMin will set a delay (in minutes) to ignore any peer
    // which sent invalid UDP frames or HTTP/HTTPS requests
    // - should be a positive small power of two <= 128
    // - default is 4, for a 4 minutes time-to-live of IP banishments
    // - you may set 0 to disable the whole IP ban safety mechanism at UDP level
    // - use pcoNoBanIP option to disable the IP ban mechanism at HTTP level
    property RejectInstablePeersMin: integer
      read fRejectInstablePeersMin write fRejectInstablePeersMin;
    /// how many milliseconds UDP broadcast should wait for a response
    // - default is 10 ms which seems enough on a local network
    // - on Windows, this value is indicative, likely to have 15ms resolution
    property BroadcastTimeoutMS: integer
      read fBroadcastTimeoutMS write fBroadcastTimeoutMS;
    /// how many responses UDP broadcast should take into account
    // - default is 24
    property BroadcastMaxResponses: integer
      read fBroadcastMaxResponses write fBroadcastMaxResponses;
    /// above how many bytes the peer network should be asked for a pcoHttpDirect
    // - default is 65536 bytes, i.e. 64KB
    property BroadCastDirectMinBytes: integer
      read fBroadCastDirectMinBytes write fBroadCastDirectMinBytes;
    /// how many of the best responses should pcoTryAllPeers also try
    // - default is 10
    property TryAllPeersCount: integer
      read fTryAllPeersCount write fTryAllPeersCount;
    /// the socket level timeout for HTTP requests
    // - default to low 500 ms because should be local
    property HttpTimeoutMS: integer
      read fHttpTimeoutMS write fHttpTimeoutMS;
    /// location of the temporary cached files, available for remote requests
    // - the files are cached using their THashDigest values as filename
    // - this folder will be purged according to CacheTempMaxMB/CacheTempMaxMin
    // - if this value equals '', or pcoNoServer is defined in Options,
    // temporary caching would be disabled
    property CacheTempPath: TFileName
      read fCacheTempPath write fCacheTempPath;
    /// above how many bytes the peer network should be asked for a temporary file
    // - there is no size limitation if the file is already in the temporary
    // cache, or if the waoNoMinimalSize option is specified by WGet()
    // - default is 2048 bytes, i.e. 2KB
    property CacheTempMinBytes: integer
      read fCacheTempMinBytes  write fCacheTempMinBytes;
    /// after how many MB in CacheTempPath the folder should be cleaned
    // - default is 1000, i.e. just below 1 GB
    // - THttpPeerCache.Create will also always ensure that this value won't
    // take more than 25% of the CacheTempPath folder available space
    property CacheTempMaxMB: integer
      read fCacheTempMaxMB write fCacheTempMaxMB;
    /// after how many minutes files in CacheTempPath could be cleaned
    // - i.e. the Time-To-Live (TTL) of temporary files
    // - default is 60 minutes, i.e. 1 hour
    property CacheTempMaxMin: integer
      read fCacheTempMaxMin write fCacheTempMaxMin;
    /// location of the permanent cached files, available for remote requests
    // - in respect to CacheTempPath, this folder won't be purged
    // - the files are cached using their THashDigest values as filename
    // - if this value equals '', or pcoNoServer is defined in Options,
    // permanent caching would be disabled
    property CachePermPath: TFileName
      read fCachePermPath write fCachePermPath;
    /// above how many bytes the peer network should be asked for a permanent file
    // - there is no size limitation if the file is already in the permanent
    // cache, or if the waoNoMinimalSize option is specified by WGet()
    // - default is 2048 bytes, i.e. 2KB, which is just two network MTU trips
    property CachePermMinBytes: integer
      read fCachePermMinBytes  write fCachePermMinBytes;
    /// allow to customize the UUID used to identify this node
    // - instead of the default GetComputerUuid() from SMBios
    // - should be a valid UUID/GUID text as recognized by RawUtf8ToGuid()
    property Uuid: RawUtf8
      read fUuid write fUuid;
  end;

  /// information about THttpPeerCrypt.MessageDecode() success
  THttpPeerCryptMessageDecode = (
    mdOk,
    mdBParam,
    mdBLen,
    mdB64,
    mdBearer,
    mdLen,
    mdCrc,
    mdAes,
    mdSeq,
    mdKind,
    mdHw,
    mdAlgo);

  /// abstract parent to THttpPeerCache for its cryptographic core
  THttpPeerCrypt = class(TInterfacedPersistent)
  protected
    fAesSafe: TLightLock; // topmost to ensure proper aarch64 alignment
    fClientSafe: TLightLock; // if try to download with background direct mode
    fSettings: THttpPeerCacheSettings;
    fSharedMagic, fFrameSeqLow: cardinal;
    fFrameSeq: integer;
    fIP4, fMaskIP4, fBroadcastIP4, fClientIP4, fLastNetworkTix: cardinal;
    fAesEnc, fAesDec: TAesGcmAbstract;
    fLog: TSynLogClass;
    fPort, fIpPort, fDirectSecret: RawUtf8;
    fClient: THttpClientSocket;
    fInstable: THttpAcceptBan; // from Settings.RejectInstablePeersMin
    fMac: TMacAddress;
    fSettingsOwned, fVerboseLog: boolean;
    fUuid: TGuid;
    fServerTls, fClientTls: TNetTlsContext;
    procedure AfterSettings; virtual;
    function CurrentConnections: integer; virtual;
    procedure MessageInit(aKind: THttpPeerCacheMessageKind; aSeq: cardinal;
      out aMsg: THttpPeerCacheMessage); virtual;
    procedure MessageEncode(const aMsg: THttpPeerCacheMessage;
      out aEncoded: THttpPeerCacheMessageEncoded);
    procedure MessageEncodeBearer(const aMsg: THttpPeerCacheMessage;
      out aBearer: RawUtf8);
    function MessageDecode(aFrame: PHttpPeerCacheMessageEncoded; aFrameLen: PtrInt;
      out aMsg: THttpPeerCacheMessage): THttpPeerCryptMessageDecode;
    function BearerDecode(const aBearerToken: RawUtf8;
      aExpected: THttpPeerCacheMessageKind; out aMsg: THttpPeerCacheMessage;
      aParams: PRawUtf8 = nil): THttpPeerCryptMessageDecode; virtual;
    procedure LocalPeerClientSetup(const aIp: RawUtf8;
      aClient: THttpClientSocket; aRecvTimeout: integer);
    function LocalPeerRequest(const aRequest: THttpPeerCacheMessage;
      var aResp : THttpPeerCacheMessage; const aUrl: RawUtf8;
      aOutStream: TStreamRedirect; aRetry: boolean): integer;
    function GetUuidText: RawUtf8;
  public
    /// initialize the cryptography of this peer-to-peer node instance
    // - warning: inherited class should also call AfterSettings once
    // fSettings is defined
    constructor Create(const aSharedSecret: RawByteString;
      aServerTls, aClientTls: PNetTlsContext); reintroduce;
    /// finalize this class instance
    destructor Destroy; override;
    /// check if the network interface defined in Settings did actually change
    // - you may want to recreate a peer-cache to track the new network layout
    function NetworkInterfaceChanged: boolean;
    /// encode a remote URI for pcoHttpDirect download at localhost
    // - aSharedSecret should match the Create() value
    // - returns aDirectUri as '/https/microsoft.com/...' and aDirectHeaderBearer
    class function HttpDirectUri(const aSharedSecret: RawByteString;
      const aRemoteUri, aRemoteHash: RawUtf8;
      out aDirectUri, aDirectHeaderBearer: RawUtf8; aPermanent: boolean = false;
      aOptions: PHttpRequestExtendedOptions = nil): boolean;
    /// decode a remote URI for pcoHttpDirect download at localhost
    // - as previously encoded by HttpDirectUri() class function
    class function HttpDirectUriReconstruct(P: PUtf8Char;
      out Decoded: TUri): boolean;
    /// optional TLS options for the peer HTTPS server
    // - e.g. to set a custom certificate for this peer
    // - when ServerTls.Enabled is set, ClientTls.Enabled and other params should match
    property ServerTls: TNetTlsContext
      read fServerTls write fServerTls;
    /// optional TLS options for the peer HTTPS client
    // - e.g. set ClientTls.OnPeerValidate to verify a peer ServerTls certificate
    // - when ClientTls.Enabled is set, ServerTls.Enabled and other params should match
    property ClientTls: TNetTlsContext
      read fClientTls write fClientTls;
    /// the network interface used for UDP and TCP process
    // - the main fields are published below as Network* properties
    property Mac: TMacAddress
      read fMac;
    /// the 'ip:port' value used for UDP and TCP process
    property IpPort: RawUtf8
      read fIpPort;
    /// the raw 32-bit IPv4 value used for UDP and TCP process
    property Ip4: cardinal
      read fIp4;
  published
    /// define how this instance handles its process
    property Settings: THttpPeerCacheSettings
      read fSettings;
    /// the UUID used to identify this PeerCache node
    property Uuid: RawUtf8
      read GetUuidText;
    /// which network interface is used for UDP and TCP process
    property NetworkInterface: RawUtf8
      read fMac.Name;
    /// the local IP address used for UDP and TCP process
    property NetworkIP: RawUtf8
      read fMac.IP;
    /// the port value used for UDP and TCP process
    property NetworkPort: RawUtf8
      read fPort;
    /// the IP used for UDP and TCP process broadcast
    property NetworkBroadcast: RawUtf8
      read fMac.Broadcast;
  end;

  /// exception class raised on THttpPeerCache issues
  EHttpPeerCache = class(ESynException);

  THttpPeerCache = class;

  /// background UDP server thread, associated to a THttpPeerCache instance
  THttpPeerCacheThread = class(TUdpServerThread)
  protected
    fOwner: THttpPeerCache;
    fSent, fResponses: integer;
    fRespSafe: TLightLock;
    fResp: THttpPeerCacheMessageDynArray;
    fRespCount: integer;
    fBroadcastCurrentSeq: cardinal;
    fBroadcastEvent: TSynEvent;   // e.g. for pcoUseFirstResponse
    fBroadcastAddr: TNetAddr;     // from fBroadcastIP4 + fSettings.Port
    fBroadcastSafe: TOSLightLock; // non-reentrant, to serialize Broadcast()
    fBroadcastIpPort: RawUtf8;
    procedure OnFrameReceived(len: integer; var remote: TNetAddr); override;
    procedure OnIdle(tix64: Int64); override;
    procedure OnShutdown; override; // = Destroy
    function Broadcast(const aReq: THttpPeerCacheMessage;
      out aAlone: boolean): THttpPeerCacheMessageDynArray;
    function AddResponseAndDone(const aMessage: THttpPeerCacheMessage): boolean;
    function GetResponses(aSeq: cardinal): THttpPeerCacheMessageDynArray;
  public
    /// initialize the background UDP server thread
    constructor Create(Owner: THttpPeerCache); reintroduce;
    /// finalize this instance
    destructor Destroy; override;
  published
    property Sent: integer
      read fSent;
  end;

  THttpPeerCacheLocalFileName = set of (
    lfnSetDate,
    lfnEnsureDirectoryExists);

  /// callback to optimize HTTP/HTTPS remote access a given URI with pcoHttpDirect
  // - as defined in THttpPeerCache.OnDirectOptions property
  // - should override proper options (and/or header/URI) and return HTTP_SUCCESS
  TOnHttpPeerCacheDirectOptions = function(var aUri: TUri; var aHeader: RawUtf8;
    var aOptions: THttpRequestExtendedOptions): integer of object;

  EHttpServerCache = class(ESynException);

  /// implement a local peer-to-peer download cache via UDP and TCP
  // - UDP broadcasting is used for local peers discovery
  // - TCP is bound to a local THttpServer/THttpAsyncServer content delivery
  // - will maintain its own local folders of cached files, stored by hash
  THttpPeerCache = class(THttpPeerCrypt, IWGetAlternate)
  protected
    fHttpServer: THttpServerGeneric;
    fUdpServer: THttpPeerCacheThread;
    fPermFilesPath, fTempFilesPath: TFileName;
    fTempFilesMaxSize: Int64; // from Settings.CacheTempMaxMB
    fTempCurrentSize: Int64;
    fBroadcastStart: Int64;   // QueryPerformanceMicroSeconds()
    fTempFilesTix, fInstableTix, fBroadcastTix: cardinal;
    fFilesSafe: TOSLock; // concurrent cached files access
    fPartials: THttpPartials;
    fOnDirectOptions: TOnHttpPeerCacheDirectOptions;
    // most of these internal methods are virtual for proper customization
    procedure StartHttpServer(aHttpServerClass: THttpServerSocketGenericClass;
      aHttpServerThreadCount: integer; const aIP: RawUtf8); virtual;
    function CurrentConnections: integer; override;
    function ComputeFileName(const aHash: THashDigest): TFileName; virtual;
    function PermFileName(const aFileName: TFileName;
      aFlags: THttpPeerCacheLocalFileName): TFileName; virtual;
    function LocalFileName(const aMessage: THttpPeerCacheMessage;
      aFlags: THttpPeerCacheLocalFileName;
      aFileName: PFileName; aSize: PInt64): integer;
    function CachedFileName(const aParams: THttpClientSocketWGet;
      aFlags: THttpPeerCacheLocalFileName;
      out aLocal: TFileName; out isTemp: boolean): boolean;
    function DirectFileName(Ctxt: THttpServerRequestAbstract;
      const aMessage: THttpPeerCacheMessage; aHttp: PHttpRequestContext;
      out aFileName: TFileName; out aSize: Int64; const aParams: RawUtf8): integer;
    procedure DirectFileNameBackgroundGet(Sender: TObject);
    function DirectFileNameHead(Ctxt: THttpServerRequestAbstract;
      const aHash: THashDigest; const aParams: RawUtf8): cardinal;
    function TooSmallFile(const aParams: THttpClientSocketWGet;
      aSize: Int64; const aCaller: ShortString): boolean;
    function PartialFileName(const aMessage: THttpPeerCacheMessage;
      aHttp: PHttpRequestContext; aFileName: PFileName; aSize: PInt64): integer;
    function TempFolderEstimateNewSize(aAddingSize: Int64): Int64;
    function Check(Status: THttpPeerCryptMessageDecode;
      const Ctxt: ShortString; const Msg: THttpPeerCacheMessage): boolean;
  public
    /// initialize this peer-to-peer cache instance
    // - any supplied aSettings should be owned by the caller (e.g from a main
    // settings class instance)
    // - aSharedSecret is used to cipher and authenticate each UDP frame between
    // all peer nodes, and also generate HTTP authentication bearers
    // - if aSettings = nil, default values will be used by this instance
    // - you can supply THttpAsyncServer class to replace default THttpServer
    // - may raise some exceptions if the HTTP server cannot be started
    constructor Create(aSettings: THttpPeerCacheSettings;
      const aSharedSecret: RawByteString;
      aHttpServerClass: THttpServerSocketGenericClass = nil;
      aHttpServerThreadCount: integer = 2; aLogClass: TSynLogClass = nil;
      aServerTls: PNetTlsContext = nil; aClientTls: PNetTlsContext = nil); reintroduce;
    /// finalize this peer-to-peer cache instance
    destructor Destroy; override;
    /// IWGetAlternate main processing method, as used by THttpClientSocketWGet
    // - will transfer Sender.Server/Port/RangeStart/RangeEnd into OutStream
    // - OutStream.LimitPerSecond will be overriden during the call
    // - could return 0 to fallback to a regular GET (e.g. not cached)
    function OnDownload(Sender: THttpClientSocket;
      var Params: THttpClientSocketWGet; const Url: RawUtf8;
      ExpectedFullSize: Int64; OutStream: TStreamRedirect): integer; virtual;
    /// IWGetAlternate main processing method, as used by THttpClientSocketWGet
    // - if a file has been downloaded from the main repository, this method
    // should be called to copy the content into this instance files cache
    // - also makes Rename(Partial, ToRename) with proper progressive support
    procedure OnDownloaded(var Params: THttpClientSocketWGet;
      const Partial, ToRename: TFileName; PartialID: integer); virtual;
    /// IWGetAlternate main processing method, as used by THttpClientSocketWGet
    // - OnDownload() may have returned corrupted data: local cache file is
    // likely to be deleted, for safety
    procedure OnDownloadFailed(const Params: THttpClientSocketWGet);
    /// IWGetAlternate main processing method, as used by THttpClientSocketWGet
    // - make this .part file available as pcfResponsePartial
    // - returns PartialID > 0 sequence
    function OnDownloading(const Params: THttpClientSocketWGet;
      const Partial: TFileName; ExpectedFullSize: Int64): THttpPartialID;
    /// IWGetAlternate main processing method, as used by THttpClientSocketWGet
    /// notify the alternate download implementation that OnDownloading() failed
    // - e.g. THttpPeerCache is likely to abort publishing this partial file
    procedure OnDownloadingFailed(ID: THttpPartialID);
    /// broadcast a pcfPing on the network interface and return the responses
    function Ping: THttpPeerCacheMessageDynArray;
    /// method called by the HttpServer before any request is processed
    // - will reject anything but a GET with a proper bearer, from the right IP
    function OnBeforeBody(var aUrl, aMethod, aInHeaders,
      aInContentType, aRemoteIP, aBearerToken: RawUtf8; aContentLength: Int64;
      aFlags: THttpServerRequestFlags): cardinal; virtual;
    /// method called by the HttpServer to process a request
    // - statically serve a local file from decoded bearer hash
    function OnRequest(Ctxt: THttpServerRequestAbstract): cardinal; virtual;
    /// is called on a regular basis for background regular process
    // - is called from THttpPeerCacheThread.OnIdle
    // - e.g. to implement optional CacheTempMaxMin disk space release,
    // actually reading and purging the CacheTempPath folder every minute
    // - could call Instable.DoRotate every minute to refresh IP banishments
    procedure OnIdle(tix64: Int64);
    /// returns the current state of this PeerCache instance
    function State: TWGetAlternateState;
    /// event to customize the access of a given URI in pcoHttpDirect mode
    property OnDirectOptions: TOnHttpPeerCacheDirectOptions
      read fOnDirectOptions write fOnDirectOptions;
    /// access to the current directory storing temporary cache files
    property TempFilesPath: TFileName
      read fTempFilesPath;
    /// access to the current directory storing permanent cache files
    property PermFilesPath: TFileName
      read fPermFilesPath;
  published
    /// the associated HTTP/HTTPS server delivering cached context
    property HttpServer: THttpServerGeneric
      read fHttpServer;
    /// the current state of banned IP from incorrect UDP/HTTP requests
    // - follow RejectInstablePeersMin settings
    property Instable: THttpAcceptBan
      read fInstable;
  end;

  /// one THttpPeerCache.OnDownload instance
  THttpPeerCacheProcess = class(TSynPersistent)
  protected
    fOwner: THttpPeerCache;
  public
  published
    property Owner: THttpPeerCache
      read fOwner;
  end;

const
  PCF_RESPONSE = [
    pcfPong,
    pcfResponseNone,
    pcfResponseOverloaded,
    pcfResponsePartial,
    pcfResponseFull];

  PEER_CACHE_PATTERN = '*.cache';

  PEER_CACHE_MESSAGELEN = SizeOf(THttpPeerCacheMessageEncoded);
  PEER_CACHE_AESLEN = PEER_CACHE_MESSAGELEN - SizeOf(cardinal);
  /// base-64 HttpDirectUri() bearer size in chars, from PEER_CACHE_MESSAGELEN
  PEER_CACHE_BEARERLEN = 326;

function ToText(pcf: THttpPeerCacheMessageKind): PShortString; overload;
function ToText(md: THttpPeerCryptMessageDecode): PShortString; overload;
function ToText(const msg: THttpPeerCacheMessage): ShortString; overload;
  {$ifdef HASINLINE} inline; {$endif}

procedure MsgToShort(const msg: THttpPeerCacheMessage; var result: ShortString);

/// hash an URL and the "Etag:" or "Last-Modified:" headers
// - could be used to identify a HTTP resource as a binary hash on a given server
// - aHeaders could be supplied as nil so that only the URI resource is hashed
// - returns 0 if aUrl/aHeaders have not enough information
// - returns the number of hash bytes written to aDigest.Bin
function HttpRequestHash(aAlgo: THashAlgo; const aUri: TUri;
  aHeaders: PUtf8Char; out aDigest: THashDigest): integer;

/// hash an URL and the "Etag:" or "Last-Modified:" headers into 32 ascii chars
// - you could set any custom aDiglen in 5/10/15/20/25/30 set
// - aHeaders could be supplied as nil so that only the URI resource is hashed
// - using SHA-256 and lowercase Base-32 encoding, so perfect for a file name
function HttpRequestHashBase32(const aUri: TUri; aHeaders: PUtf8Char;
  aDiglen: integer = 20): RawUtf8;

/// get the content full length, from "Content-Length:" or "Content-Range:"
function HttpRequestLength(aHeaders: PUtf8Char; out Len: PtrInt): PUtf8Char;


{$ifdef USEWININET}

{ **************** THttpApiServer HTTP/1.1 Server Over Windows http.sys Module }

type
  THttpApiServer = class;

  /// processing sub-thread of THttpApiServer
  THttpApiServerThread = class(TLoggedThread)
  protected
    fOwner: THttpApiServer;
    fStarted: boolean;
    // just redirect to fOwner.DoExecute
    procedure DoExecute; override;
  public
    /// initialize this processing thread
    constructor Create(aOwner: THttpApiServer); reintroduce;
  end;
  THttpApiServerThreads = array of THttpApiServerThread;

  /// HTTP server using fast http.sys kernel-mode server
  // - The HTTP Server API enables applications to communicate over HTTP without
  // using Microsoft Internet Information Server (IIS). Applications can register
  // to receive HTTP requests for particular URLs, receive HTTP requests, and send
  // HTTP responses. The HTTP Server API includes TLS support so that applications
  // can exchange data over secure HTTP connections without IIS. It is also
  // designed to work with I/O completion ports.
  // - The HTTP Server API is supported on Windows Server 2003 operating systems
  // and on Windows XP with Service Pack 2 (SP2). Be aware that Microsoft IIS 5
  // running on Windows XP with SP2 is not able to share port 80 with other HTTP
  // applications running simultaneously.
  THttpApiServer = class(THttpServerGeneric)
  protected
    fReqQueue: THandle;                         // the internal request queue
    fThreads: THttpApiServerThreads;            // additional processing threads
    fRegisteredUnicodeUrl: TSynUnicodeDynArray; // all registered URLs
    fServerSessionID: HTTP_SERVER_SESSION_ID;
    fUrlGroupID: HTTP_URL_GROUP_ID;
    fLoggingServiceName: RawUtf8;
    fRegisteredUrl: RawUtf8;
    fReceiveBufferSize: cardinal;
    fAuthenticationSchemes: THttpApiRequestAuthentications; // 8-bit
    fLogging: boolean;                                      // 8-bit
    function GetRegisteredUrl: RawUtf8;
    function GetHttpQueueLength: cardinal; override;
    function GetConnectionsActive: cardinal; override;
    procedure SetHttpQueueLength(aValue: cardinal); override;
    function GetProperty(dest: PHTTP_QOS_SETTING_INFO; destlen: cardinal;
      qos: HTTP_QOS_SETTING_TYPE): boolean;
    procedure SetProperty(value: PHTTP_QOS_SETTING_INFO; valuelen: cardinal;
      qos: HTTP_QOS_SETTING_TYPE; alsoForSession: boolean = false);
    function GetMaxBandwidth: cardinal;
    procedure SetMaxBandwidth(aValue: cardinal);
    function GetMaxConnections: cardinal;
    procedure SetMaxConnections(aValue: cardinal);
    function GetApiVersion: RawUtf8; override;
    function Check(Api: THttpApiFunction; Error: integer;
      Level: TSynLogLevel = sllWarning): integer;
    procedure Ensure(Api: THttpApiFunction; Error: integer);
    procedure DoAfterResponse(Ctxt: THttpServerRequest; const Referer: RawUtf8;
      StatusCode: cardinal; Elapsed, Received, Sent: QWord); virtual;
    /// server main loop - don't change directly
    // - will call the Request public virtual method with the appropriate
    // parameters to retrieve the content
    procedure DoExecute; override;
    /// retrieve flags for SendHttpResponse
   // - if response content type is not STATICFILE_CONTENT_TYPE
    function GetSendResponseFlags(Ctxt: THttpServerRequest): integer; virtual;
    procedure DestroyMainThread; virtual;
  public
    /// initialize the HTTP Service
    // - will raise an exception if http.sys is not available e.g. before
    // Windows XP SP2) or if the request queue creation failed
    // - if you override this contructor, put the AddUrl() methods within,
    // and you can set CreateSuspended to FALSE
    // - if you will call AddUrl() methods later, set CreateSuspended to TRUE,
    // then call explicitly the Resume method, after all AddUrl() calls, in
    // order to start the server
    constructor Create(QueueName: SynUnicode = '';
      const OnStart: TOnNotifyThread = nil; const OnStop: TOnNotifyThread = nil;
      const ProcessName: RawUtf8 = ''; ProcessOptions: THttpServerOptions = [];
      aLog: TSynLogClass = nil; ServerThreadPoolCount: integer = 32); reintroduce;
    /// ensure the HTTP server threads are actually started
    // - note that http.sys requires global system-wide registration for its
    // TLS/https settings
    function WaitStarted(Seconds: cardinal = 30): boolean;
    /// release all associated memory and handles
    destructor Destroy; override;
    /// register the URLs to Listen On
    // - e.g. AddUrl('root','888')
    // - aDomainName could be either a fully qualified case-insensitive domain
    // name, an IPv4 or IPv6 literal string, or a wildcard ('+' will bound
    // to all domain names for the specified port, '*' will accept the request
    // when no other listening hostnames match the request for that port)
    // - return 0 (NO_ERROR) on success, an error code if failed: under Vista
    // and Seven, you could have ERROR_ACCESS_DENIED if the process is not
    // running with enough rights (by default, UAC requires administrator rights
    // for adding an URL to http.sys registration list) - solution is to call
    // the THttpApiServer.AddUrlAuthorize class method during program setup
    // - if this method is not used within an overridden constructor, default
    // Create must have be called with CreateSuspended = TRUE and then call the
    // Resume method after all Url have been added
    // - if aRegisterUri is TRUE, the URI will be registered (need adminitrator
    // rights) - default is FALSE, as defined by Windows security policy
    function AddUrl(const aRoot, aPort: RawUtf8; Https: boolean = false;
      const aDomainName: RawUtf8 = '*'; aRegisterUri: boolean = false;
      aContext: Int64 = 0): integer;
    /// un-register the URLs to Listen On
    // - this method expect the same parameters as specified to AddUrl()
    // - return 0 (NO_ERROR) on success, an error code if failed (e.g.
    // -1 if the corresponding parameters do not match any previous AddUrl)
    function RemoveUrl(const aRoot, aPort: RawUtf8; Https: boolean = false;
      const aDomainName: RawUtf8 = '*'): integer;
    /// will authorize a specified URL prefix
    // - will allow to call AddUrl() later for any user on the computer
    // - if aRoot is left '', it will authorize any root for this port
    // - must be called with Administrator rights: this class function is to be
    // used in a Setup program for instance, especially under Vista or Seven,
    // to reserve the Url for the server
    // - add a new record to the http.sys URL reservation store
    // - return '' on success, an error message otherwise
    // - will first delete any matching rule for this URL prefix
    // - if OnlyDelete is true, will delete but won't add the new authorization;
    // in this case, any error message at deletion will be returned
    class function AddUrlAuthorize(const aRoot, aPort: RawUtf8; Https: boolean = false;
      const aDomainName: RawUtf8 = '*'; OnlyDelete: boolean = false): RawUtf8;
    /// access to the additional processing threads
    property Threads: THttpApiServerThreads
      read fThreads;
  public
    { HTTP API 2.0 methods and properties }
    /// can be used to check if the HTTP API 2.0 is available
    class function HasApi2: boolean;
      {$ifdef HASINLINE} inline; static; {$endif}
    /// enable HTTP API 2.0 advanced timeout settings
    // - all those settings are set for the current URL group
    // - will raise an EHttpApiServer exception if the old HTTP API 1.x is used
    // so you should better test the availability of the method first:
    // ! if aServer.HasApi2 then
    // !   SetTimeOutLimits(....);
    // - aEntityBody is the time, in seconds, allowed for the request entity
    // body to arrive - default value is 2 minutes
    // - aDrainEntityBody is the time, in seconds, allowed for the HTTP Server
    // API to drain the entity body on a Keep-Alive connection - default value
    // is 2 minutes
    // - aRequestQueue is the time, in seconds, allowed for the request to
    // remain in the request queue before the application picks it up - default
    // value is 2 minutes
    // - aIdleConnection is the time, in seconds, allowed for an idle connection;
    // is similar to THttpServer.ServerKeepAliveTimeOut - default value is
    // 2 minutes
    // - aHeaderWait is the time, in seconds, allowed for the HTTP Server API
    // to parse the request header - default value is 2 minutes
    // - aMinSendRate is the minimum send rate, in bytes-per-second, for the
    // response - default value is 150 bytes-per-second
    // - any value set to 0 will set the HTTP Server API default value
    procedure SetTimeOutLimits(aEntityBody, aDrainEntityBody,
      aRequestQueue, aIdleConnection, aHeaderWait, aMinSendRate: cardinal);
    /// enable HTTP API 2.0 logging
    // - will raise an EHttpApiServer exception if the old HTTP API 1.x is used
    // so you should better test the availability of the method first:
    // ! if aServer.HasApi2 then
    // !   LogStart(....);
    // - you can select the output folder and the expected logging layout
    // - aSoftwareName will set the optional W3C-only software name string
    // - aRolloverSize will be used only when aRolloverType is hlrSize
    procedure LogStart(const aLogFolder: TFileName;
      aType: THttpApiLoggingType = hltW3C;
      const aSoftwareName: TFileName = '';
      aRolloverType: THttpApiLoggingRollOver = hlrDaily;
      aRolloverSize: cardinal = 0;
      aLogFields: THttpApiLogFields = [hlfDate..hlfSubStatus];
      aFlags: THttpApiLoggingFlags = [hlfUseUtf8Conversion]);
    /// disable HTTP API 2.0 logging
    procedure LogStop;
    /// enable HTTP API 2.0 server-side authentication
    // - once enabled, the client sends an unauthenticated request: it is up to
    // the server application to generate the initial 401 challenge with proper
    // WWW-Authenticate headers; any further authentication steps will be
    // perform in kernel mode, until the authentication handshake is finalized;
    // later on, the application can check the AuthenticationStatus property
    // of THttpServerRequest and its associated AuthenticatedUser value
    // see https://msdn.microsoft.com/en-us/library/windows/desktop/aa364452
    // - will raise an EHttpApiServer exception if the old HTTP API 1.x is used
    // so you should better test the availability of the method first:
    // ! if aServer.HasApi2 then
    // !   SetAuthenticationSchemes(....);
    // - this method will work on the current group, for all instances
    // - see HTTPAPI_AUTH_ENABLE_ALL constant to set all available schemes
    // - optional Realm parameters can be used when haBasic scheme is defined
    // - optional DomainName and Realm parameters can be used for haDigest
    procedure SetAuthenticationSchemes(schemes: THttpApiRequestAuthentications;
      const DomainName: SynUnicode = ''; const Realm: SynUnicode = '');
    /// read-only access to HTTP API 2.0 server-side enabled authentication schemes
    property AuthenticationSchemes: THttpApiRequestAuthentications
      read fAuthenticationSchemes;
    /// read-only access to check if the HTTP API 2.0 logging is enabled
    // - use LogStart/LogStop methods to change this property value
    property Logging: boolean
      read fLogging;
    /// the current HTTP API 2.0 logging Service name
    // - should be UTF-8 encoded, if LogStart(aFlags=[hlfUseUtf8Conversion])
    property LoggingServiceName: RawUtf8
      read fLoggingServiceName write fLoggingServiceName;
    /// read-only access to the low-level HTTP API 2.0 Session ID
    property ServerSessionID: HTTP_SERVER_SESSION_ID
      read fServerSessionID;
    /// read-only access to the low-level HTTP API 2.0 URI Group ID
    property UrlGroupID: HTTP_URL_GROUP_ID
      read fUrlGroupID;
    /// how many bytes are retrieved in a single call to ReceiveRequestEntityBody
    // - set by default to 1048576, i.e. 1 MB - practical limit is around 20 MB
    // - you may customize this value if you encounter HTTP error HTTP_NOTACCEPTABLE
    // (406) from client, corresponding to an ERROR_NO_SYSTEM_RESOURCES (1450)
    // exception on server side, when uploading huge data content
    property ReceiveBufferSize: cardinal
      read fReceiveBufferSize write fReceiveBufferSize;
  published
    /// return the list of registered URL on this server instance as a CSV text
    property RegisteredUrl: RawUtf8
      read GetRegisteredUrl;
    /// the maximum allowed bandwidth rate in bytes per second (via HTTP API 2.0)
    // - Setting this value to 0 allows an unlimited bandwidth
    // - by default Windows not limit bandwidth (actually limited to 4 Gbit/sec).
    // - will return 0 if the system does not support HTTP API 2.0 (i.e.
    // under Windows XP or Server 2003)
    property MaxBandwidth: cardinal
      read GetMaxBandwidth write SetMaxBandwidth;
    /// the maximum number of HTTP connections allowed (via HTTP API 2.0)
    // - Setting this value to 0 allows an unlimited number of connections
    // - by default Windows does not limit number of allowed connections
    // - will return 0 if the system does not support HTTP API 2.0 (i.e.
    // under Windows XP or Server 2003)
    property MaxConnections: cardinal
      read GetMaxConnections write SetMaxConnections;
  end;


{ ****************** THttpApiWebSocketServer Over Windows http.sys Module }

type
  TSynThreadPoolHttpApiWebSocketServer = class;
  TSynWebSocketGuard = class;
  THttpApiWebSocketServer = class;
  THttpApiWebSocketServerProtocol = class;

  /// current state of a THttpApiWebSocketConnection
  TWebSocketState = (
    wsConnecting,
    wsOpen,
    wsClosing,
    wsClosedByClient,
    wsClosedByServer,
    wsClosedByGuard,
    wsClosedByShutdown);

  /// structure representing a single WebSocket connection
  {$ifdef USERECORDWITHMETHODS}
  THttpApiWebSocketConnection = record
  {$else}
  THttpApiWebSocketConnection = object
  {$endif USERECORDWITHMETHODS}
  private
    fOverlapped: TOverlapped;
    fState: TWebSocketState;
    fProtocol: THttpApiWebSocketServerProtocol;
    fOpaqueHTTPRequestId: HTTP_REQUEST_ID;
    fWSHandle: WEB_SOCKET_HANDLE;
    fLastActionContext: pointer;
    fLastReceiveTickCount: Int64;
    fPrivateData: pointer;
    fBuffer: RawByteString;
    fCloseStatus: WEB_SOCKET_CLOSE_STATUS;
    fIndex: integer;
    function ProcessActions(ActionQueue: cardinal): boolean;
    function ReadData(const WebsocketBufferData): integer;
    procedure WriteData(const WebsocketBufferData);
    procedure BeforeRead;
    procedure DoOnMessage(aBufferType: WEB_SOCKET_BUFFER_TYPE;
      aBuffer: pointer; aBufferSize: ULONG);
    procedure DoOnConnect;
    procedure DoOnDisconnect;
    procedure InternalSend(aBufferType: WEB_SOCKET_BUFFER_TYPE; WebsocketBufferData: pointer);
    procedure Ping;
    procedure Disconnect;
    procedure CheckIsActive(Tix64: Int64);
    // call onAccept Method of protocol, and if protocol not accept connection or
    // can not be accepted from other reasons return false else return true
    function TryAcceptConnection(aProtocol: THttpApiWebSocketServerProtocol;
      Ctxt: THttpServerRequestAbstract; aNeedHeader: boolean): boolean;
  public
    /// Send data to client
    procedure Send(aBufferType: WEB_SOCKET_BUFFER_TYPE;
      aBuffer: pointer; aBufferSize: ULONG);
    /// Close connection
    procedure Close(aStatus: WEB_SOCKET_CLOSE_STATUS;
      aBuffer: pointer; aBufferSize: ULONG);
    /// Index of connection in protocol's connection list
    property Index: integer
      read fIndex;
    /// Protocol of connection
    property Protocol: THttpApiWebSocketServerProtocol
       read fProtocol;
    /// Custom user data
    property PrivateData: pointer
      read fPrivateData write fPrivateData;
    /// Access to the current state of this connection
    property State: TWebSocketState
      read fState;
  end;

  PHttpApiWebSocketConnection = ^THttpApiWebSocketConnection;

  THttpApiWebSocketConnectionVector =
    array[0..MaxInt div SizeOf(PHttpApiWebSocketConnection) - 1] of
    PHttpApiWebSocketConnection;

  PHttpApiWebSocketConnectionVector = ^THttpApiWebSocketConnectionVector;

  /// Event handler on THttpApiWebSocketServerProtocol Accepted connection
  TOnHttpApiWebSocketServerAcceptEvent = function(Ctxt: THttpServerRequest;
    var Conn: THttpApiWebSocketConnection): boolean of object;
  /// Event handler on THttpApiWebSocketServerProtocol Message received
  TOnHttpApiWebSocketServerMessageEvent = procedure(var Conn: THttpApiWebSocketConnection;
    aBufferType: WEB_SOCKET_BUFFER_TYPE; aBuffer: pointer; aBufferSize: ULONG) of object;
  /// Event handler on THttpApiWebSocketServerProtocol connection
  TOnHttpApiWebSocketServerConnectEvent = procedure(
    var Conn: THttpApiWebSocketConnection) of object;
  /// Event handler on THttpApiWebSocketServerProtocol disconnection
  TOnHttpApiWebSocketServerDisconnectEvent = procedure(var Conn: THttpApiWebSocketConnection;
    aStatus: WEB_SOCKET_CLOSE_STATUS; aBuffer: pointer; aBufferSize: ULONG) of object;

  /// Protocol Handler of websocket endpoints events
  // - maintains a list of all WebSockets clients for a given protocol
  THttpApiWebSocketServerProtocol = class
  private
    fName: RawUtf8;
    fSafe: TOSLock;
    fServer: THttpApiWebSocketServer;
    fConnections: PHttpApiWebSocketConnectionVector;
    fConnectionsCapacity: integer;
    // number of used connections. Some of them can be nil (if not used anymore)
    fConnectionsCount: integer;
    fFirstEmptyConnectionIndex: integer;
    fIndex: integer;
    fOnAccept: TOnHttpApiWebSocketServerAcceptEvent;
    fOnMessage: TOnHttpApiWebSocketServerMessageEvent;
    fOnFragment: TOnHttpApiWebSocketServerMessageEvent;
    fOnConnect: TOnHttpApiWebSocketServerConnectEvent;
    fOnDisconnect: TOnHttpApiWebSocketServerDisconnectEvent;
    fPendingForClose: TSynList;
    fManualFragmentManagement: boolean;
    function AddConnection(aConn: PHttpApiWebSocketConnection): integer;
    procedure RemoveConnection(index: integer);
    procedure DoShutdown;
  public
    /// initialize the WebSockets process
    // - if aManualFragmentManagement is true, onMessage will appear only for whole
    // received messages, otherwise OnFragment handler must be passed (for video
    // broadcast, for example)
    constructor Create(const aName: RawUtf8; aManualFragmentManagement: boolean;
      aServer: THttpApiWebSocketServer;
      const aOnAccept: TOnHttpApiWebSocketServerAcceptEvent;
      const aOnMessage: TOnHttpApiWebSocketServerMessageEvent;
      const aOnConnect: TOnHttpApiWebSocketServerConnectEvent;
      const aOnDisconnect: TOnHttpApiWebSocketServerDisconnectEvent;
      const aOnFragment: TOnHttpApiWebSocketServerMessageEvent = nil);
    /// finalize the process
    destructor Destroy; override;
    /// text identifier
    property Name: RawUtf8
      read fName;
    /// identify the endpoint instance
    property Index: integer
      read fIndex;
    /// OnFragment event will be called for each fragment
    property ManualFragmentManagement: boolean
      read fManualFragmentManagement;
    /// event triggerred when a WebSockets client is initiated
    property OnAccept: TOnHttpApiWebSocketServerAcceptEvent
      read fOnAccept;
    /// event triggerred when a WebSockets message is received
    property OnMessage: TOnHttpApiWebSocketServerMessageEvent
      read fOnMessage;
    /// event triggerred when a WebSockets client is connected
    property OnConnect: TOnHttpApiWebSocketServerConnectEvent
      read fOnConnect;
    /// event triggerred when a WebSockets client is gracefully disconnected
    property OnDisconnect: TOnHttpApiWebSocketServerDisconnectEvent
      read fOnDisconnect;
    /// event triggerred when a non complete frame is received
    // - required if ManualFragmentManagement is true
    property OnFragment: TOnHttpApiWebSocketServerMessageEvent
      read fOnFragment;

    /// Send message to the WebSocket connection identified by its index
    function Send(index: integer; aBufferType: ULONG;
      aBuffer: pointer; aBufferSize: ULONG): boolean;
    /// Send message to all connections of this protocol
    function Broadcast(aBufferType: ULONG;
      aBuffer: pointer; aBufferSize: ULONG): boolean;
    /// Close WebSocket connection identified by its index
    function Close(index: integer; aStatus: WEB_SOCKET_CLOSE_STATUS;
      aBuffer: pointer; aBufferSize: ULONG): boolean;
  end;

  THttpApiWebSocketServerProtocolDynArray = array of THttpApiWebSocketServerProtocol;

  /// HTTP & WebSocket server using fast http.sys kernel-mode server
  // - can be used like simple THttpApiServer
  // - when AddUrlWebSocket() is called, WebSocket support is added
  // and the frames will be received in asynchronous mode
  THttpApiWebSocketServer = class(THttpApiServer)
  protected
    fOwnedProtocolsSafe: TLightLock;
    fThreadPoolServer: TSynThreadPoolHttpApiWebSocketServer;
    fGuard: TSynWebSocketGuard;
    fLastConnection: PHttpApiWebSocketConnection;
    fPingTimeout: integer;
    fOnWSThreadStart: TOnNotifyThread;
    fOnWSThreadTerminate: TOnNotifyThread;
    fSendOverlaped: TOverlapped;
    fServiceOverlaped: TOverlapped;
    fOnServiceMessage: TThreadMethod;
    fOwnedProtocols: THttpApiWebSocketServerProtocolDynArray;
    function UpgradeToWebSocket(Ctxt: THttpServerRequestAbstract): cardinal;
    procedure DoAfterResponse(Ctxt: THttpServerRequest; const Referer: RawUtf8;
      StatusCode: cardinal; Elapsed, Received, Sent: QWord); override;
    function GetSendResponseFlags(Ctxt: THttpServerRequest): integer; override;
    procedure DestroyMainThread; override;
  public
    /// initialize the HTTPAPI based Server with WebSocket support
    // - will raise an exception if http.sys or websocket.dll is not available
    // (e.g. before Windows 8) or if the request queue creation failed
    // - for aPingTimeout explanation see PingTimeout property documentation
    constructor Create(aSocketThreadsCount: integer = 1;
      aPingTimeout: integer = 0; const QueueName: SynUnicode = '';
      const aOnWSThreadStart: TOnNotifyThread = nil;
      const aOnWSThreadTerminate: TOnNotifyThread = nil;
      ProcessOptions: THttpServerOptions = []); reintroduce;
    /// prepare the process for a given THttpApiWebSocketServerProtocol
    procedure RegisterProtocol(const aName: RawUtf8; aManualFragmentManagement: boolean;
      const aOnAccept: TOnHttpApiWebSocketServerAcceptEvent;
      const aOnMessage: TOnHttpApiWebSocketServerMessageEvent;
      const aOnConnect: TOnHttpApiWebSocketServerConnectEvent;
      const aOnDisconnect: TOnHttpApiWebSocketServerDisconnectEvent;
      const aOnFragment: TOnHttpApiWebSocketServerMessageEvent = nil);
    /// register the URLs to Listen on using WebSocket
    // - aProtocols is an array of a recond with callbacks, server call during
    // WebSocket activity
    function AddUrlWebSocket(const aRoot, aPort: RawUtf8; Https: boolean = false;
      const aDomainName: RawUtf8 = '*'; aRegisterUri: boolean = false): integer;
    /// handle the HTTP request
    function Request(Ctxt: THttpServerRequestAbstract): cardinal; override;
    /// Ping timeout in seconds. 0 mean no ping.
    // - if connection not receive messages longer than this timeout
    // TSynWebSocketGuard will send ping frame
    // - if connection not receive any messages longer than double of
    // this timeout it will be closed
    property PingTimeout: integer
      read fPingTimeout;
    /// access to the associated endpoints as a thread-safe array copy
    function GetRegisteredProtocols: THttpApiWebSocketServerProtocolDynArray;
    /// event called when the processing thread starts
    property OnWSThreadStart: TOnNotifyThread
      read fOnWSThreadStart write fOnWSThreadStart;
    /// event called when the processing thread termintes
    property OnWSThreadTerminate: TOnNotifyThread
      read fOnWSThreadTerminate write fOnWSThreadTerminate;
    /// send a "service" message to a WebSocketServer to wake up a WebSocket thread
    // - can be called from any thread
    // - when a webSocket thread receives such a message it will call onServiceMessage
    // in the thread context
    procedure SendServiceMessage;
    /// event called when a service message is raised
    property OnServiceMessage: TThreadMethod
      read fOnServiceMessage write fOnServiceMessage;
  end;

  /// a Thread Pool, used for fast handling of WebSocket requests
  TSynThreadPoolHttpApiWebSocketServer = class(TSynThreadPool)
  protected
    fServer: THttpApiWebSocketServer;
    procedure OnThreadStart(Sender: TThread);
    procedure OnThreadTerminate(Sender: TThread);
    function NeedStopOnIOError: boolean; override;
    // aContext is a PHttpApiWebSocketConnection, or fServer.fServiceOverlaped
    // (SendServiceMessage) or fServer.fSendOverlaped (WriteData)
    procedure Task(aCaller: TSynThreadPoolWorkThread;
      aContext: pointer); override;
  public
    /// initialize the thread pool
    constructor Create(Server: THttpApiWebSocketServer;
      NumberOfThreads: integer = 1); reintroduce;
  end;

  /// Thread for closing deprecated WebSocket connections
  // - i.e. which have not responsed after PingTimeout interval
  TSynWebSocketGuard = class(TThread)
  protected
    fServer: THttpApiWebSocketServer;
    procedure Execute; override;
  public
    /// initialize the background thread
    constructor Create(Server: THttpApiWebSocketServer); reintroduce;
  end;

{$endif USEWININET}


implementation


{ ******************** Abstract UDP Server }

{ TUdpServerThread }

procedure TUdpServerThread.OnIdle(tix64: Int64);
begin
  // do nothing by default
end;

constructor TUdpServerThread.Create(LogClass: TSynLogClass;
  const BindAddress, BindPort, ProcessName: RawUtf8; TimeoutMS: integer);
var
  ident: RawUtf8;
  res: TNetResult;
begin
  GetMem(fFrame, SizeOf(fFrame^));
  ident := ProcessName;
  if ident = '' then
    FormatUtf8('udp%srv', [BindPort], ident);
  LogClass.Add.Log(sllTrace, 'Create: bind %:% for input requests on %',
    [BindAddress, BindPort, ident], self);
  inherited Create({suspended=}false, nil, nil, LogClass, ident);
  res := NewSocket(BindAddress, BindPort, nlUdp, {bind=}true,
    TimeoutMS, TimeoutMS, TimeoutMS, 10, fSock, @fSockAddr);
  fBound := true; // notify DoExecute() ASAP that fSock should be set (or not)
  if res <> nrOk then
  begin
    // Windows seems to require this to avoid breaking the process on error
    {$ifdef OSWINDOWS}
    Resume{%H-}; // force Execute/DoExecute launch
    SleepHiRes(10);
    {$endif OSWINDOWS}
    // on binding error, raise exception before the thread is actually created
    raise EUdpServer.Create('Create binding error on %s:%s', self,
      [BindAddress, BindPort], res);
  end;
  AfterBind;
end;

destructor TUdpServerThread.Destroy;
var
  sock: TNetSocket;
  ilog: ISynLog;
begin
  fLogClass.EnterLocal(ilog, 'Destroy: ending % - processing=%',
    [fProcessName, fProcessing], self);
  // notify thread termination (if not already done)
  Terminate;
  // try to release fSock.WaitFor(1000) in DoExecute
  if fProcessing and
     (fSock <> nil) then
  {$ifdef OSPOSIX} // a broadcast address won't reach DoExecute
  if (fSockAddr.IP4 and $ff000000) = $ff000000 then // check x.x.x.255
    fSock.ShutdownAndClose({rdwr=}true) // will release acept() ASAP
  else
  {$endif OSPOSIX}
  begin
    sock := fSockAddr.NewSocket(nlUdp);
    if sock <> nil then
    begin
      fLogClass.Add.Log(sllTrace, 'Destroy: send final packet', self);
      sock.SetSendTimeout(10);
      sock.SendTo(pointer(UDP_SHUTDOWN), length(UDP_SHUTDOWN), fSockAddr);
      sock.ShutdownAndClose(false);
    end
    else
      fLogClass.Add.Log(sllTrace, 'Destroy: error creating final socket', self);
  end;
  // finalize this thread process
  TerminateAndWaitFinished;
  fLogClass.Add.Log(sllDebug, 'Destroy: TerminateAndWaitFinished Processing=% [%]',
    [fProcessing, fExecuteMessage], self);
  inherited Destroy;
  if fSock <> nil then
    fSock.ShutdownAndClose({rdwr=}true);
  FreeMem(fFrame);
end;

function TUdpServerThread.GetIPWithPort: RawUtf8;
begin
  fSockAddr.IPWithPort(result);
end;

procedure TUdpServerThread.AfterBind;
begin
  // do nothing by default
end;

procedure TUdpServerThread.DoExecute;
var
  len: integer;
  tix64: Int64;
  tix, lasttix: cardinal;
  remote: TNetAddr;
  res: TNetResult;
begin
  lasttix := 0;
  // main server process loop
  if not fBound then
    SleepHiRes(100, fBound, {boundDone=}true);
  if fSock = nil then // paranoid check
    FormatUtf8('%.DoExecute: % Bind failed', [self, fProcessName], fExecuteMessage)
  else
    while not Terminated do
    begin
      if fSock.WaitFor(1000, [neRead, neError]) <> [] then
      begin
        if Terminated then
        begin
          fLogClass.Add.Log(sllDebug, 'DoExecute: Terminated', self);
          break;
        end;
        res := fSock.RecvPending(len);
        if (res = nrOk) and
           (len >= 4) then
        begin
          PInteger(fFrame)^ := 0;
          len := fSock.RecvFrom(fFrame, SizeOf(fFrame^), remote);
          if Terminated then
            break;
          if (len >= 0) and // -1=error
             (CompareBuf(UDP_SHUTDOWN, fFrame, len) <> 0) then // paranoid
          begin
            inc(fReceived);
            OnFrameReceived(len, remote);
          end;
        end
        else if res <> nrRetry then
          SleepHiRes(100); // don't loop with 100% cpu on failure
      end;
      if Terminated then
        break;
      tix64 := mormot.core.os.GetTickCount64;
      tix := tix64 shr 9; // div 512
      if tix <> lasttix then
      begin
        lasttix := tix;
        OnIdle(tix64); // called every 512 ms at most
      end;
    end;
  // notify method to close all connections
  OnShutdown;
end;


{ ******************** Custom URI Routing using an efficient Radix Tree }

function UriMethod(const Text: RawUtf8; out Method: TUriRouterMethod): boolean;
begin
  result := false;
  if Text = '' then
    exit;
  case PCardinal(Text)^ of // case-sensitive test in occurrence order
    ord('G') + ord('E') shl 8 + ord('T') shl 16:
      Method := urmGet;
    ord('P') + ord('O') shl 8 + ord('S') shl 16 + ord('T') shl 24:
      Method := urmPost;
    ord('P') + ord('U') shl 8 + ord('T') shl 16:
      Method := urmPut;
    ord('P') + ord('A') shl 8 + ord('T') shl 16 + ord('C') shl 24:
      Method := urmPatch;
    ord('H') + ord('E') shl 8 + ord('A') shl 16 + ord('D') shl 24:
      Method := urmHead;
    ord('D') + ord('E') shl 8 + ord('L') shl 16 + ord('E') shl 24:
      Method := urmDelete;
    ord('O') + ord('P') shl 8 + ord('T') shl 16 + ord('I') shl 24:
      Method := urmOptions;
  else
    exit;
  end;
  result := true;
end;

function IsValidUriRoute(p: PUtf8Char): boolean;
begin
  result := false;
  if p = nil then
    exit;
  repeat
    if p^ = '<' then // parse <param> or <path:param> place-holders
    begin
      inc(p);
      while p^ <> '>' do
        if p^ = #0 then
          exit
        else
          inc(p);
    end
    else if not (p^ in ['/', '_', '-', '.', '$', '0'..'9', 'a'..'z', 'A'..'Z']) then
      exit; // not a valid plain URI character
    inc(p);
  until p^ = #0;
  result := true;
end;


{ TUriTreeNode }

function TUriTreeNode.Split(const Text: RawUtf8): TRadixTreeNode;
begin
  result := inherited Split(Text);
  TUriTreeNode(result).Data := Data;
  Finalize(Data);
  FillCharFast(Data, SizeOf(Data), 0);
end;

function TUriTreeNode.LookupParam(Ctxt: TObject; Pos: PUtf8Char; Len: integer): boolean;
var
  req: THttpServerRequest absolute Ctxt;
  n: PtrInt;
  v: PIntegerArray;
begin
  result := false;
  if Len < 0 then // Pos^ = '?par=val&par=val&...'
  begin
    include(req.fInternalFlags, ifUrlParamPosSet);
    req.fUrlParamPos := Pos; // for faster req.UrlParam()
    exit;
  end;
  req.fRouteName := pointer(Names); // fast assignment as pointer reference
  n := length(Names) * 2; // length(Names[]) = current parameter index
  if length(req.fRouteValuePosLen) < n then
    SetLength(req.fRouteValuePosLen, n + 24); // alloc once by 12 params
  v := @req.fRouteValuePosLen[n - 2];
  n := Pos - pointer(req.Url);
  if PtrUInt(n) > PtrUInt(length(req.Url)) then
    exit; // paranoid check to avoid any overflow
  v[0] := n;   // value position (0-based) in Ctxt.Url
  v[1] := Len; // value length in Ctxt.Url
  result := true;
end;

procedure TUriTreeNode.RewriteUri(Ctxt: THttpServerRequestAbstract);
var
  n: TDALen;
  len: integer;
  t, v: PIntegerArray;
  p: PUtf8Char;
  new: pointer; // fast temporary RawUtf8
begin
  // compute length of the new URI with injected values
  t := pointer(Data.ToUriPosLen); // [pos1,len1,valndx1,...] trio rules
  n := PDALen(PAnsiChar(t) - _DALEN)^ + _DAOFF;
  v := pointer(THttpServerRequest(Ctxt).fRouteValuePosLen); // [pos,len] pairs
  if v = nil then
     exit; // paranoid
  len := Data.ToUriStaticLen;
  repeat
    if t[2] >= 0 then            // t[2]=valndx in v=fRouteValuePosLen[]
      inc(len, v[t[2] * 2 + 1]); // add value length
    t := @t[3];
    dec(n, 3)
  until n = 0;
  // compute the new URI with injected values
  new := FastNewString(len, CP_UTF8);
  t := pointer(Data.ToUriPosLen);
  n := PDALen(PAnsiChar(t) - _DALEN)^ + _DAOFF;
  p := new; // write new URI
  repeat
    if t[1] <> 0 then    // t[1]=len
    begin
      MoveFast(PByteArray(Data.ToUri)[t[0]], p^, t[1]); // static
      inc(p, t[1]);
    end;
    if t[2] >= 0 then    // t[2]=valndx in fRouteValuePosLen[]
    begin
      v := @THttpServerRequest(Ctxt).fRouteValuePosLen[t[2] * 2];
      MoveFast(PByteArray(Ctxt.Url)[v[0]], p^, v[1]); // value [pos,len] pair
      inc(p, v[1]);
    end;
    t := @t[3];
    dec(n, 3)
  until n = 0;
  FastAssignNew(THttpServerRequest(Ctxt).fUrl, new); // replace
  //if p - new <> len then raise EUriRouter.Create('??');
end;


{ TUriTree }

function TUriTree.Root: TUriTreeNode;
begin
  result := fRoot as TUriTreeNode;
end;


{ TUriRouter }

destructor TUriRouter.Destroy;
var
  m: TUriRouterMethod;
begin
  inherited Destroy;
  for m := low(fTree) to high(fTree) do
    fTree[m].Free;
end;

procedure TUriRouter.Clear(aMethods: TUriRouterMethods);
var
  m: TUriRouterMethod;
begin
  if self = nil then
    exit; // avoid unexpected GPF
  fSafe.WriteLock;
  try
    FillCharFast(fEntries, SizeOf(fEntries), 0);
    for m := low(fTree) to high(fTree) do
      if m in aMethods then
        FreeAndNil(fTree[m]);
  finally
    fSafe.WriteUnLock;
  end;
end;

procedure TUriRouter.Setup(aFrom: TUriRouterMethod; const aFromUri: RawUtf8;
  aTo: TUriRouterMethod; const aToUri: RawUtf8;
  const aExecute: TOnHttpServerRequest; aExecuteOpaque: pointer);
var
  n: TUriTreeNode;
  u: PUtf8Char;
  fromU, toU, item: RawUtf8;
  names: TRawUtf8DynArray;
  pos: PtrInt;
begin
  if self = nil then
    exit; // avoid unexpected GPF
  fromU := StringReplaceAll(aFromUri, '*', '<path:path>');
  toU := StringReplaceAll(aToUri, '*', '<path:path>');
  if not IsValidUriRoute(pointer(fromU)) then
    EUriRouter.RaiseUtf8('Invalid char in %.Setup(''%'')',
      [self, aFromUri]);
  fSafe.WriteLock;
  try
    if fTree[aFrom] = nil then
      fTree[aFrom] := TUriTree.Create(fTreeNodeClass, fTreeOptions);
    n := fTree[aFrom].Setup(fromU, names) as TUriTreeNode;
    if n = nil then
      exit;
    // the leaf should have the Rewrite/Run information to process on match
    if n.Data.ToUri <> '' then
      if toU = n.Data.ToUri then
        exit // same redirection: do nothing
      else
        EUriRouter.RaiseUtf8('%.Setup(''%''): already redirect to %',
          [self, aFromUri, n.Data.ToUri]);
    if Assigned(n.Data.Execute) then
      if CompareMem(@n.Data.Execute, @aExecute, SizeOf(TMethod)) then
        exit // same callback: do nothing
      else
        EUriRouter.RaiseUtf8('%.Setup(''%''): already registered',
          [self, aFromUri]);
    if Assigned(aExecute) then
    begin
      // this URI should redirect to a TOnHttpServerRequest callback
      n.Data.Execute := aExecute;
      n.Data.ExecuteOpaque := aExecuteOpaque;
    end
    else
    begin
      n.Data.ToUriMethod := aTo;
      n.Data.ToUri := toU;
      n.Data.ToUriPosLen := nil; // store [pos1,len1,valndx1,...] trios
      n.Data.ToUriStaticLen := 0;
      n.Data.ToUriErrorStatus := Utf8ToInteger(toU, 200, 599, 0);
      if n.Data.ToUriErrorStatus = 0 then // a true URI, not an HTTP error code
      begin
        // pre-compute the rewritten URI into Data.ToUriPosLen[]
        u := pointer(toU);
        if u = nil then
          EUriRouter.RaiseUtf8('No ToUri in %.Setup(''%'')',
            [self, aFromUri]);
        if PosExChar('<', toU) <> 0 then // n.Data.ToUriPosLen=nil to use ToUri
          repeat
            pos := u - pointer(toU);
            GetNextItem(u, '<', item); // static
            AddInteger(n.Data.ToUriPosLen, pos);          // position
            AddInteger(n.Data.ToUriPosLen, length(item)); // length (may be 0)
            inc(n.Data.ToUriStaticLen, length(item));
            if (u = nil) or
               (u^ = #0) then
              pos := -1
            else
            begin
              GetNextItem(u, '>', item); // <name>
              pos := PosExChar(':', item);
              if pos <> 0 then
                system.delete(item, 1, pos);
              if item = '' then
                EUriRouter.RaiseUtf8('Void <> in %.Setup(''%'')',
                  [self, aToUri]);
              pos := FindRawUtf8(names, item);
              if pos < 0 then
                EUriRouter.RaiseUtf8('Unknown <%> in %.Setup(''%'')',
                  [item, self, aToUri]);
            end;
            AddInteger(n.Data.ToUriPosLen, pos);  // value index in Names[]
          until (u = nil) or
                (u^ = #0);
      end;
    end;
    inc(fEntries[aFrom]);
  finally
    fSafe.WriteUnLock;
  end;
end;

constructor TUriRouter.Create(aNodeClass: TRadixTreeNodeClass;
  aOptions: TRadixTreeOptions);
begin
  if aNodeClass = nil then
    EUriRouter.RaiseUtf8('%.Create with aNodeClass=nil', [self]);
  fTreeNodeClass := aNodeClass;
  fTreeOptions := aOptions;
  inherited Create;
end;

procedure TUriRouter.Rewrite(aFrom: TUriRouterMethod; const aFromUri: RawUtf8;
  aTo: TUriRouterMethod; const aToUri: RawUtf8);
begin
  Setup(aFrom, aFromUri, aTo, aToUri, nil, nil);
end;

procedure TUriRouter.Run(aFrom: TUriRouterMethods; const aFromUri: RawUtf8;
  const aExecute: TOnHttpServerRequest; aExecuteOpaque: pointer);
var
  m: TUriRouterMethod;
begin
  for m := low(fTree) to high(fTree) do
    if m in aFrom then
      Setup(m, aFromUri, m, '', aExecute, aExecuteOpaque);
end;

procedure TUriRouter.Get(const aFrom, aTo: RawUtf8; aToMethod: TUriRouterMethod);
begin
  Rewrite(urmGet, aFrom, aToMethod, aTo);
end;

procedure TUriRouter.Post(const aFrom, aTo: RawUtf8; aToMethod: TUriRouterMethod);
begin
  Rewrite(urmPost, aFrom, aToMethod, aTo);
end;

procedure TUriRouter.Put(const aFrom, aTo: RawUtf8; aToMethod: TUriRouterMethod);
begin
  Rewrite(urmPut, aFrom, aToMethod, aTo);
end;

procedure TUriRouter.Patch(const aFrom, aTo: RawUtf8; aToMethod: TUriRouterMethod);
begin
  Rewrite(urmPatch, aFrom, aToMethod, aTo);
end;

procedure TUriRouter.Delete(const aFrom, aTo: RawUtf8; aToMethod: TUriRouterMethod);
begin
  Rewrite(urmDelete, aFrom, aToMethod, aTo);
end;

procedure TUriRouter.Options(const aFrom, aTo: RawUtf8;
  aToMethod: TUriRouterMethod);
begin
  Rewrite(urmOptions, aFrom, aToMethod, aTo);
end;

procedure TUriRouter.Head(const aFrom, aTo: RawUtf8; aToMethod: TUriRouterMethod);
begin
  Rewrite(urmHead, aFrom, aToMethod, aTo);
end;

procedure TUriRouter.Get(const aUri: RawUtf8;
  const aExecute: TOnHttpServerRequest; aExecuteOpaque: pointer);
begin
  Run([urmGet], aUri, aExecute, aExecuteOpaque);
end;

procedure TUriRouter.Post(const aUri: RawUtf8;
  const aExecute: TOnHttpServerRequest; aExecuteOpaque: pointer);
begin
  Run([urmPost], aUri, aExecute, aExecuteOpaque);
end;

procedure TUriRouter.Put(const aUri: RawUtf8;
  const aExecute: TOnHttpServerRequest; aExecuteOpaque: pointer);
begin
  Run([urmPut], aUri, aExecute, aExecuteOpaque);
end;

procedure TUriRouter.Patch(const aUri: RawUtf8;
  const aExecute: TOnHttpServerRequest; aExecuteOpaque: pointer);
begin
  Run([urmPatch], aUri, aExecute, aExecuteOpaque);
end;

procedure TUriRouter.Delete(const aUri: RawUtf8;
  const aExecute: TOnHttpServerRequest; aExecuteOpaque: pointer);
begin
  Run([urmDelete], aUri, aExecute, aExecuteOpaque);
end;

procedure TUriRouter.Options(const aUri: RawUtf8;
  const aExecute: TOnHttpServerRequest; aExecuteOpaque: pointer);
begin
  Run([urmOptions], aUri, aExecute, aExecuteOpaque);
end;

procedure TUriRouter.Head(const aUri: RawUtf8;
  const aExecute: TOnHttpServerRequest; aExecuteOpaque: pointer);
begin
  Run([urmHead], aUri, aExecute, aExecuteOpaque);
end;

procedure TUriRouter.RunMethods(RouterMethods: TUriRouterMethods;
  Instance: TObject; const Prefix: RawUtf8);
var
  met: TPublishedMethodInfoDynArray;
  m: PtrInt;
begin
  if (self <> nil) and
     (Instance <> nil) and
     (RouterMethods <> []) then
    for m := 0 to GetPublishedMethods(Instance, met) - 1 do
      Run(RouterMethods, Prefix + StringReplaceChars(met[m].Name, '_', '-'),
        TOnHttpServerRequest(met[m].Method));
end;

function TUriRouter.Process(Ctxt: THttpServerRequestAbstract): integer;
var
  m: TUriRouterMethod;
  t: TUriTree;
  found: TUriTreeNode;
begin
  result := 0; // nothing to process
  if (self = nil) or
     (Ctxt = nil) or
     (Ctxt.Url = '') or
     not UriMethod(Ctxt.Method, m) then
    exit;
  THttpServerRequest(Ctxt).fRouteName := nil; // paranoid: if called w/o Prepare
  THttpServerRequest(Ctxt).fRouteNode := nil;
  t := fTree[m];
  if t = nil then
    exit; // this method has no registration yet
  fSafe.ReadLock;
  {$ifdef HASFASTTRYFINALLY}
  try
  {$else}
  begin
  {$endif HASFASTTRYFINALLY}
    // fast recursive parsing - may return nil, but never raises exception
    found := pointer(TUriTreeNode(t.fRoot).Lookup(pointer(Ctxt.Url), Ctxt));
  {$ifdef HASFASTTRYFINALLY}
  finally
  {$endif HASFASTTRYFINALLY}
    fSafe.ReadUnLock;
  end;
  if found <> nil then
    // there is something to react on
    if Assigned(found.Data.Execute) then
    begin
      // request is implemented via a method
      THttpServerRequest(Ctxt).fRouteNode := found;
      result := found.Data.Execute(Ctxt);
    end
    else if found.Data.ToUri <> '' then
    begin
      // request is not implemented here, but the Url should be rewritten
      if m <> found.Data.ToUriMethod then
        Ctxt.Method := URIROUTERMETHOD[found.Data.ToUriMethod];
      if found.Data.ToUriErrorStatus <> 0 then
        result := found.Data.ToUriErrorStatus // redirect to an error code
      else if found.Data.ToUriPosLen = nil then
        Ctxt.Url := found.Data.ToUri    // only static -> just replace URI
      else
        found.RewriteUri(Ctxt);         // compute new URI with injected values
    end;
end;

function TUriRouter.Lookup(const aUri, aUriMethod: RawUtf8): TUriTreeNode;
var
  m: TUriRouterMethod;
  t: TUriTree;
begin
  result := nil;
  if (self = nil) or
     (aUri = '') or
     not UriMethod(aUriMethod, m) then
    exit;
  t := fTree[m];
  if t = nil then
    exit; // this method has no registration yet
  fSafe.ReadLock;
  {$ifdef HASFASTTRYFINALLY}
  try
  {$else}
  begin
  {$endif HASFASTTRYFINALLY}
    result := pointer(TUriTreeNode(t.fRoot).Lookup(pointer(aUri), nil));
  {$ifdef HASFASTTRYFINALLY}
  finally
  {$endif HASFASTTRYFINALLY}
    fSafe.ReadUnLock;
  end;
end;


{ ******************** Shared Server-Side HTTP Process }

{ THttpServerRequest }

constructor THttpServerRequest.Create(aServer: THttpServerGeneric;
  aConnectionID: THttpServerConnectionID; aConnectionThread: TSynThread;
  aConnectionAsyncHandle: TConnectionAsyncHandle;
  aConnectionFlags: THttpServerRequestFlags;
  aConnectionOpaque: PHttpServerConnectionOpaque);
begin
  inherited Create;
  fServer := aServer;
  fConnectionID := aConnectionID;
  fConnectionThread := aConnectionThread;
  fConnectionAsyncHandle := aConnectionAsyncHandle;
  fConnectionFlags := aConnectionFlags;
  fConnectionOpaque := aConnectionOpaque;
end;

procedure THttpServerRequest.Recycle(aConnectionID: THttpServerConnectionID;
  aConnectionThread: TSynThread; aConnectionAsyncHandle: TConnectionAsyncHandle;
  aConnectionFlags: THttpServerRequestFlags;
  aConnectionOpaque: PHttpServerConnectionOpaque);
begin
  fConnectionID := aConnectionID;
  fConnectionAsyncHandle := aConnectionAsyncHandle;
  fConnectionThread := aConnectionThread;
  fConnectionFlags := aConnectionFlags;
  fConnectionOpaque := aConnectionOpaque;
  // reset fields as Create() does
  FastAssignNew(fHost);
  FastAssignNew(fAuthBearer);
  FastAssignNew(fUserAgent);
  fRespStatus := 0;
  fOutContent := '';
  FastAssignNew(fOutContentType);
  FastAssignNew(fOutCustomHeaders);
  fAuthenticationStatus := hraNone;
  fInternalFlags := [];
  FastAssignNew(fAuthenticatedUser);
  fErrorMessage := '';
  fUrlParamPos := nil;
  fRouteNode := nil;
  fRouteName := nil; // no fRouteValuePosLen := nil (to reuse allocated array)
  // Prepare() will set the other fields
end;

destructor THttpServerRequest.Destroy;
begin
  fTempWriter.Free;
  // inherited Destroy; is void
end;

const
  _CMD_200: array[boolean, boolean] of string[31] = (
   ('HTTP/1.1 200 OK'#13#10,
    'HTTP/1.0 200 OK'#13#10),
   ('HTTP/1.1 206 Partial Content'#13#10,
    'HTTP/1.0 206 Partial Content'#13#10));
  _CMD_XXX: array[boolean] of string[15] = (
    'HTTP/1.1 ',
    'HTTP/1.0 ');

function THttpServerRequest.SetupResponse(var Context: THttpRequestContext;
  CompressGz, MaxSizeAtOnce: integer): PRawByteStringBuffer;

  procedure ProcessStaticFile;
  var
    fn: TFileName;
    fsiz: Int64;
  begin
    ExtractOutContentType;
    fn := Utf8ToString(OutContent); // safer than Utf8ToFileName() here
    OutContent := '';
    ExtractHeader(fOutCustomHeaders, STATICFILE_PROGSIZE, nil, @Context.ContentLength);
    if Context.ContentLength <> 0 then
      // STATICFILE_PROGSIZE: file is not fully available: wait for sending
      if ((not (rfWantRange in Context.ResponseFlags)) or
          Context.ValidateRange) then
      begin
        if FileInfoByName(fn, fsiz, Context.ContentLastModified) and
          (fsiz >= 0) and // not a folder
          (fsiz <= Context.ContentLength) then
        begin
          Context.ContentStream := TStreamWithPositionAndSize.Create; // <> nil
          Context.ResponseFlags := Context.ResponseFlags +
            [rfAcceptRange, rfContentStreamNeedFree, rfProgressiveStatic];
        end
        else
          fRespStatus := HTTP_NOTFOUND;
      end
      else
        fRespStatus := HTTP_RANGENOTSATISFIABLE
    else if (not Assigned(fServer.OnSendFile)) or
            (not fServer.OnSendFile(self, fn)) then
    begin
      // regular file sending by chunks
      fRespStatus := Context.ContentFromFile(fn, CompressGz);
      if fRespStatus = HTTP_SUCCESS then
        OutContent := Context.Content; // small static file content
    end;
    if not StatusCodeIsSuccess(fRespStatus) then
      fErrorMessage := 'Error getting file'; // detected by ProcessErrorMessage
  end;

  procedure ProcessErrorMessage;
  begin
    HtmlEscapeString(fErrorMessage, fOutContentType, hfAnyWhere); // safety
    FormatUtf8(
      '<!DOCTYPE html><html><body style="font-family:verdana">' +
      '<h1>% Server Error %</h1><hr>' +
      '<p>HTTP % %</p><p>%</p><small>%</small></body></html>',
      [fServer.ServerName, fRespStatus, fRespStatus,
       fServer.StatusCodeToText(fRespStatus)^, fOutContentType, XPOWEREDVALUE],
      RawUtf8(fOutContent));
    fOutCustomHeaders := '';
    fOutContentType := HTML_CONTENT_TYPE; // body = HTML message to display
  end;

var
  P: PUtf8Char;
  h: PRawByteStringBuffer;
  // note: caller should have set hfConnectionClose in Context.HeaderFlags
begin
  // process content
  Context.ContentLength := 0; // needed by ProcessStaticFile
  Context.ContentLastModified := 0;
  Context.CommandUri := fUrl; // may have been normalized/cleaned during process
  if (fOutContentType <> '') and
     (fOutContentType[1] = '!') then
    if fOutContentType = NORESPONSE_CONTENT_TYPE then
      fOutContentType := '' // true HTTP always expects a response
    else if (fOutContent <> '') and
            (fOutContentType = STATICFILE_CONTENT_TYPE) then
      ProcessStaticFile;
  if fErrorMessage <> '' then
    ProcessErrorMessage;
  // append Command
  h := @Context.Head;
  h^.Reset; // reuse 2KB header buffer
  if fRespStatus = HTTP_SUCCESS then // optimistic approach
    h^.AppendShort(_CMD_200[
      rfWantRange in Context.ResponseFlags, // HTTP_PARTIALCONTENT=206 support
      rfHttp10 in Context.ResponseFlags])   // HTTP/1.0 support
  else
  begin // other cases
    h^.AppendShort(_CMD_XXX[rfHttp10 in Context.ResponseFlags]);
    h^.Append(SmallUInt32Utf8[MinPtrUInt(high(SmallUInt32Utf8), fRespStatus)]);
    h^.Append(' ');
    h^.Append(mormot.core.text.StatusCodeToText(fRespStatus)^); // need English
    h^.AppendCRLF;
  end;
  // append (and sanitize CRLF) custom headers from Request() method
  P := pointer(OutCustomHeaders);
  if P <> nil then
    Context.HeadAddCustom(P, P + PStrLen(P - _STRLEN)^);
  P := pointer(Context.ResponseHeaders);
  if P <> nil then // e.g. 'WWW-Authenticate: #####'#13#10
    Context.HeadAddCustom(P, P + PStrLen(P - _STRLEN)^);
  // generic headers
  if not (hhServer in Context.HeadCustom) then
    h^.Append(fServer.fRequestHeaders); // Server: and X-Powered-By:
  if hsoIncludeDateHeader in fServer.Options then
    fServer.AppendHttpDate(h^);
  Context.Content := fOutContent;
  if (fOutContentType = '') and
     (fOutContent <> '') and
     not (hsoContentTypeNoGuess in fServer.Options) then
    GetMimeContentTypeFromBuffer(fOutContent, Context.ContentType)
  else
    Context.ContentType := fOutContentType;
  fOutContent := ''; // dec RefCnt to release body memory ASAP
  result := Context.CompressContentAndFinalizeHead(MaxSizeAtOnce); // set State
  // now TAsyncConnectionsSockets.Write(result) should be called
end;

procedure THttpServerRequest.SetErrorMessage(const Fmt: RawUtf8;
  const Args: array of const);
begin
  FormatString(Fmt, Args, fErrorMessage);
end;

function THttpServerRequest.TempJsonWriter(
  var temp: TTextWriterStackBuffer): TJsonWriter;
begin
  if fTempWriter = nil then
  begin
    fTempWriter := TJsonWriter.CreateOwnedStream(temp, {noshared=}true);
    fTempWriter.FlushToStreamNoAutoResize := true;
  end
  else
    fTempWriter.CancelAllWith(temp); // reuse during THttpServerRequest lifetime
  result := fTempWriter;
end;

function THttpServerRequest.SetOutJson(Value: pointer; TypeInfo: PRttiInfo): cardinal;
var
  temp: TTextWriterStackBuffer; // 8KB work buffer on stack
begin
  TempJsonWriter(temp).AddTypedJson(Value, TypeInfo, []);
  fTempWriter.SetText(RawUtf8(fOutContent));
  fOutContentType := JSON_CONTENT_TYPE_VAR;
  result := HTTP_SUCCESS;
end;

function THttpServerRequest.SetOutJson(Value: TObject): cardinal;
var
  temp: TTextWriterStackBuffer;
begin
  TempJsonWriter(temp).WriteObject(Value, []);
  fTempWriter.SetText(RawUtf8(fOutContent));
  fOutContentType := JSON_CONTENT_TYPE_VAR;
  result := HTTP_SUCCESS;
end;

function THttpServerRequest.RouteOpaque: pointer;
begin
  result := fRouteNode;
  if result <> nil then
    result := TUriTreeNode(result).Data.ExecuteOpaque;
end;

function THttpServerRequest.AsyncHandle: TConnectionAsyncHandle;
begin
  result := fConnectionAsyncHandle;
  if result = 0 then
    EHttpServer.RaiseUtf8('% has no async response support', [fServer]);
end;

procedure THttpServerRequest.ToDocVariant(out Dest: TDocVariantData);
var
  p: PUtf8Char;
begin
  p := UrlParamPos;
  if p = nil then
    Dest.InitJson(fInContent, JSON_FAST) // try decoding from JSON body
  else
    Dest.InitFromUrl(p + 1, JSON_FAST);  // parameters from /uri?n1=v1&n2=v2
  Dest.AddValueText('url', Split(fUrl, '?'));
  Dest.AddValueText('method', fMethod);
  if fInContent <> '' then
    Dest.AddValueText('content', fInContent);
end;

{$ifdef USEWININET}

function THttpServerRequest.GetFullUrl: SynUnicode;
begin
  if fHttpApiRequest = nil then
    result := ''
  else
    SetString(result, fHttpApiRequest^.CookedUrl.pFullUrl,
      fHttpApiRequest^.CookedUrl.FullUrlLength shr 1); // length in bytes
end;

{$endif USEWININET}


{ THttpServerGeneric }

constructor THttpServerGeneric.Create(const OnStart, OnStop: TOnNotifyThread;
  const ProcessName: RawUtf8; ProcessOptions: THttpServerOptions;
  aLog: TSynLogClass);
begin
  fOptions := ProcessOptions; // should be set before SetServerName
  SetServerName('mORMot2 (' + OS_TEXT + ')');
  if hsoEnableLogging in fOptions then
  begin
    fLogger := THttpLogger.Create;
    fLogger.Parse(LOGFORMAT_COMBINED);   // default nginx-like format
    fOnAfterResponse := fLogger.Append;  // redirect requests to the logger
  end;
  if fOptions * [hsoTelemetryCsv, hsoTelemetryJson] <> [] then
  begin
    fAnalyzer := THttpAnalyzer.Create; // no suspend file involved
    fAnalyzer.OnContinue := fLogger;
    fOnAfterResponse := fAnalyzer.Append;
    if hsoTelemetryCsv in fOptions then
      THttpAnalyzerPersistCsv.CreateOwned(fAnalyzer);
    if hsoTelemetryJson in fOptions then
      THttpAnalyzerPersistJson.CreateOwned(fAnalyzer);
  end;
  inherited Create(hsoCreateSuspended in fOptions, OnStart, OnStop,
    aLog, ProcessName);
end;

destructor THttpServerGeneric.Destroy;
begin
  inherited Destroy;
  FreeAndNil(fRoute);
  FreeAndNil(fAnalyzer);
  FreeAndNil(fLogger);
end;

function THttpServerGeneric.Route: TUriRouter;
begin
  result := nil; // avoid GPF
  if self = nil then
    exit;
  result := fRoute;
  if result <> nil then
    exit;
  GlobalLock; // paranoid thread-safety
  try
    if fRoute = nil then
    begin
      if fRouterClass = nil then
        fRouterClass := TUriTreeNode;
      fRoute := TUriRouter.Create(fRouterClass);
    end;
  finally
    GlobalUnLock;
  end;
  result := fRoute;
end;

function THttpServerGeneric.ReplaceRoute(another: TUriRouter): TUriRouter;
begin
  result := nil;
  if self = nil then
    exit;
  if fFavIconRouted then
    another.Get('/favicon.ico', GetFavIcon); // let SetFavIcon() continue
  GlobalLock; // paranoid thread-safety
  try
    result := fRoute;
    if result <> nil then
      result.Safe.WriteLock;
    fRoute := another;
    if result <> nil then
      result.Safe.WriteUnLock;
  finally
    GlobalUnLock;
  end;
end;

procedure THttpServerGeneric.SetFavIcon(const FavIconContent: RawByteString);
begin
  if FavIconContent = 'default' then
    fFavIcon := FavIconBinary
  else
    fFavIcon := FavIconContent;
  if fFavIconRouted then
    exit; // need to register the route once, but allow custom icon
  Route.Get('/favicon.ico', GetFavIcon);
  fFavIconRouted := true;
end;

function THttpServerGeneric.GetFavIcon(Ctxt: THttpServerRequestAbstract): cardinal;
begin
  if fFavIcon = '' then
    result := HTTP_NOTFOUND
  else if FindNameValue(pointer(Ctxt.InHeaders), 'IF-NONE-MATCH:') <> nil then
    result := HTTP_NOTMODIFIED
  else
  begin
    Ctxt.OutContent := fFavIcon;
    Ctxt.OutContentType := 'image/x-icon';
    Ctxt.OutCustomHeaders := 'Etag: "ok"';
    result := HTTP_SUCCESS;
  end;
end;

function THttpServerGeneric.NextConnectionID: integer;
begin
  result := InterlockedIncrement(fCurrentConnectionID);
  if result = maxInt - 2048 then
    fCurrentConnectionID := 0; // paranoid keep ID in positive 31-bit range
end;

procedure THttpServerGeneric.RegisterCompress(aFunction: THttpSocketCompress;
  aCompressMinSize: integer; aPriority: integer);
begin
  fCompressList.RegisterFunc(aFunction, aCompressMinSize, aPriority);
end;

procedure THttpServerGeneric.Shutdown;
begin
  if self <> nil then
    fShutdownInProgress := true;
end;

function THttpServerGeneric.Request(Ctxt: THttpServerRequestAbstract): cardinal;
begin
  if (self = nil) or
     fShutdownInProgress or
     not Assigned(OnRequest) then
    result := HTTP_NOTFOUND
  else
  begin
    if Assigned(Ctxt.ConnectionThread) and
       Assigned(fOnThreadStart) and
       Ctxt.ConnectionThread.InheritsFrom(TSynThread) and
       (not Assigned(TSynThread(Ctxt.ConnectionThread).StartNotified)) then
      NotifyThreadStart(TSynThread(Ctxt.ConnectionThread));
    result := OnRequest(Ctxt);
  end;
end;

function THttpServerGeneric.{%H-}Callback(Ctxt: THttpServerRequest;
  aNonBlocking: boolean): cardinal;
begin
  raise EHttpServer.CreateUtf8('%.Callback is not implemented: try to use ' +
    'another communication protocol, e.g. WebSockets', [self]);
end;

procedure THttpServerGeneric.AsyncResponse(Connection: TConnectionAsyncHandle;
  const Content, ContentType: RawUtf8; Status: cardinal);
begin
  EHttpServer.RaiseUtf8('%.AsyncResponse is not implemented: try to use ' +
    'another server class, e.g. THttpAsyncServer', [self]);
end;

procedure THttpServerGeneric.AsyncResponseFmt(Connection: TConnectionAsyncHandle;
  const ContentFmt: RawUtf8; const Args: array of const;
  const ContentType: RawUtf8; Status: cardinal);
var
  json: RawUtf8;
begin
  FormatUtf8(ContentFmt, Args, json);
  AsyncResponse(Connection, json, ContentType, Status);
end;

procedure THttpServerGeneric.AsyncResponseJson(Connection: TConnectionAsyncHandle;
  Value: pointer; TypeInfo: PRttiInfo; Status: cardinal);
var
  json: RawUtf8;
begin
  SaveJson(Value^, TypeInfo, [], json);
  AsyncResponse(Connection, json, JSON_CONTENT_TYPE_VAR, Status);
end;

procedure THttpServerGeneric.AsyncResponseError(
  Connection: TConnectionAsyncHandle; const Message: RawUtf8; Status: cardinal);
begin
  AsyncResponse(Connection, Message, TEXT_CONTENT_TYPE, Status);
end;

procedure THttpServerGeneric.ParseRemoteIPConnID(const Headers: RawUtf8;
  var RemoteIP: RawUtf8; var RemoteConnID: THttpServerConnectionID);
var
  P: PUtf8Char;
begin
  if self = nil then // = nil e.g. from TRtspOverHttpServer
    exit;
  // real Internet IP (replace RemoteIP='127.0.0.1' from a proxy)
  if fRemoteIPHeaderUpper <> '' then
    FindNameValue(Headers, pointer(fRemoteIPHeaderUpper),
      RemoteIP, {keepnotfound=}true, {Sep=}':');
  // real proxy connection ID
  if fRemoteConnIDHeaderUpper <> '' then
  begin
    P := FindNameValue(pointer(Headers), pointer(fRemoteConnIDHeaderUpper));
    if (P <> nil) and
       (P^ = ':') then
      SetQWord(P + 1, PQWord(@RemoteConnID)^);
  end;
  if RemoteConnID = 0 then
    // fallback to 31-bit sequence
    RemoteConnID := NextConnectionID;
end;

procedure THttpServerGeneric.AppendHttpDate(var Dest: TRawByteStringBuffer);
begin
  // overriden in THttpAsyncServer.AppendHttpDate with its own per-second cache
  Dest.AppendShort(HttpDateNowUtc);
end;

function THttpServerGeneric.StatusCodeToText(Code: cardinal): PRawUtf8;
begin
  result := mormot.core.text.StatusCodeToText(Code); // default English text
end;

procedure THttpServerGeneric.SetTlsServerNameCallback(
  const OnAccept: TOnNetTlsAcceptServerName);
begin
  // do nothing by default
end;

function THttpServerGeneric.CanNotifyCallback: boolean;
begin
  result := (self <> nil) and
            (fCallbackSendDelay <> nil);
end;

procedure THttpServerGeneric.SetRouterClass(aRouter: TRadixTreeNodeClass);
begin
  if fRouterClass <> nil then
    EHttpServer.RaiseUtf8('%.RouterClass already set', [self]);
  fRouterClass := aRouter;
end;

procedure THttpServerGeneric.SetServerName(const aName: RawUtf8);
begin
  fServerName := aName;
  FormatUtf8('Server: %'#13#10, [fServerName], fRequestHeaders);
  if not (hsoNoXPoweredHeader in fOptions) then
    Append(fRequestHeaders, XPOWEREDNAME + ': ' + XPOWEREDVALUE + #13#10);
  fDefaultRequestOptions := [];
  if hsoHeadersUnfiltered in fOptions then
    include(fDefaultRequestOptions, hroHeadersUnfiltered);
end;

procedure THttpServerGeneric.SetOptions(opt: THttpServerOptions);
begin
  if fOptions = opt then
    exit;
  fOptions := opt;
  SetServerName(fServerName); // recompute fRequestHeaders
end;

procedure THttpServerGeneric.SetOnRequest(
  const aRequest: TOnHttpServerRequest);
begin
  fOnRequest := aRequest;
end;

procedure THttpServerGeneric.SetOnBeforeBody(
  const aEvent: TOnHttpServerBeforeBody);
begin
  fOnBeforeBody := aEvent;
end;

procedure THttpServerGeneric.SetOnBeforeRequest(
  const aEvent: TOnHttpServerRequest);
begin
  fOnBeforeRequest := aEvent;
end;

procedure THttpServerGeneric.SetOnAfterRequest(
  const aEvent: TOnHttpServerRequest);
begin
  fOnAfterRequest := aEvent;
end;

procedure THttpServerGeneric.SetOnAfterResponse(
  const aEvent: TOnHttpServerAfterResponse);
begin
  fOnAfterResponse := aEvent;
end;

function THttpServerGeneric.DoBeforeRequest(Ctxt: THttpServerRequest): cardinal;
begin
  if Assigned(fOnBeforeRequest) then
    result := fOnBeforeRequest(Ctxt)
  else
    result := 0;
end;

function THttpServerGeneric.DoAfterRequest(Ctxt: THttpServerRequest): cardinal;
begin
  if Assigned(fOnAfterRequest) then
    result := fOnAfterRequest(Ctxt)
  else
    result := 0;
end;

procedure THttpServerGeneric.SetMaximumAllowedContentLength(aMax: Int64);
begin
  fMaximumAllowedContentLength := aMax;
end;

procedure THttpServerGeneric.SetRemoteIPHeader(const aHeader: RawUtf8);
begin
  fRemoteIPHeader := aHeader;
  fRemoteIPHeaderUpper := UpperCase(aHeader);
end;

procedure THttpServerGeneric.SetRemoteConnIDHeader(const aHeader: RawUtf8);
begin
  fRemoteConnIDHeader := aHeader;
  fRemoteConnIDHeaderUpper := UpperCase(aHeader);
end;


const
  // was generated from InitNetTlsContextSelfSignedServer() commented lines
  // - .pfx/pkcs#12 of RSA-2048 cert+pk for CN:127.0.0.1 valid until 03/03/2035
  // - was regenerated with OpenSSL 3.x to avoid usage of legacy RC2 algorithm
  // as the initial constant did - so that TCryptCertOpenSsl.Load() can succeed
  // - Save() used '3des=pass' password to force SHA1-3DES p12Legacy format
  PRIVKEY_PFX: array[0..2400] of byte = (
    $30, $82, $09, $5d, $02, $01, $03, $30, $82, $09, $27, $06, $09, $2a, $86, $48,
    $86, $f7, $0d, $01, $07, $01, $a0, $82, $09, $18, $04, $82, $09, $14, $30, $82,
    $09, $10, $30, $82, $03, $c7, $06, $09, $2a, $86, $48, $86, $f7, $0d, $01, $07,
    $06, $a0, $82, $03, $b8, $30, $82, $03, $b4, $02, $01, $00, $30, $82, $03, $ad,
    $06, $09, $2a, $86, $48, $86, $f7, $0d, $01, $07, $01, $30, $1c, $06, $0a, $2a,
    $86, $48, $86, $f7, $0d, $01, $0c, $01, $03, $30, $0e, $04, $08, $bc, $54, $f1,
    $25, $8b, $4d, $58, $6e, $02, $02, $08, $00, $80, $82, $03, $80, $6a, $e7, $2c,
    $98, $af, $16, $5b, $3f, $63, $9e, $9b, $cb, $7b, $4f, $0f, $16, $e7, $bd, $bb,
    $03, $40, $da, $a2, $51, $48, $97, $2e, $cb, $28, $69, $9b, $cd, $12, $01, $2d,
    $e7, $ca, $a9, $57, $58, $68, $77, $e2, $e1, $94, $0f, $dc, $2a, $e9, $06, $dc,
    $51, $7b, $91, $f4, $54, $00, $6a, $d6, $d6, $18, $9e, $f4, $8b, $30, $d2, $32,
    $e2, $93, $b4, $63, $7b, $39, $3f, $0a, $17, $ae, $52, $87, $69, $a3, $30, $c8,
    $75, $13, $81, $9e, $53, $74, $38, $4e, $3d, $5c, $b1, $a4, $fc, $14, $81, $0b,
    $e4, $0b, $58, $3a, $95, $36, $57, $e8, $65, $e5, $b4, $d8, $55, $da, $f8, $6c,
    $7c, $09, $36, $70, $40, $9e, $99, $0f, $39, $67, $6d, $1a, $24, $7c, $02, $a6,
    $ee, $23, $5c, $69, $29, $36, $77, $ac, $fe, $e1, $b2, $af, $11, $2e, $46, $d1,
    $6b, $70, $e4, $01, $31, $0b, $72, $42, $55, $b7, $70, $4c, $aa, $d5, $df, $f4,
    $26, $b7, $67, $52, $45, $93, $26, $e3, $b1, $40, $76, $c8, $2f, $f5, $07, $8a,
    $1e, $56, $90, $ab, $1d, $5d, $75, $4c, $b2, $2d, $5c, $5d, $7d, $bc, $ec, $bc,
    $d6, $3c, $74, $7b, $19, $31, $5e, $fa, $dd, $04, $a7, $bf, $97, $93, $b9, $48,
    $59, $3a, $4f, $22, $c9, $4e, $a3, $e6, $4d, $04, $01, $83, $fd, $53, $f4, $00,
    $2f, $c2, $65, $fe, $13, $95, $e2, $68, $a9, $39, $14, $39, $0d, $6c, $fa, $04,
    $96, $0f, $44, $a2, $79, $6b, $e3, $b3, $e0, $5f, $e6, $31, $55, $56, $b6, $6b,
    $8b, $b8, $e1, $8d, $78, $cb, $dc, $da, $f3, $2a, $18, $30, $d0, $41, $07, $48,
    $2d, $d9, $1d, $b8, $dc, $e1, $54, $71, $29, $60, $48, $ee, $50, $41, $0c, $85,
    $47, $47, $6c, $db, $0b, $7e, $30, $90, $49, $9e, $c2, $30, $58, $5d, $1e, $69,
    $d6, $2e, $6f, $2d, $da, $48, $20, $37, $e4, $10, $e4, $58, $42, $b7, $20, $f7,
    $c7, $dd, $2a, $50, $c2, $a8, $ed, $cc, $df, $87, $9e, $05, $f6, $d3, $6f, $c7,
    $4c, $68, $d3, $cd, $71, $23, $21, $93, $f5, $70, $88, $9c, $aa, $a6, $01, $00,
    $24, $39, $0d, $d4, $c8, $0d, $a1, $4c, $c1, $54, $39, $32, $1f, $32, $68, $15,
    $77, $95, $98, $33, $36, $2b, $4c, $1a, $9f, $86, $2e, $e5, $87, $22, $9e, $54,
    $b4, $ba, $11, $5b, $31, $6c, $9b, $77, $2f, $c6, $93, $09, $2d, $d9, $a4, $17,
    $3b, $bb, $15, $5a, $b6, $4a, $70, $f0, $02, $9d, $79, $8a, $ad, $e6, $f5, $b6,
    $5a, $62, $74, $25, $11, $d1, $e7, $6c, $bb, $ae, $66, $bc, $47, $8a, $68, $e2,
    $81, $4c, $2b, $a6, $f6, $71, $2f, $38, $5a, $eb, $26, $1b, $0f, $c3, $2c, $7f,
    $9e, $09, $ad, $ce, $16, $61, $75, $b1, $69, $03, $a3, $a1, $73, $dc, $d8, $48,
    $62, $16, $ed, $4b, $74, $80, $31, $4d, $61, $26, $24, $40, $04, $65, $31, $9c,
    $77, $6e, $40, $a7, $64, $55, $87, $01, $64, $7c, $e0, $f1, $18, $fe, $ef, $a9,
    $b4, $9d, $cf, $e3, $13, $66, $38, $b0, $4b, $12, $1e, $3e, $e2, $17, $fa, $75,
    $03, $ca, $cb, $4e, $25, $90, $16, $fd, $ad, $c1, $f1, $82, $b6, $95, $36, $33,
    $49, $fa, $91, $a1, $3e, $4e, $07, $10, $d0, $86, $ab, $8a, $a7, $15, $b1, $9c,
    $aa, $ec, $40, $f0, $d0, $0d, $b4, $bd, $cf, $2b, $e3, $c6, $eb, $6a, $cb, $6b,
    $a1, $53, $74, $55, $12, $a3, $8c, $bc, $d7, $69, $23, $aa, $ed, $04, $cf, $5b,
    $84, $26, $fc, $80, $5a, $61, $46, $6f, $f9, $3e, $0a, $33, $77, $ea, $27, $59,
    $ed, $bb, $2b, $fb, $2d, $f6, $b2, $e8, $d9, $7b, $88, $53, $21, $0c, $c3, $5e,
    $96, $44, $32, $f6, $5e, $2b, $c6, $09, $b5, $10, $12, $93, $f8, $6a, $0e, $6d,
    $7b, $04, $44, $5f, $ff, $01, $a7, $bd, $21, $56, $31, $1c, $24, $56, $3a, $f3,
    $ad, $4a, $da, $27, $f0, $d2, $a3, $00, $8c, $aa, $cc, $66, $7e, $c3, $45, $cb,
    $3b, $57, $7d, $93, $2b, $f3, $e1, $1f, $26, $df, $88, $35, $5e, $49, $3a, $fc,
    $84, $b0, $7c, $46, $82, $d6, $0a, $ca, $50, $21, $fd, $69, $3c, $70, $bf, $0b,
    $07, $86, $62, $39, $d7, $33, $aa, $2c, $89, $f0, $a1, $db, $40, $8a, $04, $ac,
    $6f, $ef, $e9, $75, $44, $6c, $46, $cc, $00, $3f, $a2, $5f, $ff, $89, $5d, $b3,
    $35, $1c, $37, $c9, $52, $ab, $36, $b8, $9f, $25, $a1, $e8, $d8, $fa, $e2, $4d,
    $ce, $fa, $c5, $99, $46, $4d, $f8, $87, $be, $b5, $c2, $c3, $80, $b0, $78, $39,
    $f9, $72, $5b, $db, $98, $61, $4b, $74, $3f, $62, $b5, $f5, $bb, $17, $26, $17,
    $e4, $6d, $3d, $09, $ac, $3e, $95, $7a, $ee, $aa, $75, $0f, $50, $b5, $af, $af,
    $e9, $45, $3c, $52, $2e, $bb, $72, $a3, $a1, $40, $bd, $2e, $3a, $27, $a2, $43,
    $ad, $26, $82, $e5, $ae, $38, $fd, $8e, $b6, $65, $fe, $0f, $ab, $6b, $fa, $81,
    $e7, $9b, $d8, $69, $b2, $0e, $a8, $e0, $50, $1c, $19, $b6, $7a, $dd, $8c, $b8,
    $a4, $52, $21, $46, $0c, $db, $c3, $9e, $13, $b1, $08, $e0, $25, $fd, $fc, $48,
    $04, $61, $f6, $bc, $f1, $bd, $cb, $51, $b8, $70, $43, $38, $c7, $d9, $af, $3b,
    $74, $8c, $b0, $5b, $a5, $95, $0f, $a3, $c8, $6d, $2e, $73, $4f, $51, $a2, $4e,
    $ee, $07, $fe, $4c, $95, $8b, $4a, $8c, $52, $ba, $eb, $62, $d2, $30, $82, $05,
    $41, $06, $09, $2a, $86, $48, $86, $f7, $0d, $01, $07, $01, $a0, $82, $05, $32,
    $04, $82, $05, $2e, $30, $82, $05, $2a, $30, $82, $05, $26, $06, $0b, $2a, $86,
    $48, $86, $f7, $0d, $01, $0c, $0a, $01, $02, $a0, $82, $04, $ee, $30, $82, $04,
    $ea, $30, $1c, $06, $0a, $2a, $86, $48, $86, $f7, $0d, $01, $0c, $01, $03, $30,
    $0e, $04, $08, $ad, $0d, $c3, $b1, $f1, $40, $45, $42, $02, $02, $08, $00, $04,
    $82, $04, $c8, $38, $b0, $70, $9a, $57, $38, $93, $5c, $85, $61, $30, $6f, $5a,
    $de, $d8, $26, $ac, $8b, $aa, $a1, $2e, $85, $8a, $07, $8d, $2b, $a6, $16, $e2,
    $0d, $8b, $eb, $49, $b9, $30, $f0, $99, $0a, $83, $48, $d0, $1f, $93, $34, $a9,
    $e9, $6f, $3d, $1b, $25, $c5, $51, $73, $aa, $65, $36, $29, $e1, $54, $34, $c7,
    $bd, $0b, $94, $e7, $6b, $b1, $38, $b3, $4d, $ae, $7d, $49, $07, $ad, $93, $73,
    $66, $a2, $00, $e9, $02, $3c, $71, $af, $fe, $fb, $5c, $42, $7f, $62, $1a, $8a,
    $19, $d1, $12, $4d, $d6, $a0, $76, $9f, $2d, $5f, $29, $ea, $47, $83, $4a, $98,
    $7c, $e6, $54, $ae, $3d, $ae, $22, $9d, $04, $af, $31, $32, $5d, $3b, $9b, $ef,
    $26, $55, $7b, $16, $54, $8f, $0b, $7d, $33, $f5, $43, $cf, $4d, $08, $54, $f5,
    $89, $d3, $bf, $b6, $f3, $18, $ad, $7b, $cf, $b3, $15, $8f, $49, $94, $2d, $30,
    $8d, $43, $e7, $60, $23, $d8, $dc, $c4, $8e, $bf, $cc, $a2, $03, $fd, $7a, $7e,
    $96, $25, $e2, $fb, $7c, $56, $5d, $b1, $0f, $99, $36, $aa, $0f, $49, $a4, $79,
    $42, $f4, $c7, $81, $29, $59, $cd, $9f, $c5, $2d, $ae, $e2, $5b, $1c, $fb, $bf,
    $ef, $02, $ef, $4a, $5c, $20, $0b, $27, $94, $99, $d3, $0b, $0b, $b9, $d4, $7c,
    $b8, $19, $a7, $70, $6d, $61, $b0, $78, $01, $9d, $f9, $fb, $13, $08, $07, $17,
    $58, $dc, $1f, $8a, $ba, $a7, $80, $59, $de, $27, $90, $90, $5a, $c0, $82, $98,
    $27, $4f, $73, $eb, $e9, $ea, $c9, $9b, $7b, $e8, $05, $e1, $ef, $40, $0a, $d3,
    $2c, $dd, $09, $f0, $eb, $a5, $c8, $66, $ac, $4b, $66, $e3, $1c, $19, $ad, $99,
    $99, $9d, $87, $0e, $80, $ec, $70, $8a, $5c, $c6, $28, $01, $be, $f7, $ac, $34,
    $7e, $90, $3b, $fc, $3b, $e0, $66, $72, $bc, $bf, $bf, $b9, $7a, $16, $e7, $5b,
    $67, $83, $74, $89, $5f, $4e, $b1, $bb, $51, $69, $75, $00, $36, $52, $93, $e3,
    $53, $1e, $de, $cb, $a3, $6b, $19, $fd, $67, $29, $63, $48, $e9, $50, $22, $de,
    $3a, $a0, $bb, $f2, $4f, $59, $01, $5d, $71, $f0, $64, $05, $6b, $4a, $de, $0f,
    $77, $77, $e2, $5d, $18, $ad, $ac, $95, $b8, $b9, $f5, $fa, $c5, $f8, $e1, $ce,
    $f5, $ba, $93, $0c, $fa, $e1, $7d, $fc, $ea, $b7, $0b, $dd, $91, $22, $55, $c3,
    $95, $9a, $3e, $eb, $5a, $aa, $56, $e6, $96, $22, $7b, $9d, $0a, $fa, $28, $bf,
    $26, $6e, $fa, $5a, $b2, $9a, $30, $34, $6b, $a9, $79, $79, $a6, $da, $ce, $c5,
    $f2, $5f, $51, $ce, $fc, $51, $22, $e6, $b0, $db, $a3, $c7, $95, $22, $7d, $0d,
    $73, $fd, $bc, $e4, $56, $69, $5f, $f3, $89, $3b, $aa, $6b, $c0, $bf, $be, $3d,
    $f6, $be, $24, $df, $3a, $fb, $ee, $40, $10, $2e, $e9, $df, $da, $2d, $2a, $6a,
    $d0, $51, $9e, $9e, $81, $48, $27, $87, $56, $4d, $af, $21, $e7, $b3, $e0, $26,
    $ed, $9e, $fd, $fa, $19, $50, $7d, $14, $58, $e6, $af, $3f, $62, $7f, $68, $48,
    $00, $4e, $cc, $10, $28, $e5, $60, $02, $ec, $fc, $43, $cc, $d2, $9b, $b4, $e9,
    $d6, $05, $4d, $dc, $a8, $b5, $e9, $44, $5b, $04, $a7, $c0, $90, $4e, $b4, $08,
    $d7, $4e, $24, $0f, $5d, $5e, $b9, $ae, $9d, $81, $4d, $d3, $8d, $11, $22, $f8,
    $b4, $b1, $3f, $fb, $32, $83, $0f, $73, $ac, $af, $33, $52, $f7, $5d, $36, $a3,
    $a8, $91, $49, $9e, $e1, $07, $65, $f0, $42, $be, $06, $f3, $9c, $2f, $41, $5c,
    $1c, $33, $cd, $bb, $4f, $df, $c0, $4b, $1f, $c8, $9f, $dd, $f8, $65, $48, $c6,
    $63, $6c, $79, $d8, $5a, $5d, $36, $45, $44, $f2, $6d, $6d, $49, $f2, $48, $ed,
    $79, $0c, $a4, $56, $0d, $7e, $fa, $10, $2a, $c2, $2b, $02, $9c, $f4, $3d, $04,
    $6c, $62, $26, $0a, $37, $1f, $d1, $51, $cd, $e5, $db, $f5, $d6, $e7, $51, $29,
    $9b, $6a, $19, $26, $34, $62, $0d, $a4, $4a, $cd, $4e, $be, $c7, $47, $90, $2d,
    $bc, $4b, $b3, $c4, $ff, $28, $5d, $96, $84, $e1, $dd, $8f, $38, $f6, $fb, $cf,
    $86, $0e, $50, $2d, $da, $dc, $06, $8d, $2d, $c4, $90, $58, $bb, $36, $ff, $f6,
    $38, $05, $bf, $b2, $eb, $27, $83, $46, $c2, $aa, $02, $d3, $b8, $ca, $94, $c8,
    $0f, $9a, $46, $fb, $8b, $56, $92, $e3, $3b, $e8, $35, $60, $b9, $57, $31, $81,
    $3a, $44, $6b, $f5, $ed, $9c, $f1, $b6, $39, $eb, $29, $39, $28, $ab, $1c, $f3,
    $cb, $9e, $bd, $86, $2b, $b9, $6b, $c7, $23, $fd, $51, $6a, $5a, $2e, $df, $74,
    $e1, $1c, $6e, $7f, $76, $c8, $17, $24, $b8, $5a, $d7, $59, $23, $bd, $ff, $70,
    $0f, $71, $b2, $46, $4f, $12, $8a, $f2, $22, $a5, $e9, $5b, $3d, $a9, $3a, $d5,
    $36, $52, $c7, $fd, $8b, $fd, $35, $20, $d2, $f6, $c6, $34, $d2, $6f, $de, $ef,
    $fc, $cf, $fc, $6e, $c7, $30, $6c, $cd, $f6, $62, $cb, $7d, $8d, $b8, $67, $c1,
    $cf, $24, $f4, $bb, $7c, $5d, $89, $50, $51, $4c, $12, $85, $38, $98, $3d, $67,
    $9c, $ec, $a5, $af, $7c, $b8, $89, $4d, $54, $98, $02, $29, $78, $83, $d0, $a4,
    $13, $30, $36, $d6, $ca, $9a, $85, $fd, $89, $83, $81, $1e, $52, $ba, $1b, $60,
    $cb, $46, $16, $de, $ea, $41, $50, $67, $c9, $16, $7a, $d9, $03, $35, $59, $cd,
    $5e, $94, $3a, $6b, $c2, $52, $92, $7d, $c3, $61, $a3, $6d, $aa, $75, $79, $ea,
    $d1, $54, $0e, $15, $dc, $de, $32, $11, $de, $d6, $23, $31, $57, $3e, $e1, $d3,
    $1c, $94, $75, $30, $ff, $a5, $5d, $2f, $1a, $77, $6d, $ce, $48, $fd, $bc, $bc,
    $65, $5a, $13, $41, $0e, $2f, $58, $2f, $41, $d0, $e5, $2d, $20, $84, $32, $69,
    $75, $ed, $0f, $4c, $dc, $10, $00, $35, $1f, $dc, $b4, $ba, $6f, $c4, $00, $97,
    $74, $5c, $d1, $51, $40, $3f, $11, $fe, $a0, $e0, $2f, $c6, $ea, $14, $26, $72,
    $d5, $70, $42, $1a, $2b, $dd, $fe, $64, $54, $99, $63, $1d, $0e, $86, $e4, $a9,
    $04, $b9, $90, $2d, $88, $9f, $cc, $68, $f5, $f6, $33, $f3, $0b, $4d, $b2, $bb,
    $41, $e5, $82, $63, $85, $79, $d6, $15, $cf, $e4, $23, $18, $1f, $35, $5c, $98,
    $83, $61, $0d, $8e, $fd, $7f, $e5, $83, $6d, $c7, $4c, $dc, $51, $b6, $d9, $09,
    $30, $a5, $9b, $15, $00, $94, $b3, $98, $5b, $40, $4e, $40, $de, $da, $11, $8a,
    $16, $64, $ca, $17, $6b, $ff, $02, $71, $86, $64, $92, $b8, $33, $22, $e5, $82,
    $6e, $49, $56, $77, $d6, $7e, $83, $ba, $a8, $f6, $6a, $0c, $56, $0c, $b4, $2c,
    $3d, $20, $b1, $fb, $5b, $db, $06, $24, $bf, $48, $e7, $ae, $89, $c7, $42, $5a,
    $4c, $b6, $7f, $cd, $51, $dd, $bc, $4d, $b1, $21, $95, $f1, $32, $12, $05, $90,
    $a4, $1c, $13, $12, $bc, $c7, $7a, $f6, $36, $ea, $a9, $f4, $1a, $a8, $a1, $56,
    $9a, $5a, $17, $84, $0d, $ff, $0f, $30, $7b, $9a, $23, $1c, $1c, $bf, $4c, $81,
    $51, $7b, $72, $cd, $43, $ea, $c6, $f5, $bb, $7f, $f5, $97, $d3, $87, $7b, $c3,
    $03, $73, $c3, $d7, $12, $d2, $f1, $7a, $e1, $74, $92, $bc, $5e, $7b, $fb, $f2,
    $37, $9d, $fc, $2a, $2b, $19, $3d, $cd, $38, $48, $e6, $69, $79, $ec, $d7, $00,
    $e8, $91, $62, $2a, $13, $0a, $66, $d4, $7c, $f4, $72, $31, $25, $30, $23, $06,
    $09, $2a, $86, $48, $86, $f7, $0d, $01, $09, $15, $31, $16, $04, $14, $b4, $df,
    $63, $02, $4c, $1e, $34, $b3, $26, $13, $6e, $20, $ce, $7c, $cb, $b0, $eb, $93,
    $b6, $08, $30, $2d, $30, $21, $30, $09, $06, $05, $2b, $0e, $03, $02, $1a, $05,
    $00, $04, $14, $c3, $de, $e4, $37, $ed, $29, $0f, $50, $48, $18, $ca, $d9, $69,
    $b5, $14, $06, $58, $a3, $e4, $8b, $04, $08, $48, $05, $11, $9c, $39, $f2, $02,
    $34);

function PrivKeyCertPfx: RawByteString;
begin
  FastSetRawByteString(result, @PRIVKEY_PFX, SizeOf(PRIVKEY_PFX));
end;

var
  SelfSignedCert: array[TCryptAsymAlgo] of ICryptCert; // one generated per algo

procedure InitNetTlsContextSelfSignedServer(var TLS: TNetTlsContext;
  Algo: TCryptAsymAlgo; UsePreComputed: boolean);
begin
  InitNetTlsContext(TLS);
  TLS.IgnoreCertificateErrors := true; // needed if no mutual auth is done
  if UsePrecomputed or
     (CryptCertOpenSsl[Algo] = nil) then // pure SChannel will use embedded PFX
  // can't use CryptCertX509[] because SChannel/SSPI requires PFX binary format
  begin
    TLS.CertificateBin := PrivKeyCertPfx; // use pre-computed key
    TLS.PrivatePassword := 'pass';
    exit;
  end;
  // generate a reusable per-algo ICryptCert instance (RSA-2048 can take time)
  if SelfSignedCert[Algo] = nil then
    SelfSignedCert[Algo] := CryptCertOpenSsl[Algo].Generate(
      CU_TLS_SERVER, '127.0.0.1', nil, 3650);
  //writeln(BinToSource('PRIVKEY_PFX', '', // force SHA1-3DES p12Legacy format
  //  SelfSignedCert[Algo].Save(cccCertWithPrivateKey, '3des=pass', ccfBinary)));
  // no temporary file needed: we just provide the shared OpenSSL handles
  TLS.CertificateRaw := SelfSignedCert[Algo].Handle;           // PX509
  TLS.PrivateKeyRaw  := SelfSignedCert[Algo].PrivateKeyHandle; // PEVP_PKEY
end;

const
  // RLE-encoded /favicon.ico, as decoded into FavIconBinary function result
  // - using Base64 encoding is the easiest with Delphi and RawByteString :)
  _FAVICON_BINARY: RawUtf8 =
    'aQOi9AjOyJ+H/gMAAAEAAQAYGBAAAQAEAOgBAAAWAAAAKAAAABgAAAAwAAAAAQAEWhEAEFoH' +
    'AAEC7wAFBQgAVVVVAAMDwwCMjIwA////AG1tcQCjo6sACQmbADU1NgAAACsACAhPAMvLywAA' +
    'AHEADy34AABu/QBaEFVXYiJnWgdVUmd8zHdmWgVVVmRCERESRGRaBFUiYVoEERlmZVVVUiIR' +
    'ERqqERESJlVVdiERq93d26ERIsVVZBEa2DMziNoRFiVUdhGtgzAAM42hFHzCQRG4MAAAADix' +
    'EUJCYRrTWgQAM9oRYiIhG4MAAOAAA4oRIpKRG4MAD/4AA4oRIpKRG4MADv4AA4oRIiIhGoMA' +
    'AAAOA9oRKUlhStMwAAAAONERKVJhmbgzDuADOLF5ZlxEERuDMzMzixmUZVVEkRG9Z3eNsREk' +
    'RVVWQRGWu7u2kRlGVVVcJJGUzMzEESQlVVVVwndaBBGXcsVaBFVnd3REd3RlWgZVR3zMdEVV' +
    'VVX///8A/4D/AP4APwD4AA8A8AAHAOAAAwDAAAEAwAABAIBaHwCAAAAAgAABAMAAAQDgAAMA' +
    '4AAHAPAABwD8AB8A/wB/AA==';

var
  _FavIconBinary: RawByteString;

function FavIconBinary: RawByteString;
begin
  if _FavIconBinary = '' then
    _FavIconBinary := AlgoRle.Decompress(Base64ToBin(_FAVICON_BINARY));
  result := _FavIconBinary;
end;

type
  TSortByMacAddress = class // a fake class to propagate TMacAddressFilter
    function Compare(const A, B): integer;
  end;

const
  NETHW_ORDER: array[TMacAddressKind] of byte = ( // Kind to sort priority
    2,  // makUndefined
    0,  // makEthernet
    1,  // makWifi
    4,  // makTunnel
    3,  // makPpp
    5,  // makCellular
    6); // makSoftware

function TSortByMacAddress.Compare(const A, B): integer;
var
  ma: TMacAddress absolute A;
  mb: TMacAddress absolute B;
  filter: TMacAddressFilter;
begin
  result := 0;
  if @ma = @mb then
    exit;
  // was called as arr.Sort(TSortByMacAddress(PtrUInt(byte(Filter))).Compare)
  byte(filter) := PtrInt(self);
  // sort with gateway first
  if not (mafIgnoreGateway in filter) then
  begin
    result := ord(ma.Gateway = '') - ord(mb.Gateway = '');
    if result <> 0 then
      exit;
  end;
  // sort by kind
  if not (mafIgnoreKind in filter) then
  begin
    result := CompareCardinal(NETHW_ORDER[ma.Kind], NETHW_ORDER[mb.Kind]);
    if result <> 0 then
      exit;
  end;
  // sort by speed within this kind and gateway
  if not (mafIgnoreSpeed in filter) then
  begin
    result := CompareCardinal(mb.Speed, ma.Speed);
    if result <> 0 then
      exit;
  end;
  // fallback to sort by IfIndex or plain MAC address
  result := CompareCardinal(ma.IfIndex, mb.IfIndex);
  if result = 0 then
    result := SortDynArrayAnsiStringI(ma.Address, mb.Address);
  if result = 0 then
    result := ComparePointer(@ma, @mb);
end;

function GetMainMacAddress(out Mac: TMacAddress; Filter: TMacAddressFilter): boolean;
var
  allowed, available: TMacAddressKinds;
  all: TMacAddressDynArray;
  arr: TDynArray;
  i, bct: PtrInt;
begin
  result := false;
  all := copy(GetMacAddresses({upanddown=}false)); // using a 65-seconds cache
  if all = nil then
    exit;
  arr.Init(TypeInfo(TMacAddressDynArray), all);
  bct := 0;
  available := [];
  for i := 0 to high(all) do
    with all[i] do
    begin
      include(available, Kind);
      if Broadcast <> '' then
        inc(bct);
      {writeln(Kind, ' ', Address,' name=',Name,' ifindex=',IfIndex,
         ' ip=',ip,' netmask=',netmask,' broadcast=',broadcast);}
    end;
  allowed := [];
  if mafLocalOnly in Filter then
    allowed := [makEthernet, makWifi]
  else if mafEthernetOnly in Filter then
    include(allowed, makEthernet);
  if (available * allowed) <> [] then // e.g. if all makUndefined
    for i := high(all) downto 0 do
      if not (all[i].Kind in allowed) then
        arr.Delete(i);
  if (mafRequireBroadcast in Filter) and
     (bct <> 0) then
    for i := high(all) downto 0 do
      if all[i].Broadcast = '' then
        arr.Delete(i);
  if all = nil then
    exit;
  if length(all) > 1 then
    arr.Sort(TSortByMacAddress(PtrUInt(byte(Filter))).Compare);
  Mac := all[0];
  result := true;
end;

function GetMainMacAddress(out Mac: TMacAddress;
  const InterfaceNameAddressOrIP: RawUtf8; UpAndDown: boolean): boolean;
var
  n: integer;
  all: TMacAddressDynArray;
  mask: TIp4SubNet;
  m, fnd: ^TMacAddress;
begin
  // retrieve the current network interfaces
  result := false;
  if InterfaceNameAddressOrIP = '' then
    exit;
  all := GetMacAddresses(UpAndDown); // from cache
  n := length(all);
  if n = 0 then
    exit;
  m := pointer(all);
  fnd := nil;
  if mask.From(InterfaceNameAddressOrIP) then
    // search as IP bitmask pattern e.g. '192.168.1.0/24' or '192.168.1.13'
    repeat
      if mask.Match(m^.IP) then // e.g. 192.168.1.2 against '192.168.1.0/24'
        if (fnd = nil) or
           (NETHW_ORDER[m^.Kind] < NETHW_ORDER[fnd^.Kind]) then
          fnd := m; // pickup the interface with the best hardware (paranoid)
      inc(m);
      dec(n);
    until n = 0
  else
    // search for interface Name or MAC Address
    repeat
      if IdemPropNameU(m^.Name,    InterfaceNameAddressOrIP) or
         IdemPropNameU(m^.Address, InterfaceNameAddressOrIP) then
      begin
        fnd := m;
        break;
      end;
      inc(m);
      dec(n);
    until n = 0;
  if fnd = nil then
    exit;
  Mac := fnd^;
  result := true;
end;


{ ******************** THttpServerSocket/THttpServer HTTP/1.1 Server }

{ THttpServerSocketGeneric }

constructor THttpServerSocketGeneric.Create(const aPort: RawUtf8;
  const OnStart, OnStop: TOnNotifyThread; const ProcessName: RawUtf8;
  ServerThreadPoolCount: integer; KeepAliveTimeOut: integer;
  ProcessOptions: THttpServerOptions; aLog: TSynLogClass);
begin
  fSockPort := aPort;
  fCompressGz := -1;
  fBlackListUriReloadMin := MinsPerDay; // 1440 minutes = daily by default
  SetServerKeepAliveTimeOut(KeepAliveTimeOut); // 30 seconds by default
  // event handlers set before inherited Create to be visible in childs
  fOnThreadStart := OnStart;
  SetOnTerminate(OnStop);
  fProcessName := ProcessName; // TSynThreadPoolTHttpServer needs it now
  inherited Create(OnStart, OnStop, ProcessName, ProcessOptions, aLog);
end;

destructor THttpServerSocketGeneric.Destroy;
begin
  inherited Destroy;
  {$ifdef OSPOSIX}
  FreeAndNil(fSspiKeyTab);
  {$endif OSPOSIX}
end;

function THttpServerSocketGeneric.GetApiVersion: RawUtf8;
begin
  result := SocketApiVersion;
end;

function THttpServerSocketGeneric.GetRegisterCompressGzStatic: boolean;
begin
  result := fCompressGz >= 0;
end;

procedure THttpServerSocketGeneric.SetRegisterCompressGzStatic(Value: boolean);
begin
  if Value then
    fCompressGz := fCompressList.CompressIndex('gzip')
  else
    fCompressGz := -1;
end;

function THttpServerSocketGeneric.{%H-}WebSocketsEnable(const aWebSocketsURI,
  aWebSocketsEncryptionKey: RawUtf8; aWebSocketsAjax: boolean;
  aWebSocketsBinaryOptions: TWebSocketProtocolBinaryOptions): pointer;
begin
  raise EHttpServer.CreateUtf8('Unexpected %.WebSocketEnable: requires ' +
    'HTTP_BIDIR (useBidirSocket or useBidirAsync) kind of server', [self]);
end;

procedure THttpServerSocketGeneric.WaitStarted(Seconds: integer;
  const CertificateFile, PrivateKeyFile: TFileName;
  const PrivateKeyPassword: RawUtf8; const CACertificatesFile: TFileName);
var
  tls: TNetTlsContext;
begin
  InitNetTlsContext(tls, {server=}true,
    CertificateFile, PrivateKeyFile, PrivateKeyPassword, CACertificatesFile);
  WaitStarted(Seconds, @tls);
end;

procedure THttpServerSocketGeneric.WaitStarted(
  Seconds: integer; TLS: PNetTlsContext);
var
  tix32: cardinal;
begin
  tix32 := GetTickSec + cardinal(Seconds);
  repeat
    if Terminated then
      exit;
    case GetExecuteState of
      esRunning:
        break;
      esFinished:
        EHttpServer.RaiseUtf8('%.Execute aborted due to %',
          [self, fExecuteMessage]);
    end;
    SleepHiRes(1); // warning: waits typically 1-15 ms on Windows
    if GetTickSec > tix32 then
      EHttpServer.RaiseUtf8('%.WaitStarted timeout % after % seconds [%]',
        [self, ToText(GetExecuteState)^, Seconds, fExecuteMessage]);
  until false;
  // now the server socket has been bound, and is ready to accept connections
  if (hsoEnableTls in fOptions) and
     (TLS <> nil) and
     ((TLS^.CertificateFile <> '') or
      (TLS^.CertificateRaw <> nil) or
      (TLS^.CertificateBin <> '')) and
     ((fSock = nil) or
      not fSock.TLS.Enabled) then
  begin
    fLogClass.Add.Log(sllTrace, 'WaitStarted TLS setup % %',
      [TLS^.CertificateFile, TLS^.CACertificatesFile], self);
    while (fSock = nil) and
          (GetTickSec <= tix32) do
      SleepHiRes(5); // paranoid on some servers which propagate the pointer
    if (fSock <> nil) and
       not fSock.TLS.Enabled then // call InitializeTlsAfterBind once
    begin
      fSock.TLS := TLS^;
      InitializeTlsAfterBind; // validate TLS certificate(s) now
      SleepHiRes(1); // let some warmup happen
    end;
  end;
  fLogClass.Add.Log(sllTrace, 'WaitStarted done', self);
end;

procedure THttpServerSocketGeneric.WaitStartedHttps(Seconds: integer;
  UsePreComputed: boolean);
var
  net: TNetTlsContext;
begin
  InitNetTlsContextSelfSignedServer(net, caaRS256, UsePreComputed);
  WaitStarted(Seconds, @net);
end;

function THttpServerSocketGeneric.GetStat(
  one: THttpServerSocketGetRequestResult): integer;
begin
  result := fStats[one];
end;

procedure THttpServerSocketGeneric.IncStat(
  one: THttpServerSocketGetRequestResult);
begin
  if not (hsoNoStats in fOptions) then
    LockedInc32(@fStats[one]);
end;

function THttpServerSocketGeneric.HeaderRetrieveAbortTix: Int64;
begin
  result := fHeaderRetrieveAbortDelay;
  if result <> 0 then
    inc(result, mormot.core.os.GetTickCount64()); // FPC requires () on Windows
end;

function THttpServerSocketGeneric.DoRequest(Ctxt: THttpServerRequest): boolean;
var
  cod: integer;
begin
  result := false; // error
  try
    // first try any URI rewrite or direct callback execution
    if fRoute <> nil then
    begin
      cod := fRoute.Process(Ctxt);
      if cod <> 0 then
      begin
        if (Ctxt.OutContent = '') and
           (cod <> HTTP_ASYNCRESPONSE) and
           not StatusCodeIsSuccess(cod) then
        begin
          Ctxt.fErrorMessage := 'Wrong route';
          IncStat(grRejected);
        end;
        Ctxt.RespStatus := cod;
        result := true; // a callback was executed
        exit;
      end;
    end;
    // fallback to Request() / OnRequest main processing callback
    cod := DoBeforeRequest(Ctxt);
    if cod <> 0 then
    begin
      Ctxt.RespStatus := cod;
      if Ctxt.OutContent = '' then
        Ctxt.fErrorMessage := 'Rejected request';
      IncStat(grRejected);
    end
    else
    begin
      Ctxt.RespStatus := Request(Ctxt); // calls OnRequest event handler
      if Ctxt.InContent <> '' then
        Ctxt.InContent := ''; // release memory ASAP
      cod := DoAfterRequest(Ctxt);
      if cod > 0 then
        Ctxt.RespStatus := cod;
    end;
    result := true; // success
  except
    on E: Exception do
    begin
      // intercept and return Internal Server Error 500 on any fatal exception
      Ctxt.RespStatus := HTTP_SERVERERROR;
      Ctxt.SetErrorMessage('%: %', [E, E.Message]);
      IncStat(grException);
      // will keep soClose as result to shutdown the connection
    end;
  end;
end;

function THttpServerSocketGeneric.DoProcessBody(var Ctxt: THttpRequestContext;
  var Dest: TRawByteStringBuffer; MaxSize: PtrInt): THttpRequestProcessBody;
begin
  // called when up to MaxSize (128/256KB typical) bytes could be sent
  result := hrpDone;
  if Ctxt.ContentLength = 0 then
    // we just finished background ProcessWrite of the last chunk
    Ctxt.State := hrsResponseDone
  else if Ctxt.State = hrsSendBody then
    if rfProgressiveStatic in Ctxt.ResponseFlags then
      // support progressive/partial body process
      result := fProgressiveRequests.ProcessBody(Ctxt, Dest, MaxSize)
    else
      // send in the background from ContentStream / fContentPos
      result := Ctxt.ProcessBody(Dest, MaxSize);
end;

procedure THttpServerSocketGeneric.DoProgressiveRequestFree(
  var Ctxt: THttpRequestContext);
begin
  if (fProgressiveRequests = nil) or
     not (rfProgressiveStatic in Ctxt.ResponseFlags) then
    exit;
  try
    fProgressiveRequests.Remove(@Ctxt);
  except
    ; // ignore any exception in callbacks
  end;
  exclude(Ctxt.ResponseFlags, rfProgressiveStatic); // remove it once
end;

procedure THttpServerSocketGeneric.SetServerKeepAliveTimeOut(Value: cardinal);
begin
  fServerKeepAliveTimeOut := Value; // in ms
  if Value = 0 then
    fServerKeepAliveTimeOutSec := 0 // 0 means no keep-alive
  else if Value <= 1999 then
    fServerKeepAliveTimeOutSec := 1 // minimum 1 second
  else
    fServerKeepAliveTimeOutSec := Value div 1000;
end;

function THttpServerSocketGeneric.OnNginxAllowSend(
  Context: THttpServerRequestAbstract; const LocalFileName: TFileName): boolean;
var
  match, i, f: PtrInt;
  folderlefttrim: ^TFileName;
begin
  match := 0;
  folderlefttrim := pointer(fNginxSendFileFrom);
  if LocalFileName <> '' then
    for f := 1 to length(fNginxSendFileFrom) do
    begin
      match := length(folderlefttrim^);
      for i := 1 to match do // case sensitive left search
        if LocalFileName[i] <> folderlefttrim^[i] then
        begin
          match := 0;
          break;
        end;
      if match <> 0 then
        break; // found matching folderlefttrim
      inc(folderlefttrim);
    end;
  result := match <> 0;
  if not result then
    exit; // no match -> manual send
  Context.AddOutHeader(['X-Accel-Redirect: ',
    copy(Context.OutContent, match + 1, 1024)]); // remove '/var/www'
  Context.OutContent := '';
end;

procedure THttpServerSocketGeneric.NginxSendFileFrom(
  const FileNameLeftTrim: TFileName);
var
  n: PtrInt;
begin
  n := length(fNginxSendFileFrom);
  SetLength(fNginxSendFileFrom, n + 1);
  fNginxSendFileFrom[n] := FileNameLeftTrim;
  fOnSendFile := OnNginxAllowSend;
end;

procedure THttpServerSocketGeneric.InitializeTlsAfterBind;
begin
  if fSock.TLS.Enabled then
    exit;
  fSafe.Lock; // load certificates once from first connected thread
  try
    if not fSock.TLS.Enabled then
      fSock.DoTlsAfter(cstaBind);  // validate certificates now
  finally
    fSafe.UnLock;
  end;
end;

procedure THttpServerSocketGeneric.SetTlsServerNameCallback(
  const OnAccept: TOnNetTlsAcceptServerName);
begin
  if Assigned(fSock) then
    fSock.TLS.OnAcceptServerName := OnAccept;
end;

procedure THttpServerSocketGeneric.SetBlackListUri(const Uri: RawUtf8);
var
  ban: THttpAcceptBan;
begin
  ban := GetBanned;
  if (ban = nil) or
     (Uri = fBlackListUri) then
    exit; // unchanged or unsupported
  fBlackListUri := Uri;
  if Uri = '' then // disable the whole blacklist process
  begin
    fBlackListUriNextTix := 0; // disable BlackListUriReloadMin
    ban.Safe.Lock; // protect ban.BlackList access
    try
      ban.BlackList.Clear;
    finally
      ban.Safe.UnLock;
    end;
  end
  else
    fBlackListUriNextTix := 1; // force (re)load once on next idle in a thread
end;

procedure THttpServerSocketGeneric.RefreshBlackListUriExecute(Sender: TObject);
var
  ban: THttpAcceptBan;
  status, n: integer;
  crc, tix32: cardinal;
  list: RawUtf8;
  log: TSynLog;
begin
  // remote HTTP/HTTPS GET blacklist request in its own TLoggedWorkThread
  ban := GetBanned;
  if ban = nil then
    exit;
  log := fLogClass.Add;
  log.Log(sllTrace, 'RefreshBlackListUriExecute %', [fBlackListUri], self);
  status := 0;
  list := HttpGetWeak(fBlackListUri, '', @status);
  log.Log(sllTrace, 'RefreshBlackListUriExecute=% %', [status, KB(list)], self);
  if list = '' then
  begin
    log.Log(sllTrace, 'RefreshBlackListUriExecute will retry soon enough', self);
    tix32 := GetTickSec + SecsPerMin * 30;
    if tix32 < fBlackListUriNextTix then
      fBlackListUriNextTix := tix32; // retry at least twice an hour
    exit;
  end;
  crc := DefaultHash(list); // may be AesNiHash32()
  if crc = fBlackListUriCrc then
  begin
    log.Log(sllTrace, 'RefreshBlackListUriExecute: unchanged', self);
    exit;
  end;
  fBlackListUriCrc := crc;
  ban.Safe.Lock; // protect ban.BlackList access
  try
    n := ban.BlackList.LoadFrom(list);
  finally
    ban.Safe.UnLock;
  end;
  log.Log(sllDebug, 'RefreshBlackListUriExecute: set % rules', [n], self);
end;

procedure THttpServerSocketGeneric.RefreshBlackListUri(tix32: cardinal);
begin // caller ensured tix32 >= fBlackListUriNextTix
  fBlackListUriNextTix := fBlackListUriReloadMin * 60;
  if fBlackListUriNextTix <> 0 then
    inc(fBlackListUriNextTix, tix32);
  // use a dedicated thread since idle methods should not be blocking
  TLoggedWorkThread.Create(fLogClass, 'blacklist', self, RefreshBlackListUriExecute);
end;

procedure THttpServerSocketGeneric.SetBlackListUriReloadMin(Minutes: integer);
var
  olduri: RawUtf8;
begin
  if Minutes = fBlackListUriReloadMin then
    exit;
  fBlackListUriReloadMin := Minutes;
  olduri := fBlackListUri;
  fBlackListUri := ''; // force reset
  SetBlackListUri(olduri);
end;

procedure THttpServerSocketGeneric.SetAuthorizeNone;
begin
  fAuthorize := hraNone;
  fAuthorizerBasic := nil;
  fAuthorizerDigest := nil;
  fAuthorizeBasic := nil;
  fAuthorizeBasicRealm := '';
end;

procedure THttpServerSocketGeneric.SetAuthorizeDigest(
  const Digest: IDigestAuthServer);
begin
  SetAuthorizeNone;
  if Digest = nil then
    exit;
  fAuthorizerDigest := Digest;
  fAuthorize := hraDigest;
end;

procedure THttpServerSocketGeneric.SetAuthorizeBasic(
  const Basic: IBasicAuthServer);
begin
  SetAuthorizeNone;
  if Basic = nil then
    exit;
  fAuthorizerBasic := Basic;
  fAuthorize := hraBasic;
  fAuthorizeBasicRealm := Basic.BasicInit;
end;

procedure THttpServerSocketGeneric.SetAuthorizeBasic(const BasicRealm: RawUtf8;
  const OnBasicAuth: TOnHttpServerBasicAuth);
begin
  SetAuthorizeNone;
  if not Assigned(OnBasicAuth) then
    exit;
  fAuthorize := hraBasic;
  fAuthorizeBasic := OnBasicAuth;
  FormatUtf8('WWW-Authenticate: Basic realm="%"'#13#10, [BasicRealm],
    fAuthorizeBasicRealm);
end;

procedure THttpServerSocketGeneric.SetAuthorizeNegotiate;
begin
  SetAuthorizeNone;
  if not InitializeDomainAuth then
    EHttpServer.RaiseUtf8('%.SetAuthorizeNegotiate: no % available',
      [self, SECPKGNAMEAPI]);
   fAuthorize := hraNegotiate;
end;

function THttpServerSocketGeneric.AuthorizeServerMem: TDigestAuthServerMem;
begin
  result := nil;
  if self = nil then
    exit;
  if (fAuthorizerDigest <> nil) and
     fAuthorizerDigest.Instance.InheritsFrom(TDigestAuthServerMem) then
    result := TDigestAuthServerMem(fAuthorizerDigest.Instance)
  else if (fAuthorizerBasic <> nil) and
           fAuthorizerBasic.Instance.InheritsFrom(TDigestAuthServerMem) then
    result := TDigestAuthServerMem(fAuthorizerBasic.Instance)
end;

function THttpServerSocketGeneric.ComputeWwwAuthenticate(Opaque: Int64): RawUtf8;
begin
  // return the expected 'WWW-Authenticate: ####'#13#10 header content
  result := '';
  case fAuthorize of
    hraBasic:
      result := fAuthorizeBasicRealm; // includes trailing #13#10
    hraDigest:
      if fAuthorizerDigest <> nil then
        result := fAuthorizerDigest.DigestInit(Opaque, 0);
    hraNegotiate:
      result := SECPKGNAMEHTTPWWWAUTHENTICATE + #13#10; // with no NTLM support
  end;
end;

{$ifdef OSPOSIX}
procedure THttpServerSocketGeneric.SetKeyTab(const aKeyTab: TFileName);
var
  res: RawUtf8;
begin
  if FileIsKeyTab(aKeyTab) then
    if InitializeDomainAuth then
    begin
      fSafe.Lock;
      if fSspiKeyTab = nil then
        fSspiKeyTab := TServerSspiKeyTab.Create;
      fSafe.UnLock;
      if fSspiKeyTab.SetKeyTab(aKeyTab) then
        res := 'ok'
      else
        res := 'SetKeyTab failed';
    end
    else
      res := 'GSSAPI not available'
  else
    res := 'invalid file';
  fLogClass.Add.Log(LOG_DEBUGERROR[res <> 'ok'],
    'SetKeyTab(%): %', [aKeyTab, res], self);
end;

function THttpServerSocketGeneric.GetKeyTab: TFileName;
begin
  result := '';
  if fSspiKeyTab <> nil then
    result := fSspiKeyTab.KeyTab;
end;
{$endif OSPOSIX}

function THttpServerSocketGeneric.Authorization(var Http: THttpRequestContext;
  Opaque: Int64): TAuthServerResult;
var
  auth, b64, b64end: PUtf8Char;
  user, pass, url: RawUtf8;
  bin, bout: RawByteString;
  ctx: TSecContext;
begin
  // parse the 'Authorization: basic/digest/negotiate <magic>' header
  result := asrRejected;
  auth := FindNameValue(pointer(Http.Headers), 'AUTHORIZATION: ');
  if auth = nil then
    exit;
  try
    case fAuthorize of
      hraBasic:
        if IdemPChar(auth, 'BASIC ') and
           BasicServerAuth(auth + 6, user, pass) then
          try
            if Assigned(fAuthorizeBasic) then
              if fAuthorizeBasic(self, user, pass) then
                result := asrMatch
              else
                result := asrIncorrectPassword
            else if Assigned(fAuthorizerBasic) then
              result := fAuthorizerBasic.CheckCredential(user, pass);
          finally
            FillZero(pass);
          end;
      hraDigest:
        if (fAuthorizerDigest <> nil) and
           IdemPChar(auth, 'DIGEST ') then
        begin
          result := fAuthorizerDigest.DigestAuth(
             auth + 7, Http.CommandMethod, Opaque, 0, user, url);
          if (result = asrMatch) and
             (url <> Http.CommandUri) then
            result := asrRejected;
        end;
      hraNegotiate:
        // simple implementation assuming a two-way Negotiate/Kerberos handshake
        // - see TRestServerAuthenticationSspi.Auth() for NTLM / three-way
        if IdemPChar(auth, 'NEGOTIATE ') then
        begin
          b64 := auth + 10; // parse 'Authorization: Negotiate <base64 encoding>'
          b64end := PosChar(b64, #13);
          if (b64end = nil) or
             not Base64ToBin(PAnsiChar(b64), b64end - auth, bin) or
             ServerSspiDataNtlm(bin) then // two-way Kerberos only
            exit;
          {$ifdef OSPOSIX}
          if Assigned(fSspiKeyTab) then
            fSspiKeyTab.PrepareKeyTab; // do nothing if no KeyTab changed or set
          {$endif OSPOSIX}
          InvalidateSecContext(ctx);
          try
            if ServerSspiAuth(ctx, bin, bout) then
            begin
              ServerSspiAuthUser(ctx, user);
              Http.ResponseHeaders := BinToBase64(bout,
                SECPKGNAMEHTTPWWWAUTHENTICATE, #13#10, {magic=}false);
              result := asrMatch;
            end;
          finally
            FreeSecContext(ctx);
          end;
        end;
    else
      exit;
    end;
    if result = asrMatch then
      Http.BearerToken := user; // see THttpServerRequestAbstract.Prepare
  except
    on E: Exception do
    begin
      fLogClass.Add.Log(sllTrace, 'Authorization: % from %', [PClass(E)^, auth], self);
      result := asrRejected; // any processing error should silently fail the auth
    end;
  end;
  fLogClass.Add.Log(sllTrace, 'Authorization(%): % %',
    [ToText(fAuthorize)^, ToText(result)^, user], self);
end;

function THttpServerSocketGeneric.ComputeRejectBody(
  var Body: RawByteString; Opaque: Int64; Status: integer): boolean;
var
  reason: PRawUtf8;
  auth, html: RawUtf8;
begin
  reason := StatusCodeToText(status); // customizable method
  FormatUtf8('<!DOCTYPE html><html><head><title>%</title></head>' +
             '<body style="font-family:verdana"><h1>%</h1>' +
             '<p>Server rejected this request as % %.</body></html>',
    [reason^, reason^, status, reason^], html);
  result := (status = HTTP_UNAUTHORIZED) and
            (fAuthorize <> hraNone);
  if result then // don't close the connection but set grWwwAuthenticate
    auth := ComputeWwwAuthenticate(Opaque); // includes #13#10 trailer
  FormatUtf8('HTTP/1.% % %'#13#10'%' + HTML_CONTENT_TYPE_HEADER +
    #13#10'Content-Length: %'#13#10#13#10'%', [ord(result), status, reason^,
    auth, length(html), html], RawUtf8(Body));
end;


{ THttpServer }

constructor THttpServer.Create(const aPort: RawUtf8;
  const OnStart, OnStop: TOnNotifyThread; const ProcessName: RawUtf8;
  ServerThreadPoolCount: integer; KeepAliveTimeOut: integer;
  ProcessOptions: THttpServerOptions; aLog: TSynLogClass);
begin
  if fThreadPool <> nil then
    fThreadPool.ContentionAbortDelay := 5000; // 5 seconds default
  fInternalHttpServerRespList := TSynObjectListLocked.Create({ownobject=}false);
  if fThreadRespClass = nil then
    fThreadRespClass := THttpServerResp;
  if fSocketClass = nil then
    fSocketClass := THttpServerSocket;
  fServerSendBufferSize := 256 shl 10; // 256KB seems fine on Windows + POSIX
  inherited Create(aPort, OnStart, OnStop, ProcessName, ServerThreadPoolCount,
    KeepAliveTimeOut, ProcessOptions, aLog);
  fBanned := THttpAcceptBan.Create; // for hsoBan40xIP or BlackList
  if ServerThreadPoolCount > 0 then
  begin
    fThreadPool := TSynThreadPoolTHttpServer.Create(self, ServerThreadPoolCount);
    fHttpQueueLength := 1000;
    if hsoThreadCpuAffinity in ProcessOptions then
      SetServerThreadsAffinityPerCpu(nil, TThreadDynArray(fThreadPool.WorkThread))
    else if hsoThreadSocketAffinity in ProcessOptions then
      SetServerThreadsAffinityPerSocket(nil, TThreadDynArray(fThreadPool.WorkThread));
  end
  else if ServerThreadPoolCount < 0 then
    fMonoThread := true; // accept() + recv() + send() in a single thread
    // setting fHeaderRetrieveAbortDelay may be a good idea
end;

destructor THttpServer.Destroy;
var
  endtix: Int64;
  i: PtrInt;
  ilog: ISynLog;
  l: TSynLog;
  dummy: TNetSocket; // touch-and-go to the server to release main Accept()
begin
  l := nil;
  if hsoLogVerbose in fOptions then
    l := fLogClass.EnterLocal(ilog, 'Destroy % state=%',
      [fProcessName, ToText(fExecuteState)^], self);
  Terminate; // set Terminated := true for THttpServerResp.Execute
  if fThreadPool <> nil then
    fThreadPool.fTerminated := true; // notify background process
  if (fExecuteState = esRunning) and
     (Sock <> nil) then
  begin
    l.Log(sllTrace, 'Destroy: final connection', self);
    if Sock.SocketLayer <> nlUnix then
      Sock.Close; // shutdown TCP/UDP socket to unlock Accept() in Execute
    if NewSocket(Sock.Server, Sock.Port, Sock.SocketLayer,
       {dobind=}false, 10, 10, 10, 0, dummy) = nrOK then
      // Windows TCP/UDP socket may not release Accept() until something happen
      dummy.ShutdownAndClose({rdwr=}false);
    if Sock.SockIsDefined then
      Sock.Close; // nlUnix expects shutdown after accept() returned
  end;
  try
    if (fInternalHttpServerRespList <> nil) and // HTTP/1.1 long running threads
       (fInternalHttpServerRespList.Count <> 0) then
    begin
      l.Log(sllTrace, 'Destroy RespList=%',
        [fInternalHttpServerRespList.Count], self);
      endtix := mormot.core.os.GetTickCount64 + 20000;
      fInternalHttpServerRespList.Safe.ReadOnlyLock; // notify
      for i := 0 to fInternalHttpServerRespList.Count - 1 do
        THttpServerResp(fInternalHttpServerRespList.List[i]).Shutdown;
      fInternalHttpServerRespList.Safe.ReadOnlyUnLock;
      repeat
        // wait for all THttpServerResp.Execute to be finished
        fInternalHttpServerRespList.Safe.ReadOnlyLock;
        try
          if (fInternalHttpServerRespList.Count = 0) and
             (fExecuteState <> esRunning) then
            break;
        finally
          fInternalHttpServerRespList.Safe.ReadOnlyUnLock;
        end;
        SleepHiRes(10);
      until mormot.core.os.GetTickCount64 > endtix;
    end;
    FreeAndNilSafe(fInternalHttpServerRespList);
  finally
    l.Log(sllTrace, 'Destroy: finalize threads', self);
    FreeAndNilSafe(fThreadPool); // release all associated threads
    FreeAndNilSafe(fSock);
    if (fBanned <> nil) and
       (fBanned.Total <> 0) then
      l.Log(sllTrace, 'Destroy %', [fBanned], self);
    FreeAndNil(fBanned);
    inherited Destroy; // direct Thread abort, no wait till ended
  end;
end;

function THttpServer.GetExecuteState: THttpServerExecuteState;
begin
  result := fExecuteState;
end;

function THttpServer.GetBanned: THttpAcceptBan;
begin
  result := fBanned;
end;

function THttpServer.GetHttpQueueLength: cardinal;
begin
  result := fHttpQueueLength;
end;

procedure THttpServer.SetHttpQueueLength(aValue: cardinal);
begin
  fHttpQueueLength := aValue;
end;

function THttpServer.GetConnectionsActive: cardinal;
begin
  result := fServerConnectionActive;
end;

procedure THttpServer.DoCallbacks(tix64: Int64; sec32: integer);
var
  i: integer;
begin // is called at most every second, but maybe up to 5 seconds delay
  if Assigned(fOnAcceptIdle) then
    fOnAcceptIdle(self, tix64); // e.g. TAcmeLetsEncryptServer.OnAcceptIdle
  if Assigned(fLogger) then
    fLogger.OnIdle(tix64) // flush log file(s) on idle server
  else if Assigned(fAnalyzer) then
    fAnalyzer.OnIdle(tix64); // consolidate telemetry if needed
  if Assigned(fBanned) and
     (fBanned.Count <> 0) then
  begin
    if fBanSec <> 0 then
      for i := fBanSec + 1 to sec32 do // as many DoRotate as elapsed seconds
        fBanned.DoRotate // update internal THttpAcceptBan lists
    {$ifdef OSPOSIX} // Windows would require some activity - not an issue
    else
      fSock.ReceiveTimeout := 1000 // accept() to exit after one second
    {$endif OSPOSIX};
    fBanSec := sec32;
  end;
  if (fBlackListUriNextTix <> 0) and
     (cardinal(sec32) >= fBlackListUriNextTix) then
    RefreshBlackListUri(sec32);
  {$ifdef OSPOSIX}
  if Assigned(fSspiKeyTab) and
     fSspiKeyTab.TryRefresh(sec32) then
    fLogClass.Add.Log(sllDebug, 'DoCallbacks: refreshed %', [fSspiKeyTab], self);
  {$endif OSPOSIX}
end;

procedure THttpServer.DoExecute;
var
  cltsock: TNetSocket;
  cltaddr: TNetAddr;
  cltservsock: THttpServerSocket;
  res: TNetResult;
  banlen {$ifdef OSWINDOWS}, sec, acceptsec {$endif}: integer;
  tix64: QWord;
begin
  // THttpServerGeneric thread preparation: launch any OnHttpThreadStart event
  fExecuteState := esBinding;
  // main server process loop
  try
    // BIND + LISTEN (TLS is done later)
    fSock := TCrtSocket.Create(5000);
    if fLogClass <> nil  then
      fSock.OnLog := fLogClass.DoLog; // bind + TLS process
    fSock.BindPort(fSockPort, nlTcp, hsoReusePort in fOptions);
    if (fLogClass <> nil) and
       not (hsoLogVerbose in fOptions) then
      fSock.OnLog := nil;
    fExecuteState := esRunning;
    if not fSock.SockIsDefined then // paranoid check
      EHttpServer.RaiseUtf8('%.Execute: %.Bind failed', [self, fSock]);
    // main ACCEPT loop
    {$ifdef OSWINDOWS}
    acceptsec := 0;
    {$endif OSWINDOWS}
    while not Terminated do
    begin
      res := Sock.Sock.Accept(cltsock, cltaddr, {async=}false);
      if not (res in [nrOK, nrRetry]) then
      begin
        if Terminated then
          break;
        SleepHiRes(1); // failure (too many clients?) -> wait and retry
        continue;
      end;
      if Terminated or
         (Sock = nil) then
      begin
        if res = nrOk then
          cltsock.ShutdownAndClose({rdwr=}true);
        break; // don't accept input if server is down, and end thread now
      end;
      {$ifdef OSPOSIX}
      if res = nrRetry then // accept() timeout after 1 or 5 seconds on POSIX
      begin
        tix64 := mormot.core.os.GetTickCount64;
        DoCallbacks(tix64, tix64 div 1000);
        continue;
      end;
      {$else} // Windows accept() does not timeout and return nrRetry
      tix64 := mormot.core.os.GetTickCount64;
      sec := tix64 div 1000;
      if sec <> acceptsec then // trigger the callbacks once per second
      begin
        acceptsec := sec;
        DoCallbacks(tix64, sec);
      end;
      if res = nrRetry then
        continue; // not seen in practice, but won't hurt
      {$endif OSPOSIX}
      if fBanned.IsBanned(cltaddr) then // IP filtering from blacklist
      begin
        banlen := ord(HTTP_BANIP_RESPONSE[0]);
        cltsock.Send(@HTTP_BANIP_RESPONSE[1], banlen); // 418 I'm a teapot
        cltsock.ShutdownAndClose({rdwr=}true, {waitms=}10);
        continue; // abort even before TLS or HTTP start
      end;
      OnConnect;
      if fMonoThread then
        // ServerThreadPoolCount < 0 would use a single thread to rule them all
        // - may be defined when the server is expected to have very low usage,
        // e.g. for port 80 to 443 redirection or to implement Let's Encrypt
        // HTTP-01 challenges (on port 80) using OnHeaderParsed callback,
        // or for EphemeralHttpServer() function
        try
          cltservsock := fSocketClass.Create(self);
          try
            if (fLogClass <> nil) and
               (hsoLogVerbose in fOptions) then
              cltservsock.OnLog := fLogClass.DoLog;
            cltservsock.AcceptRequest(cltsock, @cltaddr);
            if hsoEnableTls in fOptions then
              cltservsock.DoTlsAfter(cstaAccept);
            case cltservsock.GetRequest({withbody=}true, HeaderRetrieveAbortTix) of
              grBodyReceived,
              grHeaderReceived:
                begin
                  include(cltservsock.Http.HeaderFlags, hfConnectionClose);
                  Process(cltservsock, 0, self);
                end;
              grClosed,      // e.g. gracefully disconnected
              grIntercepted: // handled by OnHeaderParsed event -> no ban
                ;
            else
              if (hsoBan40xIP in fOptions) and
                 fBanned.BanIP(cltaddr.IP4) then // e.g. after grTimeout
                IncStat(grBanned);
            end;
            OnDisconnect;
          finally
            cltservsock.Free;
          end;
        except
          on E: Exception do
            // do not stop thread on TLS or socket error
            if Assigned(fSock.OnLog) then
              fSock.OnLog(sllTrace, 'Execute: % [%]', [PClass(E)^, E.Message], self);
        end
      else if Assigned(fThreadPool) then
      begin
        // ServerThreadPoolCount > 0 will use the thread pool to process the
        // request header, and probably its body unless kept-alive or upgraded
        // - this is the most efficient way of using this server class
        cltservsock := fSocketClass.Create(self);
        // note: we tried to reuse the fSocketClass instance -> no perf benefit
        cltservsock.AcceptRequest(cltsock, @cltaddr);
        if not fThreadPool.Push(pointer(cltservsock), {waitoncontention=}true) then
          // was false if there is no idle thread in the pool, and queue is full
          cltservsock.Free; // will call DirectShutdown(cltsock)
      end
      else
        // ServerThreadPoolCount = 0 is a (somewhat resource hungry) fallback
        // implementation with one thread for each incoming socket
        fThreadRespClass.Create(cltsock, cltaddr, self);
    end;
  except
    on E: Exception do
      // any exception would break and release the thread
      FormatUtf8('% [%]', [E, E.Message], fExecuteMessage);
  end;
  fSafe.Lock;
  fExecuteState := esFinished;
  fSafe.UnLock;
end;

procedure THttpServer.OnConnect;
begin
  LockedInc32(@fServerConnectionCount);
  LockedInc32(@fServerConnectionActive);
end;

procedure THttpServer.OnDisconnect;
begin
  LockedDec32(@fServerConnectionActive);
end;

const
  STATICFILE_PROGWAITMS = 10; // up to 16ms on Windows

procedure SetAfterResponse(var ctx: TOnHttpServerAfterResponseContext;
  req: THttpServerRequest; cs: THttpServerSocket);
begin
  ctx.User := pointer(req.AuthenticatedUser);
  ctx.Method := pointer(req.Method);
  ctx.Host := pointer(req.Host);
  ctx.Url := pointer(req.Url);
  ctx.Referer := pointer(cs.Http.Referer);
  ctx.UserAgent := pointer(req.UserAgent);
  ctx.RemoteIP := pointer(req.RemoteIP);
  ctx.Connection := req.ConnectionID;
  ctx.Flags := req.ConnectionFlags;
  ctx.State := cs.Http.State;
  ctx.StatusCode := req.RespStatus;
  ctx.Tix64 := 0;
  ctx.Received := cs.BytesIn;
  ctx.Sent := cs.BytesOut;
end;

procedure DoAfterResponse(req: THttpServerRequest; cs: THttpServerSocket;
  started: Int64);
var
  ctx: TOnHttpServerAfterResponseContext;
begin
  SetAfterResponse(ctx, req, cs);
  QueryPerformanceMicroSeconds(ctx.ElapsedMicroSec);
  dec(ctx.ElapsedMicroSec, started);
  try
    cs.Server.fOnAfterResponse(ctx); // e.g. THttpLogger or THttpAnalyzer
  except
    on E: Exception do // paranoid
    begin
      cs.Server.fOnAfterResponse := nil; // won't try again
      if Assigned(cs.OnLog) then
        cs.OnLog(sllWarning,
          'Process: OnAfterResponse raised % -> disabled', [E], cs.Server);
    end;
  end;
end;

procedure THttpServer.Process(ClientSock: THttpServerSocket;
  ConnectionID: THttpServerConnectionID; ConnectionThread: TSynThread);
var
  started: Int64;
var
  req: THttpServerRequest;
  output: PRawByteStringBuffer;
  dest: TRawByteStringBuffer;
begin
  if (ClientSock = nil) or
     (ClientSock.Http.Headers = '') or
     Terminated then
    // we didn't get the request = socket read error
    exit; // -> send will probably fail -> nothing to send back
  // compute and send back the response
  if Assigned(fOnAfterResponse) then
    QueryPerformanceMicroSeconds(started);
  req := THttpServerRequest.Create(self, ConnectionID, ConnectionThread, 0,
    ClientSock.fRequestFlags, ClientSock.GetConnectionOpaque);
  try
    LockedInc32(@fCurrentProcess);
    // compute the response
    req.Prepare(ClientSock.Http, ClientSock.fRemoteIP, fAuthorize);
    DoRequest(req);
    output := req.SetupResponse(
      ClientSock.Http, fCompressGz, fServerSendBufferSize);
    if (hsoBan40xIP in fOptions) and
       fBanned.ShouldBan(req.RespStatus, ClientSock.fRemoteIP) then
      IncStat(grBanned);
    // send back the response
    if Terminated then
      exit;
    if hfConnectionClose in ClientSock.Http.HeaderFlags then
      ClientSock.fKeepAliveClient := false;
    if ClientSock.TrySndLow(output.Buffer, output.Len) then // header[+body]
      while not Terminated do
      begin
        case ClientSock.Http.State of
          hrsResponseDone:
            break; // finished (set e.g. by ClientSock.Http.ProcessBody)
          hrsSendBody:
            begin
              dest.Reset; // body is retrieved from Content/ContentStream
              case DoProcessBody(ClientSock.Http, dest, fServerSendBufferSize) of
                hrpSend:
                  if ClientSock.TrySndLow(dest.Buffer, dest.Len) then
                    continue;
                hrpWait:
                  begin
                    SleepHiRes(STATICFILE_PROGWAITMS);
                    continue; // wait until got some data
                  end;
                hrpDone:
                  break;
              else // hrpAbort:
                if Assigned(ClientSock.OnLog) then
                  ClientSock.OnLog(sllWarning,
                    'Process: ProcessBody aborted (ProgressiveID=%)',
                    [ClientSock.Http.ProgressiveID], self);
              end;
            end;
        end;
        ClientSock.fKeepAliveClient := false; // socket close on write error
        break;
      end
    else
      ClientSock.fKeepAliveClient := false;
    // the response has been sent: handle optional OnAfterResponse event
    if Assigned(fOnAfterResponse) then
      DoAfterResponse(req, ClientSock, started);
  finally
    req.Free;
    LockedDec32(@fCurrentProcess);
    if Assigned(fProgressiveRequests) then
      DoProgressiveRequestFree(ClientSock.Http); // e.g. THttpPartials.Remove
    ClientSock.Http.ProcessDone;   // ContentStream.Free
  end;
  // add transfert stats to main socket
  if Sock <> nil then
  begin
    fSafe.Lock;
    Sock.BytesIn := Sock.BytesIn + ClientSock.BytesIn;
    Sock.BytesOut := Sock.BytesOut + ClientSock.BytesOut;
    fSafe.UnLock;
  end;
  ClientSock.fBytesIn := 0;
  ClientSock.fBytesOut := 0;
end;


{ THttpServerSocket }

procedure THttpServerSocket.TaskProcess(aCaller: TSynThreadPoolWorkThread);
var
  freeme: boolean;
  res: THttpServerSocketGetRequestResult;
begin
  // process this THttpServerSocket request in the thread pool
  freeme := true;
  try
    // (slow) TLS handshake is done in this sub-thread
    if (hsoEnableTls in fServer.Options) and
       (fSecure = nil) then
      DoTlsAfter(cstaAccept);
    // get and validate the headers of this first request
    res := GetRequest({withbody=}false, fServer.HeaderRetrieveAbortTix);
    // process any auth steps, then body in this thread or in a fThreadRespClass
    freeme := TaskProcessBody(aCaller, res);
  finally
    if freeme then // false if kept-alive in a fThreadRespClass thread
      Free;
  end;
end;

function THttpServerSocket.TaskProcessBody(aCaller: TSynThreadPoolWorkThread;
  aHeaderResult: THttpServerSocketGetRequestResult): boolean;
var
  pool: TSynThreadPoolTHttpServer;
begin
  result := true; // freeme = true by default
  if (fServer = nil) or
     fServer.Terminated  then
    exit;
  // properly get the incoming body and process the request
  repeat
    fServer.IncStat(aHeaderResult);
    case aHeaderResult of
      grHeaderReceived:
        begin
          pool := TSynThreadPoolTHttpServer(aCaller.Owner);
          // connection and header seem valid -> process request further
          if (fServer.fServerKeepAliveTimeOut > 0) and
             (fServer.fInternalHttpServerRespList.Count < pool.MaxBodyThreadCount) and
             (KeepAliveClient or
              (Http.ContentLength > pool.BigBodySize)) then
          begin
            // HTTP/1.1 Keep Alive (including WebSockets) or posted data > 16 MB
            // -> process in dedicated background thread
            fServer.fThreadRespClass.Create(self, fServer);
            result := false; // freeme = false: THttpServerResp will own self
          end
          else
          begin
            // no Keep Alive = multi-connection -> process in the Thread Pool
            if not (hfConnectionUpgrade in Http.HeaderFlags) and
               not HttpMethodWithNoBody(Method) then
            begin
              GetBody; // we need to get it now
              fServer.IncStat(grBodyReceived);
            end;
            // multi-connection -> process now
            fServer.Process(self, fRemoteConnectionID, aCaller);
            fServer.OnDisconnect;
            // no Shutdown here: will be done client-side
          end;
        end;
      grIntercepted:
        ; // response was sent by OnHeaderParsed()
      grWwwAuthenticate:
        // return 401 and wait for the "Authorize:" answer in the thread pool
        aHeaderResult := GetRequest(false, fServer.HeaderRetrieveAbortTix);
    else
      begin
        if Assigned(fServer.Sock.OnLog) then
          fServer.Sock.OnLog(sllTrace, 'Task: close after GetRequest=% from %',
              [ToText(aHeaderResult)^, fRemoteIP], self);
        if (aHeaderResult <> grClosed) and
           (hsoBan40xIP in fServer.Options) and
           fServer.fBanned.BanIP(fRemoteIP) then
          fServer.IncStat(grBanned);
      end;
    end;
  until aHeaderResult <> grWwwAuthenticate; // continue handshake in this thread
end;

constructor THttpServerSocket.Create(aServer: THttpServer);
begin
  inherited Create(5000);
  if aServer <> nil then // nil e.g. from TRtspOverHttpServer
  begin
    fServer := aServer;
    if aServer.fCompressList.Algo <> nil then
      Http.CompressList := @aServer.fCompressList;
    fSocketLayer := aServer.Sock.SocketLayer;
    if hsoEnableTls in aServer.fOptions then
    begin
      if not aServer.fSock.TLS.Enabled then // if not already in WaitStarted()
        aServer.InitializeTlsAfterBind;     // load certificate(s) once
      TLS.AcceptCert := aServer.Sock.TLS.AcceptCert; // TaskProcess cstaAccept
    end;
    OnLog := aServer.Sock.OnLog;
  end;
end;

function THttpServerSocket.GetRequest(withBody: boolean;
  headerMaxTix: Int64): THttpServerSocketGetRequestResult;
var
  P: PUtf8Char;
  status, tix32: cardinal;
  noheaderfilter, http10: boolean;
begin
  try
    // use SockIn with 1KB buffer if not already initialized: 2x faster
    if SockIn = nil then
      CreateSockIn;
    // abort now with no exception if socket is obviously broken
    result := grClosed;
    if fServer <> nil then
    begin
      if (SockInPending(100) < 0) or
         (fServer = nil) or
         fServer.Terminated then
        exit;
      noheaderfilter := hsoHeadersUnfiltered in fServer.Options;
    end
    else
      noheaderfilter := false;
    // 1st line is command: 'GET /path HTTP/1.1' e.g.
    SockRecvLn(Http.CommandResp);
    P := pointer(Http.CommandResp);
    if P = nil then
      exit; // connection is likely to be broken or closed
    GetNextItem(P, ' ', Http.CommandMethod); // 'GET'
    GetNextItem(P, ' ', Http.CommandUri);    // '/path'
    result := grRejected;
    if (P = nil) or
       (PCardinal(P)^ <>
         ord('H') + ord('T') shl 8 + ord('T') shl 16 + ord('P') shl 24) then
      exit;
    http10 := P[7] = '0';
    fKeepAliveClient := ((fServer = nil) or
                         (fServer.fServerKeepAliveTimeOut > 0)) and
                        not http10;
    Http.Content := '';
    // get and parse HTTP request header
    if not GetHeader(noheaderfilter) then
    begin
      SockSendFlush('HTTP/1.0 400 Bad Request'#13#10 +
        'Content-Length: 16'#13#10#13#10'Rejected Headers');
      exit;
    end;
    fServer.ParseRemoteIPConnID(Http.Headers, fRemoteIP, fRemoteConnectionID);
    if hfConnectionClose in Http.HeaderFlags then
      fKeepAliveClient := false;
    if (Http.ContentLength < 0) and
       (KeepAliveClient or
        IsGet(Http.CommandMethod)) then
      Http.ContentLength := 0; // HTTP/1.1 and no content length -> no eof
    if (headerMaxTix > 0) and
       (mormot.core.os.GetTickCount64 > headerMaxTix) then
    begin
      result := grTimeout;
      exit; // allow 10 sec for header -> DOS/TCPSYN Flood
    end;
    if fServer <> nil then
    begin
      // allow very early THttpServer.OnHeaderParsed low-level callback
      if Assigned(fServer.fOnHeaderParsed) then
      begin
        result := fServer.fOnHeaderParsed(self);
        if result <> grHeaderReceived then
          exit; // the callback made its own SockSend() response
      end;
      // validate allowed PayLoad size
      if (Http.ContentLength > 0) and
         (fServer.MaximumAllowedContentLength > 0) and
         (Http.ContentLength > fServer.MaximumAllowedContentLength) then
      begin
        // 413 HTTP error (and close connection)
        fServer.ComputeRejectBody(Http.Content, 0, HTTP_PAYLOADTOOLARGE);
        SockSendFlush(Http.Content);
        result := grOversizedPayload;
        exit;
      end;
      // implement early hsoRejectBotUserAgent detection as 418 I'm a teapot
      if (hsoRejectBotUserAgent in fServer.Options) and
         (Http.UserAgent <> '') and
         IsHttpUserAgentBot(Http.UserAgent) then
      begin
        SockSend(@HTTP_BANIP_RESPONSE[1], ord(HTTP_BANIP_RESPONSE[0]));
        SockSendFlush;
        result := grRejected;
        exit;
      end;
      // support optional Basic/Digest authentication
      fRequestFlags := HTTP_TLS_FLAGS[TLS.Enabled] +
                       HTTP_UPG_FLAGS[hfConnectionUpgrade in Http.HeaderFlags] +
                       HTTP_10_FLAGS[http10];
      if (hfHasAuthorization in Http.HeaderFlags) and
         (fServer.fAuthorize <> hraNone) then
      begin
        if fServer.Authorization(Http, fRemoteConnectionID) = asrMatch then
        begin
          fAuthorized := fServer.fAuthorize;
          include(fRequestFlags, hsrAuthorized);
        end
        else
        begin
          tix32 := mormot.core.os.GetTickCount64 shr 12;
          if fAuthTix32 = tix32 then
          begin
            // 403 HTTP error if not authorized (and close connection)
            fServer.ComputeRejectBody(Http.Content, 0, HTTP_FORBIDDEN);
            SockSendFlush(Http.Content);
            result := grRejected;
            exit;
          end
          else
            // 401 HTTP error to ask for credentials and renew after 4 seconds
            // (ConnectionID may have changed in-between)
            fAuthTix32 := tix32;
        end;
      end;
      // allow OnBeforeBody callback for quick response
      if Assigned(fServer.OnBeforeBody) then
      begin
        HeadersPrepare(fRemoteIP); // will include remote IP to Http.Headers
        status := fServer.OnBeforeBody(Http.CommandUri, Http.CommandMethod,
          Http.Headers, Http.ContentType, fRemoteIP, Http.BearerToken,
          Http.ContentLength, fRequestFlags);
        {$ifdef SYNCRTDEBUGLOW}
        TSynLog.Add.Log(sllCustom2,
          'GetRequest sock=% OnBeforeBody=% Command=% Headers=%', [fSock, status,
          LogEscapeFull(Command), LogEscapeFull(allheaders)], self);
        {$endif SYNCRTDEBUGLOW}
        if status <> HTTP_SUCCESS then
        begin
          if fServer.ComputeRejectBody(Http.Content, fRemoteConnectionID, status) then
            result := grWwwAuthenticate
          else
            result := grRejected;
          SockSendFlush(Http.Content);
          exit;
        end;
      end;
    end;
    // implement 'Expect: 100-Continue' Header
    if hfExpect100 in Http.HeaderFlags then
      // client waits for the server to parse the headers and return 100
      // before sending the request body
      SockSendFlush('HTTP/1.1 100 Continue'#13#10#13#10);
    // now the server could retrieve the HTTP request body
    if withBody and
       not (hfConnectionUpgrade in Http.HeaderFlags) then
    begin
      if not HttpMethodWithNoBody(Http.CommandMethod) then
        GetBody;
      result := grBodyReceived;
    end
    else
      result := grHeaderReceived;
  except
    on E: Exception do
      result := grException;
  end;
end;

function THttpServerSocket.GetConnectionOpaque: PHttpServerConnectionOpaque;
begin
  if (fServer = nil) or
     (fServer.fRemoteConnIDHeaderUpper = '') then
    result := @fConnectionOpaque
  else
    result := nil // "opaque" is clearly unsupported behind a proxy
end;


{ THttpServerResp }

constructor THttpServerResp.Create(aSock: TNetSocket; const aSin: TNetAddr;
  aServer: THttpServer);
var
  c: THttpServerSocketClass;
begin
  fClientSock := aSock;
  fClientSin := aSin;
  if aServer = nil then
    c := THttpServerSocket
  else
    c := aServer.fSocketClass;
  Create(c.Create(aServer), aServer); // on Linux, Execute raises during Create
end;

constructor THttpServerResp.Create(aServerSock: THttpServerSocket;
  aServer: THttpServer);
begin
  fServer := aServer;
  fServerSock := aServerSock;
  fOnThreadTerminate := fServer.fOnThreadTerminate;
  fServer.fInternalHttpServerRespList.Add(self);
  fConnectionID := aServerSock.fRemoteConnectionID;
  FreeOnTerminate := true;
  inherited Create({suspended=}false);
end;

procedure THttpServerResp.Shutdown;
begin
  Terminate;
  if fServerSock <> nil then
    fServerSock.Close;
end;

procedure THttpServerResp.Execute;

  procedure HandleRequestsProcess;
  var
    keepaliveendtix, beforetix, headertix, tix: Int64;
    pending: TCrtSocketPending;
    res: THttpServerSocketGetRequestResult;
    banned: boolean;
  begin
    {$ifdef SYNCRTDEBUGLOW}
    try
    {$endif SYNCRTDEBUGLOW}
    try
      repeat
        beforetix := mormot.core.os.GetTickCount64;
        keepaliveendtix := beforetix + fServer.fServerKeepAliveTimeOut;
        repeat
          // within this loop, break=wait for next command, exit=quit
          if (fServer = nil) or
             fServer.Terminated or
             (fServerSock = nil) then
            // server is down -> close connection
            exit;
          pending := fServerSock.SockReceivePending(50); // 50 ms timeout
          if (fServer = nil) or
             fServer.Terminated then
            // server is down -> disconnect the client
            exit;
          {$ifdef SYNCRTDEBUGLOW}
          TSynLog.Add.Log(sllCustom2, 'HandleRequestsProcess: sock=% pending=%',
            [fServerSock.fSock, _CSP[pending]], self);
          {$endif SYNCRTDEBUGLOW}
          case pending of
            cspSocketError,
            cspSocketClosed:
              begin
                if Assigned(fServer.Sock.OnLog) then
                  fServer.Sock.OnLog(sllTrace, 'Execute: Socket error from %',
                    [fServerSock.RemoteIP], self);
                exit; // disconnect the client
              end;
            cspNoData:
              begin
                tix := mormot.core.os.GetTickCount64;
                if tix >= keepaliveendtix then
                begin
                  if Assigned(fServer.Sock.OnLog) then
                    fServer.Sock.OnLog(sllTrace, 'Execute: % KeepAlive=% timeout',
                      [fServerSock.RemoteIP, keepaliveendtix - tix], self);
                  exit; // reached keep alive time out -> close connection
                end;
                if tix - beforetix < 40 then
                begin
                  {$ifdef SYNCRTDEBUGLOW}
                  // getsockopt(fServerSock.fSock,SOL_SOCKET,SO_ERROR,@error,errorlen) returns 0 :(
                  TSynLog.Add.Log(sllCustom2,
                    'HandleRequestsProcess: sock=% LOWDELAY=%',
                    [fServerSock.fSock, tix - beforetix], self);
                  {$endif SYNCRTDEBUGLOW}
                  SleepHiRes(1); // seen only on Windows in practice
                  if (fServer = nil) or
                     fServer.Terminated then
                    // server is down -> disconnect the client
                    exit;
                end;
                beforetix := tix;
              end;
            cspDataAvailable,
            cspDataAvailableOnClosedSocket:
              begin
                // get request and headers
                headertix := fServer.HeaderRetrieveAbortDelay;
                if headertix > 0 then
                  inc(headertix, beforetix);
                res := fServerSock.GetRequest({withbody=}true, headertix);
                if (fServer = nil) or
                   fServer.Terminated then
                  // server is down -> disconnect the client
                  exit;
                if pending = cspDataAvailableOnClosedSocket then
                  fServerSock.KeepAliveClient := false; // we can't keep it
                fServer.IncStat(res);
                case res of
                  grBodyReceived,
                  grHeaderReceived:
                    begin
                      if res = grBodyReceived then
                        fServer.IncStat(grHeaderReceived);
                      // calc answer and send response
                      fServer.Process(fServerSock, ConnectionID, self);
                      // keep connection only if necessary
                      if fServerSock.KeepAliveClient then
                        break
                      else
                        exit;
                    end;
                  grWwwAuthenticate:
                    if fServerSock.KeepAliveClient then
                      break
                    else
                      exit;
                else
                  begin
                    banned := (res <> grClosed) and
                              (hsoBan40xIP in fServer.Options) and
                              fServer.fBanned.BanIP(fServerSock.RemoteIP);
                    if banned then
                      fServer.IncStat(grBanned);
                    if Assigned(fServer.Sock.OnLog) then
                      fServer.Sock.OnLog(sllTrace,
                        'Execute: close after GetRequest=% from % (ban=%)',
                        [ToText(res)^, fServerSock.RemoteIP, banned], self);
                    exit;
                  end;
                end;
              end;
          end;
        until false;
      until false;
    except
      on E: Exception do
        ; // any exception will silently disconnect the client
    end;
    {$ifdef SYNCRTDEBUGLOW}
    finally
      TSynLog.Add.Log(sllCustom2, 'HandleRequestsProcess: close sock=%',
        [fServerSock.fSock], self);
    end;
    {$endif SYNCRTDEBUGLOW}
  end;

var
  netsock: TNetSocket;
begin
  SetCurrentThreadName('=conn-%', [fServerSock.RemoteConnectionID]);
  fServer.NotifyThreadStart(self);
  try
    try
      if fClientSock.Socket <> 0 then
      begin
        // direct call from incoming socket
        netsock := fClientSock;
        fClientSock := nil; // fServerSock owns fClientSock
        fServerSock.AcceptRequest(netsock, @fClientSin);
        if fServer <> nil then
          HandleRequestsProcess;
      end
      else
      begin
        // call from TSynThreadPoolTHttpServer -> handle first request
        if not (fBodyRetrieved in fServerSock.fFlags) and
           not HttpMethodWithNoBody(fServerSock.Http.CommandMethod) then
          fServerSock.GetBody;
        fServer.Process(fServerSock, ConnectionID, self);
        if (fServer <> nil) and
           fServerSock.KeepAliveClient then
          HandleRequestsProcess; // process further kept alive requests
      end;
    finally
      try
        if fServer <> nil then
          try
            fServer.OnDisconnect;
            if Assigned(fOnThreadTerminate) then
              fOnThreadTerminate(self);
          finally
            fServer.fInternalHttpServerRespList.Remove(self);
            fServer := nil;
            fOnThreadTerminate := nil;
          end;
      finally
        FreeAndNilSafe(fServerSock);
        // if Destroy happens before fServerSock.GetRequest() in Execute below
        fClientSock.ShutdownAndClose({rdwr=}false);
      end;
    end;
  except
    on Exception do
      ; // just ignore unexpected exceptions here, especially during clean-up
  end;
  TSynLog.NotifyThreadEnded; // manual TSynThread notification
end;


{ TSynThreadPoolTHttpServer }

constructor TSynThreadPoolTHttpServer.Create(Server: THttpServer;
  NumberOfThreads: integer);
begin
  fServer := Server;
  fOnThreadTerminate := fServer.fOnThreadTerminate;
  fBigBodySize := THREADPOOL_BIGBODYSIZE;
  fMaxBodyThreadCount := THREADPOOL_MAXWORKTHREADS;
  fPoolName := 'http';
  inherited Create(NumberOfThreads,
    {$ifdef USE_WINIOCP} INVALID_HANDLE_VALUE {$else} {queuepending=}true{$endif},
    Server.ProcessName);
end;

{$ifndef USE_WINIOCP}
function TSynThreadPoolTHttpServer.QueueLength: integer;
begin
  if fServer = nil then
    result := 10000
  else
    result := fServer.fHttpQueueLength;
end;
{$endif USE_WINIOCP}

procedure TSynThreadPoolTHttpServer.Task(
  aCaller: TSynThreadPoolWorkThread; aContext: pointer);
begin
  // process this THttpServerSocket in the thread pool
  if (fServer = nil) or
     fServer.Terminated then
    THttpServerSocket(aContext).Free
  else
    THttpServerSocket(aContext).TaskProcess(aCaller);
end;

procedure TSynThreadPoolTHttpServer.TaskAbort(aContext: pointer);
begin
  THttpServerSocket(aContext).Free;
end;


function ToText(res: THttpServerSocketGetRequestResult): PShortString;
begin
  result := GetEnumName(TypeInfo(THttpServerSocketGetRequestResult), ord(res));
end;

function ToText(state: THttpServerExecuteState): PShortString;
begin
  result := GetEnumName(TypeInfo(THttpServerExecuteState), ord(state));
end;


{ THttpServerEphemeral }

type
  THttpServerEphemeral = class(THttpServer)
  protected
    fResponse: RawUtf8;
    fParams: PDocVariantData;
    fMethod: TUriMethods;
    fDone: TSynEvent;
    fReceived: TSynEvent;
  public
    constructor Create(const aPort, aResponse: RawUtf8; aParams: PDocVariantData;
      aLogClass: TSynLogClass; aMethod: TUriMethods; aOptions: THttpServerOptions); reintroduce;
    destructor Destroy; override;
    function Request(Ctxt: THttpServerRequestAbstract): cardinal; override;
    procedure OnResponded(var Context: TOnHttpServerAfterResponseContext);
  end;

constructor THttpServerEphemeral.Create(const aPort, aResponse: RawUtf8;
  aParams: PDocVariantData; aLogClass: TSynLogClass; aMethod: TUriMethods;
  aOptions: THttpServerOptions);
begin
  fResponse := aResponse;
  fParams := aParams;
  fMethod := aMethod;
  fOnAfterResponse := OnResponded;
  fReceived := TSynEvent.Create;
  inherited Create(
    aPort, nil, nil, 'ephemeral', {threadpool=}-1, 0, aOptions, aLogClass);
end;

destructor THttpServerEphemeral.Destroy;
begin
  inherited Destroy;
  fReceived.Free; 
end;

function THttpServerEphemeral.Request(Ctxt: THttpServerRequestAbstract): cardinal;
var
  m: TUriMethod;
begin
  m := ToMethod(Ctxt.Method);
  if m in fMethod then
  begin
    if fParams <> nil then
      THttpServerRequest(Ctxt).ToDocVariant(fParams^);
    Ctxt.OutContent := fResponse;
    result := HTTP_SUCCESS;
  end
  else
    result := HTTP_NOTALLOWED;
end;

procedure THttpServerEphemeral.OnResponded(var Context: TOnHttpServerAfterResponseContext);
begin
  fReceived.SetEvent; // response sent: we can shutdown the server
end;


function EphemeralHttpServer(const aPort: RawUtf8; out aParams: TDocVariantData;
  aTimeOutSecs: integer; aLogClass: TSynLogClass; const aResponse: RawUtf8;
  aMethods: TUriMethods; aOptions: THttpServerOptions): boolean;
var
  server: THttpServerEphemeral;
begin
  aParams.Clear;
  server := THttpServerEphemeral.Create(
    aPort, aResponse, @aParams, aLogClass, aMethods, aOptions);
  try
    result := server.fReceived.WaitForSafe(aTimeOutSecs * MilliSecsPerSec);
    if aLogClass <> nil then
      aLogClass.Add.Log(sllDebug, 'EphemeralHttpServer(%)=% %',
        [aPort, BOOL_STR[result], variant(aParams)], server);
    if result then
      SleepHiRes(10); // wait for the client connection to gracefully disconnect
  finally
    server.Free;
  end;
end;


{ ******************** THttpPeerCache Local Peer-to-peer Cache }

{ THttpPeerCrypt }

procedure THttpPeerCrypt.AfterSettings;
var
  err: RawUtf8;
begin
  if fSettings = nil then
    EHttpPeerCache.RaiseUtf8('%.AfterSettings(nil)', [self]);
  fLog.Add.Log(sllTrace, 'Create: with %', [fSettings], self);
  err := fSettings.GuessInterface(fMac);
  if err <> '' then
    EHttpPeerCache.RaiseUtf8('%.Create: %', [self, err]);
  IPToCardinal(fMac.IP, fIP4);
  IPToCardinal(fMac.NetMask, fMaskIP4);
  IPToCardinal(fMac.Broadcast, fBroadcastIP4);
  UInt32ToUtf8(fSettings.Port, fPort);
  FormatUtf8('%:%', [fMac.IP, fPort], fIpPort); // UDP/TCP bound to this network
  if fSettings.RejectInstablePeersMin > 0 then
  begin
    fInstable := THttpAcceptBan.Create(fSettings.RejectInstablePeersMin);
    fInstable.WhiteIP := fIP4; // no UDP ban from localhost
  end;
  fLog.Add.Log(sllDebug, 'Create: network="%" as % (broadcast=%) %',
    [fMac.Name, fIpPort, fMac.Broadcast, fMac.Address], self);
end;

function THttpPeerCrypt.GetUuidText: RawUtf8;
begin
  ToUtf8(fUuid, result);
end;

function THttpPeerCrypt.CurrentConnections: integer;
begin
  result := 0; // to be properly overriden with the HTTP server information
end;

procedure THttpPeerCrypt.MessageInit(aKind: THttpPeerCacheMessageKind;
  aSeq: cardinal; out aMsg: THttpPeerCacheMessage);
var
  n: cardinal;
begin
  FillCharFast(aMsg, SizeOf(aMsg) - SizeOf(aMsg.Padding), 0);
  SharedRandom.Fill(@aMsg.Padding, SizeOf(aMsg.Padding)); // TLecuyer is enough
  if aSeq = 0 then
    aSeq := InterlockedIncrement(fFrameSeq);
  aMsg.Seq := aSeq;
  aMsg.Kind := aKind;
  aMsg.Uuid := fUuid;
  aMsg.Os := OSVersion32;
  aMsg.IP4 := fIP4;
  aMsg.DestIP4 := fBroadcastIP4; // overriden in DoSendResponse()
  aMsg.MaskIP4 := fMaskIP4;
  aMsg.BroadcastIP4 := fBroadcastIP4;
  aMsg.Speed := fMac.Speed;
  aMsg.Hardware := fMac.Kind;
  aMsg.Timestamp := UnixTimeMinimalUtc;
  n := CurrentConnections; // virtual method
  if n > 65535 then
    n := 65535;
  aMsg.Connections := n;
end;

// UDP frames are AES-GCM encrypted and signed, ending with a 32-bit crc, fixed
// to crc32c(): md5/sha (without SHA-NI) are slower than AES-GCM-128 itself ;)
// - on x86_64
//    2000 messages in 275us i.e. 6.9M/s, aver. 137ns, 1.3 GB/s   = AES-GCM-128
//    10000 altered in 126us i.e. 75.6M/s, aver. 12ns, 14.1 GB/s  = crc32c()
// - on i386
//    2000 messages in 673us i.e. 2.8M/s, aver. 336ns, 544.1 MB/s
//    10000 altered in 310us i.e. 30.7M/s, aver. 31ns, 5.7 GB/s

procedure THttpPeerCrypt.MessageEncode(const aMsg: THttpPeerCacheMessage;
  out aEncoded: THttpPeerCacheMessageEncoded);
begin
  // AES-GCM-128 encoding and authentication
  fAesSafe.Lock;
  try
    if fAesEnc.AesGcmBuffer(@aMsg, @aEncoded, SizeOf(aMsg), SizeOf(aEncoded),
          true, true) <> PEER_CACHE_AESLEN then
      exit;
  finally
    fAesSafe.UnLock;
  end;
  // append salted checksum to quickly reject any fuzzing attempt (endsize=4)
  aEncoded.crc := crc32c(fSharedMagic, @aEncoded, PEER_CACHE_AESLEN);
end;

procedure THttpPeerCrypt.MessageEncodeBearer(const aMsg: THttpPeerCacheMessage;
  out aBearer: RawUtf8);
var
  bin: THttpPeerCacheMessageEncoded;
begin
  MessageEncode(aMsg, bin);
  aBearer := AuthorizationBearer(BinToBase64uri(@bin, SizeOf(bin)));
end;

function THttpPeerCrypt.MessageDecode(
  aFrame: PHttpPeerCacheMessageEncoded; aFrameLen: PtrInt;
  out aMsg: THttpPeerCacheMessage): THttpPeerCryptMessageDecode;
var
  tmp: THttpPeerCacheMessageEncoded; // need more space to decode the padding
begin
  // quickly reject any naive fuzzing attempt against length and 32-bit checksum
  result := mdLen;
  PByte(@aMsg.Kind)^ := 255; // so that ToText(aMsg) = ToText(aMsg.Kind)^ = ''
  if aFrameLen <> PEER_CACHE_MESSAGELEN then
    exit;
  result := mdCrc;
  if crc32c(fSharedMagic, pointer(aFrame), PEER_CACHE_AESLEN) <> aFrame^.crc then
     exit;
  // AES-GCM-128 decoding and authentication
  result := mdAes;
  fAesSafe.Lock;
  try
    if fAesDec.AesGcmBuffer(aFrame, @tmp, PEER_CACHE_AESLEN, SizeOf(tmp),
         {enc=}false, {iv=}true) <> SizeOf(aMsg) then
      exit;
  finally
    fAesSafe.UnLock;
  end;
  MoveFast(tmp, aMsg, SizeOf(aMsg));
  // check consistency of the decoded THttpPeerCacheMessage value
  if (aMsg.Kind in PCF_RESPONSE) and  // responses are broadcasted on POSIX
     (aMsg.DestIP4 = fIP4) and // validate against local sequence
     ((aMsg.Seq < fFrameSeqLow) or
       (aMsg.Seq > cardinal(fFrameSeq))) then
    result := mdSeq
  else if ord(aMsg.Kind) > ord(high(aMsg.Kind)) then
    result := mdKind
  else if ord(aMsg.Hardware) > ord(high(aMsg.Hardware)) then
    result := mdHw
  else if ord(aMsg.Hash.Algo) > ord(high(aMsg.Hash.Algo)) then
    result := mdAlgo
  else
    result := mdOk;
end;

function THttpPeerCrypt.BearerDecode(
  const aBearerToken: RawUtf8; aExpected: THttpPeerCacheMessageKind;
  out aMsg: THttpPeerCacheMessage; aParams: PRawUtf8): THttpPeerCryptMessageDecode;
var
  tok: THttpPeerCacheMessageEncoded; // no memory allocation
  bearerlen: PtrInt;
begin
  result := mdBLen;
  bearerlen := length(aBearerToken) - PEER_CACHE_BEARERLEN;
  if bearerlen <> 0 then
    if bearerlen > 0 then
    begin
      result := mdBParam;
      if aBearerToken[PEER_CACHE_BEARERLEN + 1] <> '?' then
        exit;
      if aParams <> nil then // e.g. URI-encoded THttpRequestExtendedOptions
        FastSetString(aParams^,
          @PByteArray(aBearerToken)[PEER_CACHE_BEARERLEN + 1], bearerlen - 1);
    end
    else
      exit; // not enough input
  result := mdB64;
  if not Base64uriToBin(pointer(aBearerToken), @tok, PEER_CACHE_BEARERLEN, SizeOf(tok)) then
    exit;
  result := MessageDecode(@tok, SizeOf(tok), aMsg);
  if (result = mdOk) and
     (aExpected >= pcfBearer) and
     (aMsg.Kind <> aExpected) then
    result := mdBearer;
end;

procedure THttpPeerCrypt.LocalPeerClientSetup(const aIp: RawUtf8;
  aClient: THttpClientSocket; aRecvTimeout: integer);
var
  tls: boolean;
begin
  aClient.ResetExtendedOptions;
  if fVerboseLog then
    aClient.OnLog := fLog.DoLog;
  tls := true;
  if fClientTls.Enabled then
    aClient.TLS := fClientTls
  else if pcoSelfSignedHttps in fSettings.Options then
    aClient.TLS.IgnoreCertificateErrors := true // self-signed
  else
    tls := false;
  aClient.OpenBind(aIp, fPort, {bind=}false, tls); // try to connect
  aClient.ReceiveTimeout := aRecvTimeout; // socket timeout once connected
end;

function THttpPeerCrypt.LocalPeerRequest(const aRequest: THttpPeerCacheMessage;
  var aResp : THttpPeerCacheMessage; const aUrl: RawUtf8;
  aOutStream: TStreamRedirect; aRetry: boolean): integer;
var
  head, ip, method: RawUtf8;

  procedure LocalPeerRequestFailed(E: TClass);
  begin
    fLog.Add.Log(sllWarning, 'OnDownload: % %:% % failed as % %',
      [method, ip, fPort, aUrl, StatusCodeToText(result)^, E], self);
    if (fInstable <> nil) and // add to RejectInstablePeersMin list
       (E <> nil) and         // on OpenBind() error
       not aRetry then        // not from partial request before broadcast
      fInstable.BanIP(aResp.IP4); // this peer may have a HTTP firewall issue
    FreeAndNil(fClient);
    fClientIP4 := 0;
    result := 0; // will fallback to regular GET on the main repository
  end;

begin
  result := 0;
  try
    // compute the call parameters and the request bearer
    IP4Text(@aResp.IP4, ip);
    fLog.Add.Log(LOG_DEBUGERROR[ip = ''], 'OnDownload: request %:% %',
      [ip, fPort, aUrl], self);
   if ip = '' then
      exit; // clearly invalid (sllError logged above)
    aResp.Kind := pcfBearer; // authorize OnBeforeBody with response message
    MessageEncodeBearer(aResp, head);
    // ensure we have the right peer
    if (fClient <> nil) and
       ((fClientIP4 <> aResp.IP4) or
        not fClient.SockConnected) then
      FreeAndNil(fClient);
    // ensure we have the expected HTTP/HTTPS connection
    if fClient = nil then
    begin
      fClient := THttpClientSocket.Create(fSettings.HttpTimeoutMS);
      LocalPeerClientSetup(ip, fClient, 5000);
    end;
    // makes the HEAD/GET request, optionally with the needed range bytes
    fClient.RangeStart := aRequest.RangeStart;
    fClient.RangeEnd   := aRequest.RangeEnd;
    if aOutStream = nil then
      method := 'HEAD'
    else
    begin
      if fSettings.LimitMBPerSec >= 0 then // -1 to keep original value
        aOutStream.LimitPerSecond := fSettings.LimitMBPerSec shl 20; // bytes/sec
      method := 'GET';
    end;
    result := fClient.Request(
      aUrl, method, 30000, head, '',  '', aRetry, nil, aOutStream);
    fLog.Add.Log(sllTrace, 'OnDownload: request=%', [result], self);
    if result in HTTP_GET_OK then
      fClientIP4 := aResp.IP4 // success or not found (HTTP_NOCONTENT)
    else
      LocalPeerRequestFailed(nil); // error downloading from local peer
  except
    on E: Exception do
      LocalPeerRequestFailed(PClass(E)^);
  end;
end;

constructor THttpPeerCrypt.Create(const aSharedSecret: RawByteString;
  aServerTls, aClientTls: PNetTlsContext);
var
  key: THash256Rec;
begin
  // setup internal processing status
  fFrameSeqLow := Random31Not0; // 31-bit random start value set at startup
  fFrameSeq := fFrameSeqLow;
  // setup internal cryptography
  if aSharedSecret = '' then
    EHttpPeerCache.RaiseUtf8('%.Create without aSharedSecret', [self]);
  HmacSha256('4b0fb62af680447c9d0604fc74b908fa', aSharedSecret, key.b);
  // favor our TAesGcm for small messages, instead of OpenSSL TAesFast[mGCM]
  fAesEnc := TAesGcm.Create(key.Lo) as TAesGcmAbstract; // lower 128-bit
  fAesDec := fAesEnc.Clone as TAesGcmAbstract; // two AES-GCM-128 instances
  HmacSha256(key.b, '2b6f48c3ffe847b9beb6d8de602c9f25', key.b); // paranoid
  fSharedMagic := key.h.c3; // 32-bit derivation for anti-fuzzing checksum
  FastSetString(fDirectSecret, @key, SizeOf(key)); // for HttpDirectUri()
  if Assigned(fLog) then
    // log includes safe 16-bit key.w[0] fingerprint
    fLog.Add.Log(sllTrace, 'Create: Uuid=% SecretFingerPrint=%, Seq=#%',
      [UuidToShort(fUuid), key.w[0], CardinalToHexShort(fFrameSeq)], self);
  FillZero(key.b);
  if aServerTls <> nil then
    fServerTls := aServerTls^;
  if aClientTls <> nil then
    fClientTls := aClientTls^;
end;

destructor THttpPeerCrypt.Destroy;
begin
  FreeAndNilSafe(fClient);
  FreeAndNil(fInstable);
  FreeAndNil(fAesEnc);
  FreeAndNil(fAesDec);
  fSharedMagic := 0;
  inherited Destroy;
  FillZero(fDirectSecret);
end;

function THttpPeerCrypt.NetworkInterfaceChanged: boolean;
var
  newmac: TMacAddress;
  err: RawUtf8;
  tix: cardinal;
begin
  result := false;
  if self = nil then
    exit;
  tix := GetTickSec;
  if tix = fLastNetworkTix then
    exit;
  fLastNetworkTix := tix;
  MacIPAddressFlush; // thread-safe flush mormot.net.sock cache
  err := fSettings.GuessInterface(newmac);
  result := (err = '') and
            ((fMac.Name <> newmac.Name) or
             (fMac.IP <> newmac.IP) or
             (fMac.Broadcast <> newmac.Broadcast) or
             (fMac.NetMask <> newmac.NetMask));
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'NetworkInterfaceChanged=% [% % % %] %',
      [BOOL_STR[result], newmac.Name, newmac.IP, newmac.Broadcast, newmac.NetMask, err], self);
end;

const // URI start for pcfBearerDirect/pcfBearerDirectPermanent peer requests
  DIRECTURI_32 = ord('/') + ord('h') shl 8 + ord('t') shl 16 + ord('t') shl 24;

class function THttpPeerCrypt.HttpDirectUri(const aSharedSecret: RawByteString;
  const aRemoteUri, aRemoteHash: RawUtf8;
  out aDirectUri, aDirectHeaderBearer: RawUtf8; aPermanent: boolean;
  aOptions: PHttpRequestExtendedOptions): boolean;
var
  c: THttpPeerCrypt;
  msg: THttpPeerCacheMessage;
  p: RawUtf8;
  uri: TUri;
begin
  result := false;
  if (aSharedSecret = '') or
     (aRemoteHash = '') or
     not uri.From(aRemoteUri) then
    exit;
  c := THttpPeerCrypt.Create(aSharedSecret, nil, nil);
  try
    c.MessageInit(pcfBearerDirect, 0, msg);
    if aPermanent then
      msg.Kind := pcfBearerDirectPermanent;
    if not HashDetect(aRemoteHash, msg.Hash) then
      exit;
    if uri.Port <> DEFAULT_PORT[uri.Https] then
      Join(['_', uri.Port], p); // '_' is ok for URI, but not for domain
    FormatUtf8('/%/%%/%', [uri.Scheme, uri.Server, p, uri.Address], aDirectUri);
    msg.Opaque := crc63c(pointer(aDirectUri), length(aDirectUri)); // no replay
    c.MessageEncodeBearer(msg, aDirectHeaderBearer);
    if aOptions <> nil then // extended options are URI-encoded to the bearer
      aDirectHeaderBearer :=
        aOptions^.ToUrlEncode(aDirectHeaderBearer, c.fDirectSecret);
    result := true;
  finally
    c.Free;
  end;
end;

class function THttpPeerCrypt.HttpDirectUriReconstruct(P: PUtf8Char;
  out Decoded: TUri): boolean;
var
  scheme, domain: RawUtf8;
begin
  result := false;
  if (P = nil) or
     (PCardinal(P)^ <> DIRECTURI_32) then
    exit;
  inc(P); // http/... or https/...
  GetNextItem(P, '/', scheme);
  GetNextItem(P, '/', domain); // domain/... or domain_port/...
  domain := StringReplaceChars(domain, '_', ':');
  if (domain <> '') and
     (P <> nil) then
    result := Decoded.From(Make([scheme, '://', domain, '/', P]));
end;


{ THttpPeerCacheSettings }

constructor THttpPeerCacheSettings.Create;
begin
  inherited Create;
  fPort := 8099;
  fLimitMBPerSec := 10;
  fLimitClientCount := 32;
  fRejectInstablePeersMin := 4;
  fCacheTempPath := '*';
  fCacheTempMinBytes := 2048;
  fCacheTempMaxMB := 1000;
  fCacheTempMaxMin := 60;
  fCachePermPath := '*';
  fCachePermMinBytes := 2048;
  fBroadcastTimeoutMS := 10;
  fBroadcastMaxResponses := 24;
  fBroadCastDirectMinBytes := 64 shl 10;
  fTryAllPeersCount := 10;
  fHttpTimeoutMS := 500;
end;

function THttpPeerCacheSettings.GuessInterface(out Mac: TMacAddress): RawUtf8;
begin
  result := '';
  if fInterfaceName <> '' then
  begin
    if not GetMainMacAddress(Mac, fInterfaceName, {UpAndDown=}true) then
      // allow to pickup "down" interfaces if name is explicit
      result := FormatUtf8('impossible to find the [%] network interface',
        [fInterfaceName]);
  end
  else if not GetMainMacAddress(Mac, [mafLocalOnly, mafRequireBroadcast]) then
    result := 'impossible to find a local network interface';
end;

function THttpPeerCacheSettings.HttpDirectUri(
  const aSharedSecret: RawByteString; const aRemoteUri, aRemoteHash: RawUtf8;
  out aDirectUri, aDirectHeaderBearer: RawUtf8; aForceTls, aPermanent: boolean;
  aOptions: PHttpRequestExtendedOptions): boolean;
var
  mac: TMacAddress;
begin
  result := false;
  if (self = nil) or
     (aSharedSecret = '') or
     (pcoNoServer in fOptions) or
     (GuessInterface(mac) <> '') or
     not THttpPeerCrypt.HttpDirectUri(aSharedSecret, aRemoteUri, aRemoteHash,
           aDirectUri, aDirectHeaderBearer, aPermanent, aOptions) then
    exit;
  aForceTls := aForceTls or (pcoSelfSignedHttps in fOptions);
  aDirectUri := Make([HTTPS_TEXT[aForceTls], mac.IP, ':', fPort, aDirectUri]);
  result := true;
end;


{ THttpPeerCacheThread }

constructor THttpPeerCacheThread.Create(Owner: THttpPeerCache);
begin
  fOwner := Owner;
  fBroadcastSafe.Init;
  fBroadcastAddr.SetIP4Port(fOwner.fBroadcastIP4, fOwner.Settings.Port);
  fBroadcastAddr.IPWithPort(fBroadcastIpPort);
  fBroadcastEvent := TSynEvent.Create;
  // POSIX requires to bind to the broadcast address to receive brodcasted frames
  inherited Create(fOwner.fLog,
    fOwner.fMac.{$ifdef OSPOSIX}Broadcast{$else}IP{$endif}, // OS-specific
    fOwner.fPort, 'udp-PeerCache', 100);
end;

destructor THttpPeerCacheThread.Destroy;
begin
  if Assigned(fBroadcastEvent) and
     (fBroadcastCurrentSeq <> 0) then
    fBroadcastEvent.SetEvent;
  inherited Destroy;
  fBroadcastEvent.Free;
  fBroadcastSafe.Done;
end;

const
  _LATE: array[boolean] of string[7] = ('', 'late ');

procedure THttpPeerCacheThread.OnFrameReceived(len: integer;
  var remote: TNetAddr);
var
  msg, resp: THttpPeerCacheMessage;

  procedure DoLog(const Fmt: RawUtf8; const Args: array of const);
  var
    txt: ShortString;
    us: TShort16;
    stop: Int64;
  begin
    if not fOwner.fLog.HasLevel([sllTrace]) then
      exit;
    FormatShort(Fmt, Args, txt);
    us[0] := #0;
    if (fOwner.fBroadcastStart <> 0) and
       (msg.Kind in PCF_RESPONSE) then
    begin
      QueryPerformanceMicroSeconds(stop);
      dec(stop, fOwner.fBroadcastStart);
      if stop > 0 then
        MicroSecToString(stop, us);
    end;
    fOwner.fLog.Add.Log(sllTrace, 'OnFrameReceived: % % %',
      [remote.IP4Short, txt, us], self);
  end;

  procedure DoSendResponse;
  var
    res: TNetResult;
    sock: TNetSocket;
    frame: THttpPeerCacheMessageEncoded;
  begin
    // compute PCF_RESPONSE frame
    resp.DestIP4 := remote.IP4; // notify actual source IP (over broadcast)
    fOwner.MessageEncode(resp, frame);
    // respond on main UDP port and on broadcast (POSIX) or local (Windows) IP
    if msg.Os.os = osWindows then
    begin // local IP is good enough on Windows
      remote.SetPort(fBroadcastAddr.Port);
      sock := remote.NewSocket(nlUdp);
    end
    else
    begin // need to broadcast
      remote.SetIP4Port(fOwner.fBroadcastIP4, fBroadcastAddr.Port);
      sock := remote.NewSocket(nlUdp);
      sock.SetBroadcast(true);
    end;
    res := sock.SendTo(@frame, SizeOf(frame), remote);
    sock.Close;
    if fOwner.fVerboseLog then
      DoLog('send %=%', [ToText(resp.Kind)^, _NR[res]]);
    inc(fSent);
  end;

var
  ok: THttpPeerCryptMessageDecode;
  late: boolean;
begin
  // quick return if this frame is not worth decoding
  if (fOwner = nil) or
     (fOwner.fSettings = nil) or     // avoid random GPF at shutdown
     (remote.IP4 = 0) or
     (remote.IP4 = fOwner.fIP4) then // Windows broadcasts to self :)
    exit;
  // RejectInstablePeersMin option: validate the input frame IP
  PByte(@msg.Kind)^ := 255; // ToText(msg.Kind)^ = ''
  if fOwner.fInstable.IsBanned(remote) then
  begin
    if fOwner.fVerboseLog then
      DoLog('banned /%', [fOwner.fInstable.Count]);
    exit;
  end;
  // validate the input frame content
  ok := fOwner.MessageDecode(pointer(fFrame), len, msg);
  if fOwner.Check(ok, 'OnFrameReceived', msg) then
    if (msg.Kind in PCF_RESPONSE) and    // responses are broadcasted on POSIX
       (msg.DestIP4 <> fOwner.fIP4) then // will also detect any unexpected NAT
     begin
       if fOwner.fVerboseLog then
         DoLog('ignored % %<>%', [ToText(msg.Kind)^,
           IP4ToShort(@msg.DestIP4), fOwner.fIpPort]);
       exit;
     end;
  late := (msg.Kind in PCF_RESPONSE) and
          (msg.Seq <> fBroadcastCurrentSeq);
  if fOwner.fVerboseLog then
    if ok = mdOk then
      DoLog('%%', [_LATE[late], ToText(msg)])
    else if ok <= mdAes then // decoding error
      DoLog('unexpected % len=% [%]',
        [ToText(ok)^, len, EscapeToShort(pointer(fFrame), len)]);
  // process the frame message
  if ok = mdOk then
    case msg.Kind of
      pcfPing:
        begin
          fOwner.MessageInit(pcfPong, msg.Seq, resp);
          DoSendResponse;
        end;
      pcfRequest:
        begin
          fOwner.MessageInit(pcfResponseNone, msg.Seq, resp);
          resp.Hash := msg.Hash;
          if not (pcoNoServer in fOwner.Settings.Options) then
            if integer(fOwner.fHttpServer.ConnectionsActive) >
                        fOwner.Settings.LimitClientCount then
              resp.Kind := pcfResponseOverloaded
            else if fOwner.LocalFileName(
                             msg, [], nil, @resp.Size) = HTTP_SUCCESS then
              resp.Kind := pcfResponseFull
            else if fOwner.PartialFileName(
                             msg, nil, nil, @resp.Size) = HTTP_SUCCESS then
              resp.Kind := pcfResponsePartial;
          DoSendResponse;
        end;
      pcfPong,
      pcfResponsePartial,
      pcfResponseFull:
        if not late then
        begin
          inc(fResponses);
          if AddResponseAndDone(msg) then
          begin
            fBroadcastCurrentSeq := 0;   // ignore next responses
            fBroadcastEvent.SetEvent;    // notify MessageBroadcast
          end;
        end;
      pcfResponseNone,
      pcfResponseOverloaded:
        if not late then
          inc(fResponses);
    end
  else // not ok = this UDP packet is invalid
    if fOwner.fInstable <> nil then // RejectInstablePeersMin
      if fOwner.fInstable.BanIP(remote.IP4) then
        DoLog('ban IP after %', [ToText(ok)^]);
end;

function SortMessagePerPriority(const VA, VB): integer;
var
  a: THttpPeerCacheMessage absolute VA;
  b: THttpPeerCacheMessage absolute VB;
begin
  result := CompareCardinal(ord(b.Kind), ord(a.Kind));
  if result <> 0 then // pcfResponseFull first
    exit;
  result := CompareCardinal(NETHW_ORDER[a.Hardware], NETHW_ORDER[b.Hardware]);
  if result <> 0 then // ethernet first
    exit;
  result := CompareCardinal(b.Speed, a.Speed);
  if result <> 0 then // highest speed first
    exit;
  result := CompareCardinal(a.Connections, b.Connections);
  if result <> 0 then // less active
    exit;
  result := ComparePointer(@a, @b); // by pointer in a dynamic array = received first
end;

function THttpPeerCacheThread.Broadcast(const aReq: THttpPeerCacheMessage;
  out aAlone: boolean): THttpPeerCacheMessageDynArray;
var
  sock: TNetSocket;
  res: TNetResult;
  frame: THttpPeerCacheMessageEncoded;
  start, stop: Int64;
begin
  result := nil;
  if self = nil then
    exit;
  QueryPerformanceMicroSeconds(start);
  fOwner.MessageEncode(aReq, frame);
  fBroadcastSafe.Lock; // serialize OnDownload() or Ping() calls
  try
    // setup this broadcasting sequence
    QueryPerformanceMicroSeconds(fOwner.fBroadcastStart);
    fBroadcastEvent.ResetEvent;
    fBroadcastCurrentSeq := aReq.Seq; // ignore any other responses
    fResponses := 0; // reset counter for this broadcast (not late)
    // broadcast request over the UDP sub-net of the selected network interface
    sock := fBroadcastAddr.NewSocket(nlUdp);
    if sock = nil then
      exit;
    try
      sock.SetBroadcast(true);
      res := sock.SendTo(@frame, SizeOf(frame), fBroadcastAddr);
      if fOwner.fVerboseLog then
        fOwner.fLog.Add.Log(sllTrace, 'Broadcast: % % = %',
          [fBroadcastIpPort, ToText(aReq), _NR[res]], self);
      if res <> nrOk then
        exit;
    finally
      sock.Close;
    end;
    // wait for the (first) response(s)
    fBroadcastEvent.WaitFor(fOwner.Settings.BroadcastTimeoutMS);
    result := GetResponses(aReq.Seq);
  finally
    fBroadcastCurrentSeq := 0; // ignore any late responses
    fOwner.fBroadcastStart := 0;
    aAlone := (fResponses = 0);
    fBroadcastSafe.UnLock;
  end;
  // select the best responses
  if length(result) > 1 then
    DynArray(TypeInfo(THttpPeerCacheMessageDynArray), result).
      Sort(SortMessagePerPriority);
  QueryPerformanceMicroSeconds(stop);
  fOwner.fLog.Add.Log(sllTrace, 'Broadcast: %=%/% in %', [ToText(aReq.Kind)^,
    length(result), fResponses, MicroSecToString(stop - start)], self);
end;

function THttpPeerCacheThread.AddResponseAndDone(
  const aMessage: THttpPeerCacheMessage): boolean;
begin
  if fRespCount < fOwner.Settings.BroadcastMaxResponses then
  begin
    fRespSafe.Lock;
    try
      if fRespCount = length(fResp) then
        SetLength(fResp, NextGrow(fRespCount));
      fResp[fRespCount] := aMessage;
      inc(fRespCount);
    finally
      fRespSafe.UnLock;
    end;
  end;
  result := (pcoUseFirstResponse in fOwner.Settings.Options) or
            (fRespCount >= fOwner.Settings.BroadcastMaxResponses);
end;

function THttpPeerCacheThread.GetResponses(
  aSeq: cardinal): THttpPeerCacheMessageDynArray;
var
  i, c, n: PtrInt;
begin
  result := nil;
  // retrieve the pending responses
  fBroadcastCurrentSeq := 0; // no more reponse from now on
  fRespSafe.Lock;
  try
    if fRespCount = 0 then
      exit;
    pointer(result) := pointer(fResp); // assign with no refcount
    pointer(fResp) := nil;
    c := fRespCount;
    fRespCount := 0;
  finally
    fRespSafe.UnLock;
  end;
  // filter the responses matching aSeq (paranoid)
  n := 0;
  for i := 0 to c - 1 do
    if result[i].Seq = aSeq then
    begin
      if i <> n then
        result[n] := result[i];
      inc(n);
    end;
  if n = 0 then
    result := nil
  else
    DynArrayFakeLength(result, n);
end;

procedure THttpPeerCacheThread.OnIdle(tix64: Int64);
begin
  fOwner.OnIdle(tix64); // do nothing but once every minute
end;

procedure THttpPeerCacheThread.OnShutdown;
begin
  // nothing to be done in our case
end;


{ THttpPeerCache }

constructor THttpPeerCache.Create(aSettings: THttpPeerCacheSettings;
  const aSharedSecret: RawByteString;
  aHttpServerClass: THttpServerSocketGenericClass;
  aHttpServerThreadCount: integer; aLogClass: TSynLogClass;
  aServerTls, aClientTls: PNetTlsContext);
var
  ilog: ISynLog;
  avail, existing: Int64;
begin
  fLog := aLogClass;
  if fLog = nil then
    fLog := TSynLog;
  fLog.EnterLocal(ilog, 'Create threads=% %', [aHttpServerThreadCount, aLogClass], self);
  fFilesSafe.Init;
  // intialize the cryptographic state in inherited THttpPeerCrypt.Create
  if (fSettings = nil) or
     (fSettings.Uuid = '') then // allow UUID customization
    GetComputerUuid(fUuid)
  else if not RawUtf8ToGuid(fSettings.Uuid, fUuid) then
    EHttpPeerCache.RaiseUtf8('Invalid %.Create(uuid=%)', [self, fSettings.Uuid]);
  inherited Create(aSharedSecret, aServerTls, aClientTls);
  // setup the processing options
  if aSettings = nil then
  begin
    fSettings := THttpPeerCacheSettings.Create;
    fSettingsOwned := true;
  end
  else
    fSettings := aSettings;
  fVerboseLog := (pcoVerboseLog in fSettings.Options) and
                 fLog.HasLevel([sllTrace]);
  // check the temporary files cache folder and its maximum allowed size
  if fSettings.CacheTempPath = '*' then // not customized
    fSettings.CacheTempPath := TemporaryFileName;
  fTempCurrentSize := -1; // will force compute current folder size
  fTempFilesPath := EnsureDirectoryExists(fSettings.CacheTempPath);
  if fTempFilesPath <> '' then
  begin
    OnIdle(0); // initial clean-up
    fTempFilesMaxSize := Int64(fSettings.CacheTempMaxMB) shl 20;
    avail := GetDiskAvailable(fTempFilesPath);
    existing := DirectorySize(fTempFilesPath, false, PEER_CACHE_PATTERN);
    if Assigned(ilog) then
      ilog.Log(sllDebug, 'Create: % folder has % available, with % existing cache',
        [fTempFilesPath, KB(avail), KB(existing)], self);
    if (avail <> 0) and
       not (pcoCacheTempNoCheckSize in fSettings.Options) then
    begin
      avail := (avail + existing) shr 2;
      if fTempFilesMaxSize > avail then
      begin
        fTempFilesMaxSize := avail; // allow up to 25% of the folder capacity
        if Assigned(ilog) then
          ilog.Log(sllDebug, 'Create: trim CacheTempMax=%', [KB(avail)], self);
      end;
    end;
  end;
  // ensure we have somewhere to cache
  if fSettings.CachePermPath = '*' then // not customized
    fSettings.CachePermPath := MakePath(
      [GetSystemPath(spCommonData), Executable.ProgramName, 'permcache']);
  fPermFilesPath := EnsureDirectoryExists(fSettings.CachePermPath);
  if (fTempFilesPath = '') and
     (fPermFilesPath = '') then
    EHttpPeerCache.RaiseUtf8('%.Create: no cache defined', [self]);
  // retrieve the local network interface (in inherited THttpPeerCrypt)
  AfterSettings; // fSettings should have been defined
  // start the local UDP server on this interface
  fUdpServer := THttpPeerCacheThread.Create(self);
  if Assigned(ilog) then
    ilog.Log(sllTrace, 'Create: started %', [fUdpServer], self);
  // start the local HTTP/HTTPS server on this interface
  if not (pcoNoServer in fSettings.Options) then
  begin
    StartHttpServer(aHttpServerClass, aHttpServerThreadCount, fIpPort);
    fHttpServer.ServerName := Executable.ProgramName;
    fHttpServer.OnBeforeBody := OnBeforeBody;
    fHttpServer.OnRequest := OnRequest;
    if Assigned(ilog) then
      ilog.Log(sllDebug, 'Create: started %', [fHttpServer], self);
  end;
end;

procedure THttpPeerCache.StartHttpServer(
  aHttpServerClass: THttpServerSocketGenericClass;
  aHttpServerThreadCount: integer; const aIP: RawUtf8);
var
  opt: THttpServerOptions;
  srv: THttpServerSocketGeneric;
begin
  if fClientTls.Enabled <> fServerTls.Enabled then
    EHttpPeerCache.RaiseUtf8(
      '%.StartHttpServer: inconsistent ClientTls=% ServerTls=%',
      [self, fClientTls.Enabled, fServerTls.Enabled]);
  if aHttpServerClass = nil then
    aHttpServerClass := THttpServer; // classic per-client thread server is enough
  opt := [hsoNoXPoweredHeader, hsoThreadSmooting];
  if not (pcoNoBanIP in fSettings.Options) then // RejectInstablePeersMin = UDP
    include(opt, hsoBan40xIP);
  if fVerboseLog then
    include(opt, hsoLogVerbose);
  if fServerTls.Enabled or
     (pcoSelfSignedHttps in fSettings.Options) then
    include(opt, hsoEnableTls);
  fHttpServer := aHttpServerClass.Create(aIP, nil, nil, 'PeerCache',
    aHttpServerThreadCount, 30000, opt, fLog);
  if aHttpServerClass.InheritsFrom(THttpServerSocketGeneric) then
  begin
    srv := fHttpServer as THttpServerSocketGeneric;
    // no TCP/HTTP ban from localhost
    if Assigned(srv.Banned) then
      srv.Banned.WhiteIP := fIP4;
    // note: both THttpServer and THttpAsyncServer support rfProgressiveStatic
    fPartials := THttpPartials.Create;
    if fVerboseLog then
      fPartials.OnLog := fLog.DoLog;
    srv.fProgressiveRequests := fPartials;
    // actually start and wait for the local HTTP(S) server to be available
    if fServerTls.Enabled then
    begin
      fLog.Add.Log(sllTrace, 'StartHttpServer: HTTPS from ServerTls using %',
        [fServerTls.PrivateKeyFile], self);
      srv.WaitStarted(10, @fServerTls);
    end
    else if pcoSelfSignedHttps in fSettings.Options then
    begin
      fLog.Add.Log(sllTrace, 'StartHttpServer: self-signed HTTPS', self);
      srv.WaitStartedHttps(10);
    end
    else
    begin
      fLog.Add.Log(sllTrace, 'StartHttpServer: plain HTTP', self);
      srv.WaitStarted(10);
    end;
  end;
end;

function THttpPeerCache.CurrentConnections: integer;
begin
  if Assigned(fSettings) and
     (pcoNoServer in fSettings.Options) then
    result := 0
  else
    result := fHttpServer.ConnectionsActive;
end;

destructor THttpPeerCache.Destroy;
var
  ilog: ISynLog;
begin
  fLog.EnterLocal(ilog, 'Destroy % %', [fUdpServer, fHttpServer], self);
  if fSettingsOwned then
    fSettings.Free;
  fSettings := nil; // notify OnDownload/OnIdle/OnFrameReceived calls
  FreeAndNil(fUdpServer);
  FreeAndNil(fHttpServer);
  FreeAndNil(fPartials);
  fFilesSafe.Done;
  inherited Destroy;
end;

function THttpPeerCache.Check(Status: THttpPeerCryptMessageDecode;
  const Ctxt: ShortString; const Msg: THttpPeerCacheMessage): boolean;
var
  msgtxt: ShortString;
begin
  result := (Status = mdOk);
  if result or
     not fLog.HasLevel([sllTrace]) then
    exit;
  msgtxt[0] := #0;
  if Status > mdAes then // decrypt ok but wrong content: log msg
    MsgToShort(Msg, msgtxt);
  fLog.Add.Log(sllTrace, '%: decode=% %', [Ctxt, ToText(Status)^, msgtxt], self);
end;

function THttpPeerCache.State: TWGetAlternateState;
begin
  result := [];
  if (self = nil) or
     not Assigned(fSettings) then // nil at shutdown
    exit;
  if Assigned(fHttpServer) and
     (fHttpServer.fCurrentProcess > 0) then
    include(result, gasProcessing);
  if not fPartials.IsVoid then
    include(result, gasPartials);
end;

function THttpPeerCache.ComputeFileName(const aHash: THashDigest): TFileName;
begin
  // filename is binary algo + hash encoded as hexadecimal up to 520-bit
  result := FormatString('%.cache',
    [BinToHexLower(@aHash, SizeOf(aHash.Algo) + HASH_SIZE[aHash.Algo])]);
  // note: it does not make sense to obfuscate this file name because the file
  // content itself is unencrypted on disk, so the hash can be recomputed anyway
end;

function THttpPeerCache.PermFileName(const aFileName: TFileName;
  aFlags: THttpPeerCacheLocalFileName): TFileName;
begin
  if Assigned(fSettings) and
     (pcoCacheTempSubFolders in fSettings.Options) then
  begin
    // create sub-folders using the first hash nibble (0..9/a..z), in a way
    // similar to git - aFileName[1..2] is the algorithm, so hash starts at [3]
    result := MakePath([fPermFilesPath, aFileName[3]]);
    if lfnEnsureDirectoryExists in aFlags then
      result := EnsureDirectoryExistsNoExpand(result);
    result := result + aFileName;
  end
  else
    result := fPermFilesPath + aFileName;
end;

function THttpPeerCache.LocalFileName(const aMessage: THttpPeerCacheMessage;
  aFlags: THttpPeerCacheLocalFileName; aFileName: PFileName;
  aSize: PInt64): integer;
var
  perm, temp, name, fn: TFileName;
  size: Int64;
begin
  name := ComputeFileName(aMessage.Hash);
  if fPermFilesPath <> '' then
    perm := PermFileName(name, aFlags); // with pcoCacheTempSubFolders support
  if fTempFilesPath <> '' then
    temp := fTempFilesPath + name;
  fFilesSafe.Lock; // disable any concurrent file access
  try
    size := FileSize(perm); // fast syscall on all platforms
    if size <> 0 then // found in permanent cache folder
      fn := perm
    else
    begin
      size := FileSize(temp);
      if size <> 0 then // found in temporary cache folder
      begin
        fn := temp;
        if lfnSetDate in aFlags then
          FileSetDateFromUnixUtc(temp, UnixTimeUtc); // renew TTL
      end;
    end;
  finally
    fFilesSafe.UnLock;
  end;
  if fVerboseLog then
    if fn = '' then
      fLog.Add.Log(sllTrace, 'LocalFileName: none', self)
    else
      fLog.Add.Log(sllTrace, 'LocalFileName: % size=% msg: size=% start=% end=%',
        [fn, size, aMessage.Size, aMessage.RangeStart, aMessage.RangeEnd], self);
  result := HTTP_NOTFOUND;
  if size = 0 then
    exit; // not existing
  result := HTTP_NOTACCEPTABLE; // 406
  if (aMessage.Size <> 0) and
     // ExpectedSize may be 0 if waoNoHeadFirst was set, or for pcfBearerDirect*
     (size <> aMessage.Size) then
    exit; // invalid file
  result := HTTP_SUCCESS;
  if aFileName <> nil then
    aFileName^ := fn;
  if aSize <> nil then
    aSize^ := size;
end;

function WGetToHash(const Params: THttpClientSocketWGet;
  out Hash: THashDigest): boolean;
begin
  result := (Params.Hash <> '') and
            (Params.Hasher <> nil) and
            Params.Hasher.InheritsFrom(TStreamRedirectSynHasher) and
       TStreamRedirectSynHasher(Params.Hasher).GetHashDigest(Params.Hash, Hash);
end;

function THttpPeerCache.CachedFileName(const aParams: THttpClientSocketWGet;
  aFlags: THttpPeerCacheLocalFileName;
  out aLocal: TFileName; out isTemp: boolean): boolean;
var
  hash: THashDigest;
begin
  if not WGetToHash(aParams, hash) then
  begin
    result := false;
    exit;
  end;
  aLocal := ComputeFileName(hash);
  isTemp := (fPermFilesPath = '') or
            not (waoPermanentCache in aParams.AlternateOptions);
  if isTemp then
    aLocal := fTempFilesPath + aLocal
  else
    aLocal := PermFileName(aLocal, aFlags); // with sub-folder
  result := true;
end;

function THttpPeerCache.TempFolderEstimateNewSize(aAddingSize: Int64): Int64;
var
  i: integer;
  deleted: Int64;
  dir: TFindFilesDynArray;
  d: PFindFiles;
  fn: TFileName;
begin
  // check if the file is clearly too big to be cached
  result := aAddingSize;
  if result > fTempFilesMaxSize then
    exit;
  // compute the current folder cache size
  result := fTempCurrentSize;
  if result < 0 then // first time, or after OnIdle DirectoryDeleteOlderFiles()
  begin
    dir := FindFiles(fTempFilesPath, PEER_CACHE_PATTERN, '', [ffoExcludesDir]);
    result := FindFilesSize(dir);
    fTempCurrentSize := result; // may be 0
  end;
  // enough space to write this file?
  inc(result, aAddingSize); // simulate adding this file
  if result < fTempFilesMaxSize then
    exit;
  // delete oldest files in cache up to CacheTempMaxMB
  if dir = nil then
    dir := FindFiles(fTempFilesPath, PEER_CACHE_PATTERN, '', [ffoExcludesDir]);
  FindFilesSortByTimestamp(dir); // only sort if needed
  deleted := 0;
  d := pointer(dir);
  for i := 1 to length(dir) do
  begin
    fn := fTempFilesPath + d^.Name;
    if not fPartials.HasFile(fn) then // if not currently downloading
      if DeleteFile(fn) then
      begin
        dec(result, d^.Size);
        inc(deleted, d^.Size);
        if result < fTempFilesMaxSize then
          break; // we have deleted enough old files
      end;
    inc(d);
  end;
  fLog.Add.Log(sllTrace, 'OnDownloaded: deleted %', [KB(deleted)], self);
  dec(fTempCurrentSize, deleted);
end;

function THttpPeerCache.TooSmallFile(const aParams: THttpClientSocketWGet;
  aSize: Int64; const aCaller: ShortString): boolean;
var
  minsize: Int64;
begin
  result := false; // continue
  if (fSettings = nil) or
     (waoNoMinimalSize in aParams.AlternateOptions) then
    exit;
  if (waoPermanentCache in aParams.AlternateOptions) and
     (fPermFilesPath <> '') then
    minsize := fSettings.CachePermMinBytes
  else
    minsize := fSettings.CacheTempMinBytes;
  if aSize >= minsize then
    exit; // big enough
  if fVerboseLog then
    fLog.Add.Log(sllTrace, '%: size < minsize=%', [aCaller, KB(minsize)], self);
  result := true; // too small
end;

function THttpPeerCache.OnDownload(Sender: THttpClientSocket;
  var Params: THttpClientSocketWGet; const Url: RawUtf8;
  ExpectedFullSize: Int64; OutStream: TStreamRedirect): integer;
var
  req: THttpPeerCacheMessage;
  resp : THttpPeerCacheMessageDynArray;
  fn: TFileName;
  u: RawUtf8;
  local: TFileStreamEx;
  i: PtrInt;
  tix: cardinal;
  outStreamInitialPos: Int64;
  alone: boolean;
  log: ISynLog;
  l: TSynLog;

  function ResetOutStreamPosition: boolean;
  begin
    // not moved (e.g. first request, or connection issue) returns true
    result := (fSettings <> nil) and // nil at shutdown
              (OutStream.Position = outStreamInitialPos);
    if not result then
      // TStreamRedirect requires full rewind for full content re-hash
      if outStreamInitialPos = 0 then
        result := OutStream.Seek(0, soBeginning) = 0; // will call ReHash
      // TODO: fix range support - TStreamRedirect.Seek() Rehash after Append()
  end;

begin
  result := 0;
  // validate WGet caller context
  if (self = nil) or
     (fSettings = nil) or // nil at shutdown
     (Sender = nil) or
     (Params.Hash = '') or
     (Url = '') or
     (OutStream = nil) then
    exit;
  if not Params.Hasher.InheritsFrom(TStreamRedirectSynHasher) then
    EHttpPeerCache.RaiseUtf8('%.OnDownload: unexpected %', [Params.Hasher]);
  outStreamInitialPos := OutStream.Position;
  // prepare a request frame
  l := nil;
  fLog.EnterLocal(log, 'OnDownload % % % %', [KBNoSpace(ExpectedFullSize),
    Params.Hasher.GetHashName, Params.Hash, Url], self);
  if Assigned(log) then // log=nil if fLog=nil or sllEnter is not enabled
    l := log.Instance;
  MessageInit(pcfRequest, 0, req);
  if not WGetToHash(Params, req.Hash) then
  begin
    l.Log(sllWarning, 'OnDownload: invalid hash=%', [Params.Hash], self);
    exit;
  end;
  req.Size := ExpectedFullSize; // may be 0 if waoNoHeadFirst
  req.RangeStart := Sender.RangeStart;
  req.RangeEnd := Sender.RangeEnd;
  // always check if we don't already have this file cached locally
  if not (pcoNoServer in fSettings.Options) and
     (LocalFileName(req, [lfnSetDate], @fn, @req.Size) = HTTP_SUCCESS) then
  begin
    l.Log(sllDebug, 'OnDownload: from local %', [fn], self);
    local := TFileStreamEx.Create(fn, fmOpenReadShared);
    try
      // range support
      if req.RangeStart > 0 then
        req.RangeStart := local.Seek(req.RangeStart, soBeginning);
      if (req.RangeEnd <= 0) or
         (req.RangeEnd >= req.Size) then
        req.RangeEnd := req.Size - 1;
      req.Size := req.RangeEnd - req.RangeStart + 1;
      // fetch the data
      if req.Size > 0 then
      begin
        OutStream.LimitPerSecond := 0; // not relevant within the same process
        OutStream.CopyFrom(local, req.Size);
      end;
    finally
      local.Free;
    end;
    if req.RangeStart > 0 then
      result := HTTP_PARTIALCONTENT
    else
      result := HTTP_SUCCESS;
    Params.SetStep(wgsAlternateFromCache, [fn]);
    exit;
  end;
  // ensure the file is big enough for broadcasting
  if (ExpectedFullSize <> 0) and
     TooSmallFile(Params, ExpectedFullSize, 'OnDownload') then
    exit; // you are too small, buddy
  // try first the current/last HTTP peer client (if any)
  FormatUtf8('?%=%', [Sender.Server, Url], u); // just <> DIRECTURI_32 (for log)
  if (fClient <> nil) and
     fClient.SockConnected and
     (fClientIP4 <> 0) and
     ((pcoTryLastPeer in fSettings.Options) or
      (waoTryLastPeer in Params.AlternateOptions)) and
     fClientSafe.TryLock then
    try
      SetLength(resp, 1); // create a "fake" response to reuse this connection
      resp[0] := req;
      FillZero(resp[0].Uuid);
      // OnRequest() returns HTTP_NOCONTENT (204) - and not 404 - if not found
      result := LocalPeerRequest(req, resp[0], u, OutStream, {aRetry=}true);
      if result in [HTTP_SUCCESS, HTTP_PARTIALCONTENT] then
      begin
        Params.SetStep(wgsAlternateLastPeer, [fClient.Server]);
        exit; // successful direct downloading from last peer
      end;
      result := 0; // may be HTTP_NOCONTENT (204) if not found on this peer
    finally
      fClientSafe.UnLock;
    end;
  // broadcast the request over UDP
  if not Assigned(fSettings) then
    exit;
  tix := 0;
  if (pcoBroadcastNotAlone in fSettings.Options) or
     (waoBroadcastNotAlone in Params.AlternateOptions) then
  begin
    tix := GetTickSec + 1;
    if fBroadcastTix = tix then  // disable broadcasting within up to 1s delay
      exit;
  end;
  Params.SetStep(wgsAlternateBroadcast, [fUdpServer.fBroadcastIpPort]);
  resp := fUdpServer.Broadcast(req, alone);
  if (resp = nil) or
     not Assigned(fSettings) then
  begin
    if (tix <> 0) and // pcoBroadcastNotAlone
       alone then
      fBroadcastTix := tix; // no broadcast within the next second
    exit; // no match
  end;
  fBroadcastTix := 0; // resp<>nil -> broadcasting seems fine
  // HTTP/HTTPS request over the best peer corresponding to this response
  Params.SetStep(wgsAlternateGet, [IP4ToShort(@resp[0].IP4)]);
  if not ResetOutStreamPosition then
    exit; // partial download would fail the hash anyway
  fClientSafe.Lock;
  try
    result := LocalPeerRequest(req, resp[0], u, OutStream, {aRetry=}false);
  finally
    fClientSafe.UnLock;
  end;
  if (result in [HTTP_SUCCESS, HTTP_PARTIALCONTENT]) or
     not ResetOutStreamPosition or // rewind OutStream for main server fallback
     not ((pcoTryAllPeers in fSettings.Options) or
          (waoTryAllPeers in Params.AlternateOptions)) then
    exit;
  // try up to the best TryAllPeersCount peers of our broadcast response
  for i := 1 to MinPtrInt(length(resp), fSettings.TryAllPeersCount) - 1 do
    if not fInstable.IsBanned(resp[i].IP4) then // banned in-between (unlikely)
      if fClientSafe.TryLock then
      try
        Params.SetStep(wgsAlternateGetNext, [IP4ToShort(@resp[i].IP4)]);
        result := LocalPeerRequest(req, resp[i], u, OutStream, {aRetry=}false);
        if (result in [HTTP_SUCCESS, HTTP_PARTIALCONTENT]) or
           not ResetOutStreamPosition then
          exit; // success
      finally
        fClientSafe.UnLock;
      end;
end;

function THttpPeerCache.Ping: THttpPeerCacheMessageDynArray;
var
  req: THttpPeerCacheMessage;
  alone: boolean;
begin
  MessageInit(pcfPing, 0, req);
  result := fUdpServer.Broadcast(req, alone);
end;

type
  TOnBeforeBodyErr = set of (
    eShutdown, eBearer, eNoGetHead, eUrl, eIp1, eBanned, eDecode, eIp2, eUuid,
    eDirectIp, eDirectDecode, eDirectKind, eDirectOpaque, aDirectDisabled);

function THttpPeerCache.OnBeforeBody(var aUrl, aMethod, aInHeaders,
  aInContentType, aRemoteIP, aBearerToken: RawUtf8; aContentLength: Int64;
  aFlags: THttpServerRequestFlags): cardinal;
var
  msg: THttpPeerCacheMessage;
  ip4: cardinal;
  err: TOnBeforeBodyErr;
begin
  // should return HTTP_SUCCESS=200 to continue the process, or an HTTP
  // error code to reject the request immediately as a "TeaPot", close the
  // socket and ban this IP for a few seconds at accept() level
  PByte(@msg.Kind)^ := 255; // ToText(msg.Kind)^ = ''
  err := [];
  if fSettings = nil then
    include(err, eShutdown); // avoid GPF at shutdown
  if length(aBearerToken) < PEER_CACHE_BEARERLEN then // base64uri length
    include(err, eBearer);
  if not (IsGet(aMethod) or
          IsHead(aMethod)) then
    include(err, eNoGetHead);
  if aUrl = '' then // URI is just ignored but something should be specified
    include(err, eUrl)
  else if err = [] then
    if PCardinal(aUrl)^ = DIRECTURI_32 then // start with '/htt'
    begin
      // pcfBearerDirect* for pcoHttpDirect mode: /https/microsoft.com/...
      if (aRemoteIp <> '') and
         (PCardinal(aRemoteIP)^ <> HOST_127) and
         (aRemoteIP <> fMac.IP) then
        include(err, eDirectIp); // only accepted from local
      if not Check(BearerDecode(aBearerToken, pcfRequest, msg),
               'OnBeforeBody Direct', msg) then
        include(err, eDirectDecode)
      else
      begin
        if not (msg.Kind in [pcfBearerDirect, pcfBearerDirectPermanent]) then
          include(err, eDirectKind);
        if Int64(msg.Opaque) <> crc63c(pointer(aUrl), length(aUrl)) then
          include(err, eDirectOpaque); // see THttpPeerCrypt.HttpDirectUri()
      end;
      if not (pcoHttpDirect in fSettings.Options) then
        include(err, aDirectDisabled);
    end
    else
    begin
      // pcfBearer for regular request in broadcasting mode
      if not IPToCardinal(aRemoteIP, ip4) then
        include(err, eIp1)
      else if fInstable.IsBanned(ip4) then // banned from RejectInstablePeersMin
        include(err, eBanned);
      if err = [] then
        if Check(BearerDecode(aBearerToken, pcfBearer, msg), 'OnBeforeBody', msg) then
        begin
          if msg.IP4 <> fIP4 then
            include(err, eIp2);
          if not ((IsZero(THash128(msg.Uuid)) or // IsZero for "fake" response bearer
                 IsEqualGuid(msg.Uuid, fUuid))) then
            include(err, eUuid);
        end
        else
          include(err, eDecode);
    end;
  result := HTTP_SUCCESS;
  if err <> [] then
    result := HTTP_FORBIDDEN;
  if fVerboseLog or
     (err <> []) then
    fLog.Add.Log(sllTrace, 'OnBeforeBody %=% % % % [%]',
      [aMethod, result, aRemoteIP, ToText(msg.Kind)^, aUrl,
       GetSetNameShort(TypeInfo(TOnBeforeBodyErr), @err)], self);
end;

procedure THttpPeerCache.OnIdle(tix64: Int64);
var
  tix: cardinal;
  size: Int64;
begin
  // avoid GPF at shutdown
  if fSettings = nil then
    exit;
  // check state every minute (65,536 seconds)
  if tix64 = 0 then
    tix64 := GetTickCount64;
  tix := (tix64 shr 16) + 1;
  // renew banned peer IPs TTL to implement RejectInstablePeersMin
  if (fInstable <> nil) and
     (fInstableTix <> tix) then
  begin
    fInstableTix := tix;
    if fInstable.Count <> 0 then
      if fInstable.DoRotate <> 0 then
        fLog.Add.Log(sllTrace, 'OnIdle: %', [fInstable], self);
  end;
  // check the temp folder every minute
  if fTempFilesTix <> tix then
    if fFilesSafe.TryLock then // within the lock
    try
      fTempFilesTix := tix;
      fTempCurrentSize := -1; // call FindFiles() again every minute
      // handle temporary cache folder deprecation
      if (fSettings.CacheTempMaxMin <= 0) or
         (fTempFilesPath = '') then
        exit;
      DirectoryDeleteOlderFiles(fTempFilesPath,
        fSettings.CacheTempMaxMin / MinsPerDay, PEER_CACHE_PATTERN, false, @size);
      if size <> 0 then // something changed on disk
        fLog.Add.Log(sllTrace, 'OnIdle: deleted %', [KBNoSpace(size)], self);
    finally
      fFilesSafe.UnLock; // re-allow background file access
    end;
end;

procedure THttpPeerCache.OnDownloaded(var Params: THttpClientSocketWGet;
  const Partial, ToRename: TFileName; PartialID: integer);
var
  local: TFileName;
  localsize, sourcesize, tot, start, stop: Int64;
  localok, istemp: boolean;
begin
  // avoid GPF at shutdown or in case of unexpected method call
  if (fSettings = nil) or
     (pcoNoServer in fSettings.Options) then
    exit;
  sourcesize := FileSize(Partial);
  localok := false; // for proper THttpPartials cleanup on abort
  try
    // the supplied downloaded source file should be big enough
    if (sourcesize = 0) or // paranoid
       TooSmallFile(Params, sourcesize, 'OnDownloaded') then
      exit;
    // compute the local cache file name from the known file hash
    if not CachedFileName(Params, [lfnEnsureDirectoryExists], local, istemp) then
    begin
      fLog.Add.Log(sllWarning,
        'OnDownloaded: no hash specified for %', [Partial], self);
      exit;
    end;
    // check if this file was not already in the cache folder
    // - outside fFilesSafe.Lock because happens just after OnDownload from cache
    localsize := FileSize(local);
    if localsize <> 0 then
    begin
      fLog.Add.Log(LOG_TRACEWARNING[localsize <> sourcesize],
        'OnDownloaded: % already in cache', [Partial], self);
      // size mismatch may happen on race condition (hash collision is unlikely)
      if localsize = sourcesize then
      begin
        Params.SetStep(wgsAlternateAlreadyInCache, [local]);
        localok := true;
      end
      else
        Params.SetStep(wgsAlternateWrongSizeInCache,
          [local, ' ', localsize, '<>', sourcesize]); // paranoid
      exit;
    end;
    QueryPerformanceMicroSeconds(start);
    fFilesSafe.Lock; // disable any concurrent file access
    try
      // ensure adding this file won't trigger the maximum cache size limit
      if (fTempFilesMaxSize > 0) and
         istemp then
      begin
        tot := TempFolderEstimateNewSize(sourcesize);
        if tot >= fTempFilesMaxSize then
        begin
          fLog.Add.Log(sllDebug, 'OnDownloaded: % is too big (%) for tot=%',
            [Partial, KBNoSpace(sourcesize), KBNoSpace(tot)], self);
          exit;
        end;
      end;
      // actually copy the source file into the local cache folder
      localok := CopyFile(Partial, local, {failsifexists=}false);
      if localok then
      begin
        Params.SetStep(wgsAlternateCopiedInCache, [local]);
        if istemp then
        begin
          // force timestamp = now within the temporary folder
          FileSetDateFromUnixUtc(local, UnixTimeUtc);
          inc(fTempCurrentSize, localsize);
        end;
      end
      else
        Params.SetStep(wgsAlternateFailedCopyInCache, [Partial, ' into ', local]);
    finally
      fFilesSafe.UnLock;
    end;
    QueryPerformanceMicroSeconds(stop);
    fLog.Add.Log(LOG_TRACEWARNING[not localok],
      'OnDownloaded: % copied % into % in %', [KBNoSpace(sourcesize),
      Partial, local, MicroSecToString(stop - start)], self);
  finally
    fPartials.Safe.WriteLock; // safely move file without background access
    try
      if ToRename <> '' then
      begin
        // optional rename *.partial to final WGet() file name
        if RenameFile(Partial, ToRename) then
        begin
          Params.SetStep(wgsAlternateRename, [ToRename]);
          if not localok then
            // if we can't use the cached local file, switch to renamed file
            // - it will work as long as it can (usually just fine)
            if fPartials.DoneLocked(Partial, ToRename) then
              PartialID := 0;
        end
        else
          Params.SetStep(wgsAlternateFailedRename, [Partial, ' as ', ToRename]);
      end;
      if localok then
        fPartials.DoneLocked(Partial, local)
      else if (PartialID <> 0) and
              (sourcesize <> 0) then
          fPartials.DoneLocked(PartialID);
    finally
      fPartials.Safe.WriteUnLock;
    end;
    if (PartialID <> 0) and
       (sourcesize = 0) then
      OnDownloadingFailed(PartialID); // clearly something went wrong
  end;
end;

procedure THttpPeerCache.OnDownloadFailed(const Params: THttpClientSocketWGet);
var
  local: TFileName;
  istemp: boolean;
begin
  // compute the local cache file name from the known file hash
  if not CachedFileName(Params, [lfnEnsureDirectoryExists], local, istemp) then
    fLog.Add.Log(sllWarning, 'OnDowloadFailed: missing hash', self)
  // actually delete the local (may be corrupted) file
  else if DeleteFile(local) then
    fLog.Add.Log(sllTrace, 'OnDowloadFailed: deleted %', [local], self)
  else
    fLog.Add.Log(sllLastError, 'OnDowloadFailed: error deleting %', [local], self);
end;

function THttpPeerCache.OnDownloading(const Params: THttpClientSocketWGet;
  const Partial: TFileName; ExpectedFullSize: Int64): THttpPartialID;
var
  h: THashDigest;
begin
  if (fPartials = nil) or // not supported by this fHttpServer class
     (waoNoProgressiveDownloading in Params.AlternateOptions) or
     not WGetToHash(Params, h) or
     TooSmallFile(Params, ExpectedFullSize, 'OnDownloading') then
    result := 0
  else
    result := fPartials.Add(Partial, ExpectedFullSize, h, {http=}nil);
end;

function THttpPeerCache.PartialFileName(const aMessage: THttpPeerCacheMessage;
  aHttp: PHttpRequestContext; aFileName: PFileName; aSize: PInt64): integer;
var
  fn: TFileName;
  size: Int64;
  id: THttpPartialID;
begin
  result := HTTP_NOTFOUND;
  if fPartials.IsVoid then // not supported or not used yet
    exit;
  fn := fPartials.Find(aMessage.Hash, size, @id);
  if fVerboseLog then
    if fn = '' then
      fLog.Add.Log(sllTrace, 'PartialFileName: none', self)
    else
      fLog.Add.Log(sllTrace, 'PartialFileName: % id=% size=% msg: size=% start=% end=%',
        [fn, id, size, aMessage.Size, aMessage.RangeStart, aMessage.RangeEnd], self);
  if size = 0 then
    exit; // not existing
  result := HTTP_NOTACCEPTABLE; // 406
  if (aMessage.Size <> 0) and // ExpectedSize may be 0 if waoNoHeadFirst was set
     (size <> aMessage.Size) then
    exit; // invalid file
  if (aHttp <> nil) and // register the partial for this HTTP request
     not fPartials.Associate(aMessage.Hash, aHttp) then
  begin
    fLog.Add.Log(sllWarning, 'PartialFileName: % race condition', [fn], self);
    exit; // race condition with Find() - paranoid
  end;
  result := HTTP_SUCCESS;
  if aFileName <> nil then
    aFileName^ := fn;
  if aSize <> nil then
    aSize^ := size;
end;

procedure THttpPeerCache.OnDownloadingFailed(ID: THttpPartialID);
begin
  // unregister and abort any partial downloading process
  if fPartials.Abort(ID) <> 0 then
    SleepHiRes(100); // wait for THttpServer.Process abort
end;

type
  THttpClientSocketPeerCache = class(THttpClientSocket)
  public
    // some additional internal parameters and methods for proper threading
    DestFileName: TFileName;
    DestStream: TStreamRedirect;
    RemoteUri: RawUtf8;
    RemoteHeaders: RawUtf8;
    ExpectedHash: RawUtf8;
    PartialID: THttpPartialID;
    function ExpectedHashOrRaiseEHttpPeerCache: Int64;
    procedure AbortDownload(Sender: THttpPeerCache; E: Exception);
    destructor Destroy; override;
  end;

function THttpClientSocketPeerCache.ExpectedHashOrRaiseEHttpPeerCache: Int64;
var
  done: RawUtf8;
begin
  done := DestStream.GetHash;
  if not PropNameEquals(done, ExpectedHash) then
    EHttpPeerCache.RaiseUtf8('GET % hash % failed: %<>%',
      [RemoteUri, DestStream, done, ExpectedHash]);
  result := DestStream.Size;
end;

procedure THttpClientSocketPeerCache.AbortDownload(Sender: THttpPeerCache; E: Exception);
begin
  Sender.fLog.Add.Log(sllDebug, 'DirectFileName(%) raised %',
    [DestFileName, E], Sender);
  Sender.OnDownloadingFailed(PartialID); // abort progressive HTTP requests
  Sender.fFilesSafe.Lock; // disable any concurrent file access
  try
    FreeAndNil(DestStream);   // close incorrect stream before deleting file
    DeleteFile(DestFileName); // remove any incompleted/invalid file
  finally
    Sender.fFilesSafe.UnLock;
  end;
end;

destructor THttpClientSocketPeerCache.Destroy;
begin
  FreeAndNil(DestStream);
  inherited Destroy;
end;

function DirectConnectAndHead(Sender: THttpPeerCache;
  Ctxt: THttpServerRequestAbstract; redirmax: integer; const aParams: RawUtf8;
  out cs: THttpClientSocketPeerCache): integer;
var
  cslog: TSynLogProc;
  uri: TUri;
  hdr: RawUtf8;
  opt: THttpRequestExtendedOptions;
begin
  cs := nil;
  // compute the remote URI and its associated options/header
  result := HTTP_BADREQUEST;
  if not Sender.HttpDirectUriReconstruct(pointer(Ctxt.Url), uri) then
    exit;
  if aParams = '' then
    opt.Init
  else if not opt.InitFromUrl(aParams, Sender.fDirectSecret) then
    exit; // e.g. 'ti=1&as=3'
  opt.CreateTimeoutMS := 1000;
  opt.RedirectMax := redirmax;
  if Assigned(Sender.fOnDirectOptions) then
  begin
    result := Sender.fOnDirectOptions(uri, hdr, opt); // customizable
    if result <> HTTP_SUCCESS then
      exit;
  end;
  // HEAD to the original server to connect, retrieving size and redirection
  cslog := nil;
  if Sender.fVerboseLog then
    cslog := Sender.fLog.DoLog;
  cs := THttpClientSocketPeerCache.OpenOptions(uri, opt, cslog);
  if redirmax = 0 then // from DirectFileNameHead()
    include(cs.Http.Options, hroHeadersUnfiltered);
  result := cs.Head(uri.Address, 30000, hdr);
  if not (result in HTTP_GET_OK) then
  begin
    FreeAndNil(cs);
    exit;
  end;
  if cs.Redirected <> '' then
    uri.Address := cs.Redirected; // follow 3xx redirection
  cs.RemoteUri := uri.Address;
  cs.RemoteHeaders := hdr;
  Ctxt.OutCustomHeaders := cs.Headers; // to include e.g. Content-Type:
end;

function THttpPeerCache.DirectFileName(Ctxt: THttpServerRequestAbstract;
  const aMessage: THttpPeerCacheMessage; aHttp: PHttpRequestContext;
  out aFileName: TFileName; out aSize: Int64; const aParams: RawUtf8): integer;
var
  cs:  THttpClientSocketPeerCache;
  err, ip: RawUtf8;
  i: PtrInt;
  localsize: Int64;
  alone: boolean;
  req: THttpPeerCacheMessage;
  peers: THttpPeerCacheMessageDynArray;
begin
  cs := nil;
  result := HTTP_BADREQUEST;
  try
    if aMessage.Kind in [pcfBearerDirect, pcfBearerDirectPermanent] then
    try
      // HEAD to the original server to connect and retrieve size + redirection
      err := 'head';
      result := DirectConnectAndHead(self, Ctxt, {redir=}3, aParams, cs);
      if not (result in HTTP_GET_OK) then
        exit;
      result := HTTP_LENGTHREQUIRED;
      aSize := cs.Http.ContentLength;
      Int32ToUtf8(aSize, err);
      if (aSize <= 0) or  // progressive request requires a final size
         (fSettings = nil) then // shutdown in progress
        exit;
      // try from cached files
      err := 'from partial';
      if fPartials <> nil then // check partial first (local may not be finished)
        result := PartialFileName(aMessage, aHttp, @aFileName, @localsize);
      if result <> HTTP_SUCCESS then
      begin
        err := 'from local';
        result := LocalFileName(aMessage, [lfnSetDate], @aFileName, @localsize);
      end;
      if result = HTTP_SUCCESS then
        if localsize <> aSize then // paranoid
        begin
          fLog.Add.Log(sllWarning, 'DirectFileName delete % size: %=% remote=%',
            [aFileName, err, localsize, aSize], self);
          fFilesSafe.Lock;
          try
            DeleteFile(aFileName); // this file seems invalid: clean from cache
            fTempCurrentSize := -1;
          finally
            fFilesSafe.UnLock;
          end;
        end
        else
        begin
          if err = 'from local' then
            aSize := 0; // not progressive
          exit;
        end;
      // prepare direct download into the final cache file as in LocalFileName()
      aFileName := ComputeFileName(aMessage.Hash);
      fFilesSafe.Lock; // disable any concurrent file access
      try
        case aMessage.Kind of
          pcfBearerDirect:
            begin
              result := HTTP_PAYLOADTOOLARGE;
              err := 'temp';
              if TempFolderEstimateNewSize(aSize) >= fTempFilesMaxSize then
                exit;
              aFileName := fTempFilesPath + aFileName;
            end;
          pcfBearerDirectPermanent:
            aFileName := PermFileName(aFileName, [lfnEnsureDirectoryExists]);
        end;
        // create the proper TStreamRedirect to store and hash
        err := 'file';
        result := HTTP_CONFLICT;
        if (FileSize(aFileName) > 0) or
           (fSettings = nil) then // shutdown in progress
          exit; // paranoid: existing cached files should have been checked
        cs.DestStream := HASH_STREAMREDIRECT[aMessage.Hash.Algo].Create(
          TFileStreamEx.Create(aFileName, fmCreate or fmShareRead));
        // mimics THttpPeerCache.OnDownloading() for progressive mode
        cs.DestFileName := aFileName;
        cs.PartialID := fPartials.Add(aFileName, aSize, aMessage.Hash, aHttp);
      finally
        fFilesSafe.UnLock;
      end;
      // ask the local peers for this resource (enabled by default above 64KB)
      cs.ExpectedHash := ToText(aMessage.Hash);
      if (aSize > fSettings.BroadCastDirectMinBytes) and
         not (pcoHttpDirectNoBroadcast in fSettings.Options) then
      begin
        MessageInit(pcfRequest, 0, req);
        req.Hash := aMessage.Hash;
        req.Size := aSize;
        // try first the current/last HTTP peer client (disabled by default)
        if (pcoHttpDirectTryLastPeer in fSettings.Options) and
           (fClient <> nil) and
           (fClientIP4 <> 0) and
           fClient.SockConnected and
           fClientSafe.TryLock then
        try
          SetLength(peers, 1);
          peers[0] := req;
          FillZero(peers[0].Uuid); // "fake" HEAD request to check this peer
          result := LocalPeerRequest(req, peers[0], Ctxt.Url, nil, {retry=}false);
          if fSettings = nil then
            exit; // shutdown in progress
          if result <> HTTP_SUCCESS then // returns HTTP_NOCONTENT if not found
            peers := nil; // make full GET in background thread
        finally
          fClientSafe.UnLock;
        end;
        // make an actual UDP broadcast about this file hash
        if peers = nil then
          peers := fUdpServer.Broadcast(req, alone);
        if peers <> nil then
          for i := 0 to high(peers) do
            if peers[i].IP4 <> fIP4 then
            begin
              // switch to this best local peer instead of main server
              IP4Text(@peers[i].IP4, ip);
              fLog.Add.Log(sllDebug, 'DirectFileName(%) switch to %:% peer',
                [Ctxt.Url, ip, fPort], self);
              cs.Close;
              LocalPeerClientSetup(ip, cs, 5000); // local cs, not fClient
              // local peer requires a new bearer
              peers[i].Kind := pcfBearer;
              MessageEncodeBearer(peers[i], cs.RemoteHeaders);
              FormatUtf8('?%', [Ctxt.Url], cs.RemoteUri); // <> DIRECTURI_32
              // make a quick HEAD on this new socket to verify the peer
              result := cs.Request(cs.RemoteUri, 'HEAD', 30000, cs.RemoteHeaders);
              if fSettings = nil then
                exit; // shutdown in progress
              if result in HTTP_GET_OK then
                break; // this peer seems just fine: background GET
            end;
      end;
      // start the GET request to the remote URI into aFileName via a sub-thread
      Make(['direct-', cs.PartialID], err);
      TLoggedWorkThread.Create(fLog, err, cs, DirectFileNameBackgroundGet);
      cs := nil; // will be owned by TLoggedWorkThread from now on
      result := HTTP_SUCCESS;
      // caller OnRequest() will return aFileName in progressive mode
    except
      on E: Exception do
      begin
        ClassToText(PClass(E)^, err);
        if (cs <> nil) and
           (cs.DestStream <> nil) then
          cs.AbortDownload(self, E)
        else
          fLog.Add.Log(sllDebug, 'DirectFileName(% %)=%',
            [Ctxt.Method, Ctxt.Url, PClass(E)^], self);
        result := HTTP_NOTFOUND;
      end;
    end;
  finally
    if (err <> '') and
       fVerboseLog then
      fLog.Add.Log(sllTrace, 'DirectFileName=% % (%)', [result, aFileName, err], self);
    cs.Free;
  end;
end;

procedure THttpPeerCache.DirectFileNameBackgroundGet(Sender: TObject);
var
  cs: THttpClientSocketPeerCache absolute Sender;
  res: integer;
  endsize: Int64;
begin
  // remote HTTP/HTTPS GET request executed in its own TLoggedWorkThread thread
  try
    try
      // make the actual blocking GET request in this background thread
      res := cs.Request(cs.RemoteUri, 'GET', 30000, cs.RemoteHeaders, '', '',
        {retry=}false, {instream=}nil, {outstream=}cs.DestStream);
      if fSettings = nil then
        exit; // shutdown
      if not (res in HTTP_GET_OK) then
        EHttpPeerCache.RaiseUtf8('GET % failed as %', [cs.RemoteUri, res]);
      endsize := cs.ExpectedHashOrRaiseEHttpPeerCache;
      fLog.Add.Log(sllTrace, 'DirectFileNameBackgroundGet(%)=% size=%',
        [cs.DestFileName, res, endsize], self);
    except
      on E: Exception do
        cs.AbortDownload(self, E);
    end;
  finally
    cs.Free; // overriden Destroy will make cs.DestStream.Free
  end;
end;

procedure AddReprDigest(Ctxt: THttpServerRequestAbstract; const Hash: THashDigest);
begin // implement pcoHttpReprDigest option
  Ctxt.SetOutCustomHeader(['Repr-Digest: ', HASH_TXT_LOWER[Hash.Algo],
    '=:', BinToBase64Short(@Hash.Bin, HASH_SIZE[Hash.Algo]), ':']);
end;

function THttpPeerCache.DirectFileNameHead(Ctxt: THttpServerRequestAbstract;
  const aHash: THashDigest; const aParams: RawUtf8): cardinal;
var
  cs:  THttpClientSocketPeerCache;
begin
  try
    cs := nil;
    try
      // direct HEAD to the remote server, with no redirection
      result := DirectConnectAndHead(self, Ctxt, {redir=}0, aParams, cs);
      if pcoHttpReprDigest in fSettings.Options then
        AddReprDigest(Ctxt, aHash);
    finally
      cs.Free;
    end;
  except
    on E: Exception do
    begin
      fLog.Add.Log(sllDebug, 'DirectFileNameHead(%)=%', [Ctxt.Url, PClass(E)^], self);
      result := HTTP_BADGATEWAY;
    end;
  end;
end;

type
  TOnRequestError = (
    oreOK,
    oreNoLocalFile,
    oreLocalFileNotAcceptable,
    oreDirectRemoteUriFailed,
    oreShutdown);

function THttpPeerCache.OnRequest(Ctxt: THttpServerRequestAbstract): cardinal;
var
  msg: THttpPeerCacheMessage;
  fn: TFileName;
  http: PHttpRequestContext;
  progsize: Int64; // expected progressive file size, to be supplied as header
  err: TOnRequestError;
  errtxt, opt: RawUtf8;
begin
  // avoid GPF at shutdown
  result := HTTP_BADREQUEST;
  if fSettings = nil then
    exit;
  // retrieve context - already checked by OnBeforeBody
  err := oreOK;
  http := (Ctxt as THttpServerRequest).fHttp;
  if Check(BearerDecode(Ctxt.AuthBearer, pcfRequest, msg, @opt), 'OnRequest', msg) then
  try
    // resource will always be identified by decoded bearer hash
    progsize := 0;
    case msg.Kind of
      pcfBearerDirect,
      pcfBearerDirectPermanent:
        if IsHead(Ctxt.Method) then
        begin
          // proxy the HEAD request to the final server
          result := DirectFileNameHead(Ctxt, msg.Hash, opt);
          if not (result in HTTP_GET_OK) then
            err := oreDirectRemoteUriFailed;
          exit;
        end
        else
        begin
          // remote HEAD + GET new partial file in a background thread
          result := DirectFileName(Ctxt, msg, http, fn, progsize, opt);
          if result <> HTTP_SUCCESS then
          begin
            err := oreDirectRemoteUriFailed;
            exit;
          end;
        end;
    else // pcfBearer
      begin
        // try first from PartialFileName() then LocalFileName()
        if fPartials <> nil then // check partial first (local may not be finished)
          result := PartialFileName(msg, http, @fn, @progsize);
        if result <> HTTP_SUCCESS then
          result := LocalFileName(msg, [lfnSetDate], @fn, nil);
      end;
    end;
    if fSettings = nil then
    begin
      err := oreShutdown;
      result := HTTP_BADGATEWAY; // 502
    end
    else if result <> HTTP_SUCCESS then
    begin
      // no local/partial known file: nothing to return
      if result = HTTP_NOTACCEPTABLE then // 406
        err := oreLocalFileNotAcceptable
      else
        err := oreNoLocalFile;
      if IsZero(THash128(msg.Uuid)) then // from "fake" response bearer
        result := HTTP_NOCONTENT; // OnDownload should make a broadcast
    end
    else
    begin
      // HTTP_SUCCESS: return the (partial) file as requested
      Ctxt.OutContent := StringToUtf8(fn);
      Ctxt.OutContentType := STATICFILE_CONTENT_TYPE;
      if progsize <> 0 then // append header for rfProgressiveStatic mode
        Ctxt.SetOutCustomHeader([STATICFILE_PROGSIZE + ' ', progsize]);
      if pcoHttpReprDigest in fSettings.Options then
        AddReprDigest(Ctxt, msg.Hash);
    end;
  finally
    if err <> oreOK then
      errtxt := GetEnumNameUnCamelCase(TypeInfo(TOnRequestError), ord(err));
    if not StatusCodeIsSuccess(result) then
      Ctxt.OutContent := Join([StatusCodeToErrorMsg(result), ' - ', errtxt]);
    fLog.Add.Log(sllDebug, 'OnRequest=% % % % fn=% progsiz=% progid=% %',
      [result, Ctxt.Method, Ctxt.RemoteIP, Ctxt.Url, fn, progsize,
       http^.ProgressiveID, errtxt], self);
  end;
end;


function ToText(pcf: THttpPeerCacheMessageKind): PShortString;
begin
  result := GetEnumName(TypeInfo(THttpPeerCacheMessageKind), ord(pcf));
end;

function ToText(md: THttpPeerCryptMessageDecode): PShortString;
begin
  result := GetEnumName(TypeInfo(THttpPeerCryptMessageDecode), ord(md));
end;

function ToText(const msg: THttpPeerCacheMessage): ShortString;
begin
  MsgToShort(msg, result);
end;

procedure MsgToShort(const msg: THttpPeerCacheMessage; var result: ShortString);
var
  algoext: PUtf8Char;
  algohex: string[SizeOf(msg.Hash.Bin.b) * 2];
begin
  result[0] := #0;
  if msg.Kind > high(msg.Kind) then
    exit; // clearly invalid message
  algoext := nil;
  algohex[0] := #0;
  if not IsZero(msg.Hash.Bin.b) then // append e.g. 'xxxHexaHashxxx.sha256'
  begin
    BinToHexLower(@msg.Hash.Bin, @algohex[1], HASH_SIZE[msg.Hash.Algo]);
    algohex[0] := AnsiChar(HASH_SIZE[msg.Hash.Algo] * 2);
    algoext := pointer(HASH_EXT[msg.Hash.Algo]);
  end;
  with msg do
    FormatShort('% #% % %% % % to % % % %Mb/s % %% siz=% con=% ',
      [ToText(Kind)^, CardinalToHexShort(Seq), OS_INITIAL[Os.os],
       OsvToShort(Os)^, WinOsBuild(Os, ' '), MAK_TXT[Hardware],
       IP4ToShort(@IP4), IP4ToShort(@DestIP4),
       IP4ToShort(@MaskIP4), IP4ToShort(@BroadcastIP4), Speed,
       UnixTimeToFileShort(QWord(Timestamp) + UNIXTIME_MINIMAL),
       algohex, algoext, Size, Connections], result);
  AppendShortUuid(msg.Uuid, result);
end;

function HttpRequestLength(aHeaders: PUtf8Char; out Len: PtrInt): PUtf8Char;
var
  s: PtrInt;
begin
  result := FindNameValuePointer(aHeaders, 'CONTENT-RANGE: ', Len);
  if result = nil then // no range
    result := FindNameValuePointer(aHeaders, 'CONTENT-LENGTH: ', Len)
  else
  begin // content-range: bytes 100-199/3083 -> extract 3083
    s := Len;
    while true do
      case result[s - 1] of
        '/':
          break;
        '0' .. '9':
          dec(s);
      else
        result := nil;
        exit;
      end;
    inc(result, s);
    dec(Len, s);
  end;
end;

function HttpRequestHash(aAlgo: THashAlgo; const aUri: TUri;
  aHeaders: PUtf8Char; out aDigest: THashDigest): integer;
var
  hasher: TSynHasher;
  h: PUtf8Char;
  l: PtrInt; // var PtrInt, not integer
begin
  result := 0;
  if (aUri.Server = '') or
     (aUri.Address = '') or
     not hasher.Init(aAlgo) then
    exit;
  hasher.Update(HTTPS_TEXT[aUri.Https]); // hash normalized URI
  hasher.Update(@aAlgo, 1); // separator
  hasher.Update(aUri.Server);
  hasher.Update(@aAlgo, 1);
  hasher.Update(aUri.Port);
  hasher.Update(@aAlgo, 1);
  hasher.Update(aUri.Address);
  if aHeaders <> nil then
  begin
    hasher.Update(@aAlgo, 1);
    h := FindNameValuePointer(aHeaders, 'ETAG: ', l); // ETAG + URI are genuine
    if h = nil then
    begin
      // fallback to file date and full size
      h := FindNameValuePointer(aHeaders, 'LAST-MODIFIED: ', l);
      if h = nil then
        exit;
      hasher.Update(h, l);
      h := HttpRequestLength(aHeaders, l);
      if h = nil then
        exit;
    end;
    hasher.Update(h, l);
  end;
  result := hasher.Final(aDigest.Bin);
  aDigest.Algo := aAlgo;
end;

function HttpRequestHashBase32(const aUri: TUri; aHeaders: PUtf8Char; aDiglen: integer): RawUtf8;
var
  dig: THashDigest;
begin
  if (aDigLen = 0) or
     (aDigLen mod 5 <> 0) or
     (HttpRequestHash(hfSHA256, aUri, aHeaders, dig) < aDiglen) then
    result := ''
  else // e.g. default aDigLen=20 bytes=160-bit as 32 chars
    result := BinToBase32(@dig.Bin, aDiglen, {lower=}true);
end;

{$ifdef USEWININET}

{ **************** THttpApiServer HTTP/1.1 Server Over Windows http.sys Module }

{ THttpApiServerThread }

procedure THttpApiServerThread.DoExecute;
begin
  if fOwner = nil then
    exit;
  fStarted := true;
  fOwner.DoExecute; // this main method is re-entrant by design
end;

constructor THttpApiServerThread.Create(aOwner: THttpApiServer);
begin
  fOwner := aOwner;
  inherited Create({suspended=}false, aOwner.fOnThreadStart,
    aOwner.fOnThreadTerminate, aOwner.fLogClass,
    Make([aOwner.ProcessName, ' #', length(aOwner.fThreads) + 1]));
end;


{ THttpApiServer }

class function THttpApiServer.HasApi2: boolean;
begin
  result := Http.Version.MajorVersion >= 2;
end;

function THttpApiServer.AddUrl(const aRoot, aPort: RawUtf8; Https: boolean;
  const aDomainName: RawUtf8; aRegisterUri: boolean; aContext: Int64): integer;
var
  uri: SynUnicode;
  n: PtrInt;
begin
  result := -1;
  if (Self = nil) or
     (fReqQueue = 0) or
     (Http.Module = 0) then
    exit;
  uri := RegURL(aRoot, aPort, Https, aDomainName);
  if uri = '' then
    exit; // invalid parameters
  if aRegisterUri then
    AddUrlAuthorize(aRoot, aPort, Https, aDomainName);
  if HasApi2 then
    result := Check(hAddUrlToUrlGroup,
      Http.AddUrlToUrlGroup(fUrlGroupID, pointer(uri), aContext))
  else
    result := Check(hAddUrl,
      Http.AddUrl(fReqQueue, pointer(uri)));
  if result <> NO_ERROR then
    exit;
  n := length(fRegisteredUnicodeUrl);
  SetLength(fRegisteredUnicodeUrl, n + 1);
  fRegisteredUnicodeUrl[n] := uri;
end;

function THttpApiServer.RemoveUrl(const aRoot, aPort: RawUtf8; Https: boolean;
  const aDomainName: RawUtf8): integer;
var
  uri: SynUnicode;
  i, j, n: PtrInt;
begin
  result := -1;
  if (Self = nil) or
     (fReqQueue = 0) or
     (Http.Module = 0) then
    exit;
  uri := RegURL(aRoot, aPort, Https, aDomainName);
  if uri = '' then
    exit; // invalid parameters
  n := High(fRegisteredUnicodeUrl);
  for i := 0 to n do
    if fRegisteredUnicodeUrl[i] = uri then
    begin
      if HasApi2 then
        result := Check(hRemoveUrlFromUrlGroup,
          Http.RemoveUrlFromUrlGroup(fUrlGroupID, pointer(uri), 0))
      else
        result := Check(hRemoveUrl,
          Http.RemoveUrl(fReqQueue, pointer(uri)));
      if result <> NO_ERROR then
        exit; // shall be handled by caller
      for j := i to n - 1 do
        fRegisteredUnicodeUrl[j] := fRegisteredUnicodeUrl[j + 1];
      SetLength(fRegisteredUnicodeUrl, n);
      exit;
    end;
end;

class function THttpApiServer.AddUrlAuthorize(const aRoot, aPort: RawUtf8;
  Https: boolean; const aDomainName: RawUtf8; OnlyDelete: boolean): RawUtf8;
begin // just redirect to our wrapper in mormot.lib.winhttp
  result := HttpApiAuthorize(aRoot, aPort, Https, aDomainName, OnlyDelete);
end;

function THttpApiServer.GetApiVersion: RawUtf8;
begin
  FormatUtf8('http.sys %.%',
    [Http.Version.MajorVersion, Http.Version.MinorVersion], result);
end;

function THttpApiServer.Check(Api: THttpApiFunction; Error: integer;
  Level: TSynLogLevel): integer;
var
  msg: RawUtf8;
begin
  result := Error;
  if Assigned(fLogClass) and
     not HttpApiSucceed(Api, msg, Error) then
    fLogClass.Add.Log(Level, msg, self);
end;

procedure THttpApiServer.Ensure(Api: THttpApiFunction; Error: integer);
begin
  if Assigned(fLogClass) then
    fLogClass.Add.Log(sllTrace, '%=%', [HttpApiFunction[Api], Error], self);
  EHttpApiServer.RaiseOnError(Api, Error);
end;

constructor THttpApiServer.Create(QueueName: SynUnicode;
  const OnStart, OnStop: TOnNotifyThread; const ProcessName: RawUtf8;
  ProcessOptions: THttpServerOptions; aLog: TSynLogClass;
  ServerThreadPoolCount: integer);
var
  log: ISynLog;
  binding: HTTP_BINDING_INFO;
  i: PtrInt;
begin
  aLog.EnterLocal(log, 'Create(%) processname=% threads=%',
    [QueueName, ProcessName, ServerThreadPoolCount], self);
  // initialize this thread in suspended mode
  inherited Create(OnStart, OnStop, ProcessName,
    ProcessOptions + [hsoCreateSuspended] - [hsoThreadCpuAffinity,
      hsoThreadSocketAffinity, hsoReusePort, hsoThreadSmooting], aLog);
  // create the Request Queue
  HttpApiInitialize; // will raise an exception in case of failure
  if Assigned(log) then
    log.Log(sllHttp, 'Create: new % handle', [GetApiVersion], self);
  Ensure(hInitialize,
    Http.Initialize(Http.Version, HTTP_INITIALIZE_SERVER));
  if HasApi2 then
  begin
    Ensure(hCreateServerSession,
      Http.CreateServerSession(Http.Version, fServerSessionID));
    Ensure(hCreateUrlGroup,
      Http.CreateUrlGroup(fServerSessionID, fUrlGroupID));
    if QueueName = '' then
      Utf8ToSynUnicode(Int64ToUtf8(fServerSessionID), QueueName);
    Ensure(hCreateRequestQueue,
      Http.CreateRequestQueue(Http.Version, pointer(QueueName), nil, 0, fReqQueue));
    binding.Flags := 1;
    binding.RequestQueueHandle := fReqQueue;
    Ensure(hSetUrlGroupProperty,
      Http.SetUrlGroupProperty(fUrlGroupID,
        HttpServerBindingProperty, @binding, SizeOf(binding)));
  end
  else
    Ensure(hCreateHttpHandle,
      Http.CreateHttpHandle(fReqQueue));
  // start the other processing threads
  fReceiveBufferSize := 1 shl 20; // i.e. 1 MB
  if Assigned(log) then
    log.Log(sllTrace, 'Create: start threads', self);
  for i := 2 to ServerThreadPoolCount do
    ObjArrayAdd(fThreads, THttpApiServerThread.Create(self));
  // eventually start the main thread
  Append(fProcessName, [' #', ServerThreadPoolCount]);
  if not (hsoCreateSuspended in ProcessOptions) then
  begin
    Suspended := false;
    exclude(fOptions, hsoCreateSuspended);
  end;
end;

function THttpApiServer.WaitStarted(Seconds: cardinal): boolean;
var
  tix32: cardinal;
  i: PtrInt;
begin
  result := false;
  if fReceiveBufferSize = 0 then
    exit; // Create never reached actual activation
  tix32 := GetTickSec + Seconds; // never wait forever
  repeat
    SleepHiRes(1);  // warning: waits typically 1-15 ms on Windows
    result := true; // fProcessing may be false if main thread did abort
    for i := 0 to high(fThreads) do
      result := result and ((fThreads[i] = nil) or fThreads[i].fStarted);
  until result or
        (GetTickSec > tix32);
  fLogClass.Add.Log(sllTrace, 'WaitStarted(%)=%', [Seconds, BOOL_STR[result]], self);
end;

procedure THttpApiServer.DestroyMainThread;
var
  i: PtrInt;
begin
  if fReqQueue <> 0 then
  begin
    fLogClass.Add.Log(sllTrace, 'DestroyMainThread=%', [length(fThreads)], self);
    for i := 0 to high(fThreads) do
      if fThreads[i] <> nil then
        fThreads[i].Terminate; // for CloseHandle() below to finish Execute
    if HasApi2 then
    begin
      if fUrlGroupID <> 0 then
      begin
        Check(hRemoveUrlFromUrlGroup,
          Http.RemoveUrlFromUrlGroup(fUrlGroupID, nil, HTTP_URL_FLAG_REMOVE_ALL));
        Check(hCloseUrlGroup,
          Http.CloseUrlGroup(fUrlGroupID));
        fUrlGroupID := 0;
      end;
      CloseHandle(fReqQueue);
      if fServerSessionID <> 0 then
      begin
        Check(hCloseServerSession,
          Http.CloseServerSession(fServerSessionID));
        fServerSessionID := 0;
      end;
    end
    else
    begin
      for i := 0 to high(fRegisteredUnicodeUrl) do
        Check(hRemoveUrl,
          Http.RemoveUrl(fReqQueue, pointer(fRegisteredUnicodeUrl[i])));
      CloseHandle(fReqQueue); // will break all THttpApiServer.Execute
    end;
    fReqQueue := 0;
    {$ifdef FPC}
    for i := 0 to high(fThreads) do
      if fThreads[i] <> nil then
        WaitForSingleObject(fThreads[i].Handle, 30000); // maybe needed on FPC
    {$endif FPC}
    ObjArrayClear(fThreads, {continueOnException:}true);
    Check(hTerminate,
      Http.Terminate(HTTP_INITIALIZE_SERVER));
  end;
end;

destructor THttpApiServer.Destroy;
begin
  Terminate; // for Execute to be notified about end of process
  try
    if Http.Module <> 0 then
      DestroyMainThread;
    {$ifdef FPC}
    WaitForSingleObject(Handle, 30000); // sometimes needed on FPC
    {$endif FPC}
  finally
    inherited Destroy;
  end;
end;

function THttpApiServer.GetSendResponseFlags(Ctxt: THttpServerRequest): integer;
begin
  result := 0;
end;

type
  TVerbText = array[hvOPTIONS..pred(hvMaximum)] of RawUtf8;

const
  VERB_TEXT: TVerbText = (
    'OPTIONS',
    'GET',
    'HEAD',
    'POST',
    'PUT',
    'DELETE',
    'TRACE',
    'CONNECT',
    'TRACK',
    'MOVE',
    'COPY',
    'PROPFIND',
    'PROPPATCH',
    'MKCOL',
    'LOCK',
    'UNLOCK',
    'SEARCH');

var
  global_verbs: TVerbText; // to avoid memory allocation on Delphi

procedure ReqToLog(req: PHTTP_REQUEST; ctxt: THttpServerRequest;
  log: PHTTP_LOG_FIELDS_DATA); // better code generation with a sub-function
begin
  log^.MethodNum     := req^.Verb;
  log^.UriStemLength := req^.CookedUrl.AbsPathLength;
  log^.UriStem       := req^.CookedUrl.pAbsPath;
  with req^.headers.KnownHeaders[reqUserAgent] do
  begin
    log^.UserAgentLength := RawValueLength;
    log^.UserAgent       := pRawValue;
  end;
  with req^.headers.KnownHeaders[reqHost] do
  begin
    log^.HostLength := RawValueLength;
    log^.Host       := pRawValue;
  end;
  with req^.headers.KnownHeaders[reqReferrer] do
  begin
    log^.ReferrerLength := RawValueLength;
    log^.Referrer       := pRawValue;
  end;
  with req^.headers.KnownHeaders[respServer] do
  begin
    log^.ServerNameLength := RawValueLength;
    log^.ServerName       := pRawValue;
  end;
  log^.ClientIp       := pointer(ctxt.fRemoteIP);
  log^.ClientIpLength := length(ctxt.fRemoteIP);
  log^.Method         := pointer(ctxt.fMethod);
  log^.MethodLength   := length(ctxt.fMethod);
  log^.UserName       := pointer(ctxt.fAuthenticatedUser);
  log^.UserNameLength := Length(ctxt.fAuthenticatedUser);
end;

procedure THttpApiServer.DoExecute;
var // lots of local variable so that this method is thread-safe
  req: PHTTP_REQUEST;
  resp: PHTTP_RESPONSE;
  reqbuf, respbuf, logbuf: TBytes;
  reqid: HTTP_REQUEST_ID;
  i: PtrInt;
  bytesread, bytessent, flags: cardinal;
  compressset: THttpSocketCompressSet;
  comprec: PHttpSocketCompressRec;
  err: HRESULT;
  incontlen, rangestart, rangelen: Qword;
  incontlenchunk, incontlenread: cardinal;
  incontenc, inaccept, host, range, referer: RawUtf8;
  outstat, outmsg: RawUtf8;
  outstatcode, afterstatcode: cardinal;
  respsent: boolean;
  ctxt: THttpServerRequest;
  filehandle: THandle;
  bufread, V: PUtf8Char;
  heads: HTTP_UNKNOWN_HEADERS;
  outcontlen: ULARGE_INTEGER;
  datachunkmem: HTTP_DATA_CHUNK_INMEMORY;
  datachunkfile: HTTP_DATA_CHUNK_FILEHANDLE;
  started, elapsed: Int64;

  procedure HttpSendResponse(flags: cardinal);
  var
    log: PHTTP_LOG_FIELDS_DATA;
    err: integer;
  begin
    // update log information
    ctxt.RespStatus := resp^.StatusCode; // for ReqToLog()
    log := nil;
    if fLogging then // after LogStart (and http.sys API v2)
    begin
      log := pointer(logbuf);
      log^.ProtocolStatus    := resp^.StatusCode;
      log^.ServerNameLength  := length(fServerName);
      log^.ServerName        := pointer(fServerName);
      log^.ServiceNameLength := length(fLoggingServiceName);
      log^.ServiceName       := pointer(fLoggingServiceName);
      ReqToLog(req, ctxt, log);
    end;
    // send the resp^ HTTP response to the req^ HTTP request
    resp^.Version := req^.Version;
    with resp^.headers.KnownHeaders[respServer] do
    begin
      pRawValue      := pointer(fServerName);
      RawValueLength := length(fServerName);
    end;
    err := Http.SendHttpResponse(fReqQueue, req^.RequestId, flags, resp^, nil,
        bytessent, nil, 0, nil, log);
    if err <> NO_ERROR then
      Check(hSendHttpResponse, err);
    FillcharFast(resp^, SizeOf(resp^), 0);
  end;

  procedure SendError(StatusCode: cardinal; const ErrorMsg: RawUtf8;
    E: Exception = nil);
  begin
    try
      resp^.SetStatus(StatusCode, outstat);
      FormatUtf8('<!DOCTYPE html><html><body style="font-family:verdana;">' +
        '<h1>Server Error %: %</h1><p>', [StatusCode, outstat], outmsg);
      if E <> nil then
        Append(outmsg, [E, ' Exception raised:<br>']);
      Append(outmsg, HtmlEscape(ErrorMsg), '</p><p><small>' + XPOWEREDVALUE);
      resp^.SetContent(datachunkmem, outmsg, HTML_CONTENT_TYPE);
      HttpSendResponse(0);
    except
      on Exception do
        ; // ignore any HttpApi level errors here (client may have crashed)
    end;
  end;

  function SendResponse: boolean;
  var
    R: PUtf8Char;
    flags: cardinal;
  begin
    result := not Terminated; // true=success
    if not result then
      exit;
    respsent := true;
    resp^.SetStatus(outstatcode, outstat);
    if Terminated then
      exit;
    // associate response headers
    resp^.SetHeaders(pointer(ctxt.OutCustomHeaders),
      heads, hsoNoXPoweredHeader in fOptions);
    if fCompressList.AcceptEncoding <> '' then
      resp^.AddCustomHeader(pointer(fCompressList.AcceptEncoding), heads, false);
    if ctxt.OutContentType = STATICFILE_CONTENT_TYPE then
    begin
      // response is file -> OutContent is UTF-8 file name to be served
      filehandle := FileOpen(Utf8ToString(ctxt.OutContent), fmOpenReadShared);
      if not ValidHandle(filehandle)  then
      begin
        SendError(HTTP_NOTFOUND, GetErrorText); // text message from OS
        result := false; // notify fatal error
      end;
      try // http.sys will serve then close the file from kernel
        datachunkfile.DataChunkType := hctFromFileHandle;
        datachunkfile.filehandle := filehandle;
        flags := 0;
        datachunkfile.ByteRange.StartingOffset.QuadPart := 0;
        Int64(datachunkfile.ByteRange.Length.QuadPart) := -1; // to eof
        with req^.headers.KnownHeaders[reqRange] do
        begin
          if (RawValueLength > 6) and
             IdemPChar(pointer(pRawValue), 'BYTES=') and
             (pRawValue[6] in ['0'..'9']) then
          begin
            FastSetString(range, pRawValue + 6, RawValueLength - 6); // need #0 end
            R := pointer(range);
            rangestart := GetNextRange(R);
            if R^ = '-' then
            begin
              outcontlen.QuadPart := FileSize(filehandle);
              datachunkfile.ByteRange.Length.QuadPart :=
                outcontlen.QuadPart - rangestart;
              inc(R);
              flags := HTTP_SEND_RESPONSE_FLAG_PROCESS_RANGES;
              datachunkfile.ByteRange.StartingOffset.QuadPart := rangestart;
              if R^ in ['0'..'9'] then
              begin
                rangelen := GetNextRange(R) - rangestart + 1;
                if Int64(rangelen) < 0 then
                  rangelen := 0;
                if rangelen < datachunkfile.ByteRange.Length.QuadPart then
                  // "bytes=0-499" -> start=0, len=500
                  datachunkfile.ByteRange.Length.QuadPart := rangelen;
              end; // "bytes=1000-" -> start=1000, to eof
              FormatUtf8('Content-range: bytes %-%/%', [rangestart,
                rangestart + datachunkfile.ByteRange.Length.QuadPart - 1,
                outcontlen.QuadPart], range);
              resp^.AddCustomHeader(pointer(range), heads, false);
              resp^.SetStatus(HTTP_PARTIALCONTENT, outstat);
            end;
          end;
        end;
        with resp^.headers.KnownHeaders[respAcceptRanges] do
        begin
          pRawValue := 'bytes';
          RawValueLength := 5;
        end;
        resp^.EntityChunkCount := 1;
        resp^.pEntityChunks := @datachunkfile;
        HttpSendResponse(flags);
      finally
        FileClose(filehandle);
      end;
    end
    else
    begin
      // response is in OutContent -> send it from memory
      if ctxt.OutContentType = NORESPONSE_CONTENT_TYPE then
        ctxt.OutContentType := ''; // true HTTP always expects a response
      if (integer(compressset) <> 0) and
         (fCompressList.Algo <> nil) then
        with resp^.headers.KnownHeaders[reqContentEncoding] do
          if RawValueLength = 0 then // no previous encoding -> try compression
          begin
            comprec := fCompressList.CompressContent(
              compressset, ctxt.OutContentType, ctxt.fOutContent);
            if comprec <> nil then
            begin
              pRawValue      := pointer(comprec^.Name);
              RawValueLength := length(comprec^.Name);
            end;
          end;
      resp^.SetContent(datachunkmem, ctxt.OutContent, ctxt.OutContentType);
      HttpSendResponse(GetSendResponseFlags(ctxt));
    end;
  end;

begin
  if Terminated then
    exit;
  ctxt := THttpServerRequest.Create(self, 0, self, 0, [], nil);
  try
    // reserve working buffers
    SetLength(heads, 64);
    SetLength(reqbuf, 16384 + SizeOf(HTTP_REQUEST)); // req^ + 16 KB of headers
    SetLength(respbuf, SizeOf(HTTP_RESPONSE));
    if HasApi2 then
      SetLength(logbuf, SizeOf(HTTP_LOG_FIELDS_DATA));
    req := pointer(reqbuf);
    resp := pointer(respbuf);
    if global_verbs[hvOPTIONS] = '' then
      global_verbs := VERB_TEXT;
    // main loop reusing a single ctxt instance for this thread
    reqid := 0;
    ctxt.fServer := self;
    repeat
      // release input/output body buffers ASAP
      ctxt.fInContent := '';
      ctxt.fOutContent := '';
      // retrieve next pending request, and read its headers
      FillcharFast(req^, SizeOf(HTTP_REQUEST), 0);
      err := Http.ReceiveHttpRequest(fReqQueue, reqid, 0,
        req^, length(reqbuf), bytesread); // blocking until received something
      if Terminated then
        break;
      case err of
        NO_ERROR:
          try
            LockedInc32(@fCurrentProcess);
            // parse method and main headers as ctxt.Prepare() does
            bytessent := 0;
            ctxt.fHttpApiRequest := req;
            ctxt.Recycle(req^.ConnectionID, self, {asynchandle=}0,
              HTTP_TLS_FLAGS[req^.pSslInfo <> nil] +
              // no HTTP_UPG_FLAGS[]: plain THttpApiServer don't support upgrade
              HTTP_10_FLAGS[(req^.Version.MajorVersion = 1) and
                            (req^.Version.MinorVersion = 0)],
              // ctxt.fConnectionOpaque is not supported by http.sys
              nil);
            FastSetString(ctxt.fUrl, req^.pRawUrl, req^.RawUrlLength);
            if req^.Verb in [low(global_verbs)..high(global_verbs)] then
              ctxt.fMethod := global_verbs[req^.Verb]
            else
              FastSetString(ctxt.fMethod, req^.pUnknownVerb, req^.UnknownVerbLength);
            with req^.headers.KnownHeaders[reqContentType] do
              FastSetString(ctxt.fInContentType, pRawValue, RawValueLength);
            with req^.headers.KnownHeaders[reqUserAgent] do
              FastSetString(ctxt.fUserAgent, pRawValue, RawValueLength);
            with req^.headers.KnownHeaders[reqHost] do
              FastSetString(ctxt.fHost, pRawValue, RawValueLength);
            host := ctxt.Host; // may be reset during Request()
            with req^.Headers.KnownHeaders[reqAuthorization] do
              if (RawValueLength > 7) and
                 IdemPChar(pointer(pRawValue), 'BEARER ') then
                FastSetString(ctxt.fAuthBearer, pRawValue + 7, RawValueLength - 7);
            with req^.headers.KnownHeaders[reqAcceptEncoding] do
              FastSetString(inaccept, pRawValue, RawValueLength);
            with req^.headers.KnownHeaders[reqReferrer] do
              FastSetString(referer, pRawValue, RawValueLength);
            fCompressList.DecodeAcceptEncoding(pointer(inaccept), compressset);
            ctxt.fInHeaders := RetrieveHeadersAndGetRemoteIPConnectionID(
              req^, fRemoteIPHeaderUpper, fRemoteConnIDHeaderUpper,
              {out} ctxt.fRemoteIP, PQWord(@ctxt.fConnectionID)^);
            // retrieve any SetAuthenticationSchemes() information
            if byte(fAuthenticationSchemes) <> 0 then // set only with HTTP API 2.0
              // https://docs.microsoft.com/en-us/windows/win32/http/authentication-in-http-version-2-0
              for i := 0 to req^.RequestInfoCount - 1 do
                with req^.pRequestInfo^[i] do
                if InfoType = HttpRequestInfoTypeAuth then
                  case pInfo^.AuthStatus of
                    HttpAuthStatusSuccess:
                      if pInfo^.AuthType > HttpRequestAuthTypeNone then
                      begin
                        byte(ctxt.fAuthenticationStatus) := ord(pInfo^.AuthType) + 1;
                        if pInfo^.AccessToken <> 0 then
                        begin
                          ctxt.fAuthenticatedUser := LookupToken(pInfo^.AccessToken);
                          // AccessToken lifecycle is application responsibility
                          CloseHandle(pInfo^.AccessToken);
                          ctxt.fAuthBearer := ctxt.fAuthenticatedUser;
                          include(ctxt.fConnectionFlags, hsrAuthorized);
                        end;
                      end;
                    HttpAuthStatusFailure:
                      ctxt.fAuthenticationStatus := hraFailed;
                  end;
            // abort request if > MaximumAllowedContentLength or OnBeforeBody
            with req^.headers.KnownHeaders[reqContentLength] do
            begin
              V := pointer(pRawValue);
              SetQWord(V, V + RawValueLength, incontlen);
            end;
            if (incontlen > 0) and
               (fMaximumAllowedContentLength > 0) and
               (incontlen > QWord(fMaximumAllowedContentLength)) then
            begin
              SendError(HTTP_PAYLOADTOOLARGE, 'Rejected');
              continue;
            end;
            if (hsoRejectBotUserAgent in fOptions) and
               (ctxt.fUserAgent <> '') and
               IsHttpUserAgentBot(ctxt.fUserAgent) then
            begin
              SendError(HTTP_TEAPOT, BOTBUSTER_RESPONSE);
              continue;
            end;
            if Assigned(OnBeforeBody) then
            begin
              err := OnBeforeBody(ctxt.fUrl, ctxt.fMethod, ctxt.fInHeaders,
                ctxt.fInContentType, ctxt.fRemoteIP, ctxt.fAuthBearer, incontlen,
                ctxt.ConnectionFlags);
              if err <> HTTP_SUCCESS then
              begin
                SendError(err, 'Rejected');
                continue;
              end;
            end;
            // retrieve body
            if HTTP_REQUEST_FLAG_MORE_ENTITY_BODY_EXISTS and req^.flags <> 0 then
            begin
              with req^.headers.KnownHeaders[reqContentEncoding] do
                FastSetString(incontenc, pRawValue, RawValueLength);
              if incontlen <> 0 then
              begin
                // receive body chunks
                SetLength(ctxt.fInContent, incontlen);
                bufread := pointer(ctxt.InContent);
                incontlenread := 0;
                repeat
                  bytesread := 0;
                  if HasApi2 then
                    // speed optimization for Vista+
                    flags := HTTP_RECEIVE_REQUEST_ENTITY_BODY_FLAG_FILL_BUFFER
                  else
                    flags := 0;
                  incontlenchunk := incontlen - incontlenread;
                  if (fReceiveBufferSize >= 1024) and
                     (incontlenchunk > fReceiveBufferSize) then
                    incontlenchunk := fReceiveBufferSize;
                  err := Http.ReceiveRequestEntityBody(fReqQueue,
                    req^.RequestId, flags, bufread, incontlenchunk, bytesread);
                  if Terminated then
                    exit;
                  inc(incontlenread, bytesread);
                  if err = ERROR_HANDLE_EOF then
                  begin
                    if incontlenread < incontlen then
                      SetLength(ctxt.fInContent, incontlenread);
                    err := NO_ERROR;
                    break; // should loop until returns ERROR_HANDLE_EOF
                  end;
                  if err <> NO_ERROR then
                  begin
                    Check(hReceiveRequestEntityBody, err);
                    break;
                  end;
                  inc(bufread, bytesread);
                until incontlenread = incontlen;
                if err <> NO_ERROR then
                begin
                  SendError(HTTP_NOTACCEPTABLE, WinApiErrorUtf8(err, Http.Module));
                  continue;
                end;
                // optionally uncompress input body
                if incontenc <> '' then
                  fCompressList.UncompressContent(incontenc, ctxt.fInContent);
              end;
            end;
            QueryPerformanceMicroSeconds(started);
            try
              // compute response
              respsent := false;
              outstatcode := 0;
              if fRoute <> nil then
                // URI rewrite or event callback execution
                outstatcode := fRoute.Process(Ctxt);
              if outstatcode = 0 then // no router callback was executed
              begin
                // regular server-side OnRequest execution
                outstatcode := DoBeforeRequest(ctxt);
                if outstatcode > 0 then
                  if not SendResponse or
                     (outstatcode <> HTTP_ACCEPTED) then
                    continue;
                outstatcode := Request(ctxt); // call OnRequest for main process
                afterstatcode := DoAfterRequest(ctxt);
                if afterstatcode > 0 then
                  outstatcode := afterstatcode;
              end;
              // send response
              if not respsent then
                if not SendResponse then
                  continue;
              QueryPerformanceMicroSeconds(elapsed);
              dec(elapsed, started);
              ctxt.Host := host; // may have been reset during Request()
              DoAfterResponse(
                ctxt, referer, outstatcode, elapsed, incontlen, bytessent);
            except
              on E: Exception do
                // handle any exception raised during process: show must go on!
                if not respsent then
                  if not E.InheritsFrom(EHttpApiServer) or // ensure still connected
                     (EHttpApiServer(E).LastApiError <> HTTPAPI_ERROR_NONEXISTENTCONNECTION) then
                    SendError(HTTP_SERVERERROR, StringToUtf8(E.Message), E);
            end;
          finally
            LockedDec32(@fCurrentProcess);
            reqid := 0; // reset Request ID to handle the next pending request
          end;
        ERROR_MORE_DATA:
          begin
            // input buffer was too small to hold the request headers
            fLogClass.Add.Log(sllDebug,
              'DoExecute: increase buffer size to %', [bytesread], self);
            reqid := req^.RequestId;
            SetLength(reqbuf, bytesread);
            req := pointer(reqbuf); // will try again
          end;
        ERROR_CONNECTION_INVALID:
          begin
            Check(hReceiveHttpRequest, err);
            if reqid = 0 then
              break;
            // TCP connection was corrupted by the peer -> ignore + next request
            reqid := 0;
          end
      else
        begin
          Check(hReceiveHttpRequest, err);
          break; // unhandled err value
        end;
      end;
    until Terminated;
  finally
    ctxt.Free;
  end;
end;

function THttpApiServer.GetHttpQueueLength: cardinal;
begin
  result := 0;
  if (self = nil) or
     (fReqQueue = 0) or
     not HasApi2 then
    exit;
  EHttpApiServer.RaiseOnError(hQueryRequestQueueProperty,
    Http.QueryRequestQueueProperty(fReqQueue, HttpServerQueueLengthProperty,
      @result, SizeOf(result)));
end;

procedure THttpApiServer.SetHttpQueueLength(aValue: cardinal);
begin
  EHttpApiServer.RaiseCheckApi2(hSetRequestQueueProperty);
  if (self <> nil) and
     (fReqQueue <> 0) then
    Ensure(hSetRequestQueueProperty,
      Http.SetRequestQueueProperty(fReqQueue, HttpServerQueueLengthProperty,
        @aValue, SizeOf(aValue)));
end;

function THttpApiServer.GetConnectionsActive: cardinal;
begin
  result := 0; // unsupported
end;

function THttpApiServer.GetRegisteredUrl: RawUtf8;
var
  i: PtrInt;
begin
  result := fRegisteredUrl;
  if (result <> '') or
     (fRegisteredUnicodeUrl = nil) then
    exit;
  fRegisteredUrl := SynUnicodeToUtf8(fRegisteredUnicodeUrl[0]);
  for i := 1 to high(fRegisteredUnicodeUrl) do
    Append(fRegisteredUrl, [',', fRegisteredUnicodeUrl[i]]);
  result := fRegisteredUrl;
end;

function THttpApiServer.GetProperty(dest: PHTTP_QOS_SETTING_INFO;
  destlen: cardinal; qos: HTTP_QOS_SETTING_TYPE): boolean;
begin
  result := false;
  if (self = nil) or
     not HasApi2 then
    exit;
  if fUrlGroupID = 0 then
    exit;
  dest.QosType := qos;
  dest.QosSetting := PAnsiChar(dest) + SizeOf(dest^); // should be after header
  FillCharFast(dest.QosSetting^, destlen - SizeOf(dest^), 0); // for safety
  EHttpApiServer.RaiseOnError(hQueryUrlGroupProperty,
    Http.QueryUrlGroupProperty(fUrlGroupID, HttpServerQosProperty, dest, destlen));
  result := true;
end;

procedure THttpApiServer.SetProperty(value: PHTTP_QOS_SETTING_INFO;
  valuelen: cardinal; qos: HTTP_QOS_SETTING_TYPE; alsoForSession: boolean);
begin
  EHttpApiServer.RaiseCheckApi2(hSetUrlGroupProperty);
  if fUrlGroupID = 0 then
    exit;
  value.QosType := qos;
  value.QosSetting := PAnsiChar(value) + SizeOf(value^); // just after header
  if alsoForSession then
    Ensure(hSetServerSessionProperty,
      Http.SetServerSessionProperty(fServerSessionID, HttpServerQosProperty,
        value, valuelen));
  Ensure(hSetUrlGroupProperty,
    Http.SetUrlGroupProperty(fUrlGroupID, HttpServerQosProperty, value, valuelen));
end;

procedure THttpApiServer.SetMaxBandwidth(aValue: cardinal);
var
  limit: HTTP_BANDWIDTH_LIMIT_INFO;
begin
  limit.Flags := 1;
  if aValue = 0 then
    limit.MaxBandwidth := HTTP_LIMIT_INFINITE
  else if aValue < HTTP_MIN_ALLOWED_BANDWIDTH_THROTTLING_RATE then
    limit.MaxBandwidth := HTTP_MIN_ALLOWED_BANDWIDTH_THROTTLING_RATE
  else
    limit.MaxBandwidth := aValue;
  SetProperty(@limit, SizeOf(limit),
    HttpQosSettingTypeBandwidth, {alsoSession=}true);
end;

function THttpApiServer.GetMaxBandwidth: cardinal;
var
  limit: HTTP_BANDWIDTH_LIMIT_INFO;
begin
  result := 0;
  if GetProperty(@limit, SizeOf(limit), HttpQosSettingTypeBandwidth) then
    result := limit.MaxBandwidth;
  if result = HTTP_LIMIT_INFINITE then
    result := 0; // cleaner for end-user
end;

function THttpApiServer.GetMaxConnections: cardinal;
var
  limit: HTTP_CONNECTION_LIMIT_INFO;
begin
  result := 0;
  if GetProperty(@limit, SizeOf(limit), HttpQosSettingTypeConnectionLimit) then
    result := limit.MaxConnections;
  if result = HTTP_LIMIT_INFINITE then
    result := 0; // cleaner for end-user
end;

procedure THttpApiServer.SetMaxConnections(aValue: cardinal);
var
  limit: HTTP_CONNECTION_LIMIT_INFO;
begin
  limit.Flags := 1;
  if aValue = 0 then
    limit.MaxConnections := HTTP_LIMIT_INFINITE
  else
    limit.MaxConnections := aValue;
  SetProperty(@limit, SizeOf(limit), HttpQosSettingTypeConnectionLimit);
end;

procedure THttpApiServer.LogStart(const aLogFolder: TFileName;
  aType: THttpApiLoggingType; const aSoftwareName: TFileName;
  aRolloverType: THttpApiLoggingRollOver; aRolloverSize: cardinal;
  aLogFields: THttpApiLogFields; aFlags: THttpApiLoggingFlags);
var
  log: HTTP_LOGGING_INFO;
  folder, software: SynUnicode;
begin
  if self = nil then
    exit;
  EHttpApiServer.RaiseCheckApi2(hSetUrlGroupProperty);
  // disable any previous logging
  fLogging := false;
  // setup log parameters
  FillcharFast(log, SizeOf(log), 0);
  log.Flags := 1;
  log.LoggingFlags := byte(aFlags);
  if aLogFolder = '' then
    raise EHttpApiServer.CreateFmt('LogStart(aLogFolder="")', []);
  if length(aLogFolder) > 212 then
    // http://msdn.microsoft.com/en-us/library/windows/desktop/aa364532
    raise EHttpApiServer.CreateFmt('LogStart(%s): too long path', [aLogFolder]);
  folder   := SynUnicode(aLogFolder);
  software := SynUnicode(aSoftwareName);
  log.SoftwareNameLength  := length(software) * 2; // in bytes
  log.SoftwareName        := pointer(software);
  log.DirectoryNameLength := length(folder) * 2;
  log.DirectoryName       := pointer(folder);      // in bytes
  log.Format := HTTP_LOGGING_TYPE(aType);
  if aType = hltNCSA then
    aLogFields := [hlfDate .. hlfSubStatus];
  log.Fields := integer(aLogFields);
  log.RolloverType := HTTP_LOGGING_ROLLOVER_TYPE(aRolloverType);
  if aRolloverType = hlrSize then
    log.RolloverSize := aRolloverSize;
  Ensure(hSetUrlGroupProperty,
    Http.SetUrlGroupProperty(fUrlGroupID,
      HttpServerLoggingProperty, @log, SizeOf(log)));
  // on success, enable the use of these HTTP_LOGGING_INFO settings
  fLogging := true;
end;

procedure THttpApiServer.LogStop;
begin
  if self <> nil then
    fLogging := false;
end;

procedure THttpApiServer.SetAuthenticationSchemes(
  schemes: THttpApiRequestAuthentications; const DomainName, Realm: SynUnicode);
var
  auth: HTTP_SERVER_AUTHENTICATION_INFO;
begin
  if self = nil then
    exit;
  EHttpApiServer.RaiseCheckApi2(hSetUrlGroupProperty);
  fAuthenticationSchemes := schemes;
  FillcharFast(auth, SizeOf(auth), 0);
  auth.Flags := 1;
  auth.AuthSchemes := byte(schemes);
  auth.ReceiveMutualAuth := true;
  if haBasic in schemes then
  begin
    auth.BasicParams.RealmLength := Length(Realm) * 2; // in bytes
    auth.BasicParams.Realm       := pointer(Realm);
  end;
  if haDigest in schemes then
  begin
    auth.DigestParams.DomainNameLength := Length(DomainName) * 2; // in bytes
    auth.DigestParams.DomainName       := pointer(DomainName);
    auth.DigestParams.RealmLength      := Length(Realm) * 2;      // in bytes
    auth.DigestParams.Realm            := pointer(Realm);
  end;
  Ensure(hSetUrlGroupProperty,
    Http.SetUrlGroupProperty(fUrlGroupID,
      HttpServerAuthenticationProperty, @auth, SizeOf(auth)));
end;

procedure THttpApiServer.SetTimeOutLimits(aEntityBody, aDrainEntityBody,
  aRequestQueue, aIdleConnection, aHeaderWait, aMinSendRate: cardinal);
var
  timeout: HTTP_TIMEOUT_LIMIT_INFO;
begin
  if self = nil then
    exit;
  EHttpApiServer.RaiseCheckApi2(hSetUrlGroupProperty);
  FillcharFast(timeout, SizeOf(timeout), 0);
  timeout.Flags := 1;
  timeout.EntityBody      := aEntityBody;
  timeout.DrainEntityBody := aDrainEntityBody;
  timeout.RequestQueue    := aRequestQueue;
  timeout.IdleConnection  := aIdleConnection;
  timeout.HeaderWait      := aHeaderWait;
  timeout.MinSendRate     := aMinSendRate;
  Ensure(hSetUrlGroupProperty,
    Http.SetUrlGroupProperty(fUrlGroupID,
      HttpServerTimeoutsProperty, @timeout, SizeOf(timeout)));
end;

procedure THttpApiServer.DoAfterResponse(Ctxt: THttpServerRequest;
  const Referer: RawUtf8; StatusCode: cardinal; Elapsed, Received, Sent: QWord);
var
  ctx: TOnHttpServerAfterResponseContext;
begin
  if Assigned(fOnAfterResponse) then
  try
    ctx.Connection := Ctxt.ConnectionID;
    ctx.User := pointer(Ctxt.AuthenticatedUser);
    ctx.Method := pointer(Ctxt.Method);
    ctx.Host := pointer(Ctxt.Host);
    ctx.Url := pointer(Ctxt.Url);
    ctx.Referer := pointer(Referer);
    ctx.UserAgent := pointer(Ctxt.UserAgent);
    ctx.RemoteIP := pointer(Ctxt.RemoteIP);
    ctx.Flags := Ctxt.ConnectionFlags;
    ctx.State := hrsResponseDone;
    ctx.StatusCode := StatusCode;
    ctx.ElapsedMicroSec := Elapsed;
    ctx.Received := Received;
    ctx.Sent := Sent;
    ctx.Tix64 := 0;
    fOnAfterResponse(ctx); // e.g. THttpLogger or THttpAnalyzer
  except
    on E: Exception do // paranoid
      fOnAfterResponse := nil; // won't try again
  end;
end;


{ ****************** THttpApiWebSocketServer Over Windows http.sys Module }

{ THttpApiWebSocketServerProtocol }

const
  WebSocketConnectionCapacity = 1000;

function THttpApiWebSocketServerProtocol.AddConnection(
  aConn: PHttpApiWebSocketConnection): integer;
var
  i: PtrInt;
begin
  if fFirstEmptyConnectionIndex >= fConnectionsCapacity - 1 then
  begin
    inc(fConnectionsCapacity, WebSocketConnectionCapacity);
    ReallocMem(fConnections, fConnectionsCapacity * SizeOf(PHttpApiWebSocketConnection));
    FillcharFast(fConnections^[fConnectionsCapacity - WebSocketConnectionCapacity],
      WebSocketConnectionCapacity * SizeOf(PHttpApiWebSocketConnection), 0);
  end;
  if fFirstEmptyConnectionIndex >= fConnectionsCount then
    fConnectionsCount := fFirstEmptyConnectionIndex + 1;
  fConnections[fFirstEmptyConnectionIndex] := aConn;
  result := fFirstEmptyConnectionIndex;
  for i := fFirstEmptyConnectionIndex + 1 to fConnectionsCount do
  begin
    if fConnections[i] = nil then
    begin
      fFirstEmptyConnectionIndex := i;
      Break;
    end;
  end;
end;

function THttpApiWebSocketServerProtocol.Broadcast(
  aBufferType: WEB_SOCKET_BUFFER_TYPE; aBuffer: pointer;
  aBufferSize: ULONG): boolean;
var
  i: PtrInt;
begin
  fSafe.Lock;
  try
    for i := 0 to fConnectionsCount - 1 do
      if Assigned(fConnections[i]) then
        fConnections[i].Send(aBufferType, aBuffer, aBufferSize);
  finally
    fSafe.UnLock;
  end;
  result := true;
end;

function THttpApiWebSocketServerProtocol.Close(index: integer;
  aStatus: WEB_SOCKET_CLOSE_STATUS; aBuffer: pointer; aBufferSize: ULONG): boolean;
var
  conn: PHttpApiWebSocketConnection;
begin
  result := false;
  if cardinal(index) < cardinal(fConnectionsCount) then
  begin
    conn := fConnections^[index];
    if (conn <> nil) and
       (conn.fState = wsOpen) then
    begin
      conn.Close(aStatus, aBuffer, aBufferSize);
      result := true;
    end;
  end;
end;

constructor THttpApiWebSocketServerProtocol.Create(const aName: RawUtf8;
  aManualFragmentManagement: boolean; aServer: THttpApiWebSocketServer;
  const aOnAccept: TOnHttpApiWebSocketServerAcceptEvent;
  const aOnMessage: TOnHttpApiWebSocketServerMessageEvent;
  const aOnConnect: TOnHttpApiWebSocketServerConnectEvent;
  const aOnDisconnect: TOnHttpApiWebSocketServerDisconnectEvent;
  const aOnFragment: TOnHttpApiWebSocketServerMessageEvent);
begin
  if aManualFragmentManagement and
     (not Assigned(aOnFragment)) then
    raise EWebSocketApi.CreateFmt(
      'Error register WebSocket protocol. Protocol %s does not use buffer, ' +
      'but OnFragment handler is not assigned', [aName]);
  fSafe.Init;
  fPendingForClose := TSynList.Create;
  fName := aName;
  fManualFragmentManagement := aManualFragmentManagement;
  fServer := aServer;
  fOnAccept := aOnAccept;
  fOnMessage := aOnMessage;
  fOnConnect := aOnConnect;
  fOnDisconnect := aOnDisconnect;
  fOnFragment := aOnFragment;
  fConnectionsCapacity := WebSocketConnectionCapacity;
  fConnectionsCount := 0;
  fFirstEmptyConnectionIndex := 0;
  fConnections := AllocMem(fConnectionsCapacity * SizeOf(PHttpApiWebSocketConnection));
end;

destructor THttpApiWebSocketServerProtocol.Destroy;
var
  i: PtrInt;
  conn: PHttpApiWebSocketConnection;
begin
  fSafe.Lock;
  try
    for i := 0 to fPendingForClose.Count - 1 do
    begin
      conn := fPendingForClose[i];
      if Assigned(conn) then
      begin
        conn.DoOnDisconnect();
        conn.Disconnect();
        Dispose(conn);
      end;
    end;
    fPendingForClose.Free;
  finally
    fSafe.UnLock;
  end;
  fSafe.Done;
  FreeMem(fConnections);
  fConnections := nil;
  inherited;
end;

procedure THttpApiWebSocketServerProtocol.DoShutdown;
var
  i: PtrInt;
  conn: PHttpApiWebSocketConnection;
const
  sReason = 'Server shutdown';
begin
  fSafe.Lock;
  try
    for i := 0 to fConnectionsCount - 1 do
    begin
      conn := fConnections[i];
      if Assigned(conn) then
      begin
        RemoveConnection(i);
        conn.fState := wsClosedByShutdown;
        conn.fBuffer := sReason;
        conn.fCloseStatus := WEB_SOCKET_ENDPOINT_UNAVAILABLE_CLOSE_STATUS;
        conn.Close(WEB_SOCKET_ENDPOINT_UNAVAILABLE_CLOSE_STATUS,
          pointer(conn.fBuffer), Length(conn.fBuffer));
// IocpPostQueuedStatus(fServer.fThreadPoolServer.FRequestQueue, 0, 0, @conn.fOverlapped);
      end;
    end;
  finally
    fSafe.UnLock;
  end;
end;

procedure THttpApiWebSocketServerProtocol.RemoveConnection(index: integer);
begin
  fPendingForClose.Add(fConnections[index]);
  fConnections[index] := nil;
  if fFirstEmptyConnectionIndex > index then
    fFirstEmptyConnectionIndex := index;
end;

function THttpApiWebSocketServerProtocol.Send(index: integer;
  aBufferType: WEB_SOCKET_BUFFER_TYPE; aBuffer: pointer; aBufferSize: ULONG): boolean;
var
  conn: PHttpApiWebSocketConnection;
begin
  result := false;
  if (index >= 0) and
     (index < fConnectionsCount) then
  begin
    conn := fConnections^[index];
    if (conn <> nil) and
       (conn.fState = wsOpen) then
    begin
      conn.Send(aBufferType, aBuffer, aBufferSize);
      result := true;
    end;
  end;
end;


 { THttpApiWebSocketConnection }

function THttpApiWebSocketConnection.TryAcceptConnection(
  aProtocol: THttpApiWebSocketServerProtocol;
  Ctxt: THttpServerRequestAbstract; aNeedHeader: boolean): boolean;
var
  req: PHTTP_REQUEST;
  reqhead: WEB_SOCKET_HTTP_HEADER_ARR;
  srvhead: PWEB_SOCKET_HTTP_HEADER;
  srvheadcount: ULONG;
begin
  fState := wsConnecting;
  fBuffer := '';
  fWSHandle := nil;
  fLastActionContext := nil;
  FillcharFast(fOverlapped, SizeOf(fOverlapped), 0);
  fProtocol := aProtocol;
  req := PHTTP_REQUEST((Ctxt as THttpServerRequest).HttpApiRequest);
  fIndex := fProtocol.fFirstEmptyConnectionIndex;
  fOpaqueHTTPRequestId := req^.RequestId;
  if (fProtocol = nil) or
     (Assigned(fProtocol.OnAccept) and
      not fProtocol.OnAccept(Ctxt as THttpServerRequest, Self)) then
  begin
    result := false;
    exit;
  end;
  EWebSocketApi.RaiseOnError(hCreateServerHandle,
    WebSocketApi.CreateServerHandle(nil, 0, fWSHandle));
  reqhead := HttpSys2ToWebSocketHeaders(req^.headers);
  if aNeedHeader then
    result := WebSocketApi.BeginServerHandshake(fWSHandle,
      pointer(fProtocol.name), nil, 0, @reqhead[0], Length(reqhead), srvhead,
      srvheadcount) = S_OK
  else
    result := WebSocketApi.BeginServerHandshake(fWSHandle, nil, nil, 0,
      pointer(reqhead), Length(reqhead),
      srvhead, srvheadcount) = S_OK;
  if result then
  try
    Ctxt.OutCustomHeaders := WebSocketHeadersToText(srvhead, srvheadcount);
  finally
    result := WebSocketApi.EndServerHandshake(fWSHandle) = S_OK;
  end;
  if not result then
    Disconnect
  else
    fLastReceiveTickCount := 0;
end;

procedure THttpApiWebSocketConnection.DoOnMessage(
  aBufferType: WEB_SOCKET_BUFFER_TYPE; aBuffer: pointer; aBufferSize: ULONG);

  procedure PushFragmentIntoBuffer;
  var
    l: integer;
  begin
    l := Length(fBuffer);
    SetLength(fBuffer, l + integer(aBufferSize));
    MoveFast(aBuffer^, fBuffer[l + 1], aBufferSize);
  end;

begin
  if fProtocol = nil then
    exit;
  if (aBufferType = WEB_SOCKET_UTF8_FRAGMENT_BUFFER_TYPE) or
     (aBufferType = WEB_SOCKET_BINARY_FRAGMENT_BUFFER_TYPE) then
  begin
    // Fragment
    if not fProtocol.ManualFragmentManagement then
      PushFragmentIntoBuffer;
    if Assigned(fProtocol.OnFragment) then
      fProtocol.OnFragment(self, aBufferType, aBuffer, aBufferSize);
  end
  else
  begin
    // last Fragment
    if Assigned(fProtocol.OnMessage) then
    begin
      if fProtocol.ManualFragmentManagement then
        fProtocol.OnMessage(self, aBufferType, aBuffer, aBufferSize)
      else
      begin
        PushFragmentIntoBuffer;
        fProtocol.OnMessage(self, aBufferType, pointer(fBuffer), Length(fBuffer));
        fBuffer := '';
      end;
    end;
  end;
end;

procedure THttpApiWebSocketConnection.DoOnConnect;
begin
  if (fProtocol <> nil) and
     Assigned(fProtocol.OnConnect) then
    fProtocol.OnConnect(self);
end;

procedure THttpApiWebSocketConnection.DoOnDisconnect;
begin
  if (fProtocol <> nil) and
     Assigned(fProtocol.OnDisconnect) then
    fProtocol.OnDisconnect(self, fCloseStatus, pointer(fBuffer), length(fBuffer));
end;

function THttpApiWebSocketConnection.ReadData(const WebsocketBufferData): integer;
var
  err: HRESULT;
  read: cardinal;
  buf: WEB_SOCKET_BUFFER_DATA absolute WebsocketBufferData;
begin
  result := 0;
  if fWSHandle = nil then
    exit;
  err := Http.ReceiveRequestEntityBody(fProtocol.fServer.fReqQueue,
    fOpaqueHTTPRequestId, 0, buf.pbBuffer, buf.ulBufferLength, read,
    @self.fOverlapped);
  case err of
    // On page reload Safari do not send a WEB_SOCKET_INDICATE_RECEIVE_COMPLETE_ACTION
    // with BufferType = WEB_SOCKET_CLOSE_BUFFER_TYPE, instead it send a dummy packet
    // (WEB_SOCKET_RECEIVE_FROM_NETWORK_ACTION) and terminate socket
    // see forum discussion https://synopse.info/forum/viewtopic.php?pid=27125
    ERROR_HANDLE_EOF:
      result := -1;
    ERROR_IO_PENDING:
      ; //
    NO_ERROR:
      ; //
  else
    // todo: close connection?
  end;
end;

procedure THttpApiWebSocketConnection.WriteData(const WebsocketBufferData);
var
  err: HRESULT;
  inmem: HTTP_DATA_CHUNK_INMEMORY;
  writ: cardinal;
  buf: WEB_SOCKET_BUFFER_DATA absolute WebsocketBufferData;
begin
  if fWSHandle = nil then
    exit;
  writ := 0;
  inmem.DataChunkType := hctFromMemory;
  inmem.pBuffer := buf.pbBuffer;
  inmem.BufferLength := buf.ulBufferLength;
  err := Http.SendResponseEntityBody(fProtocol.fServer.fReqQueue,
    fOpaqueHTTPRequestId, HTTP_SEND_RESPONSE_FLAG_BUFFER_DATA or
    HTTP_SEND_RESPONSE_FLAG_MORE_DATA, 1, @inmem, writ, nil, nil,
    @fProtocol.fServer.fSendOverlaped);
  case err of
    ERROR_HANDLE_EOF:
      Disconnect;
    ERROR_IO_PENDING:
      ; //
    NO_ERROR:
      ; //
  else
    // todo: close connection?
  end;
end;

procedure THttpApiWebSocketConnection.CheckIsActive(Tix64: Int64);
var
  elapsed, delay: Int64;
begin
  if (@self = nil) or
     (fLastReceiveTickCount <= 0) or
     (fProtocol.fServer.fPingTimeout <= 0) then
    exit;
  elapsed := Tix64 - fLastReceiveTickCount;
  delay := fProtocol.fServer.PingTimeout * 1000;
  if elapsed >= delay then
    if elapsed > 2 * delay then
    begin
      fProtocol.RemoveConnection(fIndex);
      fState := wsClosedByGuard;
      fCloseStatus := WEB_SOCKET_ENDPOINT_UNAVAILABLE_CLOSE_STATUS;
      fBuffer := 'Closed after ping timeout';
      IocpPostQueuedStatus(
        fProtocol.fServer.fThreadPoolServer.FRequestQueue, 0, nil, @fOverlapped);
    end
    else
      Ping;
end;

procedure THttpApiWebSocketConnection.Disconnect;
var //Err: HRESULT; //todo: handle error
  chunk: HTTP_DATA_CHUNK_INMEMORY;
  writ: cardinal;
begin
  WebSocketApi.AbortHandle(fWSHandle);
  WebSocketApi.DeleteHandle(fWSHandle);
  fWSHandle := nil;
  chunk.DataChunkType := hctFromMemory;
  chunk.pBuffer := nil;
  chunk.BufferLength := 0;
  Http.SendResponseEntityBody(fProtocol.fServer.fReqQueue, fOpaqueHTTPRequestId,
    HTTP_SEND_RESPONSE_FLAG_DISCONNECT, 1, @chunk, writ, nil, nil, nil);
end;

procedure THttpApiWebSocketConnection.BeforeRead;
begin
  // if reading is in progress then try read messages else try receive new messages
  if fState in [wsOpen, wsClosing] then
  begin
    if Assigned(fLastActionContext) then
    begin
      EWebSocketApi.RaiseOnError(hCompleteAction,
        WebSocketApi.CompleteAction(fWSHandle, fLastActionContext,
        fOverlapped.InternalHigh));
      fLastActionContext := nil;
    end
    else
      EWebSocketApi.RaiseOnError(hReceive,
        WebSocketApi.Receive(fWSHandle, nil, nil));
  end
  else
    raise EWebSocketApi.CreateFmt(
      'THttpApiWebSocketConnection.BeforeRead state is not wsOpen (%d)',
      [ord(fState)]);
end;

const
  C_WEB_SOCKET_BUFFER_SIZE = 2;

type
  TWebSocketBufferDataArr = array[0..C_WEB_SOCKET_BUFFER_SIZE - 1] of WEB_SOCKET_BUFFER_DATA;

function THttpApiWebSocketConnection.ProcessActions(
  ActionQueue: WEB_SOCKET_ACTION_QUEUE): boolean;
var
  buf: TWebSocketBufferDataArr;
  bufcount: ULONG;
  buftyp: WEB_SOCKET_BUFFER_TYPE;
  action: WEB_SOCKET_ACTION;
  appctxt: pointer;
  actctxt: pointer;
  i: PtrInt;
  err: HRESULT;

  procedure CloseConnection;
  begin
    fProtocol.fSafe.Lock;
    try
      fProtocol.RemoveConnection(fIndex);
    finally
      fProtocol.fSafe.UnLock;
    end;
    EWebSocketApi.RaiseOnError(hCompleteAction,
      WebSocketApi.CompleteAction(fWSHandle, actctxt, 0));
  end;

begin
  result := true;
  repeat
    bufcount := Length(buf);
    EWebSocketApi.RaiseOnError(hGetAction,
      WebSocketApi.GetAction(fWSHandle, ActionQueue, @buf[0], bufcount,
      action, buftyp, appctxt, actctxt));
    case action of
      WEB_SOCKET_NO_ACTION:
        ;
      WEB_SOCKET_SEND_TO_NETWORK_ACTION:
        begin
          for i := 0 to bufcount - 1 do
            WriteData(buf[i]);
          if fWSHandle <> nil then
          begin
            err := WebSocketApi.CompleteAction(fWSHandle, actctxt, 0);
            EWebSocketApi.RaiseOnError(hCompleteAction, err);
          end;
          result := false;
          exit;
        end;
      WEB_SOCKET_INDICATE_SEND_COMPLETE_ACTION:
        ;
      WEB_SOCKET_RECEIVE_FROM_NETWORK_ACTION:
        begin
          for i := 0 to bufcount - 1 do
            if ReadData(buf[i]) = -1 then
            begin
              fState := wsClosedByClient;
              fBuffer := '';
              fCloseStatus := WEB_SOCKET_ENDPOINT_UNAVAILABLE_CLOSE_STATUS;
              CloseConnection;
            end;
          fLastActionContext := actctxt;
          result := false;
          exit;
        end;
      WEB_SOCKET_INDICATE_RECEIVE_COMPLETE_ACTION:
        begin
          fLastReceiveTickCount := mormot.core.os.GetTickCount64;
          if buftyp = WEB_SOCKET_CLOSE_BUFFER_TYPE then
          begin
            if fState = wsOpen then
              fState := wsClosedByClient
            else
              fState := wsClosedByServer;
            FastSetRawByteString(fBuffer, buf[0].pbBuffer, buf[0].ulBufferLength);
            fCloseStatus := buf[0].Reserved1;
            CloseConnection;
            result := false;
            exit;
          end
          else if buftyp = WEB_SOCKET_PING_PONG_BUFFER_TYPE then
          begin
            // todo: may be answer to client's ping
            EWebSocketApi.RaiseOnError(hCompleteAction,
              WebSocketApi.CompleteAction(fWSHandle, actctxt, 0));
            exit;
          end
          else if buftyp = WEB_SOCKET_UNSOLICITED_PONG_BUFFER_TYPE then
          begin
            // todo: may be handle this situation
            EWebSocketApi.RaiseOnError(hCompleteAction,
              WebSocketApi.CompleteAction(fWSHandle, actctxt, 0));
            exit;
          end
          else
          begin
            DoOnMessage(buftyp, buf[0].pbBuffer, buf[0].ulBufferLength);
            EWebSocketApi.RaiseOnError(hCompleteAction,
              WebSocketApi.CompleteAction(fWSHandle, actctxt, 0));
            exit;
          end;
        end
    else
      raise EWebSocketApi.CreateFmt('Invalid WebSocket action %d', [byte(action)]);
    end;
    err := WebSocketApi.CompleteAction(fWSHandle, actctxt, 0);
    if actctxt <> nil then
      EWebSocketApi.RaiseOnError(hCompleteAction, err);
  until {%H-}action = WEB_SOCKET_NO_ACTION;
end;

procedure THttpApiWebSocketConnection.InternalSend(
  aBufferType: WEB_SOCKET_BUFFER_TYPE; WebsocketBufferData: pointer);
begin
  EWebSocketApi.RaiseOnError(hSend,
    WebSocketApi.Send(fWSHandle, aBufferType, WebsocketBufferData, nil));
  ProcessActions(WEB_SOCKET_SEND_ACTION_QUEUE);
end;

procedure THttpApiWebSocketConnection.Send(aBufferType: WEB_SOCKET_BUFFER_TYPE;
  aBuffer: pointer; aBufferSize: ULONG);
var
  buf: WEB_SOCKET_BUFFER_DATA;
begin
  if fState <> wsOpen then
    exit;
  buf.pbBuffer := aBuffer;
  buf.ulBufferLength := aBufferSize;
  InternalSend(aBufferType, @buf);
end;

procedure THttpApiWebSocketConnection.Close(aStatus: WEB_SOCKET_CLOSE_STATUS;
  aBuffer: pointer; aBufferSize: ULONG);
var
  buf: WEB_SOCKET_BUFFER_DATA;
begin
  if fState = wsOpen then
    fState := wsClosing;
  buf.pbBuffer := aBuffer;
  buf.ulBufferLength := aBufferSize;
  buf.Reserved1 := aStatus;
  InternalSend(WEB_SOCKET_CLOSE_BUFFER_TYPE, @buf);
end;

procedure THttpApiWebSocketConnection.Ping;
begin
  InternalSend(WEB_SOCKET_PING_PONG_BUFFER_TYPE, nil);
end;


{ THttpApiWebSocketServer }

constructor THttpApiWebSocketServer.Create(
  aSocketThreadsCount, aPingTimeout: integer; const QueueName: SynUnicode;
  const aOnWSThreadStart, aOnWSThreadTerminate: TOnNotifyThread;
  ProcessOptions: THttpServerOptions);
begin
  inherited Create(QueueName, nil, nil, '', ProcessOptions);
  if not (WebSocketApi.WebSocketEnabled) then
    raise EWebSocketApi.Create('WebSocket API not supported');
  fPingTimeout := aPingTimeout;
  if fPingTimeout > 0 then
    fGuard := TSynWebSocketGuard.Create(Self);
  fOnWSThreadStart := aOnWSThreadStart;
  fOnWSThreadTerminate := aOnWSThreadTerminate;
  fThreadPoolServer := TSynThreadPoolHttpApiWebSocketServer.Create(Self,
    aSocketThreadsCount);
end;

function THttpApiWebSocketServer.GetRegisteredProtocols: THttpApiWebSocketServerProtocolDynArray;
begin
  fOwnedProtocolsSafe.Lock; // thread-safe by-reference copy
  result := fOwnedProtocols;
  fOwnedProtocolsSafe.UnLock;
end;

procedure THttpApiWebSocketServer.DestroyMainThread;
var
  i: PtrInt;
begin
  fGuard.Free;
  for i := 0 to Length(fOwnedProtocols) - 1 do
    fOwnedProtocols[i].DoShutdown;
  FreeAndNilSafe(fThreadPoolServer);
  ObjArrayClear(fOwnedProtocols);
  inherited DestroyMainThread;
end;

procedure THttpApiWebSocketServer.DoAfterResponse(Ctxt: THttpServerRequest;
  const Referer: RawUtf8; StatusCode: cardinal; Elapsed, Received, Sent: QWord);
begin
  if Assigned(fLastConnection) then
    IocpPostQueuedStatus(fThreadPoolServer.FRequestQueue, 0, nil,
      @fLastConnection.fOverlapped);
  inherited DoAfterResponse(Ctxt, Referer, StatusCode, Elapsed, Received, Sent);
end;

function THttpApiWebSocketServer.GetSendResponseFlags(Ctxt: THttpServerRequest): integer;
begin
  if (PHTTP_REQUEST(Ctxt.HttpApiRequest)^.UrlContext = WEB_SOCKET_URL_CONTEXT) and
     (fLastConnection <> nil) then
    result := HTTP_SEND_RESPONSE_FLAG_OPAQUE or
      HTTP_SEND_RESPONSE_FLAG_MORE_DATA or HTTP_SEND_RESPONSE_FLAG_BUFFER_DATA
  else
    result := inherited GetSendResponseFlags(Ctxt);
end;

function THttpApiWebSocketServer.UpgradeToWebSocket(
  Ctxt: THttpServerRequestAbstract): cardinal;
var
  proto: THttpApiWebSocketServerProtocol;
  protos: THttpApiWebSocketServerProtocolDynArray;
  i, j: PtrInt;
  req: PHTTP_REQUEST;
  p: PHTTP_UNKNOWN_HEADER;
  ch, chB: PUtf8Char;
  protoname: RawUtf8;
  specified: boolean;
begin
  result := HTTP_NOTFOUND;
  // search for explicit name specified in 'SEC-WEBSOCKET-PROTOCOL:' header
  proto := nil;
  protos := GetRegisteredProtocols; // local copy
  specified := false;
  req := PHTTP_REQUEST((Ctxt as THttpServerRequest).HttpApiRequest);
  p := req^.headers.pUnknownHeaders;
  for j := 1 to req^.headers.UnknownHeaderCount do
  begin
    if (p.NameLength = Length(sProtocolHeader)) and
       IdemPChar(p.pName, pointer(sProtocolHeader)) then
    begin
      specified := true;
      ch := p.pRawValue;
      while (proto = nil) and
            ((ch - p.pRawValue) < p.RawValueLength) do
      begin
        while ((ch - p.pRawValue) < p.RawValueLength) and
              (ch^ in [',', ' ']) do
          inc(ch);
        chB := ch;
        while ((ch - p.pRawValue) < p.RawValueLength) and
              not (ch^ in [',']) do
          inc(ch);
        FastSetString(protoname, chB, ch - chB);
        for i := 0 to Length(protos) - 1 do
          if protos[i].name = protoname then
          begin
            proto := protos[i];
            break;
          end;
      end;
      if proto <> nil then
        break;
    end;
    inc(p);
  end;
  if not specified and
     (proto = nil) and
     (Length(protos) = 1) then
    proto := protos[0]; // fallback to our single known protocol
  if proto = nil then
    exit;
  // add the connection for this protocol
  proto.fSafe.Lock;
  try
    New(fLastConnection);
    if fLastConnection.TryAcceptConnection(proto, Ctxt, specified) then
    begin
      proto.AddConnection(fLastConnection);
      result := HTTP_SWITCHINGPROTOCOLS;
    end
    else
    begin
      Dispose(fLastConnection);
      fLastConnection := nil;
      result := HTTP_NOTALLOWED;
    end;
  finally
    proto.fSafe.UnLock;
  end;
end;

function THttpApiWebSocketServer.AddUrlWebSocket(const aRoot, aPort: RawUtf8;
  Https: boolean; const aDomainName: RawUtf8; aRegisterUri: boolean): integer;
begin
  result := AddUrl(
    aRoot, aPort, Https, aDomainName, aRegisterUri, WEB_SOCKET_URL_CONTEXT);
end;

procedure THttpApiWebSocketServer.RegisterProtocol(const aName: RawUtf8;
  aManualFragmentManagement: boolean;
  const aOnAccept: TOnHttpApiWebSocketServerAcceptEvent;
  const aOnMessage: TOnHttpApiWebSocketServerMessageEvent;
  const aOnConnect: TOnHttpApiWebSocketServerConnectEvent;
  const aOnDisconnect: TOnHttpApiWebSocketServerDisconnectEvent;
  const aOnFragment: TOnHttpApiWebSocketServerMessageEvent);
var
  proto: THttpApiWebSocketServerProtocol;
begin
  if self = nil then
    exit;
  proto := THttpApiWebSocketServerProtocol.Create(aName,
    aManualFragmentManagement, self, aOnAccept, aOnMessage, aOnConnect,
    aOnDisconnect, aOnFragment);
  ObjArrayAdd(fOwnedProtocols, proto, fOwnedProtocolsSafe);
end;

function THttpApiWebSocketServer.Request(
  Ctxt: THttpServerRequestAbstract): cardinal;
begin
  if PHTTP_REQUEST(THttpServerRequest(Ctxt).HttpApiRequest).
       UrlContext = WEB_SOCKET_URL_CONTEXT then
    result := UpgradeToWebSocket(Ctxt)
  else
  begin
    result := inherited Request(Ctxt);
    fLastConnection := nil;
  end;
end;

procedure THttpApiWebSocketServer.SendServiceMessage;
begin
  IocpPostQueuedStatus(fThreadPoolServer.FRequestQueue, 0, nil, @fServiceOverlaped);
end;


{ TSynThreadPoolHttpApiWebSocketServer }

function TSynThreadPoolHttpApiWebSocketServer.NeedStopOnIOError: boolean;
begin
  // If connection closed by guard than ERROR_HANDLE_EOF or ERROR_OPERATION_ABORTED
  // can be returned - Other connections must work normally
  result := false;
end;

procedure TSynThreadPoolHttpApiWebSocketServer.OnThreadStart(Sender: TThread);
begin
  if Assigned(fServer.OnWSThreadStart) then
    fServer.OnWSThreadStart(Sender);
end;

procedure TSynThreadPoolHttpApiWebSocketServer.OnThreadTerminate(Sender: TThread);
begin
  if Assigned(fServer.OnWSThreadTerminate) then
    fServer.OnWSThreadTerminate(Sender);
end;

procedure TSynThreadPoolHttpApiWebSocketServer.Task(
  aCaller: TSynThreadPoolWorkThread; aContext: pointer);
var
  conn: PHttpApiWebSocketConnection;
begin
  if aContext = @fServer.fSendOverlaped then
    exit;
  if aContext = @fServer.fServiceOverlaped then
  begin
    if Assigned(fServer.OnServiceMessage) then
      fServer.OnServiceMessage;
    exit;
  end;
  conn := PHttpApiWebSocketConnection(aContext);
  if conn.fState = wsConnecting then
  begin
    conn.fState := wsOpen;
    conn.fLastReceiveTickCount := mormot.core.os.GetTickCount64;
    conn.DoOnConnect();
  end;
  if conn.fState in [wsOpen, wsClosing] then
    repeat
      conn.BeforeRead;
    until not conn.ProcessActions(WEB_SOCKET_RECEIVE_ACTION_QUEUE);
  if conn.fState in [wsClosedByGuard] then
    EWebSocketApi.RaiseOnError(hCompleteAction,
      WebSocketApi.CompleteAction(conn.fWSHandle, conn.fLastActionContext, 0));
  if conn.fState in
       [wsClosedByClient, wsClosedByServer, wsClosedByGuard, wsClosedByShutdown] then
  begin
    conn.DoOnDisconnect;
    if conn.fState = wsClosedByClient then
      conn.Close(conn.fCloseStatus, pointer(conn.fBuffer), length(conn.fBuffer));
    conn.Disconnect;
    conn.Protocol.fSafe.Lock;
    try
      conn.Protocol.fPendingForClose.Remove(conn);
    finally
      conn.Protocol.fSafe.UnLock;
    end;
    Dispose(conn);
  end;
end;

constructor TSynThreadPoolHttpApiWebSocketServer.Create(
  Server: THttpApiWebSocketServer; NumberOfThreads: integer);
begin
  fServer := Server;
  fOnThreadStart := OnThreadStart;
  fOnThreadTerminate := OnThreadTerminate;
  inherited Create(NumberOfThreads, Server.fReqQueue);
end;


{ TSynWebSocketGuard }

procedure TSynWebSocketGuard.Execute;
var
  i, j: PtrInt;
  proto: THttpApiWebSocketServerProtocol;
  protos: THttpApiWebSocketServerProtocolDynArray;
  tix: Int64;
begin
  repeat
    tix := mormot.core.os.GetTickCount64;
    protos := fServer.GetRegisteredProtocols; // local copy
    for i := 0 to Length(protos) - 1 do
    begin
      proto := protos[i];
      proto.fSafe.Lock;
      try
        for j := 0 to proto.fConnectionsCount - 1 do
          if Terminated then
            exit
          else
            proto.fConnections^[j]^.CheckIsActive(tix);
      finally
        proto.fSafe.UnLock;
      end;
    end;
    inc(tix, fServer.PingTimeout * MilliSecsPerSec);
    while not Terminated and
          (mormot.core.os.GetTickCount64 < tix) do
      SleepHiRes(100);
  until Terminated;
end;

constructor TSynWebSocketGuard.Create(Server: THttpApiWebSocketServer);
begin
  fServer := Server;
  inherited Create({suspended=}false);
end;

{$endif USEWININET}

initialization
  assert(SizeOf(THttpPeerCacheMessage) = 192);

end.

