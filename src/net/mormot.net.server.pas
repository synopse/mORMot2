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
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.fmt,
  mormot.core.variants,
  mormot.core.zip,
  mormot.core.log,
  mormot.core.search,
  mormot.net.sock,
  mormot.net.http,
  mormot.lib.sspi,   // void unit on POSIX
  mormot.lib.gssapi, // void unit on Windows
  mormot.net.client,
  mormot.crypt.core,
  mormot.crypt.secure,
  mormot.crypt.factory;


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
    fReceived, fTimeout, fRebindRetry, fRebindCount, fFrameLen: integer;
    fFrameIfIndex: integer; // the receiving interface, if fPacketInfo is set
    fInitialBound, fAutoRebind, fFrameOwned, fPacketInfo: boolean;
    fBindAddress, fBindPort: RawUtf8;
    function DoBind: TNetResult; virtual;
    function GetIPWithPort: RawUtf8;
    /// will loop for any pending UDP frame, and execute FrameReceived method
    procedure DoExecute; override;
    // this is the main processing method for all incoming frames
    procedure OnFrameReceived(len: integer; var remote: TNetAddr); virtual; abstract;
    procedure OnIdle(tix64: Int64); virtual; // called every second
    procedure OnShutdown; virtual; abstract;
  public
    /// initialize and bind the server instance, in non-suspended state
    constructor Create(LogClass: TSynLogClass; const BindAddress, BindPort,
      ProcessName: RawUtf8; TimeoutMS: integer); reintroduce;
    /// finalize the processing thread
    destructor Destroy; override;
    /// notify the UDP main thread that it is finished
    procedure TerminateAndWaitFinished(TimeOutMs: integer = 5000); override;
    /// low-level access to the bound UDP socket (for debugging purposes)
    property Sock: TNetSocket
      read fSock;
    property BindAddress: RawUtf8
      read fBindAddress write fBindAddress;
    property BindPort: RawUtf8
      read fBindPort write fBindPort;
  published
    property IPWithPort: RawUtf8
      read GetIPWithPort;
    property Received: integer
      read fReceived;
    property AutoRebind: boolean
      read fAutoRebind write fAutoRebind;
    property RebindRetry: integer
      read fRebindRetry;
    property RebindCount: integer
      read fRebindCount;
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
    /// the HTTP error code for a Rewrite() with an integer ToUri (e.g. '404')
    ToUriErrorStatus: word;
    /// the URI method to be used after ToUri rewrite
    ToUriMethod: TUriRouterMethod;
    /// some internal flags corresponding to this URI rewrite definition
    ToUriFlags: set of (tufHasParameters);
    /// the size of all ToUriPosLen[] static content
    ToUriStaticLen: cardinal;
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
    function GetFullUrl: RawUtf8; virtual;
    procedure DoPurgeHeaders;
    procedure ProcessErrorMessage;
    procedure ProcessStaticFile(var Context: THttpRequestContext; CompressGz: integer);
    procedure ProcessOutStream(var Context: THttpRequestContext);
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
    /// append a "Repr-Digest:" HTTP header to the request output
    procedure SetOutReprDigest(const Hash: THashDigest);
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
    /// for THttpApiServer, input parameter containing the caller full URL
    property FullUrl: RawUtf8
      read GetFullUrl;
  end;
  /// meta-class of HTTP server requests instances
  THttpServerRequestClass = class of THttpServerRequest;

  /// available HTTP server options
  // - some THttpServerGeneric classes may have only partial support of them
  // - hsoHeadersUnfiltered will store all headers, not only relevant (i.e.
  // include raw Content-Length, Content-Type and Content-Encoding entries)
  // - hsoHeadersInterning triggers TRawUtf8Interning to reduce memory usage
  // - hsoHeadersSanitize will search and reject any #0 in headers lines
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
  // - hsoTextError will return a small and non-verbose UTF-8 text in case of
  // HTTP errors, instead of the default human-friendly HTML page
  // - hsoIocpWriteDirect try to reduce IOCP async sending contention on Windows
  THttpServerOption = (
    hsoHeadersUnfiltered,
    hsoHeadersInterning,
    hsoHeadersSanitize,
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
    hsoRejectBotUserAgent,
    hsoTextError,
    hsoIocpWriteDirect);

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
    fOnBodyDownload: TOnHttpServerBodyDownload;
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
    fOnIdle: TOnPollSocketsIdle;
    function GetApiVersion: RawUtf8; virtual; abstract;
    procedure SetRouterClass(aRouter: TRadixTreeNodeClass);
    procedure SetServerName(const aName: RawUtf8); virtual;
    procedure SetOptions(opt: THttpServerOptions);
    procedure SetOnRequest(const aRequest: TOnHttpServerRequest); virtual;
    procedure SetOnBeforeBody(const aEvent: TOnHttpServerBeforeBody); virtual;
    procedure SetOnBodyDownload(const aEvent: TOnHttpServerBodyDownload); virtual;
    procedure SetOnBeforeRequest(const aEvent: TOnHttpServerRequest); virtual;
    procedure SetOnAfterRequest(const aEvent: TOnHttpServerRequest); virtual;
    procedure SetOnAfterResponse(const aEvent: TOnHttpServerAfterResponse); virtual;
    procedure SetMaximumAllowedContentLength(aMax: Int64); virtual;
    procedure SetRemoteIPHeader(const aHeader: RawUtf8); virtual;
    procedure SetRemoteConnIDHeader(const aHeader: RawUtf8); virtual;
    function GetHttpQueueLength: cardinal; virtual; abstract;
    procedure SetHttpQueueLength(aValue: cardinal); virtual; abstract;
    function GetConnectionsActive: cardinal; virtual; abstract;
    procedure DoIdle(tix64: Int64; sec32: cardinal); virtual;
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
    // - the first registered algorithm will be the preferred one for compression
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
    /// event handler called just before the body is downloaded from the client
    // - could return a TStream instance (e.g. a TFileStreamEx spooling into a
    // local temporary file) to receive the incoming request body with a fixed
    // memory usage, whatever its size - see TOnHttpServerBodyDownload
    // - only implemented by socket-based servers, i.e. THttpServer and
    // THttpAsyncServer classes, not by THttpApiServer
    // - warning: this handler must be thread-safe (can be called by several
    // threads simultaneously)
    property OnBodyDownload: TOnHttpServerBodyDownload
      read fOnBodyDownload write SetOnBodyDownload;
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
    /// this callback would be called on idle state, typically every few seconds
    // - any implementation should not be blocking for a long period
    property OnIdle: TOnPollSocketsIdle
      read fOnIdle write fOnIdle;
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


/// hash a normalized URL and the "Etag:" or "Last-Modified:" headers
// - could be used to identify a HTTP resource as a binary hash on a given server
// - aHeaders could be supplied as nil so that only the URI resource is hashed
// - aUpCaseUri=true would force aUri to be hashed as uppercase
// - returns 0 if aUrl/aHeaders have not enough information
// - returns the number of hash bytes written to aDigest.Bin
function HttpRequestHash(aAlgo: THashAlgo; const aUri: TUri; aHeaders: PUtf8Char;
  out aDigest: THashDigest; aUpCaseUri: boolean = false): integer;

/// hash an URL and the "Etag:" or "Last-Modified:" headers into 32 ascii chars
// - aHeaders could be supplied as nil so that only the URI resource is hashed
// - aUpCaseUri=true would force aUri to be hashed as uppercase
// - using SHA-256 and lowercase Base-32 encoding, so perfect for a file name
// - with Base-32, 32 chars means 160-bit or 20 bytes into aDig^ binary hash
function HttpRequestHashBase32(const aUri: TUri; aName: PShort32 = nil;
  aHeaders: PUtf8Char = nil; aDig: PHash160 = nil; aUpCaseUri: boolean = false): boolean;


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
    function GetAborted: boolean; override;
    // from TSynThreadPoolTHttpServer.Task
    procedure TaskProcess(aCaller: TSynThreadPoolWorkThread); virtual;
    function TaskProcessBody(aCaller: TSynThreadPoolWorkThread;
      aHeaderResult: THttpServerSocketGetRequestResult): boolean;
    procedure DownloadBody; {$ifdef HASINLINE} inline; {$endif}
    procedure GetBodyInMemory;
    procedure GetBodyStream;
    // implement fServer.OnBodyDownload in GetRequest - false if rejected as 415
    function DoOnBodyDownload: boolean; virtual;
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
    {$ifndef USE_THREADWINIOCP}
    function QueueLength: integer; override;
    {$endif USE_THREADWINIOCP}
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
    fAuthorizeNegotiateCbtHash: RawByteString;
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
    procedure DoIdle(tix64: Int64; sec32: cardinal); override;
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
    function GetApiVersion: RawUtf8; override; // = SocketApiVersion
    function GetExecuteState: THttpServerExecuteState; virtual; abstract;
    function GetBanned: THttpAcceptBan; virtual; abstract;
    function GetRegisterCompressGzStatic: boolean;
    procedure SetRegisterCompressGzStatic(Value: boolean);
    function ComputeWwwAuthenticate(Opaque: Int64): RawUtf8;
    function ComputeRejectBody(var Body: RawByteString; Opaque: Int64;
      Status: integer; const ExtraHeader: RawUtf8 = ''): boolean;
      virtual; // return true for grWwwAuthenticate - ExtraHeader needs #13#10
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
    // - optional CbtHash is the binary tls-server-end-point certificate digest;
    // if supplied, a matching client channel binding is mandatory
    procedure SetAuthorizeNegotiate(const CbtHash: RawByteString = '');
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
    /// set the THttpPartials instance used for progressive downloads
    // - for internal use e.g. in mormot.net.peercache
    procedure SetProgressiveRequests(aPartials: THttpPartials);
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
  // - consider using THttpAsyncServer from mormot.net.async for any realistic
  // HTTP server, e.g. from web browsers, especially if simultaneous concurrent
  // connections are possible; use THttpServer only for small/internal services,
  // or behind a safe HTTP/1.0 reverse proxy
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
  // - some browsers (especially Chromium-based like Chrome or Edge) initiate
  // speculative/preconnect idle TCP connection, which would block the threads
  // from the thread pool for no benefit: do not use THttpServer from browsers
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
    fThreadPoolRetrieveAbortDelay: cardinal;
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
    procedure DoIdle(tix64: Int64; sec32: cardinal); override;
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
    /// milliseconds delay to reject a connection from the thread pool
    // - default 5000ms seems safe for early detection of speculative/preconnect
    // idle TCP connection (e.g. from Chromium/Edge browsers)
    // - set 0 will disable this feature and fallback to the minimum value of
    // HeaderRetrieveAbortDelay or ServerKeepAliveTimeOut properties; otherwise,
    // don't put a too small value here (<500 ms) especially on slow networks
    property ThreadPoolRetrieveAbortDelay: cardinal
      read fThreadPoolRetrieveAbortDelay write fThreadPoolRetrieveAbortDelay;
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
// - typical usage is for Single-Sign On credential entering, or for testing
// - you can set aResponseContentType = STATICFILE_CONTENT_TYPE to serve a file
// - raise an Exception on binding error, and returns false on timeout
// - returns true on success, with encoded parameters as aParams - and the
// received URL/Method/Content values as aParams.U['url'/'method'/'content']
function EphemeralHttpServer(const aPort: RawUtf8; out aParams: TDocVariantData;
  aTimeOutSecs: integer = 60; aLogClass: TSynLogClass = nil;
  const aResponse: RawUtf8 = 'You can close this window.';
  aMethods: TUriMethods = [mGET, mHEAD, mPOST]; aOptions: THttpServerOptions = [];
  const aResponseContentType: RawUtf8 = ''): boolean; overload;

/// create an ephemeral socket-based HTTP Server instance with a fixed response
// - typical usage is for testing
function EphemeralHttpServer(aLogClass: TSynLogClass; const aResponse: RawUtf8;
  aMethods: TUriMethods = [mGET, mHEAD]; aOptions: THttpServerOptions = [];
  const aResponseContentType: RawUtf8 = ''): THttpServer; overload;


implementation


{ ******************** Abstract UDP Server }

{ TUdpServerThread }

procedure TUdpServerThread.OnIdle(tix64: Int64);
begin
  // do nothing by default
end;

function TUdpServerThread.DoBind: TNetResult;
begin
  fInitialBound := false;
  fLogClass.Add.Log(sllDebug, 'DoBind %:%', [fBindAddress, fBindPort], self);
  result := NewSocket(fBindAddress, fBindPort, nlUdp, {bind=}true,
    fTimeout, fTimeout, fTimeout, 10, fSock, @fSockAddr);
  if (result = nrOK) and
     fPacketInfo then
    result := fSock.SetPacketInfo(true); // for RecvFromIf() in DoExecute
  if result <> nrOK then
    fLogClass.Add.Log(sllWarning, 'DoBind=%', [NetLastErrorMsg], self);
  fInitialBound := true; // notify DoExecute() ASAP that fSock was set (or not)
end;

constructor TUdpServerThread.Create(LogClass: TSynLogClass;
  const BindAddress, BindPort, ProcessName: RawUtf8; TimeoutMS: integer);
var
  ident: RawUtf8;
  res: TNetResult;
begin
  fBindAddress := BindAddress;
  fBindPort := BindPort;
  fTimeout := TimeoutMS;
  if fFrame = nil then // allocate a 64KB UDP frame buffer (if not yet)
  begin
    fFrameLen := SizeOf(fFrame^);
    GetMem(fFrame, fFrameLen);
    fFrameOwned := true;
  end;
  ident := ProcessName;
  if ident = '' then
    FormatUtf8('udp%srv', [fBindPort], ident);
  LogClass.Add.Log(sllTrace, 'Create: bind %:% for input requests on %',
    [fBindAddress, fBindPort, ident], self);
  inherited Create({suspended=}false, nil, nil, LogClass, ident);
  res := DoBind;
  if res = nrOk then
    exit; // successfully bound to this UDP port
  // Windows seems to require this to avoid breaking the process on error
  {$ifdef OSWINDOWS}
  Resume{%H-}; // force Execute/DoExecute launch
  SleepHiRes(10);
  {$endif OSWINDOWS}
  // on binding error, raise exception before the thread is actually created
  raise EUdpServer.Create('Create binding error on %s:%s', self,
    [BindAddress, BindPort], res);
end;

destructor TUdpServerThread.Destroy;
var
  ilog: ISynLog;
begin
  fLogClass.EnterLocal(ilog, 'Destroy: ending % - processing=%',
    [fProcessName, fProcessing], self);
  // finalize this thread process
  TerminateAndWaitFinished;
  fLogClass.Add.Log(sllDebug, 'Destroy: TerminateAndWaitFinished Processing=% [%]',
    [fProcessing, fExecuteMessage], self);
  inherited Destroy;
  if fSock <> nil then
    fSock.ShutdownAndClose({rdwr=}true);
  if fFrameOwned then
    FreeMem(fFrame);
end;

procedure TUdpServerThread.TerminateAndWaitFinished(TimeOutMs: integer);
var
  sock: TNetSocket;
begin
  // notify thread termination (if not already done)
  Terminate;
  // try to release fSock.WaitFor(1000) in DoExecute
  if (fSock = nil) or
     not fProcessing then
    exit;
  {$ifdef OSPOSIX} // a broadcast address won't reach DoExecute
  if (fSockAddr.IP4 and $ff000000) = $ff000000 then // check x.x.x.255
  begin
    fSock.ShutdownAndClose({rdwr=}true); // will release acept() ASAP
    exit;
  end;
  {$endif OSPOSIX}
  sock := fSockAddr.NewSocket(nlUdp);
  if sock = nil then
    fLogClass.Add.Log(sllTrace,
      'TerminateAndWaitFinished: error creating final socket', self)
  else
  begin
    fLogClass.Add.Log(sllTrace,
      'TerminateAndWaitFinished: send final packet', self);
    sock.SetSendTimeout(10);
    sock.SendTo(pointer(UDP_SHUTDOWN), length(UDP_SHUTDOWN), fSockAddr);
    sock.ShutdownAndClose(false);
  end;
end;

function TUdpServerThread.GetIPWithPort: RawUtf8;
begin
  fSockAddr.IPWithPort(result);
end;

procedure TUdpServerThread.DoExecute;
var
  len: integer;
  tix64: Int64;
  tix, lasttix, delay: cardinal;
  res: TNetResult;
  ev: TNetEvents;
  remote: TNetAddr;
begin
  lasttix := 0;
  // main server process loop
  if not fInitialBound then
    SleepHiRes(100, fInitialBound, {Bound value done=}true);
  if fSock = nil then // paranoid check
    FormatUtf8('%.DoExecute: % Bind failed', [self, fProcessName], fExecuteMessage)
  else
  repeat
    // inner loop handling receiving frames from bound fSock
    while not Terminated do
    begin
      ev := fSock.WaitFor(1000, [neRead, neError]);
      if Terminated then
      begin
        fLogClass.Add.Log(sllDebug, 'DoExecute: Terminated', self);
        break;
      end;
      if neRead in ev then // ev=[neRead,neError] for ICMP port unreachable
      begin
        res := fSock.RecvPending(len);
        if res = nrOk then
        begin
          if len > 0 then
          begin
            // some UDP packet received
            PInteger(fFrame)^ := 0;
            if fPacketInfo then // also retrieve the receiving interface
              len := fSock.RecvFromIf(fFrame, fFrameLen, remote, fFrameIfIndex)
            else
              len := fSock.RecvFrom(fFrame, fFrameLen, remote);
            if Terminated then
              break;
            if len < 0 then // paranoid
            begin
              fLogClass.Add.Log(sllWarning, 'DoExecute: abort after RecvFrom=%',
                [NetLastErrorMsg], self);
              break;
            end;
            if not EqualBuf(UDP_SHUTDOWN, fFrame, len) then // from Destroy
            begin
              inc(fReceived);
              OnFrameReceived(len, remote); // new request
            end;
          end
          else
            // len = 0 for ICMP port unreachable: flush reception buffer
            fSock.RecvFrom(fFrame, fFrameLen, remote);
        end
        else if res <> nrRetry then
        begin
          fLogClass.Add.Log(sllDebug, 'DoExecute: RecvPending=%',
            [NetLastErrorMsg], self);
          break;
        end;
      end
      else if neError in ev then
      begin
        fLogClass.Add.Log(sllWarning, 'DoExecute: WaitFor=%',
          [NetLastErrorMsg], self);
        break;
      end;
      if Terminated then
        break;
      tix64 := mormot.core.os.GetTickCount64;
      tix := tix64 div MilliSecsPerSec;
      if tix <> lasttix then
      begin
        lasttix := tix;
        OnIdle(tix64); // called every second at most
      end;
    end;
    // here, Terminated or broken fSock: notify method to close all connections
    OnShutdown;
    if Terminated or
       not fAutoRebind then
      break;
    // implement fAutoRebind=true after main fSock error
    // (e.g. transient wifi down/up or "iptables DROP")
    delay := 1000; // exponential retry period backoff from 1 to 10 seconds
    repeat
      inc(fRebindRetry);
      fLogClass.Add.Log(sllDebug, 'DoExecute: % AutoRebind attempt retry=% rebind=%',
        [fProcessName, fRebindRetry, fRebindCount], self);
      fSock.Close;
      fSock := nil;
      if SleepOrTerminated(delay) then // wait a little and retry
        break;
      res := DoBind;
      if res = nrOk then
      begin
        inc(fRebindCount);
        break; // re-bound = go back to the main WaitFor/RecFrom loop
      end;
      fLogClass.Add.Log(sllWarning, 'DoExecute: DoBind=% -> sleep and retry',
        [_NR[res]], self);
      if delay < 10000 then
        inc(delay, delay shr 2); // up to 10 seconds retry period
    until false;
  until Terminated;
end;


{ ******************** Custom URI Routing using an efficient Radix Tree }

function UriMethod(const Text: RawUtf8; out Method: TUriRouterMethod): boolean;
begin
  result := false;
  if Text = '' then
    exit;
  case PCardinal(Text)^ of // case-sensitive test in occurrence order
    GET_24:
      Method := urmGet;
    POST_32:
      Method := urmPost;
    PUT_24:
      Method := urmPut;
    HEAD_32:
      Method := urmHead;
    DELE_32:
      Method := urmDelete;
    OPTI_32:
      Method := urmOptions;
    ord('P') + ord('A') shl 8 + ord('T') shl 16 + ord('C') shl 24:
      Method := urmPatch;
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
  len, paramslen: integer;
  t, v: PIntegerArray;
  p: PUtf8Char;
  new: pointer; // fast temporary RawUtf8
begin
  // compute length of the new URI with injected values
  if THttpServerRequest(Ctxt).fRouteName = nil then
    exit; // paranoid
  t := pointer(Data.ToUriPosLen); // [pos1,len1,valndx1,...] trio rules
  n := PDALen(PAnsiChar(t) - _DALEN)^ + _DAOFF;
  v := pointer(THttpServerRequest(Ctxt).fRouteValuePosLen); // [pos,len] pairs
  if v = nil then
     exit; // paranoid
  paramslen := 0;
  p := THttpServerRequest(Ctxt).fUrlParamPos; // filled during parsing
  if p <> nil then
    paramslen := StrLen(p);
  len := integer(Data.ToUriStaticLen) + paramslen;
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
  if paramslen <> 0 then // append original parameters
  begin
    MoveFast(THttpServerRequest(Ctxt).fUrlParamPos^, p^, paramslen);
    if tufHasParameters in Data.ToUriFlags then
      p^ := '&'; // there are already some parameters in the rewrite
  end;
  FastAssignNew(THttpServerRequest(Ctxt).fUrl, new); // replace
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
      n.Data.ToUriFlags := [];
      n.Data.ToUriErrorStatus := Utf8ToInteger(toU, 200, 599, 0);
      if n.Data.ToUriErrorStatus = 0 then // a true URI, not an HTTP error code
      begin
        // pre-compute the rewritten URI into Data.ToUriPosLen[]
        u := pointer(toU);
        if u = nil then
          EUriRouter.RaiseUtf8('No ToUri in %.Setup(''%'')',
            [self, aFromUri]);
        if PosExChar('?', toU) <> 0 then
          include(n.Data.ToUriFlags, tufHasParameters);
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
  req: THttpServerRequest absolute Ctxt;
begin
  result := 0; // nothing to process
  if (self = nil) or
     (req = nil) or
     (req.Url = '') or
     not UriMethod(req.Method, m) then
    exit;
  req.fRouteName := nil; // paranoid: if called w/o Prepare
  req.fRouteNode := nil;
  req.fUrlParamPos := nil;
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
    found := pointer(TUriTreeNode(t.fRoot).Lookup(pointer(req.Url), req));
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
      req.fRouteNode := found;
      result := found.Data.Execute(req);
    end
    else if found.Data.ToUri <> '' then
    begin
      // request is not implemented here, but the Url should be rewritten
      if m <> found.Data.ToUriMethod then
        req.Method := URIROUTERMETHOD[found.Data.ToUriMethod];
      result := found.Data.ToUriErrorStatus; // redirect to an error code
      if result = 0 then
        if found.Data.ToUriPosLen = nil then // only static: just replace URI
          if req.fUrlParamPos = nil then
            req.Url := found.Data.ToUri
          else
            Make([found.Data.ToUri, req.UrlParamPos], req.fUrl) // append params
        else
          found.RewriteUri(req); // compute new URI with injected values
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
  fInContentStream := nil; // paranoid: Prepare() would set it anyway
  fOutContent := '';
  OutContentStreamDiscard; // paranoid: SetupResponse() did hand it over
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
  inherited Destroy; // release any pending SetOutStream() owned stream
end;

procedure THttpServerRequest.DoPurgeHeaders;
begin // sub-function to avoid implicit try..finally
  fOutCustomHeaders := PurgeHeaders(fOutCustomHeaders);
end;

procedure THttpServerRequest.ProcessErrorMessage;
var
  txt: PRawUtf8;
begin
  if fOutCustomHeaders <> '' then // keep meaningful headers
    DoPurgeHeaders;
  txt := fServer.StatusCodeToText(fRespStatus);
  if hsoTextError in fServer.Options then // fast and good enough
  begin
    Make([fRespStatus, ' ', txt^, ': ', fErrorMessage], RawUtf8(fOutContent));
    fOutContentType := TEXT_CONTENT_TYPE;
    exit;
  end;
  HtmlEscapeString(fErrorMessage, fOutContentType, hfAnyWhere); // safety
  FormatUtf8(
    '<!DOCTYPE html><html><body style="font-family:verdana">' +
    '<h1>% Server Error %</h1>' +
    '<p><b>HTTP % %:</b> %</p><hr><small><i>% on %</i></small></body></html>',
    [fServer.ServerName, fRespStatus, fRespStatus, txt^, fOutContentType,
     XPOWEREDVALUE, OS_TEXT], RawUtf8(fOutContent));
  fOutContentType := HTML_CONTENT_TYPE; // body = human friendly HTML message
end;

procedure THttpServerRequest.ProcessOutStream(var Context: THttpRequestContext);
begin
  // hand a SetOutStream() body to the context, as ProcessStaticFile() does
  if not StatusCodeIsSuccess(fRespStatus) then
  begin
    // a handler answering e.g. 404 or its own 416 from a stream should not
    // have that body range-processed, nor replaced by our own error page
    exclude(Context.ResponseFlags, rfWantRange);
    // and should not advertise a Range: support we just disabled
    include(fOutContentStreamOpt, hosNoRange);
  end;
  if Context.ContentFromStream(fOutContentStream, fOutContentStreamPos,
       fOutContentStreamLength, fOutContentStreamOpt) = HTTP_SUCCESS then
  begin
    fOutContentStream := nil; // handed over: no double free by Recycle/Destroy
    fOutContentStreamOpt := [];
    fOutContentStreamLength := 0;
    fOutContentStreamPos := 0;
  end
  else
  begin
    OutContentStreamDiscard; // release it: there is no body to send
    fRespStatus := HTTP_RANGENOTSATISFIABLE;
    fErrorMessage := 'Out of range'; // detected by ProcessErrorMessage
  end;
end;

procedure THttpServerRequest.ProcessStaticFile(var Context: THttpRequestContext;
  CompressGz: integer);
var
  fn: TFileName;
  fsiz: Int64;
begin
  ExtractOutContentType;
  fn := Utf8ToString(fOutContent); // safer than Utf8ToFileName() here
  fOutContent := '';
  ExtractHeader(fOutCustomHeaders, STATICFILE_PROGSIZE, nil, @Context.ContentLength);
  if Context.ContentLength <> 0 then
    // STATICFILE_PROGSIZE: file is not fully available: wait for sending
    if ((not (rfWantRange in Context.ResponseFlags)) or
        Context.ValidateRange) then
      if IsHead(Context.CommandMethod) or // HEAD needs no file but a length
         (FileInfoByName(fn, fsiz, Context.ContentLastModified) and
          (fsiz >= 0) and // not a folder
          (fsiz <= Context.ContentLength)) then
      begin
        // void Context.ContentStream <> nil needed with both GET and HEAD
        Context.ContentStream := TStreamWithPositionAndSize.Create;
        Context.ResponseFlags := Context.ResponseFlags +
          [rfAcceptRange, rfContentStreamNeedFree, rfProgressiveStatic];
      end
      else
        fRespStatus := HTTP_NOTFOUND
    else
      // the expected full size of the progressive file
      fRespStatus := Context.RangeNotSatisfiable(Context.ContentLength)
  else if (not Assigned(fServer.OnSendFile)) or
          (not fServer.OnSendFile(self, fn)) then
  begin
    // regular file sending by chunks
    fRespStatus := Context.ContentFromFile(fn, CompressGz);
    if fRespStatus = HTTP_SUCCESS then
      fOutContent := Context.Content; // small static file content
  end;
  if not StatusCodeIsSuccess(fRespStatus) then
    fErrorMessage := 'Error getting file'; // detected by ProcessErrorMessage
end;

const
  _CMD_200: array[0 .. 3] of TShort31 = (
   'HTTP/1.1 200 OK'#13#10,
   'HTTP/1.0 200 OK'#13#10,
   'HTTP/1.1 206 Partial Content'#13#10,
   'HTTP/1.0 206 Partial Content'#13#10);
  _CMD_XXX: array[boolean] of TShort15 = (
    'HTTP/1.1 ',
    'HTTP/1.0 ');

function THttpServerRequest.SetupResponse(var Context: THttpRequestContext;
  CompressGz, MaxSizeAtOnce: integer): PRawByteStringBuffer;
var
  P: PUtf8Char;
  c: PShort31;
  f: THttpRequestResponseFlags;
  h: PRawByteStringBuffer;
  // note: caller should have set hfConnectionClose in Context.HeaderFlags
begin
  // process content
  Context.ContentLength := 0; // needed by ProcessStaticFile
  Context.ContentLastModified := 0;
  Context.CommandUri := fUrl; // may have been normalized/cleaned during process
  P := pointer(fOutContentType);
  if (P <> nil) and
     (P^ = '!') then
    if fOutContentType = NORESPONSE_CONTENT_TYPE then
      fOutContentType := '' // true HTTP always expects a response
    else if (fOutContent <> '') and
            (fOutContentType = STATICFILE_CONTENT_TYPE) then
      ProcessStaticFile(Context, CompressGz);
  if fOutContentStream <> nil then
    if fErrorMessage = '' then
      ProcessOutStream(Context) // SetOutStream() response body
    else
      OutContentStreamDiscard; // return error not SetOutStream()
  if fErrorMessage <> '' then
    ProcessErrorMessage;
  // append Command
  h := @Context.Head;
  h^.Reset; // reuse 2KB header buffer
  if fRespStatus = HTTP_SUCCESS then // optimistic approach
  begin
    c := @_CMD_200;
    f := Context.ResponseFlags;
    if rfWantRange in f then
      inc(c, 2); // HTTP_PARTIALCONTENT=206 support
    if rfHttp10 in f then
      inc(c);    // HTTP/1.0 support
    h^.AppendShort(c^);
  end
  else
  begin // other cases
    h^.AppendShort(_CMD_XXX[rfHttp10 in Context.ResponseFlags]);
    h^.Append999(fRespStatus);
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

procedure THttpServerRequest.SetOutReprDigest(const Hash: THashDigest);
begin // implement e.g. pcoHttpReprDigest option
  SetOutCustomHeader(['Repr-Digest: ', HASH_TXT_LOWER[Hash.Algo],
    '=:', BinToBase64Short(@Hash.Bin, HASH_SIZE[Hash.Algo]), ':']);
end;

function THttpServerRequest.TempJsonWriter(
  var temp: TTextWriterStackBuffer): TJsonWriter;
begin
  if fTempWriter = nil then
  begin
    fTempWriter := TJsonWriter.CreateOwnedStream(temp);
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

function THttpServerRequest.GetFullUrl: RawUtf8;
begin
  FastAssignNew(result);
end;


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
  // Start would be done by TLoggedThread.AfterConstruction after all Creates
  fStartAfterConstruction := not (hsoCreateSuspended in fOptions);
  inherited Create({suspended=}true, OnStart, OnStop, aLog, ProcessName);
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
    Ctxt.OutCustomHeaders := 'Etag: "ok"'#13#10'Cache-Control: max-age=604800';
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
  if hsoHeadersSanitize in fOptions then
    include(fDefaultRequestOptions, hroHeadersSanitize);
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

procedure THttpServerGeneric.SetOnBodyDownload(
  const aEvent: TOnHttpServerBodyDownload);
begin
  fOnBodyDownload := aEvent;
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

procedure THttpServerGeneric.DoIdle(tix64: Int64; sec32: cardinal);
begin // is called at most every second, but maybe up to 5 seconds delay
  if Assigned(fLogger) then
    fLogger.OnIdle(tix64)      // flush log file(s) on idle server
  else if Assigned(fAnalyzer) then
    fAnalyzer.OnIdle(tix64);   // consolidate telemetry if needed
  if Assigned(fOnIdle) then
    fOnIdle(self, tix64);      // custom callback
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
  if CryptCertOpenSslSelfSigned[Algo] = nil then
    CryptCertOpenSslSelfSigned[Algo] := CryptCertOpenSsl[Algo].Generate(
      CU_TLS_SERVER, '127.0.0.1', nil, 3650);
  //writeln(BinToSource('PRIVKEY_PFX', '', // force SHA1-3DES p12Legacy format
  //  SelfSignedCert[Algo].Save(cccCertWithPrivateKey, '3des=pass', ccfBinary)));
  // no temporary file needed: we just provide the shared OpenSSL handles
  TLS.CertificateRaw := CryptCertOpenSslSelfSigned[Algo].Handle;           // PX509
  TLS.PrivateKeyRaw  := CryptCertOpenSslSelfSigned[Algo].PrivateKeyHandle; // PEVP_PKEY
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


function HttpRequestHash(aAlgo: THashAlgo; const aUri: TUri; aHeaders: PUtf8Char;
  out aDigest: THashDigest; aUpCaseUri: boolean): integer;
var
  hasher: TSynHasher;
  h: PUtf8Char;
  l: PtrInt; // not integer
begin
  result := 0;
  aDigest.Algo := aAlgo;
  if (aUri.Server = '') or
     not hasher.Init(aAlgo) then
    exit;
  hasher.Update(HTTPS_TEXT[aUri.Https]); // hash normalized URI
  hasher.Update(@aAlgo, 1); // separator
  hasher.UpdateUpper(pointer(aUri.Server), length(aUri.Server));
  hasher.Update(@aAlgo, 1);
  hasher.Update(aUri.Port);
  hasher.Update(@aAlgo, 1);
  l := UriTruncAnchorLen(aUri.Address);
  if aUpCaseUri then
    hasher.UpdateUpper(pointer(aUri.Address), l) // normalize
  else
    hasher.Update(pointer(aUri.Address), l);
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
      h := HttpRequestLength(aHeaders, @l);
      if h = nil then
        exit;
    end;
    hasher.Update(h, l);
  end;
  result := hasher.Final(aDigest.Bin, {noinit=}true);
end;

function HttpRequestHashBase32(const aUri: TUri; aName: PShort32;
  aHeaders: PUtf8Char; aDig: PHash160; aUpCaseUri: boolean): boolean;
var
  dig: THashDigest;
begin // SizeOf(aDig^)=20 bytes=160-bit as 32 chars of case-insensitive base-32
  result := HttpRequestHash(hfSHA256, aUri, aHeaders, dig, aUpCaseUri) = SizeOf(THash256);
  if not result then
    FillZero(dig.Bin.b160);
  if aName <> nil then
    if result then
    begin
      aName^[0] := #32;
      BinToBase32(@dig.Bin, @aName^[1], SizeOf(aDig^), @b32encLower);
    end
    else
      aName^[0] := #0;
  if aDig <> nil then
    MoveFast(dig.Bin, aDig^, SizeOf(aDig^));
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

procedure THttpServerSocketGeneric.DoIdle(tix64: Int64; sec32: cardinal);
begin
  // logger + telemetry + OnIdle()
  inherited DoIdle(tix64, sec32);
  // BlackListUri regular refresh support
  if (fBlackListUriNextTix <> 0) and
     (sec32 >= fBlackListUriNextTix) then
    RefreshBlackListUri(sec32);
  {$ifdef OSPOSIX}
  // POSIX Kerberos keytab regular refresh
  if Assigned(fSspiKeyTab) and
     fSspiKeyTab.TryRefresh(sec32) then
    fLogClass.Add.Log(sllDebug, 'DoIdle: refreshed %', [fSspiKeyTab], self);
  {$endif OSPOSIX}
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
           (Ctxt.OutContentStream = nil) and // SetOutStream() is a body too
           (cod <> HTTP_ASYNCRESPONSE) and
           not StatusCodeIsSuccess(cod) then
        begin
          if Ctxt.ErrorMessage = '' then
            Ctxt.ErrorMessage := 'Wrong route'; // if no OnRequest()
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
      if (Ctxt.OutContent = '') and
         (Ctxt.OutContentStream = nil) then // SetOutStream() is a body too
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
  list := HttpGetWeak(fBlackListUri, '', @status); // TODO: use etag + 304
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

procedure THttpServerSocketGeneric.SetProgressiveRequests(aPartials: THttpPartials);
begin
  fProgressiveRequests := aPartials;
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
  fAuthorizeNegotiateCbtHash := '';
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

procedure THttpServerSocketGeneric.SetAuthorizeNegotiate(
  const CbtHash: RawByteString);
var
  len: integer;
begin
  SetAuthorizeNone;
  if not InitializeDomainAuth then
    EHttpServer.RaiseUtf8('%.SetAuthorizeNegotiate: no % available',
      [self, SECPKGNAMEAPI]);
  len := length(CbtHash);
  if (len <> 0) and
     (len > SizeOf(THash512)) then
    EHttpServer.RaiseUtf8('%.SetAuthorizeNegotiate: invalid CbtHash length=%',
      [self, len]);
  fAuthorize := hraNegotiate;
  fAuthorizeNegotiateCbtHash := CbtHash;
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
  FastAssignNew(result);
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
begin
  TServerSspiKeyTab.FileSetter(fSspiKeyTab, aKeyTab, fLogClass.DoLog, self);
end;

function THttpServerSocketGeneric.GetKeyTab: TFileName;
begin
  result := fSspiKeyTab.KeyTab;
end;
{$endif OSPOSIX}

function THttpServerSocketGeneric.Authorization(var Http: THttpRequestContext;
  Opaque: Int64): TAuthServerResult;
var
  auth: PUtf8Char;
  user, pass, url, header: RawUtf8;
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
        if IdemPChar(auth, 'NEGOTIATE ') then
        begin
          // parse 'Authorization: Negotiate <base64 encoding>'
          {$ifdef OSPOSIX}
          if Assigned(fSspiKeyTab) then
            fSspiKeyTab.PrepareKeyTab; // do nothing if no KeyTab changed or set
          {$endif OSPOSIX}
          header := KerberosServerAuthHeader('', auth + 10,
                      @user, fAuthorizeNegotiateCbtHash);
          if header <> '' then
          begin
            Http.ResponseHeaders := header;
            result := asrMatch;
          end;
        end;
    else
      exit;
    end;
    if result = asrMatch then
      Http.BearerToken := user; // see THttpServerRequestAbstract.Prepare
  except
    on E: Exception do // e.g. ESynSspi/EGssApi on authentication error
    begin
      fLogClass.Add.Log(sllTrace, 'Authorization: % from %', [PClass(E)^, auth], self);
      result := asrRejected; // any processing error should silently fail the auth
    end;
  end;
  fLogClass.Add.Log(sllTrace, 'Authorization(%): % %',
    [ToText(fAuthorize)^, ToText(result)^, user], self);
end;

function THttpServerSocketGeneric.ComputeRejectBody(
  var Body: RawByteString; Opaque: Int64; Status: integer;
  const ExtraHeader: RawUtf8): boolean;
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
  FormatUtf8('HTTP/1.% % %'#13#10'%%' + HTML_CONTENT_TYPE_HEADER +
    #13#10'Content-Length: %'#13#10#13#10'%', [ord(result), status, reason^,
    auth, ExtraHeader, length(html), html], RawUtf8(Body));
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
  fThreadPoolRetrieveAbortDelay := 5000; // allow 5 seconds wait in thread pool
  fServerSendBufferSize := 256 shl 10;   // 256KB seems fine on Windows + POSIX
  inherited Create(aPort, OnStart, OnStop, ProcessName, ServerThreadPoolCount,
    KeepAliveTimeOut, ProcessOptions, aLog);
  fBanned := THttpAcceptBan.Create;      // for hsoBan40xIP or BlackList
  if ServerThreadPoolCount > 0 then
  begin
    ThreadCountAdjust(ServerThreadPoolCount); // e.g. WinARM PRISM
    fThreadPool := TSynThreadPoolTHttpServer.Create(self, ServerThreadPoolCount);
    if ServerThreadPoolCount > 1 then
      fHttpQueueLength := 1000
    else
      fHttpQueueLength := 100; // single threaded server is not meant to scale
    if hsoThreadCpuAffinity in ProcessOptions then
      SetServerThreadsAffinityPerCpu(nil, TThreadDynArray(fThreadPool.WorkThread))
    else if hsoThreadSocketAffinity in ProcessOptions then
      SetServerThreadsAffinityPerSocket(nil, TThreadDynArray(fThreadPool.WorkThread));
  end
  else if ServerThreadPoolCount < 0 then
  begin
    fMonoThread := true; // accept() + recv() + send() in a single thread
    if fHeaderRetrieveAbortDelay = 0 then
      fHeaderRetrieveAbortDelay := 200; // shortest block and wait possible
  end;
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
    if NewSocket(Sock.Server, Sock.Port, Sock.SocketLayer, {dobind=}false,
         10, 10, 10, {retry=}0, dummy) = nrOK then
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

procedure THttpServer.DoIdle(tix64: Int64; sec32: cardinal);
var
  i: integer;
begin
  // logger + telemetry + OnIdle + RefreshBlackListUri + fSspiKeyTab.TryRefresh
  inherited DoIdle(tix64, sec32);
  // e.g. TAcmeLetsEncryptServer.OnAcceptIdle
  if Assigned(fOnAcceptIdle) then
    fOnAcceptIdle(self, tix64);
  // update internal THttpAcceptBan lists
  if Assigned(fBanned) and
     (fBanned.Count <> 0) then
  begin
    if fBanSec <> 0 then
      for i := fBanSec + 1 to sec32 do // as many DoRotate as elapsed seconds
        fBanned.DoRotate
    {$ifdef OSPOSIX} // Windows would require some activity - not an issue
    else
      fSock.ReceiveTimeout := 1000 // accept() to exit after one second
    {$endif OSPOSIX};
    fBanSec := sec32;
  end;
end;

procedure THttpServer.DoExecute;
var
  cltsock: TNetSocket;
  cltaddr: TNetAddr;
  cltservsock: THttpServerSocket;
  res: TNetResult;
  err, banlen {$ifdef OSWINDOWS} , sec, acceptsec {$endif}: integer;
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
    if not fSock.SockIsDefined then // paranoid check
      EHttpServer.RaiseUtf8('%.DoExecute: %.Bind failed', [self, fSock]);
    fExecuteState := esRunning;
    // main ACCEPT loop
    {$ifdef OSWINDOWS}
    acceptsec := 0;
    {$endif OSWINDOWS}
    while not Terminated do
    begin
      res := Sock.Sock.Accept(cltsock, cltaddr, {async=}false, @err);
      if not (res in [nrOK, nrRetry]) then
      begin
        fLogClass.Add.Log(sllDebug, 'DoExecute: accept() failed as % %',
          [_NR[res], SystemErrorShort(err)], self);
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
        DoIdle(tix64, tix64 div 1000);
        continue;
      end;
      {$else} // Windows accept() does not timeout and return nrRetry
      tix64 := mormot.core.os.GetTickCount64;
      sec := tix64 div 1000;
      if sec <> acceptsec then // trigger the callbacks once per second
      begin
        acceptsec := sec;
        DoIdle(tix64, sec);
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
              fSock.OnLog(sllTrace, 'DoExecute: % [%]', [PClass(E)^, E.Message], self);
        end
      else if Assigned(fThreadPool) then
      begin
        // ServerThreadPoolCount > 0 will use the thread pool to process the
        // request header, and probably its body unless kept-alive or upgraded
        // - this is the most efficient way of using this server class
        cltservsock := fSocketClass.Create(self);
        // note: we tried to reuse the fSocketClass instance -> no perf benefit
        cltservsock.AcceptRequest(cltsock, @cltaddr);
        if Assigned(fSock.OnLog) then
          fSock.OnLog(sllTrace, 'DoExecute: push %', [cltservsock], self);
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

function THttpServerSocket.GetAborted: boolean;
begin
  result := (fAborted in fFlags) or // inherited GetAborted
            ((fServer <> nil) and
             fServer.Terminated);   // abort e.g. any background SockRecvLn()
end;

procedure THttpServerSocket.DownloadBody; // properly inlined
begin
  if Http.ContentStream = nil then
    GetBodyInMemory // default in-memory download into Http.Content
  else
    GetBodyStream;  // use Http.ContentStream from OnBodyDownload event
end;

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
  if GetAborted then
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
               ((Http.ContentLength > 0) or
                (hfTransferChunked in Http.HeaderFlags)) then
            begin
              DownloadBody; // we need to get it now
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
      if aServer.Sock.TLS.ClientCertificateAuthentication then
      begin
        TLS := aServer.Sock.TLS; // full mTLS policy for TaskProcess cstaAccept
        ResetNetTlsContext(TLS); // keep AcceptCert but clear shared outputs
      end
      else
        TLS.AcceptCert := aServer.Sock.TLS.AcceptCert; // historical fast path
    end;
    OnLog := aServer.Sock.OnLog;
  end;
end;

function THttpServerSocket.GetRequest(withBody: boolean;
  headerMaxTix: Int64): THttpServerSocketGetRequestResult;
var
  status, tix32, max: cardinal;
  startTix, pendingMaxTix, tix: Int64;
  pending: integer;
begin
  try
    if Http.CommandUri <> '' then
      Http.Reset;
    // abort now with no exception if socket is obviously broken
    result := grClosed;
    // use SockIn with 1KB buffer if not already initialized: 2x faster
    if SockIn = nil then
      CreateSockIn;
    // Chromium/Edge browsers create "speculative/preconnect TCP connections"
    // - i.e. open an idle socket with NO activity, which blocks our pool
    // - so we need to wait for actual data, checking server termination
    startTix := 0;
    pendingMaxTix := headerMaxTix; // e.g. from fServer.HeaderRetrieveAbortTix
    repeat
      if GetAborted then
        exit; // server is down -> close connection
      pending := SockInPending(100);
      if GetAborted then
        exit;
      if pending > 0 then
        break // SockRecvLn() won't block
      else if pending = 0 then
      begin
        tix := mormot.core.os.GetTickCount64;
        if startTix = 0 then
          startTix := tix;
        if pendingMaxTix = 0 then // lazy starTix+timeout setting
        begin
          max := 10000;
          if fServer <> nil then
          begin
            max := MaxPtrUInt(fServer.fServerKeepAliveTimeOut, max);
            if (fServer.fThreadPoolRetrieveAbortDelay <> 0) and
               (max > fServer.fThreadPoolRetrieveAbortDelay) and
               not withBody then
              // withBody=false: ensure early release (5s) from thread pool
              max := fServer.fThreadPoolRetrieveAbortDelay;
          end;
          pendingMaxTix := startTix + max;
        end
        else if tix >= pendingMaxTix then
        begin
          if (fServer <> nil) and
             Assigned(fServer.Sock.OnLog) then
            fServer.Sock.OnLog(sllTrace, 'Execute: % %s timeout',
              [fRemoteIP, (tix - startTix) div MilliSecsPerSec], self);
          result := grTimeout;
          exit; // reached headers time out -> close connection
        end;
      end
      else // pending < 0 -> clased
      begin
        if (fServer <> nil) and
           Assigned(fServer.Sock.OnLog) then
          fServer.Sock.OnLog(sllTrace, 'Execute: % Socket error',
            [fRemoteIP], self);
        exit; // grClosed: disconnect the client and release this thread
      end;
      {$ifdef OSWINDOWS}
      SleepHiRes(1); // seems to be needed only with Windows select()
      {$endif OSWINDOWS}
    until false;
    // 1st line is command: 'GET /path HTTP/1.1' e.g.
    SockRecvLn(Http.CommandUri);
    if Http.CommandUri = '' then
      exit; // likely to be a broken or closed connection
    result := grRejected;
    if not Http.ParseCommand then
      exit; // reject invalid command - e.g. if TLS was involved
    fKeepAliveClient := ((fServer = nil) or
                         (fServer.fServerKeepAliveTimeOut > 0)) and
                        not (rfHttp10 in Http.ResponseFlags);
    // get and parse HTTP request header
    if not GetHeader((fServer <> nil) and
                     (hsoHeadersUnfiltered in fServer.Options),
                     {NoHttpReset=}true) then
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
        not (rfHttp10 in Http.ResponseFlags) or
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
                       HTTP_10_FLAGS[rfHttp10 in Http.ResponseFlags];
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
          tix32 := GetTickSec shr 2;
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
      // track and limit the cumulated size of a chunked body - whether
      // OnBodyDownload supplies a stream for it or not: the check above only
      // applies to an explicit Content-Length
      Http.ContentMaxSize := fServer.MaximumAllowedContentLength;
      // OnBodyDownload callback can supply a stream for the body - even for HEAD
      if Assigned(fServer.OnBodyDownload) and
         not (hfConnectionUpgrade in Http.HeaderFlags) and
         ((Http.ContentLength > 0) or
          (hfTransferChunked in Http.HeaderFlags)) then
        if not DoOnBodyDownload then
        begin
          result := grRejected;
          exit;
        end;
    end;
    // implement 'Expect: 100-Continue' Header
    if (hfExpect100 in Http.HeaderFlags) and
       ((Http.ContentLength > 0) or
        (hfTransferChunked in Http.HeaderFlags)) then
      // client waits for the server to parse the headers and return 100
      // before sending the request body
      SockSendFlush('HTTP/1.1 100 Continue'#13#10#13#10);
    // now the server could retrieve the HTTP request body
    if withBody and
       not (hfConnectionUpgrade in Http.HeaderFlags) then
    begin
      if (Http.ContentLength > 0) or
         (hfTransferChunked in Http.HeaderFlags) then // even for HEAD
        DownloadBody;
      result := grBodyReceived;
    end
    else
      result := grHeaderReceived;
  except
    on EHttpSocketOverflow do
      result := grOversizedPayload; // e.g. chunked body over ContentMaxSize
    on E: Exception do
      result := grException;
  end;
end;

function THttpServerSocket.DoOnBodyDownload: boolean;
var
  fn: TFileName; // managed local variables are fine in this slow sub-method
  strm: TStream;
  len: Int64;
begin // called from GetRequest() to process OnBodyDownload event
  result := true;
  HeadersPrepare(fRemoteIP); // will include remote IP to Http.Headers
  len := Http.ContentLength;
  if hfTransferChunked in Http.HeaderFlags then
    len := -1; // a chunked body size is not known in advance
  strm := fServer.fOnBodyDownload(Http.CommandUri, Http.CommandMethod,
    Http.Headers, Http.ContentType, fRemoteIP, len);
  if strm = nil then
    exit; // fallback to regular in-memory Content buffering
  if Http.ContentEncoding <> nil then
  begin
    // a streamed body does not support Content-Encoding: compression
    fn := '';
    if strm.InheritsFrom(TFileStreamEx) then
      fn := TFileStreamEx(strm).FileName;
    strm.Free;
    if fn <> '' then
      DeleteFile(fn); // remove the (void) spool file
    fServer.ComputeRejectBody(Http.Content, 0,
      HTTP_UNSUPPORTEDMEDIATYPE, 'Accept-Encoding: identity'#13#10);
    SockSendFlush(Http.Content);
    result := false;
  end
  else
  begin
    // the (maybe deferred) DownloadBody will fill this stream
    Http.ContentStream := strm; // freed by Reset on broken connection
    include(Http.ResponseFlags, rfContentStreamNeedFree);
    if strm.InheritsFrom(TFileStreamEx) then
      Http.SetContentInputName(TFileStreamEx(strm));
  end;
end;

procedure THttpServerSocket.GetBodyInMemory;
begin
  try
    GetBody; // default in-memory download into Http.Content
  except
    on EHttpSocketOverflow do
    begin
      // e.g. a chunked body over ContentMaxSize: report 413 before closing,
      // as GetBodyStream does
      fServer.ComputeRejectBody(Http.Content, 0, HTTP_PAYLOADTOOLARGE);
      SockSendFlush(Http.Content);
      raise; // the caller will close the connection
    end;
  end;
end;

procedure THttpServerSocket.GetBodyStream;
begin
  // download into the stream supplied by the OnBodyDownload event
  try
    GetBody(Http.ContentStream);
    // the stream (and any spool file) remains during the request processing,
    // as InContentStream - and is released by ProcessDone afterwards
    Http.ContentInputDone;
  except
    on EHttpSocketOverflow do
    begin
      // e.g. a chunked body over ContentMaxSize: report 413 before closing
      Http.ProcessDone; // release the stream + delete any partial spool file
      fServer.ComputeRejectBody(Http.Content, 0, HTTP_PAYLOADTOOLARGE);
      SockSendFlush(Http.Content);
      raise; // the caller will close the connection
    end;
    on EStreamError do
    begin
      // e.g. ENOSPC when spooling the body: report it before closing
      Http.ProcessDone; // release the stream + delete any partial spool file
      fServer.ComputeRejectBody(Http.Content, 0, HTTP_INSUFFICIENTSTORAGE);
      SockSendFlush(Http.Content);
      raise; // the caller will close the connection
    end;
    on Exception do
    begin
      Http.ProcessDone; // release the stream + delete any partial spool file
      raise;
    end;
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
    fServerSock.Abort;
end;

procedure THttpServerResp.Execute;

  procedure HandleRequestsProcess;
  var
    res: THttpServerSocketGetRequestResult;
    banned: boolean;
  begin
    {$ifdef SYNCRTDEBUGLOW}
    try
    {$endif SYNCRTDEBUGLOW}
    try
      repeat
        res := fServerSock.GetRequest({body=}true, fServer.HeaderRetrieveAbortTix);
        if fServerSock.GetAborted then
          // server is down -> disconnect the client
          exit;
        fServer.IncStat(res);
        case res of
          grBodyReceived,
          grHeaderReceived:
            begin
              if res = grBodyReceived then
                fServer.IncStat(grHeaderReceived);
              // calc answer and send response
              fServer.Process(fServerSock, ConnectionID, self);
            end;
          grWwwAuthenticate:
            ; // try to continue on this connection
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
        if (fServer = nil) or
           fServer.Terminated then
          exit;
        // keep on this connection only if likely
      until not fServerSock.KeepAliveClient;
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

  procedure HandleCleanup; // sub-function for FPC Win64-aarch64 compilation
  begin
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
           ((fServerSock.Http.ContentLength > 0) or
            (hfTransferChunked in fServerSock.Http.HeaderFlags)) then
          fServerSock.DownloadBody;
        fServer.Process(fServerSock, ConnectionID, self);
        if (fServer <> nil) and
           fServerSock.KeepAliveClient then
          HandleRequestsProcess; // process further kept alive requests
      end;
    finally
      HandleCleanup;
    end;
  except
    on Exception do
      ; // just ignore unexpected exceptions here, especially during clean-up
  end;
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
  {$ifdef USE_THREADWINIOCP}
  inherited Create(NumberOfThreads, INVALID_HANDLE_VALUE, Server.ProcessName);
  {$else}
  inherited Create(NumberOfThreads, {queuependingctx=}true, Server.ProcessName);
  {$endif USE_THREADWINIOCP}
end;

{$ifndef USE_THREADWINIOCP}
function TSynThreadPoolTHttpServer.QueueLength: integer;
begin
  if fServer = nil then
    result := 10000
  else
    result := fServer.fHttpQueueLength;
end;
{$endif USE_THREADWINIOCP}

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
    fResponse, fResponseContentType: RawUtf8;
    fParams: PDocVariantData;
    fMethod: TUriMethods;
    fDone: TSynEvent;
    fReceived: TSynEvent;
  public
    constructor Create(const aPort, aResponse, aResponseContentType: RawUtf8;
      aParams: PDocVariantData; aLogClass: TSynLogClass; aMethod: TUriMethods;
      aOptions: THttpServerOptions; aThreadPool: integer); reintroduce;
    destructor Destroy; override;
    function Request(Ctxt: THttpServerRequestAbstract): cardinal; override;
    procedure OnResponded(var Context: TOnHttpServerAfterResponseContext);
  end;

constructor THttpServerEphemeral.Create(const aPort, aResponse,
  aResponseContentType: RawUtf8; aParams: PDocVariantData; aLogClass: TSynLogClass;
  aMethod: TUriMethods; aOptions: THttpServerOptions; aThreadPool: integer);
begin
  fResponse := aResponse;
  fResponseContentType := aResponseContentType;
  fParams := aParams;
  fMethod := aMethod;
  fOnAfterResponse := OnResponded;
  fReceived := TSynEvent.Create;
  inherited Create(aPort, nil, nil, 'ephemeral', aThreadPool, 0, aOptions, aLogClass);
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
    if fResponseContentType <> '' then
      Ctxt.OutContentType := fResponseContentType;
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
  aMethods: TUriMethods; aOptions: THttpServerOptions;
  const aResponseContentType: RawUtf8): boolean;
var
  server: THttpServerEphemeral;
begin
  aParams.Clear;
  server := THttpServerEphemeral.Create(aPort, aResponse, aResponseContentType,
    @aParams, aLogClass, aMethods, aOptions, {threadpool=}-1);
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

function EphemeralHttpServer(aLogClass: TSynLogClass; const aResponse: RawUtf8;
  aMethods: TUriMethods; aOptions: THttpServerOptions;
  const aResponseContentType: RawUtf8): THttpServer;
begin
  result := THttpServerEphemeral.Create('0', aResponse, aResponseContentType,
    nil, aLogClass, aMethods, aOptions, {threadpool=}2);
end;

end.
