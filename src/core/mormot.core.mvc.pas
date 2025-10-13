/// Framework Core MVC Transport Agnostic Logic
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.mvc;

{
  *****************************************************************************

   Model-View-Controller (MVC) pattern and Mustache
    - Web Views Implementation using Mustache
    - ViewModel/Controller Sessions using Cookies
    - Web Renderer Returning Mustache Views or Json
    - Application ViewModel/Controller using Interfaces
    
   Abstract MVC logic, as used by mormot.rest.mvc.pas and mormot.net.mvc.pas

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  classes,
  sysutils,
  variants,
  mormot.core.base,
  mormot.core.os,
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.variants,
  mormot.core.data,
  mormot.core.rtti,
  mormot.crypt.core,
  mormot.core.json,
  mormot.core.search,  // for FileNames()
  mormot.crypt.secure, // for TBinaryCookieGenerator
  mormot.core.log,
  mormot.core.interfaces,
  mormot.core.mustache;


{ ************ Web Views Implementation using Mustache }

var
  /// TDocVariantOptions for efficient MVC data context rendering
  // - all instances are transient, so interning is of no benefit here
  JSON_MVC: TDocVariantOptions =
    [dvoReturnNullForUnknownProperty,
     dvoValueCopiedByReference,
     dvoSerializeAsExtendedJson,
     dvoAllowDoubleValue];

type
  /// Exception class triggerred by mORMot MVC/MVVM applications internally
  // - those error are internal fatal errors of the server side process
  EMvcException = class(ESynException);

  /// record type to define MVC commands e.g. to redirect to another URI
  // - do NOT access those record property directly, but rather use
  // TMvcApplication.GotoView/GotoError/GotoDefault class methods, e.g.
  // !  function TBlogApplication.Logout: TMvcAction;
  // !  begin
  // !    CurrentSession.Finalize;
  // !    GotoDefault(result);
  // !  end;
  // - this record type should match exactly TServiceCustomAnswer layout,
  // so that TServiceMethod.InternalExecute() would handle it directly
  TMvcAction = record
    /// the method name to be executed - match TServiceCustomAnswer.Header field
    RedirectToMethodName: RawUtf8;
    /// may contain a JSON object which will be used to specify parameters
    // to the specified method - match TServiceCustomAnswer.Content field
    RedirectToMethodParameters: RawUtf8;
    /// HTTP Status code to be returned - match TServiceCustomAnswer.Status field
    // - if RedirectMethodName is set, will return 307 HTTP_TEMPORARYREDIRECT
    // by default, but you can set here the expected HTTP Status code, e.g.
    // 201 HTTP_CREATED or 404 HTTP_NOTFOUND
    ReturnedStatus: cardinal;
  end;
  /// the MVC-redirection way of accessing a PServiceCustomAnswer record pointer
  PMvcAction = ^TMvcAction;

  /// TMvcView.Flags rendering context
  // - viewHasGenerationTimeTag is set if TMvcViewsAbstract.ViewGenerationTimeTag
  // text appears in the template (or its partials), to scan only if needed
  TMvcViewFlags = set of (
    viewHasGenerationTimeTag);

  // cache status for one static resource
  TMvcRunCacheStatic = record
    FileName: TFileName;
    Body: RawByteString;
    Header: RawUtf8;
    Etag: RawUtf8;
  end;
  PMvcRunCacheStatic = ^TMvcRunCacheStatic;
  TMvcRunCacheStatics = array of TMvcRunCacheStatic;

  /// an abstract class able to implement Views
  TMvcViewsAbstract = class
  protected
    fFactory: TInterfaceFactory;
    fLogClass: TSynLogClass;
    fViewTemplateFolder, fViewStaticFolder: TFileName;
    fFactoryErrorIndex: integer;
    fViewGenerationTimeTag: RawUtf8;
    procedure NotifyContentChanged; virtual;
    procedure SetViewTemplateFolder(const aFolder: TFileName);
    /// overriden implementations should return the rendered content
    function Render(methodIndex: integer; const Context: variant;
      var Answer: TServiceCustomAnswer): TMvcViewFlags; virtual; abstract;
    function RenderFlags(methodIndex: PtrInt): TMvcViewFlags; virtual; abstract;
    /// compute the static file name - from fViewStaticFolder by default
    // - called if cacheStatic has been defined or not
    procedure FillStaticFileName(var aFile: TMvcRunCacheStatic); virtual;
    /// compute the static file contents - from fViewStaticFolder by default
    // - called only if cacheStatic has been defined
    procedure FillStaticFileContent(var aFile: TMvcRunCacheStatic); virtual;
  public
    /// initialize the class
    constructor Create(aInterface: PRttiInfo; aLogClass: TSynLogClass);
    /// read-only access to the associated factory for the implementation class
    property Factory: TInterfaceFactory
      read fFactory;
    /// the associated TSynLog class
    property LogClass: TSynLogClass
      read fLogClass write fLogClass;
    /// set or retrieve the local folder containing the Mustache views
    // - if you change it, it will also change the ViewStaticFolder as '.static'
    property ViewTemplateFolder: TFileName
      read fViewTemplateFolder write SetViewTemplateFolder;
    /// set or retrieve the .static local folder name
    property ViewStaticFolder: TFileName
      read fViewStaticFolder write fViewStaticFolder;
    /// any occurrence of this tag in a rendered view will be converted
    // into the rendering time in microseconds
    // - equals '[[GENERATION_TIME_TAG]]' by default
    property ViewGenerationTimeTag: RawUtf8
      read fViewGenerationTimeTag write fViewGenerationTimeTag;
  end;

  /// general parameters defining the Mustache Views process
  // - used as a separate value so that we would be able to store the
  // settings in a file, e.g. encoded as a JSON object
  TMvcViewsMustacheParameters = record
    /// where the mustache template files are stored
    // - if not set, will search in a 'Views' folder under the current executable
    Folder: TFileName;
    /// the file extensions to search in the given Folder, specified as CSV
    // - if not set, will search for 'html,json,css'
    CsvExtensions: TFileName;
    /// defines if the view files should be checked for modification
    // - any value would automatically update the rendering template, if the
    // file changed on disk after a given number of seconds - default is 5
    // - setting 0 would disable this feature
    FileTimestampMonitorAfterSeconds: cardinal;
    /// file extension (e.g. '.html') to be used to create void templates
    // - default '' will create no void template file in the given Folder
    ExtensionForNotExistingTemplate: TFileName;
    /// set of block helpers to be registered to TSynMustache
    // - default will use TSynMustache.HelpersGetStandardList definition
    Helpers: TSynMustacheHelpers;
  end;

  /// define TMvcViewsMustache.RegisterExpressionHelpersForTables labels
  THtmlTableStyleLabel = (
    labelFalse,
    labelTrue,
    labelOff,
    labelOn,
    labelValue);

  /// define TMvcViewsMustache.RegisterExpressionHelpersForTables CSS styling
  TExpressionHtmlTableStyle = class
  public
    class procedure StartTable(WR: TTextWriter); virtual;
    class procedure BeforeFieldName(WR: TTextWriter); virtual;
    class procedure BeforeValue(WR: TTextWriter); virtual;
    class procedure AddLabel(WR: TTextWriter; const text: string;
      kind: THtmlTableStyleLabel); virtual;
    class procedure AfterValue(WR: TTextWriter); virtual;
    class procedure EndTable(WR: TTextWriter); virtual;
  end;
  /// to define TMvcViewsMustache.RegisterExpressionHelpersForTables CSS styling
  TExpressionHtmlTableStyleClass = class of TExpressionHtmlTableStyle;

  /// TMvcViewsMustache.RegisterExpressionHelpersForTables via Bootstrap CSS
  TExpressionHtmlTableStyleBootstrap = class(TExpressionHtmlTableStyle)
  public
    class procedure StartTable(WR: TTextWriter); override;
    class procedure AddLabel(WR: TTextWriter; const text: string;
      kind: THtmlTableStyleLabel); override;
  end;

  // mustache view status for one fFactory.Methods[]
  TMvcViewMustache = record
    Safe: TOSLightLock; // = TOSLightMutex = SRW lock or direct pthread mutex
    Mustache: TSynMustache;
    Template: RawUtf8;
    MethodName: TFileName;
    SearchPattern: TFileName;
    FileName: TFileName;
    ShortFileName: TFileName;
    FileExt: TFileName;
    ContentTypeHeader: RawUtf8;
    FileAgeLast: TUnixTime;
    FileAgeTix32: cardinal;
    Flags: TMvcViewFlags;
  end;

  /// a class able to implement Views using Mustache templates
  TMvcViewsMustache = class(TMvcViewsAbstract)
  protected
    fViewTemplateFileTimestampMonitor: cardinal;
    fViewPartials: TSynMustachePartials;
    fViewHelpers: TSynMustacheHelpers;
    fViews: array of TMvcViewMustache; // follows fFactory.Methods[]
    procedure NotifyContentChanged; override;
    /// search for template files in ViewTemplateFolder
    function FindTemplateFileNames(const Mask: TFileName): TFileNameDynArray; virtual;
    function GetTemplateFileAge(const View: TMvcViewMustache): TUnixTime; virtual;
    procedure SetTemplateContent(var View: TMvcViewMustache); virtual;
    /// search for template partials files in ViewTemplateFolder
    function FindPartialFileNames: TFileNameDynArray; virtual;
    /// overriden implementations should return the rendered content
    function Render(methodIndex: integer; const Context: variant;
      var Answer: TServiceCustomAnswer): TMvcViewFlags; override;
    function RenderFlags(methodIndex: PtrInt): TMvcViewFlags; override;
    // some helpers defined here to avoid mormot.crypt.core link
    class procedure md5(const Value: variant; out Result: variant);
    class procedure sha1(const Value: variant; out Result: variant);
    class procedure sha256(const Value: variant; out Result: variant);
    class procedure sha512(const Value: variant; out Result: variant);
  public
    /// create an instance of this ViewModel implementation class
    // - define the associated REST instance, the interface definition and the
    // local folder where the mustache template files are stored
    // - will search and parse the matching views (and associated *.partial)
    constructor Create(aInterface: PRttiInfo;
      const aParameters: TMvcViewsMustacheParameters;
      aLogClass: TSynLogClass = nil); reintroduce; overload; virtual;
    /// finalize the instance
    destructor Destroy; override;
    /// create an instance of this ViewModel implementation class
    // - this overloaded version will use default parameters (i.e. search for
    // html+json+css in the "Views" sub-folder under the executable)
    // - will search and parse the matching views (and associated *.partial),
    // optionally creating void templates for any missing view
    constructor Create(aInterface: PRttiInfo;
      const aTemplatesFolder: TFileName = ''; aLogClass: TSynLogClass = nil;
      const aExtensionForNotExistingTemplate: TFileName = ''); overload;
    /// define the supplied Expression Helpers definition
    // - returns self so that may be called in a fluent interface
    function RegisterExpressionHelpers(const aNames: array of RawUtf8;
      const aEvents: array of TSynMustacheHelperEvent): TMvcViewsMustache;
    /// define some Expression Helpers for hashing
    // - i.e. md5, sha1 sha256 and sha512 hashing
    // - would allow e.g. to compute a Gravatar URI via:
    // ! <img src=http://www.gravatar.com/avatar/{{md5 email}}?s=200></img>
    // - returns self so that may be called in a fluent interface
    function RegisterExpressionHelpersForCrypto: TMvcViewsMustache;
  end;

/// check if a method has no TMvcAction result parameter using RTTI
function MethodHasView(const aMethod: TInterfaceMethod): boolean;


{ ************ ViewModel/Controller Sessions using Cookies }

type
  TMvcApplication = class;

  /// an abstract class able to implement ViewModel/Controller sessions
  // - see TMvcSessionWithCookies to implement cookie-based sessions
  // - this kind of ViewModel will implement client side storage of sessions,
  // storing any (simple) record content on the browser client side
  // - at login, a record containing session-related information (session ID,
  // display and login name, preferences, rights...) can be computed only once
  // on the server side from the Model, then stored on the client side (typically
  // in a cookie): later on, session information can be retrieved by the server
  // logic (via CheckAndRetrieve - note that any security attribute should be
  // verified against the Model), then the renderer (CheckAndRetrieveInfo
  // returning the record as TDocVariant in the data context "Session" field) -
  // such a pattern is very efficient and allows good scaling
  // - session are expected to be tied to the TMvcSessionAbstract instance
  // lifetime, so are lost after server restart, unless they are persisted
  // via LoadContext/SaveContext methods
  TMvcSessionAbstract = class
  protected
    fApplication: TMvcApplication;
  public
    /// create an instance of this ViewModel implementation class
    constructor Create(Owner: TMvcApplication); virtual;
    /// will create a new session
    // - setting an optional record data, and returning the internal session ID
    // - you can supply a time period, after which the session will expire -
    // default is 1 hour - note that overriden methods may not implement it
    function Initialize(PRecordData: pointer = nil;
      PRecordTypeInfo: PRttiInfo = nil;
      SessionTimeOutMinutes: cardinal = 60): integer; virtual; abstract;
    /// fast check if there is a session associated to the current context
    function Exists: boolean; virtual; abstract;
    /// retrieve the current session ID
    // - can optionally retrieve the associated record Data parameter
    // - Invalidate=true would force this cookie to be rejected in the future,
    // and avoid cookies replay attacks e.g. from Finalize()
    function CheckAndRetrieve(PRecordData: pointer = nil;
      PRecordTypeInfo: PRttiInfo = nil; PExpires: PUnixTime = nil;
      Invalidate: boolean = false): integer; virtual; abstract;
    /// retrieve the session information as a JSON object
    // - returned as a TDocVariant, including any associated record Data and
    // optionally its session ID
    // - will call CheckAndRetrieve() then RecordSaveJson() and _JsonFast()
    // - to be called in overriden TMvcApplication.GetViewInfo method
    // - warning: PSessionID^ should be a 32-bit "integer" variable, not a PtrInt
    function CheckAndRetrieveInfo(PRecordDataTypeInfo: PRttiInfo;
      PSessionID: PInteger = nil; Invalidate: boolean = false): variant; virtual;
    /// clear the session
    procedure Finalize(PRecordTypeInfo: PRttiInfo = nil); virtual; abstract;
    /// return all session generation information as ready-to-be stored string
    // - to be retrieved via LoadContext, e.g. after restart
    function SaveContext: RawUtf8; virtual; abstract;
    /// restore session generation information from SaveContext format
    // - returns TRUE on success
    function LoadContext(const Saved: RawUtf8): boolean; virtual; abstract;
    /// access to the owner MVC Application
    property Application: TMvcApplication
      read fApplication;
  end;

  /// a class able to implement ViewModel/Controller sessions with cookies
  // - this kind of ViewModel will implement cookie-based sessions, able to
  // store any (simple) record content in the cookie, on the browser client side
  // - those cookies have the same feature set than JWT, but with a lower
  // payload (thanks to binary serialization), and cookie safety (not accessible
  // from JavaScript): they are digitally signed (with AES-GCM-128 and a
  // temporary secret key), they include an unique session identifier (like
  // "jti" claim), issue and expiration dates (like "iat" and "exp" claims),
  // and they are encrypted with a temporary key - this secret keys is tied to
  // the TMvcSessionWithCookies instance lifetime, so new cookies are generated
  // after server restart, unless they are persisted via LoadContext/SaveContext
  // - signature and encryption are weak, but very fast, to avoid DDOS attacks
  TMvcSessionWithCookies = class(TMvcSessionAbstract)
  protected
    fContext: TBinaryCookieGenerator;
    function GetCookieName: RawUtf8;
    procedure SetCookieName(const Value: RawUtf8);
    // overriden e.g. in TMvcSessionWithRestServer using ServiceContext threadvar
    function GetCookie(out Value: PUtf8Char): integer; virtual; abstract;
    procedure SetCookie(const Value: RawUtf8); virtual; abstract;
  public
    /// create an instance of this ViewModel implementation class
    constructor Create(Owner: TMvcApplication); override;
    /// finalize this instance
    destructor Destroy; override;
    /// fast check if there is a cookie session associated to the current context
    function Exists: boolean; override;
    /// will initialize the session cookie
    // - setting an optional record data, which will be stored Base64-encoded
    // - will return the 32-bit internal session ID
    // - you can supply a time period, after which the session will expire -
    // default is 1 hour, and could go up to
    function Initialize(PRecordData: pointer = nil;
      PRecordTypeInfo: PRttiInfo = nil;
      SessionTimeOutMinutes: cardinal = 60): integer; override;
    /// retrieve the session ID from the current cookie
    // - can optionally retrieve the record Data parameter stored in the cookie
    // - Invalidate=true would force this cookie to be rejected in the future
    // - will return the 32-bit internal session ID, or 0 if the cookie is invalid
    function CheckAndRetrieve(PRecordData: pointer = nil;
      PRecordTypeInfo: PRttiInfo = nil; PExpires: PUnixTime = nil;
      Invalidate: boolean = false): integer; override;
    /// clear the session
    // - by deleting the cookie on the client side
    procedure Finalize(PRecordTypeInfo: PRttiInfo = nil); override;
    /// return all cookie generation information as base64 encoded text
    // - to be retrieved via LoadContext
    function SaveContext: RawUtf8; override;
    /// restore cookie generation information from SaveContext text format
    // - returns TRUE after checking the crc and unserializing the supplied data
    // - WARNING: if the unerlying record type structure changed (i.e. any
    // field is modified or added), restoration will lead to data corruption of
    // low-level binary content, then trigger unexpected GPF: if you change the
    // record type definition, do NOT use LoadContext - and reset all cookies
    function LoadContext(const Saved: RawUtf8): boolean; override;
    /// direct access to the low-level information used for cookies generation
    // - use SaveContext and LoadContext methods to persist this information
    // before server shutdown, so that the cookies can be re-used after restart
    property Context: TBinaryCookieGenerator
      read fContext write fContext;
    /// you can customize the cookie name
    // - default is 'mORMot', and cookie is restricted to Path=/RestRoot on
    // TMvcApplicationRest from mormot.rest.mvc
    property CookieName: RawUtf8
      read GetCookieName write SetCookieName;
  end;

  /// implement a single ViewModel/Controller in-memory session
  // - this kind of session could be used in-process, e.g. for a VCL/FMX GUI
  // - do NOT use it with multiple clients, e.g. from HTTP remote access
  TMvcSessionSingle = class(TMvcSessionWithCookies)
  protected
    fSingleCookie: RawUtf8;
    function GetCookie(out Value: PUtf8Char): integer; override;
    procedure SetCookie(const Value: RawUtf8); override;
  end;

  /// implement ViewModel/Controller sessions over the MVC renderer context
  // - e.g. when the MVC is implemented directly over a HTTP/HTTPS server
  // - will use a threadvar to access TMvcRendererReturningData.Headers
  TMvcSessionWithRenderer = class(TMvcSessionWithCookies)
  protected
    function GetCookie(out Value: PUtf8Char): integer; override;
    procedure SetCookie(const Value: RawUtf8); override;
  end;


{ ************ Web Renderer Returning Mustache Views or Json }

  /// abstract MVC rendering execution context
  // - you shoud not execute this abstract class, but any of the inherited class
  // - one instance inherited from this class would be allocated for each event
  // - may return some data (when inheriting from TMvcRendererReturningData), or
  // even simply display the value in a VCL/FMX GUI, without any output
  TMvcRendererAbstract = class
  protected
    fApplication: TMvcApplication;
    fMethod: PInterfaceMethod;
    fMethodIndex: integer;
    fTix32: cardinal;
    fInput, fRemoteIP, fRemoteUserAgent: RawUtf8;
    procedure Renders(var outContext: variant; status: cardinal;
      forcesError: boolean); virtual; abstract;
    function Redirects(const action: TMvcAction): boolean; virtual;
    procedure AddErrorContext(var context: variant; error: integer);
    procedure CommandError(const ErrorName: RawUtf8; const ErrorValue: variant;
      ErrorCode: integer); virtual;
    function StatusCodeToErrorText(Code: integer): RawUtf8; virtual;
    function ExecuteJsonOverride(var Action: TMvcAction): boolean; virtual;
  public
    /// main execution method of the rendering process
    // - Input should have been set with the incoming execution context
    procedure ExecuteCommand; virtual;
    /// incoming execution context, to be processed via ExecuteCommand() method
    // - should be specified as a raw JSON object
    property Input: RawUtf8
      read fInput write fInput;
    /// access to the owner MVC Application
    property Application: TMvcApplication
      read fApplication;
    /// the associated client IP address
    property RemoteIP: RawUtf8
      read fRemoteIP;
    /// the associated client User Agent
    property RemoteUserAgent: RawUtf8
      read fRemoteUserAgent;
    /// the associated execution method
    property Method: PInterfaceMethod
      read fMethod;
  end;

  /// how TMvcRunWithViews.SetCache should cache the content of a given method
  // - cacheRoot* consider no query-string; whereas cacheWithParameters* will
  // maintain a cache also according to the query-string values
  // - use *IgnoringSession flavour if the output is identical for every visitor
  // - use *IfSession / *IfNoSession variant to save memory when the page differs
  // only between logged-in / guest users
  // - use *WithSession or disable caching if the page is unique per user
  // - see https://gist.github.com/flydev-fr/e0f0a24dc0ab9b39ef1f7bc2ac78f2bc
  TMvcRendererCachePolicy = (
    cacheNone,
    cacheRootIgnoringSession,
    cacheRootIfSession,
    cacheRootIfNoSession,
    cacheRootWithSession,
    cacheWithParametersIgnoringSession,
    cacheWithParametersIfSession,
    cacheWithParametersIfNoSession);

  TMvcRunWithViews = class;

  /// abstract MVC rendering execution context, returning some content
  // - the Output property would contain the content to be returned
  // - can be used to return e.g. some rendered HTML or some raw JSON,
  // or even some server-side generated report as PDF, using our mORMotReport.pas
  TMvcRendererReturningData = class(TMvcRendererAbstract)
  protected
    fRun: TMvcRunWithViews;
    fOutput: TServiceCustomAnswer;
    fInputCookieStart: PUtf8Char;
    fOutputFlags: TMvcViewFlags;
    fCacheDisabled, fExecuteJsonOverride: boolean;
    fInputCookieLen: integer;
    fInputHeaders: PUtf8Char;
    fInputContext: PVariant;
    fCacheCurrentInputValueKey: RawUtf8;
    fOutputCookieName, fOutputCookieValue: RawUtf8;
    function Redirects(const action: TMvcAction): boolean; override;
    function GetCookieFromHeaders(const CookieName: RawUtf8;
      out Value: PUtf8Char): integer; virtual;
    procedure SetCookieToHeaders(const CookieName, CookieValue: RawUtf8); virtual;
    function ExecuteJsonOverride(var Action: TMvcAction): boolean; override;
  public
    /// initialize a rendering process for a given MVC Application/ViewModel
    // - you need to specify a MVC Views engine, e.g. TMvcViewsMustache instance
    // - using a regular method is faster than overriding Create() on FPC
    procedure Prepare(aRun: TMvcRunWithViews; const aIP, aUserAgent: RawUtf8;
      aTix32: cardinal; aContext: PVariant; aHeaders: PUtf8Char;
      aMethod: PInterfaceMethod); virtual;
    /// main execution method of the rendering process
    // - this overriden method would serialize InputContext^ into Input JSON,
    // and handle proper caching as defined by TMvcRunWithViews.SetCache()
    procedure ExecuteCommand; override;
    /// caller should retrieve this value after ExecuteCommand method execution
    property Output: TServiceCustomAnswer
      read fOutput;
    /// set by ExecuteCommand e.g. during TMvcRendererFromViews.Renders
    property OutputFlags: TMvcViewFlags
      read fOutputFlags;
    /// set by ExecuteCommand via SetCookieToHeaders()
    property OutputCookieName: RawUtf8
      read fOutputCookieName;
    /// set by ExecuteCommand via SetCookieToHeaders()
    property OutputCookieValue: RawUtf8
      read fOutputCookieValue;
    /// low-level access to the associated TDocVariantData context (or varEmpty)
    // - TMvcApplication.OnBeforeRender can customize it before ExecuteCommand
    property InputContext: PVariant
      read fInputContext;
  end;
  TMvcRendererReturningDataClass = class of TMvcRendererReturningData;

  /// MVC rendering execution context, returning some rendered View content
  // - will use an associated Views templates system, e.g. a Mustache renderer
  TMvcRendererFromViews = class(TMvcRendererReturningData)
  protected
    // Renders() will fill Output using the corresponding View, to be sent back
    procedure Renders(var outContext: variant; status: cardinal;
      forcesError: boolean); override;
  end;

  /// MVC rendering execution context, returning some un-rendered JSON content
  // - may be used e.g. for debugging purpose
  // - for instance, TMvcRunOnRestServer will return such context with the
  // supplied URI ends with '/json' (e.g. for any /root/method/json request)
  TMvcRendererJson = class(TMvcRendererReturningData)
  protected
    // Renders() will fill Output with the outgoing JSON, to be sent back
    procedure Renders(var outContext: variant; status: cardinal;
      forcesError: boolean); override;
  end;

  /// abstract class used by TMvcApplication to run
  // - a single TMvcApplication logic may handle several TMvcRun instances
  TMvcRun = class
  protected
    fApplication: TMvcApplication;
  public
    /// link this runner class to a specified MVC application
    // - will also reset the associated Application.Session instance
    constructor Create(aApplication: TMvcApplication); reintroduce;
    /// method called to flush the caching mechanism for all MVC commands
    procedure NotifyContentChanged; virtual;
    /// you may call this method to flush any caching mechanism for a MVC command
    procedure NotifyContentChangedForMethod(
      aMethodIndex: integer); overload; virtual;
    /// you may call this method to flush any caching mechanism for a MVC command
    procedure NotifyContentChangedForMethod(
      const aMethodName: RawUtf8); overload;
    /// read-write access to the associated MVC Application/ViewModel instance
    property Application: TMvcApplication
      read fApplication write fApplication;
  end;

  // cache status for one fFactory.Methods[]
  TMvcRunCacheMethod = record
    Safe: TLightLock;
    Policy: TMvcRendererCachePolicy;
    TimeOutSeconds: word;
    RootValueExpirationTime: cardinal;
    RootValue: RawUtf8;
    ParamsValue: TSynNameValue; // Name=input, Value=cached, Tag=timeout
  end;
  /// the kinds of optional content which may be published
  // - publishMvcInfo will define a /root/[aSubURI/]mvc-info HTML page,
  // which is pretty convenient when working with views
  // - publishStatic will define a /root/[aSubURI/].static sub-folder,
  // ready to serve any file available in the Views\.static local folder,
  // via an in-memory cache (if cacheStatic is also defined)
  // - cacheStatic enables an in-memory cache of publishStatic files; if not set,
  // TRestServerUriContext.ReturnFile is called to avoid buffering, which may
  // be a better solution on http.sys or if NGINX's X-Accel-Redirect header is set
  // - registerOrmTableAsExpressions will register Mustache Expression Helpers
  // for every TOrm table of the Server data model (only for TMvcRunOnRestServer)
  // - by default, TRestServer authentication would be by-passed for all
  // MVC routes, unless bypassAuthentication option is undefined
  // - allowJsonFormat will recognize ####/json URIs and return the Mustache
  // data context as plain JSON without any HTML rendering
  // - defaultErrorContext will include basic {{originalErrorContext}}
  // information - could be disabled for verbose object debugging purposes
  TMvcPublishOption = (
    publishMvcInfo,
    publishStatic,
    cacheStatic,
    registerOrmTableAsExpressions,
    bypassAuthentication,
    allowJsonFormat,
    defaultErrorContext);

  /// which kind of optional content should be publish
  TMvcPublishOptions = set of TMvcPublishOption;

  /// abstract class used by TMvcApplication to run TMvcViews-based process
  // - this inherited class will host a MVC Views instance, and handle
  // an optional simple in-memory cache
  TMvcRunWithViews = class(TMvcRun)
  protected
    fViews: TMvcViewsAbstract;
    fMvcInfoCache: RawUtf8;
    fMethodCache: array of TMvcRunCacheMethod; // follows fFactory.Methods[]
    fStaticCache: TSynDictionary;              // RawUtf8/TMvcRunCacheStatic
    fStaticCacheControlMaxAge: integer;
    fOnIdlePurgeCacheTix32: cardinal; // GetTickSec
    fPublishOptions: TMvcPublishOptions;
    fAllowedMethods: TUriMethods;
    procedure ComputeMvcInfoCache;
    procedure ComputeStaticCache(var aCache: TMvcRunCacheStatic;
      const aUri, aContentType: RawUtf8);
    procedure PurgeMethodCache(tix32: cardinal);
    // return cache.FileName or cache.Body+ContentHeader+Etag
    procedure ProcessStatic(const name: RawUtf8; var cached: TMvcRunCacheStatic);
  public
    /// link this runner class to a specified MVC application
    constructor Create(aApplication: TMvcApplication;
      const aTemplatesFolder: TFileName; aViews: TMvcViewsAbstract;
      aPublishOptions: TMvcPublishOptions; aAllowedMethods: TUriMethods); reintroduce;
    /// finalize this instance
    destructor Destroy; override;
    /// method called to flush the caching mechanism for all MVC commands
    procedure NotifyContentChanged; override;
    /// method called to flush the caching mechanism for a MVC command
    procedure NotifyContentChangedForMethod(aMethodIndex: integer); override;
    /// defines the caching policy for a given MVC command
    // - a time expiration period (up to 5 minutes) can also be defined per
    // MVC command - leaving default 0 will set to 5 minutes expiration delay
    // - function calls can be chained to create some fluent definition interface
    // like in TAnyBLogapplication.Create:
    // ! fMainRunner := TMvcRunWithViews.Create(self).
    // !   SetCache('default', cacheRoot);
    function SetCache(const aMethodName: RawUtf8;
      aPolicy: TMvcRendererCachePolicy;
      aTimeOutSeconds: cardinal = 0): TMvcRunWithViews; virtual;
    /// define some custom content for a static file
    // - only used if cacheStatic has been defined
    // - this method is thread safe
    procedure AddStaticCache(const aFileName: TFileName; const aUri: RawUtf8;
      const aBody: RawByteString; const aContentType: RawUtf8 = '');
    /// read-write access to the associated MVC Views instance
    property Views: TMvcViewsAbstract
      read fViews;
    /// current publishing options, as specified to the constructor
    // - all options are included by default
    property PublishOptions: TMvcPublishOptions
      read fPublishOptions write fPublishOptions;
    /// current HTTP methods recognized, as specified to the constructor
    // - equals [mGET, mPOST] by default, as expected by HTML applications
    // with standard www-form
    property AllowedMethods: TUriMethods
      read fAllowedMethods;
    /// optional "Cache-Control: max-age=###" header seconds value for static content
    property StaticCacheControlMaxAge: integer
      read fStaticCacheControlMaxAge write fStaticCacheControlMaxAge;
  end;


{ ************ Application ViewModel/Controller using Interfaces }

  /// Exception class triggerred by mORMot MVC/MVVM applications externally
  // - those error are external errors which should be notified to the client
  // - can be used to change the default view, e.g. on application error:
  // ! procedure TMyMvcApplication.Default(var Scope: variant);
  // ! var data: variant;
  // ! begin
  // !   if not GetDataFromScope(Scope, data) then
  // !     EMvcApplication.GotoError(HTTP_NOTFOUND);
  // !   RenderData(Scope, data); // is never executed after GotoError()
  // ! end;
  // - since exceptions have a performance overhead, this method should be
  // exceptional and you shouild rather use TMvcApplication.Redirect*() methods:
  // ! procedure TMyMvcApplication.Default(var Scope: variant);
  // ! var data: variant;
  // ! begin
  // !   if not GetDataFromScope(Scope, data) then
  // !   begin
  // !     RedirectError(HTTP_NOTFOUND);
  // !     exit; // mandatory to avoid RenderData() execution
  // !   end;
  // !   RenderData(Scope, data);
  // ! end;
  EMvcApplication = class(ESynException)
  protected
    fAction: TMvcAction;
  public
    /// same as calling TMvcApplication.GotoView()
    // - HTTP_TEMPORARYREDIRECT will change the URI, but HTTP_SUCCESS won't
    constructor CreateGotoView(const aMethod: RawUtf8;
      const aParametersNameValuePairs: array of const;
      aStatus: cardinal = HTTP_TEMPORARYREDIRECT);
    /// same as calling TMvcApplication.GotoError()
    constructor CreateGotoError(const aErrorMessage: string;
      aErrorCode: integer = HTTP_BADREQUEST); overload;
    /// same as calling TMvcApplication.GotoError()
    constructor CreateGotoError(aHtmlErrorCode: integer); overload;
    /// same as calling TMvcApplication.GotoDefault
    // - HTTP_TEMPORARYREDIRECT will change the URI, but HTTP_SUCCESS won't
    constructor CreateDefault(aStatus: cardinal = HTTP_TEMPORARYREDIRECT);
    /// just a wrapper around raise CreateGotoView()
    // - consider instead the faster TMvcApplication.RedirectView() method
    class procedure GotoView(const aMethod: RawUtf8;
      const aParametersNameValuePairs: array of const;
      aStatus: cardinal = HTTP_TEMPORARYREDIRECT);
    /// just a wrapper around raise CreateGotoError()
    // - consider instead the faster TMvcApplication.RedirectError() method
    class procedure GotoError(const aErrorMessage: string;
      aErrorCode: integer = HTTP_BADREQUEST); overload;
    /// just a wrapper around raise CreateGotoError()
    // - consider instead the faster TMvcApplication.RedirectError() method
    class procedure GotoError(aHtmlErrorCode: integer); overload;
    /// just a wrapper around raise CreateDefault()
    // - consider instead the faster TMvcApplication.RedirectDefault() method
    class procedure Default(aStatus: cardinal = HTTP_TEMPORARYREDIRECT);
  end;

  /// defines the main and error pages for the ViewModel of one application
  IMvcApplication = interface(IInvokable)
    ['{C48718BF-861B-448A-B593-8012DB51E15D}']
    /// the default main page
    // - whole data context is retrieved and returned as a TDocVariant
    procedure Default(var Scope: variant);
    /// the error page
    // - in addition to the error message, a whole data context is retrieved
    // and returned as a TDocVariant
    procedure Error(var Msg: RawUtf8; var Scope: variant);
  end;

  /// event as called by TMvcApplication.OnBeforeRender/OnAfterRender
  // - should return TRUE to continue the process, or FALSE to abort
  TOnMvcRender = function(Sender: TMvcRendererReturningData): boolean of object;

  /// event called by TMvcApplication.OnSessionCreate/OnSessionFinalized
  TOnMvcSession = procedure(Sender: TMvcSessionAbstract; SessionID: integer;
    const Info: variant) of object;

  /// abstract parent class to implement a MVC/MVVM application
  // - you should not inherit directly from this class, but e.g. mormot.rest.mvc
  // TMvcApplicationRest and associate a TMvcRunOnRestServer
  // - inherits from TInjectableObject, so that you could resolve dependencies
  // via services or stubs, following the IoC pattern
  TMvcApplication = class(TInjectableObject)
  protected
    fFactory: TInterfaceFactory;
    fFactoryEntry: pointer;
    fFactoryErrorIndex: integer;
    fRenderOptions: set of (roDefaultErrorContext);
    fSession: TMvcSessionAbstract;
    fMethodUri: TRawUtf8DynArray;
    fLogClass: TSynLogClass;
    fLocker: IAutoLocker;
    // if any TMvcRun instance is store here, will be freed by Destroy
    // but note that a single TMvcApplication logic may handle several TMvcRun
    fMainRunner: TMvcRun;
    fOnBeforeRender, fOnAfterRender: TOnMvcRender;
    fOnSessionCreate, fOnSessionFinalized: TOnMvcSession;
    fExecuteCached: TInterfaceMethodExecuteCachedDynArray;
    procedure SetInterface(aInterface: PRttiInfo);
    procedure SetSession(Value: TMvcSessionAbstract);
    procedure DoRedirect(var action: TMvcAction); virtual;
    /// generic IMvcApplication.Error method implementation
    procedure Error(var Msg: RawUtf8; var Scope: variant); virtual;
    /// every view will have this data context transmitted as "main":...
    procedure GetViewInfo(MethodIndex: integer; out info: variant); virtual;
    /// compute the data context e.g. for the /mvc-info URI
    procedure GetMvcInfo(out info: variant); virtual;
  public
    /// finalize the application
    // - and release any associated CurrentSession, Views, and fMainRunner
    destructor Destroy; override;

    /// to be called when the data model did change to force content re-creation
    // - this default implementation will call fMainRunner.NotifyContentChanged
    procedure FlushAnyCache; virtual;
    /// wrapper to redirect a TMvcAction method result into another view
    // - if status is HTTP_TEMPORARYREDIRECT, it will change the URI
    // whereas HTTP_SUCCESS would just render the view for the current URI
    class procedure GotoView(var Action: TMvcAction; const MethodName: RawUtf8;
      const ParametersNameValuePairs: array of const;
      Status: cardinal = HTTP_TEMPORARYREDIRECT);
    /// wrapper to redirect a TMvcAction method result into the 'Error' view
    class procedure GotoError(var Action: TMvcAction; const Msg: string;
      ErrorCode: integer = HTTP_BADREQUEST); overload;
    /// wrapper to redirect a TMvcAction method result into the 'Error' view
    class procedure GotoError(var Action: TMvcAction;
      ErrorCode: integer); overload;
    /// wrapper to redirect a TMvcAction method result into the 'Default' view
    class procedure GotoDefault(var Action: TMvcAction;
      Status: cardinal = HTTP_TEMPORARYREDIRECT);
    /// wrapper to redirect to the 'Error' view with a HTTP status code
    // - is similar to EMvcApplication.GotoError() with less overhead, but
    // needing an explicit "exit" to stop the execution flow of the method
    // ! procedure TMyMvcApplication.Default(var Scope: variant);
    // ! var data: variant;
    // ! begin
    // !   if not GetDataFromScope(Scope, data) then
    // !   begin
    // !     RedirectError(HTTP_NOTFOUND);
    // !     exit; // mandatory to avoid RenderData() execution
    // !   end;
    // !   RenderData(Scope, data);
    // ! end;
    procedure RedirectError(ErrorCode: integer); overload;
    /// wrapper to redirect to the 'Error' view with an error message
    // - is similar to EMvcApplication.GotoError() with less overhead, but
    // needing an explicit "exit" to stop the execution flow of the method
    procedure RedirectError(const Msg: string;
      ErrorCode: integer = HTTP_BADREQUEST); overload;
    /// wrapper to redirect to the 'Default' view
    // - is similar to EMvcApplication.Default() with less overhead, but
    // needing an explicit "exit" to stop the execution flow of the method
    procedure RedirectDefault(Status: cardinal = HTTP_TEMPORARYREDIRECT);
    /// wrapper to override the current view into another
    // - is similar to EMvcApplication.GotoView() with less overhead, but
    // needing an explicit "exit" to stop the execution flow of the method
    // - as called by other RedirectError/RedirectDefault methods
    // - if status is HTTP_TEMPORARYREDIRECT, it will change the URI
    // whereas HTTP_SUCCESS would just render the view for the current URI
    procedure RedirectView(const MethodName: RawUtf8;
      const ParametersNameValuePairs: array of const;
      Status: cardinal = HTTP_TEMPORARYREDIRECT);

    /// read-only access to the associated factory for IMvcApplication interface
    property Factory: TInterfaceFactory
      read fFactory;
    /// the URI of each Factory.Methods[]
    // - e.g. 'start' for IService._Start()
    property MethodUri: TRawUtf8DynArray
      read fMethodUri;
    /// read-write access to the associated Session instance
    property CurrentSession: TMvcSessionAbstract
      read fSession write SetSession;
    /// this event is called before a page is rendered
    // - you can override the supplied Sender.InputContext^ if needed
    // - note that Sender.Input JSON is not yet computed
    property OnBeforeRender: TOnMvcRender
      read fOnBeforeRender write fOnBeforeRender;
    /// this event is called after the page has been rendered
    // - Sender.Output contains the result of Renderer.ExecuteCommand
    // - note that Sender.Input has been unserialized so is no valid JSON anymore
    property OnAfterRender: TOnMvcRender
      read fOnAfterRender write fOnAfterRender;
    /// this event is called when a session/cookie has been initiated
    property OnSessionCreate: TOnMvcSession
      read fOnSessionCreate write fOnSessionCreate;
    /// this event is called when a session/cookie has been finalized
    property OnSessionFinalized: TOnMvcSession
      read fOnSessionFinalized write fOnSessionFinalized;

    /// the associated TSynLog class
    property LogClass: TSynLogClass
      read fLogClass write fLogClass;
    /// global mutex which may be used to protect ViewModel/Controller code
    // - you may call Locker.ProtectMethod in any implementation method to
    // ensure that no other thread would access the same data
    // - for store some cache data among methods, you may consider defining a
    // ILockedDocVariant private field, and use it to store values safely
    // - note that regular RestModel CRUD operations are already thread safe, so
    // it is not necessary to use this Locker with ORM or SOA methods
    property Locker: IAutoLocker
      read fLocker;
    /// read-write access to the main associated TMvcRun instance
    // - if any TMvcRun instance is stored here, will be freed by Destroy
    // - but note that a single TMvcApplication logic may handle several TMvcRun
    property MainRunner: TMvcRun
      read fMainRunner;
  end;



var
  /// the default sub-URI path of the static content used for Views
  STATIC_URI: RawUtf8 = '.static';


implementation


{ ************ Web Views Implementation using Mustache }

const
  MUSTACHE_METHODPARTIAL =
  '{{<method}}{{verb}} {{methodName}}{{#hasInParams}}({{#args}}{{^dirResult}}' +
  '{{dirName}} {{argName}}: {{typeDelphi}}{{commaArg}}{{/dirResult}}{{/args}})' +
  '{{/hasInParams}}{{#args}}{{#dirResult}}: {{typeDelphi}}{{/dirResult}}' +
  '{{/args}};{{/method}}';

  MUSTACHE_VOIDVIEW = MUSTACHE_METHODPARTIAL +
  '<<! void template created for the {{interfaceName}}.{{methodName}} View:'#13#10 +
  ' defined as'#13#10'   {{>method}}'#13#10' with the following data context:'#13#10 +
  '   * Main: variant'#13#10'{{#args}}{{#dirOutput}}   * {{argName}}:' +
  ' {{typePascal}}'#13#10'{{/dirOutput}}{{/args}}>>'#13#10;

  MUSTACHE_MVCINFO = MUSTACHE_METHODPARTIAL +
  '{{<url}}/{{root}}/{{uriName}}{{#hasInParams}}?' +
  '{{#args}}{{#dirInput}}{{camelName}}=</b>..[{{typePascal}}]..<b>' +
  '{{#commaInSingle}}&{{/commaInSingle}}{{/dirInput}}{{/args}}{{/hasInParams}}{{/url}}' +
  '{{<mustache}}<b>&#123;{main&#125;}</b>: Variant{{#args}}{{#dirOutput}}<br><b>{&#123;' +
  '{{uriName}}&#125;}</b>: {{typePascal}}{{/dirOutput}}{{/args}}{{/mustache}}' +
  '<!DOCTYPE html><html><head><title>{{Name}} Information</title></head><body ' +
  'style="font-family:Verdana;"><h1>{{Name}} mormot.rest.mvc Information</h1>' +
  '<p><strong>Generated by a <i>mORMot</i> {{mORMot}} server</strong><br>' +
  '<small>&copy;Synopse Informatique - <a href=https://synopse.info>' +
  'https://synopse.info</a></small></p><h2>Controller Definition</h2>' +
  '<p>Registered interface is:</p><pre>'#13#10 +
  '  I{{name}} = interface(IInvokable)'#13#10'{{#methods}}'#13#10 +
  '    {{>method}}'#13#10'{{/methods}}'#13#10'  end;'#13#10 +
  '</pre><p>Use this page as reference when writing your <a href=' +
  'https://mustache.github.io>Mustache</a> Views.</p>' +
  '<h2>Available Commands</h2><p>You can access the following commands:</p>' +
  '<ul>{{#methods}}<li><b>{{>url}}</b>{{/methods}}</ul><p>Any missing parameter ' +
  'would be replaced by its default value.</p><h2>Available Views</h2>' +
  '<p>The following views are defined, with expected data context:</p><ul>' +
  '{{#methods}}{{^resultIsServiceCustomAnswer}}<li><b>{{>url}}</b><p>{{>mustache}}' +
  '</p></li>{{/resultIsServiceCustomAnswer}}{{/methods}}</ul><p>' +
  'Currently, all views are located in the <code>{{viewsFolder}}</code> folder.</p>';

  MUSTACHE_DEFAULTERROR =
  '<!DOCTYPE html><html><head><title>mormot.rest.mvc Error</title></head><body style=' +
  '"font-family:Verdana;"><h1>mormot.rest.mvc Default Error Page</h1><p>A <code>' +
  '{{exceptionName}}</code> exception did raise during {{className}} process ' +
  'with the following message:</p><pre>{{exceptionMessage}}</pre><p>' +
  'Triggered with the following context:</p><pre>{{originalErrorContext}}</pre>';


{ TMvcViewsAbstract }

constructor TMvcViewsAbstract.Create(aInterface: PRttiInfo;
  aLogClass: TSynLogClass);
begin
  inherited Create;
  fFactory := TInterfaceFactory.Get(aInterface);
  fFactoryErrorIndex := fFactory.FindMethodIndex('Error');
  if aLogClass = nil then
    fLogClass := TSynLog
  else
    fLogClass := aLogClass;
  fViewGenerationTimeTag := '[[GENERATION_TIME_TAG]]';
end;

procedure TMvcViewsAbstract.SetViewTemplateFolder(const aFolder: TFileName);
begin
  fViewTemplateFolder := IncludeTrailingPathDelimiter(aFolder);
  fViewStaticFolder := MakePath([fViewTemplateFolder,  STATIC_URI], true);
end;

procedure TMvcViewsAbstract.FillStaticFileName(var aFile: TMvcRunCacheStatic);
begin
  aFile.FileName := fViewStaticFolder + aFile.FileName;
end;

procedure TMvcViewsAbstract.FillStaticFileContent(var aFile: TMvcRunCacheStatic);
begin
  aFile.Body := StringFromFile(aFile.FileName);
end;

procedure TMvcViewsAbstract.NotifyContentChanged;
begin
  // TMvcViewsMustache.NotifyContentChanged will reload all partials
end;


{ Customization of HTML CSS tables }

{ TExpressionHtmlTableStyle }

class procedure TExpressionHtmlTableStyle.AddLabel(WR: TTextWriter;
  const text: string; kind: THtmlTableStyleLabel);
const
  SETLABEL: array[THtmlTableStyleLabel] of TShort3 = (
    '', '', '- ', '+ ', '');
begin
  WR.AddShorter(SETLABEL[kind]);
  WR.AddHtmlEscapeString(text);
  WR.AddShorter('&nbsp;');
end;

class procedure TExpressionHtmlTableStyle.AfterValue(WR: TTextWriter);
begin
  WR.AddShort('</td></tr>');
end;

class procedure TExpressionHtmlTableStyle.BeforeFieldName(WR: TTextWriter);
begin
  WR.AddShorter('<tr><td>');
end;

class procedure TExpressionHtmlTableStyle.BeforeValue(WR: TTextWriter);
begin
  WR.AddShort('</td><td>');
end;

class procedure TExpressionHtmlTableStyle.EndTable(WR: TTextWriter);
begin
  WR.AddShorter('</table>');
end;

class procedure TExpressionHtmlTableStyle.StartTable(WR: TTextWriter);
begin
  WR.AddShorter('<table>');
end;


{ TExpressionHtmlTableStyleBootstrap }

class procedure TExpressionHtmlTableStyleBootstrap.AddLabel(WR: TTextWriter;
  const text: string; kind: THtmlTableStyleLabel);
const
  SETLABEL: array[THtmlTableStyleLabel] of string[7] = (
    'danger', 'success', 'danger', 'success', 'primary');
begin
  WR.AddShort('<span class="label label-');
  WR.AddShorter(SETLABEL[kind]);
  WR.Add('"', '>');
  WR.AddHtmlEscapeString(text);
  WR.AddShorter('</span>');
end;

class procedure TExpressionHtmlTableStyleBootstrap.StartTable(WR: TTextWriter);
begin
  WR.AddShort('<table class="table table-striped table-bordered">');
end;



{ TMvcViewsMustache }

constructor TMvcViewsMustache.Create(aInterface: PRttiInfo;
  const aParameters: TMvcViewsMustacheParameters; aLogClass: TSynLogClass);
var
  i, n: PtrInt;
  folder, ext: TFileName;
  files: TFileNameDynArray;
  info: variant;
  v: ^TMvcViewMustache;
  m: PInterfaceMethod;
begin
  inherited Create(aInterface, aLogClass);
  // get views
  fViewTemplateFileTimestampMonitor := aParameters.FileTimestampMonitorAfterSeconds;
  folder := aParameters.Folder;
  if folder = '' then
  begin
    folder := Executable.ProgramFilePath + 'Views';
    if not DirectoryExists(folder) then
      DirectoryExistsMake([Executable.ProgramFilePath + '..', 'Views'], @folder);
  end;
  SetViewTemplateFolder(folder); // set with the proper method
  if (aParameters.ExtensionForNotExistingTemplate <> '') and
     not DirectoryExists(folder) then
    ForceDirectories(folder);
  if aParameters.CsvExtensions = '' then
    ext := ',html,json,css,'
  else
    ext := ',' + SysUtils.LowerCase(aParameters.CsvExtensions) + ',';
  SetLength(fViews, fFactory.MethodsCount);
  v := pointer(fViews);
  m := pointer(fFactory.Methods);
  n := fFactory.MethodsCount;
  repeat
    v^.Safe.Init; // mandatory for OS locks
    if MethodHasView(m^) then
    begin
      Utf8ToFileName(m^.Uri, v^.MethodName);
      v^.SearchPattern := v^.MethodName + '.*';
      files := FindTemplateFileNames(v^.SearchPattern);
      if files <> nil then
        for i := 0 to length(files) - 1 do
        begin
          v^.FileName := files[i];
          v^.ShortFileName := ExtractFileName(v^.FileName);
          v^.FileExt := SysUtils.LowerCase(ExtractExt(v^.ShortFileName, true));
          if Pos(',' + v^.FileExt + ',', ext) > 0 then
            // found a template with the right extension
            break;
        end
        // if no exact extension match, return last matching 'MethodName.*'
      else
      begin
        fLogClass.Add.Log(sllWarning,
          '%.Create: Missing View file in %', [self, v^.SearchPattern]);
        if aParameters.ExtensionForNotExistingTemplate <> '' then
        begin
          // create void template content with methods information as comment
          v^.FileExt := aParameters.ExtensionForNotExistingTemplate;
          v^.ShortFileName := v^.MethodName + v^.FileExt;
          delete(v^.FileExt, 1, 1); // remove initial '.'
          v^.FileName := fViewTemplateFolder + v^.ShortFileName; // default place
          info := ContextFromMethod(m^);
          _ObjAddProp(
            'interfaceName', fFactory.InterfaceRtti.Name, info);
          FileFromString(StringReplaceChars(StringReplaceChars(
            TSynMustache.Parse(MUSTACHE_VOIDVIEW).Render(info),
            '<', '{'), '>', '}'), v^.FileName);
        end;
      end;
      v^.ContentTypeHeader := GetMimeContentTypeHeader('', v^.ShortFileName);
    end;
    inc(v);
    inc(m);
    dec(n);
  until n = 0;
  fViewHelpers := aParameters.Helpers;
  // get partials
  fViewPartials := TSynMustachePartials.Create;
  NotifyContentChanged;
end;

procedure TMvcViewsMustache.NotifyContentChanged;
var
  i: PtrInt;
  files: TFileNameDynArray;
  content: RawUtf8;
begin
  files := FindPartialFileNames;
  fViewPartials.List.Safe.WriteLock;
  try
    fViewPartials.List.Clear;
    for i := 0 to length(files) - 1 do
    begin
      content := StringFromFile(files[i]);
      if not IsVoid(content) then
      try
        fViewPartials.Add(GetFileNameWithoutExtOrPath(files[i]), content);
      except
        on E: Exception do
          fLogClass.Add.Log(sllError, '%.Create: Invalid Partial file % - %',
            [self, files[i], E]);
      end;
    end;
  finally
    fViewPartials.List.Safe.WriteUnLock;
  end;
end;

constructor TMvcViewsMustache.Create(aInterface: PRttiInfo;
  const aTemplatesFolder: TFileName; aLogClass: TSynLogClass;
  const aExtensionForNotExistingTemplate: TFileName);
var
  params: TMvcViewsMustacheParameters;
begin
  FillcharFast(params, SizeOf(params), 0);
  params.Folder := aTemplatesFolder;
  params.FileTimestampMonitorAfterSeconds := 5;
  params.ExtensionForNotExistingTemplate := aExtensionForNotExistingTemplate;
  params.Helpers := TSynMustache.HelpersGetStandardList;
  Create(aInterface, params, aLogClass);
end;

destructor TMvcViewsMustache.Destroy;
var
  i: PtrInt;
begin
  inherited;
  fViewPartials.Free;
  for i := 0 to length(fViews) - 1 do
    fViews[i].Safe.Done; // mandatory for OS locks
end;

function TMvcViewsMustache.RegisterExpressionHelpers(
  const aNames: array of RawUtf8;
  const aEvents: array of TSynMustacheHelperEvent): TMvcViewsMustache;
begin
  if self <> nil then
    TSynMustache.HelperAdd(fViewHelpers, aNames, aEvents);
  result := self;
end;

function TMvcViewsMustache.RegisterExpressionHelpersForCrypto: TMvcViewsMustache;
begin
  result := RegisterExpressionHelpers(['md5', 'sha1', 'sha256', 'sha512'],
                                      [ md5,   sha1,   sha256,   sha512 ]);
end;

class procedure TMvcViewsMustache.md5(const Value: variant;
  out Result: variant);
begin
  RawUtf8ToVariant(mormot.crypt.core.Md5(ToUtf8(Value)), Result);
end;

class procedure TMvcViewsMustache.sha1(const Value: variant;
  out Result: variant);
begin
  RawUtf8ToVariant(mormot.crypt.core.Sha1(ToUtf8(Value)), Result);
end;

class procedure TMvcViewsMustache.sha256(const Value: variant;
  out Result: variant);
begin
  RawUtf8ToVariant(mormot.crypt.core.Sha256(ToUtf8(Value)), Result);
end;

class procedure TMvcViewsMustache.sha512(const Value: variant;
  out Result: variant);
begin
  RawUtf8ToVariant(mormot.crypt.core.Sha512(ToUtf8(Value)), Result);
end;

function TMvcViewsMustache.FindTemplateFileNames(
  const Mask: TFileName): TFileNameDynArray;
begin
  result := FileNames(fViewTemplateFolder, Mask);
end;

function TMvcViewsMustache.GetTemplateFileAge(const View: TMvcViewMustache): TUnixTime;
begin
  result := FileAgeToUnixTimeUtc(View.FileName);
end;

procedure TMvcViewsMustache.SetTemplateContent(var View: TMvcViewMustache);
begin
  View.Template := StringFromFile(View.FileName);
end;

function TMvcViewsMustache.FindPartialFileNames: TFileNameDynArray;
begin
  result := FileNames(fViewTemplateFolder, '*.partial');
end;

function TMvcViewsMustache.Render(methodIndex: integer; const Context: variant;
  var Answer: TServiceCustomAnswer): TMvcViewFlags;
var
  age: TUnixTime;
  m: TSynMustache;
  v: ^TMvcViewMustache;
  tix32: cardinal;

    procedure UpdateView;
    begin
      v^.Mustache := nil; // no Mustache.Free: TSynMustache instances are cached
      if v^.MethodName = '' then
        EMvcException.RaiseUtf8('%.Render(%): not a View',
          [self, fFactory.Methods[methodIndex].Uri]);
      if v^.FileName = '' then
        EMvcException.RaiseUtf8('%.Render(''%''): Missing Template in ''%''',
          [self, v^.MethodName, v^.SearchPattern]);
      v^.FileAgeLast := age;
      SetTemplateContent(v^);
      if v^.Template <> '' then
      try
        v^.Mustache := TSynMustache.Parse(v^.Template);
        if v^.Mustache.FoundInTemplate(fViewGenerationTimeTag, fViewPartials) then
          include(v^.Flags, viewHasGenerationTimeTag);
      except
        on E: Exception do
          EMvcException.RaiseUtf8('%.Render(''%''): Invalid Template: % - %',
            [self, v^.ShortFileName, E, E.Message]);
      end
      else
        EMvcException.RaiseUtf8('%.Render(''%''): Missing Template in ''%''',
          [self, v^.ShortFileName, v^.SearchPattern]);
      if tix32 <> 0 then
        v^.FileAgeTix32 := tix32 + fViewTemplateFileTimestampMonitor;
    end;

begin
  // retrieve (and update if needed) the TSynMustache instance of this view
  if cardinal(methodIndex) >= cardinal(length(fViews)) then
    EMvcException.RaiseUtf8('%.Render(methodIndex=%)', [self, methodIndex]);
  tix32 := 0;
  if fViewTemplateFileTimestampMonitor <> 0 then
    tix32 := GetTickSec;
  v := @fViews[methodIndex];
  v^.Safe.Lock;
  try
    if (v^.Mustache = nil) or
       ((tix32 <> 0) and
        (tix32 >= v^.FileAgeTix32)) then
    begin
      age := GetTemplateFileAge(v^);
      if (v^.Mustache = nil) or
         (age <> v^.FileAgeLast) then
        UpdateView;
    end;
    m := v^.Mustache;
    result := v^.Flags;
    Answer.Header := v^.ContentTypeHeader;
  finally
    v^.Safe.UnLock;
  end;
  // render the TSynMustache template
  Answer.Content := m.Render(Context, fViewPartials, fViewHelpers);
  if Answer.Content <> '' then
    exit;
  // rendering failure
  v^.Safe.Lock;
  v^.Mustache := nil; // force reload view ASAP
  v^.Safe.UnLock;
  EMvcException.RaiseUtf8(
    '%.Render(''%''): Void or invalid template: please fix %',
    [self, v^.ShortFileName, v^.FileName]);
end;

function TMvcViewsMustache.RenderFlags(methodIndex: PtrInt): TMvcViewFlags;
begin
  if PtrUInt(methodIndex) >= PtrUInt(length(fViews)) then
    result := []
  else
    result := fViews[methodIndex].Flags;
end;


function MethodHasView(const aMethod: TInterfaceMethod): boolean;
begin
  // any method returning a TMvcAction does not have an associated view
  result := (aMethod.ArgsResultIndex < 0) or
    (aMethod.Args[aMethod.ArgsResultIndex].ArgRtti.Info <> TypeInfo(TMvcAction));
end;



{ ************ ViewModel/Controller Sessions using Cookies }

{ TMvcSessionAbstract }

constructor TMvcSessionAbstract.Create(Owner: TMvcApplication);
begin
  fApplication := Owner;
  inherited Create;
end;

procedure CookieRecordToVariant(rec: pointer; recrtti: PRttiInfo;
  var result: variant);
var
  json: RawUtf8;
  rc: TRttiCustom;
begin
  // create a TDocVariant from the binary record content
  rc := SaveJson(rec^, recrtti, TEXTWRITEROPTIONS_MUSTACHE, json);
  TDocVariantData(result).InitJsonInPlace(pointer(json), JSON_MVC, nil,
    {capacity=}rc.PropsCount + 1); // +1 for the additional "id" field
end;

function TMvcSessionAbstract.CheckAndRetrieveInfo(PRecordDataTypeInfo: PRttiInfo;
  PSessionID: PInteger; Invalidate: boolean): variant;
var
  rec: TByteToWord; // 512 bytes to store locally any kind of record
  recsize: integer;
  sessionID: integer;
begin
  SetVariantNull(result);
  if PRecordDataTypeInfo = nil then
    // sessionID decoding only
    recsize := 0
  else
  begin
    // binary decoding of a record/object
    recsize := PRecordDataTypeInfo^.RecordSize;
    if (recsize = 0) or // = 0 if not rkRecordTypes
       (recsize > SizeOf(rec)) then
      EMvcException.RaiseUtf8('%.CheckAndRetrieveInfo: incorrect % % (size=%)',
        [self, PRecordDataTypeInfo^.RawName, ToText(PRecordDataTypeInfo^.Kind)^,
         recsize]);
    FillCharFast(rec, recsize, 0);
  end;
  try
    sessionID := CheckAndRetrieve(@rec, PRecordDataTypeInfo, nil, Invalidate);
    if PSessionID <> nil then
      PSessionID^ := sessionID;
    if sessionID <> 0 then
    begin
      if recsize > 0 then
        CookieRecordToVariant(@rec, PRecordDataTypeInfo, result);
      _ObjAddProps(['id', sessionID], result);
    end
    else
      recsize := 0; // rec is still filled with zeros
  finally
    if recsize > 0 then
      // manual finalization of managed fields
      FastRecordClear(@rec, PRecordDataTypeInfo);
  end;
end;


{ TMvcSessionWithCookies }

constructor TMvcSessionWithCookies.Create(Owner: TMvcApplication);
begin
  inherited Create(Owner);
  fContext := TBinaryCookieGenerator.Create('mORMot');
end;

destructor TMvcSessionWithCookies.Destroy;
begin
  inherited Destroy;
  fContext.Free;
end;

function TMvcSessionWithCookies.Exists: boolean;
var
  dummy: PUtf8Char;
begin
  result := GetCookie(dummy) <> 0;
end;

function TMvcSessionWithCookies.GetCookieName: RawUtf8;
begin
  result := fContext.CookieName;
end;

procedure TMvcSessionWithCookies.SetCookieName(const Value: RawUtf8);
begin
  if not PropNameValid(pointer(Value)) then
     EMvcException.RaiseUtf8('%.SetCookieName(%): invalid name', [self, Value]);
  fContext.CookieName := Value;
end;

function TMvcSessionWithCookies.CheckAndRetrieve(PRecordData: pointer;
  PRecordTypeInfo: PRttiInfo; PExpires: PUnixTime; Invalidate: boolean): integer;
var
  cookie: PUtf8Char;
begin
  result := GetCookie(cookie);
  if result = 0 then
    exit; // no cookie -> no session
  result := fContext.Validate(
    cookie, result, PRecordData, PRecordTypeInfo, PExpires, nil, Invalidate);
  if (result <= 0) and
     not Invalidate then
    // delete any invalid/expired cookie on server side
    Finalize;
end;

function TMvcSessionWithCookies.Initialize(PRecordData: pointer;
  PRecordTypeInfo: PRttiInfo; SessionTimeOutMinutes: cardinal): integer;
var
  cookie: RawUtf8;
  info: variant;
begin
  result := fContext.Generate(cookie, SessionTimeOutMinutes,
    PRecordData, PRecordTypeInfo);
  if result = 0 then
    exit;
  if Assigned(fApplication) and
     Assigned(fApplication.OnSessionCreate) then
  begin
    if (PRecordData <> nil) and
       (PRecordTypeInfo <> nil) then
      CookieRecordToVariant(PRecordData, PRecordTypeInfo, info);
    fApplication.OnSessionCreate(self, result, info);
  end;
  SetCookie(cookie); // will be sent back to the client and stored there
end;

procedure TMvcSessionWithCookies.Finalize(PRecordTypeInfo: PRttiInfo);
var
  sessionID: integer;
  info: variant;
begin
  if Assigned(fApplication) and
     Assigned(fApplication.OnSessionFinalized) then
  begin
    info := CheckAndRetrieveInfo(PRecordTypeInfo, @sessionID, {invalidate=}true);
    if sessionID = 0 then
      exit; // nothing to finalize
    fApplication.OnSessionFinalized(self, sessionID, info);
  end;
  SetCookie(COOKIE_EXPIRED); // notify the client to delete this cookie
end;

function TMvcSessionWithCookies.LoadContext(const Saved: RawUtf8): boolean;
begin
  result := fContext.Load(Saved);
end;

function TMvcSessionWithCookies.SaveContext: RawUtf8;
begin
  result := fContext.Save;
end;


{ TMvcSessionSingle }

function TMvcSessionSingle.GetCookie(out Value: PUtf8Char): integer;
begin
  Value := pointer(fSingleCookie);
  result := length(fSingleCookie);
end;

procedure TMvcSessionSingle.SetCookie(const Value: RawUtf8);
begin
  fSingleCookie := Value;
end;


{ TMvcSessionWithRenderer }

threadvar
  _CurrentRenderer: TMvcRendererReturningData; // per-thread context

function TMvcSessionWithRenderer.GetCookie(out Value: PUtf8Char): integer;
var
  ctxt: TMvcRendererReturningData;
begin
  result := 0; // avoid GPF on virtual method execution
  ctxt := _CurrentRenderer;
  if ctxt = nil then
    exit;
  result := ctxt.GetCookieFromHeaders(fContext.CookieName, Value);
  if result <> 0 then
    exit;
  Value := pointer(ctxt.fOutputCookieValue);
  if (Value <> nil) and
     (ctxt.fOutputCookieName = fContext.CookieName) then
    result := PStrLen(Value - _STRLEN)^; // return value from SetCookie()
end;

procedure TMvcSessionWithRenderer.SetCookie(const Value: RawUtf8);
var
  ctxt: TMvcRendererReturningData;
begin
  ctxt := _CurrentRenderer;
  if ctxt <> nil then // avoid GPF on virtual method execution
    ctxt.SetCookieToHeaders(fContext.CookieName, Value); // fOutputCookieName/Value
end;


{ ************ Web Renderer Returning Mustache Views or Json }

{ TMvcRendererAbstract }

procedure TMvcRendererAbstract.CommandError(const ErrorName: RawUtf8;
  const ErrorValue: variant; ErrorCode: integer);
var
  info, renderContext: variant;
begin
  fApplication.GetViewInfo(fMethodIndex, info);
  renderContext := _ObjFast([
    'main',      info,
    ErrorName, ErrorValue]);
  AddErrorContext(renderContext, ErrorCode);
  Renders(renderContext, ErrorCode, true);
end;

function TMvcRendererAbstract.StatusCodeToErrorText(Code: integer): RawUtf8;
begin
  result := mormot.core.text.StatusCodeToErrorMsg(Code); // default English
end;

procedure TMvcRendererAbstract.AddErrorContext(
  var context: variant; error: integer);
var
  details: RawUtf8;
begin
  _ObjAddProps([
    'msg',       StatusCodeToErrorText(error),
    'errorCode', error,
    'ip',        fRemoteIP,
    'useragent', fRemoteUserAgent], context, {dontadddef=}true);
  if roDefaultErrorContext in fApplication.fRenderOptions then
    Join([fApplication.fFactory.InterfaceName, ' ', NowToString,
      ' ', fRemoteIP, ' ', fRemoteUserAgent], details)
  else
    details := JsonReformat(VariantSaveJson(context));
  _ObjAddPropU('originalErrorContext', details, context);
end;

function TMvcRendererAbstract.ExecuteJsonOverride(var Action: TMvcAction): boolean;
begin
  result := false; // do nothing by default
end;

procedure TMvcRendererAbstract.ExecuteCommand;
var
  exec: TInterfaceMethodExecuteCached;
  isAction: boolean;
  renderContext: TDocVariantData;
  info: variant;
  action: TMvcAction;
  err: ShortString;
begin
  action.ReturnedStatus := HTTP_SUCCESS;
  try
    if fMethod <> nil then
    repeat
      try
        // execute the method and generate the JSON output
        isAction := imfResultIsServiceCustomAnswer in fMethod^.Flags;
        exec := fApplication.fExecuteCached[fMethodIndex].Acquire(
                  [], [twoForceJsonExtended]);
        try
          exec.WR.AddDirect('{');
          exec.ServiceCustomAnswerStatus := action.ReturnedStatus;
          err := '';
          if not exec.ExecuteJson([fApplication.fFactoryEntry],
              pointer(fInput), exec.WR, @err, true) then
          begin
            if err = '' then
              err := 'execution error';
            EMvcException.RaiseUtf8('%.CommandRunMethod(I%): %',
              [self, fMethod^.InterfaceDotMethodName, err])
          end;
          if ExecuteJsonOverride(action) then
            // TMvcRendererReturningData override via _CurrentRenderer
            isAction := true
          else if isAction then
          begin
            // was a TMvcAction mapped in a TServiceCustomAnswer record
            action.RedirectToMethodName := exec.ServiceCustomAnswerHead;
            action.ReturnedStatus       := exec.ServiceCustomAnswerStatus;
            exec.WR.SetText(action.RedirectToMethodParameters);
          end
          else
          begin
            // compute the Mustache data context directly from the WR buffer
            exec.WR.AddDirect('}');
            renderContext.InitJsonInPlace(exec.WR.GetTextAsBuffer, JSON_MVC,
              nil, {capacity=}fMethod^.ArgsOutputValuesCount + 1);
          end;
        finally
          fApplication.fExecuteCached[fMethodIndex].Release(exec);
        end;
        if not isAction then
        begin
          fApplication.GetViewInfo(fMethodIndex, info);
          renderContext.AddValue('main', info);
          if fMethodIndex = fApplication.fFactoryErrorIndex then
            AddErrorContext(variant(renderContext), action.ReturnedStatus);
          // rendering, e.g. with fast Mustache {{template}} over TDocVariant
          Renders(variant(renderContext), action.ReturnedStatus, false);
          // now fOutput contains the rendered result
          exit; // success
        end;
      except
        // handle EMvcApplication.GotoView/GotoError/Default redirections
        on E: EMvcApplication do
          // lower level exceptions will be handled just below
          action := E.fAction;
      end;
      // handle TMvcAction redirection
      fInput := action.RedirectToMethodParameters;
      fMethodIndex := fApplication.fFactory.
        FindMethodIndex(action.RedirectToMethodName);
      if action.ReturnedStatus = 0 then
        action.ReturnedStatus := HTTP_SUCCESS
      else if (action.ReturnedStatus = HTTP_TEMPORARYREDIRECT) or
              (action.ReturnedStatus = HTTP_FOUND) or
              (action.ReturnedStatus = HTTP_SEEOTHER) or
              (action.ReturnedStatus = HTTP_MOVEDPERMANENTLY) then
        if Redirects(action) then
          // if redirection is implemented
          exit
        else
          // fallback is to stay here
          action.ReturnedStatus := HTTP_SUCCESS;
      if fMethodIndex < 0 then
        exit; // no loop to handle redirection
      fMethod := @fApplication.fFactory.Methods[fMethodIndex];
    until false;
    // if we reached here, there was a wrong URI -> render the 404 error page
    CommandError('notfound', true, HTTP_NOTFOUND);
  except
    on E: Exception do
      CommandError('exception',
        ObjectToVariantDebug(E, '%.ExecuteCommand', [self]),
        HTTP_SERVERERROR);
  end;
end;

function TMvcRendererAbstract.Redirects(const action: TMvcAction): boolean;
begin
  result := false;
end; // indicates redirection did not happen -> caller should do it manually


{ TMvcRendererFromViews }

procedure TMvcRendererFromViews.Renders(var outContext: variant;
  status: cardinal; forcesError: boolean);

  procedure RenderError;
  begin
    try
      // specific rendering of the error context
      fOutputFlags := fRun.fViews.Render(
        fRun.fViews.fFactoryErrorIndex, outContext, fOutput);
    except
      // fallback to our default HTML error template, if custom one is buggy
      on E: Exception do
      begin
        _ObjAddProps([
        'exceptionName',    ClassNameShort(E)^,
        'exceptionMessage', E.Message,
        'className',        ClassNameShort(self)^], outContext);
        fOutput.Content := TSynMustache.Parse(MUSTACHE_DEFAULTERROR).
                           Render(outContext);
        fOutput.Header := HTML_CONTENT_TYPE_HEADER;
      end;
    end;
  end;

var
  head: PVarData;
begin
  if forcesError or
     (fMethodIndex = fRun.fViews.fFactoryErrorIndex) then
    // specific rendering of an error page
    RenderError
  else
    // regular view page rendering
    fOutputFlags := fRun.fViews.Render(fMethodIndex, outContext, fOutput);
  head := _Safe(outContext)^.GetVarData('CustomOutHttpHeader');
  if head <> nil then
    AppendLine(fOutput.Header, [PVariant(head)^]);
  fOutput.Status := status;
end;


{ TMvcRendererJson }

procedure TMvcRendererJson.Renders(var outContext: variant; status: cardinal;
  forcesError: boolean);
var
  json: RawUtf8;
begin
  VariantToUtf8(outContext, json);
  JsonBufferReformat(pointer(json), RawUtf8(fOutput.Content));
  fOutput.Header := JSON_CONTENT_TYPE_HEADER_VAR;
  fOutput.Status := status;
end;


{ TMvcRun }

constructor TMvcRun.Create(aApplication: TMvcApplication);
begin
  if aApplication = nil then
    EMvcException.RaiseUtf8('%.Create(aApplication=nil)', [self]);
  fApplication := aApplication;
  fApplication.SetSession(nil);
end;

procedure TMvcRun.NotifyContentChangedForMethod(aMethodIndex: integer);
begin
  // do nothing at this abstract level
end;

procedure TMvcRun.NotifyContentChanged;
var
  m: PtrInt;
begin
  for m := 0 to fApplication.fFactory.MethodsCount - 1 do
    NotifyContentChangedForMethod(m)
end;

procedure TMvcRun.NotifyContentChangedForMethod(const aMethodName: RawUtf8);
begin
  NotifyContentChangedForMethod(
    fApplication.fFactory.FindMethodIndex(aMethodName));
end;


{ TMvcRunWithViews }

constructor TMvcRunWithViews.Create(aApplication: TMvcApplication;
  const aTemplatesFolder: TFileName; aViews: TMvcViewsAbstract;
  aPublishOptions: TMvcPublishOptions; aAllowedMethods: TUriMethods);
begin
  inherited Create(aApplication);
  fPublishOptions := aPublishOptions; // all options by default
  if defaultErrorContext in aPublishOptions then
    include(aApplication.fRenderOptions, roDefaultErrorContext);
  fAllowedMethods := aAllowedMethods; // [mGET, mPOST] by default
  if mGET in fAllowedMethods then
    // mHEAD added for proper browsers pre-requests
    fAllowedMethods := fAllowedMethods + [mHEAD];
  // setup the associated mustache views
  if aViews = nil then
    aViews := TMvcViewsMustache.Create(fApplication.fFactory.InterfaceRtti.Info,
      aTemplatesFolder, fApplication.LogClass, '.html');
  fViews := aViews;
  // setup the per-method processing
  SetLength(fMethodCache, fApplication.fFactory.MethodsCount);
  // setup the in-memory static cache
  fStaticCache := TSynDictionary.Create(TypeInfo(TRawUtf8DynArray),
    TypeInfo(TMvcRunCacheStatics), {caseinsens=}false);
end;

destructor TMvcRunWithViews.Destroy;
begin
  inherited Destroy;
  fViews.Free; // the TMvcViewsAbstract was owned by this TMvcRun instance
  fStaticCache.Free;
end;

function TMvcRunWithViews.SetCache(const aMethodName: RawUtf8;
  aPolicy: TMvcRendererCachePolicy;
  aTimeOutSeconds: cardinal): TMvcRunWithViews;
const
  MAX_CACHE_TIMEOUT = 60 * 15; // 900 seconds = 15 minutes
var
  ndx: PtrInt;
  c: ^TMvcRunCacheMethod;
begin
  ndx := fApplication.fFactory.CheckMethodIndex(aMethodName);
  if aTimeOutSeconds - 1 >= MAX_CACHE_TIMEOUT then
    aTimeOutSeconds := MAX_CACHE_TIMEOUT; // c^.TimeOutSeconds is 16-bit
  c := @fMethodCache[ndx];
  c^.Safe.Lock;
  c^.Policy := aPolicy;
  c^.TimeOutSeconds := aTimeOutSeconds;
  c^.Safe.UnLock;
  NotifyContentChangedForMethod(ndx);
  result := self;
end;

procedure TMvcRunWithViews.NotifyContentChanged;
begin
  inherited NotifyContentChanged; // call all NotifyContentChangedForMethod()
  if Assigned(fViews) then
    fViews.NotifyContentChanged; // TMvcViewsMustache will reload all partials
end;

procedure TMvcRunWithViews.NotifyContentChangedForMethod(aMethodIndex: integer);
var
  c: ^TMvcRunCacheMethod;
begin
  if cardinal(aMethodIndex) >= cardinal(Length(fMethodCache)) then
    exit; // paranoid
  c := @fMethodCache[aMethodIndex];
  if c^.Policy = cacheNone then
    exit;
  c^.Safe.Lock;
  case c^.Policy of
    cacheRootIgnoringSession,
    cacheRootIfSession,
    cacheRootIfNoSession:
      c^.RootValue := '';
    cacheRootWithSession,
    cacheWithParametersIgnoringSession,
    cacheWithParametersIfSession,
    cacheWithParametersIfNoSession:
      c^.ParamsValue.Init(false); // reset
  end;
  c^.Safe.UnLock;
end;

procedure TMvcRunWithViews.ComputeStaticCache(var aCache: TMvcRunCacheStatic;
  const aUri, aContentType: RawUtf8);
var
  etag: cardinal;
begin // caller should have set aCache.FileName+Body
  if aContentType <> '' then
    Join([HEADER_CONTENT_TYPE, aContentType], aCache.Header)
  else
    aCache.Header := GetMimeContentTypeHeader(aCache.Body, aCache.FileName);
  etag := crc32cHash(aCache.Body, crc32cHash(aUri));
  BinToHexLower(@etag, SizeOf(etag), aCache.Etag);
end;

procedure TMvcRunWithViews.AddStaticCache(const aFileName: TFileName;
  const aUri: RawUtf8; const aBody: RawByteString; const aContentType: RawUtf8);
var
  cache: TMvcRunCacheStatic;
begin
  if (aFileName = '') or
     (aBody = '') or
     not (cacheStatic in fPublishOptions) then
    exit;
  cache.FileName := aFileName;
  cache.Body := aBody;
  ComputeStaticCache(cache, aUri, aContentType);
  fStaticCache.Add(aUri, cache);
end;

procedure TMvcRunWithViews.ComputeMvcInfoCache;
var
  mvcinfo: variant;
begin
  fApplication.GetMvcInfo(mvcinfo);
  mvcinfo.viewsFolder := fViews.ViewTemplateFolder;
  fMvcInfoCache := TSynMustache.Parse(MUSTACHE_MVCINFO).Render(mvcinfo);
end;

function CompByTag(const Item, Tix32): integer;
begin // called as CompByTag(List[], tix32) to return 0 if List[].Tag >= tix32
  result := ord(cardinal(TSynNameValueItem(Item).Tag) < cardinal(Tix32));
end;

procedure TMvcRunWithViews.PurgeMethodCache(tix32: cardinal);
var
  i: integer;
  c: ^TMvcRunCacheMethod;
begin // called at most once per second
  fOnIdlePurgeCacheTix32 := tix32;
  c := pointer(fMethodCache);
  for i := 1 to length(fMethodCache) do
  begin
    c^.Safe.Lock;
    try
      if (c^.RootValue <> '') and
         (tix32 >= c^.RootValueExpirationTime) then
        c^.RootValue := '';
      if (c^.ParamsValue.Count <> 0) and
         (PDynArray(@c^.ParamsValue.DynArray)^.FindAndDeleteAll(
            tix32, CompByTag, MaxInt) > 0) then
        c^.ParamsValue.DynArray.ForceReHash; // after direct DynArray.Delete()
    finally
      c^.Safe.UnLock;
    end;
    inc(c);
  end;
end;

procedure TMvcRunWithViews.ProcessStatic(const name: RawUtf8;
  var cached: TMvcRunCacheStatic);
begin
  if (cacheStatic in fPublishOptions) and
     fStaticCache.FindAndCopy(name, cached, {updatetimeout=}false) then
    exit; // found in cache
  if not NormalizeUriToFileName(name, cached.FileName) then
    exit; // avoid injection
  fViews.FillStaticFileName(cached);
  if not (cacheStatic in fPublishOptions) then
    exit; // caller will serve back cached.FileName
  fViews.FillStaticFileContent(cached);
  if cached.Body = '' then
    exit; // nothing to cache
  ComputeStaticCache(cached, name, '');
  fStaticCache.Add(name, cached);
end;


{ TMvcRendererReturningData }

procedure TMvcRendererReturningData.Prepare(aRun: TMvcRunWithViews;
  const aIP, aUserAgent: RawUtf8; aTix32: cardinal; aContext: PVariant;
  aHeaders: PUtf8Char; aMethod: PInterfaceMethod);
begin
  fApplication := aRun.Application;
  fRun := aRun;
  fRemoteIP := aIP;
  fRemoteUserAgent := aUserAgent;
  fInputHeaders := aHeaders;
  fInputContext := aContext;
  fTix32 := aTix32;
  fMethod := aMethod;
  if fMethod <> nil then
    fMethodIndex := aMethod^.ExecutionMethodIndex;
  dec(fMethodIndex, RESERVED_VTABLE_SLOTS);
end;

function TMvcRendererReturningData.GetCookieFromHeaders(const CookieName: RawUtf8;
  out Value: PUtf8Char): integer;
begin
  if fInputCookieStart = nil then
  begin
    fInputCookieLen := CookieFromHeaders(
                         fInputHeaders, CookieName, fInputCookieStart);
    if fInputCookieLen = 0 then
      fInputCookieStart := pointer(CookieName); // not void to check once
  end;
  Value := fInputCookieStart;
  result := fInputCookieLen;
end;

procedure TMvcRendererReturningData.SetCookieToHeaders(
  const CookieName, CookieValue: RawUtf8);
begin
  fOutputCookieName  := CookieName;
  fOutputCookieValue := CookieValue;
end;

function TMvcRendererReturningData.ExecuteJsonOverride(var Action: TMvcAction): boolean;
begin
  result := fExecuteJsonOverride; // as set by TMvcApplication.DoRedirect()
  if not result then
    exit;
  Action := PMvcAction(@fOutput)^; // same fields
  fExecuteJsonOverride := false;   // override once
end;

procedure TMvcRendererReturningData.ExecuteCommand;
var
  ctxt: PPointer; // _CurrentRenderer threadvar resolved once

  procedure ExecuteFromCache(const value: RawUtf8);
  begin
    fOutput.Status := HTTP_SUCCESS;
    Split(value, #0, fOutput.Header, RawUtf8(fOutput.Content));
    fOutputFlags := fRun.fViews.RenderFlags(fMethodIndex);
    ctxt^ := nil;
  end;

  function RetrievedFrom(const key: RawUtf8;
    const params: TSynNameValue): boolean;
  var
    v: PSynNameValueItem;
  begin
    v := params.FindItem(key);
    if (v <> nil) and
       (v^.Value <> '') then
      if fTix32 < cardinal(v^.Tag) then
      begin
        ExecuteFromCache(v^.Value);
        result := true;
        exit;
      end
      else
        v^.Value := ''; // remove obsolete data from memory
    fCacheCurrentInputValueKey := key;
    result := false;
  end;

  function RetrievedSessionFrom(aSession: cardinal;
    const params: TSynNameValue): boolean;
  begin
    result := RetrievedFrom(UInt32ToUtf8(aSession), params);
  end;

var
  sessionID, exp32: cardinal;
  c: ^TMvcRunCacheMethod;
  dv: PDocVariantData;
label
  doRoot, doInput;
begin
  // finalize the input JSON needed for ExecuteCommand
  if _SafeObject(fInputContext^, dv) and
     (dv^.Count > 0) then
  begin
    // try ?p.a1=5,p.a2=dfasdfa -> {"p":{"a1":5,"a2":"dfasdfa"}}
    if (fMethod^.ArgsInputValuesCount = 1) and
       (dv^.Count > 1) then
      with fMethod^.Args[fMethod^.ArgsInFirst] do
        dv^.FlattenAsNestedObject(@ParamName^[1], ord(ParamName^[0]));
    // compute JSON data context as expected by inherited ExecuteCommand
    DocVariantType.ToJson(PVarData(dv), fInput);
  end;
  // setup the associated threadvar reference - needed for CurrentSession.Exists
  ctxt := @_CurrentRenderer;
  ctxt^ := self;
  // return any cached content
  c := nil;
  if (cardinal(fMethodIndex) < cardinal(Length(fRun.fMethodCache))) and
     not fCacheDisabled then
  begin
    if fTix32 = 0 then
      fTix32 := GetTickSec;
    // first purge from any deprecated cached content (at most once per second)
    if fRun.fOnIdlePurgeCacheTix32 <> fTix32 then
      fRun.PurgeMethodCache(fTix32);
    // retrieve the caching context for this method (if enabled)
    c := @fRun.fMethodCache[fMethodIndex];
    if c^.Policy = cacheNone then
      c := nil;
  end;
  if c <> nil then
  begin
    FastAssignNew(fCacheCurrentInputValueKey);
    c^.Safe.Lock;
    try
      case c^.Policy of
        cacheRootIgnoringSession:
          if fInput = '' then
doRoot:     if c^.RootValue <> '' then
              if fTix32 < c^.RootValueExpirationTime then
              begin
                ExecuteFromCache(c^.RootValue);
                exit;
              end
              else
                c^.RootValue := ''; // free deprecated content from memory
        cacheRootIfSession:
          if (fInput = '') and
             fApplication.CurrentSession.Exists then
            goto doRoot;
        cacheRootIfNoSession:
          if (fInput = '') and
             not fApplication.CurrentSession.Exists then
            goto doRoot;
        cacheRootWithSession:
          if fInput = '' then
          begin
            sessionID := fApplication.CurrentSession.CheckAndRetrieve;
            if sessionID = 0 then
              goto doRoot
            else if RetrievedSessionFrom(sessionID, c^.ParamsValue) then
              exit;
          end;
        cacheWithParametersIgnoringSession:
doInput:  if fInput = '' then
            goto doRoot
          else if RetrievedFrom(fInput, c^.ParamsValue) then
            exit;
        cacheWithParametersIfSession:
          if fApplication.CurrentSession.Exists then
            goto doInput;
        cacheWithParametersIfNoSession:
          if not fApplication.CurrentSession.Exists then
            goto doInput;
      end;
    finally
      c^.Safe.UnLock;
    end;
  end;
  // compute the context and render the page using the corresponding View
  inherited ExecuteCommand;
  ctxt^ := nil;
  // update cache for this method
  if c = nil then
    exit;
  c^.Safe.Lock;
  try
    exp32 := fTix32 + c^.TimeOutSeconds;
    if fCacheCurrentInputValueKey = '' then
      if fOutput.Status = HTTP_SUCCESS then
      begin
        Join([fOutput.Header, #0, fOutput.Content], c^.RootValue);
        c^.RootValueExpirationTime := exp32;
      end
      else
        c^.RootValue := ''
    else
      if fOutput.Status = HTTP_SUCCESS then
        c^.ParamsValue.AddJoined(fCacheCurrentInputValueKey,
          [fOutput.Header, #0, fOutput.Content], exp32)
      else
        c^.ParamsValue.Add(fCacheCurrentInputValueKey, ''); // cache the error
  finally
    c^.Safe.UnLock;
  end;
end;

function TMvcRendererReturningData.Redirects(const action: TMvcAction): boolean;
begin
  Make(['Location: ', UrlEncodeJsonObject(action.RedirectToMethodName,
    action.RedirectToMethodParameters, ['main'])], fOutput.Header);
  fOutput.Status := action.ReturnedStatus;
  result := true;
end;



{ ************ Application ViewModel/Controller using Interfaces }

{ EMvcApplication }

constructor EMvcApplication.CreateDefault(aStatus: cardinal);
begin
  inherited CreateFmt('CreateDefault(%d)', [aStatus]);
  TMvcApplication.GotoDefault(fAction, aStatus);
end;

constructor EMvcApplication.CreateGotoError(const aErrorMessage: string;
  aErrorCode: integer);
begin
  inherited CreateFmt('Error #%d: %s', [aErrorCode, aErrorMessage]);
  TMvcApplication.GotoError(fAction, aErrorMessage, aErrorCode);
end;

constructor EMvcApplication.CreateGotoError(aHtmlErrorCode: integer);
begin
  inherited CreateFmt('Error=%d', [aHtmlErrorCode]);
  TMvcApplication.GotoError(fAction, aHtmlErrorCode);
end;

constructor EMvcApplication.CreateGotoView(const aMethod: RawUtf8;
  const aParametersNameValuePairs: array of const; aStatus: cardinal);
begin
  inherited CreateFmt('GotoView(''%s'',%d)', [aMethod, aStatus]);
  TMvcApplication.GotoView(fAction, aMethod, aParametersNameValuePairs, aStatus);
end;

class procedure EMvcApplication.GotoView(const aMethod: RawUtf8;
  const aParametersNameValuePairs: array of const; aStatus: cardinal);
begin
  raise CreateGotoView(aMethod, aParametersNameValuePairs, aStatus)
  {$ifdef FPC} at get_caller_addr(get_frame), get_caller_frame(get_frame) {$endif}
end;

class procedure EMvcApplication.GotoError(const aErrorMessage: string;
  aErrorCode: integer);
begin
  raise CreateGotoError(aErrorMessage, aErrorCode)
  {$ifdef FPC} at get_caller_addr(get_frame), get_caller_frame(get_frame) {$endif}
end;

class procedure EMvcApplication.GotoError(aHtmlErrorCode: integer);
begin
  raise CreateGotoError(aHtmlErrorCode)
  {$ifdef FPC} at get_caller_addr(get_frame), get_caller_frame(get_frame) {$endif}
end;

class procedure EMvcApplication.Default(aStatus: cardinal);
begin
  raise CreateDefault(aStatus)
  {$ifdef FPC} at get_caller_addr(get_frame), get_caller_frame(get_frame) {$endif}
end;


{ TMvcApplication }

procedure TMvcApplication.SetInterface(aInterface: PRttiInfo);
var
  n: integer;
  met: PInterfaceMethod;
  entry: PInterfaceEntry;
  uri: PRawUtf8;
begin
  fLocker := TAutoLocker.Create;
  // setup this interface and its methods
  if fFactory <> nil then
    EMvcException.RaiseUtf8('%.SetInterface(%) twice after %',
      [aInterface.RawName, fFactory.InterfaceName]);
  fFactory := TInterfaceFactory.Get(aInterface);
  fFactoryErrorIndex := fFactory.FindMethodIndex('Error');
  if fFactoryErrorIndex < 0 then
    EMvcException.RaiseUtf8('% does not implement the ' +
      'IMvcApplication.Error() method', [aInterface.RawName]);
  entry := GetInterfaceEntry(fFactory.InterfaceGuid^);
  if entry = nil then
    EMvcException.RaiseUtf8('%.Start: this class should implement %',
      [self, aInterface.RawName]);
  fFactoryEntry := PAnsiChar(self) + entry^.IOffset;
  n := fFactory.MethodsCount;
  SetLength(fMethodUri, n);
  met := pointer(fFactory.Methods);
  uri := pointer(fMethodUri);
  repeat
    if not MethodHasView(met^) then
      if met^.ArgsOutFirst <> met^.ArgsResultIndex then
        EMvcException.RaiseUtf8(
          '%.Start: I% var/out param not allowed with TMvcAction result',
          [self, met^.InterfaceDotMethodName])
      else
        // maps TMvcAction in TMvcApplication.RunOnRestServer
        include(met^.Flags, imfResultIsServiceCustomAnswer);
    uri^ := met^.Uri;
    if (uri^[1] = '_') and
       (uri^[2] <> '_') then
      // e.g. IService._Start() -> /service/start
      delete(uri^, 1, 1);
    inc(uri);
    inc(met);
    dec(n);
  until n = 0;
  FlushAnyCache;
  // (re)prepare some reusable execution context (avoid most memory allocations)
  ObjArrayClear(fExecuteCached);
  TInterfaceMethodExecuteCached.Prepare(fFactory, fExecuteCached);
end;

destructor TMvcApplication.Destroy;
begin
  ObjArrayClear(fExecuteCached); // should be done beore inherited
  inherited Destroy;
  fMainRunner.Free;
  fSession.Free;
end;

procedure TMvcApplication.Error(var Msg: RawUtf8; var Scope: variant);
begin
  // do nothing: just pass input error Msg and data Scope to the view
end;

class procedure TMvcApplication.GotoView(var Action: TMvcAction;
  const MethodName: RawUtf8; const ParametersNameValuePairs: array of const;
  status: cardinal);
begin
  Action.ReturnedStatus := status;
  Action.RedirectToMethodName := MethodName;
  if high(ParametersNameValuePairs) < 1 then
    Action.RedirectToMethodParameters := ''
  else
    Action.RedirectToMethodParameters := JsonEncode(ParametersNameValuePairs);
end;

class procedure TMvcApplication.GotoError(var Action: TMvcAction;
  const Msg: string; ErrorCode: integer);
begin
  GotoView(Action, 'Error', ['Msg', Msg], ErrorCode);
end;

class procedure TMvcApplication.GotoError(var Action: TMvcAction;
  ErrorCode: integer);
begin
  if ErrorCode <= HTTP_CONTINUE then
    ErrorCode := HTTP_BADREQUEST;
  GotoView(Action, 'Error', ['Msg', StatusCodeToShort(ErrorCode)], ErrorCode);
end;

class procedure TMvcApplication.GotoDefault(var Action: TMvcAction;
  Status: cardinal);
begin
  Action.ReturnedStatus := Status;
  Action.RedirectToMethodName := 'Default';
  Action.RedirectToMethodParameters := '';
end;

procedure TMvcApplication.RedirectError(ErrorCode: integer);
var
  action: TMvcAction;
begin
  GotoError(action, ErrorCode);
  DoRedirect(action);
end;

procedure TMvcApplication.RedirectError(const Msg: string; ErrorCode: integer);
var
  action: TMvcAction;
begin
  GotoError(action, Msg, ErrorCode);
  DoRedirect(action);
end;

procedure TMvcApplication.RedirectDefault(Status: cardinal);
var
  action: TMvcAction;
begin
  GotoDefault(action, Status);
  DoRedirect(action);
end;

procedure TMvcApplication.RedirectView(const MethodName: RawUtf8;
  const ParametersNameValuePairs: array of const; Status: cardinal);
var
  action: TMvcAction;
begin
  GotoView(action, MethodName, ParametersNameValuePairs, Status);
  DoRedirect(action);
end;

procedure TMvcApplication.DoRedirect(var action: TMvcAction);
var
  ctxt: TMvcRendererReturningData;
begin
  // retrieve TMvcRendererReturningData.ExecuteCommand context
  ctxt := _CurrentRenderer;
  if ctxt = nil then
    EMvcException.RaiseUtf8('Unexpected %.Redirect(%) outside a method',
      [self, action.RedirectToMethodName]);
  if ctxt.fExecuteJsonOverride then
    EMvcException.RaiseUtf8('Unexpected %.Redirect(%) twice (existing=%)',
      [self, action.RedirectToMethodName,
       PMvcAction(@ctxt.fOutput)^.RedirectToMethodName]);
  // set context as expected by TMvcRendererReturningData.ExecuteJsonOverride
  ctxt.fExecuteJsonOverride := true;
  ctxt.fOutput := PServiceCustomAnswer(@action)^;
end;

procedure TMvcApplication.SetSession(Value: TMvcSessionAbstract);
begin
  FreeAndNilSafe(fSession);
  fSession := Value;
end;

procedure TMvcApplication.GetViewInfo(MethodIndex: integer; out info: variant);
begin
  TDocVariantData(info).InitFast(8, dvObject);
  if MethodIndex >= 0 then
    TDocVariantData(info).AddValueText(
      'pageName', fFactory.Methods[MethodIndex].Uri);
end;

procedure TMvcApplication.GetMvcInfo(out info: variant);
begin
  info := _ObjFast(['name',    fFactory.InterfaceRtti.Name,
                    'mORMot',  RawUtf8(SYNOPSE_FRAMEWORK_VERSION),
                    'methods', ContextFromMethods(fFactory)]);
end;

procedure TMvcApplication.FlushAnyCache;
begin
  if fMainRunner <> nil then
    fMainRunner.NotifyContentChanged;
end;




initialization
  assert(SizeOf(TMvcAction) = SizeOf(TServiceCustomAnswer));
  TSynLog.Family.ExceptionIgnore.Add(EMvcApplication); // redirection, not error

end.
