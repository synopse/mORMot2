/// MVC Web Server over mORMot's REST and Mustache
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.rest.mvc;

{
  *****************************************************************************

   Web Server using Model-View-Controller (MVC) pattern and Mustache
    - Web Views Implementation using Mustache
    - ViewModel/Controller Sessions using Cookies
    - Web Renderer Returning Mustache Views or Json
    - Application ViewModel/Controller using Interfaces

   TODO: remove TMvcRunOnRestServer in favor of THttpServer.Router

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  variants,
  contnrs,
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
  mormot.core.mustache,
  mormot.soa.codegen, // for mvcinfo or default template generation
  mormot.orm.base,
  mormot.orm.core,
  mormot.rest.core,
  mormot.rest.server;


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
    Safe: TOSLightLock;
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
    /// define Expression Helpers for some ORM tables
    // - e.g. to read a TMyOrm from its ID value and put its fields
    // in the current rendering data context, you can write:
    // ! aView.RegisterExpressionHelpersForTables(aServer,[TMyOrm]);
    // then use the following Mustache tag
    // ! {{#TMyOrm MyRecordID}} ... {{/TMyOrm MyRecordID}}
    // - use Bootstap CSS by default, but you can supply your aHtmlTableStyle
    // - returns self so that may be called in a fluent interface
    function RegisterExpressionHelpersForTables(aRest: TRest;
      const aTables: array of TOrmClass;
      aHtmlTableStyle: TExpressionHtmlTableStyleClass = nil): TMvcViewsMustache; overload;
    /// define Expression Helpers for all ORM tables of the supplied model
    // - e.g. to read a TMyOrm from its ID value and put its fields
    // in the current rendering data context, you can write:
    // ! aView.RegisterExpressionHelpersForTables(aServer);
    // then use the following Mustache tag
    // ! {{#TMyOrm MyRecordID}} ... {{/TMyOrm MyRecordID}}
    // - returns self so that may be called in a fluent interface
    function RegisterExpressionHelpersForTables(aRest: TRest;
      aHtmlTableStyle: TExpressionHtmlTableStyleClass = nil): TMvcViewsMustache; overload;
    /// define some Expression Helpers for hashing
    // - i.e. md5, sha1 sha256 and sha512 hashing
    // - would allow e.g. to compute a Gravatar URI via:
    // ! <img src=http://www.gravatar.com/avatar/{{md5 email}}?s=200></img>
    // - returns self so that may be called in a fluent interface
    function RegisterExpressionHelpersForCrypto: TMvcViewsMustache;
  end;



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
    // - default is 'mORMot', and cookie is restricted to Path=/RestRoot
    property CookieName: RawUtf8
      read GetCookieName write SetCookieName;
  end;

  /// implement a ViewModel/Controller sessions using TMvcRendererReturningData
  // - will use threadvar to access TMvcRendererReturningData.Headers
  TMvcSessionWithRenderer = class(TMvcSessionWithCookies)
  protected
    function GetCookie(out Value: PUtf8Char): integer; override;
    procedure SetCookie(const Value: RawUtf8); override;
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


{ ************ Web Renderer Returning Mustache Views or Json }

  /// record type to define commands e.g. to redirect to another URI
  // - do NOT access those record property directly, but rather use
  // TMvcApplication.GotoView/GotoError/GotoDefault methods, e.g.
  // !  function TBlogApplication.Logout: TMvcAction;
  // !  begin
  // !    CurrentSession.Finalize;
  // !    GotoDefault(result);
  // !  end;
  // - this record type should match exactly TServiceCustomAnswer layout,
  // so that TServiceMethod.InternalExecute() would handle it directly
  TMvcAction = record
    /// the method name to be executed
    RedirectToMethodName: RawUtf8;
    /// may contain a JSON object which will be used to specify parameters
    // to the specified method
    RedirectToMethodParameters: RawUtf8;
    /// which HTTP Status code should be returned
    // - if RedirectMethodName is set, will return 307 HTTP_TEMPORARYREDIRECT
    // by default, but you can set here the expected HTTP Status code, e.g.
    // 201 HTTP_CREATED or 404 HTTP_NOTFOUND
    ReturnedStatus: cardinal;
  end;

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
    fCacheDisabled: boolean;
    fInputCookieLen: integer;
    fInputHeaders: PUtf8Char;
    fInputContext: PVariant;
    fCacheCurrentInputValueKey: RawUtf8;
    fOutputCookieName, fOutputCookieValue: RawUtf8;
    function Redirects(const action: TMvcAction): boolean; override;
    function GetCookie(const CookieName: RawUtf8;
      out Value: PUtf8Char): integer; virtual;
    procedure SetCookie(const CookieName, CookieValue: RawUtf8); virtual;
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
  // for every TOrm table of the Server data model
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
    fPublishOptions: TMvcPublishOptions;
    fAllowedMethods: TUriMethods;
    procedure ComputeMvcInfoCache;
    procedure ComputeStaticCache(var aCache: TMvcRunCacheStatic;
      const aUri, aContentType: RawUtf8);
    // return cache.FileName or cache.Body+ContentHeader+Etag
    procedure ProcessStatic(const name: RawUtf8; var cached: TMvcRunCacheStatic);
  public
    /// link this runner class to a specified MVC application
    constructor Create(aApplication: TMvcApplication;
      aViews: TMvcViewsAbstract = nil); reintroduce;
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

  /// run TMvcApplication directly within a TRestServer method-based service
  // - this is the easiest way to host and publish a MVC Application, optionally
  // in conjunction with REST/AJAX client access
  TMvcRunOnRestServer = class(TMvcRunWithViews)
  protected
    fRestServer: TRestServer;
    /// callback used for the rendering on the TRestServer
    procedure RunOnRestServerRoot(Ctxt: TRestServerUriContext);
    procedure RunOnRestServerSub(Ctxt: TRestServerUriContext);
    procedure InternalRunOnRestServer(Ctxt: TRestServerUriContext;
      const MethodName: RawUtf8);
  public
    /// this constructor will publish some views to a TRestServer instance
    // - the associated RestModel can match the supplied TRestServer, or be
    // another instance (if the data model is not part of the publishing server)
    // - all TMvcApplication methods would be registered to the TRestServer,
    // as /root/methodName if aSubURI is '', or as /root/aSubURI/methodName
    // - if aApplication has no Views instance associated, this constructor will
    // initialize a Mustache renderer in its default folder, with '.html' void
    // template generation
    // - will also create a TMvcSessionWithRenderer for simple cookie sessions
    // - aPublishOptions could be used to specify integration with the server
    // - aAllowedMethods will render standard GET/POST by default
    constructor Create(aApplication: TMvcApplication;
      const aTemplatesFolder: TFileName = ''; aRestServer: TRestServer = nil;
      const aSubURI: RawUtf8 = ''; aViews: TMvcViewsAbstract = nil;
      aPublishOptions: TMvcPublishOptions=
        [low(TMvcPublishOption) .. high(TMvcPublishOption)];
      aAllowedMethods: TUriMethods = [mGET, mPOST]); reintroduce;
  end;



{ ************ Application ViewModel/Controller using Interfaces }

    /// Exception class triggerred by mORMot MVC/MVVM applications internally
  // - those error are internal fatal errors of the server side process
  EMvcException = class(ESynException);

  /// Exception class triggerred by mORMot MVC/MVVM applications externally
  // - those error are external errors which should be notified to the client
  // - can be used to change the default view, e.g. on application error
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
    class procedure GotoView(const aMethod: RawUtf8;
      const aParametersNameValuePairs: array of const;
      aStatus: cardinal = HTTP_TEMPORARYREDIRECT);
    /// just a wrapper around raise CreateGotoError()
    class procedure GotoError(const aErrorMessage: string;
      aErrorCode: integer = HTTP_BADREQUEST); overload;
    /// just a wrapper around raise CreateGotoError()
    class procedure GotoError(aHtmlErrorCode: integer); overload;
    /// just a wrapper around raise CreateDefault()
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

  /// parent class to implement a MVC/MVVM application
  // - you should inherit from this class, then implement an interface inheriting
  // from IMvcApplication to define the various commands of the application
  // - here the Model would be a TRest instance, Views will be defined by
  // TMvcViewsAbstract (e.g. TMvcViewsMustache), and the ViewModel/Controller
  // will be implemented with IMvcApplication methods of the inherited class
  // - inherits from TInjectableObject, so that you could resolve dependencies
  // via services or stubs, following the IoC pattern
  TMvcApplication = class(TInjectableObject)
  protected
    fFactory: TInterfaceFactory;
    fFactoryEntry: pointer;
    fFactoryErrorIndex: integer;
    fRenderOptions: set of (roDefaultErrorContext);
    fSession: TMvcSessionAbstract;
    fRestModel: TRest;
    fRestServer: TRestServer;
    fLocker: IAutoLocker;
    // if any TMvcRun instance is store here, will be freed by Destroy
    // but note that a single TMvcApplication logic may handle several TMvcRun
    fMainRunner: TMvcRun;
    fOnBeforeRender, fOnAfterRender: TOnMvcRender;
    fOnSessionCreate, fOnSessionFinalized: TOnMvcSession;
    fExecuteCached: TInterfaceMethodExecuteCachedDynArray;
    procedure SetSession(Value: TMvcSessionAbstract);
    /// to be called when the data model did change to force content re-creation
    // - this default implementation will call fMainRunner.NotifyContentChanged
    procedure FlushAnyCache; virtual;
    /// generic IMvcApplication.Error method implementation
    procedure Error(var Msg: RawUtf8; var Scope: variant); virtual;
    /// every view will have this data context transmitted as "main":...
    procedure GetViewInfo(MethodIndex: integer; out info: variant); virtual;
    /// compute the data context e.g. for the /mvc-info URI
    procedure GetMvcInfo(out info: variant); virtual;
    /// wrappers to redirect to IMvcApplication standard methods
    // - if status is HTTP_TEMPORARYREDIRECT, it will change the URI
    // whereas HTTP_SUCCESS would just render the view for the current URI
    class procedure GotoView(var Action: TMvcAction; const MethodName: RawUtf8;
      const ParametersNameValuePairs: array of const;
      Status: cardinal = HTTP_TEMPORARYREDIRECT);
    class procedure GotoError(var Action: TMvcAction; const Msg: string;
      ErrorCode: integer = HTTP_BADREQUEST); overload;
    class procedure GotoError(var Action: TMvcAction;
      ErrorCode: integer); overload;
    class procedure GotoDefault(var Action: TMvcAction;
      Status: cardinal = HTTP_TEMPORARYREDIRECT);
  public
    /// initialize the instance of the MVC/MVVM application
    // - define the associated REST instance, and the interface definition for
    // application commands
    // - is not defined as constructor, since this TInjectableObject may
    // expect injection using the CreateInjected() constructor
    procedure Start(aRestModel: TRest; aInterface: PRttiInfo); virtual;
    /// finalize the application
    // - and release any associated CurrentSession, Views, and fMainRunner
    destructor Destroy; override;

    /// read-only access to the associated mORMot REST instance implementing the
    // MVC data Model of the application
    // - is a TRestServer instance e.g. for TMvcRunOnRestServer
    property RestModel: TRest
      read fRestModel;
    /// read-only access to the associated factory for IMvcApplication interface
    property Factory: TInterfaceFactory
      read fFactory;
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
  /// the pseudo-method name for the MVC information html page
  MVCINFO_URI: RawUtf8 = 'mvc-info';

  /// the pseudo-method name for any static content for Views
  STATIC_URI: RawUtf8 = '.static';



implementation

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
  '{{<url}}/{{root}}/{{methodName}}{{#hasInParams}}?' +
  '{{#args}}{{#dirInput}}{{lowerName}}=</b>..[{{typePascal}}]..<b>' +
  '{{#commaInSingle}}&{{/commaInSingle}}{{/dirInput}}{{/args}}{{/hasInParams}}{{/url}}' +
  '{{<mustache}}<b>&#123;{Main&#125;}</b>: variant{{#args}}{{#dirOutput}}<br><b>{&#123;' +
  '{{argName}}&#125;}</b>: {{typePascal}}{{/dirOutput}}{{/args}}{{/mustache}}' +
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


{ ************ Web Views Implementation using Mustache }

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

type
  TExpressionHelperForTable = class
  public
    Rest: TRest;
    Table: TOrmClass;
    TableProps: TOrmProperties;
    HtmlTableStyle: TExpressionHtmlTableStyleClass;
    constructor Create(aRest: TRest; aTable: TOrmClass;
      var aHelpers: TSynMustacheHelpers;
      aHtmlTableStyle: TExpressionHtmlTableStyleClass);
    procedure ExpressionGet(const Value: variant; out Result: variant);
    procedure ExpressionHtmlTable(const Value: variant; out Result: variant);
  end;


{ TExpressionHelperForTable }

constructor TExpressionHelperForTable.Create(aRest: TRest; aTable: TOrmClass;
  var aHelpers: TSynMustacheHelpers;
  aHtmlTableStyle: TExpressionHtmlTableStyleClass);
var
  HelperName: RawUtf8;
begin
  aRest.PrivateGarbageCollector.Add(self);
  Rest := aRest;
  HelperName := RawUtf8(aTable.ClassName);
  Table := aTable;
  TableProps := aTable.OrmProps;
  TSynMustache.HelperAdd(aHelpers, HelperName, ExpressionGet);
  if aHtmlTableStyle = nil then
    aHtmlTableStyle := TExpressionHtmlTableStyleBootstrap;
  HtmlTableStyle := aHtmlTableStyle;
  TSynMustache.HelperAdd(aHelpers,
    HelperName + '.HtmlTable', ExpressionHtmlTable);
end;

procedure TExpressionHelperForTable.ExpressionHtmlTable(const Value: variant;
  out Result: variant);
var
  Rec: PDocVariantData;
  f, i, j: PtrInt;
  int: integer;
  Field: TOrmPropInfo;
  timelog: TTimeLogBits;
  caption: string;
  sets: TStringList;
  u: RawUtf8;
  W: TTextWriter;
  tmp: TTextWriterStackBuffer; // 8KB work buffer on stack
const
  ONOFF: array[boolean] of THtmlTableStyleLabel = (
    labelOff, labelOn);
  ENUM: array[boolean, boolean] of THtmlTableStyleLabel = (
   (labelValue, labelValue),
   (labelFalse, labelTrue));
begin
  if _SafeObject(Value, Rec) then
  begin
    W := TTextWriter.CreateOwnedStream(tmp);
    try
      HtmlTableStyle.StartTable(W);
      for f := 0 to TableProps.Fields.Count - 1 do
      begin
        Field := TableProps.Fields.List[f];
        i := Rec^.GetValueIndex(Field.Name);
        if i < 0 then
          continue;
        if not (Field.OrmFieldType in
            [oftAnsiText,
             oftUtf8Text,
             oftInteger,
             oftFloat,
             oftCurrency,
             oftTimeLog,
             oftModTime,
             oftCreateTime,
             oftDateTime,
             oftDateTimeMS,
             oftUnixTime,
             oftUnixMSTime,
             oftBoolean,
             oftEnumerate,
             oftSet]) then
          // we support only most obvious types in 'case OrmFieldType of" below
          continue;
        HtmlTableStyle.BeforeFieldName(W);
        GetCaptionFromPCharLen(TrimLeftLowerCase(Field.Name), caption);
        W.AddHtmlEscapeString(caption);
        HtmlTableStyle.BeforeValue(W);
        VariantToUtf8(Rec^.Values[i], u);
        case Field.OrmFieldType of
          oftAnsiText,
          oftUtf8Text,
          oftInteger,
          oftFloat,
          oftCurrency:
            W.AddHtmlEscape(pointer(u));
          oftTimeLog,
          oftModTime,
          oftCreateTime:
            if VariantToInt64(Rec^.Values[i], timelog.Value) then
              W.AddHtmlEscapeString(timelog.i18nText);
          oftDateTime,
          oftDateTimeMS:
            begin
              timelog.From(u);
              W.AddHtmlEscapeString(timelog.i18nText);
            end;
          oftUnixTime,
          oftUnixMSTime:
            if VariantToInt64(Rec^.Values[i], timelog.Value) then
            begin
              if Field.OrmFieldType = oftUnixTime then
                timelog.FromUnixTime(timelog.Value)
              else
                timelog.FromUnixMSTime(timelog.Value);
              W.AddHtmlEscapeString(timelog.i18nText);
            end;
          oftBoolean,
          oftEnumerate:
            if Field.InheritsFrom(TOrmPropInfoRttiEnum) then
            begin
              caption := TOrmPropInfoRttiEnum(Field).GetCaption(u, int);
              HtmlTableStyle.AddLabel(W, caption,
                ENUM[Field.OrmFieldType = oftBoolean, int <> 0]);
            end;
          oftSet:
            if Field.InheritsFrom(TOrmPropInfoRttiSet) and
               VariantToInteger(Rec^.Values[i], int) then
            begin
              sets := TStringList.Create;
              try
                TOrmPropInfoRttiSet(Field).SetEnumType^.AddCaptionStrings(sets);
                for j := 0 to sets.Count - 1 do
                begin
                  HtmlTableStyle.AddLabel(W, sets[j], ONOFF[GetBit(int, j)]);
                  W.AddShorter('<br/>');
                end;
              finally
                sets.Free;
              end;
            end;
        end;
        HtmlTableStyle.AfterValue(W);
      end;
      HtmlTableStyle.EndTable(W);
      RawUtf8ToVariant(W.Text, Result);
    finally
      W.Free;
    end;
  end
  else
    // not an object -> return input value as is
    Result := Value;
end;

procedure TExpressionHelperForTable.ExpressionGet(const Value: variant;
  out Result: variant);
var
  Rec: TOrm;
  ID: integer;
begin
  SetVariantNull(Result);
  if not VariantToInteger(Value, ID) then
    exit;
  Rec := Table.Create;
  try
    if Rest.ORM.Retrieve(ID, Rec) then
      Result := Rec.GetSimpleFieldsAsDocVariant(true);
  finally
    Rec.Free;
  end;
end;


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

function MethodHasView(const aMethod: TInterfaceMethod): boolean;
begin
  // any method returning a TMvcAction does not have an associated view
  result := (aMethod.ArgsResultIndex < 0) or
    (aMethod.Args[aMethod.ArgsResultIndex].ArgRtti.Info <> TypeInfo(TMvcAction));
end;

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
    v^.Safe.Init;
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
    fViews[i].Safe.Done;
end;

function TMvcViewsMustache.RegisterExpressionHelpers(
  const aNames: array of RawUtf8;
  const aEvents: array of TSynMustacheHelperEvent): TMvcViewsMustache;
begin
  if self <> nil then
    TSynMustache.HelperAdd(fViewHelpers, aNames, aEvents);
  result := self;
end;

function TMvcViewsMustache.RegisterExpressionHelpersForTables(
  aRest: TRest; const aTables: array of TOrmClass;
  aHtmlTableStyle: TExpressionHtmlTableStyleClass): TMvcViewsMustache;
var
  t: PtrInt;
begin
  if (self <> nil) and
     (aRest <> nil) then
    for t := 0 to high(aTables) do
      if aRest.Model.GetTableIndex(aTables[t]) >= 0 then
        TExpressionHelperForTable.Create(
          aRest, aTables[t], fViewHelpers, aHtmlTableStyle);
  result := self;
end;

function TMvcViewsMustache.RegisterExpressionHelpersForTables(aRest: TRest;
  aHtmlTableStyle: TExpressionHtmlTableStyleClass): TMvcViewsMustache;
var
  t: PtrInt;
begin
  if (self <> nil) and
     (aRest <> nil) then
    for t := 0 to aRest.Model.TablesMax do
      TExpressionHelperForTable.Create(
        aRest, aRest.Model.Tables[t], fViewHelpers, aHtmlTableStyle);
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
    {capacity=}rc.PropsCount + 1);
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


{ TMvcSessionWithRenderer }

threadvar
  _CurrentRenderer: TMvcRendererAbstract;

function CurrentRenderer: TMvcRendererReturningData;
  {$ifdef HASINLINE} inline; {$endif}
begin
  result := _CurrentRenderer as TMvcRendererReturningData;
end;

function TMvcSessionWithRenderer.GetCookie(out Value: PUtf8Char): integer;
begin
  result := CurrentRenderer.GetCookie(fContext.CookieName, Value);
end;

procedure TMvcSessionWithRenderer.SetCookie(const Value: RawUtf8);
begin
  CurrentRenderer.SetCookie(fContext.CookieName, Value);
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
          if isAction then
          begin
            // was a TMvcAction mapped in a TServiceCustomAnswer record
            action.RedirectToMethodName := exec.ServiceCustomAnswerHead;
            action.ReturnedStatus := exec.ServiceCustomAnswerStatus;
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
  aViews: TMvcViewsAbstract);
begin
  inherited Create(aApplication);
  fViews := aViews;
  SetLength(fMethodCache, fApplication.fFactory.MethodsCount);
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


{ TMvcRunOnRestServer }

constructor TMvcRunOnRestServer.Create(aApplication: TMvcApplication;
  const aTemplatesFolder: TFileName; aRestServer: TRestServer;
  const aSubURI: RawUtf8; aViews: TMvcViewsAbstract;
  aPublishOptions: TMvcPublishOptions; aAllowedMethods: TUriMethods);
var
  m: PtrInt;
  bypass: boolean;
  method: RawUtf8;
begin
  if aApplication = nil then
    EMvcException.RaiseUtf8('%.Create(aApplication=nil)', [self]);
  if defaultErrorContext in aPublishOptions then
    include(aApplication.fRenderOptions, roDefaultErrorContext);
  if aRestServer = nil then
    fRestServer := aApplication.RestModel as TRestServer
  else
    fRestServer := aRestServer;
  if aViews = nil then
    aViews := TMvcViewsMustache.Create(aApplication.fFactory.InterfaceRtti.Info,
      aTemplatesFolder, fRestServer.LogClass, '.html')
  else
    aViews.fLogClass := fRestServer.LogClass;
  inherited Create(aApplication, aViews);
  fPublishOptions := aPublishOptions; // all options by default
  fAllowedMethods := aAllowedMethods; // [mGET, mPOST] by default
  if mGET in fAllowedMethods then
    // mHEAD added for proper browsers pre-requests
    fAllowedMethods := fAllowedMethods + [mHEAD];
  bypass := bypassAuthentication in fPublishOptions;
  if aSubURI <> '' then
    fRestServer.ServiceMethodRegister(
      aSubURI, RunOnRestServerSub, bypass, fAllowedMethods)
  else
  begin
    for m := 0 to fApplication.fFactory.MethodsCount - 1 do
    begin
      method := fApplication.fFactory.Methods[m].Uri;
      if (method[1] = '_') and
         (method[2] <> '_') then
        // e.g. IService._Start() -> /service/start
        delete(method, 1, 1);
      fRestServer.ServiceMethodRegister(
        method, RunOnRestServerRoot, bypass, fAllowedMethods);
    end;
    if publishMvcInfo in fPublishOptions then
      fRestServer.ServiceMethodRegister(
        MVCINFO_URI, RunOnRestServerRoot, bypass, fAllowedMethods);
    if publishStatic in fPublishOptions then
      fRestServer.ServiceMethodRegister(
        STATIC_URI, RunOnRestServerRoot, bypass, fAllowedMethods);
  end;
  if (registerOrmTableAsExpressions in fPublishOptions) and
     aViews.InheritsFrom(TMvcViewsMustache) then
    TMvcViewsMustache(aViews).RegisterExpressionHelpersForTables(fRestServer);
  fApplication.SetSession(TMvcSessionWithRenderer.Create(aApplication));
  // no remote ORM access via REST, and route method_name as method/name too
  fRestServer.Options := fRestServer.Options +
    [rsoNoTableURI, rsoNoInternalState, rsoMethodUnderscoreAsSlashUri];
end;

procedure TMvcRunOnRestServer.InternalRunOnRestServer(
  Ctxt: TRestServerUriContext; const MethodName: RawUtf8);
var
  p: PUtf8Char;
  inputContext: variant;
  mainMethod, subMethod, body: RawUtf8;
  cache: TMvcRunCacheStatic;
  meth: PInterfaceMethod;
  r: TMvcRendererReturningData;
  start: Int64;
begin
  // 1. parse URI
  p := pointer(MethodName);
  if GetNextItemMultiple(p, '/?', mainMethod) = '/' then
    GetNextItem(p, '?', subMethod);
  // 2. implement mvc-info endpoint
  if (publishMvcInfo in fPublishOptions) and
     PropNameEquals(mainMethod, MVCINFO_URI) then
  begin
    if fMvcInfoCache = '' then
      ComputeMvcInfoCache;
    Ctxt.Returns(fMvcInfoCache, HTTP_SUCCESS, HTML_CONTENT_TYPE_HEADER, true);
    exit;
  end;
  // 3. serve static resources, with proper caching
  if (publishStatic in fPublishOptions) and
     PropNameEquals(mainMethod, STATIC_URI) then
  begin
    // this method uses a local in-memory cache, but would do the same as:
    // Ctxt.ReturnFileFromFolder(fViews.ViewStaticFolder);
    ProcessStatic(subMethod, cache);
    if cache.FileName <> '' then
      Ctxt.ReturnFile(cache.FileName, {304=}true, '', '', '', fStaticCacheControlMaxAge)
    else if cache.Body = '' then
      Ctxt.Error('', HTTP_NOTFOUND, fStaticCacheControlMaxAge)
    else
      Ctxt.Returns(cache.Body, HTTP_SUCCESS, cache.Header,
        {handle304=}true, false, fStaticCacheControlMaxAge, cache.Etag);
    exit;
  end;
  // 4. check basic context: HTTP method, URI and input parameters
  if not (Ctxt.Method in fAllowedMethods) then
  begin
    Ctxt.Error('', HTTP_NOTALLOWED); // we have an HTTP status code for that
    exit;
  end;
  meth := fApplication.fFactory.FindMethod(mainMethod);
  if meth = nil then
  begin
    Ctxt.Error('', HTTP_NOTFOUND);
    exit;
  end;
  // 5. render regular page using proper viewer
  QueryPerformanceMicroSeconds(start);
  inputContext := Ctxt.GetInputAsTDocVariant(JSON_MVC, meth);
  if subMethod <> '' then
    if (allowJsonFormat in fPublishOptions) and
       PropNameEquals(subMethod, 'json') then
      r := TMvcRendererJson.Create
    else
    begin
      Ctxt.Error('', HTTP_NOTFOUND);
      exit;
    end
  else
    r := TMvcRendererFromViews.Create;
  try
    r.Prepare(self, Ctxt.Call^.LowLevelRemoteIP, Ctxt.Call^.LowLevelUserAgent,
      Ctxt.TickCount64 div MilliSecsPerSec, @inputContext,
      pointer(Ctxt.Call^.InHead), meth);
    // handle OnBeforeRender custom callback
    if Assigned(fApplication.OnBeforeRender) then
      if not fApplication.OnBeforeRender(r) then
        exit; // aborted by this event handler
    // serialize r.InputContext^ into r.Input and execute into r.Output
    r.ExecuteCommand; // with proper caching
    // handle OnAfterRender custom callback
    if Assigned(fApplication.OnAfterRender) then
      if not fApplication.OnAfterRender(r) then
        exit; // processed by this event handler
    // return the response with proper ViewGenerationTimeTag replacement
    body := r.Output.Content;
    if viewHasGenerationTimeTag in r.OutputFlags then
      body := StringReplaceAll(body, fViews.ViewGenerationTimeTag,
        ShortStringToAnsi7String(MicroSecFrom(start)));
    Ctxt.Returns(body, r.Output.Status, r.Output.Header,
      {handle304=}true, {noerrorprocess=}true, {cachecontrol=}0,
      {hashwithouttime=}crc32cUtf8ToHex(r.Output.Content));
    if r.fOutputCookieName <> '' then
      Ctxt.OutCookie[r.fOutputCookieName] := r.fOutputCookieValue;
  finally
    r.Free;
  end;
end;

procedure TMvcRunOnRestServer.RunOnRestServerRoot(Ctxt: TRestServerUriContext);
begin
  InternalRunOnRestServer(Ctxt, Ctxt.UriWithoutRoot);
end;

procedure TMvcRunOnRestServer.RunOnRestServerSub(Ctxt: TRestServerUriContext);
begin
  if Ctxt.UriMethodPath = '' then
    Ctxt.Redirect(Ctxt.UriWithoutSignature + '/default')
  else
    InternalRunOnRestServer(Ctxt, Ctxt.UriMethodPath);
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
  _CurrentRenderer := self;
end;

function TMvcRendererReturningData.GetCookie(const CookieName: RawUtf8;
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

procedure TMvcRendererReturningData.SetCookie(const CookieName, CookieValue: RawUtf8);
begin
  fOutputCookieName  := CookieName;
  fOutputCookieValue := CookieValue;
end;

procedure TMvcRendererReturningData.ExecuteCommand;

  procedure ExecuteFromCache(const value: RawUtf8);
  begin
    fOutput.Status := HTTP_SUCCESS;
    Split(value, #0, fOutput.Header, RawUtf8(fOutput.Content));
    fOutputFlags := fRun.fViews.RenderFlags(fMethodIndex);
  end;

  function RetrievedFrom(const key: RawUtf8;
    const params: TSynNameValue): boolean;
  var
    v: PSynNameValueItem;
  begin
    v := params.FindItem(key);
    if (v <> nil) and
       (v^.Value <> '') and
       (fTix32 < cardinal(v^.Tag)) then
    begin
      ExecuteFromCache(v^.Value);
      result := true;
    end
    else
    begin
      fCacheCurrentInputValueKey := key;
      result := false;
    end;
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
  // return any cached content
  c := nil;
  if (cardinal(fMethodIndex) < cardinal(Length(fRun.fMethodCache))) and
     not fCacheDisabled then
  begin
    c := @fRun.fMethodCache[fMethodIndex];
    if c^.Policy = cacheNone then
      c := nil;
  end;
  if c <> nil then
  begin
    FastAssignNew(fCacheCurrentInputValueKey);
    if fTix32 = 0 then
      fTix32 := GetTickSec;
    c^.Safe.Lock;
    try
      case c^.Policy of
        cacheRootIgnoringSession:
          if fInput = '' then
doRoot:     if (c^.RootValue <> '') and
               (fTix32 < c^.RootValueExpirationTime) then
            begin
              ExecuteFromCache(c^.RootValue);
              exit;
            end;
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
  raise CreateGotoView(aMethod, aParametersNameValuePairs, aStatus);
end;

class procedure EMvcApplication.GotoError(const aErrorMessage: string;
  aErrorCode: integer);
begin
  raise CreateGotoError(aErrorMessage, aErrorCode);
end;

class procedure EMvcApplication.GotoError(aHtmlErrorCode: integer);
begin
  raise CreateGotoError(aHtmlErrorCode);
end;

class procedure EMvcApplication.Default(aStatus: cardinal);
begin
  raise CreateDefault(aStatus);
end;


{ TMvcApplication }

procedure TMvcApplication.Start(aRestModel: TRest; aInterface: PRttiInfo);
var
  m: PtrInt;
  met: PInterfaceMethod;
  entry: PInterfaceEntry;
begin
  fLocker := TAutoLocker.Create;
  fRestModel := aRestModel;
  fFactory := TInterfaceFactory.Get(aInterface);
  fFactoryErrorIndex := fFactory.FindMethodIndex('Error');
  if fFactoryErrorIndex < 0 then
    EMvcException.RaiseUtf8('% does not implement the ' +
      'IMvcApplication.Error() method', [aInterface.RawName]);
  entry := GetInterfaceEntry(fFactory.InterfaceGuid^);
  if entry = nil then
    EMvcException.RaiseUtf8('%.Start(%): this class should implement %',
      [self, aRestModel, aInterface.RawName]);
  fFactoryEntry := PAnsiChar(self) + entry^.IOffset;
  met := pointer(fFactory.Methods);
  for m := 1 to fFactory.MethodsCount do
  begin
    if not MethodHasView(met^) then
      if met^.ArgsOutFirst <> met^.ArgsResultIndex then
        EMvcException.RaiseUtf8(
          '%.Start(%): I% var/out param not allowed with TMvcAction result',
          [self, aRestModel, met^.InterfaceDotMethodName])
      else
        // maps TMvcAction in TMvcApplication.RunOnRestServer
        include(met^.Flags, imfResultIsServiceCustomAnswer);
    inc(met);
  end;
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

class procedure TMvcApplication.GotoView(var Action: TMvcAction; const
  MethodName: RawUtf8; const ParametersNameValuePairs: array of const;
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

procedure TMvcApplication.SetSession(Value: TMvcSessionAbstract);
begin
  FreeAndNilSafe(fSession);
  fSession := Value;
end;

procedure TMvcApplication.GetViewInfo(MethodIndex: integer; out info: variant);
begin
  TDocVariantData(info).InitFast(8, dvObject);
  if MethodIndex >= 0 then
    TDocVariantData(info).AddValueFromText(
      'pageName', fFactory.Methods[MethodIndex].Uri);
end;

procedure TMvcApplication.GetMvcInfo(out info: variant);
begin
  info := _ObjFast(['name',    fFactory.InterfaceRtti.Name,
                    'mORMot',  RawUtf8(SYNOPSE_FRAMEWORK_VERSION),
                    'root',    RestModel.Model.Root,
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

