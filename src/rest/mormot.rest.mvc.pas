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
  mormot.core.perf,
  mormot.core.rtti,
  mormot.core.crypto,
  mormot.core.json,
  mormot.core.search,
  mormot.core.secure,
  mormot.core.log,
  mormot.core.interfaces,
  mormot.core.mustache,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.orm.server,
  mormot.soa.core,
  mormot.soa.server,
  mormot.soa.codegen,
  mormot.rest.core,
  mormot.rest.server;


{ ************ Web Views Implementation using Mustache }

type
  /// TMvcView.Flags rendering context
  // - viewHasGenerationTimeTag is set if TMvcViewsAbstract.ViewGenerationTimeTag
  // text appears in the template, for this time value not to affect the cache
  TMvcViewFlags = set of (
    viewHasGenerationTimeTag);

  /// define a particular rendered View
  // - is initialized by TMvcRendererFromViews.Renders(), then rendered by the
  // TMvcViewsAbstract.Render() method
  TMvcView = record
    /// the low-level content of this View
    Content: RawByteString;
    /// the MIME content type of this View
    ContentType: RawUtf8;
    /// some additional rendering information about this View
    Flags: TMvcViewFlags;
  end;

  /// an abstract class able to implement Views
  TMvcViewsAbstract = class
  protected
    fFactory: TInterfaceFactory;
    fLogClass: TSynLogClass;
    fViewTemplateFolder, fViewStaticFolder: TFileName;
    fFactoryErrorIndex: integer;
    fViewFlags: TMvcViewFlags;
    fViewGenerationTimeTag: RawUtf8;
    procedure SetViewTemplateFolder(const aFolder: TFileName);
    /// overriden implementations should return the rendered content
    procedure Render(methodIndex: Integer; const Context: variant;
      var View: TMvcView); virtual; abstract;
    /// return the static file contents - from fViewStaticFolder by default
    // - called if cacheStatic has been defined
    function GetStaticFile(const aFileName: TFileName): RawByteString; virtual;
  public
    /// initialize the class
    constructor Create(aInterface: PRttiInfo; aLogClass: TSynLogClass);
    /// read-only access to the associated factory for the implementation class
    property Factory: TInterfaceFactory
      read fFactory;
    /// read-only access to the local folder containing the Mustache views
    property ViewTemplateFolder: TFileName
      read fViewTemplateFolder write SetViewTemplateFolder;
    /// retrieve the .static local folder name
    property ViewStaticFolder: TFileName
      read fViewStaticFolder;
    /// any occurence of this tag in a rendered view will be converted
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
    // - any value would automatically update the rendering template, if the file
    // changed after a given number of seconds - default is 5 seconds
    // - setting 0 would be slightly faster, since content would never be checked
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

  /// a class able to implement Views using Mustache templates
  TMvcViewsMustache = class(TMvcViewsAbstract)
  protected
    fViewTemplateFileTimestampMonitor: cardinal;
    fViewPartials: TSynMustachePartials;
    fViewHelpers: TSynMustacheHelpers;
    fViews: array of record // follows fFactory.Methods[]
      Mustache: TSynMustache;
      Template: RawUtf8;
      MethodName: TFileName;
      SearchPattern: TFileName;
      FileName: TFileName;
      ShortFileName: TFileName;
      FileExt: TFileName;
      ContentType: RawUtf8;
      Locker: IAutoLocker;
      FileAgeLast: PtrUInt;
      FileAgeCheckTick: Int64;
      Flags: TMvcViewFlags;
    end;
    function GetRenderer(methodIndex: integer; var view: TMvcView): TSynMustache;
    /// search for template files in ViewTemplateFolder
    function FindTemplates(const Mask: TFileName): TFileNameDynArray; virtual;
    /// return the template file contents
    function GetTemplate(const aFileName: TFileName): RawUtf8; virtual;
    /// return the template file date and time
    function GetTemplateAge(const aFileName: TFileName): PtrUInt; virtual;
    /// overriden implementations should return the rendered content
    procedure Render(methodIndex: Integer; const Context: variant;
      var View: TMvcView); override;
    // some helpers defined here to avoid mormot.core.crypto link
    class procedure md5(const Value: variant; out result: variant);
    class procedure sha1(const Value: variant; out result: variant);
    class procedure sha256(const Value: variant; out result: variant);
    class procedure sha512(const Value: variant; out result: variant);
  public
    /// create an instance of this ViewModel implementation class
    // - define the associated REST instance, the interface definition and the
    // local folder where the mustache template files are stored
    // - will search and parse the matching views (and associated *.partial)
    constructor Create(aInterface: PRttiInfo;
      const aParameters: TMvcViewsMustacheParameters;
      aLogClass: TSynLogClass = nil); reintroduce; overload; virtual;
    /// create an instance of this ViewModel implementation class
    // - this overloaded version will use default parameters (i.e. search for
    // html+json+css in the "Views" sub-folder under the executable)
    // - will search and parse the matching views (and associated *.partial),
    // optionally creating void templates for any missing view
    constructor Create(aInterface: PRttiInfo; aLogClass: TSynLogClass = nil;
      aExtensionForNotExistingTemplate: TFileName = ''); overload;
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

    /// finalize the instance
    destructor Destroy; override;
  end;



{ ************ ViewModel/Controller Sessions using Cookies }

type
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
  public
    /// create an instance of this ViewModel implementation class
    constructor Create; virtual;
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
    function CheckAndRetrieve(PRecordData: pointer = nil;
      PRecordTypeInfo: PRttiInfo = nil;
      PExpires: PCardinal = nil): integer; virtual; abstract;
    /// retrieve the session information as a JSON object
    // - returned as a TDocVariant, including any associated record Data
    // - will call CheckAndRetrieve() then RecordSaveJson() and _JsonFast()
    function CheckAndRetrieveInfo(
      PRecordDataTypeInfo: PRttiInfo): variant; virtual;
    /// clear the session
    procedure Finalize; virtual; abstract;
    /// return all session generation information as ready-to-be stored string
    // - to be retrieved via LoadContext, e.g. after restart
    function SaveContext: RawUtf8; virtual; abstract;
    /// restore session generation information from SaveContext format
    // - returns TRUE on success
    function LoadContext(const Saved: RawUtf8): boolean; virtual; abstract;
  end;

  /// information used by TMvcSessionWithCookies for cookie generation
  // - i.e. the session ID, cookie name, encryption and HMAC secret keys
  // - this data can be persisted so that the very same cookie information
  // are available after server restart
  TMvcSessionWithCookiesContext = packed record
    /// the cookie name, used for storage on the client side
    CookieName: RawUtf8;
    /// an increasing counter, to implement unique session ID
    SessionSequence: integer;
    /// secret information, used for HMAC digital signature of cookie content
    Secret: THMAC_CRC32C;
    /// random IV used as CTR on Crypt[] secret key
    CryptNonce: cardinal;
    /// secret information, used for encryption of the cookie content
    Crypt: array[byte] of byte;
  end;

  /// a class able to implement ViewModel/Controller sessions with cookies
  // - this kind of ViewModel will implement cookie-based sessions, able to
  // store any (simple) record content in the cookie, on the browser client side
  // - those cookies have the same feature set than JWT, but with a lower
  // payload (thanks to binary serialization), and cookie safety (not accessible
  // from JavaScript): they are digitally signed (with HMAC-CRC32C and a
  // temporary secret key), they include an unique session identifier (like
  // "jti" claim), issue and expiration dates (like "iat" and "exp" claims),
  // and they are encrypted with a temporary key - this secret keys is tied to
  // the TMvcSessionWithCookies instance lifetime, so new cookies are generated
  // after server restart, unless they are persisted via LoadContext/SaveContext
  // - signature and encryption are weak, but very fast, to avoid DDOS attacks
  TMvcSessionWithCookies = class(TMvcSessionAbstract)
  protected
    fContext: TMvcSessionWithCookiesContext;
    // overriden e.g. in TMvcSessionWithRestServer using ServiceContext threadvar
    function GetCookie: RawUtf8; virtual; abstract;
    procedure SetCookie(const cookie: RawUtf8); virtual; abstract;
    procedure Crypt(P: PAnsiChar; bytes: integer);
    function CheckAndRetrieveFromCookie(const cookie: RawUtf8;
      PRecordData, PRecordTypeInfo: PRttiInfo; PExpires: PCardinal): integer;
  public
    /// create an instance of this ViewModel implementation class
    constructor Create; override;
    /// will initialize the session cookie
    // - setting an optional record data, which will be stored Base64-encoded
    // - will return the 32-bit internal session ID
    // - you can supply a time period, after which the session will expire -
    // default is 1 hour, and could go up to
    function Initialize(PRecordData: pointer = nil;
      PRecordTypeInfo: PRttiInfo = nil;
      SessionTimeOutMinutes: cardinal = 60): integer; override;
    /// fast check if there is a cookie session associated to the current context
    function Exists: boolean; override;
    /// retrieve the session ID from the current cookie
    // - can optionally retrieve the record Data parameter stored in the cookie
    // - will return the 32-bit internal session ID, or 0 if the cookie is invalid
    function CheckAndRetrieve(PRecordData: pointer = nil;
      PRecordTypeInfo: PRttiInfo = nil;
      PExpires: PCardinal = nil): integer; override;
    /// clear the session
    // - by deleting the cookie on the client side
    procedure Finalize; override;
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
    property Context: TMvcSessionWithCookiesContext
      read fContext write fContext;
    /// you can customize the cookie name
    // - default is 'mORMot', and cookie is restricted to Path=/RestRoot
    property CookieName: RawUtf8
      read fContext.CookieName write fContext.CookieName;
  end;

  /// implement a ViewModel/Controller sessions in a TRestServer instance
  // - will use ServiceContext.Request threadvar to access the client cookies
  TMvcSessionWithRestServer = class(TMvcSessionWithCookies)
  protected
    function GetCookie: RawUtf8; override;
    procedure SetCookie(const cookie: RawUtf8); override;
  end;

  /// implement a single ViewModel/Controller in-memory session
  // - this kind of session could be used in-process, e.g. for a VCL/FMX GUI
  // - do NOT use it with multiple clients, e.g. from HTTP remote access
  TMvcSessionSingle = class(TMvcSessionWithCookies)
  protected
    fSingleCookie: RawUtf8;
    function GetCookie: RawUtf8; override;
    procedure SetCookie(const cookie: RawUtf8); override;
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

  TMvcApplication = class;

  /// abstract MVC rendering execution context
  // - you shoud not execute this abstract class, but any of the inherited class
  // - one instance inherited from this class would be allocated for each event
  // - may return some data (when inheriting from TMvcRendererReturningData), or
  // even simply display the value in a VCL/FMX GUI, without any output
  TMvcRendererAbstract = class
  protected
    fApplication: TMvcApplication;
    fMethodIndex: integer;
    fMethodReturnsAction: boolean;
    fInput: RawUtf8;
    procedure Renders(var outContext: variant; status: cardinal;
      forcesError: boolean); virtual; abstract;
    function Redirects(const action: TMvcAction): boolean; virtual;
    procedure CommandError(const ErrorName: RawUtf8; const ErrorValue: variant;
      ErrorCode: Integer); virtual;
  public
    /// initialize a rendering process for a given MVC Application/ViewModel
    constructor Create(aApplication: TMvcApplication); reintroduce;
    /// main execution method of the rendering process
    // - Input should have been set with the incoming execution context
    procedure ExecuteCommand(aMethodIndex: integer); virtual;
    /// incoming execution context, to be processed via ExecuteCommand() method
    // - should be specified as a raw JSON object
    property Input: RawUtf8
      read fInput write fInput;
  end;

  /// how TMvcRendererReturningData should cache its content
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
    fOutputFlags: TMvcViewFlags;
    fCacheEnabled: boolean;
    fCacheCurrent: (noCache, rootCache, inputCache);
    fCacheCurrentSec: cardinal;
    fCacheCurrentInputValueKey: RawUtf8;
    function Redirects(const action: TMvcAction): boolean; override;
  public
    /// initialize a rendering process for a given MVC Application/ViewModel
    // - you need to specify a MVC Views engine, e.g. TMvcViewsMustache instance
    constructor Create(aRun: TMvcRunWithViews); reintroduce; virtual;
    /// main execution method of the rendering process
    // - this overriden method would handle proper caching as defined by
    // TMvcRunWithViews.SetCache()
    procedure ExecuteCommand(aMethodIndex: integer); override;
    /// caller should retrieve this value after ExecuteCommand method execution
    property Output: TServiceCustomAnswer
      read fOutput;
  end;

  TMvcRendererReturningDataClass = class of TMvcRendererReturningData;

  /// MVC rendering execution context, returning some rendered View content
  // - will use an associated Views templates system, e.g. a Mustache renderer
  TMvcRendererFromViews = class(TMvcRendererReturningData)
  protected
    // Renders() will fill Output using the corresponding View, to be sent back
    procedure Renders(var outContext: variant; status: cardinal;
      forcesError: boolean); override;
  public
    /// initialize a rendering process for a given MVC Application/ViewModel
    // - this overriden constructor will ensure that cache is enabled
    constructor Create(aRun: TMvcRunWithViews); override;
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

  /// abstract class used by TMvcApplication to run TMvcViews-based process
  // - this inherited class will host a MVC Views instance, and handle
  // an optional simple in-memory cache
  TMvcRunWithViews = class(TMvcRun)
  protected
    fViews: TMvcViewsAbstract;
    fCacheLocker: IAutoLocker;
    fCache: array of record
      Policy: TMvcRendererCachePolicy;
      TimeOutSeconds: cardinal;
      RootValue: RawUtf8;
      RootValueExpirationTime: cardinal;
      InputValues: TSynNameValue;
    end;
  public
    /// link this runner class to a specified MVC application
    constructor Create(aApplication: TMvcApplication;
      aViews: TMvcViewsAbstract = nil); reintroduce;
    /// method called to flush the caching mechanism for a MVC command
    procedure NotifyContentChangedForMethod(aMethodIndex: integer); override;
    /// defines the caching policy for a given MVC command
    // - a time expiration period (up to 5 minutes) can also be defined per
    // MVC command - leaving default 0 will set to 5 minutes expiration delay
    // - function calls can be chained to create some fluent definition interface
    // like in TAnyBLogapplication.Create:
    // ! fMainRunner := TMvcRunWithViews.Create(self).SetCache('default',cacheRoot);
    function SetCache(const aMethodName: RawUtf8;
      aPolicy: TMvcRendererCachePolicy;
      aTimeOutSeconds: cardinal = 0): TMvcRunWithViews; virtual;
    /// finalize this instance
    destructor Destroy; override;
    /// read-write access to the associated MVC Views instance
    property Views: TMvcViewsAbstract
      read fViews;
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
  // - registerORMTableAsExpressions will register Mustache Expression Helpers
  // for every TOrm table of the Server data model
  // - by default, TRestServer authentication would be by-passed for all
  // MVC routes, unless bypassAuthentication option is undefined
  TMvcPublishOption = (
    publishMvcInfo,
    publishStatic,
    cacheStatic,
    registerORMTableAsExpressions,
    bypassAuthentication);

  /// which kind of optional content should be publish
  TMvcPublishOptions = set of TMvcPublishOption;

  /// run TMvcApplication directly within a TRestServer method-based service
  // - this is the easiest way to host and publish a MVC Application, optionally
  // in conjunction with REST/AJAX client access
  TMvcRunOnRestServer = class(TMvcRunWithViews)
  protected
    fRestServer: TRestServer;
    fPublishOptions: TMvcPublishOptions;
    fMvcInfoCache: RawUtf8;
    fStaticCache: TSynNameValue;
    fStaticCacheControlMaxAge: integer;
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
    // - will also create a TMvcSessionWithRestServer for simple cookie sessions
    // - aPublishOptions could be used to specify integration with the server
    constructor Create(aApplication: TMvcApplication;
      aRestServer: TRestServer = nil; const aSubURI: RawUtf8 = '';
      aViews: TMvcViewsAbstract = nil;
      aPublishOptions: TMvcPublishOptions=
        [low(TMvcPublishOption) .. high(TMvcPublishOption)]); reintroduce;
    /// define some content for a static file
    // - only used if cacheStatic has been defined
    function AddStaticCache(const aFileName: TFileName;
      const aFileContent: RawByteString): RawByteString;
    /// current publishing options, as specify to the constructor
    property PublishOptions: TMvcPublishOptions
      read fPublishOptions write fPublishOptions;
    /// optional "Cache-Control: max-age=###" header value for static content
    property StaticCacheControlMaxAge: integer
      read fStaticCacheControlMaxAge write fStaticCacheControlMaxAge;
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
    fSession: TMvcSessionAbstract;
    fRestModel: TRest;
    fRestServer: TRestServer;
    fLocker: IAutoLocker;
    // if any TMvcRun instance is store here, will be freed by Destroy
    // but note that a single TMvcApplication logic may handle several TMvcRun
    fMainRunner: TMvcRun;
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


const
  /// the pseudo-method name for the MVC information html page
  MVCINFO_URI = 'mvc-info';

  /// the pseudo-method name for any static content for Views
  STATIC_URI = '.static';



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
  '{{#args}}{{#dirInput}}{{argName}}=</b>..[{{typePascal}}]..<b>' +
  '{{#commaInSingle}}&{{/commaInSingle}}{{/dirInput}}{{/args}}{{/hasInParams}}{{/url}}' +
  '{{<mustache}}<b>&#123;{Main&#125;}</b>: variant{{#args}}{{#dirOutput}}<br><b>{&#123;' +
  '{{argName}}&#125;}</b>: {{typePascal}}{{/dirOutput}}{{/args}}{{/mustache}}' +
  '<html><head><title>{{Name}} Information</title></head><body ' +
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
  '<html><head><title>mormot.rest.mvc Error</title></head><body style=' +
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
  fViewTemplateFolder :=
    IncludeTrailingPathDelimiter(aFolder);
  fViewStaticFolder :=
    IncludeTrailingPathDelimiter(fViewTemplateFolder + STATIC_URI);
end;

function TMvcViewsAbstract.GetStaticFile(
  const aFileName: TFileName): RawByteString;
begin
  result := StringFromFile(fViewStaticFolder + aFileName);
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
    procedure ExpressionGet(const Value: variant; out result: variant);
    procedure ExpressionHtmlTable(const Value: variant; out result: variant);
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
  out result: variant);
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
  tmp: TTextWriterStackBuffer;
const
  ONOFF: array[boolean] of THtmlTableStyleLabel = (
    labelOff, labelOn);
  ENUM: array[boolean, boolean] of THtmlTableStyleLabel = (
   (labelValue, labelValue),
   (labelFalse, labelTrue));
begin
  Rec := _Safe(Value);
  if Rec^.Kind = dvObject then
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
            [oftAnsiText, oftUtf8Text, oftInteger, oftFloat, oftCurrency,
             oftTimeLog, oftModTime, oftCreateTime, oftDateTime, oftDateTimeMS,
             oftUnixTime, oftUnixMSTime, oftBoolean, oftEnumerate, oftSet]) then
          // we support only most obvious types in 'case OrmFieldType of" below
          continue;
        HtmlTableStyle.BeforeFieldName(W);
        GetCaptionFromPCharLen(TrimLeftLowerCase(Field.Name), caption);
        W.AddHtmlEscapeString(caption);
        HtmlTableStyle.BeforeValue(W);
        VariantToUtf8(Rec^.Values[i], u);
        case Field.OrmFieldType of
          oftAnsiText, oftUtf8Text, oftInteger, oftFloat, oftCurrency:
            W.AddHtmlEscape(pointer(u));
          oftTimeLog, oftModTime, oftCreateTime:
            if VariantToInt64(Rec^.Values[i], timelog.Value) then
              W.AddHtmlEscapeString(timelog.i18nText);
          oftDateTime, oftDateTimeMS:
            begin
              timelog.From(u);
              W.AddHtmlEscapeString(timelog.i18nText);
            end;
          oftUnixTime, oftUnixMSTime:
            if VariantToInt64(Rec^.Values[i], timelog.Value) then
            begin
              if Field.OrmFieldType = oftUnixTime then
                timelog.FromUnixTime(timelog.Value)
              else
                timelog.FromUnixMSTime(timelog.Value);
              W.AddHtmlEscapeString(timelog.i18nText);
            end;
          oftBoolean, oftEnumerate:
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
                  W.AddShort('<br/>');
                end;
              finally
                sets.Free;
              end;
            end;
        end;
        HtmlTableStyle.AfterValue(W);
      end;
      HtmlTableStyle.EndTable(W);
      RawUtf8ToVariant(W.Text, result);
    finally
      W.Free;
    end;
  end
  else
    // not an object -> return input value as is
    result := Value;
end;

procedure TExpressionHelperForTable.ExpressionGet(const Value: variant;
  out result: variant);
var
  Rec: TOrm;
  ID: integer;
begin
  SetVariantNull(result);
  if not VariantToInteger(Value, ID) then
    exit;
  Rec := Table.Create;
  try
    if Rest.ORM.Retrieve(ID, Rec) then
      result := Rec.GetSimpleFieldsAsDocVariant(true);
  finally
    Rec.Free;
  end;
end;


{ TExpressionHtmlTableStyle }

class procedure TExpressionHtmlTableStyle.AddLabel(WR: TTextWriter;
  const text: string; kind: THtmlTableStyleLabel);
const
  SETLABEL: array[THtmlTableStyleLabel] of string[3] = (
    '', '', '- ', '+ ', '');
begin
  WR.AddShort(SETLABEL[kind]);
  WR.AddHtmlEscapeString(text);
  WR.AddShort('&nbsp;');
end;

class procedure TExpressionHtmlTableStyle.AfterValue(WR: TTextWriter);
begin
  WR.AddShort('</td></tr>');
end;

class procedure TExpressionHtmlTableStyle.BeforeFieldName(WR: TTextWriter);
begin
  WR.AddShort('<tr><td>');
end;

class procedure TExpressionHtmlTableStyle.BeforeValue(WR: TTextWriter);
begin
  WR.AddShort('</td><td>');
end;

class procedure TExpressionHtmlTableStyle.EndTable(WR: TTextWriter);
begin
  WR.AddShort('</table>');
end;

class procedure TExpressionHtmlTableStyle.StartTable(WR: TTextWriter);
begin
  WR.AddShort('<table>');
end;


{ TExpressionHtmlTableStyleBootstrap }

class procedure TExpressionHtmlTableStyleBootstrap.AddLabel(WR: TTextWriter;
  const text: string; kind: THtmlTableStyleLabel);
const
  SETLABEL: array[THtmlTableStyleLabel] of string[7] = (
    'danger', 'success', 'danger', 'success', 'primary');
begin
  WR.AddShort('<span class="label label-');
  WR.AddShort(SETLABEL[kind]);
  WR.Add('"', '>');
  WR.AddHtmlEscapeString(text);
  WR.AddShort('</span>');
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
  m, i: PtrInt;
  LowerExt: TFileName;
  files: TFileNameDynArray;
  partial: TSynMustache;
  partialName: RawUtf8;
  info: variant;
begin
  inherited Create(aInterface, aLogClass);
  // get views
  fViewTemplateFileTimestampMonitor :=
    aParameters.FileTimestampMonitorAfterSeconds;
  if aParameters.Folder = '' then
    ViewTemplateFolder := ExeVersion.ProgramFilePath + 'Views'
  else
    ViewTemplateFolder := aParameters.Folder;
  if (aParameters.ExtensionForNotExistingTemplate <> '') and
     not DirectoryExists(ViewTemplateFolder) then
    CreateDir(ViewTemplateFolder);
  if aParameters.CsvExtensions = '' then
    LowerExt := ',html,json,css,'
  else
    LowerExt := ',' + SysUtils.LowerCase(aParameters.CsvExtensions) + ',';
  SetLength(fViews, fFactory.MethodsCount);
  for m := 0 to fFactory.MethodsCount - 1 do
    if MethodHasView(fFactory.Methods[m]) then
      with fViews[m] do
      begin
        Locker := TAutoLocker.Create;
        MethodName := Utf8ToString(fFactory.Methods[m].Uri);
        SearchPattern := MethodName + '.*';
        files := FindTemplates(SearchPattern);
        if length(files) > 0 then
        begin
          for i := 0 to length(files) - 1 do
          begin
            ShortFileName := files[i];
            FileExt := SysUtils.LowerCase(
              copy(ExtractFileExt(ShortFileName), 2, 100));
            if Pos(',' + FileExt + ',', LowerExt) > 0 then
              // found a template with the right extension
              break;
          end;
          FileName := ViewTemplateFolder + ShortFileName;
          ContentType := GetMimeContentType(nil, 0, ShortFileName);
        end
        else
        begin
          fLogClass.Add.Log(sllWarning,
            '%.Create: Missing View file in %', [self, SearchPattern]);
          if aParameters.ExtensionForNotExistingTemplate <> '' then
          begin
            ShortFileName :=
              MethodName + aParameters.ExtensionForNotExistingTemplate;
            FileName := ViewTemplateFolder + ShortFileName;
            info := ContextFromMethod(fFactory.Methods[m]);
            _ObjAddProp(
              'interfaceName', fFactory.InterfaceTypeInfo^.RawName, info);
            FileFromString(StringReplaceChars(StringReplaceChars(
              TSynMustache.Parse(MUSTACHE_VOIDVIEW).Render(info),
              '<', '{'), '>', '}'), FileName);
          end;
        end;
      end;
  fViewHelpers := aParameters.Helpers;
  // get partials
  fViewPartials := TSynMustachePartials.Create;
  files := FindTemplates('*.partial');
  for i := 0 to length(files) - 1 do
  begin
    StringToUtf8(GetFileNameWithoutExt(files[i]), partialName);
    try
      partial := fViewPartials.Add(partialName, GetTemplate(files[i]));
      if not (viewHasGenerationTimeTag in fViewFlags) and
         partial.FoundInTemplate(fViewGenerationTimeTag) then
        include(fViewFlags, viewHasGenerationTimeTag);
    except
      on E: Exception do
        fLogClass.Add.Log(sllError, '%.Create: Invalid Partial file % - %',
          [self, files[i], E]);
    end;
  end;
end;

constructor TMvcViewsMustache.Create(aInterface: PRttiInfo;
  aLogClass: TSynLogClass; aExtensionForNotExistingTemplate: TFileName);
var
  params: TMvcViewsMustacheParameters;
begin
  FillcharFast(params, sizeof(params), 0);
  params.FileTimestampMonitorAfterSeconds := 5;
  params.ExtensionForNotExistingTemplate := aExtensionForNotExistingTemplate;
  params.Helpers := TSynMustache.HelpersGetStandardList;
  Create(aInterface, params, aLogClass);
end;

destructor TMvcViewsMustache.Destroy;
begin
  inherited;
  fViewPartials.Free;
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
                                      [md5, sha1, sha256, sha512]);
end;

class procedure TMvcViewsMustache.md5(const Value: variant;
  out result: variant);
begin
  RawUtf8ToVariant(mormot.core.crypto.Md5(ToUtf8(Value)), result);
end;

class procedure TMvcViewsMustache.sha1(const Value: variant;
  out result: variant);
begin
  RawUtf8ToVariant(mormot.core.crypto.Sha1(ToUtf8(Value)), result);
end;

class procedure TMvcViewsMustache.sha256(const Value: variant;
  out result: variant);
begin
  RawUtf8ToVariant(mormot.core.crypto.Sha256(ToUtf8(Value)), result);
end;

class procedure TMvcViewsMustache.sha512(const Value: variant;
  out result: variant);
begin
  RawUtf8ToVariant(mormot.core.crypto.Sha512(ToUtf8(Value)), result);
end;

function TMvcViewsMustache.GetRenderer(methodIndex: integer;
  var view: TMvcView): TSynMustache;
var
  age: PtrUInt;
begin
  if cardinal(methodIndex) >= fFactory.MethodsCount then
    raise EMvcException.CreateUtf8(
      '%.Render(methodIndex=%)', [self, methodIndex]);
  with fViews[methodIndex],
       Locker.ProtectMethod do
  begin
    if MethodName = '' then
      raise EMvcException.CreateUtf8(
        '%.Render(''%''): not a View', [self, MethodName]);
    if (Mustache = nil) and
       (FileName = '') then
      raise EMvcException.CreateUtf8(
        '%.Render(''%''): Missing Template in ''%''',
        [self, MethodName, SearchPattern]);
    if (Mustache = nil) or
       ((fViewTemplateFileTimestampMonitor <> 0) and
        (FileAgeCheckTick < GetTickCount64)) then
    begin
      age := GetTemplateAge(ShortFileName);
      if (Mustache = nil) or
         (age <> FileAgeLast) then
      begin
        Mustache := nil; // no Mustache.Free: TSynMustache instances are cached
        FileAgeLast := age;
        Template := GetTemplate(ShortFileName);
        if Template <> '' then
        try
          Mustache := TSynMustache.Parse(Template);
          if Mustache.FoundInTemplate(fViewGenerationTimeTag) then
            include(Flags, viewHasGenerationTimeTag);
        except
          on E: Exception do
            raise EMvcException.CreateUtf8(
              '%.Render(''%''): Invalid Template: % - %',
              [self, ShortFileName, E, E.Message]);
        end
        else
          raise EMvcException.CreateUtf8(
            '%.Render(''%''): Missing Template in ''%''',
            [self, ShortFileName, SearchPattern]);
        if fViewTemplateFileTimestampMonitor <> 0 then
          FileAgeCheckTick := GetTickCount64 +
            Int64(fViewTemplateFileTimestampMonitor) * Int64(1000);
      end;
    end;
    view.ContentType := ContentType;
    view.Flags := view.Flags + Flags;
    result := Mustache;
  end;
end;

function TMvcViewsMustache.FindTemplates(
    const Mask: TFileName): TFileNameDynArray;
begin
  result := FindFilesDynArrayToFileNames(FindFiles(
    ViewTemplateFolder, Mask, '', {sorted=}false, {withdir=}false));
end;

function TMvcViewsMustache.GetTemplate(const aFileName: TFileName): RawUtf8;
begin
  result := AnyTextFileToRawUtf8(ViewTemplateFolder + aFileName, true);
end;

{$WARN SYMBOL_DEPRECATED OFF} // we don't need TDateTime, just values to compare
function TMvcViewsMustache.GetTemplateAge(const aFileName: TFileName): PtrUInt;
begin
  result := FileAge(ViewTemplateFolder + aFileName);
end;
{$WARN SYMBOL_DEPRECATED ON}

procedure TMvcViewsMustache.Render(methodIndex: Integer; const Context: variant;
  var View: TMvcView);
begin
  View.Content := GetRenderer(methodIndex, View).Render(
    Context, fViewPartials, fViewHelpers);
  if IsVoid(View.Content) then
    // rendering failure
    with fViews[methodIndex] do
    begin
      Locker.Enter;
      try
        Mustache := nil; // force reload view ASAP
      finally
        Locker.Leave;
      end;
      raise EMvcException.CreateUtf8(
        '%.Render(''%''): Void [%] Template - please customize this file!',
        [self, ShortFileName, FileName]);
    end;
end;


{ ************ ViewModel/Controller Sessions using Cookies }

{ TMvcSessionAbstract }

constructor TMvcSessionAbstract.Create;
begin
  inherited;
end;

function TMvcSessionAbstract.CheckAndRetrieveInfo(
  PRecordDataTypeInfo: PRttiInfo): variant;
var
  rec: TByteToWord; // 512 bytes to store locally any kind of record
  recsize: integer;
  sessionID: integer;

  procedure ProcessSession;
  var
    recjson: RawUtf8;
  begin
    // create a TDocVariant from the binary record content
    SaveJson(rec, PRecordDataTypeInfo, TEXTWRITEROPTIONS_MUSTACHE, recjson);
    TDocVariantData(result).InitJsonInPlace(
      pointer(recjson), JSON_OPTIONS_FAST);
  end;

begin
  SetVariantNull(result);
  if PRecordDataTypeInfo = nil then
    // sessionID decoding only
    recsize := 0
  else
  begin
    // binary decoding of a rkRecord
    recsize := PRecordDataTypeInfo^.RecordSize;
    if recsize > SizeOf(rec) then
      raise EMvcException.CreateUtf8(
        '%.CheckAndRetrieveInfo: recsize=% overflow', [self, recsize]);
    FillCharFast(rec, recsize, 0);
  end;
  try
    sessionID := CheckAndRetrieve(@rec, PRecordDataTypeInfo);
    if sessionID <> 0 then
    begin
      if recsize > 0 then
        ProcessSession;
      _ObjAddProps(['id', sessionID], result);
    end;
  finally
    if recsize > 0 then
      // manual finalization of managed fields
      FastRecordClear(@rec, PRecordDataTypeInfo);
  end;
end;


{ TMvcSessionWithCookies }

constructor TMvcSessionWithCookies.Create;
var
  rnd: THash512;
begin
  inherited Create;
  fContext.CookieName := 'mORMot';
  // temporary secret for encryption
  fContext.CryptNonce := Random32;
  TAesPrng.Main.FillRandom(@fContext.Crypt, sizeof(fContext.Crypt));
  // temporary secret for HMAC-CRC32C
  TAesPrng.Main.FillRandom(@rnd, sizeof(rnd));
  fContext.Secret.Init(@rnd, sizeof(rnd));
end;

procedure XorMemoryCtr(data: PCardinal; key256bytes: PCardinalArray;
  size: PtrUInt; ctr: cardinal);
begin
  while size >= sizeof(cardinal) do
  begin
    dec(size, sizeof(cardinal));
    data^ := data^ xor key256bytes[ctr and $3f] xor ctr;
    inc(data);
    ctr := ((ctr xor (ctr shr 15)) * 2246822519); // prime-number ctr diffusion
    ctr := ((ctr xor (ctr shr 13)) * 3266489917);
    ctr := ctr xor (ctr shr 16);
  end;
  while size <> 0 do
  begin
    dec(size);
    PByteArray(data)[size] := PByteArray(data)[size] xor ctr;
    ctr := ctr shr 8; // 1..3 pending iterations
  end;
end;

procedure TMvcSessionWithCookies.Crypt(P: PAnsiChar; bytes: integer);
begin
  XorMemoryCtr(@P[4], @fContext.Crypt, bytes - 4,
    {ctr=}xxHash32(fContext.CryptNonce, P, 4));
end;

function TMvcSessionWithCookies.Exists: boolean;
begin
  result := GetCookie <> '';
end;

type
  // map the binary layout of our base-64 serialized cookies
  TCookieContent = packed record
    head: packed record
      cryptnonce: cardinal; // ctr=hash32(cryptnonce)
      hmac: cardinal;       // = signature
      session: integer;     // = jti claim
      issued: cardinal;     // = iat claim (from UnixTimeUtc - Y2106)
      expires: cardinal;    // = exp claim
    end;
    data: array[0..2047] of byte; // binary serialization of record value
  end;
  PCookieContent = ^TCookieContent;

function TMvcSessionWithCookies.CheckAndRetrieve(PRecordData: pointer;
  PRecordTypeInfo: PRttiInfo; PExpires: PCardinal): integer;
var
  cookie: RawUtf8;
begin
  cookie := GetCookie;
  if cookie = '' then
    // no cookie -> no session
    result := 0
  else
    result := CheckAndRetrieveFromCookie(
      cookie, PRecordData, PRecordTypeInfo, PExpires);
end;

function TMvcSessionWithCookies.CheckAndRetrieveFromCookie(const cookie: RawUtf8;
  PRecordData, PRecordTypeInfo: PRttiInfo; PExpires: PCardinal): integer;
var
  clen, len: integer;
  now: cardinal;
  ccend: PAnsiChar;
  cc: TCookieContent;
begin
  result := 0; // parsing error
  if cookie = '' then
    exit;
  clen := length(cookie);
  len := Base64uriToBinLength(clen);
  if (len >= sizeof(cc.head)) and
     (len <= sizeof(cc)) and
     Base64uriDecode(pointer(cookie), @cc, clen) then
  begin
    Crypt(@cc, len);
    if (cardinal(cc.head.session) <= cardinal(fContext.SessionSequence)) then
    begin
      if PExpires <> nil then
        PExpires^ := cc.head.expires;
      now := UnixTimeUtc;
      if (cc.head.issued <= now) and
         (cc.head.expires >= now) and
         (fContext.Secret.Compute (@cc.head.session, len - 8) = cc.head.hmac) then
        if (PRecordData = nil) or
           (PRecordTypeInfo = nil) then
          result := cc.head.session
        else if len > sizeof(cc.head) then
        begin
          ccend := PAnsiChar(@cc) + len;
          if BinaryLoad(PRecordData, @cc.data, PRecordTypeInfo,
              nil, ccend, rkRecordTypes) = ccend then
            result := cc.head.session;
        end;
    end;
  end;
  if result = 0 then
    // delete any invalid/expired cookie on server side
    Finalize;
end;

function TMvcSessionWithCookies.Initialize(PRecordData: pointer;
  PRecordTypeInfo: PRttiInfo; SessionTimeOutMinutes: cardinal): integer;
var
  saved: TSynTempBuffer;
  cc: TCookieContent;
begin
  result := InterlockedIncrement(fContext.SessionSequence);
  if result = MaxInt - 1024 then
    // thread-safe overflow rounding (disconnecting previous sessions)
    fContext.SessionSequence := 0;
  if (PRecordData <> nil) and
     (PRecordTypeInfo <> nil) then
    BinarySave(PRecordData, saved, PRecordTypeInfo, rkRecordTypes)
  else
    saved.Init(0); // save.buf=nil and save.len=0
  try
    if saved.len > sizeof(cc.data) then
      // all cookies storage should be < 4K
      raise EMvcApplication.CreateGotoError('Too Big Too Fat Cookie');
    cc.head.cryptnonce := Random32;
    cc.head.session := result;
    cc.head.issued := UnixTimeUtc;
    if SessionTimeOutMinutes = 0 then
      // 1 month expiration is a reasonable high value
      SessionTimeOutMinutes := 31 * 24 * 60;
    cc.head.expires := cc.head.issued + SessionTimeOutMinutes * 60;
    if saved.len > 0 then
      MoveFast(saved.buf^, cc.data, saved.len);
    inc(saved.len, sizeof(cc.head));
    cc.head.hmac := fContext.Secret.Compute(@cc.head.session, saved.len - 8);
    Crypt(@cc, saved.len);
    SetCookie(BinToBase64Uri(@cc, saved.len));
  finally
    saved.Done;
  end;
end;

procedure TMvcSessionWithCookies.Finalize;
begin
  SetCookie(COOKIE_EXPIRED);
end;

function TMvcSessionWithCookies.LoadContext(const Saved: RawUtf8): boolean;
begin
  result := RecordLoadBase64(pointer(Saved), length(Saved), fContext,
    TypeInfo(TMvcSessionWithCookiesContext));
end;

function TMvcSessionWithCookies.SaveContext: RawUtf8;
begin
  result := RecordSaveBase64(fContext, TypeInfo(TMvcSessionWithCookiesContext));
end;


{ TMvcSessionWithRestServer }

function TMvcSessionWithRestServer.GetCookie: RawUtf8;
begin
  result := ServiceRunningContext.Request.InCookie[fContext.CookieName];
end;

procedure TMvcSessionWithRestServer.SetCookie(const cookie: RawUtf8);
var
  ctxt: TRestServerUriContext;
begin
  ctxt := ServiceRunningContext.Request;
  ctxt.OutSetCookie := fContext.CookieName + '=' + cookie;
  ctxt.InCookie[CookieName] := cookie;
end;


{ TMvcSessionSingle }

function TMvcSessionSingle.GetCookie: RawUtf8;
begin
  result := fSingleCookie;
end;

procedure TMvcSessionSingle.SetCookie(const cookie: RawUtf8);
begin
  fSingleCookie := cookie;
end;



{ ************ Web Renderer Returning Mustache Views or Json }


{ TMvcRendererAbstract }

constructor TMvcRendererAbstract.Create(aApplication: TMvcApplication);
begin
  fApplication := aApplication;
end;

procedure TMvcRendererAbstract.CommandError(const ErrorName: RawUtf8;
  const ErrorValue: variant; ErrorCode: Integer);
var
  info, renderContext: variant;
begin
  fApplication.GetViewInfo(fMethodIndex, info);
  renderContext := _ObjFast([
    'main', info,
    'msg', StatusCodeToErrorMsg(ErrorCode),
    'errorCode', ErrorCode,
    ErrorName, ErrorValue]);
  renderContext.originalErrorContext := JsonReformat(ToUtf8(renderContext));
  Renders(renderContext, ErrorCode, true);
end;

procedure TMvcRendererAbstract.ExecuteCommand(aMethodIndex: integer);
var
  action: TMvcAction;
  exec: TInterfaceMethodExecute;
  isAction: boolean;
  WR: TTextWriter;
  m: PInterfaceMethod;
  methodOutput: RawUtf8;
  renderContext, info: variant;
  err: shortstring;
  tmp: TTextWriterStackBuffer;
begin
  action.ReturnedStatus := HTTP_SUCCESS;
  fMethodIndex := aMethodIndex;
  try
    if fMethodIndex >= 0 then
    begin
      repeat
        try
          m := @fApplication.fFactory.Methods[fMethodIndex];
          isAction := m^.ArgsResultIsServiceCustomAnswer;
          WR := TJsonSerializer.CreateOwnedStream(tmp);
          try
            WR.Add('{');
            exec := TInterfaceMethodExecute.Create(m);
            try
              exec.Options := [optVariantCopiedByReference];
              exec.ServiceCustomAnswerStatus := action.ReturnedStatus;
              err := '';
              if not exec.ExecuteJson([fApplication.fFactoryEntry],
                  pointer(fInput), WR, @err, true) then
              begin
                if err = '' then
                  err := 'execution error';
                raise EMvcException.CreateUtf8('%.CommandRunMethod(I%): %',
                  [self, m^.InterfaceDotMethodName, err])
              end;
              action.RedirectToMethodName := exec.ServiceCustomAnswerHead;
              action.ReturnedStatus := exec.ServiceCustomAnswerStatus;
            finally
              exec.Free;
            end;
            if not isAction then
              WR.Add('}');
            WR.SetText(methodOutput);
          finally
            WR.Free;
          end;
          if isAction then
            // was a TMvcAction mapped in a TServiceCustomAnswer record
            action.RedirectToMethodParameters := methodOutput
          else
          begin
            // rendering, e.g. with fast Mustache {{template}}
            _Json(methodOutput, renderContext, JSON_OPTIONS_FAST);
            fApplication.GetViewInfo(fMethodIndex, info);
            _Safe(renderContext)^.AddValue('main', info);
            if fMethodIndex = fApplication.fFactoryErrorIndex then
              _ObjAddProps([
                'errorCode', action.ReturnedStatus,
                'originalErrorContext', JsonReformat(ToUtf8(renderContext))],
                renderContext);
            Renders(renderContext, action.ReturnedStatus, false);
            exit; // success
          end;
        except
          on E: EMvcApplication do
            // lower level exceptions will be handled below
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
      until fMethodIndex < 0; // loop to handle redirection
    end;
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

constructor TMvcRendererFromViews.Create(aRun: TMvcRunWithViews);
begin
  inherited Create(aRun);
  fCacheEnabled := true;
end;

procedure TMvcRendererFromViews.Renders(var outContext: variant;
  status: cardinal; forcesError: boolean);
var
  view: TMvcView;
begin
  view.Flags := fRun.fViews.fViewFlags;
  if forcesError or
     (fMethodIndex = fRun.fViews.fFactoryErrorIndex) then
  try
    // current rendering of the error page
    fRun.fViews.Render(fRun.fViews.fFactoryErrorIndex, outContext, view);
  except
    // fallback to our default HTML error template, if custom one is buggy
    on E: Exception do
    begin
      _ObjAddProps([
        'exceptionName', E.ClassName,
        'exceptionMessage', E.Message,
        'className', ClassName], outContext);
      view.Content := TSynMustache.Parse(MUSTACHE_DEFAULTERROR).
        Render(outContext);
      view.ContentType := HTML_CONTENT_TYPE;
    end;
  end
  else
    // regular view page rendering
    fRun.fViews.Render(fMethodIndex, outContext, view);
  fOutput.Content := view.Content;
  fOutput.Header := HEADER_CONTENT_TYPE + view.ContentType;
  fOutput.Status := status;
  fOutputFlags := view.Flags;
end;


{ TMvcRendererJson }

procedure TMvcRendererJson.Renders(var outContext: variant; status: cardinal;
  forcesError: boolean);
begin
  fOutput.Content := JsonReformat(ToUtf8(outContext));
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
  fCacheLocker := TAutoLocker.Create;
end;

function TMvcRunWithViews.SetCache(const aMethodName: RawUtf8;
  aPolicy: TMvcRendererCachePolicy;
  aTimeOutSeconds: cardinal): TMvcRunWithViews;
const
  MAX_CACHE_TIMEOUT = 60 * 15; // 15 minutes
var
  aMethodIndex: PtrInt;
begin
  with fCacheLocker.ProtectMethod do
  begin
    aMethodIndex := fApplication.fFactory.CheckMethodIndex(aMethodName);
    if fCache = nil then
      SetLength(fCache, fApplication.fFactory.MethodsCount);
    with fCache[aMethodIndex] do
    begin
      Policy := aPolicy;
      if aTimeOutSeconds - 1 >= MAX_CACHE_TIMEOUT then
        TimeOutSeconds := MAX_CACHE_TIMEOUT
      else
        TimeOutSeconds := aTimeOutSeconds;
      NotifyContentChangedForMethod(aMethodIndex);
    end;
  end;
  result := self;
end;

destructor TMvcRunWithViews.Destroy;
begin
  fViews.Free;
  inherited;
end;

procedure TMvcRunWithViews.NotifyContentChangedForMethod(aMethodIndex: integer);
begin
  inherited;
  with fCacheLocker.ProtectMethod do
    if cardinal(aMethodIndex) < cardinal(Length(fCache)) then
      with fCache[aMethodIndex] do
        case Policy of
          cacheRootIgnoringSession, cacheRootIfSession, cacheRootIfNoSession:
            RootValue := '';
          cacheRootWithSession, cacheWithParametersIgnoringSession,
          cacheWithParametersIfSession, cacheWithParametersIfNoSession:
            InputValues.Init(false);
        end;
end;


{ TMvcRunOnRestServer }

constructor TMvcRunOnRestServer.Create(aApplication: TMvcApplication;
  aRestServer: TRestServer; const aSubURI: RawUtf8; aViews: TMvcViewsAbstract;
  aPublishOptions: TMvcPublishOptions);
var
  m: PtrInt;
  bypass: boolean;
  method: RawUtf8;
begin
  if aApplication = nil then
    raise EMvcException.CreateUtf8('%.Create(aApplication=nil)', [self]);
  if aRestServer = nil then
    fRestServer := aApplication.RestModel as TRestServer
  else
    fRestServer := aRestServer;
  if aViews = nil then
    aViews := TMvcViewsMustache.Create(aApplication.fFactory.InterfaceTypeInfo,
      fRestServer.LogClass, '.html')
  else
    aViews.fLogClass := fRestServer.LogClass;
  inherited Create(aApplication, aViews);
  fPublishOptions := aPublishOptions;
  bypass := bypassAuthentication in fPublishOptions;
  if aSubURI <> '' then
    fRestServer.ServiceMethodRegister(aSubURI, RunOnRestServerSub, bypass)
  else
  begin
    for m := 0 to fApplication.fFactory.MethodsCount - 1 do
    begin
      method := fApplication.fFactory.Methods[m].Uri;
      if method[1] = '_' then
        // e.g. IService._Start() -> /service/start
        delete(method, 1, 1);
      fRestServer.ServiceMethodRegister(method, RunOnRestServerRoot, bypass);
    end;
    if publishMvcInfo in fPublishOptions then
      fRestServer.ServiceMethodRegister(
        MVCINFO_URI, RunOnRestServerRoot, bypass);
    if publishStatic in fPublishOptions then
      fRestServer.ServiceMethodRegister(
        STATIC_URI, RunOnRestServerRoot, bypass);
  end;
  if (registerORMTableAsExpressions in fPublishOptions) and
     aViews.InheritsFrom(TMvcViewsMustache) then
    TMvcViewsMustache(aViews).RegisterExpressionHelpersForTables(fRestServer);
  fStaticCache.Init({casesensitive=}true);
  fApplication.SetSession(TMvcSessionWithRestServer.Create);
end;

function TMvcRunOnRestServer.AddStaticCache(const aFileName: TFileName;
  const aFileContent: RawByteString): RawByteString;
begin
  if aFileContent <> '' then
    result := GetMimeContentType(
      pointer(aFileContent), length(aFileContent), aFileName) + #10 +
      // also cache content-type
      aFileContent
  else
    result := '';
  fStaticCache.Add(StringToUtf8(aFileName), result);
end;

procedure TMvcRunOnRestServer.InternalRunOnRestServer(
  Ctxt: TRestServerUriContext; const MethodName: RawUtf8);
var
  mvcinfo, inputContext: variant;
  rawMethodName, rawFormat, cached, body, content: RawUtf8;
  staticFileName: TFileName;
  rendererClass: TMvcRendererReturningDataClass;
  renderer: TMvcRendererReturningData;
  methodIndex: integer;
  method: PInterfaceMethod;
  timer: TPrecisionTimer;
begin
  Split(MethodName, '/', rawMethodName, rawFormat);
  // 1. implement mvc-info endpoint
  if (publishMvcInfo in fPublishOptions) and
     IdemPropNameU(rawMethodName, MVCINFO_URI) then
  begin
    if fMvcInfoCache = '' then
    begin
      fApplication.GetMvcInfo(mvcinfo);
      mvcinfo.viewsFolder := fViews.ViewTemplateFolder;
      fMvcInfoCache := TSynMustache.Parse(MUSTACHE_MVCINFO).Render(mvcinfo);
    end;
    Ctxt.Returns(fMvcInfoCache, HTTP_SUCCESS, HTML_CONTENT_TYPE_HEADER, True);
  end
  else
  // 2. serve static resources, with proper caching
  if (publishStatic in fPublishOptions) and
     IdemPropNameU(rawMethodName, STATIC_URI) then
  begin
    // code below will use a local in-memory cache, but would do the same as:
    // Ctxt.ReturnFileFromFolder(fViews.ViewStaticFolder);
    fCacheLocker.Enter;
    try
      if cacheStatic in fPublishOptions then
        cached := fStaticCache.Value(rawFormat, #0)
      else
        cached := #0;
      if cached = #0 then
        if PosEx('..', rawFormat) > 0 then // avoid injection
          // cached='' means HTTP_NOTFOUND
          cached := ''
        else
        begin
          staticFileName := Utf8ToString(
            StringReplaceChars(rawFormat, '/', PathDelim));
          if cacheStatic in fPublishOptions then
          begin
            // retrieve and cache
            cached := fViews.GetStaticFile(staticFileName);
            cached := AddStaticCache(staticFileName, cached);
          end
          else
          begin
            // no cache
            staticFileName := fViews.ViewStaticFolder + staticFileName;
            Ctxt.ReturnFile(staticFileName, {handle304=}true, '', '', '',
              fStaticCacheControlMaxAge);
            exit;
          end;
        end;
    finally
      fCacheLocker.Leave;
    end;
    if cached = '' then
      Ctxt.Error('', HTTP_NOTFOUND, fStaticCacheControlMaxAge)
    else
    begin
      Split(cached, #10, content, cached);
      Ctxt.Returns(cached, HTTP_SUCCESS, HEADER_CONTENT_TYPE + content,
        {handle304=}true, false, fStaticCacheControlMaxAge);
    end;
  end
  else
  begin
    // 3. render regular page using proper viewer
    timer.Start;
    if IdemPropNameU(rawFormat, 'json') then
      rendererClass := TMvcRendererJson
    else
      rendererClass := TMvcRendererFromViews;
    renderer := rendererClass.Create(self);
    try
      if Ctxt.Method in [mGET, mPOST] then
      begin
        methodIndex := fApplication.fFactory.FindMethodIndex(rawMethodName);
        if methodIndex >= 0 then
        begin
          method := @fApplication.fFactory.Methods[methodIndex];
          inputContext := Ctxt.GetInputAsTDocVariant(
            JSON_OPTIONS_FAST_EXTENDED, method);
          if not VarIsEmpty(inputContext) then
            with _Safe(inputContext)^ do
            begin
              if (kind = dvObject) and
                 (Count > 0) then
                // try {"p.a1":5,"p.a2":"dfasdfa"} -> {"p":{"a1":5,"a2":"dfasdfa"}}
                if method^.ArgsInputValuesCount = 1 then
                  FlattenAsNestedObject(ShortStringToUtf8(
                    method^.Args[method^.ArgsInFirst].ParamName^));
              renderer.fInput := ToJson;
            end;
        end;
        renderer.ExecuteCommand(methodIndex);
      end
      else
        renderer.CommandError('notfound', true, HTTP_NOTFOUND);
      body := renderer.Output.Content;
      if viewHasGenerationTimeTag in renderer.fOutputFlags then
        body := StringReplaceAll(body, fViews.ViewGenerationTimeTag,
          ShortStringToAnsi7String(timer.Stop));
      Ctxt.Returns(body, renderer.Output.Status, renderer.Output.Header,
        {handle304=}true, {noerrorprocess=}true, {cachecontrol=}0,
        {hashwithouttime=}crc32cUtf8ToHex(renderer.Output.Content));
    finally
      renderer.Free;
    end;
  end;
end;

procedure TMvcRunOnRestServer.RunOnRestServerRoot(Ctxt: TRestServerUriContext);
begin
  InternalRunOnRestServer(Ctxt, Ctxt.Uri + '/' + Ctxt.UriBlobFieldName);
end;

procedure TMvcRunOnRestServer.RunOnRestServerSub(Ctxt: TRestServerUriContext);
begin
  if Ctxt.UriBlobFieldName = '' then
    Ctxt.Redirect(Ctxt.UriWithoutSignature + '/default')
  else
    InternalRunOnRestServer(Ctxt, Ctxt.UriBlobFieldName);
end;


{ TMvcRendererReturningData }

constructor TMvcRendererReturningData.Create(aRun: TMvcRunWithViews);
begin
  fRun := aRun;
  inherited Create(fRun.Application);
end;

procedure TMvcRendererReturningData.ExecuteCommand(aMethodIndex: integer);

  procedure SetOutputValue(const aValue: RawUtf8);
  begin
    fOutput.Status := HTTP_SUCCESS;
    Split(aValue, #0, fOutput.Header, RawUtf8(fOutput.Content));
  end;

  function RetrievedFromInputValues(const aKey: RawUtf8;
    const aInputValues: TSynNameValue): boolean;
  var
    i: PtrInt;
  begin
    i := aInputValues.Find(aKey);
    if (i >= 0) and
       (aInputValues.List[i].Value <> '') and
       (fCacheCurrentSec < cardinal(aInputValues.List[i].Tag)) then
    begin
      SetOutputValue(aInputValues.List[i].Value);
      result := true;
    end
    else
    begin
      fCacheCurrent := inputCache;
      fCacheCurrentInputValueKey := aKey;
      result := false;
    end;
  end;

var
  sessionID: integer;
label
  doRoot, doInput;
begin
  // first check if content can be retrieved from cache
  if not fCacheEnabled then
  begin
    inherited ExecuteCommand(aMethodIndex);
    exit;
  end;
  fCacheCurrent := noCache;
  fCacheCurrentSec := GetTickCount64 div 1000;
  fRun.fCacheLocker.Enter;
  try
    if cardinal(aMethodIndex) < cardinal(Length(fRun.fCache)) then
      with fRun.fCache[aMethodIndex] do
      begin
        case Policy of
          cacheRootIgnoringSession:
            if fInput = '' then
doRoot:       if (RootValue <> '') and
                 (fCacheCurrentSec < RootValueExpirationTime) then
              begin
                SetOutputValue(RootValue);
                exit;
              end
              else
                fCacheCurrent := rootCache;
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
              else if RetrievedFromInputValues(
                        UInt32ToUtf8(sessionID), InputValues) then
                exit;
            end;
          cacheWithParametersIgnoringSession:
doInput:    if fInput = '' then
              goto doRoot
            else if RetrievedFromInputValues(fInput, InputValues) then
              exit;
          cacheWithParametersIfSession:
            if fApplication.CurrentSession.Exists then
              goto doInput;
          cacheWithParametersIfNoSession:
            if not fApplication.CurrentSession.Exists then
              goto doInput;
        end;
      end;
  finally
    fRun.fCacheLocker.Leave; // ExecuteCommand() process should not be locked
  end;
  // compute the context and render the page using the corresponding View
  inherited ExecuteCommand(aMethodIndex);
  // update cache
  if fCacheCurrent <> noCache then
  try
    fRun.fCacheLocker.Enter;
    with fRun.fCache[aMethodIndex] do
    begin
      inc(fCacheCurrentSec, TimeOutSeconds);
      case fCacheCurrent of
        rootCache:
          if fOutput.Status = HTTP_SUCCESS then
          begin
            RootValue := fOutput.Header + #0 + fOutput.Content;
            RootValueExpirationTime := fCacheCurrentSec;
          end
          else
            RootValue := '';
        inputCache:
          if fOutput.Status = HTTP_SUCCESS then
            InputValues.Add(fCacheCurrentInputValueKey, fOutput.Header + #0 +
              fOutput.Content, fCacheCurrentSec)
          else
            InputValues.Add(fCacheCurrentInputValueKey, '');
      end;
    end;
  finally
    fRun.fCacheLocker.Leave;
  end;
end;

function TMvcRendererReturningData.Redirects(const action: TMvcAction): boolean;
begin
  fOutput.Header := 'Location: ' + UrlEncodeJsonObject(action.RedirectToMethodName,
    action.RedirectToMethodParameters, ['main']);
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


{ TMvcApplication }

procedure TMvcApplication.Start(aRestModel: TRest; aInterface: PRttiInfo);
var
  m: PtrInt;
  entry: PInterfaceEntry;
begin
  fLocker := TAutoLocker.Create;
  fRestModel := aRestModel;
  fFactory := TInterfaceFactory.Get(aInterface);
  fFactoryErrorIndex := fFactory.FindMethodIndex('Error');
  if fFactoryErrorIndex < 0 then
    raise EMvcException.CreateUtf8(
      '% does not implement the IMvcApplication.Error() method',
      [aInterface.RawName]);
  entry := GetInterfaceEntry(fFactory.InterfaceIID);
  if entry = nil then
    raise EMvcException.CreateUtf8(
      '%.Start(%): this class should implement %',
      [self, aRestModel, fFactory.InterfaceTypeInfo^.RawName]);
  fFactoryEntry := PAnsiChar(self) + entry^.IOffset;
  for m := 0 to fFactory.MethodsCount - 1 do
    if not MethodHasView(fFactory.Methods[m]) then
      with fFactory.Methods[m] do
        if ArgsOutFirst <> ArgsResultIndex then
          raise EMvcException.CreateUtf8(
            '%.Start(%): %.% var/out param not allowed with TMvcAction result',
            [self, aRestModel, fFactory.InterfaceTypeInfo^.RawName, URI])
        else
          // maps TMvcAction in TMvcApplication.RunOnRestServer
          ArgsResultIsServiceCustomAnswer := true;
  FlushAnyCache;
end;

destructor TMvcApplication.Destroy;
begin
  inherited;
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
  GotoView(Action, 'Error', ['Msg', StatusCodeToErrorMsg(ErrorCode)], ErrorCode);
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
  FreeAndNil(fSession);
  fSession := Value;
end;

procedure TMvcApplication.GetViewInfo(MethodIndex: integer; out info: variant);
begin
  if MethodIndex >= 0 then
    info := _ObjFast(['pageName', fFactory.Methods[MethodIndex].Uri])
  else
    info := _ObjFast([]);
end;

procedure TMvcApplication.GetMvcInfo(out info: variant);
begin
  info := _ObjFast(['name', fFactory.InterfaceTypeInfo^.RawName,
                    'mORMot', SYNOPSE_FRAMEWORK_VERSION,
                    'root', RestModel.Model.Root,
                    'methods', ContextFromMethods(fFactory)]);
end;

procedure TMvcApplication.FlushAnyCache;
begin
  if fMainRunner <> nil then
    fMainRunner.NotifyContentChanged;
end;


initialization
  assert(sizeof(TMvcAction) = sizeof(TServiceCustomAnswer));

end.

