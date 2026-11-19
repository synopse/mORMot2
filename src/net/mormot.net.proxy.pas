/// HTTP Server With Proxy and Cache
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.proxy;

{
  *****************************************************************************

   HTTP Server With Proxy and Cache
   - FastCGI Asynchronous Client Connections
   - High Level Web Server Definitions
   - THttpProxyServer HTTP Server With Proxy and Cache

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
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.datetime,
  mormot.core.threads,
  mormot.core.search,
  mormot.core.log,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.zip,
  mormot.crypt.core,
  mormot.crypt.secure,
  mormot.net.sock,
  mormot.net.http,
  mormot.net.client,
  mormot.net.server,
  mormot.net.async;


{ ******************** FastCGI Asynchronous Client Connections }

const
  /// the only FastCGI protocol version, as stored in TFastCgiHeader.Version
  FCGI_VERSION_1 = 1;
  /// the maximum content length of a single FastCGI record
  FCGI_MAXCONTENT = 65535;

type
  /// the FastCGI record types, as stored in TFastCgiHeader.RecType
  TFastCgiRecordType = (
    frtNone,
    frtBeginRequest,
    frtAbortRequest,
    frtEndRequest,
    frtParams,
    frtStdIn,
    frtStdOut,
    frtStdErr,
    frtData,
    frtGetValues,
    frtGetValuesResult,
    frtUnknownType);

  /// the 8 bytes header of each FastCGI record
  // - RequestID and ContentLen are stored in network (big-endian) order
  TFastCgiHeader = packed record
    Version: byte;
    RecType: TFastCgiRecordType;
    RequestID: word;
    ContentLen: word;
    PaddingLen: byte;
    Reserved: byte;
  end;
  PFastCgiHeader = ^TFastCgiHeader;

  /// decode the FastCGI records sent back by a responder application
  {$ifdef USERECORDWITHMETHODS}
  TFastCgiResponse = record
  {$else}
  TFastCgiResponse = object
  {$endif USERECORDWITHMETHODS}
  public
    /// the frtStdOut content received so far, i.e. CGI headers and body
    // - is appended by each record, as THttpRequestContext does for chunks,
    // then FastCgiParseOutput() makes it the HTTP response body with no copy
    Output: RawByteString;
    /// the frtStdErr content received so far
    Error: RawUtf8;
    /// the application exit code, as received with frtEndRequest
    AppStatus: cardinal;
    /// the protocol status, as received with frtEndRequest
    // - 0 means FCGI_REQUEST_COMPLETE
    ProtocolStatus: byte;
    /// true once frtEndRequest has been received
    Done: boolean;
    /// reset all fields, before a new request
    procedure Init;
    /// decode some received data
    // - returns the number of bytes consumed, as whole records only: the
    // caller should keep any remaining bytes for the next call
    // - returns -1 on protocol error
    function Parse(P: PAnsiChar; Len: PtrInt): PtrInt;
  end;

/// append one name/value pair to a FastCGI frtParams content
procedure FastCgiAppendParam(var Dest: TSynTempAdder;
  Name: PUtf8Char; NameLen: PtrInt; Value: PUtf8Char; ValueLen: PtrInt); overload;

/// append one name/value pair to a FastCGI frtParams content
procedure FastCgiAppendParam(var Dest: TSynTempAdder;
  const Name, Value: RawUtf8); overload;

/// append a whole FastCGI stream, i.e. frtParams or frtStdIn
// - the content is split into records of FCGI_MAXCONTENT bytes at most,
// and is always followed by the void record which ends the stream
procedure FastCgiAppendStream(var Dest: TSynTempAdder;
  RecType: TFastCgiRecordType; Content: PAnsiChar; ContentLen: PtrInt);

/// append a whole FastCGI responder request
// - as frtBeginRequest, then the frtParams and frtStdIn streams
// - with RequestID = 1 and no FCGI_KEEP_CONN flag, i.e. the application
// should close the connection once it has sent its response
procedure FastCgiAppendRequest(var Dest: TSynTempAdder;
  Params: PAnsiChar; ParamsLen: PtrInt; const Body: RawByteString);

/// append the CGI/1.1 variables of a HTTP request to a FastCGI frtParams content
// - Path/PathLen is the URI path below the UriPrefix route, with no ?query
// - the script is located as the Path up to its first segment with a dot -
// or ending with ScriptExt, e.g. '.php', if supplied - then set as
// SCRIPT_NAME below UriPrefix and as SCRIPT_FILENAME below Root: any
// following segment is the PATH_INFO
// - returns false if Path has no script, or is unsafe, i.e. is absolute or
// contains a '..' segment, a ':' or a '\'
// - with Wsgi=true, SCRIPT_NAME is UriPrefix and PATH_INFO is the
// decoded path under that prefix; no physical script is required
// - a "Proxy:" header is never transmitted as HTTP_PROXY, and any header
// with an underscore in its name is ignored
function FastCgiAppendParams(var Dest: TSynTempAdder;
  Ctxt: THttpServerRequestAbstract; const Root, UriPrefix, ServerPort: RawUtf8;
  Path: PUtf8Char; PathLen: PtrInt; Wsgi: boolean = false;
  const ScriptExt: RawUtf8 = ''): boolean;

/// parse the CGI headers of a FastCGI frtStdOut content
// - header lines could end with LF or CRLF
// - Status is set from any "Status:" header, or as 302 with a "Location:"
// header, or as 200
// - "Content-Type:" is returned as ContentType, "Content-Length:" is ignored,
// and all other headers are returned CRLF separated as Headers
// - on success, the headers are removed from Output, which becomes the body
// - returns false if the headers are not terminated by a void line, or if
// a header line is invalid
function FastCgiParseOutput(var Output: RawByteString; out Status: integer;
  var ContentType, Headers: RawUtf8): boolean;

type
  TFastCgiAsyncConnections = class;

  /// one asynchronous connection to a FastCGI responder application
  // - handles a single request, then is closed by the application
  TFastCgiAsyncConnection = class(TAsyncConnection)
  protected
    fHttpHandle: PtrUInt; // = TConnectionAsyncHandle = 0 once answered
    fResponse: TFastCgiResponse;
    procedure AfterCreate; override;
    function OnRead: TPollAsyncSocketOnReadWrite; override;
    procedure OnClose; override;
    function OnLastOperationIdle(nowsec: TAsyncConnectionSec): boolean; override;
    procedure HttpResponse(Status: integer);
  end;

  /// asynchronous connections to a FastCGI responder application, e.g. php-fpm
  // - each HTTP request has its own connection, and is answered via the
  // AsyncResponse() method of the associated HTTP server
  TFastCgiAsyncConnections = class(TAsyncClient)
  protected
    fHttpServer: THttpServerGeneric;
    fLayer: TNetLayer;
    fTimeoutSec: integer;
  public
    /// initialize the connections to a FastCGI application
    // - aUri is e.g. 'fcgi://127.0.0.1:9000' or 'fcgi://unix:/run/php-fpm.sock:'
    // - aTimeoutSec is how long an application may take with no data sent back
    constructor Create(aHttpServer: THttpServerGeneric; const aUri: TUri;
      aTimeoutSec: integer; aLog: TSynLogClass); reintroduce;
    /// connect to the application, then asynchronously send a request
    // - the response will be sent back to aHttpHandle via AsyncResponse()
    // - returns false if the application could not be reached: the caller
    // should then answer to the HTTP client by itself
    function Send(aHttpHandle: TConnectionAsyncHandle;
      aRequest: pointer; aRequestLen: PtrInt): boolean;
    /// how many seconds an application may take with no data sent back
    function GetLastOperationIdleSeconds: cardinal; override;
    /// should be called every second, e.g. from the HTTP server idle process
    // - on Windows IOCP, a TAsyncClient has no idle process of its own, which
    // is needed to detect any timeout and to release the closed connections
    procedure Idle(NowTix: Int64);
  end;


{ ******************** High Level Web Server Definitions }

type
  /// most used tuning options for a modern and safe HTTP/HTTPS Server
  // - over the years, a lot of expectations where added to the basic behavior
  // of a HTTP server, e.g. for better security or interoperability: we define
  // a set of well-known behaviors
  // - flags used e.g. by TWebServerLocal, or mORMot 2 Boilerplate project
  TWebServerBehavior = (

    /// Allow cross-origin requests
    // - see https://enable-cors.org https://www.w3.org/TR/cors
    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Access_control_CORS
    // - Warning: Do not use this without understanding the consequences.
    // This will permit access from any other website.
    // Instead of using this file, consider using a specific rule such as
    // allowing access based on (sub)domain: "subdomain.example.com"
    wsbAllowCrossOrigin,

    /// Send the CORS header for images when browsers request it
    // - see
    // https://developer.mozilla.org/en-US/docs/Web/HTML/CORS_enabled_image
    // https://blog.chromium.org/2011/07/using-cross-domain-images-in-webgl-and.html
    // - use TWebServerLocal.FileTypesImage to specify the actual file types
    wsbAllowCrossOriginImages,

    /// Allow cross-origin access to web fonts
    // - see https://developers.google.com/fonts/docs/troubleshooting
    // - use TWebServerLocal.FileTypesFont to specify file types
    wsbAllowCrossOriginFonts,

    /// Allow cross-origin access to the timing information for all resources
    // - If a resource isn't served with a 'Timing-Allow-Origin' header that would
    // allow its timing information to be shared with the document, some of the
    // attributes of the 'PerformanceResourceTiming' object will be set to zero.
    // - see https://www.w3.org/TR/resource-timing/
    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Timing-Allow-Origin
    // https://www.stevesouders.com/blog/2014/08/21/resource-timing-practical-tips/
    wsbAllowCrossOriginTiming,

    /// Set content for HTTP 400 "Bad Request" response code equals to '/404'
    wsbDelegateBadRequestTo404,

    /// Set content for HTTP 401 "Unauthorized" response code equals to '/404'
    wsbDelegateUnauthorizedTo404,

    /// Set content for HTTP 403 "Forbidden" response code equals to '/404'
    wsbDelegateForbiddenTo404,

    /// Set content for HTTP 404 "Not Found" response code equals to '/404'
    wsbDelegateNotFoundTo404,

    /// Set content for HTTP 405 "Not Allowed" response code equals to '/404'
    wsbDelegateNotAllowedTo404,

    /// Set content for HTTP 406 "Not Acceptable" response code equals to '/404'
    wsbDelegateNotAcceptableTo404,

    /// Set Internet Explorer XUA Document mode
    // - Force Internet Explorer 8/9/10 to render pages in the highest mode
    // available in various cases when it may not.
    // - Warning: since Internet Explorer 11, document modes are deprecated.
    // If your business still relies on older web apps and services that were
    // designed for older versions of Internet Explorer, you might want to
    // consider enabling 'Enterprise Mode' throughout your company.
    // - see https://hsivonen.fi/doctype/#ie8
    // https://msdn.microsoft.com/en-us/library/ie/bg182625.aspx#docmode
    // https://blogs.msdn.microsoft.com/ie/2014/04/02/stay-up-to-date-with-enterprise-mode-for-internet-explorer-11/
    // https://msdn.microsoft.com/en-us/library/ff955275.aspx
    wsbSetXuaCompatible,

    // Serve resources with the proper media types (f.k.a. MIME types)
    // - use TWebServerGlobal.ForceMimeTypes/ForceMimeTypesExt to set the MIME types
    // - see http://www.iana.org/assignments/media-types
    // https://svn.apache.org/repos/asf/httpd/httpd/trunk/docs/conf/mime.types
    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types
    wsbForceMimeType,

    // Serve all resources labeled as 'text/html' or 'text/plain'
    // with the media type 'charset' parameter set to 'UTF-8'
    wsbForceTextUtf8Charset,

    /// Serve the specified file types with the media type 'charset' parameter
    // set to 'UTF-8'
    // - use TWebServerLocal.FileTypesRequiredCharSet to setup file types
    wsbForceUtf8Charset,

    /// Redirect from 'http://' to the 'https://' version of the URL
    wsbForceHttps,

    /// Forcing 'https://' in the context of Let's Encrypt (ACME)
    // - If you're using cPanel AutoSSL or the Let's Encrypt webroot method it
    // will fail to validate the certificate if validation requests are
    // redirected to HTTPS. Turn on the condition(s) you need.
    // - see https://www.iana.org/assignments/well-known-uris/well-known-uris.xhtml
    //  https://tools.ietf.org/html/draft-ietf-acme-acme-12
    // $ /.well-known/acme-challenge/
    // $ /.well-known/cpanel-dcv/[\w-]+$
    // $ /.well-known/pki-validation/[A-F0-9]{32}\.txt(?:\ Comodo\ DCV)?$
    // - The next simplified patterns are used:
    // $ /.well-known/acme-challenge/*
    // $ /.well-known/cpanel-dcv/*
    // $ /.well-known/pki-validation/*
    wsbForceHttpsExceptLetsEncrypt,

    /// Protect website against clickjacking
    // - The example below sends the 'X-Frame-Options' response header with the
    // value 'DENY', informing browsers not to display the content of the web
    // page in any frame.
    // - This might not be the best setting for everyone. You should read about
    // the other two possible values the 'X-Frame-Options' header field can
    // have: 'SAMEORIGIN' and 'ALLOW-FROM'.
    // https://tools.ietf.org/html/rfc7034#section-2.1.
    // - Keep in mind that while you could send the 'X-Frame-Options' header for
    // all of your website's pages, this has the potential downside that it
    // forbids even non-malicious framing of your content (e.g.: when users
    // visit your website using a Google Image Search results page).
    // - Nonetheless, you should ensure that you send the 'X-Frame-Options' header
    // for all pages that allow a user to make a state-changing operation
    // (e.g: pages that contain one-click purchase links, checkout or
    // bank-transfer confirmation pages, pages that make permanent configuration
    // changes, etc.).
    // - Sending the 'X-Frame-Options' header can also protect your website
    // against more than just clickjacking attacks.
    // - see https://cure53.de/xfo-clickjacking.pdf.
    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Frame-Options
    // https://tools.ietf.org/html/rfc7034
    // https://blogs.msdn.microsoft.com/ieinternals/2010/03/30/combating-clickjacking-with-x-frame-options/
    // https://www.owasp.org/index.php/Clickjacking
    wsbSetXFrameOptions,

    /// Block access to all hidden files and directories except for the
    // visible content from within the '/.well-known/' hidden directory
    // - These types of files usually contain user preferences or the preserved
    // state of a utility, and can include rather private places like, for
    // example, the '.git' or '.svn' directories.
    // - The '/.well-known/' directory represents the standard (RFC 5785) path
    // prefix for "well-known locations" (e.g.: '/.well-known/manifest.json',
    // '/.well-known/keybase.txt'), and therefore, access to its visible content
    // should not be blocked.
    // - see https://www.mnot.net/blog/2010/04/07/well-known
    // https://tools.ietf.org/html/rfc5785
    wsbDelegateHidden,

    /// Block access to files that can expose sensitive information
    // - By default, block access to backup and source files that may be left by
    // some text editors and can pose a security risk when anyone has access to
    // them. see https://feross.org/cmsploit/
    //  - Use TWebServerLocal.FileTypesBlocked to specify file types
    // that might end up on your production server and can expose sensitive
    // information about your website. These files may include:
    // configuration files, files that contain metadata about the project
    // (e.g.: project dependencies, build scripts, etc.).
    // - use TWebServerLocal.FileTypesBlocked to specify file types
    // - this option also blocks any URL paths ended with '~' or '#'
    wsbDelegateBlocked,

    /// Prevent some browsers from MIME-sniffing the response
    // - This reduces exposure to drive-by download attacks and cross-origin data
    // leaks, and should be left uncommented, especially if the server is
    // serving user-uploaded content or content that could potentially be
    // treated as executable by the browser.
    // - see https://mimesniff.spec.whatwg.org/
    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Content-Type-Options
    // https://blogs.msdn.microsoft.com/ie/2008/07/02/ie8-security-part-v-comprehensive-protection/
    wsbPreventMimeSniffing,

    /// Protect website reflected Cross-Site Scripting (XSS) attacks
    // - Try to re-enable the cross-site scripting (XSS) filter built into
    // most web browsers.
    // - The filter is usually enabled by default, but in some cases, it may
    // be disabled by the user. However, in Internet Explorer, for example,
    // it can be re-enabled just by sending the  'X-XSS-Protection' header
    // with the value of '1'.
    // - Prevent web browsers from rendering the web page if a potential
    // reflected (a.k.a non-persistent) XSS attack is detected by the filter.
    // - By default, if the filter is enabled and browsers detect a reflected
    // XSS attack, they will attempt to block the attack by making the
    // smallest possible modifications to the returned web page.
    // - Unfortunately, in some browsers (e.g.: Internet Explorer), this
    // default behavior may allow the XSS filter to be exploited. Therefore,
    // it's better to inform browsers to prevent the rendering of the page
    // altogether, instead of attempting to modify it.
    // - warning: Do not rely on the XSS filter to prevent XSS attacks! Ensure that you
    // are taking all possible measures to prevent XSS attacks, the most
    // obvious being: validating and sanitizing your website's inputs.
    // - see https://hackademix.net/2009/11/21/ies-xss-filter-creates-xss-vulnerabilities
    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-XSS-Protection
    // https://blogs.msdn.microsoft.com/ie/2008/07/02/ie8-security-part-iv-the-xss-filter/
    // https://blogs.msdn.microsoft.com/ieinternals/2011/01/31/controlling-the-xss-filter/
    // https://www.owasp.org/index.php/Cross-site_Scripting_%28XSS%29
    // - use TWebServerLocal.FileTypesAsset to exclude some file types
    wsbEnableXssFilter,

    /// Set a strict Referrer Policy to mitigate information leakage.
    // - The 'Referrer-Policy' header is included in responses for resources
    // that are able to request (or navigate to) other resources.
    // - This includes the commonly used resource types:
    // HTML, CSS, XML/SVG, PDF documents, scripts and workers.
    // - To prevent referrer leakage entirely, specify the 'no-referrer' value
    // instead. Note that the effect could impact analytics metrics negatively.
    // - // To check your Referrer Policy, you can use an online service, such as:
    // https://securityheaders.com/
    // https://observatory.mozilla.org/
    // https://scotthelme.co.uk/a-new-security-header-referrer-policy/
    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Referrer-Policy
    // - use TWebServerLocal.ReferrerPolicy and
    // TWebServerLocal.ReferrerPolicyContentTypes properties
    wsbEnableReferrerPolicy,

    /// Prevent the HTTP Server from responding to 'TRACE' HTTP requests
    // - The TRACE method, while seemingly harmless, can be successfully leveraged
    // in some scenarios to steal legitimate users' credentials.
    // - Modern browsers now prevent TRACE requests being made via JavaScript,
    // however, other ways of sending TRACE requests with browsers have been
    // discovered, such as using Java.
    // - see https://tools.ietf.org/html/rfc7231#section-4.3.8
    // https://www.owasp.org/index.php/Cross_Site_Tracing
    // https://www.owasp.org/index.php/Test_HTTP_Methods_(OTG-CONFIG-006)
    // https://httpd.apache.org/docs/current/mod/core.html#traceenable
    wsbDisableTraceMethod,

    /// Remove the 'X-Powered-By' response header that:
    // - is set by some frameworks and server-side languages (e.g.: ASP.NET, PHP),
    // and its value contains information about them (e.g.: their name, version
    // number)
    // - doesn't provide any value to users, contributes to header bloat, and in
    // some cases, the information it provides can expose vulnerabilities
    // - If you can, you should disable the 'X-Powered-By' header from the
    //     language/framework level (e.g.: for PHP, you can do that by setting
    //     'expose_php = off' in 'php.ini').
    // - see https://php.net/manual/en/ini.core.php#ini.expose-php
    wsbDeleteXPoweredBy,

    /// Force compression for mangled 'Accept-Encoding' request headers
    // - see https://calendar.perfplanet.com/2010/pushing-beyond-gzipping
    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Accept-Encoding
    // - use TWebServerLocal.MangledEncodingHeaders and
    // TWebServerLocal.MangledEncodingHeaderValues properties
    wsbFixMangledAcceptEncoding,

    /// Map the specified filename extensions to the GZip encoding type
    // - to let the HTTP Server serve the file types with the appropriate
    // 'Content-Encoding' response header (do note that this will NOT make
    // HTTP Server compress them!).
    // - If these files types would be served without an appropriate
    // 'Content-Encoding' response header, client applications (e.g.: browsers)
    // wouldn't know that they first need to uncompress the response, and thus,
    // wouldn't be able to understand the content.
    // - see https://httpd.apache.org/docs/current/mod/mod_mime.html#addencoding
    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Encoding
    // - use TWebServerLocal.FileTypesForceGZipHeader to setup file types
    wsbForceGZipHeader,

    /// Allow static assets to be cached by proxy servers
    wsbSetCachePublic,

    /// Allow static assets to be cached only by browser,
    // but not by intermediate proxy servers
    wsbSetCachePrivate,

    /// disable Content transformation
    // - Prevent intermediate caches or proxies (such as those used by mobile
    // network providers) and browsers data-saving features from modifying
    // the website's content using the 'cache-control: no-transform' directive.
    // - see https://tools.ietf.org/html/rfc7234#section-5.2.2.4
    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Cache-Control
    // - warning: Carefully consider the impact on your visitors before disabling
    // content transformation. These transformations are performed to
    // improve the experience for data- and cost-constrained users
    // (e.g. users on a 2G connection).
    // - You can test the effects of content transformation applied by
    // Google's Lite Mode by visiting:
    // https://googleweblight.com/i?u=https://www.example.com
    // https://support.google.com/webmasters/answer/6211428
    // https://developers.google.com/speed/pagespeed/module/configuration#notransform
    wsbSetCacheNoTransform,

    /// Allow static assets to be validated with server before return cached copy
    wsbSetCacheNoCache,

    /// Allow static assets not to be cached
    wsbSetCacheNoStore,

    /// Allow static assets to be cached strictly following the server rules
    wsbSetCacheMustRevalidate,

    /// Add 'max-age' value based on content-type/expires mapping
    // - i.e. serve resources with a far-future expiration date.
    // - warning: If you don't control versioning with filename-based cache
    // busting, you should consider lowering the cache times to something like
    // one week.
    // - see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Expires
    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Cache-Control
    // - use TWebServerLocal.Expires options to control expirations
    wsbSetCacheMaxAge,

    /// Use ETag / If-None-Match caching
    // - see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/ETag
    // https://developer.yahoo.com/performance/rules.html#etags
    // https://tools.ietf.org/html/rfc7232#section-2.3
    wsbEnableCacheByETag,

    /// Use Last-Modified/If-Modified-Since caching
    // - see https://developer.yahoo.com/performance/rules.html#etags
    // https://tools.ietf.org/html/rfc7232#section-2.3
    wsbEnableCacheByLastModified,

    /// Serve resources with a far-future expiration date
    // - If you don't control versioning with filename-based cache busting, you
    // should consider lowering the cache times to something like one week.
    // - see https://httpd.apache.org/docs/current/mod/mod_expires.html
    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Cache-Control
    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Expires
    // - use TWebServerLocal.Expires property
    wsbSetExpires,

    /// Enables filename-based cache busting
    // - i.e. removes all query path of the URL '/style.css?v231' to '/style.css'
    wsbEnableCacheBusting,

    /// Filename-based cache busting
    // - i.e. removes infix query path of the URL '/style.123456.css' to '/style.css'
    // - if you're not using a build process to manage your filename version
    // revving, you might want to consider enabling the following directives.
    // - to understand why this is important and even a better solution than
    // using something like '*.css?v231', please see:
    // https://www.stevesouders.com/blog/2008/08/23/revving-filenames-dont-use-querystring/
    wsbEnableCacheBustingBeforeExt,

    /// Remove 'Server-InternalState' HTTP header
    wsbDeleteServerInternalState,

    // Delete content generation for '' and '/' URLs to '/index.html'
    wsbDelegateRootToIndex,

    /// Instead of index.html rendering the inherited "/Default" URL will be called
    // - allows to inject custom IMVCApplication.Default() interface method
    wsbDelegateIndexToInheritedDefault,

    /// Instead of 404.html rendering the inherited "/404" URL will be called
    // - allows to inject custom IMVCApplication._404() interface method
    wsbDelegate404ToInherited_404,

    /// Add 'Vary: Accept-Encoding' header for assets with GZip/Brotli encoding
    wsbVaryAcceptEncoding
  );

  /// set of tuning options for a modern and safe HTTP/HTTPS Web Server
  // - flags used e.g. by TWebServerLocal, or the mORMot 2 Boilerplate project
  TWebServerBehaviors = set of TWebServerBehavior;

  /// Suppressing or forcing the 'www.' host prefix at the beginning of URLs
  // - The same content should never be available under two different URLs,
  // especially not with and without 'www.' at the beginning.
  // This can cause SEO problems (duplicate content), and therefore, you should
  // choose one of the alternatives and redirect the other one.
  // - The rule assumes by default that both HTTP and HTTPS environments are
  // available for redirection. If your SSL certificate could not handle one
  // of the domains used during redirection, you should turn the condition on.
  // - wsrOff: won't suppress or force 'www.' at the beginning of URLs
  // - wsrSuppress: suppress the 'www.' at the beginning of URLs,
  // redirecting e.g. www.example.com into example.com
  // - wsrForce: forces the 'www.' at the beginning of URLs,
  // redirecting e.g. example.com into www.example.com
  // - Be aware that wsrForce might not be a good idea if you use "real"
  // subdomains for certain parts of your website
  TWebServerRewrite = (
    wsrOff,
    wsrSuppress,
    wsrForce);

  /// how to implement HTTP Strict Transport Security (HSTS) redirection
  // - If a user types 'example.com' in their browser, even if the server redirects
  // them to the secure version of the website, that still leaves a window of
  // opportunity (the initial HTTP connection) for an attacker to downgrade or
  // redirect the request.
  // - The HSTS header ensures that a browser only connects to your server
  // via HTTPS, regardless of what the users type in the browser's address bar.
  // - Be aware that Strict Transport Security is not revokable and you must
  // ensure being able to serve the site over HTTPS for the duration you've
  // specified in the 'max-age' directive. When you don't have a valid TLS
  // connection anymore (e.g. due to an expired TLS certificate) your visitors
  // will see a nasty error message even when attempting to connect over HTTP.
  // - wshOff: do not provide any HSTS header
  // - wshOn: add regular 'max-age=31536000' HSTS header value
  // - wshIncludeSubDomains: add 'max-age=31536000; includeSubDomains' HSTS header
  // - wshIncludeSubDomainsPreload: add
  // 'max-age=31536000; includeSubDomains; preload' HSTS header
  // - see https://tools.ietf.org/html/rfc6797#section-6.1
  // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Strict-Transport-Security
  // https://www.html5rocks.com/en/tutorials/security/transport-layer-security/
  // https://blogs.msdn.microsoft.com/ieinternals/2014/08/18/strict-transport-security/
  // https://hstspreload.org/
  TWebServerHsts = (
    wshOff,
    wshOn,
    wshIncludeSubDomains,
    wshIncludeSubDomainsPreload);

  /// how to implement DNS Prefetch Control
  // - DNS prefetching is a feature by which browsers proactively perform
  // domain name resolution on both links that the user may choose to follow
  // as well as URLs for items referenced by the document, including images,
  // CSS, JavaScript, and so forth.
  // - This prefetching is performed in the background, so that the DNS is
  // likely to have been resolved by the time the referenced items are needed.
  // This reduces latency when the user clicks a link.
  // - wsdPrefetchNone: do not add 'X-DNS-Prefetch-Control' header
  // - wsdPrefetchOff: turn off DNS Prefetch
  // - wsdPrefetchOn: turn on DNS Prefetch (default)
  // - see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-DNS-Prefetch-Control
  TWebServerDpc = (
    wsdPrefetchNone,
    wsdPrefetchOff,
    wsdPrefetchOn);

  /// the known encoding/compression schemes for a Web Server
  // - THttpServerGeneric.RegisterCompress can support any kind of compression
  // using a callback function; but we define here the most used encodings in
  // current browsers, e.g. to be able to cache content or hashes at runtime
  // - wseIdentity means no compression
  // - wseGZip will use the well-known GZip encoding (via libdeflate if available)
  // - wseBrotli is reserved for future use (e.g. mORMot 2 Boilerplate project)
  TWebServerEncoding = (
    wseIdentity,
    wseGZip,
    wseBrotli);
  /// the known encoding/compression schemes for a Web Server
  TWebServerEncodings = set of TWebServerEncoding;

  /// define the security parameters for a Web Server for a given route
  // - is defined as a record so that it could be assigned and processed per URI
  // - values will be assigned by reference, so with minimal memory consumption
  // - FileTypes*[] are ordered TWordDynArray indexes to TWebServerGlobal.MimeType[]
  TWebServerLocal = record
    /// most used tuning options for a modern and safe HTTP/HTTPS Server
    Behaviors: TWebServerBehaviors;
    /// how to handle the 'www.' host prefix at the beginning of URLs
    Rewrite: TWebServerRewrite;
    /// how to implement HTTP Strict Transport Security (HSTS) redirection
    Hsts: TWebServerHsts;
    /// how to implement DNS Prefetch Control
    Dpc: TWebServerDpc;
    /// file extension ordered indexes for wsbAllowCrossOriginImages behavior
    FileTypesImage: TWordDynArray;
    /// file extension ordered indexes for wsbAllowCrossOriginFonts behavior
    FileTypesFont: TWordDynArray;
    /// file extension ordered indexes for wsbForceUtf8Charset behavior
    FileTypesRequiredCharSet: TWordDynArray;
    /// file extension ordered indexes for wsbDelegateBlocked behavior
    FileTypesBlocked: TWordDynArray;
    /// file extension ordered indexes for wsbForceGZipHeader behavior
    FileTypesForceGZipHeader: TWordDynArray;
    /// supported Referrer Policy for wsbEnableReferrerPolicy behavior
    ReferrerPolicy: RawUtf8;
    /// content types for wsbEnableReferrerPolicy behavior
    ReferrerPolicyContentTypes: TWordDynArray;
    /// headers for wsbFixMangledAcceptEncoding behavior
    MangledEncodingHeaders: TRawUtf8DynArray;
    /// values for wsbFixMangledAcceptEncoding behavior
    MangledEncodingHeaderValues: TRawUtf8DynArray;
    /// expiration for wsbSetCacheMaxAge and wsbSetExpires behaviors
    ExpiresDefault: integer;
    /// content-types for wsbSetCacheMaxAge and wsbSetExpires behaviors
    ExpiresContentTypes: TRawUtf8DynArray;
    /// values for wsbSetCacheMaxAge and wsbSetExpires behaviors
    ExpiresValues: TCardinalDynArray;
  end;

  /// information about a given file extension of a supported mime type
  // - as stored in TWebServerGlobal.MimeType[]
  TWebServerMimeType = record
    /// file extensions of supported mime types, excluding initial '.' character
    Extension: RawUtf8;
    /// specific mime type for wsbForceMimeType behavior
    ForceMimeType: RawUtf8;
  end;
  /// information about all known file extensions of supported mime types
  // - as stored in TWebServerGlobal.MimeType[]
  TWebServerMimeTypeDynArray = array of TWebServerMimeType;

  /// define the security parameters common to all Web Servers
  TWebServerGlobal = class(TSynPersistent)
  protected
    fMimeType: TWebServerMimeTypeDynArray;
    fMimeTypeCount: integer;
    fMimeTypes: TDynArrayHashed;
  public
    /// initialize this instance
    constructor Create; override;
    /// quickly returns the index of a MimeType[] entry from this file extension
    function FindMimeType(Extension: PUtf8Char): PtrInt;
      {$ifdef HASINLINE} inline; {$endif}
    /// search then add if not existing of a file extension
    // - returns the index of the matching MimeType[] entry
    function AddIfNeeded(const Extension: RawUtf8): PtrInt;
    /// search (or add) a file extension, then insert its index in a
    // TWordDynArray sorted array
    // - used e.g. to fill TWebServerLocal behavior-enabled properties
    function AddToIndexes(var Indexes: TSortedWordArray;
      const Extension: RawUtf8): PtrInt;
    /// process a request using the specified local parameters
    // - should return 0 to continue processing, or a result code to stop any
    // further response computation, and return the current output state
    function Request(Context: THttpServerRequestAbstract;
      const Local: TWebServerLocal): cardinal;
    /// file extensions of supported mime types for wsbForceMimeType behavior
    // - see also https://github.com/jshttp/mime-db/blob/master/db.json
    // - TWebServerLocal.FileTypes*[] TWordDynArray contains indexes to this
    // - warning: length(MimeType) is the capacity - use MimeTypeCount
    property MimeType: TWebServerMimeTypeDynArray
      read fMimeType;
    /// how many file extensions are currently known
    property MimeTypeCount: integer
      read fMimeTypeCount;
  end;




{ ******************** THttpProxyServer HTTP Server With Proxy and Cache }

type
  /// the result of THttpProxyMem.FromUri
  THttpProxyCacheKind = set of (
    pckIgnore,
    pckForce);

  /// define the caching settings of content for THttpProxyServer
  // - set the memory cache settings if used as exact THttpProxyMem class
  THttpProxyMem = class(TSynPersistent)
  protected
    fMaxSizeKB: integer;
    fTimeoutSec: integer;
    fIgnoreCsv, fForceCsv: RawUtf8;
    fIgnore, fForce: TUriMatch; // parsed fIgnoreCsv, fForceCsv
  public
    /// setup the default values of this cache
    constructor Create; override;
    /// check the IgnoreCsv and ForceCSv properties against a given URI
    function FromUri(const uri: TUriMatchName): THttpProxyCacheKind;
  published
    /// size (in kilobytes) below which the file should be included in this cache
    // - default -1 will use the main THttpProxyServerSettings value
    property MaxSizeKB: integer
      read fMaxSizeKB write fMaxSizeKB default -1;
    /// how many seconds this file should remain in this cache
    // - default -1 will use the main THttpProxyServerSettings value
    property TimeoutSec: integer
      read fTimeoutSec write fTimeoutSec default -1;
    /// CSV list of GLOB file names to be excluded to this cache
    // - e.g. '*.changelog,*.tmp'
    property IgnoreCsv: RawUtf8
      read fIgnoreCsv write fIgnoreCsv;
    /// CSV list of GLOB file names to be included in this cache even if
    // its size does not match MaxSize
    // - e.g. '*.xml'
    property ForceCsv: RawUtf8
      read fForceCsv write fForceCsv;
  end;

  /// define disk cache settings of content for THttpProxyServer
  THttpProxyDisk = class(THttpProxyMem)
  protected
    fPath: TFileName;
  published
    /// default '' will use the main THttpProxyServerSettings value
    property Path: TFileName
      read fPath write fPath;
  end;

  THttpProxyUrl = class;

  /// THttpProxyUrl.OnRemoteClient callback signature
  // - could be used e.g. for interaction, or to customize TLS or proxy options
  TOnHttpProxyUrlClient = procedure(Sender: THttpProxyUrl; const Uri: TUri;
    var Options: THttpRequestExtendedOptions) of object;

  /// set of THttpProxyUrl.Settings.Options items, to customize a specific URI
  // - hpoNoSubFolder disable access to any sub-folder within this URI
  // - hpoNoFolderHtmlIndex disable the HTML index generation at folder level
  // - hpoPublishMd5/psoPublishSha1/psoPublishSha256 enable hash content
  // generation on server side with .md5/.sha1/.sha256 extension on a resource
  // - hpoDisable304 disable "if-none-match:" / "if-modified-since:" headers
  // default support as efficient 304 HTTP_NOTMODIFIED response
  // - hpoClientCacheSubFolder could be used with a lot of cached files, to
  // generate sub-folders following the first hash nibble (0..9/a..z)
  // - hpoClientHeadNoRefresh won't refresh the HEAD timeout when a resource
  // is accessed, so that the cache is always flushed after HttpHeadCacheSec
  // - hpoClientIgnoreTlsError will ignore any HTTPS issue
  // - hpoClientAlllowWinApi will be used for THttpProxyUrl.RemoteClientHead()
  // - hpoClientAllowRedirect enable 30x HTTP redirections (disabled by default)
  // - hpoClientNoCacheDirect disable caching of dynamic pages < HttpDirectGetKB
  // - hpoClientLowerCaseUri will force remote http://... to be in lowercase
  // - hpoClientNormalizeCaseHash to compute the local hash from uppercase URI
  // - hpoNoXProxyName will disable our custom 'X-Proxy-Name: xxxx' header
  // - hpoNoXCache will purge any X-Cache: X-Served-By: Via: Age: headers
  // - hpoFastCgiWsgi makes fcgi:// sources pass SCRIPT_NAME/PATH_INFO for
  // a WSGI application instead of locating a physical script file
  THttpProxyUrlOption = (
    hpoNoSubFolder,
    hpoNoFolderHtmlIndex,
    hpoDisableFolderHtmlIndexCache,
    hpoPublishMd5,
    hpoPublishSha1,
    hpoPublishSha256,
    hpoDisable304,
    hpoClientCacheSubFolder,
    hpoClientHeadNoRefresh,
    hpoClientIgnoreTlsError,
    hpoClientAlllowWinApi,
    hpoClientAllowRedirect,
    hpoClientNoCacheDirect,
    hpoClientLowerCaseUri,
    hpoClientNormalizeCaseHash,
    hpoNoXProxyName,
    hpoNoXCache,
    hpoFastCgiWsgi);
  /// store THttpProxyUrl.Settings options for a given URI
  THttpProxyUrlOptions = set of THttpProxyUrlOption;

  THttpProxyUrlSettings = class;

  /// callback function prototype for THttpProxyServerSettings.AddEvent()
  TOnHttpProxyServerRequest = function(Ctxt: THttpServerRequestAbstract;
    const Uri: TUriMatchName; Met: TUriRouterMethod;
    ProxyUrl: THttpProxyUrl): cardinal of object;

  /// define one URL content / THttpProxyUrl setting for THttpProxyServer
  THttpProxyUrlSettings = class(TSynAutoCreateFields)
  protected
    fUrl, fSource: RawUtf8;
    fDisabled: boolean;
    fOptions: THttpProxyUrlOptions;
    fMethods: TUriRouterMethods;
    fCacheControlMaxAgeSec: integer;
    fHttpKeepAlive, fHttpHeadCacheSec, fHttpDirectGetKB, fHttpLimitPerSecond: integer;
    fFastCgiTimeoutSec: integer;
    fFastCgiIndex: RawUtf8;
    fMemCache: THttpProxyMem; // owned as TSynAutoCreateFields
    fDiskCache: THttpProxyDisk;
    fRejectCsv: RawUtf8;
    fLocalFolder: TFileName;
    fOnRemoteClient: TOnHttpProxyUrlClient;
    fOnRequest: TOnHttpProxyServerRequest;
  public
    /// setup the default values of this URL
    constructor Create; override;
    /// allow to customize the processing options in a fluent calling interface
    function SetOptions(aOptions: THttpProxyUrlOptions): THttpProxyUrlSettings;
    /// optional event handler when a remote URI connection is instantiated
    property OnRemoteClient: TOnHttpProxyUrlClient
      read fOnRemoteClient write fOnRemoteClient;
    /// optional event handler when a callback is to be called
    property OnRequest: TOnHttpProxyServerRequest
      read fOnRequest write fOnRequest;
  published
    /// this source won't be processed if this property is set to true
    property Disabled: boolean
      read fDisabled write fDisabled;
    /// which methods are applied to the local Source folder or relayed to
    // the Remote server
    // - equals by default [urmGet, urmHead]
    property Methods: TUriRouterMethods
      read fMethods write fMethods;
    /// refined the process of this URI definition
    property Options: THttpProxyUrlOptions
      read fOptions write fOptions;
    /// the local URI prefix to route the main HTTP(S) server of this instance
    // - a typical value is e.g. 'debian' for 'http://ftp.debian.org/debian'
    property Url: RawUtf8
      read fUrl write fUrl;
    /// a local folder name or remote origin URL to ask
    // - if Source is a local folder (e.g. 'd:/mysite' or '/var/www/mysite'),
    // the Url prefix chars will be removed from the client request, then used
    // to locate the file to be served within this local folder
    // - if Source is a remote URI (like http://....), the Url prefix chars will
    // be removed from the client request, then appended to this remote URI for
    // remote proxy with caching, which is e.g. 'http://ftp.debian.org/debian'
    // 'or http://security.debian.org/debian-security' matching Local 'debian' or
    // 'debian-security' prefixes, to compute a source remote URI
    // - if Source is a FastCGI application (like fcgi://127.0.0.1:9000/var/www
    // or fcgi://unix:/run/php-fpm.sock:/var/www), the request is transmitted
    // to this application, e.g. php-fpm, locating the script from the Url
    // within the folder of the application host set after the address
    // - hpoFastCgiWsgi instead maps Url to WSGI SCRIPT_NAME, and the
    // remaining request path to PATH_INFO (no SCRIPT_FILENAME is sent)
    property Source: RawUtf8
      read fSource write fSource;
    /// optional bandwidth limitation of Source http:// remote URIs download
    // - would trigger a TStreamRedirect.LimitPerSecond additional process
    property HttpLimitPerSecond: integer
      read fHttpLimitPerSecond write fHttpLimitPerSecond;
    /// how many seconds a Source fcgi:// application may take with no data
    // - the client then receives a 504 HTTP_GATEWAYTIMEOUT response
    // - default is 60 seconds
    property FastCgiTimeoutSec: integer
      read fFastCgiTimeoutSec write fFastCgiTimeoutSec;
    /// enable front controller support for a fcgi:// Source, e.g. 'index.php'
    // - as nginx "try_files $uri $uri/ /index.php?$query_string" does: a path
    // up to its first segment with the FastCgiIndex extension (e.g. '.php') is
    // a script, sent to the application as usual; any other existing file is
    // returned as from a local folder (GET and HEAD only); a folder path
    // ending with '/' is sent to its FastCgiIndex script, if any; and any
    // other path is sent to the root FastCgiIndex script, with no PATH_INFO
    // and REQUEST_URI unchanged
    // - the Source folder should be accessible from this server with the same
    // path, as for nginx
    // - if this Source is published at the root, the built-in /favicon.ico is
    // disabled, unless THttpProxyServerMainSettings.FaviconFile is set
    property FastCgiIndex: RawUtf8
      read fFastCgiIndex write fFastCgiIndex;
    /// how many seconds HEAD requests could be cached in memory
    // - 0 would disable head caching
    // - default is 60, i.e. cache HEAD response for 1 minute
    property HttpHeadCacheSec: integer
      read fHttpHeadCacheSec write fHttpHeadCacheSec;
    /// seconds of HTTP keep alive source - default to 30
    property HttpKeepAlive: integer
      read fHttpKeepAlive write fHttpKeepAlive;
    /// below this KB size, will make a blocking GET and no background proxy
    // - default is 16 KB
    property HttpDirectGetKB: integer
      read fHttpDirectGetKB write fHttpDirectGetKB;
    /// CSV list of GLOB file or directly names to be rejected as not found
    // - e.g. '*.secret'
    property RejectCsv: RawUtf8
      read fRejectCsv write fRejectCsv;
    /// support optional "Cache-Control: max-age=..." header timeout value
    // - default 0 value will disable this header transmission
    // - to be used in conjunction with the hpoIfModifiedSince option
    property CacheControlMaxAgeSec: integer
      read fCacheControlMaxAgeSec write fCacheControlMaxAgeSec;
    /// overwrite the main MemCache setting to tune in-memory caching
    // - can be used for both local file or remote URI lookups
    property MemCache: THttpProxyMem
      read fMemCache write fMemCache;
    /// overwrite the main DiskCache setting to tune on-disk caching
    // - disk cache is available only for remote URI lookups, i.e. if Source
    // is defined as 'http://...' and not as a local file
    property DiskCache: THttpProxyDisk
      read fDiskCache write fDiskCache;
  end;
  /// store a dynamic array of THttpProxyUrlSettings instances
  THttpProxyUrlSettingsObjArray = array of THttpProxyUrlSettings;

  /// the available high-level options for THttpProxyServerMainSettings
  // - psoLogVerbose could be used to debug a server in production
  // - psoExcludeDateHeader won't include the default "Date: ..." HTTP header
  // - psoHttpsSelfSigned will enable HTTPS with a self-signed certificate
  // - psoReusePort will set SO_REUSEPORT on POSIX, to bind several servers
  // - psoEnableLogging enable an associated THttpServerGeneric.Logger instance
  // - psoRejectBotUserAgent identifies and rejects Bots via IsHttpUserAgentBot()
  // - psoBan40xIP will reject any IP for a few seconds after a 4xx error code
  // - psoDisableMemCache will globally disable all MemCache settings
  // - psoDisableGzip won't use runtime gzip content compression
  THttpProxyServerOption = (
    psoLogVerbose,
    psoExcludeDateHeader,
    psoHttpsSelfSigned,
    psoReusePort,
    psoEnableLogging,
    psoRejectBotUserAgent,
    psoBan40xIP,
    psoDisableMemCache,
    psoDisableGzip);

  /// a set of available options for THttpProxyServerMainSettings
  THttpProxyServerOptions = set of THttpProxyServerOption;

  /// define the THttpProxyServer HTTP(S) server settings
  THttpProxyServerMainSettings = class(TSynAutoCreateFields)
  protected
    fPort: RawUtf8;
    fOptions: THttpProxyServerOptions;
    fThreadCount: integer;
    fCertificateFile: TFileName;
    fCACertificatesFile: TFileName;
    fPrivateKeyFile: TFileName;
    fPrivateKeyPassword: SpiUtf8;
    fServerName: RawUtf8;
    fLog: THttpLoggerSettings;
    fFaviconFile: TFileName;
  public
    /// initialize the default settings
    constructor Create; override;
    /// assign the HTTPS/TLS settings to a context
    function SetupTls(var tls: TNetTlsContext): boolean;
  published
    /// the local port used for HTTP/HTTPS content delivery
    // - is '8098' by default (THttpPeerCache uses 8099), unassigned by IANA
    // - you can bind to a specific 'IP:port' if needed
    property Port: RawUtf8
      read fPort write fPort;
    /// customize this proxy cache HTTP/HTTPS process
    property Options: THttpProxyServerOptions
      read fOptions write fOptions;
    /// the number of threads of the HTTP/HTTPS content delivery
    // - default value is the number of CPU cores + 1, which is enough to scale
    // well if no content is to be generated
    property ThreadCount: integer
      read fThreadCount write fThreadCount;
    /// custom log settings for the psoEnableLogging option
    // - e.g. to override default LOGFORMAT_COMBINED output, or rotation settings
    property Log: THttpLoggerSettings
      read fLog write fLog;
    /// optional HTTPS certificate file name
    // - should also set PrivateKeyFile and PrivateKeyPassword
    property CertificateFile: TFileName
      read fCertificateFile write fCertificateFile;
    /// optional HTTPS private key file name
    property PrivateKeyFile: TFileName
      read fPrivateKeyFile write fPrivateKeyFile;
    /// optional HTTPS private key file password
    property PrivateKeyPassword: SpiUtf8
      read fPrivateKeyPassword write fPrivateKeyPassword;
    /// optional HTTPS certificates authorities file
    property CACertificatesFile: TFileName
      read fCACertificatesFile write fCACertificatesFile;
    /// optional alternate favicon.ico file name
    property FaviconFile: TFileName
      read fFaviconFile write fFaviconFile;
    /// optional Server name for HTTP/HTTPS
    // - to overwrite the default value set by the framework e.g. 'mORMot2 (Linux)'
    property ServerName: RawUtf8
      read fServerName write fServerName;
  end;

  /// hold one or several remote content source(s) process for THttpProxyServer
  THttpProxyUrlObjArray = array of THttpProxyUrl;

  THttpProxyServer = class;

  /// define the THttpProxyServer forward proxy process
  THttpProxyServerSettings = class(TSynAutoCreateFields)
  protected
    fServer: THttpProxyServerMainSettings;
    fMemCache: THttpProxyMem;
    fDiskCache: THttpProxyDisk;
    fUrl: THttpProxyUrlSettingsObjArray;
  public
    /// initialize the default settings
    constructor Create; override;
    /// append and own a given THttpProxyUrlSettings definition at runtime
    // - this instance will be stored and owned in Url[].Settings array
    function AddUrl(one: THttpProxyUrlSettings): THttpProxyUrlSettings;
    /// create a THttpProxyUrlSettings definition to serve a local static folder
    // - if optional ExceptionClass is supplied, the local folder should exist
    // - this instance will be stored and owned in Url[].Settings array
    function AddFolder(const folder: TFileName; const uri: RawUtf8 = '';
      RaiseExceptionOnNonExistingFolder: ESynExceptionClass = nil): THttpProxyUrlSettings;
    /// create a THttpProxyUrlSettings definition as remote URI proxy
    function AddProxy(const remoteuri: RawUtf8; const localuri: RawUtf8 = ''): THttpProxyUrlSettings;
    /// create a THttpProxyUrlSettings definition as FastCGI application
    // - fcgiuri is e.g. 'fcgi://127.0.0.1:9000/var/www' for php-fpm
    // - set hpoFastCgiWsgi for Flask/WSGI (no script path is needed)
    // - all methods are transmitted by default, not only GET and HEAD
    function AddFastCgi(const fcgiuri: RawUtf8; const localuri: RawUtf8 = ''): THttpProxyUrlSettings;
    /// create a THttpProxyUrlSettings definition for using a callback
    function AddEvent(const request: TOnHttpProxyServerRequest;
      const uri: RawUtf8 = ''): THttpProxyUrlSettings;
    /// load URI definitions from local *.json files (also supports JSON5 or HJson)
    // - returns the number of added THttpProxyUrl instances into Url[]
    function AddFromFiles(const settingsfolder: TFileName;
      const mask: TFileName = '*.json'): integer;
  published
    /// define the HTTP/HTTPS server configuration
    property Server: THttpProxyServerMainSettings
      read fServer write fServer;
    /// default in-memory cache settings
    // - is set to MaxSize = 4KB and TimeoutSec = 5 minutes
    // - can be overriden by Url[].MemCache property
    property MemCache: THttpProxyMem
      read fMemCache write fMemCache;
    /// default on-disk local cache settings
    // - can be overriden by Url[].DiskCache property
    property DiskCache: THttpProxyDisk
      read fDiskCache write fDiskCache;
    /// access the remote content sources settings
    // - owned as a TSynAutoCreateFields dynarray
    property Url: THttpProxyUrlSettingsObjArray
      read fUrl;
  end;

  /// define how a THttpProxyUrl definition is implemented
  THttpProxySource = (
    hpsUndefined,
    hpsLocalFolder,
    hpsRemoteUri,
    hpsEvent,
    hpsFastCgi);
  THttpProxySources = set of THttpProxySource;

  /// internal HEAD request cache entry for a given resource (80-bytes on CPU64)
  THeadCache = packed record
    /// origin headers ready to be sent back to the client with the file content
    PurgedHeaders: RawUtf8;
    /// resource full size, e.g. from headers Content-Length
    Size: Int64;
    /// resource remote timestamp, e.g. from headers Last-Modified
    TimeMS: TUnixMSTime;
    /// HTTP code number from remote server, e.g. 200, 206, 301 or 404
    Status: word;
    /// Base-32 encoded URI + etag/lastmod from Headers
    HashB32: TShort32;
    /// THashDigest-compatible 20-bytes truncated SHA-256(Uri+etag/lastmod)
    HashDigest: THashAlgo;
    Hash160: THash160;
  end;
  THeadCaches = array of THeadCache;

  /// process one remote content source for THttpProxyServer
  THttpProxyUrl = class(TSynPersistent)
  protected
    fSettings: THttpProxyUrlSettings;
    fOwner: THttpProxyServer;
    fSource: THttpProxySource;
    fAlgos: THashAlgos; // may be in [hfMD5, hfSha1, hfSha256] range
    fRemoteUri: TUri;
    fFastCgi: TFastCgiAsyncConnections; // for hpsFastCgi
    fFastCgiRoot, fFastCgiPrefix: RawUtf8;
    fFastCgiExt: RawUtf8;       // e.g. '.php' from Settings.FastCgiIndex
    fFastCgiFolder: TFileName;  // fFastCgiRoot as local folder for FastCgiIndex
    fHeadCache: TSynDictionary; // THash160 / THeadCache
    fMemCache: TSynDictionary;  // THash160 / Content:RawByteString
    fHashCache: TSynDictionary; // name:RawUtf8 / hash[fAlgos]:TRawUtf8DynArray
    fReject: TUriMatch;
    fRemoteClient: IHttpClient;
    fOsSafe: TOSLightLock; // non-reentrant lock for fRemoteClient + file access
    procedure SetRemoteClient;
    procedure BackgroundGet(Sender: TObject);
  public
    /// initialize this instance
    constructor Create(aSettings: THttpProxyUrlSettings;
      aOwner: THttpProxyServer); reintroduce;
    /// finalize this instance and its associated Settings
    destructor Destroy; override;
    /// return a local file content with proper fMemCache support
    function ReturnFile(ctxt: THttpServerRequestAbstract; const hash: THash160;
      const filename: TFileName; const uri: TUriMatchName; var size: Int64;
      lastmod: TUnixMSTime; var loginfo: PUtf8Char; canbecached: boolean = true): cardinal;
    /// return a file of the local folder of this Source, from its relative name
    // - name is decoded, safe and ending with #0: its SHA-256 is the cache key
    function ReturnLocalFile(ctxt: THttpServerRequestAbstract;
      const uri: TUriMatchName; name: PUtf8Char; namelen: PtrInt;
      var filename: TFileName; var hash: THash256Rec; var size: Int64;
      var loginfo: PUtf8Char): cardinal;
    /// compute the (probably cached) hash of a given URI resource
    function ReturnHash(ctxt: THttpServerRequestAbstract;
      const name: RawUtf8; var fn: TFileName): integer;
    /// perform a HTTP HEAD on the remote proxy URI using a shared connection
    procedure RemoteClientHead(const uri: TUri; const hash: THash160;
      var cache: THeadCache);
    /// perform a HTTP GET on the remote proxy URI using a shared connection
    function RemoteClientGet(const uri: TUri): RawByteString;
  published
    /// how this URI is implemented
    property Source: THttpProxySource
      read fSource;
    /// access to the associated (and owned) settings for this URI
    property Settings: THttpProxyUrlSettings
      read fSettings;
  end;

  /// Exception class raised by THttpProxyServer
  EHttpProxyServer = class(ESynException);

  /// implements a HTTP server with forward proxy and caching
  THttpProxyServer = class(TSynPersistent)
  protected
    fSettings: THttpProxyServerSettings;
    fUrl: THttpProxyUrlObjArray;
    fLog: TSynLogClass;
    fFlags: set of (fUrlSet, fSettingsOwned, fHasLog, fHasCacheClean);
    fSources: THttpProxySources;
    fUrlOptions: THttpProxyUrlOptions; // consolidated from all fUrl[].Options
    fTempFilesTix: cardinal;
    fServer: THttpAsyncServer;
    fGC: TObjectDynArray;
    fPartials: THttpPartials;
    fBackgroundCount: integer; // detached workers still referencing this proxy
    procedure BackgroundFinished(Sender: TObject);
    function SetupTls(var tls: TNetTlsContext): boolean; virtual;
    procedure AfterServerStarted; virtual;
    procedure OnIdle(Sender: TObject; NowTix: Int64);
    function OnExecute(Ctxt: THttpServerRequestAbstract): cardinal;
    function OnGetHeadLocalFolder(Ctxt: THttpServerRequest; const Uri: TUriMatchName): cardinal;
    function OnGetHeadRemoteUri(Ctxt: THttpServerRequest; var Uri: TUriMatchName): cardinal;
    function OnFastCgi(Ctxt: THttpServerRequest; const Uri: TUriMatchName): cardinal;
    function OnFastCgiIndex(Ctxt: THttpServerRequest; const Uri: TUriMatchName;
      One: THttpProxyUrl; var Path: TSynTempAdder): cardinal;
    procedure OnBackgroundDeleteDeprecated(Sender: TObject);
  public
    /// initialize this forward proxy instance
    // - the supplied aSettings should be owned by the caller (e.g from a main
    // settings class instance) - if nil is supplied, this instance will setup
    // and own its own instance
    constructor Create(aSettings: THttpProxyServerSettings); reintroduce; virtual;
    /// finalize this class instance
    destructor Destroy; override;
    /// actually start the HTTP/HTTPS server
    // - you can specify a private key password if not already in Settings
    // - may raise some exceptions if the HTTP server cannot be started
    procedure Start(const aPrivateKeyPassword: SpiUtf8 = '');
    /// finalize the HTTP/HTTPS server
    procedure Stop;
    /// the local HTTP(S) asynchronous server
    property Server: THttpAsyncServer
      read fServer;
    /// the kind of definitions stored in Settings.Uri[]
    property Sources: THttpProxySources
      read fSources;
    /// access to the used settings
    property Settings: THttpProxyServerSettings
      read fSettings;
    /// access to the defined source URIs process, set between Start/Stop calls
    property Url: THttpProxyUrlObjArray
      read fUrl;
  end;

function ToText(hps: THttpProxySource): PShortString; overload;


implementation


{ ******************** FastCGI Asynchronous Client Connections }

procedure FastCgiAppendHeader(var Dest: TSynTempAdder;
  RecType: TFastCgiRecordType; ContentLen: PtrInt);
var
  h: PFastCgiHeader;
begin
  h := Dest.Add(SizeOf(h^));
  h^.Version := FCGI_VERSION_1;
  h^.RecType := RecType;
  h^.RequestID := $0100; // = 1 in network order: a single request per connection
  h^.ContentLen := bswap16(ContentLen);
  PWord(@h^.PaddingLen)^ := 0; // PaddingLen = Reserved = 0
end;

procedure FastCgiAppendLen(var Dest: TSynTempAdder; Len: PtrInt);
begin
  if Len < 128 then
    Dest.Add(AnsiChar(Len)) // lengths up to 127 are stored as a single byte
  else
    Dest.Add32BigEndian(cardinal(Len) or $80000000); // or as 31-bit
end;

procedure FastCgiAppendName(var Dest: TSynTempAdder;
  Name: PUtf8Char; NameLen, ValueLen: PtrInt);
begin // the caller should then append ValueLen bytes of value
  FastCgiAppendLen(Dest, NameLen);
  FastCgiAppendLen(Dest, ValueLen);
  Dest.Add(Name, NameLen);
end;

procedure FastCgiAppendParam(var Dest: TSynTempAdder;
  Name: PUtf8Char; NameLen: PtrInt; Value: PUtf8Char; ValueLen: PtrInt);
begin
  FastCgiAppendName(Dest, Name, NameLen, ValueLen);
  Dest.Add(Value, ValueLen);
end;

procedure FastCgiAppendParam(var Dest: TSynTempAdder;
  const Name, Value: RawUtf8);
begin
  FastCgiAppendName(Dest, pointer(Name), length(Name), length(Value));
  Dest.Add(Value);
end;

procedure FastCgiAppendPath(var Dest: TSynTempAdder; const Name, Folder: RawUtf8;
  Path: PUtf8Char; PathLen: PtrInt);
begin // append Folder + '/' + Path as value, with no temporary string
  FastCgiAppendName(Dest, pointer(Name), length(Name),
    length(Folder) + 1 + PathLen);
  Dest.Add(Folder);
  Dest.AddDirect('/');
  Dest.Add(Path, PathLen);
end;

procedure FastCgiAppendStream(var Dest: TSynTempAdder;
  RecType: TFastCgiRecordType; Content: PAnsiChar; ContentLen: PtrInt);
var
  n: PtrInt;
begin
  while ContentLen > 0 do
  begin
    n := ContentLen;
    if n > FCGI_MAXCONTENT then
      n := FCGI_MAXCONTENT;
    FastCgiAppendHeader(Dest, RecType, n);
    Dest.Add(Content, n);
    inc(Content, n);
    dec(ContentLen, n);
  end;
  FastCgiAppendHeader(Dest, RecType, 0); // a void record ends the stream
end;

procedure FastCgiAppendRequest(var Dest: TSynTempAdder;
  Params: PAnsiChar; ParamsLen: PtrInt; const Body: RawByteString);
var
  n: PtrInt;
begin
  // a single memory allocation, if any: each record has a 8 bytes header
  n := ParamsLen + length(Body);
  Dest.Prepare(n + (n div FCGI_MAXCONTENT + 6) * SizeOf(TFastCgiHeader));
  // frtBeginRequest
  FastCgiAppendHeader(Dest, frtBeginRequest, 8);
  Dest.Add16BigEndian(1); // role = FCGI_RESPONDER
  Dest.AddChars(#0, 6);   // flags = 0 i.e. no FCGI_KEEP_CONN, then 5 reserved
  // frtParams and frtStdIn streams
  FastCgiAppendStream(Dest, frtParams, Params, ParamsLen);
  FastCgiAppendStream(Dest, frtStdIn, pointer(Body), length(Body));
end;

const
  // the HTTP headers which should not be sent as HTTP_* variables
  FCGI_NOHEADER: array[0..4] of PAnsiChar = (
    'CONTENT-LENGTH:', // already sent as CONTENT_LENGTH
    'CONTENT-TYPE:',   // already sent as CONTENT_TYPE
    'USER-AGENT:',     // already sent as HTTP_USER_AGENT
    'PROXY:',          // HTTP_PROXY is a setting for most applications (httpoxy)
    nil);

procedure FastCgiAppendHeaders(var Dest: TSynTempAdder; P, PEnd: PUtf8Char);
var
  len, n: PtrInt;
  c: AnsiChar;
  v: PUtf8Char;
  name: TByteToAnsiChar; // 'HTTP_' + the header name in upper case
begin // caller ensured P <> nil
  PCardinal(@name)^ := HTTP_32;
  name[4] := '_';
  repeat
    while P^ <= ' ' do
      if P^ <> #0 then
        inc(P) // ignore any kind of line feed or void line
      else
        exit;  // end of input
    len := BufferLineLength(P, PEnd);
    if IdemPPChar(P, @FCGI_NOHEADER) < 0 then
    begin
      // "Name-Two: value" is sent as HTTP_NAME_TWO = value
      n := 0;
      repeat
        c := NormToUpperAnsi7[P[n]];
        case c of
          'A' .. 'Z',
          '0' .. '9':
            ;
          '-':
            c := '_';
        else
          break; // ':' ends the name - '_' could be confused with '-'
        end;
        name[n + 5] := c;
        inc(n);
      until n = SizeOf(name) - 5;
      if (n <> 0) and
         (P[n] = ':') then
      begin
        v := P + n + 1;
        while v^ = ' ' do
          inc(v);
        FastCgiAppendParam(Dest, @name, n + 5, v, P + len - v);
      end;
    end;
    inc(P, len);
  until false;
end;

function FastCgiScriptLen(P: PUtf8Char; Len: PtrInt; const Ext: RawUtf8): PtrInt;
var
  s, e: PUtf8Char;
  script, extlen: PtrInt;
  dot: boolean;
begin
  // the script is the path up to its first segment with a dot, or ending with
  // Ext if supplied: returns 0 if there is no script, -1 if the path is unsafe
  result := -1;
  if P^ = '/' then
    exit; // should be below the root folder
  extlen := length(Ext);
  script := 0;
  dot := false;
  s := P; // start of the current segment
  e := P;
  repeat
    case e^ of
      '.':
        dot := true;
      ':',
      '\':
        exit; // no drive, stream or Windows path delimiter
      #0,
      '/':
        begin
          if (e - s = 2) and
             (PWord(s)^ = ord('.') + ord('.') shl 8) then
            exit; // no parent folder
          if dot and
             (script = 0) and
             ((extlen = 0) or
              ((e - s > extlen) and
               IdemPropNameU(Ext, e - extlen, extlen))) then
            script := e - P; // but continue to validate the PATH_INFO
          if e^ = #0 then
            break;
          s := e + 1;
        end;
    end;
    inc(e);
  until false;
  if e - P = Len then // no #0 within the path
    result := script;
end;

function FastCgiAppendParams(var Dest: TSynTempAdder;
  Ctxt: THttpServerRequestAbstract; const Root, UriPrefix, ServerPort: RawUtf8;
  Path: PUtf8Char; PathLen: PtrInt; Wsgi: boolean; const ScriptExt: RawUtf8): boolean;
var
  name, p: PUtf8Char;
  namelen, scriptlen, i: PtrInt;
  tmp: TSynTempBuffer; // no heap allocation for a path up to 4KB
  num: TTemp24;
begin
  result := false;
  if (PathLen < 0) or
     ((PathLen = 0) and not Wsgi) then
    exit;
  // decode the path once, with no temporary RawUtf8 allocation
  name := nil;
  namelen := 0;
  scriptlen := 0;
  if PathLen <> 0 then
  begin
    tmp.Init(Path, PathLen);
    name := tmp.buf;
    namelen := DoUrlDecode(tmp.buf, tmp.buf, '+') - name;
    name[namelen] := #0;
    if Wsgi then
    begin
      // no filesystem lookup; still reject an encoded NUL in PATH_INFO
      if StrLen(name) <> namelen then
      begin
        tmp.Done;
        exit;
      end;
    end
    else
      scriptlen := FastCgiScriptLen(name, namelen, ScriptExt);
  end;
  if Wsgi or
     (scriptlen > 0) then
  begin
    // the standard CGI/1.1 variables
    FastCgiAppendParam(Dest, 'GATEWAY_INTERFACE', 'CGI/1.1');
    FastCgiAppendParam(Dest, 'SERVER_SOFTWARE', 'mORMot2');
    FastCgiAppendParam(Dest, 'SERVER_PROTOCOL', 'HTTP/1.1');
    FastCgiAppendParam(Dest, 'REQUEST_METHOD', Ctxt.Method);
    FastCgiAppendParam(Dest, 'REQUEST_URI', Ctxt.Url);
    i := PosExChar('?', Ctxt.Url);
    if i = 0 then
      i := length(Ctxt.Url);
    FastCgiAppendParam(Dest, 'QUERY_STRING', 12,
      PUtf8Char(pointer(Ctxt.Url)) + i, length(Ctxt.Url) - i);
    if Wsgi then
    begin
      FastCgiAppendParam(Dest, 'SCRIPT_NAME', UriPrefix);
      if namelen <> 0 then
        FastCgiAppendPath(Dest, 'PATH_INFO', '', name, namelen)
      else if (length(Ctxt.Url) > length(UriPrefix)) and
              (Ctxt.Url[length(UriPrefix) + 1] = '/') then
        FastCgiAppendParam(Dest, 'PATH_INFO', '/')
      else
        FastCgiAppendParam(Dest, 'PATH_INFO', '');
    end
    else
    begin
      FastCgiAppendPath(Dest, 'SCRIPT_NAME', UriPrefix, name, scriptlen);
      FastCgiAppendPath(Dest, 'SCRIPT_FILENAME', Root, name, scriptlen);
      FastCgiAppendParam(Dest, 'DOCUMENT_ROOT', Root);
      if scriptlen < namelen then
        FastCgiAppendParam(Dest, 'PATH_INFO', 9,
          name + scriptlen, namelen - scriptlen);
    end;
    if Ctxt.RemoteIP = '' then // see RemoteIPLocalHostAsVoidInServers
      FastCgiAppendParam(Dest, 'REMOTE_ADDR', '127.0.0.1')
    else
      FastCgiAppendParam(Dest, 'REMOTE_ADDR', Ctxt.RemoteIP);
    i := length(Ctxt.Host);
    while (i > 0) and
          (Ctxt.Host[i] in ['0' .. '9']) do
      dec(i);
    if (i > 0) and
       (Ctxt.Host[i] = ':') then
      dec(i) // trim any :port
    else
      i := length(Ctxt.Host);
    FastCgiAppendParam(Dest, 'SERVER_NAME', 11, pointer(Ctxt.Host), i);
    FastCgiAppendParam(Dest, 'SERVER_PORT', ServerPort);
    if hsrHttps in Ctxt.ConnectionFlags then
      FastCgiAppendParam(Dest, 'HTTPS', 'on');
    if Ctxt.InContentType <> '' then
      FastCgiAppendParam(Dest, 'CONTENT_TYPE', Ctxt.InContentType);
    if Ctxt.InContent <> '' then
    begin
      p := pointer(StrUInt32(@num[23], length(Ctxt.InContent)));
      FastCgiAppendParam(Dest, 'CONTENT_LENGTH', 14, p, PUtf8Char(@num[23]) - p);
    end;
    if Ctxt.UserAgent <> '' then
      FastCgiAppendParam(Dest, 'HTTP_USER_AGENT', Ctxt.UserAgent);
    // each other request header as HTTP_NAME
    p := pointer(Ctxt.InHeaders);
    if p <> nil then
      FastCgiAppendHeaders(Dest, p, p + length(Ctxt.InHeaders));
    result := true;
  end;
  if PathLen <> 0 then
    tmp.Done;
end;

const
  // the CGI headers which are not sent back as such to the HTTP client
  FCGI_CGIHEADER: array[0..4] of PAnsiChar = (
    'STATUS:',
    'CONTENT-TYPE:',
    'CONTENT-LENGTH:',
    'LOCATION:',
    nil);

function FastCgiParseOutput(var Output: RawByteString; out Status: integer;
  var ContentType, Headers: RawUtf8): boolean;
var
  p, e, next, v: PUtf8Char;
  len, body, known: PtrInt;
  location: boolean;
  h: TSynTempAdder; // no heap allocation up to 4KB of headers
begin
  result := false;
  Status := 0;
  location := false;
  body := -1;
  p := pointer(Output);
  e := p + length(Output);
  h.Init;
  repeat
    // each line could end with LF or CRLF
    next := PosChar(p, e - p, #10);
    if next = nil then
      break; // the headers are not ended by a void line
    len := next - p;
    inc(next);
    if (len <> 0) and
       (p[len - 1] = #13) then
      dec(len);
    if len = 0 then
    begin
      body := next - PUtf8Char(pointer(Output)); // void line = end of headers
      break;
    end;
    // parse this "Name: value" line
    v := PosChar(p, len, ':');
    if (v = nil) or
       (v = p) then
      break;
    repeat
      inc(v);
    until v^ <> ' ';
    known := IdemPPChar(p, @FCGI_CGIHEADER);
    case known of
      0: // STATUS:
        Status := GetCardinal(v);
      1: // CONTENT-TYPE:
        FastSetString(ContentType, v, p + len - v);
      2: // CONTENT-LENGTH:
        ; // ignored: computed by the HTTP server from the actual body
    else
      begin
        if known = 3 then      // LOCATION:
          location := true;    // but still sent back to the HTTP client
        h.Add(p, len);         // whole header content
        h.AddDirect(#13, #10); // HTTP headers require CRLF
      end;
    end;
    p := next;
  until false;
  if Status = 0 then
    if location then
      Status := HTTP_FOUND
    else
      Status := HTTP_SUCCESS;
  if (body < 0) or
     (Status < 100) or
     (Status > 599) then
  begin
    h.Store.Done;
    exit;
  end;
  if h.Store.added <> 0 then // = h.Size
    dec(h.Store.added, 2);   // no CRLF after the last header
  h.Done(Headers);
  if GetRefCount(Output) <> 1 then
    delete(Output, 1, body) // should never happen - but safe
  else
  begin
    body := e - next;
    if body = 0 then
      FastAssignNew(Output)
    else
    begin
      MoveFast(next^, pointer(Output)^, body);
      FakeLength(Output, body); // truncate in Output with no allocation
    end;
  end;
  result := true;
end;


{ TFastCgiResponse }

procedure TFastCgiResponse.Init;
begin
  Output := '';
  Error := '';
  AppStatus := 0;
  ProtocolStatus := 0;
  Done := false;
end;

function TFastCgiResponse.Parse(P: PAnsiChar; Len: PtrInt): PtrInt;
var
  h: PFastCgiHeader;
  clen, total: PtrInt;
begin
  result := 0;
  while (Len >= SizeOf(TFastCgiHeader)) and
        not Done do
  begin
    h := pointer(P);
    if h^.Version <> FCGI_VERSION_1 then
    begin
      result := -1;
      exit;
    end;
    clen := bswap16(h^.ContentLen);
    total := SizeOf(TFastCgiHeader) + clen + h^.PaddingLen;
    if Len < total then
      break; // wait for the whole record
    inc(P, SizeOf(TFastCgiHeader));
    case h^.RecType of
      frtStdOut:
        Append(Output, P, clen);
      frtStdErr:
        Append(Error, P, clen);
      frtEndRequest:
        begin
          if clen < 8 then
          begin
            result := -1;
            exit;
          end;
          AppStatus := bswap32(PCardinal(P)^);
          ProtocolStatus := ord(P[4]);
          Done := true;
        end;
    end; // any other record type is ignored
    inc(P, total - SizeOf(TFastCgiHeader));
    dec(Len, total);
    inc(result, total);
  end;
end;


{ TFastCgiAsyncConnection }

procedure TFastCgiAsyncConnection.AfterCreate;
begin
  fHttpHandle := 0;
  fResponse.Init;
end;

procedure TFastCgiAsyncConnection.HttpResponse(Status: integer);
var
  h: PtrUInt;
  o: TFastCgiAsyncConnections;
  server: THttpServerGeneric;
  ctype, headers: RawUtf8;
begin
  o := pointer(fOwner);
  // the HTTP client should be answered only once, but this method is called
  // from several threads: by OnRead from the threads polling the sockets, by
  // OnLastOperationIdle from the idle thread, and by OnClose from any thread
  // closing this connection - Send may also reset fHttpHandle from a HTTP
  // thread: so a compare-and-swap is needed to let a single caller continue
  h := fHttpHandle;
  if (h = 0) or
     not LockedExc(fHttpHandle, 0, h) or
     o.Terminated then // HTTP server may be gone
    exit;
  server := o.fHttpServer;
  if fResponse.Error <> '' then
    o.DoLog(sllDebug, 'HttpResponse(%) stderr=%',
      [Status, EscapeToShort(fResponse.Error)], self);
  if Status = HTTP_SUCCESS then
    if (fResponse.ProtocolStatus = 0) and
       FastCgiParseOutput(fResponse.Output, Status, ctype, headers) then
    begin
      // Output is now the body, and is sent with no copy
      server.AsyncResponse(h, RawUtf8(fResponse.Output), ctype, Status,
        headers);
      fResponse.Output := ''; // now owned by the HTTP server
      exit;
    end
    else
      Status := HTTP_BADGATEWAY; // invalid response from the application
  server.AsyncResponseError(h, 'FastCGI application failure', Status);
end;

function TFastCgiAsyncConnection.OnRead: TPollAsyncSocketOnReadWrite;
var
  n: PtrInt;
begin
  result := soClose;
  n := fResponse.Parse(fRd.Buffer, fRd.Len);
  if n < 0 then
    exit; // protocol error: OnClose will answer 502
  fRd.Remove(n);
  if fResponse.Done then
    HttpResponse(HTTP_SUCCESS) // then close, since FCGI_KEEP_CONN was not set
  else
    result := soContinue;
end;

procedure TFastCgiAsyncConnection.OnClose;
begin
  inherited OnClose; // set fClosed flag
  HttpResponse(HTTP_BADGATEWAY); // closed with no frtEndRequest
  fRd.Clear; // one request per connection: don't keep this buffer in the GC
  fResponse.Init;
end;

function TFastCgiAsyncConnection.OnLastOperationIdle(
  nowsec: TAsyncConnectionSec): boolean;
begin
  result := fHttpHandle <> 0;
  if not result then
    exit;
  HttpResponse(HTTP_GATEWAYTIMEOUT); // no data from the application
  fOwner.Sockets.Stop(self, 'fastcgi timeout');
end;


{ TFastCgiAsyncConnections }

constructor TFastCgiAsyncConnections.Create(aHttpServer: THttpServerGeneric;
  const aUri: TUri; aTimeoutSec: integer; aLog: TSynLogClass);
begin
  fHttpServer := aHttpServer;
  fLayer := aUri.Layer;
  fTimeoutSec := aTimeoutSec;
  inherited Create(aUri.Server, aUri.Port, {clients=}0, nil, nil,
    TFastCgiAsyncConnection, 'fastcgi', aLog, ASYNC_OPTION_PROD, {threads=}1);
  fSockets.ReadMaxBytes := 128 shl 10; // parse 64KB records as they come
end;

function TFastCgiAsyncConnections.GetLastOperationIdleSeconds: cardinal;
begin
  result := fTimeoutSec;
end;

procedure TFastCgiAsyncConnections.Idle(NowTix: Int64);
begin
  {$ifdef USE_WINIOCP}
  {$ifndef NO_ASYNC_WINIOCP}
  ProcessIdleTix(self, NowTix); // on POSIX, is called from TAsyncClient.DoExecute
  {$endif NO_ASYNC_WINIOCP}
  {$endif USE_WINIOCP}
end;

function TFastCgiAsyncConnections.Send(aHttpHandle: TConnectionAsyncHandle;
  aRequest: pointer; aRequestLen: PtrInt): boolean;
var
  sock: TNetSocket;
  addr: TNetAddr;
  c: TAsyncConnection;
  f: TFastCgiAsyncConnection absolute c;
begin
  result := false;
  if Terminated then
    exit;
  // connect as blocking connection, since the application is likely to be local,
  // but with all timeout=0 to speed up the process especially on Windows
  with fThreadClients do
    if NewSocket(Address, Port, fLayer, {dobind=}false, {timeout=}0, 0, 0,
         {retry=}0, sock, @addr) <> nrOk then
      exit;
  if (sock.MakeAsync <> nrOk) or
     not ConnectionCreate(sock, addr, c) then
  begin
    sock.ShutdownAndClose({rdwr=}false);
    exit;
  end;
  // set the handle before any read: the response may come very quickly
  f.fHttpHandle := aHttpHandle;
  if not fSockets.Start(c) then
  begin
    f.fHttpHandle := 0; // the caller will answer
    ConnectionAbortAndNil(c);
    exit;
  end;
  // true if sent, or if OnClose did already answer to the HTTP client
  result := Write(c, aRequest, aRequestLen) or
            not LockedExc(f.fHttpHandle, 0, aHttpHandle);
end;


{ ******************** High Level Web Server Definitions }

{ TWebServerGlobal }

constructor TWebServerGlobal.Create;
begin
  inherited Create;
  fMimeTypes.InitSpecific(TypeInfo(TWebServerMimeTypeDynArray), fMimeType,
    ptPUtf8Char, @fMimeTypeCount, {caseinsensitive=}true);
  // index TWebServerMimeType.Extension as ptPUtf8Char for FindMimeType()
end;

function TWebServerGlobal.FindMimeType(Extension: PUtf8Char): PtrInt;
begin
  result := fMimeTypes.FindHashed(Extension); // search as PUtf8Char
end;

function TWebServerGlobal.AddIfNeeded(const Extension: RawUtf8): PtrInt;
var
  added: boolean;
begin
  result := -1;
  if Extension = '' then
    exit;
  result := fMimeTypes.FindHashedForAdding(Extension, added);
  if added then
    fMimeType[result].Extension := Extension;
end;

function TWebServerGlobal.AddToIndexes(var Indexes: TSortedWordArray;
  const Extension: RawUtf8): PtrInt;
begin
  result := AddIfNeeded(Extension);
  if (result >= 0) and
     (result <= high(word)) then
    Indexes.Add(result);
end;

function TWebServerGlobal.Request(Context: THttpServerRequestAbstract;
  const Local: TWebServerLocal): cardinal;
begin
  result := 0; // should continue the process
  // to be continued
end;


{ ******************** THttpProxyServer HTTP Proxy with Cache }

{ THttpProxyMem }

constructor THttpProxyMem.Create;
begin
  inherited Create;
  fMaxSizeKB := -1; // use main THttpProxyServerSettings value
  fTimeoutSec := -1;
end;

function THttpProxyMem.FromUri(const uri: TUriMatchName): THttpProxyCacheKind;
begin
  result := [];
  if (fForceCsv <> '') and
     fForce.Check(fForceCsv, uri, PathCaseInsensitive) then
    include(result, pckForce);
  if (fIgnoreCsv <> '') and
     fIgnore.Check(fIgnoreCsv, uri, PathCaseInsensitive) then
    include(result, pckIgnore);
end;


{ THttpProxyUrlSettings }

constructor THttpProxyUrlSettings.Create;
begin
  inherited Create;
  fMethods := [urmGet, urmHead];
  fOptions := [];
  fHttpKeepAlive := 30;      // 30 seconds
  fHttpHeadCacheSec := 60;   // 1 minute
  fHttpDirectGetKB := 16;    // 16 KB
  fFastCgiTimeoutSec := 60;  // 1 minute
end;

function THttpProxyUrlSettings.SetOptions(aOptions: THttpProxyUrlOptions): THttpProxyUrlSettings;
begin
  result := self;
  if result <> nil then
    fOptions := fOptions + aOptions;
end;


{ THttpProxyUrl }

constructor THttpProxyUrl.Create(aSettings: THttpProxyUrlSettings;
  aOwner: THttpProxyServer);
begin
  inherited Create;
  fOsSafe.Init;
  fSettings := aSettings;
  fOwner := aOwner;
end;

destructor THttpProxyUrl.Destroy;
begin
  inherited Destroy;
  FreeAndNil(fFastCgi);
  FreeAndNil(fMemCache);
  FreeAndNil(fHashCache);
  FreeAndNil(fHeadCache);
  // fSettings are owned by THttpProxyServerSettings.Url[]
  fOsSafe.Done; // mandatory for TOSLightLock
end;

function THttpProxyUrl.ReturnHash(ctxt: THttpServerRequestAbstract;
  const name: RawUtf8; var fn: TFileName): integer;
var
  i: PtrInt;
  a, hf: THashAlgo;
  hashes: TRawUtf8DynArray;
  ext: PUtf8Char;
begin
  result := HTTP_NOTFOUND;
  ext := ExtractExtP(name, {withoutdot:}true);
  if ext = nil then
    exit;
  case PCardinal(ext)^ of
    MD5_LO:
      hf := hfMd5;
    SHA_LO + ord('1') shl 24:
      hf := hfSHA1;
    SHA_LO + ord('2') shl 24:
      hf := hfSHA256;
  else
    exit;
  end;
  if not (hf in fAlgos) then
    exit;
  if not fHashCache.FindAndCopy(name, hashes) then
  begin
    // first time this hash is requested: try to load and compute now
    i := length(fn);
    while i > 0 do
      if fn[i] = '.' then
        break // remove the .md5/.sha1/.sha256 file extension
      else
        dec(i);
    if i = 0 then
      exit;
    SetLength(fn, i - 1); // xxx.ext.md5 -> xxx.ext
    hashes := HashFileRaw(fn, fAlgos);
    if hashes = nil then
      exit; // no such file
    fHashCache.Add(name, hashes); // first insertion wins on contention
  end;
  // return the pre-computed hash of this file
  i := 0;
  for a := low(a) to high(a) do
    if a in fAlgos then
      if a = hf then
      begin
        ctxt.OutContent := hashes[i];
        ctxt.OutContentType := TEXT_CONTENT_TYPE;
        result := HTTP_SUCCESS;
        exit;
      end
      else
        inc(i);
end;

procedure THttpProxyUrl.SetRemoteClient;
var
  client: TSimpleHttpClient;
begin
  fOwner.fLog.Add.Log(sllTrace, 'RemoteClientHead: connect to %:%',
    [fRemoteUri.Server, fRemoteUri.Port], self);
  client := TSimpleHttpClient.Create(
    not (hpoClientAlllowWinApi in fSettings.Options));
  fRemoteClient := client;
  if not (hpoClientAllowRedirect in fSettings.Options) then
    client.Options^.RedirectMax := 0; // no automatic redirection by default
  if hpoClientIgnoreTlsError in fSettings.Options then
    client.Options^.TLS.IgnoreCertificateErrors := true;
  if Assigned(fSettings.OnRemoteClient) then
    fSettings.OnRemoteClient(self, fRemoteUri, client.Options^);
  if psoLogVerbose in fOwner.fSettings.Server.Options then
    client.OnLog := TSynLog.DoLog;
  // TODO: on scaling issue, maintain a pool of client connections
end;

const
  PURGED_PROXY =
    'CONTENT-LENGTH:|CONTENT-RANGE:|CONTENT-ENCODING:|CONNECTION:|' +
    'KEEP-ALIVE:|DATE:|TE:|TRAILER:|X-TIMER:|';
  PURGED: array[{=hpNoXCache}boolean] of PUtf8Char = (
    PURGED_PROXY,
    PURGED_PROXY + 'VIA:|X-CACHE|X-SERVED-BY:|X-CLACKS-|AGE:|');

procedure THttpProxyUrl.RemoteClientHead(const uri: TUri; const hash: THash160;
  var cache: THeadCache);
var
  keepalive, status: integer;
  d: TUnixTime; // converted to cache.TimeMS: TUnixMSTime
begin // this method is protected by fOsSafe.Lock
  // initialize the shared connection for HEAD and smallest GET
  if fRemoteClient = nil then
    SetRemoteClient;
  // always first try with a clean HEAD request
  keepalive := fSettings.HttpKeepAlive * MilliSecsPerSec;
  status := fRemoteClient.Request(uri, 'HEAD', '', '', '', keepalive);
  if StatusCodeIsSuccess(status) then // 2xx..3xx range
    if HttpRequestLength(pointer(fRemoteClient.Headers)) = nil then
      // server has no length: try range GET trick for a 200 response
      if status = HTTP_SUCCESS then // but return 204 NoContent or 3xx Redirect
      begin
        // Apache+Varnish may return 'Content-Range: bytes 0-0/*' for text/html
        status := fRemoteClient.Request(uri, 'GET',
          'Range: bytes=0-0', '', '', keepalive);
        if status = HTTP_PARTIALCONTENT then // 'Range:' triggers 206
          status := HTTP_SUCCESS;            // but we emulate a regular 200
      end;
  // set Headers and extract Size + TimeMS fields and base-32 encoded filename
  cache.Status := status;
  cache.Size := 0;
  d := 0;
  if StatusCodeIsSuccess(status) then // 2xx..3xx range
    GetHeaderInfo(fRemoteClient.Headers, cache.Size, d);
  cache.TimeMS := d * MilliSecsPerSec;
  cache.HashDigest := hfSHA256_160; // SHA-256 truncated to 160-bit
  HttpRequestHashBase32(Uri, @cache.HashB32, pointer(fRemoteClient.Headers),
    @cache.Hash160, hpoClientNormalizeCaseHash in fSettings.Options);
  cache.PurgedHeaders := PurgeHeaders(fRemoteClient.Headers, false,
    PURGED[hpoNoXCache in fSettings.Options]);
  // optionnaly cache the headers and its decoded fields
  if Assigned(fHeadCache) and
     (status < HTTP_SERVERERROR) and  // retry on pure 5xx or 666 (client)
     (cache.Size >= 0) then           // only store if the size was known
    fHeadCache.Add(hash, cache);
  fOwner.fLog.Add.Log(sllTrace, 'RemoteClientHead(%)=% size=% lastmod=% as % %',
    [uri.Address, status, cache.Size, d, cache.HashB32,
     fRemoteClient.LastError], self);
end;

function THttpProxyUrl.RemoteClientGet(const uri: TUri): RawByteString;
var
  status: integer;
begin // this method is protected by fOsSafe.Lock
  FastAssignNew(result);
  status := fRemoteClient.Request(uri, 'GET', '', '', '',
       fSettings.HttpKeepAlive * MilliSecsPerSec);
  if status in HTTP_GET_OK then // 200/204/206
    result := fRemoteClient.Body;
  fOwner.fLog.Add.Log(sllTrace, 'RemoteClientGet(%)=% size=%',
    [uri.Address, status, length(result)], self);
end;

type
  // state machine for HEAD + GET request methods to the remote server
  {$ifdef USERECORDWITHMETHODS}
  TStartProxyRequest = record
  {$else}
  TStartProxyRequest = object
  {$endif USERECORDWITHMETHODS}
    ctxt: THttpServerRequestAbstract;
    proxy: THttpProxyUrl;       // server proxy definition
    filename: TFileName;        // full file name, including path
    localsize: Int64;           // local file size
    loginfo: PUtf8Char;         // optional error message / log context
    head: THeadCache;           // remote headers retrieved via HEAD
    remote: TUri;               // URI members of the remote resource
    remotehash: THash160;       // remote TUri hash without etag/lastmod
    function MakeHeadAndComputeFilename: cardinal;
    function MakeGet(const path: TUriMatchName): cardinal;
  end;
  TStartProxyRequestClient = class(THttpClientSocket)
  public
    // some additional internal parameters and methods for proper threading
    uri: RawUtf8;
    filestream: TFileStreamEx;
    writestream: TStream;
    partialid: THttpPartialID;
    expectedsize: Int64;
  end;

function TStartProxyRequest.MakeHeadAndComputeFilename: cardinal;
begin // this method is protected by proxy.fOsSafe.Lock
  // always perform a HEAD request to the original server
  if not Assigned(proxy.fHeadCache) or // see HttpHeadCacheSec for this URI
     not proxy.fHeadCache.FindAndCopy(remotehash, head,
       {updatetimeout=}not (hpoClientHeadNoRefresh in proxy.fSettings.Options)) then
    proxy.RemoteClientHead(remote, remotehash, head);
  result := head.Status;
  localsize := head.Size; // for proper logging
  if not StatusCodeIsSuccess(result) then // 4xx.. range
    exit;
  if (head.Size < 0) and
     (result in [HTTP_SUCCESS, HTTP_PARTIALCONTENT]) then // not 204 or 3xx
    // note: progressive download needs an eventual size (by now), but Apache
    // may not provide Content-Length/Range on dynamic content (text/html)
    if FindNameValue(pointer(head.PurgedHeaders), 'CONTENT-TYPE: TEXT/HTM') = nil then
    begin
      result := HTTP_BADGATEWAY; // 502
      loginfo := 'nosize';
      exit;
    end;
  // compute the local file name from the base-32 hash of these headers
  if head.HashB32[0] <> #0 then // '' if no etag/lastmod
    if hpoClientCacheSubFolder in proxy.Settings.Options then
      MakePath([ // hash partitioning into subfolders
        proxy.Settings.DiskCache.Path, head.HashB32[1], head.HashB32], filename)
    else
      MakePath([proxy.Settings.DiskCache.Path, head.HashB32], filename);
end;

function TStartProxyRequest.MakeGet(const path: TUriMatchName): cardinal;
var
  direct: RawByteString;
  background: TStartProxyRequestClient;
  log: TSynLogClass;
  id: THttpPartialID;
  stream: TFileStreamEx;
  opt: THttpRequestExtendedOptions; // local copy since is modified by Open()
begin // this method is protected by proxy.fOsSafe.Lock
  result := HTTP_BADREQUEST;
  log := proxy.fOwner.fLog;
  // no matching local file: need to download to return the GET body
  localsize := head.Size; // reset to the expected final size from HEAD
  if (filename = '') or
     (localsize <= 0) or
     (localsize < proxy.fSettings.HttpDirectGetKB shl 10) then
  begin
    // use the blocking connection for smallest files < 16KB (or without size)
    if localsize <> 0 then
      if (filename = '') and // e.g. dynamic HTML folder index from Apache
         not (hpoClientNoCacheDirect in proxy.fSettings.Options) then
      begin
        if proxy.fMemCache.FindAndCopy(remotehash, direct) then
          loginfo := 'smallmem'
        else
        begin
          direct := proxy.RemoteClientGet(remote);
          proxy.fMemCache.Add(remotehash, direct);
        end;
      end else
        direct := proxy.RemoteClientGet(remote); // eventually written as file
    if (localsize = 0) or
       (direct <> '') then
      if ((localsize < 0) or // no length/range = retrieve full dynamic content
          (length(direct) = localsize)) and
         ((filename = '') or
          FileFromString(direct, filename)) then
      begin
        if localsize < 0 then
          localsize := length(direct);
        if loginfo = nil then
          loginfo := 'small';
        result := ctxt.SetOutContent(direct,
                    not (hpoDisable304 in proxy.fSettings.Options));
      end
      else
      begin
        loginfo := 'geterror';
        result := HTTP_BADGATEWAY; // 502
      end;
    exit;
  end;
  // big files need an asynchronous GET to the uri server
  stream := TFileStreamEx.Create(filename, fmCreate or fmShareRead);
  id := proxy.fOwner.fPartials.Add(filename, {expected=}localsize,
    PHashDigest(@head.HashDigest), Ctxt.ConnectionHttp, {eventualtime=}0);
  if id = 0 then
  begin
    stream.Free;
    loginfo := 'part'; // paranoid
    result := HTTP_SERVERERROR; // 500
    exit;
  end;
  // connect and start background downloading (unlocked)
  opt := proxy.fRemoteClient.Options^; // local copy for this instance
  ctxt.SetOutProgressiveFile(filename, localsize);
  try
    background := TStartProxyRequestClient.OpenOptions(remote, opt);
    background.filestream := stream;
    background.partialid := id;
    background.expectedsize := localsize;
    if proxy.fSettings.HttpLimitPerSecond > 0 then
    begin
      background.writestream := TStreamRedirect.Create(stream);
      (background.writestream as TStreamRedirect).LimitPerSecond :=
        proxy.fSettings.HttpLimitPerSecond;
    end
    else
      background.writestream := stream;
    background.uri := remote.Address;
    Make(['get-', id], head.PurgedHeaders); // already set to OutCustomHeaders
    LockedInc32(@proxy.fOwner.fBackgroundCount);
    try
      TLoggedWorkThread.Create(log, head.PurgedHeaders,
        background, proxy.BackgroundGet, proxy.fOwner.BackgroundFinished);
    except
      LockedDec32(@proxy.fOwner.fBackgroundCount);
      raise;
    end;
    loginfo := pointer(head.PurgedHeaders);
    result := HTTP_SUCCESS;
  except
    // handle main exception from TStartProxyRequestClient.OpenOptions
    stream.Free;
    // abort all progressive clients associated with this failed download
    proxy.fOwner.fPartials.Abort(id);
    if FileExists(filename) and
       not DeleteFile(filename) then
      log.Add.Log(sllLastError, 'MakeGet: delete failed %', [filename], proxy);
    loginfo := 'connect';
    result := HTTP_BADGATEWAY;
  end;
end;

procedure THttpProxyUrl.BackgroundGet(Sender: TObject);
var
  back: TStartProxyRequestClient absolute Sender;
  status: integer;
  msg: RawUtf8;
  fn: TFileName; // local copy
  id: THttpPartialID;
  expected, actual: Int64;
  ok: boolean;
begin
  status := 0;
  actual := -1;
  msg := 'exception';
  ok := false;
  fn := back.filestream.FileName;
  id := back.partialid;
  expected := back.expectedsize;
  try
    // make the actual HTTP/HTTPS blocking huge GET in this thread
    status := back.Request(back.uri, 'GET',
      fSettings.HttpKeepAlive * MilliSecsPerSec, '', '', '',
      {AsRetry=}false, nil, back.writestream);
    actual := back.filestream.Size;
    if status in HTTP_GET_OK then
      if actual = expected then
      begin
        msg := 'ok';
        ok := true;
      end
      else
        msg := 'size mismatch'
    else
      msg := 'unexpected status';
  finally
    // stop writing before notifying progressive readers of a failure
    if back.writestream <> back.filestream then
      back.writestream.Free // TStreamRedirect.Destroy releases filestream
    else
      back.filestream.Free;
    back.Free;
    fOwner.fLog.Add.Log(sllInfo, 'BackgroundGet=%: % [%] size=% expected=%',
      [status, fn, msg, actual, expected], self);
    if not ok then
    begin
      // abort all progressive clients associated with this download
      fOwner.fPartials.Abort(id);
      fOsSafe.Lock; // as in OnGetHeadRemoteUri / MakeGet (non-rentrant lock)
      try
        if FileExists(fn) and
           not DeleteFile(fn) then
          fOwner.fLog.Add.Log(sllLastError,
            'BackgroundGet: delete failed %', [fn], self);
      finally
        fOsSafe.UnLock;
      end;
    end;
  end;
end;

function THttpProxyUrl.ReturnFile(ctxt: THttpServerRequestAbstract;
  const hash: THash160; const filename: TFileName;
  const uri: TUriMatchName; var size: Int64; lastmod: TUnixMSTime;
  var loginfo: PUtf8Char; canbecached: boolean): cardinal;

  procedure FromCache;
  var
    cached: RawByteString;
  begin
    if fMemCache.FindAndCopy(hash, cached) then
      loginfo := 'memcache'
    else
    begin
      cached := StringFromFile(filename);
      if canbecached then
        fMemCache.Add(hash, cached);
    end;
    ctxt.ExtractOutContentType; // reverse ctxt.SetOutFile(fn)
    ctxt.OutContent := cached;
  end;

var
  pck: THttpProxyCacheKind;
  with304: boolean;
begin
  // prepare file streaming as response
  with304 := not (hpoDisable304 in fSettings.Options);
  if fSource <> hpsRemoteUri then
    // from hpsLocalFolder: check local file size+date attributes
    result := ctxt.SetOutFile(filename, with304, '',
      fSettings.CacheControlMaxAgeSec, @size)
  else
  begin
    // from hpsRemoteUri: we have the resource size+date/etag attributes
    result := ctxt.SetOutFile(filename, with304,
      size, lastmod, fSettings.CacheControlMaxAgeSec);
    if (result = HTTP_SUCCESS) or
       (result = HTTP_NOTMODIFIED) then
      FileSetDateFromUnixUtc(filename, UnixTimeUtc); // touch file timestamp
  end;
  loginfo := 'direct';
  // try to use the memory cache content (for GET, not HEAD)
  if (result <> HTTP_SUCCESS) or
     IsHead(ctxt.Method) or
     not Assigned(fMemCache) then
    exit;
  pck := fSettings.MemCache.FromUri(uri);
  if not (pckIgnore in pck) then
    if (pckForce in pck) or
       (size <= Int64(fSettings.MemCache.MaxSizeKB) shl 10) then
      FromCache;
end;

function THttpProxyUrl.ReturnLocalFile(ctxt: THttpServerRequestAbstract;
  const uri: TUriMatchName; name: PUtf8Char; namelen: PtrInt;
  var filename: TFileName; var hash: THash256Rec; var size: Int64;
  var loginfo: PUtf8Char): cardinal;
var
  sha: TSha256;
begin
  if fSource = hpsFastCgi then // front controller static files
    MakePath([fFastCgiFolder, name], filename)
  else
    MakePath([fSettings.fLocalFolder, name], filename);
  sha.Full(name, namelen, hash.b);
  result := ReturnFile(ctxt, hash.sha1, filename, uri, size, {lastmod=}0, loginfo);
end;


{ THttpProxyServerMainSettings }

constructor THttpProxyServerMainSettings.Create;
begin
  inherited Create;
  fThreadCount := SystemInfo.dwNumberOfProcessors + 1;
  fPort := '8098';
end;

function THttpProxyServerMainSettings.SetupTls(var tls: TNetTlsContext): boolean;
begin
  result := (self <> nil) and
            ((fCertificateFile <> '') or
             (fCACertificatesFile <> ''));
  if not result then
    exit;
  if tls.PrivatePassword = '' then
    tls.PrivatePassword := fPrivateKeyPassword;
  InitNetTlsContext(tls, {server=}true, fCertificateFile,
    fPrivateKeyFile, tls.PrivatePassword, fCACertificatesFile);
end;


{ THttpProxyServerSettings }

constructor THttpProxyServerSettings.Create;
begin
  // redirect to AutoCreateFields(self);
  inherited Create;
  // set default values in this main instance
  fDiskCache.Path := Executable.ProgramFilePath + 'proxycache';
  fDiskCache.TimeoutSec := 4 * SecsPerHour; // TTL on disk = 4 hours
  fMemCache.MaxSizeKB := 4;
  fMemCache.TimeoutSec := 15 * SecsPerMin;  // TTL in memory = 15 minutes
end;

function THttpProxyServerSettings.AddUrl(
  one: THttpProxyUrlSettings): THttpProxyUrlSettings;
begin
  result := one;
  if result <> nil then
    if (result.Source = '') and
       not Assigned(result.OnRequest) then
      FreeAndNil(result)
    else
      // will be owned as a new TSynAutoCreateFields fUri[] instance
      PtrArrayAdd(fUrl, result);
end;

function THttpProxyServerSettings.AddFolder(const folder: TFileName;
  const uri: RawUtf8; RaiseExceptionOnNonExistingFolder: ESynExceptionClass): THttpProxyUrlSettings;
begin
  if RaiseExceptionOnNonExistingFolder <> nil then
    if not DirectoryExists(folder) then
      RaiseExceptionOnNonExistingFolder.RaiseUtf8(
        '%.AddFolder: % does not exist', [self, folder]);
  result := THttpProxyUrlSettings.Create;
  result.Url := uri;
  if RaiseExceptionOnNonExistingFolder = nil then
    RaiseExceptionOnNonExistingFolder := EHttpProxyServer;
  result.Source := StringToUtf8(EnsureDirectoryExists(
    folder, RaiseExceptionOnNonExistingFolder));
  result := AddUrl(result);
end;

function THttpProxyServerSettings.AddProxy(
  const remoteuri, localuri: RawUtf8): THttpProxyUrlSettings;
begin
  if not IsHttp(remoteuri) then
    EHttpProxyServer.RaiseUtf8('%.AddRemote(%)', [self, remoteuri]);
  result := THttpProxyUrlSettings.Create;
  result.Url := localuri;
  result.Source := remoteuri;
  result := AddUrl(result);
end;

function THttpProxyServerSettings.AddFastCgi(
  const fcgiuri, localuri: RawUtf8): THttpProxyUrlSettings;
begin
  if not IdemPChar(pointer(fcgiuri), 'FCGI://') then
    EHttpProxyServer.RaiseUtf8('%.AddFastCgi(%)', [self, fcgiuri]);
  result := THttpProxyUrlSettings.Create;
  result.Url := localuri;
  result.Source := fcgiuri;
  result.Methods := [urmGet, urmHead, urmPost, urmPut, urmDelete,
    urmOptions, urmPatch];
  result := AddUrl(result);
end;

function THttpProxyServerSettings.AddEvent(
  const request: TOnHttpProxyServerRequest; const uri: RawUtf8): THttpProxyUrlSettings;
begin
  if not Assigned(request) then
    EHttpProxyServer.RaiseUtf8('%.AddEvent(nil)', [self]);
  result := THttpProxyUrlSettings.Create;
  result.Url := uri;
  result.OnRequest := request;
  result := AddUrl(result);
end;

function THttpProxyServerSettings.AddFromFiles(
  const settingsfolder, mask: TFileName): integer;
var
  F: TSearchRec;
  fn: TFileName;
  one: THttpProxyUrlSettings;
begin
  result := 0;
  if (settingsfolder = '') or
     (FindFirst(MakePath([settingsfolder, mask]), faAnyFile - faDirectory, F) <> 0) then
    exit;
  repeat
    if SearchRecValidFile(F) then
    begin
      MakePath([settingsfolder, F.Name], fn);
      one := THttpProxyUrlSettings.Create;
      if not JsonFileToObject(fn, one, nil, JSONPARSER_TOLERANTOPTIONS) then
        one.Free
      else if AddUrl(one) <> nil then
        inc(result); // successfully added this file
    end;
  until FindNext(F) <> 0;
  FindClose(F);
end;


{ THttpProxyServer }

constructor THttpProxyServer.Create(aSettings: THttpProxyServerSettings);
begin
  inherited Create; // call TSynAutoCreateFields
  fLog := TSynLog;
  if aSettings = nil then
  begin
    fSettings := THttpProxyServerSettings.Create;
    include(fFlags, fSettingsOwned);
  end
  else
    fSettings := aSettings;
end;

destructor THttpProxyServer.Destroy;
begin
  if fServer <> nil then
    fServer.Shutdown; // set flag ASAP
  Stop;
  if fSettingsOwned in fFlags then
    fSettings.Free;
  fSettings := nil;
  ObjArrayClear(fGC);
  inherited Destroy;
end;

procedure THttpProxyServer.Start(const aPrivateKeyPassword: SpiUtf8);
var
  {%H-}log: ISynLog;
  hso: THttpServerOptions;
  tls: TNetTlsContext;
  fav: RawByteString;
  i: PtrInt;
begin
  fLog.EnterLocal(log, 'Start %', [fSettings], self);
  if Assigned(log) then
    include(fFlags, fHasLog);
  if fServer <> nil then
    EHttpProxyServer.RaiseUtf8('Duplicated %.Start', [self]);
  // compute THttpAsyncServer options from settings
  hso := [hsoNoXPoweredHeader,  // we better hide ourself
          hsoIncludeDateHeader, // as most proxies do
          hsoThreadSmooting,    // won't hurt and seems the smoothest
          hsoIocpWriteDirect];  // better IOCP scaling when serving files
  if Assigned(log) and
     (psoLogVerbose in fSettings.Server.Options) then
    include(hso, hsoLogVerbose);
  if psoExcludeDateHeader in fSettings.Server.Options then
    exclude(hso, hsoIncludeDateHeader);
  if psoReusePort in fSettings.Server.Options then
    include(hso, hsoReusePort);
  if psoEnableLogging in fSettings.Server.Options then
    include(hso, hsoEnableLogging);
  if psoRejectBotUserAgent in fSettings.Server.Options then
    include(hso, hsoRejectBotUserAgent);
  if psoBan40xIP in fSettings.Server.Options then
    include(hso, hsoBan40xIP);
  tls.PrivatePassword := aPrivateKeyPassword; // if not in fSettings
  if (psoHttpsSelfSigned in fSettings.Server.Options) or
     SetupTls(tls) then
    include(hso, hsoEnableTls);
  // launch the HTTP(S) server
  fServer := THttpAsyncServer.Create(fSettings.Server.Port, nil, nil, '',
    fSettings.Server.ThreadCount, 30000, hso, fLog);
  if fSettings.Server.ServerName <> '' then
    fServer.ServerName := fSettings.Server.ServerName; // override 'mORMot (OS)'
  if fServer.Logger <> nil then
    fServer.Logger.Settings := fSettings.Server.Log;   // override default
  fav := StringFromFile(fSettings.Server.FaviconFile);
  if fav = '' then
  begin
    fav := 'default';
    // a front controller at the root has its own /favicon.ico file
    for i := 0 to high(fSettings.Url) do
      with fSettings.Url[i] do
        if (not Disabled) and
           (FastCgiIndex <> '') and
           ((Url = '') or
            (Url = '/')) then
          fav := '';
  end;
  if fav <> '' then
    fServer.SetFavIcon(fav); // do once
  fServer.IdleEverySecond; // initial setup of background processing
  fServer.OnIdle := OnIdle;
  if not (psoDisableGzip in fSettings.Server.Options) then
    fServer.RegisterCompress(CompressGZip);
  // setup the URI routes
  AfterServerStarted;
  if hpsRemoteUri in fSources then
  begin
    fPartials := THttpPartials.Create;
    if hsoLogVerbose in hso then
      fPartials.OnLog := fLog.DoLog;
    fServer.SetProgressiveRequests(fPartials);
  end;
  // wait for actual server availability
  if hsoEnableTls in hso then
    if psoHttpsSelfSigned in fSettings.Server.Options then
      fServer.WaitStartedHttps
    else
      fServer.WaitStarted(30, @tls)
  else
    fServer.WaitStarted;
  if Assigned(log) then
    log.Log(sllDebug, 'Start: %', [fServer], self);
  include(fFlags, fUrlSet); // now fUrl[] should remain stable
end;

procedure THttpProxyServer.Stop;
var
  i: PtrInt;
begin
  if fServer <> nil then
    with fLog.Enter('Stop %', [fServer], self) do
    begin
      fServer.Shutdown;
      // FastCGI connections do answer via fServer.AsyncResponse
      for i := 0 to high(fUrl) do
        if fUrl[i].fFastCgi <> nil then
          fUrl[i].fFastCgi.Shutdown;
      FreeAndNil(fServer);
    end;
  // FreeAndNil(fServer) joined the request/idle threads, so the count
  // cannot grow any more - detached workers may still use fUrl/fPartials
  while fBackgroundCount <> 0 do
    SleepHiRes(10);
  FreeAndNil(fPartials);
  exclude(fFlags, fUrlSet);
  ObjArrayClear(fUrl);
end;

procedure THttpProxyServer.BackgroundFinished(Sender: TObject);
begin
  LockedDec32(@fBackgroundCount); // called from background GET
end;

function THttpProxyServer.SetupTls(var tls: TNetTlsContext): boolean;
begin
  result := fSettings.Server.SetupTls(tls); // can be overriden if needed
end;

procedure THttpProxyServer.AfterServerStarted;
var
  uri, nfo: RawUtf8;
  new, old: TUriRouter;
  one: THttpProxyUrl;
  s: THttpProxyUrlSettings;
  hps: THttpProxySource;
  i: PtrInt;
begin
  fSources := [];
  fUrlOptions := [];
  fFlags := fFlags - [fHasCacheClean, fUrlSet];
  ObjArrayClear(fUrl);
  new := TUriRouter.Create(TUriTreeNode);
  try
    // 1. compute all routes from Settings[]
    for i := 0 to high(fSettings.Url) do
    begin
      s := fSettings.Url[i];
      if s.Disabled then
        continue;
      one := THttpProxyUrl.Create(s, self);
      PtrArrayAdd(fUrl, one);
      // validate source as local file folder or remote http(s) server
      hps := hpsUndefined;
      if Assigned(s.OnRequest) then
        // execute a callback event
        hps := hpsEvent
      else if s.Source <> '' then
        if IdemPChar(pointer(s.Source), 'FCGI://') then
        begin
          // FastCGI application as 'fcgi://server:port/folder'
          if one.fRemoteUri.From(s.Source) and
             (one.fRemoteUri.Layer in [nlTcp, nlUnix]) then
          begin
            hps := hpsFastCgi;
            one.fFastCgiRoot := one.fRemoteUri.Address;
            if (length(one.fFastCgiRoot) < 2) or
               (one.fFastCgiRoot[2] <> ':') then // 'c:/www' on Windows
              insert('/', one.fFastCgiRoot, 1);
            while (length(one.fFastCgiRoot) > 1) and
                  (one.fFastCgiRoot[length(one.fFastCgiRoot)] = '/') do
              SetLength(one.fFastCgiRoot, length(one.fFastCgiRoot) - 1);
            one.fFastCgi := TFastCgiAsyncConnections.Create(fServer,
              one.fRemoteUri, s.FastCgiTimeoutSec, fLog);
            if (s.FastCgiIndex <> '') and
               not (hpoFastCgiWsgi in s.Options) then
            begin
              // front controller: the root is also a local folder
              one.fFastCgiExt := ExtractExtU(s.FastCgiIndex); // e.g. '.php'
              Utf8ToFileName(one.fFastCgiRoot, one.fFastCgiFolder);
              one.fFastCgiFolder := IncludeTrailingPathDelimiter(
                NormalizeFileName(one.fFastCgiFolder));
            end;
          end;
        end
        else if IsHttp(s.Source) then
        begin
          // detects also 'http://unix:/path/to/socket.sock:/url/path'
          if one.fRemoteUri.From(s.Source) then
            hps := hpsRemoteUri;
        end
        else
        begin
          // should be a local existing folder name
          Utf8ToFileName(s.Source, s.fLocalFolder);
          if DirectoryExists(s.fLocalFolder) then
          begin
            s.fLocalFolder := IncludeTrailingPathDelimiter(s.fLocalFolder);
            hps := hpsLocalFolder;
          end;
        end;
      one.fSource := hps;
      if hps = hpsUndefined then
      begin
        fLog.Add.Log(sllWarning, 'AfterServerStarted: unexpected %', [one], self);
        continue;
      end;
      include(fSources, hps);
      fUrlOptions := fUrlOptions + s.fOptions;
      // normalize cache settings
      nfo := '';
      if not (hps in [hpsEvent, hpsFastCgi]) or
         (one.fFastCgiFolder <> '') then // front controller static files
      begin
        if not (psoDisableMemCache in fSettings.Server.Options) then
        begin
          if s.MemCache.MaxSizeKB < 0 then
            s.MemCache.MaxSizeKB := fSettings.MemCache.MaxSizeKB;
          if s.MemCache.TimeoutSec < 0 then
            s.MemCache.TimeoutSec := fSettings.MemCache.TimeoutSec;
          if (s.MemCache.MaxSizeKB > 0) and
             (s.MemCache.TimeoutSec > 0) then
            one.fMemCache := TSynDictionary.Create(TypeInfo(THash160DynArray),
              TypeInfo(TRawByteStringDynArray), PathCaseInsensitive,
              s.MemCache.TimeoutSec);
        end;
        if hps = hpsRemoteUri then
        begin
          if s.DiskCache.MaxSizeKB < 0 then
            s.DiskCache.MaxSizeKB := fSettings.DiskCache.MaxSizeKB;
          if s.DiskCache.Path = '' then
            s.DiskCache.Path := fSettings.DiskCache.Path;
          s.DiskCache.Path := EnsureDirectoryExists(s.DiskCache.Path);
          if s.DiskCache.TimeoutSec <= 0 then
            s.DiskCache.TimeoutSec := fSettings.DiskCache.TimeoutSec;
          if s.HttpHeadCacheSec > 0 then
            one.fHeadCache := TSynDictionary.Create(TypeInfo(THash160DynArray),
              TypeInfo(THeadCaches), {caseins=}false, s.HttpHeadCacheSec);
          if (s.DiskCache.Path <> '') and
             (s.DiskCache.TimeoutSec > 0) then
            include(fFlags, fHasCacheClean);
          Make([' in ', s.DiskCache.Path], nfo);
        end;
      end;
      // prepare optional hash cache
      one.fAlgos := [];
      if hps = hpsLocalFolder then
      begin
        if hpoPublishMd5 in s.Options then
          include(one.fAlgos, hfMd5);
        if hpoPublishSha1 in s.Options then
          include(one.fAlgos, hfSha1);
        if hpoPublishSha256 in s.Options then
          include(one.fAlgos, hfSha256);
        if one.fAlgos <> [] then
          one.fHashCache := TSynDictionary.Create(TypeInfo(TRawUtf8DynArray),
            TypeInfo(TRawUtf8DynArrayDynArray), PathCaseInsensitive, SecsPerHour);
      end;
      // compute and register this URI
      uri := s.fUrl;
      while (uri <> '') and
            (uri[length(uri)] = '/') do
         SetLength(uri, length(uri) - 1);
      if (uri <> '') and
         (uri[1] <> '/') then
        insert('/', uri, 1);
      one.fFastCgiPrefix := uri;
      if uri <> '' then
        new.Run(s.Methods, uri, OnExecute, one);
      new.Run(s.Methods, uri + '/', OnExecute, one);
      new.Run(s.Methods, uri + '/*', OnExecute, one);
      fLog.Add.Log(sllDebug, 'AfterServerStarted: register % URI as % from %%%',
        [uri, ToText(hps)^, s.fLocalFolder, one.fRemoteUri.URI, nfo], self);
    end;
    // 2. replace existing routes at once
    old := fServer.ReplaceRoute(new); // thread-safe
    new := nil; // is owned by fServer from now on
    if old <> nil then
      PtrArrayAdd(fGC, old); // delayed release at shutdown
  finally
    new.Free;
  end;
end;

procedure THttpProxyServer.OnIdle(Sender: TObject; NowTix: Int64);
var
  i, mem, hash, head: integer;
  tixmin: cardinal;
  one: ^THttpProxyUrl;
  tmp: TSynLogClass; // for Delphi 7 compilation
begin
  // delete any deprecated in-memory cached content - called every second
  if not (fUrlSet in fFlags) then
    exit; // called too early to access fUrl[] safely
  mem := 0;
  hash := 0;
  head := 0;
  one := pointer(fUrl);
  for i := 1 to length(fUrl) do
  begin
    inc(mem,  one^.fMemCache.DeleteDeprecated(NowTix));
    inc(hash, one^.fHashCache.DeleteDeprecated(NowTix));
    inc(head, one^.fHeadCache.DeleteDeprecated(NowTix));
    if one^.fFastCgi <> nil then
      one^.fFastCgi.Idle(NowTix);
    inc(one);
  end;
  if mem + hash + head <> 0 then
    fLog.Add.Log(sllTrace, 'OnIdle: gc mem=% hash=% head=%',
      [mem, hash, head], self);
  // delete deprecated file content in background thread - every 17 minutes
  if not (fHasCacheClean in fFlags) then
    exit;
  tixmin := (NowTix shr 20) + 1; // check folder every 1048 seconds
  if fTempFilesTix = tixmin then
    exit;
  fTempFilesTix := tixmin;
  tmp := nil;
  LockedInc32(@fBackgroundCount);
  try
    TLoggedWorkThread.Create(tmp, 'cacheclean', nil,
      OnBackgroundDeleteDeprecated, BackgroundFinished);
  except
    LockedDec32(@fBackgroundCount);
    raise;
  end;
end;

procedure THttpProxyServer.OnBackgroundDeleteDeprecated(Sender: TObject);
var
  size: Int64;
  one: ^THttpProxyUrl;
  cache: THttpProxyDisk;
  i, n: integer;
  ok: boolean;
begin // folder timestamp check is called every 17 minutes, and may be slow
  if not (fUrlSet in fFlags) then
    exit; // called too early to access fUrl[] safely
  one := pointer(fUrl);
  for i := 1 to length(fUrl) do
  begin
    cache := one^.fSettings.DiskCache;
    if (one^.fSource = hpsRemoteUri) and
       (cache.Path <> '') and
       (cache.TimeoutSec > 0) then
    begin
      n := 0;
      size := 0;
      ok := DirectoryDeleteOlderFiles(cache.Path,
        cache.TimeoutSec * SecsPerDate, FILES_ALL,
        {recursive=}hpoClientCacheSubFolder in one^.fSettings.Options, @size);
      if not (fUrlSet in fFlags) then
        exit; // paranoid abort at Shutdown
      if (n <> 0) or
         not ok then // something changed on disk
        fLog.Add.Log(sllTrace, 'OnIdle: remote=% delete=% old=%=%',
          [one^.fRemoteUri.Server, BOOL_STR[ok], n, KBNoSpace(size)], self);
    end;
    inc(one);
  end;
end;

function THttpProxyServer.OnGetHeadLocalFolder(Ctxt: THttpServerRequest;
  const Uri: TUriMatchName): cardinal;
var
  fn: TFileName;
  path, name, html: RawUtf8;
  siz: Int64;
  one: THttpProxyUrl;
  opt: THttpProxyUrlOptions;
  loginfo: PUtf8Char;
  dig: THash256Rec; // 160-bit truncated SHA-256 hash of resource name
begin
  // supplied URI should be a safe resource reference
  result := HTTP_FORBIDDEN; // 403
  one := Ctxt.RouteOpaque;
  opt := one.Settings.Options;
  // try to assign a local file to the output Ctxt
  FastSetString(path, Uri.Path.Text, Uri.Path.Len); // with no ?query
  UrlDecodeVar(pointer(path), length(path), name, {space=}'+'); // needs #0
  NormalizeFileNameU(name);
  if not SafePathNameU(name) then
    exit;
  if hpoNoSubFolder in opt then
    if PosExChar(PathDelim, name) <> 0 then
      exit;
  // stream the content from local file
  result := one.ReturnLocalFile(Ctxt, Uri, pointer(name), length(name),
    fn, dig, siz, loginfo);
  // additional response types
  case result of
    HTTP_NOTFOUND:
      // this URI is no file, but may be a folder
      if (siz < 0) and // siz=-1 for folder
         not (hpoNoFolderHtmlIndex in opt) then
      begin
        // return the folder files info as cached HTML
        if (hpoDisableFolderHtmlIndexCache in opt) or
           not one.fMemCache.FindAndCopy(dig.sha1, html) then
        begin
          FolderHtmlIndex(fn, Ctxt.Url, StringReplaceChars(name, PathDelim, '/'),
            html, hpoNoSubFolder in opt);
          if Assigned(one.fMemCache) and
             not (hpoDisableFolderHtmlIndexCache in opt) then
            one.fMemCache.Add(dig.sha1, html); // first inserted wins
        end;
        loginfo := 'html';
        result := Ctxt.SetOutContent(
                    html, not (hpoDisable304 in opt), HTML_CONTENT_TYPE);
      end
      else if siz = 0 then
        // check URI for any .md5/.sha1/.sha256 hash extension
        if Assigned(one.fHashCache) then
          result := one.ReturnHash(Ctxt, name, fn);
  end; // may be e.g. HTTP_SUCCESS (200) or HTTP_NOTMODIFIED (304)
  if not StatusCodeIsSuccess(result) then // in 4xx.. range
    Ctxt.SetErrorMessage('serving %', [name]);
  if fHasLog in fFlags then
    fLog.Add.Log(sllDebug, 'OnExecute: % % fn=% status=% size=% %',
      [Ctxt.Method, Ctxt.Url, fn, result, siz, loginfo], self);
end;

function THttpProxyServer.OnGetHeadRemoteUri(Ctxt: THttpServerRequest;
  var Uri: TUriMatchName): cardinal;
var
  req: TStartProxyRequest;
  start: Int64;
  pid: THttpPartialID;
  localdate: TUnixMSTime;
  instance: TObject;
begin
  if fHasLog in fFlags then
    QueryPerformanceMicroSeconds(start);
  // supplied URI should be a safe resource reference
  result := HTTP_FORBIDDEN; // 403
  req.ctxt := Ctxt;
  req.proxy := Ctxt.RouteOpaque;
  req.loginfo := nil;
  // compute the remote URI and its hash corresponding to the original server
  if hpoNoSubFolder in req.proxy.Settings.Options then
    if ByteScanIndex(pointer(Uri.Path.Text), Uri.Path.Len, ord('/')) <> 0 then
      exit;
  req.remote := req.proxy.fRemoteUri;
  if hpoClientLowerCaseUri in req.proxy.Settings.Options then
    CaseBuffer(Uri.Path.Text, Uri.Path.Len, @NormToLowerAnsi7); // normalize
  if (Uri.Path.Len <> 0) and
     (req.remote.Address <> '') then
    AppendIfNone(req.remote.Address, '/'); // Source may have no trailing /
  Append(req.remote.Address, Uri.Path.Text, Uri.Path.Len);
  if not HttpRequestHashBase32(req.remote, nil, nil, @req.remotehash,
           hpoClientNormalizeCaseHash in req.proxy.Settings.Options) then
    exit; // paranoid
  // blocking to ensure file consistency and remote connection sharing
  req.proxy.fOsSafe.Lock;
  try
    // retrieve the headers, from cache or HEAD, and compute the local file name
    // from hashed URI + header etag/lastmod
    result := req.MakeHeadAndComputeFilename;
    Ctxt.OutCustomHeaders := req.head.PurgedHeaders;
    if IsHead(Ctxt.Method) then
    begin
      // no actual file is needed to implement a HEAD request
      if req.loginfo = nil then
        req.loginfo := 'head';
      if result in [HTTP_SUCCESS, HTTP_PARTIALCONTENT] then // regular response
        Ctxt.SetOutProgressiveFile(req.filename, req.head.Size)
      else if (result <> HTTP_NOCONTENT) and
              (req.head.Size >= 0) then
        Ctxt.AddOutHeader(['Content-Length: ', req.head.Size]);
    end
    else if result in HTTP_GET_OK then // 2xx
    begin
      result := HTTP_NOTFOUND; // default to trigger req.MakeGet(Uri)
      // check any local file
      if (req.filename <> '') and
         FileInfoByName(req.filename, req.localsize, localdate) and
         (req.localsize >= 0) then
        // we have a local cached file
        if (req.head.Size >= 0) and
           (req.localsize = req.head.Size) then
          // it is safe to assume a complete local file is correct (as 200/304)
          result := req.proxy.ReturnFile(Ctxt, req.head.Hash160, req.filename,
            Uri, req.localsize, req.head.TimeMS, req.loginfo)
        else if fPartials.Find(PHashDigest(@req.head.HashDigest)^,
                  req.localsize, @pid, Ctxt.ConnectionHttp) = req.filename then
        begin
          // there is progressive/partial match
          Ctxt.SetOutProgressiveFile(req.filename, req.localsize);
          Make(['part-', pid], req.head.PurgedHeaders); // temp
          req.loginfo := pointer(req.head.PurgedHeaders);
          result := HTTP_SUCCESS;
        end
        else
        begin
          // delete aborted/invalid file on disk and retry from scratch
          fLog.Add.Log(sllWarning, 'OnExecute: delete=% disk=% expected=%',
            [req.head.HashB32, req.localsize, req.head.Size], self);
          if not DeleteFile(req.filename) then
            fLog.Add.Log(sllLastError, 'OnExecute: delete=%', [req.filename], self);
          result := HTTP_NOTFOUND;
        end;
      // check if no matching local file
      if not StatusCodeIsSuccess(result) then // in 4xx.. range
        result := req.MakeGet(Uri); // initiate a GET proxy request
    end;
  finally
    req.proxy.fOsSafe.UnLock;
  end;
  if (req.loginfo <> nil) and
     not StatusCodeIsSuccess(result) then // in 4xx.. range
    Ctxt.SetErrorMessage('proxy %', [req.loginfo])
  else if (req.head.HashB32[0] <> #0) and
          not (hpoNoXProxyName in req.proxy.Settings.Options) then
    Ctxt.AddOutHeader(['X-Proxy-Name: ', req.head.HashB32]);
  if not (fHasLog in fFlags) then
    exit;
  instance := nil;
  if fLog.HasLevel([sllTrace, sllDebug]) then
    instance := self; // include THttpProxyServer context in verbose logs
  fLog.Add.Log(sllInfo, 'OnExecute: % % ip=% fn=% status=% size=% info=% in %',
    [Ctxt.Method, Ctxt.Url, Ctxt.RemoteIP, req.head.HashB32, result,
     req.localsize, req.loginfo, MicroSecFrom(start)], instance);
end;

function THttpProxyServer.OnFastCgiIndex(Ctxt: THttpServerRequest;
  const Uri: TUriMatchName; One: THttpProxyUrl; var Path: TSynTempAdder): cardinal;
var
  name: PUtf8Char;
  namelen, scriptlen: PtrInt;
  fn: TFileName; // as needed by the file system API
  siz: Int64;
  ts: TUnixMSTime;
  dig: THash256Rec;
  loginfo: PUtf8Char;
begin
  // returns 0 to send Path (or Uri.Path if Path.Size = 0) to the application
  result := 0;
  if Uri.Path.Len = 0 then
  begin
    Path.Add(One.Settings.FastCgiIndex); // the root index script
    exit;
  end;
  // URI-decode the path within Path itself, ending with #0 as expected
  Path.Add(Uri.Path.Text, Uri.Path.Len);
  Path.AddDirect(#0);
  name := Path.Buffer;
  namelen := DoUrlDecode(pointer(name), pointer(name), '+') - name;
  name[namelen] := #0;
  scriptlen := FastCgiScriptLen(name, namelen, One.fFastCgiExt);
  if scriptlen = 0 then // not a script path
    if name[namelen - 1] = '/' then
    begin
      // a folder: is sent to its own index script, if any
      Path.Size := namelen;
      Path.Add(One.Settings.FastCgiIndex);
      Path.AddDirect(#0);
      MakePath([One.fFastCgiFolder, PUtf8Char(Path.Buffer)], fn); // may realloc
      Path.Size := 0;
      if FileExists(fn) then
        Path.Add(Uri.Path.Text, Uri.Path.Len); // still URI-encoded
      Path.Add(One.Settings.FastCgiIndex); // or the root index script
    end
    else
    begin
      // an existing file is returned as from a local folder
      if IsGet(Ctxt.Method) or
         IsHead(Ctxt.Method) then
      begin
        result := One.ReturnLocalFile(Ctxt, Uri, name, namelen,
          fn, dig, siz, loginfo);
        if result = HTTP_NOTFOUND then // 200 or 304, or 404 if no such file
          result := 0;
      end
      else
      begin
        MakePath([One.fFastCgiFolder, name], fn);
        if FileInfoByName(fn, siz, ts) and
           (siz >= 0) then
          result := HTTP_NOTALLOWED; // not a folder: static file only for GET
      end;
      if result = 0 then
      begin
        Path.Size := 0;
        Path.Add(One.Settings.FastCgiIndex); // the root index script
      end;
    end
  else if scriptlen < 0 then
    result := HTTP_NOTFOUND // unsafe path
  else
    Path.Size := 0; // a script: Uri.Path is sent as it is
end;

function THttpProxyServer.OnFastCgi(Ctxt: THttpServerRequest;
  const Uri: TUriMatchName): cardinal;
var
  one: THttpProxyUrl;
  len: integer;
  start: Int64;
  p: PUtf8Char;
  plen: PtrInt;
  path, params, request: TSynTempAdder; // no heap allocation for most requests
begin
  QueryPerformanceMicroSeconds(start);
  len := 0;
  one := Ctxt.RouteOpaque;
  p := Uri.Path.Text;
  plen := Uri.Path.Len;
  // optional front controller: static files and index scripts
  result := 0;
  path.Init;
  if one.fFastCgiFolder <> '' then
  begin
    result := OnFastCgiIndex(Ctxt, Uri, one, path);
    if path.Size <> 0 then
    begin
      p := path.Buffer;
      plen := path.Size;
    end;
  end;
  if result = 0 then
  begin
    // compute the CGI variables from this request
    result := HTTP_NOTFOUND; // unsafe path or no script
    params.Init;
    if FastCgiAppendParams(params, Ctxt, one.fFastCgiRoot, one.fFastCgiPrefix,
         fServer.SockPort, p, plen, hpoFastCgiWsgi in one.Settings.Options,
         one.fFastCgiExt) then
    begin
      // send to the application: its response will come via fServer.AsyncResponse
      request.Init;
      FastCgiAppendRequest(request, params.Buffer, params.Size, Ctxt.InContent);
      len := request.Size;
      if one.fFastCgi.Send(Ctxt.AsyncHandle, request.Buffer, len) then
        result := HTTP_ASYNCRESPONSE
      else
        result := HTTP_BADGATEWAY; // the application could not be reached
      request.Store.Done;
    end;
    params.Store.Done;
  end;
  path.Store.Done;
  if fHasLog in fFlags then
    fLog.Add.Log(sllDebug, 'OnFastCgi: % % status=% len=% in %',
      [Ctxt.Method, Ctxt.Url, result, len, MicroSecFrom(start)], self);
end;

function THttpProxyServer.OnExecute(Ctxt: THttpServerRequestAbstract): cardinal;
var
  one: THttpProxyUrl;
  uri: TUriMatchName;
  met: TUriRouterMethod;
begin
  // retrieve O(1) execution context
  result := HTTP_NOTFOUND; // 404 by default
  one := Ctxt.RouteOpaque;
  if (one = nil) or
     one.Settings.Disabled or
     (one.Source = hpsUndefined) then
    exit;
  // validate the request method
  result := HTTP_NOTALLOWED; // 405 Method Not Allowed
  if not (UriMethod(Ctxt.Method, met) and
          (met in one.Settings.Methods)) then
    exit;
  // retrieve path and resource/file name from URI
  Ctxt.RouteAt(0, uri.Path);
  if uri.Path.Len > 512 then // obviously invalid
    exit;
  uri.ParsePath; // compute uri.Name for file-level TUriMatch
  // ensure was not marked as rejected
  result := HTTP_FORBIDDEN; // 403
  if (one.Settings.RejectCsv <> '') and
     one.fReject.Check(one.Settings.RejectCsv, uri, PathCaseInsensitive) then
    exit;
  // actual request processing
  result := HTTP_NOTALLOWED; // 405 Method Not Allowed
  case one.Source of
    hpsEvent:
      result := one.Settings.OnRequest(Ctxt, uri, met, one);
      // no cache nor method filtering for callback (by design)
    hpsFastCgi:
      result := OnFastCgi(THttpServerRequest(Ctxt), uri);
  else
    case met of
      urmGet,
      urmHead:
        case one.Source of
          hpsLocalFolder:
            result := OnGetHeadLocalFolder(THttpServerRequest(Ctxt), uri);
          hpsRemoteUri:
            result := OnGetHeadRemoteUri(THttpServerRequest(Ctxt), uri);
        end;
      urmPost,
      urmPut,
      urmDelete:
          ; { TODO: implement proxy with POST/PUT/DELETE }
    end;
  end;
end;

function ToText(hps: THttpProxySource): PShortString;
begin
  result := GetEnumName(TypeInfo(THttpProxySource), ord(hps));
end;


initialization
  {$ifndef HASDYNARRAYTYPE}
  Rtti.RegisterObjArray(
    TypeInfo(THttpProxyUrlObjArray), THttpProxyUrl);
  Rtti.RegisterObjArray(
    TypeInfo(THttpProxyUrlSettingsObjArray), THttpProxyUrlSettings);
  {$endif HASDYNARRAYTYPE}

end.
