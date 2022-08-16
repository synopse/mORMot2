# Mustaches on the Web

This is a sample extracted from a [Delphi Praxis Forum page](https://www.delphipraxis.net/211114-mormot-mustache-editor-mit-integriertem-http-server-zum-anzeigen-von-html-seiten.html).

With the example code you get:
- A simple mustache editor (TMemo/TSynEdit component) with auto-completion for common HTML tags.
- An integrated HTTP server for HTTP/1.0, HTTP/1.1, HTTPS with self-signed or own certificate.
- The Edge web browser for viewing HTML pages with undockable stand-alone window.
- Direct linking of images, CSS, JavaScript from ZIP files into the HTML page.
- And some little things more...

Mustache Editor Instruction Manual:
- Edge Browser is used for the display. With the function key "F5" the display is refreshed. With "F6" the project data are saved. If a project name has been assigned, it is saved immediately.
- Images, CSS, JavaScript, JSON or ZIP files are loaded only from the assets directory. The folder structure/directory depth in a ZIP file is not limited. Example of a path is: Bootstrap.zip/bootstrap.min.css
- Partials for Mustache Renderer can only be loaded from the Partials directory. Partials correspond to Pascal include files.
- The Bootstrap files "bootstrap.min.css" and "bootstrap.bundle.min.js" are always added.
- External files are loaded in order of appearance. Possible to specify the order of Bootstrap files by entering "*.css" or "*.js" in the order.
- JavaScript files are loaded with option "defer". This behavior can be influenced. By prepending a "!" directly to the file name, it is loaded immediately. Prefixing a "?" will load with the "async" option.
- The root for loaded JSON data is the filename. The JSON object from the Person.json file is addressed with "Person.FirstName".
- The auto-complete for Mustache Editor covers common HTML tags. The definitions are stored in the file HtmlAutoComplete.dci.
- The browser window can be undocked and placed as a stand-alone view.
    
## Source code

The Source code has been extracted from the original `.zip` attached with the article.

# Translated Documentation

Just copy and pasted from Google Translator, with a quick manual review and formatting.

## Presentation

In the fifth article of the mORMot introduction series, two components of the library are discussed. One is the HTTP server classes, the other is the Mustache template engine.

Mustache is called Logic-less, with the reasoning: " We call it logic-less because there are no if statements, else clauses, or for loops ". It works by expanding tags that represent data - none, single or whole lists - in a template. This tool is powerful when creating HTML for WebApps.

The contained references to the documentation link to the currently [available mORMot1 help](https://translate.google.com/website?sl=auto&tl=en&hl=de&client=webapp&u=https://synopse.info/files/html/Synopse+mORMot+Framework+SAD+1.18.html). mORMot2 is used for the example. Class and function names may differ slightly. Attached is the source code and the executable program. Disclaimer: The source code is neither tested nor optimized. It should work with Delphi 10.4 or later. The use of the materials provided is at your own risk.

The sample application is a simple editor. Depending on the compiler directive, the TMemo or TSynEdit component is used. The input is prepared with the help of the mORMot Mustache Renderer and as HTML via the integrated HTTP serverPage rendered in Edge browser. With the example source code for the program you get:
- A built-in HTTP server for HTTP/1.0, HTTP/1.1, HTTPS with self-signed or custom certificate.
- The Edge web browser for displaying HTML pages with dockable stand-alone windows.
- Direct linking of images, CSS, JavaScript from ZIP files into the HTML page.
- A simple mustache editor with auto-complete for common HTML tags.

## Prior knowledge

HTTP server and REST server are separate in mORMot, two concepts that can each be used independently.

The classes `THttpServer`, `THttpAsyncServer` and `THttpApiServer` are available for implementing an HTTP server. The `THttpServer` is a socket-based server. It uses a thread pool for short-lived HTTP/1.0 requests and one thread per connection for HTTP/1.1. The `THttpAsyncServer` is fully event driven and capable of handling thousands of simultaneous connections. Only available under Windows, `THttpApiServer` is based on the `http.sys` module. This module is also used in IIS and .NET. 

The socket-based servers support TLS natively. OpenSSL is used if present, otherwise the Windows Schannel API. If you are interested in more, the two articles [New Async HTTP/WebSocket Server on mORMot 2](https://translate.google.com/website?sl=auto&tl=en&hl=de&client=webapp&u=https://blog.synopse.info/?post/2022/05/21/New-Async-HTTP/WebSocket-Server-on-mORMot-2) and [Native TLS Support for mORMot 2 REST or WebSockets Servers](https://translate.google.com/website?sl=auto&tl=en&hl=de&client=webapp&u=https://blog.synopse.info/?post/2022/07/09/Included-TLS-Support-for-mORMot-REST-or-WebSockets) are recommended.

## HTTP server

For our example, the class `THttpServer` is used. The operating mode of the server can be determined with a command line switch or via a selection dialog. You can choose between: HTTP/1.0, HTTP/1.1, HTTPS with a self-signed certificate or, if available, with your own certificate. The command line switch is `-sm` with the options `hsmHttp10 | hsmHttp11 | hsmHttpsSelf`.

The callback function where all higher-level processes come together is the OnRequest event . The prototype is defined in the mormot.net.http unit as follows:
```
type
  TOnHttpServerRequest = function (Ctxt: THttpServerRequestAbstract): cardinal of object ;
```

Input parameters are accepted via the `THttpServerRequest` class and the output arguments, such as header, body and status code, are filled with content that is then sent as a response. Specifically, you take the input URI and break it down into its parts. Here we use path ( path ) and query ( query ). In contrast to the REST servers, method or interface based, there is no automatic routing and you have to do it yourself. The selectors in the example are " INDEX.HTML " and " .ZIP/". More specialization is not necessary. The rest is fed into a standard function.

```
function TFileServer.DoRequest(pmCtxt: THttpServerRequestAbstract): Cardinal;
var
  urlPath: RawUtf8;
  urlQuery: RawUtf8;
begin
   if not SplitUrlSchemePathAndQuery(pmCtxt.Url, urlPath, urlQuery) then Exit(HTTP_BADREQUEST); //=>

   if IdemPChar(PUtf8Char(Pointer(urlPath)), DEFAULTHTML_FILENAME) then
    Result := DoDefaultFileRequest(pmCtxt, urlPath)
   else if PosI(ZIPFILE_SEPERATOR, urlPath) > 0 then
    Result := DoZipFileRequest(pmCtxt, urlPath)
   else
    Result := DoFileRequest(pmCtxt, urlPath);
end ;
```

Standard because an optimized procedure for transferring files is implemented in the `THttp##Server` classes. The mechanism is activated when `OutContentType` is assigned the constant `STATICFILE_CONTENT_TYPE`. Furthermore, the file name is passed to `OutContent` and the appropriate `OutCustomHeaders` are selected at the end.

```
function TFileServer.DoFileRequest(pmCtxt: THttpServerRequestAbstract; const pmcUriPath: RawUtf8): Cardinal;
var
  fileName: TFileName;
begin
  Utf8ToFileName(pmcUriPath, fileName);
   if CheckFileName(fileName, [fnvFileName], @fileName) then
   begin
    fileName := MakePath([Executable.ProgramFilePath, ASSETS_FOLDER, fileName]);
    pmCtxt.OutContent := StringToUtf8(fileName);
    pmCtxt.OutContentType := STATICFILE_CONTENT_TYPE;
    pmCtxt.OutCustomHeaders := STATICFILE_CONTENT_TYPE_HEADER + #13#10 + GetMimeContentTypeHeader(' ', fileName);
    Result := HTTP_SUCCESS;
   end
   else
    Result := HTTP_NOTFOUND;
end ;
```

In the article, the query part of a URI is not used. Nevertheless, I would like to use an example to show how it can be broken down into its individual parts. It uses some low-level functions that exist in the library for this task. In REST servers, more specialized functions are available via the `TRestServerUriContext` class. The order of the parameters is arbitrary. Only the last `UrlDecode##()` function (look very carefully!) with the transfer of the address is particularly important.

```
const
  PARAMS: RawUtf8 = ' u=Text&i=10&d=8.8 ';
var
  p: PUtf8Char;
  u: RawUtf8;
  i: integer;
  d: double;
begin
  p := PUtf8Char(Pointer(PARAMS));
   if UrlDecodeNeedParameters(p, ' U,I,D ') then
   begin
     repeat
      UrlDecodeValue(p, ' U= ', u);
      UrlDecodeDouble(p, ' D= ', d);
      UrlDecodeInteger(p, ' I= ', i, @p);
     until p = Nile ;

    ShowMessage(Utf8ToString(FormatUtf8(' String: % '#13#10' Integer: % '#13#10' Double: % ', [u, i, d])));
   end ;
```

The instantiation of an HTTP server is done with a few lines:

```
FHttpServer := THttpServer.Create( {Port=}  ' 8080 ', Nil , Nil , ' ', {ServerThreadPoolCount=}  2, {KeepAliveTimeOut=}  0);
FHttpServer.WaitStarted(WAIT_SECONDS);
```
Nothing else can be shown, that's it. An HTTP/1.0 server running on port 8080 with a maximum of 2 threads. If I showed how easy it would be to enable TLS, users of other libraries could. I don't want to take on that responsibility.

## Mustache Template Engine

The template language consists of only a few elements, they are Variables, Sections and Partials. All mustache tags are enclosed in double curly brackets `{{...}}`. 
A `FirstName` variable is thus encoded as follows: `{{FirstName}}`.
A section represents a loaded context. In practice a JSON object or array. A section of the type "if ..." starts with `{{#...}}`, an "if not ..." with `{{^...}}`. Sections must end with `{{/...}}`. 

If the context is a list, it will be traversed from the first to the last element and applied. The section is also the root for the variables. 

Example: A list of personal data (objects) is to be run through and the name (variable) of each person is to be output. Solution: ```{{#Persons}}{{Name}}<br>{{/Persons}}```. Outside of a section, the full path must be specified for variables. For the context person: ```{{Person.Name}}```.

Partials correspond to Pascal include files. Everything important can be read here: [Mustache template engine](https://translate.google.com/website?sl=auto&tl=en&hl=de&client=webapp&u=https://synopse.info/files/html/Synopse%2520mORMot%2520Framework%2520SAD%25201.18.html%23TITL_81). 

All theory is gray - this is how it looks in practice:

```
<!-- Hero is loaded from the Partials folder -->
{{>Hero}}
{{#Person}}
<!-- CSS class h3 is defined in Bootstrap -->
<p class="h3">Person data </p>
<ul>
  <li>Name: {{FirstName}} {{LastName}}</li>
  <!-- CurrToText is a self-written Expression Helper to format money output -->
  <li>Income: {{CurrToText Income}}</li>
</ul>
{{/Person}}
{{^Person}}
<p>If this is displayed, the Person data object was not found. </p>
{{/Person}}
Cheers {{Person.FirstName}}
```

The language can be expanded with its own functions (expression helpers). These are registered in a list of type `TSynMustacheHelpers` and passed to the processor. Its structure is as follows:

```
procedure TFileServer.CurrToText( const pmcValue: Variant; out pmoResult: Variant);
begin
  pmoResult := CurrToStrF(pmcValue, ffCurrency, 2);
end ;
```

Equipped with this prior knowledge, now for concrete implementation in the application. We assemble the HTML page for display in the Edge web browser . The mustache renderer part is just a few lines:

```
var
  writer: TTextWriter;
  contextData: TDocVariantData;
begin
  ...
  PrepareMustachePartials;
  PrepareMustacheContextData(pmcProjectData.MustacheDataFiles, contextData);
  writer.AddString(TSynMustache.Parse(pmcProjectData.Mustache).Render(Variant(contextData), FMustachePartials, FMustacheHelpers));
```

The genius of a `DocVariant` is that it can be an arbitrarily complex data structure made up of object(s) and/or arrays, or a combination of both. Only the available memory is the limit. The compilation of the context data is a clear example of the easy handling:

```
procedure TFileServer.PrepareMustacheContextData( const pmDataFiles: TRawUtf8DynArray; out pmoContextData: TDocVariantData);
var
  context: TDocVariantData;
  contextName: RawUtf8;
begin
  pmoContextData.Init;
   var assetsFolder: TFileName := MakePath([Executable.ProgramFilePath, ASSETS_FOLDER], True);
   var contextFiles: TFileNameDynArray := FileNames(assetsFolder, CONTEXTFILE_SEARCHMASK, [ffoExcludesDir]);
   for var i: Integer := 0 toHigh(contextFiles) do
   begin
     if FindPropName(pmDataFiles, StringToUtf8(contextFiles[i])) < 0 then Continue; //->

    context.InitJsonFromFile(assetsFolder + contextFiles[i], JSON_FAST_FLOAT);
    contextName := StringToUtf8(GetFileNameWithoutExt(contextFiles[i]));
    pmoContextData.AddValue(contextName, Variant(context));
    context.Clear;
   end ;
end ;
```
In the end, all JSON data is loaded into a DocVariant and passed to the Mustache renderer as a package. WOW.

## The Mustache Editor: short and sweet

- Edge Browser is used for the display. With the function key "F5" the display is refreshed. With "F6" the project data are saved. If a project name has been assigned, it is saved immediately.
- Images, CSS, JavaScript, JSON or ZIP files are loaded only from the assets directory. The folder structure/directory depth in a ZIP file is not limited. Example of a path is: Bootstrap.zip/bootstrap.min.css
- Partials for Mustache Renderer can only be loaded from the Partials directory. Partials correspond to Pascal include files.
- The Bootstrap files "bootstrap.min.css" and "bootstrap.bundle.min.js" are always added.
- External files are loaded in order of appearance. Possible to specify the order of Bootstrap files by entering "*.css" or "*.js" in the order.
- JavaScript files are loaded with option "defer". This behavior can be influenced. By prepending a "!" directly to the file name, it is loaded immediately. Prefixing a "?" will load with the "async" option.
- The root for loaded JSON data is the filename. The JSON object from the Person.json file is addressed with "Person.FirstName".
- The auto-complete for Mustache Editor covers common HTML tags. The definitions are stored in the file HtmlAutoComplete.dci.
- The browser window can be undocked and placed as a stand-alone view.
- Everything else is left to the play instinct of the user...

# Summary

Today's article took a first look at a central building block, the phalanx of HTTP servers, the mORMot library. The enormous performance of these server classes are the foundation for the power of the subsequent RestServer. As a tool, the Mustache Template Engine, with an easy-to-learn language, opens up the possibility of connecting a template and JSON data for processing. It is based on the `mormot.rest.mvc unit`, which enables the creation of web apps based on the model-view-controller pattern. 

mORMot is well documented. The help includes more than 2500 pages. The first 650 or so pages contain a general part that is well worth reading, the rest is

API documentation. mORMot does not have to be installed in the IDE ! It is sufficient to paste the appropriate library paths. Many examples and a friendly forum are available. If there is more interest in mORMot, I can also briefly introduce other parts in a similar way.

See you soon...
Thomas 