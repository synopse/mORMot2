# Method Based Services

This is a sample extracted from a [Delphi Praxis Forum page](https://www.delphipraxis.net/210980-mormot-einfuehrung-methodenbasierte-services-server-und-client.html).

With the example code you get:
- An HTTP server for HTTP/1.0/1.1 requests.
- Authentication of the user on the server and creation of a user-specific directory.
- Login with the client to the server and exchange content list and image documents.
- Connection of a progress indicator with the help of a mediator.
- Acceleration in saving and reading graphic formats JPEG, PNG, GIF, TIFF.
- And some little things more...

The interface is kept very simple and includes only a few functions. The source code is written in such a way that it invites you to try it out on your own.

## Source code

The Source code has been extracted from the original `.zip` attached with the article.

But a `TestImage.png` picture is expected to be available in the executable folder, for testing the PNG reading/writing.

# Translated Documentation

Just copy and pasted from Google Translator, with a quick manual review and formatting.

## Presentation

The third article in the mORMot introduction series deals for the first time with the area for which the library was created.

Arnaud Bouchez describes his work as a client-server ORM/SOA framework and writes: " The Synopse mORMot framework implements a Client-Server RESTful architecture, trying to follow some MVC, N-Tier, ORM, SOA best-practice patterns ". 

The implementation happens on the principles of these architecture principles . If the terms SOA and RESTful architecture do not mean much to you, you can read about them at the following links:

    Wikipedia: [Service-Oriented Architecture](https://en.wikipedia.org/wiki/Service-oriented_architecture) , mORMot Help: [Service-Oriented Architecture](https://synopse.info/files/html/Synopse%20mORMot%20Framework%20SAD%201.18.html#TITL_17)
    Wikipedia: [Representational State Transfer](https://en.wikipedia.org/wiki/Representational_state_transfer) , mORMot Help: [Representational state transfer](https://synopse.info/files/html/Synopse%20mORMot%20Framework%20SAD%201.18.html#TITL_9)

The sample application server and client is intended to present as much functionality as possible with just a few lines of source code. This is about concepts, not about a finished copy-paste solution. The contained references to the documentation link to the currently available mORMot1 help. mORMot2 is used for the example . Class and function names may differ slightly.

Attached is the source code and the executable program for server and client. Disclaimer: The source code is neither tested nor optimized. It should work with Delphi 10.2 or later. The use of the materials provided is at your own risk. And even worse today: Anyone who makes the example server accessible on the Internet despite all the warnings should write the following sentence 1000 times as a punishment: "I will never again use a program in an area for which it was not intended!". I personally supervise the implementation.

Based on the first two example applications, images are also managed here. The server stores the images in the user's own directory (each user has their own directory). With the example source code for the server and client you get:

- An HTTP server for HTTP/1.0/1.1 requests.
- Authentication of the user on the server and creation of a separate directory.
- Registration with the client on the server and exchange of content list and image documents.
- Connection of a progress display with the help of a mediator.
- Accelerated saving and reading of the graphic formats JPEG, PNG, GIF, TIFF.

## Basic prior knowledge

The following points are only touched upon briefly. Each individual would warrant a separate treatise. They are important for understanding the example application.

### 1) REST Server

The RestServer inherits REST functionality from the `TRestServer` class and its descendants. Any number of RestServers, each with their own model, can be operated in one program. However, each model must have a unique root name. In a shop application, for example, it makes sense to distribute the administration and shop areas with their respective functions to specialized RestServers. If the RestServer "Shop" is registered under the root name `Shop`, a service function `ArticleBuy` is called via the root `Shop/ArticleBuy` . A full URI might look like this:

```
https://www.domain.de/Shop/ArticleBuy?ArticleID=123&AccessToken=xyz
```

These descendants of the `TRestServer` class are currently available in mORMot:

- `TRestServerFullMemory` - The ideal gateway to the outside world. This lightweight class implements a fast in-memory engine with basic CRUD functions for the ORM. It is entirely sufficient to handle authentication and host services in a standalone manner. Functionality that is not available is also not a target for attack.
- `TRestServerDB` - This class is the most important of its kind and hosts a SQLite engine as the central database layer. Your possibilities were already shown in the first article of the series.

### 2) Connection protocol 

The RestServer can be addressed stand-alone, i.e. directly via its methods, or in a client-server model via the communication layer HTTP/1.0/1.1 via TCP/IP . The possibilities depend on the protocol used. For a client-server application is generally used, let's ignore special cases such as `WebSockets`, on the server the class `TRestHttpServer` and on the client the class `TRestHttpClientUri` or descendants. There are different server modes when instantiating the `TRestHttpServer` class to select:

- `useHttpApi###` - Uses the kernel-mode http.sys server available since Windows XP SP3.
- `useHttpAsync` - A socket based server in event-driven mode with a thread pool. Therefore, it scales very well over many connections, especially keep-alive connections.
- `useHttpSocket` - A socket based server with one thread per active connection using a thread pool. This type is better used behind a reverse proxy server in HTTP/1.0 mode.

The classes `TRestHttpClientSocket`, `TRestHttpClientWinINet` and `TRestHttpClientWinHttp` are available for the client on Windows. In the example we use `WinHttp` to implement a progress display with the help of a mediator.

### 3) Authentication

The authentication classes present in mORMot are : `TRestServerAuthenticationNone`, `TRestServerAuthenticationDefault`, `TRestServerAuthenticationHttpBasic` and `TRestServerAuthenticationSsp`i. As an alternative, the use of JWT (JSON Web Tokens) is possible. In the example, mORMot's own proprietary solution is used. It is implemented in the `TRestServerAuthenticationDefault` class. In the sample server, logging is enabled. The ping/pong between client and server to establish a session can be tracked in the printout. The procedure is a good compromise between security and speed.

Even if authentication is enabled, individual methods can be exempted by calling the RestServer function `ServiceMethodByPassAuthentication('MethodName')`. One of the default service method, freely accessible functions is `Timestamp`. To test on the sample server, enter the following call in the browser:

```
http:// localhost :8080/Files/Timestamp
```
After execution, a multi-digit number, the timestamp of the server, is displayed in the browser window.

## Accelerated saving and reading of images

The last item in the list has already been discussed in the previous article. It is only mentioned again to complement it. 

## Your own RestServer
In mORMot, there are two types of services:

- Method-based Services, mORMot help: [Client-Server services via methods](https://synopse.info/files/html/Synopse%20mORMot%20Framework%20SAD%201.18.html#TITL_49)
- Interface-based Services, mORMot help: [Client-Server services via interfaces](https://synopse.info/files/html/Synopse%20mORMot%20Framework%20SAD%201.18.html#TITL_63)

In order not to let the whole thing get out of hand, I omit further possibilities. They shouldn't play a role here.

The interface of the Rest server is kept very simple and includes only a few functions:

```
TFileRestServer = class (TRestServerFullMemory)
strict private
  FDataFolder: TFileName;
protected
   const
    DEFAULT_FILE_EXT = ' ._ ';
protected
   function GetSessionUserDirName(pmCtxt: TRestServerUriContext; out pmoDirName: TFileName): Boolean;
   property DataFolder: TFileName
     read FDataFolder;
public
   constructor Create( constpmcRootName: RawUtf8; const pmcDataFolder: TFileName); introduce ;
   function InitAuthForAllCustomers( const pmcFileName: TFileName): Boolean;
published
   procedure LoadFile(pmCtxt: TRestServerUriContext);
   procedure SaveFile(pmCtxt: TRestServerUriContext);
   procedure GetAllFileNames(pmCtxt: TRestServerUriContext);
end ;
```

As the topic already suggests, we implement method-based services. If you look at the definition, you will notice the three functions `LoadFile`, `SaveFile` and `GetAllFileNames` in the published section. The prototype is defined as follows:

```
type
  TOnRestServerCallBack = procedure (pmCtxt: TRestServerUriContext) of Object;
```

Any method defined in this way in the `published` section is automatically registered as a service function. The name of the method is the service name. Routing is always via RootName/ServiceName . In the example discussed, RootName is "Files". So the route for the GetAllFileNames function looks like this:
```
GET Files/GetAllFileNames
```

Methods can also be registered directly with the RestServer function `ServiceMethodRegister('MethodName', MyRestServerCallBackEvent)`. Methods of this type in the published section are automatically registered. It's not complicated at all.

## Implementing a service

Let's look at a concrete example. The query of the `GetAllFileNames` service returns the content list of the user's own data directory. The result is returned in JSON format.

```
procedure TFileRestServer.GetAllFileNames(pmCtxt: TRestServerUriContext);
var
  dirName: TFileName;
  dirFiles: TFileNameDynArray;
begin
   if (pmCtxt.Method = mGET)
     and GetSessionUserDirName(pmCtxt, dirName) then
   begin
    dirFiles := FileNames([DataFolder, dirName], FILES_ALL, [ffoSortByName, ffoExcludesDir]);
     for var i: Integer := Low(dirFiles) to High(dirFiles) do
     begin
       if ExtractFileExt(dirFiles[i]) = DEFAULT_FILE_EXT then
        dirFiles[i] := GetFileNameWithoutExt(dirFiles[i]);
     end ;

    pmCtxt.Returns(DynArraySaveJson(dirFiles, TypeInfo(TFileNameDynArray)), HTTP_SUCCESS);
   end
   else
    pmCtxt.Error(StringToUtf8(SErrHttpForbidden), HTTP_FORBIDDEN);
end ;
```

It is very important to observe the principle: Never trust data that comes from outside! Our function does not require any input parameters. The authorization of access to the function from the outside is already authorized via the authentication. The `TRestServerUriContext` class has a large number of functions, such as convenient handling of input and output values or session data. The `DynArraySaveJson()` function is used to serialize the data into JSON. With the mORMot tool box, nothing is left to be desired and it couldn't be easier.

## User administration for the server

Authentication is enabled when the RestServer is instantiated. The administration is built with the ORM classes `TAuthGroup` and `TAuthUser` or their descendants. It is important to note that defaults are automatically created in both tables if this behavior is not explicitly suppressed. The created records for the group are useful, but all users should be created themselves. The user data in the example is loaded from the `"Customer.config"` file in the program directory. The entire implementation takes place in a few lines of source code.

## The full server

A production server should be based on the `TSynDaemon` class to be created, which features installation as Windows service or Linux daemon. For the sake of clarity, I have chosen a simpler variant for the example. The program consists of two components, the RestServer described above and the HTTP transmission protocol. More effort is not necessary to start an HTTP server:


```
function TTestServerMain.RunServer( const pmcPort: RawUtf8): Boolean;
begin
  Result := False;
   if (FHttpServer = Nil )
     and FRestServer.InitAuthForAllCustomers(FCustomerConfigFile) then
   begin
    FHttpServer := TRestHttpServer.Create(pmcPort, [FRestServer], ' + '  {DomainName} , useHttpSocket {or useHttpAsync} );
    FHttpServer.AccessControlAllowOrigin := ' *';
    result := true;
   end ;
end ;
```
A wildcard character is used for the URL prefix, `"+"` binds to all domain names for the given port if `http.sys` is used - but just ignored for `useHttpSocket` or `useHttpAsync`.

## The client application

The easiest way is to build a counterpart to the `TFileRestServer` class for the client. The complete REST logic is encapsulated in it. The `TRestHttpClientWinHttp` class is used as the ancestor. It provides access to the `TWinHttpApi` class, which allows easy connection of our progress bar. Each service function of the server gets its counterpart with the functionality required for the client:

```
TFileHttpClient = class (TRestHttpClientWinHttp)
public
   constructor Create( const pmcServerURI: RawUtf8 = SERVER_URI; const pmcServerPort: RawUtf8 = SERVER_PORT); introduce ;
   function LoadImage(pmImage: TImage; const pmcImageName: String ): Boolean;
   function SaveImage(pmImage: TImage; const pmcImageName: String ): Boolean;
   procedure GetAllFileNames(pmFileNames: TStrings);
end ;
```

The service methods on the server are called via the `CallBack###()` functions. The RESTful root schema `ModelRoot/MethodName` is used. If a call requires parameters, the query string is formed using the `UrlEncode()` function by passing name/value pairs. If successful, `200/HTTP_SUCCESS` is returned. Many HTTP status code constants are defined in the `mormot.core.os` unit . The concrete implementation looks like this:

```
function TFileHttpClient.SaveImage(pmImage: TImage; const pmcImageName: String ): Boolean;
var
  return: RawUtf8;
begin
  Result := False;
   if pmImage = Nil then Exit; //=>
   if pmcImageName = ' '  then Exit; //=>

   var stream: TRawByteStringStream := TRawByteStringStream.Create;
   try
    pmImage.Picture.SaveToStream(stream);
     if stream.Position > 0 then
     begin
       var methodName: RawUtf8 := TFileServiceFunction. Name .SaveFile
        + UrlEncode([TFileServiceFunction.Param.SaveFile_ImageName, StringToUtf8(pmcImageName)]);

      Result := (CallBackPut(methodName, stream.DataString, return) = HTTP_SUCCESS);
     end ;
   finally
    stream.Free;
   end ;
end ;
```
If you haven't noticed, I avoid string identifiers whenever possible. I prefer to define constants for this. Makes more work at the beginning, but quickly saves it again.

## Progress Bar with Mediator

Mediators are an elegant way to encapsulate functionality and achieve easy and safe use. Even if the following example is under-complex , the profit is recognizable. The function could also hide a semi-modal dialog with a progress bar.

```
type
  TWinHttpProgressGuiHelper = class (TCustomWinHttpProgressGuiHelper)
   private
    FProgressBar: TProgressBar;
    FProgressStepWidth: Integer;
     procedure InternalProgress(pmCurrentSize, pmContentLength: Cardinal);
   protected
     function DoUploadProgress(pmSender: TWinHttpApi; pmCurrentSize, pmContentLength: Cardinal): Boolean; override ;
     procedure DoDownloadProgress(pmSender: TWinHttpApi; pmCurrentSize, pmContentLength: Cardinal); override ;
   public
     constructor Create(pmProgressBar: TProgressBar; pmStepWidth: Integer = 5); introduce ;
     procedure Prepare(pmRestClient: TRestHttpClientWinHttp; pmProgressMode: TWinHttpProgressMode); override ;
     procedure Done; override ;
   end ;
  
function TWinHttpProgressGuiHelper.DoUploadProgress(pmSender: TWinHttpApi; pmCurrentSize, pmContentLength: Cardinal): Boolean;
begin
  Result := True;
   if pmCurrentSize = 0 then
    FProgressBar.Position := 0
   else if pmCurrentSize >= pmContentLength then
    Done
   else
    InternalProgress(pmCurrentSize, pmContentLength);
end ;
```

Implementing the binding is a simple call to the Prepare function :

```
FProgressHelper.Prepare(FRestClient, whpUpload);
try
  FRestClient.SaveImage(Image, edtImageName.Text);
finally
  FProgressHelper.Done;
end ;
```

In order to see the progress bar in action, an image with a size of at least 10MB should be selected. If you look at the server's log file, you'll see that a function takes just a fraction of a millisecond to execute. When the server runs locally, there is hardly any latency. At this speed, any progress bar has a hard time. You have to give the ad a real chance.

# Summary

Today's article was particularly challenging due to the need to radically shorten it. With this action, there is always a risk of losing the common thread of understanding. I hope I didn't exaggerate and the explanations remained comprehensible.

mORMot is well documented. The help includes more than 2500 pages. The first 650 pages contain a very readable general part, the rest is API documentation. mORMot does not have to be installed in the IDE ! It is sufficient to paste the appropriate library paths. Many examples and a friendly forum are available. If there is more interest in mORMot, I can also briefly introduce other parts in a similar way.

See you soon...
Thomas
