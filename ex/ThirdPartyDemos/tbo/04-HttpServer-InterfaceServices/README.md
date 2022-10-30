# Interface Based Services

This is a sample extracted from a [Delphi Praxis Forum page](https://www.delphipraxis.net/211015-mormot-einfuehrung-interface-based-services-server-und-client.html).

With the example code you get:
- An HTTP server for HTTP/1.0/1.1 requests.
- Authentication of the user on the server and creation of a user-specific directory.
- Login with the client to the server and exchange content list and text documents.
- A simple Markdown editor with preview of HTML output in the browser window.
- And some little things more...

The interface is kept very simple and includes only a few functions. The source code is written in such a way that it invites you to try it out on your own.

## Source code

The Source code has been extracted from the original `.zip` attached with the article.

# Translated Documentation

Just copy and pasted from Google Translator, with a quick manual review and formatting.

## Presentation

The fourth article in the mORMot presentation series also deals with the area for which the library was created. It assumes the basic knowledge acquired in the third article, *Introduction to Method-Based Services - Server and Client*. Anyone who is not yet familiar with the introduction to the topic should do so before reading on. The contained references to the documentation link to the currently available mORMot1 help. mORMot2 is used for the example. Class and function names may differ slightly.

Attached is the source code and the executable program for server and client. Disclaimer: The source code is neither tested nor optimized. It should work with Delphi 10.2 or later. The use of the materials provided is at your own risk. And even worse today: Anyone who makes the example server accessible on the Internet despite all the warnings should write the following sentence 1000 times as a punishment: "I will never again use a program in an area for which it was not intended!". I personally supervise the implementation.

In a modification of the first three example applications, text documents are managed this time. The server saves the documents in the user's own directory (each user has his own directory). With the example source code for the server and client you get:

- An HTTP server for HTTP/1.0/1.1 requests.
- Authentication of the user on the server and creation of a separate directory.
- Registration with the client on the server and exchange of content list and text documents.
- A simple markdown editor with preview of the HTML output in the browser window.

## Basic prior knowledge

*Interface-based services* are a broad area in mORMot and the way of implementation is a real high mass. The following explanations are only a taster of the respective topic and greatly simplified. Each one would warrant a separate essay. They are important for understanding the example application. If you want to know more, you should take a look at the help and start with [Client-Server services via interfaces](https://synopse.info/files/html/Synopse%20mORMot%20Framework%20SAD%201.18.html#TITL_63).

In order to communicate between server and client, a contract is required. With interface-based services, this contract is created by defining an interface. And as long as the definition of the interface is the same on the server and the clients, smooth communication is guaranteed. All measures that are necessary for this interaction of both sides (server and client) are automatically taken care of by mORMot. The developer only has to take care of the business logic in the respective service object. Can or may one believe that? You have to read on for that...

### 1) Naming convention

How do I name my service and the objects that work with it? Spirits disagree on this and some recommendations based on academic considerations are contrary. I've gotten into the habit of naming the interface after its purpose (subject) without any additions - one can argue about that. In the example, that would be `IDocument`. The service objects that implement this interface in the server and client are provided with the suffix `service`. The object is thus named `TDocumentService`.

### 2) Define Interfaces

Each interface needs a GUID to access via to ensure RTTI. Using `IInvokable` is a good choice. Interfaces may be inherited. Interface methods can be functions or procedures. The parameters can be passed by value or by reference. The permitted parameters include simple types such as string, integer or enumerations, but also complex types such as objects, records and dynamic arrays. An overview of permitted types can be found [in the help](https://synopse.info/files/html/Synopse%20mORMot%20Framework%20SAD%201.18.html#TITL_154).

First of all, you have no influence on the coding of the data transmitted between server and client. All is done automatically, by convention. `RawByteString` or complex types like records are transmitted as Base64 encoded JSON. When transferring larger files, it is advantageous to avoid the Base64 encoding overhead and make better use of method-based services . Both methods, method and interface-based services, can be used together. Small insert: With the record `TServiceCustomAnswer` is defined as a result, `interface` functions also offer the possibility of using their own data format for the return.

### 3) Lifetime of a service object

Let's replay the process from the client to the server when executing a service call. The desired interface is provided via the `RestHttpClient` class. This allows one of its methods to be called. mORMot sends this request to the server. A service object is registered in the server's RestServer for each interface, which is now called or instantiated. An option of the type TServiceInstanceImplementationinfluenced. This is specified when registering the interface. An interface must be operated on server and client with the same settings. There are the following options:

- `sicSingle` - An object instance is created for each invocation. This is the most complex, but also the most secure way of operating a service. If you are still unfamiliar with which option is suitable for a specific case, keep this setting, which is also the standard.
- `sicShared` - An object instance is used for all incoming calls. The implementation of the function on the server must be thread-safe. These are, for example, all calls via the internal ORM.
- `sicClientDriven` - An object instance is created and deallocated synchronously with the lifetime of the corresponding interface on the client side. Put simply: if the interface on the client is cleared, the service object on the server is also hit.
- `sicPerSession sicPerUser sicPerGroup sicPerThread` - As the name suggests, the instance is associated with the running session, user, user group or thread.

Stopover : mORMot is so much faster than the onboard tools that the default settings will suffice and tuning will not be an issue for a long time. Once you get the hang of it, what seems hard and difficult now becomes simple and easy.

### 4) Execution options of an interface

When registering an `interface` on the server, options for execution `TInterfaceMethodOptions` can be specified. By default, a method is invoked on the thread that received it. That is, the methods are re-entrant and execution must be thread-safe. Exception: `sicSingle` and `sicPerThread`. This behavior can be controlled via the `ServiceFactoryServerInstanc`e, discussed in the next section, of each interface.

For example, if a method should be executed in the main thread: 
```(RestServer.ServiceContainer.Info(IDocument) as TServiceFactoryServer).SetOptions([' GetAllNames '], [optExecInMainThread]);
``` 
More on this in the help under [Server-side execution options - threading](https://synopse.info/files/html/Synopse%20mORMot%20Framework%20SAD%201.18.html#TITL_72).

### 5) Security when accessing the interface 

Authentication is required to access an interface. Except by setting the `TServiceFactoryServer` property `ByPassAuthentication` to `true`, this requirement is deactivated. Each registered interface is represented by an instance of this class. It's about `RestServer.ServiceContainer` accessible. Since the authorization of the interface methods also runs via the `ServiceFactoryServer` instance, here is an example: 
```
(RestServer.ServiceContainer.Info(IDocument) as TServiceFactoryServer).ByPassAuthentication := True; 
```

Access to the interfaces can be authorized. By default it is unlimited. Security policies can be fine-tuned for each interface and each method of an interface. The functions `Allow`, `AllowAll`, `AllowAllByID`, `AllowAllByName`, `Deny`, `DenyAll`, `DenyAllByID` and `DenyAllByName` are available for this purpose. These methods are implemented as fluent interfaces. 
Example: 
```(RestServer.ServiceContainer.Info(IDocument) as TServiceFactoryServer).Allow(['Load ', ' Save ']).Deny([' GetAllNames ']);
```
Or, following my preference, replace the string identifiers with constants and write: 
```
.Allow([TDocumentService.IMN.Load, TDocumentService.IMN.Save]).Deny([TDocumentService.IMN.GetAllNames]);
```
The abbreviation IMN stands for Interface Method Name.

## Your own RestServer

The interface of the server's service object is kept very simple and only includes a few functions:
```
TDocumentService = class (TInjectableObjectRest, IDocument)
public
   function GetSessionUserDirName( out pmoDirName: TFileName): Boolean;
   //*** IDocument ***
   function Load( const pmcName: TFileName; out pmoData: RawBlob): Boolean;
   function Save( const pmcName: TFileName; const pmcData: RawBlob): Boolean;
   procedure GetAllNames( outpmoFileNames: TFileNameDynArray);
end ;
```
As the topic suggests, we implement interface-based services. The service object defines the three functions `Load`, `Save` and `GetAllNames` of the `IDocument` interface.

Let's take a look at the implementation using the `GetAllNames` service method as an example. The method returns the content list of the user's own data directory. The result is returned as a dynamic array of `TFileName`.
```
procedure TDocumentService.GetAllNames( out pmoFileNames: TFileNameDynArray);
var
  dirName: TFileName;
begin
   if not GetSessionUserDirName(dirName) then Exit; //=>

  pmoFileNames := FileNames(dirName, FILES_ALL, [ffoSortByName, ffoExcludesDir]);
   for var i: Integer := Low(pmoFileNames) to High(pmoFileNames) do
   begin
     if ExtractFileExt(pmoFileNames[i]) = TFileRestServer.DEFAULT_FILE_EXT then
      pmoFileNames[i] := GetFileNameWithoutExt(pmoFileNames[i]);
   end ;
end ;
```

It is very important to observe the principle: Never trust data that comes from outside! Our function does not require any input parameters. All incoming data must also be checked with this type of service implementation. mORMot serializes the `TFileNameDynArray` as JSON and sends it to the client. This process runs automatically and requires no intervention. There is no noticeable difference in the implementation of the function from the usual handling.

## User management for the server

With the framework, only the authentication has to be programmed in the RestServer. Management is done with descendants of the ORM classes `TAuthGroup` and `TAuthUseror`. It is important to note that defaults are automatically created in both tables if this behavior is not explicitly suppressed. The created records for the group are useful, but all users should be created themselves.

```
function TFileRestServer.InitAuthForAllCustomers( const pmcFileName: TFileName): Boolean;
var
  json: RawUtf8;
  customers: TCustomerItemArray;
begin
  Result := False;
   if not FileExists(pmcFileName) then Exit; //=>

  json := AnyTextFileToRawUtf8(pmcFileName, {AssumeUtf8IfNoBom=}  True);
   if IsValidJson(json)
     and (DynArrayLoadJson(customers, Pointer(json), TypeInfo(TCustomerItemArray)) <> Nil ) then
   begin
     var authUser: TFileAuthUser := TFileAuthUser.Create;
     try
       for var i: Integer := 0 to High(customers) do
       begin
         if (customers[i].LoginUserName <> ' ')
           and (customers[i].LoginPassword <> ' ') then
         begin
          authUser.CustomerNum := customers[i].CustomerNum;
          ...
          authUser.GroupRights := TAuthGroup(3); // AuthGroup: User
          Server.Add(authUser, True);
         end ;
       end ;
     finally
      authUser.Free;
     end ;

    Result := (Server.TableRowCount(TFileAuthUser) > 0);
   end ;
end ;
```
The user data in the example is loaded from the `"Customer.config"` file in the program directory.

## The complete server

The program consists of two components, the RestServer described above and the HTTP transmission protocol. More effort is not necessary to start an HTTP server:

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
A wildcard character is used for the URL prefix , "+" binds to all domain names for the given port if `http.sys` is used.

## The client application

The easiest way is to build a counterpart to the `TDocumentService` class for the client. The complete REST logic is encapsulated in it. Each service function of the server gets its counterpart with the functionality required for the client. For the sake of clarity, I created the RestServer directly in the client's service object for the example:
```
TDocumentService = class (TObject)
strict private
  FClient: TRestHttpClient;
public
   constructor Create( const pmcServerURI: RawUtf8; const pmcServerPort: RawUtf8);
   destructor Destroy; override ;
   function InitializeServices: Boolean;
   function Load(pmEditor: TMemo; const pmcDocName: String ): Boolean;
   functionsSave(pmEditor: TMemo; const pmcDocName: String ): Boolean;
   procedure GetAllNames(pmFileNames: TStrings);
   property Client: TRestHttpClient
     read FClient;
end ;
```
The `Resolve()` function of the `RestServer` gives us the `IDocument` interface. In order to establish communication with the server and bring the interface to life there with the instantiation of the `TDocumentService` class, only one call of the `GetAllNames` method is necessary. It should be noted that objects created by the interface, for example in a `T##ObjArray`, must be released themselves.

```
procedure TDocumentService.GetAllNames(pmFileNames: TStrings);
var
  service: IDocument;
  fileNames: TFileNameDynArray;
begin
   if pmFileNames = Nil then Exit; //=>
   if not FClient.Resolve(IDocument, service) then Exit; //=>

  pmFileNames.BeginUpdate;
   try
    pmFileNames.Clear;
    service.GetAllNames(fileNames);
     for var i: Integer := 0 toHigh(fileNames) do
      pmFileNames.Add(fileNames[i]);
   finally
    pmFileNames.EndUpdate;
   end ;
end ;
```

I hope I didn't promise too much at the beginning and it was worth reading.

# Summary

Today's article was also a particular challenge, mainly due to the need to radically shorten it. With this action, there is always a risk of losing the common thread of understanding. I hope the explanations remain understandable.

mORMot is well documented. The help includes more than 2500 pages. The first 650 pages contain a very readable general part, the rest is API documentation. mORMot does not have to be in the IDEbe installed! It is sufficient to paste the appropriate library paths. Many examples and a friendly forum are available. If there is more interest in mORMot, I can also briefly introduce other parts in a similar way.

See you soon...
Thomas 