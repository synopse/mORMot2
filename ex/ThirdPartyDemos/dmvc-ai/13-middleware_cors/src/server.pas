unit server;

{$I mormot.defines.inc}

interface

uses
  sysutils,
  TypInfo,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.log,
  mormot.core.rtti,
  mormot.core.unicode,
  mormot.net.http,
  mormot.orm.core,
  mormot.rest.sqlite3,
  mormot.rest.http.server,
  mormot.rest.core,
  mormot.soa.core,
  entities,
  api.interfaces,
  api.impl;

type
  /// CORS Configuration Mode
  TCorsMode = (
    cmAllowAll,           // Allow all origins (*)
    cmSpecificOrigins,    // Allow specific origins (localhost:9090, anotherserver.com)
    cmRestrictive         // Disallow certain origins
  );

  /// CORS Middleware Sample Server
  // - Demonstrates CORS configuration in mORMot2
  // - Equivalent to DMVC TMVCCORSMiddleware
  TCorsSampleServer = class
  protected
    fRestServer: TRestServerDB;
    fHttpServer: TRestHttpServer;
    fCustomerApi: ICustomerApi;
    fCorsMode: TCorsMode;
    procedure ConfigureCors(aMode: TCorsMode);
    function OnBeforeBody(var aUrl, aMethod, aInHeaders, aInContentType,
      aRemoteIP: RawUtf8; aContentLength: integer;
      aFlags: THttpServerRequestFlags): cardinal;
  public
    constructor Create(const aPort: RawUtf8 = '8080';
      const aDbFileName: TFileName = 'cors_sample.db';
      aCorsMode: TCorsMode = cmAllowAll); reintroduce;
    destructor Destroy; override;
    /// start the HTTP server
    procedure Start;
    /// stop the HTTP server
    procedure Stop;
  end;


implementation


{ TCorsSampleServer }

constructor TCorsSampleServer.Create(const aPort: RawUtf8;
  const aDbFileName: TFileName; aCorsMode: TCorsMode);
begin
  TSynLog.Add.Log(sllInfo, 'Creating CORS Sample Server...');
  fCorsMode := aCorsMode;

  // Create SQLite3 database with ORM entities
  fRestServer := TRestServerDB.CreateSqlite3([TOrmCustomer], aDbFileName);
  TSynLog.Add.Log(sllInfo, 'Database initialized: %', [aDbFileName]);

  // Register the API service interface
  // Note: ServiceDefine expects a class, not an instance - framework creates instances as needed
  fRestServer.ServiceDefine(TCustomerApi, [ICustomerApi], sicShared);
  TSynLog.Add.Log(sllInfo, 'Customer API registered');

  // Create HTTP server
  fHttpServer := TRestHttpServer.Create(aPort, [fRestServer],
    '+', HTTP_DEFAULT_MODE, {threaded=}32);

  // Configure CORS based on mode
  ConfigureCors(aCorsMode);

  TSynLog.Add.Log(sllInfo, 'HTTP Server created on port %', [aPort]);
  TSynLog.Add.Log(sllInfo, 'CORS Sample Server created successfully');
end;

destructor TCorsSampleServer.Destroy;
begin
  TSynLog.Add.Log(sllInfo, 'Destroying CORS Sample Server...');
  Stop;
  fHttpServer.Free;
  fCustomerApi := nil;
  fRestServer.Free;
  TSynLog.Add.Log(sllInfo, 'CORS Sample Server destroyed');
  inherited Destroy;
end;

procedure TCorsSampleServer.ConfigureCors(aMode: TCorsMode);
begin
  case aMode of
    cmAllowAll:
      begin
        // DMVC equivalent: FMVC.AddMiddleware(TMVCCORSMiddleware.Create);
        // Allow all origins (*)
        fHttpServer.AccessControlAllowOrigin := '*';
        TSynLog.Add.Log(sllInfo, 'CORS: Allow all origins (*)');
      end;

    cmSpecificOrigins:
      begin
        // DMVC equivalent: TMVCCORSMiddleware.Create('https://anotherserver.com,http://localhost:9090')
        // Allow specific origins (comma-separated)
        fHttpServer.AccessControlAllowOrigin := 'https://anotherserver.com,http://localhost:9090';
        TSynLog.Add.Log(sllInfo, 'CORS: Allow specific origins');
      end;

    cmRestrictive:
      begin
        // DMVC equivalent: TMVCCORSMiddleware.Create('https://anotherserver.com')
        // More restrictive - only one origin
        fHttpServer.AccessControlAllowOrigin := 'https://anotherserver.com';
        TSynLog.Add.Log(sllInfo, 'CORS: Restrictive mode (single origin)');
      end;
  end;

  // Additional CORS headers (equivalent to DMVC middleware defaults)
  // Note: mORMot2 TRestHttpServer automatically handles CORS headers via ComputeAccessControlHeader
  // The framework automatically adds: Access-Control-Allow-Methods, Access-Control-Allow-Headers,
  // Access-Control-Expose-Headers, and Access-Control-Max-Age based on AccessControlAllowOrigin
  //
  // For custom CORS header control, you can use OnBeforeBody callback to add specific headers
  // Example code (commented out - mORMot2 handles these automatically):
  //   fHttpServer.AccessControlAllowMethods := 'GET,POST,PUT,DELETE,OPTIONS';
  //   fHttpServer.AccessControlAllowHeaders := 'Content-Type,Authorization,X-Requested-With';
  //   fHttpServer.AccessControlExposeHeaders := 'Content-Type,Content-Length';
  //   fHttpServer.AccessControlMaxAge := 86400; // 24 hours

  TSynLog.Add.Log(sllInfo, 'CORS headers configured:');
  TSynLog.Add.Log(sllInfo, '  Allow-Origin: %', [fHttpServer.AccessControlAllowOrigin]);
  TSynLog.Add.Log(sllInfo, '  Allow-Credential: %', [fHttpServer.AccessControlAllowCredential]);
  TSynLog.Add.Log(sllInfo, '  (mORMot2 automatically handles Allow-Methods, Allow-Headers, etc.)');
end;

function TCorsSampleServer.OnBeforeBody(var aUrl, aMethod, aInHeaders,
  aInContentType, aRemoteIP: RawUtf8; aContentLength: integer;
  aFlags: THttpServerRequestFlags): cardinal;
begin
  // Custom CORS preflight handling (for advanced scenarios)
  // mORMot2 automatically handles OPTIONS requests via AccessControlAllowOrigin
  // This callback is here for reference and advanced customization

  TSynLog.Add.Log(sllTrace, 'Request: % % from %', [aMethod, aUrl, aRemoteIP]);

  // Return 0 to continue normal processing
  Result := 0;

  // For custom preflight handling, you could do:
  // if aMethod = 'OPTIONS' then
  // begin
  //   Result := 200; // HTTP_SUCCESS
  //   // Custom headers would be added here
  // end;
end;

procedure TCorsSampleServer.Start;
begin
  TSynLog.Add.Log(sllInfo, 'CORS Sample Server started');
  TSynLog.Add.Log(sllInfo, 'Available endpoints:');
  TSynLog.Add.Log(sllInfo, '  POST /CustomerApi/CreateCustomer');
  TSynLog.Add.Log(sllInfo, '  POST /CustomerApi/GetCustomer');
  TSynLog.Add.Log(sllInfo, '  POST /CustomerApi/GetAllCustomers');
  TSynLog.Add.Log(sllInfo, '');
  TSynLog.Add.Log(sllInfo, 'CORS Configuration Mode: %', [GetEnumName(TypeInfo(TCorsMode), Ord(fCorsMode))]);
end;

procedure TCorsSampleServer.Stop;
begin
  TSynLog.Add.Log(sllInfo, 'CORS Sample Server stopping...');
end;


end.
