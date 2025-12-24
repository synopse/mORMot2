unit server;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.unicode,
  mormot.core.text,
  mormot.net.server,
  mormot.net.http,
  api.interfaces,
  api.impl;

type
  TSslDemoServer = class
  private
    fHttpServer: THttpServer;
    fPort: RawUtf8;
    fApi: TMyApi;
    function HandleRequest(Ctxt: THttpServerRequestAbstract): cardinal;
  public
    constructor Create(const aPort: RawUtf8);
    destructor Destroy; override;
    procedure Start(const aCertificateFile, aPrivateKeyFile: TFileName;
      const aPrivateKeyPassword: RawUtf8 = '');
    procedure StartSelfSigned;
  end;

implementation

{ TSslDemoServer }

constructor TSslDemoServer.Create(const aPort: RawUtf8);
begin
  inherited Create;
  fPort := aPort;
end;

destructor TSslDemoServer.Destroy;
begin
  fHttpServer.Free;
  fApi.Free;
  inherited;
end;

function TSslDemoServer.HandleRequest(Ctxt: THttpServerRequestAbstract): cardinal;
var
  url: RawUtf8;
begin
  url := Ctxt.Url;

  // Route: GET /
  if (Ctxt.Method = 'GET') and (url = '/') then
  begin
    Ctxt.OutContent := fApi.Index;
    Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
    result := HTTP_SUCCESS;
    exit;
  end;

  // Route: GET /people
  if (Ctxt.Method = 'GET') and (url = '/people') then
  begin
    Ctxt.OutContent := fApi.GetPeople;
    Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
    result := HTTP_SUCCESS;
    exit;
  end;

  // Default: Not Found
  Ctxt.OutContent := '{"error":"Not Found"}';
  Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
  result := HTTP_NOTFOUND;
end;

procedure TSslDemoServer.Start(const aCertificateFile, aPrivateKeyFile: TFileName;
  const aPrivateKeyPassword: RawUtf8);
begin
  // Create API implementation
  fApi := TMyApi.Create;

  // Create HTTP server with TLS support
  fHttpServer := THttpServer.Create(
    fPort,
    nil,  // Start method
    nil,  // ThreadPool (nil for default)
    '',   // ServerName
    32,   // ThreadPoolCount
    30000, // KeepAliveTimeOut
    [hsoEnableTls] // Enable TLS/HTTPS support
  );

  // Register API request handler
  fHttpServer.OnRequest := HandleRequest;

  // Wait for server to start and configure TLS certificates
  fHttpServer.WaitStarted(
    10,  // Timeout in seconds
    aCertificateFile,
    aPrivateKeyFile,
    aPrivateKeyPassword
  );

  TSynLog.Add.Log(sllInfo, 'HTTPS Server started on port % with TLS', [fPort]);
end;

procedure TSslDemoServer.StartSelfSigned;
begin
  // Create API implementation
  fApi := TMyApi.Create;

  // Create HTTP server with TLS support
  fHttpServer := THttpServer.Create(
    fPort,
    nil,  // Start method
    nil,  // ThreadPool (nil for default)
    '',   // ServerName
    32,   // ThreadPoolCount
    30000, // KeepAliveTimeOut
    [hsoEnableTls] // Enable TLS/HTTPS support
  );

  // Register API request handler
  fHttpServer.OnRequest := HandleRequest;

  // Start with self-signed certificate (for testing only)
  fHttpServer.WaitStartedHttps(10, true);

  TSynLog.Add.Log(sllInfo, 'HTTPS Server started on port % with self-signed certificate', [fPort]);
end;

end.
