unit server;

interface

uses
  mormot.orm.core,
  mormot.rest.memserver,
  mormot.rest.http.server,
  mormot.rest.server,
  mormot.net.server;

type
  TCompressionServer = class
  private
    fHttpServer: TRestHttpServer;
    fRestServer: TRestServerFullMemory;
  public
    constructor Create(const aPort: string);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  end;

implementation

uses
  mormot.core.base,
  mormot.core.log,
  mormot.core.os,
  mormot.rest.core,
  api.interfaces,
  api.impl;

{ TCompressionServer }

constructor TCompressionServer.Create(const aPort: string);
begin
  inherited Create;

  // Create REST server
  fRestServer := TRestServerFullMemory.CreateWithOwnModel([]);

  // Register the API service
  fRestServer.ServiceDefine(TCustomersApi, [ICustomersApi], sicShared);

  // Create HTTP server with compression enabled
  fHttpServer := TRestHttpServer.Create(
    aPort,
    [fRestServer],
    '+',
    useHttpSocket,
    32,
    secNone
  );

  // Enable compression on HTTP server
  // mORMot2's HTTP server has built-in compression support
  // It automatically compresses responses > 1024 bytes when client accepts gzip/deflate
  fHttpServer.HttpServer.Compress := [hcSynShaAes, hcSynLZ, hcDeflate, hcGZip];

  // Set compression threshold (default is 1024 bytes)
  fHttpServer.HttpServer.CompressMinSize := 1024;
end;

destructor TCompressionServer.Destroy;
begin
  Stop;
  fHttpServer.Free;
  fRestServer.Free;
  inherited;
end;

procedure TCompressionServer.Start;
begin
  writeln('');
  writeln('** mORMot2 Response Compression Sample **');
  writeln('');
  writeln('Server started on http://localhost:', fHttpServer.Port);
  writeln('');
  writeln('To test the compression middleware call the following URLs:');
  writeln('  http://localhost:', fHttpServer.Port, '/root/CustomersApi.GetCustomers');
  writeln('    (compression enabled - response > 1024 bytes)');
  writeln('  http://localhost:', fHttpServer.Port, '/root/CustomersApi.GetTallCustomers');
  writeln('    (compression disabled - response < 1024 bytes)');
  writeln('');
  writeln('Compression modes supported:');
  writeln('  - gzip (recommended)');
  writeln('  - deflate');
  writeln('  - synlz (mORMot proprietary - very fast)');
  writeln('');
  writeln('Press [Ctrl+C] to quit');
  writeln('');
end;

procedure TCompressionServer.Stop;
begin
  // HTTP server automatically stops in destructor
end;

end.
