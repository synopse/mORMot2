unit server;

{$I mormot.defines.inc}

interface

uses
  System.SysUtils,
  mormot.core.base,
  mormot.core.log,
  mormot.rest.http.server,
  mormot.rest.server,
  mormot.rest.memserver,
  mormot.soa.core,
  api.interfaces,
  api.impl;

type
  TJSONWriterSampleServer = class
  private
    fHttpServer: TRestHttpServer;
    fRestServer: TRestServerFullMemory;
  public
    constructor Create(const aPort: RawUtf8);
    destructor Destroy; override;
  end;

implementation

{ TJSONWriterSampleServer }

constructor TJSONWriterSampleServer.Create(const aPort: RawUtf8);
begin
  inherited Create;

  // Create in-memory REST server
  fRestServer := TRestServerFullMemory.CreateWithOwnModel([]);

  // Register service implementation
  fRestServer.ServiceDefine(TJSONWriterSample, [IJSONWriterSample], sicShared);

  // Create HTTP server
  fHttpServer := TRestHttpServer.Create(
    aPort,
    [fRestServer],
    '+',
    useHttpSocket
  );

  fHttpServer.AccessControlAllowOrigin := '*'; // Allow CORS for testing
end;

destructor TJSONWriterSampleServer.Destroy;
begin
  fHttpServer.Free;
  fRestServer.Free;
  inherited;
end;

end.
