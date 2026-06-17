unit app.server;

{$I mormot.defines.inc}

interface

uses
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.rest.server,
  mormot.rest.memserver,
  mormot.soa.core,
  mormot.soa.server,
  mormot.rest.http.server,
  dom.infra,      // IEventPersistence
  api.mobile,     // IApiMobile
  api.mobile.impl; // TApiMobile

const
  /// the TCP port on which the mobile API is published over HTTP
  AUDITTRAIL_HTTP_PORT = '8080';

type
  /// the SOA composition root which publishes IApiMobile over HTTP
  // - this is the "edge" of a two-server topology: the edge owns no data (a
  //   void ORM model, so no ORM REST route can ever be reached) and exposes
  //   only the application API, while the injected persistence keeps its own
  //   private ORM server entirely in-process
  // - the same wiring is reused by the stand-alone daemon (AuditTrailServer.dpr,
  //   step 8) and by the remote regression test (tests.pas RemoteCall, step 7)
  TAuditTrailServer = class
  protected
    fPersistence: IEventPersistence;
    fEdge: TRestServerFullMemory;
    fHttp: TRestHttpServer;
  public
    /// wire IApiMobile onto an HTTP edge, injecting the persistence by hand
    // - persist is the step 5 ORM persistence (or any other IEventPersistence
    //   implementation), kept alive for the whole server lifetime
    // - port is the TCP port the HTTP server will bind and listen to
    constructor Create(const persist: IEventPersistence;
      const port: RawUtf8 = AUDITTRAIL_HTTP_PORT); reintroduce;
    /// release the HTTP server, the edge server, then the persistence
    destructor Destroy; override;
    /// the in-memory edge server actually publishing IApiMobile (void model)
    property Edge: TRestServerFullMemory
      read fEdge;
    /// the HTTP server exposing the edge to remote mobile clients
    property Http: TRestHttpServer
      read fHttp;
  end;


implementation


{ TAuditTrailServer }

constructor TAuditTrailServer.Create(const persist: IEventPersistence;
  const port: RawUtf8);
begin
  inherited Create;
  fPersistence := persist;
  // the edge server has a void ORM model: it owns no table, so the ORM REST
  // routes simply have no address a client could reach - only services exist
  fEdge := TRestServerFullMemory.CreateWithOwnModel([]);
  // explicit constructor injection at the composition root: the service talks
  // to the private persistence we pass here, never to the edge's own (empty)
  // ORM - the shared instance is owned/freed by fEdge
  fEdge.ServiceDefine(TApiMobile.Create(fPersistence), [IApiMobile]);
  // only the edge is handed to HTTP; the persistence ORM stays off the network
  fHttp := TRestHttpServer.Create(port, [fEdge], '+', HTTP_DEFAULT_MODE);
end;

destructor TAuditTrailServer.Destroy;
begin
  FreeAndNil(fHttp);   // stop accepting requests first
  FreeAndNil(fEdge);   // frees the shared TApiMobile, releasing its persistence
  fPersistence := nil; // releases the private ORM server held by the persistence
  inherited Destroy;
end;

end.
