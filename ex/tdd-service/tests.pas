unit tests;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.test,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.soa.core,
  mormot.rest.http.client,
  mormot.db.raw.sqlite3,
  mormot.db.raw.sqlite3.static,
  dom.entities,
  dom.infra,
  infra.orm,
  api.mobile,
  api.mobile.impl,
  app.server;


type
  TAuditTrailTests = class(TSynTestsLogged)
  published
    procedure Domain;
    procedure MobileApi;
  end;

  TAuditTrailDomainTests = class(TSynTestCase)
  published
    // some unit tests for Domain entities
    procedure Entities;
  end;

  TAuditTrailMobileApiTests = class(TSynTestCase)
  protected
    fMobileSeq: integer;
    procedure TestMobile(const mobile: IApiMobile);
  published
    procedure DirectCall;
    procedure RemoteCall;
  end;


implementation


{ TAuditTrailTests }

procedure TAuditTrailTests.Domain;
begin
  AddCase([TAuditTrailDomainTests]);
end;

procedure TAuditTrailTests.MobileApi;
begin
  AddCase([TAuditTrailMobileApiTests]);
end;


{ TAuditTrailDomainTests }

procedure TAuditTrailDomainTests.Entities;
var
  e: TDomEvent;
begin
  e := TDomEvent.Create;
  try
    e.Normalize;
    Check(not e.HasAllNeededFields);
    e.Source := 1;
    Check(not e.HasAllNeededFields);
    e.Description := 'toto';
    e.Context := 123456;
    Check(e.HasAllNeededFields);
    e.Description := ' ';
    Check(not e.HasAllNeededFields);
    e.Description := 'titi';
    Check(e.HasAllNeededFields);
  finally
    e.Free;
  end;
end;


{ TAuditTrailMobileApiTests }

procedure TAuditTrailMobileApiTests.TestMobile(const mobile: IApiMobile);
var
  id: TSourceID;
  ev: TEventID;
  evs: TEvents;
begin
  if CheckFailed(Assigned(mobile), 'no API') then
    exit;
  // validate mobile sourcing
  id := mobile.Register(Make(['test', InterlockedIncrement(fMobileSeq)]));
  CheckNotEqual(id, 0, 'register');
  Check(mobile.Login(id), 'registered');
  // validate event posting
  Check(mobile.LastEvent(id, 10) = nil, 'last event');
  ev := mobile.NewEvent(0, 'toto');
  CheckEqual(ev, 0, 'no id');
  ev := mobile.NewEvent(id, ' ');
  CheckEqual(ev, 0, 'no description');
  ev := mobile.NewEvent(id, 'toto');
  CheckNotEqual(ev, 0, 'new event');
  evs := mobile.LastEvent(id, 10);
  if CheckEqual(length(evs), 1) then
  begin
    CheckEqual(evs[0].ID, id, 'evs id');
    CheckNotEqual(evs[0].TimeStamp, 0, 'evs time');
    CheckEqual(evs[0].Description, 'toto', 'evs');
  end;
  // validate mobile unsourcing
  Check(mobile.UnRegister(id), 'unregister');
  Check(not mobile.Login(id), 'unregistered');
  Check(not mobile.UnRegister(id), 'unregister twice');
  Check(not mobile.Login(id), 'still unregistered');
end;

procedure TAuditTrailMobileApiTests.DirectCall;
var
  api: IApiMobile;
  persistence: IEventPersistence;
begin
  persistence := TEventPersistence.Create(SQLITE_MEMORY_DATABASE_NAME);
  api := TApiMobile.Create(persistence);
  TestMobile(api);
end;

procedure TAuditTrailMobileApiTests.RemoteCall;
var
  server: TAuditTrailServer;
  client: TRestHttpClient;
  persistence: IEventPersistence;
  api: IApiMobile;
begin
  // same wiring as the stand-alone daemon (AuditTrailServer.dpr), in-process:
  // an in-memory SQLite3 persistence behind the HTTP edge publishing IApiMobile
  persistence := TEventPersistence.Create(SQLITE_MEMORY_DATABASE_NAME);
  server := TAuditTrailServer.Create(persistence, AUDITTRAIL_HTTP_PORT);
  try
    // connect a real HTTP client and resolve IApiMobile as a remote proxy
    client := TRestHttpClient.CreateWithOwnModel(
      '127.0.0.1', AUDITTRAIL_HTTP_PORT, 'root');
    try
      client.ServiceDefine([IApiMobile], sicShared);
      if CheckFailed(client.Services.Resolve(TypeInfo(IApiMobile), api),
           'resolve IApiMobile') then
        exit;
      // exactly the same assertions as DirectCall, now over the wire:
      // this is the location transparency the Clean Architecture relies on
      TestMobile(api);
    finally
      api := nil;
      client.Free;
    end;
  finally
    server.Free;
  end;
end;

end.
