unit api.mobile.impl;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.os,
  mormot.core.variants,
  mormot.core.interfaces,
  api.mobile,
  dom.entities,
  dom.infra; // IEventPersistence

type
  /// implement the mobile API on the server side
  TApiMobile = class(TInterfacedObject, IApiMobile)
  protected
    fPersistence: IEventPersistence;
  public
    constructor Create(const persist: IEventPersistence); reintroduce;
    // IApiMobile methods
    function Login(id: TSourceID): boolean;
    function Register(const descr: RawUtf8): TSourceID;
    function UnRegister(id: TSourceID): boolean;
    function NewEvent(id: TSourceID; const eventinfo: RawUtf8): TEventID;
  end;


implementation


{ TApiMobile }

constructor TApiMobile.Create(const persist: IEventPersistence);
begin
  fPersistence := persist;
end;

function TApiMobile.Login(id: TSourceID): boolean;
begin
  result := false;
  if not Assigned(fPersistence) or
    not fPersistence.IsSource(id) then
    exit;
  result := true;
end;

function TApiMobile.Register(const descr: RawUtf8): TSourceID;
var
  s: TDomSource;
begin
  result := 0;
  if not Assigned(fPersistence) then
    exit;
  s := TDomSource.Create;
  try
    s.Description := descr;
    result := fPersistence.NewSource(s);
  finally
    s.Free;
  end;
end;

function TApiMobile.UnRegister(id: TSourceID): boolean;
begin
  result := false;
  if not Assigned(fPersistence) then
    exit;
  result := fPersistence.RemoveSource(id);
end;

function TApiMobile.NewEvent(id: TSourceID; const eventinfo: RawUtf8): TEventID;
var
  e: TDomEvent;
begin
  result := 0;
  if not Assigned(fPersistence) or
    not fPersistence.IsSource(id) then
    exit;
  e := TDomEvent.Create;
  try
    e.Source := id;
    e.Description := eventinfo;
    e.Context := _ObjFast(['kind', 'mobile']);
    if not e.HasAllNeededFields then
      exit;
    result := fPersistence.AddEvent(e);
  finally
    e.Free;
  end;
end;


end.
