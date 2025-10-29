unit infra.orm;

{$I mormot.defines.inc}

interface

uses
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  dom.entities,
  dom.infra,
  mormot.orm.core,
  mormot.rest.sqlite3;

type
  /// a single interface, to eventually be split into source/event types
  TEventPersistence = class(TInterfacedObject, IEventPersistence)
  protected
    fRest: TRestServerDB;
    fOrm: IRestOrm;
  public
    constructor Create(const aFileName: TFileName); reintroduce;
    destructor Destroy; override;
    // IEventPersistence methods
    function IsSource(id: TDomSourceID): boolean;
    function NewSource(source: TDomSource): TDomSourceID;
    function RemoveSource(id: TDomSourceID): boolean;
    function AddEvent(event: TDomEvent): TDomEventID;
    function LastEvents(source: TDomSourceID; count: integer): TDomEventObjArray;
  end;


implementation


{ TEventPersistence }

constructor TEventPersistence.Create(const aFileName: TFileName);
begin
  fRest := TRestServerDB.CreateSqlite3([TDomSource, TDomEvent], aFileName);
  fOrm := fRest.Orm;
end;

destructor TEventPersistence.Destroy;
begin
  fOrm := nil; // to be released before fRest owner
  fRest.Free;
  inherited Destroy;
end;

function TEventPersistence.IsSource(id: TDomSourceID): boolean;
begin
  result := fOrm.MemberExists(TDomSource, id);
end;

function TEventPersistence.NewSource(source: TDomSource): TDomSourceID;
begin
  result := fOrm.Add(source, {send=}true);
end;

function TEventPersistence.RemoveSource(id: TDomSourceID): boolean;
begin
  result := IsSource(id) and
            fOrm.Delete(TDomSource, id);
end;

function TEventPersistence.AddEvent(event: TDomEvent): TDomEventID;
begin
  result := 0;
  if not event.HasAllNeededFields then
    exit;
  event.TimeStamp := UnixMSTimeUtcFast;
  result := fOrm.Add(event, {send=}true);
end;

function TEventPersistence.LastEvents(source: TDomSourceID;
  count: integer): TDomEventObjArray;
begin
  ObjArrayClear(result);
  if (count <= 0) or
     not IsSource(source) then
    exit;
  if count > 50 then
    count := 50; // not too many
  fOrm.RetrieveListObjArray(result, TDomEvent,
    Make(['where source=? order by timestamp desc limit ', count]), [source]);
end;

end.
