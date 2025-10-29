unit dom.infra;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.os,
  dom.entities;

type
  /// a single interface, which may be later on split into source/event types
  IEventPersistence = interface(IInvokable)
    function IsSource(id: TDomSourceID): boolean;
    function NewSource(source: TDomSource): TDomSourceID;
    function RemoveSource(id: TDomSourceID): boolean;
    function AddEvent(event: TDomEvent): TDomEventID;
    function LastEvents(source: TDomSourceID; count: integer): TDomEventObjArray;
  end;


implementation

end.
