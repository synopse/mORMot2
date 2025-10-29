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
    function NewSource(const source: TDomSource): TDomSourceID;
    function RemoveSource(id: TDomSourceID): boolean;
    function AddEvent(const event: TDomEvent): TDomEventID;
    function LastEvents(count: integer): TDomEventObjArray;
  end;


implementation

end.
