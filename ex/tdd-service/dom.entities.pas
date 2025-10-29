unit dom.entities;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.orm.core;

// note: we are in KDD-mode = use TOrm as meta-type with IDValue: TID

type
  /// some specialized 64-bit ID numbers - using strong typing
  TDomSourceID = type TID;
  TDomEventID  = type TID;

  /// domain entity storing an Audit Trail Event Source
  TDomSource = class(TOrm)
  protected
    fDescription: RawUtf8;
  published
    /// some human-friendly text giving information about this event source
    property Description: RawUtf8
      read fDescription write fDescription;
  end;
  TDomSourceObjArray = array of TDomSource;

  /// domain entity storing an Audit Trail Event
  TDomEvent = class(TOrm)
  protected
    fDescription: RawUtf8;
    fContext: variant;
    fSource: TDomSourceID;
    fTimeStamp: TUnixMSTime;
  public
    /// normalize the fields content
    procedure Normalize; virtual;
    /// quickly check this entity consistency
    function HasAllNeededFields: boolean; virtual;
  published
    /// a milliseconds Unix timestamp
    property TimeStamp: TUnixMSTime
      read fTimeStamp write fTimeStamp;
    /// is a 64-bit ID to identify the node generating this event
    // - should not be void / 0
    property Source: TDomSourceID
      read fSource write fSource;
    /// some human-friendly text giving information about this event
    // - should not be void
    property Description: RawUtf8
      read fDescription write fDescription;
    /// is likely to be a TDocVariant with any kind of JSON
    // - should not be void
    property Context: variant
      read fContext write fContext;
  end;
  TDomEventObjArray = array of TDomEvent;


implementation


{ TDomEvent }

procedure TDomEvent.Normalize;
begin
  TrimSelf(fDescription);
end;

function TDomEvent.HasAllNeededFields: boolean;
begin
  result := false;
  Normalize;
  if (fSource = 0) or
     (fDescription = '') or
     VarIsEmptyOrNull(fContext) then
    exit;
  result := true; // OK
end;


end.
