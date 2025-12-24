unit entities;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.orm.core;

type
  /// specialized 64-bit ID numbers - using strong typing
  TSampleID = type TID;

  /// domain entity storing sample data
  TOrmSample = class(TOrm)
  protected
    fName: RawUtf8;
    fDescription: RawUtf8;
    fCreatedAt: TUnixMSTime;
  public
    /// normalize the fields content
    procedure Normalize; virtual;
    /// quickly check this entity consistency
    function HasAllNeededFields: boolean; virtual;
  published
    /// sample name
    // - should not be void
    property Name: RawUtf8
      read fName write fName;
    /// human-friendly description
    property Description: RawUtf8
      read fDescription write fDescription;
    /// a milliseconds Unix timestamp
    property CreatedAt: TUnixMSTime
      read fCreatedAt write fCreatedAt;
  end;
  TOrmSampleObjArray = array of TOrmSample;


implementation


{ TOrmSample }

procedure TOrmSample.Normalize;
begin
  TrimSelf(fName);
  TrimSelf(fDescription);
end;

function TOrmSample.HasAllNeededFields: boolean;
begin
  result := false;
  Normalize;
  if (fName = '') then
    exit;
  result := true; // OK
end;


end.
