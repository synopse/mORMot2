unit entities;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.orm.core;

type
  /// ORM entity for a simple greeting record
  TOrmGreeting = class(TOrm)
  protected
    fName: RawUtf8;
    fMessage: RawUtf8;
    fTimestamp: TUnixMSTime;
  public
    /// validate that all required fields are set
    function HasAllNeededFields: boolean;
  published
    /// the name of the person to greet
    property Name: RawUtf8 read fName write fName;
    /// the greeting message
    property Message: RawUtf8 read fMessage write fMessage;
    /// when the greeting was created (Unix timestamp in milliseconds)
    property Timestamp: TUnixMSTime read fTimestamp write fTimestamp;
  end;

  /// dynamic array of TOrmGreeting
  TOrmGreetingObjArray = array of TOrmGreeting;

implementation

{ TOrmGreeting }

function TOrmGreeting.HasAllNeededFields: boolean;
begin
  result := (fName <> '') and (fMessage <> '');
end;

end.
