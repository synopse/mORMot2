unit entities;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.orm.core;

type
  /// Message ID type - using strong typing
  TMessageID = type TID;

  /// Simple message entity for demo
  TOrmMessage = class(TOrm)
  protected
    fContent: RawUtf8;
    fTimestamp: TUnixMSTime;
  public
    /// normalize the fields content
    procedure Normalize; virtual;
    /// quickly check this entity consistency
    function HasAllNeededFields: boolean; virtual;
  published
    /// message content
    property Content: RawUtf8
      read fContent write fContent;
    /// timestamp when message was created
    property Timestamp: TUnixMSTime
      read fTimestamp write fTimestamp;
  end;
  TOrmMessageObjArray = array of TOrmMessage;


implementation


{ TOrmMessage }

procedure TOrmMessage.Normalize;
begin
  TrimSelf(fContent);
end;

function TOrmMessage.HasAllNeededFields: boolean;
begin
  result := false;
  Normalize;
  if (fContent = '') then
    exit;
  result := true; // OK
end;


end.
