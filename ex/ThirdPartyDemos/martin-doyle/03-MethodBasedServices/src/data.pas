unit data;

interface

{$I mormot.defines.inc}
uses
  mormot.core.base,
  mormot.core.data,
  mormot.core.json,
  mormot.orm.core;

const
  HttpPort = '11111';
  __TRestSampleValue = 'Name: RawUTF8; Question: RawUTF8';

type
  TSample = packed record
    Name: RawUTF8;
    Question: RawUTF8;
  end;

  TOrmSample = class(TOrm)
  private
    FName: RawUTF8;
    FQuestion: RawUTF8;
    FTime: TModTime;
  published
    property Name: RawUTF8 read FName write FName;
    property Question: RawUTF8 read FQuestion write FQuestion;
    property Time: TModTime read FTime write FTime;
  end;

  function CreateSampleModel: TOrmModel;

implementation

function CreateSampleModel: TOrmModel;
begin
  Result := TOrmModel.Create([TOrmSample]);
end;

initialization

end.
