unit data;

interface

{$I mormot.defines.inc}
uses
  mormot.core.base,
  mormot.orm.base,
  mormot.orm.core;

const
  HttpPort = '11111';

type
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
  result := TOrmModel.Create([TOrmSample]);
end;

end.
