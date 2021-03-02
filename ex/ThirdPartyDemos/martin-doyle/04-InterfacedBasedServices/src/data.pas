unit data;

interface

{$I mormot.defines.inc}
uses
  mormot.core.base,
  mormot.core.data,
  mormot.core.json,
  mormot.core.interfaces,
  mormot.orm.core;

const
  HttpPort = '11111';

type
  TSample = packed record
    Name: RawUTF8;
    Question: RawUTF8;
  end;

  IExample = interface(IInvokable)
    ['{52A512A1-8A54-4A2E-BB24-F87501DBA396}']
    function Add(var ASample: TSample): Integer;
    function Find(var ASample: TSample): Integer;
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
  result := TOrmModel.Create([TOrmSample]);
end;


initialization

  TInterfaceFactory.RegisterInterfaces([TypeInfo(IExample)]);
end.
