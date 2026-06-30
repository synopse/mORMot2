unit data;

interface

{$I mormot.defines.inc}
uses
  mormot.core.base,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.interfaces;

const
  HttpPort = '11111';

type
  TSample = packed record
    Name: RawUTF8;
    Question: RawUTF8;
  end;

  TSampleInfo = packed record
    ID: TID;
    Name: RawUTF8;
  end;
  TSampleInfoDynArray = array of TSampleInfo;

  IExample = interface(IInvokable)
    ['{52A512A1-8A54-4A2E-BB24-F87501DBA396}']
    function Add(var ASample: TSample): Integer;
    function Find(var ASample: TSample): Integer;
    function List(out ASamples: TSampleInfoDynArray): Integer;
    function Delete(AID: TID): Integer;
  end;


implementation


initialization
  Rtti.RegisterFromText([
    TypeInfo(TSample),     'name,question:RawUtf8',
    TypeInfo(TSampleInfo), 'id:TID name:RawUtf8'
  ]);
  TInterfaceFactory.RegisterInterfaces([
    TypeInfo(IExample)]);
end.
