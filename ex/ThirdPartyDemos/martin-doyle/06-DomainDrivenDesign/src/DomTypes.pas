unit DomTypes;

interface

{$I mormot.defines.inc}
uses
  mormot.core.base,
  mormot.core.json,
  mormot.orm.core;

type
  TName = type RawUTF8;
  TQuestion = type RawUTF8;

type
  TSample = packed record
    Name: TName;
    Question: TQuestion;
  end;

  TSampleInfo = packed record
    ID: TID;
    Name: TName;
  end;
  TSampleInfoDynArray = array of TSampleInfo;

implementation


end.
