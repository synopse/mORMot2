unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.interfaces;

type
  IMyApi = interface(IInvokable)
    ['{8E7F2A4C-9B3D-4E1F-A6C8-5D2E9F1B4A7C}']
    function Index: RawJson;
    function GetPeople: RawJson;
  end;

implementation

end.
