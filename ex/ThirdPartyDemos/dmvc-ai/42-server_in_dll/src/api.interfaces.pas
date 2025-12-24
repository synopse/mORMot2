unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.interfaces;

type
  /// Main REST API interface for server_in_dll sample
  IMainApi = interface(IInvokable)
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']

    /// Simple GET endpoint returning a message
    function GetMessage: RawUtf8;

    /// Simple calculation endpoint
    function Divide(a, b: Integer): Double;
  end;

implementation

end.
