unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.interfaces,
  mormot.core.base;

type
  /// Calculator API for servercontainer demo
  ICalculatorApi = interface(IInvokable)
    ['{C1D2E3F4-A5B6-7890-CDEF-AB1234567890}']

    /// Divide two numbers
    function Divide(a, b: Integer): Double;

    /// Add two numbers
    function Add(a, b: Integer): Integer;

    /// Get server info
    function GetInfo: RawUtf8;
  end;

implementation

end.
