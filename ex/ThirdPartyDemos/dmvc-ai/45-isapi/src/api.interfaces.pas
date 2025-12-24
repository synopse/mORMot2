unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.interfaces;

type
  /// Simple calculator service interface for ISAPI demonstration
  {$M+}
  ICalculator = interface(IInvokable)
    ['{8B5E9D12-3C4A-4F5B-9E6D-1A2B3C4D5E6F}']
    /// Divide two numbers
    function Divide(const a, b: Double): Double;
    /// Get current server timestamp
    function GetTimestamp: RawUtf8;
  end;
  {$M-}


implementation


end.
