unit api.impl;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.interfaces,
  mormot.rest.server,
  mormot.soa.server,
  api.interfaces;

type
  /// Concurrency Test API Implementation
  TConcurrencyTestApi = class(TInjectableObjectRest, IConcurrencyTestApi)
  public
    /// Simple endpoint that returns a single character
    // Returns 'X' to minimize processing overhead for pure concurrency testing
    function Index: RawUtf8;
  end;


implementation


{ TConcurrencyTestApi }

function TConcurrencyTestApi.Index: RawUtf8;
begin
  // Minimal response for maximum throughput testing
  Result := 'X';
end;


end.
