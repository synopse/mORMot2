unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.interfaces;

type
  /// Concurrency Test API Interface
  // Minimal interface for performance testing
  IConcurrencyTestApi = interface(IInvokable)
    ['{A5B6C7D8-E9F0-4A1B-8C2D-3E4F5A6B7C8D}']

    /// Simple endpoint that returns a single character
    // Used for concurrency and load testing
    function Index: RawUtf8;
  end;


implementation


end.
