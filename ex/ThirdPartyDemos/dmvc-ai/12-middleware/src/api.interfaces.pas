unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.interfaces;

type
  /// Simple API service interface for middleware demonstration
  // Port of DMVC TApp1MainController
  IMiddlewareApi = interface(IInvokable)
    ['{8F9E6B4A-1C2D-4E5F-8A9B-3C4D5E6F7A8B}']

    /// Returns a simple text response
    // GET /MiddlewareApi/Index
    // Demonstrates middleware adding headers to responses
    function Index: RawUtf8;

    /// Returns request headers to show what middleware sees
    // GET /MiddlewareApi/ShowHeaders
    function ShowHeaders: RawUtf8;

    /// Echo endpoint for testing
    // GET /MiddlewareApi/Echo?msg=...
    function Echo(const msg: RawUtf8): RawUtf8;
  end;

implementation

end.
