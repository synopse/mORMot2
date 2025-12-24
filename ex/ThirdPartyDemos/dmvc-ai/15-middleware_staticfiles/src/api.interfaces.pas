unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.interfaces;

type
  /// Custom filter callback for static file serving
  /// @param PathInfo - The requested file path (can be modified by filter)
  /// @param Allow - Set to False to deny access to the file
  TStaticFileFilter = reference to procedure(var PathInfo: string; var Allow: Boolean);

  /// Simple API service interface
  IApiService = interface(IInvokable)
    ['{8E3F5D21-1A2B-4C3D-8F9E-1B2C3D4E5F60}']

    /// Get a welcome message
    function GetWelcome: RawUtf8;

    /// Get a reversed string
    function ReverseString(const Value: RawUtf8): RawUtf8;
  end;

implementation

end.
