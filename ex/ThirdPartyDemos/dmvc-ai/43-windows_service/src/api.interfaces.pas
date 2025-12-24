unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.interfaces,
  mormot.core.base;

type
  /// Service status information
  TServiceStatus = packed record
    Running: Boolean;
    Uptime: Integer; // seconds
    Port: Integer;
    Message: RawUtf8;
  end;

  /// REST API interface for Windows Service
  IServiceApi = interface(IInvokable)
    ['{B1C2D3E4-F5A6-7890-BCDE-FA1234567890}']

    /// Get service status
    function GetStatus: TServiceStatus;

    /// Echo endpoint for testing
    function Echo(const aMessage: RawUtf8): RawUtf8;
  end;

implementation

end.
