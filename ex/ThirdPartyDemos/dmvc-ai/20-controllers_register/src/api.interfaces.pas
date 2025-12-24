unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.interfaces,
  entities;

type
  /// Service 1: General API information
  /// Port of DMVC MyController1 (/api)
  IService1 = interface(IInvokable)
    ['{8B7E9A4C-1D3F-4E6B-9C2A-5F8D7E3B4A1C}']

    /// Get API information (online status, server datetime)
    /// Original: GET /api
    function GetInfo: TPerson; // Reusing TPerson for JSON structure
  end;

  /// Service 2: Person API
  /// Port of DMVC MyController2 (/api/person)
  IService2 = interface(IInvokable)
    ['{7C6D8B3E-2A4F-5D9C-8E1B-4A7C9F2D6E3B}']

    /// Get person information
    /// Original: GET /api/person
    function GetPerson: TPerson;
  end;

  /// Service Registry Manager
  /// Allows dynamic registration/unregistration of services
  IServiceRegistry = interface(IInvokable)
    ['{9D8E7F4A-3B5C-6E2D-7F1A-5C8B9E4D3A2F}']

    /// List all registered services
    function ListServices: RawUtf8;

    /// Load (register) a service by name
    function LoadService(const ServiceName: RawUtf8): Boolean;

    /// Unload (unregister) a service by name
    function UnloadService(const ServiceName: RawUtf8): Boolean;

    /// Get service status
    function GetServiceStatus(const ServiceName: RawUtf8): RawUtf8;
  end;

implementation

end.
