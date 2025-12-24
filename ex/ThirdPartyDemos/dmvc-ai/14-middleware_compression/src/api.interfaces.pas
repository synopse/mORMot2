unit api.interfaces;

interface

uses
  mormot.core.base,
  mormot.rest.core;

type
  ICustomersApi = interface(IInvokable)
    ['{8F7B9C3D-4E2A-4F1B-9D8C-6A5E3F2B1C0D}']

    /// Get large customer list (demonstrates compression enabled)
    function GetCustomers: RawJson;

    /// Get small customer list (demonstrates compression threshold)
    function GetTallCustomers: RawJson;
  end;

implementation

end.
