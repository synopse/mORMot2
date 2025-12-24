unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.json;

type
  /// REST API for demonstrating trace middleware
  // Port of DMVC TMainController
  ITraceApi = interface(IInvokable)
    ['{C8F9B4D2-3E5C-4F6E-AD9B-2C7E6F4D5B8A}']

    /// Basic hello world endpoint
    function Index: RawJson;

    /// Returns reversed string
    function GetReversedString(const aValue: RawUtf8): RawUtf8;

    /// Triggers an error for tracing error scenarios
    procedure DoError;

    /// Sample endpoint that returns JSON data
    function GetCustomers: RawJson;

    /// Sample endpoint with parameter
    function GetCustomer(aId: Integer): RawJson;

    /// Sample POST endpoint
    function CreateCustomer(const aCustomerData: RawJson): Integer;

    /// Sample PUT endpoint
    procedure UpdateCustomer(aId: Integer; const aCustomerData: RawJson);

    /// Sample DELETE endpoint
    procedure DeleteCustomer(aId: Integer);
  end;

implementation

end.
