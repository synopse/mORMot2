unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.json,
  mormot.rest.core;

type
  /// REST API for demonstrating analytics middleware
  // Port of DMVC TMainController
  IAnalyticsApi = interface(IInvokable)
    ['{B7F8A3E1-2D4C-4F5E-9C8A-1B6D5E3F4A7C}']

    /// Basic hello world endpoint
    // Port of DMVC Index action
    function Index: RawJson;

    /// Returns reversed string
    // Port of DMVC GetReversedString
    function GetReversedString(const aValue: RawUtf8): RawUtf8;

    /// Triggers an error for testing error analytics
    // Port of DMVC DoError
    procedure DoError;

    /// Sample CRUD endpoint - get all customers
    function GetCustomers: RawJson;

    /// Sample CRUD endpoint - get customer by ID
    function GetCustomer(aId: Integer): RawJson;

    /// Sample CRUD endpoint - create customer
    function CreateCustomer(const aCustomerData: RawJson): Integer;

    /// Sample CRUD endpoint - update customer
    procedure UpdateCustomer(aId: Integer; const aCustomerData: RawJson);

    /// Sample CRUD endpoint - delete customer
    procedure DeleteCustomer(aId: Integer);
  end;

implementation

end.
