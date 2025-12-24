unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.interfaces;

type
  /// Customer Data Transfer Object
  // - Simple DTO for customer data
  TCustomerDto = packed record
    id: TID;
    name: RawUtf8;
    email: RawUtf8;
  end;

  /// API response with data
  TCustomerResponse = packed record
    data: TCustomerDto;
  end;

  /// Customer API Service Interface
  // - Demonstrates CORS-enabled REST API
  // - All methods can be called from web browsers
  ICustomerApi = interface(IInvokable)
    ['{8B3F9A2E-1C4D-4E5F-9A2B-3C4D5E6F7A8B}']

    /// Create a new customer
    // - POST /CustomerApi/CreateCustomer
    // - Accepts JSON: {"hello":"world"} for demo compatibility
    // - Returns: {"data":{"message":"..."}}
    function CreateCustomer(const hello: RawUtf8): RawJson;

    /// Get a customer by ID
    // - POST /CustomerApi/GetCustomer {"id":123}
    // - Returns customer data
    function GetCustomer(id: TID): TCustomerDto;

    /// Get all customers
    // - POST /CustomerApi/GetAllCustomers
    // - Returns array of customers
    function GetAllCustomers: TRawUtf8DynArray;
  end;


implementation


end.
