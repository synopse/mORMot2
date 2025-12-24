unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.interfaces,
  mormot.core.rtti,
  mormot.core.variants;

type
  /// Dataset-based Customer API
  // Demonstrates direct SQL queries instead of ORM
  // Similar to DMVC's dataset-based approach
  ICustomerDataApi = interface(IInvokable)
    ['{3B4C5D6E-7F8A-9B0C-1D2E-3F4A5B6C7D8E}']

    /// Get all customers using SQL query
    function GetCustomers: TVariantDynArray;

    /// Get customer by ID using SQL query
    function GetCustomerById(id: Integer): Variant;

    /// Create customer using SQL INSERT
    function CreateCustomer(const code, description, city, note: RawUtf8;
      rating: Integer): Integer;

    /// Update customer using SQL UPDATE
    function UpdateCustomer(id: Integer; const code, description, city,
      note: RawUtf8; rating: Integer): Boolean;

    /// Delete customer using SQL DELETE
    function DeleteCustomer(id: Integer): Boolean;

    /// Get customer statistics with nested data (demonstrates TDocVariant construction)
    // Returns object with customer counts by rating, top cities, etc.
    function GetCustomerStatistics: Variant;

    /// Get customers with extended info (demonstrates nested objects/arrays)
    // Returns array of customers with additional computed fields
    function GetCustomersWithDetails: TVariantDynArray;

    /// Demonstrates various TDocVariant manipulation patterns
    // Returns object showing different access methods and construction patterns
    function DemoDocVariantPatterns: Variant;
  end;

implementation

initialization
  TInterfaceFactory.RegisterInterfaces([TypeInfo(ICustomerDataApi)]);

end.
