unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.interfaces,
  mormot.core.variants,
  entities;

type
  /// Simple Customer API interface
  // Demonstrates basic CRUD operations with ActiveRecord pattern
  ICustomerApi = interface(IInvokable)
    ['{2A3B4C5D-6E7F-8A9B-0C1D-2E3F4A5B6C7D}']

    /// Get customer by ID
    function GetCustomer(id: TID): Variant;

    /// Get all customers
    function GetAllCustomers: TVariantDynArray;

    /// Get customers by city
    function GetCustomersByCity(const city: RawUtf8): TVariantDynArray;

    /// Create new customer
    function CreateCustomer(const code, companyName, city, note: RawUtf8;
      rating: Integer): TID;

    /// Update customer
    function UpdateCustomer(id: TID; const code, companyName, city,
      note: RawUtf8; rating: Integer): Boolean;

    /// Delete customer
    function DeleteCustomer(id: TID): Boolean;

    /// Bulk create customers (transaction demo)
    function BulkCreateCustomers(const customers: TVariantDynArray): Integer;
  end;

implementation

end.
