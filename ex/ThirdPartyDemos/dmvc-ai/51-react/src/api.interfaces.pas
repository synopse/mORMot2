unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.rtti,
  mormot.core.interfaces,
  entities;

type
  /// Customers CRUD API interface
  // Port of DMVC TCustomersController to mORMot2 interface-based services
  // Provides RESTful CRUD operations for customers
  ICustomersApi = interface(IInvokable)
    ['{F8E7D6C5-B4A3-9281-7069-58473A2B1C0D}']

    /// Get all customers
    // GET /customers
    function GetAll: TCustomerDtos;

    /// Get customer by ID
    // GET /customers/:id
    function GetById(id: TID): TCustomerDto;

    /// Create new customer
    // POST /customers
    function CreateCustomer(const customer: TCustomerDto): TID;

    /// Update existing customer
    // PUT /customers/:id
    procedure Update(id: TID; const customer: TCustomerDto);

    /// Delete customer
    // DELETE /customers/:id
    procedure Delete(id: TID);
  end;


implementation


initialization
  // Customize DTO field names (match JSON case from frontend)
  Rtti.RegisterFromText(TypeInfo(TCustomerDto),
    'ID:Int64 Code:RawUtf8 Description:RawUtf8 City:RawUtf8 Note:RawUtf8 Rating:Integer');

  // Register interface for TypeInfo(ICustomersApi)
  TInterfaceFactory.RegisterInterfaces([TypeInfo(ICustomersApi)]);

end.
