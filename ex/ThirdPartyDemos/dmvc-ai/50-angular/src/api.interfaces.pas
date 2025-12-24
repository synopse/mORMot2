unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.text,
  mormot.core.interfaces;

type
  /// Customer DTO (Data Transfer Object)
  // Used for JSON serialization to Angular frontend
  TCustomerDto = record
    ID: TID;
    FirstName: RawUtf8;
    LastName: RawUtf8;
    Email: RawUtf8;
    Phone: RawUtf8;
    City: RawUtf8;
    Country: RawUtf8;
  end;

  /// Array of customer DTOs
  TCustomerDtos = array of TCustomerDto;

  /// Customer API interface
  // - Exposes REST endpoints for Angular frontend
  // - All methods are exposed via JSON-RPC over HTTP
  ICustomerApi = interface(IInvokable)
    ['{8E7A3F92-1234-5678-9ABC-DEF012345678}']
    /// Get all customers
    // Exposed as: POST http://localhost:8080/CustomerApi.GetAll
    function GetAll: TCustomerDtos;

    /// Get customer by ID
    // Exposed as: POST http://localhost:8080/CustomerApi.GetById
    // Body: {"id":1}
    function GetById(id: TID): TCustomerDto;

    /// Create new customer
    // Exposed as: POST http://localhost:8080/CustomerApi.CreateCustomer
    // Body: {"customer":{"firstname":"John","lastname":"Doe",...}}
    function CreateCustomer(const customer: TCustomerDto): TID;

    /// Update existing customer
    // Exposed as: POST http://localhost:8080/CustomerApi.Update
    // Body: {"id":1,"customer":{...}}
    procedure Update(id: TID; const customer: TCustomerDto);

    /// Delete customer
    // Exposed as: POST http://localhost:8080/CustomerApi.Delete
    // Body: {"id":1}
    procedure Delete(id: TID);

    /// Search customers by name
    // Exposed as: POST http://localhost:8080/CustomerApi.Search
    // Body: {"query":"John"}
    function Search(const query: RawUtf8): TCustomerDtos;
  end;


implementation


end.
