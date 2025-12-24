unit api.impl;

{$I mormot.defines.inc}

interface

uses
  sysutils,
  mormot.core.base,
  mormot.core.json,
  mormot.core.text,
  mormot.core.interfaces,
  mormot.core.log,
  mormot.orm.core,
  entities,
  api.interfaces;

type
  /// Implementation of Customer API
  TCustomerApi = class(TInjectableObject, ICustomerApi)
  protected
    fOrm: IRestOrm;
  public
    constructor Create(const aRest: IRestOrm); reintroduce;

    // ICustomerApi implementation
    function CreateCustomer(const hello: RawUtf8): RawJson;
    function GetCustomer(id: TID): TCustomerDto;
    function GetAllCustomers: TRawUtf8DynArray;
  end;


implementation


{ TCustomerApi }

constructor TCustomerApi.Create(const aRest: IRestOrm);
begin
  inherited Create;
  fOrm := aRest;
  TSynLog.Add.Log(sllDebug, 'TCustomerApi created');
end;

function TCustomerApi.CreateCustomer(const hello: RawUtf8): RawJson;
var
  customer: TOrmCustomer;
  response: RawUtf8;
begin
  TSynLog.Add.Log(sllDebug, 'CreateCustomer called with hello=%', [hello]);

  // Create customer with demo data
  customer := TOrmCustomer.Create;
  try
    customer.Name := 'Demo Customer';
    customer.Email := FormatUtf8('demo@%.com', [hello]);

    if fOrm.Add(customer, true) <> 0 then
    begin
      // Return response matching DMVC format: {"data":{"message":"..."}}
      response := FormatUtf8('{"data":{"message":"Customer created with message: %"}}', [hello]);
      Result := RawJson(response);
      TSynLog.Add.Log(sllDebug, 'Customer created successfully: ID=%', [customer.ID]);
    end
    else
    begin
      Result := RawJson('{"error":"Failed to create customer"}');
      TSynLog.Add.Log(sllWarning, 'Failed to create customer');
    end;
  finally
    customer.Free;
  end;
end;

function TCustomerApi.GetCustomer(id: TID): TCustomerDto;
var
  customer: TOrmCustomer;
begin
  TSynLog.Add.Log(sllDebug, 'GetCustomer called with id=%', [id]);

  customer := TOrmCustomer.Create(fOrm, id);
  try
    if customer.ID <> 0 then
    begin
      Result.id := customer.ID;
      Result.name := customer.Name;
      Result.email := customer.Email;
      TSynLog.Add.Log(sllDebug, 'Customer found: % (%)', [customer.Name, customer.Email]);
    end
    else
    begin
      // Return empty result
      Result.id := 0;
      Result.name := '';
      Result.email := '';
      TSynLog.Add.Log(sllWarning, 'Customer not found: id=%', [id]);
    end;
  finally
    customer.Free;
  end;
end;

function TCustomerApi.GetAllCustomers: TRawUtf8DynArray;
var
  customers: array of TOrmCustomer;
  i: Integer;
begin
  TSynLog.Add.Log(sllDebug, 'GetAllCustomers called');

  // Retrieve all customers from database
  if fOrm.RetrieveListObjArray(customers, TOrmCustomer, '', []) then
  begin
    SetLength(Result, Length(customers));
    for i := 0 to High(customers) do
    begin
      Result[i] := FormatUtf8('{"id":%,"name":"%","email":"%"}',
        [customers[i].ID, customers[i].Name, customers[i].Email]);
      customers[i].Free;  // Free each customer object
    end;
    TSynLog.Add.Log(sllDebug, 'Returning % customers', [Length(Result)]);
  end
  else
  begin
    SetLength(Result, 0);
    TSynLog.Add.Log(sllDebug, 'No customers found');
  end;
end;


end.
