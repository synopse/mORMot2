unit api.impl;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.interfaces,
  mormot.core.variants,
  mormot.core.json,
  mormot.orm.core,
  mormot.orm.base,
  mormot.rest.server,
  mormot.soa.server,
  entities,
  api.interfaces;

type
  /// Customer API implementation using mORMot2 ORM
  TCustomerApiService = class(TInjectableObjectRest, ICustomerApi)
  private
    function OrmToVariant(orm: TOrm): Variant;
  public
    function GetCustomer(id: TID): Variant;
    function GetAllCustomers: TVariantDynArray;
    function GetCustomersByCity(const city: RawUtf8): TVariantDynArray;
    function CreateCustomer(const code, companyName, city, note: RawUtf8;
      rating: Integer): TID;
    function UpdateCustomer(id: TID; const code, companyName, city,
      note: RawUtf8; rating: Integer): Boolean;
    function DeleteCustomer(id: TID): Boolean;
    function BulkCreateCustomers(const customers: TVariantDynArray): Integer;
  end;

implementation

{ TCustomerApiService }

function TCustomerApiService.OrmToVariant(orm: TOrm): Variant;
var
  json: RawUtf8;
begin
  if orm = nil then
  begin
    Result := Null;
    Exit;
  end;
  json := orm.GetJsonValues(true, true, ooSelect);
  Result := _JsonFast(json);
end;

function TCustomerApiService.GetCustomer(id: TID): Variant;
var
  customer: TCustomerOrm;
begin
  customer := TCustomerOrm.Create(Server.Orm, id);
  try
    if customer.ID = 0 then
      Result := Null
    else
      Result := OrmToVariant(customer);
  finally
    customer.Free;
  end;
end;

function TCustomerApiService.GetAllCustomers: TVariantDynArray;
var
  customers: TOrmCustomerObjArray;
  i: Integer;
begin
  if Server.Orm.RetrieveListObjArray(customers, TCustomerOrm, '', []) then
  try
    SetLength(Result, length(customers));
    for i := 0 to High(customers) do
      Result[i] := OrmToVariant(customers[i]);
  finally
    ObjArrayClear(customers);
  end;
end;

function TCustomerApiService.GetCustomersByCity(
  const city: RawUtf8): TVariantDynArray;
var
  customers: TOrmCustomerObjArray;
  i: Integer;
begin
  if Server.Orm.RetrieveListObjArray(customers, TCustomerOrm,
    'City=?', [city]) then
  try
    SetLength(Result, length(customers));
    for i := 0 to High(customers) do
      Result[i] := OrmToVariant(customers[i]);
  finally
    ObjArrayClear(customers);
  end;
end;

function TCustomerApiService.CreateCustomer(const code, companyName, city,
  note: RawUtf8; rating: Integer): TID;
var
  customer: TCustomerOrm;
begin
  customer := TCustomerOrm.Create;
  try
    customer.Code := code;
    customer.CompanyName := companyName;
    customer.City := city;
    customer.Rating := rating;
    customer.Note := note;
    Result := Server.Orm.Add(customer, true);
  finally
    customer.Free;
  end;
end;

function TCustomerApiService.UpdateCustomer(id: TID; const code, companyName,
  city, note: RawUtf8; rating: Integer): Boolean;
var
  customer: TCustomerOrm;
begin
  customer := TCustomerOrm.Create(Server.Orm, id);
  try
    if customer.ID = 0 then
      Exit(false);
    customer.Code := code;
    customer.CompanyName := companyName;
    customer.City := city;
    customer.Rating := rating;
    customer.Note := note;
    Result := Server.Orm.Update(customer);
  finally
    customer.Free;
  end;
end;

function TCustomerApiService.DeleteCustomer(id: TID): Boolean;
begin
  Result := Server.Orm.Delete(TCustomerOrm, id);
end;

function TCustomerApiService.BulkCreateCustomers(
  const customers: TVariantDynArray): Integer;
var
  i: Integer;
  customer: TCustomerOrm;
  v: PVariant;
begin
  Result := 0;

  // Demonstrate transaction support
  // Similar to DMVC's TMVCActiveRecord.CurrentConnection.StartTransaction
  if Server.Orm.TransactionBegin(TCustomerOrm) then
  try
    for i := 0 to High(customers) do
    begin
      v := @customers[i];
      customer := TCustomerOrm.Create;
      try
        // Extract values from variant (JSON object)
        customer.Code := VariantToUtf8(DocVariantData(v^)^.U['code']);
        customer.CompanyName := VariantToUtf8(DocVariantData(v^)^.U['companyName']);
        customer.City := VariantToUtf8(DocVariantData(v^)^.U['city']);
        customer.Rating := DocVariantData(v^)^.I['rating'];
        customer.Note := VariantToUtf8(DocVariantData(v^)^.U['note']);

        if Server.Orm.Add(customer, true) > 0 then
          Inc(Result);
      finally
        customer.Free;
      end;
    end;
    Server.Orm.Commit;
  except
    Server.Orm.RollBack;
    raise;
  end;
end;

end.
