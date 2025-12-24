unit api.impl;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.log,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.soa.core,
  mormot.soa.server,
  api.interfaces,
  entities;

type
  /// Customer CRUD API implementation
  // Ports DMVC Angular sample backend to mORMot2
  TCustomerApi = class(TInjectableObjectRest, ICustomerApi)
  protected
    /// Converts TOrmCustomer to TCustomerDto
    function OrmToDto(const orm: TOrmCustomer): TCustomerDto;
    /// Converts TCustomerDto to TOrmCustomer
    procedure DtoToOrm(const dto: TCustomerDto; orm: TOrmCustomer);
  public
    // ICustomerApi methods
    function GetAll: TCustomerDtos;
    function GetById(id: TID): TCustomerDto;
    function CreateCustomer(const customer: TCustomerDto): TID;
    procedure Update(id: TID; const customer: TCustomerDto);
    procedure Delete(id: TID);
    function Search(const query: RawUtf8): TCustomerDtos;
  end;


implementation


{ TCustomerApi }

function TCustomerApi.OrmToDto(const orm: TOrmCustomer): TCustomerDto;
begin
  result.ID := orm.ID;
  result.FirstName := orm.FirstName;
  result.LastName := orm.LastName;
  result.Email := orm.Email;
  result.Phone := orm.Phone;
  result.City := orm.City;
  result.Country := orm.Country;
end;

procedure TCustomerApi.DtoToOrm(const dto: TCustomerDto; orm: TOrmCustomer);
begin
  orm.FirstName := dto.FirstName;
  orm.LastName := dto.LastName;
  orm.Email := dto.Email;
  orm.Phone := dto.Phone;
  orm.City := dto.City;
  orm.Country := dto.Country;
  // Don't set ID - handled by ORM
end;

function TCustomerApi.GetAll: TCustomerDtos;
var
  customers: TOrmCustomers;
  i: PtrInt;
begin
  TSynLog.Add.Log(sllInfo, 'GetAll: Fetching all customers');

  if fServer.Orm.RetrieveListObjArray(customers, TOrmCustomer, '', []) then
  try
    SetLength(result, Length(customers));
    for i := 0 to High(customers) do
      result[i] := OrmToDto(customers[i]);

    TSynLog.Add.Log(sllInfo, 'GetAll: Returned % customers', [Length(result)]);
  finally
    ObjArrayClear(customers);
  end;
end;

function TCustomerApi.GetById(id: TID): TCustomerDto;
var
  customer: TOrmCustomer;
begin
  TSynLog.Add.Log(sllInfo, 'GetById: id=%', [id]);

  customer := TOrmCustomer.Create(fServer.Orm, id);
  try
    if customer.ID = 0 then
      raise EServiceException.CreateUtf8('Customer % not found', [id]);

    result := OrmToDto(customer);
    TSynLog.Add.Log(sllInfo, 'GetById: Found customer %', [result.Email]);
  finally
    customer.Free;
  end;
end;

function TCustomerApi.CreateCustomer(const customer: TCustomerDto): TID;
var
  orm: TOrmCustomer;
begin
  TSynLog.Add.Log(sllInfo, 'CreateCustomer: email=%', [customer.Email]);

  orm := TOrmCustomer.Create;
  try
    DtoToOrm(customer, orm);

    result := fServer.Orm.Add(orm, true);
    if result = 0 then
      raise EServiceException.Create('Failed to create customer');

    TSynLog.Add.Log(sllInfo, 'CreateCustomer: Created customer id=%', [result]);
  finally
    orm.Free;
  end;
end;

procedure TCustomerApi.Update(id: TID; const customer: TCustomerDto);
var
  orm: TOrmCustomer;
begin
  TSynLog.Add.Log(sllInfo, 'Update: id=%, email=%', [id, customer.Email]);

  orm := TOrmCustomer.Create(fServer.Orm, id);
  try
    if orm.ID = 0 then
      raise EServiceException.CreateUtf8('Customer % not found', [id]);

    DtoToOrm(customer, orm);

    if not fServer.Orm.Update(orm) then
      raise EServiceException.CreateUtf8('Failed to update customer %', [id]);

    TSynLog.Add.Log(sllInfo, 'Update: Updated customer id=%', [id]);
  finally
    orm.Free;
  end;
end;

procedure TCustomerApi.Delete(id: TID);
var
  customer: TOrmCustomer;
begin
  TSynLog.Add.Log(sllInfo, 'Delete: id=%', [id]);

  customer := TOrmCustomer.Create(fServer.Orm, id);
  try
    if customer.ID = 0 then
      raise EServiceException.CreateUtf8('Customer % not found', [id]);

    if not fServer.Orm.Delete(TOrmCustomer, id) then
      raise EServiceException.CreateUtf8('Failed to delete customer %', [id]);

    TSynLog.Add.Log(sllInfo, 'Delete: Deleted customer id=%', [id]);
  finally
    customer.Free;
  end;
end;

function TCustomerApi.Search(const query: RawUtf8): TCustomerDtos;
var
  customers: TOrmCustomers;
  i: PtrInt;
  whereClause: RawUtf8;
begin
  TSynLog.Add.Log(sllInfo, 'Search: query=%', [query]);

  if query = '' then
  begin
    result := GetAll;
    Exit;
  end;

  // Search in first name, last name, or email
  whereClause := 'FirstName LIKE ? OR LastName LIKE ? OR Email LIKE ? ORDER BY LastName, FirstName';
  if fServer.Orm.RetrieveListObjArray(customers, TOrmCustomer, whereClause,
    ['%' + query + '%', '%' + query + '%', '%' + query + '%']) then
  try
    SetLength(result, Length(customers));
    for i := 0 to High(customers) do
      result[i] := OrmToDto(customers[i]);

    TSynLog.Add.Log(sllInfo, 'Search: Found % customers', [Length(result)]);
  finally
    ObjArrayClear(customers);
  end;
end;


end.
