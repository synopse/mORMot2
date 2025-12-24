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
  mormot.core.unicode,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.soa.core,
  api.interfaces,
  entities;

type
  /// Customers CRUD API implementation
  // Ports DMVC TCustomersController and business logic from TCustomer to mORMot2
  TCustomersApi = class(TInterfacedObject, ICustomersApi)
  protected
    fRest: IRestOrm;
    /// Validates customer data (code and description required, length limits)
    procedure ValidateCustomer(const customer: TCustomerDto);
    /// Converts TOrmCustomer to TCustomerDto
    function OrmToDto(const orm: TOrmCustomer): TCustomerDto;
    /// Converts TCustomerDto to TOrmCustomer
    procedure DtoToOrm(const dto: TCustomerDto; orm: TOrmCustomer);
  public
    constructor Create(const aRest: IRestOrm); reintroduce;
    // ICustomersApi methods
    function GetAll: TCustomerDtos;
    function GetById(id: TID): TCustomerDto;
    function CreateCustomer(const customer: TCustomerDto): TID;
    procedure Update(id: TID; const customer: TCustomerDto);
    procedure Delete(id: TID);
  end;


implementation


{ TCustomersApi }

constructor TCustomersApi.Create(const aRest: IRestOrm);
begin
  inherited Create;
  fRest := aRest;
end;

procedure TCustomersApi.ValidateCustomer(const customer: TCustomerDto);
var
  errors: TRawUtf8DynArray;
begin
  // Port of TCustomer.OnBeforeInsertOrUpdate validation
  errors := nil;

  if Length(customer.Code) > 15 then
    AddRawUtf8(errors, 'Code too long (max length 15)');

  if Length(customer.Description) > 50 then
    AddRawUtf8(errors, 'Description too long (max length 50)');

  if Length(customer.Description) = 0 then
    AddRawUtf8(errors, 'Description is required');

  if Length(customer.Code) = 0 then
    AddRawUtf8(errors, 'Code is required');

  if Length(errors) > 0 then
    raise EServiceException.CreateUtf8('Validation errors: %', [RawUtf8ArrayToCsv(errors)]);
end;

function TCustomersApi.OrmToDto(const orm: TOrmCustomer): TCustomerDto;
begin
  result.ID := orm.ID;
  result.Code := orm.Code;
  result.Description := orm.Description;
  result.City := orm.City;
  result.Note := orm.Note;
  result.Rating := orm.Rating;
end;

procedure TCustomersApi.DtoToOrm(const dto: TCustomerDto; orm: TOrmCustomer);
begin
  orm.Code := dto.Code;
  orm.Description := dto.Description;
  orm.City := dto.City;
  orm.Note := dto.Note;
  orm.Rating := dto.Rating;
  // Don't set ID - handled by ORM
end;

function TCustomersApi.GetAll: TCustomerDtos;
var
  customers: TOrmCustomers;
  i: PtrInt;
begin
  // Port of TCustomersController.GetCustomers
  // Original: TMVCActiveRecord.SelectRQL<TCustomer>('sort(+id)', 200)
  TSynLog.Add.Log(sllInfo, 'GetAll: Fetching all customers');

  fRest.RetrieveListObjArray(customers, TOrmCustomer, '', []);
  try
    SetLength(result, Length(customers));
    for i := 0 to High(customers) do
      result[i] := OrmToDto(customers[i]);

    TSynLog.Add.Log(sllInfo, 'GetAll: Returned % customers', [Length(result)]);
  finally
    ObjArrayClear(customers);
  end;
end;

function TCustomersApi.GetById(id: TID): TCustomerDto;
var
  customer: TOrmCustomer;
begin
  // Port of TCustomersController.GetCustomer
  // Original: TMVCActiveRecord.GetByPK<TCustomer>(id)
  TSynLog.Add.Log(sllInfo, 'GetById: id=%', [id]);

  customer := TOrmCustomer.Create(fRest, id);
  try
    if customer.ID = 0 then
      raise EServiceException.CreateUtf8('Customer % not found', [id]);

    result := OrmToDto(customer);
    TSynLog.Add.Log(sllInfo, 'GetById: Found customer code=%', [result.Code]);
  finally
    customer.Free;
  end;
end;

function TCustomersApi.CreateCustomer(const customer: TCustomerDto): TID;
var
  orm: TOrmCustomer;
begin
  // Port of TCustomersController.CreateCustomer
  // Original: lCustomer.Insert
  TSynLog.Add.Log(sllInfo, 'Create: code=%, description=%',
    [customer.Code, customer.Description]);

  // Validate customer data
  ValidateCustomer(customer);

  orm := TOrmCustomer.Create;
  try
    DtoToOrm(customer, orm);

    result := fRest.Add(orm, true);
    if result = 0 then
      raise EServiceException.Create('Failed to create customer');

    TSynLog.Add.Log(sllInfo, 'Create: Created customer id=%', [result]);
  finally
    orm.Free;
  end;
end;

procedure TCustomersApi.Update(id: TID; const customer: TCustomerDto);
var
  orm: TOrmCustomer;
begin
  // Port of TCustomersController.UpdateCustomer
  // Original: lCustomer.Update
  TSynLog.Add.Log(sllInfo, 'Update: id=%, code=%, description=%',
    [id, customer.Code, customer.Description]);

  // Validate customer data
  ValidateCustomer(customer);

  orm := TOrmCustomer.Create(fRest, id);
  try
    if orm.ID = 0 then
      raise EServiceException.CreateUtf8('Customer % not found', [id]);

    DtoToOrm(customer, orm);

    if not fRest.Update(orm) then
      raise EServiceException.CreateUtf8('Failed to update customer %', [id]);

    TSynLog.Add.Log(sllInfo, 'Update: Updated customer id=%', [id]);
  finally
    orm.Free;
  end;
end;

procedure TCustomersApi.Delete(id: TID);
var
  customer: TOrmCustomer;
begin
  // Port of TCustomersController.DeleteCustomer
  // Original: lCustomer.Delete
  TSynLog.Add.Log(sllInfo, 'Delete: id=%', [id]);

  customer := TOrmCustomer.Create(fRest, id);
  try
    if customer.ID = 0 then
      raise EServiceException.CreateUtf8('Customer % not found', [id]);

    if not fRest.Delete(TOrmCustomer, id) then
      raise EServiceException.CreateUtf8('Failed to delete customer %', [id]);

    TSynLog.Add.Log(sllInfo, 'Delete: Deleted customer id=%', [id]);
  finally
    customer.Free;
  end;
end;


end.
