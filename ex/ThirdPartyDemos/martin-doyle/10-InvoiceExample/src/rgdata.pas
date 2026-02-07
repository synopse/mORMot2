{:
———————————————————————————————————————————————— © martindoyle 2017-2025 ——
 Project : Rechnung

 Using mORMot2
     Synopse mORMot2 framework. Copyright (C) 2025 Arnaud Bouchez
     Synopse Informatique - http://synopse.info

  Module : rgData.pas

  Last modified
    Date : 15.09.2024
    Author : Martin Doyle
    Email : martin-doyle@online.de

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to
    deal in the Software without restriction, including without limitation the
    rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
    sell copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in
    all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
    FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
    IN THE SOFTWARE.
————————————————————————————————————————————————————————————————————————————
}
unit rgData;

interface

{$I mormot.defines.inc}

uses
  Classes,
  mormot.core.base,
  mormot.core.Data,
  mormot.core.json,
  mormot.core.os,
  mormot.core.rtti,
  mormot.db.core,
  mormot.orm.base,
  mormot.orm.core;

type
  TAddressItem = class(TCollectionItem)
  private
    FCity: RawUtf8;
    FCityArea: RawUtf8;
    FCode: RawUtf8;
    FCountry: RawUtf8;
    FRegion: RawUtf8;
    FStreet1: RawUtf8;
    FStreet2: RawUtf8;
  public
    procedure Clear;
  published
    property Street1: RawUtf8 read FStreet1 write FStreet1;
    property Street2: RawUtf8 read FStreet2 write FStreet2;
    property Code: RawUtf8 read FCode write FCode;
    property City: RawUtf8 read FCity write FCity;
    property CityArea: RawUtf8 read FCityArea write FCityArea;
    property Region: RawUtf8 read FRegion write FRegion;
    property Country: RawUtf8 read FCountry write FCountry;
  end;

  TAddressCollection = class(TCollection)
  protected
    function GetItem(Index: integer): TAddressItem;
    procedure SetItem(Index: integer; Value: TAddressItem);
  public
    property Items[Index: integer]: TAddressItem read GetItem write SetItem; default;
  end;

  TPersonItem = class(TCollectionItem)
  private
    FFirstName: RawUtf8;
    FMiddleName: RawUtf8;
    FLastName: RawUtf8;
    FPhones: TRawUtf8List;
    FEmails: TRawUtf8List;
    FAddresses: TAddressCollection;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Clear;
  published
    property FirstName: RawUtf8 read FFirstName write FFirstName;
    property MiddleName: RawUtf8 read FMiddleName write FMiddleName;
    property LastName: RawUtf8 read FLastName write FLastName;
    property Phones: TRawUtf8List read FPhones write FPhones;
    property Emails: TRawUtf8List read FEmails write FEmails;
    property Addresses: TAddressCollection read FAddresses write FAddresses;
  end;

  TPersonCollection = class(TCollection)
  protected
    function GetItem(Index: integer): TPersonItem;
    procedure SetItem(Index: integer; Value: TPersonItem);
  public
    property Items[Index: integer]: TPersonItem read GetItem write SetItem; default;
  end;

  TItem = class(TCollectionItem)
  private
    FDescription: RawUtf8;
    FDiscount: longint;
    FListPrice: currency;
    FPartNo: RawUtf8;
    FPosition: longint;
    FQuantity: double;
  published
    property PartNo: RawUtf8 read FPartNo write FPartNo;
    property Description: RawUtf8 read FDescription write FDescription;
    property ListPrice: currency read FListPrice write FListPrice;
    property Quantity: double read FQuantity write FQuantity;
    property Discount: longint read FDiscount write FDiscount;
    property Position: longint read FPosition write FPosition;
  end;

  TItemCollection = class(TCollection)
  protected
    function GetItem(Index: integer): TItem;
    procedure SetItem(Index: integer; Value: TItem);
  public
    property Items[Index: integer]: TItem read GetItem write SetItem; default;
  end;

  TOrmBase = class(TOrm)
  private
    FCreated: TCreateTime;
    FCreatedBy: RawUtf8;
    FModified: TModTime;
    FModifiedBy: RawUtf8;
    FVersion: TRecordVersion;
  published
    property Created: TCreateTime read FCreated write FCreated;
    property CreatedBy: RawUtf8 read FCreatedBy write FCreatedBy;
    property Modified: TModTime read FModified write FModified;
    property ModifiedBy: RawUtf8 read FModifiedBy write FModifiedBy;
    property Version: TRecordVersion read FVersion write FVersion;
  end;

  TOrmEmployee = class(TOrmBase)
  private
    FContact: TPersonItem;
    FEmployeeNo: RawUtf8;
    FHireDate: TTimeLog;
    FSalary: currency;
  protected
    class procedure InternalDefineModel(Props: TOrmProperties); override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
  published
    property EmployeeNo: RawUtf8 read FEmployeeNo write FEmployeeNo stored AS_UNIQUE;
    property Contact: TPersonItem read FContact write FContact;
    property HireDate: TTimeLog read FHireDate write FHireDate;
    property Salary: currency read FSalary write FSalary;
  end;


  TOrmCustomer = class(TOrmBase)
  private
    FCompany: RawUtf8;
    FContacts: TPersonCollection;
    FCustomerNo: RawUtf8;
  protected
    class procedure InternalDefineModel(Props: TOrmProperties); override;
    procedure InternalCreate; override;
  public
    destructor Destroy; override;
  published
    property CustomerNo: RawUtf8 read FCustomerNo write FCustomerNo stored AS_UNIQUE;
    property Company: RawUtf8 read FCompany write FCompany;
    property Contacts: TPersonCollection read FContacts write FContacts;
  end;

  TOrmCustomerOrder = class(TOrmBase)
  private
    FCustomer: TOrmCustomer;
    FItems: TItemCollection;
    FItemsTotal: currency;
    FOrderNo: RawUtf8;
    FSaleDate: TTimeLog;
    FShipDate: TTimeLog;
    FSeller: TOrmEmployee;
    FShipAddress: TAddressItem;
    FShipContact: TPersonItem;
    FAmountPaid: currency;
  protected
    class procedure InternalDefineModel(Props: TOrmProperties); override;
    procedure InternalCreate; override;
  public
    destructor Destroy; override;
    function CalculateTotal: currency;
  published
    property OrderNo: RawUtf8 read FOrderNo write FOrderNo stored AS_UNIQUE;
    property SaleDate: TTimeLog read FSaleDate write FSaleDate;
    property ShipDate: TTimeLog read FShipDate write FShipDate;
    property Customer: TOrmCustomer read FCustomer write FCustomer;
    property Seller: TOrmEmployee read FSeller write FSeller;
    property Items: TItemCollection read FItems write FItems;
    property ItemsTotal: currency read FItemsTotal write FItemsTotal;
    property ShipAddress: TAddressItem read FShipAddress write FShipAddress;
    property ShipContact: TPersonItem read FShipContact write FShipContact;
    property AmountPaid: currency read FAmountPaid write FAmountPaid;
  end;


function CreateModel: TOrmModel;

implementation

function CreateModel: TOrmModel;
begin
  Result := TOrmModel.Create([TOrmEmployee, TOrmCustomer, TOrmCustomerOrder]);
end;

{
********************************* TAddressItem *********************************
}
procedure TAddressItem.Clear;
begin
  FCity := '';
  FCityArea := '';
  FCode := '';
  FCountry := '';
  FRegion := '';
  FStreet1 := '';
  FStreet2 := '';
end;

{
****************************** TAddressCollection ******************************
}
function TAddressCollection.GetItem(Index: integer): TAddressItem;
begin
  Result := TAddressItem(inherited GetItem(Index));
end;

procedure TAddressCollection.SetItem(Index: integer; Value: TAddressItem);
begin
  inherited SetItem(Index, Value);
end;
{
********************************* TPersonItem **********************************
}
constructor TPersonItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FAddresses := TAddressCollection.Create(TAddressItem);
  FPhones := TRawUtf8List.Create;
  FEmails := TRawUtf8List.Create;
end;

destructor TPersonItem.Destroy;
begin
  FAddresses.Free;
  FPhones.Free;
  FEmails.Free;
  inherited Destroy;
end;

procedure TPersonItem.Clear;
begin
  FFirstName := '';
  FMiddleName := '';
  FLastName := '';
  FPhones.Clear;
  FEmails.Clear;
  FAddresses.Clear;
end;

{
****************************** TPersonCollection *******************************
}
function TPersonCollection.GetItem(Index: integer): TPersonItem;
begin
  Result := TPersonItem(inherited GetItem(Index));
end;

procedure TPersonCollection.SetItem(Index: integer; Value: TPersonItem);
begin
  inherited SetItem(Index, Value);
end;

{
******************************* TItemCollection ********************************
}
function TItemCollection.GetItem(Index: integer): TItem;
begin
  Result := TItem(inherited GetItem(Index));
end;

procedure TItemCollection.SetItem(Index: integer; Value: TItem);
begin
  inherited SetItem(Index, Value);
end;

{
********************************* TOrmEmployee *********************************
}
constructor TOrmEmployee.Create;
begin
  inherited Create;
  FContact := TPersonItem.Create(nil);
end;

destructor TOrmEmployee.Destroy;
begin
  FContact.Free;
  inherited Destroy;
end;


{
********************************* TOrmCustomer *********************************
}
procedure TOrmCustomer.InternalCreate;
begin
  inherited InternalCreate;
  FContacts := TPersonCollection.Create(TPersonItem);
end;

destructor TOrmCustomer.Destroy;
begin
  FContacts.Free;
  inherited Destroy;
end;

class procedure TOrmCustomer.InternalDefineModel(Props: TOrmProperties);
begin
  inherited InternalDefineModel(Props);
  AddFilterNotVoidText(['Company']);
end;

{
******************************** TOrmEmployee *********************************
}
class procedure TOrmEmployee.InternalDefineModel(Props: TOrmProperties);
begin
  inherited InternalDefineModel(Props);
  AddFilterNotVoidText(['EmployeeNo']);
end;

{
******************************** TOrmCustomerOrder *********************************
}
procedure TOrmCustomerOrder.InternalCreate;
begin
  inherited InternalCreate;
  FItems := TItemCollection.Create(TItem);
  FShipContact := TPersonItem.Create(nil);
  FShipAddress := TAddressItem.Create(nil);
end;

destructor TOrmCustomerOrder.Destroy;
begin
  FShipAddress.Free;
  FShipContact.Free;
  FItems.Free;
  inherited Destroy;
end;

class procedure TOrmCustomerOrder.InternalDefineModel(Props: TOrmProperties);
begin
  inherited InternalDefineModel(Props);
  AddFilterNotVoidText(['OrderNo']);
end;

function TOrmCustomerOrder.CalculateTotal: currency;
var
  i: integer;
  ItemTotal: currency;
begin
  Result := 0;
  if Assigned(FItems) then
    for i := 0 to FItems.Count - 1 do
    begin
      ItemTotal := FItems[i].ListPrice * FItems[i].Quantity;
      ItemTotal := ItemTotal * (100 - FItems[i].Discount) / 100;
      Result := Result + ItemTotal;
    end;
end;


initialization
  Rtti.RegisterCollection(TAddressCollection, TAddressItem);
  Rtti.RegisterCollection(TPersonCollection, TPersonItem);
  Rtti.RegisterCollection(TItemCollection, TItem);
end.
