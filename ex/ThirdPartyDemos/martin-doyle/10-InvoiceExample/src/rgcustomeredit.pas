{:
---------------------------------------------------(C) martindoyle 2017-2026 --
 Project : Rechnung

 Using mORMot2
     Synopse mORMot2 framework. Copyright (C) 2025 Arnaud Bouchez
     Synopse Informatique - http://synopse.info

  Module : rgCustomerEdit.pas

  Last modified
    Date : 09.02.2026
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
--------------------------------------------------------------------------------
}
unit rgCustomerEdit;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, MdForms, mdLayout, rgClient, rgDtoTypes;

type

  { TCustomerEditForm }

  TCustomerEditForm = class(TMDDBModeForm)
    LabelCustomerNo: TLabel;
    LabelCompany: TLabel;
    LabelPhone: TLabel;
    LabelFax: TLabel;
    LabelAddress: TLabel;
    LabelZip: TLabel;
    LabelCity: TLabel;
    LabelCountry: TLabel;
    EditCustomerNo: TEdit;
    EditCompany: TEdit;
    EditPhone: TEdit;
    EditFax: TEdit;
    EditAddress: TEdit;
    EditZip: TEdit;
    EditCity: TEdit;
    EditCountry: TEdit;
    SaveButton: TButton;
    CancelButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    FCustomerID: longint;
    FOriginalCustomerNo: string;
    FOriginalCompany: string;
    FOriginalPhone: string;
    FOriginalFax: string;
    FOriginalAddress: string;
    FOriginalZip: string;
    FOriginalCity: string;
    FOriginalCountry: string;
    procedure LoadCustomerData;
    procedure ClearFields;
    procedure SetFieldsEnabled(AEnabled: Boolean);
    procedure SetupLayout;
    procedure StoreOriginalValues;
    function HasChanges: Boolean;
  protected
    procedure SetFormMode(AValue: TFormMode); override;
  public
    function GetFormMenu: TMainMenu; override;
    procedure ShowCustomer(ACustomerID: longint);
    procedure NewCustomer;
  end;

var
  CustomerEditForm: TCustomerEditForm;

implementation

uses
  mormot.core.base,
  mormot.core.unicode;

{$R *.dfm}

{ TCustomerEditForm }

procedure TCustomerEditForm.FormCreate(Sender: TObject);
begin
  FCustomerID := 0;
  FormMode := fmBrowse;
end;

procedure TCustomerEditForm.SetupLayout;
begin
  InitLayout(LabelCustomerNo.Height, EditCustomerNo.Height,
    7.0 * LabelCustomerNo.Height, 18.75 * LabelCustomerNo.Height);

  // Prepare label-edit pairs: AutoSize off, uniform sizes, text centered, FocusControl
  PrepareLabelEdit(LabelCustomerNo, EditCustomerNo);
  PrepareLabelEdit(LabelCompany, EditCompany);
  PrepareLabelEdit(LabelPhone, EditPhone);
  PrepareLabelEdit(LabelFax, EditFax);
  PrepareLabelEdit(LabelAddress, EditAddress);
  PrepareLabelEdit(LabelZip, EditZip);
  PrepareLabelEdit(LabelCity, EditCity);
  PrepareLabelEdit(LabelCountry, EditCountry);

  // Position first label
  LabelCustomerNo.SetBounds(Layout.Margins.Left, Layout.Margins.Top,
    LabelWidth, EditHeight);

  // Position first edit next to label
  Layout.PlaceRight(LabelCustomerNo, EditCustomerNo, 1.0);

  // Position remaining label-edit pairs in column
  Layout.PlaceBelow(LabelCustomerNo, LabelCompany, 0.5);
  Layout.PlaceRight(LabelCompany, EditCompany, 1.0);

  Layout.PlaceBelow(LabelCompany, LabelPhone, 0.5);
  Layout.PlaceRight(LabelPhone, EditPhone, 1.0);

  Layout.PlaceBelow(LabelPhone, LabelFax, 0.5);
  Layout.PlaceRight(LabelFax, EditFax, 1.0);

  Layout.PlaceBelow(LabelFax, LabelAddress, 0.5);
  Layout.PlaceRight(LabelAddress, EditAddress, 1.0);

  Layout.PlaceBelow(LabelAddress, LabelZip, 0.5);
  Layout.PlaceRight(LabelZip, EditZip, 1.0);

  Layout.PlaceBelow(LabelZip, LabelCity, 0.5);
  Layout.PlaceRight(LabelCity, EditCity, 1.0);

  Layout.PlaceBelow(LabelCity, LabelCountry, 0.5);
  Layout.PlaceRight(LabelCountry, EditCountry, 1.0);

  // Place buttons below last edit, right-aligned to edit's right edge
  Layout.PlaceBelowRight(EditCountry, CancelButton, 1.0);
  Layout.PlaceLeft(CancelButton, SaveButton, 0.5);

  Layout.AutoSizeForm;
  Position := poDesktopCenter;
end;

procedure TCustomerEditForm.FormDestroy(Sender: TObject);
begin
  // nothing to free - services accessed via RgServices global
end;

procedure TCustomerEditForm.FormShow(Sender: TObject);
begin
  SetupLayout;
end;

procedure TCustomerEditForm.SetFormMode(AValue: TFormMode);
begin
  inherited SetFormMode(AValue);

  case AValue of
    fmBrowse:
    begin
      SetFieldsEnabled(False);
      SaveButton.Enabled := False;
      CancelButton.Caption := '&Close';
    end;
    fmInsert, fmEdit:
    begin
      SetFieldsEnabled(True);
      SaveButton.Enabled := True;
      CancelButton.Caption := '&Cancel';
    end;
  end;
end;

procedure TCustomerEditForm.ClearFields;
begin
  EditCustomerNo.Text := '';
  EditCompany.Text := '';
  EditPhone.Text := '';
  EditFax.Text := '';
  EditAddress.Text := '';
  EditZip.Text := '';
  EditCity.Text := '';
  EditCountry.Text := '';
end;

procedure TCustomerEditForm.StoreOriginalValues;
begin
  FOriginalCustomerNo := EditCustomerNo.Text;
  FOriginalCompany := EditCompany.Text;
  FOriginalPhone := EditPhone.Text;
  FOriginalFax := EditFax.Text;
  FOriginalAddress := EditAddress.Text;
  FOriginalZip := EditZip.Text;
  FOriginalCity := EditCity.Text;
  FOriginalCountry := EditCountry.Text;
end;

function TCustomerEditForm.HasChanges: Boolean;
begin
  Result := (EditCustomerNo.Text <> FOriginalCustomerNo) or
            (EditCompany.Text <> FOriginalCompany) or
            (EditPhone.Text <> FOriginalPhone) or
            (EditFax.Text <> FOriginalFax) or
            (EditAddress.Text <> FOriginalAddress) or
            (EditZip.Text <> FOriginalZip) or
            (EditCity.Text <> FOriginalCity) or
            (EditCountry.Text <> FOriginalCountry);
end;

procedure TCustomerEditForm.SetFieldsEnabled(AEnabled: Boolean);
begin
  EditCustomerNo.Enabled := AEnabled;
  EditCompany.Enabled := AEnabled;
  EditPhone.Enabled := AEnabled;
  EditFax.Enabled := AEnabled;
  EditAddress.Enabled := AEnabled;
  EditZip.Enabled := AEnabled;
  EditCity.Enabled := AEnabled;
  EditCountry.Enabled := AEnabled;
end;

procedure TCustomerEditForm.LoadCustomerData;
var
  Customer: TDtoCustomer;
  Res: TCustomerEditResult;
begin
  if FCustomerID > 0 then
  begin
    Res := RgServices.CustomerService.GetCustomer(FCustomerID, Customer);
    if Res = cerSuccess then
    begin
      EditCustomerNo.Text := Utf8ToString(Customer.CustomerNo);
      EditCompany.Text := Utf8ToString(Customer.Company);
      EditPhone.Text := Utf8ToString(Customer.Phone);
      EditFax.Text := Utf8ToString(Customer.Fax);
      EditAddress.Text := Utf8ToString(Customer.Address);
      EditZip.Text := Utf8ToString(Customer.Zip);
      EditCity.Text := Utf8ToString(Customer.City);
      EditCountry.Text := Utf8ToString(Customer.Country);
    end
    else
    begin
      ShowMessage('Customer not found: ' + IntToStr(FCustomerID));
      ClearFields;
    end;
  end;
end;

procedure TCustomerEditForm.ShowCustomer(ACustomerID: longint);
begin
  FCustomerID := ACustomerID;
  ClearFields;
  LoadCustomerData;
  StoreOriginalValues;
  FormMode := fmEdit;
  ShowModal;
end;

procedure TCustomerEditForm.NewCustomer;
var
  CustomerNo: RawUtf8;
begin
  FCustomerID := 0;
  ClearFields;
  RgServices.CustomerService.GenerateCustomerNo(CustomerNo);
  EditCustomerNo.Text := Utf8ToString(CustomerNo);
  StoreOriginalValues;
  FormMode := fmInsert;
  ShowModal;
end;

procedure TCustomerEditForm.SaveButtonClick(Sender: TObject);
var
  Customer: TDtoCustomer;
  NewID: longint;
  Res: TCustomerEditResult;
begin
  if Trim(EditCustomerNo.Text) = '' then
  begin
    ShowMessage('Customer number is required.');
    EditCustomerNo.SetFocus;
    Exit;
  end;

  if Trim(EditCompany.Text) = '' then
  begin
    ShowMessage('Company name is required.');
    EditCompany.SetFocus;
    Exit;
  end;

  Finalize(Customer);
  FillChar(Customer, SizeOf(Customer), 0);
  Customer.CustomerID := FCustomerID;
  Customer.CustomerNo := StringToUtf8(Trim(EditCustomerNo.Text));
  Customer.Company := StringToUtf8(Trim(EditCompany.Text));
  Customer.Phone := StringToUtf8(Trim(EditPhone.Text));
  Customer.Fax := StringToUtf8(Trim(EditFax.Text));
  Customer.Address := StringToUtf8(Trim(EditAddress.Text));
  Customer.Zip := StringToUtf8(Trim(EditZip.Text));
  Customer.City := StringToUtf8(Trim(EditCity.Text));
  Customer.Country := StringToUtf8(Trim(EditCountry.Text));

  if FormMode = fmInsert then
    Res := RgServices.CustomerService.CreateCustomer(Customer, NewID)
  else
    Res := RgServices.CustomerService.UpdateCustomer(FCustomerID, Customer);

  case Res of
    cerSuccess:
      ModalResult := mrOk;
    cerMissingField:
      ShowMessage('Required fields are missing.');
    cerDatabaseError:
      ShowMessage('Database error. The customer could not be saved.');
  end;
end;

procedure TCustomerEditForm.CancelButtonClick(Sender: TObject);
begin
  if (FormMode in [fmInsert, fmEdit]) and HasChanges then
  begin
    if MessageDlg('Are you sure you want to cancel your changes?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      ModalResult := mrCancel;
  end
  else
    ModalResult := mrCancel;
end;

function TCustomerEditForm.GetFormMenu: TMainMenu;
begin
  Result := nil;
end;

end.
