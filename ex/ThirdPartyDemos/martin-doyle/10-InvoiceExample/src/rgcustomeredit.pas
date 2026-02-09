{:
———————————————————————————————————————————————— (C) martindoyle 2017-2026 ——
 Project : Rechnung

 Using mORMot2
     Synopse mORMot2 framework. Copyright (C) 2025 Arnaud Bouchez
     Synopse Informatique - http://synopse.info

  Module : rgCustomerEdit.pas

  Last modified
    Date : 28.01.2026
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
  mormot.core.base;

{$R *.dfm}

{ TCustomerEditForm }

procedure TCustomerEditForm.FormCreate(Sender: TObject);
begin
  FCustomerID := 0;
  FormMode := fmBrowse;
  SetupLayout;
end;

procedure TCustomerEditForm.SetupLayout;
var
  Layout: TLayoutHelper;
  Margins: TLayoutMargins;
  LabelWidth, EditWidth: Integer;
  BaseHeight: Integer;
begin
  // Use label height as base unit for all calculations
  BaseHeight := LabelCustomerNo.Height;

  // Calculate margins based on base height
  Margins := LayoutMargins(BaseHeight);
  Layout := TLayoutHelper.Create(Self, Margins);
  try
    Layout.AdjustForPlatform;

    // Calculate widths based on base height (proportional sizing)
    LabelWidth := Round(6.25 * BaseHeight);  // ~100px at 16px height
    EditWidth := Round(18.75 * BaseHeight);  // ~300px at 16px height - uniform width

    // Set label widths for alignment
    LabelCustomerNo.Width := LabelWidth;
    LabelCompany.Width := LabelWidth;
    LabelPhone.Width := LabelWidth;
    LabelFax.Width := LabelWidth;
    LabelAddress.Width := LabelWidth;
    LabelZip.Width := LabelWidth;
    LabelCity.Width := LabelWidth;
    LabelCountry.Width := LabelWidth;

    // Set all edit widths the same for consistency
    EditCustomerNo.Width := EditWidth;
    EditCompany.Width := EditWidth;
    EditPhone.Width := EditWidth;
    EditFax.Width := EditWidth;
    EditAddress.Width := EditWidth;
    EditZip.Width := EditWidth;
    EditCity.Width := EditWidth;
    EditCountry.Width := EditWidth;

    // Position first label
    LabelCustomerNo.SetBounds(Margins.Left, Margins.Top,
      LabelWidth, LabelCustomerNo.Height);

    // Position first edit next to label with proportional spacing
    Layout.Place(LabelCustomerNo, EditCustomerNo, ldRight, 1.0);

    // Position remaining labels and edits in column with increased vertical spacing
    Layout.Place(LabelCustomerNo, LabelCompany, ldBelow, 1.0);
    Layout.Place(LabelCompany, EditCompany, ldRight, 1.0);

    Layout.Place(LabelCompany, LabelPhone, ldBelow, 1.0);
    Layout.Place(LabelPhone, EditPhone, ldRight, 1.0);

    Layout.Place(LabelPhone, LabelFax, ldBelow, 1.0);
    Layout.Place(LabelFax, EditFax, ldRight, 1.0);

    Layout.Place(LabelFax, LabelAddress, ldBelow, 1.0);
    Layout.Place(LabelAddress, EditAddress, ldRight, 1.0);

    Layout.Place(LabelAddress, LabelZip, ldBelow, 1.0);
    Layout.Place(LabelZip, EditZip, ldRight, 1.0);

    Layout.Place(LabelZip, LabelCity, ldBelow, 1.0);
    Layout.Place(LabelCity, EditCity, ldRight, 1.0);

    Layout.Place(LabelCity, LabelCountry, ldBelow, 1.0);
    Layout.Place(LabelCountry, EditCountry, ldRight, 1.0);

    // Auto-size the form based on all content except buttons
    Layout.AutoSizeForm;

    // Position buttons below last edit control with section spacing (2x base height)
    SaveButton.Top := EditCountry.Top + EditCountry.Height + (2 * BaseHeight);
    CancelButton.Top := SaveButton.Top;

    // Position buttons horizontally at bottom-right with proper margins
    CancelButton.Left := ClientWidth - Margins.Right - CancelButton.Width;
    SaveButton.Left := CancelButton.Left - Margins.Middle - SaveButton.Width;

    // Adjust form height to include buttons
    ClientHeight := CancelButton.Top + CancelButton.Height + Margins.Bottom;

    Position := poMainFormCenter;
  finally
    Layout.Free;
  end;
end;

procedure TCustomerEditForm.FormDestroy(Sender: TObject);
begin
  // nothing to free - services accessed via RgServices global
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
      EditCustomerNo.Text := Customer.CustomerNo;
      EditCompany.Text := Customer.Company;
      EditPhone.Text := Customer.Phone;
      EditFax.Text := Customer.Fax;
      EditAddress.Text := Customer.Address;
      EditZip.Text := Customer.Zip;
      EditCity.Text := Customer.City;
      EditCountry.Text := Customer.Country;
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
  Customer.CustomerNo := Trim(EditCustomerNo.Text);
  Customer.Company := Trim(EditCompany.Text);
  Customer.Phone := Trim(EditPhone.Text);
  Customer.Fax := Trim(EditFax.Text);
  Customer.Address := Trim(EditAddress.Text);
  Customer.Zip := Trim(EditZip.Text);
  Customer.City := Trim(EditCity.Text);
  Customer.Country := Trim(EditCountry.Text);

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
