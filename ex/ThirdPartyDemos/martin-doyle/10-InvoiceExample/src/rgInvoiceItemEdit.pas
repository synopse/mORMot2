{:
---------------------------------------------------(C) martindoyle 2017-2026 --
 Project : Rechnung

 Using mORMot2
     Synopse mORMot2 framework. Copyright (C) 2025 Arnaud Bouchez
     Synopse Informatique - http://synopse.info

  Module : rgInvoiceItemEdit.pas

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
unit rgInvoiceItemEdit;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin, Menus, MdForms, mdLayout, rgDtoTypes;

type

  { TInvoiceItemEditForm }

  TInvoiceItemEditForm = class(TMDDBModeForm)
    LabelDescription: TLabel;
    EditDescription: TEdit;
    LabelQuantity: TLabel;
    EditQuantity: TEdit;
    LabelPrice: TLabel;
    EditPrice: TEdit;
    LabelDiscount: TLabel;
    SpinDiscount: TSpinEdit;
    LabelPercent: TLabel;
    OKButton: TButton;
    CancelButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure EditQuantityKeyPress(Sender: TObject; var Key: char);
    procedure EditPriceKeyPress(Sender: TObject; var Key: char);
  private
    FItem: TDtoInvoiceItem;
    FIsNew: Boolean;
    FSaveSuccessful: Boolean;
    procedure SetupLayout;
    function ValidateInput: Boolean;
  public
    function GetFormMenu: TMainMenu; override;
    function ShowItemEdit(var AItem: TDtoInvoiceItem; AIsNew: Boolean): Boolean;
  end;

var
  InvoiceItemEditForm: TInvoiceItemEditForm;

implementation

uses
  mormot.core.base,
  mormot.core.text,
  mormot.core.unicode,
  mdNumbers;

{$R *.dfm}

{ TInvoiceItemEditForm }

procedure TInvoiceItemEditForm.FormCreate(Sender: TObject);
begin
  FSaveSuccessful := False;
  SetupLayout;
end;

procedure TInvoiceItemEditForm.SetupLayout;
var
  Layout: TLayoutHelper;
  Margins: TLayoutMargins;
  LabelWidth, EditWidth: Integer;
  BaseHeight: Integer;
begin
  BaseHeight := LabelDescription.Height;
  Margins := LayoutMargins(BaseHeight);
  Layout := TLayoutHelper.Create(Self, Margins);
  try
    Layout.AdjustForPlatform;

    LabelWidth := Round(7 * BaseHeight);
    EditWidth := Round(14 * BaseHeight);

    LabelDescription.Width := LabelWidth;
    LabelQuantity.Width := LabelWidth;
    LabelPrice.Width := LabelWidth;
    LabelDiscount.Width := LabelWidth;

    EditDescription.Width := EditWidth;
    EditQuantity.Width := Round(6 * BaseHeight);
    EditPrice.Width := Round(8 * BaseHeight);
    SpinDiscount.Width := Round(4 * BaseHeight);

    LabelDescription.SetBounds(Margins.Left, Margins.Top,
      LabelWidth, LabelDescription.Height);

    Layout.Place(LabelDescription, EditDescription, ldRight, 0.5);

    Layout.Place(LabelDescription, LabelQuantity, ldBelow, 1.0);
    Layout.Place(LabelQuantity, EditQuantity, ldRight, 0.5);

    Layout.Place(LabelQuantity, LabelPrice, ldBelow, 1.0);
    Layout.Place(LabelPrice, EditPrice, ldRight, 0.5);

    Layout.Place(LabelPrice, LabelDiscount, ldBelow, 1.0);
    Layout.Place(LabelDiscount, SpinDiscount, ldRight, 0.5);
    Layout.Place(SpinDiscount, LabelPercent, ldRight, 0.25);

    OKButton.Top := SpinDiscount.Top + SpinDiscount.Height + (2 * BaseHeight);
    CancelButton.Top := OKButton.Top;

    CancelButton.Left := EditDescription.Left + EditDescription.Width - CancelButton.Width;
    OKButton.Left := CancelButton.Left - Margins.Middle - OKButton.Width;

    ClientHeight := CancelButton.Top + CancelButton.Height + Margins.Bottom;
    ClientWidth := EditDescription.Left + EditDescription.Width + Margins.Right;

    Position := poMainFormCenter;
  finally
    Layout.Free;
  end;
end;

procedure TInvoiceItemEditForm.EditQuantityKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'..'9', ',', '.', '-', #8, #13]) then
    Key := #0;
end;

procedure TInvoiceItemEditForm.EditPriceKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'..'9', ',', '.', '-', #8, #13]) then
    Key := #0;
end;

function TInvoiceItemEditForm.ValidateInput: Boolean;
var
  TempQty: Double;
  TempPrice: Currency;
begin
  Result := False;

  if Trim(EditDescription.Text) = '' then
  begin
    ShowMessage('Description is required.');
    EditDescription.SetFocus;
    Exit;
  end;

  if not TryStrToFloat(EditQuantity.Text, TempQty) then
  begin
    ShowMessage('Please enter a valid quantity.');
    EditQuantity.SetFocus;
    Exit;
  end;

  if not TryStrToCurr(EditPrice.Text, TempPrice) then
  begin
    ShowMessage('Please enter a valid price.');
    EditPrice.SetFocus;
    Exit;
  end;

  FItem.Description := StringToUtf8(Trim(EditDescription.Text));
  FItem.Quantity := TempQty;
  FItem.ListPrice := TempPrice;
  FItem.Discount := SpinDiscount.Value;

  Result := True;
end;

procedure TInvoiceItemEditForm.OKButtonClick(Sender: TObject);
begin
  if ValidateInput then
  begin
    FSaveSuccessful := True;
    ModalResult := mrOk;
  end;
end;

procedure TInvoiceItemEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

function TInvoiceItemEditForm.ShowItemEdit(var AItem: TDtoInvoiceItem;
  AIsNew: Boolean): Boolean;
begin
  FItem := AItem;
  FIsNew := AIsNew;
  FSaveSuccessful := False;

  if AIsNew then
    Caption := 'New Invoice Item'
  else
    Caption := 'Edit Invoice Item';

  EditDescription.Text := Utf8ToString(AItem.Description);
  EditQuantity.Text := FormatFloat(FMT_QTY_EDIT, AItem.Quantity);
  EditPrice.Text :=  FormatCurr(FMT_CURR_EDIT, AItem.ListPrice);
  SpinDiscount.Value := AItem.Discount;

  ActiveControl := EditDescription;

  Result := (ShowModal = mrOk) and FSaveSuccessful;

  if Result then
    AItem := FItem;
end;

function TInvoiceItemEditForm.GetFormMenu: TMainMenu;
begin
  Result := nil;
end;

end.
