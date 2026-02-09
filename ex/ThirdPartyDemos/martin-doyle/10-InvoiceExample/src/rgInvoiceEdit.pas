{:
———————————————————————————————————————————————— (C) martindoyle 2017-2026 ——
 Project : Rechnung

 Using mORMot2
     Synopse mORMot2 framework. Copyright (C) 2025 Arnaud Bouchez
     Synopse Informatique - http://synopse.info

  Module : rgInvoiceEdit.pas

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
————————————————————————————————————————————————————————————————————————————
}
unit rgInvoiceEdit;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, MdForms, mdLayout, mdGrids, rgDtoTypes,
  rgInvoiceItemEdit;

type

  { TInvoiceEditForm }

  TInvoiceEditForm = class(TMDDBModeForm)
    LabelCustomer: TLabel;
    LabelCustomerValue: TLabel;
    LabelOrderNo: TLabel;
    EditOrderNo: TEdit;
    LabelSaleDate: TLabel;
    EditSaleDate: TEdit;
    LabelShipDate: TLabel;
    EditShipDate: TEdit;
    ItemsToolbarPanel: TPanel;
    AddItemButton: TButton;
    EditItemButton: TButton;
    RemoveItemButton: TButton;
    LabelTotal: TLabel;
    LabelTotalValue: TLabel;
    SaveButton: TButton;
    CancelButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AddItemButtonClick(Sender: TObject);
    procedure EditItemButtonClick(Sender: TObject);
    procedure RemoveItemButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    FItems: TDtoInvoiceItemArray;
    FInvoiceID: longint;
    FCustomerID: longint;
    FIsNew: Boolean;
    FSaveSuccessful: Boolean;
    FItemsListGrid: TMDListGrid;
    procedure SetupLayout;
    procedure SetListGridColumns;
    procedure LoadItemsToListGrid;
    procedure UpdateButtons;
    procedure UpdateTotal;
    function ValidateInput: Boolean;
    procedure ItemsListGridSelectItem(Sender: TObject; Item: TMDListItem;
      Selected: Boolean);
    procedure ItemsListGridDblClick(Sender: TObject);
  public
    function GetFormMenu: TMainMenu; override;
    function ShowNewInvoice(ACustomerID: longint): Boolean;
    function ShowEditInvoice(AInvoiceID: longint): Boolean;
  end;

var
  InvoiceEditForm: TInvoiceEditForm;

implementation

uses
  mormot.core.base,
  mormot.core.text,
  mormot.core.unicode,
  mdDates,
  rgClient;

{$R *.dfm}

{ TInvoiceEditForm }

procedure TInvoiceEditForm.FormCreate(Sender: TObject);
begin
  FInvoiceID := 0;
  FCustomerID := 0;
  FIsNew := False;
  FSaveSuccessful := False;
  SetLength(FItems, 0);

  // Create TMDListGrid (replaces TListView to fix BUG-004)
  FItemsListGrid := TMDListGrid.Create(Self);
  FItemsListGrid.Parent := Self;
  FItemsListGrid.OnSelectItem := ItemsListGridSelectItem;
  FItemsListGrid.OnDblClick := ItemsListGridDblClick;

  SetListGridColumns;
  SetupLayout;
end;

procedure TInvoiceEditForm.SetupLayout;
var
  Layout: TLayoutHelper;
  Margins: TLayoutMargins;
  LabelWidth, EditWidth: Integer;
  BaseHeight: Integer;
begin
  BaseHeight := LabelCustomer.Height;
  Margins := LayoutMargins(BaseHeight);
  Layout := TLayoutHelper.Create(Self, Margins);
  try
    Layout.AdjustForPlatform;

    LabelWidth := Round(8 * BaseHeight);
    EditWidth := Round(12 * BaseHeight);

    LabelCustomer.Width := LabelWidth;
    LabelOrderNo.Width := LabelWidth;
    LabelSaleDate.Width := LabelWidth;
    LabelShipDate.Width := LabelWidth;

    EditOrderNo.Width := EditWidth;
    EditSaleDate.Width := Round(8 * BaseHeight);
    EditShipDate.Width := Round(8 * BaseHeight);

    LabelCustomer.SetBounds(Margins.Left, Margins.Top,
      LabelWidth, LabelCustomer.Height);

    Layout.Place(LabelCustomer, LabelCustomerValue, ldRight, 0.5);

    Layout.Place(LabelCustomer, LabelOrderNo, ldBelow, 1.0);
    Layout.Place(LabelOrderNo, EditOrderNo, ldRight, 0.5);

    Layout.Place(LabelOrderNo, LabelSaleDate, ldBelow, 1.0);
    Layout.Place(LabelSaleDate, EditSaleDate, ldRight, 0.5);

    Layout.Place(EditSaleDate, LabelShipDate, ldRight, 2.0);
    Layout.Place(LabelShipDate, EditShipDate, ldRight, 0.5);

    ItemsToolbarPanel.Top := EditSaleDate.Top + EditSaleDate.Height + (2 * BaseHeight);
    ItemsToolbarPanel.Left := Margins.Left;
    ItemsToolbarPanel.Width := ClientWidth - 2 * Margins.Left;
    ItemsToolbarPanel.Height := Round(2.5 * BaseHeight);

    AddItemButton.Left := 0;
    AddItemButton.Top := (ItemsToolbarPanel.Height - AddItemButton.Height) div 2;
    EditItemButton.Left := AddItemButton.Left + AddItemButton.Width + (BaseHeight div 2);
    EditItemButton.Top := AddItemButton.Top;
    RemoveItemButton.Left := EditItemButton.Left + EditItemButton.Width + (BaseHeight div 2);
    RemoveItemButton.Top := AddItemButton.Top;

    FItemsListGrid.Top := ItemsToolbarPanel.Top + ItemsToolbarPanel.Height + (BaseHeight div 2);
    FItemsListGrid.Left := Margins.Left;
    FItemsListGrid.Width := ClientWidth - 2 * Margins.Left;
    FItemsListGrid.Height := Round(12 * BaseHeight);

    LabelTotal.Top := FItemsListGrid.Top + FItemsListGrid.Height + BaseHeight;
    LabelTotalValue.Top := LabelTotal.Top;
    LabelTotalValue.Left := FItemsListGrid.Left + FItemsListGrid.Width - LabelTotalValue.Width;
    LabelTotal.Left := LabelTotalValue.Left - LabelTotal.Width - BaseHeight;

    SaveButton.Top := LabelTotal.Top + LabelTotal.Height + (2 * BaseHeight);
    CancelButton.Top := SaveButton.Top;

    CancelButton.Left := ClientWidth - Margins.Right - CancelButton.Width;
    SaveButton.Left := CancelButton.Left - Margins.Middle - SaveButton.Width;

    ClientHeight := CancelButton.Top + CancelButton.Height + Margins.Bottom;
    ClientWidth := FItemsListGrid.Left + FItemsListGrid.Width + Margins.Right;

    Position := poMainFormCenter;
  finally
    Layout.Free;
  end;
end;

procedure TInvoiceEditForm.SetListGridColumns;
var
  Col: TMDListColumn;
  BaseHeight: Integer;
begin
  BaseHeight := Canvas.TextHeight('Ag');
  if BaseHeight < 16 then
    BaseHeight := 16;

  FItemsListGrid.RowSelect := True;

  Col := FItemsListGrid.Columns.Add;
  Col.Caption := 'Pos';
  Col.Width := Round(3 * BaseHeight);
  Col.Alignment := taCenter;

  Col := FItemsListGrid.Columns.Add;
  Col.Caption := 'Description';
  Col.Width := Round(18 * BaseHeight);

  Col := FItemsListGrid.Columns.Add;
  Col.Caption := 'Qty';
  Col.Width := Round(4 * BaseHeight);
  Col.Alignment := taRightJustify;

  Col := FItemsListGrid.Columns.Add;
  Col.Caption := 'Price';
  Col.Width := Round(6 * BaseHeight);
  Col.Alignment := taRightJustify;

  Col := FItemsListGrid.Columns.Add;
  Col.Caption := 'Disc%';
  Col.Width := Round(4 * BaseHeight);
  Col.Alignment := taRightJustify;

  Col := FItemsListGrid.Columns.Add;
  Col.Caption := 'Amount';
  Col.Width := Round(7 * BaseHeight);
  Col.Alignment := taRightJustify;
end;

procedure TInvoiceEditForm.LoadItemsToListGrid;
var
  i: Integer;
  ListItem: TMDListItem;
begin
  FItemsListGrid.Items.BeginUpdate;
  try
    FItemsListGrid.Items.Clear;

    for i := 0 to Length(FItems) - 1 do
    begin
      ListItem := FItemsListGrid.Items.Add;
      ListItem.Caption := IntToStr(FItems[i].Position);
      ListItem.SubItems.Add(Utf8ToString(FItems[i].Description));
      ListItem.SubItems.Add(Format('%.2f', [FItems[i].Quantity]));
      ListItem.SubItems.Add(Curr64ToString(PInt64(@FItems[i].ListPrice)^));
      ListItem.SubItems.Add(IntToStr(FItems[i].Discount));
      ListItem.SubItems.Add(Curr64ToString(PInt64(@FItems[i].Amount)^));
      ListItem.Data := Pointer(PtrInt(i));
    end;
  finally
    FItemsListGrid.Items.EndUpdate;
  end;

  UpdateTotal;
  UpdateButtons;
end;

procedure TInvoiceEditForm.UpdateButtons;
var
  HasSelection: Boolean;
begin
  HasSelection := FItemsListGrid.Selected <> nil;
  EditItemButton.Enabled := HasSelection;
  RemoveItemButton.Enabled := HasSelection;
end;

procedure TInvoiceEditForm.UpdateTotal;
var
  Total: currency;
  i: Integer;
begin
  Total := 0;
  for i := 0 to Length(FItems) - 1 do
    Total := Total + FItems[i].Amount;
  LabelTotalValue.Caption := Curr64ToString(PInt64(@Total)^);
end;

procedure TInvoiceEditForm.FormDestroy(Sender: TObject);
begin
  SetLength(FItems, 0);
end;

procedure TInvoiceEditForm.ItemsListGridSelectItem(Sender: TObject;
  Item: TMDListItem; Selected: Boolean);
begin
  UpdateButtons;
end;

procedure TInvoiceEditForm.ItemsListGridDblClick(Sender: TObject);
begin
  if FItemsListGrid.Selected <> nil then
    EditItemButtonClick(Sender);
end;

procedure TInvoiceEditForm.AddItemButtonClick(Sender: TObject);
var
  NewItem: TDtoInvoiceItem;
  ItemEditForm: TInvoiceItemEditForm;
  Len: Integer;
begin
  FillChar(NewItem, SizeOf(NewItem), 0);
  NewItem.Quantity := 1;
  NewItem.ListPrice := 0;
  NewItem.Discount := 0;

  ItemEditForm := TInvoiceItemEditForm.Create(Self);
  try
    if ItemEditForm.ShowItemEdit(NewItem, True) then
    begin
      Len := Length(FItems);
      SetLength(FItems, Len + 1);
      NewItem.Position := Len + 1;
      NewItem.Amount := NewItem.ListPrice * NewItem.Quantity;
      if NewItem.Discount > 0 then
        NewItem.Amount := NewItem.Amount * (100 - NewItem.Discount) / 100;
      FItems[Len] := NewItem;
      LoadItemsToListGrid;
    end;
  finally
    ItemEditForm.Free;
  end;
end;

procedure TInvoiceEditForm.EditItemButtonClick(Sender: TObject);
var
  Index: Integer;
  Item: TDtoInvoiceItem;
  ItemEditForm: TInvoiceItemEditForm;
begin
  if FItemsListGrid.Selected = nil then
    Exit;

  Index := Integer(PtrInt(FItemsListGrid.Selected.Data));
  if (Index < 0) or (Index >= Length(FItems)) then
    Exit;

  Item := FItems[Index];

  ItemEditForm := TInvoiceItemEditForm.Create(Self);
  try
    if ItemEditForm.ShowItemEdit(Item, False) then
    begin
      Item.Position := Index + 1;
      Item.Amount := Item.ListPrice * Item.Quantity;
      if Item.Discount > 0 then
        Item.Amount := Item.Amount * (100 - Item.Discount) / 100;
      FItems[Index] := Item;
      LoadItemsToListGrid;
    end;
  finally
    ItemEditForm.Free;
  end;
end;

procedure TInvoiceEditForm.RemoveItemButtonClick(Sender: TObject);
var
  Index, i: Integer;
begin
  if FItemsListGrid.Selected = nil then
    Exit;

  if MessageDlg('Are you sure you want to remove this item?',
                mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  Index := Integer(PtrInt(FItemsListGrid.Selected.Data));
  if (Index >= 0) and (Index < Length(FItems)) then
  begin
    for i := Index to Length(FItems) - 2 do
    begin
      FItems[i] := FItems[i + 1];
      FItems[i].Position := i + 1;
    end;
    SetLength(FItems, Length(FItems) - 1);
    LoadItemsToListGrid;
  end;
end;

function TInvoiceEditForm.ValidateInput: Boolean;
var
  SaleDate, ShipDate: TDateTime;
begin
  Result := False;

  if Trim(EditOrderNo.Text) = '' then
  begin
    ShowMessage('Invoice number is required.');
    EditOrderNo.SetFocus;
    Exit;
  end;

  if not AppTryStrToDate(EditSaleDate.Text, SaleDate) then
  begin
    ShowMessage(Format('Please enter a valid sale date (%s).', [AppDateFormatHint]));
    EditSaleDate.SetFocus;
    Exit;
  end;

  if not AppTryStrToDate(EditShipDate.Text, ShipDate) then
  begin
    ShowMessage(Format('Please enter a valid due date (%s).', [AppDateFormatHint]));
    EditShipDate.SetFocus;
    Exit;
  end;

  if Length(FItems) = 0 then
  begin
    ShowMessage('Invoice must have at least one item.');
    Exit;
  end;

  Result := True;
end;

procedure TInvoiceEditForm.SaveButtonClick(Sender: TObject);
var
  Invoice: TDtoInvoiceSave;
  SaleDate, ShipDate: TDateTime;
  NewID: longint;
  SaveResult: TInvoiceEditResult;
begin
  if not ValidateInput then
    Exit;

  AppTryStrToDate(EditSaleDate.Text, SaleDate);
  AppTryStrToDate(EditShipDate.Text, ShipDate);

  Finalize(Invoice);
  FillChar(Invoice, SizeOf(Invoice), 0);
  Invoice.OrderNo := StringToUtf8(Trim(EditOrderNo.Text));
  Invoice.SaleDate := SaleDate;
  Invoice.ShipDate := ShipDate;
  Invoice.Items := FItems;

  if FIsNew then
    SaveResult := RgServices.InvoiceService.CreateInvoice(FCustomerID, Invoice, NewID)
  else
    SaveResult := RgServices.InvoiceService.UpdateInvoice(FInvoiceID, Invoice);

  case SaveResult of
    ierSuccess:
    begin
      FSaveSuccessful := True;
      ModalResult := mrOk;
    end;
    ierNotFound:
      ShowMessage('Invoice not found.');
    ierMissingField:
      ShowMessage('Required field is missing.');
    ierDatabaseError:
      ShowMessage('Database error. Invoice could not be saved.');
  end;
end;

procedure TInvoiceEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

function TInvoiceEditForm.ShowNewInvoice(ACustomerID: longint): Boolean;
var
  OrderNo: RawUtf8;
  Summary: TDtoCustomerSummary;
begin
  Result := False;
  FSaveSuccessful := False;

  if ACustomerID <= 0 then
  begin
    ShowMessage('Could not create new invoice. Customer not found.');
    Exit;
  end;

  FCustomerID := ACustomerID;
  FInvoiceID := 0;
  FIsNew := True;
  SetLength(FItems, 0);

  // Get customer name via statistics service
  RgServices.StatisticsService.GetCustomerSummary(ACustomerID, Summary);

  // Generate order number
  RgServices.InvoiceService.GenerateOrderNo(OrderNo);

  Caption := 'New Invoice';
  LabelCustomerValue.Caption := Utf8ToString(Summary.CustomerName);
  EditOrderNo.Text := Utf8ToString(OrderNo);
  EditSaleDate.Text := AppDateToStr(Date);
  EditShipDate.Text := AppDateToStr(Date + 14);

  LoadItemsToListGrid;

  Result := (ShowModal = mrOk) and FSaveSuccessful;
end;

function TInvoiceEditForm.ShowEditInvoice(AInvoiceID: longint): Boolean;
var
  Detail: TDtoInvoiceDetail;
  Res: TInvoiceEditResult;
begin
  Result := False;
  FSaveSuccessful := False;

  Res := RgServices.InvoiceService.GetInvoice(AInvoiceID, Detail);
  if Res <> ierSuccess then
  begin
    ShowMessage('Invoice not found.');
    Exit;
  end;

  FInvoiceID := AInvoiceID;
  FCustomerID := Detail.CustomerID;
  FIsNew := False;
  FItems := Detail.Items;

  Caption := 'Edit Invoice';
  LabelCustomerValue.Caption := Utf8ToString(Detail.CustomerName);
  EditOrderNo.Text := Utf8ToString(Detail.OrderNo);
  EditSaleDate.Text := AppDateToStr(Detail.SaleDate);
  EditShipDate.Text := AppDateToStr(Detail.ShipDate);

  LoadItemsToListGrid;

  Result := (ShowModal = mrOk) and FSaveSuccessful;
end;

function TInvoiceEditForm.GetFormMenu: TMainMenu;
begin
  Result := nil;
end;

end.
