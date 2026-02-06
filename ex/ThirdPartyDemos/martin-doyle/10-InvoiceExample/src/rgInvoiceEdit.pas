{:
———————————————————————————————————————————————— © martindoyle 2017-2026 ——
 Project : Rechnung

 Using mORMot2
     Synopse mORMot2 framework. Copyright (C) 2025 Arnaud Bouchez
     Synopse Informatique - http://synopse.info

  Module : rgInvoiceEdit.pas

  Last modified
    Date : 01.02.2026
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
  ExtCtrls, Menus, MdForms, mdLayout, mdGrids, rgClient, rgDtoTypes,
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
    FInvoiceService: IInvoiceEditService;
    FInvoiceID: longint;
    FCustomerID: longint;
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
  mormot.core.text;

{$R *.dfm}

{ TInvoiceEditForm }

procedure TInvoiceEditForm.FormCreate(Sender: TObject);
begin
  FInvoiceService := TInvoiceEditService.Create;
  FInvoiceID := 0;
  FCustomerID := 0;
  FSaveSuccessful := False;

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
  Item: TDtoInvoiceItem;
  ListItem: TMDListItem;
begin
  FItemsListGrid.Items.BeginUpdate;
  try
    FItemsListGrid.Items.Clear;

    for i := 0 to FInvoiceService.GetItemCount - 1 do
    begin
      Item := FInvoiceService.GetItem(i);

      ListItem := FItemsListGrid.Items.Add;
      ListItem.Caption := IntToStr(Item.Position);
      ListItem.SubItems.Add(Item.Description);
      ListItem.SubItems.Add(Format('%.2f', [Item.Quantity]));
      ListItem.SubItems.Add(Format('%.2n', [Double(Item.ListPrice)]));
      ListItem.SubItems.Add(IntToStr(Item.Discount));
      ListItem.SubItems.Add(Format('%.2n', [Double(Item.Amount)]));
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
begin
  LabelTotalValue.Caption := Format('%.2n', [Double(FInvoiceService.GetItemsTotal)]);
end;

procedure TInvoiceEditForm.FormDestroy(Sender: TObject);
begin
  FInvoiceService := nil;
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
begin
  FillChar(NewItem, SizeOf(NewItem), 0);
  NewItem.Quantity := 1;
  NewItem.ListPrice := 0;
  NewItem.Discount := 0;

  ItemEditForm := TInvoiceItemEditForm.Create(Self);
  try
    if ItemEditForm.ShowItemEdit(NewItem, True) then
    begin
      FInvoiceService.AddItem(NewItem);
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
  Item := FInvoiceService.GetItem(Index);

  ItemEditForm := TInvoiceItemEditForm.Create(Self);
  try
    if ItemEditForm.ShowItemEdit(Item, False) then
    begin
      FInvoiceService.UpdateItem(Index, Item);
      LoadItemsToListGrid;
    end;
  finally
    ItemEditForm.Free;
  end;
end;

procedure TInvoiceEditForm.RemoveItemButtonClick(Sender: TObject);
var
  Index: Integer;
begin
  if FItemsListGrid.Selected = nil then
    Exit;

  if MessageDlg('Are you sure you want to remove this item?',
                mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  Index := Integer(PtrInt(FItemsListGrid.Selected.Data));
  FInvoiceService.DeleteItem(Index);
  LoadItemsToListGrid;
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

  if not TryStrToDate(EditSaleDate.Text, SaleDate) then
  begin
    ShowMessage('Please enter a valid sale date (dd.mm.yyyy).');
    EditSaleDate.SetFocus;
    Exit;
  end;

  if not TryStrToDate(EditShipDate.Text, ShipDate) then
  begin
    ShowMessage('Please enter a valid due date (dd.mm.yyyy).');
    EditShipDate.SetFocus;
    Exit;
  end;

  if FInvoiceService.GetItemCount = 0 then
  begin
    ShowMessage('Invoice must have at least one item.');
    Exit;
  end;

  FInvoiceService.SetOrderNo(Trim(EditOrderNo.Text));
  FInvoiceService.SetSaleDate(SaleDate);
  FInvoiceService.SetShipDate(ShipDate);

  Result := True;
end;

procedure TInvoiceEditForm.SaveButtonClick(Sender: TObject);
var
  SaveResult: TInvoiceEditResult;
begin
  if not ValidateInput then
    Exit;

  SaveResult := FInvoiceService.Save;

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
begin
  Result := False;
  FSaveSuccessful := False;

  if not FInvoiceService.CreateNewInvoice(ACustomerID) then
  begin
    ShowMessage('Could not create new invoice. Customer not found.');
    Exit;
  end;

  FCustomerID := ACustomerID;
  FInvoiceID := 0;

  Caption := 'New Invoice';
  LabelCustomerValue.Caption := FInvoiceService.GetCustomerName;
  EditOrderNo.Text := FInvoiceService.GetOrderNo;
  EditSaleDate.Text := FormatDateTime('dd.mm.yyyy', FInvoiceService.GetSaleDate);
  EditShipDate.Text := FormatDateTime('dd.mm.yyyy', FInvoiceService.GetShipDate);

  LoadItemsToListGrid;

  Result := (ShowModal = mrOk) and FSaveSuccessful;
end;

function TInvoiceEditForm.ShowEditInvoice(AInvoiceID: longint): Boolean;
begin
  Result := False;
  FSaveSuccessful := False;

  if not FInvoiceService.LoadInvoice(AInvoiceID) then
  begin
    ShowMessage('Invoice not found.');
    Exit;
  end;

  FInvoiceID := AInvoiceID;
  FCustomerID := FInvoiceService.GetCustomerID;

  Caption := 'Edit Invoice';
  LabelCustomerValue.Caption := FInvoiceService.GetCustomerName;
  EditOrderNo.Text := FInvoiceService.GetOrderNo;
  EditSaleDate.Text := FormatDateTime('dd.mm.yyyy', FInvoiceService.GetSaleDate);
  EditShipDate.Text := FormatDateTime('dd.mm.yyyy', FInvoiceService.GetShipDate);

  LoadItemsToListGrid;

  Result := (ShowModal = mrOk) and FSaveSuccessful;
end;

function TInvoiceEditForm.GetFormMenu: TMainMenu;
begin
  Result := nil;
end;

end.
