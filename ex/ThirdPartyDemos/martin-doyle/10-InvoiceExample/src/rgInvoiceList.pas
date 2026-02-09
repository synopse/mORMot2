{:
---------------------------------------------------(C) martindoyle 2017-2026 --
 Project : Rechnung

 Using mORMot2
     Synopse mORMot2 framework. Copyright (C) 2025 Arnaud Bouchez
     Synopse Informatique - http://synopse.info

  Module : rgInvoiceList.pas

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
unit rgInvoiceList;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Menus, MdForms, mdGrids, rgClient, rgDtoTypes;

type
  TPaymentCompletedEvent = procedure(Sender: TObject) of object;

  { TInvoiceListForm }

  TInvoiceListForm = class(TMDChildForm)
    ToolbarPanel: TPanel;
    NewButton: TButton;
    EditButton: TButton;
    DeleteButton: TButton;
    PayButton: TButton;
    LegendPanel: TPanel;
    LegendPaid: TLabel;
    LegendOpen: TLabel;
    LegendOverdue: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure NewButtonClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure PayButtonClick(Sender: TObject);
  private
    FCurrentCustomerID: longint;
    FBaseHeight: Integer;
    FOnPaymentCompleted: TPaymentCompletedEvent;
    FInvoiceListGrid: TMDListGrid;
    procedure LoadInvoices;
    procedure SetListGridColumns;
    procedure UpdateButtons;
    procedure UpdateColumnWidths;
    procedure SetupLayout;
    function GetBaseHeight: Integer;
    function GetSelectedInvoiceID: longint;
    function GetSelectedInvoice: TDtoOrder;
    function StatusToIndicator(AStatus: TInvoiceStatus): string;
    function IsSelectedInvoiceOpen: Boolean;
    procedure InvoiceListGridSelectItem(Sender: TObject; Item: TMDListItem;
      Selected: Boolean);
    procedure InvoiceListGridDblClick(Sender: TObject);
  public
    function GetFormMenu: TMainMenu; override;
    procedure LoadForCustomer(ACustomerID: longint);
    procedure RefreshList;
    procedure ClearList;
    property OnPaymentCompleted: TPaymentCompletedEvent
      read FOnPaymentCompleted write FOnPaymentCompleted;
  end;

var
  InvoiceListForm: TInvoiceListForm;

implementation

uses
  mormot.core.base,
  mormot.core.text,
  mormot.core.unicode,
  mdDates,
  rgPaymentEntry,
  rgInvoiceEdit;

{$R *.dfm}

const
  // Cross-platform ASCII symbols
  STATUS_PAID = '[+]';
  STATUS_OPEN = '[o]';
  STATUS_OVERDUE = '[!]';

{ TInvoiceListForm }

procedure TInvoiceListForm.FormCreate(Sender: TObject);
begin
  FCurrentCustomerID := 0;
  FBaseHeight := GetBaseHeight;

  // Create TMDListGrid (replaces TListView for consistency)
  FInvoiceListGrid := TMDListGrid.Create(Self);
  FInvoiceListGrid.Parent := Self;
  FInvoiceListGrid.Align := alClient;
  FInvoiceListGrid.OnSelectItem := InvoiceListGridSelectItem;
  FInvoiceListGrid.OnDblClick := InvoiceListGridDblClick;

  SetListGridColumns;
  SetupLayout;
end;

procedure TInvoiceListForm.FormDestroy(Sender: TObject);
begin
  // nothing to free - services accessed via RgServices global
end;

procedure TInvoiceListForm.FormShow(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TInvoiceListForm.FormResize(Sender: TObject);
begin
  UpdateColumnWidths;
end;

function TInvoiceListForm.GetBaseHeight: Integer;
begin
  Result := Canvas.TextHeight('Ag');
  if Result < 16 then
    Result := 16;
end;

procedure TInvoiceListForm.SetupLayout;
var
  Margin: Integer;
  ButtonSpacing: Integer;
  LegendSpacing: Integer;
begin
  Margin := FBaseHeight div 2;
  ButtonSpacing := FBaseHeight div 2;
  LegendSpacing := FBaseHeight;

  // Position toolbar buttons
  NewButton.Left := Margin;
  NewButton.Top := (ToolbarPanel.Height - NewButton.Height) div 2;

  EditButton.Left := NewButton.Left + NewButton.Width + ButtonSpacing;
  EditButton.Top := NewButton.Top;

  DeleteButton.Left := EditButton.Left + EditButton.Width + ButtonSpacing;
  DeleteButton.Top := NewButton.Top;

  PayButton.Left := DeleteButton.Left + DeleteButton.Width + ButtonSpacing * 2;
  PayButton.Top := NewButton.Top;

  // Position legend labels
  LegendPaid.Left := Margin;
  LegendPaid.Top := (LegendPanel.Height - LegendPaid.Height) div 2;

  LegendOpen.Left := LegendPaid.Left + LegendPaid.Width + LegendSpacing;
  LegendOpen.Top := LegendPaid.Top;

  LegendOverdue.Left := LegendOpen.Left + LegendOpen.Width + LegendSpacing;
  LegendOverdue.Top := LegendPaid.Top;
end;

procedure TInvoiceListForm.SetListGridColumns;
var
  Col: TMDListColumn;
begin
  FInvoiceListGrid.RowSelect := True;

  // Column 0: Invoice No.
  Col := FInvoiceListGrid.Columns.Add;
  Col.Caption := 'No.';
  Col.Width := Round(5 * FBaseHeight);

  // Column 1: Date
  Col := FInvoiceListGrid.Columns.Add;
  Col.Caption := 'Date';
  Col.Width := Round(6 * FBaseHeight);

  // Column 2: Amount
  Col := FInvoiceListGrid.Columns.Add;
  Col.Caption := 'Amount';
  Col.Width := Round(6 * FBaseHeight);
  Col.Alignment := taRightJustify;

  // Column 3: Paid
  Col := FInvoiceListGrid.Columns.Add;
  Col.Caption := 'Paid';
  Col.Width := Round(6 * FBaseHeight);
  Col.Alignment := taRightJustify;

  // Column 4: Open
  Col := FInvoiceListGrid.Columns.Add;
  Col.Caption := 'Open';
  Col.Width := Round(6 * FBaseHeight);
  Col.Alignment := taRightJustify;

  // Column 5: Status
  Col := FInvoiceListGrid.Columns.Add;
  Col.Caption := '';
  Col.Width := Round(2 * FBaseHeight);
  Col.Alignment := taCenter;
end;

procedure TInvoiceListForm.UpdateColumnWidths;
var
  AvailableWidth: Integer;
  NoWidth, DateWidth, AmountWidth, PaidWidth, OpenWidth, StatusWidth: Integer;
begin
  if FInvoiceListGrid = nil then Exit;
  if FInvoiceListGrid.Columns.Count < 6 then Exit;

  StatusWidth := Round(2 * FBaseHeight);
  AvailableWidth := FInvoiceListGrid.Width - StatusWidth - 24;

  // Distribute remaining width proportionally
  NoWidth := (AvailableWidth * 15) div 100;
  DateWidth := (AvailableWidth * 17) div 100;
  AmountWidth := (AvailableWidth * 23) div 100;
  PaidWidth := (AvailableWidth * 23) div 100;
  OpenWidth := AvailableWidth - NoWidth - DateWidth - AmountWidth - PaidWidth;

  FInvoiceListGrid.Columns[0].Width := NoWidth;
  FInvoiceListGrid.Columns[1].Width := DateWidth;
  FInvoiceListGrid.Columns[2].Width := AmountWidth;
  FInvoiceListGrid.Columns[3].Width := PaidWidth;
  FInvoiceListGrid.Columns[4].Width := OpenWidth;
  FInvoiceListGrid.Columns[5].Width := StatusWidth;
end;

function TInvoiceListForm.StatusToIndicator(AStatus: TInvoiceStatus): string;
begin
  case AStatus of
    isPaid: Result := STATUS_PAID;
    isOpen: Result := STATUS_OPEN;
    isOverdue: Result := STATUS_OVERDUE;
  else
    Result := '';
  end;
end;

procedure TInvoiceListForm.LoadInvoices;
var
  Invoices: TDtoOrderDynArray;
  Item: TMDListItem;
  i: Integer;
begin
  FInvoiceListGrid.Items.BeginUpdate;
  try
    FInvoiceListGrid.Items.Clear;

    if FCurrentCustomerID <= 0 then
      Exit;

    RgServices.InvoiceService.ListInvoicesForCustomer(FCurrentCustomerID, Invoices);

    for i := 0 to High(Invoices) do
    begin
      Item := FInvoiceListGrid.Items.Add;
      Item.Caption := Utf8ToString(Invoices[i].OrderNo);
      Item.SubItems.Add(AppDateToStr(Invoices[i].SaleDate));
      Item.SubItems.Add(Format('%.2n', [Invoices[i].ItemsTotal]));
      Item.SubItems.Add(Format('%.2n', [Invoices[i].AmountPaid]));
      Item.SubItems.Add(Format('%.2n', [Invoices[i].OpenAmount]));
      Item.SubItems.Add(StatusToIndicator(Invoices[i].Status));
      Item.Data := Pointer(PtrInt(Invoices[i].OrderID));
    end;
  finally
    FInvoiceListGrid.Items.EndUpdate;
  end;
end;

procedure TInvoiceListForm.UpdateButtons;
var
  HasSelection: Boolean;
begin
  HasSelection := FInvoiceListGrid.Selected <> nil;
  EditButton.Enabled := HasSelection;
  DeleteButton.Enabled := HasSelection;
  PayButton.Enabled := HasSelection and IsSelectedInvoiceOpen;
  NewButton.Enabled := FCurrentCustomerID > 0;
end;

function TInvoiceListForm.GetSelectedInvoiceID: longint;
begin
  Result := 0;
  if FInvoiceListGrid.Selected <> nil then
    Result := longint(PtrInt(FInvoiceListGrid.Selected.Data));
end;

function TInvoiceListForm.GetSelectedInvoice: TDtoOrder;
var
  Invoices: TDtoOrderDynArray;
  InvoiceID: longint;
  i: Integer;
begin
  Finalize(Result);
  FillChar(Result, SizeOf(Result), 0);
  InvoiceID := GetSelectedInvoiceID;
  if InvoiceID <= 0 then
    Exit;

  RgServices.InvoiceService.ListInvoicesForCustomer(FCurrentCustomerID, Invoices);
  for i := 0 to High(Invoices) do
  begin
    if Invoices[i].OrderID = InvoiceID then
    begin
      Result := Invoices[i];
      Exit;
    end;
  end;
end;

function TInvoiceListForm.IsSelectedInvoiceOpen: Boolean;
var
  Item: TMDListItem;
  OpenStr: string;
begin
  Result := False;
  Item := FInvoiceListGrid.Selected;
  if Item = nil then
    Exit;
  if Item.SubItems.Count < 4 then
    Exit;

  OpenStr := Item.SubItems[3];
  Result := (Trim(OpenStr) <> '') and (Trim(OpenStr) <> '0') and
            (Trim(OpenStr) <> '0,00') and (Trim(OpenStr) <> '0.00');
end;

procedure TInvoiceListForm.InvoiceListGridSelectItem(Sender: TObject;
  Item: TMDListItem; Selected: Boolean);
begin
  UpdateButtons;
end;

procedure TInvoiceListForm.InvoiceListGridDblClick(Sender: TObject);
begin
  if FInvoiceListGrid.Selected <> nil then
    EditButtonClick(Sender);
end;

procedure TInvoiceListForm.NewButtonClick(Sender: TObject);
var
  EditForm: TInvoiceEditForm;
begin
  if FCurrentCustomerID > 0 then
  begin
    EditForm := TInvoiceEditForm.Create(Application);
    try
      if EditForm.ShowNewInvoice(FCurrentCustomerID) then
      begin
        RefreshList;
        if Assigned(FOnPaymentCompleted) then
          FOnPaymentCompleted(Self);
      end;
    finally
      EditForm.Free;
    end;
  end;
end;

procedure TInvoiceListForm.EditButtonClick(Sender: TObject);
var
  InvoiceID: longint;
  EditForm: TInvoiceEditForm;
begin
  InvoiceID := GetSelectedInvoiceID;
  if InvoiceID > 0 then
  begin
    EditForm := TInvoiceEditForm.Create(Application);
    try
      if EditForm.ShowEditInvoice(InvoiceID) then
      begin
        RefreshList;
        if Assigned(FOnPaymentCompleted) then
          FOnPaymentCompleted(Self);
      end;
    finally
      EditForm.Free;
    end;
  end;
end;

procedure TInvoiceListForm.DeleteButtonClick(Sender: TObject);
var
  InvoiceID: longint;
  Res: TInvoiceEditResult;
begin
  InvoiceID := GetSelectedInvoiceID;
  if InvoiceID > 0 then
  begin
    if MessageDlg('Are you sure you want to delete this invoice?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      Res := RgServices.InvoiceService.DeleteInvoice(InvoiceID);
      case Res of
        ierSuccess:
        begin
          RefreshList;
          if Assigned(FOnPaymentCompleted) then
            FOnPaymentCompleted(Self);
        end;
        ierNotFound:
          ShowMessage('Invoice not found.');
      else
        ShowMessage('Database error. The invoice could not be deleted.');
      end;
    end;
  end;
end;

procedure TInvoiceListForm.PayButtonClick(Sender: TObject);
var
  Invoice: TDtoOrder;
  PaymentForm: TPaymentEntryForm;
begin
  Invoice := GetSelectedInvoice;
  if Invoice.OrderID <= 0 then
    Exit;

  if Invoice.OpenAmount <= 0 then
  begin
    ShowMessage('This invoice is already fully paid.');
    Exit;
  end;

  PaymentForm := TPaymentEntryForm.Create(Application);
  try
    if PaymentForm.ShowPaymentEntry(Invoice.OrderID, Utf8ToString(Invoice.OrderNo),
      Invoice.OpenAmount) then
    begin
      RefreshList;
      if Assigned(FOnPaymentCompleted) then
        FOnPaymentCompleted(Self);
    end;
  finally
    PaymentForm.Free;
  end;
end;

procedure TInvoiceListForm.LoadForCustomer(ACustomerID: longint);
begin
  FCurrentCustomerID := ACustomerID;
  LoadInvoices;
  UpdateButtons;
end;

procedure TInvoiceListForm.RefreshList;
begin
  LoadInvoices;
  UpdateButtons;
end;

procedure TInvoiceListForm.ClearList;
begin
  FCurrentCustomerID := 0;
  FInvoiceListGrid.Items.Clear;
  UpdateButtons;
end;

function TInvoiceListForm.GetFormMenu: TMainMenu;
begin
  Result := nil;
end;

end.
