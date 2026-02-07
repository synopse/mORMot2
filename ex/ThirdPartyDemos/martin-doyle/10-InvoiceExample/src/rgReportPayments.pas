{:
———————————————————————————————————————————————— © martindoyle 2017-2026 ——
 Project : Rechnung

 Using mORMot2
     Synopse mORMot2 framework. Copyright (C) 2025 Arnaud Bouchez
     Synopse Informatique - http://synopse.info

  Module : rgReportPayments.pas

  Last modified
    Date : 07.02.2026
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
unit rgReportPayments;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, rgReportBase, rgClient, rgDtoTypes;

type

  { TPaymentReceiptsReportForm }

  TPaymentReceiptsReportForm = class(TReportBaseForm)
    LabelFromDate: TLabel;
    EditFromDate: TEdit;
    LabelToDate: TLabel;
    EditToDate: TEdit;
    RefreshButton: TButton;
    procedure RefreshButtonClick(Sender: TObject);
  private
    FFromDate: TDateTime;
    FToDate: TDateTime;
    function ParseDate(const AText: string; out ADate: TDateTime): Boolean;
    function ValidateFilters: Boolean;
  protected
    procedure ConfigureColumns; override;
    procedure LoadData; override;
    function GetReportTitle: string; override;
    procedure SetupBaseLayout; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  PaymentReceiptsReportForm: TPaymentReceiptsReportForm;

implementation

uses
  mdGrids,
  mormot.core.base,
  mormot.core.text,
  mdDates;

type
  TMDListColumn = mdGrids.TMDListColumn;
  TMDListItem = mdGrids.TMDListItem;

{$R *.dfm}

{ TPaymentReceiptsReportForm }

constructor TPaymentReceiptsReportForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Default filter values: last 30 days
  FToDate := Date;
  FFromDate := Date - 30;

  EditFromDate.Text := AppDateToStr(FFromDate);
  EditToDate.Text := AppDateToStr(FToDate);
end;

destructor TPaymentReceiptsReportForm.Destroy;
begin
  inherited Destroy;
end;

function TPaymentReceiptsReportForm.GetReportTitle: string;
begin
  Result := 'Payment Receipts Report';
end;

procedure TPaymentReceiptsReportForm.SetupBaseLayout;
var
  BaseHeight: Integer;
begin
  inherited SetupBaseLayout;

  BaseHeight := Canvas.TextHeight('Ag');
  if BaseHeight < 16 then
    BaseHeight := 16;

  // Adjust filter panel height
  FilterPanel.Height := Round(4 * BaseHeight);

  // Position filter controls
  LabelFromDate.Top := Round(1.2 * BaseHeight);
  EditFromDate.Top := LabelFromDate.Top - 2;
  LabelToDate.Top := LabelFromDate.Top;
  EditToDate.Top := EditFromDate.Top;
  RefreshButton.Top := EditFromDate.Top - 2;

  // Set form size
  Width := 700;
  Height := 500;
end;

function TPaymentReceiptsReportForm.ParseDate(const AText: string;
  out ADate: TDateTime): Boolean;
begin
  Result := AppTryStrToDate(AText, ADate);
end;

procedure TPaymentReceiptsReportForm.ConfigureColumns;
var
  Col: TMDListColumn;
begin
  Col := FResultGrid.Columns.Add;
  Col.Caption := 'Date';
  Col.Width := 100;

  Col := FResultGrid.Columns.Add;
  Col.Caption := 'Customer';
  Col.Width := 250;

  Col := FResultGrid.Columns.Add;
  Col.Caption := 'Invoice No';
  Col.Width := 120;

  Col := FResultGrid.Columns.Add;
  Col.Caption := 'Amount Paid';
  Col.Width := 120;
  Col.Alignment := taRightJustify;
end;

function TPaymentReceiptsReportForm.ValidateFilters: Boolean;
var
  TempDate: TDateTime;
begin
  Result := False;

  if not ParseDate(EditFromDate.Text, TempDate) then
  begin
    ShowMessage(Format('Please enter a valid From Date (%s).', [AppDateFormatHint]));
    EditFromDate.SetFocus;
    Exit;
  end;
  FFromDate := TempDate;

  if not ParseDate(EditToDate.Text, TempDate) then
  begin
    ShowMessage(Format('Please enter a valid To Date (%s).', [AppDateFormatHint]));
    EditToDate.SetFocus;
    Exit;
  end;
  FToDate := TempDate;

  if FFromDate > FToDate then
  begin
    ShowMessage('From Date must be before or equal to To Date.');
    EditFromDate.SetFocus;
    Exit;
  end;

  Result := True;
end;

procedure TPaymentReceiptsReportForm.LoadData;
var
  Items: TDtoPaymentReceiptDynArray;
  i: integer;
  ListItem: TMDListItem;
begin
  if not ValidateFilters then
    Exit;

  RgServices.ReportService.GetPaymentReceiptsReport(FFromDate, FToDate, Items);

  for i := 0 to High(Items) do
  begin
    ListItem := FResultGrid.Items.Add;
    if Items[i].SaleDate > 0 then
      ListItem.Caption := AppDateToStr(Items[i].SaleDate)
    else
      ListItem.Caption := '';
    ListItem.SubItems.Add(Items[i].Company);
    ListItem.SubItems.Add(Items[i].OrderNo);
    ListItem.SubItems.Add(Curr64ToString(PInt64(@Items[i].AmountPaid)^));
    ListItem.Data := Pointer(PtrInt(Items[i].OrderID));
  end;
end;

procedure TPaymentReceiptsReportForm.RefreshButtonClick(Sender: TObject);
begin
  RefreshReport;
end;

end.
