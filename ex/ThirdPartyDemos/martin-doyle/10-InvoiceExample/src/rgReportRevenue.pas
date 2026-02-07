{:
———————————————————————————————————————————————— © martindoyle 2017-2026 ——
 Project : Rechnung

 Using mORMot2
     Synopse mORMot2 framework. Copyright (C) 2025 Arnaud Bouchez
     Synopse Informatique - http://synopse.info

  Module : rgReportRevenue.pas

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
unit rgReportRevenue;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, rgReportBase, rgClient, rgDtoTypes;

type

  { TCustomerRevenueReportForm }

  TCustomerRevenueReportForm = class(TReportBaseForm)
    LabelYear: TLabel;
    ComboYear: TComboBox;
    RefreshButton: TButton;
    procedure RefreshButtonClick(Sender: TObject);
  private
    FSelectedYear: integer;
    procedure PopulateYearCombo;
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
  CustomerRevenueReportForm: TCustomerRevenueReportForm;

implementation

uses
  mdGrids, DateUtils,
  mormot.core.base,
  mormot.core.text;

type
  TMDListColumn = mdGrids.TMDListColumn;
  TMDListItem = mdGrids.TMDListItem;

{$R *.dfm}

{ TCustomerRevenueReportForm }

constructor TCustomerRevenueReportForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  PopulateYearCombo;

  // Select current year by default
  FSelectedYear := YearOf(Date);
  ComboYear.ItemIndex := ComboYear.Items.IndexOf(IntToStr(FSelectedYear));
  if ComboYear.ItemIndex < 0 then
    ComboYear.ItemIndex := 0;
end;

destructor TCustomerRevenueReportForm.Destroy;
begin
  inherited Destroy;
end;

procedure TCustomerRevenueReportForm.PopulateYearCombo;
var
  CurrentYear, i: integer;
begin
  ComboYear.Items.Clear;
  CurrentYear := YearOf(Date);

  // Fixed years: last 5 years (Option B)
  for i := CurrentYear downto CurrentYear - 4 do
    ComboYear.Items.Add(IntToStr(i));
end;

function TCustomerRevenueReportForm.GetReportTitle: string;
begin
  Result := 'Customer Revenue Report';
end;

procedure TCustomerRevenueReportForm.SetupBaseLayout;
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
  LabelYear.Top := Round(1.2 * BaseHeight);
  ComboYear.Top := LabelYear.Top - 2;
  RefreshButton.Top := ComboYear.Top - 2;

  // Set form size
  Width := 750;
  Height := 500;
end;

procedure TCustomerRevenueReportForm.ConfigureColumns;
var
  Col: TMDListColumn;
begin
  Col := FResultGrid.Columns.Add;
  Col.Caption := 'Customer';
  Col.Width := 250;

  Col := FResultGrid.Columns.Add;
  Col.Caption := 'Invoices';
  Col.Width := 80;
  Col.Alignment := taRightJustify;

  Col := FResultGrid.Columns.Add;
  Col.Caption := 'Total Revenue';
  Col.Width := 120;
  Col.Alignment := taRightJustify;

  Col := FResultGrid.Columns.Add;
  Col.Caption := 'Total Paid';
  Col.Width := 120;
  Col.Alignment := taRightJustify;

  Col := FResultGrid.Columns.Add;
  Col.Caption := 'Total Open';
  Col.Width := 120;
  Col.Alignment := taRightJustify;
end;

procedure TCustomerRevenueReportForm.LoadData;
var
  Items: TDtoCustomerRevenueDynArray;
  i: integer;
  ListItem: TMDListItem;
  YearText: string;
  TempYear: integer;
begin
  YearText := Trim(ComboYear.Text);
  if YearText = '' then
  begin
    ShowMessage('Please enter or select a year.');
    ComboYear.SetFocus;
    Exit;
  end;

  TempYear := StrToIntDef(YearText, 0);
  if (TempYear < 1900) or (TempYear > 2100) then
  begin
    ShowMessage('Please enter a valid year (1900-2100).');
    ComboYear.SetFocus;
    Exit;
  end;

  FSelectedYear := TempYear;
  RgServices.ReportService.GetCustomerRevenueReport(FSelectedYear, Items);

  for i := 0 to High(Items) do
  begin
    ListItem := FResultGrid.Items.Add;
    ListItem.Caption := Items[i].Company;
    ListItem.SubItems.Add(IntToStr(Items[i].InvoiceCount));
    ListItem.SubItems.Add(Curr64ToString(PInt64(@Items[i].TotalRevenue)^));
    ListItem.SubItems.Add(Curr64ToString(PInt64(@Items[i].TotalPaid)^));
    ListItem.SubItems.Add(Curr64ToString(PInt64(@Items[i].TotalOpen)^));
    ListItem.Data := Pointer(PtrInt(Items[i].CustomerID));
  end;
end;

procedure TCustomerRevenueReportForm.RefreshButtonClick(Sender: TObject);
begin
  RefreshReport;
end;

end.
