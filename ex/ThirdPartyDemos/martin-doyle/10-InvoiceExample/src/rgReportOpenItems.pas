{:
———————————————————————————————————————————————— © martindoyle 2017-2026 ——
 Project : Rechnung

 Using mORMot2
     Synopse mORMot2 framework. Copyright (C) 2025 Arnaud Bouchez
     Synopse Informatique - http://synopse.info

  Module : rgReportOpenItems.pas

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
unit rgReportOpenItems;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, rgReportBase, rgClient, rgDtoTypes;

type

  { TOpenItemsReportForm }

  TOpenItemsReportForm = class(TReportBaseForm)
    LabelFromDate: TLabel;
    EditFromDate: TEdit;
    LabelToDate: TLabel;
    EditToDate: TEdit;
    LabelMinAmount: TLabel;
    EditMinAmount: TEdit;
    RefreshButton: TButton;
    procedure RefreshButtonClick(Sender: TObject);
  private
    FReportService: IOpenItemsReportService;
    FFromDate: TDateTime;
    FToDate: TDateTime;
    FMinAmount: currency;
    function ParseDate(const AText: string; out ADate: TDateTime): Boolean;
    function ParseAmount(const AText: string; out AAmount: currency): Boolean;
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
  OpenItemsReportForm: TOpenItemsReportForm;

implementation

uses
  mdGrids,
  mormot.core.base,
  mormot.core.text,
  mormot.core.unicode;

type
  TMDListColumn = mdGrids.TMDListColumn;
  TMDListItem = mdGrids.TMDListItem;

{$R *.dfm}

{ TOpenItemsReportForm }

constructor TOpenItemsReportForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FReportService := TOpenItemsReportService.Create;

  // Default filter values: last 90 days
  FToDate := Date;
  FFromDate := Date - 90;
  FMinAmount := 0;

  EditFromDate.Text := DateToStr(FFromDate);
  EditToDate.Text := DateToStr(FToDate);
  EditMinAmount.Text := '0';
end;

destructor TOpenItemsReportForm.Destroy;
begin
  FReportService := nil;
  inherited Destroy;
end;

function TOpenItemsReportForm.GetReportTitle: string;
begin
  Result := 'Open Items Report';
end;

procedure TOpenItemsReportForm.SetupBaseLayout;
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
  LabelMinAmount.Top := LabelFromDate.Top;
  EditMinAmount.Top := EditFromDate.Top;
  RefreshButton.Top := EditFromDate.Top - 2;

  // Set form size
  Width := 850;
  Height := 550;
end;

function TOpenItemsReportForm.ParseDate(const AText: string;
  out ADate: TDateTime): Boolean;
var
  TempText: string;
begin
  Result := False;
  ADate := 0;

  TempText := Trim(AText);
  if TempText = '' then
    Exit;

  // Use system locale for date parsing
  Result := TryStrToDate(TempText, ADate);
end;

procedure TOpenItemsReportForm.ConfigureColumns;
var
  Col: TMDListColumn;
begin
  Col := FResultGrid.Columns.Add;
  Col.Caption := 'Company';
  Col.Width := 200;

  Col := FResultGrid.Columns.Add;
  Col.Caption := 'Invoice No';
  Col.Width := 100;

  Col := FResultGrid.Columns.Add;
  Col.Caption := 'Sale Date';
  Col.Width := 100;

  Col := FResultGrid.Columns.Add;
  Col.Caption := 'Total';
  Col.Width := 100;
  Col.Alignment := taRightJustify;

  Col := FResultGrid.Columns.Add;
  Col.Caption := 'Open Amount';
  Col.Width := 110;
  Col.Alignment := taRightJustify;

  Col := FResultGrid.Columns.Add;
  Col.Caption := 'Days';
  Col.Width := 60;
  Col.Alignment := taRightJustify;
end;

function TOpenItemsReportForm.ParseAmount(const AText: string;
  out AAmount: currency): Boolean;
var
  TempText: string;
  TempValue: extended;
  err: integer;
begin
  Result := False;
  AAmount := 0;

  TempText := Trim(AText);
  if TempText = '' then
  begin
    AAmount := 0;
    Result := True;
    Exit;
  end;

  TempText := StringReplace(TempText, ',', '.', [rfReplaceAll]);
  TempValue := GetExtended(pointer(StringToUtf8(TempText)), err);
  if err <> 0 then
    Exit;
  AAmount := TempValue;
  Result := True;
end;

function TOpenItemsReportForm.ValidateFilters: Boolean;
var
  TempDate: TDateTime;
  TempAmount: currency;
begin
  Result := False;

  if not ParseDate(EditFromDate.Text, TempDate) then
  begin
    ShowMessage(Format('Please enter a valid From Date (%s).', [{$IFDEF FPC}FormatSettings.{$ENDIF}ShortDateFormat]));
    EditFromDate.SetFocus;
    Exit;
  end;
  FFromDate := TempDate;

  if not ParseDate(EditToDate.Text, TempDate) then
  begin
    ShowMessage(Format('Please enter a valid To Date (%s).', [{$IFDEF FPC}FormatSettings.{$ENDIF}ShortDateFormat]));
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

  if not ParseAmount(EditMinAmount.Text, TempAmount) then
  begin
    ShowMessage('Please enter a valid minimum amount.');
    EditMinAmount.SetFocus;
    Exit;
  end;
  FMinAmount := TempAmount;

  Result := True;
end;

procedure TOpenItemsReportForm.LoadData;
var
  i: integer;
  Item: TDtoOpenItem;
  ListItem: TMDListItem;
begin
  if not ValidateFilters then
    Exit;

  FReportService.LoadOpenItems(FFromDate, FToDate, FMinAmount);

  for i := 0 to FReportService.GetItemCount - 1 do
  begin
    Item := FReportService.GetItem(i);
    ListItem := FResultGrid.Items.Add;
    ListItem.Caption := Item.Company;
    ListItem.SubItems.Add(Item.OrderNo);
    if Item.SaleDate > 0 then
      ListItem.SubItems.Add(DateToStr(Item.SaleDate))
    else
      ListItem.SubItems.Add('');
    ListItem.SubItems.Add(Curr64ToString(PInt64(@Item.ItemsTotal)^));
    ListItem.SubItems.Add(Curr64ToString(PInt64(@Item.OpenAmount)^));
    ListItem.SubItems.Add(IntToStr(Item.DaysOverdue));
    ListItem.Data := Pointer(PtrInt(Item.OrderID));
  end;
end;

procedure TOpenItemsReportForm.RefreshButtonClick(Sender: TObject);
begin
  RefreshReport;
end;

end.
