{:
———————————————————————————————————————————————— © martindoyle 2017-2026 ——
 Project : Rechnung

 Using mORMot2
     Synopse mORMot2 framework. Copyright (C) 2025 Arnaud Bouchez
     Synopse Informatique - http://synopse.info

  Module : rgMain.pas

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
    ----------------------------------------------------------------------------
}
unit rgMain;

interface

{$I mormot.defines.inc}

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, Menus,
  StdActns, ExtCtrls, StdCtrls, MdForms, rgClient, rgDtoTypes,
  rgCustomerList, rgInvoiceList, ImgList;

type

  { TMainForm }

  TMainForm = class(TForm)
    // Images
    MainImageList: TImageList;

    // Actions
    FileExit: TFileExit;
    MainActionList: TActionList;
    HelpAboutAction: TAction;
    ReportOpenItems: TAction;
    ReportPaymentReceipts: TAction;
    ReportCustomerRevenue: TAction;
    ReportMonthlyOverview: TAction;

    // Menus
    MainMenu: TMainMenu;
  public
    {$IFDEF DARWIN}
    AppMenu: TMenuItem;
    AppAboutCmd: TMenuItem;
    AppSep1Cmd: TMenuItem;
    AppPrefCmd: TMenuItem;
    {$ELSE DARWIN}
    FileMenu: TMenuItem;
    FileExitMenuItem: TMenuItem;
    {$ENDIF DARWIN}
  published
    ReportsMenu: TMenuItem;
    ReportOpenItemsMenuItem: TMenuItem;
    ReportPaymentReceiptsMenuItem: TMenuItem;
    ReportCustomerRevenueMenuItem: TMenuItem;
    ReportMonthlyOverviewMenuItem: TMenuItem;
  public
    {$IFNDEF DARWIN}
    HelpMenu: TMenuItem;
    HelpAboutMenuItem: TMenuItem;
    {$ENDIF}
  published
    // Layout Panels
    TopPanel: TPanel;
    ContentPanel: TPanel;
    LeftPanel: TPanel;
    LeftSplitter: TSplitter;
    RightPanel: TPanel;
    CustomerSummaryPanel: TPanel;
    InvoiceListPanel: TPanel;
    StatusPanel: TPanel;
    StatusLabel: TLabel;

    // QuickInfo Labels
    QuickInfoCustomerCount: TLabel;
    QuickInfoSeparator1: TLabel;
    QuickInfoOpenItems: TLabel;
    QuickInfoSeparator2: TLabel;
    QuickInfoDueToday: TLabel;
    QuickInfoSeparator3: TLabel;
    QuickInfoOverdue: TLabel;

    // CustomerSummary Labels
    CustomerSummaryName: TLabel;
    CustomerSummaryStats: TLabel;

    // Methods
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpAboutActionExecute(Sender: TObject);
    procedure ReportOpenItemsExecute(Sender: TObject);
    procedure ReportPaymentReceiptsExecute(Sender: TObject);
    procedure ReportCustomerRevenueExecute(Sender: TObject);
    procedure ReportMonthlyOverviewExecute(Sender: TObject);

  private
    FCustomerListForm: TCustomerListForm;
    FInvoiceListForm: TInvoiceListForm;
    FCurrentCustomerID: longint;
    procedure ShowCustomerList;
    procedure ShowInvoiceList;
    procedure UpdateStatusBar(const AText: string);
    procedure UpdateQuickInfo;
    procedure UpdateCustomerSummary;
    procedure UpdateInvoiceList(CustomerID: longint);
    procedure HandleCustomerSelected(Sender: TObject; CustomerID: longint);
    procedure HandlePaymentCompleted(Sender: TObject);

  public
    CustomerService: ICustomerService;
    CustomerOrderService: ICustomerOrderService;
  end;

var
  MainForm: TMainForm;

implementation

uses
  rgAbout, rgConst, rgReportOpenItems, rgReportPayments, rgReportRevenue,
  rgReportMonthly;

{$R *.dfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  {$IFDEF DARWIN}
  AppMenu := TMenuItem.Create(Self);
  AppMenu.Caption := #$EF#$A3#$BF;  // Unicode Apple logo char
  MainMenu.Items.Insert(0, AppMenu);

  AppAboutCmd := TMenuItem.Create(AppMenu);
  AppAboutCmd.Action := HelpAboutAction;
  AppMenu.Add(AppAboutCmd);

  AppSep1Cmd := TMenuItem.Create(AppMenu);
  AppSep1Cmd.Caption := '-';
  AppMenu.Add(AppSep1Cmd);

  AppPrefCmd := TMenuItem.Create(AppMenu);
  AppPrefCmd.Caption := 'Preferences...';
  AppMenu.Add(AppPrefCmd);
  {$ELSE DARWIN}
  FileMenu := TMenuItem.Create(Self);
  FileMenu.Caption := '&File';
  MainMenu.Items.Insert(0, FileMenu);
  FileExitMenuItem := TMenuItem.Create(FileMenu);
  FileExitMenuItem.Action := FileExit;
  FileMenu.Add(FileExitMenuItem);

  HelpMenu := TMenuItem.Create(Self);
  HelpMenu.Caption := '&Help';
  MainMenu.Items.Add(HelpMenu);
  HelpAboutMenuItem := TMenuItem.Create(HelpMenu);
  HelpAboutMenuItem.Action := HelpAboutAction;
  HelpMenu.Add(HelpAboutMenuItem);
  {$ENDIF DARWIN}

  // Set Services
  CustomerService := TCustomerService.Create;
  CustomerOrderService := TCustomerOrderService.Create;

  // Initialize forms
  FCustomerListForm := nil;
  FInvoiceListForm := nil;
  FCurrentCustomerID := 0;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if FCustomerListForm <> nil then
    FreeAndNil(FCustomerListForm);
  if FInvoiceListForm <> nil then
    FreeAndNil(FInvoiceListForm);

  CustomerService := nil;
  CustomerOrderService := nil;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  UpdateQuickInfo;
  ShowCustomerList;
  ShowInvoiceList;
  UpdateStatusBar('');
end;

procedure TMainForm.ShowCustomerList;
begin
  if FCustomerListForm = nil then
  begin
    FCustomerListForm := TCustomerListForm.Create(Self, LeftPanel);
    FCustomerListForm.OnCustomerSelected := HandleCustomerSelected;
    FCustomerListForm.Show;
  end;
end;

procedure TMainForm.ShowInvoiceList;
begin
  if FInvoiceListForm = nil then
  begin
    FInvoiceListForm := TInvoiceListForm.Create(Self, InvoiceListPanel);
    FInvoiceListForm.OnPaymentCompleted := HandlePaymentCompleted;
    FInvoiceListForm.Show;
  end;
end;

procedure TMainForm.UpdateStatusBar(const AText: string);
begin
  StatusLabel.Caption := AText;
end;

procedure TMainForm.UpdateQuickInfo;
var
  Stats: TDtoDashboardStats;
begin
  RgServices.StatisticsService.GetDashboardStats(Stats);

  QuickInfoCustomerCount.Caption := Format('%d Customers', [Stats.CustomerCount]);
  QuickInfoOpenItems.Caption := Format('Open: %d (%.2n)', [Stats.OpenItemsCount, Stats.OpenItemsAmount]);
  QuickInfoDueToday.Caption := Format('Due: %d', [Stats.DueTodayCount]);
  QuickInfoOverdue.Caption := Format('Overdue: %d', [Stats.OverdueCount]);

  // Highlight overdue if > 0
  if Stats.OverdueCount > 0 then
    QuickInfoOverdue.Font.Color := clRed
  else
    QuickInfoOverdue.Font.Color := clDefault;
end;

procedure TMainForm.HandleCustomerSelected(Sender: TObject; CustomerID: longint);
begin
  FCurrentCustomerID := CustomerID;
  UpdateCustomerSummary;
  UpdateInvoiceList(CustomerID);
end;

procedure TMainForm.UpdateInvoiceList(CustomerID: longint);
begin
  if FInvoiceListForm <> nil then
  begin
    if CustomerID > 0 then
      FInvoiceListForm.LoadForCustomer(CustomerID)
    else
      FInvoiceListForm.ClearList;
  end;
end;

procedure TMainForm.UpdateCustomerSummary;
var
  Summary: TDtoCustomerSummary;
  StatsText: string;
begin
  if FCurrentCustomerID > 0 then
  begin
    RgServices.StatisticsService.GetCustomerSummary(FCurrentCustomerID, Summary);
    CustomerSummaryName.Caption := Summary.CustomerName;
    StatsText := Format('%d Invoices | Revenue: %.2n | %d open (%.2n) | %d paid',
      [Summary.InvoiceCount,
       Summary.TotalRevenue,
       Summary.OpenCount,
       Summary.OpenAmount,
       Summary.PaidCount]);
    CustomerSummaryStats.Caption := StatsText;
  end
  else
  begin
    CustomerSummaryName.Caption := '';
    CustomerSummaryStats.Caption := '';
  end;
end;

procedure TMainForm.HandlePaymentCompleted(Sender: TObject);
begin
  UpdateQuickInfo;

  if FCurrentCustomerID > 0 then
    UpdateCustomerSummary;
end;

procedure TMainForm.HelpAboutActionExecute(Sender: TObject);
begin
  ShowAboutBox;
end;

procedure TMainForm.ReportOpenItemsExecute(Sender: TObject);
var
  ReportForm: TOpenItemsReportForm;
begin
  ReportForm := TOpenItemsReportForm.Create(Self);
  try
    ReportForm.ShowReport;
  finally
    ReportForm.Free;
  end;
end;

procedure TMainForm.ReportPaymentReceiptsExecute(Sender: TObject);
var
  ReportForm: TPaymentReceiptsReportForm;
begin
  ReportForm := TPaymentReceiptsReportForm.Create(Self);
  try
    ReportForm.ShowReport;
  finally
    ReportForm.Free;
  end;
end;

procedure TMainForm.ReportCustomerRevenueExecute(Sender: TObject);
var
  ReportForm: TCustomerRevenueReportForm;
begin
  ReportForm := TCustomerRevenueReportForm.Create(Self);
  try
    ReportForm.ShowReport;
  finally
    ReportForm.Free;
  end;
end;

procedure TMainForm.ReportMonthlyOverviewExecute(Sender: TObject);
var
  ReportForm: TMonthlyOverviewReportForm;
begin
  ReportForm := TMonthlyOverviewReportForm.Create(Self);
  try
    ReportForm.ShowReport;
  finally
    ReportForm.Free;
  end;
end;

end.
