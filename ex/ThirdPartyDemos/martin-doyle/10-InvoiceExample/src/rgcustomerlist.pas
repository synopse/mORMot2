{:
———————————————————————————————————————————————— © martindoyle 2017-2026 ——
 Project : Rechnung

 Using mORMot2
     Synopse mORMot2 framework. Copyright (C) 2025 Arnaud Bouchez
     Synopse Informatique - http://synopse.info

  Module : rgCustomerList.pas

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
unit rgCustomerList;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Menus, MdForms, mdGrids, rgClient, rgDtoTypes;

type
  TCustomerSelectedEvent = procedure(Sender: TObject; CustomerID: longint) of object;

  { TCustomerListForm }

  TCustomerListForm = class(TMDChildForm)
    ToolbarPanel: TPanel;
    NewButton: TButton;
    EditButton: TButton;
    DeleteButton: TButton;
    SearchEdit: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure NewButtonClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure SearchEditChange(Sender: TObject);
  private
    FCustomerService: ICustomerService;
    FOnCustomerSelected: TCustomerSelectedEvent;
    FSearchFilter: string;
    FBaseHeight: Integer;
    FCustomerListGrid: TMDListGrid;
    procedure LoadCustomers;
    procedure SetListGridColumns;
    procedure UpdateButtons;
    procedure UpdateColumnWidths;
    procedure SetupLayout;
    function GetBaseHeight: Integer;
    function GetSelectedCustomerID: longint;
    procedure DoCustomerSelected(CustomerID: longint);
    procedure CustomerListGridSelectItem(Sender: TObject; Item: TMDListItem;
      Selected: Boolean);
    procedure CustomerListGridDblClick(Sender: TObject);
  public
    function GetFormMenu: TMainMenu; override;
    procedure RefreshList;
    property OnCustomerSelected: TCustomerSelectedEvent read FOnCustomerSelected write FOnCustomerSelected;
  end;

var
  CustomerListForm: TCustomerListForm;

implementation

uses
  mormot.core.base,
  mormot.core.text,
  mormot.core.unicode,
  rgCustomerEdit;

{$R *.dfm}

{ TCustomerListForm }

procedure TCustomerListForm.FormCreate(Sender: TObject);
begin
  FCustomerService := TCustomerService.Create;
  FSearchFilter := '';
  FBaseHeight := GetBaseHeight;

  // Create TMDListGrid (replaces TListView for BUG-004 fix)
  FCustomerListGrid := TMDListGrid.Create(Self);
  FCustomerListGrid.Parent := Self;
  FCustomerListGrid.Align := alClient;
  FCustomerListGrid.OnSelectItem := CustomerListGridSelectItem;
  FCustomerListGrid.OnDblClick := CustomerListGridDblClick;

  SetListGridColumns;
  SetupLayout;
end;

procedure TCustomerListForm.FormDestroy(Sender: TObject);
begin
  FCustomerService := nil;
end;

procedure TCustomerListForm.FormShow(Sender: TObject);
begin
  LoadCustomers;
  UpdateButtons;
end;

procedure TCustomerListForm.FormResize(Sender: TObject);
begin
  UpdateColumnWidths;
end;

function TCustomerListForm.GetBaseHeight: Integer;
begin
  Result := Canvas.TextHeight('Ag');
  if Result < 16 then
    Result := 16;
end;

procedure TCustomerListForm.SetupLayout;
var
  Margin: Integer;
  ButtonSpacing: Integer;
  SearchSpacing: Integer;
begin
  Margin := FBaseHeight div 2;
  ButtonSpacing := FBaseHeight div 2;
  SearchSpacing := FBaseHeight;

  NewButton.Left := Margin;
  NewButton.Top := (ToolbarPanel.Height - NewButton.Height) div 2;

  EditButton.Left := NewButton.Left + NewButton.Width + ButtonSpacing;
  EditButton.Top := NewButton.Top;

  DeleteButton.Left := EditButton.Left + EditButton.Width + ButtonSpacing;
  DeleteButton.Top := NewButton.Top;

  SearchEdit.Top := NewButton.Top;
  SearchEdit.Left := DeleteButton.Left + DeleteButton.Width + SearchSpacing;
  SearchEdit.Width := ToolbarPanel.ClientWidth - SearchEdit.Left - Margin;
  SearchEdit.Anchors := [akTop, akLeft, akRight];
end;

procedure TCustomerListForm.SetListGridColumns;
var
  Col: TMDListColumn;
begin
  FCustomerListGrid.RowSelect := True;

  // Column 0: Company
  Col := FCustomerListGrid.Columns.Add;
  Col.Caption := 'Company';
  Col.Width := Round(18 * FBaseHeight);

  // Column 1: City
  Col := FCustomerListGrid.Columns.Add;
  Col.Caption := 'City';
  Col.Width := Round(12 * FBaseHeight);
end;

procedure TCustomerListForm.UpdateColumnWidths;
var
  AvailableWidth: Integer;
  CompanyWidth: Integer;
  CityWidth: Integer;
  MinCompanyWidth: Integer;
  MinCityWidth: Integer;
begin
  if FCustomerListGrid.Columns.Count < 2 then Exit;

  MinCompanyWidth := Round(12 * FBaseHeight);
  MinCityWidth := Round(8 * FBaseHeight);

  AvailableWidth := FCustomerListGrid.Width - 24;

  CompanyWidth := (AvailableWidth * 60) div 100;
  CityWidth := AvailableWidth - CompanyWidth;

  if CompanyWidth < MinCompanyWidth then
    CompanyWidth := MinCompanyWidth;
  if CityWidth < MinCityWidth then
    CityWidth := MinCityWidth;

  FCustomerListGrid.Columns[0].Width := CompanyWidth;
  FCustomerListGrid.Columns[1].Width := CityWidth;
end;

procedure TCustomerListForm.LoadCustomers;
var
  Customer: TDtoCustomer;
  Item: TMDListItem;
  Count: integer;
  i: integer;
  FilterLower: RawUtf8;
  CompanyLower: RawUtf8;
  CityLower: RawUtf8;
  CustomerNoLower: RawUtf8;
  MatchesFilter: Boolean;
begin
  FCustomerListGrid.Items.BeginUpdate;
  try
    FCustomerListGrid.Items.Clear;
    FCustomerService.LoadCustomers;
    Count := FCustomerService.GetCustomerCount(False);
    FilterLower := LowerCaseUnicode(StringToUtf8(FSearchFilter));

    for i := 0 to Count - 1 do
    begin
      if FCustomerService.NextCustomer then
      begin
        Customer := FCustomerService.GetCustomer;

        if FilterLower <> '' then
        begin
          CompanyLower := LowerCaseUnicode(StringToUtf8(Customer.Company));
          CityLower := LowerCaseUnicode(StringToUtf8(Customer.City));
          CustomerNoLower := LowerCaseUnicode(StringToUtf8(Customer.CustomerNo));
          MatchesFilter := (PosEx(FilterLower, CompanyLower) > 0) or
                           (PosEx(FilterLower, CityLower) > 0) or
                           (PosEx(FilterLower, CustomerNoLower) > 0);
        end
        else
          MatchesFilter := True;

        if MatchesFilter then
        begin
          Item := FCustomerListGrid.Items.Add;
          Item.Caption := Customer.Company;
          Item.SubItems.Add(Customer.City);
          Item.Data := Pointer(PtrInt(Customer.CustomerID));
        end;
      end;
    end;
  finally
    FCustomerListGrid.Items.EndUpdate;
  end;
end;

procedure TCustomerListForm.UpdateButtons;
var
  HasSelection: Boolean;
begin
  HasSelection := FCustomerListGrid.Selected <> nil;
  EditButton.Enabled := HasSelection;
  DeleteButton.Enabled := HasSelection;
end;

function TCustomerListForm.GetSelectedCustomerID: longint;
begin
  Result := 0;
  if FCustomerListGrid.Selected <> nil then
    Result := longint(PtrInt(FCustomerListGrid.Selected.Data));
end;

procedure TCustomerListForm.DoCustomerSelected(CustomerID: longint);
begin
  if Assigned(FOnCustomerSelected) then
    FOnCustomerSelected(Self, CustomerID);
end;

procedure TCustomerListForm.CustomerListGridSelectItem(Sender: TObject;
  Item: TMDListItem; Selected: Boolean);
begin
  UpdateButtons;
  if Selected then
    DoCustomerSelected(GetSelectedCustomerID)
  else if FCustomerListGrid.Selected = nil then
    DoCustomerSelected(0);
end;

procedure TCustomerListForm.CustomerListGridDblClick(Sender: TObject);
begin
  if FCustomerListGrid.Selected <> nil then
    EditButtonClick(Sender);
end;

procedure TCustomerListForm.NewButtonClick(Sender: TObject);
var
  EditForm: TCustomerEditForm;
begin
  EditForm := TCustomerEditForm.Create(Self);
  try
    EditForm.NewCustomer;
    if EditForm.ModalResult = mrOk then
      RefreshList;
  finally
    EditForm.Free;
  end;
end;

procedure TCustomerListForm.EditButtonClick(Sender: TObject);
var
  CustomerID: longint;
  EditForm: TCustomerEditForm;
begin
  CustomerID := GetSelectedCustomerID;
  if CustomerID > 0 then
  begin
    EditForm := TCustomerEditForm.Create(Self);
    try
      EditForm.ShowCustomer(CustomerID);
      if EditForm.ModalResult = mrOk then
        RefreshList;
    finally
      EditForm.Free;
    end;
  end;
end;

procedure TCustomerListForm.DeleteButtonClick(Sender: TObject);
var
  CustomerID: longint;
begin
  CustomerID := GetSelectedCustomerID;
  if CustomerID > 0 then
  begin
    if MessageDlg('Are you sure you want to delete this customer?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      // TODO: Implement customer deletion
      ShowMessage('Delete customer ID: ' + IntToStr(CustomerID));
      RefreshList;
    end;
  end;
end;

procedure TCustomerListForm.SearchEditChange(Sender: TObject);
begin
  FSearchFilter := SearchEdit.Text;
  LoadCustomers;
  UpdateButtons;
end;

procedure TCustomerListForm.RefreshList;
begin
  LoadCustomers;
  UpdateButtons;
end;

function TCustomerListForm.GetFormMenu: TMainMenu;
begin
  Result := nil;
end;

end.
