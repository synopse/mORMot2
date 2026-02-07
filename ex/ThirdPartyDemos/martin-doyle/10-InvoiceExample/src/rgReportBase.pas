{:
———————————————————————————————————————————————— © martindoyle 2017-2026 ——
 Project : Rechnung

 Using mORMot2
     Synopse mORMot2 framework. Copyright (C) 2025 Arnaud Bouchez
     Synopse Informatique - http://synopse.info

  Module : rgReportBase.pas

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
unit rgReportBase;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, MdForms, mdLayout, mdGrids;

type

  { TReportBaseForm }

  TReportBaseForm = class(TMDDBModeForm)
    FilterPanel: TPanel;
    CloseButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
  private
    FBaseHeight: Integer;
  protected
    FResultGrid: TMDListGrid;
    function GetBaseHeight: Integer;
    procedure SetupBaseLayout; virtual;
    procedure ConfigureColumns; virtual; abstract;
    procedure LoadData; virtual; abstract;
    function GetReportTitle: string; virtual; abstract;
  public
    function GetFormMenu: TMainMenu; override;
    procedure RefreshReport;
    function ShowReport: Boolean;
  end;

var
  ReportBaseForm: TReportBaseForm;

implementation

{$R *.dfm}

{ TReportBaseForm }

procedure TReportBaseForm.FormCreate(Sender: TObject);
begin
  FBaseHeight := GetBaseHeight;

  // Create result grid dynamically (BUG-004 fix pattern)
  FResultGrid := TMDListGrid.Create(Self);
  FResultGrid.Parent := Self;
  FResultGrid.Align := alClient;
  FResultGrid.RowSelect := True;

  ConfigureColumns;
  SetupBaseLayout;
end;

procedure TReportBaseForm.FormDestroy(Sender: TObject);
begin
  // Grid is freed automatically as child component
end;

function TReportBaseForm.GetBaseHeight: Integer;
begin
  Result := Canvas.TextHeight('Ag');
  if Result < 16 then
    Result := 16;
end;

procedure TReportBaseForm.SetupBaseLayout;
var
  Layout: TLayoutHelper;
  Margins: TLayoutMargins;
begin
  Margins := LayoutMargins(FBaseHeight);
  Layout := TLayoutHelper.Create(Self, Margins);
  try
    Layout.AdjustForPlatform;

    // Filter panel at top
    FilterPanel.Align := alTop;
    FilterPanel.Height := Round(3 * FBaseHeight);
    FilterPanel.BevelOuter := bvNone;

    // Close button at bottom right
    CloseButton.Top := ClientHeight - Margins.Bottom - CloseButton.Height;
    CloseButton.Left := ClientWidth - Margins.Right - CloseButton.Width;
    CloseButton.Anchors := [akRight, akBottom];

    // Set form title
    Caption := GetReportTitle;

    Position := poMainFormCenter;
  finally
    Layout.Free;
  end;
end;

procedure TReportBaseForm.CloseButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TReportBaseForm.RefreshReport;
begin
  FResultGrid.Items.BeginUpdate;
  try
    FResultGrid.Items.Clear;
    LoadData;
  finally
    FResultGrid.Items.EndUpdate;
  end;
end;

function TReportBaseForm.ShowReport: Boolean;
begin
  RefreshReport;
  Result := ShowModal = mrCancel;
end;

function TReportBaseForm.GetFormMenu: TMainMenu;
begin
  Result := nil;
end;

end.
