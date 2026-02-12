{:
---------------------------------------------------(C) martindoyle 2017-2026 --
 Project : Rechnung

 Using mORMot2
     Synopse mORMot2 framework. Copyright (C) 2025 Arnaud Bouchez
     Synopse Informatique - http://synopse.info

  Module : rgPaymentEntry.pas

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
--------------------------------------------------------------------------------
}
unit rgPaymentEntry;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, MdForms, mdLayout, rgDtoTypes, rgClient;

type

  { TPaymentEntryForm }

  TPaymentEntryForm = class(TMDDBModeForm)
    LabelInvoice: TLabel;
    LabelInvoiceNo: TLabel;
    LabelOpenAmount: TLabel;
    LabelOpenValue: TLabel;
    LabelAmount: TLabel;
    EditAmount: TEdit;
    LabelDate: TLabel;
    EditDate: TEdit;
    SaveButton: TButton;
    CancelButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure EditAmountKeyPress(Sender: TObject; var Key: char);
  private
    FInvoiceID: longint;
    FInvoiceNo: string;
    FOpenAmount: currency;
    FPaymentAmount: currency;
    FPaymentDate: TDateTime;
    FPaymentSuccessful: Boolean;
    procedure SetupLayout;
    function ValidateInput: Boolean;
  public
    function GetFormMenu: TMainMenu; override;
    function ShowPaymentEntry(AInvoiceID: longint; const AInvoiceNo: string;
      AOpenAmount: currency): Boolean;
    property PaymentAmount: currency read FPaymentAmount;
    property PaymentDate: TDateTime read FPaymentDate;
  end;

var
  PaymentEntryForm: TPaymentEntryForm;

implementation

uses
  mormot.core.base,
  mormot.core.text,
  mormot.core.unicode,
  mdDates,
  mdNumbers;

{$R *.dfm}

{ TPaymentEntryForm }

procedure TPaymentEntryForm.FormCreate(Sender: TObject);
begin
  FInvoiceID := 0;
  FPaymentSuccessful := False;
  SetupLayout;
end;

procedure TPaymentEntryForm.SetupLayout;
var
  Layout: TLayoutHelper;
  Margins: TLayoutMargins;
  LabelWidth, EditWidth: Integer;
  BaseHeight: Integer;
begin
  BaseHeight := LabelInvoice.Height;
  Margins := LayoutMargins(BaseHeight);
  Layout := TLayoutHelper.Create(Self, Margins);
  try
    Layout.AdjustForPlatform;

    LabelWidth := LabelOpenAmount.Width;
    EditWidth := Round(10 * BaseHeight);

    LabelInvoice.Width := LabelWidth;
    LabelOpenAmount.Width := LabelWidth;
    LabelAmount.Width := LabelWidth;
    LabelDate.Width := LabelWidth;

    EditAmount.Width := EditWidth;
    EditDate.Width := EditWidth;

    LabelInvoice.SetBounds(Margins.Left, Margins.Top,
      LabelWidth, LabelInvoice.Height);

    Layout.Place(LabelInvoice, LabelInvoiceNo, ldRight, 0.5);

    Layout.Place(LabelInvoice, LabelOpenAmount, ldBelow, 1.0);
    Layout.Place(LabelOpenAmount, LabelOpenValue, ldRight, 0.5);

    Layout.Place(LabelOpenAmount, LabelAmount, ldBelow, 1.5);
    Layout.Place(LabelAmount, EditAmount, ldRight, 0.5);

    Layout.Place(LabelAmount, LabelDate, ldBelow, 1.0);
    Layout.Place(LabelDate, EditDate, ldRight, 0.5);

    Layout.AutoSizeForm;
    Position := poMainFormCenter;
    ClientHeight := ClientHeight + CancelButton.Height + Margins.Bottom;

    // Place OK button at bottom-right
    CancelButton.SetBounds(
      ClientWidth - Margins.Right - CancelButton.Width,
      ClientHeight - Margins.Bottom - CancelButton.Height,
      CancelButton.Width,
      CancelButton.Height
    );
    Layout.Place(CancelButton, SaveButton, ldLeft, 0.5);
  finally
    Layout.Free;
  end;
end;

procedure TPaymentEntryForm.FormDestroy(Sender: TObject);
begin
  // nothing to free - services accessed via RgServices global
end;

procedure TPaymentEntryForm.EditAmountKeyPress(Sender: TObject; var Key: char);
begin
  FilterNumericKey(Key, False);
end;

function TPaymentEntryForm.ValidateInput: Boolean;
var
  Amount: currency;
  PayDate: TDateTime;
begin
  Result := False;

  if not TryStrToCurr(EditAmount.Text, Amount) then
  begin
    ShowMessage('Please enter a valid amount.');
    EditAmount.SetFocus;
    Exit;
  end;

  if Amount <= 0 then
  begin
    ShowMessage('Amount must be greater than zero.');
    EditAmount.SetFocus;
    Exit;
  end;

  if Amount > FOpenAmount then
  begin
    if MessageDlg('Amount ' + FormatCurr(FMT_CURR_EDIT, Amount) +
        ' exceeds open amount ' + FormatCurr(FMT_CURR_EDIT, FOpenAmount) +
        '. Continue?',
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    begin
      EditAmount.SetFocus;
      Exit;
    end;
  end;

  if not AppTryStrToDate(EditDate.Text, PayDate) then
  begin
    ShowMessage(Format('Please enter a valid date (%s).', [AppDateFormatHint]));
    EditDate.SetFocus;
    Exit;
  end;

  FPaymentAmount := Amount;
  FPaymentDate := PayDate;
  Result := True;
end;

procedure TPaymentEntryForm.SaveButtonClick(Sender: TObject);
var
  PayResult: TPaymentResult;
begin
  if not ValidateInput then
    Exit;

  PayResult := RgServices.PaymentService.AddPayment(FInvoiceID, FPaymentAmount, FPaymentDate);

  case PayResult of
    prSuccess:
    begin
      FPaymentSuccessful := True;
      ModalResult := mrOk;
    end;
    prInvoiceNotFound:
      ShowMessage('Invoice not found.');
    prInvalidAmount:
      ShowMessage('Invalid amount.');
    prDatabaseError:
      ShowMessage('Database error. Payment could not be saved.');
  end;
end;

procedure TPaymentEntryForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

function TPaymentEntryForm.ShowPaymentEntry(AInvoiceID: longint;
  const AInvoiceNo: string; AOpenAmount: currency): Boolean;
begin
  FInvoiceID := AInvoiceID;
  FInvoiceNo := AInvoiceNo;
  FOpenAmount := AOpenAmount;
  FPaymentSuccessful := False;

  LabelInvoiceNo.Caption := AInvoiceNo;
  LabelOpenValue.Caption := FormatCurr(FMT_CURR_DISPLAY, AOpenAmount);
  EditAmount.Text := FormatCurr(FMT_CURR_EDIT, AOpenAmount);
  EditDate.Text := AppDateToStr(Date);

  EditAmount.SelectAll;

  Result := (ShowModal = mrOk) and FPaymentSuccessful;
end;

function TPaymentEntryForm.GetFormMenu: TMainMenu;
begin
  Result := nil;
end;

end.
