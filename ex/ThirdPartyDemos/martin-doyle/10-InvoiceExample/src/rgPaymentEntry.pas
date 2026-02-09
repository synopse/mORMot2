{:
———————————————————————————————————————————————— (C) martindoyle 2017-2026 ——
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
————————————————————————————————————————————————————————————————————————————
}
unit rgPaymentEntry;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, MdForms, mdLayout, rgClient;

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
    FPaymentService: IPaymentService;
    FInvoiceID: longint;
    FInvoiceNo: string;
    FOpenAmount: currency;
    FPaymentAmount: currency;
    FPaymentDate: TDateTime;
    FPaymentSuccessful: Boolean;
    procedure SetupLayout;
    function ValidateInput: Boolean;
    function ParseAmount(const AText: string; out AAmount: currency): Boolean;
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
  mdDates;

{$R *.dfm}

{ TPaymentEntryForm }

procedure TPaymentEntryForm.FormCreate(Sender: TObject);
begin
  FPaymentService := TPaymentService.Create;
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

    LabelWidth := Round(8 * BaseHeight);
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

    SaveButton.Top := EditDate.Top + EditDate.Height + (2 * BaseHeight);
    CancelButton.Top := SaveButton.Top;

    CancelButton.Left := ClientWidth - Margins.Right - CancelButton.Width;
    SaveButton.Left := CancelButton.Left - Margins.Middle - SaveButton.Width;

    ClientHeight := CancelButton.Top + CancelButton.Height + Margins.Bottom;

    Position := poMainFormCenter;
  finally
    Layout.Free;
  end;
end;

procedure TPaymentEntryForm.FormDestroy(Sender: TObject);
begin
  FPaymentService := nil;
end;

procedure TPaymentEntryForm.EditAmountKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'..'9', ',', '.', #8, #13]) then
    Key := #0;
end;

function TPaymentEntryForm.ParseAmount(const AText: string;
  out AAmount: currency): Boolean;
var
  TempText: string;
begin
  Result := False;
  AAmount := 0;

  TempText := Trim(AText);
  if TempText = '' then
    Exit;

  TempText := StringReplace(TempText, ',', '.', [rfReplaceAll]);
  AAmount := StrToCurrency(pointer(StringToUtf8(TempText)));
  Result := (AAmount <> 0);
end;

function TPaymentEntryForm.ValidateInput: Boolean;
var
  Amount: currency;
  PayDate: TDateTime;
begin
  Result := False;

  if not ParseAmount(EditAmount.Text, Amount) then
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
    if MessageDlg('Amount ' + Curr64ToString(PInt64(@Amount)^) +
        ' exceeds open amount ' + Curr64ToString(PInt64(@FOpenAmount)^) +
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

  PayResult := FPaymentService.AddPayment(FInvoiceID, FPaymentAmount, FPaymentDate);

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
  LabelOpenValue.Caption := Curr64ToString(PInt64(@AOpenAmount)^);
  EditAmount.Text := Curr64ToString(PInt64(@AOpenAmount)^);
  EditDate.Text := AppDateToStr(Date);

  EditAmount.SelectAll;

  Result := (ShowModal = mrOk) and FPaymentSuccessful;
end;

function TPaymentEntryForm.GetFormMenu: TMainMenu;
begin
  Result := nil;
end;

end.
