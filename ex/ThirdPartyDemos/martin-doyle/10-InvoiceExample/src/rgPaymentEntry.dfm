object PaymentEntryForm: TPaymentEntryForm
  Left = 400
  Top = 250
  BorderStyle = bsDialog
  Caption = 'Record Payment'
  ClientHeight = 250
  ClientWidth = 350
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LabelInvoice: TLabel
    Left = 16
    Top = 20
    Width = 38
    Height = 13
    Caption = 'Invoice:'
  end
  object LabelInvoiceNo: TLabel
    Left = 130
    Top = 20
    Width = 13
    Height = 13
    Caption = '---'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelOpenAmount: TLabel
    Left = 16
    Top = 52
    Width = 68
    Height = 13
    Caption = 'Open Amount:'
  end
  object LabelOpenValue: TLabel
    Left = 130
    Top = 52
    Width = 26
    Height = 13
    Caption = '0.00'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelAmount: TLabel
    Left = 16
    Top = 100
    Width = 39
    Height = 13
    Caption = 'Amount:'
  end
  object LabelDate: TLabel
    Left = 16
    Top = 132
    Width = 26
    Height = 13
    Caption = 'Date:'
  end
  object EditAmount: TEdit
    Left = 66
    Top = 40
    Width = 150
    Height = 21
    TabOrder = 0
    OnKeyPress = EditAmountKeyPress
  end
  object EditDate: TEdit
    Left = 50
    Top = 80
    Width = 150
    Height = 21
    TabOrder = 1
  end
  object SaveButton: TButton
    Left = 112
    Top = 76
    Width = 75
    Height = 25
    Caption = '&Save'
    Default = True
    TabOrder = 2
    OnClick = SaveButtonClick
  end
  object CancelButton: TButton
    Left = 194
    Top = 76
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 3
    OnClick = CancelButtonClick
  end
end
