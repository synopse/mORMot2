object PaymentEntryForm: TPaymentEntryForm
  Left = 400
  Height = 250
  Top = 250
  Width = 350
  BorderStyle = bsDialog
  Caption = 'Record Payment'
  ClientHeight = 250
  ClientWidth = 350
  Color = clBtnFace
  Font.Color = clWindowText
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  object LabelInvoice: TLabel
    Left = 16
    Height = 18
    Top = 20
    Width = 59
    Caption = 'Invoice:'
  end
  object LabelInvoiceNo: TLabel
    Left = 130
    Height = 18
    Top = 20
    Width = 19
    Caption = '---'
    Font.Color = clWindowText
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelOpenAmount: TLabel
    Left = 16
    Height = 18
    Top = 52
    Width = 109
    Caption = 'Open Amount:'
  end
  object LabelOpenValue: TLabel
    Left = 130
    Height = 18
    Top = 52
    Width = 37
    Caption = '0.00'
    Font.Color = clWindowText
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelAmount: TLabel
    Left = 16
    Height = 18
    Top = 100
    Width = 64
    Caption = 'Amount:'
  end
  object LabelDate: TLabel
    Left = 16
    Height = 18
    Top = 132
    Width = 41
    Caption = 'Date:'
  end
  object EditAmount: TEdit
    Left = 66
    Height = 26
    Top = 40
    Width = 150
    TabOrder = 0
    OnKeyPress = EditAmountKeyPress
  end
  object EditDate: TEdit
    Left = 50
    Height = 26
    Top = 80
    Width = 150
    TabOrder = 1
  end
  object SaveButton: TButton
    Left = 112
    Height = 25
    Top = 76
    Width = 75
    Caption = '&Save'
    Default = True
    TabOrder = 2
    OnClick = SaveButtonClick
  end
  object CancelButton: TButton
    Left = 194
    Height = 25
    Top = 76
    Width = 75
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 3
    OnClick = CancelButtonClick
  end
end
