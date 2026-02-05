object PaymentEntryForm: TPaymentEntryForm
  Left = 400
  Height = 250
  Top = 250
  Width = 350
  BorderStyle = bsDialog
  Caption = 'Record Payment'
  ClientHeight = 250
  ClientWidth = 350
  Position = poMainFormCenter
  LCLVersion = '4.5.0.0'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  object LabelInvoice: TLabel
    Left = 16
    Height = 16
    Top = 20
    Width = 50
    Caption = 'Invoice:'
  end
  object LabelInvoiceNo: TLabel
    Left = 130
    Height = 16
    Top = 20
    Width = 50
    Caption = '---'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelOpenAmount: TLabel
    Left = 16
    Height = 16
    Top = 52
    Width = 80
    Caption = 'Open Amount:'
  end
  object LabelOpenValue: TLabel
    Left = 130
    Height = 16
    Top = 52
    Width = 50
    Caption = '0.00'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelAmount: TLabel
    Left = 16
    Height = 16
    Top = 100
    Width = 55
    Caption = 'Amount:'
  end
  object EditAmount: TEdit
    Left = 130
    Height = 21
    Top = 96
    Width = 150
    TabOrder = 0
    OnKeyPress = EditAmountKeyPress
  end
  object LabelDate: TLabel
    Left = 16
    Height = 16
    Top = 132
    Width = 32
    Caption = 'Date:'
  end
  object EditDate: TEdit
    Left = 130
    Height = 21
    Top = 128
    Width = 150
    TabOrder = 1
  end
  object SaveButton: TButton
    Left = 160
    Height = 25
    Top = 180
    Width = 75
    Caption = '&Save'
    Default = True
    TabOrder = 2
    OnClick = SaveButtonClick
  end
  object CancelButton: TButton
    Left = 250
    Height = 25
    Top = 180
    Width = 75
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 3
    OnClick = CancelButtonClick
  end
end
