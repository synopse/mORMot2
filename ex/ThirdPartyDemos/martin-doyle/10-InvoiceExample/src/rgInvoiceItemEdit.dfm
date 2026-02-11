object InvoiceItemEditForm: TInvoiceItemEditForm
  Left = 400
  Top = 250
  BorderStyle = bsDialog
  Caption = 'Invoice Item'
  ClientHeight = 175
  ClientWidth = 389
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LabelDescription: TLabel
    Left = 16
    Top = 20
    Width = 56
    Height = 13
    Caption = 'Description:'
  end
  object LabelQuantity: TLabel
    Left = 16
    Top = 52
    Width = 42
    Height = 13
    Caption = 'Quantity:'
  end
  object LabelPrice: TLabel
    Left = 16
    Top = 84
    Width = 27
    Height = 13
    Caption = 'Price:'
  end
  object LabelDiscount: TLabel
    Left = 16
    Top = 116
    Width = 45
    Height = 13
    Caption = 'Discount:'
  end
  object LabelPercent: TLabel
    Left = 176
    Top = 116
    Width = 8
    Height = 13
    Caption = '%'
  end
  object EditDescription: TEdit
    Left = 30
    Top = 26
    Width = 260
    Height = 21
    TabOrder = 0
  end
  object EditQuantity: TEdit
    Left = 110
    Top = 50
    Width = 100
    Height = 21
    TabOrder = 1
    OnKeyPress = EditQuantityKeyPress
  end
  object EditPrice: TEdit
    Left = 110
    Top = 82
    Width = 120
    Height = 21
    TabOrder = 2
    OnKeyPress = EditPriceKeyPress
  end
  object SpinDiscount: TSpinEdit
    Left = 110
    Top = 114
    Width = 60
    Height = 22
    MaxValue = 100
    MinValue = 0
    TabOrder = 3
    Value = 0
  end
  object OKButton: TButton
    Left = 162
    Top = 64
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    TabOrder = 4
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 247
    Top = 64
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 5
    OnClick = CancelButtonClick
  end
end
