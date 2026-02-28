object InvoiceItemEditForm: TInvoiceItemEditForm
  Left = 400
  Height = 175
  Top = 250
  Width = 389
  BorderStyle = bsDialog
  Caption = 'Invoice Item'
  ClientHeight = 175
  ClientWidth = 389
  Color = clBtnFace
  Font.Color = clWindowText
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnShow = FormShow
  object LabelDescription: TLabel
    Left = 16
    Height = 18
    Top = 20
    Width = 91
    Caption = 'Description:'
  end
  object LabelQuantity: TLabel
    Left = 16
    Height = 18
    Top = 52
    Width = 69
    Caption = 'Quantity:'
  end
  object LabelPrice: TLabel
    Left = 16
    Height = 18
    Top = 84
    Width = 42
    Caption = 'Price:'
  end
  object LabelDiscount: TLabel
    Left = 16
    Height = 18
    Top = 116
    Width = 71
    Caption = 'Discount:'
  end
  object LabelPercent: TLabel
    Left = 176
    Height = 18
    Top = 116
    Width = 15
    Caption = '%'
  end
  object EditDescription: TEdit
    Left = 112
    Height = 26
    Top = 16
    Width = 260
    TabOrder = 0
  end
  object EditQuantity: TEdit
    Left = 110
    Height = 26
    Top = 50
    Width = 100
    TabOrder = 1
    OnKeyPress = EditQuantityKeyPress
  end
  object EditPrice: TEdit
    Left = 110
    Height = 26
    Top = 82
    Width = 120
    TabOrder = 2
    OnKeyPress = EditPriceKeyPress
  end
  object SpinDiscount: TSpinEdit
    Left = 110
    Height = 27
    Top = 114
    Width = 60
    MaxValue = 100
    TabOrder = 3
  end
  object OKButton: TButton
    Left = 162
    Height = 25
    Top = 64
    Width = 75
    Caption = '&OK'
    Default = True
    TabOrder = 4
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 247
    Height = 25
    Top = 64
    Width = 75
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 5
    OnClick = CancelButtonClick
  end
end
