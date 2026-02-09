object InvoiceItemEditForm: TInvoiceItemEditForm
  Left = 400
  Height = 220
  Top = 250
  Width = 400
  BorderStyle = bsDialog
  Caption = 'Invoice Item'
  ClientHeight = 220
  ClientWidth = 400
  Position = poMainFormCenter
  OnCreate = FormCreate
  object LabelDescription: TLabel
    Left = 16
    Height = 16
    Top = 20
    Width = 70
    Caption = 'Description:'
  end
  object EditDescription: TEdit
    Left = 110
    Height = 21
    Top = 18
    Width = 260
    TabOrder = 0
  end
  object LabelQuantity: TLabel
    Left = 16
    Height = 16
    Top = 52
    Width = 55
    Caption = 'Quantity:'
  end
  object EditQuantity: TEdit
    Left = 110
    Height = 21
    Top = 50
    Width = 100
    TabOrder = 1
    OnKeyPress = EditQuantityKeyPress
  end
  object LabelPrice: TLabel
    Left = 16
    Height = 16
    Top = 84
    Width = 35
    Caption = 'Price:'
  end
  object EditPrice: TEdit
    Left = 110
    Height = 21
    Top = 82
    Width = 120
    TabOrder = 2
    OnKeyPress = EditPriceKeyPress
  end
  object LabelDiscount: TLabel
    Left = 16
    Height = 16
    Top = 116
    Width = 55
    Caption = 'Discount:'
  end
  object SpinDiscount: TSpinEdit
    Left = 110
    Height = 21
    Top = 114
    Width = 60
    MaxValue = 100
    MinValue = 0
    TabOrder = 3
  end
  object LabelPercent: TLabel
    Left = 176
    Height = 16
    Top = 116
    Width = 15
    Caption = '%'
  end
  object OKButton: TButton
    Left = 210
    Height = 25
    Top = 160
    Width = 75
    Caption = '&OK'
    Default = True
    TabOrder = 4
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 295
    Height = 25
    Top = 160
    Width = 75
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 5
    OnClick = CancelButtonClick
  end
end
