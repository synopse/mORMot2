object InvoiceEditForm: TInvoiceEditForm
  Left = 400
  Height = 550
  Top = 200
  Width = 650
  BorderStyle = bsDialog
  Caption = 'Invoice'
  ClientHeight = 550
  ClientWidth = 650
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  object LabelCustomer: TLabel
    Left = 16
    Height = 16
    Top = 20
    Width = 60
    Caption = 'Customer:'
  end
  object LabelCustomerValue: TLabel
    Left = 140
    Height = 16
    Top = 20
    Width = 100
    Caption = '---'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelOrderNo: TLabel
    Left = 16
    Height = 16
    Top = 52
    Width = 70
    Caption = 'Invoice No:'
  end
  object EditOrderNo: TEdit
    Left = 140
    Height = 21
    Top = 48
    Width = 150
    TabOrder = 0
  end
  object LabelSaleDate: TLabel
    Left = 16
    Height = 16
    Top = 84
    Width = 60
    Caption = 'Sale Date:'
  end
  object EditSaleDate: TEdit
    Left = 140
    Height = 21
    Top = 80
    Width = 100
    TabOrder = 1
  end
  object LabelShipDate: TLabel
    Left = 280
    Height = 16
    Top = 84
    Width = 60
    Caption = 'Due Date:'
  end
  object EditShipDate: TEdit
    Left = 360
    Height = 21
    Top = 80
    Width = 100
    TabOrder = 2
  end
  object ItemsToolbarPanel: TPanel
    Left = 16
    Height = 40
    Top = 120
    Width = 618
    BevelOuter = bvNone
    TabOrder = 3
    object AddItemButton: TButton
      Left = 0
      Height = 25
      Top = 8
      Width = 80
      Caption = '&Add'
      TabOrder = 0
      OnClick = AddItemButtonClick
    end
    object EditItemButton: TButton
      Left = 88
      Height = 25
      Top = 8
      Width = 80
      Caption = '&Edit'
      Enabled = False
      TabOrder = 1
      OnClick = EditItemButtonClick
    end
    object RemoveItemButton: TButton
      Left = 176
      Height = 25
      Top = 8
      Width = 80
      Caption = '&Remove'
      Enabled = False
      TabOrder = 2
      OnClick = RemoveItemButtonClick
    end
  end
  object LabelTotal: TLabel
    Left = 450
    Height = 16
    Top = 435
    Width = 40
    Caption = 'Total:'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelTotalValue: TLabel
    Left = 550
    Height = 16
    Top = 435
    Width = 80
    Alignment = taRightJustify
    Caption = '0.00'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object SaveButton: TButton
    Left = 460
    Height = 28
    Top = 480
    Width = 80
    Caption = '&Save'
    Default = True
    TabOrder = 5
    OnClick = SaveButtonClick
  end
  object CancelButton: TButton
    Left = 554
    Height = 28
    Top = 480
    Width = 80
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 6
    OnClick = CancelButtonClick
  end
end
