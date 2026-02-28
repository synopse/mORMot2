object InvoiceEditForm: TInvoiceEditForm
  Left = 400
  Top = 200
  BorderStyle = bsDialog
  Caption = 'Invoice'
  ClientHeight = 319
  ClientWidth = 475
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Style = []
  OldCreateOrder = True
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LabelCustomer: TLabel
    Left = 16
    Top = 20
    Width = 47
    Height = 13
    Caption = 'Customer:'
  end
  object LabelCustomerValue: TLabel
    Left = 140
    Top = 20
    Width = 13
    Height = 13
    Caption = '---'
    Font.Color = clWindowText
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelOrderNo: TLabel
    Left = 16
    Top = 52
    Width = 55
    Height = 13
    Caption = 'Invoice No:'
  end
  object LabelSaleDate: TLabel
    Left = 16
    Top = 84
    Width = 50
    Height = 13
    Caption = 'Sale Date:'
  end
  object LabelShipDate: TLabel
    Left = 280
    Top = 84
    Width = 49
    Height = 13
    Caption = 'Due Date:'
  end
  object LabelTotalValue: TLabel
    Left = 300
    Top = 195
    Width = 26
    Height = 13
    Alignment = taRightJustify
    Caption = '0.00'
    Font.Color = clWindowText
    Font.Style = [fsBold]
    ParentFont = False
  end
  object EditOrderNo: TEdit
    Left = 140
    Top = 48
    Width = 150
    Height = 21
    TabOrder = 0
  end
  object EditSaleDate: TEdit
    Left = 140
    Top = 80
    Width = 100
    Height = 21
    TabOrder = 1
  end
  object EditShipDate: TEdit
    Left = 360
    Top = 80
    Width = 100
    Height = 21
    TabOrder = 2
  end
  object ItemsToolbarPanel: TPanel
    Left = 16
    Top = 120
    Width = 618
    Height = 40
    BevelOuter = bvNone
    TabOrder = 3
    object AddItemButton: TButton
      Left = 0
      Top = 8
      Width = 80
      Height = 25
      Caption = '&Add'
      TabOrder = 0
      OnClick = AddItemButtonClick
    end
    object EditItemButton: TButton
      Left = 88
      Top = 8
      Width = 80
      Height = 25
      Caption = '&Edit'
      Enabled = False
      TabOrder = 1
      OnClick = EditItemButtonClick
    end
    object RemoveItemButton: TButton
      Left = 176
      Top = 8
      Width = 80
      Height = 25
      Caption = '&Remove'
      Enabled = False
      TabOrder = 2
      OnClick = RemoveItemButtonClick
    end
  end
  object SaveButton: TButton
    Left = 156
    Top = 240
    Width = 80
    Height = 28
    Caption = '&Save'
    Default = True
    TabOrder = 5
    OnClick = SaveButtonClick
  end
  object CancelButton: TButton
    Left = 250
    Top = 240
    Width = 80
    Height = 28
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 4
    OnClick = CancelButtonClick
  end
end
