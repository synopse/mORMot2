object InvoiceListForm: TInvoiceListForm
  Left = 363
  Height = 400
  Top = 222
  Width = 600
  Caption = 'Invoice List'
  ClientHeight = 400
  ClientWidth = 600
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  object ToolbarPanel: TPanel
    Left = 0
    Height = 40
    Top = 0
    Width = 600
    Align = alTop
    BevelOuter = bvNone
    ClientHeight = 40
    ClientWidth = 600
    TabOrder = 0
    object NewButton: TButton
      Left = 8
      Height = 25
      Top = 8
      Width = 75
      Caption = '&New'
      Enabled = False
      OnClick = NewButtonClick
      TabOrder = 0
    end
    object EditButton: TButton
      Left = 88
      Height = 25
      Top = 8
      Width = 75
      Caption = '&Edit'
      Enabled = False
      OnClick = EditButtonClick
      TabOrder = 1
    end
    object DeleteButton: TButton
      Left = 168
      Height = 25
      Top = 8
      Width = 75
      Caption = '&Delete'
      Enabled = False
      OnClick = DeleteButtonClick
      TabOrder = 2
    end
    object PayButton: TButton
      Left = 268
      Height = 25
      Top = 8
      Width = 75
      Caption = '&Pay'
      Enabled = False
      OnClick = PayButtonClick
      TabOrder = 3
    end
  end
  object LegendPanel: TPanel
    Left = 0
    Height = 30
    Top = 370
    Width = 600
    Align = alBottom
    BevelOuter = bvNone
    ClientHeight = 30
    ClientWidth = 600
    TabOrder = 2
    object LegendPaid: TLabel
      Left = 8
      Height = 16
      Top = 7
      Width = 48
      Caption = '[+] Paid'
    end
    object LegendOpen: TLabel
      Left = 100
      Height = 16
      Top = 7
      Width = 52
      Caption = '[o] Open'
    end
    object LegendOverdue: TLabel
      Left = 200
      Height = 16
      Top = 7
      Width = 68
      Caption = '[!] Overdue'
    end
  end
end
