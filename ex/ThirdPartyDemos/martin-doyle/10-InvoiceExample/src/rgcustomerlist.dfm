object CustomerListForm: TCustomerListForm
  Left = 363
  Height = 400
  Top = 222
  Width = 600
  Caption = 'Customer List'
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
    TabOrder = 0
    object NewButton: TButton
      Left = 8
      Height = 25
      Top = 8
      Width = 75
      Caption = '&New'
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
    object SearchEdit: TEdit
      Left = 280
      Height = 25
      Top = 8
      Width = 200
      TabOrder = 3
      OnChange = SearchEditChange
    end
  end
end
