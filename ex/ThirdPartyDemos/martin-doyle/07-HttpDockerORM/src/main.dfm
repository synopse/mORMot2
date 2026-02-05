object MainForm: TMainForm
  Left = 320
  Top = 150
  Width = 418
  Height = 400
  Caption = 'Sample 07 - Http Docker ORM'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object LabelName: TLabel
    Left = 32
    Top = 16
    Width = 71
    Height = 16
    Caption = 'Your Name:'
    FocusControl = EditName
  end
  object LabelNames: TLabel
    Left = 32
    Top = 168
    Width = 50
    Height = 16
    Caption = 'Names:'
  end
  object EditName: TEdit
    Left = 32
    Top = 36
    Width = 121
    Height = 24
    TabOrder = 0
  end
  object ButtonFind: TButton
    Left = 184
    Top = 36
    Width = 161
    Height = 25
    Caption = 'Find'
    TabOrder = 1
    OnClick = ButtonFindClick
  end
  object MemoQuestion: TMemo
    Left = 32
    Top = 72
    Width = 313
    Height = 81
    TabOrder = 2
  end
  object ListNames: TListBox
    Left = 32
    Top = 188
    Width = 313
    Height = 121
    ItemHeight = 16
    TabOrder = 3
    OnClick = ListNamesClick
  end
  object ButtonNew: TButton
    Left = 32
    Top = 328
    Width = 70
    Height = 25
    Caption = 'New'
    TabOrder = 4
    OnClick = ButtonNewClick
  end
  object ButtonSave: TButton
    Left = 113
    Top = 328
    Width = 70
    Height = 25
    Caption = 'Save'
    TabOrder = 5
    OnClick = ButtonSaveClick
  end
  object ButtonDelete: TButton
    Left = 194
    Top = 328
    Width = 70
    Height = 25
    Caption = 'Delete'
    TabOrder = 6
    OnClick = ButtonDeleteClick
  end
  object ButtonQuit: TButton
    Left = 275
    Top = 328
    Width = 70
    Height = 25
    Caption = 'Quit'
    TabOrder = 7
    OnClick = ButtonQuitClick
  end
end
