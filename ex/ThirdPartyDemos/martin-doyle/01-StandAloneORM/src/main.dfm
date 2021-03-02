object MainForm: TMainForm
  Left = 886
  Top = 313
  Width = 418
  Height = 373
  Caption = ' Sample 01 - In Memory ORM'
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
  object LabelEdit: TLabel
    Left = 32
    Top = 48
    Width = 71
    Height = 16
    Caption = 'Your Name:'
    FocusControl = NameEdit
  end
  object NameEdit: TEdit
    Left = 32
    Top = 72
    Width = 121
    Height = 24
    TabOrder = 0
  end
  object ButtonFind: TButton
    Left = 184
    Top = 72
    Width = 161
    Height = 25
    Caption = 'Find previous message'
    TabOrder = 1
    OnClick = ButtonFindClick
  end
  object QuestionMemo: TMemo
    Left = 32
    Top = 112
    Width = 313
    Height = 129
    TabOrder = 2
  end
  object ButtonAdd: TButton
    Left = 32
    Top = 256
    Width = 137
    Height = 25
    Caption = 'Add message'
    TabOrder = 3
    OnClick = ButtonAddClick
  end
  object ButtonQuit: TButton
    Left = 272
    Top = 256
    Width = 75
    Height = 25
    Caption = 'Quit'
    TabOrder = 4
    OnClick = ButtonQuitClick
  end
end
