object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'mORMot2 Logger GUI Sample'
  ClientHeight = 481
  ClientWidth = 724
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object lblLogFile: TLabel
    Left = 16
    Top = 456
    Width = 44
    Height = 15
    Caption = 'Log file:'
  end
  object btnLoggerTest: TButton
    Left = 16
    Top = 16
    Width = 121
    Height = 33
    Caption = 'Test Logger'
    TabOrder = 0
    OnClick = btnLoggerTestClick
  end
  object memoLog: TMemo
    Left = 16
    Top = 64
    Width = 689
    Height = 377
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object btnClearLog: TButton
    Left = 152
    Top = 16
    Width = 121
    Height = 33
    Caption = 'Clear Display'
    TabOrder = 2
    OnClick = btnClearLogClick
  end
  object btnViewLogFile: TButton
    Left = 288
    Top = 16
    Width = 121
    Height = 33
    Caption = 'View Log File'
    TabOrder = 3
    OnClick = btnViewLogFileClick
  end
end
