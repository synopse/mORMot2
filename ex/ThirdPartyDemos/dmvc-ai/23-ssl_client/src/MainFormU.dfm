object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'SSL/HTTPS Client - mORMot2'
  ClientHeight = 350
  ClientWidth = 600
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    600
    350)
  PixelsPerInch = 96
  TextHeight = 13
  object lblTitle: TLabel
    Left = 8
    Top = 8
    Width = 188
    Height = 13
    Caption = 'HTTPS Client with Certificate Handling'
  end
  object lblResult: TLabel
    Left = 8
    Top = 324
    Width = 584
    Height = 13
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Ready'
  end
  object lblUrl: TLabel
    Left = 8
    Top = 32
    Width = 22
    Height = 13
    Caption = 'URL:'
  end
  object edtUrl: TEdit
    Left = 40
    Top = 29
    Width = 425
    Height = 21
    TabOrder = 0
    Text = 'https://localhost:8443/api/people'
  end
  object btnGet: TButton
    Left = 471
    Top = 27
    Width = 121
    Height = 25
    Caption = 'GET (Secure)'
    TabOrder = 1
    OnClick = btnGetClick
  end
  object memoResponse: TMemo
    Left = 8
    Top = 88
    Width = 584
    Height = 230
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object chkIgnoreCert: TCheckBox
    Left = 8
    Top = 62
    Width = 281
    Height = 17
    Caption = 'Ignore TLS Certificate Errors (for development only)'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object btnPost: TButton
    Left = 471
    Top = 56
    Width = 121
    Height = 25
    Caption = 'POST (Secure)'
    TabOrder = 4
    OnClick = btnPostClick
  end
