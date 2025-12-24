object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'mORMot2 Basic Demo VCL Client'
  ClientHeight = 450
  ClientWidth = 650
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 249
    Height = 13
    Caption = 'Run 01-basicdemo_server.exe first (localhost:8080)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblResult: TLabel
    Left = 16
    Top = 40
    Width = 28
    Height = 13
    Caption = 'Ready'
  end
  object Label2: TLabel
    Left = 16
    Top = 120
    Width = 28
    Height = 13
    Caption = 'Num1'
  end
  object Label3: TLabel
    Left = 96
    Top = 120
    Width = 28
    Height = 13
    Caption = 'Num2'
  end
  object Label4: TLabel
    Left = 16
    Top = 168
    Width = 83
    Height = 13
    Caption = 'JSON to POST'
  end
  object btnHelloWorld: TButton
    Left = 16
    Top = 72
    Width = 150
    Height = 30
    Caption = 'GET /hello'
    TabOrder = 0
    OnClick = btnHelloWorldClick
  end
  object btnDivide: TButton
    Left = 176
    Top = 115
    Width = 150
    Height = 30
    Caption = 'GET /div/{num1}/{num2}'
    TabOrder = 1
    OnClick = btnDivideClick
  end
  object btnHelloPost: TButton
    Left = 16
    Top = 187
    Width = 150
    Height = 30
    Caption = 'POST /hello'
    TabOrder = 2
    OnClick = btnHelloPostClick
  end
  object memoResponse: TMemo
    Left = 16
    Top = 232
    Width = 616
    Height = 201
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 3
  end
  object edtNum1: TEdit
    Left = 16
    Top = 136
    Width = 65
    Height = 21
    TabOrder = 4
    Text = '10'
  end
  object edtNum2: TEdit
    Left = 96
    Top = 136
    Width = 65
    Height = 21
    TabOrder = 5
    Text = '20'
  end
  object edtPostData: TEdit
    Left = 112
    Top = 165
    Width = 520
    Height = 21
    TabOrder = 6
    Text = '{"name":"Bob"}'
  end
end
