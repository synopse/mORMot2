object frmServerSelection: TfrmServerSelection
  Left = 0
  Top = 0
  ActiveControl = rbHttp10
  BorderStyle = bsDialog
  Caption = 'Server selection'
  ClientHeight = 299
  ClientWidth = 338
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnShow = FormShow
  TextHeight = 15
  object lblCertFileName: TLabel
    Left = 30
    Top = 100
    Width = 106
    Height = 15
    Caption = 'Certificate filename:'
  end
  object lblPrivKeyFileName: TLabel
    Left = 30
    Top = 150
    Width = 107
    Height = 15
    Caption = 'PrivateKey filename:'
  end
  object lblPrivKeyPassword: TLabel
    Left = 30
    Top = 200
    Width = 111
    Height = 15
    Caption = 'PrivateKey password:'
  end
  object btnStartServer: TButton
    Left = 74
    Top = 266
    Width = 125
    Height = 25
    Caption = 'Start server'
    Default = True
    TabOrder = 0
    OnClick = btnStartServerClick
  end
  object btnTerminate: TButton
    Left = 205
    Top = 266
    Width = 125
    Height = 25
    Caption = 'Terminate'
    ModalResult = 2
    TabOrder = 1
  end
  object rbHttp10: TRadioButton
    Left = 8
    Top = 8
    Width = 322
    Height = 17
    Caption = 'HTTP 1.0'
    Checked = True
    TabOrder = 2
    TabStop = True
  end
  object rbHttp11: TRadioButton
    Left = 8
    Top = 31
    Width = 322
    Height = 17
    Caption = 'HTTP 1.1'
    TabOrder = 3
  end
  object rbHttpsSelf: TRadioButton
    Left = 8
    Top = 54
    Width = 322
    Height = 17
    Caption = 'HTTPS - self-signed'
    TabOrder = 4
  end
  object rbHttpsCert: TRadioButton
    Left = 8
    Top = 77
    Width = 322
    Height = 17
    Caption = 'HTTPS - with own Certificate'
    TabOrder = 5
  end
  object edtCertFileName: TEdit
    Left = 30
    Top = 121
    Width = 300
    Height = 23
    TabOrder = 6
  end
  object edtPrivKeyFileName: TEdit
    Left = 30
    Top = 171
    Width = 300
    Height = 23
    TabOrder = 7
  end
  object edtPrivKeyPassword: TEdit
    Left = 30
    Top = 221
    Width = 300
    Height = 23
    TabOrder = 8
  end
end
