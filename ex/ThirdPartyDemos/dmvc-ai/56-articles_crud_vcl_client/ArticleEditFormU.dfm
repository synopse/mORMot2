object ArticleEditForm: TArticleEditForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Article Details'
  ClientHeight = 250
  ClientWidth = 450
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 64
    Width = 26
    Height = 13
    Caption = 'Code:'
  end
  object Label2: TLabel
    Left = 24
    Top = 96
    Width = 57
    Height = 13
    Caption = 'Description:'
  end
  object Label3: TLabel
    Left = 24
    Top = 128
    Width = 28
    Height = 13
    Caption = 'Price:'
  end
  object lblValidation: TLabel
    Left = 24
    Top = 168
    Width = 3
    Height = 13
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblInfo: TLabel
    Left = 24
    Top = 24
    Width = 298
    Height = 13
    Caption = 'Code format: CXX, CXXX, or CXXXX (e.g., C01, C123, C1234)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object edtCode: TEdit
    Left = 104
    Top = 61
    Width = 120
    Height = 21
    CharCase = ecUpperCase
    MaxLength = 5
    TabOrder = 0
  end
  object edtDescription: TEdit
    Left = 104
    Top = 93
    Width = 320
    Height = 21
    MaxLength = 200
    TabOrder = 1
  end
  object edtPrice: TEdit
    Left = 104
    Top = 125
    Width = 120
    Height = 21
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 264
    Top = 208
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 352
    Top = 208
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
