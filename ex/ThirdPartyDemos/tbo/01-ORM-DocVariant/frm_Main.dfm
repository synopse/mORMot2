object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Main'
  ClientHeight = 361
  ClientWidth = 534
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 550
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    534
    361)
  TextHeight = 15
  object btnNewImage: TButton
    Left = 10
    Top = 8
    Width = 145
    Height = 25
    Caption = 'New Image'
    TabOrder = 5
    OnClick = btnNewImageClick
  end
  object btnAddImage: TButton
    Left = 8
    Top = 68
    Width = 145
    Height = 25
    Caption = 'Add Image'
    TabOrder = 0
    OnClick = btnAddImageClick
  end
  object btnLoadImage: TButton
    Left = 8
    Top = 99
    Width = 145
    Height = 25
    Caption = 'Load Image'
    TabOrder = 1
    OnClick = btnLoadImageClick
  end
  object btnLoadSearchFTS: TButton
    Left = 8
    Top = 178
    Width = 145
    Height = 25
    Caption = 'Load SearchFTS'
    TabOrder = 2
    OnClick = btnLoadSearchFTSClick
  end
  object edtSearchPhrase: TEdit
    Left = 8
    Top = 149
    Width = 145
    Height = 23
    TabOrder = 3
    Text = 'W'#252'rzburg'
  end
  object btnLoadSearchMetaData: TButton
    Left = 8
    Top = 228
    Width = 145
    Height = 25
    Caption = 'Load SearchMetaData'
    TabOrder = 4
    OnClick = btnLoadSearchMetaDataClick
  end
  object ScrollBox: TScrollBox
    Left = 161
    Top = 8
    Width = 365
    Height = 345
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    TabOrder = 6
    object Image: TImage
      Left = 0
      Top = 0
      Width = 100
      Height = 100
      AutoSize = True
    end
  end
end
