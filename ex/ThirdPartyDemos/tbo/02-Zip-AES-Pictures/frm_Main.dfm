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
  DesignSize = (
    534
    361)
  TextHeight = 15
  object ScrollBox: TScrollBox
    Left = 161
    Top = 8
    Width = 365
    Height = 345
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    TabOrder = 3
    object Image: TImage
      Left = 0
      Top = 0
      Width = 100
      Height = 100
      AutoSize = True
    end
  end
  object btnNewImage: TButton
    Left = 10
    Top = 8
    Width = 145
    Height = 25
    Caption = 'New Image'
    TabOrder = 4
    OnClick = btnNewImageClick
  end
  object btnSaveImage: TButton
    Left = 8
    Top = 58
    Width = 147
    Height = 25
    Caption = 'Save image'
    TabOrder = 0
    OnClick = btnSaveImageClick
  end
  object btnLoadImage: TButton
    Left = 8
    Top = 89
    Width = 147
    Height = 25
    Caption = 'Load image'
    TabOrder = 1
    OnClick = btnLoadImageClick
  end
  object btnCryptSaveLoad: TButton
    Left = 10
    Top = 139
    Width = 145
    Height = 25
    Caption = 'Crypt Save Load'
    TabOrder = 2
    OnClick = btnCryptSaveLoadClick
  end
  object ProgressBar: TProgressBar
    Left = 8
    Top = 220
    Width = 147
    Height = 17
    TabOrder = 5
  end
  object btnZipInfoProgress: TButton
    Left = 8
    Top = 189
    Width = 147
    Height = 25
    Caption = 'Progress'
    TabOrder = 6
    OnClick = btnZipInfoProgressClick
  end
  object btnWriteSpeedChallenge: TButton
    Left = 8
    Top = 270
    Width = 147
    Height = 25
    Caption = 'WriteSpeed challenge'
    TabOrder = 7
    OnClick = btnWriteSpeedChallengeClick
  end
end
