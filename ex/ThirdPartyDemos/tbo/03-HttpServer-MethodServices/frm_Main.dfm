object frmMain: TfrmMain
  Left = 0
  Top = 0
  ActiveControl = btnNewImage
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
  OnShow = FormShow
  DesignSize = (
    534
    361)
  TextHeight = 15
  object lblUserName: TLabel
    Left = 8
    Top = 48
    Width = 59
    Height = 15
    Caption = 'User name:'
    FocusControl = edtUserName
  end
  object lblPassword: TLabel
    Left = 8
    Top = 98
    Width = 53
    Height = 15
    Caption = 'Password:'
    FocusControl = edtPassword
  end
  object lblImageName: TLabel
    Left = 8
    Top = 159
    Width = 69
    Height = 15
    Caption = 'Image name:'
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
  object btnNewImage: TButton
    Left = 8
    Top = 8
    Width = 147
    Height = 25
    Caption = 'New Image'
    TabOrder = 0
    OnClick = btnNewImageClick
  end
  object edtUserName: TEdit
    Left = 8
    Top = 69
    Width = 147
    Height = 23
    TabOrder = 1
    Text = 'test'
  end
  object edtPassword: TEdit
    Left = 8
    Top = 119
    Width = 147
    Height = 23
    TabOrder = 2
    Text = 'test'
  end
  object edtImageName: TEdit
    Left = 8
    Top = 180
    Width = 147
    Height = 23
    TabOrder = 3
  end
  object btnSaveImage: TButton
    Left = 8
    Top = 209
    Width = 147
    Height = 25
    Caption = 'Save image'
    TabOrder = 4
    OnClick = btnSaveImageClick
  end
  object btnLoadImage: TButton
    Left = 8
    Top = 278
    Width = 147
    Height = 25
    Caption = 'Load image'
    TabOrder = 5
    OnClick = btnLoadImageClick
  end
  object cbbAllImages: TComboBox
    Left = 8
    Top = 249
    Width = 147
    Height = 23
    TabOrder = 7
    OnDropDown = cbbAllImagesDropDown
  end
  object ProgressBar: TProgressBar
    Left = 8
    Top = 336
    Width = 147
    Height = 17
    Anchors = [akLeft, akBottom]
    ParentShowHint = False
    ShowHint = False
    TabOrder = 8
  end
end
