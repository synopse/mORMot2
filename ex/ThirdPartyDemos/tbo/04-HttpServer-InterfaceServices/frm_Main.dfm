object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'TestRestClient'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Constraints.MinHeight = 480
  Constraints.MinWidth = 640
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 15
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 624
    Height = 70
    Align = alTop
    BevelEdges = [beBottom]
    BevelKind = bkFlat
    BevelOuter = bvNone
    TabOrder = 0
    object lblLoadDocName: TLabel
      Left = 9
      Top = 44
      Width = 75
      Height = 15
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Document:'
    end
    object lblUserName: TLabel
      Left = 9
      Top = 11
      Width = 75
      Height = 15
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Username:'
    end
    object lblPassword: TLabel
      Left = 221
      Top = 11
      Width = 75
      Height = 15
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Password:'
    end
    object cbbLoadDocName: TComboBox
      Left = 90
      Top = 40
      Width = 250
      Height = 23
      TabOrder = 0
      OnDropDown = cbbLoadDocNameDropDown
    end
    object btnLoadDocExecute: TButton
      Left = 346
      Top = 39
      Width = 125
      Height = 25
      Caption = 'Load document'
      TabOrder = 1
      OnClick = btnLoadDocExecuteClick
    end
    object edtUserName: TEdit
      Left = 90
      Top = 7
      Width = 125
      Height = 23
      TabOrder = 2
      Text = 'test'
    end
    object edtPassword: TEdit
      Left = 302
      Top = 7
      Width = 125
      Height = 23
      TabOrder = 3
      Text = 'test'
    end
    object btnNewConnection: TButton
      Left = 433
      Top = 6
      Width = 125
      Height = 25
      Caption = 'New connection'
      TabOrder = 4
      OnClick = btnNewConnectionClick
    end
  end
  object PageControl: TPageControl
    Left = 0
    Top = 70
    Width = 624
    Height = 371
    ActivePage = tbsMarkdown
    Align = alClient
    TabOrder = 1
    OnChange = PageControlChange
    object tbsMarkdown: TTabSheet
      Caption = '  &Markdown  '
      object pnlMarkdown: TPanel
        Left = 0
        Top = 301
        Width = 616
        Height = 40
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        object lblSaveDocName: TLabel
          Left = 278
          Top = 11
          Width = 60
          Height = 15
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Name:'
        end
        object btnNewDocExecute: TButton
          Left = 5
          Top = 6
          Width = 125
          Height = 25
          Caption = 'New document'
          TabOrder = 0
          OnClick = btnNewDocExecuteClick
        end
        object edtSaveDocName: TEdit
          Left = 344
          Top = 7
          Width = 250
          Height = 23
          TabOrder = 1
        end
        object btnSaveDocExecute: TButton
          Left = 147
          Top = 6
          Width = 125
          Height = 25
          Caption = 'Save document'
          TabOrder = 2
          OnClick = btnSaveDocExecuteClick
        end
      end
      object memMarkdown: TMemo
        Left = 0
        Top = 0
        Width = 616
        Height = 301
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Consolas'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 1
      end
    end
    object tbsHtmlView: TTabSheet
      Caption = '  HTML &View  '
      object WebBrowser: TWebBrowser
        Left = 0
        Top = 0
        Width = 616
        Height = 341
        Align = alClient
        TabOrder = 0
        ControlData = {
          4C000000AA3F00003E2300000100000001020000000000000000000000000000
          000000004C000000000000000000000001000000E0D057007335CF11AE690800
          2B2E126203000000000000004C0000000114020000000000C000000000000046
          8000000000000000000000000000000000000000000000000000000000000000
          00000000000000000100000000000000000000000000000000000000}
      end
    end
  end
end
