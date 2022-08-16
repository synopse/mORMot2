object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Thomas little Mustache Editor'
  ClientHeight = 461
  ClientWidth = 624
  Color = clBtnFace
  Constraints.MinHeight = 500
  Constraints.MinWidth = 540
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  TextHeight = 15
  object pnlToolBar: TPanel
    Left = 0
    Top = 0
    Width = 624
    Height = 35
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblProject: TLabel
      Left = 0
      Top = 5
      Width = 80
      Height = 15
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Project:'
    end
    object btnProjectNew: TButton
      Left = 85
      Top = 0
      Width = 100
      Height = 25
      Caption = '&New'
      TabOrder = 0
      OnClick = btnProjectNewClick
    end
    object btnProjectSave: TButton
      Left = 297
      Top = 0
      Width = 100
      Height = 25
      Caption = '&Save... /(F6)'
      TabOrder = 2
      OnClick = btnProjectSaveClick
    end
    object btnProjectLoad: TButton
      Left = 191
      Top = 0
      Width = 100
      Height = 25
      Caption = '&Load...'
      TabOrder = 1
      OnClick = btnProjectLoadClick
    end
  end
  object PageControl: TPageControl
    Left = 0
    Top = 35
    Width = 624
    Height = 401
    ActivePage = tbsMustache
    Align = alClient
    TabOrder = 1
    OnChange = PageControlChange
    object tbsMustache: TTabSheet
      Caption = '  &Mustache  '
      object Splitter: TSplitter
        Left = 0
        Top = 170
        Width = 616
        Height = 8
        Cursor = crVSplit
        Align = alBottom
        AutoSnap = False
        MinSize = 120
      end
      object pnlAdditional: TPanel
        Left = 0
        Top = 178
        Width = 616
        Height = 193
        Align = alBottom
        BevelOuter = bvNone
        Constraints.MinHeight = 120
        TabOrder = 1
        object lblJavaScriptEditor: TLabel
          Left = 0
          Top = 0
          Width = 616
          Height = 15
          Align = alTop
          Caption = 'This JavaScript is inserted in the HTML header:'
          ExplicitWidth = 242
        end
        object pnlExternalCssJsFiles: TPanel
          Left = 0
          Top = 141
          Width = 616
          Height = 52
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            616
            52)
          object lblExternalCssJsFiles: TLabel
            Left = 3
            Top = 5
            Width = 432
            Height = 15
            Caption = 
              'Names of CSS or JS files in Assets directory. Entries in order s' +
              'eparated by pipe ("|").'
          end
          object edtExternalCssJsFiles: TEdit
            Left = 0
            Top = 25
            Width = 616
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
          end
        end
        object pnlJavaScriptEditor: TPanel
          Left = 0
          Top = 15
          Width = 616
          Height = 126
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 1
        end
      end
      object pnlMustache: TPanel
        Left = 0
        Top = 0
        Width = 616
        Height = 170
        Align = alClient
        BevelOuter = bvNone
        Constraints.MinHeight = 120
        TabOrder = 0
        object lblMustacheEditor: TLabel
          Left = 0
          Top = 0
          Width = 616
          Height = 15
          Align = alTop
          Caption = 'Mustache source code:'
          ExplicitWidth = 122
        end
        object pnlMustacheDataFiles: TPanel
          Left = 0
          Top = 118
          Width = 616
          Height = 52
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            616
            52)
          object lblMustacheDataFilesInfo: TLabel
            Left = 3
            Top = 5
            Width = 462
            Height = 15
            Caption = 
              'Names of JSON data files in Assets directory. Maximum 5 entries ' +
              'separated by pipe ("|").'
          end
          object lblMustacheDataFiles: TLabel
            Left = 0
            Top = 29
            Width = 100
            Height = 15
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Data1 .. Data5:'
          end
          object edtMustacheDataFiles: TEdit
            Left = 105
            Top = 25
            Width = 511
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
          end
        end
        object pnlMustacheEditor: TPanel
          Left = 0
          Top = 15
          Width = 616
          Height = 103
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 1
        end
      end
    end
    object tbsHtmlView: TTabSheet
      Caption = '  HTML &View  '
      ImageIndex = 1
      object pnlBrowserArea: TPanel
        Left = 0
        Top = 0
        Width = 616
        Height = 371
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object pnlBrowserBar: TPanel
          Left = 0
          Top = 0
          Width = 616
          Height = 35
          Align = alTop
          BevelEdges = [beBottom]
          BevelKind = bkFlat
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            616
            33)
          object btnReloadPage: TButton
            Left = 4
            Top = 4
            Width = 100
            Height = 25
            Caption = '&Reload (F5)'
            TabOrder = 0
            OnClick = btnReloadPageClick
          end
          object btnStandalone: TButton
            Left = 512
            Top = 4
            Width = 100
            Height = 25
            Anchors = [akTop, akRight]
            Caption = '&Standalone'
            TabOrder = 1
            OnClick = btnStandaloneClick
          end
        end
        object EdgeBrowser: TEdgeBrowser
          Left = 0
          Top = 35
          Width = 616
          Height = 336
          Align = alClient
          TabOrder = 1
          OnNavigationCompleted = EdgeBrowserNavigationCompleted
        end
      end
    end
  end
  object pnlStatusBar: TPanel
    Left = 0
    Top = 436
    Width = 624
    Height = 25
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'Shows the usage of mORMot HTTP Server and Mustache'
    TabOrder = 2
  end
end
