object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'mORMot2 Articles CRUD VCL Client'
  ClientHeight = 550
  ClientWidth = 850
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
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 850
    Height = 65
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 16
      Width = 280
      Height = 13
      Caption = 'Run 06-articles_crud_server.exe first (localhost:8080)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblStatus: TLabel
      Left = 16
      Top = 40
      Width = 28
      Height = 13
      Caption = 'Ready'
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 65
    Width = 850
    Height = 50
    Align = alTop
    TabOrder = 1
    object btnRefresh: TButton
      Left = 16
      Top = 10
      Width = 90
      Height = 30
      Caption = 'Refresh'
      TabOrder = 0
      OnClick = btnRefreshClick
    end
    object btnNew: TButton
      Left = 112
      Top = 10
      Width = 90
      Height = 30
      Caption = 'New...'
      TabOrder = 1
      OnClick = btnNewClick
    end
    object btnEdit: TButton
      Left = 208
      Top = 10
      Width = 90
      Height = 30
      Caption = 'Edit...'
      TabOrder = 2
      OnClick = btnEditClick
    end
    object btnDelete: TButton
      Left = 304
      Top = 10
      Width = 90
      Height = 30
      Caption = 'Delete'
      TabOrder = 3
      OnClick = btnDeleteClick
    end
    object btnSearch: TButton
      Left = 416
      Top = 10
      Width = 90
      Height = 30
      Caption = 'Search...'
      TabOrder = 4
      OnClick = btnSearchClick
    end
  end
  object gridArticles: TStringGrid
    Left = 0
    Top = 165
    Width = 850
    Height = 385
    Align = alClient
    ColCount = 6
    DefaultRowHeight = 20
    FixedCols = 0
    RowCount = 2
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
    ParentFont = False
    TabOrder = 2
    OnDblClick = gridArticlesDblClick
  end
  object pnlSearch: TPanel
    Left = 0
    Top = 115
    Width = 850
    Height = 50
    Align = alTop
    TabOrder = 3
    object Label2: TLabel
      Left = 16
      Top = 17
      Width = 83
      Height = 13
      Caption = 'Search in description:'
    end
    object edtSearch: TEdit
      Left = 112
      Top = 14
      Width = 400
      Height = 21
      TabOrder = 0
    end
    object btnSearchExecute: TButton
      Left = 528
      Top = 10
      Width = 90
      Height = 30
      Caption = 'Search'
      TabOrder = 1
      OnClick = btnSearchExecuteClick
    end
    object btnSearchClear: TButton
      Left = 624
      Top = 10
      Width = 90
      Height = 30
      Caption = 'Clear'
      TabOrder = 2
      OnClick = btnSearchClearClick
    end
  end
end
