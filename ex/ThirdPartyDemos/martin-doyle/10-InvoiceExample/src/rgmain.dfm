object MainForm: TMainForm
  Left = 72
  Height = 592
  Top = 220
  Width = 1300
  Caption = 'Rechnung'
  Menu = MainMenu
  ParentFont = True
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  object TopPanel: TPanel
    Left = 0
    Height = 50
    Top = 0
    Width = 1300
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 0
    object QuickInfoCustomerCount: TLabel
      Left = 16
      Height = 18
      Top = 15
      Width = 106
      Caption = '0 Customers'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object QuickInfoSeparator1: TLabel
      Left = 130
      Height = 18
      Top = 15
      Width = 6
      Caption = '|'
      Font.Color = clGray
      ParentFont = False
    end
    object QuickInfoOpenItems: TLabel
      Left = 150
      Height = 18
      Top = 15
      Width = 110
      Caption = 'Open: 0 (0.00)'
    end
    object QuickInfoSeparator2: TLabel
      Left = 340
      Height = 18
      Top = 15
      Width = 6
      Caption = '|'
      Font.Color = clGray
      ParentFont = False
    end
    object QuickInfoDueToday: TLabel
      Left = 360
      Height = 18
      Top = 15
      Width = 50
      Caption = 'Due: 0'
    end
    object QuickInfoSeparator3: TLabel
      Left = 450
      Height = 18
      Top = 15
      Width = 6
      Caption = '|'
      Font.Color = clGray
      ParentFont = False
    end
    object QuickInfoOverdue: TLabel
      Left = 470
      Height = 18
      Top = 15
      Width = 84
      Caption = 'Overdue: 0'
      Font.Color = clRed
      ParentFont = False
    end
  end
  object ContentPanel: TPanel
    Left = 0
    Height = 516
    Top = 50
    Width = 1300
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 1
    object LeftSplitter: TSplitter
      Left = 520
      Height = 516
      Top = 0
      Width = 5
    end
    object LeftPanel: TPanel
      Left = 0
      Height = 516
      Top = 0
      Width = 520
      Align = alLeft
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 0
    end
    object RightPanel: TPanel
      Left = 525
      Height = 516
      Top = 0
      Width = 775
      Align = alClient
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 1
      object CustomerSummaryPanel: TPanel
        Left = 0
        Height = 60
        Top = 0
        Width = 775
        Align = alTop
        BevelOuter = bvNone
        ParentBackground = False
        TabOrder = 0
        object CustomerSummaryName: TLabel
          Left = 16
          Height = 1
          Top = 8
          Width = 1
          Font.Height = -16
          Font.Style = [fsBold]
          ParentFont = False
        end
        object CustomerSummaryStats: TLabel
          Left = 16
          Height = 1
          Top = 36
          Width = 1
        end
      end
      object InvoiceListPanel: TPanel
        Left = 0
        Height = 456
        Top = 60
        Width = 775
        Align = alClient
        BevelOuter = bvNone
        ParentBackground = False
        TabOrder = 1
      end
    end
  end
  object StatusPanel: TPanel
    Left = 0
    Height = 26
    Top = 566
    Width = 1300
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 2
    object StatusLabel: TLabel
      Left = 8
      Height = 18
      Top = 5
      Width = 89
      Caption = 'StatusLabel'
    end
  end
  object MainImageList: TImageList
    Left = 215
    Top = 78
  end
  object MainActionList: TActionList
    Images = MainImageList
    Left = 92
    Top = 78
    object FileExit: TFileExit
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit'
    end
    object HelpAboutAction: TAction
      Category = 'Help'
      Caption = '&About'
      OnExecute = HelpAboutActionExecute
    end
    object ReportOpenItems: TAction
      Category = 'Reports'
      Caption = '&Open Items'
      OnExecute = ReportOpenItemsExecute
    end
    object ReportPaymentReceipts: TAction
      Category = 'Reports'
      Caption = '&Payment Receipts'
      OnExecute = ReportPaymentReceiptsExecute
    end
    object ReportCustomerRevenue: TAction
      Category = 'Reports'
      Caption = '&Customer Revenue'
      OnExecute = ReportCustomerRevenueExecute
    end
    object ReportMonthlyOverview: TAction
      Category = 'Reports'
      Caption = '&Monthly Overview'
      OnExecute = ReportMonthlyOverviewExecute
    end
  end
  object MainMenu: TMainMenu
    Images = MainImageList
    Left = 15
    Top = 13
    object ReportsMenu: TMenuItem
      Caption = '&Reports'
      object ReportOpenItemsMenuItem: TMenuItem
        Action = ReportOpenItems
      end
      object ReportPaymentReceiptsMenuItem: TMenuItem
        Action = ReportPaymentReceipts
      end
      object ReportCustomerRevenueMenuItem: TMenuItem
        Action = ReportCustomerRevenue
      end
      object ReportMonthlyOverviewMenuItem: TMenuItem
        Action = ReportMonthlyOverview
      end
    end
  end
end
