inherited MonthlyOverviewReportForm: TMonthlyOverviewReportForm
  Caption = 'Monthly Overview Report'
  ClientHeight = 500
  ClientWidth = 750
  inherited FilterPanel: TPanel
    Width = 750
    Height = 60
    object LabelYear: TLabel
      Left = 16
      Height = 17
      Top = 20
      Width = 30
      Caption = 'Year:'
    end
    object ComboYear: TComboBox
      Left = 60
      Height = 25
      Top = 17
      Width = 100
      ItemHeight = 0
      TabOrder = 0
    end
    object RefreshButton: TButton
      Left = 180
      Height = 28
      Top = 16
      Width = 80
      Caption = '&Refresh'
      TabOrder = 1
      OnClick = RefreshButtonClick
    end
  end
  inherited CloseButton: TButton
    Left = 654
    Top = 460
  end
end
