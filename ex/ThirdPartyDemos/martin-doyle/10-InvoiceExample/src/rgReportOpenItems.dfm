inherited OpenItemsReportForm: TOpenItemsReportForm
  Caption = 'Open Items Report'
  ClientHeight = 500
  ClientWidth = 800
  inherited FilterPanel: TPanel
    Width = 800
    Height = 60
    object LabelFromDate: TLabel
      Left = 16
      Height = 17
      Top = 20
      Width = 60
      Caption = 'From Date:'
    end
    object LabelToDate: TLabel
      Left = 200
      Height = 17
      Top = 20
      Width = 48
      Caption = 'To Date:'
    end
    object LabelMinAmount: TLabel
      Left = 380
      Height = 17
      Top = 20
      Width = 73
      Caption = 'Min Amount:'
    end
    object EditFromDate: TEdit
      Left = 85
      Height = 25
      Top = 17
      Width = 100
      TabOrder = 0
    end
    object EditToDate: TEdit
      Left = 260
      Height = 25
      Top = 17
      Width = 100
      TabOrder = 1
    end
    object EditMinAmount: TEdit
      Left = 465
      Height = 25
      Top = 17
      Width = 80
      TabOrder = 2
      Text = '0'
    end
    object RefreshButton: TButton
      Left = 570
      Height = 28
      Top = 16
      Width = 80
      Caption = '&Refresh'
      TabOrder = 3
      OnClick = RefreshButtonClick
    end
  end
  inherited CloseButton: TButton
    Left = 704
    Top = 460
  end
end
