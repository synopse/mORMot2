inherited PaymentReceiptsReportForm: TPaymentReceiptsReportForm
  Caption = 'Payment Receipts Report'
  ClientHeight = 500
  ClientWidth = 700
  LCLVersion = '4.5.0.0'
  inherited FilterPanel: TPanel
    Width = 700
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
    object RefreshButton: TButton
      Left = 400
      Height = 28
      Top = 16
      Width = 80
      Caption = '&Refresh'
      TabOrder = 2
      OnClick = RefreshButtonClick
    end
  end
  inherited CloseButton: TButton
    Left = 604
    Top = 460
  end
end
