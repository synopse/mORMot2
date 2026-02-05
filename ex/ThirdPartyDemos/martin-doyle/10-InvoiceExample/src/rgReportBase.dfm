object ReportBaseForm: TReportBaseForm
  Left = 400
  Height = 500
  Top = 200
  Width = 700
  BorderStyle = bsDialog
  Caption = 'Report'
  ClientHeight = 500
  ClientWidth = 700
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  object FilterPanel: TPanel
    Left = 0
    Height = 50
    Top = 0
    Width = 700
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
  end
  object CloseButton: TButton
    Left = 604
    Height = 28
    Top = 460
    Width = 80
    Anchors = [akRight, akBottom]
    Caption = '&Close'
    TabOrder = 1
    OnClick = CloseButtonClick
  end
end
