object CustomerEditForm: TCustomerEditForm
  Left = 400
  Height = 487
  Top = 250
  Width = 160
  BorderStyle = bsDialog
  Caption = 'Customer Details'
  ClientHeight = 487
  ClientWidth = 160
  Position = poMainFormCenter
  LCLVersion = '4.5.0.0'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  object LabelCustomerNo: TLabel
    Left = 16
    Height = 16
    Top = 20
    Width = 84
    Caption = 'Customer No:'
  end
  object EditCustomerNo: TEdit
    Left = 120
    Height = 21
    Top = 16
    Width = 150
    TabOrder = 0
  end
  object LabelCompany: TLabel
    Left = 16
    Height = 16
    Top = 52
    Width = 61
    Caption = 'Company:'
  end
  object EditCompany: TEdit
    Left = 120
    Height = 21
    Top = 48
    Width = 350
    TabOrder = 1
  end
  object LabelPhone: TLabel
    Left = 16
    Height = 16
    Top = 84
    Width = 42
    Caption = 'Phone:'
  end
  object EditPhone: TEdit
    Left = 120
    Height = 21
    Top = 80
    Width = 200
    TabOrder = 2
  end
  object LabelFax: TLabel
    Left = 16
    Height = 16
    Top = 116
    Width = 25
    Caption = 'Fax:'
  end
  object EditFax: TEdit
    Left = 120
    Height = 21
    Top = 112
    Width = 200
    TabOrder = 3
  end
  object LabelAddress: TLabel
    Left = 16
    Height = 16
    Top = 148
    Width = 53
    Caption = 'Address:'
  end
  object EditAddress: TEdit
    Left = 120
    Height = 21
    Top = 144
    Width = 350
    TabOrder = 4
  end
  object LabelZip: TLabel
    Left = 16
    Height = 16
    Top = 180
    Width = 23
    Caption = 'Zip:'
  end
  object EditZip: TEdit
    Left = 120
    Height = 21
    Top = 176
    Width = 100
    TabOrder = 5
  end
  object LabelCity: TLabel
    Left = 16
    Height = 16
    Top = 212
    Width = 28
    Caption = 'City:'
  end
  object EditCity: TEdit
    Left = 120
    Height = 21
    Top = 208
    Width = 250
    TabOrder = 6
  end
  object LabelCountry: TLabel
    Left = 16
    Height = 16
    Top = 244
    Width = 52
    Caption = 'Country:'
  end
  object EditCountry: TEdit
    Left = 120
    Height = 21
    Top = 240
    Width = 250
    TabOrder = 7
  end
  object SaveButton: TButton
    Left = 320
    Height = 25
    Top = 12
    Width = 75
    Caption = '&Save'
    Default = True
    TabOrder = 8
    OnClick = SaveButtonClick
  end
  object CancelButton: TButton
    Left = 400
    Height = 25
    Top = 12
    Width = 75
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 9
    OnClick = CancelButtonClick
  end
end
