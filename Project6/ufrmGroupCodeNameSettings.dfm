object frmGroupCodeNameSettings: TfrmGroupCodeNameSettings
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  ClientHeight = 232
  ClientWidth = 1124
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -20
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 24
  object lblLine: TLabel
    Left = 23
    Top = 23
    Width = 809
    Height = 24
    Caption = 
      '6-2 '#65288'1'#65289'.'#35774#32622' '#65306#31532'          '#33267'          '#27425#65288' '#36941#21382' '#65289#20043#38388' '#65307#21482#20445#30041#65339' '#31532' 1 - N '#34892#20026#39318#34892' '#65341 +
      #20013' '#65306
  end
  object lblLine1: TLabel
    Left = 23
    Top = 58
    Width = 523
    Height = 24
    Caption = '6-2 '#9312'.'#35774#32622' '#65306#21508#65288' '#26368#21069' '#65289'                                  '#30340#39318#34892' '#65292
  end
  object lblLine2: TLabel
    Left = 23
    Top = 93
    Width = 609
    Height = 24
    Caption = '6-2 '#9313'.'#35774#32622' '#65306#21508#65288' '#38543#21518' '#65289'                                  '#30340#39318#34892' '#65292#20877#36941#21382' '#12290
  end
  object Label1: TLabel
    Left = 23
    Top = 128
    Width = 1095
    Height = 24
    Caption = 
      #12304' '#27880' '#65306#31532#12304' N '#12305#33267#12304' N '#12305#27425#65288' '#20043#20869#30340#36941#21382' '#65289#65292#25165#19981#25490#21015' : '#39318#34892#65339' '#9312'.'#65288' '#20195#21495#32452#21512#25968' '#65289#12289#9313'.'#65288' '#39318#34892#20986#29616#25968' '#65289#26368#22810 +
      ' '#65341#30340#34892' '#12305
  end
  object edtGroupNumber: TEdit
    Left = 207
    Top = 23
    Width = 50
    Height = 29
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    NumbersOnly = True
    ParentFont = False
    TabOrder = 0
  end
  object edtGroupNumber2: TEdit
    Left = 287
    Top = 23
    Width = 50
    Height = 29
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    NumbersOnly = True
    ParentFont = False
    TabOrder = 1
  end
  object edtGroupCountEachFirstNumber: TEdit
    Left = 254
    Top = 93
    Width = 200
    Height = 29
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    NumbersOnly = True
    ParentFont = False
    TabOrder = 2
  end
  object edtGroupFirstNumberCount: TEdit
    Left = 254
    Top = 58
    Width = 200
    Height = 29
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    NumbersOnly = True
    ParentFont = False
    TabOrder = 3
  end
  object btnOk: TButton
    Left = 463
    Top = 170
    Width = 106
    Height = 40
    Caption = #30830#23450
    TabOrder = 4
    OnClick = btnOkClick
  end
end
