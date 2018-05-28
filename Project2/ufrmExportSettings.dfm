object frmExportSettings: TfrmExportSettings
  Left = 0
  Top = 0
  Caption = 'frmExportSettings'
  ClientHeight = 274
  ClientWidth = 1364
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -20
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 24
  object Label2: TLabel
    Left = 16
    Top = 28
    Width = 897
    Height = 24
    Alignment = taRightJustify
    Caption = #12304' '#37197#22871' '#12305#65288'1'#65289'.'#20445#25345'['#65288'1'#65289'.'#12304#25490#21015#12305#12304#8220'N'#8221#20010#20197#19978'['#30456#21516#39318#34892#12289#19981#21516#32452#21512']'#30340#32452#21512#12305']'#65288#26368#22823#37051#34892#36317#8595#65289#65306
  end
  object Label1: TLabel
    Left = 16
    Top = 63
    Width = 691
    Height = 24
    Alignment = taRightJustify
    Caption = #12304' '#37197#22871' '#12305#65288'2'#65289'.'#20445#30041' [ '#23548#20986#30340#20840#37096#65288'TXT'#65289#25991#26412' ] '#19981#21516#65288#30452#12289#26012#36830#65289#39318#34892#25968' '#65306
  end
  object Label3: TLabel
    Left = 16
    Top = 100
    Width = 715
    Height = 24
    Caption = #12304' '#37197#22871' '#12305#65288'1.2'#65289'.[ '#23548#20986#30340#20840#37096#65288'TXT'#65289#25991#26412' ] '#65292#24517#39035#28385#36275#20197#19979' '#9312'.--- '#9314'.'#20010#26465#20214' '#65306
  end
  object Label4: TLabel
    Left = 16
    Top = 133
    Width = 994
    Height = 24
    Caption = #9312'. '#20445#25345'['#65288'1'#65289'.'#12304#25490#21015#12305#12304#8220'N'#8221#20010#20197#19978'['#30456#21516#39318#34892#12289#19981#21516#32452#21512']'#30340#32452#21512#12305']'#65288#26368#22823#37051#34892#36317#8595#65289#65306#65288#23567#12289#31561#20110#8804#65289#12304' N '#12305#65307
  end
  object Label5: TLabel
    Left = 16
    Top = 163
    Width = 1199
    Height = 24
    Caption = 
      #9313'.'#12304' '#37197#22871' '#12305#65288'1'#65289'...'#12304#8220'N'#8221#20010#20197#19978'['#30456#21516#39318#34892#12289#19981#21516#32452#21512']'#30340#32452#21512#12305#65307#23646#20110#12304' '#37197#22871' '#12305#65288'2'#65289'...'#65288#22823#12289#31561#20110#8805#65289#12304' N '#12305#33539 +
      #22260#20869#30340' '#65292#20248#20808#25552#21462' '#65307
  end
  object Label6: TLabel
    Left = 16
    Top = 193
    Width = 1343
    Height = 24
    Caption = 
      #9314'.'#65288'2'#65289'.'#12304#25490#21015#12305#8220'N'#8221#20010#20197#19978#21508#20010#32452#21512#65288#30456#21516#20195#21495#12289#19981#21516#39318#34892#65289#30340#65288#19981#21516#39318#34892#25968#65306#26368#22810' - '#26368#23569#34892#65289#65306#65288#25353#65289#19981#21516#39318#34892#25968#65288#27425#25968#26368#22810' '#8594 +
      ' '#26368#23569#34892#65289#20248#20808#25552#21462' '#65307
  end
  object edtKeepMaxRowSpacing: TEdit
    Left = 919
    Top = 28
    Width = 100
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
  object edtMinGroupRowCount: TEdit
    Left = 713
    Top = 63
    Width = 100
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
  object btnOK: TButton
    Left = 576
    Top = 231
    Width = 100
    Height = 30
    Caption = #30830#23450
    TabOrder = 2
    OnClick = btnOKClick
  end
end
