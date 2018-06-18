object frmMain: TfrmMain
  Left = 0
  Top = 0
  ClientHeight = 656
  ClientWidth = 1149
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -20
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    1149
    656)
  PixelsPerInch = 96
  TextHeight = 24
  object Label1: TLabel
    Left = 5
    Top = 44
    Width = 530
    Height = 24
    Caption = #65288'1'#65289'.'#35835#21462#8220' '#34987#26597#35810' [ '#21547' 1- N '#20010#65288'TXT'#65289#25991#26412' ] '#30340#25991#20214#22841' '#8221#65306
  end
  object Label3: TLabel
    Left = 5
    Top = 120
    Width = 290
    Height = 24
    Caption = #65288'3'#65289'. '#26597#35810#8220#32452#21512#27425#25968#8221#33539#22260#65306'1'#8594
  end
  object Label4: TLabel
    Left = 5
    Top = 158
    Width = 350
    Height = 24
    Caption = #65288'4'#65289'. '#23548#20986#65288#38388#24046#65289#8220#30456#21516#32452#21512#25968#8221#65306'1'#8594
  end
  object lblUseTime: TLabel
    Left = 5
    Top = 9
    Width = 140
    Height = 24
    Caption = #22788#29702#25152#38656#26102#38388#65306
  end
  object Label2: TLabel
    Left = 5
    Top = 82
    Width = 250
    Height = 24
    Caption = #65288'2'#65289'. '#26597#35810#8220#34892#25968#8221#33539#22260#65306'1'#8594
  end
  object Label5: TLabel
    Left = 419
    Top = 158
    Width = 368
    Height = 24
    Caption = #20010' [ '#27880#65306#19981#21516#20195#21495#30340#39318#34892#38388#24046#65288#22823#8594#23567#65289']'
  end
  object Label6: TLabel
    Left = 5
    Top = 196
    Width = 289
    Height = 24
    Caption = #65288'5'#65289'. '#23548#20986#65288#36830#21644#65289#65306#65288'+'#65289'1'#8594
  end
  object Label7: TLabel
    Left = 5
    Top = 231
    Width = 350
    Height = 24
    Caption = #65288'6'#65289'. '#23548#20986#65288#36830#21644#65289#8220#30456#21516#32452#21512#25968#8221#65306'1'#8594
  end
  object Label8: TLabel
    Left = 417
    Top = 231
    Width = 368
    Height = 24
    Caption = #20010' [ '#27880#65306#19981#21516#20195#21495#30340#39318#34892#38388#24046#65288#22823#8594#23567#65289']'
  end
  object edtFileDirectory: TEdit
    Left = 541
    Top = 41
    Width = 585
    Height = 32
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnClick = edtFileDirectoryClick
  end
  object btnCompare: TButton
    Left = 8
    Top = 600
    Width = 380
    Height = 35
    Caption = #65288'7'#65289'.'#8220#24320#22987#26597#35810#8221#24182#23548#20986#8220#65288'TXT'#65289#25991#26412#8221
    TabOrder = 1
    OnClick = btnCompareClick
  end
  object edtCompareTypeCount: TEdit
    Left = 305
    Top = 117
    Width = 50
    Height = 32
    NumbersOnly = True
    TabOrder = 2
  end
  object edtExportTypeCount: TEdit
    Left = 363
    Top = 155
    Width = 50
    Height = 32
    NumbersOnly = True
    TabOrder = 3
  end
  object edtCompareRowCount: TEdit
    Left = 261
    Top = 79
    Width = 188
    Height = 32
    NumbersOnly = True
    TabOrder = 4
  end
  object edtExportFileCount: TEdit
    Left = 305
    Top = 193
    Width = 50
    Height = 32
    NumbersOnly = True
    TabOrder = 5
  end
  object edtExportTypeCount2: TEdit
    Left = 363
    Top = 228
    Width = 50
    Height = 32
    NumbersOnly = True
    TabOrder = 6
  end
  object chkExportRowSpacingFile: TCheckBox
    Left = 8
    Top = 311
    Width = 593
    Height = 25
    Caption = #65288'1'#65289'.'#65288#25490#24207#65289#32452#21512#27425#25968#65288#26368#22810' '#8594' '#23569#65289#65306#65288'1'#65289
    TabOrder = 7
  end
  object chkExportRowSpacingFile2: TCheckBox
    Left = 8
    Top = 342
    Width = 921
    Height = 25
    Caption = #65288'2'#65289'.'#65288#25490#24207#65289#9312'.'#19981#21516#39318#34892' ['#20195#21495#65306'N.'#65288'N'#12289'N'#65307'..'#65289']'#65306#65288'N'#65289#65288'N'#65289#65307#26368#21491#30340#65288'N'#65289#65288#26368#22823#8594#23567#65289#65288'2'#65289
    TabOrder = 8
  end
  object chkExportRowSpacingFile3: TCheckBox
    Left = 8
    Top = 373
    Width = 593
    Height = 25
    Caption = #65288'3'#65289'.'#65288#25490#24207#65289#9313'....'#65288#31532'N'#20010#38388#24046#65306'N'#65289#65288#26368#22823' '#8594' '#23567#65289#65306#65288'3'#65289' '
    TabOrder = 9
  end
  object chkExportRowSpacingFile4: TCheckBox
    Left = 8
    Top = 404
    Width = 593
    Height = 25
    Caption = #65288'4'#65289'.'#65288#25490#24207#65289#9314'.'#12304#31532'N-N'#20010#38388#24046#65306'N'#12305#65288#26368#22823' '#8594' '#23567#65289#65306#65288'4'#65289
    TabOrder = 10
  end
  object chkExportBearOneRowSpacingFile: TCheckBox
    Left = 8
    Top = 435
    Width = 593
    Height = 25
    Caption = #9312'-1. '#65288#25490#24207#65289#32452#21512#27425#25968#65288#36830#21644#65306'+1'#20197#19978#65289#65288#26368#22810' '#8594' '#23569#65289#65306#12304'1'#12305
    TabOrder = 11
  end
  object chkExportBearOneRowSpacingFile2: TCheckBox
    Left = 8
    Top = 466
    Width = 593
    Height = 25
    Caption = #9312'-2.'#65288#25490#24207#65289#9312'.'#9313'.'#9314'...'#65288#36830#21644#65306'+1'#20197#19978#65289#65288#26368#22823' '#8594' '#23567#65289#65306#12304'2'#12305
    TabOrder = 12
  end
  object chkSelectAll: TCheckBox
    Left = 8
    Top = 280
    Width = 420
    Height = 25
    Caption = #20840#36873'  [ '#27880#65306#23548#20986#20840#37096#65288'TXT '#25991#26412#65289']'#65292#22914#19979#65306
    TabOrder = 13
    OnClick = chkSelectAllClick
  end
  object chkExportBearOneRowSpacingFile3: TCheckBox
    Left = 8
    Top = 497
    Width = 593
    Height = 25
    Caption = #9312'-3.'#65288#25490#24207#65289#9312'.'#9313'.'#9314'...'#65288#36830#21644#65306'+2'#20197#19978#65289#65288#26368#22823' '#8594' '#23567#65289#65306#12304'3'#12305
    TabOrder = 14
  end
  object chkExportBearOneRowSpacingFile4: TCheckBox
    Left = 8
    Top = 528
    Width = 593
    Height = 25
    Caption = #9312'-4.'#65288#25490#24207#65289#9312'.'#9313'.'#9314'...'#65288#36830#21644#65306'+3'#20197#19978#65289#65288#26368#22823' '#8594' '#23567#65289#65306#12304'4'#12305
    TabOrder = 15
  end
  object chkExportBearOneRowSpacingFile5: TCheckBox
    Left = 8
    Top = 559
    Width = 593
    Height = 25
    Caption = #9312'-5.'#65288#25490#24207#65289#9312'.'#9313'.'#9314'...'#65288#36830#21644#65306'+4'#20197#19978#65289#65288#26368#22823' '#8594' '#23567#65289#65306#12304'5'#12305
    TabOrder = 16
  end
  object chkExportBearOneRowSpacingFile6: TCheckBox
    Left = 612
    Top = 466
    Width = 550
    Height = 25
    Caption = #9312'-6.'#65288#25490#24207#65289'...'#65288#36830#21644#65306'+5'#20197#19978#65289#65288#26368#22823' '#8594' '#23567#65289#65306#12304'6'#12305
    TabOrder = 17
  end
  object chkExportBearOneRowSpacingFile7: TCheckBox
    Left = 612
    Top = 497
    Width = 550
    Height = 25
    Caption = #9312'-7.'#65288#25490#24207#65289'...'#65288#36830#21644#65306'+6'#20197#19978#65289#65288#26368#22823' '#8594' '#23567#65289#65306#12304'7'#12305
    TabOrder = 18
  end
  object chkExportBearOneRowSpacingFile8: TCheckBox
    Left = 612
    Top = 528
    Width = 550
    Height = 25
    Caption = #9312'-8.'#65288#25490#24207#65289'...'#65288#36830#21644#65306'+7'#20197#19978#65289#65288#26368#22823' '#8594' '#23567#65289#65306#12304'8'#12305
    TabOrder = 19
  end
  object chkExportBearOneRowSpacingFile9: TCheckBox
    Left = 612
    Top = 559
    Width = 550
    Height = 25
    Caption = #9312'-9.'#65288#25490#24207#65289'...'#65288#36830#21644#65306'+8'#20197#19978#65289#65288#26368#22823' '#8594' '#23567#65289#65306#12304'9'#12305
    TabOrder = 20
  end
  object chkNotExportSourceData: TCheckBox
    Left = 434
    Top = 280
    Width = 559
    Height = 25
    Caption = #19981#26174#31034' '#12304' '#37197#22871' '#12305#9312'.[ '#23548#20986#20840#37096#65288'TXT'#65289#25991#26412' ] '#30340#32452#21512#20869#23481' '
    TabOrder = 21
  end
  object OpenDialog: TOpenDialog
    Filter = #25991#26412#25991#20214'|*.txt'
    Left = 312
  end
end
