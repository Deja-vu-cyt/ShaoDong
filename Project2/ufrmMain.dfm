object frmMain: TfrmMain
  Left = 0
  Top = 0
  ClientHeight = 413
  ClientWidth = 1056
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -20
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDblClick = FormDblClick
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 24
  object Label1: TLabel
    Left = 8
    Top = 44
    Width = 269
    Height = 24
    Alignment = taRightJustify
    Caption = '1.'#35835#21462#8220#34987#26597#35810#65288'TXT'#65289#25991#26412#8221#65306
  end
  object Label2: TLabel
    Left = 368
    Top = 79
    Width = 588
    Height = 24
    Alignment = taRightJustify
    Caption = '4.'#35774#32622#8220#26597#35810#12304#31532'N-N'#34892#12305#26012#36830#12304#31532'1-N'#34892#20026#39318#34892#12305#21015#8216' 1-N '#8217#27425#32452#21512#8221#65306
  end
  object Label3: TLabel
    Left = 8
    Top = 79
    Width = 293
    Height = 24
    Alignment = taRightJustify
    Caption = '2.'#35774#32622#8220#26597#35810#65288#24635#21644#21306#22495#65289#8221#21015#25968#65306
  end
  object Label4: TLabel
    Left = 8
    Top = 114
    Width = 284
    Height = 24
    Alignment = taRightJustify
    Caption = '3.'#35774#32622#8220#26597#35810#65288#31532'1'#21306#22495#65289#21015#25968#8221#65306
  end
  object Label5: TLabel
    Left = 365
    Top = 114
    Width = 610
    Height = 24
    Alignment = taRightJustify
    Caption = '5.'#35774#32622#8220#23548#20986#12304#31532'N-N'#34892#12305#26012#36830#12304#31532'1-N'#34892#20026#39318#34892#12305#21015#8216' N '#8217#20010#20197#19978#32452#21512#8221#65306
  end
  object lblUseTime: TLabel
    Left = 5
    Top = 9
    Width = 140
    Height = 24
    Caption = #22788#29702#25152#38656#26102#38388#65306
  end
  object Label6: TLabel
    Left = 5
    Top = 212
    Width = 676
    Height = 24
    Caption = #12304#25490#21015#12305' '#65288'2'#65289'.-'#65288'6'#65289'.'#8220'N'#8221#20010#20197#19978#21508#20010#32452#21512#65288#30456#21516#20195#21495#12289#19981#21516#39318#34892#65289#65292#22914#19979#65306
  end
  object edtFileName: TEdit
    Left = 283
    Top = 41
    Width = 500
    Height = 29
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = edtFileNameClick
  end
  object edtCompareSpacing: TEdit
    Left = 981
    Top = 76
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
  object btnCompare: TButton
    Left = 5
    Top = 350
    Width = 249
    Height = 30
    Caption = '6.'#8220#24320#22987#26597#35810#8221#24182#8220#23548#20986#25968#25454#8221
    TabOrder = 2
    OnClick = btnCompareClick
  end
  object edtMaxValue: TEdit
    Left = 300
    Top = 76
    Width = 50
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
  object edtFirstRangeValue: TEdit
    Left = 300
    Top = 111
    Width = 50
    Height = 29
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    NumbersOnly = True
    ParentFont = False
    TabOrder = 4
  end
  object edtExportTypeCount: TEdit
    Left = 981
    Top = 111
    Width = 50
    Height = 29
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    NumbersOnly = True
    ParentFont = False
    TabOrder = 5
  end
  object chkExportFile: TCheckBox
    Left = 5
    Top = 180
    Width = 970
    Height = 25
    Caption = 
      #65288'1'#65289'.'#12304#25490#21015#12305#8220'N'#8221#20010#20197#19978#12304' [ '#30456#21516#65288#31532#8220'N'#8221#34892#20026#39318#34892#65289'] '#12289#19981#21516'[ '#20195#21495#65306#65288#31532#8220'N.N'#8221#20010#65289#8220'N'#8221'Z.'#8220'N'#8221'Y  ] '#12305#30340 +
      #32452#21512'     '
    TabOrder = 6
  end
  object chkExportFile2: TCheckBox
    Left = 5
    Top = 247
    Width = 330
    Height = 25
    Caption = #65288'2'#65289'. '#19981#21516#39318#34892#25968#65306#26368#22810' - '#26368#23569#34892
    TabOrder = 7
  end
  object chkExportFile3: TCheckBox
    Left = 358
    Top = 247
    Width = 380
    Height = 25
    Caption = #65288'3'#65289'. '#37051#34892#36317#65306#26368#22823#8595#8220'N'#8221'- '#26368#23567#8595#8220'N'#8221
    TabOrder = 8
  end
  object chkExportFile4: TCheckBox
    Left = 5
    Top = 279
    Width = 330
    Height = 25
    Caption = #65288'4'#65289'. '#32452#21512#25968#65306#26368#22810' - '#26368#23569#20010
    TabOrder = 9
  end
  object chkExportFile5: TCheckBox
    Left = 358
    Top = 279
    Width = 380
    Height = 25
    Caption = #65288'5'#65289'. '#26080#12304#23545#24212#21015#12305#25968#65306#26368#22810' - '#26368#23569#21015
    TabOrder = 10
  end
  object chkExportFile6: TCheckBox
    Left = 5
    Top = 312
    Width = 836
    Height = 25
    Caption = #65288'6'#65289'.'#12304#20445#23384#12305#8220'N'#8221#20010#20197#19978#21508#20010#32452#21512#65306#65288#30456#21516#20195#21495#12289#19981#21516#39318#34892#65289#30340#65288#20195#21495#65306#8220'N'#8221'='#8220'N'#8221'Z.'#8220'N'#8221'Y '#65289
    TabOrder = 11
  end
  object chkSelectAll: TCheckBox
    Left = 5
    Top = 149
    Width = 420
    Height = 25
    Caption = #20840#36873'  [ '#27880#65306#23548#20986#20840#37096#65288'TXT '#25991#26412#65289']'#65292#22914#19979#65306
    TabOrder = 12
    OnClick = chkSelectAllClick
  end
  object btnExportCompareRow: TButton
    Left = 260
    Top = 350
    Width = 75
    Height = 30
    Caption = #23548#20986#34892
    TabOrder = 13
    Visible = False
    OnClick = btnExportCompareRowClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 396
    Width = 5000
    Height = 2
    BevelWidth = 3
    BorderWidth = 3
    TabOrder = 14
  end
  object OpenDialog: TOpenDialog
    Filter = #25991#26412#25991#20214'|*.txt'
    Left = 312
  end
end
