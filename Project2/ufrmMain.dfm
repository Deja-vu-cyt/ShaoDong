object frmMain: TfrmMain
  Left = 0
  Top = 0
  ClientHeight = 711
  ClientWidth = 1334
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
  PixelsPerInch = 96
  TextHeight = 24
  object PageControl1: TPageControl
    Left = 0
    Top = 196
    Width = 1334
    Height = 515
    ActivePage = TabSheet3
    Align = alClient
    TabOrder = 0
    ExplicitTop = 173
    ExplicitHeight = 510
    object TabSheet3: TTabSheet
      Caption = #19968'.'#26597#35810#65306#12304#21508#34892#38388#12305#30452#12289#26012#36830#12304#23545#24212#21015#12305#32452#21512'     '
      ImageIndex = 2
      ExplicitLeft = 8
      ExplicitHeight = 470
      object Label10: TLabel
        Left = 12
        Top = 7
        Width = 813
        Height = 24
        Caption = '4.'#35774#32622#65306#26597#35810#12304#65288#31532'N-N'#34892#20026#39318#34892#65289#30452#36830' ['#65288#31532'N-1'#34892#65289#65288#31532'1-N'#30452#36830#34892#65289']'#12305#21508#12304' N'#12305#34892#20043#38388#65306
      end
      object Label12: TLabel
        Left = 12
        Top = 41
        Width = 473
        Height = 24
        Caption = #65288'1'#65289'.'#65288#30452#36830#65289'y'#26377'          '#8594'           '#20010#12304#23545#24212#21015#12305#25968#23383
      end
      object Label14: TLabel
        Left = 12
        Top = 76
        Width = 813
        Height = 24
        Caption = '5.'#35774#32622#65306#26597#35810#12304#65288#31532'N-N'#34892#20026#39318#34892#65289#26012#36830' ['#65288#31532'N-1'#34892#65289#65288#31532'1-N'#26012#36830#34892#65289']'#12305#21508#12304' N'#12305#34892#20043#38388#65306
      end
      object Label15: TLabel
        Left = 12
        Top = 110
        Width = 473
        Height = 24
        Caption = #65288'1'#65289'.'#65288#26012#36830#65289'y'#26377'          '#8594'           '#20010#12304#23545#24212#21015#12305#25968#23383
      end
      object Label16: TLabel
        Left = 12
        Top = 180
        Width = 905
        Height = 24
        Alignment = taRightJustify
        Caption = '7. '#35774#32622#8220#23548#20986#12304#65288#31532'N-N'#34892#20026#39318#34892#65289#30452#12289#26012#36830' ['#65288#31532'N-1'#34892#65289#65288#31532'1-N'#30452#12289#26012#36830#34892#65289']'#12305#21015#8220'N'#8221#20010#20197#19978#32452#21512#8221#65306
      end
      object Label17: TLabel
        Left = 5
        Top = 281
        Width = 684
        Height = 24
        Caption = #12304#25490#21015#12305' '#65288'2'#65289'---'#65288'6'#65289'.'#8220'N'#8221#20010#20197#19978#21508#20010#32452#21512#65288#30456#21516#20195#21495#12289#19981#21516#39318#34892#65289#65292#22914#19979#65306
      end
      object Label18: TLabel
        Left = 10
        Top = 145
        Width = 907
        Height = 24
        Alignment = taRightJustify
        Caption = '6.'#35774#32622#8220#26597#35810#12304#65288#31532'N-N'#34892#20026#39318#34892#65289#30452#12289#26012#36830' ['#65288#31532'N-1'#34892#65289#65288#31532'1-N'#30452#12289#26012#36830#34892#65289']'#12305#21015#8220'  1-N   '#8221#27425#32452#21512#8221#65306
      end
      object edtVVertSameValueCount2: TEdit
        Left = 267
        Top = 41
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
      object edtVVertSameValueCount: TEdit
        Left = 186
        Top = 41
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
      object edtVVertCompareSpacing: TEdit
        Left = 839
        Top = 7
        Width = 100
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
      object edtSlantSameValueCount2: TEdit
        Left = 267
        Top = 110
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
      object edtSlantSameValueCount: TEdit
        Left = 186
        Top = 110
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
      object edtSlantCompareSpacing: TEdit
        Left = 839
        Top = 76
        Width = 100
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
      object btnVertSlantCompare: TButton
        Left = 5
        Top = 419
        Width = 249
        Height = 30
        Caption = '9.'#8220#24320#22987#26597#35810#8221#24182#8220#23548#20986#25968#25454#8221
        TabOrder = 6
        OnClick = btnCompareClick
      end
      object edtExportGroupValueCount: TEdit
        Left = 920
        Top = 180
        Width = 100
        Height = 29
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Tahoma'
        Font.Style = []
        NumbersOnly = True
        ParentFont = False
        TabOrder = 7
      end
      object chkExportVertSlantFile: TCheckBox
        Left = 5
        Top = 249
        Width = 588
        Height = 25
        Caption = #65288'1'#65289'.'#12304#25490#21015#12305#12304#8220'N'#8221#20010#20197#19978'['#30456#21516#39318#34892#12289#19981#21516#32452#21512']'#30340#32452#21512#12305'   '
        TabOrder = 8
      end
      object chkExportVertSlantFile2: TCheckBox
        Left = 5
        Top = 316
        Width = 330
        Height = 25
        Caption = #65288'2'#65289'.[ '#19981#21516#39318#34892#25968#65306#26368#22810#8594#23569' ]'
        TabOrder = 9
      end
      object chkExportVertSlantFile3: TCheckBox
        Left = 358
        Top = 316
        Width = 380
        Height = 25
        Caption = #65288'3'#65289'.[ '#37051#34892#36317#65306#26368#22823#8595#8594#23567#8595']'
        TabOrder = 10
      end
      object chkExportVertSlantFile4: TCheckBox
        Left = 5
        Top = 348
        Width = 330
        Height = 25
        Caption = #65288'4'#65289'.[ '#32452#21512#25968#65306#26368#22810#8594#23569#20010' ]'
        TabOrder = 11
      end
      object chkExportVertSlantFile5: TCheckBox
        Left = 358
        Top = 348
        Width = 380
        Height = 25
        Caption = #65288'5'#65289'.[ '#26080#12304#23545#24212#21015#12305#25968#65306#26368#22810#8594#23569#21015' ]'
        TabOrder = 12
      end
      object chkExportVertSlantFile6: TCheckBox
        Left = 5
        Top = 381
        Width = 836
        Height = 25
        Caption = #65288'6'#65289'.'#12304#31616#21270#12305#12304#8220'N'#8221#20010#20197#19978'['#30456#21516#32452#21512#12289#19981#21516#39318#34892']'#30340#32452#21512#12305
        TabOrder = 13
      end
      object chkVertSlantSelectAll: TCheckBox
        Left = 5
        Top = 218
        Width = 710
        Height = 25
        Caption = '8.'#20840#36873'[ '#27880#65306#23548#20986#65288'1'#65289'.'#12304#25490#21015#12305'-'#65288'6'#65289'.'#12304#31616#21270#12305#20840#37096#65288'TXT'#65289#25991#26412' ]'#65292#22914#19979#65306
        TabOrder = 14
        OnClick = chkVertSlantSelectAllClick
      end
      object edtCompareGroupValueCount: TEdit
        Left = 920
        Top = 145
        Width = 100
        Height = 29
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Tahoma'
        Font.Style = []
        NumbersOnly = True
        ParentFont = False
        TabOrder = 15
      end
      object btnVertSlantExportSettings: TButton
        Left = 721
        Top = 215
        Width = 420
        Height = 30
        Caption = #12304' '#37197#22871' '#12305#20877#22788#29702'[ '#23548#20986#30340#20840#37096#65288'TXT'#65289#25991#26412' ] '
        TabOrder = 16
        OnClick = btnExportSettingsClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = '  '#20108'.'#26597#35810#65306#12304#21508#34892#38388#12305#30452#36830#12304#23545#24212#21015#12305#32452#21512'      '
      ImageIndex = 1
      ExplicitHeight = 471
      object Label11: TLabel
        Left = 9
        Top = 119
        Width = 837
        Height = 24
        Alignment = taRightJustify
        Caption = '6. '#35774#32622#8220#23548#20986#12304#65288#31532'N-N'#34892#20026#39318#34892#65289#30452#36830' ['#65288#31532'N-1'#34892#65289#65288#31532' 1-N '#30452#36830#34892#65289']'#12305#21015#8220'N'#8221#20010#20197#19978#32452#21512#8221#65306
      end
      object Label13: TLabel
        Left = 5
        Top = 220
        Width = 684
        Height = 24
        Caption = #12304#25490#21015#12305' '#65288'2'#65289'---'#65288'6'#65289'.'#8220'N'#8221#20010#20197#19978#21508#20010#32452#21512#65288#30456#21516#20195#21495#12289#19981#21516#39318#34892#65289#65292#22914#19979#65306
      end
      object Label7: TLabel
        Left = 12
        Top = 15
        Width = 813
        Height = 24
        Caption = '4.'#35774#32622#65306#26597#35810#12304#65288#31532'N-N'#34892#20026#39318#34892#65289#30452#36830' ['#65288#31532'N-1'#34892#65289#65288#31532'1-N'#30452#36830#34892#65289']'#12305#21508#12304' N'#12305#34892#20043#38388#65306
      end
      object Label9: TLabel
        Left = 12
        Top = 49
        Width = 473
        Height = 24
        Caption = #65288'1'#65289'.'#65288#30452#36830#65289'y'#26377'          '#8594'           '#20010#12304#23545#24212#21015#12305#25968#23383
      end
      object Label8: TLabel
        Left = 7
        Top = 84
        Width = 839
        Height = 24
        Alignment = taRightJustify
        Caption = '5. '#35774#32622#8220#26597#35810#12304#65288#31532'N-N'#34892#20026#39318#34892#65289#30452#36830' ['#65288#31532'N-1'#34892#65289#65288#31532' 1-N '#30452#36830#34892#65289']'#12305#21015#8220'  1-N  '#8221#27425#32452#21512#8221#65306
      end
      object btnVertCompare: TButton
        Left = 5
        Top = 358
        Width = 249
        Height = 30
        Caption = '8.'#8220#24320#22987#26597#35810#8221#24182#8220#23548#20986#25968#25454#8221
        TabOrder = 0
        OnClick = btnCompareClick
      end
      object edtVertExportTypeCount: TEdit
        Left = 849
        Top = 119
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
      object chkExportVertFile: TCheckBox
        Left = 5
        Top = 188
        Width = 617
        Height = 25
        Caption = #65288'1'#65289'.'#12304#25490#21015#12305#12304#8220'N'#8221#20010#20197#19978'['#30456#21516#39318#34892#12289#19981#21516#32452#21512']'#30340#32452#21512#12305
        TabOrder = 2
      end
      object chkExportVertFile2: TCheckBox
        Left = 5
        Top = 255
        Width = 330
        Height = 25
        Caption = #65288'2'#65289'.[ '#19981#21516#39318#34892#25968#65306#26368#22810#8594#23569' ]'
        TabOrder = 3
      end
      object chkExportVertFile3: TCheckBox
        Left = 358
        Top = 255
        Width = 380
        Height = 25
        Caption = #65288'3'#65289'.[ '#37051#34892#36317#65306#26368#22823#8595#8594#23567#8595']'
        TabOrder = 4
      end
      object chkExportVertFile4: TCheckBox
        Left = 5
        Top = 287
        Width = 330
        Height = 25
        Caption = #65288'4'#65289'.[ '#32452#21512#25968#65306#26368#22810#8594#23569#20010' ]'
        TabOrder = 5
      end
      object chkExportVertFile5: TCheckBox
        Left = 358
        Top = 287
        Width = 380
        Height = 25
        Caption = #65288'5'#65289'.[ '#26080#12304#23545#24212#21015#12305#25968#65306#26368#22810#8594#23569#21015' ]'
        TabOrder = 6
      end
      object chkExportVertFile6: TCheckBox
        Left = 5
        Top = 320
        Width = 836
        Height = 25
        Caption = #65288'6'#65289'.'#12304#31616#21270#12305#12304#8220'N'#8221#20010#20197#19978'['#30456#21516#32452#21512#12289#19981#21516#39318#34892']'#30340#32452#21512#12305
        TabOrder = 7
      end
      object chkVertSelectAll: TCheckBox
        Left = 5
        Top = 157
        Width = 710
        Height = 25
        Caption = '7.'#20840#36873'[ '#27880#65306#23548#20986#65288'1'#65289'.'#12304#25490#21015#12305'-'#65288'6'#65289'.'#12304#31616#21270#12305#20840#37096#65288'TXT'#65289#25991#26412' ]'#65292#22914#19979#65306
        TabOrder = 8
        OnClick = chkVertSelectAllClick
      end
      object edtVertSameValueCount2: TEdit
        Left = 267
        Top = 49
        Width = 50
        Height = 29
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Tahoma'
        Font.Style = []
        NumbersOnly = True
        ParentFont = False
        TabOrder = 9
      end
      object edtVertSameValueCount: TEdit
        Left = 186
        Top = 49
        Width = 50
        Height = 29
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Tahoma'
        Font.Style = []
        NumbersOnly = True
        ParentFont = False
        TabOrder = 10
      end
      object edtVertCompareSpacing: TEdit
        Left = 839
        Top = 15
        Width = 100
        Height = 29
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Tahoma'
        Font.Style = []
        NumbersOnly = True
        ParentFont = False
        TabOrder = 11
      end
      object edtVertCompareTypeCount: TEdit
        Left = 849
        Top = 84
        Width = 100
        Height = 29
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Tahoma'
        Font.Style = []
        NumbersOnly = True
        ParentFont = False
        TabOrder = 12
      end
      object btnVertExportSettings: TButton
        Left = 721
        Top = 154
        Width = 420
        Height = 30
        Caption = #12304' '#37197#22871' '#12305#20877#22788#29702'[ '#23548#20986#30340#20840#37096#65288'TXT'#65289#25991#26412' ] '
        TabOrder = 13
        OnClick = btnExportSettingsClick
      end
    end
    object TabSheet1: TTabSheet
      Caption = '  '#19977'.'#26597#35810#65306#12304#21508#34892#38388#12305#26012#36830#12304#23545#24212#21015#12305#32452#21512'     '
      ExplicitHeight = 471
      object Label5: TLabel
        Left = 12
        Top = 47
        Width = 688
        Height = 24
        Alignment = taRightJustify
        Caption = '5. '#35774#32622#8220#23548#20986#12304#65288#31532' N-N '#34892#20026#39318#34892#65289#26012#36830#65288#31532' N-1 '#34892#65289#12305#21015#8220' N '#8221#20010#20197#19978#32452#21512#8221#65306
      end
      object Label6: TLabel
        Left = 15
        Top = 151
        Width = 684
        Height = 24
        Caption = #12304#25490#21015#12305' '#65288'2'#65289'---'#65288'6'#65289'.'#8220'N'#8221#20010#20197#19978#21508#20010#32452#21512#65288#30456#21516#20195#21495#12289#19981#21516#39318#34892#65289#65292#22914#19979#65306
      end
      object Label2: TLabel
        Left = 10
        Top = 12
        Width = 690
        Height = 24
        Alignment = taRightJustify
        Caption = '4. '#35774#32622#8220#26597#35810#12304#65288#31532' N-N '#34892#20026#39318#34892#65289#26012#36830#65288#31532' N-1 '#34892#65289#12305#21015#8220'   1-N   '#8221#27425#32452#21512#8221#65306
      end
      object btnSlantCompare: TButton
        Left = 15
        Top = 290
        Width = 249
        Height = 30
        Caption = '7.'#8220#24320#22987#26597#35810#8221#24182#8220#23548#20986#25968#25454#8221
        TabOrder = 0
        OnClick = btnCompareClick
      end
      object edtExportTypeCount: TEdit
        Left = 706
        Top = 47
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
      object chkExportFile: TCheckBox
        Left = 15
        Top = 116
        Width = 770
        Height = 25
        Caption = #65288'1'#65289'.'#12304#25490#21015#12305#8220'N'#8221#20010#20197#19978#12304' [ '#30456#21516#65288#31532#8220'N'#8221#34892#20026#39318#34892#65289'] '#12289#65288#19981#21516#20195#21495#65289#12305#30340#32452#21512' '#65307
        TabOrder = 2
      end
      object chkExportFile2: TCheckBox
        Left = 15
        Top = 183
        Width = 330
        Height = 25
        Caption = #65288'2'#65289'.[ '#19981#21516#39318#34892#25968#65306#26368#22810#8594#23569' ]'
        TabOrder = 3
      end
      object chkExportFile3: TCheckBox
        Left = 368
        Top = 183
        Width = 380
        Height = 25
        Caption = #65288'3'#65289'.[ '#37051#34892#36317#65306#26368#22823#8595#8594#23567#8595']'
        TabOrder = 4
      end
      object chkExportFile4: TCheckBox
        Left = 15
        Top = 215
        Width = 330
        Height = 25
        Caption = #65288'4'#65289'.[ '#32452#21512#25968#65306#26368#22810#8594#23569#20010' ]'
        TabOrder = 5
      end
      object chkExportFile5: TCheckBox
        Left = 368
        Top = 215
        Width = 380
        Height = 25
        Caption = #65288'5'#65289'.[ '#26080#12304#23545#24212#21015#12305#25968#65306#26368#22810#8594#23569#21015' ]'
        TabOrder = 6
      end
      object chkExportFile6: TCheckBox
        Left = 15
        Top = 248
        Width = 770
        Height = 25
        Caption = #65288'6'#65289'.'#12304#20445#23384#12305#8220'N'#8221#20010#20197#19978#21508#20010#32452#21512#65288#30456#21516#20195#21495#12289#19981#21516#39318#34892#65289#30340#65288#20195#21495#65306'NZ. NY '#65289
        TabOrder = 7
      end
      object chkSelectAll: TCheckBox
        Left = 15
        Top = 85
        Width = 560
        Height = 25
        Caption = '6.'#20840#36873'[ '#27880#65306#23548#20986#65288'1'#65289'.'#12304#25490#21015#12305'-'#65288'6'#65289'.'#12304#20445#23384#12305' ]'#65292#22914#19979#65306
        TabOrder = 8
        OnClick = chkSelectAllClick
      end
      object btnExportCompareRow: TButton
        Left = 270
        Top = 290
        Width = 75
        Height = 30
        Caption = #23548#20986#34892
        TabOrder = 9
        Visible = False
        OnClick = btnExportCompareRowClick
      end
      object edtCompareSpacing: TEdit
        Left = 706
        Top = 12
        Width = 100
        Height = 29
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Tahoma'
        Font.Style = []
        NumbersOnly = True
        ParentFont = False
        TabOrder = 10
      end
      object btnSlantExportSettings: TButton
        Left = 581
        Top = 82
        Width = 420
        Height = 30
        Caption = #12304' '#37197#22871' '#12305#20877#22788#29702'[ '#23548#20986#30340#20840#37096#65288'TXT'#65289#25991#26412' ] '
        TabOrder = 11
        OnClick = btnExportSettingsClick
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 1334
    Height = 196
    Align = alTop
    TabOrder = 1
    DesignSize = (
      1334
      196)
    object Label1: TLabel
      Left = 16
      Top = 52
      Width = 269
      Height = 24
      Alignment = taRightJustify
      Caption = '1.'#35835#21462#8220#34987#26597#35810#65288'TXT'#65289#25991#26412#8221#65306
    end
    object lblUseTime: TLabel
      Left = 13
      Top = 17
      Width = 140
      Height = 24
      Caption = #22788#29702#25152#38656#26102#38388#65306
    end
    object Label3: TLabel
      Left = 16
      Top = 87
      Width = 293
      Height = 24
      Alignment = taRightJustify
      Caption = '2.'#35774#32622#8220#26597#35810#65288#24635#21644#21306#22495#65289#8221#21015#25968#65306
    end
    object Label4: TLabel
      Left = 16
      Top = 122
      Width = 293
      Height = 24
      Alignment = taRightJustify
      Caption = '3.'#35774#32622#8220#26597#35810#65288#31532#19968#21306#22495#65289#8221#21015#25968#65306
    end
    object edtFileName: TEdit
      Left = 291
      Top = 52
      Width = 1014
      Height = 29
      Anchors = [akLeft, akTop, akRight]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
      OnClick = edtFileNameClick
    end
    object edtMaxValue: TEdit
      Left = 308
      Top = 87
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
    object edtFirstRangeValue: TEdit
      Left = 308
      Top = 122
      Width = 50
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
    object chkCompareCrossRange: TCheckBox
      Left = 16
      Top = 152
      Width = 137
      Height = 25
      Caption = #27604#36739#36328#21306#22495
      TabOrder = 3
    end
  end
  object OpenDialog: TOpenDialog
    Filter = #25991#26412#25991#20214'|*.txt'
    Left = 896
    Top = 10
  end
end
