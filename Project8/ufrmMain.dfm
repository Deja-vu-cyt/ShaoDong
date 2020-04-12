object frmMain: TfrmMain
  Left = 0
  Top = 0
  ClientHeight = 736
  ClientWidth = 1327
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
  OnMouseWheel = FormMouseWheel
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 24
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 1327
    Height = 736
    HorzScrollBar.Range = 1274
    HorzScrollBar.Visible = False
    VertScrollBar.Range = 800
    VertScrollBar.Tracking = True
    Align = alClient
    AutoScroll = False
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    TabOrder = 0
    DesignSize = (
      1310
      736)
    object Label1: TLabel
      Left = 19
      Top = 54
      Width = 514
      Height = 24
      Caption = '1. '#35835#21462' ['#65288' '#25991#20214#22841' '#65289#20869' 1 - N '#20010#8220' '#26597#35810#65288' TXT '#65289#25991#26412' '#8221'] '#65306
    end
    object lblUseTime: TLabel
      Left = 19
      Top = 16
      Width = 140
      Height = 24
      Caption = #22788#29702#25152#38656#26102#38388#65306
    end
    object Label3: TLabel
      Left = 19
      Top = 92
      Width = 1094
      Height = 24
      Caption = 
        '2. '#35774#32622' '#8220' '#26597#35810#65288' '#31532' 1 '#21306#22495' '#65289#8221#21015#25968' '#65306'                 '#20010' [ '#27880' '#65306#65288' '#31532' 1'#12289'3 '#27169#24335' '#65289#21015#25968#33539 +
        #22260#65288' 1 - 256 '#65289#21015' '#65292#21482#22635' '#65306'2.'#35774#32622' ... ] '
    end
    object Label4: TLabel
      Left = 19
      Top = 127
      Width = 1233
      Height = 24
      Caption = 
        '3. '#35774#32622' '#8220' '#26597#35810#65288' '#31532' 2 '#21306#22495' '#65289#8221#21015#25968' '#65306'                 '#20010' [ '#27880' '#65306#65288' '#31532' 2'#12289'4 '#27169#24335' '#65289#21015#25968#33539 +
        #22260#65288' 1 - 150 '#65289'-'#65288' 1 - 106 '#65289#21015' '#65292#35201#22635' '#65306'2.3.'#35774#32622' ... ] '
    end
    object edtFileDirectory: TEdit
      Left = 550
      Top = 54
      Width = 746
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
      OnClick = edtFileDirectoryClick
    end
    object edtIntervalValue: TEdit
      Left = 346
      Top = 92
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
    object edtIntervalValue2: TEdit
      Left = 346
      Top = 127
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
    object btnCompare: TButton
      Left = 19
      Top = 751
      Width = 494
      Height = 41
      Caption = '5.  '#26681#25454' '#65306#65288'1'#65289#65288'0-0'#65289'-'#65288'1-44'#65289'. '#35774#32622' ... '#23548#20986#25968#25454
      TabOrder = 3
      OnClick = btnCompareClick
    end
    object btnExportSettings2: TButton
      Left = 19
      Top = 209
      Width = 1270
      Height = 41
      Caption = 
        #65288'2'#65289#65288'0-1'#65289'. '#35774#32622' [ '#26597#35810#65288#21015#25968#23383#65289#65292#65288#21508#65289#34892#20043#38388' '#65292#12304' N '#12305#8594#12304' N '#12305#20010#65288#30456#21516#65289#21015#25968#23383' ]            ' +
        '                                                             '
      TabOrder = 4
      OnClick = btnExportSettings2Click
    end
    object btnExportSettings5: TButton
      Left = 19
      Top = 350
      Width = 1270
      Height = 41
      Caption = 
        #65288'5'#65289#65288'0-0'#65289'-'#65288'1-7'#65289'. '#35774#32622#12304' '#26597#35810#65288#31532' N-N '#34892#20026#39318#34892#65289#30452#36830' ['#65288#31532' N-1 '#34892#65289#65288#31532' N-1 '#30452#36830#34892#65289#12305#65288#21508#65289#12304' ' +
        'N '#12305#34892#20043#38388#65306#12304' N '#12305'                     '
      TabOrder = 5
      OnClick = btnExportSettings5Click
    end
    object btnExportSettings6: TButton
      Left = 19
      Top = 397
      Width = 1270
      Height = 41
      Caption = 
        #65288'6'#65289#65288'0-0'#65289'-'#65288'1-7'#65289'. '#35774#32622#12304' '#26597#35810#65288#31532' N-N '#34892#20026#39318#34892#65289#26012#36830' ['#65288#31532' N-1 '#34892#65289#65288#31532' N-1 '#26012#36830#34892#65289#12305#65288#21508#65289#12304' ' +
        'N '#12305#34892#20043#38388#65306#12304' N '#12305'                     '
      TabOrder = 6
      OnClick = btnExportSettings6Click
    end
    object btnExportSettings: TButton
      Left = 19
      Top = 162
      Width = 1270
      Height = 41
      Caption = 
        #65288'1'#65289#65288'0-1'#65289'-'#65288'1-8'#65289'. '#35774#32622#12304' '#26597#35810#65288#19978#19979#21508#34892#65289#26597#35810#34892#65306'N = N'#12289'N ... ( '#34892#38388#24046#65306'N ) [ '#27604#36739#34892#65306'N = ' +
        'N + N'#65288#19979#34892#24046#65306'N'#65289'] '#12305'                         '
      TabOrder = 7
      OnClick = btnExportSettingsClick
    end
    object btnExportSettings3: TButton
      Left = 19
      Top = 256
      Width = 1270
      Height = 41
      Caption = 
        #65288'3'#65289#65288'1-1'#65289'-'#65288'1-7'#65289'. '#35774#32622' [ '#26597#35810#65288#20998' N '#27573#65289#65292#65288#21508#65289#34892#20043#38388#65292#12304' N '#12305#8594#12304' N '#12305#20010#32452#21512#65288#27573#65289#65292#21482#23548#20986#12304' N '#12305 +
        #8594#12304' N '#12305#20010#65288#30456#21516#65289#21015#25968#23383' ]  '
      TabOrder = 8
      OnClick = btnExportSettings3Click
    end
    object btnExportSettings4: TButton
      Left = 19
      Top = 303
      Width = 1270
      Height = 41
      Caption = 
        #65288'4'#65289#65288'1-1'#65289'-'#65288'1-7'#65289'. '#35774#32622' [ '#26597#35810#65288#38500#20197' N '#65289#65292#65288#21508#65289#34892#20043#38388#65292#12304' N '#12305#8594#12304' N '#12305#20010#32452#21512#65288#32452#65289#65292#21482#23548#20986#12304' N '#12305 +
        #8594#12304' N '#12305#20010#65288#30456#21516#65289#21015#25968#23383' ]  '
      TabOrder = 9
      OnClick = btnExportSettings4Click
    end
    object btnExportSettings7: TButton
      Left = 19
      Top = 444
      Width = 1270
      Height = 41
      Caption = 
        #65288'7-8'#65289'. '#35774#32622' [ '#26597#35810#65288#21333#12289#21452#25968#65289#65292#65288#21508#65289#34892#20043#38388#65292#12304' N '#12305#8594#12304' N '#12305#20010#65288#30456#21516#65289#20010#25968' ]                ' +
        '                                                                ' +
        '  '
      TabOrder = 10
      OnClick = btnExportSettings7Click
    end
    object btnExportSettings8: TButton
      Left = 19
      Top = 491
      Width = 1270
      Height = 41
      Caption = 
        #65288'9-10'#65289'. '#35774#32622' [ '#26597#35810#65288#22823#12289#23567#21495#30721#65289#65292#65288#21508#65289#34892#20043#38388#65292#12304' N '#12305#8594#12304' N '#12305#20010#65288#30456#21516#65289#20010#25968' ]              ' +
        '                                                               '
      TabOrder = 11
      OnClick = btnExportSettings8Click
    end
    object btnExportSettings9: TButton
      Left = 19
      Top = 538
      Width = 1270
      Height = 41
      Caption = 
        #65288'11'#65289'. '#35774#32622' [ '#26597#35810#65288#30456#36830#21495#30721#65289#65292#65288#21508#65289#34892#20043#38388' '#65292#12304' N '#12305#8594#12304' N '#12305#20010#65288#30456#21516#65289#20010#25968' ]                ' +
        '                                                                ' +
        '  '
      TabOrder = 12
      OnClick = btnExportSettings9Click
    end
    object btnExportSettings10: TButton
      Left = 19
      Top = 585
      Width = 1270
      Height = 41
      Caption = 
        #65288'12'#65289'. '#35774#32622' [ '#26597#35810#65288#31561#24046#21495#30721#65289#65292#65288#21508#65289#34892#20043#38388' '#65292#12304' N '#12305#8594#12304' N '#12305#20010#65288#30456#21516#65289#20010#25968' ]                ' +
        '                                                                ' +
        '  '
      TabOrder = 13
      OnClick = btnExportSettings10Click
    end
    object btnExportSettings11: TButton
      Left = 19
      Top = 632
      Width = 1270
      Height = 41
      Caption = 
        #65288'13'#65289'. '#35774#32622' [ '#26597#35810#65288#20010#20301#65289#65292#65288#21508#65289#34892#20043#38388' '#65292#12304' N '#12305#8594#12304' N '#12305#20010#65288#30456#21516#65289#20010#25968' ]                  ' +
        '                                                                ' +
        '      '
      TabOrder = 14
      OnClick = btnExportSettings11Click
    end
    object chkExportFile254: TCheckBox
      Left = 19
      Top = 679
      Width = 780
      Height = 30
      Caption = #65288' 0 '#65289'4 . '#31532#65288' 2 '#65289'-'#65288' N '#65289#39033#20013#12304#65288' '#21508#34892' '#65289#34892#21495' N = N ...'#65288' '#24635#21512#24182' '#65289#12305#65288' 1-3 '#65289
      TabOrder = 15
    end
    object chkExportFile255: TCheckBox
      Left = 19
      Top = 715
      Width = 780
      Height = 30
      Caption = #65288' 0 '#65289'5 . '#31532#65288' 2 '#65289'-'#65288' N '#65289#39033#20013#12304#65288' '#30456#21516' '#65289#34892#21495' N = N ...'#65288' '#24635#21512#24182' '#65289#12305#65288' 1-3 '#65289
      TabOrder = 16
    end
  end
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 220
    Top = 10
  end
end
