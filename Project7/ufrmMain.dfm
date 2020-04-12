object frmMain: TfrmMain
  Left = 0
  Top = 0
  ClientHeight = 707
  ClientWidth = 1312
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
  PixelsPerInch = 96
  TextHeight = 24
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 1312
    Height = 200
    HorzScrollBar.Range = 1274
    HorzScrollBar.Visible = False
    VertScrollBar.Range = 150
    VertScrollBar.Tracking = True
    Align = alTop
    AutoScroll = False
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    TabOrder = 0
    DesignSize = (
      1312
      200)
    object Label1: TLabel
      Left = 19
      Top = 54
      Width = 532
      Height = 24
      Caption = '1. '#35835#21462#12304#65288' '#25991#20214#22841' '#65289#20869' 1-N '#20010' [  '#26597#35810#65288' TXT '#65289#25991#26412' ] '#12305#65306
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
      Top = 124
      Width = 1255
      Height = 24
      Caption = 
        '3. '#35774#32622' [ '#26597#35810#65288' '#31532' 1 '#21306#22495' '#65289' ] '#65288' '#21015#25968' '#65289#33539#22260' '#65306'1 '#8594'                    '#21015' [ '#65288' '#31532' ' +
        '1'#12289'3 '#27169#24335' '#65289'1 '#8594' 256 '#21015#20869' '#65307#65288' '#31532' 2'#12289'4 '#27169#24335' '#65289'1 '#8594' 150 '#21015#20869' ] '
    end
    object Label4: TLabel
      Left = 19
      Top = 159
      Width = 935
      Height = 24
      Caption = 
        '4. '#35774#32622' [ '#26597#35810#65288' '#31532' 2 '#21306#22495' '#65289' ] '#65288' '#21015#25968' '#65289#33539#22260' '#65306'1 '#8594'                    '#21015' [ '#65288' '#31532' ' +
        '2'#12289'4 '#27169#24335' '#65289'1 '#8594' 106 '#21015#20869' ]'
    end
    object Label2: TLabel
      Left = 19
      Top = 89
      Width = 532
      Height = 24
      Caption = '2. '#35835#21462#12304#65288' '#25991#20214#22841' '#65289#20869' 1-N '#20010' [  '#27604#36739#65288' TXT '#65289#25991#26412' ] '#12305#65306
    end
    object edtFileDirectory: TEdit
      Left = 550
      Top = 54
      Width = 717
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
      Left = 498
      Top = 124
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
      Left = 498
      Top = 159
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
    object edtFileDirectory2: TEdit
      Left = 550
      Top = 89
      Width = 717
      Height = 29
      Anchors = [akLeft, akTop, akRight]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 3
      OnClick = edtFileDirectoryClick
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 200
    Width = 1312
    Height = 507
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = '  5. '#26597#35810' '#65306#30456#21516#65288' '#21015#25968#23383' '#65289#25968#37327' '#12304' '#27880#65306#21508#21306#22495#65288' '#20998#24320' '#65289#12305'         '
      object Label18: TLabel
        Left = 19
        Top = 8
        Width = 1226
        Height = 24
        Caption = 
          '5-1. '#9312'. '#35774#32622' [ '#26597#35810#65288' '#31532' 1 '#21306#22495' '#65289'] '#30456#21516#65288' '#21015#25968#23383' '#65289#25968#37327' '#65306#65288' '#22823#12289#31561#20110' '#8805' '#65289'              ' +
          '      '#20010' [ '#27880' '#65306#65288' '#31532' 1'#12289'3 '#27169#24335' '#65289#21482#22635' '#65306'5.'#35774#32622' ... ] '
      end
      object Label19: TLabel
        Left = 19
        Top = 43
        Width = 1237
        Height = 24
        Caption = 
          '5-1. '#9313'. '#35774#32622' [ '#26597#35810#65288' '#31532' 2 '#21306#22495' '#65289'] '#30456#21516#65288' '#21015#25968#23383' '#65289#25968#37327' '#65306#65288' '#22823#12289#31561#20110' '#8805' '#65289'              ' +
          '      '#20010' [ '#27880' '#65306#65288' '#31532' 2'#12289'4 '#27169#24335' '#65289#35201#22635' '#65306'5.6.'#35774#32622' ... ]'
      end
      object Label16: TLabel
        Left = 19
        Top = 213
        Width = 231
        Height = 24
        Caption = '5-'#65288'1-3'#65289'. '#9314'. '#38468#21152#35774#32622' '#65306
      end
      object Label20: TLabel
        Left = 19
        Top = 243
        Width = 952
        Height = 24
        Caption = 
          '5-'#65288'1-3'#65289'. '#9312'. '#35774#32622' [ '#26597#35810#65288' '#31532' 1 '#21306#22495' '#65289' ] '#30456#21516#65288' '#21015#25968#23383' '#65289#25968#37327'  ...'#65288' '#19981' '#65289#31526#21512#65288' '#35774#32622#26465#20214' '#65289#30340 +
          #24773#20917#19979' '#65306
      end
      object Label21: TLabel
        Left = 19
        Top = 273
        Width = 1123
        Height = 24
        Caption = 
          '5-'#65288'1-3'#65289'. '#9313'. '#35774#32622' [ '#26597#35810#65288' '#31532' 2 '#21306#22495' '#65289' ] '#30456#21516#65288' '#21015#25968#23383' '#65289#25968#37327' '#65306#65288' '#22823#12289#31561#20110' '#8805' '#65289'         ' +
          '         '#20010' '#65292#25165#20801#35768#65288' '#23548#20986' '#65289#27492#34892' '#12290
      end
      object Label22: TLabel
        Left = 19
        Top = 78
        Width = 1226
        Height = 24
        Caption = 
          '5-2. '#9312'. '#35774#32622' [ '#26597#35810#65288' '#31532' 1 '#21306#22495' '#65289'] '#30456#21516#65288' '#21015#25968#23383' '#65289#25968#37327' '#65306#65288' '#22823#12289#31561#20110' '#8805' '#65289'              ' +
          '      '#20010' [ '#27880' '#65306#65288' '#31532' 1'#12289'3 '#27169#24335' '#65289#21482#22635' '#65306'5.'#35774#32622' ... ] '
      end
      object Label23: TLabel
        Left = 19
        Top = 113
        Width = 1237
        Height = 24
        Caption = 
          '5-2. '#9313'. '#35774#32622' [ '#26597#35810#65288' '#31532' 2 '#21306#22495' '#65289'] '#30456#21516#65288' '#21015#25968#23383' '#65289#25968#37327' '#65306#65288' '#22823#12289#31561#20110' '#8805' '#65289'              ' +
          '      '#20010' [ '#27880' '#65306#65288' '#31532' 2'#12289'4 '#27169#24335' '#65289#35201#22635' '#65306'5.6.'#35774#32622' ... ]'
      end
      object Label24: TLabel
        Left = 19
        Top = 148
        Width = 1226
        Height = 24
        Caption = 
          '5-3. '#9312'. '#35774#32622' [ '#26597#35810#65288' '#31532' 1 '#21306#22495' '#65289'] '#30456#21516#65288' '#21015#25968#23383' '#65289#25968#37327' '#65306#65288' '#22823#12289#31561#20110' '#8805' '#65289'              ' +
          '      '#20010' [ '#27880' '#65306#65288' '#31532' 1'#12289'3 '#27169#24335' '#65289#21482#22635' '#65306'5.'#35774#32622' ... ] '
      end
      object Label25: TLabel
        Left = 19
        Top = 183
        Width = 1237
        Height = 24
        Caption = 
          '5-3. '#9313'. '#35774#32622' [ '#26597#35810#65288' '#31532' 2 '#21306#22495' '#65289'] '#30456#21516#65288' '#21015#25968#23383' '#65289#25968#37327' '#65306#65288' '#22823#12289#31561#20110' '#8805' '#65289'              ' +
          '      '#20010' [ '#27880' '#65306#65288' '#31532' 2'#12289'4 '#27169#24335' '#65289#35201#22635' '#65306'5.6.'#35774#32622' ... ]'
      end
      object edtSeparateSameValueCount: TEdit
        Left = 714
        Top = 8
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
      object edtSeparateSameValueCount2: TEdit
        Left = 714
        Top = 43
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
      object btnCompare: TButton
        Left = 494
        Top = 316
        Width = 89
        Height = 41
        Caption = #30830#23450
        TabOrder = 2
        OnClick = btnCompareClick
      end
      object edtMinSeparateSameValueCount: TEdit
        Left = 770
        Top = 273
        Width = 100
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
      object edtSeparateSameValueCount3: TEdit
        Left = 714
        Top = 78
        Width = 100
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
      object edtSeparateSameValueCount4: TEdit
        Left = 714
        Top = 113
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
      object edtSeparateSameValueCount5: TEdit
        Left = 714
        Top = 148
        Width = 100
        Height = 29
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Tahoma'
        Font.Style = []
        NumbersOnly = True
        ParentFont = False
        TabOrder = 6
      end
      object edtSeparateSameValueCount6: TEdit
        Left = 714
        Top = 183
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
    end
    object TabSheet2: TTabSheet
      Caption = '        6. '#26597#35810' '#65306#30456#21516#65288' '#21015#25968#23383' '#65289#25968#37327' '#12304' '#27880#65306#21508#21306#22495#65288' '#21512#24182' '#65289#12305'           '
      ImageIndex = 1
      object Label10: TLabel
        Left = 19
        Top = 7
        Width = 1173
        Height = 24
        Caption = 
          '5-1. '#9312'. '#35774#32622' [ '#26597#35810#65288' '#31532' 1 '#21306#22495' '#65289'] '#30456#21516#65288' '#21015#25968#23383' '#65289#25968#37327' '#65306'                     '#8594'  ' +
          '                   '#20010' [ '#65288' '#31532' 1'#12289'3 '#27169#24335' '#65289#21482#22635' '#65306'5.'#35774#32622' ... ]'
      end
      object Label11: TLabel
        Left = 19
        Top = 42
        Width = 1190
        Height = 24
        Caption = 
          '5-1. '#9313'. '#35774#32622' [ '#26597#35810#65288' '#31532' 2 '#21306#22495' '#65289'] '#30456#21516#65288' '#21015#25968#23383' '#65289#25968#37327' '#65306'                     '#8594'  ' +
          '                   '#20010' [ '#65288' '#31532' 2'#12289'4 '#27169#24335' '#65289#35201#22635' '#65306'5.6.'#35774#32622' ... ]'
      end
      object Label5: TLabel
        Left = 19
        Top = 77
        Width = 1173
        Height = 24
        Caption = 
          '5-2. '#9312'. '#35774#32622' [ '#26597#35810#65288' '#31532' 1 '#21306#22495' '#65289'] '#30456#21516#65288' '#21015#25968#23383' '#65289#25968#37327' '#65306'                     '#8594'  ' +
          '                   '#20010' [ '#65288' '#31532' 1'#12289'3 '#27169#24335' '#65289#21482#22635' '#65306'5.'#35774#32622' ... ]'
      end
      object Label6: TLabel
        Left = 19
        Top = 112
        Width = 1190
        Height = 24
        Caption = 
          '5-2. '#9313'. '#35774#32622' [ '#26597#35810#65288' '#31532' 2 '#21306#22495' '#65289'] '#30456#21516#65288' '#21015#25968#23383' '#65289#25968#37327' '#65306'                     '#8594'  ' +
          '                   '#20010' [ '#65288' '#31532' 2'#12289'4 '#27169#24335' '#65289#35201#22635' '#65306'5.6.'#35774#32622' ... ]'
      end
      object Label7: TLabel
        Left = 19
        Top = 147
        Width = 1173
        Height = 24
        Caption = 
          '5-3. '#9312'. '#35774#32622' [ '#26597#35810#65288' '#31532' 1 '#21306#22495' '#65289'] '#30456#21516#65288' '#21015#25968#23383' '#65289#25968#37327' '#65306'                     '#8594'  ' +
          '                   '#20010' [ '#65288' '#31532' 1'#12289'3 '#27169#24335' '#65289#21482#22635' '#65306'5.'#35774#32622' ... ]'
      end
      object Label8: TLabel
        Left = 19
        Top = 182
        Width = 1190
        Height = 24
        Caption = 
          '5-3. '#9313'. '#35774#32622' [ '#26597#35810#65288' '#31532' 2 '#21306#22495' '#65289'] '#30456#21516#65288' '#21015#25968#23383' '#65289#25968#37327' '#65306'                     '#8594'  ' +
          '                   '#20010' [ '#65288' '#31532' 2'#12289'4 '#27169#24335' '#65289#35201#22635' '#65306'5.6.'#35774#32622' ... ]'
      end
      object Label9: TLabel
        Left = 19
        Top = 217
        Width = 1173
        Height = 24
        Caption = 
          '5-4. '#9312'. '#35774#32622' [ '#26597#35810#65288' '#31532' 1 '#21306#22495' '#65289'] '#30456#21516#65288' '#21015#25968#23383' '#65289#25968#37327' '#65306'                     '#8594'  ' +
          '                   '#20010' [ '#65288' '#31532' 1'#12289'3 '#27169#24335' '#65289#21482#22635' '#65306'5.'#35774#32622' ... ]'
      end
      object Label12: TLabel
        Left = 19
        Top = 252
        Width = 1190
        Height = 24
        Caption = 
          '5-4. '#9313'. '#35774#32622' [ '#26597#35810#65288' '#31532' 2 '#21306#22495' '#65289'] '#30456#21516#65288' '#21015#25968#23383' '#65289#25968#37327' '#65306'                     '#8594'  ' +
          '                   '#20010' [ '#65288' '#31532' 2'#12289'4 '#27169#24335' '#65289#35201#22635' '#65306'5.6.'#35774#32622' ... ]'
      end
      object Label13: TLabel
        Left = 19
        Top = 287
        Width = 1173
        Height = 24
        Caption = 
          '5-5. '#9312'. '#35774#32622' [ '#26597#35810#65288' '#31532' 1 '#21306#22495' '#65289'] '#30456#21516#65288' '#21015#25968#23383' '#65289#25968#37327' '#65306'                     '#8594'  ' +
          '                   '#20010' [ '#65288' '#31532' 1'#12289'3 '#27169#24335' '#65289#21482#22635' '#65306'5.'#35774#32622' ... ]'
      end
      object Label14: TLabel
        Left = 19
        Top = 322
        Width = 1190
        Height = 24
        Caption = 
          '5-5. '#9313'. '#35774#32622' [ '#26597#35810#65288' '#31532' 2 '#21306#22495' '#65289'] '#30456#21516#65288' '#21015#25968#23383' '#65289#25968#37327' '#65306'                     '#8594'  ' +
          '                   '#20010' [ '#65288' '#31532' 2'#12289'4 '#27169#24335' '#65289#35201#22635' '#65306'5.6.'#35774#32622' ... ]'
      end
      object Label15: TLabel
        Left = 19
        Top = 357
        Width = 1173
        Height = 24
        Caption = 
          '5-6. '#9312'. '#35774#32622' [ '#26597#35810#65288' '#31532' 1 '#21306#22495' '#65289'] '#30456#21516#65288' '#21015#25968#23383' '#65289#25968#37327' '#65306'                     '#8594'  ' +
          '                   '#20010' [ '#65288' '#31532' 1'#12289'3 '#27169#24335' '#65289#21482#22635' '#65306'5.'#35774#32622' ... ]'
      end
      object Label17: TLabel
        Left = 19
        Top = 392
        Width = 1190
        Height = 24
        Caption = 
          '5-6. '#9313'. '#35774#32622' [ '#26597#35810#65288' '#31532' 2 '#21306#22495' '#65289'] '#30456#21516#65288' '#21015#25968#23383' '#65289#25968#37327' '#65306'                     '#8594'  ' +
          '                   '#20010' [ '#65288' '#31532' 2'#12289'4 '#27169#24335' '#65289#35201#22635' '#65306'5.6.'#35774#32622' ... ]'
      end
      object btnCompare2: TButton
        Left = 564
        Top = 427
        Width = 89
        Height = 41
        Caption = #30830#23450
        TabOrder = 0
        OnClick = btnCompareClick
      end
      object edtSameValueCount: TEdit
        Left = 564
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
        TabOrder = 1
      end
      object edtSameValueCount3: TEdit
        Left = 564
        Top = 42
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
      object edtSameValueCount2: TEdit
        Left = 714
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
        TabOrder = 3
      end
      object edtSameValueCount4: TEdit
        Left = 714
        Top = 42
        Width = 100
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
      object edtSameValueCount5: TEdit
        Left = 564
        Top = 77
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
      object edtSameValueCount7: TEdit
        Left = 564
        Top = 112
        Width = 100
        Height = 29
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Tahoma'
        Font.Style = []
        NumbersOnly = True
        ParentFont = False
        TabOrder = 6
      end
      object edtSameValueCount6: TEdit
        Left = 714
        Top = 77
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
      object edtSameValueCount8: TEdit
        Left = 714
        Top = 112
        Width = 100
        Height = 29
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Tahoma'
        Font.Style = []
        NumbersOnly = True
        ParentFont = False
        TabOrder = 8
      end
      object edtSameValueCount9: TEdit
        Left = 564
        Top = 147
        Width = 100
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
      object edtSameValueCount11: TEdit
        Left = 564
        Top = 182
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
      object edtSameValueCount10: TEdit
        Left = 714
        Top = 147
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
      object edtSameValueCount12: TEdit
        Left = 714
        Top = 182
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
      object edtSameValueCount13: TEdit
        Left = 564
        Top = 217
        Width = 100
        Height = 29
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Tahoma'
        Font.Style = []
        NumbersOnly = True
        ParentFont = False
        TabOrder = 13
      end
      object edtSameValueCount15: TEdit
        Left = 564
        Top = 252
        Width = 100
        Height = 29
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Tahoma'
        Font.Style = []
        NumbersOnly = True
        ParentFont = False
        TabOrder = 14
      end
      object edtSameValueCount14: TEdit
        Left = 714
        Top = 217
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
      object edtSameValueCount16: TEdit
        Left = 714
        Top = 252
        Width = 100
        Height = 29
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Tahoma'
        Font.Style = []
        NumbersOnly = True
        ParentFont = False
        TabOrder = 16
      end
      object edtSameValueCount17: TEdit
        Left = 564
        Top = 287
        Width = 100
        Height = 29
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Tahoma'
        Font.Style = []
        NumbersOnly = True
        ParentFont = False
        TabOrder = 17
      end
      object edtSameValueCount19: TEdit
        Left = 564
        Top = 322
        Width = 100
        Height = 29
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Tahoma'
        Font.Style = []
        NumbersOnly = True
        ParentFont = False
        TabOrder = 18
      end
      object edtSameValueCount18: TEdit
        Left = 714
        Top = 287
        Width = 100
        Height = 29
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Tahoma'
        Font.Style = []
        NumbersOnly = True
        ParentFont = False
        TabOrder = 19
      end
      object edtSameValueCount20: TEdit
        Left = 714
        Top = 322
        Width = 100
        Height = 29
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Tahoma'
        Font.Style = []
        NumbersOnly = True
        ParentFont = False
        TabOrder = 20
      end
      object edtSameValueCount21: TEdit
        Left = 564
        Top = 357
        Width = 100
        Height = 29
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Tahoma'
        Font.Style = []
        NumbersOnly = True
        ParentFont = False
        TabOrder = 21
      end
      object edtSameValueCount23: TEdit
        Left = 564
        Top = 392
        Width = 100
        Height = 29
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Tahoma'
        Font.Style = []
        NumbersOnly = True
        ParentFont = False
        TabOrder = 22
      end
      object edtSameValueCount22: TEdit
        Left = 714
        Top = 357
        Width = 100
        Height = 29
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Tahoma'
        Font.Style = []
        NumbersOnly = True
        ParentFont = False
        TabOrder = 23
      end
      object edtSameValueCount24: TEdit
        Left = 714
        Top = 392
        Width = 100
        Height = 29
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Tahoma'
        Font.Style = []
        NumbersOnly = True
        ParentFont = False
        TabOrder = 24
      end
    end
  end
  object OpenDialog: TOpenDialog
    Filter = #25991#26412#25991#20214'|*.txt'
    Left = 256
    Top = 10
  end
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 220
    Top = 10
  end
end
