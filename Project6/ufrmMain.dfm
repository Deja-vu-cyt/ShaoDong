object frmMain: TfrmMain
  Left = 0
  Top = 0
  ClientHeight = 690
  ClientWidth = 1334
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -20
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 24
  object PageControl1: TPageControl
    Left = 0
    Top = 201
    Width = 1334
    Height = 489
    ActivePage = TabSheet3
    Align = alClient
    TabHeight = 40
    TabOrder = 0
    ExplicitHeight = 519
    object TabSheet3: TTabSheet
      Caption = #19968'.'#26597#35810#65306#12304#21508#34892#38388#12305#30452#12289#26012#36830#12304#23545#24212#21015#12305#32452#21512'     '
      ImageIndex = 2
      ExplicitHeight = 469
      object Label10: TLabel
        Left = 15
        Top = 7
        Width = 813
        Height = 24
        Caption = '4.'#35774#32622#65306#26597#35810#12304#65288#31532'N-N'#34892#20026#39318#34892#65289#30452#36830' ['#65288#31532'N-1'#34892#65289#65288#31532'1-N'#30452#36830#34892#65289']'#12305#21508#12304' N'#12305#34892#20043#38388#65306
      end
      object Label12: TLabel
        Left = 15
        Top = 41
        Width = 473
        Height = 24
        Caption = #65288'1'#65289'.'#65288#30452#36830#65289'y'#26377'          '#8594'           '#20010#12304#23545#24212#21015#12305#25968#23383
      end
      object Label14: TLabel
        Left = 15
        Top = 76
        Width = 813
        Height = 24
        Caption = '5.'#35774#32622#65306#26597#35810#12304#65288#31532'N-N'#34892#20026#39318#34892#65289#26012#36830' ['#65288#31532'N-1'#34892#65289#65288#31532'1-N'#26012#36830#34892#65289']'#12305#21508#12304' N'#12305#34892#20043#38388#65306
      end
      object Label15: TLabel
        Left = 15
        Top = 110
        Width = 473
        Height = 24
        Caption = #65288'1'#65289'.'#65288#26012#36830#65289'y'#26377'          '#8594'           '#20010#12304#23545#24212#21015#12305#25968#23383
      end
      object Label18: TLabel
        Left = 15
        Top = 145
        Width = 1296
        Height = 24
        Caption = 
          '6.'#35774#32622#65288#31532' 1- N '#34892#20026#39318#34892#65289#20381#27425#36941#21382#65288#31532' 2- N '#34892#20026#39318#34892#65289#20986#65288#30456#21516#30340#20195#21495#32452#21512#65289#65307#36941#21382#27425#25968' '#65306'           '#27425' ' +
          '[ '#27880#65306#26368#22909#36941#21382' 1-3 '#27425#65292#38480#21046#22312' 10 '#27425#20869' ]'
      end
      object Label6: TLabel
        Left = 15
        Top = 312
        Width = 950
        Height = 24
        Caption = 
          '7.'#35774#32622#8220#23548#20986#12304#65288#31532'N-N'#34892#20026#39318#34892#65289#30452#12289#26012#36830' ['#65288#31532'N-1'#34892#65289#65288#31532'1-N'#30452#12289#26012#36830#34892#65289']'#12305#21015'          '#33267'        ' +
          '  '#20010#32452#21512#8221
      end
      object Label16: TLabel
        Left = 15
        Top = 180
        Width = 607
        Height = 24
        Caption = '6-1.'#35774#32622#65306#26242#20445#30041#65288#24182#65289#23548#20986#65288' '#31526#21512#35774#32622' '#65289#30340'          '#20010#20197#19978#32452#21512#30340#34892
      end
      object edtVVertSameValueCount2: TEdit
        Left = 270
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
        Left = 189
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
        Left = 270
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
        Left = 189
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
      object edtVertSlantGroupCount: TEdit
        Left = 859
        Top = 145
        Width = 50
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
      object edtVertSlantExportCodeNameValueCount: TEdit
        Left = 762
        Top = 312
        Width = 50
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
      object edtVertSlantExportCodeNameValueCount2: TEdit
        Left = 842
        Top = 312
        Width = 50
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
      object btnVertSlantCompare: TButton
        Left = 15
        Top = 378
        Width = 482
        Height = 40
        Caption = '7-2.'#8220#26681#25454#65306'1- 7.'#35774#32622#8220#23548#20986' ...'#8220'N-N'#8221#20010#32452#21512#8221#23548#20986#25968#25454
        TabOrder = 9
        OnClick = btnCompareClick
      end
      object chkVertSlantExportSource: TCheckBox
        Left = 15
        Top = 347
        Width = 660
        Height = 25
        Caption = '7-1. '#21482#23384#20648#65288#24182#65289#23548#20986#65288' '#26368#26411#27425#36941#21382' '#65289#65306#31526#21512' 1- 7.'#35774#32622' ...'#26465#20214#30340#25968#25454
        TabOrder = 10
      end
      object edtVertSlantKeepCodeNameValueCount: TEdit
        Left = 428
        Top = 180
        Width = 50
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
      object btnVertSlantCompare2: TButton
        Left = 15
        Top = 260
        Width = 800
        Height = 40
        Caption = '6-3.'#35774#32622' '#65306#26242#20445#30041#65288#24182#65289#23548#20986#65288' '#31526#21512#35774#32622' '#65289#30340#12304' '#22635#20837#65306'N '#65307#20363' '#65306'20 '#12305#20010#20197#19978#32452#21512#30340#34892
        TabOrder = 12
        OnClick = btnCompareClick
      end
      object btnVertSlantGroupCodeNameSettings: TButton
        Left = 15
        Top = 214
        Width = 285
        Height = 40
        Caption = '6-2'#65288'1'#65289'.'#35774#32622#65288' '#36941#21382' '#65289#26465#20214
        TabOrder = 13
        OnClick = btnVertSlantGroupCodeNameSettingsClick
      end
      object btnVertSlantGroupCodeNameSettings2: TButton
        Left = 329
        Top = 214
        Width = 285
        Height = 40
        Caption = '6-2'#65288'2'#65289'.'#35774#32622#65288' '#36941#21382' '#65289#26465#20214
        TabOrder = 14
        OnClick = btnVertSlantGroupCodeNameSettings2Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = '  '#20108'.'#26597#35810#65306#12304#21508#34892#38388#12305#30452#36830#12304#23545#24212#21015#12305#32452#21512'      '
      ImageIndex = 1
      ExplicitHeight = 469
      object Label7: TLabel
        Left = 15
        Top = 15
        Width = 813
        Height = 24
        Caption = '4.'#35774#32622#65306#26597#35810#12304#65288#31532'N-N'#34892#20026#39318#34892#65289#30452#36830' ['#65288#31532'N-1'#34892#65289#65288#31532'1-N'#30452#36830#34892#65289']'#12305#21508#12304' N'#12305#34892#20043#38388#65306
      end
      object Label9: TLabel
        Left = 15
        Top = 49
        Width = 473
        Height = 24
        Caption = #65288'1'#65289'.'#65288#30452#36830#65289'y'#26377'          '#8594'           '#20010#12304#23545#24212#21015#12305#25968#23383
      end
      object Label8: TLabel
        Left = 15
        Top = 84
        Width = 1296
        Height = 24
        Caption = 
          '5.'#35774#32622#65288#31532' 1- N '#34892#20026#39318#34892#65289#20381#27425#36941#21382#65288#31532' 2- N '#34892#20026#39318#34892#65289#20986#65288#30456#21516#30340#20195#21495#32452#21512#65289#65307#36941#21382#27425#25968' '#65306'           '#27425' ' +
          '[ '#27880#65306#26368#22909#36941#21382' 1-3 '#27425#65292#38480#21046#22312' 10 '#27425#20869' ]'
      end
      object Label11: TLabel
        Left = 15
        Top = 282
        Width = 870
        Height = 24
        Caption = 
          '6.'#35774#32622#8220#23548#20986#12304#65288#31532'N-N'#34892#20026#39318#34892#65289#30452#36830' ['#65288#31532'N-1'#34892#65289#65288#31532'1-N'#30452#36830#34892#65289']'#12305#21015'          '#33267'          '#20010#32452 +
          #21512#8221
      end
      object Label17: TLabel
        Left = 15
        Top = 114
        Width = 607
        Height = 24
        Caption = '5-1.'#35774#32622#65306#26242#20445#30041#65288#24182#65289#23548#20986#65288' '#31526#21512#35774#32622' '#65289#30340'          '#20010#20197#19978#32452#21512#30340#34892
      end
      object edtVertSameValueCount2: TEdit
        Left = 270
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
        TabOrder = 0
      end
      object edtVertSameValueCount: TEdit
        Left = 189
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
        TabOrder = 1
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
        TabOrder = 2
      end
      object edtVertGroupCount: TEdit
        Left = 860
        Top = 84
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
      object edtVertExportCodeNameValueCount: TEdit
        Left = 681
        Top = 282
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
      object edtVertExportCodeNameValueCount2: TEdit
        Left = 761
        Top = 282
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
      object btnVertCompare: TButton
        Left = 15
        Top = 348
        Width = 479
        Height = 40
        Caption = '6-2.'#8220#26681#25454#65306'1- 6.'#35774#32622#8220#23548#20986' ...'#8220'N-N'#8221#20010#32452#21512#8221#23548#20986#25968#25454
        TabOrder = 6
        OnClick = btnCompareClick
      end
      object chkVertExportSource: TCheckBox
        Left = 15
        Top = 317
        Width = 660
        Height = 25
        Caption = '6-1. '#21482#23384#20648#65288#24182#65289#23548#20986#65288' '#26368#26411#27425#36941#21382' '#65289#65306#31526#21512' 1- 6.'#35774#32622' ...'#26465#20214#30340#25968#25454
        TabOrder = 7
      end
      object edtVertKeepCodeNameValueCount: TEdit
        Left = 427
        Top = 114
        Width = 50
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
      object btnVertCompare2: TButton
        Left = 15
        Top = 236
        Width = 800
        Height = 40
        Caption = '5-3.'#35774#32622' '#65306#26242#20445#30041#65288#24182#65289#23548#20986#65288' '#31526#21512#35774#32622' '#65289#30340#12304' '#22635#20837#65306'N '#65307#20363' '#65306'20 '#12305#20010#20197#19978#32452#21512#30340#34892
        TabOrder = 9
        OnClick = btnCompareClick
      end
      object btnVertGroupCodeNameSettings: TButton
        Left = 15
        Top = 144
        Width = 285
        Height = 40
        Caption = '5-2'#65288'1'#65289'.'#35774#32622#65288' '#36941#21382' '#65289#26465#20214
        TabOrder = 10
        OnClick = btnVertGroupCodeNameSettingsClick
      end
      object btnVertGroupCodeNameSettings2: TButton
        Left = 15
        Top = 190
        Width = 285
        Height = 40
        Caption = '5-2'#65288'2'#65289'.'#35774#32622#65288' '#36941#21382' '#65289#26465#20214
        TabOrder = 11
        OnClick = btnVertGroupCodeNameSettings2Click
      end
    end
    object TabSheet1: TTabSheet
      Caption = '  '#19977'.'#26597#35810#65306#12304#21508#34892#38388#12305#26012#36830#12304#23545#24212#21015#12305#32452#21512'     '
      ExplicitHeight = 469
      object Label2: TLabel
        Left = 15
        Top = 50
        Width = 1296
        Height = 24
        Caption = 
          '5.'#35774#32622#65288#31532' 1- N '#34892#20026#39318#34892#65289#20381#27425#36941#21382#65288#31532' 2- N '#34892#20026#39318#34892#65289#20986#65288#30456#21516#30340#20195#21495#32452#21512#65289#65307#36941#21382#27425#25968' '#65306'           '#27425' ' +
          '[ '#27880#65306#26368#22909#36941#21382' 1-3 '#27425#65292#38480#21046#22312' 10 '#27425#20869' ]'
      end
      object Label5: TLabel
        Left = 15
        Top = 12
        Width = 813
        Height = 24
        Caption = '4.'#35774#32622#65306#26597#35810#12304#65288#31532'N-N'#34892#20026#39318#34892#65289#26012#36830' ['#65288#31532'N-1'#34892#65289#65288#31532'1-N'#26012#36830#34892#65289']'#12305#21508#12304' N'#12305#34892#20043#38388#65306
      end
      object Label13: TLabel
        Left = 15
        Top = 255
        Width = 870
        Height = 24
        Caption = 
          '6.'#35774#32622#8220#23548#20986#12304#65288#31532'N-N'#34892#20026#39318#34892#65289#26012#36830' ['#65288#31532'N-1'#34892#65289#65288#31532'1-N'#26012#36830#34892#65289']'#12305#21015'          '#33267'          '#20010#32452 +
          #21512#8221
      end
      object Label19: TLabel
        Left = 15
        Top = 87
        Width = 607
        Height = 24
        Caption = '5-1.'#35774#32622#65306#26242#20445#30041#65288#24182#65289#23548#20986#65288' '#31526#21512#35774#32622' '#65289#30340'          '#20010#20197#19978#32452#21512#30340#34892
      end
      object edtCompareSpacing: TEdit
        Left = 834
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
        TabOrder = 0
      end
      object edtSlantGroupCount: TEdit
        Left = 857
        Top = 50
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
      object edtSlantExportCodeNameValueCount: TEdit
        Left = 681
        Top = 255
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
      object edtSlantExportCodeNameValueCount2: TEdit
        Left = 761
        Top = 255
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
      object btnSlantCompare: TButton
        Left = 15
        Top = 321
        Width = 479
        Height = 40
        Caption = '6-2.'#8220#26681#25454#65306'1- 6.'#35774#32622#8220#23548#20986' ...'#8220'N-N'#8221#20010#32452#21512#8221#23548#20986#25968#25454
        TabOrder = 4
        OnClick = btnCompareClick
      end
      object chkSlantExportSource: TCheckBox
        Left = 15
        Top = 290
        Width = 660
        Height = 25
        Caption = '6-1. '#21482#23384#20648#65288#24182#65289#23548#20986#65288' '#26368#26411#27425#36941#21382' '#65289#65306#31526#21512' 1- 6.'#35774#32622' ...'#26465#20214#30340#25968#25454
        TabOrder = 5
      end
      object edtSlantKeepCodeNameValueCount: TEdit
        Left = 428
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
        TabOrder = 6
      end
      object btnSlantCompare2: TButton
        Left = 15
        Top = 209
        Width = 800
        Height = 40
        Caption = '5-3.'#35774#32622' '#65306#26242#20445#30041#65288#24182#65289#23548#20986#65288' '#31526#21512#35774#32622' '#65289#30340#12304' '#22635#20837#65306'N '#65307#20363' '#65306'20 '#12305#20010#20197#19978#32452#21512#30340#34892
        TabOrder = 7
        OnClick = btnCompareClick
      end
      object btnSlantGroupCodeNameSettings: TButton
        Left = 15
        Top = 117
        Width = 285
        Height = 40
        Caption = '5-2'#65288'1'#65289'.'#35774#32622#65288' '#36941#21382' '#65289#26465#20214
        TabOrder = 8
        OnClick = btnVertGroupCodeNameSettingsClick
      end
      object btnSlantGroupCodeNameSettings2: TButton
        Left = 15
        Top = 163
        Width = 285
        Height = 40
        Caption = '5-2'#65288'2'#65289'.'#35774#32622#65288' '#36941#21382' '#65289#26465#20214
        TabOrder = 9
        OnClick = btnVertGroupCodeNameSettings2Click
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 1334
    Height = 201
    Align = alTop
    TabOrder = 1
    DesignSize = (
      1334
      201)
    object Label1: TLabel
      Left = 19
      Top = 51
      Width = 269
      Height = 24
      Caption = '1.'#35835#21462#8220#34987#26597#35810#65288'TXT'#65289#25991#26412#8221#65306
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
      Top = 86
      Width = 293
      Height = 24
      Caption = '2.'#35774#32622#8220#26597#35810#65288#31532#19968#21306#22495#65289#8221#21015#25968#65306
    end
    object Label4: TLabel
      Left = 19
      Top = 121
      Width = 293
      Height = 24
      Caption = '3.'#35774#32622#8220#26597#35810#65288#31532#20108#21306#22495#65289#8221#21015#25968#65306
    end
    object edtFileName: TEdit
      Left = 294
      Top = 51
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
    object edtIntervalValue: TEdit
      Left = 311
      Top = 86
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
      Left = 311
      Top = 121
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
    object chkCompareCrossRange: TCheckBox
      Left = 19
      Top = 160
      Width = 530
      Height = 25
      Caption = #26597#35810#65306#26012#36830#34892#65288' '#27599#34892#30340#21015#25968#23383#65292#36328#21306#22495' '#65289'( '#31532' 2 '#27169#24335' ) '
      TabOrder = 3
    end
  end
  object OpenDialog: TOpenDialog
    Filter = #25991#26412#25991#20214'|*.txt'
    Left = 1032
    Top = 65530
  end
  object MainMenu: TMainMenu
    Left = 1072
    object N1: TMenuItem
      Caption = #35774#32622
      object miConsumer: TMenuItem
        Caption = #28040#36153#32773
      end
      object miCombinationCalculator: TMenuItem
        Caption = #32452#21512#35745#31639#22120
      end
    end
  end
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 988
    Top = 10
  end
end
