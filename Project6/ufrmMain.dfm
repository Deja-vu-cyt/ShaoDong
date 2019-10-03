object frmMain: TfrmMain
  Left = 0
  Top = 0
  ClientHeight = 700
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
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 24
  object PageControl1: TPageControl
    Left = 0
    Top = 230
    Width = 1334
    Height = 470
    ActivePage = TabSheet2
    Align = alClient
    TabHeight = 40
    TabOrder = 0
    object TabSheet3: TTabSheet
      Caption = #19968'.'#26597#35810#65306#12304#21508#34892#38388#12305#30452#12289#26012#36830#12304#23545#24212#21015#12305#32452#21512'     '
      ImageIndex = 2
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
        Width = 463
        Height = 24
        Caption = #65288'1'#65289'.'#65288#30452#36830#65289#26377'          '#8594'           '#20010#12304#23545#24212#21015#12305#25968#23383
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
        Width = 463
        Height = 24
        Caption = #65288'1'#65289'.'#65288#26012#36830#65289#26377'          '#8594'           '#20010#12304#23545#24212#21015#12305#25968#23383
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
      object Label16: TLabel
        Left = 15
        Top = 180
        Width = 607
        Height = 24
        Caption = '6-1.'#35774#32622#65306#26242#20445#30041#65288#24182#65289#23548#20986#65288' '#31526#21512#35774#32622' '#65289#30340'          '#20010#20197#19978#32452#21512#30340#34892
      end
      object edtVVertSameValueCount2: TEdit
        Left = 258
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
        Left = 177
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
        Left = 258
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
        Left = 177
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
      object btnVertSlantCompare: TButton
        Left = 15
        Top = 352
        Width = 463
        Height = 40
        Caption = '8.'#8220#26681#25454#65306'1- 7.'#35774#32622#8220#23548#20986' ...'#8220'N-N'#8221#20010#32452#21512#8221#23548#20986#25968#25454
        TabOrder = 7
        OnClick = btnCompareClick
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
        TabOrder = 8
      end
      object btnVertSlantCompare2: TButton
        Left = 15
        Top = 260
        Width = 800
        Height = 40
        Caption = '6-3.'#35774#32622' '#65306#26242#20445#30041#65288#24182#65289#23548#20986#65288' '#31526#21512#35774#32622' '#65289#30340#12304' '#22635#20837#65306'N '#65307#20363' '#65306'20 '#12305#20010#20197#19978#32452#21512#30340#34892
        TabOrder = 9
        OnClick = btnCompareClick
      end
      object btnVertSlantGroupCodeNameSettings: TButton
        Left = 15
        Top = 214
        Width = 285
        Height = 40
        Caption = '6-2'#65288'1'#65289'.'#35774#32622#65288' '#36941#21382' '#65289#26465#20214
        TabOrder = 10
        OnClick = btnVertSlantGroupCodeNameSettingsClick
      end
      object btnVertSlantGroupCodeNameSettings2: TButton
        Left = 329
        Top = 214
        Width = 285
        Height = 40
        Caption = '6-2'#65288'2'#65289'.'#35774#32622#65288' '#36941#21382' '#65289#26465#20214
        TabOrder = 11
        OnClick = btnVertSlantGroupCodeNameSettings2Click
      end
      object btnVertSlantExportFileSettings: TButton
        Left = 15
        Top = 306
        Width = 378
        Height = 40
        Caption = '7. '#35774#32622#65288' '#36941#21382#26465#20214' '#65289#21450#65288' '#23548#20986#26041#24335' '#65289
        TabOrder = 12
        OnClick = btnVertSlantExportFileSettingsClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = '  '#20108'.'#26597#35810#65306#12304#21508#34892#38388#12305#30452#36830#12304#23545#24212#21015#12305#32452#21512'      '
      ImageIndex = 1
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
        Width = 463
        Height = 24
        Caption = #65288'1'#65289'.'#65288#30452#36830#65289#26377'          '#8594'           '#20010#12304#23545#24212#21015#12305#25968#23383
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
      object Label17: TLabel
        Left = 15
        Top = 114
        Width = 607
        Height = 24
        Caption = '5-1.'#35774#32622#65306#26242#20445#30041#65288#24182#65289#23548#20986#65288' '#31526#21512#35774#32622' '#65289#30340'          '#20010#20197#19978#32452#21512#30340#34892
      end
      object edtVertSameValueCount2: TEdit
        Left = 259
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
        Left = 175
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
      object btnVertCompare: TButton
        Left = 15
        Top = 328
        Width = 462
        Height = 40
        Caption = '7.'#8220#26681#25454#65306'1- 6.'#35774#32622#8220#23548#20986' ...'#8220'N-N'#8221#20010#32452#21512#8221#23548#20986#25968#25454
        TabOrder = 4
        OnClick = btnCompareClick
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
        TabOrder = 5
      end
      object btnVertCompare2: TButton
        Left = 15
        Top = 236
        Width = 800
        Height = 40
        Caption = '5-3.'#35774#32622' '#65306#26242#20445#30041#65288#24182#65289#23548#20986#65288' '#31526#21512#35774#32622' '#65289#30340#12304' '#22635#20837#65306'N '#65307#20363' '#65306'20 '#12305#20010#20197#19978#32452#21512#30340#34892
        TabOrder = 6
        OnClick = btnCompareClick
      end
      object btnVertGroupCodeNameSettings: TButton
        Left = 15
        Top = 144
        Width = 285
        Height = 40
        Caption = '5-2'#65288'1'#65289'.'#35774#32622#65288' '#36941#21382' '#65289#26465#20214
        TabOrder = 7
        OnClick = btnVertGroupCodeNameSettingsClick
      end
      object btnVertGroupCodeNameSettings2: TButton
        Left = 15
        Top = 190
        Width = 285
        Height = 40
        Caption = '5-2'#65288'2'#65289'.'#35774#32622#65288' '#36941#21382' '#65289#26465#20214
        TabOrder = 8
        OnClick = btnVertGroupCodeNameSettings2Click
      end
      object btnVertExportFileSettings: TButton
        Left = 15
        Top = 282
        Width = 378
        Height = 40
        Caption = '6. '#35774#32622#65288' '#36941#21382#26465#20214' '#65289#21450#65288' '#23548#20986#26041#24335' '#65289
        TabOrder = 9
        OnClick = btnVertExportFileSettingsClick
      end
    end
    object TabSheet1: TTabSheet
      Caption = '  '#19977'.'#26597#35810#65306#12304#21508#34892#38388#12305#26012#36830#12304#23545#24212#21015#12305#32452#21512'     '
      object Label2: TLabel
        Left = 15
        Top = 80
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
      object Label19: TLabel
        Left = 15
        Top = 117
        Width = 607
        Height = 24
        Caption = '5-1.'#35774#32622#65306#26242#20445#30041#65288#24182#65289#23548#20986#65288' '#31526#21512#35774#32622' '#65289#30340'          '#20010#20197#19978#32452#21512#30340#34892
      end
      object Label6: TLabel
        Left = 15
        Top = 45
        Width = 463
        Height = 24
        Caption = #65288'1'#65289'.'#65288#26012#36830#65289#26377'          '#8594'           '#20010#12304#23545#24212#21015#12305#25968#23383
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
        Top = 80
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
      object btnSlantCompare: TButton
        Left = 15
        Top = 331
        Width = 463
        Height = 40
        Caption = '7.'#8220#26681#25454#65306'1- 6.'#35774#32622#8220#23548#20986' ...'#8220'N-N'#8221#20010#32452#21512#8221#23548#20986#25968#25454
        TabOrder = 2
        OnClick = btnCompareClick
      end
      object edtSlantKeepCodeNameValueCount: TEdit
        Left = 428
        Top = 117
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
      object btnSlantCompare2: TButton
        Left = 15
        Top = 239
        Width = 800
        Height = 40
        Caption = '5-3.'#35774#32622' '#65306#26242#20445#30041#65288#24182#65289#23548#20986#65288' '#31526#21512#35774#32622' '#65289#30340#12304' '#22635#20837#65306'N '#65307#20363' '#65306'20 '#12305#20010#20197#19978#32452#21512#30340#34892
        TabOrder = 4
        OnClick = btnCompareClick
      end
      object btnSlantGroupCodeNameSettings: TButton
        Left = 15
        Top = 147
        Width = 285
        Height = 40
        Caption = '5-2'#65288'1'#65289'.'#35774#32622#65288' '#36941#21382' '#65289#26465#20214
        TabOrder = 5
        OnClick = btnVertGroupCodeNameSettingsClick
      end
      object btnSlantGroupCodeNameSettings2: TButton
        Left = 15
        Top = 193
        Width = 285
        Height = 40
        Caption = '5-2'#65288'2'#65289'.'#35774#32622#65288' '#36941#21382' '#65289#26465#20214
        TabOrder = 6
        OnClick = btnVertGroupCodeNameSettings2Click
      end
      object btnSlantExportFileSettings: TButton
        Left = 15
        Top = 285
        Width = 378
        Height = 40
        Caption = '6. '#35774#32622#65288' '#36941#21382#26465#20214' '#65289#21450#65288' '#23548#20986#26041#24335' '#65289
        TabOrder = 7
        OnClick = btnSlantExportFileSettingsClick
      end
      object edtSameValueCount2: TEdit
        Left = 258
        Top = 45
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
      object edtSameValueCount: TEdit
        Left = 177
        Top = 45
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
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 1334
    Height = 230
    Align = alTop
    TabOrder = 1
    DesignSize = (
      1334
      230)
    object Label1: TLabel
      Left = 19
      Top = 86
      Width = 534
      Height = 24
      Caption = '1. '#35835#21462' ['#65288' '#25991#20214#22841' '#65289#20869' 1 - N '#20010#8220' '#34987#26597#35810#65288' TXT '#65289#25991#26412' '#8221'] '#65306
    end
    object lblUseTime: TLabel
      Left = 19
      Top = 51
      Width = 140
      Height = 24
      Caption = #22788#29702#25152#38656#26102#38388#65306
    end
    object Label3: TLabel
      Left = 19
      Top = 121
      Width = 293
      Height = 24
      Caption = '2.'#35774#32622#8220#26597#35810#65288#31532#19968#21306#22495#65289#8221#21015#25968#65306
    end
    object Label4: TLabel
      Left = 19
      Top = 156
      Width = 293
      Height = 24
      Caption = '3.'#35774#32622#8220#26597#35810#65288#31532#20108#21306#22495#65289#8221#21015#25968#65306
    end
    object Label20: TLabel
      Left = 321
      Top = 16
      Width = 495
      Height = 24
      Caption = #9313'.'#65288#35774#32622#65289'['#65288' '#20027#26426' IP '#22320#22336' '#65289#36830#25509#65288' N '#21488#26381#21153#22120' '#65289'] '#65306
    end
    object Label21: TLabel
      Left = 14
      Top = 16
      Width = 226
      Height = 24
      Caption = #9312'.'#65288#24050#35774#32622#65289#20027#26426#31471#21475' '#65306
    end
    object Label22: TLabel
      Left = 1199
      Top = 16
      Width = 20
      Height = 24
      Anchors = [akTop, akRight]
      Caption = #9314'.'
    end
    object lblRowCount: TLabel
      Left = 420
      Top = 54
      Width = 628
      Height = 24
      Caption = #26368#21518#19968#27425#36941#21382' '#65292#29983#25104#65288' '#23384#20648' '#65289#22312' '#12304' Data '#25968#25454#24211' '#12305#30340#34892#25968' '#65306' N  '#34892' '#12290
    end
    object edtFileDirectory: TEdit
      Left = 552
      Top = 86
      Width = 756
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
      TabOrder = 1
    end
    object edtIntervalValue2: TEdit
      Left = 311
      Top = 156
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
      Top = 195
      Width = 530
      Height = 25
      Caption = #26597#35810#65306#26012#36830#34892#65288' '#27599#34892#30340#21015#25968#23383#65292#36328#21306#22495' '#65289'( '#31532' 2 '#27169#24335' ) '
      TabOrder = 3
    end
    object edtAddress: TEdit
      Left = 822
      Top = 16
      Width = 363
      Height = 32
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
    end
    object edtPort: TEdit
      Left = 246
      Top = 16
      Width = 60
      Height = 32
      NumbersOnly = True
      TabOrder = 5
      Text = '8888'
    end
    object btnOk: TButton
      Left = 1230
      Top = 9
      Width = 75
      Height = 40
      Anchors = [akTop, akRight]
      Caption = #30830#23450
      TabOrder = 6
      OnClick = btnOkClick
    end
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
