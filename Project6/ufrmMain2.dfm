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
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 24
  object PageControl1: TPageControl
    Left = 0
    Top = 230
    Width = 1334
    Height = 460
    ActivePage = TabSheet1
    Align = alClient
    TabHeight = 40
    TabOrder = 0
    object TabSheet3: TTabSheet
      Caption = '  '#19968'.'#26597#35810'      '
      ImageIndex = 2
      object Label10: TLabel
        Left = 15
        Top = 7
        Width = 77
        Height = 24
        Caption = '4.'#35774#32622#65306
      end
      object Label12: TLabel
        Left = 15
        Top = 41
        Width = 143
        Height = 24
        Caption = #65288'1'#65289'.          '#8594' '
      end
      object Label14: TLabel
        Left = 15
        Top = 76
        Width = 77
        Height = 24
        Caption = '5.'#35774#32622#65306
      end
      object Label15: TLabel
        Left = 15
        Top = 111
        Width = 137
        Height = 24
        Caption = #65288'1'#65289'.          '#8594
      end
      object Label18: TLabel
        Left = 15
        Top = 146
        Width = 77
        Height = 24
        Caption = '6.'#35774#32622#65306
      end
      object Label16: TLabel
        Left = 15
        Top = 181
        Width = 95
        Height = 24
        Caption = '6-1.'#35774#32622#65306
      end
      object edtVVertSameValueCount2: TEdit
        Left = 158
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
        Left = 77
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
        Left = 93
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
        Left = 158
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
        TabOrder = 3
      end
      object edtSlantSameValueCount: TEdit
        Left = 77
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
      object edtSlantCompareSpacing: TEdit
        Left = 93
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
        Left = 93
        Top = 146
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
        Top = 354
        Width = 128
        Height = 40
        Caption = '8.'#23548#20986#25968#25454
        TabOrder = 7
        OnClick = btnCompareClick
      end
      object edtVertSlantKeepCodeNameValueCount: TEdit
        Left = 109
        Top = 181
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
        Top = 262
        Width = 282
        Height = 40
        Caption = '6-3.'#35774#32622' '#65306#26242#20445#30041#65288#24182#65289#23548#20986
        TabOrder = 9
        OnClick = btnCompareClick
      end
      object btnVertSlantGroupCodeNameSettings: TButton
        Left = 15
        Top = 216
        Width = 285
        Height = 40
        Caption = '6-2'#65288'1'#65289'.'#35774#32622#65288' '#36941#21382' '#65289#26465#20214
        TabOrder = 10
        OnClick = btnVertSlantGroupCodeNameSettingsClick
      end
      object btnVertSlantGroupCodeNameSettings2: TButton
        Left = 306
        Top = 216
        Width = 285
        Height = 40
        Caption = '6-2'#65288'2'#65289'.'#35774#32622#65288' '#36941#21382' '#65289#26465#20214
        TabOrder = 11
        OnClick = btnVertSlantGroupCodeNameSettings2Click
      end
      object btnVertSlantExportFileSettings: TButton
        Left = 15
        Top = 308
        Width = 378
        Height = 40
        Caption = '7. '#35774#32622#65288' '#36941#21382#26465#20214' '#65289#21450#65288' '#23548#20986#26041#24335' '#65289
        TabOrder = 12
        OnClick = btnVertSlantExportFileSettingsClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = '  '#20108'.'#26597#35810'      '
      ImageIndex = 1
      object Label7: TLabel
        Left = 15
        Top = 15
        Width = 77
        Height = 24
        Caption = '4.'#35774#32622#65306
      end
      object Label9: TLabel
        Left = 15
        Top = 49
        Width = 143
        Height = 24
        Caption = #65288'1'#65289'.          '#8594' '
      end
      object Label8: TLabel
        Left = 15
        Top = 84
        Width = 83
        Height = 24
        Caption = '5. '#35774#32622#65306
      end
      object Label17: TLabel
        Left = 15
        Top = 119
        Width = 95
        Height = 24
        Caption = '5-1.'#35774#32622#65306
      end
      object edtVertSameValueCount2: TEdit
        Left = 157
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
        Left = 76
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
        Left = 93
        Top = 14
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
        Left = 99
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
        Top = 338
        Width = 134
        Height = 40
        Caption = '7.'#23548#20986#25968#25454
        TabOrder = 4
        OnClick = btnCompareClick
      end
      object edtVertKeepCodeNameValueCount: TEdit
        Left = 112
        Top = 119
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
        Top = 246
        Width = 282
        Height = 40
        Caption = '5-3.'#35774#32622' '#65306#26242#20445#30041#65288#24182#65289#23548#20986
        TabOrder = 6
        OnClick = btnCompareClick
      end
      object btnVertGroupCodeNameSettings: TButton
        Left = 15
        Top = 154
        Width = 285
        Height = 40
        Caption = '5-2'#65288'1'#65289'.'#35774#32622#65288' '#36941#21382' '#65289#26465#20214
        TabOrder = 7
        OnClick = btnVertGroupCodeNameSettingsClick
      end
      object btnVertGroupCodeNameSettings2: TButton
        Left = 15
        Top = 200
        Width = 285
        Height = 40
        Caption = '5-2'#65288'2'#65289'.'#35774#32622#65288' '#36941#21382' '#65289#26465#20214
        TabOrder = 8
        OnClick = btnVertGroupCodeNameSettings2Click
      end
      object btnVertExportFileSettings: TButton
        Left = 15
        Top = 292
        Width = 378
        Height = 40
        Caption = '6. '#35774#32622#65288' '#36941#21382#26465#20214' '#65289#21450#65288' '#23548#20986#26041#24335' '#65289
        TabOrder = 9
        OnClick = btnVertExportFileSettingsClick
      end
    end
    object TabSheet1: TTabSheet
      Caption = '  '#19977'.'#26597#35810'      '
      object Label2: TLabel
        Left = 15
        Top = 82
        Width = 83
        Height = 24
        Caption = '5. '#35774#32622#65306
      end
      object Label5: TLabel
        Left = 15
        Top = 12
        Width = 77
        Height = 24
        Caption = '4.'#35774#32622#65306
      end
      object Label19: TLabel
        Left = 15
        Top = 117
        Width = 95
        Height = 24
        Caption = '5-1.'#35774#32622#65306
      end
      object Label25: TLabel
        Left = 16
        Top = 138
        Width = 6
        Height = 24
      end
      object Label6: TLabel
        Left = 15
        Top = 47
        Width = 137
        Height = 24
        Caption = #65288'1'#65289'.          '#8594
      end
      object edtCompareSpacing: TEdit
        Left = 98
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
        Left = 104
        Top = 82
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
        Top = 336
        Width = 130
        Height = 40
        Caption = '7.'#23548#20986#25968#25454
        TabOrder = 2
        OnClick = btnCompareClick
      end
      object edtSlantKeepCodeNameValueCount: TEdit
        Left = 109
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
        Top = 244
        Width = 282
        Height = 40
        Caption = '5-3.'#35774#32622' '#65306#26242#20445#30041#65288#24182#65289#23548#20986
        TabOrder = 4
        OnClick = btnCompareClick
      end
      object btnSlantGroupCodeNameSettings: TButton
        Left = 15
        Top = 152
        Width = 285
        Height = 40
        Caption = '5-2'#65288'1'#65289'.'#35774#32622#65288' '#36941#21382' '#65289#26465#20214
        TabOrder = 5
        OnClick = btnVertGroupCodeNameSettingsClick
      end
      object btnSlantGroupCodeNameSettings2: TButton
        Left = 15
        Top = 198
        Width = 285
        Height = 40
        Caption = '5-2'#65288'2'#65289'.'#35774#32622#65288' '#36941#21382' '#65289#26465#20214
        TabOrder = 6
        OnClick = btnVertGroupCodeNameSettings2Click
      end
      object btnSlantExportFileSettings: TButton
        Left = 15
        Top = 290
        Width = 378
        Height = 40
        Caption = '6. '#35774#32622#65288' '#36941#21382#26465#20214' '#65289#21450#65288' '#23548#20986#26041#24335' '#65289
        TabOrder = 7
        OnClick = btnSlantExportFileSettingsClick
      end
      object edtSameValueCount2: TEdit
        Left = 158
        Top = 47
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
        Left = 77
        Top = 47
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
      Top = 81
      Width = 77
      Height = 24
      Caption = '1.'#35835#21462#65306
    end
    object lblUseTime: TLabel
      Left = 19
      Top = 46
      Width = 140
      Height = 24
      Caption = #22788#29702#25152#38656#26102#38388#65306
    end
    object Label3: TLabel
      Left = 19
      Top = 116
      Width = 77
      Height = 24
      Caption = '2.'#35774#32622#65306
    end
    object Label4: TLabel
      Left = 19
      Top = 151
      Width = 77
      Height = 24
      Caption = '3.'#35774#32622#65306
    end
    object Label20: TLabel
      Left = 321
      Top = 16
      Width = 501
      Height = 24
      Caption = #9313'.'#65288#35774#32622#65289'['#65288' '#20027#26426' IP '#22320#22336' '#65289#36830#25509#65288' N '#21488#26381#21153#22120' '#65289'] '#65306
    end
    object Label21: TLabel
      Left = 14
      Top = 16
      Width = 232
      Height = 24
      Caption = #9312'.'#65288#24050#35774#32622#65289#20027#26426#31471#21475' '#65306
    end
    object Label22: TLabel
      Left = 1199
      Top = 16
      Width = 26
      Height = 24
      Anchors = [akTop, akRight]
      Caption = #9314'.'
    end
    object lblRowCount: TLabel
      Left = 420
      Top = 51
      Width = 628
      Height = 24
      Caption = #26368#21518#19968#27425#36941#21382' '#65292#29983#25104#65288' '#23384#20648' '#65289#22312' '#12304' Data '#25968#25454#24211' '#12305#30340#34892#25968' '#65306' N  '#34892' '#12290
    end
    object edtFileName: TEdit
      Left = 97
      Top = 81
      Width = 1220
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
      Left = 97
      Top = 116
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
      Left = 97
      Top = 151
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
      Top = 190
      Width = 220
      Height = 25
      Caption = #26597#35810#65306'( '#31532' 2 '#27169#24335' ) '
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
