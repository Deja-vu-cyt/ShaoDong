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
  Menu = MainMenu
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 24
  object PageControl1: TPageControl
    Left = 0
    Top = 251
    Width = 1334
    Height = 460
    ActivePage = TabSheet3
    Align = alClient
    TabHeight = 40
    TabOrder = 0
    object TabSheet3: TTabSheet
      Caption = '  '#19968'.'#26597#35810'      '
      ImageIndex = 2
      object Label10: TLabel
        Left = 12
        Top = 7
        Width = 77
        Height = 24
        Caption = '4.'#35774#32622#65306
      end
      object Label12: TLabel
        Left = 12
        Top = 41
        Width = 137
        Height = 24
        Caption = #65288'1'#65289'.          '#8594
      end
      object Label14: TLabel
        Left = 12
        Top = 76
        Width = 77
        Height = 24
        Caption = '5.'#35774#32622#65306
      end
      object Label15: TLabel
        Left = 12
        Top = 110
        Width = 143
        Height = 24
        Caption = #65288'1'#65289'.          '#8594' '
      end
      object Label16: TLabel
        Left = 12
        Top = 215
        Width = 77
        Height = 24
        Caption = '7.'#35774#32622#65306
      end
      object Label18: TLabel
        Left = 12
        Top = 145
        Width = 77
        Height = 24
        Caption = '6.'#35774#32622#65306
      end
      object Label21: TLabel
        Left = 12
        Top = 180
        Width = 424
        Height = 24
        Caption = #65288'1'#65289'.'#35774#32622' '#8804'                   '#20159'           '#19975'           '#20010
      end
      object edtVVertSameValueCount2: TEdit
        Left = 155
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
        Left = 74
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
        Left = 95
        Top = 6
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
        Left = 155
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
        Left = 74
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
        Left = 95
        Top = 75
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
        Left = 12
        Top = 341
        Width = 269
        Height = 40
        Caption = '11.'#8220#24320#22987#26597#35810#8221#24182#8220#23548#20986#25968#25454#8221
        TabOrder = 6
        OnClick = btnCompareClick
      end
      object edtExportGroupValueCount: TEdit
        Left = 95
        Top = 215
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
      object edtCompareGroupValueCount: TEdit
        Left = 95
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
        TabOrder = 8
      end
      object btnVertSlantExportSettings: TButton
        Left = 12
        Top = 249
        Width = 197
        Height = 40
        Caption = '8. '#35774#32622#65288#24182#65289#20445#30041
        TabOrder = 9
        OnClick = btnExportSettingsClick
      end
      object btnExportVertSlantFileSettings: TButton
        Left = 12
        Top = 295
        Width = 224
        Height = 40
        Caption = '9-10. '#36873#25321#65288#24182#65289#23548#20986
        TabOrder = 10
        OnClick = btnExportVertSlantFileSettingsClick
      end
      object edtVertSlantMaxGroupCount: TEdit
        Left = 136
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
        TabOrder = 11
      end
      object edtVertSlantMaxGroupCount2: TEdit
        Left = 272
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
        TabOrder = 12
      end
      object edtVertSlantMaxGroupCount3: TEdit
        Left = 359
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
        TabOrder = 13
      end
    end
    object TabSheet2: TTabSheet
      Caption = '  '#20108'.'#26597#35810'      '
      ImageIndex = 1
      ExplicitLeft = 8
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label11: TLabel
        Left = 9
        Top = 154
        Width = 83
        Height = 24
        Caption = '6. '#35774#32622#65306
      end
      object Label7: TLabel
        Left = 12
        Top = 15
        Width = 77
        Height = 24
        Caption = '4.'#35774#32622#65306
      end
      object Label9: TLabel
        Left = 12
        Top = 49
        Width = 137
        Height = 24
        Caption = #65288'1'#65289'.          '#8594
      end
      object Label8: TLabel
        Left = 9
        Top = 84
        Width = 83
        Height = 24
        Caption = '5. '#35774#32622#65306
      end
      object Label19: TLabel
        Left = 9
        Top = 119
        Width = 424
        Height = 24
        Caption = #65288'1'#65289'.'#35774#32622' '#8804'                   '#20159'           '#19975'           '#20010
      end
      object btnVertCompare: TButton
        Left = 12
        Top = 281
        Width = 269
        Height = 40
        Caption = '10.'#8220#24320#22987#26597#35810#8221#24182#8220#23548#20986#25968#25454#8221
        TabOrder = 0
        OnClick = btnCompareClick
      end
      object edtVertExportTypeCount: TEdit
        Left = 95
        Top = 154
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
      object edtVertSameValueCount2: TEdit
        Left = 155
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
        TabOrder = 2
      end
      object edtVertSameValueCount: TEdit
        Left = 74
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
        TabOrder = 3
      end
      object edtVertCompareSpacing: TEdit
        Left = 95
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
        TabOrder = 4
      end
      object edtVertCompareTypeCount: TEdit
        Left = 95
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
        TabOrder = 5
      end
      object btnVertExportSettings: TButton
        Left = 12
        Top = 189
        Width = 193
        Height = 40
        Caption = '7. '#35774#32622#65288#24182#65289#20445#30041
        TabOrder = 6
        OnClick = btnExportSettingsClick
      end
      object btnExportVertFileSettings: TButton
        Left = 12
        Top = 235
        Width = 213
        Height = 40
        Caption = '8-9. '#36873#25321#65288#24182#65289#23548#20986' '
        TabOrder = 7
        OnClick = btnExportVertFileSettingsClick
      end
      object edtVertMaxGroupCount: TEdit
        Left = 136
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
        TabOrder = 8
      end
      object edtVertMaxGroupCount2: TEdit
        Left = 270
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
        TabOrder = 9
      end
      object edtVertMaxGroupCount3: TEdit
        Left = 353
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
        TabOrder = 10
      end
    end
    object TabSheet1: TTabSheet
      Caption = '  '#19977'.'#26597#35810'     '
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label5: TLabel
        Left = 12
        Top = 82
        Width = 83
        Height = 24
        Caption = '5. '#35774#32622#65306
      end
      object Label2: TLabel
        Left = 12
        Top = 12
        Width = 83
        Height = 24
        Caption = '4. '#35774#32622#65306
      end
      object Label23: TLabel
        Left = 12
        Top = 47
        Width = 424
        Height = 24
        Caption = #65288'1'#65289'.'#35774#32622' '#8804'                   '#20159'           '#19975'           '#20010
      end
      object btnSlantCompare: TButton
        Left = 12
        Top = 209
        Width = 249
        Height = 40
        Caption = '9.'#8220#24320#22987#26597#35810#8221#24182#8220#23548#20986#25968#25454#8221
        TabOrder = 0
        OnClick = btnCompareClick
      end
      object edtExportTypeCount: TEdit
        Left = 95
        Top = 82
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
      object edtCompareSpacing: TEdit
        Left = 95
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
        TabOrder = 2
      end
      object btnSlantExportSettings: TButton
        Left = 12
        Top = 117
        Width = 183
        Height = 40
        Caption = '6. '#35774#32622#65288#24182#65289#20445#30041
        TabOrder = 3
        OnClick = btnExportSettingsClick
      end
      object btnExportSlantFileSettings: TButton
        Left = 12
        Top = 163
        Width = 197
        Height = 40
        Caption = '7-8. '#36873#25321#65288#24182#65289#23548#20986
        TabOrder = 4
        OnClick = btnExportSlantFileSettingsClick
      end
      object edtSlantMaxGroupCount: TEdit
        Left = 136
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
        TabOrder = 5
      end
      object edtSlantMaxGroupCount2: TEdit
        Left = 272
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
        TabOrder = 6
      end
      object edtSlantMaxGroupCount3: TEdit
        Left = 358
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
        TabOrder = 7
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 1334
    Height = 251
    Align = alTop
    TabOrder = 1
    DesignSize = (
      1334
      251)
    object Label1: TLabel
      Left = 16
      Top = 91
      Width = 77
      Height = 24
      Caption = '1.'#35835#21462#65306
    end
    object lblUseTime: TLabel
      Left = 13
      Top = 56
      Width = 140
      Height = 24
      Caption = #22788#29702#25152#38656#26102#38388#65306
    end
    object Label3: TLabel
      Left = 16
      Top = 126
      Width = 77
      Height = 24
      Caption = '2.'#35774#32622#65306
    end
    object Label4: TLabel
      Left = 16
      Top = 161
      Width = 77
      Height = 24
      Caption = '3.'#35774#32622#65306
    end
    object Label6: TLabel
      Left = 321
      Top = 16
      Width = 521
      Height = 24
      Caption = #9313'.'#65288#35774#32622#65289'[ '#36830#25509#65288' N '#21488#26381#21153#22120' '#65289#30340#65288' '#20027#26426' IP '#22320#22336' '#65289'] '#65306
    end
    object Label13: TLabel
      Left = 14
      Top = 16
      Width = 226
      Height = 24
      Caption = #9312'.'#65288#24050#35774#32622#65289#20027#26426#31471#21475' '#65306
    end
    object Label17: TLabel
      Left = 1199
      Top = 16
      Width = 20
      Height = 24
      Anchors = [akTop, akRight]
      Caption = #9314'.'
    end
    object edtFileName: TEdit
      Left = 99
      Top = 91
      Width = 1206
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
      Left = 99
      Top = 126
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
      Left = 99
      Top = 161
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
      Left = 16
      Top = 200
      Width = 530
      Height = 25
      Caption = #26597#35810#65306'( '#31532' 2 '#27169#24335' ) '
      TabOrder = 3
    end
    object edtAddress: TEdit
      Left = 848
      Top = 16
      Width = 337
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
        Caption = #22788#29702#22120
        OnClick = miConsumerClick
      end
      object miCombinationCalculator: TMenuItem
        Caption = #32452#21512#35745#31639#22120
        OnClick = miCombinationCalculatorClick
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
