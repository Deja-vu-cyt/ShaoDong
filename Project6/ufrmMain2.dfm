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
    ExplicitHeight = 539
    object TabSheet3: TTabSheet
      Caption = '  '#19968'.'#26597#35810'      '
      ImageIndex = 2
      ExplicitHeight = 489
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
      object Label6: TLabel
        Left = 15
        Top = 308
        Width = 163
        Height = 24
        Caption = '7. '#35774#32622#65306'          '#33267
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
      object edtVertSlantExportCodeNameValueCount: TEdit
        Left = 99
        Top = 308
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
        Left = 184
        Top = 308
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
        Top = 374
        Width = 137
        Height = 40
        Caption = '7-2.'#23548#20986#25968#25454
        TabOrder = 9
        OnClick = btnCompareClick
      end
      object chkVertSlantExportSource: TCheckBox
        Left = 15
        Top = 343
        Width = 250
        Height = 25
        Caption = '7-1. '#21482#23384#20648#65288#24182#65289#23548#20986
        TabOrder = 10
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
        TabOrder = 11
      end
      object btnVertSlantCompare2: TButton
        Left = 15
        Top = 262
        Width = 282
        Height = 40
        Caption = '6-3.'#35774#32622' '#65306#26242#20445#30041#65288#24182#65289#23548#20986
        TabOrder = 12
        OnClick = btnCompareClick
      end
      object btnVertSlantGroupCodeNameSettings: TButton
        Left = 15
        Top = 216
        Width = 285
        Height = 40
        Caption = '6-2'#65288'1'#65289'.'#35774#32622#65288' '#36941#21382' '#65289#26465#20214
        TabOrder = 13
        OnClick = btnVertSlantGroupCodeNameSettingsClick
      end
      object btnVertSlantGroupCodeNameSettings2: TButton
        Left = 306
        Top = 216
        Width = 285
        Height = 40
        Caption = '6-2'#65288'2'#65289'.'#35774#32622#65288' '#36941#21382' '#65289#26465#20214
        TabOrder = 14
        OnClick = btnVertSlantGroupCodeNameSettings2Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = '  '#20108'.'#26597#35810'      '
      ImageIndex = 1
      ExplicitHeight = 489
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
      object Label11: TLabel
        Left = 15
        Top = 292
        Width = 163
        Height = 24
        Caption = '6. '#35774#32622#65306'          '#33267
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
      object edtVertExportCodeNameValueCount: TEdit
        Left = 99
        Top = 292
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
        Left = 184
        Top = 292
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
        Top = 358
        Width = 140
        Height = 40
        Caption = '6-2.'#23548#20986#25968#25454
        TabOrder = 6
        OnClick = btnCompareClick
      end
      object chkVertExportSource: TCheckBox
        Left = 15
        Top = 327
        Width = 250
        Height = 25
        Caption = '6-1. '#21482#23384#20648#65288#24182#65289#23548#20986
        TabOrder = 7
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
        TabOrder = 8
      end
      object btnVertCompare2: TButton
        Left = 15
        Top = 246
        Width = 282
        Height = 40
        Caption = '5-3.'#35774#32622' '#65306#26242#20445#30041#65288#24182#65289#23548#20986
        TabOrder = 9
        OnClick = btnCompareClick
      end
      object btnVertGroupCodeNameSettings: TButton
        Left = 15
        Top = 154
        Width = 285
        Height = 40
        Caption = '5-2'#65288'1'#65289'.'#35774#32622#65288' '#36941#21382' '#65289#26465#20214
        TabOrder = 10
        OnClick = btnVertGroupCodeNameSettingsClick
      end
      object btnVertGroupCodeNameSettings2: TButton
        Left = 15
        Top = 200
        Width = 285
        Height = 40
        Caption = '5-2'#65288'2'#65289'.'#35774#32622#65288' '#36941#21382' '#65289#26465#20214
        TabOrder = 11
        OnClick = btnVertGroupCodeNameSettings2Click
      end
    end
    object TabSheet1: TTabSheet
      Caption = '  '#19977'.'#26597#35810'      '
      ExplicitHeight = 489
      object Label2: TLabel
        Left = 15
        Top = 47
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
      object Label13: TLabel
        Left = 15
        Top = 255
        Width = 151
        Height = 24
        Caption = '6. '#35774#32622#65306'        '#33267
      end
      object Label19: TLabel
        Left = 15
        Top = 82
        Width = 95
        Height = 24
        Caption = '5-1.'#35774#32622#65306
      end
      object Label25: TLabel
        Left = 15
        Top = 152
        Width = 6
        Height = 24
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
        TabOrder = 1
      end
      object edtSlantExportCodeNameValueCount: TEdit
        Left = 92
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
        Left = 172
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
        Width = 139
        Height = 40
        Caption = '6-2.'#23548#20986#25968#25454
        TabOrder = 4
        OnClick = btnCompareClick
      end
      object chkSlantExportSource: TCheckBox
        Left = 15
        Top = 290
        Width = 250
        Height = 25
        Caption = '6-1. '#21482#23384#20648#65288#24182#65289#23548#20986
        TabOrder = 5
      end
      object edtSlantKeepCodeNameValueCount: TEdit
        Left = 109
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
        TabOrder = 6
      end
      object btnSlantCompare2: TButton
        Left = 15
        Top = 209
        Width = 282
        Height = 40
        Caption = '5-3.'#35774#32622' '#65306#26242#20445#30041#65288#24182#65289#23548#20986
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
      Width = 77
      Height = 24
      Caption = '1.'#35835#21462#65306
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
      Width = 77
      Height = 24
      Caption = '2.'#35774#32622#65306
    end
    object Label4: TLabel
      Left = 19
      Top = 121
      Width = 77
      Height = 24
      Caption = '3.'#35774#32622#65306
    end
    object edtFileName: TEdit
      Left = 97
      Top = 51
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
      Left = 97
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
      Width = 220
      Height = 25
      Caption = #26597#35810#65306'( '#31532' 2 '#27169#24335' ) '
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
