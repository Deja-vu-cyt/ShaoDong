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
    TabHeight = 40
    TabOrder = 0
    object TabSheet3: TTabSheet
      Caption = #19968'.'#26597#35810#65306#12304#21508#34892#38388#12305#30452#12289#26012#36830#12304#23545#24212#21015#12305#32452#21512'     '
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
        Width = 473
        Height = 24
        Caption = #65288'1'#65289'.'#65288#30452#36830#65289'y'#26377'          '#8594'           '#20010#12304#23545#24212#21015#12305#25968#23383
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
        Width = 473
        Height = 24
        Caption = #65288'1'#65289'.'#65288#26012#36830#65289'y'#26377'          '#8594'           '#20010#12304#23545#24212#21015#12305#25968#23383
      end
      object Label16: TLabel
        Left = 12
        Top = 180
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
        Left = 95
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
        Left = 95
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
        Left = 12
        Top = 306
        Width = 269
        Height = 40
        Caption = '11.'#8220#24320#22987#26597#35810#8221#24182#8220#23548#20986#25968#25454#8221
        TabOrder = 6
        OnClick = btnCompareClick
      end
      object edtExportGroupValueCount: TEdit
        Left = 95
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
        Top = 214
        Width = 445
        Height = 40
        Caption = '8. '#35774#32622#65288#24182#65289#20445#30041' '#65306'['#65288'TXT'#65289#25991#26412' ] '#37096#20221#25968#25454
        TabOrder = 9
        OnClick = btnExportSettingsClick
      end
      object btnExportVertSlantFileSettings: TButton
        Left = 12
        Top = 260
        Width = 389
        Height = 40
        Caption = '9-10. '#36873#25321#65288#24182#65289#23548#20986' '#65306'['#65288'TXT'#65289#25991#26412' ]'
        TabOrder = 10
        OnClick = btnExportVertSlantFileSettingsClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = '  '#20108'.'#26597#35810#65306#12304#21508#34892#38388#12305#30452#36830#12304#23545#24212#21015#12305#32452#21512'      '
      ImageIndex = 1
      object Label11: TLabel
        Left = 9
        Top = 119
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
        Width = 473
        Height = 24
        Caption = #65288'1'#65289'.'#65288#30452#36830#65289'y'#26377'          '#8594'           '#20010#12304#23545#24212#21015#12305#25968#23383
      end
      object Label8: TLabel
        Left = 9
        Top = 84
        Width = 83
        Height = 24
        Caption = '5. '#35774#32622#65306
      end
      object btnVertCompare: TButton
        Left = 12
        Top = 246
        Width = 269
        Height = 40
        Caption = '10.'#8220#24320#22987#26597#35810#8221#24182#8220#23548#20986#25968#25454#8221
        TabOrder = 0
        OnClick = btnCompareClick
      end
      object edtVertExportTypeCount: TEdit
        Left = 95
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
        TabOrder = 2
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
        Top = 154
        Width = 445
        Height = 40
        Caption = '7. '#35774#32622#65288#24182#65289#20445#30041' '#65306'['#65288'TXT'#65289#25991#26412' ] '#37096#20221#25968#25454
        TabOrder = 6
        OnClick = btnExportSettingsClick
      end
      object btnExportVertFileSettings: TButton
        Left = 12
        Top = 200
        Width = 373
        Height = 40
        Caption = '8-9. '#36873#25321#65288#24182#65289#23548#20986' '#65306'['#65288'TXT'#65289#25991#26412' ]'
        TabOrder = 7
        OnClick = btnExportVertFileSettingsClick
      end
    end
    object TabSheet1: TTabSheet
      Caption = '  '#19977'.'#26597#35810#65306#12304#21508#34892#38388#12305#26012#36830#12304#23545#24212#21015#12305#32452#21512'     '
      object Label5: TLabel
        Left = 12
        Top = 47
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
      object btnSlantCompare: TButton
        Left = 12
        Top = 176
        Width = 249
        Height = 40
        Caption = '9.'#8220#24320#22987#26597#35810#8221#24182#8220#23548#20986#25968#25454#8221
        TabOrder = 0
        OnClick = btnCompareClick
      end
      object edtExportTypeCount: TEdit
        Left = 95
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
      object btnExportCompareRow: TButton
        Left = 267
        Top = 176
        Width = 75
        Height = 40
        Caption = #23548#20986#34892
        TabOrder = 2
        Visible = False
        OnClick = btnExportCompareRowClick
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
        TabOrder = 3
      end
      object btnSlantExportSettings: TButton
        Left = 12
        Top = 84
        Width = 445
        Height = 40
        Caption = '6. '#35774#32622#65288#24182#65289#20445#30041' '#65306'['#65288'TXT'#65289#25991#26412' ] '#37096#20221#25968#25454
        TabOrder = 4
        OnClick = btnExportSettingsClick
      end
      object btnExportSlantFileSettings: TButton
        Left = 12
        Top = 130
        Width = 373
        Height = 40
        Caption = '7-8. '#36873#25321#65288#24182#65289#23548#20986' '#65306'['#65288'TXT'#65289#25991#26412' ]'
        TabOrder = 5
        OnClick = btnExportSlantFileSettingsClick
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
      Width = 77
      Height = 24
      Caption = '1.'#35835#21462#65306
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
      Width = 77
      Height = 24
      Caption = '2.'#35774#32622#65306
    end
    object Label4: TLabel
      Left = 16
      Top = 122
      Width = 77
      Height = 24
      Caption = '3.'#35774#32622#65306
    end
    object edtFileName: TEdit
      Left = 99
      Top = 52
      Width = 1214
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
      Left = 99
      Top = 87
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
    object edtFirstRangeValue: TEdit
      Left = 99
      Top = 122
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
      Top = 161
      Width = 530
      Height = 25
      Caption = #26597#35810#65306#26012#36830#34892#65288' '#27599#34892#30340#21015#25968#23383#65292#36328#21306#22495' '#65289'( '#31532' 2 '#27169#24335' ) '
      TabOrder = 3
    end
  end
  object OpenDialog: TOpenDialog
    Filter = #25991#26412#25991#20214'|*.txt'
    Left = 896
    Top = 10
  end
end