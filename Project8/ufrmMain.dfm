object frmMain: TfrmMain
  Left = 0
  Top = 0
  ClientHeight = 639
  ClientWidth = 1198
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
    Width = 1198
    Height = 639
    HorzScrollBar.Range = 1274
    HorzScrollBar.Visible = False
    VertScrollBar.Range = 400
    VertScrollBar.Tracking = True
    Align = alClient
    AutoScroll = False
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    TabOrder = 0
    DesignSize = (
      1198
      639)
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
      Width = 634
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
      Top = 350
      Width = 494
      Height = 41
      Caption = '5.  '#26681#25454' '#65306#65288'1'#65289#65288'0-0'#65289'-'#65288'1-44'#65289'. '#35774#32622' ... '#23548#20986#25968#25454
      TabOrder = 3
      OnClick = btnCompareClick
    end
    object btnExportSettings: TButton
      Left = 19
      Top = 209
      Width = 534
      Height = 41
      Caption = #39029#38754' 2 .'#65288'2'#65289#65288'0-0'#65289'-'#65288'4'#65289#65288'1-7'#65289'. '#35774#32622#65288' '#26597#35810' '#65289#26465#20214
      TabOrder = 4
      OnClick = btnExportSettingsClick
    end
    object btnExportSettings2: TButton
      Left = 19
      Top = 256
      Width = 534
      Height = 41
      Caption = #39029#38754' 3 .'#65288'5'#65289#65288'0-0'#65289'-'#65288'5'#65289#65288'1-7'#65289'. '#35774#32622#65288' '#26597#35810' '#65289#26465#20214
      TabOrder = 5
      OnClick = btnExportSettings2Click
    end
    object btnExportSettings3: TButton
      Left = 19
      Top = 303
      Width = 534
      Height = 41
      Caption = #39029#38754' 4 .'#65288'6'#65289#65288'0-0'#65289'-'#65288'6'#65289#65288'1-7'#65289'. '#35774#32622#65288' '#26597#35810' '#65289#26465#20214
      TabOrder = 6
      OnClick = btnExportSettings3Click
    end
    object btnExportSettings4: TButton
      Left = 19
      Top = 162
      Width = 486
      Height = 41
      Caption = #39029#38754' 1 .'#65288'1'#65289#65288'0-1'#65289'-'#65288'0-8'#65289'. '#35774#32622#65288' '#26597#35810' '#65289#26465#20214
      TabOrder = 7
      OnClick = btnExportSettings4Click
    end
  end
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 220
    Top = 10
  end
end
