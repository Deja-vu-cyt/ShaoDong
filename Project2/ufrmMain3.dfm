object frmMain: TfrmMain
  Left = 0
  Top = 0
  ClientHeight = 680
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
    Height = 429
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
        Top = 245
        Width = 905
        Height = 24
        Caption = '7.'#35774#32622#8220#23548#20986#12304#65288#31532'N-N'#34892#20026#39318#34892#65289#30452#12289#26012#36830' ['#65288#31532'N-1'#34892#65289#65288#31532'1-N'#30452#12289#26012#36830#34892#65289']'#12305#21015#8220' N'#8221#20010#20197#19978#32452#21512#8221#65306
      end
      object Label18: TLabel
        Left = 12
        Top = 145
        Width = 907
        Height = 24
        Caption = '6.'#35774#32622#8220#26597#35810#12304#65288#31532'N-N'#34892#20026#39318#34892#65289#30452#12289#26012#36830' ['#65288#31532'N-1'#34892#65289#65288#31532'1-N'#30452#12289#26012#36830#34892#65289']'#12305#21015#8220'  1-N   '#8221#27425#32452#21512#8221#65306
      end
      object Label25: TLabel
        Left = 12
        Top = 180
        Width = 1183
        Height = 24
        Caption = 
          #65288'1'#65289'.'#35774#32622#12304' '#21508' [ '#26012#12289#30452#36830#65288#31532' N '#34892#20026#39318#34892#65289']'#65288#32452#21512#32452#25968#65289#26497#38480' '#12305#65306#23567#65288#25110#65289#31561#20110' '#8804'                  ' +
          ' '#20159'           '#19975'          '#20010#65288#32452#21512#32452#25968#65289
      end
      object Label26: TLabel
        Left = 12
        Top = 215
        Width = 1199
        Height = 24
        Caption = 
          #65288' '#27880#65306' '#24403#8220' 6.'#35774#32622' '#8221#12304'...'#12305#65288#33267#65289#22635#20837' '#30340#65288#32452#21512#32452#25968#65289#65292#65288#20250#65289#33258#21160#20572#27490' '#65289#26102' '#65292#20877#27604#36739#65288#24182#65289#23548#20986' '#65306#21508' [ '#26368#22823#65288#19981#21516 +
          #39318#34892#25968#65289'] '#32452#21512' '#65289
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
        Left = 407
        Top = 325
        Width = 269
        Height = 40
        Caption = '11.'#8220#24320#22987#26597#35810#8221#24182#8220#23548#20986#25968#25454#8221
        TabOrder = 6
        OnClick = btnCompareClick
      end
      object edtExportGroupValueCount: TEdit
        Left = 920
        Top = 245
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
        TabOrder = 8
      end
      object btnVertSlantExportSettings: TButton
        Left = 12
        Top = 279
        Width = 445
        Height = 40
        Caption = '8. '#35774#32622#65288#24182#65289#20445#30041' '#65306'['#65288'TXT'#65289#25991#26412' ] '#37096#20221#25968#25454
        TabOrder = 9
        OnClick = btnExportSettingsClick
      end
      object btnExportVertSlantFileSettings: TButton
        Left = 12
        Top = 325
        Width = 389
        Height = 40
        Caption = '9-10. '#36873#25321#65288#24182#65289#23548#20986' '#65306'['#65288'TXT'#65289#25991#26412' ]'
        TabOrder = 10
        OnClick = btnExportVertSlantFileSettingsClick
      end
      object edtVertSlantMaxGroupCount: TEdit
        Left = 780
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
        Left = 920
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
        Left = 1000
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
      Caption = '  '#20108'.'#26597#35810#65306#12304#21508#34892#38388#12305#30452#36830#12304#23545#24212#21015#12305#32452#21512'      '
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label11: TLabel
        Left = 9
        Top = 184
        Width = 837
        Height = 24
        Caption = '6. '#35774#32622#8220#23548#20986#12304#65288#31532'N-N'#34892#20026#39318#34892#65289#30452#36830' ['#65288#31532'N-1'#34892#65289#65288#31532' 1-N '#30452#36830#34892#65289']'#12305#21015#8220'N'#8221#20010#20197#19978#32452#21512#8221#65306
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
        Left = 9
        Top = 84
        Width = 839
        Height = 24
        Caption = '5. '#35774#32622#8220#26597#35810#12304#65288#31532'N-N'#34892#20026#39318#34892#65289#30452#36830' ['#65288#31532'N-1'#34892#65289#65288#31532' 1-N '#30452#36830#34892#65289']'#12305#21015#8220'  1-N  '#8221#27425#32452#21512#8221#65306
      end
      object Label21: TLabel
        Left = 9
        Top = 119
        Width = 1143
        Height = 24
        Caption = 
          #65288'1'#65289'.'#35774#32622#12304' '#21508' [ '#30452#36830#65288#31532' N '#34892#20026#39318#34892#65289']'#65288#32452#21512#32452#25968#65289#26497#38480' '#12305#65306#23567#65288#25110#65289#31561#20110' '#8804'                   '#20159 +
          '           '#19975'          '#20010#65288#32452#21512#32452#25968#65289
      end
      object Label22: TLabel
        Left = 9
        Top = 154
        Width = 1199
        Height = 24
        Caption = 
          #65288' '#27880#65306' '#24403#8220' 5.'#35774#32622' '#8221#12304'...'#12305#65288#33267#65289#22635#20837' '#30340#65288#32452#21512#32452#25968#65289#65292#65288#20250#65289#33258#21160#20572#27490' '#65289#26102' '#65292#20877#27604#36739#65288#24182#65289#23548#20986' '#65306#21508' [ '#26368#22823#65288#19981#21516 +
          #39318#34892#25968#65289'] '#32452#21512' '#65289
      end
      object btnVertCompare: TButton
        Left = 12
        Top = 311
        Width = 269
        Height = 40
        Caption = '10.'#8220#24320#22987#26597#35810#8221#24182#8220#23548#20986#25968#25454#8221
        TabOrder = 0
        OnClick = btnCompareClick
      end
      object edtVertExportTypeCount: TEdit
        Left = 849
        Top = 184
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
        TabOrder = 4
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
        TabOrder = 5
      end
      object btnVertExportSettings: TButton
        Left = 12
        Top = 219
        Width = 445
        Height = 40
        Caption = '7. '#35774#32622#65288#24182#65289#20445#30041' '#65306'['#65288'TXT'#65289#25991#26412' ] '#37096#20221#25968#25454
        TabOrder = 6
        OnClick = btnExportSettingsClick
      end
      object btnExportVertFileSettings: TButton
        Left = 12
        Top = 265
        Width = 373
        Height = 40
        Caption = '8-9. '#36873#25321#65288#24182#65289#23548#20986' '#65306'['#65288'TXT'#65289#25991#26412' ]'
        TabOrder = 7
        OnClick = btnExportVertFileSettingsClick
      end
      object edtVertMaxGroupCount: TEdit
        Left = 738
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
        Left = 874
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
        Left = 956
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
      Caption = '  '#19977'.'#26597#35810#65306#12304#21508#34892#38388#12305#26012#36830#12304#23545#24212#21015#12305#32452#21512'     '
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label5: TLabel
        Left = 12
        Top = 112
        Width = 688
        Height = 24
        Caption = '5. '#35774#32622#8220#23548#20986#12304#65288#31532' N-N '#34892#20026#39318#34892#65289#26012#36830#65288#31532' N-1 '#34892#65289#12305#21015#8220' N '#8221#20010#20197#19978#32452#21512#8221#65306
      end
      object Label2: TLabel
        Left = 12
        Top = 12
        Width = 690
        Height = 24
        Caption = '4. '#35774#32622#8220#26597#35810#12304#65288#31532' N-N '#34892#20026#39318#34892#65289#26012#36830#65288#31532' N-1 '#34892#65289#12305#21015#8220'   1-N   '#8221#27425#32452#21512#8221#65306
      end
      object Label23: TLabel
        Left = 12
        Top = 47
        Width = 1149
        Height = 24
        Caption = 
          #65288'1'#65289'.'#35774#32622#12304' '#21508' [ '#26012#36830#65288#31532' N '#34892#20026#39318#34892#65289']'#65288#32452#21512#32452#25968#65289#26497#38480' '#12305#65306#23567#65288#25110#65289#31561#20110' '#8804'                   '#20159 +
          '           '#19975'           '#20010#65288#32452#21512#32452#25968#65289
      end
      object Label24: TLabel
        Left = 12
        Top = 82
        Width = 1193
        Height = 24
        Caption = 
          #65288' '#27880#65306' '#24403#8220'4.'#35774#32622' '#8221#12304'...'#12305#65288#33267#65289#22635#20837' '#30340#65288#32452#21512#32452#25968#65289#65292#65288#20250#65289#33258#21160#20572#27490' '#65289#26102' '#65292#20877#27604#36739#65288#24182#65289#23548#20986' '#65306#21508' [ '#26368#22823#65288#19981#21516#39318 +
          #34892#25968#65289'] '#32452#21512' '#65289
      end
      object btnSlantCompare: TButton
        Left = 12
        Top = 248
        Width = 249
        Height = 40
        Caption = '9.'#8220#24320#22987#26597#35810#8221#24182#8220#23548#20986#25968#25454#8221
        TabOrder = 0
        OnClick = btnCompareClick
      end
      object edtExportTypeCount: TEdit
        Left = 706
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
        TabOrder = 1
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
        TabOrder = 2
      end
      object btnSlantExportSettings: TButton
        Left = 12
        Top = 156
        Width = 445
        Height = 40
        Caption = '6. '#35774#32622#65288#24182#65289#20445#30041' '#65306'['#65288'TXT'#65289#25991#26412' ] '#37096#20221#25968#25454
        TabOrder = 3
        OnClick = btnExportSettingsClick
      end
      object btnExportSlantFileSettings: TButton
        Left = 12
        Top = 202
        Width = 373
        Height = 40
        Caption = '7-8. '#36873#25321#65288#24182#65289#23548#20986' '#65306'['#65288'TXT'#65289#25991#26412' ]'
        TabOrder = 4
        OnClick = btnExportSlantFileSettingsClick
      end
      object edtSlantMaxGroupCount: TEdit
        Left = 740
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
        Left = 876
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
        Left = 962
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
      Width = 269
      Height = 24
      Caption = '1.'#35835#21462#8220#34987#26597#35810#65288'TXT'#65289#25991#26412#8221#65306
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
      Width = 293
      Height = 24
      Caption = '2.'#35774#32622#8220#26597#35810#65288#31532#19968#21306#22495#65289#8221#21015#25968#65306
    end
    object Label4: TLabel
      Left = 16
      Top = 161
      Width = 293
      Height = 24
      Caption = '3.'#35774#32622#8220#26597#35810#65288#31532#20108#21306#22495#65289#8221#21015#25968#65306
    end
    object Label6: TLabel
      Left = 321
      Top = 16
      Width = 495
      Height = 24
      Caption = #9313'.'#65288#35774#32622#65289'['#65288' '#20027#26426' IP '#22320#22336' '#65289#36830#25509#65288' N '#21488#26381#21153#22120' '#65289'] '#65306
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
    object Label19: TLabel
      Left = 414
      Top = 126
      Width = 936
      Height = 24
      Caption = #12304' '#27880#65306#65288#31532'3'#27169#24335#65289#21015#25968#33539#22260#65306'[ 25'#20010#65288'0-9'#65289']'#65307#65288#31532'4'#27169#24335#65289#21015#25968#33539#22260#65306'[ 15'#20010#65288'0-9'#65289'] -'#65288'1-106'#65289#12305#9
    end
    object Label20: TLabel
      Left = 414
      Top = 161
      Width = 576
      Height = 24
      Caption = #12304#20363#65306#19981#22635#12305' [ '#27880#65306#33509#19981#22635' '#65292#34920#31034#27492#27425#26159#65288#31532'3'#27169#24335#65289#26597#35810' ]'#9
    end
    object edtFileName: TEdit
      Left = 291
      Top = 91
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
      Left = 308
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
      Left = 308
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
      Caption = #26597#35810#65306#26012#36830#34892#65288' '#27599#34892#30340#21015#25968#23383#65292#36328#21306#22495' '#65289'( '#31532' 4 '#27169#24335' ) '
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
    object btnCodeNameSortSettings: TButton
      Left = 552
      Top = 192
      Width = 640
      Height = 40
      Caption = '3-1.'#36873#25321':'#65288' 1-10 '#65289'. '#25490#24207#65339#65288#20195#21495#65289#20986#29616#27425#25968#12289#65288#20195#21495#65289#65341#65292#20877#32452#21512
      TabOrder = 7
      OnClick = btnCodeNameSortSettingsClick
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
