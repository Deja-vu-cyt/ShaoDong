object frmExportSettings9: TfrmExportSettings9
  Left = 0
  Top = 0
  ClientHeight = 559
  ClientWidth = 1146
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -20
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 24
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 1146
    Height = 559
    HorzScrollBar.Range = 1274
    HorzScrollBar.Visible = False
    VertScrollBar.Range = 540
    VertScrollBar.Tracking = True
    Align = alClient
    AutoScroll = False
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    TabOrder = 0
    object Label5: TLabel
      Left = 21
      Top = 46
      Width = 844
      Height = 24
      Caption = 
        #65288'11'#65289'. '#35774#32622' [ '#26597#35810#65288' '#30456#36830#21495#30721' '#65289#65292#65288' '#21508' '#65289#34892#20043#38388' '#65292'             '#8594'               '#20010#65288' ' +
        #30456#21516#20010#25968' '#65289#65306
    end
    object Label3: TLabel
      Left = 39
      Top = 165
      Width = 1039
      Height = 24
      Caption = 
        #9312'-1. '#26681#25454' '#65306#9312'...'#12304' '#22635#20837' N '#65292#20363#65306'2 '#12305#20010#65288' '#30340#35774#32622' ) '#65292#20915#23450' '#65306#9313'.'#9314'. '#23548#20986#65288' TXT ) '#25991#26412' '#65307#65288' '#26368#21069' ' +
        ') [ ...] '#30340' N '#20010#21306#22495' '#12290
    end
    object Label4: TLabel
      Left = 21
      Top = 99
      Width = 230
      Height = 24
      Caption = #26681#25454' '#65306#65288'11'#65289'. '#35774#32622' ... '#65306
    end
    object Label8: TLabel
      Left = 163
      Top = 334
      Width = 973
      Height = 24
      Caption = 
        '[ '#26597#35810#65288' '#30456#36830#21495#30721' '#65289'... N-N'#20010#65288' '#30456#36830#21495#30721' '#65289'] '#65306#12304' '#21508#21306#22495#65288' '#21512#24182' '#65289#12305' '#65307#65288' '#24182' ) '#23548#20986#65288' TXT ) '#25991#26412' ' +
        #65292#22914#19979' '#65306
    end
    object Label9: TLabel
      Left = 20
      Top = 364
      Width = 998
      Height = 24
      Caption = 
        #65288'11'#65289#65288'0-0'#65289'. [ '#26597#35810#65288' '#30456#36830#21495#30721' '#65289'... N-N'#20010#65288' '#30456#36830#21495#30721' '#65289'] '#65306#12304' '#21508#21306#22495#65288' '#21512#24182' '#65289#12305' 1  [ '#26368#22810#34892#25968 +
        ' '#65306#20849' N '#34892' ]'
    end
    object Label10: TLabel
      Left = 20
      Top = 394
      Width = 1005
      Height = 24
      Caption = 
        #65288'11'#65289#65288'0-0'#65289'. [ '#26597#35810#65288' '#30456#36830#21495#30721' '#65289'... N-N'#20010#65288' '#30456#36830#21495#30721' '#65289'] '#65306#12304' '#21508#21306#22495#65288' '#21512#24182' '#65289#12305' 2 '#65288' '#26368#22810#30456#21516#21015 +
        #25968' = N '#65289
    end
    object Label11: TLabel
      Left = 20
      Top = 424
      Width = 1032
      Height = 24
      Caption = 
        #65288'11'#65289#65288'0-0'#65289'. [ '#26597#35810#65288' '#30456#36830#21495#30721' '#65289'... N-N'#20010#65288' '#30456#36830#21495#30721' '#65289'] '#65306#12304' '#21508#21306#22495#65288' '#21512#24182' '#65289#12305' 3 '#65288' '#26368#26032' - ' +
        #26368#22823#34892#38388#24046' = N )'
    end
    object Label15: TLabel
      Left = 20
      Top = 268
      Width = 796
      Height = 24
      Caption = '[ '#27880' '#65306#65288' '#9312'-'#9314' '#39033' ) '#33509#37117#19981#22635#8220' N '#8221#65292#23548#20986#30340#65288' TXT ) '#25991#26412' '#65292#37117#26159#65288' '#20840#37096' ) '#30456#24212#30340#65288' '#20869#23481' ) ]'
    end
    object btnConfirm: TButton
      Left = 380
      Top = 499
      Width = 94
      Height = 41
      Caption = #30830#23450
      TabOrder = 0
      OnClick = btnConfirmClick
    end
    object edtValueCount: TEdit
      Left = 510
      Top = 46
      Width = 60
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
    object edtValueCount2: TEdit
      Left = 615
      Top = 46
      Width = 60
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
    object chkExportFile: TCheckBox
      Left = 21
      Top = 129
      Width = 662
      Height = 30
      Caption = #9312'. '#23548#20986#65288' TXT ) '#25991#26412' '#65307' ( '#26368#21069' ) [ '#27880#65306#26368#22810#34892#25968#65306#20849' N '#34892' ] N '#20010#21306#22495' '#65306
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object chkExportFile2: TCheckBox
      Left = 21
      Top = 195
      Width = 622
      Height = 30
      Caption = #9313'. '#23548#20986#65288' TXT ) '#25991#26412' '#65307' (  '#26368#21069' ) ( '#27880#65306#26368#22823#34892#38388#24046#65306'N ) N '#20010#21306#22495' '#65306
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object chkExportFile3: TCheckBox
      Left = 21
      Top = 231
      Width = 686
      Height = 30
      Caption = #9314'. '#23548#20986#65288' TXT ) '#25991#26412' '#65307' (  '#26368#21069' ) ( '#27880#65306#26368#26032' - '#26368#22823#34892#38388#24046' = N ) N '#20010#21306#22495' '#65306
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
    object edtExportCodeNameCount: TEdit
      Left = 713
      Top = 131
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
    object edtExportCodeNameCount2: TEdit
      Left = 713
      Top = 197
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
    object edtExportCodeNameCount3: TEdit
      Left = 713
      Top = 233
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
    object chkExportFile4: TCheckBox
      Left = 20
      Top = 298
      Width = 404
      Height = 30
      Caption = #9315'.'#65288' '#35774#32622' '#65306#9312'-'#9314' ) .'#65288' '#31526#21512#26465#20214' '#65289#30340' '#65306
      Checked = True
      State = cbChecked
      TabOrder = 9
    end
  end
end
