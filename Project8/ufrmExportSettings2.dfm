object frmExportSettings2: TfrmExportSettings2
  Left = 0
  Top = 0
  ClientHeight = 395
  ClientWidth = 1101
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
    Width = 1101
    Height = 395
    HorzScrollBar.Range = 1274
    HorzScrollBar.Visible = False
    VertScrollBar.Range = 360
    VertScrollBar.Tracking = True
    Align = alClient
    AutoScroll = False
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    TabOrder = 0
    object Label5: TLabel
      Left = 21
      Top = 38
      Width = 896
      Height = 24
      Caption = 
        #65288'2'#65289#65288'0-1'#65289'. '#35774#32622' [ '#26597#35810#65288' '#21015#25968#23383' '#65289#65292#65288' '#21508' '#65289#34892#20043#38388' '#65292'            '#8594'                ' +
        #20010#65288' '#30456#21516' '#65289#21015#25968#23383' ]'
    end
    object Label1: TLabel
      Left = 39
      Top = 151
      Width = 1039
      Height = 24
      Caption = 
        #9312'-1. '#26681#25454' '#65306#9312'...'#12304' '#22635#20837' N '#65292#20363#65306'2 '#12305#20010#65288' '#30340#35774#32622' ) '#65292#20915#23450' '#65306#9313'.'#9314'. '#23548#20986#65288' TXT ) '#25991#26412' '#65307#65288' '#26368#21069' ' +
        ') [ ...] '#30340' N '#20010#21306#22495' '#12290
    end
    object Label3: TLabel
      Left = 21
      Top = 85
      Width = 288
      Height = 24
      Caption = #26681#25454' '#65306#65288'2'#65289#65288'0-1'#65289'. '#35774#32622' ... '#65306
    end
    object Label15: TLabel
      Left = 17
      Top = 253
      Width = 796
      Height = 24
      Caption = '[ '#27880' '#65306#65288' '#9312'-'#9314' '#39033' ) '#33509#37117#19981#22635#8220' N '#8221#65292#23548#20986#30340#65288' TXT ) '#25991#26412' '#65292#37117#26159#65288' '#20840#37096' ) '#30456#24212#30340#65288' '#20869#23481' ) ]'
    end
    object edtColumnGroupCount: TEdit
      Left = 548
      Top = 38
      Width = 60
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
    object edtColumnGroupCount2: TEdit
      Left = 653
      Top = 38
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
    object btnConfirm: TButton
      Left = 396
      Top = 301
      Width = 94
      Height = 41
      Caption = #30830#23450
      TabOrder = 2
      OnClick = btnConfirmClick
    end
    object chkExportFile: TCheckBox
      Left = 21
      Top = 115
      Width = 662
      Height = 30
      Caption = #9312'. '#23548#20986#65288' TXT ) '#25991#26412' '#65307' ( '#26368#21069' ) [ '#27880#65306#26368#22810#34892#25968#65306#20849' N '#34892' ] N '#20010#21306#22495' '#65306
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object chkExportFile2: TCheckBox
      Left = 21
      Top = 181
      Width = 622
      Height = 30
      Caption = #9313'. '#23548#20986#65288' TXT ) '#25991#26412' '#65307' (  '#26368#21069' ) ( '#27880#65306#26368#22823#34892#38388#24046#65306'N ) N '#20010#21306#22495' '#65306
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object chkExportFile3: TCheckBox
      Left = 21
      Top = 217
      Width = 686
      Height = 30
      Caption = #9314'. '#23548#20986#65288' TXT ) '#25991#26412' '#65307' (  '#26368#21069' ) ( '#27880#65306#26368#26032' - '#26368#22823#34892#38388#24046' = N ) N '#20010#21306#22495' '#65306
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
    object edtExportCodeNameCount: TEdit
      Left = 713
      Top = 117
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
    object edtExportCodeNameCount3: TEdit
      Left = 713
      Top = 219
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
  end
end
