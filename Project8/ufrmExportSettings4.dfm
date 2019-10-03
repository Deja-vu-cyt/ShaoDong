object frmExportSettings4: TfrmExportSettings4
  Left = 0
  Top = 0
  ClientHeight = 423
  ClientWidth = 1184
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
    Width = 1184
    Height = 423
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
    ExplicitHeight = 442
    object btnConfirm: TButton
      Left = 524
      Top = 365
      Width = 94
      Height = 41
      Caption = #30830#23450
      TabOrder = 0
      OnClick = btnConfirmClick
    end
    object chkExportFile: TCheckBox
      Left = 20
      Top = 9
      Width = 686
      Height = 30
      Caption = #65288'1'#65289#65288'0-1'#65289'. '#35774#32622' [ '#26597#35810#65288' '#34892#21495' '#65289#21333' '#65307#65288' '#21508' '#65289#34892#20043#38388' '#65292#65288' '#34892#38388#24046' '#65289'] '
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object chkExportFile2: TCheckBox
      Left = 20
      Top = 45
      Width = 686
      Height = 30
      Caption = #65288'1'#65289#65288'0-2'#65289'. '#35774#32622' [ '#26597#35810#65288' '#34892#21495' '#65289#21452' '#65307#65288' '#21508' '#65289#34892#20043#38388' '#65292#65288' '#34892#38388#24046' '#65289'] '
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object chkExportFile3: TCheckBox
      Left = 20
      Top = 81
      Width = 686
      Height = 30
      Caption = #65288'1'#65289#65288'0-3'#65289'. '#35774#32622' [ '#26597#35810#65288' '#34892#21495' '#65289#22823' '#65307#65288' '#21508' '#65289#34892#20043#38388' '#65292#65288' '#34892#38388#24046' '#65289']'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object chkExportFile4: TCheckBox
      Left = 20
      Top = 117
      Width = 686
      Height = 30
      BiDiMode = bdLeftToRight
      Caption = #65288'1'#65289#65288'0-4'#65289'. '#35774#32622' [ '#26597#35810#65288' '#34892#21495' '#65289#23567' '#65307#65288' '#21508' '#65289#34892#20043#38388' '#65292#65288' '#34892#38388#24046' '#65289']'
      Checked = True
      ParentBiDiMode = False
      State = cbChecked
      TabOrder = 4
    end
    object chkExportFile5: TCheckBox
      Left = 20
      Top = 153
      Width = 730
      Height = 30
      Caption = #65288'1'#65289#65288'1-1'#65289'. '#35774#32622' [ '#26597#35810#65288' '#34892#21495' '#65289#21333' '#12289#22823' '#65307#65288' '#21508' '#65289#34892#20043#38388' '#65292#65288' '#34892#38388#24046' '#65289']'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
    object chkExportFile6: TCheckBox
      Left = 20
      Top = 189
      Width = 730
      Height = 30
      Caption = #65288'1'#65289#65288'1-2'#65289'. '#35774#32622' [ '#26597#35810#65288' '#34892#21495' '#65289#21333' '#12289#23567' '#65307#65288' '#21508' '#65289#34892#20043#38388' '#65292#65288' '#34892#38388#24046' '#65289']'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object chkExportFile7: TCheckBox
      Left = 20
      Top = 225
      Width = 730
      Height = 30
      Caption = #65288'1'#65289#65288'1-3'#65289'. '#35774#32622' [ '#26597#35810#65288' '#34892#21495' '#65289#21452' '#12289#22823' '#65307#65288' '#21508' '#65289#34892#20043#38388' '#65292#65288' '#34892#38388#24046' '#65289']'
      Checked = True
      State = cbChecked
      TabOrder = 7
    end
    object chkExportFile8: TCheckBox
      Left = 20
      Top = 261
      Width = 730
      Height = 30
      Caption = #65288'1'#65289#65288'1-4'#65289'. '#35774#32622' [ '#26597#35810#65288' '#34892#21495' '#65289#21452' '#12289#23567' '#65307#65288' '#21508' '#65289#34892#20043#38388' '#65292#65288' '#34892#38388#24046' '#65289']'
      Checked = True
      State = cbChecked
      TabOrder = 8
    end
    object chkSelectAll: TCheckBox
      Left = 20
      Top = 297
      Width = 80
      Height = 30
      Caption = #20840#36873
      Checked = True
      State = cbChecked
      TabOrder = 9
      OnClick = chkSelectAllClick
    end
    object chkDeleteProcessed: TCheckBox
      Left = 20
      Top = 329
      Width = 1160
      Height = 30
      Caption = 
        #20808#28165#38500' '#65306#24050#12304' '#8730' '#12305' '#30340#65288' '#35813#39033' '#65289#35774#32622' [ ... ] '#65292#24050#34987#65288' '#26597#35810' ) '#30340#37027#20123#34892' '#65307#20877#20445#30041#65288' '#31526#21512#26465#20214' ) '#30340#20854#23427#34892' '#65292 +
        #24182#23548#20986#65288' TXT ) '#25991#26412
      Checked = True
      State = cbChecked
      TabOrder = 10
    end
  end
end
