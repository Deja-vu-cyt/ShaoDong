object frmMain: TfrmMain
  Left = 0
  Top = 0
  ClientHeight = 661
  ClientWidth = 904
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 194
    Top = 345
    Width = 5
    Height = 316
    Align = alRight
    ExplicitLeft = 354
    ExplicitTop = 351
  end
  object Splitter2: TSplitter
    Left = 399
    Top = 345
    Width = 5
    Height = 316
    Align = alRight
    ExplicitLeft = 510
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 904
    Height = 345
    Align = alTop
    TabOrder = 0
    OnDblClick = pnlTopDblClick
    object Label1: TLabel
      Left = 5
      Top = 44
      Width = 228
      Height = 21
      Alignment = taRightJustify
      Caption = '1.'#35835#21462#8220#34987#26597#35810#65288'TXT'#65289#25991#26412#8221#65306
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 320
      Top = 79
      Width = 500
      Height = 21
      Alignment = taRightJustify
      Caption = '4.'#35774#32622#8220#26597#35810#12304#31532'N-N'#34892#12305#26012#36830#12304#31532'1-N'#34892#20026#39318#34892#12305#21015#8216' 1-N '#8217#27425#32452#21512#8221#65306
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 5
      Top = 79
      Width = 232
      Height = 21
      Alignment = taRightJustify
      Caption = '2.'#35774#32622#8220#26597#35810#65288#24635#21306#22495#65289#21015#25968#8221#65306
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 5
      Top = 114
      Width = 241
      Height = 21
      Alignment = taRightJustify
      Caption = '3.'#35774#32622#8220#26597#35810#65288#31532'1'#21306#22495#65289#21015#25968#8221#65306
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 320
      Top = 114
      Width = 519
      Height = 21
      Alignment = taRightJustify
      Caption = '5.'#35774#32622#8220#23548#20986#12304#31532'N-N'#34892#12305#26012#36830#12304#31532'1-N'#34892#20026#39318#34892#12305#21015#8216' N '#8217#20010#20197#19978#32452#21512#8221#65306
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblUseTime: TLabel
      Left = 5
      Top = 9
      Width = 119
      Height = 21
      Caption = #22788#29702#25152#38656#26102#38388#65306
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label6: TLabel
      Left = 5
      Top = 204
      Width = 519
      Height = 21
      Caption = #12304#25490#21015#12305' '#9313'.- '#9316'.'#8220'N'#8221#20010#20197#19978#21508#20010#32452#21512#65288#30456#21516#20195#21495#12289#19981#21516#39318#34892#65289#65292#22914#19979#65306
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object edtFileName: TEdit
      Left = 239
      Top = 41
      Width = 500
      Height = 29
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
    object edtCompareSpacing: TEdit
      Left = 845
      Top = 76
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
      Text = '1'
    end
    object btnCompare: TButton
      Left = 5
      Top = 308
      Width = 216
      Height = 30
      Caption = '6.'#8220#24320#22987#26597#35810#8221#24182#8220#23548#20986#25968#25454#8221
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = btnCompareClick
    end
    object edtColCount: TEdit
      Left = 252
      Top = 76
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
      Text = '256'
    end
    object edtRangeColCount: TEdit
      Left = 252
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
      Text = '256'
    end
    object edtExportTypeCount: TEdit
      Left = 845
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
      TabOrder = 5
      Text = '1'
    end
    object chkExportFile: TCheckBox
      Left = 5
      Top = 172
      Width = 796
      Height = 17
      Caption = 
        #9312'.'#12304#25490#21015#12305#8220'N'#8221#20010#20197#19978#12304' [ '#30456#21516#65288#31532#8220'N'#8221#34892#20026#39318#34892#65289'] '#12289#19981#21516'[ '#20195#21495#65306#65288#31532#8220'N.N'#8221#20010#65289#8220'N'#8221'Z.'#8220'N'#8221'Y  ] '#12305#30340#32452#21512 +
        '     '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
    end
    object chkExportFile2: TCheckBox
      Left = 5
      Top = 239
      Width = 252
      Height = 17
      Caption = #9313'. '#19981#21516#39318#34892#25968#65306#26368#22810' - '#26368#23569#34892
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 7
    end
    object chkExportFile3: TCheckBox
      Left = 275
      Top = 239
      Width = 278
      Height = 17
      Caption = #9314'. '#37051#34892#36317#65306#26368#22823#8593#8220'N'#8221'- '#26368#23567#8593#8220'N'#8221
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 8
    end
    object chkExportFile4: TCheckBox
      Left = 5
      Top = 262
      Width = 241
      Height = 17
      Caption = #9315'. '#32452#21512#25968#65306#26368#22810' - '#26368#23569#20010
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 9
    end
    object chkExportFile5: TCheckBox
      Left = 275
      Top = 262
      Width = 302
      Height = 17
      Caption = #9316'. '#26080#12304#23545#24212#21015#12305#25968#65306#26368#22810' - '#26368#23569#21015
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 10
    end
    object chkExportFile6: TCheckBox
      Left = 5
      Top = 285
      Width = 684
      Height = 17
      Caption = #9317'.'#12304#20445#23384#12305#8220'N'#8221#20010#20197#19978#21508#20010#32452#21512#65306#65288#30456#21516#20195#21495#12289#19981#21516#39318#34892#65289#30340#65288#20195#21495#65306#8220'N'#8221'='#8220'N'#8221'Z.'#8220'N'#8221'Y '#65289
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 11
    end
    object chkSelectAll: TCheckBox
      Left = 5
      Top = 149
      Width = 52
      Height = 17
      Caption = #20840#36873
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 12
      OnClick = chkSelectAllClick
    end
  end
  object dbgrdData: TDBGridEh
    Left = 0
    Top = 345
    Width = 194
    Height = 316
    Align = alClient
    DataSource = dsData
    DynProps = <>
    TabOrder = 1
    object RowDetailData: TRowDetailPanelControlEh
    end
  end
  object dbgrdFirstRow: TDBGridEh
    Left = 199
    Top = 345
    Width = 200
    Height = 316
    Align = alRight
    DataSource = dsFirstRow
    DynProps = <>
    TabOrder = 2
    object RowDetailData: TRowDetailPanelControlEh
    end
  end
  object dbgrdCompareType: TDBGridEh
    Left = 404
    Top = 345
    Width = 500
    Height = 316
    Align = alRight
    DataSource = dsCompareType
    DynProps = <>
    TabOrder = 3
    object RowDetailData: TRowDetailPanelControlEh
    end
  end
  object OpenDialog: TOpenDialog
    Filter = #25991#26412#25991#20214'|*.txt'
    Left = 312
  end
  object fdmtData: TFDMemTable
    OnNewRecord = fdmtDataNewRecord
    FieldDefs = <>
    IndexDefs = <>
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    StoreDefs = True
    Left = 352
    Top = 8
  end
  object dsData: TDataSource
    AutoEdit = False
    DataSet = fdmtData
    Enabled = False
    Left = 384
    Top = 8
  end
  object fdmtCompareType: TFDMemTable
    OnNewRecord = fdmtDataNewRecord
    FieldDefs = <>
    IndexDefs = <>
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    StoreDefs = True
    Left = 576
    Top = 8
  end
  object fdmtFirstRow: TFDMemTable
    OnNewRecord = fdmtDataNewRecord
    FieldDefs = <>
    IndexDefs = <>
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    StoreDefs = True
    Left = 432
    Top = 8
  end
  object dsFirstRow: TDataSource
    AutoEdit = False
    DataSet = fdmtFirstRow
    Enabled = False
    Left = 464
    Top = 8
  end
  object dsCompareType: TDataSource
    AutoEdit = False
    DataSet = fdmtCompareType
    Enabled = False
    Left = 608
    Top = 8
  end
end
