object frmMain: TfrmMain
  Left = 0
  Top = 0
  ClientHeight = 804
  ClientWidth = 1173
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -20
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 24
  object Splitter1: TSplitter
    Left = 0
    Top = 549
    Width = 1173
    Height = 5
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 512
    ExplicitWidth = 1520
  end
  object dbgrdDataTable: TDBGridEh
    Left = 0
    Top = 274
    Width = 1173
    Height = 275
    Align = alClient
    DataSource = dsDataTable
    DynProps = <>
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    FooterRowCount = 1
    GridLineParams.ColorScheme = glcsClassicEh
    GridLineParams.DataHorzColor = clBlack
    GridLineParams.DataVertColor = clBlack
    GridLineParams.VertEmptySpaceStyle = dessNonEh
    IndicatorOptions = [gioShowRowIndicatorEh, gioShowRecNoEh]
    IndicatorTitle.ShowDropDownSign = True
    IndicatorTitle.TitleButton = True
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit]
    OptionsEh = [dghFixed3D, dghHighlightFocus, dghClearSelection, dghDialogFind, dghShowRecNo, dghColumnResize, dghColumnMove]
    ParentFont = False
    ReadOnly = True
    RowHeight = 15
    RowLines = 1
    TabOrder = 0
    TitleParams.MultiTitle = True
    object RowDetailData: TRowDetailPanelControlEh
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1173
    Height = 274
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 648
      Top = 143
      Width = 496
      Height = 24
      Caption = '[ '#20174#19978#33267#19979#65288' '#25353#65306#22823' '#8594' '#23567' '#65289#65288#26631#27880#65289#24207#34892#21495#65306'N - 1 = ]'
    end
    object btnQuerySameColumns: TButton
      Left = 502
      Top = 90
      Width = 242
      Height = 40
      Caption = #65288'3'#65289'.'#26597#35810#8220#30456#21516#21015#25968#23383#8221
      TabOrder = 0
      OnClick = btnQuerySameColumnsClick
    end
    object Panel4: TPanel
      Left = 18
      Top = 8
      Width = 150
      Height = 30
      Caption = #22788#29702#25152#38656#26102#38388#65306
      TabOrder = 1
    end
    object pnlUseTime: TPanel
      Left = 174
      Top = 8
      Width = 300
      Height = 30
      Alignment = taLeftJustify
      TabOrder = 2
    end
    object btnRearrangeClearColumn: TButton
      Left = 311
      Top = 136
      Width = 325
      Height = 40
      Caption = #65288#37197#22871#65289'[2]. '#36716#21270#8220#28165#21015#34892#8221#9313'-'#9315
      TabOrder = 3
      OnClick = btnRearrangeClearColumnClick
    end
    object btnSortClearColumn: TButton
      Left = 18
      Top = 136
      Width = 287
      Height = 40
      Caption = #65288#37197#22871#65289'[1]. '#20498#24207#37325#26032#25490#34892' '#9312
      TabOrder = 4
      OnClick = btnSortClearColumnClick
    end
    object btnCompareClearColumn: TButton
      Left = 18
      Top = 182
      Width = 287
      Height = 40
      Caption = #65288#37197#22871#65289'[3]. '#21516#34892#27604#21015#25968#23383' '#9316
      TabOrder = 5
      OnClick = btnCompareClearColumnClick
    end
    object btnBuildClearColumn: TButton
      Left = 863
      Top = 182
      Width = 290
      Height = 40
      Caption = #65288#37197#22871#65289'[5]. '#29983#25104#8220#28165#21015#25991#26412#8221' '
      TabOrder = 6
      Visible = False
      OnClick = btnBuildClearColumnClick
    end
    object btnQueryData3: TButton
      Left = 311
      Top = 182
      Width = 546
      Height = 40
      Caption = #65288#37197#22871#65289'[4].'#65288#25490#21015#65289#30456#21516#21015#65288#34892#27425#25968#65306'N '#26368#22810#8594#26368#23569#65289
      TabOrder = 7
      OnClick = btnQueryData3Click
    end
    object btnInputResultData: TButton
      Left = 255
      Top = 90
      Width = 241
      Height = 40
      Caption = #65288'2'#65289'.'#35835#21462#65306#8220#26597#35810#25968#25454#8221
      TabOrder = 8
      OnClick = btnInputResultDataClick
    end
    object btnIntervalValueSet: TButton
      Left = 18
      Top = 90
      Width = 231
      Height = 40
      Caption = #65288'1'#65289'.'#35774#32622#8220#21015#25968#8221#33539#22260
      TabOrder = 9
      OnClick = btnIntervalValueSetClick
    end
    object chkReverseOrder: TCheckBox
      Left = 1150
      Top = 151
      Width = 20
      Height = 17
      Checked = True
      State = cbChecked
      TabOrder = 10
    end
    object Button1: TButton
      Left = 18
      Top = 44
      Width = 183
      Height = 40
      Caption = '1.'#35774#32622#8220#21015#25968#8221#33539#22260
      TabOrder = 11
      OnClick = btnIntervalValueSetClick
    end
    object Button3: TButton
      Left = 207
      Top = 44
      Width = 226
      Height = 40
      Caption = '2.'#35835#21462#65306#8220#26597#35810#25968#25454#8221
      TabOrder = 12
      OnClick = btnInputResultDataClick
    end
    object btnExportToFile: TButton
      Left = 439
      Top = 44
      Width = 155
      Height = 40
      Caption = '3.'#23548#20986#8220#25968#25454#8221
      TabOrder = 13
      OnClick = btnExportToFileClick
    end
    object pnlQueryData2: TPanel
      Left = 20
      Top = 228
      Width = 293
      Height = 40
      Alignment = taLeftJustify
      Caption = #65288'3'#65289'.'#26597#35810#8220#30456#21516#21015#25968#23383#8221#26174#31034#26639
      TabOrder = 14
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 554
    Width = 1173
    Height = 250
    Align = alBottom
    TabOrder = 2
    object dbgrdResultData: TDBGridEh
      Left = 1
      Top = 57
      Width = 1171
      Height = 192
      Align = alClient
      DataSource = dsCompareResult
      DynProps = <>
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      FooterRowCount = 1
      GridLineParams.ColorScheme = glcsClassicEh
      GridLineParams.DataHorzColor = clBlack
      GridLineParams.DataVertColor = clBlack
      GridLineParams.VertEmptySpaceStyle = dessNonEh
      IndicatorOptions = [gioShowRowIndicatorEh, gioShowRecNoEh]
      IndicatorTitle.ShowDropDownSign = True
      IndicatorTitle.TitleButton = True
      Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit]
      OptionsEh = [dghFixed3D, dghHighlightFocus, dghClearSelection, dghDialogFind, dghShowRecNo, dghColumnResize, dghColumnMove]
      ParentFont = False
      ReadOnly = True
      RowHeight = 15
      RowLines = 1
      TabOrder = 0
      TitleParams.MultiTitle = True
      Columns = <
        item
          Alignment = taCenter
          DynProps = <>
          EditButtons = <>
          FieldName = 'ConformCount'
          Footers = <>
          Title.Caption = #9312'.'#65288#25490#21015#65289#30456#21516#21015#65288#34892#27425#25968#65306'N '#26368#22810#8594#26368#23569#65289'( '#24635#21306#22495' )'
          Width = 331
        end
        item
          Alignment = taCenter
          DynProps = <>
          EditButtons = <>
          FieldName = 'OccurrenceCount'
          Footers = <>
          Title.Caption = #9313'.'#30456#21516#21015#65288#34892#27425#25968#65306#24635#20849#20960#20010#65289'( '#24635#21306#22495' )'
          Width = 233
        end
        item
          Alignment = taCenter
          DynProps = <>
          EditButtons = <>
          FieldName = 'ConformCount2'
          Footers = <>
          Title.Caption = #9312'.'#65288#25490#21015#65289#30456#21516#21015#65288#34892#27425#25968#65306'N '#26368#22810#8594#26368#23569#65289#65288#31532'1'#21306#22495#65289
          Width = 344
        end
        item
          Alignment = taCenter
          DynProps = <>
          EditButtons = <>
          FieldName = 'OccurrenceCount2'
          Footers = <>
          Title.Caption = #9313'.'#30456#21516#21015#65288#34892#27425#25968#65306#24635#20849#20960#20010#65289#65288#31532'1'#21306#22495#65289
          Width = 242
        end
        item
          Alignment = taCenter
          DynProps = <>
          EditButtons = <>
          FieldName = 'ConformCount3'
          Footers = <>
          Title.Caption = #9312'.'#65288#25490#21015#65289#30456#21516#21015#65288#34892#27425#25968#65306'N '#26368#22810#8594#26368#23569#65289#65288#31532'2'#21306#22495#65289
          Width = 330
        end
        item
          Alignment = taCenter
          DynProps = <>
          EditButtons = <>
          FieldName = 'OccurrenceCount3'
          Footers = <>
          Title.Caption = #9313'.'#30456#21516#21015#65288#34892#27425#25968#65306#24635#20849#20960#20010#65289#65288#31532'2'#21306#22495#65289
          Width = 243
        end>
      object RowDetailData: TRowDetailPanelControlEh
      end
    end
    object Panel5: TPanel
      Left = 1
      Top = 1
      Width = 1171
      Height = 56
      Align = alTop
      TabOrder = 1
      object pnlQueryData3: TPanel
        Left = 18
        Top = 9
        Width = 575
        Height = 40
        Alignment = taLeftJustify
        Caption = #65288#37197#22871#65289'[4].'#65288#25490#21015#65289#30456#21516#21015#65288#34892#27425#25968#65306'N '#26368#22810#8594#26368#23569#65289' '#26174#31034#26639
        TabOrder = 0
      end
    end
  end
  object OpenDialog: TOpenDialog
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 1035
    Top = 59
  end
  object fdmtCompareResult: TFDMemTable
    OnNewRecord = fdmtCompareResultNewRecord
    FieldDefs = <
      item
        Name = 'Flag'
        DataType = ftInteger
      end
      item
        Name = 'ConformCount'
        DataType = ftInteger
      end
      item
        Name = 'OccurrenceCount'
        DataType = ftInteger
      end
      item
        Name = 'ConformCount2'
        DataType = ftInteger
      end
      item
        Name = 'OccurrenceCount2'
        DataType = ftInteger
      end
      item
        Name = 'ConformCount3'
        DataType = ftInteger
      end
      item
        Name = 'OccurrenceCount3'
        DataType = ftInteger
      end>
    IndexDefs = <>
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    StoreDefs = True
    Left = 1208
    Top = 128
  end
  object dsCompareResult: TDataSource
    DataSet = fdmtCompareResult
    Left = 1240
    Top = 128
  end
  object dsDataTable: TDataSource
    DataSet = fdmtDataTable
    Left = 1240
    Top = 96
  end
  object fdmtDataTable: TFDMemTable
    FieldDefs = <
      item
        Name = 'ValueCount'
        DataType = ftSmallint
      end
      item
        Name = 'ValueCount2'
        DataType = ftSmallint
      end
      item
        Name = 'ConformColCount'
        DataType = ftInteger
      end
      item
        Name = 'ConformColCount2'
        DataType = ftInteger
      end
      item
        Name = 'FolderNo'
        DataType = ftInteger
      end
      item
        Name = 'FileNo'
        DataType = ftInteger
      end
      item
        Name = 'RowNo'
        DataType = ftInteger
      end
      item
        Name = 'Field1'
        DataType = ftLargeint
      end
      item
        Name = 'Field2'
        DataType = ftLargeint
      end
      item
        Name = 'Field3'
        DataType = ftLargeint
      end
      item
        Name = 'Field4'
        DataType = ftLargeint
      end>
    IndexDefs = <>
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    StoreDefs = True
    Left = 1208
    Top = 96
  end
end
