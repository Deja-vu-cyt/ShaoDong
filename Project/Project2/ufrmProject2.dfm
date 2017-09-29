object frmProject2: TfrmProject2
  Left = 0
  Top = 0
  Width = 754
  Height = 538
  Align = alClient
  TabOrder = 0
  OnResize = FrameResize
  ExplicitWidth = 451
  ExplicitHeight = 304
  object dbgrdDataTable: TDBGridEh
    Left = 0
    Top = 555
    Width = 754
    Height = 123
    Align = alClient
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
    OnAdvDrawDataCell = dbgrdDataTableAdvDrawDataCell
    object RowDetailData: TRowDetailPanelControlEh
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 754
    Height = 255
    Align = alTop
    TabOrder = 1
    ExplicitWidth = 451
    object Label1: TLabel
      Left = 34
      Top = 76
      Width = 72
      Height = 13
      Alignment = taRightJustify
      Caption = #28165#21015#34892#25991#20214#65306
    end
    object Label2: TLabel
      Left = 22
      Top = 103
      Width = 84
      Height = 13
      Alignment = taRightJustify
      Caption = #34987#28165#21015#34892#30446#24405#65306
    end
    object Label3: TLabel
      Left = 306
      Top = 49
      Width = 60
      Height = 13
      Alignment = taRightJustify
      Caption = #26080#25928#21015#25968#65306
    end
    object Label4: TLabel
      Left = 58
      Top = 49
      Width = 48
      Height = 13
      Alignment = taRightJustify
      Caption = #24635#21015#25968#65306
    end
    object Label5: TLabel
      Left = 158
      Top = 49
      Width = 96
      Height = 13
      Alignment = taRightJustify
      Caption = #31532#19968#27573#21015#25968#33539#22260#65306
    end
    object Label6: TLabel
      Left = 22
      Top = 157
      Width = 84
      Height = 13
      Alignment = taRightJustify
      Caption = #23548#20986#25991#20214#30446#24405#65306
    end
    object Label7: TLabel
      Left = 6
      Top = 130
      Width = 100
      Height = 13
      Alignment = taRightJustify
      Caption = #24179#22343#22810#23569#34892'/'#25991#26412#65306
    end
    object btnClearColumn: TButton
      Left = 22
      Top = 180
      Width = 165
      Height = 30
      Caption = '1.'#25191#34892#8220#28165#21015#21512#24182#8221
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = btnClearColumnClick
    end
    object btnExportToFile: TButton
      Left = 193
      Top = 180
      Width = 264
      Height = 30
      Caption = '2.'#23548#20986#8220#25968#25454#8221#65288#28165#21015#21512#24182#21518#35774#23450#65289
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = btnExportToFileClick
    end
    object btnQueryData2: TButton
      Left = 463
      Top = 180
      Width = 330
      Height = 30
      Caption = '4.'#26597#35810#8220#30456#21516#21015#25968#23383#8221#65288#28165#21015#21512#24182#21518#35774#23450#65289
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
    object Panel4: TPanel
      Left = 138
      Top = 9
      Width = 124
      Height = 30
      Caption = #22788#29702#25152#38656#26102#38388#65306
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
    object pnlUseTime: TPanel
      Left = 268
      Top = 9
      Width = 300
      Height = 30
      Alignment = taLeftJustify
      TabOrder = 4
    end
    object pnlClearColumn: TPanel
      Left = 231
      Top = 220
      Width = 181
      Height = 30
      Alignment = taLeftJustify
      Caption = '3.'#23548#20837#8220#28165#21015#34892#8221#26174#31034#26639
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
    end
    object pnlInputData: TPanel
      Left = 9
      Top = 220
      Width = 216
      Height = 30
      Alignment = taLeftJustify
      Caption = '2.'#35835#21462#8220#34987#28165#21015#25991#26412#8221#26174#31034#26639
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
    end
    object pnlQueryData3: TPanel
      Left = 418
      Top = 220
      Width = 367
      Height = 30
      Alignment = taLeftJustify
      Caption = '[9]. '#30456#21516#21015#34892#27425#25968#25490#21015#65288'N-N'#65289#65288#37197#22871#65289' '#26174#31034#26639
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 7
    end
    object edtClearColumnFileName: TEdit
      Left = 112
      Top = 72
      Width = 346
      Height = 21
      TabOrder = 8
      OnDblClick = edtClearColumnFileNameDblClick
    end
    object edtInputDataDirectory: TEdit
      Left = 112
      Top = 99
      Width = 346
      Height = 21
      TabOrder = 9
      OnDblClick = edtInputDataDirectoryDblClick
    end
    object edtInvalidColCount: TEdit
      Left = 372
      Top = 45
      Width = 40
      Height = 21
      NumbersOnly = True
      TabOrder = 10
    end
    object edtColCount: TEdit
      Left = 112
      Top = 45
      Width = 40
      Height = 21
      NumbersOnly = True
      TabOrder = 11
    end
    object edtRangeColNo: TEdit
      Left = 260
      Top = 45
      Width = 40
      Height = 21
      NumbersOnly = True
      TabOrder = 12
    end
    object edtExportFileDirectory: TEdit
      Left = 112
      Top = 153
      Width = 346
      Height = 21
      TabOrder = 13
    end
    object edtEachFileRowCount: TEdit
      Left = 112
      Top = 126
      Width = 40
      Height = 21
      NumbersOnly = True
      TabOrder = 14
    end
    object edtInvalidColCount2: TEdit
      Left = 418
      Top = 45
      Width = 40
      Height = 21
      NumbersOnly = True
      TabOrder = 15
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 255
    Width = 754
    Height = 250
    Align = alTop
    TabOrder = 2
    ExplicitWidth = 451
    object dbgrdClearColumn: TDBGridEh
      Left = 251
      Top = 1
      Width = 132
      Height = 248
      Align = alClient
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
      Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit]
      OptionsEh = [dghFixed3D, dghHighlightFocus, dghClearSelection, dghDialogFind, dghShowRecNo, dghColumnResize, dghColumnMove]
      ParentFont = False
      ReadOnly = True
      RowHeight = 15
      RowLines = 1
      TabOrder = 0
      OnAdvDrawDataCell = dbgrdDataTableAdvDrawDataCell
      object RowDetailData: TRowDetailPanelControlEh
      end
    end
    object dbgrdFileList: TDBGridEh
      Left = 1
      Top = 1
      Width = 250
      Height = 248
      Align = alLeft
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
      Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit]
      OptionsEh = [dghFixed3D, dghHighlightFocus, dghClearSelection, dghDialogFind, dghShowRecNo, dghColumnResize, dghColumnMove]
      ParentFont = False
      RowHeight = 15
      RowLines = 1
      SumList.Active = True
      SumList.VirtualRecords = True
      TabOrder = 1
      Columns = <
        item
          DynProps = <>
          EditButtons = <>
          FieldName = 'Chosed'
          Footers = <>
          Title.Alignment = taCenter
          Title.Caption = #36873#25321
          Width = 35
        end
        item
          DynProps = <>
          EditButtons = <>
          FieldName = 'FileNo'
          Footers = <>
          Title.Alignment = taCenter
          Title.Caption = #25991#20214#21517
          Width = 100
        end
        item
          DynProps = <>
          EditButtons = <>
          FieldName = 'RowCount'
          Footer.FieldName = 'RowCount'
          Footer.ValueType = fvtSum
          Footers = <>
          Title.Alignment = taCenter
          Title.Caption = #34892#25968
          Width = 100
        end>
      object RowDetailData: TRowDetailPanelControlEh
      end
    end
    object dbgrdResultData: TDBGridEh
      Left = 383
      Top = 1
      Width = 370
      Height = 248
      Align = alRight
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
      TabOrder = 2
      TitleParams.MultiTitle = True
      OnAdvDrawDataCell = dbgrdResultDataAdvDrawDataCell
      Columns = <
        item
          DynProps = <>
          EditButtons = <>
          FieldName = 'ConformColCount'
          Footers = <>
          Title.Caption = #9312'.'#30456#21516#21015#34892#27425#25968#25490#21015#65288#26368#22810'-'#26368#23569#65289
          Width = 159
        end
        item
          DynProps = <>
          EditButtons = <>
          FieldName = 'TotalCount'
          Footers = <>
          Title.Caption = #9313'.'#30456#21516#21015#34892#27425#25968#65288#24635#20849#20960#20010#65289
          Width = 123
        end>
      object RowDetailData: TRowDetailPanelControlEh
      end
    end
  end
  object Panel5: TPanel
    Left = 0
    Top = 505
    Width = 754
    Height = 50
    Align = alTop
    TabOrder = 3
    OnDblClick = Panel5DblClick
    ExplicitWidth = 451
    object pnlQueryData2: TPanel
      Left = 12
      Top = 10
      Width = 213
      Height = 30
      Alignment = taLeftJustify
      Caption = '8.'#26597#35810#8220#30456#21516#21015#25968#23383#8221#26174#31034#26639
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object btnQueryData: TButton
      Left = 231
      Top = 10
      Width = 98
      Height = 30
      Caption = #26597#35810
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
  end
  object OpenDialog: TOpenDialog
    Left = 464
  end
end
