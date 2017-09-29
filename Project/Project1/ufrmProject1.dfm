object frmProject1: TfrmProject1
  Left = 0
  Top = 0
  Width = 1351
  Height = 790
  TabOrder = 0
  OnResize = FrameResize
  object dbgrdDataTable: TDBGridEh
    Left = 0
    Top = 500
    Width = 1351
    Height = 290
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
    Width = 1351
    Height = 200
    Align = alTop
    TabOrder = 1
    object btnSelectClearColumn: TButton
      Left = 304
      Top = 79
      Width = 129
      Height = 30
      Caption = '3.'#23548#20837#8220#28165#21015#34892#8221
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = btnSelectClearColumnClick
    end
    object btnClearColumn: TButton
      Left = 627
      Top = 79
      Width = 165
      Height = 30
      Caption = '5.'#25191#34892#8220#28165#21015#21512#24182#8221
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = btnClearColumnClick
    end
    object btnExportToFile: TButton
      Left = 464
      Top = 115
      Width = 120
      Height = 30
      Caption = '9.'#23548#20986#8220#25968#25454#8221
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = btnExportToFileClick
    end
    object btnDeleteInvalidRow: TButton
      Left = 439
      Top = 79
      Width = 182
      Height = 30
      Caption = '4.'#21024#38500#8220#37325#22797#21450#26080#25928#34892#8221
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = btnDeleteInvalidRowClick
    end
    object btnSetColCount: TButton
      Left = 12
      Top = 79
      Width = 120
      Height = 30
      Caption = '1.'#35774#32622#8220#21015#25968#8221
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnClick = btnSetColCountClick
    end
    object btnChangeProject: TButton
      Left = 12
      Top = 9
      Width = 120
      Height = 30
      Caption = #21019#24314#39033#30446
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      OnClick = btnChangeProjectClick
    end
    object btnSortRow: TButton
      Left = 210
      Top = 115
      Width = 113
      Height = 30
      Caption = '7.'#25490#24207#8220#25968#25454#8221
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
      OnClick = btnSortRowClick
    end
    object btnQueryData2: TButton
      Left = 590
      Top = 115
      Width = 176
      Height = 30
      Caption = '10.'#26597#35810#8220#30456#21516#21015#25968#23383#8221
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 7
      OnClick = btnQueryData2Click
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
      TabOrder = 8
    end
    object pnlUseTime: TPanel
      Left = 268
      Top = 9
      Width = 300
      Height = 30
      Alignment = taLeftJustify
      TabOrder = 9
    end
    object pnlClearColumn: TPanel
      Left = 231
      Top = 162
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
      TabOrder = 10
    end
    object pnlInputData: TPanel
      Left = 9
      Top = 162
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
      TabOrder = 11
    end
    object btnInputData: TButton
      Left = 138
      Top = 79
      Width = 160
      Height = 30
      Caption = '2.'#35835#21462#8220#34987#28165#21015#25991#26412#8221
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 12
      OnClick = btnInputDataClick
    end
    object btnRearrangeClearColumn: TButton
      Left = 820
      Top = 7
      Width = 250
      Height = 30
      Caption = '[2]. '#36716#21270#8220#28165#21015#34892#8221#9313'-'#9315#65288#37197#22871#65289
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 13
      OnClick = btnRearrangeClearColumnClick
    end
    object btnSortClearColumn: TButton
      Left = 574
      Top = 7
      Width = 240
      Height = 30
      Caption = '[1]. '#20498#24207#37325#26032#25490#34892' '#9312#65288#37197#22871#65289
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 14
      OnClick = btnSortClearColumnClick
    end
    object btnCompareClearColumn: TButton
      Left = 1076
      Top = 7
      Width = 240
      Height = 30
      Caption = '[3]. '#21516#34892#27604#21015#25968#23383' '#9316#65288#37197#22871#65289
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 15
      OnClick = btnCompareClearColumnClick
    end
    object btnBuildClearColumn: TButton
      Left = 897
      Top = 43
      Width = 240
      Height = 30
      Caption = '[5]. '#29983#25104#8220#28165#21015#25991#26412#8221' '#65288#37197#22871#65289
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 16
      OnClick = btnBuildClearColumnClick
    end
    object btnQueryData3: TButton
      Left = 574
      Top = 43
      Width = 317
      Height = 30
      Caption = '[4]. '#30456#21516#21015#34892#27425#25968#25490#21015#65288'N-N'#65289#65288#37197#22871#65289
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 17
      OnClick = btnQueryData3Click
    end
    object pnlQueryData3: TPanel
      Left = 418
      Top = 162
      Width = 367
      Height = 30
      Alignment = taLeftJustify
      Caption = '[4]. '#30456#21516#21015#34892#27425#25968#25490#21015#65288'N-N'#65289#65288#37197#22871#65289' '#26174#31034#26639
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 18
    end
    object btnInputResultData: TButton
      Left = 772
      Top = 115
      Width = 161
      Height = 30
      Caption = '11.'#23548#20837#8220#26597#35810#25968#25454#8221
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 19
      OnClick = btnInputResultDataClick
    end
    object btnSetRowcount: TButton
      Left = 329
      Top = 115
      Width = 129
      Height = 30
      Caption = '8.'#35774#32622#8220#34892#25968#8221
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 20
      OnClick = btnSetRowcountClick
    end
    object btnDeleteSameRow: TButton
      Left = 12
      Top = 115
      Width = 192
      Height = 30
      Caption = '6. '#21024#38500#23436#20840#30456#21516#21015#30340#34892
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 21
      OnClick = btnDeleteSameRowClick
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 200
    Width = 1351
    Height = 250
    Align = alTop
    TabOrder = 2
    object Splitter1: TSplitter
      Left = 975
      Top = 1
      Width = 5
      Height = 248
      Align = alRight
      ExplicitLeft = 977
    end
    object dbgrdClearColumn: TDBGridEh
      Left = 251
      Top = 1
      Width = 724
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
      Left = 980
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
          FieldName = 'ConformCount'
          Footers = <>
          Title.Caption = #9312'.'#30456#21516#21015#34892#27425#25968#25490#21015#65288#26368#22810'-'#26368#23569#65289
          Width = 150
        end
        item
          DynProps = <>
          EditButtons = <>
          FieldName = 'OccurrenceCount'
          Footers = <>
          Title.Caption = #9313'.'#30456#21516#21015#34892#27425#25968#65288#24635#20849#20960#20010#65289
          Width = 120
        end
        item
          DynProps = <>
          EditButtons = <>
          FieldName = 'ConformCount2'
          Footers = <>
          Title.Caption = #9312'.'#30456#21516#21015#34892#27425#25968#25490#21015#65288#31532'1'#21306#22495#65289#65288#26368#22810'-'#26368#23569#65289
          Width = 180
        end
        item
          DynProps = <>
          EditButtons = <>
          FieldName = 'OccurrenceCount2'
          Footers = <>
          Title.Caption = #9313'.'#30456#21516#21015#34892#27425#25968#65288#31532'1'#21306#22495#65289#65288#24635#20849#20960#20010#65289
          Width = 160
        end
        item
          DynProps = <>
          EditButtons = <>
          FieldName = 'ConformCount3'
          Footers = <>
          Title.Caption = #9312'.'#30456#21516#21015#34892#27425#25968#25490#21015#65288#31532'2'#21306#22495#65289#65288#26368#22810'-'#26368#23569#65289
          Width = 180
        end
        item
          DynProps = <>
          EditButtons = <>
          FieldName = 'OccurrenceCount3'
          Footers = <>
          Title.Caption = #9313'.'#30456#21516#21015#34892#27425#25968#65288#31532'2'#21306#22495#65289#65288#24635#20849#20960#20010#65289
          Width = 160
        end>
      object RowDetailData: TRowDetailPanelControlEh
      end
    end
  end
  object Panel5: TPanel
    Left = 0
    Top = 450
    Width = 1351
    Height = 50
    Align = alTop
    TabOrder = 3
    OnDblClick = Panel5DblClick
    object pnlQueryData2: TPanel
      Left = 12
      Top = 10
      Width = 213
      Height = 30
      Alignment = taLeftJustify
      Caption = '10.'#26597#35810#8220#30456#21516#21015#25968#23383#8221#26174#31034#26639
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
      OnClick = btnQueryDataClick
    end
  end
  object OpenDialog: TOpenDialog
    Left = 448
    Top = 201
  end
end
