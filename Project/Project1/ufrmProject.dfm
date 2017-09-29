object frmProject: TfrmProject
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #39033#30446
  ClientHeight = 282
  ClientWidth = 444
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object btnAddProject: TButton
    Left = 16
    Top = 199
    Width = 90
    Height = 25
    Caption = #28155#21152#39033#30446
    TabOrder = 0
    OnClick = btnAddProjectClick
  end
  object btnDeleteProject: TButton
    Left = 112
    Top = 199
    Width = 90
    Height = 25
    Caption = #21024#38500#39033#30446
    TabOrder = 1
    OnClick = btnDeleteProjectClick
  end
  object dbgrdProject: TDBGridEh
    Left = 0
    Top = 0
    Width = 444
    Height = 193
    Align = alTop
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
    OnDblClick = dbgrdProjectDblClick
    Columns = <
      item
        DynProps = <>
        EditButtons = <>
        FieldName = 'ProjectName'
        Footers = <>
        Title.Alignment = taCenter
        Title.Caption = #39033#30446#21517#31216
        Width = 150
      end>
    object RowDetailData: TRowDetailPanelControlEh
    end
  end
  object btnOk: TButton
    Left = 168
    Top = 241
    Width = 90
    Height = 25
    Caption = #30830#23450
    TabOrder = 3
    OnClick = btnOkClick
  end
end
