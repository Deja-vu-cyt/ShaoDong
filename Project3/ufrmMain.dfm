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
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 904
    Height = 193
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
      Top = 141
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
  end
  object dbgrdData: TDBGridEh
    Left = 0
    Top = 193
    Width = 904
    Height = 468
    Align = alClient
    DataSource = dsData
    DynProps = <>
    TabOrder = 1
    object RowDetailData: TRowDetailPanelControlEh
    end
  end
  object OpenDialog: TOpenDialog
    Filter = #25991#26412#25991#20214'|*.txt'
    Left = 312
  end
  object fdmtData: TFDMemTable
    FieldDefs = <>
    CachedUpdates = True
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
end
