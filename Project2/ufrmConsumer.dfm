object frmConsumer: TfrmConsumer
  Left = 0
  Top = 0
  Caption = #22788#29702#22120
  ClientHeight = 243
  ClientWidth = 472
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object dbgrdConsumer: TDBGridEh
    Left = 0
    Top = 0
    Width = 472
    Height = 243
    Align = alClient
    DataSource = dsConsumer
    DynProps = <>
    TabOrder = 0
    Columns = <
      item
        DynProps = <>
        EditButtons = <>
        FieldName = 'ThreadID'
        Footers = <>
        Title.Alignment = taCenter
        Title.Caption = #32447#31243'ID'
        Width = 100
      end
      item
        DynProps = <>
        EditButtons = <>
        FieldName = 'FirstRow'
        Footers = <>
        Title.Alignment = taCenter
        Title.Caption = #39318#34892
        Width = 70
      end
      item
        DynProps = <>
        EditButtons = <>
        FieldName = 'CodeName'
        Footers = <>
        Title.Alignment = taCenter
        Title.Caption = #20195#21495
        Width = 100
      end>
    object RowDetailData: TRowDetailPanelControlEh
    end
  end
  object fdmtConsumer: TFDMemTable
    FieldDefs = <
      item
        Name = 'ThreadID'
        DataType = ftLargeint
      end
      item
        Name = 'FirstRow'
        DataType = ftInteger
      end
      item
        Name = 'CodeName'
        DataType = ftString
        Size = 10000
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
    Left = 56
    Top = 24
  end
  object dsConsumer: TDataSource
    DataSet = fdmtConsumer
    Left = 112
    Top = 24
  end
  object Timer: TTimer
    Enabled = False
    Interval = 2000
    OnTimer = TimerTimer
    Left = 328
    Top = 64
  end
end
