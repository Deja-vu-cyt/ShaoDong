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
        FieldName = 'Address'
        Footers = <>
        Title.Alignment = taCenter
        Title.Caption = #22320#22336
        Width = 120
      end
      item
        DynProps = <>
        EditButtons = <>
        FieldName = 'FirstRow'
        Footers = <>
        Title.Alignment = taCenter
        Title.Caption = #39318#34892
        Width = 150
      end>
    object RowDetailData: TRowDetailPanelControlEh
    end
  end
  object fdmtConsumer: TFDMemTable
    FieldDefs = <
      item
        Name = 'Address'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'FirstRow'
        DataType = ftString
        Size = 200
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
end
