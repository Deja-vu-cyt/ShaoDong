object dmConfig: TdmConfig
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 150
  Width = 215
  object FDConnection: TFDConnection
    Left = 40
    Top = 16
  end
  object fdqKeyValue: TFDQuery
    Connection = FDConnection
    SQL.Strings = (
      'SELECT * FROM KeyValue')
    Left = 120
    Top = 72
  end
  object FDPhysSQLiteDriverLink: TFDPhysSQLiteDriverLink
    Left = 120
    Top = 16
  end
end
