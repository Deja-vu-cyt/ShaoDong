object dmProject1: TdmProject1
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 274
  Width = 313
  object FDQuery: TFDQuery
    OnNewRecord = fdqClearColumnNewRecord
    CachedUpdates = True
    Connection = FDConnection
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    UpdateOptions.AssignedValues = [uvAutoCommitUpdates]
    UpdateOptions.AutoCommitUpdates = True
    Left = 168
    Top = 72
  end
  object FDConnection: TFDConnection
    LoginPrompt = False
    Left = 32
    Top = 64
  end
  object fdqDataTable: TFDQuery
    CachedUpdates = True
    Connection = FDConnection
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    Left = 112
    Top = 168
  end
  object dsDataTable: TDataSource
    DataSet = fdqDataTable
    Left = 144
    Top = 168
  end
  object fdqFileList: TFDQuery
    CachedUpdates = True
    Connection = FDConnection
    FetchOptions.AssignedValues = [evMode]
    SQL.Strings = (
      'SELECT CAST(0 AS BIT) Chosed, name FileNo, 0 [Rowcount]'
      'FROM SysObjects '
      'WHERE xtype='#39'U'#39' AND IsNumeric(name) = 1'
      'ORDER BY Cast(name AS INT)')
    Left = 112
    Top = 128
  end
  object dsFileList: TDataSource
    DataSet = fdqFileList
    Left = 144
    Top = 128
  end
  object dsClearColumn: TDataSource
    DataSet = fdqClearColumn
    Left = 48
    Top = 128
  end
  object fdqClearColumn: TFDQuery
    OnNewRecord = fdqClearColumnNewRecord
    Connection = FDConnection
    FetchOptions.AssignedValues = [evMode]
    SQL.Strings = (
      'SELECT * FROM ClearColumn')
    Left = 16
    Top = 128
  end
  object fdqKeyValue: TFDQuery
    Connection = FDConnection
    SQL.Strings = (
      'SELECT * FROM KeyValue')
    Left = 104
    Top = 64
  end
  object fdconnMaster: TFDConnection
    LoginPrompt = False
    Left = 136
    Top = 8
  end
  object fdqProject: TFDQuery
    CachedUpdates = True
    Connection = fdconnMaster
    FetchOptions.AssignedValues = [evMode]
    SQL.Strings = (
      
        'SELECT name ProjectName FROM sys.databases WHERE name NOT IN ('#39'm' +
        'aster'#39', '#39'tempdb'#39', '#39'model'#39', '#39'msdb'#39')')
    Left = 208
    Top = 128
  end
  object dsProject: TDataSource
    DataSet = fdqProject
    Left = 240
    Top = 128
  end
  object FDMemTable: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 232
    Top = 72
  end
  object dsCompareResult: TDataSource
    DataSet = fdmtCompareResult
    Left = 144
    Top = 216
  end
  object XLS: TXLSReadWriteII5
    ComponentVersion = '5.20.62'
    Version = xvExcel2007
    DirectRead = False
    DirectWrite = False
    Left = 32
    Top = 16
  end
  object fdmtCompareResult: TFDMemTable
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
    Left = 112
    Top = 216
  end
end
