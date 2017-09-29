unit udmProject1;

interface

uses
  uLog, uGlobal,
  System.SysUtils, System.Classes, System.Math, System.JSON, System.Threading,
  System.IOUtils, System.Variants, System.Generics.Collections, Winapi.Windows,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, DBGridEh, System.Types,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.UI.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Phys,
  Vcl.Dialogs, Vcl.CheckLst, Vcl.FileCtrl, Vcl.StdCtrls, Vcl.Controls,
  XLSSheetData5, XLSReadWriteII5, Xc12DataStyleSheet5, Xc12Utils5, XLSCmdFormat5,
  FireDAC.VCLUI.Wait, FireDAC.Phys.MSSQLDef,
  FireDAC.Phys.ODBCBase, FireDAC.Phys.MSSQL, FireDAC.Comp.UI, MemTableDataEh,
  DataDriverEh, MemTableEh;

type
  TdmProject1 = class(TDataModule)
    FDQuery: TFDQuery;
    FDConnection: TFDConnection;
    fdqDataTable: TFDQuery;
    dsDataTable: TDataSource;
    fdqFileList: TFDQuery;
    dsFileList: TDataSource;
    dsClearColumn: TDataSource;
    fdqClearColumn: TFDQuery;
    fdqKeyValue: TFDQuery;
    fdconnMaster: TFDConnection;
    fdqProject: TFDQuery;
    dsProject: TDataSource;
    FDMemTable: TFDMemTable;
    dsCompareResult: TDataSource;
    XLS: TXLSReadWriteII5;
    fdmtCompareResult: TFDMemTable;
    procedure DataModuleCreate(Sender: TObject);
    procedure fdqTaskListNewRecord(DataSet: TDataSet);
    procedure DataModuleDestroy(Sender: TObject);
    procedure fdqClearColumnNewRecord(DataSet: TDataSet);
  private
    FDatabaseName: string;
    FWorkState: TWorkState;
    FStopTime: Boolean;

    FColCount: Integer;
    FRangeColNo: Integer;
    FValidColCount: Integer;
    FValidColCount2: Integer;

    l: TStringList;

    procedure SetWorkState(AValue: TWorkState);
    procedure ShowLog(ALogTime: TDateTime; AMsg: string);
    procedure StartTime;
    procedure StopTime;
    function GetFieldString: string;

    procedure OpenFileList;
    procedure OpenClearColumn;
    procedure WriteKeyValue(AKey: string; AValue: Variant);
    function ReadKeyValue(AKey: string; ADefaultValue: Variant): string;

    procedure DeleteRepeatRow(AreAllFiles: Boolean);
    procedure LoadData(AFileName: string; var Arr, Arr2: TIntDyadicArray);
    procedure SetDefaultValue(DataSet: TDataSet);
  public
    procedure CreateDatabase;
    procedure DeleteDatabase;
    procedure ShrinkDatabase(ADatabaseName: string; OnlyLog: Boolean = False);
    procedure ConnectDatabase(ADatabaseName: string);
    procedure ChangeProject;

    procedure BuildDataTable(AFileNo: Integer);
    procedure InitDataTable;
    procedure SetColNo;
    procedure InputData;
    procedure InputResultData;
    procedure InputClearColumn;
    procedure SetDeleteInvalidRow;
    procedure ClearColumn;
    procedure SortRow;
    procedure SetRowcount;
    procedure ExportToFile;

    procedure BuildClearColumn(AFlag: Integer = 0);
    procedure SortClearColumn;
    procedure RearrangeClearColumn;
    procedure CompareClearColumn;
    procedure DeleteSameRow;

    procedure Init;
    procedure QueryData;
    procedure QueryData2;
    procedure QueryData3;
    property ColCount: Integer read FColCount;
    property RangeColNo: Integer read FRangeColNo;
  end;

var
  dmProject1: TdmProject1;

implementation

uses
  ufrmProject1, ufrmInput, udmConfig, ufrmSingleChoice, ufrmConfirm, udmRegister,
  ufrmProject, ufrmQueryConfig2, ufrmQueryConfig3, UfrmMain, udmMain;

{%CLASSGROUP 'VCL.Controls.TControl'}

{$R *.dfm}

procedure TdmProject1.DataModuleCreate(Sender: TObject);
begin
  frmProject1 := TfrmProject1.Create(frmMain);
  frmProject1.Align := alClient;
  frmProject1.dbgrdFileList.DataSource := dsFileList;
  frmProject1.dbgrdDataTable.DataSource := dsDataTable;
  frmProject1.dbgrdClearColumn.DataSource := dsClearColumn;
  frmProject1.dbgrdResultData.DataSource := dsCompareResult;

  with FDConnection.Params do
  begin
    Clear;
    Values['DriverID'] := 'MSSQL';
    Values['Server'] := dmConfig.ReadKeyValue('ServerName', '.');
    Values['User_Name'] := dmConfig.ReadKeyValue('UserName', 'sa');
    Values['Password'] := dmConfig.ReadKeyValue('Password', 'sa2016');
    Values['Database'] := 'master';
  end;
  with fdconnMaster.Params do
  begin
    Clear;
    Values['DriverID'] := 'MSSQL';
    Values['Server'] := dmConfig.ReadKeyValue('ServerName', '.');
    Values['User_Name'] := dmConfig.ReadKeyValue('UserName', 'sa');
    Values['Password'] := dmConfig.ReadKeyValue('Password', 'sa2016');
    Values['Database'] := 'master';
  end;

  XLS.CmdFormat.BeginEdit(Nil);
  XLS.CmdFormat.Border.Style := cbsThin;
  XLS.CmdFormat.Border.Preset(cbspOutline);
  XLS.CmdFormat.Alignment.Horizontal := chaCenter;
  XLS.CmdFormat.Alignment.Vertical := cvaCenter;
  XLS.DefaultFormat := XLS.CmdFormat.AddAsDefault('F1');

  l := TStringList.Create;
  fdmtCompareResult.CreateDataSet;
  fdmtCompareResult.AddIndex('ConformCount', 'ConformCount;ConformCount2;ConformCount3', '', [soDescending, soDescending, soDescending]);
  fdmtCompareResult.IndexName := 'ConformCount';
end;

procedure TdmProject1.DataModuleDestroy(Sender: TObject);
begin
  l.Free;
  FDConnection.Close;
  fdconnMaster.Close;
end;

procedure TdmProject1.fdqClearColumnNewRecord(DataSet: TDataSet);
var
  f: TField;
begin
  f := DataSet.FindField('Field1');
  if Assigned(f) then f.AsLargeInt := 0;
  f := DataSet.FindField('Field2');
  if Assigned(f) then f.AsLargeInt := 0;
  f := DataSet.FindField('Field3');
  if Assigned(f) then f.AsLargeInt := 0;
  f := DataSet.FindField('Field4');
  if Assigned(f) then f.AsLargeInt := 0;
end;

procedure TdmProject1.fdqTaskListNewRecord(DataSet: TDataSet);
begin
  DataSet.FieldByName('Executed').AsBoolean := False;
end;

procedure TdmProject1.SetWorkState(AValue: TWorkState);
var
  CanWork: Boolean;
begin
  FWorkState := AValue;
  TThread.Synchronize(nil, procedure
  begin
    CanWork := not dmRegister.ProbationExpired and (FWorkState = wsNone) and (FDatabaseName <> 'master');

    frmProject1.btnSetColCount.Enabled := CanWork;
    frmProject1.btnSelectClearColumn.Enabled := CanWork;
    frmProject1.btnInputData.Enabled := CanWork or (FWorkState = wsInputData);
    frmProject1.btnDeleteInvalidRow.Enabled := CanWork;
    frmProject1.btnClearColumn.Enabled := CanWork or (FWorkState = wsClearColumn);
    frmProject1.btnSortRow.Enabled := CanWork;
    frmProject1.btnSetRowcount.Enabled := CanWork;
    frmProject1.btnExportToFile.Enabled := CanWork or (FWorkState = wsExportToFile);
    frmProject1.btnInputResultData.Enabled := CanWork;
    frmProject1.btnQueryData.Enabled := CanWork;
    frmProject1.btnQueryData2.Enabled := CanWork;

    frmProject1.btnSortClearColumn.Enabled := CanWork;
    frmProject1.btnRearrangeClearColumn.Enabled := CanWork;
    frmProject1.btnCompareClearColumn.Enabled := CanWork;
    frmProject1.btnBuildClearColumn.Enabled := CanWork;
    frmProject1.btnQueryData3.Enabled := CanWork;
    frmProject1.btnDeleteSameRow.Enabled := CanWork;

    frmProject1.dbgrdFileList.Enabled := CanWork;
    frmProject1.dbgrdDataTable.Enabled := CanWork;
    frmProject1.dbgrdClearColumn.Enabled := CanWork;
    frmProject1.dbgrdResultData.Enabled := CanWork;

    case FWorkState of
      wsInputData:
      begin
        frmProject1.btnInputData.Tag := 1;
        frmProject1.btnInputData.Caption := '2.停止';
      end;
      wsInputResultData:
      begin
        frmProject1.btnInputResultData.Tag := 1;
        frmProject1.btnInputResultData.Caption := '12.停止';
      end;
      wsClearColumn:
      begin
        frmProject1.btnClearColumn.Tag := 1;
        frmProject1.btnClearColumn.Caption := '5.停止';
      end;
      wsExportToFile:
      begin
        frmProject1.btnExportToFile.Tag := 1;
        frmProject1.btnExportToFile.Caption := '9.停止';
      end
      else
      begin
        frmProject1.btnInputData.Tag := 0;
        frmProject1.btnInputData.Caption := '2.读取“被清列文本”';
        frmProject1.btnInputResultData.Tag := 0;
        frmProject1.btnInputResultData.Caption := '11.导入“查询数据”';
        frmProject1.btnClearColumn.Tag := 0;
        frmProject1.btnClearColumn.Caption := '5.执行“清列合并”';
        frmProject1.btnExportToFile.Tag := 0;
        frmProject1.btnExportToFile.Caption := '9.导出“数据”';
      end;
    end;
  end);
end;

procedure TdmProject1.ShowLog(ALogTime: TDateTime; AMsg: string);
begin
  
end;

procedure TdmProject1.StartTime;
begin
  FStopTime := False;

  TTask.Create(procedure
  var
    UseTime: string;
    StartTime: Cardinal;
    Day, Hour, Min, Sec, Seconds: Integer;
  begin
    StartTime := GetTickCount;
    while True do
    begin
      Seconds := (GetTickCount - StartTime) div  1000;
      Day := Seconds div 86400;
      Hour := (Seconds mod 86400) div 3600;
      Min := (Seconds mod 3600) div 60;
      Sec := Seconds mod 60;
      UseTime := Format('%d日%d时%d分%d秒', [Day, Hour, Min, Sec]);

      TThread.Synchronize(nil, procedure
      begin
        frmProject1.pnlUseTime.Caption := UseTime;
      end);

      if FStopTime then Break;
      Sleep(1000);
    end;
  end).Start;
end;

procedure TdmProject1.StopTime;
begin
  TThread.Synchronize(nil, procedure
  begin
    FStopTime := True;
  end);
end;

function TdmProject1.GetFieldString: string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Ceil(FColCount / 64) do
  begin
    if not Result.IsEmpty then Result := Result + ', ';
    Result := Result + 'Field' + i.ToString;
  end;
end;

procedure TdmProject1.OpenFileList;
begin
  dsFileList.DataSet := nil;
  if fdqFileList.Active then fdqFileList.Refresh
  else fdqFileList.Open;
  fdqFileList.FieldByName('Chosed').ReadOnly := False;
  TThread.Synchronize(nil, procedure
  begin
    dsFileList.DataSet := fdqFileList;
  end);
end;

procedure TdmProject1.OpenClearColumn;
begin
  dsClearColumn.DataSet := nil;
  if fdqClearColumn.Active then fdqClearColumn.Refresh
  else fdqClearColumn.Open;
  TThread.Synchronize(nil, procedure
  begin
    dsClearColumn.DataSet := fdqClearColumn;
  end);
end;

procedure TdmProject1.WriteKeyValue(AKey: string; AValue: Variant);
begin
  if fdqKeyValue.Locate('KeyName', AKey, []) then fdqKeyValue.Edit
  else
  begin
    fdqKeyValue.Append;
    fdqKeyValue.FieldByName('KeyName').AsString := AKey;
  end;
  fdqKeyValue.FieldByName('KeyValue').AsString := VarToStr(AValue).Trim;
  fdqKeyValue.Post;
end;

function TdmProject1.ReadKeyValue(AKey: string; ADefaultValue: Variant): string;
begin
  if fdqKeyValue.Active and fdqKeyValue.Locate('KeyName', AKey, []) then
    Result := fdqKeyValue.FieldByName('KeyValue').AsString.Trim
  else Result := VarToStr(ADefaultValue).Trim;
end;

procedure TdmProject1.DeleteRepeatRow(AreAllFiles: Boolean);
var
  s, sField: string;
begin
  sField := GetFieldString;
  if not AreAllFiles then sField := 'FileNo, ' + sField;

  s := 'WITH CTE AS (' + #$D#$A
    + 'SELECT ROW_NUMBER() OVER(PARTITION BY %s ORDER BY (SELECT 0)) RowNo' + #$D#$A
    + 'FROM V_DataTable' + #$D#$A
    + ')' + #$D#$A
    + 'DELETE FROM CTE WHERE RowNo > 1';
  s := Format(s, [GetFieldString]);

  FDConnection.ExecSQL(s);
end;

procedure TdmProject1.LoadData(AFileName: string; var Arr, Arr2: TIntDyadicArray);
var
  i, RowNo: Integer;
  s: string;
begin
  SetLength(Arr, 0);
  SetLength(Arr2, 0);
  l.LoadFromFile(AFileName);
  for i := 0 to l.Count - 1 do
  begin
    if not TryStrToInt(l.Names[i].Trim, RowNo) then Continue;
    if RowNo > Length(Arr) then
    begin
      SetLength(Arr, RowNo);
      SetLength(Arr2, RowNo);
    end;
    s := l.ValueFromIndex[i];
    StrToIntArray(s, Arr[RowNo - 1], Arr2[RowNo - 1]);
  end;
end;

procedure TdmProject1.SetDefaultValue(DataSet: TDataSet);
var
  f: TField;
begin
  f := DataSet.FindField('Field');
end;

procedure TdmProject1.CreateDatabase;
var
  s, DBFileName, LogFileName, FilePath, ProjectName: string;
  i: Integer;
begin
  if not Input('', '请输入项目名称', ProjectName) or ProjectName.Trim.IsEmpty then Exit;
  if not SelectDirectory('项目储存', '', FilePath) then Exit;
  FilePath := FilePath + '\';
  try
    DBFileName := FilePath + ProjectName + '.mdf';
    LogFileName := FilePath + ProjectName + '_log.ldf';
    s := 'CREATE Database %s' + #$D#$A
      + 'ON (' + #$D#$A
      + 'Name = ''%s'',' + #$D#$A
      + 'Filename = ''%s'',' + #$D#$A
      + 'Size = 5mb,' + #$D#$A
      + 'FileGrowth = 100mb' + #$D#$A
      + ')' + #$D#$A
      + 'Log ON (' + #$D#$A
      + 'Name = ''%s_log'',' + #$D#$A
      + 'Filename = ''%s'',' + #$D#$A
      + 'Size = 2mb,' + #$D#$A
      + 'FileGrowth = 100mb,' + #$D#$A
      + 'MaxSize = UNLIMITED' + #$D#$A
      + ')';
    s := Format(s, [ProjectName, ProjectName, DBFileName, ProjectName, LogFileName]);
    fdconnMaster.ExecSQL(s);
    fdconnMaster.ExecSQL('USE ' + ProjectName);
    //设置简单模式
    s := 'ALTER DATABASE %s SET RECOVERY SIMPLE';
    s := Format(s, [ProjectName]);
    fdconnMaster.ExecSQL(s);

    s := 'CREATE TABLE KeyValue(' + #$D#$A
      + '[KeyName] [varchar](50) NOT NULL,' + #$D#$A
      + '[keyValue] [varchar](512) NOT NULL' + #$D#$A
      + ') ON [PRIMARY]';
    fdconnMaster.ExecSQL(s);
    //计算有值列数
    s := 'CREATE FUNCTION CalcCount(@i BIGINT)' + #$D#$A
      + 'RETURNS SMALLINT' + #$D#$A
      + 'AS' + #$D#$A
      + 'BEGIN' + #$D#$A
      + '  DECLARE @iCount SMALLINT' + #$D#$A
      + '  SET @iCount = 0' + #$D#$A
      + '  WHILE @i <> 0' + #$D#$A
      + '  BEGIN' + #$D#$A
      + '    IF @i = -9223372036854775808 SET @i = 0' + #$D#$A
      + '    ELSE SET @i = @i & (@i - 1)' + #$D#$A
      + '    SET @iCount = @iCount + 1' + #$D#$A
      + '  END' + #$D#$A
      + '  RETURN @iCount' + #$D#$A
      + 'END';
    fdconnMaster.ExecSQL(s);
    //计算有值列数
    s := 'CREATE FUNCTION BigIntToBinStr(@i BIGINT)' + #$D#$A
      + 'RETURNS CHAR(64)' + #$D#$A
      + 'AS' + #$D#$A
      + 'BEGIN' + #$D#$A
      + '  DECLARE @cResult VARCHAR(64), @i2 TINYINT, @i3 BIGINT' + #$D#$A
      + '  SET @cResult = ''''' + #$D#$A
      + '  SET @i2 = 64' + #$D#$A
      + '  WHILE @i2 > 0' + #$D#$A
      + '  BEGIN' + #$D#$A;
    for i := 64 downto 1 do
    begin
      if i = 64 then s := s + Format('    IF @i2 = %d SET @i3 = %d' + #$D#$A, [i, i64 shl (i - 1)])
      else s := s + Format('    ELSE IF @i2 = %d SET @i3 = %d' + #$D#$A, [i, i64 shl (i - 1)]);
    end;
    s := s + '    IF @i = @i | @i3 SET @cResult = @cResult + ''1'' ELSE SET @cResult = @cResult + ''0''' + #$D#$A
      + '    SET @i2 = @i2 - 1' + #$D#$A
      + '  END' + #$D#$A
      + '  RETURN @cResult' + #$D#$A
      + 'END';
    fdconnMaster.ExecSQL(s);
  finally
    fdconnMaster.ExecSQL('USE master')
  end;
  fdqProject.Refresh;
end;

procedure TdmProject1.DeleteDatabase;
var
  ProjectName: string;
begin
  if fdqProject.RecordCount = 0 then Exit;
  ProjectName := fdqProject.FieldByName('ProjectName').AsString;
  if FDatabaseName = ProjectName then ConnectDatabase('master');
  fdconnMaster.ExecSQL('DROP DATABASE ' + ProjectName);
  fdqProject.Refresh;
end;

procedure TdmProject1.ShrinkDatabase(ADatabaseName: string; OnlyLog: Boolean = False);
var
  s: string;
begin
  FLog.Log('正在收缩数据库，请稍等...');
  try
    s := 'DBCC SHRINKFILE(N''%s_log'', 0, TRUNCATEONLY)';
    s := Format(s, [ADatabaseName]);
    if not OnlyLog then
    begin
      s := s + #$D#$A + 'DBCC SHRINKFILE(N''%s'', 0, TRUNCATEONLY)';
      s := Format(s, [ADatabaseName]);
    end;
    FDConnection.ExecSQL(s);

    FLog.Log('正在收缩数据库完成');
  except
    on e: Exception do
    begin
      e.Message := '收缩数据库失败：' + e.Message;
      FLog.Log(e.Message);
    end;
  end;

end;

procedure TdmProject1.ConnectDatabase(ADatabaseName: string);
var
  ProjectName: string;
begin
  try
    FDConnection.Close;
    FDConnection.Params.Values['Database'] := ADatabaseName;
    FDConnection.Open;
    if not fdconnMaster.Connected then fdconnMaster.Open;
    FDatabaseName := ADatabaseName;
    FLog.Log(Format('连接数据库[%s]成功', [FDatabaseName]));
  except
    on e: Exception do
    begin
      FLog.Log(Format('连接数据库[%s]失败：%s', [ADatabaseName, e.Message]));
    end;
  end;

  ProjectName := '';
  if FDConnection.Connected then
  begin
    TThread.Synchronize(nil, procedure
    begin
      dmConfig.WriteKeyValue('DatabaseName', FDatabaseName);
    end);
    
    if FDatabaseName <> 'master' then
    begin
      fdqKeyValue.Open;
      FColCount := ReadKeyValue('ColCount', 0).ToInteger;
      FRangeColNo := ReadKeyValue('RangeColNo', 0).ToInteger;
      FValidColCount := ReadKeyValue('ValidColCount', 0).ToInteger;
      FValidColCount2 := ReadKeyValue('ValidColCount2', 0).ToInteger;

      TThread.Synchronize(nil, procedure
      begin
        frmProject1.BuildDataTableGridColumn(1);
        frmProject1.BuildClearColumnGridColumn;
      end);

      if FColCount > 0 then
      begin
        OpenFileList;
        OpenClearColumn;
      end;

      ProjectName := FDatabaseName;
    end;
  end;
  TThread.Synchronize(nil, procedure
  begin
    frmMain.Caption := ProjectName;
    SetWorkState(wsNone);
    if Assigned(frmQueryConfig2) then FreeAndNil(frmQueryConfig2);
  end);
end;

procedure TdmProject1.ChangeProject;
begin
  if fdqProject.Active then fdqProject.Refresh
  else fdqProject.Open;

  if not Assigned(frmProject) then
  begin
    frmProject := TfrmProject.Create(Self);
    frmProject.dbgrdProject.DataSource := dsProject;
  end;
  frmProject.ShowModal;
  if frmProject.ModalResult <> 1 then Exit;

  ConnectDatabase(fdqProject.FieldByName('ProjectName').AsString);
end;

procedure TdmProject1.BuildDataTable(AFileNo: Integer);
var
  s, sField: string;
  i: Integer;
begin
  for i := 1 to Ceil(FColCount / 64) do sField := sField + Format('[Field%d] [bigint] NOT NULL,', [i]) + #$D#$A;
  s := 'IF Object_Id(N''%d'', N''U'') IS NOT NULL DROP TABLE [%d]' + #$D#$A
    + 'CREATE TABLE [%d] (' + #$D#$A
    + '[Id] [bigint] IDENTITY(1,1) PRIMARY KEY NOT NULL,' + #$D#$A
    + sField
    + '[ValueCount] [smallint] NOT NULL,' + #$D#$A
    + '[ValueCount2] [smallint] NOT NULL' + #$D#$A
    + ')';
  s := Format(s, [AFileNo, AFileNo, AFileNo]);
  FDConnection.ExecSQL(s);
end;

procedure TdmProject1.InitDataTable;
var
  s: string;
  FileNo: Integer;
begin
  fdqFileList.DisableControls;
  try
    s := 'IF Object_Id(N''V_DataTable'', N''V'') IS NOT NULL DROP VIEW V_DataTable';
    FDConnection.ExecSQL(s);

    s := 'CREATE VIEW V_DataTable AS' + #$D#$A;
    fdqFileList.First;
    while not fdqFileList.Eof do
    begin
      FileNo := fdqFileList.FieldByName('FileNo').AsInteger;
      if fdqFileList.RecNo > 1 then
        s := s + 'UNION ALL' + #$D#$A;
      s := s + Format( 'SELECT %d FileNo2, * FROM [%d]' + #$D#$A, [FileNo, FileNo]) ;

      fdqFileList.Next;
    end;

    FDConnection.ExecSQL(s);
  finally
    fdqFileList.EnableControls;
  end;
end;

procedure TdmProject1.SetColNo;
begin
  FColCount := ReadKeyValue('ColCount', 20).ToInteger;
  if not Input('', '请设置总列数：', FColCount) then Exit;
  if not ((FColCount >= 1) and (FColCount <= 256)) then raise Exception.Create('请输入有效数值');

  FRangeColNo := FColCount;
  if not Input('', '输入第一段列数范围', FRangeColNo) then Exit;
  if not ((FRangeColNo >= 1) and (FRangeColNo <= FColCount)) then raise Exception.Create('请输入有效数值');

  TTask.Create(procedure
  var
    s, sField, sTableList: string;
    i: Integer;
  begin
    SetWorkState(wsExecuting);
    StartTime;
    fdqFileList.DisableControls;
    try
      try
        for i := 1 to Ceil(FColCount / 64) do sField := sField + Format('[Field%d] [bigint] NOT NULL,', [i]) + #$D#$A;
        s := ''{'IF Object_Id(N''DataTable'', N''U'') IS NOT NULL DROP TABLE DataTable' + #$D#$A
          + 'CREATE TABLE [DataTable] (' + #$D#$A
          + '[FileNo] [smallint] NOT NULL,' + #$D#$A
          + sField
          + '[ValueCount] [smallint] NOT NULL,' + #$D#$A
          + ')' + #$D#$A
          //+ 'CREATE INDEX DataTable_FileNo ON DataTable(FileNo)' + #$D#$A  }

          {+ 'IF Object_Id(N''ResultData'', N''U'') IS NOT NULL DROP TABLE ResultData' + #$D#$A
          + 'CREATE TABLE [ResultData] (' + #$D#$A
          + '[Id] [bigint] IDENTITY(1,1) NOT NULL,' + #$D#$A
          + '[FolderNo] [smallint] NOT NULL,' + #$D#$A
          + '[FileNo] [smallint] NOT NULL,' + #$D#$A
          + '[RowNo] [smallint] NOT NULL,' + #$D#$A
          + '[ValueCount] [smallint] NOT NULL,' + #$D#$A
          + sField
          + '[ConformColCount] [smallint] NOT NULL,' + #$D#$A
          + 'PRIMARY KEY NONCLUSTERED (Id)' + #$D#$A
          + ')' + #$D#$A
          + 'CREATE CLUSTERED INDEX ResultData_CI ON ResultData(FolderNo, FileNo, RowNo)' + #$D#$A}

          + 'IF Object_Id(N''ClearColumn'', N''U'') IS NOT NULL DROP TABLE ClearColumn' + #$D#$A
          + 'CREATE TABLE [ClearColumn] (' + #$D#$A
          + sField
          + '[FileNo] [smallint] NOT NULL' + #$D#$A
          + ')';

        if fdqFileList.Active then
        begin
          fdqFileList.First;
          while not fdqFileList.Eof do
          begin
            s := s + #$D#$A + Format('IF Object_Id(N''%d'', N''U'') IS NOT NULL DROP TABLE [%d]', [
              fdqFileList.FieldByName('FileNo').AsInteger,
              fdqFileList.FieldByName('FileNo').AsInteger
            ]);

            fdqFileList.Next;
          end;
        end;

        FDConnection.ExecSQL(s);

        WriteKeyValue('ColCount', FColCount);
        WriteKeyValue('RangeColNo', FRangeColNo);
        WriteKeyValue('ClearColumnType', 0);
        WriteKeyValue('ClearColumnFileNo', 0);
        WriteKeyValue('ClearColNo', 1);
        WriteKeyValue('CombineFileNo', 0);

        ShrinkDatabase(FDatabaseName);

        TThread.Synchronize(nil, procedure
        begin
          frmProject1.BuildDataTableGridColumn(1);
          frmProject1.BuildClearColumnGridColumn;
        end);
        OpenClearColumn;
        OpenFileList;

        StopTime;
      except
        on e: Exception do
        begin
          e.Message := '设置列数失败：' + e.Message;
          FLog.Log(e.Message);
        end;
      end;
    finally
      fdqFileList.EnableControls;
      StopTime;
      SetWorkState(wsNone);
    end;
  end).Start;
end;

procedure TdmProject1.InputData;
var
  FilePath: string;
begin
  if not SelectDirectory('导入数据文件夹', '', FilePath) then Exit;

  TTask.Create(procedure
  var
    i, j, RowIndex, StartIndex, EndIndex, FileNo: Integer;
    s, FileName, sFileNo, sColNo: string;
    ColNo: Int64;
    r: TIntegerDynArray;

    procedure CheckStop;
    begin
      TThread.Synchronize(nil, procedure
      begin
        if frmProject1.btnInputData.Tag = 0 then Abort;
      end);
    end;

    procedure AddRow(IsLast: Boolean);
    var
      i, ValueCount, ValueCount2: Integer;
      f: TField;
    begin
      ValueCount := 0;
      ValueCount2 := 0;
      FDQuery.Append;
      for i in r do
      begin
        if i > FRangeColNo then Inc(ValueCount2)
        else Inc(ValueCount);
        f := FDQuery.FieldByName('Field' + Ceil(i / 64).ToString);
        f.AsLargeInt := f.AsLargeInt or (i64 shl (64 - i + (Ceil(i / 64) - 1) * 64));
      end;
      FDQuery.FieldByName('ValueCount').AsInteger := ValueCount;
      FDQuery.FieldByName('ValueCount2').AsInteger := ValueCount2;
      FDQuery.Post;

     if IsLast or (FDQuery.RecordCount = 10000) then
      begin
        FDQuery.ApplyUpdates;
        FDQuery.Close;
        FDQuery.Open;
      end;
      CheckStop;
    end;
  begin
    SetWorkState(wsInputData);
    StartTime;
    try
      try
        for FileName in TDirectory.GetFiles(FilePath, '*', TSearchOption.soAllDirectories) do
        begin
          CheckStop;
          if not ((LowerCase(ExtractFileExt(FileName)) = '.txt')
            or (LowerCase(ExtractFileExt(FileName)) = '.xls')
            or (LowerCase(ExtractFileExt(FileName)) = '.xlsx'))
          then Continue;
          sFileNo := ChangeFileExt(ExtractFileName(FileName), '');
          StartIndex := sFileNo.IndexOf('(');
          EndIndex := sFileNo.IndexOf(')');
          if (StartIndex > 0) and (EndIndex > 0) and (StartIndex < EndIndex) then
            sFileNo := sFileNo.Substring(StartIndex + 1, EndIndex - StartIndex - 1);
          if not TryStrToInt(sFileNo, FileNo) then
          begin
            FLog.Log('导入失败，文件名格式不对');
            Continue;
          end;
          FLog.Log(Format('正在导入数据：%s，请稍等...', [ExtractFileName(FileName)]));
          //创建表
          BuildDataTable(FileNo);

          FDQuery.Open(Format('SELECT TOP 0 * FROM [%d]', [FileNo]));

          if LowerCase(ExtractFileExt(FileName)) = '.txt' then
          begin
            l.LoadFromFile(FileName);
            for RowIndex := l.Count - 1 downto 0 do
              if l[RowIndex].Trim.IsEmpty then l.Delete(RowIndex);
            for RowIndex := 0 to l.Count - 1 do
            begin
              s := l[RowIndex].Trim;
              s := s.Substring(s.IndexOf('=') + 1);
              StrToIntArray(s, r, FRangeColNo);

              AddRow(RowIndex >= l.Count - 1);
            end;
          end
          else
          begin
            {XLS.Filename := FileName;
            XLS.Read;
            for RowIndex := 1 to XLS[0].LastRow do
            begin
              SetLength(r, 0);
              for ColNo := 1 to FColCount do
              begin
                sColNo := XLS[0].AsString[ColNo, RowIndex].Trim;
                if sColNo = '' then Continue;

                SetLength(r, Length(r) + 1);
                r[Length(r) - 1] := ColNo;
              end;

              AddRow(RowIndex >= XLS[0].LastRow);
            end;  }
          end;
          FLog.Log(Format('导入数据：%s 完成', [ExtractFileName(FileName)]));
        end;
        OpenFileList;
        InitDataTable;
        StopTime;
        FLog.Log('导入数据完成');
        ShowMessage('导入数据完成');
      except
        on e: Exception do
        begin
          e.Message := '导入数据[%s]失败：' + e.Message;
          FLog.Log(e.Message);
          if e.Message.IndexOf('aborted') = -1 then ShowMessage(e.Message);
        end;
      end;
    finally
      StopTime;
      SetWorkState(wsNone);
      FDQuery.Close;
    end;
  end).Start;
end;

procedure TdmProject1.InputResultData;
var
  FilePath: string;
begin
  if not SelectDirectory('导入数据文件夹', '', FilePath) then Exit;

  TTask.Create(procedure
  var
    i, j, RowIndex, StartIndex, EndIndex, FolderNo, FileNo, RowNo: Integer;
    s, SubFilePath, FileName, sFileNo, sColNo, sField: string;
    ColNo: Int64;
    r: TIntegerDynArray;

    procedure CheckStop;
    begin
      TThread.Synchronize(nil, procedure
      begin
        if frmProject1.btnInputResultData.Tag = 0 then Abort;
      end);
    end;

    procedure AddRow(IsLast: Boolean);
    var
      i, ValueCount, ValueCount2: Integer;
      f: TField;
    begin
      ValueCount := 0;
      ValueCount2 := 0;
      FDQuery.Append;
      for i in r do
      begin
        if i > FRangeColNo then Inc(ValueCount2)
        else Inc(ValueCount);
        f := FDQuery.FieldByName('Field' + Ceil(i / 64).ToString);
        f.AsLargeInt := f.AsLargeInt or (i64 shl (64 - i + (Ceil(i / 64) - 1) * 64));
      end;
      FDQuery.FieldByName('FolderNo').AsInteger := FolderNo;
      FDQuery.FieldByName('FileNo').AsInteger := FileNo;
      FDQuery.FieldByName('RowNo').AsInteger := RowNo;
      FDQuery.FieldByName('ValueCount').AsInteger := ValueCount;
      FDQuery.FieldByName('ValueCount2').AsInteger := ValueCount2;
      FDQuery.FieldByName('ConformColCount').AsInteger := 0;
      FDQuery.FieldByName('ConformColCount2').AsInteger := 0;
      FDQuery.Post;

      if IsLast or (FDQuery.RecordCount = 10000) then
      begin
        FDQuery.ApplyUpdates;
        FDQuery.Close;
        FDQuery.Open;
      end;
      CheckStop;
    end;
  begin
    SetWorkState(wsInputResultData);
    StartTime;
    try
      try
        //初始化
        for i := 1 to Ceil(FColCount / 64) do sField := sField + Format('[Field%d] [bigint] NOT NULL,', [i]) + #$D#$A;
        s := 'IF Object_Id(N''1'', N''U'') IS NOT NULL DROP TABLE [1]' + #$D#$A
          + 'CREATE TABLE [1] (' + #$D#$A
          + '[Id] [bigint] IDENTITY(1,1) PRIMARY KEY NOT NULL,' + #$D#$A
          + sField
          + '[ValueCount] [smallint] NOT NULL,' + #$D#$A
          + '[ValueCount2] [smallint] NOT NULL,' + #$D#$A
          + '[FolderNo] [smallint] NOT NULL,' + #$D#$A
          + '[FileNo] [smallint] NOT NULL,' + #$D#$A
          + '[RowNo] [smallint] NOT NULL,' + #$D#$A
          + '[ConformColCount] [smallint] NOT NULL,' + #$D#$A
          + '[ConformColCount2] [smallint] NOT NULL' + #$D#$A
          + ')';
        FDConnection.ExecSQL(s);
        //获取子目录
        for SubFilePath in TDirectory.GetDirectories(FilePath) do
        begin
          CheckStop;
          s := ExtractFileName(SubFilePath);
          if not TryStrToInt(s.Substring(0, s.IndexOf('.')), FolderNo) then Continue;
          //获取子目录文件
          for FileName in TDirectory.GetFiles(SubFilePath, '*', TSearchOption.soAllDirectories) do
          begin
            CheckStop;

            if not ((LowerCase(ExtractFileExt(FileName)) = '.txt')
              or (LowerCase(ExtractFileExt(FileName)) = '.xls')
              or (LowerCase(ExtractFileExt(FileName)) = '.xlsx'))
            then Continue;
            s := ExtractFileName(FileName);
            if not TryStrToInt(s.Substring(0, s.IndexOf('.')), FileNo) then Continue;

            FLog.Log(Format('正在导入数据：%s，请稍等...', [ExtractFileName(FileName)]));

            FDQuery.Open('SELECT TOP 0 * FROM [1]');

            if LowerCase(ExtractFileExt(FileName)) = '.txt' then
              l.LoadFromFile(FileName)
            else
            begin
              XLS.Filename := FileName;
              XLS.Read;

              l.Clear;
              for i := 0 to XLS[0].LastRow do l.Add(XLS[0].AsString[0, i]);
            end;

            for RowIndex := l.Count - 1 downto 0 do
              if l[RowIndex].Trim.IsEmpty then l.Delete(RowIndex);
            for RowIndex := 0 to l.Count - 1 do
            begin
              s := l[RowIndex].Trim;
              if not TryStrToInt(s.Substring(0, s.IndexOf('=')), RowNo) then Continue;
              s := s.Substring(s.IndexOf('=') + 1);
              StrToIntArray(s, r, FRangeColNo);

              AddRow(RowIndex >= l.Count - 1);
            end;

            FLog.Log(Format('导入数据：%s 完成', [ExtractFileName(FileName)]));
          end;
        end;
        OpenFileList;
        InitDataTable;
        StopTime;
        FLog.Log('导入数据完成');
        ShowMessage('导入数据完成');
      except
        on e: Exception do
        begin
          e.Message := '导入数据[%s]失败：' + e.Message;
          FLog.Log(e.Message);
          if e.Message.IndexOf('aborted') = -1 then ShowMessage(e.Message);
        end;
      end;
    finally
      StopTime;
      SetWorkState(wsNone);
      FDQuery.Close;
    end;
  end).Start;
end;

procedure TdmProject1.InputClearColumn;
var
  s, sColNo, Cell, FieldName: string;
  i, FileNo, ColNo: Integer;
  r: TIntegerDynArray;
  f: TField;
begin
  frmProject1.OpenDialog.Title := '选择清列行';
  frmProject1.OpenDialog.Filter := '*|*.txt;*.xls;*.xlsx';
  frmProject1.OpenDialog.Options := frmProject1.OpenDialog.Options - [ofAllowMultiSelect];
  if not frmProject1.OpenDialog.Execute then Exit;

  if LowerCase(ExtractFileExt(frmProject1.OpenDialog.FileName)) = '.txt' then
    l.LoadFromFile(frmProject1.OpenDialog.FileName)
  else
  begin
    XLS.Filename := frmProject1.OpenDialog.FileName;
    XLS.Read;
    l.Clear;
    for i := 0 to XLS[0].LastRow do l.Add(XLS[0].AsString[0, i]);
  end;

  FDConnection.ExecSQL('TRUNCATE TABLE ClearColumn');

  for i := 0 to l.Count - 1 do
  begin
    if not TryStrToInt(l.Names[i], FileNo) then Continue;
    s := l.ValueFromIndex[i].Trim;
    StrToIntArray(s, r, FRangeColNo);

    fdqClearColumn.Append;
    fdqClearColumn.FieldByName('FileNo').AsInteger := FileNo;
    for ColNo in r do
    begin
      f := fdqClearColumn.FieldByName('Field' + Ceil(ColNo / 64).ToString);
      f.AsLargeInt := f.AsLargeInt or (i64 shl (64 - ColNo + (Ceil(ColNo / 64) - 1) * 64));
    end;
    fdqClearColumn.Post;
  end;
  OpenClearColumn;
end;

procedure TdmProject1.SetDeleteInvalidRow;
begin
  FValidColCount := ReadKeyValue('ValidColCount', 1).ToInteger;
  if not Input('', '删除(第一段)各个文本“重复行”;' + #$D#$A + '输入行列数<', FValidColCount) then Exit;
  WriteKeyValue('ValidColCount', FValidColCount);
  if FColCount > FRangeColNo then
  begin
    FValidColCount2 := ReadKeyValue('ValidColCount2', 1).ToInteger;
    if not Input('', '删除(第二段)各个文本“重复行”;' + #$D#$A + '输入行列数<', FValidColCount2) then Exit;
    WriteKeyValue('ValidColCount2', FValidColCount2);
  end;
end;

procedure TdmProject1.ClearColumn;
begin
  TTask.Create(procedure
  var
    s, FieldName, sField, ValueCountFieldName: string;
    i, ClearColumnType, FileNo, ColNo: Integer;
    FieldValue: Int64;
    IsLocate: Boolean;

    procedure CheckStop;
    begin
      TThread.Synchronize(nil, procedure
      begin
        if frmProject1.btnClearColumn.Tag = 0 then Abort;
      end);
    end;
  begin
    ClearColumnType := ReadKeyValue('ClearColumnType', 0).ToInteger;
    sField := GetFieldString;
    SetWorkState(wsClearColumn);
    StartTime;
    try
      try
        repeat
          if ClearColumnType = 0 then
          begin
            TThread.Synchronize(nil, procedure
            begin
              FileNo := ReadKeyValue('ClearColumnFileNo', '0').ToInteger;
              ColNo := ReadKeyValue('ClearColNo', '1').ToInteger;
              fdqFileList.First;
              fdqFileList.Locate('FileNo', FileNo, []);
            end);
            while not fdqFileList.Eof do
            begin
              CheckStop;
              TThread.Synchronize(nil, procedure
              begin
                FileNo := fdqFileList.FieldByName('FileNo').AsInteger;
                WriteKeyValue('ClearColumnFileNo', FileNo);
                IsLocate := fdqClearColumn.Locate('FileNo', FileNo, []);
              end);
              if IsLocate then
              begin
                for i := ColNo to FColCount do
                begin
                  CheckStop;
                  FieldName := 'Field' + Ceil(i / 64).ToString;
                  FieldValue := fdqClearColumn.FieldByName(FieldName).AsLargeInt;
                  if FieldValue = i64 shl (64 - i + (Ceil(i / 64) - 1) * 64) or FieldValue then
                  begin
                    TThread.Synchronize(nil, procedure
                    begin
                      frmProject1.dbgrdClearColumn.SelectedIndex := i - 1;
                    end);

                    FLog.Log(Format('正在清空文件：%d 列：%d，请稍等...', [FileNo, i]));

                    ColNo := i;
                    ValueCountFieldName := 'ValueCount';
                    if ColNo > FRangeColNo then ValueCountFieldName := 'ValueCount2';
                    s := 'DECLARE @iColNo BIGINT' + #$D#$A
                      + 'SET @iColNo = %d' + #$D#$A
                      + 'BEGIN TRAN' + #$D#$A
                      + 'UPDATE [%d] SET %s = %s ^ @iColNo, %s = %s - 1'+ #$D#$A
                      + 'WHERE %s = %s | @iColNo'+ #$D#$A
                      + 'COMMIT TRAN';
                    s := Format(s, [i64 shl (64 - ColNo + (Ceil(ColNo / 64) - 1) * 64), FileNo, FieldName, FieldName,
                      ValueCountFieldName, ValueCountFieldName, FieldName, FieldName]);
                    FDConnection.ExecSQL(s);

                    WriteKeyValue('ClearColNo', i + 1);
                    FLog.Log(Format('清空文件：%d 列：%d 完成', [FileNo, i]));
                    //Sleep(500);
                    //Exit;
                  end;
                end;
                ColNo := 1;
                WriteKeyValue('ClearColNo', ColNo);
              end;
              //删除无效行
              s := 'BEGIN TRAN' + #$D#$A
                + Format('DELETE FROM [%d] WHERE ValueCount < %d' + #$D#$A, [FileNo, FValidColCount]);
              if FColCount > FRangeColNo then
                 s := s + Format('OR ValueCount2 < %d' + #$D#$A, [FValidColCount2]);
              s := s + 'COMMIT TRAN';
              FDConnection.ExecSQL(s);

              TThread.Synchronize(nil, procedure
              begin
                fdqFileList.Next;
              end);
            end;
            FileNo := 0;
            WriteKeyValue('ClearColumnFileNo', FileNo);

            fdqFileList.First;
            if fdqFileList.FieldByName('FileNo').AsInteger = 1 then
            begin
              ClearColumnType := 3;
              WriteKeyValue('ClearColumnType', ClearColumnType);
            end
            else
            begin
              ClearColumnType := 1;
              WriteKeyValue('ClearColumnType', ClearColumnType);
            end;
          end;

          if ClearColumnType = 1 then
          begin
            TThread.Synchronize(nil, procedure
            begin
              FileNo := ReadKeyValue('CombineFileNo', '0').ToInteger;
              fdqFileList.First;
              fdqFileList.Locate('FileNo', FileNo, []);
            end);
            while not fdqFileList.Eof do
            begin
              CheckStop;
              if fdqFileList.RecNo = fdqFileList.RecordCount then Break;
              FileNo := fdqFileList.FieldByName('FileNo').AsInteger;
              FLog.Log(Format('正在合并文件：%d，请稍等...', [FileNo]));

              s := 'WITH CTE AS (' + #$D#$A
                + 'SELECT *, ROW_NUMBER() OVER(PARTITION BY %s ORDER BY FileNo2) RowNo' + #$D#$A
                + 'FROM V_DataTable' + #$D#$A
                + 'WHERE FileNo2 >= %d' + #$D#$A
                + ')' + #$D#$A
                + 'INSERT [%d] SELECT %s, ValueCount, ValueCount2' + #$D#$A
                + 'FROM CTE WHERE FileNo2 > %d AND RowNo = 1';
              s := Format(s, [sField, FileNo, FileNo, sField, FileNo]);
              FDConnection.ExecSQL(s);

              WriteKeyValue('CombineFileNo', FileNo + 1);
              FLog.Log(Format('合并文件[%d]完成', [FileNo]));

              TThread.Synchronize(nil, procedure
              begin
                fdqFileList.Next;
              end);
            end;
            FileNo := 0;
            WriteKeyValue('CombineFileNo', FileNo);
            ClearColumnType := 2;
            WriteKeyValue('ClearColumnType', ClearColumnType);

            FLog.Log('合并文件完成');
          end;

          if ClearColumnType = 2 then
          begin
            CheckStop;

            FLog.Log('正在重命名，请稍等...');
            fdqFileList.DisableControls;
            try
              s := '';
              fdqFileList.First;
              while not fdqFileList.Eof do
              begin
                FileNo := fdqFileList.FieldByName('FileNo').AsInteger;
                s := s + Format('EXEC sp_rename ''%d'', ''%d''' + #$D#$A, [FileNo, FileNo - 1]) ;
                                  ;
                fdqFileList.Next;
              end;

              FDConnection.ExecSQL(s);
              FLog.Log('重命名完成');

              ClearColumnType := 0;
              WriteKeyValue('ClearColumnType', ClearColumnType);

              OpenFileList;
              InitDataTable;
            finally
              fdqFileList.First;
              fdqFileList.EnableControls;
            end;
          end;

          if ClearColumnType = 3 then
          begin
            CheckStop;
            //Exit;
            Flog.Log('正在合并数据，请稍等...');
            s := 'WITH CTE AS (' + #$D#$A
              + 'SELECT *, ROW_NUMBER() OVER(PARTITION BY %s ORDER BY FileNo2) RowNo' + #$D#$A
              + 'FROM V_DataTable' + #$D#$A
              + ')' + #$D#$A
              + 'INSERT [1] SELECT %s, ValueCount, ValueCount2' + #$D#$A
              + 'FROM CTE WHERE FileNo2 > 1 AND RowNo = 1';
            s := Format(s, [sField, sField]);
            FDConnection.ExecSQL(s);

            fdqFileList.DisableControls;
            try
              s := '';
              fdqFileList.First;
              while not fdqFileList.Eof do
              begin
                FileNo := fdqFileList.FieldByName('FileNo').AsInteger;
                if FileNo > 1 then
                  s := s + Format('IF Object_Id(N''%d'', N''U'') IS NOT NULL DROP TABLE [%d]' + #$D#$A, [FileNo, FileNo]);

                fdqFileList.Next;
              end;
              s := s + 'ALTER TABLE [1] ADD FolderNo SMALLINT DEFAULT 0' + #$D#$A
                + 'ALTER TABLE [1] ADD FileNo SMALLINT DEFAULT 0' + #$D#$A
                + 'ALTER TABLE [1] ADD RowNo SMALLINT DEFAULT 0' + #$D#$A
                + 'ALTER TABLE [1] ADD ConformColCount SMALLINT DEFAULT 0' + #$D#$A
                + 'ALTER TABLE [1] ADD ConformColCount2 SMALLINT DEFAULT 0';
              FDConnection.ExecSQL(s);

              ClearColumnType := -1;
              WriteKeyValue('ClearColumnType', ClearColumnType);

              OpenFileList;
              InitDataTable;

              Flog.Log('合并数据完成');
            finally
              fdqFileList.First;
              fdqFileList.EnableControls;
            end;
          end;
        until fdqFileList.Eof or (ClearColumnType = -1);;

        ShrinkDatabase(FDatabaseName, True);

        StopTime;
        frmMain.BringToFront;
        frmMain.PlayMusic;
        Flog.Log('执行"清列合并"完成');
        ShowMessage('执行"清列合并"完成');
      except
        on e: Exception do
        begin
          e.Message := '执行"清列合并"失败：' + e.Message;
          Flog.Log(e.Message);
          if e.Message.IndexOf('aborted') = -1 then ShowMessage(e.Message);
        end;
      end;
    finally
      StopTime;
      SetWorkState(wsNone);
    end;
  end).Start;
end;

procedure TdmProject1.SortRow;
begin
  TTask.Create(procedure
  var
    s, sField, sOrderBy: string;
    i: Integer;
  begin
    StartTime;
    try
      try
        SetWorkState(wsExecuting);

        for i := 1 to Ceil(FColCount / 64) do sField := sField + Format('[Field%d] [bigint] NOT NULL,', [i]) + #$D#$A;
        s := 'IF Object_Id(N''ResultData'', N''U'') IS NOT NULL DROP TABLE ResultData' + #$D#$A
          + 'CREATE TABLE [ResultData] (' + #$D#$A
          + '[Id] [bigint] IDENTITY(1,1) PRIMARY KEY NOT NULL,' + #$D#$A
          + sField
          + '[ValueCount] [smallint] NOT NULL,' + #$D#$A
          + '[ValueCount2] [smallint] NOT NULL,' + #$D#$A
          + '[FolderNo] [smallint] NOT NULL,' + #$D#$A
          + '[FileNo] [smallint] NOT NULL,' + #$D#$A
          + '[RowNo] [smallint] NOT NULL,' + #$D#$A
          + '[ConformColCount] [smallint] NOT NULL,' + #$D#$A
          + '[ConformColCount2] [smallint] NOT NULL' + #$D#$A
          + ')';
        FDConnection.ExecSQL(s);

        sField := GetFieldString;
        for i := 1 to Ceil(FColCount / 64) do
        begin
          if sOrderBy.IsEmpty then sOrderBy := Format('dbo.BigIntToBinStr(Field%d)', [i])
          else sOrderBy := sOrderBy + ' + ' + Format('dbo.BigIntToBinStr(Field%d)', [i]);
        end;
        sOrderBy := sOrderBy + ' DESC';
        if FColCount > FRangeColNo then sOrderBy := 'ORDER BY ValueCount, ValueCount2, ' + sOrderBy
        else sOrderBy := 'ORDER BY ValueCount DESC, ' + sOrderBy;

        Flog.Log('正在排序，请稍等...');

        s := 'INSERT INTO ResultData(%s, ValueCount, ValueCount2,' + #$D#$A
          + 'FolderNo, FileNo, RowNo, ConformColCount, ConformColCount2)' + #$D#$A
          + 'SELECT %s, ValueCount, ValueCount2, 0, 0, 0, 0, 0' + #$D#$A
          + 'FROM [1]' + #$D#$A
          + sOrderBy + #$D#$A
          + 'DROP TABLE [1]' + #$D#$A
          + 'EXEC sp_rename ''ResultData'', ''1''';
        s := Format(s, [sField, sField, sOrderBy]);
        FDConnection.ExecSQL(s);

        OpenFileList;
        InitDataTable;

        ShrinkDatabase(FDatabaseName);
        StopTime;

        Flog.Log('排序完成');
        ShowMessage('排序完成');
      except
        on e: Exception do
        begin
          e.Message := '排序失败：' + e.Message;
          Flog.Log(e.Message);
          ShowMessage(e.Message);
        end;
      end;
    finally
      StopTime;
      SetWorkState(wsNone);
    end;
  end).Start;
end;

procedure TdmProject1.SetRowcount;
var
  EachFileRowCount: Integer;
begin
  EachFileRowCount := ReadKeyValue('EachFileRowCount', 1000).ToInteger;
  if not Input('', '输入平均多少行/文本', EachFileRowCount) then Exit;
  if EachFileRowCount < 1 then raise Exception.Create('请输入有效数值');
  WriteKeyValue('EachFileRowCount', EachFileRowCount);

  TTask.Create(procedure
  var
    s, sField: string;
    i: Integer;
  begin
    StartTime;
    try
      try
        SetWorkState(wsExecuting);
        sField := GetFieldString;

        Flog.Log('正在排序，请稍等...');
        if FColCount > FRangeColNo then
        begin
          s := 'WITH CTE AS(' + #$D#$A
            + 'SELECT Id, ROW_NUMBER() OVER(PARTITION BY ValueCount, ValueCount2 ORDER BY (SELECT 0)) RowNo' + #$D#$A
            + 'FROM [1]' + #$D#$A
            + '),' + #$D#$A
            + 'CTE2 AS(' + #$D#$A
            + 'SELECT ValueCount, ValueCount2, ROW_NUMBER() OVER(ORDER BY ValueCount + ValueCount2, ValueCount, ValueCount2) FolderNo' + #$D#$A
            + 'FROM [1] GROUP BY ValueCount, ValueCount2' + #$D#$A
            + ')' + #$D#$A
            + 'UPDATE r SET r.FolderNo = CTE2.FolderNo,' + #$D#$A
            + 'r.FileNo = Ceiling(CTE.RowNo / %d.0),' + #$D#$A
            + 'r.RowNo = CASE CTE.RowNo %% %d WHEN 0 THEN %d ElSE CTE.RowNo %% %d END' + #$D#$A
            + 'FROM [1] r' + #$D#$A
            + 'LEFT JOIN CTE ON CTE.Id = r.Id' + #$D#$A
            + 'LEFT JOIN CTE2 ON CTE2.ValueCount = r.ValueCount AND CTE2.ValueCount2 = r.ValueCount2';
          s := Format(s, [EachFileRowCount, EachFileRowCount, EachFileRowCount, EachFileRowCount]);
        end
        else
        begin
          s := 'WITH CTE AS(' + #$D#$A
            + 'SELECT Id, ROW_NUMBER() OVER(PARTITION BY ValueCount ORDER BY (SELECT 0)) RowNo' + #$D#$A
            + 'FROM [1]' + #$D#$A
            + '),' + #$D#$A
            + 'CTE2 AS(' + #$D#$A
            + 'SELECT ValueCount, ROW_NUMBER() OVER(ORDER BY ValueCount) FolderNo' + #$D#$A
            + 'FROM [1] GROUP BY ValueCount' + #$D#$A
            + ')' + #$D#$A
            + 'UPDATE r SET r.FolderNo = CTE2.FolderNo,' + #$D#$A
            + 'r.FileNo = Ceiling(CTE.RowNo / %d.0),' + #$D#$A
            + 'r.RowNo = CASE CTE.RowNo %% %d WHEN 0 THEN %d ElSE CTE.RowNo %% %d END' + #$D#$A
            + 'FROM [1] r' + #$D#$A
            + 'LEFT JOIN CTE ON CTE.Id = r.Id' + #$D#$A
            + 'LEFT JOIN CTE2 ON CTE2.ValueCount = r.ValueCount';
          s := Format(s, [EachFileRowCount, EachFileRowCount, EachFileRowCount, EachFileRowCount]);
        end;
        FDConnection.ExecSQL(s);

        ShrinkDatabase(FDatabaseName);
        StopTime;

        Flog.Log('设置行数完成');
        ShowMessage('设置行数完成');
      except
        on e: Exception do
        begin
          e.Message := '设置行数失败：' + e.Message;
          Flog.Log(e.Message);
          ShowMessage(e.Message);
        end;
      end;
    finally
      StopTime;
      SetWorkState(wsNone);
    end;
  end).Start;
end;


procedure TdmProject1.ExportToFile;
var
  sField, ExportFilePath, sFieldValue: string;
  ExportType, EachFileRowCount: Integer;
begin
  if not SelectDirectory('选择导出目录', '', ExportFilePath) then Exit;
  if Copy(ExportFilePath, Length(ExportFilePath) - 1, 1) <> '\' then ExportFilePath := ExportFilePath + '\';
  ExportType := SingleChoice('', 'TXT' + #$D#$A + 'Excel');
  if ExportType = -1 then Exit;
  WriteKeyValue('ExportFilePath', ExportFilePath);

  sField := GetFieldString;
  EachFileRowCount := ReadKeyValue('EachFileRowCount', 0).ToInteger;

  TTask.Create(procedure
  var
    s, FilePath, FileName: string;
    i, ValueIndex, ValueCount, ValueCount2, FileSerialNo: Integer;
    FieldValue: Int64;
    ValueArr: array of string;

    procedure CheckStop;
    begin
      TThread.Synchronize(nil, procedure
      begin
        if frmProject1.btnExportToFile.Tag = 0 then Abort;
      end);
    end;

    function BuildFileName: string;
    var
      i, iEnd: Integer;
      sValue: string;
    begin
      if FColCount > FRangeColNo then
      begin
        iEnd := 2;
        if ValueCount < 4 then iEnd := Ceil(ValueCount / 2);
        for i := 1 to iEnd do
        begin
          if i > 1 then sValue := sValue + '、';
          sValue := sValue + ValueArr[i - 1];
        end;
        if ValueCount < 4 then iEnd := ValueCount - iEnd;
        for i := iEnd downto 1 do
          sValue := sValue + '、' + ValueArr[ValueCount - i];
        sValue := '（' + sValue + '）（';

        iEnd := 2;
        if ValueCount2 < 4 then iEnd := ValueCount + Ceil(ValueCount2 / 2);
        for i := 1 to iEnd do
        begin
          if i > 1 then sValue := sValue + '、';
          sValue := sValue + ValueArr[ValueCount + i - 1];
        end;
        if ValueCount2 < 4 then iEnd := ValueCount + ValueCount2 - iEnd;
        for i := iEnd downto 1 do
          sValue := sValue + '、' + ValueArr[Length(ValueArr) - i];
        sValue := sValue + '）';
      end
      else
      begin
        iEnd := 5;
        if ValueCount < 10 then  iEnd := Ceil(ValueCount / 2);
        for i := 1 to iEnd do
        begin
          if i > 1 then sValue := sValue + '、';
          sValue := sValue + ValueArr[i - 1];
        end;
        sValue := '（' + sValue + '）（';

        if ValueCount < 10 then iEnd := ValueCount - iEnd;
        for i := iEnd downto 1 do
        begin
          if i < iEnd then sValue := sValue + '、';
          sValue := sValue + ValueArr[Length(ValueArr) - i];
        end;
        sValue := sValue + '）';
      end;
      Result := Format('%d.%s、%d行', [FileSerialNo, sValue, FDQuery.RecordCount]);
    end;

    procedure ExportToExcel;
    var
      i, ColNo: Integer;
    begin
      XLS[0].Clear;
      XLS[0].Rows[0].Height := 440;
      for i := 1 to FColCount do
      begin
        ColNo := i;
        if ColNo > FRangeColNo then ColNo := ColNo - FRangeColNo;
        s := ColNo.ToString;
        if i < 10 then s := '0' + s;
        XLS[0].AsString[i - 1, 0] := s;
        XLS[0].Columns[i - 1].Width := 1000;
      end;

      FDQuery.First;
      while not FDQuery.Eof do
      begin
        CheckStop;
        XLS[0].Rows[FDQuery.RecNo].Height := 440;
        ValueIndex := 0;
        for i := 1 to FColCount do
        begin
          s := '';
          FieldValue := FDQuery.FieldByName('Field' + IntToStr(Ceil(i / 64))).AsLargeInt;
          if FieldValue = i64 shl (64 - i + (Ceil(i / 64) - 1) * 64) or FieldValue then
          begin
            ColNo := i;
            if ColNo > FRangeColNo then ColNo := ColNo - FRangeColNo;
            s := ColNo.ToString;
            if ColNo < 10 then s := '0' + s;

            ValueArr[ValueIndex] := s;
            Inc(ValueIndex);
          end;
          XLS[0].AsString[i - 1, FDQuery.RecNo] := s;
        end;

        if FDQuery.RecNo = 1 then FileName := FilePath + BuildFileName + '.xlsx';

        FDQuery.Next;
      end;

      XLS.Calculate;
      XLS[0].CalcDimensions;
      XLS.SaveToFile(FileName);
    end;

    procedure ExportToTXT;
    var
      i, ColNo: Integer;
      sValue: string;
    begin
      l.Clear;
      FDQuery.First;
      while not FDQuery.Eof do
      begin
        CheckStop;
        ValueIndex := 0;
        sValue := '';
        for i := 1 to FColCount do
        begin
          s := '';
          FieldValue := FDQuery.FieldByName('Field' + IntToStr(Ceil(i / 64))).AsLargeInt;
          if FieldValue = i64 shl (64 - i + (Ceil(i / 64) - 1) * 64) or FieldValue then
          begin
            ColNo := i;
            if ColNo > FRangeColNo then ColNo := ColNo - FRangeColNo;
            s := ColNo.ToString;
            if ColNo < 10 then s := '0' + s;

            if sValue <> '' then
            begin
              if (i <> ColNo) and (sValue.IndexOf('-') = -1) then sValue := sValue + ' - '
              else sValue := sValue + '、';
            end;
            sValue := sValue + s;
            ValueArr[ValueIndex] := s;

            Inc(ValueIndex);
          end;
        end;
        l.Values[FDQuery.RecNo.ToString] := sValue;

        if FDQuery.RecNo = 1 then FileName := FilePath + BuildFileName + '.txt';

        FDQuery.Next;
      end;
      l.SaveToFile(FileName);
    end;
  begin
    SetWorkState(wsExportToFile);
    StartTime;
    try
      try
        FLog.Log('正在获取导出文件列表，请稍等...');
        //if FColCount > FRangeColNo then
        begin
          s := 'WITH CTE AS (' + #$D#$A
            + 'SELECT ValueCount, ValueCount2, FolderNo, FileNo' + #$D#$A
            + 'FROM [1]' + #$D#$A
            + 'GROUP BY ValueCount, ValueCount2, FolderNo, FileNo' + #$D#$A
            + '),' + #$D#$A

            + 'CTE2 AS (' + #$D#$A
            + 'SELECT ValueCount, ValueCount2, Count(ValueCount) ValueRowCount' + #$D#$A
            + 'FROM [1] GROUP BY ValueCount, ValueCount2' + #$D#$A
            + '),' + #$D#$A

            + 'CTE3 AS (' + #$D#$A
            + 'SELECT ValueCount, ValueCount2, %s FROM [1]' + #$D#$A
            + 'WHERE FileNo = 1 AND RowNo = 1' + #$D#$A
            + ')' + #$D#$A

            + 'SELECT CTE.FolderNo, CTE.FileNo, CTE2.ValueRowCount, CTE2.ValueCount2, CTE3.*' + #$D#$A
            + 'FROM CTE' + #$D#$A
            + 'LEFT JOIN CTE2 ON CTE2.ValueCount = CTE.ValueCount AND CTE2.ValueCount2 = CTE.ValueCount2' + #$D#$A
            + 'LEFT JOIN CTE3 ON CTE3.ValueCount = CTE.ValueCount AND CTE3.ValueCount2 = CTE.ValueCount2' + #$D#$A
            + 'ORDER BY CTE.FolderNo, CTE.FileNo';
          s := Format(s, [sField]);
        end;
        FDQuery.Open(s);
        with FDMemTable do
        begin
          FDMemTable.Close;
          Data := FDQuery.Data;
          First;
          while not Eof do
          begin
            CheckStop;
            ValueCount := FieldByName('ValueCount').AsInteger;
            ValueCount2 := FieldByName('ValueCount2').AsInteger;
            FileSerialNo := FieldByName('FileNo').AsInteger;
            FLog.Log(Format('正在导出列[%d]文件[%d]，请稍等...', [ValueCount, FileSerialNo]));
            //生成子目录名

            if FColCount > FRangeColNo then
            begin
              FilePath := ExportFilePath + '%d.列数（1-%d）%d列+（1-%d）%d列= %d列、%d行\';
              FilePath := Format(FilePath, [
                FieldByName('FolderNo').AsInteger,
                FRangeColNo,
                ValueCount,
                FColCount - FRangeColNo,
                ValueCount2,
                ValueCount + ValueCount2,
                FieldByName('ValueRowCount').AsInteger
              ]);
            end
            else
            begin
              sFieldValue := '';
              for i := 1 to FColCount do
              begin
                FieldValue := FieldByName('Field' + IntToStr(Ceil(i / 64))).AsLargeInt;
                if FieldValue = i64 shl (64 - i + (Ceil(i / 64) - 1) * 64) or FieldValue then
                begin
                  sFieldValue := i.ToString + '-';
                  if i < 10 then sFieldValue := '0' + sFieldValue;
                  Break;
                end;
              end;
              for i := FColCount downto 1 do
              begin
                FieldValue := FieldByName('Field' + IntToStr(Ceil(i / 64))).AsLargeInt;
                if FieldValue = i64 shl (64 - i + (Ceil(i / 64) - 1) * 64) or FieldValue then
                begin
                  if i < 10 then sFieldValue := sFieldValue + '0' + i.ToString
                  else sFieldValue := sFieldValue + i.ToString;
                  Break;
                end;
              end;
              FilePath := ExportFilePath + '%d.首行（首尾）列数字%s、%d列、%d行\';
              FilePath := Format(FilePath, [
                FieldByName('FolderNo').AsInteger,
                sFieldValue,
                ValueCount,
                FieldByName('ValueRowCount').AsInteger
              ]);
            end;
            if not DirectoryExists(FilePath) then CreateDir(FilePath);
            SetLength(ValueArr, ValueCount + ValueCount2);

            s := 'SELECT %s FROM [1]' + #$D#$A
              + 'WHERE ValueCount = %d AND ValueCount2 = %d AND FileNo = %d';
            s := Format(s, [sField, ValueCount, ValueCount2, FileSerialNo]);
            FDQuery.Open(s);

            case ExportType of
              1: ExportToExcel
              else ExportToTXT;
            end;

            FLog.Log(Format('导出列[%d]文件[%d]完成', [ValueCount, FileSerialNo]));

            Next;
          end;
        end;
        StopTime;

        FLog.Log('导出文件完成');
        ShowMessage('导出文件完成');
      except
        on e: Exception do
        begin
          e.Message := Format('导出文件[%s]失败：', [FileName]) + e.Message;
          FLog.Log(e.Message);
          if e.Message.IndexOf('aborted') = -1 then ShowMessage(e.Message);
        end;
      end;
    finally
      StopTime;
      SetWorkState(wsNone);
    end;
  end).Start;
end;

procedure TdmProject1.BuildClearColumn(AFlag: Integer = 0);
var
  s, sSerialNo, sColNo, FileName, SaveFileName: string;
  i, j, StartIndex, EndIndex, RowNo, ColNo, StartSerialNo, MaxColNo, RangeColNo2: Integer;
  ColArr, ColArr2: TIntDyadicArray;
  r: TIntegerDynArray;
begin
  frmProject1.OpenDialog.Filter := '*|*.txt';
  frmProject1.OpenDialog.Options := frmProject1.OpenDialog.Options - [ofAllowMultiSelect];
  if not frmProject1.OpenDialog.Execute then Exit;

  s := ExtractFileName(frmProject1.OpenDialog.FileName);
  StartIndex := s.IndexOf('(');
  EndIndex := s.IndexOf(')');
  sSerialNo := s.Substring(StartIndex + 1, EndIndex - StartIndex - 1);
  if (StartIndex = -1)
    or (EndIndex = -1)
    or (StartIndex > EndIndex)
    or not TryStrToInt(sSerialNo, StartSerialNo)
  then raise Exception.Create('文件名格式无效');

  FileName := frmProject1.OpenDialog.FileName;
  SaveFileName := ExtractFileName(FileName);
  SaveFileName := ExtractFilePath(FileName) + s.Substring(0, StartIndex + 1) + '%d' + s.Substring(EndIndex, s.Length);

  if not Input('', '设置总列数', MaxColNo) then Exit;
  RangeColNo2 := MaxColNo;
  if not Input('', '输入第一段列数范围', RangeColNo2) then Exit;
  if RangeColNo2 > MaxColNo then raise Exception.Create('请输入有效数值');

  LoadData(FileName, ColArr, ColArr2);
  l.Clear;
  for i := Low(ColArr) to High(ColArr) do
  begin
    s := '';
    for ColNo := 1 to RangeColNo2 do
    begin
      sColNo := ColNo.ToString;
      if sColNo.Length = 1 then sColNo := '0' + sColNo;

      for j := Low(ColArr[i]) to High(ColArr[i]) do
        if ColNo = ColArr[i][j] then
        begin
          sColNo := '';
          Break;
        end;

      if sColNo.IsEmpty then Continue;

      if s.IsEmpty then s := sColNo
      else s := s + '、' + sColNo;
    end;
    if MaxColNo > RangeColNo2 then s := s + '-';
    for ColNo := 1 to MaxColNo - RangeColNo2 do
    begin
      sColNo := ColNo.ToString;
      if sColNo.Length = 1 then sColNo := '0' + sColNo;

      for j := Low(ColArr2[i]) to High(ColArr2[i]) do
        if ColNo = ColArr2[i][j] then
        begin
          sColNo := '';
          Break;
        end;

      if sColNo.IsEmpty then Continue;

      if s.Substring(s.Length - 1) = '-' then s := s + sColNo
      else s := s + '、' + sColNo;
    end;

    l.Values[i.ToString] := s;
  end;

  for i := 0 to l.Count - 1 do
    l[i] := (i + 1).ToString + '=' + l.ValueFromIndex[i].Trim;
  l.SaveToFile(Format(SaveFileName, [StartSerialNo]));
  for i := l.Count - 1 downto 1 do
  begin
    l.Delete(0);
    Inc(StartSerialNo);
    l.SaveToFile(Format(SaveFileName, [StartSerialNo]));
  end;
  ShowMessage('生成清列行完成');
end;

procedure TdmProject1.SortClearColumn;
var
  i, j: Integer;
  FileName, s: string;
begin
  frmProject1.OpenDialog.Title := '选择清列行';
  frmProject1.OpenDialog.Filter := '*|*.txt';
  frmProject1.OpenDialog.Options := frmProject1.OpenDialog.Options + [ofAllowMultiSelect];
  if not frmProject1.OpenDialog.Execute then Exit;
  for i := 0 to frmProject1.OpenDialog.Files.Count - 1 do
  begin
    FileName := frmProject1.OpenDialog.Files[i];
    l.LoadFromFile(FileName);
    for j := l.Count - 1 downto 0 do
      if l[i].Trim.IsEmpty then l.Delete(j);

    for j := 0 to Floor(l.Count / 2) - 1 do
    begin
      s := l[j];
      l[j] := l[l.Count - j - 1];
      l[l.Count - j - 1] := s;
    end;

    l.SaveToFile(FileName);
  end;
end;

procedure TdmProject1.RearrangeClearColumn;
var
  i, j, k, ColNo: Integer;
  FileName, s, sColNo: string;
  ColArr, ColArr2: TIntegerDynArray;
begin
  frmProject1.OpenDialog.Title := '选择清列行';
  frmProject1.OpenDialog.Filter := '*|*.txt';
  frmProject1.OpenDialog.Options := frmProject1.OpenDialog.Options + [ofAllowMultiSelect];
  if not frmProject1.OpenDialog.Execute then Exit;
  for i := 0 to frmProject1.OpenDialog.Files.Count - 1 do
  begin
    FileName := frmProject1.OpenDialog.Files[i];
    l.LoadFromFile(FileName);
    for j := l.Count - 1 downto 0 do
      if l[j].Trim.IsEmpty then l.Delete(j);

    //值排序
    for j := 0 to l.Count - 1 do
    begin
      s := l.ValueFromIndex[j];
      StrToIntArray(s, ColArr, ColArr2);
      ShellSort(ColArr);
      ShellSort(ColArr2);

      s := '';
      for k := Low(ColArr) to High(ColArr) do
      begin
        sColNo := ColArr[k].ToString;
        if sColNo.Length = 1 then sColNo := '0' + sColNo;
        if not s.IsEmpty then s := s + '、';
        s := s + sColNo;
      end;
      if Length(ColArr2) > 0 then
      begin
        s := s + '-';
        for k := Low(ColArr2) to High(ColArr2) do
        begin
          sColNo := ColArr2[k].ToString;
          if sColNo.Length = 1 then sColNo := '0' + sColNo;
          if not s.IsEmpty and (s.IndexOf('-') < s.Length - 1) then s := s + '、';
          s := s + sColNo;
        end;
      end;

      l.ValueFromIndex[j] := s;
    end;
    //插入倒序行号
    for j := 0 to l.Count - 1 do
      l[j] := Format('%s(%d)=%s', [l.Names[j], l.Count - j, l.ValueFromIndex[j].Trim]);
    l.SaveToFile(FileName.Replace('.txt', '副本.txt'));
    //设置行号
    for j := 0 to l.Count - 1 do
      l[j] := (j + 1).ToString + '=' + l.ValueFromIndex[j].Trim;
    l.SaveToFile(FileName);
  end;
end;

procedure TdmProject1.CompareClearColumn;
var
  FileName: string;
  ColArr, ColArr2, ColArr3, ColArr4: TIntDyadicArray;
  IsSecondRound: Boolean;

  procedure CompareRow(RowIndex: Integer; Row, Row2: TIntegerDynArray);
  var
    i: Integer;
    s, Key: string;
  begin
    Inc(RowIndex);
    for i := Low(Row) to High(Row) do
    begin
      if (i < Length(Row2)) and (Row[i] = Row2[i]) then Continue;
      s := Row[i].ToString;
      if s.Length = 1 then s := '0' + s;
      Key := RowIndex.ToString;
      if l.Values[Key].IsEmpty then
      begin
        if IsSecondRound then s := '-' + s;
        l.Values[Key] := s;
      end
      else
      begin
        if l.Values[Key].Substring(l.Values[Key].Length - 1) <> '-' then
          l.Values[Key] := l.Values[Key] + '、';
        l.Values[Key] := l.Values[Key] + s;
      end;

      if i < Length(Row2) then
      begin
        s := Row2[i].ToString;
        if s.Length = 1 then s := '0' + s;
      end
      else s := '@';

      Key := RowIndex.ToString + '@';
      if l.Values[Key].IsEmpty then
      begin
        if IsSecondRound then s := '-' + s;
        l.Values[Key] := s;
      end
      else
      begin
        if l.Values[Key].Substring(l.Values[Key].Length - 1) <> '-' then
          l.Values[Key] := l.Values[Key] + '、';
        l.Values[Key] := l.Values[Key] + s;
      end;
    end;
    if not IsSecondRound then
    begin
      if l.IndexOfName(RowIndex.ToString) > -1 then
        l.Values[RowIndex.ToString] := l.Values[RowIndex.ToString] + '-';
      if l.IndexOfName(RowIndex.ToString + '@') > -1 then
        l.Values[RowIndex.ToString + '@'] := l.Values[RowIndex.ToString + '@'] + '-';
    end;
  end;

  procedure CompareData(Arr, Arr2: array of TIntegerDynArray);
  var
    i: Integer;
  begin
    for i := Low(Arr) to High(Arr) do
    begin
      if i < Length(Arr2) then
      begin
        if Length(Arr[i]) >= Length(Arr2[i]) then CompareRow(i, Arr[i], Arr2[i])
        else CompareRow(i, Arr2[i], Arr[i]);
      end
      else CompareRow(i, Arr[i], Arr2[i]);
    end;
  end;
begin
  frmProject1.OpenDialog.Title := '选择清列行';
  frmProject1.OpenDialog.Filter := '*|*.txt';
  frmProject1.OpenDialog.Options := frmProject1.OpenDialog.Options + [ofAllowMultiSelect];
  if not frmProject1.OpenDialog.Execute then Exit;
  if frmProject1.OpenDialog.Files.Count <> 2 then raise Exception.Create('请选择2个清列行文件');

  FileName := frmProject1.OpenDialog.Files[0];
  LoadData(FileName, ColArr, ColArr2);
  FileName := frmProject1.OpenDialog.Files[1];
  LoadData(FileName, ColArr3, ColArr4);

  l.Clear;
  IsSecondRound := False;
  if Length(ColArr) >= Length(ColArr3) then CompareData(ColArr, ColArr3)
  else CompareData(ColArr3, ColArr);
  IsSecondRound := True;
  if Length(ColArr2) >= Length(ColArr4) then CompareData(ColArr2, ColArr4)
  else CompareData(ColArr4, ColArr2);

  FileName := ExtractFilePath(FileName) + '比较结果.txt';
  l.Text := l.Text.Replace('、@', '');
  l.Text := l.Text.Replace('@', '');
  l.SaveToFile(FileName);
end;

procedure TdmProject1.DeleteSameRow;
var
  FileName, FilePath, s: string;
  ColArr, ColArr2: TIntDyadicArray;
  r, r2: TIntegerDynArray;
  i, ConformColCount, ConformColCount2: Integer;

  {function RowExists(r, r2: TIntegerDynArray): Boolean;
  var
    i, j, k, c: Integer;
  begin
    Result := False;
    for i := Low(ColArr) to High(ColArr) do
    begin
      if Length(ColArr[i]) <> Length(r) then Continue;

      c := 0;
      for j := Low(r) to High(r) do
      begin
        for k := Low(ColArr[i]) to High(ColArr[i]) do
          if ColArr[i][k] = r[j] then
          begin
            Inc(c);
            Break;
          end;
      end;
      Result := c = Length(r);
      if Result then Break;
    end;

    if not Result then Exit;
    c := 0;
    for j := Low(r2) to High(r2) do
    begin
      for k := Low(ColArr2[i]) to High(ColArr2[i]) do
        if ColArr2[i][k] = r2[j] then
        begin
          Inc(c);
          Break;
        end;
    end;
    Result := c = Length(r2);
  end;}
  {function RowExists(r, r2: TIntegerDynArray): Boolean;
  var
    i, j, k, c: Integer;
  begin
    Result := False;
    for i := Low(ColArr) to High(ColArr) do
    begin
      c := 0;
      for j := Low(r) to High(r) do
      begin
        for k := Low(ColArr[i]) to High(ColArr[i]) do
          if ColArr[i][k] = r[j] then
          begin
            Inc(c);
            Break;
          end;
      end;
      for j := Low(r2) to High(r2) do
      begin
        for k := Low(ColArr2[i]) to High(ColArr2[i]) do
          if ColArr2[i][k] = r2[j] then
          begin
            Inc(c);
            Break;
          end;
      end;
      Result := c >= ConformColCount;
      if Result then Break;
    end;
  end; }
begin
  frmProject1.OpenDialog.Title := '选择清列行';
  frmProject1.OpenDialog.Filter := '*|*.txt';
  frmProject1.OpenDialog.Options := frmProject1.OpenDialog.Options - [ofAllowMultiSelect];
  if not frmProject1.OpenDialog.Execute then Exit;
  LoadData(frmProject1.OpenDialog.FileName, ColArr, ColArr2);

  //if not SelectDirectory('选择被清列行文件夹', '', FilePath) then Exit;
  ConformColCount := 1;
  if not Input('', '删除各个文本“相同行”;' + #$D#$A + '输入相同列数>=', ConformColCount) then Exit;

  ConformColCount2 := 256;
  if FColCount > FRangeColNo then
  begin
    ConformColCount2 := 1;
    if not Input('', '删除各个文本“相同行”;' + #$D#$A + '输入第二区域相同列数>=', ConformColCount2) then Exit;
  end;
  {for FileName in TDirectory.GetFiles(FilePath, '*', TSearchOption.soAllDirectories) do
  begin
    l.LoadFromFile(FileName);
    for i := l.Count - 1 downto 0 do
    begin
      s := l.ValueFromIndex[i];
      StrToIntArray(s, r, r2);
      if RowExists(r, r2) then l.Delete(i);
    end;

    for i := 0 to l.Count - 1 do
      l[i] := (i + 1).ToString + '=' + l.ValueFromIndex[i].Trim;
    l.SaveToFile(FileName);
  end;
  ShowMessage('删除完全相同列的行完成'); }

  TTask.Create(procedure
  var
    s, FieldName, sField, sWhere: string;
    i, j, v: Integer;
    ColNo: Int64;
    r: TIntegerDynArray;
  begin
    sField := GetFieldString;

    SetWorkState(wsExecuting);
    StartTime;
    try
      try
        //查找符合数据
        for i := Low(ColArr) to High(ColArr) do
        begin
          s := 'DECLARE @iColumnNo BIGINT' + #$D#$A
            + 'BEGIN TRAN' + #$D#$A
            + 'UPDATE [1] SET ConformColCount = 0, ConformColCount2 = 0' + #$D#$A;

          r := ColArr[i];
          for j := Low(r) to High(r) do
          begin
            v := r[j];
            if v > FColCount then Continue;

            ColNo := i64 shl (64 - v + (Ceil(v / 64) - 1) * 64);
            FieldName := 'Field' + Ceil(v / 64).ToString;
            s := s + 'SET @iColumnNo = %d' + #$D#$A
              + 'UPDATE [1] SET ConformColCount = ConformColCount + 1' + #$D#$A
              + 'WHERE %s = %s | @iColumnNo' + #$D#$A;
            s := Format(s, [ColNo, FieldName, FieldName, FieldName]);
          end;

          r := ColArr2[i];
          for j := Low(r) to High(r) do
          begin
            v := r[j] + FRangeColNo;
            if v > FColCount then Continue;

            ColNo := i64 shl (64 - v + (Ceil(v / 64) - 1) * 64);
            FieldName := 'Field' + Ceil(v / 64).ToString;
            s := s + 'SET @iColumnNo = %d' + #$D#$A
              + 'UPDATE [1] SET ConformColCount2 = ConformColCount2 + 1' + #$D#$A
              + 'WHERE %s = %s | @iColumnNo' + #$D#$A;
            s := Format(s, [ColNo, FieldName, FieldName, FieldName]);
          end;

          s := s + 'COMMIT TRAN';
          FDConnection.ExecSQL(s);

          s := 'BEGIN TRAN' + #$D#$A
            + 'DELETE FROM [1]' + #$D#$A
            + 'WHERE ConformColCount >= %d OR ConformColCount2 >= %d' + #$D#$A
            + 'COMMIT TRAN';
          s := Format(s, [ConformColCount, ConformColCount2]);
          FDConnection.ExecSQL(s);
        end;

        Flog.Log('删除完全相同列的行完成');
        ShowMessage('删除完全相同列的行完成');
      except
        on e: Exception do
        begin
          e.Message := '删除完全相同列的行失败：' + e.Message;
          FLog.Log(e.Message);
          ShowMessage(e.Message);
        end;
      end;
    finally
      StopTime;
      SetWorkState(wsNone);
    end;
  end).Start;
end;

procedure TdmProject1.Init;
begin
  Flog.Log('正在连接数据库');
  FDatabaseName := dmConfig.ReadKeyValue('DatabaseName', 'master');
  ConnectDatabase(FDatabaseName);
  if not FDConnection.Connected and (FDatabaseName <> 'master') then
  begin
    ConnectDatabase('master');
  end;
  if not FDConnection.Connected then
    raise Exception.Create('连接数据库失败，请检查数据库配置是否正确');

  TThread.Synchronize(nil, procedure
  begin
    frmProject1.Parent := frmMain;
    frmProject1.Panel5DblClick(nil);
  end);
end;

procedure TdmProject1.QueryData;
var
  s, FieldName, sField, sWhere, sFileNo: string;
  i, ConformRowCount, v, RecNo, ChosedCount: Integer;
  ColNo: Int64;
begin
  sField := 'ValueCount, ' + GetFieldString;

  ChosedCount := 0;
  RecNo := fdqFileList.RecNo;
  fdqFileList.DisableControls;
  try
    fdqFileList.First;
    while not fdqFileList.Eof do
    begin
      if fdqFileList.FieldByName('Chosed').AsBoolean then
      begin
        if sFileNo <> '' then sFileNo := sFileNo + ', ';
        sFileNo := sFileNo + fdqFileList.FieldByName('FileNo').AsString;
        Inc(ChosedCount);
      end;

      fdqFileList.Next;
    end;
  finally
    if RecNo > 0 then fdqFileList.RecNo := RecNo;
    fdqFileList.EnableControls;
  end;
  if ChosedCount = fdqFileList.RecordCount then sFileNo := '';
  if sFileNo <> '' then
    sWhere := Format('WHERE FileNo2 IN (%s)', [sFileNo]) + #$D#$A;
  s := 'WITH CTE AS(' + #$D#$A
    + 'SELECT ROW_NUMBER() OVER(ORDER BY (SELECT 0)) PageRowNo, *' + #$D#$A
    + 'FROM V_DataTable' + #$D#$A
    + sWhere
    + ')' + #$D#$A
    + 'SELECT * FROM CTE';
  s := Format(s, [sField]);

  TTask.Create(procedure
  begin
    dsDataTable.DataSet := nil;
    fdqDataTable.Open(s);

    TThread.Synchronize(nil, procedure
    begin
      frmProject1.QueryValue := [];
      frmProject1.BuildDataTableGridColumn;
      dsDataTable.DataSet := fdqDataTable;
    end);
  end).Start;
end;

procedure TdmProject1.QueryData2;
var
  FQueryValue: TIntegerDynArray;
begin
  if not Assigned(frmQueryConfig2) then frmQueryConfig2 := TfrmQueryConfig2.Create(Self);
  with frmQueryConfig2 do
  begin
    ShowModal;
    if ModalResult <> 1 then Exit;

    TTask.Create(procedure
    var
      s, FieldName, FieldName2, sField, sWhere: string;
      i, ConformRowCount, v, RecNo, ChosedCount: Integer;
      ColNo: Int64;
    begin
      sField := GetFieldString;

      SetWorkState(wsExecuting);
      StartTime;
      try
        try
          //查找符合数据
          if ValueChanged then
          begin
            SetLength(FQueryValue, 0);
            v := 1;
            for i := 0 to ControlCount - 1 do
            begin
              if not (Controls[i] is TCheckBox) then Continue;
              with Controls[i] as TCheckBox do
                if Checked then
                begin
                  SetLength(FQueryValue, Length(FQueryValue) + 1);
                  FQueryValue[Length(FQueryValue) - 1] := Tag;
                end;
            end;
            frmProject1.QueryValue := FQueryValue;

            s := 'DECLARE @iColumnNo BIGINT' + #$D#$A
              + 'BEGIN TRAN' + #$D#$A
              + 'UPDATE [1] SET ConformColCount = 0, ConformColCount2 = 0' + #$D#$A;
            for i := Low(FQueryValue) to High(FQueryValue) do
            begin
              v := FQueryValue[i];
              if v > FColCount then Continue;

              ColNo := i64 shl (64 - v + (Ceil(v / 64) - 1) * 64);
              FieldName := 'Field' + Ceil(v / 64).ToString;
              FieldName2 := 'ConformColCount';
              if v > FRangeColNo then FieldName2 := 'ConformColCount2';
              s := s + 'SET @iColumnNo = %d' + #$D#$A
                + 'UPDATE [1] SET %s = %s + 1' + #$D#$A
                + 'WHERE %s = %s | @iColumnNo' + #$D#$A;
              s := Format(s, [ColNo, FieldName2, FieldName2, FieldName, FieldName, FieldName]);
            end;
            s := s + 'COMMIT TRAN';
            FDConnection.ExecSQL(s);
            //页数
            s := 'SELECT Count(Id) FROM [1] WHERE ConformColCount > 0 OR ConformColCount2 > 0';
            ConformRowCount := VarToStr(FDConnection.ExecSQLScalar(s)).ToInteger;
            lblMaxPageNo.Caption := Format('/%d页，总行数：%d',
              [Ceil(ConformRowCount / EachPageRowCount), ConformRowCount]);
          end;
          //数据
          s := 'WITH CTE AS(' + #$D#$A
            + 'SELECT ROW_NUMBER() OVER(ORDER BY ConformColCount DESC, ConformColCount2 DESC, FolderNo, FileNo, RowNo) PageRowNo, *' + #$D#$A
            + 'FROM [1]' + #$D#$A
            + 'WHERE ConformColCount > 0 OR ConformColCount2 > 0' + #$D#$A
            + ')' + #$D#$A
            + 'SELECT * FROM CTE WHERE PageRowNo BETWEEN %d AND %d';
          s := Format(s, [(PageNo - 1) * EachPageRowCount + 1, PageNo * EachPageRowCount]);

          dsDataTable.DataSet := nil;
          fdqDataTable.Open(s);

          TThread.Synchronize(nil, procedure
          begin
            frmProject1.BuildDataTableGridColumn(1);
            dsDataTable.DataSet := fdqDataTable;
          end);
        except
          on e: Exception do
          begin
            e.Message := '查询“相同列数字”：' + e.Message;
            FLog.Log(e.Message);
            ShowMessage(e.Message);
          end;
        end;
      finally
        StopTime;
        SetWorkState(wsNone);
      end;
    end).Start;
  end;
end;

procedure TdmProject1.QueryData3;
var
  i, ConformCount, ConformCount2, ConformCount3, IdenticalColCount, CompareRowCount: Integer;
  ColArr, ColArr2: TIntDyadicArray;

  procedure Compare(SourceIndex, IdenticalColCount: Integer; var ConformCount, ConformCount2, ConformCount3: Integer);
  var
    i, j, k, ColNo, FindCount, FindCount2: Integer;
    r, sr, sr2: TIntegerDynArray;
  begin
    ConformCount := 0;
    ConformCount2 := 0;
    ConformCount3 := 0;
    sr := ColArr[SourceIndex];
    sr2 := ColArr2[SourceIndex];
    for i := SourceIndex + 1 to SourceIndex + CompareRowCount do
    begin
      FindCount := 0;
      r := ColArr[i];
      for j := Low(sr) to High(sr) do
      begin
        ColNo := sr[j];
        for k := Low(r) to High(r) do
          if r[k] = ColNo then
          begin
            Inc(FindCount);
            Break;
          end;
      end;
      FindCount2 := 0;
      r := ColArr2[i];
      for j := Low(sr2) to High(sr2) do
      begin
        ColNo := sr2[j];
        for k := Low(r) to High(r) do
          if r[k] = ColNo then
          begin
            Inc(FindCount2);
            Break;
          end;
      end;
      if ((IdenticalColCount = 0) and (FindCount = 0))
        or ((IdenticalColCount > 0) and (FindCount >= IdenticalColCount))
      then Inc(ConformCount2);
      if ((IdenticalColCount = 0) and (FindCount2 = 0))
        or ((IdenticalColCount > 0) and (FindCount2 >= IdenticalColCount))
      then Inc(ConformCount3);
      FindCount := FindCount + FindCount2;
      if ((IdenticalColCount = 0) and (FindCount = 0))
        or ((IdenticalColCount > 0) and (FindCount >= IdenticalColCount))
      then Inc(ConformCount);
    end;
  end;
begin
  if not frmProject1.OpenDialog.Execute then Exit;
  if not Assigned(frmQueryConfig3) then frmQueryConfig3 := TfrmQueryConfig3.Create(Self);
  with frmQueryConfig3 do
  begin
    ShowModal;
    if ModalResult <> 1 then Exit;
    IdenticalColCount := StrToInt(edtIdenticalColCount.Text);
    CompareRowCount := StrToInt(edtCompareRowCount.Text);

    LoadData(frmProject1.OpenDialog.FileName, ColArr, ColArr2);
    fdmtCompareResult.DisableControls;
    fdmtCompareResult.EmptyDataSet;
    try
      for i := Low(ColArr) to High(ColArr) - CompareRowCount do
      begin
        Compare(i, IdenticalColCount, ConformCount, ConformCount2, ConformCount3);
        if ConformCount > 0 then
        begin
          if fdmtCompareResult.Locate('ConformCount', ConformCount, []) then
          begin
            fdmtCompareResult.Edit;
            fdmtCompareResult.FieldByName('OccurrenceCount').AsInteger := fdmtCompareResult.FieldByName('OccurrenceCount').AsInteger + 1;
            fdmtCompareResult.Post;
          end
          else
          begin
            fdmtCompareResult.Append;
            fdmtCompareResult.FieldByName('ConformCount').AsInteger := ConformCount;
            fdmtCompareResult.FieldByName('OccurrenceCount').AsInteger := 1;
            fdmtCompareResult.FieldByName('ConformCount2').AsInteger := 0;
            fdmtCompareResult.FieldByName('OccurrenceCount2').AsInteger := 0;
            fdmtCompareResult.FieldByName('ConformCount3').AsInteger := 0;
            fdmtCompareResult.FieldByName('OccurrenceCount3').AsInteger := 0;
            fdmtCompareResult.Post;
          end;
        end;

        if ConformCount2 > 0 then
        begin
          if fdmtCompareResult.Locate('ConformCount2', ConformCount2, []) then
          begin
            fdmtCompareResult.Edit;
            fdmtCompareResult.FieldByName('OccurrenceCount2').AsInteger := fdmtCompareResult.FieldByName('OccurrenceCount2').AsInteger + 1;
            fdmtCompareResult.Post;
          end
          else
          begin
            fdmtCompareResult.Append;
            fdmtCompareResult.FieldByName('ConformCount').AsInteger := 0;
            fdmtCompareResult.FieldByName('OccurrenceCount').AsInteger := 0;
            fdmtCompareResult.FieldByName('ConformCount2').AsInteger := ConformCount2;
            fdmtCompareResult.FieldByName('OccurrenceCount2').AsInteger := 1;
            fdmtCompareResult.FieldByName('ConformCount3').AsInteger := 0;
            fdmtCompareResult.FieldByName('OccurrenceCount3').AsInteger := 0;
            fdmtCompareResult.Post;
          end;
        end;

        if ConformCount3 > 0 then
        begin
          if fdmtCompareResult.Locate('ConformCount3', ConformCount3, []) then
          begin
            fdmtCompareResult.Edit;
            fdmtCompareResult.FieldByName('OccurrenceCount3').AsInteger := fdmtCompareResult.FieldByName('OccurrenceCount3').AsInteger + 1;
            fdmtCompareResult.Post;
          end
          else
          begin
            fdmtCompareResult.Append;
            fdmtCompareResult.FieldByName('ConformCount').AsInteger := 0;
            fdmtCompareResult.FieldByName('OccurrenceCount').AsInteger := 0;
            fdmtCompareResult.FieldByName('ConformCount2').AsInteger := 0;
            fdmtCompareResult.FieldByName('OccurrenceCount2').AsInteger := 0;
            fdmtCompareResult.FieldByName('ConformCount3').AsInteger := ConformCount3;
            fdmtCompareResult.FieldByName('OccurrenceCount3').AsInteger := 1;
            fdmtCompareResult.Post;
          end;
        end;
      end;
      fdmtCompareResult.First;
    finally
      fdmtCompareResult.EnableControls;
    end;
  end;
end;

end.

