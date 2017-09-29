unit udmProject2;

interface

uses
  uLog, uGlobal, uDriveInfo, mORMot, mORMotSQLite3, SynSQLite3Static,
  System.SysUtils, System.Classes, System.Math, System.JSON, System.Threading,
  System.IOUtils, System.Variants, System.Generics.Collections, System.Types,
  Vcl.Dialogs, Vcl.Forms;

type
  TWorkType = (wtNull, wtBrowse, wtClearColumnAndCombine, wtExportToFile, wtSort);
  TClearColumnType = (cctInit, cctInputClearColumn, cctInputData, cctClearColumn,
    cctCombineFile, cctResetFileNo, cctGrouping, cctSortRow, cctFinish);

  TdmProject2 = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    Database: TSQLRestServerDB;
    Model: TSQLModel;
    Model2: TSQLModel;
    l: TStringList;

    fWorkType: TWorkType;
    fRuning: Boolean;
    fStopWork: Boolean;
    fStopTime: Boolean;

    fColCount: Integer;
    fRangeColNo: Integer;
    fInvalidColCount: Integer;
    fInvalidColCount2: Integer;
    fClearColumnFileName: string;
    fInputDataDirectory: string;
    fClearColumnType: TClearColumnType;
    fClearColumnFileNo: Integer;
    fClearColumnFileId: Integer;
    fCombineFileNo: Integer;
    fCombineFileId: Integer;
    fCombineRowId: Integer;
    fSortValueCount: Integer;
    fSortValueCount2: Integer;
    fSortFileId: Integer;
    fEachFileRowCount: Integer;
    fExportFileDirectory: string;

    procedure SetWorkType(AValue: TWorkType);
    procedure SetClearColumnType(AValue: TClearColumnType);
    procedure StopWork;
    procedure CheckStop;
    procedure StartTime;
    procedure StopTime;
    function ReadValue(AKey: string; ADefaultValue: Variant): string;
    procedure WriteValue(AKey: string; AValue: Variant);
  public
    procedure ClearColumnAndCombine;
    procedure ExportToFile;
  end;

var
  dmProject2: TdmProject2;

implementation

uses
  ufrmProject2, ufrmInput, ufrmSingleChoice, ufrmConfirm, udmRegister,
  ufrmQueryConfig2, ufrmQueryConfig3, UfrmMain,
  uDataTable, uConfig, uCacheFile;

{%CLASSGROUP 'VCL.Controls.TControl'}

{$R *.dfm}

procedure TdmProject2.DataModuleCreate(Sender: TObject);
begin
  Model := TSQLModel.Create([TSQLClearColumn, TSQLConfig]);
  Model2 := TSQLModel.Create([TSQLData]);
  Database := TSQLRestServerDB.Create(Model, 'Config.db3');
  Database.CreateMissingTables;

  l := TStringList.Create;

  frmProject2 := TfrmProject2.Create(frmMain);
  with frmProject2 do
  begin
    edtColCount.Text := ReadValue('edtColCount', '0');
    edtRangeColNo.Text := ReadValue('edtRangeColNo', '0');
    edtInvalidColCount.Text := ReadValue('edtInvalidColCount', '0');
    edtInvalidColCount2.Text := ReadValue('edtInvalidColCount2', '0');
    edtClearColumnFileName.Text := ReadValue('edtClearColumnFileName', '');
    edtInputDataDirectory.Text := ReadValue('edtInputDataDirectory', '');
    edtEachFileRowCount.Text := ReadValue('edtEachFileRowCount', '0');
    edtExportFileDirectory.Text := ReadValue('edtExportFileDirectory', '');
  end;

  fWorkType := TWorkType(ReadValue('WorkType', 0).ToInteger);
  fColCount := ReadValue('ColCount', 0).ToInteger;
  fRangeColNo := ReadValue('RangeColNo', fColCount).ToInteger;
  fInvalidColCount := ReadValue('InvalidColCount', 0).ToInteger;
  fInvalidColCount2 := ReadValue('InvalidColCount2', 0).ToInteger;
  fClearColumnFileName := ReadValue('ClearColumnFileName', '');
  fInputDataDirectory := ReadValue('InputDataDirectory', '');
  fClearColumnType := TClearColumnType(ReadValue('ClearColumnType', 0).ToInteger);
  fClearColumnFileNo := ReadValue('ClearColumnFileNo', 0).ToInteger;
  fClearColumnFileId := ReadValue('ClearColumnFileId', 0).ToInteger;
  fCombineFileNo := ReadValue('CombineFileNo', 0).ToInteger;
  fCombineFileId := ReadValue('CombineFileId', 0).ToInteger;
  fCombineRowId := ReadValue('CombineRowId', 0).ToInteger;
  fSortValueCount := ReadValue('SortValueCount', 0).ToInteger;
  fSortValueCount2 := ReadValue('SortValueCount2', 0).ToInteger;
  fSortFileId := ReadValue('SortFileId', 0).ToInteger;
  fEachFileRowCount := ReadValue('EachFileRowCount', 100).ToInteger;
  fExportFileDirectory := ReadValue('ExportFileDirectory', '');

  TThread.Synchronize(nil, procedure
  begin
    frmProject2.Parent := frmMain;
    frmProject2.Panel5DblClick(nil);
    SetWorkType(fWorkType);
  end);
end;

procedure TdmProject2.DataModuleDestroy(Sender: TObject);
begin
  l.Free;
  Database.Free;
  Model.Free;
  Model2.Free;

  with frmProject2 do
  begin
    WriteValue('edtColCount', edtColCount.Text);
    WriteValue('edtRangeColNo', edtRangeColNo.Text);
    WriteValue('edtInvalidColCount', edtInvalidColCount.Text);
    WriteValue('edtColCount', edtInvalidColCount2.Text);
    WriteValue('edtColCount', edtClearColumnFileName.Text);
    WriteValue('edtColCount', edtInputDataDirectory.Text);
    WriteValue('edtColCount', edtEachFileRowCount.Text);
    WriteValue('edtColCount', edtExportFileDirectory.Text);
  end;
end;

procedure TdmProject2.SetWorkType(AValue: TWorkType);
var
  CanWork: Boolean;
  ButtonCaption: string;
begin
  if (fWorkType = wtBrowse) and (AValue > wtBrowse) then fRuning := True;
  if (fWorkType > wtBrowse) then fRuning := not fRuning;

  ButtonCaption := '继续';
  if fRuning then
  begin
    ButtonCaption := '停止';
    if (fWorkType > wtBrowse)
      and (Application.MessageBox('是否继续', '', 4) = 7)
    then
    begin
      AValue := wtBrowse;
      fRuning := False;

      SetClearColumnType(cctInit);
    end;
  end;

  TThread.Synchronize(nil, procedure
  begin
    CanWork := not dmRegister.ProbationExpired and (AValue = wtBrowse);
    with frmProject2 do
    begin
      case AValue of
        wtBrowse:
        begin
          btnClearColumn.Caption := '1.执行“清列合并”';
          btnExportToFile.Caption := '2.导出“数据”（清列合并后设定）';
          fRuning := False;
        end;
        wtClearColumnAndCombine: btnClearColumn.Caption := ButtonCaption;
        wtExportToFile: btnExportToFile.Caption := ButtonCaption;
      end;
    end;

    if fWorkType <> AValue then
    begin
      fWorkType := AValue;
      WriteValue('WorkType', Ord(fWorkType));
    end;
  end);

  if not fRuning then Abort;
end;

procedure TdmProject2.SetClearColumnType(AValue: TClearColumnType);
begin
  TThread.Synchronize(nil, procedure
  begin
    fClearColumnType := AValue;
    WriteValue('ClearColumnType', Ord(fClearColumnType));
  end);
end;

procedure TdmProject2.StopWork;
begin
  TThread.Synchronize(nil, procedure
  begin
    fStopWork := True;
  end);
end;

procedure TdmProject2.CheckStop;
begin
  TThread.Synchronize(nil, procedure
  begin
    if fStopWork then Abort;
  end);
end;

procedure TdmProject2.StartTime;
begin
  fStopTime := False;

  TTask.Create(procedure
  var
    UseTime: string;
    StartTime: TDateTime;
    Day, Hour, Min, Sec, Seconds: Integer;
  begin
    StartTime := Now;
    while True do
    begin
      //Seconds := Now - StartTime;
      Day := Seconds div 86400;
      Hour := (Seconds mod 86400) div 3600;
      Min := (Seconds mod 3600) div 60;
      Sec := Seconds mod 60;
      UseTime := Format('%d日%d时%d分%d秒', [Day, Hour, Min, Sec]);

      TThread.Synchronize(nil, procedure
      begin
        frmProject2.pnlUseTime.Caption := UseTime;
      end);

      if fStopTime then Break;
      Sleep(1000);
    end;
  end).Start;
end;

procedure TdmProject2.StopTime;
begin
  TThread.Synchronize(nil, procedure
  begin
    fStopTime := True;
  end);
end;

function TdmProject2.ReadValue(AKey: string; ADefaultValue: Variant): string;
var
  ConfigRec: TSQLConfig;
begin
  TSQLConfig.AutoFree(ConfigRec, Database, 'KeyName = ?', [AKey]);
  if ConfigRec.FillOne then Result := ConfigRec.KeyValue
  else Result := VarToStr(ADefaultValue).Trim;
end;

procedure TdmProject2.WriteValue(AKey: string; AValue: Variant);
var
  ConfigRec: TSQLConfig;
begin
  if AKey.Trim.IsEmpty then
    raise Exception.Create('Key不能为空');

  TSQLConfig.AutoFree(ConfigRec);
  ConfigRec.KeyName := AKey.Trim;
  ConfigRec.KeyValue := VarToStr(AValue).Trim;
  if Database.Add(ConfigRec, True) = 0 then
    raise Exception.Create(Format('添加%s值：%s失败：', [ConfigRec.KeyName, ConfigRec.KeyValue]));
end;

procedure TdmProject2.ClearColumnAndCombine;
begin
  SetWorkType(wtClearColumnAndCombine);

  TTask.Create(procedure

    procedure Init;
    var
      s, FileDirectory, FileName: string;
    begin
      //删除缓存文件
      for s in Drives do
      begin
        FileDirectory := s + 'Cache Data\';
        for FileName in TDirectory.GetFiles(FileDirectory, '*.db3', TSearchOption.soAllDirectories) do
        begin
          CheckStop;
          if FileExists(FileName) then DeleteFile(FileName);
        end;
      end;
      Database.Delete(TSQLCacheFile, '');
      Database.Delete(TSQLClearColumn, '');

      with frmProject2.edtColCount do
      begin
        if not TryStrToInt(Text, fColCount) then
        begin
          SetFocus;
          SelectAll;
          raise Exception.Create('请输入有效数字');
        end;
      end;
      WriteValue('ColCount', FColCount);

      with frmProject2.edtRangeColNo do
      begin
        if not TryStrToInt(Text, fRangeColNo) then
        begin
          SetFocus;
          SelectAll;
          raise Exception.Create('请输入有效数字');
        end;
      end;
      WriteValue('RangeColNo', FRangeColNo);

      with frmProject2.edtInvalidColCount do
      begin
        if not TryStrToInt(Text, fInvalidColCount) then
        begin
          SetFocus;
          SelectAll;
          raise Exception.Create('请输入有效数字');
        end;
      end;
      WriteValue('InvalidColCount', fInvalidColCount);

      with frmProject2.edtInvalidColCount2 do
      begin
        if not TryStrToInt(Text, fInvalidColCount2) then
        begin
          SetFocus;
          SelectAll;
          raise Exception.Create('请输入有效数字');
        end;
      end;
      WriteValue('InvalidColCount2', fInvalidColCount2);

      fClearColumnFileName := frmProject2.edtClearColumnFileName.Text;
      WriteValue('ClearColumnFileName', fClearColumnFileName);
      fInputDataDirectory := frmProject2.edtInputDataDirectory.Text;
      WriteValue('InputDataDirectory', fInputDataDirectory);

      SetClearColumnType(cctInputClearColumn);
    end;

    procedure InputClearColumn;
    var
      ClearColumnRec: TSQLClearColumn;
      s: string;
      r: TIntegerDynArray;
      i, ColNo, FileNo: Integer;
    begin
      l.LoadFromFile(fClearColumnFileName);

      TSQLClearColumn.AutoFree(ClearColumnRec);
      Database.TransactionBegin(TSQLClearColumn);
      try
        Database.Delete(TSQLClearColumn, '');
        for i := 0 to l.Count - 1 do
        begin
          if not TryStrToInt(l.Names[i], FileNo) then Continue;
          s := l.ValueFromIndex[i].Trim;
          StrToIntArray(s, r, FRangeColNo);

          ClearColumnRec.FileNo := FileNo;
          ClearColumnRec.Values := r;
          if Database.Add(ClearColumnRec, True) = 0 then
            raise Exception.Create('添加清列行文本的行失败：' + #$D#$A + s);
        end;
        Database.Commit(1, True);

        SetClearColumnType(cctInputData);
      except
        Database.RollBack;
        raise;
      end;
    end;

    procedure InputData;
    var
      CacheFileRec: TSQLCacheFile;
      Database2: TSQLRestServerDB;
      DataRec: TSQLData;
      r: TIntegerDynArray;
      s, FileName, DBFileName, sFileNo: string;
      i, StartIndex, EndIndex, FileNo: Integer;
      ColNo: Int64;
    begin
      TSQLCacheFile.AutoFree(CacheFileRec);
      for FileName in TDirectory.GetFiles(fInputDataDirectory, '*.txt', TSearchOption.soAllDirectories) do
      begin
        CheckStop;
        //检验文件名
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
        //文件号存在跳过
        CacheFileRec.FillPrepare(Database, 'FileNo = ?', [FileNo]);
        if CacheFileRec.FillOne then Continue;

        FLog.Log(Format('正在导入数据：%s，请稍等...', [ExtractFileName(FileName)]));
        l.LoadFromFile(FileName);
        for i := l.Count - 1 downto 0 do
          if l[i].Trim.IsEmpty then l.Delete(i);
        DBFileName := BuildFileName(FileNo);
        Database2 := TSQLRestServerDB.Create(Model2, DBFileName);
        Database2.CreateMissingTables;
        try
          Database2.TransactionBegin(TSQLData);
          try
            TSQLData.AutoFree(DataRec);
            for i := 0 to l.Count - 1 do
            begin
              CheckStop;

              s := l[i].Trim;
              s := s.Substring(s.IndexOf('=') + 1);
              StrToIntArray(s, r, FRangeColNo);

              DataRec.Values := r;
              if Database2.Add(DataRec, True) = 0 then
                raise Exception.Create('添加被清列文本的行失败：' + #$D#$A + s);
            end;
            Database2.Commit(1, True);

            CacheFileRec.FileNo := FileNo;
            CacheFileRec.FileName := DBFileName;
            CacheFileRec.RowCount := l.Count;
            CacheFileRec.ValueCount := 0;
            CacheFileRec.ValueCount2 := 0;
            if Database.Add(CacheFileRec, True) = 0 then
              raise Exception.Create('添加缓存文件记录失败：' + #$D#$A + DBFileName);
          except
            Database2.RollBack;
            raise;
          end;
        finally
          Database2.Free;
        end;
      end;

      fClearColumnFileNo := 0;
      WriteValue('ClearColumnFileNo', fClearColumnFileNo);
      fClearColumnFileId := 0;
      WriteValue('ClearColumnFileId', fClearColumnFileId);
      SetClearColumnType(cctClearColumn);
    end;

    procedure ClearColumn;
    var
      Database2: TSQLRest;
      ClearColumnRec: TSQLClearColumn;
      CacheFileRec: TSQLCacheFile;
      DataRec: TSQLData;

      function Invalid: Boolean;
      var
        v, ValueCount, ValueCount2: Integer;
      begin
        ValueCount := 0;
        ValueCount2 := 0;
        for v in DataRec.Values do
          if v > fRangeColNo then Inc(ValueCount2) else Inc(ValueCount);
        Result := ValueCount < fInvalidColCount;
        if not Result and (fColCount > fRangeColNo) then
          Result := ValueCount2 < fInvalidColCount2;
      end;
    begin
      TSQLData.AutoFree(DataRec);
      TSQLCacheFile.AutoFree(CacheFileRec);
      TSQLClearColumn.AutoFree(ClearColumnRec, DataBase, 'ORDER BY FileNo', []);
      while ClearColumnRec.FillOne do
      begin
        CheckStop;
        if ClearColumnRec.FileNo <= fClearColumnFileNo then Continue;
        CacheFileRec.FillPrepare(DataBase, 'FileNo = ? AND Id > ? ORDER BY Id',
          [ClearColumnRec.FileNo, fClearColumnFileId]);
        while CacheFileRec.FillOne do
        begin
          CheckStop;
          Database2 := TSQLRestServerDB.Create(Model2, CacheFileRec.FileName);
          DataRec.FillPrepare(Database2);
          Database2.TransactionBegin(TSQLData, 1);
          try
            try
              while DataRec.FillOne do
              begin
                CheckStop;
                DataRec.ClearValue(ClearColumnRec.Values);
                if Invalid then Database2.Delete(TSQLData, DataRec.ID)
                else Database2.Update(DataRec);
              end;
              Database2.Commit(1, True);

              fClearColumnFileId := CacheFileRec.ID;
              WriteValue('ClearColumnFileId', fClearColumnFileId);
            except
              Database2.RollBack(1);
              raise;
            end;

            CacheFileRec.RowCount := Database2.TableRowCount(TSQLData);
            if not DataBase.Update(CacheFileRec) then
              raise Exception.Create('更新缓存文件行数失败');
          finally
            Database2.Free;
          end;
        end;
        fClearColumnFileNo := ClearColumnRec.FileNo;
        WriteValue('ClearColumnFileNo', fClearColumnFileNo);
      end;
      CacheFileRec.FillPrepare(DataBase, 'FileNo = 1', []);
      if CacheFileRec.FillOne then
        SetClearColumnType(cctGrouping)
      else
      begin
        fCombineFileNo := 0;
        WriteValue('CombineFileNo', fCombineFileNo);
        fCombineFileId := 0;
        WriteValue('CombineFileId', fCombineFileId);
        fCombineRowId := 0;
        WriteValue('CombineRowId', fCombineRowId);
        SetClearColumnType(cctCombineFile);
      end;
    end;

    procedure CombineFile;
    var
      i, RowCount, FileNo: Integer;
      DBFileName: string;
      FileNoList: TSQLTableJSON;
      CacheFileRec, CacheFileRec2, CacheFileRec3: TSQLCacheFile;
      DataRec, DataRec2: TSQLData;
      Database2, Database3: TSQLRestServerDB;
    begin
      TSQLCacheFile.AutoFree(CacheFileRec);
      TSQLCacheFile.AutoFree(CacheFileRec2);
      TSQLData.AutoFree(DataRec);
      TSQLData.AutoFree(DataRec2);

      FileNoList := Database.MultiFieldValues(TSQLCacheFile, 'FileNo',
        'FileNo > ? GROUP BY FileNo ORDER BY FileNo', [fCombineFileNo]);
      try
        while FileNoList.Step do
        begin
          FileNo := FileNoList.Field(0);
          CacheFileRec.FillPrepare(DataBase, 'FileNo > ? AND Id > ? ORDER BY FileNo, FileName', [FileNo, fCombineFileId]);
          while CacheFileRec.FillOne do
          begin
            CheckStop;
            Database2 := TSQLRestServerDB.Create(Model2, CacheFileRec.FileName);
            try
              DataRec.FillPrepare(Database2, 'ID > ?', [fCombineRowId]);
              repeat
                CheckStop;
                //获取接受数据库
                CacheFileRec2.FillPrepare(DataBase, 'FileNo = ? AND RowCount < 100000 ORDER BY FileNo LIMIT 1', [FileNo]);
                if CacheFileRec2.FillOne then
                  Database3 := TSQLRestServerDB.Create(Model2, CacheFileRec2.FileName)
                else
                begin
                  DBFileName := BuildFileName(FileNo);
                  Database3 := TSQLRestServerDB.Create(Model2, DBFileName);
                  Database3.CreateMissingTables;

                  CacheFileRec2.FileNo := FileNo;
                  CacheFileRec2.FileName := DBFileName;
                  CacheFileRec2.RowCount := 0;
                  CacheFileRec2.ValueCount := 0;
                  CacheFileRec2.ValueCount2 := 0;
                  if Database.Add(CacheFileRec2, True) = 0 then
                    raise Exception.Create('添加缓存文件失败：' + #$D#$A + DBFileName);
                end;
                try
                  RowCount := Database3.TableRowCount(TSQLData);
                  Database3.TransactionBegin(TSQLData);
                  try
                    while DataRec.FillOne do
                    begin
                      CheckStop;

                      DataRec2.Field1 := DataRec.Field1;
                      DataRec2.Field2 := DataRec.Field2;
                      DataRec2.Field3 := DataRec.Field3;
                      DataRec2.Field4 := DataRec.Field4;
                      if Database3.Add(DataRec2, True) = 0 then
                        raise Exception.Create('添加缓存文件记录失败：' + #$D#$A + DBFileName);

                      Inc(RowCount);
                      if RowCount >= 100000 then Break;
                    end;
                    Database3.Commit(1, True);

                    fCombineRowId := DataRec.ID;
                    WriteValue('CombineRowId', fCombineRowId);
                  except
                    Database3.RollBack;
                    raise;
                  end;
                  CacheFileRec2.RowCount := Database3.TableRowCount(TSQLData);
                  if not Database.Update(CacheFileRec2) then
                    raise Exception.Create('更新缓存文件行数失败');
                finally
                  Database3.Free;
                end;
              until DataRec.FillCurrentRow > DataRec.FillTable.RowCount;

              fCombineFileId := CacheFileRec.ID;
              WriteValue('CombineFileId', fCombineFileId);
              fCombineRowId := 0;
              WriteValue('CombineRowId', fCombineRowId);
            finally
              Database2.Free;
            end;
          end;
          fCombineFileNo := FileNo;
          WriteValue('CombineFileNo', fCombineFileNo);
          fCombineFileId := 0;
          WriteValue('CombineFileId', fCombineFileId);
        end;
      finally
        FileNoList.Free;
      end;

      SetClearColumnType(cctResetFileNo);
    end;

    procedure ResetFileNo;
    var
      CacheFileRec: TSQLCacheFile;
    begin
      TSQLCacheFile.AutoFree(CacheFileRec, DataBase, '', []);
      Database.TransactionBegin(TSQLData, 1);
      try
        while CacheFileRec.FillOne do
        begin
          CacheFileRec.FileNo := CacheFileRec.FileNo - 1;
          if not Database.Update(CacheFileRec) then
            raise Exception.Create('重置行号出错');
        end;
        Database.Commit(1, True);
      except
        Database.RollBack;
        raise;
      end;

      fClearColumnFileNo := 0;
      WriteValue('ClearColumnFileNo', fClearColumnFileNo);
      fClearColumnFileId := 0;
      WriteValue('ClearColumnFileId', fClearColumnFileId);
      SetClearColumnType(cctClearColumn);
    end;

    procedure Grouping;
    var
      Database2, Database3: TSQLRestServerDB;
      CacheFileRec, CacheFileRec2: TSQLCacheFile;
      DataRec: TSQLData;
      DBFileName: string;
      i, v, ValueCount, ValueCount2, FileNo: Integer;

      function RowExists(ValueCount, ValueCount2: Integer; ARow: TSQLData): Boolean;
      var
        Database2: TSQLRestServerDB;
        CacheFileRec: TSQLCacheFile;
        DataRec: TSQLData;
      begin
        Result := False;
        TSQLData.AutoFree(DataRec);
        TSQLCacheFile.AutoFree(CacheFileRec, Database,
          'ValueCount = ? AND ValueCount2 = ?',
          [ValueCount, ValueCount2]);
        while CacheFileRec.FillOne do
        begin
          Database2 := TSQLRestServerDB.Create(Model2, CacheFileRec.FileName);
          DataRec.FillPrepare(Database3,
            'Field1 = ? AND Field2 = ? AND Field3 = ? AND Field4 = ?',
            [DataRec.Field1, DataRec.Field2, DataRec.Field3, DataRec.Field4]);
          Result := DataRec.FillTable.RowCount > 0;
          if Result then Break;
        end;
      end;
    begin
      TSQLCacheFile.AutoFree(CacheFileRec, DataBase, 'ValueCount = 0 AND ValueCount2 = 0 ORDER BY FileNo', []);
      TSQLCacheFile.AutoFree(CacheFileRec2, DataBase, 'ValueCount > 0 OR ValueCount2 > 0 ORDER BY FileNo DESC LIMIT 1', []);
      FileNo := 0;
      if CacheFileRec2.FillOne then FileNo := CacheFileRec2.FileNo;
      TSQLData.AutoFree(DataRec);
      while CacheFileRec.FillOne do
      begin
        Database2 := TSQLRestServerDB.Create(Model2, CacheFileRec.FileName);
        try
          DataRec.FillPrepare(Database2);
          while DataRec.FillOne do
          begin
            ValueCount := 0;
            ValueCount2 := 0;
            for v in DataRec.Values do
              if v > fRangeColNo then Inc(ValueCount2) else Inc(ValueCount);
            //检查是否重复
            if RowExists(ValueCount, ValueCount2, DataRec) then Continue;
            //获取接收数据库
            CacheFileRec2.FillPrepare(Database,
              'ValueCount = ? AND ValueCount2 = ? AND RowCount < 100000 LIMIT 1',
              [ValueCount, ValueCount2]);
            if CacheFileRec2.FillOne then
              Database3 := TSQLRestServerDB.Create(Model2, CacheFileRec2.FileName)
            else
            begin
              DBFileName := BuildFileName(0);
              Database3 := TSQLRestServerDB.Create(Model2, DBFileName);
              Inc(FileNo);
              CacheFileRec2.FileNo := FileNo;
              CacheFileRec2.RowCount := 0;
              CacheFileRec2.ValueCount := ValueCount;
              CacheFileRec2.ValueCount2 := ValueCount2;
              CacheFileRec2.FileName := DBFileName;
              if Database.Add(CacheFileRec2, True) = 0 then
                raise Exception.Create(Format('添加缓存文件失败：%s', [DBFileName]));
            end;
            //添加数据
            if Database3.Add(DataRec, True) = 0 then
              raise Exception.Create('添加缓存记录失败');
            CacheFileRec2.RowCount := Database3.TableRowCount(TSQLData);
            if not Database.Update(CacheFileRec2) then
              raise Exception.Create('更新缓存文件行数失败');
            if not Database2.Delete(TSQLData, DataRec.ID) then
              raise Exception.Create('删除已分组缓存文件记录失败');
          end;
        finally
          Database2.Free;
        end;
        if FileExists(CacheFileRec.FileName) then
          DeleteFile(CacheFileRec.FileName);
        if not Database.Delete(TSQLCacheFile, CacheFileRec.ID) then
          raise Exception.Create('删除已分组缓存文件记录失败');
      end;

      fSortFileId := 0;
      WriteValue('SortFileId', fSortFileId);
      fSortValueCount := 0;
      WriteValue('SortValueCount', fSortValueCount);
      fSortValueCount2 := 0;
      WriteValue('SortValueCount2', fSortValueCount2);
      SetClearColumnType(cctSortRow);
    end;

    procedure SortRow;

      function NeedExchange(Value, Value2: TIntegerDynArray): Boolean;
      var
        i: Integer;
      begin
        Result := False;
        if Length(Value) = 0 then Result := True
        else
        begin
          for i := Low(Value) to High(Value) do
            if Value[i] <> Value2[i] then
            begin
              Result := Value[i] > Value2[i];
              Break;
            end;
        end;
      end;

      procedure SortEachFile;
      var
        CacheFileRec: TSQLCacheFile;
        Database2: TSQLRestServerDB;
        DataRec: TSQLData;
        i, g, j: Integer;
        v, v2: TIntegerDynArray;
      begin
        if fSortFileId = -1 then Exit;
        TSQLData.AutoFree(DataRec);
        TSQLCacheFile.AutoFree(CacheFileRec, Database, 'Id > ? ORDER BY Id', [fSortFileId]);
        while not CacheFileRec.FillOne do
        begin
          CheckStop;
          Database2 := TSQLRestServerDB.Create(Model2, CacheFileRec.FileName);
          try
            DataRec.FillPrepare(Database2);
            g := DataRec.FillTable.RowCount;
            repeat
              CheckStop;

              g := g div 2;
              for i := g to DataRec.FillTable.RowCount do
              begin
                CheckStop;

                j := i;
                while j >= g do
                begin
                  CheckStop;

                  DataRec.FillRow(j - g);
                  v := DataRec.Values;
                  DataRec.FillRow(j);
                  v2 := DataRec.Values;
                  if NeedExchange(v, v2) then
                  begin
                    Database2.TransactionBegin(TSQLData);
                    try
                      DataRec.Values := v;
                      if not Database2.Update(DataRec) then
                        raise Exception.Create('交换行失败');
                      DataRec.FillRow(j - g);
                      DataRec.Values := v2;
                      if not Database2.Update(DataRec) then
                        raise Exception.Create('交换行失败');
                      Database2.Commit(1, True);
                    except
                      on e: Exception do
                      begin
                        Database2.RollBack;
                        raise;
                      end;
                    end;
                    j := j - g;
                  end;
                end;
              end;
            until g = 0;
          finally
            Database2.Free;
          end;

          fSortFileId := CacheFileRec.ID;
          WriteValue('SortFileId', fSortFileId);
        end;
        fSortFileId := -1;
        WriteValue('SortFileId', fSortFileId);
      end;

      procedure SortGroupFile;
      type
        TCompareFileType = (cftOpenFile, cftOpenFile2, cftOpenFile3);
        TCompareFileTypes = set of TCompareFileType;
        TCacheFileRec = record
          Id: Integer;
          FileName: string;
        end;
      var
        Database2, Database3, Database4: TSQLRestServerDB;
        ValueCountList, FileNoList: TSQLTableJSON;
        CacheFileRec, CacheFileRec2, CacheFileRec3: TSQLCacheFile;
        DataRec, DataRec2: TSQLData;
        CompareFileTypes: TCompareFileTypes;
        DBFileName: string;
        MaxFileNo, FileNo, FileNo2: Integer;
        v, v2: TIntegerDynArray;
        CacheFile: TCacheFileRec;
        CacheFiles: array of TCacheFileRec;
      begin
        TSQLCacheFile.AutoFree(CacheFileRec, DataBase, 'ORDER BY FileNo DESC LIMIT 1', []);
        MaxFileNo := 0;
        if CacheFileRec.FillOne then MaxFileNo := CacheFileRec.FileNo;
        TSQLCacheFile.AutoFree(CacheFileRec2);
        TSQLCacheFile.AutoFree(CacheFileRec3);
        TSQLData.AutoFree(DataRec);
        TSQLData.AutoFree(DataRec2);
        //获取没有排序的组列表
        ValueCountList := Database.MultiFieldValues(TSQLCacheFile, 'ValueCount,ValueCount2',
          'ValueCount > ? AND ValueCount2 > ? GROUP BY ValueCount, ValueCount2 ORDER BY ValueCount, ValueCount2',
          [fSortValueCount, fSortValueCount2]
        );
        try
          while ValueCountList.Step do
          begin
            CheckStop;
            //循环对组的相邻两个文件号的数据进行比较直到文件号变为1
            repeat
              CheckStop;

              FileNoList := Database.MultiFieldValues(TSQLCacheFile, 'FileNo',
                'ValueCount = ? AND ValueCount2 = ? GROUP BY FileNo ORDER BY FileNo',
               [ValueCountList.Field(0), ValueCountList.Field(1)]
              );
              try
                if FileNoList.RowCount <= 1 then Break;
                FileNo := 0;
                if FileNoList.Step then FileNo := FileNoList.Field(0);
                FileNo2 := 0;
                if FileNoList.Step then FileNo2 := FileNoList.Field(0);
                if (FileNo = 0) or (FileNo2 = 0) then Continue;

                CacheFileRec.FillPrepare(
                  Database,
                  'ValueCount = ? AND ValueCount2 = ? AND FileNo = ?',
                  [ValueCountList.Field(0), ValueCountList.Field(1), FileNo]
                );
                CacheFileRec2.FillPrepare(
                  Database,
                  'ValueCount = ? AND ValueCount2 = ? AND FileNo = ?',
                  [ValueCountList.Field(0), ValueCountList.Field(1), FileNo2]
                );
                //循环比较两组文件列表的每一行记录，直到其中一组没有
                CompareFileTypes := [cftOpenFile, cftOpenFile2, cftOpenFile3];
                repeat
                  CheckStop;

                  if cftOpenFile in CompareFileTypes then
                  begin
                    if CacheFileRec.FillOne then
                    begin
                      Database2 := TSQLRestServerDB.Create(Model2, CacheFileRec.FileName);
                      DataRec.FillPrepare(Database2, '', []);
                      DataRec.FillOne;
                    end
                    else DataRec.Values := [];
                    CompareFileTypes := CompareFileTypes - [cftOpenFile];
                  end;
                  if cftOpenFile2 in CompareFileTypes then
                  begin
                    if CacheFileRec2.FillOne then
                    begin
                      Database3 := TSQLRestServerDB.Create(Model2, CacheFileRec2.FileName);
                      DataRec2.FillPrepare(Database3, '', []);
                      DataRec2.FillOne;
                    end
                    else DataRec2.Values := [];
                    CompareFileTypes := CompareFileTypes - [cftOpenFile2];
                  end;
                  if cftOpenFile3 in CompareFileTypes then
                  begin
                    DBFileName := BuildFileName(0);
                    Database4 := TSQLRestServerDB.Create(Model2, DBFileName);
                    CompareFileTypes := CompareFileTypes - [cftOpenFile3];
                  end;

                  v := DataRec.Values;
                  v2 := DataRec2.Values;
                  if NeedExchange(v, v2) then
                  begin
                    if Database4.Add(DataRec2, True) = 0 then
                      raise Exception.Create('添加缓存记录失败');
                    if not DataRec2.FillOne then
                    begin
                      FreeAndNil(Database3);

                      CacheFile.Id := CacheFileRec2.ID;
                      CacheFile.FileName := CacheFileRec2.FileName;
                      SetLength(CacheFiles, Length(CacheFiles) + 1);
                      CacheFiles[Length(CacheFiles) - 1] := CacheFile;

                      CompareFileTypes := CompareFileTypes + [cftOpenFile2];
                    end;
                  end
                  else
                  begin
                    if Database4.Add(DataRec, True) = 0 then
                      raise Exception.Create('添加缓存记录失败');
                    if not DataRec.FillOne then
                    begin
                      FreeAndNil(Database2);

                      CacheFile.Id := CacheFileRec.ID;
                      CacheFile.FileName := CacheFileRec.FileName;
                      SetLength(CacheFiles, Length(CacheFiles) + 1);
                      CacheFiles[Length(CacheFiles) - 1] := CacheFile;

                      CompareFileTypes := CompareFileTypes + [cftOpenFile];
                    end;
                  end;
                  CacheFileRec3.RowCount := CacheFileRec3.RowCount + 1;
                  if (CacheFileRec3.RowCount >= 100000)
                    or ((Length(v) = 0) and (Length(v2) = 0))
                  then
                  begin
                    if Assigned(Database2) then
                    begin
                      Database2.TransactionBegin(TSQLData);
                      Database2.Delete(TSQLData, 'Id < ?', [DataRec.ID]);
                      CacheFileRec.RowCount := Database2.TableRowCount(TSQLData);
                    end;
                    if Assigned(Database3) then
                    begin
                      Database3.TransactionBegin(TSQLData);
                      Database3.Delete(TSQLData, 'Id < ?', [DataRec2.ID]);
                      CacheFileRec2.RowCount := Database3.TableRowCount(TSQLData);
                    end;

                    Inc(MaxFileNo);
                    CacheFileRec3.FileNo := MaxFileNo;
                    CacheFileRec3.FileName := DBFileName;
                    CacheFileRec3.RowCount := Database4.TableRowCount(TSQLData);
                    CacheFileRec3.ValueCount := ValueCountList.Field(0);
                    CacheFileRec3.ValueCount2 := ValueCountList.Field(1);
                    FreeAndNil(Database4);

                    Database.TransactionBegin(TSQLCacheFile);
                    try
                      if not Database.Update(CacheFileRec) then
                        raise Exception.Create('更新缓存文件失败');
                      if not Database.Update(CacheFileRec2) then
                        raise Exception.Create('更新缓存文件失败');
                      if Database.Add(CacheFileRec3, True) = 0 then
                        raise Exception.Create('添加缓存文件失败');
                      for CacheFile in CacheFiles do
                        if not Database.Delete(TSQLCacheFile, CacheFile.Id) then
                          raise Exception.Create('删除缓存文件失败');
                      Database.Commit(1, True);
                      if Assigned(Database2) then Database2.Commit(1, True);
                      if Assigned(Database3) then Database3.Commit(1, True);
                    except
                      Database.RollBack;
                      if Assigned(Database2) then Database2.RollBack;
                      if Assigned(Database3) then Database3.RollBack;
                      raise;
                    end;
                    //删除缓存文件
                    for CacheFile in CacheFiles do
                    begin
                      try
                        if FileExists(CacheFile.FileName) then
                          DeleteFile(CacheFile.FileName);
                      except
                      end;
                    end;
                    SetLength(CacheFiles, 0);

                    CompareFileTypes := CompareFileTypes + [cftOpenFile3];
                  end;
                until ((Length(v) = 0) and (Length(v2) = 0));
              finally
                FileNoList.Free;
              end;
            until False;
          end;
        finally
          ValueCountList.Free;
        end;
      end;
    begin
      SortEachFile;
      SortGroupFile;

      SetClearColumnType(cctFinish);
    end;

  begin
    StartTime;
    try
      try
        repeat
          case fClearColumnType of
            cctInit: Init;
            cctInputClearColumn: InputClearColumn;
            cctInputData: InputData;
            cctClearColumn: ClearColumn;
            cctCombineFile: CombineFile;
            cctResetFileNo: ResetFileNo;
            cctGrouping: Grouping;
            cctSortRow: SortRow;
          end;
        until fClearColumnType = cctFinish;

        StopTime;
        frmMain.BringToFront;
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
      SetWorkType(wtBrowse);
    end;
  end).Start;
end;

procedure TdmProject2.ExportToFile;
var
  sField, ExportFilePath, sFieldValue: string;
  ColCount, ExportType, EachFileRowCount: Integer;
begin
  SetWorkType(wtClearColumnAndCombine);
  //fEachFileRowCount: Integer;
  //fExportFileDirectory: string;

  TTask.Create(procedure
  var
    s, FilePath, FileName: string;
    i, ValueIndex, ValueCount, FileSerialNo: Integer;
    FieldValue: Int64;
    ValueArr: array of string;

    function BuildFileName: string;
    var
      i, iEnd: Integer;
      sValue: string;
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

      //Result := Format('%d.%s、%d行', [FileSerialNo, sValue, FDQuery.RecordCount]);
    end;

    procedure ExportToTXT;
    var
      i: Integer;
      sValue: string;
    begin
      l.Clear;
      //FDQuery.First;
      //while not FDQuery.Eof do
      begin
        CheckStop;
        ValueIndex := 0;
        sValue := '';
        for i := 1 to ColCount do
        begin
          s := '';
          //FieldValue := FDQuery.FieldByName('Field' + IntToStr(Ceil(i / 64))).AsLargeInt;
          if FieldValue = i64 shl (64 - i) or FieldValue then
          begin
            s := IntToStr(i);
            if i < 10 then s := '0' + s;

            if sValue <> '' then sValue := sValue + '、';
            sValue := sValue + s;
            ValueArr[ValueIndex] := s;

            Inc(ValueIndex);
          end;
        end;
        //l.Values[FDQuery.RecNo.ToString] := sValue;

        //if FDQuery.RecNo = 1 then FileName := FilePath + BuildFileName + '.txt';

        //FDQuery.Next;
      end;
      l.SaveToFile(FileName);
    end;
  begin
    //SetWorkState(wsExportToFile);
    StartTime;
    try
      try
        FLog.Log('正在获取导出文件列表，请稍等...');
        s := 'WITH CTE AS (' + #$D#$A
          + 'SELECT ValueCount, FolderNo, FileNo' + #$D#$A
          + 'FROM ResultData' + #$D#$A
          + 'GROUP BY ValueCount, FolderNo, FileNo' + #$D#$A
          + '),' + #$D#$A

          + 'CTE2 AS (' + #$D#$A
          + 'SELECT ValueCount, Count(ValueCount) ValueRowCount' + #$D#$A
          + 'FROM ResultData GROUP BY ValueCount' + #$D#$A
          + '),' + #$D#$A

          + 'CTE3 AS (' + #$D#$A
          + 'SELECT ValueCount, %s FROM ResultData' + #$D#$A
          + 'WHERE FileNo = 1 AND RowNo = 1' + #$D#$A
          + ')' + #$D#$A

          + 'SELECT CTE.FolderNo, CTE.FileNo, CTE2.ValueRowCount, CTE3.*' + #$D#$A
          + 'FROM CTE' + #$D#$A
          + 'LEFT JOIN CTE2 ON CTE2.ValueCount = CTE.ValueCount' + #$D#$A
          + 'LEFT JOIN CTE3 ON CTE3.ValueCount = CTE.ValueCount' + #$D#$A
          + 'ORDER BY CTE.FolderNo, CTE.FileNo';
        s := Format(s, [sField]);
        {//FDQuery.Open(s);
        //with FDMemTable do
        begin
          //FDMemTable.Close;
          //Data := FDQuery.Data;
          //First;
          while not Eof do
          begin
            CheckStop;
            //ValueCount := FieldByName('ValueCount').AsInteger;
            //FileSerialNo := FieldByName('FileNo').AsInteger;
            FLog.Log(Format('正在导出列[%d]文件[%d]，请稍等...', [ValueCount, FileSerialNo]));
            //生成子目录名
            sFieldValue := '';
            for i := 1 to ColCount do
            begin
              FieldValue := FieldByName('Field' + IntToStr(Ceil(i / 64))).AsLargeInt;
              if FieldValue = i64 shl (64 - i) or FieldValue then
              begin
                sFieldValue := i.ToString + '-';
                if i < 10 then sFieldValue := '0' + sFieldValue;
                Break;
              end;
            end;
            for i := ColCount downto 1 do
            begin
              FieldValue := FieldByName('Field' + IntToStr(Ceil(i / 64))).AsLargeInt;
              if FieldValue = i64 shl (64 - i) or FieldValue then
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
            if not DirectoryExists(FilePath) then CreateDir(FilePath);
            SetLength(ValueArr, ValueCount);

            s := 'SELECT %s FROM ResultData' + #$D#$A
              + 'WHERE ValueCount = %d AND FileNo = %d';
            s := Format(s, [sField, ValueCount, FileSerialNo]);
            FDQuery.Open(s);

            case ExportType of
              1: ExportToExcel
              else ExportToTXT;
            end;

            FLog.Log(Format('导出列[%d]文件[%d]完成', [ValueCount, FileSerialNo]));

            Next;
          end;
        end;
        StopTime; }

        FLog.Log('导出文件完成');
        ShowMessage('导出文件完成');
      except
        on e: Exception do
        begin
          e.Message := '导出文件失败：' + e.Message;
          FLog.Log(e.Message);
          if e.Message.IndexOf('aborted') = -1 then ShowMessage(e.Message);
        end;
      end;
    finally
      StopTime;
      //SetWorkState(wsNone);
    end;
  end).Start;
end;

end.

