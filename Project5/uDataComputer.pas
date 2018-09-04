unit uDataComputer;

interface

uses
  System.Classes,
  System.Variants,
  System.SysUtils,
  System.Types,
  System.IOUtils,
  System.Threading,
  System.Generics.Collections,
  Data.DB,
  mORMot,
  mORMotSQLite3,
  SynSQLite3Static,
  uCommon;

type
  TSQLClearColumnRow = class(TSQLData)
  private
    fFileNo: Word;
    fNumber: Cardinal;
  published
    property FileNo: Word read fFileNo write fFileNo;
    property Number: Cardinal read fNumber write fNumber;
  end;

  TSQLResultData = class(TSQLData)
  private
    fFolderNo: Word;
    fFileNo: Word;
    fRowNo: Cardinal;
    fConformColCount: Word;
    fConformColCount2: Word;
  published
    property FolderNo: Word read fFolderNo write fFolderNo;
    property FileNo: Word read fFileNo write fFileNo;
    property RowNo: Cardinal read fRowNo write fRowNo;
    property ConformColCount: Word read fConformColCount write fConformColCount;
    property ConformColCount2: Word read fConformColCount2 write fConformColCount2;
  end;

  TDataComputer = class
  private
    fModel: TSQLModel;
    fDatabase: TSQLRestServerDB;
    fIntervalValues: TWordDynArray;

    procedure LoadData(FileNo: Word; FileName: string; Row: TSQLClearColumnRow;
      IntervalValues: TWordDynArray = []);
  public
    constructor Create;
    destructor Destroy;

    procedure SortClearColumn(Files: TStrings);
    procedure RearrangeClearColumn(Files: TStrings; ReverseOrder: Boolean);
    procedure CompareClearColumn(FileDirectory: string);
    procedure QueryData3(DataSet: TDataSet; FileName: string;
      IntervalValues: TWordDynArray; IdenticalColCount, CompareRowCount: Cardinal);
    procedure BuildClearColumn(FileName: string; aIntervalValues: TWordDynArray);

    procedure SetIntervalValues(aIntervalValues: TWordDynArray);
    procedure CheckIntervalValues;
    procedure InputResultData(FileDirectory: string);
    procedure DeleteSameColumnData(FileName: string; SameColumnCounts: TWordDynArray);
    procedure ExportToFile(FileDirectory: string; EachFileRowCount: Cardinal;
      SplitSubFileDirectory: Boolean);
    procedure CalcConformColumnCount(Cols: TWordDynArray);
    function GetConformRowCount: Cardinal;
    procedure QueryData2(DataSet: TDataSet; EachPageRowCount: Cardinal);

    property IntervalValues: TWordDynArray read fIntervalValues;
  end;

implementation

procedure TDataComputer.LoadData(FileNo: Word; FileName: string;
  Row: TSQLClearColumnRow; IntervalValues: TWordDynArray);
var
  l: TStringList;
  i, RowIndex, Digit: Integer;
  s: string;
begin
  if not TFile.Exists(FileName) then Exit;
  l := TStringList.Create;
  try
    l.LoadFromFile(FileName);
    fDatabase.TransactionBegin(TSQLRow);
    try
      for i := l.Count - 1 downto 0 do
      begin
        if not TryStrToInt(l.Names[i].Trim, Digit) then Continue;
        s := l.ValueFromIndex[i];
        Row.FileNo := FileNo;
        Row.Number := Digit;
        Row.ClearValue;
        Row.AssignValue(s, IntervalValues);
        if fDatabase.Add(Row, True) = 0 then
          raise Exception.Create('Add row failed');
      end;
      fDatabase.Commit(1, True);
    except
      on e: Exception do
      begin
        fDatabase.RollBack;
        raise Exception.Create(e.Message);
      end;
    end;
  finally
    l.Free;
  end;
end;

constructor TDataComputer.Create;
begin
  inherited Create;
  fModel := TSQLModel.Create([TSQLResultData, TSQLClearColumnRow]);
  fDatabase := TSQLRestServerDB.Create(fModel);
  fDatabase.CreateMissingTables;
  fDatabase.CreateSQLIndex(TSQLResultData, 'FolderNo', False);
  fDatabase.CreateSQLIndex(TSQLResultData, 'FileNo', False);
  fIntervalValues := [];
end;

destructor TDataComputer.Destroy;
begin
  fDatabase.Free;
  fModel.Free;
  inherited Destroy;
end;

procedure TDataComputer.SortClearColumn(Files: TStrings);
var
  l: TStringList;
  FileName, s: string;
  i: Integer;
begin
  l := TStringList.Create;
  try
    for FileName in Files do
    begin
      l.LoadFromFile(FileName);
      for i := l.Count - 1 downto 0 do
      begin
        if l[i].Trim.IsEmpty then l.Delete(i)
        else l[i] := Format('%s=%s', [l.Names[i].Trim, l.ValueFromIndex[i].Trim]);
      end;

      for i := 0 to l.Count div 2 - 1 do
      begin
        s := l[i];
        l[i] := l[l.Count - i - 1];
        l[l.Count - i - 1] := s;
      end;

      l.SaveToFile(FileName);
    end;
  finally
    l.Free;
  end;
end;

procedure TDataComputer.RearrangeClearColumn(Files: TStrings; ReverseOrder: Boolean);
var
  l: TStringList;
  FileName, s: string;
  Data: TSQLData;
  i: Integer;
begin
  TSQLData.AutoFree(Data);
  l := TStringList.Create;
  try
    for FileName in Files do
    begin
      l.LoadFromFile(FileName);
      for i := l.Count - 1 downto 0 do
        if l[i].Trim.IsEmpty then l.Delete(i);

      //值排序
      for i := 0 to l.Count - 1 do
      begin
        s := l.ValueFromIndex[i];
        Data.AssignValue(s);
        s := Data.ToString;

        l.ValueFromIndex[i] := s;
      end;
      //插入倒序行号
      for i := 0 to l.Count - 1 do
        l[i] := Format('%s(%d)=%s', [l.Names[i], l.Count - i, l.ValueFromIndex[i].Trim]);
      l.SaveToFile(FileName.Replace('.txt', '副本.txt'));
      //设置行号
      for i := 0 to l.Count - 1 do
      begin
        s := '%d=%s';
        if ReverseOrder then
          s := Format(s, [l.Count - i, l.ValueFromIndex[i]])
        else
          s := Format(s, [i + 1, l.ValueFromIndex[i]]);
        l[i] := s;
      end;
      l.SaveToFile(FileName);
    end;
  finally
    l.Free;
  end;
end;

procedure TDataComputer.CompareClearColumn(FileDirectory: string);
var
  l: TStringList;
  Row: TSQLClearColumnRow;
  Rows: array of TSQLClearColumnRow;
  Table: TSQLTableJSON;
  s, sValue, sValue2, FileName: string;
  FileNo: Word;
  i, MaxRowNumber, IntervalIndex, ValueIndex, RowIndex, IntervalCount: Integer;
  AllConform: Boolean;
begin
  if not FileDirectory.Substring(FileName.Length - 1).Equals('\') then
    FileDirectory := FileDirectory + '\';

  TSQLClearColumnRow.AutoFree(Row);
  l := TStringList.Create;
  try
    FileNo := 0;
    for FileName in TDirectory.GetFiles(FileDirectory, '*.txt') do
    begin
      if TPath.GetFileName(FileName).Equals('比较结果.txt') then Continue;

      FileNo := FileNo + 1;
      LoadData(FileNo, FileName, Row);
    end;
    FileName := FileDirectory + '比较结果.txt';

    Table := fDatabase.ExecuteList([TSQLClearColumnRow], 'SELECT Max(Number) FROM ClearColumnRow');
    MaxRowNumber := Table.GetAsInt64(1, 0);
    if Assigned(Table) then FreeAndNil(Table);

    for i := 1 to MaxRowNumber do
    begin
      Row.FillPrepare(fDatabase, 'Number = ? ORDER BY FileNo', [i]);
      if Row.FillTable.RowCount = 0 then Continue;

      for RowIndex := Low(Rows) to High(Rows) do Rows[RowIndex] := nil;
      SetLength(Rows, Row.FillTable.RowCount - 1);
      for RowIndex := Low(Rows) to High(Rows) do TSQLClearColumnRow.AutoFree(Rows[RowIndex]);

      Row.FillOne;
      IntervalCount := Length(Row.IntervalValues);
      for RowIndex := Low(Rows) to High(Rows) do
      begin
        Row.FillRow(RowIndex + 2, Rows[RowIndex]);
        if Length(Rows[RowIndex].IntervalValues) > IntervalCount then
          IntervalCount := Length(Rows[RowIndex].IntervalValues);
      end;

      for IntervalIndex := Low(Row.IntervalValues) to High(Row.IntervalValues) do
      begin
        for RowIndex := Low(Rows) to High(Rows) do
        begin
          AllConform := IntervalIndex <= High(Rows[RowIndex].IntervalValues);
          if not AllConform then Break;
        end;
        if not AllConform then Break;

        repeat
          AllConform := Row.IntervalValueCounts[0] > 0;
          if AllConform then
            for RowIndex := Low(Rows) to High(Rows) do
            begin
              AllConform := Rows[RowIndex].IntervalValueCounts[0] > 0;
              if not AllConform then Break;
            end;
          if not AllConform then Break;

          for RowIndex := Low(Rows) to High(Rows) do
          begin
            AllConform := Row.Value(IntervalIndex, 0) = Rows[RowIndex].Value(IntervalIndex, 0);
            if not AllConform then Break;
          end;
          if AllConform then
          begin
            Row.DeleteValueByIndex(IntervalIndex, 0);
            for RowIndex := Low(Rows) to High(Rows) do
              Rows[RowIndex].DeleteValueByIndex(IntervalIndex, 0);
          end;
        until not AllConform;
      end;

      AllConform := True;
      sValue := Row.ToString;
      for RowIndex := Low(Rows) to High(Rows) do
      begin
        sValue2 := Rows[RowIndex].ToString;
        AllConform := sValue = sValue2;
        if not AllConform then Break;
      end;
      if AllConform then Continue;

      s := Format('%d=%s', [i, sValue]);
      l.Add(s);
      for RowIndex := Low(Rows) to High(Rows) do
      begin
        sValue := Rows[RowIndex].ToString;
        s := Format('%d=%s', [i, sValue]);
        l.Add(s);
      end;
    end;

    l.SaveToFile(FileName);
  finally
    fDatabase.Delete(TSQLClearColumnRow, '');
    if Assigned(Table) then Table.Free;
    l.Free;
  end;
end;

procedure TDataComputer.QueryData3(DataSet: TDataSet; FileName: string;
  IntervalValues: TWordDynArray; IdenticalColCount, CompareRowCount: Cardinal);
var
  Row, Row2: TSQLClearColumnRow;
  i, ConformCount, ConformCount2, ConformCount3: Integer;
  f: TField;

  procedure Compare(IdenticalColCount: Integer; var ConformCount, ConformCount2, ConformCount3: Integer);
  var
    i, FindCount, FindCount2, RowNo: Integer;
    v: Word;
    r, r2: TWordDynArray;
  begin
    ConformCount := 0;
    ConformCount2 := 0;
    ConformCount3 := 0;
    for i := 1 to CompareRowCount do
    begin
      Row.FillRow(Row.FillCurrentRow - 1 + i, Row2);
      FindCount := 0;
      for v in Row.Values(0) do if Row2.ValueExist(v, 0) then Inc(FindCount);
      r := Row.Values(0);
      r2 := Row2.Values(0);

      FindCount2 := 0;
      if (Length(Row.IntervalValues) > 1) and (Length(Row2.IntervalValues) > 1) then
        for v in Row.Values(1) do if Row2.ValueExist(v, 1) then Inc(FindCount2);

      r := Row.Values(1);
      r2 := Row2.Values(1);

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
  TSQLClearColumnRow.AutoFree(Row);
  TSQLClearColumnRow.AutoFree(Row2);
  LoadData(1, FileName, Row, IntervalValues);
  DataSet.DisableControls;
  try
    DataSet.First;
    while not DataSet.Eof do DataSet.Delete;

    Row.FillPrepare(fDatabase, 'ORDER BY Number', []);
    while Row.FillOne do
    begin
      if Row.FillCurrentRow - 1 + CompareRowCount > Row.FillTable.RowCount then Break;

      Compare(IdenticalColCount, ConformCount, ConformCount2, ConformCount3);
      if ConformCount > 0 then
      begin
        if DataSet.Locate('ConformCount', ConformCount, []) then
        begin
          DataSet.Edit;
          f := DataSet.FieldByName('OccurrenceCount');
          f.AsInteger := f.AsInteger + 1;
          DataSet.Post;
        end
        else
        begin
          DataSet.Append;
          DataSet.FieldByName('ConformCount').AsInteger := ConformCount;
          DataSet.FieldByName('OccurrenceCount').AsInteger := 1;
          DataSet.Post;
        end;
      end;

      if (ConformCount2 > 0) and (Length(IntervalValues) > 1) then
      begin
        if DataSet.Locate('ConformCount2', ConformCount2, []) then
        begin
          DataSet.Edit;
          f := DataSet.FieldByName('OccurrenceCount2');
          f.AsInteger := f.AsInteger + 1;
          DataSet.Post;
        end
        else
        begin
          DataSet.Append;
          DataSet.FieldByName('ConformCount2').AsInteger := ConformCount2;
          DataSet.FieldByName('OccurrenceCount2').AsInteger := 1;
          DataSet.Post;
        end;
      end;

      if (ConformCount3 > 0) and (Length(IntervalValues) > 1) then
      begin
        if DataSet.Locate('ConformCount3', ConformCount3, []) then
        begin
          DataSet.Edit;
          f := DataSet.FieldByName('OccurrenceCount3');
          f.AsInteger := f.AsInteger + 1;
          DataSet.Post;
        end
        else
        begin
          DataSet.Append;
          DataSet.FieldByName('ConformCount3').AsInteger := ConformCount3;
          DataSet.FieldByName('OccurrenceCount3').AsInteger := 1;
          DataSet.Post;
        end;
      end;
    end;
    DataSet.First;
  finally
    DataSet.EnableControls;
    fDatabase.Delete(TSQLClearColumnRow, '');
  end;
end;

procedure TDataComputer.BuildClearColumn(FileName: string; aIntervalValues: TWordDynArray);
var
  l: TStringList;
  l2: TStringList;
  Data: TSQLData;
  s, sSerialNo, SaveFileName: string;
  i, StartIndex, EndIndex, RowNo, StartSerialNo: Integer;
  v, IntervalValue: Word;
begin
  s := TPath.GetFileName(FileName);
  StartIndex := s.IndexOf('(');
  EndIndex := s.IndexOf(')');
  sSerialNo := s.Substring(StartIndex + 1, EndIndex - StartIndex - 1);
  if (StartIndex = -1)
    or (EndIndex = -1)
    or (StartIndex > EndIndex)
    or not TryStrToInt(sSerialNo, StartSerialNo)
  then raise Exception.Create('文件名格式无效');

  SaveFileName := ExtractFilePath(FileName) + s.Substring(0, StartIndex + 1) + '%d' + s.Substring(EndIndex, s.Length);

  IntervalValue := 0;
  for i := Low(aIntervalValues) to High(aIntervalValues) do
    IntervalValue := IntervalValue + aIntervalValues[i];

  TSQLData.AutoFree(Data);
  l := TStringList.Create;
  try
    l.LoadFromFile(FileName);
    for i := l.Count - 1 downto 0 do  if l.Names[i].IsEmpty then l.Delete(i);
    for i := 0 to l.Count - 1 do
    begin
      s := l.ValueFromIndex[i];
      Data.AssignValue(s, aIntervalValues);

      for v := 1 to IntervalValue do
        if Data.ValueExist(v) then Data.DeleteValue(v) else Data.AddValue(v);

      s := Data.ToString;
      l.ValueFromIndex[i] := s;
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
  finally
    l.Free;
  end;
end;

procedure TDataComputer.SetIntervalValues(aIntervalValues: TWordDynArray);
begin
  fIntervalValues := aIntervalValues;
end;

procedure TDataComputer.CheckIntervalValues;
begin
  if Length(fIntervalValues) = 0 then raise Exception.Create('请先设置列数');
end;

procedure TDataComputer.InputResultData(FileDirectory: string);
var
  ResultData: TSQLResultData;
  l: TStringList;
  i, FolderNo, FileNo, RowNo: Integer;
  s, SubFileDirectory, FileName: string;
  SubFileDirectories: TStringDynArray;
begin
  CheckIntervalValues;

  fDatabase.Delete(TSQLResultData, '');
  TSQLResultData.AutoFree(ResultData);
  l := TStringList.Create;
  try
    //获取子目录
    SubFileDirectories := TDirectory.GetDirectories(FileDirectory);
    if Length(SubFileDirectories) = 0 then
    begin
      SetLength(SubFileDirectories, 1);
      SubFileDirectories[0] := FileDirectory;
    end;

    for SubFileDirectory in SubFileDirectories do
    begin
      s := ExtractFileName(SubFileDirectory);
      if not TryStrToInt(s.Substring(0, s.IndexOf('.')), FolderNo) then Continue;
      //获取子目录文件
      for FileName in TDirectory.GetFiles(SubFileDirectory, '*.txt', TSearchOption.soAllDirectories) do
      begin
        s := TPath.GetFileNameWithoutExtension(FileName);
        FileNo := 0;
        if not TryStrToInt(s.Substring(0, s.IndexOf('.')), FileNo)
          and s.Contains('- ')
        then
        begin
          s := s.Substring(s.IndexOf('- ') + 2);
          TryStrToInt(s, FileNo);
        end;

        l.LoadFromFile(FileName);
        for i := l.Count - 1 downto 0 do
          if l[i].Trim.IsEmpty then l.Delete(i);

        fDatabase.TransactionBegin(TSQLResultData);
        try
          for i := 0 to l.Count - 1 do
          begin
            if not TryStrToInt(l.Names[i].Trim, RowNo) then Continue;
            s := l.ValueFromIndex[i].Trim;
            ResultData.AssignValue(s, fIntervalValues);
            ResultData.FolderNo := FolderNo;
            ResultData.FileNo := FileNo;
            ResultData.RowNo := RowNo;
            ResultData.ConformColCount := 0;
            ResultData.ConformColCount2 := 0;
            if fDatabase.Add(ResultData, True) = 0 then
              raise Exception.CreateFmt('Input error: %d %d %d %s', [FolderNo, FileNo, RowNo, s]);
          end;

          fDatabase.Commit(1, True);
        except
          fDatabase.RollBack;
          raise;
        end;
      end;
    end;
  finally
    l.Free;
  end;
end;

procedure TDataComputer.DeleteSameColumnData(FileName: string; SameColumnCounts: TWordDynArray);
var
  ResultData: TSQLResultData;
  Data: TSQLData;
  l: TStringList;
  v, SameColumnCount: Word;
  i, IntervalIndex: Integer;
  s: string;
  IsConform: Boolean;
begin
  CheckIntervalValues;

  l := TStringList.Create;
  try
    l.LoadFromFile(FileName);

    TSQLData.AutoFree(Data);
    TSQLResultData.AutoFree(ResultData, fDatabase, '', []);
    fDatabase.TransactionBegin(TSQLResultData);
    try
      while ResultData.FillOne do
      begin
        s := ResultData.ToString;
        IsConform := False;
        for i := 0 to l.Count - 1 do
        begin
          s := l.ValueFromIndex[i];
          Data.AssignValue(s, fIntervalValues);
          for IntervalIndex := Low(fIntervalValues) to High(fIntervalValues) do
          begin
            SameColumnCount := 0;
            for v in Data.Values(IntervalIndex) do
            begin
              if ResultData.ValueExist(v, IntervalIndex) then
                SameColumnCount := SameColumnCount + 1;
            end;
            IsConform := SameColumnCount > SameColumnCounts[IntervalIndex];
            if IsConform then Break;
          end;
          if IsConform then Break;
        end;
        if IsConform then
          fDatabase.Delete(TSQLResultData, ResultData.IDValue);
      end;

      fDatabase.Commit(1, True);
    except
      fDatabase.RollBack;
      raise;
    end;
  finally
    l.Free;
  end;
end;

procedure TDataComputer.ExportToFile(FileDirectory: string; EachFileRowCount: Cardinal;
  SplitSubFileDirectory: Boolean);
var
  l: TStringList;
  ResultData: TSQLResultData;
  Table: TSQLTableJSON;
  s, SubFileDirectory, FileName, sValue: string;
  v, ValueCount, ValueCount2: Word;
  RowCount, FileNo: Cardinal;
  i, FileIndex, iValueCount: Integer;
begin
  CheckIntervalValues;

  if not FileDirectory.Substring(FileDirectory.Length - 1, 1).Equals('\') then
    FileDirectory := FileDirectory + '\';

  TSQLResultData.AutoFree(ResultData);

  s := 'SELECT ValueCount, ValueCount2, Count(Id) RowCount' + #$D#$A
    + 'FROM ResultData' + #$D#$A
    + 'GROUP BY ValueCount, ValueCount2' + #$D#$A
    + 'ORDER BY ValueCount + ValueCount2, ValueCount, ValueCount2';
  Table := fDatabase.ExecuteList([TSQLResultData], s);
  l := TStringList.Create;
  try
    FileNo := 0;
    while Table.Step do
    begin
      ValueCount := Table.FieldAsInteger('ValueCount');
      ValueCount2 := Table.FieldAsInteger('ValueCount2');
      RowCount := Table.FieldAsInteger('RowCount');
      if SplitSubFileDirectory then
      begin
        //生成子目录
        if Length(fIntervalValues) = 1 then
        begin
          ResultData.FillPrepare(fDatabase, 'ValueCount = ? AND ValueCount2 = ? LIMIT 1', [ValueCount, ValueCount2]);
          ResultData.FillOne;
          v := ResultData.Values[Low(ResultData.Values)];
          sValue := v.ToString;
          if v < 10 then sValue := '0' + sValue;
          sValue := sValue + '-';
          v := ResultData.Values[High(ResultData.Values)];
          if v < 10 then sValue := sValue + '0' + v.ToString
          else sValue := sValue + v.ToString;

          SubFileDirectory := FileDirectory + Format('%d.首行（首尾）列数字%s、%d列、%d行\', [
            Table.StepRow, sValue, ValueCount, RowCount
          ]);
        end
        else
        begin
          SubFileDirectory := FileDirectory + Format('%d.列数（1-%d）%d列+（1-%d）%d列= %d列、%d行\', [
            Table.StepRow, fIntervalValues[0], ValueCount, fIntervalValues[1],
            ValueCount2, ValueCount + ValueCount2, RowCount
          ]);
        end;
        if not TDirectory.Exists(SubFileDirectory) then TDirectory.CreateDirectory(SubFileDirectory);
      end
      else SubFileDirectory := FileDirectory;
      //生成文件
      FileIndex := 0;
      repeat
        l.Clear;
        ResultData.FillPrepare(fDatabase, 'ValueCount = ? AND ValueCount2 = ? LIMIT ? OFFSET ?',
          [ValueCount, ValueCount2, EachFileRowCount, EachFileRowCount * FileIndex]);
        while ResultData.FillOne do
        begin
          //生成文件名
          if ResultData.FillCurrentRow = 2 then
          begin
            sValue := '';
            if Length(fIntervalValues) = 1 then
            begin
              //取第一行前后各5个值
              iValueCount := 5;
              if ResultData.ValueCount < 10 then iValueCount := ResultData.ValueCount div 2;
              for i := 0 to iValueCount - 1 do
              begin
                if i > 0 then sValue := sValue + '、';
                sValue := sValue + ResultData.Values[i].ToString;
              end;
              sValue := '（' + sValue + '）（';
              if ResultData.ValueCount < 10 then iValueCount := ResultData.ValueCount - iValueCount;
              for i := iValueCount downto 1 do
              begin
                if i < iValueCount then sValue := sValue + '、';
                sValue := sValue + ResultData.Values[ResultData.ValueCount - i].ToString;
              end;
              sValue := sValue + '）';
            end
            else
            begin
              //取第一行各个区域前后各2个值
              iValueCount := 2;
              if ResultData.ValueCount < 4 then iValueCount := ResultData.ValueCount div 2;
              for i := 0 to iValueCount - 1 do
              begin
                if i > 0 then sValue := sValue + '、';
                sValue := sValue + ResultData.Values[i].ToString;
              end;
              if ResultData.ValueCount < 4 then iValueCount := ResultData.ValueCount - iValueCount;
              for i := iValueCount downto 1 do
                sValue := sValue + '、' + ResultData.Values[ResultData.ValueCount - i].ToString;
              sValue := '（' + sValue + '）（';

              iValueCount := 2;
              if ResultData.ValueCount2 < 4 then iValueCount := ResultData.ValueCount2 div 2;
              for i := 0 to iValueCount - 1 do
              begin
                if i > 0 then sValue := sValue + '、';
                sValue := sValue + ResultData.Value(1, i).ToString;
              end;
              if ResultData.ValueCount2 < 4 then iValueCount := ResultData.ValueCount2 - iValueCount;
              for i := iValueCount downto 1 do
                sValue := sValue + '、' + ResultData.Value(1, ResultData.ValueCount2 - i).ToString;
              sValue := sValue + '）';
            end;

            if SplitSubFileDirectory then FileNo := FileIndex + 1
            else FileNo := FileNo + 1;
            FileName := SubFileDirectory + Format('%d.%s、%d行.txt', [FileNo, sValue, ResultData.FillTable.RowCount]);
          end;
          s := Format('%d=%s', [ResultData.FillCurrentRow - 1, ResultData.ToString]);
          l.Add(s);
        end;
        if l.Count > 0 then l.SaveToFile(FileName);
        Inc(FileIndex);
      until ResultData.FillTable.RowCount = 0;
    end;
  finally
    l.Free;
    Table.Free;
  end;
end;

procedure TDataComputer.CalcConformColumnCount(Cols: TWordDynArray);
var
  ResultData: TSQLResultData;
  v: Word;
  IntervalIndex: Integer;
begin
  TSQLResultData.AutoFree(ResultData);
  ResultData.FillPrepare(fDatabase, '', []);
  fDatabase.TransactionBegin(TSQLResultData);
  try
    while ResultData.FillOne do
    begin
      ResultData.ConformColCount := 0;
      ResultData.ConformColCount2 := 0;
      for v in Cols do
      begin
        if ResultData.ValueExist(v) then
        begin
          IntervalIndex := ResultData.IntervalIndexOfValue(v);
          case IntervalIndex of
            0: ResultData.ConformColCount := ResultData.ConformColCount + 1;
            1: ResultData.ConformColCount2 := ResultData.ConformColCount2 + 1;
          end;
        end;
        if not fDatabase.Update(ResultData) then
          raise Exception.Create('Update Error');
      end;
    end;

    fDatabase.Commit(1, True);
  except
    fDatabase.RollBack;
  end;
end;

function TDataComputer.GetConformRowCount: Cardinal;
var
  s: string;
  Table: TSQLTableJSON;
begin
  try
    s := 'SELECT Count(Id) FROM ResultData WHERE ConformColCount > 0 OR ConformColCount2 > 0';
    Table := fDatabase.ExecuteList([TSQLResultData], s);
    Result := Table.GetAsInt64(1, 0);
  finally
    if Assigned(Table) then FreeAndNil(Table);
  end;
end;

procedure TDataComputer.QueryData2(DataSet: TDataSet; EachPageRowCount: Cardinal);
var
  ResultData: TSQLResultData;
  s, FieldName, FieldName2, sField, sWhere: string;
  i, ConformRowCount, v, ChosedCount: Integer;
  ColNo: Int64;
begin
  TSQLResultData.AutoFree(ResultData);

  s := 'ConformColCount > 0 OR ConformColCount2 > 0' + #$D#$A
    + 'ORDER BY ConformColCount DESC, ConformColCount2 DESC, FolderNo, FileNo, RowNo' + #$D#$A
    + 'LIMIT ?';
  ResultData.FillPrepare(fDatabase, s, [EachPageRowCount]);

  TThread.Synchronize(nil, procedure
  begin
    DataSet.DisableControls;
    try
      while not DataSet.Eof do DataSet.Delete;
      while ResultData.FillOne do
      begin
        DataSet.Append;
        DataSet.FieldByName('ValueCount').AsInteger := ResultData.ValueCount;
        DataSet.FieldByName('ValueCount2').AsInteger := ResultData.ValueCount2;
        DataSet.FieldByName('ConformColCount').AsInteger := ResultData.ConformColCount;
        DataSet.FieldByName('ConformColCount2').AsInteger := ResultData.ConformColCount2;
        DataSet.FieldByName('FolderNo').AsInteger := ResultData.FolderNo;
        DataSet.FieldByName('FileNo').AsInteger := ResultData.FileNo;
        DataSet.FieldByName('RowNo').AsInteger := ResultData.RowNo;
        DataSet.FieldByName('Field1').AsLargeInt := ResultData.Field1;
        DataSet.FieldByName('Field2').AsInteger := ResultData.Field2;
        DataSet.FieldByName('Field3').AsInteger := ResultData.Field3;
        DataSet.FieldByName('Field4').AsInteger := ResultData.Field4;
        DataSet.Post;
      end;
      DataSet.First;
    finally
      DataSet.EnableControls;
    end;
  end);
end;

end.
