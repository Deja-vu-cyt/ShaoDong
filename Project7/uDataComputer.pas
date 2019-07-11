unit uDataComputer;

interface

uses
  Vcl.Dialogs,
  SynCommons,
  mORMot,
  mORMotSQLite3,
  SynSQLite3,
  SynSQLite3Static,
  System.Classes,
  System.Variants,
  System.SysUtils,
  System.Types,
  System.IOUtils,
  System.Diagnostics,
  System.Math,
  uCommon,
  uFileWriter;

type
  TSQLSource = class(TSQLRow)
   protected
    fRowCount: Cardinal;
    fMaxValueCount: Word;
    fMaxValueCount2: Word;
    fRowSpacing: Cardinal;
    fValueCountGroup: RawUTF8;
  published
    property RowCount: Cardinal read fRowCount write fRowCount;
    property MaxValueCount: Word read fMaxValueCount write fMaxValueCount;
    property MaxValueCount2: Word read fMaxValueCount2 write fMaxValueCount2;
    property RowSpacing: Cardinal read fRowSpacing write fRowSpacing;
    property ValueCountGroup: RawUTF8 read fValueCountGroup write fValueCountGroup;
  end;

  TSQLCompare = class(TSQLRow);

  TSQLCompareData = class(TSQLData)
  protected
    fSourceNumber: Cardinal;
    fCompareNumber: Cardinal;
  published
    property SourceNumber: Cardinal read fSourceNumber write fSourceNumber;
    property CompareNumber: Cardinal read fCompareNumber write fCompareNumber;
  end;

  TSQLSourceGroup = class(TSQLRecord)
  protected
    fValue: RawUTF8;
    fMaxNumber: Cardinal;
    fMinNumber: Cardinal;
    fMaxRowSpacing: Cardinal;
    fDifferenceValue: Cardinal;
  published
    property Value: RawUTF8 read fValue write fValue;
    property MaxNumber: Cardinal read fMaxNumber write fMaxNumber;
    property MinNumber: Cardinal read fMinNumber write fMinNumber;
    property MaxRowSpacing: Cardinal read fMaxRowSpacing write fMaxRowSpacing;
    property DifferenceValue: Cardinal read fDifferenceValue write fDifferenceValue;
  end;

  TDataComputer = class(TThread)
  private
    fStopwatch: TStopwatch;
    fRest: TSQLRestServerDB;
    fDataMode: Byte;
    fMergeMode: Boolean;
    fMergeModeStr: string;
    fMaxSourceNumber: Cardinal;

    fIntervalValues: TWordDynArray;
    fSourceFileDirectory: string;
    fCompareFileDirectory: string;
    fCompareFileName: string;
    fExportDirectory: string;
    fSameValueCount: Word;
    fSameValueCount2: Word;
    fSameValueCount3: Word;
    fSameValueCount4: Word;
    fMinSameValueCount: Word;

    procedure SetExportDirectory(Value: string);

    procedure BuildRest;
    procedure LoadRow(Row: TSQLRow; FileName: string);
    procedure LoadSourceFile;
    procedure Compare;
    procedure BuildGroup;
    procedure BuildRowSpacing;
    procedure ExportFile;
    procedure ExportFile2;
    procedure ExportFile3;
    procedure ExportData; overload;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
  published
    property Stopwatch: TStopwatch read fStopwatch;
    property MergeMode: Boolean read fMergeMode write fMergeMode;
    property DataMode: Byte read fDataMode;
    property IntervalValues: TWordDynArray read fIntervalValues write fIntervalValues;
    property SourceFileDirectory: string read fSourceFileDirectory write fSourceFileDirectory;
    property CompareFileDirectory: string read fCompareFileDirectory write fCompareFileDirectory;
    property ExportDirectory: string read fExportDirectory write SetExportDirectory;
    property SameValueCount: Word read fSameValueCount write fSameValueCount;
    property SameValueCount2: Word read fSameValueCount2 write fSameValueCount2;
    property SameValueCount3: Word read fSameValueCount3 write fSameValueCount3;
    property SameValueCount4: Word read fSameValueCount4 write fSameValueCount4;
    property MinSameValueCount: Word read fMinSameValueCount write fMinSameValueCount;
  end;

var
  fDataComputer: TDataComputer;

  fKeyValue: TSQLKeyValue;

implementation

procedure TDataComputer.SetExportDirectory(Value: string);
begin
  fExportDirectory := Value.Trim;
  if not fExportDirectory.IsEmpty
    and not fExportDirectory.Substring(fExportDirectory.Length - 1).Equals('\')
  then fExportDirectory := fExportDirectory + '\';
end;

procedure TDataComputer.BuildRest;
begin
  fRest := TSQLRestServerDB.CreateWithOwnModel(
    [TSQLSource, TSQLCompare, TSQLCompareData, TSQLSourceGroup]
  );
  fRest.CreateMissingTables;
end;

procedure TDataComputer.LoadRow(Row: TSQLRow; FileName: string);

  function Extract(s: string): string;
  var
    i, iLeft: Integer;
  begin
    Result := s;
    for i := 1 to Length(s) do
    begin
      if s[i] = '（' then iLeft := i
      else if s[i] = '列' then
      begin
        Result := s.Substring(0, iLeft - 1);
        Break;
      end;
    end;
  end;

  function SpecialMode(s: string): Boolean;
  var
    HasLeft, HasLie: Boolean;
    c: Char;
  begin
    Result := False;
    HasLeft := False;
    HasLie := False;
    for c in s do
    begin
      case c of
        '（':
        begin
          HasLeft := True;
        end;
        '列':
        begin
          HasLie := True;
        end;
        '）':
        begin
          Result := HasLeft and not HasLie;
          if Result then Exit;
        end;
      end;
    end;
  end;

var
  i, j, Digit: Integer;
  s: string;
  SetDataMode: Boolean;
begin
  SetDataMode := Row.ClassType = TSQLSource;
  with TStringList.Create do
  begin
    try
      LoadFromFile(FileName);

      fRest.TransactionBegin(Row.RecordClass);
      try
        //fRest.Delete(Row.RecordClass, '');
        for i := Count - 1 downto 0 do
        begin
          if not TryStrToInt(Names[i].Trim, Digit) then Continue;
          Row.Number := Digit;
          s := Extract(ValueFromIndex[i]);

          //自动识别数据模式
          if SetDataMode then
          begin
            if SpecialMode(s) then fDataMode := 2;
            SetDataMode := False;
          end;

          Row.AssignValue(s, fIntervalValues, fDataMode);
          fRest.Add(Row, True);
        end;
        fRest.Commit(1, True);
      except
        on e: Exception do
        begin
          fRest.RollBack;
          raise;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TDataComputer.LoadSourceFile;
var
  Source: TSQLSource;
  FileName: string;
begin
  if Terminated then Exit;

  TSQLSource.AutoFree(Source);
  for FileName in TDirectory.GetFiles(fSourceFileDirectory, '*.txt') do
  begin
    if Terminated then Exit;

    LoadRow(Source, FileName);
  end;
  fMaxSourceNumber := StrToInt64(fRest.OneFieldValue(TSQLSource, 'Max(Number)', ''));
end;

procedure TDataComputer.Compare;
var
  Source: TSQLSource;
  Compare: TSQLCompare;
  CompareData: TSQLCompareData;
  v: Word;
  IsValid: Boolean;
begin
  if Terminated then Exit;

  TSQLSource.AutoFree(Source, fRest, '', []);
  TSQLCompare.AutoFree(Compare, fRest, 'ORDER BY Number DESC', []);
  TSQLCompareData.AutoFree(CompareData);
  CompareData.AssignValue('', fIntervalValues, fDataMode);

  fRest.TransactionBegin(TSQLCompareData);
  try
    fRest.Delete(TSQLCompareData, '');
    while Source.FillOne do
    begin
      Compare.FillRewind;
      while Compare.FillOne do
      begin
        CompareData.ClearValue;
        for v in Compare.Values do
          if Source.ValueExist(v) then CompareData.AddValue(v);
        CompareData.CalcValueCount;

        if fMergeMode then
          IsValid := ((CompareData.ValueCount >= fSameValueCount) and (CompareData.ValueCount <= fSameValueCount2)
          and (CompareData.ValueCount2 >= fSameValueCount3) and (CompareData.ValueCount2 <= fSameValueCount4))
          or ((fMinSameValueCount > 0) and (CompareData.ValueCount2 >= fMinSameValueCount))
        else
          IsValid := ((CompareData.ValueCount >= fSameValueCount) and (CompareData.ValueCount2 >= fSameValueCount3))
          or ((fMinSameValueCount > 0) and (CompareData.ValueCount2 >= fMinSameValueCount));

        if IsValid then
        begin
          CompareData.SourceNumber := Source.Number;
          CompareData.CompareNumber := Compare.Number;
          CompareData.CalcValueCount;
          fRest.Add(CompareData, True);
        end;
      end;
    end;
    Source.FillRewind;
    while Source.FillOne do
    begin
      Source.MaxValueCount := 0;
      Source.MaxValueCount2 := 0;
      CompareData.FillPrepare(fRest, 'SourceNumber = ? ORDER BY ValueCount DESC, ValueCount2 DESC, CompareNumber', [Source.Number]);
      if CompareData.FillOne then
      begin
        Source.MaxValueCount := CompareData.ValueCount;
        Source.MaxValueCount2 := CompareData.ValueCount2;
      end;
      Source.RowCount := CompareData.FillTable.RowCount;
      Source.ValueCountGroup := '';
      if not fMergeMode then
        Source.ValueCountGroup := Format('%d+%d', [Source.MaxValueCount, Source.MaxValueCount2]);

      fRest.Update(Source);
    end;

    if fMergeMode then
    begin
      Source.FillPrepare(fRest, 'ORDER BY MaxValueCount DESC, MaxValueCount2 DESC LIMIT 1', []);
      if Source.FillOne then
      begin
        Source.ValueCountGroup := Format('%d+%d', [Source.MaxValueCount, Source.MaxValueCount2]);
        fRest.Execute(Format('UPDATE Source SET ValueCountGroup = ''%s''', [Source.ValueCountGroup]))
      end;
    end;

    fRest.Commit(1, True);
  except
    fRest.RollBack;
    raise;
  end;
end;

procedure TDataComputer.BuildGroup;
var
  Group: TSQLSourceGroup;
  l: TSQLTableJSON;
begin
  if Terminated then Exit;

  fRest.Delete(TSQLSourceGroup, '');
  TSQLSourceGroup.AutoFree(Group);
  l := fRest.MultiFieldValues(TSQLSource, 'ValueCountGroup, Max(Number), Min(Number)',
    'RowCount > 0 GROUP BY ValueCountGroup ORDER BY MaxValueCount DESC, MaxValueCount2 DESC', []);
  try
    fRest.TransactionBegin(TSQLSourceGroup);
    try
      while l.Step do
      begin
        Group.Value := l.FieldAsRawUTF8(0);
        Group.MaxNumber := l.FieldAsInteger(1);
        Group.MinNumber := l.FieldAsInteger(2);
        fRest.Add(Group, True);
      end;
      fRest.Commit(1, True);
    except
      fRest.RollBack;
      raise;
    end;
  finally
    l.Free;
  end;
end;

procedure TDataComputer.BuildRowSpacing;
var
  Group: TSQLSourceGroup;
  Source, Source2: TSQLSource;
begin
  if Terminated then Exit;

  TSQLSourceGroup.AutoFree(Group, fRest, '', []);
  TSQLSource.AutoFree(Source);
  TSQLSource.AutoFree(Source2);
  fRest.TransactionBegin(TSQLSource);
  try
    while Group.FillOne do
    begin
      Group.MaxRowSpacing := 0;
      Source.FillPrepare(fRest, 'ValueCountGroup = ? AND RowCount > 0 ORDER BY Number DESC', [Group.Value]);
      while Source.FillOne do
      begin
        Source2.Number := 1;
        if Source.FillCurrentRow <= Source.FillTable.RowCount then
          Source.FillRow(Source.FillCurrentRow, Source2);

        Source.RowSpacing := Source.Number - Source2.Number;
        if Source.RowSpacing > Group.MaxRowSpacing then
          Group.MaxRowSpacing := Source.RowSpacing;

        fRest.Update(Source);
      end;

      fRest.Update(Group);
    end;

    Group.FillPrepare(fRest, '', []);
    while Group.FillOne do
    begin
      Group.DifferenceValue := fMaxSourceNumber + 1 - Group.MaxNumber - Group.MaxRowSpacing;
      fRest.Update(Group);
    end;

    fRest.Commit(1, True);
  except
    fRest.RollBack;
    raise;
  end;
end;

procedure TDataComputer.ExportFile;
var
  fr: TFileWriter;
  Group: TSQLSourceGroup;
  Source: TSQLSource;
  CompareData, CompareData2: TSQLCompareData;
  s, s2, FileName, sNumber, sCompareData, sCompareNumber: string;
begin
  if Terminated then Exit;

  FileName := fCompareFileName.Replace('.txt', '：' + fMergeModeStr + '导出结果 1 .txt');
  FileName := fExportDirectory + ExtractFileName(FileName);
  fr := TFileWriter.Create(FileName);
  try
    TSQLSourceGroup.AutoFree(Group);
    TSQLSource.AutoFree(Source);
    TSQLCompareData.AutoFree(CompareData);
    TSQLCompareData.AutoFree(CompareData2);

    Group.FillPrepare(fRest, '', []);
    while Group.FillOne and not Terminated do
    begin
      if Group.FillCurrentRow > 2 then
      begin
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn('');
      end;

      s := '[ 全部行的首行（ 第 %d 行 ）（ 加上 + ）1 ] = [ 第 %d 行 ]（ 减去 - ）[ 该组首行（ 第 %d 行 ）] =（ 行差 %d ）';
      s := Format(s, [fMaxSourceNumber, fMaxSourceNumber + 1, Group.MaxNumber, fMaxSourceNumber + 1 - Group.MaxNumber]);
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);

      Source.FillPrepare(fRest, 'ValueCountGroup = ? AND RowCount > 0 ORDER BY Number DESC', [Group.Value]);
      while Source.FillOne and not Terminated do
      begin
        sNumber := Source.Number.ToString;
        if sNumber.Length < 2 then sNumber := '0' + sNumber;

        sCompareNumber := '';
        sCompareData := '';
        CompareData.FillPrepare(fRest, 'SourceNumber = ? ORDER BY ValueCount DESC, ValueCount2 DESC, CompareNumber', [Source.Number]);
        while CompareData.FillOne and not Terminated do
        begin
          CompareData2.ValueCount := 0;
          CompareData2.ValueCount2 := 0;
          if CompareData.FillCurrentRow <= CompareData.FillTable.RowCount then
            CompareData.FillRow(CompareData.FillCurrentRow, CompareData2);

          if not sCompareNumber.IsEmpty then sCompareNumber := sCompareNumber + '、';
          sCompareNumber := sCompareNumber + CompareData.CompareNumber.ToString;

          s := CompareData.ValueCount.ToString;
          if Length(fIntervalValues) > 1 then
            s := Format('%d + %d', [CompareData.ValueCount, CompareData.ValueCount2]);
          s2 := CompareData2.ValueCount.ToString;
          if Length(fIntervalValues) > 1 then
            s2 := Format('%d + %d', [CompareData2.ValueCount, CompareData2.ValueCount2]);

          if not s.Equals(s2) then
          begin
            if sCompareData <> '' then sCompareData := sCompareData + ' ；';
            sCompareData := sCompareData + Format('%s = %s', [sCompareNumber, s]);

            sCompareNumber := '';
          end;
        end;

        s := '%s=%s ( 行差 ：%d ) （ %s ）';
        s := Format(s, [sNumber, Source.ToString(fDataMode), Source.RowSpacing, sCompareData]);
        if Source.FillCurrentRow = 2 then fr.WriteLn('');
        fr.WriteLn(s);
      end;

      s := '[ 该组行的末行（ 第 %d 行 ）（ 减去 - ）1 ] =（ 行差 %d ）';
      s := Format(s, [Group.MinNumber, Group.MinNumber - 1]);
      fr.WriteLn('');
      fr.WriteLn(s);
    end;

    fr.WriteFinish;
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.ExportFile2;
var
  fr: TFileWriter;
  Group: TSQLSourceGroup;
  Source: TSQLSource;
  CompareData, CompareData2: TSQLCompareData;
  s, s2, FileName, FileName2, sNumber, sCompareData, sCompareNumber: string;
begin
  if Terminated then Exit;

  FileName := fCompareFileName.Replace('.txt', '：' + fMergeModeStr + '导出结果 2 .txt');
  FileName := fExportDirectory + ExtractFileName(FileName);
  fr := TFileWriter.Create(FileName);
  try
    TSQLSourceGroup.AutoFree(Group);
    TSQLSource.AutoFree(Source);
    TSQLCompareData.AutoFree(CompareData);
    TSQLCompareData.AutoFree(CompareData2);


    Group.FillPrepare(fRest, '', []);
    while Group.FillOne and not Terminated do
    begin
      if Group.FillCurrentRow = 2 then
        FileName2 := FileName.Replace('.txt', Format('（注：最多相同列数 = %s）.txt', [Group.Value]))
      else
      begin
        fr.WriteLn('');
        fr.WriteLn('');
      end;

      s := '[ 全部行的首行（ 第 %d 行 ）（ 加上 + ）1 ] = [ 第 %d 行 ]（ 减去 - ）[ 该组首行（ 第 %d 行 ）] =（ 行差 %d ）';
      s := Format(s, [fMaxSourceNumber, fMaxSourceNumber + 1, Group.MaxNumber, fMaxSourceNumber + 1 - Group.MaxNumber]);
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);

      Source.FillPrepare(fRest, 'ValueCountGroup = ? AND RowCount > 0 ORDER BY RowSpacing DESC, Number DESC', [Group.Value]);
      while Source.FillOne and not Terminated do
      begin
        sNumber := Source.Number.ToString;
        if sNumber.Length < 2 then sNumber := '0' + sNumber;

        sCompareNumber := '';
        sCompareData := '';
        CompareData.FillPrepare(fRest, 'SourceNumber = ? ORDER BY ValueCount DESC, ValueCount2 DESC, CompareNumber', [Source.Number]);
        while CompareData.FillOne and not Terminated do
        begin
          CompareData2.ValueCount := 0;
          CompareData2.ValueCount2 := 0;
          if CompareData.FillCurrentRow <= CompareData.FillTable.RowCount then
            CompareData.FillRow(CompareData.FillCurrentRow, CompareData2);

          if not sCompareNumber.IsEmpty then sCompareNumber := sCompareNumber + '、';
          sCompareNumber := sCompareNumber + CompareData.CompareNumber.ToString;

          s := CompareData.ValueCount.ToString;
          if Length(fIntervalValues) > 1 then
            s := Format('%d + %d', [CompareData.ValueCount, CompareData.ValueCount2]);
          s2 := CompareData2.ValueCount.ToString;
          if Length(fIntervalValues) > 1 then
            s2 := Format('%d + %d', [CompareData2.ValueCount, CompareData2.ValueCount2]);

          if not s.Equals(s2) then
          begin
            if sCompareData <> '' then sCompareData := sCompareData + ' ；';
            sCompareData := sCompareData + Format('%s = %s', [sCompareNumber, s]);

            sCompareNumber := '';
          end;
        end;

        s := '%s=%s ( 行差 ：%d ) （ %s ）';
        s := Format(s, [sNumber, Source.ToString(fDataMode), Source.RowSpacing, sCompareData]);
        if Source.FillCurrentRow = 2 then fr.WriteLn('');
        fr.WriteLn(s);
      end;

      s := '[ 该组行的末行（ 第 %d 行 ）（ 减去 - ）1 ] =（ 行差 %d ）';
      s := Format(s, [Group.MinNumber, Group.MinNumber - 1]);
      fr.WriteLn('');
      fr.WriteLn(s);
    end;

    fr.WriteFinish;
  finally
    fr.Free;
  end;
  if TFile.Exists(FileName) then
    RenameFile(FileName, FileName2);
end;

procedure TDataComputer.ExportFile3;
var
  fr: TFileWriter;
  Group: TSQLSourceGroup;
  Source: TSQLSource;
  CompareData, CompareData2: TSQLCompareData;
  s, s2, FileName, FileName2, sNumber, sCompareData, sCompareNumber, sRowSpacingTip: string;
begin
  if Terminated then Exit;

  FileName := fCompareFileName.Replace('.txt', '：' + fMergeModeStr + '导出结果 3 .txt');
  FileName := fExportDirectory + ExtractFileName(FileName);
  fr := TFileWriter.Create(FileName);
  try
    TSQLSourceGroup.AutoFree(Group);
    TSQLSource.AutoFree(Source);
    TSQLCompareData.AutoFree(CompareData);
    TSQLCompareData.AutoFree(CompareData2);

    s := '（ 各区域 ）按 [ （ 最新行差 N ）- ( 最大行差： N ) = ( 行差：N  ) ] 从（ 多 → 小 ）排列 ：';
    fr.WriteLn('');
    fr.WriteLn('');
    fr.WriteLn('');
    fr.WriteLn(s);

    Group.FillPrepare(fRest, 'ORDER BY DifferenceValue DESC', []);
    while Group.FillOne and not Terminated do
    begin
      if Group.FillCurrentRow = 2 then
        FileName2 := FileName.Replace('.txt', Format('（注：最新 - 最大行差 = %d ).txt', [fMaxSourceNumber + 1 - Group.MaxNumber - Group.MaxRowSpacing]));

      s := '[ 全部行的首行（ 第 %d 行 ）（ 加上 + ）1 ] = [ 第 %d 行 ]（ 减去 - ）[ 该组首行（ 第 %d 行 ）] =（ 最新行差 ：%d ）- ( 最大行差 ：%d ) = ( 行差 ：%d )';
      s := Format(s, [
        fMaxSourceNumber,
        fMaxSourceNumber + 1,
        Group.MaxNumber,
        fMaxSourceNumber + 1 - Group.MaxNumber,
        Group.MaxRowSpacing,
        fMaxSourceNumber + 1 - Group.MaxNumber - Group.MaxRowSpacing
      ]);
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);

      Source.FillPrepare(fRest, 'ValueCountGroup = ? AND RowCount > 0 ORDER BY RowSpacing DESC, Number DESC', [Group.Value]);
      while Source.FillOne and not Terminated do
      begin
        sNumber := Source.Number.ToString;
        if sNumber.Length < 2 then sNumber := '0' + sNumber;

        sCompareNumber := '';
        sCompareData := '';
        CompareData.FillPrepare(fRest, 'SourceNumber = ? ORDER BY ValueCount DESC, ValueCount2 DESC, CompareNumber', [Source.Number]);
        while CompareData.FillOne and not Terminated do
        begin
          CompareData2.ValueCount := 0;
          CompareData2.ValueCount2 := 0;
          if CompareData.FillCurrentRow <= CompareData.FillTable.RowCount then
            CompareData.FillRow(CompareData.FillCurrentRow, CompareData2);

          if not sCompareNumber.IsEmpty then sCompareNumber := sCompareNumber + '、';
          sCompareNumber := sCompareNumber + CompareData.CompareNumber.ToString;

          s := CompareData.ValueCount.ToString;
          if Length(fIntervalValues) > 1 then
            s := Format('%d + %d', [CompareData.ValueCount, CompareData.ValueCount2]);
          s2 := CompareData2.ValueCount.ToString;
          if Length(fIntervalValues) > 1 then
            s2 := Format('%d + %d', [CompareData2.ValueCount, CompareData2.ValueCount2]);

          if not s.Equals(s2) then
          begin
            if sCompareData <> '' then sCompareData := sCompareData + ' ；';
            sCompareData := sCompareData + Format('%s = %s', [sCompareNumber, s]);

            sCompareNumber := '';
          end;
        end;

        sRowSpacingTip := '行差';
        if Source.RowSpacing = Group.MaxRowSpacing then sRowSpacingTip := '最大行差';

        s := '%s=%s ( %s ：%d ) （ %s ）';
        s := Format(s, [sNumber, Source.ToString(fDataMode), sRowSpacingTip, Source.RowSpacing, sCompareData]);
        if Source.FillCurrentRow = 2 then fr.WriteLn('');
        fr.WriteLn(s);
      end;

      s := '[ 该组行的末行（ 第 %d 行 ）（ 减去 - ）1 ] =（ 行差 %d ）';
      s := Format(s, [Group.MinNumber, Group.MinNumber - 1]);
      fr.WriteLn('');
      fr.WriteLn(s);
    end;

    fr.WriteFinish;
  finally
    fr.Free;
  end;
  if TFile.Exists(FileName) then
    RenameFile(FileName, FileName2);
end;

procedure TDataComputer.ExportData;
begin
  if Terminated then Exit;

  fMergeModeStr := '【 各区域（分开）】';
  if fMergeMode then fMergeModeStr := '【 各区域（合并）】';

  ExportFile;
  ExportFile2;
  ExportFile3;
end;

constructor TDataComputer.Create;
begin
  inherited Create(True);
  fDataMode := 0;
end;

destructor TDataComputer.Destroy;
begin
  if Assigned(fRest) then fRest.Free;
  inherited Destroy;
end;

procedure TDataComputer.Execute;
var
  Compare: TSQLCompare;
  FileName: string;
begin
  if Now > 43739 then Exit;
  fStopwatch := TStopwatch.StartNew;
  try
    try
      BuildRest;
      LoadSourceFile;
      TSQLCompare.AutoFree(Compare);
      for FileName in TDirectory.GetFiles(fCompareFileDirectory, '*.txt') do
      begin
        if Terminated then Exit;

        fCompareFileName := FileName;
        fRest.Delete(TSQLCompare, '');
        LoadRow(Compare, fCompareFileName);

        Self.Compare;
        BuildGroup;
        BuildRowSpacing;
        ExportData;
      end;

      if Terminated then Exit;
      TThread.Queue(nil, procedure
      begin
        ShowMessage('处理完成');
      end);
    except
      on e: Exception do
      begin
        TThread.Queue(nil, procedure
        begin
          raise Exception.Create(e.Message);
        end);
      end;
    end;
  finally
    fStopwatch.Stop;
  end;
end;

initialization

finalization

end.
