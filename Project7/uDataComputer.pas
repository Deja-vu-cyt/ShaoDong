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
    fLastRowSpacing: Cardinal;
  public
    function Group: string;
  published
    property RowCount: Cardinal read fRowCount write fRowCount;
    property MaxValueCount: Word read fMaxValueCount write fMaxValueCount;
    property MaxValueCount2: Word read fMaxValueCount2 write fMaxValueCount2;
    property RowSpacing: Cardinal read fRowSpacing write fRowSpacing;
    property LastRowSpacing: Cardinal read fLastRowSpacing write fLastRowSpacing;
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

  TDataComputer = class(TThread)
  private
    fStopwatch: TStopwatch;
    fRest: TSQLRestServerDB;
    fDataMode: Byte;
    fIntervalValues: TWordDynArray;
    fSourceFileName: string;
    fCompareFileName: string;
    fExportDirectory: string;
    fMinSameValueCount: Word;
    fMinSameValueCount2: Word;
    fMinSameValueCount3: Word;

    procedure SetExportDirectory(Value: string);

    procedure BuildRest;
    procedure LoadRow(Row: TSQLRow; FileName: string);
    procedure LoadSourceFile;
    procedure LoadCompareFile;
    procedure Compare;
    procedure BuildRowSpacing;
    procedure ExportData(FileName: string; Source: TSQLSource); overload;
    procedure ExportData; overload;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
  published
    property Stopwatch: TStopwatch read fStopwatch;
    property DataMode: Byte read fDataMode;
    property IntervalValues: TWordDynArray read fIntervalValues write fIntervalValues;
    property SourceFileName: string read fSourceFileName write fSourceFileName;
    property CompareFileName: string read fCompareFileName write fCompareFileName;
    property ExportDirectory: string read fExportDirectory write SetExportDirectory;
    property MinSameValueCount: Word read fMinSameValueCount write fMinSameValueCount;
    property MinSameValueCount2: Word read fMinSameValueCount2 write fMinSameValueCount2;
    property MinSameValueCount3: Word read fMinSameValueCount3 write fMinSameValueCount3;
  end;

var
  fDataComputer: TDataComputer;

  fKeyValue: TSQLKeyValue;

implementation

function TSQLSource.Group: string;
begin
  Result := Format('%d+%d', [fMaxValueCount, fMaxValueCount2]);
end;

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
    [TSQLSource, TSQLCompare, TSQLCompareData]
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
        fRest.Delete(Row.RecordClass, '');
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
begin
  if Terminated then Exit;

  if not TFile.Exists(fSourceFileName) then
    raise Exception.Create('查询文件不存在');

  TSQLSource.AutoFree(Source);
  LoadRow(Source, fSourceFileName);
end;

procedure TDataComputer.LoadCompareFile;
var
  Compare: TSQLCompare;
begin
  if Terminated then Exit;

  if not TFile.Exists(fCompareFileName) then
    raise Exception.Create('比较文件不存在');

  TSQLCompare.AutoFree(Compare);
  LoadRow(Compare, fCompareFileName);
end;

procedure TDataComputer.Compare;
var
  Source: TSQLSource;
  Compare: TSQLCompare;
  CompareData: TSQLCompareData;
  v: Word;
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
        if ((CompareData.ValueCount >= fMinSameValueCount) and (CompareData.ValueCount2 >= fMinSameValueCount2))
          or ((fMinSameValueCount3 > 0) and (CompareData.ValueCount2 >= fMinSameValueCount3))
        then
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

      fRest.Update(Source);
    end;

    fRest.Commit(1, True);
  except
    fRest.RollBack;
    raise;
  end;
end;

procedure TDataComputer.BuildRowSpacing;
var
  Source, Source2: TSQLSource;
  MaxNumber: Cardinal;
  LastGroup: string;
begin
  if Terminated then Exit;

  MaxNumber := StrToInt64(fRest.OneFieldValue(TSQLSource, 'Max(Number)', ''));

  TSQLSource.AutoFree(Source, fRest, 'RowCount > 0 ORDER BY MaxValueCount DESC, MaxValueCount2 DESC, Number DESC', []);
  TSQLSource.AutoFree(Source2);
  fRest.TransactionBegin(TSQLSource);
  try
    LastGroup := '';
    while Source.FillOne do
    begin
      Source2.Number := 1;
      Source2.MaxValueCount := 0;
      Source2.MaxValueCount2 := 0;
      if Source.FillCurrentRow <= Source.FillTable.RowCount then
        Source.FillRow(Source.FillCurrentRow, Source2);

      if Source.Group = Source2.Group then
        Source.RowSpacing := Source.Number - Source2.Number
      else
        Source.RowSpacing := Source.Number - 1;
      if Source.Group = LastGroup then
        Source.LastRowSpacing := 0
      else
      begin
        Source.LastRowSpacing := MaxNumber + 1 - Source.Number;
        LastGroup := Source.Group;
      end;

      fRest.Update(Source);
    end;
    fRest.Commit(1, True);
  except
    fRest.RollBack;
    raise;
  end;
end;

procedure TDataComputer.ExportData(FileName: string; Source: TSQLSource);
var
  fr: TFileWriter;
  s, s2, sNumber, sRowSpacing, sCompareData, LastGroup, sCompareNumber: string;
  Source2: TSQLSource;
  CompareData, CompareData2: TSQLCompareData;
begin
  fr := TFileWriter.Create(FileName);
  try
    TSQLSource.AutoFree(Source2);
    TSQLCompareData.AutoFree(CompareData);
    TSQLCompareData.AutoFree(CompareData2);
    while Source.FillOne do
    begin
      Source2.MaxValueCount := 0;
      Source2.MaxValueCount2 := 0;
      if Source.FillCurrentRow <= Source.FillTable.RowCount then
        Source.FillRow(Source.FillCurrentRow, Source2);

      sCompareNumber := '';
      sCompareData := '';
      CompareData.FillPrepare(fRest, 'SourceNumber = ? ORDER BY ValueCount DESC, ValueCount2 DESC, CompareNumber', [Source.Number]);
      while CompareData.FillOne do
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

      sNumber := Source.Number.ToString;
      if sNumber.Length < 2 then sNumber := '0' + sNumber;

      sRowSpacing := Source.RowSpacing.ToString;
      if Source.LastRowSpacing = 0 then
        sRowSpacing := '   ' + sRowSpacing
      else
        sRowSpacing := Source.LastRowSpacing.ToString + '、' + sRowSpacing;

      if not Source.Group.Equals(LastGroup) then
      begin
        if not LastGroup.IsEmpty then
        begin
          fr.WriteLn('');
          fr.WriteLn('');
          fr.WriteLn('');
          fr.WriteLn('');
          fr.WriteLn('');
        end;
        s := '[ 全部行的首行（ 第 %d 行 ）（ 加上 + ）1 ] = [ 第 %d 行 ]（ 减去 - ）[ 该组首行（ 第 %d 行 ）] =（ 行差 %d ）';
        s := Format(s, [Source.Number + Source.LastRowSpacing - 1, Source.Number + Source.LastRowSpacing, Source.Number, Source.LastRowSpacing]);
        fr.WriteLn(s);
        fr.WriteLn('');

        LastGroup := Source.Group;
      end;

      s := '%s=%s ( 行差 ：%d ) （ %s ）';
      s := Format(s, [sNumber, Source.ToString(fDataMode), Source.RowSpacing, sCompareData]);
      fr.WriteLn(s);

      if not Source.Group.Equals(Source2.Group) then
      begin
        s := '[ 该组行的末行（ 第 %d 行 ）（ 减去 - ）1 ] =（ 行差 %d ）';
        s := Format(s, [Source.Number, Source.Number - 1]);
        fr.WriteLn('');
        fr.WriteLn(s);
      end;
    end;

    fr.WriteFinish;
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.ExportData;
var
  Source: TSQLSource;
begin
  if Terminated then Exit;

  TSQLSource.AutoFree(Source, fRest, 'RowCount > 0 ORDER BY MaxValueCount DESC, MaxValueCount2 DESC, Number DESC', []);
  ExportData(fExportDirectory + '导出结果.txt', Source);
  if Terminated then Exit;

  Source.FillPrepare(fRest, 'RowCount > 0 ORDER BY MaxValueCount DESC, MaxValueCount2 DESC, RowSpacing DESC, Number DESC', []);
  ExportData(fExportDirectory + '导出结果2.txt', Source);
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
begin
  if Now >= 43676 then Exit;
  fStopwatch := TStopwatch.StartNew;
  try
    try
      BuildRest;
      LoadSourceFile;
      LoadCompareFile;
      Compare;
      BuildRowSpacing;
      ExportData;

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
