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
    fProcessed: Boolean;
  published
    property RowCount: Cardinal read fRowCount write fRowCount;
    property MaxValueCount: Word read fMaxValueCount write fMaxValueCount;
    property MaxValueCount2: Word read fMaxValueCount2 write fMaxValueCount2;
    property RowSpacing: Cardinal read fRowSpacing write fRowSpacing;
    property ValueCountGroup: RawUTF8 read fValueCountGroup write fValueCountGroup;
    property Processed: Boolean read fProcessed write fProcessed;
  end;

  TSQLCompareData = class(TSQLData)
  protected
    fFlag: Byte;
    fGroupNumber: Byte;
    fNumber: Cardinal;
    fGroupCodeName: RawUTF8;
    fCodeName: RawUTF8;
    fRowSpacing: Cardinal;
  published
    property Flag: Byte read fFlag write fFlag;
    property GroupNumber: Byte read fGroupNumber write fGroupNumber;
    property Number: Cardinal read fNumber write fNumber;
    property GroupCodeName: RawUTF8 read fGroupCodeName write fGroupCodeName;
    property CodeName: RawUTF8 read fCodeName write fCodeName;
    property RowSpacing: Cardinal read fRowSpacing write fRowSpacing;
  end;

  TSQLCodeName = class(TSQLRecord)
  protected
    fValue: RawUTF8;
    fValueCount: Byte;
    fFlag: Byte;
    fGroupNumber: Byte;
    fGroupCodeName: RawUTF8;
    fGroupCodeNameValueCount: Byte;
    fMaxNumber: Cardinal;
    fMinNumber: Cardinal;
    fMaxRowSpacing: Cardinal;
    fDifferenceValue: Integer;
    fRowCount: Cardinal;
    fEnabled: Boolean;
  published
    property Value: RawUTF8 read fValue write fValue;
    property ValueCount: Byte read fValueCount write fValueCount;
    property Flag: Byte read fFlag write fFlag;
    property GroupNumber: Byte read fGroupNumber write fGroupNumber;
    property GroupCodeName: RawUTF8 read fGroupCodeName write fGroupCodeName;
    property GroupCodeNameValueCount: Byte read fGroupCodeNameValueCount write fGroupCodeNameValueCount;
    property MaxNumber: Cardinal read fMaxNumber write fMaxNumber;
    property MinNumber: Cardinal read fMinNumber write fMinNumber;
    property MaxRowSpacing: Cardinal read fMaxRowSpacing write fMaxRowSpacing;
    property DifferenceValue: Integer read fDifferenceValue write fDifferenceValue;
    property RowCount: Cardinal read fRowCount write fRowCount;
    property Enabled: Boolean read fEnabled write fEnabled;
  end;

  TGroupCount = record
    Flag: Byte;
    Number: Byte;
    Value: Word;
    Value2: Word;
    Value3: Word;
    Value4: Word;
    function GetGroupValue(Flag: Byte; TotalCount, GroupNumber: Word): TWordDynArray;
  end;

  TDataComputer = class(TThread)
  private
    fStopwatch: TStopwatch;
    fRest: TSQLRestServerDB;
    fDataMode: Byte;
    fSourceMode: Byte;
    fTotalIntervalValue: Word;
    fMaxSourceNumber: Cardinal;
    fActiveGroupCounts: TArray<TGroupCount>;
    fActiveExportFile: Boolean;
    fActiveExportFile2: Boolean;
    fActiveExportFile3: Boolean;
    fActiveExportCodeNameCount: Cardinal;
    fActiveExportCodeNameCount2: Cardinal;
    fActiveExportCodeNameCount3: Cardinal;
    fCompareSpacing: Word;

    fIntervalValues: TWordDynArray;
    fSourceFileDirectory: string;
    fExportDirectory: string;
    fGroupCounts: TArray<TGroupCount>;
    fExportFile: Boolean;
    fExportFile2: Boolean;
    fExportFile3: Boolean;
    fExportCodeNameCount: Cardinal;
    fExportCodeNameCount2: Cardinal;
    fExportCodeNameCount3: Cardinal;
    fVertCompareSpacing: Word;
    fVertSameValueCount: Word;
    fVertSameValueCount2: Word;
    fVertGroupCounts: TArray<TGroupCount>;
    fVertExportFile: Boolean;
    fVertExportFile2: Boolean;
    fVertExportFile3: Boolean;
    fVertExportCodeNameCount: Cardinal;
    fVertExportCodeNameCount2: Cardinal;
    fVertExportCodeNameCount3: Cardinal;
    fCompareCrossRange: Boolean;
    fSlantCompareSpacing: Word;
    fSlantSameValueCount: Word;
    fSlantSameValueCount2: Word;
    fSlantGroupCounts: TArray<TGroupCount>;
    fSlantExportFile: Boolean;
    fSlantExportFile2: Boolean;
    fSlantExportFile3: Boolean;
    fSlantExportCodeNameCount: Cardinal;
    fSlantExportCodeNameCount2: Cardinal;
    fSlantExportCodeNameCount3: Cardinal;

    fSourceFileName: string;
    fCompareFileName: string;
    fExportFiles: TInt64DynArray;
    fDeleteProcessed: Boolean;

    procedure SetExportDirectory(Value: string);
    procedure BuildRest;
    procedure LoadRow(Row: TSQLRow; FileName: string);
    procedure LoadSourceFile;
    procedure LoadSourceFile2;
    procedure BuildSource;
    procedure Compare;
    procedure Compare2;
    procedure BuildCodeName;
    procedure BuildRowSpacing;
    procedure ExportData(Flag, GroupNumber, GroupCodeNameValueCount, CodeNameValueCount, FileFlag: Byte); overload;
    procedure ExportData; overload;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
  published
    property Stopwatch: TStopwatch read fStopwatch;
    property DataMode: Byte read fDataMode;
    property IntervalValues: TWordDynArray read fIntervalValues write fIntervalValues;
    property SourceFileDirectory: string read fSourceFileDirectory write fSourceFileDirectory;
    property ExportDirectory: string read fExportDirectory write SetExportDirectory;

    property GroupCounts: TArray<TGroupCount> read fGroupCounts write fGroupCounts;
    property ExportFile: Boolean read fExportFile write fExportFile;
    property ExportFile2: Boolean read fExportFile2 write fExportFile2;
    property ExportFile3: Boolean read fExportFile3 write fExportFile3;
    property ExportCodeNameCount: Cardinal read fExportCodeNameCount write fExportCodeNameCount;
    property ExportCodeNameCount2: Cardinal read fExportCodeNameCount2 write fExportCodeNameCount2;
    property ExportCodeNameCount3: Cardinal read fExportCodeNameCount3 write fExportCodeNameCount3;

    property VertCompareSpacing: Word read fVertCompareSpacing write fVertCompareSpacing;
    property VertSameValueCount: Word read fVertSameValueCount write fVertSameValueCount;
    property VertSameValueCount2: Word read fVertSameValueCount2 write fVertSameValueCount2;
    property VertGroupCounts: TArray<TGroupCount> read fVertGroupCounts write fVertGroupCounts;
    property VertExportFile: Boolean read fVertExportFile write fVertExportFile;
    property VertExportFile2: Boolean read fVertExportFile2 write fVertExportFile2;
    property VertExportFile3: Boolean read fVertExportFile3 write fVertExportFile3;
    property VertExportCodeNameCount: Cardinal read fVertExportCodeNameCount write fVertExportCodeNameCount;
    property VertExportCodeNameCount2: Cardinal read fVertExportCodeNameCount2 write fVertExportCodeNameCount2;
    property VertExportCodeNameCount3: Cardinal read fVertExportCodeNameCount3 write fVertExportCodeNameCount3;

    property CompareCrossRange: Boolean read fCompareCrossRange write fCompareCrossRange;
    property SlantCompareSpacing: Word read fSlantCompareSpacing write fSlantCompareSpacing;
    property SlantSameValueCount: Word read fSlantSameValueCount write fSlantSameValueCount;
    property SlantSameValueCount2: Word read fSlantSameValueCount2 write fSlantSameValueCount2;
    property SlantGroupCounts: TArray<TGroupCount> read fSlantGroupCounts write fSlantGroupCounts;
    property SlantExportFile: Boolean read fSlantExportFile write fSlantExportFile;
    property SlantExportFile2: Boolean read fSlantExportFile2 write fSlantExportFile2;
    property SlantExportFile3: Boolean read fSlantExportFile3 write fSlantExportFile3;
    property SlantExportCodeNameCount: Cardinal read fSlantExportCodeNameCount write fSlantExportCodeNameCount;
    property SlantExportCodeNameCount2: Cardinal read fSlantExportCodeNameCount2 write fSlantExportCodeNameCount2;
    property SlantExportCodeNameCount3: Cardinal read fSlantExportCodeNameCount3 write fSlantExportCodeNameCount3;

    property SourceFileName: string read fSourceFileName write fSourceFileName;
    property CompareFileName: string read fCompareFileName write fCompareFileName;
    property ExportFiles: TInt64DynArray read fExportFiles write fExportFiles;
    property DeleteProcessed: Boolean read fDeleteProcessed write fDeleteProcessed;
  end;

var
  fDataComputer: TDataComputer;
  fRestSettings: TSQLRestServerDB;
  fKeyValue: TSQLKeyValue;

implementation

function TGroupCount.GetGroupValue(Flag: Byte; TotalCount, GroupNumber: Word): TWordDynArray;
var
  i, iLength, iMod, iOffset: Integer;
begin
  iLength := TotalCount div Number;
  iMod := TotalCount mod Number;
  iOffset := 0;
  if GroupNumber > iMod then iOffset := iMod
  else iLength := iLength + 1;
  if GroupNumber > Number then iLength := 0;

  SetLength(Result, iLength);
  for i := Low(Result) to High(Result) do
    case Flag of
      2: Result[i] := i * Number + GroupNumber;
      else Result[i] := iOffset + (GroupNumber - 1) * iLength + i + 1;
    end;
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
    [TSQLRow, TSQLSource, TSQLCompareData, TSQLCodeName]
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
  SetDataMode := True;
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
            if SpecialMode(s) then fDataMode := 1;
            SetDataMode := False;
          end;

          Row.AssignValue(s, fIntervalValues, fDataMode);;
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
  Row: TSQLRow;
  FileName: string;
begin
  if Terminated then Exit;

  TSQLRow.AutoFree(Row);
  for FileName in TDirectory.GetFiles(fSourceFileDirectory, '*.txt') do
  begin
    if Terminated then Exit;

    if FileName.IndexOf('2. 读取') > -1 then
      //fSourceFileName := FileName
    else if FileName.IndexOf('3. 读取') > -1 then
      //fSourceFileName := FileName
    else
      LoadRow(Row, FileName);
  end;
end;

procedure TDataComputer.LoadSourceFile2;

  function Extract(s: string): string;
  var
    c: Char;
    HaveEqual: Boolean;
    i, iLeft: Integer;
  begin
    i := -1;
    iLeft := -1;
    HaveEqual := False;
    for c in s do
    begin
      Inc(i);
      case c of
        '(', '（':
        begin
          iLeft := i;
          HaveEqual := False;
        end;
        '=':
        begin
          HaveEqual := True;
        end;
        ')', '）':
        begin
          if (iLeft > -1) and HaveEqual then
          begin
            Result := s.Substring(iLeft + 1, i - iLeft - 1);
          end;
          iLeft := -1;
          HaveEqual := False;
        end;
      end;
    end;
  end;

  function Analysis(s: string): string;
  var
    s2, s3: TArray<string>;
    i: Integer;
  begin
    Result := '';
    s2 := s.Split(['；']);
    for s in s2 do
    begin
      i := s.IndexOf('=');
      if i > -1 then
      begin
        if not Result.IsEmpty then Result := Result + '、';
        Result := Result + s.Substring(0, i - 1);
      end;
    end;
  end;

var
  Source: TSQLSource;
  s, FileName: string;
  i: Integer;
  r, r2: TWordDynArray;
  v: Word;
begin
  if Terminated then Exit;

  fSourceFileName := '';
  fCompareFileName := '';
  for FileName in TDirectory.GetFiles(fSourceFileDirectory, '*.txt') do
  begin
    if FileName.IndexOf('2. 读取') > -1 then
      fSourceFileName := FileName
    else if FileName.IndexOf('3. 读取') > -1 then
      fCompareFileName := FileName;
  end;
  if not FileExists(fSourceFileName) then Exit;

  TSQLSource.AutoFree(Source);
  fRest.Delete(TSQLSource, '');
  LoadRow(Source, fSourceFileName);

  fMaxSourceNumber := 0;
  s := fRest.OneFieldValue(TSQLSource, 'Max(Number)', '');
  if not s.IsEmpty then fMaxSourceNumber := StrToInt64(s);

  if FileExists(fCompareFileName) then
  begin
    with TStringList.Create do
    begin
      try
        LoadFromFile(fCompareFileName);
        for i := Count - 1 downto 0 do
        begin
          s := Strings[i];
          s := Extract(s);
          s := Analysis(s);
          r.Assign(s);
          for v in r do
            if not r2.Exist(v) then r2.Add(v);
        end;
      finally
        Free;
      end;
    end;
  end;

  Source.FillPrepare(fRest, '', []);
  fRest.TransactionBegin(TSQLSource);
  try
    while Source.FillOne do
    begin
      if r2.Exist(Source.Number) then
      begin
        Source.Processed := True;
        fRest.Update(Source);
      end;
    end;
    fRest.Commit(1, True);
  except
    on e: Exception do
    begin
      fRest.RollBack;
      raise;
    end;
  end;
end;

procedure TDataComputer.BuildSource;

  function VertCompareRow(r, r2: TSQLRow): Boolean;
  var
    v, TotalSameValueCount: Word;
  begin
    TotalSameValueCount := 0;
    for v in r2.Values do
      if r.ValueExist(v) then TotalSameValueCount := TotalSameValueCount + 1;
    Result := (TotalSameValueCount >= fVertSameValueCount)
      and (TotalSameValueCount <= fVertSameValueCount2);
  end;

  function SlantCompareRow(r, r2: TSQLRow; Source: TSQLSource; Offset: Integer): Boolean;
  var
    v, v2, TotalSameValueCount: Word;
  begin
    TotalSameValueCount := 0;
    Source.ClearValue;
    for v2 in r2.Values do
    begin
      v := v2 + Offset;
      if (v < 1) or (v > fTotalIntervalValue) then Continue;
      if not fCompareCrossRange
        and (((Offset > 0) and (v > fIntervalValues[0]))
        or ((Offset < 0) and (v <= fIntervalValues[0])))
      then Continue;

      Source.AddValue(v);
      if r.ValueExist(v) then TotalSameValueCount := TotalSameValueCount + 1;
    end;
    Result := Source.HasValue
      and (TotalSameValueCount >= fSlantSameValueCount)
      and (TotalSameValueCount <= fSlantSameValueCount2);
  end;

var
  Row, Row2: TSQLRow;
  Source, Source2: TSQLSource;
  i: Integer;
  Number, CodeName: Word;
  s: string;
begin
  if Terminated then Exit;
  TSQLRow.AutoFree(Row, fRest, 'ORDER BY Number', []);
  TSQLRow.AutoFree(Row2);
  TSQLSource.AutoFree(Source);
  TSQLSource.AutoFree(Source2);

  fRest.TransactionBegin(TSQLSource, 1);
  try
    fRest.Delete(TSQLSource, '');

    while Row.FillOne and not Terminated do
    begin
      Source.Number := Row.Number;

      case fSourceMode of
        1, 2:
        begin
          if Row.FillCurrentRow - 1 <= fCompareSpacing then Continue;

          Source.ClearValue;
          for i := Row.FillCurrentRow - 2 downto Row.FillCurrentRow - 1 - fCompareSpacing do
          begin
            Row.FillRow(i, Row2);
            Number := Row.FillCurrentRow - 1 - i;
            case fSourceMode of
              1:
              begin
                //直连
                if Number <= fVertCompareSpacing then
                begin
                  if VertCompareRow(Row, Row2) then
                    Source.AddValue(Row2.Values);
                end;
              end;
              2:
              begin
                //斜连
                if Number <= fSlantCompareSpacing then
                begin
                  //右斜连
                  if SlantCompareRow(Row, Row2, Source2, Number) then
                    Source.AddValue(Source2.Values);
                  //左斜连
                  if SlantCompareRow(Row, Row2, Source2, -Number) then
                    Source.AddValue(Source2.Values);
                end;
              end;
            end;
          end;
        end;
        else
        begin
          Source.AssignValue(Row);
        end;
      end;

      fRest.Add(Source, True);
    end;
    fRest.Commit(1, True);

    fMaxSourceNumber := 0;
    s := fRest.OneFieldValue(TSQLSource, 'Max(Number)', '');
    if not s.IsEmpty then fMaxSourceNumber := StrToInt64(s);
  except
    on e: Exception do
    begin
      fRest.RollBack(1);
      raise Exception.Create(e.Message);
    end;
  end;
end;

procedure TDataComputer.Compare;
var
  Source: TSQLSource;
  CompareData, CompareData2: TSQLCompareData;
  i: Integer;
  gc: TGroupCount;
  Columns, CodeName: TWordDynArray;
  s: string;
begin
  if Terminated then Exit;

  TSQLSource.AutoFree(Source, fRest, 'ORDER BY Number', []);
  TSQLCompareData.AutoFree(CompareData);
  TSQLCompareData.AutoFree(CompareData2);
  CompareData.AssignValue('', fIntervalValues, fDataMode);

  fRest.TransactionBegin(TSQLCompareData);
  try
    fRest.Delete(TSQLCompareData, '');

    for gc in fActiveGroupCounts do
    begin
      if Terminated then Exit;

      CompareData.Flag := gc.Flag;
      case gc.Flag of
        120:
        begin
          CompareData.GroupNumber := gc.Number;
          CompareData.ClearValue;
          for i := 1 to fTotalIntervalValue do CompareData.AddValue(i);
          CompareData.CalcValueCount;
          fRest.Add(CompareData, True);
        end;
      end;
      begin
        for i := gc.Value to gc.Value2 do
        begin
          if Terminated then Exit;

          SetLength(CodeName, i);
          CompareData.GroupNumber := gc.Number;
          TCombinatorialAlgorithm.For(gc.Number, i, procedure(FirstNumber: Cardinal; Numbers: TCardinalDynArray)
          var
            i: Integer;
            v: Word;
            Values: TWordDynArray;
          begin
            for i := Low(CodeName) to High(CodeName) do CodeName[i] := Numbers[i];

            CompareData.GroupCodeName := CodeName.ToString;
            SetLength(Values, 0);
            for i := Low(Numbers) to High(Numbers) do
              Values.Add(gc.GetGroupValue(gc.Flag, fTotalIntervalValue, Numbers[i]));

            CompareData.ClearValue;
            CompareData.AddValue(Values);
            CompareData.CalcValueCount;
            fRest.Add(CompareData, True);
          end,
          function(FirstNumber, GroupCount: Cardinal): Boolean
          begin
            Result := Terminated;
          end);
        end;
      end;

      CompareData2.FillPrepare(fRest, 'Flag =? AND GroupNumber = ?', [gc.Flag, gc.Number]);
      fRest.Delete(TSQLCompareData, 'Flag =? AND GroupNumber = ?', [gc.Flag, gc.Number]);
      while CompareData2.FillOne do
      begin
        CompareData.Flag := CompareData2.Flag;
        CompareData.GroupNumber := CompareData2.GroupNumber;
        CompareData.GroupCodeName := CompareData2.GroupCodeName;
        Columns := CompareData2.Values;
        for i := gc.Value3 to gc.Value4 do
        begin
          SetLength(CodeName, i);
          TCombinatorialAlgorithm.For(CompareData2.TotalValueCount, i, procedure(FirstNumber: Cardinal; Numbers: TCardinalDynArray)
          var
            i: Integer;
          begin
            for i := Low(CodeName) to High(CodeName) do CodeName[i] := Columns[Numbers[i] - 1];

            Source.FillRewind;
            while Source.FillOne and not Terminated do
            begin
              if Source.ValueExist(CodeName) then
              begin
                CompareData.Number := Source.Number;
                CompareData.CodeName := CodeName.ToString;
                CompareData.ClearValue;
                CompareData.AddValue(CodeName);
                CompareData.CalcValueCount;
                fRest.Add(CompareData, True);
              end;
            end;
          end,
          function(FirstNumber, GroupCount: Cardinal): Boolean
          begin
            Result := Terminated;
          end);
        end;
      end;
    end;

    fRest.Commit(1, True);
  except
    fRest.RollBack;
    raise;
  end;

  {fRest.TransactionBegin(TSQLCompareData);
  try
    fRest.Delete(TSQLCompareData, '');

    CompareData.Flag := 0;
    CompareData.GroupNumber := 0;
    for i := fColumnGroupCount.Value to fColumnGroupCount.Value2 do
    begin
      if fColumnGroupCount.Value = 0 then Continue;
      SetLength(CodeName, i);
      TCombinatorialAlgorithm.For(fTotalIntervalValue, i, procedure(FirstNumber: Cardinal; Numbers: TCardinalDynArray)
      var
        i: Integer;
      begin
        for i := Low(CodeName) to High(CodeName) do CodeName[i] := Numbers[i];

        CompareData.CodeName := CodeName.ToString;
        CompareData.ClearValue;
        CompareData.AddValue(CodeName);
        CompareData.CalcValueCount;

        Source.FillRewind;
        while Source.FillOne and not Terminated do
        begin
          CompareData.Number := Source.Number;
          if Source.ValueExist(CodeName) then
            fRest.Add(CompareData, True);
        end;
      end,
      function(FirstNumber, GroupCount: Cardinal): Boolean
      begin
        Result := Terminated;
      end);
    end;

    CompareData.Flag := 1;
    for gc in fSubsectionGroupCount do
    begin
      for i := gc.Value to gc.Value2 do
      begin
        SetLength(CodeName, i);
        CompareData.GroupNumber := gc.Number;
        TCombinatorialAlgorithm.For(gc.Number, i, procedure(FirstNumber: Cardinal; Numbers: TCardinalDynArray)
        var
          i: Integer;
          v: Word;
          Values: TWordDynArray;
        begin
          for i := Low(CodeName) to High(CodeName) do CodeName[i] := Numbers[i];

          CompareData.CodeName := CodeName.ToString;
          SetLength(Values, 0);
          for i := Low(Numbers) to High(Numbers) do
            Values.Add(gc.GetGroupValue(0, fTotalIntervalValue, Numbers[i]));

          Source.FillRewind;
          while Source.FillOne and not Terminated do
          begin
            CompareData.Number := Source.Number;
            CompareData.ClearValue;
            for v in Source.Values do
              if Values.Exist(v) then CompareData.AddValue(v);

            CompareData.CalcValueCount;
            if CompareData.HasValue then fRest.Add(CompareData, True);
          end;
        end,
        function(FirstNumber, GroupCount: Cardinal): Boolean
        begin
          Result := Terminated;
        end);
      end;
    end;

    CompareData.Flag := 2;
    for gc in fFirstNumberGroupCount do
    begin
      for i := gc.Value to gc.Value2 do
      begin
        SetLength(CodeName, i);
        CompareData.GroupNumber := gc.Number;
        TCombinatorialAlgorithm.For(gc.Number, i, procedure(FirstNumber: Cardinal; Numbers: TCardinalDynArray)
        var
          i: Integer;
          v: Word;
          Values: TWordDynArray;
        begin
          for i := Low(CodeName) to High(CodeName) do CodeName[i] := Numbers[i];

          CompareData.CodeName := CodeName.ToString;
          SetLength(Values, 0);
          for i := Low(Numbers) to High(Numbers) do
            Values.Add(gc.GetGroupValue(1, fTotalIntervalValue, Numbers[i]));

          Source.FillRewind;
          while Source.FillOne and not Terminated do
          begin
            CompareData.Number := Source.Number;
            CompareData.ClearValue;
            for v in Source.Values do
              if Values.Exist(v) then CompareData.AddValue(v);

            CompareData.CalcValueCount;
            if CompareData.HasValue then fRest.Add(CompareData, True);
          end;
        end,
        function(FirstNumber, GroupCount: Cardinal): Boolean
        begin
          Result := Terminated;
        end);
      end;
    end;

    fRest.Commit(1, True);
  except
    fRest.RollBack;
    raise;
  end;}
end;

procedure TDataComputer.Compare2;
var
  Source: TSQLSource;
  CompareData, CompareData2: TSQLCompareData;
  i: Integer;
  gc: TGroupCount;
  Columns, CodeName: TWordDynArray;
  v: Int64;
  Rows, BigRows, SmallRows, OddRows, EvenRows: TWordDynArray;
begin
  if Terminated then Exit;

  TSQLSource.AutoFree(Source, fRest, 'ORDER BY Number DESC', []);
  TSQLCompareData.AutoFree(CompareData);
  TSQLCompareData.AutoFree(CompareData2);
  CompareData.AssignValue('', fIntervalValues);
  CompareData.GroupNumber := 0;
  CompareData.GroupCodeName := '';
  CompareData.CodeName := '';

  while Source.FillOne do
  begin
    if fDeleteProcessed and Source.Processed then Continue;

    if Source.Number mod 2 = 0 then EvenRows.Add(Source.Number)
    else OddRows.Add(Source.Number);

    if Source.Number > fMaxSourceNumber div 2 then BigRows.Add(Source.Number)
    else SmallRows.Add(Source.Number);
  end;

  fRest.TransactionBegin(TSQLCompareData);
  try
    fRest.Delete(TSQLCompareData, '');

    for v in fExportFiles do
    begin
      case v.ValueCount of
        1:
        begin
          if v.ValueExist(1) then
          begin
            CompareData.Flag := 3;
            Rows := OddRows;
          end;
          if v.ValueExist(2) then
          begin
            CompareData.Flag := 4;
            Rows := EvenRows;
          end;
          if v.ValueExist(3) then
          begin
            CompareData.Flag := 5;
            Rows := BigRows;
          end;
          if v.ValueExist(4) then
          begin
            CompareData.Flag := 6;
            Rows := SmallRows;
          end;
        end;
        2:
        begin
          if v.ValueExist(1) and v.ValueExist(3) then
          begin
            CompareData.Flag := 7;
            Rows := OddRows;
            Rows.Add(BigRows);
          end;
          if v.ValueExist(1) and v.ValueExist(4) then
          begin
            CompareData.Flag := 8;
            Rows := OddRows;
            Rows.Add(SmallRows);
          end;
          if v.ValueExist(2) and v.ValueExist(3) then
          begin
            CompareData.Flag := 9;
            Rows := EvenRows;
            Rows.Add(BigRows);
          end;
          if v.ValueExist(2) and v.ValueExist(4) then
          begin
            CompareData.Flag := 10;
            Rows := EvenRows;
            Rows.Add(SmallRows);
          end;
        end;
      end;

      Source.FillRewind;
      while Source.FillOne do
      begin
        if not Rows.Exist(Source.Number) then Continue;

        CompareData.Number := Source.Number;
        CompareData.AssignValue(Source);
        fRest.Add(CompareData, True);
      end;
    end;

    fRest.Commit(1, True);
  except
    fRest.RollBack;
    raise;
  end;
end;

procedure TDataComputer.BuildCodeName;
var
  CodeName: TSQLCodeName;
  l: TSQLTableJSON;
  t: TWordDynArray;
begin
  if Terminated then Exit;

  fRest.Delete(TSQLCodeName, '');
  TSQLCodeName.AutoFree(CodeName);
  CodeName.Enabled := True;
  l := fRest.MultiFieldValues(TSQLCompareData,
    'Flag, GroupNumber, GroupCodeName, CodeName, Max(Number), Min(Number), Count(Number)',
    'GROUP BY Flag, GroupNumber, GroupCodeName, CodeName', []);
  try
    fRest.TransactionBegin(TSQLCodeName);
    try
      while l.Step do
      begin
        CodeName.Flag := l.FieldAsInteger(0);
        CodeName.GroupNumber := l.FieldAsInteger(1);
        CodeName.GroupCodeName := l.FieldAsRawUTF8(2);
        CodeName.Value := l.FieldAsRawUTF8(3);
        CodeName.MaxNumber := l.FieldAsInteger(4);
        CodeName.MinNumber := l.FieldAsInteger(5);
        CodeName.RowCount := l.FieldAsInteger(6);
        t.Assign(CodeName.Value);
        CodeName.ValueCount := Length(t);
        t.Assign(CodeName.GroupCodeName);
        CodeName.GroupCodeNameValueCount := Length(t);

        fRest.Add(CodeName, True);
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
  CodeName: TSQLCodeName;
  CompareData, CompareData2: TSQLCompareData;
begin
  if Terminated then Exit;

  TSQLCodeName.AutoFree(CodeName, fRest, 'ORDER BY RowCount DESC', []);
  TSQLCompareData.AutoFree(CompareData);
  TSQLCompareData.AutoFree(CompareData2);
  fRest.TransactionBegin(TSQLCodeName);
  try
    while CodeName.FillOne and not Terminated do
    begin
      CodeName.MaxRowSpacing := 0;
      CompareData.FillPrepare(fRest, 'Flag = ? AND GroupNumber = ? AND GroupCodeName = ? AND CodeName = ? ORDER BY Number DESC',
        [CodeName.Flag, CodeName.GroupNumber, CodeName.GroupCodeName, CodeName.Value]);
      while CompareData.FillOne and not Terminated do
      begin
        CompareData2.Number := 1;
        if CompareData.FillCurrentRow <= CompareData.FillTable.RowCount then
          CompareData.FillRow(CompareData.FillCurrentRow, CompareData2);

        CompareData.RowSpacing := CompareData.Number - CompareData2.Number;
        if CompareData.RowSpacing > CodeName.MaxRowSpacing then
          CodeName.MaxRowSpacing := CompareData.RowSpacing;

        fRest.Update(CompareData);
      end;

      fRest.Update(CodeName);
    end;

    CodeName.FillPrepare(fRest, '', []);
    while CodeName.FillOne and not Terminated do
    begin
      CodeName.DifferenceValue := fMaxSourceNumber + 1 - CodeName.MaxNumber - CodeName.MaxRowSpacing;
      fRest.Update(CodeName);
    end;

    fRest.Commit(1, True);
  except
    fRest.RollBack;
    raise;
  end;
end;

procedure TDataComputer.ExportData(Flag, GroupNumber, GroupCodeNameValueCount, CodeNameValueCount, FileFlag: Byte);
var
  fr: TFileWriter;
  CodeName: TSQLCodeName;
  Source: TSQLSource;
  CompareData: TSQLCompareData;
  s, s2, FileName, FileName2, sMax: string;
begin
  if Terminated then Exit;

  case Flag of
    1:
    begin
      case fSourceMode of
        1: FileName := '（5）（1-1）. [（直连码）（分 %d 段）... %d个组合（段）]（%d个相同）列数字：.txt';
        2: FileName := '（6）（1-1）. [（斜连码）（分 %d 段）... %d个组合（段）]（%d个相同）列数字：.txt';
        else FileName := '（3）（1-1）. [（分 %d 段）... %d个组合（段）]（%d个相同）列数字：.txt';
      end;
      FileName := Format(FileName, [GroupNumber, GroupCodeNameValueCount, CodeNameValueCount]);
    end;
    2:
    begin
      case fSourceMode of
        1: FileName := '（5）（1-1）. [（直连码）（除以 %d）... %d个组合（组）]（%d个相同）列数字：.txt';
        2: FileName := '（6）（1-1）. [（斜连码）（除以 %d）... %d个组合（组）]（%d个相同）列数字：.txt';
        else FileName := '（4）（1-1）. [（除以 %d）... %d个组合（组）]（%d个相同）列数字：.txt';
      end;
      FileName := Format(FileName, [GroupNumber, GroupCodeNameValueCount, CodeNameValueCount]);
    end;
    3:
    begin
      FileName := '（1）（0-1）. [ 查询（行号）单；（各）行之间 ，（行间差 ）].txt';
    end;
    4:
    begin
      FileName := '（1）（0-2）. [ 查询（行号）双；（各）行之间 ，（行间差 ）].txt';
    end;
    5:
    begin
      FileName := '（1）（0-3）. [ 查询（行号）大；（各）行之间 ，（行间差 ）].txt';
    end;
    6:
    begin
      FileName := '（1）（0-4）. [ 查询（行号）小；（各）行之间 ，（行间差 ）].txt';
    end;
    7:
    begin
      FileName := '（1）（0-5）. [ 查询（行号）单 、大；（各）行之间 ，（行间差 ）].txt';
    end;
    8:
    begin
      FileName := '（1）（0-6）. [ 查询（行号）单 、小；（各）行之间 ，（行间差 ）].txt';
    end;
    9:
    begin
      FileName := '（1）（0-7）. [ 查询（行号）双 、大；（各）行之间 ，（行间差 ）].txt';
    end;
    10:
    begin
      FileName := '（1）（0-8）. [ 查询（行号）双 、小；（各）行之间 ，（行间差 ）].txt';
    end;
    else
    begin
      case fSourceMode of
        1: FileName := '（5）（0-0）. [（直连码）...（%d个相同）列数字 ]：.txt';
        2: FileName := '（6）（0-0）. [（斜连码）...（%d个相同）列数字 ]：.txt';
        else FileName := '（2）（0-0）. （%d个相同）列数字：.txt';
      end;
      FileName := Format(FileName, [CodeNameValueCount]);
    end;
  end;
  FileName := fExportDirectory + ExtractFileName(FileName);
  fr := TFileWriter.Create(FileName);
  try
    TSQLCodeName.AutoFree(CodeName);
    TSQLSource.AutoFree(Source);
    TSQLCompareData.AutoFree(CompareData);

    s := 'Flag = ? AND GroupNumber = ? AND GroupCodeNameValueCount = ? AND ValueCount = ?';
    case FileFlag of
      1:
      begin
        if fActiveExportCodeNameCount > 0 then
          s := s + ' AND Enabled = 1';
        s := s + ' ORDER BY MaxRowSpacing DESC';
        if fActiveExportCodeNameCount2 > 0 then
          s := s + Format(' LIMIT %d', [fActiveExportCodeNameCount2]);
      end;
      2:
      begin
        if fActiveExportCodeNameCount > 0 then
          s := s + ' AND Enabled = 1';
        s := s + ' ORDER BY DifferenceValue DESC';
        if fActiveExportCodeNameCount3 > 0 then
          s := s + Format(' LIMIT %d', [fActiveExportCodeNameCount3]);
      end;
      else
      begin
        s := s + ' ORDER BY RowCount DESC';
        if fActiveExportCodeNameCount > 0 then
          s := s + Format(' LIMIT %d', [fActiveExportCodeNameCount]);
      end;
    end;
    CodeName.FillPrepare(fRest, s, [Flag, GroupNumber, GroupCodeNameValueCount, CodeNameValueCount]);
    while CodeName.FillOne and not Terminated do
    begin
      if CodeName.FillCurrentRow = 2 then
      begin
        case FileFlag of
          1: FileName2 := FileName.Replace('.txt', Format(' ( 注：最大行间差：%d ) .txt', [CodeName.MaxRowSpacing]));
          2: FileName2 := FileName.Replace('.txt', Format(' ( 注：最新 - 最大行间差 = %d ).txt', [CodeName.DifferenceValue]));
          else FileName2 := FileName.Replace('.txt', Format('  [ 注：最多行数：共 %d 行 ] .txt', [CodeName.RowCount]));
        end;
      end;

      if CodeName.Value <> '' then
      begin
        CompareData.AssignValue(CodeName.Value);
        CompareData.IntervalValues := fIntervalValues;

        case Flag of
          1:
          begin
            s := '%d.（ 分 %d 段 ，第 %s 段 ，%d 个相同列数字 ：%s ），共 %d 行 ';
            s := Format(s, [
              CodeName.FillCurrentRow - 1,
              GroupNumber,
              CodeName.GroupCodeName,
              CodeName.ValueCount,
              CompareData.ToString(fDataMode),
              CodeName.RowCount
            ]);
          end;
          2:
          begin
            s := '%d.（ 除以 %d ＝ 第 %s 组, %d 个相同列数字 ：%s ），共 %d 行 ';
            s := Format(s, [
              CodeName.FillCurrentRow - 1,
              GroupNumber,
              CodeName.GroupCodeName,
              CodeName.ValueCount,
              CompareData.ToString(fDataMode),
              CodeName.RowCount
            ]);
          end;
          else
          begin
            s := '%d.（ 列组合 ；第 %d 个组合 ，列数字 ：%s ），共 %d 行 ：';
            s := Format(s, [
              CodeName.FillCurrentRow - 1,
              CodeName.ValueCount,
              CompareData.ToString(fDataMode),
              CodeName.RowCount
            ]);
          end;
        end;
        if CodeName.FillCurrentRow > 2 then
        begin
          fr.WriteLn('');
          fr.WriteLn('');
          fr.WriteLn('');
          fr.WriteLn('');
          fr.WriteLn('');
        end;
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn(s);
      end;

      s := '[ 全部行的首行（ 第 %d 行 ）（ 加上 + ）1 ] = [ 第 %d 行 ]（ 减去 - ）[ 该组首行（ 第 %d 行 ）] =（ 最新行间差 ：%d ）- ( 最大行间差 ：%d ) = ( 行间差 ：%d )';
      s := Format(s, [
        fMaxSourceNumber,
        fMaxSourceNumber + 1,
        CodeName.MaxNumber,
        fMaxSourceNumber + 1 - CodeName.MaxNumber,
        CodeName.MaxRowSpacing,
        CodeName.DifferenceValue
      ]);
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);

      CompareData.FillPrepare(fRest, 'Flag = ? AND GroupNumber = ? AND GroupCodeName = ? AND CodeName = ? ORDER BY Number DESC',
        [CodeName.Flag,  CodeName.GroupNumber, CodeName.GroupCodeName, CodeName.Value]);
      while CompareData.FillOne and not Terminated do
      begin
        if FileFlag = 2 then
        begin
          if not ((CompareData.FillCurrentRow = 2)
            or (CompareData.FillCurrentRow > CompareData.FillTable.RowCount)
            or (CompareData.RowSpacing = CodeName.MaxRowSpacing))
          then Continue;
        end;

        sMax := '';
        if CompareData.RowSpacing = CodeName.MaxRowSpacing then sMax := '最大';

        s := '%d=%s ( %s行间差 ：%d ) ';
        s := Format(s, [CompareData.Number, CompareData.ToString(fDataMode), sMax, CompareData.RowSpacing]);
        fr.WriteLn('');
        fr.WriteLn(s);
      end;

      s := '[ 该组行的末行（ 第 %d 行 ）（ 减去 - ）1 ] =（ 行间差 %d ）';
      s := Format(s, [CodeName.MinNumber, CodeName.MinNumber - 1]);
      fr.WriteLn('');
      fr.WriteLn(s);
    end;

    fr.WriteFinish;
  finally
    fr.Free;
  end;
  if TFile.Exists(FileName) then
  begin
    if TFile.Exists(FileName2) then TFile.Delete(FileName2);
    RenameFile(FileName, FileName2);
  end;
end;

procedure TDataComputer.ExportData;
var
  l: TSQLTableJSON;
  CodeName: TSQLCodeName;
begin
  if Terminated then Exit;

  l := fRest.MultiFieldValues(TSQLCodeName, 'Flag, GroupNumber, GroupCodeNameValueCount, ValueCount',
    'GROUP BY Flag, GroupNumber, GroupCodeNameValueCount, ValueCount ORDER BY Flag, GroupCodeNameValueCount, GroupNumber, ValueCount', []);
  try
    while l.Step and not Terminated do
    begin
      if fActiveExportCodeNameCount > 0 then
      begin
        TSQLCodeName.AutoFree(CodeName, fRest,
          'Flag = ? AND GroupNumber = ? AND GroupCodeNameValueCount = ? AND ValueCount = ? ORDER BY RowCount DESC',
          [l.FieldAsInteger(0), l.FieldAsInteger(1), l.FieldAsInteger(2), l.FieldAsInteger(3)]);
        fRest.TransactionBegin(TSQLCodeName);
        try
          while CodeName.FillOne do
          begin
            CodeName.Enabled := CodeName.FillCurrentRow - 1 <= fActiveExportCodeNameCount;
            fRest.Update(CodeName);
          end;
          fRest.Commit(1, True);
        except
          fRest.RollBack;
          raise;
        end;
      end;

      if fActiveExportFile then
        ExportData(l.FieldAsInteger(0), l.FieldAsInteger(1), l.FieldAsInteger(2), l.FieldAsInteger(3), 0);
      if fActiveExportFile2 then
        ExportData(l.FieldAsInteger(0), l.FieldAsInteger(1), l.FieldAsInteger(2), l.FieldAsInteger(3), 1);
      if fActiveExportFile3 then
        ExportData(l.FieldAsInteger(0), l.FieldAsInteger(1), l.FieldAsInteger(2), l.FieldAsInteger(3), 2);
    end;
  finally
    l.Free;
  end;
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
  FileName: string;
  i: Integer;
begin
  if Now > 43800 then Exit;
  fStopwatch := TStopwatch.StartNew;
  try
    try
      fTotalIntervalValue := 0;
      for i := Low(fIntervalValues) to High(fIntervalValues) do
        fTotalIntervalValue := fTotalIntervalValue + fIntervalValues[i];

      BuildRest;

      if Length(fExportFiles) > 0 then
      begin
        fActiveExportFile := True;
        fActiveExportFile2 := True;
        fActiveExportFile3 := True;
        fActiveExportCodeNameCount := 0;
        fActiveExportCodeNameCount2 := 0;
        fActiveExportCodeNameCount3 := 0;

        LoadSourceFile2;
        Compare2;
        BuildCodeName;
        BuildRowSpacing;
        ExportData;
      end;
      LoadSourceFile;
      if Length(fGroupCounts) > 0 then
      begin
        fActiveGroupCounts := fGroupCounts;
        fActiveExportFile := fExportFile;
        fActiveExportFile2 := fExportFile2;
        fActiveExportFile3 := fExportFile3;
        fActiveExportCodeNameCount := fExportCodeNameCount;
        fActiveExportCodeNameCount2 := fExportCodeNameCount2;
        fActiveExportCodeNameCount3 := fExportCodeNameCount3;
        fSourceMode := 0;

        BuildSource;
        Compare;
        BuildCodeName;
        BuildRowSpacing;
        ExportData;
      end;
      if Length(fVertGroupCounts) > 0 then
      begin
        fActiveGroupCounts := fVertGroupCounts;
        fActiveExportFile := fVertExportFile;
        fActiveExportFile2 := fVertExportFile2;
        fActiveExportFile3 := fVertExportFile3;
        fActiveExportCodeNameCount := fVertExportCodeNameCount;
        fActiveExportCodeNameCount2 := fVertExportCodeNameCount2;
        fActiveExportCodeNameCount3 := fVertExportCodeNameCount3;
        fSourceMode := 1;
        fCompareSpacing := fVertCompareSpacing;

        BuildSource;
        Compare;
        BuildCodeName;
        BuildRowSpacing;
        ExportData;
      end;
      if Length(fSlantGroupCounts) > 0 then
      begin
        fActiveGroupCounts := fSlantGroupCounts;
        fActiveExportFile := fSlantExportFile;
        fActiveExportFile2 := fSlantExportFile2;
        fActiveExportFile3 := fSlantExportFile3;
        fActiveExportCodeNameCount := fSlantExportCodeNameCount;
        fActiveExportCodeNameCount2 := fSlantExportCodeNameCount2;
        fActiveExportCodeNameCount3 := fSlantExportCodeNameCount3;
        fSourceMode := 2;
        fCompareSpacing := fSlantCompareSpacing;

        BuildSource;
        Compare;
        BuildCodeName;
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
  fRestSettings := TSQLRestServerDB.CreateWithOwnModel(
    [TSQLKeyValue],
    fDirectory + 'Data'
  );
  fRestSettings.CreateMissingTables;
  fKeyValue := TSQLKeyValue.Create;
  fKeyValue.SetRest(fRestSettings);

finalization
  if Assigned(fKeyValue) then FreeAndNil(fKeyValue);
  if Assigned(fRestSettings) then FreeAndNil(fRestSettings);

end.
