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
  uFileWriter,
  System.JSON.Serializers;

type
  TSQLSource = class(TSQLRow)
   protected
    {fRowCount: Cardinal;
    fMaxValueCount: Word;
    fMaxValueCount2: Word;
    fRowSpacing: Cardinal;
    fValueCountGroup: RawUTF8;
    fProcessed: Boolean;}
  published
    {property RowCount: Cardinal read fRowCount write fRowCount;
    property MaxValueCount: Word read fMaxValueCount write fMaxValueCount;
    property MaxValueCount2: Word read fMaxValueCount2 write fMaxValueCount2;
    property RowSpacing: Cardinal read fRowSpacing write fRowSpacing;
    property ValueCountGroup: RawUTF8 read fValueCountGroup write fValueCountGroup;
    property Processed: Boolean read fProcessed write fProcessed;}
  end;

  TSQLCompareData = class(TSQLData)
  protected
    fFlag: Byte;
    fGroupNumber: Byte;
    fNumber: Cardinal;
    fGroupCodeName: RawUTF8;
    fGroupCodeNameValueCount: Byte;
    fCodeName: RawUTF8;
    fCodeNameValueCount: Byte;
    fRowSpacing: Cardinal;
    fEqualDifferenceValues: TWordDynArray;
    fZeroValueCount: Boolean;
  published
    property Flag: Byte read fFlag write fFlag;
    property GroupNumber: Byte read fGroupNumber write fGroupNumber;
    property Number: Cardinal read fNumber write fNumber;
    property GroupCodeName: RawUTF8 read fGroupCodeName write fGroupCodeName;
    property GroupCodeNameValueCount: Byte read fGroupCodeNameValueCount write fGroupCodeNameValueCount;
    property CodeName: RawUTF8 read fCodeName write fCodeName;
    property CodeNameValueCount: Byte read fCodeNameValueCount write fCodeNameValueCount;
    property RowSpacing: Cardinal read fRowSpacing write fRowSpacing;
    property EqualDifferenceValues: TWordDynArray read fEqualDifferenceValues write fEqualDifferenceValues;
    property ZeroValueCount: Boolean read fZeroValueCount write fZeroValueCount;
  end;

  TSQLCombineData = class(TSQLRecord)
  protected
    fFlag: Byte;
    fGroupNumber: Byte;
    fNumber: Cardinal;
    fRowSpacing: Cardinal;
    fValue: RawUTF8;
    fValueCount: Word;
  published
    property Flag: Byte read fFlag write fFlag;
    property GroupNumber: Byte read fGroupNumber write fGroupNumber;
    property Number: Cardinal read fNumber write fNumber;
    property RowSpacing: Cardinal read fRowSpacing write fRowSpacing;
    property Value: RawUTF8 read fValue write fValue;
    property ValueCount: Word read fValueCount write fValueCount;
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

  TSQLSource2 = class(TSQLRow)
   protected
    fProcessed: Boolean;
  published
    property Processed: Boolean read fProcessed write fProcessed;
  end;

  TSQLCompare = class(TSQLRow);

  TSQLCompareSub = class(TSQLRecord)
  protected
    fValue: Cardinal;
    fNumber: Cardinal;
    fMaxValueCount: Word;
    fMaxValueCount2: Word;
  published
    property Value: Cardinal read fValue write fValue;
    property Number: Cardinal read fNumber write fNumber;
    property MaxValueCount: Word read fMaxValueCount write fMaxValueCount;
    property MaxValueCount2: Word read fMaxValueCount2 write fMaxValueCount2;
  end;

  TSQLCompareData2 = class(TSQLRecord)
  protected
    fSourceNumber: Cardinal;
    fSourceSubNumber: Cardinal;
    fCompareNumber: Cardinal;
    fCompareSubNumber: Cardinal;
    fRowSpacing: Integer;
    fPriorSameSpacingNumber: Cardinal;
    fRowSpacing2: Integer;
    fNextSameSpacingNumber: Cardinal;
    fRowSpacing3: Integer;
    //fOneRowSpacingSourceNumber: Cardinal;
    fPriorOneRowSpacingNumber: Cardinal;
    //fOneRowSpacingSourceNumber2: Cardinal;
    fNextOneRowSpacingNumber: Cardinal;
    fLastNextOneRowSpacingNumber: Boolean;
  published
    property SourceNumber: Cardinal read fSourceNumber write fSourceNumber;
    property SourceSubNumber: Cardinal read fSourceSubNumber write fSourceSubNumber;
    property CompareNumber: Cardinal read fCompareNumber write fCompareNumber;
    property CompareSubNumber: Cardinal read fCompareSubNumber write fCompareSubNumber;
    property RowSpacing: Integer read fRowSpacing write fRowSpacing;
    property PriorSameSpacingNumber: Cardinal read fPriorSameSpacingNumber write fPriorSameSpacingNumber;
    property RowSpacing2: Integer read fRowSpacing2 write fRowSpacing2;
    property NextSameSpacingNumber: Cardinal read fNextSameSpacingNumber write fNextSameSpacingNumber;
    property RowSpacing3: Integer read fRowSpacing3 write fRowSpacing3;
    //property OneRowSpacingSourceNumber: Cardinal read fOneRowSpacingSourceNumber write fOneRowSpacingSourceNumber;
    property PriorOneRowSpacingNumber: Cardinal read fPriorOneRowSpacingNumber write fPriorOneRowSpacingNumber;
    //property OneRowSpacingSourceNumber2: Cardinal read fOneRowSpacingSourceNumber2 write fOneRowSpacingSourceNumber2;
    property NextOneRowSpacingNumber: Cardinal read fNextOneRowSpacingNumber write fNextOneRowSpacingNumber;
    property LastNextOneRowSpacingNumber: Boolean read fLastNextOneRowSpacingNumber write fLastNextOneRowSpacingNumber;
  end;

  TSQLCompareDataRowSpacing = class(TSQLRecord)
  protected
    fValue: Integer;
    fRowCount: Cardinal;
    fMaxNumber: Cardinal;
    fMinNumber: Cardinal;
    fMaxRowSpacing: Cardinal;
    fDifferenceValue: Integer;
  published
    property Value: Integer read fValue write fValue;
    property RowCount: Cardinal read fRowCount write fRowCount;
    property MaxNumber: Cardinal read fMaxNumber write fMaxNumber;
    property MinNumber: Cardinal read fMinNumber write fMinNumber;
    property MaxRowSpacing: Cardinal read fMaxRowSpacing write fMaxRowSpacing;
    property DifferenceValue: Integer read fDifferenceValue write fDifferenceValue;
  end;

  TGroupCount = record
    Number: Byte;
    Value: Word;
    Value2: Word;
    Value3: Word;
    Value4: Word;
    Value5: Word;
    Value6: Word;
    Value7: Word;
    Value8: Word;
    function GetGroupValue(Flag: Byte; TotalCount, GroupNumber: Word): TWordDynArray;
  end;

  TSettings = record
    Flag: Byte;
    GroupCounts: TArray<TGroupCount>;
    ExportFile: Boolean;
    ExportFile2: Boolean;
    ExportFile3: Boolean;
    ExportFile4: Boolean;
    ExportCodeNameCount: Cardinal;
    ExportCodeNameCount2: Cardinal;
    ExportCodeNameCount3: Cardinal;
  end;

  TDataComputer = class(TThread)
  private
    fStopwatch: TStopwatch;
    fRest: TSQLRestServerDB;
    fSetDataMode: Boolean;
    fDataMode: Byte;
    fSourceMode: Byte;
    fTotalIntervalValue: Word;
    fMaxSourceNumber: Cardinal;
    fMaxSourceNumber2: Cardinal;
    fMaxCompareNumber: Cardinal;

    fActiveSettings: TSettings;
    fSettings: TArray<TSettings>;
    fCompareSpacing: Word;

    fIntervalValues: TWordDynArray;
    fSourceFileDirectory: string;
    fExportDirectory: string;

    fVertCompareSpacing: Word;
    fVertSameValueCount: Word;
    fVertSameValueCount2: Word;
    fCompareCrossRange: Boolean;
    fSlantCompareSpacing: Word;
    fSlantSameValueCount: Word;
    fSlantSameValueCount2: Word;

    fSourceFileName: string;
    fCompareFileName: string;
    fDeleteProcessed: Boolean;
    fExportFile254: Boolean;
    fExportFile255: Boolean;
    fExportOneRowSpacingFile: Boolean;
    fExportOnlyOneRowSpacingFile: Boolean;
    fTakePartRow: Boolean;
    fRowRange: Word;
    fRowRange2: Word;
    fRowRange3: Word;
    fRowRange4: Word;

    fFlagCount: Integer;
    fRange16: string;
    fRange17: string;
    fRange18: string;
    fRange19: string;
    fRange20: string;
    fRange21: string;
    fRange22: string;

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
    procedure BuildCombineData(Flag, GroupNumber: Byte); overload;
    procedure BuildCombineData2(Flag: Byte);
    procedure BuildCombineData; overload;
    procedure BuildCompareDataRowSpacing;
    procedure BuildOneRowSpacingNumber;
    procedure ExportData(Flag, GroupNumber, GroupCodeNameValueCount, CodeNameValueCount, FileFlag, FileSerialNo: Byte); overload;
    procedure ExportData; overload;
    procedure ExportCombineData(Flag, GroupNumber, FileFlag: Byte); overload;
    procedure ExportCombineData; overload;
    procedure ExportOneRowSpacingData(FileName: string; OnlyOneRowSpacing: Boolean; Flag: Byte); overload;
    procedure ExportOneRowSpacingData; overload;
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
    property Settings: TArray<TSettings> read fSettings write fSettings;
    property DeleteProcessed: Boolean read fDeleteProcessed write fDeleteProcessed;
    property VertCompareSpacing: Word read fVertCompareSpacing write fVertCompareSpacing;
    property VertSameValueCount: Word read fVertSameValueCount write fVertSameValueCount;
    property VertSameValueCount2: Word read fVertSameValueCount2 write fVertSameValueCount2;
    property CompareCrossRange: Boolean read fCompareCrossRange write fCompareCrossRange;
    property SlantCompareSpacing: Word read fSlantCompareSpacing write fSlantCompareSpacing;
    property SlantSameValueCount: Word read fSlantSameValueCount write fSlantSameValueCount;
    property SlantSameValueCount2: Word read fSlantSameValueCount2 write fSlantSameValueCount2;
    property ExportFile254: Boolean read fExportFile254 write fExportFile254;
    property ExportFile255: Boolean read fExportFile255 write fExportFile255;
    property ExportOneRowSpacingFile: Boolean read fExportOneRowSpacingFile write fExportOneRowSpacingFile;
    property ExportOnlyOneRowSpacingFile: Boolean read fExportOnlyOneRowSpacingFile write fExportOnlyOneRowSpacingFile;
    property TakePartRow: Boolean read fTakePartRow write fTakePartRow;
    property RowRange: Word read fRowRange write fRowRange;
    property RowRange2: Word read fRowRange2 write fRowRange2;
    property RowRange3: Word read fRowRange3 write fRowRange3;
    property RowRange4: Word read fRowRange4 write fRowRange4;
  end;

var
  fDataComputer: TDataComputer;
  fRestSettings: TSQLRestServerDB;
  fKeyValue: TSQLKeyValue;
  fSerializer: TJsonSerializer;

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
  fRest := TSQLRestServerDB.CreateWithOwnModel([
    TSQLRow, TSQLSource, TSQLCompareData, TSQLCodeName, TSQLCombineData,
    TSQLSource2, TSQLCompare, TSQLCompareSub, TSQLCompareData2, TSQLCompareDataRowSpacing
  ]);
  fRest.CreateMissingTables;
  fRest.CreateSQLIndex(TSQLCodeName, 'Flag', False);
  fRest.CreateSQLIndex(TSQLCompareData, 'Flag', False);
  fRest.CreateSQLIndex(TSQLCompareData, 'GroupNumber', False);
  fRest.CreateSQLIndex(TSQLCombineData, 'Flag', False);
  fRest.CreateSQLIndex(TSQLCombineData, 'GroupNumber', False);
end;

procedure TDataComputer.LoadRow(Row: TSQLRow; FileName: string);

  function Extract(s: string): string;
  var
    i, iLeft: Integer;
  begin
    Result := s;
    iLeft := 0;
    for i := 1 to Length(s) do
    begin
      if (s[i] = '(') or (s[i] = '（') then iLeft := i
      else if (s[i] = '列') or (s[i] = '差')  then
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
begin
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
          if fSetDataMode then
          begin
            if SpecialMode(s) then fDataMode := 1;
            fSetDataMode := False;
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
      fSourceFileName := FileName
    else if FileName.IndexOf('3. 读取') > -1 then
      fCompareFileName := FileName
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
    s2: TArray<string>;
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
  Source: TSQLSource2;
  Compare, Compare2: TSQLCompare;
  CompareSub: TSQLCompareSub;
  s, FileName, s2, s3, s4: string;
  i, j, Digit, SubNumber, ValueCount, ValueCount2, MaxCompareSubNumber: Integer;
  r, r2, r3: TCardinalDynArray;
  v: Cardinal;
begin
  if Terminated then Exit;

  TSQLSource2.AutoFree(Source);
  LoadRow(Source, fSourceFileName);

  fMaxSourceNumber2 := 0;
  s := fRest.OneFieldValue(TSQLSource2, 'Max(Number)', '');
  if not s.IsEmpty then fMaxSourceNumber2 := StrToInt64(s);

  if FileExists(fCompareFileName) then
  begin
    TSQLCompare.AutoFree(Compare);
    TSQLCompare.AutoFree(Compare2);
    TSQLCompareSub.AutoFree(CompareSub);
    LoadRow(Compare, fCompareFileName);

    fMaxCompareNumber := 0;
    s := fRest.OneFieldValue(TSQLCompare, 'Max(Number)', '');
    if not s.IsEmpty then fMaxCompareNumber := StrToInt64(s);

    with TStringList.Create do
    begin
      try
        LoadFromFile(fCompareFileName);
        for i := Count - 1 downto 0 do
        begin
          if not TryStrToInt(Names[i].Trim, Digit) then Continue;
          s := Strings[i];
          s := Extract(s);

          s4 := Analysis(s);
          r.Assign(s4);
          r2.Add(r);

          for s4 in s.Split(['；']) do
          begin
            j := s4.IndexOf('=');
            if j = -1 then Continue;
            s := s4.Substring(0, j - 1).Trim;
            s2 := s4.Substring(j + 1);
            j := s2.IndexOf('+');
            if j = -1 then Continue;
            s3 := s2.Substring(j + 1).Trim;
            s2 := s2.Substring(0, j - 1).Trim;
            r.Assign(s);
            if (Length(r) > 0)
              and TryStrToInt(s2, ValueCount)
              and TryStrToInt(s3, ValueCount2)
            then
            begin
              for SubNumber in r do
              begin
                CompareSub.Value := SubNumber;
                CompareSub.Number := Digit;
                CompareSub.MaxValueCount := ValueCount;
                CompareSub.MaxValueCount2 := ValueCount2;
                fRest.Add(CompareSub, True);
              end;
            end;
          end;
        end;
      finally
        Free;
      end;
    end;

    Compare.FillPrepare(fRest, 'ORDER BY Number DESC', []);
    fRest.TransactionBegin(TSQLCompare);
    try
      while Compare.FillOne do
      begin
        if Compare.FillCurrentRow > Compare.FillTable.RowCount then
          Compare2.Number := 1
        else
          Compare.FillRow(Compare.FillCurrentRow, Compare2);
        Compare.RowSpacing := Compare.Number - Compare2.Number;
        fRest.Update(Compare);
      end;
      fRest.Commit(1, True);
    except
      on e: Exception do
      begin
        fRest.RollBack;
        raise;
      end;
    end;

    if fTakePartRow then
    begin
      MaxCompareSubNumber := 0;
      s := fRest.OneFieldValue(TSQLCompareSub, 'Max(Value)', '');
      if not s.IsEmpty then MaxCompareSubNumber := s.ToInteger;

      for i := MaxCompareSubNumber - fRowRange2 to MaxCompareSubNumber - fRowRange do r3.Add(i);
      for i := MaxCompareSubNumber + fRowRange3 to MaxCompareSubNumber + fRowRange4 do r3.Add(i);
      r3.Add(MaxCompareSubNumber);
    end;
  end;

  Source.FillPrepare(fRest, '', []);
  fRest.TransactionBegin(TSQLSource);
  try
    while Source.FillOne do
    begin
      if fTakePartRow and not r3.Exist(Source.Number) then
        fRest.Delete(TSQLSource2, Source.ID)
      else
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

      case fActiveSettings.Flag of
        12, 13, 14, 15:
        begin
          if Row.FillCurrentRow - 1 <= fCompareSpacing then Continue;

          Source.ClearValue;
          for i := Row.FillCurrentRow - 2 downto Row.FillCurrentRow - 1 - fCompareSpacing do
          begin
            Row.FillRow(i, Row2);
            Number := Row.FillCurrentRow - 1 - i;
            case fActiveSettings.Flag of
              12, 13:
              begin
                //直连
                if Number <= fVertCompareSpacing then
                begin
                  if VertCompareRow(Row, Row2) then
                    Source.AddValue(Row2.Values);
                end;
              end;
              14, 15:
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
  Source2: TSQLSource2;
  CompareData: TSQLCompareData;
  CompareData2: TSQLCompareData;
  i, Flag, ValueCount, v, v2, v3, v4, IntervalValue: Integer;
  gc: TGroupCount;
  Columns, CodeName, ValueCounts: TWordDynArray;
  s: string;
  Rows, BigRows, SmallRows, OddRows, EvenRows: TCardinalDynArray;
  IsConform: Boolean;
begin
  if Terminated then Exit;

  case fActiveSettings.Flag of
    1..8: TSQLSource2.AutoFree(Source2, fRest, 'ORDER BY Number', []);
    else TSQLSource.AutoFree(Source, fRest, 'ORDER BY Number', []);
  end;
  TSQLCompareData.AutoFree(CompareData);
  TSQLCompareData.AutoFree(CompareData2);
  CompareData.AssignValue('', fIntervalValues, fDataMode);
  CompareData.Flag := fActiveSettings.Flag;
  CompareData.GroupNumber := 0;

  case fActiveSettings.Flag of
    1..8:
    begin
      while Source2.FillOne do
      begin
        if fDeleteProcessed and Source2.Processed then Continue;

        if Source2.Number mod 2 = 0 then EvenRows.Add(Source2.Number)
        else OddRows.Add(Source2.Number);

        if Source2.Number > fMaxSourceNumber2 div 2 then BigRows.Add(Source2.Number)
        else SmallRows.Add(Source2.Number);
      end;
    end;
    10, 12, 14: Flag := 1;
    11, 13, 15: Flag := 2;
    else Flag := 0;
  end;

  fRest.TransactionBegin(TSQLCompareData);
  try
    //fRest.Delete(TSQLCompareData, '');

    case fActiveSettings.Flag of
      1..8:
      begin
        case fActiveSettings.Flag of
          1: Rows := OddRows;
          2: Rows := EvenRows;
          3: Rows := BigRows;
          4: Rows := SmallRows;
          5:
          begin
            Rows := OddRows;
            Rows.Add(BigRows);
          end;
          6:
          begin
            Rows := OddRows;
            Rows.Add(SmallRows);
          end;
          7:
          begin
            Rows := EvenRows;
            Rows.Add(BigRows);
          end;
          8:
          begin
            Rows := EvenRows;
            Rows.Add(SmallRows);
          end;
        end;

        Source2.FillRewind;
        while Source2.FillOne do
        begin
          if not Rows.Exist(Source2.Number) then Continue;

          CompareData.Number := Source2.Number;
          CompareData.AssignValue(Source2);
          fRest.Add(CompareData, True);
        end;
      end;
      9..15:
      begin
        for gc in fActiveSettings.GroupCounts do
        begin
          if Terminated then Exit;
          for i := gc.Value to gc.Value2 do
          begin
            if Terminated then Exit;

            SetLength(CodeName, i);
            CompareData.GroupNumber := gc.Number;
            CompareData.GroupCodeNameValueCount := i;
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
                Values.Add(gc.GetGroupValue(Flag, fTotalIntervalValue, Numbers[i]));

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

          CompareData2.FillPrepare(fRest, 'Flag =? AND GroupNumber = ?', [fActiveSettings.Flag, gc.Number]);
          fRest.Delete(TSQLCompareData, 'Flag =? AND GroupNumber = ?', [fActiveSettings.Flag, gc.Number]);
          while CompareData2.FillOne do
          begin
            CompareData.Flag := CompareData2.Flag;
            CompareData.GroupNumber := CompareData2.GroupNumber;
            CompareData.GroupCodeName := CompareData2.GroupCodeName;
            Columns := CompareData2.Values;
            for i := gc.Value3 to gc.Value4 do
            begin
              SetLength(CodeName, i);
              CompareData.CodeNameValueCount := i;
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
      end;
      16..22:
      begin
        while Source.FillOne do
        begin
          CompareData.ClearValue;
          CompareData.EqualDifferenceValues.Clear;
          ValueCount := 0;
          v3 := 0;
          SetLength(Rows, 0);
          SetLength(ValueCounts, fTotalIntervalValue);
          for i := Low(ValueCounts) to High(ValueCounts) do ValueCounts[i] := 0;
          for v in Source.Values do
          begin
            v2 := v;
            IntervalValue := fIntervalValues[0];
            if v > fIntervalValues[0] then
            begin
              v2 := v - fIntervalValues[0];
              IntervalValue := fIntervalValues[1];
            end;
            case fActiveSettings.Flag of
              16:
              begin
                if v2 mod 2 = 1 then
                begin
                  Inc(ValueCount);
                  CompareData.AddValue(v);
                end
              end;
              17:
              begin
                if v2 mod 2 = 0 then
                begin
                  Inc(ValueCount);
                  CompareData.AddValue(v);
                end
              end;
              18:
              begin
                if v2 > IntervalValue div 2 then
                begin
                  Inc(ValueCount);
                  CompareData.AddValue(v);
                end;
              end;
              19:
              begin
                if v2 <= IntervalValue div 2 then
                begin
                  Inc(ValueCount);
                  CompareData.AddValue(v);
                end;
              end;
              20:
              begin
                if (v3 > 0) and (v - v3 = 1) then
                begin
                  Inc(ValueCount);
                  CompareData.AddValue(v);
                  CompareData.AddValue(v3);
                end;
                v3 := v;
              end;
              21:
              begin
                if v3 > 0 then
                begin
                  v4 := v - v3;
                  ValueCounts[v4 - 1] := ValueCounts[v4 - 1] + 1;
                end;
                v3 := v;
              end;
              22:
              begin
                s := v2.ToString;
                s := s[s.Length];
                v3 := s.ToInteger;
                if (v3 >= fActiveSettings.GroupCounts[0].Value3)
                  and (v3 <= fActiveSettings.GroupCounts[0].Value4)
                  and not Rows.Exist(v3)
                then
                begin
                  Rows.Add(v3);
                  Inc(ValueCount);
                  CompareData.AddValue(v);
                end;
              end;
            end;
          end;
          case fActiveSettings.Flag of
            16..20, 22:
              IsConform := (ValueCount >= fActiveSettings.GroupCounts[0].Value)
                and (ValueCount <= fActiveSettings.GroupCounts[0].Value2);
            21:
            begin
              IsConform := False;
              //设置0时，先判断有没有相同差数
              if fActiveSettings.GroupCounts[0].Value = 0 then
              begin
                for i := Low(ValueCounts) to High(ValueCounts) do
                begin
                  IsConform := ValueCounts[i] < 2;
                  if not IsConform then Break;
                end;
              end;
              if not IsConform then
              begin
                //判相同差数个数是否符合范围
                for i := Low(ValueCounts) to High(ValueCounts) do
                begin
                  if ValueCounts[i] < 2 then Continue;

                  IsConform := (ValueCounts[i] >= fActiveSettings.GroupCounts[0].Value)
                    and (ValueCounts[i] <= fActiveSettings.GroupCounts[0].Value2);
                  if IsConform then
                  begin
                    CompareData.EqualDifferenceValues.Add(i + 1);
                    if ValueCounts[i] > ValueCount then ValueCount := ValueCounts[i];

                    v3 := 0;
                    for v in Source.Values do
                    begin
                      if (v3 > 0) and (v - v3 = i + 1) then
                      begin
                        CompareData.AddValue(v);
                        CompareData.AddValue(v3);
                      end;
                      v3 := v;
                    end;
                  end;
                end;
              end;
            end;
          end;
          if IsConform then
          begin
            CompareData.ZeroValueCount := False;
            if (ValueCount = 0) and (fActiveSettings.GroupCounts[0].Value = 0) then
            begin
              CompareData.AssignValue(Source);
              CompareData.ZeroValueCount := True;
            end;
            CompareData.CalcValueCount;
            CompareData.Number := Source.Number;
            CompareData.CodeName := ValueCount.ToString;
            {CompareData.GroupCodeName := '';
            for i := 1 to ValueCount2 do
            begin
              if CompareData.GroupCodeName <> '' then CompareData.GroupCodeName := CompareData.GroupCodeName + '、';
              CompareData.GroupCodeName := CompareData.GroupCodeName + i.ToString;
            end;}
            fRest.Add(CompareData, True);
          end;
        end;
      end;
    end;

    fRest.Commit(1, True);
  except
    fRest.RollBack;
    raise;
  end;
end;

procedure TDataComputer.Compare2;
var
  Compare, Compare2: TSQLCompare;
  CompareSub, CompareSub2: TSQLCompareSub;
  CompareData, CompareData2: TSQLCompareData2;
begin
  TSQLCompare.AutoFree(Compare, fRest, 'ORDER BY Number DESC', []);
  TSQLCompare.AutoFree(Compare2);
  TSQLCompareSub.AutoFree(CompareSub);
  TSQLCompareSub.AutoFree(CompareSub2);
  TSQLCompareData2.AutoFree(CompareData);
  TSQLCompareData2.AutoFree(CompareData2);
  fRest.TransactionBegin(TSQLCompareData2);
  try
    CompareData.PriorOneRowSpacingNumber := 0;
    CompareData.NextOneRowSpacingNumber := 0;
    while Compare.FillOne do
    begin
      if Compare.FillCurrentRow > Compare.FillTable.RowCount then
      else Compare.FillRow(Compare.FillCurrentRow, Compare2);
      CompareSub.FillPrepare(fRest, 'Number = ? ', [Compare.Number]);
      CompareSub2.FillPrepare(fRest, 'Number = ? ', [Compare2.Number]);

      CompareData.SourceNumber := Compare.Number;
      CompareData.CompareNumber := Compare2.Number;
      while CompareSub.FillOne do
      begin
        CompareData.SourceSubNumber := CompareSub.Value;

        CompareSub2.FillRewind;
        while CompareSub2.FillOne do
        begin
          CompareData.CompareSubNumber := CompareSub2.Value;
          CompareData.RowSpacing := CompareData.SourceSubNumber - CompareData.CompareSubNumber;

          fRest.Add(CompareData, True);
        end;
      end;
    end;

    CompareData.FillPrepare(fRest, '', []);
    while CompareData.FillOne do
    begin
      CompareData2.FillPrepare(fRest, 'RowSpacing = ? AND SourceNumber > ? ORDER BY SourceNumber LIMIT 1',
        [CompareData.RowSpacing, CompareData.SourceNumber]);
      if CompareData2.FillOne then
        CompareData.PriorSameSpacingNumber := CompareData2.SourceNumber
      else
        CompareData.PriorSameSpacingNumber := fMaxCompareNumber + 1;

      CompareData2.FillPrepare(fRest, 'RowSpacing = ? AND SourceNumber < ? ORDER BY SourceNumber DESC LIMIT 1',
        [CompareData.RowSpacing, CompareData.SourceNumber]);
      if CompareData2.FillOne then
        CompareData.NextSameSpacingNumber := CompareData2.SourceNumber
      else
        CompareData.NextSameSpacingNumber := 1;

      CompareData.RowSpacing2 := CompareData.SourceNumber - CompareData.PriorSameSpacingNumber;
      CompareData.RowSpacing3 := CompareData.SourceNumber - CompareData.NextSameSpacingNumber;

      fRest.Update(CompareData);
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
begin
  if Terminated then Exit;

  TSQLCodeName.AutoFree(CodeName);
  CodeName.Enabled := True;
  l := fRest.MultiFieldValues(TSQLCompareData,
    'Flag, GroupNumber, GroupCodeName, GroupCodeNameValueCount, CodeName, CodeNameValueCount, Max(Number), Min(Number), Count(Number)',
    'Flag = ? GROUP BY Flag, GroupNumber, GroupCodeName, GroupCodeNameValueCount, CodeName, CodeNameValueCount', [fActiveSettings.Flag]);
  try
    fRest.TransactionBegin(TSQLCodeName);
    try
      while l.Step do
      begin
        CodeName.Flag := l.FieldAsInteger(0);
        CodeName.GroupNumber := l.FieldAsInteger(1);
        CodeName.GroupCodeName := l.FieldAsRawUTF8(2);
        CodeName.GroupCodeNameValueCount := l.FieldAsInteger(3);
        CodeName.Value := l.FieldAsRawUTF8(4);
        CodeName.ValueCount := l.FieldAsInteger(5);
        CodeName.MaxNumber := l.FieldAsInteger(6);
        CodeName.MinNumber := l.FieldAsInteger(7);
        CodeName.RowCount := l.FieldAsInteger(8);

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
  MaxSourceNumber: Integer;
begin
  if Terminated then Exit;

  case fActiveSettings.Flag of
    1..8: MaxSourceNumber := fMaxSourceNumber2;
    else MaxSourceNumber := fMaxSourceNumber;
  end;

  TSQLCodeName.AutoFree(CodeName, fRest, 'Flag = ? ORDER BY RowCount DESC', [fActiveSettings.Flag]);
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

    CodeName.FillPrepare(fRest, 'Flag = ? ORDER BY Flag, GroupCodeNameValueCount, GroupNumber, ValueCount', [fActiveSettings.Flag]);
    while CodeName.FillOne and not Terminated do
    begin
      CodeName.DifferenceValue := MaxSourceNumber + 1 - CodeName.MaxNumber - CodeName.MaxRowSpacing;
      if fActiveSettings.ExportCodeNameCount > 0 then
        CodeName.Enabled := CodeName.FillCurrentRow - 1 <= fActiveSettings.ExportCodeNameCount;

      fRest.Update(CodeName);
    end;

    fRest.Commit(1, True);
  except
    fRest.RollBack;
    raise;
  end;
end;

procedure TDataComputer.BuildCombineData(Flag, GroupNumber: Byte);
var
  CompareData, CompareData2: TSQLCompareData;
  CombineData: TSQLCombineData;
  s: string;
  LastNumber, ValueCount, FlagCount: Integer;
  t: TWordDynArray;
  IsConform: Boolean;
begin
  case Flag of
    10, 11: s := Format('Flag = %d AND GroupNumber = %d', [Flag, GroupNumber]);
    16: s := 'Flag IN (16, 17)';
    18: s := 'Flag IN (18, 19)';
    9, 20, 21, 22: s := Format('Flag = %d', [Flag]);
    254, 255: s := 'Flag >= 10';
    else Exit;
  end;
  if Flag < 254 then Inc(fFlagCount);
  s := s + ' ORDER BY Number, Flag, GroupNumber, GroupCodeNameValueCount, CodeNameValueCount';
  TSQLCompareData.AutoFree(CompareData, fRest, s, []);
  TSQLCompareData.AutoFree(CompareData2);
  TSQLCombineData.AutoFree(CombineData);
  fRest.TransactionBegin(TSQLCombineData);
  try
    CombineData.Flag := Flag;
    CombineData.GroupNumber := GroupNumber;
    CombineData.Value := '';
    CombineData.ValueCount := 0;
    LastNumber := 0;
    while CompareData.FillOne and not Terminated do
    begin
      if CompareData.FillCurrentRow <= CompareData.FillTable.RowCount then
        CompareData.FillRow(CompareData.FillCurrentRow, CompareData2);

      ValueCount := CompareData.TotalValueCount;
      if CompareData.ZeroValueCount then ValueCount := 0;
      case CompareData.Flag of
        9:
        begin
          s := '（%s个相同个数）';
          s := Format(s, [CompareData.CodeName]);
        end;
        10..11:
        begin
          s := '（%s个组合 %s个相同个数）';
          s := Format(s, [CompareData.GroupCodeName, CompareData.CodeName]);
        end;
        16:
        begin
          s := '（单数 %d 个）';
          s := Format(s, [ValueCount]);
        end;
        17:
        begin
          s := '（双数 %d 个）';
          s := Format(s, [ValueCount]);
        end;
        18:
        begin
          s := '（大号码 %d 个）';
          s := Format(s, [ValueCount]);
        end;
        19:
        begin
          s := '（小号码 %d 个）';
          s := Format(s, [ValueCount]);
        end;
        20:
        begin
          s := '（相连号码 %d 个）';
          s := Format(s, [ValueCount]);
        end;
        21:
        begin
          s := '（等差号码 %d 个）';
          s := Format(s, [ValueCount]);
        end;
        22:
        begin
          s := '（个数 %d 个）';
          s := Format(s, [ValueCount]);
        end;
      end;
      if CombineData.Value <> '' then
        CombineData.Value := CombineData.Value + '、';
      CombineData.Value := CombineData.Value + s;
      CombineData.ValueCount := CombineData.ValueCount + 1;

      if (CompareData.FillCurrentRow > CompareData.FillTable.RowCount)
        or (CompareData.Number <> CompareData2.Number)
      then
      begin
        IsConform := True;
        if Flag = 255 then
        begin
          FlagCount := StrToInt(fRest.OneFieldValue(TSQLCombineData, 'Count(*)', 'Number = ? AND Flag BETWEEN 9 AND 22', [CompareData.Number]));
          IsConform := FlagCount = fFlagCount;
        end;

        if IsConform then
        begin
          CombineData.Number := CompareData.Number;
          if LastNumber = 0 then CombineData.RowSpacing := CombineData.Number - 1
          else CombineData.RowSpacing := CombineData.Number - LastNumber;
          fRest.Add(CombineData, True);

          LastNumber := CombineData.Number;
        end;

        CombineData.Value := '';
        CombineData.ValueCount := 0;
      end;
    end;
    fRest.Commit(1, True);
  except
    fRest.RollBack;
    raise;
  end;
end;

procedure TDataComputer.BuildCombineData2(Flag: Byte);
var
  CombineData: TSQLCombineData;
  l: TSQLTableJSON;
  i, FlagCount, LastNumber: Integer;
  IsConform, IsAnd: Boolean;
  t: TWordDynArray;
  s: string;
begin
  if Terminated then Exit;

  TSQLCombineData.AutoFree(CombineData);

  l := fRest.MultiFieldValues(TSQLCompareData, 'Number, Flag',
    'Flag >= 9 GROUP BY Number, Flag ORDER BY Number, Flag', []);
  try
    CombineData.Flag := Flag;
    CombineData.GroupNumber := 0;

    fRest.TransactionBegin(TSQLCombineData);
    try
      LastNumber := 0;
      FlagCount := 0;
      while l.Step and not Terminated do
      begin
        Inc(FlagCount);
        case l.FieldAsInteger(1) of
          9..12: t.Add(l.FieldAsInteger(1) - 7);
          16..22: t.Add(l.FieldAsInteger(1) - 9);
        end;

        if (l.StepRow = l.RowCount) or (l.FieldAsInteger(0) <> l.GetAsInteger(l.StepRow + 1, 0)) then
        begin
          IsConform := True;
          if Flag = 255 then IsConform := FlagCount = fFlagCount;
          if IsConform then
          begin
            CombineData.Number := l.FieldAsInteger(0);
            if LastNumber = 0 then CombineData.RowSpacing := CombineData.Number - 1
            else CombineData.RowSpacing := CombineData.Number - LastNumber;

            IsAnd := False;
            s := '';
            for i := Low(t) to High(t) do
            begin
              case i of
                0: s := Format('（ %d ）项', [t[i]]);
                else
                begin
                  IsAnd := t[i] - t[i - 1] = 1;
                  if IsAnd and (i < High(t)) and (t[i + 1] - t[i] = 1) then Continue;

                  if IsAnd then s := s.Substring(0, s.Length - 1) + '-'
                  else s := s + '、';
                  s := s + Format('（ %d ）项', [t[i]]);
                end;
              end;
            end;
            CombineData.Value := s;
            CombineData.ValueCount := Length(t);

            fRest.Add(CombineData, True);

            LastNumber := CombineData.Number;
          end;

          t.Clear;
        end;
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

procedure TDataComputer.BuildCombineData;
var
  fs: TSettings;
  gc: TGroupCount;
begin
  fFlagCount := 0;
  for fs in fSettings do
    if fs.ExportFile4 then
      for gc in fs.GroupCounts do BuildCombineData(fs.Flag, gc.Number);
  if fExportFile254 then
    BuildCombineData2(254);
  if fExportFile255 then
    BuildCombineData2(255);
end;

procedure TDataComputer.BuildCompareDataRowSpacing;
var
  CompareDataRowSpacing: TSQLCompareDataRowSpacing;
  l: TSQLTableJSON;
begin
  if Terminated then Exit;

  TSQLCompareDataRowSpacing.AutoFree(CompareDataRowSpacing);
  l := fRest.MultiFieldValues(TSQLCompareData2,
    'RowSpacing, Count(*), Max(SourceNumber), Min(SourceNumber), Max(Abs(RowSpacing2)), Max(Abs(RowSpacing3))',
    'GROUP BY RowSpacing', []);
  try
    fRest.TransactionBegin(TSQLCompareDataRowSpacing);
    try
      while l.Step do
      begin
        CompareDataRowSpacing.Value := l.FieldAsInteger(0);
        CompareDataRowSpacing.RowCount := l.FieldAsInteger(1);
        CompareDataRowSpacing.MaxNumber := l.FieldAsInteger(2);
        CompareDataRowSpacing.MinNumber := l.FieldAsInteger(3);
        CompareDataRowSpacing.MaxRowSpacing := l.FieldAsInteger(4);
        if CompareDataRowSpacing.MaxRowSpacing < l.FieldAsInteger(5) then
          CompareDataRowSpacing.MaxRowSpacing := l.FieldAsInteger(5);
        CompareDataRowSpacing.DifferenceValue := fMaxCompareNumber + 1 - CompareDataRowSpacing.MaxNumber - CompareDataRowSpacing.MaxRowSpacing;

        fRest.Add(CompareDataRowSpacing, True);
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

procedure TDataComputer.BuildOneRowSpacingNumber;
var
  CompareDataRowSpacing: TSQLCompareDataRowSpacing;
  CompareData, CompareData2: TSQLCompareData2;
  OneRowSpacingNumber, LastSourceNumber: Integer;
begin
  if Terminated then Exit;

  TSQLCompareData2.AutoFree(CompareData);
  TSQLCompareData2.AutoFree(CompareData2);
  TSQLCompareDataRowSpacing.AutoFree(CompareDataRowSpacing, fRest, '', []);
  while CompareDataRowSpacing.FillOne do
  begin
    LastSourceNumber := 0;
    OneRowSpacingNumber := 0;
    CompareData.FillPrepare(fRest, 'RowSpacing = ? AND RowSpacing2 = -1 AND PriorSameSpacingNumber <= ? ORDER BY SourceNumber DESC',
      [CompareDataRowSpacing.Value, fMaxCompareNumber]);
    while CompareData.FillOne do
    begin
      Inc(OneRowSpacingNumber);
      //if OneRowSpacingNumber = 1 then
      //  OneRowSpacingSourceNumber := CompareData.PriorSameSpacingNumber;

      CompareData.PriorOneRowSpacingNumber := OneRowSpacingNumber;
      //CompareData.OneRowSpacingSourceNumber := OneRowSpacingSourceNumber;
      fRest.Update(CompareData);

      if LastSourceNumber - CompareData.SourceNumber > 1 then
        OneRowSpacingNumber := 0;
      LastSourceNumber := CompareData.SourceNumber;
    end;

    LastSourceNumber := 0;
    OneRowSpacingNumber := 0;
    CompareData.FillPrepare(fRest, 'RowSpacing = ? AND RowSpacing3 = 1 ORDER BY SourceNumber DESC', [CompareDataRowSpacing.Value]);
    while CompareData.FillOne do
    begin
      if CompareData.FillCurrentRow <= CompareData.FillTable.RowCount then
        CompareData.FillRow(CompareData.FillCurrentRow, CompareData2);
      Inc(OneRowSpacingNumber);
      //if OneRowSpacingNumber = 1 then
      //  OneRowSpacingSourceNumber := CompareData.SourceNumber;

      CompareData.NextOneRowSpacingNumber := OneRowSpacingNumber;
      //CompareData.OneRowSpacingSourceNumber2 := OneRowSpacingSourceNumber;
      CompareData.LastNextOneRowSpacingNumber := (CompareData.FillCurrentRow > CompareData.FillTable.RowCount)
        or (CompareData.SourceNumber - CompareData2.SourceNumber > 1);
      fRest.Update(CompareData);

      if CompareData.SourceNumber - LastSourceNumber < -1 then
        OneRowSpacingNumber := 0;
      LastSourceNumber := CompareData.SourceNumber;
    end;
  end;
end;

procedure TDataComputer.ExportData(Flag, GroupNumber, GroupCodeNameValueCount, CodeNameValueCount, FileFlag, FileSerialNo: Byte);
var
  fr: TFileWriter;
  CodeName: TSQLCodeName;
  Source: TSQLSource;
  CompareData: TSQLCompareData;
  s, sValue, FileName, FileName2, sMax: string;
  MaxSourceNumber, iMin, iMax: Integer;
begin
  if Terminated then Exit;

  case Flag of
    1:
    begin
      FileName := '（1）（1-1）. [ 查询（行号）单；（各）行之间 ，（行间差 ）].txt';
    end;
    2:
    begin
      FileName := '（1）（1-2）. [ 查询（行号）双；（各）行之间 ，（行间差 ）].txt';
    end;
    3:
    begin
      FileName := '（1）（1-3）. [ 查询（行号）大；（各）行之间 ，（行间差 ）].txt';
    end;
    4:
    begin
      FileName := '（1）（1-4）. [ 查询（行号）小；（各）行之间 ，（行间差 ）].txt';
    end;
    5:
    begin
      FileName := '（1）（1-5）. [ 查询（行号）单 、大；（各）行之间 ，（行间差 ）].txt';
    end;
    6:
    begin
      FileName := '（1）（1-6）. [ 查询（行号）单 、小；（各）行之间 ，（行间差 ）].txt';
    end;
    7:
    begin
      FileName := '（1）（1-7）. [ 查询（行号）双 、大；（各）行之间 ，（行间差 ）].txt';
    end;
    8:
    begin
      FileName := '（1）（1-8）. [ 查询（行号）双 、小；（各）行之间 ，（行间差 ）].txt';
    end;
    9:
    begin
      FileName := '（2）（0-1）. （%d个相同）列数字：.txt';
      FileName := Format(FileName, [CodeNameValueCount]);
    end;
    10:
    begin
      FileName := '（3）（1-1）. [（分 %d 段）... %d个组合（段）]（%d个相同）列数字：.txt';
      FileName := Format(FileName, [GroupNumber, GroupCodeNameValueCount, CodeNameValueCount]);
    end;
    11:
    begin
      FileName := '（4）（1-1）. [（除以 %d）... %d个组合（组）]（%d个相同）列数字：.txt';
      FileName := Format(FileName, [GroupNumber, GroupCodeNameValueCount, CodeNameValueCount]);
    end;
    12:
    begin
      FileName := '（5）（1-1）. [（直连码）（分 %d 段）... %d个组合（段）]（%d个相同）列数字：.txt';
      FileName := Format(FileName, [GroupNumber, GroupCodeNameValueCount, CodeNameValueCount]);
    end;
    13:
    begin
      FileName := '（5）（2-1）. [（直连码）（除以 %d）... %d个组合（组）]（%d个相同）列数字：.txt';
      FileName := Format(FileName, [GroupNumber, GroupCodeNameValueCount, CodeNameValueCount]);
    end;
    14:
    begin
      FileName := '（6）（1-1）. [（斜连码）（分 %d 段）... %d个组合（段）]（%d个相同）列数字：.txt';
      FileName := Format(FileName, [GroupNumber, GroupCodeNameValueCount, CodeNameValueCount]);
    end;
    15:
    begin
      FileName := '（6）（2-1）. [（斜连码）（除以 %d）... %d个组合（组）]（%d个相同）列数字：.txt';
      FileName := Format(FileName, [GroupNumber, GroupCodeNameValueCount, CodeNameValueCount]);
    end;
    16:
    begin
      FileName := '（7）（%d-%d）. [ 查询（单数），（各）行之间，（[个数范围]个） 相同个数 ].txt';
      FileName := Format(FileName, [FileSerialNo, FileFlag + 1]);
    end;
    17:
    begin
      FileName := '（8）（%d-%d）. [ 查询（双数），（各）行之间，（[个数范围]个） 相同个数 ].txt';
      FileName := Format(FileName, [FileSerialNo, FileFlag + 1]);
    end;
    18:
    begin
      FileName := '（9）（%d-%d）. [ 查询（大号码），（各）行之间，（[个数范围]个） 相同个数 ].txt';
      FileName := Format(FileName, [FileSerialNo, FileFlag + 1]);
    end;
    19:
    begin
      FileName := '（10）（%d-%d）. [ 查询（小号码），（各）行之间，（[个数范围]个） 相同个数 ].txt';
      FileName := Format(FileName, [FileSerialNo, FileFlag + 1]);
    end;
    20:
    begin
      FileName := '（11）（%d-%d）. [ 查询（相连号码）... [个数范围]个（相同个数） ].txt';
      FileName := Format(FileName, [FileSerialNo, FileFlag + 1]);
    end;
    21:
    begin
      FileName := '（12）（%d-%d）. [ 查询（等差号码）... [个数范围]个（相同个数） ].txt';
      FileName := Format(FileName, [FileSerialNo, FileFlag + 1]);
    end;
    22:
    begin
      FileName := '（13）（%d-%d）. [ 查询（个位）... [个数范围]个（相同个位个数）].txt';
      FileName := Format(FileName, [FileSerialNo, FileFlag + 1]);
    end;
  end;
  case fActiveSettings.Flag of
    16..22:
    begin
      iMin := fActiveSettings.GroupCounts[0].Value2;
      iMax := fActiveSettings.GroupCounts[0].Value;
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
        if fActiveSettings.ExportCodeNameCount > 0 then
          s := s + ' AND Enabled = 1';
        s := s + ' ORDER BY MaxRowSpacing DESC';
        if fActiveSettings.ExportCodeNameCount2 > 0 then
          s := s + Format(' LIMIT %d', [fActiveSettings.ExportCodeNameCount2]);
      end;
      2:
      begin
        if fActiveSettings.ExportCodeNameCount > 0 then
          s := s + ' AND Enabled = 1';
        s := s + ' ORDER BY DifferenceValue DESC';
        if fActiveSettings.ExportCodeNameCount3 > 0 then
          s := s + Format(' LIMIT %d', [fActiveSettings.ExportCodeNameCount3]);
      end;
      else
      begin
        s := s + ' ORDER BY RowCount DESC';
        if fActiveSettings.ExportCodeNameCount > 0 then
          s := s + Format(' LIMIT %d', [fActiveSettings.ExportCodeNameCount]);
      end;
    end;

    CodeName.FillPrepare(fRest, s, [Flag, GroupNumber, GroupCodeNameValueCount, CodeNameValueCount]);
    while CodeName.FillOne and not Terminated do
    begin
      case fActiveSettings.Flag of
        16..22:
        begin
          if fActiveSettings.Flag = 17 then
            fActiveSettings.Flag := 17;
          if StrToInt(CodeName.Value) < iMin then iMin := StrToInt(CodeName.Value);
          if StrToInt(CodeName.Value) > iMax then iMax := StrToInt(CodeName.Value);
        end;
      end;

      if CodeName.FillCurrentRow = 2 then
      begin
        case FileFlag of
          1: FileName2 := FileName.Replace('.txt', Format(' ( 注：最大行间差：%d ) .txt', [CodeName.MaxRowSpacing]));
          2: FileName2 := FileName.Replace('.txt', Format(' ( 注：最新 - 最大行间差 = %d ).txt', [CodeName.DifferenceValue]));
          else FileName2 := FileName.Replace('.txt', Format('  [ 注：最多行数：共 %d 行 ] .txt', [CodeName.RowCount]));
        end;
      end;

      if Flag > 8 then
      begin
        CompareData.AssignValue(CodeName.Value);
        CompareData.IntervalValues := fIntervalValues;

        case Flag of
          9:
          begin
            s := '%d.（ 列组合 ；第 %d 个组合 ，列数字 ：%s ），共 %d 行 ：';
            s := Format(s, [
              CodeName.FillCurrentRow - 1,
              CodeName.ValueCount,
              CompareData.ToString,
              CodeName.RowCount
            ]);
          end;
          10, 12, 14:
          begin
            s := '%d.（ 分 %d 段 ，第 %s 段 ，%d 个相同列数字 ：%s ），共 %d 行 ：';
            s := Format(s, [
              CodeName.FillCurrentRow - 1,
              GroupNumber,
              CodeName.GroupCodeName,
              CodeName.ValueCount,
              CompareData.ToString,
              CodeName.RowCount
            ]);
          end;
          11, 13, 15:
          begin
            s := '%d.（ 除以 %d ＝ 第 %s 组, %d 个相同列数字 ：%s ），共 %d 行 ：';
            s := Format(s, [
              CodeName.FillCurrentRow - 1,
              GroupNumber,
              CodeName.GroupCodeName,
              CodeName.ValueCount,
              CompareData.ToString,
              CodeName.RowCount
            ]);
          end;
          16:
          begin
            s := '%d.[（单数），（各）行之间，%s个（相同）个数 ] ，共 %d  行 ：';
            s := Format(s, [
              CodeName.FillCurrentRow - 1,
              CodeName.Value,
              CodeName.RowCount
            ]);
          end;
          17:
          begin
            s := '%d.[（双数），（各）行之间，%s个（相同）个数 ] ，共 %d  行 ：';
            s := Format(s, [
              CodeName.FillCurrentRow - 1,
              CodeName.Value,
              CodeName.RowCount
            ]);
          end;
          18:
          begin
            s := '%d.[（大号码），（各）行之间，%s个（相同）个数 ] ，共 %d  行 ：';
            s := Format(s, [
              CodeName.FillCurrentRow - 1,
              CodeName.Value,
              CodeName.RowCount
            ]);
          end;
          19:
          begin
            s := '%d.[（小号码），（各）行之间，%s个（相同）个数 ] ，共 %d  行 ：';
            s := Format(s, [
              CodeName.FillCurrentRow - 1,
              CodeName.Value,
              CodeName.RowCount
            ]);
          end;
          20:
          begin
            s := '%d.[（相连号码），（各）行之间，%s个（相同）个数 ] ，共 %d  行 ：';
            s := Format(s, [
              CodeName.FillCurrentRow - 1,
              CodeName.Value,
              CodeName.RowCount
            ]);
          end;
          21:
          begin
            s := '%d.[（等差号码），（各）行之间，%s个（相同）个数 ] ，共 %d  行 ：';
            s := Format(s, [
              CodeName.FillCurrentRow - 1,
              CodeName.Value,
              CodeName.RowCount
            ]);
          end;
          22:
          begin
            s := '%d.[（个位），（各）行之间，%s个（相同）个数 ] ，共 %d  行 ：';
            s := Format(s, [
              CodeName.FillCurrentRow - 1,
              CodeName.Value,
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

      case Flag of
        1..8: MaxSourceNumber := fMaxSourceNumber2;
        else MaxSourceNumber := fMaxSourceNumber;
      end;

      s := '[ 全部行的首行（ 第 %d 行 ）（ 加上 + ）1 ] = [ 第 %d 行 ]（ 减去 - ）[ 该组首行（ 第 %d 行 ）] =（ 最新行间差 ：%d ）- ( 最大行间差 ：%d ) = ( 行间差 ：%d )';
      s := Format(s, [
        MaxSourceNumber,
        MaxSourceNumber + 1,
        CodeName.MaxNumber,
        MaxSourceNumber + 1 - CodeName.MaxNumber,
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

        case fActiveSettings.Flag of
          20: sValue := CompareData.ToString2([1]);
          21: sValue := CompareData.ToString2(CompareData.EqualDifferenceValues);
          else sValue := CompareData.ToString;
        end;

        s := '%d=%s ( %s行间差 ：%d ) ';
        s := Format(s, [CompareData.Number, sValue, sMax, CompareData.RowSpacing]);
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
    case fActiveSettings.Flag of
      16..22:
      begin
        s := Format('%d-%d', [iMin, iMax]);
        FileName2 := FileName2.Replace('[个数范围]', s);
        case fActiveSettings.Flag of
          16: fRange16 := s;
          17: fRange17 := s;
          18: fRange18 := s;
          19: fRange19 := s;
          20: fRange20 := s;
          21: fRange21 := s;
          22: fRange22 := s;
        end;
      end;
    end;
    if TFile.Exists(FileName2) then TFile.Delete(FileName2);
    RenameFile(FileName, FileName2);
  end;
end;

procedure TDataComputer.ExportData;
var
  l: TSQLTableJSON;
  fs: TSettings;
  FileSerialNo, LastFlag: Integer;
begin
  if Terminated then Exit;

  l := fRest.MultiFieldValues(TSQLCodeName, 'Flag, GroupNumber, GroupCodeNameValueCount, ValueCount',
    'GROUP BY Flag, GroupNumber, GroupCodeNameValueCount, ValueCount ORDER BY Flag, GroupNumber, GroupCodeNameValueCount, ValueCount', []);
  try
    LastFlag := 0;
    while l.Step and not Terminated do
    begin
      if LastFlag <> l.FieldAsInteger(0) then
      begin
        LastFlag := l.FieldAsInteger(0);
        FileSerialNo := 0;
      end;
      Inc(FileSerialNo);

      for fs in fSettings do
      begin
        if fs.Flag = l.FieldAsInteger(0) then
        begin
          fActiveSettings := fs;
          Break;
        end;
      end;

      if fActiveSettings.ExportFile then
        ExportData(l.FieldAsInteger(0), l.FieldAsInteger(1), l.FieldAsInteger(2), l.FieldAsInteger(3), 0, FileSerialNo);
      if fActiveSettings.ExportFile2 then
        ExportData(l.FieldAsInteger(0), l.FieldAsInteger(1), l.FieldAsInteger(2), l.FieldAsInteger(3), 1, FileSerialNo);
      if fActiveSettings.ExportFile3 then
        ExportData(l.FieldAsInteger(0), l.FieldAsInteger(1), l.FieldAsInteger(2), l.FieldAsInteger(3), 2, FileSerialNo);
    end;
  finally
    l.Free;
  end;
end;

procedure TDataComputer.ExportCombineData(Flag, GroupNumber, FileFlag: Byte);
var
  fr: TFileWriter;
  s, FileName, sMax, sValue: string;
  CombineData: TSQLCombineData;
  RowCount, MaxNumber, MinNumber, MaxRowSpacing, MaxSourceNumber, iCount, RowNumber: Integer;
  l: TSQLTableJSON;
begin
  if Terminated then Exit;

  FileName := '';
  case Flag of
    10:
    begin
       FileName := '（3）（0-0）. [（分 %d 段）]：【 各区域（合并）】.txt';
       FileName := Format(FileName, [GroupNumber]);
    end;
    11:
    begin
      FileName := '（4）（0-0）. [（除以 %d）]：【 各区域（合并）】.txt';
      FileName := Format(FileName, [GroupNumber]);
    end;
    16:
    begin
      FileName := '（7）（0-0）. [ 查询（单数：%s个）...（双数：%s个）相同个数 ]：【 各区域（合并）】.txt';
      FileName := Format(FileName, [fRange16, fRange17]);
    end;
    18:
    begin
      FileName := '（9）（0-0）. [ 查询（大号码：%s个）...（小号码：%s个）相同个数 ]：【 各区域（合并）】.txt';
      FileName := Format(FileName, [fRange18, fRange19]);
    end;
    20:
    begin
      FileName := '（11）（0-0）. [ 查询（相连号码），（各）行之间，（%s个）相同个数 ]：【 各区域（合并）】.txt';
      FileName := Format(FileName, [fRange20]);
    end;
    21:
    begin
      FileName := '（12）（0-0）. [ 查询（等差号码），（各）行之间，（%s个）相同个数 ]：【 各区域（合并）】.txt';
      FileName := Format(FileName, [fRange21]);
    end;
    22:
    begin
      FileName := '（13）（0-0）. [ 查询（个位），（各）行之间，（%s个）相同个位个数 ]：【 各区域（合并）】.txt';
      FileName := Format(FileName, [fRange22]);
    end;
    254: FileName := '（0）4. 第（2）-（N）项中【（各行）行号 N = N ...（总合并）】.txt';
    255: FileName := '（0）5. 第（2）-（N）项中【（相同）行号 N = N ...（总合并）】.txt';
  end;

  case Flag of
    1..8: MaxSourceNumber := fMaxSourceNumber2;
    else MaxSourceNumber := fMaxSourceNumber;
  end;

  case Flag of
    10: s := Format('Flag = 10 AND GroupNumber = %d', [GroupNumber]);
    11: s := Format('Flag = 11 AND GroupNumber = %d', [GroupNumber]);
    else s := Format('Flag = %d', [Flag]);
  end;

  RowCount := StrToInt(fRest.OneFieldValue(TSQLCombineData, 'Count(*)', s));
  MaxRowSpacing := StrToInt(fRest.OneFieldValue(TSQLCombineData, 'Max(RowSpacing)', s));
  MaxNumber := StrToInt(fRest.OneFieldValue(TSQLCombineData, 'Max(Number)', s));
  MinNumber := StrToInt(fRest.OneFieldValue(TSQLCombineData, 'Min(Number)', s));

  case FileFlag of
    1: FileName := FileName.Replace('.txt', Format('2 ( 最大行间差：%d ).txt', [MaxRowSpacing]));
    2: FileName := FileName.Replace('.txt', Format('3 ( 最新 - 最大行间差 = %d ).txt', [MaxSourceNumber + 1 - MaxNumber - MaxRowSpacing]));
    else FileName := FileName.Replace('.txt', Format('1 [ 总共 %d = 行 ].txt', [RowCount]));
  end;

  case FileFlag of
    1, 2: s := s + ' ORDER BY RowSpacing DESC, Number DESC';
    else s := s + ' ORDER BY Number DESC';
  end;
  TSQLCombineData.AutoFree(CombineData, fRest, s, []);

  FileName := fExportDirectory + ExtractFileName(FileName);
  fr := TFileWriter.Create(FileName);
  try
    if (Flag in [254, 255]) and (FileFlag = 2) then
    begin
      s := '（ 0 ）（ 0-0 ）. 第（ 2 ）-（ N ）项，【 各区域（ 合并 ）（ 总合并 ）】 ：';
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);

      s := '第（ 2-6 ）项 ，（ 各 ）行之间 ，（ N-N 个相同 ）列数字 ：';
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);

      iCount := 0;
      RowNumber := 0;
      l := fRest.MultiFieldValues(TSQLCodeName, 'Value', 'Flag BETWEEN 10 AND 15 GROUP BY Value', []);
      while l.Step and not Terminated do
      begin
        if not sValue.IsEmpty then sValue := sValue + ' ；';
        sValue := sValue + l.FieldAsRawUTF8(0);
        Inc(iCount);

        if (iCount mod 10 = 0) or (l.StepRow = l.RowCount) then
        begin
          Inc(RowNumber);

          s := '%d = %s ...[（ 共 %d 组 ）相同列数字 ]';
          s := Format(s, [RowNumber, sValue, iCount]);
          fr.WriteLn('');
          fr.WriteLn(s);

          sValue := '';
        end;
      end;
      FreeAndNil(l);

      l := fRest.MultiFieldValues(TSQLCompareData, 'Flag, CodeName',
        'Flag BETWEEN 16 AND 22 GROUP BY Flag, CodeName ORDER BY Flag, CodeName DESC', []);
      while l.Step and not Terminated do
      begin
        if (l.StepRow = 1) or (l.FieldAsInteger(0) <> l.GetAsInteger(l.StepRow - 1, 0)) then
        begin
          case l.FieldAsInteger(0) of
            16: s := '第（ 7 ）项 . [ 查询（ 单数 ），（ 各 ）行之间 ，（ N-N 个 ） 相同个数 ] ：';
            17: s := '第（ 8 ）项 . [ 查询（ 双数 ），（ 各 ）行之间 ，（ N-N 个 ） 相同个数 ] ：';
            18: s := '第（ 9 ）项 . [ 查询（ 大号码 ），（ 各 ）行之间 ，（ N-N 个 ）相同个数 ] ：';
            19: s := '第（ 10 ）项 . [ 查询（ 小号码 ），（ 各 ）行之间 ，（ N-N 个 ）相同个数 ] ：';
            20: s := '第（ 11 ）项 . [ 查询（ 相连号码 ），（ 各 ）行之间 ，（ N-N 个 ）相同个数 ] ：';
            21: s := '第（ 12 ）项 . [ 查询（ 等差号码 ），（ 各 ）行之间 ，（ N-N 个 ）相同个数 ] ：';
            22: s := '第（ 13 ）项 . [ 查询（ 个位 ），（ 各 ）行之间 ，（ N-N 个 ）相同个数 ] ：';
          end;
          fr.WriteLn('');
          fr.WriteLn('');
          fr.WriteLn('');
          fr.WriteLn('');
          fr.WriteLn('');
          fr.WriteLn(s);

          sValue := '';
        end;

        if not sValue.IsEmpty then sValue := sValue + '、';
        sValue := sValue + Format('（ %s个 ）', [l.FieldAsRawUTF8(1)]);

        if (l.StepRow = l.RowCount) or (l.FieldAsInteger(0) <> l.GetAsInteger(l.StepRow + 1, 0)) then
        begin
          s := sValue + '相同个数 ；';
          fr.WriteLn('');
          fr.WriteLn(s);
        end;
      end;
      FreeAndNil(l);
    end;

    if FileFlag = 2 then
    begin
      s := '[ 全部行的首行（ 第 %d 行 ）（ 加上 + ）1 ] = [ 第 %d 行 ]（ 减去 - ）[ 该组首行（ 第 %d 行 ）] =（ 最新行间差 ：%d ）- ( 最大行间差 ：%d ) = ( 行间差 ：%d )';
      s := Format(s, [
        MaxSourceNumber,
        MaxSourceNumber + 1,
        MaxNumber,
        MaxSourceNumber + 1 - MaxNumber,
        MaxRowSpacing,
        MaxSourceNumber + 1 - MaxNumber - MaxRowSpacing
      ]);
    end
    else
    begin
      s := '[ 全部行的首行（ 第 %d 行 ）（ 加上 + ）1 ] = [ 第 %d 行 ]（ 减去 - ）[ 该组首行（ 第 %d 行 ）] =（ 行间差 ：%d ）';
      s := Format(s, [MaxSourceNumber, MaxSourceNumber + 1, MaxNumber, MaxSourceNumber + 1 - MaxNumber]);
    end;
    fr.WriteLn('');
    fr.WriteLn('');
    fr.WriteLn(s);

    while CombineData.FillOne and not Terminated do
    begin
      sMax := '';
      if CombineData.RowSpacing = MaxRowSpacing then sMax := '最大';

      s := '%d = %s( %s行间差 ：%d ) ';
      s := Format(s, [CombineData.Number, CombineData.Value, sMax, CombineData.RowSpacing]);
      if CombineData.FillCurrentRow = 2 then fr.WriteLn('');
      fr.WriteLn(s);
    end;
    s := '[ 该组行的末行（ 第 %d 行 ）（ 减去 - ）1 ] =（ 行间差 ：%d ）';
    s := Format(s, [MinNumber, MinNumber - 1]);
    fr.WriteLn('');
    fr.WriteLn(s);
  finally
    fr.Free;
    if Assigned(l) then l.Free;
  end;
end;

procedure TDataComputer.ExportCombineData;
var
  l: TSQLTableJSON;
begin
  if Terminated then Exit;

  l := fRest.MultiFieldValues(TSQLCombineData, 'Flag, GroupNumber',
    'GROUP BY Flag, GroupNumber ORDER BY Flag, GroupNumber', []);
  try
    while l.Step and not Terminated do
    begin
      ExportCombineData(l.FieldAsInteger(0), l.FieldAsInteger(1), 0);
      ExportCombineData(l.FieldAsInteger(0), l.FieldAsInteger(1), 1);
      ExportCombineData(l.FieldAsInteger(0), l.FieldAsInteger(1), 2);
    end;
  finally
    l.Free;
  end;
end;

procedure TDataComputer.ExportOneRowSpacingData(FileName: string; OnlyOneRowSpacing: Boolean; Flag: Byte);
var
  fr: TFileWriter;
  s, FileName2, sValue, sRowState, sOperator, sRowSpacing: string;
  Compare: TSQLCompare;
  CompareSub: TSQLCompareSub;
  CompareData: TSQLCompareData2;
  CompareDataRowSpacing: TSQLCompareDataRowSpacing;
  Number, Number2: Integer;
begin
  FileName := fExportDirectory + FileName;
  fr := TFileWriter.Create(FileName);
  try
    TSQLCompare.AutoFree(Compare);
    TSQLCompareSub.AutoFree(CompareSub);
    TSQLCompareData2.AutoFree(CompareData);
    {Compare.FillPrepare(fRest, 'ORDER BY Number DESC', []);
    while Compare.FillOne do
    begin
      s := '查询行 ：%d=%s ( 行间差 ：%d ) ：';
      s := Format(s, [Compare.Number, Compare.ToString, Compare.RowSpacing]);
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);

      CompareData.FillPrepare(fRest, 'SourceNumber = ? ORDER BY SourceSubNumber DESC, CompareSubNumber DESC', [Compare.Number]);
      while CompareData.FillOne do
      begin
        CompareSub.FillPrepare(fRest, 'Number = ? AND Value = ?', [CompareData.SourceNumber, CompareData.SourceSubNumber]);
        CompareSub.FillOne;

        sRowState := '间差';
        if CompareData.NextOneRowSpacingNumber > 0 then sRowState := '连和';

        s := '1. [ ( 已对应 ) ( 比较行 ) ：%d = %d + %d （ 相同 ）下行差 ：%d ] 行%s ：';
        s := Format(s, [CompareSub.Value, CompareSub.MaxValueCount, CompareSub.MaxValueCount2, CompareData.RowSpacing, sRowState]);
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn(s);

        Number := CompareData.SourceNumber;
        Number2 := CompareData.PriorSameSpacingNumber;
        sOperator := '-';
        sRowSpacing := CompareData.RowSpacing2.ToString;
        if CompareData.PriorOneRowSpacingNumber > 0 then
        begin
          Number := CompareData.SourceNumber + CompareData.PriorOneRowSpacingNumber;
          Number2 := CompareData.SourceNumber;
          sOperator := '+';
          sRowSpacing := '+' + CompareData.PriorOneRowSpacingNumber.ToString;
        end;

        s := '(1). [ 第 %d 查询行 %s ( 最靠上 ) 第 %d 查询行 ：%s ] ；';
        s := Format(s, [Number, sOperator, Number2, sRowSpacing]);
        fr.WriteLn('');
        fr.WriteLn(s);

        Number := CompareData.SourceNumber;
        Number2 := CompareData.NextSameSpacingNumber;
        sOperator := '-';
        sRowSpacing := CompareData.RowSpacing3.ToString;
        if CompareData.NextOneRowSpacingNumber > 0 then
        begin
          Number := CompareData.NextSameSpacingNumber + CompareData.NextOneRowSpacingNumber;
          sOperator := '+';
          sRowSpacing := '+' + CompareData.NextOneRowSpacingNumber.ToString;
        end;

        s := '(2). [ 第 %d 查询行 %s ( 最靠下 ) 第 %d 查询行 ：%s ] 。';
        s := Format(s, [Number, sOperator, Number2, sRowSpacing]);
        fr.WriteLn('');
        fr.WriteLn(s);
      end;
    end;}

    case Flag of
      1: s := 'ORDER BY Abs(Value), Value DESC, MaxNumber DESC';
      2: s := 'ORDER BY DifferenceValue DESC, MaxNumber DESC';
      else s := 'ORDER BY RowCount DESC, MaxNumber DESC';
    end;
    TSQLCompareDataRowSpacing.AutoFree(CompareDataRowSpacing, fRest, s, []);
    while CompareDataRowSpacing.FillOne do
    begin
      if CompareDataRowSpacing.FillCurrentRow = 2 then
      begin
        case Flag of
          1: FileName2 := FileName.Replace('.txt', Format(' 2 [ 最小 ( 相同 ) 下行差：%d ].txt', [CompareDataRowSpacing.Value]));
          2: FileName2 := FileName.Replace('.txt', Format(' 3 ( 最新 - 最大行间差 = %d ).txt', [CompareDataRowSpacing.DifferenceValue]));
          else FileName2 := FileName.Replace('.txt', Format(' 1 [ 最多行数：共 %d 行 ].txt', [CompareDataRowSpacing.RowCount]));
        end;
      end;

      s := '（ 第 %d 区域 ）行数 ：共 %d 行 ：';
      s := Format(s, [CompareDataRowSpacing.FillCurrentRow - 1, CompareDataRowSpacing.RowCount]);
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);

      s := '[ 全部行的首行（ 第 %d 行 ）（ 加上 + ）1 ] = [ 第 %d 行 ]（ 减去 - ）[ 该组首行（ 第 %d 行 ）] =（ 最新行间差 ：%d ）- ( 最大行间差 ：%d ) = ( 行间差 ：%d )';
      s := Format(s, [
        fMaxCompareNumber,
        fMaxCompareNumber + 1,
        CompareDataRowSpacing.MaxNumber,
        fMaxCompareNumber + 1 - CompareDataRowSpacing.MaxNumber,
        CompareDataRowSpacing.MaxRowSpacing,
        fMaxCompareNumber + 1 - CompareDataRowSpacing.MaxNumber - CompareDataRowSpacing.MaxRowSpacing
      ]);
      fr.WriteLn('');
      fr.WriteLn(s);

      s := 'RowSpacing = ?';
      if OnlyOneRowSpacing then
        s := s + ' AND NextOneRowSpacingNumber > 0';
      s := s + ' ORDER BY SourceNumber DESC, SourceSubNumber DESC, CompareSubNumber DESC';
      CompareData.FillPrepare(fRest, s, [CompareDataRowSpacing.Value]);
      while CompareData.FillOne do
      begin
        Compare.FillPrepare(fRest, 'Number = ?', [CompareData.SourceNumber]);
        Compare.FillOne;
        s := '查询行 ：%d=%s ( 行间差 ：%d ) ：';
        s := Format(s, [Compare.Number, Compare.ToString, Compare.RowSpacing]);
        if CompareData.FillCurrentRow > 2 then fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn(s);

        CompareSub.FillPrepare(fRest, 'Number = ? AND Value = ?', [CompareData.SourceNumber, CompareData.SourceSubNumber]);
        CompareSub.FillOne;

        sRowState := '间差';
        if CompareData.NextOneRowSpacingNumber > 0 then sRowState := '连和';

        s := '1. [ ( 已对应 ) ( 比较行 ) ：%d = %d + %d （ 相同 ）下行差 ：%d ] 行%s ：';
        s := Format(s, [CompareSub.Value, CompareSub.MaxValueCount, CompareSub.MaxValueCount2, CompareData.RowSpacing, sRowState]);
        fr.WriteLn('');
        fr.WriteLn(s);

        Number2 := CompareData.PriorSameSpacingNumber;
        sOperator := '-';
        sRowSpacing := CompareData.RowSpacing2.ToString;
        if CompareData.PriorOneRowSpacingNumber > 0 then
        begin
          Number2 := CompareData.SourceNumber + CompareData.PriorOneRowSpacingNumber;
          sOperator := '+';
          sRowSpacing := '+' + CompareData.PriorOneRowSpacingNumber.ToString;
        end;

        s := '(1). [ 第 %d 查询行 %s ( 最靠上 ) 第 %d 查询行 ：%s ] ；';
        s := Format(s, [CompareData.SourceNumber, sOperator, Number2, sRowSpacing]);
        fr.WriteLn('');
        fr.WriteLn(s);

        Number2 := CompareData.NextSameSpacingNumber;
        sRowSpacing := CompareData.RowSpacing3.ToString;
        if CompareData.NextOneRowSpacingNumber > 0 then
        begin
          sRowSpacing := '+1';
          if CompareData.LastNextOneRowSpacingNumber then
          begin
            Number2 := 1;
            sRowSpacing := (CompareData.SourceNumber - 1).ToString;
          end;
        end;
        s := '(2). [ 第 %d 查询行 - ( 最靠下 ) 第 %d 查询行 ：%s ] ';
        s := Format(s, [CompareData.SourceNumber, Number2, sRowSpacing]);

        if CompareData.NextOneRowSpacingNumber = 0 then s := s + '。'
        else
        begin
          Number := CompareData.NextSameSpacingNumber + CompareData.NextOneRowSpacingNumber;
          Number2 := CompareData.NextSameSpacingNumber;
          sRowSpacing := '+' + CompareData.NextOneRowSpacingNumber.ToString;

          s := s + '、[ 第 %d 查询行 + ( 最靠下 ) 第 %d 查询行 ：%s ] 。';
          s := Format(s, [Number, Number2, sRowSpacing]);
        end;

        fr.WriteLn('');
        fr.WriteLn(s);
      end;

      s := '[ 该组行的末行（ 第 %d 行 ）（ 减去 - ）1 ] =（ 行间差 ：%d ）';
      s := Format(s, [
        CompareDataRowSpacing.MinNumber,
        CompareDataRowSpacing.MinNumber - 1
      ]);
      fr.WriteLn('');
      fr.WriteLn(s);
    end;
  finally
    fr.Free;
  end;
  if TFile.Exists(FileName) then
  begin
    if TFile.Exists(FileName2) then TFile.Delete(FileName2);
    RenameFile(FileName, FileName2);
  end;
end;

procedure TDataComputer.ExportOneRowSpacingData;
var
  FileName: string;
begin
  if fExportOneRowSpacingFile then
  begin
    FileName := '（1）（0-1）. [ 查询 ( 已对应 ) ( 比较行 )：N=N ... ( 相同 ) 下行差：行间差 ( 及 ) 行连和 ].txt';
    ExportOneRowSpacingData(FileName, False, 0);
    ExportOneRowSpacingData(FileName, False, 1);
    ExportOneRowSpacingData(FileName, False, 2);
  end;
  if fExportOnlyOneRowSpacingFile then
  begin
    FileName := '（1）（0-2）. [ 查询 ( 已对应 ) ( 比较行 )：N=N ... ( 相同 ) 下行差：行连和 ].txt';
    ExportOneRowSpacingData(FileName, True, 0);
    ExportOneRowSpacingData(FileName, True, 1);
    ExportOneRowSpacingData(FileName, True, 2);
  end;
end;

constructor TDataComputer.Create;
begin
  inherited Create(True);
  fDataMode := 0;
  fSetDataMode := True;
end;

destructor TDataComputer.Destroy;
begin
  if Assigned(fRest) then fRest.Free;
  inherited Destroy;
end;

procedure TDataComputer.Execute;
var
  s, FileName: string;
  i: Integer;
  fs: TSettings;
begin
  if Now > 43991 then Exit;
  fStopwatch := TStopwatch.StartNew;
  try
    try
      fTotalIntervalValue := 0;
      for i := Low(fIntervalValues) to High(fIntervalValues) do
        fTotalIntervalValue := fTotalIntervalValue + fIntervalValues[i];

      BuildRest;
      LoadSourceFile;
      if FileExists(fSourceFileName) then LoadSourceFile2;
      if fExportOneRowSpacingFile or fExportOnlyOneRowSpacingFile then
      begin
        Compare2;
        BuildCompareDataRowSpacing;
        BuildOneRowSpacingNumber;
      end;

      for fs in fSettings do
      begin
        fActiveSettings := fs;

        case fActiveSettings.Flag of
          12, 13:
          begin
            fCompareSpacing := fVertCompareSpacing;
          end;
          14, 15:
          begin
            fCompareSpacing := fSlantCompareSpacing;
          end;
        end;

        if fActiveSettings.Flag > 8 then BuildSource;
        Compare;
        BuildCodeName;
        BuildRowSpacing;
      end;
      BuildCombineData;
      ExportData;
      ExportCombineData;
      ExportOneRowSpacingData;

      if Terminated then Exit;
      TThread.Queue(nil, procedure
      begin
        ShowMessage('处理完成');
      end);
    except
      on e: Exception do
      begin
        s := e.Message;
        TThread.Queue(nil, procedure
        begin
          raise Exception.Create(s);
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
  fSerializer := TJsonSerializer.Create;

finalization
  if Assigned(fKeyValue) then FreeAndNil(fKeyValue);
  if Assigned(fRestSettings) then FreeAndNil(fRestSettings);
  if Assigned(fSerializer) then FreeAndNil(fSerializer);

end.

