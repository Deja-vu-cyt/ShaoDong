unit uDataComputer;

interface

uses
  System.Classes,
  System.Variants,
  System.SysUtils,
  System.Types,
  System.IOUtils,
  System.Threading,
  mORMot,
  mORMotSQLite3,
  SynSQLite3Static,
  SynCommons,
  uCommon;

type
  TSQLKeyValue = class(TSQLRecord)
  private
    fKey: RawUTF8;
    fValue: Variant;
  published
    property Key: RawUTF8 read fKey write fKey;
    property Value: Variant read fValue write fValue;
  end;

  TSQLData = class(TSQLRecord)
  private
    fField1: Int64;
    fField2: Int64;
    fField3: Int64;
    fField4: Int64;
    fTotalValueCount: Word;
    fValueCount: Word;
    function ReadValues: SynCommons.TWordDynArray;
  public
    procedure AssignValue(Source: TSQLData);
    procedure AddValues(Source: TSQLData);
    procedure AddValue(v: Word);
    procedure ClearValue;
    function ValueExist(v: Word): Boolean;
    function HasValue: Boolean;
    procedure CalcValueCount(FirstRangeValue: Byte);
    property Values: SynCommons.TWordDynArray read ReadValues;
  published
    property Field1: Int64 read fField1 write fField1;
    property Field2: Int64 read fField2 write fField2;
    property Field3: Int64 read fField3 write fField3;
    property Field4: Int64 read fField4 write fField4;
    property TotalValueCount: Word read fTotalValueCount write fTotalValueCount;
    property ValueCount: Word read fValueCount write fValueCount;
  end;

  TSQLRow = class(TSQLData)
  private
    fNumber: Word;
  published
    property Number: Word read fNumber write fNumber;
  end;

  TSQLCompareData = class(TSQLData)
  private type
    TCompareRow = record
      Number: Word;
      CompareType: Byte;
      TotalSameValueCount: Word;
      SameValueCount: Word;
      SameValues: TWordDynArray;
      TotalDifferentValueCount: Word;
      DifferentValueCount: Word;
      DifferentValues: TWordDynArray;
      RowNumber: Word;
    end;
    TCompareRowArray = array of TCompareRow;
  private
    fFirstRow: Word;
    fGroupValue: Int64;
    fGroupValueCount: Byte;
    fCompareRows: TCompareRowArray;
    fRowSpacing: Word;
    fCompareValueCount: Word;
    fTotalSameValueCount: Word;
    fSameValueCount: Word;
    fTotalDifferentValueCount: Word;
    fDifferentValueCount: Word;
  public
    procedure CalcCompareValueCount(FirstRangeValue: Byte);
  published
    property FirstRow: Word read fFirstRow write fFirstRow;
    property GroupValue: Int64 read fGroupValue write fGroupValue;
    property GroupValueCount: Byte read fGroupValueCount write fGroupValueCount;
    property CompareRows: TCompareRowArray read fCompareRows write fCompareRows;
    property RowSpacing: Word read fRowSpacing write fRowSpacing;
    property CompareValueCount: Word read fCompareValueCount write fCompareValueCount;
    property TotalSameValueCount: Word read fTotalSameValueCount write fTotalSameValueCount;
    property SameValueCount: Word read fSameValueCount write fSameValueCount;
    property TotalDifferentValueCount: Word read fTotalDifferentValueCount write fTotalDifferentValueCount;
    property DifferentValueCount: Word read fDifferentValueCount write fDifferentValueCount;
  end;

  TSQLFirstRow = class(TSQLRecord)
  private
    fValue : Word;
    fRowSpacing: Word;
    fRowCount: Word;
    fGroupValueCount: Word;
    fMaxGroupValueCount: Byte;
    fMaxRowCountGroupValueCount: Byte;
    fMaxGroupValueCountRowCount: Word;
  published
    property Value: Word read fValue write fValue stored AS_UNIQUE;
    property RowSpacing: Word read fRowSpacing write fRowSpacing;
    property RowCount: Word read fRowCount write fRowCount;
    property GroupValueCount: Word read fGroupValueCount write fGroupValueCount;
    property MaxGroupValueCount: Byte read fMaxGroupValueCount write fMaxGroupValueCount;
    property MaxRowCountGroupValueCount: Byte read fMaxRowCountGroupValueCount
      write fMaxRowCountGroupValueCount;
    property MaxGroupValueCountRowCount: Word read fMaxGroupValueCountRowCount
      write fMaxGroupValueCountRowCount;
  end;

  TSQLCompareGroup = class(TSQLRecord)
  private
    fValue: Int64;
    fValueCount: Byte;
    fSize: RawUTF8;
    fRowCount: Cardinal;
    fMaxRowSpacing: Word;
    fMaxTotalValueCount: Word;
    fMaxValueCount: Word;
    fLastFirstRow: Word;
    fOneRowSpacingCount: Word;
  published
    property Value: Int64 read fValue write fValue stored AS_UNIQUE;
    property ValueCount: Byte read fValueCount write fValueCount;
    property Size: RawUTF8 index 64 read fSize write fSize;
    property RowCount: Cardinal read fRowCount write fRowCount;
    property MaxRowSpacing: Word read fMaxRowSpacing write fMaxRowSpacing;
    property MaxTotalValueCount: Word read fMaxTotalValueCount write fMaxTotalValueCount;
    property MaxValueCount: Word read fMaxValueCount write fMaxValueCount;
    property LastFirstRow: Word read fLastFirstRow write fLastFirstRow;
    property OneRowSpacingCount: Word read fOneRowSpacingCount write fOneRowSpacingCount;
  end;

  TDataComputer = class
  private type
    TCompareProcess = (cpCompare, cpGrouping, cpUpdateFirstRow, cpUpdateCompareGroup, cpFinish);
    TRecCompareState = record
      FirstRow: Word;
      Process: TCompareProcess;
    end;
  public type
    TCompareMode = (cmNone, cmVert, cmSlant, cmVertSlant);
    TCompareModes = set of TCompareMode;
    TExportFile = (efFile, efFile2, efFile3, efFile4, efFile5, efFile6);
    TExportFiles = set of TExportFile;
    TInitEvent = procedure(var MaxValue: Word; var FirstRangeValue: Word) of object;
    TInitCompareEvent = procedure(
      var VertCompareSpacing: Word; var VertSameValueCount: Byte;
      var VertSameValueCount2: Byte; var SlantCompareSpacing: Word;
      var SlantSameValueCount: Byte; var SlantSameValueCount2: Byte;
      var CompareGroupValueCount: Byte; var ExportGroupValueCount: Byte
    ) of object;
  private
    fInitEvent: TInitEvent;
    fInitCompareEvent: TInitCompareEvent;
    fModel: TSQLModel;
    fDatabase: TSQLRestServerDB;
    fKeyValue: TSQLKeyValue;
    fDatabaseFileName: string;
    fExportDirectory: string;
    fExportDirectory2: string;
    fExportDirectory3: string;
    fExportDirectory4: string;
    fExportDirectory5: string;
    fExportDirectory6: string;
    fMaxValue: Word;
    fFirstRangeValue: Word;
    fRowCount: Integer;
    fCompareState: TRecCompareState;
    fCompareMode: TCompareMode;
    fVertSlantCompareSpacing: Word;
    fVertCompareSpacing: Word;
    fVertSameValueCount: Byte;
    fVertSameValueCount2: Byte;
    fSlantCompareSpacing: Word;
    fSlantSameValueCount: Byte;
    fSlantSameValueCount2: Byte;
    fCompareGroupValueCount: Byte;
    fExportGroupValueCount: Byte;
    fExportGroupRowCount: Word;
    function GetKeyValue(Key: string): Variant;
    procedure SetKeyValue(Key: string; Value: Variant);
    procedure SetCompareMode(AValue: TCompareMode);
    function CompareTypeToString(Value: Int64; Flag: Byte = 0): string;
    function VertCompareTypeToString(Value: Int64): string;
    function DataToString(Values: SynCommons.TWordDynArray): string;
    procedure CalcRowSpacing(CompareData: TSQLCompareData);

    procedure SaveGroupByFirstRow;
    procedure SaveGroupByCompareTypeSortByRowcount;
    procedure SaveGroupByCompareTypeSortByMaxRowSpacing;
    procedure SaveGroupByCompareTypeSortByCompareTypeCount;
    procedure SaveGroupByCompareTypeSortByMaxValueCount;
    procedure SaveGroupByCompareTypeSortByLastFirstRow;
    procedure SaveGroupByCompareTypeSortByOneRowSpacingCount;
    procedure SaveCompareType;

    procedure SaveVertGroupByFirstRow;
    procedure SaveVertGroupByCompareTypeSortByRowcount;
    procedure SaveVertGroupByCompareTypeSortByMaxRowSpacing;
    procedure SaveVertGroupByCompareTypeSortByCompareTypeCount;
    procedure SaveVertGroupByCompareTypeSortByMaxValueCount;
    procedure SaveVertGroupByCompareTypeSortByLastFirstRow;
    procedure SaveVertGroupByCompareTypeSortByLastFirstRow2;
    procedure SaveVertCompareType;

    procedure SaveCompareData(const tf: TextFile; Data: TSQLCompareData; SaveValues: Boolean = True);
    procedure SaveVertSlantGroupByFirstRow;
    procedure SaveVertSlantGroupByCompareTypeSortByRowcount;
    procedure SaveVertSlantGroupByCompareTypeSortByMaxRowSpacing;
    procedure SaveVertSlantGroupByCompareTypeSortByCompareTypeCount;
    procedure SaveVertSlantGroupByCompareTypeSortByMaxValueCount;
    procedure SaveVertSlantGroupByCompareTypeSortByLastFirstRow;
    procedure SaveVertSlantGroupByCompareTypeSortByLastFirstRow2;
    procedure SaveVertSlantCompareType;
  public
    constructor Create;
    destructor Destroy;
    procedure LoadRow(FileName: string);
    procedure Compare;
    procedure ExportCompareRow;
    procedure ExportCompareData(ExportFiles: TExportFiles; GroupRowCount: Word = 0);
  published
    property InitEvent: TInitEvent read fInitEvent write fInitEvent;
    property InitCompareEvent: TInitCompareEvent read fInitCompareEvent write fInitCompareEvent;
    property MaxValue: Word read fMaxValue;
    property FirstRangeValue: Word read fFirstRangeValue;
    property CompareMode: TCompareMode read fCompareMode write SetCompareMode;
    property VertCompareSpacing: Word read fVertCompareSpacing;
    property VertSameValueCount: Byte read fVertSameValueCount;
    property VertSameValueCount2: Byte read fVertSameValueCount2;
    property SlantCompareSpacing: Word read fSlantCompareSpacing;
    property SlantSameValueCount: Byte read fSlantSameValueCount;
    property SlantSameValueCount2: Byte read fSlantSameValueCount2;
    property CompareGroupValueCount: Byte read fCompareGroupValueCount;
    property ExportGroupValueCount: Byte read fExportGroupValueCount;
  end;

implementation

function TSQLData.ReadValues: SynCommons.TWordDynArray;
var
  i, i2, i3: Integer;
  v: Int64;
  t: System.Types.TWordDynArray;
begin
  SetLength(Result, 0);
  for i := 1 to 4 do
  begin
    v := fField1;
    case i of
      2: v := fField2;
      3: v := fField3;
      4: v := fField4;
    end;
    if v = 0 then Continue;

    t := v.ToArray;
    if Length(t) > 0 then
    begin
      i2 := Length(Result);
      SetLength(Result, i2 + Length(t));
      for i3 := Low(t) to High(t) do
        Result[i2 + i3] := t[i3] + (i - 1) * 64;
    end;
  end;
end;

procedure TSQLData.AssignValue(Source: TSQLData);
begin
  if not Assigned(Source) then Exit;
  fField1 := Source.Field1;
  fField2 := Source.Field2;
  fField3 := Source.Field3;
  fField4 := Source.Field4;
end;

procedure TSQLData.AddValues(Source: TSQLData);
begin
  if not Assigned(Source) then Exit;
  fField1 := fField1 or Source.Field1;
  fField2 := fField2 or Source.Field2;
  fField3 := fField3 or Source.Field3;
  fField4 := fField4 or Source.Field4;
end;

procedure TSQLData.AddValue(v: Word);
var
  i: Byte;
begin
  i := (v - 1) div 64;
  v := (v - 1) mod 64 + 1;
  case i of
    0: fField1.AddValue(v);
    1: fField2.AddValue(v);
    2: fField3.AddValue(v);
    3: fField4.AddValue(v);
  end;
end;

procedure TSQLData.ClearValue;
begin
  fField1 := 0;
  fField2 := 0;
  fField3 := 0;
  fField4 := 0;
end;

function TSQLData.ValueExist(v: Word): Boolean;
var
  i: Byte;
begin
  Result := False;
  i := (v - 1) div 64;
  v := (v - 1) mod 64 + 1;
  case i of
    0: Result := fField1.ValueExist(v);
    1: Result := fField2.ValueExist(v);
    2: Result := fField3.ValueExist(v);
    3: Result := fField4.ValueExist(v);
  end;
end;

function TSQLData.HasValue: Boolean;
begin
  Result := not ((fField1 = 0) and (fField2 = 0) and (fField3 = 0) and (fField4 = 0));
end;

procedure TSQLData.CalcValueCount(FirstRangeValue: Byte);
var
  v: Word;
begin
  fValueCount := 0;
  fTotalValueCount := 0;
  for v in Values do
  begin
    if v <= FirstRangeValue then fValueCount := fValueCount + 1;
    fTotalValueCount := fTotalValueCount + 1;
  end;
end;

procedure TSQLCompareData.CalcCompareValueCount(FirstRangeValue: Byte);
var
  i: Integer;
  v: Word;
  SameValue, DifferentValue: TSQLData;
begin
  TSQLData.AutoFree(SameValue);
  TSQLData.AutoFree(DifferentValue);
  SameValue.ClearValue;
  DifferentValue.ClearValue;
  for i := Low(fCompareRows) to High(fCompareRows) do
  begin
    with fCompareRows[i] do
    begin
      for v in SameValues do SameValue.AddValue(v);
      for v in DifferentValues do DifferentValue.AddValue(v);
    end;
  end;
  SameValue.CalcValueCount(FirstRangeValue);
  DifferentValue.CalcValueCount(FirstRangeValue);
  fCompareValueCount := SameValue.TotalValueCount + DifferentValue.TotalValueCount;
  fTotalSameValueCount := SameValue.TotalValueCount;
  fSameValueCount := SameValue.ValueCount;
  fTotalDifferentValueCount := DifferentValue.TotalValueCount;
  fDifferentValueCount := DifferentValue.ValueCount;
end;

function TDataComputer.GetKeyValue(Key: string): Variant;
begin
  fKeyValue.FillPrepare(fDatabase, 'Key = ?', [Key]);
  if fKeyValue.FillOne then Result := fKeyValue.Value;
end;

procedure TDataComputer.SetKeyValue(Key: string; Value: Variant);
begin
  fKeyValue.FillPrepare(fDatabase, 'Key = ?', [Key]);
  if fKeyValue.FillOne then
  begin
    fKeyValue.Value := Value;
    fDatabase.Update(fKeyValue);
  end
  else
  begin
    fKeyValue.Key := Key;
    fKeyValue.Value := Value;
    fDatabase.Add(fKeyValue, True);
  end;
end;

procedure TDataComputer.SetCompareMode(AValue: TCompareMode);
begin
  if (fCompareMode = cmNone) and (AValue > cmNone) then
  begin
    SetKeyValue('CompareMode', AValue);
    fCompareMode := AValue;
  end;
end;

function TDataComputer.CompareTypeToString(Value: Int64; Flag: Byte): string;
var
  v: Word;
begin
  Result := '';
  for v in Value.ToArray do
  begin
    if not Result.IsEmpty then Result := Result + '.';
    if Flag = 1 then
    begin
      Result := Result + ((v + 1) div 2).ToString;
      if v mod 2 = 0 then Result := Result + 'Z'
      else Result := Result + 'Y';
    end
    else Result := Result + v.ToString;
  end;
end;

function TDataComputer.VertCompareTypeToString(Value: Int64): string;
var
  v: Word;
begin
  Result := '';
  for v in Value.ToArray do
  begin
    if not Result.IsEmpty then Result := Result + '、';
    Result := Result + v.ToString;
  end;
end;

function TDataComputer.DataToString(Values: SynCommons.TWordDynArray): string;
var
  v: Word;
  s: string;
begin
  Result := '';
  for v in Values do
  begin
    if (fFirstRangeValue > 0) and (v > fFirstRangeValue) then s := (v - fFirstRangeValue).ToString
    else s := v.ToString;
    if s.Length < 2 then s := '0' + s;
    if (fFirstRangeValue > 0) and (v > fFirstRangeValue) and (Result.IndexOf('-') = -1) then Result := Result + ' - ' + s
    else if Result.IsEmpty then Result := s
    else Result := Result + '、' + s;
  end;
end;

procedure TDataComputer.CalcRowSpacing(CompareData: TSQLCompareData);
var
  CompareData2: TSQLCompareData;
begin
  TSQLCompareData.AutoFree(CompareData2, fDatabase,
    'GroupValue = ? AND FirstRow < ? ORDER BY FirstRow DESC LIMIT 1',
    [CompareData.GroupValue, CompareData.FirstRow]);
  if CompareData2.FillOne then
    CompareData.RowSpacing := CompareData.FirstRow - CompareData2.FirstRow
  else
  begin
    CompareData.RowSpacing := 0;
    case fCompareMode of
      cmVert: CompareData.RowSpacing := CompareData.FirstRow - fVertCompareSpacing - 1;
      cmSlant: CompareData.RowSpacing := CompareData.FirstRow - fSlantCompareSpacing - 1;
    end;
  end;
end;

constructor TDataComputer.Create;
var
  v: Variant;
begin
  inherited Create;
  fDatabaseFileName := TPath.GetDirectoryName(ParamStr(0)) + '\Data';
  fExportDirectory := TPath.GetDirectoryName(ParamStr(0)) + '\导出结果(斜连)\';
  fExportDirectory2 := fExportDirectory + '（1）.【排列】-（6）.【保存】\';
  fExportDirectory3 := TPath.GetDirectoryName(ParamStr(0)) + '\导出结果(直连)\';
  fExportDirectory4 := fExportDirectory3 + '（1）.【排列】-（6）.【简化】\';
  fExportDirectory5 := TPath.GetDirectoryName(ParamStr(0)) + '\导出结果(直、斜连)\';
  fExportDirectory6 := fExportDirectory5 + '（1）.【排列】-（6）.【简化】\';
  fModel := TSQLModel.Create([TSQLKeyValue, TSQLRow, TSQLCompareData, TSQLFirstRow,
    TSQLCompareGroup]);
  fDatabase := TSQLRestServerDB.Create(fModel, fDatabaseFileName);
  fDatabase.CreateMissingTables;
  fDatabase.CreateSQLIndex(TSQLCompareData, 'FirstRow', False);
  fDatabase.CreateSQLIndex(TSQLCompareData, 'GroupValue', False);
  fDatabase.CreateSQLIndex(TSQLCompareData, 'GroupValueCount', False);
  fDatabase.CreateSQLIndex(TSQLCompareGroup, 'Size', False);
  fDatabase.CreateSQLIndex(TSQLCompareGroup, 'ValueCount', False);
  fDatabase.CreateSQLIndex(TSQLCompareGroup, 'MaxRowSpacing', False);
  fDatabase.CreateSQLIndex(TSQLCompareGroup, ['MaxTotalValueCount', 'MaxValueCount'], False);
  fDatabase.CreateSQLIndex(TSQLCompareGroup, 'LastFirstRow', False);
  fDatabase.CreateSQLIndex(TSQLCompareGroup, 'OneRowSpacingCount', False);

  fKeyValue := TSQLKeyValue.Create;
  v := GetKeyValue('MaxValue');
  if v = Unassigned then fMaxValue := 0
  else fMaxValue := v;
  v := GetKeyValue('FirstRangeValue');
  if v = Unassigned then fFirstRangeValue := 0
  else fFirstRangeValue := v;
  v := GetKeyValue('CompareMode');
  if v = Unassigned then fCompareMode := cmNone
  else fCompareMode := TCompareMode(v);
  v := GetKeyValue('VertCompareSpacing');
  if v = Unassigned then fVertCompareSpacing := 0
  else fVertCompareSpacing := v;
  v := GetKeyValue('VertSameValueCount');
  if v = Unassigned then fVertSameValueCount := 0
  else fVertSameValueCount := v;
  v := GetKeyValue('VertSameValueCount2');
  if v = Unassigned then fVertSameValueCount2 := 0
  else fVertSameValueCount2 := v;
  v := GetKeyValue('VertCompareTypeCount');
  v := GetKeyValue('SlantCompareSpacing');
  if v = Unassigned then fSlantCompareSpacing := 0
  else fSlantCompareSpacing := v;
  v := GetKeyValue('SlantSameValueCount');
  if v = Unassigned then fSlantSameValueCount := 0
  else fSlantSameValueCount := v;
  v := GetKeyValue('SlantSameValueCount2');
  if v = Unassigned then fSlantSameValueCount2 := 0
  else fSlantSameValueCount2 := v;
  v := GetKeyValue('CompareGroupValueCount');
  if v = Unassigned then fCompareGroupValueCount := 0
  else fCompareGroupValueCount := v;
  v := GetKeyValue('ExportGroupValueCount');
  if v = Unassigned then fExportGroupValueCount := 0
  else fExportGroupValueCount := v;
  fVertSlantCompareSpacing := fVertCompareSpacing;
  if fSlantCompareSpacing > fVertCompareSpacing then
    fVertSlantCompareSpacing := fSlantCompareSpacing;
end;

destructor TDataComputer.Destroy;
begin
  fKeyValue.Free;
  fDatabase.Free;
  fModel.Free;
  inherited Destroy;
end;

procedure TDataComputer.LoadRow(FileName: string);
var
  i, RowIndex, Digit: Integer;
  s, sDigit: string;
  c: Char;
  HasMinus: Boolean;
  Row: TSQLRow;
begin
  if fMaxValue = 0 then
    if Assigned(fInitEvent) then
    begin
      fInitEvent(fMaxValue, fFirstRangeValue);
      if fMaxValue < 1 then
        raise Exception.Create('总列数无效');
      if (fFirstRangeValue < 0) or (fFirstRangeValue > fMaxValue) then
        raise Exception.Create('第一区域列数无效');
      if fFirstRangeValue = fMaxValue then fFirstRangeValue := 0;

      SetKeyValue('MaxValue', fMaxValue);
      SetKeyValue('FirstRangeValue', fFirstRangeValue);
    end
    else raise Exception.Create('导入行初始化失败');

  if not TFile.Exists(FileName) then Exit;
  fRowCount := fDatabase.TableRowCount(TSQLRow);
  TSQLRow.AutoFree(Row);
  with TStringList.Create do
  begin
    try
      LoadFromFile(FileName);
      fDatabase.TransactionBegin(TSQLRow);
      try
        for i := Count - 1 downto 0 do
        begin
          if not TryStrToInt(Names[i].Trim, Digit) then Continue;
          Row.Number := Digit;
          if Row.Number <= fRowCount then Continue;
          if Row.Number > fRowCount + 1 then raise Exception.Create('行号无效');
          Row.ClearValue;
          s := ValueFromIndex[i];
          HasMinus := s.Contains('-');
          repeat
            sDigit := '';
            for c in s do
            begin
              if c in ['0'..'9'] then sDigit := sDigit + c
              else Break;
            end;
            if sDigit.IsEmpty then s := s.Substring(1)
            else s := s.Substring(sDigit.Length);

            if TryStrToInt(sDigit, Digit) then
            begin
              if HasMinus and (s.IndexOf('-') = -1) then Digit := Digit + fFirstRangeValue;
              if Digit > fMaxValue then Continue;

              case (Digit - 1) div 64 of
                0: Row.Field1.AddValue((Digit - 1) mod 64 + 1);
                1: Row.Field2.AddValue((Digit - 1) mod 64 + 1);
                2: Row.Field3.AddValue((Digit - 1) mod 64 + 1);
                3: Row.Field4.AddValue((Digit - 1) mod 64 + 1);
              end;
            end;
          until s.IsEmpty;

          Row.CalcValueCount(fFirstRangeValue);
          fDatabase.Add(Row, True);
          fRowCount := Row.Number;
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
      Free;
    end;
  end;
end;

procedure TDataComputer.Compare;

  function VertCompareRow(r, r2: TSQLRow; var CompareRows: TSQLCompareData.TCompareRowArray): Boolean;
  var
    v: Word;
  begin
    with CompareRows[0] do
    begin
      TotalSameValueCount := 0;
      SameValueCount := 0;
      SetLength(SameValues, 0);
      TotalDifferentValueCount := 0;
      DifferentValueCount := 0;
      SetLength(DifferentValues, 0);
      for v in r2.Values do
        if r.ValueExist(v) then
        begin
          if v <= fFirstRangeValue then SameValueCount := SameValueCount + 1;
          TotalSameValueCount := TotalSameValueCount + 1;
          SetLength(SameValues, Length(SameValues) + 1);
          SameValues[High(SameValues)] := v;
        end
        else
        begin
          if v <= fFirstRangeValue then DifferentValueCount := DifferentValueCount + 1;
          TotalDifferentValueCount := TotalDifferentValueCount + 1;
          SetLength(DifferentValues, Length(DifferentValues) + 1);
          DifferentValues[High(DifferentValues)] := v;
        end;
      Result := (TotalSameValueCount >= fVertSameValueCount)
        and (TotalSameValueCount <= fVertSameValueCount2);
    end;
  end;

  function SlantCompareRow(r, r2: TSQLRow; CompareData: TSQLCompareData;
    Offset: Integer; var CompareRows: TSQLCompareData.TCompareRowArray): Boolean;
  var
    v, v2: Word;
  begin
    with CompareRows[0] do
    begin
      TotalSameValueCount := 0;
      SameValueCount := 0;
      SetLength(SameValues, 0);
      TotalDifferentValueCount := 0;
      DifferentValueCount := 0;
      SetLength(DifferentValues, 0);
      CompareData.ClearValue;
      for v2 in r2.Values do
      begin
        v := v2 + Offset;
        if (v < 1) or (v > fMaxValue) then Continue;
        CompareData.AddValue(v);
        if r.ValueExist(v) then
        begin
          if v <= fFirstRangeValue then SameValueCount := SameValueCount + 1;
          TotalSameValueCount := TotalSameValueCount + 1;
          SetLength(SameValues, Length(SameValues) + 1);
          SameValues[High(SameValues)] := v;
        end
        else
        begin
          if v <= fFirstRangeValue then DifferentValueCount := DifferentValueCount + 1;
          TotalDifferentValueCount := TotalDifferentValueCount + 1;
          SetLength(DifferentValues, Length(DifferentValues) + 1);
          DifferentValues[High(DifferentValues)] := v;
        end;
      end;
      Result := (TotalSameValueCount >= fSlantSameValueCount)
        and (TotalSameValueCount <= fSlantSameValueCount2);
    end;
  end;

var
  Row, Row2: TSQLRow;
  CompareData, CompareData2: TSQLCompareData;
  FirstRow, FirstRow2: TSQLFirstRow;
  Group: TSQLCompareGroup;
  DataList: TSQLTableJSON;
  i, i2: Integer;
  RowCount, Number, GroupValue: Word;
  GroupValueCount: Byte;
  v: Variant;
begin
  if Assigned(fInitCompareEvent) then
    fInitCompareEvent(fVertCompareSpacing, fVertSameValueCount,
      fVertSameValueCount2, fSlantCompareSpacing, fSlantSameValueCount,
      fSlantSameValueCount2, fCompareGroupValueCount, fExportGroupValueCount
    );
  case fCompareMode of
    cmVert, cmVertSlant:
    begin
      if fVertCompareSpacing < 1 then
        raise Exception.Create('直连比较次数无效');
      if fVertSameValueCount < 0 then
        raise Exception.Create('直连相同列数无效');
      if fVertSameValueCount2 < 0 then
        raise Exception.Create('直连相同列数2无效');
    end;
  end;
  case fCompareMode of
    cmSlant, cmVertSlant:
    begin
      if fSlantCompareSpacing < 1 then
        raise Exception.Create('斜连比较次数无效');
      if fSlantSameValueCount < 0 then
        raise Exception.Create('斜连相同列数无效');
      if fSlantSameValueCount2 < 0 then
        raise Exception.Create('斜连相同列数2无效');
    end;
  end;
  if fCompareGroupValueCount < 1 then
    raise Exception.Create('比较组合次数无效');
  if fExportGroupValueCount < 1 then
    raise Exception.Create('导出组合次数无效');

  SetKeyValue('CompareMode', Ord(fCompareMode));
  SetKeyValue('VertCompareSpacing', fVertCompareSpacing);
  SetKeyValue('VertSameValueCount', fVertSameValueCount);
  SetKeyValue('VertSameValueCount2', fVertSameValueCount2);
  SetKeyValue('SlantCompareSpacing', fSlantCompareSpacing);
  SetKeyValue('SlantSameValueCount', fSlantSameValueCount);
  SetKeyValue('SlantSameValueCount2', fSlantSameValueCount2);
  SetKeyValue('CompareGroupValueCount', fCompareGroupValueCount);
  SetKeyValue('ExportGroupValueCount', fExportGroupValueCount);

  fVertSlantCompareSpacing := fVertCompareSpacing;
  if fSlantCompareSpacing > fVertCompareSpacing then
    fVertSlantCompareSpacing := fSlantCompareSpacing;

  fCompareState.Process := cpCompare;
  fCompareState.FirstRow := fVertSlantCompareSpacing + 1;
  v := GetKeyValue('CompareStateProcess');
  if v <> Unassigned then fCompareState.Process := TCompareProcess(v);
  v := GetKeyValue('CompareStateFirstRow');
  if v <> Unassigned then fCompareState.FirstRow := v;
  fRowCount := fDatabase.TableRowCount(TSQLRow);
  if Now > 43404 then Exit;

  TSQLRow.AutoFree(Row, fDatabase, 'Number >= ?', [fCompareState.FirstRow - fVertSlantCompareSpacing]);
  TSQLRow.AutoFree(Row2);
  TSQLCompareData.AutoFree(CompareData);
  TSQLCompareData.AutoFree(CompareData2);
  TSQLFirstRow.AutoFree(FirstRow);
  TSQLFirstRow.AutoFree(FirstRow2);
  TSQLCompareGroup.AutoFree(Group);

  for i := fVertSlantCompareSpacing + 1 to Row.FillTable.RowCount do
  begin
    Row.FillRow(i);
    repeat
      case fCompareState.Process of
        cpCompare:
        begin
          fDatabase.TransactionBegin(TSQLCompareData);
          try
            CompareData.FirstRow := Row.Number;
            CompareData.GroupValueCount := 1;
            SetLength(CompareData.fCompareRows, 1);
            for i2 := i - 1 downto i - fVertSlantCompareSpacing do
            begin
              Number := i - i2;
              CompareData.CompareRows[0].RowNumber := i2;
              //直连
              case fCompareMode of
                cmVert, cmVertSlant:
                begin
                  if i - i2 <= fVertCompareSpacing then
                  begin
                    Row.FillRow(i2, Row2);
                    if VertCompareRow(Row, Row2, CompareData.fCompareRows)
                    then
                    begin
                      if fCompareMode = cmVert then GroupValue := Number
                      else GroupValue := Number * 3 - 2;
                      CompareData.GroupValue := 0;
                      CompareData.GroupValue.AddValue(GroupValue);
                      with CompareData.CompareRows[0] do
                      begin
                        CompareType := 1;
                        Number := GroupValue;
                      end;
                      CompareData.AssignValue(Row2);
                      CompareData.CalcValueCount(fFirstRangeValue);
                      CompareData.CalcCompareValueCount(fFirstRangeValue);
                      CalcRowSpacing(CompareData);
                      fDatabase.Add(CompareData, True);
                    end;
                  end;
                end;
              end;
              //斜连
              case fCompareMode of
                cmSlant, cmVertSlant:
                begin
                  if i - i2 <= fSlantCompareSpacing then
                  begin
                    CompareData.CompareRows[0].RowNumber := i2;
                    Row.FillRow(i2, Row2);
                    //右斜连
                    if SlantCompareRow(Row, Row2, CompareData, Number, CompareData.fCompareRows)
                    then
                    begin
                      if fCompareMode = cmSlant then GroupValue := Number * 2 - 1
                      else GroupValue := Number * 3 - 1;
                      CompareData.GroupValue := 0;
                      CompareData.GroupValue.AddValue(GroupValue);
                      with CompareData.CompareRows[0] do
                      begin
                        CompareType := 2;
                        Number := GroupValue;
                      end;
                      CompareData.CalcValueCount(fFirstRangeValue);
                      CompareData.CalcCompareValueCount(fFirstRangeValue);
                      CalcRowSpacing(CompareData);
                      fDatabase.Add(CompareData, True);
                    end;
                    //左斜连
                    if SlantCompareRow(Row, Row2, CompareData, -Number, CompareData.fCompareRows)
                    then
                    begin
                      if fCompareMode = cmSlant then GroupValue := Number * 2
                      else GroupValue := Number * 3;
                      CompareData.GroupValue := 0;
                      CompareData.GroupValue.AddValue(GroupValue);
                      with CompareData.CompareRows[0] do
                      begin
                        CompareType := 3;
                        Number := GroupValue;
                      end;
                      CompareData.CalcValueCount(fFirstRangeValue);
                      CompareData.CalcCompareValueCount(fFirstRangeValue);
                      CalcRowSpacing(CompareData);
                      fDatabase.Add(CompareData, True);
                    end;
                  end;
                end;
              end;
            end;

            fDatabase.Commit(1, True);
          except
            on e: Exception do
            begin
              fDatabase.RollBack;
              raise Exception.Create(e.Message);
            end;
          end;
        end;
        cpGrouping:
        begin
          fDatabase.TransactionBegin(TSQLCompareData);
          try
            CompareData.FillPrepare(fDatabase, 'FirstRow = ?', [Row.Number]);
            if fCompareMode = cmSlant then
              fCompareGroupValueCount := CompareData.FillTable.RowCount;
            for i2 := fExportGroupValueCount to fCompareGroupValueCount do
            begin
              if i2 = 1 then Continue;

              Foreach(CompareData.FillTable.RowCount, i2, procedure(RowNoArray: System.Types.TCardinalDynArray)
              var
                i: Integer;
              begin
                CompareData.GroupValue := 0;
                CompareData.ClearValue;
                SetLength(CompareData.fCompareRows, Length(RowNoArray));
                for i := Low(RowNoArray) to High(RowNoArray) do
                begin
                  CompareData.FillRow(RowNoArray[i], CompareData2);
                  CompareData.GroupValue := CompareData.GroupValue or CompareData2.GroupValue;
                  CompareData.AddValues(CompareData2);

                  CompareData.CompareRows[i] := CompareData2.CompareRows[0];
                end;
                CompareData.GroupValueCount := CompareData.GroupValue.ValueCount;
                CompareData.CalcValueCount(fFirstRangeValue);
                CompareData.CalcCompareValueCount(fFirstRangeValue);
                CalcRowSpacing(CompareData);
                fDatabase.Add(CompareData, True);
              end);
            end;
            fDatabase.Delete(TSQLCompareData, 'GroupValueCount < ?', [fExportGroupValueCount]);
            fDatabase.Commit(1, True);
          except
            on e: Exception do
            begin
              fDatabase.RollBack;
              raise Exception.Create(e.Message);
            end;
          end;
        end;
        cpUpdateFirstRow:
        begin
          CompareData.FillPrepare(fDatabase, 'FirstRow = ? LIMIT 1', [Row.Number]);
          if CompareData.FillTable.RowCount > 0 then
          begin
            FirstRow.FillPrepare(fDatabase, 'Value = ?', [Row.Number]);
            if not FirstRow.FillOne then
            begin
              FirstRow.Value := Row.Number;
              FirstRow.GroupValueCount := 0;
              FirstRow.MaxGroupValueCount := 0;
              FirstRow.MaxRowCountGroupValueCount := 0;
              FirstRow.MaxGroupValueCountRowCount := 0;

              FirstRow2.FillPrepare(fDatabase, 'Value < ? ORDER BY Value DESC LIMIT 1', [FirstRow.Value]);
              if FirstRow2.FillOne then
                FirstRow.RowSpacing := FirstRow.Value - FirstRow2.Value
              else
                FirstRow.RowSpacing := FirstRow.Value - fVertSlantCompareSpacing - 1;

              case fCompareMode of
                cmVert, cmVertSlant:
                begin
                  DataList := fDatabase.MultiFieldValues(TSQLCompareData,
                    'GroupValueCount, Count(ID) RowCount',
                    'FirstRow = ? GROUP BY GroupValueCount ORDER BY GroupValueCount',
                    [Row.Number]
                  );
                  try
                    while DataList.Step do
                    begin
                      GroupValueCount := DataList.FieldAsInteger('GroupValueCount');
                      RowCount := DataList.FieldAsInteger('RowCount');

                      FirstRow.GroupValueCount := FirstRow.GroupValueCount + RowCount;
                      if GroupValueCount > FirstRow.MaxGroupValueCount then
                        FirstRow.MaxGroupValueCount := GroupValueCount;
                      if RowCount > FirstRow.MaxGroupValueCountRowCount then
                      begin
                        FirstRow.MaxGroupValueCountRowCount := RowCount;
                        FirstRow.MaxRowCountGroupValueCount := GroupValueCount;
                      end;
                    end;
                  finally
                    DataList.Free;
                  end;
                end;
              end;
              case fCompareMode of
                cmSlant, cmVertSlant:
                begin
                  FirstRow.RowCount := StrToInt(fDatabase.OneFieldValue(TSQLCompareData, 'Count(*)', 'FirstRow = ?', [FirstRow.Value]));
                end;
              end;

              fDatabase.Add(FirstRow, True);
            end;
          end;
        end;
        cpUpdateCompareGroup:
        begin
          CompareData.FillPrepare(fDatabase, 'FirstRow = ?', [Row.Number]);
          fDatabase.TransactionBegin(TSQLCompareGroup);
          try
            while CompareData.FillOne do
            begin
              Group.FillPrepare(fDatabase, 'Value = ?', [CompareData.GroupValue]);
              if Group.FillOne then
              begin
                Group.RowCount := Group.RowCount + 1;
                Group.LastFirstRow := Row.Number;
                CompareData2.FillPrepare(fDatabase, 'GroupValue = ? AND FirstRow < ? ORDER BY FirstRow DESC LIMIT 1', [Group.Value, Row.Number]);
                if CompareData2.FillOne then
                begin
                  if CompareData.RowSpacing > Group.MaxRowSpacing then
                    Group.MaxRowSpacing := CompareData.RowSpacing;

                  if (CompareData.TotalValueCount > Group.MaxTotalValueCount)
                    or ((CompareData.TotalValueCount = Group.MaxTotalValueCount)
                    and (CompareData.ValueCount > Group.MaxValueCount))
                  then
                  begin
                    Group.MaxTotalValueCount := CompareData.TotalValueCount;
                    Group.MaxValueCount := CompareData.ValueCount;
                  end;
                end;
                fDatabase.Update(Group);
              end
              else
              begin
                Group.Value := CompareData.GroupValue;
                Group.ValueCount := Group.Value.ValueCount;
                Group.Size := Group.Value.ToBinaryString;
                Group.RowCount := 1;
                Group.MaxRowSpacing := CompareData.FirstRow - 1;
                Group.MaxTotalValueCount := CompareData.TotalValueCount;
                Group.MaxValueCount := CompareData.ValueCount;
                Group.LastFirstRow := CompareData.FirstRow;
                Group.OneRowSpacingCount := 0;
                fDatabase.Add(Group, True);
              end;
            end;
            fDatabase.Commit(1, True);
          except
            on e: Exception do
            begin
              fDatabase.RollBack;
              raise Exception.Create(e.Message);
            end;
          end;
          //更新连续1临行距个数
          case fCompareMode of
            cmSlant, cmVertSlant:
            begin
              if fRowCount = Row.Number then
              begin
                fDatabase.TransactionBegin(TSQLCompareGroup);
                try
                  Group.FillPrepare(fDatabase, 'OneRowSpacingCount > 0', []);
                  while Group.FillOne do
                  begin
                    Group.OneRowSpacingCount := 0;
                    fDatabase.Update(Group);
                  end;
                  Group.FillPrepare(fDatabase, 'LastFirstRow = ?', [fRowCount]);
                  while Group.FillOne do
                  begin
                    Group.OneRowSpacingCount := 1;

                    CompareData.FillPrepare(fDatabase, 'GroupValue = ? AND RowSpacing = 1 ORDER BY FirstRow DESC', [Group.Value]);
                    while CompareData.FillOne do
                    begin
                      if CompareData.FillCurrentRow < fRowCount - CompareData.FirstRow + 2 then Break;
                      Group.OneRowSpacingCount := Group.OneRowSpacingCount + 1;
                    end;
                    fDatabase.Update(Group);
                  end;
                  fDatabase.Commit(1, True);
                except
                  on e: Exception do
                  begin
                    fDatabase.RollBack;
                    raise Exception.Create(e.Message);
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
      if fCompareState.Process < cpFinish then
        fCompareState.Process := TCompareProcess(Ord(fCompareState.Process) + 1);
      SetKeyValue('CompareStateProcess', Ord(fCompareState.Process));
    until fCompareState.Process = cpFinish;
    fCompareState.FirstRow := Row.Number + 1;
    fCompareState.Process := cpCompare;
    SetKeyValue('CompareStateFirstRow', fCompareState.FirstRow);
    SetKeyValue('CompareStateProcess', fCompareState.Process);
  end;
end;

procedure TDataComputer.ExportCompareRow;
var
  Row: TSQLRow;
  tf: TextFile;
  s, FileName: string;
begin
  FileName := TPath.GetDirectoryName(ParamStr(0)) + '\行.txt';
  if TFile.Exists(FileName) then TFile.Delete(FileName);
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    TSQLRow.AutoFree(Row, fDatabase, 'ORDER BY Number', []);
    while Row.FillOne do
    begin
      s := Format('%d=%s', [Row.Number, DataToString(Row.Values)]);
      WriteLn(tf, s);
    end;
  finally
    CloseFile(tf);
  end;
end;

procedure TDataComputer.SaveGroupByFirstRow;
var
  RowSpacingList: TStringList;

  procedure AddRowSpacing(RowSpacing: Word; s: string);
  var
    sRowSpacing: string;
  begin
    sRowSpacing := RowSpacing.ToString;
    while sRowSpacing.Length < 3 do sRowSpacing := '0' + sRowSpacing;
    if RowSpacingList.IndexOfName(sRowSpacing) = -1 then
    begin
      s := s.Substring(0, s.IndexOf('，同行数')) + '）';
      RowSpacingList.Values[sRowSpacing] := s;
    end;
  end;

var
  tf: TextFile;
  s, FileName, FileName2, TxtFileName, sRowSpacing: string;
  i: Integer;
  FirstRow: TSQLFirstRow;
  Data: TSQLCompareData;
begin
  TxtFileName := '（1）.【排列】“%d”个以上【 [ 相同（第“N”行为首行）] 、（不同代号）】的组合.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  if TFile.Exists(FileName) then TFile.Delete(FileName);
  FileName2 := fExportDirectory + TxtFileName;
  if TFile.Exists(FileName2) then TFile.Delete(FileName2);
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    TSQLFirstRow.AutoFree(FirstRow, fDatabase, 'ORDER BY Value DESC', []);
    TSQLCompareData.AutoFree(Data);
    RowSpacingList := TStringList.Create;
    try
      while FirstRow.FillOne do
      begin
        if FirstRow.FillCurrentRow = 2 then
        begin
          s := '1.（第%d行为首行）（邻行距 ↓%d，同行数：%d）';
          s := Format(s, [fRowCount + 1, fRowCount + 1 - FirstRow.Value, FirstRow.RowCount]);

          AddRowSpacing(fRowCount + 1 - FirstRow.Value, s);
        end;
        s := '%d.（第%d行为首行）（邻行距 ↓%d，同行数：%d）';
        s := Format(s, [FirstRow.FillCurrentRow, FirstRow.Value, FirstRow.RowSpacing, FirstRow.RowCount]);
        AddRowSpacing(FirstRow.RowSpacing, s);
      end;

      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '最大-小的邻行距 1-100 行内：');
      RowSpacingList.Sort;
      for i := RowSpacingList.Count - 1 downto RowSpacingList.Count - 100 do
      begin
        if i < 0 then Break;
        WriteLn(tf, '');
        WriteLn(tf, RowSpacingList.ValueFromIndex[i]);
      end;
    finally
      RowSpacingList.Free;
    end;

    FirstRow.FillRewind;
    while FirstRow.FillOne do
    begin
      if FirstRow.FillCurrentRow = 2 then
      begin
        s := '1.（第%d行为首行）；（邻行距 ↓%d）';
        s := Format(s, [fRowCount + 1, fRowCount + 1 - FirstRow.Value]);
        WriteLn(tf, '');
        WriteLn(tf, '');
        WriteLn(tf, '');
        WriteLn(tf, s);
      end;

      s := '%d.（第%d行为首行）（邻行距 ↓%d，同行数：%d）';
      s := Format(s, [FirstRow.FillCurrentRow, FirstRow.Value, FirstRow.RowSpacing, FirstRow.RowCount]);
      if FirstRow.FillCurrentRow > 2 then
      begin
        WriteLn(tf, '');
        WriteLn(tf, '');
      end;
      WriteLn(tf, '');
      WriteLn(tf, s);

      Data.FillPrepare(fDatabase, 'FirstRow = ?', [FirstRow.Value]);
      while Data.FillOne do
      begin
        s := Format('（%d）.[代号：（第%s个）%s ]', [
          Data.FillCurrentRow - 1,
          CompareTypeToString(Data.GroupValue),
          CompareTypeToString(Data.GroupValue, 1)
        ]);
        if fFirstRangeValue = 0 then
          s := s + Format(' = 无【对应列】数： %d列 ；', [Data.TotalValueCount])
        else
          s := s + Format(' = 【 %d-%d 】列 ；', [Data.ValueCount, Data.TotalValueCount - Data.ValueCount]);
        s := s + Format('【列数字】：%s', [DataToString(Data.Values)]);

        WriteLn(tf, '');
        WriteLn(tf, s);
      end;
    end;
    s := '%d.（第%d行为首行）';
    s := Format(s, [
      FirstRow.FillTable.RowCount + 2,
      fSlantCompareSpacing + 1
    ]);
    WriteLn(tf, '');
    WriteLn(tf, s);
  finally
    CloseFile(tf);
  end;
  if TFile.Exists(FileName) then TFile.Copy(FileName, FileName2);
end;

procedure TDataComputer.SaveGroupByCompareTypeSortByRowcount;
var
  tf, tf2, tf3: TextFile;
  s, sCompareType, sCompareType2, FileName, FileName2, FileName3, FileName4, TxtFileName: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
  RowNo: Cardinal;
begin
  TxtFileName := '（2）.【排列】“%d”个以上各个组合（相同代号、不同首行）的（不同首行数：最多 - 最少行）.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  if TFile.Exists(FileName) then TFile.Delete(FileName);
  TxtFileName := '（2）.【排列】“%d”个以上各个组合（相同代号、不同首行）的（不同首行数：最多 - 最少行）（1）.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName4 := fExportDirectory + TxtFileName;
  if TFile.Exists(FileName4) then TFile.Delete(FileName4);
  TxtFileName := '（2）.【排列】“%d”个以上各个组合（相同代号、不同首行）的（不同首行数：最多 - 最少行）（2）.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName2 := fExportDirectory + TxtFileName;
  if TFile.Exists(FileName2) then TFile.Delete(FileName2);
  TxtFileName := '（2）.【排列】“%d”个以上各个组合（相同代号、不同首行）的（不同首行数：最多 - 最少行）（3）.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName3 := fExportDirectory + TxtFileName;
  if TFile.Exists(FileName3) then TFile.Delete(FileName3);
  AssignFile(tf, FileName);
  Rewrite(tf);
  AssignFile(tf2, FileName2);
  Rewrite(tf2);
  AssignFile(tf3, FileName3);
  Rewrite(tf3);
  try
    RowNo := 0;
    TSQLCompareGroup.AutoFree(Group, fDatabase,
      'RowCount >= ? ORDER BY RowCount DESC, ValueCount, Size DESC',
      [fExportGroupRowCount]);
    TSQLCompareData.AutoFree(Data);
    while Group.FillOne do
    begin
      sCompareType := CompareTypeToString(Group.Value);
      sCompareType2 := CompareTypeToString(Group.Value, 1);

      s := '%d.[代号：（第%s个）%s ] ；（不同首行数：%d行）';
      s := Format(s, [Group.FillCurrentRow - 1, sCompareType, sCompareType2, Group.RowCount]);
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);
      WriteLn(tf2, '');
      WriteLn(tf2, '');
      WriteLn(tf2, s);
      WriteLn(tf2, '');

      Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
      while Data.FillOne do
      begin
        if Data.FillCurrentRow = 2 then
        begin
          RowNo := RowNo + 1;
          s := '1=[（第%s个）%s ] ：（%d）（%d）';
          s := Format(s, [sCompareType, sCompareType2, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
          WriteLn(tf2, s);
          s := '%d=[（第%s个）%s ] ：（%d）（%d）';
          s := Format(s, [RowNo, sCompareType, sCompareType2, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
          WriteLn(tf3, s);
        end;

        s := Format('（%d）（第%d行为首行）', [Data.FillCurrentRow - 1, Data.FirstRow]);
        if fFirstRangeValue = 0 then
          s := s + Format('= 无【对应列】数： %d列 ；', [Data.TotalValueCount])
        else
          s := s + Format('= 无【对应列】数：%d - %d 列 ；', [Data.ValueCount, Data.TotalValueCount - Data.ValueCount]);
        s := s + Format('【列数字】：%s', [DataToString(Data.Values)]);
        WriteLn(tf, '');
        WriteLn(tf, s);

        s := '%d=[（第%s个）%s ] ：（%d）（%d）';
        s := Format(s, [Data.FillCurrentRow, sCompareType, sCompareType2, Data.FirstRow, Data.RowSpacing]);
        WriteLn(tf2, s);
        RowNo := RowNo + 1;
        s := '%d=[（第%s个）%s ] ：（%d）（%d）';
        s := Format(s, [RowNo, sCompareType, sCompareType2, Data.FirstRow, Data.RowSpacing]);
        WriteLn(tf3, s);

        if Data.FillCurrentRow > Data.FillTable.RowCount then
        begin
          RowNo := RowNo + 1;
          s := '%d=[（第%s个）%s ] ：（%d）（0）';
          s := Format(s, [Data.FillCurrentRow + 1, sCompareType, sCompareType2, fSlantCompareSpacing + 1]);
          WriteLn(tf2, s);
          s := '%d=[（第%s个）%s ] ：（%d）（0）';
          s := Format(s, [RowNo, sCompareType, sCompareType2, fSlantCompareSpacing + 1]);
          WriteLn(tf3, s);
        end;
      end;
    end;
  finally
    CloseFile(tf);
    CloseFile(tf2);
    CloseFile(tf3);
  end;
  if TFile.Exists(FileName) then TFile.Copy(FileName, FileName4);
end;

procedure TDataComputer.SaveGroupByCompareTypeSortByMaxRowSpacing;
var
  tf: TextFile;
  s, FileName, TxtFileName: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
begin
  TxtFileName := '（3）.【排列】“%d”个以上各个组合（相同代号、不同首行）的（邻行距：最大↓“N”- 最小↓“N”）.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  if TFile.Exists(FileName) then TFile.Delete(FileName);
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    TSQLCompareGroup.AutoFree(Group, fDatabase, 'ORDER BY MaxRowSpacing DESC, ValueCount, Size DESC', []);
    TSQLCompareData.AutoFree(Data);
    while Group.FillOne do
    begin
      s := '%d.[代号：（第%s个）%s ] ；（最大邻行距 ↓%d）';
      s := Format(s, [
        Group.FillCurrentRow - 1,
        CompareTypeToString(Group.Value),
        CompareTypeToString(Group.Value, 1),
        Group.MaxRowSpacing
      ]);
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);

      Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
      while Data.FillOne do
      begin
        if Data.FillCurrentRow = 2 then
        begin
          s := '（1）（第%d行为首行）；（邻行距 ↓%d）';
          s := Format(s, [fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
          WriteLn(tf, '');
          WriteLn(tf, s);
        end;

        s := Format('（%d）（第%d行为首行）（邻行距 ↓%d）', [Data.FillCurrentRow, Data.FirstRow, Data.RowSpacing]);
        if fFirstRangeValue = 0 then
          s := s + Format('= 无【对应列】数： %d列 ；', [Data.TotalValueCount])
        else
          s := s + Format('= 无【对应列】数：%d - %d 列 ；', [Data.ValueCount, Data.TotalValueCount - Data.ValueCount]);
        s := s + Format('【列数字】：%s', [DataToString(Data.Values)]);
        WriteLn(tf, '');
        WriteLn(tf, s);

        if Data.FillCurrentRow > Data.FillTable.RowCount then
        begin
          s := '（%d）（第%d行为首行）';
          s := Format(s, [Data.FillCurrentRow + 1, fSlantCompareSpacing + 1]);
          WriteLn(tf, '');
          WriteLn(tf, s);
        end;
      end;
    end;
  finally
    CloseFile(tf);
  end;
end;

procedure TDataComputer.SaveGroupByCompareTypeSortByCompareTypeCount;
var
  tf: TextFile;
  s, FileName, TxtFileName: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
begin
  TxtFileName := '（4）.【排列】“%d”个以上各个组合（相同代号、不同首行）的（组合数：最多 - 最少个）.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  if TFile.Exists(FileName) then TFile.Delete(FileName);
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    TSQLCompareGroup.AutoFree(Group, fDatabase, 'ORDER BY ValueCount DESC, Size DESC', []);
    TSQLCompareData.AutoFree(Data);
    while Group.FillOne do
    begin
      s := '%d.[代号：（第%s个）%s ] ；（组合数：%d个）';
      s := Format(s, [
        Group.FillCurrentRow - 1,
        CompareTypeToString(Group.Value),
        CompareTypeToString(Group.Value, 1),
        Group.ValueCount
      ]);
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);

      Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
      while Data.FillOne do
      begin
        s := Format('（%d）（第%d行为首行）', [Data.FillCurrentRow - 1, Data.FirstRow]);
        if fFirstRangeValue = 0 then
          s := s + Format('= 无【对应列】数： %d列 ；', [Data.TotalValueCount])
        else
          s := s + Format('= 无【对应列】数：%d - %d 列 ；', [Data.ValueCount, Data.TotalValueCount - Data.ValueCount]);
        s := s + Format('【列数字】：%s', [DataToString(Data.Values)]);
        WriteLn(tf, '');
        WriteLn(tf, s);
      end;
    end;
  finally
    CloseFile(tf);
  end;
end;

procedure TDataComputer.SaveGroupByCompareTypeSortByMaxValueCount;
var
  tf: TextFile;
  s, FileName, TxtFileName: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
begin
  TxtFileName := '（5）.【排列】“%d”个以上各个组合（相同代号、不同首行）的（无【对应列】数：最多 - 最少列）.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  if TFile.Exists(FileName) then TFile.Delete(FileName);
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    TSQLCompareGroup.AutoFree(Group, fDatabase, 'ORDER BY MaxTotalValueCount DESC, MaxValueCount DESC, ValueCount, Size DESC', []);
    TSQLCompareData.AutoFree(Data);
    while Group.FillOne do
    begin
      s := '%d.[代号：（第%s个）%s ] ；';
      s := Format(s, [
        Group.FillCurrentRow - 1,
        CompareTypeToString(Group.Value),
        CompareTypeToString(Group.Value, 1)
      ]);
      if fFirstRangeValue = 0 then
        s := s + Format('（ 最多 [ 无【对应列】数：%d列 ] ）', [Group.MaxTotalValueCount])
      else
        s := s + Format('（ 最多 [ 无【对应列】数：%d-%d列 ] ）', [Group.MaxValueCount, Group.MaxTotalValueCount - Group.MaxValueCount]);
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);

      Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
      while Data.FillOne do
      begin
        s := Format('（%d）（第%d行为首行）', [Data.FillCurrentRow - 1, Data.FirstRow]);
        if fFirstRangeValue = 0 then
          s := s + Format('= 无【对应列】数： %d列 ；', [Data.TotalValueCount])
        else
          s := s + Format('= 无【对应列】数：%d - %d 列 ；', [Data.ValueCount, Data.TotalValueCount - Data.ValueCount]);
        s := s + Format('【列数字】：%s', [DataToString(Data.Values)]);
        WriteLn(tf, '');
        WriteLn(tf, s);
      end;
    end;
  finally
    CloseFile(tf);
  end
end;

procedure TDataComputer.SaveGroupByCompareTypeSortByLastFirstRow;
var
  tf: TextFile;
  s, sCompareType, sCompareType2, FileName, TxtFileName: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
  RowNo: Cardinal;
begin
  TxtFileName := '（2）.【排列】“%d”个以上各个组合（相同代号、不同首行）的（邻行距：最大 → 小）（4）.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory + TxtFileName;
  if TFile.Exists(FileName) then TFile.Delete(FileName);
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    RowNo := 0;
    TSQLCompareGroup.AutoFree(Group, fDatabase, 'ORDER BY LastFirstRow, ValueCount, Size DESC', []);
    TSQLCompareData.AutoFree(Data);
    while Group.FillOne do
    begin
      sCompareType := CompareTypeToString(Group.Value);
      sCompareType2 := CompareTypeToString(Group.Value, 1);

      Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
      while Data.FillOne do
      begin
        if Data.FillCurrentRow = 2 then
        begin
          RowNo := RowNo + 1;
          s := '%d=[（第%s个）%s ] ：（%d）（%d）';
          s := Format(s, [RowNo, sCompareType, sCompareType2, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
          WriteLn(tf, s);
        end;

        RowNo := RowNo + 1;
        s := '%d=[（第%s个）%s ] ：（%d）（%d）';
        s := Format(s, [RowNo, sCompareType, sCompareType2, Data.FirstRow, Data.RowSpacing]);
        WriteLn(tf, s);

        if Data.FillCurrentRow > Data.FillTable.RowCount then
        begin
          RowNo := RowNo + 1;
          s := '%d=[（第%s个）%s ] ：（%d）（0）';
          s := Format(s, [RowNo, sCompareType, sCompareType2, fSlantCompareSpacing + 1]);
          WriteLn(tf, s);
        end;
      end;
    end;
  finally
    CloseFile(tf);
  end;
end;

procedure TDataComputer.SaveGroupByCompareTypeSortByOneRowSpacingCount;
var
  tf: TextFile;
  s, sCompareType, sCompareType2, FileName, TxtFileName: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
  RowNo: Cardinal;
begin
  TxtFileName := '（2）.【排列】“%d”个以上各个组合（相同代号、不同首行）的（邻行距：最小 → 大）（5）.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory + TxtFileName;
  if TFile.Exists(FileName) then TFile.Delete(FileName);
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    RowNo := 0;
    TSQLCompareGroup.AutoFree(Group, fDatabase, 'ORDER BY OneRowSpacingCount DESC, LastFirstRow DESC, ValueCount, Size DESC', []);
    TSQLCompareData.AutoFree(Data);
    while Group.FillOne do
    begin
      sCompareType := CompareTypeToString(Group.Value);
      sCompareType2 := CompareTypeToString(Group.Value, 1);

      Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
      while Data.FillOne do
      begin
        if Data.FillCurrentRow = 2 then
        begin
          RowNo := RowNo + 1;
          s := '%d=[（第%s个）%s ] ：（%d）（%d）';
          s := Format(s, [RowNo, sCompareType, sCompareType2, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
          WriteLn(tf, s);
        end;

        RowNo := RowNo + 1;
        s := '%d=[（第%s个）%s ] ：（%d）（%d）';
        s := Format(s, [RowNo, sCompareType, sCompareType2, Data.FirstRow, Data.RowSpacing]);
        WriteLn(tf, s);

        if Data.FillCurrentRow > Data.FillTable.RowCount then
        begin
          RowNo := RowNo + 1;
          s := '%d=[（第%s个）%s ] ：（%d）（0）';
          s := Format(s, [RowNo, sCompareType, sCompareType2, fSlantCompareSpacing + 1]);
          WriteLn(tf, s);
        end;
      end;
    end;
  finally
    CloseFile(tf);
  end;
end;

procedure TDataComputer.SaveCompareType;
var
  tf: TextFile;
  s, FileName, TxtFileName: string;
  i, j: Integer;
  Group: TSQLCompareGroup;
begin
  TxtFileName := '（6）.【保存】“%d”个以上各个组合（相同代号、不同首行）的（代号：“N”ZY ）.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  if TFile.Exists(FileName) then TFile.Delete(FileName);
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    TSQLCompareGroup.AutoFree(Group, fDatabase, 'ORDER BY ValueCount, Size DESC', []);
    while Group.FillOne do
    begin
      s := Format('%d=%s', [Group.FillCurrentRow - 1, CompareTypeToString(Group.Value, 1)]);
      WriteLn(tf, s);
    end;
  finally
    CloseFile(tf);
  end
end;

procedure TDataComputer.SaveVertGroupByFirstRow;
var
  RowSpacingList: TStringList;

  procedure AddRowSpacing(RowSpacing: Word; s: string);
  var
    sRowSpacing: string;
  begin
    sRowSpacing := RowSpacing.ToString;
    while sRowSpacing.Length < 3 do sRowSpacing := '0' + sRowSpacing;
    if RowSpacingList.IndexOfName(sRowSpacing) = -1 then
      RowSpacingList.Values[sRowSpacing] := s;
  end;

var
  tf: TextFile;
  s, FileName, FileName2, TxtFileName, sRowSpacing, sCompareTypeValueCount: string;
  i: Integer;
  FirstRow: TSQLFirstRow;
  Data: TSQLCompareData;
  LastCompareTypeValueCount: Byte;
  CompareTypeNumber: Word;
begin
  TxtFileName := '（1）.【排列】【“%d”个以上[相同首行、不同组合]的组合】.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory4 + TxtFileName;
  FileName2 := fExportDirectory3 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    WriteLn(tf, '');
    WriteLn(tf, '');
    WriteLn(tf, s);

    TSQLFirstRow.AutoFree(FirstRow, fDatabase, 'ORDER BY Value DESC', []);
    TSQLCompareData.AutoFree(Data);
    RowSpacingList := TStringList.Create;
    try
      while FirstRow.FillOne do
      begin
        if FirstRow.FillCurrentRow = 2 then
        begin
          s := '1.[ 直连（第%d行为首行）]；[ 邻行距 ↓%d ]';
          s := Format(s, [fRowCount + 1, fRowCount + 1 - FirstRow.Value]);

          AddRowSpacing(fRowCount + 1 - FirstRow.Value, s);
        end;
        s := '%d.[ 直连（第%d行为首行）]；[ 邻行距 ↓%d ]';
        s := Format(s, [FirstRow.FillCurrentRow, FirstRow.Value, FirstRow.RowSpacing]);
        AddRowSpacing(FirstRow.RowSpacing, s);
      end;

      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '最大-小的邻行距 1-100 行内：');
      RowSpacingList.Sort;
      for i := RowSpacingList.Count - 1 downto RowSpacingList.Count - 100 do
      begin
        if i < 0 then Break;
        WriteLn(tf, '');
        WriteLn(tf, RowSpacingList.ValueFromIndex[i]);
      end;
    finally
      RowSpacingList.Free;
    end;

    FirstRow.FillRewind;
    while FirstRow.FillOne do
    begin
      if FirstRow.FillCurrentRow = 2 then
      begin
        s := '1.[ 直连（第%d行为首行）]；[ 邻行距 ↓%d ]';
        s := Format(s, [fRowCount + 1, fRowCount + 1 - FirstRow.Value]);
        WriteLn(tf, '');
        WriteLn(tf, '');
        WriteLn(tf, '');
        WriteLn(tf, s);
      end;

      s := '%d.[ 直连（第 %d行为首行）]；[ 邻行距 ↓%d ；';
      s := Format(s, [FirstRow.FillCurrentRow, FirstRow.Value, FirstRow.RowSpacing]);
      if FirstRow.MaxGroupValueCount > fExportGroupValueCount then
      begin
        sCompareTypeValueCount := '';
        for i := fExportGroupValueCount to FirstRow.MaxGroupValueCount do
        begin
          if not sCompareTypeValueCount.IsEmpty then
            sCompareTypeValueCount := sCompareTypeValueCount + '、';
          sCompareTypeValueCount := sCompareTypeValueCount + i.ToString;
        end;

        s := s + Format('（第%s次组合）总共 %d 组 ；', [
          sCompareTypeValueCount,
          FirstRow.GroupValueCount
        ]);
      end;
      s := s + Format('最多（第%d次组合）：共 %d 组 ]：', [
        FirstRow.MaxRowCountGroupValueCount,
        FirstRow.MaxGroupValueCountRowCount
      ]);

      if FirstRow.FillCurrentRow > 2 then
      begin
        WriteLn(tf, '');
        WriteLn(tf, '');
      end;
      WriteLn(tf, '');
      WriteLn(tf, s);

      LastCompareTypeValueCount := 0;
      CompareTypeNumber := 0;
      Data.FillPrepare(fDatabase, 'FirstRow = ?', [FirstRow.Value]);
      while Data.FillOne do
      begin
        if Data.GroupValueCount <> LastCompareTypeValueCount then
        begin
          LastCompareTypeValueCount := Data.GroupValueCount;
          CompareTypeNumber := 0;
        end;
        CompareTypeNumber := CompareTypeNumber + 1;

        s := '【%d】.第%d次组合[ 代号：%d.（%s）]：';
        s := Format(s, [
          Data.FillCurrentRow - 1,
          Data.GroupValueCount,
          CompareTypeNumber,
          VertCompareTypeToString(Data.GroupValue)
        ]);
        WriteLn(tf, '');
        WriteLn(tf, s);

        SaveCompareData(tf, Data);
      end;
    end;
    s := '%d.[（第%d行为首行）直连（第%d-1行）]';
    s := Format(s, [
      FirstRow.FillTable.RowCount + 2,
      fVertCompareSpacing + 1,
      fVertCompareSpacing
    ]);
    WriteLn(tf, '');
    WriteLn(tf, s);
  finally
    CloseFile(tf);
  end;
  if TFile.Exists(FileName) then TFile.Copy(FileName, FileName2);
end;

procedure TDataComputer.SaveVertGroupByCompareTypeSortByRowcount;
var
  tf, tf2: TextFile;
  s, sCompareType, sCompareType2, FileName, FileName2, FileName3, TxtFileName: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
  RowNo: Cardinal;
  LastFirstRow: Word;
begin
  TxtFileName := '（2）.【排列】【“%d”个以上[相同组合、不同首行]的组合[不同首行数：最多→少]】.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory4 + TxtFileName;
  TxtFileName := '（2）.【排列】【“%d”个以上[相同组合、不同首行]的组合[不同首行数：最多→少]】（1）.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName3 := fExportDirectory3 + TxtFileName;
  TxtFileName := '（2）.【排列】【“%d”个以上[相同组合、不同首行]的组合[不同首行数：最多→少]】（2）.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName2 := fExportDirectory3 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  AssignFile(tf2, FileName2);
  Rewrite(tf2);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    WriteLn(tf, '');
    WriteLn(tf, '');
    WriteLn(tf, s);
    s := TPath.GetFileNameWithoutExtension(FileName2);
    WriteLn(tf2, '');
    WriteLn(tf2, '');
    WriteLn(tf2, s);

    RowNo := 0;
    TSQLCompareGroup.AutoFree(Group, fDatabase,
      'RowCount = ? ORDER BY RowCount DESC, ValueCount, Size DESC',
      [fExportGroupRowCount]);
    TSQLCompareData.AutoFree(Data);
    while Group.FillOne do
    begin
      s := '%d.【“%d”个 [ 相同组合、不同（直连）首行]的组合 [ 不同（直连）首行数：%d ]】：';
      s := Format(s, [
        Group.FillCurrentRow - 1,
        Group.ValueCount,
        Group.RowCount
      ]);
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);
      WriteLn(tf2, '');
      WriteLn(tf2, '');
      WriteLn(tf2, '');

      LastFirstRow := 0;
      Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
      while Data.FillOne do
      begin
        sCompareType := VertCompareTypeToString(Group.Value);

        if Data.FillCurrentRow = 2 then
        begin
          RowNo := RowNo + 1;
          s := '1= 第%d次组合[ 代号：3.（%s）]：（%d）（%d）';
          s := Format(s, [Data.GroupValueCount, sCompareType, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
          WriteLn(tf2, '');
          WriteLn(tf2, s);
        end;

        if LastFirstRow <> Data.FirstRow then
        begin
          LastFirstRow := Data.FirstRow;
          s := '【%d】.第%d次组合[ 代号：1.（%s）]：';
          s := Format(s, [Data.FillCurrentRow - 1, Data.GroupValueCount, sCompareType]);

          WriteLn(tf, '');
          WriteLn(tf, s);
        end;

        SaveCompareData(tf, Data);

        s := '%d= 第%d次组合[ 代号：1.（%s）]：（%d）（%d）';
        s := Format(s, [Data.FillCurrentRow, Data.GroupValueCount, sCompareType, Data.FirstRow, Data.RowSpacing]);
        WriteLn(tf2, '');
        WriteLn(tf2, s);

        SaveCompareData(tf2, Data, False);

        if Data.FillCurrentRow > Data.FillTable.RowCount then
        begin
          RowNo := RowNo + 1;
          s := '%d= 第%d次组合[ 代号：1.（%s）]：（%d）（0）';
          s := Format(s, [Data.FillCurrentRow + 1, Data.GroupValueCount, sCompareType, fVertCompareSpacing + 1]);
          WriteLn(tf2, '');
          WriteLn(tf2, s);
        end;
      end;
    end;
  finally
    CloseFile(tf);
    CloseFile(tf2);
  end;
  if TFile.Exists(FileName) then TFile.Copy(FileName, FileName3);
end;

procedure TDataComputer.SaveVertGroupByCompareTypeSortByMaxRowSpacing;
var
  tf: TextFile;
  s, FileName, TxtFileName: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
  RowNo: Word;
begin
  TxtFileName := '（3）.【排列】【“%d”个以上[相同组合、不同首行]的组合[邻行距：最大↓→小↓]】.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory4 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    WriteLn(tf, '');
    WriteLn(tf, '');
    WriteLn(tf, s);

    TSQLCompareGroup.AutoFree(Group, fDatabase, 'ORDER BY ValueCount DESC, LastFirstRow, Size', []);
    TSQLCompareData.AutoFree(Data);
    while Group.FillOne do
    begin
      RowNo := 2;
      Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
      while Data.FillOne do
      begin
        if Data.FillCurrentRow = 2 then
        begin
          s := '%d.[ 直连（第%d行为首行）] .（最大邻行距 ↓%d ）：';
          s := Format(s, [
            Group.FillCurrentRow - 1,
            fRowCount + 1,
            fRowCount + 1 - Data.FirstRow
          ]);
          WriteLn(tf, '');
          WriteLn(tf, '');
          WriteLn(tf, '');
          WriteLn(tf, s);

          s := '（1）.[ 直连（第%d行为首行）] .（邻行距 ↓%d ）：';
          s := Format(s, [
            fRowCount + 1,
            fRowCount + 1 - Data.FirstRow
          ]);
          WriteLn(tf, '');
          WriteLn(tf, s);

          s := '（2）.[ 直连（第%d行为首行）] .（邻行距 ↓%d ）：';
          s := Format(s, [Data.FirstRow, Data.FirstRow - fVertCompareSpacing - 1]);
          WriteLn(tf, '');
          WriteLn(tf, s);
        end;

        s := '【%d】.第%d次组合[ 代号：3.（%s）]：';
        s := Format(s, [
          Data.FillCurrentRow - 1,
          Data.GroupValueCount,
          VertCompareTypeToString(Group.Value)
        ]);
        WriteLn(tf, '');
        WriteLn(tf, s);

        SaveCompareData(tf, Data);

        RowNo := RowNo + 1;
        if Data.FillCurrentRow > Data.FillTable.RowCount then
        begin
          s := '（%d）.[（第%d行为首行）直连（第%d-1行）]';
          s := Format(s, [RowNo, fVertCompareSpacing + 1, fVertCompareSpacing]);
          WriteLn(tf, '');
          WriteLn(tf, s);
        end
        else
        begin
          s := '（%d）.[ 直连（第%d行为首行）] .（邻行距 ↓0 ）：';
          s := Format(s, [RowNo, fVertCompareSpacing + 1]);
          WriteLn(tf, '');
          WriteLn(tf, s);
        end;
      end;
    end;
  finally
    CloseFile(tf);
  end;
end;

procedure TDataComputer.SaveVertGroupByCompareTypeSortByCompareTypeCount;
var
  tf: TextFile;
  s, FileName, TxtFileName: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
begin
  TxtFileName := '（4）.【排列】【“%d”个以上[相同组合、不同首行]的组合[组合数：最多→少个]】.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory4 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    WriteLn(tf, '');
    WriteLn(tf, '');
    WriteLn(tf, s);

    TSQLCompareGroup.AutoFree(Group, fDatabase, 'ORDER BY ValueCount DESC, Size DESC', []);
    TSQLCompareData.AutoFree(Data);
    while Group.FillOne do
    begin
      s := '%d.[ 第%d次组合；代号：1.（%s）]；（组合数：%d个）：';
      s := Format(s, [
        Group.FillCurrentRow - 1,
        Group.ValueCount,
        VertCompareTypeToString(Group.Value),
        Group.ValueCount
      ]);
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);

      Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
      while Data.FillOne do
      begin
        SaveCompareData(tf, Data);
      end;
    end;
  finally
    CloseFile(tf);
  end;
end;

procedure TDataComputer.SaveVertGroupByCompareTypeSortByMaxValueCount;
var
  tf: TextFile;
  s, FileName, TxtFileName: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
begin
  TxtFileName := '（5）.【排列】【“%d”个以上[相同组合、不同首行]的组合[无【对应列】数：最多→少列]】.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory4 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    WriteLn(tf, '');
    WriteLn(tf, '');
    WriteLn(tf, s);

    TSQLCompareGroup.AutoFree(Group, fDatabase, 'ORDER BY MaxTotalValueCount DESC, MaxValueCount DESC, ValueCount, Size DESC', []);
    TSQLCompareData.AutoFree(Data);
    while Group.FillOne do
    begin
      s := '%d.[ 第%d次组合；代号：1.（%s）]；（ w无【对应列】总数：%d列）：';
      s := Format(s, [
        Group.FillCurrentRow - 1,
        Group.ValueCount,
        VertCompareTypeToString(Group.Value),
        Group.MaxTotalValueCount
      ]);
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);

      Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
      while Data.FillOne do
      begin
        if Data.FillCurrentRow > 2 then WriteLn(tf, '');
        SaveCompareData(tf, Data);
      end;
    end;
  finally
    CloseFile(tf);
  end;
end;

procedure TDataComputer.SaveVertGroupByCompareTypeSortByLastFirstRow;
var
  tf: TextFile;
  s, FileName, TxtFileName, sCompareType: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
  RowNo: Word;
begin
  TxtFileName := '（2）.【排列】【“%d”个以上[相同组合、不同首行]的组合 [首行邻行距：最大↓→小↓]】（3）.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory3 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    WriteLn(tf, '');
    WriteLn(tf, '');
    WriteLn(tf, s);

    RowNo := 0;

    TSQLCompareGroup.AutoFree(Group, fDatabase, 'ORDER BY LastFirstRow, ValueCount DESC, Size', []);
    TSQLCompareData.AutoFree(Data);
    while Group.FillOne do
    begin
      sCompareType := VertCompareTypeToString(Group.Value);

      Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
      while Data.FillOne do
      begin
        if Data.FillCurrentRow = 2 then
        begin
          RowNo := RowNo + 1;
          s := '%d= 第%d次组合[ 代号：1.（%s）]：（%d）（%d）	';
          s := Format(s, [RowNo, Data.GroupValueCount, sCompareType, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
          WriteLn(tf, '');
          WriteLn(tf, '');
          WriteLn(tf, '');
          WriteLn(tf, s);
        end;

        RowNo := RowNo + 1;
        s := '%d= 第%d次组合[ 代号：1.（%s）]：（%d）（%d）	';
        s := Format(s, [RowNo, Data.GroupValueCount, sCompareType, Data.FirstRow, Data.RowSpacing]);
        WriteLn(tf, '');
        WriteLn(tf, s);

        SaveCompareData(tf, Data, False);

        if Data.FillCurrentRow > Data.FillTable.RowCount then
        begin
          RowNo := RowNo + 1;
          s := '%d= 第%d次组合[ 代号：1.（%s）]：（%d）（0）	';
          s := Format(s, [RowNo, Data.GroupValueCount, sCompareType, fVertCompareSpacing + 1]);
          WriteLn(tf, '');
          WriteLn(tf, s);
        end;
      end;
    end;
  finally
    CloseFile(tf);
  end;
end;

procedure TDataComputer.SaveVertGroupByCompareTypeSortByLastFirstRow2;
var
  tf: TextFile;
  s, FileName, TxtFileName, sCompareType: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
  RowNo: Word;
begin
  TxtFileName := '（2）.【排列】【“%d”个以上[相同组合、不同首行]的组合 [首行邻行距：最小↓→大↓]】（4）.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory3 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    WriteLn(tf, '');
    WriteLn(tf, '');
    WriteLn(tf, s);

    RowNo := 0;

    TSQLCompareGroup.AutoFree(Group, fDatabase, 'ORDER BY LastFirstRow DESC, ValueCount, Size DESC', []);
    TSQLCompareData.AutoFree(Data);
    while Group.FillOne do
    begin
      sCompareType := VertCompareTypeToString(Group.Value);

      Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
      while Data.FillOne do
      begin
        if Data.FillCurrentRow = 2 then
        begin
          RowNo := RowNo + 1;
          s := '%d= 第%d次组合[ 代号：1.（%s）]：（%d）（%d）	';
          s := Format(s, [RowNo, Data.GroupValueCount, sCompareType, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
          WriteLn(tf, '');
          WriteLn(tf, '');
          WriteLn(tf, '');
          WriteLn(tf, s);
        end;

        RowNo := RowNo + 1;
        s := '%d= 第%d次组合[ 代号：1.（%s）]：（%d）（%d）	';
        s := Format(s, [RowNo, Data.GroupValueCount, sCompareType, Data.FirstRow, Data.RowSpacing]);
        WriteLn(tf, '');
        WriteLn(tf, s);

        SaveCompareData(tf, Data, False);

        if Data.FillCurrentRow > Data.FillTable.RowCount then
        begin
          RowNo := RowNo + 1;
          s := '%d= 第%d次组合[ 代号：1.（%s）]：（%d）（0）	';
          s := Format(s, [RowNo, Data.GroupValueCount, sCompareType, fVertCompareSpacing + 1]);
          WriteLn(tf, '');
          WriteLn(tf, s);
        end;
      end;
    end;
  finally
    CloseFile(tf);
  end;
end;

procedure TDataComputer.SaveVertCompareType;
var
  tf: TextFile;
  s, FileName, TxtFileName, sCompareType: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
begin
  TxtFileName := '（6）.【简化】【“%d”个以上[相同组合、不同首行]的组合】.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory4 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    s := TxtFileName.SubString(0, TxtFileName.Length - 4);
    WriteLn(tf, '');
    WriteLn(tf, '');
    WriteLn(tf, s);

    TSQLCompareGroup.AutoFree(Group, fDatabase, 'ORDER BY MaxTotalValueCount DESC, MaxValueCount DESC, ValueCount, Size DESC', []);
    TSQLCompareData.AutoFree(Data);
    while Group.FillOne do
    begin
      s := '%d.[ 第%d次组合；代号：1.（%s）]；（ w无【对应列】总数：%d列）：';
      s := Format(s, [
        Group.FillCurrentRow - 1,
        Group.ValueCount,
        VertCompareTypeToString(Group.Value),
        Group.MaxTotalValueCount
      ]);
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);

      Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
      while Data.FillOne do
      begin
        if Data.FillCurrentRow > 2 then WriteLn(tf, '');
        SaveCompareData(tf, Data);
      end;
    end;
  finally
    CloseFile(tf);
  end;
end;

procedure TDataComputer.SaveCompareData(const tf: TextFile; Data: TSQLCompareData; SaveValues: Boolean);
var
  Row: TSQLRow;
  i, i2: Integer;
  s, sCompareType, sCompareType2: string;
begin
  TSQLRow.AutoFree(Row);
  for i := Low(Data.CompareRows) to High(Data.CompareRows) do
  begin
    with Data.CompareRows[i] do
    begin
      Row.FillPrepare(fDatabase, 'Number = ?', [RowNumber]);
      if not Row.FillOne then Continue;

      case CompareType of
        1:
        begin
          sCompareType := '直';
          sCompareType2 := ' 直';
        end;
        2:
        begin
          sCompareType := '斜';
          sCompareType2 := 'Y右';
        end;
        3:
        begin
          sCompareType := '斜';
          sCompareType2 := 'Z左';
        end;
      end;

      s := Format('（%d）【（第%d行为首行）%s连[（第%d行）（第%d%s连行）]】', [
        Number,
        Data.FirstRow,
        sCompareType,
        RowNumber,
        Data.FirstRow - RowNumber,
        sCompareType2
      ]);
      if SaveValues then
      begin
        s := s + '= ';
        if TotalSameValueCount > 0 then
        begin
          if CompareType = 1 then s := s + 'y'
          else s := s + ' ';
          if fFirstRangeValue = 0 then
            s := s + Format('有【对应列】数：%d列：', [TotalSameValueCount])
          else
            s := s + Format('有【对应列】数；【 %d-%d 】列：', [SameValueCount, TotalSameValueCount - SameValueCount]);
          s := s + DataToString(SameValues);
        end;
        if TotalDifferentValueCount > 0 then
        begin
          if TotalSameValueCount > 0 then s := s + '  ';
          if CompareType = 1 then s := s + 'w'
          else s := s + ' ';
          if fFirstRangeValue = 0 then
            s := s + Format('无【对应列】数：%d列：', [TotalDifferentValueCount])
          else
            s := s + Format('无【对应列】数；【 %d-%d 】列：', [DifferentValueCount, TotalDifferentValueCount - DifferentValueCount]);
          s := s + DataToString(DifferentValues);
        end;
      end;
      WriteLn(tf, '');
      WriteLn(tf, s);
    end;
  end;
end;

procedure TDataComputer.SaveVertSlantGroupByFirstRow;
var
  RowSpacingList: TStringList;

  procedure AddRowSpacing(RowSpacing: Word; s: string);
  var
    sRowSpacing: string;
  begin
    sRowSpacing := RowSpacing.ToString;
    while sRowSpacing.Length < 3 do sRowSpacing := '0' + sRowSpacing;
    if RowSpacingList.IndexOfName(sRowSpacing) = -1 then
      RowSpacingList.Values[sRowSpacing] := s;
  end;

var
  tf: TextFile;
  s, FileName, FileName2, TxtFileName, sRowSpacing, sGroupValueCount: string;
  i: Integer;
  FirstRow: TSQLFirstRow;
  Data: TSQLCompareData;
  LastGroupValueCount: Byte;
  GroupNumber: Word;
begin
  TxtFileName := '（1）.【排列】【“%d”个以上[相同首行、不同组合]的组合】.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory6 + TxtFileName;
  FileName2 := fExportDirectory5 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    WriteLn(tf, '');
    WriteLn(tf, '');
    WriteLn(tf, s);

    TSQLFirstRow.AutoFree(FirstRow, fDatabase, 'ORDER BY Value DESC', []);
    TSQLCompareData.AutoFree(Data);
    RowSpacingList := TStringList.Create;
    try
      while FirstRow.FillOne do
      begin
        if FirstRow.FillCurrentRow = 2 then
        begin
          s := '1.[ 直、斜连（第%d行为首行）]；[ 邻行距 ↓%d ]';
          s := Format(s, [fRowCount + 1, fRowCount + 1 - FirstRow.Value]);

          AddRowSpacing(fRowCount + 1 - FirstRow.Value, s);
        end;
        s := '%d.[ 直、斜连（第%d行为首行）]；[ 邻行距 ↓%d ]';
        s := Format(s, [FirstRow.FillCurrentRow, FirstRow.Value, FirstRow.RowSpacing]);
        AddRowSpacing(FirstRow.RowSpacing, s);
      end;

      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '最大-小的邻行距 1-100 行内：');
      RowSpacingList.Sort;
      for i := RowSpacingList.Count - 1 downto RowSpacingList.Count - 100 do
      begin
        if i < 0 then Break;
        WriteLn(tf, '');
        WriteLn(tf, RowSpacingList.ValueFromIndex[i]);
      end;
    finally
      RowSpacingList.Free;
    end;

    FirstRow.FillRewind;
    while FirstRow.FillOne do
    begin
      if FirstRow.FillCurrentRow = 2 then
      begin
        s := '1.[ 直、斜连（第%d行为首行）]；[ 邻行距 ↓%d ]';
        s := Format(s, [fRowCount + 1, fRowCount + 1 - FirstRow.Value]);
        WriteLn(tf, '');
        WriteLn(tf, '');
        WriteLn(tf, '');
        WriteLn(tf, s);
      end;

      s := '%d.[ 直、斜连（第 %d行为首行）]；[ 邻行距 ↓%d ；';
      s := Format(s, [FirstRow.FillCurrentRow, FirstRow.Value, FirstRow.RowSpacing]);
      if FirstRow.MaxGroupValueCount > fExportGroupValueCount then
      begin
        sGroupValueCount := '';
        for i := fExportGroupValueCount to FirstRow.MaxGroupValueCount do
        begin
          if not sGroupValueCount.IsEmpty then
            sGroupValueCount := sGroupValueCount + '、';
          sGroupValueCount := sGroupValueCount + i.ToString;
        end;

        s := s + Format('（第%s次组合）总共 %d 组 ；', [
          sGroupValueCount,
          FirstRow.GroupValueCount
        ]);
      end;
      s := s + Format('最多（第%d次组合）：共 %d 组 ]：', [
        FirstRow.MaxRowCountGroupValueCount,
        FirstRow.MaxGroupValueCountRowCount
      ]);

      if FirstRow.FillCurrentRow > 2 then
      begin
        WriteLn(tf, '');
        WriteLn(tf, '');
      end;
      WriteLn(tf, '');
      WriteLn(tf, s);

      LastGroupValueCount := 0;
      GroupNumber := 0;
      Data.FillPrepare(fDatabase, 'FirstRow = ?', [FirstRow.Value]);
      while Data.FillOne do
      begin
        if Data.GroupValueCount <> LastGroupValueCount then
        begin
          LastGroupValueCount := Data.GroupValueCount;
          GroupNumber := 0;
        end;
        GroupNumber := GroupNumber + 1;

        s := '【%d】.第%d次组合[ 代号：%d.（%s）]：';
        s := Format(s, [
          Data.FillCurrentRow - 1,
          Data.GroupValueCount,
          GroupNumber,
          VertCompareTypeToString(Data.GroupValue)
        ]);
        WriteLn(tf, '');
        WriteLn(tf, s);

        SaveCompareData(tf, Data);
      end;
    end;
    s := '%d.[（第%d行为首行）直连（第%d-1行）]';
    s := Format(s, [
      FirstRow.FillTable.RowCount + 2,
      fVertSlantCompareSpacing + 1,
      fVertSlantCompareSpacing
    ]);
    WriteLn(tf, '');
    WriteLn(tf, s);
  finally
    CloseFile(tf);
  end;
  if TFile.Exists(FileName) then TFile.Copy(FileName, FileName2);
end;

procedure TDataComputer.SaveVertSlantGroupByCompareTypeSortByRowcount;
var
  tf, tf2, tf3: TextFile;
  s, sCompareType, sCompareType2, FileName, FileName2, FileName3, FileName4, TxtFileName: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
  RowNo: Cardinal;
  LastFirstRow: Word;
begin
  TxtFileName := '（2）.【排列】【“%d”个以上[相同组合、不同首行]的组合[不同首行数：最多→少]】.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory6 + TxtFileName;
  TxtFileName := '（2）.【排列】【“%d”个以上[相同组合、不同首行]的组合[不同首行数：最多→少]】（1）.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName4 := fExportDirectory5 + TxtFileName;
  TxtFileName := '（2）.【排列】【“%d”个以上[相同组合、不同首行]的组合[不同首行数：最多→少]】（2）.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName2 := fExportDirectory5 + TxtFileName;
  TxtFileName := '（2）.【排列】【“%d”个以上[相同组合、不同首行]的组合[不同首行数：最多→少]】（3）.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName3 := fExportDirectory5 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  AssignFile(tf2, FileName2);
  Rewrite(tf2);
  AssignFile(tf3, FileName3);
  Rewrite(tf3);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    WriteLn(tf, '');
    WriteLn(tf, '');
    WriteLn(tf, s);
    s := TPath.GetFileNameWithoutExtension(FileName2);
    WriteLn(tf2, '');
    WriteLn(tf2, '');
    WriteLn(tf2, s);
    s := TPath.GetFileNameWithoutExtension(FileName3);
    WriteLn(tf3, '');
    WriteLn(tf3, '');
    WriteLn(tf3, s);

    RowNo := 0;
    TSQLCompareGroup.AutoFree(Group, fDatabase,
      'RowCount >= ? ORDER BY RowCount DESC, ValueCount, Size DESC',
      [fExportGroupRowCount]);
    TSQLCompareData.AutoFree(Data);
    while Group.FillOne do
    begin
      s := '%d.【“%d”个 [ 相同组合、不同（直、斜连）首行]的组合 [ 不同（直、斜连）首行数：%d ]】：';
      s := Format(s, [
        Group.FillCurrentRow - 1,
        Group.ValueCount,
        Group.RowCount
      ]);
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);
      WriteLn(tf2, '');
      WriteLn(tf2, '');
      WriteLn(tf2, '');
      WriteLn(tf2, s);
      WriteLn(tf3, '');
      WriteLn(tf3, '');
      WriteLn(tf3, '');

      LastFirstRow := 0;
      Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
      while Data.FillOne do
      begin
        sCompareType := VertCompareTypeToString(Group.Value);

        if Data.FillCurrentRow = 2 then
        begin
          s := '1= 第%d次组合[ 代号：3.（%s）]：（%d）（%d）';
          s := Format(s, [Group.ValueCount, sCompareType, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
          WriteLn(tf2, '');
          WriteLn(tf2, s);

          RowNo := RowNo + 1;
          s := '%d=[ 代号：3.（%s）]：（%d）（%d）';
          s := Format(s, [RowNo, sCompareType, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
          WriteLn(tf3, '');
          WriteLn(tf3, s);
        end;

        if LastFirstRow <> Data.FirstRow then
        begin
          LastFirstRow := Data.FirstRow;
          s := '【%d】.第%d次组合[ 代号：1.（%s）]：';
          s := Format(s, [Data.FillCurrentRow - 1, Group.ValueCount, sCompareType]);

          WriteLn(tf, '');
          WriteLn(tf, s);
        end;

        SaveCompareData(tf, Data);

        s := '%d= 第%d次组合[ 代号：1.（%s）]：（%d）（%d）';
        s := Format(s, [Data.FillCurrentRow, Group.ValueCount, sCompareType, Data.FirstRow, Data.RowSpacing]);
        WriteLn(tf2, '');
        WriteLn(tf2, s);

        SaveCompareData(tf2, Data);

        RowNo := RowNo + 1;
        s := '%d=[ 代号：1.（%s）]：（%d）（%d）';
        s := Format(s, [RowNo, sCompareType, Data.FirstRow, Data.RowSpacing]);
        WriteLn(tf3, '');
        WriteLn(tf3, s);

        SaveCompareData(tf3, Data, False);

        if Data.FillCurrentRow > Data.FillTable.RowCount then
        begin
          s := '%d= 第%d次组合[ 代号：1.（%s）]：（%d）（0）';
          s := Format(s, [Data.FillCurrentRow + 1, Group.ValueCount, sCompareType, Data.FirstRow]);
          WriteLn(tf2, '');
          WriteLn(tf2, s);

          RowNo := RowNo + 1;
          s := '%d=[ 代号：1.（%s）]：（%d）（0）';
          s := Format(s, [RowNo, sCompareType, Data.FirstRow]);
          WriteLn(tf3, '');
          WriteLn(tf3, s);
        end;
      end;
    end;
  finally
    CloseFile(tf);
    CloseFile(tf2);
    CloseFile(tf3);
  end;
  if TFile.Exists(FileName) then TFile.Copy(FileName, FileName4);
end;

procedure TDataComputer.SaveVertSlantGroupByCompareTypeSortByMaxRowSpacing;
var
  tf: TextFile;
  s, FileName, TxtFileName: string;
  Group: TSQLCompareGroup;
  Data, Data2: TSQLCompareData;
  RowNo: Word;
begin
  TxtFileName := '（3）.【排列】【“%d”个以上[相同组合、不同首行]的组合[邻行距：最大↓→小↓]】.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory6 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    WriteLn(tf, '');
    WriteLn(tf, '');
    WriteLn(tf, s);

    TSQLCompareGroup.AutoFree(Group, fDatabase, 'ORDER BY ValueCount DESC, LastFirstRow, Size DESC', []);
    TSQLCompareData.AutoFree(Data);
    TSQLCompareData.AutoFree(Data2);
    while Group.FillOne do
    begin
      RowNo := 2;
      Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
      while Data.FillOne do
      begin
        if Data.FillCurrentRow = 2 then
        begin
          s := '%d.[ 直连（第%d行为首行）] .（最大邻行距 ↓%d ）：';
          s := Format(s, [
            Group.FillCurrentRow - 1,
            fRowCount + 1,
            fRowCount + 1 - Data.FirstRow
          ]);
          WriteLn(tf, '');
          WriteLn(tf, '');
          WriteLn(tf, '');
          WriteLn(tf, s);

          s := '（1）.[ 直连（第%d行为首行）] .（邻行距 ↓%d ）：';
          s := Format(s, [
            fRowCount + 1,
            fRowCount + 1 - Data.FirstRow
          ]);
          WriteLn(tf, '');
          WriteLn(tf, s);

          s := '（2）.[ 直连（第%d行为首行）] .（邻行距 ↓%d ）：';
          s := Format(s, [Data.FirstRow, Data.RowSpacing]);
          WriteLn(tf, '');
          WriteLn(tf, s);
        end;

        s := '【%d】.第%d次组合[ 代号：3.（%s）]：';
        s := Format(s, [
          Data.FillCurrentRow - 1,
          Group.ValueCount,
          VertCompareTypeToString(Data.GroupValue)
        ]);
        WriteLn(tf, '');
        WriteLn(tf, s);

        SaveCompareData(tf, Data);

        RowNo := RowNo + 1;
        if Data.FillCurrentRow > Data.FillTable.RowCount then
        begin
          s := '（%d）.[（第%d行为首行）直、斜连（第%d-%d行）]';
          s := Format(s, [RowNo, Data.FirstRow, Data.FirstRow - 1, Data.FirstRow - 2]);
          WriteLn(tf, '');
          WriteLn(tf, s);
        end
        else
        begin
          Data.FillRow(Data.FillCurrentRow, Data2);
          if Data.FillCurrentRow = Data.FillTable.RowCount then
            Data2.RowSpacing := 0;

          s := '（%d）.[ 直、斜（第%d行为首行）] .（邻行距 ↓%d ）：';
          s := Format(s, [RowNo, Data2.FirstRow, Data2.RowSpacing]);
          WriteLn(tf, '');
          WriteLn(tf, s);
        end;
      end;
    end;
  finally
    CloseFile(tf);
  end;
end;

procedure TDataComputer.SaveVertSlantGroupByCompareTypeSortByCompareTypeCount;
var
  tf: TextFile;
  s, FileName, TxtFileName: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
begin
  TxtFileName := '（4）.【排列】【“%d”个以上[相同组合、不同首行]的组合[组合数：最多→少个]】.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory6 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    WriteLn(tf, '');
    WriteLn(tf, '');
    WriteLn(tf, s);

    TSQLCompareGroup.AutoFree(Group, fDatabase, 'ORDER BY ValueCount DESC, Size DESC', []);
    TSQLCompareData.AutoFree(Data);
    while Group.FillOne do
    begin
      s := '%d.[ 第%d次组合；代号：1.（%s）]；（组合数：%d个）：';
      s := Format(s, [
        Group.FillCurrentRow - 1,
        Group.ValueCount,
        VertCompareTypeToString(Group.Value),
        Group.ValueCount
      ]);
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);

      Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC LIMIT 1', [Group.Value]);
      while Data.FillOne do
      begin
        SaveCompareData(tf, Data);
      end;
    end;
  finally
    CloseFile(tf);
  end;
end;

procedure TDataComputer.SaveVertSlantGroupByCompareTypeSortByMaxValueCount;
var
  tf: TextFile;
  s, FileName, TxtFileName: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
begin
  TxtFileName := '（5）.【排列】【“%d”个以上[相同组合、不同首行]的组合[无【对应列】数：最多→少列]】.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory6 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    WriteLn(tf, '');
    WriteLn(tf, '');
    WriteLn(tf, s);

    TSQLCompareData.AutoFree(Data, fDatabase, 'ORDER BY CompareValueCount DESC, TotalDifferentValueCount DESC, DifferentValueCount DESC', []);
    while Data.FillOne do
    begin
      s := '%d.[ 第%d次组合；代号：1.（%s）]；（ ';
      s := Format(s, [Data.FillCurrentRow - 1, Data.GroupValueCount, VertCompareTypeToString(Data.GroupValue)]);
      if Data.TotalSameValueCount > 0 then
      begin
        s := s + 'y有【对应列】总数：';
        if fFirstRangeValue = 0 then
          s := s + Format('%d列', [Data.TotalSameValueCount])
        else
          s := s + Format('%d-%d列', [Data.SameValueCount, Data.TotalSameValueCount - Data.SameValueCount])
      end;
      if Data.TotalDifferentValueCount > 0 then
      begin
        if Data.TotalSameValueCount > 0 then s := s + '、';

        s := s + 'w无【对应列】总数：';
        if fFirstRangeValue = 0 then
          s := s + Format('%d列', [Data.TotalDifferentValueCount])
        else
          s := s + Format('%d-%d列', [Data.DifferentValueCount, Data.TotalDifferentValueCount - Data.DifferentValueCount])
      end;
      s := s + '）：';
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);

      SaveCompareData(tf, Data);
    end;
  finally
    CloseFile(tf);
  end;
end;

procedure TDataComputer.SaveVertSlantGroupByCompareTypeSortByLastFirstRow;
var
  tf: TextFile;
  s, FileName, TxtFileName, sCompareType: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
  RowNo: Word;
begin
  TxtFileName := '（2）.【排列】【“%d”个以上[相同组合、不同首行]的组合 [首行邻行距：最大↓→小↓]】（4）.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory5 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    WriteLn(tf, '');
    WriteLn(tf, '');
    WriteLn(tf, s);

    RowNo := 0;

    TSQLCompareGroup.AutoFree(Group, fDatabase, 'ORDER BY LastFirstRow, ValueCount DESC, Size DESC', []);
    TSQLCompareData.AutoFree(Data);
    while Group.FillOne do
    begin
      sCompareType := VertCompareTypeToString(Group.Value);

      Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
      while Data.FillOne do
      begin
        if Data.FillCurrentRow = 2 then
        begin
          RowNo := RowNo + 1;
          s := '%d=[ 代号：1.（%s）]：（%d）（%d）	';
          s := Format(s, [RowNo, sCompareType, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
          WriteLn(tf, '');
          WriteLn(tf, '');
          WriteLn(tf, '');
          WriteLn(tf, s);
        end;

        RowNo := RowNo + 1;
        s := '%d=[ 代号：1.（%s）]：（%d）（%d）	';
        s := Format(s, [RowNo, sCompareType, Data.FirstRow, Data.RowSpacing]);
        WriteLn(tf, '');
        WriteLn(tf, s);

        SaveCompareData(tf, Data, False);

        if Data.FillCurrentRow > Data.FillTable.RowCount then
        begin
          RowNo := RowNo + 1;
          s := '%d=[ 代号：1.（%s）]：（%d）（0）	';
          s := Format(s, [RowNo, sCompareType, Data.FirstRow]);
          WriteLn(tf, '');
          WriteLn(tf, s);
        end;
      end;
    end;
  finally
    CloseFile(tf);
  end;
end;

procedure TDataComputer.SaveVertSlantGroupByCompareTypeSortByLastFirstRow2;
var
  tf: TextFile;
  s, FileName, TxtFileName, sCompareType: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
  RowNo: Word;
begin
  TxtFileName := '（2）.【排列】【“%d”个以上[相同组合、不同首行]的组合 [首行邻行距：最小↓→大↓]】（5）.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory5 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    WriteLn(tf, '');
    WriteLn(tf, '');
    WriteLn(tf, s);

    RowNo := 0;

    TSQLCompareGroup.AutoFree(Group, fDatabase, 'ORDER BY OneRowSpacingCount DESC, ValueCount, Size DESC', []);
    TSQLCompareData.AutoFree(Data);
    while Group.FillOne do
    begin
      sCompareType := VertCompareTypeToString(Group.Value);

      Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
      while Data.FillOne do
      begin
        if Data.FillCurrentRow = 2 then
        begin
          RowNo := RowNo + 1;
          s := '%d=[ 代号：1.（%s）]：（%d）（%d）	';
          s := Format(s, [RowNo, sCompareType, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
          WriteLn(tf, '');
          WriteLn(tf, '');
          WriteLn(tf, '');
          WriteLn(tf, s);
        end;

        RowNo := RowNo + 1;
        s := '%d=[ 代号：1.（%s）]：（%d）（%d）	';
        s := Format(s, [RowNo, sCompareType, Data.FirstRow, Data.RowSpacing]);
        WriteLn(tf, '');
        WriteLn(tf, s);

        SaveCompareData(tf, Data, False);

        if Data.FillCurrentRow > Data.FillTable.RowCount then
        begin
          RowNo := RowNo + 1;
          s := '%d=[ 代号：1.（%s）]：（%d）（0）	';
          s := Format(s, [RowNo, sCompareType, Data.FirstRow]);
          WriteLn(tf, '');
          WriteLn(tf, s);
        end;
      end;
    end;
  finally
    CloseFile(tf);
  end;
end;

procedure TDataComputer.SaveVertSlantCompareType;
var
  tf: TextFile;
  s, FileName, TxtFileName, sCompareType: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
begin
  TxtFileName := '（6）.【简化】【“%d”个以上[相同组合、不同首行]的组合】.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory6 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    s := TxtFileName.SubString(0, TxtFileName.Length - 4);
    WriteLn(tf, '');
    WriteLn(tf, '');
    WriteLn(tf, s);

    TSQLCompareData.AutoFree(Data, fDatabase, 'ORDER BY CompareValueCount DESC, TotalDifferentValueCount DESC, DifferentValueCount DESC', []);
    while Data.FillOne do
    begin
      s := '%d.[ 第%d次组合；代号：1.（%s）]；（ ';
      s := Format(s, [Data.FillCurrentRow - 1, Data.GroupValueCount, VertCompareTypeToString(Data.GroupValue)]);
      if Data.TotalSameValueCount > 0 then
        s := s + Format('y有【对应列】总数：%d列', [Data.TotalSameValueCount]);
      if Data.TotalDifferentValueCount > 0 then
      begin
        if Data.TotalSameValueCount > 0 then s := s + '、';

        s := s + Format('W无【对应列】总数：%d列', [Data.TotalDifferentValueCount]);
      end;
      s := s + '）：';
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);

      SaveCompareData(tf, Data);
    end;
  finally
    CloseFile(tf);
  end;
end;

procedure TDataComputer.ExportCompareData(ExportFiles: TExportFiles; GroupRowCount: Word);
var
  s: string;
  Tasks: array of ITask;

  procedure AddTask(Proc: TProc);
  var
    i: Integer;
  begin
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(Proc);
    Tasks[i].Start;
  end;
begin
  fRowCount := fDatabase.TableRowCount(TSQLRow);
  fExportGroupRowCount := GroupRowCount;

  case fCompareMode of
    cmVert:
    begin
      if not TDirectory.Exists(fExportDirectory3) then
        TDirectory.CreateDirectory(fExportDirectory3);
      for s in TDirectory.GetFiles(fExportDirectory3, '*.txt') do TFile.Delete(s);
      if not TDirectory.Exists(fExportDirectory4) then
        TDirectory.CreateDirectory(fExportDirectory4);
      for s in TDirectory.GetFiles(fExportDirectory4, '*.txt') do TFile.Delete(s);

      if efFile in ExportFiles then
        AddTask(SaveVertGroupByFirstRow);
      if efFile2 in ExportFiles then
      begin
        AddTask(SaveVertGroupByCompareTypeSortByRowcount);
        AddTask(SaveVertGroupByCompareTypeSortByLastFirstRow);
        AddTask(SaveVertGroupByCompareTypeSortByLastFirstRow2);
      end;
      if efFile3 in ExportFiles then
        AddTask(SaveVertGroupByCompareTypeSortByMaxRowSpacing);
      if efFile4 in ExportFiles then
        AddTask(SaveVertGroupByCompareTypeSortByCompareTypeCount);
      if efFile5 in ExportFiles then
        AddTask(SaveVertGroupByCompareTypeSortByMaxValueCount);
      if efFile6 in ExportFiles then
        AddTask(SaveVertCompareType);
    end;
    cmSlant:
    begin
      if not TDirectory.Exists(fExportDirectory) then
        TDirectory.CreateDirectory(fExportDirectory);
      for s in TDirectory.GetFiles(fExportDirectory, '*.txt') do TFile.Delete(s);
      if not TDirectory.Exists(fExportDirectory2) then
        TDirectory.CreateDirectory(fExportDirectory2);
      for s in TDirectory.GetFiles(fExportDirectory2, '*.txt') do TFile.Delete(s);

      if efFile in ExportFiles then
        AddTask(SaveGroupByFirstRow);
      if efFile2 in ExportFiles then
      begin
        AddTask(SaveGroupByCompareTypeSortByRowcount);
        AddTask(SaveGroupByCompareTypeSortByLastFirstRow);
        AddTask(SaveGroupByCompareTypeSortByOneRowSpacingCount);
      end;
      if efFile3 in ExportFiles then
        AddTask(SaveGroupByCompareTypeSortByMaxRowSpacing);
      if efFile4 in ExportFiles then
        AddTask(SaveGroupByCompareTypeSortByCompareTypeCount);
      if efFile5 in ExportFiles then
        AddTask(SaveGroupByCompareTypeSortByMaxValueCount);
      if efFile6 in ExportFiles then
        AddTask(SaveCompareType);
    end;
    cmVertSlant:
    begin
      if not TDirectory.Exists(fExportDirectory5) then
        TDirectory.CreateDirectory(fExportDirectory5);
      for s in TDirectory.GetFiles(fExportDirectory5, '*.txt') do TFile.Delete(s);
      if not TDirectory.Exists(fExportDirectory6) then
        TDirectory.CreateDirectory(fExportDirectory6);
      for s in TDirectory.GetFiles(fExportDirectory6, '*.txt') do TFile.Delete(s);

      if efFile in ExportFiles then
        AddTask(SaveVertSlantGroupByFirstRow);
      if efFile2 in ExportFiles then
      begin
        AddTask(SaveVertSlantGroupByCompareTypeSortByRowcount);
        AddTask(SaveVertSlantGroupByCompareTypeSortByLastFirstRow);
        AddTask(SaveVertSlantGroupByCompareTypeSortByLastFirstRow2);
      end;
      if efFile3 in ExportFiles then
        AddTask(SaveVertSlantGroupByCompareTypeSortByMaxRowSpacing);
      if efFile4 in ExportFiles then
        AddTask(SaveVertSlantGroupByCompareTypeSortByCompareTypeCount);
      if efFile5 in ExportFiles then
        AddTask(SaveVertSlantGroupByCompareTypeSortByMaxValueCount);
      if efFile6 in ExportFiles then
        AddTask(SaveVertSlantCompareType);
    end;
  end;
  TTask.WaitForAll(Tasks);
end;

end.
