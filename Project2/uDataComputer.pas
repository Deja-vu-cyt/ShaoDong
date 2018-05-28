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
  uCommon,
  uFileWriter;

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
    fNumber: Cardinal;
  published
    property Number: Cardinal read fNumber write fNumber;
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
    fFirstRow: Cardinal;
    fGroupValue: Int64;
    fGroupValue2: RawUTF8;
    fGroupValueCount: Byte;
    fCompareRows: TCompareRowArray;
    fRowSpacing: Cardinal;
    fCompareValueCount: Word;
    fTotalSameValueCount: Word;
    fSameValueCount: Word;
    fTotalDifferentValueCount: Word;
    fDifferentValueCount: Word;
    fEnabled: Boolean;
  public
    procedure CalcCompareValueCount(FirstRangeValue: Byte);
  published
    property FirstRow: Cardinal read fFirstRow write fFirstRow;
    property GroupValue: Int64 read fGroupValue write fGroupValue;
    property GroupValue2: RawUTF8 read fGroupValue2 write fGroupValue2;
    property GroupValueCount: Byte read fGroupValueCount write fGroupValueCount;
    property CompareRows: TCompareRowArray read fCompareRows write fCompareRows;
    property RowSpacing: Cardinal read fRowSpacing write fRowSpacing;
    property CompareValueCount: Word read fCompareValueCount write fCompareValueCount;
    property TotalSameValueCount: Word read fTotalSameValueCount write fTotalSameValueCount;
    property SameValueCount: Word read fSameValueCount write fSameValueCount;
    property TotalDifferentValueCount: Word read fTotalDifferentValueCount write fTotalDifferentValueCount;
    property DifferentValueCount: Word read fDifferentValueCount write fDifferentValueCount;
    property Enabled: Boolean read fEnabled write fEnabled;
  end;

  TSQLFirstRow = class(TSQLRecord)
  private
    fValue : Cardinal;
    fRowSpacing: Cardinal;
    fRowCount: Cardinal;
    fGroupValueCount: Word;
    fMaxGroupValueCount: Byte;
    fMaxRowCountGroupValueCount: Byte;
    fMaxGroupValueCountRowCount: Cardinal;
    fEnabled: Boolean;
    fRowSpacing2: Cardinal;
    fRowCount2: Cardinal;
    fGroupValueCount2: Word;
    fMaxGroupValueCount2: Byte;
    fMaxRowCountGroupValueCount2: Byte;
    fMaxGroupValueCountRowCount2: Cardinal;
  published
    property Value: Cardinal read fValue write fValue stored AS_UNIQUE;
    property RowSpacing: Cardinal read fRowSpacing write fRowSpacing;
    property RowCount: Cardinal read fRowCount write fRowCount;
    property GroupValueCount: Word read fGroupValueCount write fGroupValueCount;
    property MaxGroupValueCount: Byte read fMaxGroupValueCount write fMaxGroupValueCount;
    property MaxRowCountGroupValueCount: Byte read fMaxRowCountGroupValueCount
      write fMaxRowCountGroupValueCount;
    property MaxGroupValueCountRowCount: Cardinal read fMaxGroupValueCountRowCount
      write fMaxGroupValueCountRowCount;
    property Enabled: Boolean read fEnabled write fEnabled;
    property RowSpacing2: Cardinal read fRowSpacing2 write fRowSpacing2;
    property RowCount2: Cardinal read fRowCount2 write fRowCount2;
    property GroupValueCount2: Word read fGroupValueCount2 write fGroupValueCount2;
    property MaxGroupValueCount2: Byte read fMaxGroupValueCount2 write fMaxGroupValueCount2;
    property MaxRowCountGroupValueCount2: Byte read fMaxRowCountGroupValueCount2
      write fMaxRowCountGroupValueCount2;
    property MaxGroupValueCountRowCount2: Cardinal read fMaxGroupValueCountRowCount2
      write fMaxGroupValueCountRowCount2;
  end;

  TSQLCompareGroup = class(TSQLRecord)
  private
    fValue: Int64;
    fValue3: RawUTF8;
    fValue2: RawUTF8;
    fValueCount: Byte;
    fSize: RawUTF8;
    fRowCount: Cardinal;
    fMaxRowSpacing: Cardinal;
    fMaxTotalValueCount: Word;
    fMaxValueCount: Word;
    fLastFirstRow: Cardinal;
    fOneRowSpacingCount: Cardinal;
  published
    property Value: Int64 read fValue write fValue stored AS_UNIQUE;
    property Value3: RawUTF8 read fValue3 write fValue3;
    property Value2: RawUTF8 read fValue2 write fValue2;
    property ValueCount: Byte read fValueCount write fValueCount;
    property Size: RawUTF8 index 64 read fSize write fSize;
    property RowCount: Cardinal read fRowCount write fRowCount;
    property MaxRowSpacing: Cardinal read fMaxRowSpacing write fMaxRowSpacing;
    property MaxTotalValueCount: Word read fMaxTotalValueCount write fMaxTotalValueCount;
    property MaxValueCount: Word read fMaxValueCount write fMaxValueCount;
    property LastFirstRow: Cardinal read fLastFirstRow write fLastFirstRow;
    property OneRowSpacingCount: Cardinal read fOneRowSpacingCount write fOneRowSpacingCount;
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
      var VertCompareSpacing: Cardinal; var VertSameValueCount: Byte;
      var VertSameValueCount2: Byte; var SlantCompareSpacing: Cardinal;
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
    fVertSlantCompareSpacing: Cardinal;
    fVertCompareSpacing: Cardinal;
    fVertSameValueCount: Byte;
    fVertSameValueCount2: Byte;
    fSlantCompareSpacing: Cardinal;
    fSlantSameValueCount: Byte;
    fSlantSameValueCount2: Byte;
    fCompareGroupValueCount: Byte;
    fExportGroupValueCount: Byte;
    fMinGroupRowCount: Cardinal;
    fKeepMaxRowSpacing: Cardinal;
    function GetKeyValue(Key: string): Variant;
    procedure SetKeyValue(Key: string; Value: Variant);
    procedure SetCompareMode(AValue: TCompareMode);
    function BuildSlantGroupValue(Value: string): string;
    function CompareTypeToString(Value: Int64; Flag: Byte = 0): string;
    function VertCompareTypeToString(Value: Int64): string;
    function DataToString(Values: SynCommons.TWordDynArray): string;
    procedure CalcRowSpacing(CompareData: TSQLCompareData);
    procedure CalcFirstRowGroup(FirstRow: TSQLFirstRow; Enabled: Boolean = False);
    procedure ReCalc;

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

    procedure SaveCompareData(fr: TFileWriter; Data: TSQLCompareData; SaveValues: Boolean = True);
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
    procedure ExportCompareData(ExportFiles: TExportFiles; RowSpacing: Cardinal;
      GroupRowCount: Cardinal);
  published
    property InitEvent: TInitEvent read fInitEvent write fInitEvent;
    property InitCompareEvent: TInitCompareEvent read fInitCompareEvent write fInitCompareEvent;
    property MaxValue: Word read fMaxValue;
    property FirstRangeValue: Word read fFirstRangeValue;
    property CompareMode: TCompareMode read fCompareMode write SetCompareMode;
    property VertCompareSpacing: Cardinal read fVertCompareSpacing;
    property VertSameValueCount: Byte read fVertSameValueCount;
    property VertSameValueCount2: Byte read fVertSameValueCount2;
    property SlantCompareSpacing: Cardinal read fSlantCompareSpacing;
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

function TDataComputer.BuildSlantGroupValue(Value: string): string;
var
  s: string;
  v: Cardinal;
begin
  Result := '';
  for s in Value.Split(['.']) do
  begin
    if not Result.IsEmpty then Result := Result + '.';
    v := s.ToInteger;
    Result := Result + ((v + 1) div 2).ToString;
    if v mod 2 = 0 then Result := Result + 'Z'
    else Result := Result + 'Y';
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
    if not Result.IsEmpty then Result := Result + '��';
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
    else Result := Result + '��' + s;
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
      cmVertSlant: CompareData.RowSpacing := CompareData.FirstRow - fVertSlantCompareSpacing - 1;
    end;
  end;
end;

procedure TDataComputer.CalcFirstRowGroup(FirstRow: TSQLFirstRow; Enabled: Boolean);
var
  DataList: TSQLTableJSON;
  s: string;
  RowCount: Cardinal;
  GroupValueCount: Byte;
begin
  s := 'FirstRow = ?';
  if Enabled then s := s + ' AND Enabled = 1';
  s := s + ' GROUP BY GroupValueCount ORDER BY GroupValueCount';
  DataList := fDatabase.MultiFieldValues(TSQLCompareData,
    'GroupValueCount, Count(ID) RowCount',
    s,
    [FirstRow.Value]
  );
  try
    while DataList.Step do
    begin
      GroupValueCount := DataList.FieldAsInteger('GroupValueCount');
      RowCount := DataList.FieldAsInteger('RowCount');

      if Enabled then
      begin
        FirstRow.GroupValueCount2 := FirstRow.GroupValueCount2 + RowCount;
        if GroupValueCount > FirstRow.MaxGroupValueCount2 then
          FirstRow.MaxGroupValueCount2 := GroupValueCount;
        if RowCount > FirstRow.MaxGroupValueCountRowCount2 then
        begin
          FirstRow.MaxGroupValueCountRowCount2 := RowCount;
          FirstRow.MaxRowCountGroupValueCount2 := GroupValueCount;
        end;
      end
      else
      begin
        FirstRow.GroupValueCount := FirstRow.GroupValueCount + RowCount;
        if GroupValueCount > FirstRow.MaxGroupValueCount then
          FirstRow.MaxGroupValueCount := GroupValueCount;
        if RowCount > FirstRow.MaxGroupValueCountRowCount then
        begin
          FirstRow.MaxGroupValueCountRowCount := RowCount;
          FirstRow.MaxRowCountGroupValueCount := GroupValueCount;
        end;
      end;
    end;
  finally
    DataList.Free;
  end;
end;

constructor TDataComputer.Create;
var
  v: Variant;
begin
  inherited Create;
  fDatabaseFileName := TPath.GetDirectoryName(ParamStr(0)) + '\Data';
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
        raise Exception.Create('��������Ч');
      if (fFirstRangeValue < 0) or (fFirstRangeValue > fMaxValue) then
        raise Exception.Create('��һ����������Ч');
      if fFirstRangeValue = fMaxValue then fFirstRangeValue := 0;

      SetKeyValue('MaxValue', fMaxValue);
      SetKeyValue('FirstRangeValue', fFirstRangeValue);
    end
    else raise Exception.Create('�����г�ʼ��ʧ��');

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
          if Row.Number > fRowCount + 1 then raise Exception.Create('�к���Ч');
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
  RowCount, Number, GroupValue: Cardinal;
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
        raise Exception.Create('ֱ���Ƚϴ�����Ч');
      if fVertSameValueCount < 0 then
        raise Exception.Create('ֱ����ͬ������Ч');
      if fVertSameValueCount2 < 0 then
        raise Exception.Create('ֱ����ͬ����2��Ч');
    end;
  end;
  case fCompareMode of
    cmSlant, cmVertSlant:
    begin
      if fSlantCompareSpacing < 1 then
        raise Exception.Create('б���Ƚϴ�����Ч');
      if fSlantSameValueCount < 0 then
        raise Exception.Create('б����ͬ������Ч');
      if fSlantSameValueCount2 < 0 then
        raise Exception.Create('б����ͬ����2��Ч');
    end;
  end;
  if fCompareGroupValueCount < 1 then
    raise Exception.Create('�Ƚ���ϴ�����Ч');
  if fExportGroupValueCount < 1 then
    raise Exception.Create('������ϴ�����Ч');

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
              Row.FillRow(i2, Row2);
              Number := i - i2;
              CompareData.CompareRows[0].RowNumber := Row2.Number;
              //ֱ��
              case fCompareMode of
                cmVert, cmVertSlant:
                begin
                  if i - i2 <= fVertCompareSpacing then
                  begin
                    if VertCompareRow(Row, Row2, CompareData.fCompareRows)
                    then
                    begin
                      if fCompareMode = cmVert then GroupValue := Number
                      else GroupValue := Number * 3 - 2;
                      CompareData.GroupValue := 0;
                      CompareData.GroupValue.AddValue(GroupValue);
                      CompareData.GroupValue2 := GroupValue.ToString;
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
              //б��
              case fCompareMode of
                cmSlant, cmVertSlant:
                begin
                  if i - i2 <= fSlantCompareSpacing then
                  begin
                    //��б��
                    if SlantCompareRow(Row, Row2, CompareData, Number, CompareData.fCompareRows)
                    then
                    begin
                      if fCompareMode = cmSlant then GroupValue := Number * 2 - 1
                      else GroupValue := Number * 3 - 1;
                      CompareData.GroupValue := 0;
                      CompareData.GroupValue.AddValue(GroupValue);
                      CompareData.GroupValue2 := GroupValue.ToString;
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
                    //��б��
                    if SlantCompareRow(Row, Row2, CompareData, -Number, CompareData.fCompareRows)
                    then
                    begin
                      if fCompareMode = cmSlant then GroupValue := Number * 2
                      else GroupValue := Number * 3;
                      CompareData.GroupValue := 0;
                      CompareData.GroupValue.AddValue(GroupValue);
                      CompareData.GroupValue2 := GroupValue.ToString;
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
                CompareData.GroupValue2 := '';
                CompareData.GroupValueCount := 0;
                CompareData.ClearValue;
                SetLength(CompareData.fCompareRows, Length(RowNoArray));
                for i := Low(RowNoArray) to High(RowNoArray) do
                begin
                  CompareData.FillRow(RowNoArray[i], CompareData2);
                  CompareData.GroupValue := CompareData.GroupValue or CompareData2.GroupValue;
                  if CompareData.GroupValue2 <> '' then
                    CompareData.GroupValue2 := CompareData.GroupValue2 + '.';
                  CompareData.GroupValue2 := CompareData.GroupValue2 + CompareData2.GroupValue2;
                  CompareData.GroupValueCount := CompareData.GroupValueCount + 1;
                  CompareData.AddValues(CompareData2);

                  CompareData.CompareRows[i] := CompareData2.CompareRows[0];
                end;
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
                cmVert, cmVertSlant: CalcFirstRowGroup(FirstRow);
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
                Group.Value3 := CompareData.GroupValue2;
                Group.Value2 := Group.Value3;
                Group.ValueCount := CompareData.GroupValueCount;
                Group.Size := Group.Value.ToBinaryString;
                Group.RowCount := 1;
                //Group.MaxRowSpacing := CompareData.FirstRow - 1;
                Group.MaxRowSpacing := CompareData.RowSpacing;
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
          //��������1���о����
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
          //����������о�
          case fCompareMode of
            cmVert, cmVertSlant:
            begin
              if fRowCount = Row.Number then
              begin
                fDatabase.TransactionBegin(TSQLCompareGroup);
                try
                  Group.FillPrepare(fDatabase, 'MaxRowSpacing < ? - LastFirstRow', [fRowCount + 1]);
                  while Group.FillOne do
                  begin
                    Group.MaxRowSpacing := fRowCount + 1 - Group.LastFirstRow;
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
  fr: TFileWriter;
  s, FileName: string;
begin
  FileName := TPath.GetDirectoryName(ParamStr(0)) + '\��.txt';
  if TFile.Exists(FileName) then TFile.Delete(FileName);
  fr := TFileWriter.Create(FileName);
  try
    TSQLRow.AutoFree(Row, fDatabase, 'ORDER BY Number', []);
    while Row.FillOne do
    begin
      s := Format('%d=%s', [Row.Number, DataToString(Row.Values)]);
      fr.WriteLn(s);
    end;
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.ReCalc;
var
  FirstRow, FirstRow2: TSQLFirstRow;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
  DataRowCount, LastEnabledFirstRow: Cardinal;
  RecalcRowSpacing: Boolean;
  LastNotEnabledFirstRowID: TID;
begin
  TSQLFirstRow.AutoFree(FirstRow, fDatabase, '', []);
  TSQLFirstRow.AutoFree(FirstRow2);
  TSQLCompareGroup.AutoFree(Group);
  TSQLCompareData.AutoFree(Data);
  fDatabase.TransactionBegin(TSQLFirstRow);
  try
    LastEnabledFirstRow := 0;
    RecalcRowSpacing := False;
    while FirstRow.FillOne do
    begin
      //�����������ϵĴ��Ų�����
      Data.FillPrepare(fDatabase, 'FirstRow = ?', [FirstRow.Value]);
      DataRowCount := Data.FillTable.RowCount;
      while Data.FillOne do
      begin
        Data.Enabled := False;
        Group.FillPrepare(fDatabase, 'Value = ?', [Data.GroupValue]);
        if Group.FillOne then
          Data.Enabled := Group.RowCount >= fMinGroupRowCount;
        if not Data.Enabled then DataRowCount := DataRowCount - 1;
        fDatabase.Update(Data);
      end;
      //û�д��ŵ����в���ʾ
      FirstRow.RowSpacing2 := FirstRow.RowSpacing;
      FirstRow.RowCount2 := DataRowCount;
      FirstRow.Enabled := DataRowCount > 0;
      if FirstRow.Enabled then
      begin
        //�������о�
        if RecalcRowSpacing then
        begin
          if LastEnabledFirstRow > 0 then
            FirstRow.RowSpacing2 := FirstRow.Value - FirstRow2.Value
          else
            FirstRow.RowSpacing2 := FirstRow.Value - fVertSlantCompareSpacing - 1;
          //������о������С���о࣬��һ����ʾ���лָ���ʾ
          if (fKeepMaxRowSpacing > 0) and (FirstRow.RowSpacing2 > fKeepMaxRowSpacing) then
          begin
            FirstRow.RowSpacing2 := FirstRow.RowSpacing;
            if FirstRow.FillRow(FirstRow.FillTable.RowFromID(LastNotEnabledFirstRowID), FirstRow2) then;
            begin
              FirstRow2.Enabled := True;
              fDatabase.Update(FirstRow2);
            end;
          end;
        end;
        LastEnabledFirstRow := FirstRow.Value;
      end
      else
      begin
        if FirstRow.FillCurrentRow > FirstRow.FillTable.RowCount then
        begin
          //���������в���ʾ������һ�������о������С���о࣬������лָ���ʾ
          if (fKeepMaxRowSpacing > 0)
            and (LastEnabledFirstRow > 0)
            and (fRowCount + 1 - LastEnabledFirstRow > fKeepMaxRowSpacing)
          then
          begin
            FirstRow.RowSpacing2 := FirstRow.RowSpacing;
            FirstRow.Enabled := True;
          end;
        end
        else
        begin
          LastNotEnabledFirstRowID :=  FirstRow.IDValue;
          RecalcRowSpacing := True;
        end;
      end;

      fDatabase.Update(FirstRow);
    end;
    fDatabase.Commit(1, True);
  except
    on e: Exception do
    begin
      fDatabase.RollBack;
      raise Exception.Create(e.Message);
    end;
  end;
  //����������
  fDatabase.TransactionBegin(TSQLFirstRow);
  try
    FirstRow.FillPrepare(fDatabase, 'Enabled = 1', []);
    while FirstRow.FillOne do
    begin
      if FirstRow.RowCount <> FirstRow.RowCount2 then
      begin
        FirstRow.GroupValueCount2 := 0;
        FirstRow.MaxGroupValueCount2 := 0;
        FirstRow.MaxRowCountGroupValueCount2 := 0;
        FirstRow.MaxGroupValueCountRowCount2 := 0;
        CalcFirstRowGroup(FirstRow, True);
        fDatabase.Update(FirstRow);
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
      s := s.Substring(0, s.IndexOf('��ͬ����')) + '��';
      RowSpacingList.Values[sRowSpacing] := s;
    end;
  end;

var
  fr: TFileWriter;
  s, FileName, TxtFileName, sRowSpacing: string;
  i: Integer;
  FirstRow: TSQLFirstRow;
  Data: TSQLCompareData;
begin
  TxtFileName := '��1��.�����С���%d�������ϡ� [ ��ͬ���ڡ�N����Ϊ���У�] ������ͬ���ţ��������.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  if TFile.Exists(FileName) then TFile.Delete(FileName);
  fr := TFileWriter.Create(FileName);
  try
    TSQLFirstRow.AutoFree(FirstRow, fDatabase, 'ORDER BY Value DESC', []);
    TSQLCompareData.AutoFree(Data);
    RowSpacingList := TStringList.Create;
    try
      while FirstRow.FillOne do
      begin
        if FirstRow.FillCurrentRow = 2 then
        begin
          s := '1.����%d��Ϊ���У������о� ��%d��ͬ������%d��';
          s := Format(s, [fRowCount + 1, fRowCount + 1 - FirstRow.Value, FirstRow.RowCount]);

          AddRowSpacing(fRowCount + 1 - FirstRow.Value, s);
        end;
        s := '%d.����%d��Ϊ���У������о� ��%d��ͬ������%d��';
        if fMinGroupRowCount > 0 then
          s := Format(s, [FirstRow.FillCurrentRow, FirstRow.Value, FirstRow.RowSpacing2, FirstRow.RowCount2])
        else
          s := Format(s, [FirstRow.FillCurrentRow, FirstRow.Value, FirstRow.RowSpacing, FirstRow.RowCount]);
        AddRowSpacing(FirstRow.RowSpacing, s);
      end;

      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn('���-С�����о� 1-100 ���ڣ�');
      RowSpacingList.Sort;
      for i := RowSpacingList.Count - 1 downto RowSpacingList.Count - 100 do
      begin
        if i < 0 then Break;
        fr.WriteLn('');
        fr.WriteLn(RowSpacingList.ValueFromIndex[i]);
      end;
    finally
      RowSpacingList.Free;
    end;

    FirstRow.FillRewind;
    while FirstRow.FillOne do
    begin
      if FirstRow.FillCurrentRow = 2 then
      begin
        s := '1.����%d��Ϊ���У��������о� ��%d��';
        s := Format(s, [fRowCount + 1, fRowCount + 1 - FirstRow.Value]);
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn(s);
      end;

      s := '%d.����%d��Ϊ���У������о� ��%d��ͬ������%d��';
      if fMinGroupRowCount > 0 then
        s := Format(s, [FirstRow.FillCurrentRow, FirstRow.Value, FirstRow.RowSpacing2, FirstRow.RowCount2])
      else
        s := Format(s, [FirstRow.FillCurrentRow, FirstRow.Value, FirstRow.RowSpacing, FirstRow.RowCount]);
      if FirstRow.FillCurrentRow > 2 then
      begin
        fr.WriteLn('');
        fr.WriteLn('');
      end;
      fr.WriteLn('');
      fr.WriteLn(s);

      s := 'FirstRow = ?';
      if fMinGroupRowCount > 0 then s := s + ' AND Enabled = 1';
      Data.FillPrepare(fDatabase, s, [FirstRow.Value]);
      while Data.FillOne do
      begin
        s := Format('��%d��.[���ţ�����%s����%s ]', [
          Data.FillCurrentRow - 1,
          CompareTypeToString(Data.GroupValue),
          CompareTypeToString(Data.GroupValue, 1)
        ]);
        if fFirstRangeValue = 0 then
          s := s + Format(' = �ޡ���Ӧ�С����� %d�� ��', [Data.TotalValueCount])
        else
          s := s + Format(' = �� %d-%d ���� ��', [Data.ValueCount, Data.TotalValueCount - Data.ValueCount]);
        s := s + Format('�������֡���%s', [DataToString(Data.Values)]);

        fr.WriteLn('');
        fr.WriteLn(s);
      end;
    end;
    s := '%d.����%d��Ϊ��ĩ���У�';
    s := Format(s, [
      FirstRow.FillTable.RowCount + 2,
      fSlantCompareSpacing + 1
    ]);
    fr.WriteLn('');
    fr.WriteLn(s);

    fr.CloseFile;
    for s in fr.Files do
      if TFile.Exists(s) then TFile.Copy(s, fExportDirectory + TPath.GetFileName(s));
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.SaveGroupByCompareTypeSortByRowcount;
var
  fr, fr2, fr3, fr4: TFileWriter;
  s, sCompareType, sCompareType2, FileName, FileName2, FileName3, FileName4, TxtFileName: string;
  Group, Group2: TSQLCompareGroup;
  Data: TSQLCompareData;
  RowNo, MaxRowCount, RowCountNumber: Cardinal;
begin
  TxtFileName := '��2��.�����С���%d�������ϸ�����ϣ���ͬ���š���ͬ���У��ģ���ͬ����������� - �����У�.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  TxtFileName := '��2��.�����С���%d�������ϸ�����ϣ���ͬ���š���ͬ���У��ģ���ͬ����������� - �����У���2��.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName2 := fExportDirectory + TxtFileName;
  TxtFileName := '��2��.�����С���%d�������ϸ�����ϣ���ͬ���š���ͬ���У��ģ���ͬ����������� - �����У���3��.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName3 := fExportDirectory + TxtFileName;
  TxtFileName := '��2��.�����С���%d�������ϸ�����ϣ���ͬ���š���ͬ���У��ģ���ͬ����������� - �����У���1��-���򻯲�ͳ�ƣ�.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName4 := fExportDirectory + TxtFileName;
  fr := TFileWriter.Create(FileName);
  fr2 := TFileWriter.Create(FileName2);
  fr3 := TFileWriter.Create(FileName3);
  fr4 := TFileWriter.Create(FileName4);
  try
    RowNo := 0;
    TSQLCompareGroup.AutoFree(Group, fDatabase,
      'RowCount >= ? ORDER BY RowCount DESC, ValueCount, Size DESC',
      [fMinGroupRowCount]);
    TSQLCompareGroup.AutoFree(Group2);
    TSQLCompareData.AutoFree(Data);
    while Group.FillOne do
    begin
      sCompareType := CompareTypeToString(Group.Value);
      sCompareType2 := CompareTypeToString(Group.Value, 1);

      s := '%d.[���ţ�����%s����%s ] ������ͬ��������%d�У�';
      s := Format(s, [Group.FillCurrentRow - 1, sCompareType, sCompareType2, Group.RowCount]);
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);
      fr2.WriteLn('');
      fr2.WriteLn('');
      fr2.WriteLn(s);
      fr2.WriteLn('');
      RowCountNumber := RowCountNumber + 1;
      if Group.FillCurrentRow = 2 then MaxRowCount := Group.RowCount;
      if Group.FillCurrentRow <= Group.FillTable.RowCount then
        Group.FillRow(Group.FillCurrentRow, Group2);
      if (Group.FillCurrentRow > Group.FillTable.RowCount) or (Group.RowCount <> Group2.RowCount) then
      begin
        fr4.WriteLn('');
        fr4.WriteLn('');
        fr4.WriteLn('');
        fr4.WriteLn(s);
        s := 'ͳ�ƣ�[ ��ͬ��б������������%d ] ���ܹ� ��%d �� ��';
        s := Format(s, [Group.RowCount, RowCountNumber]);
        if Group.RowCount <> MaxRowCount then
        begin
          s := s + '[ ��ͬ��б������������%d-%d ] ���ܹ� ��%d �� ��';
          s := Format(s, [MaxRowCount, Group.RowCount, Group.FillCurrentRow - 1]);
        end;
        fr4.WriteLn('');
        fr4.WriteLn(s);
        RowCountNumber := 0;
      end;

      Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
      while Data.FillOne do
      begin
        if Data.FillCurrentRow = 2 then
        begin
          RowNo := RowNo + 1;
          s := '1=[����%s����%s ] ����%d����%d��';
          s := Format(s, [sCompareType, sCompareType2, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
          fr2.WriteLn(s);
          s := '%d=[����%s����%s ] ����%d����%d��';
          s := Format(s, [RowNo, sCompareType, sCompareType2, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
          fr3.WriteLn(s);
        end;

        s := Format('��%d������%d��Ϊ���У�', [Data.FillCurrentRow - 1, Data.FirstRow]);
        if fFirstRangeValue = 0 then
          s := s + Format('= �ޡ���Ӧ�С����� %d�� ��', [Data.TotalValueCount])
        else
          s := s + Format('= �ޡ���Ӧ�С�����%d - %d �� ��', [Data.ValueCount, Data.TotalValueCount - Data.ValueCount]);
        s := s + Format('�������֡���%s', [DataToString(Data.Values)]);
        fr.WriteLn('');
        fr.WriteLn(s);

        s := '%d=[����%s����%s ] ����%d����%d��';
        s := Format(s, [Data.FillCurrentRow, sCompareType, sCompareType2, Data.FirstRow, Data.RowSpacing]);
        fr2.WriteLn(s);
        RowNo := RowNo + 1;
        s := '%d=[����%s����%s ] ����%d����%d��';
        s := Format(s, [RowNo, sCompareType, sCompareType2, Data.FirstRow, Data.RowSpacing]);
        fr3.WriteLn(s);

        if Data.FillCurrentRow > Data.FillTable.RowCount then
        begin
          RowNo := RowNo + 1;
          s := '%d=[����%s����%s ] ����%d����0��';
          s := Format(s, [Data.FillCurrentRow + 1, sCompareType, sCompareType2, fSlantCompareSpacing + 1]);
          fr2.WriteLn(s);
          s := '%d=[����%s����%s ] ����%d����0��';
          s := Format(s, [RowNo, sCompareType, sCompareType2, fSlantCompareSpacing + 1]);
          fr3.WriteLn(s);
        end;
      end;
    end;

    fr.CloseFile;
    for s in fr.Files do
    begin
      TxtFileName := TPath.GetFileName(s).Replace('�����У�', '�����У���1��');
      if TFile.Exists(s) then TFile.Copy(s, fExportDirectory + TxtFileName);
    end;
  finally
    fr.Free;
    fr2.Free;
    fr3.Free;
    fr4.Free;
  end;
end;

procedure TDataComputer.SaveGroupByCompareTypeSortByMaxRowSpacing;
var
  fr: TFileWriter;
  s, FileName, TxtFileName: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
begin
  TxtFileName := '��3��.�����С���%d�������ϸ�����ϣ���ͬ���š���ͬ���У��ģ����оࣺ������N��- ��С����N����.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  if TFile.Exists(FileName) then TFile.Delete(FileName);
  fr := TFileWriter.Create(FileName);
  try
    TSQLCompareGroup.AutoFree(Group, fDatabase,
      'RowCount >= ? ORDER BY MaxRowSpacing DESC, ValueCount, Size DESC',
      [fMinGroupRowCount]);
    TSQLCompareData.AutoFree(Data);
    while Group.FillOne do
    begin
      s := '%d.[���ţ�����%s����%s ] ����������о� ��%d��';
      s := Format(s, [
        Group.FillCurrentRow - 1,
        CompareTypeToString(Group.Value),
        CompareTypeToString(Group.Value, 1),
        Group.MaxRowSpacing
      ]);
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);

      Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
      while Data.FillOne do
      begin
        if Data.FillCurrentRow = 2 then
        begin
          s := '��1������%d��Ϊ���У��������о� ��%d��';
          s := Format(s, [fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
          fr.WriteLn('');
          fr.WriteLn(s);
        end;

        s := Format('��%d������%d��Ϊ���У������о� ��%d��', [Data.FillCurrentRow, Data.FirstRow, Data.RowSpacing]);
        if fFirstRangeValue = 0 then
          s := s + Format('= �ޡ���Ӧ�С����� %d�� ��', [Data.TotalValueCount])
        else
          s := s + Format('= �ޡ���Ӧ�С�����%d - %d �� ��', [Data.ValueCount, Data.TotalValueCount - Data.ValueCount]);
        s := s + Format('�������֡���%s', [DataToString(Data.Values)]);
        fr.WriteLn('');
        fr.WriteLn(s);

        if Data.FillCurrentRow > Data.FillTable.RowCount then
        begin
          s := '��%d������%d��Ϊ��ĩ���У�';
          s := Format(s, [Data.FillCurrentRow + 1, fSlantCompareSpacing + 1]);
          fr.WriteLn('');
          fr.WriteLn(s);
        end;
      end;
    end;
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.SaveGroupByCompareTypeSortByCompareTypeCount;
var
  fr: TFileWriter;
  s, FileName, TxtFileName: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
begin
  TxtFileName := '��4��.�����С���%d�������ϸ�����ϣ���ͬ���š���ͬ���У��ģ����������� - ���ٸ���.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  if TFile.Exists(FileName) then TFile.Delete(FileName);
  fr := TFileWriter.Create(FileName);
  try
    TSQLCompareGroup.AutoFree(Group, fDatabase,
      'RowCount >= ? ORDER BY ValueCount DESC, Size DESC',
      [fMinGroupRowCount]);
    TSQLCompareData.AutoFree(Data);
    while Group.FillOne do
    begin
      s := '%d.[���ţ�����%s����%s ] �����������%d����';
      s := Format(s, [
        Group.FillCurrentRow - 1,
        CompareTypeToString(Group.Value),
        CompareTypeToString(Group.Value, 1),
        Group.ValueCount
      ]);
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);

      Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
      while Data.FillOne do
      begin
        s := Format('��%d������%d��Ϊ���У�', [Data.FillCurrentRow - 1, Data.FirstRow]);
        if fFirstRangeValue = 0 then
          s := s + Format('= �ޡ���Ӧ�С����� %d�� ��', [Data.TotalValueCount])
        else
          s := s + Format('= �ޡ���Ӧ�С�����%d - %d �� ��', [Data.ValueCount, Data.TotalValueCount - Data.ValueCount]);
        s := s + Format('�������֡���%s', [DataToString(Data.Values)]);
        fr.WriteLn('');
        fr.WriteLn(s);
      end;
    end;
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.SaveGroupByCompareTypeSortByMaxValueCount;
var
  fr: TFileWriter;
  s, FileName, TxtFileName: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
begin
  TxtFileName := '��5��.�����С���%d�������ϸ�����ϣ���ͬ���š���ͬ���У��ģ��ޡ���Ӧ�С�������� - �����У�.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  if TFile.Exists(FileName) then TFile.Delete(FileName);
  fr := TFileWriter.Create(FileName);
  try
    TSQLCompareGroup.AutoFree(Group, fDatabase,
      'RowCount >= ? ORDER BY MaxTotalValueCount DESC, MaxValueCount DESC, ValueCount, Size DESC',
      [fMinGroupRowCount]);
    TSQLCompareData.AutoFree(Data);
    while Group.FillOne do
    begin
      s := '%d.[���ţ�����%s����%s ] ��';
      s := Format(s, [
        Group.FillCurrentRow - 1,
        CompareTypeToString(Group.Value),
        CompareTypeToString(Group.Value, 1)
      ]);
      if fFirstRangeValue = 0 then
        s := s + Format('�� ��� [ �ޡ���Ӧ�С�����%d�� ] ��', [Group.MaxTotalValueCount])
      else
        s := s + Format('�� ��� [ �ޡ���Ӧ�С�����%d-%d�� ] ��', [Group.MaxValueCount, Group.MaxTotalValueCount - Group.MaxValueCount]);
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);

      Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
      while Data.FillOne do
      begin
        s := Format('��%d������%d��Ϊ���У�', [Data.FillCurrentRow - 1, Data.FirstRow]);
        if fFirstRangeValue = 0 then
          s := s + Format('= �ޡ���Ӧ�С����� %d�� ��', [Data.TotalValueCount])
        else
          s := s + Format('= �ޡ���Ӧ�С�����%d - %d �� ��', [Data.ValueCount, Data.TotalValueCount - Data.ValueCount]);
        s := s + Format('�������֡���%s', [DataToString(Data.Values)]);
        fr.WriteLn('');
        fr.WriteLn(s);
      end;
    end;
  finally
    fr.Free;
  end
end;

procedure TDataComputer.SaveGroupByCompareTypeSortByLastFirstRow;
var
  fr: TFileWriter;
  s, sCompareType, sCompareType2, FileName, TxtFileName: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
  RowNo: Cardinal;
begin
  TxtFileName := '��2��.�����С���%d�������ϸ�����ϣ���ͬ���š���ͬ���У��ģ����оࣺ��� �� С����4��.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory + TxtFileName;
  if TFile.Exists(FileName) then TFile.Delete(FileName);
  fr := TFileWriter.Create(FileName);
  try
    RowNo := 0;
    TSQLCompareGroup.AutoFree(Group, fDatabase,
      'RowCount >= ? ORDER BY LastFirstRow, ValueCount, Size DESC',
      [fMinGroupRowCount]);
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
          s := '%d=[����%s����%s ] ����%d����%d��';
          s := Format(s, [RowNo, sCompareType, sCompareType2, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
          fr.WriteLn(s);
        end;

        RowNo := RowNo + 1;
        s := '%d=[����%s����%s ] ����%d����%d��';
        s := Format(s, [RowNo, sCompareType, sCompareType2, Data.FirstRow, Data.RowSpacing]);
        fr.WriteLn(s);

        if Data.FillCurrentRow > Data.FillTable.RowCount then
        begin
          RowNo := RowNo + 1;
          s := '%d=[����%s����%s ] ����%d����0��';
          s := Format(s, [RowNo, sCompareType, sCompareType2, fSlantCompareSpacing + 1]);
          fr.WriteLn(s);
        end;
      end;
    end;
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.SaveGroupByCompareTypeSortByOneRowSpacingCount;
var
  fr: TFileWriter;
  s, sCompareType, sCompareType2, FileName, TxtFileName: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
  RowNo: Cardinal;
begin
  TxtFileName := '��2��.�����С���%d�������ϸ�����ϣ���ͬ���š���ͬ���У��ģ����оࣺ��С �� �󣩣�5��.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory + TxtFileName;
  if TFile.Exists(FileName) then TFile.Delete(FileName);
  fr := TFileWriter.Create(FileName);
  try
    RowNo := 0;
    TSQLCompareGroup.AutoFree(Group, fDatabase,
      'RowCount >= ? ORDER BY OneRowSpacingCount DESC, LastFirstRow DESC, ValueCount, Size DESC',
      [fMinGroupRowCount]);
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
          s := '%d=[����%s����%s ] ����%d����%d��';
          s := Format(s, [RowNo, sCompareType, sCompareType2, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
          fr.WriteLn(s);
        end;

        RowNo := RowNo + 1;
        s := '%d=[����%s����%s ] ����%d����%d��';
        s := Format(s, [RowNo, sCompareType, sCompareType2, Data.FirstRow, Data.RowSpacing]);
        fr.WriteLn(s);

        if Data.FillCurrentRow > Data.FillTable.RowCount then
        begin
          RowNo := RowNo + 1;
          s := '%d=[����%s����%s ] ����%d����0��';
          s := Format(s, [RowNo, sCompareType, sCompareType2, fSlantCompareSpacing + 1]);
          fr.WriteLn(s);
        end;
      end;
    end;
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.SaveCompareType;
var
  fr: TFileWriter;
  s, FileName, TxtFileName: string;
  i, j: Integer;
  Group: TSQLCompareGroup;
begin
  TxtFileName := '��6��.�����桿��%d�������ϸ�����ϣ���ͬ���š���ͬ���У��ģ����ţ���N��ZY ��.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  if TFile.Exists(FileName) then TFile.Delete(FileName);
  fr := TFileWriter.Create(FileName);
  try
    TSQLCompareGroup.AutoFree(Group, fDatabase,
      'RowCount >= ? ORDER BY ValueCount, Size DESC',
      [fMinGroupRowCount]);
    while Group.FillOne do
    begin
      s := Format('%d=%s', [Group.FillCurrentRow - 1, CompareTypeToString(Group.Value, 1)]);
      fr.WriteLn(s);
    end;
  finally
    fr.Free;
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
  fr: TFileWriter;
  s, FileName, TxtFileName, sRowSpacing, sCompareTypeValueCount: string;
  i: Integer;
  FirstRow: TSQLFirstRow;
  Data: TSQLCompareData;
  LastGroupValueCount: Byte;
  GroupNumber: Word;
  RowSpacing: Cardinal;
  GroupValueCount: Word;
  MaxGroupValueCount: Byte;
  MaxRowCountGroupValueCount: Byte;
  MaxGroupValueCountRowCount: Cardinal;
begin
  TxtFileName := '��1��.�����С�����%d��������[��ͬ���С���ͬ���]����ϡ�.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  fr := TFileWriter.Create(FileName);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    fr.WriteLn('');
    fr.WriteLn('');
    fr.WriteLn(s);

    TSQLFirstRow.AutoFree(FirstRow, fDatabase, 'ORDER BY Value DESC', []);
    TSQLCompareData.AutoFree(Data);
    RowSpacingList := TStringList.Create;
    try
      while FirstRow.FillOne do
      begin
        if FirstRow.FillCurrentRow = 2 then
        begin
          s := '1.[ ֱ������%d��Ϊ���У�]��[ ���о� ��%d ]';
          s := Format(s, [fRowCount + 1, fRowCount + 1 - FirstRow.Value]);

          AddRowSpacing(fRowCount + 1 - FirstRow.Value, s);
        end;
        RowSpacing := FirstRow.RowSpacing;
        if fMinGroupRowCount > 0 then RowSpacing := FirstRow.RowSpacing2;
        s := '%d.[ ֱ������%d��Ϊ���У�]��[ ���о� ��%d ]';
        s := Format(s, [FirstRow.FillCurrentRow, FirstRow.Value, RowSpacing]);
        AddRowSpacing(FirstRow.RowSpacing, s);
      end;

      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn('���-С�����о� 1-100 ���ڣ�');
      RowSpacingList.Sort;
      for i := RowSpacingList.Count - 1 downto RowSpacingList.Count - 100 do
      begin
        if i < 0 then Break;
        fr.WriteLn('');
        fr.WriteLn(RowSpacingList.ValueFromIndex[i]);
      end;
    finally
      RowSpacingList.Free;
    end;

    FirstRow.FillRewind;
    while FirstRow.FillOne do
    begin
      if FirstRow.FillCurrentRow = 2 then
      begin
        s := '1.[ ֱ������%d��Ϊ���У�]��[ ���о� ��%d ]';
        s := Format(s, [fRowCount + 1, fRowCount + 1 - FirstRow.Value]);
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn(s);
      end;

      RowSpacing := FirstRow.RowSpacing;
      GroupValueCount := FirstRow.GroupValueCount;
      MaxGroupValueCount := FirstRow.MaxGroupValueCount;
      MaxRowCountGroupValueCount := FirstRow.MaxRowCountGroupValueCount;
      MaxGroupValueCountRowCount := FirstRow.MaxGroupValueCountRowCount;
      if fMinGroupRowCount > 0 then
      begin
        RowSpacing := FirstRow.RowSpacing2;
        GroupValueCount := FirstRow.GroupValueCount2;
        MaxGroupValueCount := FirstRow.MaxGroupValueCount2;
        MaxRowCountGroupValueCount := FirstRow.MaxRowCountGroupValueCount2;
        MaxGroupValueCountRowCount := FirstRow.MaxGroupValueCountRowCount2;
      end;

      s := '%d.[ ֱ������%d��Ϊ���У�]��[ ���о� ��%d ��';
      s := Format(s, [FirstRow.FillCurrentRow, FirstRow.Value, RowSpacing]);
      if MaxGroupValueCount > fExportGroupValueCount then
      begin
        sCompareTypeValueCount := '';
        for i := fExportGroupValueCount to MaxGroupValueCount do
        begin
          if not sCompareTypeValueCount.IsEmpty then
            sCompareTypeValueCount := sCompareTypeValueCount + '��';
          sCompareTypeValueCount := sCompareTypeValueCount + i.ToString;
        end;

        s := s + Format('����%s����ϣ��ܹ� %d �� ��', [
          sCompareTypeValueCount,
          GroupValueCount
        ]);
      end;
      s := s + Format('��ࣨ��%d����ϣ����� %d �� ]��', [
        MaxRowCountGroupValueCount,
        MaxGroupValueCountRowCount
      ]);
      if FirstRow.FillCurrentRow > 2 then
      begin
        fr.WriteLn('');
        fr.WriteLn('');
      end;
      fr.WriteLn('');
      fr.WriteLn(s);

      s := 'FirstRow = ?';
      if fMinGroupRowCount > 0 then s := s + ' AND Enabled = 1';
      Data.FillPrepare(fDatabase, s, [FirstRow.Value]);
      LastGroupValueCount := 0;
      GroupNumber := 0;
      while Data.FillOne do
      begin
        if Data.GroupValueCount <> LastGroupValueCount then
        begin
          LastGroupValueCount := Data.GroupValueCount;
          GroupNumber := 0;
        end;
        GroupNumber := GroupNumber + 1;

        s := '��%d��.��%d�����[ ���ţ�%d.��%s��]��';
        s := Format(s, [
          Data.FillCurrentRow - 1,
          Data.GroupValueCount,
          GroupNumber,
          VertCompareTypeToString(Data.GroupValue)
        ]);
        fr.WriteLn('');
        fr.WriteLn(s);

        SaveCompareData(fr, Data);
      end;
    end;
    s := '%d.[����%d��Ϊ��ĩ���У�ֱ������%d-1�У�]';
    s := Format(s, [
      FirstRow.FillTable.RowCount + 2,
      fVertCompareSpacing + 1,
      fVertCompareSpacing
    ]);
    fr.WriteLn('');
    fr.WriteLn(s);

    fr.CloseFile;
    for s in fr.Files do
      if TFile.Exists(s) then TFile.Copy(s, fExportDirectory + TPath.GetFileName(s));
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.SaveVertGroupByCompareTypeSortByRowcount;
var
  fr, fr2, fr3: TFileWriter;
  s, sCompareType, sCompareType2, FileName, FileName2, FileName3, TxtFileName: string;
  Group, Group2: TSQLCompareGroup;
  Data: TSQLCompareData;
  RowNo: Cardinal;
  LastFirstRow, MaxRowCount, RowCountNumber: Cardinal;
begin
  TxtFileName := '��2��.�����С�����%d��������[��ͬ��ϡ���ͬ����]�����[��ͬ��������������]��.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  TxtFileName := '��2��.�����С�����%d��������[��ͬ��ϡ���ͬ����]�����[��ͬ��������������]����2��.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName2 := fExportDirectory + TxtFileName;
  TxtFileName := '��2��.�����С�����%d��������[��ͬ��ϡ���ͬ����]�����[��ͬ��������������]����1��-���򻯲�ͳ�ƣ�.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName3 := fExportDirectory + TxtFileName;
  fr := TFileWriter.Create(FileName);
  fr2 := TFileWriter.Create(FileName2);
  fr3 := TFileWriter.Create(FileName3);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    fr.WriteLn('');
    fr.WriteLn('');
    fr.WriteLn(s);
    s := TPath.GetFileNameWithoutExtension(FileName2);
    fr2.WriteLn('');
    fr2.WriteLn('');
    fr2.WriteLn(s);
    s := TPath.GetFileNameWithoutExtension(FileName3);
    fr3.WriteLn('');
    fr3.WriteLn('');
    fr3.WriteLn(s);

    RowNo := 0;
    TSQLCompareGroup.AutoFree(Group, fDatabase,
      'RowCount >= ? ORDER BY RowCount DESC, ValueCount, Size DESC',
      [fMinGroupRowCount]);
    TSQLCompareGroup.AutoFree(Group2);
    TSQLCompareData.AutoFree(Data);
    while Group.FillOne do
    begin
      s := '%d.����%d���� [ ��ͬ��ϡ���ͬ��ֱ��������]����� [ ��ͬ��ֱ������������%d ]����';
      s := Format(s, [
        Group.FillCurrentRow - 1,
        Group.ValueCount,
        Group.RowCount
      ]);
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);
      fr2.WriteLn('');
      fr2.WriteLn('');
      fr2.WriteLn('');
      RowCountNumber := RowCountNumber + 1;
      if Group.FillCurrentRow = 2 then MaxRowCount := Group.RowCount;
      if Group.FillCurrentRow <= Group.FillTable.RowCount then
        Group.FillRow(Group.FillCurrentRow, Group2);
      if (Group.FillCurrentRow > Group.FillTable.RowCount) or (Group.RowCount <> Group2.RowCount) then
      begin
        fr3.WriteLn('');
        fr3.WriteLn('');
        fr3.WriteLn('');
        fr3.WriteLn(s);
        s := 'ͳ�ƣ�[ ��ͬ��ֱ������������%d ] ���ܹ� ��%d �� ��';
        s := Format(s, [Group.RowCount, RowCountNumber]);
        if Group.RowCount <> MaxRowCount then
        begin
          s := s + '[ ��ͬ��ֱ������������%d-%d ] ���ܹ� ��%d �� ��';
          s := Format(s, [MaxRowCount, Group.RowCount, Group.FillCurrentRow - 1]);
        end;
        fr3.WriteLn('');
        fr3.WriteLn(s);
        RowCountNumber := 0;
      end;

      LastFirstRow := 0;
      Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
      while Data.FillOne do
      begin
        sCompareType := VertCompareTypeToString(Group.Value);

        if Data.FillCurrentRow = 2 then
        begin
          RowNo := RowNo + 1;
          s := '%d= ��%d�����[ ���ţ�3.��%s��]����%d����%d��';
          s := Format(s, [RowNo, Data.GroupValueCount, sCompareType, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
          fr2.WriteLn('');
          fr2.WriteLn(s);
        end;

        if LastFirstRow <> Data.FirstRow then
        begin
          LastFirstRow := Data.FirstRow;
          s := '��%d��.��%d�����[ ���ţ�1.��%s��]��';
          s := Format(s, [Data.FillCurrentRow - 1, Data.GroupValueCount, sCompareType]);

          fr.WriteLn('');
          fr.WriteLn(s);
        end;

        SaveCompareData(fr, Data);

        RowNo := RowNo + 1;
        s := '%d= ��%d�����[ ���ţ�1.��%s��]����%d����%d��';
        s := Format(s, [RowNo, Data.GroupValueCount, sCompareType, Data.FirstRow, Data.RowSpacing]);
        fr2.WriteLn('');
        fr2.WriteLn(s);

        SaveCompareData(fr, Data, False);

        if Data.FillCurrentRow > Data.FillTable.RowCount then
        begin
          RowNo := RowNo + 1;
          s := '%d= ��%d�����[ ���ţ�1.��%s��]����%d����0��';
          s := Format(s, [RowNo, Data.GroupValueCount, sCompareType, fVertCompareSpacing + 1]);
          fr2.WriteLn('');
          fr2.WriteLn(s);
        end;
      end;
    end;

    fr.CloseFile;
    for s in fr.Files do
    begin
      TxtFileName := TPath.GetFileName(s).Replace(']��', ']����1��');
      if TFile.Exists(s) then TFile.Copy(s, fExportDirectory + TxtFileName);
    end;
  finally
    fr.Free;
    fr2.Free;
    fr3.Free;
  end;
end;

procedure TDataComputer.SaveVertGroupByCompareTypeSortByMaxRowSpacing;
var
  fr: TFileWriter;
  s, FileName, TxtFileName: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
  RowNo: Word;
begin
  TxtFileName := '��3��.�����С�����%d��������[��ͬ��ϡ���ͬ����]�����[���оࣺ������С��]��.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  fr := TFileWriter.Create(FileName);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    fr.WriteLn('');
    fr.WriteLn('');
    fr.WriteLn(s);

    TSQLCompareGroup.AutoFree(Group, fDatabase,
      'RowCount >= ? ORDER BY MaxRowSpacing DESC, ValueCount DESC, Size',
      [fMinGroupRowCount]);
    TSQLCompareData.AutoFree(Data);
    while Group.FillOne do
    begin
      RowNo := 2;
      Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
      while Data.FillOne do
      begin
        if Data.FillCurrentRow = 2 then
        begin
          s := '%d.[ ֱ������%d��Ϊ���У�] .��������о� ��%d ����';
          s := Format(s, [
            Group.FillCurrentRow - 1,
            fRowCount + 1,
            Group.MaxRowSpacing
          ]);
          fr.WriteLn('');
          fr.WriteLn('');
          fr.WriteLn('');
          fr.WriteLn(s);

          s := '��1��.[ ֱ������%d��Ϊ���У�] .�����о� ��%d ����';
          s := Format(s, [
            fRowCount + 1,
            fRowCount + 1 - Data.FirstRow
          ]);
          fr.WriteLn('');
          fr.WriteLn(s);

          s := '��2��.[ ֱ������%d��Ϊ���У�] .�����о� ��%d ����';
          s := Format(s, [Data.FirstRow, Data.FirstRow - fVertCompareSpacing - 1]);
          fr.WriteLn('');
          fr.WriteLn(s);
        end;

        s := '��%d��.��%d�����[ ���ţ�3.��%s��]��';
        s := Format(s, [
          Data.FillCurrentRow - 1,
          Data.GroupValueCount,
          VertCompareTypeToString(Group.Value)
        ]);
        fr.WriteLn('');
        fr.WriteLn(s);

        SaveCompareData(fr, Data);

        RowNo := RowNo + 1;
        if Data.FillCurrentRow > Data.FillTable.RowCount then
        begin
          s := '��%d��.[����%d��Ϊ��ĩ���У�ֱ������%d-1�У�]';
          s := Format(s, [RowNo, fVertCompareSpacing + 1, fVertCompareSpacing]);
          fr.WriteLn('');
          fr.WriteLn(s);
        end
        else
        begin
          s := '��%d��.[ ֱ������%d��Ϊ���У�] .�����о� ��0 ����';
          s := Format(s, [RowNo, fVertCompareSpacing + 1]);
          fr.WriteLn('');
          fr.WriteLn(s);
        end;
      end;
    end;
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.SaveVertGroupByCompareTypeSortByCompareTypeCount;
var
  fr: TFileWriter;
  s, FileName, TxtFileName: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
begin
  TxtFileName := '��4��.�����С�����%d��������[��ͬ��ϡ���ͬ����]�����[������������ٸ�]��.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  fr := TFileWriter.Create(FileName);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    fr.WriteLn('');
    fr.WriteLn('');
    fr.WriteLn(s);

    TSQLCompareGroup.AutoFree(Group, fDatabase,
      'RowCount >= ? ORDER BY ValueCount DESC, Size DESC',
      [fMinGroupRowCount]);
    TSQLCompareData.AutoFree(Data);
    while Group.FillOne do
    begin
      s := '%d.[ ��%d����ϣ����ţ�1.��%s��]�����������%d������';
      s := Format(s, [
        Group.FillCurrentRow - 1,
        Group.ValueCount,
        VertCompareTypeToString(Group.Value),
        Group.ValueCount
      ]);
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);

      Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
      while Data.FillOne do
      begin
        if Data.FillCurrentRow > 2 then fr.WriteLn('');
        SaveCompareData(fr, Data);
      end;
    end;
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.SaveVertGroupByCompareTypeSortByMaxValueCount;
var
  fr: TFileWriter;
  s, FileName, TxtFileName: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
begin
  TxtFileName := '��5��.�����С�����%d��������[��ͬ��ϡ���ͬ����]�����[�ޡ���Ӧ�С�������������]��.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  fr := TFileWriter.Create(FileName);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    fr.WriteLn('');
    fr.WriteLn('');
    fr.WriteLn(s);

    TSQLCompareGroup.AutoFree(Group, fDatabase,
      'RowCount >= ? ORDER BY MaxValueCount DESC, MaxTotalValueCount - MaxValueCount DESC, ValueCount, Size DESC',
      [fMinGroupRowCount]);
    TSQLCompareData.AutoFree(Data);
    while Group.FillOne do
    begin
      s := '%d.[ ��%d����ϣ����ţ�1.��%s��]���� w�ޡ���Ӧ�С�������';
      s := Format(s, [Group.FillCurrentRow - 1, Group.ValueCount, VertCompareTypeToString(Group.Value)]);
      if fFirstRangeValue = 0 then
        s := s + Format('%d��', [Group.MaxTotalValueCount])
      else
        s := s + Format('%d-%d��', [Group.MaxValueCount, Group.MaxTotalValueCount - Group.MaxValueCount]);
      s := s + '����';
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);

      Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
      while Data.FillOne do
      begin
        if Data.FillCurrentRow > 2 then fr.WriteLn('');
        SaveCompareData(fr, Data);
      end;
    end;
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.SaveVertGroupByCompareTypeSortByLastFirstRow;
var
  fr: TFileWriter;
  s, FileName, TxtFileName, sCompareType: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
  RowNo: Word;
begin
  TxtFileName := '��2��.�����С�����%d��������[��ͬ��ϡ���ͬ����]����� [�������оࣺ������С��]����3��.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory + TxtFileName;
  fr := TFileWriter.Create(FileName);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    fr.WriteLn('');
    fr.WriteLn('');
    fr.WriteLn(s);

    RowNo := 0;

    TSQLCompareGroup.AutoFree(Group, fDatabase,
      'RowCount >= ? ORDER BY LastFirstRow, ValueCount DESC, Size',
      [fMinGroupRowCount]);
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
          s := '%d= ��%d�����[ ���ţ�1.��%s��]����%d����%d��	';
          s := Format(s, [RowNo, Data.GroupValueCount, sCompareType, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
          fr.WriteLn('');
          fr.WriteLn('');
          fr.WriteLn('');
          fr.WriteLn(s);
        end;

        RowNo := RowNo + 1;
        s := '%d= ��%d�����[ ���ţ�1.��%s��]����%d����%d��	';
        s := Format(s, [RowNo, Data.GroupValueCount, sCompareType, Data.FirstRow, Data.RowSpacing]);
        fr.WriteLn('');
        fr.WriteLn(s);

        SaveCompareData(fr, Data, False);

        if Data.FillCurrentRow > Data.FillTable.RowCount then
        begin
          RowNo := RowNo + 1;
          s := '%d= ��%d�����[ ���ţ�1.��%s��]����%d����0��	';
          s := Format(s, [RowNo, Data.GroupValueCount, sCompareType, fVertCompareSpacing + 1]);
          fr.WriteLn('');
          fr.WriteLn(s);
        end;
      end;
    end;
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.SaveVertGroupByCompareTypeSortByLastFirstRow2;
var
  fr: TFileWriter;
  s, FileName, TxtFileName, sCompareType: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
  RowNo: Word;
begin
  TxtFileName := '��2��.�����С�����%d��������[��ͬ��ϡ���ͬ����]����� [�������оࣺ��С�������]����4��.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory + TxtFileName;
  fr := TFileWriter.Create(FileName);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    fr.WriteLn('');
    fr.WriteLn('');
    fr.WriteLn(s);

    RowNo := 0;

    TSQLCompareGroup.AutoFree(Group, fDatabase,
      'RowCount >= ? ORDER BY LastFirstRow DESC, ValueCount, Size DESC',
      [fMinGroupRowCount]);
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
          s := '%d= ��%d�����[ ���ţ�1.��%s��]����%d����%d��	';
          s := Format(s, [RowNo, Data.GroupValueCount, sCompareType, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
          fr.WriteLn('');
          fr.WriteLn('');
          fr.WriteLn('');
          fr.WriteLn(s);
        end;

        RowNo := RowNo + 1;
        s := '%d= ��%d�����[ ���ţ�1.��%s��]����%d����%d��	';
        s := Format(s, [RowNo, Data.GroupValueCount, sCompareType, Data.FirstRow, Data.RowSpacing]);
        fr.WriteLn('');
        fr.WriteLn(s);

        SaveCompareData(fr, Data, False);

        if Data.FillCurrentRow > Data.FillTable.RowCount then
        begin
          RowNo := RowNo + 1;
          s := '%d= ��%d�����[ ���ţ�1.��%s��]����%d����0��	';
          s := Format(s, [RowNo, Data.GroupValueCount, sCompareType, fVertCompareSpacing + 1]);
          fr.WriteLn('');
          fr.WriteLn(s);
        end;
      end;
    end;
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.SaveVertCompareType;
var
  fr: TFileWriter;
  s, FileName, TxtFileName, sCompareType: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
begin
  TxtFileName := '��6��.���򻯡�����%d��������[��ͬ��ϡ���ͬ����]����ϡ�.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  fr := TFileWriter.Create(FileName);
  try
    s := TxtFileName.SubString(0, TxtFileName.Length - 4);
    fr.WriteLn('');
    fr.WriteLn('');
    fr.WriteLn(s);

    TSQLCompareGroup.AutoFree(Group, fDatabase,
      'RowCount >= ? ORDER BY MaxValueCount DESC, MaxTotalValueCount - MaxValueCount DESC, ValueCount, Size DESC',
      [fMinGroupRowCount]);
    TSQLCompareData.AutoFree(Data);
    while Group.FillOne do
    begin
      s := '%d.[ ��%d����ϣ����ţ�1.��%s��]���� w�ޡ���Ӧ�С�������';
      s := Format(s, [Group.FillCurrentRow - 1, Group.ValueCount, VertCompareTypeToString(Group.Value)]);
      if fFirstRangeValue = 0 then
        s := s + Format('%d��', [Group.MaxTotalValueCount])
      else
        s := s + Format('%d-%d��', [Group.MaxValueCount, Group.MaxTotalValueCount - Group.MaxValueCount]);
      s := s + '����';
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);

      Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
      while Data.FillOne do
      begin
        if Data.FillCurrentRow > 2 then fr.WriteLn('');
        SaveCompareData(fr, Data);
      end;
    end;
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.SaveCompareData(fr: TFileWriter; Data: TSQLCompareData; SaveValues: Boolean = True);
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
          sCompareType := 'ֱ';
          sCompareType2 := sCompareType;
          if fCompareMode = cmVertSlant then sCompareType2 := ' ' + sCompareType2;
        end;
        2:
        begin
          sCompareType := 'б';
          sCompareType2 := 'Y��';
        end;
        3:
        begin
          sCompareType := 'б';
          sCompareType2 := 'Z��';
        end;
      end;

      s := Format('��%d��������%d��Ϊ���У�%s��[����%d�У�����%d%s���У�]��', [
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
            s := s + Format('�С���Ӧ�С�����%d�У�', [TotalSameValueCount])
          else
            s := s + Format('�С���Ӧ�С������� %d-%d ���У�', [SameValueCount, TotalSameValueCount - SameValueCount]);
          s := s + DataToString(SameValues);
        end;
        if TotalDifferentValueCount > 0 then
        begin
          if TotalSameValueCount > 0 then s := s + '  ';
          if CompareType = 1 then s := s + 'w'
          else s := s + ' ';
          if fFirstRangeValue = 0 then
            s := s + Format('�ޡ���Ӧ�С�����%d�У�', [TotalDifferentValueCount])
          else
            s := s + Format('�ޡ���Ӧ�С������� %d-%d ���У�', [DifferentValueCount, TotalDifferentValueCount - DifferentValueCount]);
          s := s + DataToString(DifferentValues);
        end;
      end;
      fr.WriteLn('');
      fr.WriteLn(s);
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
  fr: TFileWriter;
  s, FileName, TxtFileName, sRowSpacing, sGroupValueCount: string;
  i: Integer;
  FirstRow: TSQLFirstRow;
  Data: TSQLCompareData;
  LastGroupValueCount: Byte;
  GroupNumber: Word;
  RowSpacing: Cardinal;
  GroupValueCount: Word;
  MaxGroupValueCount: Byte;
  MaxRowCountGroupValueCount: Byte;
  MaxGroupValueCountRowCount: Cardinal;
begin
  TxtFileName := '��1��.�����С�����%d��������[��ͬ���С���ͬ���]����ϡ�.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  fr := TFileWriter.Create(FileName);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    fr.WriteLn('');
    fr.WriteLn('');
    fr.WriteLn(s);

    s := 'ORDER BY Value DESC';
    if fMinGroupRowCount > 0 then s := 'Enabled = 1 ' + s;
    TSQLFirstRow.AutoFree(FirstRow, fDatabase, s, []);
    TSQLCompareData.AutoFree(Data);
    RowSpacingList := TStringList.Create;
    try
      while FirstRow.FillOne do
      begin
        if FirstRow.FillCurrentRow = 2 then
        begin
          s := '1.[ ֱ��б������%d��Ϊ���У�]��[ ���о� ��%d ]';
          s := Format(s, [fRowCount + 1, fRowCount + 1 - FirstRow.Value]);

          AddRowSpacing(fRowCount + 1 - FirstRow.Value, s);
        end;

        RowSpacing := FirstRow.RowSpacing;
        if fMinGroupRowCount > 0 then RowSpacing := FirstRow.RowSpacing2;
        s := '%d.[ ֱ��б������%d��Ϊ���У�]��[ ���о� ��%d ]';
        s := Format(s, [FirstRow.FillCurrentRow, FirstRow.Value, RowSpacing]);
        AddRowSpacing(FirstRow.RowSpacing, s);
      end;

      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn('���-С�����о� 1-100 ���ڣ�');
      RowSpacingList.Sort;
      for i := RowSpacingList.Count - 1 downto RowSpacingList.Count - 100 do
      begin
        if i < 0 then Break;
        fr.WriteLn('');
        fr.WriteLn(RowSpacingList.ValueFromIndex[i]);
      end;
    finally
      RowSpacingList.Free;
    end;

    FirstRow.FillRewind;
    while FirstRow.FillOne do
    begin
      if FirstRow.FillCurrentRow = 2 then
      begin
        s := '1.[ ֱ��б������%d��Ϊ���У�]��[ ���о� ��%d ]';
        s := Format(s, [fRowCount + 1, fRowCount + 1 - FirstRow.Value]);
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn(s);
      end;

      RowSpacing := FirstRow.RowSpacing;
      GroupValueCount := FirstRow.GroupValueCount;
      MaxGroupValueCount := FirstRow.MaxGroupValueCount;
      MaxRowCountGroupValueCount := FirstRow.MaxRowCountGroupValueCount;
      MaxGroupValueCountRowCount := FirstRow.MaxGroupValueCountRowCount;
      if fMinGroupRowCount > 0 then
      begin
        RowSpacing := FirstRow.RowSpacing2;
        GroupValueCount := FirstRow.GroupValueCount2;
        MaxGroupValueCount := FirstRow.MaxGroupValueCount2;
        MaxRowCountGroupValueCount := FirstRow.MaxRowCountGroupValueCount2;
        MaxGroupValueCountRowCount := FirstRow.MaxGroupValueCountRowCount2;
      end;

      s := '%d.[ ֱ��б������%d��Ϊ���У�]��[ ���о� ��%d ��';
      s := Format(s, [FirstRow.FillCurrentRow, FirstRow.Value, RowSpacing]);
      if MaxGroupValueCount > fExportGroupValueCount then
      begin
        sGroupValueCount := '';
        for i := fExportGroupValueCount to MaxGroupValueCount do
        begin
          if not sGroupValueCount.IsEmpty then
            sGroupValueCount := sGroupValueCount + '��';
          sGroupValueCount := sGroupValueCount + i.ToString;
        end;

        s := s + Format('����%s����ϣ��ܹ� %d �� ��', [
          sGroupValueCount,
          GroupValueCount
        ]);
      end;
      s := s + Format('��ࣨ��%d����ϣ����� %d �� ]��', [
        MaxRowCountGroupValueCount,
        MaxGroupValueCountRowCount
      ]);
      if FirstRow.FillCurrentRow > 2 then
      begin
        fr.WriteLn('');
        fr.WriteLn('');
      end;
      fr.WriteLn('');
      fr.WriteLn(s);

      s := 'FirstRow = ?';
      if fMinGroupRowCount > 0 then s := s + ' AND Enabled = 1';
      Data.FillPrepare(fDatabase, s, [FirstRow.Value]);
      LastGroupValueCount := 0;
      GroupNumber := 0;
      while Data.FillOne do
      begin
        if Data.GroupValueCount <> LastGroupValueCount then
        begin
          LastGroupValueCount := Data.GroupValueCount;
          GroupNumber := 0;
        end;
        GroupNumber := GroupNumber + 1;

        s := '��%d��.��%d�����[ ���ţ�%d.��%s��]��';
        s := Format(s, [
          Data.FillCurrentRow - 1,
          Data.GroupValueCount,
          GroupNumber,
          VertCompareTypeToString(Data.GroupValue)
        ]);
        fr.WriteLn('');
        fr.WriteLn(s);

        SaveCompareData(fr, Data);
      end;
    end;
    s := '%d.[����%d��Ϊ��ĩ���У�ֱ������%d-%d�У�]';
    s := Format(s, [
      FirstRow.FillTable.RowCount + 2,
      fVertSlantCompareSpacing + 1,
      fVertSlantCompareSpacing,
      fVertSlantCompareSpacing + 1 - fVertCompareSpacing
    ]);
    fr.WriteLn('');
    fr.WriteLn(s);
    s := '%d.[����%d��Ϊ��ĩ���У�ֱ������%d-%d�У�]';
    s := Format(s, [
      FirstRow.FillTable.RowCount + 3,
      fVertSlantCompareSpacing + 1,
      fVertSlantCompareSpacing,
      fVertSlantCompareSpacing + 1 - fSlantCompareSpacing
    ]);
    fr.WriteLn('');
    fr.WriteLn(s);

    fr.CloseFile;
    for s in fr.Files do
      if TFile.Exists(s) then TFile.Copy(s, fExportDirectory + TPath.GetFileName(s));
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.SaveVertSlantGroupByCompareTypeSortByRowcount;
var
  fr, fr2, fr3, fr4: TFileWriter;
  s, sCompareType, sCompareType2, FileName, FileName2, FileName3, FileName4, TxtFileName: string;
  Group, Group2: TSQLCompareGroup;
  Data: TSQLCompareData;
  RowNo: Cardinal;
  LastFirstRow, MaxRowCount, RowCountNumber: Cardinal;
begin
  TxtFileName := '��2��.�����С�����%d��������[��ͬ��ϡ���ͬ����]�����[��ͬ��������������]��.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  TxtFileName := '��2��.�����С�����%d��������[��ͬ��ϡ���ͬ����]�����[��ͬ��������������]����2��.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName2 := fExportDirectory + TxtFileName;
  TxtFileName := '��2��.�����С�����%d��������[��ͬ��ϡ���ͬ����]�����[��ͬ��������������]����3��.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName3 := fExportDirectory + TxtFileName;
  TxtFileName := '��2��.�����С�����%d��������[��ͬ��ϡ���ͬ����]�����[��ͬ��������������]����1��-���򻯲�ͳ�ƣ�.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName4 := fExportDirectory + TxtFileName;
  fr := TFileWriter.Create(FileName);
  fr2 := TFileWriter.Create(FileName2);
  fr3 := TFileWriter.Create(FileName3);
  fr4 := TFileWriter.Create(FileName4);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    fr.WriteLn('');
    fr.WriteLn('');
    fr.WriteLn(s);
    s := TPath.GetFileNameWithoutExtension(FileName2);
    fr2.WriteLn('');
    fr2.WriteLn('');
    fr2.WriteLn(s);
    s := TPath.GetFileNameWithoutExtension(FileName3);
    fr3.WriteLn('');
    fr3.WriteLn('');
    fr3.WriteLn(s);
    s := TPath.GetFileNameWithoutExtension(FileName4);
    fr4.WriteLn('');
    fr4.WriteLn('');
    fr4.WriteLn(s);

    TSQLCompareGroup.AutoFree(Group, fDatabase,
      'RowCount >= ? ORDER BY RowCount DESC, ValueCount, Size DESC',
      [fMinGroupRowCount]);
    TSQLCompareGroup.AutoFree(Group2);
    TSQLCompareData.AutoFree(Data);
    RowNo := 0;
    RowCountNumber := 0;
    while Group.FillOne do
    begin
      s := '%d.����%d���� [ ��ͬ��ϡ���ͬ��ֱ��б��������]����� [ ��ͬ��ֱ��б������������%d ]����';
      s := Format(s, [
        Group.FillCurrentRow - 1,
        Group.ValueCount,
        Group.RowCount
      ]);
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);
      fr2.WriteLn('');
      fr2.WriteLn('');
      fr2.WriteLn('');
      fr2.WriteLn(s);
      fr3.WriteLn('');
      fr3.WriteLn('');
      fr3.WriteLn('');
      RowCountNumber := RowCountNumber + 1;
      if Group.FillCurrentRow = 2 then MaxRowCount := Group.RowCount;
      if Group.FillCurrentRow <= Group.FillTable.RowCount then
        Group.FillRow(Group.FillCurrentRow, Group2);
      if (Group.FillCurrentRow > Group.FillTable.RowCount) or (Group.RowCount <> Group2.RowCount) then
      begin
        fr4.WriteLn('');
        fr4.WriteLn('');
        fr4.WriteLn('');
        fr4.WriteLn(s);
        s := 'ͳ�ƣ�[ ��ͬ��ֱ��б������������%d ] ���ܹ� ��%d �� ��';
        s := Format(s, [Group.RowCount, RowCountNumber]);
        if Group.RowCount <> MaxRowCount then
        begin
          s := s + '[ ��ͬ��ֱ��б������������%d-%d ] ���ܹ� ��%d �� ��';
          s := Format(s, [MaxRowCount, Group.RowCount, Group.FillCurrentRow - 1]);
        end;
        fr4.WriteLn('');
        fr4.WriteLn(s);
        RowCountNumber := 0;
      end;

      LastFirstRow := 0;
      Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
      while Data.FillOne do
      begin
        sCompareType := VertCompareTypeToString(Group.Value);

        if Data.FillCurrentRow = 2 then
        begin
          RowNo := RowNo + 1;

          s := '%d= ��%d�����[ ���ţ�3.��%s��]����%d����%d��';
          s := Format(s, [RowNo, Group.ValueCount, sCompareType, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
          fr2.WriteLn('');
          fr2.WriteLn(s);

          s := '%d=[ ���ţ�3.��%s��]����%d����%d��';
          s := Format(s, [RowNo, sCompareType, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
          fr3.WriteLn('');
          fr3.WriteLn(s);
        end;

        if LastFirstRow <> Data.FirstRow then
        begin
          LastFirstRow := Data.FirstRow;
          s := '��%d��.��%d�����[ ���ţ�1.��%s��]��';
          s := Format(s, [Data.FillCurrentRow - 1, Group.ValueCount, sCompareType]);

          fr.WriteLn('');
          fr.WriteLn(s);
        end;

        SaveCompareData(fr, Data);

        RowNo := RowNo + 1;

        s := '%d= ��%d�����[ ���ţ�1.��%s��]����%d����%d��';
        s := Format(s, [RowNo, Group.ValueCount, sCompareType, Data.FirstRow, Data.RowSpacing]);
        fr2.WriteLn('');
        fr2.WriteLn(s);

        SaveCompareData(fr2, Data);

        s := '%d=[ ���ţ�1.��%s��]����%d����%d��';
        s := Format(s, [RowNo, sCompareType, Data.FirstRow, Data.RowSpacing]);
        fr3.WriteLn('');
        fr3.WriteLn(s);

        SaveCompareData(fr3, Data, False);

        if Data.FillCurrentRow > Data.FillTable.RowCount then
        begin
          RowNo := RowNo + 1;

          s := '%d= ��%d�����[ ���ţ�1.��%s��]����%d����0��';
          s := Format(s, [RowNo, Group.ValueCount, sCompareType, fVertSlantCompareSpacing + 1]);
          fr2.WriteLn('');
          fr2.WriteLn(s);

          s := '%d=[ ���ţ�1.��%s��]����%d����0��';
          s := Format(s, [RowNo, sCompareType, fVertSlantCompareSpacing + 1]);
          fr3.WriteLn('');
          fr3.WriteLn(s);
        end;
      end;
    end;

    fr.CloseFile;
    for s in fr.Files do
    begin
      TxtFileName := TPath.GetFileName(s).Replace(']��', ']����1��');
      if TFile.Exists(s) then TFile.Copy(s, fExportDirectory + TxtFileName);
    end;
  finally
    fr.Free;
    fr2.Free;
    fr3.Free;
    fr4.Free;
  end;
end;

procedure TDataComputer.SaveVertSlantGroupByCompareTypeSortByMaxRowSpacing;
var
  fr: TFileWriter;
  s, FileName, TxtFileName: string;
  Group: TSQLCompareGroup;
  Data, Data2: TSQLCompareData;
  RowNo: Word;
begin
  TxtFileName := '��3��.�����С�����%d��������[��ͬ��ϡ���ͬ����]�����[���оࣺ������С��]��.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  fr := TFileWriter.Create(FileName);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    fr.WriteLn('');
    fr.WriteLn('');
    fr.WriteLn(s);

    TSQLCompareGroup.AutoFree(Group, fDatabase,
      'RowCount >= ? ORDER BY MaxRowSpacing DESC, ValueCount DESC, Size DESC',
      [fMinGroupRowCount]);
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
          s := '%d.[ ֱ������%d��Ϊ���У�] .��������о� ��%d ����';
          s := Format(s, [
            Group.FillCurrentRow - 1,
            fRowCount + 1,
            Group.MaxRowSpacing
          ]);
          fr.WriteLn('');
          fr.WriteLn('');
          fr.WriteLn('');
          fr.WriteLn(s);

          s := '��1��.[ ֱ������%d��Ϊ���У�] .�����о� ��%d ����';
          s := Format(s, [
            fRowCount + 1,
            fRowCount + 1 - Data.FirstRow
          ]);
          fr.WriteLn('');
          fr.WriteLn(s);

          s := '��2��.[ ֱ������%d��Ϊ���У�] .�����о� ��%d ����';
          s := Format(s, [Data.FirstRow, Data.RowSpacing]);
          fr.WriteLn('');
          fr.WriteLn(s);
        end;

        s := '��%d��.��%d�����[ ���ţ�3.��%s��]��';
        s := Format(s, [
          Data.FillCurrentRow - 1,
          Group.ValueCount,
          VertCompareTypeToString(Data.GroupValue)
        ]);
        fr.WriteLn('');
        fr.WriteLn(s);

        SaveCompareData(fr, Data);

        if Data.FillCurrentRow > Data.FillTable.RowCount then
        begin
          RowNo := RowNo + 1;
          s := '��%d��.[����%d��Ϊ��ĩ���У�ֱ������%d-%d�У�]';
          s := Format(s, [RowNo, fVertSlantCompareSpacing + 1, fVertSlantCompareSpacing, fVertSlantCompareSpacing + 1 - fVertCompareSpacing]);
          fr.WriteLn('');
          fr.WriteLn(s);

          RowNo := RowNo + 1;
          s := '��%d��.[����%d��Ϊ��ĩ���У�б������%d-%d�У�]';
          s := Format(s, [RowNo, fVertSlantCompareSpacing + 1, fVertSlantCompareSpacing, fVertSlantCompareSpacing + 1 - fSlantCompareSpacing]);
          fr.WriteLn('');
          fr.WriteLn(s);
        end
        else
        begin
          RowNo := RowNo + 1;

          Data.FillRow(Data.FillCurrentRow, Data2);
          if Data.FillCurrentRow = Data.FillTable.RowCount then
            Data2.RowSpacing := 0;

          s := '��%d��.[ ֱ��б����%d��Ϊ���У�] .�����о� ��%d ����';
          s := Format(s, [RowNo, Data2.FirstRow, Data2.RowSpacing]);
          fr.WriteLn('');
          fr.WriteLn(s);
        end;
      end;
    end;
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.SaveVertSlantGroupByCompareTypeSortByCompareTypeCount;
var
  fr: TFileWriter;
  s, FileName, TxtFileName: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
begin
  TxtFileName := '��4��.�����С�����%d��������[��ͬ��ϡ���ͬ����]�����[������������ٸ�]��.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  fr := TFileWriter.Create(FileName);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    fr.WriteLn('');
    fr.WriteLn('');
    fr.WriteLn(s);

    TSQLCompareGroup.AutoFree(Group, fDatabase,
      'RowCount >= ? ORDER BY ValueCount DESC, Size DESC',
      [fMinGroupRowCount]);
    TSQLCompareData.AutoFree(Data);
    while Group.FillOne do
    begin
      s := '%d.[ ��%d����ϣ����ţ�1.��%s��]�����������%d������';
      s := Format(s, [
        Group.FillCurrentRow - 1,
        Group.ValueCount,
        VertCompareTypeToString(Group.Value),
        Group.ValueCount
      ]);
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);

      Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC LIMIT 1', [Group.Value]);
      while Data.FillOne do
      begin
        if Data.FillCurrentRow > 2 then fr.WriteLn('');
        SaveCompareData(fr, Data);
      end;
    end;
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.SaveVertSlantGroupByCompareTypeSortByMaxValueCount;
var
  fr: TFileWriter;
  s, FileName, TxtFileName: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
begin
  TxtFileName := '��5��.�����С�����%d��������[��ͬ��ϡ���ͬ����]�����[�ޡ���Ӧ�С�������������]��.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  fr := TFileWriter.Create(FileName);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    fr.WriteLn('');
    fr.WriteLn('');
    fr.WriteLn(s);

    TSQLCompareData.AutoFree(Data, fDatabase,
      'ORDER BY DifferentValueCount DESC, TotalDifferentValueCount - DifferentValueCount DESC',
      []);
    while Data.FillOne do
    begin
      s := '%d.[ ��%d����ϣ����ţ�1.��%s��]���� ';
      s := Format(s, [Data.FillCurrentRow - 1, Data.GroupValueCount, VertCompareTypeToString(Data.GroupValue)]);
      if Data.TotalSameValueCount > 0 then
      begin
        s := s + 'y�С���Ӧ�С�������';
        if fFirstRangeValue = 0 then
          s := s + Format('%d��', [Data.TotalSameValueCount])
        else
          s := s + Format('%d-%d��', [Data.SameValueCount, Data.TotalSameValueCount - Data.SameValueCount])
      end;
      if Data.TotalDifferentValueCount > 0 then
      begin
        if Data.TotalSameValueCount > 0 then s := s + '��';

        s := s + 'w�ޡ���Ӧ�С�������';
        if fFirstRangeValue = 0 then
          s := s + Format('%d��', [Data.TotalDifferentValueCount])
        else
          s := s + Format('%d-%d��', [Data.DifferentValueCount, Data.TotalDifferentValueCount - Data.DifferentValueCount])
      end;
      s := s + '����';
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);

      SaveCompareData(fr, Data);
    end;
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.SaveVertSlantGroupByCompareTypeSortByLastFirstRow;
var
  fr: TFileWriter;
  s, FileName, TxtFileName, sCompareType: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
  RowNo: Word;
begin
  TxtFileName := '��2��.�����С�����%d��������[��ͬ��ϡ���ͬ����]����� [�������оࣺ������С��]����4��.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory + TxtFileName;
  fr := TFileWriter.Create(FileName);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    fr.WriteLn('');
    fr.WriteLn('');
    fr.WriteLn(s);

    RowNo := 0;

    TSQLCompareGroup.AutoFree(Group, fDatabase,
      'RowCount >= ? ORDER BY LastFirstRow, ValueCount DESC, Size DESC',
      [fMinGroupRowCount]);
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
          s := '%d=[ ���ţ�1.��%s��]����%d����%d��	';
          s := Format(s, [RowNo, sCompareType, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
          fr.WriteLn('');
          fr.WriteLn('');
          fr.WriteLn('');
          fr.WriteLn(s);
        end;

        RowNo := RowNo + 1;
        s := '%d=[ ���ţ�1.��%s��]����%d����%d��	';
        s := Format(s, [RowNo, sCompareType, Data.FirstRow, Data.RowSpacing]);
        fr.WriteLn('');
        fr.WriteLn(s);

        SaveCompareData(fr, Data, False);

        if Data.FillCurrentRow > Data.FillTable.RowCount then
        begin
          RowNo := RowNo + 1;
          s := '%d=[ ���ţ�1.��%s��]����%d����0��	';
          s := Format(s, [RowNo, sCompareType, fVertSlantCompareSpacing + 1]);
          fr.WriteLn('');
          fr.WriteLn(s);
        end;
      end;
    end;
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.SaveVertSlantGroupByCompareTypeSortByLastFirstRow2;
var
  fr: TFileWriter;
  s, FileName, TxtFileName, sCompareType: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
  RowNo: Word;
begin
  TxtFileName := '��2��.�����С�����%d��������[��ͬ��ϡ���ͬ����]����� [�������оࣺ��С�������]����5��.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory + TxtFileName;
  fr := TFileWriter.Create(FileName);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    fr.WriteLn('');
    fr.WriteLn('');
    fr.WriteLn(s);

    RowNo := 0;

    TSQLCompareGroup.AutoFree(Group, fDatabase,
      'RowCount >= ? ORDER BY OneRowSpacingCount DESC, ValueCount, Size DESC',
      [fMinGroupRowCount]);
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
          s := '%d=[ ���ţ�1.��%s��]����%d����%d��	';
          s := Format(s, [RowNo, sCompareType, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
          fr.WriteLn('');
          fr.WriteLn('');
          fr.WriteLn('');
          fr.WriteLn(s);
        end;

        RowNo := RowNo + 1;
        s := '%d=[ ���ţ�1.��%s��]����%d����%d��	';
        s := Format(s, [RowNo, sCompareType, Data.FirstRow, Data.RowSpacing]);
        fr.WriteLn('');
        fr.WriteLn(s);

        SaveCompareData(fr, Data, False);

        if Data.FillCurrentRow > Data.FillTable.RowCount then
        begin
          RowNo := RowNo + 1;
          s := '%d=[ ���ţ�1.��%s��]����%d����0��	';
          s := Format(s, [RowNo, sCompareType, fVertSlantCompareSpacing + 1]);
          fr.WriteLn('');
          fr.WriteLn(s);
        end;
      end;
    end;
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.SaveVertSlantCompareType;
var
  fr: TFileWriter;
  s, FileName, TxtFileName, sCompareType: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
begin
  TxtFileName := '��6��.���򻯡�����%d��������[��ͬ��ϡ���ͬ����]����ϡ�.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  fr := TFileWriter.Create(FileName);
  try
    s := TxtFileName.SubString(0, TxtFileName.Length - 4);
    fr.WriteLn('');
    fr.WriteLn('');
    fr.WriteLn(s);

    TSQLCompareData.AutoFree(Data, fDatabase,
      'ORDER BY DifferentValueCount DESC, TotalDifferentValueCount - DifferentValueCount DESC',
      []);
    while Data.FillOne do
    begin
      s := '%d.[ ��%d����ϣ����ţ�1.��%s��]���� ';
      s := Format(s, [Data.FillCurrentRow - 1, Data.GroupValueCount, VertCompareTypeToString(Data.GroupValue)]);
      if Data.TotalSameValueCount > 0 then
      begin
        s := s + 'y�С���Ӧ�С�������';
        if fFirstRangeValue = 0 then
          s := s + Format('%d��', [Data.TotalSameValueCount])
        else
          s := s + Format('%d-%d��', [Data.SameValueCount, Data.TotalSameValueCount - Data.SameValueCount])
      end;
      if Data.TotalDifferentValueCount > 0 then
      begin
        if Data.TotalSameValueCount > 0 then s := s + '��';

        s := s + 'w�ޡ���Ӧ�С�������';
        if fFirstRangeValue = 0 then
          s := s + Format('%d��', [Data.TotalDifferentValueCount])
        else
          s := s + Format('%d-%d��', [Data.DifferentValueCount, Data.TotalDifferentValueCount - Data.DifferentValueCount])
      end;
      s := s + '����';
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);

      SaveCompareData(fr, Data);
    end;
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.ExportCompareData(ExportFiles: TExportFiles;
  RowSpacing: Cardinal; GroupRowCount: Cardinal);
var
  s, sCompareMode, sMode: string;
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
  fKeepMaxRowSpacing := RowSpacing;
  fMinGroupRowCount := GroupRowCount;
  if fMinGroupRowCount > 0 then Recalc;

  case fCompareMode of
    cmVert: sCompareMode := 'ֱ';
    cmSlant: sCompareMode := 'б';
    cmVertSlant: sCompareMode := 'ֱ��б';
  end;
  sMode := '1';
  if fFirstRangeValue > 0 then sMode := '2';
  fExportDirectory := TPath.GetDirectoryName(ParamStr(0))
    + Format('\���������%s��������%sģʽ��\', [sCompareMode, sMode]);
  fExportDirectory2 := fExportDirectory + '��1��.�����С�-��6��.���򻯡�\';
  if not TDirectory.Exists(fExportDirectory) then
    TDirectory.CreateDirectory(fExportDirectory);
  for s in TDirectory.GetFiles(fExportDirectory, '*.txt') do TFile.Delete(s);
  if not TDirectory.Exists(fExportDirectory2) then
    TDirectory.CreateDirectory(fExportDirectory2);
  for s in TDirectory.GetFiles(fExportDirectory2, '*.txt') do TFile.Delete(s);

  case fCompareMode of
    cmVert:
    begin
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
