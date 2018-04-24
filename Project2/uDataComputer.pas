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
  private
    fFirstRow: Word;
    fCompareType: Int64;
    fCompareTypeValueCount: Byte;
    fRowSpacing: Word;
  published
    property FirstRow: Word read fFirstRow write fFirstRow;
    property CompareType: Int64 read fCompareType write fCompareType;
    property CompareTypeValueCount: Byte read fCompareTypeValueCount write fCompareTypeValueCount;
    property RowSpacing: Word read fRowSpacing write fRowSpacing;
  end;

  TSQLFirstRow = class(TSQLRecord)
  private
    fValue : Word;
    fRowCount: Cardinal;
    fRowSpacing: Word;
  published
    property Value: Word read fValue write fValue stored AS_UNIQUE;
    property RowCount: Cardinal read fRowCount write fRowCount;
    property RowSpacing: Word read fRowSpacing write fRowSpacing;
  end;

  TSQLCompareType = class(TSQLRecord)
  private
    fValue : Int64;
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

  TSQLVertCompareData = class(TSQLData)
  private type
    TCompareRow = record
      Number: Word;
      ExistSameValueCount: Word;
      RowNumber: Word;
    end;
    TCompareRowArray = array of TCompareRow;
  private
    fFirstRow: Word;
    fCompareType: Int64;
    fCompareTypeValueCount: Byte;
    fCompareRows: TCompareRowArray;
    fRowSpacing: Word;
  published
    property FirstRow: Word read fFirstRow write fFirstRow;
    property CompareType: Int64 read fCompareType write fCompareType;
    property CompareTypeValueCount: Byte read fCompareTypeValueCount write fCompareTypeValueCount;
    property CompareRows: TCompareRowArray read fCompareRows write fCompareRows;
    property RowSpacing: Word read fRowSpacing write fRowSpacing;
  end;

  TSQLVertFirstRow = class(TSQLRecord)
  private
    fValue : Word;
    fRowSpacing: Word;
    fCompareTypeCount: Word;
    fMaxCompareTypeValueCount: Byte;
    fMaxRowCountCompareTypeValueCount: Byte;
    fMaxCompareTypeValueCountRowCount: Word;
  published
    property Value: Word read fValue write fValue stored AS_UNIQUE;
    property RowSpacing: Word read fRowSpacing write fRowSpacing;
    property CompareTypeCount: Word read fCompareTypeCount write fCompareTypeCount;
    property MaxCompareTypeValueCount: Byte read fMaxCompareTypeValueCount
      write fMaxCompareTypeValueCount;
    property MaxRowCountCompareTypeValueCount: Byte read fMaxRowCountCompareTypeValueCount
      write fMaxRowCountCompareTypeValueCount;
    property MaxCompareTypeValueCountRowCount: Word
      read fMaxCompareTypeValueCountRowCount
      write fMaxCompareTypeValueCountRowCount;
  end;

  TSQLVertCompareType = class(TSQLRecord)
  private
    fValue: Int64;
    fValueCount: Byte;
    fSize: RawUTF8;
    fRowCount: Cardinal;
    fMaxRowSpacing: Word;
    fMaxTotalValueCount: Word;
    fMaxValueCount: Word;
    fLastFirstRow: Word;
  published
    property Value: Int64 read fValue write fValue stored AS_UNIQUE;
    property ValueCount: Byte read fValueCount write fValueCount;
    property Size: RawUTF8 index 64 read fSize write fSize;
    property RowCount: Cardinal read fRowCount write fRowCount;
    property MaxRowSpacing: Word read fMaxRowSpacing write fMaxRowSpacing;
    property MaxTotalValueCount: Word read fMaxTotalValueCount write fMaxTotalValueCount;
    property MaxValueCount: Word read fMaxValueCount write fMaxValueCount;
    property LastFirstRow: Word read fLastFirstRow write fLastFirstRow;
  end;

  TSQLVertSlantCompareData = class(TSQLData)
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

  TSQLVertSlantFirstRow = class(TSQLRecord)
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
    TCompareProcess = (cpCompare, cpGrouping, cpUpdateFirstRow, cpUpdateCompareType, cpFinish);
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
    TInitCompareEvent = procedure(var CompareSpacing: Byte; var ExportTypeCount: Byte) of object;
    TInitVertCompareEvent = procedure(var CompareSpacing: Byte; var SameValueCount: Byte;
      var SameValueCount2: Byte; var CompareTypeCount: Byte; var ExportTypeCount: Byte) of object;
    TInitVertSlantCompareEvent = procedure(var CompareMode: TCompareMode;
      var VertCompareSpacing: Byte; var VertSameValueCount: Byte;
      var VertSameValueCount2: Byte; var SlantCompareSpacing: Byte;
      var SlantSameValueCount: Byte; var SlantSameValueCount2: Byte;
      var CompareGroupValueCount: Byte; var ExportGroupValueCount: Byte) of object;
  private
    fInitEvent: TInitEvent;
    fInitCompareEvent: TInitCompareEvent;
    fInitVertCompareEvent: TInitVertCompareEvent;
    fInitVertSlantCompareEvent: TInitVertSlantCompareEvent;
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
    fCompareSpacing: Byte;
    fCompareState: TRecCompareState;
    fRowCount: Integer;
    fExportTypeCount: Byte;
    fVertCompareSpacing: Byte;
    fVertSameValueCount: Byte;
    fVertSameValueCount2: Byte;
    fVertCompareTypeCount: Byte;
    fVertExportTypeCount: Byte;
    fCompareMode: TCompareMode;
    fCompareModeString: string;
    fVertSlantCompareSpacing: Byte;
    fVertSlantCompareSpacing2: Byte;
    fVVertCompareSpacing: Byte;
    fVVertSameValueCount: Byte;
    fVVertSameValueCount2: Byte;
    fSlantCompareSpacing: Byte;
    fSlantSameValueCount: Byte;
    fSlantSameValueCount2: Byte;
    fCompareGroupValueCount: Byte;
    fExportGroupValueCount: Byte;
    function GetKeyValue(Key: string): Variant;
    procedure SetKeyValue(Key: string; Value: Variant);
    function CompareTypeToString(Value: Int64; Flag: Byte = 0): string;
    function VertCompareTypeToString(Value: Int64): string;
    function DataToString(Values: SynCommons.TWordDynArray): string;
    procedure CalcRowSpacing(CompareData: TSQLCompareData);
    procedure CalcVertRowSpacing(CompareData: TSQLVertCompareData);
    procedure CalcVertSlantRowSpacing(CompareData: TSQLVertSlantCompareData);
    procedure SaveGroupByFirstRow;
    procedure SaveGroupByCompareTypeSortByRowcount;
    procedure SaveGroupByCompareTypeSortByMaxRowSpacing;
    procedure SaveGroupByCompareTypeSortByCompareTypeCount;
    procedure SaveGroupByCompareTypeSortByMaxValueCount;
    procedure SaveGroupByCompareTypeSortByLastFirstRow;
    procedure SaveGroupByCompareTypeSortByOneRowSpacingCount;
    procedure SaveCompareType;

    procedure SaveVertCompareData(const tf: TextFile; Data: TSQLVertCompareData);
    procedure SaveVertGroupByFirstRow;
    procedure SaveVertGroupByCompareTypeSortByRowcount;
    procedure SaveVertGroupByCompareTypeSortByMaxRowSpacing;
    procedure SaveVertGroupByCompareTypeSortByCompareTypeCount;
    procedure SaveVertGroupByCompareTypeSortByMaxValueCount;
    procedure SaveVertGroupByCompareTypeSortByLastFirstRow;
    procedure SaveVertGroupByCompareTypeSortByLastFirstRow2;
    procedure SaveVertCompareType;

    procedure SaveVertSlantCompareData(const tf: TextFile; Data: TSQLVertSlantCompareData; SaveSlantValues: Boolean = True);
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
    procedure VertCompare;
    procedure VertSlantCompare;
    procedure ExportCompareRow;
    procedure ExportCompareData(ExportFiles: TExportFiles);
    procedure ExportVertCompareData(ExportFiles: TExportFiles);
    procedure ExportVertSlantCompareData(ExportFiles: TExportFiles);
  published
    property InitEvent: TInitEvent read fInitEvent write fInitEvent;
    property InitCompareEvent: TInitCompareEvent read fInitCompareEvent write fInitCompareEvent;
    property InitVertCompareEvent: TInitVertCompareEvent read fInitVertCompareEvent write fInitVertCompareEvent;
    property InitVertSlantCompareEvent: TInitVertSlantCompareEvent read fInitVertSlantCompareEvent write fInitVertSlantCompareEvent;
    property MaxValue: Word read fMaxValue;
    property FirstRangeValue: Word read fFirstRangeValue;
    property CompareSpacing: Byte read fCompareSpacing;
    property ExportTypeCount: Byte read fExportTypeCount;
    property VertCompareSpacing: Byte read fVertCompareSpacing;
    property VertSameValueCount: Byte read fVertSameValueCount;
    property VertSameValueCount2: Byte read fVertSameValueCount2;
    property VertCompareTypeCount: Byte read fVertCompareTypeCount;
    property VertExportTypeCount: Byte read fVertExportTypeCount;
    property CompareMode: TCompareMode read fCompareMode;
    property VVertCompareSpacing: Byte read fVVertCompareSpacing;
    property VVertSameValueCount: Byte read fVVertSameValueCount;
    property VVertSameValueCount2: Byte read fVVertSameValueCount2;
    property SlantCompareSpacing: Byte read fSlantCompareSpacing;
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

procedure TSQLVertSlantCompareData.CalcCompareValueCount(FirstRangeValue: Byte);
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
    'CompareType = ? AND FirstRow < ? ORDER BY FirstRow DESC LIMIT 1',
    [CompareData.CompareType, CompareData.FirstRow]);
  if CompareData2.FillOne then
    CompareData.RowSpacing := CompareData.FirstRow - CompareData2.FirstRow
  else
    CompareData.RowSpacing := CompareData.FirstRow - fCompareSpacing - 1;
end;

procedure TDataComputer.CalcVertRowSpacing(CompareData: TSQLVertCompareData);
var
  CompareData2: TSQLVertCompareData;
begin
  TSQLVertCompareData.AutoFree(CompareData2, fDatabase,
    'CompareType = ? AND FirstRow < ? ORDER BY FirstRow DESC LIMIT 1',
    [CompareData.CompareType, CompareData.FirstRow]);
  if CompareData2.FillOne then
    CompareData.RowSpacing := CompareData.FirstRow - CompareData2.FirstRow
  else
    CompareData.RowSpacing := CompareData.FirstRow - fVertCompareSpacing - 1;
end;

procedure TDataComputer.CalcVertSlantRowSpacing(CompareData: TSQLVertSlantCompareData);
var
  CompareData2: TSQLVertSlantCompareData;
begin
  TSQLVertSlantCompareData.AutoFree(CompareData2, fDatabase,
    'GroupValue = ? AND FirstRow < ? ORDER BY FirstRow DESC LIMIT 1',
    [CompareData.GroupValue, CompareData.FirstRow]);
  if CompareData2.FillOne then
    CompareData.RowSpacing := CompareData.FirstRow - CompareData2.FirstRow
  else
    CompareData.RowSpacing := 0;
    //CompareData.RowSpacing := CompareData.FirstRow - fVertSlantCompareSpacing - 1;
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
    TSQLCompareType, TSQLVertCompareData, TSQLVertFirstRow, TSQLVertCompareType,
    TSQLVertSlantCompareData, TSQLVertSlantFirstRow, TSQLCompareGroup]);
  fDatabase := TSQLRestServerDB.Create(fModel, fDatabaseFileName);
  fDatabase.CreateMissingTables;
  fDatabase.CreateSQLIndex(TSQLCompareData, 'FirstRow', False);
  fDatabase.CreateSQLIndex(TSQLCompareData, 'CompareType', False);
  fDatabase.CreateSQLIndex(TSQLCompareData, 'CompareTypeValueCount', False);
  fDatabase.CreateSQLIndex(TSQLCompareType, 'Size', False);
  fDatabase.CreateSQLIndex(TSQLCompareType, 'ValueCount', False);
  fDatabase.CreateSQLIndex(TSQLCompareType, 'MaxRowSpacing', False);
  fDatabase.CreateSQLIndex(TSQLCompareType, ['MaxTotalValueCount', 'MaxValueCount'], False);
  fDatabase.CreateSQLIndex(TSQLCompareType, 'LastFirstRow', False);
  fDatabase.CreateSQLIndex(TSQLCompareType, 'OneRowSpacingCount', False);
  fDatabase.CreateSQLIndex(TSQLVertCompareData, 'FirstRow', False);
  fDatabase.CreateSQLIndex(TSQLVertCompareData, 'CompareType', False);
  fDatabase.CreateSQLIndex(TSQLVertCompareData, 'CompareTypeValueCount', False);
  fDatabase.CreateSQLIndex(TSQLVertCompareType, 'Size', False);
  fDatabase.CreateSQLIndex(TSQLVertCompareType, 'ValueCount', False);
  //fDatabase.CreateSQLIndex(TSQLVertCompareType, 'MaxRowSpacing', False);
  fDatabase.CreateSQLIndex(TSQLVertCompareType, ['MaxTotalValueCount', 'MaxValueCount'], False);
  fDatabase.CreateSQLIndex(TSQLVertCompareType, 'LastFirstRow', False);
  fKeyValue := TSQLKeyValue.Create;

  v := GetKeyValue('MaxValue');
  if v = Unassigned then fMaxValue := 0
  else fMaxValue := v;
  v := GetKeyValue('FirstRangeValue');
  if v = Unassigned then fFirstRangeValue := 0
  else fFirstRangeValue := v;
  v := GetKeyValue('CompareSpacing');
  if v = Unassigned then fCompareSpacing := 0
  else fCompareSpacing := v;
  v := GetKeyValue('ExportTypeCount');
  if v = Unassigned then fExportTypeCount := 0
  else fExportTypeCount := v;
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
  if v = Unassigned then fVertCompareTypeCount := 0
  else fVertCompareTypeCount := v;
  v := GetKeyValue('VertExportTypeCount');
  if v = Unassigned then fVertExportTypeCount := 0
  else fVertExportTypeCount := v;
  v := GetKeyValue('CompareMode');
  if v = Unassigned then fCompareMode := cmNone
  else fCompareMode := TCompareMode(v);
  v := GetKeyValue('VVertCompareSpacing');
  if v = Unassigned then fVVertCompareSpacing := 0
  else fVVertCompareSpacing := v;
  v := GetKeyValue('VVertSameValueCount');
  if v = Unassigned then fVVertSameValueCount := 0
  else fVVertSameValueCount := v;
  v := GetKeyValue('VVertSameValueCount2');
  if v = Unassigned then fVVertSameValueCount2 := 0
  else fVVertSameValueCount2 := v;
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
  fVertSlantCompareSpacing := fVVertCompareSpacing;
  if fSlantCompareSpacing > fVVertCompareSpacing then
    fVertSlantCompareSpacing := fSlantCompareSpacing;
  fVertSlantCompareSpacing2 := fVVertCompareSpacing;
  if fSlantCompareSpacing < fVVertCompareSpacing then
    fVertSlantCompareSpacing2 := fSlantCompareSpacing;
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
var
  Row, Row2: TSQLRow;
  CompareData, CompareData2: TSQLCompareData;
  FirstRow, FirstRow2: TSQLFirstRow;
  CompareType: TSQLCompareType;

  procedure CompareRow(Offset: Integer);
  var
    i, v, v2: Integer;
    IsExist: Boolean;
  begin
    CompareData.ClearValue;
    for v in Row2.Values do
    begin
      v2 := v + Offset;
      if (v2 < 1) or (v2 > fMaxValue) then Continue;

      if Row.ValueExist(v2) then
      begin
        CompareData.ClearValue;
        Exit;
      end;
      CompareData.AddValue(v2);
    end;
  end;

var
  i, i2: Integer;
  RowNo: Word;
  v: Variant;
begin
  if fCompareSpacing = 0 then
    if Assigned(fInitCompareEvent) then
    begin
      fInitCompareEvent(fCompareSpacing, fExportTypeCount);

      if (fCompareSpacing < 1) or (fCompareSpacing > 32) then
        raise Exception.Create('比较次数无效');
      if fExportTypeCount < 1 then
        raise Exception.Create('导出次数无效');

      SetKeyValue('CompareSpacing', fCompareSpacing);
      SetKeyValue('ExportTypeCount', fExportTypeCount);
    end
    else raise Exception.Create('比较斜连初始化失败');

  fCompareState.Process := cpCompare;
  fCompareState.FirstRow := fCompareSpacing + 1;
  v := GetKeyValue('CompareStateProcess');
  if v <> Unassigned then fCompareState.Process := TCompareProcess(v);
  v := GetKeyValue('CompareStateFirstRow');
  if v <> Unassigned then fCompareState.FirstRow := v;
  fRowCount := fDatabase.TableRowCount(TSQLRow);

  TSQLRow.AutoFree(Row, fDatabase, 'Number >= ?', [fCompareState.FirstRow - fCompareSpacing]);
  TSQLRow.AutoFree(Row2);
  TSQLCompareData.AutoFree(CompareData);
  TSQLCompareData.AutoFree(CompareData2);
  TSQLFirstRow.AutoFree(FirstRow);
  TSQLFirstRow.AutoFree(FirstRow2);
  TSQLCompareType.AutoFree(CompareType);

  for i := fCompareSpacing + 1 to Row.FillTable.RowCount do
  begin
    Row.FillRow(i);
    repeat
      case fCompareState.Process of
        cpCompare:
        begin
          fDatabase.TransactionBegin(TSQLCompareData);
          try
            for i2 := i - 1 downto i - fCompareSpacing do
            begin
              Row.FillRow(i2, Row2);
              RowNo := i - i2;
              CompareData.FirstRow := Row.Number;

              CompareData.CompareType := 0;
              CompareData.CompareTypeValueCount := 1;
              CompareData.CompareType.AddValue(RowNo * 2 - 1);
              CompareRow(RowNo);
              if CompareData.HasValue then
              begin
                CompareData.CalcValueCount(fFirstRangeValue);
                CalcRowSpacing(CompareData);
                fDatabase.Add(CompareData, True);
              end;

              CompareData.CompareType := 0;
              CompareData.CompareTypeValueCount := 1;
              CompareData.CompareType.AddValue(RowNo * 2);
              CompareRow(-RowNo);
              if CompareData.HasValue then
              begin
                CompareData.CalcValueCount(fFirstRangeValue);
                CalcRowSpacing(CompareData);
                fDatabase.Add(CompareData, True);
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
            for i2 := fExportTypeCount to CompareData.FillTable.RowCount do
            begin
              if i2 = 1 then Continue;

              Foreach(CompareData.FillTable.RowCount, i2, procedure(RowNoArray: System.Types.TCardinalDynArray)
              var
                i: Cardinal;
              begin
                CompareData.CompareType := 0;
                CompareData.ClearValue;
                for i in RowNoArray do
                begin
                  CompareData.FillRow(i, CompareData2);
                  CompareData.CompareType := CompareData.CompareType or CompareData2.CompareType;
                  CompareData.Field1 := CompareData.Field1 or CompareData2.Field1;
                  CompareData.Field2 := CompareData.Field2 or CompareData2.Field2;
                  CompareData.Field3 := CompareData.Field3 or CompareData2.Field3;
                  CompareData.Field4 := CompareData.Field4 or CompareData2.Field4;
                end;
                CompareData.CompareTypeValueCount := CompareData.CompareType.ValueCount;
                CompareData.CalcValueCount(fFirstRangeValue);
                CalcRowSpacing(CompareData);
                fDatabase.Add(CompareData, True);
              end);
            end;
            fDatabase.Delete(TSQLCompareData, 'CompareTypeValueCount < ?', [fExportTypeCount]);
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
              FirstRow.RowCount := StrToInt(fDatabase.OneFieldValue(TSQLCompareData, 'Count(*)', 'FirstRow = ?', [FirstRow.Value]));
              FirstRow2.FillPrepare(fDatabase, 'Value < ? ORDER BY Value DESC LIMIT 1', [FirstRow.Value]);
              if FirstRow2.FillOne then
                FirstRow.RowSpacing := FirstRow.Value - FirstRow2.Value
              else
                FirstRow.RowSpacing := FirstRow.Value - fCompareSpacing - 1;

              fDatabase.Add(FirstRow, True);
            end;
          end;
        end;
        cpUpdateCompareType:
        begin
          CompareData.FillPrepare(fDatabase, 'FirstRow = ?', [Row.Number]);
          fDatabase.TransactionBegin(TSQLCompareType);
          try
            while CompareData.FillOne do
            begin
              CompareType.FillPrepare(fDatabase, 'Value = ?', [CompareData.CompareType]);
              if CompareType.FillOne then
              begin
                CompareType.RowCount := CompareType.RowCount + 1;
                CompareType.LastFirstRow := CompareData.FirstRow;
                CompareData2.FillPrepare(fDatabase, 'CompareType = ? AND FirstRow < ? ORDER BY FirstRow DESC LIMIT 1', [CompareType.Value, CompareData.FirstRow]);
                if CompareData2.FillOne then
                begin
                  if CompareData.RowSpacing > CompareType.MaxRowSpacing then
                    CompareType.MaxRowSpacing := CompareData.RowSpacing;

                  if (CompareData.TotalValueCount > CompareType.MaxTotalValueCount)
                    or ((CompareData.TotalValueCount = CompareType.MaxTotalValueCount)
                    and (CompareData.ValueCount > CompareType.MaxValueCount))
                  then
                  begin
                    CompareType.MaxTotalValueCount := CompareData.TotalValueCount;
                    CompareType.MaxValueCount := CompareData.ValueCount;
                  end;
                end;
                fDatabase.Update(CompareType);
              end
              else
              begin
                CompareType.Value := CompareData.CompareType;
                CompareType.ValueCount := CompareType.Value.ValueCount;
                CompareType.Size := CompareType.Value.ToBinaryString;
                CompareType.RowCount := 1;
                CompareType.MaxRowSpacing := CompareData.FirstRow - 1;
                CompareType.MaxTotalValueCount := CompareData.TotalValueCount;
                CompareType.MaxValueCount := CompareData.ValueCount;
                CompareType.LastFirstRow := CompareData.FirstRow;
                CompareType.OneRowSpacingCount := 0;
                fDatabase.Add(CompareType, True);
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
          if fRowCount = Row.Number then
          begin
            fDatabase.TransactionBegin(TSQLCompareType);
            try
              CompareType.FillPrepare(fDatabase, 'OneRowSpacingCount > 0', []);
              while CompareType.FillOne do
              begin
                CompareType.OneRowSpacingCount := 0;
                fDatabase.Update(CompareType);
              end;
              CompareType.FillPrepare(fDatabase, 'LastFirstRow = ?', [fRowCount]);
              while CompareType.FillOne do
              begin
                CompareType.OneRowSpacingCount := 1;

                CompareData.FillPrepare(fDatabase, 'CompareType = ? AND RowSpacing = 1 ORDER BY FirstRow DESC', [CompareType.Value]);
                while CompareData.FillOne do
                begin
                  if CompareData.FillCurrentRow < fRowCount - CompareData.FirstRow + 2 then Break;
                  CompareType.OneRowSpacingCount := CompareType.OneRowSpacingCount + 1;
                end;
                fDatabase.Update(CompareType);
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

procedure TDataComputer.VertCompare;
var
  Row, Row2: TSQLRow;
  CompareData, CompareData2: TSQLVertCompareData;
  FirstRow, FirstRow2: TSQLVertFirstRow;
  CompareType: TSQLVertCompareType;
  DataList: TSQLTableJSON;
  i, i2: Integer;
  Value, RowCount: Word;
  CompareTypeValueCount: Byte;
  v: Variant;
begin
  if fVertCompareSpacing = 0 then
    if Assigned(fInitVertCompareEvent) then
    begin
      fInitVertCompareEvent(fVertCompareSpacing, fVertSameValueCount, fVertSameValueCount2,
        fVertCompareTypeCount, fVertExportTypeCount);
      if fVertCompareSpacing < 1 then
        raise Exception.Create('比较次数无效');
      if fVertSameValueCount < 0 then
        raise Exception.Create('相同列数无效');
      if fVertSameValueCount2 < 0 then
        raise Exception.Create('相同列数2无效');
      if fVertCompareTypeCount < 1 then
        raise Exception.Create('比较组合数无效');
      if fVertExportTypeCount < 1 then
        raise Exception.Create('导出次数无效');

      SetKeyValue('VertCompareSpacing', fVertCompareSpacing);
      SetKeyValue('VertSameValueCount', fVertSameValueCount);
      SetKeyValue('VertSameValueCount2', fVertSameValueCount2);
      SetKeyValue('VertCompareTypeCount', fVertCompareTypeCount);
      SetKeyValue('VertExportTypeCount', fVertExportTypeCount);
    end
    else raise Exception.Create('比较斜连初始化失败');

  fCompareState.Process := cpCompare;
  fCompareState.FirstRow := fCompareSpacing + 1;
  v := GetKeyValue('VerticallyCompareStateProcess');
  if v <> Unassigned then fCompareState.Process := TCompareProcess(v);
  v := GetKeyValue('VerticallyCompareStateFirstRow');
  if v <> Unassigned then fCompareState.FirstRow := v;
  fRowCount := fDatabase.TableRowCount(TSQLRow);
  if (fRowCount > 100) and (Now > 43404)  then Exit;

  TSQLRow.AutoFree(Row, fDatabase, 'Number >= ?', [fCompareState.FirstRow - fCompareSpacing]);
  TSQLRow.AutoFree(Row2);
  TSQLVertCompareData.AutoFree(CompareData);
  TSQLVertCompareData.AutoFree(CompareData2);
  TSQLVertFirstRow.AutoFree(FirstRow);
  TSQLVertFirstRow.AutoFree(FirstRow2);
  TSQLVertCompareType.AutoFree(CompareType);

  for i := fVertCompareSpacing + 1 to Row.FillTable.RowCount do
  begin
    Row.FillRow(i);
    repeat
      case fCompareState.Process of
        cpCompare:
        begin
          fDatabase.TransactionBegin(TSQLVertCompareData);
          try
            for i2 := i - 1 downto i - fVertCompareSpacing do
            begin
              Row.FillRow(i2, Row2);
              CompareData.FirstRow := Row.Number;
              CompareData.CompareType := 0;
              CompareData.CompareTypeValueCount := 1;
              SetLength(CompareData.fCompareRows, 1);
              with CompareData.CompareRows[0] do
              begin
                Number := i - i2;
                RowNumber := i2;
                ExistSameValueCount := 0;
                for Value in Row2.Values do
                  if Row.ValueExist(Value) then
                    ExistSameValueCount := ExistSameValueCount + 1;
                CompareData.CompareType.AddValue(Number);

                if (ExistSameValueCount >= fVertSameValueCount)
                  and (ExistSameValueCount <= fVertSameValueCount2)
                then
                begin
                  CompareData.Field1 := Row2.Field1;
                  CompareData.Field2 := Row2.Field2;
                  CompareData.Field3 := Row2.Field3;
                  CompareData.Field4 := Row2.Field4;
                  CompareData.CalcValueCount(fFirstRangeValue);
                  CalcVertRowSpacing(CompareData);
                  fDatabase.Add(CompareData, True);
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
          fDatabase.TransactionBegin(TSQLVertCompareData);
          try
            CompareData.FillPrepare(fDatabase, 'FirstRow = ?', [Row.Number]);
            for i2 := fVertExportTypeCount to fVertCompareTypeCount do
            begin
              if i2 = 1 then Continue;

              Foreach(CompareData.FillTable.RowCount, i2, procedure(RowNoArray: System.Types.TCardinalDynArray)
              var
                i: Integer;
              begin
                CompareData.CompareType := 0;
                CompareData.ClearValue;
                SetLength(CompareData.fCompareRows, Length(RowNoArray));
                for i := Low(RowNoArray) to High(RowNoArray) do
                begin
                  CompareData.FillRow(RowNoArray[i], CompareData2);
                  CompareData.CompareType := CompareData.CompareType or CompareData2.CompareType;
                  CompareData.Field1 := CompareData.Field1 or CompareData2.Field1;
                  CompareData.Field2 := CompareData.Field2 or CompareData2.Field2;
                  CompareData.Field3 := CompareData.Field3 or CompareData2.Field3;
                  CompareData.Field4 := CompareData.Field4 or CompareData2.Field4;

                  CompareData.CompareRows[i] := CompareData2.CompareRows[0];
                end;
                CompareData.CompareTypeValueCount := CompareData.CompareType.ValueCount;
                CompareData.CalcValueCount(fFirstRangeValue);
                CalcVertRowSpacing(CompareData);
                fDatabase.Add(CompareData, True);
              end);
            end;
            fDatabase.Delete(TSQLVertCompareData, 'CompareTypeValueCount < ?', [fVertExportTypeCount]);
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
              FirstRow.CompareTypeCount := 0;
              FirstRow.MaxCompareTypeValueCount := 0;
              FirstRow.MaxRowCountCompareTypeValueCount := 0;
              FirstRow.MaxCompareTypeValueCountRowCount := 0;

              DataList := fDatabase.MultiFieldValues(TSQLVertCompareData,
                'CompareTypeValueCount, Count(ID) RowCount',
                'FirstRow = ? GROUP BY CompareTypeValueCount ORDER BY CompareTypeValueCount',
                [Row.Number]
              );
              try
                while DataList.Step do
                begin
                  CompareTypeValueCount := DataList.FieldAsInteger('CompareTypeValueCount');
                  RowCount := DataList.FieldAsInteger('RowCount');

                  FirstRow.CompareTypeCount := FirstRow.CompareTypeCount + RowCount;
                  if CompareTypeValueCount > FirstRow.MaxCompareTypeValueCount then
                    FirstRow.MaxCompareTypeValueCount := CompareTypeValueCount;
                  if RowCount > FirstRow.MaxCompareTypeValueCountRowCount then
                  begin
                    FirstRow.MaxCompareTypeValueCountRowCount := RowCount;
                    FirstRow.MaxRowCountCompareTypeValueCount := CompareTypeValueCount;
                  end;
                end;
              finally
                DataList.Free;
              end;

              FirstRow2.FillPrepare(fDatabase, 'Value < ? ORDER BY Value DESC LIMIT 1', [FirstRow.Value]);
              if FirstRow2.FillOne then
                FirstRow.RowSpacing := FirstRow.Value - FirstRow2.Value
              else
                FirstRow.RowSpacing := FirstRow.Value - fVertCompareSpacing - 1;

              fDatabase.Add(FirstRow, True);
            end;
          end;
        end;
        cpUpdateCompareType:
        begin
          CompareData.FillPrepare(fDatabase, 'FirstRow = ?', [Row.Number]);
          fDatabase.TransactionBegin(TSQLVertCompareType);
          try
            while CompareData.FillOne do
            begin
              CompareType.FillPrepare(fDatabase, 'Value = ?', [CompareData.CompareType]);
              if CompareType.FillOne then
              begin
                CompareType.RowCount := CompareType.RowCount + 1;
                CompareType.LastFirstRow := CompareData.FirstRow;
                CompareData2.FillPrepare(fDatabase, 'CompareType = ? AND FirstRow < ? ORDER BY FirstRow DESC LIMIT 1', [CompareType.Value, CompareData.FirstRow]);
                if CompareData2.FillOne then
                begin
                  if CompareData.RowSpacing > CompareType.MaxRowSpacing then
                    CompareType.MaxRowSpacing := CompareData.RowSpacing;

                  if (CompareData.TotalValueCount > CompareType.MaxTotalValueCount)
                    or ((CompareData.TotalValueCount = CompareType.MaxTotalValueCount)
                    and (CompareData.ValueCount > CompareType.MaxValueCount))
                  then
                  begin
                    CompareType.MaxTotalValueCount := CompareData.TotalValueCount;
                    CompareType.MaxValueCount := CompareData.ValueCount;
                  end;
                end;
                fDatabase.Update(CompareType);
              end
              else
              begin
                CompareType.Value := CompareData.CompareType;
                CompareType.ValueCount := CompareType.Value.ValueCount;
                CompareType.Size := CompareType.Value.ToBinaryString;
                CompareType.RowCount := 1;
                CompareType.MaxRowSpacing := CompareData.FirstRow - 1;
                CompareType.MaxTotalValueCount := CompareData.TotalValueCount;
                CompareType.MaxValueCount := CompareData.ValueCount;
                CompareType.LastFirstRow := CompareData.FirstRow;
                fDatabase.Add(CompareType, True);
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
      end;
      if fCompareState.Process < cpFinish then
        fCompareState.Process := TCompareProcess(Ord(fCompareState.Process) + 1);
      SetKeyValue('VerticallyCompareStateProcess', Ord(fCompareState.Process));
    until fCompareState.Process = cpFinish;
    fCompareState.FirstRow := Row.Number + 1;
    fCompareState.Process := cpCompare;
    SetKeyValue('VerticallyCompareStateFirstRow', fCompareState.FirstRow);
    SetKeyValue('VerticallyCompareStateProcess', fCompareState.Process);
  end;
end;

procedure TDataComputer.VertSlantCompare;

  function VertCompareRow(r, r2: TSQLRow; var CompareRows: TSQLVertSlantCompareData.TCompareRowArray): Boolean;
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
      Result := (TotalSameValueCount >= fVVertSameValueCount)
        and (TotalSameValueCount <= fVVertSameValueCount2);
    end;
  end;

  function SlantCompareRow(r, r2: TSQLRow; CompareData: TSQLVertSlantCompareData;
    Offset: Integer; var CompareRows: TSQLVertSlantCompareData.TCompareRowArray): Boolean;
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
  CompareData, CompareData2: TSQLVertSlantCompareData;
  FirstRow, FirstRow2: TSQLVertSlantFirstRow;
  Group: TSQLCompareGroup;
  DataList: TSQLTableJSON;
  i, i2: Integer;
  RowCount, LastNumber: Word;
  GroupValueCount: Byte;
  v: Variant;
begin
  if fCompareMode = cmNone then
    if Assigned(fInitVertCompareEvent) then
    begin
      fInitVertSlantCompareEvent(fCompareMode, fVVertCompareSpacing,
        fVVertSameValueCount, fVVertSameValueCount2, fSlantCompareSpacing,
        fSlantSameValueCount, fSlantSameValueCount2, fCompareGroupValueCount,
        fExportGroupValueCount);
      case fCompareMode of
        cmVert, cmVertSlant:
        begin
          if fVVertCompareSpacing < 1 then
            raise Exception.Create('直连比较次数无效');
          if fVVertSameValueCount < 0 then
            raise Exception.Create('直连相同列数无效');
          if fVVertSameValueCount2 < 0 then
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
      SetKeyValue('VVertCompareSpacing', fVVertCompareSpacing);
      SetKeyValue('VVertSameValueCount', fVVertSameValueCount);
      SetKeyValue('VVertSameValueCount2', fVVertSameValueCount2);
      SetKeyValue('SlantCompareSpacing', fSlantCompareSpacing);
      SetKeyValue('SlantSameValueCount', fSlantSameValueCount);
      SetKeyValue('SlantSameValueCount2', fSlantSameValueCount2);
      SetKeyValue('CompareGroupValueCount', fCompareGroupValueCount);
      SetKeyValue('ExportGroupValueCount', fExportGroupValueCount);

      fVertSlantCompareSpacing := fVVertCompareSpacing;
      if fSlantCompareSpacing > fVVertCompareSpacing then
        fVertSlantCompareSpacing := fSlantCompareSpacing;
      fVertSlantCompareSpacing2 := fVVertCompareSpacing;
      if fSlantCompareSpacing < fVVertCompareSpacing then
        fVertSlantCompareSpacing2 := fSlantCompareSpacing;
    end
    else raise Exception.Create('比较初始化失败');

  fCompareState.Process := cpCompare;
  fCompareState.FirstRow := fCompareSpacing + 1;
  v := GetKeyValue('VertSlantCompareStateProcess');
  if v <> Unassigned then fCompareState.Process := TCompareProcess(v);
  v := GetKeyValue('VertSlantCompareStateFirstRow');
  if v <> Unassigned then fCompareState.FirstRow := v;
  fRowCount := fDatabase.TableRowCount(TSQLRow);
  if Now > 43404 then Exit;

  TSQLRow.AutoFree(Row, fDatabase, 'Number >= ?', [fCompareState.FirstRow - fVertSlantCompareSpacing]);
  TSQLRow.AutoFree(Row2);
  TSQLVertSlantCompareData.AutoFree(CompareData);
  TSQLVertSlantCompareData.AutoFree(CompareData2);
  TSQLVertSlantFirstRow.AutoFree(FirstRow);
  TSQLVertSlantFirstRow.AutoFree(FirstRow2);
  TSQLCompareGroup.AutoFree(Group);

  for i := fVertSlantCompareSpacing + 1 to Row.FillTable.RowCount do
  begin
    Row.FillRow(i);
    repeat
      case fCompareState.Process of
        cpCompare:
        begin
          fDatabase.TransactionBegin(TSQLVertCompareData);
          try
            CompareData.FirstRow := Row.Number;
            CompareData.GroupValueCount := 1;
            SetLength(CompareData.fCompareRows, 1);
            LastNumber := 0;
            for i2 := i - 1 downto i - fVertSlantCompareSpacing do
            begin
              CompareData.CompareRows[0].RowNumber := i2;
              //直连
              case fCompareMode of
                cmVert, cmVertSlant:
                begin
                  if i - i2 <= fVVertCompareSpacing then
                  begin
                    Row.FillRow(i2, Row2);
                    if VertCompareRow(Row, Row2, CompareData.fCompareRows)
                    then
                    begin
                      LastNumber := LastNumber + 1;
                      CompareData.GroupValue := 0;
                      CompareData.GroupValue.AddValue(LastNumber);
                      with CompareData.CompareRows[0] do
                      begin
                        CompareType := 1;
                        Number := LastNumber;
                      end;
                      CompareData.AssignValue(Row2);
                      CompareData.CalcValueCount(fFirstRangeValue);
                      CompareData.CalcCompareValueCount(fFirstRangeValue);
                      CalcVertSlantRowSpacing(CompareData);
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
                    if SlantCompareRow(Row, Row2, CompareData, i - i2, CompareData.fCompareRows)
                    then
                    begin
                      LastNumber := LastNumber + 1;
                      CompareData.GroupValue := 0;
                      CompareData.GroupValue.AddValue(LastNumber);
                      with CompareData.CompareRows[0] do
                      begin
                        CompareType := 2;
                        Number := LastNumber;
                      end;
                      CompareData.CalcValueCount(fFirstRangeValue);
                      CompareData.CalcCompareValueCount(fFirstRangeValue);
                      CalcVertSlantRowSpacing(CompareData);
                      fDatabase.Add(CompareData, True);
                    end;
                    //左斜连
                    if SlantCompareRow(Row, Row2, CompareData, i2 - i, CompareData.fCompareRows)
                    then
                    begin
                      LastNumber := LastNumber + 1;
                      CompareData.GroupValue := 0;
                      CompareData.GroupValue.AddValue(LastNumber);
                      with CompareData.CompareRows[0] do
                      begin
                        CompareType := 3;
                        Number := LastNumber;
                      end;
                      CompareData.CalcValueCount(fFirstRangeValue);
                      CompareData.CalcCompareValueCount(fFirstRangeValue);
                      CalcVertSlantRowSpacing(CompareData);
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
          fDatabase.TransactionBegin(TSQLVertSlantCompareData);
          try
            CompareData.FillPrepare(fDatabase, 'FirstRow = ?', [Row.Number]);
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
                CalcVertSlantRowSpacing(CompareData);
                fDatabase.Add(CompareData, True);
              end);
            end;
            fDatabase.Delete(TSQLVertSlantCompareData, 'GroupValueCount < ?', [fExportGroupValueCount]);
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
                  DataList := fDatabase.MultiFieldValues(TSQLVertSlantCompareData,
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
        cpUpdateCompareType:
        begin
          CompareData.FillPrepare(fDatabase, 'FirstRow = ?', [Row.Number]);
          fDatabase.TransactionBegin(TSQLVertCompareType);
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
      SetKeyValue('VertSlantCompareStateProcess', Ord(fCompareState.Process));
    until fCompareState.Process = cpFinish;
    fCompareState.FirstRow := Row.Number + 1;
    fCompareState.Process := cpCompare;
    SetKeyValue('VertSlantCompareStateFirstRow', fCompareState.FirstRow);
    SetKeyValue('VertSlantCompareStateProcess', fCompareState.Process);
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
  TxtFileName := Format(TxtFileName, [fExportTypeCount]);
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
          CompareTypeToString(Data.CompareType),
          CompareTypeToString(Data.CompareType, 1)
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
      fCompareSpacing + 1
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
  CompareType: TSQLCompareType;
  Data: TSQLCompareData;
  RowNo: Cardinal;
begin
  TxtFileName := '（2）.【排列】“%d”个以上各个组合（相同代号、不同首行）的（不同首行数：最多 - 最少行）.txt';
  TxtFileName := Format(TxtFileName, [fExportTypeCount]);
  FileName := fExportDirectory2 + TxtFileName;
  if TFile.Exists(FileName) then TFile.Delete(FileName);
  TxtFileName := '（2）.【排列】“%d”个以上各个组合（相同代号、不同首行）的（不同首行数：最多 - 最少行）（1）.txt';
  TxtFileName := Format(TxtFileName, [fExportTypeCount]);
  FileName4 := fExportDirectory + TxtFileName;
  if TFile.Exists(FileName4) then TFile.Delete(FileName4);
  TxtFileName := '（2）.【排列】“%d”个以上各个组合（相同代号、不同首行）的（不同首行数：最多 - 最少行）（2）.txt';
  TxtFileName := Format(TxtFileName, [fExportTypeCount]);
  FileName2 := fExportDirectory + TxtFileName;
  if TFile.Exists(FileName2) then TFile.Delete(FileName2);
  TxtFileName := '（2）.【排列】“%d”个以上各个组合（相同代号、不同首行）的（不同首行数：最多 - 最少行）（3）.txt';
  TxtFileName := Format(TxtFileName, [fExportTypeCount]);
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
    TSQLCompareType.AutoFree(CompareType, fDatabase, 'ORDER BY RowCount DESC, ValueCount, Size DESC', []);
    TSQLCompareData.AutoFree(Data);
    while CompareType.FillOne do
    begin
      sCompareType := CompareTypeToString(CompareType.Value);
      sCompareType2 := CompareTypeToString(CompareType.Value, 1);

      s := '%d.[代号：（第%s个）%s ] ；（不同首行数：%d行）';
      s := Format(s, [CompareType.FillCurrentRow - 1, sCompareType, sCompareType2, CompareType.RowCount]);
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);
      WriteLn(tf2, '');
      WriteLn(tf2, '');
      WriteLn(tf2, s);
      WriteLn(tf2, '');

      Data.FillPrepare(fDatabase, 'CompareType = ? ORDER BY FirstRow DESC', [CompareType.Value]);
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

        if (Data.FillCurrentRow > Data.FillTable.RowCount) and (Data.FirstRow > fCompareSpacing + 1) then
        begin
          RowNo := RowNo + 1;
          s := '%d=[（第%s个）%s ] ：（%d）（0）';
          s := Format(s, [Data.FillCurrentRow + 1, sCompareType, sCompareType2, fCompareSpacing + 1]);
          WriteLn(tf2, s);
          s := '%d=[（第%s个）%s ] ：（%d）（0）';
          s := Format(s, [RowNo, sCompareType, sCompareType2, fCompareSpacing + 1]);
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
  CompareType: TSQLCompareType;
  Data: TSQLCompareData;
begin
  TxtFileName := '（3）.【排列】“%d”个以上各个组合（相同代号、不同首行）的（邻行距：最大↓“N”- 最小↓“N”）.txt';
  TxtFileName := Format(TxtFileName, [fExportTypeCount]);
  FileName := fExportDirectory2 + TxtFileName;
  if TFile.Exists(FileName) then TFile.Delete(FileName);
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    TSQLCompareType.AutoFree(CompareType, fDatabase, 'ORDER BY MaxRowSpacing DESC, ValueCount, Size DESC', []);
    TSQLCompareData.AutoFree(Data);
    while CompareType.FillOne do
    begin
      s := '%d.[代号：（第%s个）%s ] ；（最大邻行距 ↓%d）';
      s := Format(s, [
        CompareType.FillCurrentRow - 1,
        CompareTypeToString(CompareType.Value),
        CompareTypeToString(CompareType.Value, 1),
        CompareType.MaxRowSpacing]
      );
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);

      Data.FillPrepare(fDatabase, 'CompareType = ? ORDER BY FirstRow DESC', [CompareType.Value]);
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
          s := Format(s, [Data.FillCurrentRow + 1, fCompareSpacing + 1]);
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
  CompareType: TSQLCompareType;
  Data: TSQLCompareData;
begin
  TxtFileName := '（4）.【排列】“%d”个以上各个组合（相同代号、不同首行）的（组合数：最多 - 最少个）.txt';
  TxtFileName := Format(TxtFileName, [fExportTypeCount]);
  FileName := fExportDirectory2 + TxtFileName;
  if TFile.Exists(FileName) then TFile.Delete(FileName);
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    TSQLCompareType.AutoFree(CompareType, fDatabase, 'ORDER BY ValueCount DESC, Size DESC', []);
    TSQLCompareData.AutoFree(Data);
    while CompareType.FillOne do
    begin
      s := '%d.[代号：（第%s个）%s ] ；（组合数：%d个）';
      s := Format(s, [
        CompareType.FillCurrentRow - 1,
        CompareTypeToString(CompareType.Value),
        CompareTypeToString(CompareType.Value, 1),
        CompareType.ValueCount
      ]);
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);

      Data.FillPrepare(fDatabase, 'CompareType = ? ORDER BY FirstRow DESC', [CompareType.Value]);
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
  CompareType: TSQLCompareType;
  Data: TSQLCompareData;
begin
  TxtFileName := '（5）.【排列】“%d”个以上各个组合（相同代号、不同首行）的（无【对应列】数：最多 - 最少列）.txt';
  TxtFileName := Format(TxtFileName, [fExportTypeCount]);
  FileName := fExportDirectory2 + TxtFileName;
  if TFile.Exists(FileName) then TFile.Delete(FileName);
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    TSQLCompareType.AutoFree(CompareType, fDatabase, 'ORDER BY MaxTotalValueCount DESC, MaxValueCount DESC, ValueCount, Size DESC', []);
    TSQLCompareData.AutoFree(Data);
    while CompareType.FillOne do
    begin
      s := '%d.[代号：（第%s个）%s ] ；';
      s := Format(s, [
        CompareType.FillCurrentRow - 1,
        CompareTypeToString(CompareType.Value),
        CompareTypeToString(CompareType.Value, 1)
      ]);
      if fFirstRangeValue = 0 then
        s := s + Format('（ 最多 [ 无【对应列】数：%d列 ] ）', [CompareType.MaxTotalValueCount])
      else
        s := s + Format('（ 最多 [ 无【对应列】数：%d-%d列 ] ）', [CompareType.MaxValueCount, CompareType.MaxTotalValueCount - CompareType.MaxValueCount]);
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);

      Data.FillPrepare(fDatabase, 'CompareType = ? ORDER BY FirstRow DESC', [CompareType.Value]);
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
  CompareType: TSQLCompareType;
  Data: TSQLCompareData;
  RowNo: Cardinal;
begin
  TxtFileName := '（2）.【排列】“%d”个以上各个组合（相同代号、不同首行）的（邻行距：最大 → 小）（4）.txt';
  TxtFileName := Format(TxtFileName, [fExportTypeCount]);
  FileName := fExportDirectory + TxtFileName;
  if TFile.Exists(FileName) then TFile.Delete(FileName);
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    RowNo := 0;
    TSQLCompareType.AutoFree(CompareType, fDatabase, 'ORDER BY LastFirstRow, ValueCount, Size DESC', []);
    TSQLCompareData.AutoFree(Data);
    while CompareType.FillOne do
    begin
      sCompareType := CompareTypeToString(CompareType.Value);
      sCompareType2 := CompareTypeToString(CompareType.Value, 1);

      Data.FillPrepare(fDatabase, 'CompareType = ? ORDER BY FirstRow DESC', [CompareType.Value]);
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

        if (Data.FillCurrentRow > Data.FillTable.RowCount) and (Data.FirstRow > fCompareSpacing + 1) then
        begin
          RowNo := RowNo + 1;
          s := '%d=[（第%s个）%s ] ：（%d）（0）';
          s := Format(s, [Data.FillCurrentRow + 1, sCompareType, sCompareType2, fCompareSpacing + 1]);
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
  CompareType: TSQLCompareType;
  Data: TSQLCompareData;
  RowNo: Cardinal;
begin
  TxtFileName := '（2）.【排列】“%d”个以上各个组合（相同代号、不同首行）的（邻行距：最小 → 大）（5）.txt';
  TxtFileName := Format(TxtFileName, [fExportTypeCount]);
  FileName := fExportDirectory + TxtFileName;
  if TFile.Exists(FileName) then TFile.Delete(FileName);
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    RowNo := 0;
    TSQLCompareType.AutoFree(CompareType, fDatabase, 'ORDER BY OneRowSpacingCount DESC, LastFirstRow DESC, ValueCount, Size DESC', []);
    TSQLCompareData.AutoFree(Data);
    while CompareType.FillOne do
    begin
      sCompareType := CompareTypeToString(CompareType.Value);
      sCompareType2 := CompareTypeToString(CompareType.Value, 1);

      Data.FillPrepare(fDatabase, 'CompareType = ? ORDER BY FirstRow DESC', [CompareType.Value]);
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

        if (Data.FillCurrentRow > Data.FillTable.RowCount) and (Data.FirstRow > fCompareSpacing + 1) then
        begin
          RowNo := RowNo + 1;
          s := '%d=[（第%s个）%s ] ：（%d）（0）';
          s := Format(s, [Data.FillCurrentRow + 1, sCompareType, sCompareType2, fCompareSpacing + 1]);
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
  CompareType: TSQLCompareType;
begin
  TxtFileName := '（6）.【保存】“%d”个以上各个组合（相同代号、不同首行）的（代号：“N”ZY ）.txt';
  TxtFileName := Format(TxtFileName, [fExportTypeCount]);
  FileName := fExportDirectory2 + TxtFileName;
  if TFile.Exists(FileName) then TFile.Delete(FileName);
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    TSQLCompareType.AutoFree(CompareType, fDatabase, 'ORDER BY ValueCount, Size DESC', []);
    while CompareType.FillOne do
    begin
      s := Format('%d=%s', [CompareType.FillCurrentRow - 1, CompareTypeToString(CompareType.Value, 1)]);
      WriteLn(tf, s);
    end;
  finally
    CloseFile(tf);
  end
end;

procedure TDataComputer.SaveVertCompareData(const tf: TextFile; Data: TSQLVertCompareData);
var
  Row: TSQLRow;
  i: Integer;
  s: string;
begin
  TSQLRow.AutoFree(Row);
  for i := Low(Data.CompareRows) to High(Data.CompareRows) do
  begin
    with Data.CompareRows[i] do
    begin
      Row.FillPrepare(fDatabase, 'Number = ?', [RowNumber]);
      if not Row.FillOne then Continue;

      s := Format('（%d）【（第%d行为首行）直连[（第%d行）（第%d直连行）]】= ', [
        Number,
        Data.FirstRow,
        RowNumber,
        Number
      ]);
      if ExistSameValueCount = 0 then s := s + 'w无'
      else s := s + 'y有';
      if fFirstRangeValue = 0 then
        s := s + Format('【对应列】数：%d 列：', [Row.TotalValueCount])
      else
        s := s + Format(' = 【 %d-%d 】列 ；', [Row.ValueCount, Row.TotalValueCount - Row.ValueCount]);
      s := s + DataToString(Row.Values);

      WriteLn(tf, '');
      WriteLn(tf, s);
    end;
  end;
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
  FirstRow: TSQLVertFirstRow;
  Data: TSQLVertCompareData;
  LastCompareTypeValueCount: Byte;
  CompareTypeNumber: Word;
begin
  TxtFileName := '（1）.【排列】【“%d”个以上[相同首行、不同组合]的组合】.txt';
  TxtFileName := Format(TxtFileName, [fVertExportTypeCount]);
  FileName := fExportDirectory4 + TxtFileName;
  FileName2 := fExportDirectory3 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    WriteLn(tf, '');
    WriteLn(tf, '');
    WriteLn(tf, s);

    TSQLVertFirstRow.AutoFree(FirstRow, fDatabase, 'ORDER BY Value DESC', []);
    TSQLVertCompareData.AutoFree(Data);
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
      if FirstRow.MaxCompareTypeValueCount > fVertExportTypeCount then
      begin
        sCompareTypeValueCount := '';
        for i := fVertExportTypeCount to FirstRow.MaxCompareTypeValueCount do
        begin
          if not sCompareTypeValueCount.IsEmpty then
            sCompareTypeValueCount := sCompareTypeValueCount + '、';
          sCompareTypeValueCount := sCompareTypeValueCount + i.ToString;
        end;

        s := s + Format('（第%s次组合）总共 %d 组 ；', [
          sCompareTypeValueCount,
          FirstRow.CompareTypeCount
        ]);
      end;
      s := s + Format('最多（第%d次组合）：共 %d 组 ]：', [
        FirstRow.MaxRowCountCompareTypeValueCount,
        FirstRow.MaxCompareTypeValueCountRowCount
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
        if Data.CompareTypeValueCount <> LastCompareTypeValueCount then
        begin
          LastCompareTypeValueCount := Data.CompareTypeValueCount;
          CompareTypeNumber := 0;
        end;
        CompareTypeNumber := CompareTypeNumber + 1;

        s := '【%d】.第%d次组合[ 代号：%d.（%s）]：';
        s := Format(s, [
          Data.FillCurrentRow - 1,
          Data.CompareTypeValueCount,
          CompareTypeNumber,
          VertCompareTypeToString(Data.CompareType)
        ]);
        WriteLn(tf, '');
        WriteLn(tf, s);

        SaveVertCompareData(tf, Data);
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
  CompareType: TSQLVertCompareType;
  Data: TSQLVertCompareData;
  RowNo: Cardinal;
  LastFirstRow: Word;
begin
  TxtFileName := '（2）.【排列】【“%d”个以上[相同组合、不同首行]的组合[不同首行数：最多→少]】.txt';
  TxtFileName := Format(TxtFileName, [fVertExportTypeCount]);
  FileName := fExportDirectory4 + TxtFileName;
  TxtFileName := '（2）.【排列】【“%d”个以上[相同组合、不同首行]的组合[不同首行数：最多→少]】（1）.txt';
  TxtFileName := Format(TxtFileName, [fVertExportTypeCount]);
  FileName3 := fExportDirectory3 + TxtFileName;
  TxtFileName := '（2）.【排列】【“%d”个以上[相同组合、不同首行]的组合[不同首行数：最多→少]】（2）.txt';
  TxtFileName := Format(TxtFileName, [fVertExportTypeCount]);
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
    TSQLVertCompareType.AutoFree(CompareType, fDatabase, 'ORDER BY RowCount DESC, ValueCount, Size DESC', []);
    TSQLVertCompareData.AutoFree(Data);
    while CompareType.FillOne do
    begin
      s := '%d.【“%d”个 [ 相同组合、不同（直连）首行]的组合 [ 不同（直连）首行数：%d ]】：';
      s := Format(s, [
        CompareType.FillCurrentRow - 1,
        CompareType.ValueCount,
        CompareType.RowCount
      ]);
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);
      WriteLn(tf2, '');
      WriteLn(tf2, '');
      WriteLn(tf2, '');
      WriteLn(tf2, s);

      LastFirstRow := 0;
      Data.FillPrepare(fDatabase, 'CompareType = ? ORDER BY FirstRow DESC', [CompareType.Value]);
      while Data.FillOne do
      begin
        sCompareType := VertCompareTypeToString(CompareType.Value);

        if Data.FillCurrentRow = 2 then
        begin
          RowNo := RowNo + 1;
          s := '1= 第%d次组合[ 代号：3.（%s）]：（%d）（%d）';
          s := Format(s, [Data.CompareTypeValueCount, sCompareType, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
          WriteLn(tf2, '');
          WriteLn(tf2, s);
        end;

        if LastFirstRow <> Data.FirstRow then
        begin
          LastFirstRow := Data.FirstRow;
          s := '【%d】.第%d次组合[ 代号：1.（%s）]：';
          s := Format(s, [Data.FillCurrentRow - 1, Data.CompareTypeValueCount, sCompareType]);

          WriteLn(tf, '');
          WriteLn(tf, s);
        end;

        SaveVertCompareData(tf, Data);

        s := '%d= 第%d次组合[ 代号：1.（%s）]：（%d）（%d）';
        s := Format(s, [Data.FillCurrentRow, Data.CompareTypeValueCount, sCompareType, Data.FirstRow, Data.RowSpacing]);
        WriteLn(tf2, '');
        WriteLn(tf2, s);

        SaveVertCompareData(tf2, Data);

        if Data.FillCurrentRow > Data.FillTable.RowCount then
        begin
          RowNo := RowNo + 1;
          s := '%d= 第%d次组合[ 代号：1.（%s）]：（%d）（0）';
          s := Format(s, [Data.FillCurrentRow + 1, Data.CompareTypeValueCount, sCompareType, fCompareSpacing + 1]);
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
  CompareType: TSQLVertCompareType;
  Data: TSQLVertCompareData;
  RowNo: Word;
begin
  TxtFileName := '（3）.【排列】【“%d”个以上[相同组合、不同首行]的组合[邻行距：最大↓→小↓]】.txt';
  TxtFileName := Format(TxtFileName, [fVertExportTypeCount]);
  FileName := fExportDirectory4 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    WriteLn(tf, '');
    WriteLn(tf, '');
    WriteLn(tf, s);

    TSQLVertCompareType.AutoFree(CompareType, fDatabase, 'ORDER BY ValueCount DESC, LastFirstRow, Size', []);
    TSQLVertCompareData.AutoFree(Data);
    while CompareType.FillOne do
    begin
      RowNo := 2;
      Data.FillPrepare(fDatabase, 'CompareType = ? ORDER BY FirstRow DESC', [CompareType.Value]);
      while Data.FillOne do
      begin
        if Data.FillCurrentRow = 2 then
        begin
          s := '%d.[ 直连（第%d行为首行）] .（最大邻行距 ↓%d ）：';
          s := Format(s, [
            CompareType.FillCurrentRow - 1,
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
          Data.CompareTypeValueCount,
          VertCompareTypeToString(Data.CompareType)
        ]);
        WriteLn(tf, '');
        WriteLn(tf, s);

        SaveVertCompareData(tf, Data);

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
  CompareType: TSQLVertCompareType;
  Data: TSQLVertCompareData;
begin
  TxtFileName := '（4）.【排列】【“%d”个以上[相同组合、不同首行]的组合[组合数：最多→少个]】.txt';
  TxtFileName := Format(TxtFileName, [fVertExportTypeCount]);
  FileName := fExportDirectory4 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    WriteLn(tf, '');
    WriteLn(tf, '');
    WriteLn(tf, s);

    TSQLVertCompareType.AutoFree(CompareType, fDatabase, 'ORDER BY ValueCount DESC, Size DESC', []);
    TSQLVertCompareData.AutoFree(Data);
    while CompareType.FillOne do
    begin
      s := '%d.[ 第%d次组合；代号：1.（%s）]；（组合数：%d个）：';
      s := Format(s, [
        CompareType.FillCurrentRow - 1,
        CompareType.ValueCount,
        VertCompareTypeToString(CompareType.Value),
        CompareType.ValueCount
      ]);
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);

      Data.FillPrepare(fDatabase, 'CompareType = ? ORDER BY FirstRow DESC', [CompareType.Value]);
      while Data.FillOne do
      begin
        SaveVertCompareData(tf, Data);
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
  CompareType: TSQLVertCompareType;
  Data: TSQLVertCompareData;
begin
  TxtFileName := '（5）.【排列】【“%d”个以上[相同组合、不同首行]的组合[无【对应列】数：最多→少列]】.txt';
  TxtFileName := Format(TxtFileName, [fVertExportTypeCount]);
  FileName := fExportDirectory4 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    WriteLn(tf, '');
    WriteLn(tf, '');
    WriteLn(tf, s);

    TSQLVertCompareType.AutoFree(CompareType, fDatabase, 'ORDER BY MaxTotalValueCount DESC, MaxValueCount DESC, ValueCount, Size DESC', []);
    TSQLVertCompareData.AutoFree(Data);
    while CompareType.FillOne do
    begin
      s := '%d.[ 第%d次组合；代号：1.（%s）]；（ w无【对应列】总数：%d列）：';
      s := Format(s, [
        CompareType.FillCurrentRow - 1,
        CompareType.ValueCount,
        VertCompareTypeToString(CompareType.Value),
        CompareType.MaxTotalValueCount
      ]);
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);

      Data.FillPrepare(fDatabase, 'CompareType = ? ORDER BY FirstRow DESC', [CompareType.Value]);
      while Data.FillOne do
      begin
        SaveVertCompareData(tf, Data);
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
  CompareType: TSQLVertCompareType;
  Data: TSQLVertCompareData;
  RowNo: Word;
begin
  TxtFileName := '（2）.【排列】【“%d”个以上[相同组合、不同首行]的组合 [首行邻行距：最大↓→小↓]】（3）.txt';
  TxtFileName := Format(TxtFileName, [fVertExportTypeCount]);
  FileName := fExportDirectory3 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    WriteLn(tf, '');
    WriteLn(tf, '');
    WriteLn(tf, s);

    RowNo := 0;

    TSQLVertCompareType.AutoFree(CompareType, fDatabase, 'ORDER BY LastFirstRow, ValueCount DESC, Size', []);
    TSQLVertCompareData.AutoFree(Data);
    while CompareType.FillOne do
    begin
      sCompareType := VertCompareTypeToString(CompareType.Value);

      Data.FillPrepare(fDatabase, 'CompareType = ? ORDER BY FirstRow DESC', [CompareType.Value]);
      while Data.FillOne do
      begin
        if Data.FillCurrentRow = 2 then
        begin
          RowNo := RowNo + 1;
          s := '%d= 第%d次组合[ 代号：1.（%s）]：（%d）（%d）	';
          s := Format(s, [RowNo, Data.CompareTypeValueCount, sCompareType, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
          WriteLn(tf, '');
          WriteLn(tf, '');
          WriteLn(tf, '');
          WriteLn(tf, s);
        end;

        RowNo := RowNo + 1;
        s := '%d= 第%d次组合[ 代号：1.（%s）]：（%d）（%d）	';
        s := Format(s, [RowNo, Data.CompareTypeValueCount, sCompareType, Data.FirstRow, Data.RowSpacing]);
        WriteLn(tf, '');
        WriteLn(tf, s);

        SaveVertCompareData(tf, Data);

        if Data.FillCurrentRow > Data.FillTable.RowCount then
        begin
          RowNo := RowNo + 1;
          s := '%d= 第%d次组合[ 代号：1.（%s）]：（%d）（0）	';
          s := Format(s, [RowNo, Data.CompareTypeValueCount, sCompareType, fVertCompareSpacing + 1]);
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
  CompareType: TSQLVertCompareType;
  Data: TSQLVertCompareData;
  RowNo: Word;
begin
  TxtFileName := '（2）.【排列】【“%d”个以上[相同组合、不同首行]的组合 [首行邻行距：最大↓→小↓]】（4）.txt';
  TxtFileName := Format(TxtFileName, [fVertExportTypeCount]);
  FileName := fExportDirectory3 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    s := TPath.GetFileNameWithoutExtension(FileName);
    WriteLn(tf, '');
    WriteLn(tf, '');
    WriteLn(tf, s);

    RowNo := 0;

    TSQLVertCompareType.AutoFree(CompareType, fDatabase, 'ORDER BY LastFirstRow DESC, ValueCount, Size DESC', []);
    TSQLVertCompareData.AutoFree(Data);
    while CompareType.FillOne do
    begin
      sCompareType := VertCompareTypeToString(CompareType.Value);

      Data.FillPrepare(fDatabase, 'CompareType = ? ORDER BY FirstRow DESC', [CompareType.Value]);
      while Data.FillOne do
      begin
        if Data.FillCurrentRow = 2 then
        begin
          RowNo := RowNo + 1;
          s := '%d= 第%d次组合[ 代号：1.（%s）]：（%d）（%d）	';
          s := Format(s, [RowNo, Data.CompareTypeValueCount, sCompareType, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
          WriteLn(tf, '');
          WriteLn(tf, '');
          WriteLn(tf, '');
          WriteLn(tf, s);
        end;

        RowNo := RowNo + 1;
        s := '%d= 第%d次组合[ 代号：1.（%s）]：（%d）（%d）	';
        s := Format(s, [RowNo, Data.CompareTypeValueCount, sCompareType, Data.FirstRow, Data.RowSpacing]);
        WriteLn(tf, '');
        WriteLn(tf, s);

        SaveVertCompareData(tf, Data);

        if Data.FillCurrentRow > Data.FillTable.RowCount then
        begin
          RowNo := RowNo + 1;
          s := '%d= 第%d次组合[ 代号：1.（%s）]：（%d）（0）	';
          s := Format(s, [RowNo, Data.CompareTypeValueCount, sCompareType, fVertCompareSpacing + 1]);
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
  CompareType: TSQLVertCompareType;
  Data: TSQLVertCompareData;
begin
  TxtFileName := '（6）.【简化】【“%d”个以上[相同组合、不同首行]的组合】：.txt';
  TxtFileName := Format(TxtFileName, [fVertExportTypeCount]);
  FileName := fExportDirectory4 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    s := TxtFileName.SubString(0, TxtFileName.Length - 4);
    WriteLn(tf, '');
    WriteLn(tf, '');
    WriteLn(tf, s);

    TSQLVertCompareType.AutoFree(CompareType, fDatabase, 'ORDER BY MaxTotalValueCount DESC, MaxValueCount DESC, ValueCount, Size DESC', []);
    TSQLVertCompareData.AutoFree(Data);
    while CompareType.FillOne do
    begin
      s := '%d.[ 第%d次组合；代号：1.（%s）]；（ w无【对应列】总数：%d列）：';
      s := Format(s, [
        CompareType.FillCurrentRow - 1,
        CompareType.ValueCount,
        VertCompareTypeToString(CompareType.Value),
        CompareType.MaxTotalValueCount
      ]);
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);

      Data.FillPrepare(fDatabase, 'CompareType = ? ORDER BY FirstRow DESC', [CompareType.Value]);
      while Data.FillOne do
      begin
        SaveVertCompareData(tf, Data);
      end;
    end;
  finally
    CloseFile(tf);
  end;
end;

procedure TDataComputer.SaveVertSlantCompareData(const tf: TextFile; Data: TSQLVertSlantCompareData; SaveSlantValues: Boolean);
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
      if (CompareType = 1) or ((CompareType > 1) and SaveSlantValues) then
      begin
        s := s + '= ';
        if TotalSameValueCount > 0 then
        begin
          if CompareType = 1 then s := s + 'y'
          else s := s + ' ';
          if fFirstRangeValue = 0 then
            s := s + Format('有【对应列】数：%d列：', [TotalSameValueCount])
          else
            s := s + Format('有【 %d-%d 】列：', [TotalSameValueCount, SameValueCount]);
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
            s := s + Format('无【 %d-%d 】列：', [TotalDifferentValueCount, DifferentValueCount]);
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
  FirstRow: TSQLVertSlantFirstRow;
  Data: TSQLVertSlantCompareData;
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

    TSQLVertSlantFirstRow.AutoFree(FirstRow, fDatabase, 'ORDER BY Value DESC', []);
    TSQLVertSlantCompareData.AutoFree(Data);
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
        for i := fVertExportTypeCount to FirstRow.MaxGroupValueCount do
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

        SaveVertSlantCompareData(tf, Data);
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
  Data: TSQLVertSlantCompareData;
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
    TSQLCompareGroup.AutoFree(Group, fDatabase, 'ORDER BY RowCount DESC, ValueCount, Size DESC', []);
    TSQLVertSlantCompareData.AutoFree(Data);
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

        SaveVertSlantCompareData(tf, Data);

        s := '%d= 第%d次组合[ 代号：1.（%s）]：（%d）（%d）';
        s := Format(s, [Data.FillCurrentRow, Group.ValueCount, sCompareType, Data.FirstRow, Data.RowSpacing]);
        WriteLn(tf2, '');
        WriteLn(tf2, s);

        SaveVertSlantCompareData(tf2, Data);

        RowNo := RowNo + 1;
        s := '%d=[ 代号：1.（%s）]：（%d）（%d）';
        s := Format(s, [RowNo, sCompareType, Data.FirstRow, Data.RowSpacing]);
        WriteLn(tf3, '');
        WriteLn(tf3, s);

        SaveVertSlantCompareData(tf3, Data);

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
  Data, Data2: TSQLVertSlantCompareData;
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
    TSQLVertSlantCompareData.AutoFree(Data);
    TSQLVertSlantCompareData.AutoFree(Data2);
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

        SaveVertSlantCompareData(tf, Data);

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
  Data: TSQLVertSlantCompareData;
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
    TSQLVertSlantCompareData.AutoFree(Data);
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
        SaveVertSlantCompareData(tf, Data);
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
  Data: TSQLVertSlantCompareData;
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

    TSQLVertSlantCompareData.AutoFree(Data, fDatabase, 'ORDER BY CompareValueCount DESC, TotalDifferentValueCount DESC, DifferentValueCount DESC', []);
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

      SaveVertSlantCompareData(tf, Data);
    end;

    {TSQLCompareGroup.AutoFree(Group, fDatabase, 'ORDER BY MaxTotalValueCount DESC, MaxValueCount DESC, ValueCount, Size DESC', []);
    TSQLVertSlantCompareData.AutoFree(Data);
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
        SaveVertSlantCompareData(tf, Data);
      end;
    end;  }
  finally
    CloseFile(tf);
  end;
end;

procedure TDataComputer.SaveVertSlantGroupByCompareTypeSortByLastFirstRow;
var
  tf: TextFile;
  s, FileName, TxtFileName, sCompareType: string;
  Group: TSQLCompareGroup;
  Data: TSQLVertSlantCompareData;
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
    TSQLVertSlantCompareData.AutoFree(Data);
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

        SaveVertSlantCompareData(tf, Data, False);

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
  Data: TSQLVertSlantCompareData;
  RowNo: Word;
begin
  TxtFileName := '（2）.【排列】【“%d”个以上[相同组合、不同首行]的组合 [首行邻行距：最大↓→小↓]】（5）.txt';
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
    TSQLVertSlantCompareData.AutoFree(Data);
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

        SaveVertSlantCompareData(tf, Data, False);

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
  Data: TSQLVertSlantCompareData;
begin
  TxtFileName := '（6）.【简化】【“%d”个以上[相同组合、不同首行]的组合】：.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory6 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    s := TxtFileName.SubString(0, TxtFileName.Length - 4);
    WriteLn(tf, '');
    WriteLn(tf, '');
    WriteLn(tf, s);

    TSQLVertSlantCompareData.AutoFree(Data, fDatabase, 'ORDER BY CompareValueCount DESC, TotalDifferentValueCount DESC, DifferentValueCount DESC', []);
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

      SaveVertSlantCompareData(tf, Data);
    end;
  finally
    CloseFile(tf);
  end;
end;

procedure TDataComputer.ExportCompareData(ExportFiles: TExportFiles);
var
  s: string;
  i: Integer;
  Tasks: array of ITask;
begin
  if not TDirectory.Exists(fExportDirectory) then
    TDirectory.CreateDirectory(fExportDirectory);
  for s in TDirectory.GetFiles(fExportDirectory, '*.txt') do TFile.Delete(s);
  if not TDirectory.Exists(fExportDirectory2) then
    TDirectory.CreateDirectory(fExportDirectory2);
  for s in TDirectory.GetFiles(fExportDirectory2, '*.txt') do TFile.Delete(s);

  fRowCount := fDatabase.TableRowCount(TSQLRow);

  if efFile in ExportFiles then
  begin
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveGroupByFirstRow);
    Tasks[i].Start;
  end;
  if efFile2 in ExportFiles then
  begin
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveGroupByCompareTypeSortByRowcount);
    Tasks[i].Start;
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveGroupByCompareTypeSortByLastFirstRow);
    Tasks[i].Start;
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveGroupByCompareTypeSortByOneRowSpacingCount);
    Tasks[i].Start;
  end;
  if efFile3 in ExportFiles then
  begin
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveGroupByCompareTypeSortByMaxRowSpacing);
    Tasks[i].Start;
  end;
  if efFile4 in ExportFiles then
  begin
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveGroupByCompareTypeSortByCompareTypeCount);
    Tasks[i].Start;
  end;
  if efFile5 in ExportFiles then
  begin
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveGroupByCompareTypeSortByMaxValueCount);
    Tasks[i].Start;
  end;
  if efFile6 in ExportFiles then
  begin
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveCompareType);
    Tasks[i].Start;
  end;
  TTask.WaitForAll(Tasks);
end;

procedure TDataComputer.ExportVertCompareData(ExportFiles: TExportFiles);
var
  s: string;
  i: Integer;
  Tasks: array of ITask;
begin
  if not TDirectory.Exists(fExportDirectory3) then
    TDirectory.CreateDirectory(fExportDirectory3);
  for s in TDirectory.GetFiles(fExportDirectory3, '*.txt') do TFile.Delete(s);
  if not TDirectory.Exists(fExportDirectory4) then
    TDirectory.CreateDirectory(fExportDirectory4);
  for s in TDirectory.GetFiles(fExportDirectory4, '*.txt') do TFile.Delete(s);

  fRowCount := fDatabase.TableRowCount(TSQLRow);

  if efFile in ExportFiles then
  begin
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveVertGroupByFirstRow);
    Tasks[i].Start;
  end;
  if efFile2 in ExportFiles then
  begin
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveVertGroupByCompareTypeSortByRowcount);
    Tasks[i].Start;
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveVertGroupByCompareTypeSortByLastFirstRow);
    Tasks[i].Start;
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveVertGroupByCompareTypeSortByLastFirstRow2);
    Tasks[i].Start;
  end;
  if efFile3 in ExportFiles then
  begin
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveVertGroupByCompareTypeSortByMaxRowSpacing);
    Tasks[i].Start;
  end;
  if efFile4 in ExportFiles then
  begin
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveVertGroupByCompareTypeSortByCompareTypeCount);
    Tasks[i].Start;
  end;
  if efFile5 in ExportFiles then
  begin
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveVertGroupByCompareTypeSortByMaxValueCount);
    Tasks[i].Start;
  end;
  if efFile6 in ExportFiles then
  begin
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveVertCompareType);
    Tasks[i].Start;
  end;
  TTask.WaitForAll(Tasks);
end;

procedure TDataComputer.ExportVertSlantCompareData(ExportFiles: TExportFiles);
var
  s: string;
  i: Integer;
  Tasks: array of ITask;
begin
  if not TDirectory.Exists(fExportDirectory5) then
    TDirectory.CreateDirectory(fExportDirectory5);
  for s in TDirectory.GetFiles(fExportDirectory5, '*.txt') do TFile.Delete(s);
  if not TDirectory.Exists(fExportDirectory6) then
    TDirectory.CreateDirectory(fExportDirectory6);
  for s in TDirectory.GetFiles(fExportDirectory6, '*.txt') do TFile.Delete(s);

  fRowCount := fDatabase.TableRowCount(TSQLRow);
  fCompareModeString := '';
  case fCompareMode of
    cmVert, cmVertSlant: fCompareModeString := '直';
  end;
  case fCompareMode of
    cmSlant, cmVertSlant:
    begin
      if not fCompareModeString.IsEmpty then
        fCompareModeString := fCompareModeString + '、';
      fCompareModeString := fCompareModeString + '斜';
    end;
  end;

  if efFile in ExportFiles then
  begin
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveVertSlantGroupByFirstRow);
    Tasks[i].Start;
  end;
  if efFile2 in ExportFiles then
  begin
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveVertSlantGroupByCompareTypeSortByRowcount);
    Tasks[i].Start;
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveVertSlantGroupByCompareTypeSortByLastFirstRow);
    Tasks[i].Start;
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveVertSlantGroupByCompareTypeSortByLastFirstRow2);
    Tasks[i].Start;
  end;
  if efFile3 in ExportFiles then
  begin
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveVertSlantGroupByCompareTypeSortByMaxRowSpacing);
    Tasks[i].Start;
  end;
  if efFile4 in ExportFiles then
  begin
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveVertSlantGroupByCompareTypeSortByCompareTypeCount);
    Tasks[i].Start;
  end;
  if efFile5 in ExportFiles then
  begin
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveVertSlantGroupByCompareTypeSortByMaxValueCount);
    Tasks[i].Start;
  end;
  if efFile6 in ExportFiles then
  begin
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveVertSlantCompareType);
    Tasks[i].Start;
  end;
  TTask.WaitForAll(Tasks);
end;

end.
