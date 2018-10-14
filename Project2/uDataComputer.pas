unit uDataComputer;

interface

uses
  mORMot,
  mORMotSQLite3,
  SynSQLite3Static,
  SynCommons,
  System.Classes,
  System.Variants,
  System.SysUtils,
  System.Types,
  System.IOUtils,
  System.Threading,
  System.Generics.Collections,
  uCommon,
  uFileWriter;

type
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
    fGroupValue: RawUTF8;
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
    procedure CalcCompareValueCount(aIntervalValues: TWordDynArray);
  published
    property FirstRow: Cardinal read fFirstRow write fFirstRow;
    property GroupValue: RawUTF8 read fGroupValue write fGroupValue;
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
    fValue: RawUTF8;
    fValue2: RawUTF8;
    fValueCount: Byte;
    fRowCount: Cardinal;
    fMaxRowSpacing: Cardinal;
    fMaxTotalValueCount: Word;
    fMaxValueCount: Word;
    fLastFirstRow: Cardinal;
    fOneRowSpacingCount: Cardinal;
    fEnabled: Boolean;
    fReEnabled: Boolean;
    fReEnabledFirstRow: Cardinal;
    fReEnabledFirstRow2: Cardinal;
    fReEnabledRowSpacing: Cardinal;
    fKeepMaxRowSpacing: Cardinal;
  public
    procedure BuildValue2;
  published
    property Value: RawUTF8 read fValue write fValue stored AS_UNIQUE;
    property Value2: RawUTF8 read fValue2 stored AS_UNIQUE;
    property ValueCount: Byte read fValueCount write fValueCount;
    property RowCount: Cardinal read fRowCount write fRowCount;
    property MaxRowSpacing: Cardinal read fMaxRowSpacing write fMaxRowSpacing;
    property MaxTotalValueCount: Word read fMaxTotalValueCount write fMaxTotalValueCount;
    property MaxValueCount: Word read fMaxValueCount write fMaxValueCount;
    property LastFirstRow: Cardinal read fLastFirstRow write fLastFirstRow;
    property OneRowSpacingCount: Cardinal read fOneRowSpacingCount write fOneRowSpacingCount;
    property Enabled: Boolean read fEnabled write fEnabled;
    property ReEnabled: Boolean read fReEnabled write fReEnabled;
    property ReEnabledFirstRow: Cardinal read fReEnabledFirstRow write fReEnabledFirstRow;
    property ReEnabledFirstRow2: Cardinal read fReEnabledFirstRow2 write fReEnabledFirstRow2;
    property ReEnabledRowSpacing: Cardinal read fReEnabledRowSpacing write fReEnabledRowSpacing;
    property KeepMaxRowSpacing: Cardinal read fKeepMaxRowSpacing write fKeepMaxRowSpacing;
  end;

  TDataComputer = class
  private type
    TCompareProcess = (cpCompare, cpGrouping, cpUpdateFirstRow, cpUpdateCompareGroup,
      cpUpdateGroupDataRowSpacing, cpUpdateGroupOneRowSpacingCount, cpFinish);
    TRecCompareState = record
      FirstRow: Word;
      Process: TCompareProcess;
    end;
  public type
    TCompareMode = (cmNone, cmVert, cmSlant, cmVertSlant);
    TCompareModes = set of TCompareMode;
    TExportFile = (efFile, efFile2, efFile3, efFile4, efFile5, efFile6, efFile7,
      efFile8, efFile9, efFile10, efFile11, efFile12, efFile13, efFile14, efFile15);
    TExportFiles = set of TExportFile;
    //TInitEvent = procedure(var MaxValue: Word; var FirstRangeValue: Word) of object;
    TInitCompareEvent = procedure(var CompareCrossRange: Boolean;
      var VertCompareSpacing: Cardinal; var VertSameValueCount: Byte;
      var VertSameValueCount2: Byte; var SlantCompareSpacing: Cardinal;
      var SlantSameValueCount: Byte; var SlantSameValueCount2: Byte;
      var CompareGroupValueCount: Byte; var ExportGroupValueCount: Byte
    ) of object;
  private
    //fInitEvent: TInitEvent;
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
    fFirstRangeValue: Word;  //准备去掉
    fIntervalValues: TWordDynArray;
    fRowCount: Integer;
    fCompareState: TRecCompareState;
    fCompareMode: TCompareMode;
    fCompareModeString: string;
    fCompareCrossRange: Boolean;
    fVertSlantCompareSpacing: Cardinal;
    fVertCompareSpacing: Cardinal;
    fVertSameValueCount: Byte;
    fVertSameValueCount2: Byte;
    fSlantCompareSpacing: Cardinal;
    fSlantSameValueCount: Byte;
    fSlantSameValueCount2: Byte;
    fCompareGroupValueCount: Byte;
    fExportGroupValueCount: Byte;
    fExportFiles: TExportFiles;
    fExportLite: Boolean;
    //fReCalc: Boolean;
    fRecalcMode: Byte;
    fKeepMaxRowSpacing: Cardinal;
    fGroupRowCount: Cardinal;
    fGroupCount: Cardinal;
    fReEnabledGroupCount: Cardinal;
    fHideSameGroup: Boolean;
    fDataMode: Byte;

    fTipStr: string;

    procedure SetCompareMode(AValue: TCompareMode);
    function GetCompareModeString: string;
    function BuildSlantGroupValue(Value: string): string;
    //function DataToString(Values: TWordDynArray): string;
    procedure CalcRowSpacing(CompareData: TSQLCompareData);
    procedure CalcFirstRowGroup(FirstRow: TSQLFirstRow; Enabled: Boolean = False);
    procedure ReCalc;
    function RebuildFile(RowCount: Cardinal): Boolean;
    function NumberToString(Value: Cardinal): string;
    function RebuildFileName(Sender: TFileWriter): string;
    function RebuildFileName2(Sender: TFileWriter): string; overload;
    function RebuildFileName2(Sender: TFileWriter; aRowNumber: Cardinal): string; overload;
    procedure WriteGroupReEnableTips(fr: TFileWriter; Group: TSQLCompareGroup);

    procedure SaveSlantCompareType;

    procedure SaveVertGroupByCompareTypeSortByLastFirstRow2;
    procedure SaveVertCompareType;  //同5

    procedure SaveCompareData(fr: TFileWriter; Data: TSQLCompareData; SaveValues: Boolean = True);

    procedure SaveGroupByFirstRow;
    procedure SaveGroupByCompareTypeSortByRowcount;
    procedure SaveGroupByCompareTypeSortByRowcount2;
    procedure SaveGroupByCompareTypeSortByMaxRowSpacing;
    procedure SaveGroupByCompareTypeSortByGroupValueCount;
    procedure SaveGroupByCompareTypeSortByMaxValueCount;
    procedure SaveGroupByCompareTypeSortByLastFirstRow;
    procedure SaveGroupByCompareTypeSortByOneRowSpacingCount;
    procedure SaveCompareDataSortByValueCount(FileName: string); overload;
    procedure SaveVertSlantCompareDataSortByValueCount; overload;
    procedure SaveVertSlantCompareDataSortByValueCount2;
    procedure SaveReEnabledGroup;
  public
    constructor Create;
    destructor Destroy;
    function GetKeyValue(Key: string): Variant;
    procedure SetKeyValue(Key: string; Value: Variant); overload;
    procedure SetKeyValue(Key: string; Value: TWordDynArray); overload;
    //procedure LoadRow(FileName: string); overload;
    procedure LoadRow(FileName: string; aIntervalValues: TWordDynArray); overload;
    procedure Compare;
    procedure DeleteInvalidData(aKeepMaxRowSpacing, aGroupRowCount,
      aGroupCount, aReEnabledGroupCount: Cardinal);
    procedure RestoreRecalcMode;
    procedure RecalcData(aKeepMaxRowSpacing, aGroupRowCount,
      aGroupCount, aReEnabledGroupCount: Cardinal; aHideSameGroup: Boolean); overload;
    procedure RecalcData; overload;
    procedure ExportCompareRow;
    procedure ExportCompareData(ExportFiles: TExportFiles; aExportLite: Boolean = False);
  published
    //property InitEvent: TInitEvent read fInitEvent write fInitEvent;
    property InitCompareEvent: TInitCompareEvent read fInitCompareEvent write fInitCompareEvent;
    //property MaxValue: Word read fMaxValue;
    //property FirstRangeValue: Word read fFirstRangeValue;
    property IntervalValues: TWordDynArray read fIntervalValues;
    property DataMode: Byte read fDataMode write fDataMode;
    property CompareMode: TCompareMode read fCompareMode write SetCompareMode;
    property CompareModeString: string read GetCompareModeString;
    property CompareCrossRange: Boolean read fCompareCrossRange;
    property VertCompareSpacing: Cardinal read fVertCompareSpacing;
    property VertSameValueCount: Byte read fVertSameValueCount;
    property VertSameValueCount2: Byte read fVertSameValueCount2;
    property SlantCompareSpacing: Cardinal read fSlantCompareSpacing;
    property SlantSameValueCount: Byte read fSlantSameValueCount;
    property SlantSameValueCount2: Byte read fSlantSameValueCount2;
    property CompareGroupValueCount: Byte read fCompareGroupValueCount;
    property ExportGroupValueCount: Byte read fExportGroupValueCount;
    property RecalcMode: Byte read fRecalcMode;
    property KeepMaxRowSpacing: Cardinal read fKeepMaxRowSpacing;
    property GroupRowCount: Cardinal read fGroupRowCount;
    property GroupCount: Cardinal read fGroupCount;
    property ReEnabledGroupCount: Cardinal read fReEnabledGroupCount;
    property HideSameGroup: Boolean read fHideSameGroup;
  end;

const
  EachFileRowCount: Cardinal = 1000000;
  EachFileRowNumber: Cardinal = 10000;
  EachPageRowCount: Word = 10000;

var
  fDataComputer: TDataComputer;

implementation

procedure TSQLCompareData.CalcCompareValueCount(aIntervalValues: TWordDynArray);
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
  SameValue.CalcValueCount(aIntervalValues);
  DifferentValue.CalcValueCount(aIntervalValues);
  fCompareValueCount := SameValue.TotalValueCount + DifferentValue.TotalValueCount;
  fTotalSameValueCount := SameValue.TotalValueCount;
  fSameValueCount := SameValue.ValueCount;
  fTotalDifferentValueCount := DifferentValue.TotalValueCount;
  fDifferentValueCount := DifferentValue.ValueCount;
end;

procedure TSQLCompareGroup.BuildValue2;
var
  s: string;
  i: Integer;
  v: Cardinal;
begin
  fValue2 := '';
  for s in string(fValue).Split(['、']) do
  begin
    for i := s.Length + 1 to 6 do fValue2 := fValue2 + '0';
    fValue2 := fValue2 + s;
  end;
end;

function TDataComputer.GetKeyValue(Key: string): Variant;
begin
  Result := Unassigned;
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

procedure TDataComputer.SetKeyValue(Key: string; Value: TWordDynArray);
begin
  fKeyValue.FillPrepare(fDatabase, 'Key = ?', [Key]);
  if fKeyValue.FillOne then
  begin
    fKeyValue.SetArrayValue(Value);
    fDatabase.Update(fKeyValue);
  end
  else
  begin
    fKeyValue.Key := Key;
    fKeyValue.SetArrayValue(Value);
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

function TDataComputer.GetCompareModeString: string;
begin
  if fCompareModeString.IsEmpty then
  begin
    case fCompareMode of
      cmVert: fCompareModeString := '直';
      cmSlant: fCompareModeString := '斜';
      cmVertSlant: fCompareModeString := '直、斜';
    end;
  end;
  Result := fCompareModeString;
end;

function TDataComputer.BuildSlantGroupValue(Value: string): string;
var
  s: string;
  v: Cardinal;
begin
  Result := '';
  for s in Value.Split(['、']) do
  begin
    if not Result.IsEmpty then Result := Result + '、';
    v := s.ToInteger;
    Result := Result + ((v + 1) div 2).ToString;
    if v mod 2 = 0 then Result := Result + 'Z'
    else Result := Result + 'Y';
  end;
end;

{function TDataComputer.DataToString(Values: TWordDynArray): string;
var
  v, v2, RangeNo, SubRangeNo: Word;
  s: string;
begin
  Result := '';
  for v in Values do
  begin
    if fDataMode = 0 then
    begin
      if (fFirstRangeValue > 0) and (v > fFirstRangeValue) then s := (v - fFirstRangeValue).ToString
      else s := v.ToString;
      if s.Length < 2 then s := '0' + s;
      if (fFirstRangeValue > 0) and (v > fFirstRangeValue) and (Result.IndexOf('-') = -1) then Result := Result + ' - ' + s
      else if Result.IsEmpty then Result := s
      else Result := Result + '、' + s;
    end
    else
    begin
      if (fFirstRangeValue > 0) and (v > fFirstRangeValue) then
      begin
        v2 := v - fFirstRangeValue;
        RangeNo := 2;
      end
      else
      begin
        v2 := v;
        RangeNo := 1;
      end;
      SubRangeNo := (v2 - 1) div 10 + 1;
      v2 := v2 mod 10;
      if v2 = 0 then v2 := 9
      else v2 := v2 - 1;
      s := Format('（%d-%d：%d）', [RangeNo, SubRangeNo, v2]);
      if (fFirstRangeValue > 0) and (v > fFirstRangeValue) and (Result.IndexOf('）-（') = -1) then Result := Result + ' - ' + s
      else if Result.IsEmpty then Result := s
      else Result := Result + s;
      //v2 := v;
    end;
  end;
end; }

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
  i: Integer;
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
  fDatabase.CreateSQLIndex(TSQLCompareGroup, 'Value2', False);
  fDatabase.CreateSQLIndex(TSQLCompareGroup, 'ValueCount', False);
  //fDatabase.CreateSQLIndex(TSQLCompareGroup, 'MaxRowSpacing', False);
  //fDatabase.CreateSQLIndex(TSQLCompareGroup, ['MaxTotalValueCount', 'MaxValueCount'], False);
  //fDatabase.CreateSQLIndex(TSQLCompareGroup, 'LastFirstRow', False);
  //fDatabase.CreateSQLIndex(TSQLCompareGroup, 'OneRowSpacingCount', False);

  fKeyValue := TSQLKeyValue.Create;
  fMaxValue := 0;
  v := GetKeyValue('MaxValue');
  if not VarIsEmpty(v) then fMaxValue := v;
  fFirstRangeValue := 0;
  v := GetKeyValue('FirstRangeValue');
  if not VarIsEmpty(v) then fFirstRangeValue := v;
  fIntervalValues := [];
  v := GetKeyValue('IntervalValues');
  if VarIsEmpty(v) then
  begin
    if fMaxValue > 0 then
    begin
      if fFirstRangeValue = 0 then
      begin
        SetLength(fIntervalValues, 1);
        fIntervalValues[0] := fMaxValue;
      end
      else
      begin
        SetLength(fIntervalValues, 2);
        fIntervalValues[0] := fFirstRangeValue;
        fIntervalValues[1] := fMaxValue - fFirstRangeValue;
      end;
    end;
  end
  else
  begin
    fMaxValue := 0;
    SetLength(fIntervalValues, Integer(v._Count));
    for i := 0 to v._Count - 1 do
    begin
      fIntervalValues[i] := v.Value(i);
      fMaxValue := fMaxValue + fIntervalValues[i];
    end;
  end;
  fDataMode := 0;
  fCompareMode := cmNone;
  v := GetKeyValue('CompareMode');
  if not VarIsEmpty(v) then fCompareMode := TCompareMode(v);
  fCompareCrossRange := True;
  v := GetKeyValue('CompareCrossRange');
  if not VarIsEmpty(v) then fCompareCrossRange := v;
  fVertCompareSpacing := 0;
  v := GetKeyValue('VertCompareSpacing');
  if not VarIsEmpty(v)then fVertCompareSpacing := v;
  fVertSameValueCount := 0;
  v := GetKeyValue('VertSameValueCount');
  if not VarIsEmpty(v) then fVertSameValueCount := v;
  fVertSameValueCount2 := 0;
  v := GetKeyValue('VertSameValueCount2');
  if not VarIsEmpty(v) then fVertSameValueCount2 := v;
  fSlantCompareSpacing := 0;
  v := GetKeyValue('SlantCompareSpacing');
  if not VarIsEmpty(v) then fSlantCompareSpacing := v;
  fSlantSameValueCount := 0;
  v := GetKeyValue('SlantSameValueCount');
  if not VarIsEmpty(v) then fSlantSameValueCount := v;
  fSlantSameValueCount2 := 0;
  v := GetKeyValue('SlantSameValueCount2');
  if not VarIsEmpty(v) then fSlantSameValueCount2 := v;
  fCompareGroupValueCount := 0;
  v := GetKeyValue('CompareGroupValueCount');
  if not VarIsEmpty(v) then fCompareGroupValueCount := v;
  fExportGroupValueCount := 0;
  v := GetKeyValue('ExportGroupValueCount');
  if not VarIsEmpty(v) then fExportGroupValueCount := v;

  fVertSlantCompareSpacing := fVertCompareSpacing;
  if fSlantCompareSpacing > fVertCompareSpacing then
    fVertSlantCompareSpacing := fSlantCompareSpacing;

  fRecalcMode := 0;
  v := GetKeyValue('RecalcMode');
  if not VarIsEmpty(v) then fRecalcMode := v;
  fKeepMaxRowSpacing := 0;
  v := GetKeyValue('ActiveKeepMaxRowSpacing');
  if not VarIsEmpty(v) then fKeepMaxRowSpacing := v;
  fGroupRowCount := 0;
  v := GetKeyValue('ActiveGroupRowCount');
  if not VarIsEmpty(v) then fGroupRowCount := v;
  fGroupCount := 0;
  v := GetKeyValue('ActiveGroupCount');
  if not VarIsEmpty(v) then fGroupCount := v;
  fReEnabledGroupCount := 0;
  v := GetKeyValue('ActiveReEnabledGroupCount');
  if not VarIsEmpty(v) then fReEnabledGroupCount := v;
  v := GetKeyValue('ActiveHideSameGroup');
  if not VarIsEmpty(v) then fHideSameGroup := Boolean(v);
end;

destructor TDataComputer.Destroy;
begin
  fKeyValue.Free;
  fDatabase.Free;
  fModel.Free;
  inherited Destroy;
end;

{procedure TDataComputer.LoadRow(FileName: string);
var
  i, RowIndex, Digit, SaveRowCount: Integer;
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
      fDataMode := 0;
      if fFirstRangeValue = 0 then
      begin
        SetLength(fIntervalValues, 1);
        fIntervalValues[0] := fMaxValue;
      end
      else
      begin
        SetLength(fIntervalValues, 2);
        fIntervalValues[0] := fFirstRangeValue;
        fIntervalValues[1] := fMaxValue - fFirstRangeValue;
      end;

      SetKeyValue('MaxValue', fMaxValue);
      SetKeyValue('FirstRangeValue', fFirstRangeValue);
      SetKeyValue('IntervalValues', fIntervalValues);
      SetKeyValue('DataMode', fDataMode);
    end
    else raise Exception.Create('导入行初始化失败');

  if not TFile.Exists(FileName) then Exit;
  fRowCount := fDatabase.TableRowCount(TSQLRow);
  SaveRowCount := fRowCount;
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

        if SaveRowCount <> fRowCount then RestoreRecalcMode;
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
end;}

procedure TDataComputer.LoadRow(FileName: string; aIntervalValues: TWordDynArray);
var
  i, RowIndex, Digit, SaveRowCount: Integer;
  s, sDigit: string;
  c: Char;
  HasMinus: Boolean;
  Row: TSQLRow;
begin
  if Length(fIntervalValues) = 0 then
  begin
    //fFirstRangeValue := aIntervalValues[0];
    //if fFirstRangeValue = fMaxValue then fFirstRangeValue := 0;
    fIntervalValues := aIntervalValues;

    //SetKeyValue('MaxValue', fMaxValue);
    //SetKeyValue('FirstRangeValue', fFirstRangeValue);
    SetKeyValue('IntervalValues', fIntervalValues);

    for i := Low(fIntervalValues) to High(fIntervalValues) do
      fMaxValue := fMaxValue + fIntervalValues[i];
  end;

  if not TFile.Exists(FileName) then Exit;
  fRowCount := fDatabase.TableRowCount(TSQLRow);
  SaveRowCount := fRowCount;
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
          s := ValueFromIndex[i];
          Row.AssignValue(s, aIntervalValues, fDataMode);
          fDatabase.Add(Row, True);
          fRowCount := Row.Number;
        end;
        fDatabase.Commit(1, True);

        if SaveRowCount <> fRowCount then RestoreRecalcMode;
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
          //if v <= fFirstRangeValue then SameValueCount := SameValueCount + 1;
          if v <= fIntervalValues[0] then SameValueCount := SameValueCount + 1;
          TotalSameValueCount := TotalSameValueCount + 1;
          SetLength(SameValues, Length(SameValues) + 1);
          SameValues[High(SameValues)] := v;
        end
        else
        begin
          //if v <= fFirstRangeValue then DifferentValueCount := DifferentValueCount + 1;
          if v <= fIntervalValues[0] then DifferentValueCount := DifferentValueCount + 1;
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
    v, v2, FisrtIntervalValue: Word;
  begin
    FisrtIntervalValue := fIntervalValues[0];
    if Length(fIntervalValues) = 1 then FisrtIntervalValue := 0;

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
        if not fCompareCrossRange and (((Offset > 0) and (v > FisrtIntervalValue))
          or ((Offset < 0) and (v <= FisrtIntervalValue)))
        then Continue;

        CompareData.AddValue(v);
        if r.ValueExist(v) then
        begin
          if v <= fIntervalValues[0] then SameValueCount := SameValueCount + 1;
          TotalSameValueCount := TotalSameValueCount + 1;
          SetLength(SameValues, Length(SameValues) + 1);
          SameValues[High(SameValues)] := v;
        end
        else
        begin
          if v <= fIntervalValues[0] then DifferentValueCount := DifferentValueCount + 1;
          TotalDifferentValueCount := TotalDifferentValueCount + 1;
          SetLength(DifferentValues, Length(DifferentValues) + 1);
          DifferentValues[High(DifferentValues)] := v;
        end;
      end;
      Result := CompareData.HasValue
        and (TotalSameValueCount >= fSlantSameValueCount)
        and (TotalSameValueCount <= fSlantSameValueCount2);
    end;
  end;

var
  Row, Row2: TSQLRow;
  CompareData, CompareData2: TSQLCompareData;
  FirstRow, FirstRow2: TSQLFirstRow;
  Group: TSQLCompareGroup;
  DataList: TSQLTableJSON;
  i, i2, PageIndex: Integer;
  RowCount, Number, GroupValue: Cardinal;
  GroupValueCount: Byte;
  v: Variant;
begin
  if fDatabase.TableRowCount(TSQLCompareData) = 0 then
  begin
    if Assigned(fInitCompareEvent) then
      fInitCompareEvent(fCompareCrossRange, fVertCompareSpacing, fVertSameValueCount,
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
    SetKeyValue('CompareCrossRange', fCompareCrossRange);
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
  end;

  fCompareState.Process := cpCompare;
  fCompareState.FirstRow := fVertSlantCompareSpacing + 1;
  v := GetKeyValue('CompareStateProcess');
  if not VarIsEmpty(v) then fCompareState.Process := TCompareProcess(v);
  v := GetKeyValue('CompareStateFirstRow');
  if not VarIsEmpty(v) then fCompareState.FirstRow := v;
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
              //直连
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
                      CompareData.GroupValue := GroupValue.ToString;
                      with CompareData.CompareRows[0] do
                      begin
                        CompareType := 1;
                        Number := GroupValue;
                      end;
                      CompareData.AssignValue(Row2);
                      CompareData.CalcValueCount(fIntervalValues);
                      CompareData.CalcCompareValueCount(fIntervalValues);
                      //CalcRowSpacing(CompareData);
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
                    //右斜连
                    if SlantCompareRow(Row, Row2, CompareData, Number, CompareData.fCompareRows)
                    then
                    begin
                      if fCompareMode = cmSlant then GroupValue := Number * 2 - 1
                      else GroupValue := Number * 3 - 1;
                      CompareData.GroupValue := GroupValue.ToString;
                      with CompareData.CompareRows[0] do
                      begin
                        CompareType := 2;
                        Number := GroupValue;
                      end;
                      CompareData.CalcValueCount(fIntervalValues);
                      CompareData.CalcCompareValueCount(fIntervalValues);
                      //CalcRowSpacing(CompareData);
                      fDatabase.Add(CompareData, True);
                    end;
                    //左斜连
                    if SlantCompareRow(Row, Row2, CompareData, -Number, CompareData.fCompareRows)
                    then
                    begin
                      if fCompareMode = cmSlant then GroupValue := Number * 2
                      else GroupValue := Number * 3;
                      CompareData.GroupValue := GroupValue.ToString;
                      with CompareData.CompareRows[0] do
                      begin
                        CompareType := 3;
                        Number := GroupValue;
                      end;
                      CompareData.CalcValueCount(fIntervalValues);
                      CompareData.CalcCompareValueCount(fIntervalValues);
                      //CalcRowSpacing(CompareData);
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
                CompareData.GroupValue := '';
                CompareData.GroupValueCount := 0;
                CompareData.ClearValue;
                SetLength(CompareData.fCompareRows, Length(RowNoArray));
                for i := Low(RowNoArray) to High(RowNoArray) do
                begin
                  CompareData.FillRow(RowNoArray[i], CompareData2);
                  if CompareData.GroupValue <> '' then
                    CompareData.GroupValue := CompareData.GroupValue + '、';
                  CompareData.GroupValue := CompareData.GroupValue + CompareData2.GroupValue;
                  CompareData.GroupValueCount := CompareData.GroupValueCount + 1;
                  CompareData.AddValues(CompareData2);

                  CompareData.CompareRows[i] := CompareData2.CompareRows[0];
                end;
                CompareData.CalcValueCount(fIntervalValues);
                CompareData.CalcCompareValueCount(fIntervalValues);
                //CalcRowSpacing(CompareData);
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
              {case fCompareMode of
                cmSlant, cmVertSlant:
                begin
                  FirstRow.RowCount := StrToInt(fDatabase.OneFieldValue(TSQLCompareData, 'Count(*)', 'FirstRow = ?', [FirstRow.Value]));
                end;
              end;}
              FirstRow.RowCount := StrToInt(fDatabase.OneFieldValue(TSQLCompareData, 'Count(*)', 'FirstRow = ?', [FirstRow.Value]));

              fDatabase.Add(FirstRow, True);
            end;
          end;
        end;
        cpUpdateCompareGroup:
        begin
          PageIndex := 0;
          repeat
            CompareData.FillPrepare(fDatabase, 'FirstRow = ? LIMIT ? OFFSET ?',
              [Row.Number, EachPageRowCount, PageIndex * EachPageRowCount]);
            fDatabase.TransactionBegin(TSQLCompareGroup);
            try
              while CompareData.FillOne do
              begin
                Group.FillPrepare(fDatabase, 'Value = ?', [CompareData.GroupValue]);
                if Group.FillOne then
                begin
                  //Group.RowCount := Group.RowCount + 1;
                  //Group.LastFirstRow := Row.Number;
                  {CompareData2.FillPrepare(fDatabase, 'GroupValue = ? AND FirstRow < ? ORDER BY FirstRow DESC LIMIT 1', [Group.Value, Row.Number]);
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
                  end;}
                  //fDatabase.Update(Group);
                end
                else
                begin
                  Group.Value := CompareData.GroupValue;
                  Group.BuildValue2;
                  Group.ValueCount := CompareData.GroupValueCount;
                  {Group.RowCount := 1;
                  Group.MaxRowSpacing := CompareData.RowSpacing;
                  Group.MaxTotalValueCount := CompareData.TotalValueCount;
                  Group.MaxValueCount := CompareData.ValueCount;
                  Group.LastFirstRow := CompareData.FirstRow; }
                  Group.RowCount := 0;
                  Group.MaxRowSpacing := 0;
                  Group.MaxTotalValueCount := 0;
                  Group.MaxValueCount := 0;
                  Group.LastFirstRow := 0;
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
            Inc(PageIndex);
          until CompareData.FillTable.RowCount = 0;
        end;
        cpUpdateGroupDataRowSpacing:
        begin
          if Row.Number = fRowCount then
          begin
            PageIndex := 0;
            repeat
              fDatabase.TransactionBegin(TSQLCompareGroup);
              try
                Group.FillPrepare(fDatabase, 'LIMIT ? OFFSET ?', 
                  [EachPageRowCount, PageIndex * EachPageRowCount]);
                while Group.FillOne do
                begin
                  CompareData.FillPrepare(fDatabase, 'GroupValue = ? AND FirstRow > ? ORDER BY FirstRow DESC',
                    [Group.Value, Group.LastFirstRow]);
                  while CompareData.FillOne do
                  begin
                    if CompareData.FillCurrentRow <= CompareData.FillTable.RowCount then
                      CompareData.FillRow(CompareData.FillCurrentRow, CompareData2)
                    else
                    begin
                      if Group.LastFirstRow = 0 then
                      begin
                        case fCompareMode of
                          cmVert: CompareData2.FirstRow := fVertCompareSpacing - 1;
                          cmSlant: CompareData2.FirstRow := fSlantCompareSpacing - 1;
                          cmVertSlant: CompareData2.FirstRow := fVertSlantCompareSpacing - 1;
                        end;
                      end
                      else CompareData2.FirstRow := Group.LastFirstRow;
                    end;
                    CompareData.RowSpacing := CompareData.FirstRow - CompareData2.FirstRow;
                    fDatabase.Update(CompareData);

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
                  CompareData.FillRewind;
                  if CompareData.FillOne then
                  begin
                    //更新最新首行
                    Group.LastFirstRow := CompareData.FirstRow;
                    //更新最大临行距
                    CompareData.RowSpacing := fRowCount + 1 - Group.LastFirstRow;
                    if CompareData.RowSpacing > Group.MaxRowSpacing then
                      Group.MaxRowSpacing := CompareData.RowSpacing;
                  end;

                  Group.RowCount := Group.RowCount + CompareData.FillTable.RowCount;

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
              Inc(PageIndex);
            until Group.FillTable.RowCount = 0;
          end;
        end;
        cpUpdateGroupOneRowSpacingCount:
        begin
          if (fRowCount = Row.Number) and (fCompareMode in [cmSlant, cmVertSlant]) then
          begin
            fDatabase.Execute('UPDATE CompareGroup SET OneRowSpacingCount = 0 WHERE OneRowSpacingCount > 0');
            PageIndex := 0;
            repeat
              Group.FillPrepare(fDatabase, 'LastFirstRow = ? LIMIT ? OFFSET ?',
                [fRowCount, EachPageRowCount, PageIndex * EachPageRowCount]);
              fDatabase.TransactionBegin(TSQLCompareGroup);
              try
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
              Inc(PageIndex);
            until Group.FillTable.RowCount = 0;
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

procedure TDataComputer.DeleteInvalidData(aKeepMaxRowSpacing, aGroupRowCount,
  aGroupCount, aReEnabledGroupCount: Cardinal);
var
  FirstRow, FirstRow2: TSQLFirstRow;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
  LastEnabledFirstRow, RowSpacing: Cardinal;
  RecalcRowSpacing: Boolean;
  LastNotEnabledFirstRowID: TID;
  s: string;
  PageIndex: Integer;

  procedure ReEnabledFirstCompareData(FirstRowValue, ReEnabledFirstRow, ReEnabledFirstRow2: Cardinal);
  begin
    //前N个代号恢复显示
    Data.FillPrepare(fDatabase, 'FirstRow = ? LIMIT ?', [FirstRowValue, aReEnabledGroupCount]);
    while Data.FillOne do
    begin
      Data.Enabled := True;
      fDatabase.Update(Data);
      //恢复对应代号
      Group.FillPrepare(fDatabase, 'Value = ?', [Data.GroupValue]);
      Group.FillOne;
      if not Group.Enabled then
      begin
        Group.Enabled := True;
        Group.ReEnabled := True;
        Group.ReEnabledFirstRow := ReEnabledFirstRow;
        if ReEnabledFirstRow2 > 0 then
          Group.ReEnabledFirstRow2 := ReEnabledFirstRow2
        else
          Group.ReEnabledFirstRow2 := fVertSlantCompareSpacing + 1;
        Group.ReEnabledRowSpacing := Group.ReEnabledFirstRow - Group.ReEnabledFirstRow2;
        Group.KeepMaxRowSpacing := aKeepMaxRowSpacing;
        fDatabase.Update(Group);
      end;
    end;
  end;
begin
  SetKeyValue('KeepMaxRowSpacing', aKeepMaxRowSpacing);
  SetKeyValue('GroupRowCount', aGroupRowCount);
  SetKeyValue('GroupCount', aGroupCount);
  SetKeyValue('ReEnabledGroupCount', aReEnabledGroupCount);
  //计算保留的代号
  TSQLCompareGroup.AutoFree(Group);
  PageIndex := 0;
  repeat
    s := 'ORDER BY RowCount DESC LIMIT ? OFFSET ?';
    Group.FillPrepare(fDatabase, s, [EachPageRowCount, PageIndex * EachPageRowCount]);
    fDatabase.TransactionBegin(TSQLCompareGroup);
    try
      while Group.FillOne do
      begin
        Group.Enabled := Group.FillCurrentRow - 1 <= aGroupCount;
        if not Group.Enabled and (aGroupRowCount > 0) then
          Group.Enabled := Group.RowCount >= aGroupRowCount;
        Group.ReEnabled := False;
        Group.ReEnabledFirstRow := 0;
        Group.ReEnabledFirstRow2 := 0;
        Group.ReEnabledRowSpacing := 0;
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
    Inc(PageIndex);
  until Group.FillTable.RowCount = 0;
  //计算保留的首行
  TSQLFirstRow.AutoFree(FirstRow, fDatabase, '', []);
  TSQLFirstRow.AutoFree(FirstRow2);
  TSQLCompareData.AutoFree(Data);
  fDatabase.TransactionBegin(TSQLFirstRow);
  try
    LastEnabledFirstRow := 0;
    RecalcRowSpacing := False;
    while FirstRow.FillOne do
    begin
      //首行数不符合的代号不保留
      PageIndex := 0;
      repeat
        s := 'FirstRow = ? LIMIT ? OFFSET ?';
        Data.FillPrepare(fDatabase, s, [FirstRow.Value, EachPageRowCount, PageIndex * EachPageRowCount]);
        while Data.FillOne do
        begin
          Data.Enabled := False;
          Group.FillPrepare(fDatabase, 'Value = ?', [Data.GroupValue]);
          if Group.FillOne then
            Data.Enabled := Group.Enabled;
          if not Data.Enabled then FirstRow.RowCount := FirstRow.RowCount - 1;
          fDatabase.Update(Data);
        end;
        Inc(PageIndex);
      until Data.FillTable.RowCount = 0;
      //没有代号的首行不显示
      FirstRow.Enabled := FirstRow.RowCount > 0;
      if FirstRow.Enabled then
      begin
        //计算临行距
        if RecalcRowSpacing then
        begin
          if LastEnabledFirstRow > 0 then
            RowSpacing := FirstRow.Value - LastEnabledFirstRow
          else
            RowSpacing := FirstRow.Value - fVertSlantCompareSpacing - 1;
          //如果临行距大于最小临行距，上一不显示首行恢复显示
          if (aKeepMaxRowSpacing > 0) and (RowSpacing > aKeepMaxRowSpacing) then
          begin
            if FirstRow.FillRow(FirstRow.FillTable.RowFromID(LastNotEnabledFirstRowID), FirstRow2) then;
            begin
              FirstRow2.Enabled := True;
              fDatabase.Update(FirstRow2);

              ReEnabledFirstCompareData(FirstRow2.Value, FirstRow.Value, LastEnabledFirstRow);
            end;
          end
          else FirstRow.RowSpacing := RowSpacing;
        end;
        LastEnabledFirstRow := FirstRow.Value;
      end
      else
      begin
        if FirstRow.FillCurrentRow > FirstRow.FillTable.RowCount then
        begin
          //如果最大首行不显示导致下一首行临行距大于最小临行距，最大首行恢复显示
          if (aKeepMaxRowSpacing > 0)
            and (LastEnabledFirstRow > 0)
            and (fRowCount + 1 - LastEnabledFirstRow > aKeepMaxRowSpacing)
          then
          begin
            FirstRow.Enabled := True;

            ReEnabledFirstCompareData(FirstRow.Value, fRowCount + 1, LastEnabledFirstRow);
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
  fDatabase.Execute('DELETE FROM CompareData WHERE Enabled = 0');
  fDatabase.Execute('DELETE FROM FirstRow WHERE Enabled = 0');
  fDatabase.Execute('DELETE FROM CompareGroup WHERE Enabled = 0');
  fDatabase.Execute('VACUUM');
  //计算代号情况
  fDatabase.TransactionBegin(TSQLFirstRow);
  try
    FirstRow.FillPrepare(fDatabase, '', []);
    while FirstRow.FillOne do
    begin
      FirstRow.GroupValueCount := 0;
      FirstRow.MaxGroupValueCount := 0;
      FirstRow.MaxRowCountGroupValueCount := 0;
      FirstRow.MaxGroupValueCountRowCount := 0;
      CalcFirstRowGroup(FirstRow, True);
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
end;

procedure TDataComputer.RestoreRecalcMode;
begin
  fRecalcMode := 0;
  SetKeyValue('RecalcMode', fRecalcMode);
end;

procedure TDataComputer.RecalcData(aKeepMaxRowSpacing, aGroupRowCount,
  aGroupCount, aReEnabledGroupCount: Cardinal; aHideSameGroup: Boolean);
var
  FirstRow, PreviousFirstRow, NextFirstRow: TSQLFirstRow;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
  Dictionary: TDictionary<Cardinal, Boolean>;
  HideGroup: Boolean;
  GroupRowNo, DataRowCount: Cardinal;
  s: string;
  PageIndex: Integer;
begin
  HideGroup := (aGroupRowCount > 0) or (aGroupCount > 0);
  //计算可导出代号
  TSQLCompareGroup.AutoFree(Group);
  TSQLFirstRow.AutoFree(FirstRow);
  TSQLFirstRow.AutoFree(PreviousFirstRow);
  TSQLFirstRow.AutoFree(NextFirstRow);
  TSQLCompareData.AutoFree(Data);
  if HideGroup then
  begin
    PageIndex := 0;
    GroupRowNo := 0;
    repeat
      s := 'ORDER BY RowCount DESC LIMIT ? OFFSET ?';
      Group.FillPrepare(fDatabase, s, [EachPageRowCount, PageIndex * EachPageRowCount]);
      fDatabase.TransactionBegin(TSQLCompareGroup);
      try
        while Group.FillOne do
        begin
          GroupRowNo := GroupRowNo + 1;
          Group.Enabled := GroupRowNo <= aGroupCount;
          if not Group.Enabled and (aGroupRowCount > 0) then
            Group.Enabled := Group.RowCount >= aGroupRowCount;
          Group.ReEnabled := False;
          Group.ReEnabledFirstRow := 0;
          Group.ReEnabledFirstRow2 := 0;
          Group.ReEnabledRowSpacing := 0;
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
      Inc(PageIndex);
    until Group.FillTable.RowCount = 0;
    //计算可导出首行
    FirstRow.FillPrepare(fDatabase, 'ORDER BY Value DESC', []);
    fDatabase.TransactionBegin(TSQLFirstRow);
    try
      PreviousFirstRow.Value := fRowCount + 1;
      while FirstRow.FillOne do
      begin
        //首行数不符合的代号不导出
        DataRowCount := FirstRow.RowCount;
        PageIndex := 0;
        repeat
          s := 'FirstRow = ? LIMIT ? OFFSET ?';
          Data.FillPrepare(fDatabase, s, [FirstRow.Value, EachPageRowCount, PageIndex * EachPageRowCount]);
          while Data.FillOne do
          begin
            Data.Enabled := False;
            Group.FillPrepare(fDatabase, 'Value = ?', [Data.GroupValue]);
            if Group.FillOne then
              Data.Enabled := Group.Enabled;
            if not Data.Enabled then DataRowCount := DataRowCount - 1;
            fDatabase.Update(Data);
          end;
          Inc(PageIndex);
        until Data.FillTable.RowCount = 0;
        //没有代号的首行不显示
        FirstRow.RowSpacing2 := FirstRow.RowSpacing;
        FirstRow.RowCount2 := DataRowCount;
        FirstRow.Enabled := DataRowCount > 0;
        if FirstRow.Enabled then
          FirstRow.FillRow(FirstRow.FillCurrentRow - 1, PreviousFirstRow)
        else
        begin
          //如果需要保持最大临行距的值，则要恢复显示
          if aKeepMaxRowSpacing > 0 then
          begin
            if FirstRow.FillCurrentRow > FirstRow.FillTable.RowCount then
              NextFirstRow.Value := fVertSlantCompareSpacing + 1
            else
              FirstRow.FillRow(FirstRow.FillCurrentRow, NextFirstRow);
            PreviousFirstRow.RowSpacing := PreviousFirstRow.Value - NextFirstRow.Value;
            if PreviousFirstRow.RowSpacing > aKeepMaxRowSpacing
            then
            begin
              FirstRow.Enabled := True;
              //前N个代号恢复显示
              Data.FillPrepare(fDatabase, 'FirstRow = ? LIMIT ?', [FirstRow.Value, aReEnabledGroupCount]);
              while Data.FillOne do
              begin
                Data.Enabled := True;
                fDatabase.Update(Data);
                //恢复对应代号
                Group.FillPrepare(fDatabase, 'Value = ?', [Data.GroupValue]);
                Group.FillOne;
                if not Group.Enabled then
                begin
                  Group.Enabled := True;
                  Group.ReEnabled := True;
                  Group.ReEnabledFirstRow := PreviousFirstRow.Value;
                  Group.ReEnabledFirstRow2 := NextFirstRow.Value;
                  Group.ReEnabledRowSpacing := Group.ReEnabledFirstRow - Group.ReEnabledFirstRow2;
                  Group.KeepMaxRowSpacing := aKeepMaxRowSpacing;
                  fDatabase.Update(Group);
                end;
              end;

              FirstRow.FillRow(FirstRow.FillCurrentRow - 1, PreviousFirstRow);
            end;
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
  end
  else
  begin
    fDatabase.Execute('UPDATE FirstRow SET Enabled = 1');
    fDatabase.Execute('UPDATE CompareGroup SET Enabled = 1');
    fDatabase.Execute('UPDATE CompareData SET Enabled = 1');
  end;
  //隐藏相同代号
  if aHideSameGroup then
  begin
    Dictionary := TDictionary<Cardinal, Boolean>.Create;
    try
      PageIndex := 0;
      repeat
        s := 'ORDER BY RowCount DESC LIMIT ? OFFSET ?';
        if HideGroup then s := 'Enabled = 1 ' + s;
        Group.FillPrepare(fDatabase, s, [EachPageRowCount, PageIndex * EachPageRowCount]);
        fDatabase.TransactionBegin(TSQLCompareGroup);
        try
          while Group.FillOne do
          begin
            Group.Enabled := False;

            s := 'GroupValue = ? ORDER BY FirstRow DESC';
            if HideGroup then s := 'Enabled = 1 AND ' + s;
            Data.FillPrepare(fDatabase, s, [Group.Value]);
            while Data.FillOne do
            begin
              Data.Enabled := False;
              if not Dictionary.ContainsKey(Data.FirstRow) then
              begin
                Data.Enabled := True;
                Group.Enabled := True;

                Dictionary.Add(Data.FirstRow, True);
              end;

              fDatabase.Update(Data);
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
        Inc(PageIndex);
      until Group.FillTable.RowCount = 0;

      {s := 'ORDER BY Value DESC';
      if HideGroup then s := 'Enabled = 1 ' + s;
      FirstRow.FillPrepare(fDatabase, s, []);
      fDatabase.TransactionBegin(TSQLFirstRow);
      try
        while FirstRow.FillOne do
        begin
          s := 'FirstRow = ? AND Enabled = 1 LIMIT 1';
          Data.FillPrepare(fDatabase, s, [FirstRow.Value]);
          FirstRow.Enabled := Data.FillTable.RowCount > 0;

          fDatabase.Update(FirstRow);
        end;
        fDatabase.Commit(1, True);
      except
        on e: Exception do
        begin
          fDatabase.RollBack;
          raise Exception.Create(e.Message);
        end;
      end;}
    finally
      Dictionary.Free;
    end;
    {s := '';
    if HideGroup then s := 'Enabled = 1';
    FirstRow.FillPrepare(fDatabase, s, []);
    while FirstRow.FillOne do
    begin
      PageIndex := 0;
      repeat
        s := 'FirstRow = ? LIMIT ? OFFSET ?';
        if HideGroup then s := 'Enabled = 1 AND ' + s;
        Data.FillPrepare(fDatabase, s, [FirstRow.Value, EachPageRowCount, PageIndex * EachPageRowCount]);
        while Data.FillOne do
        begin
          Data.Enabled := False;
          Group.FillPrepare(fDatabase, 'Value = ?', [Data.GroupValue]);
          if Group.FillOne then
            Data.Enabled := Group.Enabled;
          if not Data.Enabled then DataRowCount := DataRowCount - 1;
          fDatabase.Update(Data);
        end;
        Inc(PageIndex);
      until Data.FillTable.RowCount = 0;

      fDatabase.Update(FirstRow);
    end;
    fDatabase.Commit(1, True); }
  end;

  fDatabase.TransactionBegin(TSQLFirstRow);
  try
    FirstRow.FillPrepare(fDatabase, 'Enabled = 1 ORDER BY Value DESC', []);
    while FirstRow.FillOne do
    begin
      //计算临行距2
      if FirstRow.FillCurrentRow > FirstRow.FillTable.RowCount then
        NextFirstRow.Value := fVertSlantCompareSpacing + 1
      else
        FirstRow.FillRow(FirstRow.FillCurrentRow, NextFirstRow);
      FirstRow.RowSpacing2 := FirstRow.Value - NextFirstRow.Value;
      //计算代号情况
      if FirstRow.RowCount <> FirstRow.RowCount2 then
      begin
        FirstRow.GroupValueCount2 := 0;
        FirstRow.MaxGroupValueCount2 := 0;
        FirstRow.MaxRowCountGroupValueCount2 := 0;
        FirstRow.MaxGroupValueCountRowCount2 := 0;
        CalcFirstRowGroup(FirstRow, True);
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

  fRecalcMode := 1;
  SetKeyValue('RecalcMode', fRecalcMode);
  fKeepMaxRowSpacing := aKeepMaxRowSpacing;
  SetKeyValue('ActiveKeepMaxRowSpacing', fKeepMaxRowSpacing);
  fGroupRowCount := aGroupRowCount;
  SetKeyValue('ActiveGroupRowCount', fGroupRowCount);
  fGroupCount := aGroupCount;
  SetKeyValue('ActiveGroupCount', fGroupCount);
  fReEnabledGroupCount := aReEnabledGroupCount;
  SetKeyValue('ActiveReEnabledGroupCount', fReEnabledGroupCount);
  fHideSameGroup := aHideSameGroup;
  SetKeyValue('ActiveHideSameGroup', Ord(fHideSameGroup));
end;

procedure TDataComputer.RecalcData;
var
  FirstRow: TSQLFirstRow;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
  s: string;
  PageIndex: Integer;
  Dictionary: TDictionary<Cardinal, Boolean>;
  c: Cardinal;
begin
  fDatabase.Execute('UPDATE CompareData SET Enabled = 0');
  TSQLFirstRow.AutoFree(FirstRow);
  TSQLCompareGroup.AutoFree(Group);
  TSQLCompareData.AutoFree(Data);
  Dictionary := TDictionary<Cardinal, Boolean>.Create;
  try
    PageIndex := 0;
    repeat
      s := 'ORDER BY RowCount DESC LIMIT ? OFFSET ?';
      Group.FillPrepare(fDatabase, s, [EachPageRowCount, PageIndex * EachPageRowCount]);
      fDatabase.TransactionBegin(TSQLCompareGroup);
      try
        while Group.FillOne do
        begin
          Group.Enabled := False;

          s := 'GroupValue = ? ORDER BY FirstRow DESC LIMIT 1';
          Data.FillPrepare(fDatabase, s, [Group.Value]);
          Data.FillOne;
          if not Dictionary.ContainsKey(Data.FirstRow) then
          begin
            Data.Enabled := True;
            fDatabase.Update(Data);
            Group.Enabled := True;

            Dictionary.Add(Data.FirstRow, True);
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
      Inc(PageIndex);
    until Group.FillTable.RowCount = 0;
  finally
    Dictionary.Free;
  end;

  s := 'ORDER BY Value DESC';
  FirstRow.FillPrepare(fDatabase, s, []);
  fDatabase.TransactionBegin(TSQLFirstRow);
  try
    while FirstRow.FillOne do
    begin
      s := 'FirstRow = ? AND Enabled = 1 LIMIT 1';
      Data.FillPrepare(fDatabase, s, [FirstRow.Value]);
      FirstRow.Enabled := Data.FillTable.RowCount > 0;
      if FirstRow.Enabled then
      begin
        FirstRow.GroupValueCount2 := 0;
        FirstRow.MaxGroupValueCount2 := 0;
        FirstRow.MaxRowCountGroupValueCount2 := 0;
        FirstRow.MaxGroupValueCountRowCount2 := 0;
        CalcFirstRowGroup(FirstRow, True);
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

  fRecalcMode := 2;
  SetKeyValue('RecalcMode', fRecalcMode);
end;

procedure TDataComputer.ExportCompareRow;
var
  Row: TSQLRow;
  fr: TFileWriter;
  s, FileName: string;
begin
  FileName := TPath.GetDirectoryName(ParamStr(0)) + '\行.txt';
  if TFile.Exists(FileName) then TFile.Delete(FileName);
  fr := TFileWriter.Create(FileName);
  try
    TSQLRow.AutoFree(Row, fDatabase, 'ORDER BY Number', []);
    while Row.FillOne do
    begin
      //s := Format('%d=%s', [Row.Number, DataToString(Row.Values)]);
      s := Format('%d=%s', [Row.Number, Row.ToString(fDataMode)]);
      fr.WriteLn(s);
    end;
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.ReCalc;
var
  FirstRow, FirstRow2, PreviousFirstRow, NextFirstRow: TSQLFirstRow;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
  GroupRowNo, DataRowCount, LastEnabledFirstRow, RowSpacing: Cardinal;
  RecalcRowSpacing: Boolean;
  LastNotEnabledFirstRowID: TID;
  s: string;
  PageIndex: Integer;

  procedure ReEnabledFirstCompareData(FirstRowValue, ReEnabledFirstRow, ReEnabledFirstRow2: Cardinal);
  begin
    //前N个代号恢复显示
    Data.FillPrepare(fDatabase, 'FirstRow = ? LIMIT ?', [FirstRowValue, fReEnabledGroupCount]);
    while Data.FillOne do
    begin
      Data.Enabled := True;
      fDatabase.Update(Data);
      //恢复对应代号
      Group.FillPrepare(fDatabase, 'Value = ?', [Data.GroupValue]);
      Group.FillOne;
      if not Group.Enabled then
      begin
        Group.Enabled := True;
        Group.ReEnabled := True;
        Group.ReEnabledFirstRow := ReEnabledFirstRow;
        if ReEnabledFirstRow2 > 0 then
          Group.ReEnabledFirstRow2 := ReEnabledFirstRow2
        else
          Group.ReEnabledFirstRow2 := fVertSlantCompareSpacing + 1;
        Group.ReEnabledRowSpacing := Group.ReEnabledFirstRow - Group.ReEnabledFirstRow2;
        fDatabase.Update(Group);
      end;
    end;
  end;
begin
  //计算可导出代号
  TSQLCompareGroup.AutoFree(Group);
  PageIndex := 0;
  GroupRowNo := 0;
  repeat
    s := 'ORDER BY RowCount DESC LIMIT ? OFFSET ?';
    Group.FillPrepare(fDatabase, s, [EachPageRowCount, PageIndex * EachPageRowCount]);
    fDatabase.TransactionBegin(TSQLCompareGroup);
    try
      while Group.FillOne do
      begin
        GroupRowNo := GroupRowNo + 1;
        Group.Enabled := GroupRowNo <= fGroupCount;
        if not Group.Enabled and (fGroupRowCount > 0) then
          Group.Enabled := Group.RowCount >= fGroupRowCount;
        Group.ReEnabled := False;
        Group.ReEnabledFirstRow := 0;
        Group.ReEnabledFirstRow2 := 0;
        Group.ReEnabledRowSpacing := 0;
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
    Inc(PageIndex);
  until Group.FillTable.RowCount = 0;
  //计算可导出首行
  TSQLFirstRow.AutoFree(FirstRow, fDatabase, '', []);
  TSQLFirstRow.AutoFree(FirstRow2);
  TSQLCompareData.AutoFree(Data);
  fDatabase.TransactionBegin(TSQLFirstRow);
  try
    LastEnabledFirstRow := 0;
    RecalcRowSpacing := False;
    while FirstRow.FillOne do
    begin
      //首行数不符合的代号不导出
      DataRowCount := FirstRow.RowCount;
      PageIndex := 0;
      repeat
        s := 'FirstRow = ? LIMIT ? OFFSET ?';
        Data.FillPrepare(fDatabase, s, [FirstRow.Value, EachPageRowCount, PageIndex * EachPageRowCount]);
        while Data.FillOne do
        begin
          Data.Enabled := False;
          Group.FillPrepare(fDatabase, 'Value = ?', [Data.GroupValue]);
          if Group.FillOne then
            Data.Enabled := Group.Enabled;
          if not Data.Enabled then DataRowCount := DataRowCount - 1;
          fDatabase.Update(Data);
        end;
        Inc(PageIndex);
      until Data.FillTable.RowCount = 0;
      //没有代号的首行不显示
      FirstRow.RowSpacing2 := FirstRow.RowSpacing;
      FirstRow.RowCount2 := DataRowCount;
      FirstRow.Enabled := DataRowCount > 0;
      if FirstRow.Enabled then
      begin
        //计算临行距
        if RecalcRowSpacing then
        begin
          if LastEnabledFirstRow > 0 then
            FirstRow.RowSpacing2 := FirstRow.Value - LastEnabledFirstRow
          else
            FirstRow.RowSpacing2 := FirstRow.Value - fVertSlantCompareSpacing - 1;
          //如果临行距大于最小临行距，上一不显示首行恢复显示
          if (fKeepMaxRowSpacing > 0) and (FirstRow.RowSpacing2 > fKeepMaxRowSpacing) then
          begin
            FirstRow.RowSpacing2 := FirstRow.RowSpacing;
            if FirstRow.FillRow(FirstRow.FillTable.RowFromID(LastNotEnabledFirstRowID), FirstRow2) then;
            begin
              FirstRow2.Enabled := True;
              fDatabase.Update(FirstRow2);

              ReEnabledFirstCompareData(FirstRow2.Value, FirstRow.Value, LastEnabledFirstRow);
            end;
          end;
        end;
        LastEnabledFirstRow := FirstRow.Value;
      end
      else
      begin
        if FirstRow.FillCurrentRow > FirstRow.FillTable.RowCount then
        begin
          //如果最大首行不显示导致下一首行临行距大于最小临行距，最大首行恢复显示
          if (fKeepMaxRowSpacing > 0)
            and (LastEnabledFirstRow > 0)
            and (fRowCount + 1 - LastEnabledFirstRow > fKeepMaxRowSpacing)
          then
          begin
            FirstRow.RowSpacing2 := FirstRow.RowSpacing;
            FirstRow.Enabled := True;

            ReEnabledFirstCompareData(FirstRow.Value, fRowCount + 1, LastEnabledFirstRow);
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
  //计算代号情况
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

function TDataComputer.RebuildFile(RowCount: Cardinal): Boolean;
begin
  Result := RowCount >= EachFileRowCount;
end;

function TDataComputer.NumberToString(Value: Cardinal): string;
var
  i: Integer;
begin
  Result := '';
  i := Value div 100000000;
  Value := Value mod 100000000;
  if i > 0 then Result := Result + Format(' %d 亿', [i]);
  i:= Value div 10000;
  Value := Value mod 10000;
  if i > 0 then Result := Result + Format(' %d 万', [i]);
  if Value > 0 then
    Result := Result + Format(' %d', [Value]);
end;

function TDataComputer.RebuildFileName(Sender: TFileWriter): string;
var
  sSub: string;
  RowCount: Cardinal;
  i: Integer;
begin
  RowCount := Sender.FileNo * EachFileRowCount;
  if Sender.LastFileNo then
    RowCount := (Sender.FileNo - 1) * EachFileRowCount + Sender.RowCount;
  sSub := NumberToString(RowCount);

  i := Sender.FileName.IndexOf('.');
  Result := Sender.FileName.SubString(0, i);
  Result := Result + Format('.【最末第%s（空白及文字）行】- %d', [sSub, Sender.FileNo - 1]);
end;

function TDataComputer.RebuildFileName2(Sender: TFileWriter): string;
begin
  Result := RebuildFileName2(Sender, 0);
end;

function TDataComputer.RebuildFileName2(Sender: TFileWriter; aRowNumber: Cardinal): string;
var
  sSub, sSub2: string;
  RowNumber: Cardinal;
  i: Integer;
begin
  RowNumber := Sender.FileNo * EachFileRowNumber;
  if Sender.LastFileNo then RowNumber := aRowNumber;
  sSub := NumberToString(RowNumber);

  i := Sender.FileName.IndexOf('.');
  Result := Sender.FileName.SubString(0, i);
  if Result.Equals('（2-2）')
    or Result.Equals('（2-3）')
    or Result.Equals('（2-4）')
    or Result.Equals('（2-5）')
  then
  begin
    sSub2 := Result.Replace('2-', '');
    Result := Result + Format('.【最末第%s = 行】%s- %d', [sSub, sSub2, Sender.FileNo - 1])
  end;
end;

procedure TDataComputer.WriteGroupReEnableTips(fr: TFileWriter; Group: TSQLCompareGroup);
var
  s: string;
begin
  s := '保留原因 ：';
  fr.WriteLn('');
  fr.WriteLn(s);
  s := '上一个：[ %s连（第 %d 行为首行）] （ 减去- ）下一个：[ %s连（第 %d 行为首行）] = [ 邻行距↓ %d ] ；';
  s := Format(s, [
    CompareModeString,
    Group.ReEnabledFirstRow,
    CompareModeString,
    Group.ReEnabledFirstRow2,
    Group.ReEnabledRowSpacing
  ]);
  fr.WriteLn('');
  fr.WriteLn(s);
  s := '再（ 减去- ）【 配套 】1.保持[（1）.【排列】【...】]（最大邻行距↓）：（小、等于 ≤）【 %d 】= [ 邻行距↓ %d ] 。';
  s := Format(s, [Group.KeepMaxRowSpacing, Group.ReEnabledRowSpacing - Group.KeepMaxRowSpacing]);
  fr.WriteLn('');
  fr.WriteLn(s);
end;

procedure TDataComputer.SaveSlantCompareType;
var
  fr: TFileWriter;
  s, FileName, TxtFileName: string;
  Group: TSQLCompareGroup;
  PageIndex: Integer;
  GroupRowNo: Cardinal;
begin
  TxtFileName := '（6）.【保存】“%d”个以上各个组合（相同代号、不同首行）的（代号：NZ. NY ）.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  if TFile.Exists(FileName) then TFile.Delete(FileName);
  fr := TFileWriter.Create(FileName);
  fr.RebuildFileEvent := RebuildFile;
  fr.RebuildFileNameEvent := RebuildFileName;
  try
    TSQLCompareGroup.AutoFree(Group);
    PageIndex := 0;
    GroupRowNo := 0;
    repeat
      s := 'ORDER BY ValueCount, Value2 LIMIT ? OFFSET ?';
      if fRecalcMode > 0 then s := 'Enabled = 1 ' + s;
      Group.FillPrepare(fDatabase, s, [EachPageRowCount, PageIndex * EachPageRowCount]);
      while Group.FillOne do
      begin
        GroupRowNo := GroupRowNo + 1;
        s := Format('%d=%s', [GroupRowNo, BuildSlantGroupValue(Group.Value)]);
        fr.WriteLn(s);
      end;
      Inc(PageIndex);
    until Group.FillTable.RowCount = 0;
    fr.RenameLastFile;
  finally
    fr.Free;
  end
end;

procedure TDataComputer.SaveVertGroupByCompareTypeSortByLastFirstRow2;
var
  fr: TFileWriter;
  s, FileName, TxtFileName: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
  RowNo: Word;
  PageIndex: Integer;
begin
  if fExportLite then TxtFileName := '（2-4）.（4）.txt'
  else TxtFileName := '（2-4）.【排列】【“%d”个以上[相同组合、不同首行]的组合 [首行邻行距：最小↓→大↓]】（4）.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory + TxtFileName;
  fr := TFileWriter.Create(FileName);
  fr.RebuildFileNameEvent := RebuildFileName2;
  try
    if not fExportLite then
    begin
      s := TPath.GetFileNameWithoutExtension(FileName);
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);
    end;

    TSQLCompareGroup.AutoFree(Group);
    TSQLCompareData.AutoFree(Data);
    RowNo := 0;
    PageIndex := 0;
    repeat
      s := 'ORDER BY LastFirstRow DESC, ValueCount, Value2 LIMIT ? OFFSET ?';
      if fRecalcMode > 0 then s := 'Enabled = 1 ' + s;
      Group.FillPrepare(fDatabase, s, [EachPageRowCount, PageIndex * EachPageRowCount]);
      while Group.FillOne do
      begin
        Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
        while Data.FillOne do
        begin
          if Data.FillCurrentRow = 2 then
          begin
            RowNo := RowNo + 1;
            s := '%d= 第%d次组合[ 代号：1.（%s）]：（%d）（%d）	';
            s := Format(s, [RowNo, Data.GroupValueCount, Group.Value, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
            fr.WriteLn('');
            fr.WriteLn('');
            fr.WriteLn('');
            fr.WriteLn(s);

            if RowNo mod EachFileRowNumber = 0 then fr.BuildActiveFileName;
          end;

          RowNo := RowNo + 1;
          s := '%d= 第%d次组合[ 代号：1.（%s）]：（%d）（%d）	';
          s := Format(s, [RowNo, Data.GroupValueCount, Group.Value, Data.FirstRow, Data.RowSpacing]);
          fr.WriteLn('');
          fr.WriteLn(s);

          SaveCompareData(fr, Data, False);

          if RowNo mod EachFileRowNumber = 0 then fr.BuildActiveFileName;

          if Data.FillCurrentRow > Data.FillTable.RowCount then
          begin
            RowNo := RowNo + 1;
            s := '%d= 第%d次组合[ 代号：1.（%s）]：（%d）（0）	';
            s := Format(s, [RowNo, Data.GroupValueCount, Group.Value, fVertCompareSpacing + 1]);
            fr.WriteLn('');
            fr.WriteLn(s);

            if RowNo mod EachFileRowNumber = 0 then fr.BuildActiveFileName;
          end;
        end;
      end;
      Inc(PageIndex);
    until Group.FillTable.RowCount = 0;
    fr.WriteFinish;
    fr.RenameLastFile(RebuildFileName2(fr, RowNo));
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
  PageIndex: Integer;
begin
  if fExportLite then TxtFileName := '（6）..txt'
  else TxtFileName := '（6）.【简化】【“%d”个以上[相同组合、不同首行]的组合】.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  fr := TFileWriter.Create(FileName);
  fr.RebuildFileEvent := RebuildFile;
  fr.RebuildFileNameEvent := RebuildFileName;
  try
    if not fExportLite then
    begin
      s := TxtFileName.SubString(0, TxtFileName.Length - 4);
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);
    end;

    TSQLCompareGroup.AutoFree(Group);
    TSQLCompareData.AutoFree(Data);
    PageIndex := 0;
    repeat
      s := 'ORDER BY MaxValueCount DESC, MaxTotalValueCount - MaxValueCount DESC, ValueCount, Value2 LIMIT ? OFFSET ?';
      //if fRecalcMode > 0 then s := 'Enabled = 1 ' + s;
      Group.FillPrepare(fDatabase, s, [EachPageRowCount, PageIndex * EachPageRowCount]);
      while Group.FillOne do
      begin
        s := '%d.[ 第%d次组合；代号：1.（%s）]；（ w无【%s列】总数：';
        s := Format(s, [Group.FillCurrentRow - 1, Group.ValueCount, Group.Value, fTipStr]);
        if Length(fIntervalValues) = 1 then
          s := s + Format('%d列', [Group.MaxTotalValueCount])
        else
          s := s + Format('%d-%d列', [Group.MaxValueCount, Group.MaxTotalValueCount - Group.MaxValueCount]);
        s := s + '）：';
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
      Inc(PageIndex);
    until Group.FillTable.RowCount = 0;
    fr.RenameLastFile;
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.SaveCompareData(fr: TFileWriter; Data: TSQLCompareData; SaveValues: Boolean = True);
var
  Row: TSQLRow;
  i, i2: Integer;
  s, sCompareType, sCompareType2, sFirstRow: string;
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
          sCompareType2 := sCompareType;
          if fCompareMode = cmVertSlant then sCompareType2 := ' ' + sCompareType2;
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
      sFirstRow := '';
      if not fExportLite then
      begin
        sFirstRow := '（第%d行为首行）%s连（第%d行）';
        sFirstRow := Format(sFirstRow, [Data.FirstRow, sCompareType, RowNumber]);
      end;
      s := Format('（%d）【%s（第%d%s连行）】', [
        Number,
        sFirstRow,
        Data.FirstRow - RowNumber,
        sCompareType2
      ]);
      if SaveValues then
      begin
        s := s + '= ';
        {if TotalSameValueCount > 0 then
        begin
          if CompareType = 1 then s := s + 'y'
          else s := s + ' ';
          if Length(fIntervalValues) = 1 then
            s := s + Format('有【对应列】数：%d列：', [TotalSameValueCount])
          else
            s := s + Format('有【对应列】数；【 %d-%d 】列：', [SameValueCount, TotalSameValueCount - SameValueCount]);
          s := s + DataToString(SameValues);
        end;}
        if TotalDifferentValueCount > 0 then
        begin
          if TotalSameValueCount > 0 then s := s + '  ';
          if CompareType = 1 then s := s + 'w无'
          else s := s + ' ';
          if not fExportLite then s := s + '【对应列】数：';
          if Length(fIntervalValues) = 1 then
            s := s + Format('%d列', [TotalDifferentValueCount])
          else
            s := s + Format('【 %d-%d 】列', [DifferentValueCount, TotalDifferentValueCount - DifferentValueCount]);
          //if not fExportLite then s := s + '：' + DataToString(DifferentValues);
          if not fExportLite then s := s + '：' + TSQLData.ToString(DifferentValues, fIntervalValues, fDataMode);
        end;
      end;
      fr.WriteLn('');
      fr.WriteLn(s);
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
      if (fCompareMode = cmSlant) and s.Contains('，同行数') then
        s := s.Substring(0, s.IndexOf('，同行数')) + '）';
      RowSpacingList.Values[sRowSpacing] := s;
    end;
  end;

var
  fr: TFileWriter;
  s, FileName, TxtFileName, sRowSpacing, sGroupValueCount, sFirstRow: string;
  i, PageIndex: Integer;
  FirstRow: TSQLFirstRow;
  Data: TSQLCompareData;
  LastGroupValueCount: Byte;
  GroupNumber: Word;
  RowNo: Cardinal;
begin
  if fExportLite then TxtFileName := '（1）..txt'
  else
    case fCompareMode of
      cmSlant: TxtFileName := '（1）.【排列】“%d”个以上【 [ 相同（第“N”行为首行）] 、（不同代号）】的组合.txt';
      else TxtFileName := '（1）.【排列】【“%d”个以上[相同首行、不同组合]的组合】.txt';;
    end;
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory + TxtFileName;
  fr := TFileWriter.Create(FileName);
  fr.RebuildFileEvent := RebuildFile;
  fr.RebuildFileNameEvent := RebuildFileName;
  try
    if not fExportLite then
    begin
      s := TPath.GetFileNameWithoutExtension(FileName);
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);
    end;

    TSQLFirstRow.AutoFree(FirstRow);
    TSQLCompareData.AutoFree(Data);

    s := 'ORDER BY Value DESC';
    if fRecalcMode > 0 then s := 'Enabled = 1 ' + s;
    FirstRow.FillPrepare(fDatabase, s, []);
    RowSpacingList := TStringList.Create;
    try
      while FirstRow.FillOne do
      begin
        if FirstRow.FillCurrentRow = 2 then
        begin
          case fCompareMode of
            cmSlant:
            begin
              sFirstRow := '';
              if not fExportLite then
              begin
                sFirstRow := '（第%d行为首行）';
                sFirstRow := Format(sFirstRow, [fRowCount + 1]);
              end;
              s := '1.%s（邻行距 ↓%d）';
              s := Format(s, [sFirstRow, fRowCount + 1 - FirstRow.Value]);
            end;
            else
            begin
              sFirstRow := '';
              if not fExportLite then
              begin
                sFirstRow := '[ %s连（第%d行为首行）]；';
                sFirstRow := Format(sFirstRow, [CompareModeString, fRowCount + 1]);
              end;
              s := '1.%s[ 邻行距 ↓%d ]';
              s := Format(s, [sFirstRow, fRowCount + 1 - FirstRow.Value]);
            end;
          end;
          AddRowSpacing(fRowCount + 1 - FirstRow.Value, s);
        end;

        if fRecalcMode > 0 then
        begin
          FirstRow.RowSpacing := FirstRow.RowSpacing2;
          FirstRow.RowCount := FirstRow.RowCount2;
        end;
        case fCompareMode of
          cmSlant:
          begin
            sFirstRow := '';
            if not fExportLite then
            begin
              sFirstRow := '（第%d行为首行）';
              sFirstRow := Format(sFirstRow, [FirstRow.Value]);
            end;
            s := '%d.%s（邻行距 ↓%d，同行数：%d）';
            s := Format(s, [FirstRow.FillCurrentRow, sFirstRow, FirstRow.RowSpacing, FirstRow.RowCount]);
          end;
          else
          begin
            sFirstRow := '';
            if not fExportLite then
            begin
              sFirstRow := '[ %s连（第%d行为首行）]；';
              sFirstRow := Format(sFirstRow, [CompareModeString, FirstRow.Value]);
            end;
            s := '%d.%s[ 邻行距 ↓%d ]';
            s := Format(s, [FirstRow.FillCurrentRow, sFirstRow, FirstRow.RowSpacing]);
          end;
        end;
        AddRowSpacing(FirstRow.RowSpacing, s);
      end;

      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn('最大-小的邻行距 1-100 行内：');
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
        case fCompareMode of
          cmSlant:
          begin
            sFirstRow := '';
            if not fExportLite then
            begin
              sFirstRow := '（第%d行为首行）；';
              sFirstRow := Format(sFirstRow, [fRowCount + 1]);
            end;
            s := '1.%s（邻行距 ↓%d）';
            s := Format(s, [sFirstRow, fRowCount + 1 - FirstRow.Value]);
          end;
          else
          begin
            sFirstRow := '';
            if not fExportLite then
            begin
              sFirstRow := '[ %s连（第%d行为首行）]；';
              sFirstRow := Format(sFirstRow, [CompareModeString, fRowCount + 1]);
            end;
            s := '1.%s[ 邻行距 ↓%d ]';
            s := Format(s, [sFirstRow, fRowCount + 1 - FirstRow.Value]);
          end;
        end;
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn(s);
      end;

      if fRecalcMode > 0 then
      begin
        FirstRow.RowSpacing := FirstRow.RowSpacing2;
        FirstRow.RowCount := FirstRow.RowCount2;
        FirstRow.GroupValueCount := FirstRow.GroupValueCount2;
        FirstRow.MaxGroupValueCount := FirstRow.MaxGroupValueCount2;
        FirstRow.MaxRowCountGroupValueCount := FirstRow.MaxRowCountGroupValueCount2;
        FirstRow.MaxGroupValueCountRowCount := FirstRow.MaxGroupValueCountRowCount2;
      end;
      case fCompareMode of
        cmSlant:
        begin
          sFirstRow := '';
          if not fExportLite then
          begin
            sFirstRow := '（第%d行为首行）';
            sFirstRow := Format(sFirstRow, [FirstRow.Value]);
          end;
          s := '%d.%s（邻行距 ↓%d，同行数：%d）';
          s := Format(s, [FirstRow.FillCurrentRow, sFirstRow, FirstRow.RowSpacing, FirstRow.RowCount]);
        end;
        else
        begin
          sFirstRow := '';
          if not fExportLite then
          begin
            sFirstRow := '[ %s连（第%d行为首行）]；';
            sFirstRow := Format(sFirstRow, [CompareModeString, FirstRow.Value]);
          end;
          s := '%d.%s[ 邻行距 ↓%d ；';
          s := Format(s, [FirstRow.FillCurrentRow, sFirstRow, FirstRow.RowSpacing]);
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
        end;
      end;
      if FirstRow.FillCurrentRow > 2 then
      begin
        fr.WriteLn('');
        fr.WriteLn('');
      end;
      fr.WriteLn('');
      fr.WriteLn(s);

      PageIndex := 0;
      RowNo := 0;
      repeat
        s := 'FirstRow = ?';
        if fRecalcMode > 0 then s := s + ' AND Enabled = 1';
        s := s + ' LIMIT ? OFFSET ?';
        Data.FillPrepare(fDatabase, s, [FirstRow.Value, EachPageRowCount, PageIndex * EachPageRowCount]);
        LastGroupValueCount := 0;
        GroupNumber := 0;
        while Data.FillOne do
        begin
          RowNo := RowNo + 1;
          case fCompareMode of
            cmSlant:
            begin
              s := Format('（%d）.[代号：（第%s个）%s ]', [
                RowNo,
                Data.GroupValue,
                BuildSlantGroupValue(Data.GroupValue)
              ]);
              if Length(fIntervalValues) = 1 then
                s := s + Format(' = 无【%s列】数： %d列 ；', [fTipStr, Data.TotalValueCount])
              else
                s := s + Format(' = 【 %d-%d 】列 ；', [Data.ValueCount, Data.TotalValueCount - Data.ValueCount]);
              //s := s + Format('【列数字】：%s', [DataToString(Data.Values)]);
              s := s + Format('【列数字】：%s', [Data.ToString(fDataMode)]);
              fr.WriteLn('');
              fr.WriteLn(s);
            end;
            else
            begin
              if Data.GroupValueCount <> LastGroupValueCount then
              begin
                LastGroupValueCount := Data.GroupValueCount;
                GroupNumber := 0;
              end;
              GroupNumber := GroupNumber + 1;

              s := '【%d】.第%d次组合[ 代号：%d.（%s）]：';
              s := Format(s, [
                RowNo,
                Data.GroupValueCount,
                GroupNumber,
                Data.GroupValue
              ]);
              fr.WriteLn('');
              fr.WriteLn(s);

              SaveCompareData(fr, Data);
            end;
          end;
        end;
        Inc(PageIndex);
      until Data.FillTable.RowCount = 0;
    end;
    case fCompareMode of
      cmVert:
      begin
        s := '%d.[（第%d行为最末首行）直连（第%d-1行）]';
        s := Format(s, [
          FirstRow.FillTable.RowCount + 2,
          fVertCompareSpacing + 1,
          fVertCompareSpacing
        ]);
        fr.WriteLn('');
        fr.WriteLn(s);
      end;
      cmSlant:
      begin
        s := '%d.（第%d行为最末首行）';
        s := Format(s, [
          FirstRow.FillTable.RowCount + 2,
          fSlantCompareSpacing + 1
        ]);
        fr.WriteLn('');
        fr.WriteLn(s);
      end;
      cmVertSlant:
      begin
        s := '%d.[（第%d行为最末首行）直连（第%d-%d行）]';
        s := Format(s, [
          FirstRow.FillTable.RowCount + 2,
          fVertSlantCompareSpacing + 1,
          fVertSlantCompareSpacing,
          fVertSlantCompareSpacing + 1 - fVertCompareSpacing
        ]);
        fr.WriteLn('');
        fr.WriteLn(s);
        s := '%d.[（第%d行为最末首行）斜连（第%d-%d行）]';
        s := Format(s, [
          FirstRow.FillTable.RowCount + 3,
          fVertSlantCompareSpacing + 1,
          fVertSlantCompareSpacing,
          fVertSlantCompareSpacing + 1 - fSlantCompareSpacing
        ]);
        fr.WriteLn('');
        fr.WriteLn(s);
      end;
    end;

    fr.RenameLastFile;
    if efFile in fExportFiles then
      for s in fr.Files do
        if TFile.Exists(s) then TFile.Copy(s, fExportDirectory2 + TPath.GetFileName(s), True);
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.SaveGroupByCompareTypeSortByRowcount;
var
  fr, fr2, fr3: TFileWriter;
  s, sCompareMode, SlantGroupValue, FileName, FileName2, FileName3, TxtFileName: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
  RowNo, GroupRowNo: Cardinal;
  LastFirstRow: Cardinal;
  PageIndex: Integer;
begin
  if fExportLite then TxtFileName := '（2-1）.（1）.txt'
  else
    case fCompareMode of
      cmSlant: TxtFileName := '（2-1）.【排列】“%d”个以上各个组合（相同代号、不同首行）的（不同首行数：最多 - 最少行）（1）.txt';
      else TxtFileName := '（2-1）.【排列】【“%d”个以上[相同组合、不同首行]的组合[不同首行数：最多→少]】（1）.txt';
    end;
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory + TxtFileName;
  if fExportLite then TxtFileName := '（2-2）.（2）.txt'
  else
    case fCompareMode of
      cmSlant: TxtFileName :='（2-2）.【排列】“%d”个以上各个组合（相同代号、不同首行）的（不同首行数：最多 - 最少行）（2）.txt';
      else TxtFileName := '（2-2）.【排列】【“%d”个以上[相同组合、不同首行]的组合[不同首行数：最多→少]】（2）.txt';
    end;
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName2 := fExportDirectory + TxtFileName;
  if fExportLite then TxtFileName := '（2-3）.（3）.txt'
  else
    case fCompareMode of
      cmSlant: TxtFileName := '（2-3）.【排列】“%d”个以上各个组合（相同代号、不同首行）的（不同首行数：最多 - 最少行）（3）.txt';
      else TxtFileName := '（2-3）.【排列】【“%d”个以上[相同组合、不同首行]的组合[不同首行数：最多→少]】（3）.txt';
    end;
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName3 := fExportDirectory + TxtFileName;
  if efFile11 in fExportFiles then
  begin
    fr := TFileWriter.Create(FileName);
    fr.RebuildFileEvent := RebuildFile;
    fr.RebuildFileNameEvent := RebuildFileName;
  end;
  if efFile12 in fExportFiles then
  begin
    fr2 := TFileWriter.Create(FileName2);
    fr2.RebuildFileNameEvent := RebuildFileName2;
  end;
  if efFile13 in fExportFiles then
  begin
    fr3 := TFileWriter.Create(FileName3);
    fr3.RebuildFileNameEvent := RebuildFileName2;
  end;
  try
    if not fExportLite then
    begin
      if Assigned(fr) then
      begin
        s := TPath.GetFileNameWithoutExtension(FileName);
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn(s);
      end;
      if Assigned(fr2) then
      begin
        s := TPath.GetFileNameWithoutExtension(FileName2);
        fr2.WriteLn('');
        fr2.WriteLn('');
        fr2.WriteLn(s);
      end;
      if Assigned(fr3) then
      begin
        s := TPath.GetFileNameWithoutExtension(FileName3);
        fr3.WriteLn('');
        fr3.WriteLn('');
        fr3.WriteLn(s);
        if fCompareMode = cmSlant then
        begin
          fr3.WriteLn('');
          fr3.WriteLn('');
          fr3.WriteLn('');
        end;
      end;
    end;

    sCompareMode := '';
    if not fExportLite then
    begin
      case fCompareMode of
        cmSlant: sCompareMode := '（斜连）';
        else sCompareMode := Format('（%s连）', [CompareModeString]);
      end;
    end;

    TSQLCompareGroup.AutoFree(Group);
    TSQLCompareData.AutoFree(Data);
    RowNo := 0;
    GroupRowNo := 0;
    PageIndex := 0;
    repeat
      s := 'ORDER BY RowCount DESC, ValueCount, Value2 LIMIT ? OFFSET ?';
      if fRecalcMode > 0 then s := 'Enabled = 1 ' + s;
      Group.FillPrepare(fDatabase, s, [EachPageRowCount, PageIndex * EachPageRowCount]);
      while Group.FillOne do
      begin
        GroupRowNo := GroupRowNo + 1;
        case fCompareMode of
          cmSlant:
          begin
            SlantGroupValue := BuildSlantGroupValue(Group.Value);

            s := '%d.[代号：（第%s个）%s ] ；[ 不同%s首行数：%d行 ]';
            s := Format(s, [GroupRowNo, Group.Value, SlantGroupValue, sCompareMode, Group.RowCount]);
          end;
          else
          begin
            s := '%d.【“%d”个 [ 相同组合、不同%s首行]的组合 [ 不同%s首行数：%d ]】：';
            s := Format(s, [
              GroupRowNo,
              Group.ValueCount,
              sCompareMode,
              sCompareMode,
              Group.RowCount
            ]);
          end;
        end;
        if Assigned(fr) then
        begin
          fr.WriteLn('');
          fr.WriteLn('');
          fr.WriteLn('');
          fr.WriteLn(s);
        end;
        if Assigned(fr2) then
        begin
          fr2.WriteLn('');
          fr2.WriteLn('');
          fr2.WriteLn('');
        end;
        case fCompareMode of
          cmSlant:
          begin
            if Assigned(fr2) then
            begin
              fr2.WriteLn(s);
              fr2.WriteLn('');
            end;
          end;
          cmVertSlant:
          begin
            if Assigned(fr2) then fr2.WriteLn(s);
            if Assigned(fr3) then
            begin
              fr3.WriteLn('');
              fr3.WriteLn('');
              fr3.WriteLn('');
            end;
          end;
        end;

        LastFirstRow := 0;
        s := 'GroupValue = ? ORDER BY FirstRow DESC';
        //if (fRecalcMode = 1) and fHideSameGroup then s := s + ' LIMIT 1';
        Data.FillPrepare(fDatabase, s, [Group.Value]);
        while Data.FillOne do
        begin
          if Data.FillCurrentRow = 2 then
          begin
            RowNo := RowNo + 1;

            if Assigned(fr2) then
            begin
              case fCompareMode of
                cmSlant:
                begin
                  s := '1=[（第%s个）%s ] ：（%d）（%d）';
                  s := Format(s, [Group.Value, SlantGroupValue, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
                end;
                else
                begin
                  s := '%d= 第%d次组合[ 代号：3.（%s）]：（%d）（%d）';
                  s := Format(s, [RowNo, Group.ValueCount, Group.Value, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
                  if Assigned(fr2) then fr2.WriteLn('');
                end;
              end;
              fr2.WriteLn(s);
              if RowNo mod EachFileRowNumber = 0 then fr2.BuildActiveFileName;
            end;

            if Assigned(fr3) then
            begin
              case fCompareMode of
                cmSlant:
                begin
                  s := '%d=[（第%s个）%s ] ：（%d）（%d）';
                  s := Format(s, [RowNo, Group.Value, SlantGroupValue, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
                end;
                cmVertSlant:
                begin
                  s := '%d=[ 代号：3.（%s）]：（%d）（%d）';
                  s := Format(s, [RowNo, Group.Value, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
                  if Assigned(fr3) then fr3.WriteLn('');
                end;
              end;
              fr3.WriteLn(s);
              if RowNo mod EachFileRowNumber = 0 then fr3.BuildActiveFileName;
            end;
          end;

          RowNo := RowNo + 1;
          case fCompareMode of
            cmSlant:
            begin
              if Assigned(fr) then
              begin
                s := Format('（%d）（第%d行为首行）', [Data.FillCurrentRow - 1, Data.FirstRow]);
                if Length(fIntervalValues) = 1 then
                  s := s + Format('= 无【%s列】数： %d列 ；', [fTipStr, Data.TotalValueCount])
                else
                  s := s + Format('= 无【%s列】数：%d - %d 列 ；', [fTipStr, Data.ValueCount, Data.TotalValueCount - Data.ValueCount]);
                //s := s + Format('【列数字】：%s', [DataToString(Data.Values)]);
                s := s + Format('【列数字】：%s', [Data.ToString(fDataMode)]);
                fr.WriteLn('');
                fr.WriteLn(s);
              end;

              if Assigned(fr2) then
              begin
                s := '%d=[（第%s个）%s ] ：（%d）（%d）';
                s := Format(s, [Data.FillCurrentRow, Group.Value, SlantGroupValue, Data.FirstRow, Data.RowSpacing]);
                fr2.WriteLn(s);
                if RowNo mod EachFileRowNumber = 0 then fr2.BuildActiveFileName;
              end;

              if Assigned(fr3) then
              begin
                s := '%d=[（第%s个）%s ] ：（%d）（%d）';
                s := Format(s, [RowNo, Group.Value, SlantGroupValue, Data.FirstRow, Data.RowSpacing]);
                fr3.WriteLn(s);
                if RowNo mod EachFileRowNumber = 0 then fr3.BuildActiveFileName;
              end;
            end;
            else
            begin
              if Assigned(fr) then
              begin
                if LastFirstRow <> Data.FirstRow then
                begin
                  LastFirstRow := Data.FirstRow;
                  s := '【%d】.第%d次组合[ 代号：1.（%s）]：';
                  s := Format(s, [Data.FillCurrentRow - 1, Group.ValueCount, Group.Value]);
                  fr.WriteLn('');
                  fr.WriteLn(s);
                end;
                SaveCompareData(fr, Data);
              end;

              if Assigned(fr2) then
              begin
                s := '%d= 第%d次组合[ 代号：1.（%s）]：（%d）（%d）';
                s := Format(s, [RowNo, Group.ValueCount, Group.Value, Data.FirstRow, Data.RowSpacing]);
                fr2.WriteLn('');
                fr2.WriteLn(s);
                SaveCompareData(fr2, Data);
                if RowNo mod EachFileRowNumber = 0 then fr2.BuildActiveFileName;
              end;

              if (fCompareMode = cmVertSlant) and Assigned(fr3) then
              begin
                s := '%d=[ 代号：1.（%s）]：（%d）（%d）';
                s := Format(s, [RowNo, Group.Value, Data.FirstRow, Data.RowSpacing]);
                fr3.WriteLn('');
                fr3.WriteLn(s);
                SaveCompareData(fr3, Data, False);
                if RowNo mod EachFileRowNumber = 0 then fr3.BuildActiveFileName;
              end;
            end;
          end;

          if Data.FillCurrentRow > Data.FillTable.RowCount then
          begin
            RowNo := RowNo + 1;

            case fCompareMode of
              cmSlant:
              begin
                if Assigned(fr2) then
                begin
                  s := '%d=[（第%s个）%s ] ：（%d）（0）';
                  s := Format(s, [Data.FillCurrentRow + 1, Group.Value, SlantGroupValue, fSlantCompareSpacing + 1]);
                  fr2.WriteLn(s);
                  if RowNo mod EachFileRowNumber = 0 then fr2.BuildActiveFileName;
                end;

                if Assigned(fr3) then
                begin
                  s := '%d=[（第%s个）%s ] ：（%d）（0）';
                  s := Format(s, [RowNo, Group.Value, SlantGroupValue, fSlantCompareSpacing + 1]);
                  fr3.WriteLn(s);
                  if RowNo mod EachFileRowNumber = 0 then fr3.BuildActiveFileName;
                end;
              end
              else
              begin
                if Assigned(fr2) then
                begin
                  s := '%d= 第%d次组合[ 代号：1.（%s）]：（%d）（0）';
                  s := Format(s, [RowNo, Group.ValueCount, Group.Value, fVertSlantCompareSpacing + 1]);
                  fr2.WriteLn('');
                  fr2.WriteLn(s);
                  if RowNo mod EachFileRowNumber = 0 then fr2.BuildActiveFileName;
                end;

                if (fCompareMode = cmVertSlant) and Assigned(fr3) then
                begin
                  s := '%d=[ 代号：1.（%s）]：（%d）（0）';
                  s := Format(s, [RowNo, Group.Value, fVertSlantCompareSpacing + 1]);
                  fr3.WriteLn('');
                  fr3.WriteLn(s);
                  if RowNo mod EachFileRowNumber = 0 then fr3.BuildActiveFileName;
                end;
              end;
            end;
          end;
        end;
      end;
      Inc(PageIndex);
    until Group.FillTable.RowCount = 0;

    if Assigned(fr) then
    begin
      fr.RenameLastFile;
      if (efFile2 in fExportFiles) then
      begin
        for s in fr.Files do
        begin
          TxtFileName := TPath.GetFileName(s).Replace('（2-1）', '（2）');
          TxtFileName := TxtFileName.Replace('（1）', '');
          if TFile.Exists(s) then TFile.Copy(s, fExportDirectory2 + TxtFileName, True);
        end;
      end;
    end;
    if Assigned(fr2) then
    begin
      fr2.WriteFinish;
      fr2.RenameLastFile(RebuildFileName2(fr2, RowNo));
    end;
    if Assigned(fr3) then
    begin
      fr3.WriteFinish;
      fr3.RenameLastFile(RebuildFileName2(fr3, RowNo));
    end;
  finally
    if Assigned(fr) then fr.Free;
    if Assigned(fr2) then fr2.Free;
    if Assigned(fr3) then fr3.Free;
  end;
end;

procedure TDataComputer.SaveGroupByCompareTypeSortByRowcount2;
var
  fr, fr2: TFileWriter;
  s, FileName, FileName2, TxtFileName: string;
  Group, Group2: TSQLCompareGroup;
  CompareSpacing, RowNo, RowNo2, MaxRowCount, RowCountNumber, RowCountNumber2: Cardinal;
  PageIndex, SerialNo: Integer;
begin
  if fExportLite then TxtFileName := '（2）..txt'
  else TxtFileName := '（2）. 简化并统计：[（TXT）文本：（2-1）【排列】【“%d”个以上[相同组合、不同首行]的组合[不同首行数：最多→少]】].txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory + TxtFileName;

  case fCompareMode of
    cmVert: SerialNo := 7;
    cmSlant: SerialNo := 6;
    cmVertSlant: SerialNo := 8;
  end;
  if fExportLite then TxtFileName := '（2-00）..txt'
  else TxtFileName := '（2-00）.符合【%d.设置...】的【（TXT）文本：（2）.简化并统计...】保留原因的每个代号及其对应内容.txt';
  TxtFileName := Format(TxtFileName, [SerialNo]);
  FileName2 := fExportDirectory + TxtFileName;
  fr := TFileWriter.Create(FileName);
  fr.RebuildFileEvent := RebuildFile;
  fr.RebuildFileNameEvent := RebuildFileName;
  if efFile9 in fExportFiles then
  begin
    fr2 := TFileWriter.Create(FileName2);
    fr2.RebuildFileEvent := RebuildFile;
    fr2.RebuildFileNameEvent := RebuildFileName;
  end;
  try
    if not fExportLite then
    begin
      s := TPath.GetFileNameWithoutExtension(FileName) + '：';
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);
      if Assigned(fr2) then
      begin
        s := TPath.GetFileNameWithoutExtension(FileName2);
        fr2.WriteLn('');
        fr2.WriteLn('');
        fr2.WriteLn(s);
      end;
    end;

    TSQLCompareGroup.AutoFree(Group);
    TSQLCompareGroup.AutoFree(Group2);

    case fCompareMode of
      cmVert: CompareSpacing := fSlantCompareSpacing;
      cmSlant: CompareSpacing := fVertCompareSpacing;
      cmVertSlant: CompareSpacing := fVertSlantCompareSpacing;
    end;

    PageIndex := 0;
    RowNo := 0;
    RowNo2 := 0;
    MaxRowCount := 0;
    RowCountNumber := 0;
    RowCountNumber2 := 0;
    repeat
      s := 'ORDER BY RowCount DESC, ValueCount, Value2 LIMIT ? OFFSET ?';
      if fRecalcMode > 0 then s := 'Enabled = 1 ' + s;
      Group.FillPrepare(fDatabase, s, [EachPageRowCount + 1, PageIndex * EachPageRowCount]);
      while Group.FillOne do
      begin
        if Group.FillCurrentRow > EachPageRowCount + 1 then Break;

        RowNo := RowNo + 1;
        RowNo2 := RowNo2 + Group.RowCount + 2;
        RowCountNumber := RowCountNumber + 1;
        RowCountNumber2 := RowCountNumber2 + Group.RowCount;
        if MaxRowCount = 0 then MaxRowCount := Group.RowCount;
        if Group.FillCurrentRow <= Group.FillTable.RowCount then
          Group.FillRow(Group.FillCurrentRow, Group2);
        if (Group.RowCount <> Group2.RowCount)
          or ((Group.FillCurrentRow > Group.FillTable.RowCount) and (Group.FillTable.RowCount < EachPageRowCount + 1))
        then
        begin
          if Group.RowCount = 16 then
            s := '';

          s := '%d.【“%d”个 [ 相同组合、不同（%s连）首行]的组合 [ 不同（%s连）首行数：%d ]】：';
          s := Format(s, [
            RowNo,
            Group.ValueCount,
            CompareModeString,
            CompareModeString,
            Group.RowCount
          ]);
          fr.WriteLn('');
          fr.WriteLn('');
          fr.WriteLn('');
          fr.WriteLn(s);

          s := '%d=[ 代号：1.（%s）]：（%d）（0）	';
          s := Format(s, [RowNo2, Group.Value, CompareSpacing + 1]);
          fr.WriteLn('');
          fr.WriteLn(s);

          s := '统计：[ 不同（%s连）首行数：%d-%d ] ；共 %d 次、总共 ：%d 个 ；';
          s := Format(s, [CompareModeString, MaxRowCount, Group.RowCount, RowCountNumber2, RowCountNumber]);
          fr.WriteLn('');
          fr.WriteLn(s);

          if (fRecalcMode > 0) and Group.ReEnabled then WriteGroupReEnableTips(fr, Group);
        end;
      end;
      Inc(PageIndex);
    until Group.FillTable.RowCount = 0;

    s := TPath.GetFileNameWithoutExtension(FileName) + '（详细资料）：';
    fr.WriteLn('');
    fr.WriteLn('');
    fr.WriteLn('');
    fr.WriteLn('');
    fr.WriteLn('');
    fr.WriteLn('');
    fr.WriteLn('');
    fr.WriteLn('');
    fr.WriteLn(s);

    PageIndex := 0;
    RowNo := 0;
    RowNo2 := 0;
    MaxRowCount := 0;
    RowCountNumber := 0;
    RowCountNumber2 := 0;
    repeat
      s := 'ORDER BY RowCount DESC, ValueCount, Value2 LIMIT ? OFFSET ?';
      if fRecalcMode > 0 then s := 'Enabled = 1 ' + s;
      Group.FillPrepare(fDatabase, s, [EachPageRowCount, PageIndex * EachPageRowCount]);
      while Group.FillOne do
      begin
        RowNo := RowNo + 1;
        s := '%d.【“%d”个 [ 相同组合、不同（%s连）首行]的组合 [ 不同（%s连）首行数：%d ]】：';
        s := Format(s, [
          RowNo,
          Group.ValueCount,
          CompareModeString,
          CompareModeString,
          Group.RowCount
        ]);
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn(s);
        if (fRecalcMode > 0) and Group.ReEnabled and Assigned(fr2) then
        begin
          fr2.WriteLn('');
          fr2.WriteLn('');
          fr2.WriteLn('');
          fr2.WriteLn(s);
        end;

        RowNo2 := RowNo2 + Group.RowCount + 2;
        s := '%d=[ 代号：1.（%s）]：（%d）（0）	';
        s := Format(s, [RowNo2, Group.Value, CompareSpacing + 1]);
        fr.WriteLn('');
        fr.WriteLn(s);
        if (fRecalcMode > 0) and Group.ReEnabled and Assigned(fr2) then
        if Group.ReEnabled then
        begin
          fr2.WriteLn('');
          fr2.WriteLn(s);
        end;

        RowCountNumber := RowCountNumber + 1;
        RowCountNumber2 := RowCountNumber2 + Group.RowCount;
        s := '统计：[ 不同（%s连）首行数：%d-%d ] ；共 %d 次、总共 ：%d 个 ；';
        s := Format(s, [CompareModeString, MaxRowCount, Group.RowCount, RowCountNumber2, RowCountNumber]);
        fr.WriteLn('');
        fr.WriteLn(s);
        if (fRecalcMode > 0) and Group.ReEnabled and Assigned(fr2) then
        begin
          WriteGroupReEnableTips(fr, Group);

          fr2.WriteLn('');
          fr2.WriteLn(s);
          WriteGroupReEnableTips(fr2, Group);
        end;
      end;
      Inc(PageIndex);
    until Group.FillTable.RowCount = 0;
    fr.RenameLastFile;
    if Assigned(fr2) then fr2.RenameLastFile;
  finally
    fr.Free;
    if Assigned(fr2) then fr2.Free;
  end;
end;

procedure TDataComputer.SaveGroupByCompareTypeSortByMaxRowSpacing;
var
  fr: TFileWriter;
  s, FileName, TxtFileName: string;
  Group: TSQLCompareGroup;
  Data, Data2: TSQLCompareData;
  RowNo, GroupRowNo: Word;
  PageIndex: Integer;
begin
  if fExportLite then TxtFileName := '（3）..txt'
  else
    case fCompareMode of
      cmSlant: TxtFileName := '（3）.【排列】“%d”个以上各个组合（相同代号、不同首行）的（邻行距：最大↓“N”- 最小↓“N”）.txt';
      cmVert, cmVertSlant: TxtFileName := '（3）.【排列】【“%d”个以上[相同组合、不同首行]的组合[邻行距：最大↓→小↓]】.txt';
    end;
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  fr := TFileWriter.Create(FileName);
  fr.RebuildFileEvent := RebuildFile;
  fr.RebuildFileNameEvent := RebuildFileName;
  try
    if not fExportLite then
    begin
      s := TPath.GetFileNameWithoutExtension(FileName);
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);
    end;

    TSQLCompareGroup.AutoFree(Group);
    TSQLCompareData.AutoFree(Data);
    TSQLCompareData.AutoFree(Data2);
    GroupRowNo := 0;
    PageIndex := 0;
    repeat
      case fCompareMode of
        cmSlant: s := 'ORDER BY MaxRowSpacing DESC, ValueCount, Value2 LIMIT ? OFFSET ?';
        else s := 'ORDER BY MaxRowSpacing DESC, ValueCount DESC, Value2 LIMIT ? OFFSET ?';
      end;
      if fRecalcMode > 0 then s := 'Enabled = 1 ' + s;
      Group.FillPrepare(fDatabase, s, [EachPageRowCount, PageIndex * EachPageRowCount]);

      while Group.FillOne do
      begin
        GroupRowNo := GroupRowNo + 1;
        case fCompareMode of
          cmSlant:
          begin
            s := '%d.[代号：（第%s个）%s ] ；（最大邻行距 ↓%d）';
            s := Format(s, [
              GroupRowNo,
              Group.Value,
              BuildSlantGroupValue(Group.Value),
              Group.MaxRowSpacing
            ]);
            fr.WriteLn('');
            fr.WriteLn('');
            fr.WriteLn('');
            fr.WriteLn(s);
          end;
        end;

        RowNo := 2;
        Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
        while Data.FillOne do
        begin
          if Data.FillCurrentRow = 2 then
          begin
            case fCompareMode of
              cmSlant:
              begin
                s := '（1）（第%d行为首行）；（邻行距 ↓%d）';
                s := Format(s, [fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
                fr.WriteLn('');
                fr.WriteLn(s);
              end;
              cmVert, cmVertSlant:
              begin
                s := '%d.[ %s连（第%d行为首行）] .（最大邻行距 ↓%d ）：';
                s := Format(s, [GroupRowNo, CompareModeString, fRowCount + 1, Group.MaxRowSpacing]);
                fr.WriteLn('');
                fr.WriteLn('');
                fr.WriteLn('');
                fr.WriteLn(s);

                s := '（1）.[ %s连（第%d行为首行）] .（邻行距 ↓%d ）：';
                s := Format(s, [CompareModeString, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
                fr.WriteLn('');
                fr.WriteLn(s);

                s := '（2）.[ %s连（第%d行为首行）] .（邻行距 ↓%d ）：';
                s := Format(s, [CompareModeString, Data.FirstRow, Data.RowSpacing]);
                fr.WriteLn('');
                fr.WriteLn(s);
              end;
            end;
          end;

          case fCompareMode of
            cmSlant:
            begin
              s := Format('（%d）（第%d行为首行）（邻行距 ↓%d）', [Data.FillCurrentRow, Data.FirstRow, Data.RowSpacing]);
              if Length(fIntervalValues) = 1 then
                s := s + Format('= 无【%s列】数： %d列 ；', [fTipStr, Data.TotalValueCount])
              else
                s := s + Format('= 无【%s列】数：%d - %d 列 ；', [fTipStr, Data.ValueCount, Data.TotalValueCount - Data.ValueCount]);
              //s := s + Format('【列数字】：%s', [DataToString(Data.Values)]);
              s := s + Format('【列数字】：%s', [Data.ToString(fDataMode)]);
              fr.WriteLn('');
              fr.WriteLn(s);
            end;
            cmVert, cmVertSlant:
            begin
              s := '【%d】.第%d次组合[ 代号：3.（%s）]：';
              s := Format(s, [
                Data.FillCurrentRow - 1,
                Group.ValueCount,
                Group.Value
              ]);
              fr.WriteLn('');
              fr.WriteLn(s);

              SaveCompareData(fr, Data);
            end;
          end;

          RowNo := RowNo + 1;
          if Data.FillCurrentRow > Data.FillTable.RowCount then
          begin
            case fCompareMode of
              cmSlant:
              begin
                s := '（%d）（第%d行为最末首行）';
                s := Format(s, [Data.FillCurrentRow + 1, fSlantCompareSpacing + 1]);
                fr.WriteLn('');
                fr.WriteLn(s);
              end;
              cmVert:
              begin
                s := '（%d）.[（第%d行为最末首行）直连（第%d-1行）]';
                s := Format(s, [RowNo, fVertCompareSpacing + 1, fVertCompareSpacing]);
                fr.WriteLn('');
                fr.WriteLn(s);
              end;
              cmVertSlant:
              begin
                s := '（%d）.[（第%d行为最末首行）直连（第%d-%d行）]';
                s := Format(s, [RowNo, fVertSlantCompareSpacing + 1, fVertSlantCompareSpacing, fVertSlantCompareSpacing + 1 - fVertCompareSpacing]);
                fr.WriteLn('');
                fr.WriteLn(s);

                RowNo := RowNo + 1;
                s := '（%d）.[（第%d行为最末首行）斜连（第%d-%d行）]';
                s := Format(s, [RowNo, fVertSlantCompareSpacing + 1, fVertSlantCompareSpacing, fVertSlantCompareSpacing + 1 - fSlantCompareSpacing]);
                fr.WriteLn('');
                fr.WriteLn(s);
              end;
            end;
          end
          else
          begin
            case fCompareMode of
              cmVert, cmVertSlant:
              begin
                Data.FillRow(Data.FillCurrentRow, Data2);
                if Data.FillCurrentRow = Data.FillTable.RowCount then
                  Data2.RowSpacing := 0;

                s := '（%d）.[ %s连（第%d行为首行）] .（邻行距 ↓%d ）：';
                s := Format(s, [RowNo, CompareModeString, Data2.FirstRow, Data2.RowSpacing]);
                fr.WriteLn('');
                fr.WriteLn(s);
              end;
            end;
          end;
        end;
      end;
      Inc(PageIndex);
    until Group.FillTable.RowCount = 0;
    fr.RenameLastFile;
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.SaveGroupByCompareTypeSortByGroupValueCount;
var
  fr: TFileWriter;
  s, FileName, TxtFileName: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
  PageIndex: Integer;
  GroupRowNo: Cardinal;
begin
  if fExportLite then TxtFileName := '（4）..txt'
  else
    case fCompareMode of
      cmSlant: TxtFileName := '（4）.【排列】“%d”个以上各个组合（相同代号、不同首行）的（组合数：最多 - 最少个）.txt';
      cmVert, cmVertSlant: TxtFileName := '（4）.【排列】【“%d”个以上[相同组合、不同首行]的组合[组合数：最多→少个]】.txt';
    end;
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  fr := TFileWriter.Create(FileName);
  fr.RebuildFileEvent := RebuildFile;
  fr.RebuildFileNameEvent := RebuildFileName;
  try
    if not fExportLite then
    begin
      s := TPath.GetFileNameWithoutExtension(FileName);
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);
    end;

    TSQLCompareGroup.AutoFree(Group);
    TSQLCompareData.AutoFree(Data);
    PageIndex := 0;
    GroupRowNo := 0;
    repeat
      s := 'ORDER BY ValueCount DESC, Value2 LIMIT ? OFFSET ?';
      if fRecalcMode > 0 then s := 'Enabled = 1 ' + s;
      Group.FillPrepare(fDatabase, s, [EachPageRowCount, PageIndex * EachPageRowCount]);
      while Group.FillOne do
      begin
        GroupRowNo := GroupRowNo + 1;
        case fCompareMode of
          cmSlant:
          begin
            s := '%d.[代号：（第%s个）%s ] ；（组合数：%d个）';
            s := Format(s, [
              GroupRowNo,
              Group.Value,
              BuildSlantGroupValue(Group.Value),
              Group.ValueCount
            ]);
          end;
          cmVert, cmVertSlant:
          begin
            s := '%d.[ 第%d次组合；代号：1.（%s）]；（组合数：%d个）：';
            s := Format(s, [
              GroupRowNo,
              Group.ValueCount,
              Group.Value,
              Group.ValueCount
            ]);
          end;
        end;
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn(s);

        case fCompareMode of
          cmVert, cmSlant: s := 'GroupValue = ? ORDER BY FirstRow DESC';
          cmVertSlant: s := 'GroupValue = ? ORDER BY FirstRow DESC LIMIT 1';
        end;
        Data.FillPrepare(fDatabase, s, [Group.Value]);
        while Data.FillOne do
        begin
          case fCompareMode of
            cmSlant:
            begin
              s := Format('（%d）（第%d行为首行）', [Data.FillCurrentRow - 1, Data.FirstRow]);
              if Length(fIntervalValues) = 1 then
                s := s + Format('= 无【%s列】数： %d列 ；', [fTipStr, Data.TotalValueCount])
              else
                s := s + Format('= 无【%s列】数：%d - %d 列 ；', [fTipStr, Data.ValueCount, Data.TotalValueCount - Data.ValueCount]);
              //s := s + Format('【列数字】：%s', [DataToString(Data.Values)]);
              s := s + Format('【列数字】：%s', [Data.ToString(fDataMode)]);
              fr.WriteLn('');
              fr.WriteLn(s);
            end;
            cmVert, cmVertSlant:
            begin
              if Data.FillCurrentRow > 2 then fr.WriteLn('');
               SaveCompareData(fr, Data);
            end;
          end;
        end;
      end;
      Inc(PageIndex);
    until Group.FillTable.RowCount = 0;
    fr.RenameLastFile;
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
  PageIndex: Integer;
  GroupRowNo: Cardinal;
begin
  if fExportLite then TxtFileName := '（5）..txt'
  else
    case fCompareMode of
      cmVert: TxtFileName := '（5）.【排列】【“%d”个以上[相同组合、不同首行]的组合[无【对应列】数：最多→少列]】.txt';
      cmSlant: TxtFileName := '（5）.【排列】“%d”个以上各个组合（相同代号、不同首行）的（无【对应列】数：最多 - 最少列）.txt';
    end;
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  fr := TFileWriter.Create(FileName);
  fr.RebuildFileEvent := RebuildFile;
  fr.RebuildFileNameEvent := RebuildFileName;
  try
    if not fExportLite then
    begin
      s := TPath.GetFileNameWithoutExtension(FileName);
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);
    end;

    TSQLCompareGroup.AutoFree(Group);
    TSQLCompareData.AutoFree(Data);
    PageIndex := 0;
    GroupRowNo := 0;
    repeat
      s := 'ORDER BY MaxValueCount DESC, MaxTotalValueCount - MaxValueCount DESC, ValueCount, Value2 LIMIT ? OFFSET ?';
      if fRecalcMode > 0 then s := 'Enabled = 1 ' + s;
      Group.FillPrepare(fDatabase, s, [EachPageRowCount, PageIndex * EachPageRowCount]);
      while Group.FillOne do
      begin
        GroupRowNo := GroupRowNo + 1;
        case fCompareMode of
          cmVert:
          begin
            s := '%d.[ 第%d次组合；代号：1.（%s）]；（ w无【%s列】总数：';
            s := Format(s, [GroupRowNo, Group.ValueCount, Group.Value, fTipStr]);
            if Length(fIntervalValues) = 1 then
              s := s + Format('%d列', [Group.MaxTotalValueCount])
            else
              s := s + Format('%d-%d列', [Group.MaxValueCount, Group.MaxTotalValueCount - Group.MaxValueCount]);
            s := s + '）：';
          end;
          cmSlant:
          begin
            s := '%d.[代号：（第%s个）%s ] ；';
            s := Format(s, [
              GroupRowNo,
              Group.Value,
              BuildSlantGroupValue(Group.Value)
            ]);
            if Length(fIntervalValues) = 1 then
              s := s + Format('（ 最多 [ 无【%s列】数：%d列 ] ）', [fTipStr, Group.MaxTotalValueCount])
            else
              s := s + Format('（ 最多 [ 无【%s列】数：%d-%d列 ] ）', [fTipStr, Group.MaxValueCount, Group.MaxTotalValueCount - Group.MaxValueCount]);
          end;
        end;
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn(s);

        Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
        while Data.FillOne do
        begin
          case fCompareMode of
            cmSlant:
            begin
              s := Format('（%d）（第%d行为首行）', [Data.FillCurrentRow - 1, Data.FirstRow]);
              if Length(fIntervalValues) = 1 then
                s := s + Format('= 无【%s列】数： %d列 ；', [fTipStr, Data.TotalValueCount])
              else
                s := s + Format('= 无【%s列】数：%d - %d 列 ；', [fTipStr, Data.ValueCount, Data.TotalValueCount - Data.ValueCount]);
              //s := s + Format('【列数字】：%s', [DataToString(Data.Values)]);
              s := s + Format('【列数字】：%s', [Data.ToString(fDataMode)]);
              fr.WriteLn('');
              fr.WriteLn(s);
            end;
            cmVert:
            begin
              if Data.FillCurrentRow > 2 then fr.WriteLn('');
              SaveCompareData(fr, Data);
            end;
          end;
        end;
      end;
      Inc(PageIndex);
    until Group.FillTable.RowCount = 0;
    fr.RenameLastFile;
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.SaveGroupByCompareTypeSortByLastFirstRow;
var
  fr: TFileWriter;
  s, FileName, TxtFileName, SlantGroupValue: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
  RowNo: Cardinal;
  PageIndex: Integer;
begin
  case fCompareMode of
    cmSlant:
    begin
      if fExportLite then TxtFileName := '（2-4）.（4）.txt'
      else TxtFileName := '（2-4）.【排列】“%d”个以上各个组合（相同代号、不同首行）的（邻行距：最大 → 小）（4）.txt';
    end;
    cmVert:
    begin
      if fExportLite then TxtFileName := '（2-3）.（3）.txt'
      else TxtFileName := '（2-3）.【排列】【“%d”个以上[相同组合、不同首行]的组合 [首行邻行距：最大↓→小↓]】（3）.txt';
    end;
    cmVertSlant:
    begin
      if fExportLite then TxtFileName := '（2-4）.（4）.txt'
      else TxtFileName := '（2-4）.【排列】【“%d”个以上[相同组合、不同首行]的组合 [首行邻行距：最大↓→小↓]】（4）.txt';
    end;
  end;
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory + TxtFileName;
  fr := TFileWriter.Create(FileName);
  fr.RebuildFileNameEvent := RebuildFileName2;
  try
    if not fExportLite then
    begin
      s := TPath.GetFileNameWithoutExtension(FileName);
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);
      if fCompareMode = cmSlant then
      begin
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn('');
      end;
    end;

    TSQLCompareGroup.AutoFree(Group);
    TSQLCompareData.AutoFree(Data);
    RowNo := 0;
    PageIndex := 0;
    repeat
      case fCompareMode of
        cmSlant: s := 'ORDER BY LastFirstRow, ValueCount, Value2 LIMIT ? OFFSET ?';
        else s := 'ORDER BY LastFirstRow, ValueCount DESC, Value2 LIMIT ? OFFSET ?';
      end;
      if fRecalcMode > 0 then s := 'Enabled = 1 ' + s;
      Group.FillPrepare(fDatabase, s, [EachPageRowCount, PageIndex * EachPageRowCount]);
      while Group.FillOne do
      begin
        SlantGroupValue := BuildSlantGroupValue(Group.Value);

        Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
        while Data.FillOne do
        begin
          if Data.FillCurrentRow = 2 then
          begin
            RowNo := RowNo + 1;
            case fCompareMode of
              cmSlant:
              begin
                s := '%d=[（第%s个）%s ] ：（%d）（%d）';
                s := Format(s, [RowNo, Group.Value, SlantGroupValue, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
              end;
              cmVert:
              begin
                s := '%d= 第%d次组合[ 代号：1.（%s）]：（%d）（%d）	';
                s := Format(s, [RowNo, Data.GroupValueCount, Group.Value, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
              end;
              cmVertSlant:
              begin
                 s := '%d=[ 代号：1.（%s）]：（%d）（%d）	';
                 s := Format(s, [RowNo, Group.Value, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
              end;
            end;
            case fCompareMode of
              cmVert, cmVertSlant:
              begin
                fr.WriteLn('');
                fr.WriteLn('');
                fr.WriteLn('');
              end;
            end;
            fr.WriteLn(s);

            if RowNo mod EachFileRowNumber = 0 then fr.BuildActiveFileName;
          end;

          RowNo := RowNo + 1;
          case fCompareMode of
            cmSlant:
            begin
              s := '%d=[（第%s个）%s ] ：（%d）（%d）';
              s := Format(s, [RowNo, Group.Value, SlantGroupValue, Data.FirstRow, Data.RowSpacing]);
            end;
            cmVert:
            begin
               s := '%d= 第%d次组合[ 代号：1.（%s）]：（%d）（%d）	';
               s := Format(s, [RowNo, Data.GroupValueCount, Group.Value, Data.FirstRow, Data.RowSpacing]);
            end;
            cmVertSlant:
            begin
               s := '%d=[ 代号：1.（%s）]：（%d）（%d）	';
               s := Format(s, [RowNo, Group.Value, Data.FirstRow, Data.RowSpacing]);
            end;
          end;
          case fCompareMode of
            cmVert, cmVertSlant: fr.WriteLn('');
          end;
          fr.WriteLn(s);

          case fCompareMode of
            cmVert, cmVertSlant: SaveCompareData(fr, Data, False);
          end;

          if RowNo mod EachFileRowNumber = 0 then fr.BuildActiveFileName;

          if Data.FillCurrentRow > Data.FillTable.RowCount then
          begin
            RowNo := RowNo + 1;
            case fCompareMode of
              cmSlant:
              begin
                s := '%d=[（第%s个）%s ] ：（%d）（0）';
                s := Format(s, [RowNo, Group.Value, SlantGroupValue, fSlantCompareSpacing + 1]);
              end;
              cmVert:
              begin
                s := '%d= 第%d次组合[ 代号：1.（%s）]：（%d）（0）	';
                s := Format(s, [RowNo, Data.GroupValueCount, Group.Value, fVertCompareSpacing + 1]);
              end;
              cmVertSlant:
              begin
                s := '%d=[ 代号：1.（%s）]：（%d）（0）	';
                s := Format(s, [RowNo, Group.Value, fVertSlantCompareSpacing + 1]);
              end;
            end;
            case fCompareMode of
              cmVert, cmVertSlant: fr.WriteLn('');
            end;
            fr.WriteLn(s);

            if RowNo mod EachFileRowNumber = 0 then fr.BuildActiveFileName;
          end;
        end;
      end;
      Inc(PageIndex);
    until Group.FillTable.RowCount = 0;
    fr.WriteFinish;
    fr.RenameLastFile(RebuildFileName2(fr, RowNo));
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.SaveGroupByCompareTypeSortByOneRowSpacingCount;
var
  fr: TFileWriter;
  s, FileName, TxtFileName, SlantGroupValue: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
  RowNo: Cardinal;
  PageIndex: Integer;
begin
  if fExportLite then TxtFileName := '（2-5）.（5）.txt'
  else
    case fCompareMode of
      cmSlant: TxtFileName := '（2-5）.【排列】“%d”个以上各个组合（相同代号、不同首行）的（邻行距：最小 → 大）（5）.txt';
      cmVertSlant: TxtFileName := '（2-5）.【排列】【“%d”个以上[相同组合、不同首行]的组合 [首行邻行距：最小↓→大↓]】（5）.txt';
    end;
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory + TxtFileName;
  fr := TFileWriter.Create(FileName);
  fr.RebuildFileNameEvent := RebuildFileName2;
  try
    if not fExportLite then
    begin
      s := TPath.GetFileNameWithoutExtension(FileName);
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);
      if fCompareMode = cmSlant then
      begin
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn('');
      end;
    end;

    TSQLCompareGroup.AutoFree(Group);
    TSQLCompareData.AutoFree(Data);
    RowNo := 0;
    PageIndex := 0;
    repeat
      s := 'ORDER BY OneRowSpacingCount DESC, LastFirstRow DESC, ValueCount, Value2 LIMIT ? OFFSET ?';
      if fRecalcMode > 0 then s := 'Enabled = 1 ' + s;
      Group.FillPrepare(fDatabase, s, [EachPageRowCount, PageIndex * EachPageRowCount]);
      while Group.FillOne do
      begin
        SlantGroupValue := BuildSlantGroupValue(Group.Value);

        Data.FillPrepare(fDatabase, 'GroupValue = ? ORDER BY FirstRow DESC', [Group.Value]);
        while Data.FillOne do
        begin
          if Data.FillCurrentRow = 2 then
          begin
            RowNo := RowNo + 1;
            case fCompareMode of
              cmSlant:
              begin
                s := '%d=[（第%s个）%s ] ：（%d）（%d）';
                s := Format(s, [RowNo, Group.Value, SlantGroupValue, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
              end;
              cmVertSlant:
              begin
                 s := '%d=[ 代号：1.（%s）]：（%d）（%d）	';
                 s := Format(s, [RowNo, Group.Value, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
              end;
            end;
            case fCompareMode of
              cmVertSlant:
              begin
                fr.WriteLn('');
                fr.WriteLn('');
                fr.WriteLn('');
              end;
            end;
            fr.WriteLn(s);

            if RowNo mod EachFileRowNumber = 0 then fr.BuildActiveFileName;
          end;

          RowNo := RowNo + 1;
          case fCompareMode of
            cmSlant:
            begin
              s := '%d=[（第%s个）%s ] ：（%d）（%d）';
              s := Format(s, [RowNo, Group.Value, SlantGroupValue, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
            end;
            cmVertSlant:
            begin
               s := '%d=[ 代号：1.（%s）]：（%d）（%d）	';
               s := Format(s, [RowNo, Group.Value, Data.FirstRow, Data.RowSpacing]);
            end;
          end;
          case fCompareMode of
            cmVertSlant: fr.WriteLn('');
          end;
          fr.WriteLn(s);

          case fCompareMode of
            cmVert, cmVertSlant: SaveCompareData(fr, Data, False);
          end;

          if RowNo mod EachFileRowNumber = 0 then fr.BuildActiveFileName;

          if Data.FillCurrentRow > Data.FillTable.RowCount then
          begin
            RowNo := RowNo + 1;
            case fCompareMode of
              cmSlant:
              begin
                s := '%d=[（第%s个）%s ] ：（%d）（0）';
                s := Format(s, [RowNo, Group.Value, SlantGroupValue, fSlantCompareSpacing + 1]);
              end;
              cmVertSlant:
              begin
                s := '%d=[ 代号：1.（%s）]：（%d）（0）	';
                s := Format(s, [RowNo, Group.Value, fVertSlantCompareSpacing + 1]);
              end;
            end;
            case fCompareMode of
              cmVertSlant: fr.WriteLn('');
            end;
            fr.WriteLn(s);

            if RowNo mod EachFileRowNumber = 0 then fr.BuildActiveFileName;
          end;
        end;
      end;
      Inc(PageIndex);
    until Group.FillTable.RowCount = 0;
    fr.WriteFinish;
    fr.RenameLastFile(RebuildFileName2(fr, RowNo));
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.SaveCompareDataSortByValueCount(FileName: string);
var
  fr: TFileWriter;
  s: string;
  Group: TSQLCompareGroup;
  Data: TSQLCompareData;
  PageIndex: Integer;
  GroupRowNo: Cardinal;
begin
  fr := TFileWriter.Create(FileName);
  fr.RebuildFileEvent := RebuildFile;
  fr.RebuildFileNameEvent := RebuildFileName;
  try
    if not fExportLite then
    begin
      s := TPath.GetFileNameWithoutExtension(FileName);
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);
    end;

    TSQLCompareGroup.AutoFree(Group);
    TSQLCompareData.AutoFree(Data);
    GroupRowNo := 0;
    PageIndex := 0;
    repeat
      //s := 'ORDER BY DifferentValueCount DESC, TotalDifferentValueCount - DifferentValueCount DESC LIMIT ? OFFSET ?';
      s := 'ORDER BY RowCount DESC, ValueCount, Value2 LIMIT ? OFFSET ?';
      if fRecalcMode > 0 then s := 'Enabled = 1 ' + s;
      Group.FillPrepare(fDatabase, s, [EachPageRowCount, PageIndex * EachPageRowCount]);
      while Group.FillOne do
      begin
        GroupRowNo := GroupRowNo + 1;
        s := 'GroupValue = ? LIMIT 1';
        Data.FillPrepare(fDatabase, s, [Group.Value]);
        Data.FillOne;

        s := '%d.[ 第%d次组合；代号：1.（%s）]；（ ';
        s := Format(s, [GroupRowNo, Data.GroupValueCount, Data.GroupValue]);
        if Data.TotalSameValueCount > 0 then
        begin
          s := s + 'y有【对应列】总数：';
          if Length(fIntervalValues) = 1 then
            s := s + Format('%d列', [Data.TotalSameValueCount])
          else
            s := s + Format('%d-%d列', [Data.SameValueCount, Data.TotalSameValueCount - Data.SameValueCount])
        end;
        if Data.TotalDifferentValueCount > 0 then
        begin
          if Data.TotalSameValueCount > 0 then s := s + '、';

          s := s + Format('w无【%s列】总数：', [fTipStr]);

          if Length(fIntervalValues) = 1 then
            s := s + Format('%d列', [Data.TotalDifferentValueCount])
          else
            s := s + Format('%d-%d列', [Data.DifferentValueCount, Data.TotalDifferentValueCount - Data.DifferentValueCount])
        end;
        s := s + '）：';
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn(s);

        SaveCompareData(fr, Data);
      end;
      Inc(PageIndex);
    until Group.FillTable.RowCount = 0;
    fr.RenameLastFile;
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.SaveVertSlantCompareDataSortByValueCount;
var
  FileName, TxtFileName: string;
begin
  if fExportLite then TxtFileName := '（5）..txt'
  else TxtFileName := '（5）.【排列】【“%d”个以上[相同组合、不同首行]的组合[无【对应列】数：最多→少列]】.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  SaveCompareDataSortByValueCount(FileName);
end;

procedure TDataComputer.SaveVertSlantCompareDataSortByValueCount2;
var
  FileName, TxtFileName: string;
begin
  if fExportLite then TxtFileName := '（6）..txt'
  else TxtFileName := '（6）.【简化】【“%d”个以上[相同组合、不同首行]的组合】.txt';
  TxtFileName := Format(TxtFileName, [fExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  SaveCompareDataSortByValueCount(FileName);
end;

procedure TDataComputer.SaveReEnabledGroup;
var
  fr: TFileWriter;
  s, FileName, TxtFileName: string;
  Group: TSQLCompareGroup;
  PageIndex: Integer;
begin
  if fExportLite then TxtFileName := '（2-01）..txt'
  else TxtFileName := '（2-01）.【（TXT）文本：（2-00）.符...】保留原因的每个代号及其对应内容 [ 邻行距↓ N 小 → 大 ].txt';
  FileName := fExportDirectory + TxtFileName;
  fr := TFileWriter.Create(FileName);
  fr.RebuildFileEvent := RebuildFile;
  fr.RebuildFileNameEvent := RebuildFileName;
  try
    if not fExportLite then
    begin
      s := TPath.GetFileNameWithoutExtension(FileName);
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);
    end;

    TSQLCompareGroup.AutoFree(Group);
    PageIndex := 0;
    repeat
      s := 'ReEnabled = 1 ORDER BY ReEnabledRowSpacing LIMIT ? OFFSET ?';
      Group.FillPrepare(fDatabase, s, [EachPageRowCount, PageIndex * EachPageRowCount]);
      while Group.FillOne do
      begin
        s := '代号：%s';
        s := Format(s, [Group.Value]);
        fr.WriteLn('');
        fr.WriteLn(s);

        WriteGroupReEnableTips(fr, Group);
      end;
      Inc(PageIndex);
    until Group.FillTable.RowCount = 0;
    fr.RenameLastFile;
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.ExportCompareData(ExportFiles: TExportFiles; aExportLite: Boolean);
var
  s, sMode, sCompareMode: string;
begin
  fRowCount := fDatabase.TableRowCount(TSQLRow);
  fExportFiles := ExportFiles;
  fExportLite := aExportLite;
  fTipStr := '';
  if not fExportLite then fTipStr := '对应';

  sMode := '1';
  if fDataMode = 1 then sMode := '3';
  if Length(fIntervalValues) > 1 then
  begin
    sMode := '2';
    if fDataMode = 1 then sMode := '4';
  end;
  sCompareMode := '';
  if not fExportLite then sCompareMode := Format('（%s连）', [CompareModeString]);
  fExportDirectory := TPath.GetDirectoryName(ParamStr(0))
    + Format('\导出结果%s（第%s模式）\', [sCompareMode, sMode]);
  case fCompareMode of
    cmSlant: fExportDirectory2 := fExportDirectory + '（1）.【排列】-（6）.【保存】\';
    else fExportDirectory2 := fExportDirectory + '（1）.【排列】-（6）.【简化】\';
  end;
  if not TDirectory.Exists(fExportDirectory) then
    TDirectory.CreateDirectory(fExportDirectory);
  for s in TDirectory.GetFiles(fExportDirectory, '*.txt') do TFile.Delete(s);
  if not TDirectory.Exists(fExportDirectory2) then
    TDirectory.CreateDirectory(fExportDirectory2);
  for s in TDirectory.GetFiles(fExportDirectory2, '*.txt') do TFile.Delete(s);

  if efFile3 in fExportFiles then
    SaveGroupByCompareTypeSortByMaxRowSpacing;
  if efFile4 in fExportFiles then
    SaveGroupByCompareTypeSortByGroupValueCount;
  if efFile5 in fExportFiles then
    case fCompareMode of
      cmVertSlant: SaveVertSlantCompareDataSortByValueCount;
      else SaveGroupByCompareTypeSortByMaxValueCount;
    end;
  if efFile6 in fExportFiles then
    case fCompareMode of
      cmSlant: SaveSlantCompareType;
      else SaveVertSlantCompareDataSortByValueCount2;
    end;
  if efFile7 in fExportFiles then SaveGroupByFirstRow;
  if efFile8 in fExportFiles then SaveGroupByCompareTypeSortByRowcount2;
  if efFile10 in fExportFiles then SaveReEnabledGroup;
  if (efFile11 in fExportFiles)
    or (efFile12 in fExportFiles)
    or (efFile13 in fExportFiles)
  then SaveGroupByCompareTypeSortByRowcount;
  if efFile14 in fExportFiles then SaveGroupByCompareTypeSortByLastFirstRow;
  if efFile15 in fExportFiles then
    case fCompareMode of
      cmVert: SaveVertGroupByCompareTypeSortByLastFirstRow2;
      else SaveGroupByCompareTypeSortByOneRowSpacingCount;
    end;
end;

end.
