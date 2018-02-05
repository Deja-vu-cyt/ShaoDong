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
    function ReadValues: System.Types.TWordDynArray;
  public
    procedure AddValue(v: Word);
    procedure ClearValue;
    function ValueExist(v: Word): Boolean;
    function HasValue: Boolean;
    property Values: System.Types.TWordDynArray read ReadValues;
  published
    property Field1: Int64 read fField1 write fField1;
    property Field2: Int64 read fField2 write fField2;
    property Field3: Int64 read fField3 write fField3;
    property Field4: Int64 read fField4 write fField4;
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
    fTotalValueCount: Word;
    fValueCount: Word;
    fRowSpacing: Word;
  public
    function CompareValueCount(v: TSQLCompareData): Byte;
  published
    property FirstRow: Word read fFirstRow write fFirstRow;
    property CompareType: Int64 read fCompareType write fCompareType;
    property CompareTypeValueCount: Byte read fCompareTypeValueCount write fCompareTypeValueCount;
    property TotalValueCount: Word read fTotalValueCount write fTotalValueCount;
    property ValueCount: Word read fValueCount write fValueCount;
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

  TDataComputer = class
  private type
    TCompareProcess = (cpCompare, cpGrouping, cpUpdateFirstRow, cpUpdateCompareType, cpFinish);
    TRecCompareState = record
      FirstRow: Word;
      Process: TCompareProcess;
    end;
  public type
    TExportFile = (efFile, efFile2, efFile3, efFile4, efFile5, efFile6);
    TExportFiles = set of TExportFile;
    TInitEvent = procedure(var MaxValue: Word; var FirstRangeValue: Word;
      var CompareSpacing: Byte; var ExportTypeCount: Byte) of object;
  private
    fInitEvent: TInitEvent;
    fModel: TSQLModel;
    fDatabase: TSQLRestServerDB;
    fKeyValue: TSQLKeyValue;
    fDatabaseFileName: string;
    fExportDirectory: string;
    fExportDirectory2: string;
    fMaxValue: Word;
    fFirstRangeValue: Word;
    fCompareSpacing: Byte;
    fCompareState: TRecCompareState;
    fRowCount: Integer;
    fExportTypeCount: Byte;
    function GetKeyValue(Key: string): Variant;
    procedure SetKeyValue(Key: string; Value: Variant);
    function CompareTypeToString(Value: Int64; Flag: Byte = 0): string;
    function DataToString(Value: TSQLData): string;
    procedure CalcDataValueCount(CompareData: TSQLCompareData);
    procedure CalcRowSpacing(CompareData: TSQLCompareData);
    procedure RecalcFirstRowRowCount;
    procedure RecalcCompareTypeRowCount;
    procedure SaveGroupByFirstRow;
    procedure SaveGroupByCompareTypeSortByRowcount;
    procedure SaveGroupByCompareTypeSortByMaxRowSpacing;
    procedure SaveGroupByCompareTypeSortByCompareTypeCount;
    procedure SaveGroupByCompareTypeSortByMaxValueCount;
    procedure SaveGroupByCompareTypeSortByLastFirstRow;
    procedure SaveGroupByCompareTypeSortByOneRowSpacingCount;
    procedure SaveCompareType;
  public
    constructor Create;
    destructor Destroy;
    procedure LoadRow(FileName: string);
    procedure Compare;
    procedure ExportCompareRow;
    procedure ExportCompareData(ExportFiles: TExportFiles);
  published
    property InitEvent: TInitEvent read fInitEvent write fInitEvent;
    property MaxValue: Word read fMaxValue;
    property FirstRangeValue: Word read fFirstRangeValue;
    property CompareSpacing: Byte read fCompareSpacing;
  end;

implementation

function TSQLData.ReadValues: System.Types.TWordDynArray;
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

function TSQLCompareData.CompareValueCount(v: TSQLCompareData): Byte;
begin
  if fTotalValueCount > v.TotalValueCount then Result := 1
  else
  if fTotalValueCount < v.TotalValueCount then Result := 2
  else
  begin
    if Self.ValueCount > v.ValueCount then Result := 1
    else
    if Self.ValueCount < v.ValueCount then Result := 2
    else Result := 0;
  end;
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

function TDataComputer.DataToString(Value: TSQLData): string;
var
  v: Word;
  s: string;
begin
  Result := '';
  for v in Value.Values do
  begin
    if (fFirstRangeValue > 0) and (v > fFirstRangeValue) then s := (v - fFirstRangeValue).ToString
    else s := v.ToString;
    if s.Length < 2 then s := '0' + s;
    if (fFirstRangeValue > 0) and (v > fFirstRangeValue) and (Result.IndexOf('-') = -1) then Result := Result + ' - ' + s
    else if Result.IsEmpty then Result := s
    else Result := Result + '、' + s;
  end;
end;

procedure TDataComputer.CalcDataValueCount(CompareData: TSQLCompareData);
var
  v: Word;
begin
  CompareData.ValueCount := 0;
  CompareData.TotalValueCount := 0;
  for v in CompareData.Values do
  begin
    if v <= fFirstRangeValue then CompareData.ValueCount := CompareData.ValueCount + 1;
    CompareData.TotalValueCount := CompareData.TotalValueCount + 1;
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
    CompareData.RowSpacing := CompareData.FirstRow - 1;
end;

constructor TDataComputer.Create;
var
  v: Variant;
begin
  inherited Create;
  fDatabaseFileName := TPath.GetDirectoryName(ParamStr(0)) + '\Data';
  fExportDirectory := TPath.GetDirectoryName(ParamStr(0)) + '\导出结果\';
  fExportDirectory2 := fExportDirectory + '（1）.【排列】-（6）.【保存】\';
  fModel := TSQLModel.Create([TSQLKeyValue, TSQLRow, TSQLCompareData, TSQLFirstRow, TSQLCompareType]);
  fDatabase := TSQLRestServerDB.Create(fModel, fDatabaseFileName);
  fDatabase.CreateMissingTables;
  fDatabase.CreateSQLIndex(TSQLCompareData, 'FirstRow', False);
  fDatabase.CreateSQLIndex(TSQLCompareData, 'CompareType', False);
  fDatabase.CreateSQLIndex(TSQLCompareData, 'CompareTypeValueCount', False);
  fDatabase.CreateSQLIndex(TSQLCompareType, 'ValueCount', False);
  fDatabase.CreateSQLIndex(TSQLCompareType, 'MaxRowSpacing', False);
  fDatabase.CreateSQLIndex(TSQLCompareType, ['MaxTotalValueCount', 'MaxValueCount'], False);
  fDatabase.CreateSQLIndex(TSQLCompareType, 'LastFirstRow', False);
  fDatabase.CreateSQLIndex(TSQLCompareType, 'OneRowSpacingCount', False);
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
      fInitEvent(fMaxValue, fFirstRangeValue, fCompareSpacing, fExportTypeCount);
      if fMaxValue < 1 then
        raise Exception.Create('总列数无效');
      if (fFirstRangeValue < 0) or (fFirstRangeValue > fMaxValue) then
        raise Exception.Create('第一区域列数无效');
      if (fCompareSpacing < 1) or (fCompareSpacing > 32) then
        raise Exception.Create('比较次数无效');
      if fExportTypeCount < 1 then
        raise Exception.Create('导出次数无效');
      if fFirstRangeValue = fMaxValue then fFirstRangeValue := 0;

      SetKeyValue('MaxValue', fMaxValue);
      SetKeyValue('FirstRangeValue', fFirstRangeValue);
      SetKeyValue('CompareSpacing', fCompareSpacing);
      SetKeyValue('ExportTypeCount', fExportTypeCount);
    end
    else raise Exception.Create('初始化失败');

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
  RowNo, RowSpacing: Word;
  v: Variant;
begin
  fCompareState.Process := cpCompare;
  fCompareState.FirstRow := fCompareSpacing + 1;
  v := GetKeyValue('CompareStateProcess');
  if v <> Unassigned then fCompareState.Process := TCompareProcess(v);
  v := GetKeyValue('CompareStateFirstRow');
  if v <> Unassigned then fCompareState.FirstRow := v;

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
                CalcDataValueCount(CompareData);
                CalcRowSpacing(CompareData);
                fDatabase.Add(CompareData, True);
              end;

              CompareData.CompareType := 0;
              CompareData.CompareTypeValueCount := 1;
              CompareData.CompareType.AddValue(RowNo * 2);
              CompareRow(-RowNo);
              if CompareData.HasValue then
              begin
                CalcDataValueCount(CompareData);
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
                CalcDataValueCount(CompareData);
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
                FirstRow.RowSpacing := FirstRow.Value - 1;

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
                  RowSpacing := CompareData.FirstRow - CompareData2.FirstRow;
                  if RowSpacing > CompareType.MaxRowSpacing then
                    CompareType.MaxRowSpacing := RowSpacing;
                  if RowSpacing = 1 then
                    CompareType.OneRowSpacingCount := CompareType.OneRowSpacingCount + 1;

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
                CompareType.LastFirstRow := Row.Number;
                CompareType.OneRowSpacingCount := 0;
                if CompareData.FirstRow = 2 then
                  CompareType.OneRowSpacingCount := 1;

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
      s := Format('%d=%s', [Row.Number, DataToString(Row)]);
      WriteLn(tf, s);
    end;
  finally
    CloseFile(tf);
  end;
end;

procedure TDataComputer.RecalcFirstRowRowCount;
var
  FirstRow: TSQLFirstRow;
begin
  fDatabase.TransactionBegin(TSQLFirstRow);
  try
    TSQLFirstRow.AutoFree(FirstRow, fDatabase, '', []);
    while FirstRow.FillOne do
    begin
      FirstRow.RowCount := StrToInt(fDatabase.OneFieldValue(TSQLCompareData,
        'Count(*)', 'FirstRow = ? AND CompareTypeValueCount >= ?', [FirstRow.Value, fExportTypeCount]));
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

procedure TDataComputer.RecalcCompareTypeRowCount;
var
  CompareType: TSQLCompareType;
begin
  fDatabase.TransactionBegin(TSQLCompareType);
  try
    TSQLCompareType.AutoFree(CompareType, fDatabase, 'ValueCount >= ?', [fExportTypeCount]);
    while CompareType.FillOne do
    begin
      CompareType.RowCount := StrToInt(fDatabase.OneFieldValue(TSQLCompareData,
        'Count(*)', 'CompareType = ? AND CompareTypeValueCount >= ?', [CompareType.Value, fExportTypeCount]));
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
      WriteLn(tf, '');
      WriteLn(tf, '');
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
        s := s + Format('【列数字】：%s', [DataToString(Data)]);

        WriteLn(tf, '');
        WriteLn(tf, s);
      end;
    end;
    s := '%d.（第%d行为首行）';
    s := Format(s, [
      FirstRow.FillTable.RowCount + 2,
      FirstRow.Value
    ]);
    WriteLn(tf, '');
    WriteLn(tf, '');
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
        s := s + Format('【列数字】：%s', [DataToString(Data)]);
        WriteLn(tf, '');
        WriteLn(tf, s);

        s := '%d=[（第%s个）%s ] ：（%d）（%d）';
        s := Format(s, [Data.FillCurrentRow, sCompareType, sCompareType2, Data.FirstRow, Data.RowSpacing]);
        WriteLn(tf2, s);
        RowNo := RowNo + 1;
        s := '%d=[（第%s个）%s ] ：（%d）（%d）';
        s := Format(s, [RowNo, sCompareType, sCompareType2, Data.FirstRow, Data.RowSpacing]);
        WriteLn(tf3, s);
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
        s := s + Format('【列数字】：%s', [DataToString(Data)]);
        WriteLn(tf, '');
        WriteLn(tf, s);

        if Data.FillCurrentRow > Data.FillTable.RowCount then
        begin
          s := '（%d）（第%d行为首行）';
          s := Format(s, [Data.FillCurrentRow + 1, Data.FirstRow]);
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
        s := s + Format('【列数字】：%s', [DataToString(Data)]);
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
        s := s + Format('【列数字】：%s', [DataToString(Data)]);
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
  TxtFileName := '（2）.【排列】“%d”个以上各个组合（相同代号、不同首行）的（最大 - 最小）（4）.txt';
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
  TxtFileName := '（2）.【排列】“%d”个以上各个组合（相同代号、不同首行）的（最小 - 最大）（5）.txt';
  TxtFileName := Format(TxtFileName, [fExportTypeCount]);
  FileName := fExportDirectory + TxtFileName;
  if TFile.Exists(FileName) then TFile.Delete(FileName);
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    RowNo := 0;
    TSQLCompareType.AutoFree(CompareType, fDatabase, 'ORDER BY OneRowSpacingCount DESC, ValueCount, Size DESC', []);
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
  TxtFileName := '（6）.【保存】“%d”个以上各个组合（相同代号、不同首行）的（代号：1.“N”ZY ）.txt';
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

procedure TDataComputer.ExportCompareData(ExportFiles: TExportFiles);
var
  i: Integer;
  Tasks: array of ITask;
begin
  if not TDirectory.Exists(fExportDirectory) then
    TDirectory.CreateDirectory(fExportDirectory);
  if not TDirectory.Exists(fExportDirectory2) then
    TDirectory.CreateDirectory(fExportDirectory2);

  fRowCount := fDatabase.TableRowCount(TSQLRow);

  if efFile in ExportFiles then
  begin
    //RecalcFirstRowRowCount;
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveGroupByFirstRow);
    Tasks[i].Start;
  end;
  if efFile2 in ExportFiles then
  begin
    //RecalcCompareTypeRowCount;

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

end.
