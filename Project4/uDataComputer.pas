unit uDataComputer;

interface

uses
  System.Classes,
  System.Variants,
  System.SysUtils,
  System.Types,
  System.Threading,
  System.IOUtils,
  mORMot,
  mORMotSQLite3,
  SynSQLite3Static,
  SynCommons,
  uCommon;

type
  TSQLRow = class(TSQLRecord)
  private
    fNumber: Word;
    fNumber2: Word;
    fIsFirstRow: Boolean;
    fCompareType: RawUTF8;
    fFirstRow: Word;
    fRowSpacing: Word;
    fRowSpacingNumber: Word;
    fRowSpacing2: Word;
    fRowSpacing3: Word;
    fRowSpacing3DestNumber: Word;
    fBearOneRowSpacingCount: Word;
    fBearOneRowSpacingNumber: Word;
    fBearOneRowSpacing: Word;
    fRowSpacing4: Word;
    fRowSpacing4Number: Word;
    fRowSpacing5: Word;
    fRowSpacing6: Word;
    fRowSpacing6DestNumber: Word;
    fTypeFlag: Byte;
    fTypeCountFlag: Byte;
  published
    property Number: Word read fNumber write fNumber;
    property Number2: Word read fNumber2 write fNumber2;
    property IsFirstRow: Boolean read fIsFirstRow write fIsFirstRow;
    property CompareType: RawUTF8 read fCompareType write fCompareType;
    property FirstRow: Word read fFirstRow write fFirstRow;
    property RowSpacing: Word read fRowSpacing write fRowSpacing;
    property RowSpacingNumber: Word read fRowSpacingNumber write fRowSpacingNumber;
    property RowSpacing2: Word read fRowSpacing2 write fRowSpacing2;
    property RowSpacing3: Word read fRowSpacing3 write fRowSpacing3;
    property RowSpacing3DestNumber: Word read fRowSpacing3DestNumber write fRowSpacing3DestNumber;
    property BearOneRowSpacingCount: Word read fBearOneRowSpacingCount write fBearOneRowSpacingCount;
    property BearOneRowSpacingNumber: Word read fBearOneRowSpacingNumber write fBearOneRowSpacingNumber;
    property BearOneRowSpacing: Word read fBearOneRowSpacing write fBearOneRowSpacing;
    property RowSpacing4: Word read fRowSpacing4 write fRowSpacing4;
    property RowSpacing4Number: Word read fRowSpacing4Number write fRowSpacing4Number;
    property RowSpacing5: Word read fRowSpacing5 write fRowSpacing5;
    property RowSpacing6: Word read fRowSpacing6 write fRowSpacing6;
    property RowSpacing6DestNumber: Word read fRowSpacing6DestNumber write fRowSpacing6DestNumber;
    property TypeFlag: Byte read fTypeFlag write fTypeFlag;
    property TypeCountFlag: Byte read fTypeCountFlag write fTypeCountFlag;
  end;

  TSQLCompareType = class(TSQLRecord)
  private
    fValue: RawUTF8;
    fTypeCount: Byte;
    fRemark: RawUTF8;
    fNumber: Word;  //在相同组合内的流水号
    fNumber2: Word;  //按最新行距大到小排序，在相同组合内的流水号
    fNumber3: Word;  //正序流水号
    fNumber4: Word;  //倒序流水号
    fNumber5: Word;  //按最新行距大到小排序，正序流水号
    fNumber6: Word;  //按最新行距大到小排序，倒序流水号
    fLastRowSpacing: Word;
    fRowSpacingNumber: Word;
    fMaxRowSpacing: Word;
    fMaxRowSpacing3: Word;
    fIsFirstRowOneRowSpacing: Boolean;
    fMaxBearOneRowSpacingCount: Word;
    fMaxRowSpacing4: Word;
    fMaxRowSpacing5: Word;
    fMaxRowSpacing6: Word;
    fTypeCountFlag: Byte;
  published
    property Value: RawUTF8 read fValue write fValue;
    property TypeCount: Byte read fTypeCount write fTypeCount;
    property Remark: RawUTF8 read fRemark write fRemark;
    property Number: Word read fNumber write fNumber;
    property Number2: Word read fNumber2 write fNumber2;
    property Number3: Word read fNumber3 write fNumber3;
    property Number4: Word read fNumber4 write fNumber4;
    property Number5: Word read fNumber5 write fNumber5;
    property Number6: Word read fNumber6 write fNumber6;
    property LastRowSpacing: Word read fLastRowSpacing write fLastRowSpacing;
    property RowSpacingNumber: Word read fRowSpacingNumber write fRowSpacingNumber;
    property MaxRowSpacing: Word read fMaxRowSpacing write fMaxRowSpacing;
    property MaxRowSpacing3: Word read fMaxRowSpacing3 write fMaxRowSpacing3;
    property IsFirstRowOneRowSpacing: Boolean read fIsFirstRowOneRowSpacing write fIsFirstRowOneRowSpacing;
    property MaxBearOneRowSpacingCount: Word read fMaxBearOneRowSpacingCount write fMaxBearOneRowSpacingCount;
    property MaxRowSpacing4: Word read fMaxRowSpacing4 write fMaxRowSpacing4;
    property MaxRowSpacing5: Word read fMaxRowSpacing5 write fMaxRowSpacing5;
    property MaxRowSpacing6: Word read fMaxRowSpacing6 write fMaxRowSpacing6;
    property TypeCountFlag: Byte read fTypeCountFlag write fTypeCountFlag;
  end;

  TSQLCompareTypeCount = class(TSQLRecord)
  private
    fValue: Byte;
    fMaxBearOneRowSpacingCount: Word;
    fMaxBearOneRowSpacingCountID: TID;
    fMaxRowSpacing4: Word;
    fMaxRowSpacing4ID: TID;
    fMaxRowSpacing5: Word;
    fMaxRowSpacing5ID: TID;
    fMaxRowSpacing6: Word;
    fMaxRowSpacing6ID: TID;
  published
    property Value: Byte read fValue write fValue;
    property MaxBearOneRowSpacingCount: Word read fMaxBearOneRowSpacingCount write fMaxBearOneRowSpacingCount;
    property MaxBearOneRowSpacingCountID: TID read fMaxBearOneRowSpacingCountID write fMaxBearOneRowSpacingCountID;
    property MaxRowSpacing4: Word read fMaxRowSpacing4 write fMaxRowSpacing4;
    property MaxRowSpacing4ID: TID read fMaxRowSpacing4ID write fMaxRowSpacing4ID;
    property MaxRowSpacing5: Word read fMaxRowSpacing5 write fMaxRowSpacing5;
    property MaxRowSpacing5ID: TID read fMaxRowSpacing5ID write fMaxRowSpacing5ID;
    property MaxRowSpacing6: Word read fMaxRowSpacing6 write fMaxRowSpacing6;
    property MaxRowSpacing6ID: TID read fMaxRowSpacing6ID write fMaxRowSpacing6ID;
  end;

  TDataComputer = class
  private type
    TBuildDataString = (bdsRowSpacing, bdsBearOneRowSpacing, bdsBearOneRowSpacing2,
      bdsRowSpacing4, bdsRowSpacing5, bdsRowSpacing6);
    TBuildDataStrings = set of TBuildDataString;
  private const
    bdsBearOneRowSpacings: TBuildDataStrings = [bdsBearOneRowSpacing, bdsRowSpacing4, bdsRowSpacing5, bdsRowSpacing6];
  public type
    TExportFile = (efRowSpacingFile, efRowSpacingFile2, efRowSpacingFile3,
      efRowSpacingFile4, efBearOneRowSpacingFile, efBearOneRowSpacingFile2,
      efBearOneRowSpacingFile3, efBearOneRowSpacingFile4, efBearOneRowSpacingFile5,
      efBearOneRowSpacingFile6, efBearOneRowSpacingFile7, efBearOneRowSpacingFile8,
      efBearOneRowSpacingFile9);
    TExportFiles = set of TExportFile;
  private
    fModel: TSQLModel;
    fDatabase: TSQLRestServerDB;
    fExportDirectory: string;
    fExportDirectory2: string;
    fCompareTypeCount: Byte;
    fExportTypeCount: Byte;
    fExportTypeCount2: Byte;
    fMinBearOneRowSpacingCount: Word;
    procedure DeleteBearOneRowSpacingCount(SpacingCount: Word);
    function BuildCompareTypeString(CompareType: TSQLCompareType; NumberFlag: Byte = 2): string;
    function BuildCompareDataString(Row: TSQLRow; BuildDataStrings: TBuildDataStrings; NumberFlag: Byte = 1): string;
    procedure SaveRowSpacingOrderByCompareTypeCount;
    procedure SaveRowSpacingOrderByCompareTypeCount2;
    procedure SaveRowSpacingOrderByCompareTypeCount3;
    procedure SaveRowSpacingOrderByCompareTypeCount4;
    procedure SaveBearOneRowSpacingOrderByCompareTypeCount;
    procedure SaveBearOneRowSpacingOrderByCompareTypeCount2;
  public
    constructor Create;
    destructor Destroy;
    procedure LoadRow(FileName: string; CompareRowCount: Word);
    procedure Compare(TypeCount: Byte);
    procedure ExportCompareData(ExportTypeCount, ExportTypeCount2: Byte; ExportFiles: TExportFiles);
  end;

implementation

constructor TDataComputer.Create;
var
  v: Variant;
begin
  inherited Create;
  fExportDirectory := TPath.GetDirectoryName(ParamStr(0)) + '\导出结果\';
  fExportDirectory2 := TPath.GetDirectoryName(ParamStr(0)) + '\导出结果（简化）：（间差、连和）各行 [ 代号 ]\';
  fModel := TSQLModel.Create([TSQLRow, TSQLCompareType, TSQLCompareTypeCount]);
  fDatabase := TSQLRestServerDB.Create(fModel);
  fDatabase.CreateMissingTables;
end;

destructor TDataComputer.Destroy;
begin
  fDatabase.Free;
  fModel.Free;
  inherited Destroy;
end;

procedure TDataComputer.LoadRow(FileName: string; CompareRowCount: Word);

  function AnalysisData(s: string; out CompareType: string; out FirstRow: Integer;
    out RowSpacing: Integer): Boolean;
  var
    i, i2: Integer;
  begin
    Result := False;
    i := s.IndexOf('[');
    i2 := s.IndexOf(']');
    if not ((i > -1) and (i2 > 2)) then Exit;
    CompareType := s.Substring(i + 1, i2 - i - 1).Trim;
    s := s.Substring(i2 + 2);
    i := CompareType.IndexOf('（');
    i2 := CompareType.IndexOf('）');
    if not ((i > -1) and (i2 > i)) then Exit;
    CompareType := CompareType.Substring(i + 1, i2 - i - 1).Trim;
    i := s.IndexOf('.');
    //if i = -1 then
    if True then
    begin
      i := s.IndexOf('（');
      i2 := s.IndexOf('）');
      if not ((i > -1) and (i2 > i)
        and TryStrToInt(s.Substring(i + 1, i2 - i - 1).Trim, FirstRow))
      then Exit;
      s := s.Substring(i2 + 1);
    end
    else
    begin
      if not TryStrToInt(s.Substring(0, i).Trim, FirstRow) then Exit;
      s := s.Substring(i + 1);
    end;
    i := s.IndexOf('（');
    i2 := s.IndexOf('）');
    if not ((i > -1) and (i2 > i)
      and TryStrToInt(s.Substring(i + 1, i2 - i - 1).Trim, RowSpacing))
    then Exit;

    Result := True;
  end;

var
  i, i2, i3, RowNumber, RowNumber2, FirstRow, RowSpacing: Integer;
  s, sCompareType, sRemark: string;
  Row: TSQLRow;
  CompareType: TSQLCompareType;
  CompareTypeCount: TSQLCompareTypeCount;
begin
  if not TFile.Exists(FileName) then Exit;
  TSQLRow.AutoFree(Row);
  TSQLCompareType.AutoFree(CompareType);
  TSQLCompareTypeCount.AutoFree(CompareTypeCount);
  with TStringList.Create do
  begin
    try
      LoadFromFile(FileName);
      fDatabase.Delete(TSQLRow, '1 = 1');
      fDatabase.Delete(TSQLCompareType, '1 = 1');
      fDatabase.Delete(TSQLCompareTypeCount, '1 = 1');
      for i := 0 to Count - 1 do
      begin
        if not TryStrToInt(Names[i].Trim, RowNumber) then Continue;
        if (CompareRowCount > 0) and (RowNumber > CompareRowCount) then Break;
        s := ValueFromIndex[i];
        if not AnalysisData(s, sCompareType, FirstRow, RowSpacing) then Continue;

        CompareType.FillPrepare(fDatabase, 'Value = ?', [sCompareType]);
        if CompareType.FillOne then
        begin
          if CompareType.Remark = '' then
          begin
            sRemark := '';
            for i2 := i + 1 to Count - 1 do
            begin
              if TryStrToInt(Names[i2].Trim, RowNumber2) then
              begin
                for i3 := i + 1 to i2 - 1 do
                begin
                  if not sRemark.IsEmpty then sRemark := sRemark + #$D#$A;
                  sRemark := sRemark + Trim(Strings[i3]);
                end;
                Break;
              end;
            end;
            CompareType.Remark := sRemark;
            fDatabase.Update(CompareType);
          end;
        end
        else
        begin
          CompareType.Value := sCompareType;
          CompareType.TypeCount := 1;
          CompareType.Remark := '';
          CompareType.LastRowSpacing := RowSpacing;
          CompareType.RowSpacingNumber := 0;
          fDatabase.Add(CompareType, True);
        end;
        CompareTypeCount.FillPrepare(fDatabase, 'Value = 1', []);
        if not CompareTypeCount.FillOne then
        begin
          CompareTypeCount.Value := 1;
          fDatabase.Add(CompareTypeCount, True);
        end;

        Row.Number := RowNumber;
        Row.Number2 := RowNumber;
        Row.IsFirstRow := False;
        Row.CompareType := sCompareType;
        Row.FirstRow := FirstRow;
        Row.RowSpacing := RowSpacing;
        Row.RowSpacingNumber := 0;
        Row.RowSpacing2 := 0;
        Row.RowSpacing3 := 0;
        Row.RowSpacing3DestNumber := 0;
        Row.RowSpacing4 := 0;
        Row.RowSpacing4Number := 0;
        Row.RowSpacing5 := 0;
        Row.RowSpacing6 := 0;
        Row.RowSpacing6DestNumber := 0;
        fDatabase.Add(Row, True);
      end;
    finally
      Free;
    end;
  end;
end;

procedure TDataComputer.Compare(TypeCount: Byte);
var
  Row, Row2: TSQLRow;
  CompareType, CompareType2: TSQLCompareType;
  CompareTypeCount: TSQLCompareTypeCount;
  i: Integer;
  BearOneRowSpacingCount, BearOneRowSpacingNumber, LastNumber: Word;
begin
  TSQLRow.AutoFree(Row);
  TSQLRow.AutoFree(Row2);
  TSQLCompareType.AutoFree(CompareType);
  TSQLCompareType.AutoFree(CompareType2);
  TSQLCompareTypeCount.AutoFree(CompareTypeCount);
  CompareType.FillPrepare(fDatabase, '', []);
  fCompareTypeCount := TypeCount;
  LastNumber := StrToInt(fDatabase.OneFieldValue(TSQLRow, 'Number2', 'ORDER BY Number DESC', []));
  for i := 2 to TypeCount do
  begin
    Foreach(CompareType.FillTable.RowCount, i, procedure(RowNoArray: System.Types.TCardinalDynArray)
    var
      i: Cardinal;
    begin
      //组合代号
      CompareType.Value := '';
      CompareType.TypeCount := 0;
      CompareType.Remark := '';
      CompareType.RowSpacingNumber := 0;
      for i in RowNoArray do
      begin
        CompareType.FillRow(i, CompareType2);
        if CompareType.Value <> '' then CompareType.Value := CompareType.Value + '；';
        CompareType.Value := CompareType.Value + CompareType2.Value;
        CompareType.TypeCount := CompareType.TypeCount + 1;
        if CompareType.Remark <> '' then CompareType.Remark := CompareType.Remark + #$D#$A + #$D#$A;
        CompareType.Remark := CompareType.Remark + CompareType2.Remark;
      end;
      fDatabase.Add(CompareType, True);
      //组合数
      CompareTypeCount.FillPrepare(fDatabase, 'Value = ?', [CompareType.TypeCount]);
      if not CompareTypeCount.FillOne then
      begin
        CompareTypeCount.Value := CompareType.TypeCount;
        fDatabase.Add(CompareTypeCount, True);
      end;
      //组合代号的行
      for i in RowNoArray do
      begin
        CompareType.FillRow(i, CompareType2);
        Row.FillPrepare(fDatabase, 'CompareType = ?', [CompareType2.Value]);
        while Row.FillOne do
        begin
          Row2.FillPrepare(fDatabase, 'CompareType = ? AND FirstRow = ?', [CompareType.Value, Row.FirstRow]);
          if not Row2.FillOne then
          begin
            Row.CompareType := CompareType.Value;
            fDatabase.Add(Row, True);
          end;
        end;
      end;
      //计算临行距
      Row.FillPrepare(fDatabase, 'CompareType = ? ORDER BY FirstRow', [CompareType.Value]);
      while Row.FillOne do
      begin
        if Row.FillCurrentRow > 2 then
        begin
          Row.FillRow(Row.FillCurrentRow - 2, Row2);
          Row.RowSpacing := Row.FirstRow - Row2.FirstRow;

          if Row.FillCurrentRow > Row.FillTable.RowCount then
          begin
            CompareType.LastRowSpacing := Row.RowSpacing;
            fDatabase.Update(CompareType);
          end;
        end;
        Row.Number := Row.FillTable.RowCount - Row.FillCurrentRow + 2;
        Row.Number2 := LastNumber + Row.FillTable.RowCount - Row.FillCurrentRow + 2;
        fDatabase.Update(Row);
      end;
      LastNumber := LastNumber + Row.FillTable.RowCount;
    end);
  end;
  //计算代号的流水号
  LastNumber := 0;
  CompareTypeCount.FillPrepare(fDatabase, 'ORDER BY Value', []);
  while CompareTypeCount.FillOne do
  begin
    CompareType.FillPrepare(fDatabase, 'TypeCount = ?', [CompareTypeCount.Value]);
    while CompareType.FillOne do
    begin
      LastNumber := LastNumber + 1;
      CompareType.Number := CompareType.FillCurrentRow - 1;
      CompareType.Number3 := LastNumber;
      fDatabase.Update(CompareType);
    end;
  end;
  LastNumber := 0;
  CompareTypeCount.FillPrepare(fDatabase, 'ORDER BY Value DESC', []);
  while CompareTypeCount.FillOne do
  begin
    CompareType.FillPrepare(fDatabase, 'TypeCount = ?', [CompareTypeCount.Value]);
    while CompareType.FillOne do
    begin
      LastNumber := LastNumber + 1;
      CompareType.Number4 := LastNumber;
      fDatabase.Update(CompareType);
    end;
  end;
  LastNumber := 0;
  CompareTypeCount.FillPrepare(fDatabase, 'ORDER BY Value', []);
  while CompareTypeCount.FillOne do
  begin
    CompareType.FillPrepare(fDatabase, 'TypeCount = ? ORDER BY LastRowSpacing DESC', [CompareTypeCount.Value]);
    while CompareType.FillOne do
    begin
      LastNumber := LastNumber + 1;
      CompareType.Number2 := CompareType.FillCurrentRow - 1;
      CompareType.Number5 := LastNumber;
      fDatabase.Update(CompareType);
    end;
  end;
 {CompareType.FillPrepare(fDatabase, 'ORDER BY Number5 DESC', []);
  while CompareType.FillOne do
  begin
    CompareType.Number6 := CompareType.FillCurrentRow - 1;
    fDatabase.Update(CompareType);
  end;}
  LastNumber := 0;
  CompareTypeCount.FillPrepare(fDatabase, 'ORDER BY Value DESC', []);
  while CompareTypeCount.FillOne do
  begin
    CompareType.FillPrepare(fDatabase, 'TypeCount = ? ORDER BY LastRowSpacing DESC', [CompareTypeCount.Value]);
    while CompareType.FillOne do
    begin
      LastNumber := LastNumber + 1;
      CompareType.Number6 := LastNumber;
      fDatabase.Update(CompareType);
    end;
  end;

  CompareTypeCount.FillPrepare(fDatabase, 'ORDER BY Value', []);
  while CompareTypeCount.FillOne do
  begin
    CompareTypeCount.MaxBearOneRowSpacingCount := 0;
    CompareTypeCount.MaxBearOneRowSpacingCountID := 0;

    CompareType.FillPrepare(fDatabase, 'TypeCount = ? ORDER BY LastRowSpacing DESC', [CompareTypeCount.Value]);
    while CompareType.FillOne do
    begin
      //重复临行距的数据只取一条
      Row.FillPrepare(fDatabase, 'CompareType = ? and RowSpacing > 0 ORDER BY FirstRow DESC', [CompareType.Value]);
      while Row.FillOne do
      begin
        if Row.FillCurrentRow = 2 then
        begin
          Row.IsFirstRow := True;
          fDatabase.Update(Row);
        end
        else
        begin
          Row2.FillPrepare(fDatabase,
            'CompareType = ? AND RowSpacing = ? AND FirstRow < ?',
            [CompareType.Value, Row.RowSpacing, Row.FirstRow]);
          if not Row2.FillOne then
          begin
            Row.RowSpacingNumber := 1;
            fDatabase.Update(Row);
          end;
        end;
      end;
      //计算间差3
      Row.FillPrepare(fDatabase, 'CompareType = ? AND RowSpacingNumber = 1 ORDER BY RowSpacing DESC', [CompareType.Value]);
      while Row.FillOne do
      begin
        if Row.FillCurrentRow = 2 then
          CompareType.MaxRowSpacing := Row.RowSpacing;
      
        if Row.FillCurrentRow <= Row.FillTable.RowCount then
        begin
          Row.FillRow(Row.FillCurrentRow, Row2);
          Row.RowSpacing2 := Row.RowSpacing - Row2.RowSpacing;
        end;
        Row.RowSpacingNumber := Row.FillTable.RowCount - Row.FillCurrentRow + 2;
        fDatabase.Update(Row);
      end;
      //按行编号排序计算间差3
      Row.FillPrepare(fDatabase, 'CompareType = ? AND RowSpacingNumber > 0 ORDER BY Number DESC', [CompareType.Value]);
      while Row.FillOne do
      begin
        if Row.FillCurrentRow = 2 then
        begin
          Row.FillRow(Row.FillCurrentRow - 1, Row2);
        end
        else
        begin
          if Row.RowSpacing > Row2.RowSpacing then
          begin
            Row.RowSpacing3 := Row.RowSpacing - Row2.RowSpacing;
            Row.RowSpacing3DestNumber := Row2.RowSpacingNumber;
            fDatabase.Update(Row);

            Row.FillRow(Row.FillCurrentRow - 1, Row2);
            if Row.RowSpacing3 >= CompareType.MaxRowSpacing3 then
            begin
              CompareType.RowSpacingNumber := Row.RowSpacingNumber;
              CompareType.MaxRowSpacing3 := Row.RowSpacing3;
            end;
          end;
        end;
      end;
      //计算连和
      BearOneRowSpacingCount := 0;
      Row.FillPrepare(fDatabase, 'CompareType = ? ORDER BY FirstRow', [CompareType.Value]);
      while Row.FillOne do
      begin
        if Row.FillCurrentRow <= Row.FillTable.RowCount then
          Row.FillRow(Row.FillCurrentRow, Row2);

        if Row.RowSpacing = 1 then
        begin
          BearOneRowSpacingCount := BearOneRowSpacingCount + 1;
          if (Row.FillCurrentRow > Row.FillTable.RowCount) or (Row2.RowSpacing > 1) then
          begin
            Row.BearOneRowSpacingCount := BearOneRowSpacingCount;
            BearOneRowSpacingCount := 0;
          end;
        end;

        fDatabase.Update(Row);
      end;
      //计算连和流水号
      BearOneRowSpacingNumber := 0;
      BearOneRowSpacingCount := 0;
      Row.FillPrepare(fDatabase, 'CompareType = ? AND BearOneRowSpacingCount > 0 ORDER BY BearOneRowSpacingCount, Number DESC', [CompareType.Value]);
      while Row.FillOne do
      begin
        if Row.FillCurrentRow > 2 then Row.FillRow(Row.FillCurrentRow - 2, Row2);
        if Row.BearOneRowSpacingCount > BearOneRowSpacingCount then
        begin
          BearOneRowSpacingCount := Row.BearOneRowSpacingCount;
          BearOneRowSpacingNumber := BearOneRowSpacingNumber + 1;

          if Row.BearOneRowSpacingCount > CompareTypeCount.MaxBearOneRowSpacingCount then
          begin
            CompareTypeCount.MaxBearOneRowSpacingCount := Row.BearOneRowSpacingCount;
            CompareTypeCount.MaxBearOneRowSpacingCountID := Row.ID;
          end;
        end;
        Row.BearOneRowSpacingNumber := BearOneRowSpacingNumber;
        if (Row.BearOneRowSpacingNumber > 1) and (Row.BearOneRowSpacingNumber > Row2.BearOneRowSpacingNumber) then
          Row.BearOneRowSpacing := Row.BearOneRowSpacingCount - Row2.BearOneRowSpacingCount;

        fDatabase.Update(Row);
      end;

      fDatabase.Update(CompareType);
    end;
    fDatabase.Update(CompareTypeCount);

    DeleteBearOneRowSpacingCount(1);
  end;
end;

procedure TDataComputer.ExportCompareData(ExportTypeCount, ExportTypeCount2: Byte;
  ExportFiles: TExportFiles);
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
  fExportTypeCount := ExportTypeCount;
  fExportTypeCount2 := ExportTypeCount2;

  if efRowSpacingFile in ExportFiles then
  begin
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveRowSpacingOrderByCompareTypeCount);
    Tasks[i].Start;
  end;
  if efRowSpacingFile2 in ExportFiles then
  begin
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveRowSpacingOrderByCompareTypeCount2);
    Tasks[i].Start;
  end;
  if efRowSpacingFile3 in ExportFiles then
  begin
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveRowSpacingOrderByCompareTypeCount3);
    Tasks[i].Start;
  end;
  if efRowSpacingFile4 in ExportFiles then
  begin
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveRowSpacingOrderByCompareTypeCount4);
    Tasks[i].Start;
  end;
  if efBearOneRowSpacingFile in ExportFiles then
  begin
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveBearOneRowSpacingOrderByCompareTypeCount);
    Tasks[i].Start;
  end;
  if efBearOneRowSpacingFile2 in ExportFiles then
  begin
    i := Length(Tasks);
    SetLength(Tasks, i + 1);
    Tasks[i] := TTask.Create(SaveBearOneRowSpacingOrderByCompareTypeCount2);
    Tasks[i].Start;
  end;
  TTask.WaitForAll(Tasks);

  SetLength(Tasks, 1);
  if efBearOneRowSpacingFile3 in ExportFiles then
  begin
    DeleteBearOneRowSpacingCount(2);
    Tasks[0] := TTask.Create(SaveBearOneRowSpacingOrderByCompareTypeCount2);
    Tasks[0].Start;
    TTask.WaitForAll(Tasks);
  end;
  if efBearOneRowSpacingFile4 in ExportFiles then
  begin
    DeleteBearOneRowSpacingCount(3);
    Tasks[0] := TTask.Create(SaveBearOneRowSpacingOrderByCompareTypeCount2);
    Tasks[0].Start;
    TTask.WaitForAll(Tasks);
  end;
  if efBearOneRowSpacingFile5 in ExportFiles then
  begin
    DeleteBearOneRowSpacingCount(4);
    Tasks[0] := TTask.Create(SaveBearOneRowSpacingOrderByCompareTypeCount2);
    Tasks[0].Start;
    TTask.WaitForAll(Tasks);
  end;
  if efBearOneRowSpacingFile6 in ExportFiles then
  begin
    DeleteBearOneRowSpacingCount(5);
    Tasks[0] := TTask.Create(SaveBearOneRowSpacingOrderByCompareTypeCount2);
    Tasks[0].Start;
    TTask.WaitForAll(Tasks);
  end;
  if efBearOneRowSpacingFile7 in ExportFiles then
  begin
    DeleteBearOneRowSpacingCount(6);
    Tasks[0] := TTask.Create(SaveBearOneRowSpacingOrderByCompareTypeCount2);
    Tasks[0].Start;
    TTask.WaitForAll(Tasks);
  end;
  if efBearOneRowSpacingFile8 in ExportFiles then
  begin
    DeleteBearOneRowSpacingCount(7);
    Tasks[0] := TTask.Create(SaveBearOneRowSpacingOrderByCompareTypeCount2);
    Tasks[0].Start;
    TTask.WaitForAll(Tasks);
  end;
  if efBearOneRowSpacingFile9 in ExportFiles then
  begin
    DeleteBearOneRowSpacingCount(8);
    Tasks[0] := TTask.Create(SaveBearOneRowSpacingOrderByCompareTypeCount2);
    Tasks[0].Start;
    TTask.WaitForAll(Tasks);
  end;
end;

procedure TDataComputer.DeleteBearOneRowSpacingCount(SpacingCount: Word);
var
  Row, Row2: TSQLRow;
  CompareType: TSQLCompareType;
  CompareTypeCount: TSQLCompareTypeCount;
  BearOneRowSpacingCount, BearOneRowSpacingNumber, RowSpacing4Number, RowSpacing: Word;
  IsClearRowSpacing, FirstBearOneRowSpacingRecorded, FirstRowSpacing4Recorded: Boolean;
begin
  TSQLRow.AutoFree(Row);
  TSQLRow.AutoFree(Row2);
  TSQLCompareType.AutoFree(CompareType);
  TSQLCompareTypeCount.AutoFree(CompareTypeCount);
  CompareTypeCount.FillPrepare(fDatabase, '', []);
  while CompareTypeCount.FillOne do
  begin
    //CompareTypeCount.MaxBearOneRowSpacingCount := 0;
    //CompareTypeCount.MaxBearOneRowSpacingCountID := 0;
    CompareTypeCount.MaxRowSpacing4 := 0;
    CompareTypeCount.MaxRowSpacing4ID := 0;
    CompareTypeCount.MaxRowSpacing5 := 0;
    CompareTypeCount.MaxRowSpacing5ID := 0;
    CompareTypeCount.MaxRowSpacing6 := 0;
    CompareTypeCount.MaxRowSpacing6ID := 0;

    CompareType.FillPrepare(fDatabase, 'TypeCount = ? ORDER BY Number', [CompareTypeCount.Value]);
    while CompareType.FillOne do
    begin
      //删除连和
      BearOneRowSpacingCount := 0;
      IsClearRowSpacing := False;
      Row.FillPrepare(fDatabase, 'CompareType = ? ORDER BY Number', [CompareType.Value]);
      while Row.FillOne do
      begin
        if (Row.BearOneRowSpacingCount > 0) and (Row.FillCurrentRow > 2) then
        begin
          if Row.BearOneRowSpacingCount < SpacingCount then
            BearOneRowSpacingCount := Row.BearOneRowSpacingCount;
        end;
        if BearOneRowSpacingCount > 0 then
        begin
          fDatabase.Delete(TSQLRow, Row.ID);
          BearOneRowSpacingCount := BearOneRowSpacingCount - 1;
          IsClearRowSpacing := BearOneRowSpacingCount = 0;
        end;
        if (Row.RowSpacing > 1) and IsClearRowSpacing then
        begin
          Row.RowSpacing4 := 0;
          Row.RowSpacing4Number := 0;
          Row.RowSpacing5 := 0;
          Row.RowSpacing6 := 0;
          Row.RowSpacing6DestNumber := 0;
          fDatabase.Update(Row);
          IsClearRowSpacing := False;
        end;
      end;
      fMinBearOneRowSpacingCount := SpacingCount;
      //计算连和流水号
      {BearOneRowSpacingNumber := 0;
      BearOneRowSpacingCount := 0;
      Row.FillPrepare(fDatabase, 'CompareType = ? AND BearOneRowSpacingCount > 0 ORDER BY BearOneRowSpacingCount, Number DESC', [CompareType.Value]);
      while Row.FillOne do
      begin
        if Row.FillCurrentRow > 2 then Row.FillRow(Row.FillCurrentRow - 2, Row2);
        if Row.BearOneRowSpacingCount > BearOneRowSpacingCount then
        begin
          BearOneRowSpacingCount := Row.BearOneRowSpacingCount;
          BearOneRowSpacingNumber := BearOneRowSpacingNumber + 1;

          if Row.BearOneRowSpacingCount > CompareTypeCount.MaxBearOneRowSpacingCount then
          begin
            CompareTypeCount.MaxBearOneRowSpacingCount := Row.BearOneRowSpacingCount;
            CompareTypeCount.MaxBearOneRowSpacingCountID := Row.ID;
          end;
        end;
        Row.BearOneRowSpacingNumber := BearOneRowSpacingNumber;
        if (Row.BearOneRowSpacingNumber > 1) and (Row.BearOneRowSpacingNumber > Row2.BearOneRowSpacingNumber) then
          Row.BearOneRowSpacing := Row.BearOneRowSpacingCount - Row2.BearOneRowSpacingCount;

        fDatabase.Update(Row);
      end; }
      //计算临行距
      if SpacingCount > 1 then
      begin
        Row.FillPrepare(fDatabase, 'CompareType = ? ORDER BY FirstRow DESC', [CompareType.Value]);
        while Row.FillOne do
        begin
          if Row.FillCurrentRow <= Row.FillTable.RowCount then
          begin
            Row.FillRow(Row.FillCurrentRow, Row2);
            Row.RowSpacing := Row.FirstRow - Row2.FirstRow;
            fDatabase.Update(Row);
          end;
        end;
      end;
      //计算间差4
      RowSpacing := 0;
      Row.FillPrepare(fDatabase, 'CompareType = ? ORDER BY FirstRow', [CompareType.Value]);
      while Row.FillOne do
      begin
        if Row.FillCurrentRow <= Row.FillTable.RowCount then
          Row.FillRow(Row.FillCurrentRow, Row2);

        if Row.RowSpacing > 1 then
        begin
          RowSpacing := RowSpacing + Row.RowSpacing;
          if (Row.FillCurrentRow > Row.FillTable.RowCount) or (Row2.RowSpacing = 1) then
          begin
            Row.RowSpacing4 := RowSpacing;
            Row.RowSpacing4Number := 0;
            Row.RowSpacing5 := 0;
            Row.RowSpacing6 := 0;
            if Row.RowSpacing4 >= CompareTypeCount.MaxRowSpacing4 then
            begin
              CompareTypeCount.MaxRowSpacing4 := Row.RowSpacing4;
              CompareTypeCount.MaxRowSpacing4ID := Row.ID;
            end;
          end;
        end
        else RowSpacing := 0;
        fDatabase.Update(Row);
      end;
      //计算间差4流水号
      RowSpacing := 0;
      RowSpacing4Number := 0;
      Row.FillPrepare(fDatabase, 'CompareType = ? AND RowSpacing4 > 0 ORDER BY RowSpacing4, Number DESC', [CompareType.Value]);
      while Row.FillOne do
      begin
        if Row.RowSpacing4 = RowSpacing  then Continue;
        RowSpacing := Row.RowSpacing4;
        RowSpacing4Number := RowSpacing4Number + 1;
        Row.RowSpacing4Number := RowSpacing4Number;
        if Row.RowSpacing4Number > 1 then
        begin
          Row.RowSpacing5 := Row.RowSpacing4 - Row2.RowSpacing4;
          if Row.RowSpacing5 > CompareTypeCount.MaxRowSpacing5 then
          begin
            CompareTypeCount.MaxRowSpacing5 := Row.RowSpacing5;
            CompareTypeCount.MaxRowSpacing5ID := Row.ID;
          end;
        end;
        Row.FillRow(Row.FillCurrentRow - 1, Row2);

        fDatabase.Update(Row);
      end;
      //计算间差6
      Row.FillPrepare(fDatabase, 'CompareType = ? AND RowSpacing4 > 0 ORDER BY Number DESC', [CompareType.Value]);
      while Row.FillOne do
      begin
        if Row.FillCurrentRow = 2 then
          Row.FillRow(Row.FillCurrentRow - 1, Row2)
        else
        begin
          if Row.RowSpacing4 > Row2.RowSpacing4 then
          begin
            Row.RowSpacing6 := Row.RowSpacing4 - Row2.RowSpacing4;
            Row.RowSpacing6DestNumber := Row2.RowSpacing4Number;
            fDatabase.Update(Row);

            Row.FillRow(Row.FillCurrentRow - 1, Row2);

            if Row.RowSpacing6 > CompareTypeCount.MaxRowSpacing6 then
            begin
              CompareTypeCount.MaxRowSpacing6 := Row.RowSpacing6;
              CompareTypeCount.MaxRowSpacing6ID := Row.ID;
            end;
          end;
        end;
        fDatabase.Update(Row);
      end;
      //计算代号的各种行
      FirstBearOneRowSpacingRecorded := False;
      FirstRowSpacing4Recorded := False;
      CompareType.MaxBearOneRowSpacingCount := 0;
      CompareType.MaxRowSpacing4 := 0;
      CompareType.MaxRowSpacing5 := 0;
      CompareType.MaxRowSpacing6 := 0;
      Row.FillPrepare(fDatabase, 'CompareType = ? ORDER BY Number', [CompareType.Value]);
      while Row.FillOne do
      begin
        Row.TypeFlag := 0;
        if Row.BearOneRowSpacingCount > 0 then
        begin
          if Row.FillCurrentRow = 2 then
          begin
            CompareType.IsFirstRowOneRowSpacing := True;
            Row.TypeFlag := 1;
          end
          else if not FirstBearOneRowSpacingRecorded then
          begin
            Row.TypeFlag := 3;
            FirstBearOneRowSpacingRecorded := True;
          end;
        end;
        if (Row.RowSpacing4 > 0) and not FirstRowSpacing4Recorded then
        begin
          Row.TypeFlag := 2;
          FirstRowSpacing4Recorded := True;
        end;
        if Row.BearOneRowSpacingCount > CompareType.MaxBearOneRowSpacingCount then
          CompareType.MaxBearOneRowSpacingCount := Row.BearOneRowSpacingCount;
        if Row.RowSpacing4 > CompareType.MaxRowSpacing4 then
          CompareType.MaxRowSpacing4 := Row.RowSpacing4;
        if Row.RowSpacing5 > CompareType.MaxRowSpacing5 then
          CompareType.MaxRowSpacing5 := Row.RowSpacing5;
        if Row.RowSpacing6 > CompareType.MaxRowSpacing6 then
          CompareType.MaxRowSpacing6 := Row.RowSpacing6;

        fDatabase.Update(Row);
      end;
      Row.FillPrepare(fDatabase, 'CompareType = ? AND BearOneRowSpacingCount = ? AND BearOneRowSpacingCount > 0',
        [CompareType.Value, CompareType.MaxBearOneRowSpacingCount]);
      while Row.FillOne do
      begin
        if Row.TypeFlag = 0 then
        begin
          Row.TypeFlag := 4;
          fDatabase.Update(Row);
        end;
      end;
      Row.FillPrepare(fDatabase, 'CompareType = ? AND RowSpacing4 = ? AND RowSpacing4 > 0',
        [CompareType.Value, CompareType.MaxRowSpacing4]);
      while Row.FillOne do
      begin
        if Row.TypeFlag = 0 then
        begin
          Row.TypeFlag := 5;
          fDatabase.Update(Row);
        end;
      end;
      Row.FillPrepare(fDatabase, 'CompareType = ? AND RowSpacing5 = ? AND RowSpacing5 > 0',
        [CompareType.Value, CompareType.MaxRowSpacing5]);
      while Row.FillOne do
      begin
        if Row.TypeFlag = 0 then
        begin
          Row.TypeFlag := 6;
          fDatabase.Update(Row);
        end;
      end;
      Row.FillPrepare(fDatabase, 'CompareType = ? AND RowSpacing6 = ? AND RowSpacing6 > 0',
        [CompareType.Value, CompareType.MaxRowSpacing6]);
      while Row.FillOne do
      begin
        if Row.TypeFlag = 0 then
        begin
          Row.TypeFlag := 7;
          fDatabase.Update(Row);
        end;
      end;

      fDatabase.Update(CompareType);
    end;
    fDatabase.Update(CompareTypeCount);
    //计算最大行排序
    CompareType.FillPrepare(fDatabase, 'TypeCount = ? AND TypeCountFlag > 0', [CompareTypeCount.Value]);
    while CompareType.FillOne do
    begin
      Row.FillPrepare(fDatabase, 'CompareType = ? AND TypeCountFlag > 0', [CompareType.Value]);
      while Row.FillOne do
      begin
        Row.TypeCountFlag := 0;
        fDatabase.Update(Row);
      end;
      CompareType.TypeCountFlag := 0;
      fDatabase.Update(CompareType);
    end;
    Row.FillPrepare(fDatabase, [CompareTypeCount.MaxBearOneRowSpacingCountID]);
    if Row.FillOne then
    begin
      Row.TypeCountFlag := 1;
      fDatabase.Update(Row);
      CompareType.FillPrepare(fDatabase, 'Value = ?', [Row.CompareType]);
      CompareType.FillOne;
      CompareType.TypeCountFlag := 1;
      fDatabase.Update(CompareType);
    end;
    Row.FillPrepare(fDatabase, [CompareTypeCount.MaxRowSpacing4ID]);
    if Row.FillOne then
    begin
      Row.TypeCountFlag := 2;
      fDatabase.Update(Row);
      CompareType.FillPrepare(fDatabase, 'Value = ?', [Row.CompareType]);
      CompareType.FillOne;
      if CompareType.TypeCountFlag = 0 then
      begin
        CompareType.TypeCountFlag := 2;
        fDatabase.Update(CompareType);
      end;
    end;
    Row.FillPrepare(fDatabase, [CompareTypeCount.MaxRowSpacing5ID]);
    if Row.FillOne and (Row.TypeCountFlag = 0) then
    begin
      Row.TypeCountFlag := 3;
      fDatabase.Update(Row);
      CompareType.FillPrepare(fDatabase, 'Value = ?', [Row.CompareType]);
      CompareType.FillOne;
      if CompareType.TypeCountFlag = 0 then
      begin
        CompareType.TypeCountFlag := 3;
        fDatabase.Update(CompareType);
      end;
    end;
    Row.FillPrepare(fDatabase, [CompareTypeCount.MaxRowSpacing6ID]);
    if Row.FillOne and (Row.TypeCountFlag = 0) then
    begin
      Row.TypeCountFlag := 4;
      fDatabase.Update(Row);
      CompareType.FillPrepare(fDatabase, 'Value = ?', [Row.CompareType]);
      CompareType.FillOne;
      if CompareType.TypeCountFlag = 0 then
      begin
        CompareType.TypeCountFlag := 4;
        fDatabase.Update(CompareType);
      end;
    end;
  end;
end;

function TDataComputer.BuildCompareTypeString(CompareType: TSQLCompareType; NumberFlag: Byte): string;
var
  Number: Word;
begin
  Number := CompareType.Number;
  if NumberFlag = 2 then Number := CompareType.Number2;
  Result := Format('.（%d-%d）[ 代号：1.（%s）]：', [
    CompareType.TypeCount,
    Number,
    CompareType.Value
  ]);
end;

function TDataComputer.BuildCompareDataString(Row: TSQLRow; BuildDataStrings: TBuildDataStrings; NumberFlag: Byte): string;
var
  Number: Word;
begin
  Number := Row.Number;
  if NumberFlag = 2 then Number := Row.Number2;
  Result := Format('%s=[ 代号：1.（%s）]：（%s）（%s）', [
    DigitToString(Number),
    Row.CompareType,
    DigitToString(Row.FirstRow),
    DigitToString(Row.RowSpacing)
  ]);
  if (bdsRowSpacing in BuildDataStrings) and (Row.FillCurrentRow > 2) then
  begin
    Result := Result + Format('（第%d个间差：%s）', [
      Row.RowSpacingNumber,
      DigitToString(Row.RowSpacing)
    ]);
    if Row.RowSpacingNumber > 1 then
    begin
      Result := Result + Format('（第%d-%d个间差：%s）', [
        Row.RowSpacingNumber,
        Row.RowSpacingNumber - 1,
        DigitToString(Row.RowSpacing2)
      ]);
    end;
    if Row.RowSpacing3 > 0 then
    begin
      Result := Result + Format('【第%d-%d个间差：%s】', [
        Row.RowSpacingNumber,
        Row.RowSpacing3DestNumber,
        DigitToString(Row.RowSpacing3)
      ]);
    end;
  end;
  if (bdsBearOneRowSpacing in BuildDataStrings) and (Row.BearOneRowSpacingCount > 0) then
  begin
    Result := Result + Format('（第%d个连和：+%d）', [
      Row.BearOneRowSpacingNumber,
      Row.BearOneRowSpacingCount
    ]);
  end;
  if (bdsBearOneRowSpacing2 in BuildDataStrings) and (Row.BearOneRowSpacing > 0) and (Row.FillCurrentRow <= Row.FillTable.RowCount) then
  begin
    Result := Result + Format('（第%d-%d个连和：+%d）', [
      Row.BearOneRowSpacingNumber,
      Row.BearOneRowSpacingNumber - 1,
      Row.BearOneRowSpacing
    ]);
  end;
  if (bdsRowSpacing4 in BuildDataStrings) and (Row.RowSpacing4Number > 0) then
  begin
    Result := Result + Format('（第%d个总间差：%s）', [
      Row.RowSpacing4Number,
      DigitToString(Row.RowSpacing4)
    ]);
  end;
  if (bdsRowSpacing5 in BuildDataStrings) and (Row.RowSpacing4Number > 1) then
  begin
    Result := Result + Format('（第%d-%d个总间差：%s）', [
      Row.RowSpacing4Number,
      Row.RowSpacing4Number - 1,
      DigitToString(Row.RowSpacing5)
    ]);
  end;
  if (bdsRowSpacing6 in BuildDataStrings) and (Row.RowSpacing6 > 0) then
  begin
    Result := Result + Format('【第%d-%d个总间差：%s】', [
      Row.RowSpacing4Number,
      Row.RowSpacing6DestNumber,
      DigitToString(Row.RowSpacing6)
    ]);
  end;
end;

procedure TDataComputer.SaveRowSpacingOrderByCompareTypeCount;
var
  tf, tf2: TextFile;
  s, FileName, FileName2, TxtFileName: string;
  Row: TSQLRow;
  CompareType: TSQLCompareType;
  CompareTypeCount: TSQLCompareTypeCount;
  Number: Word;
begin
  TxtFileName := '（1）.（排序）组合次数（最多 → 少）：（1）.txt';
  FileName := fExportDirectory + TxtFileName;
  FileName2 := fExportDirectory2 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  AssignFile(tf2, FileName2);
  Rewrite(tf2);
  try
    Number := 0;
    TSQLRow.AutoFree(Row);
    TSQLCompareType.AutoFree(CompareType);
    TSQLCompareTypeCount.AutoFree(CompareTypeCount);

    s := TxtFileName.Replace('.txt', '');
    WriteLn(tf, '');
    WriteLn(tf, s);
    s := Format('第1-%d次组合（N）间差 ：', [fCompareTypeCount]);
    WriteLn(tf, '');
    WriteLn(tf, s);

    CompareTypeCount.FillPrepare(fDatabase, 'ORDER BY Value DESC', []);
    while CompareTypeCount.FillOne do
    begin
      s := '%d.【 第%d次组合 】：';
      s := Format(s, [CompareTypeCount.FillCurrentRow - 1, CompareTypeCount.Value]);
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);

      CompareType.FillPrepare(fDatabase, 'TypeCount = ? ORDER BY Number2 LIMIT ?', [CompareTypeCount.Value, fExportTypeCount]);
      while CompareType.FillOne do
      begin
        Number := Number + 1;

        s := Format('【%d-%d】', [Number, CompareType.Number6]) + BuildCompareTypeString(CompareType);
        WriteLn(tf, '');
        WriteLn(tf, '');
        WriteLn(tf, s);
        s := s.Substring(0, s.Length - 1);
        WriteLn(tf2, s);

        Row.FillPrepare(fDatabase, 'CompareType = ? AND IsFirstRow = 1', [CompareType.Value]);
        while Row.FillOne do
        begin
          s := BuildCompareDataString(Row, []);
          WriteLn(tf, '');
          WriteLn(tf, s);
        end;
      end;
    end;

    WriteLn(tf, '');
    WriteLn(tf, '');

    CompareTypeCount.FillRewind;
    while CompareTypeCount.FillOne do
    begin
      s := '第%d次组合：';
      s := Format(s, [CompareTypeCount.Value]);
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);

      CompareType.FillPrepare(fDatabase, 'TypeCount = ? ORDER BY Number2', [CompareTypeCount.Value]);
      while CompareType.FillOne do
      begin
        s := CompareType.Number6.ToString + BuildCompareTypeString(CompareType);
        WriteLn(tf, '');
        WriteLn(tf, '');
        WriteLn(tf, s);

        Row.FillPrepare(fDatabase, 'CompareType = ? AND (IsFirstRow = 1 OR RowSpacingNumber > 0) ORDER BY IsFirstRow DESC, RowSpacing DESC', [CompareType.Value]);
        while Row.FillOne do
        begin
          s := BuildCompareDataString(Row, [bdsRowSpacing]);
          if Row.FillCurrentRow = 2 then WriteLn(tf, '');
          WriteLn(tf, s);

          if Row.FillCurrentRow > 2 then
          begin
            s := CompareType.Remark;
            WriteLn(tf, '');
            WriteLn(tf, s);
          end;
        end;
      end;
    end;
  finally
    CloseFile(tf);
    CloseFile(tf2);
  end;
end;

procedure TDataComputer.SaveRowSpacingOrderByCompareTypeCount2;
var
  tf, tf2: TextFile;
  s, FileName, FileName2, TxtFileName: string;
  Row: TSQLRow;
  CompareType: TSQLCompareType;
  CompareTypeCount: TSQLCompareTypeCount;
  Number: Word;
begin
  TxtFileName := '（2）.（排序）①.不同首行 [代号：N.（N、N；..）]：（N）（N）；最右的（N）（最大→小）（2）.txt';
  FileName := fExportDirectory + TxtFileName;
  FileName2 := fExportDirectory2 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  AssignFile(tf2, FileName2);
  Rewrite(tf2);
  try
    Number := 0;
    TSQLRow.AutoFree(Row);
    TSQLCompareType.AutoFree(CompareType);
    TSQLCompareTypeCount.AutoFree(CompareTypeCount);

    s := TxtFileName.Replace('.txt', '');
    WriteLn(tf, '');
    WriteLn(tf, s);
    s := Format('第1-%d次组合（N）间差 ：', [fCompareTypeCount]);
    WriteLn(tf, '');
    WriteLn(tf, s);

    CompareTypeCount.FillPrepare(fDatabase, 'ORDER BY Value', []);
    while CompareTypeCount.FillOne do
    begin
      s := '%d.【 第%d次组合 】：';
      s := Format(s, [CompareTypeCount.FillCurrentRow - 1, CompareTypeCount.Value]);
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);

      CompareType.FillPrepare(fDatabase, 'TypeCount = ? ORDER BY Number2 LIMIT ?', [CompareTypeCount.Value, fExportTypeCount]);
      while CompareType.FillOne do
      begin
        Number := Number + 1;

        s := Format('【%d-%d】', [Number, CompareType.Number5]) + BuildCompareTypeString(CompareType);
        WriteLn(tf, '');
        WriteLn(tf, '');
        WriteLn(tf, s);
        s := s.Substring(0, s.Length - 1);
        WriteLn(tf2, s);

        Row.FillPrepare(fDatabase,
          'CompareType = ? AND (RowSpacingNumber >= ? OR IsFirstRow = 1) ORDER BY IsFirstRow DESC, RowSpacing DESC',
          [CompareType.Value, CompareType.RowSpacingNumber]);
        while Row.FillOne do
        begin
          s := BuildCompareDataString(Row, [bdsRowSpacing]);
          WriteLn(tf, '');
          WriteLn(tf, s);
        end;
      end;
    end;

    WriteLn(tf, '');
    WriteLn(tf, '');

    CompareTypeCount.FillRewind;
    while CompareTypeCount.FillOne do
    begin
      s := '第%d次组合：';
      s := Format(s, [CompareTypeCount.Value]);
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);

      CompareType.FillPrepare(fDatabase, 'TypeCount = ? ORDER BY Number2', [CompareTypeCount.Value]);
      while CompareType.FillOne do
      begin
        s := CompareType.Number5.ToString + BuildCompareTypeString(CompareType);
        WriteLn(tf, '');
        WriteLn(tf, '');
        WriteLn(tf, s);

        Row.FillPrepare(fDatabase, 'CompareType = ? AND (IsFirstRow = 1 OR RowSpacingNumber > 0) ORDER BY IsFirstRow DESC, RowSpacing DESC', [CompareType.Value]);
        while Row.FillOne do
        begin
          s := BuildCompareDataString(Row, [bdsRowSpacing]);
          if Row.FillCurrentRow = 2 then WriteLn(tf, '');
          WriteLn(tf, s);

          if Row.FillCurrentRow > 2 then
          begin
            s := CompareType.Remark;
            WriteLn(tf, '');
            WriteLn(tf, s);
          end;
        end;
      end;
    end;
  finally
    CloseFile(tf);
    CloseFile(tf2);
  end;
end;

procedure TDataComputer.SaveRowSpacingOrderByCompareTypeCount3;
var
  tf, tf2: TextFile;
  s, FileName, FileName2, TxtFileName: string;
  Row: TSQLRow;
  CompareType: TSQLCompareType;
  CompareTypeCount: TSQLCompareTypeCount;
  Number: Word;
begin
  TxtFileName := '（3）.（排序）②.（第N个间差：N）（最大 → 小）：（3）.txt';
  FileName := fExportDirectory + TxtFileName;
  FileName2 := fExportDirectory2 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  AssignFile(tf2, FileName2);
  Rewrite(tf2);
  try
    Number := 0;
    TSQLRow.AutoFree(Row);
    TSQLCompareType.AutoFree(CompareType);
    TSQLCompareTypeCount.AutoFree(CompareTypeCount);

    s := TxtFileName.Replace('.txt', '');
    WriteLn(tf, '');
    WriteLn(tf, s);
    s := Format('第1-%d次组合（N）间差 ：', [fCompareTypeCount]);
    WriteLn(tf, '');
    WriteLn(tf, s);

    CompareTypeCount.FillPrepare(fDatabase, 'ORDER BY Value', []);
    while CompareTypeCount.FillOne do
    begin
      s := '%d.【 第%d次组合 】：';
      s := Format(s, [CompareTypeCount.FillCurrentRow - 1, CompareTypeCount.Value]);
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);

      CompareType.FillPrepare(fDatabase, 'TypeCount = ? AND Number2 <= ? ORDER BY MaxRowSpacing DESC', [CompareTypeCount.Value, fExportTypeCount]);
      while CompareType.FillOne do
      begin
        Number := Number + 1;

        s := Format('【%d-%d】', [Number, CompareType.Number5]) + BuildCompareTypeString(CompareType);
        WriteLn(tf, '');
        WriteLn(tf, '');
        WriteLn(tf, s);
        s := s.Substring(0, s.Length - 1);
        WriteLn(tf2, s);

        Row.FillPrepare(fDatabase,
          'CompareType = ? AND (RowSpacingNumber >= ? OR IsFirstRow = 1) ORDER BY IsFirstRow DESC, RowSpacing DESC',
          [CompareType.Value, CompareType.RowSpacingNumber]);
        while Row.FillOne do
        begin
          s := BuildCompareDataString(Row, [bdsRowSpacing]);
          WriteLn(tf, '');
          WriteLn(tf, s);
        end;
      end;
    end;

    WriteLn(tf, '');
    WriteLn(tf, '');

    CompareTypeCount.FillRewind;
    while CompareTypeCount.FillOne do
    begin
      s := '第%d次组合：';
      s := Format(s, [CompareTypeCount.Value]);
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);

      CompareType.FillPrepare(fDatabase, 'TypeCount = ? ORDER BY Number2', [CompareTypeCount.Value]);
      while CompareType.FillOne do
      begin
        s := CompareType.Number5.ToString + BuildCompareTypeString(CompareType);
        WriteLn(tf, '');
        WriteLn(tf, '');
        WriteLn(tf, s);

        Row.FillPrepare(fDatabase, 'CompareType = ? AND (IsFirstRow = 1 OR RowSpacingNumber > 0) ORDER BY IsFirstRow DESC, RowSpacing DESC', [CompareType.Value]);
        while Row.FillOne do
        begin
          s := BuildCompareDataString(Row, [bdsRowSpacing]);
          if Row.FillCurrentRow = 2 then WriteLn(tf, '');
          WriteLn(tf, s);

          if Row.FillCurrentRow > 2 then
          begin
            s := CompareType.Remark;
            WriteLn(tf, '');
            WriteLn(tf, s);
          end;
        end;
      end;
    end;
  finally
    CloseFile(tf);
    CloseFile(tf2);
  end;
end;

procedure TDataComputer.SaveRowSpacingOrderByCompareTypeCount4;
var
  tf, tf2: TextFile;
  s, FileName, FileName2, TxtFileName: string;
  Row: TSQLRow;
  CompareType: TSQLCompareType;
  CompareTypeCount: TSQLCompareTypeCount;
  Number: Word;
begin
  TxtFileName := '（4）.（排序）③.【第N-N个间差：N】（最大 → 小）：（4）.txt';
  FileName := fExportDirectory + TxtFileName;
  FileName2 := fExportDirectory2 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  AssignFile(tf2, FileName2);
  Rewrite(tf2);
  try
    Number := 0;
    TSQLRow.AutoFree(Row);
    TSQLCompareType.AutoFree(CompareType);
    TSQLCompareTypeCount.AutoFree(CompareTypeCount);

    s := TxtFileName.Replace('.txt', '');
    WriteLn(tf, '');
    WriteLn(tf, s);
    s := Format('第1-%d次组合（N）间差 ：', [fCompareTypeCount]);
    WriteLn(tf, '');
    WriteLn(tf, s);

    CompareTypeCount.FillPrepare(fDatabase, 'ORDER BY Value', []);
    while CompareTypeCount.FillOne do
    begin
      s := '%d.【 第%d次组合 】：';
      s := Format(s, [CompareTypeCount.FillCurrentRow - 1, CompareTypeCount.Value]);
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);

      CompareType.FillPrepare(fDatabase, 'TypeCount = ? AND Number2 <= ? ORDER BY MaxRowSpacing3 DESC', [CompareTypeCount.Value, fExportTypeCount]);
      while CompareType.FillOne do
      begin
        Number := Number + 1;

        s := Format('【%d-%d】', [Number, CompareType.Number5]) + BuildCompareTypeString(CompareType);
        WriteLn(tf, '');
        WriteLn(tf, '');
        WriteLn(tf, s);
        s := s.Substring(0, s.Length - 1);
        WriteLn(tf2, s);

        Row.FillPrepare(fDatabase,
          'CompareType = ? AND (RowSpacingNumber >= ? OR IsFirstRow = 1) ORDER BY IsFirstRow DESC, RowSpacing3 DESC',
          [CompareType.Value, CompareType.RowSpacingNumber]);
        while Row.FillOne do
        begin
          s := BuildCompareDataString(Row, [bdsRowSpacing]);
          WriteLn(tf, '');
          WriteLn(tf, s);
        end;
      end;
    end;

    WriteLn(tf, '');
    WriteLn(tf, '');

    CompareTypeCount.FillRewind;
    while CompareTypeCount.FillOne do
    begin
      s := '第%d次组合：';
      s := Format(s, [CompareTypeCount.FillCurrentRow - 1, CompareTypeCount.Value]);
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);

      CompareType.FillPrepare(fDatabase, 'TypeCount = ? ORDER BY Number2', [CompareTypeCount.Value]);
      while CompareType.FillOne do
      begin
        s := CompareType.Number5.ToString + BuildCompareTypeString(CompareType);
        WriteLn(tf, '');
        WriteLn(tf, '');
        WriteLn(tf, s);

        Row.FillPrepare(fDatabase, 'CompareType = ? AND (IsFirstRow = 1 OR RowSpacingNumber > 0) ORDER BY IsFirstRow DESC, RowSpacing DESC', [CompareType.Value]);
        while Row.FillOne do
        begin
          s := BuildCompareDataString(Row, [bdsRowSpacing]);
          if Row.FillCurrentRow = 2 then WriteLn(tf, '');
          WriteLn(tf, s);

          if Row.FillCurrentRow > 2 then
          begin
            s := CompareType.Remark;
            WriteLn(tf, '');
            WriteLn(tf, s);
          end;
        end;
      end;
    end;
  finally
    CloseFile(tf);
    CloseFile(tf2);
  end;
end;

procedure TDataComputer.SaveBearOneRowSpacingOrderByCompareTypeCount;
var
  tf, tf2: TextFile;
  s, FileName, FileName2, TxtFileName: string;
  Row: TSQLRow;
  CompareType: TSQLCompareType;
  CompareTypeCount: TSQLCompareTypeCount;
  Number: Word;
begin
  TxtFileName := '①. （排序）组合次数（连和：+1以上）（最多 → 少）：【1】.txt';
  FileName := fExportDirectory + TxtFileName;
  FileName2 := fExportDirectory2 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  AssignFile(tf2, FileName2);
  Rewrite(tf2);
  try
    Number := 0;
    TSQLRow.AutoFree(Row);
    TSQLCompareType.AutoFree(CompareType);
    TSQLCompareTypeCount.AutoFree(CompareTypeCount);

    s := TxtFileName.Replace('.txt', '');
    WriteLn(tf, '');
    WriteLn(tf, s);
    s := Format('第1-%d次组合（N），连和：+1 以上 ：', [fCompareTypeCount]);
    WriteLn(tf, '');
    WriteLn(tf, s);

    CompareTypeCount.FillPrepare(fDatabase, 'ORDER BY Value DESC', []);
    while CompareTypeCount.FillOne do
    begin
      s := '第%d次组合（N）连和：';
      s := Format(s, [CompareTypeCount.Value]);
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);

      CompareType.FillPrepare(fDatabase, 'TypeCount = ? AND Number <= ? AND IsFirstRowOneRowSpacing = 1 ORDER BY Number',
        [CompareTypeCount.Value, fExportTypeCount2]);
      while CompareType.FillOne do
      begin
        Number := Number + 1;

        s := Format('【%d-%d】', [Number, CompareType.Number4]) + BuildCompareTypeString(CompareType, 1);
        WriteLn(tf, '');
        WriteLn(tf, s);
        s := s.Substring(0, s.Length - 1);
        WriteLn(tf2, s);

        Row.FillPrepare(fDatabase, 'CompareType = ? ORDER BY Number LIMIT 1', [CompareType.Value]);
        while Row.FillOne do
        begin
          s := BuildCompareDataString(Row, [bdsBearOneRowSpacing], 2);
          WriteLn(tf, '');
          WriteLn(tf, s);
        end;
      end;
    end;

    WriteLn(tf, '');
    WriteLn(tf, '');

    CompareTypeCount.FillRewind;
    while CompareTypeCount.FillOne do
    begin
      s := '第%d次组合（N）连和：';
      s := Format(s, [CompareTypeCount.Value]);
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);

      CompareType.FillPrepare(fDatabase, 'TypeCount = ? ORDER BY Number', [CompareTypeCount.Value]);
      while CompareType.FillOne do
      begin
        s := CompareType.Number4.ToString + BuildCompareTypeString(CompareType, 1);
        WriteLn(tf, '');
        WriteLn(tf, '');
        WriteLn(tf, s);
        WriteLn(tf, '');

        Row.FillPrepare(fDatabase, 'CompareType = ? ORDER BY Number', [CompareType.Value]);
        while Row.FillOne do
        begin
          s := BuildCompareDataString(Row, bdsBearOneRowSpacings, 2);
          if Row.FillCurrentRow = 2 then WriteLn(tf, '');
          WriteLn(tf, s);

          if Row.FillCurrentRow > 2 then
          begin
            s := CompareType.Remark;
            WriteLn(tf, '');
            WriteLn(tf, s);
          end;
        end;
      end;
    end;
  finally
    CloseFile(tf);
    CloseFile(tf2);
  end;
end;

procedure TDataComputer.SaveBearOneRowSpacingOrderByCompareTypeCount2;
var
  tf, tf2: TextFile;
  s, FileName, FileName2, TxtFileName, sTypeCountFlag: string;
  Row: TSQLRow;
  CompareType: TSQLCompareType;
  CompareTypeCount: TSQLCompareTypeCount;
  Number, LastBearOneRowSpacingCount: Word;
  i: Integer;
begin
  TxtFileName := Format('①. （排序）①.②.③...【第N-N个总间差：N】等..（连和：+%d以上）（最大 → 小）：【%d】.txt',
    [fMinBearOneRowSpacingCount, fMinBearOneRowSpacingCount + 1]);
  FileName := fExportDirectory + TxtFileName;
  FileName2 := fExportDirectory2 + TxtFileName;
  AssignFile(tf, FileName);
  Rewrite(tf);
  AssignFile(tf2, FileName2);
  Rewrite(tf2);
  try
    Number := 0;
    TSQLRow.AutoFree(Row);
    TSQLCompareType.AutoFree(CompareType);
    TSQLCompareTypeCount.AutoFree(CompareTypeCount);

    s := TxtFileName.Replace('.txt', '');
    WriteLn(tf, '');
    WriteLn(tf, s);
    s := Format('第1-%d次组合（N），连和：+%d 以上 ：', [fCompareTypeCount, fMinBearOneRowSpacingCount]);
    WriteLn(tf, '');
    WriteLn(tf, s);

    CompareTypeCount.FillPrepare(fDatabase, 'ORDER BY Value', []);
    while CompareTypeCount.FillOne do
    begin
      s := '第%d次组合（N）连和：';
      s := Format(s, [CompareTypeCount.Value]);
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);

      s := '1.显示（组合同次数）的（一）.（最大连和）、（二）.（最大第N个总间差）、（三）.（最大第N-N个总间差）、（四）.【最大第N-N个总间差】：';
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);

      CompareType.FillPrepare(fDatabase, 'TypeCount = ? AND TypeCountFlag > 0 ORDER BY TypeCountFlag', [CompareTypeCount.Value]);
      while CompareType.FillOne do
      begin
        sTypeCountFlag := '';
        Row.FillPrepare(fDatabase, 'CompareType = ? AND TypeCountFlag > 0 ORDER BY TypeCountFlag', [CompareType.Value]);
        while Row.FillOne do
        begin
          if Row.ID = CompareTypeCount.MaxBearOneRowSpacingCountID then sTypeCountFlag := sTypeCountFlag + '一';
          if Row.ID = CompareTypeCount.MaxRowSpacing4ID then
          begin
            if not sTypeCountFlag.IsEmpty then sTypeCountFlag := sTypeCountFlag + '、';
            sTypeCountFlag := sTypeCountFlag + '二';
          end;
          if Row.ID = CompareTypeCount.MaxRowSpacing5ID then
          begin
            if not sTypeCountFlag.IsEmpty then sTypeCountFlag := sTypeCountFlag + '、';
            sTypeCountFlag := sTypeCountFlag + '三';
          end;
          if Row.ID = CompareTypeCount.MaxRowSpacing6ID then
          begin
            if not sTypeCountFlag.IsEmpty then sTypeCountFlag := sTypeCountFlag + '、';
            sTypeCountFlag := sTypeCountFlag + '四';
          end;
        end;
        s := Format('（%s）', [sTypeCountFlag]) + BuildCompareTypeString(CompareType, 1);
        WriteLn(tf, '');
        WriteLn(tf, s);

        Row.FillRewind;
        while Row.FillOne do
        begin
          if Row.ID = CompareTypeCount.MaxBearOneRowSpacingCountID then
          begin
            s := BuildCompareDataString(Row, [bdsBearOneRowSpacing], 2) + '[（一）.（最大连和：+N）]';
            WriteLn(tf, '');
            WriteLn(tf, s);
          end;
          if Row.ID = CompareTypeCount.MaxRowSpacing4ID then
          begin
            s := BuildCompareDataString(Row, [bdsRowSpacing4], 2) + '[（二）.（最大第N个总间差：N）]';
            WriteLn(tf, '');
            WriteLn(tf, s);
          end;
          if Row.ID = CompareTypeCount.MaxRowSpacing5ID then
          begin
            s := BuildCompareDataString(Row, [bdsRowSpacing5], 2) + '[（三）.（最大第N-N个总间差：N）]';
            WriteLn(tf, '');
            WriteLn(tf, s);
          end;
          if Row.ID = CompareTypeCount.MaxRowSpacing6ID then
          begin
            s := BuildCompareDataString(Row, [bdsRowSpacing6], 2) + '[（四）.【最大第N-N个总间差：N】]';
            WriteLn(tf, '');
            WriteLn(tf, s);
          end;
        end;
      end;

      CompareType.FillPrepare(fDatabase, 'TypeCount = ? AND Number <= ? AND IsFirstRowOneRowSpacing = 1 ORDER BY Number',
        [CompareTypeCount.Value, fExportTypeCount2]);
      while CompareType.FillOne do
      begin
        Number := Number + 1;

        s := '1-1.显示[ 相同代号（符合设置条件的首行、其它行）]：';
        WriteLn(tf, '');
        WriteLn(tf, '');
        WriteLn(tf, '');
        WriteLn(tf, s);
        s := Format('【%d-%d】', [Number, CompareType.Number3]) + BuildCompareTypeString(CompareType, 1);
        WriteLn(tf, '');
        WriteLn(tf, s);
        s := s.Substring(0, s.Length - 1);
        WriteLn(tf2, s);

        Row.FillPrepare(fDatabase, 'CompareType = ? AND TypeFlag > 0 ORDER BY Number', [CompareType.Value]);
        while Row.FillOne do
        begin
          s := BuildCompareDataString(Row, bdsBearOneRowSpacings, 2);
          WriteLn(tf, '');
          WriteLn(tf, s);
        end;

        s := '1-2.显示[ 相同代号（第N个连和：+N）（大→小）排序 ]：';
        WriteLn(tf, '');
        WriteLn(tf, '');
        WriteLn(tf, '');
        WriteLn(tf, s);

        LastBearOneRowSpacingCount := 0;
        Row.FillPrepare(fDatabase, 'CompareType = ? AND BearOneRowSpacingNumber > 0 AND BearOneRowSpacingCount >= ? ORDER BY BearOneRowSpacingNumber DESC, Number DESC',
          [CompareType.Value, fMinBearOneRowSpacingCount]);
        while Row.FillOne do
        begin
          if Row.BearOneRowSpacingCount = LastBearOneRowSpacingCount then Continue;
          LastBearOneRowSpacingCount := Row.BearOneRowSpacingCount;

          s := BuildCompareDataString(Row, [bdsBearOneRowSpacing, bdsBearOneRowSpacing2], 2);
          WriteLn(tf, '');
          WriteLn(tf, s);
        end;
      end;
    end;

    WriteLn(tf, '');
    WriteLn(tf, '');

    CompareTypeCount.FillRewind;
    while CompareTypeCount.FillOne do
    begin
      s := '第%d次组合（N）连和：';
      s := Format(s, [CompareTypeCount.Value]);
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, '');
      WriteLn(tf, s);

      CompareType.FillPrepare(fDatabase, 'TypeCount = ? ORDER BY Number', [CompareTypeCount.Value]);
      while CompareType.FillOne do
      begin
        s := CompareType.Number3.ToString + BuildCompareTypeString(CompareType, 1);
        WriteLn(tf, '');
        WriteLn(tf, '');
        WriteLn(tf, s);
        WriteLn(tf, '');

        Row.FillPrepare(fDatabase, 'CompareType = ? ORDER BY Number', [CompareType.Value]);
        while Row.FillOne do
        begin
          s := BuildCompareDataString(Row, bdsBearOneRowSpacings, 2);
          if Row.FillCurrentRow = 2 then WriteLn(tf, '');
          WriteLn(tf, s);

          if Row.FillCurrentRow > 2 then
          begin
            s := CompareType.Remark;
            WriteLn(tf, '');
            WriteLn(tf, s);
          end;
        end;
      end;
    end;
  finally
    CloseFile(tf);
    CloseFile(tf2);
  end;
end;

end.
