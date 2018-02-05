unit uDataComputer;

interface

uses
  System.Classes,
  System.Variants,
  System.SysUtils,
  System.Types,
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
    fIsFirstRow: Boolean;
    fCompareType: RawUTF8;
    fFirstRow: Word;
    fRowSpacing: Word;
    fRowSpacingNumber: Word;
    fRowSpacing2: Word;
    fRowSpacing3: Word;
    fRowSpacingNumber2: Word;
  published
    property Number: Word read fNumber write fNumber;
    property IsFirstRow: Boolean read fIsFirstRow write fIsFirstRow;
    property CompareType: RawUTF8 read fCompareType write fCompareType;
    property FirstRow: Word read fFirstRow write fFirstRow;
    property RowSpacing: Word read fRowSpacing write fRowSpacing;
    property RowSpacingNumber: Word read fRowSpacingNumber write fRowSpacingNumber;
    property RowSpacing2: Word read fRowSpacing2 write fRowSpacing2;
    property RowSpacing3: Word read fRowSpacing3 write fRowSpacing3;
    property RowSpacingNumber2: Word read fRowSpacingNumber2 write fRowSpacingNumber2;
  end;

  TSQLCompareType = class(TSQLRecord)
  private
    fValue: RawUTF8;
    fTypeCount: Byte;
    fLastRowSpacing: Word;
    fRowSpacingNumber: Word;
  published
    property Value: RawUTF8 read fValue write fValue;
    property TypeCount: Byte read fTypeCount write fTypeCount;
    property LastRowSpacing: Word read fLastRowSpacing write fLastRowSpacing;
    property RowSpacingNumber: Word read fRowSpacingNumber write fRowSpacingNumber;
  end;

  TDataComputer = class
  private
    fModel: TSQLModel;
    fDatabase: TSQLRestServerDB;
    fExportDirectory: string;
  public
    constructor Create;
    destructor Destroy;
    procedure LoadRow(FileName: string; CompareRowCount: Word);
    procedure Compare(CompareTypeCount: Byte);
    procedure ExportCompareData(ExportTypeCount: Byte);
  end;

implementation

constructor TDataComputer.Create;
var
  v: Variant;
begin
  inherited Create;
  fExportDirectory := TPath.GetDirectoryName(ParamStr(0)) + '\导出结果\';
  fModel := TSQLModel.Create([TSQLRow, TSQLCompareType]);
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
var
  i, i2, i3, RowNumber, FirstRow, RowSpacing: Integer;
  s, sCompareType: string;
  Row: TSQLRow;
  CompareType: TSQLCompareType;
begin
  if not TFile.Exists(FileName) then Exit;
  TSQLRow.AutoFree(Row);
  TSQLCompareType.AutoFree(CompareType);
  with TStringList.Create do
  begin
    try
      LoadFromFile(FileName);
      fDatabase.Delete(TSQLRow, '1 = 1');
      fDatabase.Delete(TSQLCompareType, '1 = 1');
      for i := 0 to Count - 1 do
      begin
        if not TryStrToInt(Names[i].Trim, RowNumber) then Continue;
        if (CompareRowCount > 0) and (RowNumber > CompareRowCount) then Break;
        s := ValueFromIndex[i];
        i2 := s.IndexOf('[');
        i3 := s.IndexOf(']');
        if not ((i2 > -1) and (i3 > i2)) then
          raise Exception.CreateFmt('数据格式错误' + #$D#$A + '%s', [Strings[i]]);
        i2 := s.IndexOf('：');
        if not ((i2 > -1) and (i3 > i2)) then
          raise Exception.CreateFmt('数据格式错误' + #$D#$A + '%s', [Strings[i]]);
        sCompareType := s.Substring(i2 + 1, i3 - i2 - 1).Trim;
        s := s.Substring(i3 + 2);
        i2 := s.IndexOf('.');
        if not ((i2 > -1) and TryStrToInt(s.Substring(0, i2).Trim, FirstRow)) then
          raise Exception.CreateFmt('数据格式错误' + #$D#$A + '%s', [Strings[i]]);
        s := s.Substring(i2 + 1);
        i2 := s.IndexOf('（');
        i3 := s.IndexOf('）');
        if not ((i2 > -1) and (i3 > i2) and TryStrToInt(s.Substring(i2 + 1, i3 - i2 - 1).Trim, RowSpacing)) then
          raise Exception.CreateFmt('数据格式错误' + #$D#$A + '%s', [Strings[i]]);

        CompareType.FillPrepare(fDatabase, 'Value = ?', [sCompareType]);
        if not CompareType.FillOne then
        begin
          CompareType.Value := sCompareType;
          CompareType.TypeCount := 1;
          CompareType.LastRowSpacing := RowSpacing;
          CompareType.RowSpacingNumber := 0;
          fDatabase.Add(CompareType, True);
        end;

        Row.Number := RowNumber;
        Row.IsFirstRow := False;
        Row.CompareType := sCompareType;
        Row.FirstRow := FirstRow;
        Row.RowSpacing := RowSpacing;
        Row.RowSpacingNumber := 0;
        Row.RowSpacing2 := 0;
        Row.RowSpacing3 := 0;
        Row.RowSpacingNumber2 := 0;
        fDatabase.Add(Row, True);
      end;
    finally
      Free;
    end;
  end;
end;

procedure TDataComputer.Compare(CompareTypeCount: Byte);
var
  Row, Row2: TSQLRow;
  CompareType, CompareType2: TSQLCompareType;
  i: Integer;
  MaxRowSpacing3: Word;
begin
  TSQLRow.AutoFree(Row);
  TSQLRow.AutoFree(Row2);
  TSQLCompareType.AutoFree(CompareType);
  TSQLCompareType.AutoFree(CompareType2);
  CompareType.FillPrepare(fDatabase, '', []);
  for i := 2 to CompareTypeCount do
  begin
    Foreach(CompareType.FillTable.RowCount, i, procedure(RowNoArray: System.Types.TCardinalDynArray)
    var
      i: Cardinal;
    begin
      //组合代号
      CompareType.Value := '';
      CompareType.TypeCount := 0;
      CompareType.RowSpacingNumber := 0;
      for i in RowNoArray do
      begin
        CompareType.FillRow(i, CompareType2);
        if CompareType.Value <> '' then CompareType.Value := CompareType.Value + '；';
        CompareType.Value := CompareType.Value + CompareType2.Value;
        CompareType.TypeCount := CompareType.TypeCount + 1;
      end;
      fDatabase.Add(CompareType, True);
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
        fDatabase.Update(Row);
      end;
    end);
  end;
  //删除临行距重复的数据
  CompareType.FillPrepare(fDatabase, '', []);
  while CompareType.FillOne do
  begin
    Row.FillPrepare(fDatabase, 'CompareType = ? ORDER BY FirstRow DESC', [CompareType.Value]);
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
        if Row2.FillOne then fDatabase.Delete(TSQLRow, Row.ID);
      end;
    end;
  end;
  //计算临行距的间差
  CompareType.FillPrepare(fDatabase, '', []);
  while CompareType.FillOne do
  begin
    Row.FillPrepare(fDatabase, 'CompareType = ? AND IsFirstRow = 0 ORDER BY RowSpacing DESC', [CompareType.Value]);
    while Row.FillOne do
    begin
      if Row.FillCurrentRow <= Row.FillTable.RowCount then
      begin
        Row.FillRow(Row.FillCurrentRow, Row2);
        Row.RowSpacing2 := Row.RowSpacing - Row2.RowSpacing;
      end;
      Row.RowSpacingNumber := Row.FillTable.RowCount - Row.FillCurrentRow + 2;
      fDatabase.Update(Row);
    end;
  end;
  //按编号排序计算间差
  CompareType.FillPrepare(fDatabase, '', []);
  while CompareType.FillOne do
  begin
    MaxRowSpacing3 := 0;
    Row.FillPrepare(fDatabase, 'CompareType = ? AND IsFirstRow = 0 ORDER BY Number DESC', [CompareType.Value]);
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
          Row.RowSpacingNumber2 := Row2.RowSpacingNumber;
          fDatabase.Update(Row);

          Row.FillRow(Row.FillCurrentRow - 1, Row2);
          if Row.RowSpacing3 > MaxRowSpacing3 then
          begin
            CompareType.RowSpacingNumber := Row.RowSpacingNumber;
            MaxRowSpacing3 := Row.RowSpacing3;
          end;
        end;
      end;
    end;
    fDatabase.Update(CompareType);
  end;
end;

procedure TDataComputer.ExportCompareData(ExportTypeCount: Byte);
var
  tf: TextFile;
  s, FileName, TxtFileName: string;
  Row, Row2: TSQLRow;
  CompareType, CompareType2: TSQLCompareType;
  LastTypeCount: Byte;
  TypeCountNumber: Word;
begin
  if not TDirectory.Exists(fExportDirectory) then
    TDirectory.CreateDirectory(fExportDirectory);

  TxtFileName := '（1）. 导出：不同 [N] 首行、（最大第N、N-N个间差：N）；（排序）首行最大-小（N）：（1）.txt';
  FileName := fExportDirectory + TxtFileName;
  if TFile.Exists(FileName) then TFile.Delete(FileName);
  AssignFile(tf, FileName);
  Rewrite(tf);
  try
    LastTypeCount := 0;
    TSQLRow.AutoFree(Row);
    TSQLCompareType.AutoFree(CompareType);

    CompareType.FillPrepare(fDatabase, 'ORDER BY TypeCount, LastRowSpacing DESC', []);
    while CompareType.FillOne do
    begin
      if LastTypeCount <> CompareType.TypeCount then
      begin
        LastTypeCount := CompareType.TypeCount;
        TypeCountNumber := 0;
      end;
      TypeCountNumber := TypeCountNumber + 1;

      s := '%d.（%d-%d）.代号[（1-1）：%s]：';
      s := Format(s, [
        CompareType.FillCurrentRow - 1,
        CompareType.TypeCount,
        TypeCountNumber,
        CompareType.Value
      ]);
      WriteLn(tf, '');
      WriteLn(tf, s);

      Row.FillPrepare(fDatabase,
        'CompareType = ? AND (RowSpacingNumber >= ? OR IsFirstRow = 1) ORDER BY IsFirstRow DESC, RowSpacing DESC',
        [CompareType.Value, CompareType.RowSpacingNumber]);
      while Row.FillOne do
      begin
        s := '%s=%s.（%s）';
        s := Format(s, [
          DigitToString(Row.Number),
          DigitToString(Row.FirstRow),
          DigitToString(Row.RowSpacing)
        ]);
        if Row.FillCurrentRow > 2 then
        begin
          s := s + Format('（第%d个间差：%s）', [
            Row.FillTable.RowCount - Row.FillCurrentRow + 2,
            DigitToString(Row.RowSpacing)
          ]);
          if Row.RowSpacingNumber > 1 then
          begin
            s := s + Format('（第%d-%d个间差：%s）', [
              Row.RowSpacingNumber,
              Row.RowSpacingNumber - 1,
              DigitToString(Row.RowSpacing2)
            ]);
          end;
        end;
        if Row.RowSpacing3 > 0 then
        begin
          s := s + Format('（第%d-%d个间差：%s）', [
            Row.RowSpacingNumber,
            Row.RowSpacingNumber2,
            DigitToString(Row.RowSpacing3)
          ]);
        end;
        WriteLn(tf, s);
      end;
    end;

    CompareType.FillPrepare(fDatabase, 'ORDER BY TypeCount, LastRowSpacing DESC', []);
    while CompareType.FillOne do
    begin
      if LastTypeCount <> CompareType.TypeCount then
      begin
        LastTypeCount := CompareType.TypeCount;
        TypeCountNumber := 0;
      end;
      TypeCountNumber := TypeCountNumber + 1;

      s := '%d.（%d-%d）.代号[（1-1）：%s]：';
      s := Format(s, [
        CompareType.FillCurrentRow - 1,
        CompareType.TypeCount,
        TypeCountNumber,
        CompareType.Value
      ]);
      WriteLn(tf, '');
      WriteLn(tf, s);

      Row.FillPrepare(fDatabase, 'CompareType = ? ORDER BY IsFirstRow DESC, RowSpacing DESC', [CompareType.Value]);
      while Row.FillOne do
      begin
        s := '%s=%s.（%s）';
        s := Format(s, [
          DigitToString(Row.Number),
          DigitToString(Row.FirstRow),
          DigitToString(Row.RowSpacing)
        ]);
        if Row.FillCurrentRow > 2 then
        begin
          s := s + Format('（第%d个间差：%s）', [
            Row.FillTable.RowCount - Row.FillCurrentRow + 2,
            DigitToString(Row.RowSpacing)
          ]);
          if Row.RowSpacingNumber > 1 then
          begin
            s := s + Format('（第%d-%d个间差：%s）', [
              Row.RowSpacingNumber,
              Row.RowSpacingNumber - 1,
              DigitToString(Row.RowSpacing2)
            ]);
          end;
        end;
        if Row.RowSpacing3 > 0 then
        begin
          s := s + Format('（第%d-%d个间差：%s）', [
            Row.RowSpacingNumber,
            Row.RowSpacingNumber2,
            DigitToString(Row.RowSpacing3)
          ]);
        end;
        WriteLn(tf, s);
      end;
    end;
  finally
    CloseFile(tf);
  end;
end;

end.
