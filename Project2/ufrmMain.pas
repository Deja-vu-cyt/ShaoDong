unit ufrmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Threading,
  System.Classes, Vcl.Graphics, System.Math, System.Types, System.IOUtils,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, DBGridEhGrouping,
  ToolCtrlsEh, DBGridEhToolCtrls, DynVarsEh, EhLibVCL, GridsEh, DBAxisGridsEh,
  DBGridEh;

type
  TfrmMain = class(TForm)
    OpenDialog: TOpenDialog;
    edtFileName: TEdit;
    Label1: TLabel;
    edtCompareSpacing: TEdit;
    Label2: TLabel;
    btnCompare: TButton;
    fdmtData: TFDMemTable;
    Label3: TLabel;
    edtColCount: TEdit;
    Label4: TLabel;
    edtRangeColCount: TEdit;
    Label5: TLabel;
    edtExportTypeCount: TEdit;
    DBGridEh1: TDBGridEh;
    DataSource: TDataSource;
    pnlTop: TPanel;
    lblUseTime: TLabel;
    procedure fdmtDataNewRecord(DataSet: TDataSet);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pnlTopDblClick(Sender: TObject);
    procedure edtFileNameClick(Sender: TObject);
    procedure btnCompareClick(Sender: TObject);
  private
    l: TStringList;
    fColCount: Integer;
    fRangeColCount: Integer;
    fCompareSpacing: Integer;
    fExportTypeCount: Integer;
    fFilePath: string;
    fValues: TInt64DynArray;

    FStopTime: Boolean;
    procedure StartTime;
    procedure StopTime;

    function BuildStringValue: string;
    procedure SaveGroupByFirstRow;
    procedure SaveGroupByCompareType;
    procedure SaveGroupByValueCount;
    procedure Compare;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  uGlobal;

{$R *.dfm}

procedure TfrmMain.StartTime;
begin
  FStopTime := False;

  TTask.Create(procedure
  const
    sTip: string = '处理所需时间：';
  var
    UseTime: string;
    StartTime: Cardinal;
    Day, Hour, Min, Sec, Seconds: Integer;
  begin
    StartTime := GetTickCount;
    while True do
    begin
      Seconds := (GetTickCount - StartTime) div  1000;
      Day := Seconds div 86400;
      Hour := (Seconds mod 86400) div 3600;
      Min := (Seconds mod 3600) div 60;
      Sec := Seconds mod 60;
      UseTime := Format('%d日%d时%d分%d秒', [Day, Hour, Min, Sec]);

      TThread.Synchronize(nil, procedure
      begin
        lblUseTime.Caption := sTip + UseTime;
      end);

      if FStopTime then Break;
      Sleep(1000);
    end;
  end).Start;
end;

procedure TfrmMain.StopTime;
begin
  TThread.Synchronize(nil, procedure
  begin
    FStopTime := True;
  end);
end;

function TfrmMain.BuildStringValue: string;
var
  i: Integer;
  s: string;
begin
  Result := '';
  for i := Low(fValues) to High(fValues) do
    fValues[i] := fdmtData.FieldByName('Field' + (i + 1).ToString).AsLargeInt;
  for i := 1 to fColCount do
  begin
    if fValues[Ceil(i / 64) - 1] = fValues[Ceil(i / 64) - 1] or i64 shl (64 - i mod 64) then
    begin
      if i > fRangeColCount then s := (i - fRangeColCount).ToString
      else s := i.ToString;
      if s.Length < 2 then s := '0' + s;
      if Result.IsEmpty then Result := s
      else if (i > fRangeColCount) and (Result.IndexOf('-') = -1) then Result := Result + '-' + s
      else Result := Result + '、' + s;
    end;
  end;
end;

procedure TfrmMain.SaveGroupByFirstRow;
var
  FirstRowList: TStringList;
  i, FirstRow: Integer;
  s: string;
begin
  l.Clear;
  FirstRowList := TStringList.Create;
  try
    fdmtData.Filtered := False;
    fdmtData.Filter := Format('CompareTypeCount >= %d', [fExportTypeCount]);
    fdmtData.Filtered := True;
    fdmtData.IndexFieldNames := 'FirstRow;CompareTypeCount;CompareType';
    fdmtData.First;
    while not fdmtData.Eof do
    begin
      FirstRow := fdmtData.FieldByName('FirstRow').AsInteger;
      if FirstRowList.IndexOf(FirstRow.ToString) = -1 then
        FirstRowList.Add(FirstRow.ToString);

      fdmtData.Next;
    end;

    for i := 0 to FirstRowList.Count - 1 do
    begin
      FirstRow := FirstRowList[i].ToInteger;
      fdmtData.Filtered := False;
      fdmtData.Filter := Format('CompareTypeCount >= %d AND FirstRow = %d', [fExportTypeCount, FirstRow]);
      fdmtData.Filtered := True;

      s := '第%d行（为首行）（邻行距：↑%d，同行数：%d）';
      if i = 0 then
        s := Format(s, [FirstRow, FirstRow - 1, fdmtData.RecordCount])
      else
        s := Format(s, [FirstRow, FirstRow - FirstRowList[i - 1].ToInteger, fdmtData.RecordCount]);
      l.Add(s);

      fdmtData.First;
      while not fdmtData.Eof do
      begin
        s := '（%d）（代号：1%s ）= 无【对应列】数： %d列 ；【列数字】：%s';
        s := Format(s, [
          fdmtData.RecNo,
          fdmtData.FieldByName('CompareType').AsString,
          fdmtData.FieldByName('ValueCount').AsInteger,
          BuildStringValue
        ]);
        l.Add(s);

        fdmtData.Next;
      end;
    end;
    s := '第%d行（为首行）（邻行距：↑%d）';
    s := Format(s, [FirstRow, FirstRow - FirstRowList[0].ToInteger]);
    l.Add(s);
    s := fFilePath + Format('①.【排列】“%d”个以上（代号：1.NZY ）组合.txt', [fExportTypeCount]);
    l.SaveToFile(s);
  finally
    FirstRowList.Free;
    fdmtData.Filtered := False;
    fdmtData.IndexFieldNames := '';
  end;
end;

procedure TfrmMain.SaveGroupByCompareType;
var
  CompareTypeList: TStringList;
  i, FirstRow: Integer;
  s, CompareType: string;
begin
  l.Clear;
  CompareTypeList := TStringList.Create;
  try
    fdmtData.Filtered := False;
    fdmtData.Filter := Format('CompareTypeCount >= %d', [fExportTypeCount]);
    fdmtData.Filtered := True;
    fdmtData.IndexFieldNames := 'CompareTypeCount;CompareType;FirstRow';
    fdmtData.First;
    while not fdmtData.Eof do
    begin
      CompareType := fdmtData.FieldByName('CompareType').AsString;
      if CompareTypeList.IndexOf(CompareType) = -1 then
        CompareTypeList.Add(CompareType);

      fdmtData.Next;
    end;

    for i := 0 to CompareTypeList.Count - 1 do
    begin
      CompareType := CompareTypeList[i];
      fdmtData.Filtered := False;
      fdmtData.Filter := Format('CompareTypeCount >= %d AND CompareType = ''%s''', [fExportTypeCount, CompareType]);
      fdmtData.Filtered := True;

      l.Add(Format('代号：1%s 同行数：%d', [CompareType, fdmtData.RecordCount]));

      FirstRow := 1;
      fdmtData.First;
      while not fdmtData.Eof do
      begin
        s := '（%d）第%d行（为首行）（邻行距：%d）= 无【对应列】数： %d列 ；【列数字】：%s';
        s := Format(s, [
          fdmtData.RecNo,
          fdmtData.FieldByName('FirstRow').AsInteger,
          fdmtData.FieldByName('FirstRow').AsInteger - FirstRow,
          fdmtData.FieldByName('ValueCount').AsInteger,
          BuildStringValue
        ]);
        l.Add(s);

        FirstRow := fdmtData.FieldByName('FirstRow').AsInteger;

        fdmtData.Next;
      end;
    end;
    s := fFilePath + Format('②.【排列】“%d”个以上（邻行距、同行数：N ）各个组合.txt', [fExportTypeCount]);
    l.SaveToFile(s);

    for i := 0 to CompareTypeList.Count - 1 do
      CompareTypeList[i] := Format('%d=1', [i + 1]) + CompareTypeList[i];
    s := fFilePath + Format('④.【保存】“%d”个以上（代号：1.NZY ）组合.txt', [fExportTypeCount]);
    CompareTypeList.SaveToFile(s);
  finally
    CompareTypeList.Free;
    fdmtData.Filtered := False;
    fdmtData.IndexFieldNames := '';
  end;
end;

procedure TfrmMain.SaveGroupByValueCount;
var
  ValueCountList: TStringList;
  i, FirstRow, ValueCount: Integer;
  s: string;
begin
  l.Clear;
  ValueCountList := TStringList.Create;
  try
    fdmtData.Filtered := False;
    fdmtData.Filter := Format('CompareTypeCount >= %d', [fExportTypeCount]);
    fdmtData.Filtered := True;
    fdmtData.IndexFieldNames := 'ValueCount;FirstRow;CompareTypeCount;CompareType';
    fdmtData.First;
    while not fdmtData.Eof do
    begin
      ValueCount := fdmtData.FieldByName('ValueCount').AsInteger;
      if ValueCountList.IndexOf(ValueCount.ToString) = -1 then
        ValueCountList.Add(ValueCount.ToString);

      fdmtData.Next;
    end;

    for i := 0 to ValueCountList.Count - 1 do
    begin
      ValueCount := ValueCountList[i].ToInteger;
      fdmtData.Filtered := False;
      fdmtData.Filter := Format('CompareTypeCount = %d AND ValueCount >= %d', [fExportTypeCount, ValueCount]);
      fdmtData.Filtered := True;

      l.Add(Format('列数：%d 同行数：%d', [ValueCount, fdmtData.RecordCount]));

      FirstRow := 1;
      fdmtData.First;
      while not fdmtData.Eof do
      begin
        s := '（%d）第%d行（为首行）（邻行距：%d）（代号：1%s）= 无【对应列】数：【列数字】：%s';
        s := Format(s, [
          fdmtData.RecNo,
          fdmtData.FieldByName('FirstRow').AsInteger,
          fdmtData.FieldByName('FirstRow').AsInteger - FirstRow,
          fdmtData.FieldByName('CompareType').AsString,
          BuildStringValue
        ]);
        l.Add(s);

        FirstRow := fdmtData.FieldByName('FirstRow').AsInteger;

        fdmtData.Next;
      end;
    end;
    s := fFilePath + Format('③.【排列】“%d”个以上（【对应列数】：N-N列）.txt', [fExportTypeCount]);
    l.SaveToFile(s);
  finally
    ValueCountList.Free;
    fdmtData.Filtered := False;
    fdmtData.IndexFieldNames := '';
  end;
end;

procedure TfrmMain.Compare;
var
  Arr, Arr2: TIntDyadicArray;
  i, i2, i3, i4,
  CompareRowNo, FirstRow, ValueCount: Integer;
  s, CompareType: string;
  rZ, rY, rRange, rRangeIndex: TIntegerDynArray;
  f: TField;

  function CompareRow(Row, Row2: TIntegerDynArray; Offset: Integer): TIntegerDynArray;
  var
    v, v2: Integer;
    IsExist: Boolean;
  begin
    SetLength(Result, 0);
    for v2 in Row2 do
    begin
      IsExist := False;
      for v in Row do
        if v = v2 + Offset then
        begin
          IsExist := True;
          Break;
        end;
      if IsExist then
      begin
        SetLength(Result, 0);
        Break;
      end;

      if v2 + Offset > 0 then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1] := v2 + Offset;
      end;
    end;
  end;

  procedure AddRow(FirstRow: Integer; CompareType: string; vData: TInt64DynArray);
  var
    i: Integer;
    c: Char;
    CompareTypeCount, ValueCount: Integer;
    v: Int64;
    IsExist: Boolean;
  begin
    CompareTypeCount := 0;
    for c in CompareType do
      if c = '.' then Inc(CompareTypeCount);
    ValueCount := 0;
    for v in vData do
      for i := 1 to 64 do if v = v or i64 shl (64 - i) then Inc(ValueCount);

    fdmtData.Append;
    fdmtData.FieldByName('FirstRow').AsInteger := FirstRow;
    fdmtData.FieldByName('CompareType').AsString := CompareType;
    fdmtData.FieldByName('CompareTypeCount').AsInteger := CompareTypeCount;
    fdmtData.FieldByName('ValueCount').AsInteger := ValueCount;
    for i := Low(fValues) to High(fValues) do
      fdmtData.FieldByName('Field' + (i + 1).ToString).AsLargeInt := fValues[i];
    fdmtData.Post;

    if rRange[0] = -1 then rRange[0] := fdmtData.RecNo - 1;
  end;

  procedure AddRow2(FirstRow: Integer; CompareType: string; vData: TIntegerDynArray);
  var
    i, v: Integer;
  begin
    for i := Low(fValues) to High(fValues) do fValues[i] := 0;
    for v in vData do
    begin
      i := Ceil(v / 64) - 1;
      fValues[i] := fValues[i] or (i64 shl (64 - v mod 64));
    end;
    AddRow(FirstRow, CompareType, fValues);
  end;

begin
  LoadData(edtFileName.Text, Arr, Arr2);
  //合并
  for i := Low(Arr2) to High(Arr2) do
  begin
    SetLength(Arr[i], Length(Arr[i]) + Length(Arr2[i]));
    for i2 := Low(Arr2[i]) to High(Arr2[i]) do
      Arr[i][High(Arr[i]) - High(Arr2[i]) + i2] := Arr2[i][i2] + fRangeColCount;
  end;

  SetLength(fValues, Ceil(fColCount / 64));
  fdmtData.Close;
  fdmtData.FieldDefs.Clear;
  with fdmtData.FieldDefs.AddFieldDef do
  begin
    Name := 'FirstRow';
    DataType := ftSmallInt;
  end;
  with fdmtData.FieldDefs.AddFieldDef do
  begin
    Name := 'CompareType';
    DataType := ftString;
    Size := 100;
  end;
  with fdmtData.FieldDefs.AddFieldDef do
  begin
    Name := 'CompareTypeCount';
    DataType := ftSmallInt;
  end;
  with fdmtData.FieldDefs.AddFieldDef do
  begin
    Name := 'ValueCount';
    DataType := ftSmallInt;
  end;
  for i := Low(fValues) to High(fValues) do
  begin
    with fdmtData.FieldDefs.AddFieldDef do
    begin
      Name := 'Field' + (i + 1).ToString;
      DataType := ftLargeint;
    end;
  end;
  fdmtData.Open;

  fdmtData.DisableControls;
  try
    for i := Low(Arr) to High(Arr) - fCompareSpacing do
    begin
      FirstRow := i + 1;
      SetLength(rRange, 1);
      rRange[0] := -1;
      for i2 := i + 1 to i + fCompareSpacing do
      begin
        CompareRowNo := i2 - i + 1;
        rZ := CompareRow(Arr[i], Arr[i2], i - i2);
        if Length(rZ) > 0 then
          AddRow2(FirstRow, Format('.%dZ', [CompareRowNo]), rZ);
        rY := CompareRow(Arr[i], Arr[i2], i2 - i);
        if Length(rY) > 0 then
          AddRow2(FirstRow, Format('.%dY', [CompareRowNo]), rY);
        //if (Length(rZ) > 0) and (Length(rY) > 0) then
          //AddRow2(FirstRow, Format('.%dZY', [CompareRowNo]), rZ + rY);

        if fdmtData.RecNo > rRange[Length(rRange) - 1] then
        begin
          SetLength(rRange, Length(rRange) + 1);
          rRange[Length(rRange) - 1] := fdmtData.RecNo;
        end;
      end;

      for i2 := Low(rRange) + 2 to High(rRange) do
      begin
        SetLength(rRangeIndex, i2);
        for i3 := Low(rRangeIndex) to High(rRangeIndex) do
          rRangeIndex[i3] := rRange[i3] + 1;
        repeat
          CompareType := '';
          for i3 := Low(fValues) to High(fValues) do fValues[i3] := 0;
          for i3 := Low(rRangeIndex) to High(rRangeIndex) do
          begin
            fdmtData.RecNo := rRangeIndex[i3];
            CompareType := CompareType + fdmtData.FieldByName('CompareType').AsString;
            for i4 := Low(fValues) to High(fValues) do
              fValues[i4] := fValues[i4] or fdmtData.FieldByName('Field' + (i4 + 1).ToString).AsLargeInt;

            if i3 = High(rRangeIndex) then
            begin
              i4 := i3;
              repeat
                rRangeIndex[i4] := rRangeIndex[i4] + 1;
                if rRangeIndex[i4] > rRange[i4 + 1] then
                begin
                  if i4 > 0 then rRangeIndex[i4] := rRange[i4] + 1;
                  i4 := i4 - 1;
                  if i4 < 0 then Break;
                end
                else Break;
              until False;
            end;
          end;
          AddRow(i + 1, CompareType, fValues);
        until rRangeIndex[0] > rRange[1];
      end;
    end;

    if fdmtData.RecordCount > 0 then
    begin
      SaveGroupByFirstRow;
      SaveGroupByCompareType;
      SaveGroupByValueCount;
    end;
  finally
    fdmtData.EnableControls;
  end;
end;

procedure TfrmMain.edtFileNameClick(Sender: TObject);
begin
  if not OpenDialog.Execute then Exit;
  TEdit(Sender).Text := OpenDialog.FileName;
end;

procedure TfrmMain.fdmtDataNewRecord(DataSet: TDataSet);
var
  f: TField;
  i: Integer;
begin
  for i := 0 to DataSet.FieldCount - 1 do
  begin
    f := DataSet.Fields[i];
    if f.FieldName.IndexOf('Field') > -1 then f.AsLargeInt := 0;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  if FormatDateTime('YYYY-MM-DD', Now) = '2017-10-08' then
    raise Exception.Create('软件已过期');

  l := TStringList.Create;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  l.Free;
end;

procedure TfrmMain.pnlTopDblClick(Sender: TObject);
begin
  DataSource.Enabled := not DataSource.Enabled;
end;

procedure TfrmMain.btnCompareClick(Sender: TObject);
begin
  fFilePath := TPath.GetDirectoryName(edtFileName.Text) + '\';
  if not (TryStrToInt(edtCompareSpacing.Text, fCompareSpacing) and (fCompareSpacing > 0)) then
    raise Exception.Create('请输入有效比较次数');
  if not (TryStrToInt(edtColCount.Text, fColCount) and (fColCount > 0)) then
    raise Exception.Create('请输入有效总列数');
  if not (TryStrToInt(edtRangeColCount.Text, fRangeColCount) and (fRangeColCount - 1 in [1..fColCount - 1])) then
    raise Exception.Create('请输入有效范围列数');
  if not (TryStrToInt(edtExportTypeCount.Text, fExportTypeCount) and (fExportTypeCount > 0)) then
    raise Exception.Create('请输入有效导出次数');

  TTask.Create(procedure
  begin
    StartTime;
    try
      Compare;
      StopTime;
      ShowMessage('查询完毕');
    finally
      StopTime;
    end;
  end).Start;
end;

end.
