unit ufrmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Threading,
  System.Classes, Vcl.Graphics, System.Math, System.Types, System.IOUtils, IdHTTP,
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
    procedure SaveGroupByCompareTypeValueCount;
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
  i, FirstRow, RowSpacing, MaxRowSpacing, MinRowSpacing, Rowcount: Integer;
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

    MaxRowSpacing := 0;
    MinRowSpacing := 100;
    for i := 0 to FirstRowList.Count - 1 do
    begin
      FirstRow := FirstRowList[i].ToInteger;
      fdmtData.Filtered := False;
      fdmtData.Filter := Format('CompareTypeCount >= %d AND FirstRow = %d', [fExportTypeCount, FirstRow]);
      fdmtData.Filtered := True;

      if i = 0 then RowSpacing := FirstRow - 1
      else RowSpacing := FirstRow - FirstRowList[i - 1].ToInteger;
      if RowSpacing > MaxRowSpacing then MaxRowSpacing := MaxRowSpacing;
      if RowSpacing < MinRowSpacing then MinRowSpacing := MaxRowSpacing;

      s := '第%d行（为首行）（邻行距：↑%d，同行数：%d）';
      s := Format(s, [FirstRow, RowSpacing, fdmtData.RecordCount]);
      l.Add('');
      l.Add('');
      l.Add('');
      l.Add(s);

      fdmtData.First;
      while not fdmtData.Eof do
      begin
        if fColCount = fRangeColCount then
        begin
          s := '（%d）（代号：1%s ）= 无【对应列】数： %d列 ；【列数字】：%s';
          s := Format(s, [
            fdmtData.RecNo,
            fdmtData.FieldByName('CompareType').AsString,
            fdmtData.FieldByName('ValueCount').AsInteger,
            BuildStringValue
          ]);
        end
        else
        begin
          s := '（%d）（代号：1%s ）= 【 %d-%d 】列 ；【列数字】：%s';
          s := Format(s, [
            fdmtData.RecNo,
            fdmtData.FieldByName('CompareType').AsString,
            fdmtData.FieldByName('ValueCount').AsInteger,
            fdmtData.FieldByName('ValueCount2').AsInteger,
            BuildStringValue
          ]);
        end;
        l.Add('');
        l.Add(s);

        fdmtData.Next;
      end;
    end;
    s := '第%d行（为首行）（邻行距：↑%d）';
    s := Format(s, [FirstRow, FirstRow - FirstRowList[0].ToInteger]);
    l.Add('');
    l.Add('');
    l.Add('');
    l.Add(s);

    s := '';
    Rowcount := 0;
    for i := 0 to l.Count - 2 do
    begin
      if l[i].IndexOf(Format('邻行距：↑%d', [MaxRowSpacing])) > -1 then
      begin
        s := s + l[i] + #$D#$A;
        Inc(Rowcount);
        if Rowcount >= 100 then Break;
      end;
    end;
    if MaxRowSpacing <> MinRowSpacing then
    begin
      Rowcount := 0;
      for i := 0 to l.Count - 2 do
      begin
        if l[i].IndexOf(Format('邻行距：↑%d', [MinRowSpacing])) > -1 then
        begin
          s := s + l[i] + #$D#$A;
          Inc(Rowcount);
          if Rowcount >= 100 then Break;
        end;
      end;
    end;
    l.Insert(0, s);

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
  i: Integer;
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

      l.Add('');
      l.Add('');
      l.Add('');
      l.Add(Format('（代号：1%s ）、（ 最多 [ 不同首行数：%d 行 ] ）', [CompareType, fdmtData.RecordCount]));

      fdmtData.First;
      while not fdmtData.Eof do
      begin
        if fColCount = fRangeColCount then
        begin
          s := '（%d）第%d行（为首行）= 无【对应列】数： %d列 ；【列数字】：%s';
          s := Format(s, [
            fdmtData.RecNo,
            fdmtData.FieldByName('FirstRow').AsInteger,
            fdmtData.FieldByName('ValueCount').AsInteger,
            BuildStringValue
          ]);
        end
        else
        begin
          s := '（%d）第%d行（为首行）= 【 %d-%d 】列 ；【列数字】：%s';
          s := Format(s, [
            fdmtData.RecNo,
            fdmtData.FieldByName('FirstRow').AsInteger,
            fdmtData.FieldByName('ValueCount').AsInteger,
            fdmtData.FieldByName('ValueCount2').AsInteger,
            BuildStringValue
          ]);
        end;
        l.Add('');
        l.Add(s);

        fdmtData.Next;
      end;
    end;
    s := fFilePath + Format('②.【排列】“%d”个以上各个组合（相同代号、不同首行）的（不同首行数：最多 - 最少行）.txt', [fExportTypeCount]);
    l.SaveToFile(s);

    for i := 0 to CompareTypeList.Count - 1 do
      CompareTypeList[i] := Format('%d=1', [i + 1]) + CompareTypeList[i];
    s := fFilePath + Format('⑥.【保存】“%d”个以上各个组合（相同代号、不同首行）的（代号：1.NZY ）.txt', [fExportTypeCount]);
    CompareTypeList.SaveToFile(s);
  finally
    CompareTypeList.Free;
    fdmtData.Filtered := False;
    fdmtData.IndexFieldNames := '';
  end;
end;

procedure TfrmMain.SaveGroupByCompareTypeValueCount;
var
  CompareTypeList: TStringList;
  i, ValueCount, ValueCount2: Integer;
  s, CompareType: string;
begin
  l.Clear;
  CompareTypeList := TStringList.Create;
  try
    fdmtData.Filtered := False;
    fdmtData.Filter := Format('CompareTypeCount >= %d', [fExportTypeCount]);
    fdmtData.Filtered := True;
    fdmtData.IndexFieldNames := 'CompareTypeCount;CompareType;ValueCount;FirstRow';
    fdmtData.First;
    while not fdmtData.Eof do
    begin
      CompareType := fdmtData.FieldByName('CompareType').AsString;
      ValueCount := fdmtData.FieldByName('ValueCount').AsInteger;
      ValueCount2 := fdmtData.FieldByName('ValueCount2').AsInteger;
      s := Format('%s-%d-%d', [CompareType, ValueCount, ValueCount2]);
      if CompareTypeList.IndexOf(s) = -1 then
        CompareTypeList.Add(s);

      fdmtData.Next;
    end;

    for i := 0 to CompareTypeList.Count - 1 do
    begin
      s := CompareTypeList[i];
      CompareType := s.Split(['-'])[0];
      ValueCount := s.Split(['-'])[1].ToInteger;
      ValueCount2 := s.Split(['-'])[2].ToInteger;

      fdmtData.Filtered := False;
      fdmtData.Filter := Format('CompareTypeCount >= %d AND CompareType = ''%s'' AND ValueCount = %d AND ValueCount2 = %d', [fExportTypeCount, CompareType, ValueCount, ValueCount2]);
      fdmtData.Filtered := True;

      if fColCount = fRangeColCount then
      begin
        s := '（代号：1%s ）、（ 最多 [ 无【对应列】数：%d列 ] ）';
        s := Format(s, [CompareType, ValueCount])
      end
      else
      begin
        s := '（代号：1%s ）、（ 最多 [ 无【对应列】数：%d-%d列 ] ）';
        s := Format(s, [CompareType, ValueCount, ValueCount2])
      end;
      l.Add('');
      l.Add('');
      l.Add('');
      l.Add(s);

      fdmtData.First;
      while not fdmtData.Eof do
      begin
        s := '（%d）第%d行（为首行）= 【列数字】：%s';
        s := Format(s, [
          fdmtData.RecNo,
          fdmtData.FieldByName('FirstRow').AsInteger,
          BuildStringValue
        ]);

        l.Add('');
        l.Add(s);

        fdmtData.Next;
      end;
    end;
    s := fFilePath + Format('⑤.【排列】“%d”个以上各个组合（相同代号、不同首行）的（无【对应列】数：最多 - 最少列）.txt', [fExportTypeCount]);
    l.SaveToFile(s);
  finally
    CompareTypeList.Free;
    fdmtData.Filtered := False;
    fdmtData.IndexFieldNames := '';
  end;
end;

procedure TfrmMain.Compare;
var
  Arr, Arr2: TIntDyadicArray;
  i, i2, i3, i4, i5, StartNo, EndNo,
  CompareRowNo, FirstRow, ValueCount: Integer;
  s, CompareType: string;
  rZ, rY, rRange, rRangeIndex, CombineRows: TIntegerDynArray;
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
    i, ColNo: Integer;
    c: Char;
    CompareTypeCount, ValueCount, ValueCount2: Integer;
    v: Int64;
    IsExist: Boolean;
  begin
    CompareTypeCount := 0;
    for c in CompareType do
      if c = '.' then Inc(CompareTypeCount);
    ValueCount := 0;
    ValueCount2 := 0;
    for i := Low(vData) to High(vData) do
    begin
      v := vData[i];
      for ColNo := 1 to 64 do
        if v = v or i64 shl (64 - ColNo) then
        begin
          if i * 64 + ColNo > fRangeColCount then Inc(ValueCount2)
          else Inc(ValueCount);
        end;
    end;

    fdmtData.Append;
    fdmtData.FieldByName('FirstRow').AsInteger := FirstRow;
    fdmtData.FieldByName('CompareType').AsString := CompareType;
    fdmtData.FieldByName('CompareTypeCount').AsInteger := CompareTypeCount;
    fdmtData.FieldByName('ValueCount').AsInteger := ValueCount;
    fdmtData.FieldByName('ValueCount2').AsInteger := ValueCount2;
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
  with fdmtData.FieldDefs.AddFieldDef do
  begin
    Name := 'ValueCount2';
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
      //获取符合数据
      FirstRow := i + 1;
      SetLength(rRange, 1);
      rRange[0] := -1;
      StartNo := fdmtData.RecNo + 1;
      for i2 := i + 1 to i + fCompareSpacing do
      begin
        CompareRowNo := i2 - i + 1;
        rY := CompareRow(Arr[i], Arr[i2], i2 - i);
        if Length(rY) > 0 then
          AddRow2(FirstRow, Format('.%dY', [CompareRowNo]), rY);
        rZ := CompareRow(Arr[i], Arr[i2], i - i2);
        if Length(rZ) > 0 then
          AddRow2(FirstRow, Format('.%dZ', [CompareRowNo]), rZ);
        //if (Length(rZ) > 0) and (Length(rY) > 0) then
          //AddRow2(FirstRow, Format('.%dZY', [CompareRowNo]), rZ + rY);

        if fdmtData.RecNo > rRange[Length(rRange) - 1] then
        begin
          SetLength(rRange, Length(rRange) + 1);
          rRange[Length(rRange) - 1] := fdmtData.RecNo;
        end;
      end;
      EndNo := fdmtData.RecNo;
      if EndNo <= StartNo then Continue;
      //组合数据
      for i2 := 2 to fCompareSpacing do
      begin
        if i2 < fExportTypeCount then Continue;
        SetLength(CombineRows, i2);
        for i3 := Low(CombineRows) to High(CombineRows) do
          CombineRows[i3] := StartNo + i3;
        repeat
          CompareType := '';
          for i3 := Low(fValues) to High(fValues) do fValues[i3] := 0;
          for i3 := Low(CombineRows) to High(CombineRows) do
          begin
            fdmtData.RecNo := CombineRows[i3];
            CompareType := CompareType + fdmtData.FieldByName('CompareType').AsString;
            for i4 := Low(fValues) to High(fValues) do
              fValues[i4] := fValues[i4] or fdmtData.FieldByName('Field' + (i4 + 1).ToString).AsLargeInt;
          end;
          AddRow(FirstRow, CompareType, fValues);

          CombineRows[High(CombineRows)] := CombineRows[High(CombineRows)] + 1;
          if CombineRows[High(CombineRows)] > EndNo then
          begin
            for i3 := High(CombineRows) - 1 downto Low(CombineRows) do
            begin
              CombineRows[i3] := CombineRows[i3] + 1;
              if CombineRows[i3] < EndNo - High(CombineRows) + 1 + i3 then
              begin
                for i4 := i3 + 1 to High(CombineRows) do
                  CombineRows[i4] := CombineRows[i4 - 1] + 1;
                Break;
              end;
            end;
          end;
        until CombineRows[Low(CombineRows)] > EndNo - i2 + 1;
      end;
      {for i2 := Low(rRange) + 2 to High(rRange) do
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
      end;}
    end;

    if fdmtData.RecordCount > 0 then
    begin
      SaveGroupByFirstRow;
      SaveGroupByCompareType;
      SaveGroupByCompareTypeValueCount;
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
var
  s: string;
  ProbationExpired: Boolean;
  FBeiJingTime: TDateTime;
begin
  ProbationExpired := True;
  with TIdHttp.Create do
  begin
    try
      s := 'http://api.k780.com:88/?app=life.time&appkey=10003&sign=b59bc3ef6191eb9f747dd4e83c99f2a4&format=json';
      try
        s := Get(s);
        if s.IndexOf('"success":"1"') > -1 then
        begin
          s := s.Substring(s.IndexOf('datetime_1":"') + 13, 19);
          FBeiJingTime := StrToDateTime(s);
          ProbationExpired := FBeiJingTime > StrToDateTime('2017-10-20');
        end;
      except
        on e: Exception do
        begin
          ShowMessage('校验时间失败，请检查网络连接是否正常' + #$D#$A + e.Message);
        end;
      end;
    finally
      Free;
    end;
  end;
  if ProbationExpired then
  begin
    ShowMessage('软件已过期');
    Application.Terminate;
  end;

  l := TStringList.Create;
  {edtFileName.Text := 'D:\二. 读取：被查询（TXT）文本 .txt';
  edtColCount.Text := '32';
  edtRangeColCount.Text := '32';
  edtCompareSpacing.Text := '3';
  edtExportTypeCount.Text := '1';
  btnCompare.Click;
  Application.Terminate;}
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  l.Free;
end;

procedure TfrmMain.pnlTopDblClick(Sender: TObject);
begin
  if not btnCompare.Enabled then Exit;
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

  TButton(Sender).Enabled := False;
  TTask.Create(procedure
  begin
    StartTime;
    try
      Compare;
      StopTime;
      ShowMessage('查询完毕');
    finally
      StopTime;
      TThread.Synchronize(nil, procedure
      begin
        TButton(Sender).Enabled := True;
      end);
    end;
  end).Start;
end;

end.
