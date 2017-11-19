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
  DBGridEh, Vcl.CheckLst;

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
    dbgrdData: TDBGridEh;
    dsData: TDataSource;
    pnlTop: TPanel;
    lblUseTime: TLabel;
    chkExportFile: TCheckBox;
    chkExportFile2: TCheckBox;
    chkExportFile3: TCheckBox;
    chkExportFile4: TCheckBox;
    chkExportFile5: TCheckBox;
    chkExportFile6: TCheckBox;
    Label6: TLabel;
    chkSelectAll: TCheckBox;
    fdmtCompareType: TFDMemTable;
    fdmtFirstRow: TFDMemTable;
    dbgrdFirstRow: TDBGridEh;
    dbgrdCompareType: TDBGridEh;
    dsFirstRow: TDataSource;
    dsCompareType: TDataSource;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure fdmtDataNewRecord(DataSet: TDataSet);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pnlTopDblClick(Sender: TObject);
    procedure edtFileNameClick(Sender: TObject);
    procedure btnCompareClick(Sender: TObject);
    procedure chkSelectAllClick(Sender: TObject);
  private
    l: TStringList;
    fFileName: string;
    fRowCount: Integer;
    fColCount: Integer;
    fRangeColCount: Integer;
    fCompareSpacing: Integer;
    fExportTypeCount: Integer;
    fFilePath: string;
    fExportFile: Boolean;
    fExportFile2: Boolean;
    fExportFile3: Boolean;
    fExportFile4: Boolean;
    fExportFile5: Boolean;
    fExportFile6: Boolean;
    fValues: TInt64DynArray;

    procedure OnStateChange(Working: Boolean);
    procedure Compare;
    function BuildStringValue: string;
    function BuildCompareType(v: string): string;
    procedure CheckCanbeExported;
    procedure SaveGroupByFirstRow;
    procedure SaveGroupByCompareTypeSortByRowcount;
    procedure SaveGroupByCompareTypeSortByMaxRowSpacing;
    procedure SaveGroupByCompareTypeSortByCompareTypeCount;
    procedure SaveGroupByCompareTypeSortByMaxValueCount;
    procedure SaveCompareType;
    procedure ExportToFile;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  uGlobal, uTimer;

{$R *.dfm}

procedure TfrmMain.OnStateChange(Working: Boolean);
begin
  btnCompare.Enabled := not Working;
end;

function TfrmMain.BuildStringValue: string;
var
  i: Integer;
  s: string;
begin
  for i := Low(fValues) to High(fValues) do
    fValues[i] := fdmtData.FieldByName('Field' + (i + 1).ToString).AsLargeInt;
  if fColCount > fRangeColCount then Result := uGlobal.BuildStringValue(fValues, fRangeColCount)
  else Result := uGlobal.BuildStringValue(fValues);
end;

procedure TfrmMain.chkSelectAllClick(Sender: TObject);
begin
  with TCheckBox(Sender) do
  begin
    chkExportFile.Checked := Checked;
    chkExportFile2.Checked := Checked;
    chkExportFile3.Checked := Checked;
    chkExportFile4.Checked := Checked;
    chkExportFile5.Checked := Checked;
    chkExportFile6.Checked := Checked;
  end;
end;

function TfrmMain.BuildCompareType(v: string): string;
var
  i, CompareNumber: Integer;
  s, s2: string;
begin
  Result := v.Substring(1).Replace(' ', '');

  for i := 0 to v.Length div 5 - 1 do
  begin
    s := v.Substring(i * 5 + 1, 4);
    CompareNumber := s.Replace('Y', '').Replace('Z', '').Trim.ToInteger;
    CompareNumber := CompareNumber * 2;
    if s.IndexOf('Y') > -1 then CompareNumber := CompareNumber - 1;

    if not s2.IsEmpty then s2 := s2 + '.';
    s2 := s2 + CompareNumber.ToString;
  end;
  Result := Format('（第%s个）', [s2]) + Result;
end;

procedure TfrmMain.CheckCanbeExported;
var
  FirstRow: Integer;
begin
  fdmtCompareType.Filtered := False;
  fdmtCompareType.First;
  while not fdmtCompareType.Eof do
  begin
    fdmtCompareType.Edit;
    fdmtCompareType.FieldByName('CanbeExported').AsBoolean :=
      fdmtCompareType.FieldByName('CompareTypeCount').AsInteger >= fExportTypeCount;
    fdmtCompareType.Post;

    fdmtCompareType.Next;
  end;
  fdmtCompareType.Filter := 'CanbeExported = 1';
  fdmtCompareType.Filtered := True;

  fdmtData.First;
  while not fdmtData.Eof do
  begin
    fdmtData.Edit;
    fdmtData.FieldByName('CanbeExported').AsBoolean :=
      fdmtCompareType.Locate('CompareType', fdmtData.FieldByName('CompareType').AsString, []);
    fdmtData.Post;

    fdmtData.Next;
  end;

  fdmtFirstRow.First;
  while not fdmtFirstRow.Eof do
  begin
    FirstRow := fdmtFirstRow.FieldByName('FirstRow').AsInteger;
    fdmtData.Filtered := False;
    fdmtData.Filter := Format('CanbeExported = 1 AND FirstRow = %d', [FirstRow]);
    fdmtData.Filtered := True;

    fdmtFirstRow.Edit;
    fdmtFirstRow.FieldByName('CanbeExported').AsBoolean := fdmtData.RecordCount > 0;
    fdmtFirstRow.Post;

    fdmtFirstRow.Next;
  end;
  fdmtFirstRow.Filter := 'CanbeExported = 1';
  fdmtFirstRow.Filtered := True;
end;

procedure TfrmMain.SaveGroupByFirstRow;
var
  i, FirstRow, SaveFirstRow, RowSpacing, MaxRowSpacing, MinRowSpacing, Rowcount: Integer;
  s, CompareType: string;
begin
  l.Clear;
  MaxRowSpacing := 0;
  MinRowSpacing := 100;
  fdmtFirstRow.First;
  while not fdmtFirstRow.Eof do
  begin
    FirstRow := fdmtFirstRow.FieldByName('FirstRow').AsInteger;
    fdmtData.Filtered := False;
    fdmtData.Filter := Format('CanbeExported = 1 AND FirstRow = %d', [FirstRow]);
    fdmtData.Filtered := True;

    if fdmtFirstRow.RecNo = 1 then RowSpacing := FirstRow - 1
    else RowSpacing := FirstRow - SaveFirstRow;
    if RowSpacing > MaxRowSpacing then MaxRowSpacing := RowSpacing;
    if RowSpacing < MinRowSpacing then MinRowSpacing := RowSpacing;
    SaveFirstRow := FirstRow;

    s := '%d.（第%d行为首行）（邻行距 ↑%d，同行数：%d）';
    s := Format(s, [fdmtFirstRow.RecNo, FirstRow, RowSpacing, fdmtData.RecordCount]);
    l.Add('');
    l.Add('');
    l.Add('');
    l.Add(s);

    fdmtData.First;
    while not fdmtData.Eof do
    begin
      CompareType := BuildCompareType(fdmtData.FieldByName('CompareType').AsString);
      if fColCount = fRangeColCount then
      begin
        s := '（%d）.[代号：%s ] = 无【对应列】数： %d列 ；【列数字】：%s';
        s := Format(s, [
          fdmtData.RecNo,
          CompareType,
          fdmtData.FieldByName('ValueCount').AsInteger,
          BuildStringValue
        ]);
      end
      else
      begin
        s := '（%d）.[代号：%s ] = 【 %d-%d 】列 ；【列数字】：%s';
        s := Format(s, [
          fdmtData.RecNo,
          CompareType,
          fdmtData.FieldByName('ValueCount').AsInteger,
          fdmtData.FieldByName('ValueCount2').AsInteger,
          BuildStringValue
        ]);
      end;
      l.Add('');
      l.Add(s);

      fdmtData.Next;
    end;

    fdmtFirstRow.Next;
  end;
  RowSpacing := fRowCount - fCompareSpacing - FirstRow;
  if RowSpacing > MaxRowSpacing then MaxRowSpacing := RowSpacing;
  if RowSpacing < MinRowSpacing then MinRowSpacing := RowSpacing;
  s := '（第%d行为首行）（邻行距 ↑%d）';
  s := Format(s, [fRowCount - fCompareSpacing, RowSpacing]);
  l.Add('');
  l.Add('');
  l.Add('');
  l.Add(s);

  s := '最大的邻行距 1-100 行内：' + #$D#$A + #$D#$A;
  Rowcount := 0;
  for i := 0 to l.Count - 1 do
  begin
    if l[i].IndexOf(Format('邻行距 ↑%d', [MaxRowSpacing])) > -1 then
    begin
      Inc(Rowcount);
      s := s + Format('%d.%s' + #$D#$A, [Rowcount, l[i].Substring(l[i].IndexOf('.') + 1)]);

      if Rowcount >= 100 then Break;
    end;
  end;
  if MaxRowSpacing <> MinRowSpacing then
  begin
    s := s + #$D#$A + '最小的邻行距 1-100 行内：' + #$D#$A + #$D#$A;
    Rowcount := 0;
    for i := 0 to l.Count - 1 do
    begin
      if l[i].IndexOf(Format('邻行距 ↑%d', [MinRowSpacing])) > -1 then
      begin
        Inc(Rowcount);
        s := s + Format('%d.%s' + #$D#$A, [Rowcount, l[i].Substring(l[i].IndexOf('.') + 1)]);

        if Rowcount >= 100 then Break;
      end;
    end;
  end;
  l.Insert(0, s);
  l.Insert(0, '');
  l.Insert(0, '');
  l.Insert(0, '');

  s := fFilePath + Format('①.【排列】“%d”个以上【 [ 相同（第“N”行为首行）] 、不同[ 代号：（第“N.N”个）“N”Z.“N”Y  ] 】的组合.txt', [fExportTypeCount]);
  l.SaveToFile(s);
end;

procedure TfrmMain.SaveGroupByCompareTypeSortByRowcount;
var
  s, CompareType: string;
begin
  //计算
  fdmtCompareType.IndexName := 'CompareType';
  fdmtCompareType.First;
  while not fdmtCompareType.Eof do
  begin
    CompareType := fdmtCompareType.FieldByName('CompareType').AsString;
    fdmtData.Filtered := False;
    fdmtData.Filter := Format('CanbeExported = 1 AND CompareType = ''%s''', [CompareType]);
    fdmtData.Filtered := True;

    fdmtCompareType.Edit;
    fdmtCompareType.FieldByName('RowCount').AsInteger := fdmtData.RecordCount;
    fdmtCompareType.Post;

    fdmtCompareType.Next;
  end;
  //导出
  l.Clear;
  fdmtCompareType.IndexName := 'RowCount';
  fdmtCompareType.First;
  while not fdmtCompareType.Eof do
  begin
    CompareType := fdmtCompareType.FieldByName('CompareType').AsString;
    fdmtData.Filtered := False;
    fdmtData.Filter := Format('CanbeExported = 1 AND CompareType = ''%s''', [CompareType]);
    fdmtData.Filtered := True;

    CompareType := BuildCompareType(CompareType);
    l.Add('');
    l.Add('');
    l.Add('');
    l.Add(Format('%d.[代号：%s ] ；（ 最多 [ 不同首行数：%d 行 ] ）', [fdmtCompareType.RecNo, CompareType, fdmtData.RecordCount]));

    fdmtData.First;
    while not fdmtData.Eof do
    begin
      if fColCount = fRangeColCount then
      begin
        s := '（%d）（第%d行为首行）= 无【对应列】数： %d列 ；【列数字】：%s';
        s := Format(s, [
          fdmtData.RecNo,
          fdmtData.FieldByName('FirstRow').AsInteger,
          fdmtData.FieldByName('ValueCount').AsInteger,
          BuildStringValue
        ]);
      end
      else
      begin
        s := '（%d）（第%d行为首行）= 【 %d-%d 】列 ；【列数字】：%s';
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

    fdmtCompareType.Next;
  end;
  s := fFilePath + Format('②.【排列】“%d”个以上各个组合（相同代号、不同首行）的（不同首行数：最多 - 最少行）.txt', [fExportTypeCount]);
  l.SaveToFile(s);
end;

procedure TfrmMain.SaveGroupByCompareTypeSortByMaxRowSpacing;
var
  s, CompareType: string;
  FirstRow, SaveFirstRow, RowSpacing, MaxRowSpacing: Integer;
begin
  //计算
  fdmtCompareType.IndexName := 'CompareType';
  fdmtCompareType.First;
  while not fdmtCompareType.Eof do
  begin
    CompareType := fdmtCompareType.FieldByName('CompareType').AsString;
    fdmtData.Filtered := False;
    fdmtData.Filter := Format('CanbeExported = 1 AND CompareType = ''%s''', [CompareType]);
    fdmtData.Filtered := True;

    MaxRowSpacing := 0;
    fdmtData.First;
    while not fdmtData.Eof do
    begin
      FirstRow := fdmtData.FieldByName('FirstRow').AsInteger;
      if fdmtData.RecNo = 1 then RowSpacing := FirstRow - 1
      else RowSpacing := FirstRow - SaveFirstRow;
      if RowSpacing > MaxRowSpacing then MaxRowSpacing := RowSpacing;
      SaveFirstRow := FirstRow;

      fdmtData.Next;
    end;

    fdmtCompareType.Edit;
    fdmtCompareType.FieldByName('MaxRowSpacing').AsInteger := MaxRowSpacing;
    fdmtCompareType.Post;

    fdmtCompareType.Next;
  end;
  //导出
  l.Clear;
  fdmtCompareType.IndexName := 'MaxRowSpacing';
  fdmtCompareType.First;
  while not fdmtCompareType.Eof do
  begin
    CompareType := fdmtCompareType.FieldByName('CompareType').AsString;
    fdmtData.Filtered := False;
    fdmtData.Filter := Format('CanbeExported = 1 AND CompareType = ''%s''', [CompareType]);
    fdmtData.Filtered := True;

    MaxRowSpacing := fdmtCompareType.FieldByName('MaxRowSpacing').AsInteger;
    CompareType := BuildCompareType(CompareType);
    l.Add('');
    l.Add('');
    l.Add('');
    l.Add(Format('%d.[代号：%s ] ；（最大邻行距 ↑%d）', [fdmtCompareType.RecNo, CompareType, MaxRowSpacing]));

    fdmtData.First;
    while not fdmtData.Eof do
    begin
      FirstRow := fdmtData.FieldByName('FirstRow').AsInteger;
      if fdmtData.RecNo = 1 then RowSpacing := FirstRow - 1
      else RowSpacing := FirstRow - SaveFirstRow;
      SaveFirstRow := FirstRow;

      if fColCount = fRangeColCount then
      begin
        s := '（%d）（第%d行为首行）（邻行距 ↑%d）= 无【对应列】数： %d列 ；【列数字】：%s';
        s := Format(s, [
          fdmtData.RecNo,
          fdmtData.FieldByName('FirstRow').AsInteger,
          RowSpacing,
          fdmtData.FieldByName('ValueCount').AsInteger,
          BuildStringValue
        ]);
      end
      else
      begin
        s := '（%d）（第%d行为首行）（邻行距 ↑%d）= 【 %d-%d 】列 ；【列数字】：%s';
        s := Format(s, [
          fdmtData.RecNo,
          fdmtData.FieldByName('FirstRow').AsInteger,
          RowSpacing,
          fdmtData.FieldByName('ValueCount').AsInteger,
          fdmtData.FieldByName('ValueCount2').AsInteger,
          BuildStringValue
        ]);
      end;
      l.Add('');
      l.Add(s);

      fdmtData.Next;
    end;
    RowSpacing := fRowCount - fCompareSpacing - FirstRow;
    s := '（%d）（第%d行为首行）（邻行距 ↑%d）';
    s := Format(s, [fdmtData.RecordCount + 1, fRowCount - fCompareSpacing, RowSpacing]);
    l.Add('');
    l.Add(s);

    fdmtCompareType.Next;
  end;
  s := fFilePath + Format('③.【排列】“%d”个以上各个组合（相同代号、不同首行）的（邻行距：最大↑“N”- 最小↑“N”）.txt', [fExportTypeCount]);
  l.SaveToFile(s);
end;

procedure TfrmMain.SaveGroupByCompareTypeSortByCompareTypeCount;
var
  CompareTypeList: TStringList;
  CompareTypeCount: Integer;
  s, CompareType: string;
  c: Char;
begin
  l.Clear;
  fdmtCompareType.IndexName := 'CompareType2';
  fdmtCompareType.First;
  while not fdmtCompareType.Eof do
  begin
    CompareType := fdmtCompareType.FieldByName('CompareType').AsString;
    fdmtData.Filtered := False;
    fdmtData.Filter := Format('CanbeExported = 1 AND CompareType = ''%s''', [CompareType]);
    fdmtData.Filtered := True;

    CompareType := BuildCompareType(CompareType);
    CompareTypeCount := fdmtCompareType.FieldByName('CompareTypeCount').AsInteger;
    l.Add('');
    l.Add('');
    l.Add('');
    l.Add(Format('%d.[代号：%s ] ；（ 最多 [ 组合数：%d 个 ] ）', [fdmtCompareType.RecNo, CompareType, CompareTypeCount]));

    fdmtData.First;
    while not fdmtData.Eof do
    begin
      if fColCount = fRangeColCount then
      begin
        s := '（%d）（第%d行为首行）= 无【对应列】数： %d列 ；【列数字】：%s';
        s := Format(s, [
          fdmtData.RecNo,
          fdmtData.FieldByName('FirstRow').AsInteger,
          fdmtData.FieldByName('ValueCount').AsInteger,
          BuildStringValue
        ]);
      end
      else
      begin
        s := '（%d）（第%d行为首行）= 【 %d-%d 】列 ；【列数字】：%s';
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

    fdmtCompareType.Next;
  end;
  s := fFilePath + Format('④.【排列】“%d”个以上各个组合（相同代号、不同首行）的（组合数：最多 - 最少个）.txt', [fExportTypeCount]);
  l.SaveToFile(s);
end;

procedure TfrmMain.SaveGroupByCompareTypeSortByMaxValueCount;
var
  CompareTypeList: TStringList;
  MaxValueCount, MaxValueCount2, MaxValueCount3, ValueCount, ValueCount2: Integer;
  s, CompareType: string;
begin
  //计算
  fdmtCompareType.IndexName := 'CompareType';
  fdmtCompareType.First;
  while not fdmtCompareType.Eof do
  begin
    CompareType := fdmtCompareType.FieldByName('CompareType').AsString;
    fdmtData.Filtered := False;
    fdmtData.Filter := Format('CanbeExported = 1 AND CompareType = ''%s''', [CompareType]);
    fdmtData.Filtered := True;

    MaxValueCount := 0;
    fdmtData.First;
    while not fdmtData.Eof do
    begin
      ValueCount := fdmtData.FieldByName('ValueCount').AsInteger;
      ValueCount2 := fdmtData.FieldByName('ValueCount2').AsInteger;
      if (MaxValueCount < ValueCount + ValueCount2)
        or ((MaxValueCount = ValueCount + ValueCount2) and (MaxValueCount2 < ValueCount))
      then
      begin
        MaxValueCount := ValueCount + ValueCount2;
        MaxValueCount2 := ValueCount;
        MaxValueCount3 := ValueCount2;
      end;

      fdmtData.Next;
    end;

    fdmtCompareType.Edit;
    fdmtCompareType.FieldByName('MaxValueCount').AsInteger := MaxValueCount;
    fdmtCompareType.FieldByName('ValueCount').AsInteger := MaxValueCount2;
    fdmtCompareType.FieldByName('ValueCount2').AsInteger := MaxValueCount3;
    fdmtCompareType.Post;

    fdmtCompareType.Next;
  end;

  l.Clear;
  fdmtCompareType.IndexName := 'MaxValueCount';
  fdmtCompareType.First;
  while not fdmtCompareType.Eof do
  begin
    CompareType := fdmtCompareType.FieldByName('CompareType').AsString;
    fdmtData.Filtered := False;
    fdmtData.Filter := Format('CanbeExported = 1 AND CompareType = ''%s''', [CompareType]);
    fdmtData.Filtered := True;

    CompareType := BuildCompareType(CompareType);
    ValueCount := fdmtCompareType.FieldByName('ValueCount').AsInteger;
    ValueCount2 := fdmtCompareType.FieldByName('ValueCount2').AsInteger;
    if fColCount = fRangeColCount then
    begin
      s := '%d.[代号：%s ] ；（ 最多 [ 无【对应列】数：%d列 ] ）';
      s := Format(s, [fdmtCompareType.RecNo, CompareType, ValueCount])
    end
    else
    begin
      s := '%d.[代号：%s ] ；（ 最多 [ 无【对应列】数：%d-%d列 ] ）';
      s := Format(s, [fdmtCompareType.RecNo, CompareType, ValueCount, ValueCount2])
    end;
    l.Add('');
    l.Add('');
    l.Add('');
    l.Add(s);

    fdmtData.First;
    while not fdmtData.Eof do
    begin
      ValueCount := fdmtData.FieldByName('ValueCount').AsInteger;
      ValueCount2 := fdmtData.FieldByName('ValueCount2').AsInteger;
      if fColCount = fRangeColCount then
      begin
        s := '（%d）（第%d行为首行）= 无【对应列】数： %d列 ；【列数字】：%s';
        s := Format(s, [
          fdmtData.RecNo,
          fdmtData.FieldByName('FirstRow').AsInteger,
          fdmtData.FieldByName('ValueCount').AsInteger,
          BuildStringValue
        ]);
      end
      else
      begin
        s := '（%d）（第%d行为首行）= 【 %d-%d 】列 ；【列数字】：%s';
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

    fdmtCompareType.Next;
  end;
  s := fFilePath + Format('⑤.【排列】“%d”个以上各个组合（相同代号、不同首行）的（无【对应列】数：最多 - 最少列）.txt', [fExportTypeCount]);
  l.SaveToFile(s);
end;

procedure TfrmMain.SaveCompareType;
var
  s, CompareType: string;
begin
  l.Clear;
  fdmtCompareType.IndexName := 'CompareType';
  fdmtCompareType.First;
  while not fdmtCompareType.Eof do
  begin
    CompareType := BuildCompareType(fdmtCompareType.FieldByName('CompareType').AsString);
    CompareType := CompareType.Substring(CompareType.IndexOf('）') + 1);
    s := Format('%d=%s', [fdmtCompareType.RecNo, CompareType]);
    l.Add(s);

    fdmtCompareType.Next;
  end;
  s := fFilePath + Format('⑥.【保存】“%d”个以上各个组合（相同代号、不同首行）的（代号：1.“N”ZY ）.txt', [fExportTypeCount]);
  l.SaveToFile(s);
end;

procedure TfrmMain.Compare;
var
  Arr, Arr2: TIntDyadicArray;
  i, i2, i3, i4, i5, StartNo, EndNo,
  CompareRowNo, FirstRow, ValueCount: Integer;
  s, CompareType: string;
  rZ, rY, CombineRows: TIntegerDynArray;
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

      if v2 + Offset - 1 in [0..fColCount - 1] then
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
  begin
    if not fdmtFirstRow.Locate('FirstRow', FirstRow, []) then
    begin
      fdmtFirstRow.Append;
      fdmtFirstRow.FieldByName('CanbeExported').AsBoolean := False;
      fdmtFirstRow.FieldByName('FirstRow').AsInteger := FirstRow;
      fdmtFirstRow.Post;
    end;

    CompareTypeCount := 0;
    for c in CompareType do
      if c = '.' then Inc(CompareTypeCount);
    if not fdmtCompareType.Locate('CompareType', CompareType, []) then
    begin
      fdmtCompareType.Append;
      fdmtCompareType.FieldByName('CanbeExported').AsBoolean := False;
      fdmtCompareType.FieldByName('CompareType').AsString := CompareType;
      fdmtCompareType.FieldByName('CompareTypeCount').AsInteger := CompareTypeCount;
      fdmtCompareType.FieldByName('RowCount').AsInteger := 0;
      fdmtCompareType.FieldByName('MaxRowSpacing').AsInteger := 0;
      fdmtCompareType.FieldByName('MaxValueCount').AsInteger := 0;
      fdmtCompareType.FieldByName('ValueCount').AsInteger := 0;
      fdmtCompareType.FieldByName('ValueCount2').AsInteger := 0;
      fdmtCompareType.Post;
    end;

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
    fdmtData.FieldByName('CanbeExported').AsBoolean := False;
    fdmtData.FieldByName('FirstRow').AsInteger := FirstRow;
    fdmtData.FieldByName('CompareType').AsString := CompareType;
    fdmtData.FieldByName('ValueCount').AsInteger := ValueCount;
    fdmtData.FieldByName('ValueCount2').AsInteger := ValueCount2;
    for i := Low(fValues) to High(fValues) do
      fdmtData.FieldByName('Field' + (i + 1).ToString).AsLargeInt := fValues[i];
    fdmtData.Post;
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
  LoadData(fFileName, Arr, Arr2);
  fRowCount := Length(Arr);
  //合并
  for i := Low(Arr2) to High(Arr2) do
  begin
    SetLength(Arr[i], Length(Arr[i]) + Length(Arr2[i]));
    for i2 := Low(Arr2[i]) to High(Arr2[i]) do
      Arr[i][High(Arr[i]) - High(Arr2[i]) + i2] := Arr2[i][i2] + fRangeColCount;
  end;

  for i := Low(Arr) to High(Arr) - fCompareSpacing do
  begin
    //获取符合数据
    FirstRow := i + 1;
    StartNo := fdmtData.RecNo + 1;
    for i2 := i + 1 to i + fCompareSpacing do
    begin
      CompareRowNo := i2 - i;
      rY := CompareRow(Arr[i], Arr[i2], i2 - i);
      if Length(rY) > 0 then
        AddRow2(FirstRow, Format('.%3dY', [CompareRowNo]), rY);
      rZ := CompareRow(Arr[i], Arr[i2], i - i2);
      if Length(rZ) > 0 then
        AddRow2(FirstRow, Format('.%3dZ', [CompareRowNo]), rZ);
    end;
    EndNo := fdmtData.RecNo;
    if EndNo <= StartNo then Continue;
    //组合数据
    for i2 := 2 to EndNo - StartNo + 1 do
    begin
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
  end;
end;

procedure TfrmMain.ExportToFile;
begin
  if fdmtData.RecordCount = 0 then Exit;
  CheckCanbeExported;
  if fExportFile then SaveGroupByFirstRow;
  if fExportFile2 then SaveGroupByCompareTypeSortByRowcount;
  if fExportFile3 then SaveGroupByCompareTypeSortByMaxRowSpacing;
  if fExportFile4 then SaveGroupByCompareTypeSortByCompareTypeCount;
  if fExportFile5 then SaveGroupByCompareTypeSortByMaxValueCount;
  if fExportFile6 then SaveCompareType;
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
  {ProbationExpired := True;
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
  end;}

  fdmtFirstRow.Close;
  fdmtFirstRow.FieldDefs.Clear;
  with fdmtFirstRow.FieldDefs.AddFieldDef do
  begin
    Name := 'CanbeExported';
    DataType := ftBoolean;
  end;
  with fdmtFirstRow.FieldDefs.AddFieldDef do
  begin
    Name := 'FirstRow';
    DataType := ftInteger;
  end;
  fdmtFirstRow.Open;

  fdmtCompareType.Close;
  fdmtCompareType.FieldDefs.Clear;
  with fdmtCompareType.FieldDefs.AddFieldDef do
  begin
    Name := 'CanbeExported';
    DataType := ftBoolean;
  end;
  with fdmtCompareType.FieldDefs.AddFieldDef do
  begin
    Name := 'CompareType';
    DataType := ftString;
    Size := 100;
  end;
  with fdmtCompareType.FieldDefs.AddFieldDef do
  begin
    Name := 'CompareTypeCount';
    DataType := ftSmallInt;
  end;
  with fdmtCompareType.FieldDefs.AddFieldDef do
  begin
    Name := 'RowCount';
    DataType := ftInteger;
  end;
  with fdmtCompareType.FieldDefs.AddFieldDef do
  begin
    Name := 'MaxRowSpacing';
    DataType := ftInteger;
  end;
  with fdmtCompareType.FieldDefs.AddFieldDef do
  begin
    Name := 'MaxValueCount';
    DataType := ftSmallInt;
  end;
  with fdmtCompareType.FieldDefs.AddFieldDef do
  begin
    Name := 'ValueCount';
    DataType := ftSmallInt;
  end;
  with fdmtCompareType.FieldDefs.AddFieldDef do
  begin
    Name := 'ValueCount2';
    DataType := ftSmallInt;
  end;
  fdmtCompareType.AddIndex('CompareType', 'CompareTypeCount;CompareType', '', [], '');
  fdmtCompareType.AddIndex('CompareType2', 'CompareTypeCount;CompareType', '', [], 'CompareTypeCount');
  fdmtCompareType.AddIndex('RowCount', 'RowCount;CompareTypeCount;CompareType', '', [], 'RowCount');
  fdmtCompareType.AddIndex('MaxRowSpacing', 'MaxRowSpacing;CompareTypeCount;CompareType', '', [], 'MaxRowSpacing');
  fdmtCompareType.AddIndex('MaxValueCount', 'MaxValueCount;ValueCount;CompareTypeCount;CompareType', '', [], 'MaxValueCount;ValueCount');
  fdmtCompareType.Open;

  l := TStringList.Create;
  {edtFileName.Text := 'D:\1.txt';
  edtColCount.Text := '32';
  edtRangeColCount.Text := '20';
  edtCompareSpacing.Text := '2';
  edtExportTypeCount.Text := '1';
  chkExportFile.Checked := True;
  chkExportFile2.Checked := True;
  chkExportFile3.Checked := True;
  chkExportFile4.Checked := True;
  chkExportFile5.Checked := True;
  chkExportFile6.Checked := True;
  btnCompare.Click; }
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  l.Free;
end;

procedure TfrmMain.pnlTopDblClick(Sender: TObject);
begin
  if not btnCompare.Enabled then Exit;
  dsData.Enabled := not dsData.Enabled;
  dsFirstRow.Enabled := not dsFirstRow.Enabled;
  dsCompareType.Enabled := not dsCompareType.Enabled;
end;

procedure TfrmMain.btnCompareClick(Sender: TObject);
var
  i: Integer;
begin
  if not (TryStrToInt(edtCompareSpacing.Text, fCompareSpacing) and (fCompareSpacing > 0)) then
    raise Exception.Create('请输入有效比较次数');
  if not (TryStrToInt(edtColCount.Text, fColCount) and (fColCount > 0)) then
    raise Exception.Create('请输入有效总列数');
  if not (TryStrToInt(edtRangeColCount.Text, fRangeColCount) and (fRangeColCount - 1 in [1..fColCount - 1])) then
    raise Exception.Create('请输入有效范围列数');
  if not (TryStrToInt(edtExportTypeCount.Text, fExportTypeCount) and (fExportTypeCount > 0)) then
    raise Exception.Create('请输入有效导出次数');

  fFileName := edtFileName.Text;
  fFilePath := TPath.GetDirectoryName(fFileName) + '\';
  fExportFile := chkExportFile.Checked;
  fExportFile2 := chkExportFile2.Checked;
  fExportFile3 := chkExportFile3.Checked;
  fExportFile4 := chkExportFile4.Checked;
  fExportFile5 := chkExportFile5.Checked;
  fExportFile6 := chkExportFile6.Checked;

  SetLength(fValues, Ceil(fColCount / 64));

  fdmtData.Close;
  fdmtData.FieldDefs.Clear;
  with fdmtData.FieldDefs.AddFieldDef do
  begin
    Name := 'CanbeExported';
    DataType := ftBoolean;
  end;
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

  OnStateChange(True);
  TTask.Create(procedure
  var
    fStopTime: Boolean;

    procedure StopTime;
    begin
      TThread.Synchronize(nil, procedure
      begin
        fStopTime := True;
      end);
    end;
  begin
    StartTheTime(
      function: Boolean
      begin
        Result := fStopTime;
      end,
      procedure(s: string)
      begin
        lblUseTime.Caption := s;
      end
    );
    fdmtFirstRow.DisableControls;
    fdmtCompareType.DisableControls;
    fdmtData.DisableControls;
    try
      try
        Compare;
        ExportToFile;
        StopTime;
        ShowMessage('查询完毕');
      except
        on e: Exception do raise Exception.Create(e.Message);
      end;
    finally
      fdmtData.Filtered := False;
      fdmtData.EnableControls;
      fdmtFirstRow.Filtered := False;
      fdmtFirstRow.EnableControls;
      fdmtCompareType.Filtered := False;
      fdmtCompareType.EnableControls;
      StopTime;
      TThread.Synchronize(nil, procedure
      begin
        OnStateChange(False);
      end);
    end;
  end).Start;
end;

end.
