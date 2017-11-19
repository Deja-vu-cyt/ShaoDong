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
    dbgrdData: TDBGridEh;
    dsData: TDataSource;
    pnlTop: TPanel;
    lblUseTime: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pnlTopDblClick(Sender: TObject);
    procedure edtFileNameClick(Sender: TObject);
    procedure btnCompareClick(Sender: TObject);
  private
    l: TStringList;
    fFileName: string;
    fRowCount: Integer;
    fColCount: Integer;
    fRangeColCount: Integer;
    fCompareSpacing: Integer;
    fFilePath: string;
    fValues: TInt64DynArray;
    fColumns: TInt64DynArray;

    procedure OnStateChange(Working: Boolean);
    procedure Compare;
    function BuildCompareType(v: string): string;
    procedure Save(Flag: Integer = 1);
    procedure ExportToFile;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  uGlobal, uInternetTime, uTimer;

{$R *.dfm}

procedure TfrmMain.OnStateChange(Working: Boolean);
begin
  btnCompare.Enabled := not Working;
end;

function TfrmMain.BuildCompareType(v: string): string;
var
  Arr: TIntegerDynArray;
begin
  Result := v.Replace(' ', '');

  if fColCount > fRangeColCount then
  begin
    StrToIntArray(Result, Arr);
    Result := BuildStringValue(Arr, fRangeColCount);
  end;
end;

procedure TfrmMain.Save(Flag: Integer = 1);
var
  s, CompareType, LastCompareType: string;
  CompareTypeCount, RowNo, RowSpacing: Integer;
begin
  l.Clear;
  fdmtData.First;
  while not fdmtData.Eof do
  begin
    CompareType := fdmtData.FieldByName('CompareType').AsString;
    if (Flag = 1) and CompareType.Equals(LastCompareType) then
    begin
      fdmtData.Next;
      Continue;
    end;
    LastCompareType := CompareType;
    CompareTypeCount := fdmtData.FieldByName('CompareTypeCount').AsInteger;
    RowNo := fdmtData.FieldByName('RowNo').AsInteger;
    RowSpacing := fdmtData.FieldByName('RowSpacing').AsInteger;
    case Flag of
      1, 2:
      begin
        s := '%d=列数字%s [（代号1-%d）：%s]：第%d行（邻行距↑%d）';
        s := Format(s, [
          fdmtData.RecNo,
          BuildCompareType(CompareType),
          CompareTypeCount,
          BuildCompareType(CompareType),
          RowNo,
          RowSpacing
        ]);
      end
      else
      begin
        s := '%d=[（1-%d）：%s]：%d.（%d）';
        s := Format(s, [
          fdmtData.RecNo,
          CompareTypeCount,
          BuildCompareType(CompareType),
          RowNo,
          RowSpacing
        ]);
      end;
    end;
    l.Add(s);

    fdmtData.Next;
  end;
  if fColCount = fRangeColCount then
  begin
    case Flag of
      1: s := '①. 导出：1-%d次组合：集合（各组合的首行）（1-%d）列.txt';
      2: s := '②. 导出：1-%d次组合： 加上（序号1-N=）（1-%d）列.txt';
      else s := '③. 导出：1-%d次组合： 减去（每行的文字）（1-%d）列.txt';
    end;
    s := fFilePath + Format(s, [fCompareSpacing, fColCount]);
  end
  else
  begin
    case Flag of
      1: s := '①. 导出：1-%d次组合：集合（各组合的首行）（1-%d）（1-%d）列.txt';
      2: s := '②. 导出：1-%d次组合： 加上（序号1-N=）（1-%d）（1-%d）列.txt';
      else s := '③. 导出：1-%d次组合： 减去（每行的文字）（1-%d）（1-%d）列.txt';
    end;
    s := fFilePath + Format(s, [fCompareSpacing, fRangeColCount, fColCount - fRangeColCount]);
  end;
  l.SaveToFile(s);
end;

procedure TfrmMain.Compare;

  procedure AddRow(CompareType: string; RowNo, RowSpacing: Integer);
  begin
    fdmtData.Append;
    fdmtData.FieldByName('CompareType').AsString := CompareType;
    fdmtData.FieldByName('CompareTypeCount').AsInteger := Length(CompareType.Split(['、']));
    fdmtData.FieldByName('RowNo').AsInteger := RowNo;
    fdmtData.FieldByName('RowSpacing').AsInteger := RowSpacing;
    fdmtData.Post;
  end;

var
  Arr: TInt64DyadicArray;
  i, i2, i3, ColNo, RowNo, LastRowNo: Integer;
  s, CompareType: string;
  CompareColumns: TIntegerDynArray;
  f: TField;
  IsMeet: Boolean;
begin
  LoadData(fFileName, Arr, fRangeColCount);
  fRowCount := Length(Arr);

  fdmtData.CancelUpdates;
  for i := 1 to fCompareSpacing do
  begin
    if i > fColCount then Break;
    //初始化比较列
    SetLength(CompareColumns, i);
    for i2 := Low(CompareColumns) to High(CompareColumns) do
      CompareColumns[i2] := i2 + 1;
    //循环比较
    repeat
      //合并比较列
      for i2 := Low(fColumns) to High(fColumns) do fColumns[i2] := 0;
      CompareType := '';
      for ColNo in CompareColumns do
      begin
        fColumns[(ColNo - 1) div 64] := fColumns[(ColNo - 1) div 64] or (i64 shl (64 - ColNo mod 64));
        if not CompareType.IsEmpty then CompareType := CompareType + '、';
        CompareType := CompareType + Format('%3d', [ColNo]);
      end;
      //比较每一行
      LastRowNo := 0;
      for i2 := Low(Arr) to High(Arr) do
      begin
        RowNo := i2 + 1;
        if Int64ArrayEqual(Arr[i2], fColumns) then
        begin
          AddRow(CompareType, RowNo, RowNo - LastRowNo);
          LastRowNo := RowNo;
        end;
      end;
      if LastRowNo > 0 then
        AddRow(CompareType, RowNo, RowNo - LastRowNo);
      //[比较列]最后一列加一
      i2 := High(CompareColumns);
      CompareColumns[i2] := CompareColumns[i2] + 1;
      if CompareColumns[i2] > fColCount then
      begin
        for i2 := High(CompareColumns) - 1 downto Low(CompareColumns) do
        begin
          CompareColumns[i2] := CompareColumns[i2] + 1;
          if CompareColumns[i2] + 1 = CompareColumns[i2 + 1] then Continue;
          for i3 := i2 + 1 to High(CompareColumns) do
            CompareColumns[i3] := CompareColumns[i3 - 1] + 1;
          Break;
        end;
      end;
    until CompareColumns[High(CompareColumns)] > fColCount;
  end;
end;

procedure TfrmMain.ExportToFile;
begin
  if fdmtData.RecordCount = 0 then Exit;
  Save;
  Save(2);
  Save(3);
end;

procedure TfrmMain.edtFileNameClick(Sender: TObject);
begin
  if not OpenDialog.Execute then Exit;
  TEdit(Sender).Text := OpenDialog.FileName;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  s: string;
  InternetTime: TDateTime;
begin
  try
    InternetTime := GetInternetTime;
    if (InternetTime = 0) and (InternetTime > StrToDateTime('2017-11-30')) then
      raise Exception.Create('软件已过期');
  except
    on e: Exception do
    begin
      ShowMessage(e.Message);
      Application.Terminate;
    end;
  end;

  fdmtData.Close;
  fdmtData.FieldDefs.Clear;
  with fdmtData.FieldDefs.AddFieldDef do
  begin
    Name := 'CompareType';
    DataType := ftString;
    Size := 100;
  end;
  with fdmtData.FieldDefs.AddFieldDef do
  begin
    Name := 'CompareTypeCount';
    DataType := ftInteger;
  end;
  with fdmtData.FieldDefs.AddFieldDef do
  begin
    Name := 'RowNo';
    DataType := ftInteger;
  end;
  with fdmtData.FieldDefs.AddFieldDef do
  begin
    Name := 'RowSpacing';
    DataType := ftInteger;
  end;
  fdmtData.Open;

  l := TStringList.Create;
  {edtFileName.Text := 'D:\1.txt';
  edtColCount.Text := '32';
  edtRangeColCount.Text := '32';
  edtCompareSpacing.Text := '2';
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

  fFileName := edtFileName.Text;
  fFilePath := TPath.GetDirectoryName(fFileName) + '\';

  SetLength(fValues, Ceil(fColCount / 64));
  SetLength(fColumns, Length(fValues));

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
      StopTime;
      TThread.Synchronize(nil, procedure
      begin
        OnStateChange(False);
      end);
    end;
  end).Start;
end;

end.
