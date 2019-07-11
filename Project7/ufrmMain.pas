unit ufrmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.Types, System.IOUtils, System.Diagnostics,
  Vcl.Controls, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.FileCtrl,
  Vcl.CheckLst, Vcl.ComCtrls, Vcl.Menus, Vcl.Forms;

type
  TfrmMain = class(TForm)
    OpenDialog: TOpenDialog;
    Timer: TTimer;
    Label1: TLabel;
    lblUseTime: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edtFileDirectory: TEdit;
    edtIntervalValue: TEdit;
    edtIntervalValue2: TEdit;
    Label2: TLabel;
    edtFileDirectory2: TEdit;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    edtSameValueCount: TEdit;
    edtSameValueCount3: TEdit;
    btnCompare: TButton;
    edtMinSameValueCount: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    edtMergeSameValueCount: TEdit;
    edtMergeSameValueCount3: TEdit;
    edtMergeSameValueCount2: TEdit;
    edtMergeSameValueCount4: TEdit;
    btnCompare2: TButton;
    edtMergeMinSameValueCount: TEdit;
    Label15: TLabel;
    Label16: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure edtFileDirectoryClick(Sender: TObject);
    procedure btnCompareClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    function CheckIntervalValues(var IntervalValues: TWordDynArray): Boolean;
    function CheckMergeSameValueCount(var ValueCount, ValueCount2: Integer): Boolean;
    function CheckMergeSameValueCount2(var ValueCount, ValueCount2: Integer): Boolean;
    procedure OnStateChange(Working: Boolean);
    procedure OnTerminate(Sender: TObject);
  public

  end;

var
  frmMain: TfrmMain;

implementation

uses
  uDataComputer, uControlHelper;

{$R *.dfm}

function TfrmMain.CheckIntervalValues(var IntervalValues: TWordDynArray): Boolean;
var
  v, v2: Integer;
begin
  Result := edtIntervalValue.TryToValue(v);
  if not Result then Exit;
  Result := edtIntervalValue2.TryToValue(v2);
  if not Result then Exit;
  Result := (v > 0) and (v2 >= 0) and (v + v2 <= 256);
  if not Result then Exit;
  SetLength(IntervalValues, 1);
  IntervalValues[0] := v;
  if v2 > 0 then
  begin
    SetLength(IntervalValues, 2);
    IntervalValues[1] := v2;
  end;
end;

function TfrmMain.CheckMergeSameValueCount(var ValueCount, ValueCount2: Integer): Boolean;
begin
  Result := edtMergeSameValueCount.TryToValue(ValueCount);
  if not Result then Exit;
  Result := edtMergeSameValueCount2.TryToValue(ValueCount2);
  if not Result then Exit;
  Result := (ValueCount2 > 0) and (ValueCount2 >= ValueCount);
end;

function TfrmMain.CheckMergeSameValueCount2(var ValueCount, ValueCount2: Integer): Boolean;
begin
  Result := edtMergeSameValueCount3.TryToValue(ValueCount);
  if not Result then Exit;
  Result := edtMergeSameValueCount4.TryToValue(ValueCount2);
  if not Result then Exit;
  Result := (ValueCount2 > 0) and (ValueCount2 >= ValueCount);
end;

procedure TfrmMain.OnStateChange(Working: Boolean);
begin
  btnCompare.Enabled := not Working;
  btnCompare2.Enabled := not Working;
end;

procedure TfrmMain.OnTerminate(Sender: TObject);
begin
  TThread.Synchronize(nil, procedure
  begin
    Timer.Enabled := False;
    OnStateChange(False);
    fDataComputer := nil;
  end);
end;

procedure TfrmMain.TimerTimer(Sender: TObject);
var
  TS: TTimeStamp;
  Days: Byte;
  Seconds: Int64;
begin
  Seconds := fDataComputer.Stopwatch.ElapsedMilliseconds;
  TS.Date := DateDelta;
  TS.Time := Seconds mod 86400000;
  Days := Seconds div 86400000;
  lblUseTime.Caption := Format('处理所需时间：%d日', [Days]) + FormatDateTime('H小时M分S秒', TimeStampToDateTime(TS));
end;

procedure TfrmMain.edtFileDirectoryClick(Sender: TObject);
var
  FileDirectory: string;
begin
  //if not OpenDialog.Execute then Exit;
  if not SelectDirectory('', '', FileDirectory) then Exit;

  TEdit(Sender).Text := FileDirectory;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  OnStateChange(False);
  {fDataComputer := TDataComputer.Create;
  fDataComputer.MergeMode := False;
  fDataComputer.IntervalValues := [35, 12];
  fDataComputer.SourceFileDirectory := 'I:\4. 原数据库 (第1-4模式) （配套）Project5 （调整 5）\01. （第1-4模式）（界面 、设置）（整理资料）（注：页面 1）\1. 读取 [ 查询（TXT）文本 ]（ 第 2 模式 ）\';
  fDataComputer.CompareFileDirectory := 'I:\4. 原数据库 (第1-4模式) （配套）Project5 （调整 5）\01. （第1-4模式）（界面 、设置）（整理资料）（注：页面 1）\2. 读取 [ 比较（TXT）文本 ]（ 第 2 模式 ）\';
  fDataComputer.ExportDirectory := ExtractFilePath(ParamStr(0));
  fDataComputer.SameValueCount := 2;
  fDataComputer.SameValueCount2 := 2;
  fDataComputer.SameValueCount3 := 0;
  fDataComputer.SameValueCount4 := 1;
  fDataComputer.MinSameValueCount := 2;
  fDataComputer.Start;
  fDataComputer.WaitFor;
  Application.Terminate;}
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if Assigned(fDataComputer) then
  begin
    fDataComputer.Terminate;
    fDataComputer.WaitFor;
    fDataComputer.Free;
  end;
end;

procedure TfrmMain.btnCompareClick(Sender: TObject);
var
  IntervalValues: TWordDynArray;
  SameValueCount, SameValueCount2, SameValueCount3, SameValueCount4,
  MinSameValueCount: Integer;
begin
  if not CheckIntervalValues(IntervalValues) then
    raise Exception.Create('请输入有效区域');

  if Sender = btnCompare2 then
  begin
    if not CheckMergeSameValueCount(SameValueCount, SameValueCount2) then
      raise Exception.Create('请输入有效第一区域相同列数范围');
    if not CheckMergeSameValueCount2(SameValueCount3, SameValueCount4) then
      raise Exception.Create('请输入有效第二区域相同列数范围');
    if not edtMergeMinSameValueCount.TryToValue(MinSameValueCount) then
      raise Exception.Create('请输入有效数值');
  end
  else
  begin
    if not edtSameValueCount.TryToValue(SameValueCount) then
      raise Exception.Create('请输入有效第一区域相同列数');
    if not edtSameValueCount3.TryToValue(SameValueCount3) then
      raise Exception.Create('请输入有效第二区域相同列数');
    if not edtMinSameValueCount.TryToValue(MinSameValueCount) then
      raise Exception.Create('请输入有效数值');
    SameValueCount2 := 0;
    SameValueCount4 := 0;
  end;

  OnStateChange(True);
  Timer.Enabled := True;

  fDataComputer := TDataComputer.Create;
  fDataComputer.OnTerminate := OnTerminate;
  fDataComputer.MergeMode := Sender = btnCompare2;
  fDataComputer.IntervalValues := IntervalValues;
  fDataComputer.SourceFileDirectory := edtFileDirectory.Text;
  fDataComputer.CompareFileDirectory := edtFileDirectory2.Text;
  fDataComputer.ExportDirectory := TPath.GetDirectoryName(fDataComputer.CompareFileDirectory);
  fDataComputer.SameValueCount := SameValueCount;
  fDataComputer.SameValueCount2 := SameValueCount2;
  fDataComputer.SameValueCount3 := SameValueCount3;
  fDataComputer.SameValueCount4 := SameValueCount4;
  fDataComputer.MinSameValueCount := MinSameValueCount;
  fDataComputer.Start;
end;

end.
