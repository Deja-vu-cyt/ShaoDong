unit ufrmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.Types, System.IOUtils, System.Diagnostics,
  Vcl.Controls, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.FileCtrl,
  Vcl.CheckLst, Vcl.ComCtrls, Vcl.Menus, Vcl.Forms;

type
  TfrmMain = class(TForm)
    ScrollBox1: TScrollBox;
    Label1: TLabel;
    lblUseTime: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label2: TLabel;
    edtFileDirectory: TEdit;
    edtIntervalValue: TEdit;
    edtIntervalValue2: TEdit;
    edtFileDirectory2: TEdit;
    OpenDialog: TOpenDialog;
    Timer: TTimer;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label18: TLabel;
    Label19: TLabel;
    edtSeparateSameValueCount: TEdit;
    edtSeparateSameValueCount2: TEdit;
    btnCompare: TButton;
    TabSheet2: TTabSheet;
    Label10: TLabel;
    Label11: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label17: TLabel;
    btnCompare2: TButton;
    edtSameValueCount: TEdit;
    edtSameValueCount3: TEdit;
    edtSameValueCount2: TEdit;
    edtSameValueCount4: TEdit;
    edtSameValueCount5: TEdit;
    edtSameValueCount7: TEdit;
    edtSameValueCount6: TEdit;
    edtSameValueCount8: TEdit;
    edtSameValueCount9: TEdit;
    edtSameValueCount11: TEdit;
    edtSameValueCount10: TEdit;
    edtSameValueCount12: TEdit;
    edtSameValueCount13: TEdit;
    edtSameValueCount15: TEdit;
    edtSameValueCount14: TEdit;
    edtSameValueCount16: TEdit;
    edtSameValueCount17: TEdit;
    edtSameValueCount19: TEdit;
    edtSameValueCount18: TEdit;
    edtSameValueCount20: TEdit;
    edtSameValueCount21: TEdit;
    edtSameValueCount23: TEdit;
    edtSameValueCount22: TEdit;
    edtSameValueCount24: TEdit;
    Label16: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    edtMinSeparateSameValueCount: TEdit;
    Label22: TLabel;
    Label23: TLabel;
    edtSeparateSameValueCount3: TEdit;
    edtSeparateSameValueCount4: TEdit;
    Label24: TLabel;
    Label25: TLabel;
    edtSeparateSameValueCount5: TEdit;
    edtSeparateSameValueCount6: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure edtFileDirectoryClick(Sender: TObject);
    procedure btnCompareClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    function CheckIntervalValues(var IntervalValues: TWordDynArray): Boolean;
    function CheckSeparateSameValueCount(var ValueCount, ValueCount2: Integer): Boolean;
    function CheckSameValueCount(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckSameValueCount2(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckSameValueCount3(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckSameValueCount4(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckSameValueCount5(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckSameValueCount6(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
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

function TfrmMain.CheckSeparateSameValueCount(var ValueCount, ValueCount2: Integer): Boolean;
begin
  Result := edtSeparateSameValueCount.TryToValue(ValueCount, -1);
  if not Result then Exit;
  Result := edtSeparateSameValueCount2.TryToValue(ValueCount2);
  if not Result then Exit;
  if ValueCount > -1 then
    Result := (ValueCount2 >= 0) and (ValueCount2 >= 0);
end;

function TfrmMain.CheckSameValueCount(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
begin
  Result := edtSameValueCount.TryToValue(ValueCount, -1);
  if not Result then Exit;
  Result := edtSameValueCount2.TryToValue(ValueCount2);
  if not Result then Exit;
  Result := edtSameValueCount3.TryToValue(ValueCount3);
  if not Result then Exit;
  Result := edtSameValueCount4.TryToValue(ValueCount4);
  if not Result then Exit;
  if ValueCount > -1 then
  begin
    Result := (ValueCount2 >= 0) and (ValueCount2 >= ValueCount)
      and (ValueCount4 >= 0) and (ValueCount4 >= ValueCount3);
  end;
end;

function TfrmMain.CheckSameValueCount2(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
begin
  Result := edtSameValueCount5.TryToValue(ValueCount, -1);
  if not Result then Exit;
  Result := edtSameValueCount6.TryToValue(ValueCount2);
  if not Result then Exit;
  Result := edtSameValueCount7.TryToValue(ValueCount3);
  if not Result then Exit;
  Result := edtSameValueCount8.TryToValue(ValueCount4);
  if not Result then Exit;
  if ValueCount > -1 then
  begin
    Result := (ValueCount2 >= 0) and (ValueCount2 >= ValueCount)
      and (ValueCount4 >= 0) and (ValueCount4 >= ValueCount3);
  end;
end;

function TfrmMain.CheckSameValueCount3(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
begin
  Result := edtSameValueCount9.TryToValue(ValueCount, -1);
  if not Result then Exit;
  Result := edtSameValueCount10.TryToValue(ValueCount2);
  if not Result then Exit;
  Result := edtSameValueCount11.TryToValue(ValueCount3);
  if not Result then Exit;
  Result := edtSameValueCount12.TryToValue(ValueCount4);
  if not Result then Exit;
  if ValueCount > -1 then
  begin
    Result := (ValueCount2 >= 0) and (ValueCount2 >= ValueCount)
      and (ValueCount4 >= 0) and (ValueCount4 >= ValueCount3);
  end;
end;

function TfrmMain.CheckSameValueCount4(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
begin
  Result := edtSameValueCount13.TryToValue(ValueCount, -1);
  if not Result then Exit;
  Result := edtSameValueCount14.TryToValue(ValueCount2);
  if not Result then Exit;
  Result := edtSameValueCount15.TryToValue(ValueCount3);
  if not Result then Exit;
  Result := edtSameValueCount16.TryToValue(ValueCount4);
  if not Result then Exit;
  if ValueCount > -1 then
  begin
    Result := (ValueCount2 >= 0) and (ValueCount2 >= ValueCount)
      and (ValueCount4 >= 0) and (ValueCount4 >= ValueCount3);
  end;
end;

function TfrmMain.CheckSameValueCount5(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
begin
  Result := edtSameValueCount17.TryToValue(ValueCount, -1);
  if not Result then Exit;
  Result := edtSameValueCount18.TryToValue(ValueCount2);
  if not Result then Exit;
  Result := edtSameValueCount19.TryToValue(ValueCount3);
  if not Result then Exit;
  Result := edtSameValueCount20.TryToValue(ValueCount4);
  if not Result then Exit;
  if ValueCount > -1 then
  begin
    Result := (ValueCount2 >= 0) and (ValueCount2 >= ValueCount)
      and (ValueCount4 >= 0) and (ValueCount4 >= ValueCount3);
  end;
end;

function TfrmMain.CheckSameValueCount6(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
begin
  Result := edtSameValueCount21.TryToValue(ValueCount, -1);
  if not Result then Exit;
  Result := edtSameValueCount22.TryToValue(ValueCount2);
  if not Result then Exit;
  Result := edtSameValueCount23.TryToValue(ValueCount3);
  if not Result then Exit;
  Result := edtSameValueCount24.TryToValue(ValueCount4);
  if not Result then Exit;
  if ValueCount > -1 then
  begin
    Result := (ValueCount2 >= 0) and (ValueCount2 >= ValueCount)
      and (ValueCount4 >= 0) and (ValueCount4 >= ValueCount3);
  end;
end;

procedure TfrmMain.OnStateChange(Working: Boolean);
begin
  btnCompare.Enabled := not Working;
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
  if not SelectDirectory('', '', FileDirectory) then Exit;

  TEdit(Sender).Text := FileDirectory;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  v: Variant;
begin
  ScrollBox1.VertScrollBar.Position := 0;
  PageControl1.ActivePageIndex := 0;
  OnStateChange(False);

  fKeyValue.GetKeyValue(edtFileDirectory.Name, v);
  if not VarIsEmpty(v) then edtFileDirectory.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtFileDirectory2.Name, v);
  if not VarIsEmpty(v) then edtFileDirectory2.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtIntervalValue.Name, v);
  if not VarIsEmpty(v) then edtIntervalValue.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtIntervalValue2.Name, v);
  if not VarIsEmpty(v) then edtIntervalValue2.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSeparateSameValueCount.Name, v);
  if not VarIsEmpty(v) then edtSeparateSameValueCount.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSeparateSameValueCount2.Name, v);
  if not VarIsEmpty(v) then edtSeparateSameValueCount2.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSeparateSameValueCount3.Name, v);
  if not VarIsEmpty(v) then edtSeparateSameValueCount3.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSeparateSameValueCount4.Name, v);
  if not VarIsEmpty(v) then edtSeparateSameValueCount4.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSeparateSameValueCount5.Name, v);
  if not VarIsEmpty(v) then edtSeparateSameValueCount5.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSeparateSameValueCount6.Name, v);
  if not VarIsEmpty(v) then edtSeparateSameValueCount6.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtMinSeparateSameValueCount.Name, v);
  if not VarIsEmpty(v) then edtMinSeparateSameValueCount.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSameValueCount.Name, v);
  if not VarIsEmpty(v) then edtSameValueCount.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSameValueCount2.Name, v);
  if not VarIsEmpty(v) then edtSameValueCount2.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSameValueCount3.Name, v);
  if not VarIsEmpty(v) then edtSameValueCount3.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSameValueCount4.Name, v);
  if not VarIsEmpty(v) then edtSameValueCount4.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSameValueCount5.Name, v);
  if not VarIsEmpty(v) then edtSameValueCount5.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSameValueCount6.Name, v);
  if not VarIsEmpty(v) then edtSameValueCount6.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSameValueCount7.Name, v);
  if not VarIsEmpty(v) then edtSameValueCount7.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSameValueCount8.Name, v);
  if not VarIsEmpty(v) then edtSameValueCount8.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSameValueCount9.Name, v);
  if not VarIsEmpty(v) then edtSameValueCount9.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSameValueCount10.Name, v);
  if not VarIsEmpty(v) then edtSameValueCount10.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSameValueCount11.Name, v);
  if not VarIsEmpty(v) then edtSameValueCount11.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSameValueCount12.Name, v);
  if not VarIsEmpty(v) then edtSameValueCount12.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSameValueCount13.Name, v);
  if not VarIsEmpty(v) then edtSameValueCount13.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSameValueCount14.Name, v);
  if not VarIsEmpty(v) then edtSameValueCount14.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSameValueCount15.Name, v);
  if not VarIsEmpty(v) then edtSameValueCount15.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSameValueCount16.Name, v);
  if not VarIsEmpty(v) then edtSameValueCount16.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSameValueCount17.Name, v);
  if not VarIsEmpty(v) then edtSameValueCount17.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSameValueCount18.Name, v);
  if not VarIsEmpty(v) then edtSameValueCount18.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSameValueCount19.Name, v);
  if not VarIsEmpty(v) then edtSameValueCount19.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSameValueCount20.Name, v);
  if not VarIsEmpty(v) then edtSameValueCount20.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSameValueCount21.Name, v);
  if not VarIsEmpty(v) then edtSameValueCount21.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSameValueCount22.Name, v);
  if not VarIsEmpty(v) then edtSameValueCount22.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSameValueCount23.Name, v);
  if not VarIsEmpty(v) then edtSameValueCount23.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtSameValueCount24.Name, v);
  if not VarIsEmpty(v) then edtSameValueCount24.Text := VarToStr(v);

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

procedure TfrmMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position - WheelDelta div 2;
end;

procedure TfrmMain.btnCompareClick(Sender: TObject);
var
  IntervalValues: TWordDynArray;
  SameValueCount, SameValueCount2, SameValueCount3, SameValueCount4, MinSameValueCount: Integer;
  SameValues: TArray<TRect>;
begin
  if not CheckIntervalValues(IntervalValues) then
    raise Exception.Create('请输入有效区域');

  SetLength(SameValues, 0);
  if Sender = btnCompare then
  begin
    if not edtSeparateSameValueCount.TryToValue(SameValueCount, -1) then
      raise Exception.Create('请输入有效相同列数');
    if not edtSeparateSameValueCount2.TryToValue(SameValueCount2) then
      raise Exception.Create('请输入有效相同列数');
    if SameValueCount > -1 then
    begin
      SetLength(SameValues, Length(SameValues) + 1);
      SameValues[High(SameValues)] := Rect(SameValueCount, SameValueCount2, 0, 0);
    end;

    if not edtSeparateSameValueCount3.TryToValue(SameValueCount, -1) then
      raise Exception.Create('请输入有效相同列数');
    if not edtSeparateSameValueCount4.TryToValue(SameValueCount2) then
      raise Exception.Create('请输入有效相同列数');
    if SameValueCount > -1 then
    begin
      SetLength(SameValues, Length(SameValues) + 1);
      SameValues[High(SameValues)] := Rect(SameValueCount, SameValueCount2, 0, 0);
    end;

    if not edtSeparateSameValueCount5.TryToValue(SameValueCount, -1) then
      raise Exception.Create('请输入有效相同列数');
    if not edtSeparateSameValueCount6.TryToValue(SameValueCount2) then
      raise Exception.Create('请输入有效相同列数');
    if SameValueCount > -1 then
    begin
      SetLength(SameValues, Length(SameValues) + 1);
      SameValues[High(SameValues)] := Rect(SameValueCount, SameValueCount2, 0, 0);
    end;
  end
  else
  begin
    if not CheckSameValueCount(SameValueCount, SameValueCount2, SameValueCount3, SameValueCount4) then
      raise Exception.Create('请输入有效相同列数范围');
    if SameValueCount > -1 then
    begin
      SetLength(SameValues, Length(SameValues) + 1);
      SameValues[High(SameValues)] := Rect(SameValueCount, SameValueCount2, SameValueCount3, SameValueCount4);
    end;
    if not CheckSameValueCount2(SameValueCount, SameValueCount2, SameValueCount3, SameValueCount4) then
      raise Exception.Create('请输入有效相同列数范围');
    if SameValueCount > -1 then
    begin
      SetLength(SameValues, Length(SameValues) + 1);
      SameValues[High(SameValues)] := Rect(SameValueCount, SameValueCount2, SameValueCount3, SameValueCount4);
    end;
    if not CheckSameValueCount3(SameValueCount, SameValueCount2, SameValueCount3, SameValueCount4) then
      raise Exception.Create('请输入有效相同列数范围');
    if SameValueCount > -1 then
    begin
      SetLength(SameValues, Length(SameValues) + 1);
      SameValues[High(SameValues)] := Rect(SameValueCount, SameValueCount2, SameValueCount3, SameValueCount4);
    end;
    if not CheckSameValueCount4(SameValueCount, SameValueCount2, SameValueCount3, SameValueCount4) then
      raise Exception.Create('请输入有效相同列数范围');
    if SameValueCount > -1 then
    begin
      SetLength(SameValues, Length(SameValues) + 1);
      SameValues[High(SameValues)] := Rect(SameValueCount, SameValueCount2, SameValueCount3, SameValueCount4);
    end;
    if not CheckSameValueCount5(SameValueCount, SameValueCount2, SameValueCount3, SameValueCount4) then
      raise Exception.Create('请输入有效相同列数范围');
    if SameValueCount > -1 then
    begin
      SetLength(SameValues, Length(SameValues) + 1);
      SameValues[High(SameValues)] := Rect(SameValueCount, SameValueCount2, SameValueCount3, SameValueCount4);
    end;
    if not CheckSameValueCount6(SameValueCount, SameValueCount2, SameValueCount3, SameValueCount4) then
      raise Exception.Create('请输入有效相同列数范围');
    if SameValueCount > -1 then
    begin
      SetLength(SameValues, Length(SameValues) + 1);
      SameValues[High(SameValues)] := Rect(SameValueCount, SameValueCount2, SameValueCount3, SameValueCount4);
    end;
    if Length(SameValues) = 0 then
      raise Exception.Create('请设置相同列数范围');
  end;

  fKeyValue.SetKeyValue(edtFileDirectory.Name, edtFileDirectory.Text);
  fKeyValue.SetKeyValue(edtFileDirectory2.Name, edtFileDirectory2.Text);
  fKeyValue.SetKeyValue(edtIntervalValue.Name, edtIntervalValue.Text);
  fKeyValue.SetKeyValue(edtIntervalValue2.Name, edtIntervalValue2.Text);
  fKeyValue.SetKeyValue(edtSeparateSameValueCount.Name, edtSeparateSameValueCount.Text);
  fKeyValue.SetKeyValue(edtSeparateSameValueCount2.Name, edtSeparateSameValueCount2.Text);
  fKeyValue.SetKeyValue(edtSeparateSameValueCount3.Name, edtSeparateSameValueCount3.Text);
  fKeyValue.SetKeyValue(edtSeparateSameValueCount4.Name, edtSeparateSameValueCount4.Text);
  fKeyValue.SetKeyValue(edtSeparateSameValueCount5.Name, edtSeparateSameValueCount5.Text);
  fKeyValue.SetKeyValue(edtSeparateSameValueCount6.Name, edtSeparateSameValueCount6.Text);
  fKeyValue.SetKeyValue(edtMinSeparateSameValueCount.Name, edtMinSeparateSameValueCount.Text);
  fKeyValue.SetKeyValue(edtSameValueCount.Name, edtSameValueCount.Text);
  fKeyValue.SetKeyValue(edtSameValueCount2.Name, edtSameValueCount2.Text);
  fKeyValue.SetKeyValue(edtSameValueCount3.Name, edtSameValueCount3.Text);
  fKeyValue.SetKeyValue(edtSameValueCount4.Name, edtSameValueCount4.Text);
  fKeyValue.SetKeyValue(edtSameValueCount5.Name, edtSameValueCount5.Text);
  fKeyValue.SetKeyValue(edtSameValueCount6.Name, edtSameValueCount6.Text);
  fKeyValue.SetKeyValue(edtSameValueCount7.Name, edtSameValueCount7.Text);
  fKeyValue.SetKeyValue(edtSameValueCount8.Name, edtSameValueCount8.Text);
  fKeyValue.SetKeyValue(edtSameValueCount9.Name, edtSameValueCount9.Text);
  fKeyValue.SetKeyValue(edtSameValueCount10.Name, edtSameValueCount10.Text);
  fKeyValue.SetKeyValue(edtSameValueCount11.Name, edtSameValueCount11.Text);
  fKeyValue.SetKeyValue(edtSameValueCount12.Name, edtSameValueCount12.Text);
  fKeyValue.SetKeyValue(edtSameValueCount13.Name, edtSameValueCount13.Text);
  fKeyValue.SetKeyValue(edtSameValueCount14.Name, edtSameValueCount14.Text);
  fKeyValue.SetKeyValue(edtSameValueCount15.Name, edtSameValueCount15.Text);
  fKeyValue.SetKeyValue(edtSameValueCount16.Name, edtSameValueCount16.Text);
  fKeyValue.SetKeyValue(edtSameValueCount17.Name, edtSameValueCount17.Text);
  fKeyValue.SetKeyValue(edtSameValueCount18.Name, edtSameValueCount18.Text);
  fKeyValue.SetKeyValue(edtSameValueCount19.Name, edtSameValueCount19.Text);
  fKeyValue.SetKeyValue(edtSameValueCount20.Name, edtSameValueCount20.Text);
  fKeyValue.SetKeyValue(edtSameValueCount21.Name, edtSameValueCount21.Text);
  fKeyValue.SetKeyValue(edtSameValueCount22.Name, edtSameValueCount22.Text);
  fKeyValue.SetKeyValue(edtSameValueCount23.Name, edtSameValueCount23.Text);
  fKeyValue.SetKeyValue(edtSameValueCount24.Name, edtSameValueCount24.Text);

  OnStateChange(True);
  Timer.Enabled := True;

  fDataComputer := TDataComputer.Create;
  fDataComputer.OnTerminate := OnTerminate;
  fDataComputer.IntervalValues := IntervalValues;
  fDataComputer.SourceFileDirectory := edtFileDirectory.Text;
  fDataComputer.CompareFileDirectory := edtFileDirectory2.Text;
  fDataComputer.ExportDirectory := TPath.GetDirectoryName(fDataComputer.CompareFileDirectory);
  fDataComputer.SameValues := SameValues;
  fDataComputer.MinSameValueCount := MinSameValueCount;
  fDataComputer.MergeMode := Sender = btnCompare2;
  fDataComputer.SeparateMode := Sender = btnCompare;
  fDataComputer.Start;
end;

end.
