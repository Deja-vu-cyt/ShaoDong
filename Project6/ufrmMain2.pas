unit ufrmMain2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.Types, System.IOUtils, System.Diagnostics,
  Vcl.Controls, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.FileCtrl,
  Vcl.CheckLst, Vcl.ComCtrls, Vcl.Menus, Vcl.Forms;

type
  TfrmMain = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    OpenDialog: TOpenDialog;
    Panel3: TPanel;
    Label1: TLabel;
    lblUseTime: TLabel;
    edtFileName: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    edtIntervalValue: TEdit;
    edtIntervalValue2: TEdit;
    Label7: TLabel;
    edtVertSameValueCount2: TEdit;
    Label9: TLabel;
    edtVertSameValueCount: TEdit;
    Label2: TLabel;
    edtCompareSpacing: TEdit;
    edtVertCompareSpacing: TEdit;
    Label8: TLabel;
    edtVertGroupCount: TEdit;
    TabSheet3: TTabSheet;
    Label10: TLabel;
    Label12: TLabel;
    edtVVertSameValueCount2: TEdit;
    edtVVertSameValueCount: TEdit;
    edtVVertCompareSpacing: TEdit;
    Label14: TLabel;
    Label15: TLabel;
    edtSlantSameValueCount2: TEdit;
    edtSlantSameValueCount: TEdit;
    edtSlantCompareSpacing: TEdit;
    Label18: TLabel;
    edtVertSlantGroupCount: TEdit;
    chkCompareCrossRange: TCheckBox;
    MainMenu: TMainMenu;
    N1: TMenuItem;
    miConsumer: TMenuItem;
    Timer: TTimer;
    miCombinationCalculator: TMenuItem;
    Label5: TLabel;
    edtSlantGroupCount: TEdit;
    btnVertSlantCompare: TButton;
    btnVertCompare: TButton;
    btnSlantCompare: TButton;
    Label16: TLabel;
    edtVertSlantKeepCodeNameValueCount: TEdit;
    Label17: TLabel;
    edtVertKeepCodeNameValueCount: TEdit;
    Label19: TLabel;
    edtSlantKeepCodeNameValueCount: TEdit;
    Label25: TLabel;
    btnSlantCompare2: TButton;
    btnVertCompare2: TButton;
    btnVertSlantCompare2: TButton;
    btnVertSlantGroupCodeNameSettings: TButton;
    btnVertSlantGroupCodeNameSettings2: TButton;
    btnVertGroupCodeNameSettings: TButton;
    btnVertGroupCodeNameSettings2: TButton;
    btnSlantGroupCodeNameSettings: TButton;
    btnSlantGroupCodeNameSettings2: TButton;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    edtAddress: TEdit;
    edtPort: TEdit;
    btnOk: TButton;
    btnVertSlantExportFileSettings: TButton;
    btnVertExportFileSettings: TButton;
    btnSlantExportFileSettings: TButton;
    procedure FormCreate(Sender: TObject);
    procedure edtFileNameClick(Sender: TObject);
    procedure btnCompareClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure btnVertGroupCodeNameSettingsClick(Sender: TObject);
    procedure btnVertSlantGroupCodeNameSettingsClick(Sender: TObject);
    procedure btnVertSlantGroupCodeNameSettings2Click(Sender: TObject);
    procedure btnVertGroupCodeNameSettings2Click(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnVertSlantExportFileSettingsClick(Sender: TObject);
    procedure btnVertExportFileSettingsClick(Sender: TObject);
    procedure btnSlantExportFileSettingsClick(Sender: TObject);
  private
    procedure OnStateChange(Working: Boolean);
    procedure OnFinish(Sender: TObject);
    procedure OnError(Sender: TObject);
  public

  end;

var
  frmMain: TfrmMain;

implementation

uses
  uDataComputer, ufrmGroupCodeNameSettings, ufrmGroupCodeNameSettings2,
  ufrmExportFileSettings;

{$R *.dfm}

procedure TfrmMain.OnStateChange(Working: Boolean);
begin
  btnSlantCompare.Enabled := not Working and (fSettings.CompareMode in [cmNone, cmSlant]);
  btnVertCompare.Enabled := not Working and (fSettings.CompareMode in [cmNone, cmVert]);
  btnVertSlantCompare.Enabled := not Working and (fSettings.CompareMode in [cmNone, cmVertSlant]);
  btnSlantCompare2.Enabled := not Working and (fSettings.CompareMode in [cmNone, cmSlant]);
  btnVertCompare2.Enabled := not Working and (fSettings.CompareMode in [cmNone, cmVert]);
  btnVertSlantCompare2.Enabled := not Working and (fSettings.CompareMode in [cmNone, cmVertSlant]);
  btnVertGroupCodeNameSettings.Enabled := fSettings.CompareMode in [cmNone, cmVert];
  btnSlantGroupCodeNameSettings.Enabled := fSettings.CompareMode in [cmNone, cmSlant];
  btnVertSlantGroupCodeNameSettings.Enabled := fSettings.CompareMode in [cmNone, cmVertSlant];
  btnVertGroupCodeNameSettings2.Enabled := fSettings.CompareMode in [cmNone, cmVert];
  btnSlantGroupCodeNameSettings2.Enabled := fSettings.CompareMode in [cmNone, cmSlant];
  btnVertSlantGroupCodeNameSettings2.Enabled := fSettings.CompareMode in [cmNone, cmVertSlant];
  btnVertExportFileSettings.Enabled := fSettings.CompareMode in [cmNone, cmVert];
  btnSlantExportFileSettings.Enabled := fSettings.CompareMode in [cmNone, cmSlant];
  btnVertSlantExportFileSettings.Enabled := fSettings.CompareMode in [cmNone, cmVertSlant];
  if Assigned(frmGroupCodeNameSettings) then
    frmGroupCodeNameSettings.btnOk.Enabled := not Working;
  if Assigned(frmGroupCodeNameSettings2) then
    frmGroupCodeNameSettings2.btnOk.Enabled := not Working;
  if Assigned(frmExportFileSettings) then
    frmExportFileSettings.btnOk.Enabled := not Working;
end;

procedure TfrmMain.OnFinish(Sender: TObject);
begin
  TThread.Queue(nil, procedure
  begin
    Caption := '';
    Timer.Enabled := False;
    OnStateChange(False);
    ShowMessage('查询完毕');
  end);
end;

procedure TfrmMain.OnError(Sender: TObject);
begin
  TThread.Queue(nil, procedure
  begin
    Caption := '';
    Timer.Enabled := False;
    OnStateChange(False);
    ShowMessage(TDataComputer(Sender).ErrorMessage);
  end);
end;

procedure TfrmMain.TimerTimer(Sender: TObject);
var
  TS: TTimeStamp;
  Days: Byte;
  Seconds: Int64;
begin
  if fMainApp then
  begin
    Seconds := fDataComputer.Stopwatch.ElapsedMilliseconds;
    TS.Date := DateDelta;
    TS.Time := Seconds mod 86400000;
    Days := Seconds div 86400000;
    lblUseTime.Caption := Format('处理所需时间：%d日', [Days]) + FormatDateTime('H小时M分S秒', TimeStampToDateTime(TS));
  end;

  Caption := '';
  case fGroupCodeName.Task of
    ttSyncCodeName: Caption := '正在同步数据';
    ttGroupCodeName:
    begin
      Caption := Format('1. 已处理 :  ①. 第 %d 次（ 遍历 ）产生（ 第 1 - %d 行 ）； 2. 正处理 :  ①. 第 %d 次（ 遍历 ）； ②.（ 第 %d 行 ）遍历 [ 该行（ 已组合 ）的（ 第 %d 行 ）]', [
        fGroupCodeName.BatchNumber - 1,
        fGroupCodeName.BatchNumberRowCount,
        fGroupCodeName.BatchNumber,
        fGroupCodeName.FirstNumber,
        fGroupCodeName.GroupCount
      ]);
    end;
    ttUploadCodeName: Caption := '正在上传数据';
  end;
end;

procedure TfrmMain.btnOkClick(Sender: TObject);
var
  Address, Port: string;
  i: Integer;
begin
  Address := Trim(edtAddress.Text);
  if Address.IsEmpty then raise Exception.Create('请输入有效地址');
  Port := Trim(edtPort .Text);
  if not (not Port.IsEmpty and TryStrToInt(Port, i) and (i > 0)) then
    raise Exception.Create('请输入有效端口');

  ConnectServer(Address, Port );
  ShowMessage('连接成功');
  OnStateChange(True);
  Timer.Enabled := True;
end;

procedure TfrmMain.btnSlantExportFileSettingsClick(Sender: TObject);
begin
  frmExportFileSettings.CompareMode := Ord(cmSlant);
  frmExportFileSettings.ShowModal;
end;

procedure TfrmMain.btnVertExportFileSettingsClick(Sender: TObject);
begin
  frmExportFileSettings.CompareMode := Ord(cmVert);
  frmExportFileSettings.ShowModal;
end;

procedure TfrmMain.btnVertGroupCodeNameSettings2Click(Sender: TObject);
begin
  frmGroupCodeNameSettings2.Number := 5;
  frmGroupCodeNameSettings2.ShowModal;
end;

procedure TfrmMain.btnVertGroupCodeNameSettingsClick(Sender: TObject);
begin
  frmGroupCodeNameSettings.Number := 5;
  frmGroupCodeNameSettings.ShowModal;
end;

procedure TfrmMain.btnVertSlantExportFileSettingsClick(Sender: TObject);
begin
  frmExportFileSettings.CompareMode := Ord(cmVertSlant);
  frmExportFileSettings.ShowModal;
end;

procedure TfrmMain.btnVertSlantGroupCodeNameSettings2Click(Sender: TObject);
begin
  frmGroupCodeNameSettings2.Number := 6;
  frmGroupCodeNameSettings2.ShowModal;
end;

procedure TfrmMain.btnVertSlantGroupCodeNameSettingsClick(Sender: TObject);
begin
  frmGroupCodeNameSettings.Number := 6;
  frmGroupCodeNameSettings.ShowModal;
end;

procedure TfrmMain.edtFileNameClick(Sender: TObject);
begin
  if not OpenDialog.Execute then Exit;
  TEdit(Sender).Text := OpenDialog.FileName;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  v: Variant;
begin
  PageControl1.ActivePageIndex := 0;

  with fSettings do
  begin
    ExportLite := True;
    if Length(IntervalValues) > 0 then
    begin
      edtIntervalValue.Text := IntervalValues[0].ToString;
      edtIntervalValue2.Text := '0';
      if Length(IntervalValues) > 1 then
        edtIntervalValue2.Text := IntervalValues[1].ToString;
    end;
    chkCompareCrossRange.Checked := CompareCrossRange;

    case CompareMode of
      cmVert:
      begin
        PageControl1.ActivePageIndex := 1;
        edtVertCompareSpacing.Text := VertCompareSpacing.ToString;
        edtVertSameValueCount.Text := VertSameValueCount.ToString;
        edtVertSameValueCount2.Text := VertSameValueCount2.ToString;
        edtVertGroupCount.Text := GroupCount.ToString;
        edtVertKeepCodeNameValueCount.Text := KeepCodeNameValueCount.ToString;
      end;
      cmSlant:
      begin
        PageControl1.ActivePageIndex := 2;
        edtCompareSpacing.Text := SlantCompareSpacing.ToString;
        edtSlantGroupCount.Text := GroupCount.ToString;
        edtSlantKeepCodeNameValueCount.Text := KeepCodeNameValueCount.ToString;
      end;
      cmVertSlant:
      begin
        edtVVertCompareSpacing.Text := VertCompareSpacing.ToString;
        edtVVertSameValueCount.Text := VertSameValueCount.ToString;
        edtVVertSameValueCount2.Text := VertSameValueCount2.ToString;
        edtSlantCompareSpacing.Text := SlantCompareSpacing.ToString;
        edtSlantSameValueCount.Text := SlantSameValueCount.ToString;
        edtSlantSameValueCount2.Text := SlantSameValueCount2.ToString;
        edtVertSlantGroupCount.Text := GroupCount.ToString;
        edtVertSlantKeepCodeNameValueCount.Text := KeepCodeNameValueCount.ToString;
      end;
    end;
    edtIntervalValue.ReadOnly := CompareMode > cmNone;
    edtIntervalValue2.ReadOnly := CompareMode > cmNone;
    edtVertCompareSpacing.ReadOnly := CompareMode > cmNone;
    edtVertSameValueCount.ReadOnly := CompareMode > cmNone;
    edtVertSameValueCount2.ReadOnly := CompareMode > cmNone;
    edtCompareSpacing.ReadOnly := CompareMode > cmNone;
    edtVVertCompareSpacing.ReadOnly := CompareMode > cmNone;
    edtVVertSameValueCount.ReadOnly := CompareMode > cmNone;
    edtVVertSameValueCount2.ReadOnly := CompareMode > cmNone;
    edtSlantCompareSpacing.ReadOnly := CompareMode > cmNone;
    edtSlantSameValueCount.ReadOnly := CompareMode > cmNone;
    edtSlantSameValueCount2.ReadOnly := CompareMode > cmNone;
  end;

  OnStateChange(False);
  fDataComputer.OnFinish := OnFinish;
  fDataComputer.OnError := OnError;
end;

procedure TfrmMain.btnCompareClick(Sender: TObject);
var
  v, v2: Integer;
  IntervalValues: TWordDynArray;
  CompareMode: TCompareMode;
begin
  fSettings.FileName := edtFileName.Text;
  if fSettings.CompareMode = cmNone then
  begin
    if (Sender = btnSlantCompare) or (Sender = btnSlantCompare2) then
      CompareMode := cmSlant
    else if (Sender = btnVertCompare) or (Sender = btnVertCompare2) then
      CompareMode := cmVert
    else
      CompareMode := cmVertSlant;

    if not (TryStrToInt(edtIntervalValue.Text, v) and (v >= 1)) then
    begin
      edtIntervalValue.SetFocus;
      edtIntervalValue.SelectAll;
      raise Exception.Create('请输入有效值');
    end;
    if not (TryStrToInt(edtIntervalValue2.Text, v2) and (v2 >= 0)) then
    begin
      edtIntervalValue2.SetFocus;
      edtIntervalValue2.SelectAll;
      raise Exception.Create('请输入有效值');
    end;
    if v + v2 > 256 then raise Exception.Create('总列数不能超过256');

    SetLength(IntervalValues, 1);
    IntervalValues[0] := v;
    if v2 > 0 then
    begin
      SetLength(IntervalValues, 2);
      IntervalValues[1] := v2;
    end;
    fSettings.IntervalValues := IntervalValues;
    fSettings.CompareCrossRange := chkCompareCrossRange.Checked;

    case CompareMode of
      cmVert:
      begin
        if not TryStrToInt(edtVertCompareSpacing.Text, v) then
          raise Exception.Create('请输入有效比较次数');
        fSettings.VertCompareSpacing := v;
        if not TryStrToInt(edtVertSameValueCount.Text, v) then
          raise Exception.Create('请输入有效相同列数');
        fSettings.VertSameValueCount := v;
        if not TryStrToInt(edtVertSameValueCount2.Text, v) then
          raise Exception.Create('请输入有效相同列数2');
        fSettings.VertSameValueCount2 := v;
        fSettings.SlantCompareSpacing := 0;
        fSettings.SlantSameValueCount := 0;
        fSettings.SlantSameValueCount2 := 0;
      end;
      cmSlant:
      begin
        if not TryStrToInt(edtCompareSpacing.Text, v) then
          raise Exception.Create('请输入有效比较次数');
        fSettings.SlantCompareSpacing := v;
        fSettings.VertCompareSpacing := 0;
        fSettings.VertSameValueCount := 0;
        fSettings.VertSameValueCount2 := 0;
        fSettings.SlantSameValueCount := 0;
        fSettings.SlantSameValueCount2 := 0;
      end;
      cmVertSlant:
      begin
        if not TryStrToInt(edtVVertCompareSpacing.Text, v) then
          raise Exception.Create('请输入有效直连比较次数');
        fSettings.VertCompareSpacing := v;
        if not TryStrToInt(edtVVertSameValueCount.Text, v) then
          raise Exception.Create('请输入有效相同列数');
        fSettings.VertSameValueCount := v;
        if not TryStrToInt(edtVVertSameValueCount2.Text, v) then
          raise Exception.Create('请输入有效相同列数2');
        fSettings.VertSameValueCount2 := v;
        if not TryStrToInt(edtSlantCompareSpacing.Text, v) then
          raise Exception.Create('请输入有效直连比较次数');
        fSettings.SlantCompareSpacing := v;
        if not TryStrToInt(edtSlantSameValueCount.Text, v) then
          raise Exception.Create('请输入有效相同列数');
        fSettings.SlantSameValueCount := v;
        if not TryStrToInt(edtSlantSameValueCount2.Text, v) then
          raise Exception.Create('请输入有效相同列数2');
        fSettings.SlantSameValueCount2 := v;
      end;
    end;
    if fSettings.ExportCodeNameValueCount > fSettings.ExportCodeNameValueCount2 then
      raise Exception.Create('请输入有效导出代号个数');

    fSettings.CompareMode := CompareMode;
    //WriteSettings;
  end;
  case fSettings.CompareMode of
    cmVert:
    begin
      if not TryStrToInt(edtVertGroupCount.Text, v) then
        raise Exception.Create('请输入有效遍历次数');
      fSettings.GroupCount := v;
      if not TryStrToInt(edtVertKeepCodeNameValueCount.Text, v) then
        raise Exception.Create('请输入有效最小代号个数');
      fSettings.KeepCodeNameValueCount := v;
    end;
    cmSlant:
    begin
      if not TryStrToInt(edtSlantGroupCount.Text, v) then
        raise Exception.Create('请输入有效遍历次数');
      fSettings.GroupCount := v;
      if not TryStrToInt(edtSlantKeepCodeNameValueCount.Text, v) then
        raise Exception.Create('请输入有效最小代号个数');
      fSettings.KeepCodeNameValueCount := v;
    end;
    cmVertSlant:
    begin
      if not TryStrToInt(edtVertSlantGroupCount.Text, v) then
        raise Exception.Create('请输入有效遍历次数');
      fSettings.GroupCount := v;
      if not TryStrToInt(edtVertSlantKeepCodeNameValueCount.Text, v) then
        raise Exception.Create('请输入有效最小代号个数');
      fSettings.KeepCodeNameValueCount := v;
    end;
  end;
  WriteSettings;
  if (Sender = btnVertCompare2) or (Sender = btnSlantCompare2) or (Sender = btnVertSlantCompare2) then
  begin
    fSettings.ExportFile := True;
    fSettings.ExportFile2 := False;
    fSettings.ExportFile3 := False;
    fSettings.ExportFile4 := False;
  end;

  OnStateChange(True);
  System.TMonitor.PulseAll(fDataComputer.Lock);
  Timer.OnTimer(Timer);
  Timer.Enabled := True;
end;

end.
