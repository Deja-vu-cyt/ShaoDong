unit ufrmMain3;

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
    Label6: TLabel;
    edtVertSlantExportCodeNameValueCount: TEdit;
    edtVertSlantExportCodeNameValueCount2: TEdit;
    btnVertSlantCompare: TButton;
    Label11: TLabel;
    edtVertExportCodeNameValueCount: TEdit;
    edtVertExportCodeNameValueCount2: TEdit;
    btnVertCompare: TButton;
    Label13: TLabel;
    edtSlantExportCodeNameValueCount: TEdit;
    edtSlantExportCodeNameValueCount2: TEdit;
    btnSlantCompare: TButton;
    chkVertSlantExportSource: TCheckBox;
    chkVertExportSource: TCheckBox;
    chkSlantExportSource: TCheckBox;
    Label16: TLabel;
    edtVertSlantKeepCodeNameValueCount: TEdit;
    Label17: TLabel;
    edtVertKeepCodeNameValueCount: TEdit;
    Label19: TLabel;
    edtSlantKeepCodeNameValueCount: TEdit;
    Label20: TLabel;
    Label21: TLabel;
    btnVertSlantCompare2: TButton;
    btnVertCompare2: TButton;
    btnSlantCompare2: TButton;
    btnVertSlantGroupCodeNameSettings: TButton;
    btnVertSlantGroupCodeNameSettings2: TButton;
    btnVertGroupCodeNameSettings: TButton;
    btnVertGroupCodeNameSettings2: TButton;
    btnSlantGroupCodeNameSettings: TButton;
    btnSlantGroupCodeNameSettings2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure edtFileNameClick(Sender: TObject);
    procedure btnCompareClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure btnVertSlantGroupCodeNameSettingsClick(Sender: TObject);
    procedure btnVertGroupCodeNameSettingsClick(Sender: TObject);
    procedure btnVertSlantGroupCodeNameSettings2Click(Sender: TObject);
    procedure btnVertGroupCodeNameSettings2Click(Sender: TObject);
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
  uDataComputer, ufrmGroupCodeNameSettings, ufrmGroupCodeNameSettings2;

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
  if Assigned(frmGroupCodeNameSettings) then
    frmGroupCodeNameSettings.btnOk.Enabled := not Working;
  if Assigned(frmGroupCodeNameSettings2) then
    frmGroupCodeNameSettings2.btnOk.Enabled := not Working;
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
begin
  TS.Date := DateDelta;
  TS.Time := fDataComputer.Stopwatch.ElapsedMilliseconds;
  Days := TS.Time div 86400000;
  TS.Time := TS.Time mod 86400000;
  lblUseTime.Caption := Format('处理所需时间：%d日', [Days]) + FormatDateTime('H小时M分S秒', TimeStampToDateTime(TS));

  Caption := Format('正在处理 : ①. 第  %d  次（ 遍历 ）； ②.前一次遍历产生（ 第 1 - N 行为首行 ）总行数 :  %d  行 ；③. 第 %d 行为首行 ； ④.正在（ 遍历 ）（ 第 N 行为首行 ）总行数 :  %d  行 。', [
    fDataComputer.BatchNumber,
    fDataComputer.BatchNumberRowCount,
    fDataComputer.FirstNumber,
    fDataComputer.GroupCount
  ]);
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
    DataMode := 1;
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
        chkVertExportSource.Checked := ExportSource;
        edtVertExportCodeNameValueCount.Text := ExportCodeNameValueCount.ToString;
        edtVertExportCodeNameValueCount2.Text := ExportCodeNameValueCount2.ToString;
      end;
      cmSlant:
      begin
        PageControl1.ActivePageIndex := 2;
        edtCompareSpacing.Text := SlantCompareSpacing.ToString;
        edtSlantGroupCount.Text := GroupCount.ToString;
        edtSlantKeepCodeNameValueCount.Text := KeepCodeNameValueCount.ToString;
        chkSlantExportSource.Checked := ExportSource;
        edtSlantExportCodeNameValueCount.Text := ExportCodeNameValueCount.ToString;
        edtSlantExportCodeNameValueCount2.Text := ExportCodeNameValueCount2.ToString;
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
        chkVertSlantExportSource.Checked := ExportSource;
        edtVertSlantExportCodeNameValueCount.Text := ExportCodeNameValueCount.ToString;
        edtVertSlantExportCodeNameValueCount2.Text := ExportCodeNameValueCount2.ToString;
      end;
    end;
    edtIntervalValue.ReadOnly := CompareMode > cmNone;
    edtIntervalValue2.ReadOnly := CompareMode > cmNone;
    edtVertCompareSpacing.ReadOnly := CompareMode > cmNone;
    edtVertSameValueCount.ReadOnly := CompareMode > cmNone;
    edtVertSameValueCount2.ReadOnly := CompareMode > cmNone;
    //edtVertGroupCount.ReadOnly := CompareMode > cmNone;
    //chkVertExportSource.Enabled := CompareMode = cmNone;
    //edtVertExportCodeNameValueCount.ReadOnly := CompareMode > cmNone;
    //edtVertExportCodeNameValueCount2.ReadOnly := CompareMode > cmNone;
    edtCompareSpacing.ReadOnly := CompareMode > cmNone;
    //edtSlantGroupCount.ReadOnly := CompareMode > cmNone;
    //chkSlantExportSource.Enabled := CompareMode = cmNone;
    //edtSlantExportCodeNameValueCount.ReadOnly := CompareMode > cmNone;
    //edtSlantExportCodeNameValueCount2.ReadOnly := CompareMode > cmNone;
    edtVVertCompareSpacing.ReadOnly := CompareMode > cmNone;
    edtVVertSameValueCount.ReadOnly := CompareMode > cmNone;
    edtVVertSameValueCount2.ReadOnly := CompareMode > cmNone;
    edtSlantCompareSpacing.ReadOnly := CompareMode > cmNone;
    edtSlantSameValueCount.ReadOnly := CompareMode > cmNone;
    edtSlantSameValueCount2.ReadOnly := CompareMode > cmNone;
    //edtVertSlantGroupCount.ReadOnly := CompareMode > cmNone;
    //chkVertSlantExportSource.Enabled := CompareMode = cmNone;
    //edtVertSlantExportCodeNameValueCount.ReadOnly := CompareMode > cmNone;
    //edtVertSlantExportCodeNameValueCount2.ReadOnly := CompareMode > cmNone;
  end;

  OnStateChange(False);

  //fDataComputer.OnGroupCodeName := OnGroupCodeName;
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
    if Sender = btnSlantCompare then
      CompareMode := cmSlant
    else if Sender = btnVertCompare then
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
      fSettings.ExportSource := chkVertExportSource.Checked;
      if not TryStrToInt(edtVertExportCodeNameValueCount.Text, v) then
        raise Exception.Create('请输入有效导出代号个数');
      fSettings.ExportCodeNameValueCount := v;
      if not TryStrToInt(edtVertExportCodeNameValueCount2.Text, v) then
        raise Exception.Create('请输入有效导出代号个数2');
      fSettings.ExportCodeNameValueCount2 := v;
    end;
    cmSlant:
    begin
      if not TryStrToInt(edtSlantGroupCount.Text, v) then
        raise Exception.Create('请输入有效遍历次数');
      fSettings.GroupCount := v;
      if not TryStrToInt(edtSlantKeepCodeNameValueCount.Text, v) then
        raise Exception.Create('请输入有效最小代号个数');
      fSettings.KeepCodeNameValueCount := v;
      fSettings.ExportSource := chkSlantExportSource.Checked;
      if not TryStrToInt(edtSlantExportCodeNameValueCount.Text, v) then
        raise Exception.Create('请输入有效导出代号个数');
      fSettings.ExportCodeNameValueCount := v;
      if not TryStrToInt(edtSlantExportCodeNameValueCount2.Text, v) then
        raise Exception.Create('请输入有效导出代号个数2');
      fSettings.ExportCodeNameValueCount2 := v;
    end;
    cmVertSlant:
    begin
      if not TryStrToInt(edtVertSlantGroupCount.Text, v) then
        raise Exception.Create('请输入有效遍历次数');
      fSettings.GroupCount := v;
      if not TryStrToInt(edtVertSlantKeepCodeNameValueCount.Text, v) then
        raise Exception.Create('请输入有效最小代号个数');
      fSettings.KeepCodeNameValueCount := v;
      fSettings.ExportSource := chkVertSlantExportSource.Checked;
      if not TryStrToInt(edtVertSlantExportCodeNameValueCount.Text, v) then
        raise Exception.Create('请输入有效导出代号个数');
      fSettings.ExportCodeNameValueCount := v;
      if not TryStrToInt(edtVertSlantExportCodeNameValueCount2.Text, v) then
        raise Exception.Create('请输入有效导出代号个数2');
      fSettings.ExportCodeNameValueCount2 := v;
    end;
  end;
  WriteSettings;
  fSettings.ExportFile := True;
  if (Sender = btnVertCompare2)
    or (Sender = btnSlantCompare2)
    or (Sender = btnVertSlantCompare2)
  then
  begin
    fSettings.ExportFile := False;
    fSettings.ExportSource := True;
  end;

  OnStateChange(True);
  System.TMonitor.PulseAll(fDataComputer.Lock);
  Timer.OnTimer(Timer);
  Timer.Enabled := True;
end;

end.
