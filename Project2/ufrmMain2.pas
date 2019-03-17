unit ufrmMain2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.Types, System.IOUtils,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.FileCtrl,
  Vcl.CheckLst, Vcl.ComCtrls, Vcl.Menus;

type
  TfrmMain = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label5: TLabel;
    btnSlantCompare: TButton;
    edtExportTypeCount: TEdit;
    OpenDialog: TOpenDialog;
    Label11: TLabel;
    btnVertCompare: TButton;
    edtVertExportTypeCount: TEdit;
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
    edtVertCompareTypeCount: TEdit;
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
    Label16: TLabel;
    Label18: TLabel;
    btnVertSlantCompare: TButton;
    edtExportGroupValueCount: TEdit;
    edtCompareGroupValueCount: TEdit;
    btnVertSlantExportSettings: TButton;
    btnVertExportSettings: TButton;
    btnSlantExportSettings: TButton;
    chkCompareCrossRange: TCheckBox;
    btnExportVertSlantFileSettings: TButton;
    btnExportVertFileSettings: TButton;
    btnExportSlantFileSettings: TButton;
    MainMenu: TMainMenu;
    N1: TMenuItem;
    miConsumer: TMenuItem;
    Timer: TTimer;
    miCombinationCalculator: TMenuItem;
    Label6: TLabel;
    Label13: TLabel;
    edtAddress: TEdit;
    edtPort: TEdit;
    btnOk: TButton;
    Label17: TLabel;
    Label21: TLabel;
    edtVertSlantMaxGroupCount: TEdit;
    edtVertSlantMaxGroupCount2: TEdit;
    Label19: TLabel;
    edtVertMaxGroupCount: TEdit;
    edtVertMaxGroupCount2: TEdit;
    Label23: TLabel;
    edtSlantMaxGroupCount: TEdit;
    edtSlantMaxGroupCount2: TEdit;
    edtVertSlantMaxGroupCount3: TEdit;
    edtVertMaxGroupCount3: TEdit;
    edtSlantMaxGroupCount3: TEdit;
    btnCodeNameSortSettings: TButton;
    procedure FormCreate(Sender: TObject);
    procedure edtFileNameClick(Sender: TObject);
    procedure btnCompareClick(Sender: TObject);
    procedure btnExportSettingsClick(Sender: TObject);
    procedure btnExportVertSlantFileSettingsClick(Sender: TObject);
    procedure btnExportVertFileSettingsClick(Sender: TObject);
    procedure btnExportSlantFileSettingsClick(Sender: TObject);
    procedure miConsumerClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure miCombinationCalculatorClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnCodeNameSortSettingsClick(Sender: TObject);
  private
    procedure OnStateChange(Working: Boolean);
    procedure OnGroupCodeName(FirstRow: Word);
    procedure OnFinish(Sender: TObject);
  public

  end;

var
  frmMain: TfrmMain;

implementation

uses
  uDataComputer, ufrmExportSettings2, ufrmExportVertSlantFileSettings2,
  ufrmExportVertFileSettings2, ufrmExportSlantFileSettings2, ufrmConnectionSettings,
  ufrmConsumer, ufrmCombinationCalculator, ufrmCodeNameSortSettings;

{$R *.dfm}

procedure TfrmMain.OnStateChange(Working: Boolean);
begin
  btnSlantExportSettings.Enabled := not Working and (fSettings.CompareMode in [cmNone, cmSlant]);
  btnExportSlantFileSettings.Enabled := not Working and (fSettings.CompareMode in [cmNone, cmSlant]);
  btnSlantCompare.Enabled := not Working and (fSettings.CompareMode in [cmNone, cmSlant]);

  btnVertExportSettings.Enabled := not Working and (fSettings.CompareMode in [cmNone, cmVert]);
  btnExportVertFileSettings.Enabled := not Working and (fSettings.CompareMode in [cmNone, cmVert]);
  btnVertCompare.Enabled := not Working and (fSettings.CompareMode in [cmNone, cmVert]);

  btnVertSlantExportSettings.Enabled := not Working and (fSettings.CompareMode in [cmNone, cmVertSlant]);
  btnExportVertSlantFileSettings.Enabled := not Working and (fSettings.CompareMode in [cmNone, cmVertSlant]);
  btnVertSlantCompare.Enabled := not Working and (fSettings.CompareMode in [cmNone, cmVertSlant]);

  {btnSlantCompare.Enabled := not Working;
  btnVertCompare.Enabled := not Working;
  btnVertSlantCompare.Enabled := not Working;
  btnSlantExportSettings.Enabled := not Working;
  btnVertExportSettings.Enabled := not Working;
  btnVertSlantExportSettings.Enabled := not Working;
  btnExportVertSlantFileSettings.Enabled := not Working;
  btnExportVertFileSettings.Enabled := not Working;
  btnExportSlantFileSettings.Enabled := not Working;}
end;

procedure TfrmMain.OnGroupCodeName(FirstRow: Word);
begin
  TThread.Queue(nil, procedure
  begin
    Caption := Format('正在处理 [（第 N 行为首行）] ： %d  行', [FirstRow]);
  end);
end;

procedure TfrmMain.OnFinish(Sender: TObject);
begin
  Timer.Enabled := False;
  OnStateChange(False);
  ShowMessage('查询完毕');
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
end;

procedure TfrmMain.btnExportSettingsClick(Sender: TObject);
begin
  if Sender = btnVertExportSettings then frmExportSettings.Flag := 1
  else if Sender = btnSlantExportSettings then frmExportSettings.Flag := 2
  else frmExportSettings.Flag := 0;
  frmExportSettings.ShowModal;
end;

procedure TfrmMain.btnExportSlantFileSettingsClick(Sender: TObject);
begin
  frmExportSlantFileSettings.ShowModal;
end;

procedure TfrmMain.btnExportVertFileSettingsClick(Sender: TObject);
begin
  frmExportVertFileSettings.ShowModal;
end;

procedure TfrmMain.btnExportVertSlantFileSettingsClick(Sender: TObject);
begin
  frmExportVertSlantFileSettings.ShowModal;
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
        edtVertCompareTypeCount.Text := GroupValueCount.ToString;
        edtVertMaxGroupCount.Text := (MaxGroupCount div 100000000).ToString;
        edtVertMaxGroupCount2.Text := (MaxGroupCount mod 100000000 div 10000).ToString;
        edtVertMaxGroupCount3.Text := (MaxGroupCount mod 10000).ToString;
        edtVertExportTypeCount.Text := ExportGroupValueCount.ToString;
      end;
      cmSlant:
      begin
        PageControl1.ActivePageIndex := 2;
        edtCompareSpacing.Text := SlantCompareSpacing.ToString;
        edtExportTypeCount.Text := ExportGroupValueCount.ToString;
        edtSlantMaxGroupCount.Text := (MaxGroupCount div 100000000).ToString;
        edtSlantMaxGroupCount2.Text := (MaxGroupCount mod 100000000 div 10000).ToString;
        edtSlantMaxGroupCount3.Text := (MaxGroupCount mod 10000).ToString;
      end;
      cmVertSlant:
      begin
        edtVVertCompareSpacing.Text := VertCompareSpacing.ToString;
        edtVVertSameValueCount.Text := VertSameValueCount.ToString;
        edtVVertSameValueCount2.Text := VertSameValueCount2.ToString;
        edtSlantCompareSpacing.Text := SlantCompareSpacing.ToString;
        edtSlantSameValueCount.Text := SlantSameValueCount.ToString;
        edtSlantSameValueCount2.Text := SlantSameValueCount2.ToString;
        edtCompareGroupValueCount.Text := GroupValueCount.ToString;
        edtVertSlantMaxGroupCount.Text := (MaxGroupCount div 100000000).ToString;
        edtVertSlantMaxGroupCount2.Text := (MaxGroupCount mod 100000000 div 10000).ToString;
        edtVertSlantMaxGroupCount3.Text := (MaxGroupCount mod 10000).ToString;
        edtExportGroupValueCount.Text := ExportGroupValueCount.ToString;
      end;
    end;

    edtIntervalValue.ReadOnly := CompareMode > cmNone;
    edtIntervalValue2.ReadOnly := CompareMode > cmNone;
    edtVertCompareSpacing.ReadOnly := CompareMode > cmNone;
    edtVertSameValueCount.ReadOnly := CompareMode > cmNone;
    edtVertSameValueCount2.ReadOnly := CompareMode > cmNone;
    edtVertCompareTypeCount.ReadOnly := CompareMode > cmNone;
    edtVertExportTypeCount.ReadOnly := CompareMode > cmNone;
    edtCompareSpacing.ReadOnly := CompareMode > cmNone;
    edtExportTypeCount.ReadOnly := CompareMode > cmNone;
    edtVVertCompareSpacing.ReadOnly := CompareMode > cmNone;
    edtVVertSameValueCount.ReadOnly := CompareMode > cmNone;
    edtVVertSameValueCount2.ReadOnly := CompareMode > cmNone;
    edtSlantCompareSpacing.ReadOnly := CompareMode > cmNone;
    edtSlantSameValueCount.ReadOnly := CompareMode > cmNone;
    edtSlantSameValueCount2.ReadOnly := CompareMode > cmNone;
    edtCompareGroupValueCount.ReadOnly := CompareMode > cmNone;
    edtExportGroupValueCount.ReadOnly := CompareMode > cmNone;
    edtVertMaxGroupCount.ReadOnly := CompareMode > cmNone;
    edtVertMaxGroupCount2.ReadOnly := CompareMode > cmNone;
    edtVertMaxGroupCount3.ReadOnly := CompareMode > cmNone;
    edtSlantMaxGroupCount.ReadOnly := CompareMode > cmNone;
    edtSlantMaxGroupCount2.ReadOnly := CompareMode > cmNone;
    edtSlantMaxGroupCount3.ReadOnly := CompareMode > cmNone;
    edtVertSlantMaxGroupCount.ReadOnly := CompareMode > cmNone;
    edtVertSlantMaxGroupCount2.ReadOnly := CompareMode > cmNone;
    edtVertSlantMaxGroupCount3.ReadOnly := CompareMode > cmNone;
  end;

  OnStateChange(False);

  fDataComputer := TDataComputer.Create;
  fDataComputer.OnGroupCodeName := OnGroupCodeName;
  fDataComputer.OnFinish := OnFinish;
  fDataComputer.Start;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  fConsumers.Queue.DoShutDown;
  fDataComputer.Terminate;
  System.TMonitor.PulseAll(fDataComputer.Lock);
  fDataComputer.WaitFor;
  fDataComputer.Free;
end;

procedure TfrmMain.miCombinationCalculatorClick(Sender: TObject);
begin
  CombinationCalculator;
end;

procedure TfrmMain.miConsumerClick(Sender: TObject);
begin
  frmConsumer.ShowModal;
end;

procedure TfrmMain.btnCodeNameSortSettingsClick(Sender: TObject);
begin
  CodeNameSortSet;
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

    if fSettings.CodeNameSort = 0 then
      raise Exception.Create('请选择代号排序');

    fSettings.ExportFiles := [];
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
        if not TryStrToInt(edtVertCompareTypeCount.Text, v) then
          raise Exception.Create('请输入有效比较组合数');
        fSettings.GroupValueCount := v;
        v := 0;
        if not Trim(edtVertMaxGroupCount.Text).IsEmpty
          and not TryStrToInt(edtVertMaxGroupCount.Text, v)
        then raise Exception.Create('请输入有效组合数上限');
        fSettings.MaxGroupCount := v * 100000000;
        v := 0;
        if not Trim(edtVertMaxGroupCount2.Text).IsEmpty
          and not TryStrToInt(edtVertMaxGroupCount2.Text, v)
        then raise Exception.Create('请输入有效组合数上限');
        fSettings.MaxGroupCount := fSettings.MaxGroupCount + v * 10000;
        v := 0;
        if not Trim(edtVertMaxGroupCount3.Text).IsEmpty
          and not TryStrToInt(edtVertMaxGroupCount3.Text, v)
        then raise Exception.Create('请输入有效组合数上限');
        fSettings.MaxGroupCount := fSettings.MaxGroupCount + v;
        if not TryStrToInt(edtVertExportTypeCount.Text, v) then
          raise Exception.Create('请输入有效导出次数');
        fSettings.ExportGroupValueCount := v;

        fSettings.SlantCompareSpacing := 0;
        fSettings.SlantSameValueCount := 0;
        fSettings.SlantSameValueCount2 := 0;
      end;
      cmSlant:
      begin
        if not TryStrToInt(edtCompareSpacing.Text, v) then
          raise Exception.Create('请输入有效比较次数');
        fSettings.SlantCompareSpacing := v;
        if not TryStrToInt(edtExportTypeCount.Text, v) then
          raise Exception.Create('请输入有效导出次数');
        fSettings.ExportGroupValueCount := v;
        v := 0;
        if not Trim(edtSlantMaxGroupCount.Text).IsEmpty
          and not TryStrToInt(edtSlantMaxGroupCount.Text, v)
        then raise Exception.Create('请输入有效组合数上限');
        fSettings.MaxGroupCount := v * 100000000;
        v := 0;
        if not Trim(edtSlantMaxGroupCount2.Text).IsEmpty
          and not TryStrToInt(edtSlantMaxGroupCount2.Text, v)
        then raise Exception.Create('请输入有效组合数上限');
        fSettings.MaxGroupCount := fSettings.MaxGroupCount + v * 10000;
        v := 0;
        if not Trim(edtSlantMaxGroupCount3.Text).IsEmpty
          and not TryStrToInt(edtSlantMaxGroupCount3.Text, v)
        then raise Exception.Create('请输入有效组合数上限');
        fSettings.MaxGroupCount := fSettings.MaxGroupCount + v;

        fSettings.VertCompareSpacing := 0;
        fSettings.VertSameValueCount := 0;
        fSettings.VertSameValueCount2 := 0;
        fSettings.SlantSameValueCount := 0;
        fSettings.SlantSameValueCount2 := 0;
        fSettings.GroupValueCount := 1;
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
        if not TryStrToInt(edtCompareGroupValueCount.Text, v) then
          raise Exception.Create('请输入有效比较组合数');
        fSettings.GroupValueCount := v;
        v := 0;
        if not Trim(edtVertSlantMaxGroupCount.Text).IsEmpty
          and not TryStrToInt(edtVertSlantMaxGroupCount.Text, v)
        then raise Exception.Create('请输入有效组合数上限');
        fSettings.MaxGroupCount := v * 100000000;
        v := 0;
        if not Trim(edtVertSlantMaxGroupCount2.Text).IsEmpty
          and not TryStrToInt(edtVertSlantMaxGroupCount2.Text, v)
        then raise Exception.Create('请输入有效组合数上限');
        fSettings.MaxGroupCount := fSettings.MaxGroupCount + v * 10000;
        v := 0;
        if not Trim(edtVertSlantMaxGroupCount3.Text).IsEmpty
          and not TryStrToInt(edtVertSlantMaxGroupCount3.Text, v)
        then raise Exception.Create('请输入有效组合数上限');
        fSettings.MaxGroupCount := fSettings.MaxGroupCount + v;
        if not TryStrToInt(edtExportGroupValueCount.Text, v) then
          raise Exception.Create('请输入有效导出次数');
        fSettings.ExportGroupValueCount := v;
      end;
    end;
    fSettings.CompareMode := CompareMode;
    WriteSettings;
  end;

  case fSettings.CompareMode of
    cmVert:
    begin
      with frmExportVertFileSettings do
      begin
        if chkFile.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile];
        if chkFile2.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile2];
        if chkFile3.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile3];
        if chkFile4.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile4];
        if chkFile5.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile5];
        if chkFile6.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile6];
        if chkFile7.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile7];
        if chkFile8.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile8];
        if chkFile9.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile9];
        if chkFile10.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile10];
        if chkFile11.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile11];
        if chkFile12.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile12];
        if chkFile14.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile14];
        if chkFile15.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile15];
      end;
    end;
    cmSlant:
    begin
      with frmExportSlantFileSettings do
      begin
        if chkFile.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile];
        if chkFile2.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile2];
        if chkFile3.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile3];
        if chkFile4.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile4];
        if chkFile5.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile5];
        if chkFile6.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile6];
        if chkFile7.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile7];
        if chkFile8.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile8];
        if chkFile9.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile9];
        if chkFile10.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile10];
        if chkFile11.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile11];
        if chkFile12.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile12];
        if chkFile13.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile13];
        if chkFile14.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile14];
        if chkFile15.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile15];
      end;
    end;
    cmVertSlant:
    begin
      with frmExportVertSlantFileSettings do
      begin
        if chkFile.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile];
        if chkFile2.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile2];
        if chkFile3.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile3];
        if chkFile4.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile4];
        if chkFile5.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile5];
        if chkFile6.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile6];
        if chkFile7.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile7];
        if chkFile8.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile8];
        if chkFile9.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile9];
        if chkFile10.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile10];
        if chkFile11.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile11];
        if chkFile12.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile12];
        if chkFile13.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile13];
        if chkFile14.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile14];
        if chkFile15.Checked then fSettings.ExportFiles := fSettings.ExportFiles + [efFile15];
      end;
    end;
  end;

  OnStateChange(True);
  System.TMonitor.PulseAll(fDataComputer.Lock);
  Timer.OnTimer(Timer);
  Timer.Enabled := True;
end;

end.
