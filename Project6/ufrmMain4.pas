unit ufrmMain4;

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
    btnVertCompare2: TButton;
    Label25: TLabel;
    btnSlantCompare2: TButton;
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
    lblRowCount: TLabel;
    Label6: TLabel;
    edtSameValueCount2: TEdit;
    edtSameValueCount: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure edtFileNameClick(Sender: TObject);
    procedure btnCompareClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure btnVertSlantGroupCodeNameSettingsClick(Sender: TObject);
    procedure btnVertGroupCodeNameSettingsClick(Sender: TObject);
    procedure btnVertSlantGroupCodeNameSettings2Click(Sender: TObject);
    procedure btnVertGroupCodeNameSettings2Click(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnSlantExportFileSettingsClick(Sender: TObject);
    procedure btnVertExportFileSettingsClick(Sender: TObject);
    procedure btnVertSlantExportFileSettingsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    function CheckIntervalValues(var IntervalValues: TWordDynArray): Boolean;
    procedure OnStateChange(Working: Boolean);
    procedure OnTerminate(Sender: TObject);
    procedure ShowRowCount;
  public

  end;

var
  frmMain: TfrmMain;

implementation

uses
  uCommon, uDataComputer, uControlHelper, ufrmGroupCodeNameSettings, ufrmGroupCodeNameSettings2,
  ufrmExportFileSettings;

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

procedure TfrmMain.OnStateChange(Working: Boolean);
var
  v: Variant;
  CompareMode: TCompareMode;
begin
  CompareMode := cmNone;
  fKeyValue.GetKeyValue('CompareMode', v);
  if not VarIsEmpty(v) then CompareMode := v;

  btnSlantCompare.Enabled := not Working and (CompareMode in [cmNone, cmSlant]);
  btnVertCompare.Enabled := not Working and (CompareMode in [cmNone, cmVert]);
  btnVertSlantCompare.Enabled := not Working and (CompareMode in [cmNone, cmVertSlant]);
  btnSlantCompare2.Enabled := not Working and (CompareMode in [cmNone, cmSlant]);
  btnVertCompare2.Enabled := not Working and (CompareMode in [cmNone, cmVert]);
  btnVertSlantCompare2.Enabled := not Working and (CompareMode in [cmNone, cmVertSlant]);
  btnVertGroupCodeNameSettings.Enabled := CompareMode in [cmNone, cmVert];
  btnSlantGroupCodeNameSettings.Enabled := CompareMode in [cmNone, cmSlant];
  btnVertSlantGroupCodeNameSettings.Enabled := CompareMode in [cmNone, cmVertSlant];
  btnVertGroupCodeNameSettings2.Enabled := CompareMode in [cmNone, cmVert];
  btnSlantGroupCodeNameSettings2.Enabled := CompareMode in [cmNone, cmSlant];
  btnVertSlantGroupCodeNameSettings2.Enabled := CompareMode in [cmNone, cmVertSlant];
  btnVertExportFileSettings.Enabled := CompareMode in [cmNone, cmVert];
  btnSlantExportFileSettings.Enabled := CompareMode in [cmNone, cmSlant];
  btnVertSlantExportFileSettings.Enabled := CompareMode in [cmNone, cmVertSlant];
  if Assigned(frmGroupCodeNameSettings) then
    frmGroupCodeNameSettings.btnOk.Enabled := not Working;
  if Assigned(frmGroupCodeNameSettings2) then
    frmGroupCodeNameSettings2.btnOk.Enabled := not Working;
  if Assigned(frmExportFileSettings) then
    frmExportFileSettings.btnOk.Enabled := not Working;
end;

procedure TfrmMain.OnTerminate(Sender: TObject);
begin
  TThread.Queue(nil, procedure
  begin
    fDataComputer := nil;
    Caption := '';
    Timer.Enabled := False;
    OnStateChange(False);
    ShowRowCount;
  end);
end;

procedure TfrmMain.ShowRowCount;
var
  RowCount: Int64;
begin
  RowCount := fDatabase.TableRowCount(TSQLCodeName);
  lblRowCount.Caption := Format('最后一次遍历 ，生成（ 存储 ）在 【 Data 数据库 】的行数 ： %s  行 。', [NumberToString(RowCount)]);
end;

procedure TfrmMain.TimerTimer(Sender: TObject);
const
  sGroupCodeName: string = '1. 已处理 :  ①. 第 %d 次（ 遍历 ）产生（ 第 1 - %s 行 ）； 2. 正处理 :  ①. 第 %d 次（ 遍历 ）； ②.（ 第 %s 行 ）遍历 [ 该行（ 已组合 ）的（ 第 %s 行 ）]';
var
  TS: TTimeStamp;
  Days: Byte;
  Seconds: Int64;
  BatchNumber, BatchNumberRowCount, FirstNumber, FirstNumberGroupCount: Integer;
begin
  Caption := '';
  lblUseTime.Caption := '';
  Seconds := 0;
  FirstNumber := 0;
  if fMainApp then
  begin
    Seconds := fDataComputer.Stopwatch.ElapsedMilliseconds;
    TS.Date := DateDelta;
    TS.Time := Seconds mod 86400000;
    Days := Seconds div 86400000;

    BatchNumber := fDataComputer.BatchNumber;
    BatchNumberRowCount := fDataComputer.BatchNumberRowCount;
    FirstNumber := fDataComputer.FirstNumber;
    FirstNumberGroupCount := fDataComputer.FirstNumberGroupCount;
  end
  else
  begin
    try
      if Assigned(fThread) then
      begin
        if fThread is TSyncCodeName then Caption := '正在同步代号'
        else if fThread is TDataComputer then
        begin
          BatchNumber := TDataComputer(fThread).BatchNumber;
          BatchNumberRowCount := TDataComputer(fThread).BatchNumberRowCount;
          FirstNumber := TDataComputer(fThread).FirstNumber;
          FirstNumberGroupCount := TDataComputer(fThread).FirstNumberGroupCount;
        end
        else if fThread is TUploadCodeName then Caption := '正在上传代号';
      end;
    except

    end;
  end;
  if Seconds > 0 then
    lblUseTime.Caption := Format('处理所需时间：%d日', [Days]) + FormatDateTime('H小时M分S秒', TimeStampToDateTime(TS));

  if FirstNumber > 0 then
    Caption := Format(sGroupCodeName, [
      BatchNumber - 1,
      NumberToString(BatchNumberRowCount),
      BatchNumber,
      NumberToString(FirstNumber),
      NumberToString(FirstNumberGroupCount)
    ]);
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
  lblRowCount.Visible := False;
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
  CompareMode: TCompareMode;
  IntervalValues: TWordDynArray;
  VertCompareSpacing, VertSameValueCount, VertSameValueCount2,
  SlantCompareSpacing, SlantSameValueCount, SlantSameValueCount2,
  KeepCodeNameValueCount: Word;
  GroupCount: Byte;
begin
  PageControl1.ActivePageIndex := 0;

  fKeyValue.GetKeyValue('IntervalValues', IntervalValues);
  if Length(IntervalValues) > 0 then
  begin
    edtIntervalValue.Text := IntervalValues[0].ToString;
    if Length(IntervalValues) > 1 then
      edtIntervalValue2.Text := IntervalValues[1].ToString;
  end;
  fKeyValue.GetKeyValue('CompareCrossRange', v);
  if not VarIsEmpty(v) then chkCompareCrossRange.Checked := v;

  fKeyValue.GetKeyValue('VertCompareSpacing', v);
  if not VarIsEmpty(v) then VertCompareSpacing := v;
  fKeyValue.GetKeyValue('VertSameValueCount', v);
  if not VarIsEmpty(v) then VertSameValueCount := v;
  fKeyValue.GetKeyValue('VertSameValueCount2', v);
  if not VarIsEmpty(v) then VertSameValueCount2 := v;
  fKeyValue.GetKeyValue('SlantCompareSpacing', v);
  if not VarIsEmpty(v) then SlantCompareSpacing := v;
  fKeyValue.GetKeyValue('SlantSameValueCount', v);
  if not VarIsEmpty(v) then SlantSameValueCount := v;
  fKeyValue.GetKeyValue('SlantSameValueCount2', v);
  if not VarIsEmpty(v) then SlantSameValueCount2 := v;
  fKeyValue.GetKeyValue('GroupCount', v);
  if not VarIsEmpty(v) then GroupCount := v;
  fKeyValue.GetKeyValue('KeepCodeNameValueCount', v);
  if not VarIsEmpty(v) then KeepCodeNameValueCount := v;

  CompareMode := cmNone;
  fKeyValue.GetKeyValue('CompareMode', v);
  if not VarIsEmpty(v) then CompareMode := v;
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
      edtSameValueCount.Text := SlantSameValueCount.ToString;
      edtSameValueCount2.Text := SlantSameValueCount2.ToString;
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
  edtSameValueCount.ReadOnly := CompareMode > cmNone;
  edtSameValueCount2.ReadOnly := CompareMode > cmNone;
  edtVVertCompareSpacing.ReadOnly := CompareMode > cmNone;
  edtVVertSameValueCount.ReadOnly := CompareMode > cmNone;
  edtVVertSameValueCount2.ReadOnly := CompareMode > cmNone;
  edtSlantCompareSpacing.ReadOnly := CompareMode > cmNone;
  edtSlantSameValueCount.ReadOnly := CompareMode > cmNone;
  edtSlantSameValueCount2.ReadOnly := CompareMode > cmNone;

  OnStateChange(False);
  ShowRowCount;
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
  v: Variant;
  CompareCrossRange: Boolean;
  CompareMode: TCompareMode;
  IntervalValues: TWordDynArray;
  VertCompareSpacing, VertSameValueCount, VertSameValueCount2,
  SlantCompareSpacing, SlantSameValueCount, SlantSameValueCount2,
  GroupCount, KeepCodeNameValueCount,
  ExportCodeNameValueCount, ExportCodeNameValueCount2: Integer;
begin
  CompareMode := cmNone;
  fKeyValue.GetKeyValue('CompareMode', v);
  if not VarIsEmpty(v) then CompareMode := v;
  if CompareMode = cmNone then
  begin
    if (Sender = btnSlantCompare) or (Sender = btnSlantCompare2) then
      CompareMode := cmSlant
    else if (Sender = btnVertCompare) or (Sender = btnVertCompare2) then
      CompareMode := cmVert
    else
      CompareMode := cmVertSlant;

    if not CheckIntervalValues(IntervalValues) then
      raise Exception.Create('请输入有效列数范围');

    case CompareMode of
      cmVert:
      begin
        if not edtVertCompareSpacing.TryToValue(VertCompareSpacing) then
          raise Exception.Create('请输入有效比较次数');
        if not edtVertSameValueCount.TryToValue(VertSameValueCount) then
          raise Exception.Create('请输入有效相同列数');
        if not edtVertSameValueCount2.TryToValue(VertSameValueCount2) then
          raise Exception.Create('请输入有效相同列数2');
        if not edtVertGroupCount.TryToValue(GroupCount) then
          raise Exception.Create('请输入有效遍历次数');
        if not edtVertKeepCodeNameValueCount.TryToValue(KeepCodeNameValueCount) then
          raise Exception.Create('请输入有效最小代号个数');
        SlantCompareSpacing := 0;
        SlantSameValueCount := 0;
        SlantSameValueCount2 := 0;
      end;
      cmSlant:
      begin
        if not edtCompareSpacing.TryToValue(SlantCompareSpacing) then
          raise Exception.Create('请输入有效比较次数');
        if not edtSameValueCount.TryToValue(SlantSameValueCount) then
          raise Exception.Create('请输入有效相同列数');
        if not edtSameValueCount2.TryToValue(SlantSameValueCount2) then
          raise Exception.Create('请输入有效相同列数');
        if not edtSlantGroupCount.TryToValue(GroupCount) then
          raise Exception.Create('请输入有效遍历次数');
        if not edtSlantKeepCodeNameValueCount.TryToValue(KeepCodeNameValueCount) then
          raise Exception.Create('请输入有效最小代号个数');
        VertCompareSpacing := 0;
        VertSameValueCount := 0;
        VertSameValueCount2 := 0;
      end;
      cmVertSlant:
      begin
        if not edtVVertCompareSpacing.TryToValue(VertCompareSpacing) then
          raise Exception.Create('请输入有效比较次数');
        if not edtVVertSameValueCount.TryToValue(VertSameValueCount) then
          raise Exception.Create('请输入有效相同列数');
        if not edtVVertSameValueCount2.TryToValue(VertSameValueCount2) then
          raise Exception.Create('请输入有效相同列数');
        if not edtSlantCompareSpacing.TryToValue(SlantCompareSpacing) then
          raise Exception.Create('请输入有效比较次数');
        if not edtSlantSameValueCount.TryToValue(SlantSameValueCount) then
          raise Exception.Create('请输入有效相同列数');
        if not edtSlantSameValueCount2.TryToValue(SlantSameValueCount2) then
          raise Exception.Create('请输入有效相同列数');
        if not edtVertSlantGroupCount.TryToValue(GroupCount) then
          raise Exception.Create('请输入有效遍历次数');
        if not edtVertSlantKeepCodeNameValueCount.TryToValue(KeepCodeNameValueCount) then
          raise Exception.Create('请输入有效最小代号个数');
      end;
    end;

    CompareCrossRange := chkCompareCrossRange.Checked;

    fKeyValue.SetKeyValue('CompareMode', CompareMode);
    fKeyValue.SetKeyValue('IntervalValues', IntervalValues);
    fKeyValue.SetKeyValue('CompareCrossRange', CompareCrossRange);
    fKeyValue.SetKeyValue('VertCompareSpacing', VertCompareSpacing);
    fKeyValue.SetKeyValue('VertSameValueCount', VertSameValueCount);
    fKeyValue.SetKeyValue('VertSameValueCount2', VertSameValueCount2);
    fKeyValue.SetKeyValue('SlantCompareSpacing', SlantCompareSpacing);
    fKeyValue.SetKeyValue('VertSameValueCount', VertSameValueCount);
    fKeyValue.SetKeyValue('VertSameValueCount2', VertSameValueCount2);
    fKeyValue.SetKeyValue('GroupCount', GroupCount);
    fKeyValue.SetKeyValue('KeepCodeNameValueCount', KeepCodeNameValueCount);
  end
  else
  begin
    fKeyValue.GetKeyValue('IntervalValues', IntervalValues);
    fKeyValue.GetKeyValue('CompareCrossRange', v);
    if not VarIsEmpty(v) then CompareCrossRange := v;
    fKeyValue.GetKeyValue('VertCompareSpacing', v);
    if not VarIsEmpty(v) then VertCompareSpacing := v;
    fKeyValue.GetKeyValue('VertSameValueCount', v);
    if not VarIsEmpty(v) then VertSameValueCount := v;
    fKeyValue.GetKeyValue('VertSameValueCount2', v);
    if not VarIsEmpty(v) then VertSameValueCount2 := v;
    fKeyValue.GetKeyValue('SlantCompareSpacing', v);
    if not VarIsEmpty(v) then SlantCompareSpacing := v;
    fKeyValue.GetKeyValue('VertSameValueCount', v);
    if not VarIsEmpty(v) then VertSameValueCount := v;
    fKeyValue.GetKeyValue('VertSameValueCount2', v);
    if not VarIsEmpty(v) then VertSameValueCount2 := v;
    fKeyValue.GetKeyValue('GroupCount', v);
    if not VarIsEmpty(v) then GroupCount := v;
    fKeyValue.GetKeyValue('KeepCodeNameValueCount', v);
    if not VarIsEmpty(v) then KeepCodeNameValueCount := v;
  end;
  fKeyValue.GetKeyValue('ExportCodeNameValueCount', v);
  if not VarIsEmpty(v) then ExportCodeNameValueCount := v;
  fKeyValue.GetKeyValue('ExportCodeNameValueCount2', v);
  if not VarIsEmpty(v) then ExportCodeNameValueCount2 := v;
  if not ((ExportCodeNameValueCount2 >= ExportCodeNameValueCount) and (ExportCodeNameValueCount2 > 0)) then
    raise Exception.Create('请输入有效导出代号个数范围');

  OnStateChange(True);
  Timer.Enabled := True;
  fDataComputer := TDataComputer.Create(fDatabase);
  fDataComputer.OnTerminate := OnTerminate;
  fDataComputer.DataMode := 1;
  fDataComputer.ExportLite := True;
  fDataComputer.CompareMode := CompareMode;
  fDataComputer.FileName := edtFileName.Text;
  fDataComputer.IntervalValues := IntervalValues;
  fDataComputer.CompareCrossRange := CompareCrossRange;
  fDataComputer.VertCompareSpacing := VertCompareSpacing;
  fDataComputer.VertSameValueCount := VertSameValueCount;
  fDataComputer.VertSameValueCount2 := VertSameValueCount2;
  fDataComputer.SlantCompareSpacing := SlantCompareSpacing;
  fDataComputer.SlantSameValueCount := SlantSameValueCount;
  fDataComputer.SlantSameValueCount2 := SlantSameValueCount2;
  fDataComputer.GroupCount := GroupCount;
  fDataComputer.KeepCodeNameValueCount := KeepCodeNameValueCount;
  fDataComputer.ExportCodeNameValueCount := ExportCodeNameValueCount;
  fDataComputer.ExportCodeNameValueCount2 := ExportCodeNameValueCount2;
  fKeyValue.GetKeyValue('GroupNumber', v);
  if not VarIsEmpty(v) then fDataComputer.GroupNumber := v;
  fKeyValue.GetKeyValue('GroupNumber2', v);
  if not VarIsEmpty(v) then fDataComputer.GroupNumber2 := v;
  fKeyValue.GetKeyValue('GroupFirstNumberCount', v);
  if not VarIsEmpty(v) then fDataComputer.GroupFirstNumberCount := v;
  fKeyValue.GetKeyValue('GroupCountEachFirstNumber', v);
  if not VarIsEmpty(v) then fDataComputer.GroupCountEachFirstNumber := v;
  fKeyValue.GetKeyValue('GroupNumber3', v);
  if not VarIsEmpty(v) then fDataComputer.GroupNumber3 := v;
  fKeyValue.GetKeyValue('GroupNumber4', v);
  if not VarIsEmpty(v) then fDataComputer.GroupNumber4 := v;
  fDataComputer.BuildValidityCountEachGroupNumber;
  fKeyValue.GetKeyValue('KeepExportCodeNameValueCount', v);
  if not VarIsEmpty(v) then fDataComputer.KeepExportCodeNameValueCount := v;
  fKeyValue.GetKeyValue('KeepLastBatchCodeNameOnEachComputer', v);
  if not VarIsEmpty(v) then fDataComputer.KeepLastBatchCodeNameOnEachComputer := v;
  fKeyValue.GetKeyValue('ExportFile', v);
  if not VarIsEmpty(v) then fDataComputer.ExportFile := v;
  fKeyValue.GetKeyValue('ExportFile2', v);
  if not VarIsEmpty(v) then fDataComputer.ExportFile2 := v;
  fKeyValue.GetKeyValue('ExportFile3', v);
  if not VarIsEmpty(v) then fDataComputer.ExportFile3 := v;
  fKeyValue.GetKeyValue('ExportFile4', v);
  if not VarIsEmpty(v) then fDataComputer.ExportFile4 := v;
  if (Sender = btnVertCompare2) or (Sender = btnSlantCompare2) or (Sender = btnVertSlantCompare2) then
  begin
    fDataComputer.ExportFile := True;
    fDataComputer.ExportFile2 := False;
    fDataComputer.ExportFile3 := False;
    fDataComputer.ExportFile4 := False;
  end;
  fDataComputer.Start;
end;

end.
