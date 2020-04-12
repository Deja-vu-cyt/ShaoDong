unit ufrmMain;

interface

uses
  uDataComputer,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.JSON.Serializers,
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
    edtFileDirectory: TEdit;
    edtIntervalValue: TEdit;
    edtIntervalValue2: TEdit;
    btnCompare: TButton;
    Timer: TTimer;
    btnExportSettings2: TButton;
    btnExportSettings5: TButton;
    btnExportSettings6: TButton;
    btnExportSettings: TButton;
    btnExportSettings3: TButton;
    btnExportSettings4: TButton;
    btnExportSettings7: TButton;
    btnExportSettings8: TButton;
    btnExportSettings9: TButton;
    btnExportSettings10: TButton;
    btnExportSettings11: TButton;
    chkExportFile254: TCheckBox;
    chkExportFile255: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure edtFileDirectoryClick(Sender: TObject);
    procedure btnCompareClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure btnExportSettings2Click(Sender: TObject);
    procedure btnExportSettingsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnExportSettings3Click(Sender: TObject);
    procedure btnExportSettings4Click(Sender: TObject);
    procedure btnExportSettings5Click(Sender: TObject);
    procedure btnExportSettings6Click(Sender: TObject);
    procedure btnExportSettings7Click(Sender: TObject);
    procedure btnExportSettings8Click(Sender: TObject);
    procedure btnExportSettings9Click(Sender: TObject);
    procedure btnExportSettings10Click(Sender: TObject);
    procedure btnExportSettings11Click(Sender: TObject);
  private
    fSerializer: TJsonSerializer;
    function CheckIntervalValues(var IntervalValues: TWordDynArray): Boolean;
    procedure OnStateChange(Working: Boolean);
    procedure OnTerminate(Sender: TObject);
  public

  end;

var
  frmMain: TfrmMain;

implementation

uses
  uControlHelper, ufrmExportSettings, ufrmExportSettings2, ufrmExportSettings3,
  ufrmExportSettings4, ufrmExportSettings5, ufrmExportSettings6, ufrmExportSettings7,
  ufrmExportSettings8, ufrmExportSettings9, ufrmExportSettings10, ufrmExportSettings11;

{$R *.dfm}

procedure TfrmMain.btnExportSettingsClick(Sender: TObject);
begin
  frmExportSettings.ShowModal;
end;

procedure TfrmMain.btnExportSettings10Click(Sender: TObject);
begin
  frmExportSettings10.ShowModal;
end;

procedure TfrmMain.btnExportSettings11Click(Sender: TObject);
begin
  frmExportSettings11.ShowModal;
end;

procedure TfrmMain.btnExportSettings2Click(Sender: TObject);
begin
  frmExportSettings2.ShowModal;
end;

procedure TfrmMain.btnExportSettings3Click(Sender: TObject);
begin
  frmExportSettings3.ShowModal;
end;

procedure TfrmMain.btnExportSettings4Click(Sender: TObject);
begin
  frmExportSettings4.ShowModal;
end;

procedure TfrmMain.btnExportSettings5Click(Sender: TObject);
begin
  frmExportSettings5.ShowModal;
end;

procedure TfrmMain.btnExportSettings6Click(Sender: TObject);
begin
  frmExportSettings6.ShowModal;
end;

procedure TfrmMain.btnExportSettings7Click(Sender: TObject);
begin
  frmExportSettings7.ShowModal;
end;

procedure TfrmMain.btnExportSettings8Click(Sender: TObject);
begin
  frmExportSettings8.ShowModal;
end;

procedure TfrmMain.btnExportSettings9Click(Sender: TObject);
begin
  frmExportSettings9.ShowModal;
end;

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
begin
  btnCompare.Enabled := not Working;
  if Assigned(frmExportSettings) then
    frmExportSettings.btnConfirm.Enabled := not Working;
  if Assigned(frmExportSettings2) then
    frmExportSettings2.btnConfirm.Enabled := not Working;
  if Assigned(frmExportSettings3) then
    frmExportSettings3.btnConfirm.Enabled := not Working;
  if Assigned(frmExportSettings4) then
    frmExportSettings4.btnConfirm.Enabled := not Working;
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

  fSerializer := TJsonSerializer.Create;

  fKeyValue.GetKeyValue(edtFileDirectory.Name, v);
  if not VarIsEmpty(v) then edtFileDirectory.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtIntervalValue.Name, v);
  if not VarIsEmpty(v) then edtIntervalValue.Text := VarToStr(v);
  fKeyValue.GetKeyValue(edtIntervalValue2.Name, v);
  if not VarIsEmpty(v) then edtIntervalValue2.Text := VarToStr(v);
  fKeyValue.GetKeyValue(chkExportFile254.Name, v);
  if not VarIsEmpty(v) then chkExportFile254.Checked := v;
  fKeyValue.GetKeyValue(chkExportFile255.Name, v);
  if not VarIsEmpty(v) then chkExportFile255.Checked := v;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  fSerializer.Free;
  frmExportSettings := nil;
  frmExportSettings2 := nil;
  frmExportSettings3 := nil;
  frmExportSettings4 := nil;
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

procedure TfrmMain.FormShow(Sender: TObject);
begin
  OnStateChange(False);
end;

procedure TfrmMain.btnCompareClick(Sender: TObject);
var
  IntervalValues: TWordDynArray;
  GroupCount, GroupCount2: Integer;
  Settings: TArray<TSettings>;
begin
  if not DirectoryExists(edtFileDirectory.Text) then
    raise Exception.Create('请选择查询文件目录');
  if not CheckIntervalValues(IntervalValues) then
    raise Exception.Create('请输入有效区域');

  SetLength(Settings, 0);
  if frmExportSettings.Settings.Flag > 0 then
  begin
    SetLength(Settings, Length(Settings) + 1);
    Settings[High(Settings)] := frmExportSettings.Settings;
  end;
  if frmExportSettings.Settings2.Flag > 0 then
  begin
    SetLength(Settings, Length(Settings) + 1);
    Settings[High(Settings)] := frmExportSettings.Settings2;
  end;
  if frmExportSettings.Settings3.Flag > 0 then
  begin
    SetLength(Settings, Length(Settings) + 1);
    Settings[High(Settings)] := frmExportSettings.Settings3;
  end;
  if frmExportSettings.Settings4.Flag > 0 then
  begin
    SetLength(Settings, Length(Settings) + 1);
    Settings[High(Settings)] := frmExportSettings.Settings4;
  end;
  if frmExportSettings.Settings5.Flag > 0 then
  begin
    SetLength(Settings, Length(Settings) + 1);
    Settings[High(Settings)] := frmExportSettings.Settings5;
  end;
  if frmExportSettings.Settings6.Flag > 0 then
  begin
    SetLength(Settings, Length(Settings) + 1);
    Settings[High(Settings)] := frmExportSettings.Settings6;
  end;
  if frmExportSettings.Settings7.Flag > 0 then
  begin
    SetLength(Settings, Length(Settings) + 1);
    Settings[High(Settings)] := frmExportSettings.Settings7;
  end;
  if frmExportSettings.Settings8.Flag > 0 then
  begin
    SetLength(Settings, Length(Settings) + 1);
    Settings[High(Settings)] := frmExportSettings.Settings8;
  end;
  if Length(frmExportSettings2.Settings.GroupCounts) > 0 then
  begin
    SetLength(Settings, Length(Settings) + 1);
    Settings[High(Settings)] := frmExportSettings2.Settings;
  end;
  if Length(frmExportSettings3.Settings.GroupCounts) > 0 then
  begin
    SetLength(Settings, Length(Settings) + 1);
    Settings[High(Settings)] := frmExportSettings3.Settings;
  end;
  if Length(frmExportSettings4.Settings.GroupCounts) > 0 then
  begin
    SetLength(Settings, Length(Settings) + 1);
    Settings[High(Settings)] := frmExportSettings4.Settings;
  end;
  if Length(frmExportSettings5.Settings.GroupCounts) > 0 then
  begin
    SetLength(Settings, Length(Settings) + 1);
    Settings[High(Settings)] := frmExportSettings5.Settings;
  end;
  if Length(frmExportSettings5.Settings2.GroupCounts) > 0 then
  begin
    SetLength(Settings, Length(Settings) + 1);
    Settings[High(Settings)] := frmExportSettings5.Settings2;
  end;
  if Length(frmExportSettings6.Settings.GroupCounts) > 0 then
  begin
    SetLength(Settings, Length(Settings) + 1);
    Settings[High(Settings)] := frmExportSettings6.Settings;
  end;
  if Length(frmExportSettings6.Settings2.GroupCounts) > 0 then
  begin
    SetLength(Settings, Length(Settings) + 1);
    Settings[High(Settings)] := frmExportSettings6.Settings2;
  end;
  if Length(frmExportSettings7.Settings.GroupCounts) > 0 then
  begin
    SetLength(Settings, Length(Settings) + 1);
    Settings[High(Settings)] := frmExportSettings7.Settings;
  end;
  if Length(frmExportSettings7.Settings2.GroupCounts) > 0 then
  begin
    SetLength(Settings, Length(Settings) + 1);
    Settings[High(Settings)] := frmExportSettings7.Settings2;
  end;
  if Length(frmExportSettings8.Settings.GroupCounts) > 0 then
  begin
    SetLength(Settings, Length(Settings) + 1);
    Settings[High(Settings)] := frmExportSettings8.Settings;
  end;
  if Length(frmExportSettings8.Settings2.GroupCounts) > 0 then
  begin
    SetLength(Settings, Length(Settings) + 1);
    Settings[High(Settings)] := frmExportSettings8.Settings2;
  end;
  if Length(frmExportSettings9.Settings.GroupCounts) > 0 then
  begin
    SetLength(Settings, Length(Settings) + 1);
    Settings[High(Settings)] := frmExportSettings9.Settings;
  end;
  if Length(frmExportSettings10.Settings.GroupCounts) > 0 then
  begin
    SetLength(Settings, Length(Settings) + 1);
    Settings[High(Settings)] := frmExportSettings10.Settings;
  end;
  if Length(frmExportSettings11.Settings.GroupCounts) > 0 then
  begin
    SetLength(Settings, Length(Settings) + 1);
    Settings[High(Settings)] := frmExportSettings11.Settings;
  end;
  if (Length(Settings) = 0)
    and not frmExportSettings.ExportOneRowSpacingFile
    and not frmExportSettings.ExportOnlyOneRowSpacingFile
  then
    raise Exception.Create('请设置查询条件');

  fKeyValue.SetKeyValue(edtFileDirectory.Name, edtFileDirectory.Text);
  fKeyValue.SetKeyValue(edtIntervalValue.Name, edtIntervalValue.Text);
  fKeyValue.SetKeyValue(edtIntervalValue2.Name, edtIntervalValue2.Text);
  fKeyValue.SetKeyValue(chkExportFile254.Name, chkExportFile254.Checked);
  fKeyValue.SetKeyValue(chkExportFile255.Name, chkExportFile255.Checked);

  OnStateChange(True);
  Timer.Enabled := True;

  fDataComputer := TDataComputer.Create;
  fDataComputer.OnTerminate := OnTerminate;
  fDataComputer.IntervalValues := IntervalValues;
  fDataComputer.SourceFileDirectory := edtFileDirectory.Text;
  fDataComputer.ExportDirectory := TPath.GetDirectoryName(fDataComputer.SourceFileDirectory);
  fDataComputer.DeleteProcessed := frmExportSettings.DeleteProcessed;
  fDataComputer.Settings := Settings;
  fDataComputer.VertCompareSpacing := frmExportSettings5.CompareSpacing;
  fDataComputer.VertSameValueCount := frmExportSettings5.SameValueCount;
  fDataComputer.VertSameValueCount2 := frmExportSettings5.SameValueCount2;
  fDataComputer.CompareCrossRange := True;
  fDataComputer.SlantCompareSpacing := frmExportSettings6.CompareSpacing;
  fDataComputer.SlantSameValueCount := frmExportSettings6.SameValueCount;
  fDataComputer.SlantSameValueCount2 := frmExportSettings6.SameValueCount2;
  fDataComputer.ExportFile254 := chkExportFile254.Checked;
  fDataComputer.ExportFile255 := chkExportFile255.Checked;
  fDataComputer.ExportOneRowSpacingFile := frmExportSettings.ExportOneRowSpacingFile;
  fDataComputer.ExportOnlyOneRowSpacingFile := frmExportSettings.ExportOnlyOneRowSpacingFile;
  fDataComputer.TakePartRow := frmExportSettings.TakePartRow;
  fDataComputer.RowRange := frmExportSettings.RowRange;
  fDataComputer.RowRange2 := frmExportSettings.RowRange2;
  fDataComputer.RowRange3 := frmExportSettings.RowRange3;
  fDataComputer.RowRange4 := frmExportSettings.RowRange4;
  fDataComputer.Start;
end;

end.
