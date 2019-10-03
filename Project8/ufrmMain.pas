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
    btnExportSettings: TButton;
    btnExportSettings2: TButton;
    btnExportSettings3: TButton;
    btnExportSettings4: TButton;
    procedure FormCreate(Sender: TObject);
    procedure edtFileDirectoryClick(Sender: TObject);
    procedure btnCompareClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure btnExportSettingsClick(Sender: TObject);
    procedure btnExportSettings2Click(Sender: TObject);
    procedure btnExportSettings3Click(Sender: TObject);
    procedure btnExportSettings4Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
  ufrmExportSettings4;

{$R *.dfm}

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

procedure TfrmMain.btnExportSettingsClick(Sender: TObject);
begin
  frmExportSettings.ShowModal;
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
begin
  if not DirectoryExists(edtFileDirectory.Text) then
    raise Exception.Create('请选择查询文件目录');
  if not CheckIntervalValues(IntervalValues) then
    raise Exception.Create('请输入有效区域');

  if (Length(frmExportSettings.GroupCounts) = 0)
    and (Length(frmExportSettings2.GroupCounts) = 0)
    and (Length(frmExportSettings3.GroupCounts) = 0)
    and (Length(frmExportSettings4.ExportFiles) = 0)
  then
    raise Exception.Create('请设置查询条件');

  fKeyValue.SetKeyValue(edtFileDirectory.Name, edtFileDirectory.Text);
  fKeyValue.SetKeyValue(edtIntervalValue.Name, edtIntervalValue.Text);
  fKeyValue.SetKeyValue(edtIntervalValue2.Name, edtIntervalValue2.Text);

  OnStateChange(True);
  Timer.Enabled := True;

  fDataComputer := TDataComputer.Create;
  fDataComputer.OnTerminate := OnTerminate;
  fDataComputer.IntervalValues := IntervalValues;
  fDataComputer.SourceFileDirectory := edtFileDirectory.Text;
  fDataComputer.ExportDirectory := TPath.GetDirectoryName(fDataComputer.SourceFileDirectory);

  fDataComputer.GroupCounts := frmExportSettings.GroupCounts;
  fDataComputer.ExportFile := frmExportSettings.ExportFile;
  fDataComputer.ExportFile2 := frmExportSettings.ExportFile2;
  fDataComputer.ExportFile3 := frmExportSettings.ExportFile3;
  fDataComputer.ExportCodeNameCount := frmExportSettings.ExportCodeNameCount;
  fDataComputer.ExportCodeNameCount2 := frmExportSettings.ExportCodeNameCount2;
  fDataComputer.ExportCodeNameCount3 := frmExportSettings.ExportCodeNameCount3;

  fDataComputer.VertCompareSpacing := frmExportSettings2.CompareSpacing;
  fDataComputer.VertSameValueCount := frmExportSettings2.SameValueCount;
  fDataComputer.VertSameValueCount2 := frmExportSettings2.SameValueCount2;
  fDataComputer.VertGroupCounts := frmExportSettings2.GroupCounts;
  fDataComputer.VertExportFile := frmExportSettings2.ExportFile;
  fDataComputer.VertExportFile2 := frmExportSettings2.ExportFile2;
  fDataComputer.VertExportFile3 := frmExportSettings2.ExportFile3;
  fDataComputer.VertExportCodeNameCount := frmExportSettings2.ExportCodeNameCount;
  fDataComputer.VertExportCodeNameCount2 := frmExportSettings2.ExportCodeNameCount2;
  fDataComputer.VertExportCodeNameCount3 := frmExportSettings2.ExportCodeNameCount3;

  fDataComputer.CompareCrossRange := True;
  fDataComputer.SlantCompareSpacing := frmExportSettings3.CompareSpacing;
  fDataComputer.SlantSameValueCount := frmExportSettings3.SameValueCount;
  fDataComputer.SlantSameValueCount2 := frmExportSettings3.SameValueCount2;
  fDataComputer.SlantGroupCounts := frmExportSettings3.GroupCounts;
  fDataComputer.SlantExportFile := frmExportSettings3.ExportFile;
  fDataComputer.SlantExportFile2 := frmExportSettings3.ExportFile2;
  fDataComputer.SlantExportFile3 := frmExportSettings3.ExportFile3;
  fDataComputer.SlantExportCodeNameCount := frmExportSettings3.ExportCodeNameCount;
  fDataComputer.SlantExportCodeNameCount2 := frmExportSettings3.ExportCodeNameCount2;
  fDataComputer.SlantExportCodeNameCount3 := frmExportSettings3.ExportCodeNameCount3;

  fDataComputer.ExportFiles := frmExportSettings4.ExportFiles;
  fDataComputer.DeleteProcessed := frmExportSettings4.DeleteProcessed;

  fDataComputer.Start;
end;

end.
