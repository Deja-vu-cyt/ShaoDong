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
    edtFileName: TEdit;
    edtIntervalValue: TEdit;
    edtIntervalValue2: TEdit;
    Label2: TLabel;
    edtFileName2: TEdit;
    btnCompare: TButton;
    Label5: TLabel;
    Label6: TLabel;
    edtMinSameValueCount: TEdit;
    edtMinSameValueCount2: TEdit;
    Label7: TLabel;
    edtMinSameValueCount3: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure edtFileNameClick(Sender: TObject);
    procedure btnCompareClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    function CheckIntervalValues(var IntervalValues: TWordDynArray): Boolean;
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

procedure TfrmMain.edtFileNameClick(Sender: TObject);
begin
  if not OpenDialog.Execute then Exit;
  TEdit(Sender).Text := OpenDialog.FileName;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  OnStateChange(False);
  {fDataComputer := TDataComputer.Create;
  fDataComputer.OnTerminate := OnTerminate;
  fDataComputer.IntervalValues := [35, 12];
  fDataComputer.SourceFileName := 'D:\2.txt';
  fDataComputer.CompareFileName := 'D:\3.txt';
  fDataComputer.ExportDirectory := TPath.GetDirectoryName(fDataComputer.SourceFileName);
  fDataComputer.MinSameValueCount := 2;
  fDataComputer.MinSameValueCount2 := 0;
  fDataComputer.MinSameValueCount3 := 1;
  fDataComputer.Start;}
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
  MinSameValueCount, MinSameValueCount2, MinSameValueCount3: Integer;
begin
  if not CheckIntervalValues(IntervalValues) then
    raise Exception.Create('请输入有效区域');
  MinSameValueCount := 0;
  if not edtMinSameValueCount.TryToValue(MinSameValueCount) then
    raise Exception.Create('请输入有效数值');
  if not edtMinSameValueCount2.TryToValue(MinSameValueCount2) then
    raise Exception.Create('请输入有效数值');
  if not edtMinSameValueCount3.TryToValue(MinSameValueCount3) then
    raise Exception.Create('请输入有效数值');

  if Assigned(fDataComputer) then FreeAndNil(fDataComputer);

  OnStateChange(True);
  Timer.Enabled := True;

  fDataComputer := TDataComputer.Create;
  fDataComputer.OnTerminate := OnTerminate;
  fDataComputer.IntervalValues := IntervalValues;
  fDataComputer.SourceFileName := edtFileName.Text;
  fDataComputer.CompareFileName := edtFileName2.Text;
  fDataComputer.ExportDirectory := TPath.GetDirectoryName(fDataComputer.SourceFileName);
  fDataComputer.MinSameValueCount := MinSameValueCount;
  fDataComputer.MinSameValueCount2 := MinSameValueCount2;
  fDataComputer.MinSameValueCount3 := MinSameValueCount3;
  fDataComputer.Start;
end;

end.
