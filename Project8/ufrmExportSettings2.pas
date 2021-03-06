unit ufrmExportSettings2;

interface

uses
  uDataComputer,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.Types, System.IOUtils, System.Diagnostics,
  Vcl.Controls, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.FileCtrl,
  Vcl.CheckLst, Vcl.ComCtrls, Vcl.Menus, Vcl.Forms;

type
  TfrmExportSettings2 = class(TForm)
    ScrollBox1: TScrollBox;
    Label5: TLabel;
    edtColumnGroupCount: TEdit;
    edtColumnGroupCount2: TEdit;
    btnConfirm: TButton;
    Label1: TLabel;
    chkExportFile: TCheckBox;
    chkExportFile2: TCheckBox;
    chkExportFile3: TCheckBox;
    edtExportCodeNameCount: TEdit;
    edtExportCodeNameCount2: TEdit;
    edtExportCodeNameCount3: TEdit;
    Label3: TLabel;
    Label15: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnConfirmClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
  private
    fSettings: TSettings;
    function CheckColumnGroupCount(var ValueCount, ValueCount2: Integer): Boolean;
  public
    property Settings: TSettings read fSettings;
  end;

var
  frmExportSettings2: TfrmExportSettings2;

implementation

uses
  uControlHelper;

{$R *.dfm}

function TfrmExportSettings2.CheckColumnGroupCount(var ValueCount, ValueCount2: Integer): Boolean;
begin
  Result := edtColumnGroupCount.TryToValue(ValueCount);
  if not Result then Exit;
  Result := edtColumnGroupCount2.TryToValue(ValueCount2);
  if not Result then Exit;
  if not ((ValueCount = 0) and (ValueCount2 = 0)) then
    Result := (ValueCount > 0) and (ValueCount2 >= ValueCount);
end;

procedure TfrmExportSettings2.FormCreate(Sender: TObject);
var
  v: Variant;
begin
  ScrollBox1.VertScrollBar.Position := 0;

  fSettings.Flag := 9;
  fSettings.ExportFile := True;
  fSettings.ExportFile2 := True;
  fSettings.ExportFile3 := True;

  fKeyValue.GetKeyValue('Settings9', v);
  if not VarIsEmpty(v) then fSettings := fSerializer.Deserialize<TSettings>(v);
end;

procedure TfrmExportSettings2.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position - WheelDelta div 2;
end;

procedure TfrmExportSettings2.FormShow(Sender: TObject);
var
  gc: TGroupCount;
begin
  edtColumnGroupCount.Text := '';
  edtColumnGroupCount2.Text := '';

  for gc in fSettings.GroupCounts do
  begin
    if gc.Value3 > 0 then
      edtColumnGroupCount.Text := gc.Value3.ToString;
    if gc.Value4 > 0 then
      edtColumnGroupCount2.Text := gc.Value4.ToString;
  end;

  chkExportFile.Checked := fSettings.ExportFile;
  chkExportFile2.Checked := fSettings.ExportFile2;
  chkExportFile3.Checked := fSettings.ExportFile3;
  if fSettings.ExportCodeNameCount > 0 then
    edtExportCodeNameCount.Text := fSettings.ExportCodeNameCount.ToString;
  if fSettings.ExportCodeNameCount2 > 0 then
    edtExportCodeNameCount2.Text := fSettings.ExportCodeNameCount2.ToString;
  if fSettings.ExportCodeNameCount3 > 0 then
    edtExportCodeNameCount3.Text := fSettings.ExportCodeNameCount3.ToString;
end;

procedure TfrmExportSettings2.btnConfirmClick(Sender: TObject);
var
  GroupCount, GroupCount2,
  iExportCodeNameCount, iExportCodeNameCount2, iExportCodeNameCount3: Integer;
  gc: TGroupCount;
  gcs: TArray<TGroupCount>;
begin
  if not CheckColumnGroupCount(GroupCount, GroupCount2) then
    raise Exception.Create('请输入有效组合个数范围');
  if GroupCount > 0 then
  begin
    gc.Number := 1;
    gc.Value := 1;
    gc.Value2 := 1;
    gc.Value3 := GroupCount;
    gc.Value4 := GroupCount2;

    SetLength(gcs, 1);
    gcs[0] := gc;
  end;
  if not edtExportCodeNameCount.TryToValue(iExportCodeNameCount) then
    raise Exception.Create('请输入有效导出代号数');
  if not edtExportCodeNameCount2.TryToValue(iExportCodeNameCount2) then
    raise Exception.Create('请输入有效导出代号数');
  if not edtExportCodeNameCount3.TryToValue(iExportCodeNameCount3) then
    raise Exception.Create('请输入有效导出代号数');

  fSettings.Flag := 9;
  fSettings.GroupCounts := gcs;
  fSettings.ExportFile := chkExportFile.Checked;
  fSettings.ExportFile2 := chkExportFile2.Checked;
  fSettings.ExportFile3 := chkExportFile3.Checked;
  fSettings.ExportCodeNameCount := iExportCodeNameCount;
  fSettings.ExportCodeNameCount2 := iExportCodeNameCount2;
  fSettings.ExportCodeNameCount3 := iExportCodeNameCount3;

  fKeyValue.SetKeyValue('Settings9', fSerializer.Serialize(fSettings));

  ModalResult := mrOk;
end;

end.
