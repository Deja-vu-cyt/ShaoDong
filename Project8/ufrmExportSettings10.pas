unit ufrmExportSettings10;

interface

uses
  uDataComputer,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.Types, System.IOUtils, System.Diagnostics,
  Vcl.Controls, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.FileCtrl,
  Vcl.CheckLst, Vcl.ComCtrls, Vcl.Menus, Vcl.Forms;

type
  TfrmExportSettings10 = class(TForm)
    ScrollBox1: TScrollBox;
    btnConfirm: TButton;
    Label5: TLabel;
    edtValueCount: TEdit;
    edtValueCount2: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    chkExportFile: TCheckBox;
    chkExportFile2: TCheckBox;
    chkExportFile3: TCheckBox;
    edtExportCodeNameCount: TEdit;
    edtExportCodeNameCount2: TEdit;
    edtExportCodeNameCount3: TEdit;
    chkExportFile4: TCheckBox;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label15: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnConfirmClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
  private
    fSettings: TSettings;
    function CheckValueCount(var ValueCount, ValueCount2: Integer): Boolean;
  public
    property Settings: TSettings read fSettings;
  end;

var
  frmExportSettings10: TfrmExportSettings10;

implementation

uses
  uControlHelper, uCommon;

{$R *.dfm}

function TfrmExportSettings10.CheckValueCount(var ValueCount, ValueCount2: Integer): Boolean;
begin
  Result := edtValueCount.TryToValue(ValueCount, -1);
  if not Result then Exit;
  Result := edtValueCount2.TryToValue(ValueCount2, -1);
  if not Result then Exit;
  if not ((ValueCount = -1) and (ValueCount2 = -1)) then
    Result := (ValueCount > -1) and (ValueCount2 >= ValueCount);
end;

procedure TfrmExportSettings10.FormCreate(Sender: TObject);
var
  v: Variant;
  i: Integer;
begin
  ScrollBox1.VertScrollBar.Position := 0;

  fSettings.Flag := 21;
  fSettings.ExportFile := True;
  fSettings.ExportFile2 := True;
  fSettings.ExportFile3 := True;
  fSettings.ExportFile4 := True;
  fSettings.ExportCodeNameCount := 0;
  fSettings.ExportCodeNameCount2 := 0;
  fSettings.ExportCodeNameCount3 := 0;

  fKeyValue.GetKeyValue('Settings21', v);
  if not VarIsEmpty(v) then fSettings := fSerializer.Deserialize<TSettings>(v);
end;

procedure TfrmExportSettings10.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position - WheelDelta div 2;
end;

procedure TfrmExportSettings10.FormShow(Sender: TObject);
begin
  edtValueCount.Text := '';
  edtValueCount2.Text := '';

  if Length(fSettings.GroupCounts) > 0 then
  begin
    edtValueCount.Text := fSettings.GroupCounts[0].Value.ToString;
    edtValueCount2.Text := fSettings.GroupCounts[0].Value2.ToString;
  end;
  chkExportFile.Checked := fSettings.ExportFile;
  chkExportFile2.Checked := fSettings.ExportFile2;
  chkExportFile3.Checked := fSettings.ExportFile3;
  chkExportFile4.Checked := fSettings.ExportFile4;
  if fSettings.ExportCodeNameCount > 0 then
    edtExportCodeNameCount.Text := fSettings.ExportCodeNameCount.ToString;
  if fSettings.ExportCodeNameCount2 > 0 then
    edtExportCodeNameCount2.Text := fSettings.ExportCodeNameCount2.ToString;
  if fSettings.ExportCodeNameCount3 > 0 then
    edtExportCodeNameCount3.Text := fSettings.ExportCodeNameCount3.ToString;
end;

procedure TfrmExportSettings10.btnConfirmClick(Sender: TObject);
var
  ValueCount, ValueCount2,
  iExportCodeNameCount, iExportCodeNameCount2, iExportCodeNameCount3: Integer;
  gc: TGroupCount;
  gcs, gcs2, gcs3: TArray<TGroupCount>;
begin
  if not CheckValueCount(ValueCount, ValueCount2) then
    raise Exception.Create('请输入有效相同个数范围');
  if ValueCount > -1 then
  begin
    gc.Number := 1;
    gc.Value := ValueCount;
    gc.Value2 := ValueCount2;
    gc.Value3 := 0;
    gc.Value4 := 0;

    SetLength(gcs, 1);
    gcs[0] := gc;
  end;
  if not edtExportCodeNameCount.TryToValue(iExportCodeNameCount) then
    raise Exception.Create('请输入有效导出代号数');
  if not edtExportCodeNameCount2.TryToValue(iExportCodeNameCount2) then
    raise Exception.Create('请输入有效导出代号数');
  if not edtExportCodeNameCount3.TryToValue(iExportCodeNameCount3) then
    raise Exception.Create('请输入有效导出代号数');

  fSettings.Flag := 21;
  fSettings.GroupCounts := gcs;
  fSettings.ExportFile := chkExportFile.Checked;
  fSettings.ExportFile2 := chkExportFile2.Checked;
  fSettings.ExportFile3 := chkExportFile3.Checked;
  fSettings.ExportFile4 := chkExportFile4.Checked;
  fSettings.ExportCodeNameCount := iExportCodeNameCount;
  fSettings.ExportCodeNameCount2 := iExportCodeNameCount2;
  fSettings.ExportCodeNameCount3 := iExportCodeNameCount3;

  fKeyValue.SetKeyValue('Settings21', fSerializer.Serialize(fSettings));

  ModalResult := mrOk;
end;

end.
