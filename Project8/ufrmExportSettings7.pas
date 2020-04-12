unit ufrmExportSettings7;

interface

uses
  uDataComputer,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.Types, System.IOUtils, System.Diagnostics,
  Vcl.Controls, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.FileCtrl,
  Vcl.CheckLst, Vcl.ComCtrls, Vcl.Menus, Vcl.Forms;

type
  TfrmExportSettings7 = class(TForm)
    ScrollBox1: TScrollBox;
    btnConfirm: TButton;
    Label2: TLabel;
    Label5: TLabel;
    edtValueCount: TEdit;
    edtValueCount2: TEdit;
    Label1: TLabel;
    edtValueCount3: TEdit;
    edtValueCount4: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    chkExportFile: TCheckBox;
    chkExportFile2: TCheckBox;
    chkExportFile3: TCheckBox;
    edtExportCodeNameCount: TEdit;
    edtExportCodeNameCount2: TEdit;
    edtExportCodeNameCount3: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    chkExportFile4: TCheckBox;
    chkExportFile5: TCheckBox;
    chkExportFile6: TCheckBox;
    edtExportCodeNameCount4: TEdit;
    edtExportCodeNameCount5: TEdit;
    edtExportCodeNameCount6: TEdit;
    chkExportFile7: TCheckBox;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label15: TLabel;
    Label12: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnConfirmClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
  private
    fSettings: TSettings;
    fSettings2: TSettings;
    function CheckValueCount(var ValueCount, ValueCount2: Integer): Boolean;
    function CheckValueCount2(var ValueCount, ValueCount2: Integer): Boolean;
  public
    property Settings: TSettings read fSettings;
    property Settings2: TSettings read fSettings2;
  end;

var
  frmExportSettings7: TfrmExportSettings7;

implementation

uses
  uControlHelper, uCommon;

{$R *.dfm}

function TfrmExportSettings7.CheckValueCount(var ValueCount, ValueCount2: Integer): Boolean;
begin
  Result := edtValueCount.TryToValue(ValueCount, -1);
  if not Result then Exit;
  Result := edtValueCount2.TryToValue(ValueCount2, -1);
  if not Result then Exit;
  if not ((ValueCount = -1) and (ValueCount2 = -1)) then
    Result := (ValueCount > -1) and (ValueCount2 >= ValueCount);
end;

function TfrmExportSettings7.CheckValueCount2(var ValueCount, ValueCount2: Integer): Boolean;
begin
  Result := edtValueCount3.TryToValue(ValueCount, -1);
  if not Result then Exit;
  Result := edtValueCount4.TryToValue(ValueCount2, -1);
  if not Result then Exit;
  if not ((ValueCount = -1) and (ValueCount2 = -1)) then
    Result := (ValueCount > -1) and (ValueCount2 >= ValueCount);
end;

procedure TfrmExportSettings7.FormCreate(Sender: TObject);
var
  v: Variant;
  i: Integer;
begin
  ScrollBox1.VertScrollBar.Position := 0;

  fSettings.Flag := 16;
  fSettings.ExportFile := True;
  fSettings.ExportFile2 := True;
  fSettings.ExportFile3 := True;
  fSettings.ExportFile4 := True;
  fSettings.ExportCodeNameCount := 0;
  fSettings.ExportCodeNameCount2 := 0;
  fSettings.ExportCodeNameCount3 := 0;

  fSettings2.Flag := 17;
  fSettings2.ExportFile := True;
  fSettings2.ExportFile2 := True;
  fSettings2.ExportFile3 := True;
  fSettings2.ExportFile4 := False;
  fSettings2.ExportCodeNameCount := 0;
  fSettings2.ExportCodeNameCount2 := 0;
  fSettings2.ExportCodeNameCount3 := 0;

  fKeyValue.GetKeyValue('Settings16', v);
  if not VarIsEmpty(v) then fSettings := fSerializer.Deserialize<TSettings>(v);
  fKeyValue.GetKeyValue('Settings17', v);
  if not VarIsEmpty(v) then fSettings2 := fSerializer.Deserialize<TSettings>(v);
end;

procedure TfrmExportSettings7.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position - WheelDelta div 2;
end;

procedure TfrmExportSettings7.FormShow(Sender: TObject);
begin
  edtValueCount.Text := '';
  edtValueCount2.Text := '';
  edtValueCount3.Text := '';
  edtValueCount4.Text := '';

  if Length(fSettings.GroupCounts) > 0 then
  begin
    edtValueCount.Text := fSettings.GroupCounts[0].Value.ToString;
    edtValueCount2.Text := fSettings.GroupCounts[0].Value2.ToString;
  end;
  chkExportFile.Checked := fSettings.ExportFile;
  chkExportFile2.Checked := fSettings.ExportFile2;
  chkExportFile3.Checked := fSettings.ExportFile3;
  chkExportFile7.Checked := fSettings.ExportFile4;
  if fSettings.ExportCodeNameCount > 0 then
    edtExportCodeNameCount.Text := fSettings.ExportCodeNameCount.ToString;
  if fSettings.ExportCodeNameCount2 > 0 then
    edtExportCodeNameCount2.Text := fSettings.ExportCodeNameCount2.ToString;
  if fSettings.ExportCodeNameCount3 > 0 then
    edtExportCodeNameCount3.Text := fSettings.ExportCodeNameCount3.ToString;

  if Length(fSettings2.GroupCounts) > 0 then
  begin
    edtValueCount3.Text := fSettings2.GroupCounts[0].Value.ToString;
    edtValueCount4.Text := fSettings2.GroupCounts[0].Value2.ToString;
  end;
  chkExportFile4.Checked := fSettings2.ExportFile;
  chkExportFile5.Checked := fSettings2.ExportFile2;
  chkExportFile6.Checked := fSettings2.ExportFile3;
  if fSettings2.ExportCodeNameCount > 0 then
    edtExportCodeNameCount4.Text := fSettings2.ExportCodeNameCount.ToString;
  if fSettings2.ExportCodeNameCount2 > 0 then
    edtExportCodeNameCount5.Text := fSettings2.ExportCodeNameCount2.ToString;
  if fSettings2.ExportCodeNameCount3 > 0 then
    edtExportCodeNameCount6.Text := fSettings2.ExportCodeNameCount3.ToString;
end;

procedure TfrmExportSettings7.btnConfirmClick(Sender: TObject);
var
  ValueCount, ValueCount2,
  iExportCodeNameCount, iExportCodeNameCount2, iExportCodeNameCount3,
  iExportCodeNameCount4, iExportCodeNameCount5, iExportCodeNameCount6: Integer;
  gc: TGroupCount;
  gcs, gcs2: TArray<TGroupCount>;
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

  if not CheckValueCount2(ValueCount, ValueCount2) then
    raise Exception.Create('请输入有效相同个数范围');
  if ValueCount > -1 then
  begin
    gc.Number := 1;
    gc.Value := ValueCount;
    gc.Value2 := ValueCount2;
    gc.Value3 := 0;
    gc.Value4 := 0;

    SetLength(gcs2, 1);
    gcs2[0] := gc;
  end;
  if not edtExportCodeNameCount4.TryToValue(iExportCodeNameCount4) then
    raise Exception.Create('请输入有效导出代号数');
  if not edtExportCodeNameCount5.TryToValue(iExportCodeNameCount5) then
    raise Exception.Create('请输入有效导出代号数');
  if not edtExportCodeNameCount6.TryToValue(iExportCodeNameCount6) then
    raise Exception.Create('请输入有效导出代号数');

  fSettings.Flag := 16;
  fSettings.GroupCounts := gcs;
  fSettings.ExportFile := chkExportFile.Checked;
  fSettings.ExportFile2 := chkExportFile2.Checked;
  fSettings.ExportFile3 := chkExportFile3.Checked;
  fSettings.ExportFile4 := chkExportFile7.Checked;
  fSettings.ExportCodeNameCount := iExportCodeNameCount;
  fSettings.ExportCodeNameCount2 := iExportCodeNameCount2;
  fSettings.ExportCodeNameCount3 := iExportCodeNameCount3;
  fSettings2.Flag := 17;
  fSettings2.GroupCounts := gcs2;
  fSettings2.ExportFile := chkExportFile4.Checked;
  fSettings2.ExportFile2 := chkExportFile5.Checked;
  fSettings2.ExportFile3 := chkExportFile6.Checked;
  fSettings2.ExportFile4 := False;
  fSettings2.ExportCodeNameCount := iExportCodeNameCount4;
  fSettings2.ExportCodeNameCount2 := iExportCodeNameCount5;
  fSettings2.ExportCodeNameCount3 := iExportCodeNameCount6;

  fKeyValue.SetKeyValue('Settings16', fSerializer.Serialize(fSettings));
  fKeyValue.SetKeyValue('Settings17', fSerializer.Serialize(fSettings2));

  ModalResult := mrOk;
end;

end.
