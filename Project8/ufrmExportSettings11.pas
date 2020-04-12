unit ufrmExportSettings11;

interface

uses
  uDataComputer,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.Types, System.IOUtils, System.Diagnostics,
  Vcl.Controls, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.FileCtrl,
  Vcl.CheckLst, Vcl.ComCtrls, Vcl.Menus, Vcl.Forms;

type
  TfrmExportSettings11 = class(TForm)
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
    Label1: TLabel;
    edtValueCount3: TEdit;
    edtValueCount4: TEdit;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnConfirmClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
  private
    fSettings: TSettings;
    function CheckValueCount(var ValueCount, ValueCount2: Integer): Boolean;
    function CheckValueCount2(var ValueCount, ValueCount2: Integer): Boolean;
  public
    property Settings: TSettings read fSettings;
  end;

var
  frmExportSettings11: TfrmExportSettings11;

implementation

uses
  uControlHelper, uCommon;

{$R *.dfm}

function TfrmExportSettings11.CheckValueCount(var ValueCount, ValueCount2: Integer): Boolean;
begin
  Result := edtValueCount.TryToValue(ValueCount);
  if not Result then Exit;
  Result := edtValueCount2.TryToValue(ValueCount2);
  if not Result then Exit;
  if not ((ValueCount = 0) and (ValueCount2 = 0)) then
    Result := (ValueCount > 0) and (ValueCount2 >= ValueCount);
end;

function TfrmExportSettings11.CheckValueCount2(var ValueCount, ValueCount2: Integer): Boolean;
begin
  Result := edtValueCount3.TryToValue(ValueCount);
  if not Result then Exit;
  Result := edtValueCount4.TryToValue(ValueCount2);
  if not Result then Exit;
  Result := ValueCount in [0..9];
end;

procedure TfrmExportSettings11.FormCreate(Sender: TObject);
var
  v: Variant;
  i: Integer;
begin
  ScrollBox1.VertScrollBar.Position := 0;

  fSettings.Flag := 22;
  fSettings.ExportFile := True;
  fSettings.ExportFile2 := True;
  fSettings.ExportFile3 := True;
  fSettings.ExportFile4 := True;
  fSettings.ExportCodeNameCount := 0;
  fSettings.ExportCodeNameCount2 := 0;
  fSettings.ExportCodeNameCount3 := 0;

  fKeyValue.GetKeyValue('Settings22', v);
  if not VarIsEmpty(v) then fSettings := fSerializer.Deserialize<TSettings>(v);
end;

procedure TfrmExportSettings11.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position - WheelDelta div 2;
end;

procedure TfrmExportSettings11.FormShow(Sender: TObject);
begin
  edtValueCount.Text := '';
  edtValueCount2.Text := '';

  if Length(fSettings.GroupCounts) > 0 then
  begin
    edtValueCount.Text := fSettings.GroupCounts[0].Value.ToString;
    edtValueCount2.Text := fSettings.GroupCounts[0].Value2.ToString;
    edtValueCount3.Text := fSettings.GroupCounts[0].Value3.ToString;
    edtValueCount4.Text := fSettings.GroupCounts[0].Value4.ToString;
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

procedure TfrmExportSettings11.btnConfirmClick(Sender: TObject);
var
  ValueCount, ValueCount2,
  iExportCodeNameCount, iExportCodeNameCount2, iExportCodeNameCount3: Integer;
  gc: TGroupCount;
  gcs, gcs2, gcs3: TArray<TGroupCount>;
begin
  if not CheckValueCount(ValueCount, ValueCount2) then
    raise Exception.Create('请输入有效相同个数范围');
  if ValueCount > 0 then
  begin
    gc.Number := 1;
    gc.Value := ValueCount;
    gc.Value2 := ValueCount2;
    gc.Value3 := 0;
    gc.Value4 := 0;

    SetLength(gcs, 1);
    gcs[0] := gc;
  end;
  if Length(gcs) > 0 then
  begin
    if not CheckValueCount2(ValueCount, ValueCount2) then
      raise Exception.Create('请输入有效个位范围');
    gcs[0].Value3 := ValueCount;
    gcs[0].Value4 := ValueCount2;
  end;
  if not edtExportCodeNameCount.TryToValue(iExportCodeNameCount) then
    raise Exception.Create('请输入有效导出代号数');
  if not edtExportCodeNameCount2.TryToValue(iExportCodeNameCount2) then
    raise Exception.Create('请输入有效导出代号数');
  if not edtExportCodeNameCount3.TryToValue(iExportCodeNameCount3) then
    raise Exception.Create('请输入有效导出代号数');

  fSettings.Flag := 22;
  fSettings.GroupCounts := gcs;
  fSettings.ExportFile := chkExportFile.Checked;
  fSettings.ExportFile2 := chkExportFile2.Checked;
  fSettings.ExportFile3 := chkExportFile3.Checked;
  fSettings.ExportFile4 := chkExportFile4.Checked;
  fSettings.ExportCodeNameCount := iExportCodeNameCount;
  fSettings.ExportCodeNameCount2 := iExportCodeNameCount2;
  fSettings.ExportCodeNameCount3 := iExportCodeNameCount3;

  fKeyValue.SetKeyValue('Settings22', fSerializer.Serialize(fSettings));

  ModalResult := mrOk;
end;

end.
