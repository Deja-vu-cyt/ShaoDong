unit ufrmExportSettings4;

interface

uses
  uDataComputer,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.Types, System.IOUtils, System.Diagnostics,
  Vcl.Controls, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.FileCtrl,
  Vcl.CheckLst, Vcl.ComCtrls, Vcl.Menus, Vcl.Forms;

type
  TfrmExportSettings4 = class(TForm)
    ScrollBox1: TScrollBox;
    btnConfirm: TButton;
    Label12: TLabel;
    edtFirstNumberGroupCount3: TEdit;
    edtFirstNumberGroupCount4: TEdit;
    Label13: TLabel;
    edtFirstNumberGroupCount7: TEdit;
    edtFirstNumberGroupCount8: TEdit;
    Label14: TLabel;
    edtFirstNumberGroupCount11: TEdit;
    edtFirstNumberGroupCount12: TEdit;
    Label16: TLabel;
    edtFirstNumberGroupCount15: TEdit;
    edtFirstNumberGroupCount16: TEdit;
    Label17: TLabel;
    edtFirstNumberGroupCount19: TEdit;
    edtFirstNumberGroupCount20: TEdit;
    Label18: TLabel;
    edtFirstNumberGroupCount23: TEdit;
    edtFirstNumberGroupCount24: TEdit;
    Label19: TLabel;
    edtFirstNumberGroupCount27: TEdit;
    edtFirstNumberGroupCount28: TEdit;
    edtFirstNumberGroupCount: TEdit;
    edtFirstNumberGroupCount2: TEdit;
    edtFirstNumberGroupCount5: TEdit;
    edtFirstNumberGroupCount6: TEdit;
    edtFirstNumberGroupCount9: TEdit;
    edtFirstNumberGroupCount10: TEdit;
    edtFirstNumberGroupCount13: TEdit;
    edtFirstNumberGroupCount14: TEdit;
    edtFirstNumberGroupCount17: TEdit;
    edtFirstNumberGroupCount18: TEdit;
    edtFirstNumberGroupCount21: TEdit;
    edtFirstNumberGroupCount22: TEdit;
    edtFirstNumberGroupCount25: TEdit;
    edtFirstNumberGroupCount26: TEdit;
    Label1: TLabel;
    chkExportFile: TCheckBox;
    chkExportFile2: TCheckBox;
    chkExportFile3: TCheckBox;
    edtExportCodeNameCount: TEdit;
    edtExportCodeNameCount2: TEdit;
    edtExportCodeNameCount3: TEdit;
    Label3: TLabel;
    Label5: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label15: TLabel;
    chkExportFile4: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btnConfirmClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
  private
    fSettings: TSettings;
    function CheckFirstNumberGroupCount(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckFirstNumberGroupCount2(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckFirstNumberGroupCount3(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckFirstNumberGroupCount4(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckFirstNumberGroupCount5(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckFirstNumberGroupCount6(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckFirstNumberGroupCount7(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
  public
    property Settings: TSettings read fSettings;
  end;

var
  frmExportSettings4: TfrmExportSettings4;

implementation

uses
  uControlHelper;

{$R *.dfm}

function TfrmExportSettings4.CheckFirstNumberGroupCount(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
begin
  Result := edtFirstNumberGroupCount.TryToValue(ValueCount);
  if not Result then Exit;
  Result := edtFirstNumberGroupCount2.TryToValue(ValueCount2);
  if not Result then Exit;
  Result := edtFirstNumberGroupCount3.TryToValue(ValueCount3);
  if not Result then Exit;
  Result := edtFirstNumberGroupCount4.TryToValue(ValueCount4);
  if not Result then Exit;
  if not ((ValueCount = 0) and (ValueCount2 = 0) and (ValueCount3 = 0) and (ValueCount4 = 0)) then
    Result := (ValueCount > 0) and (ValueCount2 >= ValueCount) and (ValueCount3 > 0) and (ValueCount4 >= ValueCount3);
end;

function TfrmExportSettings4.CheckFirstNumberGroupCount2(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
begin
  Result := edtFirstNumberGroupCount5.TryToValue(ValueCount);
  if not Result then Exit;
  Result := edtFirstNumberGroupCount6.TryToValue(ValueCount2);
  if not Result then Exit;
  Result := edtFirstNumberGroupCount7.TryToValue(ValueCount3);
  if not Result then Exit;
  Result := edtFirstNumberGroupCount8.TryToValue(ValueCount4);
  if not Result then Exit;
  if not ((ValueCount = 0) and (ValueCount2 = 0) and (ValueCount3 = 0) and (ValueCount4 = 0)) then
    Result := (ValueCount > 0) and (ValueCount2 >= ValueCount) and (ValueCount3 > 0) and (ValueCount4 >= ValueCount3);
end;

function TfrmExportSettings4.CheckFirstNumberGroupCount3(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
begin
  Result := edtFirstNumberGroupCount9.TryToValue(ValueCount);
  if not Result then Exit;
  Result := edtFirstNumberGroupCount10.TryToValue(ValueCount2);
  if not Result then Exit;
  Result := edtFirstNumberGroupCount11.TryToValue(ValueCount3);
  if not Result then Exit;
  Result := edtFirstNumberGroupCount12.TryToValue(ValueCount4);
  if not Result then Exit;
  if not ((ValueCount = 0) and (ValueCount2 = 0) and (ValueCount3 = 0) and (ValueCount4 = 0)) then
    Result := (ValueCount > 0) and (ValueCount2 >= ValueCount) and (ValueCount3 > 0) and (ValueCount4 >= ValueCount3);
end;

function TfrmExportSettings4.CheckFirstNumberGroupCount4(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
begin
  Result := edtFirstNumberGroupCount13.TryToValue(ValueCount);
  if not Result then Exit;
  Result := edtFirstNumberGroupCount14.TryToValue(ValueCount2);
  if not Result then Exit;
  Result := edtFirstNumberGroupCount15.TryToValue(ValueCount3);
  if not Result then Exit;
  Result := edtFirstNumberGroupCount16.TryToValue(ValueCount4);
  if not Result then Exit;
  if not ((ValueCount = 0) and (ValueCount2 = 0) and (ValueCount3 = 0) and (ValueCount4 = 0)) then
    Result := (ValueCount > 0) and (ValueCount2 >= ValueCount) and (ValueCount3 > 0) and (ValueCount4 >= ValueCount3);
end;

function TfrmExportSettings4.CheckFirstNumberGroupCount5(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
begin
  Result := edtFirstNumberGroupCount17.TryToValue(ValueCount);
  if not Result then Exit;
  Result := edtFirstNumberGroupCount18.TryToValue(ValueCount2);
  if not Result then Exit;
  Result := edtFirstNumberGroupCount19.TryToValue(ValueCount3);
  if not Result then Exit;
  Result := edtFirstNumberGroupCount20.TryToValue(ValueCount4);
  if not Result then Exit;
  if not ((ValueCount = 0) and (ValueCount2 = 0) and (ValueCount3 = 0) and (ValueCount4 = 0)) then
    Result := (ValueCount > 0) and (ValueCount2 >= ValueCount) and (ValueCount3 > 0) and (ValueCount4 >= ValueCount3);
end;

function TfrmExportSettings4.CheckFirstNumberGroupCount6(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
begin
  Result := edtFirstNumberGroupCount21.TryToValue(ValueCount);
  if not Result then Exit;
  Result := edtFirstNumberGroupCount22.TryToValue(ValueCount2);
  if not Result then Exit;
  Result := edtFirstNumberGroupCount23.TryToValue(ValueCount3);
  if not Result then Exit;
  Result := edtFirstNumberGroupCount24.TryToValue(ValueCount4);
  if not Result then Exit;
  if not ((ValueCount = 0) and (ValueCount2 = 0) and (ValueCount3 = 0) and (ValueCount4 = 0)) then
    Result := (ValueCount > 0) and (ValueCount2 >= ValueCount) and (ValueCount3 > 0) and (ValueCount4 >= ValueCount3);
end;

function TfrmExportSettings4.CheckFirstNumberGroupCount7(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
begin
  Result := edtFirstNumberGroupCount25.TryToValue(ValueCount);
  if not Result then Exit;
  Result := edtFirstNumberGroupCount26.TryToValue(ValueCount2);
  if not Result then Exit;
  Result := edtFirstNumberGroupCount27.TryToValue(ValueCount3);
  if not Result then Exit;
  Result := edtFirstNumberGroupCount28.TryToValue(ValueCount4);
  if not Result then Exit;
  if not ((ValueCount = 0) and (ValueCount2 = 0) and (ValueCount3 = 0) and (ValueCount4 = 0)) then
    Result := (ValueCount > 0) and (ValueCount2 >= ValueCount) and (ValueCount3 > 0) and (ValueCount4 >= ValueCount3);
end;

procedure TfrmExportSettings4.FormCreate(Sender: TObject);
var
  v: Variant;
begin
  ScrollBox1.VertScrollBar.Position := 0;

  fSettings.Flag := 11;
  fSettings.ExportFile := True;
  fSettings.ExportFile2 := True;
  fSettings.ExportFile3 := True;
  fSettings.ExportFile4 := True;
  fSettings.ExportCodeNameCount := 0;
  fSettings.ExportCodeNameCount2 := 0;
  fSettings.ExportCodeNameCount3 := 0;

  fKeyValue.GetKeyValue('Settings11', v);
  if not VarIsEmpty(v) then fSettings := fSerializer.Deserialize<TSettings>(v);
end;

procedure TfrmExportSettings4.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position - WheelDelta div 2;
end;

procedure TfrmExportSettings4.FormShow(Sender: TObject);
var
  gc: TGroupCount;
begin
  edtFirstNumberGroupCount.Text := '';
  edtFirstNumberGroupCount2.Text := '';
  edtFirstNumberGroupCount3.Text := '';
  edtFirstNumberGroupCount4.Text := '';
  edtFirstNumberGroupCount5.Text := '';
  edtFirstNumberGroupCount6.Text := '';
  edtFirstNumberGroupCount7.Text := '';
  edtFirstNumberGroupCount8.Text := '';
  edtFirstNumberGroupCount9.Text := '';
  edtFirstNumberGroupCount10.Text := '';
  edtFirstNumberGroupCount11.Text := '';
  edtFirstNumberGroupCount12.Text := '';
  edtFirstNumberGroupCount13.Text := '';
  edtFirstNumberGroupCount14.Text := '';
  edtFirstNumberGroupCount15.Text := '';
  edtFirstNumberGroupCount16.Text := '';
  edtFirstNumberGroupCount17.Text := '';
  edtFirstNumberGroupCount18.Text := '';
  edtFirstNumberGroupCount19.Text := '';
  edtFirstNumberGroupCount20.Text := '';
  edtFirstNumberGroupCount21.Text := '';
  edtFirstNumberGroupCount22.Text := '';
  edtFirstNumberGroupCount23.Text := '';
  edtFirstNumberGroupCount24.Text := '';
  edtFirstNumberGroupCount25.Text := '';
  edtFirstNumberGroupCount26.Text := '';
  edtFirstNumberGroupCount27.Text := '';
  edtFirstNumberGroupCount28.Text := '';

  for gc in fSettings.GroupCounts do
    case gc.Number of
      3:
      begin
        if gc.Value > 0 then
          edtFirstNumberGroupCount.Text := gc.Value.ToString;
        if gc.Value2 > 0 then
          edtFirstNumberGroupCount2.Text := gc.Value2.ToString;
        if gc.Value3 > 0 then
          edtFirstNumberGroupCount3.Text := gc.Value3.ToString;
        if gc.Value4 > 0 then
          edtFirstNumberGroupCount4.Text := gc.Value4.ToString;
      end;
      4:
      begin
        if gc.Value > 0 then
          edtFirstNumberGroupCount5.Text := gc.Value.ToString;
        if gc.Value2 > 0 then
          edtFirstNumberGroupCount6.Text := gc.Value2.ToString;
        if gc.Value3 > 0 then
          edtFirstNumberGroupCount7.Text := gc.Value3.ToString;
        if gc.Value4 > 0 then
          edtFirstNumberGroupCount8.Text := gc.Value4.ToString;
      end;
      5:
      begin
        if gc.Value > 0 then
          edtFirstNumberGroupCount9.Text := gc.Value.ToString;
        if gc.Value2 > 0 then
          edtFirstNumberGroupCount10.Text := gc.Value2.ToString;
        if gc.Value3 > 0 then
          edtFirstNumberGroupCount11.Text := gc.Value3.ToString;
        if gc.Value4 > 0 then
          edtFirstNumberGroupCount12.Text := gc.Value4.ToString;
      end;
      6:
      begin
        if gc.Value > 0 then
          edtFirstNumberGroupCount13.Text := gc.Value.ToString;
        if gc.Value2 > 0 then
          edtFirstNumberGroupCount14.Text := gc.Value2.ToString;
        if gc.Value3 > 0 then
          edtFirstNumberGroupCount15.Text := gc.Value3.ToString;
        if gc.Value4 > 0 then
          edtFirstNumberGroupCount16.Text := gc.Value4.ToString;
      end;
      7:
      begin
        if gc.Value > 0 then
          edtFirstNumberGroupCount17.Text := gc.Value.ToString;
        if gc.Value2 > 0 then
          edtFirstNumberGroupCount18.Text := gc.Value2.ToString;
        if gc.Value3 > 0 then
          edtFirstNumberGroupCount19.Text := gc.Value3.ToString;
        if gc.Value4 > 0 then
          edtFirstNumberGroupCount20.Text := gc.Value4.ToString;
      end;
      8:
      begin
        if gc.Value > 0 then
          edtFirstNumberGroupCount21.Text := gc.Value.ToString;
        if gc.Value2 > 0 then
          edtFirstNumberGroupCount22.Text := gc.Value2.ToString;
        if gc.Value3 > 0 then
          edtFirstNumberGroupCount23.Text := gc.Value3.ToString;
        if gc.Value4 > 0 then
          edtFirstNumberGroupCount24.Text := gc.Value4.ToString;
      end;
      9:
      begin
        if gc.Value > 0 then
          edtFirstNumberGroupCount25.Text := gc.Value.ToString;
        if gc.Value2 > 0 then
          edtFirstNumberGroupCount26.Text := gc.Value2.ToString;
        if gc.Value3 > 0 then
          edtFirstNumberGroupCount27.Text := gc.Value3.ToString;
        if gc.Value4 > 0 then
          edtFirstNumberGroupCount28.Text := gc.Value4.ToString;
      end;
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

procedure TfrmExportSettings4.btnConfirmClick(Sender: TObject);
var
  GroupCount, GroupCount2, GroupCount3, GroupCount4,
  iExportCodeNameCount, iExportCodeNameCount2, iExportCodeNameCount3: Integer;
  gc: TGroupCount;
  gcs: TArray<TGroupCount>;
begin
  SetLength(gcs, 0);
  if not CheckFirstNumberGroupCount(GroupCount, GroupCount2, GroupCount3, GroupCount4) then
    raise Exception.Create('请输入有效组合个数范围');
  if GroupCount > 0 then
  begin
    gc.Number := 3;
    gc.Value := GroupCount;
    gc.Value2 := GroupCount2;
    gc.Value3 := GroupCount3;
    gc.Value4 := GroupCount4;

    SetLength(gcs, Length(gcs) + 1);
    gcs[High(gcs)] := gc;
  end;
  if not CheckFirstNumberGroupCount2(GroupCount, GroupCount2, GroupCount3, GroupCount4) then
    raise Exception.Create('请输入有效组合个数范围');
  if GroupCount > 0 then
  begin
    gc.Number := 4;
    gc.Value := GroupCount;
    gc.Value2 := GroupCount2;
    gc.Value3 := GroupCount3;
    gc.Value4 := GroupCount4;

    SetLength(gcs, Length(gcs) + 1);
    gcs[High(gcs)] := gc;
  end;
  if not CheckFirstNumberGroupCount3(GroupCount, GroupCount2, GroupCount3, GroupCount4) then
    raise Exception.Create('请输入有效组合个数范围');
  if GroupCount > 0 then
  begin
    gc.Number := 5;
    gc.Value := GroupCount;
    gc.Value2 := GroupCount2;
    gc.Value3 := GroupCount3;
    gc.Value4 := GroupCount4;

    SetLength(gcs, Length(gcs) + 1);
    gcs[High(gcs)] := gc;
  end;
  if not CheckFirstNumberGroupCount4(GroupCount, GroupCount2, GroupCount3, GroupCount4) then
    raise Exception.Create('请输入有效组合个数范围');
  if GroupCount > 0 then
  begin
    gc.Number := 6;
    gc.Value := GroupCount;
    gc.Value2 := GroupCount2;
    gc.Value3 := GroupCount3;
    gc.Value4 := GroupCount4;

    SetLength(gcs, Length(gcs) + 1);
    gcs[High(gcs)] := gc;
  end;
  if not CheckFirstNumberGroupCount5(GroupCount, GroupCount2, GroupCount3, GroupCount4) then
    raise Exception.Create('请输入有效组合个数范围');
  if GroupCount > 0 then
  begin
    gc.Number := 7;
    gc.Value := GroupCount;
    gc.Value2 := GroupCount2;
    gc.Value3 := GroupCount3;
    gc.Value4 := GroupCount4;

    SetLength(gcs, Length(gcs) + 1);
    gcs[High(gcs)] := gc;
  end;
  if not CheckFirstNumberGroupCount6(GroupCount, GroupCount2, GroupCount3, GroupCount4) then
    raise Exception.Create('请输入有效组合个数范围');
  if GroupCount > 0 then
  begin
    gc.Number := 8;
    gc.Value := GroupCount;
    gc.Value2 := GroupCount2;
    gc.Value3 := GroupCount3;
    gc.Value4 := GroupCount4;

    SetLength(gcs, Length(gcs) + 1);
    gcs[High(gcs)] := gc;
  end;
  if not CheckFirstNumberGroupCount7(GroupCount, GroupCount2, GroupCount3, GroupCount4) then
    raise Exception.Create('请输入有效组合个数范围');
  if GroupCount > 0 then
  begin
    gc.Number := 9;
    gc.Value := GroupCount;
    gc.Value2 := GroupCount2;
    gc.Value3 := GroupCount3;
    gc.Value4 := GroupCount4;

    SetLength(gcs, Length(gcs) + 1);
    gcs[High(gcs)] := gc;
  end;
  if not edtExportCodeNameCount.TryToValue(iExportCodeNameCount) then
    raise Exception.Create('请输入有效导出代号数');
  if not edtExportCodeNameCount2.TryToValue(iExportCodeNameCount2) then
    raise Exception.Create('请输入有效导出代号数');
  if not edtExportCodeNameCount3.TryToValue(iExportCodeNameCount3) then
    raise Exception.Create('请输入有效导出代号数');

  fSettings.Flag := 11;
  fSettings.GroupCounts := gcs;
  fSettings.ExportFile := chkExportFile.Checked;
  fSettings.ExportFile2 := chkExportFile2.Checked;
  fSettings.ExportFile3 := chkExportFile3.Checked;
  fSettings.ExportFile4 := chkExportFile4.Checked;
  fSettings.ExportCodeNameCount := iExportCodeNameCount;
  fSettings.ExportCodeNameCount2 := iExportCodeNameCount2;
  fSettings.ExportCodeNameCount3 := iExportCodeNameCount3;

  fKeyValue.SetKeyValue('Settings11', fSerializer.Serialize(fSettings));

  ModalResult := mrOk;
end;

end.
