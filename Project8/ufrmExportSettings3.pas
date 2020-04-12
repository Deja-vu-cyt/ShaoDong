unit ufrmExportSettings3;

interface

uses
  uDataComputer,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.Types, System.IOUtils, System.Diagnostics,
  Vcl.Controls, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.FileCtrl,
  Vcl.CheckLst, Vcl.ComCtrls, Vcl.Menus, Vcl.Forms;

type
  TfrmExportSettings3 = class(TForm)
    ScrollBox1: TScrollBox;
    btnConfirm: TButton;
    Label2: TLabel;
    edtSubsectionGroupCount3: TEdit;
    edtSubsectionGroupCount4: TEdit;
    Label6: TLabel;
    edtSubsectionGroupCount7: TEdit;
    edtSubsectionGroupCount8: TEdit;
    Label7: TLabel;
    edtSubsectionGroupCount11: TEdit;
    edtSubsectionGroupCount12: TEdit;
    Label8: TLabel;
    edtSubsectionGroupCount15: TEdit;
    edtSubsectionGroupCount16: TEdit;
    Label9: TLabel;
    edtSubsectionGroupCount19: TEdit;
    edtSubsectionGroupCount20: TEdit;
    Label10: TLabel;
    edtSubsectionGroupCount23: TEdit;
    edtSubsectionGroupCount24: TEdit;
    Label11: TLabel;
    edtSubsectionGroupCount27: TEdit;
    edtSubsectionGroupCount28: TEdit;
    edtSubsectionGroupCount: TEdit;
    edtSubsectionGroupCount2: TEdit;
    edtSubsectionGroupCount5: TEdit;
    edtSubsectionGroupCount6: TEdit;
    edtSubsectionGroupCount9: TEdit;
    edtSubsectionGroupCount10: TEdit;
    edtSubsectionGroupCount13: TEdit;
    edtSubsectionGroupCount14: TEdit;
    edtSubsectionGroupCount17: TEdit;
    edtSubsectionGroupCount18: TEdit;
    edtSubsectionGroupCount21: TEdit;
    edtSubsectionGroupCount22: TEdit;
    edtSubsectionGroupCount25: TEdit;
    edtSubsectionGroupCount26: TEdit;
    Label1: TLabel;
    chkExportFile: TCheckBox;
    chkExportFile2: TCheckBox;
    chkExportFile3: TCheckBox;
    edtExportCodeNameCount: TEdit;
    edtExportCodeNameCount2: TEdit;
    edtExportCodeNameCount3: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    chkExportFile4: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btnConfirmClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
  private
    fSettings: TSettings;
    function CheckSubsectionGroupCount(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckSubsectionGroupCount2(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckSubsectionGroupCount3(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckSubsectionGroupCount4(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckSubsectionGroupCount5(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckSubsectionGroupCount6(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckSubsectionGroupCount7(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
  public
    property Settings: TSettings read fSettings;
  end;

var
  frmExportSettings3: TfrmExportSettings3;

implementation

uses
  uControlHelper;

{$R *.dfm}

function TfrmExportSettings3.CheckSubsectionGroupCount(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
begin
  Result := edtSubsectionGroupCount.TryToValue(ValueCount);
  if not Result then Exit;
  Result := edtSubsectionGroupCount2.TryToValue(ValueCount2);
  if not Result then Exit;
  Result := edtSubsectionGroupCount3.TryToValue(ValueCount3);
  if not Result then Exit;
  Result := edtSubsectionGroupCount4.TryToValue(ValueCount4);
  if not Result then Exit;
  if not ((ValueCount = 0) and (ValueCount2 = 0) and (ValueCount3 = 0) and (ValueCount4 = 0)) then
    Result := (ValueCount > 0) and (ValueCount2 >= ValueCount) and (ValueCount3 > 0) and (ValueCount4 >= ValueCount3);
end;

function TfrmExportSettings3.CheckSubsectionGroupCount2(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
begin
  Result := edtSubsectionGroupCount5.TryToValue(ValueCount);
  if not Result then Exit;
  Result := edtSubsectionGroupCount6.TryToValue(ValueCount2);
  if not Result then Exit;
  Result := edtSubsectionGroupCount7.TryToValue(ValueCount3);
  if not Result then Exit;
  Result := edtSubsectionGroupCount8.TryToValue(ValueCount4);
  if not Result then Exit;
  if not ((ValueCount = 0) and (ValueCount2 = 0) and (ValueCount3 = 0) and (ValueCount4 = 0)) then
    Result := (ValueCount > 0) and (ValueCount2 >= ValueCount) and (ValueCount3 > 0) and (ValueCount4 >= ValueCount3);
end;

function TfrmExportSettings3.CheckSubsectionGroupCount3(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
begin
  Result := edtSubsectionGroupCount9.TryToValue(ValueCount);
  if not Result then Exit;
  Result := edtSubsectionGroupCount10.TryToValue(ValueCount2);
  if not Result then Exit;
  Result := edtSubsectionGroupCount11.TryToValue(ValueCount3);
  if not Result then Exit;
  Result := edtSubsectionGroupCount12.TryToValue(ValueCount4);
  if not Result then Exit;
  if not ((ValueCount = 0) and (ValueCount2 = 0) and (ValueCount3 = 0) and (ValueCount4 = 0)) then
    Result := (ValueCount > 0) and (ValueCount2 >= ValueCount) and (ValueCount3 > 0) and (ValueCount4 >= ValueCount3);
end;

function TfrmExportSettings3.CheckSubsectionGroupCount4(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
begin
  Result := edtSubsectionGroupCount13.TryToValue(ValueCount);
  if not Result then Exit;
  Result := edtSubsectionGroupCount14.TryToValue(ValueCount2);
  if not Result then Exit;
  Result := edtSubsectionGroupCount15.TryToValue(ValueCount3);
  if not Result then Exit;
  Result := edtSubsectionGroupCount16.TryToValue(ValueCount4);
  if not Result then Exit;
  if not ((ValueCount = 0) and (ValueCount2 = 0) and (ValueCount3 = 0) and (ValueCount4 = 0)) then
    Result := (ValueCount > 0) and (ValueCount2 >= ValueCount) and (ValueCount3 > 0) and (ValueCount4 >= ValueCount3);
end;

function TfrmExportSettings3.CheckSubsectionGroupCount5(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
begin
  Result := edtSubsectionGroupCount17.TryToValue(ValueCount);
  if not Result then Exit;
  Result := edtSubsectionGroupCount18.TryToValue(ValueCount2);
  if not Result then Exit;
  Result := edtSubsectionGroupCount19.TryToValue(ValueCount3);
  if not Result then Exit;
  Result := edtSubsectionGroupCount20.TryToValue(ValueCount4);
  if not Result then Exit;
  if not ((ValueCount = 0) and (ValueCount2 = 0) and (ValueCount3 = 0) and (ValueCount4 = 0)) then
    Result := (ValueCount > 0) and (ValueCount2 >= ValueCount) and (ValueCount3 > 0) and (ValueCount4 >= ValueCount3);
end;

function TfrmExportSettings3.CheckSubsectionGroupCount6(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
begin
  Result := edtSubsectionGroupCount21.TryToValue(ValueCount);
  if not Result then Exit;
  Result := edtSubsectionGroupCount22.TryToValue(ValueCount2);
  if not Result then Exit;
  Result := edtSubsectionGroupCount23.TryToValue(ValueCount3);
  if not Result then Exit;
  Result := edtSubsectionGroupCount24.TryToValue(ValueCount4);
  if not Result then Exit;
  if not ((ValueCount = 0) and (ValueCount2 = 0) and (ValueCount3 = 0) and (ValueCount4 = 0)) then
    Result := (ValueCount > 0) and (ValueCount2 >= ValueCount) and (ValueCount3 > 0) and (ValueCount4 >= ValueCount3);
end;

function TfrmExportSettings3.CheckSubsectionGroupCount7(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
begin
  Result := edtSubsectionGroupCount25.TryToValue(ValueCount);
  if not Result then Exit;
  Result := edtSubsectionGroupCount26.TryToValue(ValueCount2);
  if not Result then Exit;
  Result := edtSubsectionGroupCount27.TryToValue(ValueCount3);
  if not Result then Exit;
  Result := edtSubsectionGroupCount28.TryToValue(ValueCount4);
  if not Result then Exit;
  if not ((ValueCount = 0) and (ValueCount2 = 0) and (ValueCount3 = 0) and (ValueCount4 = 0)) then
    Result := (ValueCount > 0) and (ValueCount2 >= ValueCount) and (ValueCount3 > 0) and (ValueCount4 >= ValueCount3);
end;

procedure TfrmExportSettings3.FormCreate(Sender: TObject);
var
  v: Variant;
begin
  ScrollBox1.VertScrollBar.Position := 0;

  fSettings.Flag := 10;
  fSettings.ExportFile := True;
  fSettings.ExportFile2 := True;
  fSettings.ExportFile3 := True;
  fSettings.ExportFile4 := True;
  fSettings.ExportCodeNameCount := 0;
  fSettings.ExportCodeNameCount2 := 0;
  fSettings.ExportCodeNameCount3 := 0;

  fKeyValue.GetKeyValue('Settings10', v);
  if not VarIsEmpty(v) then fSettings := fSerializer.Deserialize<TSettings>(v);
end;

procedure TfrmExportSettings3.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position - WheelDelta div 2;
end;

procedure TfrmExportSettings3.FormShow(Sender: TObject);
var
  gc: TGroupCount;
begin
  edtSubsectionGroupCount.Text := '';
  edtSubsectionGroupCount2.Text := '';
  edtSubsectionGroupCount3.Text := '';
  edtSubsectionGroupCount4.Text := '';
  edtSubsectionGroupCount5.Text := '';
  edtSubsectionGroupCount6.Text := '';
  edtSubsectionGroupCount7.Text := '';
  edtSubsectionGroupCount8.Text := '';
  edtSubsectionGroupCount9.Text := '';
  edtSubsectionGroupCount10.Text := '';
  edtSubsectionGroupCount11.Text := '';
  edtSubsectionGroupCount12.Text := '';
  edtSubsectionGroupCount13.Text := '';
  edtSubsectionGroupCount14.Text := '';
  edtSubsectionGroupCount15.Text := '';
  edtSubsectionGroupCount16.Text := '';
  edtSubsectionGroupCount17.Text := '';
  edtSubsectionGroupCount18.Text := '';
  edtSubsectionGroupCount19.Text := '';
  edtSubsectionGroupCount20.Text := '';
  edtSubsectionGroupCount21.Text := '';
  edtSubsectionGroupCount22.Text := '';
  edtSubsectionGroupCount23.Text := '';
  edtSubsectionGroupCount24.Text := '';
  edtSubsectionGroupCount25.Text := '';
  edtSubsectionGroupCount26.Text := '';
  edtSubsectionGroupCount27.Text := '';
  edtSubsectionGroupCount28.Text := '';

  for gc in fSettings.GroupCounts do
    case gc.Number of
      3:
      begin
        if gc.Value > 0 then
          edtSubsectionGroupCount.Text := gc.Value.ToString;
        if gc.Value2 > 0 then
          edtSubsectionGroupCount2.Text := gc.Value2.ToString;
        if gc.Value3 > 0 then
          edtSubsectionGroupCount3.Text := gc.Value3.ToString;
        if gc.Value4 > 0 then
          edtSubsectionGroupCount4.Text := gc.Value4.ToString;
      end;
      4:
      begin
        if gc.Value > 0 then
          edtSubsectionGroupCount5.Text := gc.Value.ToString;
        if gc.Value2 > 0 then
          edtSubsectionGroupCount6.Text := gc.Value2.ToString;
        if gc.Value3 > 0 then
          edtSubsectionGroupCount7.Text := gc.Value3.ToString;
        if gc.Value4 > 0 then
          edtSubsectionGroupCount8.Text := gc.Value4.ToString;
      end;
      5:
      begin
        if gc.Value > 0 then
          edtSubsectionGroupCount9.Text := gc.Value.ToString;
        if gc.Value2 > 0 then
          edtSubsectionGroupCount10.Text := gc.Value2.ToString;
        if gc.Value3 > 0 then
          edtSubsectionGroupCount11.Text := gc.Value3.ToString;
        if gc.Value4 > 0 then
          edtSubsectionGroupCount12.Text := gc.Value4.ToString;
      end;
      6:
      begin
        if gc.Value > 0 then
          edtSubsectionGroupCount13.Text := gc.Value.ToString;
        if gc.Value2 > 0 then
          edtSubsectionGroupCount14.Text := gc.Value2.ToString;
        if gc.Value3 > 0 then
          edtSubsectionGroupCount15.Text := gc.Value3.ToString;
        if gc.Value4 > 0 then
          edtSubsectionGroupCount16.Text := gc.Value4.ToString;
      end;
      7:
      begin
        if gc.Value > 0 then
          edtSubsectionGroupCount17.Text := gc.Value.ToString;
        if gc.Value2 > 0 then
          edtSubsectionGroupCount18.Text := gc.Value2.ToString;
        if gc.Value3 > 0 then
          edtSubsectionGroupCount19.Text := gc.Value3.ToString;
        if gc.Value4 > 0 then
          edtSubsectionGroupCount20.Text := gc.Value4.ToString;
      end;
      8:
      begin
        if gc.Value > 0 then
          edtSubsectionGroupCount21.Text := gc.Value.ToString;
        if gc.Value2 > 0 then
          edtSubsectionGroupCount22.Text := gc.Value2.ToString;
        if gc.Value3 > 0 then
          edtSubsectionGroupCount23.Text := gc.Value3.ToString;
        if gc.Value4 > 0 then
          edtSubsectionGroupCount24.Text := gc.Value4.ToString;
      end;
      9:
      begin
        if gc.Value > 0 then
          edtSubsectionGroupCount25.Text := gc.Value.ToString;
        if gc.Value2 > 0 then
          edtSubsectionGroupCount26.Text := gc.Value2.ToString;
        if gc.Value3 > 0 then
          edtSubsectionGroupCount27.Text := gc.Value3.ToString;
        if gc.Value4 > 0 then
          edtSubsectionGroupCount28.Text := gc.Value4.ToString;
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

procedure TfrmExportSettings3.btnConfirmClick(Sender: TObject);
var
  GroupCount, GroupCount2, GroupCount3, GroupCount4,
  iExportCodeNameCount, iExportCodeNameCount2, iExportCodeNameCount3: Integer;
  gc: TGroupCount;
  gcs: TArray<TGroupCount>;
begin
  SetLength(gcs, 0);
  if not CheckSubsectionGroupCount(GroupCount, GroupCount2, GroupCount3, GroupCount4) then
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
  if not CheckSubsectionGroupCount2(GroupCount, GroupCount2, GroupCount3, GroupCount4) then
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
  if not CheckSubsectionGroupCount3(GroupCount, GroupCount2, GroupCount3, GroupCount4) then
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
  if not CheckSubsectionGroupCount4(GroupCount, GroupCount2, GroupCount3, GroupCount4) then
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
  if not CheckSubsectionGroupCount5(GroupCount, GroupCount2, GroupCount3, GroupCount4) then
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
  if not CheckSubsectionGroupCount6(GroupCount, GroupCount2, GroupCount3, GroupCount4) then
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
  if not CheckSubsectionGroupCount7(GroupCount, GroupCount2, GroupCount3, GroupCount4) then
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

  fSettings.Flag := 10;
  fSettings.GroupCounts := gcs;
  fSettings.ExportFile := chkExportFile.Checked;
  fSettings.ExportFile2 := chkExportFile2.Checked;
  fSettings.ExportFile3 := chkExportFile3.Checked;
  fSettings.ExportFile4 := chkExportFile4.Checked;
  fSettings.ExportCodeNameCount := iExportCodeNameCount;
  fSettings.ExportCodeNameCount2 := iExportCodeNameCount2;
  fSettings.ExportCodeNameCount3 := iExportCodeNameCount3;

  fKeyValue.SetKeyValue('Settings10', fSerializer.Serialize(fSettings));

  ModalResult := mrOk;
end;

end.
