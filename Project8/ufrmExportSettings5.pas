unit ufrmExportSettings5;

interface

uses
  uDataComputer,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.Types, System.IOUtils, System.Diagnostics,
  Vcl.Controls, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.FileCtrl,
  Vcl.CheckLst, Vcl.ComCtrls, Vcl.Menus, Vcl.Forms;

type
  TfrmExportSettings5 = class(TForm)
    ScrollBox1: TScrollBox;
    Label5: TLabel;
    edtSameValueCount: TEdit;
    edtSameValueCount2: TEdit;
    btnConfirm: TButton;
    Label1: TLabel;
    edtCompareSpacing: TEdit;
    Label2: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    edtSubsectionGroupCount3: TEdit;
    edtSubsectionGroupCount4: TEdit;
    edtSubsectionGroupCount7: TEdit;
    edtSubsectionGroupCount8: TEdit;
    edtSubsectionGroupCount11: TEdit;
    edtSubsectionGroupCount12: TEdit;
    edtSubsectionGroupCount15: TEdit;
    edtSubsectionGroupCount16: TEdit;
    edtSubsectionGroupCount19: TEdit;
    edtSubsectionGroupCount20: TEdit;
    edtSubsectionGroupCount23: TEdit;
    edtSubsectionGroupCount24: TEdit;
    edtSubsectionGroupCount27: TEdit;
    edtSubsectionGroupCount28: TEdit;
    edtFirstNumberGroupCount3: TEdit;
    edtFirstNumberGroupCount4: TEdit;
    edtFirstNumberGroupCount7: TEdit;
    edtFirstNumberGroupCount8: TEdit;
    edtFirstNumberGroupCount11: TEdit;
    edtFirstNumberGroupCount12: TEdit;
    edtFirstNumberGroupCount15: TEdit;
    edtFirstNumberGroupCount16: TEdit;
    edtFirstNumberGroupCount19: TEdit;
    edtFirstNumberGroupCount20: TEdit;
    edtFirstNumberGroupCount23: TEdit;
    edtFirstNumberGroupCount24: TEdit;
    edtFirstNumberGroupCount27: TEdit;
    edtFirstNumberGroupCount28: TEdit;
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
    Label3: TLabel;
    chkExportFile: TCheckBox;
    chkExportFile2: TCheckBox;
    chkExportFile3: TCheckBox;
    edtExportCodeNameCount: TEdit;
    edtExportCodeNameCount2: TEdit;
    edtExportCodeNameCount3: TEdit;
    Label4: TLabel;
    Label15: TLabel;
    Label20: TLabel;
    chkExportFile4: TCheckBox;
    chkExportFile5: TCheckBox;
    chkExportFile6: TCheckBox;
    edtExportCodeNameCount4: TEdit;
    edtExportCodeNameCount5: TEdit;
    edtExportCodeNameCount6: TEdit;
    Label21: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnConfirmClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
  private
    fSettings: TSettings;
    fSettings2: TSettings;
    fCompareSpacing: Word;
    fSameValueCount: Word;
    fSameValueCount2: Word;
    function CheckSameValueCount(var ValueCount, ValueCount2: Integer): Boolean;
    function CheckSubsectionGroupCount(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckSubsectionGroupCount2(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckSubsectionGroupCount3(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckSubsectionGroupCount4(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckSubsectionGroupCount5(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckSubsectionGroupCount6(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckSubsectionGroupCount7(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckFirstNumberGroupCount(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckFirstNumberGroupCount2(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckFirstNumberGroupCount3(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckFirstNumberGroupCount4(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckFirstNumberGroupCount5(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckFirstNumberGroupCount6(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
    function CheckFirstNumberGroupCount7(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
  public
    property Settings: TSettings read fSettings;
    property Settings2: TSettings read fSettings2;
    property CompareSpacing: Word read fCompareSpacing write fCompareSpacing;
    property SameValueCount: Word read fSameValueCount write fSameValueCount;
    property SameValueCount2: Word read fSameValueCount2 write fSameValueCount2;
  end;

var
  frmExportSettings5: TfrmExportSettings5;

implementation

uses
  uControlHelper;

{$R *.dfm}

function TfrmExportSettings5.CheckSameValueCount(var ValueCount, ValueCount2: Integer): Boolean;
begin
  Result := edtSameValueCount.TryToValue(ValueCount);
  if not Result then Exit;
  Result := edtSameValueCount2.TryToValue(ValueCount2);
  if not Result then Exit;
  Result := (ValueCount > 0) and (ValueCount2 >= ValueCount);
end;

function TfrmExportSettings5.CheckSubsectionGroupCount(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
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

function TfrmExportSettings5.CheckSubsectionGroupCount2(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
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

function TfrmExportSettings5.CheckSubsectionGroupCount3(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
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

function TfrmExportSettings5.CheckSubsectionGroupCount4(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
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

function TfrmExportSettings5.CheckSubsectionGroupCount5(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
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

function TfrmExportSettings5.CheckSubsectionGroupCount6(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
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

function TfrmExportSettings5.CheckSubsectionGroupCount7(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
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

function TfrmExportSettings5.CheckFirstNumberGroupCount(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
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

function TfrmExportSettings5.CheckFirstNumberGroupCount2(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
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

function TfrmExportSettings5.CheckFirstNumberGroupCount3(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
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

function TfrmExportSettings5.CheckFirstNumberGroupCount4(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
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

function TfrmExportSettings5.CheckFirstNumberGroupCount5(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
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

function TfrmExportSettings5.CheckFirstNumberGroupCount6(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
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

function TfrmExportSettings5.CheckFirstNumberGroupCount7(var ValueCount, ValueCount2, ValueCount3, ValueCount4: Integer): Boolean;
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

procedure TfrmExportSettings5.FormCreate(Sender: TObject);
var
  v: Variant;
begin
  ScrollBox1.VertScrollBar.Position := 0;

  fSettings.Flag := 12;
  fSettings.ExportFile := True;
  fSettings.ExportFile2 := True;
  fSettings.ExportFile3 := True;
  fSettings.ExportCodeNameCount := 0;
  fSettings.ExportCodeNameCount2 := 0;
  fSettings.ExportCodeNameCount3 := 0;

  fSettings2.Flag := 13;
  fSettings2.ExportFile := True;
  fSettings2.ExportFile2 := True;
  fSettings2.ExportFile3 := True;
  fSettings2.ExportCodeNameCount := 0;
  fSettings2.ExportCodeNameCount2 := 0;
  fSettings2.ExportCodeNameCount3 := 0;

  fCompareSpacing := 0;
  fKeyValue.GetKeyValue('VertCompareSpacing', v);
  if not VarIsEmpty(v) then fCompareSpacing := v;
  fSameValueCount := 0;
  fKeyValue.GetKeyValue('VertSameValueCount', v);
  if not VarIsEmpty(v) then fSameValueCount := v;
  fSameValueCount2 := 0;
  fKeyValue.GetKeyValue('VertSameValueCount2', v);
  if not VarIsEmpty(v) then fSameValueCount2 := v;
  fKeyValue.GetKeyValue('Settings12', v);
  if not VarIsEmpty(v) then fSettings := fSerializer.Deserialize<TSettings>(v);
  fKeyValue.GetKeyValue('Settings13', v);
  if not VarIsEmpty(v) then fSettings2 := fSerializer.Deserialize<TSettings>(v);
end;

procedure TfrmExportSettings5.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position - WheelDelta div 2;
end;

procedure TfrmExportSettings5.FormShow(Sender: TObject);
var
  gc: TGroupCount;
begin
  edtCompareSpacing.Text := '';
  edtSameValueCount.Text := '';
  edtSameValueCount2.Text := '';
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

  if fCompareSpacing > 0 then
    edtCompareSpacing.Text := fCompareSpacing.ToString;
  if fSameValueCount > 0 then
    edtSameValueCount.Text := fSameValueCount.ToString;
  if fSameValueCount2 > 0 then
    edtSameValueCount2.Text := fSameValueCount2.ToString;

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
  if fSettings.ExportCodeNameCount > 0 then
    edtExportCodeNameCount.Text := fSettings.ExportCodeNameCount.ToString;
  if fSettings.ExportCodeNameCount2 > 0 then
    edtExportCodeNameCount2.Text := fSettings.ExportCodeNameCount2.ToString;
  if fSettings.ExportCodeNameCount3 > 0 then
    edtExportCodeNameCount3.Text := fSettings.ExportCodeNameCount3.ToString;

  for gc in fSettings2.GroupCounts do
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

procedure TfrmExportSettings5.btnConfirmClick(Sender: TObject);
var
  GroupCount, GroupCount2, GroupCount3, GroupCount4, iCompareSpacing,
  iSameValueCount, iSameValueCount2,
  iExportCodeNameCount, iExportCodeNameCount2, iExportCodeNameCount3,
  iExportCodeNameCount4, iExportCodeNameCount5, iExportCodeNameCount6: Integer;
  gc: TGroupCount;
  gcs, gcs2: TArray<TGroupCount>;
begin
  if not edtCompareSpacing.TryToValue(iCompareSpacing) then
    raise Exception.Create('请输入有效相比较行间距');

  if not CheckSameValueCount(iSameValueCount, iSameValueCount2) then
    raise Exception.Create('请输入有效相同列数范围');

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

  SetLength(gcs2, 0);
  if not CheckFirstNumberGroupCount(GroupCount, GroupCount2, GroupCount3, GroupCount4) then
    raise Exception.Create('请输入有效组合个数范围');
  if GroupCount > 0 then
  begin
    gc.Number := 3;
    gc.Value := GroupCount;
    gc.Value2 := GroupCount2;
    gc.Value3 := GroupCount3;
    gc.Value4 := GroupCount4;

    SetLength(gcs2, Length(gcs2) + 1);
    gcs2[High(gcs2)] := gc;
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

    SetLength(gcs2, Length(gcs2) + 1);
    gcs2[High(gcs2)] := gc;
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

    SetLength(gcs2, Length(gcs2) + 1);
    gcs2[High(gcs2)] := gc;
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

    SetLength(gcs2, Length(gcs2) + 1);
    gcs2[High(gcs2)] := gc;
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

    SetLength(gcs2, Length(gcs2) + 1);
    gcs2[High(gcs2)] := gc;
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

    SetLength(gcs2, Length(gcs2) + 1);
    gcs2[High(gcs2)] := gc;
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

    SetLength(gcs2, Length(gcs2) + 1);
    gcs2[High(gcs2)] := gc;
  end;
  if not edtExportCodeNameCount4.TryToValue(iExportCodeNameCount4) then
    raise Exception.Create('请输入有效导出代号数');
  if not edtExportCodeNameCount5.TryToValue(iExportCodeNameCount5) then
    raise Exception.Create('请输入有效导出代号数');
  if not edtExportCodeNameCount6.TryToValue(iExportCodeNameCount6) then
    raise Exception.Create('请输入有效导出代号数');

  fCompareSpacing := iCompareSpacing;
  fSameValueCount := iSameValueCount;
  fSameValueCount2 := iSameValueCount2;
  fSettings.Flag := 12;
  fSettings.GroupCounts := gcs;
  fSettings.ExportFile := chkExportFile.Checked;
  fSettings.ExportFile2 := chkExportFile2.Checked;
  fSettings.ExportFile3 := chkExportFile3.Checked;
  fSettings.ExportCodeNameCount := iExportCodeNameCount;
  fSettings.ExportCodeNameCount2 := iExportCodeNameCount2;
  fSettings.ExportCodeNameCount3 := iExportCodeNameCount3;
  fSettings2.Flag := 13;
  fSettings2.GroupCounts := gcs2;
  fSettings2.ExportFile := chkExportFile4.Checked;
  fSettings2.ExportFile2 := chkExportFile5.Checked;
  fSettings2.ExportFile3 := chkExportFile6.Checked;
  fSettings2.ExportCodeNameCount := iExportCodeNameCount4;
  fSettings2.ExportCodeNameCount2 := iExportCodeNameCount5;
  fSettings2.ExportCodeNameCount3 := iExportCodeNameCount6;

  fKeyValue.SetKeyValue('VertCompareSpacing', fCompareSpacing);
  fKeyValue.SetKeyValue('VertSameValueCount', fSameValueCount);
  fKeyValue.SetKeyValue('VertSameValueCount2', fSameValueCount2);
  fKeyValue.SetKeyValue('Settings12', fSerializer.Serialize(fSettings));
  fKeyValue.SetKeyValue('Settings13', fSerializer.Serialize(fSettings2));

  ModalResult := mrOk;
end;

end.
