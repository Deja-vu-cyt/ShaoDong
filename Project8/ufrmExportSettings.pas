unit ufrmExportSettings;

interface

uses
  uDataComputer,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.Types, System.IOUtils, System.Diagnostics,
  Vcl.Controls, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.FileCtrl,
  Vcl.CheckLst, Vcl.ComCtrls, Vcl.Menus, Vcl.Forms;

type
  TfrmExportSettings = class(TForm)
    ScrollBox1: TScrollBox;
    btnConfirm: TButton;
    chkExportFile: TCheckBox;
    chkExportFile2: TCheckBox;
    chkExportFile3: TCheckBox;
    chkExportFile4: TCheckBox;
    chkExportFile5: TCheckBox;
    chkExportFile6: TCheckBox;
    chkExportFile7: TCheckBox;
    chkExportFile8: TCheckBox;
    chkSelectAll: TCheckBox;
    chkDeleteProcessed: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    CheckBox1: TCheckBox;
    Label4: TLabel;
    Label5: TLabel;
    chkExportOneRowSpacingFile: TCheckBox;
    Label6: TLabel;
    Label7: TLabel;
    chkExportOnlyOneRowSpacingFile: TCheckBox;
    Label8: TLabel;
    Label9: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    chkTakePartRow: TCheckBox;
    edtRowRange2: TEdit;
    edtRowRange: TEdit;
    edtRowRange3: TEdit;
    edtRowRange4: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnConfirmClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure chkSelectAllClick(Sender: TObject);
  private
    fSettings: TSettings;
    fSettings2: TSettings;
    fSettings3: TSettings;
    fSettings4: TSettings;
    fSettings5: TSettings;
    fSettings6: TSettings;
    fSettings7: TSettings;
    fSettings8: TSettings;
    fDeleteProcessed: Boolean;
    fExportOneRowSpacingFile: Boolean;
    fExportOnlyOneRowSpacingFile: Boolean;
    fTakePartRow: Boolean;
    fRowRange: Word;
    fRowRange2: Word;
    fRowRange3: Word;
    fRowRange4: Word;
    function CheckRowRange(var ValueCount, ValueCount2: Integer): Boolean;
    function CheckRowRange2(var ValueCount, ValueCount2: Integer): Boolean;
  public
    property Settings: TSettings read fSettings;
    property Settings2: TSettings read fSettings2;
    property Settings3: TSettings read fSettings3;
    property Settings4: TSettings read fSettings4;
    property Settings5: TSettings read fSettings5;
    property Settings6: TSettings read fSettings6;
    property Settings7: TSettings read fSettings7;
    property Settings8: TSettings read fSettings8;
    property DeleteProcessed: Boolean read fDeleteProcessed;
    property ExportOneRowSpacingFile: Boolean read fExportOneRowSpacingFile;
    property ExportOnlyOneRowSpacingFile: Boolean read fExportOnlyOneRowSpacingFile;
    property TakePartRow: Boolean read fTakePartRow;
    property RowRange: Word read fRowRange;
    property RowRange2: Word read fRowRange2;
    property RowRange3: Word read fRowRange3;
    property RowRange4: Word read fRowRange4;
  end;

var
  frmExportSettings: TfrmExportSettings;

implementation

uses
  uControlHelper, uCommon;

{$R *.dfm}

function TfrmExportSettings.CheckRowRange(var ValueCount, ValueCount2: Integer): Boolean;
begin
  Result := edtRowRange.TryToValue(ValueCount);
  if not Result then Exit;
  Result := edtRowRange2.TryToValue(ValueCount2);
  if not Result then Exit;
  if not ((ValueCount = 0) and (ValueCount2 = 0)) then
    Result := (ValueCount > 0) and (ValueCount2 >= ValueCount);
end;

function TfrmExportSettings.CheckRowRange2(var ValueCount, ValueCount2: Integer): Boolean;
begin
  Result := edtRowRange3.TryToValue(ValueCount);
  if not Result then Exit;
  Result := edtRowRange4.TryToValue(ValueCount2);
  if not Result then Exit;
  if not ((ValueCount = 0) and (ValueCount2 = 0)) then
    Result := (ValueCount > 0) and (ValueCount2 >= ValueCount);
end;

procedure TfrmExportSettings.chkSelectAllClick(Sender: TObject);
begin
  chkExportFile.Checked := TCheckBox(Sender).Checked;
  chkExportFile2.Checked := TCheckBox(Sender).Checked;
  chkExportFile3.Checked := TCheckBox(Sender).Checked;
  chkExportFile4.Checked := TCheckBox(Sender).Checked;
  chkExportFile5.Checked := TCheckBox(Sender).Checked;
  chkExportFile6.Checked := TCheckBox(Sender).Checked;
  chkExportFile7.Checked := TCheckBox(Sender).Checked;
  chkExportFile8.Checked := TCheckBox(Sender).Checked;
end;

procedure TfrmExportSettings.FormCreate(Sender: TObject);
var
  v: Variant;
begin
  ScrollBox1.VertScrollBar.Position := 0;

  fSettings.Flag := 1;
  fSettings.ExportFile := True;
  fSettings.ExportFile2 := True;
  fSettings.ExportFile3 := True;
  fSettings.ExportCodeNameCount := 0;
  fSettings.ExportCodeNameCount2 := 0;
  fSettings.ExportCodeNameCount3 := 0;

  fSettings2.Flag := 2;
  fSettings2.ExportFile := True;
  fSettings2.ExportFile2 := True;
  fSettings2.ExportFile3 := True;
  fSettings2.ExportCodeNameCount := 0;
  fSettings2.ExportCodeNameCount2 := 0;
  fSettings2.ExportCodeNameCount3 := 0;

  fSettings3.Flag := 3;
  fSettings3.ExportFile := True;
  fSettings3.ExportFile2 := True;
  fSettings3.ExportFile3 := True;
  fSettings3.ExportCodeNameCount := 0;
  fSettings3.ExportCodeNameCount2 := 0;
  fSettings3.ExportCodeNameCount3 := 0;

  fSettings4.Flag := 4;
  fSettings4.ExportFile := True;
  fSettings4.ExportFile2 := True;
  fSettings4.ExportFile3 := True;
  fSettings4.ExportCodeNameCount := 0;
  fSettings4.ExportCodeNameCount2 := 0;
  fSettings4.ExportCodeNameCount3 := 0;

  fSettings5.Flag := 5;
  fSettings5.ExportFile := True;
  fSettings5.ExportFile2 := True;
  fSettings5.ExportFile3 := True;
  fSettings5.ExportCodeNameCount := 0;
  fSettings5.ExportCodeNameCount2 := 0;
  fSettings5.ExportCodeNameCount3 := 0;

  fSettings6.Flag := 6;
  fSettings6.ExportFile := True;
  fSettings6.ExportFile2 := True;
  fSettings6.ExportFile3 := True;
  fSettings6.ExportCodeNameCount := 0;
  fSettings6.ExportCodeNameCount2 := 0;
  fSettings6.ExportCodeNameCount3 := 0;

  fSettings7.Flag := 7;
  fSettings7.ExportFile := True;
  fSettings7.ExportFile2 := True;
  fSettings7.ExportFile3 := True;
  fSettings7.ExportCodeNameCount := 0;
  fSettings7.ExportCodeNameCount2 := 0;
  fSettings7.ExportCodeNameCount3 := 0;

  fSettings8.Flag := 8;
  fSettings8.ExportFile := True;
  fSettings8.ExportFile2 := True;
  fSettings8.ExportFile3 := True;
  fSettings8.ExportCodeNameCount := 0;
  fSettings8.ExportCodeNameCount2 := 0;
  fSettings8.ExportCodeNameCount3 := 0;

  fKeyValue.GetKeyValue('Settings', v);
  if not VarIsEmpty(v) then fSettings := fSerializer.Deserialize<TSettings>(v);
  fKeyValue.GetKeyValue('Settings2', v);
  if not VarIsEmpty(v) then fSettings2 := fSerializer.Deserialize<TSettings>(v);
  fKeyValue.GetKeyValue('Settings3', v);
  if not VarIsEmpty(v) then fSettings3 := fSerializer.Deserialize<TSettings>(v);
  fKeyValue.GetKeyValue('Settings4', v);
  if not VarIsEmpty(v) then fSettings4 := fSerializer.Deserialize<TSettings>(v);
  fKeyValue.GetKeyValue('Settings5', v);
  if not VarIsEmpty(v) then fSettings5 := fSerializer.Deserialize<TSettings>(v);
  fKeyValue.GetKeyValue('Settings6', v);
  if not VarIsEmpty(v) then fSettings6 := fSerializer.Deserialize<TSettings>(v);
  fKeyValue.GetKeyValue('Settings7', v);
  if not VarIsEmpty(v) then fSettings7 := fSerializer.Deserialize<TSettings>(v);
  fKeyValue.GetKeyValue('Settings8', v);
  if not VarIsEmpty(v) then fSettings8 := fSerializer.Deserialize<TSettings>(v);
  fDeleteProcessed := True;
  fKeyValue.GetKeyValue('DeleteProcessed', v);
  if not VarIsEmpty(v) then fDeleteProcessed := v;
  fExportOneRowSpacingFile := True;
  fKeyValue.GetKeyValue('ExportOneRowSpacingFile', v);
  if not VarIsEmpty(v) then fExportOneRowSpacingFile := v;
  fExportOnlyOneRowSpacingFile := True;
  fKeyValue.GetKeyValue('ExportOnlyOneRowSpacingFile', v);
  if not VarIsEmpty(v) then fExportOnlyOneRowSpacingFile := v;
  fKeyValue.GetKeyValue('TakePartRow', v);
  if not VarIsEmpty(v) then fTakePartRow := v;
  fKeyValue.GetKeyValue('RowRange', v);
  if not VarIsEmpty(v) then fRowRange := v;
  fKeyValue.GetKeyValue('RowRange2', v);
  if not VarIsEmpty(v) then fRowRange2 := v;
  fKeyValue.GetKeyValue('RowRange3', v);
  if not VarIsEmpty(v) then fRowRange3 := v;
  fKeyValue.GetKeyValue('RowRange4', v);
  if not VarIsEmpty(v) then fRowRange4 := v;
end;

procedure TfrmExportSettings.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position - WheelDelta div 2;
end;

procedure TfrmExportSettings.FormShow(Sender: TObject);
begin
  chkExportFile.Checked := fSettings.Flag = 1;
  chkExportFile2.Checked := fSettings2.Flag = 2;
  chkExportFile3.Checked := fSettings3.Flag = 3;
  chkExportFile4.Checked := fSettings4.Flag = 4;
  chkExportFile5.Checked := fSettings5.Flag = 5;
  chkExportFile6.Checked := fSettings6.Flag = 6;
  chkExportFile7.Checked := fSettings7.Flag = 7;
  chkExportFile8.Checked := fSettings8.Flag = 8;
  chkDeleteProcessed.Checked := fDeleteProcessed;
  chkExportOneRowSpacingFile.Checked := fExportOneRowSpacingFile;
  chkExportOnlyOneRowSpacingFile.Checked := fExportOnlyOneRowSpacingFile;
  chkTakePartRow.Checked := fTakePartRow;
  if fRowRange > 0 then
    edtRowRange.Text := fRowRange.ToString;
  if fRowRange2 > 0 then
    edtRowRange2.Text := fRowRange2.ToString;
  if fRowRange3 > 0 then
    edtRowRange3.Text := fRowRange3.ToString;
  if fRowRange4 > 0 then
    edtRowRange4.Text := fRowRange4.ToString;
end;

procedure TfrmExportSettings.btnConfirmClick(Sender: TObject);
var
  Value, Value2, Value3, Value4: Integer;
begin
  if chkTakePartRow.Checked then
  begin
    if not CheckRowRange(Value, Value2) then
      raise Exception.Create('请输入有效范围');
    if not CheckRowRange(Value3, Value4) then
      raise Exception.Create('请输入有效范围');
  end;

  fSettings.Flag := 0;
  if chkExportFile.Checked then fSettings.Flag := 1;
  fSettings2.Flag := 0;
  if chkExportFile2.Checked then fSettings2.Flag := 2;
  fSettings3.Flag := 0;
  if chkExportFile3.Checked then fSettings3.Flag := 3;
  fSettings4.Flag := 0;
  if chkExportFile4.Checked then fSettings4.Flag := 4;
  fSettings5.Flag := 0;
  if chkExportFile5.Checked then fSettings5.Flag := 5;
  fSettings6.Flag := 0;
  if chkExportFile6.Checked then fSettings6.Flag := 6;
  fSettings7.Flag := 0;
  if chkExportFile7.Checked then fSettings7.Flag := 7;
  fSettings8.Flag := 0;
  if chkExportFile8.Checked then fSettings8.Flag := 8;
  fDeleteProcessed := chkDeleteProcessed.Checked;
  fExportOneRowSpacingFile := chkExportOneRowSpacingFile.Checked;
  fExportOnlyOneRowSpacingFile := chkExportOnlyOneRowSpacingFile.Checked;
  fTakePartRow := chkTakePartRow.Checked;
  fRowRange := Value;
  fRowRange2 := Value2;
  fRowRange3 := Value3;
  fRowRange4 := Value4;

  fKeyValue.SetKeyValue('Settings', fSerializer.Serialize(fSettings));
  fKeyValue.SetKeyValue('Settings2', fSerializer.Serialize(fSettings2));
  fKeyValue.SetKeyValue('Settings3', fSerializer.Serialize(fSettings3));
  fKeyValue.SetKeyValue('Settings4', fSerializer.Serialize(fSettings4));
  fKeyValue.SetKeyValue('Settings5', fSerializer.Serialize(fSettings5));
  fKeyValue.SetKeyValue('Settings6', fSerializer.Serialize(fSettings6));
  fKeyValue.SetKeyValue('Settings7', fSerializer.Serialize(fSettings7));
  fKeyValue.SetKeyValue('Settings8', fSerializer.Serialize(fSettings8));
  fKeyValue.SetKeyValue('DeleteProcessed', fDeleteProcessed);
  fKeyValue.SetKeyValue('ExportOneRowSpacingFile', fExportOneRowSpacingFile);
  fKeyValue.SetKeyValue('ExportOnlyOneRowSpacingFile', fExportOnlyOneRowSpacingFile);
  fKeyValue.SetKeyValue('TakePartRow', fTakePartRow);
  fKeyValue.SetKeyValue('RowRange', fRowRange);
  fKeyValue.SetKeyValue('RowRange2', RowRange2);
  fKeyValue.SetKeyValue('RowRange3', RowRange3);
  fKeyValue.SetKeyValue('RowRange4', RowRange4);

  ModalResult := mrOk;
end;

end.
