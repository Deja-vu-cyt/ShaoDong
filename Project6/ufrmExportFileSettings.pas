unit ufrmExportFileSettings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmExportFileSettings = class(TForm)
    btnOk: TButton;
    lblLine: TLabel;
    edtExportCodeNameValueCount: TEdit;
    edtExportCodeNameValueCount2: TEdit;
    chkKeepLastBatchCodeNameOnEachComputer: TCheckBox;
    chkFile4: TCheckBox;
    chkFile2: TCheckBox;
    chkFile3: TCheckBox;
    chkFile: TCheckBox;
    chkKeepExportCodeNameValueCount: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fCompareMode: Byte;
    procedure SetCompareMode(Value: Byte);
    function CheckExportCodeNameValueCount(var ValueCount, ValueCount2: Integer): Boolean;
  public
    property CompareMode: Byte read fCompareMode write SetCompareMode;
  end;

var
  frmExportFileSettings: TfrmExportFileSettings;

implementation

uses
  uDataComputer, uControlHelper;

{$R *.dfm}

procedure TfrmExportFileSettings.SetCompareMode(Value: Byte);

  procedure BuildLineInfo(Value: Byte; out Number: Byte; out sCompareMode: string);
  begin
    case TCompareMode(Value) of
      cmVert:
      begin
        Number := 6;
        sCompareMode := '直连';
      end;
      cmSlant:
      begin
        Number := 6;
        sCompareMode := '斜连';
      end;
      cmVertSlant:
      begin
        Number := 7;
        sCompareMode := '直、斜连';
      end;
    end;
  end;

var
  s, sCompareMode, sNewCompareMode: string;
  Number, NewNumber: Byte;
begin
  if fCompareMode <> Value then
  begin
    BuildLineInfo(fCompareMode, Number, sCompareMode);
    BuildLineInfo(Value, NewNumber, sNewCompareMode);

    s := lblLine.Caption;
    lblLine.Caption := s.Replace(Number.ToString, NewNumber.ToString).Replace(sCompareMode, sNewCompareMode);
    s := chkKeepExportCodeNameValueCount.Caption;
    chkKeepExportCodeNameValueCount.Caption := s.Replace(Number.ToString, NewNumber.ToString).Replace(sCompareMode, sNewCompareMode);

    if Number <> NewNumber then
    begin
      s := chkFile4.Caption;
      chkFile4.Caption := s.Replace(Format('%d', [Number]), Format('%d', [NewNumber]));
    end;

    case TCompareMode(Value) of
      cmVert, cmSlant:
      begin
        edtExportCodeNameValueCount.Left := 690;
        edtExportCodeNameValueCount2.Left := 770;
      end;
      cmVertSlant:
      begin
        edtExportCodeNameValueCount.Left := 770;
        edtExportCodeNameValueCount2.Left := 850;
      end;
    end;

    fCompareMode := Value;
  end;
end;

procedure TfrmExportFileSettings.FormCreate(Sender: TObject);
begin
  fCompareMode := Ord(cmVertSlant);
end;

procedure TfrmExportFileSettings.FormShow(Sender: TObject);
var
  v: Variant;
begin
  fKeyValue.GetKeyValue('ExportCodeNameValueCount', v);
  if not VarIsEmpty(v) then edtExportCodeNameValueCount.Text := v;
  fKeyValue.GetKeyValue('ExportCodeNameValueCount2', v);
  if not VarIsEmpty(v) then edtExportCodeNameValueCount2.Text := v;
  fKeyValue.GetKeyValue('KeepExportCodeNameValueCount', v);
  chkKeepExportCodeNameValueCount.Checked := True;
  if not VarIsEmpty(v) then chkKeepExportCodeNameValueCount.Checked := v;
  fKeyValue.GetKeyValue('KeepLastBatchCodeNameOnEachComputer', v);
  chkKeepLastBatchCodeNameOnEachComputer.Checked := True;
  if not VarIsEmpty(v) then chkKeepLastBatchCodeNameOnEachComputer.Checked := v;
  fKeyValue.GetKeyValue('ExportFile', v);
  if not VarIsEmpty(v) then chkFile.Checked := v;
  fKeyValue.GetKeyValue('ExportFile2', v);
  if not VarIsEmpty(v) then chkFile2.Checked := v;
  fKeyValue.GetKeyValue('ExportFile3', v);
  if not VarIsEmpty(v) then chkFile3.Checked := v;
  fKeyValue.GetKeyValue('ExportFile4', v);
  if not VarIsEmpty(v) then chkFile4.Checked := v;
end;

procedure TfrmExportFileSettings.btnOkClick(Sender: TObject);
var
  i, v, ValueCount, ValueCount2, GroupFirstNumberCount, GroupCountEachFirstNumber: Integer;
begin
  if not CheckExportCodeNameValueCount(ValueCount, ValueCount2) then
    raise Exception.Create('请输入有效导出代号个数范围');

  fKeyValue.SetKeyValue('ExportCodeNameValueCount', ValueCount);
  fKeyValue.SetKeyValue('ExportCodeNameValueCount2', ValueCount2);
  fKeyValue.SetKeyValue('KeepExportCodeNameValueCount', chkKeepExportCodeNameValueCount.Checked);
  fKeyValue.SetKeyValue('KeepLastBatchCodeNameOnEachComputer', chkKeepLastBatchCodeNameOnEachComputer.Checked);
  fKeyValue.SetKeyValue('ExportFile', chkFile.Checked);
  fKeyValue.SetKeyValue('ExportFile2', chkFile2.Checked);
  fKeyValue.SetKeyValue('ExportFile3', chkFile3.Checked);
  fKeyValue.SetKeyValue('ExportFile4', chkFile4.Checked);

  ModalResult := mrOK;
end;

function TfrmExportFileSettings.CheckExportCodeNameValueCount(var ValueCount, ValueCount2: Integer): Boolean;
begin
  Result := edtExportCodeNameValueCount.TryToValue(ValueCount);
  if not Result then Exit;
  Result := edtExportCodeNameValueCount2.TryToValue(ValueCount2);
  if not Result then Exit;
  Result := (ValueCount <= ValueCount2) and (ValueCount2 > 0);
  if not Result then Exit;
end;

end.
