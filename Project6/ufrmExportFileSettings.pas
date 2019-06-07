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
    lblLine.Caption := s.Replace(sCompareMode, sNewCompareMode);

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
begin
  if fSettings.ExportCodeNameValueCount > 0 then
    edtExportCodeNameValueCount.Text := fSettings.ExportCodeNameValueCount.ToString;
  if fSettings.ExportCodeNameValueCount2 > 0 then
    edtExportCodeNameValueCount2.Text := fSettings.ExportCodeNameValueCount2.ToString;
  chkKeepLastBatchCodeNameOnEachComputer.Checked := fSettings.KeepLastBatchCodeNameOnEachComputer;
  chkFile.Checked := fSettings.ExportFile;
  chkFile2.Checked := fSettings.ExportFile2;
  chkFile3.Checked := fSettings.ExportFile3;
  chkFile4.Checked := fSettings.ExportFile4;
end;

procedure TfrmExportFileSettings.btnOkClick(Sender: TObject);
var
  i, v, ValueCount, ValueCount2, GroupFirstNumberCount, GroupCountEachFirstNumber: Integer;
begin
  if not CheckExportCodeNameValueCount(ValueCount, ValueCount2) then
    raise Exception.Create('请输入有效遍历次数');

  fSettings.ExportCodeNameValueCount := ValueCount;
  fSettings.ExportCodeNameValueCount2 := ValueCount2;
  fSettings.KeepLastBatchCodeNameOnEachComputer := chkKeepLastBatchCodeNameOnEachComputer.Checked;
  fSettings.ExportFile := chkFile.Checked;
  fSettings.ExportFile2 := chkFile2.Checked;
  fSettings.ExportFile3 := chkFile3.Checked;
  fSettings.ExportFile4 := chkFile4.Checked;
  fKeyValue.SetKeyValue('ExportCodeNameValueCount', fSettings.ExportCodeNameValueCount);
  fKeyValue.SetKeyValue('ExportCodeNameValueCount2', fSettings.ExportCodeNameValueCount2);
  fKeyValue.SetKeyValue('KeepLastBatchCodeNameOnEachComputer', fSettings.KeepLastBatchCodeNameOnEachComputer);
  fKeyValue.SetKeyValue('ExportFile', fSettings.ExportFile);
  fKeyValue.SetKeyValue('ExportFile2', fSettings.ExportFile2);
  fKeyValue.SetKeyValue('ExportFile3', fSettings.ExportFile3);
  fKeyValue.SetKeyValue('ExportFile4', fSettings.ExportFile4);

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
