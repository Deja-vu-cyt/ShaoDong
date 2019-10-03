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
    procedure FormCreate(Sender: TObject);
    procedure edtFileDirectoryClick(Sender: TObject);
    procedure btnConfirmClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure chkSelectAllClick(Sender: TObject);
  private
    fExportFiles: TInt64DynArray;
    fDeleteProcessed: Boolean;
  public
    property ExportFiles: TInt64DynArray read fExportFiles write fExportFiles;
    property DeleteProcessed: Boolean read fDeleteProcessed write fDeleteProcessed;
  end;

var
  frmExportSettings4: TfrmExportSettings4;

implementation

uses
  uControlHelper, uCommon;

{$R *.dfm}

procedure TfrmExportSettings4.chkSelectAllClick(Sender: TObject);
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

procedure TfrmExportSettings4.edtFileDirectoryClick(Sender: TObject);
var
  FileDirectory: string;
begin
  if not SelectDirectory('', '', FileDirectory) then Exit;
  TEdit(Sender).Text := FileDirectory;
end;

procedure TfrmExportSettings4.FormCreate(Sender: TObject);
var
  v: Variant;
begin
  ScrollBox1.VertScrollBar.Position := 0;

  fKeyValue.GetKeyValue('ExportFiles', fExportFiles);
  fKeyValue.GetKeyValue('DeleteProcessed', v);
  if not VarIsEmpty(v) then fDeleteProcessed := v;
end;

procedure TfrmExportSettings4.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position - WheelDelta div 2;
end;

procedure TfrmExportSettings4.FormShow(Sender: TObject);
var
  v: Int64;
begin
  chkExportFile.Checked := False;
  chkExportFile2.Checked := False;
  chkExportFile3.Checked := False;
  chkExportFile4.Checked := False;
  chkExportFile5.Checked := False;
  chkExportFile6.Checked := False;
  chkExportFile7.Checked := False;
  chkExportFile8.Checked := False;
  for v in fExportFiles do
    case v.ValueCount of
      1:
      begin
        if v.ValueExist(1) then chkExportFile.Checked := True;
        if v.ValueExist(2) then chkExportFile2.Checked := True;
        if v.ValueExist(3) then chkExportFile3.Checked := True;
        if v.ValueExist(4) then chkExportFile4.Checked := True;
      end;
      2:
      begin
        if v.ValueExist(1) and v.ValueExist(3) then chkExportFile5.Checked := True;
        if v.ValueExist(1) and v.ValueExist(4) then chkExportFile6.Checked := True;
        if v.ValueExist(2) and v.ValueExist(3) then chkExportFile7.Checked := True;
        if v.ValueExist(2) and v.ValueExist(4) then chkExportFile8.Checked := True;
      end;
    end;
  chkDeleteProcessed.Checked := fDeleteProcessed;
end;

procedure TfrmExportSettings4.btnConfirmClick(Sender: TObject);
var
  v: Int64;
begin
  SetLength(fExportFiles, 0);
  v := 0;
  if chkExportFile.Checked then
  begin
    v.AddValue(1);
    SetLength(fExportFiles, Length(fExportFiles) + 1);
    fExportFiles[High(fExportFiles)] := v;
  end;
  v := 0;
  if chkExportFile2.Checked then
  begin
    v.AddValue(2);
    SetLength(fExportFiles, Length(fExportFiles) + 1);
    fExportFiles[High(fExportFiles)] := v;
  end;
  v := 0;
  if chkExportFile3.Checked then
  begin
    v.AddValue(3);
    SetLength(fExportFiles, Length(fExportFiles) + 1);
    fExportFiles[High(fExportFiles)] := v;
  end;
  v := 0;
  if chkExportFile4.Checked then
  begin
    v.AddValue(4);
    SetLength(fExportFiles, Length(fExportFiles) + 1);
    fExportFiles[High(fExportFiles)] := v;
  end;
  v := 0;
  if chkExportFile5.Checked then
  begin
    v.AddValue(1);
    v.AddValue(3);
    SetLength(fExportFiles, Length(fExportFiles) + 1);
    fExportFiles[High(fExportFiles)] := v;
  end;
  v := 0;
  if chkExportFile6.Checked then
  begin
    v.AddValue(1);
    v.AddValue(4);
    SetLength(fExportFiles, Length(fExportFiles) + 1);
    fExportFiles[High(fExportFiles)] := v;
  end;
  v := 0;
  if chkExportFile7.Checked then
  begin
    v.AddValue(2);
    v.AddValue(3);
    SetLength(fExportFiles, Length(fExportFiles) + 1);
    fExportFiles[High(fExportFiles)] := v;
  end;
  v := 0;
  if chkExportFile8.Checked then
  begin
    v.AddValue(2);
    v.AddValue(4);
    SetLength(fExportFiles, Length(fExportFiles) + 1);
    fExportFiles[High(fExportFiles)] := v;
  end;
  fDeleteProcessed := chkDeleteProcessed.Checked;

  fKeyValue.SetKeyValue('ExportFiles', fExportFiles);
  fKeyValue.SetKeyValue('DeleteProcessed', fDeleteProcessed);

  ModalResult := mrOk;
end;

end.
