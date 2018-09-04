unit ufrmExportFileSettings;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Types, System.IOUtils,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.FileCtrl;

type
  TfrmExportFileSettings = class(TForm)
    btnOK: TButton;
    rbOneFileDirectory: TRadioButton;
    rbSubFileDirectory: TRadioButton;
    Label1: TLabel;
    edtEachPageRowCount: TEdit;
    Label2: TLabel;
    edtExportFileDirectory: TEdit;
    procedure btnOKClick(Sender: TObject);
    procedure edtExportFileDirectoryClick(Sender: TObject);
  private
    fExportFileDirectory: string;
    fEachFileRowCount: Word;
  public
    property ExportFileDirectory: string read fExportFileDirectory;
    property EachFileRowCount: Word read fEachFileRowCount;
  end;

var
  frmExportFileSettings: TfrmExportFileSettings;

function ExportFileSet: Boolean;

implementation

{$R *.dfm}

function ExportFileSet: Boolean;
begin
  if not Assigned(frmExportFileSettings) then
    frmExportFileSettings := TfrmExportFileSettings.Create(Application);
  frmExportFileSettings.ShowModal;
  Result := frmExportFileSettings.ModalResult = mrOK;
end;

procedure TfrmExportFileSettings.btnOKClick(Sender: TObject);
var
  v: Integer;
begin
  if not TDirectory.Exists(edtExportFileDirectory.Text) then
    raise Exception.Create('请选择有效目录');
  fExportFileDirectory := edtExportFileDirectory.Text;
  if not (TryStrToInt(edtEachPageRowCount.Text, v) and (v > 0)) then
    raise Exception.Create('请选择有效"平均多少行/文本"');
  fEachFileRowCount := v;

  ModalResult := mrOK;
end;

procedure TfrmExportFileSettings.edtExportFileDirectoryClick(Sender: TObject);
var
  FileDirectory: string;
begin
  if not SelectDirectory('导出数据文件夹', '', FileDirectory) then Exit;
  TEdit(Sender).Text := FileDirectory;
end;

end.
