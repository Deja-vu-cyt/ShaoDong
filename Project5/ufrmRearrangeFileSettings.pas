unit ufrmRearrangeFileSettings;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Types, System.IOUtils,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.FileCtrl;

type
  TfrmRearrangeFileSettings = class(TForm)
    btnOK: TButton;
    Label1: TLabel;
    edtFirstIntervalCol: TEdit;
    Label2: TLabel;
    edtPlaceholderFileName: TEdit;
    edtFirstIntervalCol2: TEdit;
    edtSecondIntervalCol2: TEdit;
    Label3: TLabel;
    edtSecondIntervalCol: TEdit;
    OpenDialog: TOpenDialog;
    edtFileDirectory: TEdit;
    Label4: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure edtPlaceholderFileNameClick(Sender: TObject);
    procedure edtFileDirectoryClick(Sender: TObject);
  private
    fFirstIntervalCol: Word;
    fFirstIntervalCol2: Word;
    fSecondIntervalCol: Word;
    fSecondIntervalCol2: Word;
    fPlaceholder: string;
    fFileDirectory: string;
  public
    property FirstIntervalCol: Word read fFirstIntervalCol;
    property FirstIntervalCol2: Word read fFirstIntervalCol2;
    property SecondIntervalCol: Word read fSecondIntervalCol;
    property SecondIntervalCol2: Word read fSecondIntervalCol2;
    property Placeholder: string read fPlaceholder;
    property FileDirectory: string read fFileDirectory;
  end;

var
  frmRearrangeFileSettings: TfrmRearrangeFileSettings;

function RearrangeFileSet: Boolean;

implementation

{$R *.dfm}

function RearrangeFileSet: Boolean;
begin
  if not Assigned(frmRearrangeFileSettings) then
    frmRearrangeFileSettings := TfrmRearrangeFileSettings.Create(Application);
  frmRearrangeFileSettings.ShowModal;
  Result := frmRearrangeFileSettings.ModalResult = mrOK;
end;

procedure TfrmRearrangeFileSettings.btnOKClick(Sender: TObject);
var
  v: Integer;
  l: TStringList;
begin
  if not (TryStrToInt(edtFirstIntervalCol.Text, v) and (v > 0)) then
    raise Exception.Create('请输入有效区域位置');
  fFirstIntervalCol := v;
  if not (TryStrToInt(edtFirstIntervalCol2.Text, v) and (v > 0)) then
    raise Exception.Create('请输入有效区域位置');
  fFirstIntervalCol2 := v;
  fSecondIntervalCol := 0;
  if (edtSecondIntervalCol.Text <> '') and TryStrToInt(edtSecondIntervalCol.Text, v) then
    fSecondIntervalCol := v;
  fSecondIntervalCol2 := 0;
  if (edtSecondIntervalCol2.Text <> '') and TryStrToInt(edtSecondIntervalCol2.Text, v) then
    fSecondIntervalCol2 := v;

  if FileExists(edtPlaceholderFileName.Text) then
  begin
    l := TStringList.Create;
    try
      l.LoadFromFile(edtPlaceholderFileName.Text);
      fPlaceholder := l.Text.Replace(' ', '');
    finally
      l.Free;
    end;
  end;

  fFileDirectory := edtFileDirectory.Text;

  ModalResult := mrOK;
end;

procedure TfrmRearrangeFileSettings.edtFileDirectoryClick(Sender: TObject);
var
  FileDirectory: string;
begin
  if not SelectDirectory('选择文件夹', '', FileDirectory) then Exit;
  TEdit(Sender).Text := FileDirectory;
end;

procedure TfrmRearrangeFileSettings.edtPlaceholderFileNameClick(Sender: TObject);
var
  FileDirectory: string;
begin
  if not OpenDialog.Execute then Exit;
  TEdit(Sender).Text := OpenDialog.FileName;
end;

end.
