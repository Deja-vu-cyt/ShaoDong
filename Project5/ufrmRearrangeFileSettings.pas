unit ufrmRearrangeFileSettings;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Types, System.IOUtils,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.FileCtrl;

type
  TfrmRearrangeFileSettings = class(TForm)
    btnOK: TButton;
    edtPlaceholderFileName: TEdit;
    Label3: TLabel;
    OpenDialog: TOpenDialog;
    Label5: TLabel;
    edtFileDirectory: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edtFileDirectory2: TEdit;
    Label6: TLabel;
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
  fFileDirectory := Trim(edtFileDirectory.Text);
  if fFileDirectory.IsEmpty then
  begin
    fFileDirectory := Trim(edtFileDirectory2.Text);
    if fFileDirectory.IsEmpty then
      raise Exception.Create('请选择文件夹');
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

    fFirstIntervalCol := 18;
    fFirstIntervalCol2 := 29;
    fSecondIntervalCol := 30;
    fSecondIntervalCol2 := 31;
  end
  else
  begin
    fFirstIntervalCol := 7;
    fFirstIntervalCol2 := 14;
    fSecondIntervalCol := 0;
    fSecondIntervalCol2 := 0;
  end;
  //暂时已占位符区分模式
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
