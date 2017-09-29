unit ufrmProject;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option, Vcl.FileCtrl,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Vcl.StdCtrls, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, Vcl.Grids, Vcl.DBGrids, DBGridEhGrouping, ToolCtrlsEh,
  DBGridEhToolCtrls, DynVarsEh, EhLibVCL, GridsEh, DBAxisGridsEh, DBGridEh;

type
  TfrmProject = class(TForm)
    btnAddProject: TButton;
    btnDeleteProject: TButton;
    dbgrdProject: TDBGridEh;
    btnOk: TButton;
    procedure btnAddProjectClick(Sender: TObject);
    procedure edtProjectPathDblClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure dbgrdProjectDblClick(Sender: TObject);
    procedure btnDeleteProjectClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmProject: TfrmProject;

implementation

uses
  udmConfig, udmProject1;

{$R *.dfm}

procedure TfrmProject.btnAddProjectClick(Sender: TObject);
begin
  dmProject1.CreateDatabase;
end;

procedure TfrmProject.btnDeleteProjectClick(Sender: TObject);
begin
  dmProject1.DeleteDatabase;
end;

procedure TfrmProject.btnOkClick(Sender: TObject);
begin
  if dmProject1.fdqProject.RecordCount = 0 then Exit;
  ModalResult := mrOk;
end;

procedure TfrmProject.dbgrdProjectDblClick(Sender: TObject);
begin
  btnOk.Click;
end;

procedure TfrmProject.edtProjectPathDblClick(Sender: TObject);
var
  FilePath: string;
begin
  if not SelectDirectory('选择导出目录', '', FilePath) then Exit;
  if Copy(FilePath, Length(FilePath) - 1, 1) <> '\' then FilePath := FilePath + '\';
  (Sender as TEdit).Text := FilePath;
end;

end.
