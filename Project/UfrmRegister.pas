unit ufrmRegister;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmRegister = class(TForm)
    Label1: TLabel;
    edtMachineCode: TEdit;
    Label2: TLabel;
    edtRegiesterCode: TEdit;
    btnRegister: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmRegister: TfrmRegister;

implementation

{$R *.dfm}

end.
