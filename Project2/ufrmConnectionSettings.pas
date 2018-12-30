unit ufrmConnectionSettings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmConnectionSettings = class(TForm)
    Label1: TLabel;
    edtAddress: TEdit;
    Label2: TLabel;
    edtPort: TEdit;
    btnOk: TButton;
    procedure btnOkClick(Sender: TObject);
  private
    fAddress: string;
    fPort: string;
  public
    property Address: string read fAddress;
    property Port: string read fPort;
  end;

var
  frmConnectionSettings: TfrmConnectionSettings;

function ConnectionSettings: Boolean;

implementation

{$R *.dfm}

function ConnectionSettings: Boolean;
begin
  if not Assigned(frmConnectionSettings) then
    frmConnectionSettings := TfrmConnectionSettings.Create(Application);
  frmConnectionSettings.ShowModal;
  Result := frmConnectionSettings.ModalResult = mrOk;
end;

procedure TfrmConnectionSettings.btnOkClick(Sender: TObject);
var
  i: Integer;
begin
  fAddress := Trim(edtAddress.Text);
  if fAddress.IsEmpty then raise Exception.Create('请输入有效地址');
  fPort := Trim(edtPort .Text);
  if not (not fPort.IsEmpty and TryStrToInt(fPort, i) and (i > 0)) then
    raise Exception.Create('请输入有效端口');

  ModalResult := mrOk;
end;

end.
