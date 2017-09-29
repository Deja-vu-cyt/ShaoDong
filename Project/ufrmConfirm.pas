unit ufrmConfirm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TfrmConfirm = class(TForm)
    btnYes: TButton;
    btnNo: TButton;
    mmoPrompt: TMemo;
    Panel1: TPanel;
    procedure btnYesClick(Sender: TObject);
    procedure btnNoClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public

  end;

var
  frmConfirm: TfrmConfirm;

function Confirm(const ACaption, APrompt: string): Boolean;

implementation

{$R *.dfm}

function Confirm(const ACaption, APrompt: string): Boolean;
begin
  Result := False;
  if not Assigned(frmConfirm) then frmConfirm := TfrmConfirm.Create(Application);
  frmConfirm.Caption := ACaption;
  frmConfirm.mmoPrompt.Text := APrompt;
  frmConfirm.ShowModal;
  Result := frmConfirm.ModalResult = mrOk;
end;

procedure TfrmConfirm.btnNoClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmConfirm.btnYesClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmConfirm.FormShow(Sender: TObject);
begin
  if Caption = '' then Caption := 'ÌבÊ¾';
end;

end.
