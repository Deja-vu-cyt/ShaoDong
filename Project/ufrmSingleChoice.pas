unit ufrmSingleChoice;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmSingleChoice = class(TForm)
    ListBox: TListBox;
    btnOk: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSingleChoice: TfrmSingleChoice;

function SingleChoice(const ACaption, AValue: string): Integer;

implementation

{$R *.dfm}

function SingleChoice(const ACaption, AValue: string): Integer;
begin
  Result := -1;
  if not Assigned(frmSingleChoice) then frmSingleChoice := TfrmSingleChoice.Create(Application);
  frmSingleChoice.Caption := ACaption;
  frmSingleChoice.ListBox.Items.Text := AValue;
  if frmSingleChoice.ListBox.Items.Count > 0 then
    frmSingleChoice.ListBox.ItemIndex := 0;
  frmSingleChoice.ShowModal;
  if frmSingleChoice.ModalResult = mrOk then
    Result := frmSingleChoice.ListBox.ItemIndex;
end;

procedure TfrmSingleChoice.btnOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmSingleChoice.FormShow(Sender: TObject);
begin
  if Caption = '' then Caption := 'ÌבÊ¾';
end;

end.
