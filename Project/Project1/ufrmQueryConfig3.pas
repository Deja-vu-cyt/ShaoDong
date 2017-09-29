unit ufrmQueryConfig3;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmQueryConfig3 = class(TForm)
    btnOk: TButton;
    edtIdenticalColCount: TEdit;
    Label3: TLabel;
    edtCompareRowCount: TEdit;
    Label4: TLabel;
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FValueChanged: Boolean;
    FEachPageRowCount: Integer;
    FPageNo: Integer;
  public
    property ValueChanged: Boolean read FValueChanged;
    procedure ResetState;
  end;

var
  frmQueryConfig3: TfrmQueryConfig3;

implementation

{$R *.dfm}

procedure TfrmQueryConfig3.btnOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmQueryConfig3.FormCreate(Sender: TObject);
begin
  FValueChanged := True;
end;

procedure TfrmQueryConfig3.ResetState;
begin
  FValueChanged := False;
end;

end.
