unit ufrmExportSettings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmExportSettings = class(TForm)
    Label2: TLabel;
    edtKeepMaxRowSpacing: TEdit;
    Label1: TLabel;
    edtMinGroupRowCount: TEdit;
    btnOK: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fKeepMaxRowSpacing: Cardinal;
    fMinGroupRowCount: Cardinal;
  public
    property KeepMaxRowSpacing: Cardinal read fKeepMaxRowSpacing;
    property MinGroupRowCount: Cardinal read fMinGroupRowCount;
  end;

var
  frmExportSettings: TfrmExportSettings;

implementation

{$R *.dfm}

procedure TfrmExportSettings.btnOKClick(Sender: TObject);
var
  v: Integer;
begin
  fKeepMaxRowSpacing := 0;
  if not Trim(edtKeepMaxRowSpacing.Text).IsEmpty then
  begin
    if not TryStrToInt(Trim(edtKeepMaxRowSpacing.Text), v) then
      raise Exception.Create('请输入有效最大临行距');
    fKeepMaxRowSpacing := v;
  end;
  fMinGroupRowCount := 0;
  if not Trim(edtMinGroupRowCount.Text).IsEmpty then
  begin
    if not TryStrToInt(Trim(edtMinGroupRowCount.Text), v) then
      raise Exception.Create('请输入有效最小首行数');
    fMinGroupRowCount := v;
  end;
  ModalResult := mrOK;
end;

procedure TfrmExportSettings.FormCreate(Sender: TObject);
begin
  fKeepMaxRowSpacing := 0;
  fMinGroupRowCount := 0;
end;

end.
