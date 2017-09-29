unit ufrmQueryConfig;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.Math,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type


  TfrmQueryConfig = class(TForm)
    pnlSelect: TPanel;
    btnOk: TButton;
    pnlControl: TPanel;
    Panel1: TPanel;
    lblMaxPageNo: TLabel;
    edtPageNo: TEdit;
    edtEachPageRowCount: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edtKeyPress(Sender: TObject; var Key: Char);
  private
    FValueChanged: Boolean;
    FEachPageRowCount: Integer;
    FPageNo: Integer;
    procedure CheckBoxClick(Sender: TObject);
  public
    property ValueChanged: Boolean read FValueChanged;
    property EachPageRowCount: Integer read FEachPageRowCount;
    property PageNo: Integer read FPageNo;
  end;

var
  frmQueryConfig: TfrmQueryConfig;

const
  ValueArr: array[0..12] of Integer = (19, 20, 20, 20, 20, 20, 20, 20, 20,
    20, 20, 20, 17);
  OffsetArr: array[0..12] of Integer = (1, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0);

implementation

{$R *.dfm}

procedure TfrmQueryConfig.FormCreate(Sender: TObject);
var
  i, j, v: Integer;
begin
  v := 0;
  for i := Low(ValueArr) to High(ValueArr) do
    for j := 1 to ValueArr[i] do
    begin
      Inc(v);
      with TCheckBox.Create(Self) do
      begin
        Parent := pnlSelect;
        Font.Size := 16;
        Caption := v.ToString;
        Tag := v;
        Left := (OffsetArr[i] + j - 1) * 60 + 10;
        Top := i * 30 + 10;
      end;
    end;
  v := 0;
  for i := Low(ValueArr) to High(ValueArr) do
  begin
    v := v + ValueArr[i];
    with TCheckBox.Create(Self) do
    begin
      Parent := pnlControl;
      Font.Size := 16;
      Tag := i;
      Caption := Format('第%d行全选口√', [i + 1]);
      Left := 10;
      Top := i * 30 + 10;
      Width := 200;
      Height := 30;
      OnClick := CheckBoxClick;
    end;
  end;
end;

procedure TfrmQueryConfig.FormShow(Sender: TObject);
begin
  FValueChanged := False;
end;

procedure TfrmQueryConfig.btnOkClick(Sender: TObject);
begin
  if Sender <> nil then FValueChanged := True;
  if not TryStrToInt(edtEachPageRowCount.Text, FEachPageRowCount) or (FEachPageRowCount < 1) then
    raise Exception.Create('请输入有效每页行数');
  if not TryStrToInt(edtPageNo.Text, FPageNo) or (FPageNo < 1) then
    raise Exception.Create('请输入有效页号');

  ModalResult := mrOk;
end;

procedure TfrmQueryConfig.CheckBoxClick(Sender: TObject);
var
  i, i1, i2: Integer;
begin
  i2 := 0;
  for i := Low(ValueArr) to (Sender as TCheckBox).Tag do i2 := i2 + ValueArr[i];
  i1 := i2 - ValueArr[i - 1] + 1;
  for i := 0 to pnlSelect.ControlCount - 1 do
  begin
    if not (pnlSelect.Controls[i] is TCheckBox) then Continue;
    with pnlSelect.Controls[i] as TCheckBox do
      if (Tag >= i1) and (Tag <= i2) then Checked := (Sender as TCheckBox).Checked;
  end;
end;

procedure TfrmQueryConfig.edtKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then btnOkClick(nil);
end;

end.
