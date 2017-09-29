unit ufrmQueryConfig2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.Math,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type


  TfrmQueryConfig2 = class(TForm)
    btnOk: TButton;
    Panel1: TPanel;
    lblMaxPageNo: TLabel;
    edtPageNo: TEdit;
    edtEachPageRowCount: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edtKeyPress(Sender: TObject; var Key: Char);
    procedure FormResize(Sender: TObject);
  private
    FValueChanged: Boolean;
    FEachPageRowCount: Integer;
    FPageNo: Integer;
    procedure CheckBoxClick(Sender: TObject);
    procedure CheckBoxCtrlClick(Sender: TObject);
  public
    property ValueChanged: Boolean read FValueChanged;
    property EachPageRowCount: Integer read FEachPageRowCount;
    property PageNo: Integer read FPageNo;
  end;

var
  frmQueryConfig2: TfrmQueryConfig2;

const
  ValueArr: array[0..12] of Integer = (19, 20, 20, 20, 20, 20, 20, 20, 20,
    20, 20, 20, 17);
  OffsetArr: array[0..12] of Integer = (1, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0);

implementation

{$R *.dfm}

uses
  udmProject1;

procedure TfrmQueryConfig2.FormCreate(Sender: TObject);
var
  i, j, v, MaxLeft: Integer;
begin
  MaxLeft := 0;
  v := 0;
  for i := Low(ValueArr) to High(ValueArr) do
    for j := 1 to ValueArr[i] do
    begin
      Inc(v);
      with TCheckBox.Create(Self) do
      begin
        Name := Format('chk%d_%d', [i, j]);
        Parent := Self;
        Font.Size := 14;
        if (dmProject1.ColCount > dmProject1.RangeColNo) and (v > dmProject1.RangeColNo) then
          Caption := (v - dmProject1.RangeColNo).ToString
        else
          Caption := v.ToString;
        Tag := v;
        OnClick := CheckBoxClick;

        if MaxLeft < Left + 52 then MaxLeft := Left + 52;
      end;
    end;

  v := 0;
  for i := Low(ValueArr) to High(ValueArr) do
  begin
    v := v + ValueArr[i];
    with TCheckBox.Create(Self) do
    begin
      Name := Format('chkControl%d', [i]);
      Parent := Self;
      Font.Size := 14;
      Font.Style := [fsBold];
      Tag := i;
      Caption := Format('第%d行全选口√', [i + 1]);
      Width := 200;
      OnClick := CheckBoxCtrlClick;
    end;
  end;
end;

procedure TfrmQueryConfig2.FormResize(Sender: TObject);
var
  s: string;
  i, ri, ci, OffsetLeft:Integer;
  r: TRect;
begin
  r.Left := 0;
  r.Top := 0;
  r.Width := 20 * 62;
  r.Height := Length(ValueArr) * 30;
  r.Left := (ClientWidth - r.Width) div 2;
  if r.Left < 5 then r.Left := 5;
  r.Top := (ClientHeight - r.Height - Panel1.Height) div 2;
  if r.Top < 5 then r.Top := 5;
  for i := 0 to ControlCount - 1 do
  begin
    if not (Controls[i] is TCheckBox) then Continue;

    with Controls[i] as TCheckBox do
    begin
      s := Name;
      if s.IndexOf('chkControl') = -1 then
      begin
        s := s.Replace('chk', '');
        ri := s.Split(['_'])[0].ToInteger;
        ci := s.Split(['_'])[1].ToInteger;
        OffsetLeft := OffsetArr[ri];
      end
      else
      begin
        ri := Tag;
        ci := 21;
        OffsetLeft := 0;
      end;
      Left := r.Left + (OffsetLeft + ci - 1) * 52;
      Top := r.Top + ri * 30;
    end;
  end;
end;

procedure TfrmQueryConfig2.FormShow(Sender: TObject);
begin
  FValueChanged := False;
end;

procedure TfrmQueryConfig2.btnOkClick(Sender: TObject);
begin
  if FValueChanged then edtPageNo.Text := '1';

  if not TryStrToInt(edtEachPageRowCount.Text, FEachPageRowCount) or (FEachPageRowCount < 1) then
    raise Exception.Create('请输入有效每页行数');
  if not TryStrToInt(edtPageNo.Text, FPageNo) or (FPageNo < 1) then
    raise Exception.Create('请输入有效页号');

  ModalResult := mrOk;
end;

procedure TfrmQueryConfig2.CheckBoxClick(Sender: TObject);
begin
  FValueChanged := True;
end;

procedure TfrmQueryConfig2.CheckBoxCtrlClick(Sender: TObject);
var
  i, i1, i2: Integer;
begin
  i2 := 0;
  for i := Low(ValueArr) to (Sender as TCheckBox).Tag do i2 := i2 + ValueArr[i];
  i1 := i2 - ValueArr[i - 1] + 1;
  for i := 0 to ControlCount - 1 do
  begin
    if not (Controls[i] is TCheckBox) then Continue;
    with Controls[i] as TCheckBox do
      if (Pos('chkControl', Name) = 0) and (Tag >= i1) and (Tag <= i2) then
        Checked := (Sender as TCheckBox).Checked;
  end;
  CheckBoxClick(nil);
end;

procedure TfrmQueryConfig2.edtKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then btnOkClick(nil);
end;

end.
