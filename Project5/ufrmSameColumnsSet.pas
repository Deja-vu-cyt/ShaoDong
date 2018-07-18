unit ufrmSameColumnsSet;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.Math, System.Types,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfrmSameColumnsSet = class(TForm)
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
    fIntervalValues: TWordDynArray;
    fValues: TWordDynArray;
    fValueChanged: Boolean;
    fRowCount: Cardinal;
    fEachPageRowCount: Word;
    fPageNo: Cardinal;
    procedure CheckBoxClick(Sender: TObject);
    procedure CheckBoxCtrlClick(Sender: TObject);
    procedure SetIntervalValues(Value: TWordDynArray);
    procedure SetRowCount(Value: Cardinal);
  public
    property IntervalValues: TWordDynArray read fIntervalValues write SetIntervalValues;
    property Values: TWordDynArray read fValues;
    property ValueChanged: Boolean read fValueChanged;
    property RowCount: Cardinal read fRowCount write SetRowCount;
    property EachPageRowCount: Word read fEachPageRowCount;
    property PageNo: Cardinal read fPageNo;
  end;

var
  frmSameColumnsSet: TfrmSameColumnsSet;

const
  ValueArr: array[0..12] of Integer = (19, 20, 20, 20, 20, 20, 20, 20, 20,
    20, 20, 20, 17);
  OffsetArr: array[0..12] of Integer = (1, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0);

procedure SameColumnsSet(aIntervalValues: TWordDynArray);

implementation

{$R *.dfm}

procedure SameColumnsSet(aIntervalValues: TWordDynArray);
begin
  if not Assigned(frmSameColumnsSet) then
    frmSameColumnsSet := TfrmSameColumnsSet.Create(Application);
  frmSameColumnsSet.ShowModal;
end;

procedure TfrmSameColumnsSet.SetIntervalValues(Value: TWordDynArray);
var
  i, j, v, v2, MaxLeft, IntervalIndex, IntervalValue: Integer;
begin
  if Length(Value) = 0 then Exit;
  fIntervalValues := Value;

  for i := ControlCount - 1 downto 0 do
    if (Controls[i] is TCheckBox) then TCheckBox(Controls[i]).Free;

  IntervalIndex := 0;
  IntervalValue := 0;
  MaxLeft := 0;
  v := 0;
  for i := Low(ValueArr) to High(ValueArr) do
    for j := 1 to ValueArr[i] do
    begin
      Inc(v);
      if v2 > IntervalValue + fIntervalValues[IntervalIndex] then
      begin
        IntervalValue := IntervalValue + fIntervalValues[IntervalIndex];
        Inc(IntervalIndex)
      end;
      v2 := v - IntervalValue;

      with TCheckBox.Create(Self) do
      begin
        Name := Format('chk%d_%d', [i, j]);
        Parent := Self;
        Font.Size := 14;
        Caption := v2.ToString;
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

procedure TfrmSameColumnsSet.SetRowCount(Value: Cardinal);
begin
  if not ((Value > 0) and (fRowCount <> Value)) then Exit;
  fRowCount := Value;

  lblMaxPageNo.Caption := Format('/%d页，总行数：%d', [Ceil(fRowCount / fEachPageRowCount), fRowCount]);
end;

procedure TfrmSameColumnsSet.FormCreate(Sender: TObject);
begin
  IntervalValues := [256];
  fValues := [];
end;

procedure TfrmSameColumnsSet.FormResize(Sender: TObject);
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

procedure TfrmSameColumnsSet.FormShow(Sender: TObject);
begin
  FValueChanged := False;
end;

procedure TfrmSameColumnsSet.btnOkClick(Sender: TObject);
var
  i: Integer;
begin
  if FValueChanged then
  begin
    SetLength(fValues, 0);
    for i := 0 to ControlCount - 1 do
    begin
      if not (Controls[i] is TCheckBox) then Continue;
      with TCheckBox(Controls[i]) do
      begin
        if string(Name).Contains('chkControl') then Continue;

        if Checked then
        begin
          SetLength(fValues, Length(fValues) + 1);
          fValues[Length(fValues) - 1] := Tag;
        end;
      end;
    end;

    edtPageNo.Text := '1';
  end;

  if not (TryStrToInt(edtEachPageRowCount.Text, i) and (i > 0)) then
    raise Exception.Create('请输入有效每页行数');
  fEachPageRowCount := i;
  if not (TryStrToInt(edtPageNo.Text, i) and (i > 0)) then
    raise Exception.Create('请输入有效页号');
  fPageNo := i;

  ModalResult := mrOk;
end;

procedure TfrmSameColumnsSet.CheckBoxClick(Sender: TObject);
begin
  FValueChanged := True;
end;

procedure TfrmSameColumnsSet.CheckBoxCtrlClick(Sender: TObject);
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

procedure TfrmSameColumnsSet.edtKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then btnOkClick(nil);
end;

end.
