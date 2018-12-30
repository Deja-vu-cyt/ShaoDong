unit ufrmCombinationCalculator;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.Types, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmCombinationCalculator = class(TForm)
    edtN: TEdit;
    edtR: TEdit;
    btnOk: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblResult: TLabel;
    procedure btnOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmCombinationCalculator: TfrmCombinationCalculator;

procedure CombinationCalculator;

implementation

{$R *.dfm}

procedure CombinationCalculator;
begin
  if not Assigned(frmCombinationCalculator) then
    frmCombinationCalculator := TfrmCombinationCalculator.Create(Application);
  frmCombinationCalculator.ShowModal;
end;

function GCD(a, b: Word): Word;
var
  r: Word;
begin
  if b > a then
  begin
    r := a;
    a := b;
    b := r;
  end;
  repeat
    r := a mod b;
    if r > 0 then
    begin
      a := b;
      b := r;
    end;
  until r = 0;
  Result := b;
end;

function CombinationCount(n: Word; r: Word): UInt64;
var
  Factorial, Factorial2: TWordDynArray;
  FactorialValue, FactorialValue2: UInt64;
  i, j: Integer;
  cd: word;
begin
  Result := 0;
  if (n < 0) or (r < 0) or (n < r) then Exit;
  SetLength(Factorial, r);
  for i := Low(Factorial) to High(Factorial) do Factorial[i] := n - i;
  SetLength(Factorial2, r);
  for i := Low(Factorial2) to High(Factorial2) do Factorial2[i] := r - i;

  for i := Low(Factorial) to High(Factorial) do
  begin
    for j := Low(Factorial2) to High(Factorial2) do
    begin
      if Factorial2[j] > 1 then
      begin
        cd := GCD(Factorial[i], Factorial2[j]);
        if cd = 1 then Continue;
        Factorial[i] := Factorial[i] div cd;
        Factorial2[j] := Factorial2[j] div cd;
        if Factorial[i] = 1 then Break;
      end;
    end;
  end;
  {FactorialValue := 1;
  for i := Low(Factorial) to High(Factorial) do
    FactorialValue := FactorialValue * Factorial[i];
  FactorialValue2 := 1;
  for i := Low(Factorial2) to High(Factorial2) do
    FactorialValue2 := FactorialValue2 * Factorial2[i];

  Result := FactorialValue div FactorialValue2;}

  Result := 1;
  for i := Low(Factorial) to High(Factorial) do
    Result := Result * Factorial[i];
end;

procedure TfrmCombinationCalculator.btnOkClick(Sender: TObject);
var
  n, r: Integer;
begin
  TryStrToInt(edtN.Text, n);
  TryStrToInt(edtR.Text, r);
  lblResult.Caption := CombinationCount(n, r).ToString;
  lblResult.Hint := Length(lblResult.Caption).ToString;
end;

end.
