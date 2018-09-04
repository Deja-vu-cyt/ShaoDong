unit ufrmIntervalValueSet;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Types,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmIntervalValueSet = class(TForm)
    edtIntervalValue: TEdit;
    Label1: TLabel;
    edtIntervalValue2: TEdit;
    Label2: TLabel;
    btnOK: TButton;
    procedure btnOKClick(Sender: TObject);
  private
    fIntervalValues: TWordDynArray;
  public
    property IntervalValues: TWordDynArray read fIntervalValues;
  end;

var
  frmIntervalValueSet: TfrmIntervalValueSet;

procedure IntervalValueSet;

implementation

{$R *.dfm}

procedure IntervalValueSet;
begin
  if not Assigned(frmIntervalValueSet) then
    frmIntervalValueSet := TfrmIntervalValueSet.Create(Application);
  frmIntervalValueSet.ShowModal;
end;

procedure TfrmIntervalValueSet.btnOKClick(Sender: TObject);
var
  v, v2: Integer;
begin
  if not (TryStrToInt(edtIntervalValue.Text, v) and (v >= 1) and (v <= 256)) then
  begin
    edtIntervalValue.SetFocus;
    edtIntervalValue.SelectAll;
    raise Exception.Create('请输入有效值');
  end;
  if not (TryStrToInt(edtIntervalValue2.Text, v2) and (v2 >= 0) and (v2 < 256)) then
  begin
    edtIntervalValue2.SetFocus;
    edtIntervalValue2.SelectAll;
    raise Exception.Create('请输入有效值');
  end;
  if v + v2 > 256 then raise Exception.Create('总列数不能超过256');

  SetLength(fIntervalValues, 1);
  fIntervalValues[0] := v;
  if v2 > 0 then
  begin
    SetLength(fIntervalValues, 2);
    fIntervalValues[1] := v2;
  end;

  ModalResult := mrOK;
end;

end.
