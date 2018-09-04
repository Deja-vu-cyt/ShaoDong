unit ufrmQueryConfig3;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.Types,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmQueryConfig3 = class(TForm)
    btnOk: TButton;
    edtIdenticalColCount: TEdit;
    Label3: TLabel;
    edtCompareRowCount: TEdit;
    Label4: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    edtIntervalValue: TEdit;
    edtIntervalValue2: TEdit;
    Label5: TLabel;
    procedure btnOkClick(Sender: TObject);
  private
    fIntervalValues: TWordDynArray;
    fIdenticalColCount: Word;
    fCompareRowCount: Word;
  public
    property IntervalValues: TWordDynArray read fIntervalValues;
    property IdenticalColCount: Word read fIdenticalColCount;
    property CompareRowCount: Word read fCompareRowCount;
  end;

var
  frmQueryConfig3: TfrmQueryConfig3;

implementation

{$R *.dfm}

procedure TfrmQueryConfig3.btnOkClick(Sender: TObject);
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

  if not (TryStrToInt(edtIdenticalColCount.Text, v) and (v >= 0)) then
  begin
    edtIdenticalColCount.SetFocus;
    edtIdenticalColCount.SelectAll;
    raise Exception.Create('请输入有效值');
  end;
  fIdenticalColCount := v;

  if not (TryStrToInt(edtCompareRowCount.Text, v) and (v > 0)) then
  begin
    edtCompareRowCount.SetFocus;
    edtCompareRowCount.SelectAll;
    raise Exception.Create('请输入有效值');
  end;
  fCompareRowCount := v;

  ModalResult := mrOK;
end;

end.
