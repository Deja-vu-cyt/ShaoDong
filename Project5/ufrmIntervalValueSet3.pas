unit ufrmIntervalValueSet3;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ufrmIntervalValueSet, Vcl.StdCtrls;

type
  TfrmIntervalValueSet3 = class(TfrmIntervalValueSet)
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    edtIdenticalColCount: TEdit;
    edtCompareRowCount: TEdit;
    procedure btnOKClick(Sender: TObject);
  private
    fIdenticalColCount: Word;
    fCompareRowCount: Word;
  public
    property IdenticalColCount: Word read fIdenticalColCount;
    property CompareRowCount: Word read fCompareRowCount;
  end;

var
  frmIntervalValueSet3: TfrmIntervalValueSet3;

implementation

{$R *.dfm}

procedure TfrmIntervalValueSet3.btnOKClick(Sender: TObject);
var
  v: Integer;
begin
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
  inherited;
end;

end.
