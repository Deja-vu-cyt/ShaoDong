unit ufrmDeleteSameColumnSet;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Types,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmDeleteSameColumnSet = class(TForm)
    edtIntervalValue: TEdit;
    Label1: TLabel;
    edtIntervalValue2: TEdit;
    Label2: TLabel;
    btnOK: TButton;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fSameColumnValues: TWordDynArray;
  public
    property SameColumnValues: TWordDynArray read fSameColumnValues;
  end;

var
  frmDeleteSameColumnSet: TfrmDeleteSameColumnSet;

procedure DeleteSameColumnSet;

implementation

{$R *.dfm}

procedure DeleteSameColumnSet;
begin
  if not Assigned(frmDeleteSameColumnSet) then
    frmDeleteSameColumnSet := TfrmDeleteSameColumnSet.Create(Application);
  frmDeleteSameColumnSet.ShowModal;
end;

procedure TfrmDeleteSameColumnSet.btnOKClick(Sender: TObject);
var
  v: Integer;
begin
  if not (TryStrToInt(edtIntervalValue.Text, v) and (v > 0)) then
  begin
    edtIntervalValue.SetFocus;
    edtIntervalValue.SelectAll;
    raise Exception.Create('请输入有效值');
  end;
  fSameColumnValues[0] := v;
  if not TryStrToInt(edtIntervalValue2.Text, v) then
  begin
    edtIntervalValue2.SetFocus;
    edtIntervalValue2.SelectAll;
    raise Exception.Create('请输入有效值');
  end;
  fSameColumnValues[1] := v;

  ModalResult := mrOK;
end;

procedure TfrmDeleteSameColumnSet.FormCreate(Sender: TObject);
begin
  SetLength(fSameColumnValues, 2);
end;

end.
