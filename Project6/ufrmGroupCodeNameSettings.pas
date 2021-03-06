unit ufrmGroupCodeNameSettings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmGroupCodeNameSettings = class(TForm)
    lblLine: TLabel;
    edtGroupNumber: TEdit;
    edtGroupNumber2: TEdit;
    edtGroupCountEachFirstNumber: TEdit;
    lblLine1: TLabel;
    edtGroupFirstNumberCount: TEdit;
    lblLine2: TLabel;
    btnOk: TButton;
    Label1: TLabel;
    procedure FormShow(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fNumber: Byte;
    procedure SetNumber(Value: Byte);
    function CheckGroupNumber(var GroupNumber, GroupNumber2: Integer): Boolean;
  public
    property Number: Byte read fNumber write SetNumber;
  end;

var
  frmGroupCodeNameSettings: TfrmGroupCodeNameSettings;

implementation

uses
  uDataComputer, uControlHelper;

{$R *.dfm}

procedure TfrmGroupCodeNameSettings.SetNumber(Value: Byte);
var
  s: string;
begin
  if fNumber <> Value then
  begin
    s := lblLine.Caption;
    lblLine.Caption := s.Replace(Format('%d-2', [fNumber]), Format('%d-2', [Value]));
    s := lblLine1.Caption;
    lblLine1.Caption := s.Replace(Format('%d-2', [fNumber]), Format('%d-2', [Value]));
    s := lblLine2.Caption;
    lblLine2.Caption := s.Replace(Format('%d-2', [fNumber]), Format('%d-2', [Value]));

    fNumber := Value;
  end;
end;

procedure TfrmGroupCodeNameSettings.FormCreate(Sender: TObject);
begin
  fNumber := 6;
end;

procedure TfrmGroupCodeNameSettings.FormShow(Sender: TObject);
var
  v: Variant;
begin
  fKeyValue.GetKeyValue('GroupNumber', v);
  if not VarIsEmpty(v) then edtGroupNumber.Text := v;
  fKeyValue.GetKeyValue('GroupNumber2', v);
  if not VarIsEmpty(v) then edtGroupNumber2.Text := v;
  fKeyValue.GetKeyValue('GroupFirstNumberCount', v);
  if not VarIsEmpty(v) then edtGroupFirstNumberCount.Text := v;
  fKeyValue.GetKeyValue('GroupCountEachFirstNumber', v);
  if not VarIsEmpty(v) then edtGroupCountEachFirstNumber.Text := v;
end;

procedure TfrmGroupCodeNameSettings.btnOkClick(Sender: TObject);
var
  i, v, GroupNumber, GroupNumber2, GroupFirstNumberCount, GroupCountEachFirstNumber: Integer;
begin
  if not CheckGroupNumber(GroupNumber, GroupNumber2) then
    raise Exception.Create('请输入有效遍历次数');
  if not edtGroupFirstNumberCount.TryToValue(GroupFirstNumberCount) then
    raise Exception.Create('请输入有效最前首行');
  if not edtGroupCountEachFirstNumber.TryToValue(GroupCountEachFirstNumber) then
    raise Exception.Create('请输入有效随后首行');

  fKeyValue.SetKeyValue('GroupNumber', GroupNumber);
  fKeyValue.SetKeyValue('GroupNumber2', GroupNumber2);
  fKeyValue.SetKeyValue('GroupFirstNumberCount', GroupFirstNumberCount);
  fKeyValue.SetKeyValue('GroupCountEachFirstNumber', GroupCountEachFirstNumber);

  ModalResult := mrOK;
end;

function TfrmGroupCodeNameSettings.CheckGroupNumber(var GroupNumber, GroupNumber2: Integer): Boolean;
begin
  Result := edtGroupNumber.TryToValue(GroupNumber);
  if not Result then Exit;
  Result := edtGroupNumber2.TryToValue(GroupNumber2);
  if not Result then Exit;
  Result := GroupNumber <= GroupNumber2;
  if not Result then Exit;
  if (GroupNumber2 > 0) and (GroupNumber = 0) then GroupNumber := 1;
end;

end.
