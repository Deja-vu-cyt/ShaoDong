unit ufrmInput;

interface

uses
  System.SysUtils, System.Variants, System.Classes,
  Winapi.Windows, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmInput = class(TForm)
    btnOk: TButton;
    edtValue: TEdit;
    mmoPrompt: TMemo;
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure edtValueKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    FVarType: TVarType;
  public

  end;

var
  frmInput: TfrmInput;

function Input(const ACaption, APrompt: string; var ADefault: string; AType: TVarType = varString): Boolean; overload;
function Input(const ACaption, APrompt: string; var ADefault: Integer): Boolean; overload;
function Input(const ACaption, APrompt: string; var ADefault: Double): Boolean; overload;

implementation

{$R *.dfm}

function Input(const ACaption, APrompt: string; var ADefault: string; AType: TVarType = varString): Boolean;
begin
  Result := False;
  if not Assigned(frmInput) then frmInput := TfrmInput.Create(Application);
  frmInput.Caption := ACaption;
  frmInput.mmoPrompt.Text := APrompt;
  frmInput.edtValue.Text := ADefault;
  frmInput.edtValue.SelectAll;
  frmInput.FVarType := AType;
  frmInput.ShowModal;
  Result := frmInput.ModalResult = mrOk;
  if Result then ADefault := Trim(frmInput.edtValue.Text);
end;

function Input(const ACaption, APrompt: string; var ADefault: Integer): Boolean;
var
  s: string;
begin
  s := IntToStr(ADefault);
  Result := Input(ACaption, APrompt, s, varInteger);
  if Result then ADefault := StrToInt(s);
end;

function Input(const ACaption, APrompt: string; var ADefault: Double): Boolean;
var
  s: string;
begin
  s := FloatToStr(ADefault);
  Result := Input(ACaption, APrompt, s, varDouble);
  if Result then ADefault := StrToFloat(s);
end;

procedure TfrmInput.btnOkClick(Sender: TObject);
var
  i: Integer;
  d: Double;
  s: string;
begin
  s := Trim(edtValue.Text);
  case FVarType of
    varInteger:
    begin
      if not TryStrToInt(s, i) then
        raise Exception.Create('请输入有效整数');
    end;
    varDouble:
    begin
      if not TryStrToFloat(s, d) then
        raise Exception.Create('请输入有效数值');
    end;
  end;
  ModalResult := mrOk;
end;

procedure TfrmInput.edtValueKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 13 then btnOkClick(btnOk);
end;

procedure TfrmInput.FormCreate(Sender: TObject);
begin
  ActiveControl := edtValue;
end;

procedure TfrmInput.FormShow(Sender: TObject);
begin
  if Caption = '' then Caption := '提示';
end;

end.
