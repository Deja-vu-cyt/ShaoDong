unit ufrmGroupCodeNameSettings2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.Types, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmGroupCodeNameSettings2 = class(TForm)
    lblLine: TLabel;
    edtGroupNumber3: TEdit;
    edtGroupNumber4: TEdit;
    btnOk: TButton;
    lblLine2: TLabel;
    procedure FormShow(Sender: TObject);
    procedure edtGroupNumber3KeyPress(Sender: TObject; var Key: Char);
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fNumber: Byte;
    fEdits: TArray<TEdit>;
    procedure SetNumber(Value: Byte);
    function CheckGroupNumber(var GroupNumber, GroupNumber2: Integer): Boolean;
    procedure BuildGroupNumberSet;
  public
    property Number: Byte read fNumber write SetNumber;
  end;

const
  sControlName: string = 'GroupNumberSettings';

var
  frmGroupCodeNameSettings2: TfrmGroupCodeNameSettings2;

implementation

uses
  uDataComputer, uControlHelper;

{$R *.dfm}

procedure TfrmGroupCodeNameSettings2.SetNumber(Value: Byte);
var
  s: string;
begin
  if fNumber <> Value then
  begin
    s := lblLine.Caption;
    lblLine.Caption := s.Replace(Format('%d-2', [fNumber]), Format('%d-2', [Value]));

    fNumber := Value;
  end;
end;

procedure TfrmGroupCodeNameSettings2.edtGroupNumber3KeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then BuildGroupNumberSet;
end;

procedure TfrmGroupCodeNameSettings2.FormCreate(Sender: TObject);
begin
  fNumber := 6;
end;

procedure TfrmGroupCodeNameSettings2.FormShow(Sender: TObject);
begin
  if fSettings.GroupNumber3 > 0 then
    edtGroupNumber3.Text := fSettings.GroupNumber3.ToString;
  if fSettings.GroupNumber4 > 0 then
    edtGroupNumber4.Text := fSettings.GroupNumber4.ToString;

  BuildGroupNumberSet;
end;

procedure TfrmGroupCodeNameSettings2.btnOkClick(Sender: TObject);
var
  i, v, GroupNumber3, GroupNumber4: Integer;
  Ranges: TArray<TRange>;
  ValidityCountEachGroupNumber: TInt64DynArray;
begin
  if not CheckGroupNumber(GroupNumber3, GroupNumber4) then
    raise Exception.Create('请输入有效遍历次数');

  if GroupNumber3 > 0 then
  begin
    SetLength(Ranges, GroupNumber4 - GroupNumber3 + 1);
    for i := Low(Ranges) to High(Ranges) do
    begin
      Ranges[i].Value := -1;
      Ranges[i].Value2 := -1;
      if (i + 1) * 2 > Length(fEdits) then Continue;
      with fEdits[(i + 1) * 2 - 2] do
      begin
        if not TryToValue(v, -1) then
          raise Exception.Create('请输入有效范围');
      end;
      Ranges[i].Value := v;

      with fEdits[(i + 1) * 2 - 1] do
      begin
        if not TryToValue(v, -1) then
          raise Exception.Create('请输入有效范围');
      end;
      Ranges[i].Value2 := v;

      if Ranges[i].Value > Ranges[i].Value2 then
        raise Exception.Create('请输入有效范围');
    end;
  end;

  fSettings.GroupNumber3 := GroupNumber3;
  fSettings.GroupNumber4 := GroupNumber4;
  fSettings.BuildValidityCountEachGroupNumber;
  if fSettings.GroupNumber3 > 0 then
    for i := Low(fSettings.ValidityCountEachGroupNumber) to High(fSettings.ValidityCountEachGroupNumber) do
    begin
      fSettings.ValidityCountEachGroupNumber[i].Value := Ranges[i].Value;
      fSettings.ValidityCountEachGroupNumber[i].Value2 := Ranges[i].Value2;
    end;
  fKeyValue.SetKeyValue('GroupNumber3', fSettings.GroupNumber3);
  fKeyValue.SetKeyValue('GroupNumber4', fSettings.GroupNumber4);
  SetLength(ValidityCountEachGroupNumber, Length(fSettings.ValidityCountEachGroupNumber) * 2);
  for i := Low(fSettings.ValidityCountEachGroupNumber) to High(fSettings.ValidityCountEachGroupNumber) do
  begin
    ValidityCountEachGroupNumber[(i + 1) * 2 - 2] := fSettings.ValidityCountEachGroupNumber[i].Value;
    ValidityCountEachGroupNumber[(i + 1) * 2 - 1] := fSettings.ValidityCountEachGroupNumber[i].Value2;
  end;
  fKeyValue.SetKeyValue('ValidityCountEachGroupNumber', ValidityCountEachGroupNumber);

  ModalResult := mrOK;
end;

function TfrmGroupCodeNameSettings2.CheckGroupNumber(var GroupNumber, GroupNumber2: Integer): Boolean;
begin
  Result := edtGroupNumber3.TryToValue(GroupNumber);
  if not Result then Exit;
  Result := edtGroupNumber4.TryToValue(GroupNumber2);
  if not Result then Exit;
  Result := GroupNumber <= GroupNumber2;
  if not Result then Exit;
  if (GroupNumber2 > 0) and (GroupNumber = 0) then GroupNumber := 1;
end;

procedure TfrmGroupCodeNameSettings2.BuildGroupNumberSet;
var
  lbl: TLabel;
  i, GroupNumber, GroupNumber2, iLeft, iTop, EditIndex: Integer;
  ComponentName: string;
  Component: TComponent;
begin
  for i := ControlCount - 1 downto 0 do
    if ((Controls[i] is TLabel) or (Controls[i] is TEdit))
      and (Pos(sControlName, Controls[i].Name) > 0)
    then Controls[i].Visible := False;;

  if not CheckGroupNumber(GroupNumber, GroupNumber2) then Exit;
  if GroupNumber = 0 then Exit;
  SetLength(fEdits, (GroupNumber2 - GroupNumber + 1) * 2);

  iLeft := lblLine.Left;
  iTop := lblLine.Top;
  EditIndex := -1;
  for i := 0 to GroupNumber2 - GroupNumber do
  begin
    iTop := iTop + 40;

    ComponentName := Format('lbl%s%d', [sControlName, i + 1]);
    Component := FindComponent(ComponentName);
    if Assigned(Component) then
    begin
      lbl := Component as TLabel;
      lbl.Visible := True;
    end
    else
    begin
      lbl := TLabel.Create(Self);
      lbl.Name := ComponentName;
      lbl.Parent := Self;
      lbl.Left := iLeft;
      lbl.Top := iTop;
      lbl.Caption := Format('第 %d 次（ 遍历 ）；［ 第 1 - N 行为首行 ］只（ 遍历 ）：有          至          个相同（ N ）的［ 第 2 - N 行为首行 ］', [GroupNumber + i]);
    end;

    Inc(EditIndex);
    ComponentName := Format('edt%s%d', [sControlName, EditIndex + 1]);
    Component := FindComponent(ComponentName);
    if Assigned(Component) then
    begin
      fEdits[EditIndex] := Component as TEdit;
      fEdits[EditIndex].Visible := True;
    end
    else
    begin
      fEdits[EditIndex] := TEdit.Create(Self);
      with fEdits[EditIndex] do
      begin
        Name := ComponentName;
        Parent := Self;
        Left := 563;
        if GroupNumber + i > 9 then Left := Left + 10;
        Top := iTop;
        Width := 50;
        Text := '';
        if (i <= High(fSettings.ValidityCountEachGroupNumber))
          and (fSettings.ValidityCountEachGroupNumber[i].Value > -1)
        then
          Text := fSettings.ValidityCountEachGroupNumber[i].Value.ToString;
      end;
    end;

    Inc(EditIndex);
    ComponentName := Format('edt%s%d', [sControlName, EditIndex + 1]);
    Component := FindComponent(ComponentName);
    if Assigned(Component) then
    begin
      fEdits[EditIndex] := Component as TEdit;
      fEdits[EditIndex].Visible := True;
    end
    else
    begin
      fEdits[EditIndex] := TEdit.Create(Self);
      with fEdits[EditIndex] do
      begin
        Name := ComponentName;
        Parent := Self;
        Left := 644;
        if GroupNumber + i > 9 then Left := Left + 10;
        Top := iTop;
        Width := 50;
        Text := '';
        if (i <= High(fSettings.ValidityCountEachGroupNumber))
          and (fSettings.ValidityCountEachGroupNumber[i].Value2 > -1)
        then
          Text := fSettings.ValidityCountEachGroupNumber[i].Value2.ToString;
      end;
    end;
  end;
  lblLine2.Top := iTop + 40;
  btnOk.Top := iTop + 80;
end;

end.
