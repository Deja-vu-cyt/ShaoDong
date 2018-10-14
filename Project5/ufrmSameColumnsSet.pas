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
    Label1: TLabel;
    edtEachPageRowCount: TEdit;
    pnlContainer: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edtKeyPress(Sender: TObject; var Key: Char);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    fDataMode: Byte;
    fMaxValue: Word;
    fIntervalValues: TWordDynArray;
    fCheckedList: TList;
    fEachRowValueCount: TByteDynArray;
    fValues: TWordDynArray;
    fValueChanged: Boolean;
    fEachPageRowCount: Word;
    procedure CheckBoxClick(Sender: TObject);
    procedure CheckBoxGroupClick(Sender: TObject);
    procedure BuildCheckBox(aEachRowValueCount: TByteDynArray; Offset: Byte = 0);
    procedure RenameCheckBox(aIntervalValues: TWordDynArray);
    procedure SetIntervalValues(aIntervalValues: TWordDynArray);
  public
    property DataMode: Byte read fDataMode write fDataMode;
    property IntervalValues: TWordDynArray read fIntervalValues write SetIntervalValues;
    property Values: TWordDynArray read fValues;
    property ValueChanged: Boolean read fValueChanged;
    property EachPageRowCount: Word read fEachPageRowCount;
  end;

var
  frmSameColumnsSet: TfrmSameColumnsSet;

const
  CheckBoxGroupName = 'chkGroup%d';
  CheckBoxName = 'chk%d';
  EachRowValueCount: TByteDynArray = [19, 20, 20, 20, 20, 20, 20, 20, 20,
    20, 20, 20, 17];
  EachRowValueCount2: TByteDynArray = [10, 10, 10, 10, 10, 10, 10, 10, 10,
    10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 6];

procedure SameColumnsSet(aIntervalValues: TWordDynArray; aDataMode: Byte = 0);

implementation

{$R *.dfm}

procedure SameColumnsSet(aIntervalValues: TWordDynArray; aDataMode: Byte);
begin
  if not Assigned(frmSameColumnsSet) then
    frmSameColumnsSet := TfrmSameColumnsSet.Create(Application);
  frmSameColumnsSet.DataMode := aDataMode;
  frmSameColumnsSet.IntervalValues := aIntervalValues;
  frmSameColumnsSet.ShowModal;
end;

procedure TfrmSameColumnsSet.SetIntervalValues(aIntervalValues: TWordDynArray);
var
  IsEqual: Boolean;
  i: Integer;
begin
  IsEqual := Length(aIntervalValues) = Length(fIntervalValues);
  if IsEqual then
  begin
    for i := Low(fIntervalValues) to High(fIntervalValues) do
    begin
      IsEqual := fIntervalValues[i] = aIntervalValues[i];
      if not IsEqual then Break;
    end;
  end;
  if not IsEqual then
  begin
    fIntervalValues := aIntervalValues;
    fMaxValue := 0;
    for i := Low(fIntervalValues) to High(fIntervalValues) do
      fMaxValue := fMaxValue + fIntervalValues[i];

    case fDataMode of
      1: BuildCheckBox(EachRowValueCount2);
      else BuildCheckBox(EachRowValueCount, 1);
    end;
    RenameCheckBox(aIntervalValues);
  end;
end;

procedure TfrmSameColumnsSet.FormCreate(Sender: TObject);
begin
  fDataMode := 0;
  fValues := [];
  fCheckedList := TList.Create;
end;

procedure TfrmSameColumnsSet.FormDestroy(Sender: TObject);
begin
  fCheckedList.Free;
end;

procedure TfrmSameColumnsSet.FormResize(Sender: TObject);
var
  iLeft, iTop: Integer;
begin
  iLeft := (ClientWidth - pnlContainer.Width) div 2;
  if iLeft < 0 then iLeft := 0;
  iTop := (ClientHeight - pnlContainer.Height - Panel1.Height) div 2;
  if iTop < 0 then iTop := 0;
  pnlContainer.Left := iLeft;
  pnlContainer.Top := iTop;
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
    for i := 0 to FCheckedList.Count - 1 do
    begin
      with TCheckBox(FCheckedList[i]) do
      begin
        if Checked then
        begin
          SetLength(fValues, Length(fValues) + 1);
          fValues[High(fValues)] := Tag;
        end;
      end;
    end;
    FValueChanged := False;
  end;

  if not (TryStrToInt(edtEachPageRowCount.Text, i) and (i > 0)) then
    raise Exception.Create('请输入有效每页行数');
  fEachPageRowCount := i;

  ModalResult := mrOk;
end;

procedure TfrmSameColumnsSet.CheckBoxClick(Sender: TObject);
begin
  with TCheckBox(Sender) do
  begin
    if Checked then fCheckedList.Add(Sender)
    else fCheckedList.Remove(Sender);
  end;

  FValueChanged := True;
end;

procedure TfrmSameColumnsSet.CheckBoxGroupClick(Sender: TObject);
var
  CheckBox: TControl;
  i, v: Integer;
begin
  with TCheckBox(Sender) do
  begin
    v := 0;
    for i := 0 to Tag - 1 do
      v := v + fEachRowValueCount[i];
    for i := v + 1 to v + fEachRowValueCount[Tag] do
    begin
      CheckBox := pnlContainer.FindChildControl(Format(CheckBoxName, [i]));
      if not Assigned(CheckBox) then Continue;
      TCheckBox(CheckBox).Checked := Checked;
    end;
  end;
end;

procedure TfrmSameColumnsSet.BuildCheckBox(aEachRowValueCount: TByteDynArray; Offset: Byte);
const
  CheckBoxWidth = 52;
  CheckBoxHeight = 24;
var
  i, j, v, iLeft, iTop, iMaxLeft: Integer;
begin
  for i := pnlContainer.ControlCount - 1 downto 0 do
    pnlContainer.Controls[i].Free;
  iMaxLeft := 0;
  v := 0;
  for i := Low(aEachRowValueCount) to High(aEachRowValueCount) do
  begin
    iLeft := 0;
    if i = 0 then iLeft := Offset * CheckBoxWidth;
    iTop := i * CheckBoxHeight;

    for j := 1 to aEachRowValueCount[i] do
    begin
      Inc(v);
      if v > fMaxValue then Break;
      with TCheckBox.Create(Self) do
      begin
        Name := Format(CheckBoxName, [v]);
        Parent := pnlContainer;
        Tag := v;
        OnClick := CheckBoxClick;
        Left := iLeft;
        Top := iTop;

        iLeft := iLeft + CheckBoxWidth;
        if iLeft > iMaxLeft then iMaxLeft := iLeft;
      end;
    end;

    with TCheckBox.Create(Self) do
    begin
      Name := Format(CheckBoxGroupName, [i + 1]);
      Parent := pnlContainer;
      Font.Style := [fsBold];
      Caption := Format('第%d行全选口√', [i + 1]);
      Tag := i;
      OnClick := CheckBoxGroupClick;
      Left := iMaxLeft + 10;
      Top := iTop;
      Width := 180;
    end;
    if v >= fMaxValue then Break;
  end;
  pnlContainer.Width := iMaxLeft + 180;
  pnlContainer.Height := iTop + 24;
  Panel1.BringToFront;
  FormResize(Self);
  fEachRowValueCount := aEachRowValueCount;
end;

procedure TfrmSameColumnsSet.RenameCheckBox(aIntervalValues: TWordDynArray);
var
  i, v, IntervalValue, IntervalIndex: Integer;
  CheckBox: TControl;
begin
  if Length(aIntervalValues) = 0 then Exit;
  IntervalIndex := 0;
  IntervalValue := 0;
  for i := 1 to 256 do
  begin
    CheckBox := pnlContainer.FindChildControl(Format(CheckBoxName, [i]));
    if not Assigned(CheckBox) then Continue;
    with TCheckBox(CheckBox) do
    begin
      v := i;
      if (IntervalIndex <= High(aIntervalValues))
        and (v > IntervalValue + aIntervalValues[IntervalIndex])
      then
      begin
        repeat
          if IntervalIndex <= High(aIntervalValues) then
          begin
            IntervalValue := IntervalValue + aIntervalValues[IntervalIndex];
            Inc(IntervalIndex);
          end;
        until (v <= IntervalValue + aIntervalValues[IntervalIndex])
          or (IntervalIndex > High(aIntervalValues));
      end;
      v := v - IntervalValue;
      case fDataMode of
        1: if IntervalIndex = 0 then v := (v - 1) mod 10;
      end;
      Caption := v.ToString;
    end;
  end;
end;

procedure TfrmSameColumnsSet.edtKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then btnOkClick(nil);
end;

end.
