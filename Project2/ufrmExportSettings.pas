unit ufrmExportSettings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmExportSettings = class(TForm)
    Label2: TLabel;
    edtKeepMaxRowSpacing: TEdit;
    Label1: TLabel;
    edtMinGroupCount: TEdit;
    btnOK: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    edtGroupRowCount: TEdit;
    Label7: TLabel;
    Label10: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    edtReEnabledGroupCount: TEdit;
    Label14: TLabel;
    chkRecalcMode2: TCheckBox;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    CheckBox1: TCheckBox;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fFlag: Byte;
    fKeepMaxRowSpacing: Cardinal;
    fGroupRowCount: Cardinal;
    fGroupCount: Cardinal;
    fReEnabledGroupCount: Cardinal;
    procedure SetFlag(aValue: Byte);
    function GetRecalcMode: Byte;
  public
    property Flag: Byte read fFlag write SetFlag;
    property RecalcMode: Byte read GetRecalcMode;
    property KeepMaxRowSpacing: Cardinal read fKeepMaxRowSpacing;
    property GroupRowCount: Cardinal read fGroupRowCount;
    property GroupCount: Cardinal read fGroupCount;
    property ReEnabledGroupCount: Cardinal read fReEnabledGroupCount;
  end;

var
  frmExportSettings: TfrmExportSettings;

implementation

uses
  uDataComputer;

{$R *.dfm}

procedure TfrmExportSettings.SetFlag(aValue: Byte);
var
  s, s2: string;
  i: Integer;
begin
  if fFlag <> aValue then
  begin
    case fFlag of
      0: s := 'ֱ��б��';
      1: s := 'ֱ��';
      2: s := 'б��';
    end;
    case aValue of
      0: s2 := 'ֱ��б��';
      1: s2 := 'ֱ��';
      2: s2 := 'б��';
    end;
    for i := 0 to ComponentCount - 1 do
    begin
      if Components[i] is TLabel then
      begin
        with Components[i] as TLabel do
        begin
          Caption := StringReplace(Caption, s, s2, [rfReplaceAll]);
        end;
      end;
    end;

    fFlag := aValue;
  end;
end;

function TfrmExportSettings.GetRecalcMode: Byte;
begin
  Result := 0;
  if chkRecalcMode2.Checked then Result := 2
  else
  if (fGroupCount > 0)
    or (fGroupRowCount > 0)
  then Result := 1;
end;

procedure TfrmExportSettings.btnOKClick(Sender: TObject);
var
  sKeepMaxRowSpacing, sGroupCount, sGroupRowCount, sReEnabledGroupCount: string;
  v: Integer;
begin
  if not chkRecalcMode2.Checked then
  begin
    sKeepMaxRowSpacing := Trim(edtKeepMaxRowSpacing.Text);
    if not sKeepMaxRowSpacing.IsEmpty and not (TryStrToInt(sKeepMaxRowSpacing, v) and (v >= 0)) then
      raise Exception.Create('��������Ч������о�');

    sGroupCount := Trim(edtMinGroupCount.Text);
    if not sGroupCount.IsEmpty and not (TryStrToInt(sGroupCount, v) and (v >= 0)) then
      raise Exception.Create('��������Ч���к�');

    sGroupRowCount := Trim(edtGroupRowCount.Text);
    if not sGroupRowCount.IsEmpty and not (TryStrToInt(sGroupRowCount, v) and (v >= 0)) then
      raise Exception.Create('��������Ч��С������');

    sReEnabledGroupCount := Trim(edtReEnabledGroupCount.Text);
    if not sReEnabledGroupCount.IsEmpty and not (TryStrToInt(sReEnabledGroupCount, v) and (v >= 0)) then
      raise Exception.Create('��������Ч��ǰ���к�');
  end;
  if sKeepMaxRowSpacing.IsEmpty then fKeepMaxRowSpacing := 0
  else fKeepMaxRowSpacing := sKeepMaxRowSpacing.ToInteger;
  if sGroupCount.IsEmpty then fGroupCount := 0
  else fGroupCount := sGroupCount.ToInteger;
  if sGroupRowCount.IsEmpty then fGroupRowCount := 0
  else fGroupRowCount := sGroupRowCount.ToInteger;
  if sReEnabledGroupCount.IsEmpty then fReEnabledGroupCount := 1
  else fReEnabledGroupCount := sReEnabledGroupCount.ToInteger;
  fDataComputer.SetKeyValue('KeepMaxRowSpacing', fKeepMaxRowSpacing);
  fDataComputer.SetKeyValue('GroupCount', fGroupCount);
  fDataComputer.SetKeyValue('GroupRowCount', fGroupRowCount);
  fDataComputer.SetKeyValue('ReEnabledGroupCount', fReEnabledGroupCount);
  fDataComputer.SetKeyValue('RecalcMode2', Ord(chkRecalcMode2.Checked));

  ModalResult := mrOK;
end;

procedure TfrmExportSettings.FormCreate(Sender: TObject);
var
  v: Variant;
begin
  fKeepMaxRowSpacing := 0;
  v := fDataComputer.GetKeyValue('KeepMaxRowSpacing');
  if not VarIsEmpty(v) then fKeepMaxRowSpacing := v;
  fGroupCount := 0;
  v := fDataComputer.GetKeyValue('GroupCount');
  if not VarIsEmpty(v) then fGroupCount := v;
  fGroupRowCount := 0;
  v := fDataComputer.GetKeyValue('GroupRowCount');
  if not VarIsEmpty(v) then fGroupRowCount := v;
  fReEnabledGroupCount := 0;
  v := fDataComputer.GetKeyValue('ReEnabledGroupCount');
  if not VarIsEmpty(v) then fReEnabledGroupCount := v;

  if fKeepMaxRowSpacing > 0 then
    edtKeepMaxRowSpacing.Text := fKeepMaxRowSpacing.ToString;
  if fGroupCount > 0 then
    edtMinGroupCount.Text := fGroupCount.ToString;
  if fGroupRowCount > 0 then
    edtGroupRowCount.Text := fGroupRowCount.ToString;
  if fReEnabledGroupCount > 0 then
    edtReEnabledGroupCount.Text := fReEnabledGroupCount.ToString;

  chkRecalcMode2.Checked := False;
  v := fDataComputer.GetKeyValue('RecalcMode2');
  if not VarIsEmpty(v) then chkRecalcMode2.Checked := Boolean(v);

  fFlag := 0;
end;

end.
