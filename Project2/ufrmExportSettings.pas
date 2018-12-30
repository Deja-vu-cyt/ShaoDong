unit ufrmExportSettings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TfrmExportSettings = class(TForm)
    btnOK: TButton;
    Label2: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label10: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    edtKeepMaxRowSpacing: TEdit;
    edtMinGroupCount: TEdit;
    edtGroupRowCount: TEdit;
    edtReEnabledGroupCount: TEdit;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    chkHideSameGroup: TCheckBox;
    Label8: TLabel;
    Label9: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fFlag: Byte;
    fKeepMaxRowSpacing: Cardinal;
    fGroupRowCount: Cardinal;
    fGroupCount: Cardinal;
    fReEnabledGroupCount: Cardinal;
    fHideSameGroup: Boolean;
    procedure SetFlag(aValue: Byte);
    function GetRecalcMode: Byte;
  public
    property Flag: Byte read fFlag write SetFlag;
    property RecalcMode: Byte read GetRecalcMode;
    property KeepMaxRowSpacing: Cardinal read fKeepMaxRowSpacing;
    property GroupRowCount: Cardinal read fGroupRowCount;
    property GroupCount: Cardinal read fGroupCount;
    property ReEnabledGroupCount: Cardinal read fReEnabledGroupCount;
    property HideSameGroup: Boolean read fHideSameGroup;
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
      0: s := '直、斜连';
      1: s := '直连';
      2: s := '斜连';
    end;
    case aValue of
      0: s2 := '直、斜连';
      1: s2 := '直连';
      2: s2 := '斜连';
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
  if (fGroupCount > 0)
    or (fGroupRowCount > 0)
    or fHideSameGroup
  then Result := 1;
end;

procedure TfrmExportSettings.btnOKClick(Sender: TObject);
var
  sKeepMaxRowSpacing, sGroupCount, sGroupRowCount, sReEnabledGroupCount: string;
  v: Integer;
begin
  sKeepMaxRowSpacing := Trim(edtKeepMaxRowSpacing.Text);
  if not sKeepMaxRowSpacing.IsEmpty and not (TryStrToInt(sKeepMaxRowSpacing, v) and (v >= 0)) then
    raise Exception.Create('请输入有效最大临行距');

  sGroupCount := Trim(edtMinGroupCount.Text);
  if not sGroupCount.IsEmpty and not (TryStrToInt(sGroupCount, v) and (v >= 0)) then
    raise Exception.Create('请输入有效序行号');

  sGroupRowCount := Trim(edtGroupRowCount.Text);
  if not sGroupRowCount.IsEmpty and not (TryStrToInt(sGroupRowCount, v) and (v >= 0)) then
    raise Exception.Create('请输入有效最小首行数');

  sReEnabledGroupCount := Trim(edtReEnabledGroupCount.Text);
  if not sReEnabledGroupCount.IsEmpty and not (TryStrToInt(sReEnabledGroupCount, v) and (v >= 0)) then
    raise Exception.Create('请输入有效最前序行号');

  if sKeepMaxRowSpacing.IsEmpty then fKeepMaxRowSpacing := 0
  else fKeepMaxRowSpacing := sKeepMaxRowSpacing.ToInteger;
  if sGroupCount.IsEmpty then fGroupCount := 0
  else fGroupCount := sGroupCount.ToInteger;
  if sGroupRowCount.IsEmpty then fGroupRowCount := 0
  else fGroupRowCount := sGroupRowCount.ToInteger;
  if sReEnabledGroupCount.IsEmpty then fReEnabledGroupCount := 1
  else fReEnabledGroupCount := sReEnabledGroupCount.ToInteger;
  fHideSameGroup := chkHideSameGroup.Checked;
  {fDataComputer.SetKeyValue('KeepMaxRowSpacing', fKeepMaxRowSpacing);
  fDataComputer.SetKeyValue('GroupCount', fGroupCount);
  fDataComputer.SetKeyValue('GroupRowCount', fGroupRowCount);
  fDataComputer.SetKeyValue('ReEnabledGroupCount', fReEnabledGroupCount);
  fDataComputer.SetKeyValue('HideSameGroup', Ord(fHideSameGroup));}

  ModalResult := mrOK;
end;

procedure TfrmExportSettings.FormCreate(Sender: TObject);
var
  v: Variant;
begin
  {fKeepMaxRowSpacing := 0;
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
  if not VarIsEmpty(v) then fReEnabledGroupCount := v; }

  if fKeepMaxRowSpacing > 0 then
    edtKeepMaxRowSpacing.Text := fKeepMaxRowSpacing.ToString;
  if fGroupCount > 0 then
    edtMinGroupCount.Text := fGroupCount.ToString;
  if fGroupRowCount > 0 then
    edtGroupRowCount.Text := fGroupRowCount.ToString;
  if fReEnabledGroupCount > 0 then
    edtReEnabledGroupCount.Text := fReEnabledGroupCount.ToString;

  fHideSameGroup := False;
  //v := fDataComputer.GetKeyValue('HideSameGroup');
  if not VarIsEmpty(v) then fHideSameGroup := Boolean(v);
  chkHideSameGroup.Checked := fHideSameGroup;

  fFlag := 0;
end;

end.
