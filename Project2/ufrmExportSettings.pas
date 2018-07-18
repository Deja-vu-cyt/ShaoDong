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
    Label8: TLabel;
    Label9: TLabel;
    Label6: TLabel;
    edtGroupRowCount: TEdit;
    Label7: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    edtReEnabledGroupCount: TEdit;
    Label14: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fFlag: Byte;
    fKeepMaxRowSpacing: Cardinal;
    fGroupRowCount: Cardinal;
    fGroupCount: Cardinal;
    fReEnabledGroupCount: Cardinal;
    procedure SetFlag(aValue: Byte);
  public
    property Flag: Byte read fFlag write SetFlag;
    property KeepMaxRowSpacing: Cardinal read fKeepMaxRowSpacing;
    property GroupRowCount: Cardinal read fGroupRowCount;
    property GroupCount: Cardinal read fGroupCount;
    property ReEnabledGroupCount: Cardinal read fReEnabledGroupCount;
  end;

var
  frmExportSettings: TfrmExportSettings;

implementation

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

procedure TfrmExportSettings.btnOKClick(Sender: TObject);
var
  v: Integer;
begin
  fKeepMaxRowSpacing := 0;
  if not Trim(edtKeepMaxRowSpacing.Text).IsEmpty then
  begin
    if not TryStrToInt(Trim(edtKeepMaxRowSpacing.Text), v) then
      raise Exception.Create('��������Ч������о�');
    fKeepMaxRowSpacing := v;
  end;
  fGroupRowCount := 0;
  if not Trim(edtGroupRowCount.Text).IsEmpty then
  begin
    if not TryStrToInt(Trim(edtGroupRowCount.Text), v) then
      raise Exception.Create('��������Ч��С������');
    fGroupRowCount := v;
  end;
  fGroupCount := 0;
  if not Trim(edtMinGroupCount.Text).IsEmpty then
  begin
    if not TryStrToInt(Trim(edtMinGroupCount.Text), v) then
      raise Exception.Create('��������Ч���к�');
    fGroupCount := v;
  end;
  fReEnabledGroupCount := 0;
  if not Trim(edtReEnabledGroupCount.Text).IsEmpty then
  begin
    if not TryStrToInt(Trim(edtReEnabledGroupCount.Text), v) then
      raise Exception.Create('��������Ч��ǰ���к�');
    fReEnabledGroupCount := v;
  end;
  ModalResult := mrOK;
end;

procedure TfrmExportSettings.FormCreate(Sender: TObject);
begin
  fFlag := 0;
  fKeepMaxRowSpacing := 0;
  fGroupRowCount := 0;
  fGroupCount := 0;
end;

end.
