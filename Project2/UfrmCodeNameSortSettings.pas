unit ufrmCodeNameSortSettings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.Variants, System.Classes,
  System.Generics.Collections,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmCodeNameSortSettings = class(TForm)
    chkOrderByCodeNameCount: TCheckBox;
    chkOrderByCodeNameCount2: TCheckBox;
    chkOrderByCodeNameCount3: TCheckBox;
    chkStochastic: TCheckBox;
    chkOrderByCodeNameCount4: TCheckBox;
    chkOrderByCodeName: TCheckBox;
    chkOrderByCodeName2: TCheckBox;
    chkOrderByCodeName3: TCheckBox;
    btnOK: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fList: TList<TCheckBox>;
  public

  end;

var
  frmCodeNameSortSettings: TfrmCodeNameSortSettings;

procedure CodeNameSortSet;

implementation

uses
  uCommon, uDataComputer;

{$R *.dfm}

procedure CodeNameSortSet;
begin
  if not Assigned(frmCodeNameSortSettings) then
    frmCodeNameSortSettings := TfrmCodeNameSortSettings.Create(Application);
  frmCodeNameSortSettings.ShowModal;
end;

procedure TfrmCodeNameSortSettings.btnOKClick(Sender: TObject);
var
  i: Integer;
begin
  fSettings.CodeNameSort := 0;
  for i := 0 to fList.Count - 1 do
    if fList[i].Checked then fSettings.CodeNameSort.AddValue(i + 1);
  ModalResult := mrOk;
end;

procedure TfrmCodeNameSortSettings.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  fList := TList<TCheckBox>.Create;
  fList.Add(chkOrderByCodeNameCount);
  fList.Add(chkOrderByCodeNameCount2);
  fList.Add(chkOrderByCodeNameCount3);
  fList.Add(chkStochastic);
  fList.Add(chkOrderByCodeNameCount4);
  fList.Add(chkOrderByCodeName);
  fList.Add(chkOrderByCodeName2);
  fList.Add(chkOrderByCodeName3);

  for i := 0 to fList.Count - 1 do
    fList[i].Checked := fSettings.CodeNameSort.ValueExist(i + 1);
end;

procedure TfrmCodeNameSortSettings.FormDestroy(Sender: TObject);
begin
  fList.Free;
end;

procedure TfrmCodeNameSortSettings.FormShow(Sender: TObject);
begin
  btnOk.Enabled := fSettings.CompareMode = cmNone;
end;

end.
