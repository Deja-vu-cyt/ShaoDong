unit ufrmExportVertFileSettings2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmExportVertFileSettings = class(TForm)
    chkFile: TCheckBox;
    chkFile2: TCheckBox;
    chkFile3: TCheckBox;
    chkFile4: TCheckBox;
    chkFile5: TCheckBox;
    chkFile6: TCheckBox;
    chkSelectAll: TCheckBox;
    chkSelectAll2: TCheckBox;
    chkFile7: TCheckBox;
    chkFile10: TCheckBox;
    chkFile8: TCheckBox;
    chkFile11: TCheckBox;
    chkFile12: TCheckBox;
    chkFile14: TCheckBox;
    chkFile15: TCheckBox;
    chkFile9: TCheckBox;
    procedure chkSelectAllClick(Sender: TObject);
    procedure chkSelectAll2Click(Sender: TObject);
  private

  public

  end;

var
  frmExportVertFileSettings: TfrmExportVertFileSettings;

implementation

{$R *.dfm}

procedure TfrmExportVertFileSettings.chkSelectAll2Click(Sender: TObject);
begin
  with TCheckBox(Sender) do
  begin
    chkFile7.Checked := Checked;
    chkFile8.Checked := Checked;
    chkFile9.Checked := Checked;
    chkFile10.Checked := Checked;
    chkFile11.Checked := Checked;
    chkFile12.Checked := Checked;
    chkFile14.Checked := Checked;
    chkFile15.Checked := Checked;
  end;
end;

procedure TfrmExportVertFileSettings.chkSelectAllClick(Sender: TObject);
begin
  with TCheckBox(Sender) do
  begin
    chkFile.Checked := Checked;
    chkFile2.Checked := Checked;
    chkFile3.Checked := Checked;
    chkFile4.Checked := Checked;
    chkFile5.Checked := Checked;
    chkFile6.Checked := Checked;
  end;
end;

end.
