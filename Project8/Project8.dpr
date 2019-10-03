program Project8;

uses
  Vcl.Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  uCommon in '..\uCommon.pas',
  uDataComputer in 'uDataComputer.pas',
  uFileWriter in '..\uFileWriter.pas',
  uControlHelper in '..\uControlHelper.pas',
  ufrmExportSettings4 in 'ufrmExportSettings4.pas' {frmExportSettings4},
  ufrmExportSettings in 'ufrmExportSettings.pas' {frmExportSettings},
  ufrmExportSettings2 in 'ufrmExportSettings2.pas' {frmExportSettings2},
  ufrmExportSettings3 in 'ufrmExportSettings3.pas' {frmExportSettings3};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmExportSettings4, frmExportSettings4);
  Application.CreateForm(TfrmExportSettings, frmExportSettings);
  Application.CreateForm(TfrmExportSettings2, frmExportSettings2);
  Application.CreateForm(TfrmExportSettings3, frmExportSettings3);
  Application.Run;
end.
