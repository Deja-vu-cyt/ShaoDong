program Project2;

uses
  Vcl.Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  uInternetTime in '..\uInternetTime.pas',
  uTimer in '..\uTimer.pas',
  uCommon in '..\uCommon.pas',
  uDataComputer in 'uDataComputer.pas',
  uFileWriter in '..\uFileWriter.pas',
  ufrmExportVertFileSettings in 'ufrmExportVertFileSettings.pas' {frmExportVertFileSettings},
  ufrmExportSettings in 'ufrmExportSettings.pas' {frmExportSettings},
  ufrmExportSlantFileSettings in 'ufrmExportSlantFileSettings.pas' {frmExportSlantFileSettings},
  ufrmExportVertSlantFileSettings in 'ufrmExportVertSlantFileSettings.pas' {frmExportVertSlantFileSettings};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmExportSettings, frmExportSettings);
  Application.CreateForm(TfrmExportVertSlantFileSettings, frmExportVertSlantFileSettings);
  Application.CreateForm(TfrmExportVertFileSettings, frmExportVertFileSettings);
  Application.CreateForm(TfrmExportSlantFileSettings, frmExportSlantFileSettings);
  Application.Run;
end.
