program Project2bs;

uses
  Vcl.Forms,
  uCommon in '..\uCommon.pas',
  uDataComputer in 'uDataComputer.pas',
  uFileWriter in '..\uFileWriter.pas',
  ufrmMain4 in 'ufrmMain4.pas' {frmMain},
  ufrmExportSettings2 in 'ufrmExportSettings2.pas' {frmExportSettings},
  ufrmExportSlantFileSettings2 in 'ufrmExportSlantFileSettings2.pas' {frmExportSlantFileSettings},
  ufrmExportVertFileSettings2 in 'ufrmExportVertFileSettings2.pas' {frmExportVertFileSettings},
  ufrmExportVertSlantFileSettings2 in 'ufrmExportVertSlantFileSettings2.pas' {frmExportVertSlantFileSettings},
  UfrmCodeNameSortSettings in 'UfrmCodeNameSortSettings.pas' {frmCodeNameSortSettings};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmExportSettings, frmExportSettings);
  Application.CreateForm(TfrmExportSlantFileSettings, frmExportSlantFileSettings);
  Application.CreateForm(TfrmExportVertFileSettings, frmExportVertFileSettings);
  Application.CreateForm(TfrmExportVertSlantFileSettings, frmExportVertSlantFileSettings);
  Application.Run;
end.
