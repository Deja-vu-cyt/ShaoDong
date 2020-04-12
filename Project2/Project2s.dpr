program Project2s;

uses
  Vcl.Forms,
  uCommon in '..\uCommon.pas',
  uDataComputer in 'uDataComputer.pas',
  uFileWriter in '..\uFileWriter.pas',
  ufrmMain2 in 'ufrmMain2.pas' {frmMain},
  ufrmExportSettings2 in 'ufrmExportSettings2.pas' {frmExportSettings},
  ufrmExportSlantFileSettings2 in 'ufrmExportSlantFileSettings2.pas' {frmExportSlantFileSettings},
  ufrmExportVertFileSettings2 in 'ufrmExportVertFileSettings2.pas' {frmExportVertFileSettings},
  ufrmExportVertSlantFileSettings2 in 'ufrmExportVertSlantFileSettings2.pas' {frmExportVertSlantFileSettings},
  ufrmConsumer in 'ufrmConsumer.pas' {frmConsumer},
  ServiceIntf in 'ServiceIntf.pas',
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
  Application.CreateForm(TfrmConsumer, frmConsumer);
  Application.Run;
end.
