program Project2;

uses
  Vcl.Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  uInternetTime in '..\uInternetTime.pas',
  uCommon in '..\uCommon.pas',
  uDataComputer in 'uDataComputer.pas',
  uFileWriter in '..\uFileWriter.pas',
  ufrmExportVertFileSettings in 'ufrmExportVertFileSettings.pas' {frmExportVertFileSettings},
  ufrmExportSettings in 'ufrmExportSettings.pas' {frmExportSettings},
  ufrmExportSlantFileSettings in 'ufrmExportSlantFileSettings.pas' {frmExportSlantFileSettings},
  ufrmExportVertSlantFileSettings in 'ufrmExportVertSlantFileSettings.pas' {frmExportVertSlantFileSettings},
  ServiceIntf in 'ServiceIntf.pas',
  ufrmConnectionSettings in 'ufrmConnectionSettings.pas' {frmConnectionSettings},
  ufrmConsumer in 'ufrmConsumer.pas' {frmConsumer},
  ufrmCombinationCalculator in 'ufrmCombinationCalculator.pas' {frmCombinationCalculator},
  Dejavu.WaitableTimer in '..\Dejavu.WaitableTimer.pas',
  UfrmCodeNameSortSettings in 'UfrmCodeNameSortSettings.pas' {frmCodeNameSortSettings};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmExportSettings, frmExportSettings);
  Application.CreateForm(TfrmExportVertSlantFileSettings, frmExportVertSlantFileSettings);
  Application.CreateForm(TfrmExportVertFileSettings, frmExportVertFileSettings);
  Application.CreateForm(TfrmExportSlantFileSettings, frmExportSlantFileSettings);
  Application.CreateForm(TfrmConnectionSettings, frmConnectionSettings);
  Application.CreateForm(TfrmConsumer, frmConsumer);
  Application.Run;
end.
