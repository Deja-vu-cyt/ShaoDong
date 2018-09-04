program Project5;

uses
  Vcl.Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  uDataComputer in 'uDataComputer.pas',
  uCommon in '..\uCommon.pas',
  ufrmInput in 'ufrmInput.pas' {frmInput},
  uTimer in '..\uTimer.pas',
  ufrmSameColumnsSet in 'ufrmSameColumnsSet.pas' {frmSameColumnsSet},
  ufrmExportFileSettings in 'ufrmExportFileSettings.pas' {frmExportFileSettings},
  ufrmIntervalValueSet in '..\ufrmIntervalValueSet.pas' {frmIntervalValueSet},
  ufrmQueryConfig3 in 'ufrmQueryConfig3.pas' {frmQueryConfig3};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmIntervalValueSet, frmIntervalValueSet);
  Application.CreateForm(TfrmQueryConfig3, frmQueryConfig3);
  Application.Run;
end.
