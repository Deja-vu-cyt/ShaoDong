program Project5b;

uses
  Vcl.Forms,
  ufrmMain2 in 'ufrmMain2.pas' {frmMain},
  uDataComputer in 'uDataComputer.pas',
  uCommon in '..\uCommon.pas',
  ufrmInput in 'ufrmInput.pas' {frmInput},
  uTimer in '..\uTimer.pas',
  ufrmSameColumnsSet in 'ufrmSameColumnsSet.pas' {frmSameColumnsSet},
  ufrmRearrangeFileSettings in 'ufrmRearrangeFileSettings.pas' {frmRearrangeFileSettings},
  ufrmIntervalValueSet in '..\ufrmIntervalValueSet.pas' {frmIntervalValueSet},
  ufrmIntervalValueSet2 in 'ufrmIntervalValueSet2.pas' {frmIntervalValueSet2},
  ufrmIntervalValueSet3 in 'ufrmIntervalValueSet3.pas' {frmIntervalValueSet3},
  ufrmExportFileSettings in 'ufrmExportFileSettings.pas' {frmExportFileSettings};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmIntervalValueSet3, frmIntervalValueSet3);
  Application.Run;
end.
