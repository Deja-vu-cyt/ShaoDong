program Project5;

uses
  Vcl.Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  uDataComputer in 'uDataComputer.pas',
  uCommon in '..\uCommon.pas',
  ufrmInput in 'ufrmInput.pas' {frmInput},
  uTimer in '..\uTimer.pas',
  ufrmSameColumnsSet in 'ufrmSameColumnsSet.pas' {frmSameColumnsSet},
  ufrmDeleteSameColumnSet in 'ufrmDeleteSameColumnSet.pas' {frmDeleteSameColumnSet},
  ufrmIntervalValueSet in '..\ufrmIntervalValueSet.pas' {frmIntervalValueSet};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmIntervalValueSet, frmIntervalValueSet);
  Application.Run;
end.
