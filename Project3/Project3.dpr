program Project3;

uses
  Vcl.Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  uGlobal in '..\uGlobal.pas',
  uTimer in '..\uTimer.pas',
  uInternetTime in '..\uInternetTime.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
