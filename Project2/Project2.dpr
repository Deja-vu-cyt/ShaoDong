program Project2;

uses
  Vcl.Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  uGlobal in '..\uGlobal.pas',
  uInternetTime in '..\uInternetTime.pas',
  uTimer in '..\uTimer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
