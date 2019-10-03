program Project7;

uses
  Vcl.Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  uCommon in '..\uCommon.pas',
  uDataComputer in 'uDataComputer.pas',
  uFileWriter in '..\uFileWriter.pas',
  uControlHelper in '..\uControlHelper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
