program Project6bs;

uses
  Vcl.Forms,
  ufrmMain4 in 'ufrmMain4.pas' {frmMain},
  uInternetTime in '..\uInternetTime.pas',
  uCommon in '..\uCommon.pas',
  uDataComputer in 'uDataComputer.pas',
  uFileWriter in '..\uFileWriter.pas',
  Dejavu.WaitableTimer in '..\Dejavu.WaitableTimer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
