program Project6b;

uses
  Vcl.Forms,
  ufrmMain3 in 'ufrmMain3.pas' {frmMain},
  uCommon in '..\uCommon.pas',
  uDataComputer in 'uDataComputer.pas',
  uFileWriter in '..\uFileWriter.pas',
  ufrmGroupCodeNameSettings in 'ufrmGroupCodeNameSettings.pas' {frmGroupCodeNameSettings},
  uControlHelper in '..\uControlHelper.pas',
  ufrmGroupCodeNameSettings2 in 'ufrmGroupCodeNameSettings2.pas' {frmGroupCodeNameSettings2},
  ufrmExportFileSettings in 'ufrmExportFileSettings.pas' {frmExportFileSettings};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmGroupCodeNameSettings, frmGroupCodeNameSettings);
  Application.CreateForm(TfrmGroupCodeNameSettings2, frmGroupCodeNameSettings2);
  Application.CreateForm(TfrmExportFileSettings, frmExportFileSettings);
  Application.Run;
end.
