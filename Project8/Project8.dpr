program Project8;

uses
  Vcl.Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  uCommon in '..\uCommon.pas',
  uDataComputer in 'uDataComputer.pas',
  uFileWriter in '..\uFileWriter.pas',
  uControlHelper in '..\uControlHelper.pas',
  ufrmExportSettings11 in 'ufrmExportSettings11.pas' {frmExportSettings11},
  ufrmExportSettings4 in 'ufrmExportSettings4.pas' {frmExportSettings4},
  ufrmExportSettings5 in 'ufrmExportSettings5.pas' {frmExportSettings5},
  ufrmExportSettings6 in 'ufrmExportSettings6.pas' {frmExportSettings6},
  ufrmExportSettings2 in 'ufrmExportSettings2.pas' {frmExportSettings2},
  ufrmExportSettings3 in 'ufrmExportSettings3.pas' {frmExportSettings3},
  ufrmExportSettings in 'ufrmExportSettings.pas' {frmExportSettings},
  ufrmExportSettings7 in 'ufrmExportSettings7.pas' {frmExportSettings7},
  ufrmExportSettings8 in 'ufrmExportSettings8.pas' {frmExportSettings8},
  ufrmExportSettings9 in 'ufrmExportSettings9.pas' {frmExportSettings9},
  ufrmExportSettings10 in 'ufrmExportSettings10.pas' {frmExportSettings10};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmExportSettings11, frmExportSettings11);
  Application.CreateForm(TfrmExportSettings4, frmExportSettings4);
  Application.CreateForm(TfrmExportSettings5, frmExportSettings5);
  Application.CreateForm(TfrmExportSettings6, frmExportSettings6);
  Application.CreateForm(TfrmExportSettings2, frmExportSettings2);
  Application.CreateForm(TfrmExportSettings3, frmExportSettings3);
  Application.CreateForm(TfrmExportSettings, frmExportSettings);
  Application.CreateForm(TfrmExportSettings7, frmExportSettings7);
  Application.CreateForm(TfrmExportSettings8, frmExportSettings8);
  Application.CreateForm(TfrmExportSettings9, frmExportSettings9);
  Application.CreateForm(TfrmExportSettings10, frmExportSettings10);
  Application.Run;
end.
