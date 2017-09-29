program Project;

uses
  Vcl.Forms,
  UfrmMain in 'UfrmMain.pas' {frmMain},
  UfrmRegister in 'UfrmRegister.pas' {frmRegister},
  udmProject2 in 'Project2\udmProject2.pas' {dmProject2: TDataModule},
  uLog in 'uLog.pas',
  AES in 'AES.pas',
  ElAES in 'ElAES.pas',
  ufrmInput in 'ufrmInput.pas' {frmInput},
  udmConfig in 'udmConfig.pas' {dmConfig: TDataModule},
  ufrmProject in 'Project1\ufrmProject.pas' {frmProject},
  ufrmQueryConfig2 in 'Project1\ufrmQueryConfig2.pas' {frmQueryConfig2},
  udmRegister in 'udmRegister.pas' {dmRegister: TDataModule},
  ufrmQueryConfig3 in 'Project1\ufrmQueryConfig3.pas' {frmQueryConfig3},
  ufrmProject2 in 'Project2\ufrmProject2.pas' {frmProject2: TFrame},
  udmMain in 'udmMain.pas' {dmMain: TDataModule},
  uGlobal in 'uGlobal.pas',
  ufrmConfirm in 'ufrmConfirm.pas' {frmConfirm},
  ufrmSingleChoice in 'ufrmSingleChoice.pas' {frmSingleChoice},
  udmProject1 in 'Project1\udmProject1.pas' {dmProject1: TDataModule},
  ufrmProject1 in 'Project1\ufrmProject1.pas' {frmProject1: TFrame},
  ufrmLog in 'ufrmLog.pas' {frmLog},
  uCacheFile in 'Project2\uCacheFile.pas',
  uConfig in 'Project2\uConfig.pas',
  uDataTable in 'Project2\uDataTable.pas',
  uDriveInfo in 'Project2\uDriveInfo.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.HintHidePause := 100000;
  Application.CreateForm(TdmConfig, dmConfig);
  Application.CreateForm(TdmRegister, dmRegister);
  Application.CreateForm(TdmMain, dmMain);
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TdmProject1, dmProject1);
  //Application.CreateForm(TdmProject2, dmProject2);
  Application.Run;
end.
