unit udmMain;

interface

uses
  uLog,
  System.SysUtils, System.Classes, System.Math, System.JSON, System.Threading,
  System.IOUtils, System.Variants, System.Generics.Collections, Winapi.Windows,
  XLSSheetData5, XLSReadWriteII5, Xc12DataStyleSheet5, Xc12Utils5, XLSCmdFormat5,
  Vcl.Dialogs, Vcl.Forms;

type
  TdmMain = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
  public
    procedure Init;
  end;

var
  dmMain: TdmMain;

implementation

uses
  uGlobal, ufrmMain, ufrmInput, udmRegister, udmProject1;

{%CLASSGROUP 'VCL.Controls.TControl'}

{$R *.dfm}

procedure TdmMain.DataModuleCreate(Sender: TObject);
begin
  FLog.Start;
end;

procedure TdmMain.DataModuleDestroy(Sender: TObject);
begin
  FLog.Stop;
end;

procedure TdmMain.Init;
begin
  TTask.Create(procedure
  begin
    try
      Flog.Log('���ڼ��ע��');
      dmRegister.CheckRegiester;

      dmProject1.Init;

      Flog.Log('��ʼ�����');
    except
      on e: Exception do
      begin
        Flog.Log(e.Message);
      end;
    end;
  end
  ).Start;
end;

end.

