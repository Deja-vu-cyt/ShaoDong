unit ufrmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.Threading, System.IOUtils, System.Types, System.Math,
  Vcl.Graphics, Vcl.FileCtrl,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DBGridEhGrouping, ToolCtrlsEh,
  DBGridEhToolCtrls, DynVarsEh, GridsEh, DBAxisGridsEh, DBGridEh, Data.DB,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus, Vcl.Mask, DBCtrlsEh,
  Vcl.ComCtrls, XLSSheetData5, XLSReadWriteII5, Xc12Utils5, Xc12DataStyleSheet5, XLSRange5,
  Vcl.DBGrids, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.Phys.ODBCBase,
  FireDAC.Phys.MSSQL, FireDAC.VCLUI.Wait, FireDAC.Comp.UI, Vcl.CheckLst,
  FireDAC.Phys.MSSQLDef, EhLibVCL, Vcl.MPlayer;

type
  TfrmMain = class(TForm)
    MainMenu: TMainMenu;
    N1: TMenuItem;
    miRegister: TMenuItem;
    StatusBar: TStatusBar;
    miLog: TMenuItem;
    MediaPlayer: TMediaPlayer;
    procedure miRegisterClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure miLogClick(Sender: TObject);
    procedure MediaPlayerNotify(Sender: TObject);
  private
  public
    procedure BringToFront;
    procedure PlayMusic;
  end;

var
  frmMain: TfrmMain;

procedure SwitchToThisWindow(hWnd:Thandle;fAltTab:boolean);stdcall;external 'User32.dll';

implementation

uses
  udmMain, udmRegister, uLog;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Width := 1024;
  Height := 768;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  dmMain.Init;
end;

procedure TfrmMain.MediaPlayerNotify(Sender: TObject);
begin
  if MediaPlayer.NotifyValue = nvSuccessful then PlayMusic;
end;

procedure TfrmMain.BringToFront;
begin
  SwitchToThisWindow(Handle, True);
end;

procedure TfrmMain.PlayMusic;
var
  s: string;
begin
  s := 'Happy Birthday To You.mp3';
  if FileExists(s) then
  begin
    MediaPlayer.FileName := s;
    MediaPlayer.Open;
    MediaPlayer.Play;
  end;
end;

procedure TfrmMain.miLogClick(Sender: TObject);
begin
  FLog.ShowLogWindow;
end;

procedure TfrmMain.miRegisterClick(Sender: TObject);
begin
  dmRegister.RegisterSoftware;
end;

end.
