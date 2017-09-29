unit ufrmLog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmLog = class(TForm)
    mmoLog: TMemo;
  private
    { Private declarations }
  public

  end;

var
  frmLog: TfrmLog;

procedure SwitchToThisWindow(hWnd:Thandle;fAltTab:boolean);stdcall;external 'User32.dll';

implementation

{$R *.dfm}

end.
