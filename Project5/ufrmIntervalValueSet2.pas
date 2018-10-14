unit ufrmIntervalValueSet2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ufrmIntervalValueSet, Vcl.StdCtrls;

type
  TfrmIntervalValueSet2 = class(TfrmIntervalValueSet)
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmIntervalValueSet2: TfrmIntervalValueSet2;

procedure IntervalValueSet;

implementation

{$R *.dfm}

procedure IntervalValueSet;
begin
  if not Assigned(frmIntervalValueSet2) then
    frmIntervalValueSet2 := TfrmIntervalValueSet2.Create(Application);
  frmIntervalValueSet2.ShowModal;
end;

end.
