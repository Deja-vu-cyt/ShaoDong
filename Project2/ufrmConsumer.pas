unit ufrmConsumer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  DBGridEhGrouping, ToolCtrlsEh, DBGridEhToolCtrls, DynVarsEh, EhLibVCL,
  GridsEh, DBAxisGridsEh, DBGridEh, Vcl.ExtCtrls;

type
  TfrmConsumer = class(TForm)
    fdmtConsumer: TFDMemTable;
    dbgrdConsumer: TDBGridEh;
    dsConsumer: TDataSource;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private

  public

  end;

var
  frmConsumer: TfrmConsumer;

implementation

uses
  uDataComputer, uCommon;

{$R *.dfm}

procedure TfrmConsumer.FormCreate(Sender: TObject);
begin
  fdmtConsumer.CreateDataSet;
end;

procedure TfrmConsumer.FormDestroy(Sender: TObject);
begin
  Timer.Enabled := False;
end;

procedure TfrmConsumer.FormHide(Sender: TObject);
begin
  Timer.Enabled := False;
end;

procedure TfrmConsumer.FormShow(Sender: TObject);
begin
  Timer.Enabled := True;
end;

procedure TfrmConsumer.TimerTimer(Sender: TObject);
var
  i, RecNo: Integer;
begin
  if fNotifyCallback = nil then Exit;

  with TNotifyCallback(fNotifyCallback) do
  begin
    RecNo := fdmtConsumer.RecNo;
    fdmtConsumer.DisableControls;
    try
      for i := Low(Consumers) to High(Consumers) do
      begin
        if fdmtConsumer.Locate('ThreadID', Consumers[i].ThreadID, []) then fdmtConsumer.Edit
        else
        begin
          fdmtConsumer.Append;
          fdmtConsumer.FieldByName('ThreadID').AsLargeInt := Consumers[i].ThreadID;
        end;
        fdmtConsumer.FieldByName('FirstRow').AsInteger := Consumers[i].ActiveFirstRow;
        fdmtConsumer.FieldByName('CodeName').AsString := Consumers[i].ActiveCodeName.ToString;
        fdmtConsumer.Post;
      end;
    finally
      if (RecNo > 0) and (RecNo <= fdmtConsumer.RecordCount) then fdmtConsumer.RecNo := RecNo;
      fdmtConsumer.EnableControls;
    end;
  end;

end;

end.
