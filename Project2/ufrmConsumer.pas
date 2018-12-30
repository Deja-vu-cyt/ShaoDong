unit ufrmConsumer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  DBGridEhGrouping, ToolCtrlsEh, DBGridEhToolCtrls, DynVarsEh, EhLibVCL,
  GridsEh, DBAxisGridsEh, DBGridEh;

type
  TfrmConsumer = class(TForm)
    fdmtConsumer: TFDMemTable;
    dbgrdConsumer: TDBGridEh;
    dsConsumer: TDataSource;
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure Add(Address: string);
    procedure Delete(Address: string);
    procedure Process(Address: string; FirstRow: Word);
    procedure ProcessFinish(Address: string; FirstRow: Word);
  end;

var
  frmConsumer: TfrmConsumer;

implementation

{$R *.dfm}

procedure TfrmConsumer.FormCreate(Sender: TObject);
begin
  fdmtConsumer.CreateDataSet;
end;

procedure TfrmConsumer.Add(Address: string);
begin
  TThread.Queue(nil, procedure
  var
    RecNo: Integer;
  begin
    RecNo := fdmtConsumer.RecNo;
    fdmtConsumer.DisableControls;
    try
      if not fdmtConsumer.Locate('Address', Address, []) then
      begin
        fdmtConsumer.Append;
        fdmtConsumer.FieldByName('Address').AsString := Address;
        fdmtConsumer.Post;
      end;
    finally
      if (RecNo > 0) and (RecNo <= fdmtConsumer.RecordCount) then fdmtConsumer.RecNo := RecNo;
      fdmtConsumer.EnableControls;
    end;
  end);
end;

procedure TfrmConsumer.Delete(Address: string);
begin
  TThread.Queue(nil, procedure
  var
    RecNo: Integer;
  begin
    RecNo := fdmtConsumer.RecNo;
    fdmtConsumer.DisableControls;
    try
      if fdmtConsumer.Locate('Address', Address, []) then fdmtConsumer.Delete;
    finally
      if (RecNo > 0) and (RecNo <= fdmtConsumer.RecordCount) then fdmtConsumer.RecNo := RecNo;
      fdmtConsumer.EnableControls;
    end;
  end);
end;

procedure TfrmConsumer.Process(Address: string; FirstRow: Word);
begin
  TThread.Queue(nil, procedure
  var
    RecNo: Integer;
    f: TField;
    s: string;
  begin
    RecNo := fdmtConsumer.RecNo;
    fdmtConsumer.DisableControls;
    try
      if fdmtConsumer.Locate('Address', Address, []) then
      begin
        f := fdmtConsumer.FieldByName('FirstRow');
        for s in f.AsString.Split(['、']) do
          if s.Equals(FirstRow.ToString) then Exit;
        fdmtConsumer.Edit;
        if not f.AsString.IsEmpty then f.AsString := f.AsString + '、';
        f.AsString := f.AsString + FirstRow.ToString;
        fdmtConsumer.Post;
      end;
    finally
      if (RecNo > 0) and (RecNo <= fdmtConsumer.RecordCount) then fdmtConsumer.RecNo := RecNo;
      fdmtConsumer.EnableControls;
    end;
  end);
end;

procedure TfrmConsumer.ProcessFinish(Address: string; FirstRow: Word);
begin
  TThread.Queue(nil, procedure
  var
    RecNo: Integer;
    f: TField;
    s, sFirstRow: string;
  begin
    RecNo := fdmtConsumer.RecNo;
    fdmtConsumer.DisableControls;
    try
      if fdmtConsumer.Locate('Address', Address, []) then
      begin
        f := fdmtConsumer.FieldByName('FirstRow');
        sFirstRow := '';
        for s in f.AsString.Split(['、']) do
          if not s.Equals(FirstRow.ToString) then
          begin
            if not sFirstRow.IsEmpty then sFirstRow := sFirstRow + '、';
            sFirstRow := sFirstRow + s;
          end;

        fdmtConsumer.Edit;
        f.AsString := sFirstRow;
        fdmtConsumer.Post;
      end;
    finally
      if (RecNo > 0) and (RecNo <= fdmtConsumer.RecordCount) then fdmtConsumer.RecNo := RecNo;
      fdmtConsumer.EnableControls;
    end;
  end);
end;

end.
