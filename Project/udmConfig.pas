unit udmConfig;

interface

uses
  System.SysUtils, System.Classes, System.Variants,
  FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteDef, FireDAC.Phys.SQLite;

type
  TdmConfig = class(TDataModule)
    FDConnection: TFDConnection;
    fdqKeyValue: TFDQuery;
    FDPhysSQLiteDriverLink: TFDPhysSQLiteDriverLink;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private

  public
    procedure WriteKeyValue(AKey: string; AValue: Variant);
    function ReadKeyValue(AKey: string; ADefaultValue: Variant): string;
  end;

var
  dmConfig: TdmConfig;

implementation

uses
  uGlobal;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TdmConfig.DataModuleCreate(Sender: TObject);
begin
  AppFilePath := ExtractFilePath(ParamStr(0));

  with FDConnection.Params do
  begin
    Values['DriverID'] := 'SQLite';
    Values['Database'] := AppFilePath + 'Config';
  end;
  FDConnection.Connected := True;
  fdqKeyValue.Open;
end;

procedure TdmConfig.WriteKeyValue(AKey: string; AValue: Variant);
begin
  if fdqKeyValue.Locate('KeyName', AKey, []) then fdqKeyValue.Edit
  else
  begin
    fdqKeyValue.Append;
    fdqKeyValue.FieldByName('KeyName').AsString := AKey;
  end;
  fdqKeyValue.FieldByName('KeyValue').AsString := VarToStr(AValue).Trim;
  fdqKeyValue.Post;
end;

procedure TdmConfig.DataModuleDestroy(Sender: TObject);
begin
  FDConnection.Close;
end;

function TdmConfig.ReadKeyValue(AKey: string; ADefaultValue: Variant): string;
begin
  if fdqKeyValue.Locate('KeyName', AKey, []) then
    Result := fdqKeyValue.FieldByName('KeyValue').AsString.Trim
  else Result := VarToStr(ADefaultValue).Trim;
end;

end.
