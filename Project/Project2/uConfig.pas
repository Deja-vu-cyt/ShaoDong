unit uConfig;

interface

uses
  SynCommons,
  mORMot;

type
  TSQLConfig = class(TSQLRecord)
  private
    fKeyName: RawUTF8;
    fKeyValue: RawUTF8;
  published
    property KeyName: RawUTF8 read fKeyName write fKeyName;
    property KeyValue: RawUTF8 read fKeyValue write fKeyValue;
  end;

implementation

end.
