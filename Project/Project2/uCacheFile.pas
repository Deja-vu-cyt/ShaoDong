unit uCacheFile;

interface

uses
  SynCommons,
  mORMot;

type
  TSQLCacheFile = class(TSQLRecord)
  private
    fFileNo: Integer;
    fFileName: RawUTF8;
    fRowCount: Integer;
    fValueCount: Integer;
    fValueCount2: Integer;
  published
    property FileNo: Integer read fFileNo write fFileNo;
    property FileName: RawUTF8 read fFileName write fFileName;
    property RowCount: Integer read fRowCount write fRowCount;
    property ValueCount: Integer read fValueCount write fValueCount;
    property ValueCount2: Integer read fValueCount2 write fValueCount2;
  end;

implementation

end.
