unit uDataTable;

interface

uses
  System.Types,
  mORMot;

type
  TSQLData = class(TSQLRecord)
  private
    fField1: Int64;
    fField2: Int64;
    fField3: Int64;
    fField4: Int64;
    function ReadValues: TIntegerDynArray;
    procedure WriteValues(AValue: TIntegerDynArray);
  published
    property Field1: Int64 read fField1 write fField1;
    property Field2: Int64 read fField2 write fField2;
    property Field3: Int64 read fField3 write fField3;
    property Field4: Int64 read fField4 write fField4;
    property Values: TIntegerDynArray read ReadValues write WriteValues;
    procedure ClearValue(AValue: TIntegerDynArray);
  end;

  TSQLClearColumn = class(TSQLData)
  private
    fFileNo: Integer;
  published
    property FileNo: Integer read fFileNo write fFileNo;
  end;

const
  i64: Int64 = 1;

implementation

function TSQLData.ReadValues: TIntegerDynArray;
var
  i: Integer;
  ColNo, v: Int64;
begin
  SetLength(Result, 0);
  for i := 1 to 4 do
  begin
    v := fField1;
    case i of
      2: v := fField2;
      3: v := fField3;
      4: v := fField4;
    end;
    for ColNo := 1 to 64 do
    begin
      if v = i64 shl (64 - i) or v then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1] := ColNo + (i - 1) * 64;
      end;
    end;
  end;
end;

procedure TSQLData.WriteValues(AValue: TIntegerDynArray);
var
  v: Int64;
begin
  Field1 := 0;
  Field2 := 0;
  Field3 := 0;
  Field4 := 0;
  for v in AValue do
  begin
    case v of
      1..64: Field1 := Field1 or (i64 shl (64 - v));
      65..128: Field2 := Field2 or (i64 shl (64 - v + 64));
      129..192: Field3 := Field3 or (i64 shl (64 - v + 128));
      193..256: Field4 := Field4 or (i64 shl (64 - v + 192));
    end;
  end;
end;

procedure TSQLData.ClearValue(AValue: TIntegerDynArray);
var
  v: Int64;
begin
  for v in AValue do
  begin
    case v of
      1..64: Field1 := Field1 xor (i64 shl (64 - v));
      65..128: Field2 := Field2 xor (i64 shl (64 - v + 64));
      129..192: Field3 := Field3 xor (i64 shl (64 - v + 128));
      193..256: Field4 := Field4 xor (i64 shl (64 - v + 192));
    end;
  end;
end;

end.
