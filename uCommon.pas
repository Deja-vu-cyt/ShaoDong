unit uCommon;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Types,
  System.IOUtils,
  System.Generics.Collections;

type
  TInt64DyadicArray = array of TInt64DynArray;

  TInt64Helper = record helper for Int64
  private
    const
      i64: Int64 = 1;
  public
    function ValueExist(Value: Byte): Boolean;
    procedure AddValue(Value: Byte);
    function ValueCount: Byte;
    function ToArray: TWordDynArray;
    function ToBinaryString: string;
  end;

  TInt64DynArrayHelper = record helper for TInt64DynArray
    procedure AddValue(Value: Word);
    function ToWordDynArray: TWordDynArray;
    function Compare(const Value: TInt64DynArray; Offset: Byte; MaxValue: Word): TInt64DynArray;
  end;

  TData = class
  private
    fValue: TInt64DynArray;
    fTotalValueCount: Word;
    fValueCount: Word;
    fValueCount2: Byte;
    fFirstValueRange: Byte;
  public
    constructor Create(Value: TInt64DynArray; FirstValueRange: Byte);
    function ToString: string;
    function CompareValueCount(v: TData): Byte;
    property Value: TInt64DynArray read fValue;
    property FirstValueRange: Byte read fFirstValueRange;
    property TotalValueCount: Word read fTotalValueCount;
    property ValueCount: Word read fValueCount;
    property ValueCount2: Byte read fValueCount2;
  end;

procedure BinarySearch(First, Last: Cardinal; Func: TFunc<Cardinal, Byte>);
procedure Foreach(TotalCount, EachTimeCount: Cardinal; Proc: TProc<TCardinalDynArray>);
function DigitToString(Digit: Cardinal): string;

implementation

procedure BinarySearch(First, Last: Cardinal; Func: TFunc<Cardinal, Byte>);
var
  i: Cardinal;
begin
  while First < Last do
  begin
    i := First + (Last - First) div 2;
    case Func(i) of
      1: First := i + 1;
      2: Last := i - 1;
      else Break;
    end;
  end;
end;

procedure Foreach(TotalCount, EachTimeCount: Cardinal; Proc: TProc<TCardinalDynArray>);
var
  r: TCardinalDynArray;
  i, i2, i3: Integer;
begin
  if (TotalCount = 0) or (TotalCount < EachTimeCount) then Exit;

  SetLength(r, EachTimeCount);
  for i := Low(r) to High(r) do r[i] := i + 1;
  repeat
    Proc(r);

    r[High(r)] := r[High(r)] + 1;
    if r[High(r)] > TotalCount then
    begin
      for i2 := High(r) - 1 downto Low(r) do
      begin
        r[i2] := r[i2] + 1;
        if r[i2] < TotalCount - High(r) + 1 + i2 then
        begin
          for i3 := i2 + 1 to High(r) do
            r[i3] := r[i3 - 1] + 1;
          Break;
        end;
      end;
    end;
  until r[Low(r)] > TotalCount - i + 1;
end;

function DigitToString(Digit: Cardinal): string;
begin
  Result := Digit.ToString;
  if (Result.Length < 2) and (Result <> '0') then Result := '0' + Result;
end;

function TInt64Helper.ValueExist(Value: Byte): Boolean;
begin
  Result := Self = Self or (i64 shl (64 - Value));
end;

procedure TInt64Helper.AddValue(Value: Byte);
begin
  Self := Self or (i64 shl (64 - Value));
end;

function TInt64Helper.ValueCount: Byte;
var
  v: Byte;
begin
  Result := 0;
  for v := 1 to 64 do
    if ValueExist(v) then Inc(Result);
end;

function TInt64Helper.ToArray: TWordDynArray;
var
  i, v: Byte;
begin
  SetLength(Result, ValueCount);
  i := 0;
  for v := 1 to 64 do
    if ValueExist(v) then
    begin
      Result[i] := v;
      Inc(i);
    end;
end;

function TInt64Helper.ToBinaryString: string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to 64 do
    Result := Result + Ord(Self.ValueExist(i)).ToString;
end;

procedure TInt64DynArrayHelper.AddValue(Value: Word);
var
  i: Integer;
begin
  i := (Value - 1) div 64;
  if i > High(Self) then SetLength(Self, i + 1);
  Self[i].AddValue(i mod 64);
end;

function TInt64DynArrayHelper.ToWordDynArray: TWordDynArray;
var
  i, i2, i3: Integer;
  t: TWordDynArray;
begin
  SetLength(Result, 0);
  for i := Low(Self) to High(Self) do
  begin
    //t := Self[i].ToArray(i * 64);
    if Length(t) > 0 then
    begin
      i2 := Length(Result);
      SetLength(Result, i2 + Length(t));
      for i3 := Low(t) to High(t) do
        Result[i2 + i3] := t[i3];
    end;
  end;
end;

function TInt64DynArrayHelper.Compare(const Value: TInt64DynArray;
  Offset: Byte; MaxValue: Word): TInt64DynArray;
var
  i, v, v2: Word;
begin
  SetLength(Result, 0);
  for v in Value.ToWordDynArray do
  begin
    v2 := v + Offset;
    if (v2 < 0) or (v2 > MaxValue) then Continue;
    i := v2 mod 64;
    if (i <= High(Self)) and Self[i].ValueExist(v2 div 64) then
    begin
      SetLength(Result, 0);
      Exit;
    end;
    Result.AddValue(v2);
  end;
end;

constructor TData.Create(Value: TInt64DynArray; FirstValueRange: Byte);
var
  i, ValueIndex, v: Integer;
begin
  inherited Create;
  fValue := Value;
  fFirstValueRange := FirstValueRange;
  //¼ÆËã¸öÊý
  for i := Low(fValue) to High(fValue) do
    fTotalValueCount := fTotalValueCount + fValue[i].ValueCount;
  fValueCount := fTotalValueCount;
  fValueCount2 := 0;
  if fTotalValueCount > 0 then
  begin
    ValueIndex := (fFirstValueRange - 1) div 64;
    if ValueIndex > High(fValue) then Exit;
    v := fFirstValueRange mod 64;

    for i := v + 1 to 64 do
      if fValue[ValueIndex].ValueExist(i) then Inc(fValueCount2);
    for i := ValueIndex + 1 to High(fValue) do
      fValueCount2 := fValueCount2 + fValue[i].ValueCount;

    fValueCount := fTotalValueCount - fValueCount2;
  end;
end;

function TData.ToString: string;
var
  i, j: Integer;
  Digit: Word;
  v: Int64;
  s: string;
begin
  for i := Low(fValue) to High(fValue) do
  begin
    v := fValue[i];
    for j := 1 to 64 do
    begin
      if v.ValueExist(j) then
      begin
        Digit := i * 64 + j;
        if (fFirstValueRange > 0) and (Digit > fFirstValueRange) then s := (Digit - fFirstValueRange).ToString
        else s := Digit.ToString;
        if s.Length < 2 then s := '0' + s;
        if (fFirstValueRange > 0) and (Digit > fFirstValueRange) and (Result.IndexOf('-') = -1) then Result := Result + ' - ' + s
        else if Result.IsEmpty then Result := s
        else Result := Result + '¡¢' + s;
      end;
    end;
  end;
end;

function TData.CompareValueCount(v: TData): Byte;
begin
  if fTotalValueCount > v.TotalValueCount then Result := 1
  else
  if fTotalValueCount < v.TotalValueCount then Result := 2
  else
  begin
    if Self.ValueCount > v.ValueCount then Result := 1
    else
    if Self.ValueCount < v.ValueCount then Result := 2
    else Result := 0;
  end;
end;

end.
