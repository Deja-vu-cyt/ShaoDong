unit uCommon;

interface

uses
  mORMot,
  System.Classes,
  System.SysUtils,
  System.Types,
  System.IOUtils,
  System.Generics.Collections;

type
  TInt64DyadicArray = array of TInt64DynArray;
  TWordDyadicArray = array of System.Types.TWordDynArray;

  TInt64Helper = record helper for Int64
  private
    const
      i64: Int64 = 1;
  public
    function ValueExist(Value: Byte): Boolean;
    procedure AddValue(Value: Byte);
    procedure DeleteValue(Value: Byte);
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

  TSQLData = class(TSQLRecord)
  private
    fField1: Int64;
    fField2: Int64;
    fField3: Int64;
    fField4: Int64;
    fTotalValueCount: Word;
    fValueCount: Word;
    fValueCount2: Word;
    fIntervalValues: TWordDynArray;
    fIntervalValueCounts: TByteDynArray;
    fValues: TWordDynArray;
    function GetIntervalValueByIndex(IntervalIndex: Integer): Word;
    function GetValueIndexByIntervalIndex(IntervalIndex: Integer): Word;
  public
    constructor Create;
    procedure AssignValue(Source: TSQLData); overload;
    procedure AssignValue(s: string; aIntervalValues: TWordDynArray = []); overload;
    //procedure AssignValue(s: string); overload;
    procedure AddValues(Source: TSQLData);
    procedure AddValue(v: Word);
    procedure DeleteValue(v: Word); overload;
    procedure DeleteValue(v: Word; IntervalIndex: Integer); overload;
    procedure DeleteValueByIndex(IntervalIndex, ValueIndex: Integer);
    procedure ClearValue;
    function Value(IntervalIndex, ValueIndex: Integer): Word;
    function ValueExist(v: Word): Boolean; overload;
    function ValueExist(v: Word; IntervalIndex: Integer): Boolean; overload;
    function HasValue: Boolean;
    function IntervalIndexOfValue(v: Word): Integer;
    function ToString: string;
    procedure CalcValueCount(FirstRangeValue: Byte);
    function Values: TWordDynArray; overload;
    function Values(IntervalIndex: Integer): TWordDynArray; overload;
  published
    property Field1: Int64 read fField1 write fField1;
    property Field2: Int64 read fField2 write fField2;
    property Field3: Int64 read fField3 write fField3;
    property Field4: Int64 read fField4 write fField4;
    property TotalValueCount: Word read fTotalValueCount write fTotalValueCount;
    property ValueCount: Word read fValueCount write fValueCount;
    property ValueCount2: Word read fValueCount2 write fValueCount2;
    property IntervalValues: TWordDynArray read fIntervalValues;
    property IntervalValueCounts: TByteDynArray read fIntervalValueCounts;
  end;

  TSQLRow = class(TSQLData)
  private
    fNumber: Cardinal;
  published
    property Number: Cardinal read fNumber write fNumber stored AS_UNIQUE;
  end;


procedure BinarySearch(First, Last: Cardinal; Func: TFunc<Cardinal, Byte>);
procedure Foreach(TotalCount, EachTimeCount: Cardinal; Proc: TProc<TCardinalDynArray>);
function DigitToString(Digit: Cardinal): string;

function SeparateDigit(var s: string): string;
procedure StrToArray(s: string; var Arr: TWordDynArray; Offset: Integer = 0); overload;
procedure StrToArray(s: string; var Arr, Arr2: TWordDynArray); overload;
procedure ShellSort(var x: TWordDynArray);

implementation

constructor TSQLData.Create;
begin
  
end;

function TSQLData.GetIntervalValueByIndex(IntervalIndex: Integer): Word;
var
  i: Integer;
begin
  Result := 0;
  for i := Low(fIntervalValues) to IntervalIndex - 1 do
    Result := Result + fIntervalValues[i];
end;

function TSQLData.GetValueIndexByIntervalIndex(IntervalIndex: Integer): Word;
var
  i: Integer;
begin
  Result := 0;
  for i := Low(fIntervalValueCounts) to IntervalIndex - 1 do
    Result := Result + fIntervalValueCounts[i];
end;

procedure TSQLData.AssignValue(Source: TSQLData);
begin
  if not Assigned(Source) then Exit;
  fField1 := Source.Field1;
  fField2 := Source.Field2;
  fField3 := Source.Field3;
  fField4 := Source.Field4;
  fTotalValueCount := Source.TotalValueCount;
  fValueCount := Source.ValueCount;
  fValueCount2 := Source.ValueCount2;
  fIntervalValues :=  Source.IntervalValues;
  fIntervalValueCounts :=  Source.IntervalValueCounts;
end;

procedure TSQLData.AssignValue(s: string; aIntervalValues: TWordDynArray);
var
  sDigit: string;
  c: Char;
  Digit, IntervalIndex: Integer;
  IntervaValue: Word;
  CalcIntervalValues: Boolean;
  i: Integer;
begin
  ClearValue;
  fIntervalValues := aIntervalValues;
  CalcIntervalValues := Length(fIntervalValues) = 0;

  SetLength(fIntervalValueCounts, Length(aIntervalValues));
  for i := Low(fIntervalValueCounts) to High(fIntervalValueCounts) do
    fIntervalValueCounts[i] := 0;

  if CalcIntervalValues then
  begin
    SetLength(fIntervalValues, 1);
    fIntervalValues[0] := 0;
    SetLength(fIntervalValueCounts, 1);
    fIntervalValueCounts[0] := 0;
  end;
  IntervalIndex := Low(fIntervalValues);
  IntervaValue := 0;
  repeat
    sDigit := '';
    for c in s do
    begin
      if c = '-' then
      begin
        Inc(IntervalIndex);
        if CalcIntervalValues then
        begin
          SetLength(fIntervalValues, IntervalIndex + 1);
          fIntervalValues[IntervalIndex] := 0;
          IntervaValue := IntervaValue + fIntervalValues[IntervalIndex - 1];

          SetLength(fIntervalValueCounts, IntervalIndex + 1);
          fIntervalValueCounts[IntervalIndex] := 0;
        end
        else
        begin
          if IntervalIndex > High(fIntervalValues) then Exit;
          IntervaValue := IntervaValue + fIntervalValues[IntervalIndex - 1];
        end;
      end
      else if c in ['0'..'9'] then sDigit := sDigit + c
      else Break;
    end;
    if sDigit.IsEmpty then s := s.Substring(1)
    else s := s.Substring(sDigit.Length);

    if TryStrToInt(sDigit, Digit) then
    begin
      if CalcIntervalValues then
      begin
        if Digit > fIntervalValues[IntervalIndex] then fIntervalValues[IntervalIndex] := Digit;
      end
      else
      begin
        if Digit > fIntervalValues[IntervalIndex] then Continue;
      end;
      Digit := IntervaValue + Digit;
      case (Digit - 1) div 64 of
        0: fField1.AddValue((Digit - 1) mod 64 + 1);
        1: fField2.AddValue((Digit - 1) mod 64 + 1);
        2: fField3.AddValue((Digit - 1) mod 64 + 1);
        3: fField4.AddValue((Digit - 1) mod 64 + 1);
      end;

      fIntervalValueCounts[IntervalIndex] := fIntervalValueCounts[IntervalIndex] + 1;
    end;
  until s.IsEmpty;

  fValueCount := fIntervalValueCounts[0];
  fValueCount2 := 0;
  if Length(fIntervalValueCounts) > 1 then fValueCount2 := fIntervalValueCounts[1];
  fTotalValueCount := fValueCount + fValueCount2;
end;

{procedure TSQLData.AssignValue(s: string);
var
  sDigit: string;
  c: Char;
  Digit, MaxDigit, IntervalIndex: Integer;
  IntervaValue: Word;
begin
  ClearValue;
  IntervalIndex := 0;
  SetLength(fIntervalValues, 1);
  fIntervalValues[IntervalIndex] := 0;
  IntervaValue := 0;
  repeat
    sDigit := '';
    for c in s do
    begin
      if c = '-' then
      begin
        Inc(IntervalIndex);
        SetLength(fIntervalValues, IntervalIndex + 1);
        fIntervalValues[IntervalIndex] := 0;
        IntervaValue := IntervaValue + fIntervalValues[IntervalIndex - 1];
      end
      else if c in ['0'..'9'] then sDigit := sDigit + c
      else Break;
    end;
    if sDigit.IsEmpty then s := s.Substring(1)
    else s := s.Substring(sDigit.Length);

    if TryStrToInt(sDigit, Digit) then
    begin
      if Digit > fIntervalValues[IntervalIndex] then fIntervalValues[IntervalIndex] := Digit;
      Digit := IntervaValue + Digit;
      case (Digit - 1) div 64 of
        0: fField1.AddValue((Digit - 1) mod 64 + 1);
        1: fField2.AddValue((Digit - 1) mod 64 + 1);
        2: fField3.AddValue((Digit - 1) mod 64 + 1);
        3: fField4.AddValue((Digit - 1) mod 64 + 1);
      end;
    end;
  until s.IsEmpty;
end; }

procedure TSQLData.AddValues(Source: TSQLData);
begin
  if not Assigned(Source) then Exit;
  fField1 := fField1 or Source.Field1;
  fField2 := fField2 or Source.Field2;
  fField3 := fField3 or Source.Field3;
  fField4 := fField4 or Source.Field4;
end;

procedure TSQLData.AddValue(v: Word);
var
  i: Byte;
begin
  i := (v - 1) div 64;
  v := (v - 1) mod 64 + 1;
  case i of
    0: fField1.AddValue(v);
    1: fField2.AddValue(v);
    2: fField3.AddValue(v);
    3: fField4.AddValue(v);
  end;
end;

procedure TSQLData.DeleteValue(v: Word);
var
  i: Byte;
begin
  i := (v - 1) div 64;
  v := (v - 1) mod 64 + 1;
  case i of
    0: fField1.DeleteValue(v);
    1: fField2.DeleteValue(v);
    2: fField3.DeleteValue(v);
    3: fField4.DeleteValue(v);
  end;
end;

procedure TSQLData.DeleteValue(v: Word; IntervalIndex: Integer);
var
  i: Byte;
begin
  if IntervalIndex > High(fIntervalValues) then Exit;
  v := v + GetIntervalValueByIndex(IntervalIndex);
  DeleteValue(v);
  fIntervalValueCounts[IntervalIndex] := fIntervalValueCounts[IntervalIndex] - 1;
end;

procedure TSQLData.DeleteValueByIndex(IntervalIndex, ValueIndex: Integer);
var
  v: Word;
begin
  if IntervalIndex > High(fIntervalValues) then Exit;
  if ValueIndex > fIntervalValueCounts[IntervalIndex] - 1 then Exit;
  v := Value(IntervalIndex, ValueIndex);
  DeleteValue(v, IntervalIndex);
end;

procedure TSQLData.ClearValue;
begin
  fField1 := 0;
  fField2 := 0;
  fField3 := 0;
  fField4 := 0;
end;

function TSQLData.Value(IntervalIndex, ValueIndex: Integer): Word;
var
  i: Integer;
  IntervalValue: Word;
begin
  IntervalValue := 0;
  for i := Low(fIntervalValues) to IntervalIndex - 1 do
  begin
    IntervalValue := IntervalValue + fIntervalValues[i];
    ValueIndex := ValueIndex + fIntervalValueCounts[i];
  end;
  Result := Values[ValueIndex] - IntervalValue;
end;

function TSQLData.ValueExist(v: Word): Boolean;
var
  i: Byte;
begin
  Result := False;
  i := (v - 1) div 64;
  v := (v - 1) mod 64 + 1;
  case i of
    0: Result := fField1.ValueExist(v);
    1: Result := fField2.ValueExist(v);
    2: Result := fField3.ValueExist(v);
    3: Result := fField4.ValueExist(v);
  end;
end;

function TSQLData.ValueExist(v: Word; IntervalIndex: Integer): Boolean;
begin
  v := v + GetIntervalValueByIndex(IntervalIndex);
  Result := ValueExist(v);
end;

function TSQLData.HasValue: Boolean;
begin
  Result := not ((fField1 = 0) and (fField2 = 0) and (fField3 = 0) and (fField4 = 0));
end;

function TSQLData.IntervalIndexOfValue(v: Word): Integer;
var
  i: Integer;
  IntervalValue: Word;
begin
  Result := -1;
  if v = 0 then Exit;
  IntervalValue := 0;
  for i := Low(fIntervalValues) to High(fIntervalValues) do
  begin
    IntervalValue := IntervalValue + fIntervalValues[i];
    if IntervalValue >= v then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TSQLData.CalcValueCount(FirstRangeValue: Byte);
var
  v: Word;
begin
  fValueCount := 0;
  fTotalValueCount := 0;
  for v in Values do
  begin
    if v <= FirstRangeValue then fValueCount := fValueCount + 1;
    fTotalValueCount := fTotalValueCount + 1;
  end;
end;

function TSQLData.ToString: string;
var
  v, IntervalValue: Word;
  s: string;
  i, IntervalIndex: Integer;
begin
  Result := '';
  IntervalIndex := Low(fIntervalValues);
  IntervalValue := 0;
  for v in Values do
  begin
    while v > fIntervalValues[IntervalIndex] do
    begin
      Inc(IntervalIndex);
      IntervalValue := IntervalValue + fIntervalValues[IntervalIndex - 1];
      Result := Result + '-';
    end;
    s := (v - IntervalValue).ToString;
    if s.Length < 2 then s := '0' + s;
    if not (Result.IsEmpty or Result.Substring(Result.Length - 1).Equals('-')) then Result := Result + '¡¢';
    Result := Result + s;
  end;
  for i := IntervalIndex + 1 to High(fIntervalValues) do s := s + '-';
end;

function TSQLData.Values: TWordDynArray;
var
  i, i2, i3: Integer;
  v: Int64;
  t: TWordDynArray;
begin
  //if fValueRowNumber <> FillCurrentRow then
  begin
    SetLength(fValues, 0);
    for i := 1 to 4 do
    begin
      v := fField1;
      case i of
        2: v := fField2;
        3: v := fField3;
        4: v := fField4;
      end;
      if v = 0 then Continue;

      t := v.ToArray;
      if Length(t) > 0 then
      begin
        i2 := Length(fValues);
        SetLength(fValues, i2 + Length(t));
        for i3 := Low(t) to High(t) do
          fValues[i2 + i3] := t[i3] + (i - 1) * 64;
      end;
    end;
    //fValueRowNumber := FillCurrentRow;
  end;
  Result := fValues;
end;

function TSQLData.Values(IntervalIndex: Integer): TWordDynArray;
var
  i, ValueIndex: Integer;
begin
  SetLength(Result, IntervalValueCounts[IntervalIndex]);
  ValueIndex := GetValueIndexByIntervalIndex(IntervalIndex);
  Values;
  for i := Low(Result) to High(Result) do
    Result[i] := fValues[ValueIndex + i];
end;

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

procedure TInt64Helper.DeleteValue(Value: Byte);
begin
  Self := Self xor (i64 shl (64 - Value));
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

function SeparateDigit(var s: string): string;
var
  i: Integer;
  c: Char;
begin
  Result := '';
  i := 0;
  if s.Trim.IsEmpty then
  begin
    s := '';
    Exit;
  end;
  for c in s do
  begin
    if c in ['0'..'9'] then Result := Result + c
    else
    begin
      if not Result.IsEmpty then Break;
    end;

    Inc(i);
  end;
  s := s.Substring(i)
end;

procedure StrToArray(s: string; var Arr: TWordDynArray; Offset: Integer = 0);
var
  sColNo: string;
  ColNo: Integer;
  HasMinus: Boolean;
begin
  HasMinus := s.IndexOf('-') > -1;
  SetLength(Arr, 0);
  repeat
    sColNo := SeparateDigit(s);
    if TryStrToInt(sColNo, ColNo) then
    begin
      if HasMinus and (s.IndexOf('-') = -1) then ColNo := ColNo + Offset;

      SetLength(Arr, Length(Arr) + 1);
      Arr[Length(Arr) - 1] := ColNo;
    end;
  until s = '';
end;

procedure StrToArray(s: string; var Arr, Arr2: TWordDynArray);
var
  sColNo: string;
  ColNo: Integer;
  HasMinus: Boolean;
begin
  HasMinus := s.IndexOf('-') > -1;
  SetLength(Arr, 0);
  SetLength(Arr2, 0);
  repeat
    sColNo := SeparateDigit(s);
    if TryStrToInt(sColNo, ColNo) then
    begin
      if HasMinus and (s.IndexOf('-') = -1) then
      begin
        SetLength(Arr2, Length(Arr2) + 1);
        Arr2[Length(Arr2) - 1] := ColNo;
      end
      else
      begin
        SetLength(Arr, Length(Arr) + 1);
        Arr[Length(Arr) - 1] := ColNo;
      end;
    end;
  until s = '';
end;

procedure ShellSort(var x: TWordDynArray);
var
  h, i,j ,intTmp: Integer;
begin
  h:=high(x) div 2;
  while h>0 do
  begin
    for i:=h to high(x) do
    begin
      j:=i;
      while (j>=h) and (x[j-h]>x[j]) do
      begin
        intTmp:=x[j-h];
        x[j-h]:=x[j];
        x[j]:=intTmp;
        j:=j-h;
      end;
    end;
    h:=h div 2;
  end;
end;

end.
