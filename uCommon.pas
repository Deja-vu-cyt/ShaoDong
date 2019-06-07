unit uCommon;

interface

uses
  mORMot,
  SynCommons,
  SynSQLite3,
  System.Classes,
  System.SysUtils,
  System.Types,
  System.IOUtils,
  System.Variants,
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

  TWordDynArrayHelper = record helper for TWordDynArray
  public
    procedure Assign(s: string);
    procedure Clear;
    procedure Add(v: Word);
    function Exist(v: Word): Boolean;
    function Equals(v: TWordDynArray): Boolean;
    function Contains(v: TWordDynArray): Boolean;
    function ToString: string;
  end;

  {TData = class
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
  end;}

  TSQLKeyValue = class(TSQLRecord)
  private
    fRest: TSQLRest;
    procedure CheckRest;
  protected
    fKey: RawUTF8;
    fValue: Variant;
  public
    procedure SetRest(aRest: TSQLRest);
    procedure GetKeyValue(Key: string; var v: Variant); overload;
    procedure GetKeyValue(Key: string; var v: TWordDynArray); overload;
    procedure GetKeyValue(Key: string; var v: TInt64DynArray); overload;
    procedure SetKeyValue(Key: string; v: Variant); overload;
    procedure SetKeyValue(Key: string; v: TWordDynArray); overload;
    procedure SetKeyValue(Key: string; v: TInt64DynArray); overload;

    procedure SetArrayValue(aValue: TWordDynArray);
  published
    property Key: RawUTF8 read fKey write fKey;
    property Value: Variant read fValue write fValue;
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
    //procedure AssignValue(s: string; aIntervalValues: TWordDynArray = []); overload;
    procedure AssignValue(s: string; aPlaceholder: string; aIntervalValues: TWordDynArray = []; aDataMode: Byte = 0); overload;
    procedure AssignValue(s: string; aIntervalValues: TWordDynArray = []; aDataMode: Byte = 0); overload;
    //procedure AssignValue(s: string); overload;
    procedure AddValues(Source: TSQLData);
    procedure AddValue(v: Word); overload;
    procedure AddValue(v: Word; IntervalIndex: Integer); overload;
    procedure DeleteValue(v: Word); overload;
    procedure DeleteValue(v: Word; IntervalIndex: Integer); overload;
    procedure DeleteValueByIndex(IntervalIndex, ValueIndex: Integer);
    procedure ClearValue;
    function Value(IntervalIndex, ValueIndex: Integer): Word;
    function ValueExist(v: Word): Boolean; overload;
    function ValueExist(v: Word; IntervalIndex: Integer): Boolean; overload;
    function HasValue: Boolean;
    function IntervalIndexOfValue(v: Word): Integer;
    class function ToString(aValues: TWordDynArray; aIntervalValues: TWordDynArray = [];
      aDataMode: Byte = 0; aPlaceholder: string = ''): string; overload;
    function ToString(aDataMode: Byte = 0; aPlaceholder: string = ''): string; overload;
    procedure CalcValueCount(aIntervalValues: TWordDynArray); overload;
    procedure CalcValueCount; overload;
    function Values: TWordDynArray; overload;
    function Values(IntervalIndex: Integer): TWordDynArray; overload;
    function Values(s: string): TWordDynArray; overload;
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

  TSortAlgorithm = class
  public
    class procedure Shell(var x: TWordDynArray; Compare: TFunc<Word, Word, Boolean>);
    class procedure Stochastic(var x: TWordDynArray);
    class procedure OddEvenAlternate(var x: TWordDynArray; EvenFirst: Boolean;
      CheckEven: TFunc<Word, Boolean> = nil);
  end;

  TCombinatorialAlgorithm = class
  private
    class procedure ForWorker(TotalCount, EachTimeCount, Number: Cardinal;
      Proc: TProc<Cardinal, TCardinalDynArray>; Stop: TFunc<Cardinal, Cardinal, Boolean> = nil);
  public
    class procedure &For(TotalCount, EachTimeCount, Number: Cardinal;
      Proc: TProc<Cardinal, TCardinalDynArray>; Stop: TFunc<Cardinal, Cardinal, Boolean> = nil); overload;
    class procedure &For(TotalCount, EachTimeCount: Cardinal;
      Proc: TProc<Cardinal, TCardinalDynArray>; Stop: TFunc<Cardinal, Cardinal, Boolean> = nil); overload;
  end;

var
  fDirectory: string;
  fLogDirectory: string;

procedure InternalSQLFunctionValueExist(Context: TSQLite3FunctionContext;
  Argc: Integer; var Argv: TSQLite3ValueArray); cdecl;


procedure BinarySearch(First, Last: Cardinal; Func: TFunc<Cardinal, Byte>);

{procedure Foreach(TotalCount, EachTimeCount: Cardinal; Proc: TProc<TWordDynArray>;
  Stop: TFunc<Boolean> = nil); overload;
procedure Foreach(TotalCount, EachTimeCount: Cardinal; Proc: TProc<TWordDynArray>;
  Proc2: TProc<TWordDynArray> = nil; Stop: TFunc<Boolean> = nil); overload;
procedure Foreach(TotalCount, EachTimeCount, Number: Cardinal;
  Proc: TProc<Cardinal, TWordDynArray>; Stop: TFunc<Cardinal, Cardinal, Boolean>); overload;
procedure Foreach2(TotalCount, EachTimeCount: Cardinal; Proc: TProc<TCardinalDynArray>); }
function DigitToString(Digit: Cardinal): string;

function SeparateDigit(var s: string): string;
procedure StrToArray(s: string; var Arr: TWordDynArray; Offset: Integer = 0); overload;
procedure StrToArray(s: string; var Arr, Arr2: TWordDynArray); overload;
procedure ShellSort(var x: TWordDynArray); overload;
//procedure ShellSort(var x: TWordDynArray; Compare: TFunc<Boolean, Word, Word>);

implementation

procedure TSQLKeyValue.CheckRest;
begin
  if not Assigned(fRest) then
    raise Exception.Create('Invalid rest');
end;

procedure TSQLKeyValue.SetRest(aRest: TSQLRest);
begin
  fRest := aRest;
end;

procedure TSQLKeyValue.GetKeyValue(Key: string; var v: Variant);
begin
  CheckRest;

  v := Unassigned;
  FillPrepare(fRest, 'Key = ?', [Key]);
  if FillOne then v := fValue;
end;

procedure TSQLKeyValue.GetKeyValue(Key: string; var v: TWordDynArray);
var
  v2: Variant;
  i: Integer;
begin
  GetKeyValue(Key, v2);
  if VarIsEmpty(v2) or VarIsNull(v2) then SetLength(v, 0)
  else
  begin
    SetLength(v, Integer(v2._Count));
    for i := 0 to v2._Count - 1 do v[i] := v2.Value(i);
  end;
end;

procedure TSQLKeyValue.GetKeyValue(Key: string; var v: TInt64DynArray);
var
  v2: Variant;
  i: Integer;
begin
  GetKeyValue(Key, v2);
  if VarIsEmpty(v2) or VarIsNull(v2) then SetLength(v, 0)
  else
  begin
    SetLength(v, Integer(v2._Count));
    for i := 0 to v2._Count - 1 do v[i] := v2.Value(i);
  end;
end;

procedure TSQLKeyValue.SetKeyValue(Key: string; v: Variant);
begin
  CheckRest;

  FillPrepare(fRest, 'Key = ?', [Key]);
  if FillOne then
  begin
    fValue := v;
    fRest.Update(Self);
  end
  else
  begin
    fKey := Key;
    fValue := v;
    fRest.Add(Self, True);
  end;
end;

procedure TSQLKeyValue.SetKeyValue(Key: string; v: TWordDynArray);
var
  v2: Variant;
  i: Integer;
begin
  TDocVariant.New(v2);
  for i := Low(v) to High(v) do v2.Add(v[i]);
  SetKeyValue(Key, v2);
end;

procedure TSQLKeyValue.SetKeyValue(Key: string; v: TInt64DynArray);
var
  v2: Variant;
  i: Integer;
begin
  TDocVariant.New(v2);
  for i := Low(v) to High(v) do v2.Add(v[i]);
  SetKeyValue(Key, v2);
end;

procedure TSQLKeyValue.SetArrayValue(aValue: TWordDynArray);
var
  i: Integer;
begin
  TDocVariant.New(fValue);
  for i := Low(aValue) to High(aValue) do fValue.Add(aValue[i]);
end;

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

{procedure TSQLData.AssignValue(s: string; aIntervalValues: TWordDynArray);
var
  sDigit: string;
  c: Char;
  Digit, IntervalIndex: Integer;
  IntervaValue: Word;
  CalcIntervalValues: Boolean;
  i, DigitLength: Integer;
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
      if c in ['0'..'9'] then sDigit := sDigit + c
      else Break;
    end;
    if sDigit.IsEmpty then s := s.Substring(1)
    else
    begin
      s := s.Substring(sDigit.Length);
      if c = '-' then s := s.Substring(1);
    end;

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
    end;
  until s.IsEmpty;

  CalcValueCount;
end;}

procedure TSQLData.AssignValue(s: string; aPlaceholder: string;
  aIntervalValues: TWordDynArray; aDataMode: Byte);
var
  sValue, sDigit, sTemp: string;
  c, c2: Char;
  i, i2, Digit, IntervalIndex: Integer;
  v, IntervaValue: Word;
  CalcIntervalValues: Boolean;
  HasLeft: Boolean;
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

  case aDataMode of
    1:
    begin
      for c in s do
      begin
        case c of
          '（':
          begin
            sValue := '';
          end;
          '）':
          begin
            i := sValue.IndexOf('-');
            if i > -1 then sValue := sValue.Substring(i + 1);
            i := sValue.IndexOf('：');
            if i = -1 then Continue;
            if not TryStrToInt(sValue.Substring(0, i), i2) then Continue;
            sValue := sValue.SubString(i + 1);
            if (IntervalIndex > 0) and not aPlaceholder.IsEmpty then
            begin
              for i := Low(aPlaceholder) to High(aPlaceholder) do
                sValue := sValue.Replace(aPlaceholder[i], i.ToString);
            end;
            repeat
              sDigit := '';
              for c2 in sValue do
              begin
                if c2 in ['0'..'9'] then sDigit := sDigit + c2
                else Break;
              end;
              if sDigit.IsEmpty then sValue := sValue.Substring(1)
              else sValue := sValue.Substring(sDigit.Length);

              if TryStrToInt(sDigit, Digit) then
              begin
                if IntervalIndex = 0 then v := (i2 - 1) * 10 + Digit + 1
                else v := Digit;
                //v := (i2 - 1) * 10 + Digit + 1;
                if v > fIntervalValues[IntervalIndex] then
                begin
                  if CalcIntervalValues then fIntervalValues[IntervalIndex] := v
                  else Continue;
                end;

                v := IntervaValue + v;
                case (v - 1) div 64 of
                  0: fField1.AddValue((v - 1) mod 64 + 1);
                  1: fField2.AddValue((v - 1) mod 64 + 1);
                  2: fField3.AddValue((v - 1) mod 64 + 1);
                  3: fField4.AddValue((v - 1) mod 64 + 1);
                end;

                fIntervalValueCounts[IntervalIndex] := fIntervalValueCounts[IntervalIndex] + 1;
              end;
            until sValue.IsEmpty;

            sValue := '';
          end;
          else
          begin
            if (c = '-') and sValue.Trim.IsEmpty then
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
            end;
            sValue := sValue + c;
          end;
        end;
      end;
    end;
    2:
    begin
      for c in s do
      begin
        case c of
          '（':
          begin
            sValue := '';
            HasLeft := True;
          end;
          '）':
          begin
            i := sValue.IndexOf('-');
            if i > -1 then sValue := sValue.Substring(i + 1).Trim;
            if not sTemp.IsEmpty then sTemp := sTemp + '、';
            sTemp := sTemp + sValue;
            HasLeft := False;
          end;
          else
          begin
            if HasLeft then
              sValue := sValue + c
            else
              sTemp := sTemp + c;
          end;
        end;
      end;
      AssignValue(sTemp, '', aIntervalValues, 0);
      Exit;
    end;
    else
    begin
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
          if c in ['0'..'9'] then sDigit := sDigit + c
          else Break;
        end;
        if sDigit.IsEmpty then s := s.Substring(1)
        else
        begin
          s := s.Substring(sDigit.Length);
          if c = '-' then s := s.Substring(1);
        end;

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
        end;
      until s.IsEmpty;
    end;
  end;

  CalcValueCount;
end;

procedure TSQLData.AssignValue(s: string; aIntervalValues: TWordDynArray; aDataMode: Byte);
begin
  AssignValue(s, '', aIntervalValues, aDataMode);
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

procedure TSQLData.AddValue(v: Word; IntervalIndex: Integer);
var
  i: Byte;
begin
  if IntervalIndex > High(fIntervalValues) then Exit;
  v := v + GetIntervalValueByIndex(IntervalIndex);
  AddValue(v);
  fIntervalValueCounts[IntervalIndex] := fIntervalValueCounts[IntervalIndex] + 1;
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

procedure TSQLData.CalcValueCount(aIntervalValues: TWordDynArray);
var
  i, IntervalIndex, IntervalValue: Integer;
  v: Word;
begin
  if Length(aIntervalValues) > 0 then
  begin
    fIntervalValues := aIntervalValues;
    SetLength(fIntervalValueCounts, Length(fIntervalValues));
    for i := Low(fIntervalValueCounts) to High(fIntervalValueCounts) do
      fIntervalValueCounts[i] := 0;
    IntervalIndex := 0;
    IntervalValue := fIntervalValues[IntervalIndex];
    for v in Values do
    begin
      if v > IntervalValue then
      begin
        Inc(IntervalIndex);
        IntervalValue := IntervalValue + fIntervalValues[IntervalIndex];
      end;
      fIntervalValueCounts[IntervalIndex] := fIntervalValueCounts[IntervalIndex] + 1;
    end;
  end;
  fValueCount := fIntervalValueCounts[0];
  fValueCount2 := 0;
  if Length(fIntervalValueCounts) > 1 then fValueCount2 := fIntervalValueCounts[1];
  fTotalValueCount := fValueCount + fValueCount2;
end;

procedure TSQLData.CalcValueCount;
begin
  CalcValueCount(fIntervalValues);
end;

class function TSQLData.ToString(aValues: TWordDynArray; aIntervalValues: TWordDynArray;
   aDataMode: Byte; aPlaceholder: string): string;
const
  SubIntervalValue: Byte = 10;
var
  v, v2, IntervalValue, SubIntervalNo, LastSubIntervalNo: Word;
  s: string;
  i, IntervalIndex, ValueNo: Integer;
  Unclosed: Boolean;
begin
  Result := '';
  IntervalIndex := Low(aIntervalValues);
  IntervalValue := 0;
  LastSubIntervalNo := 0;
  ValueNo := 0;
  Unclosed := False;
  for v in aValues do
  begin
    Inc(ValueNo);
    while v > IntervalValue + aIntervalValues[IntervalIndex] do
    begin
      LastSubIntervalNo := 0;
      Inc(IntervalIndex);
      ValueNo := 0;
      IntervalValue := IntervalValue + aIntervalValues[IntervalIndex - 1];
      if Unclosed then
      begin
        if Unclosed then Result := Result + '）';
        Unclosed := False;
      end;
      Result := Result + '-';
    end;
    v2 := v - IntervalValue;
    case aDataMode of
      1:
      begin
        SubIntervalNo := 1;
        if IntervalIndex = 0 then
        begin
          SubIntervalNo := (v2 - 1) div SubIntervalValue + 1;
          v2 := v2 mod SubIntervalValue;
          if v2 = 0 then v2 := SubIntervalValue;
          v2 := v2 - 1;
        end;
        s := v2.ToString;
        if not aPlaceholder.IsEmpty and (IntervalIndex > 0) and (v2 <= High(aPlaceholder)) then
          s := aPlaceholder[v2] + s;

        if SubIntervalNo > LastSubIntervalNo then
        begin
          if Unclosed then
          begin
            Result := Result + '）';
            Unclosed := False;
          end;

          Result := Result + Format('（%d-%d：', [IntervalIndex + 1, SubIntervalNo]);

          LastSubIntervalNo := SubIntervalNo;
          Unclosed := True;
        end;
        if not Result.Substring(Result.Length -1).Equals('：') then Result := Result + '、';
        Result := Result + s;
      end;
      2:
      begin
        s := v2.ToString;
        if IntervalIndex = 0 then
          s := Format('（ %d-%d ）', [ValueNo, v2])
        else
        begin
          if not Result.Substring(Result.Length -1).Equals('-') then Result := Result + '、';
          if ValueNo = 0 then Result := Result + ' ';
        end;
        Result := Result + s;
      end;
      else
      begin
        s := v2.ToString;
        if s.Length < 2 then s := '0' + s;
        if not (Result.IsEmpty or Result.Substring(Result.Length - 1).Equals('-')) then Result := Result + '、';
        Result := Result + s;
      end;
    end;
  end;
  if Unclosed then Result := Result + '）';
  for i := IntervalIndex + 1 to High(aIntervalValues) do Result := Result + '-';
end;

function TSQLData.ToString(aDataMode: Byte; aPlaceholder: string): string;
begin
  Result := ToString(Values, fIntervalValues, aDataMode, aPlaceholder);
end;

function TSQLData.Values: TWordDynArray;
var
  i, i2, i3: Integer;
  v: Int64;
  t: TWordDynArray;
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
  Result := fValues;
end;

function TSQLData.Values(IntervalIndex: Integer): TWordDynArray;
var
  i, ValueIndex: Integer;
  IntervalValue: Word;
begin
  SetLength(Result, IntervalValueCounts[IntervalIndex]);
  ValueIndex := GetValueIndexByIntervalIndex(IntervalIndex);
  IntervalValue := GetIntervalValueByIndex(IntervalIndex);
  Values;
  for i := Low(Result) to High(Result) do
    Result[i] := fValues[ValueIndex + i] - IntervalValue;
end;

function TSQLData.Values(s: string): TWordDynArray;
begin

end;

procedure InternalSQLFunctionValueExist(Context: TSQLite3FunctionContext;
  Argc: Integer; var Argv: TSQLite3ValueArray); cdecl;
var
  StartPos: Integer;
begin
  case Argc of
    1:;
    else
    begin
      ErrorWrongNumberOfArgs(Context);
      Exit;
    end;
  end;
  if (sqlite3.value_type(argv[0]) = SQLITE_NULL)
    or (sqlite3.value_type(argv[1]) = SQLITE_NULL)
  then
    sqlite3.result_int64(Context, 0)
  else
    sqlite3.result_int64(Context, 1);
  //sqlite3.value_text(argv[0]),sqlite3.value_text(argv[1]);
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

{procedure Foreach(TotalCount, EachTimeCount: Cardinal; Proc: TProc<TWordDynArray>;
  Stop: TFunc<Boolean>);
begin
  Foreach(TotalCount, EachTimeCount, Proc, nil, Stop);
end;

procedure Foreach(TotalCount, EachTimeCount: Cardinal; Proc: TProc<TWordDynArray>;
  Proc2: TProc<TWordDynArray>; Stop: TFunc<Boolean>);
var
  r, r2: TWordDynArray;
  i, i2, i3: Integer;
begin
  if (TotalCount = 0) or (TotalCount < EachTimeCount) then Exit;

  SetLength(r, EachTimeCount);
  if EachTimeCount > 0 then SetLength(r2, EachTimeCount - 1);

  for i := Low(r) to High(r) do r[i] := i + 1;
  r[High(r)] := r[High(r)] - 1;

  repeat
    if Assigned(Stop) and Stop then Exit;

    r[High(r)] := r[High(r)] + 1;
    //每次新的前n-1位执行
    if (Length(r) > 1) and (r[High(r)] - r[High(r) - 1] = 1) and Assigned(Proc2) then
    begin
      for i := Low(r2) to High(r2) do r2[i] := r[i];
      Proc2(r2);
      //不符合跳过
    end;

    if Assigned(Proc) then Proc(r);

    if r[High(r)] = TotalCount then
    begin
      for i2 := High(r) - 1 downto Low(r) do
      begin
        r[i2] := r[i2] + 1;
        if r[i2] < TotalCount - High(r) + 1 + i2 then
        begin
          for i3 := i2 + 1 to High(r) do
            r[i3] := r[i3 - 1] + 1;
          r[High(r)] := r[High(r)] - 1;
          Break;
        end;
      end;
    end;
  until r[High(r)] = TotalCount;
end;

procedure Foreach(TotalCount, EachTimeCount, Number: Cardinal;
  Proc: TProc<Cardinal, TWordDynArray>; Stop: TFunc<Cardinal, Cardinal, Boolean>);
var
  r, r2: TWordDynArray;
  i, i2, i3: Integer;
  StartNumber, EndNumber, GroupCount: Cardinal;
begin
  if (TotalCount = 0) or (TotalCount < EachTimeCount) or (TotalCount - EachTimeCount + 1 < Number) then Exit;

  SetLength(r, EachTimeCount);
  StartNumber := 1;
  EndNumber := TotalCount - EachTimeCount + 1;
  if Number > 0 then
  begin
    StartNumber := Number;
    EndNumber := Number;
  end;

  for Number := StartNumber to EndNumber do
  begin
    for i := Low(r) to High(r) do r[i] := Number + i;
    r[High(r)] := r[High(r)] - 1;
    GroupCount := 0;
    repeat
      if Assigned(Stop) and Stop(Number, GroupCount) then Exit;

      r[High(r)] := r[High(r)] + 1;
      if Assigned(Proc) then Proc(Number, r);
      GroupCount := GroupCount + 1;

      if r[High(r)] = TotalCount then
      begin
        for i2 := High(r) - 1 downto Low(r) + 1 do
        begin
          r[i2] := r[i2] + 1;
          if r[i2] < TotalCount - High(r) + 1 + i2 then
          begin
            for i3 := i2 + 1 to High(r) do
              r[i3] := r[i3 - 1] + 1;
            r[High(r)] := r[High(r)] - 1;
            Break;
          end;
        end;
      end;
    until r[High(r)] = TotalCount;
  end;
end;

procedure Foreach2(TotalCount, EachTimeCount: Cardinal; Proc: TProc<TCardinalDynArray>);
var
  r: TCardinalDynArray;
  i, i2, i3: Integer;
begin
  if (TotalCount = 0) or (TotalCount < EachTimeCount) then Exit;

  SetLength(r, EachTimeCount);
  for i := Low(r) to High(r) do r[i] := i + 1;
  repeat
    Proc(r);

    for i := Low(r) to High(r) do r[i] := r[i] + 1;
  until r[High(r)] > TotalCount;
end;}

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
    t := Self[i].ToArray;
    if Length(t) > 0 then
    begin
      i2 := Length(Result);
      SetLength(Result, i2 + Length(t));
      for i3 := Low(t) to High(t) do
        Result[i2 + i3] := i * 64 + t[i3];
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

procedure TWordDynArrayHelper.Assign(s: string);
var
  t: TArray<string>;
  i, v: Integer;
begin
  t := s.Split(['、']);
  SetLength(Self, Length(t));
  for i := Low(Self) to High(Self) do
    if TryStrToInt(t[i], v) then Self[i] := v else Self[i] := 0;
end;

procedure TWordDynArrayHelper.Clear;
begin
  SetLength(Self, 0);
end;

procedure TWordDynArrayHelper.Add(v: Word);
begin
  SetLength(Self, Length(Self) + 1);
  Self[High(Self)] := v;
end;

function TWordDynArrayHelper.Exist(v: Word): Boolean;
var
  v2: Word;
begin
  Result := False;
  for v2 in Self do
  begin
    Result := v2 = v;
    if Result then Break;
  end;
end;

function TWordDynArrayHelper.Equals(v: TWordDynArray): Boolean;
var
  i, SameCount: Integer;
begin
  Result := False;
  if Length(Self) <> Length(v) then Exit;

  SameCount := 0;
  for i := Low(Self) to High(Self) do
    if Self[i] = v[i] then Inc(SameCount) else Break;

  Result := SameCount = Length(Self);
end;

function TWordDynArrayHelper.Contains(v: TWordDynArray): Boolean;
var
  v2: Word;
  SameCount: Integer;
begin
  Result := False;
  SameCount := 0;
  for v2 in v do
    if Self.Exist(v2) then Inc(SameCount) else Break;

  Result := SameCount = Length(v);
end;

function TWordDynArrayHelper.ToString: string;
var
  v: Word;
begin
  Result := '';
  for v in Self do
  begin
    if not Result.IsEmpty then Result := Result + '、';
    Result := Result + v.ToString;
  end;
end;

{constructor TData.Create(Value: TInt64DynArray; FirstValueRange: Byte);
var
  i, ValueIndex, v: Integer;
begin
  inherited Create;
  fValue := Value;
  fFirstValueRange := FirstValueRange;
  //计算个数
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
        else Result := Result + '、' + s;
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
end;}

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

class procedure TSortAlgorithm.Shell(var x: TWordDynArray; Compare: TFunc<Word, Word, Boolean>);
var
  i, j, gap: Integer;
  t: Word;
begin
  gap := Length(x) div 2;
  while gap > 0 do
  begin
    for i := gap to High(x) do
    begin
      j := i;
      while (j >= gap) and Compare(x[j - gap], x[j]) do
      begin
        t := x[j - gap];
        x[j - gap] := x[j];
        x[j] := t;

        j := j - gap;
      end;
    end;
    gap := gap div 2;
  end;
end;

class procedure TSortAlgorithm.Stochastic(var x: TWordDynArray);
var
  i, i2: Integer;
  t: Word;
begin
  for i := Low(x) to High(x) do
  begin
    i2 := Random(High(x));
    if i2 = i then Continue;

    t := x[i];
    x[i] := x[i2];
    x[i2] := t;
  end;
end;

class procedure TSortAlgorithm.OddEvenAlternate(var x: TWordDynArray; EvenFirst: Boolean;
  CheckEven: TFunc<Word, Boolean>);
var
  t, t2: TWordDynArray;
  i, iMinCount: Integer;
  IsEven: Boolean;
begin
  for i := Low(x) to High(x) do
  begin
    if Assigned(CheckEven) then IsEven := CheckEven(x[i])
    else IsEven := x[i] mod 2 = 0;
    if IsEven then
    begin
      SetLength(t, Length(t) + 1);
      t[High(t)] := x[i];
    end
    else
    begin
      SetLength(t2, Length(t2) + 1);
      t2[High(t2)] := x[i];
    end;
  end;
  iMinCount := Length(t);
  if Length(t2) < iMinCount then iMinCount := Length(t2);
  for i := 0 to iMinCount - 1 do
  begin
    if EvenFirst then
    begin
      x[i * 2] := t[i];
      x[i * 2 + 1] := t2[i];
    end
    else
    begin
      x[i * 2] := t2[i];
      x[i * 2 + 1] := t[i];
    end;
  end;
  for i := iMinCount to High(t) do x[iMinCount + i] := t[i];
  for i := iMinCount to High(t2) do x[iMinCount + i] := t2[i];
end;

class procedure TCombinatorialAlgorithm.ForWorker(TotalCount, EachTimeCount, Number: Cardinal;
  Proc: TProc<Cardinal, TCardinalDynArray>; Stop: TFunc<Cardinal, Cardinal, Boolean>);
var
  r: TCardinalDynArray;
  i, i2, i3: Integer;
  StartNumber, EndNumber, GroupCount: Cardinal;
begin
  if (TotalCount = 0) or (TotalCount < EachTimeCount) or (TotalCount - EachTimeCount + 1 < Number) then Exit;

  SetLength(r, EachTimeCount);
  StartNumber := 1;
  EndNumber := TotalCount - EachTimeCount + 1;
  if Number > 0 then
  begin
    StartNumber := Number;
    EndNumber := Number;
  end;

  for Number := StartNumber to EndNumber do
  begin
    for i := Low(r) to High(r) do r[i] := Number + i;
    r[High(r)] := r[High(r)] - 1;
    GroupCount := 0;
    repeat
      if Assigned(Stop) and Stop(Number, GroupCount) then Exit;

      r[High(r)] := r[High(r)] + 1;
      if Assigned(Proc) then Proc(Number, r);
      GroupCount := GroupCount + 1;

      if r[High(r)] = TotalCount then
      begin
        for i2 := High(r) - 1 downto Low(r) + 1 do
        begin
          r[i2] := r[i2] + 1;
          if r[i2] < TotalCount - High(r) + 1 + i2 then
          begin
            for i3 := i2 + 1 to High(r) do
              r[i3] := r[i3 - 1] + 1;
            r[High(r)] := r[High(r)] - 1;
            Break;
          end;
        end;
      end;
    until r[High(r)] = TotalCount;
  end;
end;

class procedure TCombinatorialAlgorithm.&For(TotalCount, EachTimeCount, Number: Cardinal;
  Proc: TProc<Cardinal, TCardinalDynArray>; Stop: TFunc<Cardinal, Cardinal, Boolean> = nil);
begin
  ForWorker(TotalCount, EachTimeCount, Number, Proc, Stop);
end;

class procedure TCombinatorialAlgorithm.&For(TotalCount, EachTimeCount: Cardinal;
  Proc: TProc<Cardinal, TCardinalDynArray>; Stop: TFunc<Cardinal, Cardinal, Boolean> = nil);
begin
  ForWorker(TotalCount, EachTimeCount, 0, Proc, Stop);
end;


initialization
  fDirectory := TPath.GetDirectoryName(ParamStr(0));
  if not fDirectory.Substring(fDirectory.Length - 1).Equals('\') then fDirectory := fDirectory + '\';
  fLogDirectory := fDirectory + 'Log\';
  //if not TDirectory.Exists(fLogDirectory) then TDirectory.CreateDirectory(fLogDirectory);

end.
