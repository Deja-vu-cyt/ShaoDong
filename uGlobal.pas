unit uGlobal;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Variants, System.IOUtils,
  System.Math;

type
  TIntDyadicArray = array of TIntegerDynArray;
  TInt64DyadicArray = array of TInt64DynArray;
  TWorkState = (wsNone, wsInputData, wsInputResultData, wsClearColumn, wsExportToFile, wsExecuting);

const
  i64: Int64 = 1;

var
  AppFilePath: string;

function SeparateDigit(var s: string): string;
function BuildStringValue(Arr: TIntegerDynArray; Offset: Integer = 0): string; overload;
function BuildStringValue(Arr: TInt64DynArray; Offset: Integer = 0): string; overload;
procedure StrToIntArray(s: string; var Arr: TIntegerDynArray; Offset: Integer = 0); overload;
procedure StrToIntArray(s: string; var Arr, Arr2: TIntegerDynArray); overload; //
procedure LoadData(AFileName: string; var Arr, Arr2: TIntDyadicArray); overload; //
procedure LoadData(FileName: string; var Arr: TIntDyadicArray; Offset: Integer = 0); overload;
procedure LoadData(FileName: string; var Arr: TInt64DyadicArray; Offset: Integer = 0); overload;
function Int64ArrayEqual(Arr, Arr2: TInt64DynArray): Boolean;
function BuildGUID: string;
procedure ShellSort(var x: TIntegerDynArray);

implementation

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

function BuildStringValue(Arr: TIntegerDynArray; Offset: Integer = 0): string; overload;
var
  Digit: Integer;
  s: string;
begin
  Result := '';
  for Digit in Arr do
  begin
    if (Offset > 0) and (Digit > Offset) then s := (Digit - Offset).ToString
    else s := Digit.ToString;
    if s.Length < 2 then s := '0' + s;
    if (Offset > 0) and (Digit > Offset) and (Result.IndexOf('-') = -1) then Result := Result + ' - ' + s
    else if Result.IsEmpty then Result := s
    else Result := Result + '¡¢' + s;
  end;
  if (Offset > 0) and (Result.IndexOf('-') = -1) then Result := Result + ' - ';
end;

function BuildStringValue(Arr: TInt64DynArray; Offset: Integer = 0): string;
var
  i: Integer;
  Arr2: TIntegerDynArray;
begin
  for i := 1 to Length(Arr) * 64 do
    if Arr[Ceil(i / 64) - 1] = Arr[Ceil(i / 64) - 1] or i64 shl (64 - i mod 64) then
    begin
      SetLength(Arr2, Length(Arr2) + 1);
      Arr2[Length(Arr2) - 1] := i;
    end;
  Result := BuildStringValue(Arr2, Offset);
end;

procedure StrToIntArray(s: string; var Arr: TIntegerDynArray; Offset: Integer = 0);
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

procedure StrToIntArray(s: string; var Arr, Arr2: TIntegerDynArray);
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

procedure LoadData(AFileName: string; var Arr, Arr2: TIntDyadicArray);
var
  i, RowNo: Integer;
  s: string;
begin
  if not FileExists(AFileName) then Exit;
  SetLength(Arr, 0);
  SetLength(Arr2, 0);
  with TStringList.Create do
  begin
    try
      LoadFromFile(AFileName);
      for i := 0 to Count - 1 do
      begin
        if not TryStrToInt(Names[i].Trim, RowNo) then Continue;
        if RowNo > Length(Arr) then
        begin
          SetLength(Arr, RowNo);
          SetLength(Arr2, RowNo);
        end;
        s := ValueFromIndex[i];
        StrToIntArray(s, Arr[RowNo - 1], Arr2[RowNo - 1]);
      end;
    finally
      Free;
    end;
  end;
end;

procedure LoadData(FileName: string; var Arr: TIntDyadicArray; Offset: Integer = 0);
var
  i, RowNo: Integer;
  s: string;
begin
  if not FileExists(FileName) then Exit;
  SetLength(Arr, 0);
  with TStringList.Create do
  begin
    try
      LoadFromFile(FileName);
      for i := 0 to Count - 1 do
      begin
        if not TryStrToInt(Names[i].Trim, RowNo) then Continue;
        if RowNo > Length(Arr) then SetLength(Arr, RowNo);
        s := ValueFromIndex[i];
        StrToIntArray(s, Arr[RowNo - 1], Offset);
      end;
    finally
      Free;
    end;
  end;
end;

procedure LoadData(FileName: string; var Arr: TInt64DyadicArray; Offset: Integer = 0);
var
  Arr2: TIntDyadicArray;
  i, i2, ArrayIndex, ArrayLength, Digit: Integer;
begin
  LoadData(FileName, Arr2, Offset);
  SetLength(Arr, Length(Arr2));
  for i := Low(Arr) to High(Arr) do
  begin
    ArrayLength := 0;
    for Digit in Arr2[i] do
    begin
      ArrayIndex := (Digit - 1) div 64;
      if ArrayLength <= ArrayIndex then
      begin
        SetLength(Arr[i], ArrayIndex + 1);
        for i2 := ArrayLength to ArrayIndex do Arr[i][i2] := 0;
        ArrayLength := ArrayIndex + 1;
      end;
      Arr[i][ArrayIndex] := Arr[i][ArrayIndex] or i64 shl (64 - Digit mod 64);
    end;
  end;
end;

function Int64ArrayEqual(Arr, Arr2: TInt64DynArray): Boolean;
var
  i, ArrayLength: Integer;
  a, a2: TInt64DynArray;
begin
  Result := (Length(Arr) > 0) and (Length(Arr2) > 0);
  if not Result then Exit;

  a := Arr;
  a2 := Arr2;
  if Length(Arr) < Length(Arr2) then
  begin
    a := Arr2;
    a2 := Arr;
  end;
  ArrayLength := Length(a2);
  SetLength(a2, Length(a));
  for i := ArrayLength to High(a2) do a2[i] := 0;
  for i := Low(a) to High(a) do
  begin
    Result := a[i] = a[i] or a2[i];
    if not Result then Break;
  end;
end;

function BuildGUID: string;
begin
  Result := TGUID.NewGuid.ToString.Replace('-', '').Replace('{', '').Replace('}', '');
  Result := LowerCase(Result);
end;

procedure ShellSort(var x: TIntegerDynArray);
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

initialization

finalization

end.
