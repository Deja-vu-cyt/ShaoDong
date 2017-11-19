unit uGlobal;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Variants, System.IOUtils,
  System.Math,
  FireDAC.Comp.Client, Data.DB;

type
  TIntDyadicArray = array of TIntegerDynArray;
  TWorkState = (wsNone, wsInputData, wsInputResultData, wsClearColumn, wsExportToFile, wsExecuting);

  TFDConnectionHelp = class helper for TFDConnection
    procedure SQLiteBuildTable(ATableName, ASQL: string);
  end;

  TDataSetHelp = class helper for TDataSet
    procedure LoadFromFile(AFileName: string; AMethod: TDataSetNotifyEvent = nil);
  end;

const
  i64: Int64 = 1;

var
  AppFilePath: string;

function SeparateDigit(var s: string): string;
procedure StrToIntArray(s: string; var Arr: TIntegerDynArray; Offset: Integer = 0); overload;
procedure StrToIntArray(s: string; var Arr, Arr2: TIntegerDynArray); overload;
procedure LoadData(AFileName: string; var Arr, Arr2: TIntDyadicArray);
function BuildGUID: string;
procedure ShellSort(var x: TIntegerDynArray);

implementation

procedure TFDConnectionHelp.SQLiteBuildTable(ATableName: string; ASQL: string);
var
  s: string;
begin
  s := Format('SELECT COUNT(*) FROM sqlite_master WHERE type = ''table'' AND name = ''%s''', [ATableName]);
  if VarToStr(ExecSQLScalar(s)).ToInteger = 0 then ExecSQL(ASQL);
end;

procedure TDataSetHelp.LoadFromFile(AFileName: string; AMethod: TDataSetNotifyEvent = nil);
var
  l: TStringList;
  s, FileExt, sColNo: string;
  i, RowNo, ColNo, ValueCount: Integer;
  f: TField;
begin
  if not TFile.Exists(AFileName) then Exit;
  l := TStringList.Create;
  try
    FileExt := ExtractFileExt(AFileName);
    {if LowerCase(FileExt).Equals('.txt') then l.LoadFromFile(AFileName)
    else if LowerCase(FileExt).Equals('.xls') or LowerCase(FileExt).Equals('.xlsx') then
    begin

    end
    else Exit;  }

    for i := 0 to l.Count - 1 do
    begin
      if not TryStrToInt(l.Names[i].Trim, RowNo) then Continue;
      s := l.ValueFromIndex[i].Trim;

      ValueCount := 0;
      Append;
      repeat
        sColNo := SeparateDigit(s);
        if TryStrToInt(sColNo, ColNo) then
        begin
          f := FieldByName('Field' + Ceil(ColNo / 64).ToString);
          f.AsLargeInt := f.AsLargeInt or (i64 shl (64 - ColNo));
          Inc(ValueCount);
        end;
      until s.IsEmpty;

      f := FindField('ValueCount');
      if Assigned(f) then f.AsInteger := ValueCount;
      f := FindField('RowNo');
      if Assigned(f) then f.AsInteger := RowNo;

      if Assigned(AMethod) then AMethod(Self);
      Post;
    end;
  finally
    l.Free;
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
