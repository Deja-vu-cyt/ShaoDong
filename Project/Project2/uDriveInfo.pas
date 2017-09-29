unit uDriveInfo;

interface

uses
  Winapi.Windows, System.SysUtils, System.IOUtils, System.Types;

var
  SystemDrive: string;
  Drives: TStringDynArray;

function BuildFileName(AFileNo: Integer): string;
function GetDriveCanUseSize(ADrive: string): Integer;

implementation

function GetSystemDrive: string;
var
  c: PChar;
begin
  GetMem(c, 100);
  try
    GetSystemDirectory(c, 100) ;
    Result := TDirectory.GetDirectoryRoot(c);
  finally
    FreeMem(c);
  end;
end;

procedure InitDrives;
var
  s: string;
begin
  for s in TDirectory.GetLogicalDrives do
  begin
    if (GetDriveType(PChar(s)) <> DRIVE_FIXED) or s.Equals(SystemDrive) then Continue;

    SetLength(Drives, Length(Drives) + 1);
    Drives[Length(Drives) - 1] := s;
  end;
end;

function GetDriveCanUseSize(ADrive: string): Integer;
var
  d1, d2, d3, d4: DWORD;
begin
  GetDiskFreeSpace(PChar(ADrive), d1, d2, d3, d4);
  Result := d3 div (1024 * 1024 div d1 div d2);
end;

function BuildFileName(AFileNo: Integer): string;
var
  s: string;
begin
  for s in Drives do
  begin
    if GetDriveCanUseSize(s) > 1024 then
    begin
      Result := Format('%sCache Data\%d\', [s, AFileNo]);
      Break;
    end;
  end;
  if Result.IsEmpty then raise Exception.Create('´¢´æ¿Õ¼ä²»×ã');

  Result := Result + LowerCase(TGUID.NewGuid.ToString.Replace('-', '').Replace('{', '').Replace('}', ''));
end;

initialization
  SystemDrive := GetSystemDrive;
  InitDrives;

end.
