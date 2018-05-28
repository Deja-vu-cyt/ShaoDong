unit uFileWriter;

interface

uses
  System.Types, System.SysUtils, System.IOUtils;

type
  TFileWriter = class
  private
    fFiles: TStringDynArray;
    fWriting: Boolean;
    fFileDirectory: string;
    fFileNo: Byte;
    fRowCount: Cardinal;
    fFileName: string;
    fActiveFileName: string;
    fTextFile: TextFile;

    procedure BuildActiveFileName;
  public
    constructor Create(aFileName: string);
    destructor Destroy; override;
    procedure WriteLn(s: string);
    procedure CloseFile;
    property Files: TStringDynArray read fFiles;
  end;

implementation

constructor TFileWriter.Create(aFileName: string);
begin
  inherited Create;
  SetLength(fFiles, 0);
  fWriting := False;
  fFileName := TPath.GetDirectoryName(aFileName);
  if fFileName.Substring(fFileName.Length - 1) <> '\' then fFileName := fFileName + '\';
  fFileName := fFileName + TPath.GetFileNameWithoutExtension(aFileName);
  fFileNo := 0;
  BuildActiveFileName;
end;

destructor TFileWriter.Destroy;
begin
  CloseFile;
  inherited Destroy;
end;

procedure TFileWriter.BuildActiveFileName;
begin
  fRowCount := 0;
  fFileNo := fFileNo + 1;
  fActiveFileName := fFileName;
  if fFileNo > 1 then
    fActiveFileName := fActiveFileName + Format('- %d', [fFileNo - 1]);
  fActiveFileName := fActiveFileName + '.txt';
  CloseFile;
end;

procedure TFileWriter.WriteLn(s: string);
var
  c, c2: Char;
  iCount: Word;
begin
  if not fWriting then
  begin
    if TFile.Exists(fActiveFileName) then TFile.Delete(fActiveFileName);
    AssignFile(fTextFile, fActiveFileName);
    Rewrite(fTextFile);
    fWriting := True;
    SetLength(fFiles, Length(fFiles) + 1);
    fFiles[High(fFiles)] := fActiveFileName;
  end;
  System.WriteLn(fTextFile, s);

  iCount := 1;
  for c in s do
  begin
    if c2 + c = #$D#$A then iCount := iCount + 1;
    c2 := c;
  end;
  fRowCount := fRowCount + iCount;
  if fRowCount >= 300000 then BuildActiveFileName;
end;

procedure TFileWriter.CloseFile;
begin
  if fWriting then
  begin
    System.CloseFile(fTextFile);
    fWriting := False;
  end;
end;

end.
