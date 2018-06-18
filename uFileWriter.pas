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
    fFileNo: Word;
    fRowCount: Cardinal;
    fFileName: string;
    fActiveFileName: string;
    fTextFile: TextFile;
    fRebuildFileEvent: TFunc<Cardinal, Boolean>;
    fRebuildFileNameEvent: TFunc<TFileWriter, string>;
    fLastFileNo: Boolean;
  public
    constructor Create(aFileName: string);
    destructor Destroy; override;
    procedure BuildActiveFileName;
    procedure WriteLn(s: string);
    procedure CloseFile;
    procedure WriteFinish;
    procedure RenameLastFile(aFileName: string = '');
    property FileName: string read fFileName;
    property Files: TStringDynArray read fFiles;
    property RowCount: Cardinal read fRowCount;
    property FileNo: Word read fFileNo;
    property LastFileNo: Boolean read fLastFileNo;
    property RebuildFileEvent: TFunc<Cardinal, Boolean> read fRebuildFileEvent write fRebuildFileEvent;
    property RebuildFileNameEvent: TFunc<TFileWriter, string> read fRebuildFileNameEvent write fRebuildFileNameEvent;
  end;

implementation

constructor TFileWriter.Create(aFileName: string);
begin
  inherited Create;
  SetLength(fFiles, 0);
  fWriting := False;
  fFileDirectory := TPath.GetDirectoryName(aFileName);
  if fFileDirectory.Substring(fFileDirectory.Length - 1) <> '\' then
    fFileDirectory := fFileDirectory + '\';
  fFileName := TPath.GetFileNameWithoutExtension(aFileName);
  fFileNo := 0;
  fLastFileNo := False;

  BuildActiveFileName;
end;

destructor TFileWriter.Destroy;
begin
  CloseFile;
  inherited Destroy;
end;

procedure TFileWriter.BuildActiveFileName;
begin
  CloseFile;
  fRowCount := 0;
  fFileNo := fFileNo + 1;
  fActiveFileName := fFileName;
  if fFileNo > 1 then
  begin
    if Assigned(fRebuildFileNameEvent) then
      fActiveFileName := fRebuildFileNameEvent(Self);
  end;
  fActiveFileName := fFileDirectory + fActiveFileName + '.txt';
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

  if Assigned(fRebuildFileEvent) and fRebuildFileEvent(fRowCount) then BuildActiveFileName;
end;

procedure TFileWriter.CloseFile;
begin
  if fWriting then
  begin
    System.CloseFile(fTextFile);
    fWriting := False;
  end;
end;

procedure TFileWriter.WriteFinish;
begin
  fLastFileNo := True;
end;

procedure TFileWriter.RenameLastFile(aFileName: string );
var
  LastFileName: string;
begin
  CloseFile;
  WriteFinish;
  LastFileName := fActiveFileName;
  if fFileNo > 1 then
  begin
    if not aFileName.IsEmpty then
       fActiveFileName := aFileName
    else if Assigned(fRebuildFileNameEvent) then
      fActiveFileName := fRebuildFileNameEvent(Self);

    fActiveFileName := fFileDirectory + fActiveFileName + '.txt';
  end;
  if not LastFileName.Equals(fActiveFileName) then
    RenameFile(LastFileName, fActiveFileName);
end;

end.
