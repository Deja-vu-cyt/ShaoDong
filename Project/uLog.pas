unit uLog;

interface

uses
  System.Classes, System.SysUtils, System.Threading, System.Generics.Collections,
  FireDAC.Comp.Client, FireDAC.Phys.SQLite, FireDAC.Stan.Def, FireDAC.Stan.Async;

type
  TLogInfo = class
    LogTime: TDateTime;
    Msg: string;
  end;

  TLog = class
  private
    FLock: TObject;
    FRunning: Boolean;
    FQueue: TThreadedQueue<TLogInfo>;
    FDConnection: TFDConnection;
    function ReadRunning: Boolean;
    procedure SetRunning(AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure Log(AMsg: string);
    procedure ShowLogWindow;
    property Running: Boolean read FRunning;
  end;

var
  FLog: TLog;

implementation

uses
  ufrmLog;

constructor TLog.Create;
begin
  inherited Create;
  FRunning := False;

  frmLog := TfrmLog.Create(nil);
  FLock := TObject.Create;
  FQueue := TThreadedQueue<TLogInfo>.Create(100, INFINITE, 100);

  FDConnection := TFDConnection.Create(nil);
  with FDConnection.Params do
  begin
    Values['DriverID'] := 'SQLite';
    Values['Database'] := Format('%sLog', [ExtractFilePath(ParamStr(0))]);
  end;
end;

destructor TLog.Destroy;
begin
  FDConnection.Free;
  FreeAndNil(FQueue);
  FLock.Free;
  frmLog.Free;
  inherited Destroy;
end;

function TLog.ReadRunning: Boolean;
begin
  TMonitor.Enter(FLock);
  try
    Result := FRunning;
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TLog.SetRunning(AValue: Boolean);
begin
  TMonitor.Enter(FLock);
  try
    FRunning := AValue;
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TLog.Start;
var
  s: string;
  LogFileExists: Boolean;
  LogInfo: TLogInfo;
begin
  try
    //连接数据库
    LogFileExists := FileExists(FDConnection.Params.Values['Database']);
    FDConnection.Connected := True;
    if not LogFileExists then
    begin
      s := 'CREATE TABLE Log(' + #$D#$A
        + 'LogTime DATETIME NOT NULL,' + #$D#$A
        + 'Msg NTEXT NOT NULL' + #$D#$A
        + ')';
      FDConnection.ExecSQL(s);
    end;
    FDConnection.Open;

    repeat
      LogInfo := FQueue.PopItem;
      if LogInfo <> nil then LogInfo.Free;
    until LogInfo = nil;

    //启动写线程
    TTask.Create(procedure
    var
      s: string;
      LogInfo: TLogInfo;
    begin
      if not FDConnection.Connected then Exit;

      s := 'INSERT INTO Log VALUES(:LogTime, :Msg)';
      repeat
        try
          LogInfo := FQueue.PopItem;
          if LogInfo <> nil then
          begin
            if FDConnection.Connected then
              FDConnection.ExecSQL(s, [LogInfo.LogTime, LogInfo.Msg]);

            TThread.Synchronize(nil, procedure
            var
              i: Integer;
            begin
              if frmLog.mmoLog.Lines.Count >= 30 then
                for i := 1 to 10 do frmLog.mmoLog.Lines.Delete(8);
              frmLog.mmoLog.Lines.Add(FormatDateTime('YYYY-MM-DD HH:MM:SS', LogInfo.LogTime) + #$D#$A + '  ' + LogInfo.Msg);
            end);

            LogInfo.Free;
          end;
        except
        end;
      until not FRunning;
    end).Start;
    FRunning := True;
  except
  end;
end;

procedure TLog.Stop;
begin
  FRunning := False;
  Sleep(200);
  FDConnection.Close;
end;

procedure TLog.Log(AMsg: string);
var
  LogInfo: TLogInfo;
begin
  if not Running then Exit;

  LogInfo := TLogInfo.Create;
  LogInfo.LogTime := Now;
  LogInfo.Msg := AMsg;

  FQueue.PushItem(LogInfo);
end;

procedure TLog.ShowLogWindow;
begin
  if not frmLog.Visible then frmLog.Show;
  SwitchToThisWindow(frmLog.Handle, True);
end;

initialization;
  FLog := TLog.Create;

finalization;
  FLog.Free;

end.
