unit Dejavu.WaitableTimer;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils, System.SyncObjs,
  System.Generics.Collections,
  Winapi.Windows;

type
  TOnTimer = procedure(lpArgToCompletionRoutine: Pointer; dwTimerLowValue: DWORD;
    dwTimerHighValue: DWORD); stdcall;

  TWaitableTimer = class(TThread)
  protected
    fHandle: THandle;
    fPeriod: Cardinal;
    fOnTimer: TOnTimer;
    fSender: TObject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    procedure Stop;
    property OnTimer: TOnTimer read fOnTimer write fOnTimer;
  published
    property Period: Cardinal read fPeriod write fPeriod;
    property Sender: TObject read fSender write fSender;
  end;

  TCircularWaitableTimer = class(TWaitableTimer)
  private type
    TTask = class
      Slot: Word;
      Cycle: Cardinal;
      Value: TThreadMethod;
    end;
  private
    function GetSize: Word;
    procedure SetSize(Value: Word);
  protected
    fCurrentIndex: Word;
    fCurrentTime: TDateTime;
    fList: TObjectList<TThreadList<TTask>>;
    function NextIndex(Secs: Cardinal = 1): Word;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ExecuteTask;
    procedure AddTask(aTask: TThreadMethod; Time: TDateTime = 0);
  published
    property Size: Word read GetSize write SetSize;
  end;

implementation

constructor TWaitableTimer.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  fHandle := CreateWaitableTimer(nil, True, nil);
  fPeriod := 1000;
end;

destructor TWaitableTimer.Destroy;
begin
  CancelWaitableTimer(fHandle);
  CloseHandle(fHandle);
  inherited Destroy;
end;

procedure TWaitableTimer.Execute;
var
  DueTime: Int64;
begin
  DueTime := 0;
  if SetWaitableTimer(fHandle, DueTime, fPeriod, @fOnTimer, @fSender, False) then
  while not Terminated do
    SleepEx(INFINITE, True);
end;

procedure TWaitableTimer.Stop;
begin
  if Suspended then Resume;
  Terminate;
end;

procedure OnCircularTimer(lpArgToCompletionRoutine: Pointer; dwTimerLowValue: DWORD;
  dwTimerHighValue: DWORD); stdcall;
var
  Timer: TCircularWaitableTimer;
begin
  Timer := TCircularWaitableTimer(lpArgToCompletionRoutine^);
  Timer.ExecuteTask;
end;

constructor TCircularWaitableTimer.Create;
begin
  inherited Create;
  fList := TObjectList<TThreadList<TTask>>.Create;
  Size := 60;
  fCurrentTime := Now;
  fCurrentIndex := Size - 1;
  Sender := Self;
  fOnTimer := OnCircularTimer;
end;

destructor TCircularWaitableTimer.Destroy;
begin
  fList.Free;
  inherited Destroy;
end;

function TCircularWaitableTimer.GetSize: Word;
begin
  Result := fList.Count;
end;

procedure TCircularWaitableTimer.SetSize(Value: Word);
var
  i, j: Integer;
begin
  for i := fList.Count - 1 downto Value do
  begin
    with fList[i].LockList do
    begin
      try
        for j := Count - 1 downto 0 do Items[i].Free;
      finally
        fList[i].UnlockList;
      end;
    end;
    fList.Delete(i);
  end;
  for i := fList.Count + 1 to Value do fList.Add(TThreadList<TTask>.Create);
end;

function TCircularWaitableTimer.NextIndex(Secs: Cardinal): Word;
begin
  Result := (fCurrentIndex + Secs) mod fList.Count;
end;

procedure TCircularWaitableTimer.ExecuteTask;
var
  Slot: Word;
begin
  fCurrentTime := Now;
  fCurrentIndex := NextIndex;
  Slot := fCurrentIndex;
  TThread.CreateAnonymousThread(procedure
  var
    i: Integer;
  begin
    with fList[Slot].LockList do
    begin
      try
        for i := Count - 1 downto 0 do
        begin
          if Items[i].Cycle = 0 then
          begin
            try
              if Assigned(Items[i].Value) then Items[i].Value;
            finally
              Delete(i);
            end;
          end
          else Items[i].Cycle := Items[i].Cycle - 1;
        end;
      finally
        fList[Slot].UnlockList;
      end;
    end;
  end).Start;
end;

procedure TCircularWaitableTimer.AddTask(aTask: TThreadMethod; Time: TDateTime);
var
  Secs: Int64;
  Task: TTask;
begin
  Task := TTask.Create;
  Task.Value := aTask;
  Task.Cycle := 0;
  Task.Slot := NextIndex;
  if Time > fCurrentTime then
  begin
    Secs := SecondsBetween(Time, fCurrentTime) + 1;
    Task.Slot := NextIndex(Secs) mod Size;
    Task.Cycle := Secs div Size;
  end;
  fList[Task.Slot].Add(Task);
end;

end.
