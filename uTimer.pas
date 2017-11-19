unit uTimer;

interface

uses
  System.Classes, System.SysUtils, System.Threading, System.DateUtils;

procedure StartTheTime(CheckStop: TFunc<Boolean>; Display: TProc<string>);

implementation

procedure StartTheTime(CheckStop: TFunc<Boolean>; Display: TProc<string>);
begin
  TTask.Create(procedure
  const
    sTip: string = '处理所需时间：';
  var
    UseTime: string;
    StartTime: TDateTime;
    Day, Hour, Min, Sec, Seconds: Integer;
    Stop: Boolean;
  begin
    StartTime := Now;
    while True do
    begin
      Sleep(1000);
      Seconds := SecondsBetween(Now, StartTime);
      Day := Seconds div 86400;
      Hour := (Seconds mod 86400) div 3600;
      Min := (Seconds mod 3600) div 60;
      Sec := Seconds mod 60;
      UseTime := Format('%d日%d时%d分%d秒', [Day, Hour, Min, Sec]);

      TThread.Synchronize(nil, procedure
      begin
        Display(sTip + UseTime);

        Stop := CheckStop;
      end);
      if Stop then Break;
    end;
  end).Start;
end;

end.
