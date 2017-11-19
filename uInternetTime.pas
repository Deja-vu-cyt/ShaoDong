unit uInternetTime;

interface

uses
  System.SysUtils, IdHTTP;

function GetInternetTime: TDateTime;

implementation

function GetInternetTime: TDateTime;
var
  s: string;
begin
  Result := 0;
  with TIdHttp.Create do
  begin
    try
      s := 'http://api.k780.com:88/?app=life.time&appkey=10003&sign=b59bc3ef6191eb9f747dd4e83c99f2a4&format=json';
      try
        s := Get(s);
        if s.IndexOf('"success":"1"') > -1 then
        begin
          s := s.Substring(s.IndexOf('datetime_1":"') + 13, 19);
          Result := StrToDateTime(s);
        end;
      except
        on e: Exception do
          raise Exception.Create('校验时间失败，请检查网络连接是否正常' + #$D#$A + e.Message);
      end;
    finally
      Free;
    end;
  end;
end;

end.
