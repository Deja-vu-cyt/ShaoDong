unit uControlHelper;

interface

uses
  System.SysUtils,
  Vcl.Controls,
  Vcl.StdCtrls;

type
  TEditHelper = class helper for TWinControl
    function TryToValue(var v: Integer; EmptyValue: Integer = 0): Boolean;
  end;

implementation

function TEditHelper.TryToValue(var v: Integer; EmptyValue: Integer): Boolean;
begin
  Result := Trim(Text).IsEmpty;
  if Result then
  begin
    v := EmptyValue;
    Exit;
  end;
  Result := TryStrToInt(Trim(Text), v);
  if Result then Exit;
  SetFocus;
  if Self is TCustomEdit then TCustomEdit(Self).SelectAll;
end;

end.
