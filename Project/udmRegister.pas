unit udmRegister;

interface

uses
  System.SysUtils, System.Classes, System.Threading, Winapi.Windows, IdHTTP;

type
  TdmRegister = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FBeiJingTime: TDateTime;
    FMachineCode: string;
    FRegisterCode: string;
    FProbationExpired: Boolean;
    FClose: Boolean;
    FListeningTask: ITask;

    procedure edtMachineCodeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnRegisterClick(Sender: TObject);

    function GetCnCPUID(FormatStr: string = '%x-%x-%x-%x'): string;
    procedure SetCPU(h: THandle; CpuNo: Integer);
    procedure CheckBeiJingTime;
    procedure CheckUseDays;
    procedure ListeningUseDays;
  public
    procedure CheckRegiester;
    procedure RegisterSoftware;
    property ProbationExpired: Boolean read FProbationExpired;
  end;

var
  dmRegister: TdmRegister;

const
  EncryptKey = 'Deja vu';

implementation

uses
  AES, uLog, uGlobal,
  udmConfig, ufrmMain, ufrmRegister;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TdmRegister.DataModuleCreate(Sender: TObject);
begin
  SetCPU(GetCurrentProcess, 1);
  FMachineCode := GetCnCPUID('%0.8x%0.8x');
  FClose := False;
  //FBeiJingTime := 42736;
  FBeiJingTime := StrToDateTime('2017-01-01');
end;

procedure TdmRegister.DataModuleDestroy(Sender: TObject);
begin
  FClose := True;
  if Assigned(FListeningTask) then FListeningTask.Wait;
end;

procedure TdmRegister.edtMachineCodeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  with frmRegister.edtMachineCode do
  begin
    if ssCtrl in Shift then
    begin
      if Key = 82 then ReadOnly := False;
    end;
    if (Key = 13) and not ReadOnly then
      frmRegister.edtRegiesterCode.Text := EncryptString(Text, EncryptKey);
  end;
end;

procedure TdmRegister.btnRegisterClick(Sender: TObject);
begin
  FRegisterCode := Trim(frmRegister.edtRegiesterCode.Text);
  if FRegisterCode = '' then raise Exception.Create('注册码不能为空');
  if FMachineCode = DecryptString(FRegisterCode, EncryptKey) then
  begin
    FProbationExpired := False;
    dmConfig.WriteKeyValue('RegiesterCode', FRegisterCode);
    frmRegister.Close;
  end
  else raise Exception.Create('注册码错误');
end;

function TdmRegister.GetCnCPUID(FormatStr: string = '%x-%x-%x-%x'): string;
var
  iEax: Integer;
  iEbx: Integer;
  iEcx: Integer;
  iEdx: Integer;
begin
  asm
    push ebx
    push ecx
    push edx
    mov  eax, 1
    DW $A20F//cpuid
    mov  iEax, eax
    mov  iEbx, ebx
    mov  iEcx, ecx
    mov  iEdx, edx
    pop edx
    pop ecx
    pop ebx
  end;
  Result := Format(FormatStr, [iEax, iEbx, iEcx, iEdx]);
end;

procedure TdmRegister.SetCPU(h: THandle; CpuNo: Integer);
//CpuNo：决定了获得第几个cpu内核的第几个序列号。
var
  ProcessAffinity: DWORD_PTR;
  _SystemAffinity: DWORD_PTR;
begin
  GetProcessAffinityMask(h, ProcessAffinity, _SystemAffinity) ;
  ProcessAffinity := CpuNo; //this sets the process to only run on CPU 0
                              //for CPU 1 only use 2 and for CPUs 1 & 2 use 3
  SetProcessAffinityMask(h, ProcessAffinity)
end;

procedure TdmRegister.CheckBeiJingTime;
var
  s: string;
  IdHttp: TIdHttp;
  t: TTimeStamp;
  i: comp;
  d, d2, d3, d4: Double;
begin
  IdHttp := TIdHttp.Create;
  try
    s := 'http://api.k780.com:88/?app=life.time&appkey=10003&sign=b59bc3ef6191eb9f747dd4e83c99f2a4&format=json';
    try
      s := IdHttp.Get(s);
    except
      on e: Exception do
      begin
        TThread.Synchronize(nil, procedure
        begin
          FProbationExpired := True;
          frmMain.StatusBar.Panels[0].Text := '校验时间失败，请检查网络连接是否正常';
          Abort;
        end);
      end;
    end;

    if s.IndexOf('"success":"1"') > -1 then
    begin
      s := s.Substring(s.IndexOf('datetime_1":"') + 13, 19);
      FBeiJingTime := StrToDateTime(s);
    end;

    {if s.IndexOf('"success":"1"') > -1 then
    begin
      s := s.Substring(s.IndexOf('timestamp":"') + 12, 10);
      FBeiJingTime := 25569.334 + s.ToInt64 / 86400;
    end;}
  finally
    IdHttp.Free;
  end;
end;

procedure TdmRegister.CheckUseDays;
var
  UseDays: Integer;
  EncryptUseDays, sUseDays, EncryptLastUseDay, LastUseDay: string;
begin
  EncryptLastUseDay := dmConfig.ReadKeyValue('LastUseDay', '');
  LastUseDay := DecryptString(EncryptLastUseDay, EncryptKey);

  UseDays := 201;
  EncryptUseDays := dmConfig.ReadKeyValue('UseDays', '');
  sUseDays := DecryptString(EncryptUseDays, EncryptKey);
  TryStrToInt(sUseDays, UseDays);
  if LastUseDay <> FormatDateTime('YYYY-MM-DD', FBeiJingTime) then
  begin
    Inc(UseDays);
    dmConfig.WriteKeyValue('LastUseDay',  EncryptString(FormatDateTime('YYYY-MM-DD', FBeiJingTime), EncryptKey));
    dmConfig.WriteKeyValue('UseDays', EncryptString(UseDays.ToString, EncryptKey));
  end;
  if (UseDays > 200) or (FBeiJingTime > StrToDateTime('2017-12-31')) then
  begin
    FProbationExpired := True;
    TThread.Synchronize(nil, procedure
    begin
      frmMain.StatusBar.Panels[0].Text := '软件已超出试用天数';
      Abort;
    end);
  end
  else
  begin
    TThread.Synchronize(nil, procedure
    begin
      frmMain.StatusBar.Panels[0].Text := Format('软件剩余试用天数：%d', [200 - UseDays + 1]);
    end);
  end;
end;

procedure TdmRegister.ListeningUseDays;
begin
  FListeningTask := TTask.Create(procedure
  var
    CheckTime: TDateTime;
  begin
    CheckTime := StrToDateTime(FormatDateTime('YYYY-MM-DD', Now + 1));
    repeat
      try
        if (Now >= CheckTime) and not FProbationExpired then
        begin
          CheckBeiJingTime;
          CheckUseDays;
          CheckTime := StrToDateTime(FormatDateTime('YYYY-MM-DD', Now + 1));
        end;
      except
        on e: Exception do
        begin
          {TThread.Synchronize(nil, procedure
          begin
            raise Exception.Create(e.Message);
          end);}
        end;
      end;

      Sleep(500);
    until FClose;
  end);
  FListeningTask.Start;
end;

procedure TdmRegister.CheckRegiester;
var
  Regiestered: Boolean;
begin
  FProbationExpired := False;
  FRegisterCode := dmConfig.ReadKeyValue('RegiesterCode', '');
  if not FRegisterCode.IsEmpty then
  begin
    Regiestered := FMachineCode = DecryptString(FRegisterCode, EncryptKey);
    if Regiestered then
    begin
      TThread.Synchronize(nil, procedure
      begin
        frmMain.StatusBar.Panels[0].Text := '注册成功';
      end);
      Exit;
    end
    else FLog.Log('注册码无效');
  end;
  CheckBeiJingTime;
  CheckUseDays;
  if not Regiestered and not FProbationExpired then ListeningUseDays;
end;

procedure TdmRegister.RegisterSoftware;
begin
  if not Assigned(frmRegister) then
  begin
    frmRegister := TfrmRegister.Create(Self);
    frmRegister.edtMachineCode.OnKeyDown := edtMachineCodeKeyDown;
    frmRegister.btnRegister.OnClick := btnRegisterClick;
  end;

  frmRegister.edtMachineCode.Text := FMachineCode;
  frmRegister.edtRegiesterCode.Text := FRegisterCode;
  frmRegister.ShowModal;
end;

end.
