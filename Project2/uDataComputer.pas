unit uDataComputer;

interface

uses
  SynCommons,
  mORMot,
  mORMotSQLite3,
  mORMotHttpServer,
  mORMotHttpClient,
  SynSQLite3Static,
  System.Classes,
  System.Variants,
  System.SysUtils,
  System.Types,
  System.IOUtils,
  System.Threading,
  System.Generics.Collections,
  System.SyncObjs,
  System.Diagnostics,
  IdIPWatch,
  uCommon,
  ServiceIntf,
  uFileWriter;

type
  TSQLFirstRow = class(TSQLRecord)
  protected
    fValue : Word;
    fRowSpacing: Word;
    fRowCount: Cardinal;
    fCodeName: TInt64DynArray;
    fCodeNameValueCount: Word;
    fGrouped: Boolean;
  public
    procedure AddCodeName(v: Word);
    function CodeNameExist(aCodeName: TWordDynArray): Boolean;
    procedure ClearCodeName;
  published
    property Value: Word read fValue write fValue stored AS_UNIQUE;
    property RowSpacing: Word read fRowSpacing write fRowSpacing;
    property RowCount: Cardinal read fRowCount write fRowCount;
    property CodeName: TInt64DynArray read fCodeName write fCodeName;
    property CodeNameValueCount: Word read fCodeNameValueCount write fCodeNameValueCount;
    property Grouped: Boolean read fGrouped write fGrouped;
  end;

  TSQLCodeName = class(TSQLRecord)
  private
    fValue: RawUTF8;
    fValueCount: Byte;
    fRowCount: Word;
  public
    function SlantValue: string; overload;
    class function SlantValue(v: string): string; overload;
  published
    property Value: RawUTF8 read fValue write fValue stored AS_UNIQUE;
    property ValueCount: Byte read fValueCount write fValueCount;
    property RowCount: Word read fRowCount write fRowCount;
  end;

  TSQLCompareData = class(TSQLData)
  protected
    fFirstRow: Word;
    fCodeName: RawUTF8;
    fCodeNameValueCount: Byte;
    fRowSpacing: Word;
  published
    property FirstRow: Word read fFirstRow write fFirstRow;
    property CodeName: RawUTF8 read fCodeName write fCodeName;
    property CodeNameValueCount: Byte read fCodeNameValueCount write fCodeNameValueCount;
    property RowSpacing: Word read fRowSpacing write fRowSpacing;
  end;

  TService = class(TInterfacedObject, IService)
  public
    procedure RegisterConsumer(const Consumer: INotifyCallback; const ID, Address: string);
    procedure InitFinish(const ID: string; const ProcessorCount: Cardinal);
    procedure TaskFinish(const ID: string; const FirstRow: Cardinal; const CodeName: string);
    procedure TaskFailed(const ID: string; const FirstRow: Cardinal);
    procedure CallbackReleased(const Callback: IInvokable; const InterfaceName: RawUTF8);
  end;

  TCodeNameConsumer = class(TThread)
  protected
    fFirstRow: TSQLFirstRow;
    fGroupValueCount: Byte;
    fExportGroupValueCount: Byte;
    fMaxGroupCount: Int64;
    fCodeNameSort: Int64;
    fCodeNameCount: TDictionary<Word, Word>;
    fQueue: TThreadedQueue<Word>;
    fr: TFileWriter;
    fCodeName: TWordDynArray;
    fMaxRowCount: Word;
    fActiveFirstRow: Word;
    fActiveCodeName: TWordDynArray;
    function GetCodeNameRows(CodeName: TWordDynArray): TWordDynArray;
    function GetCodeNameRowCount(CodeName: TWordDynArray): Word; overload;
    function GetCodeNameRowCount(CodeName: TWordDynArray; Rows: TWordDynArray): Word; overload;
    procedure FindCodeNameWithMostRowCount(CodeNames: TWordDynArray);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    procedure BuildCodeNameCount;
  published
    property FirstRow: TSQLFirstRow read fFirstRow;
    property GroupValueCount: Byte read fGroupValueCount write fGroupValueCount;
    property ExportGroupValueCount: Byte read fExportGroupValueCount write fExportGroupValueCount;
    property MaxGroupCount: Int64 read fMaxGroupCount write fMaxGroupCount;
    property CodeNameSort: Int64 read fCodeNameSort write fCodeNameSort;
    property Queue: TThreadedQueue<Word> read fQueue;
    property ActiveFirstRow: Word read fActiveFirstRow;
    property ActiveCodeName: TWordDynArray read fActiveCodeName;
  end;
  TCodeNameConsumers = array of TCodeNameConsumer;

  TNotifyCallback = class(TInterfacedCallback, INotifyCallback)
  protected
    fID: string;
    fConsumers: TCodeNameConsumers;
    fQueue: TThreadedQueue<TCodeNameConsumer>;
    procedure Init(const GroupValueCount, ExportGroupValueCount, MaxGroupCount: Cardinal;
      CodeNameSort: string);
    procedure GroupCodeName(const FirstRow: Cardinal);
    procedure Finish;
  public
    constructor Create(aRest: TSQLRest; const aGUID: TGUID);
    destructor Destroy; override;
  published
    property ID: string read fID;
    property Queue: TThreadedQueue<TCodeNameConsumer> read fQueue;
    property Consumers: TCodeNameConsumers read fConsumers;
  end;

  TConsumer = class
  protected
    fID: string;
    fValue: INotifyCallback;
    fAddress: string;
    fTaskList: TThreadList<Word>;
    function TaskCount: Byte;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute(aTask: Word);
    procedure Remove(aTask: Word);
  published
    property ID: string read fID write fID;
    property Value: INotifyCallback read fValue write fValue;
    property Address: string read fAddress write fAddress;
  end;

  TConsumers = class
  protected
    fConsumerFree: TObject;
    fList: TThreadList<TConsumer>;
    fBusyList: TThreadList<TConsumer>;
    fQueue: TThreadedQueue<TConsumer>;
    fFinishCount: Word;
    function FindConsumer(ID: string): TConsumer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Consumer: INotifyCallback; ID, Address: string);
    procedure Delete(Consumer: TConsumer);
    procedure InitConsumers;
    procedure FinishConsumers;
    procedure Free(ID: string);
    procedure TaskFinish(ID: string; FirstRow: Word; CodeName: string);
    procedure TaskFailed(ID: string; FirstRow: Word);
  published
    property ConsumerFree: TObject read fConsumerFree;
    property BusyList: TThreadList<TConsumer> read fBusyList;
    property Queue: TThreadedQueue<TConsumer> read fQueue;
    property FinishCount: Word read fFinishCount;
  end;

  TCompareMode = (cmNone, cmVert, cmSlant, cmVertSlant);
  TCompareModes = set of TCompareMode;

  TExportFile = (efFile, efFile2, efFile3, efFile4, efFile5, efFile6, efFile7,
    efFile8, efFile9, efFile10, efFile11, efFile12, efFile13, efFile14, efFile15);
  TExportFiles = set of TExportFile;

  TSettings = record
  private
    fFileName: string;
    fDataMode: Byte;
    fIntervalValues: TWordDynArray;
    fFirstIntervalValue: Word;
    fTotalIntervalValue: Word;
    fCompareMode: TCompareMode;
    fCompareModeString: string;
    fCompareCrossRange: Boolean;
    fCompareSpacing: Word;
    fVertCompareSpacing: Word;
    fVertSameValueCount: Word;
    fVertSameValueCount2: Word;
    fSlantCompareSpacing: Word;
    fSlantSameValueCount: Word;
    fSlantSameValueCount2: Word;
    fGroupValueCount: Byte;
    fExportGroupValueCount: Byte;
    fMaxGroupCount: Cardinal;
    fCodeNameSort: Int64;
    fExportFiles: TExportFiles;
    fExportLite: Boolean;
    procedure SetCompareMode(Value: TCompareMode);
    procedure SetIntervalValues(Value: TWordDynArray);
    procedure SetVertCompareSpacing(Value: Word);
    procedure SetSlantCompareSpacing(Value: Word);
    procedure BuildCompareSpacing;
  public
    property FileName: string read fFileName write fFileName;
    property DataMode: Byte read fDataMode write fDataMode;
    property IntervalValues: TWordDynArray read fIntervalValues write SetIntervalValues;
    property FirstIntervalValue: Word read fFirstIntervalValue;
    property TotalIntervalValue: Word read fTotalIntervalValue;
    property CompareMode: TCompareMode read fCompareMode write SetCompareMode;
    property CompareModeString: string read fCompareModeString;
    property CompareCrossRange: Boolean read fCompareCrossRange write fCompareCrossRange;
    property CompareSpacing: Word read fCompareSpacing;
    property VertCompareSpacing: Word read fVertCompareSpacing write SetVertCompareSpacing;
    property VertSameValueCount: Word read fVertSameValueCount write fVertSameValueCount;
    property VertSameValueCount2: Word read fVertSameValueCount2 write fVertSameValueCount2;
    property SlantCompareSpacing: Word read fSlantCompareSpacing write SetSlantCompareSpacing;
    property SlantSameValueCount: Word read fSlantSameValueCount write fSlantSameValueCount;
    property SlantSameValueCount2: Word read fSlantSameValueCount2 write fSlantSameValueCount2;
    property GroupValueCount: Byte read fGroupValueCount write fGroupValueCount;
    property ExportGroupValueCount: Byte read fExportGroupValueCount write fExportGroupValueCount;
    property MaxGroupCount: Cardinal read fMaxGroupCount write fMaxGroupCount;
    property CodeNameSort: Int64 read fCodeNameSort write fCodeNameSort;
    property ExportFiles: TExportFiles read fExportFiles write fExportFiles;
    property ExportLite: Boolean read fExportLite write fExportLite;
  end;

  TOnGroupCodeName = procedure(FirstRow: Word) of object;

  TDataComputer = class(TThread)
  private
    fStopwatch: TStopwatch;
    fLock: TObject;
    fQueue: TThreadedQueue<Word>;
    fOnGroupCodeName: TOnGroupCodeName;
    fOnFinish: TNotifyEvent;

    fExportDirectory: string;
    fExportDirectory2: string;

    fRowCount: Word;
    fActiveFirstRow: Word;
    fTipStr: string;

    procedure LoadRow;
    function VertCompareRow(r, r2: TSQLRow): Boolean;
    function SlantCompareRow(r, r2: TSQLRow; CompareData: TSQLCompareData; Offset: Integer): Boolean;
    procedure CompareRow;
    procedure GroupCodeName;
    procedure UpdateFirstRow;
    procedure BuildCodeName;
    function RebuildFile(RowCount: Cardinal): Boolean;
    function NumberToString(Value: Cardinal): string;
    function RebuildFileName(Sender: TFileWriter): string;
    function RebuildFileName2(Sender: TFileWriter): string; overload;
    function RebuildFileName2(Sender: TFileWriter; aRowNumber: Cardinal): string; overload;
    procedure SaveCompareData(fr: TFileWriter; Data: TSQLCompareData; SaveValues: Boolean = True);
    procedure SaveFirstRow;
    procedure SaveCodeNameSortByRowcount;
    procedure SaveSlantCodeName;
    procedure SaveCodeNameSortByValueCount(FileName: string);
    procedure SaveVertSlantCompareDataSortByValueCount2;
    procedure ExportData;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
  published
    property Stopwatch: TStopwatch read fStopwatch;
    property Lock: TObject read fLock;
    property Queue: TThreadedQueue<Word> read fQueue;
    property OnGroupCodeName: TOnGroupCodeName read fOnGroupCodeName write fOnGroupCodeName;
    property OnFinish: TNotifyEvent read fOnFinish write fOnFinish;
    property RowCount: Word read fRowCount;
    property ActiveFirstRow: Word read fActiveFirstRow;
  end;

const
  EachFileRowCount: Cardinal = 10000;
  EachFileRowNumber: Cardinal = 10000;
  EachPageRowCount: Word = 10000;

var
  fMainApp: Boolean;
  fLocalIP: string;
  fProcessorCount: Byte;

  fSettings: TSettings;
  fDataComputer: TDataComputer;

  fDatabase: TSQLRestServerDB;
  //fDatabase2: TSQLRestServerDB;
  fKeyValue: TSQLKeyValue;
  fServer: TSQLHttpServer;

  fClient: TSQLHttpClientWebsockets;
  fService: IService;
  fNotifyCallback: INotifyCallback;

  fConsumers: TConsumers;

procedure ReadSettings;
procedure WriteSettings;
procedure ConnectServer(Address, Port: string);
procedure DisconnectServer;

implementation

procedure TSQLFirstRow.AddCodeName(v: Word);
var
  i, i2, i3: Integer;
begin
  i := (v - 1) div 64;
  v := v mod 64;

  i2 := High(fCodeName);
  if i2 < i then
  begin
    SetLength(fCodeName, i + 1);
    for i3 := i2 + 1 to i do fCodeName[i3] := 0;
  end;

  fCodeName[i].AddValue(v);
  fCodeNameValueCount := fCodeNameValueCount + 1;
end;

function TSQLFirstRow.CodeNameExist(aCodeName: TWordDynArray): Boolean;
var
  v, v2: Word;
  i, iCount: Integer;
begin
  iCount := 0;
  for v in aCodeName do
  begin
    i := (v - 1) div 64;
    v2 := v mod 64;
    if (i < Low(fCodeName)) or (i > High(fCodeName)) then Break;
    if fCodeName[i].ValueExist(v2) then Inc(iCount);
  end;
  Result := iCount = Length(aCodeName);
end;

procedure TSQLFirstRow.ClearCodeName;
begin
  SetLength(fCodeName, 0);
  fCodeNameValueCount := 0;
end;

function TSQLCodeName.SlantValue: string;
begin
  Result := SlantValue(fValue);
end;

class function TSQLCodeName.SlantValue(v: string): string;
var
  s: string;
  i: Integer;
begin
  Result := '';
  for s in v.Split(['��']) do
  begin
    if not Result.IsEmpty then Result := Result + '��';
    i := s.ToInteger;
    Result := Result + ((i + 1) div 2).ToString;
    if i mod 2 = 0 then Result := Result + 'Z'
    else Result := Result + 'Y';
  end;
end;

procedure TService.RegisterConsumer(const Consumer: INotifyCallback; const ID, Address: string);
begin
  fConsumers.Add(Consumer, ID, Address);
end;

procedure TService.InitFinish(const ID: string; const ProcessorCount: Cardinal);
var
  i: Integer;
begin
  for i := 1 to ProcessorCount do fConsumers.Free(ID);
end;

procedure TService.TaskFinish(const ID: string; const FirstRow: Cardinal; const CodeName: string);
begin
  fConsumers.TaskFinish(ID, FirstRow, CodeName);
end;

procedure TService.TaskFailed(const ID: string; const FirstRow: Cardinal);
begin
  fConsumers.TaskFailed(ID, FirstRow);
end;

procedure TService.CallbackReleased(const Callback: IInvokable; const InterfaceName: RawUTF8);
begin
  //if InterfaceName = 'INotifyCallback' then
  //  InterfaceArrayDelete(fConsumers, Callback);
end;

constructor TCodeNameConsumer.Create;
var
  FileName: string;
begin
  inherited Create(True);
  fFirstRow := TSQLFirstRow.Create;
  fCodeNameCount := TDictionary<Word, Word>.Create;
  fQueue := TThreadedQueue<Word>.Create;
  FileName := fLogDirectory + ThreadID.ToString;
  fr := TFileWriter.Create(FileName);
end;

destructor TCodeNameConsumer.Destroy;
begin
  fQueue.Free;
  fCodeNameCount.Free;
  fFirstRow.Free;
  fr.Free;
  inherited Destroy;
end;

function TCodeNameConsumer.GetCodeNameRows(CodeName: TWordDynArray): TWordDynArray;
begin
  SetLength(Result, 0);
  fFirstRow.FillRewind;
  while fFirstRow.FillOne do
  begin
    if Terminated then Exit;
    if fFirstRow.CodeNameExist(CodeName) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := fFirstRow.FillCurrentRow - 1;
    end;
  end;
end;

function TCodeNameConsumer.GetCodeNameRowCount(CodeName: TWordDynArray): Word;
begin
  Result := 0;
  fFirstRow.FillRewind;
  while fFirstRow.FillOne do
  begin
    if Terminated then Exit;
    if fFirstRow.CodeNameExist(CodeName) then Result := Result + 1;
  end;
end;

function TCodeNameConsumer.GetCodeNameRowCount(CodeName: TWordDynArray; Rows: TWordDynArray): Word;
var
  Row: Word;
begin
  Result := 0;
  for Row in Rows do
  begin
    if Terminated then Exit;
    fFirstRow.FillRow(Row);
    if fFirstRow.CodeNameExist(CodeName) then Result := Result + 1;
  end;
end;

procedure TCodeNameConsumer.FindCodeNameWithMostRowCount(CodeNames: TWordDynArray);
var
  GroupCount: Int64;
  Rows, CodeName: TWordDynArray;
  i: Integer;
  s: string;
begin
  s := Format('���ڲ��Ҵ��ţ�%s', [CodeNames.ToString]);
  fr.WriteLn(s);

  GroupCount := 0;
  for i := fExportGroupValueCount to fGroupValueCount do
  begin
    SetLength(CodeName, i);
    TCombinatorialAlgorithm.For(Length(CodeNames), i,
    procedure(FirstNumber: Cardinal; CodeNameIndexs: TCardinalDynArray)
    var
      //CodeName: TWordDynArray;
      i, RowCount: Integer;
      s: string;
    begin
      try
        if (Length(CodeNameIndexs) > 1) and (CodeNameIndexs[High(CodeNameIndexs)] - CodeNameIndexs[High(CodeNameIndexs) - 1] = 1) then
        begin
          for i := Low(CodeNameIndexs) to High(CodeNameIndexs) do
            CodeName[i] := CodeNames[CodeNameIndexs[i] - 1];
          Rows := GetCodeNameRows(CodeName);
        end;

        if Length(fActiveCodeName) <> Length(CodeNameIndexs) then
          SetLength(fActiveCodeName, Length(CodeNameIndexs));
        for i := Low(fActiveCodeName) to High(fActiveCodeName) do
          fActiveCodeName[i] := CodeNames[CodeNameIndexs[i] - 1];
        RowCount := GetCodeNameRowCount(fActiveCodeName, Rows);
        if RowCount > fMaxRowCount then
        begin
          fMaxRowCount := RowCount;

          if Length(fCodeName) <> Length(fActiveCodeName) then SetLength(fCodeName, Length(fActiveCodeName));
          for i := Low(fCodeName) to High(fCodeName) do
            fCodeName[i] := fActiveCodeName[i];
        end;

        Inc(GroupCount);
      except
        on e: Exception do
        begin
          s := Format('���ţ�%s' + #$D#$A + '%s', [fActiveCodeName.ToString, e.Message]);
          fr.WriteLn(s);
        end;
      end;
    end,
    //�Ƶ�����
    {procedure(FirstNumber: Cardinal; CodeNameIndexs: TCardinalDynArray)
    var
      CodeName: TWordDynArray;
      i: Integer;
    begin
      SetLength(CodeName, Length(CodeNameIndexs));

      for i := Low(CodeNameIndexs) to High(CodeNameIndexs) do
        CodeName[i] := CodeNames[CodeNameIndexs[i] - 1];

      Rows := GetCodeNameRows(CodeName);
    end,}
    function(FirstNumber, GroupCount: Cardinal): Boolean
    begin
      Result := Terminated or ((fMaxGroupCount > 0) and (GroupCount >= fMaxGroupCount));
    end);
  end;
end;

procedure TCodeNameConsumer.Execute;
var
  CodeNames, CodeNames2, CodeNames3, CodeNames4, CodeNames5, CodeNames6,
  CodeNames7, CodeNames8: TWordDynArray;
  i, j: Integer;
  s: string;
begin
  repeat
    fActiveFirstRow := 0;
    fActiveFirstRow := fQueue.PopItem;
    try
      if fActiveFirstRow > 0 then
      begin
        try
          s := Format('���ڼ������У�%d', [fActiveFirstRow]);
          fr.WriteLn(s);

          SetLength(CodeNames, 0);
          fFirstRow.FillRewind;
          while fFirstRow.FillOne do
          begin
            if firstRow.Value = fActiveFirstRow then
            begin
              //��������������
              CodeNames := fFirstRow.CodeName.ToWordDynArray;

              Break;
            end;
          end;
          //ɾ���������в����ڴ���
          for i := High(CodeNames) downto Low(CodeNames) do
            if GetCodeNameRowCount([CodeNames[i]]) < 2 then
            begin
              for j := i + 1 to High(CodeNames) do
                CodeNames[j - 1] := CodeNames[j];
              SetLength(CodeNames, High(CodeNames));
            end;
          //�����ŵ�������
          SetLength(CodeNames2, Length(CodeNames));
          for i := Low(CodeNames2) to High(CodeNames2) do
            CodeNames2[i] := CodeNames[High(CodeNames2) - i];
          {//�������������ϲ�
          SetLength(CodeNames3, Length(CodeNames));
          for i := Low(CodeNames3) to High(CodeNames3) do
            if i mod 2 = 0 then CodeNames3[i] := CodeNames[i div 2] else CodeNames3[i] := CodeNames2[i div 2];}
          //��������ż���
          SetLength(CodeNames3, Length(CodeNames));
          for i := Low(CodeNames3) to High(CodeNames3) do
            CodeNames3[i] := CodeNames[i];
          TSortAlgorithm.OddEvenAlternate(CodeNames3, False);
          //����˳��
          SetLength(CodeNames4, Length(CodeNames));
          for i := Low(CodeNames4) to High(CodeNames4) do
            CodeNames4[i] := CodeNames[i];
          TSortAlgorithm.Stochastic(CodeNames4);
          //�����Ŵ�����������
          SetLength(CodeNames5, Length(CodeNames));
          for i := Low(CodeNames5) to High(CodeNames5) do
            CodeNames5[i] := CodeNames[i];
          TSortAlgorithm.Shell(CodeNames5, function(x, x2: Word): Boolean
          begin
            x := fCodeNameCount.Items[x];
            x2 := fCodeNameCount.Items[x2];
            Result := x > x2;
          end);
          //�����Ŵ�����������
          SetLength(CodeNames6, Length(CodeNames));
          for i := Low(CodeNames6) to High(CodeNames6) do
            CodeNames6[i] := CodeNames5[High(CodeNames6) - i];
          //���Ŵ������������ϲ�
          SetLength(CodeNames7, Length(CodeNames));
          for i := Low(CodeNames7) to High(CodeNames7) do
            if i mod 2 = 0 then CodeNames7[i] := CodeNames6[i div 2] else CodeNames7[i] := CodeNames5[i div 2];
          //�����Ŵ���ż�����
          SetLength(CodeNames8, Length(CodeNames));
          for i := Low(CodeNames8) to High(CodeNames8) do
            CodeNames8[i] := CodeNames5[i];
          TSortAlgorithm.OddEvenAlternate(CodeNames8, True,
          function(CodeName: Word): Boolean
          begin
            Result := fCodeNameCount.Items[CodeName] mod 2 = 0;
          end);

          fMaxRowCount := 0;

          if fCodeNameSort.ValueExist(1) then
            FindCodeNameWithMostRowCount(CodeNames5);
          if Terminated then Exit;

          if fCodeNameSort.ValueExist(2) then
            FindCodeNameWithMostRowCount(CodeNames6);
          if Terminated then Exit;

          if fCodeNameSort.ValueExist(3) then
            FindCodeNameWithMostRowCount(CodeNames7);
          if Terminated then Exit;

          if fCodeNameSort.ValueExist(4) then
            FindCodeNameWithMostRowCount(CodeNames4);
          if Terminated then Exit;

          if fCodeNameSort.ValueExist(5) then
            FindCodeNameWithMostRowCount(CodeNames8);
          if Terminated then Exit;

          if fCodeNameSort.ValueExist(6) then
            FindCodeNameWithMostRowCount(CodeNames2);
          if Terminated then Exit;

          if fCodeNameSort.ValueExist(7) then
            FindCodeNameWithMostRowCount(CodeNames);
          if Terminated then Exit;

          if fCodeNameSort.ValueExist(8) then
            FindCodeNameWithMostRowCount(CodeNames3);
          if Terminated then Exit;

          TSortAlgorithm.Shell(fCodeName, function(x, x2: Word): Boolean
          begin
            Result := x > x2;
          end);
          fService.TaskFinish(TNotifyCallback(fNotifyCallback).ID, fActiveFirstRow, fCodeName.ToString);
        except
          on e: Exception do
          begin
            fr.WriteLn(e.Message);
            fService.TaskFailed(TNotifyCallback(fNotifyCallback).ID, fActiveFirstRow);
          end
        end;
      end;
    finally
      if not Terminated then
      begin
        fActiveFirstRow := 0;
        SetLength(fActiveCodeName, 0);

        TNotifyCallback(fNotifyCallback).Queue.PushItem(Self);
      end;
    end;
  until Terminated;
end;

procedure TCodeNameConsumer.BuildCodeNameCount;
var
  v: Word;
begin
  fCodeNameCount.Clear;
  fFirstRow.FillRewind;
  while fFirstRow.FillOne do
  begin
    if Terminated then Exit;
    for v in fFirstRow.CodeName.ToWordDynArray do
    begin
      if fCodeNameCount.ContainsKey(v) then
        fCodeNameCount.Items[v] := fCodeNameCount.Items[v] + 1
      else
        fCodeNameCount.Add(v, 1);
    end;
  end;
end;

constructor TNotifyCallback.Create(aRest: TSQLRest; const aGUID: TGUID);
var
  i: Integer;
begin
  inherited Create(aRest, aGUID);
  fID := TGUID.NewGuid.ToString;
   SetLength(fConsumers, fProcessorCount);
  for i := Low(fConsumers) to High(fConsumers) do
    fConsumers[i] := TCodeNameConsumer.Create;
  for i := Low(fConsumers) to High(fConsumers) do
    fConsumers[i].Start;
  fQueue := TThreadedQueue<TCodeNameConsumer>.Create(Length(fConsumers));
  for i := Low(fConsumers) to High(fConsumers) do fQueue.PushItem(fConsumers[i]);
end;

destructor TNotifyCallback.Destroy;
var
  i: Integer;
begin
  for i := Low(fConsumers) + 1 to High(fConsumers) do
  begin
    fConsumers[i].Terminate;
    fConsumers[i].Queue.DoShutDown;
  end;
  for i := Low(fConsumers) + 1 to High(fConsumers) do
  begin
    fConsumers[i].WaitFor;
    fConsumers[i].Free;
  end;
  fQueue.Free;
  inherited Destroy;
end;

procedure TNotifyCallback.Init(const GroupValueCount, ExportGroupValueCount,
  MaxGroupCount: Cardinal; CodeNameSort: string);
begin
  TThread.CreateAnonymousThread(procedure
  var
    i: Integer;
  begin
    for i := Low(fConsumers) to High(fConsumers) do
    begin
      fConsumers[i].GroupValueCount := GroupValueCount;
      fConsumers[i].ExportGroupValueCount := ExportGroupValueCount;
      fConsumers[i].MaxGroupCount := MaxGroupCount;
      fConsumers[i].CodeNameSort := CodeNameSort.ToInt64;
      fConsumers[i].FirstRow.FillPrepare(fClient, '', []);
      fConsumers[i].BuildCodeNameCount;
    end;
    fService.InitFinish(fID, fProcessorCount);
  end).Start;
end;

procedure TNotifyCallback.GroupCodeName(const FirstRow: Cardinal);
var
  Consumer: TCodeNameConsumer;
begin
  Consumer := fQueue.PopItem;
  Consumer.Queue.PushItem(FirstRow);
end;

procedure TNotifyCallback.Finish;
begin
  DisconnectServer;
end;

constructor TConsumer.Create;
begin
  inherited Create;
  fTaskList := TThreadList<Word>.Create;
end;

destructor TConsumer.Destroy;
begin
  fTaskList.Free;
  inherited Destroy;
end;

function TConsumer.TaskCount: Byte;
begin
  with fTaskList.LockList do
  begin
    try
      Result := Count;
    finally
      fTaskList.UnlockList;
    end;
  end;
end;

procedure TConsumer.Execute(aTask: Word);
var
  i: Integer;
begin
  fValue.GroupCodeName(aTask);
  fTaskList.Add(aTask);
  with fConsumers.BusyList.LockList do
  begin
    try
      i := IndexOf(Self);
      if i = -1 then Add(Self);
    finally
      fConsumers.BusyList.UnlockList;
    end;
  end;
end;

procedure TConsumer.Remove(aTask: Word);
begin
  fTaskList.Remove(aTask);
  if TaskCount = 0 then
    fConsumers.BusyList.Remove(Self);
end;

constructor TConsumers.Create;
begin
  inherited Create;
  fConsumerFree := TObject.Create;
  fList := TThreadList<TConsumer>.Create;
  fBusyList := TThreadList<TConsumer>.Create;
  fQueue := TThreadedQueue<TConsumer>.Create(0);
end;

destructor TConsumers.Destroy;
var
  i: Integer;
begin
  fQueue.Free;
  fBusyList.Free;
  with fList.LockList do
  begin
    try
      for i := 0 to Count - 1 do Items[i].Free;
    finally
      fList.UnLockList;
    end;
  end;
  fList.Free;
  fConsumerFree.Free;
  inherited Destroy;
end;

function TConsumers.FindConsumer(ID: String): TConsumer;
var
  i: Integer;
begin
  Result := nil;
  with fList.LockList do
  begin
    try
      for i := 0 to Count - 1 do
        if Items[i].ID = ID then
        begin
          Result := Items[i];
          Break;
        end;
    finally
      fList.UnLockList;
    end;
  end;
end;

procedure TConsumers.Add(Consumer: INotifyCallback; ID, Address: string);
var
  c: TConsumer;
begin
  c := TConsumer.Create;
  c.ID := ID;
  c.Value := Consumer;
  c.Address := Address;
  fList.Add(c);
end;

procedure TConsumers.Delete(Consumer: TConsumer);
begin
  with fList.LockList do
  begin
    try
      Remove(Consumer);
      if Count = 0 then fQueue.DoShutDown;
    finally
      fList.UnLockList;
    end;
  end;
  Consumer.Free;
end;

procedure TConsumers.InitConsumers;
var
  i: Integer;
begin
  fFinishCount := 0;
  with fList.LockList do
  begin
    try
      if Assigned(fQueue) then FreeAndNil(fQueue);
      fQueue := TThreadedQueue<TConsumer>.Create(Count);

      for i := Count - 1 downto 0 do
      begin
        try
          Items[i].Value.Init(
            fSettings.GroupValueCount,
            fSettings.ExportGroupValueCount,
            fSettings.MaxGroupCount,
            IntToStr(fSettings.CodeNameSort)
          );
        except
          Delete(i);
        end;
      end;
    finally
      fList.UnLockList;
    end;
  end;
end;

procedure TConsumers.FinishConsumers;
var
  i: Integer;
begin
  with fList.LockList do
  begin
    try
      for i := Count - 1 downto 0 do
      begin
        try
          Items[i].Value.Finish;
        except
          Delete(i);
        end;
      end;
    finally
      fList.UnLockList;
    end;
  end;
end;

procedure TConsumers.Free(ID: string);
var
  Consumer: TConsumer;
begin
  Consumer := FindConsumer(ID);
  if not Assigned(Consumer) then Exit;
  fQueue.Grow(1);
  fQueue.PushItem(Consumer);
end;

procedure TConsumers.TaskFinish(ID: string; FirstRow: Word; CodeName: string);
var
  Consumer: TConsumer;
  CompareData: TSQLCompareData;
begin
  Consumer := FindConsumer(ID);
  if not Assigned(Consumer) then Exit;
  Consumer.Remove(FirstRow);
  fQueue.PushItem(Consumer);

  TSQLCompareData.AutoFree(CompareData);
  CompareData.FirstRow := FirstRow;
  CompareData.CodeName := CodeName;
  CompareData.CodeNameValueCount := Length(CodeName.Split(['��']));
  fDatabase.Add(CompareData, True);
  fDatabase.Execute(Format('UPDATE FirstRow SET Grouped = 1 WHERE Value = %d', [FirstRow]));

  with fBusyList.LockList do
  begin
    try
      if Count = 0 then
        TMonitor.PulseAll(fConsumers.ConsumerFree);
    finally
      fBusyList.UnLockList;
    end;
  end;

  fFinishCount := fFinishCount + 1;
end;

procedure TConsumers.TaskFailed(ID: string; FirstRow: Word);
var
  Consumer: TConsumer;
begin
  Consumer := FindConsumer(ID);
  if not Assigned(Consumer) then Exit;
  fDataComputer.Queue.PushItem(FirstRow);
  Consumer.Remove(FirstRow);
  fQueue.PushItem(Consumer);
  TMonitor.PulseAll(fConsumers.ConsumerFree);
end;

procedure TSettings.SetCompareMode(Value: TCompareMode);
begin
  fCompareMode := Value;
  case fCompareMode of
    cmVert: fCompareModeString := 'ֱ';
    cmSlant: fCompareModeString := 'б';
    cmVertSlant: fCompareModeString := 'ֱ��б';
  end;
end;

procedure TSettings.SetIntervalValues(Value: TWordDynArray);
var
  i: Integer;
begin
  fIntervalValues := Value;
  fFirstIntervalValue := 0;
  if Length(fIntervalValues) > 1 then fFirstIntervalValue := fIntervalValues[0];
  fTotalIntervalValue := 0;
  for i := Low(fIntervalValues) to High(fIntervalValues) do
    fTotalIntervalValue := fTotalIntervalValue + fIntervalValues[i];
end;

procedure TSettings.SetVertCompareSpacing(Value: Word);
begin
  fVertCompareSpacing := Value;
  BuildCompareSpacing;
end;

procedure TSettings.SetSlantCompareSpacing(Value: Word);
begin
  fSlantCompareSpacing := Value;
  BuildCompareSpacing;
end;

procedure TSettings.BuildCompareSpacing;
begin
  fCompareSpacing := fVertCompareSpacing;
  if fSlantCompareSpacing > fCompareSpacing then
    fCompareSpacing := fSlantCompareSpacing;
end;

constructor TDataComputer.Create;
begin
  inherited Create(True);
  fLock := TObject.Create;
end;

destructor TDataComputer.Destroy;
begin
  fLock.Free;
  inherited Destroy;
end;

procedure TDataComputer.LoadRow;
var
  i, Digit: Integer;
  s: string;
  Row: TSQLRow;
begin
  fRowCount := 0;
  s := fDatabase.OneFieldValue(TSQLRow, 'Max(Number)', '', []);
  if not s.IsEmpty then fRowCount := s.ToInteger;

  if not TFile.Exists(fSettings.FileName) then Exit;

  TSQLRow.AutoFree(Row);
  with TStringList.Create do
  begin
    try
      LoadFromFile(fSettings.FileName);

      //fDatabase.Delete(TSQLRow, '');
      fDatabase.TransactionBegin(TSQLRow);
      try
        for i := Count - 1 downto 0 do
        begin
          if not (TryStrToInt(Names[i].Trim, Digit) and (Digit > fRowCount)) then Continue;
          Row.Number := Digit;
          s := ValueFromIndex[i];
          Row.AssignValue(s, fSettings.IntervalValues, fSettings.DataMode);
          fDatabase.Add(Row, True);
          fRowCount := Digit;
        end;
        fDatabase.Commit(1, True);
      except
        on e: Exception do
        begin
          fDatabase.RollBack;
          raise Exception.Create(e.Message);
        end;
      end;
    finally
      Free;
    end;
  end;
end;

function TDataComputer.VertCompareRow(r, r2: TSQLRow): Boolean;
var
  v, TotalSameValueCount, TotalDifferentValueCount: Word;
begin
  TotalSameValueCount := 0;
  TotalDifferentValueCount := 0;
  for v in r2.Values do
    if r.ValueExist(v) then TotalSameValueCount := TotalSameValueCount + 1
    else TotalDifferentValueCount := TotalDifferentValueCount + 1;
  Result := (TotalSameValueCount >= fSettings.VertSameValueCount)
    and (TotalSameValueCount <= fSettings.VertSameValueCount2);
end;

function TDataComputer.SlantCompareRow(r, r2: TSQLRow; CompareData: TSQLCompareData; Offset: Integer): Boolean;
var
  v, v2, TotalSameValueCount, TotalDifferentValueCount: Word;
begin
  TotalSameValueCount := 0;
  TotalDifferentValueCount := 0;
  CompareData.ClearValue;
  for v2 in r2.Values do
  begin
    v := v2 + Offset;
    if (v < 1) or (v > fSettings.TotalIntervalValue) then Continue;
    if not fSettings.CompareCrossRange
      and (((Offset > 0) and (v > fSettings.FirstIntervalValue))
      or ((Offset < 0) and (v <= fSettings.FirstIntervalValue)))
    then Continue;

    CompareData.AddValue(v);
    if r.ValueExist(v) then TotalSameValueCount := TotalSameValueCount + 1
    else TotalDifferentValueCount := TotalDifferentValueCount + 1;
  end;
  Result := CompareData.HasValue
    and (TotalSameValueCount >= fSettings.SlantSameValueCount)
    and (TotalSameValueCount <= fSettings.SlantSameValueCount2);
end;

procedure TDataComputer.CompareRow;
var
  Row, Row2: TSQLRow;
  CompareData: TSQLCompareData;
  FirstRow: TSQLFirstRow;
  i: Integer;
  Number, LastFirtRow: Word;
  s: string;
begin
  LastFirtRow := 0;
  s := fDatabase.OneFieldValue(TSQLFirstRow, 'Max(Value)', '', []);
  if not s.IsEmpty then LastFirtRow := s.ToInteger;
  //fDatabase.Delete(TSQLFirstRow, '');
  //fDatabase.Delete(TSQLCompareData, '');

  TSQLFirstRow.AutoFree(FirstRow);
  TSQLRow.AutoFree(Row, fDatabase, 'Number > ? ORDER BY Number', [LastFirtRow - fSettings.CompareSpacing]);
  TSQLRow.AutoFree(Row2);
  TSQLCompareData.AutoFree(CompareData);

  fDatabase.TransactionBegin(TSQLCompareData);
  try
    while Row.FillOne and not Terminated do
    begin
      if Row.FillCurrentRow - 1 <= fSettings.CompareSpacing then Continue;

      FirstRow.Value := Row.Number;
      FirstRow.RowCount := 0;
      FirstRow.ClearCodeName;
      FirstRow.Grouped := False;
      CompareData.FirstRow := Row.Number;
      CompareData.CodeNameValueCount := 1;
      for i := Row.FillCurrentRow - 2 downto Row.FillCurrentRow - 1 - fSettings.CompareSpacing do
      begin
        Row.FillRow(i, Row2);
        Number := Row.FillCurrentRow - 1 - i;
        //ֱ��
        if Number <= fSettings.VertCompareSpacing then
        begin
          if VertCompareRow(Row, Row2)
          then
          begin
            if fSettings.CompareMode = cmVert then CompareData.CodeName := Number.ToString
            else CompareData.CodeName := (Number * 3 - 2).ToString;
            {CompareData.AssignValue(Row2);
            CompareData.CalcValueCount(fSettings.IntervalValues);
            fDatabase.Add(CompareData, True);}

            FirstRow.RowCount := FirstRow.RowCount + 1;
            FirstRow.AddCodeName(StrToInt(CompareData.CodeName));
          end;
        end;
        //б��
        if Number <= fSettings.SlantCompareSpacing then
        begin
          //��б��
          if SlantCompareRow(Row, Row2, CompareData, Number)
          then
          begin
            if fSettings.CompareMode = cmSlant then CompareData.CodeName := (Number * 2 - 1).ToString
            else CompareData.CodeName := (Number * 3 - 1).ToString;
            {CompareData.CalcValueCount(fSettings.IntervalValues);
            fDatabase.Add(CompareData, True);}

            FirstRow.RowCount := FirstRow.RowCount + 1;
            FirstRow.AddCodeName(StrToInt(CompareData.CodeName));
          end;
          //��б��
          if SlantCompareRow(Row, Row2, CompareData, -Number)
          then
          begin
            if fSettings.CompareMode = cmSlant then CompareData.CodeName := (Number * 2).ToString
            else CompareData.CodeName := (Number * 3).ToString;
            {CompareData.CalcValueCount(fSettings.IntervalValues);
            fDatabase.Add(CompareData, True);}

            FirstRow.RowCount := FirstRow.RowCount + 1;
            FirstRow.AddCodeName(StrToInt(CompareData.CodeName));
          end;
        end;
      end;
      if FirstRow.CodeNameValueCount >= fSettings.ExportGroupValueCount then
        fDatabase.Add(FirstRow, True);
    end;
    fDatabase.Commit(1, True);
  except
    on e: Exception do
    begin
      fDatabase.RollBack;
      raise Exception.Create(e.Message);
    end;
  end;
end;

procedure TDataComputer.GroupCodeName;
var
  FirstRow: TSQLFirstRow;
  s: string;
  Task: Word;
  Consumer: TConsumer;
begin
  {TSQLFirstRow.AutoFree(FirstRow, fDatabase, 'CodeNameValueCount < ?', [fSettings.ExportGroupValueCount]);
  while FirstRow.FillOne do
  begin
    fDatabase.Delete(TSQLFirstRow, FirstRow.ID);
    fDatabase.Delete(TSQLCompareData, 'FirstRow = ?', [FirstRow.Value]);
  end;}

  TSQLFirstRow.AutoFree(FirstRow, fDatabase, 'Grouped = 0', []);
  if FirstRow.FillTable.RowCount = 0 then Exit;
  fConsumers.InitConsumers;

  fQueue := TThreadedQueue<Word>.Create(FirstRow.FillTable.RowCount);
  TMonitor.Enter(fConsumers.ConsumerFree);
  try
    while FirstRow.FillOne do fQueue.PushItem(FirstRow.Value);
    repeat
      Task := 0;
      Task := fQueue.PopItem;
      if Task > 0 then
      begin
        fActiveFirstRow := Task;
        try
          Consumer := nil;
          Consumer := fConsumers.Queue.PopItem;
          if Assigned(Consumer) then
          begin
            try
              Consumer.Execute(Task);
            except
              fConsumers.Delete(Consumer);
              raise;
            end;
          end;
        except
          fQueue.PushItem(Task);
        end;

        if Assigned(fOnGroupCodeName) then fOnGroupCodeName(Task);
      end;
      if fQueue.QueueSize = 0 then
        TMonitor.Wait(fConsumers.ConsumerFree, Cardinal.MaxValue);
    until Terminated or (fQueue.QueueSize = 0);
  finally
    TMonitor.Exit(fConsumers.ConsumerFree);
    if Assigned(fQueue) then FreeAndNil(fQueue);
  end;
end;

procedure TDataComputer.UpdateFirstRow;
var
  FirstRow: TSQLFirstRow;
  LastFirstRow: Word;
begin
  TSQLFirstRow.AutoFree(FirstRow, fDatabase, 'ORDER BY Value', []);
  fDatabase.TransactionBegin(TSQLFirstRow);
  try
    LastFirstRow := fSettings.CompareSpacing + 1;
    while FirstRow.FillOne do
    begin
      FirstRow.RowSpacing := FirstRow.Value - LastFirstRow;
      fDatabase.Update(FirstRow);

      LastFirstRow := FirstRow.Value;
    end;
    fDatabase.Commit(1, True);
  except
    fDatabase.RollBack;
    raise;
  end;
end;

procedure TDataComputer.BuildCodeName;
var
  CodeName: TSQLCodeName;
  DataList: TSQLTableJSON;
  s: string;
begin
  TSQLCodeName.AutoFree(CodeName);
  DataList := fDatabase.MultiFieldValues(TSQLCompareData,
    'CodeName, Max(CodeNameValueCount) ValueCount, Count(ID) RowCount',
    'CodeNameValueCount >= ? GROUP BY CodeName',
    [fSettings.ExportGroupValueCount]
  );
  try
    fDatabase.TransactionBegin(TSQLCodeName);
    try
      while DataList.Step do
      begin
        s := DataList.FieldAsRawUTF8('CodeName');
        CodeName.FillPrepare(fDatabase, 'Value = ?', [s]);
        if CodeName.FillOne then
        begin
           CodeName.ValueCount := DataList.FieldAsInteger('ValueCount');
           CodeName.RowCount := DataList.FieldAsInteger('RowCount');
           fDatabase.Update(CodeName);
        end
        else
        begin
          CodeName.Value := s;
          CodeName.ValueCount := DataList.FieldAsInteger('ValueCount');
          CodeName.RowCount := DataList.FieldAsInteger('RowCount');
          fDatabase.Add(CodeName, True);
        end;
      end;
      fDatabase.Commit(1, True);
    except
      fDatabase.RollBack;
      raise;
    end;
  finally
    DataList.Free;
  end;
end;

function TDataComputer.RebuildFile(RowCount: Cardinal): Boolean;
begin
  Result := RowCount >= EachFileRowCount;
end;

function TDataComputer.NumberToString(Value: Cardinal): string;
var
  i: Integer;
begin
  Result := '';
  i := Value div 100000000;
  Value := Value mod 100000000;
  if i > 0 then Result := Result + Format(' %d ��', [i]);
  i:= Value div 10000;
  Value := Value mod 10000;
  if i > 0 then Result := Result + Format(' %d ��', [i]);
  if Value > 0 then
    Result := Result + Format(' %d', [Value]);
end;

function TDataComputer.RebuildFileName(Sender: TFileWriter): string;
var
  sSub: string;
  RowCount: Cardinal;
  i: Integer;
begin
  RowCount := Sender.FileNo * EachFileRowCount;
  if Sender.LastFileNo then
    RowCount := (Sender.FileNo - 1) * EachFileRowCount + Sender.RowCount;
  sSub := NumberToString(RowCount);

  i := Sender.FileName.IndexOf('.');
  Result := Sender.FileName.SubString(0, i);
  Result := Result + Format('.����ĩ��%s���հ׼����֣��С�- %d', [sSub, Sender.FileNo - 1]);
end;

function TDataComputer.RebuildFileName2(Sender: TFileWriter): string;
begin
  Result := RebuildFileName2(Sender, 0);
end;

function TDataComputer.RebuildFileName2(Sender: TFileWriter; aRowNumber: Cardinal): string;
var
  sSub, sSub2: string;
  RowNumber: Cardinal;
  i: Integer;
begin
  RowNumber := Sender.FileNo * EachFileRowNumber;
  if Sender.LastFileNo then RowNumber := aRowNumber;
  sSub := NumberToString(RowNumber);

  i := Sender.FileName.IndexOf('.');
  Result := Sender.FileName.SubString(0, i);
  if Result.Equals('��2-2��')
    or Result.Equals('��2-3��')
    or Result.Equals('��2-4��')
    or Result.Equals('��2-5��')
  then
  begin
    sSub2 := Result.Replace('2-', '');
    Result := Result + Format('.����ĩ��%s = �С�%s- %d', [sSub, sSub2, Sender.FileNo - 1])
  end;
end;

procedure TDataComputer.SaveCompareData(fr: TFileWriter; Data: TSQLCompareData; SaveValues: Boolean = True);
var
  Row: TSQLRow;
  i, i2: Integer;
  s, sCompareType, sCompareType2, sFirstRow: string;
begin
  {TSQLRow.AutoFree(Row);
  for i := Low(Data.CompareRows) to High(Data.CompareRows) do
  begin
    with Data.CompareRows[i] do
    begin
      Row.FillPrepare(fDatabase, 'Number = ?', [RowNumber]);
      if not Row.FillOne then Continue;

      case fSettings.CompareType of
        1:
        begin
          sCompareType := 'ֱ';
          sCompareType2 := sCompareType;
          if fSettings.CompareMode = cmVertSlant then sCompareType2 := ' ' + sCompareType2;
        end;
        2:
        begin
          sCompareType := 'б';
          sCompareType2 := 'Y��';
        end;
        3:
        begin
          sCompareType := 'б';
          sCompareType2 := 'Z��';
        end;
      end;
      sFirstRow := '';
      if not fSettings.ExportLite then
      begin
        sFirstRow := '����%d��Ϊ���У�%s������%d�У�';
        sFirstRow := Format(sFirstRow, [Data.FirstRow, sCompareType, RowNumber]);
      end;
      s := Format('��%d����%s����%d%s���У���', [
        Number,
        sFirstRow,
        Data.FirstRow - RowNumber,
        sCompareType2
      ]);
      if SaveValues then
      begin
        s := s + '= '; }
        {if TotalSameValueCount > 0 then
        begin
          if CompareType = 1 then s := s + 'y'
          else s := s + ' ';
          if Length(fIntervalValues) = 1 then
            s := s + Format('�С���Ӧ�С�����%d�У�', [TotalSameValueCount])
          else
            s := s + Format('�С���Ӧ�С������� %d-%d ���У�', [SameValueCount, TotalSameValueCount - SameValueCount]);
          s := s + DataToString(SameValues);
        end;}
        {if TotalDifferentValueCount > 0 then
        begin
          if TotalSameValueCount > 0 then s := s + '  ';
          if CompareType = 1 then s := s + 'w��'
          else s := s + ' ';
          if not fSettings.ExportLite then s := s + '����Ӧ�С�����';
          if Length(fSettings.IntervalValues) = 1 then
            s := s + Format('%d��', [TotalDifferentValueCount])
          else
            s := s + Format('�� %d-%d ����', [DifferentValueCount, TotalDifferentValueCount - DifferentValueCount]);
          //if not fExportLite then s := s + '��' + DataToString(DifferentValues);
          if not fExportLite then s := s + '��' + TSQLData.ToString(DifferentValues, fIntervalValues, fDataMode);
        end;
      end;
      fr.WriteLn('');
      fr.WriteLn(s);
    end;
  end; }
end;

procedure TDataComputer.SaveFirstRow;
var
  RowSpacingList: TStringList;

  procedure AddRowSpacing(RowSpacing: Word; s: string);
  var
    sRowSpacing: string;
  begin
    sRowSpacing := RowSpacing.ToString;
    while sRowSpacing.Length < 3 do sRowSpacing := '0' + sRowSpacing;
    if RowSpacingList.IndexOfName(sRowSpacing) = -1 then
    begin
      if (fSettings.CompareMode = cmSlant) and s.Contains('��ͬ����') then
        s := s.Substring(0, s.IndexOf('��ͬ����')) + '��';
      RowSpacingList.Values[sRowSpacing] := s;
    end;
  end;

var
  fr: TFileWriter;
  s, FileName, TxtFileName, sRowSpacing, sGroupValueCount, sFirstRow: string;
  i, PageIndex: Integer;
  FirstRow: TSQLFirstRow;
  Data: TSQLCompareData;
  LastCodeNameValueCount: Byte;
  GroupNumber: Word;
  RowNo: Cardinal;
begin
  if fSettings.ExportLite then TxtFileName := '��1��..txt'
  else
    case fSettings.CompareMode of
      cmSlant: TxtFileName := '��1��.�����С���%d�������ϡ� [ ��ͬ���ڡ�N����Ϊ���У�] ������ͬ���ţ��������.txt';
      else TxtFileName := '��1��.�����С�����%d��������[��ͬ���С���ͬ���]����ϡ�.txt';;
    end;
  TxtFileName := Format(TxtFileName, [fSettings.ExportGroupValueCount]);
  FileName := fExportDirectory + TxtFileName;
  fr := TFileWriter.Create(FileName);
  fr.RebuildFileEvent := RebuildFile;
  fr.RebuildFileNameEvent := RebuildFileName;
  try
    if not fSettings.ExportLite then
    begin
      s := TPath.GetFileNameWithoutExtension(FileName);
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);
    end;

    TSQLFirstRow.AutoFree(FirstRow);
    TSQLCompareData.AutoFree(Data);

    s := 'ORDER BY Value DESC';
    //if fRecalcMode > 0 then s := 'Enabled = 1 ' + s;
    FirstRow.FillPrepare(fDatabase, s, []);
    RowSpacingList := TStringList.Create;
    try
      while FirstRow.FillOne do
      begin
        if FirstRow.FillCurrentRow = 2 then
        begin
          case fSettings.CompareMode of
            cmSlant:
            begin
              sFirstRow := '';
              if not fSettings.ExportLite then
              begin
                sFirstRow := '����%d��Ϊ���У�';
                sFirstRow := Format(sFirstRow, [fRowCount + 1]);
              end;
              s := '1.%s�����о� ��%d��';
              s := Format(s, [sFirstRow, fRowCount + 1 - FirstRow.Value]);
            end;
            else
            begin
              sFirstRow := '';
              if not fSettings.ExportLite then
              begin
                sFirstRow := '[ %s������%d��Ϊ���У�]��';
                sFirstRow := Format(sFirstRow, [fSettings.CompareModeString, fRowCount + 1]);
              end;
              s := '1.%s[ ���о� ��%d ]';
              s := Format(s, [sFirstRow, fRowCount + 1 - FirstRow.Value]);
            end;
          end;
          AddRowSpacing(fRowCount + 1 - FirstRow.Value, s);
        end;

        {if fRecalcMode > 0 then
        begin
          FirstRow.RowSpacing := FirstRow.RowSpacing2;
          FirstRow.RowCount := FirstRow.RowCount2;
        end;}
        case fSettings.CompareMode of
          cmSlant:
          begin
            sFirstRow := '';
            if not fSettings.ExportLite then
            begin
              sFirstRow := '����%d��Ϊ���У�';
              sFirstRow := Format(sFirstRow, [FirstRow.Value]);
            end;
            s := '%d.%s�����о� ��%d��ͬ������%d��';
            s := Format(s, [FirstRow.FillCurrentRow, sFirstRow, FirstRow.RowSpacing, FirstRow.RowCount]);
          end;
          else
          begin
            sFirstRow := '';
            if not fSettings.ExportLite then
            begin
              sFirstRow := '[ %s������%d��Ϊ���У�]��';
              sFirstRow := Format(sFirstRow, [fSettings.CompareModeString, FirstRow.Value]);
            end;
            s := '%d.%s[ ���о� ��%d ]';
            s := Format(s, [FirstRow.FillCurrentRow, sFirstRow, FirstRow.RowSpacing]);
          end;
        end;
        AddRowSpacing(FirstRow.RowSpacing, s);
      end;

      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn('���-С�����о� 1-100 ���ڣ�');
      RowSpacingList.Sort;
      for i := RowSpacingList.Count - 1 downto RowSpacingList.Count - 100 do
      begin
        if i < 0 then Break;
        fr.WriteLn('');
        fr.WriteLn(RowSpacingList.ValueFromIndex[i]);
      end;
    finally
      RowSpacingList.Free;
    end;

    FirstRow.FillRewind;
    while FirstRow.FillOne do
    begin
      if FirstRow.FillCurrentRow = 2 then
      begin
        case fSettings.CompareMode of
          cmSlant:
          begin
            sFirstRow := '';
            if not fSettings.ExportLite then
            begin
              sFirstRow := '����%d��Ϊ���У���';
              sFirstRow := Format(sFirstRow, [fRowCount + 1]);
            end;
            s := '1.%s�����о� ��%d��';
            s := Format(s, [sFirstRow, fRowCount + 1 - FirstRow.Value]);
          end;
          else
          begin
            sFirstRow := '';
            if not fSettings.ExportLite then
            begin
              sFirstRow := '[ %s������%d��Ϊ���У�]��';
              sFirstRow := Format(sFirstRow, [fSettings.CompareModeString, fRowCount + 1]);
            end;
            s := '1.%s[ ���о� ��%d ]';
            s := Format(s, [sFirstRow, fRowCount + 1 - FirstRow.Value]);
          end;
        end;
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn(s);
      end;

      {if fRecalcMode > 0 then
      begin
        FirstRow.RowSpacing := FirstRow.RowSpacing2;
        FirstRow.RowCount := FirstRow.RowCount2;
        FirstRow.GroupValueCount := FirstRow.GroupValueCount2;
        FirstRow.MaxGroupValueCount := FirstRow.MaxGroupValueCount2;
        FirstRow.MaxRowCountGroupValueCount := FirstRow.MaxRowCountGroupValueCount2;
        FirstRow.MaxGroupValueCountRowCount := FirstRow.MaxGroupValueCountRowCount2;
      end;}
      case fSettings.CompareMode of
        cmSlant:
        begin
          sFirstRow := '';
          if not fSettings.ExportLite then
          begin
            sFirstRow := '����%d��Ϊ���У�';
            sFirstRow := Format(sFirstRow, [FirstRow.Value]);
          end;
          s := '%d.%s�����о� ��%d��ͬ������%d��';
          s := Format(s, [FirstRow.FillCurrentRow, sFirstRow, FirstRow.RowSpacing, FirstRow.RowCount]);
        end;
        else
        begin
          sFirstRow := '';
          if not fSettings.ExportLite then
          begin
            sFirstRow := '[ %s������%d��Ϊ���У�]��';
            sFirstRow := Format(sFirstRow, [fSettings.CompareModeString, FirstRow.Value]);
          end;
          //s := '%d.%s[ ���о� ��%d ��';
          s := '%d.%s[ ���о� ��%d ]';
          s := Format(s, [FirstRow.FillCurrentRow, sFirstRow, FirstRow.RowSpacing]);
          {if FirstRow.MaxGroupValueCount > fExportGroupValueCount then
          begin
            sGroupValueCount := '';
            for i := fExportGroupValueCount to FirstRow.MaxGroupValueCount do
            begin
              if not sGroupValueCount.IsEmpty then
                sGroupValueCount := sGroupValueCount + '��';
              sGroupValueCount := sGroupValueCount + i.ToString;
            end;

            s := s + Format('����%s����ϣ��ܹ� %d �� ��', [
              sGroupValueCount,
              FirstRow.GroupValueCount
            ]);
          end;
          s := s + Format('��ࣨ��%d����ϣ����� %d �� ]��', [
            FirstRow.MaxRowCountGroupValueCount,
            FirstRow.MaxGroupValueCountRowCount
          ]);}
        end;
      end;
      if FirstRow.FillCurrentRow > 2 then
      begin
        fr.WriteLn('');
        fr.WriteLn('');
      end;
      fr.WriteLn('');
      fr.WriteLn(s);

      PageIndex := 0;
      RowNo := 0;
      repeat
        s := 'FirstRow = ?';
        //if fRecalcMode > 0 then s := s + ' AND Enabled = 1';
        s := s + ' LIMIT ? OFFSET ?';
        Data.FillPrepare(fDatabase, s, [FirstRow.Value, EachPageRowCount, PageIndex * EachPageRowCount]);
        LastCodeNameValueCount := 0;
        GroupNumber := 0;
        while Data.FillOne do
        begin
          RowNo := RowNo + 1;
          case fSettings.CompareMode of
            cmSlant:
            begin
              s := Format('��%d��.[���ţ�����%s����%s ]', [
                RowNo,
                Data.CodeName,
                TSQLCodeName.SlantValue(Data.CodeName)
              ]);
              if Length(fSettings.IntervalValues) = 1 then
                s := s + Format(' = �ޡ�%s�С����� %d�� ��', [fTipStr, Data.TotalValueCount])
              else
                s := s + Format(' = �� %d-%d ���� ��', [Data.ValueCount, Data.TotalValueCount - Data.ValueCount]);
              s := s + Format('�������֡���%s', [Data.ToString(fSettings.DataMode)]);
              fr.WriteLn('');
              fr.WriteLn(s);
            end;
            else
            begin
              if Data.CodeNameValueCount <> LastCodeNameValueCount then
              begin
                LastCodeNameValueCount := Data.CodeNameValueCount;
                GroupNumber := 0;
              end;
              GroupNumber := GroupNumber + 1;   //��Ҫ

              //s := '��%d��.��%d�����[ ���ţ�%d.��%s��]��';
              s := '��%d��.��%d�����[ ���ţ���%s��]';
              s := Format(s, [
                RowNo,
                Data.CodeNameValueCount,
                //GroupNumber,
                Data.CodeName
              ]);
              fr.WriteLn('');
              fr.WriteLn(s);

              SaveCompareData(fr, Data);
            end;
          end;
        end;
        Inc(PageIndex);
      until Data.FillTable.RowCount = 0;
    end;
    case fSettings.CompareMode of
      cmVert:
      begin
        s := '%d.[����%d��Ϊ��ĩ���У�ֱ������%d-1�У�]';
        s := Format(s, [
          FirstRow.FillTable.RowCount + 2,
          fSettings.VertCompareSpacing + 1,
          fSettings.VertCompareSpacing
        ]);
        fr.WriteLn('');
        fr.WriteLn(s);
      end;
      cmSlant:
      begin
        s := '%d.����%d��Ϊ��ĩ���У�';
        s := Format(s, [
          FirstRow.FillTable.RowCount + 2,
          fSettings.SlantCompareSpacing + 1
        ]);
        fr.WriteLn('');
        fr.WriteLn(s);
      end;
      cmVertSlant:
      begin
        s := '%d.[����%d��Ϊ��ĩ���У�ֱ������%d-%d�У�]';
        s := Format(s, [
          FirstRow.FillTable.RowCount + 2,
          fSettings.CompareSpacing + 1,
          fSettings.CompareSpacing,
          fSettings.CompareSpacing + 1 - fSettings.VertCompareSpacing
        ]);
        fr.WriteLn('');
        fr.WriteLn(s);
        s := '%d.[����%d��Ϊ��ĩ���У�б������%d-%d�У�]';
        s := Format(s, [
          FirstRow.FillTable.RowCount + 3,
          fSettings.CompareSpacing + 1,
          fSettings.CompareSpacing,
          fSettings.CompareSpacing + 1 - fSettings.SlantCompareSpacing
        ]);
        fr.WriteLn('');
        fr.WriteLn(s);
      end;
    end;

    fr.RenameLastFile;
    if efFile in fSettings.ExportFiles then
      for s in fr.Files do
        if TFile.Exists(s) then TFile.Copy(s, fExportDirectory2 + TPath.GetFileName(s), True);
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.SaveCodeNameSortByRowcount;
var
  fr, fr2, fr3: TFileWriter;
  s, sCompareMode, SlantGroupValue, FileName, FileName2, FileName3, TxtFileName: string;
  CodeName: TSQLCodeName;
  Data: TSQLCompareData;
  RowNo, GroupRowNo: Cardinal;
  LastFirstRow: Cardinal;
  PageIndex: Integer;
begin
  if fSettings.ExportLite then TxtFileName := '��2-1��.��1��.txt'
  else
    case fSettings.CompareMode of
      cmSlant: TxtFileName := '��2-1��.�����С���%d�������ϸ�����ϣ���ͬ���š���ͬ���У��ģ���ͬ����������� - �����У���1��.txt';
      else TxtFileName := '��2-1��.�����С�����%d��������[��ͬ��ϡ���ͬ����]�����[��ͬ��������������]����1��.txt';
    end;
  TxtFileName := Format(TxtFileName, [fSettings.ExportGroupValueCount]);
  FileName := fExportDirectory + TxtFileName;
  if fSettings.ExportLite then TxtFileName := '��2-2��.��2��.txt'
  else
    case fSettings.CompareMode of
      cmSlant: TxtFileName :='��2-2��.�����С���%d�������ϸ�����ϣ���ͬ���š���ͬ���У��ģ���ͬ����������� - �����У���2��.txt';
      else TxtFileName := '��2-2��.�����С�����%d��������[��ͬ��ϡ���ͬ����]�����[��ͬ��������������]����2��.txt';
    end;
  TxtFileName := Format(TxtFileName, [fSettings.ExportGroupValueCount]);
  FileName2 := fExportDirectory + TxtFileName;
  if fSettings.ExportLite then TxtFileName := '��2-3��.��3��.txt'
  else
    case fSettings.CompareMode of
      cmSlant: TxtFileName := '��2-3��.�����С���%d�������ϸ�����ϣ���ͬ���š���ͬ���У��ģ���ͬ����������� - �����У���3��.txt';
      else TxtFileName := '��2-3��.�����С�����%d��������[��ͬ��ϡ���ͬ����]�����[��ͬ��������������]����3��.txt';
    end;
  TxtFileName := Format(TxtFileName, [fSettings.ExportGroupValueCount]);
  FileName3 := fExportDirectory + TxtFileName;
  if efFile11 in fSettings.ExportFiles then
  begin
    fr := TFileWriter.Create(FileName);
    fr.RebuildFileEvent := RebuildFile;
    fr.RebuildFileNameEvent := RebuildFileName;
  end;
  if efFile12 in fSettings.ExportFiles then
  begin
    fr2 := TFileWriter.Create(FileName2);
    fr2.RebuildFileNameEvent := RebuildFileName2;
  end;
  if efFile13 in fSettings.ExportFiles then
  begin
    fr3 := TFileWriter.Create(FileName3);
    fr3.RebuildFileNameEvent := RebuildFileName2;
  end;
  try
    if not fSettings.ExportLite then
    begin
      if Assigned(fr) then
      begin
        s := TPath.GetFileNameWithoutExtension(FileName);
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn(s);
      end;
      if Assigned(fr2) then
      begin
        s := TPath.GetFileNameWithoutExtension(FileName2);
        fr2.WriteLn('');
        fr2.WriteLn('');
        fr2.WriteLn(s);
      end;
      if Assigned(fr3) then
      begin
        s := TPath.GetFileNameWithoutExtension(FileName3);
        fr3.WriteLn('');
        fr3.WriteLn('');
        fr3.WriteLn(s);
        if fSettings.CompareMode = cmSlant then
        begin
          fr3.WriteLn('');
          fr3.WriteLn('');
          fr3.WriteLn('');
        end;
      end;
    end;

    sCompareMode := '';
    if not fSettings.ExportLite then
    begin
      case fSettings.CompareMode of
        cmSlant: sCompareMode := '��б����';
        else sCompareMode := Format('��%s����', [fSettings.CompareModeString]);
      end;
    end;

    TSQLCodeName.AutoFree(CodeName);
    TSQLCompareData.AutoFree(Data);
    RowNo := 0;
    GroupRowNo := 0;
    PageIndex := 0;
    repeat
      s := 'ORDER BY RowCount DESC, ValueCount LIMIT ? OFFSET ?';
      //if fRecalcMode > 0 then s := 'Enabled = 1 ' + s;
      CodeName.FillPrepare(fDatabase, s, [EachPageRowCount, PageIndex * EachPageRowCount]);
      while CodeName.FillOne do
      begin
        GroupRowNo := GroupRowNo + 1;
        case fSettings.CompareMode of
          cmSlant:
          begin
            SlantGroupValue := CodeName.SlantValue;

            s := '%d.[���ţ�����%s����%s ] ��[ ��ͬ%s��������%d�� ]';
            s := Format(s, [GroupRowNo, CodeName.Value, SlantGroupValue, sCompareMode, CodeName.RowCount]);
          end;
          else
          begin
            s := '%d.����%d���� [ ��ͬ��ϡ���ͬ%s����]����� [ ��ͬ%s��������%d ]��[���ţ�%s ] ��';
            s := Format(s, [
              GroupRowNo,
              CodeName.ValueCount,
              sCompareMode,
              sCompareMode,
              CodeName.RowCount,
              CodeName.Value
            ]);
          end;
        end;
        if Assigned(fr) then
        begin
          fr.WriteLn('');
          fr.WriteLn('');
          fr.WriteLn('');
          fr.WriteLn(s);
        end;
        if Assigned(fr2) then
        begin
          fr2.WriteLn('');
          fr2.WriteLn('');
          fr2.WriteLn('');
        end;
        case fSettings.CompareMode of
          cmSlant:
          begin
            if Assigned(fr2) then
            begin
              fr2.WriteLn(s);
              fr2.WriteLn('');
            end;
          end;
          cmVertSlant:
          begin
            if Assigned(fr2) then fr2.WriteLn(s);
            if Assigned(fr3) then
            begin
              fr3.WriteLn('');
              fr3.WriteLn('');
              fr3.WriteLn('');
            end;
          end;
        end;

        LastFirstRow := 0;
        s := 'CodeName = ? ORDER BY FirstRow DESC';
        Data.FillPrepare(fDatabase, s, [CodeName.Value]);
        while Data.FillOne do
        begin
          if Data.FillCurrentRow = 2 then
          begin
            RowNo := RowNo + 1;

            if Assigned(fr2) then
            begin
              case fSettings.CompareMode of
                cmSlant:
                begin
                  s := '1=[����%s����%s ] ����%d����%d��';
                  s := Format(s, [CodeName.Value, SlantGroupValue, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
                end;
                else
                begin
                  s := '%d= ��%d�����[ ���ţ�3.��%s��]����%d����%d��';
                  s := Format(s, [RowNo, CodeName.ValueCount, CodeName.Value, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
                  if Assigned(fr2) then fr2.WriteLn('');
                end;
              end;
              fr2.WriteLn(s);
              if RowNo mod EachFileRowNumber = 0 then fr2.BuildActiveFileName;
            end;

            if Assigned(fr3) then
            begin
              case fSettings.CompareMode of
                cmSlant:
                begin
                  s := '%d=[����%s����%s ] ����%d����%d��';
                  s := Format(s, [RowNo, CodeName.Value, SlantGroupValue, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
                end;
                cmVertSlant:
                begin
                  s := '%d=[ ���ţ�3.��%s��]����%d����%d��';
                  s := Format(s, [RowNo, CodeName.Value, fRowCount + 1, fRowCount + 1 - Data.FirstRow]);
                  if Assigned(fr3) then fr3.WriteLn('');
                end;
              end;
              fr3.WriteLn(s);
              if RowNo mod EachFileRowNumber = 0 then fr3.BuildActiveFileName;
            end;
          end;

          RowNo := RowNo + 1;
          case fSettings.CompareMode of
            cmSlant:
            begin
              if Assigned(fr) then
              begin
                s := Format('��%d������%d��Ϊ���У�', [Data.FillCurrentRow - 1, Data.FirstRow]);
                if Length(fSettings.IntervalValues) = 1 then
                  s := s + Format('= �ޡ�%s�С����� %d�� ��', [fTipStr, Data.TotalValueCount])
                else
                  s := s + Format('= �ޡ�%s�С�����%d - %d �� ��', [fTipStr, Data.ValueCount, Data.TotalValueCount - Data.ValueCount]);
                s := s + Format('�������֡���%s', [Data.ToString(fSettings.DataMode)]);
                fr.WriteLn('');
                fr.WriteLn(s);
              end;

              if Assigned(fr2) then
              begin
                s := '%d=[����%s����%s ] ����%d����%d��';
                s := Format(s, [Data.FillCurrentRow, CodeName.Value, SlantGroupValue, Data.FirstRow, Data.RowSpacing]);
                fr2.WriteLn(s);
                if RowNo mod EachFileRowNumber = 0 then fr2.BuildActiveFileName;
              end;

              if Assigned(fr3) then
              begin
                s := '%d=[����%s����%s ] ����%d����%d��';
                s := Format(s, [RowNo, CodeName.Value, SlantGroupValue, Data.FirstRow, Data.RowSpacing]);
                fr3.WriteLn(s);
                if RowNo mod EachFileRowNumber = 0 then fr3.BuildActiveFileName;
              end;
            end;
            else
            begin
              if Assigned(fr) then
              begin
                if LastFirstRow <> Data.FirstRow then
                begin
                  LastFirstRow := Data.FirstRow;
                  //s := '��%d��.��%d�����[ ���ţ���%s��]��';
                  //s := Format(s, [Data.FillCurrentRow - 1, Group.ValueCount, Group.Value]);
                  s := '��%d��.��%d��Ϊ����';
                  s := Format(s, [Data.FillCurrentRow - 1, Data.FirstRow]);
                  fr.WriteLn('');
                  fr.WriteLn(s);
                end;
                SaveCompareData(fr, Data);
              end;

              if Assigned(fr2) then
              begin
                s := '%d= ��%d�����[ ���ţ���%s��]����%d����%d��';
                s := Format(s, [RowNo, CodeName.ValueCount, CodeName.Value, Data.FirstRow, Data.RowSpacing]);
                fr2.WriteLn('');
                fr2.WriteLn(s);
                SaveCompareData(fr2, Data);
                if RowNo mod EachFileRowNumber = 0 then fr2.BuildActiveFileName;
              end;

              if (fSettings.CompareMode = cmVertSlant) and Assigned(fr3) then
              begin
                s := '%d=[ ���ţ���%s��]����%d����%d��';
                s := Format(s, [RowNo, CodeName.Value, Data.FirstRow, Data.RowSpacing]);
                fr3.WriteLn('');
                fr3.WriteLn(s);
                SaveCompareData(fr3, Data, False);
                if RowNo mod EachFileRowNumber = 0 then fr3.BuildActiveFileName;
              end;
            end;
          end;

          if Data.FillCurrentRow > Data.FillTable.RowCount then
          begin
            RowNo := RowNo + 1;

            case fSettings.CompareMode of
              cmSlant:
              begin
                if Assigned(fr2) then
                begin
                  s := '%d=[����%s����%s ] ����%d����0��';
                  s := Format(s, [Data.FillCurrentRow + 1, CodeName.Value, SlantGroupValue, fSettings.SlantCompareSpacing + 1]);
                  fr2.WriteLn(s);
                  if RowNo mod EachFileRowNumber = 0 then fr2.BuildActiveFileName;
                end;

                if Assigned(fr3) then
                begin
                  s := '%d=[����%s����%s ] ����%d����0��';
                  s := Format(s, [RowNo, CodeName.Value, SlantGroupValue, fSettings.SlantCompareSpacing + 1]);
                  fr3.WriteLn(s);
                  if RowNo mod EachFileRowNumber = 0 then fr3.BuildActiveFileName;
                end;
              end
              else
              begin
                if Assigned(fr2) then
                begin
                  s := '%d= ��%d�����[ ���ţ���%s��]����%d����0��';
                  s := Format(s, [RowNo, CodeName.ValueCount, CodeName.Value, fSettings.CompareSpacing + 1]);
                  fr2.WriteLn('');
                  fr2.WriteLn(s);
                  if RowNo mod EachFileRowNumber = 0 then fr2.BuildActiveFileName;
                end;

                if (fSettings.CompareMode = cmVertSlant) and Assigned(fr3) then
                begin
                  s := '%d=[ ���ţ���%s��]����%d����0��';
                  s := Format(s, [RowNo, CodeName.Value, fSettings.CompareSpacing + 1]);
                  fr3.WriteLn('');
                  fr3.WriteLn(s);
                  if RowNo mod EachFileRowNumber = 0 then fr3.BuildActiveFileName;
                end;
              end;
            end;
          end;
        end;
      end;
      Inc(PageIndex);
    until CodeName.FillTable.RowCount = 0;

    if Assigned(fr) then
    begin
      fr.RenameLastFile;
      if (efFile2 in fSettings.ExportFiles) then
      begin
        for s in fr.Files do
        begin
          TxtFileName := TPath.GetFileName(s).Replace('��2-1��', '��2��');
          TxtFileName := TxtFileName.Replace('��1��', '');
          if TFile.Exists(s) then TFile.Copy(s, fExportDirectory2 + TxtFileName, True);
        end;
      end;
    end;
    if Assigned(fr2) then
    begin
      fr2.WriteFinish;
      fr2.RenameLastFile(RebuildFileName2(fr2, RowNo));
    end;
    if Assigned(fr3) then
    begin
      fr3.WriteFinish;
      fr3.RenameLastFile(RebuildFileName2(fr3, RowNo));
    end;
  finally
    if Assigned(fr) then fr.Free;
    if Assigned(fr2) then fr2.Free;
    if Assigned(fr3) then fr3.Free;
  end;
end;

procedure TDataComputer.SaveSlantCodeName;
var
  fr: TFileWriter;
  s, FileName, TxtFileName: string;
  CodeName: TSQLCodeName;
  PageIndex: Integer;
  GroupRowNo: Cardinal;
begin
  if fSettings.ExportLite then TxtFileName := '��6��..txt'
  else TxtFileName := '��6��.�����桿��%d�������ϸ�����ϣ���ͬ���š���ͬ���У��ģ����ţ�NZ. NY ��.txt';
  TxtFileName := Format(TxtFileName, [fSettings.ExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  if TFile.Exists(FileName) then TFile.Delete(FileName);
  fr := TFileWriter.Create(FileName);
  fr.RebuildFileEvent := RebuildFile;
  fr.RebuildFileNameEvent := RebuildFileName;
  try
    TSQLCodeName.AutoFree(CodeName);
    PageIndex := 0;
    GroupRowNo := 0;
    repeat
      //s := 'ORDER BY ValueCount, Value2 LIMIT ? OFFSET ?';
      s := 'LIMIT ? OFFSET ?';
      //if fSettings.RecalcMode > 0 then s := 'Enabled = 1 ' + s;
      CodeName.FillPrepare(fDatabase, s, [EachPageRowCount, PageIndex * EachPageRowCount]);
      while CodeName.FillOne do
      begin
        GroupRowNo := GroupRowNo + 1;
        s := Format('%d=%s', [GroupRowNo, CodeName.SlantValue]);
        fr.WriteLn(s);
      end;
      Inc(PageIndex);
    until CodeName.FillTable.RowCount = 0;
    fr.RenameLastFile;
  finally
    fr.Free;
  end
end;

procedure TDataComputer.SaveCodeNameSortByValueCount(FileName: string);
var
  fr: TFileWriter;
  s: string;
  CodeName: TSQLCodeName;
  Data: TSQLCompareData;
  PageIndex: Integer;
  GroupRowNo: Cardinal;
begin
  fr := TFileWriter.Create(FileName);
  fr.RebuildFileEvent := RebuildFile;
  fr.RebuildFileNameEvent := RebuildFileName;
  try
    if not fSettings.ExportLite then
    begin
      s := TPath.GetFileNameWithoutExtension(FileName);
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn(s);
    end;

    TSQLCodeName.AutoFree(CodeName);
    TSQLCompareData.AutoFree(Data);
    GroupRowNo := 0;
    PageIndex := 0;
    repeat
      //s := 'ORDER BY RowCount DESC, ValueCount, Value2 LIMIT ? OFFSET ?';
      s := 'LIMIT ? OFFSET ?';
      //if fRecalcMode > 0 then s := 'Enabled = 1 ' + s;
      CodeName.FillPrepare(fDatabase, s, [EachPageRowCount, PageIndex * EachPageRowCount]);
      while CodeName.FillOne do
      begin
        GroupRowNo := GroupRowNo + 1;
        s := 'CodeName = ? LIMIT 1';
        Data.FillPrepare(fDatabase, s, [CodeName.Value]);
        Data.FillOne;

        //s := '%d.[ ��%d����ϣ����ţ���%s��]���� ';
        s := '%d.[ ��%d����ϣ����ţ���%s��]';
        s := Format(s, [GroupRowNo, CodeName.ValueCount, CodeName.Value]);
        {if Data.TotalSameValueCount > 0 then
        begin
          s := s + 'y�С���Ӧ�С�������';
          if Length(fIntervalValues) = 1 then
            s := s + Format('%d��', [Data.TotalSameValueCount])
          else
            s := s + Format('%d-%d��', [Data.SameValueCount, Data.TotalSameValueCount - Data.SameValueCount])
        end;
        if Data.TotalDifferentValueCount > 0 then
        begin
          if Data.TotalSameValueCount > 0 then s := s + '��';

          s := s + Format('w�ޡ�%s�С�������', [fTipStr]);

          if Length(fIntervalValues) = 1 then
            s := s + Format('%d��', [Data.TotalDifferentValueCount])
          else
            s := s + Format('%d-%d��', [Data.DifferentValueCount, Data.TotalDifferentValueCount - Data.DifferentValueCount])
        end;
        s := s + '����';}
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn(s);
        s := Format('����%d��Ϊ���У�', [Data.FirstRow]);
        fr.WriteLn('');
        fr.WriteLn(s);

        //SaveCompareData(fr, Data);
      end;
      Inc(PageIndex);
    until CodeName.FillTable.RowCount = 0;
    fr.RenameLastFile;
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.SaveVertSlantCompareDataSortByValueCount2;
var
  FileName, TxtFileName: string;
begin
  if fSettings.ExportLite then TxtFileName := '��6��..txt'
  else TxtFileName := '��6��.���򻯡�����%d��������[��ͬ��ϡ���ͬ����]����ϡ�.txt';
  TxtFileName := Format(TxtFileName, [fSettings.ExportGroupValueCount]);
  FileName := fExportDirectory2 + TxtFileName;
  SaveCodeNameSortByValueCount(FileName);
end;

procedure TDataComputer.ExportData;
var
  s, sMode, sCompareMode: string;
begin
  sMode := '1';
  if fSettings.DataMode = 1 then sMode := '3';
  if Length(fSettings.IntervalValues) > 1 then
  begin
    sMode := '2';
    if fSettings.DataMode = 1 then sMode := '4';
  end;
  sCompareMode := '';
  if not fSettings.ExportLite  then sCompareMode := Format('��%s����', [fSettings.CompareModeString]);
  fExportDirectory := fDirectory + Format('\�������%s����%sģʽ��\', [sCompareMode, sMode]);
  case fSettings.CompareMode of
    cmSlant: fExportDirectory2 := fExportDirectory + '��1��.�����С�-��6��.�����桿\';
    else fExportDirectory2 := fExportDirectory + '��1��.�����С�-��6��.���򻯡�\';
  end;
  fTipStr := '';
  if not fSettings.ExportLite then fTipStr := '��Ӧ';

  if not TDirectory.Exists(fExportDirectory) then
    TDirectory.CreateDirectory(fExportDirectory);
  for s in TDirectory.GetFiles(fExportDirectory, '*.txt') do TFile.Delete(s);
  if not TDirectory.Exists(fExportDirectory2) then
    TDirectory.CreateDirectory(fExportDirectory2);
  for s in TDirectory.GetFiles(fExportDirectory2, '*.txt') do TFile.Delete(s);

  {if efFile3 in fExportFiles then
    SaveGroupByCompareTypeSortByMaxRowSpacing;
  if efFile4 in fExportFiles then
    SaveGroupByCompareTypeSortByGroupValueCount;
  if efFile5 in fExportFiles then
    case fCompareMode of
      cmVertSlant: SaveVertSlantCompareDataSortByValueCount;
      else SaveGroupByCompareTypeSortByMaxValueCount;
    end;}
  if efFile6 in fSettings.ExportFiles then
    case fSettings.CompareMode of
      cmSlant: SaveSlantCodeName;
      else SaveVertSlantCompareDataSortByValueCount2;
    end;
  if efFile7 in fSettings.ExportFiles then SaveFirstRow;
  //if efFile8 in fExportFiles then SaveGroupByCompareTypeSortByRowcount2;
  //if efFile10 in fExportFiles then SaveReEnabledGroup;
  if (efFile11 in fSettings.ExportFiles)
    or (efFile12 in fSettings.ExportFiles)
    or (efFile13 in fSettings.ExportFiles)
  then SaveCodeNameSortByRowcount;
  {if efFile14 in fExportFiles then SaveGroupByCompareTypeSortByLastFirstRow;
  if efFile15 in fExportFiles then
    case fCompareMode of
      cmVert: SaveVertGroupByCompareTypeSortByLastFirstRow2;
      else SaveGroupByCompareTypeSortByOneRowSpacingCount;
    end;}
end;

procedure TDataComputer.Execute;
begin
  TMonitor.Enter(fLock);
  try
    repeat
      TMonitor.Wait(fLock, Cardinal.MaxValue);
      if Terminated then Break;

      fStopwatch := TStopwatch.StartNew;
      try
        if Now >= 44562 then  Break;   //2022-01-01

        LoadRow;
        if Terminated then Break;
        CompareRow;
        if Terminated then Break;
        GroupCodeName;
        if Terminated then Break;
        UpdateFirstRow;
        if Terminated then Break;
        BuildCodeName;
        if Terminated then Break;
        ExportData;
        if Terminated then Break;

        fStopwatch.Stop;
        if Assigned(fOnFinish) then fOnFinish(Self);
      finally
        fStopwatch.Stop;
      end;
    until Terminated;

    fConsumers.FinishConsumers;
  finally
    TMonitor.Exit(fLock);
  end;
end;

procedure ReadSettings;
var
  v: Variant;
  IntervalValues: TWordDynArray;
begin
  fKeyValue.GetKeyValue('IntervalValues', IntervalValues);
  fSettings.IntervalValues := IntervalValues;
  fKeyValue.GetKeyValue('CompareCrossRange', v);
  if not VarIsEmpty(v) then fSettings.CompareCrossRange := v;
  fSettings.CompareMode := cmNone;
  fKeyValue.GetKeyValue('CompareMode', v);
  if not VarIsEmpty(v) then fSettings.CompareMode := v;
  fKeyValue.GetKeyValue('VertCompareSpacing', v);
  if not VarIsEmpty(v) then fSettings.VertCompareSpacing := v;
  fKeyValue.GetKeyValue('VertSameValueCount', v);
  if not VarIsEmpty(v) then fSettings.VertSameValueCount := v;
  fKeyValue.GetKeyValue('VertSameValueCount2', v);
  if not VarIsEmpty(v) then fSettings.VertSameValueCount2 := v;
  fKeyValue.GetKeyValue('SlantCompareSpacing', v);
  if not VarIsEmpty(v) then fSettings.SlantCompareSpacing := v;
  fKeyValue.GetKeyValue('SlantSameValueCount', v);
  if not VarIsEmpty(v) then fSettings.SlantSameValueCount := v;
  fKeyValue.GetKeyValue('SlantSameValueCount2', v);
  if not VarIsEmpty(v) then fSettings.SlantSameValueCount2 := v;
  fKeyValue.GetKeyValue('GroupValueCount', v);
  if not VarIsEmpty(v) then fSettings.GroupValueCount := v;
  fKeyValue.GetKeyValue('MaxGroupCount', v);
  if not VarIsEmpty(v) then fSettings.MaxGroupCount := v;
  fKeyValue.GetKeyValue('ExportGroupValueCount', v);
  if not VarIsEmpty(v) then fSettings.ExportGroupValueCount := v;
  fKeyValue.GetKeyValue('CodeNameSort', v);
  if not VarIsEmpty(v) then fSettings.CodeNameSort := v;
end;

procedure WriteSettings;
begin
  fKeyValue.SetKeyValue('IntervalValues', fSettings.IntervalValues);
  fKeyValue.SetKeyValue('CompareCrossRange', fSettings.CompareCrossRange);
  fKeyValue.SetKeyValue('CompareMode', fSettings.CompareMode);
  fKeyValue.SetKeyValue('VertCompareSpacing', fSettings.VertCompareSpacing);
  fKeyValue.SetKeyValue('VertSameValueCount', fSettings.VertSameValueCount);
  fKeyValue.SetKeyValue('VertSameValueCount2', fSettings.VertSameValueCount2);
  fKeyValue.SetKeyValue('SlantCompareSpacing', fSettings.SlantCompareSpacing);
  fKeyValue.SetKeyValue('SlantSameValueCount', fSettings.SlantSameValueCount);
  fKeyValue.SetKeyValue('SlantSameValueCount2', fSettings.SlantSameValueCount2);
  fKeyValue.SetKeyValue('GroupValueCount', fSettings.GroupValueCount);
  fKeyValue.SetKeyValue('MaxGroupCount', fSettings.MaxGroupCount);
  fKeyValue.SetKeyValue('ExportGroupValueCount', fSettings.ExportGroupValueCount);
  fKeyValue.SetKeyValue('CodeNameSort', fSettings.CodeNameSort);
end;

procedure ConnectServer(Address, Port: string);
var
  i: Integer;
begin
  DisconnectServer;

  fClient := TSQLHttpClientWebsockets.Create(Address, Port, TSQLModel.Create([TSQLFirstRow, TSQLCompareData]));
  fClient.Model.Owner := fClient;
  fClient.WebSocketsUpgrade('encryptionkey');
  if not fClient.ServerTimestampSynchronize then
    raise Exception.Create('����ʧ��');
  fClient.ServiceDefine([IService], sicShared);
  if not fClient.Services.Resolve(IService, fService) then
    raise EServiceException.Create('Service IService unavailable');
  fNotifyCallback := TNotifyCallback.Create(fClient, INotifyCallback);
  fService.RegisterConsumer(fNotifyCallback, TNotifyCallback(fNotifyCallback).ID, fLocalIP);

  fMainApp := Address = fLocalIP;
end;

procedure DisconnectServer;
begin
  if Assigned(fClient) then
  begin
    fNotifyCallback := nil;
    fService := nil;
    FreeAndNil(fClient);
  end;
end;

initialization

  with TIdIPWatch.Create do
  begin
    try
      fLocalIP := LocalIP;
    finally
      Free;
    end;
  end;

  fDatabase := TSQLRestServerDB.CreateWithOwnModel([TSQLKeyValue, TSQLRow, TSQLFirstRow,
    TSQLCodeName, TSQLCompareData], fDirectory + 'Data');
  fDatabase.CreateMissingTables;
  fDatabase.CreateSQLIndex(TSQLCompareData, 'FirstRow', False);
  fDatabase.CreateSQLIndex(TSQLCompareData, 'GroupValue', False);
  fDatabase.ServiceDefine(TService, [IService], sicShared);//.SetOptions([], [optExecLockedPerInterface]);
  //fDatabase2 := TSQLRestServerDB.CreateWithOwnModel([TSQLKeyValue], fDirectory + 'Data');
  //fDatabase2.CreateMissingTables;
  fKeyValue := TSQLKeyValue.Create;
  fKeyValue.SetRest(fDatabase);
  ReadSettings;

  if not Assigned(fConsumers) then fConsumers := TConsumers.Create;

  try
    fServer := TSQLHttpServer.Create('8888', [fDatabase], '+', useBidirSocket);
    fServer.WebSocketsEnable(fDatabase, 'encryptionkey');
  except
  end;

  fProcessorCount := GetEnvironmentVariable('NUMBER_OF_PROCESSORS').ToInteger;   //fProcessorCount := 1;
  ConnectServer(fLocalIP, '8888');

finalization
  if Assigned(fConsumers) then FreeAndNil(fConsumers);
  if Assigned(fKeyValue) then FreeAndNil(fKeyValue);
  if Assigned(fServer) then FreeAndNil(fServer);
  if Assigned(fDatabase) then FreeAndNil(fDatabase);
  //if Assigned(fDatabase2) then FreeAndNil(fDatabase2);

end.
