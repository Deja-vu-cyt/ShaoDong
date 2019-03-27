unit uDataComputer;

interface

uses
  SynCommons,
  mORMot,
  mORMotSQLite3,
  mORMotHttpServer,
  mORMotHttpClient,
  SynSQLite3,
  SynSQLite3Static,
  System.Classes,
  System.Variants,
  System.SysUtils,
  System.Types,
  System.IOUtils,
  System.Generics.Collections,
  System.SyncObjs,
  System.Diagnostics,
  System.Math,
  IdIPWatch,
  uCommon,
  uFileWriter;

type
  TSQLCompareData = class(TSQLData)
  end;

  TSQLFirstRow = class(TSQLRecord)
  protected
    fValue: Word;
    fCodeName: TWordDynArray;
    fCodeNameValueCount: Word;
    fGrouped: Boolean;
    fRowSpacing: Word;
    fRowCount: Word;
  public
    procedure ClearCodeName;
    procedure AddCodeName(aCodeName: Word);
  published
    property Value: Word read fValue write fValue stored AS_UNIQUE;
    property CodeName: TWordDynArray read fCodeName write fCodeName;
    property CodeNameValueCount: Word read fCodeNameValueCount write fCodeNameValueCount;
    property Grouped: Boolean read fGrouped write fGrouped;
    property RowSpacing: Word read fRowSpacing write fRowSpacing;
    property RowCount: Word read fRowCount write fRowCount;
  end;

  TSQLCodeName = class(TSQLRecord)
  protected
    fValue: RawUTF8;
    fValueCount: Word;
    fBatchNumber: Byte;
    fBatchNumberRepeat: Boolean;
    fFirstRow: TWordDynArray;
    fFirstRowValueCount: Word;
    fVersion: TRecordVersion;
  public
    procedure ClearFirstRow;
    function MergeFirstRow(aFirstRow: TWordDynArray): Boolean;
    procedure Intersection(aCodeName: string);
    function ToWordDynArray: TWordDynArray;
  published
    property Value: RawUTF8 read fValue write fValue stored AS_UNIQUE;
    property ValueCount: Word read fValueCount write fValueCount;
    property BatchNumber: Byte read fBatchNumber write fBatchNumber;
    property BatchNumberRepeat: Boolean read fBatchNumberRepeat write fBatchNumberRepeat;
    property FirstRow: TWordDynArray read fFirstRow write fFirstRow;
    property FirstRowValueCount: Word read fFirstRowValueCount write fFirstRowValueCount;
    property Version: TRecordVersion read fVersion write fVersion;
  end;

  TSQLFirstRowCodeName = class(TSQLRecord)
  protected
    fFirstRow: Word;
    fCodeName: RawUTF8;
    fCodeNameValueCount: Word;
  published
    property FirstRow: Word read fFirstRow write fFirstRow;
    property CodeName: RawUTF8 read fCodeName write fCodeName;
    property CodeNameValueCount: Word read fCodeNameValueCount write fCodeNameValueCount;
  end;

  INotifyCallback = interface(IInvokable)
    ['{EA7EFE51-3EBA-4047-A356-253374518D1D}']
    procedure SyncCodeName;
    procedure GroupCodeName(const BatchNumber, TotalNumber, Number, KeepCodeNameValueCount: Cardinal);
    procedure UploadCodeName;
  end;

  TNotifyCallback = class(TInterfacedCallback, INotifyCallback)
  protected
    fID: string;
    procedure SyncCodeName;
    procedure GroupCodeName(const BatchNumber, ConsumerCount, Number, KeepCodeNameValueCount: Cardinal);
    procedure UploadCodeName;
  public
    constructor Create(aRest: TSQLRest; const aGUID: TGUID);
  published
    property ID: string read fID;
  end;

  IService = interface(IServiceWithCallbackReleased)
    ['{C92DCBEA-C680-40BD-8D9C-3E6F2ED9C9CF}']
    procedure RegisterConsumer(const Consumer: INotifyCallback; const ID, Address: string);
    procedure SyncCodeNameFinish(const ID: string);
    procedure GroupCodeNameFinish(const ID: string);
    procedure UploadCodeNameFinish(const ID: string);
  end;

  TService = class(TInterfacedObject, IService)
  public
    procedure RegisterConsumer(const Consumer: INotifyCallback; const ID, Address: string);
    procedure SyncCodeNameFinish(const ID: string);
    procedure GroupCodeNameFinish(const ID: string);
    procedure UploadCodeNameFinish(const ID: string);
    procedure CallbackReleased(const Callback: IInvokable; const InterfaceName: RawUTF8);
  end;

  TRange = record
    Value: Integer;
    Value2: Integer;
    function Between(v: Integer): Boolean;
  end;

  TCompareMode = (cmNone, cmVert, cmSlant, cmVertSlant);
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
    fGroupCount: Byte;
    fKeepCodeNameValueCount: Word;
    fGroupNumber: Byte;
    fGroupNumber2: Byte;
    fGroupFirstNumberCount: Cardinal;
    fGroupCountEachFirstNumber: Cardinal;
    fGroupNumber3: Byte;
    fGroupNumber4: Byte;
    fValidityCountEachGroupNumber: TArray<TRange>;
    fExportCodeNameValueCount: Word;
    fExportCodeNameValueCount2: Word;
    fExportLite: Boolean;
    fExportSource: Boolean;
    fExportFile: Boolean;
    procedure SetCompareMode(Value: TCompareMode);
    procedure SetIntervalValues(Value: TWordDynArray);
    procedure SetVertCompareSpacing(Value: Word);
    procedure SetSlantCompareSpacing(Value: Word);
    procedure BuildCompareSpacing;
  public
    procedure BuildValidityCountEachGroupNumber;
    procedure AddValidityCountEachGroupNumber(v, v2: Word);
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
    property GroupCount: Byte read fGroupCount write fGroupCount;
    property KeepCodeNameValueCount: Word read fKeepCodeNameValueCount write fKeepCodeNameValueCount;
    property GroupNumber: Byte read fGroupNumber write fGroupNumber;
    property GroupNumber2: Byte read fGroupNumber2 write fGroupNumber2;
    property GroupFirstNumberCount: Cardinal read fGroupFirstNumberCount write fGroupFirstNumberCount;
    property GroupCountEachFirstNumber: Cardinal read fGroupCountEachFirstNumber write fGroupCountEachFirstNumber;
    property GroupNumber3: Byte read fGroupNumber3 write fGroupNumber3;
    property GroupNumber4: Byte read fGroupNumber4 write fGroupNumber4;
    property ValidityCountEachGroupNumber: TArray<TRange> read fValidityCountEachGroupNumber write fValidityCountEachGroupNumber;
    property ExportCodeNameValueCount: Word read fExportCodeNameValueCount write fExportCodeNameValueCount;
    property ExportCodeNameValueCount2: Word read fExportCodeNameValueCount2 write fExportCodeNameValueCount2;
    property ExportLite: Boolean read fExportLite write fExportLite;
    property ExportSource: Boolean read fExportSource write fExportSource;
    property ExportFile: Boolean read fExportFile write fExportFile;
  end;

  TDataComputer = class(TThread)
  private type
    TConsumer = class
    protected
      fID: string;
      fValue: INotifyCallback;
    published
      property ID: string read fID write fID;
      property Value: INotifyCallback read fValue write fValue;
    end;
  private
    fStopwatch: TStopwatch;
    fLock: TObject;
    fOnFinish: TNotifyEvent;
    fOnError: TNotifyEvent;
    fErrorMessage: string;
    fExportDirectory: string;
    fExportDirectory2: string;

    fRowCount: Word;
    fLastComapreRow: Word;
    fBuildCodeNameFirstRow: Word;
    fLastGroupNumber: Byte;
    fBatchNumber: Byte;
    fBatchNumberRowCount: Cardinal;
    fFirstNumber: Cardinal;
    fGroupCount: Cardinal;
    fFirstRowUpdated: Boolean;
    fBuildFirstRowValueCount: Word;
    fBuildFirstRowValueCount2: Word;

    fConsumers: TThreadList<TConsumer>;
    fFinishCount: Integer;

    fLock2: TObject;
    fCodeName: TSQLCodeName;

    fCacheDB: TSQLRestServerDB;

    procedure RestoreState;
    procedure LoadRow;
    function VertCompareRow(r, r2: TSQLRow): Boolean;
    function SlantCompareRow(r, r2: TSQLRow; CompareData: TSQLCompareData; Offset: Integer): Boolean;
    procedure CompareRow;
    procedure BuildCodeName;

    procedure AddConsumer(Consumer: INotifyCallback; ID, Address: string);
    procedure ClearConsumers;
    function FindConsumer(ID: string): TConsumer;
    procedure ConsumerSyncCodeName;
    procedure ConsumerGroupCodeName;
    procedure ConsumerUploadCodeName;
    procedure SyncCodeNameFinish(const ID: string);
    procedure GroupCodeNameFinish(const ID: string);
    procedure UploadCodeNameFinish(const ID: string);
    procedure ConsumerGroupCodeName2;
    procedure ConsumerSaveCodeName(CodeName: TSQLCodeName);
    procedure GroupCodeName;

    procedure MergeFirstRow;
    procedure UpdateFirstRow;

    function RebuildFile(RowCount: Cardinal): Boolean;
    function NumberToString(Value: Cardinal): string;
    function RebuildFileName(Sender: TFileWriter): string;
    function RebuildFileName2(Sender: TFileWriter): string; overload;
    function RebuildFileName2(Sender: TFileWriter; aRowNumber: Cardinal): string; overload;
    function CodeNameToString(CodeName: string): string; overload;
    function CodeNameToString(CodeName: TWordDynArray): string; overload;
    procedure SaveToFile(BatchNumber: Byte);
    procedure SaveToFile2;
    procedure SaveToFile3;
    procedure ExportData;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
  published
    property Stopwatch: TStopwatch read fStopwatch;
    property Lock: TObject read fLock;
    property OnFinish: TNotifyEvent read fOnFinish write fOnFinish;
    property OnError: TNotifyEvent read fOnError write fOnError;
    property ErrorMessage: string read fErrorMessage;
    property BatchNumber: Byte read fBatchNumber;
    property BatchNumberRowCount: Cardinal read fBatchNumberRowCount;
    property FirstNumber: Cardinal read fFirstNumber;
    property GroupCount: Cardinal read fGroupCount;
  end;

  TGroupCodeName = class(TThread)
  public type
    TThreadTask = (ttWait, ttSyncCodeName, ttGroupCodeName, ttUploadCodeName);
  private
    fLock: TObject;
    fTask: TThreadTask;
    fClient: TSQLHttpClientWebsockets;
    fDatabase: TSQLRestServerDB;
    fCodeName: TSQLCodeName;
    fCodeName2: TSQLCodeName;
    fBatchNumber: Byte;
    fConsumerCount: Word;
    fNumber: Word;
    fKeepCodeNameValueCount: Word;
    fCodeNameDeltaCount: Cardinal;
    procedure SyncCodeName;
    procedure Group; overload;
    procedure Group(RowCount, RowNumber: Int64); overload;
    procedure UploadCodeName;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    procedure StartWork;
    procedure Stop;
  published
    property Client: TSQLHttpClientWebsockets read fClient write fClient;
    property Task: TThreadTask read fTask write fTask;
    property BatchNumber: Byte read fBatchNumber write fBatchNumber;
    property ConsumerCount: Word read fConsumerCount write fConsumerCount;
    property Number: Word read fNumber write fNumber;
    property KeepCodeNameValueCount: Word read fKeepCodeNameValueCount write fKeepCodeNameValueCount;
  end;

  TGroupCodeName2 = class(TThread)
  private
    fFileName: string;
    fCacheDB: TSQLRestServerDB;
    fCodeName: TSQLCodeName;
    fCodeName2: TSQLCodeName;
    fBatchNumber: Byte;
    fRowCount: Int64;
    fConsumerCount: Word;
    fNumber: Word;
    fKeepCodeNameValueCount: Word;
    procedure Group(RowCount, RowNumber: Int64); overload;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    procedure SyncCodeName;
  published
    property BatchNumber: Byte read fBatchNumber write fBatchNumber;
    property RowCount: Int64 read fRowCount write fRowCount;
    property ConsumerCount: Word read fConsumerCount write fConsumerCount;
    property Number: Word read fNumber write fNumber;
    property KeepCodeNameValueCount: Word read fKeepCodeNameValueCount write fKeepCodeNameValueCount;
  end;

const
  EachFileRowCount: Cardinal = 1000000;
  EachFileRowNumber: Cardinal = 200000;
  EachPageRowCount: Word = 10000;

var
  fSettings: TSettings;
  fDataComputer: TDataComputer;
  fDatabase: TSQLRestServerDB;
  fKeyValue: TSQLKeyValue;

  fLocalIP: string;
  fServer: TSQLHttpServer;
  fClient: TSQLHttpClientWebsockets;
  fNotifyCallback: INotifyCallback;
  fService: IService;

  fGroupCodeName: TGroupCodeName;

procedure ReadSettings;
procedure WriteSettings;
procedure ConnectServer(Address, Port: string);
procedure DisconnectServer;

implementation

procedure Int64DynArrayEquals(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); cdecl;
var
  Blob: Pointer;
  i, Count, ElemSize, SameCount: integer;
  v: TArray<string>;
begin
  if not CheckNumberOfArgs(Context, 2, argc) then Exit;
  Blob := sqlite3.value_blob(argv[0]);
  if Blob <> nil then
  begin
    Blob := SimpleDynArrayLoadFrom(Blob, sqlite3.user_data(Context), Count, ElemSize);
    if Blob <> nil then
    begin
      v := UTF8ToString(sqlite3.value_text(argv[1])).Split(['、']);
      if Length(v) <> Count then Exit;

      SameCount := 0;
      for i := 0 to Count - 1 do
      begin
        case ElemSize of
          1: if PByteArray(Blob)^[i] = v[i].ToInt64 then Inc(SameCount);
          2: if PWordArray(Blob)^[i] = v[i].ToInt64 then Inc(SameCount);
          4: if PIntegerArray(Blob)^[i] = v[i].ToInt64 then Inc(SameCount);
          8: if PInt64Array(Blob)^[i] = v[i].ToInt64 then Inc(SameCount);
        end;
      end;
      if SameCount = Count then
        sqlite3.result_int64(Context, Int64(True));
    end;
  end;
end;

procedure TSQLFirstRow.ClearCodeName;
begin
  fCodeName.Clear;
  fCodeNameValueCount := 0;
end;

procedure TSQLFirstRow.AddCodeName(aCodeName: Word);
begin
  fCodeName.Add(aCodeName);
  fCodeNameValueCount := Length(fCodeName);
end;

procedure TSQLCodeName.ClearFirstRow;
begin
  fFirstRow.Clear;
  fFirstRowValueCount := 0;
end;

function TSQLCodeName.MergeFirstRow(aFirstRow: TWordDynArray): Boolean;
var
  v: Word;
  Changed: Boolean;
begin
  Result := False;
  for v in aFirstRow do
    if not fFirstRow.Exist(v) then
    begin
      fFirstRow.Add(v);
      Result := True;
    end;
  if Result then
  begin
    fFirstRowValueCount := Length(fFirstRow);

    TSortAlgorithm.Shell(fFirstRow,
    function(x, x2: Word): Boolean
    begin
      Result := x > x2;
    end);
  end;
end;

procedure TSQLCodeName.Intersection(aCodeName: string);
var
  t, t2: TWordDynArray;
  v: Word;
begin
  t.Clear;
  t2.Assign(aCodeName);
  for v in ToWordDynArray do if t2.Exist(v) then t.Add(v);

  fValue := t.ToString;
  fValueCount := Length(t);
end;

function TSQLCodeName.ToWordDynArray: TWordDynArray;
begin
  Result.Assign(fValue);
end;

constructor TNotifyCallback.Create(aRest: TSQLRest; const aGUID: TGUID);
begin
  inherited Create(aRest, aGUID);
  fID := TGUID.NewGuid.ToString;
end;

procedure TNotifyCallback.SyncCodeName;
begin
  fGroupCodeName.Task := ttSyncCodeName;
  fGroupCodeName.StartWork;
end;

procedure TNotifyCallback.GroupCodeName(const BatchNumber, ConsumerCount, Number, KeepCodeNameValueCount: Cardinal);
begin
  fGroupCodeName.Task := ttGroupCodeName;
  fGroupCodeName.BatchNumber := BatchNumber;
  fGroupCodeName.ConsumerCount := ConsumerCount;
  fGroupCodeName.Number := Number;
  fGroupCodeName.KeepCodeNameValueCount := KeepCodeNameValueCount;
  fGroupCodeName.StartWork;
end;

procedure TNotifyCallback.UploadCodeName;
begin
  fGroupCodeName.Task := ttUploadCodeName;
  fGroupCodeName.StartWork;
end;

procedure TService.RegisterConsumer(const Consumer: INotifyCallback; const ID, Address: string);
begin
  fDataComputer.AddConsumer(Consumer, ID, Address);
end;

procedure TService.SyncCodeNameFinish(const ID: string);
begin
  fDataComputer.SyncCodeNameFinish(ID);
end;

procedure TService.GroupCodeNameFinish(const ID: string);
begin
  fDataComputer.GroupCodeNameFinish(ID);
end;

procedure TService.UploadCodeNameFinish(const ID: string);
begin
  fDataComputer.UploadCodeNameFinish(ID);
end;

procedure TService.CallbackReleased(const Callback: IInvokable; const InterfaceName: RawUTF8);
begin
  //if InterfaceName = 'INotifyCallback' then
  //  InterfaceArrayDelete(fConsumers, Callback);
end;

function TRange.Between(v: Integer): Boolean;
begin
  Result := (v >= Value) and (v <= Value2);
end;

procedure TSettings.SetCompareMode(Value: TCompareMode);
begin
  fCompareMode := Value;
  case fCompareMode of
    cmVert: fCompareModeString := '直';
    cmSlant: fCompareModeString := '斜';
    cmVertSlant: fCompareModeString := '直、斜';
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

procedure TSettings.BuildValidityCountEachGroupNumber;
var
  i: Integer;
begin
  SetLength(fValidityCountEachGroupNumber, 0);
  if fGroupNumber = 0 then Exit;
  SetLength(fValidityCountEachGroupNumber, fGroupNumber4 - fGroupNumber3 + 1);
  for i := Low(fValidityCountEachGroupNumber) to High(fValidityCountEachGroupNumber) do
  begin
    fValidityCountEachGroupNumber[i].Value := -1;
    fValidityCountEachGroupNumber[i].Value2 := -1;
  end;
end;

procedure TSettings.AddValidityCountEachGroupNumber(v, v2: Word);
var
  i: Integer;
begin
  i := Length(fValidityCountEachGroupNumber);
  SetLength(fValidityCountEachGroupNumber, i + 1);
  fValidityCountEachGroupNumber[i].Value := v;
  fValidityCountEachGroupNumber[i].Value2 := v2;
end;

constructor TDataComputer.Create;
begin
  inherited Create(True);
  fLock := TObject.Create;
  fLock2 := TObject.Create;
  fConsumers := TThreadList<TConsumer>.Create;
  fCodeName := TSQLCodeName.Create;
  RestoreState;
end;

destructor TDataComputer.Destroy;
begin
  fLock.Free;
  fLock2.Free;
  ClearConsumers;
  fConsumers.Free;
  fCodeName.Free;
  inherited Destroy;
end;

procedure TDataComputer.RestoreState;
var
  v: Variant;
  s: string;
begin
  fRowCount := 0;
  s := fDatabase.OneFieldValue(TSQLRow, 'Max(Number)', '', []);
  if not s.IsEmpty then fRowCount := s.ToInteger;

  fLastComapreRow := 0;
  fKeyValue.GetKeyValue('LastComapreRow', v);
  if not VarIsEmpty(v) then fLastComapreRow := v;

  fBuildCodeNameFirstRow := 0;
  fKeyValue.GetKeyValue('BuildCodeNameFirstRow', v);
  if not VarIsEmpty(v) then fBuildCodeNameFirstRow := v;

  fLastGroupNumber := 0;
  fKeyValue.GetKeyValue('LastGroupNumber', v);
  if not VarIsEmpty(v) then fLastGroupNumber := v;

  fFirstRowUpdated := False;
  fKeyValue.GetKeyValue('FirstRowUpdated', v);
  if not VarIsEmpty(v) then fFirstRowUpdated := v;

  fBuildFirstRowValueCount := 0;
  fKeyValue.GetKeyValue('BuildFirstRowValueCount', v);
  if not VarIsEmpty(v) then fBuildFirstRowValueCount := v;

  fBuildFirstRowValueCount2 := 0;
  fKeyValue.GetKeyValue('BuildFirstRowValueCount2', v);
  if not VarIsEmpty(v) then fBuildFirstRowValueCount2 := v;
end;

procedure TDataComputer.LoadRow;
var
  i, Digit: Integer;
  s: string;
  Row: TSQLRow;
begin
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
  FirstRow: TSQLFirstRow;
  CompareData: TSQLCompareData;
  i: Integer;
  Number, CodeName: Word;
  s: string;
begin
  TSQLRow.AutoFree(Row, fDatabase, 'Number > ? ORDER BY Number', [fLastComapreRow - fSettings.CompareSpacing]);
  TSQLRow.AutoFree(Row2);
  TSQLFirstRow.AutoFree(FirstRow);
  TSQLCompareData.AutoFree(CompareData);

  fDatabase.TransactionBegin(TSQLFirstRow);
  try
    while Row.FillOne and not Terminated do
    begin
      if Row.FillCurrentRow - 1 <= fSettings.CompareSpacing then Continue;
      fLastComapreRow := Row.Number;

      FirstRow.Value := Row.Number;
      FirstRow.ClearCodeName;
      FirstRow.Grouped := False;
      for i := Row.FillCurrentRow - 2 downto Row.FillCurrentRow - 1 - fSettings.CompareSpacing do
      begin
        Row.FillRow(i, Row2);
        Number := Row.FillCurrentRow - 1 - i;
        //直连
        if Number <= fSettings.VertCompareSpacing then
        begin
          if VertCompareRow(Row, Row2)
          then
          begin
            if fSettings.CompareMode = cmVert then CodeName := Number
            else CodeName := Number * 3 - 2;

            FirstRow.AddCodeName(CodeName);
          end;
        end;
        //斜连
        if Number <= fSettings.SlantCompareSpacing then
        begin
          //右斜连
          if SlantCompareRow(Row, Row2, CompareData, Number)
          then
          begin
            if fSettings.CompareMode = cmSlant then CodeName := Number * 2 - 1
            else CodeName := Number * 3 - 1;

            FirstRow.AddCodeName(CodeName);
          end;
          //左斜连
          if SlantCompareRow(Row, Row2, CompareData, -Number)
          then
          begin
            if fSettings.CompareMode = cmSlant then CodeName := Number * 2
            else CodeName := Number * 3;

            FirstRow.AddCodeName(CodeName);
          end;
        end;
      end;
     fDatabase.Add(FirstRow, True);

    end;
    fDatabase.Commit(1, True);

    fKeyValue.SetKeyValue('LastComapreRow', fLastComapreRow);
  except
    on e: Exception do
    begin
      fDatabase.RollBack;
      raise Exception.Create(e.Message);
    end;
  end;
end;

procedure TDataComputer.BuildCodeName;
var
  FirstRow: TSQLFirstRow;
  CodeName: TSQLCodeName;
  i: Integer;
begin
  TSQLFirstRow.AutoFree(FirstRow, fDatabase, 'Value > ? ORDER BY Value', [fBuildCodeNameFirstRow]);
  TSQLCodeName.AutoFree(CodeName);
  fDatabase.TransactionBegin(TSQLCodeName);
  try
    while FirstRow.FillOne and not Terminated do
    begin
      CodeName.FillPrepare(fDatabase, 'BatchNumber = 0 AND Value = ?', [FirstRow.CodeName.ToString]);
      if CodeName.FillOne then
      begin
        CodeName.MergeFirstRow([FirstRow.Value]);
        CodeName.FirstRowValueCount := Length(CodeName.FirstRow);
        fDatabase.Update(CodeName);
      end
      else
      begin
        CodeName.Value := FirstRow.CodeName.ToString;
        CodeName.ValueCount := Length(CodeName.Value);
        CodeName.BatchNumber := 0;
        CodeName.BatchNumberRepeat := False;
        CodeName.FirstRow := [FirstRow.Value];
        CodeName.FirstRowValueCount := Length(CodeName.FirstRow);
        fDatabase.Add(CodeName, True);
      end;
    end;
    if Terminated then Exit;

    fDatabase.Commit(1, True);
    if FirstRow.FillTable.RowCount > 0 then
    begin
      fBuildCodeNameFirstRow := FirstRow.Value;
      fKeyValue.SetKeyValue('BuildCodeNameFirstRow', fBuildCodeNameFirstRow);
    end;
  except
    on e: Exception do
    begin
      fDatabase.RollBack;
      raise Exception.Create(e.Message);
    end;
  end;
end;

procedure TDataComputer.AddConsumer(Consumer: INotifyCallback; ID, Address: string);
var
  t: TConsumer;
begin
  t := TConsumer.Create;
  t.Value := Consumer;
  t.ID := ID;
  fConsumers.Add(t);
end;

procedure TDataComputer.ClearConsumers;
var
  i: Integer;
begin
  with fConsumers.LockList do
  begin
    try
      for i := Count - 1 downto 0 do Items[i].Free;
    finally
      fConsumers.UnlockList;
    end
  end;
end;

function TDataComputer.FindConsumer(ID: string): TConsumer;
var
  i: Integer;
begin
  Result := nil;
  with fConsumers.LockList do
  begin
    try
      for i := 0 to Count - 1 do
        if Items[i].ID = ID then
        begin
          Result := Items[i];
          Break;
        end;
    finally
      fConsumers.UnlockList;
    end
  end;
end;

procedure TDataComputer.ConsumerSyncCodeName;
var
  i, ConsumerCount: Integer;
begin
  fFinishCount := 0;
  with fConsumers.LockList do
  begin
    try
      ConsumerCount := Count;
      for i := 0 to Count - 1 do Items[i].Value.SyncCodeName;
    finally
      fConsumers.UnlockList;
    end
  end;
  repeat
    Sleep(1000);
  until fFinishCount = ConsumerCount;
end;

procedure TDataComputer.ConsumerGroupCodeName;
var
  i, ConsumerCount: Integer;
begin
  fFinishCount := 0;
  with fConsumers.LockList do
  begin
    try
      ConsumerCount := Count;
      for i := 0 to Count - 1 do
        Items[i].Value.GroupCodeName(fBatchNumber, Count, i + 1, fSettings.KeepCodeNameValueCount);
    finally
      fConsumers.UnlockList;
    end
  end;
  repeat
    Sleep(1000);
  until fFinishCount = ConsumerCount;
end;

procedure TDataComputer.ConsumerUploadCodeName;
var
  i: Integer;
begin
  with fConsumers.LockList do
  begin
    try
      for i := 0 to Count - 1 do
      begin
        fFinishCount := 0;
        Items[i].Value.UploadCodeName;
        repeat
          Sleep(1000);
        until fFinishCount = 1;
      end;
    finally
      fConsumers.UnlockList;
    end
  end;
end;

procedure TDataComputer.SyncCodeNameFinish(const ID: string);
var
  Consumer: TConsumer;
begin
  Consumer := FindConsumer(ID);
  if not Assigned(Consumer) then Exit;

  TInterlocked.Increment(fFinishCount);
end;

procedure TDataComputer.GroupCodeNameFinish(const ID: string);
var
  Consumer: TConsumer;
begin
  Consumer := FindConsumer(ID);
  if not Assigned(Consumer) then Exit;

  TInterlocked.Increment(fFinishCount);
end;

procedure TDataComputer.UploadCodeNameFinish(const ID: string);
var
  Consumer: TConsumer;
begin
  //Consumer := FindConsumer(ID);
  //if not Assigned(Consumer) then Exit;

  TInterlocked.Increment(fFinishCount);
end;

procedure TDataComputer.ConsumerGroupCodeName2;
var
  Consumers: array of TGroupCodeName2;
  i, PageIndex: Integer;
  RowCount: Int64;
  CodeName: TSQLCodeName;
begin
  RowCount := fDatabase.TableRowCount(TSQLCodeName);
  fDatabase.TransactionBegin(TSQLCodeName);
  try
    SetLength(Consumers, 5);
    for i := Low(Consumers) to High(Consumers) do
    begin
      Consumers[i] := TGroupCodeName2.Create;
      Consumers[i].BatchNumber := fBatchNumber;
      Consumers[i].RowCount := RowCount;
      Consumers[i].ConsumerCount := Length(Consumers);
      Consumers[i].Number := i + 1;
      Consumers[i].KeepCodeNameValueCount := fSettings.KeepCodeNameValueCount;
      Consumers[i].SyncCodeName;
    end;
    for i := Low(Consumers) to High(Consumers) do Consumers[i].Start;
    for i := Low(Consumers) to High(Consumers) do
    begin
      Consumers[i].WaitFor;
      Consumers[i].Free;
    end;
    fDatabase.Commit(1, True);
  except
    fDatabase.RollBack;
    raise;
  end;
  if Terminated then Exit;

  TSQLCodeName.AutoFree(CodeName);
  PageIndex := 0;
  repeat
    CodeName.FillPrepare(fDatabase, 'BatchNumberRepeat = 1 LIMIT ? OFFSET ?',
      [EachPageRowCount, PageIndex * EachPageRowCount]);
    fDatabase.TransactionBegin(TSQLCodeName);
    try
      while CodeName.FillOne and not Terminated do
      begin
        CodeName.BatchNumber := fBatchNumber;
        CodeName.BatchNumberRepeat := False;
        fDatabase.Update(CodeName);
      end;
      fDatabase.Commit(1, True);
    except
      fDatabase.RollBack;
      raise;
    end;
    Inc(PageIndex);
  until Terminated or (CodeName.FillTable.RowCount = 0);
  if Terminated then Exit;

  fDatabase.Delete(TSQLCodeName, 'BatchNumber < ?', [fBatchNumber]);
end;

procedure TDataComputer.ConsumerSaveCodeName(CodeName: TSQLCodeName);
begin
  TMonitor.Enter(fLock2);
  try
    fCodeName.FillPrepare(fDatabase, 'Value = ?', [CodeName.Value]);
    if fCodeName.FillOne then
    begin
      if fCodeName.BatchNumber <> fBatchNumber then
      begin
        fCodeName.BatchNumberRepeat := True;
        fDatabase.Update(fCodeName);
      end;
      if fCodeName.MergeFirstRow(CodeName.FirstRow) then
      begin
        fDatabase.Update(fCodeName);
      end;
    end
    else
    begin
      CodeName.BatchNumber := fBatchNumber;
      CodeName.BatchNumberRepeat := False;
      fDatabase.Add(CodeName, True);
    end;
  finally
    TMonitor.Exit(fLock2);
  end;
end;

procedure TDataComputer.GroupCodeName;
var
  CodeName, CodeName2: TSQLCodeName;
  s: string;
  i, j, PageIndex, GroupCountEachFirstNumber, FirstRowCount: Integer;
  ValidityFirstRowCount: TRange;
  v: Variant;
  Grouped: Boolean;
begin
  TSQLCodeName.AutoFree(CodeName);
  TSQLCodeName.AutoFree(CodeName2);

  {CodeName.BatchNumber := 0;
  CodeName.FirstRowValueCount := 1;
  CodeName.FirstRow := [501];
  CodeName.Value := [1, 2, 4, 6, 7, 8, 11];
  CompareData.ValueCount := 7;
  fDatabase.Add(CodeName, True);
  CodeName.FirstRow := [502];
  CodeName.Value := [2, 4, 5, 8];
  CodeName.ValueCount := 4;
  fDatabase.Add(CodeName, True);
  CodeName.FirstRow := [503];
  CodeName.Value := [1, 2, 4, 6, 10, 11];
  CodeName.ValueCount := 6;
  fDatabase.Add(CodeName, True);
  CodeName.FirstRow := [504];
  CodeName.Value := [2, 3, 4, 7, 9, 10];
  CodeName.ValueCount := 6;
  fDatabase.Add(CodeName, True);
  CodeName.FirstRow := [505];
  CodeName.Value := [1, 2, 4, 5, 6, 8, 10];
  CodeName.ValueCount := 7;
  fDatabase.Add(CodeName, True); }

  for i := fLastGroupNumber + 1 to fSettings.GroupCount do
  begin
    fBatchNumber := i;
    fDatabase.Delete(TSQLCodeName, 'BatchNumber = ?', [fBatchNumber]);

    GroupCountEachFirstNumber := 0;
    if (fBatchNumber >= fSettings.GroupNumber) and (fBatchNumber <= fSettings.GroupNumber2) then
      GroupCountEachFirstNumber := fSettings.GroupCountEachFirstNumber;

    ValidityFirstRowCount.Value := -1;
    if (fBatchNumber >= fSettings.GroupNumber3) and (fBatchNumber <= fSettings.GroupNumber4) then
      ValidityFirstRowCount := fSettings.ValidityCountEachGroupNumber[fBatchNumber - fSettings.GroupNumber];

    fBatchNumberRowCount := fDatabase.TableRowCount(TSQLCodeName);
    fDatabase.TransactionBegin(TSQLCodeName);
    try
      for j := 1 to fBatchNumberRowCount - 1 do
      begin
        if Terminated then Exit;
        fFirstNumber := j;

        Foreach(fBatchNumberRowCount, 2, fFirstNumber,
        procedure(FirstNumber: Cardinal; FirstRowIndexs: TWordDynArray)
        var
          s: string;
        begin
          s := 'BatchNumber = ? LIMIT 1 OFFSET ?';
          CodeName.FillPrepare(fDatabase, s, [fBatchNumber - 1, FirstRowIndexs[0] - 1]);
          CodeName.FillOne;
          CodeName2.FillPrepare(fDatabase, s, [fBatchNumber - 1, FirstRowIndexs[1] - 1]);
          CodeName2.FillOne;

          CodeName.Intersection(CodeName2.Value);
          if CodeName.ValueCount = 0 then Exit;
          if (fSettings.KeepCodeNameValueCount > 0) and (CodeName.ValueCount < fSettings.KeepCodeNameValueCount) then Exit;
          //相同首行个数是否在有效范围内
          FirstRowCount := CodeName.FirstRowValueCount;
          CodeName.MergeFirstRow(CodeName2.FirstRow);
          if ValidityFirstRowCount.Value > -1 then
          begin
            FirstRowCount := FirstRowCount * 2 - CodeName.FirstRowValueCount;
            if not ValidityFirstRowCount.Between(FirstRowCount) then Exit;
          end;

          CodeName2.FillPrepare(fDatabase, 'Value = ?', [CodeName.Value]);
          if CodeName2.FillOne then
          begin
            if CodeName2.BatchNumber <> fBatchNumber then
            begin
              CodeName2.BatchNumberRepeat := True;
              fDatabase.Update(CodeName2);
            end;
            if CodeName2.MergeFirstRow(CodeName.FirstRow) then
              fDatabase.Update(CodeName2);
          end
          else
          begin
            CodeName.BatchNumber := i;
            fDatabase.Add(CodeName, True);
          end;
        end,
        function(FirstNumber, GroupCount: Cardinal): Boolean
        begin
          fGroupCount := GroupCount;
          Result := Terminated;
          if not Result and (fSettings.GroupFirstNumberCount > 0) then
            Result := FirstNumber > fSettings.GroupFirstNumberCount;
          if not Result and (GroupCountEachFirstNumber > 0) then
            Result := GroupCount >= GroupCountEachFirstNumber;
        end);
      end;


      {Foreach(RowCount, 2,
      procedure(FirstRowIndexs: TWordDynArray)
      begin
        s := 'BatchNumber = ? LIMIT 1 OFFSET ?';
        CodeName.FillPrepare(fDatabase, s, [i - 1, FirstRowIndexs[0] - 1]);
        CodeName.FillOne;
        CodeName2.FillPrepare(fDatabase, s, [i - 1, FirstRowIndexs[1] - 1]);
        CodeName2.FillOne;
        CodeName.Intersection(CodeName2.Value);
        if (CodeName.ValueCount > 0)
          and (CodeName.ValueCount >= fSettings.KeepCodeNameValueCount)
        then
        begin
          CodeName.MergeFirstRow(CodeName2.FirstRow);

          CodeName2.FillPrepare(fDatabase, 'Value = ?', [CodeName.Value]);
          if CodeName2.FillOne then
          begin
            if CodeName2.BatchNumber <> fBatchNumber then
            begin
              CodeName2.BatchNumberRepeat := True;
              fDatabase.Update(CodeName2);
            end;
            if CodeName2.MergeFirstRow(CodeName.FirstRow) then
              fDatabase.Update(CodeName2);
          end
          else
          begin
            CodeName.BatchNumber := i;
            fDatabase.Add(CodeName, True);
          end;
        end;
      end,
      function: Boolean
      begin
        Result := Terminated;
      end);
      if Terminated then Exit; }

      fDatabase.Commit(1, True);
    except
      on e: Exception do
      begin
        fDatabase.RollBack;
        raise Exception.Create(e.Message);
      end;
    end;
    if Terminated then Exit;

    PageIndex := 0;
    repeat
      CodeName.FillPrepare(fDatabase, 'BatchNumberRepeat = 1 LIMIT ? OFFSET ?',
        [EachPageRowCount, PageIndex * EachPageRowCount]);
      fDatabase.TransactionBegin(TSQLCodeName);
      try
        while CodeName.FillOne and not Terminated do
        begin
          CodeName.BatchNumber := fBatchNumber;
          CodeName.BatchNumberRepeat := False;
          fDatabase.Update(CodeName);
        end;
        fDatabase.Commit(1, True);
      except
        fDatabase.RollBack;
        raise;
      end;
      Inc(PageIndex);
    until Terminated or (CodeName.FillTable.RowCount = 0);
    if Terminated then Exit;

    fDatabase.Delete(TSQLCodeName, 'BatchNumber < ?', [fBatchNumber]);
    if Terminated then Exit;

    {ConsumerSyncCodeName;
    ConsumerGroupCodeName;
    ConsumerUploadCodeName;}

    //ConsumerGroupCodeName2;

    fLastGroupNumber := fBatchNumber;
    fKeyValue.SetKeyValue('LastGroupNumber', fLastGroupNumber);
    if fLastGroupNumber = 1 then
      fDatabase.Execute('UPDATE FirstRow SET Grouped = 1');
    if fLastGroupNumber = fSettings.GroupCount then
    begin
      fFirstRowUpdated := False;
      fKeyValue.SetKeyValue('FirstRowUpdated', fFirstRowUpdated);
    end;

    fDatabase.Delete(TSQLCodeName, 'BatchNumber = ?', [i - 2]);
  end;
end;

procedure TDataComputer.MergeFirstRow;
var
  FirstRow: TSQLFirstRow;
  CodeName: TSQLCodeName;
  PageIndex: Integer;
  s: string;
begin
  TSQLFirstRow.AutoFree(FirstRow, fDatabase, 'Grouped = 0', []);
  if FirstRow.FillTable.RowCount = 0 then Exit;
  TSQLCodeName.AutoFree(CodeName);

  s := 'LIMIT ? OFFSET ?';
  PageIndex := 0;
  repeat
    CodeName.FillPrepare(fDatabase, s, [EachPageRowCount, PageIndex * EachPageRowCount]);
    fDatabase.TransactionBegin(TSQLCodeName);
    try
      while CodeName.FillOne and not Terminated do
      begin
        FirstRow.FillRewind;
        while FirstRow.FillOne and not Terminated do
        begin
          if FirstRow.CodeName.Contains(CodeName.ToWordDynArray) then
          begin
            CodeName.MergeFirstRow([FirstRow.Value]);
            fDatabase.Update(CodeName);
          end;
        end;
      end;
      fDatabase.Commit(1, True);
    except
      on e: Exception do
      begin
        fDatabase.RollBack;
        raise Exception.Create(e.Message);
      end;
    end;
    Inc(PageIndex);
  until CodeName.FillTable.RowCount = 0;

  fFirstRowUpdated := False;
  fKeyValue.SetKeyValue('FirstRowUpdated', fFirstRowUpdated);

  fDatabase.TransactionBegin(TSQLFirstRow);
  try
    FirstRow.FillRewind;
    while FirstRow.FillOne and not Terminated do
    begin
      FirstRow.Grouped := True;
      fDatabase.Update(FirstRow);
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

procedure TDataComputer.UpdateFirstRow;
var
  CodeName: TSQLCodeName;
  FirstRow: TSQLFirstRow;
  FirstRowCodeName: TSQLFirstRowCodeName;
  sPage: string;
  i, PageIndex, LastFirstRowValue: Integer;
begin
  if fFirstRowUpdated
    and fSettings.ExportFile
    and (fBuildFirstRowValueCount = fSettings.ExportCodeNameValueCount)
    and (fBuildFirstRowValueCount2 = fSettings.ExportCodeNameValueCount2)
  then Exit;

  fDatabase.Execute('UPDATE FirstRow SET RowCount = 0');
  fDatabase.Delete(TSQLFirstRowCodeName, '', []);

  TSQLCodeName.AutoFree(CodeName);
  TSQLFirstRow.AutoFree(FirstRow);
  TSQLFirstRowCodeName.AutoFree(FirstRowCodeName);

  sPage := 'ValueCount >= ? AND ValueCount <= ? ORDER BY ValueCount DESC LIMIT ? OFFSET ?';
  PageIndex := 0;
  repeat
    CodeName.FillPrepare(fDatabase, sPage, [fSettings.ExportCodeNameValueCount,
      fSettings.ExportCodeNameValueCount2, EachPageRowCount, PageIndex * EachPageRowCount]);
    fDatabase.TransactionBegin(TSQLFirstRowCodeName);
    try
      while CodeName.FillOne and not Terminated do
      begin
        FirstRowCodeName.CodeName := CodeName.Value;
        FirstRowCodeName.CodeNameValueCount := CodeName.ValueCount;
        for i := Low(CodeName.FirstRow) to High(CodeName.FirstRow) do
        begin
          FirstRow.FillPrepare(fDatabase, 'Value = ?', [CodeName.FirstRow[i]]);
          if FirstRow.FillOne then
          begin
            FirstRow.RowCount := FirstRow.RowCount + 1;
            fDatabase.Update(FirstRow);
          end;

          FirstRowCodeName.FirstRow := CodeName.FirstRow[i];
          fDatabase.Add(FirstRowCodeName, True);
        end;
      end;
      fDatabase.Commit(1, True);
    except
      on e: Exception do
      begin
        fDatabase.RollBack;
        raise Exception.Create(e.Message);
      end;
    end;

    Inc(PageIndex);
  until Terminated or (CodeName.FillTable.RowCount = 0);

  FirstRow.FillPrepare(fDatabase, 'RowCount > 0 ORDER BY Value', []);
  fDatabase.TransactionBegin(TSQLFirstRow);
  try
    while FirstRow.FillOne and not Terminated do
    begin
      if FirstRow.FillCurrentRow = 2 then
        FirstRow.RowSpacing := FirstRow.Value - fSettings.CompareSpacing - 1
      else
        FirstRow.RowSpacing := FirstRow.Value - LastFirstRowValue;
      LastFirstRowValue := FirstRow.Value;

      fDatabase.Update(FirstRow);
    end;
    fDatabase.Commit(1, True);
  except
    on e: Exception do
    begin
      fDatabase.RollBack;
      raise Exception.Create(e.Message);
    end;
  end;

  fFirstRowUpdated := True;
  fBuildFirstRowValueCount := fSettings.ExportCodeNameValueCount;
  fBuildFirstRowValueCount2 := fSettings.ExportCodeNameValueCount2;
  fKeyValue.SetKeyValue('FirstRowUpdated', fFirstRowUpdated);
  fKeyValue.SetKeyValue('BuildFirstRowValueCount', fBuildFirstRowValueCount);
  fKeyValue.SetKeyValue('BuildFirstRowValueCount2', fBuildFirstRowValueCount2);
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
  if i > 0 then Result := Result + Format(' %d 亿', [i]);
  i:= Value div 10000;
  Value := Value mod 10000;
  if i > 0 then Result := Result + Format(' %d 万', [i]);
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

  i := Sender.FileName.IndexOf('）');
  Result := Sender.FileName.SubString(0, i + 1);
  Result := Result + Format('.【最末第%s（空白及文字）行】- %d', [sSub, Sender.FileNo - 1]);
end;

function TDataComputer.RebuildFileName2(Sender: TFileWriter): string;
begin
  Result := RebuildFileName2(Sender, 0);
end;

function TDataComputer.RebuildFileName2(Sender: TFileWriter; aRowNumber: Cardinal): string;
var
  sSub: string;
  RowNumber: Cardinal;
  i: Integer;
begin
  RowNumber := Sender.FileNo * EachFileRowNumber;
  if Sender.LastFileNo then RowNumber := aRowNumber;
  sSub := NumberToString(RowNumber);

  if Sender.FileName.Contains('（2-1）')
    or Sender.FileName.Contains('（6）')
  then i := Sender.FileName.IndexOf('）')
  else i := Sender.FileName.IndexOf('次');
  Result := Sender.FileName.SubString(0, i + 1);
  Result := Result + Format('【最末第%s=行】- %d', [sSub, Sender.FileNo - 1]);
end;

function TDataComputer.CodeNameToString(CodeName: string): string;
var
  t: TWordDynArray;
begin
  t.Assign(CodeName);
  Result := CodeNameToString(t);
end;

function TDataComputer.CodeNameToString(CodeName: TWordDynArray): string;
var
  v: Word;
begin
  Result := '';
  case fSettings.CompareMode of
    cmVert: Result := CodeName.ToString;
    else
    begin
      for v in CodeName do
      begin
        if not Result.IsEmpty then Result := Result + '、';
        case fSettings.CompareMode of
          cmSlant:
          begin
            Result := Result + ((v - 1) div 2 + 1).ToString;
            case v mod 2 of
              1: Result := Result + 'Z';
              else Result := Result + 'Y';
            end;
          end;
          cmVertSlant:
          begin
            Result := Result + ((v - 1) div 3 + 1).ToString;
            case v mod 3 of
              1: ;
              2: Result := Result + 'Z';
              else Result := Result + 'Y';
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TDataComputer.SaveToFile(BatchNumber: Byte);
var
  fr: TFileWriter;
  s, sPage, FileName, TxtFileName: string;
  i, PageIndex: Integer;
  CodeName: TSQLCodeName;
  RowNumber: Cardinal;
begin
  TxtFileName := '0 .［ 各（第 N 行为首行）的（符合条件的代号）］第 %d 次（ 依次遍历 ）导出［ 相同（符合条件的代号）］.txt';
  TxtFileName := Format(TxtFileName, [BatchNumber]);
  FileName := fExportDirectory + TxtFileName;
  fr := TFileWriter.Create(FileName);
  fr.RebuildFileNameEvent := RebuildFileName2;
  try
    s := TPath.GetFileNameWithoutExtension(FileName) + ':';
    fr.WriteLn('');
    fr.WriteLn(s);
    fr.WriteLn('');

    TSQLCodeName.AutoFree(CodeName);
    sPage := 'BatchNumber = ? ORDER BY FirstRowValueCount, ValueCount DESC LIMIT ? OFFSET ?';
    RowNumber := 0;
    PageIndex := 0;
    repeat
      CodeName.FillPrepare(fDatabase, sPage, [BatchNumber, EachPageRowCount, PageIndex * EachPageRowCount]);
      while CodeName.FillOne and not Terminated do
      begin
        RowNumber := RowNumber + 1;
        s := '%d=［（ 第 %s 行为首行 ）不同首行数 ：共 %d 个 ］、［ 相同（代号组合）］共 %d 个 ：%s';
        s := Format(s, [
          RowNumber,
          CodeName.FirstRow.ToString,
          CodeName.FirstRowValueCount,
          CodeName.ValueCount,
          CodeNameToString(CodeName.ToWordDynArray)
        ]);

        fr.WriteLn('');
        fr.WriteLn(s);

        if RowNumber mod EachFileRowNumber = 0 then fr.BuildActiveFileName;
      end;

      Inc(PageIndex);
    until Terminated or (CodeName.FillTable.RowCount = 0);

    fr.WriteFinish;
    fr.RenameLastFile(RebuildFileName2(fr, RowNumber));
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.SaveToFile2;
var
  fr, fr2: TFileWriter;
  s, sPage, FileName, FileName2, TxtFileName: string;
  i, PageIndex: Integer;
  CodeName: TSQLCodeName;
  RowNumber: Cardinal;
begin
  if fSettings.ExportLite then TxtFileName := '（2-1）.txt'
  else
  begin
    TxtFileName := '（2-1）.【排列】【“%d-%d”个[相同组合、不同首行]的组合[不同首行数：最多→少]】（1）.txt';
    TxtFileName := Format(TxtFileName, [fSettings.ExportCodeNameValueCount, fSettings.ExportCodeNameValueCount2]);
  end;
  FileName := fExportDirectory + TxtFileName;
  fr := TFileWriter.Create(FileName);
  fr.RebuildFileNameEvent := RebuildFileName2;

  if fSettings.ExportLite then TxtFileName := '（6）.txt'
  else
  begin
    TxtFileName := '（6）.【简化】【“%d-%d”个[相同组合、不同首行]的组合】.txt';
    TxtFileName := Format(TxtFileName, [fSettings.ExportCodeNameValueCount, fSettings.ExportCodeNameValueCount2]);
  end;
  FileName2 := fExportDirectory2 + TxtFileName;
  fr2 := TFileWriter.Create(FileName2);
  fr2.RebuildFileNameEvent := RebuildFileName2;
  try
    s := TPath.GetFileNameWithoutExtension(FileName) + ':';
    fr.WriteLn('');
    fr.WriteLn(s);
    fr.WriteLn('');
    s := TPath.GetFileNameWithoutExtension(FileName2) + ':';
    fr2.WriteLn('');
    fr2.WriteLn(s);
    fr2.WriteLn('');

    TSQLCodeName.AutoFree(CodeName);
    sPage := 'ValueCount >= ? AND ValueCount <= ? ORDER BY ValueCount DESC LIMIT ? OFFSET ?';
    RowNumber := 0;
    PageIndex := 0;
    repeat
      CodeName.FillPrepare(fDatabase, sPage, [fSettings.ExportCodeNameValueCount,
        fSettings.ExportCodeNameValueCount2, EachPageRowCount, PageIndex * EachPageRowCount]);
      while CodeName.FillOne and not Terminated do
      begin
        RowNumber := RowNumber + 1;

        s := '%d.【“%d”个 [ 相同组合、不同首行]的组合 [ 不同首行数：%d ]】[代号：%s ] ：';
        s := Format(s, [
          RowNumber,
          CodeName.ValueCount,
          Length(CodeName.FirstRow),
          CodeNameToString(CodeName.ToWordDynArray)
        ]);
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn(s);

        s := '%d.[ %d个组合；代号：（%s）]';
        s := Format(s, [
          RowNumber,
          CodeName.ValueCount,
          CodeNameToString(CodeName.ToWordDynArray)
        ]);
        fr2.WriteLn('');
        fr2.WriteLn(s);

        for i := High(CodeName.FirstRow) downto Low(CodeName.FirstRow) do
        begin
          s := '【%d】.第%d行为首行';
          s := Format(s, [Length(CodeName.FirstRow) - i, CodeName.FirstRow[i]]);
          fr.WriteLn('');
          fr.WriteLn(s);

          if i = High(CodeName.FirstRow) then
          begin
            s := '（第%d行为首行）';
            s := Format(s, [CodeName.FirstRow[i]]);
            fr2.WriteLn('');
            fr2.WriteLn(s);
          end;
        end;

        if RowNumber mod EachFileRowNumber = 0 then
        begin
          fr.BuildActiveFileName;
          fr2.BuildActiveFileName;
        end;
      end;

      Inc(PageIndex);
    until Terminated or (CodeName.FillTable.RowCount = 0);

    fr.WriteFinish;
    fr.RenameLastFile(RebuildFileName2(fr, RowNumber));
    fr2.WriteFinish;
    fr2.RenameLastFile(RebuildFileName2(fr2, RowNumber));
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.SaveToFile3;
var
  fr: TFileWriter;
  s, FileName, TxtFileName, sFirstRow: string;
  i, PageIndex: Integer;
  FirstRow: TSQLFirstRow;
  FirstRowCodeName: TSQLFirstRowCodeName;
  RowSpacingList: TStringList;
  RowNumber: Cardinal;

  procedure AddRowSpacing(RowSpacing: Word; s: string);
  var
    sRowSpacing: string;
  begin
    sRowSpacing := RowSpacing.ToString;
    while sRowSpacing.Length < 3 do sRowSpacing := '0' + sRowSpacing;
    if RowSpacingList.IndexOfName(sRowSpacing) = -1 then
    begin
      if (fSettings.CompareMode = cmSlant) and s.Contains('，同行数') then
        s := s.Substring(0, s.IndexOf('，同行数')) + '）';
      RowSpacingList.Values[sRowSpacing] := s;
    end;
  end;
begin
  if fSettings.ExportLite then TxtFileName := '（1）.txt'
  else
  begin
    TxtFileName := '（1）.【排列】【“%d-%d”个以上[相同首行、不同组合]的组合】.txt';
    TxtFileName := Format(TxtFileName, [fSettings.ExportCodeNameValueCount, fSettings.ExportCodeNameValueCount2]);
  end;
  FileName := fExportDirectory + TxtFileName;
  fr := TFileWriter.Create(FileName);
  fr.RebuildFileEvent := RebuildFile;
  fr.RebuildFileNameEvent := RebuildFileName;
  try
    s := TPath.GetFileNameWithoutExtension(FileName) + ':';
    fr.WriteLn('');
    fr.WriteLn(s);
    fr.WriteLn('');

    TSQLFirstRow.AutoFree(FirstRow, fDatabase, 'RowCount > 0 ORDER BY Value DESC', []);
    TSQLFirstRowCodeName.AutoFree(FirstRowCodeName);

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
                sFirstRow := '（第%d行为首行）';
                sFirstRow := Format(sFirstRow, [fRowCount + 1]);
              end;
              s := '1.%s（邻行距 ↓%d）';
              s := Format(s, [sFirstRow, fRowCount + 1 - FirstRow.Value]);
            end;
            else
            begin
              sFirstRow := '';
              if not fSettings.ExportLite then
              begin
                sFirstRow := '[ %s连（第%d行为首行）]；';
                sFirstRow := Format(sFirstRow, [fSettings.CompareModeString, fRowCount + 1]);
              end;
              s := '1.%s[ 邻行距 ↓%d ]';
              s := Format(s, [sFirstRow, fRowCount + 1 - FirstRow.Value]);
            end;
          end;
          AddRowSpacing(fRowCount + 1 - FirstRow.Value, s);
        end;

        case fSettings.CompareMode of
          cmSlant:
          begin
            sFirstRow := '';
            if not fSettings.ExportLite then
            begin
              sFirstRow := '（第%d行为首行）';
              sFirstRow := Format(sFirstRow, [FirstRow.Value]);
            end;
            s := '%d.%s（邻行距 ↓%d，同行数：%d）';
            s := Format(s, [FirstRow.FillCurrentRow, sFirstRow, FirstRow.RowSpacing, FirstRow.RowCount]);
          end;
          else
          begin
            sFirstRow := '';
            if not fSettings.ExportLite then
            begin
              sFirstRow := '[ %s连（第%d行为首行）]；';
              sFirstRow := Format(sFirstRow, [fSettings.CompareModeString, FirstRow.Value]);
            end;
            s := '%d.%s[ 邻行距 ↓%d ]';
            s := Format(s, [FirstRow.FillCurrentRow, sFirstRow, FirstRow.RowSpacing]);
          end;
        end;
        AddRowSpacing(FirstRow.RowSpacing, s);
      end;

      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn('');
      fr.WriteLn('最大-小的邻行距 1-100 行内：');
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
              sFirstRow := '（第%d行为首行）；';
              sFirstRow := Format(sFirstRow, [fRowCount + 1]);
            end;
            s := '1.%s（邻行距 ↓%d）';
            s := Format(s, [sFirstRow, fRowCount + 1 - FirstRow.Value]);
          end;
          else
          begin
            sFirstRow := '';
            if not fSettings.ExportLite then
            begin
              sFirstRow := '[ %s连（第%d行为首行）]；';
              sFirstRow := Format(sFirstRow, [fSettings.CompareModeString, fRowCount + 1]);
            end;
            s := '1.%s[ 邻行距 ↓%d ]';
            s := Format(s, [sFirstRow, fRowCount + 1 - FirstRow.Value]);
          end;
        end;
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn('');
        fr.WriteLn(s);
      end;

      case fSettings.CompareMode of
        cmSlant:
        begin
          sFirstRow := '';
          if not fSettings.ExportLite then
          begin
            sFirstRow := '（第%d行为首行）';
            sFirstRow := Format(sFirstRow, [FirstRow.Value]);
          end;
          s := '%d.%s（邻行距 ↓%d，同行数：%d）';
          s := Format(s, [FirstRow.FillCurrentRow, sFirstRow, FirstRow.RowSpacing, FirstRow.RowCount]);
        end;
        else
        begin
          sFirstRow := '';
          if not fSettings.ExportLite then
          begin
            sFirstRow := '[ %s连（第%d行为首行）]；';
            sFirstRow := Format(sFirstRow, [fSettings.CompareModeString, FirstRow.Value]);
          end;

          s := '%d.%s[ 邻行距 ↓%d ]';
          s := Format(s, [FirstRow.FillCurrentRow, sFirstRow, FirstRow.RowSpacing]);
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
      RowNumber := 0;
      repeat
        FirstRowCodeName.FillPrepare(fDatabase, 'FirstRow = ? LIMIT ? OFFSET ?',
          [FirstRow.Value, EachPageRowCount, PageIndex * EachPageRowCount]);
        while FirstRowCodeName.FillOne and not Terminated do
        begin
          RowNumber := RowNumber + 1;
          case fSettings.CompareMode of
            cmSlant:
            begin
              s := Format('（%d）.[代号：%s ]', [
                RowNumber,
                CodeNameToString(FirstRowCodeName.CodeName)
              ]);
              fr.WriteLn('');
              fr.WriteLn(s);
            end;
            else
            begin
              s := '【%d】.%d个组合[ 代号：%s]';
              s := Format(s, [
                RowNumber,
                FirstRowCodeName.CodeNameValueCount,
                CodeNameToString(FirstRowCodeName.CodeName)
              ]);
              fr.WriteLn('');
              fr.WriteLn(s);
            end;
          end;
        end;
        Inc(PageIndex);
      until Terminated or (FirstRowCodeName.FillTable.RowCount = 0);
    end;
    if Terminated then Exit;

    case fSettings.CompareMode of
      cmVert:
      begin
        s := '%d.[（第%d行为最末首行）直连（第%d-1行）]';
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
        s := '%d.（第%d行为最末首行）';
        s := Format(s, [
          FirstRow.FillTable.RowCount + 2,
          fSettings.SlantCompareSpacing + 1
        ]);
        fr.WriteLn('');
        fr.WriteLn(s);
      end;
      cmVertSlant:
      begin
        s := '%d.[（第%d行为最末首行）直连（第%d-%d行）]';
        s := Format(s, [
          FirstRow.FillTable.RowCount + 2,
          fSettings.CompareSpacing + 1,
          fSettings.CompareSpacing,
          fSettings.CompareSpacing + 1 - fSettings.VertCompareSpacing
        ]);
        fr.WriteLn('');
        fr.WriteLn(s);
        s := '%d.[（第%d行为最末首行）斜连（第%d-%d行）]';
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

    fr.WriteFinish;
    fr.RenameLastFile;
  finally
    fr.Free;
  end;
end;

procedure TDataComputer.ExportData;
var
  s, sMode, sCompareMode: string;
  DataList: TSQLTableJSON;
  i: Integer;
begin
  sMode := '1';
  if fSettings.DataMode = 1 then sMode := '3';
  if Length(fSettings.IntervalValues) > 1 then
  begin
    sMode := '2';
    if fSettings.DataMode = 1 then sMode := '4';
  end;
  sCompareMode := '';
  if not fSettings.ExportLite  then sCompareMode := Format('（%s连）', [fSettings.CompareModeString]);
  fExportDirectory := fDirectory + Format('\导出结果%s（第%s模式）\', [sCompareMode, sMode]);
  fExportDirectory2 := fExportDirectory + '（1）.【排列】-（6）.【简化】\';

  if not TDirectory.Exists(fExportDirectory) then
    TDirectory.CreateDirectory(fExportDirectory);
  for s in TDirectory.GetFiles(fExportDirectory, '*.txt') do TFile.Delete(s);
  if not TDirectory.Exists(fExportDirectory2) then
    TDirectory.CreateDirectory(fExportDirectory2);
  for s in TDirectory.GetFiles(fExportDirectory2, '*.txt') do TFile.Delete(s);

  if fSettings.ExportSource then
  begin
    DataList := fDatabase.MultiFieldValues(TSQLCodeName,
      'BatchNumber',
      'BatchNumber > 0 GROUP BY BatchNumber ORDER BY BatchNumber',
      []
    );
    try
      while DataList.Step do
        SaveToFile(DataList.FieldAsInteger('BatchNumber'));
    finally
      DataList.Free;
    end;
  end;
  if fSettings.ExportFile then
  begin
    SaveToFile2;
    SaveToFile3;
  end;
end;

procedure TDataComputer.Execute;
begin
  TMonitor.Enter(fLock);
  try
    repeat
      try
        TMonitor.Wait(fLock, Cardinal.MaxValue);
        if Terminated then Break;

        fStopwatch := TStopwatch.StartNew;
        try
          if Now >= 43586 then  Break;   //5.1

          LoadRow;
          if Terminated then Break;
          CompareRow;
          if Terminated then Break;
          BuildCodeName;
          if Terminated then Break;
          GroupCodeName;
          if Terminated then Break;
          MergeFirstRow;
          if Terminated then Break;
          UpdateFirstRow;
          if Terminated then Break;
          ExportData;
          if Terminated then Break;

          fStopwatch.Stop;
          if Assigned(fOnFinish) then fOnFinish(Self);
        finally
          fStopwatch.Stop;
        end;
      except
        on e: Exception do
        begin
          fErrorMessage := e.Message;
          if Assigned(fOnError) then fOnError(Self);
        end;
      end;
    until Terminated;
  finally
    TMonitor.Exit(fLock);
  end;
end;

constructor TGroupCodeName.Create;
begin
  inherited Create(True);
  fLock := TObject.Create;
  fTask := ttWait;
  fCodeName := TSQLCodeName.Create;
  fCodeName2 := TSQLCodeName.Create;
end;

destructor TGroupCodeName.Destroy;
begin
  fLock.Free;
  fCodeName.Free;
  fCodeName2.Free;
  inherited Destroy;
end;

procedure TGroupCodeName.SyncCodeName;
var
  FileName: string;
begin
  if Assigned(fDatabase) then FreeAndNil(fDatabase);
  FileName := fDirectory + 'Cache';
  if FileExists(FileName) then DeleteFile(FileName);

  fDatabase := TSQLRestServerDB.CreateWithOwnModel([TSQLCodeName], FileName);
  fDatabase.CreateMissingTables;
  fDatabase.RecordVersionSynchronizeSlave(TSQLCodeName, fClient);

  fService.SyncCodeNameFinish(TNotifyCallback(fNotifyCallback).ID);
  fTask := ttWait;
end;

procedure TGroupCodeName.Group;
var
  RowCount, RowNumber: Int64;
  PageIndex: Integer;
begin
  RowCount := fDatabase.TableRowCount(TSQLCodeName);
  RowNumber := fNumber;
  fCodeNameDeltaCount := 0;
  fDatabase.TransactionBegin(TSQLCodeName);
  while (RowNumber <= Ceil((RowCount - 1) / 2)) and not Terminated do
  begin
    Group(RowCount, RowNumber);
    if RowCount - RowNumber <> RowNumber then
      Group(RowCount, RowCount - RowNumber);

    RowNumber := RowNumber + fConsumerCount;
  end;
  if Terminated then Exit;
  fDatabase.Commit(1, True);

  PageIndex := 0;
  repeat
    fCodeName.FillPrepare(fDatabase, 'BatchNumberRepeat = 1 LIMIT ? OFFSET ?',
      [EachPageRowCount, PageIndex * EachPageRowCount]);
    fClient.TransactionBegin(TSQLCodeName);
    try
      while fCodeName.FillOne and not Terminated do
      begin
        fCodeName.BatchNumber := fBatchNumber;
        fCodeName.BatchNumberRepeat := False;
        fDatabase.Update(fCodeName);
      end;
      fClient.Commit(1, True);
    except
      on e: Exception do
      begin
        fClient.RollBack;
        raise;
      end;
    end;
    Inc(PageIndex);
  until Terminated or (fCodeName.FillTable.RowCount = 0);
  if Terminated then Exit;

  fDatabase.Delete(TSQLCodeName, 'BatchNumber < ?', [fBatchNumber]);
  if Terminated then Exit;

  fService.GroupCodeNameFinish(TNotifyCallback(fNotifyCallback).ID);
  fTask := ttWait;
end;

procedure TGroupCodeName.Group(RowCount, RowNumber: Int64);
begin
  Foreach(RowCount, 2, RowNumber,
  procedure(FirstNumber: Cardinal; FirstRowIndexs: TWordDynArray)
  var
    s: string;
  begin
    s := 'BatchNumber = ? LIMIT 1 OFFSET ?';
    fCodeName.FillPrepare(fDatabase, s, [fBatchNumber - 1, FirstRowIndexs[0] - 1]);
    fCodeName.FillOne;
    fCodeName2.FillPrepare(fDatabase, s, [fBatchNumber - 1, FirstRowIndexs[1] - 1]);
    fCodeName2.FillOne;
    fCodeName.Intersection(fCodeName2.Value);
    if (fCodeName.ValueCount > 0)
      and (fCodeName.ValueCount >= fKeepCodeNameValueCount)
    then
    begin
      fCodeName.MergeFirstRow(fCodeName2.FirstRow);

      s := 'Value = ?';
      fCodeName2.FillPrepare(fDatabase, s, [fCodeName.Value]);
      if fCodeName2.FillOne then
      begin
        if fCodeName2.BatchNumber <> fBatchNumber then
        begin
          fCodeName2.BatchNumberRepeat := True;
          fDatabase.Update(fCodeName2);
        end;
        if fCodeName2.MergeFirstRow(fCodeName.FirstRow) then
        begin
          fDatabase.Update(fCodeName2);
          fCodeNameDeltaCount := fCodeNameDeltaCount + 1;
        end;
      end
      else
      begin
        fCodeName.BatchNumber := fBatchNumber;
        fCodeName.BatchNumberRepeat := False;
        fDatabase.Add(fCodeName, True);
        fCodeNameDeltaCount := fCodeNameDeltaCount + 1;
      end;

      if fCodeNameDeltaCount >= 100000 then
      begin
        fDatabase.Commit(1, True);
        fDatabase.TransactionBegin(TSQLCodeName);
        fCodeNameDeltaCount := 0;
      end;
    end;
  end,
  function(FirstNumber, GroupCount: Cardinal): Boolean
  begin
    Result := Terminated;
  end);
end;

procedure TGroupCodeName.UploadCodeName;
var
  PageIndex: Integer;
begin
  PageIndex := 0;
  repeat
    fCodeName.FillPrepare(fDatabase, 'LIMIT ? OFFSET ?',
      [EachPageRowCount, PageIndex * EachPageRowCount]);
    fClient.TransactionBegin(TSQLCodeName);
    try
      while fCodeName.FillOne and not Terminated do
      begin
        fCodeName2.FillPrepare(fClient, 'Value = ?', [fCodeName.Value]);
        if fCodeName2.FillOne then
        begin
          if fCodeName2.MergeFirstRow(fCodeName2.FirstRow) then
            fClient.Update(fCodeName2);
        end
        else
        begin
          fClient.Add(fCodeName, True);
        end;
      end;
      fClient.Commit(1, True);
    except
      on e: Exception do
      begin
        fClient.RollBack;
        raise;
      end;
    end;
    Inc(PageIndex);
  until Terminated or (fCodeName.FillTable.RowCount = 0);
  if Terminated then Exit;

  fService.UploadCodeNameFinish(TNotifyCallback(fNotifyCallback).ID);
  fTask := ttWait;
end;

procedure TGroupCodeName.Execute;
begin
  TMonitor.Enter(fLock);
  try
    repeat
      case fTask of
        ttWait: TMonitor.Wait(fLock, Cardinal.MaxValue);
        ttSyncCodeName: SyncCodeName;
        ttGroupCodeName: Group;
        ttUploadCodeName: UploadCodeName;
      end;
    until Terminated;
  finally
    TMonitor.Exit(fLock);
  end;
end;

procedure TGroupCodeName.StartWork;
begin
  TMonitor.PulseAll(fLock);
end;

procedure TGroupCodeName.Stop;
begin
  Terminate;
  StartWork;
  WaitFor;
end;

constructor TGroupCodeName2.Create;
begin
  inherited Create(True);
  fCodeName := TSQLCodeName.Create;
  fCodeName2 := TSQLCodeName.Create;
end;

destructor TGroupCodeName2.Destroy;
begin
  fCodeName.Free;
  fCodeName2.Free;
  inherited Destroy;
end;

procedure TGroupCodeName2.Group(RowCount, RowNumber: Int64);
begin
  Foreach(RowCount, 2, RowNumber,
  procedure(FirstNumber: Cardinal; FirstRowIndexs: TWordDynArray)
  var
    s: string;
  begin
    s := 'BatchNumber = ? LIMIT 1 OFFSET ?';
    fCodeName.FillPrepare(fCacheDB, s, [fBatchNumber - 1, FirstRowIndexs[0] - 1]);
    fCodeName.FillOne;
    fCodeName2.FillPrepare(fCacheDB, s, [fBatchNumber - 1, FirstRowIndexs[1] - 1]);
    fCodeName2.FillOne;
    fCodeName.Intersection(fCodeName2.Value);
    if (fCodeName.ValueCount > 0)
      and (fCodeName.ValueCount >= fKeepCodeNameValueCount)
    then
    begin
      fCodeName.MergeFirstRow(fCodeName2.FirstRow);
      fDataComputer.ConsumerSaveCodeName(fCodeName);

      {s := 'Value = ?';
      fCodeName2.FillPrepare(fDatabase, s, [fCodeName.Value]);
      if fCodeName2.FillOne then
      begin
        if fCodeName2.BatchNumber <> fBatchNumber then
        begin
          fCodeName2.BatchNumberRepeat := True;
          fDatabase.Update(fCodeName2);
        end;
        if fCodeName2.MergeFirstRow(fCodeName.FirstRow) then
        begin
          fDatabase.Update(fCodeName2);
        end;
      end
      else
      begin
        fCodeName.BatchNumber := fBatchNumber;
        fCodeName.BatchNumberRepeat := False;
        fDatabase.Add(fCodeName, True);
      end;}
    end;
  end,
  function(FirstNumber, GroupCount: Cardinal): Boolean
  begin
    Result := Terminated and not fDataComputer.Terminated;
  end);
end;

procedure TGroupCodeName2.Execute;
var
  RowNumber: Int64;
begin
  RowNumber := fNumber;
  while (RowNumber <= Ceil((fRowCount - 1) / 2)) and not Terminated and not fDataComputer.Terminated do
  begin
    Group(fRowCount, RowNumber);
    if fRowCount - RowNumber <> RowNumber then
      Group(fRowCount, fRowCount - RowNumber);

    RowNumber := RowNumber + fConsumerCount;
  end;
end;

procedure TGroupCodeName2.SyncCodeName;
begin
  if Assigned(fCacheDB) then FreeAndNil(fCacheDB);
  fFileName := fDirectory + ThreadID.ToString;
  if FileExists(fFileName) then DeleteFile(fFileName);

  fCacheDB := TSQLRestServerDB.CreateWithOwnModel([TSQLCodeName]);
  fCacheDB.CreateMissingTables;
  fCacheDB.RecordVersionSynchronizeSlave(TSQLCodeName, fDatabase);
end;

procedure ReadSettings;
var
  v: Variant;
  IntervalValues: TWordDynArray;
  ValidityCountEachGroupNumber: TInt64DynArray;
  i: Integer;
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
  fKeyValue.GetKeyValue('GroupCount', v);
  if not VarIsEmpty(v) then fSettings.GroupCount := v;
  fKeyValue.GetKeyValue('fGroupCountEachFirstNumber', v);
  if not VarIsEmpty(v) then fSettings.KeepCodeNameValueCount := v;
  fKeyValue.GetKeyValue('GroupNumber', v);
  if not VarIsEmpty(v) then fSettings.GroupNumber := v;
  fKeyValue.GetKeyValue('GroupNumber2', v);
  if not VarIsEmpty(v) then fSettings.GroupNumber2 := v;
  fKeyValue.GetKeyValue('GroupFirstNumberCount', v);
  if not VarIsEmpty(v) then fSettings.GroupFirstNumberCount := v;
  fKeyValue.GetKeyValue('GroupCountEachFirstNumber', v);
  if not VarIsEmpty(v) then fSettings.GroupCountEachFirstNumber := v;
  fKeyValue.GetKeyValue('GroupNumber3', v);
  if not VarIsEmpty(v) then fSettings.GroupNumber3 := v;
  fKeyValue.GetKeyValue('GroupNumber4', v);
  if not VarIsEmpty(v) then fSettings.GroupNumber4 := v;
  fKeyValue.GetKeyValue('ValidityCountEachGroupNumber', ValidityCountEachGroupNumber);
  fSettings.BuildValidityCountEachGroupNumber;
  for i := Low(fSettings.ValidityCountEachGroupNumber) to High(fSettings.ValidityCountEachGroupNumber) do
  begin
    if (i + 1) * 2 - 1 <= High(ValidityCountEachGroupNumber) then
    begin
      fSettings.ValidityCountEachGroupNumber[i].Value := ValidityCountEachGroupNumber[(i + 1) * 2 - 2];
      fSettings.ValidityCountEachGroupNumber[i].Value2 := ValidityCountEachGroupNumber[(i + 1) * 2 - 1];
    end;
  end;
  fKeyValue.GetKeyValue('ExportSource', v);
  if not VarIsEmpty(v) then fSettings.ExportSource := v;
  fKeyValue.GetKeyValue('ExportCodeNameValueCount', v);
  if not VarIsEmpty(v) then fSettings.ExportCodeNameValueCount := v;
  fKeyValue.GetKeyValue('ExportCodeNameValueCount2', v);
  if not VarIsEmpty(v) then fSettings.ExportCodeNameValueCount2 := v;
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
  fKeyValue.SetKeyValue('GroupCount', fSettings.GroupCount);
  fKeyValue.SetKeyValue('KeepCodeNameValueCount', fSettings.KeepCodeNameValueCount);
  fKeyValue.SetKeyValue('ExportSource', fSettings.ExportSource);
  fKeyValue.SetKeyValue('ExportCodeNameValueCount', fSettings.ExportCodeNameValueCount);
  fKeyValue.SetKeyValue('ExportCodeNameValueCount2', fSettings.ExportCodeNameValueCount2);
end;

procedure ConnectServer(Address, Port: string);
var
  i: Integer;
begin
  DisconnectServer;

  fClient := TSQLHttpClientWebsockets.Create(Address, Port, TSQLModel.Create([TSQLCodeName]));
  fClient.Model.Owner := fClient;
  fClient.WebSocketsUpgrade('encryptionkey');
  if not fClient.ServerTimestampSynchronize then
    raise Exception.Create('连接失败');
  fClient.ServiceDefine([IService], sicShared);
  if not fClient.Services.Resolve(IService, fService) then
    raise EServiceException.Create('Service IService unavailable');
  fNotifyCallback := TNotifyCallback.Create(fClient, INotifyCallback);
  fService.RegisterConsumer(fNotifyCallback, TNotifyCallback(fNotifyCallback).ID, fLocalIP);
  fGroupCodeName.Client := fClient;
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
  //TInterfaceFactory.RegisterInterfaces([TypeInfo(IService), TypeInfo(INotifyCallback)]);

  fDatabase := TSQLRestServerDB.CreateWithOwnModel(
    [TSQLKeyValue, TSQLRow, TSQLFirstRow, TSQLCodeName, TSQLFirstRowCodeName],
    fDirectory + 'Data'
  );
  fDatabase.CreateMissingTables;
  fDatabase.CreateSQLIndex(TSQLCodeName, 'BatchNumber', False);
  fDatabase.CreateSQLIndex(TSQLCodeName, 'ValueCount', False);
  fDatabase.CreateSQLIndex(TSQLCodeName, 'FirstRowValueCount', False);
  fDatabase.CreateSQLIndex(TSQLFirstRowCodeName, 'FirstRow', False);
  //fDatabase.ServiceDefine(TService, [IService], sicShared).SetOptions([], [optExecLockedPerInterface]);

  //sqlite3.create_function(fDatabase.DB.DB, 'WordDynArrayEquals', 2, SQLITE_ANY,
  //  TypeInfo(TWordDynArray), Int64DynArrayEquals, nil, nil);

  fKeyValue := TSQLKeyValue.Create;
  fKeyValue.SetRest(fDatabase);
  ReadSettings;

  {try
    fServer := TSQLHttpServer.Create('8888', [fDatabase], '+', useBidirSocket);
    fServer.WebSocketsEnable(fDatabase, 'encryptionkey');
  except
  end;

  with TIdIPWatch.Create do
  begin
    try
      fLocalIP := LocalIP;
    finally
      Free;
    end;
  end; }

  fDataComputer := TDataComputer.Create;
  fDataComputer.Start;

  fGroupCodeName := TGroupCodeName.Create;
  fGroupCodeName.Start;

  //ConnectServer('127.0.0.1', '8888');

finalization
  DisconnectServer;
  if Assigned(fGroupCodeName) then
  begin
    fGroupCodeName.Stop;
    fGroupCodeName.Free;
  end;
  if Assigned(fDataComputer) then
  begin
    fDataComputer.Terminate;
    TMonitor.PulseAll(fDataComputer.Lock);
    fDataComputer.WaitFor;
    fDataComputer.Free;
  end;
  if Assigned(fServer) then FreeAndNil(fServer);
  if Assigned(fKeyValue) then FreeAndNil(fKeyValue);
  if Assigned(fDatabase) then FreeAndNil(fDatabase);

end.
