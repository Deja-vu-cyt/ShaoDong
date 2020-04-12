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
  Vcl.Dialogs,
  IdIPWatch,
  uCommon,
  uFileWriter,
  ufrmMain;

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
    fFirstRow: TWordDynArray;
    fFirstRowValueCount: Word;
  public
    procedure ClearFirstRow;
    function MergeFirstRow(aFirstRow: TWordDynArray): Boolean;
    procedure Intersection(aCodeName: string);
    function ToWordDynArray: TWordDynArray;
  published
    property Value: RawUTF8 read fValue write fValue;// stored AS_UNIQUE;
    property ValueCount: Word read fValueCount write fValueCount;
    property BatchNumber: Byte read fBatchNumber write fBatchNumber;
    property FirstRow: TWordDynArray read fFirstRow write fFirstRow;
    property FirstRowValueCount: Word read fFirstRowValueCount write fFirstRowValueCount;
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
    procedure Init;
    function InitFinish: Boolean;
    procedure SyncCodeName(BatchNumber, RowCount: Int64);
    function SyncCodeNameFinish: Boolean;
    procedure GroupCodeName(Settings: Variant);
    function GroupCodeNameFinish: Boolean;
    procedure UploadCodeName;
    function UploadCodeNameFinish: Boolean;
    procedure ExportFile(const IntervalValues: TWordDynArray;
      const CompareMode, VertCompareSpacing, SlantCompareSpacing,
      ExportCodeNameValueCount, ExportCodeNameValueCount2: Cardinal;
      const ExportFile, ExportFile2, ExportFile3, ExportFile4: Boolean);
    procedure Terminate;
  end;

  TNotifyCallback = class(TInterfacedCallback, INotifyCallback)
  protected
    fID: string;
    fInitFinish: Boolean;
    procedure Init;
    function InitFinish: Boolean;
    procedure SyncCodeName(BatchNumber, RowCount: Int64);
    function SyncCodeNameFinish: Boolean;
    procedure GroupCodeName(Settings: Variant);
    function GroupCodeNameFinish: Boolean;
    procedure UploadCodeName;
    function UploadCodeNameFinish: Boolean;
    procedure ExportFile(const IntervalValues: TWordDynArray;
      const CompareMode, VertCompareSpacing, SlantCompareSpacing,
      ExportCodeNameValueCount, ExportCodeNameValueCount2: Cardinal;
      const ExportFile, ExportFile2, ExportFile3, ExportFile4: Boolean);
    procedure Terminate;
  public
    constructor Create(aRest: TSQLRest; const aGUID: TGUID);
  published
    property ID: string read fID;
  end;

  IService = interface(IServiceWithCallbackReleased)
    ['{C92DCBEA-C680-40BD-8D9C-3E6F2ED9C9CF}']
    procedure RegisterConsumer(const Consumer: INotifyCallback; const ID, Address: string);
  end;

  TService = class(TInterfacedObject, IService)
  public
    procedure RegisterConsumer(const Consumer: INotifyCallback; const ID, Address: string);
    procedure CallbackReleased(const Callback: IInvokable; const InterfaceName: RawUTF8);
  end;

  TConsumer = class
  protected
    fID: string;
    fAddress: string;
    fValue: INotifyCallback;
  published
    property ID: string read fID write fID;
    property Address: string read fAddress write fAddress;
    property Value: INotifyCallback read fValue write fValue;
  end;

  TConsumers = class
  private
    fList: TList<TConsumer>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Consumer: INotifyCallback; ID, Address: string);
    procedure Clear;
    function Find(ID: string): TConsumer;
    procedure Remove(Consumer: TConsumer);
    property List: TList<TConsumer> read fList;
  end;

  TRange = record
    Value: Integer;
    Value2: Integer;
    function Between(v: Integer): Boolean;
  end;

  TCompareMode = (cmNone, cmVert, cmSlant, cmVertSlant);

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

    fFileDirectory: string;
    fDataMode: Byte;
    fExportLite: Boolean;
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
    fKeepExportCodeNameValueCount: Boolean;
    fKeepLastBatchCodeNameOnEachComputer: Boolean;
    fExportFile: Boolean;
    fExportFile2: Boolean;
    fExportFile3: Boolean;
    fExportFile4: Boolean;

    fExportDirectory: string;
    fExportDirectory2: string;

    fRowCount: Word;
    fLastComapreRow: Word;
    fBuildCodeNameFirstRow: Word;
    fLastGroupNumber: Byte;
    fBatchNumber: Byte;
    fBatchNumberRowCount: Cardinal;
    fFirstNumber: Cardinal;
    fFirstNumberGroupCount: Cardinal;
    fBatchNumberGroupFirstNumberCount: Cardinal;
    fBatchNumberGroupCountEachFirstNumber: Cardinal;
    fBatchNumberKeepCodeNameValueCount: Word;
    fBatchNumberKeepCodeNameValueCount2: Word;
    fValidityFirstRowCount: TRange;
    fCodeNameCacheIndex: Cardinal;
    fCodeNameRowNumber: Cardinal;
    fFirstRowUpdated: Boolean;
    fBuildFirstRowValueCount: Word;
    fBuildFirstRowValueCount2: Word;

    fRest: TSQLRestServer;
    fCodeName: TSQLCodeName;
    fCodeName2: TSQLCodeName;
    fCodeName3: TSQLCodeName;
    fCodeNameDeltaCount: Cardinal;
    fConsumerCount: Byte;
    fConsumerNumber: Byte;
    fGroupCodeNameFinished: Boolean;

    procedure Init;
    procedure RestoreState;
    procedure LoadRow;
    function VertCompareRow(r, r2: TSQLRow): Boolean;
    function SlantCompareRow(r, r2: TSQLRow; CompareData: TSQLCompareData; Offset: Integer): Boolean;
    procedure CompareRow;
    procedure BuildCodeName;
    procedure FillCodeName(Rows: TCardinalDynArray);
    procedure GroupCodeName(RowCount, RowNumber: Int64); overload;
    procedure GroupCodeName; overload;
    procedure ConsumerGroupCodeName;
    procedure MergeFirstRow;
    procedure UpdateFirstRow;

    function RebuildFile(RowCount: Cardinal): Boolean;
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
    constructor Create(aRest: TSQLRestServer);
    destructor Destroy; override;
    procedure Execute; override;
    procedure BuildValidityCountEachGroupNumber;
  published
    property Stopwatch: TStopwatch read fStopwatch;
    property FileDirectory: string read fFileDirectory write fFileDirectory;
    property DataMode: Byte read fDataMode write fDataMode;
    property ExportLite: Boolean read fExportLite write fExportLite;
    property IntervalValues: TWordDynArray read fIntervalValues write fIntervalValues;
    property CompareMode: TCompareMode read fCompareMode write fCompareMode;
    property CompareCrossRange: Boolean read fCompareCrossRange write fCompareCrossRange;
    property VertCompareSpacing: Word read fVertCompareSpacing write fVertCompareSpacing;
    property VertSameValueCount: Word read fVertSameValueCount write fVertSameValueCount;
    property VertSameValueCount2: Word read fVertSameValueCount2 write fVertSameValueCount2;
    property SlantCompareSpacing: Word read fSlantCompareSpacing write fSlantCompareSpacing;
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
    property ValidityFirstRowCount: TRange read fValidityFirstRowCount write fValidityFirstRowCount;
    property ExportCodeNameValueCount: Word read fExportCodeNameValueCount write fExportCodeNameValueCount;
    property ExportCodeNameValueCount2: Word read fExportCodeNameValueCount2 write fExportCodeNameValueCount2;
    property KeepExportCodeNameValueCount: Boolean read fKeepExportCodeNameValueCount write fKeepExportCodeNameValueCount;
    property KeepLastBatchCodeNameOnEachComputer: Boolean read fKeepLastBatchCodeNameOnEachComputer write fKeepLastBatchCodeNameOnEachComputer;
    property ExportFile: Boolean read fExportFile write fExportFile;
    property ExportFile2: Boolean read fExportFile2 write fExportFile2;
    property ExportFile3: Boolean read fExportFile3 write fExportFile3;
    property ExportFile4: Boolean read fExportFile4 write fExportFile4;
    property ConsumerCount: Byte read fConsumerCount write fConsumerCount;
    property ConsumerNumber: Byte read fConsumerNumber write fConsumerNumber;
    property BatchNumber: Byte read fBatchNumber write fBatchNumber;
    property BatchNumberRowCount: Cardinal read fBatchNumberRowCount;
    property BatchNumberGroupFirstNumberCount: Cardinal read fBatchNumberGroupFirstNumberCount write fBatchNumberGroupFirstNumberCount;
    property BatchNumberGroupCountEachFirstNumber: Cardinal read fBatchNumberGroupCountEachFirstNumber write fBatchNumberGroupCountEachFirstNumber;
    property BatchNumberKeepCodeNameValueCount: Word read fBatchNumberKeepCodeNameValueCount write fBatchNumberKeepCodeNameValueCount;
    property BatchNumberKeepCodeNameValueCount2: Word read fBatchNumberKeepCodeNameValueCount2 write fBatchNumberKeepCodeNameValueCount2;
    property FirstNumber: Cardinal read fFirstNumber;
    property FirstNumberGroupCount: Cardinal read fFirstNumberGroupCount;
    property GroupCodeNameFinished: Boolean read fGroupCodeNameFinished;
  end;

  TSyncCodeName = class(TThread)
    fClient: TSQLHttpClientWebsockets;
    fRest: TSQLRestServer;
    fSyncFinished: Boolean;
    fBatchNumber: Byte;
    fBatchNumberRowCount: Cardinal;
  public
    constructor Create(aRest: TSQLRestServer; aClient: TSQLHttpClientWebsockets);
    procedure Execute; override;
  published
    property SyncFinished: Boolean read fSyncFinished;
    property BatchNumber: Byte read fBatchNumber write fBatchNumber;
    property BatchNumberRowCount: Cardinal read fBatchNumberRowCount write fBatchNumberRowCount;
  end;

  TUploadCodeName = class(TThread)
    fClient: TSQLHttpClientWebsockets;
    fRest: TSQLRestServer;
    fUploadFinished: Boolean;
  public
    constructor Create(aRest: TSQLRestServer; aClient: TSQLHttpClientWebsockets);
    procedure Execute; override;
  published
    property UploadFinished: Boolean read fUploadFinished;
  end;

const
  //EachFileRowCount: Cardinal = 1000000;
  //EachFileRowNumber: Cardinal = 200000;
  EachFileRowCount: Cardinal = 1000;
  EachFileRowNumber: Cardinal = 200;
  EachPageRowCount: Word = 10000;

var
  fMainApp: Boolean;
  fDataComputer: TDataComputer;
  fDatabase: TSQLRestServerDB;
  fKeyValue: TSQLKeyValue;
  fConsumers: TConsumers;
  fThread: TThread;

  fLocalIP: string;
  fServer: TSQLHttpServer;
  fClient: TSQLHttpClientWebsockets;
  fNotifyCallback: INotifyCallback;
  fService: IService;

procedure BuildDatabase;
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
  fInitFinish := False;
end;

procedure TNotifyCallback.Init;
begin
  fInitFinish := True;
end;

function TNotifyCallback.InitFinish: Boolean;
begin
  Result := fInitFinish;
end;

procedure TNotifyCallback.SyncCodeName(BatchNumber, RowCount: Int64);
begin
  fThread := TSyncCodeName.Create(fDatabase, fClient);
  TSyncCodeName(fThread).BatchNumber := BatchNumber;
  TSyncCodeName(fThread).BatchNumberRowCount := RowCount;
  fThread.Start;
end;

function TNotifyCallback.SyncCodeNameFinish: Boolean;
begin
  Result := Assigned(fThread) and TSyncCodeName(fThread).SyncFinished;
end;

procedure TNotifyCallback.GroupCodeName(Settings: Variant);
var
  r: TRange;
begin
  fThread := TDataComputer.Create(fDatabase);
  with TDataComputer(fThread) do
  begin
    BatchNumber := Settings.BatchNumber;
    ConsumerCount := Settings.ConsumerCount;
    ConsumerNumber := Settings.ConsumerNumber;
    KeepCodeNameValueCount := Settings.KeepCodeNameValueCount;
    BatchNumberGroupFirstNumberCount := Settings.BatchNumberGroupFirstNumberCount;
    BatchNumberGroupCountEachFirstNumber := Settings.BatchNumberGroupCountEachFirstNumber;
    r.Value := Settings.ValidityFirstRowCount.Value;
    r.Value2 := Settings.ValidityFirstRowCount.Value2;
    ValidityFirstRowCount := r;
    BatchNumberKeepCodeNameValueCount := Settings.BatchNumberKeepCodeNameValueCount;
    BatchNumberKeepCodeNameValueCount2 := Settings.BatchNumberKeepCodeNameValueCount2;
    ExportFile := False;
    ExportFile2 := False;
    ExportFile3 := False;
    ExportFile4 := False;
  end;
  fThread.Start;
end;

function TNotifyCallback.GroupCodeNameFinish: Boolean;
begin
  Result := Assigned(fThread) and TDataComputer(fThread).GroupCodeNameFinished;
end;

procedure TNotifyCallback.UploadCodeName;
begin
  fThread := TUploadCodeName.Create(fDatabase, fClient);
  fThread.Start;
end;

function TNotifyCallback.UploadCodeNameFinish: Boolean;
begin
  Result := Assigned(fThread) and TUploadCodeName(fThread).UploadFinished;
end;

procedure TNotifyCallback.ExportFile(const IntervalValues: TWordDynArray;
  const CompareMode, VertCompareSpacing, SlantCompareSpacing,
  ExportCodeNameValueCount, ExportCodeNameValueCount2: Cardinal;
  const ExportFile, ExportFile2, ExportFile3, ExportFile4: Boolean);
begin
  if fMainApp then Exit;

  {fDataCo.IntervalValues := IntervalValues;
  fCompareMode := TCompareMode(CompareMode);
  fVertCompareSpacing := VertCompareSpacing;
  fSlantCompareSpacing := SlantCompareSpacing;
  fExportCodeNameValueCount := ExportCodeNameValueCount;
  fExportCodeNameValueCount2 := ExportCodeNameValueCount2;
  fExportFile := ExportFile;
  fExportFile2 := ExportFile2;
  fExportFile3 := ExportFile3;
  fExportFile4 := ExportFile4;}
end;

procedure TNotifyCallback.Terminate;
begin
  fInitFinish := False;
  if Assigned(fThread) then
  begin
    fThread.FreeOnTerminate := True;
    fThread.Terminate;
    fThread := nil;
  end;
  TThread.Queue(nil, procedure
  begin
    frmMain.ShowRowCount;
  end);
end;

procedure TService.RegisterConsumer(const Consumer: INotifyCallback; const ID, Address: string);
begin
  fConsumers.Add(Consumer, ID, Address);
end;

procedure TService.CallbackReleased(const Callback: IInvokable; const InterfaceName: RawUTF8);
begin
  //if InterfaceName = 'INotifyCallback' then
  //  InterfaceArrayDelete(fConsumers, Callback);
end;

constructor TConsumers.Create;
begin
  fList := TList<TConsumer>.Create;
end;

destructor TConsumers.Destroy;
begin
  fList.Free;
end;

procedure TConsumers.Add(Consumer: INotifyCallback; ID, Address: string);
var
  t: TConsumer;
begin
  t := Find(ID);
  if Assigned(t) then
  begin
    t.Value := Consumer;
    t.Address := Address;
  end
  else
  begin
    t := TConsumer.Create;
    t.ID := ID;
    t.Value := Consumer;
    t.Address := Address;
    fList.Add(t);
  end;
end;

procedure TConsumers.Clear;
var
  i: Integer;
begin
  for i := fList.Count - 1 downto 0 do
    fList[i].Free;
  fList.Clear;
end;

function TConsumers.Find(ID: string): TConsumer;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to fList.Count - 1 do
    if fList[i].ID = ID then
    begin
      Result := fList[i];
      Break;
    end;
end;

procedure TConsumers.Remove(Consumer: TConsumer);
begin
  fList.Remove(Consumer);
end;

function TRange.Between(v: Integer): Boolean;
begin
  Result := (v >= Value) and (v <= Value2);
end;

constructor TDataComputer.Create(aRest: TSQLRestServer);
begin
  inherited Create(True);
  fRest := aRest;
  if fMainApp then RestoreState;
  fCodeName := TSQLCodeName.Create;
  fCodeName2 := TSQLCodeName.Create;
  fCodeName3 := TSQLCodeName.Create;
end;

destructor TDataComputer.Destroy;
begin
  fCodeName.Free;
  fCodeName2.Free;
  fCodeName3.Free;
  inherited Destroy;
end;

procedure TDataComputer.Init;
var
  i: Integer;
begin
  case fCompareMode of
    cmVert: fCompareModeString := '直';
    cmSlant: fCompareModeString := '斜';
    cmVertSlant: fCompareModeString := '直、斜';
  end;

  fFirstIntervalValue := 0;
  if Length(fIntervalValues) > 1 then fFirstIntervalValue := fIntervalValues[0];
  fTotalIntervalValue := 0;
  for i := Low(fIntervalValues) to High(fIntervalValues) do
    fTotalIntervalValue := fTotalIntervalValue + fIntervalValues[i];

  fCompareSpacing := fVertCompareSpacing;
  if fSlantCompareSpacing > fCompareSpacing then
    fCompareSpacing := fSlantCompareSpacing;
end;

procedure TDataComputer.RestoreState;
var
  v: Variant;
  s: string;
begin
  fFirstNumber := 0;
  fGroupCodeNameFinished := False;
  fRowCount := 0;
  s := fRest.OneFieldValue(TSQLRow, 'Max(Number)', '', []);
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
  s, FileName: string;
  Row: TSQLRow;
begin
  if Terminated then Exit;
  if not TDirectory.Exists(fFileDirectory) then Exit;

  TSQLRow.AutoFree(Row);
  with TStringList.Create do
  begin
    try
      for FileName in TDirectory.GetFiles(fFileDirectory, '*.txt') do
      begin
        LoadFromFile(FileName);

        //fRest.Delete(TSQLRow, '');
        fRest.TransactionBegin(TSQLRow, 1);
        try
          for i := Count - 1 downto 0 do
          begin
            if not (TryStrToInt(Names[i].Trim, Digit) and (Digit > fRowCount)) then Continue;
            Row.Number := Digit;
            s := ValueFromIndex[i];
            Row.AssignValue(s, fIntervalValues, fDataMode);
            fRest.Add(Row, True);
            fRowCount := Digit;
          end;
          fRest.Commit(1, True);
        except
          on e: Exception do
          begin
            fRest.RollBack(1);
            raise Exception.Create(e.Message);
          end;
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
  Result := (TotalSameValueCount >= fVertSameValueCount)
    and (TotalSameValueCount <= fVertSameValueCount2);
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
    if (v < 1) or (v > fTotalIntervalValue) then Continue;
    if not fCompareCrossRange
      and (((Offset > 0) and (v > fFirstIntervalValue))
      or ((Offset < 0) and (v <= fFirstIntervalValue)))
    then Continue;

    CompareData.AddValue(v);
    if r.ValueExist(v) then TotalSameValueCount := TotalSameValueCount + 1
    else TotalDifferentValueCount := TotalDifferentValueCount + 1;
  end;
  Result := CompareData.HasValue
    and (TotalSameValueCount >= fSlantSameValueCount)
    and (TotalSameValueCount <= fSlantSameValueCount2);
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
  if Terminated then Exit;
  TSQLRow.AutoFree(Row, fRest, 'Number > ? ORDER BY Number', [fLastComapreRow - fCompareSpacing]);
  TSQLRow.AutoFree(Row2);
  TSQLFirstRow.AutoFree(FirstRow);
  TSQLCompareData.AutoFree(CompareData);

  fRest.TransactionBegin(TSQLFirstRow, 1);
  try
    while Row.FillOne and not Terminated do
    begin
      if Row.FillCurrentRow - 1 <= fCompareSpacing then Continue;
      fLastComapreRow := Row.Number;

      FirstRow.Value := Row.Number;
      FirstRow.ClearCodeName;
      FirstRow.Grouped := False;
      for i := Row.FillCurrentRow - 2 downto Row.FillCurrentRow - 1 - fCompareSpacing do
      begin
        Row.FillRow(i, Row2);
        Number := Row.FillCurrentRow - 1 - i;
        //直连
        if Number <= fVertCompareSpacing then
        begin
          if VertCompareRow(Row, Row2)
          then
          begin
            if fCompareMode = cmVert then CodeName := Number
            else CodeName := Number * 3 - 2;

            FirstRow.AddCodeName(CodeName);
          end;
        end;
        //斜连
        if Number <= fSlantCompareSpacing then
        begin
          //右斜连
          if SlantCompareRow(Row, Row2, CompareData, Number)
          then
          begin
            if fCompareMode = cmSlant then CodeName := Number * 2 - 1
            else CodeName := Number * 3 - 1;

            FirstRow.AddCodeName(CodeName);
          end;
          //左斜连
          if SlantCompareRow(Row, Row2, CompareData, -Number)
          then
          begin
            if fCompareMode = cmSlant then CodeName := Number * 2
            else CodeName := Number * 3;

            FirstRow.AddCodeName(CodeName);
          end;
        end;
      end;
     fRest.Add(FirstRow, True);

    end;
    fRest.Commit(1, True);

    fKeyValue.SetKeyValue('LastComapreRow', fLastComapreRow);
  except
    on e: Exception do
    begin
      fRest.RollBack(1);
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
  if Terminated then Exit;
  TSQLFirstRow.AutoFree(FirstRow, fRest, 'Value > ? ORDER BY Value', [fBuildCodeNameFirstRow]);
  TSQLCodeName.AutoFree(CodeName);
  fRest.TransactionBegin(TSQLCodeName, 1);
  try
    while FirstRow.FillOne and not Terminated do
    begin
      CodeName.FillPrepare(fRest, 'BatchNumber = 0 AND Value = ?', [FirstRow.CodeName.ToString]);
      if CodeName.FillOne then
      begin
        CodeName.MergeFirstRow([FirstRow.Value]);
        CodeName.FirstRowValueCount := Length(CodeName.FirstRow);
        fRest.Update(CodeName);
      end
      else
      begin
        CodeName.Value := FirstRow.CodeName.ToString;
        CodeName.ValueCount := Length(CodeName.Value);
        CodeName.BatchNumber := 0;
        CodeName.FirstRow := [FirstRow.Value];
        CodeName.FirstRowValueCount := Length(CodeName.FirstRow);
        fRest.Add(CodeName, True);
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
      fRest.RollBack(1);
      raise Exception.Create(e.Message);
    end;
  end;
end;


procedure TDataComputer.FillCodeName(Rows: TCardinalDynArray);
const
  CacheCount: Cardinal = 1000000;
begin
  if (Rows[0] <> fCodeNameRowNumber)
    or (Rows[1] > (Rows[0] - 1 + (fCodeNameCacheIndex + 1) * CacheCount))
  then
  begin
    if Rows[1] > (Rows[0] - 1 + (fCodeNameCacheIndex + 1) * CacheCount) then
      fCodeNameCacheIndex := fCodeNameCacheIndex + 1;

    fCodeName.FillPrepare(fRest, 'BatchNumber = ? LIMIT ? OFFSET ?',
      [fBatchNumber - 1, CacheCount, Rows[0] - 1 + fCodeNameCacheIndex * CacheCount]);

    if (Rows[0] <> fCodeNameRowNumber) then
    begin
      fCodeName.FillOne;
      fCodeName3.Value := fCodeName.Value;
      fCodeName3.ValueCount := fCodeName.ValueCount;
      fCodeName3.FirstRow := fCodeName.FirstRow;
      fCodeName3.FirstRowValueCount := fCodeName.FirstRowValueCount;
      fCodeNameRowNumber := Rows[0];
    end;
  end;

  fCodeName.Value := fCodeName3.Value;
  fCodeName.ValueCount := fCodeName3.ValueCount;
  fCodeName.FirstRow := fCodeName3.FirstRow;
  fCodeName.FirstRowValueCount := fCodeName3.FirstRowValueCount;
  fCodeName.FillRow(Rows[1] - (Rows[0] - 1 + fCodeNameCacheIndex * CacheCount), fCodeName2);
end;

procedure TDataComputer.GroupCodeName(RowCount, RowNumber: Int64);
var
  FirstRowCount: Integer;
begin
  TCombinatorialAlgorithm.For(RowCount, 2, RowNumber,
  procedure(FirstNumber: Cardinal; Rows: TCardinalDynArray)
  var
    s: string;
  begin
    {s := 'BatchNumber = ? LIMIT 1 OFFSET ?';
    fCodeName.FillPrepare(fRest, s, [fBatchNumber - 1, Rows[0] - 1]);
    fCodeName.FillOne;
    fCodeName2.FillPrepare(fRest, s, [fBatchNumber - 1, Rows[1] - 1]);
    fCodeName2.FillOne;}
    //fCodeName.FillRow(Rows[0]);
    //fCodeName.FillRow(Rows[1], fCodeName2);
    FillCodeName(Rows);
    fCodeName.Intersection(fCodeName2.Value);
    if fCodeName.ValueCount = 0 then Exit;
    if (fKeepCodeNameValueCount > 0) and (fCodeName.ValueCount < fKeepCodeNameValueCount) then Exit;
    //代号个数不在导出个数范围内
    if (fBatchNumberKeepCodeNameValueCount2 > 0)
      and ((fCodeName.ValueCount < fBatchNumberKeepCodeNameValueCount) or (fCodeName.ValueCount > fBatchNumberKeepCodeNameValueCount2))
    then Exit;
    //相同首行个数是否在有效范围内
    FirstRowCount := fCodeName.FirstRowValueCount + fCodeName2.FirstRowValueCount;
    fCodeName.MergeFirstRow(fCodeName2.FirstRow);
    if fValidityFirstRowCount.Value > -1 then
    begin
      FirstRowCount := FirstRowCount - fCodeName.FirstRowValueCount;
      if not fValidityFirstRowCount.Between(FirstRowCount) then Exit;
    end;

    fCodeName.BatchNumber := fBatchNumber;
    fRest.Add(fCodeName, True);
    fCodeNameDeltaCount := fCodeNameDeltaCount + 1;

    if fCodeNameDeltaCount >= 100000 then
    begin
      fRest.Commit(1, True);
      fRest.TransactionBegin(TSQLCodeName, 1);
      fCodeNameDeltaCount := 0;
    end;
  end,
  function(FirstNumber, GroupCount: Cardinal): Boolean
  begin
    fFirstNumberGroupCount := GroupCount;
    Result := Terminated;
    if not Result and (fBatchNumberGroupCountEachFirstNumber > 0) then
      Result := GroupCount >= fBatchNumberGroupCountEachFirstNumber;
  end);
end;

procedure TDataComputer.GroupCodeName;
begin
  if Terminated then Exit;
  if not fMainApp then
    fBatchNumberRowCount := fRest.TableRowCount(TSQLCodeName);

  fGroupCodeNameFinished := False;
  //fCodeName.FillPrepare(fRest, 'BatchNumber = ?', [fBatchNumber - 1]);
  fFirstNumber := fConsumerNumber;
  fCodeNameDeltaCount := 0;
  fRest.TransactionBegin(TSQLCodeName, 1);
  {while (fFirstNumber <= Ceil((fBatchNumberRowCount - 1) / 2)) and not Terminated do
  begin
    if (fBatchNumberGroupFirstNumberCount > 0) and (fFirstNumber > fBatchNumberGroupFirstNumberCount) then Break;

    GroupCodeName(fBatchNumberRowCount, fFirstNumber);
    if fBatchNumberRowCount - fFirstNumber <> fFirstNumber then
      GroupCodeName(fBatchNumberRowCount, fBatchNumberRowCount - fFirstNumber);

    fFirstNumber := fFirstNumber + fConsumerCount;
  end;}
  while (fFirstNumber < fBatchNumberRowCount) and not Terminated do
  begin
    if (fBatchNumberGroupFirstNumberCount > 0) and (fFirstNumber > fBatchNumberGroupFirstNumberCount) then Break;

    fCodeNameCacheIndex := 0;
    GroupCodeName(fBatchNumberRowCount, fFirstNumber);

    fFirstNumber := fFirstNumber + fConsumerCount;
  end;
  if Terminated then Exit;
  fRest.Commit(1, True);

  fCodeName.FillPrepare(fRest, '1 = 2', []);
  if not fMainApp then
    fRest.Delete(TSQLCodeName, 'BatchNumber < ?', [fBatchNumber]);

  fFirstNumber := 0;
  fGroupCodeNameFinished := True;
end;

procedure TDataComputer.ConsumerGroupCodeName;
var
  s: string;
  i, j, PageIndex, FirstRowCount: Integer;
  Settings, ValidityFirstRowCount: Variant;
  WorkFinish: Boolean;
begin
  if Terminated then Exit;

  TDocVariant.New(Settings);
  TDocVariant.New(ValidityFirstRowCount);

  for i := fLastGroupNumber + 1 to fGroupCount do
  begin
    if Terminated then Exit;
    fBatchNumber := i;
    //通知设置
    Settings.KeepCodeNameValueCount := fKeepCodeNameValueCount;

    fBatchNumberGroupFirstNumberCount := 0;
    fBatchNumberGroupCountEachFirstNumber := 0;
    Settings.BatchNumberGroupFirstNumberCount := 0;
    Settings.BatchNumberGroupCountEachFirstNumber := 0;
    if (fBatchNumber >= fGroupNumber) and (fBatchNumber <= fGroupNumber2) then
    begin
      fBatchNumberGroupFirstNumberCount := fGroupFirstNumberCount;
      fBatchNumberGroupCountEachFirstNumber := fGroupCountEachFirstNumber;
      Settings.BatchNumberGroupFirstNumberCount := fGroupFirstNumberCount;
      Settings.BatchNumberGroupCountEachFirstNumber := fGroupCountEachFirstNumber;
    end;

    fValidityFirstRowCount.Value := -1;
    fValidityFirstRowCount.Value2 := -1;
    ValidityFirstRowCount.Value := -1;
    ValidityFirstRowCount.Value2 := -1;
    if (fBatchNumber >= fGroupNumber3) and (fBatchNumber <= fGroupNumber3) then
    begin
      fValidityFirstRowCount.Value := fValidityCountEachGroupNumber[fBatchNumber - fGroupNumber3].Value;
      fValidityFirstRowCount.Value2 := fValidityCountEachGroupNumber[fBatchNumber - fGroupNumber3].Value2;
      ValidityFirstRowCount.Value := fValidityFirstRowCount.Value;
      ValidityFirstRowCount.Value2 := fValidityFirstRowCount.Value2;
    end;
    Settings.ValidityFirstRowCount := ValidityFirstRowCount;

    fRest.Delete(TSQLCodeName, 'BatchNumber = ?', [fBatchNumber]);
    fBatchNumberRowCount := fRest.TableRowCount(TSQLCodeName);
    Settings.BatchNumber := fBatchNumber;
    Settings.BatchNumberRowCount := fBatchNumberRowCount;

    fBatchNumberKeepCodeNameValueCount := 0;
    fBatchNumberKeepCodeNameValueCount2 := 0;
    Settings.BatchNumberKeepCodeNameValueCount := 0;
    Settings.BatchNumberKeepCodeNameValueCount2 := 0;
    if fKeepExportCodeNameValueCount and (fBatchNumber = fGroupCount) then
    begin
      fBatchNumberKeepCodeNameValueCount := fExportCodeNameValueCount;
      fBatchNumberKeepCodeNameValueCount2 := fExportCodeNameValueCount2;
      Settings.BatchNumberKeepCodeNameValueCount := fExportCodeNameValueCount;
      Settings.BatchNumberKeepCodeNameValueCount2 := fExportCodeNameValueCount2;
    end;
    //通知其它线程初始化
    for j := fConsumers.List.Count - 1 downto 0 do
    begin
      if Terminated then Exit;
      try
        fConsumers.List[j].Value.Init;
      except
        fConsumers.Remove(fConsumers.List[j]);
      end;
    end;
    //检查其它线程初始化情况
    WorkFinish := True;
    repeat
      for j := fConsumers.List.Count - 1 downto 0 do
      begin
        if Terminated then Exit;
        WorkFinish := fConsumers.List[j].Value.InitFinish;
        if not WorkFinish then Break;
      end;
      if not WorkFinish then Sleep(500);
    until WorkFinish or Terminated;
    fConsumerCount := fConsumers.List.Count + 1;
    //通知其它线程同步代号
    for j := fConsumers.List.Count - 1 downto 0 do
    begin
      if Terminated then Exit;
      fConsumers.List[j].Value.SyncCodeName(fBatchNumber, fBatchNumberRowCount);
    end;
    //检查其它线程同步代号情况
    WorkFinish := True;
    repeat
      for j := fConsumers.List.Count - 1 downto 0 do
      begin
        if Terminated then Exit;
        WorkFinish := fConsumers.List[j].Value.SyncCodeNameFinish;
        if not WorkFinish then Break;
      end;
      if not WorkFinish then Sleep(500);
    until WorkFinish or Terminated;
    //通知其它线程组合代号
    Settings.ConsumerCount := fConsumerCount;
    for j := fConsumers.List.Count - 1 downto 0 do
    begin
      if Terminated then Exit;
      Settings.ConsumerNumber := j + 2;
      fConsumers.List[j].Value.GroupCodeName(Settings);
    end;
    //主工作线程组合
    fConsumerNumber := 1;
    GroupCodeName;
    //检查其它线程组合代号情况
    WorkFinish := True;
    repeat
      for j := fConsumers.List.Count - 1 downto 0 do
      begin
        if Terminated then Exit;
        WorkFinish := fConsumers.List[j].Value.GroupCodeNameFinish;
        if not WorkFinish then Break;
      end;
      if not WorkFinish then Sleep(500);
    until WorkFinish or Terminated;
    if (fBatchNumber < fGroupCount) or ((fBatchNumber = fGroupCount) and not fKeepLastBatchCodeNameOnEachComputer) then
    begin
      //通知其它线程上传代号
      for j := fConsumers.List.Count - 1 downto 0 do
      begin
        if Terminated then Exit;
        fConsumers.List[j].Value.UploadCodeName;
        //检查其它线程上传代号情况
        repeat
          WorkFinish := fConsumers.List[j].Value.UploadCodeNameFinish;

          if not WorkFinish then Sleep(500);
        until WorkFinish or Terminated;
      end;
    end;
    //通知其它线程结束
    for j := fConsumers.List.Count - 1 downto 0 do
    begin
      if Terminated then Exit;
      fConsumers.List[j].Value.Terminate;
    end;

    if Terminated then Exit;
    fRest.Delete(TSQLCodeName, 'BatchNumber < ?', [fBatchNumber]);
    fLastGroupNumber := fBatchNumber;
    fKeyValue.SetKeyValue('LastGroupNumber', fLastGroupNumber);
    if fLastGroupNumber = 1 then
      fRest.Execute('UPDATE FirstRow SET Grouped = 1');
    if fLastGroupNumber = fGroupCount then
    begin
      fFirstRowUpdated := False;
      fKeyValue.SetKeyValue('FirstRowUpdated', fFirstRowUpdated);
    end;
  end;
end;

procedure TDataComputer.MergeFirstRow;
var
  FirstRow: TSQLFirstRow;
  CodeName: TSQLCodeName;
  PageIndex: Integer;
  s: string;
begin
  if Terminated then Exit;
  TSQLFirstRow.AutoFree(FirstRow, fRest, 'Grouped = 0', []);
  if FirstRow.FillTable.RowCount = 0 then Exit;
  TSQLCodeName.AutoFree(CodeName);

  s := 'LIMIT ? OFFSET ?';
  PageIndex := 0;
  repeat
    CodeName.FillPrepare(fDatabase, s, [EachPageRowCount, PageIndex * EachPageRowCount]);
    fRest.TransactionBegin(TSQLCodeName, 1);
    try
      while CodeName.FillOne and not Terminated do
      begin
        FirstRow.FillRewind;
        while FirstRow.FillOne and not Terminated do
        begin
          if FirstRow.CodeName.Contains(CodeName.ToWordDynArray) then
          begin
            CodeName.MergeFirstRow([FirstRow.Value]);
            fRest.Update(CodeName);
          end;
        end;
      end;
      fRest.Commit(1, True);
    except
      on e: Exception do
      begin
        fRest.RollBack(1);
        raise Exception.Create(e.Message);
      end;
    end;
    Inc(PageIndex);
  until CodeName.FillTable.RowCount = 0;

  fFirstRowUpdated := False;
  fKeyValue.SetKeyValue('FirstRowUpdated', fFirstRowUpdated);

  fRest.TransactionBegin(TSQLFirstRow, 1);
  try
    FirstRow.FillRewind;
    while FirstRow.FillOne and not Terminated do
    begin
      FirstRow.Grouped := True;
      fRest.Update(FirstRow);
    end;
    fRest.Commit(1, True);
  except
    on e: Exception do
    begin
      fRest.RollBack(1);
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
  if Terminated then Exit;
  if not fExportFile2
    or (fFirstRowUpdated
    and (fBuildFirstRowValueCount = fExportCodeNameValueCount)
    and (fBuildFirstRowValueCount2 = fExportCodeNameValueCount2))
  then Exit;

  fRest.Execute('UPDATE FirstRow SET RowCount = 0');
  fRest.Delete(TSQLFirstRowCodeName, '', []);

  TSQLCodeName.AutoFree(CodeName);
  TSQLFirstRow.AutoFree(FirstRow);
  TSQLFirstRowCodeName.AutoFree(FirstRowCodeName);

  sPage := 'ValueCount >= ? AND ValueCount <= ? ORDER BY ValueCount DESC LIMIT ? OFFSET ?';
  PageIndex := 0;
  repeat
    CodeName.FillPrepare(fRest, sPage, [fExportCodeNameValueCount,
      fExportCodeNameValueCount2, EachPageRowCount, PageIndex * EachPageRowCount]);
    fRest.TransactionBegin(TSQLFirstRowCodeName, 1);
    try
      while CodeName.FillOne and not Terminated do
      begin
        FirstRowCodeName.CodeName := CodeName.Value;
        FirstRowCodeName.CodeNameValueCount := CodeName.ValueCount;
        for i := Low(CodeName.FirstRow) to High(CodeName.FirstRow) do
        begin
          FirstRow.FillPrepare(fRest, 'Value = ?', [CodeName.FirstRow[i]]);
          if FirstRow.FillOne then
          begin
            FirstRow.RowCount := FirstRow.RowCount + 1;
            fRest.Update(FirstRow);
          end;

          FirstRowCodeName.FirstRow := CodeName.FirstRow[i];
          fRest.Add(FirstRowCodeName, True);
        end;
      end;
      fRest.Commit(1, True);
    except
      on e: Exception do
      begin
        fRest.RollBack(1);
        raise Exception.Create(e.Message);
      end;
    end;

    Inc(PageIndex);
  until Terminated or (CodeName.FillTable.RowCount = 0);

  FirstRow.FillPrepare(fRest, 'RowCount > 0 ORDER BY Value', []);
  fRest.TransactionBegin(TSQLFirstRow, 1);
  try
    while FirstRow.FillOne and not Terminated do
    begin
      if FirstRow.FillCurrentRow = 2 then
        FirstRow.RowSpacing := FirstRow.Value - fCompareSpacing - 1
      else
        FirstRow.RowSpacing := FirstRow.Value - LastFirstRowValue;
      LastFirstRowValue := FirstRow.Value;

      fRest.Update(FirstRow);
    end;
    fRest.Commit(1, True);
  except
    on e: Exception do
    begin
      fRest.RollBack(1);
      raise Exception.Create(e.Message);
    end;
  end;

  fFirstRowUpdated := True;
  fBuildFirstRowValueCount := fExportCodeNameValueCount;
  fBuildFirstRowValueCount2 := fExportCodeNameValueCount2;
  fKeyValue.SetKeyValue('FirstRowUpdated', fFirstRowUpdated);
  fKeyValue.SetKeyValue('BuildFirstRowValueCount', fBuildFirstRowValueCount);
  fKeyValue.SetKeyValue('BuildFirstRowValueCount2', fBuildFirstRowValueCount2);
end;

function TDataComputer.RebuildFile(RowCount: Cardinal): Boolean;
begin
  Result := RowCount >= EachFileRowCount;
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
  Result := Result + Format('（%d）.【最末第%s（空白及文字）行】- %d', [Sender.FileNo, sSub, Sender.FileNo - 1]);
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
  else i := Sender.FileName.IndexOf('0');
  Result := Sender.FileName.SubString(0, i + 1);
  Result := Result + Format('（%d）.【最末第%s=行】- %d', [Sender.FileNo, sSub, Sender.FileNo - 1]);
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
  case fCompareMode of
    cmVert: Result := CodeName.ToString;
    else
    begin
      for v in CodeName do
      begin
        if not Result.IsEmpty then Result := Result + '、';
        case fCompareMode of
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
  TxtFileName := '0（1）.［ 各（第 N 行为首行）的（符合条件的代号）］第 %d 次（ 依次遍历 ）导出［ 相同（符合条件的代号）］.txt';
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
      CodeName.FillPrepare(fRest, sPage, [BatchNumber, EachPageRowCount, PageIndex * EachPageRowCount]);
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
  if fExportFile3 then
  begin
    if fExportLite then TxtFileName := '（2-1）.txt'
    else
    begin
      TxtFileName := '（2-1）（1）.【排列】【“%d-%d”个[相同组合、不同首行]的组合[不同首行数：最多→少]】（1）.txt';
      TxtFileName := Format(TxtFileName, [fExportCodeNameValueCount, fExportCodeNameValueCount2]);
    end;
    FileName := fExportDirectory + TxtFileName;
    fr := TFileWriter.Create(FileName);
    fr.RebuildFileNameEvent := RebuildFileName2;
  end;

  if fExportFile4 then
  begin
    if fExportLite then TxtFileName := '（6）.txt'
    else
    begin
      TxtFileName := '（6）（1）.【简化】【“%d-%d”个[相同组合、不同首行]的组合】.txt';
      TxtFileName := Format(TxtFileName, [fExportCodeNameValueCount, fExportCodeNameValueCount2]);
    end;
    FileName2 := fExportDirectory2 + TxtFileName;
    fr2 := TFileWriter.Create(FileName2);
    fr2.RebuildFileNameEvent := RebuildFileName2;
  end;
  try
    if Assigned(fr) then
    begin
      s := TPath.GetFileNameWithoutExtension(FileName) + ':';
      fr.WriteLn('');
      fr.WriteLn(s);
      fr.WriteLn('');
    end;
    if Assigned(fr2) then
    begin
      s := TPath.GetFileNameWithoutExtension(FileName2) + ':';
      fr2.WriteLn('');
      fr2.WriteLn(s);
      fr2.WriteLn('');
    end;

    TSQLCodeName.AutoFree(CodeName);
    sPage := 'ValueCount >= ? AND ValueCount <= ? ORDER BY ValueCount DESC LIMIT ? OFFSET ?';
    RowNumber := 0;
    PageIndex := 0;
    repeat
      CodeName.FillPrepare(fRest, sPage, [fExportCodeNameValueCount,
        fExportCodeNameValueCount2, EachPageRowCount, PageIndex * EachPageRowCount]);
      while CodeName.FillOne and not Terminated do
      begin
        RowNumber := RowNumber + 1;

        if Assigned(fr) then
        begin
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
        end;

        if Assigned(fr2) then
        begin
          s := '%d.[ %d个组合；代号：（%s）]';
          s := Format(s, [
            RowNumber,
            CodeName.ValueCount,
            CodeNameToString(CodeName.ToWordDynArray)
          ]);
          fr2.WriteLn('');
          fr2.WriteLn(s);
        end;

        for i := High(CodeName.FirstRow) downto Low(CodeName.FirstRow) do
        begin
          if Assigned(fr) then
          begin
            s := '【%d】.第%d行为首行';
            s := Format(s, [Length(CodeName.FirstRow) - i, CodeName.FirstRow[i]]);
            fr.WriteLn('');
            fr.WriteLn(s);
          end;

          if i = High(CodeName.FirstRow) then
          begin
            if Assigned(fr2) then
            begin
              s := '（第%d行为首行）';
              s := Format(s, [CodeName.FirstRow[i]]);
              fr2.WriteLn('');
              fr2.WriteLn(s);
            end;
          end;
        end;

        if RowNumber mod EachFileRowNumber = 0 then
        begin
          if Assigned(fr) then fr.BuildActiveFileName;
          if Assigned(fr2) then fr2.BuildActiveFileName;
        end;
      end;

      Inc(PageIndex);
    until Terminated or (CodeName.FillTable.RowCount = 0);

    if Assigned(fr) then
    begin
      fr.WriteFinish;
      fr.RenameLastFile(RebuildFileName2(fr, RowNumber));
    end;
    if Assigned(fr2) then
    begin
      fr2.WriteFinish;
      fr2.RenameLastFile(RebuildFileName2(fr2, RowNumber));
    end;
  finally
    if Assigned(fr) then fr.Free;
    if Assigned(fr2) then fr2.Free;
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
      if (fCompareMode = cmSlant) and s.Contains('，同行数') then
        s := s.Substring(0, s.IndexOf('，同行数')) + '）';
      RowSpacingList.Values[sRowSpacing] := s;
    end;
  end;
begin
  if fExportLite then TxtFileName := '（1）.txt'
  else
  begin
    TxtFileName := '（1）（1）.【排列】【“%d-%d”个以上[相同首行、不同组合]的组合】.txt';
    TxtFileName := Format(TxtFileName, [fExportCodeNameValueCount, fExportCodeNameValueCount2]);
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

    TSQLFirstRow.AutoFree(FirstRow, fRest, 'RowCount > 0 ORDER BY Value DESC', []);
    TSQLFirstRowCodeName.AutoFree(FirstRowCodeName);

    RowSpacingList := TStringList.Create;
    try
      while FirstRow.FillOne do
      begin
        if FirstRow.FillCurrentRow = 2 then
        begin
          case fCompareMode of
            cmSlant:
            begin
              sFirstRow := '';
              if not fExportLite then
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
              if not fExportLite then
              begin
                sFirstRow := '[ %s连（第%d行为首行）]；';
                sFirstRow := Format(sFirstRow, [fCompareModeString, fRowCount + 1]);
              end;
              s := '1.%s[ 邻行距 ↓%d ]';
              s := Format(s, [sFirstRow, fRowCount + 1 - FirstRow.Value]);
            end;
          end;
          AddRowSpacing(fRowCount + 1 - FirstRow.Value, s);
        end;

        case fCompareMode of
          cmSlant:
          begin
            sFirstRow := '';
            if not fExportLite then
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
            if not fExportLite then
            begin
              sFirstRow := '[ %s连（第%d行为首行）]；';
              sFirstRow := Format(sFirstRow, [fCompareModeString, FirstRow.Value]);
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
        case fCompareMode of
          cmSlant:
          begin
            sFirstRow := '';
            if not fExportLite then
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
            if not fExportLite then
            begin
              sFirstRow := '[ %s连（第%d行为首行）]；';
              sFirstRow := Format(sFirstRow, [fCompareModeString, fRowCount + 1]);
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

      case fCompareMode of
        cmSlant:
        begin
          sFirstRow := '';
          if not fExportLite then
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
          if not fExportLite then
          begin
            sFirstRow := '[ %s连（第%d行为首行）]；';
            sFirstRow := Format(sFirstRow, [fCompareModeString, FirstRow.Value]);
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
        FirstRowCodeName.FillPrepare(fRest, 'FirstRow = ? LIMIT ? OFFSET ?',
          [FirstRow.Value, EachPageRowCount, PageIndex * EachPageRowCount]);
        while FirstRowCodeName.FillOne and not Terminated do
        begin
          RowNumber := RowNumber + 1;
          case fCompareMode of
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

    case fCompareMode of
      cmVert:
      begin
        s := '%d.[（第%d行为最末首行）直连（第%d-1行）]';
        s := Format(s, [
          FirstRow.FillTable.RowCount + 2,
          fVertCompareSpacing + 1,
          fVertCompareSpacing
        ]);
        fr.WriteLn('');
        fr.WriteLn(s);
      end;
      cmSlant:
      begin
        s := '%d.（第%d行为最末首行）';
        s := Format(s, [
          FirstRow.FillTable.RowCount + 2,
          fSlantCompareSpacing + 1
        ]);
        fr.WriteLn('');
        fr.WriteLn(s);
      end;
      cmVertSlant:
      begin
        s := '%d.[（第%d行为最末首行）直连（第%d-%d行）]';
        s := Format(s, [
          FirstRow.FillTable.RowCount + 2,
          fCompareSpacing + 1,
          fCompareSpacing,
          fCompareSpacing + 1 - fVertCompareSpacing
        ]);
        fr.WriteLn('');
        fr.WriteLn(s);
        s := '%d.[（第%d行为最末首行）斜连（第%d-%d行）]';
        s := Format(s, [
          FirstRow.FillTable.RowCount + 3,
          fCompareSpacing + 1,
          fCompareSpacing,
          fCompareSpacing + 1 - fSlantCompareSpacing
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
  if Terminated then Exit;
  sMode := '1';
  if fDataMode = 1 then sMode := '3';
  if Length(fIntervalValues) > 1 then
  begin
    sMode := '2';
    if fDataMode = 1 then sMode := '4';
  end;
  sCompareMode := '';
  if not fExportLite  then sCompareMode := Format('（%s连）', [fCompareModeString]);
  fExportDirectory := fDirectory + Format('\导出结果%s（第%s模式）\', [sCompareMode, sMode]);
  fExportDirectory2 := fExportDirectory + '（1）.【排列】-（6）.【简化】\';

  if not TDirectory.Exists(fExportDirectory) then
    TDirectory.CreateDirectory(fExportDirectory);
  for s in TDirectory.GetFiles(fExportDirectory, '*.txt') do TFile.Delete(s);
  if not TDirectory.Exists(fExportDirectory2) then
    TDirectory.CreateDirectory(fExportDirectory2);
  for s in TDirectory.GetFiles(fExportDirectory2, '*.txt') do TFile.Delete(s);

  if fExportFile then
  begin
    DataList := fRest.MultiFieldValues(TSQLCodeName,
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
  if fExportFile2 then SaveToFile3;
  if fExportFile3 or fExportFile4 then SaveToFile2;
end;

procedure TDataComputer.Execute;
var
  ErrorMessage: string;
begin
  if Now > 43861 then Exit; //1.1

  fStopwatch := TStopwatch.StartNew;
  try
    try
      Init;
      if fMainApp then
      begin
        LoadRow;
        CompareRow;
        BuildCodeName;
        ConsumerGroupCodeName;
        MergeFirstRow;
        UpdateFirstRow;
      end
      else GroupCodeName;
      //ConsumerExportFile;
      ExportData;

      if Terminated then Exit;
      if fMainApp then
      begin
        TThread.Queue(nil, procedure
        begin
          ShowMessage('查询完成');
        end);
      end;
    except
      on e: Exception do
      begin
        ErrorMessage := e.Message;
        TThread.Synchronize(nil, procedure
        begin
          ShowMessage(ErrorMessage);
        end);
      end;
    end;
  finally
    fStopwatch.Stop;
  end;
end;

procedure TDataComputer.BuildValidityCountEachGroupNumber;
var
  i: Integer;
begin
  SetLength(fValidityCountEachGroupNumber, fGroupNumber4 - fGroupNumber3 + 1);
  for i := Low(fValidityCountEachGroupNumber) to High(fValidityCountEachGroupNumber) do
  begin
    fValidityCountEachGroupNumber[i].Value := -1;
    fValidityCountEachGroupNumber[i].Value2 := -1;
  end;
end;

constructor TSyncCodeName.Create(aRest: TSQLRestServer; aClient: TSQLHttpClientWebsockets);
begin
  inherited Create(True);
  fRest := aRest;
  fClient := aClient;
  fSyncFinished := False;
end;

procedure TSyncCodeName.Execute;
var
  RowCount, PageIndex: Int64;
  ErrorMessage: string;
  CodeName: TSQLCodeName;
begin
  TSQLCodeName.AutoFree(CodeName);
  try
    fRest.Delete(TSQLCodeName, '', []);

    PageIndex := 0;
    repeat
      CodeName.FillPrepare(fClient, 'BatchNumber = ? LIMIT ? OFFSET ?',
        [fBatchNumber - 1, EachPageRowCount, PageIndex * EachPageRowCount]);
      fRest.TransactionBegin(TSQLCodeName, 1);
      try
        while CodeName.FillOne and not Terminated do fRest.Add(CodeName, True);

        fRest.Commit(1, True);
      except
        fRest.RollBack(1);
      end;

      fSyncFinished := CodeName.FillTable.RowCount = 0;

      Inc(PageIndex);
    until Terminated or fSyncFinished;

    //fRest.RecordVersionSynchronizeSlave(TSQLCodeName, fClient);
    if Terminated then Exit;
    RowCount := fRest.TableRowCount(TSQLCodeName);
    fSyncFinished := RowCount = fBatchNumberRowCount;
    if not fSyncFinished then
      raise Exception.CreateFmt('同步代号数据记录数不相同：%d %d', [RowCount, fBatchNumberRowCount]);
  except
    on e: Exception do
    begin
      ErrorMessage := e.Message;
      TThread.Synchronize(nil, procedure
      begin
        ShowMessage(ErrorMessage);
      end);
      raise;
    end;
  end;
end;

constructor TUploadCodeName.Create(aRest: TSQLRestServer; aClient: TSQLHttpClientWebsockets);
begin
  inherited Create(True);
  fRest := aRest;
  fClient := aClient;
  fUploadFinished := False;
end;

procedure TUploadCodeName.Execute;
var
  CodeName: TSQLCodeName;
  PageIndex: Integer;
  ErrorMessage: string;
begin
  TSQLCodeName.AutoFree(CodeName);
  try
    PageIndex := 0;
    repeat
      CodeName.FillPrepare(fRest, 'LIMIT ? OFFSET ?', [EachPageRowCount, PageIndex * EachPageRowCount]);
      fClient.TransactionBegin(TSQLCodeName);
      try
        while CodeName.FillOne and not Terminated do fClient.Add(CodeName, True);
        fClient.Commit(1, True);
      except
        fClient.RollBack(1);
        raise ;
      end;

      fUploadFinished := CodeName.FillTable.RowCount = 0;

      Inc(PageIndex);
    until Terminated or fUploadFinished;
  except
    on e: Exception do
    begin
      ErrorMessage := e.Message;
      TThread.Synchronize(nil, procedure
      begin
        ShowMessage(ErrorMessage);
      end);
      raise;
    end;
  end;
end;

procedure BuildDatabase;
begin
  fDatabase := TSQLRestServerDB.CreateWithOwnModel(
    [TSQLKeyValue, TSQLRow, TSQLFirstRow, TSQLCodeName, TSQLFirstRowCodeName],
    fDirectory + 'Data'
  );
  fDatabase.CreateMissingTables;
  fDatabase.CreateSQLIndex(TSQLCodeName, 'BatchNumber', False);
  fDatabase.CreateSQLIndex(TSQLCodeName, 'ValueCount', False);
  fDatabase.CreateSQLIndex(TSQLCodeName, 'FirstRowValueCount', False);
  fDatabase.CreateSQLIndex(TSQLFirstRowCodeName, 'FirstRow', False);
  fDatabase.ServiceDefine(TService, [IService], sicShared).SetOptions([], [optExecLockedPerInterface]);
  //sqlite3.create_function(fDatabase.DB.DB, 'WordDynArrayEquals', 2, SQLITE_ANY,
  //  TypeInfo(TWordDynArray), Int64DynArrayEquals, nil, nil);
  fKeyValue := TSQLKeyValue.Create;
  fKeyValue.SetRest(fDatabase);
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

  fMainApp := False;
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
  fMainApp := True;
  TInterfaceFactory.RegisterInterfaces([TypeInfo(IService), TypeInfo(INotifyCallback)]);

  BuildDatabase;

  with TIdIPWatch.Create do
  begin
    try
      fLocalIP := LocalIP;
    finally
      Free;
    end;
  end;

  try
    fServer := TSQLHttpServer.Create('8888', [fDatabase], '+', useBidirSocket);
    fServer.WebSocketsEnable(fDatabase, 'encryptionkey');

    fConsumers := TConsumers.Create
  except
  end;

finalization
  DisconnectServer;

  if Assigned(fThread) then
  begin
    fThread.Terminate;
    fThread.WaitFor;
    FreeAndNil(fThread);
  end;

  if Assigned(fDataComputer) then
  begin
    fDataComputer.Terminate;
    fDataComputer.WaitFor;
    FreeAndNil(fDataComputer);
  end;

  if Assigned(fServer) then FreeAndNil(fServer);
  if Assigned(fConsumers) then FreeAndNil(fConsumers);
  if Assigned(fKeyValue) then FreeAndNil(fKeyValue);
  if Assigned(fDatabase) then FreeAndNil(fDatabase);

end.
