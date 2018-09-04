unit ufrmMain;

interface

uses
  uDataComputer,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.Threading, System.Types,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.FileCtrl,
  DBGridEhGrouping, ToolCtrlsEh,
  DBGridEhToolCtrls, DynVarsEh, Vcl.ExtCtrls, Vcl.StdCtrls, EhLibVCL, GridsEh,
  DBAxisGridsEh, DBGridEh, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FireDAC.Stan.Async, FireDAC.DApt;

type
  TfrmMain = class(TForm)
    dbgrdDataTable: TDBGridEh;
    OpenDialog: TOpenDialog;
    Panel1: TPanel;
    btnQuerySameColumns: TButton;
    Panel4: TPanel;
    pnlUseTime: TPanel;
    btnRearrangeClearColumn: TButton;
    btnSortClearColumn: TButton;
    btnCompareClearColumn: TButton;
    btnBuildClearColumn: TButton;
    btnQueryData3: TButton;
    btnInputResultData: TButton;
    Panel3: TPanel;
    dbgrdResultData: TDBGridEh;
    fdmtCompareResult: TFDMemTable;
    dsCompareResult: TDataSource;
    dsDataTable: TDataSource;
    btnIntervalValueSet: TButton;
    fdmtDataTable: TFDMemTable;
    chkReverseOrder: TCheckBox;
    Label1: TLabel;
    Button1: TButton;
    Button3: TButton;
    btnExportToFile: TButton;
    pnlQueryData2: TPanel;
    Splitter1: TSplitter;
    Panel5: TPanel;
    pnlQueryData3: TPanel;
    procedure btnSortClearColumnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRearrangeClearColumnClick(Sender: TObject);
    procedure btnCompareClearColumnClick(Sender: TObject);
    procedure btnBuildClearColumnClick(Sender: TObject);
    procedure btnQueryData3Click(Sender: TObject);
    procedure fdmtCompareResultNewRecord(DataSet: TDataSet);
    procedure btnQuerySameColumnsClick(Sender: TObject);
    procedure btnIntervalValueSetClick(Sender: TObject);
    procedure btnInputResultDataClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnExportToFileClick(Sender: TObject);
  private
    fDataComputer: TDataComputer;
    procedure OnStateChange(Enalbed: Boolean);
    procedure BuildDataTableGridColumn;
    procedure dbgrdDataTableAdvDrawDataCell(Sender: TCustomDBGridEh;
      Cell, AreaCell: TGridCoord; Column: TColumnEh; const ARect: TRect;
      var Params: TColCellParamsEh; var Processed: Boolean);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  ufrmInput, ufrmQueryConfig3, uTimer, ufrmIntervalValueSet, ufrmSameColumnsSet,
  ufrmExportFileSettings;

{$R *.dfm}

procedure TfrmMain.OnStateChange(Enalbed: Boolean);
begin
  btnQuerySameColumns.Enabled := Enalbed;
end;

procedure TfrmMain.BuildDataTableGridColumn;
var
  i: Integer;
  IntervalValue, ColNo: Word;
begin
  dbgrdDataTable.BeginLayout;
  dbgrdDataTable.OnAdvDrawDataCell := nil;
  try
    dbgrdDataTable.Columns.Clear;
    with dbgrdDataTable.Columns.Add do
    begin
      FieldName := 'ValueCount';
      Alignment := TAlignment.taCenter;
      Title.Alignment := TAlignment.taCenter;
      if Length(fDataComputer.IntervalValues) > 1 then Title.Caption := '��.��������|��һ����'
      else Title.Caption := '��.��������';
      Width := 100;
    end;
    if Length(fDataComputer.IntervalValues) > 1 then
    begin
      with dbgrdDataTable.Columns.Add do
      begin
        FieldName := 'ValueCount2';
        Alignment := TAlignment.taCenter;
        Title.Alignment := TAlignment.taCenter;
        Title.Caption := '��.��������|�ڶ�����';
        Width := 100;
      end;
    end;
    with dbgrdDataTable.Columns.Add do
    begin
      FieldName := 'ConformColCount';
      Alignment := TAlignment.taCenter;
      Title.Alignment := TAlignment.taCenter;
      if Length(fDataComputer.IntervalValues) > 1 then Title.Caption := '��.��ͬ����|��һ����'
      else Title.Caption := '��.��ͬ����';
      Width := 100;
    end;
    if Length(fDataComputer.IntervalValues) > 1 then
    begin
      with dbgrdDataTable.Columns.Add do
      begin
        FieldName := 'ConformColCount2';
        Alignment := TAlignment.taCenter;
        Title.Alignment := TAlignment.taCenter;
        Title.Caption := '��.��ͬ����|�ڶ�����';
        Width := 100;
      end;
    end;
    with dbgrdDataTable.Columns.Add do
    begin
      FieldName := 'FolderNo';
      Alignment := TAlignment.taCenter;
      Title.Alignment := TAlignment.taCenter;
      Title.Caption := '��.�����ļ����к�';
      Width := 140;
    end;
    with dbgrdDataTable.Columns.Add do
    begin
      FieldName := 'FileNo';
      Alignment := TAlignment.taCenter;
      Title.Alignment := TAlignment.taCenter;
      Title.Caption := '��.�����ı��У���β��';
      Width := 200;
    end;
    with dbgrdDataTable.Columns.Add do
    begin
      FieldName := 'RowNo';
      Alignment := TAlignment.taCenter;
      Title.Alignment := TAlignment.taCenter;
      Title.Caption := '��.�����ı��ڼ���';
      Width := 140;
    end;
    IntervalValue := 0;
    for i := Low(fDataComputer.IntervalValues) to High(fDataComputer.IntervalValues) do
    begin
      if i > 0 then IntervalValue := IntervalValue + fDataComputer.IntervalValues[i - 1];
      for ColNo := 1 to fDataComputer.IntervalValues[i] do
      begin
        with dbgrdDataTable.Columns.Add do
        begin
          Tag := IntervalValue + ColNo;
          FieldName := 'Field' + ((Tag - 1) div 64 + 1).ToString;
          Alignment := TAlignment.taCenter;
          Title.Alignment := TAlignment.taCenter;
          Title.Caption := ColNo.ToString;
          if ColNo < 10 then Title.Caption := '0' + Title.Caption;
          Width := 45;
        end;
      end;
    end;
  finally
    dbgrdDataTable.EndLayout;
    dbgrdDataTable.OnAdvDrawDataCell := dbgrdDataTableAdvDrawDataCell;
  end;
end;

procedure TfrmMain.btnExportToFileClick(Sender: TObject);
begin
  if not ExportFileSet then Exit;

  fDataComputer.ExportToFile(
    frmExportFileSettings.ExportFileDirectory,
    frmExportFileSettings.EachFileRowCount,
    frmExportFileSettings.rbSubFileDirectory.Checked
  );
  ShowMessage('��������������');
end;

procedure TfrmMain.dbgrdDataTableAdvDrawDataCell(Sender: TCustomDBGridEh;
  Cell, AreaCell: TGridCoord; Column: TColumnEh; const ARect: TRect;
  var Params: TColCellParamsEh; var Processed: Boolean);
var
  i, ColNo: Integer;
  i64, v: Int64;
  IntervalValue: Word;
begin
  i64 := 1;
  Params.Background := clWhite;
  if TryStrToInt(Column.Title.Caption, ColNo) and Assigned(Column.Field) then
  begin
    v := Column.Field.AsLargeInt;
    Params.Text := '';
    if v = i64 shl (64 - Column.Tag) or v then
    begin
      Params.Text := Column.Title.Caption;

      if Assigned(frmSameColumnsSet) then
      begin
        for i := Low(frmSameColumnsSet.Values) to High(frmSameColumnsSet.Values) do
          if Column.Tag = frmSameColumnsSet.Values[i] then
          begin
            Params.Background := clBlack;
            with Sender as TDBGridEh do
            begin
              Canvas.Font.Color := clWhite;
            end;
            Break;
          end;
      end;
    end;
  end;
end;

procedure TfrmMain.btnBuildClearColumnClick(Sender: TObject);
var
  MaxColNo, RangeColNo: Integer;
begin
  OpenDialog.Options := OpenDialog.Options - [ofAllowMultiSelect];
  if not OpenDialog.Execute then Exit;

  IntervalValueSet;
  if frmIntervalValueSet.ModalResult <> mrOK then Exit;

  fDataComputer.BuildClearColumn(OpenDialog.FileName, frmIntervalValueSet.IntervalValues);

  ShowMessage('�������������');
end;

procedure TfrmMain.btnCompareClearColumnClick(Sender: TObject);
var
  FileDirectory: string;
begin
  if not SelectDirectory('���������ļ���', '', FileDirectory) then Exit;
  fDataComputer.CompareClearColumn(FileDirectory);
  ShowMessage('�Ƚ����������');
end;

procedure TfrmMain.btnInputResultDataClick(Sender: TObject);
var
  FileDirectory: string;
begin
  if not SelectDirectory('���������ļ���', '', FileDirectory) then Exit;
  fDataComputer.InputResultData(FileDirectory);
  ShowMessage('���� ( ��ѯ�����ݡ�) ���');
end;

procedure TfrmMain.btnQuerySameColumnsClick(Sender: TObject);
var
  SameValuesChanged: Boolean;
  SameValues: TWordDynArray;
  EachPageRowCount: Word;
begin
  SameColumnsSet(fDataComputer.IntervalValues);
  if frmSameColumnsSet.ModalResult <> mrOK then Exit;
  SameValuesChanged := frmSameColumnsSet.ValueChanged;
  SameValues := frmSameColumnsSet.Values;
  EachPageRowCount := frmSameColumnsSet.EachPageRowCount;

  OnStateChange(False);
  TTask.Create(procedure
  var
    fStopTime: Boolean;
    ConformRowCount: Cardinal;

    procedure StopTime;
    begin
      TThread.Synchronize(nil, procedure
      begin
        fStopTime := True;
      end);
    end;
  begin
    StartTheTime(
      function: Boolean
      begin
        Result := fStopTime;
      end,
      procedure(s: string)
      begin
        pnlUseTime.Caption := s;
      end
    );

    try
      try
        if SameValuesChanged then
        begin
          fDataComputer.CalcConformColumnCount(SameValues);
          ConformRowCount := fDataComputer.GetConformRowCount;
        end;
        fDataComputer.QueryData2(fdmtDataTable, EachPageRowCount);

        StopTime;
        ShowMessage('��ѯ���');
      except
        on e: Exception do raise Exception.Create(e.Message);
      end;
    finally
      StopTime;
      TThread.Synchronize(nil, procedure
      begin
        OnStateChange(True);
      end);
    end;
  end).Start;
end;

procedure TfrmMain.btnQueryData3Click(Sender: TObject);
var
  IdenticalColCount, CompareRowCount: Cardinal;
begin
  OpenDialog.Options := OpenDialog.Options - [ofAllowMultiSelect];
  if not OpenDialog.Execute then Exit;
  if not Assigned(frmQueryConfig3) then frmQueryConfig3 := TfrmQueryConfig3.Create(Self);
  frmQueryConfig3.ShowModal;
  if frmQueryConfig3.ModalResult <> mrOK then Exit;

  fDataComputer.QueryData3(
    fdmtCompareResult,
    OpenDialog.FileName,
    frmQueryConfig3.IntervalValues,
    frmQueryConfig3.IdenticalColCount,
    frmQueryConfig3.CompareRowCount
  );
end;

procedure TfrmMain.btnRearrangeClearColumnClick(Sender: TObject);
begin
  OpenDialog.Options := OpenDialog.Options + [ofAllowMultiSelect];
  if not OpenDialog.Execute then Exit;
  fDataComputer.RearrangeClearColumn(OpenDialog.Files, chkReverseOrder.Checked);
end;

procedure TfrmMain.btnSortClearColumnClick(Sender: TObject);
begin
  OpenDialog.Options := OpenDialog.Options + [ofAllowMultiSelect];
  if not OpenDialog.Execute then Exit;
  fDataComputer.SortClearColumn(OpenDialog.Files);
end;


procedure TfrmMain.btnIntervalValueSetClick(Sender: TObject);
begin
  IntervalValueSet;
  if frmIntervalValueSet.ModalResult <> mrOK then Exit;
  fDataComputer.SetIntervalValues(frmIntervalValueSet.IntervalValues);
  BuildDataTableGridColumn;
end;

procedure TfrmMain.fdmtCompareResultNewRecord(DataSet: TDataSet);
begin
  DataSet.FieldByName('ConformCount').AsInteger := 0;
  DataSet.FieldByName('OccurrenceCount').AsInteger := 0;
  DataSet.FieldByName('ConformCount2').AsInteger := 0;
  DataSet.FieldByName('OccurrenceCount2').AsInteger := 0;
  DataSet.FieldByName('ConformCount3').AsInteger := 0;
  DataSet.FieldByName('OccurrenceCount3').AsInteger := 0;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  fDataComputer := TDataComputer.Create;

  fdmtCompareResult.CreateDataSet;
  fdmtCompareResult.AddIndex('ConformCount', 'ConformCount;ConformCount2;ConformCount3', '', [soDescending, soDescending, soDescending]);
  fdmtCompareResult.IndexName := 'ConformCount';

  fdmtDataTable.CreateDataSet;

  BuildDataTableGridColumn;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  fDataComputer.Free;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  pnlQueryData2.Left := dbgrdDataTable.Left;
  pnlQueryData3.Left := dbgrdResultData.Left;
end;

end.
