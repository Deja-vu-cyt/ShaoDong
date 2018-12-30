unit ufrmMain2;

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
  ufrmInput, uTimer, ufrmIntervalValueSet2, ufrmIntervalValueSet3, ufrmSameColumnsSet,
  ufrmExportFileSettings, ufrmRearrangeFileSettings;

{$R *.dfm}

procedure TfrmMain.OnStateChange(Enalbed: Boolean);
begin
  btnQuerySameColumns.Enabled := Enalbed;
end;

procedure TfrmMain.BuildDataTableGridColumn;
const
  SubIntervalValue: Byte = 10;
var
  i: Integer;
  IntervalValue, v, v2, SubIntervalNo: Word;
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
      if Length(fDataComputer.IntervalValues) > 1 then Title.Caption := '①.该行列数|第一区域'
      else Title.Caption := '①.该行列数';
      Width := 100;
    end;
    if Length(fDataComputer.IntervalValues) > 1 then
    begin
      with dbgrdDataTable.Columns.Add do
      begin
        FieldName := 'ValueCount2';
        Alignment := TAlignment.taCenter;
        Title.Alignment := TAlignment.taCenter;
        Title.Caption := '①.该行列数|第二区域';
        Width := 100;
      end;
    end;
    with dbgrdDataTable.Columns.Add do
    begin
      FieldName := 'ConformColCount';
      Alignment := TAlignment.taCenter;
      Title.Alignment := TAlignment.taCenter;
      if Length(fDataComputer.IntervalValues) > 1 then Title.Caption := '②.相同列数|第一区域'
      else Title.Caption := '②.相同列数';
      Width := 100;
    end;
    if Length(fDataComputer.IntervalValues) > 1 then
    begin
      with dbgrdDataTable.Columns.Add do
      begin
        FieldName := 'ConformColCount2';
        Alignment := TAlignment.taCenter;
        Title.Alignment := TAlignment.taCenter;
        Title.Caption := '②.相同列数|第二区域';
        Width := 100;
      end;
    end;
    with dbgrdDataTable.Columns.Add do
    begin
      FieldName := 'FolderNo';
      Alignment := TAlignment.taCenter;
      Title.Alignment := TAlignment.taCenter;
      Title.Caption := '③.所在文件夹行号';
      Width := 140;
    end;
    with dbgrdDataTable.Columns.Add do
    begin
      FieldName := 'FileNo';
      Alignment := TAlignment.taCenter;
      Title.Alignment := TAlignment.taCenter;
      Title.Caption := '④.所在文本行（或）尾号';
      Width := 200;
    end;
    with dbgrdDataTable.Columns.Add do
    begin
      FieldName := 'RowNo';
      Alignment := TAlignment.taCenter;
      Title.Alignment := TAlignment.taCenter;
      Title.Caption := '⑤.所在文本第几行';
      Width := 140;
    end;
    IntervalValue := 0;
    for i := Low(fDataComputer.IntervalValues) to High(fDataComputer.IntervalValues) do
    begin
      if i > 0 then IntervalValue := IntervalValue + fDataComputer.IntervalValues[i - 1];
      for v := 1 to fDataComputer.IntervalValues[i] do
      begin
        with dbgrdDataTable.Columns.Add do
        begin
          Tag := IntervalValue + v;
          FieldName := 'Field' + ((Tag - 1) div 64 + 1).ToString;
          Alignment := TAlignment.taCenter;
          Title.Alignment := TAlignment.taCenter;
          v2 := v;
          SubIntervalNo := 1;
          if i = 0 then
          begin
            SubIntervalNo := (v2 - 1) div SubIntervalValue + 1;
            v2 := v mod SubIntervalValue;
            if v2 = 0 then v2 := SubIntervalValue;
            v2 := v2 - 1;
          end;
          Title.Caption := Format('%d-%d|%d', [i + 1, SubIntervalNo, v2]);
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

  fDataComputer.ExportToFile2(
    frmExportFileSettings.ExportFileDirectory,
    frmExportFileSettings.EachFileRowCount,
    frmExportFileSettings.rbSubFileDirectory.Checked
  );
  ShowMessage('导出结果数据完成');
end;

procedure TfrmMain.dbgrdDataTableAdvDrawDataCell(Sender: TCustomDBGridEh;
  Cell, AreaCell: TGridCoord; Column: TColumnEh; const ARect: TRect;
  var Params: TColCellParamsEh; var Processed: Boolean);
var
  i, ColNo: Integer;
  i64, v: Int64;
  IntervalValue: Word;
  s: string;
begin
  i64 := 1;
  Params.Background := clWhite;
  s := Column.Title.Caption;
  s := s.Substring(s.IndexOf('|') + 1);
  if TryStrToInt(s, ColNo) and Assigned(Column.Field) then
  begin
    v := Column.Field.AsLargeInt;
    Params.Text := '';
    if v = i64 shl (64 - Column.Tag) or v then
    begin
      Params.Text := s;

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
  if frmIntervalValueSet2.ModalResult <> mrOK then Exit;

  fDataComputer.BuildClearColumn(OpenDialog.FileName, frmIntervalValueSet2.IntervalValues);

  ShowMessage('生成清列行完成');
end;

procedure TfrmMain.btnCompareClearColumnClick(Sender: TObject);
var
  FileDirectory: string;
begin
  if not SelectDirectory('导入数据文件夹', '', FileDirectory) then Exit;
  fDataComputer.CompareClearColumn(FileDirectory);
  ShowMessage('比较清列行完成');
end;

procedure TfrmMain.btnInputResultDataClick(Sender: TObject);
var
  FileDirectory: string;
begin
  if not SelectDirectory('导入数据文件夹', '', FileDirectory) then Exit;
  fDataComputer.InputResultData(FileDirectory);
  ShowMessage('导入 ( 查询“数据”) 完成');
end;

procedure TfrmMain.btnQuerySameColumnsClick(Sender: TObject);
var
  SameValuesChanged: Boolean;
  SameValues: TWordDynArray;
  EachPageRowCount: Word;
begin
  SameColumnsSet(fDataComputer.IntervalValues, 1);
  if frmSameColumnsSet.ModalResult <> mrOK then Exit;
  //SameValuesChanged := frmSameColumnsSet.ValueChanged;
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
        //if SameValuesChanged then
        begin
          fDataComputer.CalcConformColumnCount(SameValues);
          ConformRowCount := fDataComputer.GetConformRowCount;
        end;
        fDataComputer.QueryData2(fdmtDataTable, EachPageRowCount);

        StopTime;
        ShowMessage('查询完毕');
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
  if not Assigned(frmIntervalValueSet3) then
    frmIntervalValueSet3 := TfrmIntervalValueSet3.Create(Self);
  frmIntervalValueSet3.ShowModal;
  if frmIntervalValueSet3.ModalResult <> mrOK then Exit;

  fDataComputer.QueryData3(
    fdmtCompareResult,
    OpenDialog.FileName,
    frmIntervalValueSet3.IntervalValues,
    frmIntervalValueSet3.IdenticalColCount,
    frmIntervalValueSet3.CompareRowCount
  );
end;

procedure TfrmMain.btnRearrangeClearColumnClick(Sender: TObject);
begin
  if not RearrangeFileSet then Exit;

  fDataComputer.RearrangeClearColumn(
    frmRearrangeFileSettings.FileDirectory,
    frmRearrangeFileSettings.FirstIntervalCol,
    frmRearrangeFileSettings.FirstIntervalCol2,
    frmRearrangeFileSettings.SecondIntervalCol,
    frmRearrangeFileSettings.SecondIntervalCol2,
    frmRearrangeFileSettings.Placeholder,
    chkReverseOrder.Checked
  );
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
  if frmIntervalValueSet2.ModalResult <> mrOK then Exit;
  fDataComputer.SetIntervalValues(frmIntervalValueSet2.IntervalValues);
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
  fDataComputer.DataMode := 1;

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
