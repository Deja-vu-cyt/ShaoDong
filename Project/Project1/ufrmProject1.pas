unit ufrmProject1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.Math, System.Types,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DBGridEhGrouping,
  ToolCtrlsEh, DBGridEhToolCtrls, DynVarsEh, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Menus, EhLibVCL, GridsEh, DBAxisGridsEh, DBGridEh;

type
  TfrmProject1 = class(TFrame)
    dbgrdDataTable: TDBGridEh;
    Panel1: TPanel;
    btnSelectClearColumn: TButton;
    btnClearColumn: TButton;
    btnExportToFile: TButton;
    btnDeleteInvalidRow: TButton;
    btnSetColCount: TButton;
    btnChangeProject: TButton;
    btnSortRow: TButton;
    btnQueryData2: TButton;
    Panel4: TPanel;
    pnlUseTime: TPanel;
    pnlClearColumn: TPanel;
    pnlInputData: TPanel;
    btnInputData: TButton;
    btnRearrangeClearColumn: TButton;
    btnSortClearColumn: TButton;
    btnCompareClearColumn: TButton;
    btnBuildClearColumn: TButton;
    btnQueryData3: TButton;
    pnlQueryData3: TPanel;
    Panel3: TPanel;
    dbgrdClearColumn: TDBGridEh;
    dbgrdFileList: TDBGridEh;
    dbgrdResultData: TDBGridEh;
    Panel5: TPanel;
    pnlQueryData2: TPanel;
    btnQueryData: TButton;
    OpenDialog: TOpenDialog;
    btnInputResultData: TButton;
    btnSetRowcount: TButton;
    btnDeleteSameRow: TButton;
    Splitter1: TSplitter;
    procedure btnChangeProjectClick(Sender: TObject);
    procedure btnSetColCountClick(Sender: TObject);
    procedure btnInputDataClick(Sender: TObject);
    procedure btnSelectClearColumnClick(Sender: TObject);
    procedure btnDeleteInvalidRowClick(Sender: TObject);
    procedure btnClearColumnClick(Sender: TObject);
    procedure btnSortRowClick(Sender: TObject);
    procedure btnExportToFileClick(Sender: TObject);
    procedure btnQueryData2Click(Sender: TObject);
    procedure btnQueryDataClick(Sender: TObject);
    procedure btnSortClearColumnClick(Sender: TObject);
    procedure btnRearrangeClearColumnClick(Sender: TObject);
    procedure btnCompareClearColumnClick(Sender: TObject);
    procedure btnBuildClearColumn2Click(Sender: TObject);
    procedure btnBuildClearColumn3Click(Sender: TObject);
    procedure btnBuildClearColumnClick(Sender: TObject);
    procedure btnInputResultDataClick(Sender: TObject);
    procedure btnQueryData3Click(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure dbgrdResultDataAdvDrawDataCell(Sender: TCustomDBGridEh; Cell,
      AreaCell: TGridCoord; Column: TColumnEh; const ARect: TRect;
      var Params: TColCellParamsEh; var Processed: Boolean);
    procedure Panel5DblClick(Sender: TObject);
    procedure dbgrdDataTableAdvDrawDataCell(Sender: TCustomDBGridEh; Cell,
      AreaCell: TGridCoord; Column: TColumnEh; const ARect: TRect;
      var Params: TColCellParamsEh; var Processed: Boolean);
    procedure btnSetRowcountClick(Sender: TObject);
    procedure btnDeleteSameRowClick(Sender: TObject);
  private
    FQueryValue: TIntegerDynArray;
  public
    procedure BuildDataTableGridColumn(AFlag: Integer = 0);
    procedure BuildClearColumnGridColumn;
    property QueryValue: TIntegerDynArray read FQueryValue write FQueryValue;
  end;

var
  frmProject1: TfrmProject1;

implementation

uses
  udmProject1;

{$R *.dfm}

procedure TfrmProject1.btnBuildClearColumn2Click(Sender: TObject);
begin
  dmProject1.BuildClearColumn(1);
end;

procedure TfrmProject1.btnBuildClearColumn3Click(Sender: TObject);
begin
  dmProject1.BuildClearColumn(2);
end;

procedure TfrmProject1.btnBuildClearColumnClick(Sender: TObject);
begin
  dmProject1.BuildClearColumn;
end;

procedure TfrmProject1.btnChangeProjectClick(Sender: TObject);
begin
  dmProject1.ChangeProject;
end;

procedure TfrmProject1.btnClearColumnClick(Sender: TObject);
begin
  with Sender as TButton do
  begin
    if Tag = 0 then dmProject1.ClearColumn
    else Tag := 0;
  end;
end;

procedure TfrmProject1.btnCompareClearColumnClick(Sender: TObject);
begin
  dmProject1.CompareClearColumn;
end;

procedure TfrmProject1.btnDeleteInvalidRowClick(Sender: TObject);
begin
  dmProject1.SetDeleteInvalidRow;
end;

procedure TfrmProject1.btnDeleteSameRowClick(Sender: TObject);
begin
  dmProject1.DeleteSameRow;
end;

procedure TfrmProject1.btnExportToFileClick(Sender: TObject);
begin
  with Sender as TButton do
  begin
    if Tag = 0 then dmProject1.ExportToFile
    else Tag := 0;
  end;
end;

procedure TfrmProject1.btnInputDataClick(Sender: TObject);
begin
  with Sender as TButton do
  begin
    if Tag = 0 then dmProject1.InputData
    else Tag := 0;
  end;
end;

procedure TfrmProject1.btnInputResultDataClick(Sender: TObject);
begin
  dmProject1.InputResultData;
end;

procedure TfrmProject1.btnQueryData2Click(Sender: TObject);
begin
  dmProject1.QueryData2;
end;

procedure TfrmProject1.btnQueryData3Click(Sender: TObject);
begin
  dmProject1.QueryData3;
end;

procedure TfrmProject1.btnQueryDataClick(Sender: TObject);
begin
  dmProject1.QueryData;
end;

procedure TfrmProject1.btnRearrangeClearColumnClick(Sender: TObject);
begin
  dmProject1.RearrangeClearColumn;
end;

procedure TfrmProject1.btnSelectClearColumnClick(Sender: TObject);
begin
  dmProject1.InputClearColumn;
end;

procedure TfrmProject1.btnSetColCountClick(Sender: TObject);
begin
  dmProject1.SetColNo;
end;

procedure TfrmProject1.btnSetRowcountClick(Sender: TObject);
begin
  dmProject1.SetRowcount;
end;

procedure TfrmProject1.btnSortClearColumnClick(Sender: TObject);
begin
  dmProject1.SortClearColumn;
end;

procedure TfrmProject1.btnSortRowClick(Sender: TObject);
begin
  dmProject1.SortRow;
end;

procedure TfrmProject1.dbgrdDataTableAdvDrawDataCell(Sender: TCustomDBGridEh;
  Cell, AreaCell: TGridCoord; Column: TColumnEh; const ARect: TRect;
  var Params: TColCellParamsEh; var Processed: Boolean);
var
  i, ColNo, ColNo2: Integer;
  i64, v: Int64;
begin
  i64 := 1;
  Params.Background := clWhite;
  if TryStrToInt(Column.Title.Caption, ColNo) and Assigned(Column.Field) then
  begin
    ColNo2 := ColNo;
    if Column.Tag = 1 then ColNo2 := ColNo2 + dmProject1.RangeColNo;
    v := Column.Field.AsLargeInt;
    Params.Text := '';
    if v = i64 shl (64 - ColNo2) or v then
    begin
      Params.Text := ColNo.ToString;
      if ColNo < 10 then Params.Text := '0' + Params.Text;

      if Sender = dbgrdDataTable then
      begin
        for i := Low(FQueryValue) to High(FQueryValue) do
          if ColNo2 = FQueryValue[i] then
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

procedure TfrmProject1.dbgrdResultDataAdvDrawDataCell(Sender: TCustomDBGridEh;
  Cell, AreaCell: TGridCoord; Column: TColumnEh; const ARect: TRect;
  var Params: TColCellParamsEh; var Processed: Boolean);
var
  i, ColNo: Integer;
  i64, v: Int64;
  CompareField: string;
begin
  if Params.Text = '0' then Params.Text := '';
  Exit;
  i64 := 1;
  Params.Background := clWhite;
  if TryStrToInt(Column.Title.Caption, ColNo) then
  begin
    v := Column.Field.AsLargeInt;
    Params.Text := '';
    if v = i64 shl (64 - ColNo) or v then
    begin
      Params.Text := ColNo.ToString;
      if ColNo < 10 then Params.Text := '0' + Params.Text;

      v := Column.Field.DataSet.FieldByName('CompareField' + Ceil(ColNo / 64).ToString).AsLargeInt;
      if v = i64 shl (64 - ColNo) or v then
      begin
        Params.Background := clBlack;
        with Sender as TDBGridEh do Canvas.Font.Color := clWhite;
      end;
    end;
  end;
end;

procedure TfrmProject1.FrameResize(Sender: TObject);
begin
  pnlClearColumn.Left := dbgrdClearColumn.Left;
  pnlInputData.Left := dbgrdFileList.Left;
  pnlQueryData2.Left := dbgrdDataTable.Left;
  pnlQueryData3.Left := dbgrdResultData.Left;
end;

procedure TfrmProject1.Panel5DblClick(Sender: TObject);
begin
  btnQueryData.Visible := dbgrdDataTable.Tag = 1;
  dbgrdFileList.FindFieldColumn('Chosed').Visible := dbgrdDataTable.Tag = 1;
  dbgrdFileList.FindFieldColumn('RowCount').Visible := dbgrdDataTable.Tag = 1;

  if dbgrdDataTable.Tag = 1 then dbgrdDataTable.Tag := 0
  else dbgrdDataTable.Tag := 1;
end;

procedure TfrmProject1.BuildDataTableGridColumn(AFlag: Integer = 0);
var
  i, ColNo: Integer;
begin
  dbgrdDataTable.OnAdvDrawDataCell := nil;
  try
    dbgrdDataTable.Columns.Clear;
    case AFlag of
      0:
      begin
        with dbgrdDataTable.Columns.Add do
        begin
          FieldName := 'FileNo2';
          Alignment := TAlignment.taCenter;
          Title.Alignment := TAlignment.taCenter;
          Title.Caption := '文本号';
          Width := 55;
        end;
      end;
    end;
    with dbgrdDataTable.Columns.Add do
    begin
      FieldName := 'ValueCount';
      Alignment := TAlignment.taCenter;
      Title.Alignment := TAlignment.taCenter;
      if dmProject1.ColCount > dmProject1.RangeColNo then Title.Caption := '①.该行列数|第一区域'
      else Title.Caption := '①.该行列数';
      Width := 100;
    end;
    if dmProject1.ColCount > dmProject1.RangeColNo then
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
      if dmProject1.ColCount > dmProject1.RangeColNo then Title.Caption := '②.相同列数|第一区域'
      else Title.Caption := '②.相同列数';
      Width := 100;
    end;
    if dmProject1.ColCount > dmProject1.RangeColNo then
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
      Title.Caption := '④.所在文本行号';
      Width := 140;
    end;
    with dbgrdDataTable.Columns.Add do
    begin
      FieldName := 'RowNo';
      Alignment := TAlignment.taCenter;
      Title.Alignment := TAlignment.taCenter;
      Title.Caption := '⑤.所在文本第几行';
      Width := 140;
    end;
    for i := 1 to dmProject1.ColCount do
    begin
      ColNo := i;
      with dbgrdDataTable.Columns.Add do
      begin
        if ColNo > dmProject1.RangeColNo then
        begin
          ColNo := ColNo - dmProject1.RangeColNo;
          Tag := 1;
        end;
        FieldName := 'Field' + Ceil(i / 64).ToString;
        Alignment := TAlignment.taCenter;
        Title.Alignment := TAlignment.taCenter;
        Title.Caption := ColNo.ToString;
        if ColNo < 10 then Title.Caption := '0' + Title.Caption;
        Width := 45;
      end;
    end;
  finally
    dbgrdDataTable.OnAdvDrawDataCell := dbgrdDataTableAdvDrawDataCell;
  end;
end;

procedure TfrmProject1.BuildClearColumnGridColumn;
var
  i, ColNo: Integer;
begin
  dbgrdClearColumn.OnAdvDrawDataCell := nil;
  try
    dbgrdClearColumn.Columns.Clear;
    for i := 1 to dmProject1.ColCount do
    begin
      ColNo := i;
      with dbgrdClearColumn.Columns.Add do
      begin
        if ColNo > dmProject1.RangeColNo then
        begin
          ColNo := ColNo - dmProject1.RangeColNo;
          Tag := 1;
        end;
        FieldName := 'Field' + Ceil(i / 64).ToString;
        Alignment := TAlignment.taCenter;
        Title.Alignment := TAlignment.taCenter;
        Title.Caption := ColNo.ToString;
        if ColNo < 10 then Title.Caption := '0' + Title.Caption;
        Width := 45;
      end;
    end;
  finally
    dbgrdClearColumn.OnAdvDrawDataCell := dbgrdDataTableAdvDrawDataCell;
  end;
end;

end.
