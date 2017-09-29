unit ufrmProject2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.Math, System.Types, Vcl.FileCtrl,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DBGridEhGrouping,
  ToolCtrlsEh, DBGridEhToolCtrls, DynVarsEh, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Menus, EhLibVCL, GridsEh, DBAxisGridsEh, DBGridEh;

type
  TfrmProject2 = class(TFrame)
    dbgrdDataTable: TDBGridEh;
    Panel1: TPanel;
    btnClearColumn: TButton;
    btnExportToFile: TButton;
    btnQueryData2: TButton;
    Panel4: TPanel;
    pnlUseTime: TPanel;
    pnlClearColumn: TPanel;
    pnlInputData: TPanel;
    pnlQueryData3: TPanel;
    Panel3: TPanel;
    dbgrdClearColumn: TDBGridEh;
    dbgrdFileList: TDBGridEh;
    dbgrdResultData: TDBGridEh;
    Panel5: TPanel;
    pnlQueryData2: TPanel;
    btnQueryData: TButton;
    Label1: TLabel;
    edtClearColumnFileName: TEdit;
    Label2: TLabel;
    edtInputDataDirectory: TEdit;
    Label3: TLabel;
    edtInvalidColCount: TEdit;
    OpenDialog: TOpenDialog;
    Label4: TLabel;
    edtColCount: TEdit;
    Label5: TLabel;
    edtRangeColNo: TEdit;
    Label6: TLabel;
    edtExportFileDirectory: TEdit;
    Label7: TLabel;
    edtEachFileRowCount: TEdit;
    edtInvalidColCount2: TEdit;
    procedure FrameResize(Sender: TObject);
    procedure dbgrdResultDataAdvDrawDataCell(Sender: TCustomDBGridEh; Cell,
      AreaCell: TGridCoord; Column: TColumnEh; const ARect: TRect;
      var Params: TColCellParamsEh; var Processed: Boolean);
    procedure Panel5DblClick(Sender: TObject);
    procedure dbgrdDataTableAdvDrawDataCell(Sender: TCustomDBGridEh; Cell,
      AreaCell: TGridCoord; Column: TColumnEh; const ARect: TRect;
      var Params: TColCellParamsEh; var Processed: Boolean);
    procedure edtClearColumnFileNameDblClick(Sender: TObject);
    procedure edtInputDataDirectoryDblClick(Sender: TObject);
    procedure btnClearColumnClick(Sender: TObject);
    procedure btnExportToFileClick(Sender: TObject);
  private
    FQueryValue: TIntegerDynArray;
  public
    property QueryValue: TIntegerDynArray read FQueryValue write FQueryValue;
    procedure BuildDataTableGridColumn(AColCount: Integer; AFlag: Integer = 0);
    procedure BuildClearColumnGridColumn(AColCount: Integer);
    function Button(AButtonName: string): TButton;
  end;

var
  frmProject2: TfrmProject2;

implementation

uses
  udmProject1, udmProject2;

{$R *.dfm}

procedure TfrmProject2.btnClearColumnClick(Sender: TObject);
begin
  dmProject2.ClearColumnAndCombine;
end;

procedure TfrmProject2.btnExportToFileClick(Sender: TObject);
begin
  dmProject2.ExportToFile;
end;

procedure TfrmProject2.dbgrdDataTableAdvDrawDataCell(Sender: TCustomDBGridEh;
  Cell, AreaCell: TGridCoord; Column: TColumnEh; const ARect: TRect;
  var Params: TColCellParamsEh; var Processed: Boolean);
var
  i, ColNo: Integer;
  i64, v: Int64;
begin
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

      if Sender = dbgrdDataTable then
      begin
        for i := Low(FQueryValue) to High(FQueryValue) do
          if ColNo = FQueryValue[i] then
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

procedure TfrmProject2.dbgrdResultDataAdvDrawDataCell(Sender: TCustomDBGridEh;
  Cell, AreaCell: TGridCoord; Column: TColumnEh; const ARect: TRect;
  var Params: TColCellParamsEh; var Processed: Boolean);
var
  i, ColNo: Integer;
  i64, v: Int64;
  CompareField: string;
begin
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

procedure TfrmProject2.edtClearColumnFileNameDblClick(Sender: TObject);
begin
  OpenDialog.Title := '选择"清列行"文件';
  OpenDialog.Filter := '*|*.txt';
  //OpenDialog.Filter := '*|*.txt;*.xls;*.xlsx';
  if not OpenDialog.Execute then Exit;
  TEdit(Sender).Text := OpenDialog.FileName;
end;

procedure TfrmProject2.edtInputDataDirectoryDblClick(Sender: TObject);
var
  s: string;
begin
  if not SelectDirectory('导入数据文件夹', '', s) then Exit;
  TEdit(Sender).Text := s;
end;

procedure TfrmProject2.FrameResize(Sender: TObject);
begin
  pnlClearColumn.Left := dbgrdClearColumn.Left;
  pnlInputData.Left := dbgrdFileList.Left;
  pnlQueryData2.Left := dbgrdDataTable.Left;
  pnlQueryData3.Left := dbgrdResultData.Left;
end;

procedure TfrmProject2.Panel5DblClick(Sender: TObject);
begin
  btnQueryData.Visible := dbgrdDataTable.Tag = 1;
  dbgrdFileList.FindFieldColumn('Chosed').Visible := dbgrdDataTable.Tag = 1;
  dbgrdFileList.FindFieldColumn('RowCount').Visible := dbgrdDataTable.Tag = 1;

  if dbgrdDataTable.Tag = 1 then dbgrdDataTable.Tag := 0
  else dbgrdDataTable.Tag := 1;
end;

procedure TfrmProject2.BuildDataTableGridColumn(AColCount: Integer; AFlag: Integer = 0);
var
  i: Integer;
begin
  dbgrdDataTable.Columns.Clear;
  case AFlag of
    0:
    begin
      with dbgrdDataTable.Columns.Add do
      begin
        FieldName := 'FileNo';
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
    Title.Caption := '①.该行列数';
    Width := 100;
  end;
  with dbgrdDataTable.Columns.Add do
  begin
    FieldName := 'ConformColCount';
    Alignment := TAlignment.taCenter;
    Title.Alignment := TAlignment.taCenter;
    Title.Caption := '②.相同列数';
    Width := 100;
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
  for i := 1 to AColCount do
  begin
    with dbgrdDataTable.Columns.Add do
    begin
      FieldName := 'Field' + Ceil(i / 64).ToString;
      Alignment := TAlignment.taCenter;
      Title.Alignment := TAlignment.taCenter;
      Title.Caption := i.ToString;
      if i < 10 then Title.Caption := '0' + Title.Caption;
      Width := 45;
    end;
  end;
end;

procedure TfrmProject2.BuildClearColumnGridColumn(AColCount: Integer);
var
  i: Integer;
begin
  dbgrdClearColumn.Columns.Clear;
  for i := 1 to AColCount do
  begin
    with dbgrdClearColumn.Columns.Add do
    begin
      FieldName := 'Field' + Ceil(i / 64).ToString;
      Alignment := TAlignment.taCenter;
      Title.Alignment := TAlignment.taCenter;
      Title.Caption := i.ToString;
      if i < 10 then Title.Caption := '0' + Title.Caption;
      Width := 45;
    end;
  end;
end;

function TfrmProject2.Button(AButtonName: string): TButton;
begin
  Result := Self.FindComponent(AButtonName) as TButton;
end;

end.
