unit ufrmMain;

interface

uses
  uDataComputer,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Threading,
  System.Classes, Vcl.Graphics, System.Math, System.Types, System.IOUtils, IdHTTP,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.FileCtrl,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, DBGridEhGrouping,
  ToolCtrlsEh, DBGridEhToolCtrls, DynVarsEh, EhLibVCL, GridsEh, DBAxisGridsEh,
  DBGridEh, Vcl.CheckLst;

type
  TfrmMain = class(TForm)
    OpenDialog: TOpenDialog;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblUseTime: TLabel;
    edtFileName: TEdit;
    btnCompare: TButton;
    edtCompareTypeCount: TEdit;
    edtExportTypeCount: TEdit;
    Label2: TLabel;
    edtCompareRowCount: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    edtExportFileCount: TEdit;
    edtExportTypeCount2: TEdit;
    Panel1: TPanel;
    Label8: TLabel;
    chkExportRowSpacingFile: TCheckBox;
    chkExportRowSpacingFile2: TCheckBox;
    chkExportRowSpacingFile3: TCheckBox;
    chkExportRowSpacingFile4: TCheckBox;
    chkExportBearOneRowSpacingFile: TCheckBox;
    chkExportBearOneRowSpacingFile2: TCheckBox;
    chkSelectAll: TCheckBox;
    chkExportBearOneRowSpacingFile3: TCheckBox;
    chkExportBearOneRowSpacingFile4: TCheckBox;
    chkExportBearOneRowSpacingFile5: TCheckBox;
    chkExportBearOneRowSpacingFile6: TCheckBox;
    chkExportBearOneRowSpacingFile7: TCheckBox;
    chkExportBearOneRowSpacingFile8: TCheckBox;
    chkExportBearOneRowSpacingFile9: TCheckBox;
    chkNotExportSourceData: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edtFileNameClick(Sender: TObject);
    procedure btnCompareClick(Sender: TObject);
    procedure chkSelectAllClick(Sender: TObject);
  private
    fDataComputer: TDataComputer;
    procedure OnStateChange(Working: Boolean);
  public

  end;

var
  frmMain: TfrmMain;

implementation

uses
  uTimer;

{$R *.dfm}

procedure TfrmMain.OnStateChange(Working: Boolean);
begin
  btnCompare.Enabled := not Working;
end;

procedure TfrmMain.chkSelectAllClick(Sender: TObject);
begin
  with TCheckBox(Sender) do
  begin
    chkExportRowSpacingFile.Checked := Checked;
    chkExportRowSpacingFile2.Checked := Checked;
    chkExportRowSpacingFile3.Checked := Checked;
    chkExportRowSpacingFile4.Checked := Checked;
    chkExportBearOneRowSpacingFile.Checked := Checked;
    chkExportBearOneRowSpacingFile2.Checked := Checked;
    chkExportBearOneRowSpacingFile3.Checked := Checked;
    chkExportBearOneRowSpacingFile4.Checked := Checked;
    chkExportBearOneRowSpacingFile5.Checked := Checked;
    chkExportBearOneRowSpacingFile6.Checked := Checked;
    chkExportBearOneRowSpacingFile7.Checked := Checked;
    chkExportBearOneRowSpacingFile8.Checked := Checked;
    chkExportBearOneRowSpacingFile9.Checked := Checked;
  end;
end;

procedure TfrmMain.edtFileNameClick(Sender: TObject);
begin
  if not OpenDialog.Execute then Exit;
  TEdit(Sender).Text := OpenDialog.FileName;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  fDataComputer := uDataComputer.TDataComputer.Create;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  fDataComputer.Free;
end;

procedure TfrmMain.btnCompareClick(Sender: TObject);
var
  CompareRowCount, CompareTypeCount, ExportTypeCount, ExportTypeCount2, ExportFileCount: Integer;
  ExportFiles: TDataComputer.TExportFiles;
  ExportSourceData: Boolean;
begin
  if not TryStrToInt(edtCompareRowCount.Text, CompareRowCount) then
  begin
    edtCompareRowCount.SetFocus;
    edtCompareRowCount.SelectAll;
    raise Exception.Create('请输入有效“查询行数”');
  end;
  if not TryStrToInt(edtCompareTypeCount.Text, CompareTypeCount) then
  begin
    edtCompareTypeCount.SetFocus;
    edtCompareTypeCount.SelectAll;
    raise Exception.Create('请输入有效“查询组合数”');
  end;
  if not TryStrToInt(edtExportTypeCount.Text, ExportTypeCount) then
  begin
    edtExportTypeCount.SetFocus;
    edtExportTypeCount.SelectAll;
    raise Exception.Create('请输入有效“导出间差组合数”');
  end;
  if not TryStrToInt(edtExportFileCount.Text, ExportFileCount) then
  begin
    edtExportFileCount.SetFocus;
    edtExportFileCount.SelectAll;
    raise Exception.Create('请输入有效“导出连和文件数”');
  end;
  if not TryStrToInt(edtExportTypeCount2.Text, ExportTypeCount2) then
  begin
    edtExportTypeCount2.SetFocus;
    edtExportTypeCount2.SelectAll;
    raise Exception.Create('请输入有效“导出连和组合数”');
  end;
  ExportSourceData := not chkNotExportSourceData.Checked;

  ExportFiles := [];
  if chkExportRowSpacingFile.Checked then ExportFiles := ExportFiles + [efRowSpacingFile];
  if chkExportRowSpacingFile2.Checked then ExportFiles := ExportFiles + [efRowSpacingFile2];
  if chkExportRowSpacingFile3.Checked then ExportFiles := ExportFiles + [efRowSpacingFile3];
  if chkExportRowSpacingFile4.Checked then ExportFiles := ExportFiles + [efRowSpacingFile4];
  if chkExportBearOneRowSpacingFile.Checked then
    ExportFiles := ExportFiles + [efBearOneRowSpacingFile];
  if chkExportBearOneRowSpacingFile2.Checked and (ExportFileCount >= 1) then
    ExportFiles := ExportFiles + [efBearOneRowSpacingFile2];
  if chkExportBearOneRowSpacingFile3.Checked and (ExportFileCount >= 2) then
    ExportFiles := ExportFiles + [efBearOneRowSpacingFile3];
  if chkExportBearOneRowSpacingFile4.Checked and (ExportFileCount >= 3) then
    ExportFiles := ExportFiles + [efBearOneRowSpacingFile4];
  if chkExportBearOneRowSpacingFile5.Checked and (ExportFileCount >= 4) then
    ExportFiles := ExportFiles + [efBearOneRowSpacingFile5];
  if chkExportBearOneRowSpacingFile6.Checked and (ExportFileCount >= 5) then
    ExportFiles := ExportFiles + [efBearOneRowSpacingFile6];
  if chkExportBearOneRowSpacingFile7.Checked and (ExportFileCount >= 6) then
    ExportFiles := ExportFiles + [efBearOneRowSpacingFile7];
  if chkExportBearOneRowSpacingFile8.Checked and (ExportFileCount >= 7) then
    ExportFiles := ExportFiles + [efBearOneRowSpacingFile8];
  if chkExportBearOneRowSpacingFile9.Checked and (ExportFileCount >= 8) then
    ExportFiles := ExportFiles + [efBearOneRowSpacingFile9];

  OnStateChange(True);
  TTask.Create(procedure
  var
    fStopTime: Boolean;

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
        lblUseTime.Caption := s;
      end
    );

    try
      try
        if ExportFiles <> [] then
        begin
          fDataComputer.LoadRow(edtFileName.Text, CompareRowCount);
          fDataComputer.Compare(CompareTypeCount);
          fDataComputer.ExportCompareData(ExportTypeCount, ExportTypeCount, ExportFiles, ExportSourceData);
        end;
        StopTime;
        ShowMessage('查询完毕');
      except
        on e: Exception do raise Exception.Create(e.Message);
      end;
    finally
      StopTime;
      TThread.Synchronize(nil, procedure
      begin
        OnStateChange(False);
      end);
    end;
  end).Start;
end;

end.
