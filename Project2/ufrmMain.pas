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
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblUseTime: TLabel;
    Label6: TLabel;
    edtFileName: TEdit;
    edtCompareSpacing: TEdit;
    btnCompare: TButton;
    edtMaxValue: TEdit;
    edtFirstRangeValue: TEdit;
    edtExportTypeCount: TEdit;
    chkExportFile: TCheckBox;
    chkExportFile2: TCheckBox;
    chkExportFile3: TCheckBox;
    chkExportFile4: TCheckBox;
    chkExportFile5: TCheckBox;
    chkExportFile6: TCheckBox;
    chkSelectAll: TCheckBox;
    btnExportCompareRow: TButton;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edtFileNameClick(Sender: TObject);
    procedure btnCompareClick(Sender: TObject);
    procedure chkSelectAllClick(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure btnExportCompareRowClick(Sender: TObject);
  private
    fDataComputer: TDataComputer;
    procedure OnStateChange(Working: Boolean);
    procedure Init(var MaxValue: Word; var FirstRangeValue: Word;
      var CompareSpacing: Byte; var ExportTypeCount: Byte);
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

procedure TfrmMain.Init(var MaxValue: Word; var FirstRangeValue: Word;
  var CompareSpacing: Byte; var ExportTypeCount: Byte);
var
  v: Integer;
begin
  if not TryStrToInt(edtMaxValue.Text, v) then
    raise Exception.Create('请输入有效总列数');
  MaxValue := v;
  if not TryStrToInt(edtFirstRangeValue.Text, v) then
    raise Exception.Create('请输入有效范围列数');
  FirstRangeValue := v;
  if not TryStrToInt(edtCompareSpacing.Text, v) then
    raise Exception.Create('请输入有效比较次数');
  CompareSpacing := v;
  if not TryStrToInt(edtExportTypeCount.Text, v) then
    raise Exception.Create('请输入有效导出次数');
  ExportTypeCount := v;
end;

procedure TfrmMain.btnExportCompareRowClick(Sender: TObject);
begin
  fDataComputer.ExportCompareRow;
end;

procedure TfrmMain.chkSelectAllClick(Sender: TObject);
begin
  with TCheckBox(Sender) do
  begin
    chkExportFile.Checked := Checked;
    chkExportFile2.Checked := Checked;
    chkExportFile3.Checked := Checked;
    chkExportFile4.Checked := Checked;
    chkExportFile5.Checked := Checked;
    chkExportFile6.Checked := Checked;
  end;
end;

procedure TfrmMain.edtFileNameClick(Sender: TObject);
begin
  if not OpenDialog.Execute then Exit;
  TEdit(Sender).Text := OpenDialog.FileName;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  fDataComputer := TDataComputer.Create;
  fDataComputer.InitEvent := Init;

  if fDataComputer.MaxValue > 0 then
  begin
    edtMaxValue.ReadOnly := True;
    edtMaxValue.Text := fDataComputer.MaxValue.ToString;
    edtFirstRangeValue.ReadOnly := True;
    edtFirstRangeValue.Text := fDataComputer.FirstRangeValue.ToString;
    edtCompareSpacing.ReadOnly := True;
    edtCompareSpacing.Text := fDataComputer.CompareSpacing.ToString;
  end;
end;

procedure TfrmMain.FormDblClick(Sender: TObject);
begin
  btnExportCompareRow.Visible := not btnExportCompareRow.Visible;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  fDataComputer.Free;
end;

procedure TfrmMain.btnCompareClick(Sender: TObject);
var
  ExportTypeCount: Integer;
  ExportFiles: TDataComputer.TExportFiles;
begin
  ExportFiles := [];
  if chkExportFile.Checked then ExportFiles := ExportFiles + [efFile];
  if chkExportFile2.Checked then ExportFiles := ExportFiles + [efFile2];
  if chkExportFile3.Checked then ExportFiles := ExportFiles + [efFile3];
  if chkExportFile4.Checked then ExportFiles := ExportFiles + [efFile4];
  if chkExportFile5.Checked then ExportFiles := ExportFiles + [efFile5];
  if chkExportFile6.Checked then ExportFiles := ExportFiles + [efFile6];

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
        fDataComputer.LoadRow(edtFileName.Text);
        fDataComputer.Compare;
        fDataComputer.ExportCompareData(ExportFiles);
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
