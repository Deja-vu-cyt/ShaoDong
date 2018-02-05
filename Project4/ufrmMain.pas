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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edtFileNameClick(Sender: TObject);
    procedure btnCompareClick(Sender: TObject);
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

procedure TfrmMain.edtFileNameClick(Sender: TObject);
begin
  if not OpenDialog.Execute then Exit;
  TEdit(Sender).Text := OpenDialog.FileName;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  fDataComputer := TDataComputer.Create;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  fDataComputer.Free;
end;

procedure TfrmMain.btnCompareClick(Sender: TObject);
var
  CompareRowCount, CompareTypeCount, ExportTypeCount: Integer;
begin
  if not TryStrToInt(edtCompareRowCount.Text, CompareRowCount) then
    raise Exception.Create('请输入有效“查询行数”');
  if not TryStrToInt(edtCompareTypeCount.Text, CompareTypeCount) then
    raise Exception.Create('请输入有效“查询组合数”');
  if not TryStrToInt(edtExportTypeCount.Text, ExportTypeCount) then
    raise Exception.Create('请输入有效“导出组合数”');

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
        fDataComputer.LoadRow(edtFileName.Text, CompareRowCount);
        fDataComputer.Compare(CompareTypeCount);
        fDataComputer.ExportCompareData(ExportTypeCount);
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
