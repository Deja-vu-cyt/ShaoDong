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
  DBGridEh, Vcl.CheckLst, Vcl.ComCtrls;

type
  TfrmMain = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label5: TLabel;
    Label6: TLabel;
    btnSlantCompare: TButton;
    edtExportTypeCount: TEdit;
    chkExportFile: TCheckBox;
    chkExportFile2: TCheckBox;
    chkExportFile3: TCheckBox;
    chkExportFile4: TCheckBox;
    chkExportFile5: TCheckBox;
    chkExportFile6: TCheckBox;
    chkSelectAll: TCheckBox;
    btnExportCompareRow: TButton;
    OpenDialog: TOpenDialog;
    Label11: TLabel;
    Label13: TLabel;
    btnVertCompare: TButton;
    edtVertExportTypeCount: TEdit;
    chkExportVertFile: TCheckBox;
    chkExportVertFile2: TCheckBox;
    chkExportVertFile3: TCheckBox;
    chkExportVertFile4: TCheckBox;
    chkExportVertFile5: TCheckBox;
    chkExportVertFile6: TCheckBox;
    chkVertSelectAll: TCheckBox;
    Panel3: TPanel;
    Label1: TLabel;
    lblUseTime: TLabel;
    edtFileName: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    edtMaxValue: TEdit;
    edtFirstRangeValue: TEdit;
    Label7: TLabel;
    edtVertSameValueCount2: TEdit;
    Label9: TLabel;
    edtVertSameValueCount: TEdit;
    Label2: TLabel;
    edtCompareSpacing: TEdit;
    edtVertCompareSpacing: TEdit;
    Label8: TLabel;
    edtVertCompareTypeCount: TEdit;
    TabSheet3: TTabSheet;
    Label10: TLabel;
    Label12: TLabel;
    edtVVertSameValueCount2: TEdit;
    edtVVertSameValueCount: TEdit;
    edtVVertCompareSpacing: TEdit;
    Label14: TLabel;
    Label15: TLabel;
    edtSlantSameValueCount2: TEdit;
    edtSlantSameValueCount: TEdit;
    edtSlantCompareSpacing: TEdit;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    btnVertSlantCompare: TButton;
    edtExportGroupValueCount: TEdit;
    chkExportVertSlantFile: TCheckBox;
    chkExportVertSlantFile2: TCheckBox;
    chkExportVertSlantFile3: TCheckBox;
    chkExportVertSlantFile4: TCheckBox;
    chkExportVertSlantFile5: TCheckBox;
    chkExportVertSlantFile6: TCheckBox;
    chkVertSlantSelectAll: TCheckBox;
    edtCompareGroupValueCount: TEdit;
    btnVertSlantExportSettings: TButton;
    btnVertExportSettings: TButton;
    btnSlantExportSettings: TButton;
    chkCompareCrossRange: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edtFileNameClick(Sender: TObject);
    procedure btnCompareClick(Sender: TObject);
    procedure chkSelectAllClick(Sender: TObject);
    procedure btnExportCompareRowClick(Sender: TObject);
    procedure Panel1DblClick(Sender: TObject);
    procedure chkVertSelectAllClick(Sender: TObject);
    procedure chkVertSlantSelectAllClick(Sender: TObject);
    procedure btnExportSettingsClick(Sender: TObject);
  private
    fDataComputer: TDataComputer;
    procedure OnStateChange(Working: Boolean);
    procedure Init(var MaxValue: Word; var FirstRangeValue: Word);
    procedure InitCompare(var CompareCrossRange: Boolean;
      var VertCompareSpacing: Cardinal; var VertSameValueCount: Byte;
      var VertSameValueCount2: Byte; var SlantCompareSpacing: Cardinal;
      var SlantSameValueCount: Byte; var SlantSameValueCount2: Byte;
      var CompareGroupValueCount: Byte; var ExportGroupValueCount: Byte);
  public

  end;

var
  frmMain: TfrmMain;

implementation

uses
  uTimer, ufrmExportSettings;

{$R *.dfm}

procedure TfrmMain.OnStateChange(Working: Boolean);
begin
  btnSlantCompare.Enabled := not Working and (fDataComputer.CompareMode in [cmNone, cmSlant]);
  btnVertCompare.Enabled := not Working and (fDataComputer.CompareMode in [cmNone, cmVert]);
  btnVertSlantCompare.Enabled := not Working and (fDataComputer.CompareMode in [cmNone, cmVertSlant]);
  btnSlantExportSettings.Enabled := not Working and (fDataComputer.CompareMode in [cmNone, cmSlant]);
  btnVertExportSettings.Enabled := not Working and (fDataComputer.CompareMode in [cmNone, cmVert]);
  btnVertSlantExportSettings.Enabled := not Working and (fDataComputer.CompareMode in [cmNone, cmVertSlant]);
end;

procedure TfrmMain.Panel1DblClick(Sender: TObject);
begin
  btnExportCompareRow.Visible := not btnExportCompareRow.Visible;
end;

procedure TfrmMain.Init(var MaxValue: Word; var FirstRangeValue: Word);
var
  v: Integer;
begin
  if not TryStrToInt(edtMaxValue.Text, v) then
    raise Exception.Create('请输入有效总列数');
  MaxValue := v;
  if not TryStrToInt(edtFirstRangeValue.Text, v) then
    raise Exception.Create('请输入有效范围列数');
  FirstRangeValue := v;

end;

procedure TfrmMain.InitCompare(var CompareCrossRange: Boolean;
  var VertCompareSpacing: Cardinal; var VertSameValueCount: Byte;
  var VertSameValueCount2: Byte; var SlantCompareSpacing: Cardinal;
  var SlantSameValueCount: Byte; var SlantSameValueCount2: Byte;
  var CompareGroupValueCount: Byte; var ExportGroupValueCount: Byte);
var
  v: Integer;
begin
  CompareCrossRange := chkCompareCrossRange.Checked;
  case fDataComputer.CompareMode of
    cmSlant:
    begin
      if not TryStrToInt(edtCompareSpacing.Text, v) then
        raise Exception.Create('请输入有效比较次数');
      SlantCompareSpacing := v;
      if not TryStrToInt(edtExportTypeCount.Text, v) then
        raise Exception.Create('请输入有效导出次数');
      ExportGroupValueCount := v;

      VertCompareSpacing := 0;
      VertSameValueCount := 0;
      VertSameValueCount2 := 0;
      SlantSameValueCount := 0;
      SlantSameValueCount2 := 0;
      CompareGroupValueCount := 1;
    end;
    cmVert:
    begin
      if not TryStrToInt(edtVertCompareSpacing.Text, v) then
        raise Exception.Create('请输入有效比较次数');
      VertCompareSpacing := v;
      if not TryStrToInt(edtVertSameValueCount.Text, v) then
        raise Exception.Create('请输入有效相同列数');
      VertSameValueCount := v;
      if not TryStrToInt(edtVertSameValueCount2.Text, v) then
        raise Exception.Create('请输入有效相同列数2');
      VertSameValueCount2 := v;
      if not TryStrToInt(edtVertCompareTypeCount.Text, v) then
        raise Exception.Create('请输入有效比较组合数');
      CompareGroupValueCount := v;
      if not TryStrToInt(edtVertExportTypeCount.Text, v) then
        raise Exception.Create('请输入有效导出次数');
      ExportGroupValueCount := v;

      SlantCompareSpacing := 0;
      SlantSameValueCount := 0;
      SlantSameValueCount2 := 0;
    end;
    cmVertSlant:
    begin
      if not TryStrToInt(edtVVertCompareSpacing.Text, v) then
        raise Exception.Create('请输入有效直连比较次数');
      VertCompareSpacing := v;
      if not TryStrToInt(edtVVertSameValueCount.Text, v) then
        raise Exception.Create('请输入有效相同列数');
      VertSameValueCount := v;
      if not TryStrToInt(edtVVertSameValueCount2.Text, v) then
        raise Exception.Create('请输入有效相同列数2');
      VertSameValueCount2 := v;
      if not TryStrToInt(edtSlantCompareSpacing.Text, v) then
        raise Exception.Create('请输入有效直连比较次数');
      SlantCompareSpacing := v;
      if not TryStrToInt(edtSlantSameValueCount.Text, v) then
        raise Exception.Create('请输入有效相同列数');
      SlantSameValueCount := v;
      if not TryStrToInt(edtSlantSameValueCount2.Text, v) then
        raise Exception.Create('请输入有效相同列数2');
      SlantSameValueCount2 := v;
      if not TryStrToInt(edtCompareGroupValueCount.Text, v) then
        raise Exception.Create('请输入有效比较组合数');
      CompareGroupValueCount := v;
      if not TryStrToInt(edtExportGroupValueCount.Text, v) then
        raise Exception.Create('请输入有效导出次数');
      ExportGroupValueCount := v;
    end;
  end;
end;

procedure TfrmMain.btnExportCompareRowClick(Sender: TObject);
begin
  fDataComputer.ExportCompareRow;
end;

procedure TfrmMain.btnExportSettingsClick(Sender: TObject);
begin
  if Sender = btnVertExportSettings then frmExportSettings.Flag := 1
  else if Sender = btnSlantExportSettings then frmExportSettings.Flag := 2
  else frmExportSettings.Flag := 0;
  frmExportSettings.ShowModal;
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

procedure TfrmMain.chkVertSelectAllClick(Sender: TObject);
begin
  with TCheckBox(Sender) do
  begin
    chkExportVertFile.Checked := Checked;
    chkExportVertFile2.Checked := Checked;
    chkExportVertFile3.Checked := Checked;
    chkExportVertFile4.Checked := Checked;
    chkExportVertFile5.Checked := Checked;
    chkExportVertFile6.Checked := Checked;
  end;
end;

procedure TfrmMain.chkVertSlantSelectAllClick(Sender: TObject);
begin
  with TCheckBox(Sender) do
  begin
    chkExportVertSlantFile.Checked := Checked;
    chkExportVertSlantFile2.Checked := Checked;
    chkExportVertSlantFile3.Checked := Checked;
    chkExportVertSlantFile4.Checked := Checked;
    chkExportVertSlantFile5.Checked := Checked;
    chkExportVertSlantFile6.Checked := Checked;
  end;
end;

procedure TfrmMain.edtFileNameClick(Sender: TObject);
begin
  if not OpenDialog.Execute then Exit;
  TEdit(Sender).Text := OpenDialog.FileName;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;

  fDataComputer := TDataComputer.Create;
  fDataComputer.InitEvent := Init;
  fDataComputer.InitCompareEvent := InitCompare;

  chkCompareCrossRange.Checked := fDataComputer.CompareCrossRange;
  if fDataComputer.MaxValue > 0 then
  begin
    edtMaxValue.ReadOnly := True;
    edtMaxValue.Text := fDataComputer.MaxValue.ToString;
    edtFirstRangeValue.ReadOnly := True;
    edtFirstRangeValue.Text := fDataComputer.FirstRangeValue.ToString;
  end;
  case fDataComputer.CompareMode of
    cmVert:
    begin
      PageControl1.ActivePageIndex := 1;
      edtVertCompareSpacing.ReadOnly := True;
      edtVertCompareSpacing.Text := fDataComputer.VertCompareSpacing.ToString;
      edtVertSameValueCount.ReadOnly := True;
      edtVertSameValueCount.Text := fDataComputer.VertSameValueCount.ToString;
      edtVertSameValueCount2.ReadOnly := True;
      edtVertSameValueCount2.Text := fDataComputer.VertSameValueCount2.ToString;
      edtVertCompareTypeCount.ReadOnly := True;
      edtVertCompareTypeCount.Text := fDataComputer.CompareGroupValueCount.ToString;
      edtVertExportTypeCount.ReadOnly := True;
      edtVertExportTypeCount.Text := fDataComputer.ExportGroupValueCount.ToString;
    end;
    cmSlant:
    begin
      PageControl1.ActivePageIndex := 2;
      edtCompareSpacing.ReadOnly := True;
      edtCompareSpacing.Text := fDataComputer.SlantCompareSpacing.ToString;
      edtExportTypeCount.ReadOnly := True;
      edtExportTypeCount.Text := fDataComputer.ExportGroupValueCount.ToString;
    end;
    cmVertSlant:
    begin
      edtVVertCompareSpacing.ReadOnly := True;
      edtVVertCompareSpacing.Text := fDataComputer.VertCompareSpacing.ToString;
      edtVVertSameValueCount.ReadOnly := True;
      edtVVertSameValueCount.Text := fDataComputer.VertSameValueCount.ToString;
      edtVVertSameValueCount2.ReadOnly := True;
      edtVVertSameValueCount2.Text := fDataComputer.VertSameValueCount2.ToString;
      edtSlantCompareSpacing.ReadOnly := True;
      edtSlantCompareSpacing.Text := fDataComputer.SlantCompareSpacing.ToString;
      edtSlantSameValueCount.ReadOnly := True;
      edtSlantSameValueCount.Text := fDataComputer.SlantSameValueCount.ToString;
      edtSlantSameValueCount2.ReadOnly := True;
      edtSlantSameValueCount2.Text := fDataComputer.SlantSameValueCount2.ToString;
      edtCompareGroupValueCount.ReadOnly := True;
      edtCompareGroupValueCount.Text := fDataComputer.CompareGroupValueCount.ToString;
      edtExportGroupValueCount.ReadOnly := True;
      edtExportGroupValueCount.Text := fDataComputer.ExportGroupValueCount.ToString;
    end;
  end;
  OnStateChange(False);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  fDataComputer.Free;
end;

procedure TfrmMain.btnCompareClick(Sender: TObject);
var
  ExportFiles: TDataComputer.TExportFiles;
begin
  if Sender = btnSlantCompare then
    fDataComputer.CompareMode := cmSlant
  else if Sender = btnVertCompare then
    fDataComputer.CompareMode := cmVert
  else
    fDataComputer.CompareMode := cmVertSlant;

  ExportFiles := [];
  case fDataComputer.CompareMode of
    cmVert:
    begin
      if chkExportVertFile.Checked then ExportFiles := ExportFiles + [efFile];
      if chkExportVertFile2.Checked then ExportFiles := ExportFiles + [efFile2];
      if chkExportVertFile3.Checked then ExportFiles := ExportFiles + [efFile3];
      if chkExportVertFile4.Checked then ExportFiles := ExportFiles + [efFile4];
      if chkExportVertFile5.Checked then ExportFiles := ExportFiles + [efFile5];
      if chkExportVertFile6.Checked then ExportFiles := ExportFiles + [efFile6];
    end;
    cmSlant:
    begin
      if chkExportFile.Checked then ExportFiles := ExportFiles + [efFile];
      if chkExportFile2.Checked then ExportFiles := ExportFiles + [efFile2];
      if chkExportFile3.Checked then ExportFiles := ExportFiles + [efFile3];
      if chkExportFile4.Checked then ExportFiles := ExportFiles + [efFile4];
      if chkExportFile5.Checked then ExportFiles := ExportFiles + [efFile5];
      if chkExportFile6.Checked then ExportFiles := ExportFiles + [efFile6];
    end;
    cmVertSlant:
    begin
      if chkExportVertSlantFile.Checked then ExportFiles := ExportFiles + [efFile];
      if chkExportVertSlantFile2.Checked then ExportFiles := ExportFiles + [efFile2];
      if chkExportVertSlantFile3.Checked then ExportFiles := ExportFiles + [efFile3];
      if chkExportVertSlantFile4.Checked then ExportFiles := ExportFiles + [efFile4];
      if chkExportVertSlantFile5.Checked then ExportFiles := ExportFiles + [efFile5];
      if chkExportVertSlantFile6.Checked then ExportFiles := ExportFiles + [efFile6];
    end;
  end;

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
        fDataComputer.ExportCompareData(ExportFiles,
          frmExportSettings.KeepMaxRowSpacing,
          frmExportSettings.GroupRowCount,
          frmExportSettings.GroupCount);
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
