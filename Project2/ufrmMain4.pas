unit ufrmMain4;

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
    btnSlantCompare: TButton;
    edtExportTypeCount: TEdit;
    OpenDialog: TOpenDialog;
    Label11: TLabel;
    btnVertCompare: TButton;
    edtVertExportTypeCount: TEdit;
    Panel3: TPanel;
    Label1: TLabel;
    lblUseTime: TLabel;
    edtFileName: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    edtIntervalValue: TEdit;
    edtIntervalValue2: TEdit;
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
    edtVVertSameValueCount2: TEdit;
    edtVVertSameValueCount: TEdit;
    edtVVertCompareSpacing: TEdit;
    Label14: TLabel;
    Label15: TLabel;
    edtSlantSameValueCount2: TEdit;
    edtSlantSameValueCount: TEdit;
    edtSlantCompareSpacing: TEdit;
    Label16: TLabel;
    Label18: TLabel;
    btnVertSlantCompare: TButton;
    edtExportGroupValueCount: TEdit;
    edtCompareGroupValueCount: TEdit;
    btnVertSlantExportSettings: TButton;
    btnVertExportSettings: TButton;
    btnSlantExportSettings: TButton;
    chkCompareCrossRange: TCheckBox;
    btnExportVertSlantFileSettings: TButton;
    btnExportVertFileSettings: TButton;
    btnExportSlantFileSettings: TButton;
    Label12: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edtFileNameClick(Sender: TObject);
    procedure btnCompareClick(Sender: TObject);
    procedure btnExportSettingsClick(Sender: TObject);
    procedure btnExportVertSlantFileSettingsClick(Sender: TObject);
    procedure btnExportVertFileSettingsClick(Sender: TObject);
    procedure btnExportSlantFileSettingsClick(Sender: TObject);
  private
    procedure OnStateChange(Working: Boolean);
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
  uTimer, ufrmExportSettings2, ufrmExportVertSlantFileSettings2,
  ufrmExportVertFileSettings2, ufrmExportSlantFileSettings2;

{$R *.dfm}

procedure TfrmMain.OnStateChange(Working: Boolean);
begin
  btnSlantCompare.Enabled := not Working and (fDataComputer.CompareMode in [cmNone, cmSlant]);
  btnVertCompare.Enabled := not Working and (fDataComputer.CompareMode in [cmNone, cmVert]);
  btnVertSlantCompare.Enabled := not Working and (fDataComputer.CompareMode in [cmNone, cmVertSlant]);
  btnSlantExportSettings.Enabled := not Working and (fDataComputer.CompareMode in [cmNone, cmSlant]);
  btnVertExportSettings.Enabled := not Working and (fDataComputer.CompareMode in [cmNone, cmVert]);
  btnVertSlantExportSettings.Enabled := not Working and (fDataComputer.CompareMode in [cmNone, cmVertSlant]);
  btnExportVertSlantFileSettings.Enabled := not Working and (fDataComputer.CompareMode in [cmNone, cmVertSlant]);
  btnExportVertFileSettings.Enabled := not Working and (fDataComputer.CompareMode in [cmNone, cmVert]);
  btnExportSlantFileSettings.Enabled := not Working and (fDataComputer.CompareMode in [cmNone, cmSlant]);
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

procedure TfrmMain.btnExportSettingsClick(Sender: TObject);
begin
  if Sender = btnVertExportSettings then frmExportSettings.Flag := 1
  else if Sender = btnSlantExportSettings then frmExportSettings.Flag := 2
  else frmExportSettings.Flag := 0;
  frmExportSettings.ShowModal;
end;

procedure TfrmMain.btnExportSlantFileSettingsClick(Sender: TObject);
begin
  frmExportSlantFileSettings.ShowModal;
end;

procedure TfrmMain.btnExportVertFileSettingsClick(Sender: TObject);
begin
  frmExportVertFileSettings.ShowModal;
end;

procedure TfrmMain.btnExportVertSlantFileSettingsClick(Sender: TObject);
begin
  frmExportVertSlantFileSettings.ShowModal;
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
  fDataComputer.DataMode := 1;
  fDataComputer.InitCompareEvent := InitCompare;

  chkCompareCrossRange.Checked := fDataComputer.CompareCrossRange;
  if Length(fDataComputer.IntervalValues) > 0 then
  begin
    edtIntervalValue.ReadOnly := True;
    edtIntervalValue.Text := fDataComputer.IntervalValues[0].ToString;
    edtIntervalValue2.ReadOnly := True;
    if Length(fDataComputer.IntervalValues) > 1 then
      edtIntervalValue2.Text := fDataComputer.IntervalValues[1].ToString;
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
  v, v2: Integer;
  IntervalValues: TWordDynArray;
begin
  if Sender = btnSlantCompare then
    fDataComputer.CompareMode := cmSlant
  else if Sender = btnVertCompare then
    fDataComputer.CompareMode := cmVert
  else
    fDataComputer.CompareMode := cmVertSlant;

  if Length(fDataComputer.IntervalValues) = 0 then
  begin
    if not (TryStrToInt(edtIntervalValue.Text, v) and (v >= 1) and (v <= 256)) then
    begin
      edtIntervalValue.SetFocus;
      edtIntervalValue.SelectAll;
      raise Exception.Create('请输入有效值');
    end;
    if not (TryStrToInt(edtIntervalValue2.Text, v2) and (v2 >= 0) and (v2 < 256)) then
    begin
      edtIntervalValue2.SetFocus;
      edtIntervalValue2.SelectAll;
      raise Exception.Create('请输入有效值');
    end;
    if v + v2 > 256 then raise Exception.Create('总列数不能超过256');

    SetLength(IntervalValues, 1);
    IntervalValues[0] := v;
    if v2 > 0 then
    begin
      SetLength(IntervalValues, 2);
      IntervalValues[1] := v2;
    end;
  end;

  ExportFiles := [];
  case fDataComputer.CompareMode of
    cmVert:
    begin
      with frmExportVertFileSettings do
      begin
        if chkFile.Checked then ExportFiles := ExportFiles + [efFile];
        if chkFile2.Checked then ExportFiles := ExportFiles + [efFile2];
        if chkFile3.Checked then ExportFiles := ExportFiles + [efFile3];
        if chkFile4.Checked then ExportFiles := ExportFiles + [efFile4];
        if chkFile5.Checked then ExportFiles := ExportFiles + [efFile5];
        if chkFile6.Checked then ExportFiles := ExportFiles + [efFile6];
        if chkFile7.Checked then ExportFiles := ExportFiles + [efFile7];
        if chkFile8.Checked then ExportFiles := ExportFiles + [efFile8];
        if chkFile9.Checked then ExportFiles := ExportFiles + [efFile9];
        if chkFile10.Checked then ExportFiles := ExportFiles + [efFile10];
        if chkFile11.Checked then ExportFiles := ExportFiles + [efFile11];
        if chkFile12.Checked then ExportFiles := ExportFiles + [efFile12];
        if chkFile14.Checked then ExportFiles := ExportFiles + [efFile14];
        if chkFile15.Checked then ExportFiles := ExportFiles + [efFile15];
      end;
    end;
    cmSlant:
    begin
      with frmExportSlantFileSettings do
      begin
        if chkFile.Checked then ExportFiles := ExportFiles + [efFile];
        if chkFile2.Checked then ExportFiles := ExportFiles + [efFile2];
        if chkFile3.Checked then ExportFiles := ExportFiles + [efFile3];
        if chkFile4.Checked then ExportFiles := ExportFiles + [efFile4];
        if chkFile5.Checked then ExportFiles := ExportFiles + [efFile5];
        if chkFile6.Checked then ExportFiles := ExportFiles + [efFile6];
        if chkFile7.Checked then ExportFiles := ExportFiles + [efFile7];
        if chkFile8.Checked then ExportFiles := ExportFiles + [efFile8];
        if chkFile9.Checked then ExportFiles := ExportFiles + [efFile9];
        if chkFile10.Checked then ExportFiles := ExportFiles + [efFile10];
        if chkFile11.Checked then ExportFiles := ExportFiles + [efFile11];
        if chkFile12.Checked then ExportFiles := ExportFiles + [efFile12];
        if chkFile13.Checked then ExportFiles := ExportFiles + [efFile13];
        if chkFile14.Checked then ExportFiles := ExportFiles + [efFile14];
        if chkFile15.Checked then ExportFiles := ExportFiles + [efFile15];
      end;
    end;
    cmVertSlant:
    begin
      with frmExportVertSlantFileSettings do
      begin
        if chkFile.Checked then ExportFiles := ExportFiles + [efFile];
        if chkFile2.Checked then ExportFiles := ExportFiles + [efFile2];
        if chkFile3.Checked then ExportFiles := ExportFiles + [efFile3];
        if chkFile4.Checked then ExportFiles := ExportFiles + [efFile4];
        if chkFile5.Checked then ExportFiles := ExportFiles + [efFile5];
        if chkFile6.Checked then ExportFiles := ExportFiles + [efFile6];
        if chkFile7.Checked then ExportFiles := ExportFiles + [efFile7];
        if chkFile8.Checked then ExportFiles := ExportFiles + [efFile8];
        if chkFile9.Checked then ExportFiles := ExportFiles + [efFile9];
        if chkFile10.Checked then ExportFiles := ExportFiles + [efFile10];
        if chkFile11.Checked then ExportFiles := ExportFiles + [efFile11];
        if chkFile12.Checked then ExportFiles := ExportFiles + [efFile12];
        if chkFile13.Checked then ExportFiles := ExportFiles + [efFile13];
        if chkFile14.Checked then ExportFiles := ExportFiles + [efFile14];
        if chkFile15.Checked then ExportFiles := ExportFiles + [efFile15];
      end;
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
        fDataComputer.LoadRow(edtFileName.Text, IntervalValues);
        fDataComputer.Compare;
        case frmExportSettings.RecalcMode of
          1:
          begin
            if (fDataComputer.RecalcMode <> 1)
            or (frmExportSettings.KeepMaxRowSpacing <> fDataComputer.KeepMaxRowSpacing)
            or (frmExportSettings.GroupRowCount <> fDataComputer.GroupRowCount)
            or (frmExportSettings.GroupCount <> fDataComputer.GroupCount)
            or (frmExportSettings.ReEnabledGroupCount <> fDataComputer.ReEnabledGroupCount)
            or (frmExportSettings.HideSameGroup <> fDataComputer.HideSameGroup)
          then
            fDataComputer.RecalcData(
              frmExportSettings.KeepMaxRowSpacing,
              frmExportSettings.GroupRowCount,
              frmExportSettings.GroupCount,
              frmExportSettings.ReEnabledGroupCount,
              frmExportSettings.HideSameGroup
            );
          end;
          else fDataComputer.RestoreRecalcMode;
        end;
        if ExportFiles <> [] then fDataComputer.ExportCompareData(ExportFiles, True);
        StopTime;
        ShowMessage('查询完毕');
      except
        on e: Exception do
        begin
          TThread.Synchronize(nil, procedure
          begin
            ShowMessage(e.Message);
          end);
        end;
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
