object frmQueryConfig: TfrmQueryConfig
  Left = 0
  Top = 0
  ClientHeight = 461
  ClientWidth = 784
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlSelect: TPanel
    Left = 0
    Top = 0
    Width = 584
    Height = 421
    Align = alClient
    TabOrder = 0
  end
  object pnlControl: TPanel
    Left = 584
    Top = 0
    Width = 200
    Height = 421
    Align = alRight
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 421
    Width = 784
    Height = 40
    Align = alBottom
    TabOrder = 2
    object lblMaxPageNo: TLabel
      Left = 126
      Top = 9
      Width = 6
      Height = 13
      Caption = '0'
    end
    object btnOk: TButton
      Left = 466
      Top = 6
      Width = 75
      Height = 25
      Caption = #30830#23450
      TabOrder = 0
      OnClick = btnOkClick
    end
    object edtPageNo: TEdit
      Left = 70
      Top = 6
      Width = 50
      Height = 21
      NumbersOnly = True
      TabOrder = 1
      Text = '1'
      OnKeyPress = edtKeyPress
    end
    object edtEachPageRowCount: TEdit
      Left = 14
      Top = 6
      Width = 50
      Height = 21
      NumbersOnly = True
      TabOrder = 2
      Text = '1000'
      OnKeyPress = edtKeyPress
    end
  end
end
