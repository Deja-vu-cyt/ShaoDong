object frmSameColumnsSet: TfrmSameColumnsSet
  Left = 0
  Top = 0
  ClientHeight = 461
  ClientWidth = 1214
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 23
  object Panel1: TPanel
    Left = 0
    Top = 400
    Width = 1214
    Height = 61
    Align = alBottom
    TabOrder = 0
    object Label1: TLabel
      Left = 352
      Top = 15
      Width = 200
      Height = 24
      Caption = #9312'.'#19968#27425#24615#26174#31034#34892#24635#25968#65306
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object btnOk: TButton
      Left = 790
      Top = 6
      Width = 75
      Height = 43
      Caption = #30830#23450
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = btnOkClick
    end
    object edtEachPageRowCount: TEdit
      Left = 558
      Top = 13
      Width = 100
      Height = 29
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      NumbersOnly = True
      ParentFont = False
      TabOrder = 1
      OnKeyPress = edtKeyPress
    end
  end
  object pnlContainer: TPanel
    Left = 168
    Top = 56
    Width = 129
    Height = 73
    BevelOuter = bvNone
    TabOrder = 1
  end
end
