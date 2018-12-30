object frmConnectionSettings: TfrmConnectionSettings
  Left = 0
  Top = 0
  Caption = #36830#25509#35774#32622
  ClientHeight = 95
  ClientWidth = 190
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 18
    Top = 11
    Width = 36
    Height = 13
    Caption = #22320#22336#65306
  end
  object Label2: TLabel
    Left = 18
    Top = 38
    Width = 36
    Height = 13
    Caption = #31471#21475#65306
  end
  object edtAddress: TEdit
    Left = 55
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object edtPort: TEdit
    Left = 55
    Top = 35
    Width = 121
    Height = 21
    NumbersOnly = True
    TabOrder = 1
    Text = '8888'
  end
  object btnOk: TButton
    Left = 55
    Top = 62
    Width = 75
    Height = 25
    Caption = #30830#23450
    TabOrder = 2
    OnClick = btnOkClick
  end
end
