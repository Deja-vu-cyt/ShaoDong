object frmCombinationCalculator: TfrmCombinationCalculator
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = #25490#21015#32452#21512#35745#31639#22120
  ClientHeight = 207
  ClientWidth = 284
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -20
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 24
  object Label1: TLabel
    Left = 54
    Top = 32
    Width = 60
    Height = 24
    Caption = #21015#25968#65306
  end
  object Label2: TLabel
    Left = 14
    Top = 70
    Width = 100
    Height = 24
    Caption = #32452#21512#20010#25968#65306
  end
  object Label3: TLabel
    Left = 54
    Top = 105
    Width = 60
    Height = 24
    Caption = #32467#26524#65306
  end
  object lblResult: TLabel
    Left = 120
    Top = 105
    Width = 120
    Height = 24
    Caption = '                    '
    ParentShowHint = False
    ShowHint = True
  end
  object edtN: TEdit
    Left = 120
    Top = 29
    Width = 121
    Height = 32
    NumbersOnly = True
    TabOrder = 0
  end
  object edtR: TEdit
    Left = 120
    Top = 67
    Width = 121
    Height = 32
    NumbersOnly = True
    TabOrder = 1
  end
  object btnOk: TButton
    Left = 120
    Top = 135
    Width = 89
    Height = 40
    Caption = #35745#31639
    TabOrder = 2
    OnClick = btnOkClick
  end
end
