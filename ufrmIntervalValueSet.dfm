object frmIntervalValueSet: TfrmIntervalValueSet
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = #35774#32622#21015#25968
  ClientHeight = 142
  ClientWidth = 225
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
    Left = 16
    Top = 11
    Width = 100
    Height = 24
    Alignment = taRightJustify
    Caption = #31532#19968#21306#22495#65306
  end
  object Label2: TLabel
    Left = 16
    Top = 46
    Width = 100
    Height = 24
    Alignment = taRightJustify
    Caption = #31532#20108#21306#22495#65306
  end
  object edtIntervalValue: TEdit
    Left = 111
    Top = 8
    Width = 100
    Height = 29
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    NumbersOnly = True
    ParentFont = False
    TabOrder = 0
  end
  object edtIntervalValue2: TEdit
    Left = 111
    Top = 43
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
  end
  object btnOK: TButton
    Left = 84
    Top = 82
    Width = 60
    Height = 40
    Caption = #30830#23450
    TabOrder = 2
    OnClick = btnOKClick
  end
end
