object frmQueryConfig3: TfrmQueryConfig3
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  ClientHeight = 141
  ClientWidth = 194
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 22
    Top = 16
    Width = 102
    Height = 21
    Alignment = taRightJustify
    Caption = #30456#21516#21015#20010#25968#65306
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 22
    Top = 51
    Width = 102
    Height = 21
    Alignment = taRightJustify
    Caption = #34892#23545#27604#27425#25968#65306
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object btnOk: TButton
    Left = 57
    Top = 97
    Width = 75
    Height = 25
    Caption = #30830#23450
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = btnOkClick
  end
  object edtIdenticalColCount: TEdit
    Left = 119
    Top = 13
    Width = 50
    Height = 29
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    NumbersOnly = True
    ParentFont = False
    TabOrder = 1
    Text = '1'
  end
  object edtCompareRowCount: TEdit
    Left = 119
    Top = 48
    Width = 50
    Height = 29
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    NumbersOnly = True
    ParentFont = False
    TabOrder = 2
    Text = '1'
  end
end
