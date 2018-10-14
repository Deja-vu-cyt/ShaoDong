object frmRearrangeFileSettings: TfrmRearrangeFileSettings
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = #30456#21516#21015#25968
  ClientHeight = 211
  ClientWidth = 598
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
    Left = 26
    Top = 17
    Width = 505
    Height = 21
    Caption = #9312'.'#22635#20837' ['#21508#34892#65288#31532#19968#21306#22495#65289#21015#25968#23383' ] '#20301#32622#65306#31532'             '#8594'              '#21015
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 26
    Top = 55
    Width = 505
    Height = 21
    Caption = #9313'.'#22635#20837' ['#21508#34892#65288#31532#20108#21306#22495#65289#21015#25968#23383' ] '#20301#32622#65306#31532'             '#8594'              '#21015
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 26
    Top = 90
    Width = 332
    Height = 21
    Caption = #9314'.'#35835#21462' [ '#27721#23383#65288#26631#27880#65289#25968#23383' ]'#65288'TXT'#65289#25991#26412#65306
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 26
    Top = 128
    Width = 269
    Height = 21
    Caption = #9315'.'#23548#20837' [ '#35201#22788#29702#30340#65288'TXT'#65289#25991#26412' ] '#65306
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object btnOK: TButton
    Left = 301
    Top = 160
    Width = 96
    Height = 40
    Caption = #30830#23450
    TabOrder = 5
    OnClick = btnOKClick
  end
  object edtFirstIntervalCol: TEdit
    Left = 370
    Top = 14
    Width = 50
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
  object edtPlaceholderFileName: TEdit
    Left = 364
    Top = 84
    Width = 217
    Height = 32
    ReadOnly = True
    TabOrder = 4
    OnClick = edtPlaceholderFileNameClick
  end
  object edtFirstIntervalCol2: TEdit
    Left = 457
    Top = 14
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
  end
  object edtSecondIntervalCol2: TEdit
    Left = 457
    Top = 49
    Width = 50
    Height = 29
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    NumbersOnly = True
    ParentFont = False
    TabOrder = 3
  end
  object edtSecondIntervalCol: TEdit
    Left = 370
    Top = 49
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
  end
  object edtFileDirectory: TEdit
    Left = 301
    Top = 122
    Width = 280
    Height = 32
    ReadOnly = True
    TabOrder = 6
    OnClick = edtFileDirectoryClick
  end
  object OpenDialog: TOpenDialog
    Filter = 'TXT|*.txt'
    Left = 552
  end
end
