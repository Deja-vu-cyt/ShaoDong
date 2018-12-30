object frmRearrangeFileSettings: TfrmRearrangeFileSettings
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = #30456#21516#21015#25968
  ClientHeight = 223
  ClientWidth = 689
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
  object Label3: TLabel
    Left = 26
    Top = 146
    Width = 273
    Height = 21
    Caption = #9313'. '#35835#21462#65306'[ '#27721#23383#65288#26631#27880#65289#25968#23383' ] '#34920#65306
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label5: TLabel
    Left = 26
    Top = 20
    Width = 84
    Height = 21
    Caption = '1.'#26684#24335' 1 '#65306
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 26
    Top = 81
    Width = 84
    Height = 21
    Caption = '2.'#26684#24335' 2 '#65306
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 26
    Top = 47
    Width = 366
    Height = 21
    Caption = #9312'. '#35835#21462#65306'[ '#35201#36716#21270#65288#21015#25968#23383#65289'] '#30340#65288'TXT'#65289#25991#26412#65306
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label6: TLabel
    Left = 26
    Top = 108
    Width = 366
    Height = 21
    Caption = #9312'. '#35835#21462#65306'[ '#35201#36716#21270#65288#21015#25968#23383#65289'] '#30340#65288'TXT'#65289#25991#26412#65306
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object btnOK: TButton
    Left = 301
    Top = 178
    Width = 96
    Height = 40
    Caption = #30830#23450
    TabOrder = 1
    OnClick = btnOKClick
  end
  object edtPlaceholderFileName: TEdit
    Left = 305
    Top = 140
    Width = 363
    Height = 32
    TabOrder = 0
    OnClick = edtPlaceholderFileNameClick
  end
  object edtFileDirectory: TEdit
    Left = 388
    Top = 41
    Width = 280
    Height = 32
    TabOrder = 2
    OnClick = edtFileDirectoryClick
  end
  object edtFileDirectory2: TEdit
    Left = 388
    Top = 102
    Width = 280
    Height = 32
    TabOrder = 3
    OnClick = edtFileDirectoryClick
  end
  object OpenDialog: TOpenDialog
    Filter = 'TXT|*.txt'
    Left = 568
    Top = 3
  end
end
