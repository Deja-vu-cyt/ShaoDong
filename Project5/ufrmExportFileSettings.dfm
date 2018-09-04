object frmExportFileSettings: TfrmExportFileSettings
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = #30456#21516#21015#25968
  ClientHeight = 229
  ClientWidth = 684
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
    Left = 42
    Top = 97
    Width = 294
    Height = 21
    Caption = #9314'.'#24179#22343'                      '#34892#65295#65288'TXT'#65289#25991#26412
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 42
    Top = 135
    Width = 239
    Height = 21
    Caption = #9315'.'#20648#23384#65288#23548#20986#25968#25454#65289#30340#25991#20214#22841#65306
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object btnOK: TButton
    Left = 290
    Top = 177
    Width = 96
    Height = 40
    Caption = #30830#23450
    TabOrder = 0
    OnClick = btnOKClick
  end
  object rbOneFileDirectory: TRadioButton
    Left = 24
    Top = 24
    Width = 385
    Height = 25
    Caption = #9312'.'#21333#25991#20214#22841#20869#65306'[ 1- N '#20010#65288'TXT'#65289#25991#26412' ] '
    TabOrder = 1
  end
  object rbSubFileDirectory: TRadioButton
    Left = 24
    Top = 55
    Width = 545
    Height = 25
    Caption = #9313'.'#24635#25991#20214#22841#20869#65306'[ 1- N '#20010#25991#20214#22841#37324#65306'1- N '#20010#65288'TXT'#65289#25991#26412' ]'
    Checked = True
    TabOrder = 2
    TabStop = True
  end
  object edtEachPageRowCount: TEdit
    Left = 102
    Top = 94
    Width = 91
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
  object edtExportFileDirectory: TEdit
    Left = 276
    Top = 129
    Width = 378
    Height = 32
    ReadOnly = True
    TabOrder = 4
    OnClick = edtExportFileDirectoryClick
  end
end
