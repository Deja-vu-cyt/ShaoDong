object frmExportSettings: TfrmExportSettings
  Left = 0
  Top = 0
  Caption = 'frmExportSettings'
  ClientHeight = 469
  ClientWidth = 412
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -20
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 24
  object Label2: TLabel
    Left = 9
    Top = 38
    Width = 236
    Height = 24
    Caption = '1. '#20445#25345' '#65306#65288' '#23567#12289#31561#20110#8804' '#65289
  end
  object Label1: TLabel
    Left = 9
    Top = 108
    Width = 236
    Height = 24
    Caption = '3. '#20445#30041' '#65306#65288' '#22823#12289#31561#20110#8805' '#65289
  end
  object Label4: TLabel
    Left = 8
    Top = 138
    Width = 57
    Height = 24
    Caption = #65288'1'#65289'.'
  end
  object Label5: TLabel
    Left = 8
    Top = 168
    Width = 57
    Height = 24
    Caption = #65288'2'#65289'.'
  end
  object Label6: TLabel
    Left = 9
    Top = 73
    Width = 198
    Height = 24
    Caption = '2. '#20445#30041' '#65306'             1'#8594
  end
  object Label7: TLabel
    Left = 8
    Top = 198
    Width = 57
    Height = 24
    Caption = #65288'3'#65289'.'
  end
  object Label10: TLabel
    Left = 8
    Top = 229
    Width = 56
    Height = 24
    Caption = '     '#9312'. '
  end
  object Label15: TLabel
    Left = 8
    Top = 267
    Width = 56
    Height = 24
    Caption = '     '#9313'. '
  end
  object Label16: TLabel
    Left = 8
    Top = 297
    Width = 239
    Height = 24
    Caption = '     '#9314'. '#20445#30041#65288#26368#21069#65289#65306'1.'#8594' '
  end
  object Label13: TLabel
    Left = 7
    Top = 327
    Width = 56
    Height = 24
    Caption = '     '#9315'. '
  end
  object Label8: TLabel
    Left = 8
    Top = 8
    Width = 83
    Height = 24
    Caption = #35774#32622' 1 '#65306
  end
  object Label9: TLabel
    Left = 8
    Top = 357
    Width = 83
    Height = 24
    Caption = #35774#32622' 2 '#65306
  end
  object btnOK: TButton
    Left = 251
    Top = 407
    Width = 100
    Height = 40
    Caption = #30830#23450
    TabOrder = 0
    OnClick = btnOKClick
  end
  object edtKeepMaxRowSpacing: TEdit
    Left = 251
    Top = 38
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
  object edtMinGroupCount: TEdit
    Left = 251
    Top = 73
    Width = 100
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
  object edtGroupRowCount: TEdit
    Left = 251
    Top = 108
    Width = 100
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
  object edtReEnabledGroupCount: TEdit
    Left = 251
    Top = 297
    Width = 100
    Height = 29
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    NumbersOnly = True
    ParentFont = False
    TabOrder = 4
  end
  object chkHideSameGroup: TCheckBox
    Left = 97
    Top = 360
    Width = 25
    Height = 20
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
end
