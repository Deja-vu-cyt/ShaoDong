object frmMain: TfrmMain
  Left = 0
  Top = 0
  ClientHeight = 425
  ClientWidth = 1137
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -20
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    1137
    425)
  PixelsPerInch = 96
  TextHeight = 24
  object Label1: TLabel
    Left = 19
    Top = 54
    Width = 249
    Height = 24
    Caption = '1.'#35835#21462#8220#26597#35810#65288'TXT'#65289#25991#26412#8221#65306
  end
  object lblUseTime: TLabel
    Left = 19
    Top = 16
    Width = 140
    Height = 24
    Caption = #22788#29702#25152#38656#26102#38388#65306
  end
  object Label3: TLabel
    Left = 19
    Top = 124
    Width = 473
    Height = 24
    Caption = '3. '#35774#32622' [ '#26597#35810#65288' '#31532' 1 '#21306#22495' '#65289' ] '#65288' '#21015#25968' '#65289#33539#22260' '#65306'1 '#8594
  end
  object Label4: TLabel
    Left = 19
    Top = 159
    Width = 473
    Height = 24
    Caption = '4. '#35774#32622' [ '#26597#35810#65288' '#31532' 2 '#21306#22495' '#65289' ] '#65288' '#21015#25968' '#65289#33539#22260' '#65306'1 '#8594
  end
  object Label2: TLabel
    Left = 19
    Top = 89
    Width = 249
    Height = 24
    Caption = '2.'#35835#21462#8220#27604#36739#65288'TXT'#65289#25991#26412#8221#65306
  end
  object Label5: TLabel
    Left = 19
    Top = 194
    Width = 652
    Height = 24
    Caption = #9312'. '#35774#32622' [ '#26597#35810#65288' '#31532' 1 '#21306#22495' '#65289' ] '#30456#21516#65288' '#21015#25968#23383' '#65289#25968#37327' '#65306#65288' '#22823#12289#31561#20110' '#8805' '#65289
  end
  object Label6: TLabel
    Left = 19
    Top = 229
    Width = 652
    Height = 24
    Caption = #9313'. '#35774#32622' [ '#26597#35810#65288' '#31532' 2 '#21306#22495' '#65289' ] '#30456#21516#65288' '#21015#25968#23383' '#65289#25968#37327' '#65306#65288' '#22823#12289#31561#20110' '#8805' '#65289
  end
  object Label7: TLabel
    Left = 19
    Top = 264
    Width = 132
    Height = 24
    Caption = #9314'. '#38468#21152#35774#32622' '#65306
  end
  object Label8: TLabel
    Left = 19
    Top = 294
    Width = 853
    Height = 24
    Caption = #9312'. '#35774#32622' [ '#26597#35810#65288' '#31532' 1 '#21306#22495' '#65289' ] '#30456#21516#65288' '#21015#25968#23383' '#65289#25968#37327'  ...'#65288' '#19981' '#65289#31526#21512#65288' '#35774#32622#26465#20214' '#65289#30340#24773#20917#19979' '#65306
  end
  object Label9: TLabel
    Left = 19
    Top = 324
    Width = 1030
    Height = 24
    Caption = 
      #9313'. '#35774#32622' [ '#26597#35810#65288' '#31532' 2 '#21306#22495' '#65289' ] '#30456#21516#65288' '#21015#25968#23383' '#65289#25968#37327'  '#65306#65288' '#22823#12289#31561#20110' '#8805' '#65289'                 ' +
      ' '#20010' '#65292#25165#20801#35768#65288' '#23548#20986' '#65289#27492#34892' '#12290
  end
  object edtFileName: TEdit
    Left = 274
    Top = 54
    Width = 842
    Height = 29
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 0
    OnClick = edtFileNameClick
    ExplicitWidth = 689
  end
  object edtIntervalValue: TEdit
    Left = 498
    Top = 124
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
  object edtIntervalValue2: TEdit
    Left = 498
    Top = 159
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
  object edtFileName2: TEdit
    Left = 274
    Top = 89
    Width = 842
    Height = 29
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 3
    OnClick = edtFileNameClick
    ExplicitWidth = 689
  end
  object btnCompare: TButton
    Left = 677
    Top = 359
    Width = 89
    Height = 41
    Caption = #30830#23450
    TabOrder = 4
    OnClick = btnCompareClick
  end
  object edtMinSameValueCount: TEdit
    Left = 677
    Top = 194
    Width = 100
    Height = 29
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    NumbersOnly = True
    ParentFont = False
    TabOrder = 5
  end
  object edtMinSameValueCount2: TEdit
    Left = 677
    Top = 229
    Width = 100
    Height = 29
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    NumbersOnly = True
    ParentFont = False
    TabOrder = 6
  end
  object edtMinSameValueCount3: TEdit
    Left = 677
    Top = 324
    Width = 100
    Height = 29
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    NumbersOnly = True
    ParentFont = False
    TabOrder = 7
  end
  object OpenDialog: TOpenDialog
    Filter = #25991#26412#25991#20214'|*.txt'
    Left = 256
    Top = 10
  end
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 220
    Top = 10
  end
end
