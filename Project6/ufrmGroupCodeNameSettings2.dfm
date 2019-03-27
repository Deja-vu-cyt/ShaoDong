object frmGroupCodeNameSettings2: TfrmGroupCodeNameSettings2
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  ClientHeight = 512
  ClientWidth = 1124
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -20
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 24
  object lblLine: TLabel
    Left = 23
    Top = 23
    Width = 809
    Height = 24
    Caption = 
      '6-2'#65288'2'#65289' .'#35774#32622' '#65306#31532'          '#33267'          '#27425#65288' '#36941#21382' '#65289#20043#38388' '#65307#21482#20445#30041#65339' '#31532' 1 - N '#34892#20026#39318#34892' '#65341 +
      #20013' '#65306
  end
  object lblLine2: TLabel
    Left = 23
    Top = 58
    Width = 1095
    Height = 24
    Caption = 
      #12304' '#27880' '#65306#31532#12304' N '#12305#33267#12304' N '#12305#27425#65288' '#20043#20869#30340#36941#21382' '#65289#65292#25165#19981#25490#21015' : '#39318#34892#65339' '#9312'.'#65288' '#20195#21495#32452#21512#25968' '#65289#12289#9313'.'#65288' '#39318#34892#20986#29616#25968' '#65289#26368#22810 +
      ' '#65341#30340#34892' '#12305
  end
  object edtGroupNumber3: TEdit
    Left = 207
    Top = 23
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
    OnKeyPress = edtGroupNumber3KeyPress
  end
  object edtGroupNumber4: TEdit
    Left = 287
    Top = 23
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
    OnKeyPress = edtGroupNumber3KeyPress
  end
  object btnOk: TButton
    Left = 463
    Top = 88
    Width = 106
    Height = 40
    Caption = #30830#23450
    TabOrder = 2
    OnClick = btnOkClick
  end
end
