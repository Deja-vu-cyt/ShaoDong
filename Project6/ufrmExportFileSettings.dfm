object frmExportFileSettings: TfrmExportFileSettings
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  ClientHeight = 265
  ClientWidth = 1313
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
    Top = 16
    Width = 950
    Height = 24
    Caption = 
      '7.'#35774#32622#8220#23548#20986#12304#65288#31532'N-N'#34892#20026#39318#34892#65289#30452#12289#26012#36830' ['#65288#31532'N-1'#34892#65289#65288#31532'1-N'#30452#12289#26012#36830#34892#65289']'#12305#21015'          '#33267'        ' +
      '  '#20010#32452#21512#8221
  end
  object btnOk: TButton
    Left = 463
    Top = 212
    Width = 106
    Height = 40
    Caption = #30830#23450
    TabOrder = 0
    OnClick = btnOkClick
  end
  object edtExportCodeNameValueCount: TEdit
    Left = 770
    Top = 16
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
  object edtExportCodeNameValueCount2: TEdit
    Left = 850
    Top = 16
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
  object chkKeepLastBatchCodeNameOnEachComputer: TCheckBox
    Left = 23
    Top = 51
    Width = 877
    Height = 23
    Caption = #9312'.'#65339' '#21508#65288' '#20998#26381#21153#22120' '#65289#65288' '#26368#26411#27425#36941#21382' '#65289#22788#29702#22909#30340#25968#25454' '#65341#26242#20445#23384#22312#65339' '#21508#65288' '#20998#26381#21153#22120' '#65289#20013' '#65341
    TabOrder = 3
  end
  object chkFile4: TCheckBox
    Left = 23
    Top = 80
    Width = 666
    Height = 23
    Caption = #9313'. '#21482#23384#20648#65288#24182#65289#23548#20986#65288' '#26368#26411#27425#36941#21382' '#65289#65306#31526#21512' 1- 7'#35774#32622' ...'#26465#20214#30340#25968#25454
    TabOrder = 4
  end
  object chkFile2: TCheckBox
    Left = 23
    Top = 109
    Width = 1138
    Height = 23
    Caption = 
      #9314'. '#21551#29992' :'#10' '#65288'1'#65289'.'#12304#25490#21015#12305#12304#8220'N-N'#8221#20010#20197#19978'['#30456#21516#39318#34892#12289#19981#21516#32452#21512']'#30340#32452#21512#12305'-'#65288' 1-N '#65289#30340#35774#32622' '#65307#65288#24182#65289#23548#20986#65288'TXT'#65289 +
      #25991#26412' ...'
    TabOrder = 5
  end
  object chkFile3: TCheckBox
    Left = 23
    Top = 138
    Width = 1282
    Height = 23
    Caption = 
      #9315'. '#21551#29992' :'#10' '#65288'2-1'#65289'.'#12304#25490#21015#12305#12304#8220'N-N'#8221#20010'['#30456#21516#32452#21512#12289#19981#21516#39318#34892']'#30340#32452#21512'['#19981#21516#39318#34892#25968#65306#26368#22810#8594#23569']'#12305'-'#65288' 1-N '#65289#30340#35774#32622 +
      ' '#65307#65288#24182#65289#23548#20986#65288'TXT'#65289#25991#26412' ...'
    TabOrder = 6
  end
  object chkFile: TCheckBox
    Left = 23
    Top = 167
    Width = 1282
    Height = 23
    Caption = 
      #9316'. '#23548#20986' :'#10'  0 .'#65339' '#21508#65288#31532' N '#34892#20026#39318#34892#65289#30340#65288#31526#21512#26465#20214#30340#20195#21495#65289#65341#31532' N '#27425#65288' '#20381#27425#36941#21382' '#65289#23548#20986#65339' '#30456#21516#65288#31526#21512#26465#20214#30340#20195#21495 +
      #65289#65341#30340#65288'TXT'#65289#25991#26412' ...'
    TabOrder = 7
  end
end
