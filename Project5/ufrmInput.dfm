object frmInput: TfrmInput
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #25552#31034
  ClientHeight = 153
  ClientWidth = 194
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 19
  object btnOk: TButton
    AlignWithMargins = True
    Left = 40
    Top = 118
    Width = 114
    Height = 25
    Margins.Left = 40
    Margins.Top = 10
    Margins.Right = 40
    Margins.Bottom = 10
    Align = alBottom
    Caption = #30830#23450
    TabOrder = 0
    OnClick = btnOkClick
  end
  object edtValue: TEdit
    AlignWithMargins = True
    Left = 20
    Top = 78
    Width = 154
    Height = 27
    Margins.Left = 20
    Margins.Right = 20
    Align = alBottom
    TabOrder = 1
    OnKeyDown = edtValueKeyDown
  end
  object mmoPrompt: TMemo
    AlignWithMargins = True
    Left = 5
    Top = 5
    Width = 184
    Height = 65
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    BevelOuter = bvNone
    BorderStyle = bsNone
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 2
  end
end
