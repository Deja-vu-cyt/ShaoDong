object frmMain: TfrmMain
  Left = 0
  Top = 0
  ClientHeight = 542
  ClientWidth = 784
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 800
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poDesktopCenter
  Visible = True
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object StatusBar: TStatusBar
    Left = 0
    Top = 523
    Width = 784
    Height = 19
    Panels = <
      item
        Width = 150
      end>
  end
  object MediaPlayer: TMediaPlayer
    Left = 40
    Top = -29
    Width = 253
    Height = 30
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 1
    OnNotify = MediaPlayerNotify
  end
  object MainMenu: TMainMenu
    Left = 296
    Top = 8
    object N1: TMenuItem
      Caption = #24110#21161
      object miRegister: TMenuItem
        Caption = #27880#20876
        OnClick = miRegisterClick
      end
      object miLog: TMenuItem
        Caption = #26085#24535
        OnClick = miLogClick
      end
    end
  end
end
