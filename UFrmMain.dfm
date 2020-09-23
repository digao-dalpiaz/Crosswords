object FrmMain: TFrmMain
  Left = 0
  Top = 0
  Caption = 'Scrabble'
  ClientHeight = 359
  ClientWidth = 588
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  StyleElements = [seFont, seBorder]
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 588
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object LbVersion: TLabel
      Left = 112
      Top = 6
      Width = 35
      Height = 13
      Caption = 'Version'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      StyleElements = [seClient, seBorder]
    end
    object LbMode: TLabel
      Left = 250
      Top = 6
      Width = 12
      Height = 13
      Caption = '---'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      StyleElements = [seClient, seBorder]
    end
    object Label1: TLabel
      Left = 216
      Top = 6
      Width = 30
      Height = 13
      Caption = 'Mode:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clYellow
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      StyleElements = [seClient, seBorder]
    end
    object Label2: TLabel
      Left = 320
      Top = 6
      Width = 34
      Height = 13
      Caption = 'Player:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clYellow
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      StyleElements = [seClient, seBorder]
    end
    object LbPlayer: TLabel
      Left = 358
      Top = 6
      Width = 12
      Height = 13
      Caption = '---'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      StyleElements = [seClient, seBorder]
    end
    object LinkLabel1: TLinkLabel
      Left = 8
      Top = 6
      Width = 86
      Height = 17
      Caption = '<a href="http://digaodalpiaz.com/">digaodalpiaz.com</a>'
      TabOrder = 0
      OnLinkClick = LinkLabel1LinkClick
    end
  end
  object Dam: TDam
    Language = dgEnglish
    HandleExceptions = True
    DamDefault = True
    DamUnitName = 'UDams'
    Left = 40
    Top = 48
  end
end
