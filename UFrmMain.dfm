object FrmMain: TFrmMain
  Left = 0
  Top = 0
  Caption = 'Crosswords'
  ClientHeight = 491
  ClientWidth = 742
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  StyleElements = [seFont, seBorder]
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object BoxTitle: TPanel
    Left = 0
    Top = 0
    Width = 742
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object LbVersion: TLabel
      Left = 112
      Top = 0
      Width = 35
      Height = 25
      Align = alLeft
      Caption = 'Version'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ShowAccelChar = False
      Layout = tlCenter
      StyleElements = [seClient, seBorder]
      ExplicitHeight = 13
    end
    object LbLink: TLabel
      Left = 15
      Top = 0
      Width = 82
      Height = 25
      Cursor = crHandPoint
      Align = alLeft
      Caption = 'digaodalpiaz.com'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clTeal
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
      ShowAccelChar = False
      Layout = tlCenter
      StyleElements = [seClient, seBorder]
      OnClick = LbLinkClick
      ExplicitHeight = 13
    end
    object LbSpace1: TLabel
      Left = 0
      Top = 0
      Width = 15
      Height = 25
      Align = alLeft
      AutoSize = False
    end
    object LbSpace2: TLabel
      Left = 97
      Top = 0
      Width = 15
      Height = 25
      Align = alLeft
      AutoSize = False
      ExplicitLeft = 112
    end
    object LbSpace3: TLabel
      Left = 147
      Top = 0
      Width = 15
      Height = 25
      Align = alLeft
      AutoSize = False
      ExplicitLeft = 152
    end
    object BoxTitleSide: TPanel
      Left = 716
      Top = 0
      Width = 26
      Height = 25
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object BtnSettings: TSpeedButton
        Left = 0
        Top = 0
        Width = 26
        Height = 25
        Align = alClient
        Flat = True
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          1800000000000003000000000000000000000000000000000000C080FFC080FF
          C080FFC080FFC080FFC080FF070706323027323027070706C080FFC080FFC080
          FFC080FFC080FFC080FFC080FFC080FFC080FF040403C080FFC080FF171612ED
          E4BAEDE4BA171612C080FFC080FF040403C080FFC080FFC080FFC080FFC080FF
          201F197A766025231D4C493BD0C8A3FFF5C8FFF5C8D0C8A34C493B25231D7A76
          60201F19C080FFC080FFC080FF0404037A7660FFF5C8FFF5C8F9EFC3A098796C
          62476C6247A09879F9EFC3FFF5C8FFF5C87A7660040403C080FFC080FFC080FF
          25231DFFF5C8D6CDA753482EB38839EAB148EAB148B3883953482ED6CDA7FFF5
          C825231DC080FFC080FFC080FFC080FF4C493BF9EFC353482EECB248F5B94BE3
          AB45E3AB45F5B94BECB24853482EF9EFC34C493BC080FFC080FF070706171612
          D0C8A3A09879B38839F5B94B856B3B736B5E736B5E856B3BF5B94BB38839A098
          79D0C8A3171612070706323027EDE4BAFFF5C86C6247EAB148E3AB45736B5EFF
          FFFFFFFFFF736B5EE3AB45EAB1486C6247FFF5C8EDE4BA323027323027EDE4BA
          FFF5C874694CE2AA45E3AB45736B5EFFFFFFFFFFFF736B5EE3AB45EAB1486C62
          47FFF5C8EDE4BA323027070706171612D0C8A3B9B08BA07A33F5B94B856B3B73
          6B5E736B5E856B3BF5B94BB38839A09879D0C8A3171612070706C080FFC080FF
          4C493BF7EDC2C5AA6EF0B54AF5B94BE3AB45E3AB45F5B94BECB24853482EF9EF
          C34C493BC080FFC080FFC080FFC080FF25231DFFF5C8E4DAB2CEB373B5893AE8
          AF47EAB148B3883953482ED6CDA7FFF5C825231DC080FFC080FFC080FF040403
          7A7660FFF5C8FFF5C8FAF0C4A69E7D6E64486C6247A09879F9EFC3FFF5C8FFF5
          C87A7660040403C080FFC080FFC080FF201F197A766025231D4C493BD0C8A3FF
          F5C8FFF5C8D0C8A34C493B25231D7A7660201F19C080FFC080FFC080FFC080FF
          C080FF040403C080FFC080FF171612EDE4BAEDE4BA171612C080FFC080FF0404
          03C080FFC080FFC080FFC080FFC080FFC080FFC080FFC080FFC080FF07070632
          3027323027070706C080FFC080FFC080FFC080FFC080FFC080FF}
        OnClick = BtnSettingsClick
        ExplicitWidth = 25
      end
    end
    object BoxConInfo: TPanel
      Left = 162
      Top = 0
      Width = 554
      Height = 25
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      Visible = False
      object LbLbMode: TLabel
        Left = 0
        Top = 0
        Width = 33
        Height = 25
        Align = alLeft
        Caption = 'Mode: '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clYellow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ShowAccelChar = False
        Layout = tlCenter
        StyleElements = [seClient, seBorder]
        ExplicitHeight = 13
      end
      object LbMode: TLabel
        Left = 33
        Top = 0
        Width = 12
        Height = 25
        Align = alLeft
        Caption = '---'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ShowAccelChar = False
        Layout = tlCenter
        StyleElements = [seClient, seBorder]
        ExplicitHeight = 13
      end
      object LbSpace4: TLabel
        Left = 45
        Top = 0
        Width = 15
        Height = 25
        Align = alLeft
        AutoSize = False
        ExplicitLeft = 216
      end
      object LbLbPlayer: TLabel
        Left = 60
        Top = 0
        Width = 37
        Height = 25
        Align = alLeft
        Caption = 'Player: '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clYellow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ShowAccelChar = False
        Layout = tlCenter
        StyleElements = [seClient, seBorder]
        ExplicitHeight = 13
      end
      object LbPlayer: TLabel
        Left = 97
        Top = 0
        Width = 12
        Height = 25
        Align = alLeft
        Caption = '---'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ShowAccelChar = False
        Layout = tlCenter
        StyleElements = [seClient, seBorder]
        ExplicitHeight = 13
      end
      object LbSpace5: TLabel
        Left = 109
        Top = 0
        Width = 15
        Height = 25
        Align = alLeft
        AutoSize = False
        ExplicitLeft = 272
      end
      object LbLbRules: TLabel
        Left = 124
        Top = 0
        Width = 33
        Height = 25
        Align = alLeft
        Caption = 'Rules: '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clYellow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ShowAccelChar = False
        Layout = tlCenter
        StyleElements = [seClient, seBorder]
        ExplicitHeight = 13
      end
      object LbRules: TLabel
        Left = 157
        Top = 0
        Width = 12
        Height = 25
        Align = alLeft
        Caption = '---'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ShowAccelChar = False
        Layout = tlCenter
        StyleElements = [seClient, seBorder]
        ExplicitHeight = 13
      end
      object BtnRestart: TSpeedButton
        Left = 474
        Top = 0
        Width = 80
        Height = 25
        Align = alRight
        Caption = 'Restart'
        Flat = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        StyleElements = [seClient, seBorder]
        OnClick = BtnRestartClick
        ExplicitLeft = 465
      end
    end
  end
  object Dam: TDam
    Language = dgEnglish
    HandleExceptions = True
    MessageFont.Charset = DEFAULT_CHARSET
    MessageFont.Color = clWhite
    MessageFont.Height = -13
    MessageFont.Name = 'Segoe UI'
    MessageFont.Style = []
    DamDefault = True
    DamUnitName = 'UDams'
    Left = 40
    Top = 48
    object _QuestionCloseApp: TDamMsg
      Icon = diQuest
      Buttons = dbYesNo
      SwapFocus = True
      Dam = Dam
    end
    object _QuestionKillPlayer: TDamMsg
      Icon = diQuest
      Buttons = dbYesNo
      SwapFocus = True
      Dam = Dam
    end
    object _QuestionStopGame: TDamMsg
      Icon = diQuest
      Buttons = dbYesNo
      SwapFocus = True
      Dam = Dam
    end
    object _QuestionRestartGame: TDamMsg
      Icon = diQuest
      Buttons = dbYesNo
      SwapFocus = True
      Dam = Dam
    end
  end
end
