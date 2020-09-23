object FrmStart: TFrmStart
  Left = 0
  Top = 0
  BorderStyle = bsNone
  ClientHeight = 370
  ClientWidth = 625
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LbTitle: TLabel
    Left = 0
    Top = 0
    Width = 625
    Height = 41
    Align = alTop
    Alignment = taCenter
    AutoSize = False
    Caption = 'Welcome to Scrabble'
    Color = clYellow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -24
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
    Layout = tlCenter
    StyleElements = [seBorder]
    ExplicitWidth = 626
  end
  object LbPlayerName: TLabel
    Left = 16
    Top = 56
    Width = 55
    Height = 13
    Caption = 'Your name:'
  end
  object Pages: TPageControl
    Left = 16
    Top = 112
    Width = 593
    Height = 201
    ActivePage = TabServer
    TabOrder = 1
    TabStop = False
    object TabClient: TTabSheet
      Caption = 'Client'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LbServerAddress: TLabel
        Left = 16
        Top = 16
        Width = 77
        Height = 13
        Caption = 'Server address:'
      end
      object EdServerAddress: TEdit
        Left = 16
        Top = 32
        Width = 297
        Height = 21
        TabOrder = 0
      end
    end
    object TabServer: TTabSheet
      Caption = 'Server'
      ImageIndex = 1
      object LbTableSize: TLabel
        Left = 16
        Top = 19
        Width = 51
        Height = 13
        Caption = 'Table size:'
      end
      object LbTableSizeX: TLabel
        Left = 136
        Top = 19
        Width = 6
        Height = 13
        Caption = 'x'
      end
      object LbDictionary: TLabel
        Left = 16
        Top = 56
        Width = 52
        Height = 13
        Caption = 'Dictionary:'
      end
      object Label1: TLabel
        Left = 16
        Top = 123
        Width = 99
        Height = 13
        Caption = 'Initial letters pocket:'
      end
      object Label2: TLabel
        Left = 16
        Top = 147
        Width = 69
        Height = 13
        Caption = 'Rebuy letters:'
      end
      object EdSizeW: TEdit
        Left = 88
        Top = 16
        Width = 41
        Height = 21
        Alignment = taRightJustify
        NumbersOnly = True
        TabOrder = 0
      end
      object EdSizeH: TEdit
        Left = 152
        Top = 16
        Width = 41
        Height = 21
        Alignment = taRightJustify
        NumbersOnly = True
        TabOrder = 1
      end
      object EdDictionary: TComboBox
        Left = 16
        Top = 72
        Width = 265
        Height = 21
        Style = csDropDownList
        TabOrder = 2
      end
      object EdInitialLetters: TEdit
        Left = 128
        Top = 120
        Width = 41
        Height = 21
        Alignment = taRightJustify
        NumbersOnly = True
        TabOrder = 3
      end
      object EdRebuyLetters: TEdit
        Left = 128
        Top = 144
        Width = 41
        Height = 21
        Alignment = taRightJustify
        NumbersOnly = True
        TabOrder = 4
      end
    end
  end
  object EdPlayerName: TEdit
    Left = 16
    Top = 72
    Width = 249
    Height = 21
    TabOrder = 0
  end
  object BtnJoin: TBitBtn
    Left = 192
    Top = 328
    Width = 121
    Height = 33
    Caption = 'Join'
    TabOrder = 2
    OnClick = BtnJoinClick
  end
  object BtnExit: TBitBtn
    Left = 320
    Top = 328
    Width = 121
    Height = 33
    Caption = 'Exit'
    TabOrder = 3
    OnClick = BtnExitClick
  end
end
