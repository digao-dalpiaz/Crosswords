object FrmSettings: TFrmSettings
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Settings'
  ClientHeight = 162
  ClientWidth = 457
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LbGridZoom: TLabel
    Left = 16
    Top = 51
    Width = 153
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Grid zoom:'
  end
  object LbLogFontSize: TLabel
    Left = 16
    Top = 83
    Width = 153
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Log font size:'
  end
  object Bevel1: TBevel
    Left = 16
    Top = 120
    Width = 425
    Height = 9
    Shape = bsTopLine
  end
  object CkSounds: TCheckBox
    Left = 16
    Top = 16
    Width = 97
    Height = 17
    Caption = 'Enable Sounds'
    TabOrder = 0
  end
  object BtnOK: TButton
    Left = 144
    Top = 128
    Width = 81
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 5
    OnClick = BtnOKClick
  end
  object BtnCancel: TButton
    Left = 232
    Top = 128
    Width = 81
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object EdGridZoom: TEdit
    Left = 176
    Top = 48
    Width = 49
    Height = 21
    Alignment = taRightJustify
    NumbersOnly = True
    TabOrder = 1
    Text = '100'
  end
  object BtnZoom: TUpDown
    Left = 225
    Top = 48
    Width = 16
    Height = 21
    Associate = EdGridZoom
    Min = 50
    Max = 200
    Position = 100
    TabOrder = 2
  end
  object EdLogFontSize: TEdit
    Left = 176
    Top = 80
    Width = 49
    Height = 21
    Alignment = taRightJustify
    NumbersOnly = True
    TabOrder = 3
    Text = '10'
  end
  object BtnLogFontSize: TUpDown
    Left = 225
    Top = 80
    Width = 16
    Height = 21
    Associate = EdLogFontSize
    Min = 7
    Max = 20
    Position = 10
    TabOrder = 4
  end
end
