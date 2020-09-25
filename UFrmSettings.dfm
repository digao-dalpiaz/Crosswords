object FrmSettings: TFrmSettings
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Settings'
  ClientHeight = 201
  ClientWidth = 465
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
    Top = 123
    Width = 51
    Height = 13
    Caption = 'Grid zoom:'
  end
  object LbLanguage: TLabel
    Left = 16
    Top = 16
    Width = 51
    Height = 13
    Caption = 'Language:'
  end
  object CkSounds: TCheckBox
    Left = 16
    Top = 80
    Width = 97
    Height = 17
    Caption = 'Enable Sounds'
    TabOrder = 1
  end
  object BtnOK: TButton
    Left = 152
    Top = 168
    Width = 81
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 4
    OnClick = BtnOKClick
  end
  object BtnCancel: TButton
    Left = 240
    Top = 168
    Width = 81
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object EdGridZoom: TEdit
    Left = 96
    Top = 120
    Width = 49
    Height = 21
    Alignment = taRightJustify
    NumbersOnly = True
    TabOrder = 2
    Text = '50'
  end
  object BtnZoom: TUpDown
    Left = 145
    Top = 120
    Width = 16
    Height = 21
    Associate = EdGridZoom
    Min = 50
    Max = 200
    Position = 50
    TabOrder = 3
  end
  object EdLanguage: TComboBox
    Left = 16
    Top = 32
    Width = 249
    Height = 21
    Style = csDropDownList
    TabOrder = 0
  end
end
