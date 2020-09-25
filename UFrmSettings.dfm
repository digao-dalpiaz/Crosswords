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
    Top = 59
    Width = 51
    Height = 13
    Caption = 'Grid zoom:'
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
    Left = 152
    Top = 168
    Width = 81
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 3
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
    TabOrder = 4
  end
  object EdGridZoom: TEdit
    Left = 80
    Top = 56
    Width = 49
    Height = 21
    Alignment = taRightJustify
    NumbersOnly = True
    TabOrder = 1
    Text = '0'
  end
  object BtnZoom: TUpDown
    Left = 129
    Top = 56
    Width = 16
    Height = 21
    Associate = EdGridZoom
    Min = 50
    Max = 200
    TabOrder = 2
  end
end
