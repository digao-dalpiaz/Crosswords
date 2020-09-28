object FrmRules: TFrmRules
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Game Rules'
  ClientHeight = 235
  ClientWidth = 505
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
  object LbTableSize: TLabel
    Left = 16
    Top = 19
    Width = 51
    Height = 13
    Caption = 'Table size:'
  end
  object LbTableSizeX: TLabel
    Left = 176
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
  object LbInitialLetters: TLabel
    Left = 16
    Top = 123
    Width = 99
    Height = 13
    Caption = 'Initial letters pocket:'
  end
  object LbRebuyLetters: TLabel
    Left = 16
    Top = 147
    Width = 69
    Height = 13
    Caption = 'Rebuy letters:'
  end
  object Bevel1: TBevel
    Left = 16
    Top = 192
    Width = 473
    Height = 9
    Shape = bsTopLine
  end
  object EdSizeW: TEdit
    Left = 128
    Top = 16
    Width = 41
    Height = 21
    Alignment = taRightJustify
    NumbersOnly = True
    TabOrder = 0
  end
  object EdSizeH: TEdit
    Left = 192
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
  object BtnOK: TButton
    Left = 168
    Top = 200
    Width = 81
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 5
    OnClick = BtnOKClick
  end
  object BtnCancel: TButton
    Left = 256
    Top = 200
    Width = 81
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
end
