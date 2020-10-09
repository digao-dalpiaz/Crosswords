object FrmRules: TFrmRules
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Game Rules'
  ClientHeight = 297
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
  object LbHandLetters: TLabel
    Left = 16
    Top = 115
    Width = 169
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Letters in hand:'
  end
  object Bevel1: TBevel
    Left = 16
    Top = 256
    Width = 473
    Height = 9
    Shape = bsTopLine
  end
  object LbTurnTimeoutSecs: TLabel
    Left = 32
    Top = 203
    Width = 169
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Turn timeout (sec):'
  end
  object LbGoalScore: TLabel
    Left = 16
    Top = 139
    Width = 169
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Goal score:'
  end
  object LbAgreementTimeoutSecs: TLabel
    Left = 32
    Top = 227
    Width = 169
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Agreement timeout (sec):'
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
  object EdHandLetters: TEdit
    Left = 192
    Top = 112
    Width = 41
    Height = 21
    Alignment = taRightJustify
    NumbersOnly = True
    TabOrder = 3
  end
  object BtnOK: TButton
    Left = 168
    Top = 264
    Width = 81
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 8
    OnClick = BtnOKClick
  end
  object BtnCancel: TButton
    Left = 256
    Top = 264
    Width = 81
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 9
  end
  object CkTurnTimeout: TCheckBox
    Left = 16
    Top = 176
    Width = 153
    Height = 17
    Caption = 'Player turn time-out'
    TabOrder = 5
  end
  object EdTurnTimeoutSecs: TEdit
    Left = 208
    Top = 200
    Width = 49
    Height = 21
    Alignment = taRightJustify
    NumbersOnly = True
    TabOrder = 6
  end
  object EdGoalScore: TEdit
    Left = 192
    Top = 136
    Width = 41
    Height = 21
    Alignment = taRightJustify
    NumbersOnly = True
    TabOrder = 4
  end
  object EdAgreementTimeoutSecs: TEdit
    Left = 208
    Top = 224
    Width = 49
    Height = 21
    Alignment = taRightJustify
    NumbersOnly = True
    TabOrder = 7
  end
end
