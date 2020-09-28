object FrmLog: TFrmLog
  Left = 0
  Top = 0
  Align = alBottom
  BorderStyle = bsNone
  ClientHeight = 83
  ClientWidth = 487
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
  object EdLog: TRichEdit
    Left = 0
    Top = 0
    Width = 487
    Height = 83
    TabStop = False
    Align = alClient
    BorderStyle = bsNone
    Color = 1973790
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    StyleElements = [seClient, seBorder]
    Zoom = 100
  end
end
