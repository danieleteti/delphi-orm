object Form11: TForm11
  Left = 0
  Top = 0
  Caption = 'Form11'
  ClientHeight = 256
  ClientWidth = 346
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 19
  object Button1: TButton
    Left = 24
    Top = 160
    Width = 273
    Height = 65
    Caption = 'Persist VCL controls to database'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    WordWrap = True
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 8
    Width = 313
    Height = 49
    Caption = 'Reload controls from database'
    TabOrder = 1
    OnClick = Button2Click
  end
end
