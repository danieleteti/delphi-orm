object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'DORM Sample 1'
  ClientHeight = 320
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 159
    Top = 16
    Width = 234
    Height = 49
    Caption = 'Create a Person and fill list'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ListBox1: TListBox
    Left = 16
    Top = 71
    Width = 377
    Height = 241
    ItemHeight = 13
    TabOrder = 1
  end
  object Button2: TButton
    Left = 16
    Top = 16
    Width = 137
    Height = 49
    Caption = 'Clear Table'
    TabOrder = 2
    OnClick = Button2Click
  end
  object dormSession1: TdormSession
    UseMappingFile = False
    Environment = deDevelopment
    ConfigFileName = 'dorm.conf'
    Left = 208
    Top = 24
  end
end
