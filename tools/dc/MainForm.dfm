object Form6: TForm6
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'dormDatabaseCreator'
  ClientHeight = 397
  ClientWidth = 324
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 318
    Height = 50
    Align = alTop
    Alignment = taCenter
    Caption = 'Generate SQL script for a dorm managed sql database'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
    ExplicitWidth = 294
  end
  object Label2: TLabel
    Left = 8
    Top = 61
    Width = 124
    Height = 13
    Caption = 'Configuration file full path'
  end
  object Button1: TButton
    Left = 8
    Top = 116
    Width = 150
    Height = 30
    Caption = 'DEV'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 152
    Width = 150
    Height = 30
    Caption = 'TEST'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 164
    Top = 152
    Width = 150
    Height = 30
    Caption = 'Open Last SCRIPT'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 164
    Top = 116
    Width = 150
    Height = 30
    Caption = 'RELEASE'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Edit1: TEdit
    Left = 8
    Top = 80
    Width = 306
    Height = 21
    TabOrder = 4
  end
end
