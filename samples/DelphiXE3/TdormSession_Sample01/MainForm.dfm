object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'DORM Sample 1'
  ClientHeight = 515
  ClientWidth = 763
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    763
    515)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 119
    Top = 16
    Width = 114
    Height = 49
    Caption = 'Create a Person, save it and refresh from db'
    TabOrder = 0
    WordWrap = True
    OnClick = Button1Click
  end
  object ListBox1: TListBox
    Left = 8
    Top = 71
    Width = 554
    Height = 397
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    ScrollWidth = 10
    TabOrder = 1
  end
  object Button2: TButton
    Left = 8
    Top = 16
    Width = 105
    Height = 49
    Caption = 'Clear DB Data'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 239
    Top = 16
    Width = 114
    Height = 49
    Caption = 'Create 3 person in the list, save the list and refresh from db'
    TabOrder = 3
    WordWrap = True
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 599
    Top = 16
    Width = 106
    Height = 49
    Caption = 'Refresh Data from DB'
    TabOrder = 4
    WordWrap = True
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 359
    Top = 16
    Width = 114
    Height = 49
    Caption = 'Add 3 person to the list (do not save in db)'
    TabOrder = 5
    WordWrap = True
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 479
    Top = 16
    Width = 114
    Height = 49
    Caption = 'Save List and Refresh from DB'
    TabOrder = 6
    WordWrap = True
    OnClick = Button6Click
  end
  object Panel1: TPanel
    Left = 0
    Top = 474
    Width = 763
    Height = 41
    Align = alBottom
    Caption = '...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
  end
  object ListBox2: TListBox
    Left = 568
    Top = 71
    Width = 187
    Height = 397
    Anchors = [akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 8
  end
  object dormSession1: TdormSession
    UseMappingFile = False
    Environment = deDevelopment
    ConfigFileName = 'dorm.conf'
    OnAfterPersistObject = dormSession1AfterPersistObject
    Left = 56
    Top = 88
  end
end
