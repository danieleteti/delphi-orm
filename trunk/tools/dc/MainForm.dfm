object frmMain: TfrmMain
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  BorderStyle = bsToolWindow
  Caption = 'dormDatabaseCreator'
  ClientHeight = 325
  ClientWidth = 324
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 318
    Height = 75
    Align = alTop
    Alignment = taCenter
    Caption = 
      'Generate SQL script, for a dorm managed sql database, starting f' +
      'rom configuration file'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
    ExplicitWidth = 307
  end
  object Label2: TLabel
    Left = 8
    Top = 207
    Width = 124
    Height = 13
    Caption = 'Configuration file full path'
  end
  object SpeedButton1: TSpeedButton
    Left = 279
    Top = 223
    Width = 35
    Height = 22
    Caption = '...'
    OnClick = SpeedButton1Click
  end
  object Button1: TButton
    Left = 8
    Top = 251
    Width = 150
    Height = 30
    Caption = 'DEV'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 287
    Width = 150
    Height = 30
    Caption = 'TEST'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 164
    Top = 287
    Width = 150
    Height = 30
    Caption = 'Open Last SCRIPT'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 164
    Top = 251
    Width = 150
    Height = 30
    Caption = 'RELEASE'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Edit1: TEdit
    Left = 8
    Top = 224
    Width = 265
    Height = 21
    TabOrder = 4
  end
  object ListBox1: TListBox
    Left = 8
    Top = 96
    Width = 306
    Height = 97
    ItemHeight = 13
    TabOrder = 5
  end
  object FileOpenDialog1: TFileOpenDialog
    DefaultExtension = '*.conf'
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'dorm config file (*.conf)'
        FileMask = '*.conf'
      end
      item
        DisplayName = 'All files'
        FileMask = '*.*'
      end>
    FileTypeIndex = 0
    Options = [fdoFileMustExist]
    Left = 208
    Top = 176
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.conf'
    Filter = 'dorm config file|*.conf|All Files|*.*'
    Left = 240
    Top = 176
  end
end
