object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'dorm (the Delphi ORM), Mapping file creator'
  ClientHeight = 473
  ClientWidth = 540
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    540
    473)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 45
    Width = 94
    Height = 13
    Caption = 'Available strategies'
  end
  object Label2: TLabel
    Left = 8
    Top = 164
    Width = 143
    Height = 13
    Caption = 'Selected strategy parameters'
  end
  object Label3: TLabel
    Left = 8
    Top = 309
    Width = 143
    Height = 13
    Caption = 'Mapping output filename path'
  end
  object Label4: TLabel
    Left = 8
    Top = 365
    Width = 56
    Height = 13
    Caption = 'Output logs'
  end
  object lbStrategies: TListBox
    Left = 8
    Top = 64
    Width = 337
    Height = 89
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbStrategiesClick
  end
  object vlUserProperties: TValueListEditor
    Left = 8
    Top = 183
    Width = 525
    Height = 114
    TabOrder = 1
    ColWidths = (
      177
      342)
  end
  object edtFileName: TEdit
    Left = 8
    Top = 328
    Width = 337
    Height = 21
    TabOrder = 2
    TextHint = 'Mapping file name path'
  end
  object Button1: TButton
    Left = 351
    Top = 326
    Width = 42
    Height = 25
    Action = acSelectFileName
    TabOrder = 3
  end
  object memLogs: TMemo
    AlignWithMargins = True
    Left = 8
    Top = 384
    Width = 524
    Height = 81
    Anchors = [akLeft, akTop, akRight, akBottom]
    Ctl3D = False
    ParentCtl3D = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 4
    WordWrap = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 540
    Height = 33
    Align = alTop
    TabOrder = 5
    object SpeedButton2: TSpeedButton
      AlignWithMargins = True
      Left = 322
      Top = 4
      Width = 122
      Height = 25
      Action = acGenerateMapping
      Align = alLeft
      ExplicitLeft = 271
      ExplicitHeight = 41
    end
    object SpeedButton3: TSpeedButton
      AlignWithMargins = True
      Left = 216
      Top = 4
      Width = 100
      Height = 25
      Action = asSaveProjectAs
      Align = alLeft
      ExplicitHeight = 41
    end
    object SpeedButton4: TSpeedButton
      AlignWithMargins = True
      Left = 110
      Top = 4
      Width = 100
      Height = 25
      Action = acSaveProject
      Align = alLeft
      ExplicitHeight = 41
    end
    object SpeedButton5: TSpeedButton
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 100
      Height = 25
      Action = acOpenProject
      Align = alLeft
      ExplicitHeight = 41
    end
  end
  object chkClasses: TCheckBox
    Left = 360
    Top = 64
    Width = 156
    Height = 17
    Caption = 'Generate Delphi Classes'
    TabOrder = 6
  end
  object ActionList1: TActionList
    Left = 192
    Top = 144
    object acGenerateMapping: TAction
      Caption = 'Generate mapping'
      OnExecute = acGenerateMappingExecute
      OnUpdate = acGenerateMappingUpdate
    end
    object acSelectFileName: TAction
      Caption = '...'
      OnExecute = acSelectFileNameExecute
    end
    object acOpenProject: TAction
      Caption = '&Open'
      ImageIndex = 1
      OnExecute = acOpenProjectExecute
    end
    object acSaveProject: TAction
      Caption = '&Save'
      ImageIndex = 0
      OnExecute = acSaveProjectExecute
    end
    object asSaveProjectAs: TAction
      Caption = 'Save as...'
    end
    object acClose: TAction
      Caption = '&Close'
      ImageIndex = 2
    end
  end
  object SaveTextFileDialog1: TSaveTextFileDialog
    Left = 88
    Top = 112
  end
  object MainMenu1: TMainMenu
    Left = 312
    Top = 96
    object File1: TMenuItem
      Caption = '&File'
      object Open1: TMenuItem
        Action = acOpenProject
      end
      object Save1: TMenuItem
        Action = acSaveProject
      end
      object Close1: TMenuItem
        Action = asSaveProjectAs
      end
      object Close2: TMenuItem
        Action = acClose
      end
    end
    object Mapping1: TMenuItem
      Caption = '&Mapping'
      object Generate1: TMenuItem
        Action = acGenerateMapping
      end
    end
  end
  object FileSaveDialog1: TFileSaveDialog
    DefaultExtension = 'dorm Mapping Creator|*.dmc'
    DefaultFolder = '.'
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'dorm Mapping Creator File'
        FileMask = '*.dmc'
      end>
    Options = [fdoOverWritePrompt]
    Title = 'Mapping file creator'
    Left = 200
    Top = 240
  end
  object FileOpenDialog1: TFileOpenDialog
    DefaultExtension = 'dorm Mapping Creator|*.dmc'
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'dorm Mapping Creator'
        FileMask = '*.dmc'
      end>
    Options = []
    Left = 72
    Top = 240
  end
end
