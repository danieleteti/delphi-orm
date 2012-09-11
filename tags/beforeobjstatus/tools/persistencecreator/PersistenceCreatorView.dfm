object frmPersistenceCreatorView: TfrmPersistenceCreatorView
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'dorm (the Delphi ORM), Persistence file creator'
  ClientHeight = 369
  ClientWidth = 772
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 772
    Height = 369
    Align = alClient
    TabOrder = 0
    object lblEnvironments: TLabel
      Left = 8
      Top = 61
      Width = 65
      Height = 13
      Caption = 'environments'
    end
    object lblDatabaseAdapter: TLabel
      Left = 135
      Top = 61
      Width = 89
      Height = 13
      Caption = 'database_adapter'
    end
    object lblDatabaseConnectionString: TLabel
      Left = 135
      Top = 107
      Width = 136
      Height = 13
      Caption = 'database_connection_string'
    end
    object lblKeysGenerator: TLabel
      Left = 135
      Top = 153
      Width = 76
      Height = 13
      Caption = 'keys_generator'
    end
    object lblKeyType: TLabel
      Left = 135
      Top = 199
      Width = 45
      Height = 13
      Caption = 'key_type'
    end
    object lblNullKeyValue: TLabel
      Left = 135
      Top = 245
      Width = 71
      Height = 13
      Caption = 'null_key_value'
    end
    object lblLoggerClassName: TLabel
      Left = 135
      Top = 291
      Width = 91
      Height = 13
      Caption = 'logger_class_name'
    end
    object lblCustomAdapterConfig: TLabel
      Left = 471
      Top = 61
      Width = 107
      Height = 13
      Caption = 'custom adapter config'
    end
    object lblPersistentClasses: TLabel
      Left = 8
      Top = 199
      Width = 88
      Height = 13
      Caption = 'persistent_classes'
    end
    object lbxEnvironments: TListBox
      Left = 8
      Top = 80
      Width = 121
      Height = 113
      ItemHeight = 13
      TabOrder = 0
      OnClick = lbxEnvironmentsClick
    end
    object edtDatabaseConnectionString: TEdit
      Left = 135
      Top = 126
      Width = 329
      Height = 21
      TabOrder = 1
    end
    object btnOpenPersistenceFile: TButton
      Left = 119
      Top = 16
      Width = 105
      Height = 25
      Caption = 'Open persistent file'
      TabOrder = 2
      OnClick = btnOpenPersistenceFileClick
    end
    object btnSavePersistenceFile: TButton
      Left = 230
      Top = 16
      Width = 114
      Height = 25
      Caption = 'Save persistent file'
      TabOrder = 3
      OnClick = btnSavePersistenceFileClick
    end
    object edtNullKeyValue: TEdit
      Left = 135
      Top = 264
      Width = 329
      Height = 21
      TabOrder = 4
    end
    object cmbKeyType: TComboBox
      Left = 135
      Top = 218
      Width = 329
      Height = 21
      Style = csDropDownList
      TabOrder = 5
    end
    object cmbDatabaseAdapter: TComboBox
      Left = 135
      Top = 80
      Width = 329
      Height = 21
      Style = csDropDownList
      TabOrder = 6
    end
    object cmbKeysGenerator: TComboBox
      Left = 135
      Top = 172
      Width = 329
      Height = 21
      Style = csDropDownList
      TabOrder = 7
    end
    object cmbLoggerClassName: TComboBox
      Left = 135
      Top = 310
      Width = 329
      Height = 21
      Style = csDropDownList
      TabOrder = 8
    end
    object vleCustomAdapterConfig: TValueListEditor
      Left = 471
      Top = 80
      Width = 293
      Height = 251
      KeyOptions = [keyEdit, keyAdd, keyDelete, keyUnique]
      TabOrder = 9
      ColWidths = (
        150
        137)
    end
    object btnCreateNewMappingFile: TButton
      Left = 350
      Top = 16
      Width = 135
      Height = 25
      Caption = 'Create new mapping file'
      Enabled = False
      TabOrder = 10
    end
    object btnPickMappingFile: TButton
      Left = 491
      Top = 16
      Width = 135
      Height = 25
      Caption = 'Pick mapping file'
      Enabled = False
      TabOrder = 11
    end
    object lbxPersistentClasses: TListBox
      Left = 8
      Top = 218
      Width = 121
      Height = 113
      ItemHeight = 13
      PopupMenu = mnuPersistentClasses
      TabOrder = 12
    end
    object stbStatus: TStatusBar
      Left = 1
      Top = 349
      Width = 770
      Height = 19
      Panels = <
        item
          Width = 300
        end
        item
          Width = 400
        end>
    end
    object btnNewPersistentFile: TButton
      Left = 8
      Top = 16
      Width = 105
      Height = 25
      Caption = 'New persistent file'
      TabOrder = 14
      OnClick = btnNewPersistentFileClick
    end
  end
  object mnuPersistentClasses: TPopupMenu
    Left = 56
    Top = 232
    object mnuAddNewClass: TMenuItem
      Caption = '&Add New Class'
      OnClick = mnuAddNewClassClick
    end
    object mnuRemoveClass: TMenuItem
      Caption = '&Remove Class'
      OnClick = mnuRemoveClassClick
    end
  end
end
