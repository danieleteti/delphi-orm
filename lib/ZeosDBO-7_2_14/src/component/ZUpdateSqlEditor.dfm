object ZUpdateSQLEditForm: TZUpdateSQLEditForm
  Left = 339
  Top = 271
  Width = 406
  Height = 293
  ActiveControl = UpdateTableName
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 390
    Height = 223
    ActivePage = SQLPage
    Align = alClient
    TabOrder = 0
    OnChanging = PageControlChanging
    object FieldsPage: TTabSheet
      Caption = 'Options'
      object GroupBox1: TGroupBox
        Left = 0
        Top = 0
        Width = 382
        Height = 195
        Align = alClient
        Caption = ' SQL Generation '
        TabOrder = 0
        DesignSize = (
          382
          195)
        object Label1: TLabel
          Left = 8
          Top = 24
          Width = 61
          Height = 13
          Caption = 'Table &Name:'
          FocusControl = UpdateTableName
        end
        object Label3: TLabel
          Left = 128
          Top = 24
          Width = 51
          Height = 13
          Anchors = [akTop, akRight]
          Caption = '&Key Fields:'
          FocusControl = KeyFieldList
        end
        object Label4: TLabel
          Left = 256
          Top = 24
          Width = 68
          Height = 13
          Anchors = [akTop, akRight]
          Caption = 'Update &Fields:'
          FocusControl = UpdateFieldList
        end
        object UpdateTableName: TComboBox
          Left = 8
          Top = 40
          Width = 113
          Height = 21
          TabOrder = 0
          OnChange = UpdateTableNameChange
          OnClick = UpdateTableNameClick
        end
        object KeyFieldList: TListBox
          Left = 126
          Top = 40
          Width = 117
          Height = 146
          Anchors = [akLeft, akTop, akBottom]
          ItemHeight = 13
          MultiSelect = True
          PopupMenu = FieldListPopup
          TabOrder = 6
          OnClick = SettingsChanged
        end
        object UpdateFieldList: TListBox
          Left = 254
          Top = 40
          Width = 120
          Height = 146
          Anchors = [akLeft, akTop, akRight, akBottom]
          ItemHeight = 13
          MultiSelect = True
          PopupMenu = FieldListPopup
          TabOrder = 7
          OnClick = SettingsChanged
        end
        object GenerateButton: TButton
          Left = 8
          Top = 136
          Width = 113
          Height = 22
          Caption = '&Generate SQL'
          TabOrder = 4
          OnClick = GenerateButtonClick
        end
        object PrimaryKeyButton: TButton
          Left = 8
          Top = 112
          Width = 113
          Height = 22
          Caption = 'Select &Primary Keys'
          TabOrder = 3
          OnClick = PrimaryKeyButtonClick
        end
        object DefaultButton: TButton
          Left = 8
          Top = 88
          Width = 113
          Height = 21
          Caption = '&Dataset Defaults'
          Enabled = False
          TabOrder = 2
          OnClick = DefaultButtonClick
        end
        object QuoteFields: TCheckBox
          Left = 8
          Top = 168
          Width = 113
          Height = 15
          Caption = '&Quote Field Names'
          TabOrder = 5
          OnClick = SettingsChanged
        end
        object GetTableFieldsButton: TButton
          Left = 8
          Top = 64
          Width = 113
          Height = 21
          Caption = 'Get &Table Fields'
          TabOrder = 1
          OnClick = GetTableFieldsButtonClick
        end
      end
    end
    object SQLPage: TTabSheet
      Caption = 'SQL'
      object Label2: TLabel
        Left = 8
        Top = 40
        Width = 48
        Height = 13
        Caption = 'S&QL Text:'
        FocusControl = SQLMemo
      end
      object SQLMemo: TMemo
        Left = 8
        Top = 56
        Width = 368
        Height = 130
        Align = alCustom
        Anchors = [akLeft, akTop, akRight, akBottom]
        ScrollBars = ssVertical
        TabOrder = 0
        WordWrap = False
        OnKeyPress = SQLMemoKeyPress
      end
      object StatementType: TRadioGroup
        Left = 0
        Top = 0
        Width = 382
        Height = 35
        Align = alTop
        Caption = 'Statement Type'
        Columns = 3
        Items.Strings = (
          '&Modify'
          '&Insert'
          '&Delete')
        TabOrder = 1
        OnClick = StatementTypeClick
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 223
    Width = 390
    Height = 34
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      390
      34)
    object OkButton: TButton
      Left = 162
      Top = 5
      Width = 65
      Height = 22
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = OkButtonClick
    end
    object CancelButton: TButton
      Left = 239
      Top = 5
      Width = 65
      Height = 22
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object HelpButton: TButton
      Left = 315
      Top = 5
      Width = 65
      Height = 22
      Anchors = [akRight, akBottom]
      Caption = '&Help'
      TabOrder = 2
      OnClick = HelpButtonClick
    end
  end
  object FieldListPopup: TPopupMenu
    Left = 54
    Top = 270
    object miSelectAll: TMenuItem
      Caption = '&Select All'
      OnClick = SelectAllClick
    end
    object miClearAll: TMenuItem
      Caption = '&Clear All'
      OnClick = ClearAllClick
    end
  end
end
