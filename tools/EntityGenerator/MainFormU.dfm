object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'DORM Class creator [ALPHA VERSION]'
  ClientHeight = 384
  ClientWidth = 838
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
  object Splitter1: TSplitter
    Left = 0
    Top = 186
    Width = 838
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 169
    ExplicitWidth = 215
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 838
    Height = 58
    Align = alTop
    TabOrder = 0
    object Button2: TButton
      Left = 1
      Top = 1
      Width = 120
      Height = 56
      Align = alLeft
      Caption = 'Generate BO'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = Button2Click
    end
    object Edit1: TEdit
      Left = 127
      Top = 10
      Width = 200
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      Text = 'TMyBO'
      TextHint = 'Classname'
    end
    object Edit2: TEdit
      Left = 335
      Top = 10
      Width = 162
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      TextHint = 'Entity'
    end
    object NoPropertyCase: TCheckBox
      Left = 503
      Top = 4
      Width = 97
      Height = 17
      Caption = 'NoPropertyCase'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object Edit3: TEdit
      Left = 622
      Top = 5
      Width = 169
      Height = 21
      TabOrder = 4
      Text = 'InheritedClass'
    end
    object NoId: TCheckBox
      Left = 503
      Top = 22
      Width = 97
      Height = 17
      Caption = 'NoId'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
    object NoObjVersion: TCheckBox
      Left = 503
      Top = 39
      Width = 97
      Height = 17
      Caption = 'NoObjVersion'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 58
    Width = 838
    Height = 128
    Align = alTop
    Caption = 'Panel1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object Splitter2: TSplitter
      Left = 455
      Top = 20
      Height = 107
      ExplicitLeft = 352
      ExplicitTop = 16
      ExplicitHeight = 100
    end
    object Label2: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 830
      Height = 13
      Align = alTop
      Caption = 'FireDAC connection parameters'
      ExplicitWidth = 152
    end
    object Memo1: TMemo
      AlignWithMargins = True
      Left = 463
      Top = 25
      Width = 369
      Height = 97
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'Consolas'
      Font.Style = []
      Lines.Strings = (
        'select * from '
        'scheda_documento where id < '
        '1')
      ParentFont = False
      TabOrder = 0
    end
    object Memo3: TMemo
      AlignWithMargins = True
      Left = 4
      Top = 23
      Width = 448
      Height = 101
      Align = alLeft
      Color = clActiveBorder
      Lines.Strings = (
        'Memo3')
      TabOrder = 1
    end
  end
  object Memo2: TMemo
    Left = 0
    Top = 189
    Width = 838
    Height = 195
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database=ibsa_2013'
      'User_Name=sa'
      'Password=B4s1cs2013'
      'Server=PCNBASICS04\SQLEXPRESS2012'
      'DriverID=MSSQL')
    ConnectedStoredUsage = []
    LoginPrompt = False
    Left = 304
    Top = 112
  end
  object qry: TFDQuery
    Connection = FDConnection1
    FetchOptions.AssignedValues = [evRecsMax, evRowsetSize, evUnidirectional, evAutoFetchAll]
    FetchOptions.Unidirectional = True
    FetchOptions.RowsetSize = 1
    FetchOptions.RecsMax = 1
    FetchOptions.AutoFetchAll = afDisable
    Left = 328
    Top = 200
  end
  object FDPhysFBDriverLink1: TFDPhysFBDriverLink
    Left = 368
    Top = 32
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 424
    Top = 104
  end
  object FDPhysMSSQLDriverLink1: TFDPhysMSSQLDriverLink
    Left = 448
    Top = 200
  end
end
