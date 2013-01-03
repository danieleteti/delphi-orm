object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'ToDo List - A Sample App using DORM'
  ClientHeight = 439
  ClientWidth = 725
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 25
  object lbTodos: TListBox
    Left = 0
    Top = 41
    Width = 725
    Height = 342
    Align = alClient
    BorderStyle = bsNone
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Consolas'
    Font.Style = []
    IntegralHeight = True
    ItemHeight = 19
    ParentFont = False
    PopupMenu = PopupMenu1
    TabOrder = 0
    OnDblClick = lbTodosDblClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 725
    Height = 41
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 493
      Height = 33
      Align = alLeft
      Alignment = taCenter
      AutoSize = False
      Caption = 'My DORM TODO List'
      Layout = tlCenter
    end
    object BitBtn2: TBitBtn
      AlignWithMargins = True
      Left = 418
      Top = 4
      Width = 97
      Height = 33
      Action = acNew
      Align = alRight
      Caption = 'New'
      TabOrder = 0
    end
    object BitBtn3: TBitBtn
      AlignWithMargins = True
      Left = 624
      Top = 4
      Width = 97
      Height = 33
      Action = acDelete
      Align = alRight
      Caption = 'Delete'
      TabOrder = 1
    end
    object BitBtn5: TBitBtn
      AlignWithMargins = True
      Left = 521
      Top = 4
      Width = 97
      Height = 33
      Action = acEdit
      Align = alRight
      Caption = 'Edit'
      TabOrder = 2
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 392
    Width = 725
    Height = 47
    Align = alBottom
    TabOrder = 2
    object BitBtn4: TBitBtn
      AlignWithMargins = True
      Left = 104
      Top = 4
      Width = 94
      Height = 39
      Action = acPersist
      Align = alLeft
      Caption = 'Persist'
      TabOrder = 0
    end
    object BitBtn1: TBitBtn
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 94
      Height = 39
      Action = acRefresh
      Align = alLeft
      Caption = 'Refresh'
      TabOrder = 1
    end
    object chkFilter: TCheckBox
      AlignWithMargins = True
      Left = 504
      Top = 4
      Width = 217
      Height = 39
      Align = alRight
      Caption = 'Only not completed'
      TabOrder = 2
      OnClick = chkFilterClick
    end
  end
  object ActionList1: TActionList
    Left = 192
    Top = 208
    object acRefresh: TAction
      Caption = 'Refresh'
      OnExecute = acRefreshExecute
    end
    object acNew: TAction
      Caption = 'New'
      OnExecute = acNewExecute
    end
    object acEdit: TAction
      Caption = 'Edit'
      OnExecute = acEditExecute
      OnUpdate = acEditUpdate
    end
    object acPersist: TAction
      Caption = 'Persist'
      OnExecute = acPersistExecute
    end
    object acDelete: TAction
      Caption = 'Delete'
      OnExecute = acDeleteExecute
      OnUpdate = acDeleteUpdate
    end
    object acCreateRandomData: TAction
      Caption = 'Create some random data'
      OnExecute = acCreateRandomDataExecute
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 360
    Top = 136
    object Createsomerandomdata1: TMenuItem
      Action = acCreateRandomData
    end
  end
end
