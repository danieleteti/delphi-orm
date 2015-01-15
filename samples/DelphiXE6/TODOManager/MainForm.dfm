object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'ToDo List - A Sample App using DORM'
  ClientHeight = 562
  ClientWidth = 928
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -24
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 32
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 928
    Height = 52
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 631
      Height = 42
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alLeft
      Alignment = taCenter
      AutoSize = False
      Caption = 'My DORM TODO List'
      Layout = tlCenter
    end
    object BitBtn2: TBitBtn
      AlignWithMargins = True
      Left = 535
      Top = 5
      Width = 124
      Height = 42
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Action = acNew
      Align = alRight
      Caption = 'New'
      TabOrder = 0
    end
    object BitBtn3: TBitBtn
      AlignWithMargins = True
      Left = 799
      Top = 5
      Width = 124
      Height = 42
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Action = acDelete
      Align = alRight
      Caption = 'Delete'
      TabOrder = 1
    end
    object BitBtn5: TBitBtn
      AlignWithMargins = True
      Left = 667
      Top = 5
      Width = 124
      Height = 42
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Action = acEdit
      Align = alRight
      Caption = 'Edit'
      TabOrder = 2
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 502
    Width = 928
    Height = 60
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    TabOrder = 1
    object BitBtn4: TBitBtn
      AlignWithMargins = True
      Left = 133
      Top = 5
      Width = 120
      Height = 50
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Action = acPersist
      Align = alLeft
      Caption = 'Persist'
      TabOrder = 0
    end
    object BitBtn1: TBitBtn
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 120
      Height = 50
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Action = acRefresh
      Align = alLeft
      Caption = 'Refresh'
      TabOrder = 1
    end
    object chkFilter: TCheckBox
      AlignWithMargins = True
      Left = 645
      Top = 5
      Width = 278
      Height = 50
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alRight
      Caption = 'Only not completed'
      TabOrder = 2
      OnClick = chkFilterClick
    end
  end
  object StringGrid1: TStringGrid
    Tag = 3
    Left = 0
    Top = 52
    Width = 928
    Height = 450
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    ColCount = 3
    DoubleBuffered = True
    FixedCols = 0
    RowCount = 201
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
    ParentDoubleBuffered = False
    PopupMenu = PopupMenu1
    TabOrder = 2
    OnDblClick = StringGrid1DblClick
    OnDrawCell = StringGrid1DrawCell
    ColWidths = (
      500
      131
      96)
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
  object BindSourceTodos: TPrototypeBindSource
    AutoActivate = True
    AutoPost = False
    FieldDefs = <
      item
        Name = 'Title'
        Generator = 'LoremIpsum'
        ReadOnly = True
      end
      item
        Name = 'Description'
        Generator = 'LoremIpsum'
        ReadOnly = True
      end
      item
        Name = 'DueDate'
        FieldType = ftDate
        Generator = 'Date'
        ReadOnly = True
      end
      item
        Name = 'Done'
        FieldType = ftBoolean
        Generator = 'Booleans'
        ReadOnly = True
      end>
    ScopeMappings = <>
    OnCreateAdapter = BindSourceTodosCreateAdapter
    Left = 528
    Top = 128
  end
  object BindingsList1: TBindingsList
    Methods = <>
    OutputConverters = <>
    Left = 20
    Top = 5
    object LinkGridToDataSource1: TLinkGridToDataSource
      Category = 'Quick Bindings'
      DataSource = BindSourceTodos
      GridControl = StringGrid1
      Columns = <
        item
          MemberName = 'Title'
          Width = 500
        end
        item
          MemberName = 'DueDate'
          Header = 'Due Date'
          Width = 110
        end
        item
          MemberName = 'Done'
          Width = 60
          CustomFormat = #39#39
        end>
    end
  end
end
