object frmEditTodo: TfrmEditTodo
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Todo'
  ClientHeight = 324
  ClientWidth = 318
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  PopupMode = pmAuto
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 17
  object Label1: TLabel
    Left = 16
    Top = 53
    Width = 66
    Height = 17
    Caption = 'Description'
  end
  object Label2: TLabel
    Left = 16
    Top = 201
    Width = 53
    Height = 17
    Caption = 'Due date'
  end
  object edtTitle: TLabeledEdit
    Left = 16
    Top = 24
    Width = 281
    Height = 25
    EditLabel.Width = 24
    EditLabel.Height = 17
    EditLabel.Caption = 'Title'
    TabOrder = 0
  end
  object memDescription: TMemo
    Left = 16
    Top = 74
    Width = 281
    Height = 105
    TabOrder = 1
  end
  object dtpDueDate: TDateTimePicker
    Left = 16
    Top = 224
    Width = 105
    Height = 25
    Date = 0.943474120373139200
    Time = 0.943474120373139200
    TabOrder = 2
    OnChange = dtpDueDateChange
  end
  object chkDone: TCheckBox
    Left = 200
    Top = 224
    Width = 97
    Height = 17
    Caption = 'Done'
    TabOrder = 3
  end
  object BitBtn1: TBitBtn
    Left = 197
    Top = 282
    Width = 113
    Height = 34
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 4
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 78
    Top = 282
    Width = 113
    Height = 34
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 5
    OnClick = BitBtn2Click
  end
  object ActionList1: TActionList
    Left = 216
    Top = 96
    object acLoad: TAction
      Caption = 'Load'
    end
    object acUpdate: TAction
      Caption = 'Update'
    end
    object acSaveAsNew: TAction
      Caption = 'Save as New'
    end
    object acCommitAndRestart: TAction
      Caption = 'Commit And Restart Transaction'
    end
  end
  object BindSourceTodo: TPrototypeBindSource
    AutoActivate = False
    RecordCount = 1
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
    OnCreateAdapter = BindSourceTodoCreateAdapter
    Left = 138
    Top = 128
  end
  object BindingsList1: TBindingsList
    Methods = <>
    OutputConverters = <>
    UseAppManager = True
    Left = 20
    Top = 5
    object LinkControlToField1: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = BindSourceTodo
      FieldName = 'Title'
      Control = edtTitle
      Track = False
    end
    object LinkControlToField2: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = BindSourceTodo
      FieldName = 'Description'
      Control = memDescription
      Track = False
    end
    object LinkPropertyToField1: TLinkPropertyToField
      Category = 'Quick Bindings'
      DataSource = BindSourceTodo
      FieldName = 'DueDate'
      Component = dtpDueDate
      ComponentProperty = 'Date'
    end
    object LinkControlToField3: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = BindSourceTodo
      FieldName = 'Done'
      Control = chkDone
      Track = True
    end
  end
end
