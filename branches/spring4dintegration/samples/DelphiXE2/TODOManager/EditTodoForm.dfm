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
    Lines.Strings = (
      'memDescription')
    TabOrder = 1
  end
  object dtpDueDate: TDateTimePicker
    Left = 16
    Top = 224
    Width = 105
    Height = 25
    Date = 41254.943474120370000000
    Time = 41254.943474120370000000
    TabOrder = 2
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
      OnExecute = acLoadExecute
    end
    object acUpdate: TAction
      Caption = 'Update'
      OnExecute = acUpdateExecute
    end
    object acSaveAsNew: TAction
      Caption = 'Save as New'
      OnExecute = acSaveAsNewExecute
      OnUpdate = acSaveAsNewUpdate
    end
    object acCommitAndRestart: TAction
      Caption = 'Commit And Restart Transaction'
    end
  end
end
