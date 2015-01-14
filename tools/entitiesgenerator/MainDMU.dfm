object dmMain: TdmMain
  OldCreateOrder = False
  Height = 290
  Width = 533
  object FDPhysFBDriverLink1: TFDPhysFBDriverLink
    Left = 216
    Top = 24
  end
  object Connection: TFDConnection
    Params.Strings = (
      'Database=keystone2'
      'User_Name=sa'
      'Password=Password1'
      'Server=PCWIN701'
      'DriverID=MSSQL')
    ConnectedStoredUsage = []
    LoginPrompt = False
    Left = 152
    Top = 104
  end
  object FDPhysMSSQLDriverLink1: TFDPhysMSSQLDriverLink
    Left = 296
    Top = 192
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 272
    Top = 96
  end
  object qry: TFDQuery
    Connection = Connection
    FetchOptions.AssignedValues = [evRecsMax, evRowsetSize, evUnidirectional, evAutoFetchAll]
    FetchOptions.Unidirectional = True
    FetchOptions.RowsetSize = 1
    FetchOptions.RecsMax = 1
    FetchOptions.AutoFetchAll = afDisable
    Left = 176
    Top = 192
  end
end
