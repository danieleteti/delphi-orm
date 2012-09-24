object ClientProxy: TClientProxy
  OldCreateOrder = False
  Height = 271
  Width = 415
  object SQLConnection1: TSQLConnection
    DriverName = 'DataSnap'
    LoginPrompt = False
    Params.Strings = (
      'Port=211'
      'CommunicationProtocol=tcp/ip'
      'DatasnapContext=datasnap/')
    Left = 48
    Top = 40
    UniqueId = '{5A73AC8B-F3D8-459E-AE4C-35D96CC05720}'
  end
end
