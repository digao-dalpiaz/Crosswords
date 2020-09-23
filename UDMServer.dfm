object DMServer: TDMServer
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 206
  Width = 359
  object S: TDzTCPServer
    OnClientLoginCheck = SClientLoginCheck
    OnClientLoginSuccess = SClientLoginSuccess
    OnClientDisconnect = SClientDisconnect
    OnClientRead = SClientRead
    Left = 88
    Top = 112
  end
end
