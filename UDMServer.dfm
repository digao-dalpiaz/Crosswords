object DMServer: TDMServer
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
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
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 184
    Top = 80
  end
end
