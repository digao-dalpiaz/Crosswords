object DMClient: TDMClient
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 272
  Width = 391
  object C: TDzTCPClient
    OnLoginRequest = CLoginRequest
    OnLoginResponse = CLoginResponse
    OnConnect = CConnect
    OnDisconnect = CDisconnect
    OnRead = CRead
    OnError = CError
    OnConnectionLost = CConnectionLost
    Left = 184
    Top = 112
  end
end
