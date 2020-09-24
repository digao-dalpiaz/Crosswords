unit UDMServer;

interface

uses
  System.Classes, DzSocket, UMatrix;

type
  TDMServer = class(TDataModule)
    S: TDzTCPServer;
    procedure DataModuleCreate(Sender: TObject);
    procedure SClientLoginCheck(Sender: TObject; Socket: TDzSocket;
      var Accept: Boolean; const RequestData: string; var ResponseData: string);
    procedure SClientLoginSuccess(Sender: TObject; Socket: TDzSocket);
    procedure SClientDisconnect(Sender: TObject; Socket: TDzSocket);
    procedure SClientRead(Sender: TObject; Socket: TDzSocket; const Cmd: Char;
      const A: string);
  private
    GameRunning, InAgreement: Boolean;
    CurrentPlayerIndex: Integer;

    Matrix: TMatrixData;

    function PlayerNameAlreadyExists(const PlayerName: string): Boolean;
    procedure SendPlayersList(Exclude: TDzSocket = nil);
    procedure SendLetters(Socket: TDzSocket);
    procedure SelectNextPlayer;
    procedure SendMatrix;
    procedure LetterReceived(Socket: TDzSocket; const A: string);
    procedure MessageReceived(Socket: TDzSocket; const A: string);
    function GetCurrentSocket: TDzSocket;
    procedure PlayerTurnDoneReceived(Socket: TDzSocket);
    procedure AgreementReceived(Socket: TDzSocket; const A: string);
    procedure ClearAllAgreements;
    function IsAllPlayersAgree(Socket: TDzSocket): Boolean;
    procedure CompletePlayerTurn(Socket: TDzSocket);
    function IsThereLettersUsed: Boolean;
    procedure RebuyLetters(Socket: TDzSocket);
  public
    procedure Initialize;
    procedure StartGame;
    procedure SendRules(Socket: TDzSocket);
  end;

var
  DMServer: TDMServer;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses UVars, UClient, System.SysUtils, System.Variants;

procedure TDMServer.DataModuleCreate(Sender: TObject);
begin
  S.Port := INT_TCP_PORT;

  S.AutoFreeObjs := True;
  S.EnumeratorOnlyAuth := True;
end;

procedure TDMServer.Initialize;
begin
  GameRunning := False;
  InAgreement := False;
  CurrentPlayerIndex := -1;
  SetLength(Matrix, 0);

  S.Open;
end;

function TDMServer.PlayerNameAlreadyExists(const PlayerName: string): Boolean;
var
  Sok: TDzSocket;
begin
  Result := False;

  S.Lock;
  try
    for Sok in S do
      if SameText(TClient(Sok.Data).PlayerName, PlayerName) then Exit(True);
  finally
    S.Unlock;
  end;
end;

procedure TDMServer.SClientLoginCheck(Sender: TObject; Socket: TDzSocket;
  var Accept: Boolean; const RequestData: string; var ResponseData: string);
var
  D: TMsgArray;
  C: TClient;
begin
  D := DataToArray(RequestData);

  //Check app version
  if D[0] <> STR_VERSION then
  begin
    Accept := False;
    ResponseData := 'This version is incompatible with the server!';
    Exit;
  end;

  //Check game password
  if D[2] <> pubPassword then
  begin
    Accept := False;
    ResponseData := 'The password to this game is incorrect.';
    Exit;
  end;

  //Check if game is already running
  if GameRunning then
  begin
    Accept := False;
    ResponseData := 'You cannot join the game because it is already running!';
    Exit;
  end;

  //Check if player name already exists
  if PlayerNameAlreadyExists(D[1]) then
  begin
    Accept := False;
    ResponseData := 'This player name already exists!';
    Exit;
  end;

  //LOGIN IS VALID!
  C := TClient.Create;
  C.PlayerName := D[1];
  Socket.Data := C;
end;

procedure TDMServer.SClientLoginSuccess(Sender: TObject; Socket: TDzSocket);
var
  C: TClient;
begin
  C := Socket.Data;
  S.SendAllEx(Socket, 'C', C.PlayerName);

  SendPlayersList;
  SendRules(Socket); //send game rules to the player
end;

procedure TDMServer.SClientDisconnect(Sender: TObject; Socket: TDzSocket);
var
  C: TClient;
begin
  if not Socket.Auth then Exit;

  C := Socket.Data;
  S.SendAllEx(Socket, 'D', C.PlayerName);

  SendPlayersList(Socket);
end;

procedure TDMServer.SClientRead(Sender: TObject; Socket: TDzSocket;
  const Cmd: Char; const A: string);
begin
  case Cmd of
    'M': MessageReceived(Socket, A);
    '#': LetterReceived(Socket, A);
    '!': PlayerTurnDoneReceived(Socket);
    'H': AgreementReceived(Socket, A);
  end;
end;

procedure TDMServer.MessageReceived(Socket: TDzSocket; const A: string);
var
  C: TClient;
begin
  C := Socket.Data;
  S.SendAllEx(Socket, 'M', ArrayToData([C.PlayerName, A]));
end;

procedure TDMServer.SendPlayersList(Exclude: TDzSocket);
var
  Sok, CurSok: TDzSocket;
  C: TClient;
  Lst: TStringList;
begin
  CurSok := GetCurrentSocket;

  Lst := TStringList.Create;
  try
    S.Lock;
    try
      for Sok in S do
      begin
        if Sok = Exclude then Continue;

        C := Sok.Data;
        Lst.Add(ArrayToData([C.PlayerName, C.Letters.Length, C.Score, Sok=CurSok, C.Agree]));
      end;
    finally
      S.Unlock;
    end;

    S.SendAllEx(Exclude, 'L', Lst.Text);
  finally
    Lst.Free;
  end;
end;

procedure TDMServer.StartGame;
var
  Sok: TDzSocket;
  C: TClient;
begin
  GameRunning := True;

  LoadDictionaryLetters;

  //--Get players letters and send to each one
  S.Lock;
  try
    for Sok in S do
    begin
      C := Sok.Data;
      C.RandomizeInitialLetters;

      SendLetters(Sok);
    end;
  finally
    S.Unlock;
  end;
  //--

  SetLength(Matrix, pubServerProps.SizeH, pubServerProps.SizeW);
  Matrix[pubServerProps.SizeH div 2, pubServerProps.SizeW div 2].&Set(GetRandomLetter, False);
  SendMatrix;

  S.SendAll('R'); //send start game signal to all

  SelectNextPlayer;
end;

procedure TDMServer.SendLetters(Socket: TDzSocket);
var
  C: TClient;
begin
  C := Socket.Data;
  S.Send(Socket, 'T', C.Letters);
end;

procedure TDMServer.SelectNextPlayer;
begin
  Inc(CurrentPlayerIndex);
  if CurrentPlayerIndex>S.GetAuthConnections-1 then
    CurrentPlayerIndex := 0;

  SendPlayersList; //update players list

  S.Send(GetCurrentSocket, '>'); //send to current player its turn signal
end;

function TDMServer.GetCurrentSocket: TDzSocket;
var
  Sok: TDzSocket;
  I: Integer;
begin
  I := -1;

  S.Lock;
  try
    for Sok in S do
    begin
      Inc(I);
      if I=CurrentPlayerIndex then Exit(Sok);
    end;
  finally
    S.Unlock;
  end;

  Result := nil;
end;

procedure TDMServer.SendMatrix;
begin
  S.SendAll('X', MatrixDataToString(Matrix));
end;

procedure TDMServer.LetterReceived(Socket: TDzSocket; const A: string);
var
  D: TMsgArray;
  X, Y: Integer;
  B: TBlock;
  Letter: Char;
begin
  if InAgreement then
    raise Exception.Create('Internal: A player tried to define a letter in agreement period!');

  if Socket<>GetCurrentSocket then
    raise Exception.Create('Internal: A player tried to define a letter when is not its turn!');

  D := DataToArray(A);

  X := D[0];
  Y := D[1];

  B := Matrix[Y, X];
  if not ( (B.Letter=BLANK_LETTER) or (B.Temp) ) then
    raise Exception.Create('Internal: A player tried to define a letter in an unallowed condition!');

  Letter := VarToStr(D[2])[1];

  Matrix[Y, X].&Set(Letter, True);

  SendMatrix; //mandar para todos a matrix atualizada
end;

procedure TDMServer.PlayerTurnDoneReceived(Socket: TDzSocket);
begin
  if InAgreement then
    raise Exception.Create('Internal: A player tried to set its turn done in agreeement period!');

  if Socket<>GetCurrentSocket then
    raise Exception.Create('Internal: A player tried to set its turn done when is not its turn!');

  if IsThereLettersUsed then
  begin
    InAgreement := True;
    S.Send(Socket, 'W'); //send wait log
    S.SendAllEx(Socket, 'G'); //send agreement request signal
  end else
  begin
    RebuyLetters(Socket);
    SelectNextPlayer;
  end;
end;

procedure TDMServer.AgreementReceived(Socket: TDzSocket; const A: string);
var CurSok: TDzSocket;

  procedure DisableAgreement;
  begin
     InAgreement := False;
     S.SendAllEx(CurSok, 'K');

     ClearAllAgreements;
  end;

begin
  if not InAgreement then
    raise Exception.Create('Internal: A player tried to set agreement but not in agreeement period!');

  CurSok := GetCurrentSocket;

  if DataToArray(A)[0]{Agree} then
  begin
    TClient(Socket.Data).Agree := True;

    if IsAllPlayersAgree(CurSok) then //Check if all players have set agreement
    begin
      DisableAgreement;
      CompletePlayerTurn(CurSok); //SendPlayersList will be called here
      Exit;
    end;
  end else
  begin
    DisableAgreement;
    S.Send(CurSok, 'J'); //send reject agreement to current player
  end;

  SendPlayersList; //update players list
end;

function TDMServer.IsAllPlayersAgree(Socket: TDzSocket): Boolean;
var
  Sok: TDzSocket;
begin
  S.Lock;
  try
    for Sok in S do
      if Sok<>Socket then
        if not TClient(Sok.Data).Agree then Exit(False);
  finally
    S.Unlock;
  end;

  Result := True;
end;

procedure TDMServer.ClearAllAgreements;
var
  Sok: TDzSocket;
begin
  S.Lock;
  try
    for Sok in S do
      TClient(Sok.Data).Agree := False;
  finally
    S.Unlock;
  end;
end;

function TDMServer.IsThereLettersUsed: Boolean;
var
  X, Y: Integer;
begin
  for Y := 0 to High(Matrix) do
    for X := 0 to High(Matrix[Y]) do
      if Matrix[Y, X].Temp then Exit(True);

  Result := False;
end;

procedure TDMServer.CompletePlayerTurn(Socket: TDzSocket);
var
  X, Y: Integer;
  B: TBlock;
  C: TClient;
  RemLetters, StoLetters: string;
begin
  C := Socket.Data;

  StoLetters := C.Letters;

  for Y := 0 to High(Matrix) do
  begin
    for X := 0 to High(Matrix[Y]) do
    begin
      B := Matrix[Y, X];
      if B.Temp then
      begin
        //remove used letter from player letters list
        RemLetters := StoLetters.Replace(B.Letter, string.Empty, [{avoid replace all}]);
        if RemLetters = StoLetters then
          raise Exception.Create('Internal: A player tried to use an inexistent letter!');

        StoLetters := RemLetters;
        Matrix[Y, X].ClearTemp;
      end;
    end;
  end;

  C.Letters := StoLetters;

  SendMatrix;
  S.Send(Socket, 'F'); //finish turn log
  SelectNextPlayer;
end;

procedure TDMServer.RebuyLetters(Socket: TDzSocket);
var
  I: Integer;
  C: TClient;
begin
  C := Socket.Data;

  for I := 1 to pubServerProps.RebuyLetters do
    C.Letters := C.Letters + GetRandomLetter;

  SendLetters(Socket);
  S.Send(Socket, 'B', pubServerProps.RebuyLetters.ToString);
end;

procedure TDMServer.SendRules(Socket: TDzSocket);
var
  A: string;
begin
  A := ArrayToData([
    LST_DICTIONARY[GetDictionaryIndexByID(pubServerProps.DictionaryID)].LanguageName,
    pubServerProps.SizeW,
    pubServerProps.SizeH,
    pubServerProps.InitialLetters,
    pubServerProps.RebuyLetters]);

  if Socket<>nil then
    S.Send(Socket, 'u', A)
  else
    S.SendAll('U', A);
end;

end.
