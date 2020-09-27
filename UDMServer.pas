unit UDMServer;

interface

uses
  System.Classes, DzSocket, UMatrix, UClient;

type
  TForConnectionsProc = reference to procedure(Sok: TDzSocket; C: TClient; var Cancel: Boolean);

  TServerStatus = (ssPreparing, ssTurn, ssAgreement, ssFreezed, ssOver);

  TDMServer = class(TDataModule)
    S: TDzTCPServer;
    procedure DataModuleCreate(Sender: TObject);
    procedure SClientLoginCheck(Sender: TObject; Socket: TDzSocket;
      var Accept: Boolean; const RequestData: string; var ResponseData: string);
    procedure SClientLoginSuccess(Sender: TObject; Socket: TDzSocket);
    procedure SClientDisconnect(Sender: TObject; Socket: TDzSocket);
    procedure SClientRead(Sender: TObject; Socket: TDzSocket; const Cmd: Char;
      const A: string);
    procedure DataModuleDestroy(Sender: TObject);
  private
    PlayersList: TPlayersList;
    CurrentPlayerIndex: Integer;
    Status: TServerStatus;
    Matrix: TMatrixData;

    function PlayerNameAlreadyExists(const PlayerName: string): Boolean;
    procedure SendPlayersList(Exclude: TDzSocket = nil);
    procedure SendLetters(Socket: TDzSocket);
    procedure SelectNextPlayer;
    procedure SendMatrix;
    procedure LetterReceived(Socket: TDzSocket; const A: string);
    procedure MessageReceived(Socket: TDzSocket; const A: string);
    function GetCurrentPlayer: TClient;
    procedure PlayerTurnDoneReceived(Socket: TDzSocket);
    procedure AgreementReceived(Socket: TDzSocket; const A: string);
    procedure ClearAllAgreements;
    function IsAllPlayersAgree(WithSocket: TDzSocket): Boolean;
    procedure CompletePlayerTurn(Socket: TDzSocket);
    function IsThereLettersUsed: Boolean;
    procedure RebuyLetters(Socket: TDzSocket);
    procedure ForConnections(P: TForConnectionsProc);
    procedure SetGameOver;
    procedure RemoveAllDisconectedPlayers;
  public
    procedure Initialize;
    procedure StartGame;
    procedure SendRules(Socket: TDzSocket);
    procedure RestartGame;
    procedure KillPlayer(C: TClient);
    procedure ContinueGame;
    function FindPlayerByHash(const Hash: string): TClient;
  end;

var
  DMServer: TDMServer;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses UVars, System.SysUtils, System.Variants, UFrmRules, UFrmDrop;

procedure TDMServer.DataModuleCreate(Sender: TObject);
begin
  S.Port := INT_TCP_PORT;
  S.EnumeratorOnlyAuth := True;

  PlayersList := TPlayersList.Create;
end;

procedure TDMServer.DataModuleDestroy(Sender: TObject);
begin
  PlayersList.Free;
end;

procedure TDMServer.Initialize;
begin
  PlayersList.Clear;
  CurrentPlayerIndex := -1;
  SetLength(Matrix, 0);
  Status := ssPreparing;

  TRules.Load; //load game rules

  S.Open;
end;

procedure TDMServer.ForConnections(P: TForConnectionsProc);
var
  C: TClient;
  Cancel: Boolean;
begin
  Cancel := False;

  S.Lock;
  try
    for C in PlayersList do
    begin
      P(C.Socket, C, Cancel);
      if Cancel then Break;
    end;
  finally
    S.Unlock;
  end;
end;

procedure TDMServer.RemoveAllDisconectedPlayers;
var
  I: Integer;
begin
  S.Lock;
  try
    for I := PlayersList.Count-1 downto 0 do
      if PlayersList[I].Socket=nil then PlayersList.Delete(I);
  finally
    S.Unlock;
  end;
end;

function TDMServer.FindPlayerByHash(const Hash: string): TClient;
var
  ResClient: TClient;
begin
  ResClient := nil;

  ForConnections(
    procedure(Sok: TDzSocket; C: TClient; var Cancel: Boolean)
    begin
      if C.Hash = Hash then
      begin
        ResClient := C;
        Cancel := True;
      end;
    end
  );

  Result := ResClient;
end;

function TDMServer.PlayerNameAlreadyExists(const PlayerName: string): Boolean;
var
  Found: Boolean;
begin
  Found := False;

  ForConnections(
    procedure(Sok: TDzSocket; C: TClient; var Cancel: Boolean)
    begin
      if SameText(C.PlayerName, PlayerName) then
      begin
        Found := True;
        Cancel := True;
      end;
    end
  );

  Result := Found;
end;

procedure TDMServer.SClientLoginCheck(Sender: TObject; Socket: TDzSocket;
  var Accept: Boolean; const RequestData: string; var ResponseData: string);
var
  D: TMsgArray;
  C: TClient;
begin
  Accept := False;

  D := DataToArray(RequestData);

  {Version check must be always first because different versions may have
  reject tags changed, so it can result an exception in client.}

  //Check app version
  if D[0] <> STR_VERSION then
  begin
    ResponseData := 'VERSION_WRONG';
    Exit;
  end;

  //Check game password
  if D[3] <> pubPassword then
  begin
    ResponseData := 'PASSWORD_WRONG';
    Exit;
  end;

  if D[2]<>string.Empty then //Hash
  begin
    C := FindPlayerByHash(D[2]);

    if C=nil then
    begin
      ResponseData := 'HASH_WRONG';
      Exit;
    end;

    if C.Socket<>nil then
    begin
      ResponseData := 'ALREADY_CONNECTED';
      Exit;
    end;

    if not Assigned(FrmDrop) then
      raise Exception.Create('Internal: Missing players form not assigned');

    FrmDrop.DelPlayer(C);
  end else
  begin
    //Check if game is already running
    if Status<>ssPreparing then
    begin
      ResponseData := 'ALREADY_RUNNING';
      Exit;
    end;

    //Check if player name already exists
    if PlayerNameAlreadyExists(D[1]) then
    begin
      ResponseData := 'ALREADY_PLAYER_NAME';
      Exit;
    end;

    //LOGIN IS VALID!
    C := TClient.Create;
    C.PlayerName := D[1];
    PlayersList.Add(C);
  end;

  C.Socket := Socket;
  Socket.Data := C;

  //send back player name, bacause if hash connection, there is no name from client
  //and send hash so we can auto-set in start form for use when disconnected
  ResponseData := ArrayToData([C.PlayerName, C.Hash]);

  Accept := True;
end;

procedure TDMServer.SClientLoginSuccess(Sender: TObject; Socket: TDzSocket);
var
  C: TClient;
begin
  C := Socket.Data;
  S.SendAllEx(Socket, 'C', C.PlayerName); //send client connected to others

  SendPlayersList;
  SendRules(Socket); //send game rules to the player
end;

procedure TDMServer.SClientDisconnect(Sender: TObject; Socket: TDzSocket);
var
  C: TClient;
begin
  if not Socket.Auth then Exit;

  C := Socket.Data;
  S.SendAllEx(Socket, 'D', C.PlayerName); //send client disconnected to others

  C.Socket := nil;
  if Status<>ssPreparing then
  begin
    if Status<>ssOver then
    begin
      Status := ssFreezed;
      S.SendAll('?'); //send paused by connection drop signal
      DoPlayerDroped(C); //player disconnected during the game
      ClearAllAgreements; //if in agreement, clear because will back in turn status
    end;
  end else
    PlayersList.Remove(C);

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

procedure TDMServer.SendPlayersList(Exclude: TDzSocket = nil);
var
  CurPlayer: TClient;
  Lst: TStringList;
begin
  CurPlayer := GetCurrentPlayer;

  Lst := TStringList.Create;
  try
    ForConnections(
      procedure(Sok: TDzSocket; C: TClient; var Cancel: Boolean)
      begin
        Lst.Add(ArrayToData([C.PlayerName, C.Letters.Length, C.Score, C=CurPlayer, C.Agree, C.Socket=nil]));
      end
    );

    S.SendAllEx(Exclude, 'L', Lst.Text);
  finally
    Lst.Free;
  end;
end;

procedure TDMServer.StartGame;
begin
  Status := ssTurn;

  LoadDictionaryLetters;

  //--Get players letters and send to each one
  ForConnections(
    procedure(Sok: TDzSocket; C: TClient; var Cancel: Boolean)
    begin
      C.RandomizeInitialLetters;
      SendLetters(Sok);
    end
  );
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
  if CurrentPlayerIndex>PlayersList.Count-1 then
    CurrentPlayerIndex := 0;

  SendPlayersList; //update players list

  S.Send(GetCurrentPlayer.Socket, '>'); //send to current player its turn signal
end;

function TDMServer.GetCurrentPlayer: TClient;
begin
  if CurrentPlayerIndex = -1 then Exit(nil);

  Result := PlayersList[CurrentPlayerIndex];
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
  if Status<>ssTurn then
    raise Exception.Create('Internal: A player tried to define a letter when status is not turn');

  if Socket<>GetCurrentPlayer.Socket then
    raise Exception.Create('Internal: A player tried to define a letter when is not its turn');

  D := DataToArray(A);

  X := D[0];
  Y := D[1];

  B := Matrix[Y, X];
  if not ( (B.Letter=BLANK_LETTER) or (B.Temp) ) then
    raise Exception.Create('Internal: A player tried to define a letter in an unallowed condition');

  Letter := VarToStr(D[2])[1];

  Matrix[Y, X].&Set(Letter, True);

  SendMatrix; //send updated matrix to all
end;

procedure TDMServer.PlayerTurnDoneReceived(Socket: TDzSocket);
begin
  if Status<>ssTurn then
    raise Exception.Create('Internal: A player tried to set its turn done when status is not turn');

  if Socket<>GetCurrentPlayer.Socket then
    raise Exception.Create('Internal: A player tried to set its turn done when is not its turn');

  if IsThereLettersUsed then
  begin
    Status := ssAgreement;
    S.Send(Socket, 'W'); //send wait log
    S.SendAllEx(Socket, 'G'); //send agreement request signal
  end else
  begin
    RebuyLetters(Socket);
    SelectNextPlayer;
  end;
end;

procedure TDMServer.AgreementReceived(Socket: TDzSocket; const A: string);
var
  C: TClient;
  CurSok: TDzSocket;

  procedure DisableAgreement;
  begin
     Status := ssTurn;
     S.SendAllEx(CurSok, 'K');

     ClearAllAgreements;
  end;

begin
  if Status<>ssAgreement then
    raise Exception.Create('Internal: A player tried to set agreement when status is not agreeement');

  C := Socket.Data;
  CurSok := GetCurrentPlayer.Socket;

  if DataToArray(A)[0]{Agree} then
  begin
    C.Agree := True;

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

function TDMServer.IsAllPlayersAgree(WithSocket: TDzSocket): Boolean;
var
  SomeDisagree: Boolean;
begin
  SomeDisagree := False;

  ForConnections(
    procedure(Sok: TDzSocket; C: TClient; var Cancel: Boolean)
    begin
      if Sok<>WithSocket then
        if not C.Agree then
        begin
          SomeDisagree := True;
          Cancel := True;
        end;
    end
  );

  Result := not SomeDisagree;
end;

procedure TDMServer.ClearAllAgreements;
begin
  ForConnections(
    procedure(Sok: TDzSocket; C: TClient; var Cancel: Boolean)
    begin
      C.Agree := False;
    end
  );
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
    for X := 0 to High(Matrix[Y]) do
    begin
      B := Matrix[Y, X];
      if B.Temp then
      begin
        //remove used letter from player letters list
        RemLetters := StoLetters.Replace(B.Letter, string.Empty, [{avoid replace all}]);
        if RemLetters = StoLetters then
          raise Exception.Create('Internal: A player tried to use an inexistent letter');

        StoLetters := RemLetters;
        Matrix[Y, X].ClearTemp;
      end;
    end;

  Inc(C.Score, Length(C.Letters) - Length(StoLetters));
  C.Letters := StoLetters;

  SendMatrix;
  S.Send(Socket, 'F'); //finish turn log

  if C.Letters.IsEmpty then
    SetGameOver
  else
    SelectNextPlayer;
end;

procedure TDMServer.SetGameOver;
begin
  Status := ssOver;
  CurrentPlayerIndex := -1;
  SendPlayersList;
  S.SendAll('E'); //send end game signal
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
    LST_DICTIONARY[GetCurrentDictionaryIndex].LanguageName,
    pubServerProps.SizeW,
    pubServerProps.SizeH,
    pubServerProps.InitialLetters,
    pubServerProps.RebuyLetters]);

  if Socket<>nil then
    S.Send(Socket, 'u', A)
  else
    S.SendAll('U', A);
end;

procedure TDMServer.RestartGame;
begin
  //If players left when game is over, object remains in list to show score,
  //so when restarting game, this objects must be removed.
  //The same situation when server stops the game directly from drop form.
  RemoveAllDisconectedPlayers;
  //

  CurrentPlayerIndex := -1; //when restarting directly from drop form
  Status := ssPreparing;

  SetLength(Matrix, 0);
  SendMatrix;

  //--Reset players game data and send letters to each one
  ForConnections(
    procedure(Sok: TDzSocket; C: TClient; var Cancel: Boolean)
    begin
      C.ResetGameData;
      SendLetters(Sok);
    end
  );
  //--
  SendPlayersList;

  S.SendAll('P'); //send preparing new game to all
end;

procedure TDMServer.KillPlayer(C: TClient);
var
  I: Integer;
begin
  I := PlayersList.IndexOf(C);
  if I=-1 then
    raise Exception.Create('Internal: Player object not found');

  PlayersList.Delete(I);

  if CurrentPlayerIndex>I then Dec(CurrentPlayerIndex);
  if CurrentPlayerIndex>PlayersList.Count-1 then CurrentPlayerIndex := 0;
  SendPlayersList;
end;

procedure TDMServer.ContinueGame;
begin
  S.SendAll('/'); //send signal to inform that game will continue

  Status := ssTurn;

  Dec(CurrentPlayerIndex); //just because will inc on select next player
  SelectNextPlayer;
end;

end.
