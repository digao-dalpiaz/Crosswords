unit UDMServer;

interface

uses
  System.Classes, Vcl.ExtCtrls, DzSocket,
  //
  UMatrix, UClient;

type
  TForConnectionsProc = reference to procedure(Sok: TDzSocket; C: TClient; var Cancel: Boolean);

  TServerStatus = (ssPreparing, ssTurn, ssAgreement, ssContest, ssFreezed, ssOver);

  TDMServer = class(TDataModule)
    S: TDzTCPServer;
    Timer: TTimer;
    procedure DataModuleCreate(Sender: TObject);
    procedure SClientLoginCheck(Sender: TObject; Socket: TDzSocket;
      var Accept: Boolean; const RequestData: string; var ResponseData: string);
    procedure SClientLoginSuccess(Sender: TObject; Socket: TDzSocket);
    procedure SClientDisconnect(Sender: TObject; Socket: TDzSocket);
    procedure SClientRead(Sender: TObject; Socket: TDzSocket; const Cmd: Char;
      const A: string);
    procedure DataModuleDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    PlayersList: TPlayersList;
    CurPlayer: TClient;
    _CurrentPlayerIndex: Integer;

    Matrix: TMatrixData;
    CenterBlock: TBlock;

    Status: TServerStatus;
    TimerSeconds: Integer;

    procedure SetCurrentPlayerIndex(const Value: Integer);
    property CurrentPlayerIndex: Integer read _CurrentPlayerIndex write SetCurrentPlayerIndex;

    function PlayerNameAlreadyExists(const PlayerName: string): Boolean;
    procedure SendPlayersList;
    procedure SendLetters(Socket: TDzSocket);
    procedure SelectNextPlayer(KeepCurrent: Boolean = False);
    procedure SendMatrix(Socket: TDzSocket = nil);
    procedure LetterReceived(Socket: TDzSocket; const A: string);
    procedure MessageReceived(Socket: TDzSocket; const A: string);
    procedure PlayerTurnDoneReceived(Socket: TDzSocket; ByTimeout: Boolean);
    procedure AgreementReceived(Socket: TDzSocket; const A: string);
    procedure ClearAllAgreements;
    function IsAllPlayersAgree: Boolean;
    procedure CompletePlayerTurn;
    procedure ForConnections(P: TForConnectionsProc);
    procedure RemoveAllDisconectedPlayers;
    procedure ContestReceived(Socket: TDzSocket; const A: string);
    procedure StartAgreementPeriod;
    procedure RemoveTempPlayerLetters;
    procedure StopTimer;
    procedure SetGameOver;
    procedure StartTimer(Seconds: Integer);
    procedure StopAgreementPeriod(Accepted: Boolean);
    procedure AgreementAllAccepted;
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

uses UVars, UDictionary, UFrmRules, UFrmDrop,
  System.SysUtils, System.StrUtils, System.Variants, System.Math;

procedure TDMServer.DataModuleCreate(Sender: TObject);
begin
  S.Port := INT_TCP_PORT;
  S.EnumeratorOnlyAuth := True;

  Matrix := TMatrixData.Create;
  PlayersList := TPlayersList.Create;
end;

procedure TDMServer.DataModuleDestroy(Sender: TObject);
begin
  Matrix.Free;
  PlayersList.Free;
end;

procedure TDMServer.Initialize;
begin
  Matrix.Clear;
  PlayersList.Clear;
  CurrentPlayerIndex := -1;
  CenterBlock := nil;
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
    if Status<>ssFreezed then
    begin
      ResponseData := 'NOT_FREEZED';
      Exit;
    end;

    //FIND PLAYER BY HASH
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

  SendPlayersList; //update players list to all

  SendRules(Socket); //send game rules to the player
  SendMatrix(Socket); //send game rules to the player (may be reconnected)
  SendLetters(Socket); //send letters to the player (may be reconnected)
end;

procedure TDMServer.SClientDisconnect(Sender: TObject; Socket: TDzSocket);
var
  C: TClient;
begin
  if not Socket.Auth then Exit;

  C := Socket.Data;
  S.SendAll('D', C.PlayerName); //send client disconnected to others

  C.Socket := nil;
  if Status<>ssPreparing then
  begin
    if Status<>ssOver then
    begin
      //this will enter every player that disconnects
      Status := ssFreezed;
      StopTimer;

      S.SendAll('?'); //send paused by connection drop signal
      DoPlayerDroped(C); //player disconnected during the game
      ClearAllAgreements; //if in agreement, clear because will back in turn status
      RemoveTempPlayerLetters;
    end;
  end else
    PlayersList.Remove(C);

  SendPlayersList;
end;

procedure TDMServer.SClientRead(Sender: TObject; Socket: TDzSocket;
  const Cmd: Char; const A: string);
begin
  case Cmd of
    'M': MessageReceived(Socket, A);
    '#': LetterReceived(Socket, A);
    '!': PlayerTurnDoneReceived(Socket, False);
    'H': AgreementReceived(Socket, A);
    'Y': ContestReceived(Socket, A);
  end;
end;

procedure TDMServer.MessageReceived(Socket: TDzSocket; const A: string);
var
  C: TClient;
begin
  C := Socket.Data;
  S.SendAllEx(Socket, 'M', ArrayToData([C.PlayerName, A]));
end;

procedure TDMServer.SendPlayersList;
var
  Lst: TStringList;
begin
  Lst := TStringList.Create;
  try
    ForConnections(
      procedure(Sok: TDzSocket; C: TClient; var Cancel: Boolean)
      begin
        Lst.Add(ArrayToData([C.PlayerName, C.Score, C=CurPlayer, C.Agree, C.Socket=nil]));
      end
    );

    S.SendAll('L', Lst.Text);
  finally
    Lst.Free;
  end;
end;

procedure TDMServer.StartGame;
begin
  Status := ssTurn;
  PlayersList.RandomList; //random players order

  LoadDictionaryLetters;

  //--Get players letters and send to each one
  ForConnections(
    procedure(Sok: TDzSocket; C: TClient; var Cancel: Boolean)
    begin
      C.RandomizeLetters(True);
      SendLetters(Sok);
    end
  );
  //--

  Matrix.Init(pubServerProps.SizeH, pubServerProps.SizeW);
  CenterBlock := Matrix[pubServerProps.SizeH div 2][pubServerProps.SizeW div 2];
  CenterBlock.&Set(GetRandomLetter, False);
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

procedure TDMServer.SelectNextPlayer(KeepCurrent: Boolean = False);
begin
  if not KeepCurrent then
    CurrentPlayerIndex := CurrentPlayerIndex+1;

  SendPlayersList; //update players list

  S.Send(CurPlayer.Socket, '>'); //send to current player its turn signal

  if pubServerProps.TurnTimeout then
    StartTimer(pubServerProps.TurnTimeoutSecs);
end;

procedure TDMServer.SetCurrentPlayerIndex(const Value: Integer);
var
  Index: Integer;
begin
  Index := Value;
  if Index>PlayersList.Count-1 then Index := 0;
  _CurrentPlayerIndex := Index;

  if Index<>-1 then
    CurPlayer := PlayersList[Index]
  else
    CurPlayer := nil;
end;

procedure TDMServer.StartTimer(Seconds: Integer);
begin
  TimerSeconds := Seconds;
  Timer.Enabled := True;
  S.SendAll(':', Seconds.ToString); //send timer start signal
end;

procedure TDMServer.StopTimer;
begin
  if Timer.Enabled then
  begin
    Timer.Enabled := False;
    S.SendAll('.'); //send stop timer signal
  end;
end;

procedure TDMServer.SendMatrix(Socket: TDzSocket = nil);
var
  A: string;
begin
  A := Matrix.SaveToString;

  if Socket<>nil then
    S.Send(Socket, 'X', A)
  else
    S.SendAll('X', A);
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

  if Socket<>CurPlayer.Socket then
    raise Exception.Create('Internal: A player tried to define a letter when is not its turn');

  D := DataToArray(A);

  X := D[0];
  Y := D[1];

  B := Matrix[Y][X];
  if not ( (B.Letter=BLANK_LETTER) or (B.Temp) ) then
    raise Exception.Create('Internal: A player tried to define a letter in an unallowed condition');

  Letter := VarToStr(D[2])[1];

  B.&Set(Letter, True);
  Matrix.ValidateSequence(CenterBlock);

  SendMatrix; //send updated matrix to all
end;

procedure TDMServer.PlayerTurnDoneReceived(Socket: TDzSocket; ByTimeout: Boolean);
begin
  if Status<>ssTurn then
    raise Exception.Create('Internal: A player tried to set its turn done when status is not turn');

  if Socket<>CurPlayer.Socket then
    raise Exception.Create('Internal: A player tried to set its turn done when is not its turn');

  StopTimer;

  if Matrix.ContainsAnyTemp then //player put letters in the grid
  begin
    if Matrix.ContainsAnyInvalid then
    begin
      if ByTimeout then
      begin
        RemoveTempPlayerLetters;
        S.Send(CurPlayer.Socket, '&'); //send auto rejected by invalid letters signal
        SelectNextPlayer;
        Exit;
      end else
        raise Exception.Create('Internal: A player tried to set its turn done having letters out of sequence');
    end;

    StartAgreementPeriod;
  end else
  begin
    CurPlayer.RandomizeLetters(True);
    SendLetters(CurPlayer.Socket);
    S.Send(CurPlayer.Socket, 'B'); //send letters exchanged signal

    SelectNextPlayer;
  end;
end;

procedure TDMServer.StartAgreementPeriod;
begin
  Status := ssAgreement;
  S.Send(CurPlayer.Socket, 'W'); //send wait log
  S.SendAllEx(CurPlayer.Socket, 'G'); //send agreement request signal

  if pubServerProps.TurnTimeout then
    StartTimer(pubServerProps.AgreementTimeoutSecs);
end;

procedure TDMServer.StopAgreementPeriod(Accepted: Boolean);
begin
  StopTimer;
  S.SendAllEx(CurPlayer.Socket, 'K', ArrayToData([Accepted]));
  ClearAllAgreements;
end;

procedure TDMServer.AgreementReceived(Socket: TDzSocket; const A: string);
var
  C: TClient;
begin
  if Status<>ssAgreement then
    raise Exception.Create('Internal: A player tried to set agreement when status is not agreeement');

  C := Socket.Data;

  if DataToArray(A)[0]{Agree} then
  begin
    C.Agree := True;

    if IsAllPlayersAgree then //Check if all players have set agreement
    begin
      AgreementAllAccepted; //SendPlayersList will be called here
      Exit;
    end;
  end else
  begin
    if pubServerProps.TurnTimeout then
    begin
      Status := ssContest;
      S.Send(CurPlayer.Socket, 'O'); //send open contest period to current player
    end else
    begin
      Status := ssTurn;
      S.Send(CurPlayer.Socket, 'J'); //send reject agreement to current player
    end;

    StopAgreementPeriod(False);
  end;

  SendPlayersList; //update players list
end;

procedure TDMServer.AgreementAllAccepted;
begin
  Status := ssTurn;
  StopAgreementPeriod(True);
  CompletePlayerTurn;
end;

procedure TDMServer.ContestReceived(Socket: TDzSocket; const A: string);
var
  Accept: Boolean;
begin
  if Status<>ssContest then
    raise Exception.Create('Internal: A player tried to set contest when status is not contest');

  if Socket<>CurPlayer.Socket then
    raise Exception.Create('Internal: A player tried to set contest when is not its turn');

  Accept := DataToArray(A)[0]{Accept};

  S.SendAllEx(CurPlayer.Socket, 'Q', ArrayToData([Accept]));

  if Accept then
  begin
    RemoveTempPlayerLetters;

    Status := ssTurn;
    SelectNextPlayer;
  end else
  begin
    //request a new agreement period
    StartAgreementPeriod;
  end;
end;

procedure TDMServer.RemoveTempPlayerLetters;
begin
  Matrix.RemoveAllTempLetters;
  SendMatrix;

  if CurPlayer.Socket<>nil then //when a player disconnects, its socket is nil
    SendLetters(CurPlayer.Socket); //return letters to player hand
end;

function TDMServer.IsAllPlayersAgree: Boolean;
var
  SomeDisagree: Boolean;
begin
  SomeDisagree := False;

  ForConnections(
    procedure(Sok: TDzSocket; C: TClient; var Cancel: Boolean)
    begin
      if (not C.Agree) and (C<>CurPlayer) then
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

procedure TDMServer.CompletePlayerTurn;
var
  C: TClient;
  RemLetters, StoLetters: string;
  Row: TMatrixDataRow;
  B: TBlock;
begin
  C := CurPlayer;

  StoLetters := C.Letters;

  for Row in Matrix do
    for B in Row do
      if B.Temp then
      begin
        //remove used letter from player letters list
        RemLetters := StoLetters.Replace(B.Letter, string.Empty, [{avoid replace all}]);
        if RemLetters = StoLetters then
          raise Exception.Create('Internal: A player tried to use an inexistent letter');

        StoLetters := RemLetters;
        B.ClearTemp;
      end;

  Inc(C.Score, Length(C.Letters) - Length(StoLetters));
  C.Letters := StoLetters;

  C.RandomizeLetters(False);
  SendLetters(C.Socket);

  SendMatrix;
  S.Send(C.Socket, 'F'); //finish turn log

  if C.Score >= pubServerProps.GoalScore then
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

procedure TDMServer.SendRules(Socket: TDzSocket);
var
  A: string;
begin
  A := ArrayToData([
    LST_DICTIONARY[GetCurrentDictionaryIndex].LanguageName,
    pubServerProps.SizeW,
    pubServerProps.SizeH,
    pubServerProps.HandLetters,
    pubServerProps.GoalScore,
    IfThen(pubServerProps.TurnTimeout, pubServerProps.TurnTimeoutSecs),
    IfThen(pubServerProps.TurnTimeout, pubServerProps.AgreementTimeoutSecs)]);

  if Socket<>nil then
    S.Send(Socket, 'u', A)
  else
    S.SendAll('U', A);
end;

procedure TDMServer.RestartGame;
begin
  StopTimer; //if restarting during the game

  //If players left when game is over, object remains in list to show score,
  //so when restarting game, this objects must be removed.
  //The same situation when server stops the game directly from drop form.
  RemoveAllDisconectedPlayers;
  //

  CurrentPlayerIndex := -1; //when restarting directly from drop form
  Status := ssPreparing;

  Matrix.Clear;
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
  I, PlayerIndex: Integer;
begin
  I := PlayersList.IndexOf(C);
  if I=-1 then
    raise Exception.Create('Internal: Player object not found');

  PlayersList.Delete(I);

  PlayerIndex := CurrentPlayerIndex;
  if PlayerIndex>I then Dec(PlayerIndex);
  CurrentPlayerIndex := PlayerIndex;

  SendPlayersList;
end;

procedure TDMServer.ContinueGame;
begin
  S.SendAll('/'); //send signal to inform that game will continue

  Status := ssTurn;
  SelectNextPlayer(True);
end;

procedure TDMServer.TimerTimer(Sender: TObject);
begin
  Dec(TimerSeconds);
  if TimerSeconds<0 then raise Exception.Create('Internal: Negative timer');

  if TimerSeconds=0 then
  begin
    case Status of
      ssTurn:
      begin
        S.Send(CurPlayer.Socket, '~'); //send player time out signal
        PlayerTurnDoneReceived(CurPlayer.Socket, True);
      end;

      ssAgreement:
      begin
        AgreementAllAccepted;
      end;

      else raise Exception.Create('Internal: Timeout in incorrect status');
    end;
  end;
end;

end.
