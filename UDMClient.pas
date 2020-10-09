unit UDMClient;

interface

uses System.Classes, DzSocket;

type
  TDMClient = class(TDataModule)
    C: TDzTCPClient;
    procedure DataModuleCreate(Sender: TObject);
    procedure CLoginRequest(Sender: TObject; Socket: TDzSocket;
      var Data: string);
    procedure CLoginResponse(Sender: TObject; Socket: TDzSocket;
      Accepted: Boolean; const Data: string);
    procedure CConnect(Sender: TObject; Socket: TDzSocket);
    procedure CConnectionLost(Sender: TObject; Socket: TDzSocket);
    procedure CDisconnect(Sender: TObject; Socket: TDzSocket;
      const WasConnected: Boolean);
    procedure CError(Sender: TObject; Socket: TDzSocket;
      const Event: TErrorEvent; const ErrorCode: Integer;
      const ErrorMsg: string);
    procedure CRead(Sender: TObject; Socket: TDzSocket; const Cmd: Char;
      const A: string);
  private
    procedure MessageReceived(const A: string);
    procedure PlayersListReceived(const A: string);
    procedure LettersReceived(const A: string);
    procedure RulesReceived(const A: string; ToOne: Boolean);
  public
    procedure SendLetter(X, Y: Integer; const Letter: Char);
    procedure SendAgreement(Agree: Boolean);
    procedure SendContest(Accept: Boolean);
  end;

var
  DMClient: TDMClient;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses UVars, ULanguage, UDMServer,
  UFrmMain, UFrmStart, UFrmGame, UFrmLog,
  System.SysUtils, System.StrUtils;

procedure TDMClient.DataModuleCreate(Sender: TObject);
begin
  C.Port := INT_TCP_PORT;
end;

procedure TDMClient.CConnect(Sender: TObject; Socket: TDzSocket);
begin
  Log(Lang.Get('LOG_CONNECTED'));
end;

procedure TDMClient.CDisconnect(Sender: TObject; Socket: TDzSocket;
  const WasConnected: Boolean);
begin
  Log(Lang.Get('LOG_DISCONNECTED'));

  if pubModeServer then DMServer.S.Close; //turn off server

  FrmGame.Timer.Enabled := False;
  FrmGame.Hide;

  FrmMain.BoxConInfo.Visible := False;

  FrmStart.Show;
  FrmStart.EnableControls(True);
end;

procedure TDMClient.CConnectionLost(Sender: TObject; Socket: TDzSocket);
begin
  Log(Lang.Get('LOG_CONNECTION_LOST'));
end;

procedure TDMClient.CError(Sender: TObject; Socket: TDzSocket;
  const Event: TErrorEvent; const ErrorCode: Integer; const ErrorMsg: string);
begin
  Log(Format(Lang.Get('LOG_SOCKET_ERROR'), [ErrorMsg]));
end;

procedure TDMClient.CLoginRequest(Sender: TObject; Socket: TDzSocket;
  var Data: string);
begin
  Data := ArrayToData([STR_VERSION, pubPlayerName, pubPlayerHash, pubPassword]);
end;

procedure TDMClient.CLoginResponse(Sender: TObject; Socket: TDzSocket;
  Accepted: Boolean; const Data: string);
var
  D: TMsgArray;
begin
  if Accepted then
  begin
    Log(Lang.Get('LOG_LOGIN_ACCEPTED'));

    FrmStart.Hide;

    D := DataToArray(Data);
    pubPlayerName := D[0];
    FrmStart.EdHash.Text := D[1]; //auto-set for use when disconnect

    FrmMain.LbMode.Caption := Lang.Get(IfThen(pubModeServer, 'MODE_SERVER', 'MODE_CLIENT'));
    FrmMain.LbPlayer.Caption := pubPlayerName;
    FrmMain.LbRules.Caption := string.Empty;
    FrmMain.BoxConInfo.Visible := True;

    FrmGame.Initialize(FrmStart.BoxReconnect.Visible);
    FrmGame.Show;

    if pubModeServer then
      Log(Lang.Get('LOG_PREPARING_GAME_SERVER'))
    else
      if FrmStart.BoxReconnect.Visible then
        Log(Lang.Get('LOG_RECONECTED_GAME_CLIENT'))
      else
        Log(Lang.Get('LOG_PREPARING_GAME_CLIENT'));
  end else
  begin
    Log(Format(Lang.Get('LOG_LOGIN_REJECTED'), [Lang.Get('CONN_REJECT_'+Data)]));
  end;
end;

procedure TDMClient.CRead(Sender: TObject; Socket: TDzSocket; const Cmd: Char;
  const A: string);
begin
  case Cmd of
    'C': Log(Format(Lang.Get('LOG_PLAYER_JOIN'), [A]));
    'D': Log(Format(Lang.Get('LOG_PLAYER_LEFT'), [A]));
    'U', 'u': RulesReceived(A, Cmd='u');
    'M': MessageReceived(A);
    'L': PlayersListReceived(A);
    'T': LettersReceived(A);
    'R': FrmGame.GameStartedReceived;
    'X': FrmGame.MatrixReceived(A);
    '>': FrmGame.InitMyTurn;
    '~': FrmGame.MyTurnTimeoutReceived;
    'G': FrmGame.AgreementRequestReceived;
    'K': FrmGame.AgreementFinishReceived(A);
    'W': FrmGame.WaitValidationReceived;
    'F': FrmGame.ValidationAcceptedReceived;
    'J': FrmGame.ValidationRejectedReceived;
    'O': FrmGame.OpenContestPeriodReceived;
    'B': FrmGame.LettersExchangedReceived;
    'E': FrmGame.GameOverReceived;
    'P': FrmGame.ReceivedPreparingNewGame;
    '?': FrmGame.ReceivedPausedByDrop;
    '/': FrmGame.ReceivedDropContinue;
    ':': FrmGame.ReceivedTimerStart(A);
    '.': FrmGame.ReceivedTimerStop;
    '&': FrmGame.ReceivedAutoRejectedByInvalidLetters;
    'Q': FrmGame.ReceivedContestResponse(A);
  end;
end;

procedure TDMClient.RulesReceived(const A: string; ToOne: Boolean);
var
  D: TMsgArray;
  Dictionary: string;
  SizeW, SizeH, HandLetters, GoalScore, TurnTimeout, AgreementTimeout: Integer;
begin
  D := DataToArray(A);

  Dictionary := D[0];
  SizeW := D[1];
  SizeH := D[2];
  HandLetters := D[3];
  GoalScore := D[4];
  TurnTimeout := D[5];
  AgreementTimeout := D[6];

  FrmMain.LbRules.Caption :=
    Format(Lang.Get('TITLE_RULES_DEFINITION'), [
      Dictionary, SizeW, SizeH, HandLetters, GoalScore, TurnTimeout, AgreementTimeout]);

  FrmGame.LbPosition.Caption := string.Empty;
  FrmGame.PB.SetMatrixSize(SizeH, SizeW);

  if not ToOne then
    Log(Lang.Get('LOG_RULES_CHANGED'));
  //When an user connects, it receive the game rules, so the log should be omitted.
end;

procedure TDMClient.MessageReceived(const A: string);
var
  D: TMsgArray;
begin
  D := DataToArray(A);
  FrmGame.ChatLog(D[0], D[1], True);

  DoSound('PLING');
end;

procedure TDMClient.PlayersListReceived(const A: string);
begin
  FrmGame.LPlayers.Items.Text := A;
end;

procedure TDMClient.LettersReceived(const A: string);
var
  Lst: TStringList;
  Letter: Char;
begin
  Lst := TStringList.Create;
  try
    for Letter in A do
      Lst.Add(Letter);

    FrmGame.LLetters.Items.Assign(Lst);
  finally
    Lst.Free;
  end;
end;

procedure TDMClient.SendLetter(X, Y: Integer; const Letter: Char);
begin
  C.Send('#', ArrayToData([X, Y, Letter]));
end;

procedure TDMClient.SendAgreement(Agree: Boolean);
begin
  C.Send('H', ArrayToData([Agree]));
end;

procedure TDMClient.SendContest(Accept: Boolean);
begin
  C.Send('Y', ArrayToData([Accept]));
end;

end.
