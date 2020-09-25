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
  end;

var
  DMClient: TDMClient;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses UVars, UFrmGame, UFrmLog, UFrmStart, UFrmMain, System.SysUtils, UDMServer;

procedure TDMClient.DataModuleCreate(Sender: TObject);
begin
  C.Port := INT_TCP_PORT;
end;

procedure TDMClient.CConnect(Sender: TObject; Socket: TDzSocket);
begin
  Log('Connected.');
end;

procedure TDMClient.CDisconnect(Sender: TObject; Socket: TDzSocket;
  const WasConnected: Boolean);
begin
  Log('Disconnected.');

  FrmGame.Hide;

  if pubModeServer then DMServer.S.Close; //turn off server

  FrmStart.Show;
  FrmStart.EnableControls(True);
end;

procedure TDMClient.CConnectionLost(Sender: TObject; Socket: TDzSocket);
begin
  Log('Connection lost.');
end;

procedure TDMClient.CError(Sender: TObject; Socket: TDzSocket;
  const Event: TErrorEvent; const ErrorCode: Integer; const ErrorMsg: string);
begin
  Log('ERROR: '+ErrorMsg);
end;

procedure TDMClient.CLoginRequest(Sender: TObject; Socket: TDzSocket;
  var Data: string);
begin
  Data := ArrayToData([STR_VERSION, pubPlayerName, pubPassword]);
end;

procedure TDMClient.CLoginResponse(Sender: TObject; Socket: TDzSocket;
  Accepted: Boolean; const Data: string);
begin
  if Accepted then
  begin
    Log('Login accepted.');

    FrmStart.Hide;

    FrmGame.Initialize;
    FrmGame.Show;

    if pubModeServer then
      Log('Please, wait for all players to connect and then click "Start Game".')
    else
      Log('Please, wait while server is starting the game. In the meantime you can use chat.');
  end else
  begin
    Log('Login rejected: '+Data);
  end;
end;

procedure TDMClient.CRead(Sender: TObject; Socket: TDzSocket; const Cmd: Char;
  const A: string);
begin
  case Cmd of
    'C': Log(Format('Player %s just joined.', [A]));
    'D': Log(Format('Player %s left.', [A]));
    'U', 'u': RulesReceived(A, Cmd='u');
    'M': MessageReceived(A);
    'L': PlayersListReceived(A);
    'R': FrmGame.GameStartedReceived;
    'T': LettersReceived(A);
    'X': FrmGame.MatrixReceived(A);
    '>': FrmGame.InitMyTurn;
    'G': FrmGame.AgreementRequestReceived;
    'J': FrmGame.DisagreeReceived;
    'K': FrmGame.AgreementFinishReceived;
    'W': begin
           Log('Please, wait while your opponents validate your words.');
           DoSound('WAIT');
         end;
    'B': begin
           Log(Format('You bought %s more letter(s) since you didn''t fill anything in this round.', [A]));
           DoSound('BUY');
         end;
    'F': begin
           Log('Your words were accepted and your turn to play is over.');
           DoSound('DONE');
         end;
    'E': FrmGame.GameOverReceived;
    'P': FrmGame.ReceivedPreparingNewGame;
  end;
end;

procedure TDMClient.RulesReceived(const A: string; ToOne: Boolean);
var D: TMsgArray;
begin
  D := DataToArray(A);

  FrmGame.PB.SetMatrixSize(D[1], D[2]);

  FrmMain.LbRules.Caption :=
    Format('Dictionary: %s / Size: %s x %s / Init. Letters: %s / Rebuy: %s', [
    D[0], D[1], D[2], D[3], D[4]]);

  if not ToOne then
    Log('The game rules have been changed by the server.')
end;

procedure TDMClient.MessageReceived(const A: string);
var D: TMsgArray;
begin
  D := DataToArray(A);
  FrmGame.ChatLog(D[0], D[1]);

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

end.
