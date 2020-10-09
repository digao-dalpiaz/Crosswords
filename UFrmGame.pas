unit UFrmGame;

interface

uses Vcl.Forms, System.ImageList, Vcl.ImgList, Vcl.Controls, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.ComCtrls, Vcl.ExtCtrls, System.Classes,
  //
  System.Types, URichEditUnicode, UMatrix;

type
  TGameStatus = (
    gsUnknown, gsPreparing,
    gsPlaying, gsMyTurn, gsWaitValid, gsAgreement, gsContest,
    gsPaused, gsGameOver);

  TFrmGame = class(TForm)
    BoxSide: TPanel;
    BoxChat: TPanel;
    EdChatLog: TRichEdit;
    EdChatMsg: TEdit;
    LPlayers: TListBox;
    LLetters: TListBox;
    BoxOperations: TPanel;
    BtnStartGame: TBitBtn;
    LbLetters: TLabel;
    LbChat: TLabel;
    IL: TImageList;
    BtnDone: TBitBtn;
    BtnAgree: TBitBtn;
    BtnDisagree: TBitBtn;
    BtnRules: TBitBtn;
    BtnRestart: TBitBtn;
    BoxGrid: TPanel;
    SB: TScrollBox;
    LbPosition: TLabel;
    BoxStatus: TPanel;
    BoxHeaderPlayers: TPanel;
    LbHeaderPlayer: TLabel;
    LbHeaderScore: TLabel;
    BtnContestAccept: TBitBtn;
    BtnContestReject: TBitBtn;
    Timer: TTimer;
    LbTimer: TLabel;
    procedure EdChatMsgKeyPress(Sender: TObject; var Key: Char);
    procedure BtnStartGameClick(Sender: TObject);
    procedure LPlayersDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormCreate(Sender: TObject);
    procedure LLettersDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure BtnDoneClick(Sender: TObject);
    procedure BtnAgreeClick(Sender: TObject);
    procedure BtnDisagreeClick(Sender: TObject);
    procedure BtnRulesClick(Sender: TObject);
    procedure BtnRestartClick(Sender: TObject);
    procedure BtnContestAcceptClick(Sender: TObject);
    procedure BtnContestRejectClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  public
    Status: TGameStatus;
    PB: TMatrixImage;

    procedure Initialize(Reconnected: Boolean);
    procedure MatrixReceived(const A: string);
    procedure ChatLog(const Player, Text: string; Other: Boolean);
    procedure GameStartedReceived;
    procedure InitMyTurn;
    procedure MyTurnTimeoutReceived;
    procedure WaitValidationReceived;
    procedure ValidationAcceptedReceived;
    procedure ValidationRejectedReceived;
    procedure AgreementRequestReceived;
    procedure AgreementFinishReceived(const A: string);
    procedure OpenContestPeriodReceived;
    procedure ReceivedPausedByDrop;
    procedure ReceivedDropContinue;
    procedure GameOverReceived;
    procedure ReceivedPreparingNewGame;
    procedure ReceivedTimerStart(const A: string);
    procedure ReceivedTimerStop;
    procedure ReceivedAutoRejectedByInvalidLetters;
    procedure ReceivedContestResponse(const A: string);
    procedure LettersExchangedReceived;
  private
    TimerSeconds: Integer;

    procedure InitTranslation;
    procedure SetStatus(NewStatus: TGameStatus);
  end;

var
  FrmGame: TFrmGame;

implementation

{$R *.dfm}

uses System.SysUtils,
  Vcl.Graphics, Winapi.Windows, Winapi.Messages,
  UVars, UDams, ULanguage,
  DzSocket, UDMClient, UDMServer,
  UFrmLog, UFrmMain, UFrmRules;

procedure TFrmGame.FormCreate(Sender: TObject);
begin
  PB := TMatrixImage.Create(Self);
  PB.Parent := SB;
end;

procedure TFrmGame.InitTranslation;
begin
  LbHeaderPlayer.Caption := Lang.Get('GAME_HEADER_PLAYER');
  LbHeaderScore.Caption := Lang.Get('GAME_HEADER_SCORE');

  LbLetters.Caption := Lang.Get('GAME_BOX_LETTERS');
  LbChat.Caption := Lang.Get('GAME_BOX_CHAT');

  BtnRestart.Caption := Lang.Get('GAME_BTN_RESTART');
  BtnStartGame.Caption := Lang.Get('GAME_BTN_START');
  BtnRules.Caption := Lang.Get('GAME_BTN_RULES');
  BtnDone.Caption := Lang.Get('GAME_BTN_DONE');
  BtnAgree.Caption := Lang.Get('GAME_BTN_AGREE');
  BtnDisagree.Caption := Lang.Get('GAME_BTN_DISAGREE');
  BtnContestAccept.Caption := Lang.Get('GAME_BTN_CONTEST_ACCEPT');
  BtnContestReject.Caption := Lang.Get('GAME_BTN_CONTEST_REJECT');
end;

procedure TFrmGame.Initialize(Reconnected: Boolean);
begin
  InitTranslation;

  if Reconnected then
    SetStatus(gsPaused)
  else
    SetStatus(gsPreparing);

  PB.SetMatrixSize(0, 0);
  LPlayers.Clear;
  LLetters.Clear;

  LbPosition.Caption := string.Empty;
  LbTimer.Caption := string.Empty;
end;

procedure TFrmGame.SetStatus(NewStatus: TGameStatus);

 procedure SetStatusLabel(const LangIdentSufix: string; Color: TColor);
 begin
   BoxStatus.Caption := Lang.Get('GAME_STATUS_'+LangIdentSufix);
   BoxStatus.Color := Color;
 end;

begin
  Status := NewStatus;

  BtnStartGame.Visible := (Status = gsPreparing) and pubModeServer;
  BtnRules.Visible := (Status = gsPreparing) and pubModeServer;

  BtnDone.Visible := (Status = gsMyTurn);

  BtnAgree.Visible := (Status = gsAgreement);
  BtnDisagree.Visible := (Status = gsAgreement);

  BtnContestAccept.Visible := (Status = gsContest);
  BtnContestReject.Visible := (Status = gsContest);

  BtnRestart.Visible := (Status = gsGameOver) and pubModeServer;
  FrmMain.BtnRestart.Visible := pubModeServer;

  case Status of
    gsUnknown:
      begin
         BoxStatus.Caption := string.Empty;
         BoxStatus.Color := clBtnFace;
      end;
    gsPreparing: SetStatusLabel('PREPARING', clPurple);
    gsPlaying: SetStatusLabel('PLAYING', clBlack);
    gsMyTurn: SetStatusLabel('MYTURN', clGreen);
    gsWaitValid: SetStatusLabel('WAITVALID', clWebBrown);
    gsAgreement: SetStatusLabel('AGREEMENT', clBlue);
    gsContest: SetStatusLabel('CONTEST', $00656225);
    gsPaused: SetStatusLabel('PAUSED', $005B5B5B);
    gsGameOver: SetStatusLabel('GAMEOVER', $002C075C);
    else raise Exception.Create('Internal: Unsupported status');
  end;
end;

procedure TFrmGame.MatrixReceived(const A: string);
begin
  PB.UpdateData(A);
end;

procedure TFrmGame.ChatLog(const Player, Text: string; Other: Boolean);
var
  Color: TColor;
begin
  if Other then Color := clLime else Color := $000080FF;

  RichEditIncLogLine(EdChatLog, Player+': ', Text, clGray, Color);
end;

procedure TFrmGame.EdChatMsgKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;

    EdChatMsg.Text := Trim(EdChatMsg.Text);
    if EdChatMsg.Text = string.Empty then Exit;

    if not (LPlayers.Count>1) then
      MsgRaise(Lang.Get('GAME_MSG_CHAT_WITH_NOBODY'));

    ChatLog(pubPlayerName, EdChatMsg.Text, False);
    DMClient.C.Send('M', EdChatMsg.Text);
    EdChatMsg.Clear;
  end;
end;

procedure TFrmGame.LPlayersDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);

  procedure TextRight(X, Y: Integer; const A: string);
  var
    W: Integer;
  begin
    W := LPlayers.Canvas.TextWidth(A);
    LPlayers.Canvas.TextOut(X-W, Y, A);
  end;

var D: TMsgArray;
begin
  LPlayers.Canvas.FillRect(Rect);

  D := DataToArray(LPlayers.Items[Index]);

  LPlayers.Canvas.TextOut(LbHeaderPlayer.Left, Rect.Top+2, D[0]); //player name
  TextRight(LbHeaderScore.Left+LbHeaderScore.Width, Rect.Top+2, D[1]); //score

  if D[2] then
    IL.Draw(LPlayers.Canvas, 3, Rect.Top+1, 0); //this player turn

  if D[3] then
    IL.Draw(LPlayers.Canvas, 3, Rect.Top+1, 1); //agree

  if D[4] then
    IL.Draw(LPlayers.Canvas, LbHeaderScore.Left-25, Rect.Top+1, 2); //disconnected
end;

procedure TFrmGame.LLettersDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  LLetters.Canvas.FillRect(Rect);
  LLetters.Canvas.TextOut(Rect.Left+8, Rect.Top, LLetters.Items[Index]);
end;

procedure TFrmGame.BtnRestartClick(Sender: TObject);
begin
  SetStatus(gsUnknown); //transition status to gsPreparing
  DMServer.RestartGame;
end;

procedure TFrmGame.BtnStartGameClick(Sender: TObject);
begin
  if LPlayers.Count<2 then
    MsgRaise(Lang.Get('GAME_MSG_START_WITH_NOBODY'));

  SetStatus(gsUnknown); //transition status to gsPlaying
  DMServer.StartGame;
end;

procedure TFrmGame.BtnRulesClick(Sender: TObject);
begin
  ShowGameRules;
end;

procedure TFrmGame.BtnDoneClick(Sender: TObject);
begin
  if PB.Data.ContainsAnyInvalid then
    MsgRaise(Lang.Get('GAME_MSG_INVALID_SEQUENCE'));

  SetStatus(gsPlaying);
  DMClient.C.Send('!');
end;

procedure TFrmGame.BtnAgreeClick(Sender: TObject);
begin
  DMClient.SendAgreement(True);

  Log(Lang.Get('LOG_AGREE_SENT'));
end;

procedure TFrmGame.BtnDisagreeClick(Sender: TObject);
begin
  DMClient.SendAgreement(False);

  Log(Lang.Get('LOG_DISAGREE_SENT'));
end;

procedure TFrmGame.BtnContestAcceptClick(Sender: TObject);
begin
  SetStatus(gsPlaying);
  DMClient.SendContest(True);

  Log(Lang.Get('LOG_CONTEST_ACCEPT_SENT'));
end;

procedure TFrmGame.BtnContestRejectClick(Sender: TObject);
begin
  SetStatus(gsPlaying);
  DMClient.SendContest(False);

  Log(Lang.Get('LOG_CONTEST_REJECT_SENT'));
end;

procedure TFrmGame.GameStartedReceived;
begin
  SetStatus(gsPlaying);

  Log(Lang.Get('LOG_GAME_STARTED'));
end;

procedure TFrmGame.InitMyTurn;
begin
  SetStatus(gsMyTurn);

  Log(Lang.Get('LOG_YOUR_TURN'));
  DoSound('BELL');
end;

procedure TFrmGame.MyTurnTimeoutReceived;
begin
  SetStatus(gsPlaying);

  Log(Lang.Get('LOG_TURN_TIMEOUT'));
  DoSound('TIMEOUT');
end;

procedure TFrmGame.LettersExchangedReceived;
begin
  Log(Lang.Get('LOG_LETTERS_EXCHANGED'));
end;

procedure TFrmGame.ReceivedAutoRejectedByInvalidLetters;
begin
  Log(Lang.Get('LOG_AUTO_REJECTED_BY_INVALID_LETTERS'));
end;

procedure TFrmGame.WaitValidationReceived;
begin
  SetStatus(gsWaitValid);

  Log(Lang.Get('LOG_WAIT_VALIDATION'));
end;

procedure TFrmGame.ValidationAcceptedReceived;
begin
  SetStatus(gsPlaying);

  Log(Lang.Get('LOG_WORDS_ACCEPTED'));
  DoSound('DONE');
end;

procedure TFrmGame.ValidationRejectedReceived;
begin
  SetStatus(gsMyTurn);

  Log(Lang.Get('LOG_DISAGREE_RECEIVED'));
  DoSound('REJECT');
end;

procedure TFrmGame.AgreementRequestReceived;
begin
  SetStatus(gsAgreement);

  Log(Lang.Get('LOG_AGREEMENT_PERIOD_START'));
  DoSound('AGREEMENT');
end;

procedure TFrmGame.AgreementFinishReceived(const A: string);
begin
  SetStatus(gsPlaying);

  if DataToArray(A)[0] then
    Log(Lang.Get('LOG_AGREEMENT_PERIOD_FINISH_ACCEPTED'))
  else
    Log(Lang.Get('LOG_AGREEMENT_PERIOD_FINISH_REJECTED'));
  DoSound('AGREEMENT_END');
end;

procedure TFrmGame.OpenContestPeriodReceived;
begin
  SetStatus(gsContest);

  Log(Lang.Get('LOG_CONTEST_PERIOD_START'));
  DoSound('CONTEST');
end;

procedure TFrmGame.ReceivedContestResponse(const A: string);
begin
  if DataToArray(A)[0]{Accept} then
    Log(Lang.Get('LOG_RECEIVED_CONTEST_RESPONSE_ACCEPT'))
  else
    Log(Lang.Get('LOG_RECEIVED_CONTEST_RESPONSE_REJECT'));
end;

procedure TFrmGame.ReceivedPausedByDrop;
begin
  SetStatus(gsPaused);

  Log(Lang.Get('LOG_PAUSE_BY_DROP'));
end;

procedure TFrmGame.ReceivedDropContinue;
begin
  SetStatus(gsPlaying);

  Log(Lang.Get('LOG_DROP_CONTINUE'))
end;

procedure TFrmGame.GameOverReceived;
begin
  SetStatus(gsGameOver);

  Log(Lang.Get('LOG_GAME_OVER'));
end;

procedure TFrmGame.ReceivedPreparingNewGame;
begin
  SetStatus(gsPreparing);

  Log(Lang.Get('LOG_RESTART_GAME'));
end;

procedure TFrmGame.ReceivedTimerStart(const A: string);
begin
  TimerSeconds := A.ToInteger;
  TimerTimer(nil);
  Timer.Enabled := True;
end;

procedure TFrmGame.ReceivedTimerStop;
begin
  Timer.Enabled := False;
  LbTimer.Caption := string.Empty;
end;

procedure TFrmGame.TimerTimer(Sender: TObject);
begin
  LbTimer.Caption := Format(Lang.Get('GAME_TIMER'), [TimerSeconds]);
  if TimerSeconds=0 then
  begin
    Timer.Enabled := False;
    Exit;
  end;
  Dec(TimerSeconds);
end;

end.
