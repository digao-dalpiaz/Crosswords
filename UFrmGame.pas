unit UFrmGame;

interface

uses Vcl.Forms, System.ImageList, Vcl.ImgList, Vcl.Controls, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.ComCtrls, Vcl.ExtCtrls, System.Classes,
  //
  System.Types, URichEditUnicode, UMatrix;

type
  TGameStatus = (
    gsUnknown, gsPreparing,
    gsPlaying, gsMyTurn, gsWaitValid, gsAgreement,
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
    LbPlayers: TLabel;
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
  public
    Status: TGameStatus;
    PB: TMatrixImage;

    procedure Initialize(Reconnected: Boolean);
    procedure MatrixReceived(const A: string);
    procedure ChatLog(const Player, Text: string);
    procedure GameStartedReceived;
    procedure InitMyTurn;
    procedure WaitValidationReceived;
    procedure ValidationAcceptedReceived;
    procedure ValidationRejectedReceived;
    procedure AgreementRequestReceived;
    procedure AgreementFinishReceived(const A: string);
    procedure ReceivedPausedByDrop;
    procedure ReceivedDropContinue;
    procedure GameOverReceived;
    procedure ReceivedPreparingNewGame;
  private
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
  LbPlayers.Caption := Lang.Get('GAME_BOX_PLAYERS');
  LbLetters.Caption := Lang.Get('GAME_BOX_LETTERS');
  LbChat.Caption := Lang.Get('GAME_BOX_CHAT');

  BtnRestart.Caption := Lang.Get('GAME_BTN_RESTART');
  BtnStartGame.Caption := Lang.Get('GAME_BTN_START');
  BtnRules.Caption := Lang.Get('GAME_BTN_RULES');
  BtnDone.Caption := Lang.Get('GAME_BTN_DONE');
  BtnAgree.Caption := Lang.Get('GAME_BTN_AGREE');
  BtnDisagree.Caption := Lang.Get('GAME_BTN_DISAGREE');
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

  BtnRestart.Visible := (Status = gsGameOver) and pubModeServer;

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
    gsPaused: SetStatusLabel('PAUSED', $005B5B5B);
    gsGameOver: SetStatusLabel('GAMEOVER', $002C075C);
    else raise Exception.Create('Internal: Unsupported status');
  end;
end;

procedure TFrmGame.MatrixReceived(const A: string);
begin
  PB.UpdateData(A);
end;

procedure TFrmGame.ChatLog(const Player, Text: string);
begin
  RichEditIncLogLine(EdChatLog, Player+': ', Text, clGray, clLime);
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

    ChatLog(pubPlayerName, EdChatMsg.Text);
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

  LPlayers.Canvas.TextOut(25, Rect.Top+2, D[0]); //player name
  TextRight(200, Rect.Top+2, D[1]); //letters
  TextRight(236, Rect.Top+2, D[2]); //score

  if D[3] then
    IL.Draw(LPlayers.Canvas, 3, Rect.Top+1, 0); //this player turn

  if D[4] then
    IL.Draw(LPlayers.Canvas, 3, Rect.Top+1, 1); //agree

  if D[5] then
    IL.Draw(LPlayers.Canvas, 160, Rect.Top+1, 2); //disconnected
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

procedure TFrmGame.GameStartedReceived;
begin
  SetStatus(gsPlaying);

  Log(Lang.Get('LOG_GAME_STARTED'));
  DoSound('START');
end;

procedure TFrmGame.InitMyTurn;
begin
  SetStatus(gsMyTurn);

  Log(Lang.Get('LOG_YOUR_TURN'));
  DoSound('BELL');
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

end.
