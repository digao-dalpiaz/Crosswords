unit UFrmGame;

interface

uses Vcl.Forms, System.ImageList, Vcl.ImgList, Vcl.Controls, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.ComCtrls, Vcl.ExtCtrls, System.Classes,
  //
  System.Types, UMatrix;

type
  TGameStatus = (gsUnknown, gsPreparing, gsPlaying, gsMyTurn, gsAgreement, gsGameOver);

  TFrmGame = class(TForm)
    SB: TScrollBox;
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

    procedure InitTranslation;
    procedure Initialize;
    procedure MatrixReceived(const A: string);
    procedure ChatLog(const Player, Text: string);
    procedure GameStartedReceived;
    procedure InitMyTurn;
    procedure AgreementRequestReceived;
    procedure AgreementFinishReceived;
    procedure DisagreeReceived;
    procedure ReceivedPausedByDrop;
    procedure GameOverReceived;
    procedure ReceivedPreparingNewGame;
  private
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
  InitTranslation;

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

procedure TFrmGame.Initialize;
begin
  SetStatus(gsPreparing);

  PB.SetMatrixSize(0, 0);
  LPlayers.Clear;
  LLetters.Clear;
end;

procedure TFrmGame.SetStatus(NewStatus: TGameStatus);
begin
  Status := NewStatus;

  BtnStartGame.Visible := (Status = gsPreparing) and pubModeServer;
  BtnRules.Visible := (Status = gsPreparing) and pubModeServer;

  BtnDone.Visible := (Status = gsMyTurn);

  BtnAgree.Visible := (Status = gsAgreement);
  BtnDisagree.Visible := (Status = gsAgreement);

  BtnRestart.Visible := (Status = gsGameOver) and pubModeServer;
end;

procedure TFrmGame.MatrixReceived(const A: string);
begin
  PB.UpdateData(A);
end;

procedure TFrmGame.ChatLog(const Player, Text: string);
begin
  EdChatLog.SelStart := EdChatLog.GetTextLen;
  EdChatLog.SelAttributes.Color := clGray;
  EdChatLog.SelText := Player+': ';
  EdChatLog.SelAttributes.Color := clLime;
  EdChatLog.SelText := Text;
  EdChatLog.SelText := #13#10;

  SendMessage(EdChatLog.Handle, WM_VSCROLL, SB_BOTTOM, 0);
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
  TextRight(186, Rect.Top+2, D[1]); //letters
  TextRight(220, Rect.Top+2, D[2]); //score

  if D[3] then
    IL.Draw(LPlayers.Canvas, 3, Rect.Top+1, 0); //this player turn

  if D[4] then
    IL.Draw(LPlayers.Canvas, 3, Rect.Top+1, 1); //agree

  if D[5] then
    IL.Draw(LPlayers.Canvas, 150, Rect.Top+1, 2); //disconnected
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

procedure TFrmGame.AgreementRequestReceived;
begin
  SetStatus(gsAgreement);

  Log(Lang.Get('LOG_AGREEMENT_PERIOD_START'));
  DoSound('AGREEMENT');
end;

procedure TFrmGame.AgreementFinishReceived;
begin
  SetStatus(gsPlaying);

  Log(Lang.Get('LOG_AGREEMENT_PERIOD_FINISH'));
  DoSound('AGREEMENT_END');
end;

procedure TFrmGame.DisagreeReceived;
begin
  SetStatus(gsMyTurn);

  Log(Lang.Get('LOG_DISAGREE_RECEIVED'));
  DoSound('REJECT');
end;

procedure TFrmGame.ReceivedPausedByDrop;
begin
   SetStatus(gsPlaying);

   Log(Lang.Get('LOG_PAUSE_BY_DROP'));
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
