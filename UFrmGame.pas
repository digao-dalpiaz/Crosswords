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
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
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

    procedure Initialize;
    procedure MatrixReceived(const A: string);
    procedure ChatLog(const Player, Text: string);
    procedure GameStartedReceived;
    procedure InitMyTurn;
    procedure AgreementRequestReceived;
    procedure AgreementFinishReceived;
    procedure DisagreeReceived;
    procedure GameOverReceived;
    procedure ReceivedPreparingNewGame;
  private
    procedure SetStatus(NewStatus: TGameStatus);
  end;

var
  FrmGame: TFrmGame;

implementation

{$R *.dfm}

uses System.SysUtils, System.StrUtils,
  Vcl.Graphics, Winapi.Windows, Winapi.Messages,
  UVars, UDams, DzSocket, UDMClient, UDMServer, UFrmLog, UFrmMain, UFrmRules;

procedure TFrmGame.FormCreate(Sender: TObject);
begin
  PB := TMatrixImage.Create(Self);
  PB.Parent := SB;
end;

procedure TFrmGame.Initialize;
begin
  SetStatus(gsPreparing);

  PB.SetMatrixSize(0, 0);
  LPlayers.Clear;
  LLetters.Clear;
end;

procedure TFrmGame.FormShow(Sender: TObject);
begin
  FrmMain.LbMode.Caption := IfThen(pubModeServer, 'Server', 'Client');
  FrmMain.LbPlayer.Caption := pubPlayerName;
end;

procedure TFrmGame.FormHide(Sender: TObject);
begin
  FrmMain.LbMode.Caption := string.Empty;
  FrmMain.LbPlayer.Caption := string.Empty;
  FrmMain.LbRules.Caption := string.Empty;
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
      MsgRaise('There are no other players to chat with.');

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
end;

procedure TFrmGame.LLettersDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  LLetters.Canvas.FillRect(Rect);
  LLetters.Canvas.TextOut(Rect.Left+8, Rect.Top, LLetters.Items[Index]);
end;

procedure TFrmGame.BtnStartGameClick(Sender: TObject);
begin
  if LPlayers.Count<2 then
    MsgRaise('We need at least two players to start the game!');

  SetStatus(gsUnknown); //transition status
  DMServer.StartGame;
end;

procedure TFrmGame.BtnRestartClick(Sender: TObject);
begin
  SetStatus(gsUnknown); //transition status
  DMServer.RestartGame;
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

  Log('You have sent an agreement for the words.');
end;

procedure TFrmGame.BtnDisagreeClick(Sender: TObject);
begin
  DMClient.SendAgreement(False);

  Log('You have sent an disagreement for the words.');
end;

procedure TFrmGame.GameStartedReceived;
begin
  SetStatus(gsPlaying);

  Log('Get ready. Starting the game right now!');
  DoSound('START');
end;

procedure TFrmGame.InitMyTurn;
begin
  SetStatus(gsMyTurn);

  Log('Go, it''s your turn!');
  DoSound('BELL');
end;

procedure TFrmGame.AgreementRequestReceived;
begin
  SetStatus(gsAgreement);

  Log('Please, make sure you agree with your opponent''s words.');
  DoSound('AGREEMENT');
end;

procedure TFrmGame.AgreementFinishReceived;
begin
  SetStatus(gsPlaying);

  Log('The agreement period has ended.');
  DoSound('AGREEMENT_END');
end;

procedure TFrmGame.DisagreeReceived;
begin
  SetStatus(gsMyTurn);

  Log('At least one player does not agree with your words. Please, review it or use chat.');
  DoSound('REJECT');
end;

procedure TFrmGame.GameOverReceived;
begin
  SetStatus(gsGameOver);

  Log('Game over.');
end;

procedure TFrmGame.ReceivedPreparingNewGame;
begin
  SetStatus(gsPreparing);

  Log('The server is preparing a new game.');
end;

end.
