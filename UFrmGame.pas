unit UFrmGame;

interface

uses Vcl.Forms, System.ImageList, Vcl.ImgList, Vcl.Controls, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.ComCtrls, Vcl.ExtCtrls, System.Classes,
  //
  System.Types, UMatrix;

type
  TButtonsPage = (bpPreparing, bpPlaying, bpMyTurn, bpAgreement, bpGameOver);

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
    BtnDisconnect: TBitBtn;
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
    procedure BtnDisconnectClick(Sender: TObject);
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
    InGame, MyTurn: Boolean;
    PB: TMatrixImage;

    procedure Initialize;
    procedure ChatLog(const Player, Text: string);
    procedure MatrixReceived(const A: string);
    procedure InitMyTurn;
    procedure AgreementRequestReceived;
    procedure AgreementFinishReceived;
    procedure DisagreeReceived;
    procedure GameOverReceived;
  private
    procedure SetButtonsPage(Pg: TButtonsPage);
  end;

var
  FrmGame: TFrmGame;

implementation

{$R *.dfm}

uses System.SysUtils, UDMClient, UVars, UDMServer, UDams, DzSocket, UFrmMain,
  Vcl.Graphics, Winapi.Windows, Winapi.Messages, System.StrUtils, UFrmLog,
  UFrmRules;

procedure TFrmGame.FormCreate(Sender: TObject);
begin
  PB := TMatrixImage.Create(Self);
  PB.Parent := SB;
end;

procedure TFrmGame.Initialize;
begin
  InGame := False;
  MyTurn := False;

  SetButtonsPage(bpPreparing);

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

procedure TFrmGame.SetButtonsPage(Pg: TButtonsPage);
begin
  BtnStartGame.Visible := (Pg = bpPreparing) and pubModeServer;
  BtnRules.Visible := (Pg = bpPreparing) and pubModeServer;

  BtnDone.Visible := (Pg = bpMyTurn);

  BtnAgree.Visible := (Pg = bpAgreement);
  BtnDisagree.Visible := (Pg = bpAgreement);

  BtnRestart.Visible := (Pg = bpGameOver) and pubModeServer;
end;

procedure TFrmGame.BtnDisconnectClick(Sender: TObject);
begin
  DMClient.C.Disconnect;
end;

procedure TFrmGame.BtnRulesClick(Sender: TObject);
begin
  ShowGameRules;
end;

procedure TFrmGame.BtnStartGameClick(Sender: TObject);
begin
  if LPlayers.Count<2 then
    MsgRaise('We need at least two players to start the game!');

  SetButtonsPage(bpPlaying);

  DMServer.StartGame;
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

procedure TFrmGame.LLettersDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  LLetters.Canvas.FillRect(Rect);
  LLetters.Canvas.TextOut(Rect.Left+8, Rect.Top, LLetters.Items[Index]);
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

procedure TFrmGame.MatrixReceived(const A: string);
begin
  PB.UpdateData(A);
end;

procedure TFrmGame.InitMyTurn;
begin
  MyTurn := True;
  SetButtonsPage(bpMyTurn);

  Log('Go, it''s your turn!');
  DoSound('BELL');
end;

procedure TFrmGame.BtnDoneClick(Sender: TObject);
begin
  DMClient.C.Send('!');
  SetButtonsPage(bpPlaying);
  MyTurn := False;
end;

procedure TFrmGame.AgreementRequestReceived;
begin
  SetButtonsPage(bpAgreement);

  Log('Please, make sure you agree with your opponent''s words.');
  DoSound('AGREEMENT');
end;

procedure TFrmGame.AgreementFinishReceived;
begin
  SetButtonsPage(bpPlaying);

  Log('The agreement period has ended.');
  DoSound('AGREEMENT_END');
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

procedure TFrmGame.DisagreeReceived;
begin
  MyTurn := True;
  SetButtonsPage(bpMyTurn);

  Log('At least one player does not agree with your words. Please, review it or use chat.');

  DoSound('REJECT');
end;

procedure TFrmGame.GameOverReceived;
begin
  InGame := False;
  Log('Game over.');

  SetButtonsPage(bpGameOver);
end;

procedure TFrmGame.BtnRestartClick(Sender: TObject);
begin
  SetButtonsPage(bpPreparing);
  DMServer.RestartGame;
end;

end.
