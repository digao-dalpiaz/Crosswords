unit UFrmGame;

interface

uses Vcl.Forms, System.ImageList, Vcl.ImgList, Vcl.Controls, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.ComCtrls, Vcl.ExtCtrls, System.Classes,
  //
  System.Types, UMatrix;

type
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
  public
    InGame, MyTurn: Boolean;

    procedure Initialize(SizeW, SizeH: Integer);
    procedure ChatLog(const Player, Text: string);
    procedure MatrixReceived(const A: string);
    procedure InitMyTurn;
    procedure AgreementRequestReceived;
    procedure AgreementFinishReceived;
    procedure DisagreeReceived;
  private
    PB: TMatrixImage;
    procedure ShowAgreementButtons(Flag: Boolean);
  end;

var
  FrmGame: TFrmGame;

implementation

{$R *.dfm}

uses System.SysUtils, UDMClient, UVars, UDMServer, UDams, DzSocket, UFrmMain,
  Vcl.Graphics, Winapi.Windows, Winapi.Messages, System.StrUtils, UFrmLog;

procedure TFrmGame.FormCreate(Sender: TObject);
begin
  PB := TMatrixImage.Create(Self);
  PB.Parent := SB;
end;

procedure TFrmGame.Initialize(SizeW, SizeH: Integer);
begin
  InGame := False;
  MyTurn := False;

  BtnDone.Visible := False;
  ShowAgreementButtons(False);

  PB.SetMatrixSize(SizeW, SizeH);
  LPlayers.Clear;
  LLetters.Clear;
end;

procedure TFrmGame.FormShow(Sender: TObject);
begin
  BtnStartGame.Visible := pubModeServer;

  FrmMain.LbMode.Caption := IfThen(pubModeServer, 'Server', 'Client');
  FrmMain.LbPlayer.Caption := pubPlayerName;
end;

procedure TFrmGame.FormHide(Sender: TObject);
begin
  FrmMain.LbMode.Caption := string.Empty;
  FrmMain.LbPlayer.Caption := string.Empty;
end;

procedure TFrmGame.BtnDisconnectClick(Sender: TObject);
begin
  DMClient.C.Disconnect;
end;

procedure TFrmGame.BtnStartGameClick(Sender: TObject);
begin
  if LPlayers.Count<2 then
    MsgRaise('We need at least two players to start the game!');

  BtnStartGame.Visible := False;
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
var D: TMsgArray;
begin
  LPlayers.Canvas.FillRect(Rect);

  D := DataToArray(LPlayers.Items[Index]);

  LPlayers.Canvas.TextOut(25, Rect.Top+2, D[0]); //player name
  LPlayers.Canvas.TextOut(186, Rect.Top+2, D[1]); //letters
  LPlayers.Canvas.TextOut(208, Rect.Top+2, D[2]); //score

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
  BtnDone.Visible := True;

  Log('Go, it''s your turn!');
  DoSound('BELL');
end;

procedure TFrmGame.BtnDoneClick(Sender: TObject);
begin
  DMClient.C.Send('!');
  BtnDone.Visible := False;
  MyTurn := False;
end;

procedure TFrmGame.ShowAgreementButtons(Flag: Boolean);
begin
  BtnAgree.Visible := Flag;
  BtnDisagree.Visible := Flag;
end;

procedure TFrmGame.AgreementRequestReceived;
begin
  ShowAgreementButtons(True);

  Log('Please, make sure you agree with your opponent''s words.');
  DoSound('AGREEMENT');
end;

procedure TFrmGame.AgreementFinishReceived;
begin
  ShowAgreementButtons(False);

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
  BtnDone.Visible := True;

  Log('At least one player does not agree with your words. Please, review it or use chat.');

  DoSound('REJECT');
end;

end.
