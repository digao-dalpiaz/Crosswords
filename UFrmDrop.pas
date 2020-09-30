unit UFrmDrop;

interface

uses Vcl.Forms, Vcl.StdCtrls, Vcl.Buttons, Vcl.Controls, Vcl.ExtCtrls,
  System.Classes,
  //
  UClient;

type
  TFrmDrop = class(TForm)
    L: TListBox;
    LbMissingPlayers: TLabel;
    BtnKill: TBitBtn;
    BtnStop: TBitBtn;
    BtnContinue: TBitBtn;
    Bevel1: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure BtnContinueClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure BtnKillClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    procedure AddPlayer(C: TClient);
  public
    procedure DelPlayer(C: TClient);
  end;

var
  FrmDrop: TFrmDrop;

procedure DoPlayerDroped(C: TClient);

implementation

{$R *.dfm}

uses UVars, UDams, ULanguage, UFrmMain, UFrmGame, UDMServer, System.SysUtils;

procedure DoPlayerDroped(C: TClient);
begin
  if not Assigned(FrmDrop) then
  begin
    FrmDrop := TFrmDrop.Create(Application);
    FrmDrop.Show;
  end;

  FrmDrop.AddPlayer(C);
end;

//

procedure TFrmDrop.FormCreate(Sender: TObject);
begin
  FixFormWidth(Self);

  //--Language
  Caption := Lang.Get('DROP_CAPTION');
  LbMissingPlayers.Caption := Lang.Get('DROP_LIST_TITLE');
  BtnKill.Caption := Lang.Get('DROP_BTN_KILL');
  BtnContinue.Caption := Lang.Get('DROP_BTN_CONTINUE');
  BtnStop.Caption := Lang.Get('DROP_BTN_STOP');
  //--
end;

procedure TFrmDrop.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := (ModalResult = mrOk);
end;

procedure TFrmDrop.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  FrmDrop := nil;
end;

procedure TFrmDrop.AddPlayer(C: TClient);
begin
  L.AddItem(Format('(%s) %s', [C.Hash, C.PlayerName]), C);
end;

procedure TFrmDrop.DelPlayer(C: TClient);
var
  I: Integer;
begin
  I := L.Items.IndexOfObject(C);
  if I=-1 then
    raise Exception.Create('Internal: Player not found on missing list to remove');

  L.Items.Delete(I);
end;

procedure TFrmDrop.BtnKillClick(Sender: TObject);
begin
  if L.ItemIndex = -1 then Exit;

  if QuestionKillPlayer then
  begin
    DMServer.KillPlayer(TClient(L.Items.Objects[L.ItemIndex]));
    L.DeleteSelected;
  end;
end;

procedure TFrmDrop.BtnContinueClick(Sender: TObject);
begin
  if L.Count>0 then
    MsgRaise(Lang.Get('DROP_MSG_STILL_DROPED'));

  if FrmGame.LPlayers.Count<2 then
    MsgRaise(Lang.Get('DROP_MSG_CONTINUE_WITH_NOBODY'));

  //

  DMServer.ContinueGame;

  ModalResult := mrOk;
  Close;
end;

procedure TFrmDrop.BtnStopClick(Sender: TObject);
begin
  if not QuestionStopGame then Exit;  

  DMServer.RestartGame;

  ModalResult := mrOk;
  Close;
end;

end.
