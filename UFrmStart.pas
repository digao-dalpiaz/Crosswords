unit UFrmStart;

interface

uses Vcl.Forms, Vcl.ExtCtrls, Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons,
  System.Classes;

type
  TFrmStart = class(TForm)
    LbTitle: TLabel;
    LbPlayerName: TLabel;
    EdPlayerName: TEdit;
    BtnJoin: TBitBtn;
    BtnExit: TBitBtn;
    EdPassword: TEdit;
    LbPassword: TLabel;
    BoxOper: TRadioGroup;
    BoxClient: TPanel;
    LbServerAddress: TLabel;
    EdServerAddress: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure BtnExitClick(Sender: TObject);
    procedure BtnJoinClick(Sender: TObject);
    procedure BoxOperClick(Sender: TObject);
  public
    procedure InitTransation;
    procedure EnableControls(En: Boolean);
  end;

var
  FrmStart: TFrmStart;

implementation

{$R *.dfm}

uses UDMClient, UDMServer, UVars, UDams, System.SysUtils,
  UFrmLog, ULanguage;

procedure TFrmStart.FormCreate(Sender: TObject);
begin
  InitTransation;

  EdPlayerName.MaxLength := 30;

  BoxOper.ItemIndex := 0;
  BoxOperClick(nil);

  EdServerAddress.Text := 'localhost';
end;

procedure TFrmStart.InitTransation;
begin
  LbTitle.Caption := Lang.Get('START_CAPTION');
  LbPlayerName.Caption := Lang.Get('START_NAME');
  BoxOper.Caption := Lang.Get('START_OPERATION');
  LbServerAddress.Caption := Lang.Get('START_SERVER_ADDR');
  LbPassword.Caption := Lang.Get('START_CONN_PASSWORD');

  BoxOper.Items[0] := Lang.Get('MODE_CLIENT');
  BoxOper.Items[1] := Lang.Get('MODE_SERVER');

  BtnJoin.Caption := Lang.Get('START_BTN_JOIN');
  BtnExit.Caption := Lang.Get('START_BTN_EXIT');
end;

procedure TFrmStart.BoxOperClick(Sender: TObject);
begin
  pubModeServer := (BoxOper.ItemIndex=1);
  BoxClient.Visible := not pubModeServer;
end;

procedure TFrmStart.BtnJoinClick(Sender: TObject);
begin
  EdPlayerName.Text := Trim(EdPlayerName.Text);
  if EdPlayerName.Text = string.Empty then
  begin
    MsgError(Lang.Get('START_MSG_BLANK_NAME'));
    EdPlayerName.SetFocus;
    Exit;
  end;

  if BoxClient.Visible then
  begin
    EdServerAddress.Text := Trim(EdServerAddress.Text);
    if EdServerAddress.Text = string.Empty then
    begin
      MsgError(Lang.Get('START_MSG_BLANK_SERVER_ADDR'));
      EdServerAddress.SetFocus;
      Exit;
    end;
  end;

  //

  if pubModeServer then
  begin
    //SERVER MODE
    DMClient.C.Host := 'localhost';
    DMServer.Initialize;
  end else
  begin
    //CLIENT MODE
    DMClient.C.Host := EdServerAddress.Text;
  end;

  EnableControls(False);

  pubPlayerName := EdPlayerName.Text;
  pubPassword := EdPassword.Text;

  Log(Lang.Get('LOG_CONNECTING'));
  DMClient.C.Connect;
end;

procedure TFrmStart.EnableControls(En: Boolean);
begin
  BtnJoin.Enabled := En;
  BtnExit.Enabled := En;
  Self.Enabled := En;
end;

procedure TFrmStart.BtnExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

end.
