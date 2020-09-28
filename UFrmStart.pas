unit UFrmStart;

interface

uses Vcl.Forms, Vcl.Mask, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Controls, Vcl.Buttons,
  System.Classes;

type
  TFrmStart = class(TForm)
    LbTitle: TLabel;
    BtnJoin: TBitBtn;
    BtnExit: TBitBtn;
    EdPassword: TEdit;
    LbPassword: TLabel;
    BoxOper: TRadioGroup;
    BoxClient: TPanel;
    LbServerAddress: TLabel;
    EdServerAddress: TEdit;
    CkReconnect: TCheckBox;
    BoxName: TPanel;
    BoxReconnect: TPanel;
    EdPlayerName: TEdit;
    LbPlayerName: TLabel;
    EdHash: TMaskEdit;
    LbHash: TLabel;
    LbLanguage: TLabel;
    EdLanguage: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure BtnExitClick(Sender: TObject);
    procedure BtnJoinClick(Sender: TObject);
    procedure BoxOperClick(Sender: TObject);
    procedure CkReconnectClick(Sender: TObject);
    procedure EdLanguageChange(Sender: TObject);
  private
    procedure InitTransation;
    procedure LoadLanguages;
  public
    procedure EnableControls(En: Boolean);
  end;

var
  FrmStart: TFrmStart;

implementation

{$R *.dfm}

uses UDMClient, UDMServer, UVars, UDams,
  System.SysUtils, System.StrUtils,
  UFrmMain, UFrmLog, ULanguage;

procedure TFrmStart.FormCreate(Sender: TObject);
begin
  InitTransation;

  LoadLanguages;
  EdLanguage.ItemIndex := GetCurrentLanguageIndex;

  EdPlayerName.MaxLength := 30;

  BoxOper.ItemIndex := 0;
  BoxOperClick(nil);

  EdServerAddress.Text := 'localhost';
end;

procedure TFrmStart.InitTransation;
begin
  LbTitle.Caption := Lang.Get('START_CAPTION');
  LbPlayerName.Caption := Lang.Get('START_NAME');
  LbHash.Caption := Lang.Get('START_HASH');
  BoxOper.Caption := Lang.Get('START_OPERATION');
  CkReconnect.Caption := Lang.Get('START_RECONNECT');
  LbServerAddress.Caption := Lang.Get('START_SERVER_ADDR');
  LbPassword.Caption := Lang.Get('START_CONN_PASSWORD');
  LbLanguage.Caption := Lang.Get('START_LANGUAGE');

  BoxOper.Items[0] := Lang.Get('MODE_CLIENT');
  BoxOper.Items[1] := Lang.Get('MODE_SERVER');

  BtnJoin.Caption := Lang.Get('START_BTN_JOIN');
  BtnExit.Caption := Lang.Get('START_BTN_EXIT');
end;

procedure TFrmStart.BoxOperClick(Sender: TObject);
begin
  pubModeServer := (BoxOper.ItemIndex=1);

  CkReconnect.Visible := not pubModeServer;
  BoxClient.Visible := not pubModeServer;

  CkReconnectClick(nil);
end;

procedure TFrmStart.CkReconnectClick(Sender: TObject);
var
  Reconnect: Boolean;
begin
  Reconnect := CkReconnect.Checked and not pubModeServer;

  BoxName.Visible := not Reconnect;
  BoxReconnect.Visible := Reconnect;
end;

procedure TFrmStart.BtnJoinClick(Sender: TObject);
begin
  if BoxName.Visible then
  begin
    EdPlayerName.Text := Trim(EdPlayerName.Text);
    if EdPlayerName.Text = string.Empty then
    begin
      MsgError(Lang.Get('START_MSG_BLANK_NAME'));
      EdPlayerName.SetFocus;
      Exit;
    end;
  end;

  if BoxReconnect.Visible then
  begin
    if EdHash.Text = string.Empty then //mask edit
    begin
      MsgError(Lang.Get('START_MSG_BLANK_HASH'));
      EdHash.SetFocus;
      Exit;
    end;

    if Length(EdHash.Text)<>EdHash.MaxLength then
    begin
      MsgError(Lang.Get('START_MSG_INVALID_HASH'));
      EdHash.SetFocus;
      Exit;
    end;
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

  pubPlayerName := IfThen(BoxName.Visible, EdPlayerName.Text);
  pubPlayerHash := IfThen(BoxReconnect.Visible, EdHash.Text);
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

procedure TFrmStart.LoadLanguages;
var
  D: TLangDefinition;
begin
  for D in LST_LANGUAGES do
    EdLanguage.Items.Add(D.Name);
end;

procedure TFrmStart.EdLanguageChange(Sender: TObject);
begin
  pubLanguageID := LST_LANGUAGES[EdLanguage.ItemIndex].ID;
  FrmMain.ConfigLanguage(True); //save config language
  Lang.LoadLanguage; //reload language

  //reload screen translation
  FrmMain.InitTranslation;
  FrmStart.InitTransation;
end;

end.
