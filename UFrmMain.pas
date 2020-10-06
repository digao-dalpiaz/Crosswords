unit UFrmMain;

interface

uses Vcl.Forms, DamUnit, Vcl.Buttons, Vcl.Controls, Vcl.StdCtrls,
  System.Classes, Vcl.ExtCtrls;

type
  TFrmMain = class(TForm)
    Dam: TDam;
    BoxTitle: TPanel;
    LbVersion: TLabel;
    _QuestionCloseApp: TDamMsg;
    BoxTitleSide: TPanel;
    BtnSettings: TSpeedButton;
    LbLink: TLabel;
    LbSpace1: TLabel;
    LbSpace2: TLabel;
    LbSpace3: TLabel;
    BoxConInfo: TPanel;
    LbLbMode: TLabel;
    LbMode: TLabel;
    LbSpace4: TLabel;
    LbLbPlayer: TLabel;
    LbPlayer: TLabel;
    LbSpace5: TLabel;
    LbLbRules: TLabel;
    LbRules: TLabel;
    _QuestionKillPlayer: TDamMsg;
    _QuestionStopGame: TDamMsg;
    BtnRestart: TSpeedButton;
    _QuestionRestartGame: TDamMsg;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure BtnSettingsClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure LbLinkClick(Sender: TObject);
    procedure BtnRestartClick(Sender: TObject);
  private
    procedure InitStartPage;
    procedure InitGamePage;
  public
    procedure ConfigLanguage(Save: Boolean);
    procedure InitTranslation;
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

uses UVars, UDams, ULanguage,
  UFrmStart, UFrmGame, UFrmLog, UFrmSettings, UDMClient, UDMServer, UFrmDrop,
  System.SysUtils, System.IniFiles, Winapi.ShellAPI;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;

  ConfigLanguage(False); //load config language
  Lang.LoadLanguage;
  InitTranslation;

  TSettings.Load;

  Randomize;

  InitLogArea;
  InitStartPage;
  InitGamePage;

  FrmStart.Show;
end;

procedure TFrmMain.ConfigLanguage(Save: Boolean);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetIniFilePath);
  try
    if Save then
      Ini.WriteString('Language', 'ID', pubLanguageID)
    else
      pubLanguageID := Ini.ReadString('Language', 'ID', 'EN');
  finally
    Ini.Free;
  end;
end;

procedure TFrmMain.InitTranslation;
begin
  LbVersion.Caption := Format(Lang.Get('TITLE_VERSION'), [STR_VERSION]);
  LbLbMode.Caption := Lang.Get('TITLE_MODE')+' ';
  LbLbPlayer.Caption := Lang.Get('TITLE_PLAYER')+' ';
  LbLbRules.Caption := Lang.Get('TITLE_RULES')+' ';
  BtnRestart.Caption := Lang.Get('TITLE_RESTART');

  _QuestionCloseApp.Message := Lang.Get('MSG_CLOSE_APP');
  _QuestionKillPlayer.Message := Lang.Get('MSG_KILL_PLAYER');
  _QuestionStopGame.Message := Lang.Get('MSG_STOP_GAME');
  _QuestionRestartGame.Message := Lang.Get('MSG_RESTART_GAME');
end;

procedure TFrmMain.InitStartPage;
begin
  FrmStart := TFrmStart.Create(Application);
  FrmStart.Parent := Self;
end;

procedure TFrmMain.InitGamePage;
begin
  FrmGame := TFrmGame.Create(Application);
  FrmGame.Parent := Self;
end;

procedure TFrmMain.FormResize(Sender: TObject);
begin
  FrmStart.Left := (ClientWidth - FrmStart.Width) div 2;
  FrmStart.Top := BoxTitle.Height + ((ClientHeight-FrmStart.Height-BoxTitle.Height-FrmLog.Height) div 2);
end;

procedure TFrmMain.LbLinkClick(Sender: TObject);
begin
  ShellExecute(0, '', 'http://digaodalpiaz.com/', '', '', 0);
end;

procedure TFrmMain.BtnRestartClick(Sender: TObject);
begin
  if Assigned(FrmDrop) then Exit;

  if QuestionRestartGame then
    DMServer.RestartGame;
end;

procedure TFrmMain.BtnSettingsClick(Sender: TObject);
begin
   ShowSettings;
end;

procedure TFrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if DMClient.C.Connected then
    if not QuestionCloseApp then CanClose := False;
end;

end.
