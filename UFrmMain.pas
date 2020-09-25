unit UFrmMain;

interface

uses Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, System.Classes,
  DamUnit, Vcl.Buttons;

type
  TFrmMain = class(TForm)
    Dam: TDam;
    BoxTitle: TPanel;
    LbVersion: TLabel;
    LbMode: TLabel;
    LbLbMode: TLabel;
    LbLbPlayer: TLabel;
    LbPlayer: TLabel;
    _QuestionCloseApp: TDamMsg;
    BoxTitleSide: TPanel;
    BtnSettings: TSpeedButton;
    LbLbRules: TLabel;
    LbRules: TLabel;
    LbLink: TLabel;
    LbSpace1: TLabel;
    LbSpace2: TLabel;
    LbSpace3: TLabel;
    LbSpace4: TLabel;
    LbSpace5: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure BtnSettingsClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure LbLinkClick(Sender: TObject);
  private
    procedure InitStartPage;
    procedure InitGamePage;
  public
    procedure InitTranslation;
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

uses UVars, UFrmStart, UFrmGame, UFrmLog, UFrmSettings, UDams,
  Winapi.ShellAPI, UDMClient, ULanguage, System.SysUtils;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;

  TSettings.Load;

  Lang.LoadLanguage;
  InitTranslation;

  Randomize;

  InitLogArea;
  InitStartPage;
  InitGamePage;

  FrmStart.Show;
end;

procedure TFrmMain.InitTranslation;
begin
  LbVersion.Caption := Format(Lang.Get('TITLE_VERSION'), [STR_VERSION]);
  LbLbMode.Caption := Lang.Get('TITLE_MODE')+' ';
  LbLbPlayer.Caption := Lang.Get('TITLE_PLAYER')+' ';
  LbLbRules.Caption := Lang.Get('TITLE_RULES')+' ';

  _QuestionCloseApp.Message := Lang.Get('MSG_CLOSE_APP');
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
