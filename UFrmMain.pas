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
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure BtnSettingsClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure LbLinkClick(Sender: TObject);
  private
    procedure InitStartPage;
    procedure InitGamePage;
  public
    ClientRules: record
      Dictionary: string;
      SizeW, SizeH, InitialLetters, RebuyLetters: Integer;
    end;

    procedure InitTranslation;
    procedure ShowConnectionBox;
    procedure UpdateClientRules;
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

uses UVars, UDams, ULanguage,
  UFrmStart, UFrmGame, UFrmLog, UFrmSettings, UDMClient,
  System.StrUtils, System.SysUtils, Winapi.ShellAPI;

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

  UpdateClientRules;
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

procedure TFrmMain.ShowConnectionBox;
begin
  LbMode.Caption := Lang.Get(IfThen(pubModeServer, 'MODE_SERVER', 'MODE_CLIENT'));
  LbPlayer.Caption := pubPlayerName;
  LbRules.Caption := string.Empty; //will be received in further message

  BoxConInfo.Visible := True;
end;

procedure TFrmMain.UpdateClientRules;
begin
  with ClientRules do
  begin
    LbRules.Caption :=
      Format(Lang.Get('TITLE_RULES_DEFINITION'), [
      Dictionary, SizeW, SizeH, InitialLetters, RebuyLetters]);
  end;
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
