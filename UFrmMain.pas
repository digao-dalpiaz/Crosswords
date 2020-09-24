unit UFrmMain;

interface

uses Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, System.Classes,
  DamUnit, Vcl.Buttons;

type
  TFrmMain = class(TForm)
    Dam: TDam;
    BoxTitle: TPanel;
    LbLink: TLinkLabel;
    LbVersion: TLabel;
    LbMode: TLabel;
    LbLbMode: TLabel;
    LbLbPlayer: TLabel;
    LbPlayer: TLabel;
    _QuestionCloseApp: TDamMsg;
    BoxTitleSide: TPanel;
    BtnSettings: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure LbLinkLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure BtnSettingsClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    procedure InitStartPage;
    procedure InitGamePage;
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

uses UVars, UFrmStart, UFrmGame, UFrmLog, UFrmSettings, UDams,
  Winapi.ShellAPI, UDMClient;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;

  LbVersion.Caption := 'Version '+STR_VERSION;

  TSettings.Load;

  Randomize;

  InitLogArea;
  InitStartPage;
  InitGamePage;

  FrmStart.Show;
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
  FrmStart.Top := (ClientHeight-FrmLog.Height - FrmStart.Height) div 2;
end;

procedure TFrmMain.LbLinkLinkClick(Sender: TObject; const Link: string;
  LinkType: TSysLinkType);
begin
  ShellExecute(0, '', PChar(Link), '', '', 0);
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
