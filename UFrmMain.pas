unit UFrmMain;

interface

uses Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, System.Classes,
  DamUnit;

type
  TFrmMain = class(TForm)
    Dam: TDam;
    Panel1: TPanel;
    LinkLabel1: TLinkLabel;
    LbVersion: TLabel;
    LbMode: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    LbPlayer: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure LinkLabel1LinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
  private
    procedure InitStartPage;
    procedure InitGamePage;
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

uses UVars, UFrmStart, UFrmGame, UFrmLog, Winapi.ShellAPI;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;

  LbVersion.Caption := 'Version '+STR_VERSION;

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

procedure TFrmMain.LinkLabel1LinkClick(Sender: TObject; const Link: string;
  LinkType: TSysLinkType);
begin
  ShellExecute(0, '', PChar(Link), '', '', 0);
end;

end.
