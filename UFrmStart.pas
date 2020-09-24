unit UFrmStart;

interface

uses Vcl.Forms, Vcl.StdCtrls, Vcl.Buttons, Vcl.Controls, Vcl.ComCtrls,
  System.Classes, Vcl.ExtCtrls;

type
  TFrmStart = class(TForm)
    LbTitle: TLabel;
    LbPlayerName: TLabel;
    EdPlayerName: TEdit;
    BtnJoin: TBitBtn;
    BtnExit: TBitBtn;
    EdPassword: TEdit;
    Label1: TLabel;
    BoxOper: TRadioGroup;
    BoxClient: TPanel;
    LbServerAddress: TLabel;
    EdServerAddress: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure BtnExitClick(Sender: TObject);
    procedure BtnJoinClick(Sender: TObject);
    procedure BoxOperClick(Sender: TObject);
  public
    procedure EnableControls(En: Boolean);
  end;

var
  FrmStart: TFrmStart;

implementation

{$R *.dfm}

uses UDMClient, UDMServer, UVars, UDams, System.SysUtils,
  UFrmLog;

procedure TFrmStart.FormCreate(Sender: TObject);
begin
  EdPlayerName.MaxLength := 30;

  BoxOper.ItemIndex := 0;
  BoxOperClick(nil);

  EdServerAddress.Text := 'localhost';
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
    MsgError('Please, type your name.');
    EdPlayerName.SetFocus;
    Exit;
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
    EdServerAddress.Text := Trim(EdServerAddress.Text);
    if EdServerAddress.Text = string.Empty then
    begin
      MsgError('Please, type the server address.');
      EdServerAddress.SetFocus;
      Exit;
    end;

    DMClient.C.Host := EdServerAddress.Text;
  end;

  EnableControls(False);

  pubPlayerName := EdPlayerName.Text;
  pubPassword := EdPassword.Text;

  Log('Connecting...');
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
