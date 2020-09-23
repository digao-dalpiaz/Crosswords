program Scrabble;

{$R *.dres}

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  UVars in 'UVars.pas',
  UClient in 'UClient.pas',
  UDMClient in 'UDMClient.pas' {DMClient: TDataModule},
  UDMServer in 'UDMServer.pas' {DMServer: TDataModule},
  UFrmMain in 'UFrmMain.pas' {FrmMain},
  UFrmStart in 'UFrmStart.pas' {FrmStart},
  UFrmGame in 'UFrmGame.pas' {FrmGame},
  UFrmLog in 'UFrmLog.pas' {FrmLog},
  UMatrix in 'UMatrix.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Windows10 Green');
  Application.CreateForm(TDMClient, DMClient);
  Application.CreateForm(TDMServer, DMServer);
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
