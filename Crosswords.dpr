program Crosswords;

{$R *.dres}

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  UVars in 'UVars.pas',
  UDictionary in 'UDictionary.pas',
  ULanguage in 'ULanguage.pas',
  UClient in 'UClient.pas',
  UMatrix in 'UMatrix.pas',
  URichEditUnicode in 'URichEditUnicode.pas',
  UDMClient in 'UDMClient.pas' {DMClient: TDataModule},
  UDMServer in 'UDMServer.pas' {DMServer: TDataModule},
  UFrmMain in 'UFrmMain.pas' {FrmMain},
  UFrmStart in 'UFrmStart.pas' {FrmStart},
  UFrmGame in 'UFrmGame.pas' {FrmGame},
  UFrmLog in 'UFrmLog.pas' {FrmLog},
  UFrmSettings in 'UFrmSettings.pas' {FrmSettings},
  UFrmRules in 'UFrmRules.pas' {FrmRules},
  UFrmDrop in 'UFrmDrop.pas' {FrmDrop};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Charcoal Dark Slate');
  Application.CreateForm(TDMClient, DMClient);
  Application.CreateForm(TDMServer, DMServer);
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
