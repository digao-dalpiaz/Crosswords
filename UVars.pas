unit UVars;

interface

uses Vcl.Forms;

const
  STR_VERSION = '1.0 alpha 12';
  INT_TCP_PORT = 6631;

  BLANK_LETTER = #0;

var
  pubPlayerName, pubPlayerHash, pubPassword: string;
  pubModeServer: Boolean;

  //SERVER PROPERTIES
  pubServerProps: record
    SizeW, SizeH: Integer;
    DictionaryID: string;
    HandLetters, GoalScore: Integer;
    TurnTimeout: Boolean; TurnTimeoutSecs, AgreementTimeoutSecs: Integer;

    Letters: string;
  end;

  //SETTINGS
  pubLanguageID: string;
  pubEnableSounds: Boolean;
  pubGridZoom: Integer;
  pubLogFontSize: Integer;

procedure DoSound(const ResName: string);
function GetIniFilePath: String;
procedure FixFormWidth(F: TForm);

implementation

uses Winapi.MMSystem, System.SysUtils;

procedure DoSound(const ResName: string);
begin
  if pubEnableSounds then
    PlaySound(PChar('SND_'+ResName), HInstance, SND_RESOURCE or SND_ASYNC);
end;

function GetIniFilePath: String;
begin
  Result := ExtractFilePath(Application.ExeName)+'Crosswords.ini';
end;

procedure FixFormWidth(F: TForm);
begin
  F.ClientWidth := F.ClientWidth+8; //fix theme behavior
end;

end.
