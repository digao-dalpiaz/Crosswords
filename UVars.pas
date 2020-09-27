unit UVars;

interface

const
  STR_VERSION = '1.0 alpha 4';
  INT_TCP_PORT = 6631;

  BLANK_LETTER = #0;

type
  TDictionary = record
    ID, LanguageName: string;
  end;

const LST_DICTIONARY: array[0..1] of TDictionary = (
  (ID: 'EN'; LanguageName: 'English'),
  (ID: 'BR'; LanguageName: 'Portuguese Brazil')
);

var
  pubPlayerName, pubPlayerHash, pubPassword: string;
  pubModeServer: Boolean;

  //SERVER PROPERTIES
  pubServerProps: record
    SizeW, SizeH: Integer;
    DictionaryID: string;
    InitialLetters, RebuyLetters: Integer;

    Letters: string;
  end;

  //SETTINGS
  pubLanguageID: string;
  pubEnableSounds: Boolean;
  pubGridZoom: Integer;

function GetCurrentDictionaryIndex: Integer;
procedure LoadDictionaryLetters;
function GetRandomLetter: Char;

procedure DoSound(const ResName: string);

function GetIniFilePath: String;

implementation

uses System.Classes, System.SysUtils, Winapi.MMSystem, System.Types, Vcl.Forms;

function GetCurrentDictionaryIndex: Integer;
var
  I: Integer;
begin
  for I := 0 to High(LST_DICTIONARY) do
    if LST_DICTIONARY[I].ID = pubServerProps.DictionaryID then Exit(I);

  raise Exception.Create('Internal: Dictionary not found by ID');
end;

procedure LoadDictionaryLetters;
var
  R: TResourceStream;
  S: TStringList;
  I: Integer;
  Name, Value: string;
  Letters: string;
begin
  S := TStringList.Create;
  try
    R := TResourceStream.Create(HInstance, 'DIC_'+pubServerProps.DictionaryID, RT_RCDATA);
    try
      S.LoadFromStream(R);
    finally
      R.Free;
    end;

    if S.Count=0 then
      raise Exception.Create('Internal: No letters found in the resource');

    for I := 0 to S.Count-1 do
    begin
      Name := S.Names[I];
      Value := S.ValueFromIndex[I];

      if Name.Length<>1 then
        raise Exception.Create('Internal: Letter must contain exactly one character');

      if StrToIntDef(Value, 0)<=0 then
        raise Exception.Create('Internal: Letter occurrences number are invalid');

      Letters := Letters + StringOfChar(Name[1], Value.ToInteger);
    end;
  finally
    S.Free;
  end;

  pubServerProps.Letters := Letters;
end;

function GetRandomLetter: Char;
begin
  Result := pubServerProps.Letters[Random(pubServerProps.Letters.Length)+1];
end;

procedure DoSound(const ResName: string);
begin
  if pubEnableSounds then
    PlaySound(PChar('SND_'+ResName), HInstance, SND_RESOURCE or SND_ASYNC);
end;

function GetIniFilePath: String;
begin
  Result := ExtractFilePath(Application.ExeName)+'Scrabble.ini';
end;

end.
