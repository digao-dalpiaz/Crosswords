unit UVars;

interface

const
  STR_VERSION = '1.0 alpha';
  INT_TCP_PORT = 6631;

  BLANK_LETTER = #0;

type
  TDictionary = record
    Language, Resource: string;
  end;

const LST_DICTIONARY: array[0..0] of TDictionary = (
  (Language: 'Portuguese Brazil'; Resource: 'BR')
);

var
  pubPlayerName: string;
  pubModeServer: Boolean;

  //SERVER PROPERTIES
  pubServerProps: record
    Letters: String;
    SizeW, SizeH: Integer;
    InitialLetters, RebuyLetters: Integer;
  end;

  //SETTINGS
  pubEnableSounds: Boolean;

function GetDictionaryLetters(Dic: TDictionary): string;
function GetRandomLetter: Char;

procedure DoSound(const ResName: string);

implementation

uses System.Classes, System.SysUtils, Winapi.MMSystem, System.Types;

function GetDictionaryLetters(Dic: TDictionary): string;
var
  R: TResourceStream;
  S: TStringList;
  I: Integer;
  Name, Value: string;
  Letters: string;
begin
  S := TStringList.Create;
  try
    R := TResourceStream.Create(HInstance, 'DIC_'+Dic.Resource, RT_RCDATA);
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

  Result := Letters;
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

end.
