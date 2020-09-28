unit UDictionary;

interface

type
  TDictionary = record
    ID, LanguageName: string;
  end;

const LST_DICTIONARY: array[0..1] of TDictionary = (
  (ID: 'EN'; LanguageName: 'English'),
  (ID: 'BR'; LanguageName: 'Portuguese Brazil')
);

function GetCurrentDictionaryIndex: Integer;
procedure LoadDictionaryLetters;
function GetRandomLetter: Char;

implementation

uses UVars, System.SysUtils, System.Classes, System.Types;

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

end.
