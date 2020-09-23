unit UVars;

interface

const
  STR_VERSION = '1.0 alfa';
  INT_TCP_PORT = 6631;

  BLANK_LETTER = #0;

type
  TDictionary = record
    Language: string;
    LettersMAX,
    LettersMED,
    LettersMIN: string;
  end;

const LST_DICTIONARY: array[0..0] of TDictionary = (
  (Language: 'Portuguese Brazil'; LettersMAX: 'AEO'; LettersMED: 'BCDFGILMNPRSTUV'; LettersMIN: 'HJKQXYWZ')
);

var
  pubPlayerName: string;
  pubModeServer: Boolean;

  pubServerProps: record
    Dictionary: TDictionary;
    SizeW, SizeH: Integer;
    InitialLetters, RebuyLetters: Integer;
  end;

function GetRandomLetter: Char;

procedure DoSound(const ResName: string);

implementation

uses System.SysUtils, Winapi.MMSystem;

function GetRandomLetter: Char;
var
  IGrp: Integer;
  LetGrp: string;
begin
  repeat
    IGrp := Random(12);
    case IGrp of
      0..2 : LetGrp := pubServerProps.Dictionary.LettersMIN;
      3..6 : LetGrp := pubServerProps.Dictionary.LettersMED;
      7..11: LetGrp := pubServerProps.Dictionary.LettersMAX;
      else raise Exception.Create('Invalid group index');
    end;
  until not LetGrp.IsEmpty; //it's possible that some group don't have letters

  Result := LetGrp[Random(LetGrp.Length)+1];
end;

procedure DoSound(const ResName: string);
begin
  PlaySound(PChar(ResName), HInstance, SND_RESOURCE or SND_ASYNC);
end;

end.
