unit UClient;

interface

//Client object in Server connections

type
  TClient = class
  public
    PlayerName: string;
    Letters: string;
    Score: Integer;
    Agree: Boolean;

    procedure RandomizeInitialLetters;
  end;

implementation

uses UVars, System.SysUtils;

procedure TClient.RandomizeInitialLetters;
var
  I: Integer;
begin
  if not Letters.IsEmpty then
    raise Exception.Create('Player letters should be empty');

  for I := 1 to pubServerProps.InitialLetters do
    Letters := Letters + GetRandomLetter;
end;

end.
