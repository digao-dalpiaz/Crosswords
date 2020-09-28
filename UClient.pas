unit UClient;

interface

//Client object in Server connections

uses System.Generics.Collections, DzSocket;

type
  TClient = class
  public
    Hash: string;
    PlayerName: string;

    Letters: string;
    Score: Integer;
    Agree: Boolean;

    Socket: TDzSocket; //nil means disconnected

    constructor Create;

    procedure RandomizeInitialLetters;
    procedure ResetGameData;
  end;

  TPlayersList = class(TObjectList<TClient>);

implementation

uses UVars, UDictionary, UDMServer, System.SysUtils;

constructor TClient.Create;
var
  TmpHash: string;
begin
  repeat
    TmpHash := FormatFloat('000000', Random(999999)+1);
  until DMServer.FindPlayerByHash(TmpHash)=nil; //ensure unique

  Hash := TmpHash;
end;

procedure TClient.RandomizeInitialLetters;
var
  I: Integer;
begin
  if not Letters.IsEmpty then
    raise Exception.Create('Internal: Player letters should be empty');

  for I := 1 to pubServerProps.InitialLetters do
    Letters := Letters + GetRandomLetter;
end;

procedure TClient.ResetGameData;
begin
  Letters := string.Empty;
  Score := 0;
  Agree := False;
end;

end.
