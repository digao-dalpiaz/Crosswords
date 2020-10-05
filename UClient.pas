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

    procedure RandomizeLetters(ChangeAll: Boolean);
    procedure ResetGameData;
  end;

  TPlayersList = class(TObjectList<TClient>)
  public
    procedure RandomList;
  end;

implementation

uses UVars, UDictionary, UDMServer, System.SysUtils,
  System.Generics.Defaults;

constructor TClient.Create;
var
  TmpHash: string;
begin
  repeat
    TmpHash := FormatFloat('000000', Random(999999)+1);
  until DMServer.FindPlayerByHash(TmpHash)=nil; //ensure unique

  Hash := TmpHash;
end;

procedure TClient.RandomizeLetters(ChangeAll: Boolean);
var
  I: Integer;
begin
  if (not ChangeAll) and (Letters.Length = pubServerProps.HandLetters) then
    raise Exception.Create('Internal: Cannot randomize letters without change all when hand is full');

  if ChangeAll then
    Letters := string.Empty;

  for I := Letters.Length+1 to pubServerProps.HandLetters do
    Letters := Letters + GetRandomLetter;
end;

procedure TClient.ResetGameData;
begin
  Letters := string.Empty;
  Score := 0;
  Agree := False;
end;

//

procedure TPlayersList.RandomList;
begin
  Sort(TComparer<TClient>.Construct(
    function(const L, R: TClient): Integer
    begin
      Result := Random(3)-1; //-1 or 0 or 1
    end
  ));
end;

end.
