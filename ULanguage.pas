unit ULanguage;

interface

uses System.Classes, DamUnit;

type
  TLangDefinition = record
    ID, Name: string;
    DamLang: TDamLanguage;
  end;

const
  LST_LANGUAGES: array[0..1] of TLangDefinition = (
    (ID: 'EN'; Name: 'English'; DamLang: dgEnglish),
    (ID: 'BR'; Name: 'Portuguese Brazil'; DamLang: dgPortuguese)
  );

type
  TLang = class
  private
    Data: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadLanguage;

    function Get(const Ident: string): string;
  end;

var Lang: TLang;

function GetCurrentLanguageIndex: Integer;

implementation

uses System.SysUtils, System.Types, UVars, UFrmMain;

function GetCurrentLanguageIndex: Integer;
var
  I: Integer;
begin
  for I := 0 to High(LST_LANGUAGES) do
    if LST_LANGUAGES[I].ID = pubLanguageID then Exit(I);

  raise Exception.Create('Internal: Language not found by ID');
end;

constructor TLang.Create;
begin
  inherited;
  Data := TStringList.Create;
end;

destructor TLang.Destroy;
begin
  Data.Free;
  inherited;
end;

procedure TLang.LoadLanguage;
var
  LD: TLangDefinition;
  R: TResourceStream;
begin
  LD := LST_LANGUAGES[GetCurrentLanguageIndex];

  FrmMain.Dam.Language := LD.DamLang;

  R := TResourceStream.Create(HInstance, 'LANG_'+LD.ID, RT_RCDATA);
  try
    Data.LoadFromStream(R, TEncoding.UTF8);
  finally
    R.Free;
  end;
end;

function TLang.Get(const Ident: string): string;
begin
  Result := Data.Values[Ident];

  if Result.IsEmpty then
    raise Exception.CreateFmt('Cannot retrieve language ident "%s"', [Ident]);
end;

initialization
  Lang := TLang.Create;

finalization
  Lang.Free;

end.
