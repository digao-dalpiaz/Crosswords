unit UFrmRules;

interface

uses Vcl.Forms, Vcl.StdCtrls, Vcl.Controls, System.Classes, Vcl.ExtCtrls;

type
  TFrmRules = class(TForm)
    LbTableSize: TLabel;
    EdSizeW: TEdit;
    LbTableSizeX: TLabel;
    EdSizeH: TEdit;
    LbDictionary: TLabel;
    EdDictionary: TComboBox;
    LbInitialLetters: TLabel;
    EdInitialLetters: TEdit;
    LbRebuyLetters: TLabel;
    EdRebuyLetters: TEdit;
    BtnOK: TButton;
    BtnCancel: TButton;
    Bevel1: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
  private
    procedure LoadDictionaryList;
  end;

  TRules = class
  public
    class procedure Save(F: TFrmRules);
    class procedure Load;
  end;

var
  FrmRules: TFrmRules;

procedure ShowGameRules;

implementation

{$R *.dfm}

uses UVars, UDams, UDictionary, ULanguage, UDMServer,
  System.SysUtils, System.IniFiles;

class procedure TRules.Load;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetIniFilePath);
  try
    pubServerProps.SizeW := Ini.ReadInteger('Rules', 'SizeW', 30);
    pubServerProps.SizeH := Ini.ReadInteger('Rules', 'SizeH', 20);
    pubServerProps.DictionaryID := Ini.ReadString('Rules', 'DictionaryID', 'EN');
    pubServerProps.InitialLetters := Ini.ReadInteger('Rules', 'InitialLetters', 10);
    pubServerProps.RebuyLetters := Ini.ReadInteger('Rules', 'RebuyLetters', 5);
  finally
    Ini.Free;
  end;
end;

class procedure TRules.Save(F: TFrmRules);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetIniFilePath);
  try
    Ini.WriteInteger('Rules', 'SizeW', StrToInt(F.EdSizeW.Text));
    Ini.WriteInteger('Rules', 'SizeH', StrToInt(F.EdSizeH.Text));
    Ini.WriteString('Rules', 'DictionaryID', LST_DICTIONARY[F.EdDictionary.ItemIndex].ID);
    Ini.WriteInteger('Rules', 'InitialLetters', StrToInt(F.EdInitialLetters.Text));
    Ini.WriteInteger('Rules', 'RebuyLetters', StrToInt(F.EdRebuyLetters.Text));
  finally
    Ini.Free;
  end;
end;

//

procedure ShowGameRules;
begin
  FrmRules := TFrmRules.Create(Application);
  FrmRules.ShowModal;
  FrmRules.Free;
end;

//

procedure TFrmRules.FormCreate(Sender: TObject);
begin
  FixFormWidth(Self);

  //--Translation
  Caption := Lang.Get('RULES_CAPTION');
  LbTableSize.Caption := Lang.Get('RULES_GRID_SIZE');
  LbDictionary.Caption := Lang.Get('RULES_DICTIONARY');
  LbInitialLetters.Caption := Lang.Get('RULES_INITIAL_LETTERS');
  LbRebuyLetters.Caption := Lang.Get('RULES_REBUY_LETTERS');

  BtnOK.Caption := Lang.Get('DLG_OK');
  BtnCancel.Caption := Lang.Get('DLG_CANCEL');
  //--

  LoadDictionaryList;

  EdSizeW.Text := IntToStr(pubServerProps.SizeW);
  EdSizeH.Text := IntToStr(pubServerProps.SizeH);
  EdDictionary.ItemIndex := GetCurrentDictionaryIndex;
  EdInitialLetters.Text := IntToStr(pubServerProps.InitialLetters);
  EdRebuyLetters.Text := IntToStr(pubServerProps.RebuyLetters);
end;

procedure TFrmRules.LoadDictionaryList;
var D: TDictionary;
begin
  for D in LST_DICTIONARY do
    EdDictionary.Items.Add(D.LanguageName);
end;

procedure TFrmRules.BtnOKClick(Sender: TObject);
begin
  if StrToIntDef(EdSizeW.Text, 0) = 0 then
  begin
    MsgError(Lang.Get('RULES_MSG_BLANK_FIELD'));
    EdSizeW.SetFocus;
    Exit;
  end;
  if StrToIntDef(EdSizeH.Text, 0) = 0 then
  begin
    MsgError(Lang.Get('RULES_MSG_BLANK_FIELD'));
    EdSizeH.SetFocus;
    Exit;
  end;

  if EdDictionary.ItemIndex = -1 then
  begin
    MsgError(Lang.Get('RULES_MSG_BLANK_FIELD'));
    EdDictionary.SetFocus;
    Exit;
  end;

  if StrToIntDef(EdInitialLetters.Text, 0) = 0 then
  begin
    MsgError(Lang.Get('RULES_MSG_BLANK_FIELD'));
    EdInitialLetters.SetFocus;
    Exit;
  end;
  if StrToIntDef(EdRebuyLetters.Text, 0) = 0 then
  begin
    MsgError(Lang.Get('RULES_MSG_BLANK_FIELD'));
    EdRebuyLetters.SetFocus;
    Exit;
  end;

  //

  TRules.Save(Self);
  TRules.Load; //reload rules

  //

  DMServer.SendRules(nil); //send rules to all players

  ModalResult := mrOk;
end;

end.
