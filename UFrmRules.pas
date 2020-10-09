unit UFrmRules;

interface

uses Vcl.Forms, Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls, System.Classes;

type
  TFrmRules = class(TForm)
    LbTableSize: TLabel;
    EdSizeW: TEdit;
    LbTableSizeX: TLabel;
    EdSizeH: TEdit;
    LbDictionary: TLabel;
    EdDictionary: TComboBox;
    LbHandLetters: TLabel;
    EdHandLetters: TEdit;
    BtnOK: TButton;
    BtnCancel: TButton;
    Bevel1: TBevel;
    CkTurnTimeout: TCheckBox;
    LbTurnTimeoutSecs: TLabel;
    EdTurnTimeoutSecs: TEdit;
    LbGoalScore: TLabel;
    EdGoalScore: TEdit;
    EdAgreementTimeoutSecs: TEdit;
    LbAgreementTimeoutSecs: TLabel;
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
    pubServerProps.HandLetters := Ini.ReadInteger('Rules', 'HandLetters', 7);
    pubServerProps.GoalScore := Ini.ReadInteger('Rules', 'GoalScore', 50);
    pubServerProps.TurnTimeout := Ini.ReadBool('Rules', 'TurnTimeout', False);
    pubServerProps.TurnTimeoutSecs := Ini.ReadInteger('Rules', 'TurnTimeoutSecs', 60);
    pubServerProps.AgreementTimeoutSecs := Ini.ReadInteger('Rules', 'AgreementTimeoutSecs', 10);
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
    Ini.WriteInteger('Rules', 'HandLetters', StrToInt(F.EdHandLetters.Text));
    Ini.WriteInteger('Rules', 'GoalScore', StrToInt(F.EdGoalScore.Text));
    Ini.WriteBool('Rules', 'TurnTimeout', F.CkTurnTimeout.Checked);
    Ini.WriteInteger('Rules', 'TurnTimeoutSecs', StrToInt(F.EdTurnTimeoutSecs.Text));
    Ini.WriteInteger('Rules', 'AgreementTimeoutSecs', StrToInt(F.EdAgreementTimeoutSecs.Text));
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
  LbHandLetters.Caption := Lang.Get('RULES_HAND_LETTERS');
  LbGoalScore.Caption := Lang.Get('RULES_GOAL_SCORE');
  CkTurnTimeout.Caption := Lang.Get('RULES_TURN_TIMEOUT_FLAG');
  LbTurnTimeoutSecs.Caption := Lang.Get('RULES_TURN_TIMEOUT_SECS');
  LbAgreementTimeoutSecs.Caption := Lang.Get('RULES_AGREEMENT_TIMEOUT_SECS');

  BtnOK.Caption := Lang.Get('DLG_OK');
  BtnCancel.Caption := Lang.Get('DLG_CANCEL');
  //--

  LoadDictionaryList;

  EdSizeW.Text := IntToStr(pubServerProps.SizeW);
  EdSizeH.Text := IntToStr(pubServerProps.SizeH);
  EdDictionary.ItemIndex := GetCurrentDictionaryIndex;
  EdHandLetters.Text := IntToStr(pubServerProps.HandLetters);
  EdGoalScore.Text := IntToStr(pubServerProps.GoalScore);
  CkTurnTimeout.Checked := pubServerProps.TurnTimeout;
  EdTurnTimeoutSecs.Text := IntToStr(pubServerProps.TurnTimeoutSecs);
  EdAgreementTimeoutSecs.Text := IntToStr(pubServerProps.AgreementTimeoutSecs);
end;

procedure TFrmRules.LoadDictionaryList;
var D: TDictionary;
begin
  for D in LST_DICTIONARY do
    EdDictionary.Items.Add(D.LanguageName);
end;

procedure TFrmRules.BtnOKClick(Sender: TObject);

  procedure CheckIntField(Ed: TEdit);
  begin
    if StrToIntDef(Ed.Text, 0) = 0 then
    begin
      MsgError(Lang.Get('RULES_MSG_BLANK_FIELD'));
      Ed.SetFocus;
      Exit;
    end;
  end;

begin
  CheckIntField(EdSizeW);
  CheckIntField(EdSizeH);

  if EdDictionary.ItemIndex = -1 then
  begin
    MsgError(Lang.Get('RULES_MSG_BLANK_FIELD'));
    EdDictionary.SetFocus;
    Exit;
  end;

  CheckIntField(EdHandLetters);
  CheckIntField(EdGoalScore);

  if CkTurnTimeout.Checked then
  begin
    CheckIntField(EdTurnTimeoutSecs);
    CheckIntField(EdAgreementTimeoutSecs);
  end;

  //

  TRules.Save(Self);
  TRules.Load; //reload rules

  //

  DMServer.SendRules(nil); //send rules to all players

  ModalResult := mrOk;
end;

end.
