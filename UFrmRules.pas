unit UFrmRules;

interface

uses Vcl.Forms, Vcl.StdCtrls, Vcl.Controls, System.Classes;

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
    procedure FormCreate(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
  private
    procedure LoadDictionaryList;
  end;

var
  FrmRules: TFrmRules;

procedure ShowGameRules;

implementation

{$R *.dfm}

uses UVars, System.SysUtils, UDams, UDMServer;

procedure ShowGameRules;
begin
  FrmRules := TFrmRules.Create(Application);
  FrmRules.ShowModal;
  FrmRules.Free;
end;

//

procedure TFrmRules.FormCreate(Sender: TObject);
begin
  LoadDictionaryList;

  EdSizeW.Text := IntToStr(pubServerProps.SizeW);
  EdSizeH.Text := IntToStr(pubServerProps.SizeH);
  EdDictionary.ItemIndex := pubServerProps.DictionaryIndex;
  EdInitialLetters.Text := IntToStr(pubServerProps.InitialLetters);
  EdRebuyLetters.Text := IntToStr(pubServerProps.RebuyLetters);
end;

procedure TFrmRules.LoadDictionaryList;
var D: TDictionary;
begin
  for D in LST_DICTIONARY do
    EdDictionary.Items.Add(D.Language);
end;

procedure TFrmRules.BtnOKClick(Sender: TObject);
begin
  if StrToIntDef(EdSizeW.Text, 0) = 0 then
  begin
    MsgError('Please, type the table width size.');
    EdSizeW.SetFocus;
    Exit;
  end;
  if StrToIntDef(EdSizeH.Text, 0) = 0 then
  begin
    MsgError('Please, type the table height size.');
    EdSizeH.SetFocus;
    Exit;
  end;

  if EdDictionary.ItemIndex = -1 then
  begin
    MsgError('Please, specify dictionary language.');
    EdDictionary.SetFocus;
    Exit;
  end;

  if StrToIntDef(EdInitialLetters.Text, 0) = 0 then
  begin
    MsgError('Please, type the initial letters pocket.');
    EdInitialLetters.SetFocus;
    Exit;
  end;
  if StrToIntDef(EdRebuyLetters.Text, 0) = 0 then
  begin
    MsgError('Please, type the rebuy letters.');
    EdRebuyLetters.SetFocus;
    Exit;
  end;

  //

  pubServerProps.SizeW := StrToInt(EdSizeW.Text);
  pubServerProps.SizeH := StrToInt(EdSizeH.Text);
  pubServerProps.DictionaryIndex := EdDictionary.ItemIndex;
  pubServerProps.InitialLetters := StrToInt(EdInitialLetters.Text);
  pubServerProps.RebuyLetters := StrToInt(EdRebuyLetters.Text);

  //

  DMServer.SendRules(nil); //send rules to all players

  ModalResult := mrOk;
end;

end.
