unit UFrmSettings;

interface

uses Vcl.Forms, Vcl.StdCtrls, System.Classes, Vcl.Controls, Vcl.ComCtrls;

type
  TFrmSettings = class(TForm)
    CkSounds: TCheckBox;
    BtnOK: TButton;
    BtnCancel: TButton;
    LbGridZoom: TLabel;
    EdGridZoom: TEdit;
    BtnZoom: TUpDown;
    LbLanguage: TLabel;
    EdLanguage: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
  private
    procedure LoadLanguages;
  end;

  TSettings = class
  public
    class procedure Load;
    class procedure Save(F: TFrmSettings);
  end;

var
  FrmSettings: TFrmSettings;

procedure ShowSettings;

implementation

{$R *.dfm}

uses UVars, System.IniFiles, System.SysUtils, UFrmGame,
  ULanguage, UFrmMain, UFrmStart;

class procedure TSettings.Load;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetIniFilePath);
  try
    pubLanguageID := Ini.ReadString('Global', 'LanguageID', 'EN');
    pubEnableSounds := Ini.ReadBool('Global', 'Sounds', True);
    pubGridZoom := Ini.ReadInteger('Global', 'GridZoom', 100);
  finally
    Ini.Free;
  end;
end;

class procedure TSettings.Save(F: TFrmSettings);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetIniFilePath);
  try
    Ini.WriteString('Global', 'LanguageID', LST_LANGUAGES[F.EdLanguage.ItemIndex].ID);
    Ini.WriteBool('Global', 'Sounds', F.CkSounds.Checked);
    Ini.WriteInteger('Global', 'GridZoom', F.BtnZoom.Position);
  finally
    Ini.Free;
  end;
end;

//

procedure ShowSettings;
begin
  FrmSettings := TFrmSettings.Create(Application);
  FrmSettings.ShowModal;
  FrmSettings.Free;
end;

//

procedure TFrmSettings.FormCreate(Sender: TObject);
begin
  //--Translation
  Caption := Lang.Get('SETTINGS_CAPTION');
  LbLanguage.Caption := Lang.Get('SETTINGS_LANGUAGE');
  CkSounds.Caption := Lang.Get('SETTINGS_SOUNDS');
  LbGridZoom.Caption := Lang.Get('SETTINGS_ZOOM');

  BtnOK.Caption := Lang.Get('DLG_OK');
  BtnCancel.Caption := Lang.Get('DLG_CANCEL');
  //--

  LoadLanguages;

  EdLanguage.ItemIndex := GetCurrentLanguageIndex;
  CkSounds.Checked := pubEnableSounds;
  BtnZoom.Position := pubGridZoom;
end;

procedure TFrmSettings.LoadLanguages;
var
  D: TLangDefinition;
begin
  for D in LST_LANGUAGES do
    EdLanguage.Items.Add(D.Name);
end;

procedure TFrmSettings.BtnOKClick(Sender: TObject);
begin
  TSettings.Save(Self);
  TSettings.Load; //reload settings

  Lang.LoadLanguage; //reload language
  FrmMain.InitTranslation;
  FrmStart.InitTransation;
  FrmGame.InitTranslation;

  FrmGame.PB.UpdateZoom; //reload grid zoom

  ModalResult := mrOk;
end;

end.
