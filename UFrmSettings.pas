unit UFrmSettings;

interface

uses Vcl.Forms, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls,
  System.Classes;

type
  TFrmSettings = class(TForm)
    CkSounds: TCheckBox;
    BtnOK: TButton;
    BtnCancel: TButton;
    LbGridZoom: TLabel;
    EdGridZoom: TEdit;
    BtnZoom: TUpDown;
    LbLogFontSize: TLabel;
    EdLogFontSize: TEdit;
    BtnLogFontSize: TUpDown;
    Bevel1: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
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

uses UVars, ULanguage, System.IniFiles,
  UFrmGame, UFrmLog;

class procedure TSettings.Load;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetIniFilePath);
  try
    pubEnableSounds := Ini.ReadBool('Global', 'Sounds', True);
    pubGridZoom := Ini.ReadInteger('Global', 'GridZoom', 100);
    pubLogFontSize := Ini.ReadInteger('Global', 'LogFontSize', 10);
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
    Ini.WriteBool('Global', 'Sounds', F.CkSounds.Checked);
    Ini.WriteInteger('Global', 'GridZoom', F.BtnZoom.Position);
    Ini.WriteInteger('Global', 'LogFontSize', F.BtnLogFontSize.Position);
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
  FixFormWidth(Self);

  //--Translation
  Caption := Lang.Get('SETTINGS_CAPTION');
  CkSounds.Caption := Lang.Get('SETTINGS_SOUNDS');
  LbGridZoom.Caption := Lang.Get('SETTINGS_ZOOM');
  LbLogFontSize.Caption := Lang.Get('SETTINGS_LOG_FONT_SIZE');

  BtnOK.Caption := Lang.Get('DLG_OK');
  BtnCancel.Caption := Lang.Get('DLG_CANCEL');
  //--

  CkSounds.Checked := pubEnableSounds;
  BtnZoom.Position := pubGridZoom;
  BtnLogFontSize.Position := pubLogFontSize;
end;

procedure TFrmSettings.BtnOKClick(Sender: TObject);
begin
  TSettings.Save(Self);
  TSettings.Load; //reload settings

  FrmGame.PB.UpdateZoom; //update grid zoom
  FrmLog.UpdateFontSize; //update font size

  ModalResult := mrOk;
end;

end.
