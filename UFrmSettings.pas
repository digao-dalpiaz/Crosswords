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

uses UVars, System.IniFiles, System.SysUtils, UFrmGame;

class procedure TSettings.Load;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetIniFilePath);
  try
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
  CkSounds.Checked := pubEnableSounds;
  BtnZoom.Position := pubGridZoom;
end;

procedure TFrmSettings.BtnOKClick(Sender: TObject);
begin
  TSettings.Save(Self);
  TSettings.Load; //reload settings

  FrmGame.PB.UpdateZoom;

  ModalResult := mrOk;
end;

end.
