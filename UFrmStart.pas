unit UFrmStart;

interface

uses Vcl.Forms, Vcl.StdCtrls, Vcl.Controls, Vcl.ComCtrls, System.Classes;

type
  TFrmStart = class(TForm)
    LbTitle: TLabel;
    Pages: TPageControl;
    TabClient: TTabSheet;
    TabServer: TTabSheet;
    LbPlayerName: TLabel;
    EdPlayerName: TEdit;
    EdServerAddress: TEdit;
    LbServerAddress: TLabel;
    LbTableSize: TLabel;
    EdSizeW: TEdit;
    EdSizeH: TEdit;
    LbTableSizeX: TLabel;
    LbDictionary: TLabel;
    EdDictionary: TComboBox;
    BtnJoin: TButton;
    BtnExit: TButton;
    Label1: TLabel;
    EdInitialLetters: TEdit;
    Label2: TLabel;
    EdRebuyLetters: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure BtnExitClick(Sender: TObject);
    procedure BtnJoinClick(Sender: TObject);
  private
    procedure LoadDictionaryList;
  public
    procedure EnableControls(En: Boolean);
  end;

var
  FrmStart: TFrmStart;

implementation

{$R *.dfm}

uses UDMClient, UDMServer, UVars, UDams, System.SysUtils,
  UFrmLog;

procedure TFrmStart.FormCreate(Sender: TObject);
begin
  Pages.ActivePageIndex := 0;

  EdPlayerName.MaxLength := 30;

  LoadDictionaryList;

  //Default client settings
  EdServerAddress.Text := 'localhost';

  //Default server settings
  EdSizeW.Text := '30';
  EdSizeH.Text := '20';
  EdDictionary.ItemIndex := 0;
  EdInitialLetters.Text := '10';
  EdRebuyLetters.Text := '5';
end;

procedure TFrmStart.LoadDictionaryList;
var D: TDictionary;
begin
  for D in LST_DICTIONARY do
    EdDictionary.Items.Add(D.Language);
end;

procedure TFrmStart.BtnJoinClick(Sender: TObject);
begin
  EdPlayerName.Text := Trim(EdPlayerName.Text);
  if EdPlayerName.Text = string.Empty then
  begin
    MsgError('Please, type your name.');
    EdPlayerName.SetFocus;
    Exit;
  end;

  //

  pubModeServer := (Pages.ActivePage = TabServer);

  if pubModeServer then
  begin
    //SERVER MODE

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

    pubServerProps.Dictionary := LST_DICTIONARY[EdDictionary.ItemIndex];
    pubServerProps.SizeW := StrToInt(EdSizeW.Text);
    pubServerProps.SizeH := StrToInt(EdSizeH.Text);
    pubServerProps.InitialLetters := StrToInt(EdInitialLetters.Text);
    pubServerProps.RebuyLetters := StrToInt(EdRebuyLetters.Text);

    DMClient.C.Host := 'localhost';
    DMServer.Initialize;
  end else
  begin
    //CLIENT MODE

    EdServerAddress.Text := Trim(EdServerAddress.Text);
    if EdServerAddress.Text = string.Empty then
    begin
      MsgError('Please, type the server address.');
      EdServerAddress.SetFocus;
      Exit;
    end;

    DMClient.C.Host := EdServerAddress.Text;
  end;

  EnableControls(False);

  pubPlayerName := EdPlayerName.Text;

  Log('Connecting...');
  DMClient.C.Connect;
end;

procedure TFrmStart.EnableControls(En: Boolean);
begin
  BtnJoin.Enabled := En;
  BtnExit.Enabled := En;
  Self.Enabled := En;
end;

procedure TFrmStart.BtnExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

end.
