unit URichEditUnicode;

interface

uses Vcl.ComCtrls, Vcl.Controls;

type
  TRichEdit = class(Vcl.ComCtrls.TRichEdit)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

implementation

uses Winapi.Windows, System.SysUtils;

procedure TRichEdit.CreateParams(var Params: TCreateParams);
begin
  inherited;
  CreateSubClass(Params, 'RICHEDIT50W');
end;

var RichModule: HMODULE;

initialization
  RichModule := LoadLibrary('MSFTEDIT.DLL');
  if RichModule=0 then
    raise Exception.Create('Could not load RichEdit library');

finalization
  if RichModule<>0 then
    FreeLibrary(RichModule)

end.
