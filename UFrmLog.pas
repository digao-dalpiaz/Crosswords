unit UFrmLog;

interface

uses Vcl.Forms, System.Classes, Vcl.Controls, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TFrmLog = class(TForm)
    EdLog: TRichEdit;
  end;

var
  FrmLog: TFrmLog;

procedure InitLogArea;
procedure Log(const Text: string);

implementation

{$R *.dfm}

uses System.SysUtils, Vcl.Graphics, Winapi.Windows, Winapi.Messages,
  UFrmMain;

procedure InitLogArea;
begin
  FrmLog := TFrmLog.Create(Application);
  FrmLog.Parent := FrmMain;
  FrmLog.Show;
end;

procedure Log(const Text: string);
var
  R: TRichEdit;
begin
  R := FrmLog.EdLog;

  R.SelStart := R.GetTextLen;
  R.SelAttributes.Color := clGray;
  R.SelText := FormatDateTime('hh:nn:ss', Now)+' - ';
  R.SelAttributes.Color := clWhite;
  R.SelText := Text;
  R.SelText := #13#10;

  SendMessage(R.Handle, WM_VSCROLL, SB_BOTTOM, 0);
end;

end.
