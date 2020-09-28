unit UFrmLog;

interface

uses Vcl.Forms, System.Classes, Vcl.Controls, Vcl.StdCtrls, Vcl.ComCtrls,
  //
  Vcl.Graphics;

type
  TFrmLog = class(TForm)
    EdLog: TRichEdit;
    procedure FormCreate(Sender: TObject);
  public
    procedure UpdateFontSize;
  end;

var
  FrmLog: TFrmLog;

procedure InitLogArea;
procedure Log(const Text: string);

procedure RichEditIncLogLine(R: TRichEdit; const Text1, Text2: string; Color1, Color2: TColor);

implementation

{$R *.dfm}

uses System.SysUtils, Winapi.Windows, Winapi.Messages,
  UVars, UFrmMain;

procedure InitLogArea;
begin
  FrmLog := TFrmLog.Create(Application);
  FrmLog.Parent := FrmMain;
  FrmLog.Show;
end;

procedure Log(const Text: string);
begin
  RichEditIncLogLine(FrmLog.EdLog, FormatDateTime('hh:nn:ss', Now)+' - ', Text, clGray, clWhite);
end;

procedure RichEditIncLogLine(R: TRichEdit; const Text1, Text2: string; Color1, Color2: TColor);
begin
  R.SelStart := R.GetTextLen;
  R.SelAttributes.Color := Color1;
  R.SelText := Text1;
  R.SelAttributes.Color := Color2;
  R.SelText := Text2;
  R.SelText := #13#10;

  SendMessage(R.Handle, WM_VSCROLL, SB_BOTTOM, 0);
end;

//

procedure TFrmLog.FormCreate(Sender: TObject);
begin
  UpdateFontSize;
end;

procedure TFrmLog.UpdateFontSize;
begin
  EdLog.Font.Size := pubLogFontSize;
end;

end.
