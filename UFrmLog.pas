unit UFrmLog;

interface

uses Vcl.Forms, System.Classes, Vcl.Controls, Vcl.StdCtrls;

type
  TFrmLog = class(TForm)
    EdLog: TMemo;
  end;

var
  FrmLog: TFrmLog;

procedure InitLogArea;
procedure Log(const Text: string);

implementation

{$R *.dfm}

uses System.SysUtils, UFrmMain;

procedure InitLogArea;
begin
  FrmLog := TFrmLog.Create(Application);
  FrmLog.Parent := FrmMain;
  FrmLog.Show;
end;

procedure Log(const Text: string);
begin
  FrmLog.EdLog.Lines.Add(FormatDateTime('hh:nn:ss', Now)+' - '+Text);
end;

end.
