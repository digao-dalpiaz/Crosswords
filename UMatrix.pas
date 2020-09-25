unit UMatrix;

interface

uses Vcl.ExtCtrls, Vcl.Graphics, System.Classes, Vcl.Controls, System.Types;

type
  TBlock = record
  private
    FLetter: Char;
    FTemp: Boolean;
  public
    property Letter: Char read FLetter;
    property Temp: Boolean read FTemp;

    procedure &Set(const Letter: Char; Temp: Boolean);

    procedure ClearTemp;
  end;
  TMatrixData = array of array of TBlock;

  TMatrixImage = class(TImage)
  private
    Data: TMatrixData;

    SelBox: TPoint;

    procedure Rebuild;

    const
      BoxW = 30;
      BoxH = 25;

  protected
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    procedure UpdateData(const A: string);
    procedure SetMatrixSize(X, Y: Integer);
  end;

function MatrixDataToString(const Data: TMatrixData): string;
procedure StringToMatrixData(const A: string; var Data: TMatrixData);

implementation

uses System.SysUtils, UFrmGame, UDams, UVars, System.StrUtils, UDMClient;

procedure TBlock.&Set(const Letter: Char; Temp: Boolean);
begin
  FLetter := Letter;
  FTemp := Temp and (Letter<>BLANK_LETTER);
end;

procedure TBlock.ClearTemp;
begin
  FTemp := False;
end;

//

function MatrixDataToString(const Data: TMatrixData): string;
var
  S: TStringList;
  X, Y: Integer;
  B: TBlock;
begin
  S := TStringList.Create;
  try
    for Y := 0 to High(Data) do
      for X := 0 to High(Data[Y]) do
      begin
        B := Data[Y, X];
        if B.Letter<>BLANK_LETTER then
          S.Add(Format('%dx%d=%s%s', [X, Y, B.Letter, IfThen(B.Temp, '*')]));
      end;

    Result := S.Text;
  finally
    S.Free;
  end;
end;

procedure StringToMatrixData(const A: string; var Data: TMatrixData);
var
  X, Y: Integer;
  S: TStringList;
  Item, Position, Value, PosX, PosY: string;
  Ar: TArray<string>;
begin
  for Y := 0 to High(Data) do
    for X := 0 to High(Data[Y]) do
      Data[Y, X].&Set(BLANK_LETTER, False);

  S := TStringList.Create;
  try
    S.Text := A;

    for Item in S do
    begin
      Ar := Item.Split(['=']);
      Position := Ar[0];
      Value := Ar[1];

      Ar := Position.Split(['x']);
      PosX := Ar[0];
      PosY := Ar[1];

      Data[PosY.ToInteger, PosX.ToInteger].&Set(Value[1], Value.EndsWith('*'));
    end;

  finally
    S.Free;
  end;
end;

//

procedure TMatrixImage.SetMatrixSize(X, Y: Integer);
begin
  SetLength(Data, 0); //clear
  SetLength(Data, Y, X);

  Width := (BoxW * X)+1;
  Height := (BoxH * Y)+1;

  SelBox := TPoint.Create(-1, -1);

  Rebuild;
end;

procedure TMatrixImage.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Previous: TBlock;

  procedure SetLetter(const C: Char; PCheck: TProc);
  var
    Line, Col: Integer;
  begin
    Line := SelBox.Y;
    Col := SelBox.X;

    Previous := Data[Line, Col];
    PCheck;

    Data[Line, Col].&Set(C, True);

    Rebuild;

    DMClient.SendLetter(Col, Line, C);
  end;

begin
  inherited;

  if not FrmGame.InGame then Exit;

  if not FrmGame.MyTurn then
    MsgRaise('Relax, it''s not your turn yet!');

  if Shift = [ssLeft] then
  begin
    if FrmGame.LLetters.ItemIndex = -1 then
      MsgRaise('Please, select a letter in the side panel.');

    SetLetter(FrmGame.LLetters.Items[FrmGame.LLetters.ItemIndex][1],
      procedure
      begin
        if Previous.Letter<>BLANK_LETTER then
          MsgRaise('There is already a letter in this block.');
      end);
    FrmGame.LLetters.DeleteSelected;
  end else
  if Shift = [ssRight] then
  begin
    SetLetter(BLANK_LETTER,
      procedure
      begin
        if Previous.Letter=BLANK_LETTER then
          MsgRaise('There is no letter to remove in this block.');

        if not Previous.Temp then
          MsgRaise('You cannot remove this letter because it''s not from this move.');
      end);
    FrmGame.LLetters.ItemIndex := FrmGame.LLetters.Items.Add(Previous.Letter);
  end;
end;

procedure TMatrixImage.MouseMove(Shift: TShiftState; X, Y: Integer);
var P: TPoint;
begin
  inherited;

  P := TPoint.Create(X div BoxW, Y div BoxH);

  if (P.X<>SelBox.X) or
     (P.Y<>SelBox.Y) then
  begin
    SelBox := P;
    Rebuild;
  end;
end;

procedure TMatrixImage.UpdateData(const A: string);
begin
  StringToMatrixData(A, Data);
  Rebuild;
end;

procedure TMatrixImage.Rebuild;
var
  B: TBitmap;
  Block: TBlock;
  Line, Col: Integer;
  X, Y: Integer;
begin
  B := TBitmap.Create;
  try
    B.SetSize(Width, Height);
    B.Canvas.Font.Name := 'Consolas';
    B.Canvas.Font.Size := 12;
    B.Canvas.Font.Style := [fsBold];
    B.Canvas.Pen.Color := clGray;

    Y := 0;

    for Line := 0 to High(Data) do
    begin
      X := 0;

      for Col := 0 to High(Data[Line]) do
      begin
        if (SelBox.X=Col) and (SelBox.Y=Line) then
          B.Canvas.Brush.Color := clYellow
        else
          B.Canvas.Brush.Color := clWhite;

        B.Canvas.Rectangle(X, Y, X+BoxW+1, Y+BoxH+1);

        Block := Data[Line, Col];
        if Block.Temp then
          B.Canvas.Font.Color := clRed
        else
          B.Canvas.Font.Color := clBlack;

        B.Canvas.TextOut(X+10, Y+3, Block.Letter);

        Inc(X, BoxW);
      end;

      Inc(Y, BoxH);
    end;

    Picture.Assign(B);
  finally
    B.Free;
  end;
end;

end.
