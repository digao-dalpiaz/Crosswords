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
    BoxW, BoxH: Integer;
    procedure Rebuild;
    procedure CalcBoxSize;
  protected
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    procedure SetMatrixSize(X, Y: Integer);
    procedure UpdateData(const A: string);

    procedure UpdateZoom;
  end;

function MatrixDataToString(const Data: TMatrixData): string;
procedure StringToMatrixData(const A: string; var Data: TMatrixData);

implementation

uses System.SysUtils, UFrmGame, UDams, UVars, System.StrUtils, UDMClient,
  ULanguage;

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

procedure TMatrixImage.CalcBoxSize;
var
  LX, LY: Integer;
begin
  BoxW := Round(30 * pubGridZoom/100);
  BoxH := Round(25 * pubGridZoom/100);

  LY := Length(Data);
  if LY>0 then LX := Length(Data[0]) else LX := 0;

  Width := (BoxW * LX)+1;
  Height := (BoxH * LY)+1;
end;

procedure TMatrixImage.SetMatrixSize(X, Y: Integer);
begin
  SetLength(Data, 0); //clear
  SetLength(Data, Y, X);

  SelBox := TPoint.Create(-1, -1);
  CalcBoxSize;
  Rebuild;
end;

procedure TMatrixImage.UpdateZoom;
begin
  CalcBoxSize;
  Rebuild;
end;

procedure TMatrixImage.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Previous: TBlock;

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

  if not (FrmGame.Status in [gsPlaying, gsMyTurn, gsAgreement]) then Exit;

  if FrmGame.Status <> gsMyTurn then
    MsgRaise(Lang.Get('GAME_MSG_NOT_YOUR_TURN'));

  if Shift = [ssLeft] then
  begin
    if FrmGame.LLetters.ItemIndex = -1 then
      MsgRaise(Lang.Get('GAME_MSG_SELECT_LETTER'));

    SetLetter(FrmGame.LLetters.Items[FrmGame.LLetters.ItemIndex][1],
      procedure
      begin
        if Previous.Letter<>BLANK_LETTER then
          MsgRaise(Lang.Get('GAME_MSG_ALREADY_LETTER_BLOCK'));
      end);
    FrmGame.LLetters.DeleteSelected;
  end else
  if Shift = [ssRight] then
  begin
    SetLetter(BLANK_LETTER,
      procedure
      begin
        if Previous.Letter=BLANK_LETTER then
          MsgRaise(Lang.Get('GAME_MSG_NO_LETTER_BLOCK'));

        if not Previous.Temp then
          MsgRaise(Lang.Get('GAME_MSG_CANT_REMOVE_LETTER_BLOCK'));
      end);
    FrmGame.LLetters.ItemIndex := FrmGame.LLetters.Items.Add(Previous.Letter);
  end;
end;

procedure TMatrixImage.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
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
  TE: TSize;
begin
  B := TBitmap.Create;
  try
    B.SetSize(Width, Height);
    B.Canvas.Font.Name := 'Consolas';
    B.Canvas.Font.Size := Round(14 * pubGridZoom/100);
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

        TE := B.Canvas.TextExtent(Block.Letter);
        B.Canvas.TextOut(X+((BoxW-TE.Width) div 2), Y+((BoxH-TE.Height) div 2), Block.Letter);

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
