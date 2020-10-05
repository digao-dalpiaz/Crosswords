unit UMatrix;

interface

uses Vcl.ExtCtrls, System.Classes, Vcl.Controls, System.Types,
  System.Generics.Collections;

type
  TBlock = class
  private
    X, Y: Integer;

    FLetter: Char;
    FTemp: Boolean;

    Invalid: Boolean;
  public
    constructor Create(X, Y: Integer);

    property Letter: Char read FLetter;
    property Temp: Boolean read FTemp;

    procedure &Set(const Letter: Char; Temp: Boolean);
    procedure ClearTemp;
  end;
  TMatrixDataRow = class(TObjectList<TBlock>);
  TMatrixData = class(TObjectList<TMatrixDataRow>)
  private
    function GetColCount: Integer;
    procedure SetBlankAllBlocks;
  public
    procedure Init(Rows, Cols: Integer);

    procedure ValidateSequence(CenterBlock: TBlock);
    function ContainsAnyInvalid: Boolean;
    function ContainsAnyTemp: Boolean;
    procedure RemoveAllTempLetters;

    procedure LoadFromString(const A: string);
    function SaveToString: string;
  end;

  TMatrixImage = class(TImage)
  private
    SelBox: TPoint;
    BoxW, BoxH: Integer;
    procedure Rebuild;
    procedure CalcBoxSize;
  protected
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    Data: TMatrixData;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetMatrixSize(Rows, Cols: Integer);
    procedure UpdateData(const A: string);
    procedure UpdateZoom;
  end;

implementation

uses System.SysUtils, UFrmGame, UDams, UVars, System.StrUtils,
  UDMClient, ULanguage, Vcl.Graphics;

constructor TBlock.Create(X, Y: Integer);
begin
  inherited Create;
  Self.X := X;
  Self.Y := Y;
end;

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

procedure TMatrixData.Init(Rows, Cols: Integer);
var
  X, Y: Integer;
  R: TMatrixDataRow;
  B: TBlock;
begin
  Clear;

  for Y := 0 to Rows-1 do
  begin
    R := TMatrixDataRow.Create;
    Add(R);

    for X := 0 to Cols-1 do
    begin
      B := TBlock.Create(X, Y);
      R.Add(B);
    end;
  end;
end;

procedure TMatrixData.SetBlankAllBlocks;
var
  Row: TMatrixDataRow;
  B: TBlock;
begin
  for Row in Self do
    for B in Row do
    begin
      B.&Set(BLANK_LETTER, False);
      B.Invalid := False;
    end;
end;

function TMatrixData.GetColCount: Integer;
begin
  if Count>0 then
    Result := First.Count
  else
    Result := 0;
end;

procedure TMatrixData.ValidateSequence(CenterBlock: TBlock);
var
  LSeq: TList<TBlock>;

  procedure Run(B: TBlock);

    procedure Check(Row, Col: Integer);
    var
      nearB: TBlock;
    begin
      if (Row<0) or (Row>Count-1) or
         (Col<0) or (Col>GetColCount-1) then Exit;

      nearB := Self[Row][Col];
      if nearB.Letter<>BLANK_LETTER then
        Run(nearB);
    end;

  begin
    if LSeq.Contains(B) then Exit;    
    LSeq.Add(B);

    Check(B.Y-1, B.X); //up
    Check(B.Y+1, B.X); //down
    Check(B.Y, B.X-1); //left
    Check(B.Y, B.X+1); //right
  end;

var
  Row: TMatrixDataRow;
  B: TBlock;
begin
  LSeq := TList<TBlock>.Create;
  try
    Run(CenterBlock);

    //--Set Invalid flag in all blocks
    for Row in Self do
      for B in Row do
        B.Invalid := (B.Letter<>BLANK_LETTER) and not LSeq.Contains(B);
    //--
  finally
    LSeq.Free;
  end;
end;

function TMatrixData.ContainsAnyInvalid: Boolean;
var
  Row: TMatrixDataRow;
  B: TBlock;
begin
  for Row in Self do
    for B in Row do
      if B.Invalid then Exit(True);

   Result := False;
end;

function TMatrixData.ContainsAnyTemp: Boolean;
var
  Row: TMatrixDataRow;
  B: TBlock;
begin
  for Row in Self do
    for B in Row do
      if B.Temp then Exit(True);

  Result := False;
end;

procedure TMatrixData.RemoveAllTempLetters;
var
  Row: TMatrixDataRow;
  B: TBlock;
begin
  for Row in Self do
    for B in Row do
      if B.Temp then
      begin
        B.&Set(BLANK_LETTER, False);
        B.Invalid := False; //could be invalid letters if player turn have reached time-out
      end;
end;

function TMatrixData.SaveToString: string;
var
  S: TStringList;
  Row: TMatrixDataRow;
  B: TBlock;
begin
  S := TStringList.Create;
  try
    for Row in Self do
      for B in Row do
      begin
        if B.Letter<>BLANK_LETTER then
          S.Add(Format('%dx%d=%s', [B.X, B.Y, B.Letter+IfThen(B.Temp, '*')+IfThen(B.Invalid, '#')]));
      end;

    Result := S.Text;
  finally
    S.Free;
  end;
end;

procedure TMatrixData.LoadFromString(const A: string);
var
  S: TStringList;
  Item, Position, Value, PosX, PosY: string;
  Ar: TArray<string>;
  B: TBlock;
begin
  SetBlankAllBlocks;

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

      B := Self[PosY.ToInteger][PosX.ToInteger];
      B.&Set(Value[1], Value.Contains('*'));
      B.Invalid := Value.Contains('#');
    end;

  finally
    S.Free;
  end;
end;

//

constructor TMatrixImage.Create(AOwner: TComponent);
begin
  inherited;
  Data := TMatrixData.Create;
end;

destructor TMatrixImage.Destroy;
begin
  Data.Free;
  inherited;
end;

procedure TMatrixImage.CalcBoxSize;
begin
  BoxW := Round(30 * pubGridZoom/100);
  BoxH := Round(25 * pubGridZoom/100);

  Height := (BoxH * Data.Count)+1;
  Width := (BoxW * Data.GetColCount)+1;
end;

procedure TMatrixImage.SetMatrixSize(Rows, Cols: Integer);
begin
  Data.Init(Rows, Cols);
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
  B: TBlock;
  PreviousLetter: Char;

  procedure SetLetter(const C: Char; PCheck: TProc);
  begin
    B := Data[SelBox.Y][SelBox.X];
    PCheck;

    PreviousLetter := B.Letter;

    B.&Set(C, True);
    Rebuild;

    DMClient.SendLetter(SelBox.X, SelBox.Y, C);
  end;

begin
  inherited;

  if (SelBox.Y>Data.Count-1) or
     (SelBox.X>Data.GetColCount-1) then Exit;

  if not (FrmGame.Status in [gsPlaying, gsMyTurn, gsWaitValid, gsAgreement, gsContest]) then Exit;

  if FrmGame.Status <> gsMyTurn then
    MsgRaise(Lang.Get('GAME_MSG_NOT_YOUR_TURN'));

  if Shift = [ssLeft] then
  begin
    if FrmGame.LLetters.ItemIndex = -1 then
      MsgRaise(Lang.Get('GAME_MSG_SELECT_LETTER'));

    SetLetter(FrmGame.LLetters.Items[FrmGame.LLetters.ItemIndex][1],
      procedure
      begin
        if B.Letter<>BLANK_LETTER then
          MsgRaise(Lang.Get('GAME_MSG_ALREADY_LETTER_BLOCK'));
      end);
    FrmGame.LLetters.DeleteSelected;
  end else
  if Shift = [ssRight] then
  begin
    SetLetter(BLANK_LETTER,
      procedure
      begin
        if B.Letter=BLANK_LETTER then
          MsgRaise(Lang.Get('GAME_MSG_NO_LETTER_BLOCK'));

        if not B.Temp then
          MsgRaise(Lang.Get('GAME_MSG_CANT_REMOVE_LETTER_BLOCK'));
      end);
    FrmGame.LLetters.ItemIndex := FrmGame.LLetters.Items.Add(PreviousLetter);
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
    FrmGame.LbPosition.Caption := ' '+Format(Lang.Get('GAME_GRID_POSITION'), [P.Y+1, P.X+1]);

    SelBox := P;
    Rebuild;
  end;
end;

procedure TMatrixImage.UpdateData(const A: string);
begin
  Data.LoadFromString(A);
  Rebuild;
end;

procedure TMatrixImage.Rebuild;
var
  B: TBitmap;
  Row: TMatrixDataRow;
  Block: TBlock;
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

    for Row in Data do
    begin
      X := 0;

      for Block in Row do
      begin
        if (SelBox.X=Block.X) and (SelBox.Y=Block.Y) then
          B.Canvas.Brush.Color := $00A68737
        else
        if Block.Invalid then
          B.Canvas.Brush.Color := $000C3DAD
        else
          B.Canvas.Brush.Color := $003E3E3E;

        B.Canvas.Rectangle(X, Y, X+BoxW+1, Y+BoxH+1);

        if Block.Temp then
          B.Canvas.Font.Color := clYellow
        else
          B.Canvas.Font.Color := clWhite;

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
