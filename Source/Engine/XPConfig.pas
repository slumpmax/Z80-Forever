unit XPConfig;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, Classes, SysUtils, Graphics, GraphType, Math;

type
  TArrayOfByte = array of Byte;
  TArrayOfWord = array of Word;
  TArrayOfInteger = array of Integer;

  TXPConfig = class
  const
    DefaultAddress = $0000;
    DefaultStackAddress = $F000;
    MaxMessageLine = 1000;
    MaxSystemLine = 80;
  end;

  TStringListHelper = class helper for TStringList
  public
    procedure AddMessage(AText: string);
  end;

  PRGB32 = ^TRGB32;
  TRGB32 = packed record
  public
    constructor Create(AR, AG, AB: Byte; Alpha: Byte = 255);
  case Integer of
    0: (B, G, R, A: Byte);
    1: (BGRA: Cardinal);
  end;
  PRGB32Array = ^TRGB32Array;
  TRGB32Array = array[0..32767] of TRGB32;

  TLine32 = array [0 .. maxint div SizeOf(TRGB32) - 1] of TRGB32;
  PLine32 = ^TLine32;

  TIntArray = array of integer;
  TDeltaArray = array of array of integer;

function GetTabStr(AText: string): string;
function ByteToShort(AByte: Byte): Integer;
function HexText(ADigit, AValue: Integer): string;
function TextIndex(const AList, AText: string; ASize: Integer): Integer;
function MyDiv(const A, B: Double): Double;
function MyRound(const X: Double): Integer;
procedure ResizeBitmap(ASrc, ADest: TBitmap; AWidth, AHeight: Integer);
procedure ResizeBitmap2(ASrc, ADest: TBitmap; AWidth, AHeight: Integer);

implementation

function GetTabStr(AText: string): string;
var
  n: Integer;
begin
  n := ((Length(AText) + 8) div 8) * 8;
  Result := Copy(AText + StringOfChar(' ', n), 1, n);
end;

function ByteToShort(AByte: Byte): Integer;
begin
  if AByte > 127 then
    Result := AByte - 256
  else Result := AByte;
end;

function HexText(ADigit, AValue: Integer): string;
var
  sd: string;
begin
  sd := IntToStr(ADigit);
  Result := Format('%' + sd + '.' + sd + 'Xh', [AValue]);
  if Result[1] > '9' then Result := '0' + Result;
end;

function TextIndex(const AList, AText: string; ASize: Integer): Integer;
begin
  Result := Pos('[' + AText + ']', AList);
  if Result > 0 then
    Result := StrToIntDef(Copy(AList, Result - ASize, ASize), -1)
  else Result := -1;
end;

function MyDiv(const A, B: Double): Double;
begin
  if B = 0 then
    Result := A
  else Result := A / B;
end;

function MyRound(const X: Double): Integer;
begin
  Result := Trunc(x);
  if Frac(x) >= 0.5 then
  begin
    if x >= 0 then
      Result := Result + 1
    else Result := Result - 1;
  end;
  // Result := Trunc(X + (-2 * Ord(X < 0) + 1) * 0.5);
end;

procedure MakeStepsAndWeights(xscale, yscale: Single; xw, yh: integer;
  out dxmin, dymin: integer; out Weights: TDeltaArray;
  out xsteps, ysteps: TIntArray);
var
  i, j: Integer;
  x1, x2: Integer;
  dxmax, dymax, intscale: Integer;
  fact: Single;
begin
  xsteps := nil;
  ysteps := nil;
  Weights := nil;
  SetLength(xsteps, xw);
  SetLength(ysteps, yh);
  intscale := round(xscale * $10000);
  // won't work if xcale > $10000/2, because then intscale
  // exceeds 32bit integer. I don't see that happening.
  x1 := 0;
  x2 := intscale shr 16;
  for i := 0 to xw - 1 do
  begin
    xsteps[i] := x2 - x1;
    x1 := x2;
    x2 := (i + 2) * intscale shr 16;
  end;
  dxmin := Ceil(xscale - 1);
  dxmax := trunc(xscale + 1);

  intscale := round(yscale * $10000);
  x1 := 0;
  x2 := intscale shr 16;
  for i := 0 to yh - 1 do
  begin
    ysteps[i] := x2 - x1;
    x1 := x2;
    x2 := (i + 2) * intscale shr 16;
  end;
  dymin := Ceil(yscale - 1);
  dymax := trunc(yscale + 1);
  SetLength(weights, dxmax - dxmin + 1, dymax - dymin + 1);
  for i := 0 to dxmax - dxmin do
  begin
    fact := 1 / (dxmin + i);
    for j := 0 to dymax - dymin do
      weights[i, j] := round(fact / (dymin + j) * $10000);
  end;
end;

procedure ResizeBitmap(ASrc, ADest: TBitmap; AWidth, AHeight: Integer);
var
  xscale, yscale: Single;
  x1: Integer;
  ix, iy: Integer;
  totalRed, totalGreen, totalBlue: integer;
  ratio: integer;
  x, y, y0: Integer;
  r1, r2: TRect;
  x3: integer;
  RowDest, RowSource: PRGB32Array;
  weights: TDeltaArray;
  xsteps, ysteps: TIntArray;
  w, h, dxmin, dymin: Integer;
  dx, dy: Integer;
  rsrc, rdest: TRawImage;
begin
  ASrc.PixelFormat := pf32bit; //to be on the safe side
  ADest.PixelFormat := pf32bit;
  ADest.SetSize(AWidth, AHeight);
  if (ADest.Width >= ASrc.Width) or (ADest.Height >= ASrc.Height) then
  begin  //we don't do upsampling
    r1 := Rect(0, 0, ASrc.Width, ASrc.Height);
    r2 := r1;
    OffsetRect(r2, (ADest.Width - ASrc.Width) div 2,
      (ADest.Height - ASrc.Height) div 2);
    ADest.Canvas.CopyRect(r2, ASrc.Canvas, r1);
    Exit;
  end;
  w := ADest.Width;
  h := ADest.Height;
  xscale := ASrc.Width / w;
  yscale := ASrc.Height / h; // turns div into mults
  MakeStepsAndWeights(xscale, yscale, w, h, dxmin, dymin, weights, xsteps, ysteps);
  // Make 3 lookup tables for the steps and the ratios

  w := w - 1;
  h := h - 1;

  ADest.BeginUpdate;
  try
    rsrc := ASrc.RawImage;
    rdest := ADest.RawImage;
    y0 := 0;
    for y := 0 to h do
    begin
      RowDest := PRGB32Array(rdest.GetLineStart(y));
      dy := ysteps[y];
      x1 := 0;
      x3 := 0;
      for x := 0 to w do
      begin
        dx := xsteps[x];
        totalRed := 0;
        totalGreen := 0;
        totalBlue := 0;
        for iy := 1 to dy do
        begin
          RowSource := PRGB32Array(rsrc.GetLineStart(y0 + y + iy - 1));
          for ix := 1 to dx do
          begin
            totalRed := totalRed + RowSource[x1 + ix - 1].r;
            totalGreen := totalGreen + RowSource[x1 + ix - 1].g;
            totalBlue := totalBlue + RowSource[x1 + ix - 1].b; //maybe add the alpha-channel optionally
          end;
        end;
        ratio := weights[dx - dxmin, dy - dymin];
        RowDest[x3].r := (totalRed * ratio) shr 16;  //"rounding"
        RowDest[x3].g := (totalGreen * ratio) shr 16;
        RowDest[x3].b := (totalBlue * ratio) shr 16;
        x1 := x1 + dx;
        x3 := x3 + 1;
      end;
      y0 := y0 + dy - 1;
    end;
  finally
    ADest.EndUpdate;
  end;
  //SharpenMod(Work, Dest, min(1 + 0.4 * (xscale - 1), 2.5));
  //The sharpening makes the thumb look nicer, but is omitted here
end;

procedure ResizeBitmap2(ASrc, ADest: TBitmap; AWidth, AHeight: Integer);
var
  xini, xfi, yini, yfi, saltx, salty: Single;
  x, y, px, py, tpix: Integer;
  PixelColor: TRGB32;
  r, g, b: Longint;
  rsrc, rdest: TRawImage;
  ps, pd: PRGB32Array;
begin
  ADest.SetSize(AWidth, AHeight);
  saltx := ASrc.Width / AWidth;
  salty := ASrc.Height / AHeight;
  yfi := 0;
  ADest.BeginUpdate;
  try
    rsrc := ASrc.RawImage;
    rdest := ADest.RawImage;
    for y := 0 to AHeight - 1 do
    begin
      pd := PRGB32Array(rdest.GetLineStart(y));
      yini := yfi;
      yfi  := yini + salty;
      if yfi >= ASrc.Height then yfi := ASrc.Height - 1;
      xfi := 0;
      for x := 0 to AWidth - 1 do
      begin
        xini := xfi;
        xfi  := xini + saltx;
        if xfi >= ASrc.Width then xfi := ASrc.Width - 1;
        r := 0;
        g := 0;
        b := 0;
        tpix := 0;
        for py := MyRound(yini) to MyRound(yfi) do
        begin
          ps := PRGB32Array(rsrc.GetLineStart(py));
          for px := MyRound(xini) to MyRound(xfi) do
          begin
            Inc(tpix);
            PixelColor := ps[px];
            r := r + PixelColor.R;
            g := g + PixelColor.G;
            b := b + PixelColor.B;
          end;
        end;
        pd[x].Create(
          MyRound(MyDiv(r, tpix)),
          MyRound(MyDiv(g, tpix)),
          MyRound(MyDiv(b, tpix))
        );
      end;
    end;
  finally
    ADest.EndUpdate;
  end;
end;

{ TRGB32 }

constructor TRGB32.Create(AR, AG, AB: Byte; Alpha: Byte);
begin
  R := AR;
  G := AG;
  B := AB;
  A := Alpha;
end;

{ TStringListHelper }

procedure TStringListHelper.AddMessage(AText: string);
begin
  if Count >= TXPConfig.MaxMessageLine then Delete(Count - 1);
  Add(AText);
end;

end.
