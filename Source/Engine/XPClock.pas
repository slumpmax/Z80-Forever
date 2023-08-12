unit XPClock;

{$MODE Delphi}

interface

const
  clk_Japan     = 0;
  clk_US        = 1;
  clk_Inter     = 2;
  clk_UK        = 3;
  clk_France    = 4;
  clk_Germany   = 5;
  clk_Italy     = 6;
  clk_Spain     = 7;
  clk_Arab      = 8;
  clk_Korea     = 9;
  clk_USSR      = 10;
  clk_Undefined = 11;

type
  TXPClockBlock = array[0..12] of Byte;
  TXPClockRegs = packed record
  private
    FBlocks: array[0..3] of TXPClockBlock;
    FRegs: array[13..15] of Byte;
    function GetRegs(AIndex: Integer): Byte;
    procedure SetRegs(AIndex: Integer; Value: Byte);
    function GetWidth: Byte;
    procedure SetWidth(const Value: Byte);
    function GetValues(AIndex: Integer): Byte;
    procedure SetValues(AIndex: Integer; AValue: Byte);
  public
    procedure Clear;
    procedure Reset;
    property Regs[AIndex: Integer]: Byte read GetRegs write SetRegs;
    property Values[AIndex: Integer]: Byte read GetValues write SetValues; default;
    property BlockNumber: Byte index $0CD read GetRegs write SetRegs;
    property XAdjust: Byte index $2001 read GetRegs write SetRegs;
    property YAdjust: Byte index $2002 read GetRegs write SetRegs;
    property Screen1: Byte index $20E3 read GetRegs write SetRegs;
    property Interlaced: Byte index $21D3 read GetRegs write SetRegs;
    property Alternated: Byte index $22B3 read GetRegs write SetRegs;
    property Width: Byte read GetWidth write SetWidth;
    property TextColor: Byte index $2006 read GetRegs write SetRegs;
    property BGColor: Byte index $2007 read GetRegs write SetRegs;
    property BorderColor: Byte index $2008 read GetRegs write SetRegs;
    property KeyOn: Byte index $20E9 read GetRegs write SetRegs;
    property KeyClick: Byte index $21D9 read GetRegs write SetRegs;
    property NonMSXPrinter: Byte index $22B9 read GetRegs write SetRegs;
    property Baud2400: Byte index $2379 read GetRegs write SetRegs;
    property BeepVolume: Byte index $20CA read GetRegs write SetRegs;
    property BeepTimbre: Byte index $223A read GetRegs write SetRegs;
    property StartupScreenColor: Byte index $20CB read GetRegs write SetRegs;
    property Area: Byte index $200C read GetRegs write SetRegs;
  end;

  TXPClock = class
  private
    FRegs: TXPClockRegs;
    FRegIndex: Byte;
  public
    constructor Create;
    procedure Clear;
    procedure Reset;
    function ReadPort(APort: Byte): Byte;
    procedure WritePort(APort, AData: Byte);
  end;

implementation

{ TXPClockRegs }

procedure TXPClockRegs.Clear;
begin
  Reset;
end;

function TXPClockRegs.GetRegs(AIndex: Integer): Byte;
var
  nr, nb, shift, mask: Integer;
begin
  shift := (AIndex shr 8) and 3;
  mask := ((AIndex shr 4) and 15) xor 15;
  nr := AIndex and 15;
  if nr < 13 then
  begin
    nb := (AIndex shr 12) and 3;
    Result := (FBlocks[nb][nr] and mask) shr shift;
  end
  else Result := (FRegs[nr] and mask) shr shift;
end;

function TXPClockRegs.GetValues(AIndex: Integer): Byte;
begin
  Result := GetRegs((BlockNumber shl 12) or (AIndex and $FFF));
end;

function TXPClockRegs.GetWidth: Byte;
begin
  Result := FBlocks[2][4] or ((FBlocks[2][5] and 7) shl 4);
end;

procedure TXPClockRegs.Reset;
begin
  FBlocks[2][0] := 10;
  Area := clk_Inter;
  XAdjust := 0;
  YAdjust := 0;
  Screen1 := 0;
  Interlaced := 0;
  Alternated := 0;
  Width := 40;
  TextColor := 15;
  BGColor := 4;
  BorderColor := 4;
  KeyOn := 1;
  KeyClick := 1;
  NonMSXPrinter := 0;
  Baud2400 := 1;
  BeepVolume := 3;
  BeepTimbre := 3;
  StartupScreenColor := 4;
end;

procedure TXPClockRegs.SetRegs(AIndex: Integer; Value: Byte);
var
  nb, nr, shift, mask: Integer;
begin
  shift := (AIndex shr 8) and 3;
  mask := (AIndex shr 4) and 15;
  nr := AIndex and 15;
  if nr < 13 then
  begin
    nb := (AIndex shr 12) and 3;
    FBlocks[nb][nr] := (FBlocks[nb][nr] and mask) or ((Value shl shift) and (mask xor 15));
  end
  else FRegs[nr] := (FRegs[nr] and mask) or ((Value shl shift) and (mask xor 15));
end;

procedure TXPClockRegs.SetValues(AIndex: Integer; AValue: Byte);
begin
  SetRegs((BlockNumber shr 12) or (AIndex and $FFF), AValue);
end;

procedure TXPClockRegs.SetWidth(const Value: Byte);
begin
  FBlocks[2][4] := Value and 15;
  FBlocks[2][5] := (Value shr 4) and 7;
end;

{ TXPClock }

procedure TXPClock.Clear;
begin
  FRegs.Clear;
end;

constructor TXPClock.Create;
begin
  FRegIndex := 0;
end;

function TXPClock.ReadPort(APort: Byte): Byte;
begin
  case APort of
    $B4: Result := FRegIndex;
    $B5: Result := FRegs[FRegIndex];
  else
    Result := $FF;
  end;
end;

procedure TXPClock.Reset;
begin
  FRegs.Reset;
end;

procedure TXPClock.WritePort(APort, AData: Byte);
begin
  case APort of
    $B4: FRegIndex := AData and 15;
    $B5: FRegs[FRegIndex] := AData and 15;
  end;
end;

end.
