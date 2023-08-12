unit XPDrive;

{$MODE Delphi}

interface

uses
  XPSlot, Classes, SysUtils, LCLType;

type
  TXPDriveReg = packed record
  case Integer of
    0: (Reg: array[0..7] of Byte);
    1: (Control, Track, Sector, Data, Side, Drive, Unused, Request: Byte);
  end;

  TXPDriveRegs = class
  private
    FReg: TXPDriveReg;
    function GetReg(AIndex: Integer): Byte;
    procedure SetReg(AIndex: Integer; AValue: Byte);
    function GetBool(AIndex: Integer): Boolean;
    procedure SetBool(AIndex: Integer; AValue: Boolean);
  public
    constructor Create;

    property Reg[AIndex: Integer]: Byte read GetReg write SetReg; default;
    property Busy: Boolean index $FE00 read GetBool write SetBool;
    property IndexMark: Boolean index $FD10 read GetBool write SetBool;
    property Track0: Boolean index $FB20 read GetBool write SetBool;
    property CRCError: Boolean index $F730 read GetBool write SetBool;
    property SeekError: Boolean index $EF40 read GetBool write SetBool;
    property HeadLoaded: Boolean index $DF50 read GetBool write SetBool;
    property Protected: Boolean index $BF60 read GetBool write SetBool;
    property NotReady: Boolean index $7F70 read GetBool write SetBool;

    property DateRequest: Boolean index $FD10 read GetBool write SetBool;
    property LostData: Boolean index $FB20 read GetBool write SetBool;
    property ErrorCode: Byte index $E730 read GetReg write SetReg;
    property Fault: Boolean index $DF50 read GetBool write SetBool;
    property DataMark: Boolean index $DF50 read GetBool write SetBool;
  end;

  TXPDebugSlotDrive = class(TXPDebugSlotMemory, IInterface)
  private
    FRegs: TXPDriveRegs;
  protected
    function GetMemory(AAddress: Word): Byte; override;
    procedure SetMemory(AAddress: Word; AValue: Byte); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    property Regs: TXPDriveRegs read FRegs;
  end;

implementation

{ TXPDebugSlotDrive }

constructor TXPDebugSlotDrive.Create;
begin
  inherited Create(4, True);
  FRegs := TXPDriveRegs.Create;
  Reset;
end;

destructor TXPDebugSlotDrive.Destroy;
begin
  FreeAndNil(FRegs);
  inherited Destroy;
end;

function TXPDebugSlotDrive.GetMemory(AAddress: Word): Byte;
begin
  case AAddress of
    $7FF8..$7FFF: Result := FRegs[AAddress - $7FF8];
  else
    Result := inherited GetMemory(AAddress);
  end;
end;

procedure TXPDebugSlotDrive.Reset;
begin
  Mappers[0] := 0;
  Mappers[1] := 0;
  Mappers[2] := 0;
  Mappers[3] := 0;
  LoadFromResource(0, 'MSX2-DISK', RT_RCDATA);
end;

procedure TXPDebugSlotDrive.SetMemory(AAddress: Word; AValue: Byte);
begin
  case AAddress of
    $7FF8: ;
    $7FF9..$7FFC: FRegs[AAddress - $7FF8] := AValue;
    $7FFD: ;
    $7FFE: ;
    $7FFF: ;
  else
    inherited SetMemory(AAddress, AValue);
  end;
end;

{ TXPDriveRegs }

constructor TXPDriveRegs.Create;
begin
  FillChar(FReg, SizeOf(FReg), 0);
end;

function TXPDriveRegs.GetBool(AIndex: Integer): Boolean;
begin
  Result := GetReg(AIndex) <> 0;
end;

function TXPDriveRegs.GetReg(AIndex: Integer): Byte;
var
  shift, mask: Byte;
begin
  shift := (AIndex shr 4) and 15;
  mask := ((AIndex shr 8) and 255) xor 255;
  Result := (FReg.Reg[AIndex and 15] and mask) shr shift;
end;

procedure TXPDriveRegs.SetBool(AIndex: Integer; AValue: Boolean);
begin
  if AValue then
    SetReg(AIndex, 1)
  else SetReg(AIndex, 0);
end;

procedure TXPDriveRegs.SetReg(AIndex: Integer; AValue: Byte);
var
  shift, mask, b: Byte;
begin
  shift := (AIndex shr 4) and 15;
  mask := (AIndex shr 8) and 255;
  AIndex := AIndex and 15;
  b := FReg.Reg[AIndex];
  FReg.Reg[AIndex] := (b and mask) or ((AValue shl shift) and (mask xor 255));
end;

end.
