unit XPSlot;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, Math;

type
  PXPDebugMemory = ^TXPDebugMemory;
  TXPDebugMemory = array[0..16383] of Byte;

  TXPDebugSlot = class abstract (TInterfacedObject, IInterface)
  private
    FWrited, FReadOnly: Boolean;
    FMappers: array[0..3] of Integer;
    function GetMappers(ASegment: Integer): Integer;
    procedure SetMappers(ASegment: Integer; AValue: Integer);
  protected
    function GetMapReadOnly(ASegment: Integer): Boolean; virtual;
    procedure SetMapReadOnly(ASegment: Integer; AReadOnly: Boolean); virtual;
    function GetMemory(AAddress: Word): Byte; virtual;
    procedure SetMemory(AAddress: Word; AValue: Byte); virtual;
  public
    constructor Create;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property MapReadOnly[ASegment: Integer]: Boolean read GetMapReadOnly write SetMapReadOnly;
    property Mappers[ASegment: Integer]: Integer read GetMappers write SetMappers; default;
    property Memory[AAddress: Word]: Byte read GetMemory write SetMemory;
    property Writed: Boolean read FWrited;
  end;

  TXPDebugSlotMemory = class(TXPDebugSlot, IInterface)
  private
    FMemory: array of TXPDebugMemory;
  protected
    function GetMemory(AAddress: Word): Byte; override;
    procedure SetMemory(AAddress: Word; AValue: Byte); override;
  public
    constructor Create(ASize: Integer; AReadOnly: Boolean);
    destructor Destroy; override;
    procedure Bload(AFileName: string; ASlot: Integer);
    procedure LoadFromResource(APage: Integer; AName: string; AType: PChar); overload;
    procedure LoadFromResource(APage: Integer; AName, AType: string); overload;
  end;

  TXPDebugSlotMapper = class(TXPDebugSlot, IInterface)
  private
    FSlots: array[0..3] of TXPDebugSlot;
    FSecondary: Boolean;
    function GetSlots(AIndex: Integer): TXPDebugSlot;
    procedure SetSlots(AIndex: Integer; AValue: TXPDebugSlot);
    function GetValue: Byte;
    procedure SetValue(AValue: Byte);
    function GetMapSlot(ASegment: Integer): TXPDebugSlot;
    function GetText(ASegment: Integer): string;
  protected
    function GetMemory(AAddress: Word): Byte; override;
    procedure SetMemory(AAddress: Word; AValue: Byte); override;
    function GetMapReadOnly(ASegment: Integer): Boolean; override;
    procedure SetMapReadOnly(ASegment: Integer; AReadOnly: Boolean); override;
  public
    constructor Create(ASecondary: Boolean);
    destructor Destroy; override;
    procedure Clear;
    property Secondary: Boolean read FSecondary;
    property Slots[AIndex: Integer]: TXPDebugSlot read GetSlots write SetSlots;
    property MapSlot[ASegment: Integer]: TXPDebugSlot read GetMapSlot;
    property Value: Byte read GetValue write SetValue;
    property Text[ASegment: Integer]: string read GetText;
  end;

implementation

{ TXPDebugSlotMemory }

procedure TXPDebugSlotMemory.Bload(AFileName: string; ASlot: Integer);
var
  fs: TFileStream;
  n1, n2, n3: Word;
  c: Byte;
begin
  c := 0;
  n1 := 0;
  n2 := 0;
  n3 := 0;
  fs := TFileStream.Create(AFileName, fmOpenRead);
  try
    fs.Read(c, 1);
    case c of
      $FE:
      begin
        fs.Read(n1, 2);
        fs.Read(n2, 2);
        fs.Read(n3, 2);
        fs.Read(FMemory[ASlot][n1 and $3FFF], Min(16384, n2 - n1 + 1));
      end;
      $FF:
      begin
        FMemory[ASlot][0] := 0;
        fs.Read(FMemory[ASlot][1], Min(16383, fs.Size));
      end;
    else
      FMemory[ASlot][0] := c;
      fs.Read(FMemory[ASlot][1], Min(16383, fs.Size));
    end;
  finally
    fs.Free;
  end;
end;

constructor TXPDebugSlotMemory.Create(ASize: Integer; AReadOnly: Boolean);
begin
  inherited Create;
  SetLength(FMemory, ASize);
  FReadOnly := AReadOnly;
end;

destructor TXPDebugSlotMemory.Destroy;
begin
  FMemory := nil;
  inherited Destroy;
end;

function TXPDebugSlotMemory.GetMemory(AAddress: Word): Byte;
var
  page: Integer;
begin
  page := (FMappers[AAddress shr 14] and $FFFF) mod Length(FMemory);
  Result := FMemory[page][AAddress and $3FFF];
end;

procedure TXPDebugSlotMemory.LoadFromResource(APage: Integer; AName: string;
  AType: PChar);
var
  rs: TResourceStream;
begin
  rs := TResourceStream.Create(hInstance, AName, AType);
  try
    rs.Read(FMemory[APage][0], 16384);
  finally
    rs.Free;
  end;
end;

procedure TXPDebugSlotMemory.LoadFromResource(APage: Integer; AName, AType: string);
begin
  LoadFromResource(APage, AName, PChar(AType));
end;

procedure TXPDebugSlotMemory.SetMemory(AAddress: Word; AValue: Byte);
var
  page: Integer;
begin
  FWrited := not FReadOnly;
  if FWrited then
  begin
    page := (FMappers[AAddress shr 14] and $FFFF) mod Length(FMemory);
    FMemory[page][AAddress and $3FFF] := AValue;
  end;
end;

{ TXPDebugSlotMapper }

procedure TXPDebugSlotMapper.Clear;
var
  n: Integer;
begin
  for n := 0 to 3 do Slots[n] := nil;
end;

constructor TXPDebugSlotMapper.Create(ASecondary: Boolean);
begin
  inherited Create;
  FSecondary := ASecondary;
  Clear;
end;

destructor TXPDebugSlotMapper.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TXPDebugSlotMapper.GetMapReadOnly(ASegment: Integer): Boolean;
var
  slot: TXPDebugSlot;
begin
  slot := MapSlot[ASegment];
  if slot <> nil then
    Result := slot.MapReadOnly[ASegment]
  else Result := FReadOnly;
end;

function TXPDebugSlotMapper.GetMapSlot(ASegment: Integer): TXPDebugSlot;
var
  n: Integer;
begin
  n := Mappers[ASegment];
  if (n >= 0) and (n < 4) then
    Result := FSlots[n]
  else Result := nil;
end;

function TXPDebugSlotMapper.GetSlots(AIndex: Integer): TXPDebugSlot;
begin
  if (AIndex >= 0) and (AIndex < 4) then
    Result := FSlots[AIndex]
  else Result := nil;
end;

function TXPDebugSlotMapper.GetText(ASegment: Integer): string;
var
  slot: TXPDebugSlot;
  n: Integer;
begin
  n := Mappers[ASegment];
  if (n >= 0) and (n < 4) then
    Result := IntToStr(n)
  else Result := 'X';
  slot := MapSlot[ASegment];
  if slot is TXPDebugSlotMapper then Result := Result + '-' + TXPDebugSlotMapper(slot).Text[ASegment];
end;

function TXPDebugSlotMapper.GetValue: Byte;
begin
  Result := (FMappers[0] and 3) or ((FMappers[1] and 3) shl 2)
    or ((FMappers[2] and 3) shl 4) or ((FMappers[3] and 3) shl 6);
end;

function TXPDebugSlotMapper.GetMemory(AAddress: Word): Byte;
var
  slot: TXPDebugSlot;
begin
  if FSecondary and (AAddress = $FFFF) then
    Result := Value xor 255
  else
  begin
    slot := MapSlot[AAddress shr 14];
    if slot <> nil then
      Result := slot.Memory[AAddress]
    else Result := $FF;
  end;
end;

procedure TXPDebugSlotMapper.SetMemory(AAddress: Word; AValue: Byte);
var
  slot: TXPDebugSlot;
begin
  FReadOnly := False;
  if FSecondary and (AAddress = $FFFF) then
  begin
    Value := AValue;
    FWrited := True;
  end
  else
  begin
    slot := MapSlot[AAddress shr 14];
    if slot <> nil then
    begin
      slot.Memory[AAddress] := AValue;
      FWrited := slot.FWrited;
      FReadOnly := slot.FReadOnly;
    end
    else FWrited := False;
  end;
end;

procedure TXPDebugSlotMapper.SetMapReadOnly(ASegment: Integer; AReadOnly: Boolean);
var
  slot: TXPDebugSlot;
begin
  slot := FSlots[ASegment];
  if slot <> nil then
    slot.MapReadOnly[ASegment] := AReadOnly
  else FReadOnly := AReadOnly;
end;

procedure TXPDebugSlotMapper.SetSlots(AIndex: Integer;
  AValue: TXPDebugSlot);
begin
  if (AIndex >= 0) and (AIndex < 4) then FSlots[AIndex] := AValue;
end;

procedure TXPDebugSlotMapper.SetValue(AValue: Byte);
begin
  FMappers[0] := AValue and 3;
  FMappers[1] := (AValue shr 2) and 3;
  FMappers[2] := (AValue shr 4) and 3;
  FMappers[3] := (AValue shr 6) and 3;
end;

{ TXPDebugSlot }

constructor TXPDebugSlot.Create;
var
  n: Integer;
begin
  inherited Create;
  for n := 0 to 3 do FMappers[n] := -1;
  FWrited := False;
  FReadOnly := False;
end;

function TXPDebugSlot.GetMappers(ASegment: Integer): Integer;
begin
  if (ASegment >= 0) and (ASegment < 4) then
    Result := FMappers[ASegment]
  else Result := -1;
end;

function TXPDebugSlot.GetMemory(AAddress: Word): Byte;
begin
  Result := $FF;
end;

function TXPDebugSlot.GetMapReadOnly(ASegment: Integer): Boolean;
begin
  Result := FReadOnly;
end;

procedure TXPDebugSlot.SetMappers(ASegment: Integer; AValue: Integer);
begin
  if (ASegment >= 0) and (ASegment < 4) then FMappers[ASegment] := AValue;
end;

procedure TXPDebugSlot.SetMemory(AAddress: Word; AValue: Byte);
begin
  FWrited := False;
end;

procedure TXPDebugSlot.SetMapReadOnly(ASegment: Integer; AReadOnly: Boolean);
begin
  FReadOnly := AReadOnly;
end;

end.
