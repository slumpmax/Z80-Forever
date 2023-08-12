unit XPDebug;

{$MODE Delphi}

interface

uses
  XPConfig, XPAffect, XPVideo, XPClock, XPSlot, XPDrive,
  Classes, SysUtils, Math, LCLType;

type
  TXPDebugFlag = packed record
  private
    FFlag: Byte;
    function GetBit(const Index: Integer): Integer;
    procedure SetBit(const Index: Integer; Value: Integer);
    function GetBool(const Index: Integer): Boolean;
    procedure SetBool(const Index: Integer; Value: Boolean);
  public
    property S: Boolean index $7F80 read GetBool write SetBool;
    property Z: Boolean index $BF40 read GetBool write SetBool;
    property X1: Boolean index $DF20 read GetBool write SetBool;
    property H: Boolean index $EF10 read GetBool write SetBool;
    property X2: Boolean index $F708 read GetBool write SetBool;
    property PV: Boolean index $FB04 read GetBool write SetBool;
    property N: Boolean index $FD02 read GetBool write SetBool;
    property C: Boolean index $FE01 read GetBool write SetBool;
    property BitS: Integer index $7F80 read GetBit write SetBit;
    property BitZ: Integer index $BF40 read GetBit write SetBit;
    property BitX1: Integer index $DF20 read GetBit write SetBit;
    property BitH: Integer index $EF10 read GetBit write SetBit;
    property BitX2: Integer index $F708 read GetBit write SetBit;
    property BitPV: Integer index $FB04 read GetBit write SetBit;
    property BitN: Integer index $FD02 read GetBit write SetBit;
    property BitC: Integer index $FE01 read GetBit write SetBit;
    property Value: Byte read FFlag write FFlag;
  end;

  TXPDebugReg = packed record
  case Integer of
    0: (AF, BC, DE, HL, AF_, BC_, DE_, HL_, IX, IY, SP, PC, IR: Word);
    1: (
      F: TXPDebugFlag;
      A, C, B, E, D, L, H: Byte;
      F_: TXPDebugFlag;
      A_, C_, B_, E_, D_, L_, H_: Byte;
      IXL, IXH, IYL, IYH, SPL, SPH, PCL, PCH, R, I: Byte
    );
  end;

  TXPDebugRegs = class
  private
    FReg: TXPDebugReg;
    FAffected: TXPAffected;
    function GetF: Byte;
    function GetF_: Byte;
    function GetCF: Boolean;
    function GetHF: Boolean;
    function GetNF: Boolean;
    function GetPVF: Boolean;
    function GetSF: Boolean;
    function GetXF1: Boolean;
    function GetXF2: Boolean;
    function GetZF: Boolean;
    function GetCBit: Byte;
    function GetHBit: Byte;
    function GetNBit: Byte;
    function GetPVBit: Byte;
    function GetSBit: Byte;
    function GetX1Bit: Byte;
    function GetX2Bit: Byte;
    function GetZBit: Byte;
    procedure SetAF(const Value: Word);
    procedure SetAF_(const Value: Word);
    procedure SetBC(const Value: Word);
    procedure SetBC_(const Value: Word);
    procedure SetDE(const Value: Word);
    procedure SetDE_(const Value: Word);
    procedure SetHL(const Value: Word);
    procedure SetHL_(const Value: Word);
    procedure SetIX(const Value: Word);
    procedure SetIY(const Value: Word);
    procedure SetPC(const Value: Word);
    procedure SetSP(const Value: Word);
    procedure SetA(const Value: Byte);
    procedure SetA_(const Value: Byte);
    procedure SetB(const Value: Byte);
    procedure SetB_(const Value: Byte);
    procedure SetC(const Value: Byte);
    procedure SetC_(const Value: Byte);
    procedure SetD(const Value: Byte);
    procedure SetD_(const Value: Byte);
    procedure SetE(const Value: Byte);
    procedure SetE_(const Value: Byte);
    procedure SetF(const Value: Byte);
    procedure SetF_(const Value: Byte);
    procedure SetH(const Value: Byte);
    procedure SetH_(const Value: Byte);
    procedure SetI(const Value: Byte);
    procedure SetL(const Value: Byte);
    procedure SetL_(const Value: Byte);
    procedure SetR(const Value: Byte);
    procedure SetIXH(const Value: Byte);
    procedure SetIXL(const Value: Byte);
    procedure SetIYH(const Value: Byte);
    procedure SetIYL(const Value: Byte);
    procedure SetCF(const Value: Boolean);
    procedure SetHF(const Value: Boolean);
    procedure SetNF(const Value: Boolean);
    procedure SetPVF(const Value: Boolean);
    procedure SetSF(const Value: Boolean);
    procedure SetXF1(const Value: Boolean);
    procedure SetXF2(const Value: Boolean);
    procedure SetZF(const Value: Boolean);
    procedure SetCBit(const Value: Byte);
    procedure SetHBit(const Value: Byte);
    procedure SetNBit(const Value: Byte);
    procedure SetPVBit(const Value: Byte);
    procedure SetSBit(const Value: Byte);
    procedure SetX1Bit(const Value: Byte);
    procedure SetX2Bit(const Value: Byte);
    procedure SetZBit(const Value: Byte);
    function GetCBit_: Byte;
    function GetCF_: Boolean;
    function GetHBit_: Byte;
    function GetHF_: Boolean;
    function GetNBit_: Byte;
    function GetNF_: Boolean;
    function GetPVBit_: Byte;
    function GetPVF_: Boolean;
    function GetSBit_: Byte;
    function GetSF_: Boolean;
    function GetX1Bit_: Byte;
    function GetX2Bit_: Byte;
    function GetXF1_: Boolean;
    function GetXF2_: Boolean;
    function GetZBit_: Byte;
    function GetZF_: Boolean;
    procedure SetCBit_(const Value: Byte);
    procedure SetCF_(const Value: Boolean);
    procedure SetHBit_(const Value: Byte);
    procedure SetHF_(const Value: Boolean);
    procedure SetNBit_(const Value: Byte);
    procedure SetNF_(const Value: Boolean);
    procedure SetPVBit_(const Value: Byte);
    procedure SetPVF_(const Value: Boolean);
    procedure SetSBit_(const Value: Byte);
    procedure SetSF_(const Value: Boolean);
    procedure SetX1Bit_(const Value: Byte);
    procedure SetX2Bit_(const Value: Byte);
    procedure SetXF1_(const Value: Boolean);
    procedure SetXF2_(const Value: Boolean);
    procedure SetZBit_(const Value: Byte);
    procedure SetZF_(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(ARegs: TXPDebugRegs);
    property Affected: TXPAffected read FAffected;
    property AF: Word read FReg.AF write SetAF;
    property BC: Word read FReg.BC write SetBC;
    property DE: Word read FReg.DE write SetDE;
    property HL: Word read FReg.HL write SetHL;
    property AF_: Word read FReg.AF_ write SetAF_;
    property BC_: Word read FReg.BC_ write SetBC_;
    property DE_: Word read FReg.DE_ write SetDE_;
    property HL_: Word read FReg.HL_ write SetHL_;
    property IX: Word read FReg.IX write SetIX;
    property IY: Word read FReg.IY write SetIY;
    property PC: Word read FReg.PC write SetPC;
    property SP: Word read FReg.SP write SetSP;

    property A: Byte read FReg.A write SetA;
    property B: Byte read FReg.B write SetB;
    property C: Byte read FReg.C write SetC;
    property D: Byte read FReg.D write SetD;
    property E: Byte read FReg.E write SetE;
    property F: Byte read GetF write SetF;
    property H: Byte read FReg.H write SetH;
    property L: Byte read FReg.L write SetL;
    property I: Byte read FReg.I write SetI;
    property R: Byte read FReg.R write SetR;
    property A_: Byte read FReg.A_ write SetA_;
    property B_: Byte read FReg.B_ write SetB_;
    property C_: Byte read FReg.C_ write SetC_;
    property D_: Byte read FReg.D_ write SetD_;
    property E_: Byte read FReg.E_ write SetE_;
    property F_: Byte read GetF_ write SetF_;
    property H_: Byte read FReg.H_ write SetH_;
    property L_: Byte read FReg.L_ write SetL_;
    property IXH: Byte read FReg.IXH write SetIXH;
    property IXL: Byte read FReg.IXL write SetIXL;
    property IYH: Byte read FReg.IYH write SetIYH;
    property IYL: Byte read FReg.IYL write SetIYL;

    property SF: Boolean read GetSF write SetSF;
    property ZF: Boolean read GetZF write SetZF;
    property XF1: Boolean read GetXF1 write SetXF1;
    property HF: Boolean read GetHF write SetHF;
    property XF2: Boolean read GetXF2 write SetXF2;
    property PVF: Boolean read GetPVF write SetPVF;
    property NF: Boolean read GetNF write SetNF;
    property CF: Boolean read GetCF write SetCF;

    property SF_: Boolean read GetSF_ write SetSF_;
    property ZF_: Boolean read GetZF_ write SetZF_;
    property XF1_: Boolean read GetXF1_ write SetXF1_;
    property HF_: Boolean read GetHF_ write SetHF_;
    property XF2_: Boolean read GetXF2_ write SetXF2_;
    property PVF_: Boolean read GetPVF_ write SetPVF_;
    property NF_: Boolean read GetNF_ write SetNF_;
    property CF_: Boolean read GetCF_ write SetCF_;

    property SBit: Byte read GetSBit write SetSBit;
    property ZBit: Byte read GetZBit write SetZBit;
    property X1Bit: Byte read GetX1Bit write SetX1Bit;
    property HBit: Byte read GetHBit write SetHBit;
    property X2Bit: Byte read GetX2Bit write SetX2Bit;
    property PVBit: Byte read GetPVBit write SetPVBit;
    property NBit: Byte read GetNBit write SetNBit;
    property CBit: Byte read GetCBit write SetCBit;

    property SBit_: Byte read GetSBit_ write SetSBit_;
    property ZBit_: Byte read GetZBit_ write SetZBit_;
    property X1Bit_: Byte read GetX1Bit_ write SetX1Bit_;
    property HBit_: Byte read GetHBit_ write SetHBit_;
    property X2Bit_: Byte read GetX2Bit_ write SetX2Bit_;
    property PVBit_: Byte read GetPVBit_ write SetPVBit_;
    property NBit_: Byte read GetNBit_ write SetNBit_;
    property CBit_: Byte read GetCBit_ write SetCBit_;
  end;

  TXPDebugLine = class
  private
    FAddress, FCycle: Integer;
    FCodes: TArrayOfByte;
    FText: string;
    FBreaked: Boolean;
  public
    constructor Create(AAddress: Integer); overload;
    constructor Create(AAddress: Integer; ACodes: array of Byte; AText: string;
      ACycle: Integer); overload;
    destructor Destroy; override;
    procedure AddCode(ACode: Byte); overload;
    procedure AddCode(ACodes: array of Byte); overload;
    procedure SetText(ACode, AParam: string);
    property Address: Integer read FAddress;
    property Codes: TArrayOfByte read FCodes;
    property Text: string read FText;
    property Cycle: Integer read FCycle;
    property Breaked: Boolean read FBreaked write FBreaked;
  end;

  TXPDebugLines = class
  private
    FLines: TList;
    FSelectIndex, FSelectOffset: Integer;
    function GetLines(AIndex: Integer): TXPDebugLine;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Count: Integer;
    function Add(ALine: TXPDebugLine): Integer;
    function SelectedLine: TXPDebugLine;
    function SelectedAddress(AIndex: Integer): Integer;
    procedure AddLine(AAddress: Integer; ACodes: array of Byte; AText: string;
      ACycle: Integer);
    procedure UpdateSelectIndex(AAddress: Integer);
    property SelectIndex: Integer read FSelectIndex write FSelectIndex;
    property SelectOffset: Integer read FSelectOffset write FSelectOffset;
    property Lines[AIndex: Integer]: TXPDebugLine read GetLines; default;
  end;

  TXPBreakPoints = class
  private
    FPoints: array of Integer;
    function GetPoints(AAddress: Integer): Boolean;
    procedure SetPoints(AAddress: Integer; AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Count: Integer;
    procedure Assign(ABreakPoints: TXPBreakPoints);
    procedure Add(AAddress: Integer); overload;
    procedure Add(AAddresses: array of Integer); overload;
    procedure Delete(AAddress: Integer);
    function IndexOf(AAddress: Integer): Integer;
    property Points[AAddress: Integer]: Boolean read GetPoints write SetPoints; default;
  end;

  TXPDebugSystem = (sysNone, sysDOS, sysBASIC);
  TXPDebugThread = class;
  TXPDebug = class
  private
    FRegs, FPRegs: TXPDebugRegs;
    FLines, FSystemLines: TXPDebugLines;
    FMapper: TXPDebugSlotMapper;
    FMainRAM, FMainROM, FExtROM: TXPDebugSlotMemory;
    FDrive: TXPDebugSlotDrive;
    FVideo: TXPVideo;
    FClock: TXPClock;
    FErrors, FMessages: TStringList;
    FCodes: array of Byte;
    FBreakPoints: TXPBreakPoints;
    IFF1, IFF2, FExecuted, FCanPaint: Boolean;
    FIndexCode, FIntMode: Byte;
    FIndexOffset, FStartAddress, FStackAddress: Integer;
    FDecodeAddress, FLastTime, FReturnCount: Integer;
    FDecodeLine: TXPDebugLine;
    FThread: TXPDebugThread;
    FSystem: TXPDebugSystem;
    FOnStop: TNotifyEvent;
    FOnUpdate: TNotifyEvent;
    FOnPaint: TNotifyEvent;
    function GetReg8A(AIndex: Integer): Byte;
    function GetReg8B(AIndex: Integer): Byte;
    function GetReg16A(AIndex: Integer): Word;
    function GetReg16B(AIndex: Integer): Word;
    function GetReg16M(AIndex: Integer): Byte;
    procedure SetReg8A(AIndex: Integer; AValue: Byte);
    procedure SetReg8B(AIndex: Integer; AValue: Byte);
    procedure SetReg16A(AIndex: Integer; AValue: Word);
    procedure SetReg16B(AIndex: Integer; AValue: Word);
    procedure SetReg16M(AIndex: Integer; AValue: Byte);
    function FetchIndexOffset: Byte;
    function FetchDecodeIndexOffset: Byte;
    function NextAddress(ALine: TXPDebugLine): TArrayOfInteger;
    procedure SetSystem(AValue: TXPDebugSystem);
  public
    VRAM: array[0..131071] of Byte;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Reset;
    procedure AddMessage(AText: string);
    function FetchByte: Byte;
    function FetchWord: Word;
    function FetchShortAddress: Integer;
    function FetchDecodeByte: Byte;
    function FetchDecodeWord: Word;
    function FetchDecodeShortAddress: Integer;
    function ReadByte(AAddress: Integer): Byte;
    function ReadShortAddress(AAddress: Integer): Integer;
    function ReadWord(AAddress: Integer): Word;
    procedure WriteByte(AAddress: Integer; AValue: Byte; AUpdated: Boolean = False);
    procedure WriteWord(AAddress: Integer; AValue: Word; AUpdated: Boolean = False);
    procedure JumpShort(AValue: Integer; ACondition: Boolean = True);
    procedure Jump(AValue: Word; ACondition: Boolean = True);
    procedure Call(AValue: Word; ACondition: Boolean = True);
    procedure Ret(ACondition: Boolean = True);
    procedure Push(AValue: Word);
    function Pop: Word;
    function OpAdd8(n1, n2: Byte): Byte;
    function OpAdc8(n1, n2: Byte): Byte;
    function OpSub8(n1, n2: Byte): Byte;
    function OpSbc8(n1, n2: Byte): Byte;
    function OpAnd8(n1, n2: Byte): Byte;
    function OpXor8(n1, n2: Byte): Byte;
    function OpOr8(n1, n2: Byte): Byte;
    function OpCp8(n1, n2: Byte): Byte;
    function GetCycle(ACodes: array of Byte): Integer;
    function ReadPort(APort: Byte): Byte;
    procedure WritePort(APort, AValue: Byte; AUpdated: Boolean = False);
    procedure UpdateSelectIndexs;
    procedure StepInto(AUpdated: Boolean; AIndexCode: Byte = 0);
    procedure StepOver;
    procedure Run;
    procedure RunAndReturn;
    procedure RunTo(AAddress: Integer); overload;
    procedure RunTo(AAddresses: TArrayOfInteger); overload;
    procedure RunToLine(ALineIndex: Integer);
    procedure RunToSystemLine(ALineIndex: Integer);
    procedure Stop;
    function FormatCode(AText: string): string;
    function DecodeText(ACode, AParam: string): string;
    function DecodeLine(AAddress: Word): TXPDebugLine;
    procedure SystemDecode(AMaxLine: Integer = 0);
    property LastTime: Integer read FLastTime;
    property CanPaint: Boolean read FCanPaint;
    property StartAddress: Integer read FStartAddress write FStartAddress;
    property StackAddress: Integer read FStackAddress write FStackAddress;
    property Video: TXPVideo read FVideo write FVideo;
    property MainRAM: TXPDebugSlotMemory read FMainRAM;
    property Regs: TXPDebugRegs read FRegs;
    property PRegs: TXPDebugRegs read FPRegs;
    property Lines: TXPDebugLines read FLines;
    property SystemLines: TXPDebugLines read FSystemLines;
    property BreakPoints: TXPBreakPoints read FBreakPoints;
    property Mapper: TXPDebugSlotMapper read FMapper;
    property Errors: TStringList read FErrors;
    property Messages: TStringList read FMessages;
    property Executed: Boolean read FExecuted write FExecuted;
    property System: TXPDebugSystem read FSystem write SetSystem;
    property ReturnCount: Integer read FReturnCount write FReturnCount;
    property Reg8A[AIndex: Integer]: Byte read GetReg8A write SetReg8A;
    property Reg8B[AIndex: Integer]: Byte read GetReg8B write SetReg8B;
    property Reg16A[AIndex: Integer]: Word read GetReg16A write SetReg16A;
    property Reg16B[AIndex: Integer]: Word read GetReg16B write SetReg16B;
    property Reg16M[AIndex: Integer]: Byte read GetReg16M write SetReg16M;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  end;

  TXPDebugThread = class(TThread)
  strict private
    FBreakPoints: TXPBreakPoints;
  private
    FDebug: TXPDebug;
    procedure DoPaint;
    procedure DoStop;
    procedure DoUpdate;
    procedure SyncStop(Sender: TObject);
    procedure SetBreakPoints(AValue: TXPBreakPoints);
  public
    constructor Create(ADebug: TXPDebug);
    destructor Destroy; override;
    procedure Execute; override;
    property BreakPoints: TXPBreakPoints read FBreakPoints write SetBreakPoints;
  end;

implementation

{ TXPDebug }

procedure TXPDebug.AddMessage(AText: string);
begin
  FMessages.AddMessage(AText);
end;

procedure TXPDebug.Call(AValue: Word; ACondition: Boolean = True);
begin
  if ACondition then
  begin
    if FReturnCount >= 0 then Inc(FReturnCount);
    Push(Regs.PC);
    Jump(AValue);
  end;
end;

procedure TXPDebug.Clear;
begin
  FVideo.Clear;
  FClock.Clear;
  FLines.Clear;
  FSystemLines.Clear;
  FErrors.Clear;
  FMessages.Clear;
  FStartAddress := -1;
  FStackAddress := TXPConfig.DefaultStackAddress;
  FSystem := sysNone;
  Reset;
end;

constructor TXPDebug.Create;
begin
  FMainRAM := TXPDebugSlotMemory.Create(8, False);
  FMainROM := TXPDebugSlotMemory.Create(2, True);
  FExtROM := TXPDebugSlotMemory.Create(4, True);
  FDrive := TXPDebugSlotDrive.Create;
  FMapper := TXPDebugSlotMapper.Create(False);
  FLines := TXPDebugLines.Create;
  FSystemLines := TXPDebugLines.Create;
  FRegs := TXPDebugRegs.Create;
  FPRegs := TXPDebugRegs.Create;
  FErrors := TStringList.Create;
  FMessages := TStringList.Create;
  FVideo := TXPVideo.Create;
  FClock := TXPClock.Create;
  FBreakPoints := TXPBreakPoints.Create;
  FVideo.Messages := FMessages;
  FSystem := sysNone;
  FThread := nil;
  FCodes := nil;
  FOnStop := nil;
  FOnUpdate := nil;
  FOnPaint := nil;
  Clear;
end;

function TXPDebug.DecodeLine(AAddress: Word): TXPDebugLine;
const
  DReg8: array[0..7] of string = ('B', 'C', 'D', 'E', 'H', 'L', 'ii', 'A');
  DReg16A: array[0..3] of string = ('BC', 'DE', 'HL', 'SP');
  DReg16B: array[0..3] of string = ('BC', 'DE', 'HL', 'AF');
  DCodes: array[0..255] of string = (
    'NOP',        'LD BC,n2',  'LD (BC),A',  'INC BC', 'INC B',    'DEC B',    'LD B,n1',    'RLCA',
    'EX AF,AF''', 'ADD xy,BC', 'LD A,(BC)',  'DEC BC', 'INC C',    'DEC C',    'LD C,n1',    'RRCA',
    'DJNZ e',     'LD DE,n2',  'LD (DE),A',  'INC DE', 'INC D',    'DEC D',    'LD D,n1',    'RLA',
    'JR e',       'ADD xy,DE', 'LD A,(DE)',  'DEC DE', 'INC E',    'DEC E',    'LD E,n1',    'RRA',
    'JR NZ,e',    'LD xy,n2',  'LD (n2),xy', 'INC xy', 'INC h',    'DEC h',    'LD h,n1',    'DAA',
    'JR Z,e',     'ADD xy,xy', 'LD xy,(n2)', 'DEC xy', 'INC l',    'DEC l',    'LD l,n1',    'CPL',
    'JR NC,e',    'LD SP,n2',  'LD (n2),A',  'INC SP', 'INC (ii)', 'DEC (ii)', 'LD (ii),n1', 'SCF',
    'JR C,e',     'ADD xy,SP', 'LD A,(n2)',  'DEC SP', 'INC A',    'DEC A',    'LD A,n1',    'CCF',

    'LD B,B',    'LD B,C',    'LD B,D',    'LD B,E',    'LD B,h',    'LD B,l',    'LD B,(ii)',    'LD B,A',
    'LD C,B',    'LD C,C',    'LD C,D',    'LD C,E',    'LD C,h',    'LD C,l',    'LD C,(ii)',    'LD C,A',
    'LD D,B',    'LD D,C',    'LD D,D',    'LD D,E',    'LD D,h',    'LD D,l',    'LD D,(ii)',    'LD D,A',
    'LD E,B',    'LD E,C',    'LD E,D',    'LD E,E',    'LD E,h',    'LD E,l',    'LD E,(ii)',    'LD E,A',
    'LD h,B',    'LD h,C',    'LD h,D',    'LD h,E',    'LD h,h',    'LD h,l',    'LD h,(ii)',    'LD H,A',
    'LD l,B',    'LD l,C',    'LD l,D',    'LD l,E',    'LD l,h',    'LD l,l',    'LD l,(ii)',    'LD L,A',
    'LD (ii),B', 'LD (ii),C', 'LD (ii),D', 'LD (ii),E', 'LD (ii),H', 'LD (ii),L', 'HALT',         'LD (ii),A',
    'LD A,B',    'LD A,C',    'LD A,D',    'LD A,E',    'LD A,h',    'LD A,l',    'LD A,(ii)',    'LD A,A',

    'ADD A,B', 'ADD A,C', 'ADD A,D', 'ADD A,E', 'ADD A,h', 'ADD A,l', 'ADD A,(ii)', 'ADD A,A',
    'ADC A,B', 'ADC A,C', 'ADC A,D', 'ADC A,E', 'ADC A,h', 'ADC A,l', 'ADC A,(ii)', 'ADC A,A',
    'SUB B',   'SUB C',   'SUB D',   'SUB E',   'SUB h',   'SUB l',   'SUB (ii)',   'SUB A',
    'SBC A,B', 'SBC A,C', 'SBC A,D', 'SBC A,E', 'SBC A,h', 'SBC A,l', 'SBC A,(ii)', 'SBC A,A',
    'AND B',   'AND C',   'AND D',   'AND E',   'AND h',   'AND l',   'AND (ii)',   'AND A',
    'XOR B',   'XOR C',   'XOR D',   'XOR E',   'XOR h',   'XOR l',   'XOR (ii)',   'XOR A',
    'OR B',    'OR C',    'OR D',    'OR E',    'OR h',    'OR l',    'OR (ii)',    'OR A',
    'CP B',    'CP C',    'CP D',    'CP E',    'CP h',    'CP l',    'CP (ii)',    'CP A',

    'RET NZ', 'POP BC',   'JP NZ,n2', 'JP n2',      'CALL NZ,n2', 'PUSH BC', 'ADD A,n1', 'RST 00H',
    'RET Z',  'RET',      'JP Z,n2',  '',           'CALL Z,n2',  'CALL n2', 'ADC A,n1', 'RST 08H',
    'RET NC', 'POP DE',   'JP NC,n2', 'OUT (n1),A', 'CALL NC,n2', 'PUSH DE', 'SUB n1',   'RST 10H',
    'RET C',  'EXX',      'JP C,n2',  'IN A,(n1)',  'CALL C,n2',  '',        'SBC A,n1', 'RST 18H',
    'RET PO', 'POP xy',   'JP PO,n2', 'EX (SP),xy', 'CALL PO,n2', 'PUSH xy', 'AND n1',   'RST 20H',
    'RET PE', 'JP (xy)',  'JP PE,n2', 'EX DE,HL',   'CALL PE,n2', '',        'XOR n1',   'RST 28H',
    'RET P',  'POP AF',   'JP P,n2',  'DI',         'CALL P,n2',  'PUSH AF', 'OR n1',    'RST 30H',
    'RET M',  'LD SP,xy', 'JP M,n2',  'EI',         'CALL M,n2',  '',        'CP n1',    'RST 38H'
  );
  DCodeExts: array[0..255] of string = (
    '', '', '', '', '', '', '', '',
    '', '', '', '', '', '', '', '',
    '', '', '', '', '', '', '', '',
    '', '', '', '', '', '', '', '',
    '', '', '', '', '', '', '', '',
    '', '', '', '', '', '', '', '',
    '', '', '', '', '', '', '', '',
    '', '', '', '', '', '', '', '',

    'IN B,(C)', 'OUT (C),B', 'SBC HL,BC', 'LD (n2),BC', 'NEG', 'RETN', 'IM 0', 'LD I,A',
    'IN C,(C)', 'OUT (C),C', 'ADC HL,BC', 'LD BC,(n2)', '',    'RETI', '',     'LD R,A',
    'IN D,(C)', 'OUT (C),D', 'SBC HL,DE', 'LD (n2),DE', '',    '',     'IM 1', 'LD A,I',
    'IN E,(C)', 'OUT (C),E', 'ADC HL,DE', 'LD DE,(n2)', '',    '',     'IM 2', 'LD A,R',
    'IN H,(C)', 'OUT (C),H', 'SBC HL,HL', '',           '',    '',     '',     'RRD',
    'IN L,(C)', 'OUT (C),L', 'ADC HL,HL', '',           '',    '',     '',     'RLD',
    '',         '',          'SBC HL,SP', 'LD (n2),SP', '',    '',     '',     '',
    'IN A,(C)', 'OUT (C),A', 'ADC HL,SP', 'LD SP,(n2)', '',    '',     '',     '',

    '',     '',     '',     '',     '', '', '', '',
    '',     '',     '',     '',     '', '', '', '',
    '',     '',     '',     '',     '', '', '', '',
    '',     '',     '',     '',     '', '', '', '',
    'LDI',  'CPI',  'INI',  'OUTI', '', '', '', '',
    'LDD',  'CPD',  'IND',  'OUTD', '', '', '', '',
    'LDIR', 'CPIR', 'INIR', 'OTIR', '', '', '', '',
    'LDDR', 'CPDR', 'INDR', 'OTDR', '', '', '', '',

    '', '', '', '', '', '', '', '',
    '', '', '', '', '', '', '', '',
    '', '', '', '', '', '', '', '',
    '', '', '', '', '', '', '', '',
    '', '', '', '', '', '', '', '',
    '', '', '', '', '', '', '', '',
    '', '', '', '', '', '', '', '',
    '', '', '', '', '', '', '', ''
  );
  DCodeBits: array[0..7] of string = ('RLC', 'RRC', 'RL', 'RR', 'SLA', 'SRA', 'SLL', 'SRL');
var
  code, b, c: Byte;
  fetched: Boolean;
  s: string;

  function DREG16I: string;
  begin
    case FIndexCode of
      $DD: Result := 'IX';
      $FD: Result := 'IY';
    else
      Result := 'HL';
    end;
  end;

begin
  Result := TXPDebugLine.Create(AAddress);
  FDecodeLine := Result;
  FDecodeAddress := AAddress;
  FIndexCode := 0;
  repeat
    fetched := False;
    code := FetchDecodeByte;
    s := '';
    case code of
      $CB:
      begin
        FetchDecodeIndexOffset;
        code := FetchDecodeByte;
        b := code and 7;
        c := (code shr 3) and 7;
        case code of
          $00..$3F: s := Format('%s %s', [DCodeBits[c], DReg8[b]]);
          $40..$7F: s := Format('BIT %d,%s', [c, DReg8[b]]);
          $80..$BF: s := Format('RES %d,%s', [c, DReg8[b]]);
          $C0..$FF: s := Format('SET %d,%s', [c, DReg8[b]]);
        end;
        s := FormatCode(s);
      end;
      $DD, $FD:
      begin
        FIndexCode := code;
        FIndexOffset := -1;
        fetched := True;
      end;
      $ED:
      begin
        code := FetchDecodeByte;
        s := FormatCode(DCodeExts[code]);
      end;
    else
      s := FormatCode(DCodes[code]);
    end;
  until not fetched;
  Result.FText := s;
  Result.FCycle := GetCycle(Result.FCodes);
end;

function TXPDebug.DecodeText(ACode, AParam: string): string;
begin
  Result := GetTabStr(ACode) + AParam;
end;

destructor TXPDebug.Destroy;
begin
  FOnUpdate := nil;
  FOnPaint := nil;
  FOnStop := nil;
  if FThread <> nil then
  begin
    FThread.Terminate;
    FThread.WaitFor;
    FreeAndNil(FThread);
  end;
  FreeAndNil(FErrors);
  FreeAndNil(FMessages);
  FreeAndNil(FRegs);
  FreeAndNil(FPRegs);
  FreeAndNil(FLines);
  FreeAndNil(FSystemLines);
  FreeAndNil(FMapper);
  FreeAndNil(FClock);
  FreeAndNil(FVideo);
  FreeAndNil(FBreakPoints);
  FMainRAM := nil;
  FMainROM := nil;
  FExtROM := nil;
  FDrive := nil;
  FCodes := nil;
  inherited Destroy;
end;

function TXPDebug.FetchByte: Byte;
var
  n: Integer;
begin
  Result := ReadByte(Regs.PC);
  Regs.PC := Regs.PC + 1;
  n := Length(FCodes);
  SetLength(FCodes, n + 1);
  FCodes[n] := Result;
end;

function TXPDebug.FetchDecodeByte: Byte;
begin
  Result := ReadByte(FDecodeAddress);
  FDecodeAddress := FDecodeAddress + 1;
  FDecodeLine.AddCode(Result);
end;

function TXPDebug.FetchDecodeIndexOffset: Byte;
begin
  if FIndexCode <> 0 then
  begin
    if FIndexOffset < 0 then FIndexOffset := FetchDecodeByte;
    Result := FIndexOffset;
  end
  else Result := 0;
end;

function TXPDebug.FetchDecodeShortAddress: Integer;
begin
  Result := ReadByte(FDecodeAddress);
  FDecodeAddress := FDecodeAddress + 1;
  FDecodeLine.AddCode(Result);
  if Result > 127 then Result := Result - 256;
end;

function TXPDebug.FetchDecodeWord: Word;
begin
  Result := ReadWord(FDecodeAddress);
  FDecodeAddress := FDecodeAddress + 2;
  FDecodeLine.AddCode([Result and 255, Result shr 8]);
end;

function TXPDebug.FetchIndexOffset: Byte;
begin
  if FIndexCode <> 0 then
  begin
    if FIndexOffset < 0 then FIndexOffset := FetchByte;
    Result := FIndexOffset;
  end
  else Result := 0;
end;

function TXPDebug.FetchShortAddress: Integer;
begin
  Result := FetchByte;
  if Result > 127 then Result := Result - 256;
end;

function TXPDebug.FetchWord: Word;
begin
  Result := FetchByte or (FetchByte shl 8);
end;

function TXPDebug.FormatCode(AText: string): string;
const
  ParamStr: array[0..6] of string = ('n1', 'n2', 'e', 'xy', 'h', 'l', 'ii');
var
  n, p: Integer;
  s: string;
  c: Char;
begin
  for n := 0 to Length(ParamStr) - 1 do
  begin
    s := ParamStr[n];
    Result := '';
    while AText <> '' do
    begin
      p := Pos(s, AText);
      if p > 0 then
      begin
        Result := Result + Copy(AText, 1, p - 1);
        Delete(AText, 1, p + Length(s) - 1);
        case n of
          0: Result := Result + HexText(2, FetchDecodeByte); // n1
          1: Result := Result + HexText(4, FetchDecodeWord); // n2
          2: // e
          begin
            p := FetchDecodeShortAddress;
            Result := Result + HexText(4, (FDecodeAddress + p) and $FFFF);
          end;
          3: // xy
          case FIndexCode of
            $DD: Result := Result + 'IX';
            $FD: Result := Result + 'IY';
          else
            Result := Result + 'HL';
          end;
          4: // h
          case FIndexCode of
            $DD: Result := Result + 'IXH';
            $FD: Result := Result + 'IYH';
          else
            Result := Result + 'H';
          end;
          5: // l
          case FIndexCode of
            $DD: Result := Result + 'IXL';
            $FD: Result := Result + 'IYL';
          else
            Result := Result + 'L';
          end;
          6: // ii
          case FIndexCode of
            $DD, $FD:
            begin
              FetchDecodeIndexOffset;
              if FIndexCode = $DD then
                c := 'X'
              else c:= 'Y';
              Result := Result + Format('I%s + %d', [c, FIndexOffset]);
            end;
          else
            Result := Result + 'HL';
          end;
        end;
      end
      else
      begin
        Result := Result + AText;
        AText := '';
      end;
    end;
    AText := Result;
  end;
  p := Pos(' ', Result);
  if p > 0 then
  begin
    Result := Format('%-8.8s%s', [Copy(Result, 1, p - 1), Copy(Result, p, Length(Result) - p + 1)]);
  end;
end;

function TXPDebug.GetCycle(ACodes: array of Byte): Integer;
const
  TState: array[0..255] of Byte = (
    4,10, 7, 6, 4, 4, 7, 4,  4,11, 7, 6, 4, 4, 7, 4, // 00
   13,10, 7, 6, 4, 4, 7, 4, 12, 4, 7, 6, 4, 4, 7, 4, // 10
   12,10,16, 6, 4, 4, 7, 4, 12, 4,16, 6, 4, 4, 7, 4, // 20
   12,10,13, 6,11,11,10, 4, 12, 4,13, 6, 4, 4, 7, 4, // 30
    4, 4, 4, 4, 4, 4, 7, 4,  4, 4, 4, 4, 4, 4, 7, 4, // 40
    4, 4, 4, 4, 4, 4, 7, 4,  4, 4, 4, 4, 4, 4, 7, 4, // 50
    4, 4, 4, 4, 4, 4, 7, 4,  4, 4, 4, 4, 4, 4, 7, 4, // 60
    7, 7, 7, 7, 7, 7, 4, 7,  4, 4, 4, 4, 4, 4, 7, 4, // 70
    4, 4, 4, 4, 4, 4, 7, 4,  4, 4, 4, 4, 4, 4, 7, 4, // 80
    4, 4, 4, 4, 4, 4, 7, 4,  4, 4, 4, 4, 4, 4, 7, 4, // 90
    4, 4, 4, 4, 4, 4, 7, 4,  4, 4, 4, 4, 4, 4, 7, 4, // A0
    4, 4, 4, 4, 4, 4, 7, 4,  4, 4, 4, 4, 4, 4, 7, 4, // B0
   11,10,10,10,17,11, 7,11, 11,10,10, 4,17,17, 7,11, // C0
   11,10,10,11,17,11, 7,11, 11, 4,10,11,17, 4, 7,11, // D0
   11,10,10,19,17,11, 7,11, 11, 4,10, 4,17, 4, 7,11, // E0
   11,10,10, 4,17,11, 7,11, 11, 6,10, 4,17, 4, 7,11  // F0
  );
  TStateX: array[0..255] of Byte = ( // DD, FD
    0, 0, 0, 0, 0, 0, 0, 0,  0,15, 0, 0, 0, 0, 0, 0, // 00
    0, 0, 0, 0, 0, 0, 0, 0,  0,15, 0, 0, 0, 0, 0, 0, // 10
    0,14,20,10, 0, 0, 0, 0,  0,15,20,10, 0, 0, 0, 0, // 20
    0, 0, 0, 0,23,23,19, 0,  0,15, 0, 0, 0, 0, 0, 0, // 30
    0, 0, 0, 0, 0, 0,19, 0,  0, 0, 0, 0, 0, 0,19, 0, // 40
    0, 0, 0, 0, 0, 0,19, 0,  0, 0, 0, 0, 0, 0,19, 0, // 50
    0, 0, 0, 0, 0, 0,19, 0,  0, 0, 0, 0, 0, 0,19, 0, // 60
   19,19,19,19,19,19, 0,19,  0, 0, 0, 0, 0, 0,19, 0, // 70
    0, 0, 0, 0, 0, 0,19, 0,  0, 0, 0, 0, 0, 0,19, 0, // 80
    0, 0, 0, 0, 0, 0,19, 0,  0, 0, 0, 0, 0, 0,19, 0, // 90
    0, 0, 0, 0, 0, 0,19, 0,  0, 0, 0, 0, 0, 0,19, 0, // A0
    0, 0, 0, 0, 0, 0,19, 0,  0, 0, 0, 0, 0, 0,19, 0, // B0
    0,14, 0, 0, 0,15, 0, 0,  0, 0, 0, 8, 0, 0, 0, 0, // C0
    0,14, 0, 0, 0,15, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, // D0
    0,14, 0,23, 0,15, 0, 0,  0, 8, 0, 0, 0, 0, 0, 0, // E0
    0,14, 0, 0, 0,15, 0, 0,  0,10, 0, 0, 0, 0, 0, 0  // F0
  );
  TStateExt: array[0..255] of Byte = ( // ED
    0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, // 00
    0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, // 10
    0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, // 20
    0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, // 30
   12,12,15,20, 8,14, 8, 9, 12,12,15,20, 0,14, 0, 9, // 40
   12,12,15,20, 0, 0, 8, 9, 12,12,15,20, 0, 0, 8, 9, // 50
   12,12,15,20, 0, 0, 0,18, 12,12,15,20, 0, 0, 0,18, // 60
   12,12,15,20, 0, 0, 0, 0, 12,12,15,20, 0, 0, 0, 0, // 70
    0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, // 80
    0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, // 90
   16,16,16,16, 0, 0, 0, 0, 16,16,16,16, 0, 0, 0, 0, // A0
   21,21,21,21, 0, 0, 0, 0, 21,21,21,21, 0, 0, 0, 0, // B0
    0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, // C0
    0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, // D0
    0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, // E0
    0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0  // F0
  );
  TStateBit: array[0..255] of Byte = ( // CB
    8, 8, 8, 8, 8, 8,15, 8,  8, 8, 8, 8, 8, 8,15, 8, // 00
    8, 8, 8, 8, 8, 8,15, 8,  8, 8, 8, 8, 8, 8,15, 8, // 10
    8, 8, 8, 8, 8, 8,15, 8,  8, 8, 8, 8, 8, 8,15, 8, // 20
    8, 8, 8, 8, 8, 8,15, 8,  8, 8, 8, 8, 8, 8,15, 8, // 30
    8, 8, 8, 8, 8, 8,12, 8,  8, 8, 8, 8, 8, 8,12, 8, // 40
    8, 8, 8, 8, 8, 8,12, 8,  8, 8, 8, 8, 8, 8,12, 8, // 50
    8, 8, 8, 8, 8, 8,12, 8,  8, 8, 8, 8, 8, 8,12, 8, // 60
    8, 8, 8, 8, 8, 8,12, 8,  8, 8, 8, 8, 8, 8,12, 8, // 70
    8, 8, 8, 8, 8, 8,15, 8,  8, 8, 8, 8, 8, 8,15, 8, // 80
    8, 8, 8, 8, 8, 8,15, 8,  8, 8, 8, 8, 8, 8,15, 8, // 90
    8, 8, 8, 8, 8, 8,15, 8,  8, 8, 8, 8, 8, 8,15, 8, // A0
    8, 8, 8, 8, 8, 8,15, 8,  8, 8, 8, 8, 8, 8,15, 8, // B0
    8, 8, 8, 8, 8, 8,15, 8,  8, 8, 8, 8, 8, 8,15, 8, // C0
    8, 8, 8, 8, 8, 8,15, 8,  8, 8, 8, 8, 8, 8,15, 8, // D0
    8, 8, 8, 8, 8, 8,15, 8,  8, 8, 8, 8, 8, 8,15, 8, // E0
    8, 8, 8, 8, 8, 8,15, 8,  8, 8, 8, 8, 8, 8,15, 8  // F0
  );
  TStateBitX: array[0..255] of Byte = ( // DD CB, FD CB
    0, 0, 0, 0, 0, 0,23, 0,  0, 0, 0, 0, 0, 0,23, 0, // 00
    0, 0, 0, 0, 0, 0,23, 0,  0, 0, 0, 0, 0, 0,23, 0, // 10
    0, 0, 0, 0, 0, 0,23, 0,  0, 0, 0, 0, 0, 0,23, 0, // 20
    0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0,23, 0, // 30
    0, 0, 0, 0, 0, 0,20, 0,  0, 0, 0, 0, 0, 0,20, 0, // 40
    0, 0, 0, 0, 0, 0,20, 0,  0, 0, 0, 0, 0, 0,20, 0, // 50
    0, 0, 0, 0, 0, 0,20, 0,  0, 0, 0, 0, 0, 0,20, 0, // 60
    0, 0, 0, 0, 0, 0,20, 0,  0, 0, 0, 0, 0, 0,20, 0, // 70
    0, 0, 0, 0, 0, 0,23, 0,  0, 0, 0, 0, 0, 0,23, 0, // 80
    0, 0, 0, 0, 0, 0,23, 0,  0, 0, 0, 0, 0, 0,23, 0, // 90
    0, 0, 0, 0, 0, 0,23, 0,  0, 0, 0, 0, 0, 0,23, 0, // A0
    0, 0, 0, 0, 0, 0,23, 0,  0, 0, 0, 0, 0, 0,23, 0, // B0
    0, 0, 0, 0, 0, 0,23, 0,  0, 0, 0, 0, 0, 0,23, 0, // C0
    0, 0, 0, 0, 0, 0,23, 0,  0, 0, 0, 0, 0, 0,23, 0, // D0
    0, 0, 0, 0, 0, 0,23, 0,  0, 0, 0, 0, 0, 0,23, 0, // E0
    0, 0, 0, 0, 0, 0,23, 0,  0, 0, 0, 0, 0, 0,23, 0  // F0
  );
var
  len, n: Integer;
  code: Byte;
begin
  n := 0;
  Result := 0;
  len := Length(ACodes);
  if len > 0 then
  begin
    code := ACodes[n];
    Inc(n);
    case code of
      $DD, $FD:
      begin
        if n < len then
        begin
          code := ACodes[n];
          Inc(n);
          if code = $CB then
          begin
            if n > len + 1 then
            begin
              Inc(n);
              code := ACodes[n];
              Result := TStateBitX[code];
            end;
          end
          else Result := TStateX[code];
        end;
      end;
      $CB: if n < len then Result := TStateBit[ACodes[n]];
      $ED: if n < len then Result := TStateExt[ACodes[n]];
    else
      Result := TState[code];
    end;
  end;
  if Result = 0 then Result := len * 4;
end;

function TXPDebug.GetReg16A(AIndex: Integer): Word;
begin
  case AIndex of
    0: Result := Regs.BC;
    1: Result := Regs.DE;
    2: case FIndexCode of
      $DD: Result := Regs.IX;
      $FD: Result := Regs.IY;
    else
      Result := Regs.HL;
    end;
    3: Result := Regs.SP;
  else
    FErrors.Add(Format('Invalid Reg16A index %d at %4.4Xh', [AIndex, Regs.PC]));
    Result := 0;
  end;
end;

function TXPDebug.GetReg16B(AIndex: Integer): Word;
begin
  case AIndex of
    0: Result := Regs.BC;
    1: Result := Regs.DE;
    2: case FIndexCode of
      $DD: Result := Regs.IX;
      $FD: Result := Regs.IY;
    else
      Result := Regs.HL;
    end;
    3: Result := Regs.AF;
  else
    FErrors.Add(Format('Invalid Reg16B index %d at %4.4Xh', [AIndex, Regs.PC]));
    Result := 0;
  end;
end;

function TXPDebug.GetReg16M(AIndex: Integer): Byte;
var
  p: Integer;
begin
  p := -1;
  case AIndex of
    0: p := Regs.BC;
    1: p := Regs.DE;
    2:
    begin
      case FIndexCode of
        $DD: p := Regs.IX + FetchIndexOffset;
        $FD: p := Regs.IY + FetchIndexOffset;
      else
        p := Regs.HL;
      end;
    end;
    3: p := Regs.SP;
  else
    FErrors.Add(Format('Invalid Reg16M index %d at %4.4Xh', [AIndex, Regs.PC]));
  end;
  if p >= 0 then
    Result := ReadByte(p)
  else Result := 0;
end;

function TXPDebug.GetReg8A(AIndex: Integer): Byte;
begin
  case AIndex of
    0: Result := Regs.B;
    1: Result := Regs.C;
    2: Result := Regs.D;
    3: Result := Regs.E;
    4: Result := Regs.H;
    5: Result := Regs.L;
    6:
    case FIndexCode of
      $DD: Result := ReadByte(Regs.IX + FetchIndexOffset);
      $FD: Result := ReadByte(Regs.IY + FetchIndexOffset);
    else
      Result := ReadByte(Regs.HL);
    end;
    7: Result := Regs.A;
  else
    FErrors.Add(Format('Invalid Reg8 index %d at %4.4Xh', [AIndex, Regs.PC]));
    Result := 0;
  end;
end;

function TXPDebug.GetReg8B(AIndex: Integer): Byte;
begin
  case AIndex of
    0: Result := Regs.B;
    1: Result := Regs.C;
    2: Result := Regs.D;
    3: Result := Regs.E;
    4:
    case FIndexCode of
      $DD: Result := Regs.IXH;
      $FD: Result := Regs.IYH;
    else
      Result := Regs.H;
    end;
    5:
    case FIndexCode of
      $DD: Result := Regs.IXL;
      $FD: Result := Regs.IYL;
    else
      Result := Regs.L;
    end;
    6:
    case FIndexCode of
      $DD: Result := ReadByte(Regs.IX + FetchIndexOffset);
      $FD: Result := ReadByte(Regs.IY + FetchIndexOffset);
    else
      Result := ReadByte(Regs.HL);
    end;
    7: Result := Regs.A;
  else
    FErrors.Add(Format('Invalid Reg8 index %d at %4.4Xh', [AIndex, Regs.PC]));
    Result := 0;
  end;
end;

procedure TXPDebug.Jump(AValue: Word; ACondition: Boolean = True);
begin
  if ACondition then Regs.PC := AValue;
end;

procedure TXPDebug.JumpShort(AValue: Integer; ACondition: Boolean);
begin
  if ACondition then Regs.PC := Regs.PC + AValue;
end;

function TXPDebug.NextAddress(ALine: TXPDebugLine): TArrayOfInteger;
var
  ad, n: Integer;
begin
  Result := nil;
  if ALine <> nil then
  begin
    n := Length(ALine.FCodes);
    if n > 0 then
    begin
      ad := ALine.FAddress + n;
      SetLength(Result, 1);
      Result[0] := ad;
      ad := -1;
      case ALine.FCodes[0] of
        $C3, $C2, $CA, $D2, $DA, $E2, $EA, $F2, $FA: ad := ALine.FCodes[1] or (ALine.FCodes[2] shl 8);
        $10, $18, $20, $28, $30, $38: ad := ALine.FAddress + n + ByteToShort(ALine.FCodes[1]);
        $C9, $C0, $C8, $D0, $D8, $E0, $E8, $F0, $F8: ad := ReadWord(FRegs.SP);
        $E9: ad := FRegs.HL;
        $DD: if ALine.FCodes[1] = $E9 then ad := FRegs.IX;
        $FD: if ALine.FCodes[1] = $E9 then ad := FRegs.IY;
      end;
      if ad >= 0 then
      begin
        SetLength(Result, 2);
        Result[1] := ad;
      end;
    end;
  end;
end;

function TXPDebug.OpAdc8(n1, n2: Byte): Byte;
begin
  Result := n1 + n2 + Regs.CBit;
  Regs.SF := (Result and $80) <> 0;
  Regs.ZF := Result = 0;
  Regs.HF := (n1 and 15) + (n2 and 15) + Regs.CBit > 15;
  Regs.PVF := ((((n1 or n2) and $80) = 0) and ((Result and $80) <> 0))
    or ((((n1 and n2) and $80) <> 0) and ((Result and $80) = 0));
  Regs.NF := False;
  Regs.CF := n1 + n2 + Regs.CBit > 255;
end;

function TXPDebug.OpAdd8(n1, n2: Byte): Byte;
begin
  Result := n1 + n2;
  Regs.SF := (Result and $80) <> 0;
  Regs.ZF := Result = 0;
  Regs.HF := (n1 and 15) + (n2 and 15) > 15;
  Regs.PVF := ((((n1 or n2) and $80) = 0) and ((Result and $80) <> 0))
    or ((((n1 and n2) and $80) <> 0) and ((Result and $80) = 0));
  Regs.NF := False;
  Regs.CF := n1 + n2 > 255;
end;

function TXPDebug.OpAnd8(n1, n2: Byte): Byte;
begin
  Result := n1 and n2;
  Regs.SF := (Result and $80) <> 0;
  Regs.ZF := Result = 0;
  Regs.HF := True;
  Regs.PVF := ((((n1 or n2) and $80) = 0) and ((Result and $80) <> 0))
    or ((((n1 and n2) and $80) <> 0) and ((Result and $80) = 0));
  Regs.NF := False;
  Regs.CF := False;
end;

function TXPDebug.OpCp8(n1, n2: Byte): Byte;
begin
  Result := n1 - n2;
  Regs.SF := (Result and $80) <> 0;
  Regs.ZF := Result = 0;
  Regs.HF := (n1 and 15) < (n2 and 15);
  Regs.PVF := (((n1 and $80) = 0) and ((n2 and $80) <> 0) and ((Result and $80) = 0))
    or (((n1 and $80) <> 0) and ((n2 and $80) <> 0)  and ((Result and $80) = 0));
  Regs.NF := True;
  Regs.CF := n1 < n2;
  Result := n1;
end;

function TXPDebug.OpOr8(n1, n2: Byte): Byte;
begin
  Result := n1 or n2;
  Regs.A := Result;
  Regs.SF := (Result and $80) <> 0;
  Regs.ZF := Result = 0;
  Regs.HF := False;
  Regs.PVF := ((((n1 or n2) and $80) = 0) and ((Result and $80) <> 0))
    or ((((n1 and n2) and $80) <> 0) and ((Result and $80) = 0));
  Regs.NF := False;
  Regs.CF := False;
end;

function TXPDebug.OpSbc8(n1, n2: Byte): Byte;
begin
  Result := n1 - n2 - Regs.CBit;
  Regs.SF := (Result and $80) <> 0;
  Regs.ZF := Result = 0;
  Regs.HF := (n1 and 15) < (n2 and 15) + Regs.CBit;
  Regs.PVF := (((n1 and $80) = 0) and ((n2 and $80) <> 0) and ((Result and $80) = 0))
    or (((n1 and $80) <> 0) and ((n2 and $80) <> 0)  and ((Result and $80) = 0));
  Regs.NF := True;
  Regs.CF := n1 < n2 + Regs.CBit;
end;

function TXPDebug.OpSub8(n1, n2: Byte): Byte;
begin
  Result := n1 - n2;
  Regs.SF := (Result and $80) <> 0;
  Regs.ZF := Result = 0;
  Regs.HF := (n1 and 15) < (n2 and 15);
  Regs.PVF := (((n1 and $80) = 0) and ((n2 and $80) <> 0) and ((Result and $80) = 0))
    or (((n1 and $80) <> 0) and ((n2 and $80) <> 0)  and ((Result and $80) = 0));
  Regs.NF := True;
  Regs.CF := n1 < n2;
end;

function TXPDebug.OpXor8(n1, n2: Byte): Byte;
begin
  Result := n1 xor n2;
  Regs.SF := (Result and $80) <> 0;
  Regs.ZF := Result = 0;
  Regs.HF := False;
  Regs.PVF := (Result and 1) <> 0;
  Regs.NF := False;
  Regs.CF := False;
end;

function TXPDebug.Pop: Word;
begin
  Result := ReadWord(Regs.SP);
  Regs.SP := Regs.SP + 2;
end;

procedure TXPDebug.Push(AValue: Word);
begin
  Regs.SP := Regs.SP - 2;
  WriteWord(Regs.SP, AValue, True);
end;

function TXPDebug.ReadByte(AAddress: Integer): Byte;
begin
  Result := FMapper.Memory[AAddress];
end;

function TXPDebug.ReadPort(APort: Byte): Byte;
begin
  case APort of
    $98..$9B: Result := FVideo.ReadPort(APort);
    $A8: Result := (FMapper[0] and 3) or ((FMapper[1] and 3) shl 2)
      or ((FMapper[2] and 3) shl 4) or ((FMapper[3] and 3) shl 6);
    $B4..$B5: Result := FClock.ReadPort(APort);
    $FC..$FF: Result := FMainRAM[APort and 3] or $F8;
  else
    Result := $FF;
  end;
end;

function TXPDebug.ReadShortAddress(AAddress: Integer): Integer;
begin
  Result := ReadByte(AAddress);
  if Result > 127 then Result := Result - 256;
end;

function TXPDebug.ReadWord(AAddress: Integer): Word;
begin
  Result := ReadByte(AAddress) or (ReadByte(AAddress + 1) shl 8);
end;

procedure TXPDebug.Reset;
var
  secmap: TXPDebugSlotMapper;
begin
  FVideo.Reset;
  FClock.Reset;
  FRegs.Clear;
  FPRegs.Clear;
  FMapper.Clear;
  FLastTime := 0;

  if FStartAddress < 0 then
    Regs.PC := 0
  else Regs.PC := FStartAddress;
  Regs.SP := FStackAddress;

  IFF1 := False;
  IFF2 := False;
  FIntMode := 0;
  FIndexCode := 0;
  FIndexOffset := -1;
  FCodes := nil;
  FExecuted := False;
  FCanPaint := False;
  FReturnCount := -1;

  FMainROM[0] := 0;
  FMainROM[1] := 1;
  FMainROM[2] := -1;
  FMainROM[3] := -1;
  FMainROM.LoadFromResource(0, 'MSX2-BIOS', RT_RCDATA);
  FMainROM.LoadFromResource(1, 'MSX2-BASIC', RT_RCDATA);

  FExtROM[0] := 0;
  FExtROM[1] := 0;
  FExtROM[2] := 0;
  FExtROM[3] := 0;
  FExtROM.LoadFromResource(0, 'MSX2-EXT', RT_RCDATA);

  FDrive.Reset;

  FMainRAM[0] := 3;
  FMainRAM[1] := 2;
  FMainRAM[2] := 1;
  FMainRAM[3] := 0;

  secmap := TXPDebugSlotMapper.Create(True);
  secmap.Slots[0] := FExtROM;
  secmap.Slots[1] := nil;
  secmap.Slots[2] := FMainRAM;
  secmap.Slots[3] := FDrive;

  FMapper.Slots[0] := FMainROM;
  FMapper.Slots[1] := nil;
  FMapper.Slots[2] := nil;
  FMapper.Slots[3] := secmap;

  SetSystem(FSystem);

  UpdateSelectIndexs;
end;

procedure TXPDebug.Ret(ACondition: Boolean);
begin
  if ACondition then
  begin
    FRegs.PC := Pop;
    if FReturnCount > 0 then Dec(FReturnCount);
  end;
end;

procedure TXPDebug.Run;
begin
  FReturnCount := -1;
  RunTo(-1);
end;

procedure TXPDebug.RunAndReturn;
begin
  FReturnCount := 1;
  RunTo(-1);
end;

procedure TXPDebug.RunTo(AAddress: Integer);
begin
  RunTo([AAddress]);
end;

procedure TXPDebug.RunTo(AAddresses: TArrayOfInteger);
var
  line: TXPDebugLine;
begin
  line := FLines.SelectedLine;
  if line = nil then line := FSystemLines.SelectedLine;
  if line <> nil then
  begin
    Regs.Affected.Clear;
    PRegs.Assign(Regs);
    FVideo.Regs.Affected.Clear;
    FExecuted := True;
    FreeAndNil(FThread);
    FThread := TXPDebugThread.Create(Self);
    FThread.BreakPoints := FBreakPoints;
    FThread.BreakPoints.Add(AAddresses);
    FThread.Start;
  end;
end;

procedure TXPDebug.RunToLine(ALineIndex: Integer);
var
  p: Integer;
begin
  p := FLines.SelectedAddress(ALineIndex);
  if p >= 0 then
  begin
    FReturnCount := -1;
    RunTo(p);
  end;
end;

procedure TXPDebug.RunToSystemLine(ALineIndex: Integer);
var
  p: Integer;
begin
  p := FSystemLines.SelectedAddress(ALineIndex);
  if p >= 0 then
  begin
    FReturnCount := -1;
    RunTo(p);
  end;
end;

procedure TXPDebug.SetReg16A(AIndex: Integer; AValue: Word);
begin
  case AIndex of
    0: Regs.BC := AValue;
    1: Regs.DE := AValue;
    2: case FIndexCode of
      $DD: Regs.IX := AValue;
      $FD: Regs.IY := AValue;
    else
      Regs.HL := AValue;
    end;
    3: Regs.SP := AValue;
  else
    raise Exception.Create('Invalid Reg16A index');
  end;
end;

procedure TXPDebug.SetReg16B(AIndex: Integer; AValue: Word);
begin
  case AIndex of
    0: Regs.BC := AValue;
    1: Regs.DE := AValue;
    2: case FIndexCode of
      $DD: Regs.IX := AValue;
      $FD: Regs.IY := AValue;
    else
      Regs.HL := AValue;
    end;
    3: Regs.AF := AValue;
  else
    raise Exception.Create('Invalid Reg16B index');
  end;
end;

procedure TXPDebug.SetReg16M(AIndex: Integer; AValue: Byte);
var
  p: Integer;
begin
  p := -1;
  case AIndex of
    0: p := Regs.BC;
    1: p := Regs.DE;
    2:
    begin
      case FIndexCode of
        $DD: p := Regs.IX + FetchIndexOffset;
        $FD: p := Regs.IY + FetchIndexOffset;
      else
        p := Regs.HL;
      end;
    end;
    3: p := Regs.SP;
  else
    FErrors.Add(Format('Invalid Reg16M index %d at %4.4Xh', [AIndex, Regs.PC]));
  end;
  if p >= 0 then WriteByte(p, AValue, True);
end;

procedure TXPDebug.SetReg8A(AIndex: Integer; AValue: Byte);
begin
  case AIndex of
    0: Regs.B := AValue;
    1: Regs.C := AValue;
    2: Regs.D := AValue;
    3: Regs.E := AValue;
    4: Regs.H := AValue;
    5: Regs.L := AValue;
    6:
    case FIndexCode of
      $DD: WriteByte(Regs.IX + FetchIndexOffset, AValue, True);
      $FD: WriteByte(Regs.IY + FetchIndexOffset, AValue, True);
    else
      WriteByte(Regs.HL, AValue, True);
    end;
    7: Regs.A := AValue;
  else
    FErrors.Add(Format('Invalid Reg8 index %d at %4.4Xh', [AIndex, Regs.PC]));
  end;
end;

procedure TXPDebug.SetReg8B(AIndex: Integer; AValue: Byte);
begin
  case AIndex of
    0: Regs.B := AValue;
    1: Regs.C := AValue;
    2: Regs.D := AValue;
    3: Regs.E := AValue;
    4:
    case FIndexCode of
      $DD: Regs.IXH := AValue;
      $FD: Regs.IYH := AValue;
    else
      Regs.H := AValue;
    end;
    5:
    case FIndexCode of
      $DD: Regs.IXL := AValue;
      $FD: Regs.IYL := AValue;
    else
      Regs.L := AValue;
    end;
    6:
    case FIndexCode of
      $DD: WriteByte(Regs.IX + FetchIndexOffset, AValue, True);
      $FD: WriteByte(Regs.IY + FetchIndexOffset, AValue, True);
    else
      WriteByte(Regs.HL, AValue, True);
    end;
    7: Regs.A := AValue;
  else
    FErrors.Add(Format('Invalid Reg8 index %d at %4.4Xh', [AIndex, Regs.PC]));
  end;
end;

procedure TXPDebug.SetSystem(AValue: TXPDebugSystem);
var
  secmap: TXPDebugSlotMapper;
begin
  if AValue <> FSystem then
  begin
    case AValue of
      sysDOS:
      begin
        FMainRAM.LoadFromResource(3, 'MSX2-RAM-DOS-0000', RT_RCDATA);
        FMainRAM.LoadFromResource(2, 'MSX2-RAM-DOS-4000', RT_RCDATA);
        FMainRAM.LoadFromResource(1, 'MSX2-RAM-DOS-8000', RT_RCDATA);
        FMainRAM.LoadFromResource(0, 'MSX2-RAM-DOS-C000', RT_RCDATA);
      end;
    else
      FMainRAM.LoadFromResource(3, 'MSX2-RAM-BASIC-8000-TEST', RT_RCDATA);
      FMainRAM.LoadFromResource(2, 'MSX2-RAM-BASIC-C000-TEST', RT_RCDATA);
    end;
  end;
  FSystem := AValue;
  secmap := TXPDebugSlotMapper(FMapper.Slots[3]);
  case FSystem of
    sysDOS:
    begin
      FMapper[0] := 3;
      FMapper[1] := 3;
      FMapper[2] := 3;
      FMapper[3] := 3;
      secmap[0] := 2;
      secmap[1] := 2;
      secmap[2] := 2;
      secmap[3] := 2;
    end;
  else
    FMapper[0] := 0;
    FMapper[1] := 0;
    FMapper[2] := 3;
    FMapper[3] := 3;
    secmap[0] := 0;
    secmap[1] := 3;
    secmap[2] := 2;
    secmap[3] := 2;
  end;
end;

procedure TXPDebug.StepInto(AUpdated: Boolean; AIndexCode: Byte);
const
  AREG8: array[0..7] of string = ('B', 'C', 'D', 'E', 'H', 'L', '', 'A');
  AREG16A: array[0..3] of array[0..1] of string = (('B', 'C'), ('D', 'E'), ('H', 'L'), ('SP', ''));
  AREG16B: array[0..3] of array[0..1] of string = (('B', 'C'), ('D', 'E'), ('H', 'L'), ('A', 'F'));
var
  code, b, c1, c2: Byte;
  w, w1, w2: Word;
  n: Integer;
begin
  FIndexCode := AIndexCode;
  if FIndexCode = 0 then
  begin
    FCodes := nil;
    FIndexOffset := -1;
  end;
  if AUpdated then
  begin
    Regs.Affected.Clear;
    PRegs.Assign(Regs);
    FVideo.Regs.Affected.Clear;
  end;
  code := FetchByte;
  case code of
    $00: ; // NOP
    $01, $11, $21, $31: // LD rr,nn
    begin
      b := (code shr 4) and 3;
      Reg16A[b] := FetchWord;
    end;
    $02, $12: Reg16M[(code shr 4) and 3] := Regs.A; // LD (rr),A
    $22: WriteWord(FetchWord, Reg16A[2], True); // LD (nnnn),HL
    $32: WriteByte(FetchWord, Regs.A, True); // LD (nnnn),A
    $03, $13, $23, $33: // INC rr
    begin
      b := (code shr 4) and 3;
      Reg16A[b] := Reg16A[b] + 1;
    end;
    $04, $0C, $14, $1C, $24, $2C, $34, $3C: // INC r
    begin
      b := (code shr 3) and 7;
      if code = $34 then FetchIndexOffset;
      c1 := Reg8B[b];
      c2 := c1 + 1;
      Reg8B[b] := c2;
      Regs.SF := (c2 and $80) <> 0;
      Regs.ZF := c2 = 0;
      Regs.HF := (c1 and 15) + (c2 and 15) > 15;
      Regs.PVF := c1 = $7F;
      Regs.NF := False;
    end;
    $05, $0D, $15, $1D, $25, $2D, $35, $3D: // DEC r
    begin
      b := (code shr 3) and 7;
      if code = $35 then FetchIndexOffset;
      c1 := Reg8B[b];
      c2 := c1 - 1;
      Reg8B[b] := c2;
      Regs.SF := (c2 and $80) <> 0;
      Regs.ZF := c2 = 0;
      Regs.HF := (c1 and 15) < (c2 and 15);
      Regs.PVF := c1 = $80;
      Regs.NF := True;
    end;
    $06, $0E, $16, $1E, $26, $2E, $36, $3E: // LD r,n
    begin
      b := (code shr 3) and 7;
      if code = $36 then FetchIndexOffset;
      Reg8B[b] := FetchByte;
    end;
    $07: // RLCA
    begin
      c1 := Regs.A;
      c2 := ((c1 shl 1) and $FE) or ((c1 shr 7) and 1);
      Regs.A := c2;
      Regs.HF := False;
      Regs.NF := False;
      Regs.CF := (c1 and $80) <> 0;
    end;
    $0F: // RRCA
    begin
      c1 := Regs.A;
      c2 := ((c1 shr 1) and $7F) or ((c1 shl 7) and $80);
      Regs.A := c2;
      Regs.HF := False;
      Regs.NF := False;
      Regs.CF := (c1 and 1) <> 0;
    end;
    $17: // RLA
    begin
      b := Regs.CBit;
      c1 := Regs.A;
      c2 := ((c1 shl 1) or b) and 255;
      Regs.A := c2;
      Regs.HF := False;
      Regs.NF := False;
      Regs.CF := (c1 and $80) <> 0;
    end;
    $1F: // RRA
    begin
      b := Regs.CBit;
      c1 := Regs.A;
      c2 := ((c1 shr 1) or (b shl 7)) and 255;
      Regs.A := c2;
      Regs.HF := False;
      Regs.NF := False;
      Regs.CF := (c1 and 1) <> 0;
    end;
    $27: // DAA
    begin
      c1 := Regs.A;
      c2 := c1;
      Regs.A := c2;
      Regs.SF := (c2 and $80) <> 0;
      Regs.ZF := c2 = 0;
//      Regs.HF := (c1 and $10) <> (c2 and $10);
      Regs.PVF := (c2 and 1) = 0;
//      Regs.CF := True;
    end;
    $2F: // CPL
    begin
      c1 := Regs.A;
      c2 := c1 xor 255;
      Regs.A := c2;
      Regs.HF := True;
      Regs.NF := True;
    end;
    $37: // SCF
    begin
      Regs.CF := True;
      Regs.HF := False;
      Regs.NF := False;
    end;
    $3F: // CCF
    begin
      Regs.HF := Regs.CF;
      Regs.CF := not Regs.CF;
      Regs.NF := False;
    end;
    $08: // EX AF,AF'
    begin
      w1 := Regs.AF;
      Regs.AF := Regs.AF_;
      Regs.AF_ := w1;
    end;
    $09, $19, $29, $39:
    begin // ADD HL,BC-DE-HL-SP
      w1 := Reg16A[2];
      w2 := w1 + Reg16A[(code shr 4) and 3];
      Reg16A[2] := w2;
      Regs.HF := (w1 and $FFF) + (w2 and $FFF) > $FFF;
      Regs.NF := False;
      Regs.CF := w1 + w2 > 65535;
    end;
    $0A, $1A: Regs.A := ReadByte(Reg16A[(code shr 4) and 3]); // LD A,(BC)-(DE)
    $2A: Reg16A[2] := ReadWord(FetchWord); // LD HL,(nnnn)
    $3A: Regs.A := ReadByte(FetchWord); // LD A,(nnnn)
    $0B, $1B, $2B, $3B: // DEC BC-DE-HL-SP
    begin
      b := (code shr 4) and 3;
      w1 := Reg16A[b];
      w2 := w1 - 1;
      Reg16A[b] := w2;
    end;
    $10: // DJNZ
    begin
      n := FetchShortAddress;
      c1 := Regs.B;
      c2 := c1 - 1;
      Regs.B := c2;
      if c2 <> 0 then Regs.PC := Regs.PC + n;
    end;
    $18: JumpShort(FetchShortAddress);              // JR
    $20: JumpShort(FetchShortAddress, not Regs.ZF); // JR NZ,e
    $28: JumpShort(FetchShortAddress, Regs.ZF);     // JR Z,e
    $30: JumpShort(FetchShortAddress, not Regs.CF); // JR NC,e
    $38: JumpShort(FetchShortAddress, Regs.CF);     // JR C,e
    $76: // HALT
    begin
      FErrors.Add('Halt');
      FExecuted := False;
    end;
    $40..$45, $47..$4D, $4F..$55, $57..$5D,
    $5F..$65, $67..$6D, $6F, $78..$7D, $7F:
      Reg8B[(code shr 3) and 7] := Reg8B[code and 7]; // LD r,r
    $46, $4E, $56, $5E, $66, $6E, $70..$75, $77, $7E:
      Reg8A[(code shr 3) and 7] := Reg8A[code and 7]; // LD r,r
    $80..$87: Regs.A := OpAdd8(Regs.A, Reg8B[code and 7]); // ADD A,r
    $88..$8F: Regs.A := OpAdc8(Regs.A, Reg8B[code and 7]); // ADC A,r
    $90..$97: Regs.A := OpSub8(Regs.A, Reg8B[code and 7]); // SUB A,r
    $98..$9F: Regs.A := OpSbc8(Regs.A, Reg8B[code and 7]); // SBC A,r
    $A0..$A7: Regs.A := OpAnd8(Regs.A, Reg8B[code and 7]); // AND A,r
    $A8..$AF: Regs.A := OpXor8(Regs.A, Reg8B[code and 7]); // XOR A,r
    $B0..$B7: Regs.A := OpOr8(Regs.A, Reg8B[code and 7]); // OR A,r
    $B8..$BF: Regs.A := OpCp8(Regs.A, Reg8B[code and 7]); // CP A,r
    $C0: Ret(not Regs.ZF); // RET NZ
    $C8: Ret(Regs.ZF); // RET Z
    $D0: Ret(not Regs.CF); // RET NC
    $D8: Ret(Regs.CF); // RET C
    $E0: Ret(not Regs.PVF); // RET PO
    $E8: Ret(Regs.PVF); // RET PE
    $F0: Ret(not Regs.SF); // RET P
    $F8: Ret(Regs.SF); // RET M
    $C1, $D1, $E1, $F1: Reg16B[(code shr 4) and 3] := Pop; // POP rr
    $C2: Jump(FetchWord, not Regs.ZF); // JP NZ,nnnn
    $CA: Jump(FetchWord, Regs.ZF); // JP Z,nnnn
    $D2: Jump(FetchWord, not Regs.CF); // JP NC,nnnn
    $DA: Jump(FetchWord, Regs.CF); // JP C,nnnn
    $E2: Jump(FetchWord, not Regs.PVF); // JP PO,nnnn
    $EA: Jump(FetchWord, Regs.PVF); // JP PE,nnnn
    $F2: Jump(FetchWord, not Regs.SF); // JP P,nnnn
    $FA: Jump(FetchWord, Regs.SF); // JP M,nnnn
    $C3: Jump(FetchWord); // JP nnnn
    $C4: Call(FetchWord, not Regs.ZF); // CALL NZ,nnnn
    $CC: Call(FetchWord, Regs.ZF); // CALL Z,nnnn
    $D4: Call(FetchWord, not Regs.CF); // CALL NC,nnnn
    $DC: Call(FetchWord, Regs.CF); // CALL C,nnnn
    $E4: Call(FetchWord, not Regs.PVF); // CALL PO,nnnn
    $EC: Call(FetchWord, Regs.PVF); // CALL PE,nnnn
    $F4: Call(FetchWord, not Regs.SF); // CALL P,nnnn
    $FC: Call(FetchWord, Regs.SF); // CALL M,nnnn
    $C5, $D5, $E5, $F5: Push(Reg16B[(code shr 4) and 3]); // PUSH rr
    $C6: Regs.A := OpAdd8(Regs.A, FetchByte); // ADD A,nn
    $CE: Regs.A := OpAdc8(Regs.A, FetchByte); // ADC A,nn
    $D6: Regs.A := OpSub8(Regs.A, FetchByte); // SUB A,nn
    $DE: Regs.A := OpSbc8(Regs.A, FetchByte); // SBC A,nn
    $E6: Regs.A := OpAnd8(Regs.A, FetchByte); // AND nn
    $EE: Regs.A := OpXor8(Regs.A, FetchByte); // XOR nn
    $F6: Regs.A := OpOr8(Regs.A, FetchByte); // OR nn
    $FE: Regs.A := OpCp8(Regs.A, FetchByte); // CP nn
    $C7, $CF, $D7, $DF, $E7, $EF, $F7, $FF: Call(code and $38); // RST n
    $C9: Ret; // RET
    $D3: WritePort(FetchByte, Regs.A, True); // OUT (nn),A
    $DB: Regs.A := ReadPort(FetchByte); // IN A,(nn)
    $D9: // EXX
    begin
      w1 := Regs.BC;
      Regs.BC := Regs.BC_;
      Regs.BC_ := w1;
      w1 := Regs.DE;
      Regs.DE := Regs.DE_;
      Regs.DE_ := w1;
      w1 := Regs.HL;
      Regs.HL := Regs.HL_;
      Regs.HL_ := w1;
    end;
    $E3: // EX (SP),HL
    begin
      w1 := ReadWord(Regs.SP);
      WriteWord(Regs.SP, Regs.HL, True);
      Regs.HL := w1;
    end;
    $EB: // EX DE,HL
    begin
      w1 := Regs.DE;
      Regs.DE := Regs.HL;
      Regs.HL := w1;
    end;
    $F3: // DI
    begin
      IFF1 := False;
      IFF2 := False;
    end;
    $FB: // EI
    begin
      IFF1 := True;
      IFF2 := True;
    end;
    $E9:
    case FIndexCode of
      $DD: Jump(Regs.IX);
      $FD: Jump(Regs.IY);
    else
      Jump(Regs.HL); // JP (HL)
    end;
    $F9: Regs.SP := Regs.HL; // LD SP,HL
    $CD: Call(FetchWord); // CALL nnnn
    $CB: // Bit Ops
    begin
      FetchIndexOffset;
      code := FetchByte;
      c1 := code and 7;
      c2 := (code shr 3) and 7;
      n := Reg8A[c1];
      case code of
        $00..$3F:
        begin
          case c2 of
            0: // RLC r
            begin
              Regs.CBit := (n shr 7) and 1;
              b := ((n shl 1) and $FE) or Regs.CBit;
              Reg8A[c1] := b;
              Regs.SF := (b and $80) <> 0;
              Regs.ZF := b = 0;
              Regs.HF := False;
              Regs.PVF := (b and 1) = 0;
              Regs.NF := False;
            end;
            1: // RRC r
            begin
              Regs.CBit := n and 1;
              b := ((n shr 1) and $7F) or (Regs.CBit shl 7);
              Reg8A[c1] := b;
              Regs.SF := (b and $80) <> 0;
              Regs.ZF := b = 0;
              Regs.HF := False;
              Regs.PVF := (b and 1) = 0;
              Regs.NF := False;
            end;
            2: // RL r
            begin
              n := (n shl 1) or Regs.CBit;
              Regs.CBit := (n shr 8) and 1;
              b := n and 255;
              Reg8A[c1] := b;
              Regs.SF := (b and $80) <> 0;
              Regs.ZF := b = 0;
              Regs.HF := False;
              Regs.PVF := (b and 1) = 0;
              Regs.NF := False;
            end;
            3: // RR r
            begin
              n := (n and 255) or (Regs.CBit shl 8);
              Regs.CBit := n and 1;
              b := n shr 1;
              Reg8A[c1] := b;
              Regs.SF := (b and $80) <> 0;
              Regs.ZF := b = 0;
              Regs.HF := False;
              Regs.PVF := (b and 1) = 0;
              Regs.NF := False;
            end;
            4: // SLA r
            begin
              Regs.CBit := (n shr 7) and 1;
              b := (n shl 1) and $FE;
              Reg8A[c1] := b;
              Regs.SF := (b and $80) <> 0;
              Regs.ZF := b = 0;
              Regs.HF := False;
              Regs.PVF := (b and 1) = 0;
              Regs.NF := False;
            end;
            5: // SRA r
            begin
              Regs.CBit := n and 1;
              b := ((n and $80) or (n shr 1)) and 255;
              Reg8A[c1] := b;
              Regs.SF := (b and $80) <> 0;
              Regs.ZF := b = 0;
              Regs.HF := False;
              Regs.PVF := (b and 1) = 0;
              Regs.NF := False;
            end;
            6: // SLL r
            begin
              Regs.CBit := (n shr 7) and 1;
              b := ((n shl 1) and $FE) or 1;
              Reg8A[c1] := b;
              Regs.SF := (b and $80) <> 0;
              Regs.ZF := b = 0;
              Regs.HF := False;
              Regs.PVF := (b and 1) = 0;
              Regs.NF := False;
            end;
            7: // SRL r
            begin
              Regs.CBit := n and 1;
              b := (n shr 1) and $7F;
              Reg8A[c1] := b;
              Regs.SF := (b and $80) <> 0;
              Regs.ZF := b = 0;
              Regs.HF := False;
              Regs.PVF := (b and 1) = 0;
              Regs.NF := False;
            end;
          end;
        end;
        $40..$7F: // BIT b,r
        begin
          Regs.SF := False;
          Regs.ZF := (n and (1 shl c2)) = 0;
          Regs.HF := True;
          Regs.PVF := False;
          Regs.NF := False;
        end;
        $80..$BF: Reg8A[c1] := n and ((1 shl c2) xor 255); // RES b,r
        $C0..$FF: Reg8A[c1] := n or (1 shl c2); // SET b,r
      end;
    end;
    $DD, $FD:
    begin
      if FIndexCode = 0 then
      begin
        FIndexOffset := -1;
        StepInto(False, code);
      end
      else FErrors.Add(Format('Invalid opcode: %2.2X %2.2X', [FIndexCode, code]));
    end;
    $ED:
    begin
      code := FetchByte;
      case code of
        $40, $48, $50, $58, $60, $68, $70, $78: Reg8A[(code shr 3) and 7] := ReadPort(Regs.C); // IN r,(C)
        $41, $49, $51, $59, $61, $69, $71, $79: WritePort(Regs.C, Reg8A[(code shr 3) and 7]); // OUT (C),r
        $42, $52, $62, $72:
        begin // SBC HL,rr
          w1 := Regs.HL;
          w2 := Reg16A[(code shr 4) and 3];
          w := w1 - w2 - Regs.CBit;
          Regs.HL := w;
          Regs.SF := (w and $8000) <> 0;
          Regs.ZF := w = 0;
          Regs.HF := (w1 and $FFF) < (w2 and $FFF) + Regs.CBit;
          Regs.PVF := (((w1 and $8000) = 0) and ((w2 and $8000) <> 0) and ((w and $8000) = 0))
            or (((w1 and $8000) <> 0) and ((w2 and $8000) <> 0)  and ((w and $8000) = 0));
          Regs.NF := True;
          Regs.CF := w1 < w2 + Regs.CBit;
        end;
        $43, $53, $73: WriteWord(FetchWord, Reg16A[(code shr 4) and 3], True); // LD (nnnn),rr
        $63: WriteWord(FetchWord, Regs.HL, True); // LD (nnnn),HL // ED 63 XX XX
        $44, $4C, $54, $5C, $64, $6C, $74, $7C: // NEG // ED xx
          Regs.A := OpSub8(0, Regs.A);
        $45, $55, $5D, $65, $6D, $75: Ret; // RETN // ED xx
        $46: FIntMode := 0; // IM 0
        $56: FIntMode := 1; // IM 1
        $5E: FIntMode := 2; // IM 2
        $47: Regs.I := Regs.A; // LD I,A
        $4F: Regs.R := Regs.A; // LD R,A
        $57: // LD A,I
        begin
          b := Regs.I;
          Regs.A := b;
          Regs.SF := (b and $80) <> 0;
          Regs.ZF := b = 0;
          Regs.HF := False;
          Regs.PVF := IFF2;
          Regs.NF := False;
        end;
        $5F: // LD A,R
        begin
          b := Regs.R;
          Regs.A := b;
          Regs.SF := (b and $80) <> 0;
          Regs.ZF := b = 0;
          Regs.HF := False;
          Regs.PVF := IFF2;
          Regs.NF := False;
        end;
        $4A: // ADC HL,rr
        begin
          w1 := Regs.HL;
          w2 := Reg16A[(code shr 4) and 3];
          w := w1 + w2 + Regs.CBit;
          Regs.HL := w;
          Regs.SF := (w and $8000) <> 0;
          Regs.ZF := w = 0;
          Regs.HF := (w1 and $FFF) + (w2 and $FFF) + Regs.CBit > $FFF;
          Regs.PVF := ((((w1 or w2) and $8000) = 0) and ((w and $8000) <> 0))
            or ((((w1 and w2) and $8000) <> 0) and ((w and $8000) = 0));
          Regs.NF := True;
          Regs.CF := w1 + w2 + Regs.CBit > 65535;
        end;
        $4B, $5B, $7B: Reg16A[(code shr 4) and 3] := ReadWord(FetchWord); // LD rr,(nnnn)
        $6B: Regs.HL := ReadWord(FetchWord); // LD HL,(nnnn) // ED 6B XX XX
        $4D, $7D: Ret; // RETI // ED XX
        $67: // RRD
        begin
          c1 := Regs.A;
          b := (c1 and $C0) or ((c1 shl 4) and $30) or ((c1 shr 2) and $C0) or ((c1 shr 2) and 3);
          Regs.A := b;
          Regs.SF := (b and $80) <> 0;
          Regs.ZF := b = 0;
          Regs.HF := False;
          Regs.PVF := (b and 1) = 0;
          Regs.NF := False;
        end;
        $6F: // RLD
        begin
          c1 := Regs.A;
          b := (c1 and $C0) or ((c1 shl 2) and $30) or ((c1 shl 2) and $C0) or ((c1 shr 4) and 3);
          Regs.A := b;
          Regs.SF := (b and $80) <> 0;
          Regs.ZF := b = 0;
          Regs.HF := False;
          Regs.PVF := (b and 1) = 0;
          Regs.NF := False;
        end;
        $A0: // LDI
        begin
          WriteByte(Regs.DE, ReadByte(Regs.HL));
          Regs.DE := Regs.DE + 1;
          Regs.HL := Regs.HL + 1;
          Regs.BC := Regs.BC - 1;
          Regs.HF := False;
          Regs.PVF := Regs.BC <> 0;
          Regs.NF := False;
        end;
        $A1: // CPI
        begin
          c1 := Regs.A;
          c2 := ReadByte(Regs.HL);
          b := c1 - c2;
          Regs.HL := Regs.HL + 1;
          Regs.BC := Regs.BC - 1;
          Regs.SF := (b and $80) <> 0;
          Regs.ZF := b = 0;
          Regs.HF := ((c1 and 8) = 0) and ((c2 and 8) <> 0);
          Regs.PVF := Regs.BC <> 0;
          Regs.NF := True;
        end;
        $A2: // INI
        begin
          b := ReadPort(Regs.C);
          WriteByte(Regs.HL, b);
          Regs.HL := Regs.HL + 1;
          Regs.B := Regs.B - 1;
          Regs.SF := False;
          Regs.ZF := Regs.B = 0;
          Regs.HF := False;
          Regs.PVF := False;
          Regs.NF := True;
        end;
        $A3: // OUTI
        begin
          b := ReadByte(Regs.HL);
          WritePort(Regs.C, b);
          Regs.HL := Regs.HL + 1;
          Regs.B := Regs.B - 1;
          Regs.SF := False;
          Regs.ZF := Regs.B = 0;
          Regs.HF := False;
          Regs.PVF := False;
          Regs.NF := True;
        end;
        $A8: // LDD
        begin
          WriteByte(Regs.DE, ReadByte(Regs.HL));
          Regs.DE := Regs.DE - 1;
          Regs.HL := Regs.HL - 1;
          Regs.BC := Regs.BC - 1;
          Regs.HF := False;
          Regs.PVF := Regs.BC <> 0;
          Regs.NF := False;
        end;
        $A9: // CPD
        begin
          c1 := Regs.A;
          c2 := ReadByte(Regs.HL);
          b := c1 - c2;
          Regs.HL := Regs.HL - 1;
          Regs.BC := Regs.BC - 1;
          Regs.SF := (b and $80) <> 0;
          Regs.ZF := b = 0;
          Regs.HF := ((c1 and 8) = 0) and ((c2 and 8) <> 0);
          Regs.PVF := Regs.BC <> 0;
          Regs.NF := True;
        end;
        $AA: // IND
        begin
          b := ReadPort(Regs.C);
          WriteByte(Regs.HL, b);
          Regs.HL := Regs.HL - 1;
          Regs.B := Regs.B - 1;
          Regs.SF := False;
          Regs.ZF := Regs.B = 0;
          Regs.HF := False;
          Regs.PVF := False;
          Regs.NF := True;
        end;
        $AB: // OUTD
        begin
          b := ReadByte(Regs.HL);
          WritePort(Regs.C, b);
          Regs.HL := Regs.HL - 1;
          Regs.B := Regs.B - 1;
          Regs.SF := False;
          Regs.ZF := Regs.B = 0;
          Regs.HF := False;
          Regs.PVF := False;
          Regs.NF := True;
        end;
        $B0: // LDIR
        begin
          WriteByte(Regs.DE, ReadByte(Regs.HL));
          Regs.DE := Regs.DE + 1;
          Regs.HL := Regs.HL + 1;
          Regs.BC := Regs.BC - 1;
          Regs.HF := False;
          Regs.PVF := Regs.BC <> 0;
          Regs.NF := False;
          if (Regs.BC <> 0) then Regs.PC := Regs.PC - 2;
        end;
        $B1: // CPIR
        begin
          c1 := Regs.A;
          c2 := ReadByte(Regs.HL);
          b := c1 - c2;
          Regs.HL := Regs.HL + 1;
          Regs.BC := Regs.BC - 1;
          Regs.SF := (b and $80) <> 0;
          Regs.ZF := b = 0;
          Regs.HF := ((c1 and 8) = 0) and ((c2 and 8) <> 0);
          Regs.PVF := Regs.BC <> 0;
          Regs.NF := True;
          if (Regs.BC <> 0) then Regs.PC := Regs.PC - 2;
        end;
        $B2: // INIR
        begin
          b := ReadPort(Regs.C);
          WriteByte(Regs.HL, b);
          Regs.HL := Regs.HL + 1;
          Regs.B := Regs.B - 1;
          Regs.SF := False;
          Regs.ZF := True;
          Regs.HF := False;
          Regs.PVF := False;
          Regs.NF := True;
          if (Regs.B <> 0) then Regs.PC := Regs.PC - 2;
        end;
        $B3: // OTIR
        begin
          b := ReadByte(Regs.HL);
          WritePort(Regs.C, b);
          Regs.HL := Regs.HL + 1;
          Regs.B := Regs.B - 1;
          Regs.SF := False;
          Regs.ZF := True;
          Regs.HF := False;
          Regs.PVF := False;
          Regs.NF := True;
          if (Regs.B <> 0) then Regs.PC := Regs.PC - 2;
        end;
        $B8: // LDDR
        begin
          WriteByte(Regs.DE, ReadByte(Regs.HL));
          Regs.DE := Regs.DE - 1;
          Regs.HL := Regs.HL - 1;
          Regs.BC := Regs.BC - 1;
          Regs.HF := False;
          Regs.PVF := Regs.BC <> 0;
          Regs.NF := False;
          if (Regs.BC <> 0) then Regs.PC := Regs.PC - 2;
        end;
        $B9: // CPDR
        begin
          c1 := Regs.A;
          c2 := ReadByte(Regs.HL);
          b := c1 - c2;
          Regs.HL := Regs.HL - 1;
          Regs.BC := Regs.BC - 1;
          Regs.SF := (b and $80) <> 0;
          Regs.ZF := b = 0;
          Regs.HF := ((c1 and 8) = 0) and ((c2 and 8) <> 0);
          Regs.PVF := Regs.BC <> 0;
          Regs.NF := True;
          if (Regs.BC <> 0) then Regs.PC := Regs.PC - 2;
        end;
        $BA: // INDR
        begin
          b := ReadPort(Regs.C);
          WriteByte(Regs.HL, b);
          Regs.HL := Regs.HL - 1;
          Regs.B := Regs.B - 1;
          Regs.SF := False;
          Regs.ZF := True;
          Regs.HF := False;
          Regs.PVF := False;
          Regs.NF := True;
          if (Regs.B <> 0) then Regs.PC := Regs.PC - 2;
        end;
        $BB: // OTDR
        begin
          b := ReadByte(Regs.HL);
          WritePort(Regs.C, b);
          Regs.HL := Regs.HL - 1;
          Regs.B := Regs.B - 1;
          Regs.SF := False;
          Regs.ZF := True;
          Regs.HF := False;
          Regs.PVF := False;
          Regs.NF := True;
          if (Regs.B <> 0) then Regs.PC := Regs.PC - 2;
        end;
      else
        FErrors.Add(Format('Invalid opcode ED %2.2X', [code]));
      end;
    end;
  end;
  FLastTime := FLastTime + GetCycle(FCodes);
  FCanPaint := FVideo.UpdateState(FLastTime);
  FCodes := nil;
  if AUpdated then UpdateSelectIndexs;
end;

procedure TXPDebug.StepOver;
begin
  FReturnCount := -1;
  if FLines.SelectIndex >= 0 then
    RunTo(NextAddress(FLines[FLines.SelectIndex]))
  else RunTo(NextAddress(FSystemLines[FSystemLines.SelectIndex]));
end;

procedure TXPDebug.Stop;
begin
  FExecuted := False;
  FReturnCount := -1;
end;

procedure TXPDebug.SystemDecode(AMaxLine: Integer);
var
  n, nmax, len, pc: Integer;
  line: TXPDebugLine;
begin
  if (FSystemLines.FSelectIndex < 0) or (FSystemLines.FSelectOffset > 0) then FSystemLines.Clear;
  len := FSystemLines.Count;
  n := Max(0, FSystemLines.FSelectIndex);
  nmax := n + Max(AMaxLine, TXPConfig.MaxSystemLine);
  pc := FRegs.PC;
  while n < nmax do
  begin
    if n < len then
      line := FSystemLines[n]
    else
    begin
      line := DecodeLine(pc);
      FSystemLines.Add(line);
    end;
    pc := pc + Length(line.FCodes);
    Inc(n);
  end;
end;

procedure TXPDebug.UpdateSelectIndexs;
begin
  FLines.UpdateSelectIndex(FRegs.PC);
  FSystemLines.UpdateSelectIndex(FRegs.PC);
end;

procedure TXPDebug.WriteByte(AAddress: Integer; AValue: Byte; AUpdated: Boolean);
begin
  FMapper.Memory[AAddress] := AValue;
  if AUpdated then
  begin
    if FMapper.Writed then
      AddMessage(Format('RAM (%4.4Xh) <- %2.2Xh (%d)', [AAddress, AValue, AValue]))
    else if FMapper.ReadOnly then
      AddMessage(Format('ROM (%4.4Xh) <- %2.2Xh (%d)', [AAddress, AValue, AValue]))
    else AddMessage(Format('Can not write memory at %4.4Xh', [AAddress]));
  end;
end;

procedure TXPDebug.WritePort(APort, AValue: Byte; AUpdated: Boolean);
var
  mark: string;
begin
  mark := '';
  case APort of
    $98..$9B: FVideo.WritePort(APort, AValue);
    $A8:
    begin
      FMapper[0] := AValue and 3;
      FMapper[1] := (AValue shr 2) and 3;
      FMapper[2] := (AValue shr 4) and 3;
      FMapper[3] := (AValue shr 6) and 3;
    end;
    $B4..$B5: FClock.WritePort(APort, AValue);
    $FC..$FF: FMainRAM[APort and 3] := AValue and 7;
  else
    mark := '*** ';
  end;
  if AUpdated then AddMessage(Format('%sPort (%2.2Xh) <- %2.2Xh (%d)', [mark, APort, AValue, AValue]));
end;

procedure TXPDebug.WriteWord(AAddress: Integer; AValue: Word; AUpdated: Boolean);
begin
  WriteByte(AAddress, AValue and 255);
  WriteByte(AAddress + 1, AValue shr 8);
  if AUpdated then AddMessage(Format('RAM (%4.4Xh) <- %4.4Xh (%d)', [AAddress, AValue, AValue]));
end;

{ TXPDebugLines }

function TXPDebugLines.Add(ALine: TXPDebugLine): Integer;
begin
  Result := FLines.Add(ALine);
end;

procedure TXPDebugLines.AddLine(AAddress: Integer; ACodes: array of Byte;
  AText: string; ACycle: Integer);
var
  a: array of Byte;
  n, m, len: Integer;
begin
  n := 0;
  len := Length(ACodes);
  while n < len do
  begin
    m := 0;
    a := nil;
    while (m < 4) and (n < len) do
    begin
      SetLength(a, m + 1);
      a[m] := ACodes[n];
      Inc(n);
      Inc(m);
    end;
    Add(TXPDebugLine.Create(AAddress, a, AText, ACycle));
    Inc(AAddress, m);
    AText := '';
  end;
end;

procedure TXPDebugLines.Clear;
var
  line: TXPDebugLine;
  n: Integer;
begin
  n := FLines.Count;
  while n > 0 do
  begin
    Dec(n);
    line := FLines[n];
    FLines[n] := nil;
    line.Free;
  end;
  FLines.Clear;
  FSelectIndex := 0;
  FSelectOffset := 0;
end;

function TXPDebugLines.Count: Integer;
begin
  Result := FLines.Count;
end;

constructor TXPDebugLines.Create;
begin
  FLines := TList.Create;
  FSelectIndex := 0;
  FSelectOffset := 0;
end;

destructor TXPDebugLines.Destroy;
begin
  Clear;
  FreeAndNil(FLines);
  inherited Destroy;
end;

function TXPDebugLines.GetLines(AIndex: Integer): TXPDebugLine;
begin
  if (AIndex >= 0) and (AIndex < FLines.Count) then
    Result := TXPDebugLine(FLines[AIndex])
  else Result := nil;
end;

function TXPDebugLines.SelectedAddress(AIndex: Integer): Integer;
var
  line: TXPDebugLine;
begin
  if (AIndex >= 0) and (AIndex < FLines.Count) then
    line := FLines[AIndex]
  else line := nil;
  if line <> nil then
    Result := line.Address
  else Result := -1;
end;

function TXPDebugLines.SelectedLine: TXPDebugLine;
begin
  if (FSelectIndex >= 0) and (FSelectIndex < FLines.Count) then
    Result := FLines[FSelectIndex]
  else Result := nil;
end;

procedure TXPDebugLines.UpdateSelectIndex(AAddress: Integer);
var
  n, len, ad1, ad2: Integer;
  line: TXPDebugLine;
begin
  len := FLines.Count;
  SelectIndex := -1;
  SelectOffset := -1;
  if len > 0 then
  begin
    line := Lines[len - 1];
    ad1 := Lines[0].Address;
    ad2 := line.Address + Length(line.FCodes);
    if (AAddress >= ad1) and (AAddress < ad2)  then
    begin
      n := 0;
      ad1 := -1;
      while (n < len) and (AAddress > ad1) do
      begin
        line := FLines[n];
        ad1 := line.Address;
        if AAddress >= ad1 then
        begin
          SelectIndex := n;
          SelectOffset := AAddress - ad1;
        end;
        Inc(n);
      end;
    end;
  end;
end;

{ TXPDebugLine }

procedure TXPDebugLine.AddCode(ACodes: array of Byte);
var
  n: Integer;
begin
  for n := 0 to Length(ACodes) - 1 do AddCode(ACodes[n]);
end;

procedure TXPDebugLine.AddCode(ACode: Byte);
var
  n: Integer;
begin
  n := Length(FCodes);
  SetLength(FCodes, n + 1);
  FCodes[n] := ACode;
end;

constructor TXPDebugLine.Create(AAddress: Integer; ACodes: array of Byte;
  AText: string; ACycle: Integer);
var
  n: Integer;
begin
  FAddress := AAddress;
  FText := AText;
  FBreaked := False;
  FCycle := ACycle;
  n := Length(ACodes);
  SetLength(FCodes, n);
  while n > 0 do
  begin
    Dec(n);
    FCodes[n] := ACodes[n];
  end;
end;

constructor TXPDebugLine.Create(AAddress: Integer);
begin
  FAddress := AAddress;
  FCodes := nil;
  FText := '';
  FCycle := 0;
  FBreaked := False;
end;

destructor TXPDebugLine.Destroy;
begin
  FCodes := nil;
  inherited Destroy;
end;

procedure TXPDebugLine.SetText(ACode, AParam: string);
begin
  FText := GetTabStr(ACode) + AParam;
end;

{ TXPDebugFlags }

function TXPDebugFlag.GetBit(const Index: Integer): Integer;
begin
  if (FFlag and (Index and 255)) <> 0 then
    Result := 1
  else Result := 0;
end;

function TXPDebugFlag.GetBool(const Index: Integer): Boolean;
begin
  Result := (FFlag and (Index and 255)) <> 0;
end;

procedure TXPDebugFlag.SetBit(const Index: Integer; Value: Integer);
begin
  SetBool(Index, Value <> 0);
end;

procedure TXPDebugFlag.SetBool(const Index: Integer; Value: Boolean);
begin
  if Value then
    FFlag := FFlag or (Index and 255)
  else FFlag := FFlag and (Index shr 8);
end;

{ TXPDebugRegs }

procedure TXPDebugRegs.Assign(ARegs: TXPDebugRegs);
begin
  FReg := ARegs.FReg;
  FAffected.Clear;
end;

procedure TXPDebugRegs.Clear;
begin
  FAffected.Clear;
  FillChar(FReg, SizeOf(FReg), 0);
end;

constructor TXPDebugRegs.Create;
begin
  FAffected := TXPAffected.Create;
end;

destructor TXPDebugRegs.Destroy;
begin
  FreeAndNil(FAffected);
  inherited Destroy;
end;

function TXPDebugRegs.GetCBit: Byte;
begin
  Result := FReg.F.BitC;
end;

function TXPDebugRegs.GetCBit_: Byte;
begin
  Result := FReg.F_.BitC;
end;

function TXPDebugRegs.GetCF: Boolean;
begin
  Result := Freg.F.C;
end;

function TXPDebugRegs.GetCF_: Boolean;
begin
  Result := FReg.F_.C;
end;

function TXPDebugRegs.GetF: Byte;
begin
  Result := FReg.F.Value;
end;

function TXPDebugRegs.GetF_: Byte;
begin
  Result := FReg.F_.Value;
end;

function TXPDebugRegs.GetHBit: Byte;
begin
  Result := FReg.F.BitH;
end;

function TXPDebugRegs.GetHBit_: Byte;
begin
  Result := FReg.F_.BitH;
end;

function TXPDebugRegs.GetHF: Boolean;
begin
  Result := Freg.F.H;
end;

function TXPDebugRegs.GetHF_: Boolean;
begin
  Result := FReg.F_.H;
end;

function TXPDebugRegs.GetNBit: Byte;
begin
  Result := FReg.F.BitN;
end;

function TXPDebugRegs.GetNBit_: Byte;
begin
  Result := FReg.F_.BitN;
end;

function TXPDebugRegs.GetNF: Boolean;
begin
  Result := Freg.F.N;
end;

function TXPDebugRegs.GetNF_: Boolean;
begin
  Result := FReg.F_.N;
end;

function TXPDebugRegs.GetPVBit: Byte;
begin
  Result := FReg.F.BitPV;
end;

function TXPDebugRegs.GetPVBit_: Byte;
begin
  Result := FReg.F_.BitPV;
end;

function TXPDebugRegs.GetPVF: Boolean;
begin
  Result := Freg.F.PV;
end;

function TXPDebugRegs.GetPVF_: Boolean;
begin
  Result := FReg.F_.PV;
end;

function TXPDebugRegs.GetSBit: Byte;
begin
  Result := FReg.F.BitS;
end;

function TXPDebugRegs.GetSBit_: Byte;
begin
  Result := FReg.F_.BitS;
end;

function TXPDebugRegs.GetSF: Boolean;
begin
  Result := Freg.F.S;
end;

function TXPDebugRegs.GetSF_: Boolean;
begin
  Result := FReg.F_.S;
end;

function TXPDebugRegs.GetX1Bit: Byte;
begin
  Result := FReg.F.BitX1;
end;

function TXPDebugRegs.GetX1Bit_: Byte;
begin
  Result := FReg.F_.BitX1;
end;

function TXPDebugRegs.GetX2Bit: Byte;
begin
  Result := FReg.F.BitX2;
end;

function TXPDebugRegs.GetX2Bit_: Byte;
begin
  Result := FReg.F_.BitX2;
end;

function TXPDebugRegs.GetXF1: Boolean;
begin
  Result := Freg.F.X1;
end;

function TXPDebugRegs.GetXF1_: Boolean;
begin
  Result := FReg.F_.X1;
end;

function TXPDebugRegs.GetXF2: Boolean;
begin
  Result := Freg.F.X2;
end;

function TXPDebugRegs.GetXF2_: Boolean;
begin
  Result := FReg.F_.X2;
end;

function TXPDebugRegs.GetZBit: Byte;
begin
  Result := FReg.F.BitZ;
end;

function TXPDebugRegs.GetZBit_: Byte;
begin
  Result := FReg.F_.BitZ;
end;

function TXPDebugRegs.GetZF: Boolean;
begin
  Result := Freg.F.Z;
end;

function TXPDebugRegs.GetZF_: Boolean;
begin
  Result := FReg.F_.Z;
end;

procedure TXPDebugRegs.SetA(const Value: Byte);
begin
  FReg.A := Value;
  FAffected.Add('A');
end;

procedure TXPDebugRegs.SetAF(const Value: Word);
begin
  FReg.AF := Value;
  FAffected.Add(['A', 'F']);
end;

procedure TXPDebugRegs.SetAF_(const Value: Word);
begin
  FReg.AF_ := Value;
  FAffected.Add(['A_', 'F_']);
end;

procedure TXPDebugRegs.SetA_(const Value: Byte);
begin
  FReg.A_ := Value;
  FAffected.Add('A_');
end;

procedure TXPDebugRegs.SetB(const Value: Byte);
begin
  FReg.B := Value;
  FAffected.Add('B');
end;

procedure TXPDebugRegs.SetBC(const Value: Word);
begin
  FReg.BC := Value;
  FAffected.Add(['B', 'C']);
end;

procedure TXPDebugRegs.SetBC_(const Value: Word);
begin
  FReg.BC_ := Value;
  FAffected.Add(['B_', 'C_']);
end;

procedure TXPDebugRegs.SetB_(const Value: Byte);
begin
  FReg.B_ := Value;
  FAffected.Add('B_');
end;

procedure TXPDebugRegs.SetC(const Value: Byte);
begin
  FReg.C := Value;
  FAffected.Add('C');
end;

procedure TXPDebugRegs.SetCBit(const Value: Byte);
begin
  FReg.F.BitC := Value;
  FAffected.Add(['CF', 'F']);
end;

procedure TXPDebugRegs.SetCBit_(const Value: Byte);
begin
  FReg.F_.BitC := Value;
  FAffected.Add(['CF_', 'F_']);
end;

procedure TXPDebugRegs.SetCF(const Value: Boolean);
begin
  FReg.F.C := Value;
  FAffected.Add(['CF', 'F']);
end;

procedure TXPDebugRegs.SetCF_(const Value: Boolean);
begin
  FReg.F_.C := Value;
  FAffected.Add(['CF_', 'F_']);
end;

procedure TXPDebugRegs.SetC_(const Value: Byte);
begin
  FReg.C_ := Value;
  FAffected.Add('C_');
end;

procedure TXPDebugRegs.SetD(const Value: Byte);
begin
  FReg.D := Value;
  FAffected.Add('D');
end;

procedure TXPDebugRegs.SetDE(const Value: Word);
begin
  FReg.DE := Value;
  FAffected.Add(['D', 'E']);
end;

procedure TXPDebugRegs.SetDE_(const Value: Word);
begin
  FReg.DE_ := Value;
  FAffected.Add(['D_', 'E_']);
end;

procedure TXPDebugRegs.SetD_(const Value: Byte);
begin
  FReg.D_ := Value;
  FAffected.Add('D_');
end;

procedure TXPDebugRegs.SetE(const Value: Byte);
begin
  FReg.E := Value;
  FAffected.Add('E');
end;

procedure TXPDebugRegs.SetE_(const Value: Byte);
begin
  FReg.E_ := Value;
  FAffected.Add('E_');
end;

procedure TXPDebugRegs.SetF(const Value: Byte);
begin
  FReg.F.Value := Value;
  FAffected.Add(['F', 'SF', 'ZF', 'XF1', 'HF', 'XF2', 'PVF', 'NF', 'CF']);
end;

procedure TXPDebugRegs.SetF_(const Value: Byte);
begin
  FReg.F_.Value := Value;
  FAffected.Add(['F_', 'SF_', 'ZF_', 'XF1_', 'HF_', 'XF2_', 'PVF_', 'NF_', 'CF_']);
end;

procedure TXPDebugRegs.SetH(const Value: Byte);
begin
  FReg.H := Value;
  FAffected.Add('H');
end;

procedure TXPDebugRegs.SetHBit(const Value: Byte);
begin
  FReg.F.BitH := Value;
  FAffected.Add(['F', 'HF']);
end;

procedure TXPDebugRegs.SetHBit_(const Value: Byte);
begin
  FReg.F_.BitH := Value;
  FAffected.Add(['HF_', 'F_']);
end;

procedure TXPDebugRegs.SetHF(const Value: Boolean);
begin
  FReg.F.H := Value;
  FAffected.Add(['F', 'HF']);
end;

procedure TXPDebugRegs.SetHF_(const Value: Boolean);
begin
  FReg.F_.H := Value;
  FAffected.Add(['HF_', 'F_']);
end;

procedure TXPDebugRegs.SetHL(const Value: Word);
begin
  FReg.HL := Value;
  FAffected.Add(['H', 'L']);
end;

procedure TXPDebugRegs.SetHL_(const Value: Word);
begin
  FReg.HL_ := Value;
  FAffected.Add(['H_', 'L_']);
end;

procedure TXPDebugRegs.SetH_(const Value: Byte);
begin
  FReg.H_ := Value;
  FAffected.Add('H_');
end;

procedure TXPDebugRegs.SetI(const Value: Byte);
begin
  FReg.I := Value;
  FAffected.Add('I');
end;

procedure TXPDebugRegs.SetIX(const Value: Word);
begin
  FReg.IX := Value;
  FAffected.Add('IX');
end;

procedure TXPDebugRegs.SetIXH(const Value: Byte);
begin
  FReg.IXH := Value;
  FAffected.Add('IX');
end;

procedure TXPDebugRegs.SetIXL(const Value: Byte);
begin
  FReg.IXL := Value;
  FAffected.Add('IX');
end;

procedure TXPDebugRegs.SetIY(const Value: Word);
begin
  FReg.IY := Value;
  FAffected.Add('IY');
end;

procedure TXPDebugRegs.SetIYH(const Value: Byte);
begin
  FReg.IYH := Value;
  FAffected.Add('IY');
end;

procedure TXPDebugRegs.SetIYL(const Value: Byte);
begin
  FReg.IYL := Value;
  FAffected.Add('IY');
end;

procedure TXPDebugRegs.SetL(const Value: Byte);
begin
  FReg.L := Value;
  FAffected.Add('L');
end;

procedure TXPDebugRegs.SetL_(const Value: Byte);
begin
  FReg.L_ := Value;
  FAffected.Add('L_');
end;

procedure TXPDebugRegs.SetNBit(const Value: Byte);
begin
  FReg.F.BitN := Value;
  FAffected.Add(['F', 'NF']);
end;

procedure TXPDebugRegs.SetNBit_(const Value: Byte);
begin
  FReg.F_.BitN := Value;
  FAffected.Add(['NF_', 'F_']);
end;

procedure TXPDebugRegs.SetNF(const Value: Boolean);
begin
  FReg.F.N := Value;
  FAffected.Add(['F', 'NF']);
end;

procedure TXPDebugRegs.SetNF_(const Value: Boolean);
begin
  FReg.F_.N := Value;
  FAffected.Add(['NF_', 'F_']);
end;

procedure TXPDebugRegs.SetPC(const Value: Word);
begin
  FReg.PC := Value;
  FAffected.Add('PC');
end;

procedure TXPDebugRegs.SetPVBit(const Value: Byte);
begin
  FReg.F.BitPV := Value;
  FAffected.Add(['F', 'PVF']);
end;

procedure TXPDebugRegs.SetPVBit_(const Value: Byte);
begin
  FReg.F_.BitPV := Value;
  FAffected.Add(['PVF_', 'F_']);
end;

procedure TXPDebugRegs.SetPVF(const Value: Boolean);
begin
  FReg.F.PV := Value;
  FAffected.Add(['F', 'PVF']);
end;

procedure TXPDebugRegs.SetPVF_(const Value: Boolean);
begin
  FReg.F_.PV := Value;
  FAffected.Add(['PVF_', 'F_']);
end;

procedure TXPDebugRegs.SetR(const Value: Byte);
begin
  FReg.R := Value;
  FAffected.Add('R');
end;

procedure TXPDebugRegs.SetSBit(const Value: Byte);
begin
  FReg.F.BitS := Value;
  FAffected.Add(['F', 'SF']);
end;

procedure TXPDebugRegs.SetSBit_(const Value: Byte);
begin
  FReg.F_.BitS := Value;
  FAffected.Add(['SF_', 'F_']);
end;

procedure TXPDebugRegs.SetSF(const Value: Boolean);
begin
  FReg.F.S := Value;
  FAffected.Add(['F', 'SF']);
end;

procedure TXPDebugRegs.SetSF_(const Value: Boolean);
begin
  FReg.F_.S := Value;
  FAffected.Add(['SF_', 'F_']);
end;

procedure TXPDebugRegs.SetSP(const Value: Word);
begin
  FReg.SP := Value;
  FAffected.Add('SP');
end;

procedure TXPDebugRegs.SetX1Bit(const Value: Byte);
begin
  FReg.F.BitX1 := Value;
  FAffected.Add(['F', 'XF1']);
end;

procedure TXPDebugRegs.SetX1Bit_(const Value: Byte);
begin
  FReg.F_.BitX1 := Value;
  FAffected.Add(['XF1_', 'F_']);
end;

procedure TXPDebugRegs.SetX2Bit(const Value: Byte);
begin
  FReg.F.BitX2 := Value;
  FAffected.Add(['F', 'XF2']);
end;

procedure TXPDebugRegs.SetX2Bit_(const Value: Byte);
begin
  FReg.F_.BitX2 := Value;
  FAffected.Add(['XF2_', 'F_']);
end;

procedure TXPDebugRegs.SetXF1(const Value: Boolean);
begin
  FReg.F.X1 := Value;
  FAffected.Add(['F', 'XF1']);
end;

procedure TXPDebugRegs.SetXF1_(const Value: Boolean);
begin
  FReg.F_.X1 := Value;
  FAffected.Add(['XF1_', 'F_']);
end;

procedure TXPDebugRegs.SetXF2(const Value: Boolean);
begin
  FReg.F.X2 := Value;
  FAffected.Add(['F', 'XF2']);
end;

procedure TXPDebugRegs.SetXF2_(const Value: Boolean);
begin
  FReg.F_.X2 := Value;
  FAffected.Add(['XF2_', 'F_']);
end;

procedure TXPDebugRegs.SetZBit(const Value: Byte);
begin
  FReg.F.BitZ := Value;
  FAffected.Add(['F', 'ZF']);
end;

procedure TXPDebugRegs.SetZBit_(const Value: Byte);
begin
  FReg.F_.BitZ := Value;
  FAffected.Add(['ZF_', 'F_']);
end;

procedure TXPDebugRegs.SetZF(const Value: Boolean);
begin
  FReg.F.Z := Value;
  FAffected.Add(['F', 'ZF']);
end;

procedure TXPDebugRegs.SetZF_(const Value: Boolean);
begin
  FReg.F_.Z := Value;
  FAffected.Add(['ZF_', 'F_']);
end;

{ TXPDebugThread }

constructor TXPDebugThread.Create(ADebug: TXPDebug);
begin
  inherited Create(True);
  FDebug := ADebug;
  FBreakPoints := TXPBreakPoints.Create;
  OnTerminate := SyncStop;
  FreeOnTerminate := False;
end;

destructor TXPDebugThread.Destroy;
begin
  FreeAndNil(FBreakPoints);
  inherited Destroy;
end;

procedure TXPDebugThread.DoPaint;
begin
  if Assigned(FDebug.FOnPaint) then FDebug.FOnPaint(Self);
end;

procedure TXPDebugThread.DoStop;
begin
  if Assigned(FDebug.FOnStop) then FDebug.FOnStop(Self);
end;

procedure TXPDebugThread.DoUpdate;
begin
  if Assigned(FDebug.FOnUpdate) then FDebug.FOnUpdate(Self);
end;

procedure TXPDebugThread.SetBreakPoints(AValue: TXPBreakPoints);
begin
  FBreakPoints.Assign(AValue);
end;

procedure TXPDebugThread.SyncStop(Sender: TObject);
begin
  DoStop;
end;

procedure TXPDebugThread.Execute;
begin
  while not Terminated and FDebug.FExecuted do
  begin
    FDebug.StepInto(False);
    if FBreakPoints[FDebug.Regs.PC] // or (FDebug.FErrors.Count > 0)
      or (FDebug.FReturnCount = 0)
    then FDebug.FExecuted := False;
    if Assigned(FDebug.FOnPaint) and FDebug.FCanPaint then Synchronize(DoPaint);
    if Assigned(FDebug.FOnUpdate) then Synchronize(DoUpdate);
  end;
  FDebug.UpdateSelectIndexs;
end;

{ TXPBreakPoints }

procedure TXPBreakPoints.Add(AAddress: Integer);
var
  n: Integer;
begin
  n := IndexOf(AAddress);
  if n < 0 then
  begin
    n := Length(FPoints);
    SetLength(FPoints, n + 1);
    FPoints[n] := AAddress;
  end;
end;

procedure TXPBreakPoints.Add(AAddresses: array of Integer);
var
  n: Integer;
begin
  n := Length(AAddresses);
  while n > 0 do
  begin
    Dec(n);
    Add(AAddresses[n]);
  end;
end;

procedure TXPBreakPoints.Assign(ABreakPoints: TXPBreakPoints);
var
  n: Integer;
begin
  n := ABreakPoints.Count;
  SetLength(FPoints, n);
  while n > 0 do
  begin
    Dec(n);
    FPoints[n] := ABreakPoints.FPoints[n];
  end;
end;

procedure TXPBreakPoints.Clear;
begin
  FPoints := nil;
end;

function TXPBreakPoints.Count: Integer;
begin
  Result := Length(FPoints);
end;

constructor TXPBreakPoints.Create;
begin
  Clear;
end;

procedure TXPBreakPoints.Delete(AAddress: Integer);
var
  n: Integer;
begin
  n := IndexOf(AAddress);
  if n >= 0 then System.Delete(FPoints, n, 1);
end;

destructor TXPBreakPoints.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TXPBreakPoints.GetPoints(AAddress: Integer): Boolean;
begin
  Result := IndexOf(AAddress) >= 0;
end;

function TXPBreakPoints.IndexOf(AAddress: Integer): Integer;
var
  n: Integer;
begin
  Result := -1;
  n := Length(FPoints);
  while (n > 0) and (Result < 0) do
  begin
    Dec(n);
    if FPoints[n] = AAddress then Result := n;
  end;
end;

procedure TXPBreakPoints.SetPoints(AAddress: Integer; AValue: Boolean);
begin
  if AValue then
    Add(AAddress)
  else Delete(AAddress);
end;

end.
