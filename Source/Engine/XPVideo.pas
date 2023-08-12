unit XPVideo;

{$MODE Delphi}

interface

uses
  XPConfig, XPAffect,
  Classes, SysUtils, Graphics, IntfGraphics, LCLType;

//| R#0  | VDP(0)  | mode register #0                                         |
//| R#1  | VDP(1)  | mode register #1                                         |
//| R#2  | VDP(2)  | pattern name table                                       |
//| R#3  | VDP(3)  | colour table (LOW)                                       |
//| R#4  | VDP(4)  | pattern generator table                                  |
//| R#5  | VDP(5)  | sprite attribute table (LOW)                             |
//| R#6  | VDP(6)  | sprite pattern generator table                           |
//| R#7  | VDP(7)  | border colour/character colour at text mode              |
//| R#8  | VDP(9)  | mode register #2                                         |
//| R#9  | VDP(10) | mode register #3                                         |
//| R#10 | VDP(11) | colour table (HIGH)                                      |
//| R#11 | VDP(12) | sprite attribute table (HIGH)                            |
//| R#12 | VDP(13) | character colour at text blinks                          |
//| R#13 | VDP(14) | blinking period                                          |
//| R#14 | VDP(15) | VRAM access address (HIGH)                               |
//| R#15 | VDP(16) | indirect specification of S#n                            |
//| R#16 | VDP(17) | indirect specification of P#n                            |
//| R#17 | VDP(18) | indirect specification of R#n                            |
//| R#18 | VDP(19) | screen location adjustment (ADJUST)                      |
//| R#19 | VDP(20) | scanning line number when the interrupt occurs           |
//| R#20 | VDP(21) | colour burst signal 1                                    |
//| R#21 | VDP(22) | colour burst signal 2                                    |
//| R#22 | VDP(23) | colour burst signal 3                                    |
//| R#23 | VDP(24) | screen hard scroll                                       |
//|      |         |                                                          |
//| R#32 | VDP(33) | SX: X-coordinate to be transferred (LOW)                 |
//| R#33 | VDP(34) | SX: X-coordinate to be transferred (HIGH)                |
//| R#34 | VDP(35) | SY: Y-coordinate to be transferred (LOW)                 |
//| R#35 | VDP(36) | SY: Y-coordinate to be transferred (HIGH)                |
//| R#36 | VDP(37) | DX: X-coordinate to be transferred to (LOW)              |
//| R#37 | VDP(38) | DX: X-coordinate to be transferred to (HIGH)             |
//| R#38 | VDP(39) | DY: Y-coordinate to be transferred to (LOW)              |
//| R#39 | VDP(40) | DY: Y-coordinate to be transferred to (HIGH)             |
//| R#40 | VDP(41) | NX: num. of dots to be transferred in X direction (LOW)  |
//| R#41 | VDP(42) | NX: num. of dots to be transferred in X direction (HIGH) |
//| R#42 | VDP(43) | NY: num. of dots to be transferred in Y direction (LOW)  |
//| R#43 | VDP(44) | NY: num. of dots to be transferred in Y direction (HIGH) |
//| R#44 | VDP(45) | CLR: for transferring data to CPU                        |
//| R#45 | VDP(46) | ARG: bank switching between VRAM and expanded VRAM       |
//| R#46 | VDP(47) | CMR: send VDP command

//| S#0  | VDP(8)  | interrupt information                                    |
//| S#1  | VDP(-1) | interrupt information                                    |
//| S#2  | VDP(-2) | DP command control information/etc.                      |
//| S#3  | VDP(-3) | coordinate detected (LOW)                                |
//| S#4  | VDP(-4) | coordinate detected (HIGH)                               |
//| S#5  | VDP(-5) | coordinate detected (LOW)                                |
//| S#6  | VDP(-6) | coordinate detected (HIGH)                               |
//| S#7  | VDP(-7) | data obtained by VDP command                             |
//| S#8  | VDP(-8) | X-coordinate obtained by search command (LOW)            |
//| S#9  | VDP(-9) | X-coordinate obtained by search command (HIGH)

// SCREEN 0 = #0: xxxx000x, #1: xxx10xxx, Width 40         00010   10000
// SCREEN 0 = #0: xxxx010x, #1: xxx10xxx, Width 80         01010   10010
// SCREEN 1 = #0: xxxx000x, #1: xxx00xxx                   00000   00000
// SCREEN 2 = #0: xxxx001x, #1: xxx00xxx                   00100   00001
// SCREEN 3 = #0: xxxx000x, #1: xxx01xxx                   00001   01000
// SCREEN 4 = #0: xxxx010x, #1: xxx00xxx                   01000   00010
// SCREEN 5 = #0: xxxx011x, #1: xxx00xxx                   01100   00011
// SCREEN 6 = #0: xxxx100x, #1: xxx00xxx                   10000   00100
// SCREEN 7 = #0: xxxx101x, #1: xxx00xxx                   10100   00101
// SCREEN 8 = #0: xxxx111x, #1: xxx00xxx                   11100   00111

type
  PBGREntry = ^TBGREntry;
  TBGREntry = packed record
  case Integer of
    0: (Values: array[0..2] of Byte);
    1: (B, G, R: Byte);
  end;
  PBGRArray = ^TBGRArray;
  TBGRArray = array[0..32767] of TBGREntry;

  PBGRAEntry = ^TBGRAEntry;
  TBGRAEntry = packed record
  case Integer of
    0: (Values: array[0..3] of Byte);
    1: (B, G, R, A: Byte);
    2: (BGRA: Cardinal);
  end;
  PBGRAArray = ^TBGRAArray;
  TBGRAArray = array[0..32767] of TBGRAEntry;

  TXPVideoPal = class
  const
    CVals: array[0..7] of Byte = (0, 36, 73, 109, 146, 182, 219, 255);
  private
    FIndex: Integer;
    FAffected: TXPAffected;
    FValues: array[0..1] of Byte;
    procedure AddAffected;
    function GetPal(const Index: Integer): Byte;
    procedure SetPal(const Index: Integer; const Value: Byte);
    function GetGRB: Word;
    procedure SetGRB(const Value: Word);
    function GetValues(AIndex: Integer): Byte;
    procedure SetValues(AIndex: Integer; const Value: Byte);
    function GetColor: TColor;
    function GetCPal(const Index: Integer): Byte;
    procedure SetCPal(const Index: Integer; const Value: Byte);
    function GetBGRA: TBGRAEntry;
    procedure SetBGRA(const Value: TBGRAEntry);
    function GetBGR: TBGREntry;
    procedure SetBGR(const Value: TBGREntry);
  public
    constructor Create(AIndex: Integer; AAffected: TXPAffected);
    procedure Clear;
    procedure Assign(APal: TXPVideoPal);
    property R: Byte index $040F read GetPal write SetPal;
    property G: Byte index $10F0 read GetPal write SetPal;
    property B: Byte index $00F0 read GetPal write SetPal;
    property CR: Byte index $040F read GetCPal write SetCPal;
    property CG: Byte index $10F0 read GetCPal write SetCPal;
    property CB: Byte index $00F0 read GetCPal write SetCPal;
    property GRB: Word read GetGRB write SetGRB;
    property BGR: TBGREntry read GetBGR write SetBGR;
    property BGRA: TBGRAEntry read GetBGRA write SetBGRA;
    property Color: TColor read GetColor;
    property Values[AIndex: Integer]: Byte read GetValues write SetValues;
  end;

  PXPSpriteAttribute = ^TXPSpriteAttribute;
  TXPSpriteAttribute = packed record
    Y, X, N, C: Byte;
  end;

  TXPVideoReg = packed record
  case Integer of
    0: (
      Regs: array[0..46] of Byte;
      Status: array[0..9] of Byte;
    );
    1: (
      Controls: array[0..23] of Byte;
      Unused: array[0..7] of Byte;
      SX, SY, DX, DY, NX, NY: Word;
    );
  end;

  TXPVideo = class;
  TXPVideoRegs = class
  private
    FReg: TXPVideoReg;
    FPals: array[0..15] of TXPVideoPal;
    FVideo: TXPVideo;
    FAffected: TXPAffected;
    function GetReg8(AIndex: Integer): Byte;
    function GetReg16(AIndex: Integer): Word;
    procedure SetReg8(AIndex: Integer; AValue: Byte);
    procedure SetReg16(AIndex: Integer; AValue: Word);
    function GetStatus(AIndex: Integer): Byte;
    procedure SetStatus(AIndex: Integer; AValue: Byte);
    function GetPals(AIndex: Integer): TXPVideoPal;
    function GetColorTable: Word;
    procedure SetColorTable(AValue: Word);
    function GetMode: Integer;
    procedure SetMode(AValue: Integer);
    procedure SetAdjustX(AValue: Integer);
    procedure SetAdjustY(AValue: Integer);
    function GetAdjustX: Integer;
    function GetAdjustY: Integer;
    function GetStatusBool(AIndex: Integer): Boolean;
    procedure SetStatusBool(AIndex: Integer; AValue: Boolean);
  public
    constructor Create(AVideo: TXPVideo);
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(ARegs: TXPVideoRegs);

    property Affected: TXPAffected read FAffected write FAffected;
    property Regs[AIndex: Integer]: Byte read GetReg8 write SetReg8; default;
    property Reg16[AIndex: Integer]: Word read GetReg16 write SetReg16;
    property Status[AIndex: Integer]: Byte read GetStatus write SetStatus;
    property Pals[AIndex: Integer]: TXPVideoPal read GetPals;
    property ColorTable: Word read GetColorTable write SetColorTable;
    property Mode: Integer read GetMode write SetMode;
    property AdjustX: Integer read GetAdjustX write SetAdjustX;
    property AdjustY: Integer read GetAdjustY write SetAdjustY;

    property VInterrupt: Boolean index $70 read GetStatusBool write SetStatusBool;
    property SpriteOverflow: Boolean index $60 read GetStatusBool write SetStatusBool;
    property SpriteCollision: Boolean index $50 read GetStatusBool write SetStatusBool;
    property SpriteOverflowNumber: Byte index $0E00 read GetStatus write SetStatus;

    property LightpenFlag: Boolean index $71 read GetStatusBool write SetStatusBool;
    property LightpenPress: Boolean index $61 read GetStatusBool write SetStatusBool;
    property ChipID: Byte index $13E1 read GetStatus write SetStatus;
    property HInterrupt: Boolean index $01 read GetStatusBool write SetStatusBool;

    property TransferReady: Boolean index $72 read GetStatusBool write SetStatusBool;
    property VBlank: Boolean index $62 read GetStatusBool write SetStatusBool;
    property HBlank: Boolean index $52 read GetStatusBool write SetStatusBool;
    property BorderFound: Boolean index $42 read GetStatusBool write SetStatusBool;
    property EvenRow: Boolean index $12 read GetStatusBool write SetStatusBool;
    property CommandBusy: Boolean index $02 read GetStatusBool write SetStatusBool;

    property Mode0: Byte index 0 read GetReg8 write SetReg8;
    property Mode1: Byte index 1 read GetReg8 write SetReg8;
    property NameTable: Byte index 2 read GetReg8 write SetReg8;
    property ColorTableLow: Byte index 3 read GetReg8 write SetReg8;
    property PatternTable: Byte index 4 read GetReg8 write SetReg8;
    property SpriteAttributeLow: Byte index 5 read GetReg8 write SetReg8;
    property SpritePattern: Byte index 6 read GetReg8 write SetReg8;
    property TextColor: Byte index $4F007 read GetReg8 write SetReg8;
    property BGColor: Byte index $0F007 read GetReg8 write SetReg8;
    property Mode2: Byte index 8 read GetReg8 write SetReg8;
    property Mode3: Byte index 9 read GetReg8 write SetReg8;
    property Line212: Byte index $7FE09 read GetReg8 write SetReg8;
    property ColorTableHigh: Byte index 10 read GetReg8 write SetReg8;
    property SpriteAttributeHigh: Byte index 11 read GetReg8 write SetReg8;
    property BlinkColor: Byte index 12 read GetReg8 write SetReg8;
    property BlinkPeriod: Byte index 13 read GetReg8 write SetReg8;
    property MemoryAddressHigh: Byte index 14 read GetReg8 write SetReg8;
    property StatusIndex: Byte index 15 read GetReg8 write SetReg8;
    property PaletteIndex: Byte index 16 read GetReg8 write SetReg8;
    property RegisterIndex: Byte index 17 read GetReg8 write SetReg8;
    property Adjust: Byte index 18 read GetReg8 write SetReg8;
    property LineNumber: Byte index 19 read GetReg8 write SetReg8;
    property ColorBurst1: Byte index 20 read GetReg8 write SetReg8;
    property ColorBurst2: Byte index 21 read GetReg8 write SetReg8;
    property ColorBurst3: Byte index 22 read GetReg8 write SetReg8;
    property Scroll: Byte index 23 read GetReg8 write SetReg8;
    property SXL: Byte index 32 read GetReg8 write SetReg8;
    property SXH: Byte index 33 read GetReg8 write SetReg8;
    property SYL: Byte index 34 read GetReg8 write SetReg8;
    property SYH: Byte index 35 read GetReg8 write SetReg8;
    property DXL: Byte index 36 read GetReg8 write SetReg8;
    property DXH: Byte index 37 read GetReg8 write SetReg8;
    property DYL: Byte index 38 read GetReg8 write SetReg8;
    property DYH: Byte index 39 read GetReg8 write SetReg8;
    property NXL: Byte index 40 read GetReg8 write SetReg8;
    property NXH: Byte index 41 read GetReg8 write SetReg8;
    property NYL: Byte index 42 read GetReg8 write SetReg8;
    property NYH: Byte index 43 read GetReg8 write SetReg8;
    property Color: Byte index 44 read GetReg8 write SetReg8;
    property Param: Byte index 45 read GetReg8 write SetReg8;
    property Command: Byte index 46 read GetReg8 write SetReg8;
    property SX: Word index 32 read GetReg16 write SetReg16;
    property SY: Word index 34 read GetReg16 write SetReg16;
    property DX: Word index 36 read GetReg16 write SetReg16;
    property DY: Word index 38 read GetReg16 write SetReg16;
    property NX: Word index 40 read GetReg16 write SetReg16;
    property NY: Word index 42 read GetReg16 write SetReg16;
    property Param_YLonger: Byte index $0FE2D read GetReg8 write SetReg8;     // for LINE cmd, Y is longest
    property Param_DiffColor: Byte index $1FE2D read GetReg8 write SetReg8;   // search for match or different color
    property Param_XReverse: Byte index $2FE2D read GetReg8 write SetReg8;    // NX is negative
    property Param_YReverse: Byte index $3FE2D read GetReg8 write SetReg8;    // NY is negative
    property Param_SrcExtVRAM: Byte index $4FE2D read GetReg8 write SetReg8;  // Source is extended VRAM
    property Param_DestExtVRAM: Byte index $5FE2D read GetReg8 write SetReg8; // Destination is extended VRAM
    property Param_ExtRAM: Byte index $6FE2D read GetReg8 write SetReg8;      // VRAM or Extended RAM
    property S_Int0: Byte index 0 read GetStatus write SetStatus;
    property S_Int1: Byte index 1 read GetStatus write SetStatus;
    property S_DPCmd: Byte index 2 read GetStatus write SetStatus;
    property S_Coor0Low: Byte index 3 read GetStatus write SetStatus;
    property S_Coor0High: Byte index 4 read GetStatus write SetStatus;
    property S_Coor1Low: Byte index 5 read GetStatus write SetStatus;
    property S_Coor1High: Byte index 6 read GetStatus write SetStatus;
    property S_Data: Byte index 7 read GetStatus write SetStatus;
    property S_XSearchLow: Byte index 8 read GetStatus write SetStatus;
    property S_XSearchHigh: Byte index 9 read GetStatus write SetStatus;
  end;

  TXPVideoMemory = array[0..131071] of Byte;

  TXPVideo = class
  const
    MODE_SCREEN0 = $10;
    MODE_SCREEN9 = $12;
    MODE_SCREEN1 = $00;
    MODE_SCREEN2 = $01;
    MODE_SCREEN3 = $08;
    MODE_SCREEN4 = $02;
    MODE_SCREEN5 = $03;
    MODE_SCREEN6 = $04;
    MODE_SCREEN7 = $05;
    MODE_SCREEN8 = $07;
    CyclePerScanLine = 147;
    BorderWidth = 30;
    BorderHeight = 30;

    // VDP Commands
    CMDSTOP         =     $00;     // Stop
    CMDINVALID1     =     $10;     // Not used
    CMDINVALID2     =     $20;     // Not used
    CMDINVALID3     =     $30;     // Not used
    CMDPOINT        =     $40;     // Point
    CMDPSET         =     $50;     // Pset
    CMDSEARCH       =     $60;     // Search
    CMDLINE         =     $70;     // Line
    CMDLMMV         =     $80;     // Low speed fill
    CMDLMMM         =     $90;     // Low speed copy
    CMDLMCM         =     $A0;     // Low speed getpixels
    CMDLMMC         =     $B0;     // Low speed setpixels
    CMDHMMV         =     $C0;     // High speed fill
    CMDHMMM         =     $D0;     // High speed copy
    CMDYMMM         =     $E0;     // High speed copy y-direction
    CMDHMMC         =     $F0;     // High speed setpixels

    // VDP Command Logical Operations
    LCMD_IMP        =     $00;     // DES = SRC
    LCMD_AND        =     $01;     // DES = SRC and DES
    LCMD_OR         =     $02;     // DES = SRC or DES
    LCMD_EOR        =     $03;     // DES = SRC xor DES
    LCMD_NOT        =     $04;     // DES = not SRC
    LCMD_TIMP       =     $08;     // Ignore CL0, DES = SRC
    LCMD_TAND       =     $09;     // Ignore CL0, DES = SRC and DES
    LCMD_TOR        =     $0A;     // Ignore CL0, DES = SRC or DES
    LCMD_TEOR       =     $0B;     // Ignore CL0, DES = SRC xor DES
    LCMD_TNOT       =     $0C;     // Ignore CL0, DES = not SRC

    ScreenVDP: array[0..9, 0..23] of Byte = (
      (
        $00, $70, $00, $27, $01, $F7, $1E, $F4,   // SCREEN 0
        $08, $02, $00, $01, $00, $00, $00, $00,
        $0F, $00, $00, $00, $00, $3B, $05, $00
      ),
      (
        $00, $60, $06, $80, $00, $36, $07, $04,   // SCREEN 1
        $08, $02, $00, $00, $00, $00, $00, $00,
        $0F, $00, $00, $00, $00, $3B, $05, $00
      ),
      (
        $02, $60, $06, $FF, $03, $36, $07, $04,   // SCREEN 2
        $08, $02, $00, $00, $00, $00, $00, $00,
        $0F, $00, $00, $00, $00, $3B, $05, $00
      ),
      (
        $00, $68, $02, $FF, $00, $36, $07, $04,   // SCREEN 3
        $08, $02, $00, $00, $00, $00, $00, $00,
        $0F, $00, $00, $00, $00, $3B, $05, $00
      ),
      (
        $04, $60, $06, $FF, $03, $3F, $07, $04,   // SCREEN 4
        $08, $02, $00, $00, $00, $00, $00, $00,
        $0F, $00, $00, $00, $00, $3B, $05, $00
      ),
      (
        $06, $60, $1F, $FF, $03, $EF, $0F, $04,   // SCREEN 5
        $08, $82, $00, $00, $00, $00, $00, $00,
        $0F, $00, $00, $00, $00, $3B, $05, $00
      ),
      (
        $08, $60, $1F, $FF, $03, $EF, $0F, $04,   // SCREEN 6
        $08, $82, $00, $00, $00, $00, $00, $00,
        $0F, $00, $00, $00, $00, $3B, $05, $00
      ),
      (
        $0A, $60, $1F, $FF, $03, $F7, $1E, $04,   // SCREEN 7
        $08, $82, $00, $01, $00, $00, $00, $00,
        $0F, $00, $00, $00, $00, $3B, $05, $00
      ),
      (
        $0E, $60, $1F, $FF, $03, $F7, $1E, $04,   // SCREEN 8
        $08, $82, $00, $01, $00, $00, $00, $00,
        $0F, $00, $00, $00, $00, $3B, $05, $00
      ),
      (
        $04, $70, $03, $27, $02, $F7, $1E, $F4,   // SCREEN 0, WIDTH 80
        $08, $02, $00, $01, $17, $00, $00, $00,
        $0F, $00, $00, $00, $00, $3B, $05, $00
      )
    );

    ScreenStatus: array[0..9, 0..9] of Byte = (
      ($9F, $00, $6E, $00, $FE, $00, $FC, $04, $D2, $FE),   // SCREEN 0
      ($9F, $00, $4C, $00, $FE, $00, $FC, $04, $D2, $FE),   // SCREEN 1
      ($9F, $00, $6C, $00, $FE, $00, $FC, $04, $D2, $FE),   // SCREEN 2
      ($9F, $00, $4E, $00, $FE, $00, $FC, $04, $D2, $FE),   // SCREEN 3
      ($9F, $00, $2E, $00, $FE, $00, $FC, $04, $D2, $FE),   // SCREEN 4
      ($9F, $00, $4E, $00, $FE, $00, $FC, $44, $D2, $FE),   // SCREEN 5
      ($9F, $00, $4E, $00, $FE, $00, $FC, $55, $D2, $FE),   // SCREEN 6
      ($9F, $00, $6C, $00, $FE, $00, $FC, $44, $D2, $FE),   // SCREEN 7
      ($9F, $00, $4E, $00, $FE, $00, $FC, $04, $D2, $FE),   // SCREEN 8
      ($9F, $00, $2C, $00, $FE, $00, $FC, $04, $D2, $FE)    // SCREEN 0, WIDTH 80
    );

    DefaultPals: array[0..15] of Word = (
      $000, $000, $611, $733,
      $117, $327, $151, $627,
      $171, $373, $661, $663,
      $411, $265, $555, $777
    );

    Screen8Color: array[0..255] of TColor = (
      $00000000, $00550000, $00AA0000, $00FF0000, $00000024, $00550024, $00AA0024, $00FF0024,
      $00000048, $00550048, $00AA0048, $00FF0048, $0000006D, $0055006D, $00AA006D, $00FF006D,
      $00000091, $00550091, $00AA0091, $00FF0091, $000000B6, $005500B6, $00AA00B6, $00FF00B6,
      $000000DA, $005500DA, $00AA00DA, $00FF00DA, $000000FF, $005500FF, $00AA00FF, $00FF00FF,
      $00002400, $00552400, $00AA2400, $00FF2400, $00002424, $00552424, $00AA2424, $00FF2424,
      $00002448, $00552448, $00AA2448, $00FF2448, $0000246D, $0055246D, $00AA246D, $00FF246D,
      $00002491, $00552491, $00AA2491, $00FF2491, $000024B6, $005524B6, $00AA24B6, $00FF24B6,
      $000024DA, $005524DA, $00AA24DA, $00FF24DA, $000024FF, $005524FF, $00AA24FF, $00FF24FF,
      $00004800, $00554800, $00AA4800, $00FF4800, $00004824, $00554824, $00AA4824, $00FF4824,
      $00004848, $00554848, $00AA4848, $00FF4848, $0000486D, $0055486D, $00AA486D, $00FF486D,
      $00004891, $00554891, $00AA4891, $00FF4891, $000048B6, $005548B6, $00AA48B6, $00FF48B6,
      $000048DA, $005548DA, $00AA48DA, $00FF48DA, $000048FF, $005548FF, $00AA48FF, $00FF48FF,
      $00006D00, $00556D00, $00AA6D00, $00FF6D00, $00006D24, $00556D24, $00AA6D24, $00FF6D24,
      $00006D48, $00556D48, $00AA6D48, $00FF6D48, $00006D6D, $00556D6D, $00AA6D6D, $00FF6D6D,
      $00006D91, $00556D91, $00AA6D91, $00FF6D91, $00006DB6, $00556DB6, $00AA6DB6, $00FF6DB6,
      $00006DDA, $00556DDA, $00AA6DDA, $00FF6DDA, $00006DFF, $00556DFF, $00AA6DFF, $00FF6DFF,
      $00009100, $00559100, $00AA9100, $00FF9100, $00009124, $00559124, $00AA9124, $00FF9124,
      $00009148, $00559148, $00AA9148, $00FF9148, $0000916D, $0055916D, $00AA916D, $00FF916D,
      $00009191, $00559191, $00AA9191, $00FF9191, $000091B6, $005591B6, $00AA91B6, $00FF91B6,
      $000091DA, $005591DA, $00AA91DA, $00FF91DA, $000091FF, $005591FF, $00AA91FF, $00FF91FF,
      $0000B600, $0055B600, $00AAB600, $00FFB600, $0000B624, $0055B624, $00AAB624, $00FFB624,
      $0000B648, $0055B648, $00AAB648, $00FFB648, $0000B66D, $0055B66D, $00AAB66D, $00FFB66D,
      $0000B691, $0055B691, $00AAB691, $00FFB691, $0000B6B6, $0055B6B6, $00AAB6B6, $00FFB6B6,
      $0000B6DA, $0055B6DA, $00AAB6DA, $00FFB6DA, $0000B6FF, $0055B6FF, $00AAB6FF, $00FFB6FF,
      $0000DA00, $0055DA00, $00AADA00, $00FFDA00, $0000DA24, $0055DA24, $00AADA24, $00FFDA24,
      $0000DA48, $0055DA48, $00AADA48, $00FFDA48, $0000DA6D, $0055DA6D, $00AADA6D, $00FFDA6D,
      $0000DA91, $0055DA91, $00AADA91, $00FFDA91, $0000DAB6, $0055DAB6, $00AADAB6, $00FFDAB6,
      $0000DADA, $0055DADA, $00AADADA, $00FFDADA, $0000DAFF, $0055DAFF, $00AADAFF, $00FFDAFF,
      $0000FF00, $0055FF00, $00AAFF00, $00FFFF00, $0000FF24, $0055FF24, $00AAFF24, $00FFFF24,
      $0000FF48, $0055FF48, $00AAFF48, $00FFFF48, $0000FF6D, $0055FF6D, $00AAFF6D, $00FFFF6D,
      $0000FF91, $0055FF91, $00AAFF91, $00FFFF91, $0000FFB6, $0055FFB6, $00AAFFB6, $00FFFFB6,
      $0000FFDA, $0055FFDA, $00AAFFDA, $00FFFFDA, $0000FFFF, $0055FFFF, $00AAFFFF, $00FFFFFF
    );
    Screen8BGRA: array[0..255] of Cardinal = (
      $FF000000, $FF000055, $FF0000AA, $FF0000FF, $FF240000, $FF240055, $FF2400AA, $FF2400FF,
      $FF480000, $FF480055, $FF4800AA, $FF4800FF, $FF6D0000, $FF6D0055, $FF6D00AA, $FF6D00FF,
      $FF910000, $FF910055, $FF9100AA, $FF9100FF, $FFB60000, $FFB60055, $FFB600AA, $FFB600FF,
      $FFDA0000, $FFDA0055, $FFDA00AA, $FFDA00FF, $FFFF0000, $FFFF0055, $FFFF00AA, $FFFF00FF,
      $FF002400, $FF002455, $FF0024AA, $FF0024FF, $FF242400, $FF242455, $FF2424AA, $FF2424FF,
      $FF482400, $FF482455, $FF4824AA, $FF4824FF, $FF6D2400, $FF6D2455, $FF6D24AA, $FF6D24FF,
      $FF912400, $FF912455, $FF9124AA, $FF9124FF, $FFB62400, $FFB62455, $FFB624AA, $FFB624FF,
      $FFDA2400, $FFDA2455, $FFDA24AA, $FFDA24FF, $FFFF2400, $FFFF2455, $FFFF24AA, $FFFF24FF,
      $FF004800, $FF004855, $FF0048AA, $FF0048FF, $FF244800, $FF244855, $FF2448AA, $FF2448FF,
      $FF484800, $FF484855, $FF4848AA, $FF4848FF, $FF6D4800, $FF6D4855, $FF6D48AA, $FF6D48FF,
      $FF914800, $FF914855, $FF9148AA, $FF9148FF, $FFB64800, $FFB64855, $FFB648AA, $FFB648FF,
      $FFDA4800, $FFDA4855, $FFDA48AA, $FFDA48FF, $FFFF4800, $FFFF4855, $FFFF48AA, $FFFF48FF,
      $FF006D00, $FF006D55, $FF006DAA, $FF006DFF, $FF246D00, $FF246D55, $FF246DAA, $FF246DFF,
      $FF486D00, $FF486D55, $FF486DAA, $FF486DFF, $FF6D6D00, $FF6D6D55, $FF6D6DAA, $FF6D6DFF,
      $FF916D00, $FF916D55, $FF916DAA, $FF916DFF, $FFB66D00, $FFB66D55, $FFB66DAA, $FFB66DFF,
      $FFDA6D00, $FFDA6D55, $FFDA6DAA, $FFDA6DFF, $FFFF6D00, $FFFF6D55, $FFFF6DAA, $FFFF6DFF,
      $FF009100, $FF009155, $FF0091AA, $FF0091FF, $FF249100, $FF249155, $FF2491AA, $FF2491FF,
      $FF489100, $FF489155, $FF4891AA, $FF4891FF, $FF6D9100, $FF6D9155, $FF6D91AA, $FF6D91FF,
      $FF919100, $FF919155, $FF9191AA, $FF9191FF, $FFB69100, $FFB69155, $FFB691AA, $FFB691FF,
      $FFDA9100, $FFDA9155, $FFDA91AA, $FFDA91FF, $FFFF9100, $FFFF9155, $FFFF91AA, $FFFF91FF,
      $FF00B600, $FF00B655, $FF00B6AA, $FF00B6FF, $FF24B600, $FF24B655, $FF24B6AA, $FF24B6FF,
      $FF48B600, $FF48B655, $FF48B6AA, $FF48B6FF, $FF6DB600, $FF6DB655, $FF6DB6AA, $FF6DB6FF,
      $FF91B600, $FF91B655, $FF91B6AA, $FF91B6FF, $FFB6B600, $FFB6B655, $FFB6B6AA, $FFB6B6FF,
      $FFDAB600, $FFDAB655, $FFDAB6AA, $FFDAB6FF, $FFFFB600, $FFFFB655, $FFFFB6AA, $FFFFB6FF,
      $FF00DA00, $FF00DA55, $FF00DAAA, $FF00DAFF, $FF24DA00, $FF24DA55, $FF24DAAA, $FF24DAFF,
      $FF48DA00, $FF48DA55, $FF48DAAA, $FF48DAFF, $FF6DDA00, $FF6DDA55, $FF6DDAAA, $FF6DDAFF,
      $FF91DA00, $FF91DA55, $FF91DAAA, $FF91DAFF, $FFB6DA00, $FFB6DA55, $FFB6DAAA, $FFB6DAFF,
      $FFDADA00, $FFDADA55, $FFDADAAA, $FFDADAFF, $FFFFDA00, $FFFFDA55, $FFFFDAAA, $FFFFDAFF,
      $FF00FF00, $FF00FF55, $FF00FFAA, $FF00FFFF, $FF24FF00, $FF24FF55, $FF24FFAA, $FF24FFFF,
      $FF48FF00, $FF48FF55, $FF48FFAA, $FF48FFFF, $FF6DFF00, $FF6DFF55, $FF6DFFAA, $FF6DFFFF,
      $FF91FF00, $FF91FF55, $FF91FFAA, $FF91FFFF, $FFB6FF00, $FFB6FF55, $FFB6FFAA, $FFB6FFFF,
      $FFDAFF00, $FFDAFF55, $FFDAFFAA, $FFDAFFFF, $FFFFFF00, $FFFFFF55, $FFFFFFAA, $FFFFFFFF
    );

  private
    FBitmap, FOBitmap: TBitmap;
    FMemory, FSaveMemory: TXPVideoMemory;
    FRegs: TXPVideoRegs;
    FPreCode, FPrePal, FRegIndex: Integer;
    FReadOffset, FWriteOffset, FLastTime, FScanLine, FStartScreen, FStartBorder: Integer;
    FNameAddress, FPatternAddress, FColorAddress, FPaletteAddress: Integer;
    FSpriteAttributeAddress, FSpritePatternAddress, FSpriteColorAddress: Integer;
    FNameShift, FNameMask, FNameStartBit: Integer;
    FPatternShift, FPatternMask, FPatternStartBit: Integer;
    FColorShift, FColorMask, FColorStartBit: Integer;
    FSpriteAttrShift, FSpriteAttrMask, FSpriteAttrStartBit: Integer;
    FSpritePatternShift, FSpritePatternMask, FSpritePatternStartBit: Integer;
    FBlinked, FCommandExecuted: Boolean;
    procedure AddMessage(AText: string);
    function GetStatus(AIndex: Integer): Byte;
    procedure SetStatus(AIndex: Integer; AValue: Byte);
    function GetPals(AIndex: Integer): TXPVideoPal;
    procedure UpdateStatus;
    function GetScreen: Integer;
    procedure SetScreen(const Value: Integer);
    function GetPage: Integer;
    procedure SetPage(AValue: Integer);
  public
    Messages: TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Reset;
    procedure ScreenMode(AMode: Integer);
    procedure ColorRestore;
    procedure DrawTo(ACanvas: TCanvas; ARect: TRect);
    function ReadPort(APort: Byte): Byte;
    procedure WritePort(APort, AValue: Byte);
    procedure Bload(AFileName: string; AOffset: Integer = 0);
    procedure SaveMemory;
    procedure ExecuteCommand;
    procedure CommandPoint;
    procedure CommandPset;
    procedure CommandSearch;
    procedure CommandLine;
    procedure CommandLMMV;
    procedure CommandLMMM;
    procedure CommandLMCM;
    procedure CommandLMMC;
    procedure CommandHMMV;
    procedure CommandHMMM;
    procedure CommandYMMM;
    procedure CommandHMMC;
    function UpdateState(ACycle: Integer): Boolean;
    procedure UpdateScanLine;
    procedure UpdateScreen;
    procedure DrawLine0(ALine: PBGRAArray; ALeft, ATop, AY: Integer);
    procedure DrawLine1(ALine: PBGRAArray; ALeft, ATop, AY: Integer);
    procedure DrawLine2(ALine: PBGRAArray; ALeft, ATop, AY: Integer);
    procedure DrawLine3(ALine: PBGRAArray; ALeft, ATop, AY: Integer);
    procedure DrawLine4(ALine: PBGRAArray; ALeft, ATop, AY: Integer);
    procedure DrawLine5(ALine: PBGRAArray; ALeft, ATop, AY: Integer);
    procedure DrawLine6(ALine: PBGRAArray; ALeft, ATop, AY: Integer);
    procedure DrawLine7(ALine: PBGRAArray; ALeft, ATop, AY: Integer);
    procedure DrawLine8(ALine: PBGRAArray; ALeft, ATop, AY: Integer);
    procedure DrawLine9(ALine: PBGRAArray; ALeft, ATop, AY: Integer);
    procedure LoadFromResource(AAddress: Integer; AName: string; AType: PChar); overload;
    procedure LoadFromResource(AAddress: Integer; AName, AType: string); overload;

    property Regs: TXPVideoRegs read FRegs write FRegs;
    property Status[AIndex: Integer]: Byte read GetStatus write SetStatus;
    property Pals[AIndex: Integer]: TXPVideoPal read GetPals;
    property Memory: TXPVideoMemory read FMemory write FMemory;
    property ReadIndex: Integer read FReadOffset write FReadOffset;
    property WriteIndex: Integer read FWriteOffset write FWriteOffset;
    property StartScreen: Integer read FStartScreen write FStartScreen;
    property StartBorder: Integer read FStartBorder write FStartBorder;
    property Screen: Integer read GetScreen write SetScreen;
    property Page: Integer read GetPage write SetPage;
    property LastTime: Integer read FLastTime;
    property Bitmap: TBitmap read FBitmap;
  end;

implementation

{ TXPVideo }

procedure TXPVideo.AddMessage(AText: string);
begin
  if Messages <> nil then Messages.AddMessage(AText);
end;

procedure TXPVideo.Bload(AFileName: string; AOffset: Integer);
var
  fs: TFileStream;
  n1, n2, n3: Word;
  c: Byte;
begin
  Initialize(c);
  Initialize(n1);
  Initialize(n2);
  Initialize(n3);
  fs := TFileStream.Create(AFileName, fmOpenRead);
  try
    fs.Read(c, 1);
    if c = $FE then
    begin
      fs.Read(n1, 2);
      fs.Read(n2, 2);
      fs.Read(n3, 2);
      fs.Read(FMemory[AOffset + n1], n2 - n1 + 1);
    end;
  finally
    fs.Free;
  end;
end;

procedure TXPVideo.Reset;
begin
  ScreenMode(FStartScreen);
  FRegs.BGColor := FStartBorder;
  FMemory := FSaveMemory;
  ColorRestore;
  FPreCode := -1;
  FPrePal := -1;
  FRegIndex := 0;
  FReadOffset := 0;
  FWriteOffset := 0;
  FLastTime := 0;
  FScanLine := 0;
  FNameAddress := 0;
  FPatternAddress := 0;
  FColorAddress := 0;
  FSpriteAttributeAddress := 0;
  FSpritePatternAddress := 0;
  FSpriteColorAddress := 0;
  FBlinked := False;
  UpdateScreen;
end;

procedure TXPVideo.Clear;
begin
  FRegs.Clear;
  FPreCode := -1;
  FPrePal := -1;
  FRegIndex := 0;
  FReadOffset := 0;
  FWriteOffset := 0;
  FLastTime := 0;
  FScanLine := 0;
  FNameAddress := 0;
  FPatternAddress := 0;
  FColorAddress := 0;
  FSpriteAttributeAddress := 0;
  FSpritePatternAddress := 0;
  FSpriteColorAddress := 0;
  FStartScreen := 6;
  FStartBorder := 4;
  FBlinked := False;
  FCommandExecuted := False;
  FillChar(FMemory[0], SizeOf(FMemory), 0);
  ScreenMode(FStartScreen);
  FSaveMemory := FMemory;
end;

procedure TXPVideo.ColorRestore;
var
  n: Integer;
begin
  for n := 0 to 15 do
  begin
    FRegs.Pals[n].Values[0] := FMemory[FPaletteAddress + (n shl 1) + 0];
    FRegs.Pals[n].Values[1] := FMemory[FPaletteAddress + (n shl 1) + 1];
  end;
end;

procedure TXPVideo.CommandHMMC;
begin

end;

procedure TXPVideo.CommandHMMM;
const
  XShift: array[5..8] of Byte = (1, 2, 1, 0);
  YShift: array[5..8] of Byte = (7, 7, 8, 8);
var
  pd, ps, nx, ny, px: Integer;
  bx, by: Byte;
begin
  bx := XShift[Screen];
  by := YShift[Screen];
  ps := (FRegs.SY shl by) + (FRegs.SX shr bx);
  pd := (FRegs.DY shl by) + (FRegs.DX shr bx);
  nx := FRegs.NX shr bx;
  ny := FRegs.NY;
  while ny > 0 do
  begin
    px := 0;
    while px < nx do
    begin
      FMemory[(pd + px) and $1FFFF] := FMemory[(ps + px) and $1FFFF];
      Inc(px);
    end;
    Inc(ps, 1 shl by);
    Inc(pd, 1 shl by);
    Dec(ny);
  end;
end;

procedure TXPVideo.CommandHMMV;
const
  XShift: array[5..8] of Byte = (1, 2, 1, 0);
  YShift: array[5..8] of Byte = (7, 7, 8, 8);
var
  pc, nx, ny, px: Integer;
  bx, by, c: Byte;
begin
  bx := XShift[Screen];
  by := YShift[Screen];
  pc := (FRegs.DY shl by) + (FRegs.DX shr bx);
  nx := FRegs.NX shr bx;
  ny := FRegs.NY;
  c := FRegs.Color;
  while ny > 0 do
  begin
    px := 0;
    while px < nx do
    begin
      FMemory[(pc + px) and $1FFFF] := c;
      Inc(px);
    end;
    Inc(pc, 1 shl by);
    Dec(ny);
  end;
end;

procedure TXPVideo.CommandLine;
begin

end;

procedure TXPVideo.CommandLMCM;
begin

end;

procedure TXPVideo.CommandLMMC;
begin

end;

procedure TXPVideo.CommandLMMM;
const
  XShifts: array[5..8] of Byte = (1, 2, 1, 0);    // div x factor
  YShifts: array[5..8] of Byte = (7, 7, 8, 8);    // inc y factor
  CMasks: array[5..8] of Byte = (15, 3, 15, 255); // max color id
  CShifts: array[5..8] of Byte = (2, 1, 2, 0);    // bit per color factor
  XMasks: array[5..8] of Byte = (1, 3, 1, 0);     // div x modulus
var
  pc, qc, nx, ny, px, py, qx, qy: Integer;
  bx, by, bn, bo, cn, co, cc, cmask, xmask, cshift, logic: Byte;
  alpha: Boolean;
begin
  bx := XShifts[Screen];
  by := YShifts[Screen];
  cmask := CMasks[Screen];
  cshift := CShifts[Screen];
  xmask := XMasks[Screen];
  logic := FRegs.Command and 7;
  alpha := (FRegs.Command and 8) <> 0;
  py := FRegs.SY;
  qy := FRegs.DY;
  ny := FRegs.NY;
  while ny > 0 do
  begin
    px := FRegs.SX;
    qx := FRegs.DX;
    nx := FRegs.NX;
    while nx > 0 do
    begin
      pc := (py shl by) + (px shr bx);
      qc := (qy shl by) + (qx shr bx);
      bn := ((xmask - (px and xmask)) shl cshift);
      bo := ((xmask - (qx and xmask)) shl cshift);
      cn := (FMemory[pc and $1FFFF] shr bn) and cmask;
      cc := FMemory[qc and $1FFFF];
      co := cc and (255 - (cmask shl bo));
      cc := (cc shr bo) and cmask;
      case logic of
        LCMD_AND: cc := cc and cn;
        LCMD_OR: cc := cc or cn;
        LCMD_EOR: cc := cc xor cn;
        LCMD_NOT: cc := (not cn) and cmask;
      else
        cc := cn;
      end;
      if not alpha or (alpha and (cn <> 0)) then
      begin
        FMemory[qc and $1FFFF] := co or ((cc and cmask) shl bo);
      end;
      Inc(px);
      Inc(qx);
      Dec(nx);
    end;
    Inc(py);
    Inc(qy);
    Dec(ny);
  end;
end;

procedure TXPVideo.CommandLMMV;
const
  XShifts: array[5..8] of Byte = (1, 2, 1, 0);    // div x factor
  YShifts: array[5..8] of Byte = (7, 7, 8, 8);    // inc y factor
  CMasks: array[5..8] of Byte = (15, 3, 15, 255); // max color id
  CShifts: array[5..8] of Byte = (2, 1, 2, 0);    // bit per color factor
  XMasks: array[5..8] of Byte = (1, 3, 1, 0);     // div x modulus
var
  pc, nx, ny, px, py: Integer;
  bx, by, bn, co, cc, cn, cmask, xmask, cshift, logic: Byte;
  alpha: Boolean;
begin
  bx := XShifts[Screen];
  by := YShifts[Screen];
  cmask := CMasks[Screen];
  cshift := CShifts[Screen];
  xmask := XMasks[Screen];
  logic := FRegs.Command and 7;
  alpha := (FRegs.Command and 8) <> 0;
  cn := FRegs.Color and cmask;
  py := FRegs.DY;
  ny := FRegs.NY;
  while ny > 0 do
  begin
    px := FRegs.DX;
    nx := FRegs.NX;
    while nx > 0 do
    begin
      pc := (py shl by) + (px shr bx);
      bn := ((xmask - (px and xmask)) shl cshift);
      cc := FMemory[pc and $1FFFF];
      co := cc and (255 - (cmask shl bn));
      cc := (cc shr bn) and cmask;
      case logic of
        LCMD_AND: cc := cc and cn;
        LCMD_OR: cc := cc or cn;
        LCMD_EOR: cc := cc xor cn;
        LCMD_NOT: cc := (not cn) and cmask;
      else
        cc := cn;
      end;
      if not alpha or (alpha and (cn <> 0)) then
      begin
        FMemory[pc and $1FFFF] := co or ((cc and cmask) shl bn);
      end;
      Inc(px);
      Dec(nx);
    end;
    Inc(py);
    Dec(ny);
  end;
end;

procedure TXPVideo.CommandPoint;
begin

end;

procedure TXPVideo.CommandPset;
begin

end;

procedure TXPVideo.CommandSearch;
begin

end;

procedure TXPVideo.CommandYMMM;
begin

end;

constructor TXPVideo.Create;
begin
  FBitmap := TBitmap.Create;
  FBitmap.PixelFormat := pf32bit;
  FBitmap.SetSize(BorderWidth * 2 + 512, BorderHeight * 2 + 424);
  FOBitmap := TBitmap.Create;
  FOBitmap.PixelFormat := pf32bit;
  FRegs := TXPVideoRegs.Create(Self);
  Messages := nil;
  Clear;
end;

destructor TXPVideo.Destroy;
begin
  FreeAndNil(FRegs);
  FreeAndNil(FOBitmap);
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

procedure TXPVideo.DrawLine0(ALine: PBGRAArray; ALeft, ATop, AY: Integer);
var
  n, b, px, pname, ppat, py: Integer;
  ch: Byte;
  ct, cb: TBGRAEntry;
begin
  py := (AY + (FRegs.Scroll shl 1)) and 511;
  cb := FRegs.Pals[FRegs.BGColor].BGRA;
  for n := 0 to 15 do ALine[ALeft + n] := cb;
  for n := 0 to 15 do ALine[ALeft + n + 496] := cb;
  pname := ((FRegs.NameTable and $7F) shl 10) + (py shr 4) * 40;
  ppat := ((FRegs.PatternTable and $3F) shl 11) + ((py shr 1) and 7);
  ct := FRegs.Pals[FRegs.TextColor].BGRA;
  for n := 0 to 39 do
  begin
    ch := FMemory[pname + n];
    ch := FMemory[ppat + (ch shl 3)];
    for b := 0 to 5 do
    begin
      px := ALeft + 16 + n * 12 + (b shl 1);
      if (ch and $80) <> 0 then
      begin
        ALine[px] := ct;
        ALine[px + 1] := ct;
      end
      else
      begin
        ALine[px] := cb;
        ALine[px + 1] := cb;
      end;
      ch := ch shl 1;
    end;
  end;
end;

procedure TXPVideo.DrawLine1(ALine: PBGRAArray; ALeft, ATop, AY: Integer);
var
  n, b, px, py, pname, ppat, pcol: Integer;
  ch, cp: Byte;
  ct, cb: TBGRAEntry;
begin
  py := (AY + (FRegs.Scroll shl 1)) and 511;
  pname := ((FRegs.NameTable and $7F) shl 10) + (py shr 4) * 32;
  ppat := ((FRegs.PatternTable and $3F) shl 11) + ((py shr 1) and 7);
  pcol := (FRegs.ColorTable and $7FF) shl 6;
  for n := 0 to 31 do
  begin
    ch := FMemory[pname + n];
    cp := FMemory[ppat + (ch shl 3)];
    ch := FMemory[pcol + (ch shr 3)];
    ct := FRegs.Pals[ch shr 4].BGRA;
    cb := FRegs.Pals[ch and 15].BGRA;
    for b := 0 to 7 do
    begin
      px := ALeft + (n shl 4) + (b shl 1);
      if (cp and $80) <> 0 then
      begin
        ALine[px] := ct;
        ALine[px + 1] := ct;
      end
      else
      begin
        ALine[px] := cb;
        ALine[px + 1] := cb;
      end;
      cp := cp shl 1;
    end;
  end;
end;

procedure TXPVideo.DrawLine2(ALine: PBGRAArray; ALeft, ATop, AY: Integer);
var
  n, b, px, py, pname, ppat, pcol: Integer;
  ch, cp: Byte;
  ct, cb: TBGRAEntry;
begin
  py := (AY + (FRegs.Scroll shl 1)) and 511;
  pname := ((FRegs.NameTable and $7F) shl 10) + ((py shr 4) shl 5);
  ppat := ((FRegs.PatternTable and $3C) shl 11) + ((py shr 7) shl 11) + ((py shr 1) and 7);
  pcol := ((FRegs.ColorTable and $380) shl 6) + ((py shr 7) shl 11) + ((py shr 1) and 7);
  for n := 0 to 31 do
  begin
    ch := FMemory[pname + n];
    cp := FMemory[ppat + (ch shl 3)];
    ch := FMemory[pcol + (ch shl 3)];
    ct := FRegs.Pals[ch shr 4].BGRA;
    cb := FRegs.Pals[ch and 15].BGRA;
    for b := 0 to 7 do
    begin
      px := ALeft + (n shl 4) + (b shl 1);
      if (cp and $80) <> 0 then
      begin
        ALine[px] := ct;
        ALine[px + 1] := ct;
      end
      else
      begin
        ALine[px] := cb;
        ALine[px + 1] := cb;
      end;
      cp := cp shl 1;
    end;
  end;
end;

procedure TXPVideo.DrawLine3(ALine: PBGRAArray; ALeft, ATop, AY: Integer);
var
  n, b, px, py, pname, ppat: Integer;
  ch: Byte;
  cb: TBGRAEntry;
begin
  py := (AY + (FRegs.Scroll shl 1)) and 511;
  pname := ((FRegs.NameTable and $7F) shl 10) + ((py shr 4) shl 5);
  ppat := ((FRegs.PatternTable and $3F) shl 11) + ((py shr 3) and 7);
  for n := 0 to 63 do
  begin
    ch := FMemory[pname + (n shr 1)];
    px := ALeft + (n shl 3);
    ch := FMemory[ppat + (ch shl 3)];
    if (n and 1) = 0 then
      cb := FRegs.Pals[ch shr 4].BGRA
    else cb := FRegs.Pals[ch and 15].BGRA;
    for b := 0 to 7 do ALine[px + b] := cb;
  end;
end;

procedure TXPVideo.DrawLine4(ALine: PBGRAArray; ALeft, ATop, AY: Integer);
var
  n, b, px, py, pname, ppat, pcol: Integer;
  ch, cp: Byte;
  ct, cb: TBGRAEntry;
begin
  py := (AY + (FRegs.Scroll shl 1)) and 511;
  pname := ((FRegs.NameTable and $7F) shl 10) + ((py shr 4) shl 5);
  ppat := ((FRegs.PatternTable and $3C) shl 11) + ((py shr 7) shl 11) + ((py shr 1) and 7);
  pcol := ((FRegs.ColorTable and $380) shl 6) + ((py shr 7) shl 11) + ((py shr 1) and 7);
  for n := 0 to 31 do
  begin
    ch := FMemory[pname + n];
    cp := FMemory[ppat + (ch shl 3)];
    ch := FMemory[pcol + (ch shl 3)];
    ct := FRegs.Pals[ch shr 4].BGRA;
    cb := FRegs.Pals[ch and 15].BGRA;
    for b := 0 to 7 do
    begin
      px := ALeft + (n shl 4) + (b shl 1);
      if (cp and $80) <> 0 then
      begin
        ALine[px] := ct;
        ALine[px + 1] := ct;
      end
      else
      begin
        ALine[px] := cb;
        ALine[px + 1] := cb;
      end;
      cp := cp shl 1;
    end;
  end;
end;

procedure TXPVideo.DrawLine5(ALine: PBGRAArray; ALeft, ATop, AY: Integer);
var
  n, b, px, py, pname: Integer;
  cb: Byte;
begin
  py := (AY + (FRegs.Scroll shl 1)) and 511;
  pname := ((FRegs.NameTable and $60) shl 10) + ((py shr 1) shl 7);
  for n := 0 to 255 do
  begin
    cb := FMemory[pname + (n shr 1)];
    px := ALeft + (n shl 1);
    b := (1 - (n and 1)) shl 2;
    ALine[px] := FRegs.Pals[(cb shr b) and 15].BGRA;
    ALine[px + 1] := FRegs.Pals[(cb shr b) and 15].BGRA;
  end;
end;

procedure TXPVideo.DrawLine6(ALine: PBGRAArray; ALeft, ATop, AY: Integer);
var
  n, b, px, py, pname: Integer;
  cb: Byte;
begin
  py := (AY + (FRegs.Scroll shl 1)) and 511;
  pname := ((FRegs.NameTable and $60) shl 10) + ((py shr 1) shl 7);
  for n := 0 to 511 do
  begin
    cb := FMemory[pname + (n shr 2)];
    px := ALeft + n;
    b := (3 - (n and 3)) shl 1;
    ALine[px] := FRegs.Pals[(cb shr b) and 3].BGRA;
  end;
end;

procedure TXPVideo.DrawLine7(ALine: PBGRAArray; ALeft, ATop, AY: Integer);
var
  n, b, px, py, pname: Integer;
  cb: Byte;
begin
  py := (AY + (FRegs.Scroll shl 1)) and 511;
  pname := ((FRegs.NameTable and $20) shl 11) + ((py shr 1) shl 8);
  for n := 0 to 511 do
  begin
    cb := FMemory[pname + (n shr 1)];
    px := ALeft + n;
    b := (1 - (n and 1)) shl 2;
    ALine[px] := FRegs.Pals[(cb shr b) and 15].BGRA;
  end;
end;

procedure TXPVideo.DrawLine8(ALine: PBGRAArray; ALeft, ATop, AY: Integer);
var
  n, px, py, pname: Integer;
  cb: Byte;
begin
  py := (AY + (FRegs.Scroll shl 1)) and 511;
  pname := ((FRegs.NameTable and $20) shl 11) + ((py shr 1) shl 8);
  for n := 0 to 255 do
  begin
    cb := FMemory[pname + n];
    px := ALeft + (n shl 1);
    ALine[px].BGRA := Screen8BGRA[cb];
    ALine[px + 1].BGRA := Screen8BGRA[cb];
  end;
end;

procedure TXPVideo.DrawLine9(ALine: PBGRAArray; ALeft, ATop, AY: Integer);
var
  n, b, px, py, pname, ppat, pcol: Integer;
  cb: TBGRAEntry;
  ch, cc: Byte;
  bk: Boolean;
begin
  py := (AY + (FRegs.Scroll shl 1)) and 511;
  cb := FRegs.Pals[FRegs.BGColor].BGRA;
  for n := 0 to 15 do
  begin
    ALine[ALeft + n] := cb;
    ALine[ALeft + n + 496] := cb;
  end;

  pname := ((FRegs.NameTable and $7C) shl 10) + (py shr 4) * 80;
  pcol := ((FRegs.ColorTable and $7F8) shl 6) + (py shr 4) * 10;
  for n := 0 to 79 do
  begin
    ch := FMemory[pname + n];
    bk := FBlinked and (((FMemory[pcol + (n shr 3)] shr (7 - (n and 7))) and 1) <> 0);
    ppat := (FRegs.PatternTable shl 11) + (ch shl 3) + ((AY shr 1) and 7);
    ch := FMemory[ppat];
    for b := 0 to 5 do
    begin
      px := ALeft + 16 + n * 6 + b;
      if (ch and $80) <> 0 then
        if bk then
          cc := FRegs.BlinkColor shr 4
        else cc := FRegs.TextColor
      else if bk then
        cc := FRegs.BlinkColor and 15
      else cc := FRegs.BGColor;
      ALine[px] := FRegs.Pals[cc].BGRA;
      ch := ch shl 1;
    end;
  end;
end;

procedure TXPVideo.DrawTo(ACanvas: TCanvas; ARect: TRect);
begin
  ResizeBitmap2(FBitmap, FOBitmap, ARect.Width, ARect.Height);
  ACanvas.Draw(ARect.Left, ARect.Top, FOBitmap);
end;

procedure TXPVideo.ExecuteCommand;
begin
  case FRegs[46] and $F0 of
    CMDSTOP: FCommandExecuted := False;
    CMDPOINT: CommandPoint;
    CMDPSET: CommandPset;
    CMDSEARCH: CommandSearch;
    CMDLINE: CommandLine;
    CMDLMMV: CommandLMMV;
    CMDLMMM: CommandLMMM;
    CMDLMCM: CommandLMCM;
    CMDLMMC: CommandLMMC;
    CMDHMMV: CommandHMMV;
    CMDHMMM: CommandHMMM;
    CMDYMMM: CommandYMMM;
    CMDHMMC: CommandHMMC;
  end;
end;

function TXPVideo.GetPage: Integer;
begin
  case FRegs.Mode of
    MODE_SCREEN0: Result := FRegs.NameTable and $7F;
    MODE_SCREEN1: Result := FRegs.NameTable and $7F;
    MODE_SCREEN2: Result := FRegs.NameTable and $7F;
    MODE_SCREEN3: Result := FRegs.NameTable and $7F;
    MODE_SCREEN4: Result := FRegs.NameTable and $7F;
    MODE_SCREEN5: Result := (FRegs.NameTable shr 5) and 3;
    MODE_SCREEN6: Result := (FRegs.NameTable shr 5) and 3;
    MODE_SCREEN7: Result := (FRegs.NameTable shr 5) and 1;
    MODE_SCREEN8: Result := (FRegs.NameTable shr 5) and 1;
    MODE_SCREEN9: Result := (FRegs.NameTable shr 2) and $1F;
  else
    Result := -1;
  end;
end;

function TXPVideo.GetPals(AIndex: Integer): TXPVideoPal;
begin
  Result := FRegs.Pals[AIndex];
end;

function TXPVideo.GetScreen: Integer;
begin
  case FRegs.Mode of
    MODE_SCREEN0: Result := 0;
    MODE_SCREEN1: Result := 1;
    MODE_SCREEN2: Result := 2;
    MODE_SCREEN3: Result := 3;
    MODE_SCREEN4: Result := 4;
    MODE_SCREEN5: Result := 5;
    MODE_SCREEN6: Result := 6;
    MODE_SCREEN7: Result := 7;
    MODE_SCREEN8: Result := 8;
    MODE_SCREEN9: Result := 9;
  else
    Result := -1;
  end;
end;

function TXPVideo.GetStatus(AIndex: Integer): Byte;
begin
  Result := FRegs.Status[AIndex];
end;

procedure TXPVideo.LoadFromResource(AAddress: Integer; AName: string;
  AType: PChar);
var
  rs: TResourceStream;
begin
  rs := TResourceStream.Create(hInstance, AName, AType);
  try
    rs.Read(FMemory[AAddress], rs.Size);
  finally
    rs.Free;
  end;
end;

procedure TXPVideo.LoadFromResource(AAddress: Integer; AName, AType: string);
begin
  LoadFromResource(AAddress, AName, PChar(AType));
end;

function TXPVideo.ReadPort(APort: Byte): Byte;
begin
  case APort of
    $98:
    begin
      Result := FMemory[FReadOffset];
      FReadOffset := (FReadOffset + 1) and $1FFFF;
    end;
    $99:
    begin
      UpdateStatus;
      Result := Status[FRegs.StatusIndex];
    end;
    $9A:
    begin
      Result := 0;
    end;
    $9B:
    begin
      Result := 0;
    end;
  else
    Result := 0;
  end;
end;

procedure TXPVideo.SaveMemory;
begin
  FSaveMemory := FMemory;
end;

procedure TXPVideo.ScreenMode(AMode: Integer);
var
  p: PXPSpriteAttribute;
  n: Integer;
  s: string;
begin
  case AMode of
    0:
    begin
      FNameAddress := 0;
      FPatternAddress := $800;
      FColorAddress := -1;
      FSpriteAttributeAddress := -1;
      FSpritePatternAddress := -1;
      FSpriteColorAddress := -1;
      FPaletteAddress := $400;

      FNameShift := 0;
      FNameMask := $7F;
      FNameStartBit := 10;

      FPatternShift := 0;
      FPatternMask := $3F;
      FPatternStartBit := 11;

      FColorShift := 0;
      FColorMask := $FFFF;
      FColorStartBit := 0;

      FSpriteAttrShift := 0;
      FSpriteAttrMask := $FFFF;
      FSpriteAttrStartBit := 0;

      FSpritePatternShift := 0;
      FSpritePatternMask := $FF;
      FSpritePatternStartBit := 101;
    end;
    1:
    begin
      FNameAddress := $1800;
      FPatternAddress := 0;
      FColorAddress := $2000;
      FSpriteAttributeAddress := $1B00;
      FSpritePatternAddress := $3800;
      FSpriteColorAddress := -1;
      FPaletteAddress := $2020;

      FNameShift := 0;
      FNameMask := $7F;
      FNameStartBit := 10;

      FPatternShift := 0;
      FPatternMask := $3F;
      FPatternStartBit := 11;

      FColorShift := 0;
      FColorMask := $07FF;
      FColorStartBit := 6;

      FSpriteAttrShift := 0;
      FSpriteAttrMask := $03FF;
      FSpriteAttrStartBit := 7;

      FSpritePatternShift := 0;
      FSpritePatternMask := $3F;
      FSpritePatternStartBit := 11;
    end;
    2:
    begin
      FNameAddress := $1800;
      FPatternAddress := 0;
      FColorAddress := $2000;
      FSpriteAttributeAddress := $1B00;
      FSpritePatternAddress := $3800;
      FSpriteColorAddress := -1;
      FPaletteAddress := $1B80;

      FNameShift := 0;
      FNameMask := $7F;
      FNameStartBit := 10;

      FPatternShift := 2;
      FPatternMask := $0F;
      FPatternStartBit := 13;

      FColorShift := 7;
      FColorMask := $000F;
      FColorStartBit := 13;

      FSpriteAttrShift := 0;
      FSpriteAttrMask := $03FF;
      FSpriteAttrStartBit := 7;

      FSpritePatternShift := 0;
      FSpritePatternMask := $3F;
      FSpritePatternStartBit := 11;
    end;
    3:
    begin
      FNameAddress := $800;
      FPatternAddress := 0;
      FColorAddress := -1;
      FSpriteAttributeAddress := $1B00;
      FSpritePatternAddress := $3800;
      FSpriteColorAddress := -1;
      FPaletteAddress := $2020;

      FNameShift := 0;
      FNameMask := $7F;
      FNameStartBit := 10;

      FPatternShift := 0;
      FPatternMask := $3F;
      FPatternStartBit := 11;

      FColorShift := 0;
      FColorMask := $FFFF;
      FColorStartBit := 0;

      FSpriteAttrShift := 0;
      FSpriteAttrMask := $03FF;
      FSpriteAttrStartBit := 7;

      FSpritePatternShift := 0;
      FSpritePatternMask := $3F;
      FSpritePatternStartBit := 11;
    end;
    4:
    begin
      FNameAddress := $1800;
      FPatternAddress := 0;
      FColorAddress := $2000;
      FSpriteAttributeAddress := $1E00;
      FSpritePatternAddress := $3800;
      FSpriteColorAddress := $1C00;
      FPaletteAddress := $1B80;

      FNameShift := 0;
      FNameMask := $7F;
      FNameStartBit := 10;

      FPatternShift := 2;
      FPatternMask := $0F;
      FPatternStartBit := 13;

      FColorShift := 7;
      FColorMask := $000F;
      FColorStartBit := 13;

      FSpriteAttrShift := 0;
      FSpriteAttrMask := $03FF;
      FSpriteAttrStartBit := 7;

      FSpritePatternShift := 0;
      FSpritePatternMask := $3F;
      FSpritePatternStartBit := 11;
    end;
    5:
    begin
      FNameAddress := 0;
      FPatternAddress := -1;
      FColorAddress := -1;
      FSpriteAttributeAddress := $7600;
      FSpritePatternAddress := $7800;
      FSpriteColorAddress := $7400;
      FPaletteAddress := $7680;

      FNameShift := 5;
      FNameMask := $3F;
      FNameStartBit := 15;

      FPatternShift := 0;
      FPatternMask := $FF;
      FPatternStartBit := 0;

      FColorShift := 0;
      FColorMask := $FFFF;
      FColorStartBit := 0;

      FSpriteAttrShift := 3;
      FSpriteAttrMask := $007F;
      FSpriteAttrStartBit := 10;

      FSpritePatternShift := 0;
      FSpritePatternMask := $3F;
      FSpritePatternStartBit := 11;
    end;
    6:
    begin
      FNameAddress := 0;
      FPatternAddress := -1;
      FColorAddress := -1;
      FSpriteAttributeAddress := $7600;
      FSpritePatternAddress := $7800;
      FSpriteColorAddress := $7400;
      FPaletteAddress := $7680;
    end;
    7:
    begin
      FNameAddress := 0;
      FPatternAddress := -1;
      FColorAddress := -1;
      FSpriteAttributeAddress := $FA00;
      FSpritePatternAddress := $F000;
      FSpriteColorAddress := $F800;
      FPaletteAddress := $FA80;
    end;
    8:
    begin
      FNameAddress := 0;
      FPatternAddress := -1;
      FColorAddress := -1;
      FSpriteAttributeAddress := $FA00;
      FSpritePatternAddress := $F000;
      FSpriteColorAddress := -1;
      FPaletteAddress := $FA80;
    end;
    9: // SCREEN 0, WIDTH 80
    begin
      FNameAddress := 0;
      FPatternAddress := $1000;
      FColorAddress := $800;
      FSpriteAttributeAddress := -1;
      FSpritePatternAddress := -1;
      FSpriteColorAddress := -1;
      FPaletteAddress := $F00;
    end;
  end;
  FRegs.Clear;
  for n := 0 to 23 do FRegs[n] := ScreenVDP[AMode, n];
  for n := 0 to 9 do FRegs.Status[n] := ScreenStatus[AMode, n];
  for n := 0 to 15 do FRegs.Pals[n].GRB := DefaultPals[n];
  s := 'SC' + IntToStr(AMode) + '-';
  if FPaletteAddress >= 0 then LoadFromResource(FPaletteAddress, 'SCR-PALETTE', RT_RCDATA);
  if FNameAddress >= 0 then
  begin
    if AMode = 6 then
    begin
      LoadFromResource(FNameAddress, s + 'NAME-0', RT_RCDATA);
      LoadFromResource(FNameAddress + $8000, s + 'NAME-1', RT_RCDATA);
      LoadFromResource(FNameAddress + $10000, s + 'NAME-2', RT_RCDATA);
      LoadFromResource(FNameAddress + $18000, s + 'NAME-3', RT_RCDATA);
    end
    else LoadFromResource(FNameAddress, s + 'NAME', RT_RCDATA);
  end;
  if FPatternAddress >= 0 then LoadFromResource(FPatternAddress, s + 'PATTERN', RT_RCDATA);
  if FColorAddress >= 0 then LoadFromResource(FColorAddress, s + 'COLOR', RT_RCDATA);
  if FSpriteAttributeAddress >= 0 then
  begin
    for n := 0 to 31 do
    begin
      p := @FMemory[FSpriteAttributeAddress + (n shl 2)];
      p.Y := 216;
      p.X := 0;
      p.N := 0;
      p.C := 0;
    end;
  end;
  if FSpritePatternAddress >= 0 then
  begin
    for n := 0 to 256 * 8 - 1 do FMemory[FSpritePatternAddress + n] := 0;
  end;
  if FSpriteColorAddress >= 0 then
  begin
    for n := 0 to 32 * 16 - 1 do FMemory[FSpriteColorAddress + n] := 0;
  end;
  ColorRestore;
  UpdateScreen;
end;

procedure TXPVideo.SetPage(AValue: Integer);
begin
  case FRegs.Mode of
    MODE_SCREEN0: FRegs.NameTable := AValue and $7F;
    MODE_SCREEN1: FRegs.NameTable := AValue and $7F;
    MODE_SCREEN2: FRegs.NameTable := AValue and $7F;
    MODE_SCREEN3: FRegs.NameTable := AValue and $7F;
    MODE_SCREEN4: FRegs.NameTable := AValue and $7F;
    MODE_SCREEN5: FRegs.NameTable := ((AValue and 3) shl 5) or $1F;
    MODE_SCREEN6: FRegs.NameTable := ((AValue and 3) shl 5) or $1F;
    MODE_SCREEN7: FRegs.NameTable := ((AValue and 1) shl 5) or $1F;
    MODE_SCREEN8: FRegs.NameTable := ((AValue and 1) shl 5) or $1F;
    MODE_SCREEN9: FRegs.NameTable := ((AValue and $1F) shl 2) or 3;
  else
    FRegs.NameTable := AValue;
  end;
end;

procedure TXPVideo.SetScreen(const Value: Integer);
begin
  case Value of
    0: FRegs.Mode := MODE_SCREEN0;
    1: FRegs.Mode := MODE_SCREEN1;
    2: FRegs.Mode := MODE_SCREEN2;
    3: FRegs.Mode := MODE_SCREEN3;
    4: FRegs.Mode := MODE_SCREEN4;
    5: FRegs.Mode := MODE_SCREEN5;
    6: FRegs.Mode := MODE_SCREEN6;
    7: FRegs.Mode := MODE_SCREEN7;
    8: FRegs.Mode := MODE_SCREEN8;
    9: FRegs.Mode := MODE_SCREEN9;
  end;
end;

procedure TXPVideo.SetStatus(AIndex: Integer; AValue: Byte);
begin
  FRegs.Status[AIndex] := AValue;
end;

procedure TXPVideo.UpdateScanLine;
var
  nx, ny, ntop, nbottom, nleft: Integer;
  pc: PBGRAArray;
  cb: TBGRAEntry;
begin
  if FRegs.Line212 = 0 then
  begin
    ntop := BorderHeight + 20;
    nbottom := ntop + 384;
  end
  else
  begin
    ntop := BorderHeight;
    nbottom := ntop + 424;
  end;
  Dec(ntop, FRegs.AdjustY shl 1);
  nleft := BorderWidth - (FRegs.AdjustX shl 1);

  FBitmap.BeginUpdate;
  try
    pc := PBGRAArray(FBitmap.RawImage.GetLineStart(FScanLine));
    cb := FRegs.Pals[FRegs.BGColor].BGRA;
    if (FScanLine < ntop) or (FScanLine >= nbottom) then
    begin
      for nx := 0 to 511 + BorderWidth * 2 do pc[nx] := cb;
      FRegs.VBlank := True;
    end
    else
    begin
      for nx := 0 to nleft - 1 do pc[nx] := cb;
      for nx := 0 to BorderWidth + BorderWidth - nleft - 1 do pc[nx + nleft + 512] := cb;
      ny := FScanLine - ntop;
      case FRegs.Mode of
        MODE_SCREEN0: DrawLine0(pc, nleft, ntop, ny);
        MODE_SCREEN1: DrawLine1(pc, nleft, ntop, ny);
        MODE_SCREEN2: DrawLine2(pc, nleft, ntop, ny);
        MODE_SCREEN3: DrawLine3(pc, nleft, ntop, ny);
        MODE_SCREEN4: DrawLine4(pc, nleft, ntop, ny);
        MODE_SCREEN5: DrawLine5(pc, nleft, ntop, ny);
        MODE_SCREEN6: DrawLine6(pc, nleft, ntop, ny);
        MODE_SCREEN7: DrawLine7(pc, nleft, ntop, ny);
        MODE_SCREEN8: DrawLine8(pc, nleft, ntop, ny);
        MODE_SCREEN9: DrawLine9(pc, nleft, ntop, ny);
      else
        for nx := 0 to 511 do pc[nleft + nx] := cb;
      end;
      FRegs.VBlank := False;
    end;
  finally
    FBitmap.EndUpdate;
  end;
end;

procedure TXPVideo.UpdateScreen;
var
  y, maxy: Integer;
begin
  maxy := BorderHeight * 2 + 424 - 1;
  for y := 0 to maxy do
  begin
    FScanLine := y;
    UpdateScanLine;
  end;
end;

function TXPVideo.UpdateState(ACycle: Integer): Boolean;
begin
  Result := False;
  while ACycle - FLastTime > CyclePerScanLine do
  begin
    FLastTime := FLastTime + CyclePerScanLine;
    UpdateScanLine;
    Inc(FScanLine);
    if FScanLine >= BorderHeight * 2 + 424 then
    begin
      FScanLine := 0;
      Result := True;
    end;
  end;
end;

procedure TXPVideo.UpdateStatus;
begin
end;

procedure TXPVideo.WritePort(APort, AValue: Byte);
var
  pal: TXPVideoPal;
  b: Byte;
begin
  case APort of
    $98:
    begin
      FMemory[FWriteOffset] := AValue;
      AddMessage(Format('VRAM:(%5.5Xh) <- %2.2Xh', [FWriteOffset, AValue]));
      FWriteOffset := (FWriteOffset + 1) and $1FFFF;
    end;
    $99:
    begin
      if FPreCode < 0 then
        FPreCode := AValue
      else
      begin
        b := AValue and $3F;
        case AValue shr 6 of
          0: // VRAM Read
          begin
            FReadOffset := ((FRegs.MemoryAddressHigh and 7) shl 14)
              or ((AValue and $3F) shl 8) or FPreCode;
            AddMessage(Format('VRAM read offset <- %5.5Xh', [FReadOffset]));
          end;
          1: // VRAM Write
          begin
            FWriteOffset := ((FRegs.MemoryAddressHigh and 7) shl 14)
              or ((AValue and $3F) shl 8) or FPreCode;
            AddMessage(Format('VRAM write offset #%d <- %5.5Xh', [b, FWriteOffset]));
          end;
          2: // Write register
          begin
            FRegs[b] := FPreCode;
            AddMessage(Format('VDP Reg #%d <- %2.2Xh', [b, FPreCode]));
          end;
          3: AddMessage('Bad VDP mode 3');
        end;
        FPreCode := -1;
      end;
    end;
    $9A:
    begin
      if FPrePal < 0 then
        FPrePal := AValue
      else
      begin
        pal := Pals[FRegs.PaletteIndex and 15];
        pal.Values[0] := FPrePal;
        pal.Values[1] := AValue;
        AddMessage(Format('PAL #%2.2d <- %1.1X %1.1X %1.1X', [FRegs.PaletteIndex, pal.R, pal.G, pal.B]));
        FRegs.PaletteIndex := (FRegs.PaletteIndex + 1) and 15;
        FPrePal := -1;
      end;
    end;
    $9B:
    begin
      b := FRegs.RegisterIndex and $3F;
      FRegs[b] := AValue;
      if (FRegs.RegisterIndex and $C0) = 0 then FRegs.RegisterIndex := (b + 1) and $3F;
      AddMessage(Format('VDP Reg #%d <- %2.2Xh', [b, AValue]));
    end;
  end;
end;

{ TXPVideoRegs }

procedure TXPVideoRegs.Assign(ARegs: TXPVideoRegs);
var
  n: Integer;
begin
  for n := 0 to 15 do FPals[n].Assign(ARegs.FPals[n]);
  FReg := ARegs.FReg;
  FAffected.Clear;
end;

procedure TXPVideoRegs.Clear;
var
  n: Integer;
begin
  FAffected.Clear;
  FillChar(FReg, SizeOf(FReg), 0);
  for n := 0 to 15 do FPals[n].Clear;
end;

constructor TXPVideoRegs.Create(AVideo: TXPVideo);
var
  n: Integer;
begin
  FVideo := AVideo;
  FAffected := TXPAffected.Create;
  for n := 0 to 15 do FPals[n] := TXPVideoPal.Create(n, FAffected);
end;

destructor TXPVideoRegs.Destroy;
var
  n: Integer;
begin
  for n := 0 to 15 do FreeAndNil(FPals[n]);
  FreeAndNil(FAffected);
  inherited Destroy;
end;

function TXPVideoRegs.GetAdjustX: Integer;
begin
  Result := Adjust and 15;
  if Result > 7 then Result := Result - 16;
end;

function TXPVideoRegs.GetAdjustY: Integer;
begin
  Result := Adjust shr 4;
  if Result > 7 then Result := Result - 16;
end;

function TXPVideoRegs.GetColorTable: Word;
begin
  Result := ColorTableLow or (ColorTableHigh shl 8);
end;

function TXPVideoRegs.GetMode: Integer;
begin
  Result := (Mode1 and $18) or ((Mode0 shr 1) and 7);
end;

function TXPVideoRegs.GetPals(AIndex: Integer): TXPVideoPal;
begin
  Result := FPals[AIndex];
end;

function TXPVideoRegs.GetReg16(AIndex: Integer): Word;
begin
  Result := FReg.Regs[AIndex] or (FReg.Regs[AIndex + 1] shl 8);
end;

function TXPVideoRegs.GetReg8(AIndex: Integer): Byte;
begin
  Result := (FReg.Regs[AIndex and 255] shr (AIndex shr 16)) and (255 - (AIndex shr 8)) and 255;
end;

function TXPVideoRegs.GetStatus(AIndex: Integer): Byte;
begin
  Result := (FReg.Status[AIndex and 15] shr (AIndex shr 12)) and (255 - (AIndex shr 4)) and 255;
end;

function TXPVideoRegs.GetStatusBool(AIndex: Integer): Boolean;
begin
  Result := ((FReg.Status[AIndex and 15] shr (AIndex shr 4)) and 1) <> 0;
end;

procedure TXPVideoRegs.SetAdjustX(AValue: Integer);
begin
  if AValue < 0 then AValue := AValue + 16;
  Adjust := (Adjust and $F0) or (AValue and 15);
end;

procedure TXPVideoRegs.SetAdjustY(AValue: Integer);
begin
  if AValue < 0 then AValue := AValue + 16;
  Adjust := (Adjust and 15) or ((AValue and 15) shl 4);
end;

procedure TXPVideoRegs.SetColorTable(AValue: Word);
begin
  ColorTableLow := AValue and 255;
  ColorTableHigh := AValue shr 8;
end;

procedure TXPVideoRegs.SetMode(AValue: Integer);
begin
  Mode0 := (Mode0 and $F1) or ((AValue and 7) shl 1);
  Mode1 := (Mode1 and $E7) or (AValue and $18);
end;

procedure TXPVideoRegs.SetReg16(AIndex: Integer; AValue: Word);
begin
  SetReg8(AIndex, AValue and 255);
  SetReg8(AIndex + 1, AValue shr 8);
end;

procedure TXPVideoRegs.SetReg8(AIndex: Integer; AValue: Byte);
var
  n, b: Byte;
begin
  n := AIndex and 255;
  b := (AIndex shr 8) and 255;
  FReg.Regs[n] := (FReg.Regs[n] and b) or ((AValue and (255 - b)) shl (AIndex shr 16));
  FAffected.Add(IntToStr(n));
  case n of
    46: FVideo.ExecuteCommand;
  end;
end;

procedure TXPVideoRegs.SetStatus(AIndex: Integer; AValue: Byte);
var
  n, b: Byte;
begin
  n := AIndex and 15;
  b := (AIndex shr 4) and 255;
  FReg.Status[n] := (FReg.Status[n] and b) or ((AValue and (255 - b)) shl (AIndex shr 12));
  FAffected.Add('S' + IntToStr(n));
end;

procedure TXPVideoRegs.SetStatusBool(AIndex: Integer; AValue: Boolean);
var
  m, b: Byte;
begin
  m := 1 shl (AIndex shr 4);
  AIndex := AIndex and 15;
  b := FReg.Status[AIndex];
  if AValue then
    FReg.Status[AIndex] := b or m
  else FReg.Status[AIndex] := b and (255 - m);
end;

{ TXPVideoPal }

function TXPVideoPal.GetValues(AIndex: Integer): Byte;
begin
  Result := FValues[AIndex and 1];
end;

procedure TXPVideoPal.AddAffected;
begin
  FAffected.Add(Format('P%d', [FIndex]));
end;

procedure TXPVideoPal.Assign(APal: TXPVideoPal);
begin
  FValues := APal.FValues;
  AddAffected;
end;

procedure TXPVideoPal.Clear;
begin
  GRB := 0;
end;

constructor TXPVideoPal.Create(AIndex: Integer; AAffected: TXPAffected);
begin
  FIndex := AIndex;
  FAffected := AAffected;
end;

function TXPVideoPal.GetBGR: TBGREntry;
begin
  Result.B := CB;
  Result.G := CG;
  Result.R := CR;
end;

function TXPVideoPal.GetBGRA: TBGRAEntry;
begin
  Result.B := CB;
  Result.G := CG;
  Result.R := CR;
  Result.A := $FF;
end;

function TXPVideoPal.GetColor: TColor;
begin
  Result := (CVals[B] shl 16) or (CVals[G] shl 8) or CVals[R];
end;

function TXPVideoPal.GetCPal(const Index: Integer): Byte;
begin
  Result := CVals[GetPal(Index)];
end;

function TXPVideoPal.GetGRB: Word;
begin
  Result := FValues[0] or (FValues[1] shl 8);
end;

function TXPVideoPal.GetPal(const Index: Integer): Byte;
begin
  Result := (FValues[(Index shr 12) and 1] shr ((Index shr 8) and 15)) and 7;
end;

procedure TXPVideoPal.SetBGR(const Value: TBGREntry);
begin
  CB := Value.B;
  CG := Value.G;
  CR := Value.R;
end;

procedure TXPVideoPal.SetBGRA(const Value: TBGRAEntry);
begin
  CB := Value.B;
  CG := Value.G;
  CR := Value.R;
end;

procedure TXPVideoPal.SetCPal(const Index: Integer; const Value: Byte);
begin
  SetPal(Index, Value shr 5);
end;

procedure TXPVideoPal.SetGRB(const Value: Word);
begin
  FValues[0] := Value and 255;
  FValues[1] := Value shr 8;
  AddAffected;
end;

procedure TXPVideoPal.SetPal(const Index: Integer; const Value: Byte);
var
  n: Integer;
begin
  n := (Index shr 12) and 1;
  FValues[n] := (FValues[n] and Index and 255) or ((Value and 7) shl ((Index shr 16) and 15));
  AddAffected;
end;

procedure TXPVideoPal.SetValues(AIndex: Integer; const Value: Byte);
begin
  FValues[AIndex and 1] := Value;
  AddAffected;
end;

end.
