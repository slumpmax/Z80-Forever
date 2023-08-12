unit XPCompiler;

{$MODE Delphi}

interface

uses
  XPConfig, XPVideo, XPDebug,
  LCLIntf, LCLType, Classes, SysUtils, Variants, Math, Forms;

type
  TInt64Rec = packed record
  case Integer of
    0: (VInt64: Int64);
    1: (VUInt64: UInt64);
    2: (VInteger: Integer);
    3: (VCardinal: Cardinal);
    4: (VSmallInt: SmallInt);
    5: (VWord: Word);
    6: (VShortInt: ShortInt);
    7: (VByte: Byte);
    8: (VByte8: array[0..7] of Byte);
    9: (VByte4: array[0..3] of Byte);
    10: (VByte2: array[0..1] of Byte);
    11: (VByte1: array[0..0] of Byte);
  end;

  TZ80RegType = (rgt8, rgt16a, rgt16b, rgtRegMem, rgtSys, rgtAddr, rgtValue,
    rgtIX = $DD, rgtIY = $FD);
  TZ80Register = record
    Text: string;
    Size, Offset: Byte;
    RegType: TZ80RegType;
    Index: ShortInt;
    Value: Word;
    function DifferSize(AReg: TZ80Register): Boolean;
    function DifferIndex(AReg: TZ80Register): Boolean;
    function IsIndexReg: Boolean;
    function IsByte: Boolean;
    function IsWord: Boolean;
    function HaveIndex: Boolean;
    function PreCode: Byte;
  end;

  TSymbolType = (sbtNone, sbtLabel, sbtConst, sbtClass);

  TSymbolObject = class
  private
    FName, FClassName: string;
    FTaked: Boolean;
    FUsed: Integer;
    FValue: Variant;
  protected
    function GetValueType: TSymbolType; virtual;
    function GetValue: Variant; virtual;
    procedure SetValue(AValue: Variant); virtual;
  public
    constructor Create(AName: string);
    property Name: string read FName;
    property Value: Variant read GetValue write SetValue;
    property ValueType: TSymbolType read GetValueType;
  end;

  TSymbolLabel = class(TSymbolObject)
  protected
    function GetValueType: TSymbolType; override;
  end;

  TSymbolConst = class(TSymbolObject)
  protected
    function GetValueType: TSymbolType; override;
  end;

  TSymbolClassItem = class
  private
    FIndex, FSize, FTotalSize: Integer;
    FName: string;
    FValue: Variant;
    function GetVBytes: Variant;
  public
    constructor Create;
    property VBytes: Variant read GetVBytes;
  end;

  TSymbolClass = class(TSymbolObject)
  private
    FItems: TList;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TSymbolClassItem; overload;
    function GetItems(AIndex: string): TSymbolClassItem; overload;
    function GetSize: Integer;
  protected
    function GetValueType: TSymbolType; override;
  public
    constructor Create(AName: string);
    destructor Destroy; override;
    procedure Clear;
    function AddItem(AItem: TSymbolClassItem): Integer;
    function IndexOf(AName: string): Integer;
    property Count: Integer read GetCount;
    property Size: integer read GetSize;
    property ItemFromIndex[AIndex: Integer]: TSymbolClassItem read GetItems;
    property Items[AIndex: string]: TSymbolClassItem read GetItems; default;
  end;

  TSymbolList = class
  private
    FList: TList;
    function GetSymbol(AName: string): TSymbolObject;
    function GetSymbolByIndex(AIndex: Integer): TSymbolObject;
    function GetValue(AName: string): Variant;
    function GetValueType(AName: string): TSymbolType;
    function AddItem(AItem: TSymbolObject): Integer;
    function GetValueString(AName: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Count: Integer;
    function UndefinedCount: Integer;
    function Add(AName: string): TSymbolObject;
    function AddClass(AName: string): TSymbolClass;
    function AddConst(AName: string; AValue: Variant): TSymbolConst;
    function AddLabel(AName: string; AValue: Variant): TSymbolLabel;
    function IndexOf(AName: string): Integer;
    property Symbol[AName: string]: TSymbolObject read GetSymbol; default;
    property SymbolByIndex[AIndex: Integer]: TSymbolObject read GetSymbolByIndex;
    property Value[AName: string]: Variant read GetValue;
    property ValueString[AName: string]: string read GetValueString;
    property ValueType[AName: string]: TSymbolType read GetValueType;
  end;

  TPostfixRec = record
    OpCode: Char;
    Value: Int64;
  end;

  TPostfixList = class
  private
    FList: array of TPostfixRec;
  public
    constructor Create;
    destructor Destroy; override;
    procedure PushOpcode(AOpCode: Char);
    procedure PushValue(AValue: Int64);
    function Pop: Variant;
    function PopInt64: Int64;
    function PopString: string;
  end;

  TSourceLine = record
  public
    FileName, Text: string;
    LineIndex: Integer;
    procedure Clear;
  end;

  TSourceTexts = class
  private
    FFileName: string;
    FTexts: TStrings;
    FLineIndex: Integer;
  public
    constructor Create(AFileName: string; ATexts: TStrings = nil); overload;
    destructor Destroy; override;
    function FetchLine: TSourceLine;
    property Texts: TStrings read FTexts;
  end;

  TSourceList = class
  private
    FSources: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Reset;
    function Add(AFileName: string; ATexts: TStrings = nil): Integer;
    function IndexOf(AFileName: string): Integer;
    function FetchLine: TSourceLine;
  end;

  TXPCompiler = class
  private
    FMemory, FCodes: array of Byte;
    FExpStack: Variant;
    FOpcode, FCondition, FExpression, FOutputExt, FPath: string;
    FSources: TSourceList;
    FLine: TSourceLine;
    FOutput: TStrings;
    FBuffers, FWarnings, FErrors: TStringList;
    FSymbols, FOpcodes1, FOpcodes2, FRegisterCode: TSymbolList;
    FErrorCount, FWarningCount, FPass, FOutputExtCount, FIndexReg, FPreIndex, FIndexOffset: Integer;
    FAddress, FPreAddress, FStartAddress, FEndAddress, FRunAddress: Integer;
    FHaveCode, FRunning, FLabeled: Boolean;
    FDebug: TXPDebug;
    function FetchOpcode(var AText: string): string;
    function FetchLine(var AText: string): string;
    function FetchInt64(var AText: string): Int64;
    function FetchByte(var AText: string): Integer;
    function FetchWord(var AText: string): Integer;
    function FetchDWord(var AText: string): Integer;
    function FetchQWord(var AText: string): Int64;
    function FetchRegister(var AText: string): TZ80Register;
    function FetchShortAddress(var AText: string): Integer;
    function FetchCondition(var AText: string): Integer;
    function FetchExp(var AText: string): string;
    function FetchLabel(var AText: string): string;
    function FetchNumber(var AText: string): string;
    function FetchString(var AText: string; out ADelim: string): string;
    function FetchFunction(var AText, AName: string): Boolean;
    procedure FetchPostfixExp(var AText: string; AList: TStrings);
    procedure FetchRegOp8(AOffset, AShift, AValueCode: Byte);
    procedure FetchShiftOp(AOffset: Byte);
    procedure FetchBitOp(AOffset: Byte);
    function FetchSymbolClassItem(AText: string): TSymbolClassItem;
    function PostfixToInt64(AList: TStrings): Variant;
    function RequireChar(var AText: string; AChar: Char; Forced: Boolean = True): Boolean;
    function RequireSpace(var AText: string; Forced: Boolean = True): Boolean;
    function RequireNewLine(var AText: string; Forced: Boolean = True): Boolean;
    function StrDecToInt64(AText: string): Int64;
    function StrBinToInt64(AText: string): Int64;
    function StrOctToInt64(AText: string): Int64;
    function StrHexToInt64(AText: string): Int64;
    function StrNumToInt64(AText: string): Int64;

    // for decompile
    function GetByte: Byte;
    function GetWord: Word;
    function GetByteStr: string;
    function GetWordStr: string;
    function GetMemStr: string;
    function GetAddrStr: string;
    function GetShortAddrStr: string;
    function GetReg16A(AIndex: Integer): string;
    function GetReg16B(AIndex: Integer): string;
    function GetReg16M(AIndex: Integer): string;
    function GetReg8(AIndex: Integer): string;
    procedure FetchIndexOffset;

    procedure AddCode(ACode: Byte);
    procedure AddCodes(ACodes: array of Byte); overload;
    procedure AddCodes(ACodes: Variant); overload;
    procedure AddText(AText: string);
    procedure AddCodeText(ACodeText: string);
    procedure AddDecodeText(ACode: string); overload;
    procedure AddDecodeText(ACode, AParam: string); overload;
    procedure AddDecodeText(ALabel, ACode, AParam: string); overload;
    procedure AddDecodeLabel;
    procedure AddDecodeConst;
    procedure AddError(AErrorText: string);
    procedure AddWarning(AWarningText: string);
    procedure AddReport(AText: string);
    procedure FlushBuffers(AForced: Boolean = False);
    procedure CompilePass(APass: Integer);
    function CompileCode0: Boolean;
    function CompileCode1: Boolean;
    function CompileCode2: Boolean;
    function CompileCode3: Boolean;
    function CompileCode9: Boolean;
    procedure DecompilePass(APass: Integer);
  public
    UseHeader: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Compile(AFileName: string; AReports: TStrings; ATexts: TStrings = nil);
    procedure Decompile(AFileName: string; AOutput: TStrings); overload;
    procedure Decompile(AOutput: TStrings; AStartAddress, AEndAddress, ARunAddress: Integer); overload;
    procedure Decompile(AOutput: TStrings); overload;
    procedure SaveToFile(AFileName: string; AFirstAddress, ALastAddress: Word);
    property Address: Integer read FAddress;
    property Debug: TXPDebug read FDebug;
    property ErrorCount: Integer read FErrorCount;
    property OutputExt: string read FOutputExt write FOutputExt;
    property StartAddress: Integer read FStartAddress;
    property WarningCount: Integer read FWarningCount;
  end;

const
  DefaultAddress = $0000;
  ActiveOperators = '[*]01:10 [/]02:10 [+]03:08 [-]04:08 [&]05:07 [^]06:06 [|]07:05';

implementation

{ TXPCompiler }

procedure TXPCompiler.AddCode(ACode: Byte);
var
  n: Integer;
begin
  n := Length(FCodes);
  SetLength(FCodes, n + 1);
  FCodes[n] := ACode;
end;

procedure TXPCompiler.AddCodes(ACodes: array of Byte);
var
  n: Integer;
begin
  for n := 0 to Length(ACodes) - 1 do AddCode(ACodes[n]);
end;

procedure TXPCompiler.AddCodes(ACodes: Variant);
var
  m, n: Integer;
begin
  if not VarIsArray(ACodes) then ACodes := VarArrayOf(ACodes);
  n := VarArrayLowBound(ACodes, 1);
  m := VarArrayHighBound(ACodes, 1);
  while n <= m do
  begin
    AddCode(Byte(ACodes[n]));
    Inc(n);
  end;
end;

procedure TXPCompiler.AddCodeText(ACodeText: string);
var
  n, nc, nd: Integer;
  s: string;
begin
  if ACodeText = '' then Exit;
  if FPass = 3 then FDebug.Lines.AddLine(FAddress, FCodes, ACodeText, FDebug.GetCycle(FCodes));
  nc := Length(FCodes);
  n := 0;
  s := '';
  repeat
    if n < nc then
    begin
      if n = 0 then
        s := Format('%4.4X: ', [FAddress])
      else s := '      ';
      nd := Min(n + 4, nc);
      while n < nd do
      begin
        s := s + Format('%2.2X ', [FCodes[n]]);
        FMemory[FAddress] := FCodes[n];
        FDebug.WriteByte(FAddress, FCodes[n]);
        if not FHaveCode then
        begin
          FStartAddress := FAddress;
          FHaveCode := True;
        end;
        Inc(FAddress);
        Inc(n);
      end;
    end;
    if FPass = 3 then FBuffers.Add(Format('%-18s %s', [s, ACodeText]));
    ACodeText := '';
  until n >= nc;
end;

procedure TXPCompiler.AddDecodeText(ACode: string);
begin
  AddDecodeText('', ACode, '');
end;

procedure TXPCompiler.AddDecodeText(ACode, AParam: string);
begin
  AddDecodeText('', ACode, AParam);
end;

procedure TXPCompiler.AddDecodeConst;
var
  sym: TSymbolObject;
  n: Integer;
begin
  if FPass = 1 then Exit;
  for n := 0 to FSymbols.Count - 1 do
  begin
    sym := FSymbols.SymbolByIndex[n];
    if (sym.ValueType = sbtConst) or not sym.FTaked then AddDecodeText(sym.Name, 'EQU', Format('$%X', [Word(sym.Value)]));
  end;
  if FSymbols.Count > 0 then AddDecodeText('');
end;

procedure TXPCompiler.AddDecodeLabel;
var
  sym: TSymbolObject;
  n: Integer;
begin
  for n := 0 to FSymbols.Count - 1 do
  begin
    sym := FSymbols.SymbolByIndex[n];
    if (sym.Value >= FPreAddress) and (sym.Value < FAddress) then
    begin
      if sym.ValueType = sbtLabel then
      begin
        AddReport('');
        if sym.Value = FPreAddress then
          AddReport(sym.Name + ':')
        else
        begin
          AddReport(sym.Name + '_0:');
          AddReport(Format('%sEQU %s_0 + %d', [GetTabStr(sym.Name), sym.Name, Word(sym.Value - FPreAddress)]));
        end;
        sym.FTaked := True;
      end
      else AddReport(Format('; %sEQU $%4.4X', [GetTabStr(sym.Name), Word(sym.Value)]));
    end;
  end;
end;

procedure TXPCompiler.AddDecodeText(ALabel, ACode, AParam: string);
begin
  AddDecodeLabel;
  if FIndexReg > 0 then
  begin
    AddReport(GetTabStr(ALabel) + GetTabStr('DB') + Format('$%2.2X', [FIndexReg]));
    FIndexReg := 0;
    ALabel := '';
  end;
  if ACode <> '' then ALabel := GetTabStr(ALabel);
  if AParam <> '' then ACode := GetTabStr(ACode);
  AddReport(ALabel + ACode + AParam);
  FPreAddress := FAddress;
end;

procedure TXPCompiler.AddError(AErrorText: string);
var
  s: string;
begin
//  if FPass = 1 then Exit;
  if FPass < 3 then Exit;
  if FOpcode <> '' then AErrorText := '[' + FOpcode + '] ' + AErrorText;
  s := UpperCase(ExtractFileName(FLine.FileName));
  AddReport(Format('Error in [%s] line %d: %s', [s, FLine.LineIndex + 1, AErrorText]));
  FErrors.Add(Format('[%4.4X] Error in [%s] line %d: %s', [FAddress, s, FLine.LineIndex + 1, AErrorText]));
  Inc(FErrorCount);
//  FOpcode := '';
end;

procedure TXPCompiler.AddReport(AText: string);
begin
//  if FPass = 1 then Exit;
  if FPass < 3 then Exit;
  FBuffers.Add(AText);
  FlushBuffers;
end;

procedure TXPCompiler.AddText(AText: string);
begin
  if FPass < 3 then Exit;
  AddReport(AText);
end;

procedure TXPCompiler.AddWarning(AWarningText: string);
begin
  if FPass < 3 then Exit;
  if FOpcode <> '' then AWarningText := '[' + FOpcode + '] ' + AWarningText;
  AddReport(Format('Warning in line %d: %s', [FLine.LineIndex, AWarningText]));
  FWarnings.Add(Format('[%4.4X] Warning in line %d: %s', [FAddress, FLine.LineIndex, AWarningText]));
  Inc(FWarningCount);
end;

procedure TXPCompiler.Compile(AFileName: string; AReports, ATexts: TStrings);
var
  n, nl, ml, nd, md: Integer;
  sym: TSymbolObject;
  cls: TSymbolClass;
  ci: TSymbolClassItem;
  s, sd: string;
  v: Variant;
begin
  FPath := IncludeTrailingPathDelimiter(ExtractFilePath(AFileName));
  AReports.Clear;
  FBuffers.Clear;
  FSymbols.Clear;
  FErrors.Clear;
  FWarnings.Clear;
  FOutput := AReports;
  FErrorCount := 0;
  FWarningCount := 0;
  FillChar(FMemory[0], 65536, 0);
  FSources.Clear;
  FSources.Add(AFileName, ATexts);
  CompilePass(1);
  CompilePass(2);
  CompilePass(3);
  AddReport('');
  AddReport('Label list');
  AddReport('==========');
  for n := 0 to FSymbols.Count - 1 do
  begin
    sym := FSymbols.SymbolByIndex[n];
    try
      if sym.ValueType = sbtLabel then AddReport(Format('%-17s: %4.4xH', [sym.Name, Integer(sym.Value)]));
    except
      AddReport('NO');
    end;
  end;
  AddReport('');
  AddReport('Symbol list');
  AddReport('===========');
  for n := 0 to FSymbols.Count - 1 do
  begin
    with FSymbols.SymbolByIndex[n] do
    begin
      if ValueType = sbtConst then AddReport(Format('%-17s: %s', [Name, Value]));
    end;
  end;
  AddReport('');
  AddReport('Class list');
  AddReport('===========');
  for n := 0 to FSymbols.Count - 1 do
  begin
    sym := FSymbols.SymbolByIndex[n];
    if sym is TSymbolClass then
    begin
      cls := TSymbolClass(sym);
      AddReport(Format('%s:', [cls.Name]));
      ml := cls.Count - 1;
      for nl := 0 to ml do
      begin
        ci := cls.Items[nl];
        case ci.FSize of
          0: sd := 'DS';
          1: sd := 'DB';
          2: sd := 'DW';
          4: sd := 'DD';
          8: sd := 'DQ';
        end;
        s := Format('    %4.4Xh    %-12s    %-4s', [ci.FIndex, ci.FName, sd]);
        md := VarArrayHighBound(ci.FValue, 1);
        for nd := 0 to md do
        begin
          if (nd > 0) then
          begin
            if ((nd and 3) = 0) and (nd > 0) then
            begin
              AddReport(s);
              s := '            ';
            end
            else s := s + ', ';
          end;
          v := ci.FValue[nd];
          case ci.FSize of
            0:
              if VarArrayHighBound(v, 1) > 0 then
                s := s + Format('%4.4Xh, %2.2Xh', [Integer(v[0]), Integer(v[1])])
              else s := s + Format('%4.4Xh', [Integer(v[0])]);
            1: s := s + Format('%2.2Xh', [Integer(v)]);
            2: s := s + Format('%4.4Xh', [Integer(v)]);
            4: s := s + Format('%8.8Xh', [Integer(v)]);
            8: s := s + Format('%16.16Xh', [Integer(v)]);
          end;
        end;
        if md >= 0 then AddReport(s);
      end;
    end;
  end;
  AddReport('');
  AddReport('Unused symbol list');
  AddReport('=====================');
  for n := 0 to FSymbols.Count - 1 do
  begin
    with FSymbols.SymbolByIndex[n] do
    begin
      if FUsed = 0 then
      begin
        if ValueType = sbtClass then
          AddReport(Format('%-17s: CLASS', [Name]))
        else AddReport(Format('%-17s: %s', [Name, Value]));
      end;
    end;
  end;
  AddReport('');
  AddReport(Format('%d Undefined symbols', [FSymbols.UndefinedCount]));
  AddReport('=====================');
  for n := 0 to FSymbols.Count - 1 do
  begin
    with FSymbols.SymbolByIndex[n] do
    begin
      if ValueType = sbtNone then AddReport(Format('%-17s: %s', [Name, Value]));
    end;
  end;
  AddReport('');
  AddReport(Format('%d Error(s)', [FErrorCount]));
  AddReport('=====================');
  for n := 0 to FErrors.Count - 1 do
  begin
    AddReport(FErrors[n]);
  end;
  AddReport('');
  AddReport(Format('%d Warning(s)', [FWarningCount]));
  AddReport('=====================');
  for n := 0 to FWarnings.Count - 1 do
  begin
    AddReport(FWarnings[n]);
  end;
  FlushBuffers(True);
  FDebug.UpdateSelectIndexs;
  FDebug.Video.SaveMemory;
end;

function TXPCompiler.CompileCode0: Boolean;
var
  fname, s, delim: string;
  n: Integer;
const
  PragmaStr: string = '000[#OUTEXT] 001[#DEBUG] 002[#INCLUDE]'; // 00-07
  DebugStr: string = '000[START] 001[STACK] 002[SCREEN] 003[SLOAD] '
    + '004[BLOAD] 005[BORDER] 006[SYSTEM]';
  SystemStr: string = '000[DOS] 001[BASIC]';
begin
  Result := True;
  case TextIndex(PragmaStr, FOpcode, 3) of
    0: // #OUTEXT
    begin
      RequireSpace(FLine.Text);
      FOutputExt := FetchLabel(FLine.Text);
      AddCodeText(Format('#OUTEXT   %s', [FOutputExt]));
      Inc(FOutputExtCount);
      if FOutputExtCount > 1 then AddWarning(Format('Output extension define more than once. (%s)', [FOutputExt]));
      AddText('');
    end;
    1: // #DEBUG
    begin
      RequireSpace(FLine.Text);
      s := FetchOpcode(FLine.Text);
      case TextIndex(DebugStr, s, 3) of
        0: // START
        begin
          RequireSpace(FLine.Text);
          FDebug.Regs.PC := FetchWord(FLine.Text);
          FDebug.StartAddress := FDebug.Regs.PC;
        end;
        1: // STACK
        begin
          RequireSpace(FLine.Text);
          FDebug.Regs.SP := FetchWord(FLine.Text);
          FDebug.StackAddress := FDebug.Regs.SP;
        end;
        2: // SCREEN
        begin
          RequireSpace(FLine.Text);
          FDebug.Video.StartScreen := FetchByte(FLine.Text);
          FDebug.Video.ScreenMode(FDebug.Video.StartScreen);
          FDebug.Video.UpdateScreen;
        end;
        3: // SLOAD
        begin
          RequireSpace(FLine.Text);
          fname := FetchString(FLine.Text, delim);
          if (fname <> '') and FileExists(FPath + fname) then
          begin
            RequireSpace(FLine.Text);
            n := FetchDWord(FLine.Text);
            FDebug.Video.Bload(FPath + fname, n);
          end
          else AddError(Format('File not found [%s]', [fname]));
        end;
        4: // BLOAD
        begin
          RequireSpace(FLine.Text);
          fname := FetchString(FLine.Text, delim);
          if (fname <> '') and FileExists(FPath + fname) then
          begin
            RequireSpace(FLine.Text);
            n := FetchByte(FLine.Text);
            FDebug.MainRAM.Bload(FPath + fname, n);
          end
          else AddError(Format('File not found [%s]', [fname]));
        end;
        5: // BORDER
        begin
          RequireSpace(FLine.Text);
          FDebug.Video.StartBorder := FetchByte(FLine.Text);
        end;
        6: // SYSTEM
        begin
          RequireSpace(FLine.Text);
          s := UpperCase(FetchOpcode(FLine.Text));
          case TextIndex(SystemStr, s, 3) of
            0: FDebug.System := sysDOS;
            1: FDebug.System := sysBASIC;
          else
            AddError(Format('Unknown debug system [%s]', [s]));
          end;
        end;
      else
        AddError(Format('Unknown "#DEBUG %s"', [s]));
      end;
    end;
    2: // #INCLUDE
    begin
      RequireSpace(FLine.Text);
      fname := FetchString(FLine.Text, delim);
      RequireNewLine(FLine.Text, True);
      if (fname <> '') and FileExists(FPath + fname) then
      begin
        if FSources.Add(FPath + fname) < 0 then AddError(Format('Duplicate include file [%s]', [fname]));
      end
      else AddError(Format('File not found [%s]', [fname]));
    end;
  else
    Result := False;
  end;
end;

function TXPCompiler.CompileCode1: Boolean;
const
  OpcodeStr: string =
      '000[ORG]  000[.ORG] 001[.END]'
    + '002[EQU]  002[.EQU]'
    + '003[DB]   003[DEFB] 003[.DB] 003[.DEFB]'
    + '003[DM]   003[DEFM] 003[.DM] 003[.DEFM]'
    + '004[DW]   004[DEFW] 004[.DW] 004[.DEFW]'
    + '005[DD]   005[DEFD] 005[.DD] 005[.DEFD]'
    + '006[DQ]   006[DEFQ] 006[.DQ] 006[.DEFQ]'
    + '007[DS]   007[DEFS] 007[.DS] 007[.DEFS]'
    + '020[LD]   021[DJNZ] 022[INC] 023[DEC]'
    + '024[ADD]  025[ADC]  026[SUB] 027[SBC]'
    + '028[AND]  029[XOR]  030[OR]  031[CP]'
    + '032[CALL] 033[RET]  034[POP] 035[PUSH]'
    + '036[JR]   037[JP]   038[RST] 039[IN]'
    + '040[OUT]  041[EX]   042[RLC] 043[RRC]'
    + '044[RL]   045[RR]   046[SLA] 047[SRA]'
    + '048[SLL]  049[SRL]  050[BIT] 051[RES]'
    + '052[SET]  053[CLASS]';
  OpcodeExStr = '[AF,AF'']0800[DE,HL]EB00[(SP),HL]E300[(SP),IX]E3DD[(SP),IY]E3FD';
var
  n: TInt64Rec;
  p, cd: Integer;
  reg, reg2: TZ80Register;
  s, sc: string;
  buf: array of Byte;
  w: Word;
  b: Byte;
begin
  p := Pos('[' + FOpcode + ']', OpcodeStr);
  Result := p > 0;
  if not Result then Exit;
  p := StrToIntDef(Copy(OpcodeStr, p - 3, 3), -1);
  case p of
    0: // ORG
    begin
      RequireSpace(FLine.Text);
      w := FAddress;
      FAddress := FetchWord(FLine.Text);
      AddCodeText(Format('ORG   %4.4XH', [FAddress]));
      if (FAddress < w) and FHaveCode then AddWarning('Origin address less than before');
      if (FAddress < FStartAddress) or not FHaveCode then FStartAddress := FAddress;
      if FDebug.StartAddress < 0 then FDebug.StartAddress := FAddress;
      RequireNewLine(FLine.Text);
      AddText('');
    end;
    1: FRunning := False; // .END
    2, 53: // EQU .EQU CLASS
    begin
      AddError('Symbol name is missing');
    end;
    3: // DB DEFB DM DEFM .DB .DEFB .DM .DEFM
    begin
      s := 'DB    ';
      sc := '';
      repeat
        n.VByte := FetchByte(FLine.Text);
        AddCode(n.VByte);
        s := s + sc + Format('%2.2XH', [n.VByte]);
        sc := ',';
      until not RequireChar(FLine.Text, ',', False);
      AddCodeText(s);
    end;
    4: // DW DEFW .DW .DEFW
    begin
      s := 'DW    ';
      sc := '';
      repeat
        n.VWord := FetchWord(FLine.Text);
        AddCodes(n.VByte2);
        s := s + sc + Format('%4.4XH', [n.VWord]);
        sc := ',';
      until not RequireChar(FLine.Text, ',', False);
      AddCodeText(s);
    end;
    5: // DD DEFD .DD .DEFD
    begin
      s := 'DD    ';
      sc := '';
      repeat
        n.VCardinal := FetchDWord(FLine.Text);
        AddCodes(n.VByte4);
        s := s + sc + Format('%8.8XH', [n.VWord]);
        sc := ',';
      until not RequireChar(FLine.Text, ',', False);
      AddCodeText(s);
    end;
    6: // DQ DEFQ .DQ .DEFQ
    begin
      s := 'DQ    ';
      sc := '';
      repeat
        n.VUInt64 := FetchQWord(FLine.Text);
        AddCodes(n.VByte8);
        s := s + sc + Format('%16.16XH', [n.VUInt64]);
        sc := ',';
      until not RequireChar(FLine.Text, ',', False);
      AddCodeText(s);
    end;
    7: // DS DEFS .DS .DEFS
    begin
      w := FetchWord(FLine.Text);
      s := Format('DS    %4.4XH', [w]);
      if RequireChar(FLine.Text, ',', False) then
      begin
        b := FetchByte(FLine.Text);
        s := s + Format(',%2.2XH', [b]);
      end
      else b := 0;
      buf := nil;
      SetLength(buf, w);
      try
        FillChar(buf[0], w, b);
        AddCodes(buf);
      finally
        SetLength(buf, 0);
      end;
      AddCodeText(s);
    end;
    20: // LD
    begin
      RequireSpace(FLine.Text);
      reg := FetchRegister(FLine.Text);
      RequireChar(FLine.Text, ',');
      reg2 := FetchRegister(FLine.Text);
      if reg.IsIndexReg then
        AddCode(reg.PreCode)
      else if reg2.IsIndexReg then AddCode(reg2.PreCode);
      if reg.DifferSize(reg2) then
        AddError('Register size not match')
      else if reg.IsByte and reg2.IsByte then
      begin
        case reg.RegType of
          rgtValue: AddError('Can not assign to value');
          rgtAddr:
            if reg2.Offset = 7 then
              AddCodes([$32, reg.Value and 255, reg.Value shr 8])
            else AddError('Not support ' + reg2.Text);
          rgtRegMem:
          begin
            if reg2.Text <> 'A' then
              AddError('Bad register: ' + reg2.Text)
            else AddCode(2 + (reg.Offset shl 4));
          end;
          rgtSys:
          begin
            if reg2.Text <> 'A' then
              AddError('Bad register: ' + reg2.Text)
            else AddCodes([$ED, $47 + (reg.Offset shl 3)]);
          end
        else
          case reg2.RegType of
            rgtValue:
            begin
              AddCode(6 + (reg.Offset shl 3));
              if reg.HaveIndex then AddCode(reg.Index);
              AddCode(reg2.Offset);
            end;
            rgtAddr:
              if reg.Offset = 7 then
                AddCodes([$3A, reg2.Value and 255, reg2.Value shr 8])
              else AddError('Not support ' + reg.Text);
            rgtRegMem:
            begin
              if reg.Text <> 'A' then
                AddError('Bad register: ' + reg.Text)
              else AddCode($A + (reg2.Offset shl 4));
            end;
            rgtSys:
            begin
              if reg.Text <> 'A' then
                AddError('Bad register: ' + reg.Text)
              else AddCodes([$ED, $57 + (reg2.Offset shl 3)]);
            end
          else
            if (reg.Offset = 6) and (reg2.Offset = 6) then
              AddError('Not supported')
            else
            begin
              if (reg.Offset in [4, 5]) and (reg2.Offset in [4, 5])
                and (reg.RegType <> reg2.RegType)
              then
                AddError('Register size mismatch')
              else
              begin
                AddCode($40 + (reg.Offset shl 3) + reg2.Offset);
                if reg.HaveIndex then AddCode(reg.Index);
                if reg2.HaveIndex then AddCode(reg2.Index);
              end;
            end;
          end;
        end;
      end
      else
      begin
        case reg.RegType of
          rgt16a, rgtIX, rgtIY:
          begin
            case reg2.RegType of
              rgtValue: AddCodes([1 + (reg.Offset shl 4), reg2.Value and 255, reg2.Value shr 8]);
              rgtAddr:
              begin
                if reg.Offset = 2 then
                  AddCode($2A)
                else AddCodes([$ED, $4B + reg.Offset shl 4]);
                AddCodes([reg2.Value and 255, reg2.Value shr 8]);
              end;
            else
              if (reg.Offset = 3) and (reg2.Offset = 2) then
                AddCode($F9)
              else AddError('Expression not supported ' + reg2.Text);
            end;
          end;
          rgtAddr:
          begin
            case reg2.RegType of
              rgt16a, rgtIX, rgtIY:
              begin
                if reg2.Offset = 2 then
                  AddCode($22)
                else AddCodes([$ED, $43 + (reg2.Offset shl 4)]);
                AddCodes([reg.Value and 255, reg.Value shr 8]);
              end;
            end;
          end
        else
        end;
      end;
      s := Format('%-6.6s%s,%s', [FOpCode, reg.Text, reg2.Text]);
      AddCodeText(s);
      RequireNewLine(FLine.Text);
    end;
    21, 36: // DJNZ, JR
    begin
      RequireSpace(FLine.Text);
      cd := FetchCondition(FLine.Text);
      if (cd > 24) or ((cd >= 0) and (p = 21)) then
        AddError('Condition error ' + FCondition)
      else
      begin
        FOpCode := Format('%-6.6s', [FOpCode]);
        if p = 21 then
          cd := $10
        else if cd < 0 then
          cd := $18
        else
        begin
          cd := $20 + cd;
          FOpCode := FOpCode + FCondition + ','
        end;
        s := UpperCase(Trim(FLine.Text));
        n.VShortInt := FetchShortAddress(FLine.Text);
        AddCodes([cd, n.VByte]);
        AddCodeText(Format('%s%s (%4.4XH)', [FOpCode, s, FAddress + n.VShortInt + 2]));
        RequireNewLine(FLine.Text);
      end;
    end;
    22: // INC
    begin
      reg := FetchRegister(FLine.Text);
      if reg.IsIndexReg then
      begin
        AddCode(reg.PreCode);
      end;
      if reg.IsByte then
      begin
        AddCode(4 + (reg.Offset shl 3));
        if reg.HaveIndex then AddCode(reg.Index);
      end
      else
      begin
        if (reg.RegType <> rgt16a) and not reg.IsIndexReg then
          AddError('Bad register ' + reg.Text)
        else AddCode(3 + (reg.Offset shl 4));
      end;
      AddCodeText(Format('%-6.6s%s', [FOpCode, reg.Text]));
      RequireNewLine(FLine.Text);
    end;
    23: // DEC
    begin
      reg := FetchRegister(FLine.Text);
      if reg.IsIndexReg then AddCode(reg.PreCode);
      if reg.IsByte then
      begin
        AddCode(5 + (reg.Offset shl 3));
        if reg.HaveIndex then AddCode(reg.Index);
      end
      else
      begin
        if (reg.RegType <> rgt16a) and not reg.IsIndexReg then
          AddError('Bad register ' + reg.Text)
        else AddCode(11 + (reg.Offset shl 4));
      end;
      AddCodeText(Format('%-6.6s%s', [FOpCode, reg.Text]));
      RequireNewLine(FLine.Text);
    end;
    24: // ADD
    begin
      RequireSpace(FLine.Text);
      reg := FetchRegister(FLine.Text);
      if (reg.Text <> 'A') and (reg.Text <> 'HL') and (reg.Text <> 'IX') and (reg.Text <> 'IY') then
        AddError('Bad register ' + reg.Text)
      else
      begin
        RequireChar(FLine.Text, ',');
        reg2 := FetchRegister(FLine.Text);
        if reg.IsIndexReg then
          AddCode(reg.PreCode)
        else if reg2.IsIndexReg then AddCode(reg2.PreCode);
        if reg.DifferSize(reg2) then
          AddError('Bad register ' + reg.Text)
        else
        begin
//          if reg2.IsIndexReg then AddCode(reg2.PreCode);
          if reg.IsByte then
          begin
            if reg2.RegType = rgtValue then
              AddCodes([$C6, reg2.Offset])
            else
            begin
              AddCode($80 + reg2.Offset);
              if reg2.HaveIndex then AddCode(reg2.Index);
            end;
          end
          else
          begin
            if reg.DifferIndex(reg2) then
              AddError('Bad register ' + reg2.Text)
            else if (reg2.Offset < 4) then
              AddCode(9 + (reg2.Offset shl 4))
            else AddError('Bad syntax: ' + reg2.Text);
          end;
          AddCodeText(Format('%-6.6s%s,%s', [FOpCode, reg.Text, reg2.Text]));
          RequireNewLine(FLine.Text);
        end;
      end;
    end;
    25: // ADC
    begin
      reg := FetchRegister(FLine.Text);
      if (reg.Text <> 'A') and (reg.Text <> 'HL') and (reg.Text <> 'IX') and (reg.Text <> 'IY') then
        AddError('Bad register ' + reg.Text)
      else
      begin
        RequireChar(FLine.Text, ',', True);
        reg2 := FetchRegister(FLine.Text);
        if reg.DifferSize(reg2) then
          AddError('Bad register ' + reg.Text)
        else
        begin
          if reg.IsIndexReg then
            AddCode(reg.PreCode)
          else if reg2.IsIndexReg then AddCode(reg2.PreCode);
          if reg.IsByte then
          begin
            if reg2.RegType = rgtValue then
              AddCodes([$CE, reg2.Offset])
            else
            begin
              AddCode($88 + reg2.Offset);
              if reg2.HaveIndex then AddCode(reg2.Index);
            end;
          end
          else
          begin
            if reg.DifferIndex(reg2) then
              AddError('Bad register ' + reg2.Text)
            else
            begin
              AddCodes([$ED, $4A + (reg2.Offset shl 4)]);
            end;
          end;
          AddCodeText(Format('%-6.6s%s,%s', [FOpCode, reg.Text, reg2.Text]));
          RequireNewLine(FLine.Text);
        end;
      end;
    end;
    26: // SUB
    begin
      FetchRegOp8($90, 0, $D6);
    end;
    27: // SBC
    begin
      reg := FetchRegister(FLine.Text);
      if (reg.Text <> 'A') and (reg.Text <> 'HL') and (reg.Text <> 'IX') and (reg.Text <> 'IY') then
        AddError('Bad register ' + reg.Text)
      else
      begin
        RequireChar(FLine.Text, ',', True);
        reg2 := FetchRegister(FLine.Text);
        if reg.Size <> reg2.Size then
          AddError('Bad register ' + reg.Text)
        else
        begin
          if reg2.IsIndexReg then AddCode(reg2.PreCode);
          if reg.IsByte then
          begin
            if reg2.RegType = rgtValue then
              AddCodes([$DE, reg2.Offset])
            else
            begin
              AddCode($98 + reg2.Offset);
              if reg2.HaveIndex then AddCode(reg2.Index);
            end;
          end
          else
          begin
            if reg.DifferIndex(reg2) then
              AddError('Bad register ' + reg2.Text)
            else
            begin
              AddCodes([$ED, $42 + (reg2.Offset shl 4)]);
            end;
          end;
          AddCodeText(Format('%-6.6s%s,%s', [FOpCode, reg.Text, reg2.Text]));
          RequireNewLine(FLine.Text);
        end;
      end;
    end;
    28: // AND
    begin
      FetchRegOp8($A0, 0, $E6);
    end;
    29: // XOR
    begin
      FetchRegOp8($A8, 0, $EE);
    end;
    30: // OR
    begin
      FetchRegOp8($B0, 0, $F6);
    end;
    31: // CP
    begin
      FetchRegOp8($B8, 0, $FE);
    end;
    32: // CALL
    begin
      RequireSpace(FLine.Text);
      cd := FetchCondition(FLine.Text);
      FOpCode := Format('%-6.6s', [FOpCode]);
      if cd < 0 then
        cd := $CD
      else
      begin
        cd := $C4 + cd;
        FOpCode := FOpCode + FCondition + ','
      end;
      s := UpperCase(Trim(FLine.Text));
      n.VWord := FetchWord(FLine.Text);
      AddCodes([cd, n.VByte2[0], n.VByte2[1]]);
      AddCodeText(Format('%s%s (%4.4XH)', [FOpCode, s, n.VWord]));
      RequireNewLine(FLine.Text);
    end;
    33: // RET
    begin
      if RequireSpace(FLine.Text, False) then
      begin
        cd := FetchCondition(FLine.Text);
        FOpCode := Format('%-6.6s', [FOpCode]);
        cd := $C0 + cd;
        FOpCode := FOpCode + FCondition;
      end
      else cd := $C9;
      AddCode(cd);
      AddCodeText(FOpCode);
      RequireNewLine(FLine.Text);
    end;
    34: // POP
    begin
      reg := FetchRegister(FLine.Text);
      if reg.IsIndexReg then AddCode(reg.PreCode);
      if reg.IsByte then
        AddError('Bad register ' + reg.Text)
      else AddCode($C1 + (reg.Offset shl 4));
      AddCodeText(Format('%-6.6s%s', [FOpCode, reg.Text]));
      RequireNewLine(FLine.Text);
    end;
    35: // PUSH
    begin
      reg := FetchRegister(FLine.Text);
      if reg.IsIndexReg then AddCode(reg.PreCode);
      if reg.IsByte then
        AddError('Bad register ' + reg.Text)
      else AddCode($C5 + (reg.Offset shl 4));
      AddCodeText(Format('%-6.6s%s', [FOpCode, reg.Text]));
      RequireNewLine(FLine.Text);
    end;
    37: // JP
    begin
      RequireSpace(FLine.Text);
      if SameText(FLine.Text, '(HL)') then
      begin
        AddCode($E9);
        AddCodeText(FOpCode + '    (HL)');
//        RequireNewLine(FLine);
      end
      else if SameText(FLine.Text, '(IX)') then
      begin
        AddCodes([$DD, $E9]);
        AddCodeText(FOpCode + '    (IX)');
//        RequireNewLine(FLine);
      end
      else if SameText(FLine.Text, '(IY)') then
      begin
        AddCodes([$FD, $E9]);
        AddCodeText(FOpCode + '    (IY)');
//        RequireNewLine(FLine);
      end
      else
      begin
        cd := FetchCondition(FLine.Text);
        FOpCode := Format('%-6.6s', [FOpCode]);
        if cd < 0 then
          cd := $C3
        else
        begin
          cd := $C2 + cd;
          FOpCode := FOpCode + FCondition + ','
        end;
        n.VWord := FetchWord(FLine.Text);
        AddCodes([cd, n.VByte2[0], n.VByte2[1]]);
        AddCodeText(Format('%s%4.4XH ; %s', [FOpCode, n.VWord, FCondition]));
//        RequireNewLine(FLine);
      end;
    end;
    38: // RST
    begin
      n.VByte := FetchByte(FLine.Text);
      if (n.VByte and $C7) <> 0 then
        AddError('Bad address ' + FExpression)
      else AddCode($C7 + n.VByte);
      AddCodeText(Format('%-6.6s%2.2XH', [FOpCode, n.VByte]));
//      RequireNewLine(FLine);
    end;
    39: // IN
    begin
      RequireSpace(FLine.Text);
      reg := FetchRegister(FLine.Text);
      RequireChar(FLine.Text, ',');
      if reg.IsWord or (reg.Offset = 6) then
        AddError('Bad register ' + reg.Text)
      else if (Copy(FLine.Text, 1, 1) <> '(') or (Copy(FLine.Text, Length(FLine.Text), 1) <> ')') then
        AddError('Bad port: ' + FLine.Text)
      else
      begin
        s := UpperCase(Trim(Copy(FLine.Text, 2, Length(FLine.Text) - 2)));
        if reg.Offset = 7 then
        begin
          if s = 'C' then
            AddCodes([$ED, $78])
          else
          begin
            n.VByte := FetchByte(s);
            AddCodes([$DB, n.VByte]);
            s := Format('%2.2XH', [n.VByte]);
          end;
        end
        else if s = 'C' then
          AddCodes([$ED, $40 + reg.Offset shl 3])
        else AddError('Bad port number: ' + s);
      end;
      AddCodeText(Format('%-6.6s%s,(%s)', [FOpCode, reg.Text, s]));
    end;
    40: // OUT
    begin
      RequireSpace(FLine.Text);
      RequireChar(FLine.Text, '(');
      FLine.Text := UpperCase(Trim(FLine.Text));
      p := Pos(')', FLine.Text);
      if p = 0 then
        AddError('Bad port')
      else
      begin
        s := Trim(Copy(FLine.Text, 1, p - 1));
        Delete(FLine.Text, 1, p);
        RequireChar(FLine.Text, ',');
        reg := FetchRegister(FLine.Text);
        if reg.IsWord or (reg.Offset = 6) then
          AddError('Bad register ' + reg.Text)
        else if reg.Offset = 7 then
        begin
          if s = 'C' then
            AddCodes([$ED, $79])
          else
          begin
            n.VByte := FetchByte(s);
            AddCodes([$D3, n.VByte]);
            s := Format('%2.2XH', [n.VByte]);
          end;
        end
        else if s <> 'C' then
          AddError('Bad port number: ' + s)
        else AddCodes([$ED, $41 + reg.Offset shl 3]);
      end;
      AddCodeText(Format('%-6.6s(%s),%s', [FOpCode, s, reg.Text]));
      RequireNewLine(FLine.Text);
    end;
    41: // EX
    begin
      RequireSpace(FLine.Text);
      s := UpperCase(Trim(FetchLine(FLine.Text)));
      p := Pos('[' + s + ']', OpcodeExStr);
      if p = 0 then
        AddError('Unknown register: ' + s)
      else
      begin
        s := Copy(OpcodeExStr, p + Length(s) + 2, 4);
        n.VWord := StrToIntDef('$' + s, 0);
        if n.VByte2[0] = 0 then
          AddCode(n.VByte2[1])
        else AddCodes(n.VByte2);
        AddCodeText(Format('%-6.6s%s', [FOpCode, FLine.Text]));
      end;
    end;
    42: FetchShiftOp(0);   // RLC
    43: FetchShiftOp(8);   // RRC
    44: FetchShiftOp($10); // RL
    45: FetchShiftOp($18); // RR
    46: FetchShiftOp($20); // SLA
    47: FetchShiftOp($28); // SRA
    48: FetchShiftOp($30); // SLL
    49: FetchShiftOp($38); // SRL
    50: FetchBitOp($40);   // BIT
    51: FetchBitOp($80);   // RES
    52: FetchBitOp($C0);   // SET
  end;
  FOpcode := '';
end;

function TXPCompiler.CompileCode2: Boolean;
var
  sym: TSymbolObject;
begin
  sym := FOpcodes1.Symbol[FOpcode];
  Result := Assigned(sym);
  if Result then
  begin
    AddCode(sym.Value);
    AddCodeText(FOpCode);
    RequireNewLine(FLine.Text);
  end;
end;

function TXPCompiler.CompileCode3: Boolean;
var
  sym: TSymbolObject;
begin
  sym := FOpcodes2.Symbol[FOpcode];
  Result := Assigned(sym);
  if Result then
  begin
    AddCodes([sym.Value shr 8, sym.Value and 255]);
    AddCodeText(FOpCode);
    RequireNewLine(FLine.Text);
  end;
end;

function TXPCompiler.CompileCode9: Boolean;
var
  sym: TSymbolObject;
  cls: TSymbolClass;
  ci: TSymbolClassItem;
  p, offs, nf, nd, md: Integer;
  have, done: Boolean;
  v: Variant;
  s, c: string;
begin
  Result := False;
  if FLabeled then
  begin
    AddError('Invalid OPCODE');
    FOpcode := '';
    Exit;
  end;
  have := RequireChar(FLine.Text, ':', False);
  sym := FSymbols[FOpcode];
  if sym <> nil then
  begin
    if sym.FTaked then
    begin
      AddError('Duplicate LABEL');
      FOpcode := '';
      Exit;
    end;
  end;
  s := FetchOpcode(FLine.Text);
  if (s = 'EQU') or (s = '.EQU') then
  begin
    RequireSpace(FLine.Text);
    p := Pos(';', FLine.Text);
    if p > 0 then FLine.Text := Trim(Copy(FLine.Text, 1, p - 1));
    FSymbols.AddConst(FOpcode, FLine.Text).FTaked := FPass = 3;
    FLine.Text := '';
    Result := True;
  end
  else if s = 'CLASS' then
  begin
    RequireSpace(FLine.Text, False);
    done := RequireChar(FLine.Text, '{');
    if done then
    begin
      cls := FSymbols.AddClass(FOpcode);
      cls.FTaked := FPass = 3;
      FLine := FSources.FetchLine;
      offs := 0;
      while (FLine.Text <> '}') and (FLine.LineIndex >= 0) and FRunning do
      begin
        ci := FetchSymbolClassItem(FLine.Text);
        ci.FIndex := offs;
        Inc(offs, ci.FTotalSize);
        cls.AddItem(ci);
        if FLine.Text <> '' then AddError('Syntax error: "' + FLine.Text + '"');
        FLine := FSources.FetchLine;
      end;
      FLine.Text := '';
      Result := True;
    end;
  end
  else if s = 'OBJECT' then
  begin
    RequireSpace(FLine.Text, False);
    s := FetchOpcode(FLine.Text);
    sym := FSymbols.Symbol[s];
    if sym = nil then
      AddError(Format('Symbol "%s" is not defined', [s]))
    else
    begin
      if not (sym is TSymbolClass) then
        AddError(Format('Symbol "%s" is not CLASS', [s]))
      else
      begin
        cls := TSymbolClass(sym);
        Inc(cls.FUsed);
        with FSymbols.AddLabel(FOpCode, FAddress) do
        begin
          FTaked := FPass = 3;
          FClassName := s;
        end;
        AddText('');
        AddText(Format('%-12s   OBJECT  %s', [FOpCode + ':', s]));
        for nf := 0 to cls.Count - 1 do
        begin
          ci := cls[nf];
          AddCodes(ci.VBytes);
          case ci.FSize of
            0: s := 'DS';
            2: s := 'DW';
            4: s := 'DD';
            8: s := 'DQ';
          else
            s := 'DB';
          end;
          if ci.FName <> '' then
            c := ci.FName + ':'
          else c := '';
          s := Format('%-12s   %-4s    ', [c, s]);
          md := VarArrayHighBound(ci.FValue, 1);
          for nd := 0 to md do
          begin
            if nd > 0 then s := s + ', ';
            v := ci.FValue[nd];
            case ci.FSize of
              0: s := s + Format('%4.4Xh, %2.2Xh', [Word(v[0]), Byte(v[1])]);
              2: s := s + Format('%4.4Xh', [Word(v)]);
              4: s := s + Format('%8.8Xh', [Cardinal(v)]);
              8: s := s + Format('%16.16Xh', [UInt64(v)]);
            else
              s := s + Format('%2.2Xh', [Byte(v)]);
            end;
          end;
          AddCodeText(s);
          SetLength(FCodes, 0);
        end;
        FLine.Text := '';
        Result := True;
      end;
    end;
    FOpCode := '';
    FLine.Text := '';
  end
  else
  begin
    FSymbols.AddLabel(FOpcode, FAddress).FTaked := FPass = 3;
    AddText('');
    AddText(FOpCode + ':');
    FLabeled := True;
    if (not have) and (s = '') then AddError('Missing opcode');
    FOpcode := s;
  end;
end;

procedure TXPCompiler.CompilePass(APass: Integer);
begin
  AddReport('Compile pass ' + IntToStr(APass));
  AddReport('==============');
  FPass := APass;
  FAddress := DefaultAddress;
  FStartAddress := DefaultAddress;
  FHaveCode := False;
  FRunning := True;
  FDebug.Clear;
  FOutputExtCount := 0;
  FSources.Reset;
  FLine := FSources.FetchLine;
  while (FLine.LineIndex >= 0) and FRunning do
  begin
    FLabeled := False;
    FOpcode := FetchOpcode(FLine.Text);
    SetLength(FCodes, 0);
    while FOpcode <> '' do
    begin
      if CompileCode0 then Break;
      if CompileCode1 then Break;
      if CompileCode2 then Break;
      if CompileCode3 then Break;
      if CompileCode9 then Break;
    end;
    FLine := FSources.FetchLine;
  end;
  AddReport('');
  if not FHaveCode then FStartAddress := FAddress;
end;

constructor TXPCompiler.Create;
begin
  SetLength(FMemory, 65536);
  FDebug := TXPDebug.Create;
  FOutput := nil;
  FExpStack := NULL;
  FAddress := DefaultAddress;
  FStartAddress := DefaultAddress;
  FHaveCode := False;
  FErrorCount := 0;
  FWarningCount := 0;
  FLine.Clear;
  FPass := 1;
  UseHeader := False;
  FSymbols := TSymbolList.Create;
  FBuffers := TStringList.Create;
  FWarnings := TStringList.Create;
  FErrors := TStringList.Create;
  FSources := TSourceList.Create;
  FOutputExt := 'bin';

  FOpcodes1 := TSymbolList.Create;
  FOpcodes1.Add('NOP').Value := $00;
  FOpcodes1.Add('RLCA').Value := $07;
  FOpcodes1.Add('RRCA').Value := $0F;
  FOpcodes1.Add('RLA').Value := $17;
  FOpcodes1.Add('RRA').Value := $1F;
  FOpcodes1.Add('DAA').Value := $27;
  FOpcodes1.Add('CPL').Value := $2F;
  FOpcodes1.Add('SCF').Value := $37;
  FOpcodes1.Add('CCF').Value := $3F;
  FOpcodes1.Add('HALT').Value := $76;
  FOpcodes1.Add('RET').Value := $C9;
  FOpcodes1.Add('EXX').Value := $D9;
  FOpcodes1.Add('DI').Value := $F3;
  FOpcodes1.Add('EI').Value := $FB;

  FOpcodes2 := TSymbolList.Create;
  FOpcodes2.Add('NEG').Value := $ED44;
  FOpcodes2.Add('RETN').Value := $ED45;
  FOpcodes2.Add('RETI').Value := $ED4D;
  FOpcodes2.Add('RRD').Value := $ED67;
  FOpcodes2.Add('RLD').Value := $ED6F;
  FOpcodes2.Add('LDI').Value := $EDA0;
  FOpcodes2.Add('CPI').Value := $EDA1;
  FOpcodes2.Add('INI').Value := $EDA2;
  FOpcodes2.Add('OUTI').Value := $EDA3;
  FOpcodes2.Add('LDD').Value := $EDA8;
  FOpcodes2.Add('CPD').Value := $EDA9;
  FOpcodes2.Add('IND').Value := $EDAA;
  FOpcodes2.Add('OUTD').Value := $EDAB;
  FOpcodes2.Add('LDIR').Value := $EDB0;
  FOpcodes2.Add('CPIR').Value := $EDB1;
  FOpcodes2.Add('INIR').Value := $EDB2;
  FOpcodes2.Add('OTIR').Value := $EDB3;
  FOpcodes2.Add('LDDR').Value := $EDB8;
  FOpcodes2.Add('CPDR').Value := $EDB9;
  FOpcodes2.Add('INDR').Value := $EDBA;
  FOpcodes2.Add('OTDR').Value := $EDBB;

  FRegisterCode := TSymbolList.Create;
  FRegisterCode.Add('B').Value := 0;
  FRegisterCode.Add('C').Value := 1;
  FRegisterCode.Add('D').Value := 2;
  FRegisterCode.Add('E').Value := 3;
  FRegisterCode.Add('H').Value := 4;
  FRegisterCode.Add('L').Value := 5;
  FRegisterCode.Add('(HL)').Value := 6;
  FRegisterCode.Add('A').Value := 7;
  FRegisterCode.Add('BC').Value := 8;
  FRegisterCode.Add('DE').Value := 9;
  FRegisterCode.Add('HL').Value := 10;
  FRegisterCode.Add('SP').Value := 11;
  FRegisterCode.Add('IX').Value := 0;
  FRegisterCode.Add('IY').Value := 0;
  FRegisterCode.Add('AF').Value := 0;
  FRegisterCode.Add('AF''').Value := 0;
  FRegisterCode.Add('(IX)').Value := 0;
  FRegisterCode.Add('(IY)').Value := 0;
  FRegisterCode.Add('IXH').Value := 0;
  FRegisterCode.Add('IXL').Value := 0;
  FRegisterCode.Add('IYH').Value := 0;
  FRegisterCode.Add('IYL').Value := 0;
end;

procedure TXPCompiler.Decompile(AFileName: string; AOutput: TStrings);
var
  fs: TFileStream;
  n: Integer;
  ext: string;
  buf: array[0..6] of Byte;
begin
  UseHeader := False;
  FillChar(FMemory[0], 65536, 0);
  fs := TFileStream.Create(AFileName, fmOpenRead);
  try
    FEndAddress := $FFFB;
    ext := UpperCase(ExtractFileExt(AFileName));
    if ext = '.COM' then
    begin
      FStartAddress := $100;
      FRunAddress := $100;
      n := 0;
    end
    else
    begin
      FStartAddress := 0;
      FRunAddress := 0;
      Initialize(buf);
      n := fs.Read(buf[0], 7);
      if (n > 6) and (buf[0] = $FE) then
      begin
        FStartAddress := buf[1] or (buf[2] shl 8);
        FEndAddress := (buf[3] or (buf[4] shl 8));
        FRunAddress := buf[5] or (buf[6] shl 8);
        UseHeader := True;
        n := 0;
      end
      else Move(buf[0], FMemory[FStartAddress], n);
    end;
    Inc(n, fs.Read(FMemory[FStartAddress + n], FEndAddress - FStartAddress - n + 1));
    FEndAddress := FStartAddress + n - 1;
  finally
    fs.Free;
  end;
  Decompile(AOutput, FStartAddress, FEndAddress, FRunAddress);
end;

procedure TXPCompiler.Decompile(AOutput: TStrings; AStartAddress, AEndAddress, ARunAddress: Integer);
begin
  FStartAddress := AStartAddress;
  FEndAddress := AEndAddress;
  FRunAddress := ARunAddress;
  Decompile(AOutput);
end;

procedure TXPCompiler.Decompile(AOutput: TStrings);
begin
  FOutput := AOutput;
  FOutput.Clear;
  FSymbols.Clear;
  FSymbols.AddLabel('PROG_START', FStartAddress);
  FSymbols.AddLabel('PROG_END', FEndAddress + 1);
  FSymbols.AddLabel('PROG_RUN', FRunAddress);
  DecompilePass(1);
  DecompilePass(2);
end;

procedure TXPCompiler.DecompilePass(APass: Integer);
const
  REG16: array[0..3] of string = ('BC', 'DE', 'HL', 'SP');
  REG8: array[0..7] of string = ('B', 'C', 'D', 'E', 'H', 'L', '(HL)', 'A');
  OPCODE1: array[0..7] of string = ('RLCA', 'RRCA', 'RLA', 'RRA', 'DAA', 'CPL', 'SCF', 'CCF');
  OPCODE2A: array[0..7] of string = ('ADD', 'ADC', 'SUB', 'SBC', 'AND', 'XOR', 'OR', 'CP');
  OPCODE2B: array[0..7] of string = ('A,', 'A,', '', 'A,', '', '', '', '');
  BITOP: array[0..7] of string = ('RLC', 'RRC', 'RL', 'RR', 'SLA', 'SRA', 'SLL', 'SRL');
  PARAM1: array[0..7] of string = ('NZ', 'Z', 'NC', 'C', 'PO', 'PE', 'P', 'M');
var
  idx: Integer;
  b: Byte;
begin
  FBuffers.Clear;
  FPass := APass;
  FAddress := FStartAddress;
  FPreAddress := FStartAddress;
  idx := 0;
  AddDecodeConst;
  if UseHeader then
  begin
    AddDecodeText('#outext', 'bin', '');
    AddDecodeText('');
    AddDecodeText('ORG', Format('$%4.4X', [FStartAddress - 7]));
    AddDecodeText('');
    AddDecodeText('; BSAVE Header', '', '');
    AddDecodeText('DB', '$FE');
    AddDecodeText('DW', 'PROG_START');
    AddDecodeText('DW', 'PROG_END - 1');
    AddDecodeText('DW', 'PROG_RUN');
  end
  else
  begin
    AddDecodeText('#outext', 'com', '');
    AddDecodeText('');
    AddDecodeText('ORG', Format('$%4.4X', [FStartAddress]));
  end;
  while FAddress < FEndAddress + 1 do
  begin
    FIndexOffset := 256;
    FIndexReg := idx;
    FPreIndex := idx;
    idx := 0;
    b := GetByte;
    case b of
      $00: AddDecodeText('NOP');
      $01, $11, $21, $31: AddDecodeText('LD', GetReg16A((b shr 4) and 3) + ',' + GetWordStr);
      $02, $12: AddDecodeText('LD', GetReg16M((b shr 4) and 3) + ',A');
      $22: AddDecodeText('LD', GetMemStr + ',' + GetReg16A(2));
      $32: AddDecodeText('LD', GetMemStr + ',A');
      $03, $13, $23, $33: AddDecodeText('INC', GetReg16A((b shr 4) and 3));
      $04, $0C, $14, $1C, $24, $2C, $34, $3C: AddDecodeText('INC', GetReg8((b shr 3) and 7));
      $05, $0D, $15, $1D, $25, $2D, $35, $3D: AddDecodeText('DEC', GetReg8((b shr 3) and 7));
      $06, $0E, $16, $1E, $26, $2E, $36, $3E: AddDecodeText('LD', GetReg8((b shr 3) and 7) + ',' + GetByteStr);
      $07, $0F, $17, $1F, $27, $2F, $37, $3F: AddDecodeText(OPCODE1[(b shr 3) and 7]);
      $08: AddDecodeText('EX', 'AF,AF''');
      $09, $19, $29, $39: AddDecodeText('ADD', GetReg16A(2) + ',' + GetReg16A((b shr 4) and 3));
      $0A, $1A: AddDecodeText('LD', 'A,(' + GetReg16A((b shr 4) and 3) + ')');
      $2A: AddDecodeText('LD', GetReg16A(2) + ',' + GetMemStr);
      $3A: AddDecodeText('LD', 'A,' + GetMemStr);
      $0B, $1B, $2B, $3B: AddDecodeText('DEC', GetReg16A((b shr 4) and 3));
      $10: AddDecodeText('DJNZ', GetShortAddrStr);
      $18: AddDecodeText('JR', GetShortAddrStr);
      $20, $28, $30, $38: AddDecodeText('JR', PARAM1[(b shr 3) and 3] + ',' + GetShortAddrStr);
      $76: AddDecodeText('HALT');
      $70..$75, $77: AddDecodeText('LD', GetReg8(6) + ',' + REG8[b and 7]);
      $40..$65, $67..$6D, $6F, $78..$7F: AddDecodeText('LD', GetReg8((b shr 3) and 7) + ',' + GetReg8(b and 7));
      $66, $6E: AddDecodeText('LD', REG8[(b shr 3) and 7] + ',' + GetReg8(b and 7));
      $80..$BF: AddDecodeText(OPCODE2A[(b shr 3) and 7], OPCODE2B[(b shr 3) and 7] + GetReg8(b and 7));
      $C0, $C8, $D0, $D8, $E0, $E8, $F0, $F8: AddDecodeText('RET', PARAM1[(b shr 3) and 7]);
      $C1, $D1, $E1, $F1: AddDecodeText('POP', GetReg16B((b shr 4) and 3));
      $C2, $CA, $D2, $DA, $E2, $EA, $F2, $FA: AddDecodeText('JP', PARAM1[(b shr 3) and 7] + ',' + GetAddrStr);
      $C3: AddDecodeText('JP', GetAddrStr);
      $C4, $CC, $D4, $DC, $E4, $EC, $F4, $FC: AddDecodeText('CALL', PARAM1[(b shr 3) and 7] + ',' + GetAddrStr);
      $C5, $D5, $E5, $F5: AddDecodeText('PUSH', GetReg16B((b shr 4) and 3));
      $C6, $CE, $D6, $DE, $E6, $EE, $F6, $FE: AddDecodeText(OPCODE2A[(b shr 3) and 7], OPCODE2B[(b shr 3) and 7] + GetByteStr);
      $C7, $CF, $D7, $DF, $E7, $EF, $F7, $FF: AddDecodeText('RST', Format('$%2.2X', [b and $38]));
      $C9: AddDecodeText('RET');
      $D3: AddDecodeText('OUT', '(' + GetByteStr + '),A');
      $DB: AddDecodeText('IN', 'A,(' + GetByteStr + ')');
      $D9: AddDecodeText('EXX');
      $E3: AddDecodeText('EX', '(SP),HL');
      $EB: AddDecodeText('EX', 'DE,HL');
      $F3: AddDecodeText('DI');
      $FB: AddDecodeText('EI');
      $E9: AddDecodeText('JP', '(HL)');
      $F9: AddDecodeText('LD', 'SP,HL');
      $CD: AddDecodeText('CALL', GetAddrStr);
      $CB:
      begin
        FetchIndexOffset;
        b := GetByte;
        case b of
          $00..$3F: AddDecodeText(BITOP[(b shr 3) and 7], GetReg8(b and 7));
          $40..$7F: AddDecodeText('BIT', IntToStr((b shr 3) and 7) + ',' + GetReg8(b and 7));
          $80..$BF: AddDecodeText('RES', IntToStr((b shr 3) and 7) + ',' + GetReg8(b and 7));
          $C0..$FF: AddDecodeText('SET', IntToStr((b shr 3) and 7) + ',' + GetReg8(b and 7));
        end;
      end;
      $DD, $FD:
      begin
        if FIndexReg = 0 then
          idx := b
        else AddDecodeText('DB', Format('$%2.2X', [b]));
      end;
      $ED:
      begin
        if FIndexReg > 0 then
          AddDecodeText('DB', Format('$%2.2X', [b]))
        else
        begin
          b := GetByte;
          case b of
            $40, $48, $50, $58, $60, $68, $70, $78: AddDecodeText('IN', REG8[(b shr 3) and 7] + ',(C)');
            $41, $49, $51, $59, $61, $69, $71, $79: AddDecodeText('OUT', '(C),' + REG8[(b shr 3) and 7]);
            $42, $52, $62, $72: AddDecodeText('SBC', 'HL,' + REG16[(b shr 4) and 3]);
            $43, $53, $73: AddDecodeText('LD', GetMemStr + ',' + REG16[(b shr 4) and 3]);
            $63: AddDecodeText('LD', GetMemStr + ',HL ; ED 63 XX XX');
            $44, $4C, $54, $5C, $64, $6C, $74, $7C: AddDecodeText('NEG ;ED ' + Format('%2.2d', [b]));
            $45, $55, $5D, $65, $6D, $75: AddDecodeText('RETN ; ED ' + Format('%2.2d', [b]));
            $46: AddDecodeText('IM', '0');
            $56: AddDecodeText('IM', '1');
            $5E: AddDecodeText('IM', '2');
            $47: AddDecodeText('LD', 'I,A');
            $4F: AddDecodeText('LD', 'R,A');
            $57: AddDecodeText('LD', 'A,I');
            $5F: AddDecodeText('LD', 'A,R');
            $4A: AddDecodeText('ADC', 'HL,' + REG16[(b shr 4) and 3]);
            $4B, $5B, $7B: AddDecodeText('LD', REG16[(b shr 4) and 3] + ',' + GetMemStr);
            $6B: AddDecodeText('LD', 'HL,' + GetMemStr + ' ; ED 6B XX XX');
            $4D, $7D: AddDecodeText('RETI ; ED ' + Format('%2.2d', [b]));
            $67: AddDecodeText('RRD');
            $6F: AddDecodeText('RLD');
            $A0: AddDecodeText('LDI');
            $A1: AddDecodeText('CPI');
            $A2: AddDecodeText('INI');
            $A3: AddDecodeText('OUTI');
            $A8: AddDecodeText('LDD');
            $A9: AddDecodeText('CPD');
            $AA: AddDecodeText('IND');
            $AB: AddDecodeText('OUTD');
            $B0: AddDecodeText('LDIR');
            $B1: AddDecodeText('CPIR');
            $B2: AddDecodeText('INIR');
            $B3: AddDecodeText('OTIR');
            $B8: AddDecodeText('LDDR');
            $B9: AddDecodeText('CPDR');
            $BA: AddDecodeText('INDR');
            $BB: AddDecodeText('OTDR');
          else
            AddDecodeText('DB', Format('$%2.2X,$%2.2X', [$ED, b]));
          end;
        end;
      end;
    end;
  end;
  FPreAddress := FAddress;
  Inc(FAddress);
  AddDecodeLabel;
  Dec(FAddress);
  FlushBuffers(True);
end;

destructor TXPCompiler.Destroy;
begin
  FBuffers.Free;
  FWarnings.Free;
  FErrors.Free;
  FSymbols.Free;
  FOpcodes1.Free;
  FOpcodes2.Free;
  FRegisterCode.Free;
  FDebug.Free;
  FSources.Free;
  SetLength(FMemory, 0);
  SetLength(FCodes, 0);
  inherited Destroy;
end;

procedure TXPCompiler.FetchShiftOp(AOffset: Byte);
var
  reg: TZ80Register;
begin
  reg := FetchRegister(FLine.Text);
  if reg.Size <> 1 then
    AddError('Bad register: ' + reg.Text)
  else
  begin
    if reg.IsIndexReg then AddCode(reg.PreCode);
    AddCode($CB);
    if reg.HaveIndex then AddCode(reg.Index);
    AddCode(AOffset + reg.Offset);
    AddCodeText(Format('%-6.6s%s', [FOpCode, reg.Text]));
    RequireNewLine(FLine.Text);
  end;
end;

procedure TXPCompiler.FetchBitOp(AOffset: Byte);
var
  reg: TZ80Register;
  b: Byte;
begin
  RequireSpace(FLine.Text);
  b := FetchByte(FLine.Text);
  if b > 7 then
    AddError('Bit offset out of range: ' + IntToStr(b))
  else
  begin
    RequireChar(FLine.Text, ',');
    reg := FetchRegister(FLine.Text);
    if reg.IsWord then
      AddError('Bad register: ' + reg.Text)
    else
    begin
      if reg.IsIndexReg then AddCode(reg.PreCode);
      AddCodes([$CB]);
      if reg.HaveIndex then AddCode(reg.Index);
      AddCodes([AOffset + (b shl 3) + reg.Offset]);
      AddCodeText(Format('%-6.6s%d,%s', [FOpCode, b, reg.Text]));
      RequireNewLine(FLine.Text);
    end;
  end;
end;

function TXPCompiler.FetchByte(var AText: string): Integer;
begin
  Result := FetchInt64(AText) and $FF;
end;

function TXPCompiler.FetchCondition(var AText: string): Integer;
const
  ConStr = '[NZ][Z] [NC][C] [PO][PE][P] [M] ';
var
  p, q: Integer;
begin
  Result := -1;
  q := Pos(',', AText);
  if q = 0 then q := Length(AText) + 1;
  FCondition := UpperCase(Copy(AText, 1, q - 1));
  p := Pos('[' + FCondition + ']', ConStr);
  if p > 0 then
  begin
    Result := ((p - 1) shr 2) shl 3;
    Delete(AText, 1, q);
  end;
end;

function TXPCompiler.FetchDWord(var AText: string): Integer;
begin
  Result := FetchInt64(AText) and $FFFFFFFF;
end;

function TXPCompiler.FetchExp(var AText: string): string;
var
  done: Boolean;
  c: Char;
begin
  Result := '';
  done := False;
  AText := TrimLeft(AText);
  while not done and (AText <> '') do
  begin
    c := AText[1];
    case c of
      ',', ';': done := True;
    else
      Result := Result + c;
      Delete(AText, 1, 1);
    end;
  end;
end;

function TXPCompiler.FetchFunction(var AText, AName: string): Boolean;
const
  FuncStr = '000[SIZE]';
var
  sym: TSymbolObject;
  p: Integer;
  s: string;
begin
  Result := False;
  p := Pos('[' + AName + ']', FuncStr);
  if p <= 0 then Exit;
  p := StrToIntDef(Copy(FuncStr, p - 3, 3), -1);
  RequireSpace(AText, False);
  RequireChar(AText, '(');
  case p of
    0: // SIZE
    begin
      s := FetchOpcode(AText);
      sym := FSymbols[s];
      if sym = nil then
        AddError('Symbol "' + s + '" not defined')
      else if sym.FClassName <> '' then
      begin
        sym := FSymbols[sym.FClassName];
        AName := IntToStr(TSymbolClass(sym).Size);
      end
      else if sym is TSymbolClass then
      begin
        AName := IntToStr(TSymbolClass(sym).Size);
      end
      else AddError('Can not get size from "' + s + '"');
    end;
  else
    AddError('No function "' + AName + '"');
  end;
  RequireChar(AText, ')');
  Result := True;
end;

procedure TXPCompiler.FetchIndexOffset;
begin
  if FIndexReg > 0 then FIndexOffset := GetByte;
end;

function TXPCompiler.FetchInt64(var AText: string): Int64;
var
  sl: TStringList;
  v: Variant;
begin
  sl := TStringList.Create;
  try
    FetchPostfixExp(AText, sl);
    v := PostfixToInt64(sl);
    if VarType(v) = varInt64 then
      Result := v
    else
    begin
      Result := 0;
      AddError('Expression error');
    end;
  finally
    sl.Free;
  end;
end;

function TXPCompiler.FetchLabel(var AText: string): string;
var
  done: Boolean;
  c: Char;
begin
  Result := '';
  done := False;
  while (AText <> '') and (not done) do
  begin
    c := AText[1];
    if ((UpCase(c) >= 'A') and (UpCase(c) <= 'Z')) or (c = '_') or (c = '.') or (c = '@') then
      Result := Result + c
    else if (Result <> '') and (c >= '0') and (c <= '9') then
      Result := Result + c
    else done := True;
    if not done then Delete(AText, 1, 1);
  end;
end;

function TXPCompiler.FetchLine(var AText: string): string;
var
  done: Boolean;
  c: Char;
begin
  Result := '';
  done := False;
  while (AText <> '') and (not done) do
  begin
    c := AText[1];
    if c <> ';' then
    begin
      Result := Result + c;
      Delete(AText, 1, 1);
    end
    else done := True;
  end;
end;

function TXPCompiler.FetchNumber(var AText: string): string;
var
  done: Boolean;
  c: Char;
begin
  Result := '';
  done := False;
  while (AText <> '') and (not done) do
  begin
    c := AText[1];
    if ((UpCase(c) >= 'A') and (UpCase(c) <= 'Z'))
      or (c = '_') or (c = '.') or (c = '@') or (c = '$')
      or ((c >= '0') and (c <= '9')) then
      Result := Result + c
    else done := True;
    if not done then Delete(AText, 1, 1);
  end;
end;

function TXPCompiler.FetchOpcode(var AText: string): string;
var
  done: Boolean;
  c: Char;
begin
  Result := '';
  done := False;
  AText := TrimLeft(AText);
  repeat
    if Length(AText) < 1 then
      done := True
    else
    begin
      c := UpCase(AText[1]);
      if ((c >= 'A') and (c <= 'Z')) or ((c >= '0') and (c <= '9')) or (c = '_') or (c = '@')
        or (c = '#') or (c = '.') then
      begin
        Result := Result + c;
        Delete(AText, 1, 1);
      end
      else done := True;
    end;
  until done;
end;

function OpNumber(op: string): Integer;
begin
  Result := Pos('[' + op + ']', ActiveOperators);
  if Result > 0 then Result := StrToIntDef(Copy(ActiveOperators, Result + Length(op) + 2, 2), 0);
end;

function OpDegree(op: string): Integer;
begin
  Result := Pos('[' + op + ']', ActiveOperators);
  if Result > 0 then Result := StrToIntDef(Copy(ActiveOperators, Result + Length(op) + 5, 2), 0);
end;

procedure TXPCompiler.FetchPostfixExp(var AText: string; AList: TStrings);
var
  sym: TSymbolObject;
  cls: TSymbolClass;
  ci: TSymbolClassItem;
  opstack: string;
  done: Boolean;
  s, sf: string;
  c: Char;
  p: Integer;
begin
  done := False;
  opstack := '';
  AList.Clear;
  AText := Trim(AText);
  FExpression := AText;
  while (AText <> '') and not done do
  begin
    c := AText[1];
    if OpDegree(c) > 0 then
    begin
      s := Copy(opstack, 1, 1);
      while (s <> '') and (s <> '(') do
      begin
        if OpDegree(s[1]) >= OpDegree(c) then
        begin
	        AList.Add(s);
          Delete(opstack, 1, 1);
          s := Copy(opstack, 1, 1);
        end
        else s := '';
      end;
      opstack := c + opstack;
      Delete(AText, 1, 1);
    end
    else if c = '(' then
    begin
      opstack := '(' + opstack;
      Delete(AText, 1, 1);
    end
    else if c = ')' then
    begin
      s := Copy(opstack, 1, 1);
      while (s <> '') and (s <> '(') do
      begin
        AList.Add(s);
        Delete(opstack, 1, 1);
        s := Copy(opstack, 1, 1);
      end;
      if opstack <> '' then Delete(opstack, 1, 1);
      Delete(AText, 1, 1);
    end
    else
    begin
      if ((c >= '0') and (c <= '9')) or (c = '$') then
      begin
        s := FetchNumber(AText);
        AList.Add(s);
      end
      else
      begin
        s := FetchLabel(AText);
        if s <> '' then
        begin
//          if not FetchFunction(AText, s) and (FPass = 2) then
          if not FetchFunction(AText, s) and (FPass = 3) then
          begin
            p := Pos('.', s);
            if p > 0 then
            begin
              sf := Copy(s, p + 1, Length(s) - p);
              s := Copy(s, 1, p - 1);
            end
            else sf := '';
            sym := FSymbols[s];
            if sym = nil then
            begin
              AddError('Symbol [' + s + '] not defined');
              s := '';
            end
            else
            begin
              if sf <> '' then
              begin
                if sym.FClassName = '' then
                begin
                  if not (sym is TSymbolClass) then
                  begin
                    AddError('Symbol [' + s + '] not an OBJECT');
                    s := '';
                  end
                  else
                  begin
                    cls := TSymbolClass(sym);
                    ci := cls[sf];
                    if ci = nil then
                    begin
                      AddError(Format('No member "%s" in CLASS "%s"', [sf, s]));
                      s := '';
                    end
                    else
                    begin
                      s := IntToStr(ci.FIndex);
                      cls.FUsed := cls.FUsed + 1;
                    end;
                  end;
                end
                else
                begin
                  cls := TSymbolClass(FSymbols[sym.FClassName]);
                  ci := cls[sf];
                  if ci = nil then
                  begin
                    AddError(Format('No member "%s" in CLASS "%s"', [sf, s]));
                    s := '';
                  end
                  else
                  begin
                    s := IntToStr(Word(sym.Value) + ci.FIndex);
                    sym.FUsed := sym.FUsed + 1;
                  end;
                end;
              end
              else
              begin
                s := VarToStrDef(sym.Value, '');
                sym.FUsed := sym.FUsed + 1;
              end;
            end;
          end;
          AList.Add(s);
        end
        else done := True;
      end;
    end;
    AText := Trim(AText);
  end;
  while opstack <> '' do
  begin
    AList.Add(Copy(opstack, 1, 1));
    Delete(opstack, 1, 1);
  end;
end;

function TXPCompiler.FetchQWord(var AText: string): Int64;
begin
  Result := FetchInt64(AText);
end;

function TXPCompiler.FetchRegister(var AText: string): TZ80Register;
const
  RegStr = ' B:000100 C:000101 D:000102 E:000103 H:000104 L:000105'
    + ' (HL):000106 A:000107 BC:010200 DE:010201 HL:010202 SP:010203'
    + ' AF:020203 IX:DD0202 IY:FD0202 I:030100 R:030101'
    + ' IXH:DD0104 IXL:DD0105 IYH:FD0104 IYL:FD0105';
  IndexStr = ' IX:DD IY:FD';
var
  p, d: Integer;
  s, st, sign: string;
begin
  Result.RegType := rgt8;
  Result.Index := 0;
  Result.Offset := 0;
  Result.Text := UpperCase(Trim(FetchExp(AText)));
  p := Pos(' ' + Result.Text + ':', RegStr);
  if p > 0 then
  begin
    s := Copy(RegStr, p + Length(Result.Text) + 2, 6);
    case StrToInt('$' + Copy(s, 1, 2)) of
      0: Result.RegType := rgt8;
      1: Result.RegType := rgt16a;
      2: Result.RegType := rgt16b;
      3: Result.RegType := rgtSys;
      $DD: Result.RegType := rgtIX;
      $FD: Result.RegType := rgtIY;
    end;
    Result.Size := StrToInt('$' + Copy(s, 3, 2));
    Result.Offset := StrToInt('$' + Copy(s, 5, 2));
  end
  else
  begin
    s := Trim(Result.Text);
    if Copy(s, 1, 1) = '(' then
    begin
      if Copy(s, Length(s), 1) <> ')' then
        AddError('No close parenthesis')
      else
      begin
        Result.Size := 1;
        s := Trim(Copy(s, 2, Length(s) - 2));
        p := Pos(' ' + Copy(s, 1, 2) + ':', IndexStr);
        st := Trim(Copy(s, 3, Length(s) - 2));
        sign := Copy(st, 1, 1);
        st := Trim(Copy(st, 2, Length(st) - 1));
        if (p > 0) and ((sign = '+') or (sign = '-')) then
        begin
          case StrToIntDef('$' + Copy(IndexStr, p + 4, 2), 0) of
            $DD: Result.RegType := rgtIX;
            $FD: Result.RegType := rgtIY;
          end;
          d := FetchWord(st);
          st := Trim(st);
          if sign = '-' then d := -d;
          if (d > 127) or (d < -128) then AddError('Index out of range');
          if st <> '' then AddError('Bad index');
          Result.Index := d;
          Result.Offset := 6;
        end
        else if s = 'BC' then
        begin
          Result.Offset := 0;
          Result.RegType := rgtRegMem;
        end
        else if s = 'DE' then
        begin
          Result.Offset := 1;
          Result.RegType := rgtRegMem;
        end
        else
        begin
          Result.Value := FetchWord(s);
          Result.RegType := rgtAddr;
        end;
      end;
    end
    else
    begin
      Result.Value := FetchWord(s);
      Result.Offset := Result.Value and 255;
      Result.RegType := rgtValue;
      Result.Size := 1;
    end;
  end;
end;

function TXPCompiler.FetchShortAddress(var AText: string): Integer;
begin
  Result := FetchWord(AText);
  Result := Result - FAddress - 2;
  if (Result < -128) or (Result > 127) then
    AddError('Short address out of range')
end;

function TXPCompiler.FetchString(var AText: string; out ADelim: string): string;
var
  c: string;
begin
  Result := '';
  ADelim := Copy(AText, 1, 1);
  if (ADelim = '"') or (ADelim = '''') then
  begin
    Delete(AText, 1, 1);
    c := Copy(AText, 1, 1);
    while (c <> '') and (c <> ADelim) do
    begin
      Result := Result + c;
      Delete(AText, 1, 1);
      c := Copy(AText, 1, 1);
    end;
    Delete(AText, 1, 1);
  end
  else ADelim := '';
end;

function TXPCompiler.FetchSymbolClassItem(AText: string): TSymbolClassItem;
const
  DataOp = '001[DB]001[DEFB]001[.DB]001[.DEFB]'
    + '001[DM]001[DEFM]001[.DM]001[.DEFM]'
    + '002[DW]002[DEFW]002[.DW]002[.DEFW]'
    + '004[DD]004[DEFD]004[.DD]004[.DEFD]'
    + '008[DQ]008[DEFQ]008[.DQ]008[.DEFQ]'
    + '000[DS]000[DEFS]000[.DS]000[.DEFS]';
var
  n, nd, d, dtype: Integer;
  stype: string;

  function GetDataType(s: string): Integer;
  begin
    Result := Pos('[' + s + ']', DataOp);
    if Result > 0 then
      Result := StrToIntDef(Copy(DataOp, Result - 3, 3), -1)
    else Result := -1;
  end;

begin
  Result := TSymbolClassItem.Create;
  stype := FetchOpcode(FLine.Text);
  dtype := GetDataType(stype);
  if dtype < 0 then
  begin
    Result.FName := stype;
    RequireChar(FLine.Text, ':', False);
    RequireSpace(FLine.Text, False);
    stype := FetchOpCode(FLine.Text);
    dtype := GetDataType(stype);
  end
  else Result.FName := '';
  Result.FValue := VarArrayOf([]);
  RequireSpace(FLine.Text, False);
  if dtype < 0 then
  begin
    AddError('Unknown CLASS member type');
    Exit;
  end;
  Result.FSize := dtype;
  n := 0;
  repeat
    VarArrayReDim(Result.FValue, n);
    case Result.FSize of
      0:
      begin
        nd := FetchWord(FLine.Text);
        if RequireChar(FLine.Text, ',', False) then
          d := FetchByte(FLine.Text)
        else d := 0;
        Result.FValue[n] := VarArrayOf([nd, d]);
        Inc(Result.FTotalSize, nd);
      end;
      1: Result.FValue[n] := FetchByte(FLine.Text);
      2: Result.FValue[n] := FetchWord(FLine.Text);
      4: Result.FValue[n] := FetchDWord(FLine.Text);
      8: Result.FValue[n] := FetchQWord(FLine.Text);
    end;
    Inc(Result.FTotalSize, Result.FSize);
    Inc(n);
  until not RequireChar(FLine.Text, ',', False);
end;

//function TXPCompiler.FetchShortInt(var AText: string): ShortInt;
//begin
//  Result := FetchInt64(AText);
//end;

function TXPCompiler.FetchWord(var AText: string): Integer;
begin
  Result := FetchInt64(AText) and $FFFF;
end;

procedure TXPCompiler.FlushBuffers(AForced: Boolean);
begin
  if AForced or (FBuffers.Count > 1000) then
  begin
    FOutput.AddStrings(FBuffers);
    FBuffers.Clear;
  end;
end;

function TXPCompiler.GetAddrStr: string;
var
  w: Word;
begin
  w := GetWord;
  Result := Format('LABEL_%4.4X', [w]);
  FSymbols.AddLabel(Result, w);
end;

function TXPCompiler.GetByte: Byte;
begin
  Result := FMemory[FAddress];
  Inc(FAddress);
end;

function TXPCompiler.GetByteStr: string;
var
  b: Byte;
begin
  b := GetByte;
  Result := Format('B_%2.2X', [b]);
  FSymbols.AddConst(Result, b);
end;

function TXPCompiler.GetMemStr: string;
begin
  Result := '(' + GetAddrStr + ')';
end;

function TXPCompiler.GetReg16A(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := 'BC';
    1: Result := 'DE';
    3: Result := 'SP';
  else
    case FIndexReg of
      $DD:
      begin
        Result := 'IX';
        FIndexReg := 0;
      end;
      $FD:
      begin
        Result := 'IY';
        FIndexReg := 0;
      end
    else
      Result := 'HL';
    end;
  end;
end;

function TXPCompiler.GetReg16B(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := 'BC';
    1: Result := 'DE';
    3: Result := 'AF';
  else
    case FIndexReg of
      $DD:
      begin
        Result := 'IX';
        FIndexReg := 0;
      end;
      $FD:
      begin
        Result := 'IY';
        FIndexReg := 0;
      end
    else
      Result := 'HL';
    end;
  end;
end;

function TXPCompiler.GetReg16M(AIndex: Integer): string;
var
  s: string;
  b: Byte;
begin
  case AIndex of
    0: Result := '(BC)';
    1: Result := '(DE)';
    3: Result := '(SP)';
  else
    case FIndexReg of
      $DD: s := 'IX';
      $FD: s := 'IY';
    else
      s := 'HL';
    end;
    case FIndexReg of
      $DD, $FD:
      begin
        b := GetByte;
        if b < 128 then
          Result := Format('(IX + %d)', [b])
        else Result := Format('IX - %d)', [256 - b]);
        FIndexReg := 0;
      end;
    end;
    Result := '(' + s + ')';
  end;
end;

function TXPCompiler.GetReg8(AIndex: Integer): string;
var
  s: string;
begin
  case AIndex of
    0: Result := 'B';
    1: Result := 'C';
    2: Result := 'D';
    3: Result := 'E';
    4:
    begin
      case FPreIndex of
        $DD:
        begin
          Result := 'IXH';
          FIndexReg := 0;
        end;
        $FD:
        begin
          Result := 'IYH';
          FIndexReg := 0;
        end
      else
        Result := 'H';
      end;
    end;
    5:
    begin
      case FPreIndex of
        $DD:
        begin
          Result := 'IXL';
          FIndexReg := 0;
        end;
        $FD:
        begin
          Result := 'IYL';
          FIndexReg := 0;
        end
      else
        Result := 'L';
      end;
    end;
    6:
    begin
      case FPreIndex of
        $DD: s := 'IX';
        $FD: s := 'IY';
      else
        s := 'HL';
      end;
      case FPreIndex of
        $DD, $FD:
        begin
          if FIndexOffset > 255 then FIndexOffset := GetByte;
          if FIndexOffset < 128 then
            s := Format('%s + %d', [s, FIndexOffset])
          else s := Format('%s - %d', [s, 256 - FIndexOffset]);
          FIndexReg := 0;
        end;
      end;
      Result := '(' + s + ')';
    end;
  else
    Result := 'A';
  end;
end;

function TXPCompiler.GetShortAddrStr: string;
var
  w: Word;
begin
  w := GetByte;
  if w < 128 then
    w := FAddress + w
  else w := FAddress + w - 256;
  Result := Format('LABEL_%4.4X', [w]);
  FSymbols.AddLabel(Result, w);
end;

function TXPCompiler.GetWord: Word;
begin
  Result := FMemory[FAddress] or (FMemory[FAddress + 1] shl 8);
  Inc(FAddress, 2);
end;

function TXPCompiler.GetWordStr: string;
var
  w: Word;
begin
  w := GetWord;
  Result := Format('W_%4.4X', [w]);
  FSymbols.AddConst(Result, w);
end;

function TXPCompiler.PostfixToInt64(AList: TStrings): Variant;
var
  pl: TPostfixList;
  n: Integer;
  a, b: Int64;
  s: string;
  sl: TStringList;
  va, vb: Variant;
begin
  pl := TPostfixList.Create;
  try
    while AList.Count > 0 do
    begin
      s := AList[0];
      AList.Delete(0);
      n := OpNumber(s);
      if n > 0 then
      begin
        vb := pl.Pop;
        va := pl.Pop;
        if VarType(va) = varInt64 then
          a := va
        else a := 0;
        if VarType(vb) = varInt64 then
          b := vb
        else b := 0;
        case n of
          1: a := a * b;
          2: if (b <> 0) then a := a div b;
          3: a := a + b;
          4: a := a - b;
          5: a := a and b;
          6: a := a xor b;
          7: a := a or b;
        else
          AddError('Bad operator: ' + s);
        end;
        pl.PushValue(a);
      end
      else
      begin
        sl := TStringList.Create;
        try
          FetchPostfixExp(s, sl);
          case sl.Count of
            0: pl.PushValue(0);
            1: pl.PushValue(StrNumToInt64(sl[0]));
          else
            va := PostfixToInt64(sl);
            if VarType(va) <> varInt64 then va := 0;
            pl.PushValue(va);
          end;
        finally
          sl.Free;
        end;
      end;
    end;
    Result := pl.Pop;
  finally
    pl.Free;
  end;
end;

procedure TXPCompiler.FetchRegOp8(AOffset, AShift, AValueCode: Byte);
var
  reg: TZ80Register;
begin
  reg := FetchRegister(FLine.Text);
  if reg.IsIndexReg then AddCode(reg.PreCode);
  if reg.IsByte then
  begin
    if reg.RegType = rgtValue then
      AddCodes([AValueCode, reg.Offset])
    else
    begin
      AddCode(AOffset + (reg.Offset shl AShift));
      if reg.HaveIndex then AddCode(reg.Index);
    end;
  end
  else AddError('8 bits register only');
  AddCodeText(Format('%-6.6s%s', [FOpCode, reg.Text]));
  RequireNewLine(FLine.Text);
end;

function TXPCompiler.RequireChar(var AText: string; AChar: Char; Forced: Boolean): Boolean;
var
  s: string;
begin
  AText := TrimLeft(AText);
  s := Copy(AText, 1, 1);
  Result := s = AChar;
  if Result then
    Delete(AText, 1, 1)
  else if Forced then AddError('Must be "' + AChar + '"');
end;

function TXPCompiler.RequireNewLine(var AText: string; Forced: Boolean): Boolean;
begin
  AText := TrimLeft(AText);
  Result := (AText = '') or (Copy(AText, 1, 1) = ';');
  if Result then
  begin
    AText := '';
    FOpcode := '';
  end
  else if Forced then
  begin
    AddError('End of line error "' + AText + '"');
  end;
end;

function TXPCompiler.RequireSpace(var AText: string; Forced: Boolean): Boolean;
var
  s: string;
begin
  s := Copy(AText, 1, 1);
  Result := (s = #9) or (s = ' ');
  AText := TrimLeft(AText);
  if not Result and Forced then AddError('Must be space');
end;

procedure TXPCompiler.SaveToFile(AFileName: string; AFirstAddress,
  ALastAddress: Word);
var
  fs: TFileStream;
begin
  AFileName := ChangeFileExt(AFileName, '.' + FOutputExt);
  fs := TFileStream.Create(AFileName, fmCreate);
  try
    fs.Write(FMemory[AFirstAddress], ALastAddress - AFirstAddress + 1);
  finally
    fs.Free;
  end;
end;

function TXPCompiler.StrBinToInt64(AText: string): Int64;
var
  p: Integer;
  c: Char;
begin
  Result := 0;
  p := 1;
  while (p <= Length(AText)) and ((Result and $8000000000000000) = 0) do
  begin
    Result := Result shl 1;
    c := AText[p];
    Inc(p);
    if c = '1' then
      Result := Result or 1
    else if c <> '0' then
    begin
      AddError('Invalid BINARY value format');
      p := Length(AText) + 1;
    end;
  end;
  if p <= Length(AText) then AddError('BINARY value out of range');
end;

function TXPCompiler.StrDecToInt64(AText: string): Int64;
var
  p, n: Integer;
  c: Char;
begin
  Result := 0;
  p := 1;
//  while (p <= Length(AText)) and (Result < 10000000000000000000) do
  while p <= Length(AText) do
  begin
    c := AText[p];
    Inc(p);
    if (c < '0') or (c > '9') then
    begin
      AddError('Invalid DECIMAL value format');
      p := Length(AText) + 1;
    end
    else
    begin
      n := Ord(c) - Ord('0');
      if (Result > 1844674407370955161) or ((Result = 1844674407370955161) and (n > 5)) then
      begin
        AddError('DECIMAL value out of range');
        p := Length(AText) + 1;
      end
      else
      begin
        Result := Result * 10;
        Result := Result + n
      end
    end;
  end;
  if p <= Length(AText) then AddError('DECIMAL value out of range');
end;

function TXPCompiler.StrHexToInt64(AText: string): Int64;
var
  p: Integer;
  c: Char;
begin
  Result := 0;
  p := 1;
  while (p <= Length(AText)) and ((Result and $F000000000000000) = 0) do
  begin
    Result := Result shl 4;
    c := UpCase(AText[p]);
    Inc(p);
    if (c >= '0') and (c <= '9') then
      Result := Result or (Ord(c) - Ord('0'))
    else if (c >= 'A') and (c <= 'F') then
      Result := Result or (Ord(c) - 55)
    else
    begin
      AddError('Invalid HEXADECIMAL value format');
      p := Length(AText) + 1;
    end;
  end;
  if p <= Length(AText) then AddError('HEXADECIMAL value out of range');
end;

function TXPCompiler.StrNumToInt64(AText: string): Int64;
var
  c: Char;
begin
  Result := 0;
  if AText = '' then Exit;
  c := AText[1];
  if (c >= '0') and (c <= '9') then
  begin
    c := UpCase(AText[Length(AText)]);
    if (c >= '0') and (c <= '9') then
      c := 'D'
    else Delete(AText, Length(AText), 1);
    if AText = '' then
      AddError('Invalid value format')
    else
    case c of
      'D': Result := StrDecToInt64(AText);
      'B': Result := StrBinToInt64(AText);
      'O', 'Q': Result := StrOctToInt64(AText);
      'H': Result := StrHexToInt64(AText);
    else
      AddError('Invalid Number Type');
    end;
  end
  else if c = '$' then
  begin
    Delete(AText, 1, 1);
    Result := StrHexToInt64(AText);
  end
  else Result := StrNumToInt64(FSymbols.ValueString[AText]);
end;

function TXPCompiler.StrOctToInt64(AText: string): Int64;
var
  p: Integer;
  c: Char;
begin
  Result := 0;
  p := 1;
  while (p <= Length(AText)) and ((Result and $8000000000000000) = 0) do
  begin
    if Result > $1FFFFFFFFFFFFFFF then
    begin
      AddError('OCTAL value out of range');
      p := Length(AText) + 1
    end
    else
    begin
      Result := Result shl 3;
      c := AText[p];
      Inc(p);
      if (c >= '0') and (c <= '7') then
        Result := Result or (Ord(c) - Ord('0'))
      else
      begin
        AddError('Invalid OCTAL value format');
        p := Length(AText) + 1;
      end;
    end;
  end;
  if p <= Length(AText) then AddError('OCTAL value out of range');
end;

{ TSymbolObject }

constructor TSymbolObject.Create(AName: string);
begin
  FName := AName;
  FTaked := False;
  FClassName := '';
  FUsed := 0;
  FValue := Unassigned;
end;

function TSymbolObject.GetValue: Variant;
begin
  Result := FValue;
end;

function TSymbolObject.GetValueType: TSymbolType;
begin
  Result := sbtNone;
end;

procedure TSymbolObject.SetValue(AValue: Variant);
begin
  FValue := AValue;
end;

{ TSymbolList }

function TSymbolList.Add(AName: string): TSymbolObject;
begin
  Result := GetSymbol(AName);
  if Result = nil then
  begin
    Result := TSymbolObject.Create(AName);
    AddItem(Result);
  end;
end;

function TSymbolList.AddClass(AName: string): TSymbolClass;
begin
  Result := TSymbolClass(GetSymbol(AName));
  if Result = nil then
  begin
    Result := TSymbolClass.Create(AName);
    AddItem(Result);
  end
  else Result.Clear;
end;

function TSymbolList.AddConst(AName: string; AValue: Variant): TSymbolConst;
begin
  Result := TSymbolConst(GetSymbol(AName));
  if Result = nil then
  begin
    Result := TSymbolConst.Create(AName);
    AddItem(Result);
  end;
  Result.FValue := AValue;
end;

function TSymbolList.AddItem(AItem: TSymbolObject): Integer;
begin
  Result := FList.Add(AItem);
end;

function TSymbolList.AddLabel(AName: string; AValue: Variant): TSymbolLabel;
begin
  Result := TSymbolLabel(GetSymbol(AName));
  if Result = nil then
  begin
    Result := TSymbolLabel.Create(AName);
    AddItem(Result);
  end;
  Result.FValue := AValue;
end;

procedure TSymbolList.Clear;
begin
  FList.Clear;
end;

function TSymbolList.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TSymbolList.Create;
begin
  FList := TList.Create;
end;

destructor TSymbolList.Destroy;
var
  sym: TSymbolObject;
  n: Integer;
begin
  n := FList.Count;
  while n > 0 do
  begin
    Dec(n);
    sym := FList[n];
    FList[n] := nil;
    sym.Free;
  end;
  FreeAndNil(FList);
  inherited Destroy;
end;

function TSymbolList.GetValueString(AName: string): string;
begin
  Result := VarToStrDef(Value[AName], '');
end;

function TSymbolList.GetSymbol(AName: string): TSymbolObject;
var
  n: Integer;
begin
  Result := nil;
  n := FList.Count;
  while (n > 0) and (Result = nil) do
  begin
    Dec(n);
    if SameText(TSymbolObject(FList[n]).Name, AName) then Result := FList[n];
  end;
end;

function TSymbolList.GetSymbolByIndex(AIndex: Integer): TSymbolObject;
begin
  Result := TSymbolObject(FList[AIndex]);
end;

function TSymbolList.GetValue(AName: string): Variant;
var
  so: TSymbolObject;
begin
  so := GetSymbol(AName);
  if so <> nil then
    Result := so.Value
  else Result := NULL;
end;

function TSymbolList.GetValueType(AName: string): TSymbolType;
var
  so: TSymbolObject;
begin
  so := GetSymbol(AName);
  if so <> nil then
    Result := so.ValueType
  else Result := sbtNone;
end;

function TSymbolList.IndexOf(AName: string): Integer;
var
  n: Integer;
begin
  Result := -1;
  n := FList.Count;
  while (n > 0) and (Result < 0) do
  begin
    Dec(n);
    if TSymbolObject(FList[n]).Name = AName then Result := n;
  end;
end;

function TSymbolList.UndefinedCount: Integer;
var
  n: Integer;
begin
  Result := 0;
  for n := 0 to FList.Count - 1 do
  begin
    if SymbolByIndex[n].ValueType = sbtNone then Inc(Result)
  end;
end;

{ TPostfixList }

constructor TPostfixList.Create;
begin
  SetLength(FList, 0);
end;

destructor TPostfixList.Destroy;
begin
  SetLength(FList, 0);
  inherited Destroy;
end;

function TPostfixList.Pop: Variant;
var
  n: Integer;
begin
  n := Length(FList) - 1;
  if n >= 0 then
  begin
    with FList[n] do
    begin
      if Opcode = #0 then
        Result := Value
      else Result := Opcode;
    end;
    SetLength(FList, n);
  end
  else Result := NULL;
end;

function TPostfixList.PopInt64: Int64;
var
  v: Variant;
begin
  v := Pop;
  if VarType(v) = varInt64 then
    Result := v
  else Result := 0;
end;

function TPostfixList.PopString: string;
var
  v: Variant;
begin
  v := Pop;
  if VarIsNull(v) then
    Result := ''
  else Result := v;
end;

procedure TPostfixList.PushOpcode(AOpCode: Char);
var
  n: Integer;
begin
  n := Length(FList);
  SetLength(FList, n + 1);
  FList[n].OpCode := AOpCode;
  FList[n].Value := 0;
end;

procedure TPostfixList.PushValue(AValue: Int64);
var
  n: Integer;
begin
  n := Length(FList);
  SetLength(FList, n + 1);
  FList[n].OpCode := #0;
  FList[n].Value := AValue;
end;

{ TZ80Register }

function TZ80Register.IsByte: Boolean;
begin
  Result := (Size = 1) or (RegType = rgtValue) or (RegType = rgtAddr);
end;

function TZ80Register.IsIndexReg: Boolean;
begin
  Result := (RegType = rgtIX) or (RegType = rgtIY);
end;

function TZ80Register.IsWord: Boolean;
begin
  Result := (Size = 2) or (RegType = rgtValue) or (RegType = rgtAddr);
end;

function TZ80Register.PreCode: Byte;
begin
  case RegType of
    rgtIX: Result := $DD;
    rgtIY: Result := $FD;
  else
    Result := 0;
  end;
end;

function TZ80Register.DifferIndex(AReg: TZ80Register): Boolean;
begin
  Result := ((RegType = rgtIX) and (AReg.RegType = rgtIY))
    or ((RegType = rgtIY) and (AReg.RegType = rgtIX));
end;

function TZ80Register.DifferSize(AReg: TZ80Register): Boolean;
begin
  Result := (Size <> AReg.Size) and (RegType <> rgtValue) and (AReg.RegType <> rgtValue)
    and (RegType <> rgtAddr) and (AReg.RegType <> rgtAddr);
end;

function TZ80Register.HaveIndex: Boolean;
begin
  Result := IsIndexReg and (Offset = 6);
end;

{ TSymbolClass }

function TSymbolClass.AddItem(AItem: TSymbolClassItem): Integer;
begin
  if AItem <> nil then
    Result := FItems.Add(AItem)
  else Result := -1;
end;

procedure TSymbolClass.Clear;
var
  sclass: TSymbolClassItem;
  n: Integer;
begin
  n := FItems.Count;
  while n > 0 do
  begin
    Dec(n);
    sclass := FItems[n];
    FItems[n] := nil;
    sclass.Free;
  end;
  FItems.Clear;
end;

constructor TSymbolClass.Create(AName: string);
begin
  inherited Create(AName);
  FItems := TList.Create;
end;

destructor TSymbolClass.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TSymbolClass.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TSymbolClass.GetItems(AIndex: string): TSymbolClassItem;
var
  n: Integer;
begin
  n := IndexOf(AIndex);
  if n < 0 then
    Result := nil
  else Result := Items[n];
end;

function TSymbolClass.GetSize: Integer;
var
  n: Integer;
begin
  Result := 0;
  n := FItems.Count;
  while n > 0 do
  begin
    Dec(n);
    Inc(Result, Items[n].FTotalSize);
  end;
end;

function TSymbolClass.GetItems(AIndex: Integer): TSymbolClassItem;
begin
  Result := TSymbolClassItem(FItems[AIndex]);
end;

function TSymbolClass.GetValueType: TSymbolType;
begin
  Result := sbtClass;
end;

function TSymbolClass.IndexOf(AName: string): Integer;
var
  n: Integer;
begin
  Result := -1;
  if (AName <> '') then
  begin
    n := FItems.Count;
    while (n > 0) and (Result < 0) do
    begin
      Dec(n);
      if Items[n].FName = AName then Result := n;
    end;
  end;
end;

{ TSymbolConst }

function TSymbolConst.GetValueType: TSymbolType;
begin
  Result := sbtConst;
end;

{ TSymbolLabel }

function TSymbolLabel.GetValueType: TSymbolType;
begin
  Result := sbtLabel;
end;

{ TSymbolClassItem }

constructor TSymbolClassItem.Create;
begin
  FIndex := 0;
  FSize := 0;
  FTotalSize := 0;
  FName := '';
  FValue := Unassigned;
end;

function TSymbolClassItem.GetVBytes: Variant;
var
  n, nd, md, nb: Integer;
  v: Variant;
  b: Byte;
begin
  Result := VarArrayOf([]);
  md := VarArrayHighBound(FValue, 1);
  nb := 0;
  for nd := 0 to md do
  begin
    v := FValue[nd];
    if FSize > 0 then
    begin
      VarArrayRedim(Result, nb + FSize - 1);
      for n := 1 to FSize do
      begin
        Result[nb] := Byte(v);
        v := UInt64(v) shr 8;
        Inc(nb);
      end;
    end
    else
    begin
      if VarArrayHighBound(v, 1) > 0 then
        b := v[1]
      else b := 0;
      n := v[0];
      VarArrayRedim(Result, nb + n - 1);
      while n > 0 do
      begin
        Result[nb] := b;
        Inc(nb);
        Dec(n);
      end;
    end;
  end;
end;

{ TSourceTexts }

constructor TSourceTexts.Create(AFileName: string; ATexts: TStrings);
begin
  FTexts := TStringList.Create;
  if ATexts <> nil then
    FTexts.Assign(ATexts)
  else FTexts.LoadFromFile(AFileName);
  FFileName := AFileName;
  FLineIndex := 0;
end;

destructor TSourceTexts.Destroy;
begin
  FTexts.Free;
  inherited Destroy;
end;

function TSourceTexts.FetchLine: TSourceLine;
begin
  Result.FileName := FFileName;
  if FLineIndex < FTexts.Count then
  begin
    Result.Text := UpperCase(Trim(FTexts[FLineIndex]));
    Result.LineIndex := FLineIndex;
    Inc(FLineIndex);
  end
  else
  begin
    Result.Text := '';
    Result.LineIndex := -1;
  end;
end;

{ TSourceList }

function TSourceList.Add(AFileName: string; ATexts: TStrings): Integer;
begin
  if IndexOf(AFileName) < 0 then
    Result := FSources.Add(TSourceTexts.Create(AFileName, ATexts))
  else Result := -1;
end;

procedure TSourceList.Clear;
var
  stexts: TSourceTexts;
  n: Integer;
begin
  n := FSources.Count;
  while n > 0 do
  begin
    Dec(n);
    stexts := FSources[n];
    FSources[n] := nil;
    stexts.Free;
  end;
  FSources.Clear;
end;

constructor TSourceList.Create;
begin
  FSources := TList.Create;
end;

destructor TSourceList.Destroy;
begin
  Clear;
  FreeAndNil(FSources);
  inherited Destroy;
end;

function TSourceList.FetchLine: TSourceLine;
var
  n: Integer;
begin
  Result.Clear;
  n := FSources.Count;
  while (n > 0) and (Result.LineIndex < 0) do
  begin
    Dec(n);
    Result := TSourceTexts(FSources[n]).FetchLine;
  end;
end;

function TSourceList.IndexOf(AFileName: string): Integer;
var
  n: Integer;
begin
  Result := -1;
  n := FSources.Count;
  while (n > 0) and (Result < 0) do
  begin
    Dec(n);
    if SameText(AFileName, TSourceTexts(FSources[n]).FFileName) then Result := n;
  end;
end;

procedure TSourceList.Reset;
var
  stexts: TSourceTexts;
  n: Integer;
begin
  n := FSources.Count;
  while n > 1 do
  begin
    Dec(n);
    stexts := FSources[n];
    FSources[n] := nil;
    FSources.Delete(n);
    stexts.Free;
  end;
  if n > 0 then TSourceTexts(FSources[0]).FLineIndex := 0;
end;

{ TSourceLine }

procedure TSourceLine.Clear;
begin
  FileName := '';
  Text := '';
  LineIndex := -1;
end;

end.
