unit FormMain;

{$MODE Delphi}

interface

uses
  XPConfig, XPCompiler, XPAffect, XPVideo, XPDebug, LCLIntf, LCLType,
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Grids, ActnList, FrameEdit;

type
  THistoryRec = record
  public
    SelStart, SelLength: Integer;
    Text: string;
  end;
  THistoryArray = array of THistoryRec;

  { TMainForm }

  TMainForm = class(TForm)
    SourceEdit: TEdit;
    BrowseSourceButton: TButton;
    OpenSourceDialog: TOpenDialog;
    PageMain: TPageControl;
    TabMain: TTabSheet;
    TabOutput: TTabSheet;
    SaveDialog: TSaveDialog;
    PanelFile: TPanel;
    AssembleButton: TButton;
    SaveAsButton: TButton;
    SaveButton: TButton;
    CheckOverwrite: TCheckBox;
    AssembleDialog: TSaveDialog;
    NewButton: TButton;
    MainActionList: TActionList;
    ActionSearch: TAction;
    ActionSearchNext: TAction;
    ActionGoto: TAction;
    ActionSave: TAction;
    ActionNew: TAction;
    ActionSelectAll: TAction;
    ActionDeleteLine: TAction;
    ReloadButton: TButton;
    FormatButton: TButton;
    TabDebug: TTabSheet;
    Panel2: TPanel;
    PaintDebug: TPaintBox;
    Panel3: TPanel;
    GroupRegCurrent: TGroupBox;
    Label1: TLabel;
    LabelRegA: TLabel;
    LabelRegB: TLabel;
    Label4: TLabel;
    LabelRegC: TLabel;
    Label6: TLabel;
    LabelRegD: TLabel;
    Label8: TLabel;
    LabelRegE: TLabel;
    Label10: TLabel;
    LabelRegL: TLabel;
    LabelRegH: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    LabelRegIX: TLabel;
    Label16: TLabel;
    LabelRegIY: TLabel;
    Label18: TLabel;
    LabelRegSP: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    LabelRegPC: TLabel;
    Label23: TLabel;
    LabelFlagS: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    LabelFlagS_: TLabel;
    LabelRegC_: TLabel;
    Label29: TLabel;
    LabelRegB_: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    LabelRegD_: TLabel;
    Label34: TLabel;
    LabelRegE_: TLabel;
    LabelRegL_: TLabel;
    Label37: TLabel;
    LabelRegH_: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    LabelRegA_: TLabel;
    LabelRegF: TLabel;
    Label43: TLabel;
    LabelRegF_: TLabel;
    Label45: TLabel;
    LabelRegI: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    LabelRegR: TLabel;
    GroupDebug: TGroupBox;
    PanelSearch: TPanel;
    SearchButton: TButton;
    GotoButton: TButton;
    LabelKeyPress: TLabel;
    LabelKeyDown: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    ActionRun: TAction;
    ActionStepInto: TAction;
    ActionRunToCursor: TAction;
    ActionPause: TAction;
    LabelFlagZ: TLabel;
    LabelFlagZ_: TLabel;
    LabelFlagX1_: TLabel;
    LabelFlagX1: TLabel;
    LabelFlagH_: TLabel;
    LabelFlagH: TLabel;
    LabelFlagX2_: TLabel;
    LabelFlagX2: TLabel;
    LabelFlagPV: TLabel;
    LabelFlagPV_: TLabel;
    LabelFlagN: TLabel;
    LabelFlagN_: TLabel;
    LabelFlagC: TLabel;
    LabelFlagC_: TLabel;
    Button5: TButton;
    ActionStepOver: TAction;
    Panel5: TPanel;
    PageMessage: TPageControl;
    TabMessages: TTabSheet;
    TabErrors: TTabSheet;
    MemoError: TMemo;
    MemoMessage: TMemo;
    Button6: TButton;
    ActionReset: TAction;
    PanelCodes: TPanel;
    LabelMemPC: TLabel;
    LabelMemSP: TLabel;
    PageMisc: TPageControl;
    TabPReg: TTabSheet;
    Label2: TLabel;
    LabelPRegA: TLabel;
    LabelPRegB: TLabel;
    Label53: TLabel;
    LabelPRegC: TLabel;
    Label55: TLabel;
    LabelPRegD: TLabel;
    Label57: TLabel;
    LabelPRegE: TLabel;
    Label59: TLabel;
    LabelPRegL: TLabel;
    LabelPRegH: TLabel;
    Label62: TLabel;
    Label63: TLabel;
    LabelPRegIX: TLabel;
    Label65: TLabel;
    LabelPRegIY: TLabel;
    Label67: TLabel;
    LabelPRegSP: TLabel;
    Label69: TLabel;
    Label70: TLabel;
    LabelPRegPC: TLabel;
    Label72: TLabel;
    LabelPFlagS: TLabel;
    Label74: TLabel;
    Label75: TLabel;
    LabelPFlagS_: TLabel;
    LabelPRegC_: TLabel;
    Label78: TLabel;
    LabelPRegB_: TLabel;
    Label80: TLabel;
    Label81: TLabel;
    LabelPRegD_: TLabel;
    Label83: TLabel;
    LabelPRegE_: TLabel;
    LabelPRegL_: TLabel;
    Label86: TLabel;
    LabelPRegH_: TLabel;
    Label88: TLabel;
    Label89: TLabel;
    LabelPRegA_: TLabel;
    LabelPRegF: TLabel;
    Label92: TLabel;
    LabelPRegF_: TLabel;
    Label94: TLabel;
    LabelPRegI: TLabel;
    Label96: TLabel;
    Label97: TLabel;
    LabelPRegR: TLabel;
    LabelPFlagZ: TLabel;
    LabelPFlagZ_: TLabel;
    LabelPFlagX1: TLabel;
    LabelPFlagX1_: TLabel;
    LabelPFlagH: TLabel;
    LabelPFlagH_: TLabel;
    LabelPFlagX2: TLabel;
    LabelPFlagX2_: TLabel;
    LabelPFlagPV: TLabel;
    LabelPFlagPV_: TLabel;
    LabelPFlagN: TLabel;
    LabelPFlagN_: TLabel;
    LabelPFlagC: TLabel;
    LabelPFlagC_: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label15: TLabel;
    Label17: TLabel;
    Label19: TLabel;
    Label22: TLabel;
    Label24: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label30: TLabel;
    Label33: TLabel;
    Label35: TLabel;
    LabelIFF1: TLabel;
    Label38: TLabel;
    LabelIFF2: TLabel;
    Label42: TLabel;
    LabelPIFF1: TLabel;
    Label41: TLabel;
    LabelPIFF2: TLabel;
    Label46: TLabel;
    TabVDP: TTabSheet;
    Label36: TLabel;
    LabelVDP0: TLabel;
    LabelVDP1: TLabel;
    Label50: TLabel;
    LabelVDP2: TLabel;
    Label56: TLabel;
    LabelVDP3: TLabel;
    Label68: TLabel;
    Label103: TLabel;
    LabelVDP4: TLabel;
    LabelVDP5: TLabel;
    Label106: TLabel;
    LabelVDP6: TLabel;
    Label108: TLabel;
    LabelVDP7: TLabel;
    Label110: TLabel;
    Label111: TLabel;
    LabelVDP8: TLabel;
    LabelVDP9: TLabel;
    Label114: TLabel;
    Label51: TLabel;
    LabelVDP12: TLabel;
    LabelVDP13: TLabel;
    Label60: TLabel;
    LabelVDP14: TLabel;
    Label66: TLabel;
    LabelVDP15: TLabel;
    Label73: TLabel;
    Label76: TLabel;
    LabelVDP16: TLabel;
    LabelVDP17: TLabel;
    Label82: TLabel;
    LabelVDP18: TLabel;
    Label85: TLabel;
    LabelVDP19: TLabel;
    Label90: TLabel;
    Label91: TLabel;
    LabelVDP20: TLabel;
    LabelVDP21: TLabel;
    Label98: TLabel;
    Label99: TLabel;
    LabelVDPSX: TLabel;
    LabelVDPSY: TLabel;
    Label102: TLabel;
    LabelVDPDX: TLabel;
    Label116: TLabel;
    LabelVDPDY: TLabel;
    Label118: TLabel;
    Label119: TLabel;
    LabelVDPNX: TLabel;
    LabelVDPNY: TLabel;
    Label122: TLabel;
    LabelVDPCL: TLabel;
    Label124: TLabel;
    LabelVDPAG: TLabel;
    Label126: TLabel;
    Label127: TLabel;
    LabelVDPCM: TLabel;
    Label131: TLabel;
    LabelVDPS0: TLabel;
    LabelVDPS1: TLabel;
    Label134: TLabel;
    LabelVDPS2: TLabel;
    Label136: TLabel;
    LabelVDPS3: TLabel;
    Label138: TLabel;
    Label139: TLabel;
    LabelVDPS4: TLabel;
    LabelVDPS5: TLabel;
    Label142: TLabel;
    LabelVDPS6: TLabel;
    Label144: TLabel;
    LabelVDPS7: TLabel;
    Label146: TLabel;
    Label147: TLabel;
    LabelVDPS8: TLabel;
    LabelVDPS9: TLabel;
    Label150: TLabel;
    LabelVDP10: TLabel;
    Label152: TLabel;
    LabelVDP22: TLabel;
    Label154: TLabel;
    LabelVDP11: TLabel;
    Label156: TLabel;
    LabelVDP23: TLabel;
    Label158: TLabel;
    PageRun: TPageControl;
    TabOwnRun: TTabSheet;
    GridCompile: TDrawGrid;
    TabSystemRun: TTabSheet;
    GridSystem: TDrawGrid;
    TabPalette: TTabSheet;
    Label44: TLabel;
    LabelPAL0: TLabel;
    LabelPAL1: TLabel;
    Label54: TLabel;
    LabelPAL2: TLabel;
    Label71: TLabel;
    LabelPAL3: TLabel;
    Label95: TLabel;
    Label58: TLabel;
    LabelPAL4: TLabel;
    LabelPAL5: TLabel;
    Label79: TLabel;
    LabelPAL6: TLabel;
    Label93: TLabel;
    LabelPAL7: TLabel;
    Label101: TLabel;
    Label104: TLabel;
    Label109: TLabel;
    Label113: TLabel;
    Label117: TLabel;
    Label120: TLabel;
    Label125: TLabel;
    Label129: TLabel;
    Label132: TLabel;
    Label133: TLabel;
    LabelPAL8: TLabel;
    LabelPAL9: TLabel;
    LabelPAL10: TLabel;
    LabelPAL11: TLabel;
    LabelPAL12: TLabel;
    LabelPAL13: TLabel;
    LabelPAL14: TLabel;
    LabelPAL15: TLabel;
    Label163: TLabel;
    Label175: TLabel;
    Label49: TLabel;
    LabelCycle: TLabel;
    TabRAM: TTabSheet;
    Panel1: TPanel;
    EditRAMAddress: TLabeledEdit;
    GridRAM: TDrawGrid;
    Label52: TLabel;
    LabelScreen: TLabel;
    Label64: TLabel;
    LabelPage: TLabel;
    TabAppend: TTabSheet;
    Button7: TButton;
    ActionRunReturn: TAction;
    TabSlot: TTabSheet;
    LabelSlot0: TLabel;
    Label77: TLabel;
    LabelSlot1: TLabel;
    Label87: TLabel;
    LabelSlot2: TLabel;
    Label105: TLabel;
    LabelSlot3: TLabel;
    Label112: TLabel;
    ActionSaveAs: TAction;
    procedure BrowseSourceButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure AssembleButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ActionSearchExecute(Sender: TObject);
    procedure ActionSearchNextExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure ActionSelectAllExecute(Sender: TObject);
    procedure ActionDeleteLineExecute(Sender: TObject);
    procedure ActionGotoExecute(Sender: TObject);
    procedure ReloadButtonClick(Sender: TObject);
    procedure FormatButtonClick(Sender: TObject);
    procedure GridCompileDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure PaintDebugPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ActionRunExecute(Sender: TObject);
    procedure ActionStepIntoExecute(Sender: TObject);
    procedure ActionStepOverExecute(Sender: TObject);
    procedure ActionResetExecute(Sender: TObject);
    procedure ActionPauseExecute(Sender: TObject);
    procedure ActionRunToCursorExecute(Sender: TObject);
    procedure PaintDebugClick(Sender: TObject);
    procedure GridCompileMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridRAMDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure EditRAMAddressChange(Sender: TObject);
    procedure PageMainChange(Sender: TObject);
    procedure PageMainDrawTab(Control: TCustomTabControl; TabIndex: Integer;
      const Rect: TRect; Active: Boolean);
    procedure PageMainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PageMainMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ActionRunReturnExecute(Sender: TObject);
    procedure ActionSaveAsExecute(Sender: TObject);
  private
    FFrame, FMainFrame, FOutputFrame: TEditFrame;
    OldWndMethod: TWndMethod;
    FExtraFrames: TList;
    function LoadFile(AFileName: string; Reload: Boolean = False): Boolean;
    function GetMemText(ATitle: string; AAddress: Integer): string;
    function GetPageIndex(AFrame: TEditFrame): Integer;
    function GetPageFrame(AIndex: Integer = -1): TEditFrame;
    function GetEditFrame(AIndex: Integer = -1): TEditFrame;
    function FilesChanged: Boolean;
    function CreateEditFrame: TEditFrame;
    procedure DeleteEditFrame(AIndex: Integer);
    procedure UpdateValue(AFormat: string; ALabel: TLabel; AValue: Integer; AAffected: Boolean); overload;
    procedure UpdateValue(AFormat: string; ALabel: TLabel; AValues: array of Byte; AAffected: Boolean); overload;
    procedure UpdateValue(AFormat: string; ALabel1, ALabel2: TLabel; AValue1, AValue2: Integer; AAffected: Boolean); overload;
    procedure UpdatePal(ALabel: TLabel; APal: TXPVideoPal; AAffected: Boolean); overload;
    procedure UpdateTabTitle(AIndex: Integer = -1);
    procedure UpdateInfos;
    procedure SaveAll;
    procedure EnableDebug(AEnabled: Boolean);
    procedure PanelWndProc(var Message: TMessage);
    procedure DebugStop(Sender: TObject);
    procedure DebugPaint(Sender: TObject);
    procedure MemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MemoKeyPress(Sender: TObject; var Key: Char);
    procedure MemoTextChanged(Sender: TObject);
    procedure MemoSaveChanged(Sender: TObject);
    function GetExtraFrames(AIndex: Integer): TEditFrame;
    property ExtraFrames[AIndex: Integer]: TEditFrame read GetExtraFrames;
  public
    Compiler: TXPCompiler;
    procedure UpdateSource;
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  FormPage;

const
  TabCloseRect: TRect = (Left: -14; Top: 5; Right: -4; Bottom: 15);

procedure TMainForm.ActionSelectAllExecute(Sender: TObject);
begin
  FFrame := GetPageFrame;
  if FFrame <> nil then
  begin
    FFrame.SelectAll;
    FocusControl(FFrame.MemoEdit);
  end;
end;

procedure TMainForm.ActionStepIntoExecute(Sender: TObject);
begin
  if PageMain.TabIndex <> 2 then
    PageMain.TabIndex := 2
  else Compiler.Debug.StepInto(True);
  UpdateInfos;
end;

procedure TMainForm.ActionStepOverExecute(Sender: TObject);
begin
  if PageMain.TabIndex <> 2 then
    PageMain.TabIndex := 2
  else
  begin
//    UpdateInfos;
    EnableDebug(False);
    Compiler.Debug.StepOver;
  end;
end;

procedure TMainForm.ActionDeleteLineExecute(Sender: TObject);
begin
  FFrame := GetPageFrame;
  if FFrame <> nil then
  begin
    FFrame.DeleteLine;
    FocusControl(FFrame.MemoEdit);
  end;
end;

procedure TMainForm.ActionGotoExecute(Sender: TObject);
begin
  FFrame := GetPageFrame;
  if FFrame <> nil then
  begin
    FFrame.GotoLine;
    FocusControl(FFrame.MemoEdit);
  end;
end;

procedure TMainForm.ActionNewExecute(Sender: TObject);
begin
  FFrame := GetEditFrame;
  FFrame.New;
  SourceEdit.Text := '';
  PageMain.TabIndex := GetPageIndex(FFrame);
  UpdateTabTitle(PageMain.TabIndex);
  if PageMain.TabIndex = 0 then Compiler.Debug.Clear;
  FocusControl(FFrame.MemoEdit);
end;

procedure TMainForm.ActionPauseExecute(Sender: TObject);
begin
  Caption := FloatToStr(Now);
  Compiler.Debug.Stop;
end;

procedure TMainForm.ActionResetExecute(Sender: TObject);
begin
  Compiler.Debug.Reset;
  UpdateInfos;
end;

procedure TMainForm.ActionRunExecute(Sender: TObject);
begin
  UpdateInfos;
  if PageMain.TabIndex <> 2 then
    PageMain.TabIndex := 2
  else
  begin
    EnableDebug(False);
    Compiler.Debug.Run;
  end;
end;

procedure TMainForm.ActionRunReturnExecute(Sender: TObject);
begin
  if PageMain.TabIndex <> 2 then
    PageMain.TabIndex := 2
  else
  begin
    EnableDebug(False);
    Compiler.Debug.RunAndReturn;
  end;
end;

procedure TMainForm.ActionRunToCursorExecute(Sender: TObject);
var
  row: Integer;
begin
  if PageMain.TabIndex <> 2 then
    PageMain.TabIndex := 2
  else
  begin
    if PageRun.TabIndex = 0 then
      row := GridCompile.Selection.Top
    else row := GridSystem.Selection.Top;
    EnableDebug(False);
    if PageRun.TabIndex = 0 then
      Compiler.Debug.RunToLine(row)
    else Compiler.Debug.RunToSystemLine(row);
  end;
end;

procedure TMainForm.ActionSaveAsExecute(Sender: TObject);
begin
  FFrame := GetPageFrame;
  if FFrame <> nil then
  begin
    if FFrame.SaveAs then
    begin
      SourceEdit.Text := FFrame.FileName;
      UpdateTabTitle;
      SaveButton.Enabled := False;
    end;
  end;
end;

procedure TMainForm.ActionSaveExecute(Sender: TObject);
begin
  FFrame := GetEditFrame;
  if FFrame.Save then SaveButton.Enabled := False;
end;

procedure TMainForm.ActionSearchExecute(Sender: TObject);
begin
  FFrame := GetPageFrame;
  if FFrame <> nil then
  begin
    FFrame.Search;
    FocusControl(FFrame.MemoEdit);
  end;
end;

procedure TMainForm.ActionSearchNextExecute(Sender: TObject);
begin
  FFrame := GetPageFrame;
  if FFrame <> nil then
  begin
    FFrame.SearchNext;
    FocusControl(FFrame.MemoEdit);
  end;
end;

procedure TMainForm.AssembleButtonClick(Sender: TObject);
var
  fname: string;
begin
  PageMain.TabIndex := 1;
  Compiler.Compile(FMainFrame.FileName, FOutputFrame.MemoEdit.Lines, FMainFrame.MemoEdit.Lines);
  FOutputFrame.MemoEdit.VertScrollBar.Position := FOutputFrame.MemoEdit.VertScrollBar.Range;
  FOutputFrame.MemoEdit.SelStart := Length(FOutputFrame.MemoEdit.Text);
  Compiler.Debug.Reset;
  GridCompile.RowCount := Compiler.Debug.Lines.Count;
  GridSystem.RowCount := Compiler.Debug.SystemLines.Count;
  UpdateInfos;
  if Compiler.Address - Compiler.StartAddress = 0 then Exit;
  fname := ChangeFileExt(FMainFrame.FileName, '.' + Compiler.OutputExt);
  if not CheckOverwrite.Checked and FileExists(fname) then
  begin
    AssembleDialog.FileName := fname;
    AssembleDialog.Filter := Format('Output files (*.%s)|*.%s', [Compiler.OutputExt, Compiler.OutputExt]);
    if AssembleDialog.Execute then
    begin
      Compiler.SaveToFile(AssembleDialog.FileName, Compiler.StartAddress, Compiler.Address - 1);
      CheckOverwrite.Checked := True;
    end;
  end
  else
  begin
    Compiler.SaveToFile(fname, Compiler.StartAddress, Compiler.Address - 1);
    CheckOverwrite.Checked := True;
  end;
end;

procedure TMainForm.BrowseSourceButtonClick(Sender: TObject);
begin
  if OpenSourceDialog.Execute then LoadFile(OpenSourceDialog.FileName);
end;

procedure TMainForm.DebugPaint(Sender: TObject);
begin
  if PageForm.Visible then PageForm.Repaint;
  PaintDebug.Repaint;
end;

procedure TMainForm.DebugStop(Sender: TObject);
begin
  UpdateInfos;
  EnableDebug(True);
end;

procedure TMainForm.DeleteEditFrame(AIndex: Integer);
var
  tab: TTabSheet;
  frame: TFrame;
begin
  if (AIndex > 2) and (AIndex < PageMain.PageCount - 1) then
  begin
    tab := PageMain.Pages[AIndex];
    tab.Parent := nil;
    Dec(AIndex, 3);
    FFrame := FExtraFrames[AIndex];
    FFrame.Parent := nil;
    frame := FExtraFrames[AIndex];
    FExtraFrames[AIndex] := nil;
    FExtraFrames.Delete(AIndex);
    frame.Free;
    tab.Free;
    PageMain.TabIndex := AIndex + 2;
    PageMain.Invalidate;
  end;
end;

procedure TMainForm.GridRAMDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  ad, n: Integer;
  grid: TDrawGrid;
  cv: TCanvas;
  s: string;
begin
  ad := StrToIntDef('$' + EditRAMAddress.Text, 0) + (ARow shl 4);
  if ACol = 0 then
    s := Format('%5.5X:', [ad])
  else
  begin
    s := '';
    for n := 0 to 15 do
    begin
      if n = 8 then
        s := s + '-'
      else if n > 0 then s := s + ' ';
      s := s + Format('%2.2X', [Compiler.Debug.ReadByte(ad + n)]);
    end;
  end;
  grid := TDrawGrid(Sender);
  cv := grid.Canvas;
  cv.Brush.Color := grid.Color;
  cv.FillRect(Rect);
  cv.Font := grid.Font;
  cv.TextRect(Rect, Rect.Left, Rect.Top, s);
end;

procedure TMainForm.EditRAMAddressChange(Sender: TObject);
begin
  GridRAM.Invalidate;
end;

procedure TMainForm.EnableDebug(AEnabled: Boolean);
begin
  ActionRun.Enabled := AEnabled;
  ActionRunToCursor.Enabled := AEnabled;
  ActionRunReturn.Enabled := AEnabled;
  ActionStepInto.Enabled := AEnabled;
  ActionStepOver.Enabled := AEnabled;
  ActionReset.Enabled := AEnabled;
  PanelFile.Enabled := AEnabled;
  PanelSearch.Enabled := AEnabled;
  SourceEdit.Enabled := AEnabled;
  BrowseSourceButton.Enabled := AEnabled;
  PageMain.Enabled := AEnabled;
end;

procedure TMainForm.ReloadButtonClick(Sender: TObject);
begin
  FFrame := GetEditFrame;
  LoadFile(FFrame.FileName, True);
end;

function TMainForm.FilesChanged: Boolean;
var
  n: Integer;
begin
  Result := False;
  n := PageMain.PageCount;
  while (n > 0) and not Result do
  begin
    Dec(n);
    FFrame := GetPageFrame(n);
    if FFrame <> nil then Result := Result or FFrame.TextChanged;
  end;
end;

procedure TMainForm.FormatButtonClick(Sender: TObject);
begin
  FFrame := GetPageFrame;
  if FFrame <> nil then
  begin
    Enabled := False;
    FormatButton.Enabled := False;
    try
      FFrame.FormatText;
    finally
      FormatButton.Enabled := True;
      Enabled := True;
    end;
    FocusControl(FFrame.MemoEdit);
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FilesChanged then
  begin
    case MessageDlg('Do you want to save changed the document before exit?', mtWarning, [mbYes, mbNo, mbCancel], 0, mbCancel) of
      mrYes: SaveAll;
      mrCancel: CanClose := False;
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PaintDebug.ControlStyle := PaintDebug.ControlStyle + [csOpaque];
  FExtraFrames := TList.Create;

  FMainFrame := CreateEditFrame;
  FMainFrame.Parent := PageMain.Pages[0];

  FOutputFrame := CreateEditFrame;
  FOutputFrame.MemoEdit.Color := $C0FFFF;
  FOutputFrame.ReadOnly := True;
  FOutputFrame.LineNumberVisible := False;
  FOutputFrame.Parent := PageMain.Pages[1];

  OldWndMethod := Panel2.WindowProc;
  Panel2.WindowProc := PanelWndProc;
  Compiler := TXPCompiler.Create;
  Compiler.Debug.OnStop := DebugStop;
//  Compiler.Debug.OnUpdate := DebugUpdate;
  Compiler.Debug.OnPaint := DebugPaint;
  SaveButton.Enabled := False;
  FFrame := nil;
  Compiler.Debug.SystemDecode;
  UpdateInfos;
//  AppendHistory;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Compiler);
  FreeAndNil(FExtraFrames);
  FreeAndNil(FOutputFrame);
  FreeAndNil(FMainFrame);
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  GridCompile.ColWidths[1] := GridCompile.ClientWidth - GridCompile.ColWidths[0];
  GridSystem.ColWidths[1] := GridSystem.ClientWidth - GridSystem.ColWidths[0];
  GridRAM.ColWidths[1] := GridRAM.ClientWidth - GridRAM.ColWidths[0];
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  PageMain.TabIndex := 0;
  PaintDebugPaint(PaintDebug);
  FocusControl(FMainFrame.MemoEdit);
end;

function TMainForm.GetEditFrame(AIndex: Integer): TEditFrame;
begin
  if AIndex < 0 then AIndex := PageMain.TabIndex;
  if AIndex < 3 then
    Result := FMainFrame
  else Result := FExtraFrames[AIndex - 3];
end;

function TMainForm.GetExtraFrames(AIndex: Integer): TEditFrame;
begin
  Result := TEditFrame(FExtraFrames[AIndex]);
end;

function TMainForm.GetMemText(ATitle: string; AAddress: Integer): string;
var
  n: Integer;
  b: Byte;
begin
  Result := Format('[%2.2s]:', [ATitle]);
  for n := 0 to 15 do
  begin
    b := Compiler.Debug.ReadByte(AAddress + n);
    if n = 8 then
      Result := Result + '-'
    else Result := Result + ' ';
    Result := Result + Format('%2.2X', [b]);
  end;
end;

function TMainForm.GetPageFrame(AIndex: Integer): TEditFrame;
var
  n: Integer;
begin
  n := PageMain.TabIndex;
  if AIndex < 0 then AIndex := n;
  case AIndex of
    0: Result := FMainFrame;
    1: Result := FOutputFrame;
    2: Result := nil;
  else
    Dec(n, 3);
    if (n >= 0) and (n < FExtraFrames.Count) then
      Result := FExtraFrames[n]
    else Result := nil;
  end;
end;

function TMainForm.GetPageIndex(AFrame: TEditFrame): Integer;
var
  n: Integer;
begin
  if AFrame = FMainFrame then
    Result := 0
  else if AFrame = FOutputFrame then
    Result := 1
  else
  begin
    Result := 2;
    n := FExtraFrames.Count;
    while (n > 0) and (Result = 2) do
    begin
      Dec(n);
      if FExtraFrames[n] = AFrame then Result := n + 3;
    end;
  end;
end;

procedure TMainForm.GridCompileDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  grid: TDrawGrid;
  cv: TCanvas;
  lines: TXPDebugLines;
  line: TXPDebugLine;
  sel: Boolean;
  n, nr, nc, w: Integer;
  s: string;
  r: TRect;
begin
  grid := TDrawGrid(Sender);
  if grid.Name = 'GridSystem' then
    lines := Compiler.Debug.SystemLines
  else lines := Compiler.Debug.Lines;
  cv := grid.Canvas;
  cv.Font.Assign(grid.Font);
  line := lines[ARow];
  sel := gdSelected in State;
  nr := lines.SelectIndex;
  nc := lines.SelectOffset;
  case ACol of
    0:
    begin
      if nr = ARow then
        if sel then
          cv.Brush.Color := $FF80C0
        else cv.Brush.Color := $80FFFF
      else if sel then
        cv.Brush.Color := $FF8080
      else cv.Brush.Color := $E0E0E0;
      cv.FillRect(Rect);
      if (nr = ARow) and (nc >= 0) then
      begin
        n := cv.TextWidth('A');
        cv.Brush.Color := $00E0E0;
        r := Rect;
        Inc(r.Left, n * (6 + nc * 3));
        r.Width := n * 2 + 8;
        cv.FillRect(r);
      end;
      if line <> nil then
      begin
        Rect.Inflate(-4, 0, 0, 0);
        s := Format('%4.4X:', [line.Address]);
        w := cv.TextWidth(s);
        cv.Brush.Style := bsClear;
        if line.Breaked then
        begin
          cv.Font.Color := $0000C0;
          cv.Font.Style := [fsBold];
        end
        else
        begin
          cv.Font.Color := clBlack;
          cv.Font.Style := [];
        end;
        cv.TextRect(Rect, Rect.Left, Rect.Top, s);
        s := '';
        for n := 0 to Length(line.Codes) - 1 do s := s + Format(' %2.2X', [line.Codes[n]]);
        if s <> '' then
        begin
          cv.Font.Color := $C00000;
          cv.Font.Style := [];
          Rect.Left := Rect.Left + w;
          cv.TextRect(Rect, Rect.Left, Rect.Top, s);
        end;
      end;
    end;
  else
    if lines.SelectIndex = ARow then
      if sel then
        cv.Brush.Color := $FFA0F0
      else cv.Brush.Color := grid.Color
    else if sel then
      cv.Brush.Color := $FFC0C0
    else cv.Brush.Color := clWhite;
    cv.FillRect(Rect);
    if line <> nil then
    begin
      Rect.Inflate(-8, 0, 0, 0);
      s := line.Text;
      if s <> '' then
      begin
        cv.Brush.Style := bsClear;
        cv.TextRect(Rect, Rect.Left, Rect.Top, s);
      end;
    end;
  end;
  if ARow = grid.RowCount - 1 then
  begin
    Compiler.Debug.SystemDecode(grid.RowCount + TXPConfig.MaxSystemLine);
    GridSystem.RowCount := Compiler.Debug.SystemLines.Count;
    GridSystem.Row := ARow;
  end;
end;

procedure TMainForm.GridCompileMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  lines: TXPDebugLines;
  line: TXPDebugLine;
  grid: TDrawGrid;
begin
  grid := TDrawGrid(Sender);
  if grid.Name = 'GridSystem' then
    lines := Compiler.Debug.SystemLines
  else lines := Compiler.Debug.Lines;
  if X < 40 then
  begin
    line := lines[grid.Row];
    if line <> nil then
    begin
      line.Breaked := not line.Breaked;
      grid.Invalidate;
    end;
  end;
end;

function TMainForm.LoadFile(AFileName: string; Reload: Boolean): Boolean;
var
  ext: string;
begin
  FFrame := GetEditFrame;
  if FFrame.TextChanged then
  begin
    if Reload then
      case MessageDlg('Do you want to REPLACE changed?', mtWarning, [mbYes, mbCancel], 0, mbCancel) of
        mrYes: FFrame.TextChanged := False
      end
    else
    case MessageDlg('Do you want to SAVE changed?', mtWarning, [mbYes, mbNo, mbCancel], 0, mbCancel) of
      mrYes: ActionSave.Execute;
      mrNo: FFrame.TextChanged := False;
    end;
  end;
  if not FFrame.TextChanged then
  begin
    PageMain.TabIndex := GetPageIndex(FFrame);
    Enabled := False;
    FFrame.MemoEdit.Enabled := False;
    try
      ext := UpperCase(ExtractFileExt(AFileName));
      if ext = '.ASM' then
      begin
        FFrame.MemoEdit.Lines.LoadFromFile(AFileName);
        FFrame.TextChanged := False;
      end
      else
      begin
        Compiler.Decompile(AFileName, FFrame.MemoEdit.Lines);
        CheckOverwrite.Checked := False;
        FFrame.TextChanged := True;
      end;
      FFrame.History := nil;
      FFrame.HIstoryIndex := -1;
      FFrame.InvalidateLineNumber;
      FFrame.FileName := AFileName;
      SourceEdit.Text := AFileName;
      UpdateTabTitle;
      Result := True;
    finally
      FFrame.MemoEdit.Enabled := True;
      Enabled := True;
      FocusControl(FFrame.MemoEdit);
    end;
  end
  else Result := False;
  SaveButton.Enabled := FilesChanged;
end;

procedure TMainForm.MemoTextChanged(Sender: TObject);
begin
  SaveButton.Enabled := True;
end;

procedure TMainForm.MemoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  LabelKeyDown.Caption := Format('KeyDown: $%X %D', [Key, Key]);
end;

procedure TMainForm.MemoKeyPress(Sender: TObject; var Key: Char);
begin
  LabelKeyPress.Caption := Format('KeyPress: $%X %D', [Ord(Key), Ord(Key)]);
end;

procedure TMainForm.MemoSaveChanged(Sender: TObject);
begin
  ActionSave.Execute;
end;

procedure TMainForm.PageMainChange(Sender: TObject);
var
  page: TPageControl;
  tab, atab: TTabSheet;
  frame: TEditFrame;
  n: Integer;
  ev: TNotifyEvent;
begin
  page := TPageControl(Sender);
  n := page.PageCount;
  if page.TabIndex = n - 1 then
  begin
    atab := page.Pages[n - 1];
    tab := TTabSheet.Create(page);
    tab.PageControl := page;
    ev := page.OnChange;
    page.OnChange := nil;
    try
      atab.PageIndex := n;
      page.ActivePageIndex := n - 1;
      frame := CreateEditFrame;
      frame.Parent := tab;
      FExtraFrames.Add(frame);
      UpdateTabTitle;
    finally
      page.OnChange := ev;
    end;
  end;
  FFrame := GetEditFrame;
  SourceEdit.Text := FFrame.FileName;
end;

procedure TMainForm.PageMainDrawTab(Control: TCustomTabControl;
  TabIndex: Integer; const Rect: TRect; Active: Boolean);
var
  cv: TControlCanvas;
  r: TRect;
  s: string;
begin
  cv := TControlCanvas.Create;
  cv.Control := Control;
  r := Rect;
  Inc(r.Left, 8);
  Inc(r.Top, 2);
  s := PageMain.Pages[TabIndex].Caption;
  case TabIndex of
    1: cv.Font.Color := clGreen;
    2: cv.Font.Color := clBlue;
  else
    cv.Font.Color := clBlack;
  end;
  cv.Brush.Style := bsClear;
  cv.TextRect(r, Rect.Left, Rect.Top, s);
  if (TabIndex > 2) and (TabIndex < PageMain.PageCount - 1) then
  begin
    cv.Pen.Color := clBlack;
    cv.Pen.Width := 1;
    r := TabCloseRect;
    r.Offset(Rect.Right, Rect.Top);
    cv.MoveTo(r.Left, r.Top);
    cv.LineTo(r.Right - 1, r.Bottom - 1);
    cv.MoveTo(r.Right - 2, r.Top);
    cv.LineTo(r.Left - 1, r.Bottom - 1);
  end;
  cv.Free;
end;

procedure TMainForm.PageMainMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  n: Integer;
  rt, rc: TRect;
begin
  n := PageMain.IndexOfTabAt(X, Y);
  if (n > 2) and (n < PageMain.PageCount - 1) then
  begin
    rt := PageMain.TabRect(n);
    rc := TabCloseRect;
    rc.Offset(rt.Right, rt.Top);
    if rc.Contains(Point(X, Y)) then DeleteEditFrame(n);
  end;
end;

procedure TMainForm.PageMainMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  n: Integer;
  rt, rc: TRect;
var
  cv: TControlCanvas;
begin
  cv := TControlCanvas.Create;
  cv.Control := PageMain;
  n := PageMain.IndexOfTabAt(X, Y);
  if (n > 2) and (n < PageMain.PageCount - 1) then
  begin
    rt := PageMain.TabRect(n);
    rc := TabCloseRect;
    rc.Offset(rt.Right, rt.Top - 2);
    rt := rc;
    rt.Inflate(2, 2);
    if rc.Contains(Point(X, Y)) then
    begin
      cv.Brush.Color := $C0A080;
      cv.FillRect(rt);
      cv.Pen.Color := clBlack;
      cv.Pen.Width := 1;
      cv.MoveTo(rc.Left, rc.Top);
      cv.LineTo(rc.Right - 1, rc.Bottom - 1);
      cv.MoveTo(rc.Right - 2, rc.Top);
      cv.LineTo(rc.Left - 1, rc.Bottom - 1);
    end;
  end;
  cv.Free;
end;

procedure TMainForm.PaintDebugClick(Sender: TObject);
begin
  PageForm.Visible := not PageForm.Visible;
end;

procedure TMainForm.PaintDebugPaint(Sender: TObject);
begin
  Compiler.Debug.Video.DrawTo(TPaintBox(Sender).Canvas, TPaintBox(Sender).ClientRect);
end;

procedure TMainForm.PanelWndProc(var Message: TMessage);
begin
  if Message.Msg <> WM_ERASEBKGND then OldWndMethod(Message);
end;

procedure TMainForm.SaveAll;
var
  n: Integer;
begin
  PageMain.TabIndex := 0;
  FMainFrame.Save;
  n := FExtraFrames.Count;
  while n > 0 do
  begin
    Dec(n);
    PageMain.TabIndex := n + 3;
    ExtraFrames[n].Save;
  end;
end;

function TMainForm.CreateEditFrame: TEditFrame;
begin
  Result := TEditFrame.Create(nil);
  Result.OnChanged := MemoTextChanged;
  Result.OnSaveChanged := MemoSaveChanged;
  Result.OnKeyDown := MemoKeyDown;
  Result.OnKeyPress := MemoKeyPress;
end;

procedure TMainForm.UpdateValue(AFormat: string; ALabel: TLabel;
  AValue: Integer; AAffected: Boolean);
begin
  if AAffected then
    ALabel.Font.Color := $FF0000
  else ALabel.Font.Color := clBlack;
  ALabel.Caption := Format(AFormat, [AValue]);
end;

procedure TMainForm.UpdateValue(AFormat: string; ALabel1, ALabel2: TLabel;
  AValue1, AValue2: Integer; AAffected: Boolean);
begin
  UpdateValue(AFormat, ALabel1, AValue1, AAffected);
  if ALabel2 <> nil then
  begin
    if AValue1 <> AValue2 then
      ALabel2.Font.Color := $0000C0
    else ALabel2.Font.Color := clBlack;
    ALabel2.Caption := Format(AFormat, [AValue2]);
  end;
end;

procedure TMainForm.UpdateInfos;
var
  db: TXPDebug;
  af, vf: TXPAffected;
  vdo: TXPVideo;
begin
  db := Compiler.Debug;
  vdo := Compiler.Debug.Video;
  af := db.Regs.Affected;
  vf := vdo.Regs.Affected;
  UpdateValue('%2.2X', LabelRegA, LabelPRegA, db.Regs.A, db.PRegs.A, af['A']);
  UpdateValue('%2.2X', LabelRegB, LabelPRegB, db.Regs.B, db.PRegs.B, af['B']);
  UpdateValue('%2.2X', LabelRegC, LabelPRegC, db.Regs.C, db.PRegs.C, af['C']);
  UpdateValue('%2.2X', LabelRegD, LabelPRegD, db.Regs.D, db.PRegs.D, af['D']);
  UpdateValue('%2.2X', LabelRegE, LabelPRegE, db.Regs.E, db.PRegs.E, af['E']);
  UpdateValue('%2.2X', LabelRegF, LabelPRegF, db.Regs.F, db.PRegs.F, af['F']);
  UpdateValue('%2.2X', LabelRegH, LabelPRegH, db.Regs.H, db.PRegs.H, af['H']);
  UpdateValue('%2.2X', LabelRegL, LabelPRegL, db.Regs.L, db.PRegs.L, af['L']);
  UpdateValue('%2.2X', LabelRegI, LabelPRegI, db.Regs.I, db.PRegs.I, af['I']);
  UpdateValue('%2.2X', LabelRegR, LabelPRegR, db.Regs.R, db.PRegs.R, af['R']);

  UpdateValue('%2.2X', LabelRegA_, LabelPRegA_, db.Regs.A_, db.PRegs.A_, af['A_']);
  UpdateValue('%2.2X', LabelRegB_, LabelPRegB_, db.Regs.B_, db.PRegs.B_, af['B_']);
  UpdateValue('%2.2X', LabelRegC_, LabelPRegC_, db.Regs.C_, db.PRegs.C_, af['C_']);
  UpdateValue('%2.2X', LabelRegD_, LabelPRegD_, db.Regs.D_, db.PRegs.D_, af['D_']);
  UpdateValue('%2.2X', LabelRegE_, LabelPRegE_, db.Regs.E_, db.PRegs.E_, af['E_']);
  UpdateValue('%2.2X', LabelRegF_, LabelPRegF_, db.Regs.F_, db.PRegs.F_, af['F_']);
  UpdateValue('%2.2X', LabelRegH_, LabelPRegH_, db.Regs.H_, db.PRegs.H_, af['H_']);
  UpdateValue('%2.2X', LabelRegL_, LabelPRegL_, db.Regs.L_, db.PRegs.L_, af['L_']);

  UpdateValue('%4.4X', LabelRegIX, LabelPRegIX, db.Regs.IX, db.PRegs.IX, af['IX']);
  UpdateValue('%4.4X', LabelRegIY, LabelPRegIY, db.Regs.IY, db.PRegs.IY, af['IY']);
  UpdateValue('%4.4X', LabelRegPC, LabelPRegPC, db.Regs.PC, db.PRegs.PC, af['PC']);
  UpdateValue('%4.4X', LabelRegSP, LabelPRegSP, db.Regs.SP, db.PRegs.SP, af['SP']);

  UpdateValue('%1.1X', LabelFlagS, LabelPFlagS, db.Regs.SBit, db.PRegs.SBit, af[['SF', 'F']]);
  UpdateValue('%1.1X', LabelFlagZ, LabelPFlagZ, db.Regs.ZBit, db.PRegs.ZBit, af[['ZF', 'F']]);
  UpdateValue('%1.1X', LabelFlagX1, LabelPFlagX1, db.Regs.X1Bit, db.PRegs.X1Bit, af[['XF1', 'F']]);
  UpdateValue('%1.1X', LabelFlagH, LabelPFlagH, db.Regs.HBit, db.PRegs.HBit, af[['HF', 'F']]);
  UpdateValue('%1.1X', LabelFlagX2, LabelPFlagX2, db.Regs.X2Bit, db.PRegs.X2Bit, af[['XF2', 'F']]);
  UpdateValue('%1.1X', LabelFlagPV, LabelPFlagPV, db.Regs.PVBit, db.PRegs.PVBit, af[['PVF', 'F']]);
  UpdateValue('%1.1X', LabelFlagN, LabelPFlagN, db.Regs.NBit, db.PRegs.NBit, af[['NF', 'F']]);
  UpdateValue('%1.1X', LabelFlagC, LabelPFlagC, db.Regs.CBit, db.PRegs.CBit, af[['CF', 'F']]);

  UpdateValue('%1.1X', LabelFlagS_, LabelPFlagS_, db.Regs.SBit_, db.PRegs.SBit_, af[['SF_', 'F_']]);
  UpdateValue('%1.1X', LabelFlagZ_, LabelPFlagZ_, db.Regs.ZBit_, db.PRegs.ZBit_, af[['ZF_', 'F_']]);
  UpdateValue('%1.1X', LabelFlagX1_, LabelPFlagX1_, db.Regs.X1Bit_, db.PRegs.X1Bit_, af[['XF1_', 'F_']]);
  UpdateValue('%1.1X', LabelFlagH_, LabelPFlagH_, db.Regs.HBit_, db.PRegs.HBit_, af[['HF_', 'F_']]);
  UpdateValue('%1.1X', LabelFlagX2_, LabelPFlagX2_, db.Regs.X2Bit_, db.PRegs.X2Bit_, af[['XF2_', 'F_']]);
  UpdateValue('%1.1X', LabelFlagPV_, LabelPFlagPV_, db.Regs.PVBit_, db.PRegs.PVBit_, af[['PVF_', 'F_']]);
  UpdateValue('%1.1X', LabelFlagN_, LabelPFlagN_, db.Regs.NBit_, db.PRegs.NBit_, af[['NF_', 'F_']]);
  UpdateValue('%1.1X', LabelFlagC_, LabelPFlagC_, db.Regs.CBit_, db.PRegs.CBit_, af[['CF_', 'F_']]);

  UpdateValue('%2.2X', LabelVDP0, vdo.Regs[0], vf[['0']]);
  UpdateValue('%2.2X', LabelVDP1, vdo.Regs[1], vf[['1']]);
  UpdateValue('%2.2X', LabelVDP2, vdo.Regs[2], vf[['2']]);
  UpdateValue('%2.2X', LabelVDP3, vdo.Regs[3], vf[['3']]);
  UpdateValue('%2.2X', LabelVDP4, vdo.Regs[4], vf[['4']]);
  UpdateValue('%2.2X', LabelVDP5, vdo.Regs[5], vf[['5']]);
  UpdateValue('%2.2X', LabelVDP6, vdo.Regs[6], vf[['6']]);
  UpdateValue('%2.2X', LabelVDP7, vdo.Regs[7], vf[['7']]);
  UpdateValue('%2.2X', LabelVDP8, vdo.Regs[8], vf[['8']]);
  UpdateValue('%2.2X', LabelVDP9, vdo.Regs[9], vf[['9']]);
  UpdateValue('%2.2X', LabelVDP10, vdo.Regs[10], vf[['10']]);
  UpdateValue('%2.2X', LabelVDP11, vdo.Regs[11], vf[['11']]);
  UpdateValue('%2.2X', LabelVDP12, vdo.Regs[12], vf[['12']]);
  UpdateValue('%2.2X', LabelVDP13, vdo.Regs[13], vf[['13']]);
  UpdateValue('%2.2X', LabelVDP14, vdo.Regs[14], vf[['14']]);
  UpdateValue('%2.2X', LabelVDP15, vdo.Regs[15], vf[['15']]);
  UpdateValue('%2.2X', LabelVDP16, vdo.Regs[16], vf[['16']]);
  UpdateValue('%2.2X', LabelVDP17, vdo.Regs[17], vf[['17']]);
  UpdateValue('%2.2X', LabelVDP18, vdo.Regs[18], vf[['18']]);
  UpdateValue('%2.2X', LabelVDP19, vdo.Regs[19], vf[['19']]);
  UpdateValue('%2.2X', LabelVDP20, vdo.Regs[20], vf[['20']]);
  UpdateValue('%2.2X', LabelVDP21, vdo.Regs[21], vf[['21']]);
  UpdateValue('%2.2X', LabelVDP22, vdo.Regs[22], vf[['22']]);
  UpdateValue('%2.2X', LabelVDP23, vdo.Regs[23], vf[['23']]);

  UpdateValue('%4.4X', LabelVDPSX, [vdo.Regs[32], vdo.Regs[33]], vf[['32', '33']]);
  UpdateValue('%4.4X', LabelVDPSY, [vdo.Regs[34], vdo.Regs[35]], vf[['34', '35']]);
  UpdateValue('%4.4X', LabelVDPDX, [vdo.Regs[36], vdo.Regs[37]], vf[['36', '37']]);
  UpdateValue('%4.4X', LabelVDPDY, [vdo.Regs[38], vdo.Regs[39]], vf[['38', '39']]);
  UpdateValue('%4.4X', LabelVDPNX, [vdo.Regs[40], vdo.Regs[41]], vf[['40', '41']]);
  UpdateValue('%4.4X', LabelVDPNY, [vdo.Regs[42], vdo.Regs[43]], vf[['42', '43']]);
  UpdateValue('%2.2X', LabelVDPCL, vdo.Regs[44], vf[['44']]);
  UpdateValue('%2.2X', LabelVDPAG, vdo.Regs[45], vf[['45']]);
  UpdateValue('%2.2X', LabelVDPCM, vdo.Regs[46], vf[['46']]);

  UpdateValue('%2.2X', LabelVDPS0, vdo.Status[0], vf[['S0']]);
  UpdateValue('%2.2X', LabelVDPS1, vdo.Status[1], vf[['S1']]);
  UpdateValue('%2.2X', LabelVDPS2, vdo.Status[2], vf[['S2']]);
  UpdateValue('%2.2X', LabelVDPS3, vdo.Status[3], vf[['S3']]);
  UpdateValue('%2.2X', LabelVDPS4, vdo.Status[4], vf[['S4']]);
  UpdateValue('%2.2X', LabelVDPS5, vdo.Status[5], vf[['S5']]);
  UpdateValue('%2.2X', LabelVDPS6, vdo.Status[6], vf[['S6']]);
  UpdateValue('%2.2X', LabelVDPS7, vdo.Status[7], vf[['S7']]);
  UpdateValue('%2.2X', LabelVDPS8, vdo.Status[8], vf[['S8']]);
  UpdateValue('%2.2X', LabelVDPS9, vdo.Status[9], vf[['S9']]);

  UpdatePal(LabelPAL0, vdo.Pals[0], vf[['P0']]);
  UpdatePal(LabelPAL1, vdo.Pals[1], vf[['P1']]);
  UpdatePal(LabelPAL2, vdo.Pals[2], vf[['P2']]);
  UpdatePal(LabelPAL3, vdo.Pals[3], vf[['P3']]);
  UpdatePal(LabelPAL4, vdo.Pals[4], vf[['P4']]);
  UpdatePal(LabelPAL5, vdo.Pals[5], vf[['P5']]);
  UpdatePal(LabelPAL6, vdo.Pals[6], vf[['P6']]);
  UpdatePal(LabelPAL7, vdo.Pals[7], vf[['P7']]);
  UpdatePal(LabelPAL8, vdo.Pals[8], vf[['P8']]);
  UpdatePal(LabelPAL9, vdo.Pals[9], vf[['P9']]);
  UpdatePal(LabelPAL10, vdo.Pals[10], vf[['P10']]);
  UpdatePal(LabelPAL11, vdo.Pals[11], vf[['P11']]);
  UpdatePal(LabelPAL12, vdo.Pals[12], vf[['P12']]);
  UpdatePal(LabelPAL13, vdo.Pals[13], vf[['P13']]);
  UpdatePal(LabelPAL14, vdo.Pals[14], vf[['P14']]);
  UpdatePal(LabelPAL15, vdo.Pals[15], vf[['P15']]);

  UpdateValue('%d', LabelScreen, db.Video.Screen, False);
  UpdateValue('%d', LabelPage, db.Video.Page, False);
  UpdateValue('%10.10d', LabelCycle, db.LastTime, False);

  LabelSlot0.Caption := db.Mapper.Text[0];
  LabelSlot1.Caption := db.Mapper.Text[1];
  LabelSlot2.Caption := db.Mapper.Text[2];
  LabelSlot3.Caption := db.Mapper.Text[3];

  MemoError.Text := Compiler.Debug.Errors.Text;
  MemoError.VertScrollBar.Position := MemoError.VertScrollBar.Range;
  MemoMessage.Text := Compiler.Debug.Messages.Text;
  MemoMessage.VertScrollBar.Position := MemoError.VertScrollBar.Range;
  if MemoError.Lines.Count > 0 then PageMessage.TabIndex := 1;

  LabelMemPC.Caption := GetMemText('PC', Compiler.Debug.Regs.PC);
  LabelMemSP.Caption := GetMemText('SP', Compiler.Debug.Regs.SP);

  if Compiler.Debug.Lines.SelectIndex >= 0 then
  begin
    GridCompile.Row := Compiler.Debug.Lines.SelectIndex;
    PageRun.TabIndex := 0;
  end
  else
  begin
    GridCompile.Row := 0;
    PageRun.TabIndex := 1;
    Compiler.Debug.SystemDecode;
    GridSystem.RowCount := Compiler.Debug.SystemLines.Count;
    GridSystem.Row := Compiler.Debug.SystemLines.SelectIndex;
  end;
  GridCompile.Invalidate;
  GridSystem.Invalidate;
  GridRAM.Invalidate;
  PaintDebug.Repaint;
  if Visible and PageForm.Visible then PageForm.Repaint;
end;

procedure TMainForm.UpdatePal(ALabel: TLabel; APal: TXPVideoPal;
  AAffected: Boolean);
begin
  if AAffected then
    ALabel.Font.Color := $FF0000
  else ALabel.Font.Color := clBlack;
  ALabel.Caption := Format('%1d %1d %1d', [APal.R, APal.G, APal.B]);
end;

procedure TMainForm.UpdateSource;
begin
  FFrame := GetEditFrame;
  SourceEdit.Text := FFrame.FileName;
end;

procedure TMainForm.UpdateTabTitle(AIndex: Integer);
var
  s: string;
begin
  if AIndex < 0 then AIndex := PageMain.TabIndex;
  case AIndex of
    0:
    begin
      s := UpperCase(ExtractFileName(FMainFrame.FileName));
      if s = '' then s := '<No name>';
    end;
    1: s := 'Output';
    2: s := 'Debug';
  else
    s := UpperCase(ExtractFileName(TEditFrame(FExtraFrames[AIndex - 3]).FileName));
    if s = '' then s := '<No name>';
    s := s + '      ';
  end;
  PageMain.Pages[AIndex].Caption := s;
end;

procedure TMainForm.UpdateValue(AFormat: string; ALabel: TLabel;
  AValues: array of Byte; AAffected: Boolean);
var
  n, v: Integer;
begin
  v := 0;
  n := Length(AValues);
  while n > 0 do
  begin
    Dec(n);
    v := (v shl 8) or AValues[n];
  end;
  if AAffected then
    ALabel.Font.Color := $FF0000
  else ALabel.Font.Color := clBlack;
  ALabel.Caption := Format(AFormat, [v]);
end;

end.
