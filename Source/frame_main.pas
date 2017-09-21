unit frame_main;

interface

uses
  MXCompiler, Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ComCtrls,
  Dialogs, LCLType, ActnList, ExtCtrls, BCButton, BGRACustomDrawn,
  BCImageButton, Graphics, LMessages, Windows;

const
  LM_PAINTLINE = LM_USER;

type

  { TMemo }

  TMemo  = class(StdCtrls.TMemo)
  private
   procedure CNCommand(var Message: TLMCommand); message CN_COMMAND;
   procedure LMVScroll(var Msg: TLMHScroll); message LM_VSCROLL;
  end;

  { TFrameMain }

  TFrameMain = class(TFrame)
    ActionDeleteLine: TAction;
    ActionSelectAll: TAction;
    ActionGoto: TAction;
    ActionSearchNext: TAction;
    ActionSearch: TAction;
    ActionBrowse: TAction;
    ActionAssemble: TAction;
    ActionNew: TAction;
    ActionSaveAs: TAction;
    ActionSave: TAction;
    ActionMain: TActionList;
    ButtonAssemble: TButton;
    ButtonBrowse: TButton;
    ButtonSave: TButton;
    ButtonSaveAs: TButton;
    ButtonNew: TButton;
    ButtonSearch: TButton;
    ButtonGoto: TButton;
    CheckOverwrite: TCheckBox;
    DialogOpenSource: TOpenDialog;
    DialogAssemble: TSaveDialog;
    EditFileName: TEdit;
    MemoEdit: TMemo;
    MemoOutput: TMemo;
    PageMain: TPageControl;
    PaintBoxLine: TPaintBox;
    Panel1: TPanel;
    DialogSave: TSaveDialog;
    TabEdit: TTabSheet;
    TabOutput: TTabSheet;
    procedure ActionAssembleExecute(Sender: TObject);
    procedure ActionBrowseExecute(Sender: TObject);
    procedure ActionDeleteLineExecute(Sender: TObject);
    procedure ActionGotoExecute(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure ActionSaveAsExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionSearchExecute(Sender: TObject);
    procedure ActionSearchNextExecute(Sender: TObject);
    procedure ActionSelectAllExecute(Sender: TObject);
    procedure MemoEditChange(Sender: TObject);
    procedure MemoEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure MemoEditMouseDown(Sender: TObject; Button: TMouseButton;
     Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxLinePaint(Sender: TObject);
  private
    Compiler: TMXCompiler;
    FChanged: Boolean;
    FSearchText: string;
    procedure LMPaintLine(var msg: TMsg); message LM_PAINTLINE;
    procedure SearchText(AMemo: TMemo; AText: string; AStartPos: Integer = 0);
    { private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FrameCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FrameShow(Sender: TObject);
    { public declarations }
  end;

implementation

{$R *.lfm}

{ TMemo }

procedure TMemo.CNCommand(var Message: TLMCommand);
var
  con: TWinControl;
begin
  case Message.NotifyCode of
    EN_VSCROLL:
    begin
      con := Parent;
      while not (con is TFrame) and (con <> nil) do
      begin
        con := con.Parent;
      end;
      if con <> nil then con.Perform(LM_PAINTLINE, 0, 0);
    end;
  end;
  inherited;
end;

procedure TMemo.LMVScroll(var Msg: TLMHScroll);
var
  con: TWinControl;
begin
  begin
    con := Parent;
    while not (con is TFrame) and (con <> nil) do
    begin
      con := con.Parent;
    end;
    if con <> nil then con.Perform(LM_PAINTLINE, 0, 0);
  end;
  inherited;
end;

{ TFrameMain }

procedure TFrameMain.ActionSaveExecute(Sender: TObject);
var
  ext: string;
begin
  ext := UpperCase(ExtractFileExt(EditFileName.Text));
  if (EditFileName.Text = '') or (ext <> '.ASM') then
    ButtonSaveAs.Click
  else
  begin
    MemoEdit.Lines.SaveToFile(EditFileName.Text);
    ButtonSave.Enabled := False;
    FChanged := False;
  end;
end;

procedure TFrameMain.ActionSearchExecute(Sender: TObject);
var
  memo: TMemo;
begin
  if InputQuery('Find text', 'Search text:', FSearchText) then
  begin
    if PageMain.TabIndex = 0 then
      memo := MemoEdit
    else memo := MemoOutput;
    SearchText(memo, FSearchText)
  end;
end;

procedure TFrameMain.ActionSearchNextExecute(Sender: TObject);
var
  memo: TMemo;
begin
  if FSearchText <> '' then
  begin
    if PageMain.TabIndex = 0 then
      memo := MemoEdit
    else memo := MemoOutput;
    SearchText(memo, FSearchText, memo.SelStart + memo.SelLength);
  end
end;

procedure TFrameMain.ActionSelectAllExecute(Sender: TObject);
begin
  if PageMain.TabIndex = 0 then
  begin
    if MemoEdit.Focused then MemoEdit.SelectAll;
  end
  else if MemoOutput.Focused then MemoOutput.SelectAll;
end;

procedure TFrameMain.ActionSaveAsExecute(Sender: TObject);
begin
  if EditFileName.Text = '' then
    DialogSave.FileName := ''
  else DialogSave.FileName := ChangeFileExt(EditFileName.Text, '.asm');
  if DialogSave.Execute then
  begin
    MemoEdit.Lines.SaveToFile(DialogSave.FileName);
    EditFileName.Text := DialogSave.FileName;
    ButtonSave.Enabled := False;
    FChanged := False;
  end;
end;

procedure TFrameMain.ActionNewExecute(Sender: TObject);
begin
  if FChanged then
  begin
    case MessageDlg('Do you want to save changed previous document?', mtWarning, [mbYes, mbNo, mbCancel], 0, mbCancel) of
      mrYes: ButtonSave.Click;
      mrNo: FChanged := False;
    end;
  end;
  if not FChanged then
  begin
    PageMain.TabIndex := 0;
    MemoEdit.Clear;
    EditFileName.Text := '';
    FChanged := False;
    MemoEdit.SetFocus;
    PaintBoxLine.Invalidate;
  end;
end;

procedure TFrameMain.ActionAssembleExecute(Sender: TObject);
var
  fname: string;
begin
  PageMain.TabIndex := 1;
  Compiler.Compile(MemoEdit.Lines, MemoOutput.Lines);
  MemoOutput.SelStart := Length(MemoOutput.Text);
  MemoOutput.SelLength := 0;
  MemoOutput.Perform(EM_LINESCROLL, 0, MemoOutput.Lines.Count - 1);
  if Compiler.Address - Compiler.StartAddress = 0 then Exit;
  fname := ChangeFileExt(EditFileName.Text, '.' + Compiler.OutputExt);
  if (not CheckOverwrite.Checked and FileExists(fname)) or (EditFileName.Text = '') then
  begin
    DialogAssemble.FileName := fname;
    DialogAssemble.Filter := Format('Output files (*.%s)|*.%s', [Compiler.OutputExt, Compiler.OutputExt]);
    if DialogAssemble.Execute then
    begin
      Compiler.SaveToFile(DialogAssemble.FileName, Compiler.StartAddress, Compiler.Address - 1);
      CheckOverwrite.Checked := True;
    end;
  end
  else
  begin
    Compiler.SaveToFile(fname, Compiler.StartAddress, Compiler.Address - 1);
    CheckOverwrite.Checked := True;
  end;
end;

procedure TFrameMain.ActionBrowseExecute(Sender: TObject);
var
  ext: string;
begin
  if DialogOpenSource.Execute then
  begin
    if FChanged then
    begin
      case MessageDlg('Do you want to save changed previous document?', mtWarning, [mbYes, mbNo, mbCancel], 0, mbCancel) of
        mrYes: ButtonSave.Click;
        mrNo: FChanged := False;
      end;
    end;
    if not FChanged then
    begin
      EditFileName.Text := DialogOpenSource.FileName;
      ext := UpperCase(ExtractFileExt(EditFileName.Text));
      if ext = '.ASM' then
      begin
        MemoEdit.Lines.LoadFromFile(EditFileName.Text);
        ButtonSave.Enabled := False;
      end
      else
      begin
        Compiler.Decompile(EditFileName.Text, MemoEdit.Lines);
        CheckOverwrite.Checked := False;
        ButtonSave.Enabled := True;
      end;
      PageMain.TabIndex := 0;
      FChanged := False;
      MemoEdit.SetFocus;
      PaintBoxLine.Invalidate;
    end;
  end;
end;

procedure TFrameMain.ActionDeleteLineExecute(Sender: TObject);
begin
  if MemoEdit.Focused then MemoEdit.Lines.Delete(MemoEdit.CaretPos.y);
end;

procedure TFrameMain.ActionGotoExecute(Sender: TObject);
var
  r, nr, n: Integer;
  s: string;
begin
  if InputQuery('Goto Line Number', 'Enter line number:', s) then
  begin
    PageMain.TabIndex := 0;
    r := StrToIntDef(s, MemoEdit.CaretPos.Y + 1);
    if r < 1 then
      r := 1
    else if r > MemoEdit.Lines.Count then r := MemoEdit.Lines.Count;
    n := 0;
    nr := 0;
    Dec(r);
    while nr < r do
    begin
      Inc(n, Length(MemoEdit.Lines[nr]) + 2);
      Inc(nr);
    end;
    MemoEdit.SelStart := n;
    MemoEdit.SelLength := Length(MemoEdit.Lines[r]);
    MemoEdit.SetFocus;
    PaintBoxLine.Invalidate;
  end;
end;

procedure TFrameMain.MemoEditChange(Sender: TObject);
begin
  FChanged := True;
  ButtonSave.Enabled := True;
end;

procedure TFrameMain.MemoEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_SHIFT, VK_CONTROL, VK_MENU:;
  else
    PaintBoxLine.Invalidate;
  end;
end;

procedure TFrameMain.MemoEditMouseDown(Sender: TObject; Button: TMouseButton;
 Shift: TShiftState; X, Y: Integer);
begin
  PaintBoxLine.Invalidate;
end;

procedure TFrameMain.PaintBoxLinePaint(Sender: TObject);
var
  ScrollInfo: TScrollInfo;
  n, m, y, h: Integer;
begin
  with TPaintBox(Sender).Canvas do
  begin
    Brush.Color := clSilver;
    FillRect(ClipRect);
    Font.Assign(MemoEdit.Font);
    h := TextHeight('A');
    with ScrollInfo do
    begin
      cbSize := SizeOf(Self);
      fMask := SIF_POS or SIF_RANGE or SIF_TRACKPOS;
    end;
    GetScrollInfo(MemoEdit.Handle, SB_VERT, ScrollInfo);
    n := ScrollInfo.nPos + 1;
    Brush.Color := clGray;
    FillRect(Bounds(0, 4 + (MemoEdit.CaretPos.Y - n + 1) * h, PaintBoxLine.Width, h));
    y := 0;
    Brush.Style := bsClear;
    m := MemoEdit.Lines.Count;
    while (y < MemoEdit.ClientRect.Bottom) and (n <= m) do
    begin
      TextOut(4, 4 + y, IntToStr(n));
      Inc(y, h);
      Inc(n);
    end;
  end;
end;

procedure TFrameMain.LMPaintLine(var msg: TMsg);
begin
  PaintBoxLine.Invalidate;
end;

procedure TFrameMain.SearchText(AMemo: TMemo; AText: string; AStartPos: Integer
  );
var
  found: Boolean;
  lpos: Integer;
  s, sm: string;
begin
  Inc(AStartPos);
  found := False;
  sm := AMemo.Text;
  s := Copy(sm, AStartPos, Length(AText));
  lpos := Length(sm);
  Inc(AStartPos, Length(AText));
  while (AStartPos <= lpos) and not found do
  begin
    found := CompareText(AText, s) = 0;
    if not found then
    begin
      Delete(s, 1, 1);
      s := s + Copy(sm, AStartPos, 1);
      Inc(AStartPos);
    end;
  end;
  if found then
  begin
    AMemo.SelStart := AStartPos - Length(AText) - 1;
    AMemo.SelLength := Length(AText);
    AMemo.SetFocus;
  end;
end;

constructor TFrameMain.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
  Compiler := TMXCompiler.Create;
  ButtonSave.Enabled := False;
  FChanged := False;
  FSearchText := '';
end;

destructor TFrameMain.Destroy;
begin
  Compiler.Free;
  inherited Destroy;
end;

procedure TFrameMain.FrameCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if FChanged then
  begin
    case MessageDlg('Do you want to save changed the document before exit?', mtWarning, [mbYes, mbNo, mbCancel], 0, mbCancel) of
      mrYes: MemoEdit.Lines.SaveToFile(EditFileName.Text);
      mrCancel: CanClose := False;
    end;
  end;
end;

procedure TFrameMain.FrameShow(Sender: TObject);
begin
  PageMain.TabIndex := 0;
  MemoEdit.SetFocus;
end;

end.

