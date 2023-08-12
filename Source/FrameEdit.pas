unit FrameEdit;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, StdCtrls, ExtCtrls, Graphics,
  Controls, Forms, Dialogs, Math;

type
  THistoryRec = record
  public
    SelStart, SelLength: Integer;
    Text: string;
  end;
  THistoryArray = array of THistoryRec;

  TMemo = class(StdCtrls.TMemo)
  private
    FPaintBoxLine: TPaintBox;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure WMVScroll(var Msg: TWMHScroll); message WM_VSCROLL;
  end;

  TEditFrame = class(TFrame)
    PaintBoxLine: TPaintBox;
    MemoEdit: TMemo;
    SaveDialog: TSaveDialog;
    OpenSourceDialog: TOpenDialog;
    procedure PaintBoxLinePaint(Sender: TObject);
    procedure MemoEditChange(Sender: TObject);
    procedure MemoEditClick(Sender: TObject);
    procedure MemoEditDblClick(Sender: TObject);
    procedure MemoEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MemoEditKeyPress(Sender: TObject; var Key: Char);
    procedure MemoEditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FTextChanged, FLineNumberVisible: Boolean;
    FFileName, FMemoText, FSelText, FSearchText: string;
    FSelStart, FSelLength, FHistoryIndex: Integer;
    FHistory: THistoryArray;
    FOnChanged, FOnSaveChanged: TNotifyEvent;
    FOnKeyDown: TKeyEvent;
    FOnKeyPress: TKeyPressEvent;
    procedure AppendHistory;
    procedure GetMemoSelected;
    procedure DoSaveChanged;
    procedure SearchText(AMemo: TMemo; AText: string; AStartPos: Integer = 0);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetLineNumberVisible(const Value: Boolean);
    function GetLinePos(ALine: Integer): Integer;
    function GetReadOnly: Boolean;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure New;
    function Save: Boolean;
    function SaveAs: Boolean;
    procedure SelectAll;
    procedure DeleteLine;
    procedure GotoLine;
    procedure Search;
    procedure SearchNext;
    procedure FormatText;
    procedure InvalidateLineNumber;
    property TextChanged: Boolean read FTextChanged write FTextChanged;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property LineNumberVisible: Boolean read FLineNumberVisible write SetLineNumberVisible;
    property History: THistoryArray read FHistory write FHistory;
    property HistoryIndex: Integer read FHistoryIndex write FHistoryIndex;
    property FileName: string read FFileName write FFileName;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnSaveChanged: TNotifyEvent read FOnSaveChanged write FOnSaveChanged;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    { Public declarations }
  end;

implementation

{$R *.lfm}

procedure TEditFrame.AppendHistory;
begin
  if FHistoryIndex >= 100 then
    Delete(FHistory, 0, 1)
  else Inc(FHistoryIndex);
  SetLength(FHistory, FHistoryIndex + 1);
  FHistory[FHistoryIndex].SelStart := MemoEdit.SelStart;
  FHistory[FHistoryIndex].SelLength := MemoEdit.SelLength;
  FHistory[FHistoryIndex].Text := MemoEdit.Text;
end;

constructor TEditFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PaintBoxLine.ControlStyle := PaintBoxLine.ControlStyle + [csOpaque];
  MemoEdit.FPaintBoxLine := PaintBoxLine;
  FTextChanged := False;
  FLineNumberVisible := True;
  FHistory := nil;
  FHistoryIndex := -1;
  FFileName := '';
  FMemoText := '';
  FSelText := '';
  FSearchText := '';
  FSelStart := 0;
  FSelLength := 0;
  FOnChanged := nil;
  FOnKeyDown := nil;
  FOnKeyPress := nil;
  ReadOnly := False;
end;

procedure TEditFrame.DeleteLine;
begin
  if MemoEdit.Focused then MemoEdit.Lines.Delete(MemoEdit.CaretPos.Y);
end;

destructor TEditFrame.Destroy;
begin
  FHistory := nil;
  inherited Destroy;
end;

procedure TEditFrame.DoSaveChanged;
begin
  if Assigned(FOnSaveChanged) then FOnSaveChanged(Self);
end;

procedure TEditFrame.FormatText;
var
  n, p, nt: Integer;
  s, sf: string;
  c: Char;
  sl: TStringList;
begin
  MemoEdit.Enabled := False;
  sl := TStringList.Create;
  try
    sl.Text := MemoEdit.Text;
    for n := 0 to sl.Count - 1 do
    begin
      p := 0;
      sf := '';
      s := sl[n];
      while s <> '' do
      begin
        c := s[1];
        if c = #9 then
        begin
          nt := 8 - ((p + 8) and 7);
          sf := sf + StringOfChar(' ', nt);
          Inc(p, nt);
        end
        else
        begin
          sf := sf + c;
          Inc(p);
        end;
        Delete(s, 1, 1);
      end;
      sl[n] := sf;
    end;
    MemoEdit.Text := sl.Text;
  finally
    sl.Free;
    MemoEdit.Enabled := True;
  end;
  AppendHistory;
end;

function TEditFrame.GetLinePos(ALine: Integer): Integer;
var
  n: Integer;
  s: string;
begin
  s := MemoEdit.Text;
  n := 1;
  Result := 0;
  while (ALine > 0) and (n <= Length(s)) do
  begin
    if s[n] = #10 then
    begin
      Dec(ALine);
      Result := n;
    end;
    Inc(n);
  end;
end;

procedure TEditFrame.GetMemoSelected;
var
  len: Integer;
  c: string;
begin
  FMemoText := MemoEdit.Text;
  FSelStart := MemoEdit.SelStart;
  FSelLength := MemoEdit.SelLength;
  len := Length(FMemoText);

  if (Copy(FMemoText, FSelStart + FSelLength, 1) = #10) and (FSelLength > 0) then
  begin
    Dec(FSelLength);
    if (Copy(FSelText, FSelStart + FSelLength, 1) = #13) and (FSelLength > 0) then
    begin
      Dec(FSelLength);
    end;
  end;

  while (FSelStart > 0) and (Copy(FMemoText, FSelStart, 1) <> #10) do
  begin
    Dec(FSelStart);
    Inc(FSelLength);
  end;
  c := Copy(FMemoText, FSelStart + FSelLength + 1, 1);
  while (FSelLength < len) and (c <> #13) and (c <> #10) do
  begin
    Inc(FSelLength);
    c := Copy(FMemoText, FSelStart + FSelLength + 1, 1);
  end;
  FSelText := Copy(FMemoText, FSelStart + 1, FSelLength);
end;

function TEditFrame.GetReadOnly: Boolean;
begin
  Result := MemoEdit.ReadOnly;
end;

procedure TEditFrame.GotoLine;
var
  r, rn, n: Integer;
  s: string;
begin
  s := '';
  if InputQuery('Goto Line Number', 'Enter line number:', s) then
  begin
    r := StrToIntDef(s, MemoEdit.CaretPos.Y + 1);
    if r < 1 then
      r := 1
    else if r > MemoEdit.Lines.Count then r := MemoEdit.Lines.Count;
    if r < MemoEdit.Lines.Count - 15 then
      rn := r - 10
    else rn := r;
    MemoEdit.VertScrollBar.Position := Floor(rn * MemoEdit.VertScrollBar.Range / MemoEdit.Lines.Count);
    n := GetLinePos(r - 1);
    MemoEdit.SelStart := n;
    MemoEdit.SelLength := Length(MemoEdit.Lines[r - 1]);
    InvalidateLineNumber;
  end;
end;

procedure TEditFrame.InvalidateLineNumber;
begin
  if PaintBoxLine.Visible then PaintBoxLine.Invalidate;
end;

procedure TEditFrame.MemoEditChange(Sender: TObject);
begin
  if not ReadOnly then
  begin
    FTextChanged := True;
    if Assigned(FOnChanged) then FOnChanged(Self);
  end;
end;

procedure TEditFrame.MemoEditClick(Sender: TObject);
begin
  FSelStart := MemoEdit.SelStart;
end;

procedure TEditFrame.MemoEditDblClick(Sender: TObject);
const
  SystemChars: string = ' .,;:/*+-%&()=@!''"?<>[]{}^%#|\'#13#10#9;
var
  done: Boolean;
  p, n: Integer;
  s: string;
  c: Char;
begin
  p := FSelStart;
  FSelLength := 0;
  s := MemoEdit.Text;
  n := Length(s);
  repeat
    if FSelStart > 0 then
    begin
      c := s[FSelStart];
      done := Pos(c, SystemChars) > 0;
      if not done then
      begin
        Dec(FSelStart);
        Inc(FSelLength);
      end;
    end
    else done := True;
  until done;
  repeat
    if p < n then
    begin
      c := s[p + 1];
      done := Pos(c, SystemChars) > 0;
      if not done then
      begin
       Inc(FSelLength);
       Inc(p);
      end;
    end
    else done := True;
  until done;
  MemoEdit.SelStart := FSelStart;
  MemoEdit.SelLength := FSelLength;
end;

procedure TEditFrame.MemoEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  his_append: Boolean;
  n, p: Integer;
  s, sc: string;
begin
  if Assigned(FOnKeyDown) then FOnKeyDown(Sender, Key, Shift);
  his_append := False;
  case Key of
    VK_DELETE: his_append := True;
    VK_HOME:
    begin
      if not (ssCtrl in Shift) then
      begin
        GetMemoSelected;
        p := FSelStart;
        if p = MemoEdit.SelStart then
        begin
          s := MemoEdit.Lines[MemoEdit.CaretPos.Y];
          n := p + Length(s);
          sc := Copy(s, p - FSelStart + 1, 1);
          while (p < n) and ((sc = ' ') or (sc = #9)) do
          begin
            Inc(p);
            sc := Copy(s, p - FSelStart + 1, 1);
          end;
        end;
        MemoEdit.SelStart := p;
        MemoEdit.SelLength := 0;
        Key := 0;
      end;
    end;
  end;
  if his_append and not ReadOnly then AppendHistory;
  InvalidateLineNumber;
end;

procedure TEditFrame.MemoEditKeyPress(Sender: TObject; var Key: Char);
var
  kshift, his_append, uncmt: Boolean;
//  kctrl: Boolean;
  n, nl, p: Integer;
  s, sl, sn: string;
  c: Char;
begin
  if Assigned(FOnKeyPress) then FOnKeyPress(Sender, Key);
  his_append := True;
  kshift := (GetAsyncKeyState(VK_SHIFT) and $8000) <> 0;
//  kctrl := (GetAsyncKeyState(VK_CONTROL) and $8000) <> 0;
  case Key of
    #9: // TAB
    begin
      GetMemoSelected;
      if Pos(#10, FSelText) > 0 then
      begin
        if not kshift then
        begin
          s := '    ';
          while FSelText <> '' do
          begin
            c := FSelText[1];
            s := s + c;
            if c = #10 then s := s + '    ';
            Delete(FSelText, 1, 1);
          end;
        end
        else
        begin
          s := '';
          while FSelText <> '' do
          begin
            n := 4;
            while (n > 0) and (Copy(FSelText, 1, 1) = ' ') do
            begin
              Delete(FSelText, 1, 1);
              Dec(n);
            end;
            c := #0;
            while (FSelText <> '') and (c <> #10) do
            begin
              c := FSelText[1];
              s := s + c;
              Delete(FSelText, 1, 1);
            end;
          end;
        end;
        MemoEdit.Text := Copy(FMemoText, 1, FSelStart) + s + Copy(FMemoText, FSelStart + FSelLength + 1, Length(FMemoText) - FSelStart - FSelLength);
        MemoEdit.SelStart := FSelStart;
        MemoEdit.SelLength := Length(s);
      end
      else if not kshift then
      begin
        nl := MemoEdit.CaretPos.Y;
        sl := MemoEdit.Lines[nl];
        sn := StringOfChar(' ', 4 - ((MemoEdit.SelStart - FSelStart + 4) and 3));
        s := Copy(sl, 1, MemoEdit.SelStart - FSelStart) + sn;
        n := MemoEdit.SelStart - FSelStart + MemoEdit.SelLength;
        s := s + Copy(sl, n + 1, Length(sl) - n);
        FSelStart := MemoEdit.SelStart + Length(sn);
        MemoEdit.Lines.BeginUpdate;
        try
          MemoEdit.Lines[nl] := s;
          MemoEdit.SelStart := FSelStart;
          MemoEdit.SelLength := 0;
        finally
          MemoEdit.Lines.EndUpdate;
        end;
      end
      else
      begin
        nl := MemoEdit.CaretPos.Y;
        sl := MemoEdit.Lines[nl];
        p := MemoEdit.SelStart - FSelStart;
        n := 0;
        while (n < 4) and (p > 0) and (Copy(sl, p, 1) = ' ') do
        begin
          Inc(n);
          Dec(p);
        end;
        FSelStart := MemoEdit.SelStart - n;
        s := Copy(sl, 1, p);
        n := p + n + MemoEdit.SelLength;
        s := s + Copy(sl, n + 1, Length(sl) - n);
        MemoEdit.Lines.BeginUpdate;
        try
          MemoEdit.Lines[nl] := s;
          MemoEdit.SelStart := FSelStart;
          MemoEdit.SelLength := 0;
        finally
          MemoEdit.Lines.EndUpdate;
        end;
      end;
      Key := #0;
    end;
    #$11: // CTRL + Q
    begin
      GetMemoSelected;
      n := 0;
      uncmt := True;
      while (n < FSelLength) and uncmt do
      begin
        while (n < FSelLength) and ((FSelText[n + 1] = ' ') or (FSelText[n + 1] = #9)) do Inc(n);
        if (n < FSelLength) and (FSelText[n + 1] <> ';') and (FSelText[n + 1] <> #13) and (FSelText[n + 1] <> #10) then uncmt := False;
        Inc(n);
        while (n < FSelLength) and ((FSelText[n + 1] <> #13) and (FSelText[n + 1] <> #10)) do Inc(n);
        if (n < FSelLength) and (FSelText[n + 1] = #10) then Inc(n);
      end;
      s := '';
      n := 0;
      while n < FSelLength do
      begin
        while (n < FSelLength) and ((FSelText[n + 1] = ' ') or (FSelText[n + 1] = #9)) do
        begin
          s := s + FSelText[n + 1];
          Inc(n);
        end;
        if uncmt then
        begin
          if (n < FSelLength) and (FSelText[n + 1] = ';') then Inc(n);
          if (n < FSelLength) and (FSelText[n + 1] = ' ') then Inc(n);
        end
        else s := s + '; ';
        while (n < FSelLength) and (FSelText[n + 1] <> #10) do
        begin
          s := s + FSelText[n + 1];
          Inc(n);
        end;
        if (n < FSelLength) and (FSelText[n + 1] = #10) then
        begin
          s := s + #10;
          Inc(n);
        end;
      end;
      MemoEdit.Text := Copy(FMemoText, 1, FSelStart) + s + Copy(FMemoText, FSelStart + FSelLength + 1, Length(FMemoText) - FSelStart - FSelLength);
      MemoEdit.SelStart := FSelStart;
      MemoEdit.SelLength := Length(s);
      Key := #0;
    end;
    #$1A: // CTRL + Z
    begin
      if kshift then
      begin
        if FHistoryIndex < Length(FHistory) - 1 then
        begin
          Inc(FHistoryIndex);
          MemoEdit.Text := FHistory[FHistoryIndex].Text;
          MemoEdit.SelStart := FHistory[FHistoryIndex].SelStart;
          MemoEdit.SelLength := FHistory[FHistoryIndex].SelLength;
        end;
      end
      else if FHistoryIndex > 0 then
      begin
        Dec(FHistoryIndex);
        MemoEdit.Text := FHistory[FHistoryIndex].Text;
        MemoEdit.SelStart := FHistory[FHistoryIndex].SelStart;
        MemoEdit.SelLength := FHistory[FHistoryIndex].SelLength;
      end;
      his_append := False;
      Key := #0;
    end;
  end;
//  if kctrl then
//  case Key of
//  end;
  if his_append and not ReadOnly then AppendHistory;
  InvalidateLineNumber;
end;

procedure TEditFrame.MemoEditMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  InvalidateLineNumber;
end;

procedure TEditFrame.New;
begin
  if FTextChanged then
  begin
    case MessageDlg('Do you want to save changed previous document?', mtWarning, [mbYes, mbNo, mbCancel], 0, mbCancel) of
      mrYes: DoSaveChanged;
      mrNo: FTextChanged := False;
    end;
  end;
  if not FTextChanged then
  begin
    FHistory := nil;
    FHIstoryIndex := -1;
    MemoEdit.Clear;
    FFileName := '';
    FTextChanged := False;
    InvalidateLineNumber;
  end;
end;

procedure TEditFrame.PaintBoxLinePaint(Sender: TObject);
var
  ScrollInfo: TScrollInfo;
  n, m, y, h, nc, nr: Integer;
begin
  with PaintBoxLine.Canvas do
  begin
    Brush.Color := clSilver;
    FillRect(ClipRect);
    Font.Assign(MemoEdit.Font);
    h := TextHeight('A');
    n := MemoEdit.VertScrollBar.Position;
    nc := MemoEdit.Lines.Count;
    nr := MemoEdit.VertScrollBar.Range;
    if nr > 0 then
      n := Floor(n * nc / nr) + 1
    else n := 1;
    Brush.Color := clGray;
    FillRect(Bounds(0, (MemoEdit.CaretPos.Y - n + 1) * h, PaintBoxLine.Width, h));
    y := 0;
    Brush.Style := bsClear;
    m := MemoEdit.Lines.Count;
    while (y < MemoEdit.ClientHeight) and (n <= m) do
    begin
      TextOut(4, y, IntToStr(n));
      Inc(y, h);
      Inc(n);
    end;
  end;
end;

function TEditFrame.Save: Boolean;
var
  ext: string;
begin
  ext := UpperCase(ExtractFileExt(FFileName));
  if (FFileName <> '') and (ext = '.ASM') then
  begin
    MemoEdit.Lines.SaveToFile(FFileName);
    FTextChanged := False;
    Result := True;
  end
  else Result := SaveAs;
end;

function TEditFrame.SaveAs: Boolean;
begin
  if FFileName = '' then
    SaveDialog.FileName := ''
  else SaveDialog.FileName := ChangeFileExt(FFileName, '.asm');
  Result := SaveDialog.Execute;
  if Result then
  begin
    MemoEdit.Lines.SaveToFile(SaveDialog.FileName);
    FFileName := SaveDialog.FileName;
    FTextChanged := False;
  end;
end;

procedure TEditFrame.Search;
begin
  if InputQuery('Find text', 'Search text:', FSearchText) then
  begin
    SearchText(MemoEdit, FSearchText)
  end;
end;

procedure TEditFrame.SearchNext;
begin
  if FSearchText <> '' then
  begin
    SearchText(MemoEdit, FSearchText, MemoEdit.SelStart + MemoEdit.SelLength);
  end
end;

procedure TEditFrame.SearchText(AMemo: TMemo; AText: string;
  AStartPos: Integer);
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
  end;
end;

procedure TEditFrame.SelectAll;
begin
  if MemoEdit.Focused then MemoEdit.SelectAll;
end;

procedure TEditFrame.SetLineNumberVisible(const Value: Boolean);
begin
  if FLineNumberVisible <> Value then
  begin
    FLineNumberVisible := Value;
    PaintBoxLine.Visible := Value;
  end;
end;

procedure TEditFrame.SetReadOnly(const Value: Boolean);
begin
  if MemoEdit.ReadOnly <> Value then
  begin
    MemoEdit.ReadOnly := Value;
  end;
end;

{ TMemo }

procedure TMemo.CNCommand(var Message: TWMCommand);
begin
  case Message.NotifyCode of
    EN_VSCROLL: if (FPaintBoxLine <> nil) and FPaintBoxLine.Visible then FPaintBoxLine.Invalidate;
  end;
  inherited;
end;

procedure TMemo.WMVScroll(var Msg: TWMHScroll);
begin
  if (FPaintBoxLine <> nil) and FPaintBoxLine.Visible then FPaintBoxLine.Invalidate;
  inherited;
end;

end.
