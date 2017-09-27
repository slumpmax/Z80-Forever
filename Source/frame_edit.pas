unit frame_edit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, LMessages,
  Graphics, Windows;

const
  LM_PAINTLINE = LM_USER;

type

  { TMemo }

  TMemo  = class(StdCtrls.TMemo)
  private
    procedure CNCommand(var Message: TLMCommand); message CN_COMMAND;
    procedure LMVScroll(var Msg: TLMHScroll); message LM_VSCROLL;
  end;

  { TFrameEdit }

  TFrameEdit = class(TFrame)
    MemoEdit: TMemo;
    PaintBoxLine: TPaintBox;
    procedure MemoEditChange(Sender: TObject);
    procedure MemoEditClick(Sender: TObject);
    procedure MemoEditDblClick(Sender: TObject);
    procedure MemoEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure MemoEditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxLinePaint(Sender: TObject);
  private
    FFileName: string;
    FChanged, FOverwrited: Boolean;
    FSelStart: Integer;
    FOnChanged: TNotifyEvent;
    procedure DoChanged;
    procedure LMPaintLine(var msg: TMsg); message LM_PAINTLINE;
    procedure SetOverwrited(AValue: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear;
    procedure LoadFromFile(AFileName: string = '');
    procedure SaveToFile(AFileName: string = '');
    property Changed: Boolean read FChanged write FChanged;
    property Overwrited: Boolean read FOverwrited write SetOverwrited;
    property FileName: string read FFileName write FFileName;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  { TFrameEditList }

  TFrameEditList = class(TList)
  private
    function GetFrames(AIndex: Integer): TFrameEdit;
  public
    procedure Clear; override;
    function AddFrame: TFrameEdit;
    function IndexOf(AFrame: TFrameEdit): Integer; overload;
    function IndexOf(AFileName: string): Integer; overload;
    property Frames[AIndex: Integer]: TFrameEdit read GetFrames; default;
  end;


implementation

{$R *.lfm}

uses
  main_routine;

{ TFrameEditList }

function TFrameEditList.GetFrames(AIndex: Integer): TFrameEdit;
begin
  Result := TFrameEdit(inherited Items[AIndex]);
end;

procedure TFrameEditList.Clear;
var
  frm: TFrameEdit;
  n: Integer;
begin
  n := Count;
  while n > 0 do
  begin
    Dec(n);
    frm := inherited Items[n];
    inherited Items[n] := nil;
    frm.Free;
  end;
  inherited Clear;
end;

function TFrameEditList.AddFrame: TFrameEdit;
begin
  Result := TFrameEdit.Create(nil);
  Add(Result);
end;

function TFrameEditList.IndexOf(AFrame: TFrameEdit): Integer;
var
  n: Integer;
begin
  Result := -1;
  n := Count;
  while (n > 0) and (Result < 0) do
  begin
    Dec(n);
    if AFrame = Frames[n] then Result := n;
  end;
end;

function TFrameEditList.IndexOf(AFileName: string): Integer;
var
  n: Integer;
begin
  Result := -1;
  n := Count;
  while (n > 0) and (Result < 0) do
  begin
    Dec(n);
    if SameText(AFileName, Frames[n].FileName) then Result := n;
  end;
end;

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

{ TFrameEdit }

procedure TFrameEdit.PaintBoxLinePaint(Sender: TObject);
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

procedure TFrameEdit.DoChanged;
begin
  if Assigned(FOnChanged) then FOnChanged(Self);
end;

procedure TFrameEdit.LMPaintLine(var msg: TMsg);
begin
  PaintBoxLine.Invalidate;
end;

procedure TFrameEdit.SetOverwrited(AValue: Boolean);
begin
  if FOverwrited = AValue then Exit;
  FOverwrited := AValue;
  DoChanged;
end;

constructor TFrameEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FChanged := False;
  FOverwrited := True;
end;

procedure TFrameEdit.Clear;
begin
  MemoEdit.Clear;
  FFileName := '';
  FChanged := False;
  PaintBoxLine.Invalidate;
  DoChanged;
end;

procedure TFrameEdit.LoadFromFile(AFileName: string);
begin
  if AFileName = '' then AFileName := FFileName;
  MemoEdit.Lines.LoadFromFile(AFileName);
  FFileName := AFileName;
  FChanged := False;
  DoChanged;
end;

procedure TFrameEdit.SaveToFile(AFileName: string);
begin
  if AFileName = '' then AFileName := FFileName;
  MemoEdit.Lines.SaveToFile(AFileName);
  FFileName := AFileName;
  FChanged := False;
  DoChanged;
end;

procedure TFrameEdit.MemoEditChange(Sender: TObject);
begin
  FChanged := True;
  DoChanged;
end;

procedure TFrameEdit.MemoEditClick(Sender: TObject);
begin
  FSelStart := TMemo(Sender).SelStart;
end;

procedure TFrameEdit.MemoEditDblClick(Sender: TObject);
begin
  SelectWord(TMemo(Sender), FSelStart);
end;

procedure TFrameEdit.MemoEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_SHIFT, VK_CONTROL, VK_MENU:;
  else
    PaintBoxLine.Invalidate;
  end;
end;

procedure TFrameEdit.MemoEditMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  PaintBoxLine.Invalidate;
end;

end.

