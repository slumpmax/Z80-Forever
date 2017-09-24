unit form_theme;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LCLType, LCLIntf, LMessages, frame_main;

const
    LM_THEMECONTROLCLICK = LM_USER;
type

  TPNGImage = TPortableNetworkGraphic;
  TThemeForm = class;

  { TThemeControl }

  { TThemePos }

  TThemePos = record
    X, Y: Integer;
    procedure Assign(APos: TThemePos);
    procedure SetPos(AX, AY: Integer);
  end;
  TThemeImageList = class

  end;

  TThemeControlState = (ttcNormal, ttcHover, ttcDown, ttcDisable);
  TThemeControl = class
  private
    FParent: TThemeForm;
    FState: TThemeControlState;
    FName: string;
    FNoBorder, FNoHover: Boolean;
    FOnClick, FOnDblClick: TNotifyEvent;
    FOnMouseDown, FOnMouseUp: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    procedure DoClick;
    procedure DoDblClick;
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer);
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SetNoBorder(AValue: Boolean);
    procedure SetParent(AValue: TThemeForm);
    procedure SetState(AValue: TThemeControlState);
  public
    Picture: TRasterImage;
    CopyWidth, CopyHeight: Integer;
    Left, Top, Width, Height, MarginTop, MarginBottom: Integer;
    NormalPos, HoverPos, DownPos, DisablePos: TThemePos;
    Visible, Enabled, CanFocus: Boolean;
    constructor Create(AParent: TThemeForm; AName: string; APic: TRasterImage; ACopyWidth, ACopyHeight: Integer);
    destructor Destroy; override;
    function Hit(X, Y: Integer): Boolean;
    procedure Click;
    procedure PaintTo(Canvas: TCanvas); virtual;
    procedure Paint;
    procedure Hide;
    procedure Show;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
    procedure SetPos(ALeft, ATop: Integer);
    procedure SetSize(AWidth, AHeight: Integer);
    procedure SetMargin(ATop, ABottom: Integer);
    property Name: string read FName write FName;
    property NoBorder: Boolean read FNoBorder write SetNoBorder;
    property NoHover: Boolean read FNoHover write FNoHover;
    property Parent: TThemeForm read FParent write SetParent;
    property State: TThemeControlState read FState write SetState;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
  end;

  { TThemeLabel }

  TThemeLabel = class(TThemeControl)
  private
    FCaption: string;
    FFont: TFont;
    procedure SetCaption(AValue: string);
  public
    constructor Create(AParent: TThemeForm; AName: string; ACaption: string = '');
    procedure PaintTo(ACanvas: TCanvas); override;
    property Caption: string read FCaption write SetCaption;
    property Font: TFont read FFont;
  end;

  { TThemeControlList }

  TThemeControlList = class(TList)
  private
    function GetControls(AIndex: Integer): TThemeControl;
  public
    constructor Create;
    procedure Clear; override;
    procedure Delete(AControl: TThemeControl);
    function Hit(X, Y: Integer): TThemeControl;
    property Controls[AIndex: Integer]: TThemeControl read GetControls; default;
  end;

  { TThemeForm }

  TThemeForm = class(TForm)
    ClickTimer: TTimer;
  var
    AppProps: TApplicationProperties;
    procedure AppPropsRestore(Sender: TObject);
    procedure ClickTimerTimer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer
      );
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FFrame, FBackFrame: TFrame;
    FBackImage: TImage;
    FBackBitmap, FThemeBitmap: TBitmap;
    FThemeControls: TThemeControlList;
    FHoverControl, FDownControl, FUpControl, FClickControl: TThemeControl;
    FDownTime: TDateTime;
    FThemePic, FIconPic: TPNGImage;
    FBarLabel: TThemeLabel;
    FButtonClose, FButtonMinimize, FButtonMaximize, FIconSys: TThemeControl;
    FBarLeft, FBarRight, FBarTop, FBarBottom: TThemeControl;
    FMoveIndex, FDblClickTime, FClickCount: Integer;
    FMoveX, FMoveY, FMoveWidth, FMoveHeight: Integer;
    FRegionChanged: Boolean;
    FLastState: TWindowState;
    procedure ResizeBegin(X, Y: Integer);
    procedure ResizeEnd;
    procedure FBarTopMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure FBarTopMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FBarTopMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure FBarBottomMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure FBarBottomMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FBarLeftMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure FBarLeftMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FBarRightMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure FBarRightMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FButtonMinimizeClick(Sender: TObject);
    procedure FButtonMaximizeClick(Sender: TObject);
    procedure FButtonCloseClick(Sender: TObject);
    procedure FIconSysClick(Sender: TObject);
    procedure FBarTopDblClick(Sender: TObject);
    procedure LMThemeControlClick(var msg: TMsg); message LM_THEMECONTROLCLICK;
    { private declarations }
  public
    property DblClickTime: Integer read FDblClickTime write FDblClickTime;
    property ThemeControls: TThemeControlList read FThemeControls;
    { public declarations }
  end;

var
  ThemeForm: TThemeForm;

implementation

{$R *.lfm}

{ TThemeControlList }

function TThemeControlList.GetControls(AIndex: Integer): TThemeControl;
begin
  Result := TThemeControl(inherited Items[AIndex]);
end;

constructor TThemeControlList.Create;
begin
  inherited Create;
end;

procedure TThemeControlList.Clear;
var
  con: TThemeControl;
  n: Integer;
begin
  n := Count;
  while n > 0 do
  begin
    Dec(n);
    con := Items[n];
    Items[n] := nil;
    con.Free;
  end;
  inherited Clear;
end;

procedure TThemeControlList.Delete(AControl: TThemeControl);
var
  n: Integer;
begin
  n := IndexOf(AControl);
  if n > 0 then inherited Delete(n);
end;

function TThemeControlList.Hit(X, Y: Integer): TThemeControl;
var
  n: Integer;
begin
  Result := nil;
  n := Count;
  while (n > 0) and (Result = nil) do
  begin
    Dec(n);
    if Controls[n].Hit(X, Y) then Result := Controls[n];
  end;
end;

{ TThemeLabel}

procedure TThemeLabel.SetCaption(AValue: string);
begin
  if FCaption <> AValue then
  begin
    FCaption := AValue;
    Width := Parent.Canvas.TextWidth(FCaption);
    Height := Parent.Canvas.TextHeight(FCaption);
  end;
end;

constructor TThemeLabel.Create(AParent: TThemeForm; AName, ACaption: string);
begin
  inherited Create(AParent, AName, nil, 0, 0);
  FFont := TFont.Create;
  FFont.Assign(AParent.Font);
  FNoHover := True;
  FCaption := '';
  SetCaption(ACaption);
end;

procedure TThemeLabel.PaintTo(ACanvas: TCanvas);
begin
  ACanvas.Font.Assign(FFont);
  ACanvas.Brush.Style := bsClear;
  ACanvas.TextOut(Left, Top, FCaption);
end;

{ TThemePos }

procedure TThemePos.Assign(APos: TThemePos);
begin
  X := APos.X;
  Y := APos.Y;
end;

procedure TThemePos.SetPos(AX, AY: Integer);
begin
  X := AX;
  Y := AY;
end;

{ TThemeControl }

procedure TThemeControl.DoClick;
begin
  if Assigned(FOnClick) then FOnClick(Self);
end;

procedure TThemeControl.DoDblClick;
begin
  if Assigned(FOnDblClick) then FOnDblClick(Self);
end;

procedure TThemeControl.DoMouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then FOnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TThemeControl.DoMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMove) then FOnMouseMove(Self, Shift, X, Y);
end;

procedure TThemeControl.DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseUp) then FOnMouseUp(Self, Button, Shift, X, Y);
end;

procedure TThemeControl.SetNoBorder(AValue: Boolean);
begin
  if FNoBorder <> AValue then
  begin
    FNoBorder := AValue;
    if FNoBorder then
      Dec(Height, MarginTop + MarginBottom)
    else Inc(Height, MarginTop + MarginBottom)
  end;
end;

procedure TThemeControl.SetParent(AValue: TThemeForm);
begin
  if FParent <> AValue then
  begin
    if FParent <> nil then FParent.FThemeControls.Delete(Self);
    FParent := AValue;
    if FParent <> nil then FParent.FThemeControls.Add(Self);
  end;
end;

procedure TThemeControl.SetState(AValue: TThemeControlState);
begin
  if NoHover then
  case AValue of
    ttcHover, ttcDown: AValue := ttcNormal;
  end;
  if FState <> AValue then
  begin
    FState := AValue;
    Paint;
  end;
end;

constructor TThemeControl.Create(AParent: TThemeForm; AName: string; APic: TRasterImage;
  ACopyWidth, ACopyHeight: Integer);
begin
  inherited Create;
  FName := AName;
  Picture := APic;
  CopyWidth := ACopyWidth;
  CopyHeight := ACopyHeight;
  Width := ACopyWidth;
  Height := ACopyHeight;
  Left := 0;
  Top := 0;
  MarginTop := 0;
  MarginBottom := 0;
  NormalPos.SetPos(0, 0);
  HoverPos.SetPos(0, 0);
  DownPos.SetPos(0, 0);
  DisablePos.SetPos(0, 0);
  Visible := True;
  Enabled := True;
  FNoBorder := False;
  FNoHover := False;
  CanFocus := True;
  State := ttcNormal;
  FOnClick := nil;
  FOnDblClick := nil;
  FOnMouseDown := nil;
  FOnMouseMove := nil;
  FOnMouseUp := nil;
  FParent := nil;
  SetParent(AParent);
end;

destructor TThemeControl.Destroy;
begin
  SetParent(nil);
  inherited Destroy;
end;

function TThemeControl.Hit(X, Y: Integer): Boolean;
begin
  Result := CanFocus and Visible and (X >= Left) and (X < Left + Width) and (Y >= Top) and (Y < Top + Height);
end;

procedure TThemeControl.Click;
begin
  DoClick;
end;

procedure TThemeControl.PaintTo(Canvas: TCanvas);
var
  mt, md: Integer;
  p: TThemePos;
  bm: TBitmap;
begin
  if not Visible then Exit;
  case State of
    ttcHover: p := HoverPos;
    ttcDown: p := DownPos;
    ttcDisable: p := DisablePos;
  else
    p := NormalPos;
  end;
  if (MarginTop > 0) and not FNoBorder then
  begin
    Canvas.CopyRect(Bounds(Left, Top, Width, MarginTop),
      Picture.Canvas, Bounds(p.x, p.y, CopyWidth, MarginTop));
  end;
  if (MarginBottom > 0) and not FNoBorder then
  begin
    Canvas.CopyRect(Bounds(Left, Height - MarginBottom, Width, MarginBottom),
      Picture.Canvas, Bounds(p.x, p.y + CopyHeight - MarginBottom, CopyWidth, MarginBottom));
  end;
  if FNoBorder then
  begin
    mt := 0;
    md := 0;
  end
  else
  begin
    mt := MarginTop;
    md := MarginBottom;
  end;
  bm := TBitmap.Create;
  bm.PixelFormat := pf24bit;
  bm.SetSize(CopyWidth, CopyHeight - MarginTop - MarginBottom);
  bm.Canvas.CopyRect(Bounds(0, 0, bm.Width, bm.Height), Picture.Canvas,
    Bounds(p.X, p.Y + MarginTop, bm.Width, bm.Height));
  Canvas.StretchDraw(Bounds(Left, Top + mt, Width, Height - mt - md), bm);
  bm.Free;
end;

procedure TThemeControl.Paint;
begin
  PaintTo(FParent.Canvas);
end;

procedure TThemeControl.Hide;
begin
  Visible := False;
end;

procedure TThemeControl.Show;
begin
  Visible := True;
end;

procedure TThemeControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  Left := ALeft;
  Top := ATop;
  Width := AWidth;
  Height := AHeight;
end;

procedure TThemeControl.SetPos(ALeft, ATop: Integer);
begin
  Left := ALeft;
  Top := ATop;
end;

procedure TThemeControl.SetSize(AWidth, AHeight: Integer);
begin
  Width := AWidth;
  Height := AHeight;
end;

procedure TThemeControl.SetMargin(ATop, ABottom: Integer);
begin
  MarginTop := ATop;
  MarginBottom := ABottom;
end;

{ TThemeForm }

procedure TThemeForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  TFrameMain(FFrame).FrameCloseQuery(Sender, CanClose);
end;

procedure TThemeForm.AppPropsRestore(Sender: TObject);
begin
  FFrame.Hide;
  try
    WindowState := FLastState;
  finally
    FFrame.Show;
  end;
end;

procedure TThemeForm.ClickTimerTimer(Sender: TObject);
begin
  ClickTimer.Enabled := False;
  FClickControl.DoDblClick;
end;

procedure TThemeForm.FormCreate(Sender: TObject);
var
  rs: TResourceStream;
begin
  DoubleBuffered := True;
  FThemeControls := TThemeControlList.Create;
  rs := TResourceStream.Create(hInstance, 'THEME', 'PNG');
  try
    FThemePic := TPNGImage.Create;
    FThemePic.LoadFromStream(rs);
  finally
    rs.Free;
  end;
  rs := TResourceStream.Create(hInstance, 'ICON16', 'PNG');
  try
    FIconPic := TPNGImage.Create;
    FIconPic.LoadFromStream(rs);
  finally
    rs.Free;
  end;
  FThemeBitmap := TBitmap.Create;
  FThemeBitmap.PixelFormat := pf24bit;
  FThemeBitmap.SetSize(FThemePic.Width, FThemePic.Height);
  FThemeBitmap.Canvas.Draw(0, 0, FThemePic);
  FBarLeft := TThemeControl.Create(Self, 'Left', FThemeBitmap, 7, 120);
  with FBarLeft do
  begin
    NormalPos.SetPos(1, 1);
    DisablePos.SetPos(104, 1);
    SetMargin(28, 7);
    NoHover := True;
    OnMouseDown := FBarLeftMouseDown;
    OnMouseMove := FBarLeftMouseMove;
    OnMouseUp := FBarTopMouseUp;
  end;
  FBarRight := TThemeControl.Create(Self, 'Right', FThemeBitmap, 7, 120);
  with FBarRight do
  begin
    NormalPos.SetPos(94, 1);
    DisablePos.SetPos(197, 1);
    SetMargin(28, 7);
    NoHover := True;
    OnMouseDown := FBarRightMouseDown;
    OnMouseMove := FBarRightMouseMove;
    OnMouseUp := FBarTopMouseUp;
  end;
  FBarTop := TThemeControl.Create(Self, 'Top', FThemeBitmap, 86, 28);
  with FBarTop do
  begin
    NormalPos.SetPos(8, 1);
    DisablePos.SetPos(111, 1);
    SetMargin(7, 0);
    NoHover := True;
    OnMouseDown := FBarTopMouseDown;
    OnMouseMove := FBarTopMouseMove;
    OnMouseUp := FBarTopMouseUp;
    OnDblClick := FBarTopDblClick;
  end;
  FBarBottom := TThemeControl.Create(Self, 'Bottom', FThemeBitmap, 86, 7);
  with FBarBottom do
  begin
    NormalPos.SetPos(8, 114);
    DisablePos.SetPos(111, 114);
    NoHover := True;
    OnMouseDown := FBarBottomMouseDown;
    OnMouseMove := FBarBottomMouseMove;
    OnMouseUp := FBarTopMouseUp;
  end;
  FIconSys := TThemeControl.Create(Self, 'IconSys', FIconPic, 16, 16);
  FIconSys.NoHover := True;
  FButtonMinimize := TThemeControl.Create(Self, 'Minimize', FThemePic, 22, 21);
  with FButtonMinimize do
  begin
    NormalPos.SetPos(208, 119);
    HoverPos.SetPos(231, 119);
    DownPos.SetPos(254, 119);
    DisablePos.SetPos(255, 72);
    SetMargin(2, 0);
  end;
  FButtonMaximize := TThemeControl.Create(Self, 'Maximize', FThemePic, 22, 21);
  with FButtonMaximize do
  begin
    NormalPos.SetPos(208, 49);
    HoverPos.SetPos(231, 49);
    DownPos.SetPos(255, 49);
    DisablePos.SetPos(208, 72);
    SetMargin(2, 0);
  end;
  FButtonClose := TThemeControl.Create(Self, 'Close', FThemePic, 33, 21);
  with FButtonClose do
  begin
    NormalPos.SetPos(206, 3);
    HoverPos.SetPos(206, 26);
    DownPos.SetPos(240, 3);
    DisablePos.SetPos(240, 26);
    SetMargin(2, 0);
  end;
  FBarLabel := TThemeLabel.Create(Self, 'Label', Application.Title);
  FBarLabel.Font.Color := clWhite;
  FBarLabel.Font.Style := [fsBold];
  FBarLabel.CanFocus := False;
  Color := $EFECE7;

  FFrame := TFrameMain.Create(nil);
  ClientWidth := FFrame.Width + FBarLeft.Width + FBarRight.Width;
  ClientHeight := FFrame.Height + FBarTop.Height + FBarBottom.Height;
  FFrame.Align := alNone;
  FFrame.Anchors := [akLeft, akTop];
  FFrame.Left := FBarLeft.Width;
  FFrame.Top := FBarTop.Height;
  FFrame.ParentColor := True;
  //FFrame.Color := FColor;
  FFrame.Parent := Self;
  FBackFrame := TFrame.Create(nil);
  FBackImage := TImage.Create(nil);
  FBackBitmap := TBitmap.Create;
  FBackImage.Picture.Bitmap := TBitmap.Create;
  FBackImage.Picture.Bitmap.PixelFormat := pf24bit;
  FBackImage.Anchors := [akLeft, akTop, akRight, akBottom];
  FBackImage.Parent := FBackFrame;
  FBackFrame.Anchors := [akLeft, akTop, akRight, akBottom];
  FBackFrame.Visible := False;
  FBackFrame.Parent := Self;

  FButtonMinimize.Top := 1;
  FButtonMaximize.Top := 1;
  FButtonClose.Top := 1;
  FIconSys.SetPos(FBarLeft.Width, FIconSys.Top + (FBarTop.Height - FIconSys.Height) div 2);
  FBarLabel.SetPos(FIconSys.Left + FIconSys.Width + 4, FIconSys.Top + (FIconSys.Height - FBarLabel.Height) div 2);

  FButtonMinimize.OnClick := FButtonMinimizeClick;
  FButtonMaximize.OnClick := FButtonMaximizeClick;
  FButtonClose.OnClick := FButtonCloseClick;
  FIconSys.OnClick := FIconSysClick;
  FHoverControl := nil;
  FDownControl := nil;
  FUpControl := nil;
  FDblClickTime := 300;
  FClickCount := 0;
  FRegionChanged := True;
  FLastState := wsNormal;

  if Width > Screen.WorkAreaWidth - 16 then Width := Screen.WorkAreaWidth - 16;
  if Height > Screen.WorkAreaHeight - 16 then Height := Screen.WorkAreaHeight - 16;
  Left := (Screen.WorkAreaWidth - Width) div 2;
  Top := (Screen.WorkAreaHeight - Height) div 2;
end;

procedure TThemeForm.FormDestroy(Sender: TObject);
begin
  FBackImage.Free;
  FBackFrame.Free;
  FFrame.Free;
  FThemeControls.Free;
  FThemeBitmap.Free;
  FThemePic.Free;
  FIconPic.Free;
end;

procedure TThemeForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  t: Integer;
begin
  FDownControl := FThemeControls.Hit(X, Y);
  if FDownControl <> nil then
  begin
    if FDownControl.NoHover then
      FDownControl.State := ttcNormal
    else FDownControl.State := ttcDown;
    FDownControl.DoMouseDown(Button, Shift, X, Y);
    if (Button = mbLeft) and (FDownControl = FUpControl) then
    begin
      t := Round((Now - FDownTime) * 24 * 60 * 60 * 1000);
      if t < FDblClickTime then
      begin
        Inc(FClickCount);
        if FClickCount = 1 then
        begin
          FClickControl := FDownControl;
          ClickTimer.Enabled := True;
        end;
      end
      else FClickCount := 0;
    end
    else FClickCount := 0;
  end;
  FUpControl := nil;
  FDownTime := Now;
end;

procedure TThemeForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  hcon: TThemeControl;
begin
  if FDownControl <> nil then
  begin
    if FDownControl.Hit(X, Y) and not FDownControl.NoHover then
      FDownControl.State := ttcDown
    else FDownControl.State := ttcNormal;
    FDownControl.DoMouseMove(Shift, X, Y);
  end
  else
  begin
    hcon := FThemeControls.Hit(X, Y);
    if FHoverControl <> hcon then
    begin
      if FHoverControl <> nil then FHoverControl.State := ttcNormal;
      FHoverControl := hcon;
      if FHoverControl <> nil then
      begin
        if FHoverControl.NoHover then
          FHoverControl.State := ttcNormal
        else FHoverControl.State := ttcHover;
       end
    end;
    Cursor := crDefault;
    if FHoverControl <> nil then
    begin
      FHoverControl.DoMouseMove(Shift, X, Y);
    end
  end
end;

procedure TThemeForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  t: Integer;
begin
  FUpControl := nil;
  if FDownControl <> nil then
  begin
    FDownControl.DoMouseUp(Button, Shift, X, Y);
    if FDownControl.Hit(X, Y) then
    begin
      FDownControl.State := ttcHover;
      if Button = mbLeft then
      begin
        FDownControl.DoClick;
        t := Round((Now - FDownTime) * 24 * 60 * 60 * 1000);
        if t < FDblClickTime then
        begin
          FUpControl := FDownControl;
          FDownTime := Now;
        end;
      end;
    end
    else FDownControl.State := ttcNormal;
    FDownControl := nil;
    Cursor := crDefault;
  end;
end;

procedure TThemeForm.FormPaint(Sender: TObject);
var
  rgn: HRGN;
  n: Integer;
begin
  for n := 0 to FThemeControls.Count - 1 do FThemeControls[n].Paint;
  if FRegionChanged then
  begin
    if WindowState = wsMaximized then
      rgn := CreateRectRgn(0, 0, Width + 1, Height)
    else rgn := CreateRoundRectRgn(0, 0, Width + 1, Height + 1, 9, 9);
    SetWindowRgn(Handle, rgn, True);
    FRegionChanged := False;
  end;
end;

procedure TThemeForm.FormResize(Sender: TObject);
begin
  if WindowState = wsMaximized then
  begin
    FBarLeft.Hide;
    FBarRight.Hide;
    FBarBottom.Hide;
    FBarTop.NoBorder := True;
    FButtonMinimize.NoBorder := True;
    FButtonMaximize.NoBorder := True;
    FButtonClose.NoBorder := True;
    FBarTop.Left := 0;
    FBarTop.Width := ClientWidth;
    FButtonClose.Top := 0;
    FButtonMaximize.Top := 0;
    FButtonMinimize.Top := 0;
    FButtonClose.Left := ClientWidth - FBarRight.Width - FButtonClose.Width;
    FButtonMaximize.Left := FButtonClose.Left - FButtonMaximize.Width;
    FButtonMinimize.Left := FButtonMaximize.Left - FButtonMinimize.Width;
    FFrame.SetBounds(0, FBarTop.Height, ClientWidth, ClientHeight - FBarTop.Height);
  end
  else
  begin
    FBarLeft.Show;
    FBarRight.Show;
    FBarBottom.Show;
    FBarTop.NoBorder := False;
    FButtonMinimize.NoBorder := False;
    FButtonMaximize.NoBorder := False;
    FButtonClose.NoBorder := False;
    FBarTop.Left := FBarLeft.Width;
    FBarTop.Width := ClientWidth - FBarLeft.Width - FBarRight.Width;
    FBarBottom.Left := FBarTop.Left;
    FBarBottom.Width := FBarTop.Width;
    FBarBottom.Top := ClientHeight - FBarBottom.Height;
    FBarLeft.Height := ClientHeight;
    FBarRight.Left := ClientWidth - FBarRight.Width;
    FBarRight.Height := ClientHeight;
    FButtonClose.Top := 1;
    FButtonMaximize.Top := 1;
    FButtonMinimize.Top := 1;
    FButtonClose.Left := FBarRight.Left - FButtonClose.Width;
    FButtonMaximize.Left := FButtonClose.Left - FButtonMaximize.Width;
    FButtonMinimize.Left := FButtonMaximize.Left - FButtonMinimize.Width;
    FFrame.SetBounds(FBarLeft.Width, FBarTop.Height,
      ClientWidth - FBarLeft.Width - FBarRight.Width, ClientHeight - FBarTop.Height - FBarBottom.Height);
  end;
  FIconSys.Top := (FBarTop.Height - FIconSys.Height) div 2;
  FBarLabel.Top := (FBarTop.Height - FBarLabel.Height) div 2;
  FRegionChanged := True;
  Repaint;
end;

procedure TThemeForm.FormShow(Sender: TObject);
begin
  TFrameMain(FFrame).FrameShow(Sender);
end;

procedure TThemeForm.ResizeBegin(X, Y: Integer);
var
  dc: HDC;
begin
  FMoveX := X;
  FMoveY := Y;
  FMoveWidth := Width;
  FMoveHeight := Height;
  FBackFrame.SetBounds(FFrame.Left, FFrame.Top, FFrame.Width, FFrame.Height);
  FBackImage.SetBounds(0, 0, FFrame.Width, FFrame.Height);
  FBackImage.Picture.Bitmap.SetSize(FFrame.Width, FFrame.Height);
  dc := GetDC(0);
  try
    FBackBitmap.LoadFromDevice(dc);
    FBackImage.Canvas.CopyRect(Bounds(0, 0, FFrame.Width, FFrame.Height), FBackBitmap.Canvas, Bounds(Left + FFrame.Left, Top + FFrame.Top, FFrame.Width, FFrame.Height));
  finally
    DeleteDC(dc);
  end;
  FBackFrame.Show;
  FFrame.Hide;
end;

procedure TThemeForm.ResizeEnd;
begin
  FFrame.Show;
  FBackFrame.Hide;
end;

procedure TThemeForm.FBarLeftMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if Y < FBarLeft.MarginTop then
      FMoveIndex := 1
    else if Y >= Height - FBarLeft.MarginBottom then
      FMoveIndex := 2
    else FMoveIndex := 0;
    ResizeBegin(X, Y);
  end;
end;

procedure TThemeForm.FBarLeftMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if ssLeft in Shift then
    case FMoveIndex of
      1:
      begin
        Cursor := crSizeNW;
        SetBounds(Left + X - FMoveX, Top + Y - FMoveY, Width - X + FMoveX, Height - Y + FMoveY);
      end;
      2:
      begin
        Cursor := crSizeSW;
        SetBounds(Left + X - FMoveX, Top, Width - X + FMoveX, FMoveHeight + Y - FMoveY);
      end
    else
      Cursor := crSizeW;
      SetBounds(Left + X - FMoveX, Top, Width - X + FMoveX, Height);
    end
  else if Y < FBarLeft.MarginTop then
    Cursor := crSizeNW
  else if Y >= Height - FBarLeft.MarginBottom then
    Cursor := crSizeSW
  else Cursor := crSizeW;
end;

procedure TThemeForm.FBarRightMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if Y < FBarRight.MarginTop then
      FMoveIndex := 1
    else if Y >= Height - FBarRight.MarginBottom then
      FMoveIndex := 2
    else FMoveIndex := 0;
    ResizeBegin(X, Y);
  end;
end;

procedure TThemeForm.FBarRightMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if ssLeft in Shift then
    case FMoveIndex of
      1:
      begin
        SetBounds(Left, Top + Y - FMoveY, FMoveWidth + X - FMoveX, Height - Y + FMoveY);
        Cursor := crSizeNE;
      end;
      2:
      begin
        SetBounds(Left, Top, FMoveWidth + X - FMoveX, FMoveHeight + Y - FMoveY);
        Cursor := crSizeSE;
      end
    else
      SetBounds(Left, Top, FMoveWidth + X - FMoveX, Height);
      Cursor := crSizeE;
    end
  else if Y < FBarRight.MarginTop then
    Cursor := crSizeNE
  else if Y >= Height - FBarRight.MarginBottom then
    Cursor := crSizeSE
  else Cursor := crSizeE;
end;

procedure TThemeForm.FBarBottomMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then ResizeBegin(X, Y);
end;

procedure TThemeForm.FBarBottomMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then SetBounds(Left, Top, Width, FMoveHeight + Y - FMoveY);
  Cursor := crSizeS;
end;

procedure TThemeForm.FBarTopMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if Y >= FBarBottom.Height then
      FMoveIndex := 1
    else FMoveIndex := 0;
    ResizeBegin(X, Y);
  end;
end;

procedure TThemeForm.FBarTopMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
    if FMoveIndex > 0 then
    begin
      Cursor := crDefault;
      SetBounds(Left + X - FMoveX, Top + Y - FMoveY, Width, Height);
    end
    else
    begin
      Cursor := crSizeN;
      SetBounds(Left, Top + Y - FMoveY, Width, Height - Y + FMoveY);
    end
  else if Y >= FBarBottom.Height then
    Cursor := crDefault
  else Cursor := crSizeN;
end;

procedure TThemeForm.FBarTopMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then ResizeEnd;
end;

procedure TThemeForm.FButtonMinimizeClick(Sender: TObject);
begin
  FLastState := WindowState;
  Application.Minimize;
end;

procedure TThemeForm.FButtonMaximizeClick(Sender: TObject);
begin
  FFrame.Hide;
  try
    if WindowState = wsMaximized then
      WindowState := wsNormal
    else
    begin
      WindowState := wsMaximized;
      SetBounds(Screen.WorkAreaLeft, Screen.WorkAreaTop, Screen.WorkAreaWidth, Screen.WorkAreaHeight);
    end;
    FLastState := WindowState;
  finally
    FFrame.Show;
  end;
end;

procedure TThemeForm.FButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TThemeForm.FIconSysClick(Sender: TObject);
begin
  Perform(LM_SYSCOMMAND, SC_CLOSE, 0);
end;

procedure TThemeForm.FBarTopDblClick(Sender: TObject);
begin
  FButtonMaximize.Click;
end;

procedure TThemeForm.LMThemeControlClick(var msg: TMsg);
begin
  FClickControl.DoDblClick;
end;

end.

