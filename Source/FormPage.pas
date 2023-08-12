unit FormPage;

{$MODE Delphi}

interface

uses
  XPConfig,
  SysUtils, Variants, Classes, Graphics, IntfGraphics, GraphType,
  Controls, Forms, Dialogs, ExtCtrls;

type
  TPageForm = class(TForm)
    PaintPage0: TPaintBox;
    PaintPage1: TPaintBox;
    PaintPage2: TPaintBox;
    PaintPage3: TPaintBox;
    procedure PaintPage0Paint(Sender: TObject);
    procedure PaintPage1Paint(Sender: TObject);
    procedure PaintPage2Paint(Sender: TObject);
    procedure PaintPage3Paint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FBitmaps: array[0..3] of TBitmap;
    FWBitmap: TBitmap;
    procedure PaintPage(APaintBox: TPaintBox; APage: Integer);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PageForm: TPageForm;

implementation

{$R *.lfm}

uses
  XPVideo, FormMain;

procedure TPageForm.FormCreate(Sender: TObject);
var
  n: Integer;
begin
  for n := 0 to 3 do
  begin
    FBitmaps[n] := TBitmap.Create;
    FBitmaps[n].PixelFormat := pf32bit;
    FBitmaps[n].SetSize(256, 256);
  end;
  FWBitmap := TBitmap.Create;
  FWBitmap.PixelFormat := pf32bit;
  FWBitmap.SetSize(512, 256);
  ControlStyle := ControlStyle + [csOpaque];
end;

procedure TPageForm.FormDestroy(Sender: TObject);
var
  n: Integer;
begin
  for n := 0 to 3 do FBitmaps[n].Free;
  FWBitmap.Free;
end;

procedure TPageForm.PaintPage(APaintBox: TPaintBox; APage: Integer);
const
  Addrs: array[0..9, 0..3] of Integer = (
    (0, 0, 0, 0),
    (0, 0, 0, 0),
    (0, 0, 0, 0),
    (0, 0, 0, 0),
    (0, 0, 0, 0),
    (0, $8000, $10000, $18000),
    (0, $8000, $10000, $18000),
    (0, $80, $10000, $10080),
    (0, $10000, 0, $10000),
    (0, 0, 0, 0)
  );
var
  vdo: TXPVideo;
  p: PBGRAArray;
  c, d, e: TBGRAEntry;
  x, y, sx, ad, mode: Integer;
  b: Byte;
  bm: TBitmap;
  raw: TRawImage;
begin
  vdo := MainForm.Compiler.Debug.Video;
  mode := vdo.StartScreen;
  ad := Addrs[mode, APage];
  if mode = 6 then
    bm := FWBitmap
  else bm := FBitmaps[APage];
  bm.BeginUpdate;
  try
    raw := bm.RawImage;
    case mode of
      5:
      for y := 0 to 255 do
      begin
        p := PBGRAArray(raw.GetLineStart(y));
        for x := 0 to 255 do
        begin
          b := vdo.Memory[ad + (x shr 1)];
          sx := ((x and 1) shl 2) xor 4;
          p[x] := vdo.Pals[(b shr sx) and 15].BGRA;
        end;
        Inc(ad, 128);
      end;
      6:
      begin
        for y := 0 to 255 do
        begin
          p := PBGRAArray(raw.GetLineStart(y));
          for x := 0 to 511 do
          begin
            b := vdo.Memory[ad + (x shr 2)];
            sx := 6 - ((x and 3) shl 1);
            p[x] := vdo.Pals[(b shr sx) and 3].BGRA;
          end;
          Inc(ad, 128);
        end;
        ResizeBitmap2(bm, FBitmaps[APage], 256, 256);
      end;
      7:
      for y := 0 to 255 do
      begin
        p := PBGRAArray(raw.GetLineStart(y));
        for x := 0 to 255 do
        begin
          b := vdo.Memory[ad + (x shr 1)];
          sx := ((x and 1) shl 2) xor 4;
          p[x] := vdo.Pals[(b shr sx) and 15].BGRA;
        end;
        Inc(ad, 256);
      end;
      8:
      for y := 0 to 255 do
      begin
        p := PBGRAArray(raw.GetLineStart(y));
        for x := 0 to 255 do
        begin
          b := vdo.Memory[ad + x];
          p[x].BGRA := TXPVideo.Screen8BGRA[b];
        end;
        Inc(ad, 256);
      end;
    end;
  finally
    bm.EndUpdate;
  end;
  APaintBox.Canvas.Draw(0, 0, FBitmaps[APage]);
end;

procedure TPageForm.PaintPage0Paint(Sender: TObject);
begin
  PaintPage(TPaintBox(Sender), 0);
end;

procedure TPageForm.PaintPage1Paint(Sender: TObject);
begin
  PaintPage(TPaintBox(Sender), 1);
end;

procedure TPageForm.PaintPage2Paint(Sender: TObject);
begin
  PaintPage(TPaintBox(Sender), 2);
end;

procedure TPageForm.PaintPage3Paint(Sender: TObject);
begin
  PaintPage(TPaintBox(Sender), 3);
end;

end.
