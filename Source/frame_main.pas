unit frame_main;

interface

uses
  MXCompiler, Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ComCtrls,
  Dialogs, LCLType, ActnList, ExtCtrls, Graphics, LMessages,
  Windows, frame_edit;

const
  LM_PAINTLINE = LM_USER;

type

  { TFrameMain }

  TFrameMain = class(TFrame)
    ActionClose: TAction;
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
    ButtonClose: TButton;
    ButtonSave: TButton;
    ButtonSaveAs: TButton;
    ButtonNew: TButton;
    ButtonSearch: TButton;
    ButtonGoto: TButton;
    CheckOverwrite: TCheckBox;
    DialogOpenSource: TOpenDialog;
    DialogAssemble: TSaveDialog;
    EditFileName: TEdit;
    ImageListPage: TImageList;
    MemoOutput: TMemo;
    PageMain: TPageControl;
    Panel1: TPanel;
    DialogSave: TSaveDialog;
    TabOutput: TTabSheet;
    TabSheet1: TTabSheet;
    procedure ActionAssembleExecute(Sender: TObject);
    procedure ActionBrowseExecute(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionDeleteLineExecute(Sender: TObject);
    procedure ActionGotoExecute(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure ActionSaveAsExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionSearchExecute(Sender: TObject);
    procedure ActionSearchNextExecute(Sender: TObject);
    procedure ActionSelectAllExecute(Sender: TObject);
    procedure PageMainChange(Sender: TObject);
  private
    Compiler: TMXCompiler;
    FEditFrames: TFrameEditList;
    FSearchText, FCompileFile: string;
    FNewNumber: Integer;
    procedure UpdateButton;
    procedure EditFrameChanged(Sender: TObject);
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

uses
  main_routine;

{ TFrameMain }

procedure TFrameMain.ActionSaveExecute(Sender: TObject);
var
  frm: TFrameEdit;
  ext: string;
begin
  frm := FEditFrames[PageMain.PageIndex - 1];
  ext := UpperCase(ExtractFileExt(frm.FileName));
  if (Copy(frm.FileName, 1, 1) = '<') or (ext <> '.ASM') then
    ActionSaveAs.Execute
  else frm.SaveToFile(frm.FileName);
end;

procedure TFrameMain.ActionSearchExecute(Sender: TObject);
var
  frm: TFrameEdit;
  memo: TMemo;
begin
  if InputQuery('Find text', 'Search text:', FSearchText) then
  begin
    if PageMain.TabIndex = 0 then
      memo := MemoOutput
    else
    begin
      frm := FEditFrames[PageMain.TabIndex - 1];
      memo := frm.MemoEdit;
    end;
    SearchText(memo, FSearchText, memo.SelStart)
  end;
end;

procedure TFrameMain.ActionSearchNextExecute(Sender: TObject);
var
  frm: TFrameEdit;
  memo: TMemo;
begin
  if FSearchText <> '' then
  begin
    if PageMain.TabIndex = 0 then
      memo := MemoOutput
    else
    begin
      frm := FEditFrames[PageMain.TabIndex - 1];
      memo := frm.MemoEdit;
    end;
    SearchText(memo, FSearchText, memo.SelStart + memo.SelLength);
  end
end;

procedure TFrameMain.ActionSelectAllExecute(Sender: TObject);
var
  frm: TFrameEdit;
begin
  if PageMain.TabIndex = 0 then
  begin
    if MemoOutput.Focused then MemoOutput.SelectAll;
  end
  else
  begin
    frm := FEditFrames[PageMain.TabIndex - 1];
    if frm.MemoEdit.Focused then frm.MemoEdit.SelectAll;
  end;
end;

procedure TFrameMain.ActionSaveAsExecute(Sender: TObject);
var
  frm: TFrameEdit;
begin
  frm := FEditFrames[PageMain.PageIndex - 1];
  if frm.FileName = '' then
    DialogSave.FileName := ''
  else DialogSave.FileName := ChangeFileExt(frm.FileName, '.asm');
  if DialogSave.Execute then frm.SaveToFile(DialogSave.FileName);
end;

procedure TFrameMain.ActionNewExecute(Sender: TObject);
begin
  PageMain.PageIndex := PageMain.PageCount - 1;
  PageMainChange(PageMain);
end;

procedure TFrameMain.ActionAssembleExecute(Sender: TObject);
var
  frm: TFrameEdit;
  fname: string;
  n: Integer;
begin
  frm := FEditFrames[PageMain.PageIndex - 1];
  Compiler.SourceTexts.Clear;
  for n := 0 to FEditFrames.Count - 1 do
  begin
    Compiler.SourceTexts.AddSource(FEditFrames[n].FileName, FEditFrames[n].MemoEdit.Lines);
  end;
  Compiler.Compile(frm.FileName, MemoOutput.Lines);
  FCompileFile := frm.FileName;
  PageMain.PageIndex := 0;
  PageMainChange(PageMain);
  MemoOutput.SelStart := Length(MemoOutput.Text);
  MemoOutput.SelLength := 0;
  if Compiler.Address - Compiler.StartAddress = 0 then Exit;
  fname := ChangeFileExt(frm.FileName, '.' + Compiler.OutputExt);
  if (not frm.Overwrited and FileExists(fname)) or (frm.FileName = '') then
  begin
    DialogAssemble.FileName := fname;
    DialogAssemble.Filter := Format('Output files (*.%s)|*.%s', [Compiler.OutputExt, Compiler.OutputExt]);
    if DialogAssemble.Execute then
    begin
      Compiler.SaveToFile(DialogAssemble.FileName, Compiler.StartAddress, Compiler.Address - 1);
      frm.Overwrited := True;
    end;
  end
  else
  begin
    Compiler.SaveToFile(fname, Compiler.StartAddress, Compiler.Address - 1);
    frm.Overwrited := True;
  end;
end;

procedure TFrameMain.ActionBrowseExecute(Sender: TObject);
var
  frm: TFrameEdit;
  s, ext: string;
  n: Integer;
  do_new: Boolean;
begin
  if DialogOpenSource.Execute then
  begin
    n := FEditFrames.IndexOf(DialogOpenSource.FileName);
    if n >= 0 then
      PageMain.PageIndex := n + 1
    else
    begin
      do_new := True;
      if PageMain.PageIndex > 0 then
      begin
        frm := FEditFrames[PageMain.PageIndex - 1];
        if (Copy(frm.FileName, 1, 1) = '<') and not frm.Changed then do_new := False;
      end;
      if do_new then PageMain.PageIndex := PageMain.PageCount - 1;
    end;
    PageMainChange(PageMain);
    frm := FEditFrames[PageMain.PageIndex - 1];
    if frm.Changed then
    begin
      s := '"' + ExtractFileName(frm.FileName) + '"';
      case MessageDlg(s + ' has changed. Do you want to save changed?', mtWarning, [mbYes, mbNo, mbCancel], 0, mbCancel) of
        mrYes: ActionSave.Execute;
        mrNo: frm.Changed := False;
      end;
    end;
    if not frm.Changed then
    begin
      frm.FileName := DialogOpenSource.FileName;
      ext := UpperCase(ExtractFileExt(frm.FileName));
      if ext = '.ASM' then
      begin
        frm.LoadFromFile;
        PageMain.ActivePage.ImageIndex := 1;
      end
      else
      begin
        Compiler.Decompile(frm.FileName, frm.MemoEdit.Lines);
        frm.Overwrited := False;
        ActionSave.Enabled := True;
        frm.Changed := False;
        if ext = '.COM' then
          PageMain.ActivePage.ImageIndex := 3
        else PageMain.ActivePage.ImageIndex := 2;
      end;
      PageMain.ActivePage.Caption := ExtractFileName(frm.FileName);
      frm.MemoEdit.SetFocus;
      frm.PaintBoxLine.Invalidate;
    end;
  end;
end;

procedure TFrameMain.ActionCloseExecute(Sender: TObject);
var
  frm: TFrameEdit;
  n: Integer;
  s: string;
begin
  n := PageMain.PageIndex - 1;
  frm := FEditFrames[n];
  if frm.Changed then
  begin
    s := '"' + ExtractFileName(frm.FileName) + '"';
    case MessageDlg(s + ' has changed. Do you want to save changed?', mtWarning, [mbYes, mbNo, mbCancel], 0, mbCancel) of
      mrYes: ActionSave.Execute;
      mrNo: frm.Changed := False;
    end;
  end;
  if not frm.Changed then
  begin
    FEditFrames.Delete(n);
    PageMain.Pages[n + 1].Free;
    if n + 1 = PageMain.PageCount - 1 then PageMain.PageIndex := n;
    PageMainChange(PageMain);
  end;
end;

procedure TFrameMain.ActionDeleteLineExecute(Sender: TObject);
var
  frm: TFrameEdit;
begin
  frm := FEditFrames[PageMain.PageIndex - 1];
  if frm.MemoEdit.Focused then frm.MemoEdit.Lines.Delete(frm.MemoEdit.CaretPos.y);
end;

procedure TFrameMain.ActionGotoExecute(Sender: TObject);
var
  frm: TFrameEdit;
  r, nr, n: Integer;
  s: string;
begin
  if InputQuery('Goto Line Number', 'Enter line number:', s) then
  begin
    if PageMain.TabIndex = 0 then
    begin
      PageMain.PageIndex := FEditFrames.IndexOf(FCompileFile) + 1;
      PageMainChange(PageMain);
    end;
    frm := FEditFrames[PageMain.PageIndex - 1];
    r := StrToIntDef(s, frm.MemoEdit.CaretPos.Y + 1);
    if r < 1 then
      r := 1
    else if r > frm.MemoEdit.Lines.Count then r := frm.MemoEdit.Lines.Count;
    n := 0;
    nr := 0;
    Dec(r);
    while nr < r do
    begin
      Inc(n, Length(frm.MemoEdit.Lines[nr]) + 2);
      Inc(nr);
    end;
    frm.MemoEdit.SelStart := n;
    frm.MemoEdit.SelLength := Length(frm.MemoEdit.Lines[r]);
    frm.MemoEdit.SetFocus;
    frm.PaintBoxLine.Invalidate;
  end;
end;

procedure TFrameMain.PageMainChange(Sender: TObject);
var
  frm: TFrameEdit;
  n: Integer;
begin
  if PageMain.ActivePage.Caption = '+' then
  begin
    frm := FEditFrames.AddFrame;
    frm.Align := alClient;
    frm.Parent := PageMain.ActivePage;
    frm.OnChanged := EditFrameChanged;
    frm.MemoEdit.SetFocus;
    frm.FileName := Format('<new_%d>', [FNewNumber]);
    Inc(FNewNumber);
    PageMain.ActivePage.Caption := frm.FileName;
    n := PageMain.PageIndex;
    with PageMain.AddTabSheet do
    begin
      PageIndex := n + 1;
      Caption := '+';
    end;
    frm.MemoEdit.SetFocus;
  end
  else if PageMain.PageIndex > 0 then
  begin
    frm := FEditFrames[PageMain.PageIndex - 1];
    EditFrameChanged(frm);
    frm.MemoEdit.SetFocus;
  end
  else
  begin
    EditFileName.Text := FCompileFile;
    MemoOutput.SetFocus;
  end;
  UpdateButton;
end;

procedure TFrameMain.UpdateButton;
var
  frm: TFrameEdit;
begin
  if PageMain.TabIndex = 0 then
  begin
    ActionSave.Enabled := False;
    ActionSaveAs.Enabled := False;
    ActionBrowse.Enabled := True;
    ActionAssemble.Enabled := False;
    ActionGoto.Enabled := FCompileFile <> '';
    ActionNew.Enabled := True;
    ActionClose.Enabled := False;
    CheckOverwrite.Enabled := False;
    EditFileName.Enabled := True;
  end
  else
  begin
    frm := FEditFrames[PageMain.TabIndex - 1];
    ActionSave.Enabled := frm.Changed;
    ActionSaveAs.Enabled := True;
    ActionBrowse.Enabled := True;
    ActionAssemble.Enabled := True;
    ActionGoto.Enabled := True;
    ActionNew.Enabled := True;
    ActionClose.Enabled := True;
    CheckOverwrite.Enabled := True;
    CheckOverwrite.Checked := frm.Overwrited;
    EditFileName.Enabled := True;
  end;
end;

procedure TFrameMain.EditFrameChanged(Sender: TObject);
var
  frm: TFrameEdit;
  n: Integer;
begin
  frm := TFrameEdit(Sender);
  EditFileName.Text := frm.FileName;
  ActionSave.Enabled := frm.Changed;
  CheckOverwrite.Checked := frm.Overwrited;
  n := FEditFrames.IndexOf(frm);
  if n >= 0 then PageMain.Pages[n + 1].Caption := ExtractFileName(frm.FileName);
end;

constructor TFrameMain.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
  FEditFrames := TFrameEditList.Create;
  Compiler := TMXCompiler.Create;
  ActionSave.Enabled := False;
  FSearchText := '';
  FCompileFile := '';
  FNewNumber := 1;
end;

destructor TFrameMain.Destroy;
begin
  Compiler.Free;
  FEditFrames.Free;
  inherited Destroy;
end;

procedure TFrameMain.FrameCloseQuery(Sender: TObject; var CanClose: boolean);
var
  n: Integer;
begin
  while (PageMain.PageCount > 2) and CanClose do
  begin
    n := PageMain.PageCount;
    PageMain.PageIndex := 1;
    PageMainChange(PageMain);
    ActionClose.Execute;
    CanClose := n > PageMain.PageCount;
  end;
end;

procedure TFrameMain.FrameShow(Sender: TObject);
begin
  PageMain.TabIndex := 0;
  UpdateButton;
  MemoOutput.SetFocus;
end;

end.

