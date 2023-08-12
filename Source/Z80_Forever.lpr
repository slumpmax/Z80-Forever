program Z80_Forever;

{$MODE Delphi}

uses
  Forms, Interfaces,
  FormMain in 'FormMain.pas' {MainForm},
  XPVideo in 'Engine\XPVideo.pas',
  XPCompiler in 'Engine\XPCompiler.pas',
  XPAffect in 'Engine\XPAffect.pas',
  XPDebug in 'Engine\XPDebug.pas',
  XPConfig in 'Engine\XPConfig.pas',
  FormPage in 'FormPage.pas' {PageForm},
  FrameEdit in 'FrameEdit.pas' {EditFrame: TFrame},
  XPClock in 'Engine\XPClock.pas',
  XPDrive in 'Engine\XPDrive.pas',
  XPSlot in 'Engine\XPSlot.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Z80 Forever';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TPageForm, PageForm);
  Application.Run;
end.
