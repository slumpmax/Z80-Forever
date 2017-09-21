program Z80_Forever;

{$mode objfpc}{$H+}

{$R 'resource_main.res' 'resource_main.rc'}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, form_theme, frame_main
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='Z80 Forever';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TThemeForm, ThemeForm);
  Application.Run;
end.

