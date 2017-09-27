unit main_routine;

{$mode delphi}

interface

uses
  Classes, SysUtils, StdCtrls, Dialogs, Controls;

procedure SelectWord(AMemo: TMemo; ASelStart: Integer);
procedure SearchText(AMemo: TMemo; AText: string; AStartPos: Integer);

implementation

procedure SelectWord(AMemo: TMemo; ASelStart: Integer);
const
  IgnoreChar = #9#10#13' !@#$%^&*()_+-=~`[]{}\|/.,:;<>"''';
var
  n, nl, ns: Integer;
  done, is_delim: Boolean;
  s: string;
begin
  s := AMemo.Text;
  ns := Length(s);
  if ASelStart >= ns then Exit;
  n := ASelStart + 1;
  is_delim := Pos(s[n], IgnoreChar) > 0;
  nl := 0;
  done := False;
  while (n <= ns) and not done do
  begin
    done := Pos(s[n], IgnoreChar) > 0;
    if is_delim then done := not done;
    if not done then Inc(nl);
    Inc(n);
  end;
  n := ASelStart;
  done := False;
  while (n > 0) and not done do
  begin
    done := Pos(s[n], IgnoreChar) > 0;
    if is_delim then done := not done;
    if not done then
    begin
      Dec(ASelStart);
      Inc(nl);
    end;
    Dec(n);
  end;
  AMemo.SelStart := ASelStart;
  AMemo.SelLength := nl;
end;

procedure SearchText(AMemo: TMemo; AText: string; AStartPos: Integer);
var
  found, done: Boolean;
  lpos, npos: Integer;
  s, sm: string;
begin
  done := False;
  while not done do
  begin
    npos := AStartPos;
    Inc(npos);
    found := False;
    sm := AMemo.Text;
    s := Copy(sm, npos, Length(AText));
    lpos := Length(sm);
    Inc(npos, Length(AText));
    while (npos <= lpos) and not found do
    begin
      found := CompareText(AText, s) = 0;
      if not found then
      begin
        Delete(s, 1, 1);
        s := s + Copy(sm, npos, 1);
        Inc(npos);
      end;
    end;
    if not found and (AStartPos > 0) then
    begin
      done := MessageDlg('Can not find search text any more.'#13#10'Do you want to continue search from start?',
        mtWarning, [mbYes, mbNo], 0) = mrNo;
      if not done then AStartPos := 0;
    end
    else done := True;
  end;
  if found then
  begin
    AMemo.SelStart := npos - Length(AText) - 1;
    AMemo.SelLength := Length(AText);
    AMemo.SetFocus;
  end
  else if AStartPos = 0 then ShowMessage(Format('Can not find any search text. [%s]', [AText]))
end;

end.

