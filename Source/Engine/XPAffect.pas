unit XPAffect;

{$MODE Delphi}

interface

uses
  SysUtils;

type
  TXPAffected = class
  private
    FTexts: array of string;
    function GetAffected(AText: string): Boolean; overload;
    function GetAffected(ATexts: array of string): Boolean; overload;
    procedure SetAffected(AText: string; AValue: Boolean); overload;
    procedure SetAffected(ATexts: array of string; AValue: Boolean); overload;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function IndexOf(AText: string): Integer;
    procedure Add(AText: string); overload;
    procedure Add(ATexts: array of string); overload;
    procedure Delete(AText: string); overload;
    procedure Delete(ATexts: array of string); overload;
    property Affected[AText: string]: Boolean read GetAffected write SetAffected; default;
    property Affects[ATexts: array of string]: Boolean read GetAffected write SetAffected;

  end;

implementation

{ TXPAffected }

procedure TXPAffected.Add(ATexts: array of string);
var
  n: Integer;
begin
  n := Length(ATexts);
  while n > 0 do
  begin
    Dec(n);
    Add(ATexts[n]);
  end;
end;

procedure TXPAffected.Add(AText: string);
var
  n: Integer;
begin
  if (AText <> '') and not GetAffected(AText) then
  begin
    n := Length(FTexts);
    SetLength(FTexts, n + 1);
    FTexts[n] := AText;
  end;
end;

procedure TXPAffected.Clear;
begin
  FTexts := nil;
end;

constructor TXPAffected.Create;
begin
  Clear;
end;

procedure TXPAffected.Delete(ATexts: array of string);
var
  n: Integer;
begin
  n := Length(ATexts);
  while n > 0 do
  begin
    Dec(n);
    Delete(ATexts[n]);
  end;
end;

procedure TXPAffected.Delete(AText: string);
var
  n: Integer;
begin
  n := IndexOf(AText);
  if n >= 0 then System.Delete(FTexts, n, 1);
end;

destructor TXPAffected.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TXPAffected.GetAffected(ATexts: array of string): Boolean;
var
  n: Integer;
begin
  Result := False;
  n := Length(ATexts);
  while (n > 0) and not Result do
  begin
    Dec(n);
    Result := Result or GetAffected(ATexts[n]);
  end;
end;

function TXPAffected.GetAffected(AText: string): Boolean;
begin
  Result := IndexOf(AText) >= 0;
end;

function TXPAffected.IndexOf(AText: string): Integer;
var
  n: Integer;
begin
  Result := -1;
  n := Length(FTexts);
  while (n > 0) and (Result < 0) do
  begin
    Dec(n);
    if CompareText(FTexts[n], AText) = 0 then Result := n;
  end;
end;

procedure TXPAffected.SetAffected(ATexts: array of string;
  AValue: Boolean);
var
  n: Integer;
begin
  n := Length(ATexts);
  while n > 0 do
  begin
    Dec(n);
    SetAffected(ATexts[n], AValue);
  end;
end;

procedure TXPAffected.SetAffected(AText: string; AValue: Boolean);
begin
  if AValue then
    Add(AText)
  else Delete(AText);
end;

end.
