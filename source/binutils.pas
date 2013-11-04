unit BinUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function IntToBin( Int: Integer ): String;
function BinToInt( Binary: String ):Longint;

implementation

function BinToInt( Binary: String ):Longint;
var
  i, j: Integer;
begin
  Result := 0;
  j := 1;
  for i := Length( Binary ) downTo 1 do
  begin
    Result := Result + StrToInt( Binary[i] ) * j;
    j := j*2;
  end;
end;

function IntToBin( Int: Integer ): String;
var
  i, j: Integer;
begin
  Result := '';
  i := 0;
  j := 1;
  while i = 0 do
    if( ( Int Mod (j*2) ) = Int )
      then i := j
      else j := j * 2;
  while i > 0 do
  begin
    if( ( Int div i ) > 0 ) then
    begin
      Int := Int - i;
      Result := Result + '1';
    end
    else Result := Result + '0';
    i := Trunc( i * 0.5 );
  end;
end;

end.

