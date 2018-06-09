unit math_fx;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils;

//2^(-x*4)/(1-2^(-4)) - 2^(-4); x from 0 to 1
function FallOff_Exp(t: Single; p: Single = 4): Single;

implementation

uses
  Math;

function FallOff_Exp(t: Single; p: Single): Single;
var e1: Single;
begin
  e1 := power(2, -p);
  Result := power(2, -t*p) / (1 - e1) - e1;
end;

end.

