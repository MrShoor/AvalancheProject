{$IfDef FPC}
  {$Macro On}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
  {$IfDef CPU86}
    {$FPUType sse2}
  {$EndIf}
  {$IfDef CPU64}
    {$FPUType sse64}
  {$EndIf}
  {$Define notDCC}
{$Else}
  {$Define DCC}
  {$IfDef WIN32}
    {$Define Windows}
  {$EndIf}
  {$IfDef WIN64}
    {$Define Windows}
  {$EndIf}
{$EndIf}
