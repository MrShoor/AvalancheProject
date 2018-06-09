unit avFileUtils;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils;

function ExpandAppRelativePath(const ALocalPath: string): string;

implementation

function ExpandAppRelativePath(const ALocalPath: string): string;
begin
  Result := ExpandFileName(ExtractFilePath(ParamStr(0)) + ALocalPath);
end;

end.

