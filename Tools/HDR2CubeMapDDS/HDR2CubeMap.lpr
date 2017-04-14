program HDR2CubeMap;

{$AppType Console}

uses
  SysUtils,
  CubeMapConverter;

begin
  try
    DoConvert(ParseParams);
  except
    on E: Exception do
    begin
      WriteLn(E.Message);
      ExitCode := 1;
    end;
  end;
end.

