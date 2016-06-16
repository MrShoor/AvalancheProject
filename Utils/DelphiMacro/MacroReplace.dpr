program MacroReplace;

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes;

const
  TOKEN_TARGET = '$Target';
  TOKEN_OUTPUT = '$Output';

procedure DoWork;
var slKeys: TStringList;
    slSrc: TStringList;
    name, value: String;
    i: Integer;
begin
  slKeys := nil;
  try
    slKeys := TStringList.Create;
    slKeys.LoadFromFile(ParamStr(1));
    for i := 0 to slKeys.Count - 1 do
    begin
      name := slKeys.Names[i];
      value := slKeys.ValueFromIndex[i];
      if (name = TOKEN_TARGET) and FileExists(value) then
      begin
        FreeAndNil(slSrc);
        slSrc := TStringList.Create;
        slSrc.LoadFromFile(value);
        Continue;
      end;

      if (name = TOKEN_OUTPUT) and Assigned(slSrc) then
      begin
        slSrc.SaveToFile(value);
        FreeAndNil(slSrc);
        Continue;
      end;

      if Assigned(slSrc) then
        slSrc.Text := StringReplace(slSrc.Text, name, value, [rfReplaceAll, rfIgnoreCase]);
    end;
  finally
    FreeAndNil(slKeys);
  end;
end;

begin
  try
    if FileExists(ParamStr(1)) then
      DoWork;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
