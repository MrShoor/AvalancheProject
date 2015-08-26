program apcc;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  untMain,
  SysUtils,
  GLSLCompiler,
  davTypes in 'davTypes.pas';

begin
  try
    DoWork;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
