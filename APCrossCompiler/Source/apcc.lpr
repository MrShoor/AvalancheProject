program apcc;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, CompileTask, apccUtils, avTypes,
  untMain, HLSLCompiler, GLSLCompiler;



begin
  try
    DoWork;
  except
    on E: Exception do
    begin
      SetConsoleColor(CONSOLE_Red);
      Writeln(E.ClassName, ': ', E.Message);
      SetConsoleColor(CONSOLE_Default);
      ExitCode := 1;
    end;
  end;
end.

