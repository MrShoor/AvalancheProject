program cGLSL;

uses
  Forms,
  untCompilerForm in 'untCompilerForm.pas' {frmcGLSL},
  untCompilerTypes in 'untCompilerTypes.pas',
  untGLWrappers in 'untGLWrappers.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.ShowMainForm := False;
  Application.CreateForm(TfrmcGLSL, frmcGLSL);
  Application.Run;
end.
