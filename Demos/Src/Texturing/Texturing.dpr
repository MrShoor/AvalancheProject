program Texturing;

{$R 'shaders.res' 'Texturing_shaders\shaders.rc'}

uses
  Forms,
  untmain in 'untmain.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
