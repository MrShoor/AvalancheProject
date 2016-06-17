program Instancing;

{$AppType Console}

{$R 'shaders.res' 'Instancing_shaders\shaders.rc'}

uses
  Vcl.Forms,
  untmain in 'untmain.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
