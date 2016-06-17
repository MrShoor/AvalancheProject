program avm_Import;

{$R 'shaders.res' 'MeshShader\shaders.rc'}

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
