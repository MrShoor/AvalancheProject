program JFDistanceField;

{$R 'shaders.res' 'shaders\shaders.rc'}

uses
  Vcl.Forms,
  untMain in 'untMain.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
