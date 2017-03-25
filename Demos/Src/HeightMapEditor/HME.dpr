program HME;

{$R 'shaders.res' 'shaders\shaders.rc'}

uses
  Vcl.Forms,
  untMain in 'untMain.pas' {frmMain},
  untHMEFrame in 'untHMEFrame.pas' {frmHMEditor: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
