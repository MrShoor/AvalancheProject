program Test1;

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, untTest1, avBase, avTypes, avContext_OGL, avPlatform, avLog, avcontnrs,
  mutils, avContext, runtimetypeinfocontrols, avRes, avTess, 
avControls, avCameraController, avcanvas;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

