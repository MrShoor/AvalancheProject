program HashMap_Test;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, hashmap_Fill_Int_Int, fpcunittestrunner,
  hashmap_Check_Int_Intf;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

