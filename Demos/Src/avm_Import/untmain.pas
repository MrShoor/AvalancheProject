unit untMain;

{$mode objfpc}{$H+}
{$R 'MeshShader\shaders.res'}

interface

uses
  LMessages, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, avRes, avTypes, mutils, avCameraController, avModel;

const ObjInd = 0;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    AnimationTimer: TTimer;
    procedure AnimationTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    procedure RenderScene;
  protected
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
  public
    FMain: TavMainRender;
    FFBO : TavFrameBuffer;

    FModels: TavModelCollection;

    FProg: TavProgram;

    FInstances: array of IavModelInstance;
//    FBody: IavModelInstance;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FMain := TavMainRender.Create(Nil);
  FMain.Window := Handle;
  FMain.Init3D(apiDX11);
//  FMain.Projection.Ortho := True;
  FMain.Camera.Eye := Vec(0,10,-30);
  FMain.Projection.OrthoHeight := 20;

  FFBO := Create_FrameBuffer(FMain, [TTextureFormat.RGBA, TTextureFormat.D32f]);

  FModels := TavModelCollection.Create(FMain);
  FModels.AddFromFile('test.txt');

  FProg := TavProgram.Create(FMain);
  FProg.LoadFromJSON('avMesh', True);

  with TavCameraController.Create(FMain) do
  begin
    CanRotate := True;
    CanMove := True;
    MovePlane := Plane(0,0,1,0);
  end;
end;

procedure TfrmMain.AnimationTimerTimer(Sender: TObject);
begin
  FMain.InvalidateWindow;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMain);
end;

procedure TfrmMain.FormPaint(Sender: TObject);
begin
  RenderScene;
end;

procedure TfrmMain.RenderScene;
var MName: String;
    i: Integer;
begin
  if FMain = Nil then Exit;
  if not FMain.Inited3D then Exit;
  if FMain.Bind then
  try
    FMain.States.DepthTest := True;
//    FMain.States.Wireframe := True;

    FFBO.FrameRect := RectI(0,0,ClientWidth, ClientHeight);
    FFBO.Select();
    FFBO.Clear(0, Vec(0,0,0,0));
    FFBO.ClearDS(1);

    FProg.Select;

    if FMain.FrameID > 1 then
    begin
      FModels.Select;

      if FInstances = nil then
      begin
        SetLength(FInstances, FModels.ModelsCount);
        i := 0;
        FModels.Reset;
        while FModels.Next(MName) do
        begin
          FInstances[i] := FModels.CreateInstance(MName);
          Inc(i);
        end;
      end;

      for i := 0 to Length(FInstances) - 1 do
        FInstances[i].AnimationStart('DE_Provoke');
//        FInstances[i].AnimationStart('DE_CombatRun');
      for i := 0 to Length(FInstances) - 1 do
        FInstances[i].Draw;
    end;

    FFBO.BlitToWindow();
    FMain.Present;
  finally
    FMain.Unbind;
  end;
end;

procedure TfrmMain.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  Message.Result := 1;
end;

end.

