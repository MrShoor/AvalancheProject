unit untMain;

{$mode objfpc}{$H+}
{$R 'MeshShader\shaders.res'}

interface

uses
  LMessages, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, avRes, avTypes, mutils, avMesh, avCameraController;

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

    FMeshes: TavMeshes;
    FActiveAnimation: IavAnimation;

    FMeshVB: TavVB;
    FMeshIB: TavIB;
    FMeshTransform: TavTexture;

    FProg: TavProgram;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
const ObjInd = 0;
      AnimInd = 0;
begin
  FMain := TavMainRender.Create(Nil);
  FMain.Window := Handle;
  FMain.Init3D(apiDX11);
//  FMain.Projection.Ortho := True;
  FMain.Projection.OrthoHeight := 20;

  FFBO := Create_FrameBuffer(FMain, [TTextureFormat.RGBA, TTextureFormat.D32f]);

  LoadFromFile('test.txt', FMeshes);

  FMeshVB := TavVB.Create(FMain);
  FMeshIB := TavIB.Create(FMain);
  FMeshTransform := TavTexture.Create(FMain);
  WriteLn(FMeshes[ObjInd].name);
  Caption := FMeshes[ObjInd].name;
  FMeshVB.Vertices := FMeshes[ObjInd].vert as IVerticesData;
  FMeshIB.Indices := FMeshes[ObjInd].ind as IIndicesData;
  if Assigned(FMeshes[ObjInd].Armature) then
  begin
    FMeshTransform.TargetFormat := TTextureFormat.RGBA32f;
    FMeshTransform.TexData := FMeshes[ObjInd].Armature.BoneTransformData;

    if FMeshes[ObjInd].Armature.AnimCount > AnimInd then
    begin
      FActiveAnimation := FMeshes[ObjInd].Armature.Anim[AnimInd];
      FActiveAnimation.Enabled := True;
    end;
  end;

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
  if Assigned(FActiveAnimation) then
  begin
    FActiveAnimation.Frame := FActiveAnimation.Frame + 0.4;
    if Assigned(FActiveAnimation.Armature) then
    begin
      FActiveAnimation.Armature.UpdateTransformData;
      FMeshTransform.Invalidate;
      FMain.InvalidateWindow;
    end;
  end;
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
    FProg.SetAttributes(FMeshVB, FMeshIB, nil);
    if Assigned(FMeshTransform) then
    begin
      FProg.SetUniform('BonePixelHeight', 1/FMeshTransform.Height);
      FProg.SetUniform('BoneTransform', FMeshTransform, Sampler_NoFilter);
    end
    else
      FProg.SetUniform('BonePixelHeight', 0.0);
    FProg.Draw();

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

