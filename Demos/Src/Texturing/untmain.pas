unit untMain;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  LCLType, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, avRes, avTypes, avTess, avContnrs, mutils,
  avCameraController, avTexLoader, ContextSwitcher;

type

  { TCubeVertex }

  TCubeVertex = packed record
    vsCoord: TVec3;
    vsNormal: TVec3;
    vsTexCrd: TVec2;
    class function Layout: IDataLayout; static;
  end;
  ICubeVertices = specialize IArray<TCubeVertex>;
  TCubeVertices = specialize TVerticesRec<TCubeVertex>;

  { TfrmMain }

  TfrmMain = class(TForm)
    procedure cbDXChange(Sender: TObject);
    procedure cbOGLChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    FMain: TavMainRender;

    FProgram: TavProgram;
    FCubeVertices: TavVB;
    FCubeIndices: TavIB;
    FTexture: TavTexture;

    FFrameBuffer: TavFrameBuffer;
  public
    procedure EraseBackground(DC: HDC); override;
    procedure RenderScene;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{$R 'Texturing_shaders\shaders.rc'}

{ TCubeVertex }

class function TCubeVertex.Layout: IDataLayout;
begin
  Result := LB.Add('vsCoord', ctFloat, 3).
               Add('vsNormal', ctFloat, 3).
               Add('vsTexCrd', ctFloat, 2).Finish();
end;

procedure GenCube(XHalfSize, YHalfSize, ZHalfSize: Single; USize, VSize: Single; out OutVert: IVerticesData; out OutInd: IIndicesData);
var Vert : ICubeVertices;
    V : TCubeVertex;
    Ind: IIndices;
    i: Integer;
begin
  Vert := TCubeVertices.Create;

  V.vsNormal := Vec(0,0,-1.0);

  V.vsCoord := Vec(-XHalfSize,-YHalfSize,-ZHalfSize); V.vsTexCrd := Vec(0.0, 0.0); Vert.Add(V);
  V.vsCoord := Vec(-XHalfSize, YHalfSize,-ZHalfSize); V.vsTexCrd := Vec(0.0, VSize); Vert.Add(V);
  V.vsCoord := Vec( XHalfSize,-YHalfSize,-ZHalfSize); V.vsTexCrd := Vec(USize, 0.0); Vert.Add(V);
  V.vsCoord := Vec( XHalfSize, YHalfSize,-ZHalfSize); V.vsTexCrd := Vec(USize, VSize); Vert.Add(V);

  V.vsNormal := Vec(0,0,1.0);
  V.vsCoord := Vec(-XHalfSize, YHalfSize, ZHalfSize); V.vsTexCrd := Vec(0.0, 0.0); Vert.Add(V);
  V.vsCoord := Vec(-XHalfSize,-YHalfSize, ZHalfSize); V.vsTexCrd := Vec(0.0, VSize); Vert.Add(V);
  V.vsCoord := Vec( XHalfSize, YHalfSize, ZHalfSize); V.vsTexCrd := Vec(USize, 0.0); Vert.Add(V);
  V.vsCoord := Vec( XHalfSize,-YHalfSize, ZHalfSize); V.vsTexCrd := Vec(USize, VSize); Vert.Add(V);

  V.vsNormal := Vec(0,-1.0,0);
  V.vsCoord := Vec(-XHalfSize,-YHalfSize,-ZHalfSize); V.vsTexCrd := Vec(0.0, 0.0); Vert.Add(V);
  V.vsCoord := Vec( XHalfSize,-YHalfSize,-ZHalfSize); V.vsTexCrd := Vec(0.0, VSize); Vert.Add(V);
  V.vsCoord := Vec(-XHalfSize,-YHalfSize, ZHalfSize); V.vsTexCrd := Vec(USize, 0.0); Vert.Add(V);
  V.vsCoord := Vec( XHalfSize,-YHalfSize, ZHalfSize); V.vsTexCrd := Vec(USize, VSize); Vert.Add(V);

  V.vsNormal := Vec(0,1.0,0);
  V.vsCoord := Vec( XHalfSize, YHalfSize,-ZHalfSize); V.vsTexCrd := Vec(0.0, 0.0); Vert.Add(V);
  V.vsCoord := Vec(-XHalfSize, YHalfSize,-ZHalfSize); V.vsTexCrd := Vec(0.0, VSize); Vert.Add(V);
  V.vsCoord := Vec( XHalfSize, YHalfSize, ZHalfSize); V.vsTexCrd := Vec(USize, 0.0); Vert.Add(V);
  V.vsCoord := Vec(-XHalfSize, YHalfSize, ZHalfSize); V.vsTexCrd := Vec(USize, VSize); Vert.Add(V);

  V.vsNormal := Vec(-1.0,0,0);
  V.vsCoord := Vec(-XHalfSize,-YHalfSize,-ZHalfSize); V.vsTexCrd := Vec(0.0, 0.0); Vert.Add(V);
  V.vsCoord := Vec(-XHalfSize,-YHalfSize, ZHalfSize); V.vsTexCrd := Vec(0.0, VSize); Vert.Add(V);
  V.vsCoord := Vec(-XHalfSize, YHalfSize,-ZHalfSize); V.vsTexCrd := Vec(USize, 0.0); Vert.Add(V);
  V.vsCoord := Vec(-XHalfSize, YHalfSize, ZHalfSize); V.vsTexCrd := Vec(USize, VSize); Vert.Add(V);

  V.vsNormal := Vec(1.0,0,0);
  V.vsCoord := Vec( XHalfSize,-YHalfSize, ZHalfSize); V.vsTexCrd := Vec(0.0, 0.0); Vert.Add(V);
  V.vsCoord := Vec( XHalfSize,-YHalfSize,-ZHalfSize); V.vsTexCrd := Vec(0.0, VSize); Vert.Add(V);
  V.vsCoord := Vec( XHalfSize, YHalfSize, ZHalfSize); V.vsTexCrd := Vec(USize, 0.0); Vert.Add(V);
  V.vsCoord := Vec( XHalfSize, YHalfSize,-ZHalfSize); V.vsTexCrd := Vec(USize, VSize); Vert.Add(V);

  Ind := Create_IIndices;
  Ind.PrimitiveType := ptTriangles;
  for i := 0 to 5 do
  begin
    Ind.Add(i*4 + 0);
    Ind.Add(i*4 + 2);
    Ind.Add(i*4 + 1);

    Ind.Add(i*4 + 1);
    Ind.Add(i*4 + 2);
    Ind.Add(i*4 + 3);
  end;

  OutVert := Vert as IVerticesData;
  OutInd := Ind;
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
const H = 1.0;
var vert: IVerticesData;
    ind : IIndicesData;

    cc: TavCameraController;
begin
  FMain := TavMainRender.Create(Nil);
  FMain.Window := Handle;
  FMain.Init3D();
  FMain.Camera.Eye := Vec(-1.6, 1.4,-2.0);
  FMain.Projection.FarPlane := 10.0;
  FMain.Projection.NearPlane := 0.1;

  TavContextSwitcher.Create(FMain);

  FFrameBuffer := Create_FrameBuffer(FMain, [TTextureFormat.RGBA, TTextureFormat.D32f]);

  FProgram := TavProgram.Create(FMain);
  FProgram.LoadFromJSON('base', True);

  FTexture := TavTexture.Create(FMain);
  FTexture.TargetFormat := TTextureFormat.RGBA;
  FTexture.AutoGenerateMips := True;
  FTexture.TexData := LoadTexture('..\Media\tig.jpg', SIZE_DEFAULT, SIZE_DEFAULT, TImageFormat.A8R8G8B8);

  GenCube(H, H, H, 1, 1, vert, ind);

  FCubeVertices := TavVB.Create(FMain);
  FCubeVertices.Vertices := vert;
  FCubeIndices := TavIB.Create(FMain);
  FCubeIndices.PrimType := ptTriangles;
  FCubeIndices.Indices := ind;
  FCubeIndices.CullMode := cmNone;

  cc := TavCameraController.Create(FMain);
  cc.CanRotate := True;
end;

procedure TfrmMain.cbOGLChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TfrmMain.cbDXChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMain);
end;

procedure TfrmMain.FormPaint(Sender: TObject);
begin
  RenderScene;
end;

procedure TfrmMain.EraseBackground(DC: HDC);
begin
  //inherited EraseBackground(DC);
end;

procedure TfrmMain.RenderScene;
begin
  if FMain = nil then Exit;
  if FMain.Bind then
  try
    FMain.States.DepthTest := True;

    FFrameBuffer.FrameRect := RectI(0, 0, ClientWidth, ClientHeight);
    FFrameBuffer.Select;

    FMain.Clear(Vec(0.0,0.2,0.4,1.0), True, FMain.Projection.DepthRange.y, True);

    FProgram.Select;
    FProgram.SetAttributes(FCubeVertices, FCubeIndices, nil);
    FProgram.SetUniform('Diffuse', FTexture, Sampler_LinearClamped);
    FProgram.Draw();

    FFrameBuffer.BlitToWindow;
    FMain.Present;
  finally
    FMain.Unbind;
  end;
end;

end.

