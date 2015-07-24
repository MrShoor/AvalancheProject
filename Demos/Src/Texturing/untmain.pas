unit untMain;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  LCLType, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  avRes, avTypes, avTess, avContnrs, mutils, avCameraController, avTexLoader;

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

    procedure BuildFramebuffer;
  public
    procedure EraseBackground(DC: HDC); override;
    procedure RenderScene;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}
{$R 'Texturing_shaders\shaders.res'}

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
    USize, VSize: Single;
begin
  FMain := TavMainRender.Create(Nil);
  FMain.Window := Handle;
  FMain.Init3D();

  FProgram := TavProgram.Create(FMain);
  FProgram.LoadFromJSON('OGL_base', True);

  FTexture := TavTexture.Create(FMain);
  FTexture.TargetFormat := TTextureFormat.RGBA;
  FTexture.TexData := LoadTexture('..\Media\tig.jpg', SIZE_DEFAULT, SIZE_DEFAULT, TImageFormat.B8G8R8A8);

  USize := FTexture.TexData.Data(0,0).Width/NextPow2(FTexture.TexData.Data(0,0).Width);
  VSize := FTexture.TexData.Data(0,0).Height/NextPow2(FTexture.TexData.Data(0,0).Height);
  GenCube(H, H, H, USize, VSize, vert, ind);

  FCubeVertices := TavVB.Create(FMain);
  FCubeVertices.Vertices := vert;
  FCubeIndices := TavIB.Create(FMain);
  FCubeIndices.PrimType := ptTriangles;
  FCubeIndices.Indices := ind;
  FCubeIndices.CullMode := cmBack;

  BuildFramebuffer;

  cc := TavCameraController.Create(FMain);
  cc.CanRotate := True;
//  cc.Enabled := True;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMain);
end;

procedure TfrmMain.FormPaint(Sender: TObject);
begin
  RenderScene;
end;

procedure TfrmMain.BuildFramebuffer;
var tex: TavTexture;
begin
  FFrameBuffer := TavFrameBuffer.Create(FMain);
  tex := TavTexture.Create(FFrameBuffer);
  tex.TargetFormat := TTextureFormat.RGBA;
  FFrameBuffer.SetColor(0, tex, 0);

  tex := TavTexture.Create(FFrameBuffer);
  tex.TargetFormat := TTextureFormat.D32f;
  FFrameBuffer.SetDepth(tex, 0);
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
    FMain.Clear(Vec(0.0,0.0,0.0,1.0), True, 1, True);

    FFrameBuffer.FrameRect := RectF(0, ClientWidth, 0, ClientHeight);
    FFrameBuffer.Select;
//    FMain.States.Wireframe := True;
//    FMain.States.DepthTest := True;
    FProgram.Select;
    FProgram.SetAttributes(FCubeVertices, FCubeIndices, nil);
    FProgram.SetUniform('Diffuse', FTexture, Sampler_NoFilter);
    FProgram.Draw();
    FMain.Present;
  finally
    FMain.Unbind;
  end;
end;

end.

