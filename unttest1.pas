unit untTest1;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, avTypes, avRes, mutils, dglOpenGL, TypInfo, avContnrs, avTess,
  Windows, avCameraController, avControls;

type
  { TPolyVertex }

  TPolyVertex = packed record
    vsCoord: TVec3;
    vsColor1: TVec4b;
    class function Layout: IDataLayout; static;
  end;

  { TQuadVertex }

  TQuadVertex = packed record
    quadCoord: TVec2;
    class function Layout: IDataLayout; static;
  end;

  { TPointVertex }

  TPointVertex = packed record
    vsCoord : TVec3;
    vsColor1: TVec4b;
    class function Layout: IDataLayout; static;
  end;

  IQuadVertices = specialize IArray<TQuadVertex>;
  TQuadVertices = specialize TVerticesRec<TQuadVertex>;

  IPointVertices = specialize IArray<TPointVertex>;
  TPointVertices = specialize TVerticesRec<TPointVertex>;

  IPolyVertices = specialize IArray<TPolyVertex>;
  TPolyVertices = specialize TVerticesRec<TPolyVertex>;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    RenderTimer: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RenderTimerTimer(Sender: TObject);
  private
    FMain: TavMainRender;
    FPointProgram: TavProgram;
    FPolyProgram: TavProgram;
    FVBQuad: TavVB;
    FVBPoints: TavVB;

    FVBCube: TavVB;
    FIBCube: TavIB;

    FRootControl: TavRootControl;
    FDebugControl: TavDebugControl;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  GraphUtil;

{$R *.lfm}

{ TPolyVertex }

class function TPolyVertex.Layout: IDataLayout;
begin
  Result := LB.Add('vsCoord', ctFloat, 3).
               Add('vsColor1', ctUByte, 4, True).Finish();
end;

{$R 'C:\MyProj\DKontrol\code\Shaders\default_make'}

{ TPointVertex }

class function TPointVertex.Layout: IDataLayout;
begin
  Result := LB.Add('vsCoord', ctFloat, 3).
               Add('vsColor1', ctUByte, 4, True).Finish();
end;

{ TQuadVertex }

class function TQuadVertex.Layout: IDataLayout; static;
begin
  Result := LB.Add('quadCoord', ctFloat, 2).Finish();
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var QuadData: IQuadVertices;
    QVert: TQuadVertex;
    PointData: IPointVertices;
    PVert: TPointVertex;
    PolyData: IPolyVertices;
    PolyVert: TPolyVertex;
    PolyInd: IIndices;
    i: Integer;
    rgb: Integer;
    cc: TavCameraController;
begin
  FMain := TavMainRender.Create(nil);
  FMain.Window := Handle;
  FMain.Init3D;

//  FPointProgram := TavProgram.Create(FMain);
//  FPointProgram.LoadFromJSON('C:\MyProj\Avalanche\Shaders\!Out\OGL_dots.glsl');

//  FPolyProgram := TavProgram.Create(FMain);
//  FPolyProgram.LoadFromJSON('C:\MyProj\Avalanche\Shaders\!Out\OGL_poly.glsl');
//  FPointProgram.LoadFromJSON('OGL_dots', True);
//  FPointProgram.LoadFromJSON('OGL_dots.glsl');

  QuadData := TQuadVertices.Create(TQuadVertex.Layout);
  QVert.quadCoord := Vec(-1.0, -1.0);
  QuadData.Add(QVert);
  QVert.quadCoord := Vec(-1.0,  1.0);
  QuadData.Add(QVert);
  QVert.quadCoord := Vec( 1.0, -1.0);
  QuadData.Add(QVert);
  QVert.quadCoord := Vec( 1.0,  1.0);
  QuadData.Add(QVert);
  FVBQuad := TavVB.Create(FMain);
  FVBQuad.Vertices := QuadData As IVerticesData;
  FVBQuad.PrimType := ptTriangleStrip;
  FVBQuad.CullMode := cmNone;

  PointData := TPointVertices.Create(TPointVertex.Layout);
  for i := 0 to 100 do
  begin
    rgb := ColorHLSToRGB(Random(255), 128, 255);
    PVert.vsCoord := Vec(Random-0.5, Random-0.5, Random-0.5)*4.0;
    PVert.vsColor1 := Vec(GetRValue(rgb), GetGValue(rgb), GetBValue(rgb), 255);
    PointData.Add(PVert);
  end;
  PVert.vsColor1 := Vec(Byte(255),255,255,255);
  PVert.vsCoord := Vec(0.0, 0.0, Random);
  PointData.Add(PVert);
  FVBPoints := TavVB.Create(FMain);
  FVBPoints.Vertices := PointData As IVerticesData;

  PolyData := TPolyVertices.Create(TPolyVertex.Layout);
  PolyVert.vsColor1 := Vec(Byte(255), 255, Byte(0), 255);
  PolyVert.vsCoord := Vec(-1.0, -1.0, -1.0); PolyData.Add(PolyVert);
  PolyVert.vsCoord := Vec(-1.0,  1.0, -1.0); PolyData.Add(PolyVert);
  PolyVert.vsCoord := Vec( 1.0, -1.0, -1.0); PolyData.Add(PolyVert);
  PolyVert.vsCoord := Vec( 1.0,  1.0, -1.0); PolyData.Add(PolyVert);

  PolyVert.vsCoord := Vec(-1.0, -1.0,  1.0); PolyData.Add(PolyVert);
  PolyVert.vsCoord := Vec(-1.0,  1.0,  1.0); PolyData.Add(PolyVert);
  PolyVert.vsCoord := Vec( 1.0, -1.0,  1.0); PolyData.Add(PolyVert);
  PolyVert.vsCoord := Vec( 1.0,  1.0,  1.0); PolyData.Add(PolyVert);

  PolyVert.vsCoord := Vec(-1.0, -1.0, -1.0); PolyData.Add(PolyVert);
  PolyVert.vsCoord := Vec( 1.0, -1.0, -1.0); PolyData.Add(PolyVert);
  PolyVert.vsCoord := Vec(-1.0, -1.0,  1.0); PolyData.Add(PolyVert);
  PolyVert.vsCoord := Vec( 1.0, -1.0,  1.0); PolyData.Add(PolyVert);

  PolyVert.vsCoord := Vec(-1.0,  1.0, -1.0); PolyData.Add(PolyVert);
  PolyVert.vsCoord := Vec( 1.0,  1.0, -1.0); PolyData.Add(PolyVert);
  PolyVert.vsCoord := Vec(-1.0,  1.0,  1.0); PolyData.Add(PolyVert);
  PolyVert.vsCoord := Vec( 1.0,  1.0,  1.0); PolyData.Add(PolyVert);

  PolyVert.vsCoord := Vec(-1.0, -1.0, -1.0); PolyData.Add(PolyVert);
  PolyVert.vsCoord := Vec(-1.0, -1.0,  1.0); PolyData.Add(PolyVert);
  PolyVert.vsCoord := Vec(-1.0,  1.0, -1.0); PolyData.Add(PolyVert);
  PolyVert.vsCoord := Vec(-1.0,  1.0,  1.0); PolyData.Add(PolyVert);

  PolyVert.vsCoord := Vec( 1.0, -1.0, -1.0); PolyData.Add(PolyVert);
  PolyVert.vsCoord := Vec( 1.0, -1.0,  1.0); PolyData.Add(PolyVert);
  PolyVert.vsCoord := Vec( 1.0,  1.0, -1.0); PolyData.Add(PolyVert);
  PolyVert.vsCoord := Vec( 1.0,  1.0,  1.0); PolyData.Add(PolyVert);

  PolyInd := Create_IIndices;
  PolyInd.PrimitiveType := ptTriangles;
  for i := 0 to 5 do
  begin
    PolyInd.Add(i*4 + 0);
    PolyInd.Add(i*4 + 1);
    PolyInd.Add(i*4 + 2);

    PolyInd.Add(i*4 + 1);
    PolyInd.Add(i*4 + 2);
    PolyInd.Add(i*4 + 3);
  end;

  FVBCube := TavVB.Create(FMain);
  FVBCube.Vertices := PolyData as IVerticesData;
  FIBCube := TavIB.Create(FMain);
  FIBCube.CullMode := cmNone;
  FIBCube.PrimType := ptTriangles;
  FIBCube.Indices := PolyInd;

  cc := TavCameraController.Create(FMain);
  cc.CanMove := True;
  cc.CanRotate := True;
  cc.MovePlane := Plane(Vec(0.0,0.0,1.0), Vec(0.0,0.0,0.0));

  FRootControl  := TavRootControl.Create(FMain);
  FRootControl.DPI := Sqrt(Sqr(2560)+Sqr(1600))/30;
  FDebugControl := TavDebugControl.Create(FRootControl);
  FDebugControl.Position := RectF(0,2,0,2);
//  FDebugControl.Position :=
end;

procedure TForm1.Button1Click(Sender: TObject);
var rotor: TQuat;
    dirvector: TVec3;
begin
  rotor := Quat(Normalize(FMain.Camera.Up), 0.5*Pi);
  dirvector := FMain.Camera.At - FMain.Camera.Eye;
  dirvector := rotor * dirvector;
  FMain.Camera.Play(FMain.Camera.At + dirvector, FMain.Camera.At, FMain.Camera.Up, 0.10);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMain);
end;

procedure TForm1.RenderTimerTimer(Sender: TObject);
begin
  if FMain = nil then Exit;
  if FMain.Bind then
  try
    FMain.Clear(Black);
    glEnable(GL_DEPTH_CLAMP);
    glEnable(GL_PRIMITIVE_RESTART);
    glPrimitiveRestartIndex($FFFFFFFF);
    glDisable(GL_DEPTH_TEST);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glViewport(0,0,ClientWidth,ClientHeight);
//    FMain.States.Wireframe := True;

    FMain.Projection.Aspect := ClientHeight/ClientWidth;
{
    FPointProgram.Select;
    FPointProgram.SetAttributes(FVBQuad, nil, FVBPoints);
    FPointProgram.SetUniform('Rad', 2.0);
    FPointProgram.SetUniform('FiModifier', Vec(0.25, (GetTickCount mod 10000000)/1000*2*Pi ));
    FPointProgram.SetUniform('PixelSize', Vec(80/ClientWidth, 80/ClientHeight));
    FPointProgram.Draw(FVBPoints.Vertices.VerticesCount);

    FPolyProgram.Select;
    FPolyProgram.SetAttributes(FVBCube, FIBCube, nil);
    FPolyProgram.Draw;
}
    FRootControl.Draw;
{
    glUseProgram(0);
    glBegin(GL_POINTS);
      glVertex2d(0, 0);
    glEnd;
}
    FMain.Present;
  finally
    FMain.Unbind;
  end;
end;

end.

