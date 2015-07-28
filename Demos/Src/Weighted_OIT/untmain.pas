unit untMain;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  LCLType, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, avRes, avTypes, avTess, avContnrs, mutils,
  avCameraController;

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

  { TCubeInstance }

  TCubeInstance = packed record
    aiPosition: TVec3;
    aiColor : TVec4;
    class function Layout: IDataLayout; static;
  end;
  ICubeInstances = specialize IArray<TCubeInstance>;
  TCubeInstances = specialize TVerticesRec<TCubeInstance>;

  { TfrmMain }

  TfrmMain = class(TForm)
    cbSorted: TCheckBox;
    procedure cbSortedChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    FMain: TavMainRender;

    FProgram: TavProgram;
    FCubeVertices : TavVB;
    FCubeIndices  : TavIB;
    FCubeInstances: TavVB;

    FFrameBuffer: TavFrameBuffer;
  public
    procedure EraseBackground(DC: HDC); override;
    procedure RenderScene;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{$R 'WOIT_shaders\shaders.res'}

{ TCubeVertex }

class function TCubeVertex.Layout: IDataLayout;
begin
  Result := LB.Add('vsCoord', ctFloat, 3).
               Add('vsNormal', ctFloat, 3).
               Add('vsTexCrd', ctFloat, 2).Finish();
end;

{ TCubeInstance }

class function TCubeInstance.Layout: IDataLayout;
begin
  Result := LB.Add('aiPosition', ctFloat, 3).
               Add('aiColor', ctFloat, 4).Finish();
end;

procedure GenCube(XHalfSize, YHalfSize, ZHalfSize: Single; USize, VSize: Single; out OutVert: IVerticesData; out OutInd: IIndicesData);
var Vert : ICubeVertices;
    V : TCubeVertex;
    Ind: IIndices;
    i: Integer;
begin
  Vert := TCubeVertices.Create;

  V.vsNormal := Vec(0,0,-1);

  V.vsCoord := Vec(-XHalfSize,-YHalfSize,-ZHalfSize); V.vsTexCrd := Vec(0.0,   0.0);   Vert.Add(V);
  V.vsCoord := Vec(-XHalfSize, YHalfSize,-ZHalfSize); V.vsTexCrd := Vec(0.0,   VSize); Vert.Add(V);
  V.vsCoord := Vec( XHalfSize,-YHalfSize,-ZHalfSize); V.vsTexCrd := Vec(USize, 0.0);   Vert.Add(V);
  V.vsCoord := Vec( XHalfSize, YHalfSize,-ZHalfSize); V.vsTexCrd := Vec(USize, VSize); Vert.Add(V);

  V.vsNormal := Vec(0,0,1);
  V.vsCoord := Vec(-XHalfSize, YHalfSize, ZHalfSize); V.vsTexCrd := Vec(0.0,   0.0);   Vert.Add(V);
  V.vsCoord := Vec(-XHalfSize,-YHalfSize, ZHalfSize); V.vsTexCrd := Vec(0.0,   VSize); Vert.Add(V);
  V.vsCoord := Vec( XHalfSize, YHalfSize, ZHalfSize); V.vsTexCrd := Vec(USize, 0.0);   Vert.Add(V);
  V.vsCoord := Vec( XHalfSize,-YHalfSize, ZHalfSize); V.vsTexCrd := Vec(USize, VSize); Vert.Add(V);

  V.vsNormal := Vec(0,-1,0);
  V.vsCoord := Vec(-XHalfSize,-YHalfSize,-ZHalfSize); V.vsTexCrd := Vec(0.0,   0.0);   Vert.Add(V);
  V.vsCoord := Vec( XHalfSize,-YHalfSize,-ZHalfSize); V.vsTexCrd := Vec(0.0,   VSize); Vert.Add(V);
  V.vsCoord := Vec(-XHalfSize,-YHalfSize, ZHalfSize); V.vsTexCrd := Vec(USize, 0.0);   Vert.Add(V);
  V.vsCoord := Vec( XHalfSize,-YHalfSize, ZHalfSize); V.vsTexCrd := Vec(USize, VSize); Vert.Add(V);

  V.vsNormal := Vec(0,1,0);
  V.vsCoord := Vec( XHalfSize, YHalfSize,-ZHalfSize); V.vsTexCrd := Vec(0.0,   0.0);   Vert.Add(V);
  V.vsCoord := Vec(-XHalfSize, YHalfSize,-ZHalfSize); V.vsTexCrd := Vec(0.0,   VSize); Vert.Add(V);
  V.vsCoord := Vec( XHalfSize, YHalfSize, ZHalfSize); V.vsTexCrd := Vec(USize, 0.0);   Vert.Add(V);
  V.vsCoord := Vec(-XHalfSize, YHalfSize, ZHalfSize); V.vsTexCrd := Vec(USize, VSize); Vert.Add(V);

  V.vsNormal := Vec(-1,0,0);
  V.vsCoord := Vec(-XHalfSize,-YHalfSize,-ZHalfSize); V.vsTexCrd := Vec(0.0,   0.0);   Vert.Add(V);
  V.vsCoord := Vec(-XHalfSize,-YHalfSize, ZHalfSize); V.vsTexCrd := Vec(0.0,   VSize); Vert.Add(V);
  V.vsCoord := Vec(-XHalfSize, YHalfSize,-ZHalfSize); V.vsTexCrd := Vec(USize, 0.0);   Vert.Add(V);
  V.vsCoord := Vec(-XHalfSize, YHalfSize, ZHalfSize); V.vsTexCrd := Vec(USize, VSize); Vert.Add(V);

  V.vsNormal := Vec(1,0,0);
  V.vsCoord := Vec( XHalfSize,-YHalfSize, ZHalfSize); V.vsTexCrd := Vec(0.0,   0.0);   Vert.Add(V);
  V.vsCoord := Vec( XHalfSize,-YHalfSize,-ZHalfSize); V.vsTexCrd := Vec(0.0,   VSize); Vert.Add(V);
  V.vsCoord := Vec( XHalfSize, YHalfSize, ZHalfSize); V.vsTexCrd := Vec(USize, 0.0);   Vert.Add(V);
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

function GenCubeInstances(const RangeMin, RangeMax: TVec3I): IVerticesData;
var  instV: TCubeInstance;
     inst : ICubeInstances;
     i, j, k: Integer;
     rMin, rMax: TVec3;
begin
  rMin := RangeMin;
  rMax := RangeMax;

  inst := TCubeInstances.Create();
  for i := RangeMin.x to RangeMax.x do
    for j := RangeMin.y to RangeMax.y do
      for k := RangeMin.z to RangeMax.z do
      begin
        instV.aiPosition := Vec(i, j, k) * 4.0;
        instV.aiColor.xyz := (Vec(i, j, k) - rMin) / (rMax - rMin);
        instV.aiColor.w := 0.5;
        inst.Add(instV);
      end;
  Result := inst as IVerticesData;
end;

function GenCubeInstancesSorted(const RangeMin, RangeMax: TVec3I): IVerticesData;
var  instV: TCubeInstance;
     inst : ICubeInstances;
     i, j, k: Integer;
     rMin, rMax: TVec3;
     lst: TList;
begin
  rMin := RangeMin;
  rMax := RangeMax;

  inst := TCubeInstances.Create();
  for i := RangeMin.x to RangeMax.x do
    for j := RangeMin.y to RangeMax.y do
      for k := RangeMin.z to RangeMax.z do
      begin
        instV.aiPosition := Vec(i, j, k) * 4.0;
        instV.aiColor.xyz := (Vec(i, j, k) - rMin) / (rMax - rMin);
        instV.aiColor.w := 0.5;
        inst.Add(instV);
      end;
  lst := TList.Create;
  try
    for i := 0 to inst.Count - 1 do
      lst.Add(inst.PItem[i]);
    lst.Sort();
  finally
    lst.Free;
  end;

  Result := inst as IVerticesData;
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
  FMain.Camera.Eye := Vec(-16, 14, -20);
  FMain.Projection.FarPlane := 300.0;
  FMain.Projection.NearPlane := 0.1;

  FFrameBuffer := Create_FrameBuffer(FMain, [TTextureFormat.RGBA, TTextureFormat.D32f]);

  FProgram := TavProgram.Create(FMain);
  FProgram.LoadFromJSON('OGL_base', True);

  GenCube(H, H, H, 0, 0, vert, ind);

  FCubeVertices := TavVB.Create(FMain);
  FCubeVertices.Vertices := vert;
  FCubeIndices := TavIB.Create(FMain);
  FCubeIndices.PrimType := ptTriangles;
  FCubeIndices.Indices := ind;
  FCubeIndices.CullMode := cmBack;
  FCubeInstances := TavVB.Create(FMain);
  FCubeInstances.Vertices := GenCubeInstances(Vec(-2,-2,-2), Vec(2, 2, 2));

  cc := TavCameraController.Create(FMain);
  cc.CanRotate := True;
end;

procedure TfrmMain.cbSortedChange(Sender: TObject);
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
    FMain.States.DepthTest := False;
    FMain.States.Blending := True;
    FMain.States.SetBlendFunctions(bfSrcAlpha, bfInvSrcAlpha);

    FFrameBuffer.FrameRect := RectI(0, 0, ClientWidth, ClientHeight);
    FFrameBuffer.Select;

    FMain.Clear(Vec(0.0,0.2,0.4,1.0), True, FMain.Projection.DepthRange.y, True);

    FProgram.Select;
    FProgram.SetAttributes(FCubeVertices, FCubeIndices, FCubeInstances);
    FProgram.Draw(FCubeInstances.BuildedVertCount);

    FFrameBuffer.BlitToWindow;
    FMain.Present;
  finally
    FMain.Unbind;
  end;
end;

end.


