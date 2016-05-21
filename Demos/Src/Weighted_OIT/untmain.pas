unit untMain;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  LCLType, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, avRes, avTypes, avTess, avContnrs, avContnrsDefaults, mutils,
  avCameraController, Math, ContextSwitcher;

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

  { TQuadVertex }

  TQuadVertex = packed record
    vsCoord: TVec2;
    class function Layout: IDataLayout; static;
  end;
  IQuadVertices = specialize IArray<TQuadVertex>;
  TQuadVertices = specialize TVerticesRec<TQuadVertex>;

  { TCubeInstance }

  TCubeInstance = packed record
    aiPosition: TVec3;
    aiColor : TVec4;
    class function Layout: IDataLayout; static;
  end;
  PCubeInstance = ^TCubeInstance;
  ICubeInstances = specialize IArray<TCubeInstance>;
  TCubeInstances = specialize TVerticesRec<TCubeInstance>;

  { TfrmMain }

  TfrmMain = class(TForm)
    Button1: TButton;
    cbSorted: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure cbSortedChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
  private
    FMain: TavMainRender;

    FCubeVertices : TavVB;
    FCubeIndices  : TavIB;
    FCubeInstances: TavVB;

    FQuad : TavVB;

    FClassic_Program: TavProgram;
    FClassic_FrameBuffer: TavFrameBuffer;

    FWOIT_FrameBuffer: TavFrameBuffer;
    FWOIT_AccumProgram: TavProgram;
    FWOIT_ResolveProgram: TavProgram;

    FContextSwitcher: TavContextSwitcher;
  private
    function GenCubeInstances(ZSorted: Boolean = False): IVerticesData; overload;
    function GenCubeInstances(const RangeMin, RangeMax: TVec3I; ZSorted: Boolean = False): IVerticesData; overload;
  public
    procedure EraseBackground(DC: HDC); override;
    procedure RenderScene;
  end;

var
  frmMain: TfrmMain;

implementation

type

  { TDepthComparator }

  TDepthComparator = class (TInterfacedObject, specialize IComparer<TCubeInstance>)
  private
    FTransform: TMat4;
    function Compare(const Left, Right: TCubeInstance): Integer;
  public
    constructor Create(const Transform: TMat4);
  end;

{$R *.lfm}

{ TDepthComparator }

function TDepthComparator.Compare(const Left, Right: TCubeInstance): Integer;
begin
  Result := -Sign( (Left.aiPosition * FTransform).z - (Right.aiPosition * FTransform).z );
end;

constructor TDepthComparator.Create(const Transform: TMat4);
begin
  FTransform := Transform;
end;

{$R 'WOIT_shaders\shaders.rc'}

{ TCubeVertex }

class function TCubeVertex.Layout: IDataLayout;
begin
  Result := LB.Add('vsCoord', ctFloat, 3).
               Add('vsNormal', ctFloat, 3).
               Add('vsTexCrd', ctFloat, 2).Finish();
end;

{ TQuadVertex }

class function TQuadVertex.Layout: IDataLayout;
begin
  Result := LB.Add('vsCoord', ctFloat, 2).Finish();
end;

{ TCubeInstance }

class function TCubeInstance.Layout: IDataLayout;
begin
  Result := LB.Add('aiPosition', ctFloat, 3).
               Add('aiColor', ctFloat, 4).Finish();
end;

function GenScreeAlignedQuad(): IVerticesData;
var Vert: IQuadVertices;
    V: TQuadVertex;
begin
  Vert := TQuadVertices.Create;
  V.vsCoord := Vec(-1, -1); Vert.Add(V);
  V.vsCoord := Vec(-1,  1); Vert.Add(V);
  V.vsCoord := Vec( 1, -1); Vert.Add(V);
  V.vsCoord := Vec( 1,  1); Vert.Add(V);
  Result := Vert as IVerticesData;
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

  FContextSwitcher := TavContextSwitcher.Create(FMain, False);

//  FMain.Camera.Eye := Vec(-16, 14, -20);
  FMain.Camera.Eye := Vec(-14, 0, 0);
  FMain.Projection.FarPlane := 500.0;
  FMain.Projection.NearPlane := 0.1;

  FClassic_FrameBuffer := Create_FrameBuffer(FMain, [TTextureFormat.RGBA, TTextureFormat.D32f]);
  FWOIT_FrameBuffer := Create_FrameBuffer(FMain, [TTextureFormat.RGBA16f, TTextureFormat.R16f]);

  FClassic_Program := TavProgram.Create(FMain);
  FClassic_Program.LoadFromJSON('Classic', True);
  FWOIT_AccumProgram := TavProgram.Create(FMain);
  FWOIT_AccumProgram.LoadFromJSON('WOIT_Accum', True);
  FWOIT_ResolveProgram := TavProgram.Create(FMain);
  FWOIT_ResolveProgram.LoadFromJSON('WOIT_Resolve', True);

  GenCube(H, H, H, 0, 0, vert, ind);

  FCubeVertices := TavVB.Create(FMain);
  FCubeVertices.Vertices := vert;
  FCubeIndices := TavIB.Create(FMain);
  FCubeIndices.PrimType := ptTriangles;
  FCubeIndices.Indices := ind;
  FCubeIndices.CullMode := cmBack;
  FCubeInstances := TavVB.Create(FMain);
  FCubeInstances.Vertices := GenCubeInstances();

  FQuad := TavVB.Create(FMain);
  FQuad.Vertices := GenScreeAlignedQuad;
  FQuad.PrimType := ptTriangleStrip;

  cc := TavCameraController.Create(FMain);
  cc.CanRotate := True;
end;

procedure TfrmMain.cbSortedChange(Sender: TObject);
begin
  FCubeInstances.Vertices := GenCubeInstances(cbSorted.Checked);
  Invalidate;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  FContextSwitcher.SwitchContext;
  case FMain.ActiveApi of
    apiOGL: Caption := 'OpenGL';
    apiDX11: Caption := 'DirectX';
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMain);
end;

procedure TfrmMain.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SetFocus;
end;

procedure TfrmMain.FormPaint(Sender: TObject);
begin
  RenderScene;
end;

function TfrmMain.GenCubeInstances(ZSorted: Boolean): IVerticesData;
begin
  Result := GenCubeInstances(Vec(-2,-2,-2), Vec(2, 2, 2), ZSorted);
end;

function DepthComparator(item1, item2: Pointer; dataSize: Integer; userData: Pointer): Integer;
var v1: PCubeInstance absolute item1;
    v2: PCubeInstance absolute item2;
    m: PMat4 absolute userData;
begin
  Result := -Sign( (v1^.aiPosition * m^).z - (v2^.aiPosition * m^).z );
end;

function TfrmMain.GenCubeInstances(const RangeMin, RangeMax: TVec3I; ZSorted: Boolean = False): IVerticesData;
var  instV: TCubeInstance;
     inst : ICubeInstances;
     i, j, k: Integer;
     rMin, rMax, rDelta: TVec3;
begin
  rMin := RangeMin;
  rMax := RangeMax;
  rDelta := rMax - rMin;

  inst := TCubeInstances.Create();
  for i := RangeMin.x to RangeMax.x do
    for j := RangeMin.y to RangeMax.y do
      for k := RangeMin.z to RangeMax.z do
      begin
        instV.aiPosition := Vec(i, j, k) * 4.0;
        instV.aiColor.xyz := (Vec(i, j, k) - rMin) / rDelta;
        if i*j*k=0 then
          instV.aiColor.w := 1.0
        else
          instV.aiColor.w := 0.33;
        inst.Add(instV);
      end;
  if ZSorted then
    inst.Sort(TDepthComparator.Create(FMain.Camera.Matrix));

  Result := inst as IVerticesData;
end;

procedure TfrmMain.EraseBackground(DC: HDC);
begin
  //inherited EraseBackground(DC);
end;

procedure TfrmMain.RenderScene;
begin
  if FMain = nil then Exit;
  if cbSorted.Checked then FCubeInstances.Vertices := GenCubeInstances(cbSorted.Checked);
  if FMain.Bind then
  try
    if cbSorted.Checked then
    begin
      //classic OVER blending with back-to-front sort
      FMain.States.Blending[AllTargets] := True;
      FMain.States.SetBlendFunctions(bfSrcAlpha, bfInvSrcAlpha);

      FClassic_FrameBuffer.FrameRect := RectI(0, 0, ClientWidth, ClientHeight);
      FClassic_FrameBuffer.Select;

      //FMain.Clear(Vec(0.0,0.2,0.4,1.0), True, FMain.Projection.DepthRange.y, True);
      FMain.Clear(Vec(0,0,0,0));

      FClassic_Program.Select;
      FClassic_Program.SetAttributes(FCubeVertices, FCubeIndices, FCubeInstances);
      FClassic_Program.Draw(FCubeInstances.BuildedVertCount);

      FClassic_FrameBuffer.BlitToWindow;
    end
    else
    begin
      //
      FWOIT_FrameBuffer.FrameRect := RectI(0, 0, ClientWidth, ClientHeight);
      FWOIT_FrameBuffer.Select();

      FMain.States.Blending[AllTargets] := True;
      FMain.States.SetBlendFunctions(bfOne, bfOne, 0); //accum buffer
      FMain.States.SetBlendFunctions(bfDstColor, bfZero, 1); //product buffer

      FWOIT_FrameBuffer.Clear(0, Vec(0,0,0,0));
      FWOIT_FrameBuffer.Clear(1, Vec(1.0,1.0,1.0,1.0));

      FWOIT_AccumProgram.Select;
      FWOIT_AccumProgram.SetAttributes(FCubeVertices, FCubeIndices, FCubeInstances);
      FWOIT_AccumProgram.SetUniform('NearFarPlane', Vec(FMain.Projection.NearPlane, FMain.Projection.FarPlane));
      FWOIT_AccumProgram.Draw(FCubeInstances.BuildedVertCount);

      FClassic_FrameBuffer.FrameRect := RectI(0, 0, ClientWidth, ClientHeight);
      FClassic_FrameBuffer.Select();
      FClassic_FrameBuffer.Clear(0, Vec(0,0,0,0));
      FMain.States.SetBlendFunctions(bfSrcAlpha, bfInvSrcAlpha, 0);

      FWOIT_ResolveProgram.Select;
      FWOIT_ResolveProgram.SetAttributes(FQuad, nil, nil);
      FWOIT_ResolveProgram.SetUniform('Accum', FWOIT_FrameBuffer.GetColor(0), Sampler_NoFilter);
      FWOIT_ResolveProgram.SetUniform('Product', FWOIT_FrameBuffer.GetColor(1), Sampler_NoFilter);
      FWOIT_ResolveProgram.Draw();

      FClassic_FrameBuffer.BlitToWindow();
    end;
    FMain.Present;
  finally
    FMain.Unbind;
  end;
end;

end.


