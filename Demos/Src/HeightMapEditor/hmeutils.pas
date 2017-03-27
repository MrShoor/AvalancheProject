unit HMEUtils;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, avRes, mutils, avTypes, avContnrs, avTess;

type
  TGroundCell = packed record
    aiPosSize    : TVec4;
    aiBorderDelta: TVec4;
    aiQuadDelta  : TVec2;
    class function Layout: IDataLayout; static;
  end;
  IGroundCellVertices = {$IfDef FPC}specialize{$EndIf}IArray<TGroundCell>;
  TGroundCellVertices = {$IfDef FPC}specialize{$EndIf}TVerticesRec<TGroundCell>;

  TavGroundQuadBuilder = class (TavMainRenderChild)
  private
    FProg: TavProgram;
  public
    procedure Build(const AArea: TVec2i; const ACellSize: Integer;
                    const AHeightMap: TavTexture; HeightScale: Single;
                    const AOutBuffer: TavVB);
    procedure AfterConstruction; override;
  end;

  { TavNormalMapBuilder }

  TavNormalMapBuilder = class (TavMainRenderChild)
  private
    FProg: TavProgram;
    FQuadVB: TavVB;
  public
    procedure Build(const AHeightMap: TavTexture; const ANormalMap: TavTexture; HeightScale: Single; GenerateMips: Boolean);
    procedure AfterConstruction; override;
  end;

function GetHeightAt(const AHeightMap: ITextureData; x, y: Single; AHeightScale: Single): Single;
function RayCast(const AHeightMap: ITextureData; AHeightScale: Single; const ARay: TLine; out IntPt: TVec3): Boolean;

implementation

uses
  avUtils, Math;

function GetHeightAt(const AHeightMap: ITextureData; x, y: Single; AHeightScale: Single): Single;
var xf, xc, yf, yc: Integer;
    hy: array [0..1] of Single;
    hmip: ITextureMip;
begin
  Result := 0;
  hmip := AHeightMap.MipData(0,0);
  if hmip = nil then Exit;
  x := x - 0.5;
  y := y - 0.5;
  if x < 0 then Exit;
  if y < 0 then Exit;
  if x >= hmip.Width-1 then Exit;
  if y >= hmip.Height-1 then Exit;
  xf := Math.Floor(x);
  yf := Math.Floor(y);
  xc := (xf+1);
  yc := (yf+1);
  hy[0] := Lerp(PWord(hmip.Pixel(xf, yf))^, PWord(hmip.Pixel(xc, yf))^, x - xf);
  hy[1] := Lerp(PWord(hmip.Pixel(xf, yc))^, PWord(hmip.Pixel(xc, yc))^, x - xf);
  Result := -Lerp(hy[0], hy[1], y - yf) / $FFFF * AHeightScale;
end;

function RayCast(const AHeightMap: ITextureData; AHeightScale: Single; const ARay: TLine; out IntPt: TVec3): Boolean;
var lowPlane: TPlane;
    t, tMax, tMin, tMaxStep, h: Single;
    i: Integer;
begin
  Result := False;

  tMaxStep := 1.0/(Max(Abs(ARay.Dir.x),Abs(ARay.Dir.y)));
  if not Intersect(Plane(0,0,1,0), ARay, IntPt) then
    tMax := 2000 * tMaxStep
  else
    tMax := Dot(IntPt-ARay.Pnt, ARay.Dir) / LenSqr(ARay.Dir);
  if tMax < 0 then tMax := 2000 * tMaxStep;

  IntPt := ARay.Dir;
  t := 0;
  while (t < tMax) do
  begin
    IntPt := ARay.Pnt + ARay.Dir*t;
    if IntPt.z > GetHeightAt(AHeightMap, IntPt.x, IntPt.y, AHeightScale) then Break;
    t := t + tMaxStep;
  end;
  if t >= tMax then
  begin
    IntPt := ARay.Pnt + ARay.Dir*tMax;
    Result := Abs(GetHeightAt(AHeightMap, IntPt.x, IntPt.y, AHeightScale) - IntPt.z) < 0.1;
    Exit;
  end;

  tMax := t;
  tMin := tMax - tMaxStep;
  for i := 0 to 39 do
  begin
    t := (tMin + tMax) * 0.5;
    IntPt := ARay.Pnt + ARay.Dir*t;
    h := GetHeightAt(AHeightMap, IntPt.x, IntPt.y, AHeightScale);
    if Abs(IntPt.z - h)<0.01 then Exit(True);
    if IntPt.z > h then
      tMax := t
    else
      tMin := t;
  end;

  Result := Intersect(Plane(0,0,1,0), ARay, IntPt);
  //Assert(False);
end;

{ TavNormalMapBuilder }

procedure TavNormalMapBuilder.AfterConstruction;
begin
  inherited;
  FProg := TavProgram.Create(Self);
  FProg.Load('Build_NormalMap', True);
  FQuadVB := GenQuad_VB(Self, Vec(0, 0, 1, 1));
end;

procedure TavNormalMapBuilder.Build(const AHeightMap, ANormalMap: TavTexture; HeightScale: Single; GenerateMips: Boolean);
var fbo: TavFrameBuffer;
    wasBinded: Boolean;
    oldWire: Boolean;
begin
  wasBinded := Main.Binded;
  try
    if not wasBinded then
      if not Main.Bind then Exit;

    fbo := nil;
    try
      AHeightMap.Build;

      fbo := TavFrameBuffer.Create(Self);
      fbo.SetColor(0, ANormalMap);
      fbo.FrameRect := RectI(0, 0, Round(AHeightMap.ImageSize.x), Round(AHeightMap.ImageSize.y));
      fbo.Select();

      oldWire := Main.States.Wireframe;
      Main.States.Wireframe := False;

      FProg.Select();
      FProg.SetAttributes(FQuadVB, nil, nil);
      FProg.SetUniform('HeightMap', AHeightMap, Sampler_NoFilterClamped);
      FProg.SetUniform('HeightScale', HeightScale);
      FProg.Draw();

      Main.States.Wireframe := oldWire;

      if GenerateMips then
        ANormalMap.GenerateMips;
    finally
      FreeAndNil(fbo);
    end;

  finally
    if not wasBinded then
      if Main.Binded then
        Main.Unbind;
  end;
end;

{ TavGroundQuadBuilder }

procedure TavGroundQuadBuilder.AfterConstruction;
begin
  inherited;
  FProg := TavProgram.Create(Self);
  FProg.Load('Build_GroundCells', TGroundCell.Layout, True);
end;

procedure TavGroundQuadBuilder.Build(const AArea: TVec2i; const ACellSize: Integer;
                                     const AHeightMap: TavTexture; HeightScale: Single;
                                     const AOutBuffer: TavVB);
var fbo: TavFrameBuffer;
    wasBinded: Boolean;
    CellsCount: TVec2i;
begin
  wasBinded := Main.Binded;
  try
    if not wasBinded then
      if not Main.Bind then Exit;

    fbo := nil;
    try
      CellsCount.x := AArea.x div ACellSize;
      CellsCount.y := AArea.y div ACellSize;

      AHeightMap.Build;
      AOutBuffer.Build;

      fbo := TavFrameBuffer.Create(Self);
      fbo.SetStreamOut(0, AOutBuffer, 0);
      fbo.Select();

      FProg.Select();
      FProg.SetAttributes(nil, nil, nil);
      FProg.SetUniform('HeightMap', AHeightMap, Sampler_LinearClamped);
      FProg.SetUniform('HeightScale', HeightScale);
      FProg.SetUniform('CellSize', ACellSize*1.0);
      FProg.SetUniform('fArea', CellsCount*1.0);
      FProg.Draw(ptPoints, cmNone, False, 0, 0, CellsCount.x*CellsCount.y);
    finally
      FreeAndNil(fbo);
    end;

  finally
    if not wasBinded then
      if Main.Binded then
        Main.Unbind;
  end;
end;

{ TGroundCell }

{ TGroundCell }

class function TGroundCell.Layout: IDataLayout;
begin
  Result := LB.Add('aiPosSize', ctFloat, 4).
               Add('aiBorderDelta', ctFloat, 4).
               Add('aiQuadDelta', ctFloat, 2).
               Finish();
end;

end.

