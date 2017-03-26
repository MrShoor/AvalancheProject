unit HMEUtils;

{$IfDef FPC}
  {$mode objfpc}{$H+}
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

implementation

uses
  avUtils;

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

      FProg.Select();
      FProg.SetAttributes(FQuadVB, nil, nil);
      FProg.SetUniform('HeightMap', AHeightMap, Sampler_NoFilterClamped);
      FProg.SetUniform('HeightScale', HeightScale);
      FProg.Draw();

      if GenerateMips then
        ANormalMap.GenerateMips;
    finally
      FreeAndNil(fbo);
    end;

    Main.Present;
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
      FProg.SetUniform('HeightMap', AHeightMap, Sampler_NoFilterClamped);
      FProg.SetUniform('HeightScale', HeightScale);
      FProg.SetUniform('CellSize', ACellSize*1.0);
      FProg.SetUniform('fArea', CellsCount*1.0);
      FProg.Draw(ptPoints, cmNone, False, 0, 0, CellsCount.x*CellsCount.y);
    finally
      FreeAndNil(fbo);
    end;

    Main.Present;
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

